defmodule Catena.Tool.Migration do
  @moduledoc "Explicit transactional application of retained JSON migration edits."

  alias Catena.{CanonicalJCS, Interface, LanguageSelection}

  @version "0.1.93"
  @maximum_files 32
  @maximum_input_bytes 16_777_216
  @maximum_result_bytes 16_777_216
  @maximum_edits 1_024
  @json_path ~r/^\$(?:\.[A-Za-z_][A-Za-z0-9_-]*)+$/u
  @kinds ~w(module manifest)

  def profile do
    %{
      version: @version,
      input: :retained_json,
      edit_kind: :json_edit,
      applicability: :machine_applicable_only,
      authorization: :explicit,
      preview: :exact_preimage_and_result,
      transaction: :stage_then_commit,
      backups: :retained,
      rollback: :attempted_on_every_commit_failure,
      verification: :module_or_manifest,
      governance_approval_inheritance: false,
      public_source_rewrites: :held_for_p109,
      maximum_files: @maximum_files,
      maximum_input_bytes: @maximum_input_bytes,
      maximum_result_bytes: @maximum_result_bytes,
      maximum_edits: @maximum_edits
    }
  end

  def plan(root, requests) when is_binary(root) and is_list(requests) do
    root = Path.expand(root)

    with {:ok, %File.Stat{type: :directory}} <- File.lstat(root),
         true <- length(requests) in 1..@maximum_files,
         true <- unique?(Enum.map(requests, &field(&1, "path"))),
         {:ok, files} <- plan_files(root, requests),
         true <-
           Enum.sum(Enum.map(files, &byte_size(Base.decode64!(&1["preimage"])))) <=
             @maximum_input_bytes,
         true <- Enum.sum(Enum.map(files, &length(&1["edits"]))) <= @maximum_edits do
      body = %{
        "format" => "catena-migration-plan",
        "version" => @version,
        "root" => root,
        "files" => Enum.sort_by(files, & &1["path"]),
        "public_source_rewrites" => "held-for-p109"
      }

      {:ok, Map.put(body, "digest", CanonicalJCS.digest(body))}
    else
      {:error, _} = error -> error
      _ -> {:error, :invalid_migration_request}
    end
  rescue
    _ -> {:error, :invalid_migration_request}
  end

  def plan(_, _), do: {:error, :invalid_migration_request}

  def preview(plan) do
    with :ok <- verify_plan(plan) do
      {:ok,
       %{
         "format" => "catena-migration-preview",
         "version" => @version,
         "plan" => plan["digest"],
         "files" =>
           Enum.map(plan["files"], fn file ->
             %{
               "path" => file["path"],
               "preimage_digest" => file["preimage_digest"],
               "result_digest" => file["result_digest"],
               "before" => file["preimage"],
               "after" => file["result"],
               "edits" => file["edits"]
             }
           end)
       }}
    end
  end

  def apply(plan, root, options \\ [])

  def apply(plan, root, options) when is_binary(root) and is_list(options) do
    with true <- Keyword.get(options, :authorized, false),
         :ok <- verify_plan(plan),
         root = Path.expand(root),
         true <- root == plan["root"],
         {:ok, %File.Stat{type: :directory}} <- File.lstat(root),
         :ok <- verify_preimages(root, plan["files"]),
         {:ok, verification} <- verify_results(plan["files"]),
         {:ok, transaction} <- commit(root, plan, options) do
      audit_body = %{
        "format" => "catena-migration-audit",
        "version" => @version,
        "plan" => plan["digest"],
        "status" => "committed",
        "files" =>
          Enum.zip_with(plan["files"], verification, fn file, checked ->
            %{
              "path" => file["path"],
              "preimage_digest" => file["preimage_digest"],
              "result_digest" => file["result_digest"],
              "backup" => transaction.backups[file["path"]],
              "verification" => checked
            }
          end),
        "governance_approval_inherited" => false
      }

      {:ok, Map.put(audit_body, "digest", CanonicalJCS.digest(audit_body))}
    else
      false -> {:error, :migration_not_authorized}
      {:error, _} = error -> error
      {:error, reason, detail} -> {:error, reason, detail}
      _ -> {:error, :migration_failed}
    end
  rescue
    _ -> {:error, :migration_failed}
  end

  def apply(_, _, _), do: {:error, :migration_failed}

  defp plan_files(root, requests) do
    Enum.reduce_while(requests, {:ok, []}, fn request, {:ok, acc} ->
      with {:ok, path, absolute} <- safe_file(root, field(request, "path")),
           kind when kind in @kinds <- field(request, "document_kind"),
           edits when is_list(edits) and edits != [] <- field(request, "edits"),
           {:ok, edits} <- normalize_edits(edits),
           :ok <- disjoint(edits),
           {:ok, bytes} <- File.read(absolute),
           true <- byte_size(bytes) <= @maximum_input_bytes,
           {:ok, document} when is_map(document) <- JSON.decode(bytes),
           {:ok, migrated} <- apply_edits(document, edits),
           result <- CanonicalJCS.encode(migrated) <> "\n",
           true <- byte_size(result) <= @maximum_result_bytes,
           true <- result != bytes do
        file = %{
          "path" => path,
          "document_kind" => kind,
          "preimage_digest" => digest(bytes),
          "result_digest" => digest(result),
          "preimage" => Base.encode64(bytes),
          "result" => Base.encode64(result),
          "edits" => edits
        }

        {:cont, {:ok, [file | acc]}}
      else
        {:error, _} = error -> {:halt, error}
        _ -> {:halt, {:error, :invalid_migration_request}}
      end
    end)
  end

  defp normalize_edits(edits) do
    normalized =
      Enum.map(edits, fn edit ->
        %{
          "kind" => field(edit, "kind"),
          "operation" => field(edit, "operation"),
          "path" => field(edit, "path"),
          "value" => field(edit, "value"),
          "applicability" => field(edit, "applicability")
        }
      end)

    if Enum.all?(normalized, fn edit ->
         edit["kind"] == "json-edit" and edit["operation"] in ~w(add replace remove) and
           edit["applicability"] == "machine-applicable" and
           is_binary(edit["path"]) and Regex.match?(@json_path, edit["path"])
       end),
       do: {:ok, Enum.sort_by(normalized, & &1["path"])},
       else: {:error, :unsupported_migration_edit}
  end

  defp disjoint(edits) do
    paths = Enum.map(edits, &segments(&1["path"]))

    if Enum.with_index(paths)
       |> Enum.all?(fn {left, index} ->
         paths
         |> Enum.drop(index + 1)
         |> Enum.all?(&(not prefix?(left, &1) and not prefix?(&1, left)))
       end),
       do: :ok,
       else: {:error, :ambiguous_or_overlapping_migration_edits}
  end

  defp prefix?(left, right), do: Enum.take(right, length(left)) == left

  defp apply_edits(document, edits) do
    Enum.reduce_while(edits, {:ok, document}, fn edit, {:ok, current} ->
      case update_json(current, segments(edit["path"]), edit) do
        {:ok, next} -> {:cont, {:ok, next}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp update_json(document, [key], %{"operation" => "add", "value" => value})
       when is_map(document) do
    if Map.has_key?(document, key),
      do: {:error, :ambiguous_migration_edit},
      else: {:ok, Map.put(document, key, value)}
  end

  defp update_json(document, [key], %{"operation" => "replace", "value" => value})
       when is_map(document) do
    if Map.has_key?(document, key),
      do: {:ok, Map.put(document, key, value)},
      else: {:error, :missing_migration_target}
  end

  defp update_json(document, [key], %{"operation" => "remove"}) when is_map(document) do
    if Map.has_key?(document, key),
      do: {:ok, Map.delete(document, key)},
      else: {:error, :missing_migration_target}
  end

  defp update_json(document, [key | rest], edit) when is_map(document) do
    with true <- Map.has_key?(document, key),
         {:ok, child} <- update_json(document[key], rest, edit),
         do: {:ok, Map.put(document, key, child)},
         else: (_ -> {:error, :missing_migration_target})
  end

  defp update_json(_, _, _), do: {:error, :missing_migration_target}

  defp verify_plan(%{"digest" => digest_value} = plan) do
    unsigned = Map.delete(plan, "digest")
    files = plan["files"]

    if digest?(digest_value) and digest_value == CanonicalJCS.digest(unsigned) and
         plan["format"] == "catena-migration-plan" and plan["version"] == @version and
         is_binary(plan["root"]) and is_list(files) and files != [] and
         length(files) <= @maximum_files and unique?(Enum.map(files, & &1["path"])) and
         Enum.all?(files, &valid_plan_file?/1) and within_plan_bounds?(files),
       do: :ok,
       else: {:error, :invalid_migration_plan}
  rescue
    _ -> {:error, :invalid_migration_plan}
  end

  defp verify_plan(_), do: {:error, :invalid_migration_plan}

  defp valid_plan_file?(file) when is_map(file) do
    with true <-
           Enum.sort(Map.keys(file)) ==
             ~w(document_kind edits path preimage preimage_digest result result_digest),
         true <- is_binary(file["path"]) and file["document_kind"] in @kinds,
         true <- digest?(file["preimage_digest"]) and digest?(file["result_digest"]),
         {:ok, preimage} <- Base.decode64(file["preimage"]),
         {:ok, result} <- Base.decode64(file["result"]),
         true <- preimage != result and digest(preimage) == file["preimage_digest"],
         true <- digest(result) == file["result_digest"],
         {:ok, edits} <- normalize_edits(file["edits"]),
         true <- edits == file["edits"],
         :ok <- disjoint(edits) do
      true
    else
      _ -> false
    end
  end

  defp valid_plan_file?(_), do: false

  defp within_plan_bounds?(files) do
    Enum.sum(Enum.map(files, &byte_size(Base.decode64!(&1["preimage"])))) <=
      @maximum_input_bytes and
      Enum.sum(Enum.map(files, &byte_size(Base.decode64!(&1["result"])))) <=
        @maximum_result_bytes and
      Enum.sum(Enum.map(files, &length(&1["edits"]))) <= @maximum_edits
  end

  defp verify_preimages(root, files) do
    Enum.reduce_while(files, :ok, fn file, :ok ->
      with {:ok, _path, absolute} <- safe_file(root, file["path"]),
           {:ok, bytes} <- File.read(absolute),
           true <- digest(bytes) == file["preimage_digest"],
           true <- Base.decode64!(file["preimage"]) == bytes do
        {:cont, :ok}
      else
        _ -> {:halt, {:error, :stale_migration_preimage}}
      end
    end)
  end

  defp verify_results(files) do
    Enum.reduce_while(files, {:ok, []}, fn file, {:ok, acc} ->
      preimage = Base.decode64!(file["preimage"])
      result = Base.decode64!(file["result"])

      case verify_pair(file["document_kind"], preimage, result) do
        {:ok, checked} -> {:cont, {:ok, [checked | acc]}}
        {:error, :behavioral_change_requires_review} = error -> {:halt, error}
        _ -> {:halt, {:error, :migrated_document_rejected}}
      end
    end)
    |> then(fn
      {:ok, results} -> {:ok, Enum.reverse(results)}
      error -> error
    end)
  end

  defp verify_pair(kind, preimage, result) do
    with {:ok, before} <- verify_result(kind, preimage),
         {:ok, after_result} <- verify_result(kind, result) do
      if comparable_verification(before) == comparable_verification(after_result) do
        {:ok, Map.put(after_result, "semantic_relation", "preserved")}
      else
        {:error, :behavioral_change_requires_review}
      end
    end
  end

  defp comparable_verification(%{"kind" => "module"} = value), do: value["interface_digest"]

  defp comparable_verification(%{"kind" => "manifest"} = value),
    do: value["language_selection"]

  defp verify_result("module", bytes) do
    with {:ok, core} <- Catena.check_json(bytes),
         {:ok, raw} <- core |> Interface.build() |> Interface.encode() |> JSON.decode() do
      {:ok, %{"kind" => "module", "interface_digest" => raw["digest"]}}
    end
  end

  defp verify_result("manifest", bytes) do
    with {:ok, manifest} <- Catena.Package.Manifest.decode(bytes) do
      {:ok,
       %{
         "kind" => "manifest",
         "language_selection" => LanguageSelection.to_map(manifest.selection)
       }}
    end
  end

  defp commit(root, plan, options) do
    try do
      nonce = String.slice(plan["digest"], 0, 16)

      with {:ok, backup_root} <- prepare_backup_root(root, plan["digest"]),
           {:ok, entries} <- prepare_entries(root, backup_root, nonce, plan["files"]),
           result <- commit_entries(entries, Keyword.get(options, :failure_injection)) do
        case result do
          :ok ->
            Enum.each(entries, &File.rm(&1.rollback))
            {:ok, %{backups: Map.new(entries, &{&1.path, Path.relative_to(&1.backup, root)})}}

          {:error, reason, committed} ->
            case rollback(committed, Keyword.get(options, :failure_injection)) do
              :ok -> {:error, reason}
              {:error, failures} -> {:error, :migration_rollback_failed, failures}
            end
        end
      else
        {:error, _} = error -> error
        other -> {:error, :migration_stage_failed, other}
      end
    after
      cleanup_stages(root, plan)
    end
  end

  defp prepare_backup_root(root, digest_value) do
    base = Path.join(root, ".catena-migration-backups")

    with :ok <- ensure_backup_base(base),
         backup_root = Path.join(base, digest_value),
         {:error, :enoent} <- File.lstat(backup_root),
         :ok <- File.mkdir(backup_root),
         do: {:ok, backup_root},
         else: (_ -> {:error, :unsafe_or_existing_migration_backup})
  end

  defp ensure_backup_base(base) do
    case File.lstat(base) do
      {:ok, %File.Stat{type: :directory}} -> :ok
      {:error, :enoent} -> File.mkdir(base)
      _ -> {:error, :unsafe_backup_base}
    end
  end

  defp prepare_entries(root, backup_root, nonce, files) do
    Enum.reduce_while(Enum.with_index(files), {:ok, []}, fn {file, index}, {:ok, acc} ->
      with {:ok, path, absolute} <- safe_file(root, file["path"]),
           backup = Path.join(backup_root, path),
           stage = absolute <> ".catena-migration-#{nonce}-#{index}.tmp",
           rollback = absolute <> ".catena-migration-#{nonce}-#{index}.rollback",
           bytes = Base.decode64!(file["result"]),
           :ok <- File.mkdir_p(Path.dirname(backup)),
           :ok <- File.cp(absolute, backup),
           :ok <- File.write(stage, bytes, [:binary, :exclusive, :sync]) do
        entry = %{
          path: path,
          final: absolute,
          backup: backup,
          stage: stage,
          rollback: rollback
        }

        {:cont, {:ok, [entry | acc]}}
      else
        _ -> {:halt, {:error, :migration_stage_failed}}
      end
    end)
    |> then(fn
      {:ok, entries} -> {:ok, Enum.reverse(entries)}
      error -> error
    end)
  rescue
    _ -> {:error, :migration_stage_failed}
  end

  defp commit_entries(entries, injection) do
    Enum.reduce_while(Enum.with_index(entries, 1), {:ok, []}, fn {entry, index}, {:ok, done} ->
      case File.rename(entry.final, entry.rollback) do
        :ok ->
          result =
            if injected?(injection, :move_failure_at, index),
              do: {:error, :injected},
              else: File.rename(entry.stage, entry.final)

          cond do
            result != :ok ->
              {:halt, {:error, :migration_commit_failed, [entry | done]}}

            injected?(injection, :interrupt_after, index) ->
              {:halt, {:error, :migration_interrupted, [entry | done]}}

            true ->
              {:cont, {:ok, [entry | done]}}
          end

        {:error, _} ->
          {:halt, {:error, :migration_commit_failed, done}}
      end
    end)
    |> case do
      {:ok, _} -> :ok
      error -> error
    end
  rescue
    _ -> {:error, :migration_commit_failed, []}
  end

  defp rollback(entries, injection) do
    failures =
      entries
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {entry, index} ->
        if injected?(injection, :rollback_failure_at, index) do
          [%{"path" => entry.path, "reason" => "injected-rollback-failure"}]
        else
          File.rm(entry.final)

          case File.rename(entry.rollback, entry.final) do
            :ok -> []
            {:error, reason} -> [%{"path" => entry.path, "reason" => Atom.to_string(reason)}]
          end
        end
      end)

    if failures == [], do: :ok, else: {:error, failures}
  end

  defp cleanup_stages(root, plan) do
    if is_map(plan) and is_list(plan["files"]) do
      nonce = String.slice(plan["digest"] || "", 0, 16)

      plan["files"]
      |> Enum.with_index()
      |> Enum.each(fn {file, index} ->
        File.rm(Path.join(root, file["path"]) <> ".catena-migration-#{nonce}-#{index}.tmp")
      end)
    end
  end

  defp safe_file(root, path) when is_binary(path) do
    absolute = Path.expand(path, root)

    with true <- Path.type(path) == :relative and ".." not in Path.split(path),
         true <- String.starts_with?(absolute, root <> "/"),
         :ok <- no_symlinks(root, absolute),
         {:ok, %File.Stat{type: :regular}} <- File.lstat(absolute) do
      {:ok, path, absolute}
    else
      _ -> {:error, :unsafe_migration_path}
    end
  end

  defp safe_file(_, _), do: {:error, :unsafe_migration_path}

  defp no_symlinks(root, absolute) do
    relative = Path.relative_to(absolute, root)

    relative
    |> Path.split()
    |> Enum.reduce_while(root, fn part, current ->
      candidate = Path.join(current, part)

      case File.lstat(candidate) do
        {:ok, %File.Stat{type: :symlink}} -> {:halt, {:error, :symlink}}
        {:ok, _} -> {:cont, candidate}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:error, _} -> {:error, :unsafe_migration_path}
      _ -> :ok
    end
  end

  defp segments(path), do: path |> String.trim_leading("$.") |> String.split(".")

  defp unique?(items),
    do: length(items) == length(Enum.uniq(items)) and Enum.all?(items, &is_binary/1)

  defp field(map, key) when is_map(map), do: Map.get(map, key, Map.get(map, String.to_atom(key)))
  defp field(_, _), do: nil

  defp injected?(injection, key, index) when is_map(injection),
    do: Map.get(injection, key) == index

  defp injected?(injection, key, index), do: injection == {key, index}
  defp digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)
end
