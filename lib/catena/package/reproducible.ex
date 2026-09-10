defmodule Catena.Package.Reproducible do
  @moduledoc "Exact input envelopes and canonical full-output archives for retained package builds."
  alias Catena.{CanonicalJCS, Calling.Descriptor, OTP.Profile}
  @version "0.1.73"
  @limit 16_777_216
  @total 67_108_864

  def profile,
    do: %{
      version: @version,
      files: 256,
      file_bytes: @limit,
      total_bytes: @total,
      generators: 64,
      environment_entries: 64,
      path_bytes: 240,
      cache: false,
      arbitrary_generators: false,
      secret_dependent_builds: false,
      mode: 420,
      mtime: 0
    }

  def toolchain do
    with {:ok, host} <- Profile.require_supported() do
      {:ok,
       %{
         "host" => host,
         "compiler" => Descriptor.compiler_digest(),
         "limits" => Descriptor.digest(Catena.ImplementationLimits.all())
       }}
    end
  end

  def plan(files, manifest, options \\ []) do
    with true <- Keyword.keys(options) -- [:environment, :generators] == [],
         false <- Catena.Runtime.Secret.sensitive?({files, manifest, options}),
         true <- files?(files) and path?(manifest) and Map.has_key?(files, manifest),
         true <- Path.dirname(manifest) == ".",
         environment <- Keyword.get(options, :environment, %{}),
         true <- environment?(environment),
         generators <- Keyword.get(options, :generators, []),
         {:ok, generated} <- generate(files, environment, generators),
         {:ok, _decoded, outputs} <- manifest(generated, manifest),
         true <- Enum.all?(outputs, &(not Map.has_key?(generated, &1))),
         {:ok, toolchain} <- toolchain() do
      payload = %{
        "format" => "catena-reproducible-input",
        "version" => @version,
        "manifest" => manifest,
        "files" => encode_files(files),
        "environment" => environment,
        "generators" => generators,
        "toolchain" => toolchain,
        "outputs" => outputs,
        "generated" => Map.new(generated, fn {path, bytes} -> {path, hash(bytes)} end)
      }

      {:ok, Map.put(payload, "digest", digest(payload))}
    else
      _ -> {:error, :invalid_reproducible_input}
    end
  rescue
    _ -> {:error, :invalid_reproducible_input}
  end

  def verify(plan) do
    with {:ok, files} <- decode_files(plan["files"]),
         {:ok, rebuilt} <-
           plan(files, plan["manifest"],
             environment: plan["environment"],
             generators: plan["generators"]
           ),
         true <- rebuilt == plan,
         do: :ok,
         else: (_ -> {:error, :reproducible_input_mismatch})
  rescue
    _ -> {:error, :reproducible_input_mismatch}
  end

  def build(plan, root) do
    with :ok <- verify(plan), :ok <- File.mkdir(root) do
      try do
        {:ok, files} = decode_files(plan["files"])
        {:ok, files} = generate(files, plan["environment"], plan["generators"])

        for {path, bytes} <- files do
          full = Path.join(root, path)
          File.mkdir_p!(Path.dirname(full))
          File.write!(full, bytes, [:exclusive])
          File.chmod!(full, 0o644)
        end

        with {:ok, _} <-
               Catena.Package.Linker.compile_manifest(Path.join(root, plan["manifest"]),
                 reproducible_paths: true
               ),
             {:ok, output} <- read_outputs(root, plan["outputs"]),
             {:ok, archive} <- archive(output, plan["digest"]) do
          {:ok,
           %{archive: archive, digest: hash(archive), input: plan["digest"], outputs: output}}
        else
          _ -> {:error, :reproducible_build_failed}
        end
      rescue
        _ -> {:error, :reproducible_build_failed}
      after
        File.rm_rf(root)
      end
    else
      _ -> {:error, :reproducible_build_refused}
    end
  end

  def rebuild_verify(plan, archive, root) do
    with {:ok, _} <- decode_archive(archive),
         {:ok, %{archive: ^archive}} <- build(plan, root),
         do: :ok,
         else: (_ -> {:error, :reproducible_archive_mismatch})
  end

  def cache_key(plan) do
    with :ok <- verify(plan), do: {:ok, plan["digest"]}
  end

  def archive(files, input_digest) do
    if files?(files) and digest?(input_digest) do
      entries =
        files
        |> Enum.sort()
        |> Enum.map(fn {path, bytes} ->
          %{
            "path" => path,
            "mode" => 420,
            "mtime" => 0,
            "bytes" => Base.encode64(bytes),
            "digest" => hash(bytes)
          }
        end)

      {:ok,
       CanonicalJCS.encode(%{
         "format" => "catena-reproducible-archive",
         "version" => @version,
         "input" => input_digest,
         "files" => entries
       })}
    else
      {:error, :invalid_reproducible_archive}
    end
  end

  def decode_archive(binary) when is_binary(binary) and byte_size(binary) <= 100_663_296 do
    with {:ok, doc} <- CanonicalJCS.decode(binary, canonical: true),
         true <- is_list(doc["files"]) and length(doc["files"]) <= 256,
         files <-
           Map.new(doc["files"], fn entry -> {entry["path"], Base.decode64!(entry["bytes"])} end),
         {:ok, ^binary} <- archive(files, doc["input"]),
         do: {:ok, files},
         else: (_ -> {:error, :invalid_reproducible_archive})
  rescue
    _ -> {:error, :invalid_reproducible_archive}
  end

  def decode_archive(_), do: {:error, :invalid_reproducible_archive}

  def stage(binary, destination) do
    with {:ok, _} <- decode_archive(binary),
         true <- is_binary(destination),
         path <- Path.expand(destination),
         temporary <- path <> ".stage-" <> Base.encode16(:crypto.strong_rand_bytes(16)),
         :ok <- File.write(temporary, binary, [:binary, :exclusive, :sync]) do
      {:ok, %{owner: self(), destination: path, temporary: temporary, digest: hash(binary)}}
    else
      _ -> {:error, :archive_stage_failed}
    end
  end

  def commit(%{owner: owner, destination: path, temporary: temp, digest: expected})
      when owner == self() do
    with true <- staged_path?(path, temp),
         {:ok, %{type: :regular}} <- File.lstat(temp),
         {:ok, bytes} <- File.read(temp),
         true <- hash(bytes) == expected,
         {:ok, _} <- decode_archive(bytes),
         :ok <- File.rename(temp, path),
         do: :ok,
         else: (_ -> {:error, :archive_commit_failed})
  end

  def commit(_), do: {:error, :archive_commit_failed}

  def cancel(%{owner: owner, destination: path, temporary: temp}) when owner == self() do
    if staged_path?(path, temp), do: File.rm(temp), else: {:error, :invalid_archive_stage}
  end

  def cancel(_), do: {:error, :invalid_archive_stage}

  defp staged_path?(path, temp),
    do:
      is_binary(path) and is_binary(temp) and
        Path.dirname(path) == Path.dirname(temp) and String.starts_with?(temp, path <> ".stage-") and
        byte_size(temp) == byte_size(path) + 39

  defp manifest(files, name) do
    with {:ok, decoded} <- Catena.Package.Manifest.decode(files[name]),
         true <- decoded.version in ["0.1.6", "0.1.7"],
         true <- is_nil(decoded.governance),
         inputs <- Enum.map(decoded.modules, & &1["source"]) ++ decoded.interfaces,
         outputs <-
           Enum.flat_map(decoded.modules, &[&1["beam"], &1["interface"]]) ++
             [decoded.output, decoded.assurance],
         true <- Enum.all?(inputs ++ outputs, &path?/1),
         true <- Enum.all?(inputs, &Map.has_key?(files, &1)),
         true <- length(outputs) == length(Enum.uniq(outputs)),
         true <- length(outputs) <= 256 do
      {:ok, decoded, Enum.sort(outputs)}
    else
      _ -> {:error, :invalid_reproducible_manifest}
    end
  end

  defp generate(files, environment, generators)
       when is_list(generators) and length(generators) <= 64 do
    Enum.reduce_while(generators, {:ok, files}, fn generator, {:ok, files} ->
      with true <- is_map(generator) and Enum.sort(Map.keys(generator)) == ~w(output parts),
           output <- generator["output"],
           true <- path?(output) and not Map.has_key?(files, output),
           parts <- generator["parts"],
           true <- is_list(parts) and length(parts) <= 64,
           {:ok, parts} <- parts(parts, files, environment),
           true <- Enum.sum(Enum.map(parts, &byte_size/1)) <= @limit,
           next <- Map.put(files, output, IO.iodata_to_binary(parts)),
           true <- files?(next) do
        {:cont, {:ok, next}}
      else
        _ -> {:halt, {:error, :invalid_reproducible_generator}}
      end
    end)
  end

  defp generate(_, _, _), do: {:error, :invalid_reproducible_generator}

  defp parts(parts, files, environment) do
    Enum.reduce_while(parts, {:ok, []}, fn part, {:ok, acc} ->
      result =
        case part do
          %{"file" => path} when map_size(part) == 1 -> Map.fetch(files, path)
          %{"environment" => key} when map_size(part) == 1 -> Map.fetch(environment, key)
          %{"literal" => bytes} when map_size(part) == 1 and is_binary(bytes) -> {:ok, bytes}
          _ -> :error
        end

      case result do
        {:ok, bytes} -> {:cont, {:ok, acc ++ [bytes]}}
        _ -> {:halt, {:error, :undeclared_generator_input}}
      end
    end)
  end

  defp read_outputs(root, paths) do
    Enum.reduce_while(paths, {:ok, %{}}, fn path, {:ok, acc} ->
      with {:ok, %{type: :regular, size: size}} when size <= @limit <-
             File.lstat(Path.join(root, path)),
           {:ok, bytes} <- File.read(Path.join(root, path)),
           next <- Map.put(acc, path, bytes),
           true <- files?(next),
           do: {:cont, {:ok, next}},
           else: (_ -> {:halt, {:error, :invalid_build_output}})
    end)
  end

  defp environment?(env),
    do:
      is_map(env) and map_size(env) <= 64 and
        Enum.all?(env, fn {key, value} ->
          is_binary(key) and Regex.match?(~r/^[A-Z][A-Z0-9_]{0,63}$/, key) and
            is_binary(value) and String.valid?(value) and byte_size(value) <= 65536
        end)

  defp files?(files),
    do:
      is_map(files) and map_size(files) <= 256 and
        Enum.all?(files, fn {path, bytes} ->
          path?(path) and is_binary(bytes) and byte_size(bytes) <= @limit
        end) and
        Enum.sum(Enum.map(files, fn {_, bytes} -> byte_size(bytes) end)) <= @total

  defp path?(path),
    do:
      is_binary(path) and byte_size(path) in 1..240 and
        Regex.match?(~r/^[A-Za-z0-9_.-]+(?:\/[A-Za-z0-9_.-]+)*$/, path) and
        Enum.all?(String.split(path, "/"), &(&1 not in [".", ".."]))

  defp encode_files(files),
    do: Map.new(files, fn {path, bytes} -> {path, Base.encode64(bytes)} end)

  defp decode_files(files) when is_map(files) and map_size(files) <= 256 do
    decoded = Map.new(files, fn {path, bytes} -> {path, Base.decode64!(bytes)} end)
    if files?(decoded), do: {:ok, decoded}, else: {:error, :invalid_files}
  rescue
    _ -> {:error, :invalid_files}
  end

  defp decode_files(_), do: {:error, :invalid_files}
  defp digest?(d), do: is_binary(d) and Regex.match?(~r/^[0-9a-f]{64}$/, d)
  defp digest(term), do: term |> CanonicalJCS.encode() |> hash()
  defp hash(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
