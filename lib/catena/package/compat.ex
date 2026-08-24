defmodule Catena.Package.Compat do
  @moduledoc """
  API compatibility at 0.1.24: strict interface diff classification and
  SemVer claim validation.

  The classifier consumes decoded semantic interfaces — the only
  cross-package contract the language recognizes. It parses no source,
  claims no behavior, and promises no ABI.
  """

  alias Catena.{Diagnostic, Entry, Interface, Package.Deps}

  @classes [:identical, :patch, :minor, :breaking]

  @spec classes() :: [atom()]
  def classes, do: @classes

  @doc """
  Classifies the ordered diff between two decoded semantic interfaces
  into `:identical`, `:patch`, `:minor`, or `:breaking` with an
  itemized change list. Accepts encoded interface binaries (decoded
  through the shipped boundary) or pre-decoded interface maps.
  Malformed input is `CMP002`; drift that cannot be assigned — a
  different interface identity — is `CMP003`.
  """
  @spec diff(map() | binary(), map() | binary()) ::
          {:ok, %{class: atom(), changes: [map()]}} | {:error, Diagnostic.t()}
  def diff(old, new) do
    with {:ok, old} <- prepare(old, "old"),
         {:ok, new} <- prepare(new, "new") do
      if identity(old) == identity(new) do
        changes = value_changes(old, new) ++ type_changes(old, new)
        changes = changes ++ categorical_changes(old, new) ++ effect_changes(old, new)

        {:ok, %{class: class_of(changes), changes: Enum.sort_by(changes, & &1.row)}}
      else
        {:error,
         Diagnostic.new("CMP003", "interfaces have different identities and cannot be compared",
           path: "$.module",
           details: %{
             old: identity(old),
             new: identity(new)
           }
         )}
      end
    end
  end

  @doc """
  Classifies a manifest entry-set diff under matrix rows 13–15: entry
  additions and launch-marker movement are minor; entry removals and
  result changes are breaking.
  """
  @spec diff_entries(list(), list()) :: {:ok, %{class: atom(), changes: [map()]}}
  def diff_entries(old, new) do
    old = Map.new(old || [], &{&1.name, &1})
    new = Map.new(new || [], &{&1.name, &1})

    changes =
      removal_changes(old, new, 14, "entry") ++
        result_changes(old, new) ++
        addition_changes(old, new, 13, "entry") ++
        marker_changes(old, new)

    {:ok, %{class: class_of(changes), changes: Enum.sort_by(changes, & &1.row)}}
  end

  @doc """
  Validates a claimed SemVer increment against the actual interface
  diff. At 1.0.0 and above a breaking diff requires a major increment
  and an additive diff requires minor; below 1.0.0 the Cargo-style rule
  applies — breaking requires minor, additive requires patch. A claim
  below the required allowance is `CMP001`; malformed versions or
  non-increment claims are `CMP002`. Over-signaling is valid.
  """
  @spec validate_claim(map() | binary(), map() | binary(), {String.t(), String.t()}, keyword()) ::
          {:ok, %{class: atom(), required: atom(), claim: atom()}} | {:error, Diagnostic.t()}
  def validate_claim(old, new, {claimed_old, claimed_new}, options \\ []) do
    entry_class =
      case Keyword.get(options, :entries) do
        {old_entries, new_entries} ->
          case diff_entries(old_entries, new_entries) do
            {:ok, %{class: class}} -> class
          end

        nil ->
          :identical
      end

    with {:ok, %{class: class}} <- diff(old, new),
         {:ok, claim} <- claim_allowance(claimed_old, claimed_new),
         required <- allowance_of(stronger(class, entry_class)),
         :ok <- check_claim(required, claim) do
      {:ok, %{class: class, required: required, claim: claim}}
    else
      {:underclaim, required} ->
        {:error,
         Diagnostic.new("CMP001", "version claim is below the required increment class",
           path: "$.claim",
           details: %{
             required: required,
             claimed_old: claimed_old,
             claimed_new: claimed_new,
             rule: "1.0+ major-as-breaking; 0.x minor-as-breaking"
           }
         )}

      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}
    end
  end

  defp check_claim(required, claim) do
    if rank(required) <= rank(claim), do: :ok, else: {:underclaim, required}
  end

  defp stronger(a, b), do: if(rank(a) >= rank(b), do: a, else: b)

  defp prepare(binary, side) when is_binary(binary) do
    case Interface.decode(binary) do
      {:ok, interface} -> {:ok, interface}
      {:error, _} -> {:error, shape_error("undecodable #{side} interface")}
    end
  end

  defp prepare(map, _side) when is_map(map) do
    if valid_shape?(map), do: {:ok, map}, else: {:error, shape_error("malformed interface shape")}
  end

  defp prepare(_other, side), do: {:error, shape_error("undecodable #{side} interface")}

  defp valid_shape?(interface) do
    is_binary(Map.get(interface, :origin)) and is_binary(Map.get(interface, :module)) and
      is_list(Map.get(interface, :values)) and is_list(Map.get(interface, :types))
  end

  defp shape_error(reason) do
    Diagnostic.new("CMP002", reason, path: "$", details: %{reason: reason})
  end

  defp identity(interface),
    do: {Map.get(interface, :origin), Map.get(interface, :module)}

  defp class_of(changes) do
    cond do
      Enum.any?(changes, &(&1.kind == :breaking)) -> :breaking
      changes != [] -> :minor
      true -> :identical
    end
  end

  defp value_changes(old, new) do
    old_values = index(Map.get(old, :values, []), & &1.name)
    new_values = index(Map.get(new, :values, []), & &1.name)

    removal_changes(old_values, new_values, 1, "export") ++
      scheme_changes(old_values, new_values) ++
      row_changes(old_values, new_values) ++
      addition_changes(old_values, new_values, 6, "export")
  end

  defp scheme_changes(old_values, new_values) do
    Enum.flat_map(Map.keys(old_values), fn name ->
      if new_values[name] && scheme_key(old_values[name]) != scheme_key(new_values[name]) do
        [change(3, :breaking, "export #{name}", "scheme changed")]
      else
        []
      end
    end)
  end

  defp row_changes(old_values, new_values) do
    Enum.flat_map(Map.keys(old_values), fn name ->
      case {old_values[name], new_values[name]} do
        {old_value, new_value} when old_value != nil and new_value != nil ->
          old_row = row_set(old_value)
          new_row = row_set(new_value)

          cond do
            old_row == new_row ->
              []

            MapSet.subset?(old_row, new_row) ->
              [change(4, :breaking, "export #{name}", "effect row widened")]

            true ->
              [change(5, :minor, "export #{name}", "effect row narrowed")]
          end

        _ ->
          []
      end
    end)
  end

  defp type_changes(old, new) do
    old_types = index(Map.get(old, :types, []), & &1.id)
    new_types = index(Map.get(new, :types, []), & &1.id)

    removal_changes(old_types, new_types, 7, "datatype") ++
      datatype_changes(old_types, new_types) ++
      addition_changes(old_types, new_types, 8, "datatype")
  end

  defp datatype_changes(old_types, new_types) do
    Enum.flat_map(Map.keys(old_types), fn id ->
      case {old_types[id], new_types[id]} do
        {old_type, new_type} when old_type != nil and new_type != nil ->
          changes =
            if type_key(old_type) == type_key(new_type),
              do: [],
              else: [
                change(9, :breaking, "datatype #{id}", "identity, visibility, or shape changed")
              ]

          changes ++ constructor_changes(id, old_type, new_type)

        _ ->
          []
      end
    end)
  end

  defp constructor_changes(id, old_type, new_type) do
    old_constructors = index(Map.get(old_type, :constructors, []), & &1.name)
    new_constructors = index(Map.get(new_type, :constructors, []), & &1.name)

    removal_changes(old_constructors, new_constructors, 7, "constructor of #{id}") ++
      constructor_shape_changes(id, old_constructors, new_constructors) ++
      addition_changes(old_constructors, new_constructors, 8, "constructor of #{id}")
  end

  defp constructor_shape_changes(id, old_constructors, new_constructors) do
    Enum.flat_map(Map.keys(old_constructors), fn name ->
      if new_constructors[name] &&
           constructor_key(old_constructors[name]) != constructor_key(new_constructors[name]) do
        [change(7, :breaking, "constructor #{name} of #{id}", "payload changed")]
      else
        []
      end
    end)
  end

  defp categorical_changes(old, new) do
    trait_changes(old, new, :traits, 10, 11, "trait") ++
      instance_changes(old, new) ++ template_changes(old, new)
  end

  defp trait_changes(old, new, key, removed_row, added_row, subject) do
    old_traits = index(Map.get(old, key, []), &Map.get(&1, "id", inspect(&1)))
    new_traits = index(Map.get(new, key, []), &Map.get(&1, "id", inspect(&1)))

    removal_changes(old_traits, new_traits, removed_row, subject) ++
      record_changes(old_traits, new_traits, removed_row, subject) ++
      addition_changes(old_traits, new_traits, added_row, subject)
  end

  defp instance_changes(old, new),
    do: trait_changes(old, new, :instances, 10, 11, "instance")

  defp template_changes(old, new) do
    case {Map.get(old, :templates), Map.get(new, :templates)} do
      {same, same} -> []
      _other -> [change(10, :breaking, "templates", "specialization templates changed")]
    end
  end

  defp effect_changes(old, new) do
    family_changes(old, new) ++ handler_changes(old, new)
  end

  defp family_changes(old, new) do
    old_families = index(Map.get(old, :effects, []), & &1.id)
    new_families = index(Map.get(new, :effects, []), & &1.id)

    removal_changes(old_families, new_families, 10, "effect family") ++
      record_changes(old_families, new_families, 10, "effect family") ++
      addition_changes(old_families, new_families, 11, "effect family")
  end

  defp handler_changes(old, new) do
    old_handlers = index(Map.get(old, :handlers, []), & &1.id)
    new_handlers = index(Map.get(new, :handlers, []), & &1.id)

    removal_changes(old_handlers, new_handlers, 10, "handler") ++
      record_changes(old_handlers, new_handlers, 10, "handler") ++
      addition_changes(old_handlers, new_handlers, 11, "handler")
  end

  defp marker_changes(old, new) do
    old_marker = marker_of(old)
    new_marker = marker_of(new)

    if old_marker == new_marker do
      []
    else
      [
        change(
          15,
          :minor,
          "launch marker",
          "moved from #{inspect(old_marker)} to #{inspect(new_marker)}"
        )
      ]
    end
  end

  defp marker_of(entries) do
    case Enum.filter(Map.values(entries), & &1.launch) do
      [entry] -> entry.name
      [] -> nil
    end
  end

  defp result_changes(old, new) do
    Enum.flat_map(old, fn {name, entry} ->
      if new[name] && entry.result != new[name].result do
        [change(14, :breaking, "entry #{name}", "declared result changed")]
      else
        []
      end
    end)
  end

  defp removal_changes(old_index, new_index, row, subject) do
    Enum.flat_map(old_index, fn {key, _} ->
      if Map.has_key?(new_index, key),
        do: [],
        else: [change(row, :breaking, subject, "removed #{key}")]
    end)
  end

  defp addition_changes(old_index, new_index, row, subject) do
    Enum.flat_map(new_index, fn {key, _} ->
      if Map.has_key?(old_index, key),
        do: [],
        else: [change(row, :minor, subject, "added #{key}")]
    end)
  end

  defp record_changes(old_index, new_index, row, subject) do
    Enum.flat_map(old_index, fn {key, record} ->
      if Map.has_key?(new_index, key) and record != new_index[key] do
        [change(row, :breaking, subject, "changed #{key}")]
      else
        []
      end
    end)
  end

  defp index(records, key),
    do: Map.new(records, fn record -> {key.(record), record} end)

  defp change(row, kind, subject, detail),
    do: %{row: row, kind: kind, subject: subject, detail: detail}

  defp scheme_key(value) do
    {value.scheme.variables, Entry.render_type(value.scheme.type)}
  end

  defp row_set(value) do
    case Map.get(value, :uses) do
      %{entries: entries} ->
        Enum.map(entries, &entry_key/1) |> MapSet.new()

      _ ->
        MapSet.new()
    end
  end

  defp entry_key(entry) do
    {entry.family, entry.family_name, entry.capability, entry.name,
     Enum.map(Map.get(entry, :arguments, []), &Entry.render_type/1),
     Map.get(entry, :abstract?, false)}
  end

  defp type_key(type) do
    {type.id, type.arity, type.visibility, Map.get(type, :inhabitation), Map.get(type, :variance),
     Map.get(type, :positive?), Map.get(type, :regular?)}
  end

  defp constructor_key(constructor) do
    fields =
      constructor
      |> Map.get(:fields, [])
      |> Enum.map(
        &{Map.get(&1, :name), Map.get(&1, :index), Entry.render_type(Map.get(&1, :type))}
      )
      |> Enum.sort()

    {Map.get(constructor, :name), length(fields), fields}
  end

  defp allowance_of(:breaking), do: :breaking
  defp allowance_of(:minor), do: :minor
  defp allowance_of(:patch), do: :patch
  defp allowance_of(:identical), do: :patch

  defp rank(:breaking), do: 3
  defp rank(:minor), do: 2
  defp rank(:patch), do: 1
  defp rank(:identical), do: 0

  defp claim_allowance(claimed_old, claimed_new) do
    with {:ok, old_version} <- Deps.parse_version(claimed_old),
         {:ok, new_version} <- Deps.parse_version(claimed_new) do
      cond do
        new_version == old_version ->
          {:error, shape_error("claim must be an increment: #{claimed_old} -> #{claimed_new}")}

        Deps.compare(new_version, old_version) != :gt ->
          {:error, shape_error("claim must be an increment: #{claimed_old} -> #{claimed_new}")}

        new_version.major > old_version.major ->
          {:ok, :breaking}

        new_version.major >= 1 ->
          allowance_at_1x(old_version, new_version)

        true ->
          allowance_at_0x(old_version, new_version)
      end
    else
      {:error, _} -> {:error, shape_error("malformed claimed version")}
    end
  end

  defp allowance_at_1x(old_version, new_version) do
    if new_version.minor > old_version.minor do
      {:ok, :minor}
    else
      {:ok, :patch}
    end
  end

  defp allowance_at_0x(old_version, new_version) do
    if new_version.minor > old_version.minor do
      {:ok, :breaking}
    else
      {:ok, :minor}
    end
  end
end
