defmodule Catena.Standard.Outcomes do
  @moduledoc "Explicit outcome contract package. Role labels are internal, not adopted vocabulary."
  alias Catena.Categorical.Standard
  @origin "catena://outcome-contract/0.1.54"
  @module "CatenaOutcomeRoles"
  @path Application.app_dir(:catena, "priv/stdlib/catena-outcomes-0.1.54.json")
  @optional :"#{@origin}::#{@module}::Optional"
  @dependent :"#{@origin}::#{@module}::Dependent"
  @independent :"#{@origin}::#{@module}::Independent"
  @sequence :"#{@origin}::#{@module}::Sequence"
  @nonempty :"#{@origin}::#{@module}::Nonempty"

  def package! do
    @path |> File.read!() |> JSON.decode!() |> validate_package!()
  end

  def validate_package!(value) when is_map(value) do
    unless value["format"] == "catena-outcome-package" and value["contract"] == "0.1.54" and
             value["layout"] == "uniform" and get_in(value, ["ast", "origin"]) == @origin and
             get_in(value, ["ast", "module"]) == @module and
             value["digest"] == Standard.digest(Map.delete(value, "digest")) and
             value["hierarchy_digest"] == Standard.interface!()["digest"] do
      raise ArgumentError, "outcome package digest mismatch"
    end

    value
  end

  def compile do
    Catena.compile_json(JSON.encode!(package!()["ast"]), layout: :uniform)
  end

  def absent, do: {:catena_adt, @optional, 0, {}}
  def present(value), do: {:catena_adt, @optional, 1, {value}}
  def failure(error), do: {:catena_adt, @dependent, 0, {error}}
  def success(value), do: {:catena_adt, @dependent, 1, {value}}
  def valid(value), do: {:catena_adt, @independent, 1, {value}}

  def invalid({:catena_adt, @nonempty, 0, {_head, _tail}} = errors),
    do: {:catena_adt, @independent, 0, {errors}}

  def sequence(values) when is_list(values) do
    Enum.reduce(:lists.reverse(values), {:catena_adt, @sequence, 0, {}}, fn value, rest ->
      {:catena_adt, @sequence, 1, {value, rest}}
    end)
  end

  def errors([head | tail]), do: {:catena_adt, @nonempty, 0, {head, sequence(tail)}}
  def errors([]), do: raise(ArgumentError, "invalid outcomes require a nonempty error sequence")

  def errors_list({:catena_adt, @nonempty, 0, {head, tail}}),
    do: [head | sequence_list(tail, [])]

  defp sequence_list({:catena_adt, @sequence, 0, {}}, acc), do: :lists.reverse(acc)

  defp sequence_list({:catena_adt, @sequence, 1, {head, tail}}, acc),
    do: sequence_list(tail, [head | acc])

  def errors_append(first, second), do: errors(errors_list(first) ++ errors_list(second))

  def optional_embed(value), do: present(value)
  def dependent_embed(value), do: success(value)
  def independent_embed(value), do: valid(value)

  def optional_map(callback, {:catena_adt, @optional, 0, {}} = subject)
      when is_function(callback, 1), do: subject

  def optional_map(callback, {:catena_adt, @optional, 1, {value}}), do: present(callback.(value))

  def dependent_map(callback, {:catena_adt, @dependent, 0, {_}} = subject)
      when is_function(callback, 1), do: subject

  def dependent_map(callback, {:catena_adt, @dependent, 1, {value}}),
    do: success(callback.(value))

  def independent_map(callback, {:catena_adt, @independent, 0, {_}} = subject)
      when is_function(callback, 1), do: subject

  def independent_map(callback, {:catena_adt, @independent, 1, {value}}),
    do: valid(callback.(value))

  def optional_chain(callback, {:catena_adt, @optional, 0, {}} = subject)
      when is_function(callback, 1), do: subject

  def optional_chain(callback, {:catena_adt, @optional, 1, {value}}), do: callback.(value)

  def dependent_chain(callback, {:catena_adt, @dependent, 0, {_}} = subject)
      when is_function(callback, 1), do: subject

  def dependent_chain(callback, {:catena_adt, @dependent, 1, {value}}), do: callback.(value)

  def optional_map2(
        callback,
        {:catena_adt, @optional, 1, {left}},
        {:catena_adt, @optional, 1, {right}}
      ),
      do: present(callback.(left).(right))

  def optional_map2(callback, {:catena_adt, @optional, _, _}, {:catena_adt, @optional, _, _})
      when is_function(callback, 1), do: absent()

  def dependent_map2(
        callback,
        {:catena_adt, @dependent, 0, {_}} = first,
        {:catena_adt, @dependent, _, _}
      )
      when is_function(callback, 1), do: first

  def dependent_map2(
        callback,
        {:catena_adt, @dependent, 1, {_}},
        {:catena_adt, @dependent, 0, {_}} = second
      )
      when is_function(callback, 1), do: second

  def dependent_map2(
        callback,
        {:catena_adt, @dependent, 1, {left}},
        {:catena_adt, @dependent, 1, {right}}
      ),
      do: success(callback.(left).(right))

  def independent_map2(
        callback,
        {:catena_adt, @independent, 0, {first}},
        {:catena_adt, @independent, 0, {second}}
      )
      when is_function(callback, 1), do: invalid(errors_append(first, second))

  def independent_map2(
        callback,
        {:catena_adt, @independent, 0, {_}} = first,
        {:catena_adt, @independent, 1, {_}}
      )
      when is_function(callback, 1), do: first

  def independent_map2(
        callback,
        {:catena_adt, @independent, 1, {_}},
        {:catena_adt, @independent, 0, {_}} = second
      )
      when is_function(callback, 1), do: second

  def independent_map2(
        callback,
        {:catena_adt, @independent, 1, {left}},
        {:catena_adt, @independent, 1, {right}}
      ),
      do: valid(callback.(left).(right))
end
