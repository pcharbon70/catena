defmodule Catena.Standard.Collections do
  @moduledoc "Explicit finite collection contracts; operation names are internal role labels."
  alias Catena.Foreign.{Codec, Budget}
  alias Catena.Standard.Collections.{Order, Callback}
  alias Catena.Standard.Outcomes, as: Outcome
  @origin "catena://collection-contract/0.1.65"
  @sequence :"#{@origin}::CatenaCollectionRoles::Sequence"
  @map :"#{@origin}::CatenaCollectionRoles::Keyed"
  @set :"#{@origin}::CatenaCollectionRoles::Unique"

  @external_resource "priv/stdlib/catena-collections-0.1.65.json"
  @package JSON.decode!(File.read!(@external_resource))

  def package!, do: @package

  def validate_package(value) do
    if value == @package and
         value["digest"] == Catena.Categorical.Standard.digest(Map.delete(value, "digest")) and
         value["hierarchy_digest"] == Catena.Categorical.Standard.interface!()["digest"],
       do: :ok,
       else: {:error, :invalid_collection_package}
  end

  def compile do
    with :ok <- validate_package(package!()),
         do: Catena.compile_json(JSON.encode!(package!()["ast"]), layout: :uniform)
  end

  def describe(kind, value_schema, key_order \\ nil)

  def describe(kind, value_schema, key_order) when kind in [:list, :map, :set] do
    with {:ok, value} <- Codec.new({:data, value_schema}),
         :ok <- order(kind, key_order),
         true <- kind != :set or value_schema == key_order.kind do
      {:ok, %{version: "0.1.65", kind: kind, value: value, key_order: key_order}}
    else
      _ -> {:error, :invalid_collection_description}
    end
  end

  def describe(_, _, _), do: {:error, :invalid_collection_description}

  def verify(description) do
    with {:data, schema} <- description.value.schema,
         {:ok, ^description} <- describe(description.kind, schema, description.key_order) do
      :ok
    else
      _ -> {:error, :invalid_collection_description}
    end
  rescue
    _ -> {:error, :invalid_collection_description}
  end

  defp order(:list, nil), do: :ok
  defp order(kind, evidence) when kind in [:map, :set], do: Order.verify(evidence)
  defp order(_, _), do: {:error, :invalid_collection_order}

  def construct(description, values, limits) do
    with :ok <- verify(description),
         true <- is_list(values),
         :ok <- Budget.check(values, limits),
         :ok <- validate_elements(description, values, limits) do
      case description.kind do
        :list ->
          bounded_result(Outcome.success(sequence(values)), limits)

        _ ->
          {:ok, value} = strict(description, values)
          bounded_result(value, limits)
      end
    else
      false -> {:error, :invalid_collection_input}
      error -> error
    end
  end

  def entries(description, collection, limits) do
    with :ok <- verify(description),
         :ok <- Budget.check(collection, limits),
         {:ok, values} <- unwrap(description.kind, collection),
         :ok <- validate_elements(description, values, limits),
         :ok <- sorted(description, values) do
      {:ok, values}
    end
  end

  def lookup(description, collection, key, limits) do
    with :ok <- verify(description),
         true <- description.kind in [:map, :set],
         {:ok, values} <- entries(description, collection, limits),
         {:ok, key_codec} <- Codec.new({:data, description.key_order.kind}),
         {:ok, _} <- Codec.from_native(key_codec, key, limits) do
      value =
        Enum.find(values, fn entry ->
          Order.key(description.key_order, entry_key(description.kind, entry)) ==
            Order.key(description.key_order, key)
        end)

      bounded_result(
        if(is_nil(value),
          do: Outcome.absent(),
          else: Outcome.present(if(description.kind == :map, do: elem(value, 1), else: value))
        ),
        limits
      )
    else
      false -> {:error, :not_a_keyed_collection}
      error -> error
    end
  end

  def replace(description, collection, key, value, limits) do
    with :ok <- verify(description),
         true <- description.kind == :map,
         {:ok, values} <- entries(description, collection, limits),
         :ok <- validate_elements(description, [{key, value}], limits) do
      others =
        Enum.reject(values, fn {existing, _} ->
          Order.key(description.key_order, existing) == Order.key(description.key_order, key)
        end)

      result =
        if length(others) == length(values),
          do: Outcome.absent(),
          else: Outcome.present(wrap(:map, sort(description, [{key, value} | others])))

      bounded_result(result, limits)
    else
      false -> {:error, :not_a_map_collection}
      error -> error
    end
  end

  def map_values(description, collection, callback, output_schema, limits) do
    with :ok <- verify(description),
         true <- description.kind in [:list, :map],
         {:ok, values} <- entries(description, collection, limits),
         {:ok, output} <- Codec.new({:data, output_schema}),
         {:ok, target} <- describe(description.kind, output_schema, description.key_order) do
      Callback.run(callback, description.value, output, limits, fn function ->
        mapped =
          Catena.Standard.List.map(
            fn entry ->
              if description.kind == :map,
                do: {elem(entry, 0), function.(elem(entry, 1))},
                else: function.(entry)
            end,
            values
          )

        result = wrap(description.kind, mapped)
        with :ok <- Budget.check(result, limits), do: {:ok, target, result}
      end)
    else
      false -> {:error, :mapping_set_requires_explicit_collision_policy}
      error -> error
    end
  end

  def fold(description, collection, callback, initial, accumulator_schema, limits) do
    with {:ok, values} <- entries(description, collection, limits),
         {:ok, output} <- Codec.new({:data, accumulator_schema}),
         {:ok, _} <- Codec.from_native(output, initial, limits),
         {:ok, input} <-
           Codec.new({:data, {:tuple, [accumulator_schema, element_schema(description)]}}) do
      Callback.run(callback, input, output, limits, fn function ->
        {:ok, Enum.reduce(values, initial, fn value, acc -> function.({acc, value}) end)}
      end)
    end
  end

  def combine(description, values, callback, limits) do
    with :ok <- verify(description),
         true <- description.kind == :map,
         true <- is_list(values),
         :ok <- Budget.check(values, limits),
         :ok <- validate_elements(description, values, limits),
         {:data, schema} <- description.value.schema,
         {:ok, input} <- Codec.new({:data, {:tuple, [schema, schema]}}) do
      Callback.run(callback, input, description.value, limits, fn combine ->
        combined =
          Enum.reduce(values, %{}, fn {key, value}, acc ->
            identity = Order.key(description.key_order, key)

            case Map.fetch(acc, identity) do
              :error -> Map.put(acc, identity, {key, value})
              {:ok, {original, old}} -> Map.put(acc, identity, {original, combine.({old, value})})
            end
          end)

        bounded_result(wrap(:map, sort(description, Map.values(combined))), limits)
      end)
    else
      false -> {:error, :invalid_combining_builder}
      error -> error
    end
  end

  def combine_lawful(description, values, evidence, limits) do
    alias Catena.Standard.Collections.Combining

    with :ok <- verify(description),
         :ok <- Combining.verify(evidence),
         true <- description.kind == :map and description.value.schema == {:data, evidence.schema},
         true <- is_list(values),
         :ok <- Budget.check(values, limits),
         :ok <- validate_elements(description, values, limits) do
      result =
        Enum.reduce_while(values, {:ok, %{}}, fn {key, value}, {:ok, acc} ->
          identity = Order.key(description.key_order, key)

          entry =
            case Map.fetch(acc, identity) do
              :error -> {key, value}
              {:ok, {original, old}} -> {original, Combining.apply(evidence, old, value)}
            end

          case Budget.check(entry, limits) do
            :ok -> {:cont, {:ok, Map.put(acc, identity, entry)}}
            error -> {:halt, error}
          end
        end)

      with {:ok, entries} <- result,
           do: bounded_result(wrap(:map, sort(description, Map.values(entries))), limits)
    else
      false -> {:error, :invalid_lawful_builder_input}
      error -> error
    end
  end

  def fold_while(description, collection, callback, initial, schema, limits) do
    with {:ok, values} <- entries(description, collection, limits),
         {:ok, acc_codec} <- Codec.new({:data, schema}),
         {:ok, _} <- Codec.from_native(acc_codec, initial, limits),
         {:ok, input} <- Codec.new({:data, {:tuple, [schema, element_schema(description)]}}),
         {:ok, output} <-
           Codec.new({:data, {:variant, %{"continue" => schema, "stop" => schema}}}) do
      Callback.run(callback, input, output, limits, fn step ->
        result =
          Catena.Standard.List.fold_while(
            fn acc, value ->
              case step.({acc, value}) do
                {:catena_variant, :continue, next} -> {:continue, next}
                {:catena_variant, :stop, next} -> {:stop, next}
              end
            end,
            initial,
            values
          )

        bounded_result(result, limits)
      end)
    end
  end

  def list_lookup(description, collection, index, limits) when is_integer(index) do
    with :ok <- verify(description),
         true <- description.kind == :list,
         {:ok, values} <- entries(description, collection, limits) do
      result = if index < 0, do: :error, else: Enum.fetch(values, index)

      bounded_result(
        case result do
          {:ok, value} -> Outcome.present(value)
          :error -> Outcome.absent()
        end,
        limits
      )
    else
      false -> {:error, :not_a_list_collection}
      error -> error
    end
  end

  def list_lookup(_, _, _, _), do: {:error, :invalid_list_index}

  def transform_set(description, collection, callback, target_order, limits) do
    with :ok <- verify(description),
         true <- description.kind == :set,
         {:ok, values} <- entries(description, collection, limits),
         :ok <- Order.verify(target_order),
         {:ok, target} <- describe(:set, target_order.kind, target_order),
         {:ok, input} <- Codec.new({:data, description.key_order.kind}),
         {:ok, output} <- Codec.new({:data, target_order.kind}) do
      Callback.run(callback, input, output, limits, fn transform ->
        construct(target, Catena.Standard.List.map(transform, values), limits)
      end)
    else
      false -> {:error, :invalid_set_transform}
      error -> error
    end
  end

  # The callback wire variants are explicit C095 adapters to C103 nominal
  # outcomes. No exception or trap becomes an expected failure.
  def traverse(description, collection, callback, mode, output_schema, error_schema, limits)
      when mode in [:optional, :dependent, :independent] do
    with :ok <- verify(description),
         true <- description.kind in [:list, :map],
         {:ok, values} <- entries(description, collection, limits),
         {:ok, target} <- describe(description.kind, output_schema, description.key_order),
         {:ok, output} <- traversal_codec(mode, output_schema, error_schema) do
      Callback.run(callback, description.value, output, limits, fn function ->
        {reversed, errors, stopped} =
          Enum.reduce_while(values, {[], [], false}, fn entry, {acc, errors, _} ->
            value = if description.kind == :map, do: elem(entry, 1), else: entry

            case function.(value) do
              {:catena_variant, :success, mapped} ->
                mapped = if description.kind == :map, do: {elem(entry, 0), mapped}, else: mapped
                {:cont, {[mapped | acc], errors, false}}

              {:catena_variant, :failure, error} when mode == :independent ->
                {:cont, {acc, [error | errors], false}}

              {:catena_variant, :failure, error} ->
                {:halt, {acc, [error], true}}
            end
          end)

        result =
          case {mode, errors, stopped} do
            {:optional, [], false} ->
              Outcome.present(wrap(description.kind, Enum.reverse(reversed)))

            {:optional, _, _} ->
              Outcome.absent()

            {:dependent, [], false} ->
              Outcome.success(wrap(description.kind, Enum.reverse(reversed)))

            {:dependent, [error], true} ->
              Outcome.failure(error)

            {:independent, [], false} ->
              Outcome.valid(wrap(description.kind, Enum.reverse(reversed)))

            {:independent, errors, false} ->
              Outcome.invalid(Outcome.errors(Enum.reverse(errors)))
          end

        with :ok <- Budget.check(result, limits), do: {:ok, target, result}
      end)
    else
      false -> {:error, :traversal_requires_list_or_map}
      error -> error
    end
  end

  def traverse(_, _, _, _, _, _, _), do: {:error, :invalid_traversal_mode}

  defp traversal_codec(:optional, schema, :unit),
    do: Codec.new({:data, {:variant, %{"success" => schema, "failure" => :unit}}})

  defp traversal_codec(:optional, _, _), do: {:error, :optional_failure_requires_unit}

  defp traversal_codec(_, schema, error),
    do: Codec.new({:data, {:variant, %{"success" => schema, "failure" => error}}})

  # Ordinary categorical dictionary implementations receive already typed pure
  # callbacks. The explicit host-facing methods above require verified descriptors.
  def sequence_map(callback, subject) when is_function(callback, 1) do
    {:ok, values} = sequence_values(subject, [])
    sequence(Catena.Standard.List.map(callback, values))
  end

  def sequence_summarize(callback, initial, subject) when is_function(callback, 1) do
    {:ok, values} = sequence_values(subject, [])
    Catena.Standard.List.summarize(callback, initial, values)
  end

  def keyed_map(callback, {:catena_adt, @map, 0, {subject}}) when is_function(callback, 1) do
    {:ok, values} = sequence_values(subject, [])
    wrap(:map, Catena.Standard.List.map(fn {key, value} -> {key, callback.(value)} end, values))
  end

  def keyed_summarize(callback, initial, {:catena_adt, @map, 0, {subject}})
      when is_function(callback, 1) do
    {:ok, values} = sequence_values(subject, [])
    Enum.reduce(values, initial, fn {_, value}, acc -> callback.(acc).(value) end)
  end

  defp bounded_result(value, limits) do
    with :ok <- Budget.check(value, limits), do: {:ok, value}
  end

  defp strict(description, values) do
    result =
      values
      |> Enum.with_index()
      |> Enum.reduce_while({:ok, %{}}, fn {entry, index}, {:ok, seen} ->
        key = entry_key(description.kind, entry)
        identity = Order.key(description.key_order, key)

        if Map.has_key?(seen, identity),
          do: {:halt, {:duplicate, key, index}},
          else: {:cont, {:ok, Map.put(seen, identity, entry)}}
      end)

    case result do
      {:duplicate, key, index} ->
        {:ok, Outcome.failure({key, index})}

      {:ok, seen} ->
        {:ok, Outcome.success(wrap(description.kind, sort(description, Map.values(seen))))}
    end
  end

  defp sort(description, values),
    do:
      Enum.sort(values, fn a, b ->
        {:ok, order} =
          Order.compare(
            description.key_order,
            entry_key(description.kind, a),
            entry_key(description.kind, b)
          )

        order != :gt
      end)

  defp sorted(%{kind: :list}, _), do: :ok
  defp sorted(_, []), do: :ok
  defp sorted(_, [_]), do: :ok

  defp sorted(d, [a, b | rest]) do
    case Order.compare(d.key_order, entry_key(d.kind, a), entry_key(d.kind, b)) do
      {:ok, :lt} -> sorted(d, [b | rest])
      _ -> {:error, :noncanonical_key_order_or_duplicate}
    end
  end

  defp validate_elements(d, values, limits) do
    {:ok, codec} = Codec.new({:data, element_schema(d)})

    Enum.reduce_while(values, :ok, fn value, :ok ->
      case Codec.from_native(codec, value, limits) do
        {:ok, _} -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp element_schema(%{kind: :map, key_order: order, value: %{schema: {:data, value}}}),
    do: {:tuple, [order.kind, value]}

  defp element_schema(%{kind: :set, key_order: order}), do: order.kind
  defp element_schema(%{value: %{schema: {:data, value}}}), do: value
  defp entry_key(:map, {key, _}), do: key
  defp entry_key(:set, key), do: key

  def sequence(values),
    do:
      Enum.reduce(:lists.reverse(values), {:catena_adt, @sequence, 0, {}}, fn value, tail ->
        {:catena_adt, @sequence, 1, {value, tail}}
      end)

  defp wrap(:list, values), do: sequence(values)
  defp wrap(:map, values), do: {:catena_adt, @map, 0, {sequence(values)}}
  defp wrap(:set, values), do: {:catena_adt, @set, 0, {sequence(values)}}
  defp unwrap(:list, values), do: sequence_values(values, [])
  defp unwrap(:map, {:catena_adt, @map, 0, {values}}), do: sequence_values(values, [])
  defp unwrap(:set, {:catena_adt, @set, 0, {values}}), do: sequence_values(values, [])
  defp unwrap(_, _), do: {:error, :invalid_nominal_collection}
  defp sequence_values({:catena_adt, @sequence, 0, {}}, acc), do: {:ok, :lists.reverse(acc)}

  defp sequence_values({:catena_adt, @sequence, 1, {value, tail}}, acc),
    do: sequence_values(tail, [value | acc])

  defp sequence_values(_, _), do: {:error, :invalid_nominal_sequence}
end
