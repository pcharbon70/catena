defmodule Catena.Optimizer do
  @moduledoc """
  Checked, replayable optimization for the admitted pure kernel fragment.

  Revision 0.1.86 enables only literal arithmetic and right-identity rewrites.
  Every applied rewrite carries machine-checkable local premises and the whole
  result is replayed from the independently verified input before lowering.
  """

  alias Catena.Kernel.Verifier

  @version "0.1.86"
  @safe_rules [:fold_integer_literals, :right_integer_identity]
  @known_rules @safe_rules ++ [:integer_annihilation]

  def profile do
    %{
      version: @version,
      modes: [:disabled, :checked],
      enabled_rules: @safe_rules,
      refused_rules: [:integer_annihilation],
      required_premises: [
        :typed_core,
        :pure_removed_expression,
        :total_operation,
        :order_preserved
      ],
      inventories: %{
        specialization: :separate_verified_lowering,
        layout_selection: :representation_only,
        condition_lowering: :semantics_preserving_backend,
        comprehension_expansion: :checked_elaboration,
        backend_lowering: :verified_core_input
      },
      public_source: :held_for_p109,
      finite_testing_is_proof: false
    }
  end

  def optimize(core, options \\ []) do
    mode = Keyword.get(options, :mode, :disabled)
    rules = Keyword.get(options, :rules, @safe_rules)

    with :ok <- validate_mode(mode),
         :ok <- validate_rules(rules),
         :ok <- Verifier.verify(core) do
      result =
        case mode do
          :disabled ->
            result(core, core, [], [], mode, rules)

          :checked ->
            {optimized, certificates, rejected} = walk(core, [], rules)
            result(core, optimized, certificates, rejected, mode, rules)
        end

      with :ok <- Verifier.verify(result.core),
           :ok <- verify_result(core, result),
           do: {:ok, result}
    else
      {:error, _} = error -> error
      other -> {:error, {:invalid_optimizer_input, other}}
    end
  rescue
    _ -> {:error, :invalid_optimizer_input}
  end

  def evidence(result), do: Map.drop(result, [:core]) |> Map.put(:version, @version)

  def verify_result(original, result) when is_map(result) do
    with true <- result.input_digest == digest(original),
         true <- result.output_digest == digest(result.core),
         {:ok, replayed} <- replay(original, result.certificates),
         true <- replayed == result.core,
         :ok <- Verifier.verify(replayed) do
      :ok
    else
      _ -> {:error, :invalid_optimizer_certificate}
    end
  rescue
    _ -> {:error, :invalid_optimizer_certificate}
  end

  def verify_result(_original, _result), do: {:error, :invalid_optimizer_certificate}

  defp result(input, output, certificates, rejected, mode, rules) do
    %{
      core: output,
      mode: mode,
      rules: rules,
      certificates: certificates,
      rejected: rejected,
      input_digest: digest(input),
      output_digest: digest(output)
    }
  end

  defp walk(value, path, rules) when is_list(value) do
    value
    |> Enum.with_index()
    |> Enum.reduce({[], [], []}, fn {item, index}, {items, certificates, rejected} ->
      {updated, item_certificates, item_rejected} = walk(item, path ++ [index], rules)
      {items ++ [updated], certificates ++ item_certificates, rejected ++ item_rejected}
    end)
  end

  defp walk(%{__struct__: _} = value, _path, _rules), do: {value, [], []}

  defp walk(value, path, rules) when is_map(value) do
    {descended, certificates, rejected} =
      value
      |> Map.keys()
      |> Enum.sort_by(&to_string/1)
      |> Enum.reduce({value, [], []}, fn key, {map, certs, refusals} ->
        {updated, child_certs, child_refusals} = walk(Map.fetch!(map, key), path ++ [key], rules)
        {Map.put(map, key, updated), certs ++ child_certs, refusals ++ child_refusals}
      end)

    case choose_rewrite(descended, rules) do
      {:apply, rule, replacement, premises} ->
        {replacement, certificates ++ [certificate(rule, path, descended, replacement, premises)],
         rejected}

      {:refuse, rule, reason} ->
        refusal = %{rule: rule, path: path, expression_digest: digest(descended), reason: reason}
        {descended, certificates, rejected ++ [refusal]}

      :none ->
        {descended, certificates, rejected}
    end
  end

  defp walk(value, _path, _rules), do: {value, [], []}

  defp choose_rewrite(expression, rules) do
    cond do
      :fold_integer_literals in rules and
          match?({:ok, _}, rewrite_once(expression, :fold_integer_literals)) ->
        {:ok, replacement} = rewrite_once(expression, :fold_integer_literals)
        {:apply, :fold_integer_literals, replacement, premises(:fold_integer_literals)}

      :right_integer_identity in rules and
          match?({:ok, _}, rewrite_once(expression, :right_integer_identity)) ->
        {:ok, replacement} = rewrite_once(expression, :right_integer_identity)
        {:apply, :right_integer_identity, replacement, premises(:right_integer_identity)}

      annihilation_opportunity?(expression) ->
        {:refuse, :integer_annihilation, :requires_machine_checked_purity_and_totality}

      true ->
        :none
    end
  end

  defp rewrite_once(
         %{
           tag: :binary,
           operator: operator,
           left: %{tag: :integer, value: left} = literal,
           right: %{tag: :integer, value: right}
         } = expression,
         :fold_integer_literals
       )
       when operator in [:add, :subtract, :multiply] and is_integer(left) and is_integer(right) do
    value =
      case operator do
        :add -> left + right
        :subtract -> left - right
        :multiply -> left * right
      end

    {:ok, literal |> Map.put(:value, value) |> inherit_observation(expression)}
  end

  defp rewrite_once(
         %{tag: :binary, operator: :add, left: left, right: %{tag: :integer, value: 0}} =
           expression,
         :right_integer_identity
       ),
       do: {:ok, inherit_observation(left, expression)}

  defp rewrite_once(
         %{tag: :binary, operator: :multiply, left: left, right: %{tag: :integer, value: 1}} =
           expression,
         :right_integer_identity
       ),
       do: {:ok, inherit_observation(left, expression)}

  defp rewrite_once(_expression, _rule), do: :not_applicable

  defp annihilation_opportunity?(%{
         tag: :binary,
         operator: :multiply,
         left: %{tag: :integer, value: 0},
         right: right
       }),
       do: not literal_integer?(right)

  defp annihilation_opportunity?(%{
         tag: :binary,
         operator: :multiply,
         left: left,
         right: %{tag: :integer, value: 0}
       }),
       do: not literal_integer?(left)

  defp annihilation_opportunity?(_), do: false

  defp literal_integer?(%{tag: :integer, value: value}), do: is_integer(value)
  defp literal_integer?(_), do: false

  defp inherit_observation(node, expression) do
    Enum.reduce([:span, :type, :effects], node, fn key, result ->
      if Map.has_key?(expression, key),
        do: Map.put(result, key, Map.fetch!(expression, key)),
        else: result
    end)
  end

  defp premises(:fold_integer_literals),
    do: [
      :typed_core,
      :pure_removed_expression,
      :total_integer_operation,
      :left_to_right_order_preserved
    ]

  defp premises(:right_integer_identity),
    do: [:typed_core, :pure_removed_literal, :total_identity_law, :left_to_right_order_preserved]

  defp certificate(rule, path, before, replacement, premises) do
    payload = %{
      rule: rule,
      path: path,
      before_digest: digest(before),
      after_digest: digest(replacement),
      premises: premises
    }

    Map.put(payload, :digest, digest(payload))
  end

  defp replay(core, certificates) when is_list(certificates) do
    Enum.reduce_while(certificates, {:ok, core}, fn certificate, {:ok, current} ->
      with {:ok, before} <- fetch_path(current, certificate.path),
           true <- certificate.before_digest == digest(before),
           true <- certificate.digest == digest(Map.delete(certificate, :digest)),
           true <- certificate.premises == premises(certificate.rule),
           {:ok, replacement} <- rewrite_once(before, certificate.rule),
           true <- certificate.after_digest == digest(replacement),
           {:ok, updated} <- put_path(current, certificate.path, replacement) do
        {:cont, {:ok, updated}}
      else
        _ -> {:halt, {:error, :invalid_optimizer_certificate}}
      end
    end)
  end

  defp replay(_core, _certificates), do: {:error, :invalid_optimizer_certificate}

  defp fetch_path(value, []), do: {:ok, value}

  defp fetch_path(value, [index | rest]) when is_list(value) and is_integer(index) do
    case Enum.fetch(value, index) do
      {:ok, item} -> fetch_path(item, rest)
      :error -> :error
    end
  end

  defp fetch_path(value, [key | rest]) when is_map(value) do
    case Map.fetch(value, key) do
      {:ok, item} -> fetch_path(item, rest)
      :error -> :error
    end
  end

  defp fetch_path(_value, _path), do: :error

  defp put_path(_value, [], replacement), do: {:ok, replacement}

  defp put_path(value, [index | rest], replacement) when is_list(value) and is_integer(index) do
    with {:ok, child} <- Enum.fetch(value, index),
         {:ok, updated} <- put_path(child, rest, replacement),
         do: {:ok, List.replace_at(value, index, updated)}
  end

  defp put_path(value, [key | rest], replacement) when is_map(value) do
    with {:ok, child} <- Map.fetch(value, key),
         {:ok, updated} <- put_path(child, rest, replacement),
         do: {:ok, Map.put(value, key, updated)}
  end

  defp put_path(_value, _path, _replacement), do: :error

  defp validate_mode(mode) when mode in [:disabled, :checked], do: :ok
  defp validate_mode(_), do: {:error, :invalid_optimizer_mode}

  defp validate_rules(rules) when is_list(rules) do
    if length(rules) == length(Enum.uniq(rules)) and Enum.all?(rules, &(&1 in @known_rules)),
      do: :ok,
      else: {:error, :invalid_optimizer_rules}
  end

  defp validate_rules(_), do: {:error, :invalid_optimizer_rules}

  defp digest(term),
    do:
      :crypto.hash(:sha256, :erlang.term_to_binary(term, [:deterministic]))
      |> Base.encode16(case: :lower)
end
