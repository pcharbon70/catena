defmodule Catena.Comprehension do
  @moduledoc """
  The dormant elaboration boundary for revision `0.1.39` list
  comprehensions.

  `elaborate/1` maps a qualifier tree to a kernel S-expression
  module whose fused tail-recursive worker chain computes the
  comprehension: one recursive definition per generator depth
  sharing a single output accumulator, filters and bindings inline,
  and a final ordering pass. No frozen frontend carries
  comprehension expressions; this boundary is the executable
  surface until the surface-grammar capstone adopts the tokens.

  Qualifiers carry kernel fragment strings; the elaborator threads
  caller-declared binder types and the kernel checker remains the
  type authority. The elaborated module declares its own `List`
  nominal type — callers must not re-declare it.
  """

  alias Catena.Diagnostic

  @enforce_keys [:module, :origin, :qualifiers, :yield, :result_element_type]
  defstruct [
    :module,
    :origin,
    :qualifiers,
    :yield,
    :result_element_type,
    types: [],
    uses: [],
    context: []
  ]

  @type qualifier ::
          {:generator,
           [
             pattern: String.t(),
             element_type: String.t(),
             source: String.t(),
             binds: [{String.t(), String.t()}]
           ]}
          | {:case_generator,
             [
               pattern: String.t(),
               element_type: String.t(),
               source: String.t(),
               binds: [{String.t(), String.t()}]
             ]}
          | {:filter, expr: String.t()}
          | {:let,
             [
               pattern: String.t(),
               value_type: String.t(),
               expr: String.t(),
               binds: [{String.t(), String.t()}]
             ]}

  @type spec :: %__MODULE__{}

  @list_type """
  (data List
    (params a)
    (constructor Nil (fields))
    (constructor Cons (fields a (List a))))
  """

  @bind_regex ~r/\(bind ([a-z_][a-zA-Z0-9_]*)\)/
  @constructor_regex ~r/\(constructor ([A-Z][a-zA-Z0-9_]*)/
  @declared_constructor_regex ~r/\(constructor ([A-Z][a-zA-Z0-9_]*)/
  @type_head_regex ~r/^\(([A-Z][a-zA-Z0-9_]*)/

  @spec new(keyword()) :: spec()
  def new(fields), do: struct!(__MODULE__, fields)

  @doc """
  Elaborates the qualifier tree into a kernel module source.

  Returns `{:ok, source, advisories}` or `{:error, diagnostic}`.
  Advisories carry `LCP003` (unnecessary filtering marker) and
  `BS001` (unused binding) identities; errors carry `LCP001`
  (same-comprehension rebinding) and `LCP002` (filtering pattern
  that can never match).
  """
  @spec elaborate(spec()) :: {:ok, String.t(), [Diagnostic.t()]} | {:error, Diagnostic.t()}
  def elaborate(%__MODULE__{} = spec) do
    with :ok <- check_first_qualifier(spec.qualifiers),
         :ok <- check_rebinding(spec.qualifiers),
         :ok <- check_case_patterns(spec) do
      {source, advisories} = render(spec, [])
      {:ok, source, advisories}
    end
  end

  defp check_first_qualifier([first | _]) do
    case first do
      {kind, _} when kind in [:generator, :case_generator] -> :ok
      _ -> raise ArgumentError, "the first qualifier must be a generator"
    end
  end

  defp check_rebinding(qualifiers) do
    {_ordered, _seen, duplicate} =
      Enum.reduce_while(qualifiers, {[], MapSet.new(), nil}, fn qualifier, {ordered, seen, _} ->
        binders = qualifier_binders(qualifier)

        duplicate =
          Enum.find(binders, fn name -> MapSet.member?(seen, name) end)

        if duplicate do
          {:halt, {ordered, seen, duplicate}}
        else
          {:cont, {ordered ++ binders, MapSet.union(seen, MapSet.new(binders)), nil}}
        end
      end)

    case duplicate do
      nil ->
        :ok

      name ->
        {:error,
         %Diagnostic{
           id: "LCP001",
           message: "name #{name} is rebound in the same comprehension",
           severity: :error
         }}
    end
  end

  defp qualifier_binders({kind, fields}) when kind in [:generator, :case_generator] do
    pattern_binders(fields[:pattern])
  end

  defp qualifier_binders({:let, fields}), do: pattern_binders(fields[:pattern])
  defp qualifier_binders({:filter, _}), do: []

  defp pattern_binders(pattern) do
    @bind_regex
    |> Regex.scan(pattern, capture: :all_but_first)
    |> List.flatten()
  end

  defp check_case_patterns(spec) do
    spec.qualifiers
    |> Enum.with_index()
    |> Enum.reduce_while(:ok, fn
      {{:case_generator, fields}, _index}, :ok ->
        case never_matches?(fields, spec.types) do
          true ->
            {:halt,
             {:error,
              %Diagnostic{
                id: "LCP002",
                message:
                  "this filtering generator's pattern can never match: it names a constructor the element type does not declare",
                severity: :error
              }}}

          false ->
            {:cont, :ok}
        end

      _, :ok ->
        {:cont, :ok}
    end)
  end

  defp never_matches?(fields, types) do
    pattern = fields[:pattern]
    element_type = fields[:element_type]

    pattern_constructor = first_capture(@constructor_regex, pattern)
    type_name = first_capture(@type_head_regex, element_type)

    with false <- is_nil(pattern_constructor),
         false <- is_nil(type_name),
         declared when is_map(declared) and map_size(declared) > 0 <-
           declared_constructors(type_name, types),
         false <- MapSet.member?(declared, pattern_constructor) do
      true
    else
      _ -> false
    end
  end

  defp first_capture(regex, subject) do
    case Regex.run(regex, subject) do
      [_whole, capture] -> capture
      _ -> nil
    end
  end

  defp declared_constructors(type_name, types) do
    types
    |> Enum.find(fn declaration -> declaration =~ "(data #{type_name} " end)
    |> case do
      nil ->
        MapSet.new()

      declaration ->
        @declared_constructor_regex
        |> Regex.scan(declaration, capture: :all_but_first)
        |> List.flatten()
        |> MapSet.new()
    end
  end

  defp render(spec, _options) do
    prefix = prefix(spec.module)
    uses = uses_field(spec.uses)

    {worker_defs, _} =
      workers(spec, prefix, {[], [], [], 0})

    reverse_def = reverse_definition(spec, prefix, uses)

    source =
      ([
         "(module #{spec.module}",
         "  (edition 0.1)",
         "  (revision 0.1.8)",
         "  (origin \"#{spec.origin}\")",
         "  (export type List)" <>
           export_values(
             Enum.map(spec.context || [], &elem(&1, 0)) ++
               Enum.map(worker_defs, & &1.name) ++ ["#{prefix}_reverse", "main"]
           ) <>
           export_types(spec.types || []),
         "  #{@list_type}" | Enum.map(spec.types, &"  #{&1}")
       ] ++
         Enum.flat_map(spec.context || [], fn {name, type, expr} ->
           ["  (def #{name}", "    (signature #{type} #{uses})", "    #{expr})"]
         end) ++
         Enum.flat_map(worker_defs, fn worker ->
           [
             "  (def #{worker.name}",
             "    (signature #{worker.signature} #{uses})",
             "    #{worker.body})"
           ]
         end) ++
         [
           "  (def #{prefix}_reverse",
           "    (signature #{reverse_signature(spec)} #{uses})",
           "    #{reverse_def})",
           "  (def main",
           "    (signature (List #{spec.result_element_type}) #{uses})",
           "    (call (call (var #{prefix}_go1) #{first_source(spec.qualifiers)}) (construct Nil)))",
           ")"
         ])
      |> Enum.join("\n")

    advisories =
      case marker_advisories(spec) do
        [] -> unused_advisories(spec)
        other -> other ++ unused_advisories(spec)
      end

    {source, advisories}
  end

  defp export_values(names),
    do: Enum.map_join(names, "", &"\n  (export value #{&1})")

  defp export_types(types) do
    types
    |> Enum.map_join("", fn declaration ->
      case Regex.run(~r/\(data ([A-Z][a-zA-Z0-9_]*)/, declaration) do
        [_, name] -> "\n  (export type #{name})"
        _ -> ""
      end
    end)
  end

  defp prefix(module), do: module |> Macro.underscore()

  defp uses_field([]), do: "(uses)"
  defp uses_field(uses), do: "(uses #{Enum.join(uses, " ")})"

  defp first_source([{:generator, fields} | _]), do: fields[:source]
  defp first_source([{:case_generator, fields} | _]), do: fields[:source]

  defp workers(spec, prefix, _initial) do
    {acc_type, result_element_type} =
      {"(List #{spec.result_element_type})", spec.result_element_type}

    generators = generator_indices(spec.qualifiers)

    {worker_defs, _acc} =
      Enum.reduce(Enum.with_index(spec.qualifiers), {[], []}, fn {qualifier, index},
                                                                 {defs, scopes} ->
        case qualifier do
          {kind, fields} when kind in [:generator, :case_generator] ->
            depth = Enum.count(generators, fn g -> g < index end) + 1
            binds = scopes |> List.flatten()

            worker = worker_definition(spec, prefix, qualifier, depth, binds, acc_type)
            {[worker | defs], scopes ++ [fields[:binds] || []]}

          {:let, fields} ->
            {defs, scopes ++ [fields[:binds] || []]}

          {:filter, _} ->
            {defs, scopes}
        end
      end)

    {Enum.reverse(worker_defs), result_element_type}
  end

  defp generator_indices(qualifiers) do
    for {qualifier, index} <- Enum.with_index(qualifiers),
        elem(qualifier, 0) in [:generator, :case_generator],
        do: index
  end

  defp worker_definition(spec, prefix, {kind, fields}, depth, binds, acc_type) do
    element_type = fields[:element_type]
    pattern = fields[:pattern]
    source_param = "c047_source#{depth}"
    acc_param = "c047_acc#{depth}"
    head_param = "c047_head#{depth}"
    rest_param = "c047_rest#{depth}"
    worker = "#{prefix}_go#{depth}"

    inner_bind_args = var_args(binds)
    continuation = continuation(spec, prefix, depth, binds, rest_param, acc_param, fields)

    destructure =
      case kind do
        :generator ->
          match_form(var_form(head_param), [clause_form(pattern, [continuation])])

        :case_generator ->
          advance =
            advance_call(worker, rest_param, acc_param, inner_bind_args)

          match_form(var_form(head_param), [
            clause_form(pattern, [continuation]),
            clause_form("_", [advance])
          ])
      end

    inner_body =
      Enum.reduce(
        Enum.reverse(binds),
        match_form(var_form(source_param), [
          clause_form(
            "(constructor Nil)",
            [nil_case(spec, prefix, depth, acc_param, inner_bind_args)]
          ),
          clause_form(
            "(constructor Cons (bind #{head_param}) (bind #{rest_param}))",
            [destructure]
          )
        ]),
        fn {name, type}, wrapped -> fn_form(name, type, wrapped) end
      )

    body =
      fn_form(source_param, "(List #{element_type})", fn_form(acc_param, acc_type, inner_body))

    signature =
      fn_type(
        "(List #{element_type})",
        [acc_type | Enum.map(binds, fn {_n, t} -> t end)],
        acc_type
      )

    %{name: worker, signature: signature, body: body}
  end

  defp continuation(spec, prefix, depth, binds, rest_param, acc_param, generator_fields) do
    qualifiers = spec.qualifiers
    generator_positions = generator_indices(qualifiers)
    own_index = Enum.at(generator_positions, depth - 1)

    suffix_qualifiers =
      qualifiers
      |> Enum.drop(own_index + 1)

    inner_suffix(
      suffix_qualifiers,
      spec,
      prefix,
      depth,
      binds,
      binds ++ (generator_fields[:binds] || []),
      rest_param,
      acc_param
    )
  end

  defp inner_suffix([], spec, prefix, depth, binds, _scope_binds, rest_param, acc_param) do
    worker = "#{prefix}_go#{depth}"

    apply_chain(
      var_form(worker),
      [
        var_form(rest_param),
        construct_cons(spec.yield, var_form(acc_param)) | var_args(binds)
      ]
    )
  end

  defp inner_suffix(
         [{:filter, expr: expr} | rest],
         spec,
         prefix,
         depth,
         binds,
         scope_binds,
         rest_param,
         acc_param
       ) do
    continue = inner_suffix(rest, spec, prefix, depth, binds, scope_binds, rest_param, acc_param)
    worker = "#{prefix}_go#{depth}"

    skip =
      advance_call(worker, rest_param, acc_param, var_args(binds))

    match_form(expr, [
      clause_form("true", [continue]),
      clause_form("false", [skip])
    ])
  end

  defp inner_suffix(
         [{:let, fields} | rest],
         spec,
         prefix,
         depth,
         binds,
         scope_binds,
         rest_param,
         acc_param
       ) do
    continue =
      inner_suffix(
        rest,
        spec,
        prefix,
        depth,
        binds,
        scope_binds ++ (fields[:binds] || []),
        rest_param,
        acc_param
      )

    match_form(fields[:expr], [clause_form(fields[:pattern], [continue])])
  end

  defp inner_suffix(
         [next_generator | _],
         spec,
         prefix,
         depth,
         binds,
         scope_binds,
         rest_param,
         acc_param
       ) do
    next_depth = depth + 1
    next_fields = elem(next_generator, 1)
    next_worker = "#{prefix}_go#{next_depth}"
    worker = "#{prefix}_go#{depth}"

    inner_chain =
      apply_chain(
        var_form(next_worker),
        [next_fields[:source], var_form(acc_param) | var_args(scope_binds)]
      )

    apply_chain(
      var_form(worker),
      [var_form(rest_param), inner_chain | var_args(binds)]
    )
  end

  defp advance_call(worker, rest_param, acc_param, inner_bind_args) do
    apply_chain(var_form(worker), [var_form(rest_param), var_form(acc_param) | inner_bind_args])
  end

  defp apply_chain(callee, arguments) do
    Enum.reduce(arguments, callee, fn argument, applied ->
      call_form(applied, argument)
    end)
  end

  defp var_args(binds), do: Enum.map(binds, fn {name, _type} -> var_form(name) end)
  defp var_form(name), do: "(var #{name})"
  defp call_form(callee, argument), do: "(call #{callee} #{argument})"
  defp match_form(scrutinee, clauses), do: "(match #{scrutinee} #{Enum.join(clauses, " ")})"
  defp clause_form(pattern, body), do: "(case #{pattern} #{Enum.join(body, " ")})"
  defp fn_form(parameter, type, body), do: "(fn (#{parameter} #{type}) #{body})"
  defp construct_cons(head, tail), do: "(construct Cons #{head} #{tail})"

  defp nil_case(_spec, prefix, 1, acc_param, _inner_bind_args) do
    apply_chain(var_form("#{prefix}_reverse"), [
      var_form(acc_param),
      "(construct Nil)"
    ])
  end

  defp nil_case(_spec, _prefix, _depth, acc_param, _inner_bind_args) do
    var_form(acc_param)
  end

  defp fn_type(parameter, parameters, result) do
    Enum.reduce(Enum.reverse(parameters), result, fn p, acc -> "(Fn #{p} (effects) #{acc})" end)
    |> then(&"(Fn #{parameter} (effects) #{&1})")
  end

  defp reverse_definition(spec, prefix, _uses) do
    element = spec.result_element_type

    """
    (fn (c047_rev_source (List #{element}))
      (fn (c047_rev_acc (List #{element}))
        (match (var c047_rev_source)
          (case (constructor Nil) (var c047_rev_acc))
          (case (constructor Cons (bind c047_rev_head) (bind c047_rev_rest))
            (call (call (var #{prefix}_reverse) (var c047_rev_rest))
              (construct Cons (var c047_rev_head) (var c047_rev_acc)))))))
    """
    |> String.trim_trailing()
  end

  defp reverse_signature(spec) do
    element = "(List #{spec.result_element_type})"
    "(Fn #{element} (effects) (Fn #{element} (effects) #{element}))"
  end

  defp marker_advisories(spec) do
    Enum.flat_map(Enum.with_index(spec.qualifiers), fn
      {{:case_generator, fields}, index} ->
        probe_qualifiers =
          List.replace_at(spec.qualifiers, index, {:generator, fields})

        probe = %{spec | qualifiers: probe_qualifiers}

        case check_rebinding(probe_qualifiers) do
          :ok ->
            {source, _} = render_probe(probe)

            case Catena.check_kernel(source) do
              {:ok, _} ->
                [
                  %Diagnostic{
                    id: "LCP003",
                    message:
                      "this filtering marker is unnecessary: the pattern already accepts every element",
                    severity: :warning
                  }
                ]

              _ ->
                []
            end

          _ ->
            []
        end

      _ ->
        []
    end)
  end

  defp render_probe(spec) do
    {source, _} = render(spec, probe: true)
    {source, []}
  end

  defp unused_advisories(spec) do
    all_binders =
      spec.qualifiers
      |> Enum.flat_map(&qualifier_binders/1)

    fragments =
      [spec.yield] ++
        Enum.map(spec.qualifiers, fn
          {:filter, expr: expr} -> expr
          {:let, fields} -> fields[:expr]
          {kind, fields} when kind in [:generator, :case_generator] -> fields[:source]
        end)

    Enum.flat_map(all_binders, fn name ->
      used =
        Enum.any?(fragments, fn fragment ->
          fragment =~ "(var #{name})"
        end)

      if used do
        []
      else
        [
          %Diagnostic{
            id: "BS001",
            message: "binding #{name} is unused in the comprehension",
            severity: :warning
          }
        ]
      end
    end)
  end
end
