defmodule Catena.Kernel.Parser do
  @moduledoc "Decoder for the exact Catena 0.1.8 semantic-module grammar."

  alias Catena.{Diagnostic, LanguageSelection, LanguageVersion}
  alias Catena.Kernel.{Node, SExpression}

  @module_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @type_name @module_name
  @value_name ~r/^[a-z][A-Za-z0-9_]*$/
  @field_name @value_name
  @qualified_process ~r/^(?:[A-Z][A-Za-z0-9_]*\.)*[A-Z][A-Za-z0-9_]*$/
  @integer ~r/^(?:0|-[1-9][0-9]*|[1-9][0-9]*)$/
  @unary ~w(not negate)
  @binary ~w(and or equal not_equal less less_equal greater greater_equal add subtract multiply)

  @spec parse(binary(), keyword()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def parse(source, options \\ []) do
    with {:ok, form} <- SExpression.parse(source, options) do
      try do
        module = decode_module!(form, options)
        validate_module!(module)
        {:ok, module}
      catch
        {:kernel_diagnostic, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      end
    end
  end

  defp decode_module!(%Node{kind: :list, value: forms, span: span}, options) do
    case forms do
      [head, name, edition, revision, origin | declarations] ->
        atom!(head, "module")
        module_name = name!(name, @module_name, "module name")
        exact_pair!(edition, "edition", "0.1")
        exact_pair!(revision, "revision", "0.1.8")
        origin_node = origin
        origin = string_pair!(origin_node, "origin")

        if origin == "" do
          fail!("SYN002", "kernel origin must not be empty", origin_node.span)
        end

        selection = selection!(options, span)

        decoded = Enum.map(declarations, &declaration!/1)

        %{
          format: :kernel,
          version: "0.1.8",
          frontend_format: "0.1.8",
          frontend_version: "0.1.8",
          edition: selection.edition,
          language_revision: selection.language_revision,
          previews: selection.previews,
          required_previews: [],
          origin: origin,
          module: module_name,
          source: Keyword.get(options, :source, "<catena-kernel>"),
          span: span,
          exports: exports(decoded),
          imports: select_declarations(decoded, :import),
          data: select_declarations(decoded, :data),
          traits: select_declarations(decoded, :trait),
          instances: select_declarations(decoded, :instance),
          effects: select_declarations(decoded, :effect),
          handlers: select_declarations(decoded, :handler),
          definitions: select_declarations(decoded, :definition),
          processes: select_declarations(decoded, :process)
        }

      _ ->
        fail!("SYN002", "kernel input must use the exact module header", span)
    end
  end

  defp decode_module!(node, _options),
    do: fail!("SYN002", "kernel input must contain one module form", node.span)

  defp selection!(options, span) do
    requested =
      Keyword.get(options, :language_selection, %LanguageSelection{
        edition: "0.1",
        language_revision: "0.1.8",
        previews: []
      })

    case LanguageVersion.resolve_selection(requested) do
      {:ok,
       %LanguageSelection{edition: "0.1", language_revision: "0.1.8", previews: []} = selection} ->
        selection

      {:ok, selection} ->
        fail!(
          "EDN001",
          "kernel header requires edition 0.1, revision 0.1.8, and no previews",
          span,
          %{selected: LanguageSelection.to_map(selection)}
        )

      {:error, diagnostic} ->
        throw({:kernel_diagnostic, %{diagnostic | span: diagnostic.span || span}})
    end
  end

  defp declaration!(%Node{kind: :list, value: [%Node{kind: :atom, value: head} | _]} = node) do
    case head do
      "export" -> export!(node)
      "import" -> import!(node)
      "data" -> data!(node)
      "trait" -> trait!(node)
      "instance" -> instance!(node)
      "effect" -> effect!(node)
      "handler" -> handler!(node)
      "def" -> definition!(node)
      "process" -> process!(node)
      _ -> fail!("SYN002", "unknown kernel declaration #{inspect(head)}", node.span)
    end
  end

  defp declaration!(node), do: fail!("SYN002", "malformed kernel declaration", node.span)

  defp export!(%Node{value: [_, namespace, name], span: span}) do
    namespace_node = namespace
    namespace = atom_value!(namespace_node)

    regex =
      case namespace do
        "value" ->
          @value_name

        "type" ->
          @type_name

        "process" ->
          @module_name

        _ ->
          fail!(
            "SYN002",
            "export namespace must be value, type, or process",
            namespace_node.span
          )
      end

    %{kind: :export, namespace: namespace, name: name!(name, regex, "export name"), span: span}
  end

  defp export!(node), do: fail!("SYN002", "export requires a namespace and name", node.span)

  defp import!(%Node{value: [_, module, digest], span: span}) do
    digest_node = digest
    digest = string_value!(digest_node)

    if Regex.match?(~r/^[0-9a-f]{64}$/, digest) do
      %{
        kind: :import,
        module: name!(module, @module_name, "imported module"),
        digest: digest,
        span: span
      }
    else
      fail!(
        "SYN002",
        "import digest must contain 64 lowercase hexadecimal digits",
        digest_node.span
      )
    end
  end

  defp import!(node),
    do: fail!("SYN002", "import requires a module and interface digest", node.span)

  defp data!(%Node{value: [_, name, params | constructors], span: span})
       when constructors != [] do
    %{
      kind: :data,
      name: name!(name, @type_name, "datatype name"),
      parameters: type_variable_names!(params),
      constructors: Enum.map(constructors, &data_constructor!/1),
      span: span
    }
  end

  defp data!(node),
    do: fail!("SYN002", "data requires parameters and at least one constructor", node.span)

  defp type_variable_names!(%Node{kind: :list, value: [head | variables]}) do
    atom!(head, "params")
    Enum.map(variables, &name!(&1, @value_name, "type parameter"))
  end

  defp type_variable_names!(node),
    do: fail!("SYN002", "data params must contain type-variable names", node.span)

  defp data_constructor!(%Node{kind: :list, value: [head, name, fields], span: span}) do
    atom!(head, "constructor")

    %{
      name: name!(name, @module_name, "constructor name"),
      fields: constructor_fields!(fields),
      span: span
    }
  end

  defp data_constructor!(node),
    do: fail!("SYN002", "constructor requires a name and fields form", node.span)

  defp constructor_fields!(%Node{kind: :list, value: [head | fields]}) do
    atom!(head, "fields")
    Enum.map(fields, &type!/1)
  end

  defp constructor_fields!(node),
    do: fail!("SYN002", "constructor fields must be a fields form", node.span)

  defp trait!(%Node{value: [_, name, parameter | methods], span: span}) when methods != [] do
    parameter = tagged_name!(parameter, "parameter", @value_name, "trait parameter")

    %{
      kind: :trait,
      name: name!(name, @module_name, "trait name"),
      parameter: parameter,
      methods: Enum.map(methods, &trait_method!/1),
      span: span
    }
  end

  defp trait!(node),
    do: fail!("SYN002", "trait requires a parameter and at least one method", node.span)

  defp trait_method!(%Node{kind: :list, value: [head, name, type], span: span}) do
    atom!(head, "method")
    %{name: name!(name, @value_name, "trait method"), type: type!(type), span: span}
  end

  defp trait_method!(node),
    do: fail!("SYN002", "trait method requires a name and type", node.span)

  defp instance!(%Node{value: [_, trait, head | methods], span: span}) when methods != [] do
    %{
      kind: :instance,
      trait: name!(trait, @module_name, "instance trait"),
      head: type!(head),
      methods: Enum.map(methods, &instance_method!/1),
      span: span
    }
  end

  defp instance!(node),
    do: fail!("SYN002", "instance requires a trait, head type, and methods", node.span)

  defp instance_method!(%Node{kind: :list, value: [head, name, definition], span: span}) do
    atom!(head, "method")

    %{
      name: name!(name, @value_name, "instance method"),
      definition: name!(definition, @value_name, "method implementation"),
      span: span
    }
  end

  defp instance_method!(node),
    do: fail!("SYN002", "instance method requires a method and definition name", node.span)

  defp effect!(%Node{value: [_, name | operations], span: span}) when operations != [] do
    %{
      kind: :effect,
      name: name!(name, @module_name, "effect name"),
      operations: Enum.map(operations, &effect_operation!/1),
      span: span
    }
  end

  defp effect!(node),
    do: fail!("SYN002", "effect requires a name and at least one operation", node.span)

  defp effect_operation!(%Node{kind: :list, value: [head, name, params, result], span: span}) do
    atom!(head, "operation")

    %{
      name: name!(name, @value_name, "effect operation"),
      parameters: type_parameters!(params),
      result: type!(result),
      span: span
    }
  end

  defp effect_operation!(node),
    do: fail!("SYN002", "effect operation requires name, params, and result", node.span)

  defp type_parameters!(%Node{kind: :list, value: [head | parameters]}) do
    atom!(head, "params")
    Enum.map(parameters, &type!/1)
  end

  defp type_parameters!(node),
    do: fail!("SYN002", "operation params must be a params form", node.span)

  defp handler!(%Node{value: [_, name, effect, input, output, return | operations], span: span}) do
    %{
      kind: :handler,
      name: name!(name, @module_name, "handler name"),
      effect: tagged_name!(effect, "effect", @module_name, "handled effect"),
      input: tagged_type!(input, "input"),
      output: tagged_type!(output, "output"),
      return: handler_return!(return),
      operations: Enum.map(operations, &handler_operation!/1),
      span: span
    }
  end

  defp handler!(node),
    do:
      fail!(
        "SYN002",
        "handler requires effect, input, output, return, and operation forms",
        node.span
      )

  defp handler_return!(%Node{kind: :list, value: [head, parameter, body], span: span}) do
    atom!(head, "return")

    %{
      parameter: name!(parameter, @value_name, "handler return parameter"),
      body: expression!(body),
      span: span
    }
  end

  defp handler_return!(node),
    do: fail!("SYN002", "handler return requires a parameter and body", node.span)

  defp handler_operation!(%Node{
         kind: :list,
         value: [head, operation, params, resume, body],
         span: span
       }) do
    atom!(head, "operation")

    %{
      operation: name!(operation, @value_name, "handled operation"),
      parameters: parameters!(params),
      resumption: tagged_name!(resume, "resume", @value_name, "resumption name"),
      body: expression!(body),
      span: span
    }
  end

  defp handler_operation!(node),
    do: fail!("SYN002", "handler operation has the wrong shape", node.span)

  defp definition!(%Node{value: [_, name, signature, expression], span: span}) do
    {type, uses} = signature!(signature)

    %{
      kind: :definition,
      name: name!(name, @value_name, "definition name"),
      signature: type,
      variables: type |> Catena.Kernel.Type.variables() |> MapSet.to_list() |> Enum.sort(),
      uses: uses,
      expression: expression!(expression),
      span: span
    }
  end

  defp definition!(node),
    do: fail!("SYN002", "definition requires a name, signature, and expression", node.span)

  defp signature!(%Node{kind: :list, value: [head, type, uses]}) do
    atom!(head, "signature")
    {type!(type), uses!(uses)}
  end

  defp signature!(node),
    do: fail!("SYN002", "signature must contain a type and uses row", node.span)

  defp uses!(%Node{kind: :list, value: [head | entries]}) do
    atom!(head, "uses")
    Enum.map(entries, &effect_entry!/1)
  end

  defp uses!(node), do: fail!("SYN002", "uses must be an effect-row form", node.span)

  defp effect_entry!(%Node{kind: :atom, value: "Process"}), do: :process

  defp effect_entry!(%Node{kind: :atom, value: name, span: span}) do
    if Regex.match?(@module_name, name),
      do: {:effect, name},
      else: fail!("SYN002", "invalid effect entry", span)
  end

  defp effect_entry!(node), do: fail!("SYN002", "invalid effect entry", node.span)

  defp process!(%Node{value: [_, name, mailbox, params, body], span: span}) do
    %{
      kind: :process,
      name: name!(name, @module_name, "process entry name"),
      mailbox: tagged_type!(mailbox, "mailbox"),
      parameters: parameters!(params),
      body: expression!(body),
      span: span
    }
  end

  defp process!(node),
    do: fail!("SYN002", "process requires mailbox, params, and body forms", node.span)

  defp tagged_type!(%Node{kind: :list, value: [head, type]}, expected) do
    atom!(head, expected)
    type!(type)
  end

  defp tagged_type!(node, expected),
    do: fail!("SYN002", "#{expected} requires exactly one type", node.span)

  defp parameters!(%Node{kind: :list, value: [head | bindings]}) do
    atom!(head, "params")
    Enum.map(bindings, &binding!/1)
  end

  defp parameters!(node), do: fail!("SYN002", "params must contain typed bindings", node.span)

  defp binding!(%Node{kind: :list, value: [name, type], span: span}) do
    %{name: name!(name, @value_name, "binding name"), type: type!(type), span: span}
  end

  defp binding!(node), do: fail!("SYN002", "binding must contain a name and type", node.span)

  defp type!(%Node{kind: :atom, value: value, span: span}) do
    case value do
      "Int" -> :integer
      "Bool" -> :boolean
      "Unit" -> :unit
      _ -> atomic_type!(value, span)
    end
  end

  defp type!(%Node{kind: :list, value: [head | arguments], span: span}) do
    case atom_value!(head) do
      "Tuple" -> {:tuple, Enum.map(arguments, &type!/1)}
      "Process" -> one_type_argument!(:process, arguments, span)
      "Record" -> one_row_argument!(:record, arguments, span)
      "Variant" -> one_row_argument!(:variant, arguments, span)
      "Fn" -> function_type!(arguments, span)
      name -> nominal_type!(name, arguments, span)
    end
  end

  defp type!(node), do: fail!("SYN002", "malformed kernel type", node.span)

  defp atomic_type!(value, span) do
    cond do
      Regex.match?(@value_name, value) -> {:variable, value}
      Regex.match?(@type_name, value) -> {:nominal, value, []}
      true -> fail!("SYN002", "invalid type name #{inspect(value)}", span)
    end
  end

  defp one_type_argument!(tag, [argument], _span), do: {tag, type!(argument)}

  defp one_type_argument!(_tag, _arguments, span),
    do: fail!("SYN002", "type constructor has the wrong arity", span)

  defp one_row_argument!(tag, [row], _span), do: {tag, row!(row)}

  defp one_row_argument!(_tag, _arguments, span),
    do: fail!("SYN002", "row type has the wrong arity", span)

  defp function_type!([parameter, effects, result], _span),
    do: {:function, type!(parameter), effect_row!(effects), type!(result)}

  defp function_type!(_arguments, span),
    do: fail!("SYN002", "Fn requires parameter, effects, and result", span)

  defp effect_row!(%Node{kind: :list, value: [head | entries]}) do
    atom!(head, "effects")
    Enum.map(entries, &effect_entry!/1)
  end

  defp effect_row!(node),
    do: fail!("SYN002", "function effects must use an effects form", node.span)

  defp row!(%Node{kind: :list, value: [head | entries], span: span}) do
    atom!(head, "row")

    {fields, tail} =
      Enum.reduce(entries, {[], nil}, fn entry, {fields, tail} ->
        case entry do
          %Node{kind: :list, value: [entry_head, label, field_type]} ->
            atom!(entry_head, "field")

            if tail do
              fail!("SYN002", "row tail must be the final row entry", entry.span)
            end

            {[{name!(label, @field_name, "row label"), type!(field_type)} | fields], tail}

          %Node{kind: :list, value: [entry_head, variable]} ->
            atom!(entry_head, "tail")

            if tail do
              fail!("T005", "row contains more than one tail", entry.span)
            end

            {fields, name!(variable, @value_name, "row-tail variable")}

          _ ->
            fail!("SYN002", "malformed row entry", entry.span)
        end
      end)

    labels = Enum.map(fields, &elem(&1, 0))

    if length(labels) != length(Enum.uniq(labels)) do
      fail!("T005", "unique value rows cannot contain duplicate labels", span)
    end

    %{fields: Map.new(fields), tail: tail}
  end

  defp row!(node), do: fail!("SYN002", "row type requires a row form", node.span)

  defp nominal_type!(name, arguments, span) do
    if Regex.match?(@type_name, name),
      do: {:nominal, name, Enum.map(arguments, &type!/1)},
      else: fail!("SYN002", "invalid nominal type constructor", span)
  end

  defp expression!(%Node{kind: :atom, value: "true", span: span}),
    do: %{tag: :boolean, value: true, span: span}

  defp expression!(%Node{kind: :atom, value: "false", span: span}),
    do: %{tag: :boolean, value: false, span: span}

  defp expression!(%Node{kind: :atom, value: value, span: span}) do
    if Regex.match?(@integer, value),
      do: %{tag: :integer, value: String.to_integer(value), span: span},
      else: fail!("SYN002", "bare expression atom #{inspect(value)} is not valid", span)
  end

  defp expression!(%Node{kind: :list, value: [head | arguments], span: span}) do
    case atom_value!(head) do
      "unit" -> nullary!(:unit, arguments, span)
      "var" -> variable!(arguments, span)
      "fn" -> function!(arguments, span)
      "call" -> call!(arguments, span)
      "let" -> let!(:let, arguments, span)
      "sequence" -> sequence!(arguments, span)
      "tuple" -> %{tag: :tuple, elements: Enum.map(arguments, &expression!/1), span: span}
      "record" -> record!(arguments, span)
      "select" -> field_operation!(:select, arguments, span)
      "update" -> field_operation!(:update, arguments, span)
      "extend" -> field_operation!(:extend, arguments, span)
      "restrict" -> field_operation!(:restrict, arguments, span)
      "inject" -> inject!(arguments, span)
      "construct" -> construct!(arguments, span)
      "match" -> match!(:match, arguments, span)
      "annotate" -> annotate!(arguments, span)
      "trait-call" -> trait_call!(arguments, span)
      "request" -> request!(arguments, span)
      "handle" -> handle!(arguments, span)
      "resume" -> resume!(arguments, span)
      "spawn" -> spawn!(arguments, span)
      "self" -> nullary!(:self, arguments, span)
      "send" -> binary_form!(:send, arguments, span)
      "receive" -> receive!(arguments, span)
      "trap" -> unary_form!(:trap, arguments, span)
      operator when operator in @unary -> unary_operator!(operator, arguments, span)
      operator when operator in @binary -> binary_operator!(operator, arguments, span)
      other -> fail!("SYN002", "unknown kernel expression form #{inspect(other)}", span)
    end
  end

  defp expression!(node), do: fail!("SYN002", "malformed kernel expression", node.span)

  defp nullary!(tag, [], span), do: %{tag: tag, span: span}
  defp nullary!(_tag, _arguments, span), do: fail!("SYN002", "form takes no arguments", span)

  defp variable!([name], span),
    do: %{tag: :variable, name: name!(name, @value_name, "value name"), span: span}

  defp variable!(_arguments, span), do: fail!("SYN002", "var requires one value name", span)

  defp function!([binding, body], span) do
    binding = binding!(binding)

    %{
      tag: :function,
      parameter: binding.name,
      parameter_type: binding.type,
      body: expression!(body),
      span: span
    }
  end

  defp function!(_arguments, span),
    do: fail!("SYN002", "fn requires one typed binding and a body", span)

  defp call!([callee | arguments], span) when arguments != [],
    do: %{
      tag: :call,
      callee: expression!(callee),
      arguments: Enum.map(arguments, &expression!/1),
      span: span
    }

  defp call!(_arguments, span),
    do: fail!("SYN002", "call requires a callee and at least one argument", span)

  defp let!(tag, [name, value, body], span) do
    %{
      tag: tag,
      name: name!(name, @value_name, "let binding"),
      value: expression!(value),
      body: expression!(body),
      span: span
    }
  end

  defp let!(_tag, _arguments, span),
    do: fail!("SYN002", "let requires a name, value, and body", span)

  defp sequence!([first, second], span),
    do: %{tag: :sequence, first: expression!(first), second: expression!(second), span: span}

  defp sequence!(_arguments, span), do: fail!("SYN002", "sequence requires two expressions", span)

  defp record!(entries, span) do
    fields = Enum.map(entries, &record_field!/1)
    labels = Enum.map(fields, & &1.label)

    if length(labels) != length(Enum.uniq(labels)) do
      fail!("T005", "record contains a duplicate field", span)
    end

    %{tag: :record, fields: fields, span: span}
  end

  defp record_field!(%Node{kind: :list, value: [head, label, expression], span: span}) do
    atom!(head, "field")

    %{
      label: name!(label, @field_name, "record field"),
      expression: expression!(expression),
      span: span
    }
  end

  defp record_field!(node),
    do: fail!("SYN002", "record field requires a label and expression", node.span)

  defp field_operation!(tag, [record, label], span) when tag in [:select, :restrict] do
    %{
      tag: tag,
      record: expression!(record),
      label: name!(label, @field_name, "record field"),
      span: span
    }
  end

  defp field_operation!(tag, [record, label, value], span) when tag in [:update, :extend] do
    %{
      tag: tag,
      record: expression!(record),
      label: name!(label, @field_name, "record field"),
      value: expression!(value),
      span: span
    }
  end

  defp field_operation!(_tag, _arguments, span),
    do: fail!("SYN002", "record operation has the wrong arity", span)

  defp inject!([label, payload], span) do
    %{
      tag: :inject,
      label: name!(label, @field_name, "variant label"),
      payload: expression!(payload),
      span: span
    }
  end

  defp inject!(_arguments, span), do: fail!("SYN002", "inject requires a label and payload", span)

  defp construct!([constructor | arguments], span) do
    %{
      tag: :construct,
      constructor: name!(constructor, @module_name, "constructor name"),
      arguments: Enum.map(arguments, &expression!/1),
      span: span
    }
  end

  defp construct!(_arguments, span),
    do: fail!("SYN002", "construct requires a constructor name", span)

  defp match!(tag, [scrutinee | clauses], span) when clauses != [] do
    %{
      tag: tag,
      scrutinee: expression!(scrutinee),
      clauses: Enum.map(clauses, &clause!/1),
      span: span
    }
  end

  defp match!(_tag, _arguments, span),
    do: fail!("SYN002", "match requires a scrutinee and clauses", span)

  defp annotate!([expression, type], span),
    do: %{
      tag: :annotate,
      expression: expression!(expression),
      annotation: type!(type),
      span: span
    }

  defp annotate!(_arguments, span),
    do: fail!("SYN002", "annotate requires an expression and type", span)

  defp trait_call!([trait, method | arguments], span) when arguments != [] do
    %{
      tag: :trait_call,
      trait: name!(trait, @module_name, "trait name"),
      method: name!(method, @value_name, "trait method"),
      arguments: Enum.map(arguments, &expression!/1),
      span: span
    }
  end

  defp trait_call!(_arguments, span),
    do: fail!("SYN002", "trait-call requires trait, method, and arguments", span)

  defp request!([effect, operation | arguments], span) do
    %{
      tag: :request,
      effect: name!(effect, @module_name, "effect name"),
      operation: name!(operation, @value_name, "effect operation"),
      arguments: Enum.map(arguments, &expression!/1),
      span: span
    }
  end

  defp request!(_arguments, span),
    do: fail!("SYN002", "request requires an effect and operation", span)

  defp handle!([handler, expression], span) do
    %{
      tag: :handle,
      handler: name!(handler, @module_name, "handler name"),
      expression: expression!(expression),
      span: span
    }
  end

  defp handle!(_arguments, span),
    do: fail!("SYN002", "handle requires a handler and expression", span)

  defp resume!([resumption, expression], span) do
    %{
      tag: :resume,
      resumption: name!(resumption, @value_name, "resumption name"),
      expression: expression!(expression),
      span: span
    }
  end

  defp resume!(_arguments, span),
    do: fail!("SYN002", "resume requires a resumption and value", span)

  defp spawn!([entry | arguments], span) do
    %{
      tag: :spawn,
      entry: name!(entry, @qualified_process, "process entry"),
      arguments: Enum.map(arguments, &expression!/1),
      span: span
    }
  end

  defp spawn!(_arguments, span), do: fail!("SYN002", "spawn requires a process entry", span)

  defp receive!(clauses, span) when clauses != [],
    do: %{tag: :receive, clauses: Enum.map(clauses, &clause!/1), span: span}

  defp receive!(_clauses, span), do: fail!("SYN002", "receive requires at least one clause", span)

  defp unary_form!(tag, [expression], span),
    do: %{tag: tag, expression: expression!(expression), span: span}

  defp unary_form!(_tag, _arguments, span),
    do: fail!("SYN002", "unary form has the wrong arity", span)

  defp binary_form!(tag, [left, right], span),
    do: %{tag: tag, left: expression!(left), right: expression!(right), span: span}

  defp binary_form!(_tag, _arguments, span),
    do: fail!("SYN002", "binary form has the wrong arity", span)

  defp unary_operator!(operator, [operand], span),
    do: %{
      tag: :unary,
      operator: operator_atom(operator),
      operand: expression!(operand),
      span: span
    }

  defp unary_operator!(_operator, _arguments, span),
    do: fail!("SYN002", "unary operator has the wrong arity", span)

  defp binary_operator!(operator, [left, right], span) do
    %{
      tag: :binary,
      operator: operator_atom(operator),
      left: expression!(left),
      right: expression!(right),
      span: span
    }
  end

  defp binary_operator!(_operator, _arguments, span),
    do: fail!("SYN002", "binary operator has the wrong arity", span)

  defp clause!(%Node{kind: :list, value: [head, pattern, body], span: span}) do
    atom!(head, "case")
    %{pattern: pattern!(pattern), guard: nil, body: expression!(body), span: span}
  end

  defp clause!(%Node{kind: :list, value: [head, pattern, guard, body], span: span}) do
    atom!(head, "case")

    guard =
      case guard do
        %Node{kind: :list, value: [when_head, expression]} ->
          atom!(when_head, "when")
          expression!(expression)

        _ ->
          fail!("SYN002", "clause guard must use a when form", guard.span)
      end

    %{pattern: pattern!(pattern), guard: guard, body: expression!(body), span: span}
  end

  defp clause!(node), do: fail!("SYN002", "case clause has the wrong shape", node.span)

  defp pattern!(%Node{kind: :atom, value: "_", span: span}), do: %{tag: :wildcard, span: span}

  defp pattern!(%Node{kind: :atom, value: "true", span: span}),
    do: %{tag: :boolean, value: true, span: span}

  defp pattern!(%Node{kind: :atom, value: "false", span: span}),
    do: %{tag: :boolean, value: false, span: span}

  defp pattern!(%Node{kind: :atom, value: value, span: span}) do
    if Regex.match?(@integer, value),
      do: %{tag: :integer, value: String.to_integer(value), span: span},
      else: fail!("SYN002", "invalid bare pattern", span)
  end

  defp pattern!(%Node{kind: :list, value: [head | arguments], span: span}) do
    case atom_value!(head) do
      "wildcard" -> nullary!(:wildcard, arguments, span)
      "bind" -> bind_pattern!(arguments, span)
      "tuple" -> %{tag: :tuple, elements: Enum.map(arguments, &pattern!/1), span: span}
      "variant" -> variant_pattern!(arguments, span)
      "constructor" -> constructor_pattern!(arguments, span)
      "as" -> as_pattern!(arguments, span)
      "or" -> or_pattern!(arguments, span)
      other -> fail!("SYN002", "unknown kernel pattern form #{inspect(other)}", span)
    end
  end

  defp pattern!(node), do: fail!("SYN002", "malformed kernel pattern", node.span)

  defp bind_pattern!([name], span),
    do: %{tag: :bind, name: name!(name, @value_name, "pattern binding"), span: span}

  defp bind_pattern!(_arguments, span),
    do: fail!("SYN002", "bind pattern requires one name", span)

  defp variant_pattern!([label, payload], span) do
    %{
      tag: :variant,
      label: name!(label, @field_name, "variant label"),
      pattern: pattern!(payload),
      span: span
    }
  end

  defp variant_pattern!(_arguments, span),
    do: fail!("SYN002", "variant pattern requires a label and payload pattern", span)

  defp constructor_pattern!([constructor | patterns], span) do
    %{
      tag: :constructor,
      constructor: name!(constructor, @module_name, "constructor name"),
      patterns: Enum.map(patterns, &pattern!/1),
      span: span
    }
  end

  defp constructor_pattern!(_arguments, span),
    do: fail!("SYN002", "constructor pattern requires a constructor name", span)

  defp as_pattern!([pattern, name], span) do
    %{
      tag: :as,
      pattern: pattern!(pattern),
      name: name!(name, @value_name, "as-pattern binding"),
      span: span
    }
  end

  defp as_pattern!(_arguments, span), do: fail!("SYN002", "as pattern has the wrong arity", span)

  defp or_pattern!(alternatives, span) when length(alternatives) >= 2,
    do: %{tag: :or, alternatives: Enum.map(alternatives, &pattern!/1), span: span}

  defp or_pattern!(_alternatives, span),
    do: fail!("SYN002", "or pattern requires alternatives", span)

  defp validate_module!(module) do
    ensure_unique_values!(module.exports.values, "value export", "T001", module.span)
    ensure_unique_values!(module.exports.types, "type export", "A002", module.span)
    ensure_unique_values!(module.exports.processes, "process export", "PRC001", module.span)
    ensure_unique!(module.definitions, & &1.name, "definition", "T001")
    ensure_unique!(module.processes, & &1.name, "process entry", "PRC001")
    ensure_unique!(module.imports, & &1.module, "import", "SYN002")
    ensure_unique!(module.data, & &1.name, "datatype", "A002")
    ensure_unique!(module.traits, & &1.name, "trait", "TRT001")
    ensure_unique!(module.effects, & &1.name, "effect", "EFX001")
    ensure_unique!(module.handlers, & &1.name, "handler", "EFX006")

    Enum.each(module.data, fn data ->
      ensure_unique_values!(data.parameters, "datatype parameter", "A002", data.span)
      ensure_unique!(data.constructors, & &1.name, "constructor", "A002")
    end)

    module.data
    |> Enum.flat_map(& &1.constructors)
    |> ensure_unique!(& &1.name, "constructor", "A002")

    Enum.each(module.processes, fn process ->
      ensure_unique!(process.parameters, & &1.name, "process parameter", "PRC001")
    end)

    Enum.each(module.exports.values, fn export ->
      unless Enum.any?(module.definitions, &(&1.name == export)) do
        fail!("T001", "exported value #{export} has no definition", module.span)
      end
    end)

    Enum.each(module.exports.processes, fn export ->
      unless Enum.any?(module.processes, &(&1.name == export)) do
        fail!("PRC001", "exported process #{export} has no entry declaration", module.span)
      end
    end)

    Enum.each(module.exports.types, fn export ->
      unless Enum.any?(module.data, &(&1.name == export)) do
        fail!("A002", "exported type #{export} has no data declaration", module.span)
      end
    end)

    module
  end

  defp ensure_unique!(values, key, label, id) do
    keys = Enum.map(values, key)

    if length(keys) != length(Enum.uniq(keys)) do
      fail!(id, "duplicate #{label} name", hd(values).span)
    end
  end

  defp ensure_unique_values!(values, label, id, span) do
    if length(values) != length(Enum.uniq(values)) do
      fail!(id, "duplicate #{label} name", span)
    end
  end

  defp exports(declarations) do
    exports = select_declarations(declarations, :export)

    %{
      values: exports |> Enum.filter(&(&1.namespace == "value")) |> Enum.map(& &1.name),
      types: exports |> Enum.filter(&(&1.namespace == "type")) |> Enum.map(& &1.name),
      processes: exports |> Enum.filter(&(&1.namespace == "process")) |> Enum.map(& &1.name)
    }
  end

  defp select_declarations(declarations, kind), do: Enum.filter(declarations, &(&1.kind == kind))

  defp tagged_name!(%Node{kind: :list, value: [head, name]}, expected, regex, role) do
    atom!(head, expected)
    name!(name, regex, role)
  end

  defp tagged_name!(node, expected, _regex, _role),
    do: fail!("SYN002", "#{expected} form has the wrong shape", node.span)

  defp exact_pair!(%Node{kind: :list, value: [head, value]}, expected_head, expected_value) do
    atom!(head, expected_head)
    atom!(value, expected_value)
  end

  defp exact_pair!(node, expected_head, _expected_value),
    do: fail!("SYN002", "#{expected_head} header has the wrong shape", node.span)

  defp string_pair!(%Node{kind: :list, value: [head, value]}, expected_head) do
    atom!(head, expected_head)
    string_value!(value)
  end

  defp string_pair!(node, expected_head),
    do: fail!("SYN002", "#{expected_head} header has the wrong shape", node.span)

  defp atom!(%Node{kind: :atom, value: expected}, expected), do: :ok

  defp atom!(node, expected),
    do: fail!("SYN002", "expected #{inspect(expected)}", node.span)

  defp atom_value!(%Node{kind: :atom, value: value}), do: value
  defp atom_value!(node), do: fail!("SYN002", "expected an unquoted token", node.span)

  defp string_value!(%Node{kind: :string, value: value}), do: value
  defp string_value!(node), do: fail!("SYN002", "expected a metadata string", node.span)

  defp name!(%Node{kind: :atom, value: value, span: span}, regex, role) do
    if Regex.match?(regex, value), do: value, else: fail!("SYN002", "invalid #{role}", span)
  end

  defp name!(node, _regex, role),
    do: fail!("SYN002", "#{role} must be an unquoted name", node.span)

  defp fail!(id, message, span, details \\ %{}) do
    throw({:kernel_diagnostic, Diagnostic.new(id, message, span: span, details: details)})
  end

  defp operator_atom("not"), do: :not
  defp operator_atom("negate"), do: :negate
  defp operator_atom("and"), do: :and
  defp operator_atom("or"), do: :or
  defp operator_atom("equal"), do: :equal
  defp operator_atom("not_equal"), do: :not_equal
  defp operator_atom("less"), do: :less
  defp operator_atom("less_equal"), do: :less_equal
  defp operator_atom("greater"), do: :greater
  defp operator_atom("greater_equal"), do: :greater_equal
  defp operator_atom("add"), do: :add
  defp operator_atom("subtract"), do: :subtract
  defp operator_atom("multiply"), do: :multiply
end
