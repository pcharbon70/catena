defmodule Catena.AST.Decoder do
  @moduledoc "Strict decoder for the temporary, versioned Catena JSON AST."

  alias Catena.Diagnostic

  @versions ~w(0.1 0.2 0.3)
  @module_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @type_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @value_name ~r/^[a-z][a-zA-Z0-9_]*$/
  @expression_tags ~w(integer boolean variable function call let tuple annotate construct match unary binary)
  @pattern_tags ~w(wildcard bind integer boolean tuple constructor as or)

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(json) when is_binary(json) do
    with {:ok, value} <- JSON.decode(json),
         :ok <- require_map(value, "$"),
         {:ok, frontend_version} <- version(value),
         {:ok, module_name} <- name(value, "module", @module_name, "$"),
         {:ok, origin} <- origin(value, frontend_version),
         {:ok, exports} <- exports(value),
         {:ok, type_exports} <- type_exports(value, frontend_version),
         {:ok, type_groups} <- type_groups(value, frontend_version),
         {:ok, imports} <- imports(value, frontend_version),
         {:ok, definitions} <- definitions(value, frontend_version) do
      {:ok,
       %{
         version: if(frontend_version == "0.3", do: "0.3", else: "0.2"),
         frontend_version: frontend_version,
         origin: origin,
         module: module_name,
         exports: exports,
         type_exports: type_exports,
         type_groups: type_groups,
         imports: imports,
         definitions: definitions,
         source: Map.get(value, "source", "<catena-json>")
       }}
    else
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      {:error, reason} -> error("invalid JSON: #{inspect(reason)}", "$")
    end
  end

  defp version(%{"version" => version}) when version in @versions, do: {:ok, version}

  defp version(%{"version" => version}) do
    error("unsupported AST version #{inspect(version)}; expected 0.1, 0.2, or 0.3", "$.version")
  end

  defp version(_), do: error("missing AST version", "$.version")

  defp origin(_value, "0.1"), do: {:ok, "legacy://json-ast-0.1"}

  defp origin(value, version) when version in ~w(0.2 0.3) do
    case Map.get(value, "origin") do
      origin when is_binary(origin) and byte_size(origin) > 0 -> {:ok, origin}
      _ -> error("AST 0.2 requires a non-empty origin", "$.origin")
    end
  end

  defp exports(%{"exports" => exports}) when is_list(exports) do
    if Enum.all?(exports, &(is_binary(&1) and Regex.match?(@value_name, &1))) and
         length(exports) == length(Enum.uniq(exports)) do
      {:ok, exports}
    else
      error("exports must be unique value names", "$.exports")
    end
  end

  defp exports(_), do: error("exports must be a list", "$.exports")

  defp type_exports(_value, "0.1"), do: {:ok, []}

  defp type_exports(value, version) when version in ~w(0.2 0.3) do
    exports = Map.get(value, "type_exports", [])

    with true <- is_list(exports),
         {:ok, decoded} <- map_ok(exports, &type_export/2),
         true <- unique_by?(decoded, & &1.name) do
      {:ok, decoded}
    else
      false ->
        semantic_error("A002", "type exports must have unique type names", "$.type_exports")

      {:error, _} = result ->
        result
    end
  end

  defp type_export(value, index) do
    path = "$.type_exports[#{index}]"

    with :ok <- require_map(value, path),
         {:ok, name} <- name(value, "name", @type_name, path),
         visibility when visibility in ["transparent", "abstract"] <-
           Map.get(value, "visibility") do
      {:ok, %{name: name, visibility: String.to_existing_atom(visibility), path: path}}
    else
      nil ->
        error("type export requires visibility", path <> ".visibility")

      visibility when is_binary(visibility) ->
        error("visibility must be transparent or abstract", path <> ".visibility")

      {:error, _} = result ->
        result
    end
  end

  defp type_groups(_value, "0.1"), do: {:ok, []}

  defp type_groups(value, version) when version in ~w(0.2 0.3) do
    groups = Map.get(value, "type_groups", [])

    if is_list(groups) do
      map_ok(groups, &type_group/2)
    else
      error("type_groups must be a list", "$.type_groups")
    end
  end

  defp type_group(value, index) do
    path = "$.type_groups[#{index}]"
    declarations = if is_map(value), do: Map.get(value, "declarations"), else: nil

    with :ok <- require_map(value, path),
         true <- is_list(declarations) and declarations != [],
         {:ok, decoded} <- map_ok(declarations, &type_declaration(&1, &2, path)) do
      if unique_by?(decoded, & &1.name),
        do: {:ok, %{declarations: decoded, path: path}},
        else:
          semantic_error("A002", "type declaration names must be unique", path <> ".declarations")
    else
      false -> error("a type group requires declarations", path <> ".declarations")
      {:error, _} = result -> result
    end
  end

  defp type_declaration(value, index, group_path) do
    path = "#{group_path}.declarations[#{index}]"

    with :ok <- require_map(value, path),
         {:ok, name} <- name(value, "name", @type_name, path),
         {:ok, parameters} <-
           type_parameters(Map.get(value, "parameters", []), path <> ".parameters"),
         {:ok, constructors} <- constructors(Map.get(value, "constructors"), path),
         {:ok, derivations} <-
           string_list(Map.get(value, "derivations", []), path <> ".derivations") do
      {:ok,
       %{
         name: name,
         parameters: parameters,
         constructors: constructors,
         derivations: derivations,
         path: path
       }}
    end
  end

  defp type_parameters(parameters, path) when is_list(parameters) do
    with {:ok, decoded} <- map_ok(parameters, &type_parameter(&1, &2, path)),
         true <- unique_by?(decoded, & &1.name) do
      {:ok, decoded}
    else
      false -> semantic_error("A002", "type parameters must have unique names", path)
      {:error, _} = result -> result
    end
  end

  defp type_parameters(_, path), do: error("type parameters must be a list", path)

  defp type_parameter(value, index, base_path) do
    path = "#{base_path}[#{index}]"

    with :ok <- require_map(value, path),
         {:ok, name} <- name(value, "name", @value_name, path),
         kind when is_binary(kind) <- Map.get(value, "kind") do
      {:ok, %{name: name, kind: kind, path: path}}
    else
      nil -> error("type parameter requires a kind", path <> ".kind")
      {:error, _} = result -> result
    end
  end

  defp constructors(constructors, path) when is_list(constructors) do
    with {:ok, decoded} <- map_ok(constructors, &constructor(&1, &2, path)),
         true <- unique_by?(decoded, & &1.name) do
      {:ok, decoded}
    else
      false ->
        semantic_error(
          "A002",
          "constructor names must be unique within a type",
          path <> ".constructors"
        )

      {:error, _} = result ->
        result
    end
  end

  defp constructors(_, path), do: error("constructors must be a list", path <> ".constructors")

  defp constructor(value, index, declaration_path) do
    path = "#{declaration_path}.constructors[#{index}]"

    with :ok <- require_map(value, path),
         {:ok, name} <- name(value, "name", @type_name, path),
         {:ok, existentials} <-
           type_parameters(Map.get(value, "existentials", []), path <> ".existentials"),
         {:ok, fields, style} <- fields(Map.get(value, "fields", []), path) do
      {:ok,
       %{
         name: name,
         existentials: existentials,
         fields: fields,
         field_style: style,
         result: Map.get(value, "result"),
         path: path
       }}
    end
  end

  defp fields(fields, path) when is_list(fields) do
    named? =
      Enum.all?(fields, fn field ->
        is_map(field) and is_binary(Map.get(field, "name")) and Map.has_key?(field, "type")
      end)

    positional? = Enum.all?(fields, &(is_map(&1) and not Map.has_key?(&1, "type")))

    cond do
      fields == [] -> {:ok, [], :positional}
      named? -> named_fields(fields, path)
      positional? -> {:ok, fields, :positional}
      true -> error("constructor fields must be all positional or all named", path <> ".fields")
    end
  end

  defp fields(_, path), do: error("constructor fields must be a list", path <> ".fields")

  defp named_fields(fields, path) do
    decoded =
      fields
      |> Enum.with_index()
      |> Enum.map(fn {field, index} ->
        %{
          name: Map.get(field, "name"),
          type: Map.get(field, "type"),
          path: "#{path}.fields[#{index}]"
        }
      end)

    cond do
      not Enum.all?(decoded, &(Regex.match?(@value_name, &1.name) and not is_nil(&1.type))) ->
        error("named fields require value names and types", path <> ".fields")

      not unique_by?(decoded, & &1.name) ->
        semantic_error("A002", "named constructor fields must be unique", path <> ".fields")

      true ->
        {:ok, decoded, :named}
    end
  end

  defp imports(_value, "0.1"), do: {:ok, []}

  defp imports(value, version) when version in ~w(0.2 0.3) do
    imports = Map.get(value, "imports", [])

    cond do
      not is_list(imports) ->
        error("imports must be a list", "$.imports")

      version != "0.3" and
          Enum.any?(imports, &(is_map(&1) and Map.get(&1, "kind") == "condition")) ->
        semantic_error("CND001", "condition imports require AST 0.3", "$.imports")

      true ->
        {:ok, imports}
    end
  end

  defp definitions(%{"definitions" => definitions}, version) when is_list(definitions) do
    with {:ok, decoded} <- map_ok(definitions, &definition(&1, &2, version)),
         true <- unique_by?(decoded, & &1.name),
         true <- version == "0.3" or Enum.all?(decoded, &(not condition_operator?(&1.body))) do
      {:ok, decoded}
    else
      false ->
        if unique_by?(definitions, &Map.get(&1, "name")) do
          semantic_error("CND001", "condition operators require AST 0.3", "$.definitions")
        else
          error("definition names must be unique", "$.definitions")
        end

      {:error, _} = result ->
        result
    end
  end

  defp definitions(_, _version), do: error("definitions must be a list", "$.definitions")

  defp definition(value, index, version) do
    path = "$.definitions[#{index}]"
    kind = Map.get(value, "kind", "value")

    with :ok <- require_map(value, path),
         {:ok, name} <- name(value, "name", @value_name, path),
         :ok <- definition_kind(kind, version, path),
         {:ok, parameters, body, clause_definition?} <- definition_body(value, version, path) do
      {:ok,
       %{
         name: name,
         parameters: parameters,
         signature: Map.get(value, "signature"),
         body: body,
         kind: if(kind == "condition", do: :condition, else: :value),
         clause_definition?: clause_definition?,
         path: path
       }}
    end
  end

  defp definition_kind("value", _version, _path), do: :ok
  defp definition_kind("condition", "0.3", _path), do: :ok

  defp definition_kind("condition", _version, path),
    do: semantic_result("CND001", "condition declarations require AST 0.3", path <> ".kind")

  defp definition_kind(_kind, _version, path),
    do: semantic_result("CND001", "definition kind must be value or condition", path <> ".kind")

  defp definition_body(%{"clauses" => clauses} = value, "0.3", path) when is_list(clauses) do
    if Map.has_key?(value, "body") or Map.has_key?(value, "parameters") do
      semantic_error(
        "CND001",
        "a clause definition cannot also contain parameters or body",
        path
      )
    else
      with true <- clauses != [],
           {:ok, decoded} <- map_ok(clauses, &definition_clause(&1, &2, path)),
           [first | _] <- decoded,
           arity = length(first.patterns),
           true <- Enum.all?(decoded, &(length(&1.patterns) == arity)),
           true <- arity > 0 do
        parameters = Enum.map(0..(arity - 1), &"__clause_arg_#{&1}")

        {scrutinee, match_clauses} =
          if arity == 1 do
            {%{tag: :variable, name: hd(parameters), path: path <> ".clauses"},
             Enum.map(decoded, fn clause ->
               Map.put(clause, :pattern, hd(clause.patterns)) |> Map.delete(:patterns)
             end)}
          else
            {%{
               tag: :tuple,
               elements:
                 Enum.map(parameters, &%{tag: :variable, name: &1, path: path <> ".clauses"}),
               path: path <> ".clauses"
             },
             Enum.map(decoded, fn clause ->
               pattern = %{
                 tag: :tuple,
                 elements: clause.patterns,
                 path: clause.path <> ".patterns"
               }

               Map.put(clause, :pattern, pattern) |> Map.delete(:patterns)
             end)}
          end

        body = %{
          tag: :match,
          scrutinee: scrutinee,
          clauses: match_clauses,
          path: path <> ".clauses"
        }

        {:ok, parameters, body, true}
      else
        false ->
          semantic_error(
            "CND001",
            "clause definitions require one shared non-zero arity",
            path <> ".clauses"
          )

        [] ->
          semantic_error(
            "CND001",
            "clause definitions require at least one clause",
            path <> ".clauses"
          )

        {:error, _} = result ->
          result
      end
    end
  end

  defp definition_body(%{"clauses" => _clauses}, _version, path),
    do: semantic_error("CND001", "multi-clause definitions require AST 0.3", path <> ".clauses")

  defp definition_body(value, _version, path) do
    with {:ok, parameters} <- parameters(value, path),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok, parameters, body, false}
    end
  end

  defp definition_clause(value, index, definition_path) do
    path = "#{definition_path}.clauses[#{index}]"
    patterns = if is_map(value), do: Map.get(value, "patterns"), else: nil

    with :ok <- require_map(value, path),
         true <- is_list(patterns),
         {:ok, patterns} <- map_ok(patterns, &pattern(&1, "#{path}.patterns[#{&2}]")),
         {:ok, guard} <- optional_expression(Map.get(value, "guard"), path <> ".guard"),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok, %{patterns: patterns, guard: guard, body: body, path: path}}
    else
      false ->
        semantic_error("CND001", "definition clause patterns must be a list", path <> ".patterns")

      {:error, _} = result ->
        result
    end
  end

  defp parameters(value, path) do
    parameters = Map.get(value, "parameters", [])

    if is_list(parameters) and
         Enum.all?(parameters, &(is_binary(&1) and Regex.match?(@value_name, &1))) and
         length(parameters) == length(Enum.uniq(parameters)) do
      {:ok, parameters}
    else
      error("parameters must be unique value names", path <> ".parameters")
    end
  end

  defp expression(%{"tag" => tag} = value, path) when tag in @expression_tags do
    case tag do
      "integer" -> literal(value, "value", &is_integer/1, :integer, path)
      "boolean" -> literal(value, "value", &is_boolean/1, :boolean, path)
      "variable" -> variable(value, path)
      "function" -> function(value, path)
      "call" -> call(value, path)
      "let" -> let(value, path)
      "tuple" -> tuple(value, path)
      "annotate" -> annotate(value, path)
      "construct" -> construct(value, path)
      "match" -> match_expression(value, path)
      "unary" -> unary(value, path)
      "binary" -> binary(value, path)
    end
  end

  defp expression(%{"tag" => tag}, path),
    do: error("unknown expression tag #{inspect(tag)}", path)

  defp expression(_, path), do: error("expression must contain a tag", path)

  defp literal(value, key, predicate, tag, path) do
    literal = Map.get(value, key)

    if predicate.(literal),
      do: {:ok, %{tag: tag, value: literal, path: path}},
      else: error("invalid #{tag} literal", path)
  end

  defp variable(value, path) do
    with {:ok, name} <- name(value, "name", @value_name, path) do
      {:ok, %{tag: :variable, name: name, path: path}}
    end
  end

  defp function(value, path) do
    with {:ok, parameter} <- name(value, "parameter", @value_name, path),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok, %{tag: :function, parameter: parameter, body: body, path: path}}
    end
  end

  defp call(value, path) do
    arguments = Map.get(value, "arguments")

    with {:ok, callee} <- expression(Map.get(value, "callee"), path <> ".callee"),
         true <- is_list(arguments),
         {:ok, decoded} <- map_ok(arguments, &expression(&1, "#{path}.arguments[#{&2}]")) do
      {:ok, %{tag: :call, callee: callee, arguments: decoded, path: path}}
    else
      false -> error("call arguments must be a list", path <> ".arguments")
      {:error, _} = result -> result
    end
  end

  defp unary(value, path) do
    operator = Map.get(value, "operator")

    with true <- operator in ~w(not negate),
         {:ok, operand} <- expression(Map.get(value, "operand"), path <> ".operand") do
      {:ok, %{tag: :unary, operator: String.to_atom(operator), operand: operand, path: path}}
    else
      false ->
        semantic_error("CND001", "unsupported unary condition operator", path <> ".operator")

      {:error, _} = result ->
        result
    end
  end

  defp binary(value, path) do
    operator = Map.get(value, "operator")

    with true <-
           operator in ~w(and or equal not_equal less less_equal greater greater_equal add subtract multiply),
         {:ok, left} <- expression(Map.get(value, "left"), path <> ".left"),
         {:ok, right} <- expression(Map.get(value, "right"), path <> ".right") do
      {:ok,
       %{
         tag: :binary,
         operator: String.to_atom(operator),
         left: left,
         right: right,
         path: path
       }}
    else
      false ->
        semantic_error("CND001", "unsupported binary condition operator", path <> ".operator")

      {:error, _} = result ->
        result
    end
  end

  defp let(value, path) do
    with {:ok, name} <- name(value, "name", @value_name, path),
         {:ok, bound} <- expression(Map.get(value, "value"), path <> ".value"),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok, %{tag: :let, name: name, value: bound, body: body, path: path}}
    end
  end

  defp tuple(value, path) do
    elements = Map.get(value, "elements")

    if is_list(elements) do
      with {:ok, decoded} <- map_ok(elements, &expression(&1, "#{path}.elements[#{&2}]")) do
        {:ok, %{tag: :tuple, elements: decoded, path: path}}
      end
    else
      error("tuple elements must be a list", path <> ".elements")
    end
  end

  defp annotate(value, path) do
    with {:ok, expression} <- expression(Map.get(value, "expression"), path <> ".expression"),
         signature when is_map(signature) <- Map.get(value, "signature") do
      {:ok, %{tag: :annotate, expression: expression, signature: signature, path: path}}
    else
      nil -> error("annotation requires a signature", path <> ".signature")
      {:error, _} = result -> result
    end
  end

  defp construct(value, path) do
    with constructor when is_binary(constructor) <- Map.get(value, "constructor"),
         {:ok, arguments, style} <- construction_fields(value, path) do
      {:ok,
       %{
         tag: :construct,
         constructor: constructor,
         arguments: arguments,
         field_style: style,
         path: path
       }}
    else
      nil -> error("construction requires a constructor", path <> ".constructor")
      {:error, _} = result -> result
    end
  end

  defp construction_fields(%{"arguments" => arguments}, path) when is_list(arguments) do
    with {:ok, decoded} <- map_ok(arguments, &expression(&1, "#{path}.arguments[#{&2}]")) do
      {:ok, decoded, :positional}
    end
  end

  defp construction_fields(%{"fields" => fields}, path) when is_list(fields) do
    decoded =
      map_ok(fields, fn field, index ->
        field_path = "#{path}.fields[#{index}]"

        with :ok <- require_map(field, field_path),
             {:ok, name} <- name(field, "name", @value_name, field_path),
             {:ok, expression} <- expression(Map.get(field, "value"), field_path <> ".value") do
          {:ok, %{name: name, expression: expression, path: field_path}}
        end
      end)

    with {:ok, fields} <- decoded, true <- unique_by?(fields, & &1.name) do
      {:ok, fields, :named}
    else
      false -> semantic_error("A003", "construction fields must be unique", path <> ".fields")
      {:error, _} = result -> result
    end
  end

  defp construction_fields(_, path),
    do: error("construction requires arguments or fields", path)

  defp match_expression(value, path) do
    clauses = Map.get(value, "clauses")

    with {:ok, scrutinee} <- expression(Map.get(value, "scrutinee"), path <> ".scrutinee"),
         true <- is_list(clauses),
         {:ok, clauses} <- map_ok(clauses, &match_clause(&1, &2, path)) do
      {:ok, %{tag: :match, scrutinee: scrutinee, clauses: clauses, path: path}}
    else
      false -> error("match clauses must be a list", path <> ".clauses")
      {:error, _} = result -> result
    end
  end

  defp match_clause(value, index, match_path) do
    path = "#{match_path}.clauses[#{index}]"

    with :ok <- require_map(value, path),
         {:ok, pattern} <- pattern(Map.get(value, "pattern"), path <> ".pattern"),
         {:ok, guard} <- optional_expression(Map.get(value, "guard"), path <> ".guard"),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok, %{pattern: pattern, guard: guard, body: body, path: path}}
    end
  end

  defp optional_expression(nil, _path), do: {:ok, nil}
  defp optional_expression(value, path), do: expression(value, path)

  defp condition_operator?(%{tag: tag}) when tag in [:unary, :binary], do: true
  defp condition_operator?(%{tag: :function, body: body}), do: condition_operator?(body)

  defp condition_operator?(%{tag: :call, callee: callee, arguments: arguments}),
    do: condition_operator?(callee) or Enum.any?(arguments, &condition_operator?/1)

  defp condition_operator?(%{tag: :let, value: value, body: body}),
    do: condition_operator?(value) or condition_operator?(body)

  defp condition_operator?(%{tag: :tuple, elements: elements}),
    do: Enum.any?(elements, &condition_operator?/1)

  defp condition_operator?(%{tag: :annotate, expression: expression}),
    do: condition_operator?(expression)

  defp condition_operator?(%{tag: :construct, arguments: arguments}),
    do:
      Enum.any?(arguments, fn argument ->
        condition_operator?(Map.get(argument, :expression, argument))
      end)

  defp condition_operator?(%{tag: :match, scrutinee: scrutinee, clauses: clauses}),
    do:
      condition_operator?(scrutinee) or
        Enum.any?(clauses, fn clause ->
          (not is_nil(clause.guard) and condition_operator?(clause.guard)) or
            condition_operator?(clause.body)
        end)

  defp condition_operator?(_expression), do: false

  defp pattern(%{"tag" => tag} = value, path) when tag in @pattern_tags do
    case tag do
      "wildcard" -> {:ok, %{tag: :wildcard, path: path}}
      "bind" -> pattern_bind(value, path)
      "integer" -> literal(value, "value", &is_integer/1, :integer, path)
      "boolean" -> literal(value, "value", &is_boolean/1, :boolean, path)
      "tuple" -> pattern_list(value, "elements", :tuple, path)
      "constructor" -> constructor_pattern(value, path)
      "as" -> as_pattern(value, path)
      "or" -> or_pattern(value, path)
    end
  end

  defp pattern(%{"tag" => tag}, path),
    do: pattern_error("unsupported pattern tag #{inspect(tag)}", path)

  defp pattern(_, path), do: error("pattern must contain a tag", path)

  defp pattern_bind(value, path) do
    with {:ok, name} <- name(value, "name", @value_name, path) do
      {:ok, %{tag: :bind, name: name, path: path}}
    end
  end

  defp pattern_list(value, key, tag, path) do
    items = Map.get(value, key)

    if is_list(items) do
      with {:ok, decoded} <- map_ok(items, &pattern(&1, "#{path}.#{key}[#{&2}]")) do
        {:ok, %{tag: tag, elements: decoded, path: path}}
      end
    else
      error("#{key} must be a list", path <> "." <> key)
    end
  end

  defp constructor_pattern(value, path) do
    with constructor when is_binary(constructor) <- Map.get(value, "constructor"),
         {:ok, fields, style, rest?} <- pattern_fields(value, path) do
      {:ok,
       %{
         tag: :constructor,
         constructor: constructor,
         fields: fields,
         field_style: style,
         rest?: rest?,
         path: path
       }}
    else
      nil -> error("constructor pattern requires a constructor", path <> ".constructor")
      {:error, _} = result -> result
    end
  end

  defp pattern_fields(%{"arguments" => arguments}, path) when is_list(arguments) do
    with {:ok, decoded} <- map_ok(arguments, &pattern(&1, "#{path}.arguments[#{&2}]")) do
      {:ok, decoded, :positional, false}
    end
  end

  defp pattern_fields(%{"fields" => fields} = value, path) when is_list(fields) do
    decoded =
      map_ok(fields, fn field, index ->
        field_path = "#{path}.fields[#{index}]"

        with :ok <- require_map(field, field_path),
             {:ok, name} <- name(field, "name", @value_name, field_path),
             {:ok, pattern} <- pattern(Map.get(field, "pattern"), field_path <> ".pattern") do
          {:ok, %{name: name, pattern: pattern, path: field_path}}
        end
      end)

    with {:ok, fields} <- decoded, true <- unique_by?(fields, & &1.name) do
      {:ok, fields, :named, Map.get(value, "rest", false) == true}
    else
      false -> semantic_error("M003", "named pattern fields must be unique", path <> ".fields")
      {:error, _} = result -> result
    end
  end

  defp pattern_fields(_, path),
    do: error("constructor pattern requires arguments or fields", path)

  defp as_pattern(value, path) do
    with {:ok, pattern} <- pattern(Map.get(value, "pattern"), path <> ".pattern"),
         {:ok, name} <- name(value, "name", @value_name, path) do
      {:ok, %{tag: :as, pattern: pattern, name: name, path: path}}
    end
  end

  defp or_pattern(value, path) do
    alternatives = Map.get(value, "alternatives")

    if is_list(alternatives) and length(alternatives) >= 2 do
      with {:ok, decoded} <-
             map_ok(alternatives, &pattern(&1, "#{path}.alternatives[#{&2}]")) do
        {:ok, %{tag: :or, alternatives: decoded, path: path}}
      end
    else
      error("or pattern requires at least two alternatives", path <> ".alternatives")
    end
  end

  defp string_list(values, path)
       when is_list(values) do
    if Enum.all?(values, &is_binary/1) and length(values) == length(Enum.uniq(values)),
      do: {:ok, values},
      else: error("expected a list of unique strings", path)
  end

  defp string_list(_, path), do: error("expected a list of strings", path)

  defp name(value, key, regex, path) do
    case Map.get(value, key) do
      name when is_binary(name) ->
        if Regex.match?(regex, name),
          do: {:ok, name},
          else: error("invalid #{key} #{inspect(name)}", path <> "." <> key)

      _ ->
        error("missing or invalid #{key}", path <> "." <> key)
    end
  end

  defp require_map(value, _path) when is_map(value), do: :ok
  defp require_map(_, path), do: error("expected an object", path)

  defp unique_by?(items, function) do
    keys = Enum.map(items, function)
    length(keys) == length(Enum.uniq(keys))
  end

  defp map_ok(items, function) do
    items
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, acc} ->
      case function.(item, index) do
        {:ok, value} -> {:cont, {:ok, [value | acc]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  defp error(message, path), do: {:error, Diagnostic.new("T012", message, path: path)}
  defp pattern_error(message, path), do: {:error, Diagnostic.new("M005", message, path: path)}

  defp semantic_result(id, message, path),
    do: {:error, Diagnostic.new(id, message, path: path)}

  defp semantic_error(id, message, path),
    do: {:error, Diagnostic.new(id, message, path: path)}
end
