defmodule Catena.Scc do
  @moduledoc """
  The Catena 0.1.20 strongly-connected-component compilation boundary.

  All member programs of one module dependency component are compiled
  together: each member's provisional interface is built from its declared
  types and exports, every member is checked and compiled against its
  companions' provisional interfaces plus outside digest-bound interfaces,
  each computed interface is cross-verified against its provisional one,
  and the component yields one deterministic joint digest. It does not
  parse source text, resolve operator expressions, or assemble packages.
  """

  alias Catena.{AST.Decoder, Compiler, Diagnostic, Interface}

  defmodule Result do
    @moduledoc "One compiled component: its members and the joint digest."

    @enforce_keys [:members, :scc_digest]
    defstruct @enforce_keys

    @type member :: %{
            module: String.t(),
            binary: binary(),
            interface: map(),
            digest: String.t()
          }

    @type t :: %__MODULE__{
            members: [member()],
            scc_digest: String.t()
          }
  end

  @spec compile([binary()], keyword()) :: {:ok, Result.t()} | {:error, Diagnostic.t()}
  def compile(sources, options \\ []) when is_list(sources) and is_list(options) do
    layout = Keyword.get(options, :layout, :compact)
    outside = Keyword.get(options, :interfaces, [])

    with {:ok, asts} <- decode_all(sources),
         {:ok, provisionals} <- build_provisional(asts, options),
         {:ok, _cores} <- check_all(asts, provisionals, outside, options),
         {:ok, members} <- compile_all(asts, provisionals, outside, layout, options),
         :ok <- cross_verify(members, provisionals) do
      {:ok, %Result{members: members, scc_digest: joint_digest(members)}}
    end
  end

  defp decode_all(sources) do
    sources
    |> Enum.reduce_while({:ok, []}, fn source, {:ok, asts} ->
      case Decoder.decode(source, []) do
        {:ok, ast} -> {:cont, {:ok, [ast | asts]}}
        {:error, %Diagnostic{}} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, reversed} ->
        asts = Enum.reverse(reversed)

        modules = Enum.map(asts, & &1.module)

        if length(Enum.uniq(modules)) == length(modules) do
          {:ok, asts}
        else
          {:error,
           Diagnostic.new(
             "CYC001",
             "a component member exports a name without a declared signature",
             details: %{reason: "duplicate_member", modules: modules}
           )}
        end

      error ->
        error
    end
  end

  defp build_provisional(asts, options) do
    seeds = header_seeds(asts)

    with {:ok, first_round} <- stub_round(asts, seeds, options),
         {:ok, second_round} <- stub_round(asts, first_round, options) do
      {:ok, second_round}
    end
  end

  defp header_seeds(asts) do
    declarations =
      Enum.flat_map(asts, fn ast ->
        Enum.map(ast.type_groups |> Enum.flat_map(& &1.declarations), fn declaration ->
          {nominal_id(ast.origin, ast.module, declaration.name), declaration}
        end)
      end)

    inhabitation = inhabitation_fixed_point(declarations)

    Enum.map(asts, fn ast ->
      types =
        ast.type_groups
        |> Enum.flat_map(& &1.declarations)
        |> Enum.map(fn declaration ->
          %{
            id: nominal_id(ast.origin, ast.module, declaration.name),
            origin: ast.origin,
            module: ast.module,
            name: declaration.name,
            arity: length(declaration.parameters || []),
            visibility: :transparent,
            inhabitation:
              Map.get(
                inhabitation,
                nominal_id(ast.origin, ast.module, declaration.name),
                :unknown
              ),
            variance: [],
            positive?: false,
            regular?: false,
            derivations: [],
            imported?: true,
            path: "provisional://#{ast.module}/#{declaration.name}",
            constructors: []
          }
        end)

      {ast.module, %{types: types}}
    end)
    |> Map.new()
  end

  defp nominal_id(origin, module, name), do: "#{origin}::#{module}::#{name}"

  defp inhabitation_fixed_point(declarations) do
    fields_by_id =
      Map.new(declarations, fn {id, declaration} ->
        {id, Enum.map(declaration.constructors, & &1.fields)}
      end)

    iterate_inhabitation(Map.keys(fields_by_id), fields_by_id, %{})
  end

  defp iterate_inhabitation(ids, fields_by_id, statuses) do
    next =
      Map.new(ids, fn id ->
        constructor_sets = Map.fetch!(fields_by_id, id)

        status =
          if Enum.empty?(constructor_sets) do
            :empty
          else
            if Enum.any?(constructor_sets, fn fields ->
                 Enum.all?(fields, &field_inhabited?(&1, statuses))
               end) do
              :inhabited
            else
              Map.get(statuses, id, :empty)
            end
          end

        {id, status}
      end)

    if next == statuses do
      statuses
    else
      iterate_inhabitation(ids, fields_by_id, next)
    end
  end

  defp field_inhabited?(field, _statuses) do
    case field do
      %{"type" => %{"tag" => "named", "name" => qualified}} ->
        [_module, _name] = String.split(qualified, ".", parts: 2)
        true

      %{"type" => %{"tag" => "variable"}} ->
        true

      _ ->
        true
    end
  end

  defp stub_round(asts, seeds, options) do
    Enum.reduce_while(asts, {:ok, %{}}, fn ast, {:ok, provisionals} ->
      companions =
        seeds
        |> Enum.reject(fn {module, _} -> module == ast.module end)
        |> Enum.map(fn {_module, interface} -> interface end)

      stub =
        ast
        |> Map.put(:definitions, [])
        |> Map.put(:exports, [])
        |> Map.put(:imports, [])

      with {:ok, core} <- Compiler.check(stub, Keyword.put(options, :interfaces, companions)),
           encoded = Interface.build(core),
           {:ok, decoded} <- Interface.decode(Interface.encode(encoded)) do
        {:cont, {:ok, Map.put(provisionals, ast.module, decoded)}}
      else
        {:error, %Diagnostic{}} = error ->
          {:halt, error}
      end
    end)
  end

  defp check_all(asts, provisionals, outside, options) do
    Enum.reduce_while(asts, {:ok, %{}}, fn ast, {:ok, cores} ->
      companions =
        provisionals
        |> Enum.reject(fn {module, _} -> module == ast.module end)
        |> Enum.map(fn {_module, interface} -> interface end)

      case Compiler.check(ast, Keyword.put(options, :interfaces, companions ++ outside)) do
        {:ok, core} -> {:cont, {:ok, Map.put(cores, ast.module, core)}}
        {:error, %Diagnostic{}} = error -> {:halt, error}
      end
    end)
  end

  defp compile_all(asts, provisionals, outside, layout, options) do
    Enum.reduce_while(asts, {:ok, []}, fn ast, {:ok, members} ->
      companions =
        provisionals
        |> Enum.reject(fn {module, _} -> module == ast.module end)
        |> Enum.map(fn {_module, interface} -> interface end)

      compile_options =
        options |> Keyword.put(:interfaces, companions ++ outside) |> Keyword.put(:layout, layout)

      with {:ok, _module, binary, metadata} <- Compiler.compile(ast, compile_options),
           {:ok, interface} <- Interface.decode(metadata.interface_binary) do
        {:cont,
         {:ok,
          [
            %{
              module: ast.module,
              binary: binary,
              interface: interface,
              digest: interface.digest
            }
            | members
          ]}}
      else
        {:error, %Diagnostic{}} = error -> {:halt, error}
      end
    end)
  end

  defp cross_verify(members, provisionals) do
    members
    |> Enum.find_value(:ok, fn member ->
      provisional = Map.fetch!(provisionals, member.module)

      if type_surface(provisional) == type_surface(member.interface) do
        nil
      else
        {:error,
         Diagnostic.new(
           "I001",
           "component cross-verification failed: a computed interface diverges from its declared surface",
           details: %{reason: "interface_mismatch", module: member.module}
         )}
      end
    end)
  end

  defp type_surface(interface) do
    interface.types
    |> Enum.map(fn type ->
      constructors =
        type
        |> Map.get(:constructors, [])
        |> Enum.map(&{&1.id, &1.name, &1.index, length(&1.fields)})

      {type.id, type.name, type.arity, type.visibility, constructors}
    end)
    |> Enum.sort()
  end

  defp joint_digest(members) do
    members
    |> Enum.map(&"#{&1.module}:#{&1.digest}")
    |> Enum.sort()
    |> Enum.join("\n")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end
end
