defmodule Catena.Standard.Minimum do
  @moduledoc "Explicit digest-bound assembly of existing standard contracts; no implicit source names."
  alias Catena.Categorical.Standard
  @external_resource "priv/stdlib/catena-standard-0.1.69.json"
  @catalog JSON.decode!(File.read!("priv/stdlib/catena-standard-0.1.69.json"))
  @external_resource "priv/stdlib/catena-foundation-0.1.69.json"
  @foundation JSON.decode!(File.read!("priv/stdlib/catena-foundation-0.1.69.json"))
  def catalog!, do: @catalog
  def foundation!, do: @foundation

  def selection(services \\ false) when is_boolean(services) do
    %{
      "format" => "catena-minimum-selection",
      "version" => "0.1.69",
      "package" => @catalog["package"],
      "package_version" => @catalog["package_version"],
      "digest" => @catalog["digest"],
      "services" => services
    }
  end

  def decode(binary) when is_binary(binary) and byte_size(binary) <= 16384 do
    with {:ok, value} <- JSON.decode(binary), :ok <- valid_selection(value), do: {:ok, value}
  end

  def decode(_), do: {:error, :invalid_minimum_selection}

  defp valid_selection(nil), do: :ok

  defp valid_selection(value) when is_map(value) do
    if value == selection(false) or value == selection(true),
      do: :ok,
      else: {:error, :invalid_minimum_selection}
  end

  defp valid_selection(_), do: {:error, :invalid_minimum_selection}

  def resolve(selection, available \\ [@catalog])
  def resolve(nil, _), do: {:ok, %{version: "0.1.69", selection: nil, catalog: nil}}

  def resolve(selected, available) when is_list(available) and length(available) <= 64 do
    with :ok <- valid_selection(selected),
         matches <-
           Enum.filter(
             available,
             &(is_map(&1) and &1["package"] == selected["package"] and
                 &1["package_version"] == selected["package_version"])
           ),
         [catalog] <- matches,
         :ok <- verify_catalog(catalog) do
      {:ok, %{version: "0.1.69", selection: selected, catalog: catalog}}
    else
      [] -> {:error, :missing_minimum_package}
      [_ | _] -> {:error, :conflicting_minimum_package}
      error -> error
    end
  end

  def resolve(_, _), do: {:error, :invalid_minimum_selection}

  def verify(context) do
    case resolve(context.selection) do
      {:ok, ^context} -> :ok
      _ -> {:error, :invalid_minimum_context}
    end
  rescue
    _ -> {:error, :invalid_minimum_context}
  end

  def verify_catalog(catalog) do
    with true <- catalog == @catalog,
         true <- catalog["digest"] == Standard.digest(Map.delete(catalog, "digest")),
         true <- catalog["hierarchy_digest"] == Standard.interface!()["digest"],
         true <-
           Enum.all?(packages(), fn {role, package} ->
             component = catalog["components"][role]

             package["digest"] == component["digest"] and
               package["digest"] == Standard.digest(Map.delete(package, "digest")) and
               package["hierarchy_digest"] == catalog["hierarchy_digest"] and
               package["ast"]["module"] == component["module"] and
               package["ast"]["origin"] == component["origin"] and
               package["ast"]["exports"] == component["values"] and
               package["ast"]["type_exports"] == component["types"]
           end) do
      :ok
    else
      _ -> {:error, :invalid_minimum_catalog}
    end
  rescue
    _ -> {:error, :invalid_minimum_catalog}
  end

  defp packages,
    do: %{
      "foundation" => @foundation,
      "outcomes" => Catena.Standard.Outcomes.package!(),
      "collections" => Catena.Standard.Collections.package!(),
      "text" => Catena.Standard.Text.Indices.package!(),
      "numeric" => Catena.Standard.Numeric.Package.package!()
    }

  def compile_libraries(context) do
    with :ok <- verify(context) do
      if is_nil(context.selection) do
        {:ok, []}
      else
        Enum.reduce_while(Enum.sort(packages()), {:ok, []}, fn {role, package}, {:ok, acc} ->
          case Catena.compile_json(JSON.encode!(package["ast"]), layout: :uniform) do
            {:ok, module, binary, metadata} ->
              case Catena.Interface.decode(metadata.interface_binary) do
                {:ok, interface} ->
                  if interface.standard_digest == @catalog["hierarchy_digest"],
                    do:
                      {:cont,
                       {:ok,
                        acc ++
                          [
                            %{
                              role: role,
                              module: module,
                              binary: binary,
                              metadata: metadata,
                              interface: interface
                            }
                          ]}},
                    else: {:halt, {:error, :minimum_interface_mismatch}}

                error ->
                  {:halt, error}
              end

            error ->
              {:halt, error}
          end
        end)
      end
    end
  end

  def dependency(context) do
    with :ok <- verify(context), {:ok, libraries} <- compile_libraries(context) do
      if is_nil(context.selection) do
        {:ok, %{dependencies: %{}}, %{}}
      else
        name = @catalog["package"]
        version = @catalog["package_version"]

        metadata = %{
          interface_digests: Enum.map(libraries, & &1.metadata.interface["digest"]),
          component_digests: [
            @catalog["digest"] | Enum.map(@catalog["components"], fn {_, c} -> c["digest"] end)
          ],
          dependencies: %{}
        }

        {:ok, %{dependencies: %{}, prelude: %{"package" => name, "requirement" => version}},
         %{name => %{version => metadata}}}
      end
    end
  end

  def lock(context) do
    with {:ok, root, environment} <- dependency(context),
         {:ok, resolved} <- Catena.Package.Deps.resolve(root, environment),
         do: {:ok, Catena.Package.Deps.generate_lockfile(resolved)}
  end

  def replay(context, lock_bytes) do
    with {:ok, root, environment} <- dependency(context),
         {:ok, resolved} <- Catena.Package.Deps.resolve(root, environment),
         true <- lock_bytes == Catena.Package.Deps.generate_lockfile(resolved) do
      Catena.Package.Deps.replay_lockfile(root, lock_bytes, fn name, version, digest ->
        Enum.any?(
          resolved,
          &(&1.name == name and &1.version == version and &1.bundle_digest == digest)
        )
      end)
    else
      false -> {:error, :minimum_lock_mismatch}
      error -> error
    end
  end

  def compile_application(context, source) when is_binary(source) do
    with {:ok, libraries} <- compile_libraries(context),
         {:ok, module, binary, metadata} <-
           Catena.compile_json(source,
             interfaces: Enum.map(libraries, & &1.interface),
             layout: :uniform
           ) do
      {:ok,
       %{
         module: module,
         binary: binary,
         metadata: metadata,
         libraries: libraries,
         selection: context.selection,
         catalog_digest: if(context.catalog, do: context.catalog["digest"], else: nil)
       }}
    end
  end

  def prepare(context, kind, core, entry, operations, limits) do
    with :ok <- require_component(context, kind) do
      case kind do
        :numeric -> Catena.Standard.Numeric.Program.build(core, entry, operations, limits)
        :text -> Catena.Standard.Text.Program.build(core, entry, operations, limits)
        :environment -> Catena.Runtime.Environment.Program.build(core, entry, operations, limits)
        _ -> {:error, :unknown_minimum_component}
      end
    end
  end

  def invoke(context, kind, artifact, core, entry, operations, argument, limits) do
    with :ok <- require_component(context, kind) do
      case kind do
        :numeric ->
          Catena.Standard.Numeric.Program.invoke(
            artifact,
            core,
            entry,
            operations,
            argument,
            limits
          )

        :text ->
          Catena.Standard.Text.Program.invoke(artifact, core, entry, operations, argument, limits)

        :environment ->
          Catena.Runtime.Environment.Program.invoke(
            artifact,
            core,
            entry,
            operations,
            limits,
            argument
          )

        _ ->
          {:error, :unknown_minimum_component}
      end
    end
  end

  defp require_component(context, kind) do
    with :ok <- verify(context) do
      cond do
        is_nil(context.selection) ->
          {:error, :minimum_not_selected}

        kind == :environment and not context.selection["services"] ->
          {:error, :environment_not_selected}

        kind not in [:text, :numeric, :environment] ->
          {:error, :unknown_minimum_component}

        true ->
          :ok
      end
    end
  end
end
