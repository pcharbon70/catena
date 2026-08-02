defmodule Catena.Compiler do
  @moduledoc "The C001/C002 compiler pipeline and its typed-core verification gate."

  alias Catena.{Backend.ErlangAbstract, Diagnostic, Interface, Type.Infer}
  alias Catena.OTP.Compiler, as: OTPCompiler
  alias Catena.TypedCore.Verifier

  @spec check(map(), keyword()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def check(ast, options \\ []) do
    protect(fn ->
      core = Infer.module(ast, options) |> Map.put(:source, ast.source)

      case Verifier.verify(core) do
        :ok ->
          {:ok, core}

        {:error, reason} ->
          {:error, Diagnostic.new("I001", "typed-core verification failed: #{reason}")}
      end
    end)
  end

  @spec compile(map(), keyword()) :: {:ok, module(), binary(), map()} | {:error, Diagnostic.t()}
  def compile(ast, options \\ []) do
    layout = Keyword.get(options, :layout, :compact)

    with :ok <- validate_layout(layout),
         {:ok, core} <- check(ast, options),
         forms <- ErlangAbstract.lower(core, layout: layout),
         {:ok, module, binary, warnings} <-
           OTPCompiler.compile(
             forms,
             options
             |> Keyword.put_new(:source, ast.source)
             |> Keyword.put(:frontend_version, ast.frontend_version)
             |> Keyword.put(
               :specification,
               if(ast.frontend_version == "0.1", do: "0.1", else: "0.2")
             )
           ) do
      interface = Interface.build(core)

      {:ok, module, binary,
       %{
         core: core,
         warnings: warnings,
         forms: forms,
         layout: layout,
         interface: interface,
         interface_binary: Interface.encode(interface)
       }}
    end
  end

  defp validate_layout(layout) when layout in [:uniform, :compact], do: :ok

  defp validate_layout(layout),
    do: {:error, Diagnostic.new("L001", "unknown ADT layout #{inspect(layout)}")}

  defp protect(function) do
    function.()
  rescue
    error in Catena.TypeError -> {:error, error.diagnostic}
  end
end
