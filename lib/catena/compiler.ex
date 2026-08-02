defmodule Catena.Compiler do
  @moduledoc "The C001 compiler pipeline and its typed-core verification gate."

  alias Catena.{Backend.ErlangAbstract, Diagnostic, Type.Infer}
  alias Catena.OTP.Compiler, as: OTPCompiler
  alias Catena.TypedCore.Verifier

  @spec check(map()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def check(ast) do
    protect(fn ->
      core = Infer.module(ast) |> Map.put(:source, ast.source)

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
    with {:ok, core} <- check(ast),
         forms <- ErlangAbstract.lower(core),
         {:ok, module, binary, warnings} <-
           OTPCompiler.compile(forms, Keyword.put_new(options, :source, ast.source)) do
      {:ok, module, binary, %{core: core, warnings: warnings, forms: forms}}
    end
  end

  defp protect(function) do
    function.()
  rescue
    error in Catena.TypeError -> {:error, error.diagnostic}
  end
end
