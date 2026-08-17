defmodule Catena do
  @moduledoc """
  The bootstrap Catena compiler API.

  Versions 0.1.1 through 0.1.7 accept a versioned JSON AST. Normative revision
  0.1.8 additionally accepts the exact semantic-kernel S-expression format.
  Revision 0.1.9 defines the strict source-text envelope used by future
  ergonomic syntax without yet supplying a lexer or parser for that syntax.
  """

  alias Catena.{AST.Decoder, Compiler}
  alias Catena.Kernel.{Backend, Checker, Parser}

  @spec decode_source_text(binary(), keyword()) ::
          {:ok, Catena.SourceText.t()} | {:error, Catena.Diagnostic.t()}
  def decode_source_text(source, options \\ []), do: Catena.SourceText.decode(source, options)

  @spec check_json(binary(), keyword()) :: {:ok, map()} | {:error, Catena.Diagnostic.t()}
  def check_json(json, options \\ []) do
    with {:ok, ast} <- Decoder.decode(json, options) do
      Compiler.check(ast, options)
    end
  end

  @spec compile_json(binary(), keyword()) ::
          {:ok, module(), binary(), map()} | {:error, Catena.Diagnostic.t()}
  def compile_json(json, options \\ []) do
    with {:ok, ast} <- Decoder.decode(json, options) do
      Compiler.compile(ast, options)
    end
  end

  @spec check_kernel(binary(), keyword()) :: {:ok, map()} | {:error, Catena.Diagnostic.t()}
  def check_kernel(source, options \\ []) do
    with {:ok, module} <- Parser.parse(source, options) do
      Checker.check(module, options)
    end
  end

  @spec compile_kernel(binary(), keyword()) ::
          {:ok, module(), binary(), map()} | {:error, Catena.Diagnostic.t()}
  def compile_kernel(source, options \\ []) do
    with {:ok, core} <- check_kernel(source, options) do
      Backend.compile(core, options)
    end
  end
end
