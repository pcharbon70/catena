defmodule Catena do
  @moduledoc """
  The bootstrap Catena compiler API.

  Versions 0.1 and 0.2 accept a versioned JSON AST, check and elaborate it, verify
  the typed core, lowers it to Erlang Abstract Format, and delegates BEAM
  generation to OTP 29.
  """

  alias Catena.{AST.Decoder, Compiler}

  @spec check_json(binary(), keyword()) :: {:ok, map()} | {:error, Catena.Diagnostic.t()}
  def check_json(json, options \\ []) do
    with {:ok, ast} <- Decoder.decode(json) do
      Compiler.check(ast, options)
    end
  end

  @spec compile_json(binary(), keyword()) ::
          {:ok, module(), binary(), map()} | {:error, Catena.Diagnostic.t()}
  def compile_json(json, options \\ []) do
    with {:ok, ast} <- Decoder.decode(json) do
      Compiler.compile(ast, options)
    end
  end
end
