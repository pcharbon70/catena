defmodule Catena do
  @moduledoc """
  The bootstrap Catena compiler API.

  Version 0.1 accepts a versioned JSON AST, checks and elaborates it, verifies
  the typed core, lowers it to Erlang Abstract Format, and delegates BEAM
  generation to OTP 29.
  """

  alias Catena.{AST.Decoder, Compiler}

  @spec check_json(binary()) :: {:ok, map()} | {:error, Catena.Diagnostic.t()}
  def check_json(json) do
    with {:ok, ast} <- Decoder.decode(json) do
      Compiler.check(ast)
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
