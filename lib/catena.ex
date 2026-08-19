defmodule Catena do
  @moduledoc """
  The bootstrap Catena compiler API.

  Versions 0.1.1 through 0.1.7 accept a versioned JSON AST. Normative revision
  0.1.8 additionally accepts the exact semantic-kernel S-expression format.
  Revision 0.1.9 defines the strict source-text envelope used by future
  ergonomic syntax. Revision 0.1.10 validates standalone identifiers and
  qualified names. Revision 0.1.11 resolves whitespace, separators, and line
  continuation over lexer-supplied token events. Revision 0.1.12 scans nested
  comments and attaches outer documentation comments to parser-supplied
  declaration targets. Revision 0.1.13 scans one atomic literal with decoded
  payload and exact source provenance without yet supplying a complete lexer
  or parser.
  """

  alias Catena.{AST.Decoder, Compiler}
  alias Catena.Kernel.{Backend, Checker, Parser}

  @spec decode_source_text(binary(), keyword()) ::
          {:ok, Catena.SourceText.t()} | {:error, Catena.Diagnostic.t()}
  def decode_source_text(source, options \\ []), do: Catena.SourceText.decode(source, options)

  @spec parse_identifier(binary(), keyword()) ::
          {:ok, Catena.Identifier.t()} | {:error, Catena.Diagnostic.t()}
  def parse_identifier(source, options \\ []), do: Catena.Identifier.parse(source, options)

  @spec parse_qualified_name(binary(), keyword()) ::
          {:ok, Catena.QualifiedName.t()} | {:error, Catena.Diagnostic.t()}
  def parse_qualified_name(source, options \\ []),
    do: Catena.QualifiedName.parse(source, options)

  @spec audit_identifiers([binary()], keyword()) ::
          {:ok, [Catena.QualifiedName.t()], [Catena.Diagnostic.t()]}
          | {:error, Catena.Diagnostic.t()}
  def audit_identifiers(names, options \\ []), do: Catena.IdentifierAudit.audit(names, options)

  @spec resolve_layout([Catena.Layout.event()], keyword()) ::
          {:ok, Catena.Layout.Result.t()} | {:error, Catena.Diagnostic.t()}
  def resolve_layout(events, options \\ []), do: Catena.Layout.resolve(events, options)

  @spec scan_comment(binary(), keyword()) ::
          {:ok, Catena.Comment.ScanResult.t()} | {:error, Catena.Diagnostic.t()}
  def scan_comment(source, options \\ []), do: Catena.Comment.scan(source, options)

  @spec resolve_comments([Catena.Comment.event()], keyword()) ::
          {:ok, Catena.Comment.Result.t()} | {:error, Catena.Diagnostic.t()}
  def resolve_comments(events, options \\ []), do: Catena.Comment.resolve(events, options)

  @spec scan_literal(binary(), keyword()) ::
          {:ok, Catena.Literal.ScanResult.t()} | {:error, Catena.Diagnostic.t()}
  def scan_literal(source, options \\ []), do: Catena.Literal.scan(source, options)

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
