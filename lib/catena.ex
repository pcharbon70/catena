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
  or parser. Revision 0.1.14 elaborates scanned numeric literals into typed
  `Int` and finite binary64 `Float` values through one correctly rounded
  conversion. Revision 0.1.15 tokenizes complete source files into the
  whole-source token stream and resolves operator expressions over the fixed
  precedence ladder. Revision 0.1.16 binds `.cat` files to at most one
  declared module with basename verification and first-line generated
  markers. Revision 0.1.17 resolves names through per-category namespaces
  with deterministic shadowing and local-over-imported precedence, and
  revision 0.1.18 validates imports against digest-bound export sets with
  deny-able unused-import warnings. Revision 0.1.19 fixes the abstraction
  boundary, and revision 0.1.20 compiles module dependency cycles as
  strongly-connected components with joint digests.
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

  @doc """
  Elaborates one scanned numeric literal at exact revision 0.1.14.

  Accepts the exact `Catena.Literal.Numeric` components of one scanned token
  and returns its typed meaning, or one stable diagnostic. Numeric unary
  negation is available through `Catena.Numeric.negate/1`.
  """
  @spec elaborate_numeric_literal(Catena.Literal.Numeric.t(), keyword()) ::
          {:ok, Catena.Numeric.Meaning.t()} | {:error, Catena.Diagnostic.t()}
  def elaborate_numeric_literal(numeric, options \\ []),
    do: Catena.Numeric.elaborate(numeric, options)

  @doc """
  Tokenizes one complete Catena source at exact revision 0.1.15.

  Returns the lossless whole-source token stream of names, comments,
  literals, and operator/punctuation tokens with original-byte spans,
  continuation capabilities, and delimiter frame events, or one stable
  diagnostic.
  """
  @spec tokenize_source(binary(), keyword()) ::
          {:ok, Catena.Tokenizer.Result.t()} | {:error, Catena.Diagnostic.t()}
  def tokenize_source(source, options \\ []), do: Catena.Tokenizer.tokenize(source, options)

  @doc """
  Resolves one token-stream region into an operator-expression tree.

  Applies the fixed 0.1.15 precedence ladder and returns the tree or exactly
  one stable diagnostic; no recovery or partial output exists.
  """
  @spec parse_operator_expression([Catena.Tokenizer.Token.t()]) ::
          {:ok, Catena.Operator.Expression.t()} | {:error, Catena.Diagnostic.t()}
  def parse_operator_expression(tokens), do: Catena.Operator.parse(tokens)

  @doc """
  Resolves one `.cat` file unit at exact revision 0.1.16.

  Accepts source bytes, a filename, and parser-supplied module-declaration
  events, and returns the file unit — module or no-module, declared name,
  generated flag, and tool identifier — or one stable diagnostic.
  """
  @spec resolve_file_unit(binary(), binary(), [Catena.FileUnit.ModuleDeclaration.t()], keyword()) ::
          {:ok, Catena.FileUnit.Result.t()} | {:error, Catena.Diagnostic.t()}
  def resolve_file_unit(source, filename, module_declarations, options \\ []),
    do: Catena.FileUnit.resolve(source, filename, module_declarations, options)

  @doc """
  Builds one namespace environment from a scope-event stream at exact
  revision 0.1.17.
  """
  @spec build_namespace_environment([map() | atom()], keyword()) ::
          {:ok, Catena.Namespace.Environment.t()} | {:error, Catena.Diagnostic.t()}
  def build_namespace_environment(events, options \\ []),
    do: Catena.Namespace.build_environment(events, options)

  @doc """
  Resolves one reference against a namespace environment at exact revision
  0.1.17, returning its nominal identity or one stable diagnostic.
  """
  @spec resolve_name(Catena.Namespace.Environment.t(), map()) ::
          {:ok, Catena.Namespace.Resolution.t()} | {:error, Catena.Diagnostic.t()}
  def resolve_name(environment, reference), do: Catena.Namespace.resolve(environment, reference)

  @doc """
  Analyzes unused imports over a built namespace environment and a
  reference set at exact revision 0.1.18, returning deny-able warnings
  only.
  """
  @spec check_unused_imports(Catena.Namespace.Environment.t(), [map()]) ::
          {:ok, [Catena.Namespace.ImportWarning.t()]}
  def check_unused_imports(environment, references),
    do: Catena.Namespace.check_unused_imports(environment, references)

  @doc """
  Compiles one strongly-connected component of mutually dependent modules
  at exact revision 0.1.20.

  Every member is checked and compiled against its companions' declared
  provisional interfaces and outside digest-bound interfaces; the component
  yields its members' binaries and interfaces plus one deterministic joint
  digest, or exactly one diagnostic.
  """
  @spec compile_scc([binary()], keyword()) ::
          {:ok, Catena.Scc.Result.t()} | {:error, Catena.Diagnostic.t()}
  def compile_scc(sources, options \\ []),
    do: Catena.Scc.compile(sources, options)

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
