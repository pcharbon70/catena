defmodule Catena.DebuggingMetadataTest do
  use ExUnit.Case, async: false
  alias Catena.Debugging

  @moduletag obligations:
               ~w(DB-OBL-001 DB-OBL-002 DB-OBL-003 DB-OBL-004 DB-OBL-005 DB-OBL-006 DB-OBL-007 DB-OBL-008)

  defp input(source), do: %{kind: :kernel, source: source, path: "src/./debug.catena"}

  defp stack(artifact, fun) do
    assert {:module, module} =
             Catena.OTP.Compiler.load(artifact.module, ~c"debug", artifact.binary)

    try do
      fun.(module)
      flunk("expected executed failure")
    catch
      :error, _ -> __STACKTRACE__
    after
      :code.delete(module)
      :code.purge(module)
    end
  end

  test "actual escaped closure and handler failures map to exact kernel spans" do
    source = """
    (module DebugClosure (edition 0.1) (revision 0.1.8) (origin "private://not-in-beam")
      (export value maker)
      (def maker (signature (Tuple Int (Fn Int (effects) Int)) (uses))
        (tuple 1 (fn (value Int) (trap 77)))))
    """

    input = input(source)
    assert {:ok, artifact} = Debugging.build(input)

    trace =
      stack(artifact, fn module ->
        {_, closure} = apply(module, :maker, [])
        closure.(3)
      end)

    assert {:ok, frames} = Catena.Report.debug_frames(artifact, input, trace)
    frame = Enum.find(frames, &(&1.origin != nil))
    assert frame.origin.primary.path == "src/debug.catena"
    span = frame.origin.primary.span
    assert binary_part(source, span.byte_start, span.byte_end - span.byte_start) == "(trap 77)"
    assert Enum.all?(frames, &(&1.values == :redacted))
    refute artifact.binary =~ "private://not-in-beam"
    changed_input = %{input | source: String.replace(source, "trap 77", "trap 78")}
    assert {:ok, other} = Debugging.build(changed_input)
    assert {:ok, unrelated} = Debugging.frames(other, changed_input, trace)
    assert Enum.all?(unrelated, &is_nil(&1.origin))

    assert {:error, :unverified_debug_sidecar} =
             Debugging.frames(put_in(artifact.sidecar.nodes, %{}), input, trace)

    assert {:error, :unverified_debug_sidecar} =
             Debugging.frames(Map.delete(artifact, :sidecar), input, trace)

    assert {:error, :unverified_debug_sidecar} =
             Debugging.frames(%{artifact | binary: artifact.binary <> "tamper"}, input, trace)

    source = """
    (module DebugHandler (edition 0.1) (revision 0.1.8) (origin "test://handler")
      (export value main)
      (effect Ask (operation ask (params Int) Int))
      (handler Stop (effect Ask) (input Int) (output Int)
        (return result (var result))
        (operation ask (params (value Int)) (resume next) (trap 88)))
      (def main (signature Int (uses)) (handle Stop (request Ask ask 1))))
    """

    input = input(source)
    assert {:ok, artifact} = Debugging.build(input)
    trace = stack(artifact, &apply(&1, :main, []))
    assert {:ok, frames} = Debugging.frames(artifact, input, trace)
    frame = Enum.find(frames, &(&1.origin != nil))
    span = frame.origin.primary.span
    assert binary_part(source, span.byte_start, span.byte_end - span.byte_start) == "(trap 88)"
  end

  test "bounded explicit inlining keeps the fault source and call-site history" do
    input =
      input("""
      (module DebugInline (edition 0.1) (revision 0.1.8) (origin "test://inline")
        (export value main)
        (def leaf (signature Int (uses)) (trap 19))
        (def middle (signature Int (uses)) (var leaf))
        (def main (signature Int (uses)) (var middle)))
      """)

    for depth <- [0, 1, 2] do
      options = [inline_depth: depth]
      assert {:ok, artifact} = Debugging.build(input, options)
      trace = stack(artifact, &apply(&1, :main, []))
      assert {:ok, frames} = Debugging.frames(artifact, input, trace, build: options)
      frame = Enum.find(frames, &(&1.origin != nil))
      assert frame.origin.primary.span.line_start == 3

      if depth == 2,
        do: assert(Enum.count(frame.origin.chain, &match?(%{kind: :inline}, &1)) == 2)
    end

    assert {:ok, bounded} = Debugging.build(input, inline_depth: 2, max_chain: 1)
    assert Enum.any?(bounded.sidecar.nodes, fn {_, entry} -> entry.omitted > 0 end)
    assert {:error, :debug_node_limit} = Debugging.build(input, max_nodes: 1)
    assert {:error, :invalid_debug_profile} = Debugging.build(input, inline_depth: 9)
    assert {:error, :debug_source_limit} = Debugging.build(input, max_source_bytes: 1)
  end

  test "stripped and unknown host frames remain unmapped and redact arguments and metadata" do
    input =
      input(
        "(module DebugStripped (edition 0.1) (revision 0.1.8) (origin \"test://stripped\") (export value main) (def main (signature Int (uses)) (trap 1)))"
      )

    assert {:ok, artifact} = Debugging.build(input, mode: :stripped)
    trace = stack(artifact, &apply(&1, :main, []))

    trace = [
      {:foreign_host, :call, ["secret", self()],
       [file: ~c"/private/source", error_info: "secret"]}
      | trace
    ]

    assert {:ok, frames} = Debugging.frames(artifact, input, trace, build: [mode: :stripped])
    assert Enum.all?(frames, &is_nil(&1.origin))
    refute inspect(frames) =~ "secret"
    refute inspect(frames) =~ "/private"
    {:ok, codec} = Catena.Foreign.Codec.new({:data, {:tuple, [:integer]}})

    assert {:ok, [frame]} =
             Debugging.frames(artifact, input, [{:host, :f, [42], []}],
               build: [mode: :stripped],
               values: %{codec: codec, limits: %{nodes: 2, bytes: 8, depth: 1}}
             )

    assert frame.values == {:disclosed, {42}}

    for path <- ["/host/absolute", "../escape", "C:\\host", "x/../escape"] do
      assert {:error, :invalid_debug_path} = Debugging.build(%{input | path: path})
    end
  end

  test "ordinary JSON lowering maps an executed host-misuse failure to its exact expression" do
    document = %{
      "version" => "0.1.6",
      "module" => "DebugOrdinary",
      "exports" => ["add_one"],
      "definitions" => [
        %{
          "name" => "add_one",
          "parameters" => ["value"],
          "signature" => %{
            "forall" => [],
            "type" => %{
              "tag" => "function",
              "parameter" => %{"tag" => "integer"},
              "effect" => [],
              "result" => %{"tag" => "integer"}
            }
          },
          "body" => %{
            "tag" => "binary",
            "operator" => "add",
            "left" => %{"tag" => "variable", "name" => "value"},
            "right" => %{"tag" => "integer", "value" => 1}
          }
        }
      ]
    }

    source = "{\"origin\":\"pkg://é\"," <> String.trim_leading(JSON.encode!(document), "{")
    input = %{kind: :json, source: source, path: "src/ordinary.json"}
    assert {:ok, artifact} = Debugging.build(input)
    trace = stack(artifact, &apply(&1, :add_one, [:deliberate_host_type_violation]))
    assert {:ok, frames} = Debugging.frames(artifact, input, trace)
    frame = Enum.find(frames, &(&1.origin != nil))
    assert frame.origin.primary.locator == "$.definitions[0].body"
    span = frame.origin.primary.span

    assert {:ok, %{"tag" => "binary"}} =
             JSON.decode(binary_part(source, span.byte_start, span.byte_end - span.byte_start))

    assert span.byte_start + 1 > span.column_start
  end

  def host_failure(_, _), do: :erlang.error({:private_native_failure, "secret-value"})

  test "a verified foreign entry maps its boundary while redacting the host failure" do
    source = """
    (module DebugForeign (edition 0.1) (revision 0.1.8) (origin "test://foreign-debug")
      (export value main)
      (effect Host (operation work (params Int) Int))
      (def main (signature Int (uses Host)) (request Host work 1)))
    """

    families = %{"Host" => "test://debug-host"}
    {:ok, core} = Catena.Kernel.CapabilityKernel.check(source, families)
    [slot] = Map.keys(core.capabilities)
    {:ok, codec} = Catena.Foreign.Codec.new({:data, :integer})

    {:ok, declaration} =
      Catena.Foreign.Descriptor.new(
        {__MODULE__, :host_failure},
        [codec],
        codec,
        "test://debug-host",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      )

    input = %{
      kind: :foreign,
      source: source,
      path: "src/foreign.catena",
      families: families,
      entry: "main",
      bindings: %{slot => %{"work" => declaration}}
    }

    assert {:ok, artifact} = Debugging.build(input)
    limits = %{nodes: 20, bytes: 100, depth: 5}

    try do
      Catena.Foreign.Adapter.run([], limits, fn scope ->
        assert {:error, :foreign_authority_denied} =
                 Catena.Foreign.Program.invoke_debug(artifact, input, scope, limits, 1000)
      end)

      Catena.Foreign.Adapter.run([declaration], limits, fn scope ->
        assert {:trap, %{reason: :redacted, frames: frames}} =
                 Catena.Foreign.Program.invoke_debug(artifact, input, scope, limits, 1000)

        frame = Enum.find(frames, &(&1.origin != nil))
        assert frame.origin.primary.span.line_start == 4
        refute inspect(frames) =~ "secret-value"
      end)
    after
      :code.delete(artifact.module)
      :code.purge(artifact.module)
    end
  end

  test "JSON source positions count Unicode scalars and evidence stays outside BEAM" do
    document = %{
      "version" => "0.1.6",
      "module" => "DebugEvidence",
      "origin" => "pkg://tests/é",
      "source" => "secret-host-path",
      "exports" => ["main"],
      "definitions" => [
        %{
          "name" => "main",
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "parameters" => [],
          "body" => %{"tag" => "integer", "value" => 7}
        },
        %{
          "name" => "secret_checker",
          "parameters" => [],
          "verification_only" => true,
          "body" => %{"tag" => "boolean", "value" => true}
        }
      ]
    }

    source = JSON.encode!(document)
    input = %{kind: :json, source: source, path: "src/é.json"}
    assert {:ok, artifact} = Debugging.build(input)
    refute artifact.binary =~ "secret_checker"
    refute artifact.binary =~ "secret-host-path"
    assert map_size(artifact.sidecar.evidence) > 0

    {id, reference} =
      Enum.find(artifact.sidecar.evidence, fn {_, ref} -> ref.locator == "$.definitions[1]" end)

    assert {:ok, ^reference} = Debugging.evidence_link(artifact, input, id)

    changed =
      put_in(document["definitions"], [
        hd(document["definitions"]),
        put_in(Enum.at(document["definitions"], 1)["body"]["value"], false)
      ])

    assert {:ok, second} = Debugging.build(%{input | source: JSON.encode!(changed)})
    assert artifact.binary == second.binary
    refute artifact.sidecar == second.sidecar
    assert {:ok, locations} = Catena.Debugging.JSONLocations.index("{\"é\": \"👩‍💻\", \"value\": 4}")
    span = locations["$.value"]
    assert span.column_start == 23
    assert {:ok, distinct} = Catena.Debugging.JSONLocations.index("{\"a.b\":1,\"a\":{\"b\":2}}")
    assert distinct["$[\"a.b\"]"].byte_start != distinct["$.a.b"].byte_start
    assert {:ok, crlf} = Catena.Debugging.JSONLocations.index("{\r\n\"x\":2}")
    assert crlf["$.x"].line_start == 2
    assert crlf["$.x"].column_start == 5
    assert {:error, :split_source_codepoint} = Catena.SourceSpan.from_bytes("é", 1, 2)
  end
end
