defmodule Catena.C086SelectiveReceiveTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47 0.1.48)

  @hold_kernel """
  (module C086Hold
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c086/hold")
    (export type Option)
    (export process Holder)
    (export value main)
    (data Option
      (params a)
      (constructor None (fields))
      (constructor Some (fields a)))
    (process Holder
      (mailbox (Option Int))
      (params)
      (receive
        (case (constructor Some (bind message))
          (when (greater (var message) 5))
          (unit))))
    (def main
      (signature Unit (uses Process))
      (let target (spawn Holder)
        (sequence
          (send (var target) (construct Some 0))
          (send (var target) (construct Some 1))))))
  """

  describe "revision registration" do
    @tag obligations: ~w(RC-OBL-001)
    test "0.1.46 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.48"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.46" in LanguageVersion.compilable_revisions()
      refute "0.1.46" in LanguageVersion.artifact_versions()
      refute "0.1.46" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("selective-receive", "0.1.46")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-46-selective-receive")
        )

      assert change["affects"] == ~w(static-meaning)
      assert change["summary"] =~ "selective receive"

      assert String.contains?(
               change["specification"],
               "selective-receive/the-receive-rule-set.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.48"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :public_receive, 0)
      refute function_exported?(Catena, :receive_after, 1)
      refute function_exported?(Catena, :receive_timeout, 1)
    end
  end

  describe "the rule set" do
    @tag obligations: ~w(RC-OBL-002 RC-OBL-004)
    test "preservation and scan order: a rejected prefix stays queued in order" do
      assert {:ok, core} = Catena.check_kernel(@hold_kernel)

      assert {:quiescent, outcome} = Catena.Kernel.Stepper.run(core, "main")

      holder =
        Enum.find(outcome.processes, &(&1.name == :Holder or &1.name == "Holder"))

      assert holder.status == :waiting

      assert Enum.map(holder.mailbox, & &1.value) == [
               {:catena_constructor, :Some, {0}},
               {:catena_constructor, :Some, {1}}
             ]

      root = Enum.find(outcome.processes, &(&1.pid == 0))
      assert root.status == :terminated
    end

    @tag obligations: ~w(RC-OBL-002 RC-OBL-008)
    test "selection removes exactly once: the fixture's launch witness unchanged" do
      source = File.read!("test/fixtures/c010-kernel.catena")

      assert {:ok, core} = Catena.check_kernel(source)
      assert {:ok, :unit, outcome} = Catena.Kernel.Stepper.run(core, "launch")

      assert Enum.filter(outcome.trace, &(&1.label == :send)) |> Enum.map(& &1.message) == [
               {:catena_constructor, :Some, {0}},
               {:catena_constructor, :Some, {1}}
             ]

      assert [%{message: {:catena_constructor, :Some, {1}}}] =
               Enum.filter(outcome.trace, &(&1.label == :receive))

      assert {:ok, :C010Fixture, binary, _} = Catena.compile_kernel(source)
      assert {:module, :C010Fixture} = :code.load_binary(:C010Fixture, ~c"c086.beam", binary)
      assert apply(:C010Fixture, :launch, []) == :unit

      on_exit(fn ->
        :code.purge(:C010Fixture)
        :code.delete(:C010Fixture)
      end)
    end

    @tag obligations: ~w(RC-OBL-003)
    test "CND006: or-pattern expansion and non-closed types reject in the harness" do
      or_clause = %{
        path: nil,
        pattern: %{
          tag: :or,
          alternatives: [
            %{
              tag: :constructor,
              constructor: "Option.Some",
              fields: [],
              field_style: :positional
            },
            %{tag: :constructor, constructor: "Option.None", fields: [], field_style: :positional}
          ]
        },
        guard: nil,
        body: %{tag: :unit}
      }

      assert_raise Catena.TypeError, ~r/receive harness does not admit or-pattern/, fn ->
        Catena.Backend.ErlangAbstract.lower_receive!(holder_core(), [or_clause],
          message_type: {:nominal, "Option", [:integer]}
        )
      end

      assert_raise Catena.TypeError, ~r/one explicit closed message type/, fn ->
        Catena.Backend.ErlangAbstract.lower_receive!(holder_core(), [], message_type: {:var, 0})
      end
    end

    @tag obligations: ~w(RC-OBL-005 RC-OBL-006 RC-OBL-007)
    test "the routed interfaces claim nothing beyond their owners" do
      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :timeout_clause, 0)
      refute function_exported?(Catena, :protocol_state, 1)
      refute function_exported?(Catena, :send_semantics, 0)

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-46-selective-receive")
        )

      assert is_map(change)
    end

    @tag obligations: ~w(RC-OBL-002 RC-OBL-008)
    test "determinism: repeated runs repeat" do
      assert {:ok, core} = Catena.check_kernel(@hold_kernel)

      assert {:quiescent, first} = Catena.Kernel.Stepper.run(core, "main")
      assert {:quiescent, second} = Catena.Kernel.Stepper.run(core, "main")

      mailboxes = fn outcome ->
        Enum.map(outcome.processes, fn process ->
          {process.status, Enum.map(process.mailbox, fn message -> message.value end)}
        end)
      end

      assert mailboxes.(first) == mailboxes.(second)
    end
  end

  defp holder_core do
    program =
      JSON.encode!(%{
        "version" => "0.1.7",
        "edition" => "0.1",
        "language_revision" => "0.1.7",
        "previews" => [],
        "origin" => "test://c086/harness",
        "module" => "C086Harness",
        "source" => "c086.catena.json",
        "exports" => ["main"],
        "type_exports" => [%{"name" => "Option", "visibility" => "transparent"}],
        "type_groups" => [
          %{
            "declarations" => [
              %{
                "name" => "Option",
                "parameters" => [%{"name" => "a", "kind" => "Type"}],
                "constructors" => [
                  %{"name" => "None", "fields" => [], "existentials" => []},
                  %{
                    "name" => "Some",
                    "fields" => [
                      %{"name" => "value", "type" => %{"tag" => "variable", "name" => "a"}}
                    ],
                    "existentials" => []
                  }
                ],
                "derivations" => []
              }
            ]
          }
        ],
        "types" => [],
        "traits" => [],
        "instances" => [],
        "templates" => [],
        "imports" => [],
        "definitions" => [
          %{
            "name" => "main",
            "parameters" => [],
            "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}, "uses" => []},
            "body" => %{"tag" => "integer", "value" => 0}
          }
        ],
        "effects" => [],
        "handlers" => []
      })

    assert {:ok, core} = Catena.check_json(program)
    core
  end
end
