defmodule Catena.LongTermEvolutionTest do
  use ExUnit.Case, async: true
  alias Catena.Artifact.Migration
  alias Catena.CanonicalJSON

  defp ledger(overrides \\ %{}) do
    interpreters =
      Map.new(
        ~w(0.1.6 0.1.7 0.1.8),
        &{&1, fn doc, _ -> {:ok, {doc["version"], doc["decision"]}} end}
      )

    migrations = %{
      {"0.1.6", "0.1.7"} => fn doc ->
        {:ok, doc |> Map.put("version", "0.1.7") |> Map.put_new("edition", "0.1"), []}
      end,
      {"0.1.7", "0.1.8"} => fn doc ->
        {:ok, doc |> Map.put("version", "0.1.8") |> Map.put_new("kernel", "0.1.8"), []}
      end
    }

    {:ok, value} = Migration.ledger(interpreters, Map.merge(migrations, overrides))
    value
  end

  test "exact historical interpreters reproduce decisions" do
    for version <- ~w(0.1.6 0.1.7 0.1.8) do
      bytes = CanonicalJSON.encode(%{"version" => version, "decision" => "allow"})
      assert {:ok, {^version, "allow"}} = Migration.replay(bytes, ledger())
    end
  end

  test "multi-hop migration retains exact original bytes and provenance" do
    bytes =
      CanonicalJSON.encode(%{"version" => "0.1.6", "decision" => "deny", "signatures" => ["old"]})

    assert {:ok, derived} = Migration.migrate(bytes, "0.1.8", ledger())
    assert :ok = Migration.verify_derived(derived)
    assert Base.decode64!(derived["source_bytes"]) == bytes
    assert derived["document"]["signatures"] == ["old"]
    assert derived["migration_path"] == ["0.1.6->0.1.7", "0.1.7->0.1.8"]
    assert Migration.encode_derived(derived) == Migration.encode_derived(derived)
  end

  test "unknown versions and semantic loss refuse explicitly" do
    assert {:error, {:unsupported_artifact_version, "0.1.99"}} =
             Migration.replay(~s({"version":"0.1.99"}), ledger())

    lossy =
      ledger(%{
        {"0.1.6", "0.1.7"} => fn doc ->
          {:ok, Map.put(doc, "version", "0.1.7"), ["dropped policy"]}
        end
      })

    assert {:error, {:semantic_loss, ["dropped policy"]}} =
             Migration.migrate(~s({"version":"0.1.6"}), "0.1.8", lossy)
  end

  test "revoked roots and unavailable archived inputs have exact outcomes" do
    bytes = ~s({"version":"0.1.6","decision":"allow"})

    assert {:error, :missing_historical_root} =
             Migration.replay(bytes, ledger(), %{root_status: :revoked})

    assert {:ok, _} =
             Migration.replay(bytes, ledger(), %{root_status: :revoked, historical_root: true})

    assert {:error, :missing_archived_tool} =
             Migration.replay(bytes, ledger(), %{tool_available: false})

    assert {:error, :missing_archived_dependency} =
             Migration.replay(bytes, ledger(), %{dependencies_available: false})
  end

  test "profile refuses signature rewriting and unknown future formats" do
    assert %{
             version: "0.1.80",
             original_retained: true,
             signature_rewriting: false,
             unknown_future_formats: :refused
           } = Migration.profile()
  end
end
