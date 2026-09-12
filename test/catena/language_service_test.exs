defmodule Catena.LanguageServiceTest do
  use ExUnit.Case, async: true

  alias Catena.CanonicalJCS
  alias Catena.Tool.LanguageService

  test "immutable snapshots expose shared semantic queries and public completion" do
    assert {:ok, snapshot} = LanguageService.open("editor://one", 1, module_json())
    assert snapshot.previous_digest == nil

    assert {:ok, %{"result" => [], "snapshot" => identity}} =
             request(snapshot, "diagnostics")

    assert identity == %{
             "uri" => snapshot.uri,
             "version" => snapshot.version,
             "digest" => snapshot.digest
           }

    assert {:ok, %{"result" => [public]}} = request(snapshot, "completion", %{"prefix" => "a"})
    assert public["name"] == "alpha"
    assert {:ok, %{"result" => all_public}} = request(snapshot, "completion")
    assert Enum.map(all_public, & &1["name"]) == ["alpha"]

    assert {:ok, %{"result" => hover}} =
             request(snapshot, "hover", %{"symbol_id" => public["id"]})

    assert hover["type"]["text"] == ":integer"
    assert hover["visibility"] == "public"

    assert {:ok, %{"result" => definition}} =
             request(snapshot, "definition", %{"symbol_id" => public["id"]})

    assert definition["path"] == "$.definitions[0]"

    assert {:ok, %{"result" => tokens}} = request(snapshot, "semantic_tokens")
    assert tokens["coordinate_space"] == "retained-json-path"
    assert tokens["public_source_coordinates"] == "held-for-p109"
    assert length(tokens["tokens"]) == 2
  end

  test "partial retained inputs have stable snapshot-bound diagnostics" do
    assert {:ok, snapshot} = LanguageService.open("editor://partial", 7, "{")

    assert {:ok, %{"result" => [first]}} = request(snapshot, "diagnostics")
    assert {:ok, %{"result" => [second]}} = request(snapshot, "diagnostics")
    assert first == second
    assert first["compiler_id"] == "T012"
    assert byte_size(first["stable_id"]) == 64

    assert {:error, :language_service_analysis_unavailable} =
             request(snapshot, "completion")
  end

  test "changes require a newer version and stale requests are refused" do
    assert {:ok, old} = LanguageService.open("editor://change", 1, module_json())

    assert {:error, :stale_language_service_change} =
             LanguageService.change(old, 1, module_json())

    assert {:ok, new} = LanguageService.change(old, 2, module_json(2))
    assert new.previous_digest == old.digest
    refute new.digest == old.digest

    stale = envelope(old, "diagnostics") |> Map.put("version", new.version)
    assert {:error, :stale_language_service_request} = LanguageService.request(old, stale)
    assert {:ok, %{"snapshot" => %{"version" => 2}}} = request(new, "diagnostics")
  end

  test "cancellation is request-specific and bounded before dispatch" do
    assert {:ok, snapshot} = LanguageService.open("editor://cancel", 1, module_json())
    assert {:ok, cancelled} = LanguageService.cancel(snapshot, "request-1")

    assert {:ok, %{"id" => "request-1", "status" => "cancelled"}} =
             LanguageService.request(cancelled, envelope(cancelled, "hover", %{}, "request-1"))

    bounded =
      Enum.reduce(2..256, cancelled, fn index, acc ->
        {:ok, next} = LanguageService.cancel(acc, "request-#{index}")
        next
      end)

    assert {:error, :language_service_limit_exceeded} =
             LanguageService.cancel(bounded, "request-257")
  end

  test "identity-bound rename previews apply only to their exact snapshot" do
    assert {:ok, snapshot} = LanguageService.open("editor://rename", 4, module_json())
    assert {:ok, %{"result" => [symbol]}} = request(snapshot, "completion", %{"prefix" => "a"})

    assert {:ok, %{"result" => plan}} =
             request(snapshot, "rename", %{
               "symbol_id" => symbol["id"],
               "new_name" => "renamed"
             })

    assert Enum.map(plan["edits"], & &1["path"]) ==
             ["$.definitions[0].name", "$.exports[0]"]

    assert {:ok, renamed} = LanguageService.apply_rename(snapshot, plan, 5)
    assert renamed.previous_digest == snapshot.digest

    assert {:ok, %{"result" => [renamed_symbol]}} =
             request(renamed, "completion", %{"prefix" => "ren"})

    assert renamed_symbol["name"] == "renamed"

    assert {:error, :invalid_language_service_edit} =
             LanguageService.apply_rename(renamed, plan, 6)

    altered = Map.put(plan, "new_name", "forged")

    assert {:error, :invalid_language_service_edit} =
             LanguageService.apply_rename(snapshot, altered, 5)

    forged_body =
      plan
      |> Map.delete("digest")
      |> Map.put("result", Base.encode64(module_json(99)))
      |> Map.put("result_digest", digest(module_json(99)))

    forged = Map.put(forged_body, "digest", CanonicalJCS.digest(forged_body))

    assert {:error, :invalid_language_service_edit} =
             LanguageService.apply_rename(snapshot, forged, 5)
  end

  test "rename refuses collisions and unresolved top-level occurrences" do
    assert {:ok, snapshot} = LanguageService.open("editor://references", 1, referenced_json())
    assert {:ok, %{"result" => symbols}} = request(snapshot, "completion")
    answer = Enum.find(symbols, &(&1["name"] == "answer"))

    assert {:error, :language_service_rename_conflict} =
             request(snapshot, "rename", %{
               "symbol_id" => answer["id"],
               "new_name" => "use_answer"
             })

    assert {:error, :language_service_rename_requires_resolved_occurrences} =
             request(snapshot, "rename", %{
               "symbol_id" => answer["id"],
               "new_name" => "renamed"
             })
  end

  test "formatting and transport stay behind P109 and limits are published" do
    assert {:ok, snapshot} = LanguageService.open("editor://held", 1, module_json())
    assert {:ok, %{"status" => "held-for-p109"}} = request(snapshot, "formatting")

    oversized = String.duplicate("a", 257)

    assert {:error, :language_service_limit_exceeded} =
             request(snapshot, "completion", %{"prefix" => oversized})

    assert Catena.LanguageVersion.introduced(:language_service) == "0.1.96"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("language-service", "0.1.96")
    profile = Catena.ConformanceInfo.document()["language_service"]
    assert profile["public_source_protocol"] == "held_for_p109"
    assert profile["transport"] == "held_for_p109"
    assert profile["maximum_symbols"] == 4_096
    assert profile["maximum_cancelled_requests"] == 256
  end

  defp request(snapshot, method, params \\ %{}) do
    LanguageService.request(snapshot, envelope(snapshot, method, params))
  end

  defp envelope(snapshot, method, params \\ %{}, id \\ "request-1") do
    %{
      "id" => id,
      "uri" => snapshot.uri,
      "version" => snapshot.version,
      "digest" => snapshot.digest,
      "method" => method,
      "params" => params
    }
  end

  defp module_json(value \\ 1) do
    JSON.encode!(%{
      "version" => "0.1.1",
      "module" => "Editor",
      "source" => "editor://one",
      "exports" => ["alpha"],
      "definitions" => [definition("alpha", value), definition("hidden", 9)]
    })
  end

  defp referenced_json do
    JSON.encode!(%{
      "version" => "0.1.1",
      "module" => "References",
      "source" => "editor://references",
      "exports" => ["answer", "use_answer"],
      "definitions" => [
        definition("answer", 1),
        %{
          "name" => "use_answer",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{"tag" => "variable", "name" => "answer"}
        }
      ]
    })
  end

  defp definition(name, value) do
    %{
      "name" => name,
      "parameters" => [],
      "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
      "body" => %{"tag" => "integer", "value" => value}
    }
  end

  defp digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
