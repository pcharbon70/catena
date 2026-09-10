defmodule Catena.DocumentationToolTest do
  use ExUnit.Case, async: false

  alias Catena.Tool.Documentation

  defp interface(module \\ nil) do
    source = File.read!(Path.expand("../fixtures/c002-option.catena.json", __DIR__))

    source =
      if module do
        source
        |> JSON.decode!()
        |> Map.put("module", module)
        |> Map.put("origin", "test://#{module}")
        |> JSON.encode!()
      else
        source
      end

    assert {:ok, core} = Catena.check_json(source)
    core |> Catena.Interface.build() |> Catena.Interface.encode()
  end

  defp raw_interface do
    {:ok, value} = JSON.decode(interface())
    value
  end

  test "verified interface symbols render deterministically with resolved links" do
    raw = raw_interface()
    module = raw["module"]
    type_name = raw["types"] |> hd() |> Map.fetch!("name")
    value_name = raw["values"] |> hd() |> Map.fetch!("name")
    type_id = "#{module}.type.#{type_name}"
    value_id = "#{module}.value.#{value_name}"

    docs = [
      %{target_id: type_id, body: "A public data type."},
      %{target_id: value_id, body: "Returns [[#{type_id}]]."}
    ]

    assert {:ok, graph} = Documentation.build(interface(), docs)
    assert {:ok, first} = Documentation.render(graph)
    assert {:ok, ^first} = Documentation.render(graph)
    assert first =~ "[#{type_id}](##{String.replace(String.downcase(type_id), ".", "-")})"
    assert Enum.any?(graph["nodes"], &(&1["kind"] == "type"))
    assert Enum.any?(graph["nodes"], &(&1["kind"] == "value"))
  end

  test "explicit retained-input doctests run in the bounded runner" do
    raw = raw_interface()
    node = "#{raw["module"]}.value.#{raw["values"] |> hd() |> Map.fetch!("name")}"

    example =
      File.read!(Path.expand("../fixtures/c002-option.catena.json", __DIR__)) |> JSON.decode!()

    envelope = %{
      "subject_digest" => raw["digest"],
      "effects" => [],
      "format" => "json",
      "source" => example,
      "expect" => %{"status" => "ok"}
    }

    body = "Example:\n\n```catena doctest\n#{JSON.encode!(envelope)}\n```"
    assert {:ok, graph} = Documentation.build(interface(), [%{target_id: node, body: body}])
    assert [%{"status" => "pass", "observed_effects" => []}] = graph["doctests"]

    stale = put_in(envelope, ["subject_digest"], String.duplicate("0", 64))
    stale_body = "```catena doctest\n#{JSON.encode!(stale)}\n```"

    assert {:error, :invalid_or_stale_doctest} =
             Documentation.build(interface(), [%{target_id: node, body: stale_body}])
  end

  test "cross-package links require and use a verified dependency interface" do
    raw = raw_interface()
    node = "#{raw["module"]}.value.#{raw["values"] |> hd() |> Map.fetch!("name")}"
    dependency = interface("DocumentedDependency")
    {:ok, dependency_raw} = JSON.decode(dependency)
    target = "DocumentedDependency.type.#{dependency_raw["types"] |> hd() |> Map.fetch!("name")}"
    docs = [%{target_id: node, body: "Uses [[#{target}]]."}]

    assert {:error, :unresolved_documentation_link} = Documentation.build(interface(), docs)
    assert {:ok, graph} = Documentation.build(interface(), docs, dependencies: [dependency])
    assert {:ok, markdown} = Documentation.render(graph)
    assert markdown =~ "DocumentedDependency.md#documenteddependency-type"
  end

  test "hidden targets, raw HTML, unresolved links, and undeclared effects are refused" do
    raw = raw_interface()
    node = "#{raw["module"]}.value.#{raw["values"] |> hd() |> Map.fetch!("name")}"

    assert {:error, :hidden_or_unknown_documentation_target} =
             Documentation.build(interface(), [
               %{target_id: "#{raw["module"]}.value.hidden", body: "hidden"}
             ])

    assert {:error, :active_raw_html_refused} =
             Documentation.build(interface(), [%{target_id: node, body: "<script>x</script>"}])

    assert {:error, :unresolved_documentation_link} =
             Documentation.build(interface(), [
               %{target_id: node, body: "See [[Missing.value.x]]."}
             ])

    envelope = %{
      "subject_digest" => raw["digest"],
      "effects" => ["environment"],
      "format" => "json",
      "source" => %{},
      "expect" => %{"status" => "ok"}
    }

    body = "```catena doctest\n#{JSON.encode!(envelope)}\n```"

    assert {:error, :invalid_or_stale_doctest} =
             Documentation.build(interface(), [%{target_id: node, body: body}])
  end

  test "empty attachments, duplicate anchors, forged interfaces, and unauthorized internal views fail" do
    raw = raw_interface()
    node = "#{raw["module"]}.value.#{raw["values"] |> hd() |> Map.fetch!("name")}"

    assert {:error, :empty_documentation_attachment} =
             Documentation.build(interface(), [%{target_id: node, body: ""}])

    forged = raw |> put_in(["module"], "Forged") |> JSON.encode!()
    assert {:error, %{id: _}} = Documentation.build(forged, [])

    internal = [%{"module" => raw["module"], "kind" => "value", "name" => "private"}]

    assert {:error, :internal_documentation_not_authorized} =
             Documentation.build(interface(), [], internal_symbols: internal)

    colliding = [
      %{"module" => raw["module"], "kind" => "value", "name" => "a-b"},
      %{"module" => raw["module"], "kind" => "value", "name" => "a_b"}
    ]

    assert {:error, :duplicate_or_excessive_documentation_nodes} =
             Documentation.build(interface(), [],
               internal_symbols: colliding,
               authorized_internal: true
             )
  end

  test "the retained documentation slice is versioned and reports its public-source hold" do
    assert Catena.LanguageVersion.introduced(:documentation_tool) == "0.1.92"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("documentation-tool", "0.1.92")
    profile = Catena.ConformanceInfo.document()["documentation_tool"]
    assert profile["public_source_examples"] == "held_for_p109"
    assert profile["environmental_services"] == "denied"
  end
end
