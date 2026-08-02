defmodule Catena.ASTDecoderTest do
  use ExUnit.Case, async: true

  test "rejects unknown AST versions without atomizing input" do
    json =
      JSON.encode!(%{
        "version" => "99",
        "module" => "Example",
        "exports" => [],
        "definitions" => []
      })

    assert {:error, %{id: "T012", path: "$.version"}} = Catena.check_json(json)
  end

  test "rejects unknown expression tags" do
    json =
      JSON.encode!(%{
        "version" => "0.1",
        "module" => "Example",
        "exports" => [],
        "definitions" => [%{"name" => "bad", "body" => %{"tag" => "future"}}]
      })

    assert {:error, %{id: "T012"}} = Catena.check_json(json)
  end
end
