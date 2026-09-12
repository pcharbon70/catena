defmodule Catena.SelfHostingTest do
  use ExUnit.Case, async: true

  alias Catena.Tool.SelfHosting

  @package Path.expand("../self-hosting/preflight.json", __DIR__)

  test "the preflight is canonical and reports every absent self-hosting witness" do
    package = package()
    assert :ok = SelfHosting.validate(package)

    assert {:ok,
            %{
              gate: "G141",
              status: :blocked,
              self_hosted_claim_admitted: false,
              package_digest: digest,
              blockers: blockers
            }} = SelfHosting.assess(package)

    assert byte_size(digest) == 64

    assert blockers == [
             :public_source_held,
             :compiler_source_absent,
             :stage_one_absent,
             :stage_two_absent,
             :fixed_point_absent,
             :dual_implementation_suites_absent,
             :offline_reproducibility_absent,
             :rollback_drill_absent
           ]
  end

  test "a wrapper or claimed stage cannot impersonate self-hosting" do
    assert {:error, :invalid_self_hosting_preflight} =
             package() |> Map.put("claimed_self_hosted", true) |> SelfHosting.validate()

    fake_stage = %{"artifact_digest" => String.duplicate("0", 64), "builder" => "Elixir"}

    assert {:error, :invalid_self_hosting_preflight} =
             package() |> Map.put("stage_one", fake_stage) |> SelfHosting.validate()
  end

  test "target drift and hidden residual services are rejected" do
    assert {:error, :invalid_self_hosting_preflight} =
             package() |> Map.put("target", "another-vm") |> SelfHosting.validate()

    assert {:error, :invalid_self_hosting_preflight} =
             package()
             |> Map.update!("residual_host_services", &["elixir-compiler-pass" | &1])
             |> SelfHosting.validate()
  end

  test "port order cannot bypass pure passes or the checked backend" do
    assert {:error, :invalid_self_hosting_preflight} =
             package()
             |> Map.put("port_order", ~w(parser checker backend))
             |> SelfHosting.validate()
  end

  test "fixed-point and rollback evidence cannot be asserted before stages exist" do
    assert {:error, :invalid_self_hosting_preflight} =
             package() |> Map.put("fixed_point", %{"status" => "pass"}) |> SelfHosting.validate()

    assert {:error, :invalid_self_hosting_preflight} =
             package()
             |> Map.put("rollback_drill", %{"status" => "pass"})
             |> SelfHosting.validate()
  end

  test "the lifecycle and profile publish the late-0.x blocked boundary" do
    assert Catena.LanguageVersion.introduced(:self_hosting) == "0.1.98"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("self-hosting", "0.1.98")
    assert Catena.LanguageVersion.latest() == "0.1.98"

    profile = Catena.ConformanceInfo.document()["self_hosting"]
    assert profile["version"] == "0.1.98"
    assert profile["status"] == "preflight_ready_bootstrap_absent"
    assert profile["target"] == "BEAM-through-OTP-29-Erlang-Abstract-Format"
    assert profile["wrapper_counts_as_self_hosted"] == false
    assert profile["fixed_point_proves_trustworthy_bootstrap"] == false
  end

  defp package do
    @package |> File.read!() |> JSON.decode!()
  end
end
