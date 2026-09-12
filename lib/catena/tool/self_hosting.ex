defmodule Catena.Tool.SelfHosting do
  @moduledoc "G141 self-hosting preflight and honest blocked milestone assessment."

  alias Catena.CanonicalJCS

  @version "0.1.98"
  @format "catena-self-hosting-preflight"
  @target "BEAM-through-OTP-29-Erlang-Abstract-Format"
  @port_order ~w(canonical-json scc row-and-unification parser checker verifier backend)
  @required_suites ~w(application-corpus compatibility conformance differential packaging)
  @required_evidence ~w(compiler-source stage-one stage-two fixed-point offline-reproducibility rollback-drill)
  @residual_host_services ~w(crypto filesystem otp-abstract-format process)

  def profile do
    %{
      version: @version,
      gate: :g141,
      status: :preflight_ready_bootstrap_absent,
      target: @target,
      stage_zero: :retained_elixir_bootstrap,
      public_source: :held_for_p109,
      compiler_source: :absent,
      bootstrap_directory: :held_until_source_adoption,
      port_order: @port_order,
      required_suites: @required_suites,
      required_evidence: @required_evidence,
      residual_host_services: @residual_host_services,
      comparison: :exact_bytes_or_declared_semantic_oracle,
      wrapper_counts_as_self_hosted: false,
      fixed_point_proves_trustworthy_bootstrap: false,
      rollback: :required_before_completion
    }
  end

  def validate(package) when is_map(package) do
    with true <- package["format"] == @format,
         true <- package["version"] == 1,
         true <- package["language_revision"] == @version,
         true <- package["gate"] == "G141",
         true <- package["status"] == "prepared-blocked",
         true <- package["target"] == @target,
         true <- package["public_source"] == "held-for-p109",
         true <- package["compiler_source"] == nil,
         true <- package["bootstrap_directory"] == "held-until-source-adoption",
         true <- package["claimed_self_hosted"] == false,
         true <- package["unverified_stage_evidence"] == [],
         true <- package["stage_zero"] == retained_stage_zero(),
         true <- package["stage_one"] == nil,
         true <- package["stage_two"] == nil,
         true <- package["fixed_point"] == nil,
         true <- package["rollback_drill"] == nil,
         true <- exact_strings?(package["port_order"], @port_order),
         true <- exact_strings?(package["required_suites"], @required_suites),
         true <- exact_strings?(package["required_evidence"], @required_evidence),
         true <- exact_strings?(package["residual_host_services"], @residual_host_services) do
      :ok
    else
      _ -> {:error, :invalid_self_hosting_preflight}
    end
  rescue
    _ -> {:error, :invalid_self_hosting_preflight}
  end

  def validate(_), do: {:error, :invalid_self_hosting_preflight}

  def assess(package) do
    with :ok <- validate(package) do
      {:ok,
       %{
         gate: "G141",
         status: :blocked,
         blockers: [
           :public_source_held,
           :compiler_source_absent,
           :stage_one_absent,
           :stage_two_absent,
           :fixed_point_absent,
           :dual_implementation_suites_absent,
           :offline_reproducibility_absent,
           :rollback_drill_absent
         ],
         package_digest: CanonicalJCS.digest(package),
         self_hosted_claim_admitted: false
       }}
    end
  end

  defp retained_stage_zero do
    %{
      "implementation" => "Elixir-bootstrap",
      "status" => "retained-recovery-root",
      "target" => @target,
      "trust" => "audited-source-toolchain-and-host"
    }
  end

  defp exact_strings?(actual, expected),
    do: is_list(actual) and actual == Enum.uniq(actual) and actual == expected
end
