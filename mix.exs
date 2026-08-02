defmodule Catena.MixProject do
  use Mix.Project

  def project do
    [
      app: :catena,
      version: "0.1.0",
      elixir: "~> 1.20",
      start_permanent: Mix.env() == :prod,
      escript: [main_module: Catena.CLI],
      deps: []
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end
end
