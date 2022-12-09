defmodule Aoc8.MixProject do
  use Mix.Project

  def project do
    [
      app: :aoc8,
      version: "0.1.0",
      elixir: "~> 1.14",
      escript: [main_module: Aoc8],
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Aoc8, []},
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:httpoison, "~> 1.8"},
      {:matrex, "~> 0.6"}
    ]
  end
end
