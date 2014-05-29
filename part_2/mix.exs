defmodule ChatTutorial.Mixfile do
  use Mix.Project

  def project do
    [ app: :part_2,
      version: "0.0.1",
      elixir: "~> 0.13.3",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { ChatServer, []},
      registered: [ :chat_server ]
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      { :mochiweb, "2.8.1", git: "https://github.com/mochi/mochiweb"},
      { :rfc4627_jsonrpc, ~r/0.01/, git: "https://github.com/tonyg/erlang-rfc4627"},
      { :erlydtl, git: "https://github.com/derekmcloughlin/erlydtl"},
    ]
  end
end
