defmodule Aspike.Server.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: Aspike.Server.ProcessorSupervisor},
      {Aspike.Server, 4041}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Aspike.Server.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
