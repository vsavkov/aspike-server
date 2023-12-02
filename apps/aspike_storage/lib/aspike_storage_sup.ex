defmodule Aspike.Storage.Sup do
  @moduledoc false
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      %{
        id: Aspike.Ns.Registry,
        type: :worker,
        start: {Aspike.Ns.Registry, :start_link, [Aspike.Ns.Registry]}
      },
      %{
        id: Aspike.Ns.Sup,
        type: :supervisor,
        start: {Aspike.Ns.Sup, :start_link, []}
      }
    ]
    Supervisor.init(children, strategy: :rest_for_one)
  end
end
