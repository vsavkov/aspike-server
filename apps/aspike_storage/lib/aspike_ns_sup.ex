defmodule Aspike.Ns.Sup do
  use DynamicSupervisor

  @moduledoc false

  # A simple module attribute that stores the supervisor name
  @name Aspike.Ns.Sup

  def start_link do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: @name)
  end

  def start_ns do
    spec = %{
      id: Aspike.Ns,
      start: {Aspike.Ns, :start_link, []},
      restart: :temporary
    }
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
