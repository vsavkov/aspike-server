defmodule Aspike.Server do
  @moduledoc false
  use Task, restart: :transient
  require Logger

  def start_link(arg) do
    Task.start_link(__MODULE__, :accept, [arg])
  end

def accept(port) do
    {:ok, socket} = :gen_tcp.listen(port,
      [:binary,
        packet: :raw,
        active: false,
        reuseaddr: true,
        backlog: 4096])
    Logger.info "Accepting connections on port #{port}"
    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:ok, pid} = Task.Supervisor.start_child(
      Aspike.Server.ProcessorSupervisor,
      Aspike.Server.Processor, :run, [client, <<>>])
    :ok = :gen_tcp.controlling_process(client, pid)
    loop_acceptor(socket)
  end
end
