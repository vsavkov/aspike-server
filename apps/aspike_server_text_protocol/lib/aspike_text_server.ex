defmodule Aspike.TextServer do
  @moduledoc false
  use Task, restart: :transient
  require Logger

  def start_link(arg) do
    Task.start_link(__MODULE__, :accept, [arg])
  end

  def accept(port) do
    {:ok, socket} = :gen_tcp.listen(port,
      [:binary, packet: :line, active: false, reuseaddr: true])
    Logger.info "Accepting connections on port #{port}"
    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:ok, pid} = Task.Supervisor.start_child(
      Aspike.TextServer.ProcessorSupervisor,
      Aspike.TextServer.Processor, :process, [client])
    :ok = :gen_tcp.controlling_process(client, pid)
    loop_acceptor(socket)
  end
end
