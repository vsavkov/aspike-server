defmodule Aspike.TextServer.Processor do
  @moduledoc false

  def process(socket) do
    msg =
      with {:ok, data} <- read_line(socket),
           {:ok, command} <- Aspike.TextServer.Command.parse(data),
           do: Aspike.TextServer.Command.run(command, Aspike.Ns.Registry)

    write_line(socket, msg)
    process(socket)
  end

  defp read_line(socket) do
    :gen_tcp.recv(socket, 0)
  end

  defp write_line(socket, {:ok, text}) do
    :gen_tcp.send(socket, text)
  end

  defp write_line(socket, {:error, :unknown_command}) do
    # Known error. Write to the client.
    :gen_tcp.send(socket, "UNKNOWN COMMAND\r\n")
  end

  defp write_line(socket, {:error, :exists_already}) do
    # Known error. Write to the client.
    :gen_tcp.send(socket, "EXISTS ALREADY\r\n")
  end

  defp write_line(_socket, {:error, :closed}) do
    # The connection was closed, exit politely.
    exit(:shutdown)
  end

  defp write_line(socket, {:error, :not_found}) do
    :gen_tcp.send(socket, "NOT FOUND\r\n")
  end

  defp write_line(socket, {:error, error}) do
    # Unknown error. Write to the client and exit.
    :gen_tcp.send(socket, "ERROR\r\n")
    exit(error)
  end
end
