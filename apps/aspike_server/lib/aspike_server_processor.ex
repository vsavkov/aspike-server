defmodule Aspike.Server.Processor do
  @moduledoc false
  require Logger

  def run(socket, buffer) do
    case :gen_tcp.recv(socket, 0) do
      {:error, :closed} ->
        :ok
      {:ok, data} ->
        buffer1 = buffer <> data
        buffer2 = process(buffer1, socket)
        run(socket, buffer2)
    end
  end

  defp process(data, socket) do
    case Aspike.Server.Command.decode(data) do
      :need_more -> data
      {:error, reason} ->
        Logger.error "Aspike.Server.Command.parse: error: #{reason}"
      {:ok, decoded, rest} ->
        response = Aspike.Server.Command.run(decoded, Aspike.Ns.Registry)
        encoded = Aspike.Server.Command.encode(response)
        case :gen_tcp.send(socket, encoded) do
          {:error, :closed} ->
            <<>>
          :ok -> process(rest, socket)
        end
    end
  end
end
