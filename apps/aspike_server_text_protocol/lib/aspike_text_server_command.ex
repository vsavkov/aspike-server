defmodule Aspike.TextServer.Command do
  require Logger
  @doc ~S"""
  Parses the given `line` into a command.

  ## Examples

      iex> Aspike.TextServer.Command.parse "CREATE namespace_gateway\r\n"
      {:ok, {:create, "namespace_gateway"}}

      iex> Aspike.TextServer.Command.parse "CREATE  namespace_gateway  \r\n"
      {:ok, {:create, "namespace_gateway"}}

      iex> Aspike.TextServer.Command.parse "PUT namespace_gateway set_user key_1 bin_1 value_1\r\n"
      {:ok, {:put, "namespace_gateway", "set_user", "key_1", [{"bin_1", "value_1"}]}}

      iex> Aspike.TextServer.Command.parse "GET namespace_gateway set_user key_1\r\n"
      {:ok, {:get, "namespace_gateway", "set_user", "key_1"}}

      iex> Aspike.TextServer.Command.parse "GETFROMSET namespace_gateway set_user\r\n"
      {:ok, {:get_from_set, "namespace_gateway", "set_user"}}

      iex> Aspike.TextServer.Command.parse "GETBYKEY namespace_gateway key_1\r\n"
      {:ok, {:get_by_key, "namespace_gateway", "key_1"}}

      iex> Aspike.TextServer.Command.parse "REMOVE namespace_gateway set_user key_1\r\n"
      {:ok, {:remove, "namespace_gateway", "set_user", "key_1"}}

  Unknown commands or commands with the wrong number of
  arguments return an error:

      iex> Aspike.TextServer.Command.parse "UNKNOWN namespace_gateway key_1\r\n"
      {:error, :unknown_command}

      iex> Aspike.TextServer.Command.parse "GET namespace_gateway\r\n"
      {:error, :unknown_command}

  """
  def parse(line) do
    case String.split(line) do
      ["CREATE", ns] -> {:ok, {:create, ns}}
      ["NAMESPACES"] -> {:ok, {:namespaces}}
      ["SETS", ns] -> {:ok, {:sets, ns}}
      ["GET", ns, set, key] -> {:ok, {:get, ns, set, key}}
      ["GETFROMSET", ns, set] -> {:ok, {:get_from_set, ns, set}}
      ["GETBYKEY", ns, key] -> {:ok, {:get_by_key, ns, key}}
      ["PUT", ns, set, key, bin, value] -> {:ok, {:put, ns, set, key, [{bin, value}]}}
      ["REMOVE", ns, set, key] -> {:ok, {:remove, ns, set, key}}
      _ -> {:error, :unknown_command}
    end
  end

  @doc """
  Runs the given `command` on a server with `pid`.
  """
  def run(command, pid)

  def run({:create, ns}, pid) do
    case Aspike.Ns.Registry.create(pid, ns) do
      :ok -> {:ok, "OK\r\n"}
      :exists_already -> {:error, :exists_already}
    end
  end

  def run({:namespaces}, pid) do
    xs = Aspike.Ns.Registry.list(pid)
#    :io.format("[run] ~p~n", [xs])
    value = mk_string(xs)
    {:ok, "#{value}\r\nOK\r\n"}
  end

  def run({:sets, ns}, pid) do
    lookup pid, ns, fn ns_pid ->
      xs = Aspike.Ns.list(ns_pid)
#      :io.format("[run] ~p~n", [xs])
      value = mk_string(xs)
      {:ok, "#{value}\r\nOK\r\n"}
    end
  end

  def run({:get, ns, set, key}, pid) do
    lookup pid, ns, fn ns_pid ->
      value = case Aspike.Ns.get(ns_pid, set, key) do
        nil -> ""
        xs -> mk_string(xs)
      end
      {:ok, "#{value}\r\nOK\r\n"}
    end
  end

  def run({:get_from_set, ns, set}, pid) do
    lookup pid, ns, fn ns_pid ->
      value = case Aspike.Ns.get_from_set(ns_pid, set) do
        nil -> ""
        xs -> mk_string(xs)
      end
      {:ok, "#{value}\r\nOK\r\n"}
    end
  end

  def run({:get_by_key, ns, key}, pid) do
    lookup pid, ns, fn ns_pid ->
      value = case Aspike.Ns.get_by_key(ns_pid, key) do
        nil -> ""
        xs -> mk_string(xs)
      end
      {:ok, "#{value}\r\nOK\r\n"}
    end
  end

  def run({:put, ns, set, key, [{bin, value}]}, pid) do
    lookup pid, ns, fn ns_pid ->
      Aspike.Ns.put(ns_pid, set, key, [{bin, value}])
      {:ok, "OK\r\n"}
    end
  end

  def run({:remove, ns, set, key}, pid) do
    lookup pid, ns, fn ns_pid ->
      Aspike.Ns.remove(ns_pid, set, key)
      {:ok, "OK\r\n"}
    end
  end

  defp lookup(pid, ns, callback) do
    case Aspike.Ns.Registry.lookup(pid, ns) do
      {:ok, ns_pid} -> callback.(ns_pid)
      :error -> {:error, :not_found}
    end
  end

  defp mk_string(x) when is_binary(x) do
    x
  end

  defp mk_string({x, y}) do
    "{#{x},#{y}}"
  end

  defp mk_string(xs) when is_list(xs) do
    "[" <> Enum.join((for x <- xs, do: mk_string(x)), ",") <> "]"
  end

  defp mk_string(xs) when is_map(xs) do
    "%{" <>
    Enum.join((for {sk, vs} <- xs, do:
                mk_string(sk) <> ":" <> mk_string(vs)),
              ",") <> "}"
  end
end
