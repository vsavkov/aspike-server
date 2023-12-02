defmodule Aspike.Ns.Registry do
  use GenServer

  ## Client API

  @doc """
  Starts the namespace registry.
  """
  def start_link(name) do
    GenServer.start_link(__MODULE__, name, name: name)
  end

  @doc """
  Stops the namespace registry.
  """
  def stop(server) do
    GenServer.stop(server)
  end

  @doc """
  Looks up the namespace pid for `namespace` stored in `server`.

  Returns `{:ok, pid}` if the namespace exists, `:error` otherwise.
  """
  def lookup(server, namespace) when is_atom(server) do
    case :ets.lookup(server, namespace) do
      [{^namespace, pid}] -> {:ok, pid}
      [] -> :error
    end
  end

  @doc """
  Ensures there is a namespace associated to the given `namespace` in `server`.
  """
  def create(server, namespace) do
    GenServer.call(server, {:create, namespace})
  end

  def list(server) do
    GenServer.call(server, {:list})
  end

  ## GenServer callbacks

  def init(table) do
    # nss = %{} #  namespace -> pid
    nss = :ets.new(table,
      [:named_table, :protected, read_concurrency: true])
    refs = %{} # ref -> namespace
    {:ok, {nss, refs}}
  end

  def handle_call({:create, ns}, _from, {nss, refs} = state) do
    case lookup(nss, ns) do
      {:ok, _pid} ->
        {:reply, :exists_already, state}
      :error ->
        {:ok, pid} = Aspike.Ns.Sup.start_ns
        ref = Process.monitor(pid)
        refs = Map.put(refs, ref, ns)
        :ets.insert(nss, {ns, pid})
        {:reply, :ok, {nss, refs}}
    end
  end

  def handle_call({:list}, _from, {nss, _refs} = state) do
    xs = :ets.tab2list(nss)
#    :io.format("[List] ~p~n", [x])
    ys = Enum.map(xs, &elem(&1, 0))
#    :io.format("[List] ~p~n", [y])
    {:reply, ys, state}
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, {nss, refs}) do
    {ns, refs} = Map.pop(refs, ref)
    :ets.delete(nss, ns)
    {:noreply, {nss, refs}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
