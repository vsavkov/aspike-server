defmodule Aspike.Ns do

  @doc """
  Starts a new namespace.
  """
  def start_link do
    Agent.start_link(fn -> %{} end)
  end

  def list(namespace) do
    keys = Agent.get(namespace, &Map.keys(&1))
    sets = Enum.map(Enum.uniq(Enum.map(keys, &elem(&1, 0))), &to_string(&1))
#    :io.format("[list] ~p~n", [sets])
    sets
  end

  @doc """
  Gets a value from the (`namespace(pid)`, `set`) by `key`.
  """
  def get(namespace, set, key) do
    Agent.get(namespace, &Map.get(&1, {set, key}))
  end

  @doc """
  Gets values from the (`namespace(pid)`, `set`).
  """
  def get_from_set(namespace, set) do
    Agent.get(namespace, &Map.filter(&1, fn {{s,_},_} -> s == set end))
  end

  @doc """
  Gets values from the `namespace(pid)` by `key`.
  """
  def get_by_key(namespace, key) do
    Agent.get(namespace, &Map.filter(&1, fn {{_,k},_} -> k == key end))
  end

  @doc """
  Puts `[{bin, value}]` pairs in the (`namespace(pid)`, `set`).
  """
  def put(namespace, set, key, bin_value_pairs) do
    Agent.update(namespace, &Map.put(&1, {set, key}, bin_value_pairs))
  end

  @doc """
  Removes `key` from (`namespace(pid)`, `set`).

  Returns the current value of `key`, if `key` exists.
  """
  def remove(namespace, set, key) do
    Agent.get_and_update(namespace, &Map.pop(&1, {set, key}))
  end

  @doc """
  Checks if `key` exists in (`namespace(pid)`, `set`).

  Returns `true`, if `key` exists; `false` otherwise.
  """
  def exists(namespace, set, key) do
    Agent.get(namespace, &Map.has_key?(&1, {set, key}))
  end
end
