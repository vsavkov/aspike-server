defmodule Aspike.Server.Command do
  require Logger

  def decode(data) do
    :aspike_server_protocol.dec_request(data)
  end

  def encode(response) do
    :aspike_server_protocol.enc_response(response)
  end

  # Returns:
  # {login_response, #{?SESSION_TTL => ?TEST_SESSION_TTL, ?SESSION_TOKEN => ?TEST_SESSION_TOKEN1}}
  # {login_response, no_password}
  # {login_response, wrong_password}
  # {login_response, no_user}
  # {login_response, unknown_user}
  def run({:login_request, _params} = request, _pid) do
    :aspike_server_processor.process(request)
  end

  # Returns:
  # {:error, :namespace_not_found}
  # {:put_response, :ok}
  def run({:put_request, _params} = request, pid) do
    {ns_erl, set, key, bvs} = :aspike_server_processor.process(request)
    ns = to_string(ns_erl)
    case lookup(pid, ns,
           fn ns_pid -> Aspike.Ns.put(ns_pid, set, key, bvs) end) do
      {:error, :not_found} -> {:error, :namespace_not_found}
      :ok -> {:put_response, :ok}
    end
  end

  # Returns:
  # {:error, :namespace_not_found}
  # {:error, :record_not_found}
  # {:get_response, [{bin, value}]}
  def run({:get_request, _params} = request, pid) do
    {ns_erl, set, key, _bvs} = :aspike_server_processor.process(request)
    ns = to_string(ns_erl)
    case lookup(pid, ns,
           fn ns_pid -> Aspike.Ns.get(ns_pid, set, key) end) do
      {:error, :not_found} -> {:error, :namespace_not_found}
      nil -> {:error, :record_not_found}
      found -> {:get_response, found}
    end
  end

  # Returns:
  # {:error, :namespace_not_found}
  # {:error, :record_not_found}
  # {:remove_response, :ok}
  def run({:remove_request, _params} = request, pid) do
    {ns_erl, set, key} = :aspike_server_processor.process(request)
    ns = to_string(ns_erl)
    case lookup(pid, ns,
           fn ns_pid -> Aspike.Ns.remove(ns_pid, set, key) end) do
      {:error, :not_found} -> {:error, :namespace_not_found}
      nil -> {:error, :record_not_found}
      _ -> {:remove_response, :ok}
    end
  end

  # Returns:
  # {:error, :namespace_not_found}
  # {:error, :record_not_found}
  # {:exists_response, ok}
  def run({:exists_request, _params} = request, pid) do
    {ns_erl, set, key} = :aspike_server_processor.process(request)
    ns = to_string(ns_erl)
    case lookup(pid, ns,
           fn ns_pid -> Aspike.Ns.exists(ns_pid, set, key) end) do
        {:error, :not_found} -> {:error, :namespace_not_found}
        false -> {:error, :record_not_found}
        true -> {:exists_response, :ok}
    end
  end

  def run(request, _pid) do
    {:unknown_response, request}
  end

  defp lookup(pid, ns, callback) do
    case Aspike.Ns.Registry.lookup(pid, ns) do
      {:ok, ns_pid} -> callback.(ns_pid)
      :error -> {:error, :not_found}
    end
  end
end