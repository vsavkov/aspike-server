defmodule Aspike.TextServer.CommandTest do
  use ExUnit.Case

  @moduletag :capture_log
  doctest Aspike.TextServer.Command

  setup do
    {:ok, ns_registry_pid} = Aspike.Ns.Registry.start_link(:test_registry)
    {:ok, ns_registry_pid: ns_registry_pid}
  end

  test "module exists" do
    assert is_list(Aspike.TextServer.Command.module_info())
  end

  test "create namespace", %{ns_registry_pid: ns_registry_pid} do
    assert Aspike.TextServer.Command.run({:create, "namespace_1"}, ns_registry_pid)
      == {:ok, "OK\r\n"}
  end

  test "create existing namespace", %{ns_registry_pid: ns_registry_pid} do
    assert Aspike.TextServer.Command.run({:create, "namespace_1"}, ns_registry_pid)
           == {:ok, "OK\r\n"}
    assert Aspike.TextServer.Command.run({:create, "namespace_1"}, ns_registry_pid)
           == {:error, :exists_already}
  end
end
