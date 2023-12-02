defmodule Aspike.Ns.RegistryTest do
  use ExUnit.Case, async: true

#  alias AspikeNsRegistry
#
#  @moduletag :capture_log

#  doctest Aspike.Ns.Registry

  setup context do
    {:ok, _} = Aspike.Ns.Registry.start_link(context.test)
    {:ok, registry: context.test}
  end

  test "module exists" do
    assert is_list(Aspike.Ns.Registry.module_info())
  end

  test "spawns namespaces", %{registry: registry} do
    assert Aspike.Ns.Registry.lookup(registry, "namespace-1") == :error

    Aspike.Ns.Registry.create(registry, "namespace-1")
    assert {:ok, ns} = Aspike.Ns.Registry.lookup(registry, "namespace-1")
    assert is_pid(ns)

    Aspike.Ns.put(ns, "set-1", "key-1", [{"bin-1", "value-1"}])
    assert Aspike.Ns.get(ns, "set-1", "key-1") == [{"bin-1", "value-1"}]
  end

  test "removes namespaces on exit", %{registry: registry} do
    Aspike.Ns.Registry.create(registry, "namespace-1")
    assert {:ok, ns} = Aspike.Ns.Registry.lookup(registry, "namespace-1")
    assert is_pid(ns)

    Agent.stop(ns)
    # Do a call to ensure the registry processed the DOWN message
    _ = Aspike.Ns.Registry.create(registry, "bogus")
    assert Aspike.Ns.Registry.lookup(registry, "namespace-1") == :error
  end

  test "removes namespace on exit", %{registry: registry} do
    Aspike.Ns.Registry.create(registry, "namespace-1")
    assert {:ok, ns} = Aspike.Ns.Registry.lookup(registry, "namespace-1")
    assert is_pid(ns)

    # Stop the namespace with non-normal reason
    Process.exit(ns, :shutdown)

    # Wait until the namespace is dead
    ref = Process.monitor(ns)
    assert_receive {:DOWN, ^ref, _, _, _}

    # Do a call to ensure the registry processed the DOWN message
    _ = Aspike.Ns.Registry.create(registry, "bogus")
    assert Aspike.Ns.Registry.lookup(registry, "namespace-1") == :error
  end
end
