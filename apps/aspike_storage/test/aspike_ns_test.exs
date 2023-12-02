defmodule Aspike.NsTest do
  use ExUnit.Case, async: true

#  alias AspikeNamespace
#
#  @moduletag :capture_log

  doctest Aspike.Ns

  setup do
    {:ok, namespace} = Aspike.Ns.start_link
    {:ok, namespace: namespace}
  end

  test "module exists" do
    assert is_list(Aspike.Ns.module_info())
  end

  test "stores set, key, [{bin1, value1},...,{binN, valueN}] in namespace",
    %{namespace: namespace} do
    assert Aspike.Ns.get(namespace, "set1", <<"key1">>) == nil

    Aspike.Ns.put(namespace, "set1", <<"key1">>, [{"bin1", "value1"}])
    assert Aspike.Ns.get(namespace, "set1", <<"key1">>) == [{"bin1", "value1"}]
  end

  test "retrieves all key-value pairs from set in namespace",
       %{namespace: namespace} do
    assert Aspike.Ns.get_from_set(namespace, "set1") == %{}

    Aspike.Ns.put(namespace, "set1", <<"s1key1">>, [{"s1bin1", "s1value1"}])
    Aspike.Ns.put(namespace, "set1", <<"s1key2">>, [{"s1bin1", "s1value2"}])
    Aspike.Ns.put(namespace, "set1", <<"s1key3">>, [{"s1bin2", "s1value3"}])

    Aspike.Ns.put(namespace, "set2", <<"s2key1">>, [{"s2bin1", "s2value1"}])
    Aspike.Ns.put(namespace, "set2", <<"s2key2">>, [{"s2bin1", "s2value2"}])

    assert Aspike.Ns.get_from_set(namespace, "set1") ==
      %{
        {"set1", <<"s1key1">>} => [{"s1bin1", "s1value1"}],
        {"set1", <<"s1key2">>} => [{"s1bin1", "s1value2"}],
        {"set1", <<"s1key3">>} => [{"s1bin2", "s1value3"}]
      }
  end

  test "retrieves all key-value pairs from namespace by key",
       %{namespace: namespace} do
    assert Aspike.Ns.get_by_key(namespace, "key1") == %{}

    Aspike.Ns.put(namespace, "set1", <<"key1">>, [{"s1bin1", "s1value1"}])
    Aspike.Ns.put(namespace, "set1", <<"key2">>, [{"s1bin1", "s1value2"}])
    Aspike.Ns.put(namespace, "set1", <<"key3">>, [{"s1bin2", "s1value3"}])

    Aspike.Ns.put(namespace, "set2", <<"key1">>, [{"s2bin1", "s2value1"}])
    Aspike.Ns.put(namespace, "set2", <<"key2">>, [{"s2bin1", "s2value2"}])

    assert Aspike.Ns.get_by_key(namespace, "key1") ==
       %{
        {"set1", <<"key1">>} => [{"s1bin1", "s1value1"}],
        {"set2", <<"key1">>} => [{"s2bin1", "s2value1"}]
      }
  end

  test "removes key from set",
       %{namespace: namespace} do
    assert Aspike.Ns.get(namespace, "set1", <<"key1">>) == nil

    Aspike.Ns.put(namespace, "set1", <<"key1">>, [{"bin1", "value1"}])
    assert Aspike.Ns.get(namespace, "set1", <<"key1">>) == [{"bin1", "value1"}]

    Aspike.Ns.remove(namespace, "set1", <<"key1">>)
    assert Aspike.Ns.get(namespace, "set1", <<"key1">>) == nil
  end

  test "key exists",
       %{namespace: namespace} do
    assert Aspike.Ns.exists(namespace, "set1", <<"key1">>) == false

    Aspike.Ns.put(namespace, "set1", <<"key1">>, [{"bin1", "value1"}])
    assert Aspike.Ns.exists(namespace, "set1", <<"key1">>) == true
  end
end
