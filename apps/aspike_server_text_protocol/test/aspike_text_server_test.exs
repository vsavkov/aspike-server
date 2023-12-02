defmodule Aspike.TextServerTest do
  use ExUnit.Case
  @moduletag :capture_log
  doctest Aspike.TextServer

  setup do
    Application.stop(:aspike_storage)
    :ok = Application.start(:aspike_storage)
  end

  setup do
    opts = [:binary, packet: :line, active: false]
    {:ok, socket} = :gen_tcp.connect('localhost', 4040, opts)
    {:ok, socket: socket}
  end

  test "server interaction", %{socket: socket} do
    assert send_and_recv(socket, "UNKNOWN namespace\r\n") == "UNKNOWN COMMAND\r\n"

    assert send_and_recv(socket, "GET ns_1 set_1 key_1\r\n") == "NOT FOUND\r\n"

    assert send_and_recv(socket, "CREATE ns_1\r\n") == "OK\r\n"

    assert send_and_recv(socket, "PUT ns_1 set_1 key_1 bin_1 value_1\r\n") == "OK\r\n"

    # GET returns two lines
    assert send_and_recv(socket, "GET ns_1 set_1 key_1\r\n") == "[{bin_1,value_1}]\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"

    assert send_and_recv(socket, "REMOVE ns_1 set_1 key_1\r\n") == "OK\r\n"

    # GET returns two lines
    assert send_and_recv(socket, "GET ns_1 set_1 key_1\r\n") == "\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"

    # GETSET tests
    assert send_and_recv(socket, "PUT ns_1 set_1 key_1 bin_1 value_1\r\n") == "OK\r\n"

    # GETFROMSET returns two lines
    assert send_and_recv(socket, "GETFROMSET ns_1 set_1\r\n") ==
             "%{{set_1,key_1}:[{bin_1,value_1}]}\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"

    assert send_and_recv(socket, "PUT ns_1 set_1 key_2 bin_1 value_2\r\n") == "OK\r\n"
    assert send_and_recv(socket, "PUT ns_1 set_1 key_3 bin_3 value_3\r\n") == "OK\r\n"

    assert send_and_recv(socket, "PUT ns_1 set_2 key_2 bin_1 value_2\r\n") == "OK\r\n"
    assert send_and_recv(socket, "PUT ns_1 set_2 key_3 bin_3 value_3\r\n") == "OK\r\n"

    assert send_and_recv(socket, "GETFROMSET ns_1 set_1\r\n") ==
    "%{{set_1,key_1}:[{bin_1,value_1}],{set_1,key_2}:[{bin_1,value_2}],{set_1,key_3}:[{bin_3,value_3}]}\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"

    # GETBYKEY test
    assert send_and_recv(socket, "GETBYKEY ns_1 key_2\r\n") ==
             "%{{set_1,key_2}:[{bin_1,value_2}],{set_2,key_2}:[{bin_1,value_2}]}\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"
  end

  defp send_and_recv(socket, command) do
    :ok = :gen_tcp.send(socket, command)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end
end
