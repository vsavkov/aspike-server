defmodule AspikeServerTest do
  use ExUnit.Case
  doctest AspikeServer

  test "greets the world" do
    assert AspikeServer.hello() == :world
  end
end
