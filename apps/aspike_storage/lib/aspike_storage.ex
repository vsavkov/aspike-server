defmodule Aspike.Storage do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Aspike.Storage.Sup.start_link
  end

end
