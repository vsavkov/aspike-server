defmodule Aspike.Status do
#  see https://stackoverflow.com/questions/33851536/how-do-you-define-constants-in-elixir-modules
  @moduledoc false

  def ok, do: 0
  def err_NAMESPACE_NOT_FOUND, do: 20
  def err_RECORD_NOT_FOUND, do: 2
end
