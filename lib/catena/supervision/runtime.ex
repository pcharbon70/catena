defmodule Catena.Supervision.Runtime do
  @moduledoc "Narrow OTP supervisor callback for validated lifecycle descriptions."
  @behaviour :supervisor

  def start_link(flags, children), do: :supervisor.start_link(__MODULE__, {flags, children})

  @impl true
  def init({flags, children}), do: {:ok, {flags, children}}
end
