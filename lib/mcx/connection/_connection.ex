# Copyright (C) 2021 Shiryel
# You should have received a copy of the GNU Affero General Public License v3.0 along with this program.

defmodule Mcx.Connection do
  @moduledoc """
  Connection with minecraft clients

  Uses Ranch as a TCP pooling and a internal proxy for other minecraft servers
  """

  alias :ranch, as: Ranch

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end

  def start_link(_) do
    Ranch.start_listener(
      __MODULE__,
      :ranch_tcp,
      %{socket_opts: [:inet, :inet6, {:port, 25_565}], max_connections: 100},
      __MODULE__.Protocol,
      []
    )
  end
end
