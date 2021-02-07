# Copyright (C) 2021 Shiryel
# You should have received a copy of the GNU Affero General Public License v3.0 along with this program.

defmodule Mcx.Connection.Protocol do
  @moduledoc """
  Implements the ranch protocol server
  """

  defstruct transport: nil,
            socket: nil,
            server_proxy: nil

  alias :ranch, as: Ranch
  alias :proc_lib, as: ProcLib
  alias Mcx.Connection.Proxy

  use GenServer

  require Logger

  @behaviour :ranch_protocol

  @impl :ranch_protocol
  def start_link(ref, socket, transport, _opts) do
    pid = ProcLib.spawn_link(__MODULE__, :init, [{ref, socket, transport}])
    {:ok, pid}
  end

  @impl GenServer
  def init({ref, socket, transport}) do
    Logger.debug("Starting Connection...")
    status = %__MODULE__{socket: socket, transport: transport}
    {:ok, pid} = Proxy.start_link(status)
    :ok = Ranch.accept_ack(ref)
    :ok = transport.setopts(socket, active: true, nodelay: true, reuseaddr: true)
    :gen_server.enter_loop(__MODULE__, [], %{status | server_proxy: pid})
  end

  @impl GenServer
  def handle_info({:tcp, _socket, data}, %__MODULE__{server_proxy: server_proxy} = state) do
    GenServer.cast(server_proxy, {:send, data})
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, %__MODULE__{transport: transport} = state) do
    Logger.debug("Closing Connection...")
    transport.close(socket)
    {:stop, :normal, state}
  end

  @impl GenServer
  def handle_cast({:send, data}, %__MODULE__{socket: socket, transport: transport} = state) do
    Logger.debug("Sending package")
    transport.send(socket, data)
    {:noreply, state}
  end
end
