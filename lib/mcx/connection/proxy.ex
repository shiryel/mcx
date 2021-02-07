# Copyright (C) 2021 Shiryel
# You should have received a copy of the GNU Affero General Public License v3.0 along with this program.

defmodule Mcx.Connection.Proxy do
  @moduledoc """
  Proxy connection to another minecraft server

  Tipically used to proxy functions not implemented in this MCX project
  """

  defstruct client_socket: nil,
            client_transport: nil,
            server_socket: nil,
            step_state: nil

  alias :gen_tcp, as: GenTcp
  alias :proc_lib, as: ProcLib
  alias Mcx.Connection.Step

  require Logger

  use GenServer

  @spec start_link(map()) :: GenServer.on_start()
  def start_link(%Mcx.Connection.Protocol{socket: socket, transport: transport}) do
    ProcLib.spawn_link(__MODULE__, :init, [
      %__MODULE__{client_socket: socket, client_transport: transport}
    ])
    |> (&{:ok, &1}).()
  end

  @impl GenServer
  def init(status) do
    Logger.debug("Starting Proxy...")
    ip = Application.get_env(:mcx, :proxy_ip)
    port = Application.get_env(:mcx, :proxy_port)
    {:ok, socket} = GenTcp.connect(ip, port, [:binary, active: true], 3000)
    :gen_server.enter_loop(__MODULE__, [], %{status | server_socket: socket})
  end

  @impl GenServer
  def handle_info(
        {:tcp, _port, response},
        %__MODULE__{
          client_transport: client_transport,
          client_socket: client_socket,
          step_state: step_state
        } = state
      ) do
    Logger.debug("Sending...")
    Step.run(response, step_state)

    client_transport.send(client_socket, response)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, _socket}, state) do
    Logger.debug("Closing Proxy...")
    {:stop, :normal, state}
  end

  @impl GenServer
  def handle_cast(
        {:send, data},
        %__MODULE__{server_socket: socket, step_state: step_state} = state
      ) do
    Logger.debug("Receive...")

    case Step.run(data, step_state) do
      {:proxy, step_state} ->
        GenTcp.send(socket, data)
        {:noreply, %{state | step_state: step_state}}
    end
  end
end
