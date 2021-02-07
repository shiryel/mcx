# Copyright (C) 2021 Shiryel
# You should have received a copy of the GNU Affero General Public License v3.0 along with this program.

defmodule Mcx.Connection.Step do
  @moduledoc """
  Minicreaft Server Protocol Steps
  """

  alias Mcx.Encoder.LEB128

  require Logger

  @doc """
  Decodes a package, sending if needs a proxy to finish the request or can already send back the result... or do both
  """
  def run(data, state) do
    {_lenght, data} = LEB128.decode(data)
    {id, data} = LEB128.decode(data)

    step(id, data, state)
    {:proxy, data}
  end

  defp encode(id, data) do
    lenght =
      String.length(data)
      |> LEB128.encode()

    id = LEB128.encode(id)

    lenght <> id <> data
  end

  # Handshake
  # state: nil
  # id: 0
  # Protocol version | Server Address |   Server Port  | Next State
  #      Varint      |   String(255)  | Unsigned Short | VarInt Enum
  def step(0, data, %{status: 0} = state) do
    {version, data} = LEB128.decode(data)

    {status, _} =
      String.reverse(data)
      |> LEB128.decode()

    Logger.debug("Handshake, version: #{version}, status: #{status}")

    %{state | status: status}
  end

  # Server List
  # state: 1
  # id: 0
  #    -
  #    -
  def step(0, _data, %{status: 1, transport: transport, socket: socket} = state) do
    Logger.debug("Server List")

    resp =
      %{
        version: %{
          name: "1.16.5",
          protocol: 754
        },
        players: %{
          max: 100,
          online: 0,
          sample: [
            %{
              name: "default",
              id: "4566e69f-c907-48ee-8d71-d7ba5aa00d20"
            }
          ]
        },
        description: %{
          text: "Hello world"
        }
      }
      |> Jason.encode!()

    length =
      String.length(resp)
      |> LEB128.encode()

    pack = encode(0, length <> resp)
    transport.send(socket, pack)

    state
  end

  # Ping/Pong
  # state: -
  # id: 1
  #   payload
  #   payload
  def step(1, data, %{transport: transport, socket: socket} = state) do
    pack = encode(1, data)
    transport.send(socket, pack)

    Logger.debug("Ping/Pong")

    %{state | status: 2}
  end

  def step(id, data, state) do
    Logger.warn("Step ID #{id} not implemented with data: #{data}")

    state
  end
end
