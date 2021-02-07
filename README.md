# MCX

`Important: MCX is currently a work in progress with many features missing`

MCX is a project that aims to scale the vanilla minecraft servers, because we want more players and less lag on our servers!

How? We want to run multiple vanilla server "proxys" with the same seed to generate map chunks and then keep these chunks on our side using Mnesia (both in memory and in disk) to be able to then send to the players using the scalability from the ErlangVM

## How to start

MCX uses a proxy to another minecraft server, this proxy can be configured seting the `PROXY_IP` and `PROXY_PORT` env variables. I recommend using the official minecraft server, but you can test with others

After setting the proxy server, you can start this project with:
```bash
# Install dependencies
mix deps.get
# Run server
mix phx.server
```

You can visit [`localhost:4000/dashboard`](http://localhost:4000/dashboard) from your browser to see a dashboard with the server informations.

# Legal Stuff

## Software License

    MCX is a project that aims to scale the vanilla minecraft servers with Elixir
    Copyright (C) 2021 Shiryel

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program. If not, see <https://www.gnu.org/licenses/>.
