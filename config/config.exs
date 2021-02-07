# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :mcx,
  ecto_repos: [Mcx.Repo]

# Configures the endpoint
config :mcx, McxWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "vMueO3SDo6w9O/6R9Xl2kmSMonEEC39wa/Eyoe6HBasrj/514IJEs+fpC7QX55GE",
  render_errors: [view: McxWeb.ErrorView, accepts: ~w(json), layout: false],
  pubsub_server: Mcx.PubSub,
  live_view: [signing_salt: "y34KjrI3"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
