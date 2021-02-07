defmodule Mcx.Repo do
  use Ecto.Repo,
    otp_app: :mcx,
    adapter: Ecto.Adapters.Postgres
end
