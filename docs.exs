[
  main: "readme",
  extras: [
    "README.md"
  ],
  source_url: "https://github.com/shiryel/mcx",
  nest_modules_by_prefix: [
    Mcx.Connection,
    Mcx.Encoder
  ],
  groups_for_modules: [
    Base: [
      Mcx,
      Mcx.Application
    ],
    Connection: [
      Mcx.Connection,
      Mcx.Connection.Protocol,
      Mcx.Connection.Proxy,
      Mcx.Connection.Step
    ],
    Encoder: [
      Mcx.Encoder.LEB128
    ],
    Database: [
      Mcx.Repo,
    ],
    Phoenix: [
      McxWeb,
      McxWeb.Endpoint,
      McxWeb.Router,
      McxWeb.Router.Helpers,
      McxWeb.UserSocket,
      McxWeb.Gettext,
      McxWeb.ErrorView,
      McxWeb.ErrorHelpers
    ],
  ]
]
