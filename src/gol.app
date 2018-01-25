{application, gol,
  [{description, "An OTP application"},
    {vsn, "0.1.0"},
    {registered, []},
    {mod, {app, []}},
    {applications,
      [kernel,
        stdlib
      ]},
    {env, []},
    {modules, [sup, universe, register, cell_sup, cell, name_serv]},

    {maintainers, []},
    {licenses, ["Apache 2.0"]},
    {links, []}
  ]}.
