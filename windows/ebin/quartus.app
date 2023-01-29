{application, 'quartus', [
        {description, "Quartus II 13.1 runner"},
        {vsn, "1.0.0"},
        {modules, [quartus,quartus_app,quartus_sup]},
        {registered, []},
        {applications, [kernel,stdlib]},
        {mod, {quartus_app, []}},
        {env, []}
]}.