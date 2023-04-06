{application, 'shrugs', [
	{description, "Secure sHell Remote User Git Server"},
	{vsn, "0.1.0"},
	{modules, ['shrugs','shrugs_app','shrugs_cli','shrugs_config','shrugs_key_store','shrugs_ssh_daemon','shrugs_statem','shrugs_sup']},
	{registered, [shrugs_sup]},
	{applications, [kernel,stdlib,crypto,sasl,ssh,envy]},
	{mod, {shrugs_app, []}},
	{env, []}
]}.