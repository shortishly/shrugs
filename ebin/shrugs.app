{application, 'shrugs', [
	{description, "Secure sHell Remote User Git Server"},
	{vsn, "0.1.0"},
	{modules, ['shrugs','shrugs_app','shrugs_cli','shrugs_config','shrugs_git','shrugs_key_store','shrugs_ssh_daemon','shrugs_statem','shrugs_sup','shrugs_users','shrugs_util']},
	{registered, [shrugs_sup]},
	{applications, [kernel,stdlib,crypto,sasl,ssh,envy,erlexec]},
	{mod, {shrugs_app, []}},
	{env, []}
]}.