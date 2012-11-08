use Module::Build;

Module::Build -> new
(
	module_name    => 'Tree::DAG_Node',
	license        => 'perl',
	dist_abstract  => 'An N-ary tree',
	dist_author    => 'Sean M. Burke <sburke@cpan.org>',
	build_requires =>
	{
		Test::More => 0.94,
		Test::Pod  => 1.44,
	},
	configure_requires =>
	{
		Module::Build => 0.3800,
	},
	requires =>
	{
		strict   => 0,
		warnings => 0,
	},
) -> create_build_script();
