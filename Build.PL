use Module::Build;

Module::Build -> new
(
	module_name    => 'Tree::DAG_Node',
	license        => 'artistic_2',
	dist_abstract  => 'An N-ary tree',
	dist_author    => 'Sean M. Burke <sburke@cpan.org>',
	build_requires =>
	{
		Test::More => 0.98,
#		Test::Pod  => 1.45, # See t/pod.t.
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
