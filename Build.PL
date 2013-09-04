use strict;
use warnings;

use Module::Build;

Module::Build -> new
(
	module_name    => 'Tree::DAG_Node',
	license        => 'artistic_2',
	dist_abstract  => 'An N-ary tree',
	dist_author    => 'Sean M. Burke <sburke@cpan.org>',
	build_requires =>
	{
		'Test::More' => 0.98,
#		'Test::Pod'  => 1.45, # Make it optional. See t/pod.t.
	},
	configure_requires =>
	{
		'Module::Build' => 0.3800,
	},
	requires =>
	{
		'File::Temp'   => 0.2301,
		'open'         => 0,
		'Path::Tiny'   => 0.025,
		'perl'         => '5.8.1', # For the utf8 stuff.
		'Perl6::Slurp' => 0.051003,
		'strict'       => 0,
		'warnings'     => 0,
	},
) -> create_build_script();
