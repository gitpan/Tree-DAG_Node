use strict;
use warnings;

use 5.008_001; # For the utf-8 stuff.

use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile
(
	ABSTRACT => 'An N-ary tree',
	AUTHOR   => 'Sean M. Burke <sburke@cpan.org>',
	clean =>
	{
		FILES => 'blib/* Makefile MANIFEST Tree-DAG_Node-*'
	},
	dist =>
	{
		COMPRESS => 'gzip',
		SUFFIX   => 'gz'
	},
	DISTNAME  => 'Tree-DAG_Node',
	LICENSE   => 'artistic_2',
	NAME      => 'Tree::DAG_Node',
	PL_FILES  => {},
	PREREQ_PM =>
	{
		'File::Spec'        => 3.40,
		'File::Temp'        => 0.19,
		'File::Slurp::Tiny' => 0.003,
		'strict'            => 0,
		'Test::More'        => 0.98,
##		'Test::Pod'         => 1.45, # Make it optional. See t/pod.t. # Make it optional. See t/pod.t
		'warnings'          => 0,
	},
	VERSION_FROM => 'lib/Tree/DAG_Node.pm',
);
