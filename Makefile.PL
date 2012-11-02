use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile
(
	($] ge '5.005') ?
	(
		ABSTRACT => 'An N-ary tree',
		AUTHOR   => 'Sean M. Burke <sburke@cpan.org>',
	) : (),
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
	NAME      => 'Tree::DAG_Node',
	PL_FILES  => {},
	PREREQ_PM =>
	{
		strict     => 1.04,
		Test::More => 0.94,
		Test::Pod  => 1.44,
		warnings   => 1.12,
	},
	VERSION_FROM => 'lib/Tree/DAG_Node.pm',
);
