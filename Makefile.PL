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
	LICENSE   => 'artistic_2',
	PL_FILES  => {},
	PREREQ_PM =>
	{
		strict     => 0,
		Test::More => 0.98,
##		Test::Pod  => 1.45, # Make it optional. See t/pod.t. # Make it optional. See t/pod.t
		warnings   => 0,
	},
	VERSION_FROM => 'lib/Tree/DAG_Node.pm',
);
