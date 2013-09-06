use strict;
use warnings;
use warnings  qw(FATAL utf8);    # Fatalize encoding glitches.
use open      qw(:std :utf8);    # Undeclared streams in UTF-8.

use File::Spec;
use File::Temp;

use Perl6::Slurp; # For slurp().

use Test::More;

# ------------------------------------------------

sub process
{
	my($node, $file_name) = @_;

	# The EXLOCK option is for BSD-based systems.

	my($temp_dir)        = File::Temp -> newdir('temp.XXXX', CLEANUP => 1, EXLOCK => 0, TMPDIR => 1);
	my($temp_dir_name)   = $temp_dir -> dirname;
	my($test_file_name)  = File::Spec -> catfile($temp_dir_name, "$file_name.txt");
	my($input_file_name) = File::Spec -> catfile('t', "tree.$file_name.attributes.txt");
	my($root)            = $node -> read_tree($input_file_name);
	my($no_attr)         = $file_name =~ /without/ ? 1 : 0;

	open(OUT, '> :utf8', $test_file_name);
	print OUT "$_\n" for @{$root -> tree2string({no_attributes => $no_attr})};
	close OUT;

	is(slurp("$input_file_name", {utf8 => 1}), slurp("$test_file_name", {utf8 => 1}), "\u$file_name attributes: Output tree matches shipped tree");

} # End of process.

# ------------------------------------------------

BEGIN {use_ok('Tree::DAG_Node'); }

my($node) = Tree::DAG_Node -> new;

isa_ok($node, 'Tree::DAG_Node', 'new() returned correct object type');

for (qw/utf8 with without/)
{
	process($node, $_);
}

done_testing;
