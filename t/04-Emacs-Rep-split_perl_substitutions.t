# A perl test file, which can be run like so:
#   perl 04-Emacs-Rep-split_perl_substitutions.t
#                 doom@kzsu.stanford.edu     Sat May 29 17:30:43 2010

use warnings;
use strict;
$|=1;
my $DEBUG = 0;             # TODO set to 0 before ship
use Data::Dumper;

use Test::More;
BEGIN { plan tests => 3 }; # TODO revise test count

use FindBin qw( $Bin );
use lib "$Bin/../lib";

my $module;
BEGIN {
  $module = 'Emacs::Rep';
  use_ok( $module, ':all' );
}

{ my $test_name = "Testing split_perl_substitutions: single line cases";

  my $case =<<'END_CASE';
  s/skiffy/scientifiction/i;
s/mama/mammarie/;
s{again}{ TO STOP }ms;   # shottle
s/mammarie/tit/;         # bop
s/buckit/bucket/;
s/ket/kissed/;
# s/rind/rump/;
# s/ski/tire/;
s/folgers/folgers/;
s/([ua])m/$1mMm/;
END_CASE

  my $lines = split_perl_substitutions( \$case );
  ($DEBUG) && print "lines: ", Dumper($lines), "\n";

  my $expected = [
          's/skiffy/scientifiction/i;',
          's/mama/mammarie/;',
          's{again}{ TO STOP }ms;   # shottle',
          's/mammarie/tit/;         # bop',
          's/buckit/bucket/;',
          's/ket/kissed/;',
          's/folgers/folgers/;',
          's/([ua])m/$1mMm/;'
        ];

  is_deeply( $lines, $expected, "$test_name" );
}

{ my $test_name = "Testing split_perl_substitutions: multi-line cases";

  my $case =<<'END_CASE';
s/tie up skiff/tie
up
skiff/i;
s/let
  them
  go
/let them go/;
s{pork rinds}
{pork
  rinds}i;
s{pork
   rhine
  maidens}
{ pork rhine maidens };
s{ one\; two }
 { whun\; tew \; };
END_CASE

  my $lines = split_perl_substitutions( \$case );
  ($DEBUG) && print "lines: ", Dumper($lines), "\n";

  my $expected = [
's/tie up skiff/tie
up
skiff/i;',
's/let
  them
  go
/let them go/;',
's{pork rinds}
{pork
  rinds}i;',
's{pork
   rhine
  maidens}
{ pork rhine maidens };',
's{ one\; two }
 { whun\; tew \; };',
        ];

  is_deeply( $lines, $expected, "$test_name" );
}
