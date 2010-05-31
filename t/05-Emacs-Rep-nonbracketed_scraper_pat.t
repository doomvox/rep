# A perl test file, which can be run like so:
#   perl 05-Emacs-Rep-nonbracketed_scraper_pat.t

#                 doom@kzsu.stanford.edu     Sat May 29 17:30:43 2010

use warnings;
use strict;
$|=1;
my $DEBUG = 1;             # TODO set to 0 before ship
use Data::Dumper;

use Test::More;
BEGIN { plan tests => 15 }; # TODO revise test count

use FindBin qw( $Bin );
use lib "$Bin/../lib";

my $module;
BEGIN {
  $module = 'Emacs::Rep';
  use_ok( $module, ':all' );
}

my $SCRAPER_PAT = define_nonbracketed_s_scraper_pat();

{ my $test_name = "Testing the unquote routine";
  my $find = '\/home\/doom';
  dequote( \$find, '/' );
  my $expected = '/home/doom';
  is( $find, $expected, $test_name);

}

{ my $test_name = "Testing nonbracket scraper pat: single line cases";

  my @cases = (
    ['s/\/home\/doom/$HOME/;',  ['/', '/home/doom','$HOME', '']],
    [q{  s/skiffy/scientifiction/i;}, ['/', 'skiffy', 'scientifiction', 'i']],
    ['s/mama/mammarie/;',       ['/', 'mama', 'mammarie', '' ]],
    ['s/buckit/bucket/;',       ['/', 'buckit', 'bucket', '']],
    ['s/ket/kissed/;',          ['/', 'ket','kissed', '']],
    ['s^rind^rump^;',           ['^', 'rind', 'rump', '']],
    ['s%ski%tire%ogie;',        ['%', 'ski', 'tire', 'ogie']],
    ['s/([ua])m/$1mMm/;',       ['/', '([ua])m', '$1mMm', '']],
    ['s{mammarie{tit{;',        []],
    ['s/;/,/; # embed ;',       ['/', ';', ',', '']],

  );

  foreach my $case ( @cases ) {
    my $line = $case->[0];
    my $expected = $case->[1];

    my @found = ();
    if ($line =~ m{ $SCRAPER_PAT }xms ) {
      @found = ($1, $2, $3, $4);
      dequote( \$found[1], $1 );
      dequote( \$found[2], $1 );
    }

    is_deeply(\@found, \@{ $expected }, "$test_name: $line")
      or print Dumper( \@found ), "\n";
  }
}

{ my $test_name = "Testing nonbracket scraper pat: multi line cases";

  my @cases = (
    ["s/doom\ndoom/de doom/;",  ['/', "doom\ndoom",'de doom', '']],
    ["s/de doom/doom\ndoom/;",  ['/', 'de doom', "doom\ndoom", '']],
    ["s/doom\ndoom/DOOM\nDOOM/;",  ['/', "doom\ndoom", "DOOM\nDOOM", '']],
  );

  foreach my $case ( @cases ) {
    my $line = $case->[0];
    my $expected = $case->[1];

    my @found = ();
    if ($line =~ m{ $SCRAPER_PAT }xms ) {
      @found = ($1, $2, $3, $4);
      dequote( \$found[1], $1 );
      dequote( \$found[2], $1 );
    }

    is_deeply(\@found, \@{ $expected }, "$test_name: $line")
      or print Dumper( \@found ), "\n";
  }
}
