# A perl test file, which can be run like so:
#   perl 03-Emacs-Rep-strip_brackets.t
#                     doom@kzsu.stanford.edu     2010/05/14 01:41:59

use warnings;
use strict;
$|=1;
my $DEBUG = 0;             # TODO set to 0 before ship
use Data::Dumper;

use Test::More;
BEGIN { plan tests => 7 }; # TODO revise test count

use FindBin qw( $Bin );
use lib "$Bin/../lib";

my $module;
BEGIN {
  $module = 'Emacs::Rep';
  use_ok( $module, ':all' );
}

{ my $test_name = "Testing strip_brackets on curlies.";

  my $embed  = 'gorgon twist';
  my $case = "{$embed}";
  my $string = $case;
  strip_brackets(\$string);
  ($DEBUG) && print "string: $string\n";

  is( $string, $embed, "$test_name: $case" );
}

{ my $test_name = "Testing strip_brackets on parens.";

  my $embed  = 'gorgon twist';
  my $case = "($embed)";
  my $string = $case;
  strip_brackets(\$string);
  ($DEBUG) && print "string: $string\n";

  is( $string, $embed, "$test_name: $case" );
}

{ my $test_name = "Testing strip_brackets on angles.";

  my $embed  = 'gorgon twist';
  my $case = "<$embed>";
  my $string = $case;
  strip_brackets(\$string);
  ($DEBUG) && print "string: $string\n";

  is( $string, $embed, "$test_name: $case" );
}


{ my $test_name = "Testing strip_brackets on squares.";

  my $embed  = 'gorgon twist';
  my $case = "[$embed]";
  my $string = $case;
  strip_brackets(\$string);
  ($DEBUG) && print "string: $string\n";

  is( $string, $embed, "$test_name: $case" );
}


{ my $test_name = "Testing strip_brackets on spacey curlies.";

  my $embed  = 'gorgon twist';
  my $case = " { $embed } ";
  my $string = $case;
  strip_brackets(\$string);
  ($DEBUG) && print "string: $string\n";

  is( $string, $embed, "$test_name: $case" );
}


{ my $test_name = "Testing strip_brackets on nested curlies.";

  my $embed  = 'Gore {gone (baby)} ... and {beat} twist!';
  my $case = " { $embed } ";
  my $string = $case;
  strip_brackets(\$string);
  ($DEBUG) && print "string: $string\n";

  is( $string, $embed, "$test_name: $case" );
}
