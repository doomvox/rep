# A perl test file, which can be run like so:
#   perl 02-Emacs-Rep-do_finds_and_reps.t
#                     doom@kzsu.stanford.edu     2010/05/14 01:41:59

use warnings;
use strict;
$|=1;
my $DEBUG = 1;             # TODO set to 0 before ship
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

{ my $test_name = "Testing do_finds_and_reps";
  my $substitutions = define_substitution_cases( 'first' );

  my $find_reps =
    parse_perl_substitutions( $substitutions );

  ($DEBUG) && print Dumper( $find_reps );

  my $text = define_text( 'first' );

  my $locations_aref =
        do_finds_and_reps( \$text, $find_reps );

  ($DEBUG) && print Dumper( $locations_aref );

  ($DEBUG) && print Dumper( $text );

  my $expected = define_expected_locs( 'first' );
  is_deeply( $locations_aref, $expected,
             "$test_name: first case" );

  my $expected_text = define_expected_text( 'first' );
  is( $text, $expected_text,
             "$test_name: first case" );
}


### end main, into the subs

=item define_substitution_cases

=cut

sub define_substitution_cases {
  my $type = shift;

  my $first_substitutions=<<'END_S';
   s/stocky/stockish/
   s/square/squarish/
   s|individual|MAN|
END_S

  my $cases =
    {
     first => $first_substitutions,
      };

  my $substitutions = $cases->{ $type };
  $substitutions
}


=item define_text

=cut

sub define_text {
  my $type = shift;

  my $first_text=<<'END_S';
     CHAPTER I

     FOOTSTEPS TO CRIME

     IT was midnight. From the brilliance of one of Washington's broad avenues,
the lights of a large embassy building could be seen glowing upon the sidewalks
of the street on which it fronted.
     Parked cars lined the side street. One by one they were moving from their
places, edging to the space in front of the embassy, where departing guests
were ready to leave. An important social event was coming to its close.
     The broad steps of the embassy were plainly lighted. Upon them appeared
two men dressed in evening clothes. One was a tall, gray-haired individual; the
other a stocky, square-faced man who leaned heavily upon a stout cane as he
descended the steps. The two men paused as they reached the sidewalk.
END_S

  my $texts =
    {
     first => $first_text,
      };

  my $substitutions = $texts->{ $type };
  $substitutions
}



=item define_expected_locs

=cut

sub define_expected_locs {
  my $type = shift;

  my $expected =
    { 'first' =>
      [
       [
        [
         622,
         630,
         2
        ]
       ],
       [
        [
         632,
         640,
         2
        ]
       ],
       [
        [
         594,
         597,
         -7
        ]
       ]
      ];

  my $ret = $expected->{ $type };
  return $ret;
}




=item define_expected_text

=cut

sub define_expected_text {
  my $type = shift;

  my $expected =
    { 'first' =>
'     CHAPTER I

     FOOTSTEPS TO CRIME

     IT was midnight. From the brilliance of one of Washington\'s broad avenues,
the lights of a large embassy building could be seen glowing upon the sidewalks
of the street on which it fronted.
     Parked cars lined the side street. One by one they were moving from their
places, edging to the space in front of the embassy, where departing guests
were ready to leave. An important social event was coming to its close.
     The broad steps of the embassy were plainly lighted. Upon them appeared
two men dressed in evening clothes. One was a tall, gray-haired MAN; the
other a stockish, squarish-faced man who leaned heavily upon a stout cane as he
descended the steps. The two men paused as they reached the sidewalk.
',
      };


  my $ret = $expected->{ $type };
  return $ret;
}

