# A perl test file, which can be run like so:
#   perl 02-Emacs-Rep-do_finds_and_reps.t
#                     doom@kzsu.stanford.edu     2010/05/14 01:41:59

use warnings;
use strict;
$|=1;
my $DEBUG = 0;             # TODO set to 0 before ship
use Data::Dumper::Perltidy;

use Test::More;
BEGIN { plan tests => 10 }; # TODO revise test count

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
    parse_perl_substitutions( \$substitutions );

  ($DEBUG) && print Dumper( $find_reps );

  my $text = define_text( 'main_text' );

  my $locs =
        do_finds_and_reps( \$text, $find_reps );

  ($DEBUG) && print Dumper( $locs );

  ($DEBUG) && print Dumper( $text );

  my $expected = define_expected_locs( 'first' );
  is_deeply( $locs, $expected,
             "$test_name: first case" );

  my $expected_text = define_expected_text( 'first' );
  is( $text, $expected_text,
             "$test_name: first case" );

  my $report = flatten_locs( $locs );
  ($DEBUG) && print "report:\n$report\n";

my $expected_report=<<"EXPECTORANT";
0:622:630:2:stocky;
1:632:640:2:square;
2:605:608:-7:individual;
EXPECTORANT

# was:
# 2:594:597:-7:individual

  is( $report, $expected_report, "Testing flatten_locs" );
}

{
  my $test_name = "Testing revise_locations";

  my $substitutions = define_substitution_cases( 'second' );

  my $find_reps =
    parse_perl_substitutions( \$substitutions );

  ($DEBUG) && print Dumper( $find_reps );

  my $text = define_text( 'main_text' );

  my $locs =
        do_finds_and_reps( \$text, $find_reps );

  ($DEBUG) && print "change history: ", Dumper( $locs );
  ($DEBUG) && print "changed text: ", Dumper( $text );

  my $expected = define_expected_locs( 'second' );
  is_deeply( $locs, $expected,
             "Testing do_finds_and_reps locs for second case" );

  my $expected_text = define_expected_text( 'second' );
  is( $text, $expected_text,
             "Testing do_finds_and_reps text for second case" );

  ($DEBUG) && print "substitutions\n: $substitutions\n";

  my $report_1 = flatten_locs( $locs );
  ($DEBUG) && print "report before revise_locations:\n$report_1\n";

  revise_locations( $locs );
  ($DEBUG) && print "revised locs: ", Dumper( $locs ), "\n";

  my $report_2 = flatten_locs( $locs );
  ($DEBUG) && print "report after revise_locations:\n$report_2\n";

   my $expected_revlocs = define_expected_locs( 'second_revised' );

  ($DEBUG) && print "the expected locations:\n" . Dumper( $expected_revlocs ), "\n";
   is_deeply( $locs, $expected_revlocs,
              "$test_name: second case" );

}



{
  my $test_name = "Testing revise_locations";

  my $substitutions = define_substitution_cases( 'third' );
  ($DEBUG) && print "substitutions (third)", Dumper( $substitutions ), "\n";;

  my $find_reps =
    parse_perl_substitutions( \$substitutions );
  ($DEBUG) && print "parsed s cmds", Dumper( $find_reps ), "\n";

  my $text = define_text( 'main_text' );

  my $locs =
        do_finds_and_reps( \$text, $find_reps );

  ($DEBUG) && print "change history:", Dumper( $locs );
  ($DEBUG) && print "changed text: ", Dumper( $text );

  my $expected = define_expected_locs( 'third' );
  is_deeply( $locs, $expected,
             "Testing do_finds_and_reps: locs from third case" );

  my $expected_text = define_expected_text( 'third' );
  is( $text, $expected_text,
             "Testing do_finds_and_reps: modified text third case" );

  ($DEBUG) && print "substitutions\n: $substitutions\n";

  ($DEBUG) && print "third locs: ", Dumper( $locs );
  my $report_1 = flatten_locs( $locs );
#  ($DEBUG) && print "report before revise_locations:\n$report_1\n";

  revise_locations( $locs );
  ($DEBUG) && print "revised locs: ", Dumper( $locs );

  my $report_2 = flatten_locs( $locs );
#  ($DEBUG) && print "report before revise_locations:\n$report_2\n";

   my $expected_revlocs = define_expected_locs( 'third_revised' );
   is_deeply( $locs, $expected_revlocs,
              "$test_name: third case" );

}





### end main, into the subs

=item define_substitution_cases

=cut

sub define_substitution_cases {
  my $type = shift;

  my $first_substitutions=<<'END_S';
   s/stocky/stockish/;
   s/square/squarish/;
   s|individual|MAN|;
END_S

  my $second_substitutions=<<'END_S2';
   s/cars/bikes/;
   s/evening/women's/;
   s/midnight/midnightMIDNIGHTmidnight/;
   s|MIDNIGHTmidnight| (midnacht!)|;
END_S2

# Go for lots of scattered little changes
  my $third_substitutions=<<'END_S3';
   s/cars/bikes/;
   s/of/OVER-THERE/;
   s/\. /. And it was all DOOMED. /;
   s/evening/women's/;
   s/cane/vibrator/;
   s/\bin/skin/;
   s|CHAPTER|Chapped|;
END_S3

  my $cases =
    {
     first  => $first_substitutions,
     second => $second_substitutions,
     third  => $third_substitutions,
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
     main_text => $first_text,
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
        [ [ 622, 630, 2,  'stocky' ] ],
        [ [ 632, 640, 2,  'square' ] ],
        [ [ 605, 608, -7, 'individual' ] ]
    ],
      'second' =>
    [
        [ [ 261, 266, 1,  'cars' ] ],
        [ [ 573, 580, 0,  'evening' ] ],
        [ [ 54,  74,  16, 'midnight' ] ],
        [ [ 62,  74,  -4, 'MIDNIGHTmidnight' ] ]
    ],
      'second_revised' =>
      [
       [ [ 273, 278, 1,  'cars' ] ],
       [ [ 585, 592, 0,  'evening' ] ],
       [ [ 54,  70,  16, 'midnight' ] ],
       [ [ 62,  74,  -4, 'MIDNIGHTmidnight' ] ]
      ],
      'third' =>
[
    [ [ 304, 309, 1, 'cars' ] ],
    [
        [ 84,  117, 8, 'of' ],
        [ 122, 132, 8, 'of' ],
        [ 172, 182, 8, 'of' ],
        [ 249, 259, 8, 'of' ],
        [ 434, 444, 8, 'of' ],
        [ 597, 607, 8, 'of' ]
    ],
    [
        [ 62,  87,  23, '. ' ],
        [ 331, 356, 23, '. ' ],
        [ 500, 525, 23, '. ' ],
        [ 640, 665, 23, '. ' ],
        [ 720, 745, 23, '. ' ],
        [ 890, 915, 23, '. ' ]
    ],
    [ [ 703, 712, 0, 'evening' ] ],
    [ [ 856, 864, 4, 'cane' ] ],
    [ [ 423, 427, 2, 'in' ], [ 700, 704, 2, 'in' ], [ 773, 777, 2, 'in' ] ],
    [ [ 6,   13,  0, 'CHAPTER' ] ]
],


      'third_revised' =>
[
    [ [ 382, 387, 1, 'cars' ] ],
    [
        [ 84,  140, 8, 'of' ],
        [ 145, 155, 8, 'of' ],
        [ 195, 205, 8, 'of' ],
        [ 272, 282, 8, 'of' ],
        [ 482, 492, 8, 'of' ],
        [ 691, 701, 8, 'of' ]
    ],
    [
        [ 62,  87,  23, '. ' ],
        [ 331, 356, 23, '. ' ],
        [ 502, 527, 23, '. ' ],
        [ 642, 667, 23, '. ' ],
        [ 724, 749, 23, '. ' ],
        [ 900, 925, 23, '. ' ]
    ],
    [ [ 707, 716, 0, 'evening' ] ],
    [ [ 862, 870, 4, 'cane' ] ],
    [ [ 423, 427, 2, 'in' ], [ 700, 704, 2, 'in' ], [ 773, 777, 2, 'in' ] ],
    [ [ 6,   13,  0, 'CHAPTER' ] ]
],

    };

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
      'second' => '     CHAPTER I

     FOOTSTEPS TO CRIME

     IT was midnight (midnacht!). From the brilliance of one of Washington\'s broad avenues,
the lights of a large embassy building could be seen glowing upon the sidewalks
of the street on which it fronted.
     Parked bikes lined the side street. One by one they were moving from their
places, edging to the space in front of the embassy, where departing guests
were ready to leave. An important social event was coming to its close.
     The broad steps of the embassy were plainly lighted. Upon them appeared
two men dressed in women\'s clothes. One was a tall, gray-haired individual; the
other a stocky, square-faced man who leaned heavily upon a stout cane as he
descended the steps. The two men paused as they reached the sidewalk.
',

      'third' => '     Chapped I

     FOOTSTEPS TO CRIME

     IT was midnight. And it was all DOOMED. From the brilliance OVER-THERE one OVER-THERE Washington\'s broad avenues,
the lights OVER-THERE a large embassy building could be seen glowing upon the sidewalks
OVER-THERE the street on which it fronted.
     Parked bikes lined the side street. And it was all DOOMED. One by one they were moving from their
places, edging to the space skin front OVER-THERE the embassy, where departing guests
were ready to leave. And it was all DOOMED. An important social event was coming to its close.
     The broad steps OVER-THERE the embassy were plainly lighted. And it was all DOOMED. Upon them appeared
two men dressed skin women\'s clothes. And it was all DOOMED. One was a tall, gray-haired skindividual; the
other a stocky, square-faced man who leaned heavily upon a stout vibrator as he
descended the steps. And it was all DOOMED. The two men paused as they reached the sidewalk.
'

      };


  my $ret = $expected->{ $type };
  return $ret;
}

