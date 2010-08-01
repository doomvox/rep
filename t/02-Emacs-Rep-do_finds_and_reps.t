# A perl test file, which can be run like so:
#   perl 02-Emacs-Rep-do_finds_and_reps.t
#                     doom@kzsu.stanford.edu     2010/05/14 01:41:59

use warnings;
use strict;
$|=1;
my $DEBUG = 0;             # TODO set to 0 before ship
use Data::Dumper::Perltidy;

use Test::More;
BEGIN { plan tests => 12 };

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

# Dropping test of deprecated routine:
#   my $report = serialize_change_metadata( $locs );
#   ($DEBUG) && print "report:\n$report\n";

# my $expected_report=<<"EXPECTORANT";
# 0:622:630:2:stocky;
# 1:632:640:2:square;
# 2:605:608:-7:individual;
# EXPECTORANT

# # was:
# # 2:594:597:-7:individual

#   is( $report, $expected_report, "Testing serialize_change_metadata" );
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

#  my $report_1 = serialize_change_metadata( $locs );
#  ($DEBUG) && print "report before revise_locations:\n$report_1\n";

  revise_locations( $locs );
  ($DEBUG) && print "revised locs: ", Dumper( $locs ), "\n";

#  my $report_2 = serialize_change_metadata( $locs );
#  ($DEBUG) && print "report after revise_locations:\n$report_2\n";

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
#  my $report_1 = serialize_change_metadata( $locs );
#  ($DEBUG) && print "report before revise_locations:\n$report_1\n";

  revise_locations( $locs );
  ($DEBUG) && print "revised locs: ", Dumper( $locs );

#  my $report_2 = serialize_change_metadata( $locs );
#  ($DEBUG) && print "report before revise_locations:\n$report_2\n";

   my $expected_revlocs = define_expected_locs( 'third_revised' );
   is_deeply( $locs, $expected_revlocs,
              "$test_name: third case" );

}

{
  my $test_name = "Testing revise_locations";
  my $case_name = "4th case: string with semi-colons";

  my $substitutions = define_substitution_cases( 'fourth' );
  ($DEBUG) && print "substitutions (fourth)", Dumper( $substitutions ), "\n";;

  my $find_reps =
    parse_perl_substitutions( \$substitutions );
  ($DEBUG) && print "parsed s cmds", Dumper( $find_reps ), "\n";

  my $text = define_text( 'main_text' );

  my $locs =
        do_finds_and_reps( \$text, $find_reps );

  ($DEBUG) && print "change history:", Dumper( $locs );
  ($DEBUG) && print "changed text: ", Dumper( $text );

  my $expected = define_expected_locs( 'fourth' );
  is_deeply( $locs, $expected,
             "Testing do_finds_and_reps: locs $case_name" );

  my $expected_text = define_expected_text( 'fourth' );
  is( $text, $expected_text,
             "Testing do_finds_and_reps: $case_name" );

  ($DEBUG) && print "substitutions\n: $substitutions\n";

  ($DEBUG) && print "fourth locs: ", Dumper( $locs );
#  my $report_1 = serialize_change_metadata( $locs );
#  ($DEBUG) && print "report before revise_locations:\n$report_1\n";

  revise_locations( $locs );
  ($DEBUG) && print "revised locs: ", Dumper( $locs );

#  my $report_2 = serialize_change_metadata( $locs );
#  ($DEBUG) && print "report before revise_locations:\n$report_2\n";

   my $expected_revlocs = define_expected_locs( 'fourth' );
   is_deeply( $locs, $expected_revlocs,
              "$test_name: $case_name" );

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

# munging strings with semi-colons
  my $fourth_substitutions=<<'END_S3';
   s/individual; the/individual!; --The/g;
END_S3


  my $cases =
    {
     first  => $first_substitutions,
     second => $second_substitutions,
     third  => $third_substitutions,
     fourth => $fourth_substitutions,
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
       {
        'beg'  => 629,
        'pass' => 0,
        'post' => ', square-f',
        'pre'  => 'e
other a ',
        'end'   => 637,
        'delta' => 2,
        'orig'  => 'stocky',
        'rep'   => 'stockish'
       },
       {
        'beg'   => 639,
        'pass'  => 1,
        'post'  => '-faced man',
        'pre'   => 'stockish, ',
        'end'   => 647,
        'delta' => 2,
        'orig'  => 'square',
        'rep'   => 'squarish'
       },
       {
        'beg'  => 605,
        'pass' => 2,
        'post' => '; the
othe',
        'pre'   => 'ay-haired ',
        'end'   => 608,
        'delta' => -7,
        'orig'  => 'individual',
        'rep'   => 'MAN'
       }
      ]
      ,
      'second' =>
      [
       {
        'beg'   => 249,
        'pass'  => 0,
        'post'  => ' lined the',
        'pre'   => '   Parked ',
        'end'   => 254,
        'delta' => 1,
        'orig'  => 'cars',
        'rep'   => 'bikes'
       },
       {
        'beg'   => 561,
        'pass'  => 1,
        'post'  => ' clothes. ',
        'pre'   => 'ressed in ',
        'end'   => 568,
        'delta' => 0,
        'orig'  => 'evening',
        'rep'   => 'women\'s'
       },
       {
        'beg'   => 54,
        'pass'  => 2,
        'post'  => '. From the',
        'pre'   => '   IT was ',
        'end'   => 78,
        'delta' => 16,
        'orig'  => 'midnight',
        'rep'   => 'midnightMIDNIGHTmidnight'
       },
       {
        'beg'   => 62,
        'pass'  => 3,
        'post'  => '. From the',
        'pre'   => 's midnight',
        'end'   => 74,
        'delta' => -4,
        'orig'  => 'MIDNIGHTmidnight',
        'rep'   => ' (midnacht!)'
       }
      ],
      'second_revised' =>
      [
       {
        'beg'   => 249,
        'pass'  => 0,
        'post'  => ' lined the',
        'pre'   => '   Parked ',
        'end'   => 254,
        'delta' => 1,
        'orig'  => 'cars',
        'rep'   => 'bikes'
       },
       {
        'beg'   => 561,
        'pass'  => 1,
        'post'  => ' clothes. ',
        'pre'   => 'ressed in ',
        'end'   => 568,
        'delta' => 0,
        'orig'  => 'evening',
        'rep'   => 'women\'s'
       },
       {
        'beg'   => 54,
        'pass'  => 2,
        'post'  => '. From the',
        'pre'   => '   IT was ',
        'end'   => 78,
        'delta' => 16,
        'orig'  => 'midnight',
        'rep'   => 'midnightMIDNIGHTmidnight'
       },
       {
        'beg'   => 62,
        'pass'  => 3,
        'post'  => '. From the',
        'pre'   => 's midnight',
        'end'   => 74,
        'delta' => -4,
        'orig'  => 'MIDNIGHTmidnight',
        'rep'   => ' (midnacht!)'
       }
      ],
      'third' =>
      [
       {
        'beg'   => 249,
        'pass'  => 0,
        'post'  => ' lined the',
        'pre'   => '   Parked ',
        'end'   => 254,
        'delta' => 1,
        'orig'  => 'cars',
        'rep'   => 'bikes'
       },
       {
        'beg'   => 84,
        'pass'  => 1,
        'post'  => ' one of Wa',
        'pre'   => 'rilliance ',
        'end'   => 94,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 99,
        'pass'  => 1,
        'post'  => ' Washingto',
        'pre'   => 'ce of one ',
        'end'   => 109,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 149,
        'pass'  => 1,
        'post'  => ' a large e',
        'pre'   => 'he lights ',
        'end'   => 159,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'  => 226,
        'pass' => 1,
        'post' => ' the stree',
        'pre'  => 'sidewalks
',
        'end'   => 236,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 386,
        'pass'  => 1,
        'post'  => ' the embas',
        'pre'   => ' in front ',
        'end'   => 396,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 526,
        'pass'  => 1,
        'post'  => ' the embas',
        'pre'   => 'oad steps ',
        'end'   => 536,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 62,
        'pass'  => 2,
        'post'  => 'From the b',
        'pre'   => 's midnight',
        'end'   => 87,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 331,
        'pass'  => 2,
        'post'  => 'One by one',
        'pre'   => 'ide street',
        'end'   => 356,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 498,
        'pass'  => 2,
        'post'  => 'An importa',
        'pre'   => 'y to leave',
        'end'   => 523,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 638,
        'pass'  => 2,
        'post'  => 'Upon them ',
        'pre'   => 'ly lighted',
        'end'   => 663,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 716,
        'pass'  => 2,
        'post'  => 'One was a ',
        'pre'   => 'ng clothes',
        'end'   => 741,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 880,
        'pass'  => 2,
        'post'  => 'The two me',
        'pre'   => ' the steps',
        'end'   => 905,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 701,
        'pass'  => 3,
        'post'  => ' clothes. ',
        'pre'   => 'ressed in ',
        'end'   => 708,
        'delta' => 0,
        'orig'  => 'evening',
        'rep'   => 'women\'s'
       },
       {
        'beg'  => 850,
        'pass' => 4,
        'post' => ' as he
des',
        'pre'   => 'n a stout ',
        'end'   => 858,
        'delta' => 4,
        'orig'  => 'cane',
        'rep'   => 'vibrator'
       },
       {
        'beg'   => 423,
        'pass'  => 5,
        'post'  => ' front OVE',
        'pre'   => 'the space ',
        'end'   => 427,
        'delta' => 2,
        'orig'  => 'in',
        'rep'   => 'skin'
       },
       {
        'beg'   => 700,
        'pass'  => 5,
        'post'  => ' women\'s c',
        'pre'   => 'n dressed ',
        'end'   => 704,
        'delta' => 2,
        'orig'  => 'in',
        'rep'   => 'skin'
       },
       {
        'beg'   => 773,
        'pass'  => 5,
        'post'  => 'dividual; ',
        'pre'   => 'ay-haired ',
        'end'   => 777,
        'delta' => 2,
        'orig'  => 'in',
        'rep'   => 'skin'
       },
       {
        'beg'  => 6,
        'pass' => 6,
        'post' => ' I

     F',
        'pre'   => '     ',
        'end'   => 13,
        'delta' => 0,
        'orig'  => 'CHAPTER',
        'rep'   => 'Chapped'
       }
      ],

      'third_revised' =>
      [
       {
        'beg'   => 249,
        'pass'  => 0,
        'post'  => ' lined the',
        'pre'   => '   Parked ',
        'end'   => 254,
        'delta' => 1,
        'orig'  => 'cars',
        'rep'   => 'bikes'
       },
       {
        'beg'   => 84,
        'pass'  => 1,
        'post'  => ' one of Wa',
        'pre'   => 'rilliance ',
        'end'   => 94,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 99,
        'pass'  => 1,
        'post'  => ' Washingto',
        'pre'   => 'ce of one ',
        'end'   => 109,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 149,
        'pass'  => 1,
        'post'  => ' a large e',
        'pre'   => 'he lights ',
        'end'   => 159,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'  => 226,
        'pass' => 1,
        'post' => ' the stree',
        'pre'  => 'sidewalks
',
        'end'   => 236,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 386,
        'pass'  => 1,
        'post'  => ' the embas',
        'pre'   => ' in front ',
        'end'   => 396,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 526,
        'pass'  => 1,
        'post'  => ' the embas',
        'pre'   => 'oad steps ',
        'end'   => 536,
        'delta' => 8,
        'orig'  => 'of',
        'rep'   => 'OVER-THERE'
       },
       {
        'beg'   => 62,
        'pass'  => 2,
        'post'  => 'From the b',
        'pre'   => 's midnight',
        'end'   => 87,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 331,
        'pass'  => 2,
        'post'  => 'One by one',
        'pre'   => 'ide street',
        'end'   => 356,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 498,
        'pass'  => 2,
        'post'  => 'An importa',
        'pre'   => 'y to leave',
        'end'   => 523,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 638,
        'pass'  => 2,
        'post'  => 'Upon them ',
        'pre'   => 'ly lighted',
        'end'   => 663,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 716,
        'pass'  => 2,
        'post'  => 'One was a ',
        'pre'   => 'ng clothes',
        'end'   => 741,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 880,
        'pass'  => 2,
        'post'  => 'The two me',
        'pre'   => ' the steps',
        'end'   => 905,
        'delta' => 23,
        'orig'  => '. ',
        'rep'   => '. And it was all DOOMED. '
       },
       {
        'beg'   => 701,
        'pass'  => 3,
        'post'  => ' clothes. ',
        'pre'   => 'ressed in ',
        'end'   => 708,
        'delta' => 0,
        'orig'  => 'evening',
        'rep'   => 'women\'s'
       },
       {
        'beg'  => 850,
        'pass' => 4,
        'post' => ' as he
des',
        'pre'   => 'n a stout ',
        'end'   => 858,
        'delta' => 4,
        'orig'  => 'cane',
        'rep'   => 'vibrator'
       },
       {
        'beg'   => 423,
        'pass'  => 5,
        'post'  => ' front OVE',
        'pre'   => 'the space ',
        'end'   => 427,
        'delta' => 2,
        'orig'  => 'in',
        'rep'   => 'skin'
       },
       {
        'beg'   => 700,
        'pass'  => 5,
        'post'  => ' women\'s c',
        'pre'   => 'n dressed ',
        'end'   => 704,
        'delta' => 2,
        'orig'  => 'in',
        'rep'   => 'skin'
       },
       {
        'beg'   => 773,
        'pass'  => 5,
        'post'  => 'dividual; ',
        'pre'   => 'ay-haired ',
        'end'   => 777,
        'delta' => 2,
        'orig'  => 'in',
        'rep'   => 'skin'
       },
       {
        'beg'  => 6,
        'pass' => 6,
        'post' => ' I

     F',
        'pre'   => '     ',
        'end'   => 13,
        'delta' => 0,
        'orig'  => 'CHAPTER',
        'rep'   => 'Chapped'
       }
      ],
      'fourth' =>
      [
       {
        'beg'  => 605,
        'pass' => 0,
        'post' => '
other a s',
        'pre'   => 'ay-haired ',
        'end'   => 623,
        'delta' => 3,
        'orig'  => 'individual; the',
        'rep'   => 'individual!; --The'
       }
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
',

      'fourth' => '     CHAPTER I

     FOOTSTEPS TO CRIME

     IT was midnight. From the brilliance of one of Washington\'s broad avenues,
the lights of a large embassy building could be seen glowing upon the sidewalks
of the street on which it fronted.
     Parked cars lined the side street. One by one they were moving from their
places, edging to the space in front of the embassy, where departing guests
were ready to leave. An important social event was coming to its close.
     The broad steps of the embassy were plainly lighted. Upon them appeared
two men dressed in evening clothes. One was a tall, gray-haired individual!; --The
other a stocky, square-faced man who leaned heavily upon a stout cane as he
descended the steps. The two men paused as they reached the sidewalk.
',

      };


  my $ret = $expected->{ $type };
  return $ret;
}

