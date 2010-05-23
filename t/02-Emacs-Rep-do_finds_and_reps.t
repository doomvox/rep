# A perl test file, which can be run like so:
#   perl 02-Emacs-Rep-do_finds_and_reps.t
#                     doom@kzsu.stanford.edu     2010/05/14 01:41:59

use warnings;
use strict;
$|=1;
my $DEBUG = 1;             # TODO set to 0 before ship
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

{ my $test_name = "Testing do_finds_and_reps";
  my $substitutions = define_substitution_cases( 'first' );

  my $find_reps =
    parse_perl_substitutions( $substitutions );

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
0:622:630:2:stocky
1:632:640:2:square
2:594:597:-7:individual
EXPECTORANT

  is( $report, $expected_report, "Testing flatten_locs" );
}

{
  my $test_name = "Testing revise_locations";

  my $substitutions = define_substitution_cases( 'second' );

  my $find_reps =
    parse_perl_substitutions( $substitutions );

  ($DEBUG) && print Dumper( $find_reps );

  my $text = define_text( 'main_text' );

  my $locs =
        do_finds_and_reps( \$text, $find_reps );

  ($DEBUG) && print "change history: ", Dumper( $locs );
  ($DEBUG) && print "changed text: ", Dumper( $text );

  my $expected = define_expected_locs( 'second' );
  is_deeply( $locs, $expected,
             "Testing do_finds_and_reps: second case" );

  my $expected_text = define_expected_text( 'second' );
  is( $text, $expected_text,
             "Testing do_finds_and_reps: second case" );

  ($DEBUG) && print "substitutions\n: $substitutions\n";

  my $report_1 = flatten_locs( $locs );
  ($DEBUG) && print "report before revise_locations:\n$report_1\n";

  revise_locations( $locs );
#  ($DEBUG) && print "revised locs: ", Dumper( $locs );

  my $report_2 = flatten_locs( $locs );
  ($DEBUG) && print "report before revise_locations:\n$report_2\n";

   my $expected_revlocs = define_expected_locs( 'second_revised' );
   is_deeply( $locs, $expected_revlocs,
              "$test_name: second case" );

}



{
  my $test_name = "Testing revise_locations";

  my $substitutions = define_substitution_cases( 'third' );

  my $find_reps =
    parse_perl_substitutions( $substitutions );

  ($DEBUG) && print Dumper( $find_reps );

  my $text = define_text( 'main_text' );

  my $locs =
        do_finds_and_reps( \$text, $find_reps );

  ($DEBUG) && print "change history: ", Dumper( $locs );
  ($DEBUG) && print "changed text: ", Dumper( $text );

  my $expected = define_expected_locs( 'third' );
  is_deeply( $locs, $expected,
             "Testing do_finds_and_reps: third case" );

  my $expected_text = define_expected_text( 'third' );
  is( $text, $expected_text,
             "Testing do_finds_and_reps: third case" );

  ($DEBUG) && print "substitutions\n: $substitutions\n";

  my $report_1 = flatten_locs( $locs );
  ($DEBUG) && print "report before revise_locations:\n$report_1\n";

  revise_locations( $locs );
#  ($DEBUG) && print "revised locs: ", Dumper( $locs );

  my $report_2 = flatten_locs( $locs );
  ($DEBUG) && print "report before revise_locations:\n$report_2\n";

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
   s/stocky/stockish/
   s/square/squarish/
   s|individual|MAN|
END_S

  my $second_substitutions=<<'END_S2';
   s/cars/bikes/
   s/evening/women's/
   s/midnight/midnightMIDNIGHTmidnight/
   s|MIDNIGHTmidnight| (midnacht!)|
END_S2

# Go for lots of scattered little changes
  my $third_substitutions=<<'END_S3';
   s/cars/bikes/
   s/of/OVER-THERE/
   s/\. /. And it was all DOOMED. /
   s/evening/women's/
   s/cane/vibrator/
   s/\bin/skin/
   s|CHAPTER|Chapped|
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
       [
        [
         622,
         630,
         2,
         'stocky'
        ]
       ],
       [
        [
         632,
         640,
         2,
         'square'
        ]
       ],
       [
        [
         594,
         597,
         -7,
         'individual'
        ]
       ]
      ],
      'second' =>
      [
       [
        [
         244,
         249,
         1,
         'cars'
        ]
       ],
       [
        [
         553,
         560,
         0,
         'evening'
        ]
       ],
       [
        [
         45,
         69,
         16,
         'midnight'
        ]
       ],
       [
        [
         45,
         57,
         -4,
         'MIDNIGHTmidnight'
        ]
       ]
      ],
      'second_revised' =>
      [
          [
            [
              256,
              261,
              1,
              'cars'
            ]
          ],
          [
            [
              565,
              572,
              0,
              'evening'
            ]
          ],
          [
            [
              45,
              65,
              16,
              'midnight'
            ]
          ],
          [
            [
              45,
              57,
              -4,
              'MIDNIGHTmidnight'
            ]
          ]
        ],
      'third' =>
      [],
      'third_revised' =>
      [],

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

