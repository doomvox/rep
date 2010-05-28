package Emacs::Rep;
#                                doom@kzsu.stanford.edu
#                                15 May 2010

=head1 NAME

Emacs::Rep - find & replace backend for rep.pl and in-turn rep.el

=head1 SYNOPSIS

  use Emacs::Rep qw( do_finds_and_reps  parse_perl_substitutions );

   my $substitutions =
      [ [ 's/jerk/iconoclast/' ],
        [ 's/conniving/shrewd/' ].
        [ 's/(t)asteless/$1alented/i' ].
      ];

  my $find_replaces_aref =
    parse_perl_substitutions( $substitutions );

  my $locations_aref =
        do_finds_and_reps( \$text, $find_replaces_aref );


=head1 DESCRIPTION

Emacs::Rep is a module that contains one major routine,
and a handful of helper routines.

=head2 EXPORT

None by default.  Any of the following may be requested (or all
with the ':all' tag).

=over

=cut

use 5.008;
use strict;
use warnings;
my $DEBUG = 1;
use Carp;
use Data::Dumper;

require Exporter;

our @ISA = qw(Exporter);
our %EXPORT_TAGS = ( 'all' => [
  qw(
      do_finds_and_reps
      parse_perl_substitutions
      flatten_locs
      revise_locations

    ) ] );
# The above allows declaration	use Emacs::Rep ':all';
# Moving things directly into @EXPORT or (better) @EXPORT_OK saves some memory.
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw(  ); # items to export into callers namespace by default.
                      # (don't use this without a very good reason.)
our $VERSION = '0.01';


=item do_finds_and_reps

Does a series of finds and replaces on some text and
returns the beginning and end points of each of the
modfied regions, along with some other information about
the matches.

Takes two arguments:

(1) A *reference* to the text to be modified.
(2) A series of find and replace pairs in the form
    of an aref of arefs, e.g.

  $find_replaces_aref =
   [ ['jerk',            'iconoclast'],
     ['conniving',       'shrewd'].
     ['(?i)(t)asteless', '$1alented'].
   ]:

Example usage:

$locations_aref =
   do_finds_and_reps( \$text, $find_replaces_aref );

The returned history is an aref of aref of arefs, e.g.

 [
  [ [ 3,       9,   -4,  'alpha'],
    [ 39,     47,   10,  'ralpha'],
    [ 111,   130,    0,  'XXX'],
    [ 320,   332,  -33,  'blvd'],
  ],
  [ [ 12,     23,   6,  'widget'],
    [ 33,     80,   6,  'wadget'],
    [ 453,   532,   6,  'wandat'],
  ],
 ]

Each sub-array here contains the locations of changes made by
each substitution, where each change is recorded as start and
end points in the form of integers specifying the number of
the character counting from the start of the file, where the
first character is 1.

The third integer is the "delta", the change in length of the
string after modification.

The fourth field is the the string that was matched, before
it was modified.

These changed locations are recorded *during* each pass, which
means that later passes can mess up the numbering.  We can
compensate for this later, using the deltas.  See L<revise_locations>.

=cut

sub do_finds_and_reps {
  my $text_ref = shift;
  my $find_replaces = shift; # aref of aref, a series of pairs

  my @locations;
  my $pass = 0;
  foreach my $sub_pair ( @{ $find_replaces } ) {
    my ($find_pat, $replace) = @{ $sub_pair };

    my @pass = ();
    my $delta_sum = 0; # running total of deltas for the pass
    ${ $text_ref } =~
      s{$find_pat}
       {
         my $s = eval "return qq{$replace}";
         # ($DEBUG) && print STDERR "match: $&, 1st: $1, subst: $s\n";
         my $l1 = length( $& );
         my $l2 = length( $s );
         my $delta = $l2 - $l1;
         # pos now points at the beginning of the match
         # using a numbering fixed at the start of the s///ge run
         my $p = pos( ${ $text_ref } ) + 1 + $delta_sum;
         my $beg = $p;
         my $end = $p + $l2;
         push @pass, [$beg, $end, $delta, $&];
         $delta_sum += $delta;
         $s
       }ge;

    push @locations, \@pass;
    $pass++;
  }
  return \@locations; # aref of aref of arefs of pairs
}

=item revise_locations

Example usage (note, revises structure in-place):

  revise_locations( $locs );

Compensates for a problem in the change history recorded by
L<do_finds_and_reps>.

Later passes with another substitution command can move around
the modified strings from previous passes.

This routine does some numerical magic, re-interpreting previous
passes in the light of later ones.

An example of a change history:

 [
  [ [ 3,       9,   -4,  'alpha'],
    [ 39,     47,   10,  'ralpha'],
    [ 111,   130,    0,  'XXX'],
    [ 320,   332,  -33,  'blvd'],
  ],
  [ [ 12,     23,   6,  'widget'],
    [ 33,     80,   6,  'wadget'],
    [ 453,   532,   6,  'wandat'],
  ],
 ]

Given this data, we can see that the first pass needs to be
shifted forward by a delta of 6, acting at the end-point of each
changed region.

So any locations after 23 need to have 6 added to them (and
locations after 80 need another 6 and ones after 532 -- if there
were any -- would need another 6).

=cut

# So: we crunch through the data in reverse order,
# and record accumulated deltas, keyed by the location to apply
# them, i.e. to revise the beg  and end points.
# The size of earlier deltas is untouched, BUT
# the position the earlier deltas act is the revised position.

sub revise_locations {
  my $locs = shift;

  # named array indicies for readability
  my ($BEG, $END, $DELTA, $ORIG ) = 0 .. 3;
  my %delta;
  foreach my $pass ( reverse @{ $locs } ) {
    foreach my $row ( @{ $pass } ) {
      foreach my $spot ( sort {$a <=> $b} keys %delta) {
        if ( $row->[ $BEG ] >= $spot ) {
          $row->[ $BEG ] += $delta{ $spot };
        }
        if ( $row->[ $END ] >= $spot  ) {
          $row->[ $END ] += $delta{ $spot };
        }
      }
    }

    foreach my $row ( @{ $pass } ) {
      { no warnings 'uninitialized';   # TODO trial, giving it it's own for row loop
        $delta{ $row->[ $END ] } += $row->[ $DELTA ];
      }
    }

  }
}

=item flatten_locs

Serialize the locations data structure for emacs comprehension.

There's a need to preserve "pass" numbering to colorize the
changes from each substitution differently.

We pass on the delta info on the theory a use may be found for
it on the other side in emacs land (and it has!).

The result is a block of text, where each line has four
integers separated by colons, in this order:

  <pass>:<beg>:<end>:<delta>:<orig>;

The trailine semi-colon in this format allows it to work on strings
with embedded newlines, and embedded semi-colons as well.
However, an embedded semi-colon *with* a following embedded newline
*must* be backslash escaped.

TODO this isn't done to the "orig" string as of yet.

=cut

sub flatten_locs {
  my $locations = shift;
  my $ret = '';

  my $pass_count = 0;
  foreach my $pass ( @{ $locations } ) {
    foreach my $row ( @{ $pass } ) {
      my ($beg, $end, $delta, $orig) = @{ $row };
      $ret .= sprintf "%d:%d:%d:%d:%s;\n", $pass_count, $beg, $end, $delta, $orig;
    }
    $pass_count++;
   }
  return $ret;
}



=item parse_perl_substitutions

Scrape various forms of perl s///, and return the
find_replaces data structure used by L<do_finds_and_args>.

Takes one argument, an aref of "s///" strings.

End of line comments beginning with a "#" are allowed.
(At present, everything after the close of the substitution is
just ignored).

Currently, this does not support the bracket form of
substitution commands, e.g. s{}{};    TODO

Example usage:

my $substitutions =
  [ 's/pointy-haired boss/esteemed leader/',
  ];

my $find_replaces_aref =
  parse_perl_substitutions( $substitutions );

Where the returned data should look like:

   [ ['pointy-haired boss', 'esteemed leader'],
   ]

=cut

sub parse_perl_substitutions {
  my $substitutions = shift;

  # and how do you handle the s{}{} form?
  # What's the Right Way?  (B::* modules? Text::Balanced?)
  # TO START WITH: just limit the forms it can handle,
  # and document.  s/// and maybe s{}{} later.

  my @find_reps;

  # TODO
  # loads $1 with either / or {: use to choose a particular scraper(?)
  my $s_type_pat =
    qr{
        ^
        \s*
        s
        ( [/{] )    # }, cperl-mode confused again
    }x;

# perlop: "any non-alphanumeric, non-whitespace" delimiter may be used:

#  perlop:
#              If the PATTERN is delimited by bracketing quotes,
#              the REPLACEMENT has its own pair of quotes, which
#              may or may not be bracketing quotes, e.g.,
#              ""s(foo)(bar)"" or ""s<foo>/bar/"".
#
#              the four sorts of brackets (round, angle, square,
#              curly) will all nest  ((?))

# Need recursive regexps to support.  Maybe Text::Balanced easier?
# ((actually... Text::Balanced is really painful.))

  my $scraper_pat_1 =
    qr{
       ^
       \s*
       s
       ( [^[:alnum:]\s] ) # allowed separators
#       ( [/|^] )    # allowed separators
       ( (?: [^ \1 \\ ] | \\ \1 | \\ .  | \s ) *? ) # allows quoted seps, see Friedl, "Matching Delimited Text" (TODO "unroll"?)
       \1
       ( (?: [^ \1 \\ ] | \\ \1 | \s ) *? )
       \1
       ( [xmsige]*? ) # note: not a sep. we allow "ge", though they're no-ops
       .*
       $
      }x;
  # Starting with one of Friedl's designs, I've started tweaking it to get it
  # to work.  I can't imagine why I need to add a "\s" to get it to see spaces:
  # The first alt is anything that's not a sep or a backwhack: how does that not do spaces?
  # On the other hand "\\." makes sense to pass a "\b", but then why special handling of
  # "\\ \1"?

#   my $s_ref = [ split '\n', $substitutions ]; # already done in the script (for now)
   my $s_ref = $substitutions;

  foreach my $s ( @{ $s_ref } ) {
    if ( $s =~ m{ $scraper_pat_1 }x ) {
      my $find = $2;
      my $rep  = $3;
      my $raw_mods = $4;

      # The modifiers we care about (screening out spurious g or e)
      my @mods = qw( x m s i );
      my $mods = '';
      foreach my $m (@mods) {
        if ( $raw_mods =~ qr{$m}x ) {
          $mods .= $m;
        }
      }

      # modifiy $find to incorporate the modifiers internally
      $find = "(?$mods)" . $find if $mods;

      push @find_reps, [ $find, $rep ];

    } else {
#      print STDERR "Problem parsing, skipping: $s\n";  # TODO when live, should *croak*.
                                                        # And maybe: make stack of changes atomic.
#      Why is STDERR getting mixed in with return from shell-commmand-to-string?
    }
  }
  \@find_reps;
}







1;

=back

=head1 SEE ALSO

TODO Mention other useful documentation:

  o  related modules:  L<Module::Name>
  o  operating system documentation (such as man pages in UNIX)
  o  any relevant external documentation such as RFCs or standards
  o  discussion forum set up for your module (if you have it)
  o  web site set up for your module (if you have it)

=head1 AUTHOR

Joseph Brenner, E<lt>doom@kzsu.stanford.eduE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Joseph Brenner

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=head1 BUGS

None reported... yet.

=cut
