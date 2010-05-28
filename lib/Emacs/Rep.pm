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
      split_perl_substitutions
      parse_perl_substitutions
      flatten_locs
      revise_locations

    ) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw(  );
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
means that later passes can mess up the numbering.  We then
compensate for this internally, using the recorded deltas.
See L<revise_locations>.

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
  revise_locations( \@locations );
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
# The size of earlier deltas is untouched, but
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
      } # end foreach spot
    } # end foreach row
    foreach my $row ( @{ $pass } ) {
      { no warnings 'uninitialized';
        $delta{ $row->[ $END ] } += $row->[ $DELTA ];
      }
    } # end foreach row
  } # end foreach pass
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
*must* be backslash escaped, so this routine just escapes all
semi-colons.

TODO move this documentation to some place that talks about it as a
data interchange format.

=cut

sub flatten_locs {
  my $locations = shift;
  my $ret = '';

  my $pass_count = 0;
  foreach my $pass ( @{ $locations } ) {
    foreach my $row ( @{ $pass } ) {
      my ($beg, $end, $delta, $orig) = @{ $row };

      $orig =~ s{;}{\\}xmsg;

      $ret .= sprintf "%d:%d:%d:%d:%s;\n",
        $pass_count, $beg, $end, $delta, $orig;
    }
    $pass_count++;
   }
  return $ret;
}




=item split_perl_substitutions

Split the text from the perl substitutions buffer up into
an aref of individual strings, one for each substitution command.

Example usage:

  my $substitutions = split_perl_substitutions( \$substitutions_text );

This routine could *almost* just be replaces with a split on newlines:

   my $s_ref = [ split '\n', $substitutions ];

Except that we'd like to allow for multi-line substitutions.

=cut

# TODO this is a stub that presumes each substitution is on
# one line.  Modify it to allow multi-line s///.
#
# The theory is that this could be beefed up to
# split up any number of s/// commands, ideally
# using something like
#   qr{ s([^:alnum:\s]) .*?  \1 [xmisoge]* ;? }

# Even better though, would be to directly integrate
# this task with parse_perl_substitutions.
# Currently this routine is only called from there.

sub split_perl_substitutions {
  my $text_ref = shift;

  my $s_ref = [ split '\n', ${ $text_ref } ];

  return $s_ref;
}

=item parse_perl_substitutions

Scrape various forms of perl s///, and return the
find_replaces data structure used by L<do_finds_and_args>.

Takes one argument, an aref of "s///" strings.

End of line comments beginning with a "#" are allowed.
(At present, everything after the close of the substitution is
just ignored).

Currently, this does not support the bracket form of
substitution commands, e.g. s{}{};

Example usage:  ### TODO revise this, now it takes text ref, not aref.

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
  my $reps_text_ref = shift;
  my $substitutions = split_perl_substitutions( $reps_text_ref );

  my @find_reps;

  # TODO something like this could be used to choose a scraper
  # depending on whether s/// or s{}{} style is in use.
  # It loads $1 with either / or {
  my $s_type_pat =
    qr{
        ^
        \s*
        s
        ( [/{] )                                     # }
    }x;

  # The following scraper pattern should parse substitutions
  # of the form s///, and the usual variants, e.g. s###ims
  # See notes following the pattern.

  # TODO this is currently pinned using ^$ without the /ms modifiers.
  # but I want to scrape multi-line strings...
  my $scraper_pat_1 =
    qr{
       ^
       \s*
       s
       ( [^[:alnum:]\s(){}<>\[\]] ) # allowed separators:
                                    # not alpha-num or whitespace or brackets
       ( (?: [^ \1 \\ ] | \\ \1 | \\ .  | \s ) *? )
       \1
       ( (?: [^ \1 \\ ] | \\ \1 | \s ) *? )
       \1
       ( [xmsige]*? ) # we allow "ge", though g is always on and e is ignored
       .*   # ignore anything following close of s///
       $
      }x;

  # Notes: we use some of the usual tricks for allowing backslash escapes
  # of the separators, see Friedl, "Matching Delimited Text".
  # I'm not using his "unrolled" form, because I think it's poor for
  # maintainability
  #
  # I don't claim to understand why some of the tweaks above
  # were needed to get it to work:
  #
  # o   The "\s" alternation allows it to see spaces, but the
  #     first alternation is anything that's not a separator or a backwhack:
  #     how does that *not* do spaces?

  #  o  The "\\." makes sense to pass something like a "\b",
  #     but then why is there any need for special handling of
  #     escaped separators: "\\ \1"   (( TODO is there? check again. ))

   my $s_ref = $substitutions;  # aref, one substitution string in each

  my $comment_pat = qr{ ^ \s*? \# }xms;

 LINE:
  foreach my $s ( @{ $s_ref } ) {
    if ($s =~ m{ $comment_pat }xms ) {
      next LINE;
    } elsif ( $s =~ m{ $scraper_pat_1 }x ) {
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
      croak "Problem parsing: $s";
      # TODO Make sure the stack of changes atomic.
      #      revert to original state if there's a problem.
    }
  }
  \@find_reps;
}

1;

=back

=head1 SEE ALSO

This is the back-end for the script rep.pl which in turn
is the back-end for the emacs lisp code rep.el.

If rep.el is not installed, look in the "elisp" sub-directory
of this CPAN package.

A good discussion forum for projects such as this is:

  http://groups.google.com/group/emacs-perl-intersection

Web pages related to this can be found at:

  http://obsidianrook.com/p5s

The code is available on github:

  http://github.com/doomvox/substitutions


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
