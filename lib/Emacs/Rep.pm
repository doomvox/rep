package Emacs::Rep;
#                                doom@kzsu.stanford.edu
#                                15 May 2010

=head1 NAME

Emacs::Rep - find & replace backend for rep.pl and in-turn rep.el

=head1 SYNOPSIS

  use Emacs::Rep qw( do_finds_and_reps  parse_perl_substitutions );

   my $substitutions =>>'END_S';
      s/jerk/iconoclast/
      s/conniving/shrewd/
      s/(t)asteless/$1alented/i
  END_S

  my $find_replaces_aref =
    parse_perl_substitutions( \$substitutions );

  my $locations_aref =
        do_finds_and_reps( \$text, $find_replaces_aref );


=head1 DESCRIPTION

Emacs::Rep is a module that acts as a back-end for the
rep.pl script which in turn is used by the emacs library.
rep.el.

It's purpose is to perform multiple perl substitution
commands (e.g. s///g) on a given file, using emacs to
interactively display and control the changes.

The end user isn't expected to need to use these routines
(or even the rep.pl script) directly.

An application programmer might use these to add support
for some other interactive front-end.

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
use Text::Balanced qw{ extract_bracketed };

require Exporter;

our @ISA = qw(Exporter);
our %EXPORT_TAGS = ( 'all' => [
  qw(
      do_finds_and_reps
      split_perl_substitutions
      parse_perl_substitutions
      flatten_locs
      revise_locations

      strip_brackets
      accumulate_find_reps
      define_nonbracketed_s_scraper_pat
      split_perl_substitutions

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

Serialize the locations data structure into a text form to be
passed to emacs.

The result is a block of text, where each line has four
integers separated by colons, in this order:

  <pass>:<beg>:<end>:<delta>:<orig>;

The fields:

  pass  -- line number of the substitution command that made the change
  beg   -- beginning of the modified string, integer count starting at 1
  end   -- ending of the modified string, integer count starting at 1
  delta -- the change in character length due to the substitution
  orig  -- the original string that was replaced.

The trailine semi-colon in this format allows it to work easily
on strings with embedded newlines, and embedded semi-colons as well.
However, an embedded semi-colon with an immediately following embedded
newline *must* be backslash escaped.  This routine just escapes all
semi-colons in this field.

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

# used by parse_perl_substitutions
sub split_perl_substitutions {
  my $text_ref = shift;

  my $s_ref = [ split '\n', ${ $text_ref } ];

  return $s_ref;
}

## TODO Can I assume that commented lines are stripped before we get here?
## TODO Tried hacking a few multi-line parsers.  Not there yet.


=item define_nonbracketed_s_scraper_pat

This routine returns a scraper pattern that can parse substitutions
of the form s///, and the usual variants, e.g. s###ims.
It works only on the non-bracketed style (e.g. s{}{} must be handled
some other way).

Captures:
  $1  separator character (e.g. '/')
  $2  find pattern
  $3  replace string
  $4  modifiers

=cut

sub define_nonbracketed_s_scraper_pat {

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
       ( [^ \1 \# ]* )

       (.*)        # capture comments, if any

       $
      }xms;

#        \s*
#        ( [xmsige]*? ) # we allow "ge", though g is always on and e is ignored
#        ;?
#        \s*
#        #?
#        .*   # ignore anything following close of s///
#        $

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



}

=item parse_perl_substitutions

Scrape various forms of perl s///, and return the
find_replaces data structure used by L<do_finds_and_args>.

Takes one argument, an aref of "s///" strings.
The bracketed form (e.g. "s{}{}" is also supported),
however the (somewhat obscure) mixed form is not,
(i.e. "s//{}" won't work).

TODO check if still true:
  End of line comments beginning with a "#" are allowed.
  (At present, everything after the close of the substitution is
  just ignored).

Example usage:

my $substitutions =>>'END_S';
s/pointy-haired boss/esteemed leader/
s/death spiral/minor adjustment/
END_S

my $find_replaces_aref =
  parse_perl_substitutions( \$substitutions );

Where the returned data should look like:

   [ ['pointy-haired boss', 'esteemed leader'],
     ['death spiral',       'minor adjustment'],
   ]

=cut

sub parse_perl_substitutions {
  my $reps_text_ref = shift;
  my $substitutions = split_perl_substitutions( $reps_text_ref );

  my @find_reps;

  # checks for bracketed style of substitutions (loads $1 with opening bracket)
  my $bracketed_form =
    qr{
        ^
        \s*
        s                   # skipping the "s"
        (                   # capture remainder to $1
          (?: [({[<] )      # any open bracket                        # )}]>
        .*?
        )
      ;?                    # skip any trailing semi-colon
      \s*
      $
    }xms;

  my $scraper_pat_1 = define_nonbracketed_s_scraper_pat();

  my $comment_pat = qr{ ^ \s*? \# }xms;

 LINE:
  foreach my $s ( @{ $substitutions } ) {
    if ($s =~ m{ $comment_pat }xms ) {
      next LINE;
    } elsif ( $s =~ m{ $bracketed_form }xms ) {
      my $remainder = $1; # with leading s removed
      my ($find, $rep, $raw_mods);
      my $delim = '(){}[]<>';
      ($find, $remainder) = extract_bracketed( $remainder, $delim );
      ($rep, $remainder)  = extract_bracketed( $remainder, $delim );
      $raw_mods = $remainder;

      strip_brackets( \$find );
      strip_brackets( \$rep );

      accumulate_find_reps( \@find_reps, $find, $rep, $raw_mods );

    } elsif ( $s =~ m{ $scraper_pat_1 }x ) {
      my ($find, $rep, $raw_mods);
      $find     = $2;
      $rep      = $3;
      $raw_mods = $4;

      accumulate_find_reps( \@find_reps, $find, $rep, $raw_mods );

    } else {
      croak "Problem parsing: $s";
      # TODO Make sure the stack of changes atomic.
      #      revert to original state if there's a problem.
    }
  }
  \@find_reps;
}



=item accumulate_find_reps

Example usage:

 accumulate_find_reps( \@find_reps, $find, $rep, $raw_mods );

=cut

sub accumulate_find_reps {
  my $find_reps_aref = shift;
  my $find           = shift;
  my $rep            = shift;
  my $raw_mods       = shift;

  if ($raw_mods) {
    # The modifiers we care about (screening out spurious g or e or ;)
    my @mods = qw( x m s i );
    my $mods = '';
    foreach my $m (@mods) {
      if ( $raw_mods =~ qr{$m}x ) {
        $mods .= $m;
      }
    }
    # modify $find to incorporate the modifiers internally
    $find = "(?$mods)" . $find if $mods;
  }

  push @{ $find_reps_aref }, [ $find, $rep ];
}



=item strip_brackets

Example usage:

 if( strip_brackets( \$string ) ) {
    print "brackets removed, see: $string\n";
 }

=cut

sub strip_brackets {
  my $string_ref = shift;
  my $open_bracket_pat = qr{ ( [\(\{\[\<] ) }xms;                        # )}]>

  my $status;
  if( defined( $string_ref ) ) {
    my $bracket;
    if( ${ $string_ref } =~ s{ ^ \s* $open_bracket_pat \s* }{}xms ) {
      $bracket = $1;
    } else {
      return; # fail
    }

    my $close;
    if ( $bracket eq '(' ) {
      $close = ')';
    } elsif ( $bracket eq '{' ) {
      $close = '}';
    } elsif ( $bracket eq '[' ) {
      $close = ']';
    } elsif ( $bracket eq '<' ) {
      $close = '>';
    } else {
      carp "Unexpected bracket string capture: $bracket";
      return;                   # fail
    }

    my $close_bracket_pat = qr{ \Q$close\E }xms;

    if ( ${ $string_ref } =~ s{  \s*? $close_bracket_pat \s*  $ }{}xms ) {
      $status = 1;
    }
  }
  return $status;
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

  http://obsidianrook.com/rep

The code is available on github:

  http://github.com/doomvox/rep


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
