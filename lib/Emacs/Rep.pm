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
use Data::Dumper::Perltidy;
use PPI;

require Exporter;

our @ISA = qw(Exporter);
our %EXPORT_TAGS = ( 'all' => [
  qw(
      do_finds_and_reps
      split_perl_substitutions
      parse_perl_substitutions
      serialize_change_metadata
      revise_locations

      strip_brackets
      accumulate_find_reps
      define_nonbracketed_s_scraper_pat
      split_perl_substitutions
      dequote

    ) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw(  );
our $VERSION = '0.03';  # TODO manually sync-up rep.pl and rep.el versions

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
    [
     [ 219,  229, 6, 'jerk'],
     [ 649,  659, 6, 'jerk'],
     [1978, 1988, 6, 'jerk'],
   ],
   [
     [ 402,  408, -3, 'conniving'],
     [1620, 1626, -3, 'conniving'],
   ],
   [
     [ 110,  118, -1, 'tasteless'],
     [ 494,  502, -1, 'tasteless'],
     [1672, 1680, -1, 'tasteless'],
     [1697, 1705, -1, 'Tasteless'],
   ],
  ]

The sub-arrays are records of changes made by a substitution.
The change meta-data included is, in order: the beginning and
end points, the delta (change in length), and the original
string.

The start and end points are in the form of integers counting
from the start of the file, where the first character is 1.

Note that the change locations are recorded *during* each pass,
which means that later passes throw off the numbering, but we
compensate for this internally using the recorded deltas.
See L<revise_locations>.

=cut

sub do_finds_and_reps {
  my $text_ref      = shift;
  my $find_replaces = shift; # aref of aref: a series of pairs

  my $opts = shift;
  my $LIVE_DANGEROUSLY = $opts->{ LIVE_DANGEROUSLY };

  my $text_copy;
  unless( $LIVE_DANGEROUSLY ) {
    $text_copy = ${ $text_ref };
  }

  my @locations;
  my $pass = 0;
  eval {
    foreach my $sub_pair ( @{ $find_replaces } ) {
      my ($find_pat, $replace) = @{ $sub_pair };

      my @pass = ();
      my $delta_sum = 0; # running total of deltas for the pass

      ${ $text_ref } =~
        s{$find_pat}
         {
           my $s = eval "return qq{$replace}"; # TODO no need for return? # eval qq{ $replace };
#           my $s = eval qq{ $replace }; # but just using this breaks 02-*.t
           # ($DEBUG) && print STDERR "match: $&, 1st: $1, subst: $s\n";
           my $l1 = length( $& );
           my $l2 = length( $s );
           my $delta = $l2 - $l1;
           # in here, pos points at the start of the match, and it uses
           # char numbering fixed at the start of the s///ge run
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
  };
  if ($@) {
    # Send error message to STDOUT so that it won't mess up test output.
    # (and anyway, the elisp call shell-command-to-string merges in STDERR)
    #
    # The elisp function rep-run-perl-substitutions uses prefix "Problem".
    # to spot error messages
    print "Problem: $@\n";
    # roll-back
    @locations = ();
    unless( $LIVE_DANGEROUSLY ) {
      ${ $text_ref } = $text_copy;
    }
  }
  revise_locations( \@locations );
  return \@locations; # aref of aref of arefs of pairs
}

=item revise_locations

Example usage (note, revises structure in-place):

  revise_locations( $locs );

This compensates for a problem in the change history recorded
by L<do_finds_and_reps>.

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
# The size of earlier deltas is untouched, but the position
# the earlier deltas act upon is the revised position.

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

=item serialize_change_metadata

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
on strings with embedded newlines.  Any embedded semi-colons,
will be backslash escaped by this routine.

=cut

sub serialize_change_metadata {
  my $locations = shift;
  my $ret = '';

  my $pass_count = 0;
  foreach my $pass ( @{ $locations } ) {
    foreach my $row ( @{ $pass } ) {
      my ($beg, $end, $delta, $orig) = @{ $row };

      # escape any semicolons
      $orig =~ s{;}{\\;}xmsg;

      $ret .= sprintf "%d:%d:%d:%d:%s;\n",
        $pass_count, $beg, $end, $delta, $orig;
    }
    $pass_count++;
   }
  return $ret;
}

=item parse_perl_substitutions

Scrapes some text looking for various forms of perl substitutions
(i.e. "s///;", "s{}{};", etc.).

Returns the find_replaces data structure used by L<do_finds_and_args>.

Takes one argument, a scalar reference to a block of text
containing multiple perl substitution commands.

The bracketed form (e.g. "s{}{}") is supported, even the
(obscure, if not insane) mixed form is supported: "s{}//".

End of line comments (after the closing semicolon) beginning with a "#",
are allowed.

Multi-line substitutions are also allowed.  Embedded comments inside
a /x formatted pattern are not stripped out as you might expect:
rather they're carried along inside the matched pattern.

Example usage:

my $substitutions =>>'END_S';
s/pointy-haired boss/esteemed leader/
s/death spiral/minor adjustment/
END_S

my $find_replaces_aref =
  parse_perl_substitutions( \$substitutions );

Where the returned data should look like:
(( TODO revise! ))

   [ ['pointy-haired boss', 'esteemed leader'],
     ['death spiral',       'minor adjustment'],
   ]

=cut

sub parse_perl_substitutions {
  my $reps_text_ref = shift;
  my $Document = PPI::Document->new( $reps_text_ref );
  my $s_aref = $Document->find('PPI::Token::Regexp::Substitute');
  my @find_reps;
  foreach my $s_obj (@{ $s_aref }) {
    my $find      = $s_obj->get_match_string;
    my $rep       = $s_obj->get_substitute_string;
    my $modifiers = $s_obj->get_modifiers; # href
    my @delims    = $s_obj->get_delimiters;

    my $raw_mods = join '', keys %{ $modifiers };

    my $open_bracket_pat =
      qr{ ( [({[<] )                            # )}]>
        }xms;

    my $find_delims = $delims[0];
    my $rep_delims  = $delims[1];

    if ($find_delims =~ m{ $open_bracket_pat }xms ) {
      strip_brackets( \$find );
      ### TODO on multi-line $find, strip eol comments
    } elsif ($rep_delims =~ m{ $open_bracket_pat }xms ) {
      strip_brackets( \$rep );
    } else {  # is this actually a *good* idea?
      my $find_sep = substr( $find_delims, 0, 1 );
      my $rep_sep  = substr( $find_delims, 0, 1 );
      dequote( \$find, $find_sep );
      dequote( \$rep,  $rep_sep  );
    }

    accumulate_find_reps( \@find_reps, $find, $rep, $raw_mods );
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

Removes any balanced pair of surrounding bracket characters
from the referenced string.  Returns 1 for success, 0 for failure.

Text::Balanced's extract_bracketed, in it's infinite wisdom
does not extract what's inside the brackets, but instead
includes the brackets in the output.  This is a utility
to deal with this oddity.

Example usage:

 if( strip_brackets( \$string ) ) {
    print "brackets removed, see: $string\n";
 }

=cut

sub strip_brackets {
  my $string_ref = shift;
  my $open_bracket_pat = qr{ ( [\(\{\[\<] ) }xms;                        # )}]>

  my $original = ${ $string_ref };

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
    } else { # no change on failure
      $status = 0;
      ${ $string_ref } = $original;
    }
  }
  return $status;
}



=item dequote

Removes backwhack quoting, but only from the single character
supplied as a second argument.

Operates on a string reference, modifying it in place.

Example usage:

  $find = '\/home\/doom';
  dequote( \$find, '/' );

  # $find now '/home/doom';

(Sometimes it's easier to roll your own that to find someone else's.)
(Sometimes.)

=cut

sub dequote {
  my $string_ref = shift;
  my $sep = shift;
  ${ $string_ref } =~ s{ \\ \Q$sep\E }{$sep}xmsg;
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
