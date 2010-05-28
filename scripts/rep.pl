#!/usr/bin/perl
# rep.pl                   doom@kzsu.stanford.edu
#                          13 May 2010

=head1 NAME

rep.pl - perform a series of find an replaces

=head1 SYNOPSIS

  perl rep.pl --extension <bak_extension> --substitutions <changes_list>  <file_to_act_on>

  perl rep.pl --extension <bak_extension> <file_to_act_on> 's/foo/bar'

=head2 USAGE

  rep.pl -[options] [arguments]

  Options:
     -t                temp file name (for location data)
     --temp            same
     -s                substitutions list file
     --substitutions   same
     -d                debug messages on
     --debug           same
     -h                help (show usage)
     -v                show version
     --version         show version


=head1 DESCRIPTION

B<rep.pl> is a script which does finds and replaces on a file,
and records the beginning and end points of the modified
strings.

It is intended to act as an intermediary between Emacs::Rep
and the emacs lisp code which drives the "rep" process.

Emacs can then use the recorded locations to highlight the changed regions.

The elisp code must choose a unique backup_extension in order to
handle reverts:

  perl rep.pl --extension <unique_extension> --substitutions <changes_list> <file_to_act_on>

The script returns a serialized data dump of the history of the changes to the text,
in a form that looks like this:

  0:303:308:1:cars
  1:83:116:8:of
  1:113:123:8:of
  1:171:181:8:of
  1:431:441:8:of
  1:596:606:8:of
  2:61:86:23:.
  2:330:355:23:.
  2:639:664:23:.
  2:889:914:23:.
  3:702:711:0:evening
  4:855:863:4:cane

The first field corresponds to the "pass" through the file (one pass per substitution command)
The second and third fields are the begin and end points of the changed strings, counting from 1.
The fourth field is the size of the "delta", the change in length due to the modification.
The fifth field is the originally matched string, before the change.
Note: this field may contain a colon.  Any separators after the fourth should be ignored.

=cut

use warnings;
use strict;
$|=1;
use Carp;
use Data::Dumper;

use File::Path     qw( mkpath );
use File::Basename qw( fileparse basename dirname );
use File::Copy     qw( copy move );
use Fatal          qw( open close mkpath copy move );
use Cwd            qw( cwd abs_path );
use Env            qw( HOME );
use Getopt::Long   qw( :config no_ignore_case bundling );
use FindBin qw( $Bin );
use lib ("$Bin/../lib",
         "$HOME/End/Cave/Rep/Wall/Emacs-Rep/scripts/../lib"); # TODO
use Emacs::Rep     qw( :all );

our $VERSION = 0.01;
my  $prog    = basename($0);

my $DEBUG   = 1;                 # TODO set default to 0 when in production
my ( $locs_temp_file, $reps_file, $unique_extension );
GetOptions ("d|debug"           => \$DEBUG,
            "v|version"         => sub{ say_version(); },
            "h|?|help"          => sub{ say_usage();   },
            "s|substitutions=s" => \$reps_file,
            "E|extension=s"     => \$unique_extension,
           ) or say_usage();

# This version mimics "-i" behavior but reserves STDOUT to communicate
# the changed location data back to the calling program (emacs)

my $input_file = shift;  # presume we act on only one file
my $backup = $input_file . '.' . $unique_extension;

# find_and_reps are specified by perl-pseudo-code,
#   s///xims  or (someday) s{}{}xmis

# get a series of finds and replaces
#   either from the substitutions file,
#   or from command-line (a series of strings from @ARGV),

my $reps_text;
if( $reps_file ) {
  undef $/;
  open my $fh, '<', $reps_file or croak "$!";
  $reps_text = <$fh>;
} else {
  $reps_text = join( "\n", @ARGV );
}

# process the find_and_reps into an array of find and replace
# pairs (modifiers get moved inside the find.)
my $find_replaces_aref =
  parse_perl_substitutions( \$reps_text );

# Note: don't do rename this until after the substitutions parse.
# (possibly: on failure, will need to undo this -- TODO)
rename( $input_file, $backup ) or croak "can't copy $input_file to $backup: $!";
my $text;
{ undef $/;
  open my $fin, '<', $backup or croak "$!";
  $text = <$fin>;
}

# Apply the finds and replaces to text, recording the history
# of the changes
my $locations_aref;
eval {
  $locations_aref =
    do_finds_and_reps( \$text, $find_replaces_aref );
};

open my $fout, '>', $input_file or croak "$!";
print {$fout} $text;
close($fout);

# serialize the data to pass to emacs
my $flat_locs = flatten_locs( $locations_aref );
print $flat_locs;

### end main, into the subs

sub say_usage {
  my $usage=<<"USEME";
   $prog <-options> <arguments>
     TODO fill-in usage statement
USEME
  print "$usage\n";
  exit;
}

sub say_version {
  print "Running $prog version: $VERSION\n";
  exit 1;
}


__END__

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
