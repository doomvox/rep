#!/usr/bin/perl
# rep.pl                   doom@kzsu.stanford.edu
#                          13 May 2010

=head1 NAME

rep.pl - perform a series of find an replaces

=head1 SYNOPSIS

  perl rep.pl --backup <backup_file> --substitutions <filename> --target <file_to_act_on>

  perl rep.pl -b <backup_file> -f <file_to_act_on> 's/foo/bar/'

=head2 USAGE

  rep.pl -[options] [arguments]

  Options:

     -s                substitutions list file
     --substitutions   same
     -f                target file name to be modified
     --target          same
     -B                backup file name
     --backup          same

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

Emacs can then use the recorded locations to highlight the
changed regions, and it can use information about what was
replaced to perform undo operations.

The elisp code must choose a unique backup file name. This makes
it possible to do reverts of an entire run of substitutions.

The script returns a serialized data dump of the history
of the changes to the text.  See the documentation routine
L<serialize_change_metadata> in L<Emacs::Rep> for full
details of this output format.

Roughly, you can expect output that looks like:

  0:303:308:1:cars;
  1:113:123:8:of;
  1:431:441:8:of;
  1:596:606:8:of;
  2:330:355:23:.;
  3:702:711:0:evening;
  4:855:863:4:cane;

Where the colon separated fields are:

 first: the "pass" through the file (one pass per substitution command)
 second: begin point of changed string
 third:  end point of changed string
 fourth: the delta, the change in string length
 fifth: the original string that was replaced

Characters are counted from the beginning of the text,
starting with 1.

The fifth field may contain colons, but semicolons should be
escaped with a backslash.

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

our $VERSION = 0.03;
my  $prog    = basename($0);

my $DEBUG   = 0;                 # TODO set default to 0 when in production
my ( $locs_temp_file, $reps_file, $backup_file, $target_file );
GetOptions ("d|debug"           => \$DEBUG,
            "v|version"         => sub{ say_version(); },
            "h|?|help"          => sub{ say_usage();   },
            "s|substitutions=s" => \$reps_file,
            "b|backup=s"        => \$backup_file,
            "f|target=s"        => \$target_file,
           ) or say_usage();

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
my $find_replaces_aref;
eval {
  $find_replaces_aref =
    parse_perl_substitutions( \$reps_text );
};
if ($@) {
  carp "Problem parsing perl substitutions: $@";
  exit;
}

rename( $target_file, $backup_file ) or
  croak "can't copy $target_file to $backup_file: $!";

my $text;
{ undef $/;
  open my $fin, '<', $backup_file or croak "$!";
  $text = <$fin>;
  close( $fin );
}

# Apply the finds and replaces to text, recording the
# change meta-data
my $locations_aref;
eval {
  $locations_aref =
    do_finds_and_reps( \$text, $find_replaces_aref );
};
if ($@) {
  carp "Problem applying finds and replaces: $@";
  rename( $backup_file, $target_file ); # rollback!
} else {
  open my $fout, '>', $target_file or croak "$!";
  print {$fout} $text;
  close($fout);

  # serialize the data to pass to emacs
  my $flat_locs = serialize_change_metadata( $locations_aref );
  print $flat_locs;
}

### end main, into the subs

sub say_usage {
  my $usage=<<"USEME";
   $prog <-options> <arguments>

  Options:

     -s                substitutions list file
     --substitutions   same
     -f                target file name to be modified
     --target          same
     -b                backup file name
     --backup          same

     -d                debug messages on
     --debug           same
     -h                help (show usage)
     -v                show version
     --version         show version

Typical use:

  perl rep.pl -b "/tmp/edit_this.txt.bak" -f "edit_this.txt" 's/foo/bar/'

  perl rep.pl --backup <backup_file> --substitutions <filename> --target <file_to_act_on>

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
