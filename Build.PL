use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Emacs::Rep',
    license             => 'perl',
    dist_author         => q{Joseph Brenner <doom@kzsu.stanford.edu>},
    dist_version_from   => 'lib/Emacs/Rep.pm',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Emacs-Rep-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
