#!/usr/bin/perl

use warnings;
use strict;
use feature 'say';

say "running stack install";
say `stack install`;

my $stack_local_install_root = `stack path --local-install-root`;
chomp $stack_local_install_root;

my $bin_path = "$stack_local_install_root/bin/spacchetti";

my $target_path = "spacchetti";

say "copying built bin: $bin_path -> $target_path";
say `cp $bin_path $target_path`;

my $zip_path = "$ENV{TRAVIS_OS_NAME}.tar.gz";

say "zipping to $zip_path";
say `tar -zcvf $zip_path $target_path`;
