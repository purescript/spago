#!/usr/bin/perl

use warnings;
use strict;

print "\n";
print "running stack install";
print "\n";
print `stack install`;

my $stack_local_install_root = `stack path --local-install-root`;
chomp $stack_local_install_root;

my $bin_path = "$stack_local_install_root/bin/spacchetti";

my $target_path = "spacchetti";

print "\n";
print "copying built bin: $bin_path -> $target_path";
print "\n";
print `cp $bin_path $target_path`;

my $zip_path = "$ENV{TRAVIS_OS_NAME}.tar.gz";

print "\n";
print "zipping to $zip_path";
print "\n";
print `tar -zcvf $zip_path $target_path`;
