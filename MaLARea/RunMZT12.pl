#!/usr/bin/perl -w

## create tempdir, copy malarea distro (the dir passed as first arg) into it, and run it with
## the ltbinput and default args

## SYNOPSIS

## ./RunMZT12.pl /home/urban/MaLARea---0.3 /home/urban/MaLARea---0.3/doc/BatchTstMZRMZR

use strict;

die "expecting the malarea install dir as first arg, and the ltb input as second arg" if ($#ARGV != 1);

my $tmpdir = "/tmp/mal_$$";
my $prefix = "run_$$";

# if (!mkdir($tmpdir,0777))
# {
#     print("ERROR: Cannot make temp dir $tmpdir\n");
#     die("\n");
# }

my $maldir = shift(@ARGV);
my $ltbinput = shift(@ARGV);

`cp -a $maldir $tmpdir`;

chdir($tmpdir);

local $SIG{'XCPU'} = sub { `rm -r -f /dev/shm/$prefix`; die "Killed by SIXXCPU"; };

exec("time ./TheoryLearner.pl  -o\"$ltbinput\" -z1 -e\"$prefix/\" -T\"/dev/shm/\" -C4 -A256   --loadprovedby=\"data/MZT1000TrainingData\" -f2 -O1 -p128 -y1  -u1   -t400 -S0  -P1 -D1 -l1 -M0 -b1 -w6 -i5 -L15000 $prefix  |tee $prefix.log");
