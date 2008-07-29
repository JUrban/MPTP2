#!/usr/bin/perl -w

## create tempdir, copy malarea distro (the dir passed as first arg) into it, and run it with
## the ltbinput and default args

## SYNOPSIS

## ./RunLTB.pl /home/urban/MaLARea---0.3 /home/urban/LTBInput1

use strict;

die "expecting the malarea install dir as first arg, and the ltb input as second arg" if ($#ARGV != 1);

my $tmpdir = "/tmp/mal_$$";

# if (!mkdir($tmpdir,0777))
# {
#     print("ERROR: Cannot make temp dir $tmpdir\n");
#     die("\n");
# }

my $maldir = shift(@ARGV);
my $ltbinput = shift(@ARGV);

`cp -a $maldir $tmpdir`;

chdir($tmpdir);

exec("time ./TheoryLearner.pl -o\"$ltbinput\" -F1 -z1 -e\"run1/\" -T\"/dev/shm/\" -C1 -A256   -f2 -O1 -p128 -y1  -u1   -t200 -S256  -P1 -D1 -l1 -M0 -b1 -w6 -i5 -L15000 run1  |tee run1.log");

