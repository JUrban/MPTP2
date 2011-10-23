#!/usr/bin/perl
# generate a reasonable MML-compatible ordering of all premises & conjectures
# assumes all problems together with corresponding problem.allowed_local files
# algorithm:
# 0. all problems for a given article are topologically sorted using their axioms and allowed_local
# 1. only article-proper formulas are kept in the result of topological sorting
# 2. the article-level chunks are concatenated using mml.lar

# creating the 00srt data in /dev/shm/probs:

# for z in `ls`; do cd $z; for j in `ls *$z`; do perl -e '$i=shift; $f=shift; open(F,$f); while(<F>) { if(m/^fof.([^,]+), conjecture,/) {$c=$1} elsif(m/^fof.([^,]+_)$i, axiom,/) {$h{$1.$i}=() }  } open(G,"$f.allowed_local"); $_=<G>; m/^allowed_local.$c, \[(.*)\]/ or die $c; @k=split(/ *, */, $1); foreach $l (@k) {$h{$l} = ();} foreach $l (sort keys %h) {print "$c $l\n"}' $z $j; done > 00uns; tsort 00uns | tac > 00srt; cd /dev/shm/probs; done


# run like: 
# time  ~/gr/MPTP/SCRIPT/advisor/deps2mpadata.pl item-dependency-table > item_mptp_deps
# time ./pruneproblems.pl item_mptp_deps `ls`
use strict;

my %ma = ();
my %mh = ();
my @mml = ();

open(M,"mml.lar") or die;
while(<M>) {chop; push(@mml,$_);}

foreach my $a (@mml) 
{
    $ma{$a} = [];
    $mh{$a} = {};
    if(open(A,"$a/00srt"))
    {
	my $tmpa = $ma{$a};
	my $tmph = $mh{$a};
	while(<A>)
	{
	    chomp;
	    push(@$tmpa, $_);
	    $tmph->{$_} = scalar(@$tmpa)-1;
	}
    }
    else { print "warning1: $a/00srt not found\n"; }
}

my @refs = ();
my %refsh = ();

open(P,"probs2.refnr") or die;
while(<P>) {chomp; push(@refs, $_); $refsh{$_}=$#refs; }

# classify by article
# find refs that are not in 00srt and add them
foreach my $ref (@refs)
{
    my $a;
    if($ref =~ m/^rq/)
    {
	$ref =~ m/.*__[a-z]\d+_(.*)__.*/ or die $_;
	exists $mh{$1} or die $$_;
	$a = $1;
    }
    else
    {
	my @tmp1 = split(/_/, $ref);
	my $end = $#tmp1;
	my $chunk = $tmp1[$end];
	while(! (exists $mh{$chunk})) { if($end > 0) { $end--;} else {die $ref;} $chunk = $tmp1[$end] . '_' . $chunk; }
	$a = $chunk;
	my $bigger = $tmp1[$end-1] . '_' . $chunk; # check for one bigger - should be enough
	$a = $bigger if(exists $mh{$bigger});
    }

    if(! exists $mh{$a}->{$ref})
    {
	my $tmpa = $ma{$a};
	my $tmph = $mh{$a};
	print "warning2: adding $ref\n";
	push(@$tmpa, $ref);
	$tmph->{$ref} = scalar(@$tmpa)-1;
    }
}

foreach my $a (@mml)
{
    my $tmpa = $ma{$a};
    my $tmph = $mh{$a};
    foreach my $ref (@$tmpa)
    {
	if(exists $refsh{$ref})
	{
	    print ($ref, "\n");
	}
	else { print "warning3: $ref not in file refs: $a\n"; }

    }
}
