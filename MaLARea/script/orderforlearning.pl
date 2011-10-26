#!/usr/bin/perl
# generate a reasonable MML-compatible ordering of all premises & conjectures
# assumes all problems together with corresponding problem.allowed_local files
# algorithm:
# 0. all problems for a given article are topologically sorted using their axioms and allowed_local
# 1. only article-proper formulas are kept in the result of topological sorting
# 2. the article-level chunks are concatenated using mml.lar

# creating the 00srt data in /dev/shm/probs:

# for z in `ls`; do cd $z; for j in `ls *$z`; do perl -e '$i=shift; $f=shift; open(F,$f); while(<F>) { if(m/^fof.([^,]+), conjecture,/) {$c=$1} elsif(m/^fof.([^,]+_)$i, axiom,/) {$h{$1.$i}=() }  } open(G,"$f.allowed_local"); $_=<G>; m/^allowed_local.$c, \[(.*)\]/ or die $c; @k=split(/ *, */, $1); foreach $l (@k) {$h{$l} = ();} foreach $l (sort keys %h) {print "$c $l\n"}' $z $j; done > 00uns; tsort 00uns | tac > 00srt; cd /dev/shm/probs; done

# time sed -e 's/([^)]*)//g' probs3.train_0 >probs3.train_0.noweights
# then run like: ./orderforlearning.pl > 00res1
# the main result is probs2.train_0.noweights.out
# further checking:
#
# grep -v warning 00res1 >00res2
# sort 00res2 >00res2.sorted 
# diff 00res2.sorted probs3.refnr |less

# running snow incrementally (takes a lot of time with term features, so better parallelize):
#
# head -n1 probs2.train_0.noweights.out >  probs2.train_0.noweights0
# time ./snow -train -I probs2.train_0.noweights0 -F probs2.net_01  -B :0-112421
# time ./snow -test -i+ -I probs2.train_0.noweights.out -F probs2.net_01  -L 200 -o allboth  -B :0-112421> zzz1


# creating problems (typically use 40 and 200)

=begin code1

time perl -e '
$lim=shift;
open(F,"probs3.refnr"); while(<F>) {chop; $a[$i]=$_; $h{$_}=$i++;} 
open(G,"probs3.allasax"); while(<G>) { m/^fof.([^,]+),axiom/ or die $_; $p[$j]=$_; $a[$j]=$1 or die "$a[$j]:$_"; $j++;}
open(H,"probs3.allconjs"); while(<H>) { m/^fof.([^,]+),conjecture/ or die $_; exists $h{$1} or die "$1:$_"; $c[$h{$1}] = $_; }
$th = 0;
$axs=0;
open(P,"00dummy");
while(<>) {if(/^Example.*: *([0-9]+) */) { $axs=0; close(P); if(exists $c[$1]) { open(P,">advised/$a[$1]") or die; print P $c[$1]; $th=1;} else {$th=0}}
elsif (/^([0-9]+):/) { exists $a[$1] or die; if(($th==1) && ($axs++ < $lim)) { print P $p[$1]; }}}' 40 zzz

=end code1

=cut

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

my @train = ();
open(T,"probs2.train_0.noweights") or die;
while(<T>) {push(@train, $_);}
close(T);
open(T1,">probs2.train_0.noweights.out") or die;

foreach my $a (@mml)
{
    my $tmpa = $ma{$a};
    my $tmph = $mh{$a};
    foreach my $ref (@$tmpa)
    {
	if(exists $refsh{$ref})
	{
	    print ($ref, "\n");
	    print T1 $train[$refsh{$ref}];
	}
	else { print "warning3: $ref not in file refs: $a\n"; }

    }
}
close(T1);
