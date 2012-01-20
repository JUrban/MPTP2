#!/usr/bin/perl
# produce sofia input from snow input
# run like:
#
# perl -F snow2sofia.pl probs6.train_01 > probs6.softrain

use strict;

my %h=();    # hash keeping contiguous numbering for pars of features
my $z=10000; # max bound for premise numbering - we start the pair numbering here

my @prev = (); # premises already seen
my $neg = 10; # number of negative examples added

while (<>)
{
    my @l= m/(\d+)/g;
    my $q= $l[0];
    my @t= grep {$_ < 2000000} @l;
    my @f= grep {$_ >= 2000000} @l;
    my @p= ();

    if($#prev > 5*$neg) # if enough previous, put random negative example
    {
	for (my $i = $neg; --$i; ) { push(@p, $prev[int rand ($#prev)]) }
    }

    my $r= 1;  # bigger weight for the name of the formula than for the premises
    my $cnt = 0;
    foreach my $j (@t,@p)
    {
	print "$r qid:$q ";
	$cnt++;
	if($cnt > $#t) { $r= -1;}
	my @f1=();
	foreach my $i (@f)
	{
	    if (!exists($h{"$j:$i"})) { $h{"$j:$i"}=$z++; }
	    push(@f1,$h{"$j:$i"})
	}
	foreach my $k (sort {$a<=>$b} @f1) { print " $k:1"; }
	print "\n";
    }
    push(@prev, $q);
}
