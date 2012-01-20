#!/usr/bin/perl
# produce sofia input from snow input
# run like:
#
# perl -F snow2sofia.pl probs6.train_01 > probs6.softrain


$z=10000;
while (<>)
{
    @l= m/(\d+)/g;
    $q=$l[0];
    @t=grep {$_ < 2000000} @l;
    @f=grep {$_ >= 2000000} @l;
    $r=2;
    foreach $j (@t)
    {
	print "$r qid:$q ";
	$r=1;
	@f1=();
	foreach $i (@f)
	{
	    if (!exists($h{"$j:$i"})) {  $h{"$j:$i"}=$z++;	    }
	    push(@f1,$h{"$j:$i"})
	}
	foreach $k (sort {$a<=>$b} @f1) { print " $k:1";	}
	print "\n";
    }
}
