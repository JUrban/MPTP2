#!/usr/bin/perl -w

# ls *protok*simple *protok*_KBO *protok*_SOS| xargs grep -l Theorem | xargs grep Processed | perl -F ~/gr/MPTP2/Strategies/CliStr.pl | less

# ls *protok*simple *protok*_KBO *protok*_SOS| xargs grep -l Theorem | xargs grep Processed | perl -e ' while(<>) { m/^([^.]*)\.(.*): *(\d+)/ or die; if((! exists($h{$1})) || ($h{$1} > $3)) { $i{$1}=$h{$1}; $j{$1}=$g{$1}; $h{$1} = $3; $g{$1} = $2; }} foreach $k (sort keys %h) { $v{$g{$k}}{$k}=$h{$k}  } foreach $p (sort keys %v) {print "\n$p:\n"; foreach $k (sort keys %{$v{$p}}) {print "$k:$h{$k}\n" if(($h{$k}>500) && ($h{$k}<30000))}}' |less

use strict;

sub TopStratProbs
{
    my %g = ();
    my %h = ();
    my %i = ();
    my %j = ();
    my %v = ();

    while (<>) {
	m/^([^.]*)\.(.*): *(\d+)/ or die;
	if ((! exists($h{$1})) || ($h{$1} > $3))
	{
	    $i{$1}=$h{$1};
	    $j{$1}=$g{$1};
	    $h{$1} = $3;
	    $g{$1} = $2;
	}
    }

    foreach my $k (sort keys %h) {
	$v{$g{$k}}{$k}=$h{$k};
    }

    foreach my $p (sort keys %v) {
	print "\n$p:\n";
	foreach my $k (sort keys %{$v{$p}}) {
	    print "$k:$h{$k}\n" if(($h{$k}>500) && ($h{$k}<30000));
	}
    }
}

TopStratProbs();

