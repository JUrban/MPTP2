#!/usr/bin/perl

# run as :
# perl -F MMLdivtest.pl /home/mptp/public_html/mml4.100.1011/mml.lar

open(F,"MMLdivision.1011"); 
while(<F>) 
{
    if(m/^#/) { chomp; $c=$_;}
    else
    {
	chomp;
	push(@g,lc($_));
	$h{lc($_)} = $c;
    }
}
while(<>)
{
    print "SS: $_";
    chomp;
    foreach $k (@g)
    {
	if(m/^$k/) 
	{
#	    print "\t$k\n";
	    print "\t$h{$k}\n";
	    $hh{$h{$k}}++;
	}
    }
}
foreach $u (sort keys %hh)
{
    print "$hh{$u}: $u\n";
}
