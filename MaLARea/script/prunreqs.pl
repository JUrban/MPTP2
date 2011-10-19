#!/usr/bin/perl
# prune problem specs using an initial dependency table starting with conjecture
# run like: 
# cat /local/data/alama/mizar-items/data/4.150.1103/requirements/*|grep ':theorem'>/home/mptp/ph/00rq1
# time ./prunereqs.pl /home/mptp/ph/00rq1 `ls`
#
# real: rqLess, arithm: rqReal, boole: t\d+_boole$, subset: t\d+_subset
use strict;

my %h = ();

my $deps = shift;

open(F,$deps) or die $deps;
while(<F>)
{
    chop;
    my @a = split(/[ ,]+/);
    $a[0] =~ m/^([a-z0-9_]+):theorem:(\d+)$/ or die "$_";
    $a[0] = 't' . $2 . '_' . $1;
    $h{$a[0]} = ();
    foreach my $i (1 .. $#a) { $h{$a[0]}->{$a[$i]} = (); }
}

close(F);

my $f;

while($f=shift)
{
    $f=~m/.*__(.*)/ or die $f; 
    my $t = $1; 
    if(exists $h{$t}) 
    {
	open(G,$f); 
	open(G1,">$f.out"); 
	while(<G>) 
	{
	    if(m/^fof.t[0-9]+_(boole|subset),/)
	    {
		print G1 $_ if(exists $h{$t}->{uc($1)}); 
	    }
	    elsif(m/^fof.rqReal/)
	    {
		print G1 $_ if(exists $h{$t}->{'ARITHM'});
	    }
	    elsif(m/^fof.rqLess/)
	    {
		print G1 $_ if(exists $h{$t}->{'REAL'}); 
	    }
	    else
	    {
		print G1 $_;
	    }
	}
    }
}
