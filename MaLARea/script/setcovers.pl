#!/usr/bin/perl
# produce sofia input from snow input
# run like:
#
# perl -F snow2sofia.pl probs6.train_01 > probs6.softrain
#
# or to have a testfile with 10%:
#
# ./snow2sofia.pl probs6.train_01 tst1 > probs6.softrain

use strict;

my %r=();
my %h=();    # hash keeping contiguous numbering for pars of features
my $z=10000; # max bound for premise numbering - we start the pair numbering here

my @prev = (); # premises already seen
my $neg = 30; # number of negative examples added

while (my $f=shift)
{
    open(F,$f) or die; my @l=<F>; my %g=(); @g{@l}=0; close(F);
    $h{$f} = \%g;
}

my @ff = sort keys %h;

foreach my $i (0 .. $#ff)
{
    foreach my $j ($i+1 .. $#ff)
    {
	my %t = ();
	@t{ keys( %{$h{$ff[$i]}}), keys (%{$h{$ff[$j]}}) } = ();
	$r{ "$ff[$i]:$ff[$j]" } = scalar keys %t;
    }
}

foreach my $v (sort {$r{$a} <=> $r{$b}} keys %r)
{
    print "$v\t$r{$v}\n";
}




