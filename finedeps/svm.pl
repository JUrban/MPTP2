#!/usr/bin/perl
#
# Produce the svm format from ours
#
#
# ./svm.pl data > svmdata
#
while(<>)
{
    @s=(0);
    m/(^\d+);(.*);(.*)/ or die;
    @f0=split(/,/,$2);
    @f1=();
    foreach $ff (@f0) 
    { 
	if(!exists $h{$ff}) { push(@s,$ff); $h{$ff}=$#s; }
	push(@f1,$h{$ff});
    }
    @f = sort {$a <=> $b} @f1;
    @r=split(/,/,$3);
    push(@r,$1);
    $f1=join(":1 ",@f);
    foreach $r (@r) {print $r," ",$f1,":1\n" }
}

