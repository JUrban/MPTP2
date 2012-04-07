#!/usr/bin/perl
#
# Produce the svm format from ours
#
#
# ./svm.pl data > svmdata
#
while(<>)
{
    m/(^\d+);(.*);(.*)/ or die;
    @f=sort {$a <=> $b} split(/,/,$2);
    @r=split(/,/,$3);
    push(@r,$1);
    $f1=join(":1 ",@f);
    foreach $r (@r) {print $r," ",$f1,":1\n" }
}

