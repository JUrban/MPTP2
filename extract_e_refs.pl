#!/usr/bin/perl -w

# extract the references from E proof in tstp syntax;
# put them into 
# snow_proof( conjecture_name, th_or_def_names, bg_names)

use strict;

my @td_names = ();
my @bg_names = ();
my $conjecture = "";
while (<>)
{
    (/^fof[^,]+,(axiom|conjecture).*\bfile\b[^,]+,([^\)]+)[\)].*/)
	or die "Bad fof $_";

    if ("conjecture" eq $1) { $conjecture = $2; }
    else
    {
	my $ref = $2;
	if ($ref =~ m/^ *[dt]\d+_/) { push(@td_names, $ref); }
	else { push(@bg_names, $ref); }
    }
}

die "No conjecture" if ($conjecture eq "");

print "snow_proof($conjecture, [";
print (join(",", @td_names), "],[", join(",", @bg_names));
print "]).\n";
