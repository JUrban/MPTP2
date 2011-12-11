#!/usr/bin/perl -w

=head1 NAME

MizItemize.pl file ( create items from .miz and .xml files)

=head1 SYNOPSIS

MizItemize.pl ~/a

=cut

use strict;
use HTML::Entities ();


my $filestem   = shift(@ARGV);

my $ucf = uc($filestem);

my $miz = $filestem . ".miz";

my @lines=();

open(MIZ,$miz) or die "File not readable: $miz";
print  ('<?xml version="1.0"?>', "\n", "<Comments aid=\"$ucf\">\n");

my $incomment = 0;
my @curr_comments = ();

my $begcomment = 0;
my $endcomment = 0;

while($_=<MIZ>) { s/\r\n/\n/g; push(@lines, $_); }
close(MIZ);

foreach my $i (0 .. $#lines)
{ 
    $_ = $lines[$i];
    if(m/^ *::+.*/) # inside a comment
    {
	if($incomment == 1) # inside an already started comments
	{
	    push(@curr_comments, $_);
	    $endcomment = $i+1;
	}
	else  # starting a new comment
	{
	    $begcomment = $i+1;
	    $endcomment = $i+1;
	    @curr_comments = ();
	    push(@curr_comments, $_);
	    $incomment = 1;
	}
    }
    elsif(($incomment == 1) && (m/^ *$/)) # continuing a comment by empty line
    {
	push(@curr_comments, $_);
	$endcomment = $i+1;
    }
    else  # not inside a comment
    {
	if($incomment == 1) # print the previous comment if any
	{

	    print  "<Comment line=\"$begcomment\" endline=\"$endcomment\">";
	    print   HTML::Entities::encode(join('', @curr_comments));
	    print  "</Comment>\n";

	    $incomment = 0;
	}
    }
};

# print the last comment if the article ends in a comment
if($incomment == 1)
{
    print  "<Comment line=\"$begcomment\" endline=\"$endcomment\">";
    print  HTML::Entities::encode(join('', @curr_comments));
    print  "</Comment>\n";
}


print  ('</Comments>', "\n");



