#!/usr/bin/perl -w

use strict;
use CGI;
use MPTPNames;

my $query	  = new CGI;
my $input_refs	  = $query->param('refs');

my $MizHtml       = "http://mizar.cs.ualberta.ca/~mptp/7.11.06_4.150.1103/html/";


my $atpres_dir    = "/home/mptp/big/proofcomp/probs3/vout200fixed1/proved200f1min";


sub print_iframe
{
    my $url = shift;
    print<<END1
<iframe name="mizpres" src ="$url" width="90%" height="90%" style="margin:10px" frameborder="1">
<p>Your user agent does not support iframes or is currently configured
  not to display iframes. However, you may visit
  <A href="$url">the related document.</A></p>
</iframe>
END1

}

my @refs = split(/\s/, $input_refs);

print $query->header;
print $query->start_html(-dtd=>'-//W3C//DTD HTML 3.2//EN');

# print $query->start_html();



foreach my $ref (@refs)
{
    print '<dev style="height:30%;">';
    my ($href, $title) = MPTPNames::HTMLizeRef($ref,$MizHtml,'tst1');
    print $query->a({href=>$href,title=>$title,target=>"mizpres"}, $ref),":";
    if(open(F,"$atpres_dir/$ref.needed_vampire_orig"))
    {
	my @deps = <F>;
	foreach my $dep (@deps)
	{
	     my ($href, $title) = MPTPNames::HTMLizeRef($dep,$MizHtml,'tst1');
	     print $query->a({href=>$href,title=>$title,target=>"mizpres"}, $dep),",";
	}
    }
    print '</div>';
}

my ($href, $title) = MPTPNames::HTMLizeRef($refs[0],$MizHtml,'tst1');
print_iframe("$href");

print $query->end_html();
