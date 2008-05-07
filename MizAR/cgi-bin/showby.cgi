#!/usr/bin/perl -w

use strict;
use CGI;
use IO::Socket;
use File::Temp qw/ :mktemp  /;
use IPC::Open2;
use HTTP::Request::Common;
use LWP::Simple;

# possible SZS statuses
sub szs_INIT        ()  { 'Initial' } # system was not run on the problem yet
sub szs_UNKNOWN     ()  { 'Unknown' } # used when system dies
sub szs_THEOREM     ()  { 'Theorem' }
sub szs_COUNTERSAT  ()  { 'CounterSatisfiable' }
sub szs_RESOUT      ()  { 'ResourceOut' }
sub szs_GAVEUP      ()  { 'GaveUp' }   # system exited before the time limit for unknown reason


my $MyUrl = 'http://octopi.mizar.org/~mptp';
my $PalmTreeUrl = $MyUrl . "/PalmTree.jpg";
my $TemporaryDirectory = "/tmp";
my $TemporaryProblemDirectory = "$TemporaryDirectory/matp_$$";
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $Mizfiles = "/home/mptp/public_html/mml";
my $MizHtml = $MyUrl . "/mml/html/";
my $mizf =     "bin/mizf";
my $eproof =     "bin/eproof";
my $vampire =     "bin/vampire9";
my $SPASS =     "bin/SPASST";
my $cpulimit=8;
my $xsltproc =     "bin/xsltproc";
my $dbenv = "bin/dbenv.pl";
my $utilspl =  "/home/mptp/public_html/cgi-bin/bin/utils.pl";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $miz2html = "$Xsl4MizarDir/miz.xsl";
my $mizpl = "$Xsl4MizarDir/mizpl.xsl";
my $doatproof = 0;
my $atproof = '@' . 'proof';

my $query	  = new CGI;
my $input_article	  = $query->param('article');
my $input_lc	  = $query->param('lc');
my $input_tmp     = $query->param('tmp');
my $atp	  = $query->param('ATP');
my $htmlize	  = $query->param('HTML');

my ($line, $col) = $input_lc=~m/(.*)_(.*)/;
my $col1 = $col - 4;

my $idv_img = "<img SRC=\"$PalmTreeUrl\" alt=\"Show IDV graph\" title=\"Show IDV graph\">";

sub HTMLize
{
    my ($ref) = @_;
    my $res = '';
#    print '<a href="foo">goo</a>'; $MizHtml="hj";
    if(($ref=~m/^([dtl][0-9]+)_(.*)$/) || ($ref=~m/^(s[0-9]+)_(.*?)__.*$/) || ($ref=~m/^([fcr]c[0-9]+)_(.*)$/) || ($ref=~m/^dt_([klmugrv][0-9]+)_(.*)$/))
    { 
	my ($kind,$ar) = ($1,$2);
	if($kind =~ m/^l(.*)/) { $kind = 'e' . $1; }
	if($ar eq $input_article) {$res  = '#'.  uc($kind); } else {$res  = $MizHtml . $ar . '.html#' . uc($kind); }
    }
    elsif($ref=~m/^(e[0-9]+)_(.*)__(.*)$/) { $res = '#' . uc($1) . ':' . $2; }
    elsif($ref=~m/^d[et]_(c[0-9]+)_(.*)__(.*)$/) { $res = '#' . lc($1) . ':' . $2; }
    elsif($ref=~m/^(abstractness|free|existence|redefinition|symmetry|antisymmetry|asymmetry|reflexivity|irreflexivity|connectedness|commutativity|idempotence|involutiveness|projectivity)_([klmugrv][0-9]+)_(.*)$/) { if($3 eq $input_article) {$res  = '#'.  uc($2); } else { $res = $MizHtml . $3 . '.html#' . uc($2); }}
    return $res;
}



    if($htmlize != 1)    { 
 print $query->header;
print $query->start_html("ATP Output"); }
else { print $query->header('text/xml');}

my $File0 = "$TemporaryDirectory/matp_" . $input_tmp . "/problems/" . $input_article . "/" . $input_article . "__" . $line . "_";
my $File1 = $File0 . $col;
my $File2 = $File0 . $col1;
my $File;
if (-e $File1) { $File = $File1; } elsif(-e $File2) { $File = $File2}
if(    open(F,$File))
{
    if($htmlize != 1)    { print "<pre>"; }
    my $status = szs_UNKNOWN;
    if(defined $atp)
    {
	if($atp =~ m/^refs$/)
	{
	    my $spass = 0;
	    my @refs=();

	    if($spass == 1)
	    {
		my $spass_formulae_line = `bin/tptp4X -x -f dfg $File |bin/SPASS -Stdin -Memory=900000000 -PGiven=0 -PProblem=0 -TimeLimit=$cpulimit -DocProof | tee $File.sout | grep "Formulae used in the proof"`;
		if($spass_formulae_line=~m/Formulae used in the proof *: *(.*) */) { @refs = split(/ +/, $1); }
	    }
	    else
	    {
		my $eproof_pid = open(EP,"$eproof --print-statistics -xAuto -tAuto --cpu-limit=$cpulimit --memory-limit=Auto --tstp-in --tstp-out $File| tee $File.eout1 | grep -v '^#' | tee $File.eout | grep ',file('|")  or die("bad eproof input file $File");
#	    $proved_by{$conj} = [];

 		while ($_=<EP>)
 		{
		    m/.*, *file\([^\),]+, *([a-z0-9A-Z_]+) *\)/ or die "bad proof line: $File: $_";
		    my $ref = $1;
		    push( @refs, $ref);
		}		
		close(EP);


 		my $status_line = `grep -m1 'SZS status' $File.eout1`;

		if ($status_line=~m/.*SZS status[ :]*([a-zA-Z0-9_-]+)/)
		{
		    $status = $1;
		}
		else
		{
		    print "Bad E status line: $status_line, please complain";
		}
 		if (!($status eq szs_THEOREM)) { @refs = () }
	    }
	    if($#refs >= 0) 
	    {
		if($htmlize == 1)  
		{ 
		    print '<?xml version="1.0"?><div>'; 
		    print 'ATP explanation (';
		    print $query->a({href=>"$MyUrl/cgi-bin/tptp/RemoteSOT1.cgi?article=" . $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp . '&idv=1'},
				    $idv_img);
		    print ', ';
		    print $query->a({href=>"$MyUrl/cgi-bin/tptp/RemoteSOT1.cgi?article=" . $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp . '&DM=1'}, "Try more");
		    print ', ';
		    print $query->a({href=>"$MyUrl/cgi-bin/tptp/MMLQuery.cgi?article=" . $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp },
				    "MMLQuery (very experimental)");
		    print " ):<br>\n";  
#		    print $query->a({href=>"$MyUrl/cgi-bin/showby.cgi?article=" . $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp . '&DM=1'}, "Do more"), " ):<br>\n";  
		    foreach my $ref (@refs) 
		    {
			my $href=HTMLize($ref); 
			if(length($href)>0) { print $query->a({href=>$href}, $ref), ", "; } 
			else {print $ref,", ";}
		    } 
		    print "</div>"; 
		} 
		else { print join(",", @refs);} 
	    } else { 
		print "Proof not found (status: $status, "; 
		print $query->a({href=>"$MyUrl/cgi-bin/tptp/RemoteSOT1.cgi?article=" . $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp . '&DM=1'}, "Try more"), " ):<br>\n";  
	    }
	}
	else { system("$eproof --print-statistics -xAuto -tAuto --cpu-limit=$cpulimit --memory-limit=Auto --tstp-in --tstp-out $File"); }
    }
    else { local $/; $_= <F>; print $_; }
    if($htmlize != 1) { print "<pre/>";    print $query->end_html;}
    close(F);
}
else { print "You just hit a line numbering bug, please complain"; }
