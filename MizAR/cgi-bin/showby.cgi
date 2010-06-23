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


my $query	  = new CGI;
my $input_article	  = $query->param('article');
my $input_lc	  = $query->param('lc');
my $input_tmp     = $query->param('tmp');
my $atp	  = $query->param('ATP');
my $htmlize	  = $query->param('HTML');
my $spass	  = $query->param('spass');
my $eprover	  = $query->param('eprover');
my $advice	  = $query->param('advice');
my $aport	  = $query->param('ap');
my $mmlversion    = $query->param('MMLVersion');

(defined $spass) or $spass = 0;
(defined $eprover) or $eprover = 0;
(defined $advice) or $advice = 0;
$mmlversion   = '4.100.1011' unless defined($mmlversion);

# my $MyUrl = 'http://octopi.mizar.org/~mptp';
my $MyUrl = 'http://mws.cs.ru.nl/~mptp';
my $PalmTreeUrl = $MyUrl . "/PalmTree.jpg";
my $TPTPLogoUrl = $MyUrl . "/TPTP.gif";
my $TSTPLogoUrl = $MyUrl . "/TSTP.gif";
my $TemporaryDirectory = "/tmp";
my $TemporaryProblemDirectory = "$TemporaryDirectory/matp_$$";
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $Mizfiles = "/home/mptp/public_html/mml$mmlversion";
my $Bindir = "bin$mmlversion";
my $MizHtml = $MyUrl . "/mml$mmlversion/html/";
my $mizf =     "$Bindir/mizf";
my $eproof =     "$Bindir/eproof";
my $runwtlimit = "$Bindir/runwtlimit";
my $vampire =     "$Bindir/vampire_rel2";
my $SPASS =     "$Bindir/SPASST";
my $getsymbols =     "$Bindir/GetSymbols";
my $cpulimit=5;
my $xsltproc =     "$Bindir/xsltproc";
my $dbenv = "$Bindir/dbenv.pl";
my $utilspl =  "/home/mptp/public_html/cgi-bin/$Bindir/utils.pl";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $miz2html = "$Xsl4MizarDir/miz.xsl";
my $mizpl = "$Xsl4MizarDir/mizpl.xsl";
my $doatproof = 0;
my $atproof = '@' . 'proof';



my $advhost	  = "localhost";
my $advlimit	  = 16;


my %grefsyms =();     # Ref2Sym hash for each reference array of its symbols
my %greftrmstd =();   # Ref2Sym hash for each reference array of its stdterms (their shared-entry numbers)
my %greftrmnrm =();   # Ref2Sym hash for each reference array of its nrmterms (their shared-entry numbers)
my %grefposmods =();  # Ref2Sym hash for each ref array of its positive models (without offset)
my %grefnegmods =();  # Ref2Sym hash for each ref array of its negative models (without offset)

my %glocal_consts_refs =(); # contains references containing local constants

my ($line, $col) = $input_lc=~m/(.*)_(.*)/;
my $col1 = $col - 4;

my $idv_img = "<img SRC=\"$PalmTreeUrl\" alt=\"Show IDV proof tree\" title=\"Show IDV proof tree\">";
my $tptp_img = "<img SRC=\"$TPTPLogoUrl\" alt=\"Show TPTP Problem\" title=\"Show TPTP Problem\">";
my $tstp_img = "<img SRC=\"$TSTPLogoUrl\" alt=\"Show TSTP Proof\" title=\"Show TSTP Proof\">";

## provide links and titles to various MPTP references
sub HTMLize
{
    my ($ref) = @_;
    my $res = '';
    my $title = '';
#    print '<a href="foo">goo</a>'; $MizHtml="hj";
    if(($ref=~m/^([dtl][0-9]+)_(.*)$/) 
       || ($ref=~m/^(s[0-9]+)_(.*?)__.*$/) 
       || ($ref=~m/^([fcr]c[0-9]+)_(.*)$/) 
       || ($ref=~m/^dt_([klmugrv][0-9]+)_(.*)$/))
    {
	my ($kind,$ar) = ($1,$2);
	if(($ref=~m/^l.*/) && ($kind =~ m/^l(.*)/)) { $kind = 'e' . $1; }
	if($ar eq $input_article) {$res  = '#'.  uc($kind); }
	else {$res  = $MizHtml . $ar . '.html#' . uc($kind); }
	$title =  uc($ar) . ":" . uc($kind);
    }
    elsif($ref=~m/^(e[0-9]+)_(.*)__(.*)$/)
    {
	$title =  "proposition " . uc($1) . " in block " . $2;
	$res = '#' . uc($1) . ':' . $2; 
    }
    elsif($ref=~m/^d[et]_(c[0-9]+)_(.*)__(.*)$/)
    {
	$res = '#' . lc($1) . ':' . $2;
	$title =  "constant " . uc($1) . " in block " . $2;
    }
    elsif($ref=~m/^(abstractness|free|existence|redefinition|symmetry|antisymmetry|asymmetry|reflexivity|irreflexivity|connectedness|commutativity|idempotence|involutiveness|projectivity)_([klmugrv][0-9]+)_(.*)$/)
    {
	$title = $1 . " of " . uc($3) . ":" . uc($2);
	if($3 eq $input_article) {$res  = '#'.  uc($2); }
	else { $res = $MizHtml . $3 . '.html#' . uc($2); }
    }
    elsif($ref=~m/^spc([0-9]+)_boole$/) 
    {
	if($1 eq "0") { $title = $1 . " is empty"; } 
	else { $title = $1 . " is non empty"; } 
    }
    elsif($ref=~m/^spc([0-9]+)_numerals$/)
    {
	if($1 eq "0") { $title = $1 . " is Element of NAT"; }
	else { $title = $1 . " is positive Element of NAT"; }
    }
    elsif($ref=~m/^rq.*$/) { $title = "arithmetic evaluation"; }
    elsif($ref=~m/^fraenkel_.*$/) { $title = "fraenkel functor first-order instance"; }
    return ($res, $title);
}

## Extract symbols from a line created by GetSymbols
## we use %grefsyms for future here (copied from malarea)
sub ExtractSymbols
{
    my ($symstring) = @_;
    chop($symstring);
    $symstring =~ m/^symbols\( *([a-z0-9A-Z_]+) *, *\[(.*)\] *, *\[(.*)\] *\)\./ 
	or die "Bad symbols info: $symstring";
    my ($ref, $psyms, $fsyms) = ($1, $2, $3);
    my @psyms = split(/\,/, $psyms);
    my @fsyms = split(/\,/, $fsyms);
    my @allsyms = (@psyms, @fsyms);
    $grefsyms{$ref} = [];
    foreach my $sym (@allsyms)
    {

	$sym =~ m/^ *([^\/]+)[\/].*/ or die "Bad symbol $sym in $symstring";
	my $proper_sym = $1;
	push(@{$grefsyms{$ref}}, $proper_sym);
	if($proper_sym =~ m/^c[0-9]+.*/) { $glocal_consts_refs{$ref} = (); }
    }
    return $grefsyms{$ref};
}

sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }

# taken from mycgihol
sub GetRefs
{
    my ($syms, $limit) = @_;
    my ($msgin, @res1, @res);
    my $EOL = "\015\012";
    my $BLANK = $EOL x 2;
    my $remote = IO::Socket::INET->new( Proto     => "tcp",
					PeerAddr  => $advhost,
					PeerPort  => $aport,
				      );
    unless ($remote)
    {
	return ('DOWN');
#	    "The server is down, sorry\n";
#	$query->end_html unless($text_mode);
#	exit;
    }
    $remote->autoflush(1);
    print $remote join(",",@$syms) . "\n";
    $msgin = <$remote>;
    @res1  = split(/\,/, $msgin);
    close $remote;
    my $outnr = min($limit, 1 + $#res1);
    @res  = @res1[0 .. $outnr];
    return @res;
}





if($htmlize != 1)
{
    print $query->header;
    print $query->start_html("ATP Output");
}
else { print $query->header('text/xml');}

my $File0 = "$TemporaryDirectory/matp_" . $input_tmp . "/problems/" . 
    $input_article . "/" . $input_article . "__" . $line . "_";
my $File1 = $File0 . $col;
my $File2 = $File0 . $col1;
my $File;

if (-e $File1) { $File = $File1; } elsif(-e $File2) { $File = $File2}
if(    open(F,$File))
{
    if($htmlize != 1)    { print "<pre>"; }
    my $status = szs_UNKNOWN;
    my $spass_status = szs_UNKNOWN;
    if(defined $atp)
    {
	if($atp =~ m/^refs$/)
	{
	    my @refs=();

##--- Run SPASS 
	    if($spass == 1)
	    {
		my $spass_status_line =
		    `$SPASS  -Memory=900000000 -DocProof -PGiven=0 -PProblem=0 -TimeLimit=$cpulimit $File |  tee $File.sout | grep "SPASS beiseite"`;

		if ($spass_status_line=~m/.*SPASS beiseite *: *([^.]+)[.]/)
		{
		    $spass_status = $1;
		}
		else
		{
		    print "Bad SPASS status line: $spass_status_line, please complain";
		    $spass_status = szs_UNKNOWN;
		}

		if ($spass_status=~m/Proof found/)
		{
		    $spass_status = szs_THEOREM;
		    my $spass_formulae_line = `grep 'Formulae used in the proof' $File.sout`;
		    if($spass_formulae_line=~m/Formulae used in the proof *: *(.*) */) { @refs = split(/ +/, $1); }

		}
		else { $spass_status = szs_UNKNOWN; } 
		$status = $spass_status;
	    }

##--- Run Advisor
	    elsif($advice == 1)
	    {
		my $conj_syms_line = `grep conjecture $File | $Bindir/GetSymbols --`;
		my $conj_syms = ExtractSymbols($conj_syms_line);
		@refs = GetRefs($conj_syms, $advlimit);
		$status = szs_THEOREM;
	    }


##--- This is the default - EP
	    elsif($eprover == 1)
	    {
		my $eproof_pid = open(EP,"$eproof --print-statistics -xAuto -tAuto --cpu-limit=$cpulimit --memory-limit=Auto --tstp-in --tstp-out $File| tee $File.eout1 | grep -v '^#' | tee $File.eout | grep ',file('|") or die("bad eproof input file $File");
		#	    $proved_by{$conj} = [];

##--- read the needed axioms for proof
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
	    else
	    {
		print `pwd`;
		print "$runwtlimit $cpulimit $Bindir/vampire_rel2 -proof tptp -ss included -sd 1 -output_axiom_names on --mode casc -t 10 -m 1234  -input_file $File | tee $File.eout1 | grep '\bfile('|") or die("bad vampire input file $File";
		my $eproof_pid = open(EP,"$runwtlimit $cpulimit $Bindir/vampire_rel2 -proof tptp -ss included -sd 1 -output_axiom_names on --mode casc -t 10 -m 1234  -input_file $File | tee $File.eout1 | grep '\bfile('|") or die("bad vampire input file $File"); 


##--- read the needed axioms for proof
 		while ($_=<EP>)
 		{
		    m/.*\bfile\([^\),]+, *([a-z0-9A-Z_]+) *\)/ or die "bad proof line: $File: $_";
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
		    print "Bad vampire status line: $status_line, please complain";
		}
 		if (!($status eq szs_THEOREM)) { @refs = () }
	    }

##--- Process references if found - AJAX
	    if($#refs >= 0)
	    {
		if($htmlize == 1)
		{
		    print '<?xml version="1.0"?><div>';
		    if($advice == 1)
		    {
			print '<div class="box"><center><h4>Suggested hints</h4> ';
#			print 'Suggested hints';
		    }
		    else { print '<div class="box"><center><h4>ATP explanation</h4> '; }
		    if(($spass != 1) && ($advice != 1))
		    {
			print $query->a({href=>"$MyUrl/cgi-bin/tptp/RemoteSOT1.cgi?article=" .
					 $input_article . '&lc=' . $input_lc . '&tmp=' .
					 $input_tmp . '&idv=1'},
					$idv_img);
			print ', ';
		    }
		    if(($spass != 1) && ($advice != 1))
		    {
			print $query->a({href=>"$MyUrl/cgi-bin/showtmpfile.cgi?file=problems/" . 
					     $input_article . '/' . $input_article . '__' . $input_lc . '&tmp=' .
					     $input_tmp,
					     height=> "17",
					     width=> "17",
					title=>"Show TPTP problem"},
					$tptp_img);
			print ', ';
		    }
		    if(($spass != 1) && ($advice != 1))
		    {
			print $query->a({href=>"$MyUrl/cgi-bin/tptp/RemoteSOT1.cgi?article=" .
					     $input_article . '&lc=' . $input_lc . '&tmp=' .
					     $input_tmp . '&DM=1',
					     title=>"Try 20+ ATP systems in SystemOnTPTP"},
					"Export problem to SystemOnTPTP");
			print ', ';
		    }
		    if(($spass != 1) && ($advice != 1))
		    {
			print $query->a({href=>"$MyUrl/cgi-bin/showtmpfile.cgi?file=problems/" . 
					     $input_article . '/' . $input_article . '__' . $input_lc . '&tmp=' .
					     $input_tmp,
					     height=> "17",
					     width=> "17",
					title=>"Show TSTP proof"},
					$tstp_img);
			print ', ';
		    }
		    if(($spass != 1) && ($advice != 1))
		    {
			print $query->a({href=>"$MyUrl/cgi-bin/tptp/RemoteSOT1.cgi?article=" .
					 $input_article . '&lc=' . $input_lc . '&tmp=' .
					 $input_tmp . '&idv=2',
					title=>"Postprocess solution in SystemOnTSTP"},
					"Export solution to SystemOnTSTP");
			print ', ';
		    }
		    if(($spass != 1) && ($advice != 1))
		    {
			print ', ';
			print $query->a({href=>"$MyUrl/cgi-bin/tptp/MMLQuery.cgi?article=" .
					 $input_article . '&lc=' . $input_lc .
					 '&tmp=' . $input_tmp,
					 title=>"Translate ATP proof using MML Query (experimental)"},
					"MMLQuery (very experimental)");
		    }
		    print "<br>\n",'<h4>ATP Proof References</h4>' if($advice != 1);
#		    print $query->a({href=>"$MyUrl/cgi-bin/showby.cgi?article=" . $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp . '&DM=1'}, "Do more"), " ):<br>\n";

		    foreach my $ref (@refs)
		    {
			my ($href, $title) = HTMLize($ref);
			if(length($href)>0)
			{
			    if(length($title)>0) 
			    {
				print $query->a({href=>$href,title=>$title}, $ref),", ";
			    }
			    else
			    {
				print $query->font({color=>"Green",title=>$title}, $ref),", ";
			    }
			}
			elsif(length($title)>0) 
			{
			    print $query->font({color=>"Green",title=>$title}, $ref),", ";
			}
			else {print $ref,", ";}
		    }
#		    if($advice != 1) { print "</center><br/></div>"; }
		    print "</center><br/></div>";
		    print "</div>";
		}
		else { print join(",", @refs);}
	    }

	    else
	    {
		print '<div class="box"><center><h4>ATP Proof not found</h4> ',
		"status: $status", '<br/>';
#		print "ATP Proof not found (status: $status, ";
		if(($spass != 1) && ($advice != 1))
		{
		    print $query->a({class=>"txt",
				     onclick=>"makeRequest(this,\'$MyUrl/cgi-bin/showby.cgi?article=" .
				     $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp .
				     '&ap=' . $aport . '&ATP=refs&HTML=1&advice=1\')',
				     href=>'javascript:()',
				     title=>"Suggest relevant references for proving this"},
				    'Suggest hints, ');
		    print    '<span> </span>';
		    print $query->a({class=>"txt",
				     onclick=>"makeRequest(this,\'$MyUrl/cgi-bin/showby.cgi?article=" .
				     $input_article . '&lc=' . $input_lc . '&tmp=' . $input_tmp .
				     '&ap=' . $aport . '&ATP=refs&HTML=1&spass=1\')',
				     href=>'javascript:()',
				     title=>"Try the SPASS ATP system"},
				    'Try SPASS, ');
		    print    '<span> </span>';
		    print $query->a({href=>"$MyUrl/cgi-bin/tptp/RemoteSOT1.cgi?article=" .
				     $input_article . '&lc=' . $input_lc .
				     '&tmp=' . $input_tmp . '&DM=1',
				     title=>"Try 20+ ATP systems in SystemOnTPTP"},
				    "Export problem to SystemOnTPTP");
		}
		print "</center><br/></div>"; 
#		print " ):<br>\n";
	    }
	}
	else
	{
	    system("$eproof --print-statistics -xAuto -tAuto --cpu-limit=$cpulimit --memory-limit=Auto --tstp-in --tstp-out $File");
	}
    }

    else { local $/; $_= <F>; print $_; }
    if($htmlize != 1) { print "<pre/>";    print $query->end_html;}
    close(F);
}
else
{
    print "You just hit a line numbering bug, please complain";
}
