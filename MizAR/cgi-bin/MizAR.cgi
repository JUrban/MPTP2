#!/usr/bin/perl -w 

# cgi preparing mizar article for atp methods
# crude initial hack

use strict;
use CGI;
use IO::Socket;
use File::Temp qw/ :mktemp  /;
use IPC::Open2;
use HTTP::Request::Common;
use LWP::Simple;


my $query	  = new CGI;
my $ProblemSource = $query->param('ProblemSource');
my $VocFile       = $query->param('VocFile');
my $VocSource       = $query->param('VocSource');
my $input_article	  = $query->param('Formula');
my $input_name	  = $query->param('Name');
my $atp_mode	  = $query->param('ATPMode');
my $input_snow	  = $query->param('Snow');
my $linkarproofs  = $query->param('ARProofs');
# if not defined, the htmlized article is not produced, only article with errors is shown
my $generatehtml   = $query->param('HTMLize');
my $generateatp   = $query->param('GenATP');
# immediatelly try to prove all unsolved (*4), using all of MML
my $proveunsolved   = $query->param('ProveUnsolved');
my $provepositions  = $query->param('Positions');

# if defined || 1, proofs are hidden by default and shown by ajax calls (undef by default)
my $proofsbyajax  = $query->param('AjaxProofs');

# the mml version used for processing and linking; there should be a suitable default
my $mmlversion    = $query->param('MMLVersion');

# either 'HTML' or 'TEXT'; says if output is in txt or in html
my $query_mode       = $query->param('MODE');


$linkarproofs = 0 unless defined($linkarproofs);
$generateatp = 0 unless defined($generateatp);
$generatehtml = 0 unless defined($generatehtml);
$proveunsolved = "None" unless defined($proveunsolved);

$mmlversion   = '4.100.1011' unless defined($mmlversion);
$proofsbyajax = 0; # unless defined($proofsbyajax); comented - not wroking yet, trying to write the relative proof path

my @provepositions = ();
if (($proveunsolved eq "Positions") && (defined($provepositions)))
{
    @provepositions = split(/\, */, $provepositions);
}

# my $MyUrl = 'http://octopi.mizar.org/~mptp';
my $MyUrl = 'http://mws.cs.ru.nl/~mptp';
my $PalmTreeUrl = $MyUrl . "/PalmTree.jpg";
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $Mizfiles = "/home/mptp/public_html/mml$mmlversion";
my $Bindir = "bin$mmlversion";
my $utilspl =  "/home/mptp/public_html/cgi-bin/$Bindir/utils.pl";
my $TemporaryDirectory = "/tmp";
my $TemporaryProblemDirectory = "$TemporaryDirectory/matp_$$";
my $PidNr = $$;
my $MizHtml = $MyUrl . "/mml$mmlversion/html/";
my $mizf =     "$Bindir/mizf";
my $snow =     "$Bindir/snow";
my $advisor =     "$Bindir/advisor.pl";
my $exporter =     "$Bindir/mizar/exporter";
my $xsltproc =     "$Bindir/xsltproc";
my $dbenv2 = "$Bindir/dbenv2.pl";
my $mk_derived = "$Bindir/mk_derived.pl";
my $err2pl = "$Bindir/err2pl.pl";
my $err2xml = "$Bindir/err2xml.pl";
my $mizitemize = "$Bindir/MizItemize.pl";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $miz2html = "$Xsl4MizarDir/miz.xsl";
my $mizpl = "$Xsl4MizarDir/mizpl.xsl";
my $evl2pl = "$Xsl4MizarDir/evl2pl.xsl";
my $doatproof = 0;
my $atproof = '@' . 'proof';
my $idv_img = "<img SRC=\"$PalmTreeUrl\" alt=\"Show IDV proof tree\" title=\"Show IDV proof tree\">";



my $aname         = lc($input_name); 
my $aname_uc      = uc($aname);
my $ProblemFileOrig = $TemporaryProblemDirectory . "/$aname";
my $AjaxProofDir = $TemporaryProblemDirectory . "/proofs"; #/" . $aname;
my $ProblemDir = $TemporaryProblemDirectory . "/problems/" . $aname;
my $ProblemFile = $ProblemFileOrig . ".miz";
my $ProblemFileXml = $ProblemFileOrig . ".xml";
my $ProblemFileXml2 = $ProblemFileOrig . ".xml2";
my $ProblemFileHtml = $ProblemFileOrig . ".html";
my $ProblemFileErr = $ProblemFileOrig . ".err";
my $ProblemFileErr1 = $ProblemFileOrig . ".err1";
my $ProblemFileErr2 = $ProblemFileOrig . ".err2";
my $ProblemFileErrX = $ProblemFileOrig . ".errx";
my $ProblemCgiParams = $ProblemFileOrig . ".cgiparams";

my $MizOutput = $ProblemFileOrig . ".mizoutput";
my $ProblemFileBex = $ProblemFileOrig . ".bex";
my $lbytmpdir = $PidNr;
my $lbycgiparams = '\&ATP=refs\&HTML=1\&MMLVersion=' . $mmlversion;
my $lbytptpcgi= $MyUrl . "/cgi-bin/$Bindir/showby.cgi";

my $SnowDataDir =     $Mizfiles . "/mptp/snowdata";
my $SnowFileStem =    $SnowDataDir . "/thms3";
my $SnowMMLNet =      $SnowFileStem . ".net";
my $SnowMMLArch =     $SnowFileStem . ".arch";
my $AdvisorOutput  =  $ProblemFileOrig . ".adv_output";
my $SnowOutput  =     $ProblemFileOrig . ".snow_output";
my $SnowSymOffset =   500000;

my (%gsyms,$grefs,$ref);
my $ghost	  = "localhost";
my $snowport	  = -1;
my $start_snow    = $generateatp;
my $advisorport	  = -1;
my %gconstrs      =
    (
     'func'   , 'k',
     'pred'   , 'r',
     'attr'   , 'v',
     'mode'   , 'm',
     'aggr'   , 'g',
     'sel'    , 'u',
     'struct' , 'l'
    );


sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }

#----Check for proper article name
sub CheckArticleName
{
    my ($aname1) = @_;
    unless((length($aname1) < 9) && ($aname1=~m/^[a-z][a-z0-9_]*$/))
    {
	print "<pre>";
	print ("Error: article name must not exceed 8 characters, has to start with a-z, and only contain [a-z0-9_]: ", $aname1);
	print "<pre/>";
	print $query->end_html;
	die "article name";
    }
}

sub CreateTmpDir
{
    my ($tmpdir) = @_;

    if (!mkdir($tmpdir,0777)) {
        print("ERROR: Cannot make temp dir $tmpdir\n");
        die("\n");
    }

    system("chmod 0777 $tmpdir");
}
# Make temporary directories for files;
# creates the ( 'proofs', 'problems', 'dict') subdirs
# Change permissions so tptp can cleanup later if needed
sub CreateTmpDirs
{
    my ($tmproot) = @_;

    CreateTmpDir($tmproot);

    foreach my $subdir ( 'proofs', 'problems', 'dict')
    {
	CreateTmpDir($tmproot . "/". $subdir)
    }

## nasty hack to get around the dict/text issues 
##    system("ln -s $TemporaryProblemDirectory $TemporaryProblemDirectory/text");

#DEBUG print("----$TemporaryProblemDirectory----$!---\n");

}

sub LogCGIParams
{
    open (CGIPARAMS,'>',$ProblemCgiParams) || die;
    $query->save(\*CGIPARAMS);
    close CGIPARAMS;
}

# Print input article into $ProblemFile;
# handle vocabulary, convert to unix, handle atproof.
# Uses lots of global vars now.
sub SetupArticleFiles
{


#    my $ProblemFileTxt = "${TemporaryProblemDirectory}/text/$aname";
    open(PFH, ">$ProblemFileOrig") or die "$ProblemFileOrig not writable";
#    my ($ProblemFileHandle,$ProblemFileOrig) = mkstemp("${TemporaryProblemDirectory}/$aname");
    if ($ProblemSource eq "UPLOAD")
    {
	my $UploadFileHandle = $query->param('UPLOADProblem');
	if (!defined($UploadFileHandle) || $UploadFileHandle eq "")
	{
	    print("ERROR: Empty uploaded problem file\n");
	    die("ERROR: Empty uploaded problem file\n");
	}
	my $UploadLine;
#DEBUG print("UPLOAD file: $UploadFileHandle \n");
	while (defined($UploadLine = <$UploadFileHandle>)) { print PFH $UploadLine; };
	close($UploadFileHandle);
    }
    elsif ($ProblemSource eq "URL") 
    {
	my $FormulaURL = $query->param('FormulaURL');
	if (!defined($FormulaURL)) 
	{
	    die("ERROR: No URL supplied\n");
	}
	getstore($FormulaURL,$ProblemFileOrig) or
	    die("ERROR: Could not fetch from $FormulaURL");
    }
    elsif (!($input_article eq ""))
    {
#----Convert \r (DOS EOLN) to \n (UNIX EOLN)
	if($doatproof > 0) 
	{
	    $input_article =~ s/\bproof\b/$atproof/g;
	}
#    $input_article =~ s/\r/\n/g;
#----Somehow WWW services are duplicating the \n ... kill them (aargh)
#            $Formulae =~ s/\n\n/\n/g;
	printf(PFH "%s",$input_article);
    }
    else
    {
	print("ERROR: No article provided\n"); 
	die("ERROR: No article provided\n"); 
    }

    close(PFH);

    if (defined($VocSource) && ($VocSource eq 'UPLOAD')
	&& defined($VocFile) && !($VocFile eq ""))
    {
	my $VOCFileOrig1 = "${TemporaryProblemDirectory}/dict/$VocFile";
	open(VOC, ">$VOCFileOrig1") or die "$VOCFileOrig1 not writable";
	my $UploadLine;
#DEBUG print("UPLOAD file: $VocFile \n");
	while (defined($UploadLine = <$VocFile>)) { print VOC $UploadLine; };
	close($VocFile);
	close(VOC)
    }

    system("dos2unix $ProblemFileOrig");
    `mv $ProblemFileOrig $ProblemFile`;
    system("chmod 0666 $ProblemFile");
}

#  sort the .bex file
sub SortByExplanations
{
    my ($Bex) = @_;

    if(open(BEX,$Bex))
    {
	local $/;
	my $bex=<BEX>;
	close(BEX);
	if($bex=~m/((.|[\n])*?)<PolyEval/)
	{
	    open(BEX,">$Bex");
	    print BEX $1;
	    my %h=();
	    while($bex=~m/(<PolyEval((.|[\n])*?)<\/PolyEval>)/g)
	    {
		if(!(exists $h{$1})) { print BEX $1; $h{$1} = (); }
	    }
	    print BEX "</ByExplanations>\n";
	    close(BEX);
	}
    }
}

sub StartSNoW
{
#--- get unused port for SNoW
    socket(SOCK,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK,  sockaddr_in(0, INADDR_ANY));
    my $sport = (sockaddr_in(getsockname(SOCK)))[0];
#    print("snowport $sport\n");
    close(SOCK);

#--- start snow instance:
# ###TODO: wrap this in a script remembering a start time, and self-destructing
#          in one day
    system("nohup $snow -server $sport -o allboth -F $SnowMMLNet -A $SnowMMLArch > $SnowOutput 2>&1 &");

#--- get unused port for advisor
    socket(SOCK1,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK1,  sockaddr_in(0, INADDR_ANY));
    my $aport = (sockaddr_in(getsockname(SOCK1)))[0];
#    print("advisorport $aport\n");
    close(SOCK1);

    system("nohup $advisor -p $sport -a $aport -o $SnowSymOffset $SnowFileStem > $AdvisorOutput 2>&1 &");

    $lbycgiparams = $lbycgiparams . '\&ap=' . $aport;
    return ($aport, $sport);
}


print $query->header;
print $query->start_html($aname_uc) unless($query_mode eq 'TEXT');



CheckArticleName($aname);

CreateTmpDirs($TemporaryProblemDirectory);
CreateTmpDir("$AjaxProofDir/$aname");
CreateTmpDir($ProblemDir);

LogCGIParams();
SetupArticleFiles();

    # this has to precede creation of html, so that $aport
    # was set in the by-calls
if($start_snow > 0) { ($advisorport, $snowport) = StartSNoW(); }

$ENV{"MIZFILES"}= $Mizfiles;
system("$mizf $ProblemFile 2>&1 > $MizOutput");

system("cp $ProblemFileErr $ProblemFileErr1");
system("$err2pl $ProblemFileErr > $ProblemFileErr2");
system("$err2xml $ProblemFileErr > $ProblemFileErrX");
system("$mizitemize $ProblemFileOrig");

my $InferenceNr = `egrep -c '<(Proof|By|From|Now)' $ProblemFileXml`;
my $errorsnr = `wc -l <$ProblemFileErr`;

unless($query_mode eq 'TEXT')
{
    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.mizoutput&tmp=$PidNr\" target=\"MizarOutput$PidNr\">Show Mizar Output</a>\n";  
#    print $AjaxProofDir;

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.err1&tmp=$PidNr\" target=\"MizarOutput$PidNr\">($errorsnr Errors)</a>\n";

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.ploutput&tmp=$PidNr&refresh=1\" target=\"MPTPOutput$PidNr\">Generating $InferenceNr TPTP problems (click to see progress)</a><br>\n" if($generateatp > 0);
    
#    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.xml.abs&tmp=$PidNr&content-type=text%2Fplain\" target=\"XMLOutput$PidNr\">Show XML Output</a>\n";


    my $lnr = 0;
    open(PFH, "$ProblemFile");

    print ('<pre>', "\n");
    while( my $aline = <PFH>)
    {
	if($generatehtml==1)
	{
	    print ('<div id="', ++$lnr, '" style="display:none">', $aline, '</div>');
	}
	else { print $aline; }
    }
    close(PFH);
    print ('</pre>', "\n");
}

SortByExplanations($ProblemFileBex);


#    system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml > $ProblemFileXml.abs 2>$ProblemFileXml.errabs");

# ###TODO: note that const_links=2 does not work correctly yet    

system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml 2>$ProblemFileXml.errabs > $ProblemFileXml.abs") if(($generatehtml==1) || ($generateatp==1));

if($query_mode eq 'TEXT')
{
    open(F,$ProblemFileErr);
    local $/; $_= <F>; print $_;
    close(F);
}
else
{
    my $genatpparams = ($generateatp==1)? " --param by_titles 1 --param linkarproofs $linkarproofs --param ajax_by 1 --param linkbytoself 1 --param linkby 3 --param thms_tptp_links 1 --param lbytptpcgi \\\'$lbytptpcgi\\\' --param lbytmpdir \\\'$lbytmpdir\\\' --param lbycgiparams \\\'$lbycgiparams\\\' " : "";

    my $ajaxproofparams = ($proofsbyajax==1)? " -param lbytmpdir \\\'$lbytmpdir\\\'  --param ajax_proofs 3 " : " ";

    system("time $xsltproc  $genatpparams $ajaxproofparams --param const_links 1  --param default_target \\\'_self\\\'  --param linking \\\'l\\\' --param mizhtml \\\'$MizHtml\\\' --param selfext \\\'html\\\'  --param titles 1 --param colored 1 --param proof_links 1 $miz2html $ProblemFileXml.abs |tee $ProblemFileHtml 2>$ProblemFileXml.errhtml") if($generatehtml==1); 
}
 
if($generateatp > 0) 
{
    system("time $xsltproc $mizpl $ProblemFileXml.abs  > $ProblemFileXml2 2>$ProblemFileXml.errpl");
    

# ajax proofs are probably not wanted for the first stab
#    system("time $xsltproc --param default_target \\\'_self\\\' --param ajax_proof_dir \\\'$AjaxProofDir\\\' --param linking \\\'l\\\' --param mizhtml \\\'$MizHtml\\\' --param selfext \\\'html\\\' --param ajax_proofs 1 --param titles 1 --param colored 1 --param proof_links 1 $miz2html $ProblemFileXml.abs > $ProblemFileHtml 2>$ProblemFileXml.errhtml"); 

    system("$mk_derived $ProblemFileOrig 2> $ProblemFileOrig.derived_err");
    system("$xsltproc $evl2pl $ProblemFileOrig.evl   > $ProblemFileOrig.evl1");
    system("$dbenv2 $ProblemFileOrig > $ProblemFileOrig.evl2");

    my $Tmp1 = $TemporaryProblemDirectory . '/';
# swipl -G50M -s utils.pl -g "mptp2tptp('$1',[opt_NO_FRAENKEL_CONST_GEN],user),halt." |& grep "^fof"
    system("cd $TemporaryProblemDirectory; swipl -G50M -s $utilspl -g \"(A=$aname,D=\'$Tmp1\',declare_mptp_predicates,time(load_mml_for_article(A, D, [A])),time(install_index),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma]],[opt_ARTICLE_AS_TPTP_AXS,opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_ADDED_NON_MML([A]),opt_NON_MML_DIR(D),opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS,opt_ALLOWED_REF_INFO,opt_PROVED_BY_INFO])),halt).\" > $aname.ploutput 2>&1");
}

# A=m_drxj,D='/tmp/matp_704/',load_mml_for_article(A, D, [A]),install_index,mk_article_problems(A,[[mizar_by],[theorem, top_level_lemma, sublemma]],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_ADDED_NON_MML([A]),opt_NON_MML_DIR(D),opt_LINE_COL_NMS]).

#    print "<pre>";
#    open(F,$ProblemFile);
#    open(F,$ProblemFileHtml);
#    { local $/; $_= <F>; print $_; }
#    print "<pre/>";
#    print $query->end_html;
#    close(F);


# if((length($input_article) < 1)
#    or ($input_limit < 1)
#    or (0 == GetQuerySymbols($input_article, \%gsyms)))
# {
#     print "Insufficient input\n";
#     $query->end_html unless($text_mode);
#     exit;
# }

# $grefs = GetRefs(\%gsyms, $input_limit);
# if($#{ @$grefs} < 1)
# {
#     print "Input contained no known constructors, no advice given\n";
#     $query->end_html unless($text_mode);
#     exit;
# }


# my $i = -1;
# my $outnr = min($input_limit, 1+$#{ @$grefs});

# unless($text_mode)
# {
#     print "<pre>";
#     print $query->h2("References sorted by expected importance");
# }

# my $megrezurl = "http://megrez.mizar.org/cgi-bin/meaning.cgi";
# while(++$i < $outnr)
# {
#     my $ref = $grefs->[$i];
#     my ($kind, $nr, $an);
# #    MPTP-like constructors commented, we now expect Query-like format
# #    $ref=~/^([td])([0-9]+)_(.*)/ or die "Bad reference $ref\n";
# #    ($kind, $nr, $an) = ($1, $2, $3);
# #    $kind = ($kind eq "t")? "th" : "def";
#     $ref=~/^([A-Z0-9_]+):(th|def|sch) (\d+)/ or die "Bad reference $ref\n";
#     ($an, $kind, $nr) = ($1, $2, $3);

#     if($text_mode)
#     {
# #	my $nkind = ($kind eq "def")?"def ":"";
# 	my $nkind = ($kind eq "th")? "": $kind . " ";
# 	print uc($an) . ":" . $nkind . $nr . "\n";
#     }
#     else
#     {
# 	print "<a href=\"".$megrezurl."?article=".$an."&kind=".$kind
# 	    ."&number=".$nr."\" target=entry>".uc($an).":".$kind." "
# 		.$nr."</a>\n";
#     }
# }

# unless($text_mode)
# {
#     print "<pre/>";
#     print $query->end_html;
# }


#----Old advisor stuff (unused now)

# returns nr. of syms with repetitions
sub GetQuerySymbols
{
    my ($fla, $syms) = @_;
    my $res = 0;

    while($fla =~ /\b([0-9A-Z_]+):(func|pred|attr|mode|aggr|sel|struct)[ .]([0-9]+)/g)
    {
#    MPTP-like constructors commented, we now expect Query-like format
#	my $aname	= lc($1);
#	my $sname	= $gconstrs{$2}."$3"."_".$aname;

        my $sname	= $1.":".$2." ".$3;
	$syms->{$sname}	= ();	# counts can be here later
	$res++;
    }
    return $res;
}


# limit not used here yet
sub GetRefs
{
    my ($syms, $limit) = @_;
    my ($msgin, @res);
    my $EOL = "\015\012";
    my $BLANK = $EOL x 2;
    my $remote = IO::Socket::INET->new( Proto     => "tcp",
					PeerAddr  => $ghost,
					PeerPort  => $advisorport,
				      );
    unless ($remote)
    {
	print "The server is down, sorry\n";
	$query->end_html unless($query_mode eq 'TEXT');
	exit;
    }
    $remote->autoflush(1);
    print $remote join(",",(keys %$syms)) . "\n";
    $msgin = <$remote>;
    @res  = split(/\,/, $msgin);
    close $remote;
    return \@res;
}
