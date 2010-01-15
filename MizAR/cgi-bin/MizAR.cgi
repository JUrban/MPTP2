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

# my $MyUrl = 'http://octopi.mizar.org/~mptp';
my $MyUrl = 'http://mws.cs.ru.nl/~mptp';
my $PalmTreeUrl = $MyUrl . "/PalmTree.jpg";
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $Mizfiles = "/home/mptp/public_html/mml";
my $utilspl =  "/home/mptp/public_html/cgi-bin/bin/utils.pl";
my $TemporaryDirectory = "/tmp";
my $TemporaryProblemDirectory = "$TemporaryDirectory/matp_$$";
my $PidNr = $$;
my $MizHtml = $MyUrl . "/mml/html/";
my $mizf =     "bin/mizf";
my $snow =     "bin/snow";
my $advisor =     "bin/advisor.pl";
my $exporter =     "bin/mizar/exporter";
my $xsltproc =     "bin/xsltproc";
my $dbenv = "bin/dbenv.pl";
my $err2pl = "bin/err2pl.pl";
my $err2xml = "bin/err2xml.pl";
my $mizitemize = "bin/MizItemize.pl";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $miz2html = "$Xsl4MizarDir/miz.xsl";
my $mizpl = "$Xsl4MizarDir/mizpl.xsl";
my $doatproof = 0;
my $atproof = '@' . 'proof';
my $idv_img = "<img SRC=\"$PalmTreeUrl\" alt=\"Show IDV proof tree\" title=\"Show IDV proof tree\">";


my $query	  = new CGI;
my $ProblemSource = $query->param('ProblemSource');
my $VocFile       = $query->param('VocFile');
my $VocSource       = $query->param('VocSource');
my $input_article	  = $query->param('Formula');
my $input_name	  = $query->param('Name');
my $atp_mode	  = $query->param('ATPMode');
my $input_snow	  = $query->param('Snow');
my $linkarproofs  = $query->param('ARProofs');
my $aname         = lc($input_name); 
my $aname_uc      = uc($aname);
my $ProblemFileOrig = $TemporaryProblemDirectory . "/$aname";
my $AjaxProofDir = $TemporaryProblemDirectory . "/proofs/" . $aname;
my $ProblemDir = $TemporaryProblemDirectory . "/problems/" . $aname;
my $ProblemFile = $ProblemFileOrig . ".miz";
my $ProblemFileXml = $ProblemFileOrig . ".xml";
my $ProblemFileXml2 = $ProblemFileOrig . ".xml2";
my $ProblemFileHtml = $ProblemFileOrig . ".html";
my $ProblemFileDco = $ProblemFileOrig . ".dco";
my $ProblemFileDco1 = $ProblemFileOrig . ".dco1";
my $ProblemFileDco2 = $ProblemFileOrig . ".dco2";
my $ProblemFileErr = $ProblemFileOrig . ".err";
my $ProblemFileErr1 = $ProblemFileOrig . ".err1";
my $ProblemFileErr2 = $ProblemFileOrig . ".err2";
my $ProblemFileErrX = $ProblemFileOrig . ".errx";

my $MizOutput = $ProblemFileOrig . ".mizoutput";
my $ExpOutput = $ProblemFileOrig . ".expoutput";
my $ProblemFileBex = $ProblemFileOrig . ".bex";
my $lbytmpdir = $PidNr;
my $lbycgiparams = '\&ATP=refs\&HTML=1';
my $lbytptpcgi= $MyUrl . '/cgi-bin/showby.cgi';

my $SnowDataDir =     $Mizfiles . "/mptp/snowdata";
my $SnowFileStem =    $SnowDataDir . "/thms3";
my $SnowMMLNet =      $SnowFileStem . ".net";
my $SnowMMLArch =     $SnowFileStem . ".arch";
my $AdvisorOutput  =  $ProblemFileOrig . ".adv_output";
my $SnowOutput  =     $ProblemFileOrig . ".snow_output";
my $SnowSymOffset =   500000;

my $text_mode     = $query->param('Text');
my (%gsyms,$grefs,$ref);
my $ghost	  = "localhost";
my $snowport	  = -1;
my $start_snow    = 1;
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


$linkarproofs = 0 unless defined($linkarproofs);

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
unless($text_mode)
{
    print $query->start_html($aname_uc);

    CheckArticleName($aname);

    CreateTmpDirs($TemporaryProblemDirectory);
    CreateTmpDir($AjaxProofDir);
    CreateTmpDir($ProblemDir);

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

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.mizoutput&tmp=$PidNr\" target=\"MizarOutput$PidNr\">Show Mizar Output</a>\n";
#    print $AjaxProofDir;

    my $errorsnr = `wc -l <$ProblemFileErr`;

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.err1&tmp=$PidNr\" target=\"MizarOutput$PidNr\">($errorsnr Errors)</a>\n";

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.ploutput&tmp=$PidNr&refresh=1\" target=\"MPTPOutput$PidNr\">Generating $InferenceNr TPTP problems (click to see progress)</a><br>\n";

#    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.xml.abs&tmp=$PidNr&content-type=text%2Fplain\" target=\"XMLOutput$PidNr\">Show XML Output</a>\n";


    my $lnr = 0;
    open(PFH, "$ProblemFile");

    print ('<pre>', "\n");
    while( my $aline = <PFH>)
    {
	print ('<div id="', ++$lnr, '" style="display:none">', $aline, '</div>');
    }
    close(PFH);
    print ('</pre>', "\n");



    SortByExplanations($ProblemFileBex);


#    system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml > $ProblemFileXml.abs 2>$ProblemFileXml.errabs");

# ###TODO: note that const_links=2 does not work correctly yet    




    system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml 2>$ProblemFileXml.errabs > $ProblemFileXml.abs");

    system("time $xsltproc --param by_titles 1 --param const_links 1 --param linkarproofs $linkarproofs --param ajax_by 1 --param linkbytoself 1 --param linkby 3 --param thms_tptp_links 1 --param lbytptpcgi \\\'$lbytptpcgi\\\' --param lbytmpdir \\\'$lbytmpdir\\\' --param lbycgiparams \\\'$lbycgiparams\\\' --param default_target \\\'_self\\\'  --param linking \\\'l\\\' --param mizhtml \\\'$MizHtml\\\' --param selfext \\\'html\\\'  --param titles 1 --param colored 1 --param proof_links 1 $miz2html $ProblemFileXml.abs |tee $ProblemFileHtml 2>$ProblemFileXml.errhtml"); 


    system("time $xsltproc $mizpl $ProblemFileXml.abs  > $ProblemFileXml2 2>$ProblemFileXml.errpl");

# ajax proofs are probably not wanted for the first stab
#    system("time $xsltproc --param default_target \\\'_self\\\' --param ajax_proof_dir \\\'$AjaxProofDir\\\' --param linking \\\'l\\\' --param mizhtml \\\'$MizHtml\\\' --param selfext \\\'html\\\' --param ajax_proofs 1 --param titles 1 --param colored 1 --param proof_links 1 $miz2html $ProblemFileXml.abs > $ProblemFileHtml 2>$ProblemFileXml.errhtml"); 


## this is a hack to pass the refs ATP param
#    system("time $xsltproc $mizpl $ProblemFileXml.abs > $ProblemFileXml2 2>$ProblemFileXml.errpl");
    my ($Dcl, $The, $Sch, $Lem, $Evl) = ($ProblemFileOrig . '.dcl2', $ProblemFileOrig . '.the2', 
					 $ProblemFileOrig . '.sch2', $ProblemFileOrig . '.lem2', $ProblemFileOrig . '.evl2'); 
## create the derived files
    open(DCL, ">$Dcl"); open(THE, ">$The"); open(SCH, ">$Sch"); open(LEM, ">$Lem");
    open(XML2, $ProblemFileXml2);
    while($_=<XML2>)
    {
	if(m/^fof.[dt][0-9]/) { print THE $_; }
	elsif(m/^fof.(([fcr]c)|(ie))[0-9]/) { print DCL $_; }
	elsif(m/^fof.[s][0-9]/) { print SCH $_; }
	elsif(m/^fof.[l][0-9]/) { print LEM $_; }
    }
    close(DCL); close(THE); close(SCH); close(LEM);
    close(XML2);

    system("$dbenv $ProblemFileOrig > $Evl");
    system("$exporter -q $ProblemFile 2>&1 > $ExpOutput");
    system("time $xsltproc --param aname \\\'$aname_uc\\\' --param explainbyfrom 1 $addabsrefs $ProblemFileDco > $ProblemFileDco1  2>$ProblemFileDco.err");
    system("time $xsltproc --param mml 1 $mizpl $ProblemFileDco1 > $ProblemFileDco2 2>$ProblemFileDco.err2");

    my $Tmp1 = $TemporaryProblemDirectory . '/';
# swipl -G50M -s utils.pl -g "mptp2tptp('$1',[opt_NO_FRAENKEL_CONST_GEN],user),halt." |& grep "^fof"
    system("cd $TemporaryProblemDirectory; swipl -G50M -s $utilspl -g \"(A=$aname,D=\'$Tmp1\',declare_mptp_predicates,time(load_mml_for_article(A, D, [A])),time(install_index),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma]],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_ADDED_NON_MML([A]),opt_NON_MML_DIR(D),opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS,opt_ALLOWED_REF_INFO,opt_PROVED_BY_INFO])),halt).\" > $aname.ploutput 2>&1");


#    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.ploutput&tmp=$PidNr\" target=\"MPTPOutput$PidNr\">Show MPTP Output</a><br>\n";




# A=m_drxj,D='/tmp/matp_704/',load_mml_for_article(A, D, [A]),install_index,mk_article_problems(A,[[mizar_by],[theorem, top_level_lemma, sublemma]],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_ADDED_NON_MML([A]),opt_NON_MML_DIR(D),opt_LINE_COL_NMS]).

#    print "<pre>";
#    open(F,$ProblemFile);
#    open(F,$ProblemFileHtml);
#    { local $/; $_= <F>; print $_; }
#    print "<pre/>";
#    print $query->end_html;
#    close(F);
}

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
	$query->end_html unless($text_mode);
	exit;
    }
    $remote->autoflush(1);
    print $remote join(",",(keys %$syms)) . "\n";
    $msgin = <$remote>;
    @res  = split(/\,/, $msgin);
    close $remote;
    return \@res;
}
