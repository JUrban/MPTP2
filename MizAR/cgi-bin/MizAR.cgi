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

my $MyUrl = 'http://octopi.mizar.org/~mptp';
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $Mizfiles = "/home/mptp/public_html/mml";
my $utilspl =  "/home/mptp/public_html/cgi-bin/bin/utils.pl";
my $TemporaryDirectory = "/tmp";
my $TemporaryProblemDirectory = "$TemporaryDirectory/matp_$$";
my $PidNr = $$;
my $MizHtml = $MyUrl . "/mml/html/";
my $mizf =     "bin/mizf";
my $exporter =     "bin/mizar/exporter";
my $xsltproc =     "bin/xsltproc";
my $dbenv = "bin/dbenv.pl";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $miz2html = "$Xsl4MizarDir/miz.xsl";
my $mizpl = "$Xsl4MizarDir/mizpl.xsl";
my $doatproof = 0;
my $atproof = '@' . 'proof';

my $query	  = new CGI;
my $ProblemSource = $query->param('ProblemSource');
my $VocFile       = $query->param('VocFile');
my $VocSource       = $query->param('VocSource');
my $input_article	  = $query->param('Formula');
my $input_name	  = $query->param('Name');
my $atp_mode	  = $query->param('ATPMode');
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
my $MizOutput = $ProblemFileOrig . ".mizoutput";
my $ExpOutput = $ProblemFileOrig . ".expoutput";
my $ProblemFileBex = $ProblemFileOrig . ".bex";
my $lbytmpdir = $PidNr . '\&ATP=refs\&HTML=1';
my $lbytptpcgi= $MyUrl . '/cgi-bin/showby.cgi';



my $text_mode     = $query->param('Text');
my (%gsyms,$grefs,$ref);
my $ghost	  = "localhost";
my $gport	  = "60000";
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

print $query->header;
unless($text_mode)
{
    print $query->start_html("MizAR Output");

    CheckArticleName($aname);

    CreateTmpDirs($TemporaryProblemDirectory);
    CreateTmpDir($AjaxProofDir);
    CreateTmpDir($ProblemDir);

    SetupArticleFiles();

    $ENV{"MIZFILES"}= $Mizfiles;
    system("$mizf $ProblemFile 2>&1 > $MizOutput");

    my $InferenceNr = `egrep -c '<(Proof|By|From|Now)' $ProblemFileXml`;

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.mizoutput&tmp=$PidNr\" target=\"MizarOutput$PidNr\">Show Mizar Output</a>\n";
#    print $AjaxProofDir;

    SortByExplanations($ProblemFileBex);


#    system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml > $ProblemFileXml.abs 2>$ProblemFileXml.errabs");

# ###TODO: note that const_links=2 does not work correctly yet    




    system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml 2>$ProblemFileXml.errabs |tee $ProblemFileXml.abs"); 

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.ploutput&tmp=$PidNr&refresh=1\" target=\"MPTPOutput$PidNr\">Generating $InferenceNr TPTP problems (click to see progress)</a><br>\n";

    system("time $xsltproc --param by_titles 1 --param const_links 1 --param ajax_by 1 --param linkbytoself 1 --param linkby 3 --param lbytptpcgi \\\'$lbytptpcgi\\\' --param lbytmpdir \\\'$lbytmpdir\\\' --param default_target \\\'_self\\\'  --param linking \\\'l\\\' --param mizhtml \\\'$MizHtml\\\' --param selfext \\\'html\\\'  --param titles 1 --param colored 1 --param proof_links 1 $miz2html $ProblemFileXml.abs |tee $ProblemFileHtml 2>$ProblemFileXml.errhtml"); 


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
    system("cd $TemporaryProblemDirectory; swipl -G50M -s $utilspl -g \"(A=$aname,D=\'$Tmp1\',declare_mptp_predicates,time(load_mml_for_article(A, D, [A])),time(install_index),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma]],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_ADDED_NON_MML([A]),opt_NON_MML_DIR(D),opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS])),halt).\" > $aname.ploutput 2>&1");

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
					PeerPort  => $gport,
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
