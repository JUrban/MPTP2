#!/usr/bin/perl -w

=head1 NAME

mizp.pl file ( parallelize Mizar verification, return the errors file)

=head1 SYNOPSIS

mizp.pl -j16 ~/a

 Options:
   --parallelize=<arg>,     -j<arg>
   --errorsonly=<arg>,      -e<arg>
   --analyze,               -a
   --htmlize=<arg>,         -l<arg>
   --tptpize=<arg>,         -t<arg>
   --mizfiles=<arg>,        -m<arg>
   --mizhtml=<arg>,         -H<arg>
   --xsldir=<arg>,          -x<arg>
   --verifier=<arg>,        -v<arg>
   --makeenv=<arg>,         -n<arg>
   --quiet,                 -q
   --help,                  -h
   --man

=head1 OPTIONS

=over 8

=item B<<< --parallelize=<arg>, -j<arg> >>>

If greater than 1, runs problems in parallel, using Makefile with
the -j<arg> option.
Default is 1 - no parallelization.

=item B<<< --errorsonly=<arg>, -e<arg> >>>

If 1, only gives back the error file, does not touch the .miz file.
This is useful for Emacs or remote processing.
Default is 0 - works like the mizf script, putting 
errors directly into the .miz file.

=item B<<< --analyze, -a >>>

Only run analyzer (producing XML), ommit calling the Mizar checker.
This can be useful when only producing HTML or TPTP-izing.
By default the full checking takes place.

=item B<<< --htmlize=<arg>, -l<arg> >>>

If greater than 0, produce also html.
If 1, does only the basic html-ization using miz.xsl.
If 2, uses addabsrefs.xsl first, and then runs mix.xsl.
In both cases, we use ajax proofs and parallelize.
The default is 0 - don't produce.

=item B<<< --tptpize=<arg>, -t<arg> >>>

Produce also tptp problems, and possibly try to solve them.
The default is 0 (don't produce). If 1, MPTP is called to produce them.
If 2, an ATP is called to also try to solve them.

=item B<<< --mizfiles=<arg>, -m<arg> >>>

Sets the $MIZFILES environmental variable for Mizar processing.
The default is its value in the current environment.

=item B<<< --mizhtml=<arg>, -B<H><arg> >>>

The url to which the htmlization links. Only relevant if
htmlize > 0. If 0 (default), it links to the appropriate version
at mizar.uwb.edu.pl at http://mizar.uwb.edu.pl/version/ .
If 1, it links to mizfiles/html . Otherwise it is treated
as the url prefix itself.

=item B<<< --xsldir=<arg>, -x<arg> >>>

The directory with stylesheets for html-zing and tptp-zing.
The default is mizfiles/xml .

=item B<<< --verifier=<arg>, -v<arg> >>>

Sets the verifier for Mizar processing.
The default is $MIZFILES/bin/verifier, and if that does not
exist, then just "verifier" (relying on $PATH).

=item B<<< --makeenv=<arg>, -n<arg> >>>

Sets the accommodator for Mizar processing.
The default is $MIZFILES/bin/makeenv, and if that does not
exist, then just "makeenv" (relying on $PATH).

=item B<<< --tmpdir=<arg>, -B<T><arg> >>>

Directory (slash-ended) for temporary problem and result files.
Defaults to "", which means no usage of any special directory.

=item B<<< --quiet, -q >>>

Run verifier with the quite flag.

=item B<<< --runeprover=<arg>, -B<E><arg> >>>

If >= 1, and tptpize==2, run E. Default is 1, because this is
restricted by tptpize anyway.


=item B<<< --runspass=<arg>, -B<S><arg> >>>

If >= 1, and tptpize==2, run SPASS. Default is 0.

=item B<<< --runvampire=<arg>, -B<V><arg> >>>

If >= 1, and tptpize==2, run Vampire. Default is 0.

=item B<<< --runparadox=<arg>, -B<p><arg> >>>

If >= 1, and tptpize==2, run Paradox. Default is 0.

=item B<<< --runmace=<arg>, -B<M><arg> >>>

If >= 1, and tptpize==2, run Mace. Default is 0.

=item B<<< --tptpproofs, -z<arg> >>>

If > 0, try to get the TPTP format of all found proofs. This
is now done by rerunning EP on the set of needed references found
by other provers. 

=item B<<< --help, -h >>>

Print a brief help message and exit.

=item B<<< --man >>>

Print the manual page and exit.

=back

=head1 DESCRIPTION

This program splits Mizar article into smaller pieces
that get verified parallely. The errors are then merged and reported
as if the verification was serial.

=head1 CONTACT

Josef Urban firstname.lastname(at)gmail.com

=head1 LICENCE

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

=cut


use strict;
use Cwd;
use Pod::Usage;
use Getopt::Long;
use XML::LibXML;



my ($gparallelize, $gerrorsonly,    $ganalyze,
    $ghtmlize,     $gtptpize,       $gmizfiles,
    $gverifier,    $gmakeenv,       $gtmpdir,
    $gxsldir,      $gmizhtml);

my ($gquiet, $help, $man);


Getopt::Long::Configure ("bundling");

GetOptions('parallelize|j=i'    => \$gparallelize,
	   'errorsonly|e=i'    => \$gerrorsonly,
	   'analyze|a'    => \$ganalyze,
	   'htmlize|l=i'    => \$ghtmlize,
	   'tptpize|t=i'    => \$gtptpize,
	   'mizfiles|m=s'    => \$gmizfiles,
	   'mizhtml|H=s'    => \$gmizhtml,
	   'xsldir|x=s'    => \$gxsldir,
	   'verifier|v=s'    => \$gverifier,
	   'makeenv|n=s'    => \$gmakeenv,
	   'tmpdir|T=s'      => \$gtmpdir,
	   'quiet|q'          => \$gquiet,
	   'help|h'          => \$help,
	   'man'             => \$man)
    or pod2usage(2);

pod2usage(1) if($help);
pod2usage(-exitstatus => 0, -verbose => 2) if($man);

pod2usage(2) if ($#ARGV != 0);

my $gfilestem   = shift(@ARGV);
if($gfilestem =~ m/(.*)[.]miz$/) { $gfilestem = $1;}

my $gtopdir = getcwd();

$gparallelize = 1 unless(defined($gparallelize));
$gerrorsonly = 0 unless(defined($gerrorsonly));
$ghtmlize = 0 unless(defined($ghtmlize));
$gtptpize = 0 unless(defined($gtptpize));
$gmizfiles = $ENV{"MIZFILES"} unless(defined($gmizfiles));
$gmizhtml = "0" unless(defined($gmizhtml));
$gxsldir = $gmizfiles unless(defined($gxsldir));

# the stylesheets - might not exist, test with -e before using
my $addabsrefs = "$gxsldir/addabsrefs.xsl";
my $miz2html = (-e "$gxsldir/miz.xsl") ? "$gxsldir/miz.xsl" : "$gxsldir/miz.xml";
my $mizpl = "$gxsldir/mizpl.xsl";


my $gmizar_url = 'http://mizar.uwb.edu.pl';

my $gmml_anr;
my $gmml_vnr;
my $gmml_version;
my $gmizar_rnr;
my $gmizar_v1nr;
my $gmizar_v2nr;
my $gmizar_version;
my $ghtml_version;

sub ParseMmlIni
{
    open(INI, "$gmizfiles/mml.ini") or die "mml.ini does not exist";
    while($_=<INI>) 
    {
	if(m/MizarReleaseNbr *= *(\d+)/) { $gmizar_rnr = $1;}
	if(m/MizarVersionNbr *= *(\d+)/) { $gmizar_v1nr = $1;}
	if(m/MizarVariantNbr *= *(\d+)/) { $gmizar_v2nr = $1;}
	if(m/NumberOfArticles *= *(\d+)/) { $gmml_anr = $1;}
	if(m/MMLVersion *= *([0-9.]+)/) { $gmml_vnr = $1;}
    }	
    close(INI);

    (defined($gmizar_rnr) && defined($gmizar_v1nr) 
     && defined($gmizar_v2nr) && defined($gmml_anr)
     && defined($gmml_vnr)) or die "mml.ini incomplete";

    $gmizar_version = join('.',($gmizar_rnr, $gmizar_v1nr, $gmizar_v2nr));
    $gmml_version = $gmml_vnr . '.' . $gmml_anr;
    $ghtml_version = $gmizar_version . '_' . $gmml_version;
}

ParseMmlIni();

if(($gmizhtml eq "0") && ($ghtmlize > 0))
{    
    $gmizhtml = "$gmizar_url/version/$ghtml_version/html/";
}
elsif($gmizhtml eq "1")
{
    $gmizhtml = "$gmizfiles/html/";
}


unless(defined($gverifier))
{
    $gverifier = (-e "$gmizfiles/bin/verifier") ? "$gmizfiles/bin/verifier" : "verifier";
}

unless(defined($gmakeenv))
{
    $gmakeenv = (-e "$gmizfiles/bin/makeenv") ? "$gmizfiles/bin/makeenv" : "makeenv";
}

my $gaddfmsg = (-e "$gmizfiles/bin/addfmsg") ? "$gmizfiles/bin/addfmsg" : "addfmsg";
my $gerrflag = (-e "$gmizfiles/bin/errflag") ? "$gmizfiles/bin/errflag" : "errflag";

$gtmpdir = "" unless(defined($gtmpdir));

my $gquietflag = $gquiet ? ' -q ' : '';
my $gaflag = $ganalyze ? ' -a ' : '';

$ENV{"MIZFILES"}= $gmizfiles;

my $pxext = '.parx';

my $miz = $gfilestem . ".miz";
my $xml = $gfilestem . ".xml";

sub MAXLINENR ()  { 100000000 } # we will break on files with lines above this nr

sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }


my @glines=();  # array of lines in the .miz file


# run parser to get the top proof positions in xml, count their number
# divide the tpnr by the number of processors to get the nr of parallel procs
# divide the tpnrs into smilarly big groups (randomly, or by some greedy algo)
# foreach group: in .miz replace the top proof positions with @proof, up to the ones in the group
# put each .miz into its directory, verify there
# sort -u the .err files

# accommodate (if needed), then call the main Verify procedure
sub Accommodate
{
    my ($filestem) = @_;
    if(system("$gmakeenv $filestem") == 0)
    {
	Verify($filestem);
    }
    elsif($gerrorsonly == 0) 
    { 
	system("$gerrflag $filestem");
	system("$gaddfmsg $filestem $gmizfiles/mizar");
    }
}

## This creates relatively equally hard pieces
## the positions in each piece must be sorted
sub MakePieces
{
    my ($nrpieces,$plinesnr, $tppos) = @_;
#    return MakePiecesSimple($nrpieces,$plinesnr, $tppos);
    return MakePiecesByPSize($nrpieces,$plinesnr, $tppos);
}

## just divide from beginning to end, disregarding proof size
sub MakePiecesSimple
{
    my ($nrpieces,$plinesnr, $tppos) = @_;
    my @tpp = @$tppos;
    my @res = (); # array of $nrpieces of arrays of positions from $tppos 
    my $size = 1 + int((2 + $#tpp) / $nrpieces); # we add one, to make the last piece smaller
    foreach my $step (0 .. ($nrpieces - 1))
    {
	my $piece = [];
	#DEBUG print $step;
	foreach my $rng ($size*$step .. min($#tpp, $size*($step+1) - 1))
	{
	    push(@$piece, $tpp[$rng]);
	    #DEBUG print $rng, "\n";
	}
	push(@res, $piece) if(scalar(@$piece) > 0); # if we make very small pieces, the last ones could be empty
    }
    return \@res;
}

## divide trying to get equal proof sizes, greedy algo
sub MakePiecesByPSize
{
    my ($nrpieces,$plinesnr, $tppos) = @_;
    my @tpp = @$tppos;

    ## array of pieces with positions
    my @pieces = ();
    foreach my $nr (0 .. $nrpieces-1) { push(@pieces,[]); }

    ## array of total proof sizes of pieces with their positions in @pieces,
    ## always sorted form the least proof size
    my @sizes = ();
    foreach my $nr (0 .. $nrpieces-1) { push(@sizes,[0,$nr]); }

    ## sort descending by the number of proof lines
    foreach my $tp (sort { $b->[4] <=> $a->[4] } (@tpp))
    {
	my $tpsize = $tp->[4];
	my $piece = $pieces[$sizes[0]->[1]];  # the smallest piece
	push(@$piece, $tp);
	$sizes[0]->[0] += $tpsize;
	@sizes = sort { $a->[0] <=> $b->[0] } @sizes;
    }

    my @res = ();
    ## now sort positions in each piece by bl, remove empty pieces
    foreach my $piece (@pieces) 
    { 
	if(scalar(@$piece) > 0)
	{
	    @$piece = sort { $a->[0] <=> $b->[0]  } @$piece;
	    push(@res, $piece);
	}
    }

    return \@res;
}

## all the ajax proofs go usually into $gtopdir/proofs - to be in one place

my $miz2html_params = "--param default_target \\\'_self\\\'  --param linking \\\'l\\\' --param mizhtml \\\'$gmizhtml\\\' --param selfext \\\'html\\\'  --param titles 1 --param colored 1 --param ajax_proofs 1 ";

sub Htmlize
{
    my ($myfstem, $htmlize, $ajax_proof_dir) = @_;
    if($htmlize == 2)
    {
	system("xsltproc $addabsrefs $myfstem.xml 2> $myfstem.xml.errabs > $myfstem.xml.abs");
	system("xsltproc $miz2html_params --param proof_links 1 -param ajax_proof_dir \\\'$ajax_proof_dir\\\' $miz2html $myfstem.xml.abs 2>$myfstem.xml.errhtml > $myfstem.html");
    }
    elsif($htmlize == 1)
    {
	system("xsltproc $miz2html_params $miz2html $myfstem.xml 2>$myfstem.xml.errhtml > $myfstem.html");
    }    
}


## verify one chunk in a speciual subdirectory
## $piece is supposed to be sorted, $tpppos too
sub VerifyProofChunk
{
    my ($filestem, $chunk, $piece, $tppos, $htmlize) = @_;
#    my @tpp = @$tppos;
#    my @piece = @$piece;
    my $mydir = $gtmpdir . $filestem . "__" . $chunk;
    my $myfstem = "$mydir/$filestem";
    mkdir($mydir);
    SetupEnvFiles($filestem, $mydir);
    CreateAtSignFile($filestem, $mydir, $chunk, $piece, $tppos);
    system("$gverifier $gquietflag $gaflag $myfstem");
    Htmlize($myfstem, $htmlize, "proofs");
}

# extensions of the environmental files
my @accexts = (".aco", ".atr", ".dct", ".dfs", ".eid", ".ere", ".esh", ".evl", ".frm", ".prf", ".vcl",
	       ".ano", ".cho", ".dcx", ".ecl", ".eno", ".eth", ".fil", ".nol", ".sgl");

sub SetupEnvFiles
{
   my ($filestem, $mydir) = @_;
   foreach my $ext (@accexts)
   {
       my $f = $filestem . $ext;
       unlink("$mydir/$f");
       `ln -s $gtopdir/$f $mydir/$f`;
   }
}

# put @proof to $tppos, except those in $piece
# now also works for empty $piece, to be able to add @proof everywhere
sub CreateAtSignFile
{
    my ($filestem, $mydir, $chunk, $piece, $tppos) = @_;
    my @tpp = @$tppos;
    my @piece = @$piece;
    #DEBUG print "chunk:$chunk:$#piece:$#tpp:";
    my ($nexttpp, $nextcpp) = (0,0); # next top proof pos, next chunk proof pos
    my ($nexttpl,$nexttpc) = ($tpp[$nexttpp]->[0], $tpp[$nexttpp]->[1]);
    my ($nextcpl,$nextcpc);
    if(scalar(@$piece) == 0)
    {
	($nextcpl,$nextcpc) = (MAXLINENR, MAXLINENR);
    }
    else 
    {
	($nextcpl,$nextcpc)= ($piece[$nextcpp]->[0], $piece[$nextcpp]->[1]);
    }
    open(LMIZ,">$mydir/$filestem.miz");
    foreach my $lnr (1 .. 1 + $#glines)
    {
	if($lnr < $nexttpl)
	{
	    print LMIZ $glines[$lnr - 1];
	}
	else   # assert ($lnr == $nexttpl)
	{
	    if($nexttpl < $nextcpl)
	    {
                #DEBUG print LMIZ "atproof:$nexttpl:$nextcpl";
		my $l = $glines[$lnr - 1];
		print LMIZ (substr($l,0,$nexttpc-5),'@',substr($l,$nexttpc-5));
		#DEBUG print LMIZ $glines[$lnr - 1];
	    }
	    else # assert ($lnr == $nextcpl)
	    {
		print LMIZ $glines[$lnr - 1];
		if($nextcpp == $#piece) 
		{
		    $nextcpl = MAXLINENR;
		}
		else
		{
		    $nextcpp++;
		    ($nextcpl,$nextcpc) = ($piece[$nextcpp]->[0], $piece[$nextcpp]->[1]);
		}
	    }

	    if($nexttpp == $#tpp) 
	    {
		$nexttpl = MAXLINENR;
	    }
	    else
	    {
		$nexttpp++;
		($nexttpl,$nexttpc) = ($tpp[$nexttpp]->[0], $tpp[$nexttpp]->[1]);
	    }
	}

    }
    close(LMIZ);
}

sub MergeErrors
{
    my ($filestem, $piecesnr) = @_;
    my %errors = ();
    foreach my $chunk (1 .. $piecesnr)
    {
	if(open(ERR, $gtmpdir . $filestem . "__" . $chunk . '/' . $filestem . '.err'))
	{
	    while($_=<ERR>) { $errors{$_} = (); };
	    close(ERR);
	}	
    }
    ## include the master error file
    if( open(ERR, $filestem . '.err')) 
    {
	while($_=<ERR>) { $errors{$_} = (); };
	close(ERR);
    }

    open(ERR, ">$filestem.err") or die "Cannot write .err file!";
    foreach my $key (sort keys %errors) { print ERR $key;}
    close(ERR);
}

# the XPath expression for proofs that are not inside other proof
my $top_proof_xpath = '//Proof[not((name(..)="Proof") 
          or (name(..)="Now") or (name(..)="Hereby")
          or (name(..)="CaseBlock") or (name(..)="SupposeBlock"))]';

# verify parallely, calling first just parser and libxml 
# to detect top proofs and their lengths, then divide the positions
# into equal pieces, then fork and verify each of them, then merge
# the error files
sub Verify
{
    my ($filestem) = @_;
    my $pxfile = $filestem . $pxext;
    ## call Mizar parser to get the tp positions
    system("$gverifier $gquietflag -p $filestem") if($gparallelize > 1);
    if((-e $pxfile) && ($gparallelize > 1))
    {
	my $parser = XML::LibXML->new();
	my $doc = $parser->parse_file($pxfile);
	## get the top proof nodes (those whose parent is not another proof block)
	my @tpnodes = $doc->findnodes($top_proof_xpath);

	# the parallelization makes sense only if there is at least one proof,
	# if not, we should however call the verifier on the file anyway
	if($#tpnodes >= 0)
	{
	    my @tppos = (); # each entry is a list of [begline,begcol,endline,endcol,nr_of_lines]
	    my $plinesnr = 0; # total nr of proof lines in the .miz
	    foreach my $node (@tpnodes)
	    {
		# find the end position of the proof
		my ($endpos) = $node->findnodes('EndPosition[position()=last()]');
		my ($bl,$bc,$el,$ec) = ($node->findvalue('@line'),$node->findvalue('@col'),
					$endpos->findvalue('@line'),$endpos->findvalue('@col'));
		push(@tppos, [$bl,$bc,$el,$ec,$el-$bl]);
		$plinesnr += $el-$bl;
	    }
	    ## DEBUG: foreach my $ln (@tppos) {print join(',',@$ln),"\n";} print $plinesnr;
	    my $ppieces = MakePieces($gparallelize,$plinesnr, \@tppos);

	    ## the forking code
	    my @childs = ();
	    foreach my $chunk (1 .. scalar(@$ppieces)) 
	    {
		my $pid = fork();
		if ($pid) 
		{
		    # parent
		    push(@childs, $pid);
		} 
		elsif ($pid == 0) 
		{
		    # child
		    VerifyProofChunk($filestem, $chunk, $ppieces->[$chunk - 1], \@tppos, $ghtmlize);
		    #DEBUG print "$chunk\n\n";
		    #DEBUG sleep(5);
		    exit(0);
		} 
		else 
		{
		    die "couldn’t fork: $!\n";
		}
	    }

	    foreach (@childs) { waitpid($_, 0);}
	    MergeErrors($filestem, scalar(@$ppieces));
	}
	else
	{
	    system("$gverifier $gquietflag $gaflag $filestem");
	    Htmlize($filestem, $ghtmlize, "proofs");
	}
    }
    else
    {
	system("$gverifier $gquietflag $gaflag $filestem");
	Htmlize($filestem, $ghtmlize, "proofs");
    }

    if($gerrorsonly == 0) 
    { 
	system("$gerrflag $filestem");
	system("$gaddfmsg $filestem $gmizfiles/mizar");
    }
}

open(MIZ,$miz) or die "$miz not readable";
while($_=<MIZ>) { push(@glines, $_); };
close(MIZ);

Accommodate($gfilestem);


=pod

# Get theorem Propositions' positions
open(XML, $xml);
local $/;$_=<XML>;

# Search XML for theorem positions,
# and print theorems with proofs to files
while(m/((<JustifiedTheorem)(.|[\n])*?<\/JustifiedTheorem>)/g)
{
#DEBUG    print $2, "\n";
## This is fragile, could be done by xslt processing (slightly slower)
if($1=~m/<JustifiedTheorem.*[\n]<Proposition.*line=\"([0-9]+)\".*col=\"([0-9]+)\"(.|[\n])*?<EndPosition.*line=\"([0-9]+)\".*col=\"([0-9]+)\"\/> *[\n]*<\/Proof> *[\n]*<\/JustifiedTheorem>/)
{
    my ($l1,$c1,$l2,$c2) = ($1,$2,$4,$5);
#DEBUG    print join(",",($l1,$c1,$l2,$c2)), "\n";
    my $thname = $filestem . "__" . $l1 . "_" . $c1;
    my $l0 = $l1;
    my $th = $lines[$l0];
    while(!($th =~ m/\btheorem\b/)) {$th = $lines[$l0--];}
    open(F,">$thname");
    while(++$l0<=$l2) { print F $lines[$l0]; }
    close(F);
}}
close(XML);



