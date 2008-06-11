#!/usr/bin/perl -w

use strict;
use CGI;
use IO::Socket;
use File::Temp qw/ :mktemp  /;
use IPC::Open2;
use HTTP::Request::Common;
use LWP::Simple;

my $MyUrl = 'http://octopi.ms.mff.cuni.cz/~mptp';
my $TemporaryDirectory = "/tmp";
my $TemporaryProblemDirectory = "$TemporaryDirectory/matp_$$";
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $Mizfiles = "/home/mptp/public_html/mml";
my $MizHtml = $MyUrl . "/mml/html/";
my $mizf =     "bin/mizf";
my $xsltproc =     "bin/xsltproc";
my $dbenv = "bin/dbenv.pl";
my $utilspl =  "/home/mptp/public_html/cgi-bin/bin/utils.pl";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $miz2html = "$Xsl4MizarDir/miz.xsl";
my $mizpl = "$Xsl4MizarDir/mizpl.xsl";
my $doatproof = 0;
my $atproof = '@' . 'proof';

my $query	  = new CGI;
my $input_file	  = $query->param('file');
my $input_tmp     = $query->param('tmp');

print $query->header;

print $query->start_html("File Output");

my $File0 = "$TemporaryDirectory/matp_" . $input_tmp . "/" . $input_file;
open(F,$File0);

# print $File1;
    print "<pre>";
#    open(F,$File1);
    { local $/; $_= <F>; print $_; }
    print "<pre/>";
    print $query->end_html;
    close(F);
