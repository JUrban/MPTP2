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
my $input_raw     = $query->param('raw');
my $input_refresh  = $query->param('refresh');
my $content_type = $query->param('content-type');
my $File0 = "$TemporaryDirectory/matp_" . $input_tmp . "/" . $input_file;

my $print_refresh = 0;

if (defined($input_refresh))
{
    if (!(-e $File0)) { $print_refresh = 1 }
    else
    {
	my $now = time;
	my $mtime = (stat($File0))[9];
	if ($now < $mtime + 6) { $print_refresh = 1 }
    }
}

if ($print_refresh == 1) { print $query->header(-Refresh=>'2'); }
else { print $query->header(); }

print $query->start_html("File Output") unless defined($input_raw);

if (-e $File0) 
{
    open(F,$File0);

# print $File1;
    print "<pre>" unless defined($input_raw);
#    open(F,$File1);
    { local $/; $_= <F>; print $_; }
    print "<pre/>" unless defined($input_raw);
    close(F);
}

print $query->end_html unless defined($input_raw);
