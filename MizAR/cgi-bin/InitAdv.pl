#!/usr/bin/perl -w

# train from the first argument symbolically and start the daemon
#
# usage: ./InitAdv.pl filestem

use lib '/home/urban/gr/MPTP2/MizAR/cgi-bin';
use AIAdvise;

my $MizAR_path = '/home/urban/gr/MPTP2/MizAR/cgi-bin';
my $symoffset=500000;
my $filestem= shift;
my $advlimit= shift;
my ($grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref) = AIAdvise::CreateTables($symoffset, $filestem);
my $proved_by = AIAdvise::PrintProvedBy0($symoffset, $filestem, $grefnr);
AIAdvise::PrintTrainingFromHash($filestem,0,$proved_by,$grefnr, $gsymnr, $gsymarity, $grefsyms, $gnrsym, $gnrref);
AIAdvise::Learn0( $MizAR_path . "/bin/snow", $filestem, $grefnr);
my ($aport, $sport, $adv_pid, $snow_pid) = 
  AIAdvise::StartSNoW($MizAR_path . "/bin/snow", $MizAR_path . "/advisor_lean.pl", $symoffset, $filestem, $advlimit);
print "$aport,$sport\n";
