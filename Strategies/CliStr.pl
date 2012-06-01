#!/usr/bin/perl -w

# ls *protok*simple *protok*_KBO *protok*_SOS| xargs grep -l Theorem | xargs grep Processed | perl -F ~/gr/MPTP2/Strategies/CliStr.pl | less

# p.leancop.cnf.protokoll_cnf_my18simple_KBO:#

# ime ./param_ils_2_3_run.rb -numRun 0 -scenariofile example_e1/scenario-e8.txt -N 200 -validN 6 -init zz34

# ls *protok*simple *protok*_KBO *protok*_SOS| xargs grep -l Theorem | xargs grep Processed | perl -e ' while(<>) { m/^([^.]*)\.(.*): *(\d+)/ or die; if((! exists($h{$1})) || ($h{$1} > $3)) { $i{$1}=$h{$1}; $j{$1}=$g{$1}; $h{$1} = $3; $g{$1} = $2; }} foreach $k (sort keys %h) { $v{$g{$k}}{$k}=$h{$k}  } foreach $p (sort keys %v) {print "\n$p:\n"; foreach $k (sort keys %{$v{$p}}) {print "$k:$h{$k}\n" if(($h{$k}>500) && ($h{$k}<30000))}}' |less

use strict;


my $gPIdir = '/home/mptp/big/ec/paramils2.3.5-source';

my $gPIexmpldir = $gPIdir . "/example_data";

my $gPIscendir = $gPIdir . "/example_e1";

my $gstratsdir = "strats";

my $gprobprefix = 'example_data/e1/'; # ecnf1

my $gprobsuffix = '.p'; # .p.leancop.cnf

my $gmaxstr = shift;
my $gminstrprobs = shift;

$gmaxstr = 20 unless(defined($gmaxstr));
$gminstrprobs = 8 unless(defined($gminstrprobs));


sub PrintScenario
{
    my ($prot,$iter) = @_;

    open(F,">$gPIscendir/scenario-$prot.$iter.txt");


    print F <<SCEN;
algo = ruby e_wrapper1.rb
execdir = example_e1
deterministic = 0
run_obj = runlength
overall_obj = mean
cutoff_time = 3
cutoff_length = max
tunerTimeout = 20060
paramfile = example_e1/e-params.txt
outdir = example_e1/paramils-out
instance_file = example_data/$prot.$iter.txt
test_instance_file = example_data/$prot.$iter.tst

SCEN

    close(F);
}

sub PrintProbStr
{
    my ($v,$min,$max) = @_;
    foreach my $p (sort keys %$v) {
	print "\n$p:\n";
	foreach my $k (sort keys %{$v->{$p}}) {
	    print "$k:$v->{$p}{$k}\n" if(($v->{$p}{$k}>=$min) && ($v->{$p}{$k}<=$max));
	}
    }
}

sub PrintProbStrFiles
{
    my ($v,$iter,$min,$max) = @_;
    foreach my $p (sort keys %$v)
    {
	PrintScenario($p,$iter);
	open(F,">$gPIexmpldir/$p.$iter.txt");
	open(F1,">$gPIexmpldir/$p.$iter.tst");
	foreach my $k (sort keys %{$v->{$p}})
	{
	    if(($v->{$p}{$k}>=$min) && ($v->{$p}{$k}<=$max))
	    {
		print F ("$gprobprefix", "$k", $gprobsuffix, "\n");
		print F1 ("$gprobprefix", "$k", $gprobsuffix, "\n");
	    }
	}
	close(F); close (F1);
    }
}


sub TopStratProbs
{
    my ($maxstr,$minstrprobs,$min,$max) = @_;
    my %g = ();
    my %h = ();
    my %i = ();
    my %j = ();
    my %v = ();
    my %c = ();

    while (<>)
    {
	m/^([^.]*)\..*(protokoll_[^:]*).*: *(\d+)/ or die;
	if ((! exists($h{$1})) || ($h{$1} > $3))
	{
	    $i{$1}=$h{$1};
	    $j{$1}=$g{$1};
	    $h{$1} = $3;
	    $g{$1} = $2;
	}
    }

    foreach my $k (keys %g) { $c{$g{$k}}++ if( ($h{$k} >= $min) &&  ($h{$k}<=$max)); }

    #print %c,"\n";

    foreach my $s (keys %c) { $c{$s}=0 if( $c{$s} < $minstrprobs ); }

    #print %c,"\n";

    my $cnt = 0;
    foreach my $s (sort {$c{$b} <=> $c{$a}} keys %c) { $cnt++; $c{$s}=0 if($cnt > $maxstr); }

    #print %c,"\n";

    foreach my $k (sort keys %h)
    {
	$v{$g{$k}}{$k}=$h{$k} if($c{$g{$k}} > 0);
    }

    return (\%h, \%v);
}


my ($h,$v) = TopStratProbs($gmaxstr,$gminstrprobs,500,30000);

PrintProbStr($v,500,30000);

PrintProbStrFiles($v,10,500,30000);
