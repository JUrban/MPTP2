package AIAdvise;

use strict;
use IO::Socket;

sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }

##  StartSNoW($path2snow, $path2advisor, $snow_symoffset, $snow_filestem);
##
## Get unused ports for SNoW and for the symbol translation daemon
## (advisor), start them, and return the ports and the pids of snow
## and advisor.  $snow_symoffset tells the translation daemon where
## the symbol numbering starts.
##
## Be sure to sleep for sufficient amount of time (ca 40s for all MML)
## until SNoW loads before asking queries to it.
##
##
## SYNOPSIS:
## my $BinDir = "/home/urban/bin";
##
## my ($aport, $sport, $adv_pid, $snow_pid) = StartSNoW("$BinDir/snow", "$BinDir/advisor.pl", 500000, 'test1');
sub StartSNoW
{
    my ($path2snow, $path2advisor, $snow_symoffset, $snow_filestem) = @_;
    my $snow_net = $snow_filestem . '.net';
    my $snow_arch =     $snow_filestem . '.arch';
#--- get unused port for SNoW
    socket(SOCK,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK,  sockaddr_in(0, INADDR_ANY));
    my $sport = (sockaddr_in(getsockname(SOCK)))[0];
#    print("snowport $sport\n");
    close(SOCK);

#--- start snow instance:
# ###TODO: wrap this in a script remembering a start time and pid, and self-destructing
#          in one day

    my $snow_pid = fork();
    if ($snow_pid == 0)
    {
	# in child, start snow
	open STDOUT, '>', $snow_filestem . '.snow_out';
	open STDERR, '>', $snow_filestem . '.snow_err';
	exec("$path2snow -server $sport -o allboth -F $snow_net -A $snow_arch ")
	    or print STDERR "couldn't exec $path2snow: $!";
	close(STDOUT);
	close(STDERR);
	exit(0);
    }

#--- get unused port for advisor
    socket(SOCK1,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK1,  sockaddr_in(0, INADDR_ANY));
    my $aport = (sockaddr_in(getsockname(SOCK1)))[0];
#    print("advisorport $aport\n");
    close(SOCK1);

    my $adv_pid = fork();
    if ($adv_pid == 0)
    {
	# in child, start advisor
	open STDOUT, '>', $snow_filestem . '.adv_out';
	open STDERR, '>', $snow_filestem . '.adv_err';
	exec("$path2advisor -p $sport -a $aport -o $snow_symoffset $snow_filestem")
	    or print STDERR "couldn't exec $path2advisor: $!";
	exit(0);
    }
    return ($aport, $sport, $adv_pid, $snow_pid);
}




## GetRefs($advhost, $aport, $syms, $limit)
##
## Gets at most $limit references relevant for symbols $syms by asking trained bayes advisor
## running on host $advhost on port $aport.
##
## SYNOPSIS:
## my @symbols = ('+','0','succ');
## my $advisor_url = 'localhost';
## my $advisor_port = 50000;
## my $wanted_references_count = 30;
##
## my @references = GetRefs($advisor_url, $advisor_port, \@symbols, $wanted_references_count)

sub GetRefs
{
    my ($advhost, $aport, $syms, $limit) = @_;
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

## test: load snow/advisor on thms3, send it a simple request and print result, kill both

sub Tst1
{
    my $BinDir = "/home/urban/gr/MPTP2/MizAR/cgi-bin/bin";
    my ($aport, $sport, $adv_pid, $snow_pid) = StartSNoW("$BinDir/snow", "$BinDir/advisor.pl", 500000, 'thms3');
    sleep 120;
    my $input1 = ['k3_csspace3'];
    my $input2 = ['v2_rearran1'];
    my @refs1 = GetRefs('localhost', $aport, $input1, 10);
    print join(',',@refs1) . "\n\n";
    my @refs2 = GetRefs('localhost', $aport, $input2, 10);
    print join(',',@refs2) . "\n\n";
}


1;
