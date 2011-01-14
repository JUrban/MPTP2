package AIAdvise;

use strict;
use IO::Socket;



##  StartSNoW($path2snow, $path2advisor, $snow_symoffset, $snow_filestem);
##
## Get unused ports for SNoW and for the symbol translation daemon
## (advisor), start them, and return the ports and the pids of snow
## and advisor.  $snow_symoffset tells the translation daemon where
## the symbol numbering starts.
##
##
## SYNOPSIS:
## my $BinDir = "/home/urban/bin";
##
## my ($aport, $sport, $adv_pid, $snow_pid) = StartSNoW("$BinDir/snow", "$BinDir/advisor.pl", 500000, 'test1');
sub StartSNoW
{
    my ($path2snow, $path2advisor, $snow_symoffset, $snow_filestem) = @_;
    my $advisor_output = $snow_filestem . '.adv_out';
    my $snow_output = $snow_filestem . '.snow_out';
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
	open(LOG,">", "$snow_output");
	*STDERR = *LOG;
	*STDOUT = *LOG;
	exec("$path2snow -server $sport -o allboth -F $snow_net -A $snow_arch ")
	    or print STDERR "couldn't exec $path2snow: $!";
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
	open(LOG1,">", "$advisor_output");
	*STDERR = *LOG1;
	*STDOUT = *LOG1;
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

1;
