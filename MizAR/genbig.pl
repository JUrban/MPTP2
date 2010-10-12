#!/usr/bin/perl -w
# create big versions of listed problems
my $Mizfiles = "/home/mptp/public_html/mml4.145.1096";
my $MMLAxs = $Mizfiles . "/mptp/00allmmlax" ;

while ($File = shift)
{
    print "$File \n";

    my $LocalAxs = `ls *.ax`;

    if(    ( -e $File) && open(F1, $File . '.allowed_local'))
    {
	my $allowed_line = <F1>;
	close(F1);
	$allowed_line =~ m/.*\[(.*)\].*/ or die "Bad allowed_local";
	my @allowed = split(/\, */, $1);

	my $regexp = '"^fof( *\(' . join('\|',@allowed) . '\) *,"';
	`echo "include('$MMLAxs')." > $File.big`;
	`grep $regexp $LocalAxs >> $File.big`;
	`cat $File >> $File.big`;
    }
}
