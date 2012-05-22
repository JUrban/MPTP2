#!/usr/bin/perl -w

# ./e2pby conjname foo.eout > foo.pby
# for i in `ls | sed -e 's/.p.eout$//'`; do /home/mptp/gr/MPTP2/MaLARea/script/e2pby.pl $i $i.p.eout; done > 00proved_by
# 
# the same thing for z3 is simpler:
# cat *|grep '^core'  | sed -e 's/^core(\([^,]*\),\[/proved_by(\1,[\1,/' > 00zproved_by

my $e_regexp =  '.*, *file\([^\),]+, *([a-z0-9A-Z_]+) *\)';

my $conj = shift;

while ($_=<>)
{
    if(m/$e_regexp/) { push(@proved_by, $1); }
}

my $conj_refs = join(",", @proved_by);
print "proved_by($conj,[$conj_refs]).\n";
