#!/usr/bin/perl -w

# ./v2pby conjname foo.vout > foo.pby

my $vampire_regexp = '.*\bfile\([^\),]+, *([a-z0-9A-Z_]+) *\)';

my $conj = shift;

while ($_=<>)
{
    if(m/$vampire_regexp/) { push(@proved_by, $1); }
}

my $conj_refs = join(",", @proved_by);
print "proved_by($conj,[$conj_refs]).\n";
