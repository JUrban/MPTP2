#!/usr/bin/perl

# SYNOPSIS
# ./mkloggraph.pl unsolved.log >unsolved.stats
# creates summary of results of all passes in unsolved.log
$giter = 0;
$theoremnbr = 0;
$timelimit[0] = 1;
while (<>) 
{
  if(m/SOLVED: *1[+]([0-9]+)/) 
    { 
      $solved[$giter] = 1+$1; 
      die "Theorems not in sync: $1: $theoremnbr" if(($giter> 3) && (1+$1 != $theoremnbr));
    }
  if(m/THRESHOLD: *([0-9]+)/) 
    { 
       $giter++; $threshold[$giter] = $1; $theoremnbr = 0;
       $timelimit[$giter] = $timelimit[$giter - 1];
      $solved[$giter] = 0;
     }
  if(m/TIMELIMIT: *([0-9]+)/) { $timelimit[$giter] = $1; }
  if(m/LEARNING: *([0-9]+)/) { $learning[$giter] = $1; }
  if(m/Theorem/) { $theoremnbr++; }
}
#print "$giter\n";

print "iternr\ttimelim\taxlimit\tsolved\n";
foreach $i (1 .. $giter)
  {
    print "$i:\t$timelimit[$i]\t$threshold[$i]\t$solved[$i]\n";
  }


#   THRESHOLD: 4
#   TIMELIMIT: 1
#   LEARNING:3
