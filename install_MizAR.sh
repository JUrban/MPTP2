#!/bin/bash
# $1= 7.8.10 $2=4.99.1005
# script installing MizAR for particular version of library and sources
# assumes we are in some mizwrk directory
# old copied info:
# script creating the mizar html from a mizar distro
# current dir should contain the Makefile for doing this,
# and all prerequisities required by the Makefile have to be present 
ver="$1_$2"
cvsver=`echo -n $1| sed -e 's/\./_/g'`
root=`pwd`
ph=/home/mptp/public_html
cgi=$ph/cgi-bin
mycgi=$cgi/bin$2
mymml=$ph/mml$2
jobs=8

wget ftp://mizar.uwb.edu.pl/pub/system/i386-linux/mizar-$ver-i386-linux.tar
mkdir $ver
tar xf mizar-$ver-i386-linux.tar -C$ver 
cd $ver
cvs -d:pserver:softdev@mizar.uwb.edu.pl:2401/srv/cvsroot co -rver_$cvsver kernel
cd kernel
fpc -Sd -dCH_REPORT -dSCH_REPORT verifier.dpr 
cd ..
### TODO: fix mizf!!

tar xzf mizshare.tar.gz 
tar xzf mizdoc.tar.gz 
mkdir bin
tar xzf mizbin.tar.gz -Cbin
cp  kernel/verifier bin/verifier.bfex
mv bin/verifier bin/verifier.std
ln -s bin/verifier.bfex bin/verifier
mkdir $mycgi
tar xzf mizbin.tar.gz -C$mycgi

mkdir html 
cp /home/urban/gitrepo/MPTP2/mizsys/Makefile.4.145 Makefile
cp -a mml miztmp
cp Makefile  miztmp
export MIZFILES=`pwd`
cd miztmp
make -j $jobs allacc | tee 00acc.log
make -j $jobs allhdr | tee 00hdr.log
make -j $jobs allxml | tee 00xml.log
make -j $jobs allxml1 | tee 00xml1.log
make -j $jobs allhtmla1 | tee 00htmla1.log
make -j $jobs allxml2 | tee 00xml2.log
make -j $jobs allevl1 | tee 00evl1.log
make -j $jobs allevl2 | tee 00evl2.log

make hidden.acc
make hidden.hdr
make hidden.xml
make hidden.xml1
make hidden.htmla1

make tarski.acc
make tarski.hdr
make tarski.xml
make tarski.xml1
make tarski.htmla1
make tarski.xml2

make -j $jobs alldco2
make tarski.dco2

for j in `ls *.htmla1| sed -e 's/.htmla1//'`; do mv $j.htmla1 ../html/$j.html; done
cd ..
tar czf html_abstr.$ver.noproofs.tar.gz html
mv miztmp/proofs html
tar czf html_abstr.$ver.tar.gz html

git clone git@github.com:JUrban/MPTP2.git
ln -s MPTP2 mptp
cd mptp
mkdir pl
mkdir Axioms
mkdir problems
for i in dcl2 dco2 evl2 lem2 sch2 the2 xml2; do mv ../miztmp/*.$i pl; done

cp hidden.dco2 pl/hidden.dco2
ln -s ../mml.lar mml.lar

ln -s $root/$ver $ph/$ver
ln -s $root/$ver $ph/$mymml

sed -ie "s/^mml_dir(.*/mml_dir(\"\/home\/mptp\/mizwrk\/$ver\/MPTP2\/pl\/\")./" utils.pl


swipl -nodebug -A0 -L0 -G0 -T0 -q -t "[utils], mml2tptp_includes('Axioms/'), halt."
cat Axioms/*.ax > 00allmmlax
