#!/bin/bash
# $1= 7.8.10 $2=4.99.1005 $3=http://mws.cs.ru.nl/~mptp
# script installing MizAR for particular version of library and sources
# assumes we are in some mizwrk directory
# old copied info:
# script creating the mizar html from a mizar distro
# current dir should contain the Makefile for doing this,
# and all prerequisities required by the Makefile have to be present 
ver="$1_$2"
myurl=$3
cvsver=`echo -n $1| sed -e 's/\./_/g'`
root=`pwd`
ph=/home/mptp/public_html
cgi=$ph/cgi-bin
bindir=bin$2
mycgi=$cgi/bin$2
mymml=$ph/mml$2
jobs=8

wget ftp://mizar.uwb.edu.pl/pub/system/i386-linux/mizar-$ver-i386-linux.tar
mkdir $ver
tar xf mizar-$ver-i386-linux.tar -C$ver 
cd $ver
cvs -d:pserver:softdev@mizar.uwb.edu.pl:2401/srv/cvsroot co -rver_$cvsver kernel libtools
cd kernel
fpc -Sd -dCH_REPORT -dSCH_REPORT verifier.dpr 
cd ../libtools
fpc -Sd -Fu../kernel envget.dpr
cd ..
### TODO: fix mizf!!

tar xzf mizshare.tar.gz 
tar xzf mizdoc.tar.gz 
mkdir bin
tar xzf mizbin.tar.gz -Cbin
cp  kernel/verifier bin/verifier.bfex
cp  libtools/envget bin/envget
mv bin/verifier bin/verifier.std
cd bin && ln -s verifier.bfex verifier && cd ..
mkdir $mycgi
ln -s $root/$ver/bin $mycgi/mizar 
# tar xzf mizbin.tar.gz -C$mycgi

mkdir html 
sed -e 's/urban/mptp/' /home/mptp/gitrepo/MPTP2/mizsys/Makefile.4.145 > Makefile
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
# git branch --track MizAR1096 origin/MizAR1096
# git checkout MizAR1096

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
ln -s /home/mptp/gitrepo/xsl4mizar $ph/xsl4mizar

cp /home/mptp/gitrepo/MPTP2/MizAR/cgi-bin/bin/* $mycgi 

ln -s $mymml/mptp/utils.pl $mycgi/utils.pl

sed -ie "s/^mml_dir(.*/mml_dir(\"\/home\/mptp\/mizwrk\/$ver\/MPTP2\/pl\/\")./" utils.pl
sed -ie "s/^bindir=.*/bindir=$bindir/" $mycgi/mizf
sed -ie "s/^\(my .MyUrl = \).http:..mws.cs.ru.nl.~mptp.;/\1'$myurl';/" $mycgi/MizAR.cgi

swipl -nodebug -A0 -L0 -G0 -T0 -q -t "[utils], mml2tptp_includes('Axioms/'), halt."
cat Axioms/*.ax > 00allmmlax

# fix bindir in mizf

# upon first install, files in cgi-bin need to be symlinked to MizAR/cgi-bin files
# xsl4mizar expected in public_html - done
# ERROR: script_file `/home/mptp/public_html/cgi-bin/bin4.160.1126/utils.pl' does not exist - DONE
# The requested URL /~mptp/cgi-bin/bin4.160.1126/showby.cgi was not found on this server.
