mkdir axioms conjectures0 conjectures fofified renamed0 renamed
for i in `ls *.p`; do echo $i; grep "cnf([^,]*,axiom," $i | tptp4X -t fofify -u machine -- >axioms/$i; done
for i in `ls *.p`; do echo $i; grep -v "cnf([^,]*,axiom," $i >conjectures0/$i; done
cd conjectures0
mkdir res 
for i in `ls`; do /home/urban/TPTPWorld/TPTP/TPTP2X/tptp2X  -d `pwd`/res  -t fofify:obvious  $i; done
cd res
for i in `ls`; do j=`echo -n $i | sed -e 's/.fof_obvious.tptp//'`; tptp4X -u machine -c $i | sed -e "s/^fof([^,]*,/fof($j,/" > ../../conjectures/$j.p; done
cd ../..
for i in `ls *.p`; do echo $i; cat axioms/$i conjectures/$i | ../../NumberVars > fofified/$i; done
cd fofified
cat *| sort -u >00
../../../rename_from_sorted.pl
cd ../renamed0
sort -u 00 >01
../../../mk_diff_names.pl
cd ../renamed
# for i in `ls *.p`; do j=`echo -n $i| sed -e 's/.p$//'`; echo $j; sed -e "s/^fof([^,]*,conjecture/fof($j,conjecture/" $i >$i.new; done
for i in `ls *.p`; do j=`echo -n $i| sed -e 's/.p$//'`; grep -L "^fof($j,conjecture" $i; done
