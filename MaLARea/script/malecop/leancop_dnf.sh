#!/bin/sh
#-----------
# File:      leancop_dnf.sh
# Version:   1.2
# Date:      23 January 2011
#-----------
# Purpose:   Invokes the leanCoP prover
# Usage:     ./leancop_dnf.sh <problem file> [ <Server_location:Port_Number> [<time limit>]]
# Author:    Jiri Vyskocil
# License:   GNU General Public License
#-----------

#-----------
# Parameters

# set leanCoP prover path
PROVER_PATH=.

# set Prolog system, path, and options

#PROLOG=eclipse
#PROLOG_PATH=/usr/bin/eclipse
#PROLOG_OPTIONS='-e'

PROLOG=swi
PROLOG_PATH=/usr/bin/swipl
PROLOG_OPTIONS='-nodebug -L120M -G120M -T100M -q -t'

#PROLOG=sicstus
#PROLOG_PATH=/usr/bin/sicstus
#PROLOG_OPTIONS='--nologo --noinfo --goal'

# print proof [yes|no]
PRINT_PROOF=yes
# save proof [yes|no]
SAVE_PROOF=no
# proof layout [readable_with_global_index]
PROOF_LAYOUT=readable_with_global_index
# ai clauses advisor
AI_ADVISOR=localhost:9999
# best lit mode
#BEST_LIT_MODE=original_leancop_with_first_advise
# original_leancop
# naive_and_complete
# naive
# full_caching_and_complete
#BEST_LIT_MODE=smart_caching_and_complete
#BEST_LIT_MODE=original_leancop_with_first_advise
BEST_LIT_MODE="limited_smart_on_path_and_targets(2)"

# set TPTP library path
# TPTP=.

#----------
# Functions

leancop()
{
# Input: $SET, $COMP, $TIME_PC
  TLIMIT=`expr $TIME_PC '*' $TIMELIMIT / 111`
  if [ $TLIMIT -eq 0 ]; then TLIMIT=1; fi
  AI_ADVISOR_LOCATION=`echo $AI_ADVISOR | sed "s/\(.*\)\:.*/\\1/"`
  AI_ADVISOR_PORT=`echo $AI_ADVISOR | sed "s/.*\:\(.*\)/\\1/"`
  $PROLOG_PATH $PROLOG_OPTIONS \
  "assert(prolog('$PROLOG')),\
   assert(proof('$PROOF_LAYOUT')),\
   assert(best_lit_mode("$BEST_LIT_MODE")),\
   assert(ai_advisor('$AI_ADVISOR_LOCATION':$AI_ADVISOR_PORT)),\
   ['$PROVER_PATH/leancop_dnf.pl'],\
   leancop_dnf('$FILE',$SET,_),\
   halt."\
   > $OUTPUT &
  PID=$!
  CPU_SEC=0
  trap "rm $OUTPUT; kill $PID >/dev/null 2>&1; exit 2"\
   ALRM XCPU INT QUIT TERM
  while [ $CPU_SEC -lt $TLIMIT ]
  do
    sleep 1
    CPUTIME=`ps -p $PID | sed "s/.*\(..\:..\:..\).*/\\1/" | grep ..:..:..` 
#    CPUTIME=`ps -p $PID -o time | grep :`
    if [ ! -n "$CPUTIME" ]; then break; fi
    CPU_H=`expr 1\`echo $CPUTIME | cut -d':' -f1\` - 100`
    CPU_M=`expr 1\`echo $CPUTIME | cut -d':' -f2\` - 100`
    CPU_S=`expr 1\`echo $CPUTIME | cut -d':' -f3\` - 100`
    CPU_SEC=`expr 3600 '*' $CPU_H + 60 '*' $CPU_M + $CPU_S`
  done
  if [ -n "$CPUTIME" ]
  then rm $OUTPUT; kill $PID >/dev/null 2>&1
  else
    RESULT1=`egrep ' Theorem| Unsatisfiable' $OUTPUT`
    RESULT2=`egrep ' Non-Theorem| Satisfiable' $OUTPUT`
    if [ -n "$RESULT1" -o -n "$RESULT2" ]
    then
      if [ $PRINT_PROOF = yes ]
      then if [ -n "$RESULT1" -o $COMP = y ]; then cat $OUTPUT; fi
      else if [ -n "$RESULT1" ]
           then echo $RESULT1
           else if [ -n "$RESULT2" -a $COMP = y ]
                then echo $RESULT2; fi
           fi
      fi
      if [ $SAVE_PROOF != yes -o -n "$RESULT2" -a $COMP = n ]
      then rm $OUTPUT; else mv $OUTPUT $PROOF_FILE; fi
      if [ -n "$RESULT1" ]; then exit 0; fi
      if [ -n "$RESULT2" -a $COMP = y ]; then exit 1; fi
    else rm $OUTPUT
    fi
  fi
}

#-------------
# Main Program

if [ $# -eq 0 -o $# -gt 3 ]; then
 echo "Usage: $0 <problem file> [<ai_advisor_server_domain:port> [<time limit>]]"
 exit 2
fi

if [ ! -r "$1" ]; then
 echo "Error: File $1 not found" >&2
 exit 2
fi

if [ -n "`echo "$3" | grep '[^0-9]'`" ]; then
 echo "Error: Time $3 is not a number" >&2
 exit 2
fi

if [ $# -le 2 ]
 then TIMELIMIT=600
 else TIMELIMIT=$3
fi

if [ $# -ge 2 ]
 then AI_ADVISOR=$2
fi

FILE=$1
PROOF_FILE=$FILE.proof
OUTPUT=TMP_OUTPUT_leancop_`date +%F_%T_%N`

set +m

# invoke leanCoP core prover with different settings SET
# for time TIME_PC [%]; COMP=y iff settings are complete

SET="[cut,comp(7)]";                 COMP=y; TIME_PC=10; leancop
SET="[conj,def,cut]";                COMP=n; TIME_PC=15; leancop
SET="[nodef,scut,cut]";              COMP=n; TIME_PC=15; leancop
SET="[scut]";                        COMP=n; TIME_PC=10; leancop
SET="[def,cut]";                     COMP=n; TIME_PC=5;  leancop
SET="[conj,nodef,cut]";              COMP=n; TIME_PC=4;  leancop
SET="[def,scut,cut]";                COMP=n; TIME_PC=2;  leancop
SET="[scut,cut]";                    COMP=n; TIME_PC=2;  leancop
SET="[conj,def]";                    COMP=n; TIME_PC=1;  leancop
SET="[reo(40),conj,nodef,scut,cut]"; COMP=n; TIME_PC=4;  leancop
SET="[reo(42),def,scut,cut]";        COMP=n; TIME_PC=4;  leancop
SET="[reo(12),def,scut,cut]";        COMP=n; TIME_PC=2;  leancop
SET="[reo(72),def,scut,cut]";        COMP=n; TIME_PC=2;  leancop
SET="[reo(39),nodef,cut]";           COMP=n; TIME_PC=2;  leancop
SET="[reo(38),conj,def,cut]";        COMP=n; TIME_PC=2;  leancop
SET="[reo(15),conj,def,cut]";        COMP=n; TIME_PC=2;  leancop
SET="[reo(73),conj,def,cut]";        COMP=n; TIME_PC=1;  leancop
SET="[reo(57),conj,def,cut]";        COMP=n; TIME_PC=1;  leancop
SET="[reo(13),conj,nodef,scut,cut]"; COMP=n; TIME_PC=1;  leancop
SET="[reo(59),conj,nodef,scut,cut]"; COMP=n; TIME_PC=1;  leancop
SET="[reo(75),conj,nodef,scut,cut]"; COMP=n; TIME_PC=1;  leancop
SET="[reo(36),conj,nodef,scut]";     COMP=n; TIME_PC=1;  leancop
SET="[reo(16),conj,nodef,scut]";     COMP=n; TIME_PC=1;  leancop
SET="[reo(71),def,scut]";            COMP=n; TIME_PC=1;  leancop
SET="[reo(58),def,scut,cut]";        COMP=n; TIME_PC=1;  leancop
SET="[reo(76),def,scut,cut]";        COMP=n; TIME_PC=1;  leancop
SET="[reo(74),nodef,cut]";           COMP=n; TIME_PC=1;  leancop
SET="[reo(14),nodef,cut]";           COMP=n; TIME_PC=1;  leancop
SET="[reo(37),nodef,scut]";          COMP=n; TIME_PC=1;  leancop
SET="[def]";                         COMP=y; TIME_PC=99; leancop 

echo Timeout
exit 2
