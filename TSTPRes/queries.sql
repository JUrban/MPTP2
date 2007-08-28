-- all vampire successes (6633)
SELECT * FROM `Vampire___9_0` WHERE result="CSA" or result="THM" or result="UNS" or result="SAT";
-- all E successes (6978)
SELECT * FROM `E___0_999` WHERE result="CSA" or result="THM" or result="UNS" or result="SAT";
-- all SPASS successes (5842)
SELECT * FROM `SPASS___3_0` WHERE result="CSA" or result="THM" or result="UNS" or result="SAT";

-- count of vampire,fampire,E,SPASS,Leancop,SRASS sucesses on the SEU (Mizar) problems
-- 358
SELECT count(*) FROM `Vampire___9_0` WHERE (problem like "SEU%") and result="THM";
-- 405
SELECT count(*) FROM `Fampire___1_3` WHERE (problem like "SEU%") and result="THM";
-- 316
SELECT count(*) FROM `E___0_999` WHERE (problem like "SEU%") and result="THM";
-- 340
SELECT count(*) FROM `SPASS___3_0` WHERE (problem like "SEU%") and result="THM";
-- 301
SELECT count(*) FROM `leanCoP___2_0` WHERE (problem like "SEU%") and result="THM";
-- 414
SELECT count(*) FROM `SRASS___0_1` WHERE (problem like "SEU%") and result="THM";

-- order provers by their results on SEU problems
SELECT count( * ) as c , prover FROM `all` WHERE problem like "SEU%" and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC

-- order provers by their results on MPTPChallenge chainy  problems (SEU%+2)
SELECT count( * ) as c , prover FROM `all` WHERE problem like "SEU%+2" and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC

-- order provers by their results on isabelle problems (needs the isab_prob table)
SELECT count( * ) as c , prover FROM `all`,`isab_prob` WHERE `all`.problem= `isab_prob`.problem and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC


-- select SPASS results on CASC21 problems
SELECT `SPASS___3_0`.*,tptp.* FROM `SPASS___3_0`,tptp WHERE `SPASS___3_0`.problem=tptp.problem and tptp.casc21=1

-- count SPASS successes on CASC21 problems
SELECT count(*) FROM `SPASS___3_0`,tptp WHERE `SPASS___3_0`.problem=tptp.problem and tptp.casc21=1 and (`SPASS___3_0`.result="THM" or `SPASS___3_0`.result="UNS" or `SPASS___3_0`.result="SAT" or `SPASS___3_0`.result="CSA")

-- doing previous using the `all` table: (498)
SELECT count(*) FROM `all`,tptp WHERE `all`.problem=tptp.problem and tptp.casc21=1 and prover="SPASS___3_0" and (`all`.result="THM" or `all`.result="UNS" or `all`.result="SAT" or `all`.result="CSA")

-- counting  E and SPASS success on CASC21 problems (681)
SELECT count(distinct tptp.problem) FROM `all`,tptp WHERE `all`.problem=tptp.problem and tptp.casc21=1 and (prover="SPASS___3_0" or prover="E___0_999") and (`all`.result="THM" or `all`.result="UNS" or `all`.result="SAT" or `all`.result="CSA")

-- select casc21 data, sotr them by prover name
SELECT `all`.prover,`all`.problem,`all`.result,`all`.output_status,`all`.time,tptp.* FROM `all`,tptp WHERE `all`.problem=tptp.problem and tptp.casc21=1 order by `all`.prover asc

-- order systems by their success on CASC21 problems
SELECT count( * ) as c , prover FROM `all` , tptp WHERE `all`.problem = tptp.problem and tptp.casc21 = 1 and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC

-- order systems by their success on CASC21 divisions
SELECT count( * ) as c , prover,division FROM `all` , tptp WHERE `all`.problem = tptp.problem and tptp.casc21 = 1 and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover,division  ORDER BY division,`c`  DESC

-- the same for all of tptp
SELECT count( * ) as c , prover,division FROM `all` , tptp WHERE `all`.problem = tptp.problem and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover,division  ORDER BY division,`c`  DESC
