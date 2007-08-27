-- all vampire successes (6633)
SELECT * FROM `Vampire___9_0` WHERE result="CSA" or result="THM" or result="UNS";
-- all E successes (6498)
SELECT * FROM `E___0_999` WHERE result="CSA" or result="THM" or result="UNS";
-- all SPASS successes (5381)
SELECT * FROM `SPASS___3_0` WHERE result="CSA" or result="THM" or result="UNS";

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

-- select SPASS results on CASC21 problems
SELECT `SPASS___3_0`.*,tptp.* FROM `SPASS___3_0`,tptp WHERE `SPASS___3_0`.problem=tptp.problem and tptp.casc21=1

-- count SPASS successes on CASC21 problems
SELECT count(*) FROM `SPASS___3_0`,tptp WHERE `SPASS___3_0`.problem=tptp.problem and tptp.casc21=1 and (`SPASS___3_0`.result="THM" or `SPASS___3_0`.result="UNS" or `SPASS___3_0`.result="SAT" or `SPASS___3_0`.result="CSA")
