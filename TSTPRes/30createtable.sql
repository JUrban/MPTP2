-- sample SQL table creation for Bliksem___1_12
-- CREATE TABLE `Bliksem___1_12` (
-- `problem` VARCHAR( 255 ) NOT NULL ,
-- `result` CHAR( 3 ) NOT NULL ,
-- `output_status` CHAR( 3 ) NOT NULL ,
-- `time` FLOAT NOT NULL DEFAULT 700,
-- PRIMARY KEY ( `problem` ) ,
-- INDEX ( `result`) , 
-- INDEX (`output_status`) ,
-- INDEX (`time` )
-- ) TYPE = MYISAM ;

-- create the load files 
-- (assumes geoff's results are in the directory geoff_res):
-- for i in `cat 00Systems`; do ./20mksqlloadfile.sh $i; done

-- create load file for all: 
-- for i in `cat 00Systems | sed -e 's/[-.]/_/g'`; do sed -e "s/^\([^\\t]\+\)\\t/\\1\\t$i\\t/" loadsql/"$i"_r1; done > loadsql/all_r1


-- SQL table of all provers
CREATE TABLE `all` (
`problem` VARCHAR( 255 ) NOT NULL ,
`prover` VARCHAR( 255 ) NOT NULL ,
`result` CHAR( 3 ) NOT NULL ,
`output_status` CHAR( 3 ) NOT NULL ,
`time` FLOAT NOT NULL DEFAULT 700,
INDEX ( `problem` ) ,
INDEX ( `prover` ) ,
INDEX ( `result`) , 
INDEX (`output_status`) ,
INDEX (`time` )
) TYPE = MYISAM ;


-- load the all table
LOAD DATA LOCAL INFILE 'all_r1' INTO TABLE `all` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';

-- table tptp of problem divisions, categories, and whether used at CASC21
-- the casc21 field is actually boolean - 1 for casc21 problems, 0 for the rest
-- the category is NULL for:
--  FOF_UNS_EPR_EQU FOF_UNS_EPR_NEQ FOF_UNS_RFO_EQU FOF_UNS_RFO_NEQ
CREATE TABLE `tptp` (
`problem` VARCHAR( 255 ) NOT NULL ,
`division` CHAR( 3 ) NOT NULL ,
`category` CHAR( 3 ) NULL ,
`casc21` tinyint NOT NULL DEFAULT 0,
INDEX ( `problem` ) ,
INDEX ( `division` ) ,
INDEX ( `category`) , 
INDEX (`casc21`) 
) TYPE = MYISAM ;

-- create category and division data from Geoff's SPC files
-- ./40mktptploadfile.sh

-- load the tptp table
LOAD DATA LOCAL INFILE 'tptp_r1' INTO TABLE `tptp` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';


-- table with 300 integers, there should be better way ...
CREATE TABLE `n300` (
`n` SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY
) ENGINE = MYISAM ;

INSERT INTO `n300` (`n`) VALUES (1),(2),(3),(4),(5),(6),(7),(8),(9),(10),(11),(12),(13),(14),(15),(16),(17),(18),(19),(20),(21),(22),(23),(24),(25),(26),(27),(28),(29),(30),(31),(32),(33),(34),(35),(36),(37),(38),(39),(40),(41),(42),(43),(44),(45),(46),(47),(48),(49),(50),(51),(52),(53),(54),(55),(56),(57),(58),(59),(60),(61),(62),(63),(64),(65),(66),(67),(68),(69),(70),(71),(72),(73),(74),(75),(76),(77),(78),(79),(80),(81),(82),(83),(84),(85),(86),(87),(88),(89),(90),(91),(92),(93),(94),(95),(96),(97),(98),(99),(100),(101),(102),(103),(104),(105),(106),(107),(108),(109),(110),(111),(112),(113),(114),(115),(116),(117),(118),(119),(120),(121),(122),(123),(124),(125),(126),(127),(128),(129),(130),(131),(132),(133),(134),(135),(136),(137),(138),(139),(140),(141),(142),(143),(144),(145),(146),(147),(148),(149),(150),(151),(152),(153),(154),(155),(156),(157),(158),(159),(160),(161),(162),(163),(164),(165),(166),(167),(168),(169),(170),(171),(172),(173),(174),(175),(176),(177),(178),(179),(180),(181),(182),(183),(184),(185),(186),(187),(188),(189),(190),(191),(192),(193),(194),(195),(196),(197),(198),(199),(200),(201),(202),(203),(204),(205),(206),(207),(208),(209),(210),(211),(212),(213),(214),(215),(216),(217),(218),(219),(220),(221),(222),(223),(224),(225),(226),(227),(228),(229),(230),(231),(232),(233),(234),(235),(236),(237),(238),(239),(240),(241),(242),(243),(244),(245),(246),(247),(248),(249),(250),(251),(252),(253),(254),(255),(256),(257),(258),(259),(260),(261),(262),(263),(264),(265),(266),(267),(268),(269),(270),(271),(272),(273),(274),(275),(276),(277),(278),(279),(280),(281),(282),(283),(284),(285),(286),(287),(288),(289),(290),(291),(292),(293),(294),(295),(296),(297),(298),(299),(300)

-- MPTP bushy problems
CREATE TABLE `bushy_prob` (
  `problem` varchar(255) NOT NULL,
  KEY `problem` (`problem`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

insert into `bushy_prob` (`problem`) select concat(substring(problem,1,7),"1") FROM `tptp` WHERE problem LIKE "SEU%+2";

-- isabelle problems:
CREATE TABLE `isab_prob` (
  `problem` varchar(255) NOT NULL,
  KEY `problem` (`problem`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

INSERT INTO `isab_prob` (`problem`) VALUES 
('ANA007-1'), ('ANA007-2'), ('ANA008-1'), ('ANA008-2'), ('ANA009-1'),
('ANA009-2'), ('ANA010-1'), ('ANA010-2'), ('ANA011-1'), ('ANA011-2'),
('ANA012-1'), ('ANA012-2'), ('ANA013-1'), ('ANA013-2'), ('ANA014-1'),
('ANA014-2'), ('ANA015-1'), ('ANA015-2'), ('ANA016-1'), ('ANA016-2'),
('ANA017-1'), ('ANA017-2'), ('ANA018-1'), ('ANA018-2'), ('ANA019-1'),
('ANA019-2'), ('ANA020-1'), ('ANA020-2'), ('ANA021-1'), ('ANA021-2'),
('ANA022-1'), ('ANA022-2'), ('ANA023-1'), ('ANA023-2'), ('ANA024-1'),
('ANA024-2'), ('ANA025-1'), ('ANA025-2'), ('ANA026-1'), ('ANA026-2'),
('ANA027-1'), ('ANA027-2'), ('ANA028-1'), ('ANA028-2'), ('ANA029-1'),
('ANA029-2'), ('ANA030-1'), ('ANA030-2'), ('ANA031-1'), ('ANA031-2'),
('ANA032-1'), ('ANA032-2'), ('ANA033-1'), ('ANA034-1'), ('ANA034-2'),
('ANA035-1'), ('ANA035-2'), ('ANA036-1'), ('ANA037-1'), ('ANA037-2'),
('ANA038-1'), ('ANA038-2'), ('ANA039-1'), ('ANA039-2'), ('ANA040-1'),
('ANA040-2'), ('ANA041-1'), ('ANA041-2'), ('ANA042-1'), ('ANA042-2'),
('ANA043-1'), ('ANA043-2'), ('ANA044-1'), ('ANA044-2'), ('ANA045-1'),
('ANA045-2'), ('COL101-1'), ('COL101-2'), ('COL102-1'), ('COL102-2'),
('COL103-1'), ('COL103-2'), ('COL104-1'), ('COL104-2'), ('COL105-1'),
('COL105-2'), ('COL106-1'), ('COL106-2'), ('COL107-1'), ('COL108-1'),
('COL109-1'), ('COL109-2'), ('COL110-1'), ('COL110-2'), ('COL111-1'),
('COL111-2'), ('COL112-1'), ('COL112-2'), ('COL113-1'), ('COL113-2'),
('COL114-1'), ('COL114-2'), ('COL115-1'), ('COL115-2'), ('COL116-1'),
('COL116-2'), ('COL117-1'), ('COL117-2'), ('COL118-1'), ('COL118-2'),
('COL119-1'), ('COL119-2'), ('COL120-1'), ('COL120-2'), ('COL121-1'),
('COL121-2'), ('COL122-1'), ('COL122-2'), ('COL123-1'), ('COL123-2'),
('COL124-1'), ('COL124-2'), ('COM009-1'), ('COM009-2'), ('COM010-1'),
('COM010-2'), ('COM011-1'), ('COM011-2'), ('LAT259-1'), ('LAT259-2'),
('LAT260-1'), ('LAT260-2'), ('LAT261-1'), ('LAT261-2'), ('LAT262-1'),
('LAT262-2'), ('LAT263-1'), ('LAT263-2'), ('LAT264-1'), ('LAT264-2'),
('LAT265-1'), ('LAT265-2'), ('LAT266-1'), ('LAT266-2'), ('LAT267-1'),
('LAT267-2'), ('LAT268-1'), ('LAT268-2'), ('LAT269-1'), ('LAT269-2'),
('LAT270-1'), ('LAT270-2'), ('LAT271-1'), ('LAT271-2'), ('LAT272-1'),
('LAT272-2'), ('LAT273-1'), ('LAT273-2'), ('LAT274-1'), ('LAT274-2'),
('LAT275-1'), ('LAT275-2'), ('LAT276-1'), ('LAT276-2'), ('LAT277-1'),
('LAT277-2'), ('LAT278-1'), ('LAT278-2'), ('LAT279-1'), ('LAT279-2'),
('LAT280-1'), ('LAT280-2'), ('LAT281-1'), ('LAT281-2'), ('LCL429-1'),
('LCL429-2'), ('LCL430-1'), ('LCL430-2'), ('LCL431-1'), ('LCL431-2'),
('LCL432-1'), ('LCL432-2'), ('LCL433-1'), ('LCL433-2'), ('LCL434-1'),
('LCL434-2'), ('LCL435-1'), ('LCL435-2'), ('LCL436-1'), ('LCL436-2'),
('LCL437-1'), ('LCL437-2'), ('LCL438-1'), ('LCL438-2'), ('LCL439-1'),
('LCL439-2'), ('LCL440-1'), ('LCL440-2'), ('LCL441-1'), ('LCL441-2'),
('LCL442-1'), ('LCL442-2'), ('LCL443-1'), ('LCL443-2'), ('LCL444-1'),
('LCL444-2'), ('LCL445-1'), ('LCL445-2'), ('LCL446-1'), ('LCL446-2'),
('LCL447-1'), ('LCL447-2'), ('PUZ062-1'), ('PUZ062-2'), ('PUZ063-1'),
('PUZ063-2'), ('PUZ064-1'), ('PUZ064-2'), ('SET818-1'), ('SET818-2'),
('SET819-1'), ('SET819-2'), ('SET820-1'), ('SET820-2'), ('SET821-1'),
('SET821-2'), ('SET822-1'), ('SET822-2'), ('SET823-1'), ('SET823-2'),
('SET824-1'), ('SET824-2'), ('SET825-1'), ('SET825-2'), ('SET826-1'),
('SET826-2'), ('SET827-1'), ('SET827-2'), ('SET828-1'), ('SET828-2'),
('SET829-1'), ('SET829-2'), ('SET830-1'), ('SET830-2'), ('SET831-1'),
('SET831-2'), ('SET832-1'), ('SET832-2'), ('SET833-1'), ('SET833-2'),
('SET834-1'), ('SET834-2'), ('SET835-1'), ('SET835-2'), ('SET836-1'),
('SET836-2'), ('SET837-1'), ('SET837-2'), ('SET838-1'), ('SET838-2'),
('SET839-1'), ('SET839-2'), ('SET840-1'), ('SET840-2'), ('SET841-1'),
('SET841-2'), ('SET842-1'), ('SET842-2'), ('SET843-1'), ('SET843-2'),
('SET844-1'), ('SET844-2'), ('SET845-1'), ('SET845-2'), ('SET846-1'),
('SET846-2'), ('SET847-1'), ('SET847-2'), ('SET848-1'), ('SET848-2'),
('SET849-1'), ('SET849-2'), ('SET850-1'), ('SET850-2'), ('SET851-1'),
('SET851-2'), ('SET852-1'), ('SET852-2'), ('SET853-1'), ('SET853-2'),
('SET854-1'), ('SET854-2'), ('SET855-1'), ('SET855-2'), ('SET856-1'),
('SET856-2'), ('SET857-1'), ('SET857-2'), ('SET858-1'), ('SET858-2'),
('SET859-1'), ('SET859-2'), ('SET860-1'), ('SET860-2'), ('SET861-1'),
('SET861-2'), ('SET862-1'), ('SET862-2'), ('SET863-1'), ('SET863-2'),
('SET864-1'), ('SET864-2'), ('SET865-1'), ('SET865-2'), ('SWV239-1'),
('SWV239-2'), ('SWV240-1'), ('SWV240-2'), ('SWV241-1'), ('SWV241-2'),
('SWV242-1'), ('SWV242-2'), ('SWV243-1'), ('SWV243-2'), ('SWV244-1'),
('SWV244-2'), ('SWV245-1'), ('SWV245-2'), ('SWV246-1'), ('SWV246-2'),
('SWV247-1'), ('SWV247-2'), ('SWV248-1'), ('SWV248-2'), ('SWV249-1'),
('SWV249-2'), ('SWV250-1'), ('SWV250-2'), ('SWV251-1'), ('SWV251-2'),
('SWV252-1'), ('SWV252-2'), ('SWV253-1'), ('SWV253-2'), ('SWV254-1'),
('SWV254-2'), ('SWV255-1'), ('SWV255-2'), ('SWV256-1'), ('SWV256-2'),
('SWV257-1'), ('SWV257-2'), ('SWV258-1'), ('SWV258-2'), ('SWV259-1'),
('SWV259-2'), ('SWV260-1'), ('SWV260-2'), ('SWV261-1'), ('SWV261-2'),
('SWV262-1'), ('SWV262-2'), ('SWV263-1'), ('SWV263-2'), ('SWV264-1'),
('SWV264-2'), ('SWV265-1'), ('SWV265-2'), ('SWV266-1'), ('SWV266-2'),
('SWV267-1'), ('SWV267-2'), ('SWV268-1'), ('SWV268-2'), ('SWV269-1'),
('SWV269-2'), ('SWV270-1'), ('SWV270-2'), ('SWV271-1'), ('SWV271-2'),
('SWV272-1'), ('SWV272-2'), ('SWV273-1'), ('SWV273-2'), ('SWV274-1'),
('SWV274-2'), ('SWV275-1'), ('SWV275-2'), ('SWV276-1'), ('SWV276-2'),
('SWV277-1'), ('SWV277-2'), ('SWV278-1'), ('SWV278-2'), ('SWV279-1'),
('SWV279-2'), ('SWV280-1'), ('SWV280-2'), ('SWV281-1'), ('SWV281-2'),
('SWV282-1'), ('SWV282-2'), ('SWV283-1'), ('SWV283-2'), ('SWV284-1'),
('SWV284-2'), ('SWV285-1'), ('SWV285-2'), ('SWV286-1'), ('SWV286-2'),
('SWV287-1'), ('SWV287-2'), ('SWV288-1'), ('SWV288-2'), ('SWV289-1'),
('SWV289-2'), ('SWV290-1'), ('SWV290-2'), ('SWV291-1'), ('SWV291-2'),
('SWV292-1'), ('SWV292-2'), ('SWV293-1'), ('SWV293-2'), ('SWV294-1'),
('SWV294-2'), ('SWV295-1'), ('SWV295-2'), ('SWV296-1'), ('SWV296-2'),
('SWV297-1'), ('SWV298-1'), ('SWV298-2'), ('SWV299-1'), ('SWV299-2'),
('SWV300-1'), ('SWV300-2'), ('SWV301-1'), ('SWV301-2'), ('SWV302-1'),
('SWV302-2'), ('SWV303-1'), ('SWV303-2'), ('SWV304-1'), ('SWV304-2'),
('SWV305-1'), ('SWV305-2'), ('SWV306-1'), ('SWV306-2'), ('SWV307-1'),
('SWV307-2'), ('SWV308-1'), ('SWV308-2'), ('SWV309-1'), ('SWV309-2'),
('SWV310-1'), ('SWV310-2'), ('SWV311-1'), ('SWV311-2'), ('SWV312-1'),
('SWV312-2'), ('SWV313-1'), ('SWV313-2'), ('SWV314-1'), ('SWV314-2'),
('SWV315-1'), ('SWV315-2'), ('SWV316-1'), ('SWV316-2'), ('SWV317-1'),
('SWV317-2'), ('SWV318-1'), ('SWV318-2'), ('SWV319-1'), ('SWV319-2'),
('SWV320-1'), ('SWV320-2'), ('SWV321-1'), ('SWV321-2'), ('SWV322-1'),
('SWV322-2'), ('SWV323-1'), ('SWV323-2'), ('SWV324-1'), ('SWV324-2'),
('SWV325-1'), ('SWV325-2'), ('SWV326-1'), ('SWV326-2'), ('SWV327-1'),
('SWV327-2'), ('SWV328-1'), ('SWV328-2'), ('SWV329-1'), ('SWV329-2'),
('SWV330-1'), ('SWV330-2'), ('SWV331-1'), ('SWV331-2'), ('SWV332-1'),
('SWV332-2'), ('SWV333-1'), ('SWV333-2'), ('SWV334-1'), ('SWV334-2'),
('SWV335-1'), ('SWV335-2'), ('SWV336-1'), ('SWV336-2'), ('SWV337-1'),
('SWV337-2'), ('SWV338-1'), ('SWV338-2'), ('SWV339-1'), ('SWV339-2'),
('SWV340-1'), ('SWV340-2'), ('SWV341-1'), ('SWV341-2'), ('SWV342-1'),
('SWV342-2'), ('SWV343-1'), ('SWV343-2'), ('SWV344-1'), ('SWV344-2'),
('SWV345-1'), ('SWV345-2'), ('SWV346-1'), ('SWV346-2'), ('SWV347-1'),
('SWV347-2'), ('SWV348-1'), ('SWV348-2'), ('SWV349-1'), ('SWV349-2'),
('SWV350-1'), ('SWV350-2'), ('SWV351-1'), ('SWV351-2'), ('SWV352-1'),
('SWV352-2'), ('SWV353-1'), ('SWV353-2'), ('SWV354-1'), ('SWV354-2'),
('SWV355-1'), ('SWV355-2'), ('SWV356-1'), ('SWV356-2'), ('SWV357-1'),
('SWV357-2'), ('SWV358-1'), ('SWV358-2'), ('SWV359-1'), ('SWV359-2'),
('SWV360-1'), ('SWV360-2'), ('SWV361-1'), ('SWV361-2'), ('SWV362-1'),
('SWV362-2'), ('SWV363-1'), ('SWV363-2'), ('SWV364-1'), ('SWV364-2');



-- sample loading of the table form a file
-- LOAD DATA LOCAL INFILE 'Bliksem___1_12_r1' INTO TABLE `Bliksem___1_12` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';

-- the rest of tables (generate this from the 00Systems file):
-- cat 00Systems | sed -e 's/[-.]/_/g'| sed -e 's/\(.*\)/CREATE TABLE `\1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT\ 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;/'

CREATE TABLE `Bliksem___1_12` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `CARINE___0_734` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `CiME___2_01` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Darwin___1_3` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Darwin___1_4_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `DarwinFM___1_3g` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `DarwinFM___1_4_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `DCTP___10_21p` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `DCTP___1_31` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `DCTP___1_31_EPR` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `E___0_91` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `E___0_99` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `E___0_999` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `E_KRHyper___1_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `EP___0_999` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `EQP___0_9d` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Equinox___1_0b` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Equinox___1_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `E_SETHEO___csp04` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `E_SETHEO___csp04_SAT` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Fampire___1_3` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Faust___1_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `FDP___0_9_16` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Fiesta___2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Gandalf___c_2_6` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Geo___2006j` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Geo___2007f` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `GrAnDe___1_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `iProver___0_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `leanCoP___2_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `LeanTAP___2_3` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Mace2___2_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Mace4___0307` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Mace4___0607` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Matita___0_1_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Metis___2_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Otter___3_3` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Paradox___2_0b` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Paradox___2_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Prover9___0307` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Prover9___0607` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SCOTT___6_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SETHEO___3_3` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SNARK___20061020` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SOS___1_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SOS___2_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SPASS___2_2` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SPASS___3_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `SRASS___0_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `S_SETHEO___0_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Theo___2006` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Vampire___8_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Vampire___9_0` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Waldmeister___806` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `zChaff___2004_11_15` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;
CREATE TABLE `Zenon___0_4_1` (`problem` VARCHAR( 255 ) NOT NULL , `result` CHAR( 3 ) NOT NULL , `output_status` CHAR( 3 ) NOT NULL , `time` FLOAT NOT NULL DEFAULT 700, PRIMARY KEY ( `problem` ) , INDEX ( `result`) , INDEX (`output_status`) , INDEX (`time` )) TYPE = MYISAM ;





-- the rest of loading (generate this from the 00Systems file):
-- cat 00Systems | sed -e 's/[-.]/_/g'| sed -e "s/\(.*\)/LOAD DATA LOCAL INFILE \'\1_r1\' INTO TABLE \`\1\` FIELDS TERMINATED BY \'\\\t\' ENCLOSED BY \'\"\' ESCAPED BY \'\\\\\\\' LINES TERMINATED BY \'\\\n\';/"

LOAD DATA LOCAL INFILE 'Bliksem___1_12_r1' INTO TABLE `Bliksem___1_12` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'CARINE___0_734_r1' INTO TABLE `CARINE___0_734` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'CiME___2_01_r1' INTO TABLE `CiME___2_01` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Darwin___1_3_r1' INTO TABLE `Darwin___1_3` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Darwin___1_4_1_r1' INTO TABLE `Darwin___1_4_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'DarwinFM___1_3g_r1' INTO TABLE `DarwinFM___1_3g` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'DarwinFM___1_4_1_r1' INTO TABLE `DarwinFM___1_4_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'DCTP___10_21p_r1' INTO TABLE `DCTP___10_21p` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'DCTP___1_31_r1' INTO TABLE `DCTP___1_31` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'DCTP___1_31_EPR_r1' INTO TABLE `DCTP___1_31_EPR` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'E___0_91_r1' INTO TABLE `E___0_91` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'E___0_99_r1' INTO TABLE `E___0_99` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'E___0_999_r1' INTO TABLE `E___0_999` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'E_KRHyper___1_0_r1' INTO TABLE `E_KRHyper___1_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'EP___0_999_r1' INTO TABLE `EP___0_999` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'EQP___0_9d_r1' INTO TABLE `EQP___0_9d` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Equinox___1_0b_r1' INTO TABLE `Equinox___1_0b` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Equinox___1_2_r1' INTO TABLE `Equinox___1_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'E_SETHEO___csp04_r1' INTO TABLE `E_SETHEO___csp04` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'E_SETHEO___csp04_SAT_r1' INTO TABLE `E_SETHEO___csp04_SAT` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Fampire___1_3_r1' INTO TABLE `Fampire___1_3` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Faust___1_0_r1' INTO TABLE `Faust___1_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'FDP___0_9_16_r1' INTO TABLE `FDP___0_9_16` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Fiesta___2_r1' INTO TABLE `Fiesta___2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Gandalf___c_2_6_r1' INTO TABLE `Gandalf___c_2_6` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Geo___2006j_r1' INTO TABLE `Geo___2006j` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Geo___2007f_r1' INTO TABLE `Geo___2007f` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'GrAnDe___1_1_r1' INTO TABLE `GrAnDe___1_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'iProver___0_2_r1' INTO TABLE `iProver___0_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'leanCoP___2_0_r1' INTO TABLE `leanCoP___2_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'LeanTAP___2_3_r1' INTO TABLE `LeanTAP___2_3` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Mace2___2_2_r1' INTO TABLE `Mace2___2_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Mace4___0307_r1' INTO TABLE `Mace4___0307` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Mace4___0607_r1' INTO TABLE `Mace4___0607` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Matita___0_1_0_r1' INTO TABLE `Matita___0_1_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Metis___2_0_r1' INTO TABLE `Metis___2_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Otter___3_3_r1' INTO TABLE `Otter___3_3` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Paradox___2_0b_r1' INTO TABLE `Paradox___2_0b` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Paradox___2_2_r1' INTO TABLE `Paradox___2_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Prover9___0307_r1' INTO TABLE `Prover9___0307` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Prover9___0607_r1' INTO TABLE `Prover9___0607` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SCOTT___6_1_r1' INTO TABLE `SCOTT___6_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SETHEO___3_3_r1' INTO TABLE `SETHEO___3_3` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SNARK___20061020_r1' INTO TABLE `SNARK___20061020` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SOS___1_0_r1' INTO TABLE `SOS___1_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SOS___2_0_r1' INTO TABLE `SOS___2_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SPASS___2_2_r1' INTO TABLE `SPASS___2_2` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SPASS___3_0_r1' INTO TABLE `SPASS___3_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'SRASS___0_1_r1' INTO TABLE `SRASS___0_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'S_SETHEO___0_0_r1' INTO TABLE `S_SETHEO___0_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Theo___2006_r1' INTO TABLE `Theo___2006` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Vampire___8_1_r1' INTO TABLE `Vampire___8_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Vampire___9_0_r1' INTO TABLE `Vampire___9_0` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Waldmeister___806_r1' INTO TABLE `Waldmeister___806` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'zChaff___2004_11_15_r1' INTO TABLE `zChaff___2004_11_15` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
LOAD DATA LOCAL INFILE 'Zenon___0_4_1_r1' INTO TABLE `Zenon___0_4_1` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
 
