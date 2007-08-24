-- sample SQL table creation for CARINE___0_734
CREATE TABLE `CARINE___0_734` (
`problem` VARCHAR( 255 ) NOT NULL ,
`result` CHAR( 3 ) NOT NULL ,
`output_type` CHAR( 3 ) NOT NULL ,
`time` FLOAT NOT NULL DEFAULT 700,
PRIMARY KEY ( `problem` ) ,
INDEX ( `result`) , 
INDEX (`output_type`) ,
INDEX (`time` )
) TYPE = MYISAM ;

-- sample loading of the table form a file
LOAD DATA LOCAL INFILE 'CARINE___0.734_r1' INTO TABLE `CARINE___0_734` FIELDS TERMINATED BY '\t' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n';
