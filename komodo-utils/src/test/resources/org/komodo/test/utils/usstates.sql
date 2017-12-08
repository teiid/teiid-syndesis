-- ----------------------------
-- Table structure for states
-- ----------------------------
DROP TABLE IF EXISTS `states`;
CREATE TABLE `states` (
  `state` varchar(22) NOT NULL,
  `state_code` char(2) NOT NULL,
  PRIMARY KEY (`state_code`)
);

-- ----------------------------
-- Table structure for cities
-- ----------------------------
DROP TABLE IF EXISTS `cities`;
CREATE TABLE `cities` (
  `city` varchar(50) NOT NULL,
  `state_code` char(2) NOT NULL,
  CONSTRAINT pk_cityStateCode PRIMARY KEY (`city`,`state_code`),
  FOREIGN KEY (`state_code`) REFERENCES `states` (`state_code`) ON DELETE CASCADE
);

-- ----------------------------
-- Table structure for districts
-- ----------------------------
DROP TABLE IF EXISTS `districts`;
CREATE TABLE `districts` (
  `city` varchar(50) NOT NULL,
  `state_code` char(2) NOT NULL,
  `zip` bigint(5) NOT NULL,
  `latitude` double NOT NULL,
  `longitude` double NOT NULL,
  `county` varchar(50) NOT NULL,
  CONSTRAINT pk_cityZip PRIMARY KEY (`city`,`zip`),
  FOREIGN KEY (`city`,`state_code`) REFERENCES `cities` (`city`,`state_code`) ON DELETE CASCADE
);

-- ----------------------------
-- Records 
-- ----------------------------
INSERT INTO `states` VALUES ('Alaska', 'AK');
INSERT INTO `states` VALUES ('Alabama', 'AL');
INSERT INTO `states` VALUES ('Arkansas', 'AR');
INSERT INTO `states` VALUES ('Arizona', 'AZ');
INSERT INTO `states` VALUES ('California', 'CA');
INSERT INTO `states` VALUES ('Colorado', 'CO');
INSERT INTO `states` VALUES ('Connecticut', 'CT');
INSERT INTO `states` VALUES ('District of Columbia', 'DC');
INSERT INTO `states` VALUES ('Delaware', 'DE');
INSERT INTO `states` VALUES ('Florida', 'FL');
INSERT INTO `states` VALUES ('Georgia', 'GA');
INSERT INTO `states` VALUES ('Hawaii', 'HI');
INSERT INTO `states` VALUES ('Iowa', 'IA');
INSERT INTO `states` VALUES ('Idaho', 'ID');
INSERT INTO `states` VALUES ('Illinois', 'IL');
INSERT INTO `states` VALUES ('Indiana', 'IN');
INSERT INTO `states` VALUES ('Kansas', 'KS');
INSERT INTO `states` VALUES ('Kentucky', 'KY');
INSERT INTO `states` VALUES ('Louisiana', 'LA');
INSERT INTO `states` VALUES ('Massachusetts', 'MA');
INSERT INTO `states` VALUES ('Maryland', 'MD');
INSERT INTO `states` VALUES ('Maine', 'ME');
INSERT INTO `states` VALUES ('Michigan', 'MI');
INSERT INTO `states` VALUES ('Minnesota', 'MN');
INSERT INTO `states` VALUES ('Missouri', 'MO');
INSERT INTO `states` VALUES ('Mississippi', 'MS');
INSERT INTO `states` VALUES ('Montana', 'MT');
INSERT INTO `states` VALUES ('North Carolina', 'NC');
INSERT INTO `states` VALUES ('North Dakota', 'ND');
INSERT INTO `states` VALUES ('Nebraska', 'NE');
INSERT INTO `states` VALUES ('New Hampshire', 'NH');
INSERT INTO `states` VALUES ('New Jersey', 'NJ');
INSERT INTO `states` VALUES ('New Mexico', 'NM');
INSERT INTO `states` VALUES ('Nevada', 'NV');
INSERT INTO `states` VALUES ('New York', 'NY');
INSERT INTO `states` VALUES ('Ohio', 'OH');
INSERT INTO `states` VALUES ('Oklahoma', 'OK');
INSERT INTO `states` VALUES ('Oregon', 'OR');
INSERT INTO `states` VALUES ('Pennsylvania', 'PA');
INSERT INTO `states` VALUES ('Rhode Island', 'RI');
INSERT INTO `states` VALUES ('South Carolina', 'SC');
INSERT INTO `states` VALUES ('South Dakota', 'SD');
INSERT INTO `states` VALUES ('Tennessee', 'TN');
INSERT INTO `states` VALUES ('Texas', 'TX');
INSERT INTO `states` VALUES ('Utah', 'UT');
INSERT INTO `states` VALUES ('Virginia', 'VA');
INSERT INTO `states` VALUES ('Vermont', 'VT');
INSERT INTO `states` VALUES ('Washington', 'WA');
INSERT INTO `states` VALUES ('Wisconsin', 'WI');
INSERT INTO `states` VALUES ('West Virginia', 'WV');
INSERT INTO `states` VALUES ('Wyoming', 'WY');


