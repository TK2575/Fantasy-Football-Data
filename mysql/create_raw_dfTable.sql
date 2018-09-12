CREATE TABLE `2018_ff`.`raw_df` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `crte_dttm` TIMESTAMP NOT NULL,
  `week` INT NOT NULL,
  `team` VARCHAR(255) NULL,
  `result` VARCHAR(8) NOT NULL,
  `opponent` VARCHAR(255) NULL,
  `slot` VARCHAR(8) NOT NULL,
  `pos` VARCHAR(8) NOT NULL,
  `bench` BOOL NOT NULL,
  `player` VARCHAR(255) NOT NULL,
  `proj` DECIMAL(8,3) NULL,
  `points` DECIMAL(8,3) NULL,
  `stats` VARCHAR(255) NULL,
  PRIMARY KEY (`id`),
  UNIQUE INDEX `idraw_df_UNIQUE` (`id` ASC));
DROP TRIGGER IF EXISTS `2018_ff`.`raw_df_BEFORE_INSERT`;

DELIMITER $$
USE `2018_ff`$$
CREATE DEFINER = CURRENT_USER TRIGGER `2018_ff`.`raw_df_BEFORE_INSERT` BEFORE INSERT ON `raw_df` FOR EACH ROW
BEGIN
SET NEW.crte_dttm = NOW();
END$$
DELIMITER ;