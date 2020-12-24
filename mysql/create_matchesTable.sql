CREATE TABLE `2020_ff`.`matches` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `crte_dttm` TIMESTAMP NOT NULL,
  `week` INT NOT NULL,
  `team` VARCHAR(255) NULL,
  `result` VARCHAR(8) NOT NULL,
  `opponent` VARCHAR(255) NULL,
  `points` DECIMAL(8,3) NULL,
  `net_vs_proj` DECIMAL(8,3) NULL,
  `bench_points` DECIMAL(8,3) NULL,
  `optimal_points` DECIMAL(8,3) NULL,
  `playoffs` BOOL NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE INDEX `idmatches_UNIQUE` (`id` ASC));
DROP TRIGGER IF EXISTS `2020_ff`.`matches_BEFORE_INSERT`;

DELIMITER $$
USE `2020_ff`$$
CREATE DEFINER = CURRENT_USER TRIGGER `2020_ff`.`matches_BEFORE_INSERT` BEFORE INSERT ON `matches` FOR EACH ROW
BEGIN
SET NEW.crte_dttm = NOW();
END$$
DELIMITER ;