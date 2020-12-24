CREATE TABLE `2020_ff`.`roster_expanded` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `crte_dttm` TIMESTAMP NOT NULL,
  `week` INT NOT NULL,
  `team` VARCHAR(255) NULL,
  `bench` BOOL NOT NULL,
  `slot` VARCHAR(8) NOT NULL,
  `pos` VARCHAR(8) NOT NULL,
  `player` VARCHAR(255) NOT NULL,
  `points` DECIMAL(8,3) NULL,
  `proj` DECIMAL(8,3) NULL,
  `rank_ovrl` INT NULL,
  `rank_proj` INT NULL,
  `rank_pos` INT NULL,
  `perc_owned` DECIMAL(4,2) NOT NULL,
  `pass_yds` INT NULL,
  `pass_td` INT NULL,
  `pass_int` INT NULL,
  `rush_att` INT NULL,
  `rush_yds` INT NULL,
  `rush_td` INT NULL,
  `rec_tgt` INT NULL,
  `rec` INT NULL,
  `rec_yds` INT NULL,
  `rec_td` INT NULL,
  `ret_td` INT NULL,
  `2pt` INT NULL,
  `fum_lost` INT NULL,
  `fg_0-19` INT NULL,
  `fg_20-29` INT NULL,
  `fg_30-39` INT NULL,
  `fg_40-49` INT NULL,
  `fg_50+` INT NULL,
  `pat` INT NULL,
  `pts_vs` INT NULL,
  `sack` INT NULL,
  `safety` INT NULL,
  `def_int` INT NULL,
  `fum_rec` INT NULL,
  `def_td` INT NULL,
  `blk_kick` INT NULL,
  `yds_allow` INT NULL,
  PRIMARY KEY (`id`),
  UNIQUE INDEX `idroster_expanded_UNIQUE` (`id` ASC));
DROP TRIGGER IF EXISTS `2020_ff`.`roster_expanded_BEFORE_INSERT`;

DELIMITER $$
USE `2020_ff`$$
CREATE DEFINER = CURRENT_USER TRIGGER `2020_ff`.`roster_expanded_BEFORE_INSERT` BEFORE INSERT ON `roster_expanded` FOR EACH ROW
BEGIN
SET NEW.crte_dttm = NOW();
END$$
DELIMITER ;
