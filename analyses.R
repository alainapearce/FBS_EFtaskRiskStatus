# This script was written by Alaina Pearce in Summer 2022
# to set up the analyses for the Task EF x Risk Status paper
#
#     Copyright (C) 2022 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

############ Basic Data Load/Setup ############
# need to uncomment if running independently - not needed if compiling with 2022-01-27_PSU_CBBCsem.Rmd

# library(car)
# library(lme4)
# library(lmerTest)

# source('setup.R')
# source('functions.R')

## Go-NoGo ####
# False Alarms
gng_fa_model <- lm(all_p_nogo_fa ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_fa_anova <- anova(gng_fa_model)

r01_gng_ses <- r01_gng[!is.na(r01_gng$income) & !is.na(r01_gng$mom_ed), ]
r01_gng_ses$fa_pred <- predict(gng_fa_model, type = 'response')

# hits
gng_hits_model <- lm(all_p_go_hit ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_hits_anova <- anova(gng_hits_model)

# reaction time
gng_rt_model <- lm(all_rt_mean_go_hit ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_rt_anova <- anova(gng_rt_model)

# d'prime
gng_dprime_model <- lm(all_d_prime_ll ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_dprime_anova <- anova(gng_dprime_model)

## Stop-Signal Task ####

# SSRT - task design
sstdesign_ssrt_model <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS*ED + (1|sub), data = r01_sst_long[r01_sst_long$ncond_racehorse_good == 4, ])
sst_ssrtdesign_anova <- Anova(sstdesign_ssrt_model, test.statistic = 'F')

r01_sst_ses <- r01_sst_long[!is.na(r01_sst_long$income) & !is.na(r01_sst_long$mom_ed) & r01_sst_long$ncond_racehorse_good == 4, ]
r01_sst_ses$ssrt_pred <- predict(sstdesign_ssrt_model, type = 'response')

# SSRT - all
sst_ssrt_model <- lm(all_ssrt_int ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssrt_anova <- Anova(sst_ssrt_model, test.statistic = 'F')

r01_sst_all_ses <- r01_sst_cond[!is.na(r01_sst_cond$income) & !is.na(r01_sst_cond$mom_ed) & r01_sst_cond$all_racehorse_check == 1, ]
r01_sst_all_ses$ssrt_pred <- predict(sst_ssrt_model, type = 'response')

# SSRT - ED condition
sst_ssrt_EDmodel <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + ED*risk_status_mom + (1|sub), data = r01_sst_EDlong)
sst_ssrt_EDanova <- Anova(sst_ssrt_EDmodel, test.statistic = 'F')

r01_sst_ED_ses <- r01_sst_EDlong[!is.na(r01_sst_EDlong$income) & !is.na(r01_sst_EDlong$mom_ed), ]
r01_sst_ED_ses$ssrt_pred <- predict(sst_ssrt_EDmodel, type = 'response')

# SSRT - PS condition
sst_ssrt_PSmodel <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS*risk_status_mom + (1|sub), data = r01_sst_PSlong)
sst_ssrt_PSanova <- Anova(sst_ssrt_PSmodel, test.statistic = 'F')

r01_sst_PS_ses <- r01_sst_PSlong[!is.na(r01_sst_PSlong$income) & !is.na(r01_sst_PSlong$mom_ed), ]
r01_sst_PS_ses$ssrt_pred <- predict(sst_ssrt_PSmodel, type = 'response')

# SSD - task design
sstdesign_ssd_model <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS*ED + (1|sub), data = r01_sst_long[r01_sst_long$ncond_racehorse_good == 4, ])
sst_ssddesign_anova <- Anova(sstdesign_ssd_model, test.statistic = 'F')

r01_sst_ses$ssd_pred <- predict(sstdesign_ssd_model, type = 'response')

# SSD - all
sst_ssd_model <- lm(all_ssd ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssd_anova <- Anova(sst_ssd_model, test.statistic = 'F')

r01_sst_all_ses$ssd_pred <- predict(sst_ssd_model, type = 'response')

# SSD - ED condition
sst_ssd_EDmodel <- lmer(ssd ~ mom_ed + income + sex + age_yr + ED*risk_status_mom + (1|sub), data = r01_sst_EDlong)
sst_ssd_EDanova <- Anova(sst_ssd_EDmodel, test.statistic = 'F')

r01_sst_ED_ses$ssd_pred <- predict(sst_ssd_EDmodel, type = 'response')

# SSD - PS condition
sst_ssd_PSmodel <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS*risk_status_mom + (1|sub), data = r01_sst_PSlong)
sst_ssd_PSanova <- Anova(sst_ssd_PSmodel, test.statistic = 'F')

r01_sst_PS_ses$ssd_pred <- predict(sst_ssd_PSmodel, type = 'response')


## N-Back ####

# balanced accuracy
nback_balacc_mod <- lmer(p_target_ba ~ mom_ed + income + sex + age_yr + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_balacc_anova <- Anova(nback_balacc_mod, test.statistic = 'F')

r01_nback_ses <- r01_nback[!is.na(r01_nback$income) & !is.na(r01_nback$mom_ed) & r01_nback$ses == 1, ]
r01_nback_ses$balacc_pred <- predict(nback_balacc_mod, type = 'response')

# d'
nback_dprime_mod <- lmer(dprime ~ mom_ed + income + sex + age_yr + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_dprime_anova <- Anova(nback_dprime_mod, test.statistic = 'F')

r01_nback_ses$dprime_pred <- predict(nback_dprime_mod, type = 'response')

## Exploratory Analyses with Body Fat Percent ####
## Go-NoGo- Exploratory ####
# False Alarms
gng_fa_bfp_mod <- lm(all_p_nogo_fa ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_fa_bfp_sum <- summary(gng_fa_bfp_mod)

# hits
gng_hits_bfp_mod <- lm(all_p_go_hit ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_hits_bfp_sum <- summary(gng_hits_bfp_mod)

# reaction time
gng_rt_bfp_mod <- lm(all_rt_mean_go_hit ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_rt_bfp_sum <- summary(gng_rt_bfp_mod)

# d'prime
gng_dprime_bfp_mod <- lm(all_d_prime_ll ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_dprime_bfp_sum <- summary(gng_dprime_bfp_mod)

## Stop-Signal Task ####

# SSRT - all
sst_ssrt_bfp_mod <- lm(all_ssrt_int ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssrt_bfp_sum <- summary(sst_ssrt_bfp_mod)

# SSRT - ED condition
sst_ssrt_bfp_EDmod <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + ED*dxa_total_body_perc_fat + (1|sub), data = r01_sst_EDlong)
sst_ssrt_bfp_EDsum <- summary(sst_ssrt_bfp_EDmod)

sst_ssrt_bfp_EDmod_noint <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + ED*dxa_total_body_perc_fat + (1|sub), data = r01_sst_EDlong)
sst_ssrt_bfp_EDsum_noint <- summary(sst_ssrt_bfp_EDmod_noint)

# SSRT - PS condition
sst_ssrt_bfp_PSmod <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS*dxa_total_body_perc_fat + (1|sub), data = r01_sst_PSlong)
sst_ssrt_bfp_PSsum<- summary(sst_ssrt_bfp_PSmod)

sst_ssrt_bfp_PSmod_noint <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS*dxa_total_body_perc_fat + (1|sub), data = r01_sst_PSlong)
sst_ssrt_bfp_PSsum_noint <- summary(sst_ssrt_bfp_PSmod_noint)

# SSD - all
sst_ssd_bfp_mod <- lm(all_ssd ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssd_bfp_sum <- summary(sst_ssd_bfp_mod)

r01_sst_bfp_ses <- r01_sst_cond[!is.na(r01_sst_cond$income) & !is.na(r01_sst_cond$mom_ed) & r01_sst_cond$all_racehorse_check == 1 & !is.na(r01_sst_cond$dxa_total_body_perc_fat), ]
r01_sst_bfp_ses$ssd_pred <- predict(sst_ssd_bfp_mod, type = 'response')


# SSD - ED condition
sst_ssd_bfp_EDmod <- lmer(ssd ~ mom_ed + income + sex + age_yr + ED*dxa_total_body_perc_fat + (1|sub), data = r01_sst_EDlong)
sst_ssd_bfp_EDsum <- summary(sst_ssd_bfp_EDmod)

sst_ssd_bfp_EDmod_noint <- lmer(ssd ~ mom_ed + income + sex + age_yr + ED + dxa_total_body_perc_fat + (1|sub), data = r01_sst_EDlong)
sst_ssd_bfp_EDsum_noint <- summary(sst_ssd_bfp_EDmod_noint)

r01_sst_bfp_ED_ses <- r01_sst_EDlong[!is.na(r01_sst_EDlong$income) & !is.na(r01_sst_EDlong$mom_ed) & !is.na(r01_sst_EDlong$dxa_total_body_perc_fat), ]
r01_sst_bfp_ED_ses$ssd_pred <- predict(sst_ssd_bfp_EDmod_noint, type = 'response')


# SSD - PS condition
sst_ssd_bfp_PSmod <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS*dxa_total_body_perc_fat + (1|sub), data = r01_sst_PSlong)
sst_ssd_bfp_PSsum <- summary(sst_ssd_bfp_PSmod)

sst_ssd_bfp_PSmod_noInt <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS + dxa_total_body_perc_fat + (1|sub), data = r01_sst_PSlong)
sst_ssd_bfp_PSsum_noInt  <- summary(sst_ssd_bfp_PSmod_noInt)

r01_sst_bfp_PS_ses <- r01_sst_PSlong[!is.na(r01_sst_PSlong$income) & !is.na(r01_sst_PSlong$mom_ed) & !is.na(r01_sst_PSlong$dxa_total_body_perc_fat), ]
r01_sst_bfp_PS_ses$ssd_pred <- predict(sst_ssd_bfp_PSmod_noInt, type = 'response')

## N-Back ####

# balanced accuracy
nback_balacc_bfp_mod <- lmer(p_target_ba ~ mom_ed + income + sex + age_yr + block*dxa_total_body_perc_fat + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_balacc_bfp_sum <- summary(nback_balacc_bfp_mod)

r01_nback_bfp_ses <- r01_nback[!is.na(r01_nback$income) & !is.na(r01_nback$mom_ed) & r01_nback$ses == 1 & !is.na(r01_nback$dxa_total_body_perc_fat), ]
r01_nback_bfp_ses$balacc_pred <- predict(nback_balacc_bfp_mod, type = 'response')

# d'
nback_dprime_bfp_mod <- lmer(dprime ~ mom_ed + income + sex + age_yr + block*dxa_total_body_perc_fat + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_dprime_bfp_sum <- summary(nback_dprime_bfp_mod)
