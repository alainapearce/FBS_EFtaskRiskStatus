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

# load covariates
covariates_dat <- read.csv('data/covariates_dat.csv')

## Demo ###
covar_demo <- covariates_dat[covariates_dat$risk_status_mom != 'Neither', ]
covar_demo$risk_status_mom <- droplevels(factor(covar_demo$risk_status_mom))

## income
income_chi <- chisq.test(xtabs(~risk_status_mom + income, data = covar_demo))

## maternal ed
momed_fisher <- fisher.test(xtabs(~risk_status_mom + mom_ed, data = covar_demo))

## bmi percentile
bmi_t <- t.test(bmi_percentile ~ risk_status_mom, data = covar_demo)

## bmi percentile
pbf_t <- t.test(dxa_total_body_perc_fat ~ risk_status_mom, data = covar_demo)

## WASI
wasi_t <- t.test(wasi_fsiq2 ~ risk_status_mom, data = covar_demo)

## WASI
wasi_cor <- cor.test(covar_demo$dxa_total_body_perc_fat, covar_demo$wasi_fsiq2)

## Go-NoGo  - Risk ####
r01_gng <- read.csv('data/gng_data.csv')

# False Alarms
gng_fa_model <- lm(all_p_nogo_fa ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_fa_sum <- summary(gng_fa_model)

r01_gng_ses <- r01_gng[!is.na(r01_gng$income) & !is.na(r01_gng$mom_ed), ]
r01_gng_ses$fa_pred <- predict(gng_fa_model, type = 'response')

# hits
gng_hits_model <- lm(all_p_go_hit ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_hits_sum <- summary(gng_hits_model)

# reaction time
gng_rt_model <- lm(all_rt_mean_go_hit ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_rt_sum <- summary(gng_rt_model)

# d'prime
gng_dprime_model <- lm(all_d_prime_ll ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng)
gng_dprime_sum <- summary(gng_dprime_model)

## Stop-Signal Task  - Risk ####
r01_sst_long <- read.csv('data/sst_long.csv')
r01_sst_cond <- read.csv('data/sst_cond.csv')
r01_sst_EDlong <- read.csv('data/sst_EDlong.csv')
r01_sst_PSlong <- read.csv('data/sst_PSlong.csv')

# SSRT - task design
sstdesign_ssrt_model <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS*ED + (1|sub), data = r01_sst_long[r01_sst_long$ncond_racehorse_good == 4, ])
sst_ssrtdesign_sum <- summary(sstdesign_ssrt_model)

r01_sst_ses <- r01_sst_long[!is.na(r01_sst_long$income) & !is.na(r01_sst_long$mom_ed) & r01_sst_long$ncond_racehorse_good == 4, ]
r01_sst_ses$ssrt_pred <- predict(sstdesign_ssrt_model, type = 'response')

# SSRT - all
sst_ssrt_model <- lm(all_ssrt_int ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssrt_sum <- summary(sst_ssrt_model)

r01_sst_all_ses <- r01_sst_cond[!is.na(r01_sst_cond$income) & !is.na(r01_sst_cond$mom_ed) & r01_sst_cond$all_racehorse_check == 1, ]
r01_sst_all_ses$ssrt_pred <- predict(sst_ssrt_model, type = 'response')

# SSRT - ED condition
sst_ssrt_EDmodel <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + ED*risk_status_mom + (1|sub), data = r01_sst_EDlong)
sst_ssrt_EDsum <- summary(sst_ssrt_EDmodel)

r01_sst_ED_ses <- r01_sst_EDlong[!is.na(r01_sst_EDlong$income) & !is.na(r01_sst_EDlong$mom_ed), ]
r01_sst_ED_ses$ssrt_pred <- predict(sst_ssrt_EDmodel, type = 'response')

# SSRT - PS condition
sst_ssrt_PSmodel <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS*risk_status_mom + (1|sub), data = r01_sst_PSlong)
sst_ssrt_PSsum <- summary(sst_ssrt_PSmodel)

r01_sst_PS_ses <- r01_sst_PSlong[!is.na(r01_sst_PSlong$income) & !is.na(r01_sst_PSlong$mom_ed), ]
r01_sst_PS_ses$ssrt_pred <- predict(sst_ssrt_PSmodel, type = 'response')

# SSD - task design
sstdesign_ssd_model <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS*ED + (1|sub), data = r01_sst_long[r01_sst_long$ncond_racehorse_good == 4, ])
sst_ssddesign_sum <- summary(sstdesign_ssd_model)

r01_sst_ses$ssd_pred <- predict(sstdesign_ssd_model, type = 'response')

# SSD - all
sst_ssd_model <- lm(all_ssd ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssd_sum <- summary(sst_ssd_model)

r01_sst_all_ses$ssd_pred <- predict(sst_ssd_model, type = 'response')

# SSD - ED condition
sst_ssd_EDmodel <- lmer(ssd ~ mom_ed + income + sex + age_yr + ED*risk_status_mom + (1|sub), data = r01_sst_EDlong)
sst_ssd_EDsum <- summary(sst_ssd_EDmodel)

r01_sst_ED_ses$ssd_pred <- predict(sst_ssd_EDmodel, type = 'response')

# SSD - PS condition
sst_ssd_PSmodel <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS*risk_status_mom + (1|sub), data = r01_sst_PSlong)
sst_ssd_PSsum <- summary(sst_ssd_PSmodel)

r01_sst_PS_ses$ssd_pred <- predict(sst_ssd_PSmodel, type = 'response')


## N-Back - risk ####
r01_nback <- read.csv('data/nback_data.csv')

# balanced accuracy
nback_balacc_mod <- lmer(p_target_ba ~ mom_ed + income + sex + age_yr + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_balacc_sum <- summary(nback_balacc_mod)

r01_nback_ses <- r01_nback[!is.na(r01_nback$income) & !is.na(r01_nback$mom_ed) & r01_nback$ses == 1, ]
r01_nback_ses$balacc_pred <- predict(nback_balacc_mod, type = 'response')

# d'
nback_dprime_mod <- lmer(dprime ~ mom_ed + income + sex + age_yr + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_dprime_sum <- summary(nback_dprime_mod)

r01_nback_ses$dprime_pred <- predict(nback_dprime_mod, type = 'response')

## Exploratory Analyses with Body Fat Percent ####
r01_gng$bfp_center <- r01_gng$dxa_total_body_perc_fat - mean(r01_gng$dxa_total_body_perc_fat, na.rm = TRUE)
r01_sst_cond$bfp_center <- r01_sst_cond$dxa_total_body_perc_fat - mean(r01_sst_cond$dxa_total_body_perc_fat, na.rm = TRUE)
r01_sst_EDlong$bfp_center <- r01_sst_EDlong$dxa_total_body_perc_fat - mean(r01_sst_EDlong$dxa_total_body_perc_fat, na.rm = TRUE)
r01_sst_PSlong$bfp_center <- r01_sst_PSlong$dxa_total_body_perc_fat - mean(r01_sst_PSlong$dxa_total_body_perc_fat, na.rm = TRUE)
r01_nback$bfp_center <- r01_nback$dxa_total_body_perc_fat - mean(r01_nback$dxa_total_body_perc_fat, na.rm = TRUE)


## Go-NoGo- BFP ####
# False Alarms
gng_fa_bfp_mod <- lm(all_p_nogo_fa ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_fa_bfp_sum <- summary(gng_fa_bfp_mod)

r01_gng_ses$fa_bfp_pred <- predict(gng_fa_bfp_mod, type = 'response')


# hits
gng_hits_bfp_mod <- lm(all_p_go_hit ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_hits_bfp_sum <- summary(gng_hits_bfp_mod)

# reaction time
gng_rt_bfp_mod <- lm(all_rt_mean_go_hit ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_rt_bfp_sum <- summary(gng_rt_bfp_mod)

# d'prime
gng_dprime_bfp_mod <- lm(all_d_prime_ll ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_gng)
gng_dprime_bfp_sum <- summary(gng_dprime_bfp_mod)

## Stop-Signal Task  - BFP ####

# SSRT - all
sst_ssrt_bfp_mod <- lm(all_ssrt_int ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssrt_bfp_sum <- summary(sst_ssrt_bfp_mod)

r01_sst_all_ses$ssrt_bfp_pred <- predict(sst_ssrt_bfp_mod, type = 'response')

# SSRT - ED condition
sst_ssrt_bfp_EDmod <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + ED*bfp_center + (1|sub), data = r01_sst_EDlong)
sst_ssrt_bfp_EDsum <- summary(sst_ssrt_bfp_EDmod)

# SSRT - PS condition
sst_ssrt_bfp_PSmod <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS*bfp_center + (1|sub), data = r01_sst_PSlong)
sst_ssrt_bfp_PSsum<- summary(sst_ssrt_bfp_PSmod)


# SSD - all
sst_ssd_bfp_mod <- lm(all_ssd ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssd_bfp_sum <- summary(sst_ssd_bfp_mod)

r01_sst_all_ses$ssd_bfp_pred <- predict(sst_ssd_bfp_mod, type = 'response')


# SSD - ED condition
sst_ssd_bfp_EDmod <- lmer(ssd ~ mom_ed + income + sex + age_yr + ED*bfp_center + (1|sub), data = r01_sst_EDlong)
sst_ssd_bfp_EDsum <- summary(sst_ssd_bfp_EDmod)

r01_sst_bfp_ED_ses <- r01_sst_EDlong[!is.na(r01_sst_EDlong$income) & !is.na(r01_sst_EDlong$mom_ed) & !is.na(r01_sst_EDlong$dxa_total_body_perc_fat), ]
r01_sst_bfp_ED_ses$ssd_pred <- predict(sst_ssd_bfp_EDmod, type = 'response')


# SSD - PS condition
sst_ssd_bfp_PSmod <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS*bfp_center + (1|sub), data = r01_sst_PSlong)
sst_ssd_bfp_PSsum <- summary(sst_ssd_bfp_PSmod)

r01_sst_bfp_PS_ses <- r01_sst_PSlong[!is.na(r01_sst_PSlong$income) & !is.na(r01_sst_PSlong$mom_ed) & !is.na(r01_sst_PSlong$dxa_total_body_perc_fat), ]
r01_sst_bfp_PS_ses$ssd_pred <- predict(sst_ssd_bfp_PSmod, type = 'response')

## N-Back BFP ####

# balanced accuracy
nback_balacc_bfp_mod <- lmer(p_target_ba ~ mom_ed + income + sex + age_yr + block*bfp_center + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_balacc_bfp_sum <- summary(nback_balacc_bfp_mod)

r01_nback_ses$balacc_bfp_pred <- predict(nback_balacc_bfp_mod, type = 'response')

# d'
nback_dprime_bfp_mod <- lmer(dprime ~ mom_ed + income + sex + age_yr + block*bfp_center + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_dprime_bfp_sum <- summary(nback_dprime_bfp_mod)

r01_nback_ses$dprime_bfp_pred <- predict(nback_dprime_bfp_mod, type = 'response')


# Exploratory - relative impact of risk and BFP - multiple regression ####

## Go-NoGo- BFP + Risk ####
# False Alarms
gng_fa_bfp_risk_mod <- lm(all_p_nogo_fa ~ mom_ed + income + sex + age_yr + risk_status_mom + dxa_total_body_perc_fat, data = r01_gng)
gng_fa_bfp_risk_sum <- summary(gng_fa_bfp_risk_mod)

## Stop-Signal Task  - BFP + Risk ####

# SSRT - all
sst_ssrt_bfp_risk_mod <- lm(all_ssrt_int ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssrt_bfp_risk_sum <- summary(sst_ssrt_bfp_risk_mod)

r01_sst_all_ses$ssrt_pred_both <- predict(sst_ssrt_bfp_risk_mod, type = 'response')

# SSRT - ED condition
sst_ssrt_bfp_risk_EDmod <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + ED + bfp_center + risk_status_mom + (1|sub), data = r01_sst_EDlong)
sst_ssrt_bfp_risk_EDsum <- summary(sst_ssrt_bfp_risk_EDmod)

# SSRT - PS condition
sst_ssrt_bfp_risk_PSmod <- lmer(ssrt_int ~ mom_ed + income + sex + age_yr + PS + dxa_total_body_perc_fat + risk_status_mom + (1|sub), data = r01_sst_PSlong)
sst_ssrt_bfp_risk_PSsum<- summary(sst_ssrt_bfp_risk_PSmod)


# SSD - all
sst_ssd_bfp_risk_mod <- lm(all_ssd ~ mom_ed + income + sex + age_yr + dxa_total_body_perc_fat + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssd_bfp_risk_sum <- summary(sst_ssd_bfp_risk_mod)

r01_sst_all_ses$ssd_pred_both <- predict(sst_ssd_bfp_risk_mod, type = 'response')


# SSD - ED condition
sst_ssd_bfp_risk_EDmod <- lmer(ssd ~ mom_ed + income + sex + age_yr + ED + risk_status_mom + dxa_total_body_perc_fat + (1|sub), data = r01_sst_EDlong)
sst_ssd_bfp_risk_EDsum <- summary(sst_ssd_bfp_risk_EDmod)

r01_sst_bfp_ED_ses$ssd_pred_both <- predict(sst_ssd_bfp_risk_EDmod, type = 'response')

# SSD - PS condition
sst_ssd_bfp_risk_PSmod <- lmer(ssd ~ mom_ed + income + sex + age_yr + PS + risk_status_mom + dxa_total_body_perc_fat + (1|sub), data = r01_sst_PSlong)
sst_ssd_bfp_risk_PSsum <- summary(sst_ssd_bfp_risk_PSmod)

r01_sst_bfp_PS_ses$ssd_pred_both <- predict(sst_ssd_bfp_PSmod, type = 'response')

## N-Back BFP ####

# balanced accuracy
nback_balacc_bfp_risk_mod <- lmer(p_target_ba ~ mom_ed + income + sex + age_yr + block*risk_status_mom + block*bfp_center + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_balacc_bfp_risk_sum <- summary(nback_balacc_bfp_risk_mod)

r01_nback_ses$balacc_pred_both <- predict(nback_balacc_bfp_risk_mod, type = 'response')

# d'
nback_dprime_bfp_risk_mod <- lmer(dprime ~ mom_ed + income + sex + age_yr + block*risk_status_mom + block*bfp_center + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_dprime_bfp_risk_sum <- summary(nback_dprime_bfp_risk_mod)

r01_nback_ses$dprime_pred_both <- predict(nback_dprime_bfp_risk_mod, type = 'response')

# Exploratory - relative weight analysis ####

## Go-NoGo- rwa ####
r01_gng$sex_num <- ifelse(r01_gng$sex == 'Male', 0, 1)
r01_gng$mom_ed_num <- ifelse(r01_gng$mom_ed == 'High School/GED', 0, ifelse(r01_gng$mom_ed == 'AA/Technical Degree', 1, ifelse(r01_gng$mom_ed == 'Bachelor Degree', 2, ifelse(r01_gng$mom_ed == '> Bachelor Degree', 3, NA))))
r01_gng$income_num <- ifelse(r01_gng$income == '< $51,000', 0, ifelse(r01_gng$income == '$51,000 - $100,000', 1, ifelse(r01_gng$income == '>$100,000', 2, NA)))
r01_gng$risk_status_mom_num <- ifelse(r01_gng$risk_status_mom == 'Low Risk', 0, 1)

# False Alarms
gng_fa_rwa <- rwa(r01_gng, 'all_p_nogo_fa', c('mom_ed_num', 'income_num', 'sex_num', 'age_yr', 'risk_status_mom_num', 'dxa_total_body_perc_fat'), applysigns = FALSE, plot = TRUE)

## Stop-Signal Task  - RWA ####
r01_sst_cond$sex_num <- ifelse(r01_sst_cond$sex == 'Male', 0, 1)
r01_sst_cond$mom_ed_num <- ifelse(r01_sst_cond$mom_ed == 'High School/GED', 0, ifelse(r01_sst_cond$mom_ed == 'AA/Technical Degree', 1, ifelse(r01_sst_cond$mom_ed == 'Bachelor Degree', 2, ifelse(r01_sst_cond$mom_ed == '> Bachelor Degree', 3, NA))))
r01_sst_cond$income_num <- ifelse(r01_sst_cond$income == '< $51,000', 0, ifelse(r01_sst_cond$income == '$51,000 - $100,000', 1, ifelse(r01_sst_cond$income == '>$100,000', 2, NA)))
r01_sst_cond$risk_status_mom_num <- ifelse(r01_sst_cond$risk_status_mom == 'Low Risk', 0, 1)

# SSRT - all
sst_ssrt_rwa <- rwa(r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ], 'all_ssrt_int', c('mom_ed_num', 'income_num', 'sex_num', 'age_yr', 'risk_status_mom_num', 'dxa_total_body_perc_fat'), applysigns = FALSE, plot = TRUE)

# SSD - all
sst_ssd_rwa <- rwa(r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ], 'all_ssd', c('mom_ed_num', 'income_num', 'sex_num', 'age_yr', 'risk_status_mom_num', 'dxa_total_body_perc_fat'), applysigns = FALSE, plot = TRUE)

## N-Back RWA ####
r01_nback$sex_num <- ifelse(r01_nback$sex == 'Male', 0, 1)
r01_nback$mom_ed_num <- ifelse(r01_nback$mom_ed == 'High School/GED', 0, ifelse(r01_nback$mom_ed == 'AA/Technical Degree', 1, ifelse(r01_nback$mom_ed == 'Bachelor Degree', 2, ifelse(r01_nback$mom_ed == '> Bachelor Degree', 3, NA))))
r01_nback$income_num <- ifelse(r01_nback$income == '< $51,000', 0, ifelse(r01_nback$income == '$51,000 - $100,000', 1, ifelse(r01_nback$income == '>$100,000', 2, NA)))
r01_nback$risk_status_mom_num <- ifelse(r01_nback$risk_status_mom == 'Low Risk', 0, 1)
r01_nback$block_num <- ifelse(r01_nback$block == '0-Back', 0, ifelse(r01_nback$block == '1-Back', 1, 2))

r01_nback$blockrisk_status_mom <- r01_nback$risk_status_mom_num*r01_nback$block_num
r01_nback$blockbfp_center <- r01_nback$bfp_center*r01_nback$block_num

# balanced accuracy
nback_balacc_rwa <- rwa(r01_nback[r01_nback$ses == 1, ], 'p_target_ba', c('mom_ed_num', 'income_num', 'sex_num', 'age_yr', 'block_num', 'risk_status_mom_num', 'bfp_center', 'blockrisk_status_mom', 'blockbfp_center'), applysigns = FALSE, plot = TRUE)

# d'
nback_dprime_rwa <- rwa(r01_nback[r01_nback$ses == 1, ], 'dprime', c('mom_ed_num', 'income_num', 'sex_num', 'age_yr', 'block_num', 'risk_status_mom_num', 'bfp_center', 'blockrisk_status_mom', 'blockbfp_center'), applysigns = FALSE, plot = TRUE)

## Exploratory Analyses remove BMI percentile >= 85 - Reviewer Resposne ####

## Go-NoGo- BFP ####
# False Alarms
gng_fa_risk_p85_mod <- lm(all_p_nogo_fa ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng[r01_gng$bmi_percentile < 85, ])
gng_fa_risk_p85_sum <- summary(gng_fa_risk_p85_mod)

# hits
gng_hits_risk_p85_mod <- lm(all_p_go_hit ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng[r01_gng$bmi_percentile < 85, ])
gng_hits_risk_p85_sum <- summary(gng_hits_risk_p85_mod)

# reaction time
gng_rt_risk_p85_mod <- lm(all_rt_mean_go_hit ~ mom_ed + income + sex + age_yr + risk_status_mom,  data = r01_gng[r01_gng$bmi_percentile < 85, ])
gng_rt_risk_p85_sum <- summary(gng_rt_risk_p85_mod)

# d'prime
gng_dprime_risk_p85_mod <- lm(all_d_prime_ll ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_gng[r01_gng$bmi_percentile < 85, ])
gng_dprime_risk_p85_sum <- summary(gng_dprime_risk_p85_mod)

## Stop-Signal Task  - BFP ####

# SSRT - all
sst_ssrt_risk_p85_mod <- lm(all_ssrt_int ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1 & r01_sst_cond$bmi_percentile < 85, ])
sst_ssrt_risk_p85_sum <- summary(sst_ssrt_risk_p85_mod)

# SSD - all
sst_ssd_risk_p85_mod <- lm(all_ssd ~ mom_ed + income + sex + age_yr + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1 & r01_sst_cond$bmi_percentile < 85, ])
sst_ssd_risk_p85_sum <- summary(sst_ssd_risk_p85_mod)

## N-Back BFP ####

# balanced accuracy
nback_balacc_risk_p85_mod <- lmer(p_target_ba ~ mom_ed + income + sex + age_yr + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1 & r01_nback$bmi_percentile < 85, ])
nback_balacc_risk_p85_sum <- summary(nback_balacc_risk_p85_mod)

# d'
nback_dprime_risk_p85_mod <- lmer(dprime ~ mom_ed + income + sex + age_yr + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1 & r01_nback$bmi_percentile < 85, ])
nback_dprime_risk_p85_sum <- summary(nback_dprime_risk_p85_mod)

## Sensitivity Analyses with bmi_z - Reviewer Resposne ####

## Go-NoGo- BFP ####
# False Alarms
gng_fa_risk_bmiz_mod <- lm(all_p_nogo_fa ~ mom_ed + income + sex + age_yr + bmi_z + risk_status_mom, data = r01_gng)
gng_fa_risk_bmiz_sum <- summary(gng_fa_risk_bmiz_mod)

# hits
gng_hits_risk_bmiz_mod <- lm(all_p_go_hit ~ mom_ed + income + sex + age_yr + bmi_z + risk_status_mom, data = r01_gng)
gng_hits_risk_bmiz_sum <- summary(gng_hits_risk_bmiz_mod)

# reaction time
gng_rt_risk_bmiz_mod <- lm(all_rt_mean_go_hit ~ mom_ed + income + sex + age_yr + bmi_z + risk_status_mom,  data = r01_gng)
gng_rt_risk_bmiz_sum <- summary(gng_rt_risk_bmiz_mod)

# d'prime
gng_dprime_risk_bmiz_mod <- lm(all_d_prime_ll ~ mom_ed + income + sex + age_yr + bmi_z + risk_status_mom, data = r01_gng)
gng_dprime_risk_bmiz_sum <- summary(gng_dprime_risk_bmiz_mod)

## Stop-Signal Task  - BFP ####

# SSRT - all
sst_ssrt_risk_bmiz_mod <- lm(all_ssrt_int ~ mom_ed + income + sex + age_yr + bmi_z + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssrt_risk_bmiz_sum <- summary(sst_ssrt_risk_bmiz_mod)

# SSD - all
sst_ssd_risk_bmiz_mod <- lm(all_ssd ~ mom_ed + income + sex + age_yr + bmi_z + risk_status_mom, data = r01_sst_cond[r01_sst_cond$all_racehorse_check == 1, ])
sst_ssd_risk_bmiz_sum <- summary(sst_ssd_risk_bmiz_mod)

## N-Back BFP ####

# balanced accuracy
nback_balacc_risk_bmiz_mod <- lmer(p_target_ba ~ mom_ed + income + sex + age_yr + bmi_z + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_balacc_risk_bmiz_sum <- summary(nback_balacc_risk_bmiz_mod)

# d'
nback_dprime_risk_bmiz_mod <- lmer(dprime ~ mom_ed + income + sex + age_yr + bmi_z + block*risk_status_mom + (1|sub), data = r01_nback[r01_nback$ses == 1, ])
nback_dprime_risk_bmiz_sum <- summary(nback_dprime_risk_bmiz_mod)
