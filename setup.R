# This script was written by Alaina Pearce in Summer 2022
# to set up the data for the Task EF x Risk Status paper
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
# need to uncomment if running independently - not needed if compiling with TaskEF-Risk_paper.Rmd
#
# source('functions.R')
# library(haven)
# library(sjlabelled)
# library(reshape2)
# library(psycho)

#### Demographics ####

## 1) Load Data ####
r01_demo <- as.data.frame(read_spss(("data/demographics_data.sav")))
names(r01_demo)[1] <- 'sub'

#remove 2 that were removed for ADHD
r01_demo = r01_demo[r01_demo$sub != 31 & r01_demo$sub != 34 & r01_demo$sub != 134, ]

## 2) Get Variable Labels and Re-Level ####

# risk status
r01_demo$risk_status_mom <- droplevels(as_factor(r01_demo$risk_status_mom))
r01_demo$risk_status_both <- droplevels(as_factor(r01_demo$risk_status_both))
r01_demo$sex <- as_factor(r01_demo$sex)

# income
r01_demo$income <- ifelse(is.na(r01_demo$income), NA, ifelse(r01_demo$income < 3, '< $51,000', ifelse(r01_demo$income < 5, "$51,000 - $100,000", '>$100,000')))

# parental ed
r01_demo$mom_ed <- ifelse(r01_demo$measured_parent == 0, ifelse(r01_demo$parent_ed == 0, 'High School/GED', ifelse(r01_demo$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$parent_ed == 3, 'Bachelor Degree', ifelse(r01_demo$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_demo$partner_ed == 0, 'High School/GED', ifelse(r01_demo$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$partner_ed == 3, 'Bachelor Degree', ifelse(r01_demo$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

r01_demo$dad_ed <- ifelse(r01_demo$measured_parent == 1, ifelse(r01_demo$parent_ed == 0, 'High School/GED', ifelse(r01_demo$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$parent_ed == 3, 'Bachelor Degree', ifelse(r01_demo$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_demo$partner_ed == 0, 'High School/GED', ifelse(r01_demo$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_demo$partner_ed == 3, 'Bachelor Degree', ifelse(r01_demo$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

# race
r01_demo$race <- ifelse(r01_demo$race == 0, 'White/Caucasian', ifelse(r01_demo$race == 2, 'Asian', ifelse(r01_demo$race == 3, 'Black/AA', 'Other')))

# ethnicity
r01_demo$ethnicity <- ifelse(r01_demo$ethnicity == 0, 'Not Hispanic/Lantinx', 'Hispanic/Lantinx')

# tanner
r01_demo$pds_tanner_cat <- droplevels(as_factor(r01_demo$pds_tanner_cat))
r01_demo$tanner_silhouette <- ifelse(r01_demo$sex == 'Male', r01_demo$tanner_male, r01_demo$tanner_female)
r01_demo$tanner_silhouette <- ifelse(r01_demo$tanner_silhouette == 99, 'Skip', ifelse(r01_demo$tanner_silhouette == 1, 'Prepubertal', ifelse(r01_demo$tanner_silhouette == 2, 'Early Puberty', ifelse(r01_demo$tanner_silhouette == 3, 'Mid-Puberty', ifelse(r01_demo$tanner_silhouette == 4, 'Late Puberty', 'Postpubertal')))))

# food insecurity
r01_demo$hfssm_6item_cat <- droplevels(as_factor(r01_demo$hfssm_6item_cat))
r01_demo$hfssm_household_cat <- droplevels(as_factor(r01_demo$hfssm_household_cat))
r01_demo$hfssm_adult_cat <- droplevels(as_factor(r01_demo$hfssm_adult_cat))
r01_demo$hfssm_child_cat <- droplevels(as_factor(r01_demo$hfssm_child_cat))

r01_demo$hfias_category <- droplevels(as_factor(r01_demo$hfias_category))

r01_demo$cchip_category <- droplevels(as_factor(r01_demo$cchip_category))

# parents/community
r01_demo$audit_cat <- droplevels(as_factor(r01_demo$audit_cat))
r01_demo$v7_audit_cat <- droplevels(as_factor(r01_demo$v7_audit_cat))

#### Cog/Psych ####

## 1) Load Data ####
r01_qs_cps <- as.data.frame(read_spss(("data/qs_cog_psych_soc.sav")))
names(r01_qs_cps)[1] <- 'sub'

r01_qs_cps_labels <- lapply(r01_qs_cps, function(x) attributes(x)$label)

#remove 2 that were removed for ADHD
r01_qs_cps <- r01_qs_cps[r01_qs_cps$sub != 31 & r01_qs_cps$sub != 34, ]

## 2) Get Variable Labels and Re-Level ####

# risk status
r01_qs_cps$risk_status_mom <- droplevels(as_factor(r01_qs_cps$risk_status_mom))
r01_qs_cps$risk_status_both <- droplevels(as_factor(r01_qs_cps$risk_status_both))
r01_qs_cps$sex <- as_factor(r01_qs_cps$sex)

# income
r01_qs_cps$income <- ifelse(is.na(r01_qs_cps$income), NA, ifelse(r01_qs_cps$income < 3, '< $51,000', ifelse(r01_qs_cps$income < 5, "$51,000 - $100,000", '>$100,000')))

# parental ed
r01_qs_cps$mom_ed <- ifelse(r01_qs_cps$measured_parent == 0, ifelse(r01_qs_cps$parent_ed == 0, 'High School/GED', ifelse(r01_qs_cps$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_qs_cps$parent_ed == 3, 'Bachelor Degree', ifelse(r01_qs_cps$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_qs_cps$partner_ed == 0, 'High School/GED', ifelse(r01_qs_cps$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_qs_cps$partner_ed == 3, 'Bachelor Degree', ifelse(r01_qs_cps$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

r01_qs_cps$dad_ed <- ifelse(r01_qs_cps$measured_parent == 1, ifelse(r01_qs_cps$parent_ed == 0, 'High School/GED', ifelse(r01_qs_cps$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_qs_cps$parent_ed == 3, 'Bachelor Degree', ifelse(r01_qs_cps$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_qs_cps$partner_ed == 0, 'High School/GED', ifelse(r01_qs_cps$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_qs_cps$partner_ed == 3, 'Bachelor Degree', ifelse(r01_qs_cps$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

# rcmas
r01_qs_cps$rcmas_total_normcat <- droplevels(as_factor(r01_qs_cps$rcmas_total_normcat))
r01_qs_cps$rcmas_total_cutcat <- droplevels(as_factor(r01_qs_cps$rcmas_total_cutcat))
r01_qs_cps$rcmas_sd_total_normcat <- droplevels(as_factor(r01_qs_cps$rcmas_sd_total_normcat))

# breif - 2
r01_qs_cps$brief2_negativity_cat <- droplevels(as_factor(r01_qs_cps$brief2_negativity_cat))
r01_qs_cps$brief2_inconsistency_cat <- droplevels(as_factor(r01_qs_cps$brief2_inconsistency_cat))
r01_qs_cps$brief2_infrequency_cat <- droplevels(as_factor(r01_qs_cps$brief2_infrequency_cat))

r01_qs_cps$v7_brief2_negativity_cat <- droplevels(as_factor(r01_qs_cps$v7_brief2_negativity_cat))
r01_qs_cps$v7_brief2_inconsistency_cat <- droplevels(as_factor(r01_qs_cps$v7_brief2_inconsistency_cat))
r01_qs_cps$v7_brief2_infrequency_cat <- droplevels(as_factor(r01_qs_cps$v7_brief2_infrequency_cat))

r01_qs_cps[c(341:381, 383:384, 386:387, 392:398)] <- sapply(r01_qs_cps[c(341:381, 383:384, 386:387, 392:398)], as.numeric)

#### Antro ####

## 1) Load Data ####
r01_health <- as.data.frame(read_spss(("data/anthro_data.sav")))
names(r01_health)[1] <- 'sub'

r01_health_labels <- lapply(r01_health, function(x) attributes(x)$label)

#remove 2 that were removed for ADHD
r01_health <- r01_health[r01_health$sub != 31 & r01_health$sub != 34, ]

# anthro categories
r01_health$weight_status <- droplevels(as_factor(r01_health$weight_status))
r01_health$v7_weight_status <- droplevels(as_factor(r01_health$v7_weight_status))
r01_health$mom_weight_status <- droplevels(as_factor(r01_health$mom_weight_status))
r01_health$v7_mom_weight_status <- droplevels(as_factor(r01_health$v7_mom_weight_status))
r01_health$dad_weight_status <- droplevels(as_factor(r01_health$dad_weight_status))
r01_health$v7_dad_weight_status <- droplevels(as_factor(r01_health$v7_dad_weight_status))

## 2) Get Variable Labels and Re-Level ####

# risk status
r01_health$risk_status_mom <- droplevels(as_factor(r01_health$risk_status_mom))
r01_health$risk_status_both <- droplevels(as_factor(r01_health$risk_status_both))
r01_health$sex <- as_factor(r01_health$sex)

# income
r01_health$income <- ifelse(is.na(r01_health$income), NA, ifelse(r01_health$income < 3, '< $51,000', ifelse(r01_health$income < 5, "$51,000 - $100,000", '>$100,000')))

# parental ed
r01_health$mom_ed <- ifelse(r01_health$measured_parent == 0, ifelse(r01_health$parent_ed == 0, 'High School/GED', ifelse(r01_health$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_health$parent_ed == 3, 'Bachelor Degree', ifelse(r01_health$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_health$partner_ed == 0, 'High School/GED', ifelse(r01_health$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_health$partner_ed == 3, 'Bachelor Degree', ifelse(r01_health$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

r01_health$dad_ed <- ifelse(r01_health$measured_parent == 1, ifelse(r01_health$parent_ed == 0, 'High School/GED', ifelse(r01_health$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_health$parent_ed == 3, 'Bachelor Degree', ifelse(r01_health$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_health$partner_ed == 0, 'High School/GED', ifelse(r01_health$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_health$partner_ed == 3, 'Bachelor Degree', ifelse(r01_health$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))

r01_health[c(106, 119, 128, 143)] <- sapply(r01_health[c(106, 119, 128, 143)], as.numeric)

#### Covariates Data ####
covariates_dat <- merge(r01_demo[c(1:16, 337:338, 19:24)], r01_health[c(1, 106, 119, 128, 143)], by = 'sub', all.x = TRUE, all.y = FALSE)

covariates_dat <- merge(covariates_dat, r01_qs_cps[c(1, 341:388, 392:398)], by = 'sub', all.x = TRUE, all.y = FALSE)

covariates_dat$risk_status_mom <- droplevels(factor(covariates_dat$risk_status_mom))
covariates_dat$ethnicity <- droplevels(factor(covariates_dat$ethnicity))
covariates_dat$race <- droplevels(factor(covariates_dat$race))
covariates_dat$mom_ed <- droplevels(factor(covariates_dat$mom_ed))
covariates_dat$dad_ed <- droplevels(factor(covariates_dat$dad_ed))

#### Go No-Go ####

## 1) Load Data ####

r01_gng <- read.csv("data/task-gng_summary.tsv", header = TRUE, sep = "\t", na.strings = '#NAME?')

#remove 2 that were removed for ADHD
r01_gng <- r01_gng[r01_gng$sub != 31 & r01_gng$sub != 34, ]

#remove 51 - task not administered correctly (only responded on NoGo)
r01_gng <- r01_gng[r01_gng$sub != 51, ]

#make all numeric
r01_gng[2:111] <- sapply(r01_gng[2:111], as.numeric)

#proportion to percent
r01_gng[10:13] <- sapply(r01_gng[10:13], "*", 100)

#merge
r01_gng <- merge(covariates_dat, r01_gng[1:31], by = 'sub', all.x = FALSE, all.y = TRUE)

#remove risk status Neither
r01_gng <- r01_gng[r01_gng$risk_status_mom != 'Neither', ]
r01_gng$risk_status_mom <- droplevels(r01_gng$risk_status_mom)

#### N-Back ####

## 1) Load Data ####
r01_nback <- read.csv("data/task-nback_summary_long.tsv", header = TRUE, sep = "\t")

#remove 2 that were removed for ADHD
r01_nback <- r01_nback[r01_nback$sub != 31 & r01_nback$sub != 34 & r01_nback$ses == 1, ]

#merge
r01_nback <- merge(covariates_dat, r01_nback, by = 'sub', all.x = FALSE, all.y = TRUE)

#remove risk status Neither
r01_nback <- r01_nback[r01_nback$risk_status_mom != 'Neither', ]
r01_nback$risk_status_mom <- droplevels(r01_nback$risk_status_mom)

#re-name blocks
r01_nback$block <- ifelse(r01_nback$block == 'b0', '0-Back', ifelse(r01_nback$block == 'b1', '1-Back', '2-Back'))

#dprime

dprime_mat <- dprime(n_hit = r01_nback$n_target_hit,
                        n_miss = r01_nback$n_target_miss,
                        n_fa = r01_nback$n_fill_fa,
                        n_cr = r01_nback$n_fill_corr)

r01_nback$dprime <- dprime_mat$dprime

#### SST ####

## 1) Load Data ####
r01_sst_long <- read.csv("data/task-sst_summary_blockslong.tsv", header = TRUE, sep = "\t")
r01_sst_cond <- read.csv("data/task-sst_summary_condwide.tsv", header = TRUE, sep = "\t")

#remove 2 that were removed for ADHD
r01_sst_long <- r01_sst_long[r01_sst_long$sub != 31 & r01_sst_long$sub != 34, ]
r01_sst_cond <- r01_sst_cond[r01_sst_cond$sub != 31 & r01_sst_cond$sub != 34, ]

#make long by condition - ED
r01_sst_cond$ED_racehorse_check <- ifelse(r01_sst_cond$h_ed_racehorse_check == 1 & r01_sst_cond$l_ed_racehorse_check == 1, 1, 0)

r01_sst_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(1, 16, 30)], id.vars = 'sub')
r01_sst_EDlong$ED <- ifelse(r01_sst_EDlong$variable == 'h_ed_racehorse_check', 'High ED', 'Low ED')
r01_sst_EDlong$ED <- factor(r01_sst_EDlong$ED, levels = c('Low ED', 'High ED'))

r01_sst_EDlong$racehorse_check <- r01_sst_EDlong$value

goRT_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(19, 33)])
names(goRT_EDlong)[2] <- 'go_rt'
nError_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(22, 36)])
names(nError_EDlong)[2] <- 'n_error'
nMiss_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(24, 38)])
names(nMiss_EDlong)[2] <- 'n_miss'
pResp_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(25, 39)])
names(pResp_EDlong)[2] <- 'prop_resp'
ssd_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(27, 41)])
names(ssd_EDlong)[2] <- 'ssd'
ssrtMean_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(28, 42)])
names(ssrtMean_EDlong)[2] <- 'ssrt_mean'
ssrtInt_EDlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$ED_racehorse_check) & r01_sst_cond$ED_racehorse_check == 1, c(29, 43)])
names(ssrtInt_EDlong)[2] <- 'ssrt_int'

r01_sst_EDlong <- cbind.data.frame(r01_sst_EDlong[c(1, 4:5)], goRT_EDlong[2], nError_EDlong[2], nMiss_EDlong[2], pResp_EDlong[2], ssd_EDlong[2], ssrtMean_EDlong[2], ssrtInt_EDlong[2])

#make long by condition - PS
r01_sst_cond$PS_racehorse_check <- ifelse(r01_sst_cond$l_port_racehorse_check == 1 & r01_sst_cond$s_port_racehorse_check == 1, 1, 0)

r01_sst_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(1, 44, 58)], id.vars = 'sub')
r01_sst_PSlong$PS <- ifelse(r01_sst_PSlong$variable == 'l_port_racehorse_check', 'Large PS', 'Small PS')
r01_sst_PSlong$PS <- factor(r01_sst_PSlong$PS, levels = c('Small PS', 'Large PS'))

r01_sst_PSlong$racehorse_check <- r01_sst_PSlong$value

goRT_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(47, 61)])
names(goRT_PSlong)[2] <- 'go_rt'
nError_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(50, 64)])
names(nError_PSlong)[2] <- 'n_error'
nMiss_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(52, 66)])
names(nMiss_PSlong)[2] <- 'n_miss'
pResp_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(53, 67)])
names(pResp_PSlong)[2] <- 'prop_resp'
ssd_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(55, 69)])
names(ssd_PSlong)[2] <- 'ssd'
ssrtMean_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(56, 70)])
names(ssrtMean_PSlong)[2] <- 'ssrt_mean'
ssrtInt_PSlong <- melt(r01_sst_cond[!is.na(r01_sst_cond$PS_racehorse_check) & r01_sst_cond$PS_racehorse_check == 1, c(57, 71)])
names(ssrtInt_PSlong)[2] <- 'ssrt_int'

r01_sst_PSlong <- cbind.data.frame(r01_sst_PSlong[c(1, 4:5)], goRT_PSlong[2], nError_PSlong[2], nMiss_PSlong[2], pResp_PSlong[2], ssd_PSlong[2], ssrtMean_PSlong[2], ssrtInt_PSlong[2])

# racehorse_check_fn specified in functions.R
r01_sst_long$ncond_racehorse_good <- sapply(r01_sst_long$sub, FUN = racehorse_check_fn, data = r01_sst_long)

#make conditions for interaction among all blocks
r01_sst_long$ED <- ifelse(r01_sst_long$condition == 'hED_sPort' | r01_sst_long$condition == 'hED_lPort', 'High ED', 'Low ED')
r01_sst_long$ED <- factor(r01_sst_long$ED, levels = c('Low ED', 'High ED'))
r01_sst_long$PS <- ifelse(r01_sst_long$condition == 'hED_sPort' | r01_sst_long$condition == 'lED_sPort', 'Small PS', 'Large PS')
r01_sst_long$PS <- factor(r01_sst_long$PS, levels = c('Small PS', 'Large PS'))
r01_sst_long$block <- factor(r01_sst_long$block)

# get orders
# order_fn specified in functions.R

r01_sst_EDlong$order <- sapply(r01_sst_EDlong[['sub']], FUN = order_fn, data = r01_sst_long, cond = 'ED')
r01_sst_EDlong$order <- as.factor(r01_sst_EDlong$order)

r01_sst_PSlong$order <- sapply(r01_sst_PSlong[['sub']], FUN = order_fn, data = r01_sst_long, cond = 'PS')
r01_sst_PSlong$order <- as.factor(r01_sst_PSlong$order)

r01_sst_long$order <- sapply(r01_sst_long[['sub']], FUN = order_fn, data = r01_sst_long, cond = 'all')
r01_sst_long$order <- as.factor(r01_sst_long$order)

#merge
r01_sst_long <- merge(covariates_dat, r01_sst_long, by = 'sub', all.x = FALSE, all.y = TRUE)
r01_sst_EDlong <- merge(covariates_dat, r01_sst_EDlong, by = 'sub', all.x = FALSE, all.y = TRUE)
r01_sst_PSlong <- merge(covariates_dat, r01_sst_PSlong, by = 'sub', all.x = FALSE, all.y = TRUE)
r01_sst_cond <- merge(covariates_dat, r01_sst_cond, by = 'sub', all.x = FALSE, all.y = TRUE)

#remove risk status Neither
r01_sst_long <- r01_sst_long[r01_sst_long$risk_status_mom != 'Neither', ]
r01_sst_long$risk_status_mom <- droplevels(r01_sst_long$risk_status_mom)

r01_sst_EDlong <- r01_sst_EDlong[r01_sst_EDlong$risk_status_mom != 'Neither', ]
r01_sst_EDlong$risk_status_mom <- droplevels(r01_sst_EDlong$risk_status_mom)

r01_sst_PSlong <- r01_sst_PSlong[r01_sst_PSlong$risk_status_mom != 'Neither', ]
r01_sst_PSlong$risk_status_mom <- droplevels(r01_sst_PSlong$risk_status_mom)

r01_sst_cond <- r01_sst_cond[r01_sst_cond$risk_status_mom != 'Neither', ]
r01_sst_cond$risk_status_mom <- droplevels(r01_sst_cond$risk_status_mom)

