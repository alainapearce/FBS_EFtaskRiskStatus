# This script was written by Alaina Pearce in Summer 2022
# to set up the tables for the Task EF x Risk Status paper
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

# library(gtsummary)
# theme_gtsummary_compact()

# source('setup.R')

## participant table
partab_data <- covariates_dat[covariates_dat$risk_status_mom != 'Neither' & covariates_dat$bmi_screenout == 0, c(8, 12, 10, 14:18, 20, 26, 25, 27:28, 82)]

partab_data$risk_status_mom <- droplevels(partab_data$risk_status_mom)

partab_risk <-
  tbl_summary(
    data = partab_data,
    by = risk_status_mom,
    value = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education", bmi_percentile ~ "BMI %tile", dxa_total_body_perc_fat ~ 'Total Body Fat %', dxa_total_fat_mass ~ 'Total Fat Mass', dxa_est_vat_mass ~ 'Visceral Fat Mass', dxa_total_lean_mass ~ 'Lean Fat Mass', wasi_fsiq2 ~ 'IQ'),
    label = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education", bmi_percentile ~ "BMI %tile", dxa_total_body_perc_fat ~ 'Total Body Fat %', dxa_total_fat_mass ~ 'Total Fat Mass', dxa_est_vat_mass ~ 'Visceral Fat Mass', dxa_total_lean_mass ~ 'Lean Fat Mass', wasi_fsiq2 ~ 'IQ'),
    type = list(age_yr ~ "continuous", sex ~ "categorical", ethnicity ~ "categorical", race ~ "categorical", income ~ "categorical", mom_ed ~ "categorical", dad_ed ~ "categorical", bmi_percentile ~ "continuous", dxa_total_body_perc_fat ~ 'continuous', dxa_total_fat_mass ~ 'continuous', dxa_est_vat_mass ~ 'continuous', dxa_total_lean_mass ~ 'continuous', wasi_fsiq2 ~ 'continuous'),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"),
    missing = "ifany",
    digits = all_continuous() ~ 1) %>%
add_p(test = all_continuous() ~ "t.test") %>%
  modify_header(statistic ~ "**Test Statistic**") %>%
  modify_fmt_fun(statistic ~ style_sigfig)


## GNG table
gngtab_data <- r01_gng[c(8, 89, 91, 93, 95:97, 107)]
gngtab <-
  tbl_summary(
    data = gngtab_data,
    by = risk_status_mom,
    value = list(all_n_go_miss ~ "Missed, N", all_p_go_miss ~ "Missed, %", all_n_nogo_fa ~ "False Alarm, N", all_p_nogo_fa ~ "False Alarm, %", all_rt_mean_go_hit ~ "Mean Hit RT, ms", all_rt_mean_nogo_fa ~ "Mean False Alarm RT, ms", all_d_prime_ll ~ "d', loglinear"),
    label = list(all_n_go_miss ~ "Missed, N", all_p_go_miss ~ "Missed, %", all_n_nogo_fa ~ "False Alarm, N", all_p_nogo_fa ~ "False Alarm, %", all_rt_mean_go_hit ~ "Mean Hit RT, ms", all_rt_mean_nogo_fa ~ "Mean False Alarm RT, ms", all_d_prime_ll ~ "d', loglinear"),
    type = list(all_n_go_miss ~ "continuous", all_p_go_miss ~ "continuous", all_n_nogo_fa ~ "continuous", all_p_nogo_fa ~ "continuous", all_rt_mean_go_hit ~ "continuous", all_rt_mean_nogo_fa ~ "continuous", all_d_prime_ll ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

## Nback table
nbacktab0_data <- r01_nback[r01_nback$block == "0-Back" & r01_nback$ses == 1, c(8, 91:92, 97:100)]
nbacktab1_data <- r01_nback[r01_nback$block == "1-Back" & r01_nback$ses == 1, c(8, 91:92, 97:100)]
nbacktab2_data <- r01_nback[r01_nback$block == "2-Back" & r01_nback$ses == 1, c(8, 91:92, 97:100)]

nback0tab <-
  tbl_summary(
    data = nbacktab0_data,
    by = risk_status_mom,
    value = list(n_target_hit ~ "Hits, N", p_target_hit ~ "Hits, %", n_fill_fa ~ "False Alarm, N", p_fill_fa ~ "False Alarm, %", p_target_ba ~ "Ballanced Acc, %", rt_mean_target_hit ~ "Target RT, ms"),
    label = list(n_target_hit ~ "Hits, N", p_target_hit ~ "Hits, %", n_fill_fa ~ "False Alarm, N", p_fill_fa ~ "False Alarm, %", p_target_ba ~ "Ballanced Acc, %", rt_mean_target_hit ~ "Target RT, ms"),
    type = list(n_target_hit ~ "continuous", p_target_hit ~ "continuous", n_fill_fa ~ "continuous", p_fill_fa ~ "continuous", p_target_ba ~ "continuous", rt_mean_target_hit ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

nback1tab <-
  tbl_summary(
    data = nbacktab1_data,
    by = risk_status_mom,
    value = list(n_target_hit ~ "Hits, N", p_target_hit ~ "Hits, %", n_fill_fa ~ "False Alarm, N", p_fill_fa ~ "False Alarm, %", p_target_ba ~ "Ballanced Acc, %", rt_mean_target_hit ~ "Target RT, ms"),
    label = list(n_target_hit ~ "Hits, N", p_target_hit ~ "Hits, %", n_fill_fa ~ "False Alarm, N", p_fill_fa ~ "False Alarm, %", p_target_ba ~ "Ballanced Acc, %", rt_mean_target_hit ~ "Target RT, ms"),
    type = list(n_target_hit ~ "continuous", p_target_hit ~ "continuous", n_fill_fa ~ "continuous", p_fill_fa ~ "continuous", p_target_ba ~ "continuous", rt_mean_target_hit ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

nback2tab <-
  tbl_summary(
    data = nbacktab2_data,
    by = risk_status_mom,
    value = list(n_target_hit ~ "Hits, N", p_target_hit ~ "Hits, %", n_fill_fa ~ "False Alarm, N", p_fill_fa ~ "False Alarm, %", p_target_ba ~ "Ballanced Acc, %", rt_mean_target_hit ~ "Target RT, ms"),
    label = list(n_target_hit ~ "Hits, N", p_target_hit ~ "Hits, %", n_fill_fa ~ "False Alarm, N", p_fill_fa ~ "False Alarm, %", p_target_ba ~ "Ballanced Acc, %", rt_mean_target_hit ~ "Target RT, ms"),
    type = list(n_target_hit ~ "continuous", p_target_hit ~ "continuous", n_fill_fa ~ "continuous", p_fill_fa ~ "continuous", p_target_ba ~ "continuous", rt_mean_target_hit ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

nbacktab_all <-
  tbl_merge(
    tbls = list(nback0tab, nback1tab, nback2tab),
    tab_spanner = c("**0-Back**", "**1-Back**", "**2-Back**")
  )

## SST table
sstEDlow_data <- r01_sst_EDlong[r01_sst_EDlong$ED == "Low ED", c(8, 86:88, 90:92)]
sstEDhigh_data <- r01_sst_EDlong[r01_sst_EDlong$ED == "High ED", c(8, 86:88, 90:92)]

sstPSlarge_data <- r01_sst_PSlong[r01_sst_PSlong$PS == "Large PS", c(8, 86:88, 90:92)]
sstPSsmall_data <- r01_sst_PSlong[r01_sst_PSlong$PS == "Small PS", c(8, 86:88, 90:92)]

sstEDlow_tab <-
  tbl_summary(
    data = sstEDlow_data,
    by = risk_status_mom,
    value = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    label = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    type = list(go_rt ~ "continuous", n_error ~ "continuous", n_miss ~ "continuous", ssd ~ "continuous", ssrt_mean ~ "continuous", ssrt_int ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

sstEDhigh_tab <-
  tbl_summary(
    data = sstEDhigh_data,
    by = risk_status_mom,
    value = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    label = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    type = list(go_rt ~ "continuous", n_error ~ "continuous", n_miss ~ "continuous", ssd ~ "continuous", ssrt_mean ~ "continuous", ssrt_int ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

sstED_tab <-
  tbl_merge(
    tbls = list(sstEDlow_tab, sstEDhigh_tab),
    tab_spanner = c("**Low ED**", "**High ED**")
  )

sstPSlarge_tab <-
  tbl_summary(
    data = sstPSlarge_data,
    by = risk_status_mom,
    value = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    label = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    type = list(go_rt ~ "continuous", n_error ~ "continuous", n_miss ~ "continuous", ssd ~ "continuous", ssrt_mean ~ "continuous", ssrt_int ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

sstPSsmall_tab <-
  tbl_summary(
    data = sstPSsmall_data,
    by = risk_status_mom,
    value = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    label = list(go_rt ~ "Go RT, ms", n_error ~ "L/R Response Error, N", n_miss ~ "Misses, N", ssd ~ "SSD, ms", ssrt_mean ~ "SSRT - Mean Method, ms", ssrt_int ~ "SSRT - Integration Method, ms"),
    type = list(go_rt ~ "continuous", n_error ~ "continuous", n_miss ~ "continuous", ssd ~ "continuous", ssrt_mean ~ "continuous", ssrt_int ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

sstPS_tab <-
  tbl_merge(
    tbls = list(sstPSsmall_tab, sstPSlarge_tab),
    tab_spanner = c("**Small PS**", "**Large PS**")
  )

