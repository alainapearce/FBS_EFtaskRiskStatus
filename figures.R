# This script was written by Alaina Pearce in Summer 2022
# to set up the figures for the Task EF x Risk Status paper
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

# library(ggplot2)

# source('setup.R')

## Go-NoGo ####
# False Alarms

gng_fa_plot <- ggplot(r01_gng_ses, aes(x=risk_status_mom, y=fa_pred)) +
  geom_violin(trim=FALSE, fill="cornflowerblue")+
  labs(title="Go-NoGo",
       x="Familial Risk Status",
       y = "False Alarm, % \n (Adjusted for Sex, Age, and SES)") +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  theme_pubr() +
  theme(plot.title = element_text(hjust = 0.5))

## Stop-Signal Task ####
# SSRT - design
sst_ssrtdesign_plot <- ggplot(r01_sst_ses, aes(x=PS, y=ssrt_pred, fill = ED)) +
  geom_violin(trim=FALSE)+
  geom_boxplot(aes(group = interaction(ED, PS)), fill = 'white', position = position_dodge(width = 0.9),  width = 0.1, outlier.shape = NA) +
  scale_fill_manual(values = c("cornflowerblue", "orange")) +
  labs(title="Stop-Signal Task",
       x="Familial Risk Status",
       y = "Stop-Signal Reaction Time, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# SSRT - Overall
sst_ssrt_plot <- ggplot(r01_sst_all_ses, aes(x=risk_status_mom, y=ssrt_pred)) +
  geom_violin(trim=FALSE, fill = 'cornflowerblue')+
  geom_boxplot(position = position_dodge(width = 0.9),  width = 0.1, outlier.shape = NA) +
  labs(title="Stop-Signal Task",
       x="Familial Risk Status",
       y = "Stop-Signal Reaction Time, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# SSRT - ED
sst_ssrtED_plot <- ggplot(r01_sst_ED_ses, aes(x=risk_status_mom, y=ssrt_pred)) +
  geom_violin(trim=FALSE, fill = 'cornflowerblue')+
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title="Stop-Signal Task - Energy Denisty",
       x="Familial Risk Status",
       y = "Stop-Signal Reaction Time, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# SSRT - PS
sst_ssrtPS_plot <- ggplot(r01_sst_PS_ses, aes(x=risk_status_mom, y=ssrt_pred)) +
  geom_violin(trim=FALSE, fill = 'cornflowerblue')+
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title="Stop-Signal Task - Portion Size",
       x="Familial Risk Status",
       y = "Stop-Signal Reaction Time, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# SSD - design
sst_ssddesign_plot <- ggplot(r01_sst_ses, aes(x=PS, y=ssd_pred, fill = ED)) +
  geom_violin(trim=FALSE)+
  geom_boxplot(aes(group = interaction(ED, PS)), fill = 'white', position = position_dodge(width = 0.9), width = 0.1,) +
  scale_fill_manual(values = c("cornflowerblue", "orange")) +
  labs(title="Stop-Signal Task",
       x="Portion Size",
       y = "Stop Signal Delay, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

sst_ssddesignED_plot <- ggplot(r01_sst_ses, aes(x=ED, y=ssd_pred)) +
  geom_violin(trim=FALSE, fill="cornflowerblue")+
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title="Stop-Signal Task - Design ED",
       x="Energy Density",
       y = "Stop Signal Delay, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# SSD - Overall
sst_ssd_plot <- ggplot(r01_sst_all_ses, aes(x=risk_status_mom, y=ssd_pred)) +
  geom_violin(trim=FALSE, fill = 'cornflowerblue')+
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title="Stop-Signal Task",
       x="Familial Risk Status",
       y = "Stop Signal Delay, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# SSD - ED
sst_ssdED_plot <- ggplot(r01_sst_ED_ses, aes(x=risk_status_mom, y=ssd_pred)) +
  geom_violin(trim=FALSE, fill = 'cornflowerblue')+
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title="Stop-Signal Task - Energy Density",
       x="Familial Risk Status",
       y = "Stop Signal Delay, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

# SSD - PS
sst_ssdPS_plot <- ggplot(r01_sst_PS_ses, aes(x=risk_status_mom, y=ssd_pred)) +
  geom_violin(trim=FALSE, fill = 'cornflowerblue')+
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title="Stop-Signal Task - Portion Size",
       x="Familial Risk Status",
       y = "Stop Signal Delay, ms \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

## N-Back ####

# Balanced Accuracy
nback_balacc_plot <- ggplot(r01_nback_ses, aes(x=block, y=balacc_pred, fill = risk_status_mom)) +
  geom_violin(trim=FALSE)+
  geom_signif(stat = "identity", inherit.aes = FALSE,
               data = data.frame(x = c(1.7, 0.7, 2.7), xend = c(3.3, 3.3, 3.3), y = c(105, 110, 85),  annotation = c("***", " ***", "*")),
               aes(x = x, xend = xend, y = y, yend = y, annotation = annotation)) +
  geom_boxplot(aes(group = interaction(block, risk_status_mom)), fill = 'white', position = position_dodge(width = 0.9),  width = 0.1, outlier.shape = NA) +
  scale_fill_manual(values = c("cornflowerblue", "orange")) +
  labs(title="N-Back Task",
       x="Load",
       y = "Balanced Accuracy, % \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


# d'
nback_drime_plot <- ggplot(r01_nback_ses, aes(x=block, y=dprime_pred, fill = risk_status_mom)) +
  geom_violin(trim=FALSE) +
  geom_signif(stat = "identity", inherit.aes = FALSE,
              data = data.frame(x = c(1.7, 0.7, 2.7), xend = c(3.3, 3.3, 3.3), y = c(4.25, 4.5, 3),  annotation = c("***", " ***", "*")),
              aes(x = x, xend = xend, y = y, yend = y, annotation = annotation)) +
  geom_boxplot(aes(group = interaction(block, risk_status_mom)), fill = 'white', position = position_dodge(width = 0.9),  width = 0.1, outlier.shape = NA) +
  scale_fill_manual(values = c("cornflowerblue", "orange")) +
  labs(title="N-Back Task",
       x="Load",
       y = "d' \n (Adjusted for Sex, Age, and SES)") +
  theme_pubr(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())



