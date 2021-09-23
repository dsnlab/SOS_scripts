# Author: Carra Simpson
# Date: 19th July 2021
# Data cleaning of SOS CRP spreadsheet
# Edited: 10th September 2021 (to describeBy timepoints)
# Edited: 23rd September to add winsorization of log data, where indicated

setwd("~/Documents/RA-Work-2021/Monash Michelle Byrne 2021/Bio_Assay_Analysis/datasets/Raw_datasets")

library(readxl)
library(dplyr)
library(stringr)
library(broom.helpers)
library(ggplot2)
library(psych)
library(rcompanion)
library(DescTools)
library(gridExtra)

# Import tab one of dataset without password
# This dataset has been manually edited to change TAG180 duplicates (x4) that were actually TAG081
# One TAG309 3i value was edited to be TAG 1i, with the lowest of duplicates considered baseline
# A TAG164 3i duplicate was also removed, with the value matching supplied GRIDs considered the 'real' value
# TAG007 4i was assayed twice in duplicate due to NaN values. Second assay without NaN taken (plate 5, cell 2)

CRP_SOS <- read_excel("human_cvd_panel_crp_raw_data.xlsx", 1)

summary(CRP_SOS$CRP_Ave)

# CRP assayed using Millipore Human Cardiovascular Disease (CVD) Magnetic Bead Panel 3 (Acute Phase) 96-Well Plate Assay Cat. # HCVD3MAG-67K
# Undetected values were replaced manually prior to dataset import with half the lower limit of detection

# 62 undetected CRP (in duplicates), replaced with half the lower limit of detection (i.e., 0.001/2 = 0.0005)
# Change all variables to numeric for downstream analysis

CRP_SOS <- CRP_SOS %>% 
  mutate_at(vars(CRP_Ave), as.numeric)

str(CRP_SOS)

# 78 unique participant IDs
length(unique(CRP_SOS$SampleID))

# How many participants have CRP at each time point?
sum(CRP_SOS$CRP_Ave != "NA" & CRP_SOS$Time == "1")
sum(CRP_SOS$CRP_Ave != "NA" & CRP_SOS$Time == "2")
sum(CRP_SOS$CRP_Ave != "NA" & CRP_SOS$Time == "3")
sum(CRP_SOS$CRP_Ave != "NA" & CRP_SOS$Time == "4")

# Calculate and report normality statistics (skew and kurtosis)
# Kurtosis and Skew should be -2/+2 (West, et al. 1995)
# All time points are non-normal
CRP_summary <- describeBy(CRP_SOS$CRP_Ave, group = CRP_SOS$Time)
CRP_summary

# If skew or kurtosis is < -2 or > 2 then transform
# Results were unacceptable for squareroot transformed data but acceptable after log transformation
# All time points now have skewness and kurtosis < +/-2
CRP_SOS$CRP_log <- log(CRP_SOS$CRP_Ave)
CRP_log_summary <- describeBy(CRP_SOS$CRP_log, group = CRP_SOS$Time)
CRP_log_summary

# Check how many outliers remaining after log transformation
CRPlog_up_limit1 <- (CRP_log_summary[[1]][["mean"]] + 3*(CRP_log_summary[[1]][["sd"]]))
CRPlog_lo_limit1 <- (CRP_log_summary[[1]][["mean"]] - 3*(CRP_log_summary[[1]][["sd"]]))

CRPlog_up_limit2 <- (CRP_log_summary[[2]][["mean"]] + 3*(CRP_log_summary[[2]][["sd"]]))
CRPlog_lo_limit2 <- (CRP_log_summary[[2]][["mean"]] - 3*(CRP_log_summary[[2]][["sd"]]))

CRPlog_up_limit3 <- (CRP_log_summary[[3]][["mean"]] + 3*(CRP_log_summary[[3]][["sd"]]))
CRPlog_lo_limit3 <- (CRP_log_summary[[3]][["mean"]] - 3*(CRP_log_summary[[3]][["sd"]]))

CRPlog_up_limit4 <- (CRP_log_summary[[4]][["mean"]] + 3*(CRP_log_summary[[4]][["sd"]]))
CRPlog_lo_limit4 <- (CRP_log_summary[[4]][["mean"]] - 3*(CRP_log_summary[[4]][["sd"]]))

# Check the number of outliers prior to winsorizing (where relevant)
# Time 1 zero upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 1 & CRP_SOS$CRP_log > CRPlog_up_limit1,na.rm=TRUE)
sum(CRP_SOS$Time == 1 & CRP_SOS$CRP_log < CRPlog_lo_limit1,na.rm=TRUE)

# Time 2 zero upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 2 & CRP_SOS$CRP_log > CRPlog_up_limit2,na.rm=TRUE)
sum(CRP_SOS$Time == 2 & CRP_SOS$CRP_log < CRPlog_lo_limit2,na.rm=TRUE)

# Time 3 zero upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 3 & CRP_SOS$CRP_log > CRPlog_up_limit3,na.rm=TRUE)
sum(CRP_SOS$Time == 3 & CRP_SOS$CRP_log < CRPlog_lo_limit3,na.rm=TRUE)

# Time 4 zero upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 4 & CRP_SOS$CRP_log > CRPlog_up_limit4,na.rm=TRUE)
sum(CRP_SOS$Time == 4 & CRP_SOS$CRP_log < CRPlog_lo_limit4,na.rm=TRUE)

# No need to winsorize CRP after log transformation, no outliers

# Michelle to discuss downstream options with collaborators, therefore log transformed and "raw + winsorized" to be exported
# Winsorize any variables that are +/-3 SDs from the mean (within time points)
# First calculate the +/-3 SDs from the mean at each time point for each variable
CRP_up_limit1 <- (CRP_summary[[1]][["mean"]] + 3*(CRP_summary[[1]][["sd"]]))
CRP_lo_limit1 <- (CRP_summary[[1]][["mean"]] - 3*(CRP_summary[[1]][["sd"]]))

CRP_up_limit2 <- (CRP_summary[[2]][["mean"]] + 3*(CRP_summary[[2]][["sd"]]))
CRP_lo_limit2 <- (CRP_summary[[2]][["mean"]] - 3*(CRP_summary[[2]][["sd"]]))

CRP_up_limit3 <- (CRP_summary[[3]][["mean"]] + 3*(CRP_summary[[3]][["sd"]]))
CRP_lo_limit3 <- (CRP_summary[[3]][["mean"]] - 3*(CRP_summary[[3]][["sd"]]))

CRP_up_limit4 <- (CRP_summary[[4]][["mean"]] + 3*(CRP_summary[[4]][["sd"]]))
CRP_lo_limit4 <- (CRP_summary[[4]][["mean"]] - 3*(CRP_summary[[4]][["sd"]]))

# Check the number of outliers prior to winsorizing (where relevant)
# Time 1 two upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 1 & CRP_SOS$CRP_Ave > CRP_up_limit1,na.rm=TRUE)
sum(CRP_SOS$Time == 1 & CRP_SOS$CRP_Ave < CRP_lo_limit1,na.rm=TRUE)

# Time 2 two upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 2 & CRP_SOS$CRP_Ave > CRP_up_limit2,na.rm=TRUE)
sum(CRP_SOS$Time == 2 & CRP_SOS$CRP_Ave < CRP_lo_limit2,na.rm=TRUE)

# Time 3 two upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 3 & CRP_SOS$CRP_Ave > CRP_up_limit3,na.rm=TRUE)
sum(CRP_SOS$Time == 3 & CRP_SOS$CRP_Ave < CRP_lo_limit3,na.rm=TRUE)

# Time 4 three upper limit outliers, zero lower limit
sum(CRP_SOS$Time == 4 & CRP_SOS$CRP_Ave > CRP_up_limit4,na.rm=TRUE)
sum(CRP_SOS$Time == 4 & CRP_SOS$CRP_Ave < CRP_lo_limit4,na.rm=TRUE)

# Winsorize by time point due to outliers, no need to winsorize lower limit as no outliers
CRP_SOS$CRP_nontran_win <- CRP_SOS$CRP_Ave

CRP_SOS$CRP_nontran_win <- ifelse(CRP_SOS$Time==1, (Winsorize(CRP_SOS$CRP_nontran_win, maxval = CRP_up_limit1)),CRP_SOS$CRP_nontran_win)
CRP_SOS$CRP_nontran_win <- ifelse(CRP_SOS$Time==2, (Winsorize(CRP_SOS$CRP_nontran_win, maxval = CRP_up_limit2)),CRP_SOS$CRP_nontran_win)
CRP_SOS$CRP_nontran_win <- ifelse(CRP_SOS$Time==3, (Winsorize(CRP_SOS$CRP_nontran_win, maxval = CRP_up_limit3)),CRP_SOS$CRP_nontran_win)
CRP_SOS$CRP_nontran_win <- ifelse(CRP_SOS$Time==4, (Winsorize(CRP_SOS$CRP_nontran_win, maxval = CRP_up_limit4)),CRP_SOS$CRP_nontran_win)

# Removing outliers on raw data does improve non-normality, but not entirely
CRP_nontran_win_summary <- describeBy(CRP_SOS$CRP_nontran_win, group = CRP_SOS$Time)

# Save the cleaned dataset
write.csv(CRP_SOS, file="SOS_CRP_cleaned.csv")

# Boxplots for CRP
CRP_rawboxplot <- ggplot(CRP_SOS, aes(x = "", y = CRP_Ave)) +
  geom_boxplot() +
  ylab("CRP (ng/mL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

CRP_logboxplot <- ggplot(CRP_SOS, aes(x = "", y = CRP_log)) +   
  geom_boxplot() +
  ylab("CRP (log ng/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

CRP_winboxplot <- ggplot(CRP_SOS, aes(x = "", y = CRP_nontran_win)) +   
  geom_boxplot() +
  ylab("CRP win (ng/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Histograms of non-transformed and log-transformed variable
CRP_hist <-ggplot(CRP_SOS, aes(x=CRP_Ave)) + geom_histogram(color="black", fill = "violetred4", binwidth = 1.0) + theme_bw()
CRP_loghist <-ggplot(CRP_SOS, aes(x=CRP_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.50) + theme_bw()
CRP_winhist <-ggplot(CRP_SOS, aes(x=CRP_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.50) + theme_bw()

grid.arrange(CRP_rawboxplot, CRP_winboxplot, CRP_logboxplot, CRP_hist, CRP_winhist, CRP_loghist, ncol=3)


################################################################################


# Import the first excel tab of TAG dataset without password
# This dataset has been manually edited to change TAG180 duplicates (x4) that were actually TAG081
# One TAG309 3i value was edited to be TAG 1i, with the lowest of duplicates considered baseline
# A TAG164 3i duplicate was also removed, with the value matching supplied GRIDs included as the 'real value'

# Undetectables below the assay lower limits were manually replaced prior to average calculation
# per Millipore Human Cytokine/Chemokine Magnetic Bead Panel 96 Well Plate Assay
# 63 undetected IL-10 (in duplicates), replaced with half the lower limit of detection (i.e., 1.1/2 = 0.55)
# 106 undetected IL-6 (in duplicates), replaced with half the lower limit of detection (i.e., 0.9/2 = 0.45)
# 15 undetected TNF-alpha (in duplicates), replaced with half the lower limit of detection (i.e., 0.7/2 = 0.35)

cytokines_SOS <- read_excel("Human_cytokine_rawdata.xlsx", 1)

# Change all variables to numeric for downstream analysis
cytokines_SOS <- cytokines_SOS %>% 
  mutate_at(vars(IL10_Ave, IL6_Ave, TNFalpha_Ave), as.numeric)

str(cytokines_SOS)

# Number of unique participant IDs
length(unique(cytokines_SOS$SampleID))

# How many participants have IL-10 at each time point?
sum(cytokines_SOS$IL10_Ave != "NA" & cytokines_SOS$Time == "1")
sum(cytokines_SOS$IL10_Ave != "NA" & cytokines_SOS$Time == "2")
sum(cytokines_SOS$IL10_Ave != "NA" & cytokines_SOS$Time == "3")
sum(cytokines_SOS$IL10_Ave != "NA" & cytokines_SOS$Time == "4")

# How many participants have IL-6 at each time point?
sum(cytokines_SOS$IL6_Ave != "NA" & cytokines_SOS$Time == "1")
sum(cytokines_SOS$IL6_Ave != "NA" & cytokines_SOS$Time == "2")
sum(cytokines_SOS$IL6_Ave != "NA" & cytokines_SOS$Time == "3")
sum(cytokines_SOS$IL6_Ave != "NA" & cytokines_SOS$Time == "4")

# How many participants have TNF-alpha at each time point?
sum(cytokines_SOS$TNFalpha_Ave != "NA" & cytokines_SOS$Time == "1")
sum(cytokines_SOS$TNFalpha_Ave != "NA" & cytokines_SOS$Time == "2")
sum(cytokines_SOS$TNFalpha_Ave != "NA" & cytokines_SOS$Time == "3")
sum(cytokines_SOS$TNFalpha_Ave != "NA" & cytokines_SOS$Time == "4")

# Calculate and report normality statistics (skew and kurtosis)
# Kurtosis and Skew should be -2/+2 (West, et al. 1995)
# Most time points are non-normal
IL10_summary <- describeBy(cytokines_SOS$IL10_Ave, group = cytokines_SOS$Time)
IL10_summary

IL6_summary <- describeBy(cytokines_SOS$IL6_Ave, group = cytokines_SOS$Time)
IL6_summary

TNFalpha_summary <- describeBy(cytokines_SOS$TNFalpha_Ave, group = cytokines_SOS$Time)
TNFalpha_summary

# Boxplots for the cytokines
ggplot(cytokines_SOS, aes(x = "", y = IL10_Ave)) +   
  geom_boxplot() +
  ylab("IL-10 (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

ggplot(cytokines_SOS, aes(x = "", y = IL6_Ave)) +   
  geom_boxplot() +
  ylab("IL-6 (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

ggplot(cytokines_SOS, aes(x = "", y = TNFalpha_Ave)) +   
  geom_boxplot() +
  ylab("TNF-alpha (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Calculate the log of each variable
cytokines_SOS$IL10_log <- log(cytokines_SOS$IL10_Ave)
cytokines_SOS$IL6_log <- log(cytokines_SOS$IL6_Ave)
cytokines_SOS$TNFalpha_log <- log(cytokines_SOS$TNFalpha_Ave)

# Calculate and report normality statistics (skew and kurtosis)
# All cytokines are now acceptable +/- 2 skew and kurtosis

IL10_summary_log <- describeBy(cytokines_SOS$IL10_log, group = cytokines_SOS$Time)
IL10_summary_log

IL6_summary_log <- describeBy(cytokines_SOS$IL6_log, group = cytokines_SOS$Time)
IL6_summary_log

TNFalpha_summary_log <- describeBy(cytokines_SOS$TNFalpha_log, group = cytokines_SOS$Time)
TNFalpha_summary_log

# IL-10
IL10log_up_limit1 <- (IL10_summary_log[[1]][["mean"]] + 3*(IL10_summary_log[[1]][["sd"]]))
IL10log_lo_limit1 <- (IL10_summary_log[[1]][["mean"]] - 3*(IL10_summary_log[[1]][["sd"]]))

IL10log_up_limit2 <- (IL10_summary_log[[2]][["mean"]] + 3*(IL10_summary_log[[2]][["sd"]]))
IL10log_lo_limit2 <- (IL10_summary_log[[2]][["mean"]] - 3*(IL10_summary_log[[2]][["sd"]]))

IL10log_up_limit3 <- (IL10_summary_log[[3]][["mean"]] + 3*(IL10_summary_log[[3]][["sd"]]))
IL10log_lo_limit3 <- (IL10_summary_log[[3]][["mean"]] - 3*(IL10_summary_log[[3]][["sd"]]))

IL10log_up_limit4 <- (IL10_summary_log[[4]][["mean"]] + 3*(IL10_summary_log[[4]][["sd"]]))
IL10log_lo_limit4 <- (IL10_summary_log[[4]][["mean"]] - 3*(IL10_summary_log[[4]][["sd"]]))

# IL-6
IL6log_up_limit1 <- (IL6_summary_log[[1]][["mean"]] + 3*(IL6_summary_log[[1]][["sd"]]))
IL6log_lo_limit1 <- (IL6_summary_log[[1]][["mean"]] - 3*(IL6_summary_log[[1]][["sd"]]))

IL6log_up_limit2 <- (IL6_summary_log[[2]][["mean"]] + 3*(IL6_summary_log[[2]][["sd"]]))
IL6log_lo_limit2 <- (IL6_summary_log[[2]][["mean"]] - 3*(IL6_summary_log[[2]][["sd"]]))

IL6log_up_limit3 <- (IL6_summary_log[[3]][["mean"]] + 3*(IL6_summary_log[[3]][["sd"]]))
IL6log_lo_limit3 <- (IL6_summary_log[[3]][["mean"]] - 3*(IL6_summary_log[[3]][["sd"]]))

IL6log_up_limit4 <- (IL6_summary_log[[4]][["mean"]] + 3*(IL6_summary_log[[4]][["sd"]]))
IL6log_lo_limit4 <- (IL6_summary_log[[4]][["mean"]] - 3*(IL6_summary_log[[4]][["sd"]]))

# TNF_alpha
TNFalphalog_up_limit1 <- (TNFalpha_summary_log[[1]][["mean"]] + 3*(TNFalpha_summary_log[[1]][["sd"]]))
TNFalphalog_lo_limit1 <- (TNFalpha_summary_log[[1]][["mean"]] - 3*(TNFalpha_summary_log[[1]][["sd"]]))

TNFalphalog_up_limit2 <- (TNFalpha_summary_log[[2]][["mean"]] + 3*(TNFalpha_summary_log[[2]][["sd"]]))
TNFalphalog_lo_limit2 <- (TNFalpha_summary_log[[2]][["mean"]] - 3*(TNFalpha_summary_log[[2]][["sd"]]))

TNFalphalog_up_limit3 <- (TNFalpha_summary_log[[3]][["mean"]] + 3*(TNFalpha_summary_log[[3]][["sd"]]))
TNFalphalog_lo_limit3 <- (TNFalpha_summary_log[[3]][["mean"]] - 3*(TNFalpha_summary_log[[3]][["sd"]]))

TNFalphalog_up_limit4 <- (TNFalpha_summary_log[[4]][["mean"]] + 3*(TNFalpha_summary_log[[4]][["sd"]]))
TNFalphalog_lo_limit4 <- (TNFalpha_summary_log[[4]][["mean"]] - 3*(TNFalpha_summary_log[[4]][["sd"]]))

# IL-10
# Check the number of outliers prior to winsorizing (where relevant)
# Time 1, zero upper limit outliers, zero lower limit. 
# Winsorization of raw data therefore loses a lot of variation
sum(cytokines_SOS$Time == 1 & cytokines_SOS$IL10_log > IL10log_up_limit1,na.rm=TRUE)
sum(cytokines_SOS$Time == 1 & cytokines_SOS$IL10_log < IL10log_lo_limit1,na.rm=TRUE)

# Time 2, zero upper limit outliers, zero lower limit
sum(cytokines_SOS$Time == 2 & cytokines_SOS$IL10_log > IL10log_up_limit2,na.rm=TRUE)
sum(cytokines_SOS$Time == 2 & cytokines_SOS$IL10_log < IL10log_lo_limit2,na.rm=TRUE)

# Time 3, zero upper limit outliers, zero lower limit
sum(cytokines_SOS$Time == 3 & cytokines_SOS$IL10_log > IL10log_up_limit3,na.rm=TRUE)
sum(cytokines_SOS$Time == 3 & cytokines_SOS$IL10_log < IL10log_lo_limit3,na.rm=TRUE)

# Time 4, zero upper limit outliers, zero lower limit
sum(cytokines_SOS$Time == 4 & cytokines_SOS$IL10_log > IL10log_up_limit4,na.rm=TRUE)
sum(cytokines_SOS$Time == 4 & cytokines_SOS$IL10_log < IL10log_lo_limit4,na.rm=TRUE)

# Boxplots for the cytokines
ggplot(cytokines_SOS, aes(x = "", y = IL10_log)) +   
  geom_boxplot() +
  ylab("log IL-10 (pg/mL)") +
  ggtitle("SOS IL-10 log transformed") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

ggplot(cytokines_SOS, aes(x = "", y = IL6_log)) +   
  geom_boxplot() +
  ylab("log IL-6 (pg/mL)") +
  ggtitle("SOS IL-6 log transformed") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

ggplot(cytokines_SOS, aes(x = "", y = TNFalpha_log)) +   
  geom_boxplot() +
  ylab("log TNF-alpha (pg/mL)") +
  ggtitle("SOS TNF-alpha log transformed") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Histograms of transformed variables
ggplot(cytokines_SOS, aes(x=IL10_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.50) + theme_bw()
ggplot(cytokines_SOS, aes(x=IL6_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.25) + theme_bw()
ggplot(cytokines_SOS, aes(x=TNFalpha_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.20) + theme_bw()

# Winsorize any observations that are +/-3 SDs (IL-10, IL-6, TNF-alpha)
# First calculate the +/-3 SDs from the mean for each variable
IL10_up_limit <- (IL10_summary$mean + 3*(IL10_summary$sd))
IL6_up_limit <- (IL6_summary$mean + 3*(IL6_summary$sd))
TNFalpha_up_limit <- (TNFalpha_summary$mean + 3*(TNFalpha_summary$sd))

IL10_lo_limit <- (IL10_summary$mean - 3*(IL10_summary$sd))
IL6_lo_limit <- (IL6_summary$mean - 3*(IL6_summary$sd))
TNFalpha_lo_limit <- (TNFalpha_summary$mean - 3*(TNFalpha_summary$sd))

# Michelle to discuss downstream options with collaborators, therefore log transformed and "raw" winsorized to be exported
# Winsorize any variables that are +/-3 SDs from the mean (within time points)
# First calculate the +/-3 SDs from the mean for each variable
# IL-10
IL10_up_limit1 <- (IL10_summary[[1]][["mean"]] + 3*(IL10_summary[[1]][["sd"]]))
IL10_lo_limit1 <- (IL10_summary[[1]][["mean"]] - 3*(IL10_summary[[1]][["sd"]]))

IL10_up_limit2 <- (IL10_summary[[2]][["mean"]] + 3*(IL10_summary[[2]][["sd"]]))
IL10_lo_limit2 <- (IL10_summary[[2]][["mean"]] - 3*(IL10_summary[[2]][["sd"]]))

IL10_up_limit3 <- (IL10_summary[[3]][["mean"]] + 3*(IL10_summary[[3]][["sd"]]))
IL10_lo_limit3 <- (IL10_summary[[3]][["mean"]] - 3*(IL10_summary[[3]][["sd"]]))

IL10_up_limit4 <- (IL10_summary[[4]][["mean"]] + 3*(IL10_summary[[4]][["sd"]]))
IL10_lo_limit4 <- (IL10_summary[[4]][["mean"]] - 3*(IL10_summary[[4]][["sd"]]))

# IL-6
IL6_up_limit1 <- (IL6_summary[[1]][["mean"]] + 3*(IL6_summary[[1]][["sd"]]))
IL6_lo_limit1 <- (IL6_summary[[1]][["mean"]] - 3*(IL6_summary[[1]][["sd"]]))

IL6_up_limit2 <- (IL6_summary[[2]][["mean"]] + 3*(IL6_summary[[2]][["sd"]]))
IL6_lo_limit2 <- (IL6_summary[[2]][["mean"]] - 3*(IL6_summary[[2]][["sd"]]))

IL6_up_limit3 <- (IL6_summary[[3]][["mean"]] + 3*(IL6_summary[[3]][["sd"]]))
IL6_lo_limit3 <- (IL6_summary[[3]][["mean"]] - 3*(IL6_summary[[3]][["sd"]]))

IL6_up_limit4 <- (IL6_summary[[4]][["mean"]] + 3*(IL6_summary[[4]][["sd"]]))
IL6_lo_limit4 <- (IL6_summary[[4]][["mean"]] - 3*(IL6_summary[[4]][["sd"]]))

# TNF_alpha
TNFalpha_up_limit1 <- (TNFalpha_summary[[1]][["mean"]] + 3*(TNFalpha_summary[[1]][["sd"]]))
TNFalpha_lo_limit1 <- (TNFalpha_summary[[1]][["mean"]] - 3*(TNFalpha_summary[[1]][["sd"]]))

TNFalpha_up_limit2 <- (TNFalpha_summary[[2]][["mean"]] + 3*(TNFalpha_summary[[2]][["sd"]]))
TNFalpha_lo_limit2 <- (TNFalpha_summary[[2]][["mean"]] - 3*(TNFalpha_summary[[2]][["sd"]]))

TNFalpha_up_limit3 <- (TNFalpha_summary[[3]][["mean"]] + 3*(TNFalpha_summary[[3]][["sd"]]))
TNFalpha_lo_limit3 <- (TNFalpha_summary[[3]][["mean"]] - 3*(TNFalpha_summary[[3]][["sd"]]))

TNFalpha_up_limit4 <- (TNFalpha_summary[[4]][["mean"]] + 3*(TNFalpha_summary[[4]][["sd"]]))
TNFalpha_lo_limit4 <- (TNFalpha_summary[[4]][["mean"]] - 3*(TNFalpha_summary[[4]][["sd"]]))

# IL-10
# Check the number of outliers prior to winsorizing (where relevant)
# Time 1, 1 upper limit outliers, zero lower limit. 
# Winsorization of raw data therefore loses a lot of variation
sum(cytokines_SOS$Time == 1 & cytokines_SOS$IL10_Ave > IL10_up_limit1,na.rm=TRUE)
sum(cytokines_SOS$Time == 1 & cytokines_SOS$IL10_Ave < IL10_lo_limit1,na.rm=TRUE)

# Time 2 1 upper limit outliers, zero lower limit
sum(cytokines_SOS$Time == 2 & cytokines_SOS$IL10_Ave > IL10_up_limit2,na.rm=TRUE)
sum(cytokines_SOS$Time == 2 & cytokines_SOS$IL10_Ave < IL10_lo_limit2,na.rm=TRUE)

# Time 3 1 upper limit outliers, zero lower limit
sum(cytokines_SOS$Time == 3 & cytokines_SOS$IL10_Ave > IL10_up_limit3,na.rm=TRUE)
sum(cytokines_SOS$Time == 3 & cytokines_SOS$IL10_Ave < IL10_lo_limit3,na.rm=TRUE)

# Time 4 1 upper limit outliers, zero lower limit
sum(cytokines_SOS$Time == 4 & cytokines_SOS$IL10_Ave > IL10_up_limit4,na.rm=TRUE)
sum(cytokines_SOS$Time == 4 & cytokines_SOS$IL10_Ave < IL10_lo_limit4,na.rm=TRUE)

# Winsorize by time point due to outliers, no need to winsorize lower limit as no outliers
# IL-10
cytokines_SOS$IL10_nontran_win <- cytokines_SOS$IL10_Ave

cytokines_SOS$IL10_nontran_win  <- ifelse(cytokines_SOS$Time==1, (Winsorize(cytokines_SOS$IL10_nontran_win, maxval = IL10_up_limit1)),cytokines_SOS$IL10_nontran_win)
cytokines_SOS$IL10_nontran_win  <- ifelse(cytokines_SOS$Time==2, (Winsorize(cytokines_SOS$IL10_nontran_win, maxval = IL10_up_limit2)),cytokines_SOS$IL10_nontran_win)
cytokines_SOS$IL10_nontran_win  <- ifelse(cytokines_SOS$Time==3, (Winsorize(cytokines_SOS$IL10_nontran_win, maxval = IL10_up_limit3)),cytokines_SOS$IL10_nontran_win)
cytokines_SOS$IL10_nontran_win  <- ifelse(cytokines_SOS$Time==4, (Winsorize(cytokines_SOS$IL10_nontran_win, maxval = IL10_up_limit4)),cytokines_SOS$IL10_nontran_win)

# IL-6
cytokines_SOS$IL6_nontran_win <- cytokines_SOS$IL6_Ave

cytokines_SOS$IL6_nontran_win  <- ifelse(cytokines_SOS$Time==1, (Winsorize(cytokines_SOS$IL6_nontran_win, maxval = IL6_up_limit1)),cytokines_SOS$IL6_nontran_win)
cytokines_SOS$IL6_nontran_win  <- ifelse(cytokines_SOS$Time==2, (Winsorize(cytokines_SOS$IL6_nontran_win, maxval = IL6_up_limit2)),cytokines_SOS$IL6_nontran_win)
cytokines_SOS$IL6_nontran_win  <- ifelse(cytokines_SOS$Time==3, (Winsorize(cytokines_SOS$IL6_nontran_win, maxval = IL6_up_limit3)),cytokines_SOS$IL6_nontran_win)
cytokines_SOS$IL6_nontran_win  <- ifelse(cytokines_SOS$Time==4, (Winsorize(cytokines_SOS$IL6_nontran_win, maxval = IL6_up_limit4)),cytokines_SOS$IL6_nontran_win)

# TNF_alpha
cytokines_SOS$TNFalpha_nontran_win <- cytokines_SOS$TNFalpha_Ave

cytokines_SOS$TNFalpha_nontran_win  <- ifelse(cytokines_SOS$Time==1, (Winsorize(cytokines_SOS$TNFalpha_nontran_win, maxval = TNFalpha_up_limit1)),cytokines_SOS$TNFalpha_nontran_win)
cytokines_SOS$TNFalpha_nontran_win  <- ifelse(cytokines_SOS$Time==2, (Winsorize(cytokines_SOS$TNFalpha_nontran_win, maxval = TNFalpha_up_limit2)),cytokines_SOS$TNFalpha_nontran_win)
cytokines_SOS$TNFalpha_nontran_win  <- ifelse(cytokines_SOS$Time==3, (Winsorize(cytokines_SOS$TNFalpha_nontran_win, maxval = TNFalpha_up_limit3)),cytokines_SOS$TNFalpha_nontran_win)
cytokines_SOS$TNFalpha_nontran_win  <- ifelse(cytokines_SOS$Time==4, (Winsorize(cytokines_SOS$TNFalpha_nontran_win, maxval = TNFalpha_up_limit4)),cytokines_SOS$TNFalpha_nontran_win)


# Removing outliers on raw data does improve non-normality, but not entirely
IL6_nontran_win_summary <- describeBy(cytokines_SOS$IL6_nontran_win, group = cytokines_SOS$Time)
IL10_nontran_win_summary <- describeBy(cytokines_SOS$IL10_nontran_win, group = cytokines_SOS$Time)
TNFalpha_nontran_win_summary <- describeBy(cytokines_SOS$TNFalpha_nontran_win, group = cytokines_SOS$Time)

# Not necessary to winsorize as no outliers
# Save the cleaned dataset
write.csv(cytokines_SOS, file="SOS_cytokines_cleaned.csv")


# Boxplots for IL6
IL6_rawboxplot <- ggplot(cytokines_SOS, aes(x = "", y = IL6_Ave)) +
  geom_boxplot() +
  ylab("IL6 (pg/mL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

IL6_logboxplot <- ggplot(cytokines_SOS, aes(x = "", y = IL6_log)) +   
  geom_boxplot() +
  ylab("IL6 (log pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

IL6_winboxplot <- ggplot(cytokines_SOS, aes(x = "", y = IL6_nontran_win)) +   
  geom_boxplot() +
  ylab("IL6 win (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# IL6 histograms of non-transformed and log-transformed variable
IL6_hist <-ggplot(cytokines_SOS, aes(x=IL6_Ave)) + geom_histogram(color="black", fill = "violetred4", binwidth = 5.0) + theme_bw()
IL6_loghist <-ggplot(cytokines_SOS, aes(x=IL6_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.50) + theme_bw()
IL6_winhist <-ggplot(cytokines_SOS, aes(x=IL6_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = 2.5) + theme_bw()

grid.arrange(IL6_rawboxplot, IL6_winboxplot, IL6_logboxplot, IL6_hist, IL6_winhist, IL6_loghist, ncol=3)

# Boxplots for IL10
IL10_rawboxplot <- ggplot(cytokines_SOS, aes(x = "", y = IL10_Ave)) +
  geom_boxplot() +
  ylab("IL10 (pg/mL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

IL10_logboxplot <- ggplot(cytokines_SOS, aes(x = "", y = IL10_log)) +   
  geom_boxplot() +
  ylab("IL10 (log pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

IL10_winboxplot <- ggplot(cytokines_SOS, aes(x = "", y = IL10_nontran_win)) +   
  geom_boxplot() +
  ylab("IL10 win (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())


# IL10 histograms of non-transformed and log-transformed variable
IL10_hist <-ggplot(cytokines_SOS, aes(x=IL10_Ave)) + geom_histogram(color="black", fill = "violetred4", binwidth = 5.0) + theme_bw()
IL10_loghist <-ggplot(cytokines_SOS, aes(x=IL10_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.50) + theme_bw()
IL10_winhist <-ggplot(cytokines_SOS, aes(x=IL10_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = 2.5) + theme_bw()

grid.arrange(IL10_rawboxplot, IL10_winboxplot, IL10_logboxplot, IL10_hist, IL10_winhist, IL10_loghist, ncol=3)


# Boxplots for TNFalpha
TNFalpha_rawboxplot <- ggplot(cytokines_SOS, aes(x = "", y = TNFalpha_Ave)) +
  geom_boxplot() +
  ylab("TNFalpha (pg/mL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

TNFalpha_logboxplot <- ggplot(cytokines_SOS, aes(x = "", y = TNFalpha_log)) +   
  geom_boxplot() +
  ylab("TNFalpha (log pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

TNFalpha_winboxplot <- ggplot(cytokines_SOS, aes(x = "", y = TNFalpha_nontran_win)) +   
  geom_boxplot() +
  ylab("TNFalpha win (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())


# TNFalpha histograms of non-transformed and log-transformed variable
TNFalpha_hist <-ggplot(cytokines_SOS, aes(x=TNFalpha_Ave)) + geom_histogram(color="black", fill = "violetred4", binwidth = 10) + theme_bw()
TNFalpha_loghist <-ggplot(cytokines_SOS, aes(x=TNFalpha_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = .50) + theme_bw()
TNFalpha_winhist <-ggplot(cytokines_SOS, aes(x=TNFalpha_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = 5.0) + theme_bw()

grid.arrange(TNFalpha_rawboxplot, TNFalpha_winboxplot, TNFalpha_logboxplot, TNFalpha_hist, TNFalpha_winhist, TNFalpha_loghist, ncol=3)


#################################################################################
# Import tab one of dataset without password
# This dataset has only been manually edited to change TAG180 duplicates (x4) that were actually TAG081, as indicated by the duplicates (all replaced were in hormone box 6)
hormones_SOS <- read_excel("Masterfile_SOS_Saliva_12.04.2020_updated.xlsx", 1)

# Replace variable names with R friendly values
replacement_variable_names <- c("SampleID", "DHEA_pg_mL", "Testosterone_pg_mL", "Cortisol_ug_dL", "Estradiol_pg_mL", "comments")
names(hormones_SOS) <- (replacement_variable_names)

# Replacing undetectables with lower limit of sensitivity, per Salimetrics assay kits
# https://salimetrics.com/assay-kits/

# DHEA
# 5 pg/mL

# Testosterone
# 1 pg/mL

# Cortisol
# 0.007 μg/dL

# Estradiol
# 0.1 pg/mL


# Which variables have undetectables below the lower limit (i.e., QNS in read out)
# No values are above the detectable range
summary(hormones_SOS)

# DHEA, Testosterone and Estradiol contain QNS values, not Cortisol
QNS_present <- which(hormones_SOS == "QNS",arr.ind=TRUE)
colnames(hormones_SOS[,QNS_present[,2]])

# Which participants have QNS values on these variables?
# Shows that one participant has missing Testosterone and DHEA, and another missing Testosterone
row.names(hormones_SOS)[which(hormones_SOS$DHEA_pg_mL=="QNS")]
row.names(hormones_SOS)[which(hormones_SOS$Testosterone_pg_mL=="QNS")]
row.names(hormones_SOS)[which(hormones_SOS$Estradiol_pg_mL=="QNS")]

# Replace DHEA, Testosterone and Estradiol values with assay lower limit (see above Salimetrics values)
lowerlimitDHEA <- 5
lowerlimitTestosterone <- 1
lowerlimitEstradiol <- 0.1

hormones_SOS$DHEA_pg_mL[hormones_SOS$DHEA_pg_mL == "QNS"] <- lowerlimitDHEA
hormones_SOS$Testosterone_pg_mL[hormones_SOS$Testosterone_pg_mL == "QNS"] <- lowerlimitTestosterone
hormones_SOS$Estradiol_pg_mL[hormones_SOS$Estradiol_pg_mL == "QNS"] <- lowerlimitEstradiol

# Change all variables to numeric for downstream analysis
hormones_SOS <- hormones_SOS %>% 
  mutate_at(vars(DHEA_pg_mL, Testosterone_pg_mL, Cortisol_ug_dL, Estradiol_pg_mL), as.numeric)

str(hormones_SOS)

# Keeping old naming variable (Sample ID - Time) in case useful
# Then making two SampleID and Time variables separately
hormones_SOS$SampleID_Time <- hormones_SOS$SampleID
IDtimeseparated <- as.data.frame(str_split_fixed(hormones_SOS$SampleID, " ", 2))
hormones_SOS$SampleID <- IDtimeseparated$V1
hormones_SOS$Time <- IDtimeseparated$V2
hormones_SOS$Time <- strtrim(hormones_SOS$Time, 1)
# 78 unique participant IDs
length(unique(hormones_SOS$SampleID))

# 75 participants have Estradiol (only collected at baseline)
sum(hormones_SOS$Estradiol_pg_mL != "NA" & hormones_SOS$Time == "1")

# How many participants have Testosterone at each time point?
sum(hormones_SOS$Testosterone_pg_mL != "NA" & hormones_SOS$Time == "1")
sum(hormones_SOS$Testosterone_pg_mL != "NA" & hormones_SOS$Time == "2")
sum(hormones_SOS$Testosterone_pg_mL != "NA" & hormones_SOS$Time == "3")
sum(hormones_SOS$Testosterone_pg_mL != "NA" & hormones_SOS$Time == "4")

# How many participants have Cortisol at each time point?
sum(hormones_SOS$Cortisol_ug_dL != "NA" & hormones_SOS$Time == "1")
sum(hormones_SOS$Cortisol_ug_dL != "NA" & hormones_SOS$Time == "2")
sum(hormones_SOS$Cortisol_ug_dL != "NA" & hormones_SOS$Time == "3")
sum(hormones_SOS$Cortisol_ug_dL != "NA" & hormones_SOS$Time == "4")

# How many participants have DHEA at each time point?
# 75 participants have all hormones at baseline (1 hr)
# 68 participants have DHEA, Testosterone and Cortisol at 2 hr
# 71 participants have DHEA, Testosterone and Cortisol at 3 hr
# 69 participants have DHEA, Testosterone and Cortisol at 4 hr
sum(hormones_SOS$DHEA_pg_mL != "NA" & hormones_SOS$Time == "1")
sum(hormones_SOS$DHEA_pg_mL != "NA" & hormones_SOS$Time == "2")
sum(hormones_SOS$DHEA_pg_mL != "NA" & hormones_SOS$Time == "3")
sum(hormones_SOS$DHEA_pg_mL != "NA" & hormones_SOS$Time == "4")

# Visualising outliers
ggplot(hormones_SOS, aes(x=DHEA_pg_mL)) + geom_histogram(color="black", fill = "violetred4", binwidth = 20) + theme_bw()
ggplot(hormones_SOS, aes(x=Testosterone_pg_mL)) + geom_histogram(color="black", fill = "violetred4", binwidth = 4) + theme_bw()
ggplot(hormones_SOS, aes(x=Cortisol_ug_dL)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.03) + theme_bw()
ggplot(hormones_SOS, aes(x=Estradiol_pg_mL)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.05) + theme_bw()

# Calculate and report normality statistics (skew and kurtosis)
# Kurtosis and Skew should be -2/+2 (West, et al. 1995)

DHEA_summary <- describeBy(hormones_SOS$DHEA_pg_mL, group = hormones_SOS$Time)
DHEA_summary

Testosterone_summary <- describeBy(hormones_SOS$Testosterone_pg_mL, group = hormones_SOS$Time)
Testosterone_summary

Cortisol_summary <- describeBy(hormones_SOS$Cortisol_ug_dL, group = hormones_SOS$Time)
Cortisol_summary

Estradiol_summary <- describe(hormones_SOS$Estradiol_pg_mL)
Estradiol_summary

# Boxplots for the hormones
ggplot(hormones_SOS, aes(x = "", y = DHEA_pg_mL)) +   
  geom_boxplot() +
  ylab("DHEA (pg/mL)") +
  ggtitle("SOS DHEA") +
  geom_smooth(method='lm', color="black") + theme_bw()

ggplot(hormones_SOS, aes(x = "", y = Testosterone_pg_mL)) +   
  geom_boxplot() +
  ylab("Testosterone (pg/mL)") +
  ggtitle("SOS Testosterone") +
  geom_smooth(method='lm', color="black") + theme_bw()

ggplot(hormones_SOS, aes(x = "", y = Cortisol_ug_dL)) +   
  geom_boxplot() +
  ylab("Cortisol (ug/dL)") +
  ggtitle("SOS Cortisol") +
  geom_smooth(method='lm', color="black") + theme_bw()

ggplot(hormones_SOS, aes(x = "", y = Estradiol_pg_mL)) +   
  geom_boxplot() +
  ylab("Estradiol (pg/mL)") +
  ggtitle("SOS Estradiol") +
  geom_smooth(method='lm', color="black") + theme_bw()

# Calculate the log of each variable
hormones_SOS$DHEA_log <- log(hormones_SOS$DHEA_pg_mL)
hormones_SOS$Testosterone_log <- log(hormones_SOS$Testosterone_pg_mL)
hormones_SOS$Cortisol_log <- log(hormones_SOS$Cortisol_ug_dL)
hormones_SOS$Estradiol_log <- log(hormones_SOS$Estradiol_pg_mL)

# Calculate and report normality statistics (skew and kurtosis)
# Most hormones are now acceptable +/- 2 skew and kurtosis, but not all
# Estradiol only available at one timepoint

DHEA_summary_log <- describeBy(hormones_SOS$DHEA_log, group = hormones_SOS$Time)
DHEA_summary_log

Testosterone_summary_log <- describeBy(hormones_SOS$Testosterone_log, group = hormones_SOS$Time)
Testosterone_summary_log

Cortisol_summary_log <- describeBy(hormones_SOS$Cortisol_log, group = hormones_SOS$Time)
Cortisol_summary_log

Estradiol_summary_log <- describe(hormones_SOS$Estradiol_log)
Estradiol_summary_log


# DHEA log
DHEAlog_up_limit1 <- (DHEA_summary_log[[1]][["mean"]] + 3*(DHEA_summary_log[[1]][["sd"]]))
DHEAlog_lo_limit1 <- (DHEA_summary_log[[1]][["mean"]] - 3*(DHEA_summary_log[[1]][["sd"]]))

DHEAlog_up_limit2 <- (DHEA_summary_log[[2]][["mean"]] + 3*(DHEA_summary_log[[2]][["sd"]]))
DHEAlog_lo_limit2 <- (DHEA_summary_log[[2]][["mean"]] - 3*(DHEA_summary_log[[2]][["sd"]]))

DHEAlog_up_limit3 <- (DHEA_summary_log[[3]][["mean"]] + 3*(DHEA_summary_log[[3]][["sd"]]))
DHEAlog_lo_limit3 <- (DHEA_summary_log[[3]][["mean"]] - 3*(DHEA_summary_log[[3]][["sd"]]))

DHEAlog_up_limit4 <- (DHEA_summary_log[[4]][["mean"]] + 3*(DHEA_summary_log[[4]][["sd"]]))
DHEAlog_lo_limit4 <- (DHEA_summary_log[[4]][["mean"]] - 3*(DHEA_summary_log[[4]][["sd"]]))

# Testosterone
Testosteronelog_up_limit1 <- (Testosterone_summary_log[[1]][["mean"]] + 3*(Testosterone_summary_log[[1]][["sd"]]))
Testosteronelog_lo_limit1 <- (Testosterone_summary_log[[1]][["mean"]] - 3*(Testosterone_summary_log[[1]][["sd"]]))

Testosteronelog_up_limit2 <- (Testosterone_summary_log[[2]][["mean"]] + 3*(Testosterone_summary_log[[2]][["sd"]]))
Testosteronelog_lo_limit2 <- (Testosterone_summary_log[[2]][["mean"]] - 3*(Testosterone_summary_log[[2]][["sd"]]))

Testosteronelog_up_limit3 <- (Testosterone_summary_log[[3]][["mean"]] + 3*(Testosterone_summary_log[[3]][["sd"]]))
Testosteronelog_lo_limit3 <- (Testosterone_summary_log[[3]][["mean"]] - 3*(Testosterone_summary_log[[3]][["sd"]]))

Testosteronelog_up_limit4 <- (Testosterone_summary_log[[4]][["mean"]] + 3*(Testosterone_summary_log[[4]][["sd"]]))
Testosteronelog_lo_limit4 <- (Testosterone_summary_log[[4]][["mean"]] - 3*(Testosterone_summary_log[[4]][["sd"]]))

# Cortisol
Cortisollog_up_limit1 <- (Cortisol_summary_log[[1]][["mean"]] + 3*(Cortisol_summary_log[[1]][["sd"]]))
Cortisollog_lo_limit1 <- (Cortisol_summary_log[[1]][["mean"]] - 3*(Cortisol_summary_log[[1]][["sd"]]))

Cortisollog_up_limit2 <- (Cortisol_summary_log[[2]][["mean"]] + 3*(Cortisol_summary_log[[2]][["sd"]]))
Cortisollog_lo_limit2 <- (Cortisol_summary_log[[2]][["mean"]] - 3*(Cortisol_summary_log[[2]][["sd"]]))

Cortisollog_up_limit3 <- (Cortisol_summary_log[[3]][["mean"]] + 3*(Cortisol_summary_log[[3]][["sd"]]))
Cortisollog_lo_limit3 <- (Cortisol_summary_log[[3]][["mean"]] - 3*(Cortisol_summary_log[[3]][["sd"]]))

Cortisollog_up_limit4 <- (Cortisol_summary_log[[4]][["mean"]] + 3*(Cortisol_summary_log[[4]][["sd"]]))
Cortisollog_lo_limit4 <- (Cortisol_summary_log[[4]][["mean"]] - 3*(Cortisol_summary_log[[4]][["sd"]]))

# Estradiol (only one time point so no need to access "describe by" data, just regular mean and sd)
Estradiollog_up_limit <- (Estradiol_summary_log$mean + 3*(Estradiol_summary_log$sd))
Estradiollog_lo_limit <- (Estradiol_summary_log$mean - 3*(Estradiol_summary_log$sd))

# DHEA log
# Check the number of outliers prior to winsorizing 
# Time 1, zero upper limit outliers, two lower limit. 
sum(hormones_SOS$Time == 1 & hormones_SOS$DHEA_log > DHEAlog_up_limit1,na.rm=TRUE)
sum(hormones_SOS$Time == 1 & hormones_SOS$DHEA_log < DHEAlog_lo_limit1,na.rm=TRUE)

# Time 2, zero upper limit outliers, one lower limit
sum(hormones_SOS$Time == 2 & hormones_SOS$DHEA_log > DHEAlog_up_limit2,na.rm=TRUE)
sum(hormones_SOS$Time == 2 & hormones_SOS$DHEA_log < DHEAlog_lo_limit2,na.rm=TRUE)

# Time 3, zero upper limit outliers, one lower limit
sum(hormones_SOS$Time == 3 & hormones_SOS$DHEA_log > DHEAlog_up_limit3,na.rm=TRUE)
sum(hormones_SOS$Time == 3 & hormones_SOS$DHEA_log < DHEAlog_lo_limit3,na.rm=TRUE)

# Time 4, zero upper limit outliers, zero lower limit
sum(hormones_SOS$Time == 4 & hormones_SOS$DHEA_log >  DHEAlog_up_limit4,na.rm=TRUE)
sum(hormones_SOS$Time == 4 & hormones_SOS$DHEA_log <  DHEAlog_lo_limit4,na.rm=TRUE)

# Testosterone log
# Check the number of outliers prior to winsorizing 
# Time 1, zero upper limit outliers, one lower limit. 
sum(hormones_SOS$Time == 1 & hormones_SOS$Testosterone_log > Testosteronelog_up_limit1,na.rm=TRUE)
sum(hormones_SOS$Time == 1 & hormones_SOS$Testosterone_log < Testosteronelog_lo_limit1,na.rm=TRUE)

# Time 2, zero upper limit outliers, zero lower limit
sum(hormones_SOS$Time == 2 & hormones_SOS$Testosterone_log > Testosteronelog_up_limit2,na.rm=TRUE)
sum(hormones_SOS$Time == 2 & hormones_SOS$Testosterone_log < Testosteronelog_lo_limit2,na.rm=TRUE)

# Time 3, zero upper limit outlier, two lower limit
sum(hormones_SOS$Time == 3 & hormones_SOS$Testosterone_log > Testosteronelog_up_limit3,na.rm=TRUE)
sum(hormones_SOS$Time == 3 & hormones_SOS$Testosterone_log < Testosteronelog_lo_limit3,na.rm=TRUE)

# Time 4 1 zero limit outliers, one lower limit
sum(hormones_SOS$Time == 4 & hormones_SOS$Testosterone_log > Testosteronelog_up_limit4,na.rm=TRUE)
sum(hormones_SOS$Time == 4 & hormones_SOS$Testosterone_log < Testosteronelog_lo_limit4,na.rm=TRUE)


# Cortisol log
# Check the number of outliers prior to winsorizing 
# Time 1, zero upper limit outliers, zero lower limit. 
sum(hormones_SOS$Time == 1 & hormones_SOS$Cortisol_log > Cortisollog_up_limit1,na.rm=TRUE)
sum(hormones_SOS$Time == 1 & hormones_SOS$Cortisol_log < Cortisollog_lo_limit1,na.rm=TRUE)

# Time 2, zero upper limit outlier, zero lower limit
sum(hormones_SOS$Time == 2 & hormones_SOS$Cortisol_log > Cortisollog_up_limit2,na.rm=TRUE)
sum(hormones_SOS$Time == 2 & hormones_SOS$Cortisol_log < Cortisollog_lo_limit2,na.rm=TRUE)

# Time 3, zero upper limit outliers, one lower limit
sum(hormones_SOS$Time == 3 & hormones_SOS$Cortisol_log > Cortisollog_up_limit3,na.rm=TRUE)
sum(hormones_SOS$Time == 3 & hormones_SOS$Cortisol_log < Cortisollog_lo_limit3,na.rm=TRUE)

# Time 4, 0 upper limit outlier, zero lower limit
sum(hormones_SOS$Time == 4 & hormones_SOS$Cortisol_log > Cortisollog_up_limit4,na.rm=TRUE)
sum(hormones_SOS$Time == 4 & hormones_SOS$Cortisol_log < Cortisollog_lo_limit4,na.rm=TRUE)


# Estradio log
# Check the number of outliers prior to winsorizing 
# Estradiol is only collected at one time point
# zero upper limit outliers, one lower limit
sum(hormones_SOS$Estradiol_log > Estradiollog_up_limit,na.rm=TRUE)
sum(hormones_SOS$Estradiol_log < Estradiollog_lo_limit,na.rm=TRUE)

# Winsorize by time point due to outliers, no need to winsorize upper limit as no outliers
# DHEA log (only time 1, 2, 3 contained lower limit outliers)

hormones_SOS$DHEA_log  <- ifelse(hormones_SOS$Time==1, (Winsorize(hormones_SOS$DHEA_log, minval = DHEAlog_lo_limit1)),hormones_SOS$DHEA_log)
hormones_SOS$DHEA_log  <- ifelse(hormones_SOS$Time==2, (Winsorize(hormones_SOS$DHEA_log, minval = DHEAlog_lo_limit2)),hormones_SOS$DHEA_log)
hormones_SOS$DHEA_log  <- ifelse(hormones_SOS$Time==3, (Winsorize(hormones_SOS$DHEA_log, minval = DHEAlog_lo_limit3)),hormones_SOS$DHEA_log)

# Testosterone log (only time 1, 3, 4 contained lower limit outliers)

hormones_SOS$Testosterone_log  <- ifelse(hormones_SOS$Time==1, (Winsorize(hormones_SOS$Testosterone_log, minval = Testosteronelog_lo_limit1)),hormones_SOS$Testosterone_log)
hormones_SOS$Testosterone_log  <- ifelse(hormones_SOS$Time==3, (Winsorize(hormones_SOS$Testosterone_log, minval = Testosteronelog_lo_limit3)),hormones_SOS$Testosterone_log)
hormones_SOS$Testosterone_log  <- ifelse(hormones_SOS$Time==4, (Winsorize(hormones_SOS$Testosterone_log, minval = Testosteronelog_lo_limit4)),hormones_SOS$Testosterone_log)

# Cortisol log (only time 3 contained lower limit outliers)

hormones_SOS$Cortisol_log  <- ifelse(hormones_SOS$Time==3, (Winsorize(hormones_SOS$Cortisol_log, minval = Cortisollog_lo_limit3)),hormones_SOS$Cortisol_log)


# Estradiol (only collected at one time point, with one lower limit outlier)

hormones_SOS$Estradiol_log  <- Winsorize(hormones_SOS$Estradiol_log, minval = Estradiollog_lo_limit, na.rm = TRUE)


# Boxplots for the hormones
ggplot(hormones_SOS, aes(x = "", y = DHEA_log)) +   
  geom_boxplot() +
  ylab("log DHEA (pg/mL)") +
  ggtitle("SOS DHEA log transformed") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

ggplot(hormones_SOS, aes(x = "", y = Testosterone_log)) +   
  geom_boxplot() +
  ylab("log Testosterone (pg/mL)") +
  ggtitle("SOS Testosterone log transformed") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

ggplot(hormones_SOS, aes(x = "", y = Cortisol_log)) +   
  geom_boxplot() +
  ylab("log Cortisol (ug/dL)") +
  ggtitle("SOS Cortisol log transformed") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

ggplot(hormones_SOS, aes(x = "", y = Estradiol_log)) +   
  geom_boxplot() +
  ylab("log Estradiol (pg/mL)") +
  ggtitle("SOS Estradiol log transformed") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Histograms of transformed variables
ggplot(hormones_SOS, aes(x=DHEA_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.50) + theme_bw()
ggplot(hormones_SOS, aes(x=Testosterone_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.50) + theme_bw()
ggplot(hormones_SOS, aes(x=Cortisol_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.25) + theme_bw()
ggplot(hormones_SOS, aes(x=Estradiol_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = 0.20) + theme_bw()

# Michelle to discuss downstream options with collaborators, therefore log non-transformed and "raw" winsorized to be exported
# Winsorize any variables that are +/-3 SDs from the mean (within time points)
# First calculate the +/-3 SDs from the mean for each variable
# DHEA
DHEA_up_limit1 <- (DHEA_summary[[1]][["mean"]] + 3*(DHEA_summary[[1]][["sd"]]))
DHEA_lo_limit1 <- (DHEA_summary[[1]][["mean"]] - 3*(DHEA_summary[[1]][["sd"]]))

DHEA_up_limit2 <- (DHEA_summary[[2]][["mean"]] + 3*(DHEA_summary[[2]][["sd"]]))
DHEA_lo_limit2 <- (DHEA_summary[[2]][["mean"]] - 3*(DHEA_summary[[2]][["sd"]]))

DHEA_up_limit3 <- (DHEA_summary[[3]][["mean"]] + 3*(DHEA_summary[[3]][["sd"]]))
DHEA_lo_limit3 <- (DHEA_summary[[3]][["mean"]] - 3*(DHEA_summary[[3]][["sd"]]))

DHEA_up_limit4 <- (DHEA_summary[[4]][["mean"]] + 3*(DHEA_summary[[4]][["sd"]]))
DHEA_lo_limit4 <- (DHEA_summary[[4]][["mean"]] - 3*(DHEA_summary[[4]][["sd"]]))

# Testosterone
Testosterone_up_limit1 <- (Testosterone_summary[[1]][["mean"]] + 3*(Testosterone_summary[[1]][["sd"]]))
Testosterone_lo_limit1 <- (Testosterone_summary[[1]][["mean"]] - 3*(Testosterone_summary[[1]][["sd"]]))

Testosterone_up_limit2 <- (Testosterone_summary[[2]][["mean"]] + 3*(Testosterone_summary[[2]][["sd"]]))
Testosterone_lo_limit2 <- (Testosterone_summary[[2]][["mean"]] - 3*(Testosterone_summary[[2]][["sd"]]))

Testosterone_up_limit3 <- (Testosterone_summary[[3]][["mean"]] + 3*(Testosterone_summary[[3]][["sd"]]))
Testosterone_lo_limit3 <- (Testosterone_summary[[3]][["mean"]] - 3*(Testosterone_summary[[3]][["sd"]]))

Testosterone_up_limit4 <- (Testosterone_summary[[4]][["mean"]] + 3*(Testosterone_summary[[4]][["sd"]]))
Testosterone_lo_limit4 <- (Testosterone_summary[[4]][["mean"]] - 3*(Testosterone_summary[[4]][["sd"]]))

# Cortisol
Cortisol_up_limit1 <- (Cortisol_summary[[1]][["mean"]] + 3*(Cortisol_summary[[1]][["sd"]]))
Cortisol_lo_limit1 <- (Cortisol_summary[[1]][["mean"]] - 3*(Cortisol_summary[[1]][["sd"]]))

Cortisol_up_limit2 <- (Cortisol_summary[[2]][["mean"]] + 3*(Cortisol_summary[[2]][["sd"]]))
Cortisol_lo_limit2 <- (Cortisol_summary[[2]][["mean"]] - 3*(Cortisol_summary[[2]][["sd"]]))

Cortisol_up_limit3 <- (Cortisol_summary[[3]][["mean"]] + 3*(Cortisol_summary[[3]][["sd"]]))
Cortisol_lo_limit3 <- (Cortisol_summary[[3]][["mean"]] - 3*(Cortisol_summary[[3]][["sd"]]))

Cortisol_up_limit4 <- (Cortisol_summary[[4]][["mean"]] + 3*(Cortisol_summary[[4]][["sd"]]))
Cortisol_lo_limit4 <- (Cortisol_summary[[4]][["mean"]] - 3*(Cortisol_summary[[4]][["sd"]]))

# Estradiol (only one time point so no need to access "describe by" data, just regular mean and sd)
Estradiol_up_limit <- (Estradiol_summary$mean + 3*(Estradiol_summary$sd))
Estradiol_lo_limit <- (Estradiol_summary$mean - 3*(Estradiol_summary$sd))

# DHEA
# Check the number of outliers prior to winsorizing 
# Time 1, 2 upper limit outliers, zero lower limit. 
sum(hormones_SOS$Time == 1 & hormones_SOS$DHEA_pg_mL > DHEA_up_limit1,na.rm=TRUE)
sum(hormones_SOS$Time == 1 & hormones_SOS$DHEA_pg_mL < DHEA_lo_limit1,na.rm=TRUE)

# Time 2, 2 upper limit outliers, zero lower limit
sum(hormones_SOS$Time == 2 & hormones_SOS$DHEA_pg_mL > DHEA_up_limit2,na.rm=TRUE)
sum(hormones_SOS$Time == 2 & hormones_SOS$DHEA_pg_mL < DHEA_lo_limit2,na.rm=TRUE)

# Time 3, 1 upper limit outlier, zero lower limit
sum(hormones_SOS$Time == 3 & hormones_SOS$DHEA_pg_mL > DHEA_up_limit3,na.rm=TRUE)
sum(hormones_SOS$Time == 3 & hormones_SOS$DHEA_pg_mL < DHEA_lo_limit3,na.rm=TRUE)

# Time 4, 0 upper limit outliers, zero lower limit
sum(hormones_SOS$Time == 4 & hormones_SOS$DHEA_pg_mL >  DHEA_up_limit4,na.rm=TRUE)
sum(hormones_SOS$Time == 4 & hormones_SOS$DHEA_pg_mL <  DHEA_lo_limit4,na.rm=TRUE)

# Testosterone
# Check the number of outliers prior to winsorizing 
# Time 1, 1 upper limit outliers, zero lower limit. 
sum(hormones_SOS$Time == 1 & hormones_SOS$Testosterone_pg_mL > Testosterone_up_limit1,na.rm=TRUE)
sum(hormones_SOS$Time == 1 & hormones_SOS$Testosterone_pg_mL < Testosterone_lo_limit1,na.rm=TRUE)

# Time 2, 0 upper limit outliers, zero lower limit
sum(hormones_SOS$Time == 2 & hormones_SOS$Testosterone_pg_mL > Testosterone_up_limit2,na.rm=TRUE)
sum(hormones_SOS$Time == 2 & hormones_SOS$Testosterone_pg_mL < Testosterone_lo_limit2,na.rm=TRUE)

# Time 3, 1 upper limit outlier, zero lower limit
sum(hormones_SOS$Time == 3 & hormones_SOS$Testosterone_pg_mL > Testosterone_up_limit3,na.rm=TRUE)
sum(hormones_SOS$Time == 3 & hormones_SOS$Testosterone_pg_mL < Testosterone_lo_limit3,na.rm=TRUE)

# Time 4 1 upper limit outlier, zero lower limit
sum(hormones_SOS$Time == 4 & hormones_SOS$Testosterone_pg_mL > Testosterone_up_limit4,na.rm=TRUE)
sum(hormones_SOS$Time == 4 & hormones_SOS$Testosterone_pg_mL < Testosterone_lo_limit4,na.rm=TRUE)


# Cortisol
# Check the number of outliers prior to winsorizing 
# Time 1, 2 upper limit outliers, zero lower limit. 
sum(hormones_SOS$Time == 1 & hormones_SOS$Cortisol_ug_dL > Cortisol_up_limit1,na.rm=TRUE)
sum(hormones_SOS$Time == 1 & hormones_SOS$Cortisol_ug_dL < Cortisol_lo_limit1,na.rm=TRUE)

# Time 2, 1 upper limit outlier, zero lower limit
sum(hormones_SOS$Time == 2 & hormones_SOS$Cortisol_ug_dL > Cortisol_up_limit2,na.rm=TRUE)
sum(hormones_SOS$Time == 2 & hormones_SOS$Cortisol_ug_dL < Cortisol_lo_limit2,na.rm=TRUE)

# Time 3, 3 upper limit outliers, zero lower limit
sum(hormones_SOS$Time == 3 & hormones_SOS$Cortisol_ug_dL > Cortisol_up_limit3,na.rm=TRUE)
sum(hormones_SOS$Time == 3 & hormones_SOS$Cortisol_ug_dL < Cortisol_lo_limit3,na.rm=TRUE)

# Time 4, 0 upper limit outlier, zero lower limit
sum(hormones_SOS$Time == 4 & hormones_SOS$Cortisol_ug_dL > Cortisol_up_limit4,na.rm=TRUE)
sum(hormones_SOS$Time == 4 & hormones_SOS$Cortisol_ug_dL < Cortisol_lo_limit4,na.rm=TRUE)


# Estradiol
# Check the number of outliers prior to winsorizing 
# Estradiol is only collected at one time point
# 1 upper limit outliers, zero lower limit
sum(hormones_SOS$Estradiol_pg_mL > Estradiol_up_limit,na.rm=TRUE)
sum(hormones_SOS$Estradiol_pg_mL < Estradiol_lo_limit,na.rm=TRUE)


# Winsorize by time point due to outliers, no need to winsorize lower limit as no outliers
# DHEA
hormones_SOS$DHEA_nontran_win <- hormones_SOS$DHEA_pg_mL

hormones_SOS$DHEA_nontran_win  <- ifelse(hormones_SOS$Time==1, (Winsorize(hormones_SOS$DHEA_nontran_win, maxval = DHEA_up_limit1)),hormones_SOS$DHEA_nontran_win)
hormones_SOS$DHEA_nontran_win  <- ifelse(hormones_SOS$Time==2, (Winsorize(hormones_SOS$DHEA_nontran_win, maxval = DHEA_up_limit2)),hormones_SOS$DHEA_nontran_win)
hormones_SOS$DHEA_nontran_win  <- ifelse(hormones_SOS$Time==3, (Winsorize(hormones_SOS$DHEA_nontran_win, maxval = DHEA_up_limit3)),hormones_SOS$DHEA_nontran_win)
hormones_SOS$DHEA_nontran_win  <- ifelse(hormones_SOS$Time==4, (Winsorize(hormones_SOS$DHEA_nontran_win, maxval = DHEA_up_limit4)),hormones_SOS$DHEA_nontran_win)

# Testosterone
hormones_SOS$Testosterone_nontran_win <- hormones_SOS$Testosterone_pg_mL

hormones_SOS$Testosterone_nontran_win  <- ifelse(hormones_SOS$Time==1, (Winsorize(hormones_SOS$Testosterone_nontran_win, maxval = Testosterone_up_limit1)),hormones_SOS$Testosterone_nontran_win)
hormones_SOS$Testosterone_nontran_win  <- ifelse(hormones_SOS$Time==2, (Winsorize(hormones_SOS$Testosterone_nontran_win, maxval = Testosterone_up_limit2)),hormones_SOS$Testosterone_nontran_win)
hormones_SOS$Testosterone_nontran_win  <- ifelse(hormones_SOS$Time==3, (Winsorize(hormones_SOS$Testosterone_nontran_win, maxval = Testosterone_up_limit3)),hormones_SOS$Testosterone_nontran_win)
hormones_SOS$Testosterone_nontran_win  <- ifelse(hormones_SOS$Time==4, (Winsorize(hormones_SOS$Testosterone_nontran_win, maxval = Testosterone_up_limit4)),hormones_SOS$Testosterone_nontran_win)

# Cortisol
hormones_SOS$Cortisol_nontran_win <- hormones_SOS$Cortisol_ug_dL

hormones_SOS$Cortisol_nontran_win  <- ifelse(hormones_SOS$Time==1, (Winsorize(hormones_SOS$Cortisol_nontran_win, maxval = Cortisol_up_limit1)),hormones_SOS$Cortisol_nontran_win)
hormones_SOS$Cortisol_nontran_win  <- ifelse(hormones_SOS$Time==2, (Winsorize(hormones_SOS$Cortisol_nontran_win, maxval = Cortisol_up_limit2)),hormones_SOS$Cortisol_nontran_win)
hormones_SOS$Cortisol_nontran_win  <- ifelse(hormones_SOS$Time==3, (Winsorize(hormones_SOS$Cortisol_nontran_win, maxval = Cortisol_up_limit3)),hormones_SOS$Cortisol_nontran_win)
hormones_SOS$Cortisol_nontran_win  <- ifelse(hormones_SOS$Time==4, (Winsorize(hormones_SOS$Cortisol_nontran_win, maxval = Cortisol_up_limit4)),hormones_SOS$Cortisol_nontran_win)


# Estradiol
hormones_SOS$Estradiol_nontran_win <- hormones_SOS$Estradiol_pg_mL

hormones_SOS$Estradiol_nontran_win  <- Winsorize(hormones_SOS$Estradiol_pg_mL, maxval = Estradiol_up_limit, na.rm = TRUE)

# Removing outliers on raw data does improve non-normality, but not entirely
DHEA_nontran_win_summary <- describeBy(hormones_SOS$DHEA_nontran_win, group = hormones_SOS$Time)
Testosterone_nontran_win_summary <- describeBy(hormones_SOS$Testosterone_nontran_win, group = hormones_SOS$Time)
Cortisol_nontran_win_summary <- describeBy(hormones_SOS$Cortisol_nontran_win, group = hormones_SOS$Time)
Estradiol_nontran_win_summary <- describe(hormones_SOS$Estradiol_nontran_win)


# Save the cleaned dataset
write.csv(hormones_SOS, file="SOS_hormones_cleaned.csv")


# Boxplots for DHEA
DHEA_rawboxplot <- ggplot(hormones_SOS, aes(x = "", y = DHEA_pg_mL)) +
  geom_boxplot() +
  ylab("DHEA (pg/mL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

DHEA_logboxplot <- ggplot(hormones_SOS, aes(x = "", y = DHEA_log)) +   
  geom_boxplot() +
  ylab("DHEA (log pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

DHEA_winboxplot <- ggplot(hormones_SOS, aes(x = "", y = DHEA_nontran_win)) +   
  geom_boxplot() +
  ylab("DHEA win (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Histograms of non-transformed and log-transformed variable
DHEA_hist <-ggplot(hormones_SOS, aes(x=DHEA_pg_mL)) + geom_histogram(color="black", fill = "violetred4") + theme_bw()
DHEA_loghist <-ggplot(hormones_SOS, aes(x=DHEA_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = .25) + theme_bw()
DHEA_winhist <-ggplot(hormones_SOS, aes(x=DHEA_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = 30) + theme_bw()

grid.arrange(DHEA_rawboxplot, DHEA_winboxplot, DHEA_logboxplot, DHEA_hist, DHEA_winhist, DHEA_loghist, ncol=3)


# Boxplots for Testosterone
Testosterone_rawboxplot <- ggplot(hormones_SOS, aes(x = "", y = Testosterone_pg_mL)) +
  geom_boxplot() +
  ylab("Testosterone (pg/mL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

Testosterone_logboxplot <- ggplot(hormones_SOS, aes(x = "", y = Testosterone_log)) +   
  geom_boxplot() +
  ylab("Testosterone (log pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

Testosterone_winboxplot <- ggplot(hormones_SOS, aes(x = "", y = Testosterone_nontran_win)) +   
  geom_boxplot() +
  ylab("Testosterone win (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Histograms of non-transformed and log-transformed variable
Testosterone_hist <-ggplot(hormones_SOS, aes(x=Testosterone_pg_mL)) + geom_histogram(color="black", fill = "violetred4") + theme_bw()
Testosterone_loghist <-ggplot(hormones_SOS, aes(x=Testosterone_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = .25) + theme_bw()
Testosterone_winhist <-ggplot(hormones_SOS, aes(x=Testosterone_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = 3.0) + theme_bw()

grid.arrange(Testosterone_rawboxplot, Testosterone_winboxplot, Testosterone_logboxplot, Testosterone_hist, Testosterone_winhist, Testosterone_loghist, ncol=3)


# Boxplots for Cortisol
Cortisol_rawboxplot <- ggplot(hormones_SOS, aes(x = "", y = Cortisol_ug_dL)) +
  geom_boxplot() +
  ylab("Cortisol (ug/dL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

Cortisol_logboxplot <- ggplot(hormones_SOS, aes(x = "", y = Cortisol_log)) +   
  geom_boxplot() +
  ylab("Cortisol (log ug/dL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

Cortisol_winboxplot <- ggplot(hormones_SOS, aes(x = "", y = Cortisol_nontran_win)) +   
  geom_boxplot() +
  ylab("Cortisol win (ug/dL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Histograms of non-transformed and log-transformed variable
Cortisol_hist <-ggplot(hormones_SOS, aes(x=Cortisol_ug_dL)) + geom_histogram(color="black", fill = "violetred4") + theme_bw()
Cortisol_loghist <-ggplot(hormones_SOS, aes(x=Cortisol_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = .25) + theme_bw()
Cortisol_winhist <-ggplot(hormones_SOS, aes(x=Cortisol_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = .025) + theme_bw()

grid.arrange(Cortisol_rawboxplot, Cortisol_winboxplot, Cortisol_logboxplot, Cortisol_hist, Cortisol_winhist, Cortisol_loghist, ncol=3)


# Boxplots for Estradiol
Estradiol_rawboxplot <- ggplot(hormones_SOS, aes(x = "", y = Estradiol_pg_mL)) +
  geom_boxplot() +
  ylab("Estradiol (pg/mL)") +
  geom_smooth(method = 'lm', color = "black") + theme(axis.title.x=element_blank())

Estradiol_logboxplot <- ggplot(hormones_SOS, aes(x = "", y = Estradiol_log)) +   
  geom_boxplot() +
  ylab("Estradiol (log pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

Estradiol_winboxplot <- ggplot(hormones_SOS, aes(x = "", y = Estradiol_nontran_win)) +   
  geom_boxplot() +
  ylab("Estradiol win (pg/mL)") +
  geom_smooth(method='lm', color="black") + theme(axis.title.x=element_blank())

# Histograms of non-transformed and log-transformed variable
Estradiol_hist <-ggplot(hormones_SOS, aes(x=Estradiol_pg_mL)) + geom_histogram(color="black", fill = "violetred4") + theme_bw()
Estradiol_loghist <-ggplot(hormones_SOS, aes(x=Estradiol_log)) + geom_histogram(color="black", fill = "violetred4", binwidth = .15) + theme_bw()
Estradiol_winhist <-ggplot(hormones_SOS, aes(x=Estradiol_nontran_win)) + geom_histogram(color="black", fill = "violetred4", binwidth = .10) + theme_bw()

grid.arrange(Estradiol_rawboxplot, Estradiol_winboxplot, Estradiol_logboxplot, Estradiol_hist, Estradiol_winhist, Estradiol_loghist, ncol=3)
