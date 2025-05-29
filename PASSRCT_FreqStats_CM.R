#############################################################
#
#   PASS RCT Analysis R
#   
#
#   ### Author: cmunro ###
#   ### Start Date: 25.03.2025 ###
#
#############################################################

######### Set Up #########
# Required packages:
library(tidyverse)
library(haven)
library(pwr)
library(stringr)
library(car)

# Read in SPSS Master Data File:
setwd("U:/RESEARCH/PASS RCT/DATA/Data Files/Working Data Files/Cmunro_Work/R PASS RCT CM")
tDataRaw <- read_sav("PASS-RCT MAY 2025 - CM R.sav")
# table(tDataRaw$Withdrawn.0)
# table(tDataRaw$Group)

######### Functions #########

## Generate summary statistics:
fSumStatsGroup <- function(tData, sVar) {
  
  # Ensure sVar is treated as a column name
  tData <- tData[, c(sVar, "Group")]
  
  # Split data into groups
  group_data <- split(tData[[sVar]], tData$Group)
  
  # Compute summary statistics for each group
  mStats <- rbind(
    Mean    = sapply(group_data, function(x) round(mean(x, na.rm = TRUE), 2)),
    Count   = sapply(group_data, function(x) sum(!is.na(x))),
    SD      = sapply(group_data, function(x) round(sd(x, na.rm = TRUE), 2)),
    Min     = sapply(group_data, function(x) round(min(x, na.rm = TRUE), 2)),
    Median  = sapply(group_data, function(x) round(median(x, na.rm = TRUE), 2)),
    Max     = sapply(group_data, function(x) round(max(x, na.rm = TRUE), 2)),
    IQR     = sapply(group_data, function(x) round(IQR(x, na.rm = TRUE), 2))
  )
  
  # Rename columns to match group names
  colnames(mStats) <- paste("Group", names(group_data))
  
  return(mStats)
}

## Baseline Summaries Categorical:
fSummariesCat <- function(tData, sVar) {
  
  tSummary <- tData %>%
    # Only considering necessary rows
    select(Group, all_of(sVar)) %>%
    # Split any comma-separated observations
    mutate(cVar = as.character(.data[[sVar]])) %>%
    separate_rows(cVar, sep = ",\\s*") %>%
    # Adjust formatting for readability:
    group_by(Group, cVar) %>%
    summarise(Frequency = n(), .groups = "drop") %>%
    arrange(Group, cVar) %>%
    pivot_wider(
      names_from = Group,
      values_from = Frequency,
      values_fill = 0
    ) %>%
    mutate(Row_Total = rowSums(across(where(is.numeric)))) %>%  # row totals
    bind_rows(
      summarise(., across(where(is.numeric), sum)) %>%
        mutate(cVar = "Total")
    )
  
  return(tSummary)
} 

# Missing IDs:
fMissingID <- function(sVar) {
  
  # Variable of interest:
  vVar <- tAnalysisSet[[sVar]]
  
  # Number missing:
  nMissing <- sum(is.na(vVar))
  
  # Identify specific participants:
  vMissing <- is.na(vVar)
  
  # Participant IDs:
  vIDs <- tAnalysisSet$ID[vMissing]
  
  list(
    variable = sVar,
    count = nMissing,
    IDs = vIDs,
    Group = tAnalysisSet$Group[vMissing]
  )
}

# Missing IDs:
fMissing <- function(vColumns) {
  
  # Number of missing items in questionnaire:
  rowSums(is.na(tAnalysisSet[vColumns]))
  
}

# Missing totals:
fMissingTotals <- function(sVar, tData) {
  df <- data.frame(Group = tData$Group, Missing = sVar)
  
  df %>%
    group_by(Group, Missing) %>%
    summarise(N = n(), .groups = "drop") %>%
    arrange(Group, Missing)
}

# CFB surveys:
fChangeFromBaseline <- function(tData, sPrefix, vColumns.1, vColumns.2, nThreshold) {
  
  nItems.1 <- length(vColumns.1)
  nItems.2 <- length(vColumns.2)
  
  # Dynamic column names:
  Count_Col.1 <- paste0(sPrefix, "_Items_Count.1")
  Count_Col.2 <- paste0(sPrefix, "_Items_Count.2")
  Score_Col.1 <- paste0(sPrefix, "_Score_Adj.1")
  Score_Col.2 <- paste0(sPrefix, "_Score_Adj.2")
  CFB_Col     <- paste0("CFB_", sPrefix, "_Score")
  
  tData %>%
    mutate(
      
      # Count of completed items per participant:
      !!Count_Col.1 := rowSums(!is.na(across(all_of(vColumns.1)))),
      !!Count_Col.2 := rowSums(!is.na(across(all_of(vColumns.2)))),
      
      
      # Adjusted score (pro-rated) for time point 1:
      !!Score_Col.1 := ifelse(
        .data[[Count_Col.1]] >= ceiling(nItems.1 * (1 - nThreshold)),
        rowSums(across(all_of(vColumns.1)), na.rm = TRUE) * (nItems.1 / .data[[Count_Col.1]]),
        NA_real_
      ),
      
      # Adjusted score for time point 2:
      !!Score_Col.2 := ifelse(
        .data[[Count_Col.2]] >= ceiling(nItems.2 * (1 - nThreshold)),
        rowSums(across(all_of(vColumns.2)), na.rm = TRUE) * (nItems.2 / .data[[Count_Col.2]]),
        NA_real_
      ),
      
      
      # Calculate change from baseline for both outcomes:
      !!CFB_Col :=  ifelse(
        !is.na(.data[[Score_Col.1]]) & !is.na(.data[[Score_Col.2]]),
        .data[[Score_Col.2]] - .data[[Score_Col.1]],
        NA_real_
      )
    )
}

# Barchart of questionnaire responses per question:
fGraphs <- function(tData, vCols) {
  
  # Max count across all questionnaire for axis definement:
  nMaxCount <- 0
  for (var in vCols) {
    nCounts <- table(tData[[var]])
    nMaxCount <- max(nMax_Count, max(nCounts, na.rm = TRUE))
  }
  
  # Create the plots & save
  for (var in vCols) {
    
    plot <- ggplot(tData, aes_string(x = var)) +
      geom_bar(fill= "#0055B7", colour = "black", na.rm = TRUE) + 
      labs(
        title = paste("Distribution of Responses for", var),
        x = "Response",
        y = "Count") + 
      ylim(0, nMaxCount)
    
    ggsave(
      filename = paste("Graphs/", var, ".png"),
      plot = plot,
      width = 6,
      height = 4,
      dpi = 300
    )
  }
}

######### Sample Size Calculation #########
# From Protocol: "We expect that approx. 35% of participants will experience clinically
#                 meaningful symptoms of anxiety.
#                 By extension, assuming that only 35% of participants in the treatmnet
#                 group will be affected by the intervention, and the remainder will not,
#                 a sample size of 80 participants per group is needed in order to 
#                 obtain 80% or greater power to detect an effect size of 0.45 between
#                 the treatment and control groups.
#                 Assuming a 15% attrition rate, we will aim to recruit 94 participants
#                 per group to ensure a large enough sample to detect significant group
#                 differences, for a total sample size of 282 participants.

# two-sample proportions:
nEffectSize <- 0.45
nPower <- 0.80
nSignif <- 0.05
nAttr <- 0.15
nGroups <- 3

# Sample size per group, rounded up to nearest multiple of 10:
nSS_Per_Group <- ceiling(pwr.t.test(d = nEffectSize, power = nPower, sig.level = nSignif,
                                    type = "two.sample", alternative = "two.sided")$n / 10) * 10
# Adjust for attrition:
nAdj_SS_Per_Group <- round(nSS_Per_Group / (1 - nAttr))

# Total sample size:
nSample_Size <- nAdj_SS_Per_Group * nGroups

nSample_Size #282


## Now updated for effect size only relating to responders:
nResponders <- 0.35
nOverallEffectSize <- nResponders * nEffectSize

# Sample size per group, rounded up to nearest multiple of 10:
nSS_Per_Group <- ceiling(pwr.t.test(d = nOverallEffectSize, power = nPower, sig.level = nSignif,
                                    type = "two.sample", alternative = "two.sided")$n / 10) * 10
# Adjust for attrition:
nAdj_SS_Per_Group <- round(nSS_Per_Group / (1 - nAttr))

# Total sample size:
nSample_Size <- nAdj_SS_Per_Group * nGroups

nSample_Size #2259

######### Analysis Data Sets #########
# Keeping only those who consented and did not withdraw:
tAnalysisSet <- tDataRaw %>%
  filter(Consent.1 == 1, Withdrawn.0 ==0)
rm(tDataRaw)

table(tAnalysisSet$Group)

######### Baseline Characteristics #########
#
# Of interest:
# -	Relationship Status [Relationship_Status.1]
# -	Education [Education.1]
# - Income [Income.1]
# -	Study Entry (Pregnant vs. Postpartum) [Study_Entry_PREG_PP.0]
# -	Weeks pregnant at Time of Enrolment [Weeks_Preg_Intake.0]
# -	Prior Births [Pregnancies_Total.1]
# -	Vaginal [Vaginal_Births.1]
# -	Caesarean [Cesarean_Births.1]
# -	Prior Pregnancy Loss <20 weeks [Losses_Early.1]
# -	Prior Pregnancy Loss >20 weeks [Losses_Late.1]


## Relationship Status
tRelationship_Status <- fSummariesCat(tAnalysisSet, "Relationship_Status.1")

## Education
tEducation <- fSummariesCat(tAnalysisSet, "Education.1")

## Preg or PP
tStudy_Entry_PREG_PP <- fSummariesCat(tAnalysisSet, "Study_Entry_PREG_PP.0")

## Prior Births
tPriorBirths <- fSummariesCat(tAnalysisSet, "Pregnancies_Total.1") 

## Vaginal Births
tVaginalBirths <- fSummariesCat(tAnalysisSet, "Vaginal_Births.1")

## Caesarian Births
tCesareanBirths <- fSummariesCat(tAnalysisSet, "Cesarean_Births.1") 

## Early Losses
tEarlyLosses <- fSummariesCat(tAnalysisSet, "Losses_Early.1")

## Late Losses
tLateLosses <- fSummariesCat(tAnalysisSet, "Losses_Late.1")


## Income
tIncome <- tAnalysisSet %>%
  # Only considering necessary rows
  select(Group, Income.1) %>%
  # Collapse categories
  mutate(
    Income_Category = case_when(
      Income.1 == 1                      ~ "Less than $10,000",
      Income.1 >= 2 & Income.1 <= 5   ~ "$10,000–$49,000",
      Income.1 >= 6 & Income.1 <= 15  ~ "$50,000–$149,000",
      Income.1 >= 16 & Income.1 <= 25 ~ "$150,000–$249,000",
      Income.1 >= 26 & Income.1 <= 31 ~ "$250,000 or more",
      Income.1 == 99                     ~ "Prefer not to say",
      TRUE                                  ~ NA_character_
    )
  )
tFreqIncome <- tIncome %>%
  group_by(Group, Income_Category) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Group,
    values_from = Frequency,
    values_fill = 0
  ) %>%
  mutate(Row_Total = rowSums(across(where(is.numeric)))) %>%  # row totals
  bind_rows(
    summarise(., across(where(is.numeric), sum)) %>%
      mutate(Income_Category = "Total")
  )
tFreqIncome


## Weeks Pregnant
# We need to decide how to calculate the mean here as we have top- and bottom-coded
# categories
# Estimated mean using midpoints for the censored categories:
tWeeksPregnant <- tAnalysisSet %>%
  select(Group, Weeks_Preg_Intake.0) %>%
  mutate(
    Weeks_Preg_Intake.0 = as.character(Weeks_Preg_Intake.0),
    Weeks_Preg_Numeric = case_when(
      Weeks_Preg_Intake.0 == "27 or less" ~ 24,
      Weeks_Preg_Intake.0 == "42 or more"  ~ 43,
      Weeks_Preg_Intake.0 == "77" ~ NA_real_,  # re-code 77 as missing
      TRUE ~ as.numeric(Weeks_Preg_Intake.0)  # keep actual values
    )
  )

# Mean and SD by group WEIRDLY HIGH - SOMETHING NOT WORKING **note
tFreqWeeksPregnant <- tWeeksPregnant %>%
  group_by(Group) %>%
  summarise(
    Mean_Weeks = mean(Weeks_Preg_Numeric, na.rm = TRUE),
    SD_Weeks = sd(Weeks_Preg_Numeric, na.rm = TRUE),
    N = sum(!is.na(Weeks_Preg_Numeric)),
    .groups = "drop"
  )
tFreqWeeksPregnant



######### Primary Outcomes Variables #########
# Primary Outcomes:
#     - Change from baseline to 6-months postpartum Overall STAI Score
#     - Change from baseline to 6-months postpartum Overall PASS Score
#     - Change from baseline to 6-months postpartum Overall IAD13 Score



# STAI Analysis
#     - 0 participants with missing data at baseline
#     - 1 participant with missing data at follow-up 
#                 ID 1185 did not complete follow-up time point for AD Questionnaire
#         to remove from analysis.
#     - We want to adjust the score pro-rata for participants with <=20% missing questions

# Calculate CFB for STAI Questionnaire:
STAI_Items <- c("Calm", "Secure", "Tense", "Regret", "Ease",
                "Upset", "Misfortune", "Rested", "Anxious", "Comfort",
                "Confident", "Nervous", "Jittery", "Highstrung", "Relaxed",
                "Content", "Worried", "Rattled", "Joyful", "Pleasant")

# Create full list of column names for baseline and follow-up:
vSTAICols.1 <- paste0("STAI_", STAI_Items, ".1")
vSTAICols.2 <- paste0("STAI_", STAI_Items, ".2")
vSTAICols <- c(vSTAICols.1, vSTAICols.2)

tAnalysisSet <- fChangeFromBaseline(tAnalysisSet, "STAI", vSTAICols.1, vSTAICols.2, 0.2)


# PASS Analysis
#     - 0 participants with missing data at baseline
#     - 2 participant with missing data at follow-up 
#                 1185 did not complete follow-up time point for AD Questionnaire
#         to remove from analysis.
#                 1424 did not complete follow-up time point for PASS section of Questionnaire
#     - We want to adjust the score pro-rata for participants with <=20% missing questions

# Calculate CFB for PASS Questionnaire:
# Create full list of column names for baseline and follow-up:
vPASSCols.1 <- paste0("PASS_", 1:31, ".1")
vPASSCols.2 <- paste0("PASS_", 1:31, ".2")
vPASSCols <- c(vPASSCols.1, vPASSCols.2)
tAnalysisSet <- fChangeFromBaseline(tAnalysisSet, "PASS", vPASSCols.1, vPASSCols.2, 0.2)


# AD13 Analysis
#     - 23 participants completed incorrect questionnaire and will be removed from the analysis
#   If >= 2/3 questions were missed from first 4 sub-domains, we don't want to include the domain in the overall score.
#   If participant did not answer PDSR, we don't want to include this in the overall score.
#   If >= 2/5 sub domains were missed, participant should not be included in the analysis.

vIADItems <- c("GAD_Nervous", "GAD_Ctrl", "GAD_Worry",
               "MSPIN_Embarrass", "MSPIN_Avoid", "MSPIN_Fears",
               "PCLC_Repeated", "PCLC_Reminded", "PCLC_Alert",
               "VOCI_Wash", "VOCI_Check", "VOCI_Thought",
               "PDSR_Score")
vIADCols.1 <- c(paste0("IAD_", vIADItems[1:12], ".1"), paste0(vIADItems[13], ".1"))
vIADCols.2 <- c(paste0("IAD_", vIADItems[1:12], ".2"), paste0(vIADItems[13], ".2"))
vIADCols <- c(vIADCols.1, vIADCols.2)

# 23 participants completed original AD13 and need NA CFB:
vAD13Remove <- tAnalysisSet$ID[which(fMissing("AD13_GAD_Anxious.1") == 0)]
length(vAD13Remove)
table(tAnalysisSet[vAD13Remove, "Group"])

# Calculate total score for each subdomain, with value = NA if more than one question missing.
tAnalysisSet <- tAnalysisSet %>%
  rowwise() %>%
  mutate(
    
    # GAD (3 items):
    IAD_GAD_Count.1 = sum(!is.na(c_across(c(IAD_GAD_Nervous.1, IAD_GAD_Ctrl.1, IAD_GAD_Worry.1)))),
    IAD_GAD_Score.1 = ifelse(IAD_GAD_Count.1 >= 2, 
                             sum(c_across(c(IAD_GAD_Nervous.1, IAD_GAD_Ctrl.1, IAD_GAD_Worry.1)), na.rm = TRUE) * 3 / IAD_GAD_Count.1,
                             NA_real_),
    IAD_GAD_Count.2 = sum(!is.na(c_across(c(IAD_GAD_Nervous.2, IAD_GAD_Ctrl.2, IAD_GAD_Worry.2)))),
    IAD_GAD_Score.2 = ifelse(IAD_GAD_Count.2 >= 2, 
                             sum(c_across(c(IAD_GAD_Nervous.2, IAD_GAD_Ctrl.2, IAD_GAD_Worry.2)), na.rm = TRUE) * 3 / IAD_GAD_Count.2,
                             NA_real_),
    
    # Mini SPIN (3 items):
    IAD_MSPIN_Count.1 = sum(!is.na(c_across(c(IAD_MSPIN_Embarrass.1, IAD_MSPIN_Avoid.1, IAD_MSPIN_Fears.1)))),
    IAD_MSPIN_Score.1 = ifelse(IAD_MSPIN_Count.1 >= 2, 
                               sum(c_across(c(IAD_MSPIN_Embarrass.1, IAD_MSPIN_Avoid.1, IAD_MSPIN_Fears.1)), na.rm = TRUE) * 3 / IAD_MSPIN_Count.1,
                               NA_real_),
    IAD_MSPIN_Count.2 = sum(!is.na(c_across(c(IAD_MSPIN_Embarrass.2, IAD_MSPIN_Avoid.2, IAD_MSPIN_Fears.2)))),
    IAD_MSPIN_Score.2 = ifelse(IAD_MSPIN_Count.2 >= 2, 
                               sum(c_across(c(IAD_MSPIN_Embarrass.2, IAD_MSPIN_Avoid.2, IAD_MSPIN_Fears.2)), na.rm = TRUE) * 3 / IAD_MSPIN_Count.2,
                               NA_real_),
    
    # PCL-C (3 items):
    IAD_PCLC_Count.1 = sum(!is.na(c_across(c(IAD_PCLC_Repeated.1, IAD_PCLC_Reminded.1, IAD_PCLC_Alert.1)))),
    IAD_PCLC_Score.1 = ifelse(IAD_PCLC_Count.1 >= 2, 
                              sum(c_across(c(IAD_PCLC_Repeated.1, IAD_PCLC_Reminded.1, IAD_PCLC_Alert.1)), na.rm = TRUE) * 3 / IAD_PCLC_Count.1,
                              NA_real_),
    IAD_PCLC_Count.2 = sum(!is.na(c_across(c(IAD_PCLC_Repeated.2, IAD_PCLC_Reminded.2, IAD_PCLC_Alert.2)))),
    IAD_PCLC_Score.2 = ifelse(IAD_PCLC_Count.2 >= 2, 
                              sum(c_across(c(IAD_PCLC_Repeated.2, IAD_PCLC_Reminded.2, IAD_PCLC_Alert.2)), na.rm = TRUE) * 3 / IAD_PCLC_Count.2,
                              NA_real_),
    
    # VOCI (3 items):
    IAD_VOCI_Count.1 = sum(!is.na(c_across(c(IAD_VOCI_Wash.1, IAD_VOCI_Check.1, IAD_VOCI_Thought.1)))),
    IAD_VOCI_Score.1 = ifelse(IAD_VOCI_Count.1 >= 2, 
                              sum(c_across(c(IAD_VOCI_Wash.1, IAD_VOCI_Check.1, IAD_VOCI_Thought.1)), na.rm = TRUE) * 3 / IAD_VOCI_Count.1,
                              NA_real_),
    IAD_VOCI_Count.2 = sum(!is.na(c_across(c(IAD_VOCI_Wash.2, IAD_VOCI_Check.2, IAD_VOCI_Thought.2)))),
    IAD_VOCI_Score.2 = ifelse(IAD_VOCI_Count.2 >= 2, 
                              sum(c_across(c(IAD_VOCI_Wash.2, IAD_VOCI_Check.2, IAD_VOCI_Thought.2)), na.rm = TRUE) * 3 / IAD_VOCI_Count.2,
                              NA_real_),
    
    # PDSR (1 item):
    IAD_PDSR_Score.1 = ifelse(is.na(PDSR_Score.1), NA_real_, PDSR_Score.1),
    IAD_PDSR_Score.2 = ifelse(is.na(PDSR_Score.2), NA_real_, PDSR_Score.2)
    
  ) %>%
  ungroup()


# Overall score range 0-15 points, with each subdomain contributing 3 points:
IADTotal_Items <- c("GAD", "MSPIN", "PCLC", "VOCI", "PDSR")
lMaxScore <- list(GAD = 9, MSPIN = 12, PCLC = 12, VOCI = 12, PDSR = 1)

tAnalysisSet <- tAnalysisSet %>%
  rowwise() %>%
  mutate(
    
    # Total count of completed subdomains per participants (5 sub-questionnaires):
    IAD_Total_Count.1 = sum(!is.na(c_across(c(IAD_GAD_Score.1, IAD_MSPIN_Score.1, IAD_PCLC_Score.1, IAD_VOCI_Score.1, IAD_PDSR_Score.1)))),
    IAD_Total_Count.2 = sum(!is.na(c_across(c(IAD_GAD_Score.2, IAD_MSPIN_Score.2, IAD_PCLC_Score.2, IAD_VOCI_Score.2, IAD_PDSR_Score.2)))),
    
    # Total IAD13 Score per participant standardised for equal contribution allowing for 1 missing sub-domain,
    # missing where more than 1 sub-domain is missing:
    IAD_Total_Score.1 = ifelse(IAD_Total_Count.1 >= 4, sum((IAD_GAD_Score.1 / lMaxScore$GAD) * 3, 
                                                           (IAD_MSPIN_Score.1 / lMaxScore$MSPIN) * 3,
                                                           (IAD_PCLC_Score.1 / lMaxScore$PCLC) * 3,
                                                           (IAD_VOCI_Score.1 / lMaxScore$VOCI) * 3,
                                                           (IAD_PDSR_Score.1 / lMaxScore$PDSR)* 3,
                                                           na.rm = TRUE) / IAD_Total_Count.1 * 5,
                               NA_real_),
    IAD_Total_Score.2 = ifelse(IAD_Total_Count.2 >= 4, sum((IAD_GAD_Score.2 / lMaxScore$GAD) * 3, 
                                                           (IAD_MSPIN_Score.2 / lMaxScore$MSPIN) * 3,
                                                           (IAD_PCLC_Score.2 / lMaxScore$PCLC) * 3,
                                                           (IAD_VOCI_Score.2 / lMaxScore$VOCI) * 3,
                                                           (IAD_PDSR_Score.2 / lMaxScore$PDSR)* 3,
                                                           na.rm = TRUE) / IAD_Total_Count.2 * 5,
                               NA_real_),
    
    # Calculate change from baseline:
    CFB_IAD_Score =  ifelse(
      !is.na(IAD_Total_Score.1) & !is.na(IAD_Total_Score.2),
      IAD_Total_Score.2 - IAD_Total_Score.1,
      NA_real_
    )
  ) %>%
  ungroup()


tPrimaryMeans <- tAnalysisSet %>%
  group_by(Group) %>%
  summarise(
    Mean_CFB_STAI =  mean(CFB_STAI_Score, na.rm = TRUE),
    Mean_CFB_PASS =  mean(CFB_PASS_Score, na.rm = TRUE),
    Mean_CFB_IAD = mean(CFB_IAD_Score, na.rm = TRUE),
  )


tResults <- tAnalysisSet %>%
  select(
    ID,
    Group,
    STAI_Score_Adj.1,
    STAI_Score_Adj.2,
    CFB_STAI_Score,
    PASS_Score_Adj.1,
    PASS_Score_Adj.2,
    CFB_PASS_Score,
    IAD_Total_Score.1,
    IAD_Total_Score.2,
    CFB_IAD_Score
  )



######### Cut Off Analysis #########
# Only those with baseline higher than cut-off prognosis:
tCutOffSTAI <- tAnalysisSet %>%
  filter(STAI_Score_Adj.1 >= 40)

tCutOffPASS <- tAnalysisSet %>%
  filter(STAI_Score_Adj.1 >= 42)
table(tCutOffPASS$Group)

## To adjust for cut-off scores per category if deemed required.
# tCutOffIAD13 <- tAnalysisSet %>%
# filter(IAD_Total_Score.1 >= )

# Calculate change from baseline only using participants with baseline questionnaire score greater than
#   or equal to respective cut-off
tCutOffSTAI <- fChangeFromBaseline(tCutOffSTAI, "STAI", vSTAICols.1, vSTAICols.2, 0.2)
tCutOffPASS <- fChangeFromBaseline(tCutOffPASS, "PASS", vPASSCols.1, vPASSCols.2, 0.2)

tMeanSTAI <- tCutOffSTAI %>%
  group_by(Group) %>%
  summarise(
    Mean_CFB_STAI =  mean(CFB_STAI_Score, na.rm = TRUE)
  )

tMeanPASS <- tCutOffPASS %>%
  group_by(Group) %>%
  summarise(
    Mean_CFB_PASS =  mean(CFB_PASS_Score, na.rm = TRUE)
  )


######### Primary Outcomes Descriptives #########
# Summary statistics and tables for CFB across the three questionnaires:

fSumStatsGroup(tAnalysisSet, "CFB_STAI_Score")
fSumStatsGroup(tAnalysisSet, "CFB_PASS_Score")
fSumStatsGroup(tAnalysisSet, "CFB_IAD_Score")


## STAI Plots
ggplot(tAnalysisSet, aes(x = factor(Group), y = CFB_STAI_Score, fill = factor(Group))) +
  geom_boxplot(alpha = 0.7) + #, outlier.shape = NA) +  # hide outliers if they clutter
  labs(
    title = "Box Plot of CFB in STAI Score by Group",
    x = "Group",
    y = "Change from Baseline (STAI)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon"))


ggplot(tAnalysisSet, aes(x = factor(Group), y = CFB_STAI_Score, fill = factor(Group))) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
  labs(title = "Violin Plot of CFB in STAI Score Across Groups",
       x = "Group",
       y = "Change from Baseline (STAI)") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon"))


## PASS Plots
ggplot(tAnalysisSet, aes(x = factor(Group), y = CFB_PASS_Score, fill = factor(Group))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # hide outliers if they clutter
  labs(
    title = "Box Plot of CFB in PASS Score by Group",
    x = "Group",
    y = "Change from Baseline (PASS)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon"))


ggplot(tAnalysisSet, aes(x = factor(Group), y = CFB_PASS_Score, fill = factor(Group))) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
  labs(title = "Violin Plot of CFB in PASS Score Across Groups",
       x = "Group",
       y = "Change from Baseline (PASS)") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon"))


## IAD13 Plots
ggplot(tAnalysisSet, aes(x = factor(Group), y = CFB_IAD_Score, fill = factor(Group))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # hide outliers if they clutter
  geom_jitter(width = 0.15, alpha = 0.4, color = "black") +  # add individual points
  labs(
    title = "Box Plot of CFB in IAD13 Score by Group",
    x = "Group",
    y = "Change from Baseline (IAD13)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon"))


ggplot(tAnalysisSet, aes(x = factor(Group), y = CFB_IAD_Score, fill = factor(Group))) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
  labs(title = "Violin Plot of CFB in IAD13 Score Across Groups",
       x = "Group",
       y = "Change from Baseline (IAD13)") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "salmon"))


######### Primary Analysis #########

cor(tAnalysisSet$STAI_Score_Adj.1, tAnalysisSet$STAI_Score_Adj.2, use = "complete.obs")
cor(tAnalysisSet$PASS_Score_Adj.1, tAnalysisSet$PASS_Score_Adj.2, use = "complete.obs")
cor(tAnalysisSet$IAD_Total_Score.1, tAnalysisSet$IAD_Total_Score.2, use = "complete.obs")


tAnalysisSet$Group <- factor(tAnalysisSet$Group)

# STAI
mSTAI <- lm(CFB_STAI_Score ~ STAI_Score_Adj.1 + Group, tAnalysisSet)
summary(mSTAI)

crPlots(mSTAI)

plot(mSTAI$fitted.values, mSTAI$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


# Histogram
hist(mSTAI$residuals, breaks = 20, main = "Histogram of residuals")

# Q-Q plot
qqnorm(mSTAI$residuals)
qqline(mSTAI$residuals, col = "red")
dwtest(mSTAI)

vif(mSTAI)


# Example: visualize predictions by group
dSTAI <- tAnalysisSet[!is.na(tAnalysisSet$CFB_STAI_Score), ]
dSTAI$predicted <- predict(mSTAI)

ggplot(dSTAI, aes(x = Group, y = predicted)) +
  geom_boxplot() +
  labs(title = "Predicted Change in STAI by Group",
       y = "Predicted Change", x = "Group")
par(mfrow = c(2, 2))
plot(mSTAI)



# PASS
mPASS <- lm(CFB_PASS_Score ~ PASS_Score_Adj.1 + Group, tAnalysisSet)
summary(mPASS)

crPlots(mPASS)

plot(mPASS$fitted.values, mPASS$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


# Histogram
hist(mPASS$residuals, breaks = 20, main = "Histogram of residuals")

# Q-Q plot
qqnorm(mPASS$residuals)
qqline(mPASS$residuals, col = "red")
dwtest(mPASS)

vif(mPASS)


# Example: visualize predictions by group
dPASS <- tAnalysisSet[!is.na(tAnalysisSet$CFB_PASS_Score), ]
dPASS$predicted <- predict(mPASS)

ggplot(dPASS, aes(x = Group, y = predicted)) +
  geom_boxplot() +
  labs(title = "Predicted Change in PASS by Group",
       y = "Predicted Change", x = "Group")
par(mfrow = c(2, 2))
plot(mPASS)


# IAD
mIAD <- lm(CFB_IAD_Score ~ IAD_Total_Score.1 + Group, tAnalysisSet)
summary(mIAD)

crPlots(mIAD)

plot(mIAD$fitted.values, mIAD$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


# Histogram
hist(mIAD$residuals, breaks = 20, main = "Histogram of residuals")

# Q-Q plot
qqnorm(mIAD$residuals)
qqline(mIAD$residuals, col = "red")
dwtest(mIAD)

vif(mIAD)


# Example: visualize predictions by group
dIAD <- tAnalysisSet[!is.na(tAnalysisSet$CFB_IAD_Score), ]
dIAD$predicted <- predict(mIAD)

ggplot(dIAD, aes(x = Group, y = predicted)) +
  geom_boxplot() +
  labs(title = "Predicted Change in IAD by Group",
       y = "Predicted Change", x = "Group")
par(mfrow = c(2, 2))
plot(mIAD)


## Assumptions

# Linearity of covariate to outcome:
pSTAIChange <- ggplot(tAnalysisSet, aes(x = STAI_Score_Adj.1, y = CFB_STAI_Score, colour = Group)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
pSTAI <- ggplot(tAnalysisSet, aes(x = STAI_Score_Adj.1, y = STAI_Score_Adj.2, colour = Group)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

pPASSChange <- ggplot(tAnalysisSet, aes(x = PASS_Score_Adj.1, y = CFB_PASS_Score, colour = Group)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
pPASS <- ggplot(tAnalysisSet, aes(x = PASS_Score_Adj.1, y = PASS_Score_Adj.2, colour = Group)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

pIADChange <- ggplot(tAnalysisSet, aes(x = IAD_Total_Score.1, y = CFB_IAD_Score, colour = Group)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
pIAD <- ggplot(tAnalysisSet, aes(x = IAD_Total_Score.1, y = IAD_Total_Score.2, colour = Group)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)




######### Graphs #########

# Barchart of each question by response:

## STAI:
fGraphs(tAnalysisSet, vSTAICols)

## PASS:
fGraphs(tAnalysisSet, vPASSCols)

## IAD13:
fGraphs(tAnalysisSet, vIADCols)
fGraphs(tAnalysisSet, "IAD_VOCI_Wash.2")


######### Missing Data Investigation #########
# Variables for each questionnaire columns at baseline (.1) and follow-up (.2):
STAI_Items <- c("Calm", "Secure", "Tense", "Regret", "Ease",
                "Upset", "Misfortune", "Rested", "Anxious", "Comfort",
                "Confident", "Nervous", "Jittery", "Highstrung", "Relaxed",
                "Content", "Worried", "Rattled", "Joyful", "Pleasant")

IAD_Items <- c("IAD_GAD_Nervous", "IAD_GAD_Ctrl", "IAD_GAD_Worry",
               "IAD_MSPIN_Embarrass", "IAD_MSPIN_Avoid", "IAD_MSPIN_Fears",
               "IAD_PCLC_Repeated", "IAD_PCLC_Reminded", "IAD_PCLC_Alert",
               "IAD_VOCI_Wash", "IAD_VOCI_Check", "IAD_VOCI_Thought",
               "PDSR_Score")

vSTAICols.1 <- paste0("STAI_", STAI_Items, ".1")
vSTAICols.2 <- paste0("STAI_", STAI_Items, ".2")

vPASSCols.1 <- paste0("PASS_", 1:31, ".1")
vPASSCols.2 <- paste0("PASS_", 1:31, ".2")

vIADCols.1 <- paste0(IAD_Items, ".1")
vIADCols.2 <- paste0(IAD_Items, ".2")

# Investigation of Missingness:
vSTAIMissing.1 <- fMissing(vSTAICols.1); vSTAIMissing.2 <- fMissing(vSTAICols.2)
vPASSMissing.1 <- fMissing(vPASSCols.1); vPASSMissing.2 <- fMissing(vPASSCols.2)
vIADMissing.1 <- fMissing(vIADCols.1); vIADMissing.2 <- fMissing(vIADCols.2)


lQuestionnaires <- list(STAI.1 = vSTAIMissing.1, STAI.2 = vSTAIMissing.2,
                        PASS.1 = vPASSMissing.1, PASS.2 = vPASSMissing.2,
                        IAD.1 = vIADMissing.1, IAD.2 = vIADMissing.2)

lMissingTables <- lapply(lQuestionnaires, fMissingTotals, tData = tAnalysisSet)
names(lMissingTables) <- names(lQuestionnaires)


for (name in names(lMissingTables)) {
  cat("\n====", name, "====\n")
  print(lMissingTables[[name]])
}


# Missing by timepoint overall questionnaire >=80% etc.
tMissingSTAI.1 <- tAnalysisSet[which(is.na(tAnalysisSet$STAI_Score_Adj.1)), ]
table(tMissingSTAI.1$Group)
tMissingSTAI.2 <- tAnalysisSet[which(is.na(tAnalysisSet$STAI_Score_Adj.2)), ]
table(tMissingSTAI.2$Group)
tMissingPASS.1 <- tAnalysisSet[which(is.na(tAnalysisSet$PASS_Score_Adj.1)), ]
table(tMissingPASS.1$Group)
tMissingPASS.2 <- tAnalysisSet[which(is.na(tAnalysisSet$PASS_Score_Adj.2)), ]
table(tMissingPASS.2$Group)
tMissingIAD.1 <- tAnalysisSet[which(is.na(tAnalysisSet$IAD_Total_Score_z.1)), ]
table(tMissingIAD.1$Group)
tMissingIAD.2 <- tAnalysisSet[which(is.na(tAnalysisSet$IAD_Total_Score_z.2)), ]
table(tMissingIAD.2$Group)

tMissingSTAI <- tAnalysisSet[which(is.na(tAnalysisSet$CFB_STAI_Score)), ]
table(tMissingSTAI$Group)
tMissingPASS <- tAnalysisSet[which(is.na(tAnalysisSet$CFB_PASS_Score)), ]
table(tMissingPASS$Group)
tMissingIAD <- tAnalysisSet[which(is.na(tAnalysisSet$CFB_IAD_Score_z)), ]
table(tMissingIAD$Group)

# Missing data very negligible between groups:
#   STAI Analysis: 1 participant without CFB due to complete missing data at follow-up
#   PASS Analysis: 2 participants without CFB due to complete missing data at follow-up
#   AD13: 23 participants removed from analysis due to having completed incorrect questionnaire version
#        **NOTE CHECK** 1 additional participant without CFB due to complete missing data ?

# Number of participants STAI counted as missing at baseline (run after column generation):
sum(is.na(tAnalysisSet$STAI_Score_Adj.1))
sum(is.na(tAnalysisSet$STAI_Score_Adj.2))
sum(is.na(tAnalysisSet$CFB_STAI_Score))
tAnalysisSet$ID[which(is.na(tAnalysisSet$CFB_STAI_Score))]

# Number of participants PASS counted as missing at baseline:
sum(is.na(tAnalysisSet$PASS_Score_Adj.1))
sum(is.na(tAnalysisSet$PASS_Score_Adj.2))
sum(is.na(tAnalysisSet$CFB_PASS_Score))
tAnalysisSet$ID[which(is.na(tAnalysisSet$CFB_PASS_Score))]


## Further Investiagtion:
# IDentification of participants with missing values across a questionnaire:
vSTAIMissingID.1 <- tAnalysisSet$ID[which(vSTAIMissing.1 != 0)]

# Identification of participants with missing values for a specific question:
fMissingID("PASS_3.1")



######### Data Export #########
tPASSRCT <- tAnalysisSet %>%
  select(ID, Group, )