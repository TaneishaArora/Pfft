library(tidyverse)

amyloid <- read_csv('./Data/Amyloid.csv')

# Summary of Pariticipant Counts by Month
participant_counts <- table(amyloid$month) 

# Summary Statistics for Demographics for Month 0
amyloid_demographic_baseline_summary <- amyloid %>%
  filter(month == 0) %>%
  summarise(
    mean_age  = mean(age), 
    mean_edu = mean(edu), 
    prop_male = sum(sex == 2)/n(),
    prop_female = sum(sex == 1)/n(),
  )

# Summary Statisitics for Biomarkers
amyloid_biomarker_baseline_summary <- amyloid %>%
  filter(month == 0) %>%
  summarise(
    mean_abeta6_measure = mean(abeta6m, na.rm),
    prop_positive = sum(abeta6mcut == 1)/n(),
    prop_negative = sum(abeta6mcut == 2)/n(),
    prop_e2e2 = sum(genotype == 1)/n(),
    prop_e2e3 = sum(genotype == 2)/n(),
    prop_e3e3 = sum(genotype == 3)/n(),
    prop_e2e4 = sum(genotype == 4)/n(),
    prop_e3e4 = sum(genotype == 5)/n(),
    prop_e4e4 = sum(genotype == 6)/n()
  )

# Summary Statistics for Diagnoses
amyloid_diagnosis_baseline_summary <- amyloid %>%
  filter(month == 0) %>%
  summarise(
    prop_normal_cognition = sum(dx == 1)/n(),
    prop_subjectively_impaired = sum(dx == 2)/n(),
    prop_objectively_impaired = sum(dx == 3)/n(),
  )

# Summary Statistics for AVLT Test Scores for Month 0, 
# split by Amyloid Positivity
amyloid_test_performance_baseline_summary <- amyloid %>%
  filter(month == 0)  %>%
  group_by(abeta6mcut) %>% 
  summarise(
    mean_t1sum = mean(t1sum, na.rm = TRUE),
    mean_t2sum = mean(t2sum, na.rm = TRUE),
    mean_t3sum = mean(t3sum, na.rm = TRUE),
    mean_t4sum = mean(t4sum, na.rm = TRUE),
    mean_t5sum = mean(t5sum, na.rm = TRUE),
    mean_t6sum = mean(t6sum, na.rm = TRUE),
    mean_t7sum = mean(t7sum, na.rm = TRUE),
    drec_hits = mean(drec_hits, na.rm = TRUE),
    drec_fa = mean(drec_fa, na.rm = TRUE),
  )

# Preprocessing
amyloid <- amyloid %>%
  mutate(
    month = as.factor(if_else(month < 0, NA_real_, month)),
    abeta6m = if_else(abeta6m < 0, NA_real_, abeta6m),
    abeta6mcut = factor(abeta6mcut),
    sex = factor(sex),
    genotype = factor(genotype),
    dx = factor(dx),
    edu = if_else(edu < 0, NA_real_, edu),
    wordlist = factor(wordlist),
    t1sum = if_else(t1sum < 0, NA_real_, t1sum),
    t2sum = if_else(t2sum < 0, NA_real_, t2sum),
    t3sum = if_else(t3sum < 0, NA_real_, t3sum),
    t4sum = if_else(t4sum < 0, NA_real_, t4sum),
    t5sum = if_else(t5sum < 0, NA_real_, t5sum),
    t6sum = if_else(t6sum < 0, NA_real_, t6sum),
    t7sum = if_else(t7sum < 0, NA_real_, t7sum),
    distsum = if_else(distsum < 0, NA_real_, distsum),
    drec_hits = if_else(drec_hits < 0, NA_real_, drec_hits),
    drec_fa = if_else(drec_fa < 0, NA_real_, drec_fa),
    t1t2 = t2sum - t1sum,
    t2t3 = t3sum - t2sum,
    t3t4 = t4sum - t3sum,
    t4t5 = t5sum - t4sum,
    t5t6 = t6sum - t5sum,
    t6t7 = t7sum - t6sum,
    t1t5 = t5sum - t1sum,
    t5t7 = t7sum - t5sum,
    recognition_prop = drec_hits/drec_fa
  )

