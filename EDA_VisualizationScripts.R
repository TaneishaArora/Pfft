library(tidyverse)
library(ggplot2)

# Test score Progression at baseline, by different demographic traits
# Sex
baseline %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = sex)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Sex", labels = c("Female", "Male")) +
  labs(x = "AVLT", y = "Score") + 
  ggtitle("AVLT Scores by Sex")

# Amyloid Positivity
baseline %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = abeta6mcut)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Amyloid Positivity", labels = c("Negative", "Positive")) +
  labs(x = "AVLT", y = "Score") +
  ggtitle("AVLT Scores by Amyloid Positivity")

# Genotype
baseline %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = genotype)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Genotype", labels = c("e2/e2", "e2/e3", "e3/e3", "e2/e4", "e3/e4", "e4/e4")) +
  labs(x = "AVLT", y = "Score") +
  ggtitle("AVLT Scores by Genotype")

# Diagnosis
baseline %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = dx)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Diagnosis", labels = c("Cognitively Normal", "Subjectively Cognitively Impaired", "Objective Mild Cognitive Impairment")) +
  labs(x = "AVLT", y = "Score") +
  ggtitle("AVLT Scores by Diagnosis")

# Education
ntile(baseline$edu, 3)
# To check ranges
baseline %>% mutate (edu_cat  = ntile(edu, 3)) %>% filter(edu_cat == 1) %>% summarise(min(edu), max(edu))
baseline %>% mutate (edu_cat  = ntile(edu, 3)) %>% filter(edu_cat == 2) %>% summarise(min(edu), max(edu))
baseline %>% mutate (edu_cat  = ntile(edu, 3)) %>% filter(edu_cat == 3) %>% summarise(min(edu), max(edu))

baseline %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>% 
  mutate(edu_factor = as.factor(ntile(edu, 3))) %>%
  ggplot(aes(x = test_number, fill = edu_factor)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Education Bracket", labels = c("<= Bachelors", "<= Masters", "<= PhD")) +
  labs(x = "AVLT", y = "Score") + 
  ggtitle("AVLT Scores by Education Group")

# Age
# To check ranges
baseline %>% mutate (age_cat  = ntile(age, 3)) %>% filter(age_cat == 1) %>% summarise(min(age), max(age))
baseline %>% mutate (age_cat  = ntile(age, 3)) %>% filter(age_cat == 2) %>% summarise(min(age), max(age))
baseline %>% mutate (age_cat  = ntile(age, 3)) %>% filter(age_cat == 3) %>% summarise(min(age), max(age))

baseline %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>% 
  mutate(age_factor = as.factor(ntile(age, 3))) %>%
  ggplot(aes(x = test_number, fill = age_factor)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Age Range", labels = c("[54-72)", "[72-77)", "[77, 89]")) +
  labs(x = "AVLT", y = "Score") +
  ggtitle("AVLT Scores by Age Group")


##################################################################################################

# Amyloid Beta Measures at baseline based on various demographic traits
# Sex
amyloid %>%
  ggplot(aes(x = month, fill = sex)) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Sex", labels = c("Female", "Male")) +
  labs(x = "Month", y = "Amyloid Beta Levels") + 
  ggtitle("Amyloid Beta Measures vs Time, by Sex")

# Genotype
amyloid %>% 
  ggplot(aes(x = month, fill = genotype)) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Genotype", labels = c("e2/e2", "e2/e3", "e3/e3", "e2/e4", "e3/e4", "e4/e4")) +
  labs(x = "Month", y = "Amyloid Beta Levels") + 
  ggtitle("Amyloid Beta Measures vs Time, by Genotype")

# Diagnosis
amyloid %>%
  ggplot(aes(x = month, fill = dx)) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Diagnosis", labels = c("Cognitively Normal", "Subjectively Cognitively Impaired", "Objective Mild Cognitive Impairment")) +
  labs(x = "Month", y = "Amyloid Beta Levels") +
  ggtitle("Amyloid Beta Measures vs Time, by Diagnosis")

# Education
amyloid %>%
  mutate(edu_factor = as.factor(ntile(edu, 3))) %>%
  ggplot(aes(x = month, fill = edu_factor)) +
  geom_boxplot(aes(x = month, y = abeta6m)) +
  scale_fill_discrete(name = "Diagnosis", labels = c("<= Bachelors", "<= Masters", "<= PhD")) +
  labs(x = "Month", y = "Amyloid Beta Levels") +
  ggtitle("Amyloid Beta Measures vs Time, by Education")

# Age
amyloid %>%
  mutate(age_factor = as.factor(ntile(age, 3))) %>%
  ggplot(aes(x = month, fill = age_factor)) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Age", labels = c("[54-72)", "[72-77)", "[77, 89]")) +
  labs(x = "Month", y = "Amyloid Beta Levels") +
  ggtitle("Amyloid Beta Measures vs Time, by Age Group")


##################################################################################################

# Proportion Right/Wrong for Recogition Test

# get average values of drec hits and fa
recognition_prop <- baseline %>%
  group_by(abeta6mcut) %>%
  summarise(
    drec_hits_mean = mean(drec_hits, na.rm = TRUE),
    drec_fa_mean = mean(drec_fa, na.rm = TRUE)
  )

# transform data
recognition_prop <- as.data.frame(t(recognition_prop))

# remove the abeta6mcut row (first row)
recognition_prop = recognition_prop[-1,]

# combine both scores columns into one
recognition_prop <- data.frame(Score = c(recognition_prop[,"V1"], 
                                         recognition_prop[,"V2"]))

# populate drec and amyloid positivity accordingly
recognition_prop <- recognition_prop %>% mutate(
  drec = if_else(row_number() %% 2 == 0, "False Alarm", "Hit"),
  amyloid_positivity = if_else(row_number() < 3, "Negative", "Positive"),
)

# convert scores from char to numeric
recognition_prop$Score <- as.numeric(as.character(recognition_prop$Score)) 
sapply(recognition_prop, class)

# plot
ggplot(recognition_prop, aes(fill=drec, y=Score, x=amyloid_positivity)) +
  geom_bar(position="fill", stat="identity") +
  ggtitle("Word Recognition Hits/False Alarms") +
  labs(x="Amyloid Positivity", y="Relative Score",
       col="Recognized Words")

##################################################################################################

# Delta AVLT Score vs. Amyloid Positivity
baseline %>% gather("test_number", "score", c(t1t2, t2t3, t3t4, t4t5, t5t6, t6t7)) %>%
  ggplot(aes(x = test_number, fill = abeta6mcut)) +
  scale_x_discrete(labels = c("IR1-IR2", "IR2-IR3", "IR3-IR4", "IR4-IR5", "IR5-DR1", "DR1-DR2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Amyloid Positivity", labels = c("Negative", "Positive")) +
  labs(x = "AVLT", y = "Score") + 
  ggtitle("Delta AVLT Scores at Baseline")



