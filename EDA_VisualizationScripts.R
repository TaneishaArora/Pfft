library(ggplot2)

colnames(amyloid)

# Dividing the data up by visit month
zero <- amyloid %>% filter(month == 0)
six <- amyloid %>% filter(month == 6)
twelve <- amyloid %>% filter(month == 12)
eighteen <- amyloid %>% filter(month == 18)
twenty_four <- amyloid %>% filter(month == 24)
thirty_six <- amyloid %>% filter(month == 36)

# Baseline statistics (all participants are only represented at month 0)
table(amyloid$month)

# Test score Progression at baseline, by different demographic traits
# Sex
zero %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = sex)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Sex", labels = c("Female", "Male")) +
  labs(x = "AVLT", y = "Score")

# Amyloid Positivity
zero %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = abeta6mcut)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Amyloid Positivity", labels = c("Negative", "Positive")) +
  labs(x = "AVLT", y = "Score")

# Genotype
zero %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = genotype)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Genotype", labels = c("e2/e2", "e2/e3", "e3/e3", "e2/e4", "e3/e4", "e4/e4")) +
  labs(x = "AVLT", y = "Score")

# Diagnosis
zero %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>%
  ggplot(aes(x = test_number, fill = dx)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Diagnosis", labels = c("Cognitively Normal", "Subjectively Cognitively Impaired", "Objective Mild Cognitive Impairment")) +
  labs(x = "AVLT", y = "Score")

# Education
ntile(zero$edu, 3)
# To check ranges
zero %>% mutate (edu_cat  = ntile(edu, 3)) %>% filter(edu_cat == 1) %>% summarise(min(edu), max(edu))
zero %>% mutate (edu_cat  = ntile(edu, 3)) %>% filter(edu_cat == 2) %>% summarise(min(edu), max(edu))
zero %>% mutate (edu_cat  = ntile(edu, 3)) %>% filter(edu_cat == 3) %>% summarise(min(edu), max(edu))

zero %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>% 
  mutate(edu_factor = as.factor(ntile(edu, 3))) %>%
  ggplot(aes(x = test_number, fill = edu_factor)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Education Bracket", labels = c("<= Bachelors", "<= Masters", "<= PhD")) +
  labs(x = "AVLT", y = "Score")

# Age
ntile(zero$age, 3)
# To check ranges
zero %>% mutate (age_cat  = ntile(age, 3)) %>% filter(age_cat == 1) %>% summarise(min(age), max(age))
zero %>% mutate (age_cat  = ntile(age, 3)) %>% filter(age_cat == 2) %>% summarise(min(age), max(age))
zero %>% mutate (age_cat  = ntile(age, 3)) %>% filter(age_cat == 3) %>% summarise(min(age), max(age))

zero %>% gather("test_number", "score", c(t1sum, t2sum, t3sum, t4sum, t5sum, t6sum, t7sum)) %>% 
  mutate(age_factor = as.factor(ntile(age, 3))) %>%
  ggplot(aes(x = test_number, fill = age_factor)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= test_number, y = score)) +
  scale_fill_discrete(name = "Age Range", labels = c("[54-72)", "[72-77)", "[77, 89]")) +
  labs(x = "AVLT", y = "Score")


##################################################################################################

# Amyloid Beta Measures at baseline based on various demographic traits
# sex
amyloid %>%
  ggplot(aes(x = month, fill = sex)) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Sex", labels = c("Female", "Male")) +
  labs(x = "Month", y = "Amyloid Beta Levels")

# Genotype
amyloid %>% 
  ggplot(aes(x = month, fill = genotype)) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Genotype", labels = c("e2/e2", "e2/e3", "e3/e3", "e2/e4", "e3/e4", "e4/e4")) +
  labs(x = "AVLT", y = "Amyloid Beta Levels")

# Diagnosis
amyloid %>%
  ggplot(aes(x = month, fill = dx)) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Diagnosis", labels = c("Cognitively Normal", "Subjectively Cognitively Impaired", "Objective Mild Cognitive Impairment")) +
  labs(x = "AVLT", y = "Amyloid Beta Levels")

# Education
amyloid %>%
  mutate(edu_factor = as.factor(ntile(edu, 3))) %>%
  ggplot(aes(x = month, fill = edu_factor)) +
  geom_boxplot(aes(x = month, y = abeta6m)) +
  scale_fill_discrete(name = "Diagnosis", labels = c("<= Bachelors", "<= Masters", "<= PhD")) +
  labs(x = "AVLT", y = "Amyloid Beta Levels")

# Age
amyloid %>%
  mutate(age_factor = as.factor(ntile(age, 3))) %>%
  ggplot(aes(x = month, fill = age_factor)) +
  scale_x_discrete(labels = c("IR 1", "IR 2", "IR 3", "IR 4", "IR 5", "DR 1", "DR 2")) +
  geom_boxplot(aes(x= month, y = abeta6m)) +
  scale_fill_discrete(name = "Age", labels = c("[54-72)", "[72-77)", "[77, 89]")) +
  labs(x = "AVLT", y = "Amyloid Beta Levels")
