# Uncomment to install packages necessary for multinomial
# computation trees

# install.packages("psychotree")

# Load installed package
library(psychotree)


amyloid_mpt_data <- amyloid %>% 
  filter(month == 0) %>%
  filter(!is.na(drec_hits) | !is.na(drec_fa)) %>%
  select(sex, age, t1t5, t5t6, t5t7, dx, genotype)

# Creating count matrix of true positives, false positives, true negatives, and false negatives
# on the recognition-memory experiments
y <- as.matrix(
  cbind(
    old_old = amyloid_mpt_data$drec_hits, 
    old_new = 15-amyloid_mpt_data$drec_hits, 
    new_old = amyloid_mpt_data$drec_fa, 
    new_new = 15-amyloid_mpt_data$drec_fa
    )
  )

# Various MPT models
# (underlying assumption is that this tree structure represents the cognitive process
# of the recognition task across all participants in the study)

# Using the 1 High Threshold tree structure and drec_hits and drec_fa counts
# learning (T5 - T1 scores)
amyloid_tree_learning <- mpttree(y ~ t1t5, data = amyloid_mpt_data, spec = mptspec("1HT"))
plot(amyloid_tree_learning, index = c("r", "b"))


# age
amyloid_tree_age <- mpttree(y ~ age, data = amyloid_mpt_data, spec = mptspec("1HT"))
plot(amyloid_tree_age, index = c("r", "b"))


# interference effect (T6 - T5)
amyloid_tree_interference <- mpttree(y ~ t5t6, data = amyloid_mpt_data, spec = mptspec("1HT"))
plot(amyloid_tree_interference, index = c("r", "b"))


# sex
amyloid_tree_sex <- mpttree(y ~ sex, data = amyloid_mpt_data, spec = mptspec("1HT"))
plot(amyloid_tree_sex, index = c("r", "b"))


# dx
amyloid_tree_dx <- mpttree(y ~ dx, data = amyloid_mpt_data, spec = mptspec("1HT"))
plot(amyloid_tree_dx, index = c("r", "b"))


# genotype
amyloid_tree_genotype <- mpttree(y ~ genotype, data = amyloid_mpt_data, spec = mptspec("1HT"))
plot(amyloid_tree_genotype, index = c("r", "b"))

# full model
amyloid_tree_all <- mpttree(y ~ ., data = amyloid_mpt_data, spec = mptspec("1HT"))
plot(amyloid_tree_all, index = c("r", "b"))


