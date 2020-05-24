library(aod)
library(ggplot2)
library(smbinning)
library(tidyverse)
library(InformationValue)
library(caret)

set.seed(100)  # for repeatability of samples

# CORRELATION ######################################################################################

# Generate heatmap from provided correlation matrix
generate_corr_map <- function(corrmat){
  corrmat[lower.tri(corrmat)] <- NA
  melted_cormat <- melt(corrmat, na.rm = TRUE)
  
  heat_map <- 
    ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Person\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  
  heat_map + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
}

# Dividing the data up by visit month
baseline <- amyloid %>% filter(month == 0)

# Numeric Features
numeric_features <- c('abeta6m',
                      'edu',
                      't1sum',
                      't2sum',
                      't3sum',
                      't4sum',
                      't5sum',
                      't6sum',
                      't7sum',
                      'distsum',
                      'drec_hits',
                      'drec_fa',
                      't1t2',
                      't2t3',
                      't3t4',
                      't4t5',
                      't5t6',
                      't6t7',
                      't1t5',
                      't5t7',
                      'drec_fa',
                      'drec_hits')

# Correlation between all numeric features
baseline_numeric <- baseline %>% 
  select(
    numeric_features
  )

baseline_corr <- round(cor(baseline_numeric, use = "complete.obs"), 2)

generate_corr_map(baseline_corr)

# LOGISTIC REGRESSION ##############################################################################

# Split data by Amyloid Positivity
logit_amyloid <- baseline %>% 
  mutate(abeta6mcut = as.factor(if_else(abeta6mcut == 1, 0, 1)))

abeta_neg <- logit_amyloid %>% filter(abeta6mcut == 0)
abeta_pos <- logit_amyloid %>% filter(abeta6mcut == 1)

# Create Test-Train Split
abeta_neg_training_rows <- sample(1:nrow(abeta_neg), 0.7*nrow(abeta_neg))  # neg for train pos
abeta_pos_training_rows <- sample(1:nrow(abeta_pos), 0.7*nrow(abeta_neg))  # pos for training. Pick as many pos as neg
training_pos <- abeta_pos[abeta_pos_training_rows, ]  
training_neg <- abeta_neg[abeta_neg_training_rows, ]
trainingData <- rbind(training_neg, training_pos)  # row bind the negitives and positives 

test_pos <- abeta_pos[-abeta_pos_training_rows, ]
test_neg <- abeta_neg[-abeta_neg_training_rows, ]
testData <- rbind(test_neg, test_pos)  # row bind the neg and pos

# Sanity Check
table(trainingData$abeta6mcut) # equal number of entries
table(testData$abeta6mcut) # the rest of the entries

# Build logistic regression models

# Test
logit_baseline <- glm(abeta6mcut ~ t1t5, data=trainingData, family=binomial(link="logit"))
predicted_baseline <- plogis(predict(logit_baseline, testData))  # predicted scores
optCutOff_baseline <- optimalCutoff(testData$abeta6mcut, predicted)[1]

# Learning, Genotype, Diagnosis
logit_full <- glm(abeta6mcut ~ t1t5 + genotype + dx, data=trainingData, family=binomial(link="logit"))
predicted_full <- plogis(predict(logit_full, testData))  # predicted scores
optCutOff_full <- optimalCutoff(testData$abeta6mcut, predicted)[1]

# model diagnostics
summary(logit_baseline)
summary(logit_full)

# Misclassification error: lower the better
misClassError(testData$abeta6mcut, predicted_baseline, threshold = optCutOff_baseline)
misClassError(testData$abeta6mcut, predicted_full, threshold = optCutOff_full)

# ROC: higher the better
plotROC(testData$abeta6mcut, predicted_baseline)
plotROC(testData$abeta6mcut, predicted_full)

# true positive rate
InformationValue::sensitivity(testData$abeta6mcut, predicted_baseline, threshold = optCutOff_baseline)
InformationValue::sensitivity(testData$abeta6mcut, predicted_full, threshold = optCutOff_full)

# false positive rate
InformationValue::specificity(testData$abeta6mcut,  predicted_baseline, threshold = optCutOff_baseline)
InformationValue::specificity(testData$abeta6mcut,  predicted_full, threshold = optCutOff_full)

# confusion matrix
# The columns are actuals, while rows are predicteds.
InformationValue::confusionMatrix(testData$abeta6mcut, predicted_baseline, threshold = optCutOff_baseline)
InformationValue::confusionMatrix(testData$abeta6mcut, predicted_full, threshold = optCutOff_full)