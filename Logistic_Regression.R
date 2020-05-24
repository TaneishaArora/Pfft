library(aod)
library(ggplot2)
library(smbinning)
library(InformationValue)

amyloid %>%
  filter(rid %in% rid_0to12$rid) %>%
  ggplot(aes(x = month, fill = abeta6mcut)) +
  geom_boxplot(aes(x= month, y = t1t5)) +
  labs(x = "Month", y = "T5sum - T1sum")

logit_amyloid <- amyloid %>% filter(month == 0)
table(logit_amyloid$abeta6mcut)

# change abeta6m values from (1, 2) to (0, 1)
logit_amyloid$abeta6mcut <- as.character(logit_amyloid$abeta6mcut)
logit_amyloid$abeta6mcut[logit_amyloid$abeta6mcut == "1"] <- "0"
logit_amyloid$abeta6mcut[logit_amyloid$abeta6mcut == "2"] <- "1"
logit_amyloid$abeta6mcut <- as.factor(logit_amyloid$abeta6mcut)

# Create Training Data
abeta_neg <- logit_amyloid[which(logit_amyloid$abeta6mcut == 0), ]  # all negatives
abeta_pos <- logit_amyloid[which(logit_amyloid$abeta6mcut == 1), ]  # all positives
set.seed(100)  # for repeatability of samples

abeta_neg_training_rows <- sample(1:nrow(abeta_neg), 0.7*nrow(abeta_neg))  # neg for training
abeta_pos_training_rows <- sample(1:nrow(abeta_pos), 0.7*nrow(abeta_neg))  # pos for training. Pick as many pos as neg

training_neg <- abeta_neg[abeta_neg_training_rows, ]  
training_pos <- abeta_pos[abeta_pos_training_rows, ]
trainingData <- rbind(training_neg, training_pos)  # row bind the negitives and positives 

# Create Test Data
test_neg <- abeta_neg[-abeta_neg_training_rows, ]
test_pos <- abeta_pos[-abeta_pos_training_rows, ]
testData <- rbind(test_neg, test_pos)  # row bind the neg and pos

table(trainingData$abeta6mcut) # equal number of entries
table(testData$abeta6mcut) # the rest of the entries


# compute information values 
# TODO: research IV
# segregate continuous and factor variables
continuous_vars <- c('sex',
                     'genotype',
                     'dx',
                     'wordlist')
factor_vars<- c('abeta6m',
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

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(26))  # init for IV results

# compute IV (information values) for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="abeta6mcut", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="abeta6mcut", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

# idk whats happening here. 
iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df

# build logi models and predict
logitMod <- glm(abeta6mcut ~ t1t5 + genotype + dx, data=trainingData, family=binomial(link="logit"))
predicted <- plogis(predict(logitMod, testData))  # predicted scores

optCutOff <- optimalCutoff(testData$abeta6mcut, predicted)[1]

# model diagnostics
summary(logitMod)

# misclassification error: lower the better
misClassError(testData$abeta6mcut, predicted, threshold = optCutOff)

# ROC: higher the better
plotROC(testData$abeta6mcut, predicted)

# true positive rate
sensitivity(testData$abeta6mcut, predicted, threshold = optCutOff)

# false positive rate
specificity(testData$abeta6mcut, predicted, threshold = optCutOff)

# confusion matrix
# The columns are actuals, while rows are predicteds.
confusionMatrix(testData$abeta6mcut, predicted, threshold = optCutOff)

