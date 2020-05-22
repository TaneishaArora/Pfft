library(ggplot2)

colnames(amyloid)

## ===============================================================
## WORD LISTS VS TEST SCORES======================================
## ===============================================================
# get the average test scores per iteration 
# for both word list groups
mean_scores_word_list <- amyloid %>%
  group_by(wordlist) %>%
  summarise(
    t1_mean = mean(t1sum),
    t2_mean = mean(t2sum),
    t3_mean = mean(t3sum),
    t4_mean = mean(t4sum),
    t5_mean = mean(t5sum),
    t6_mean = mean(t6sum),
    t7_mean = mean(t7sum, na.rm = TRUE),
  )

# transform data
avg_scores <- as.data.frame(t(mean_scores_word_list))

# remove the word list row (first row)
avg_scores = avg_scores[-1,]

# combine both scores columns into one
avg_scores <- data.frame(score = c(avg_scores[,"V1"], 
                                   avg_scores[,"V2"]))

# populate test iteration and word list accordingly
avg_scores <- avg_scores %>% mutate(
  test_iteration = if_else(row_number() %% 7 == 0, 7, 
                           row_number() %% 7),
  word_list = if_else(row_number() < 8, 1, 2),
)

# convert to numeric
avg_scores$test_iteration <- as.numeric(as.character(avg_scores$test_iteration)) 

# plot
ggplot(data=avg_scores, 
       aes(x=test_iteration, 
           y=score, 
           group=word_list, 
           color=as.factor(word_list))) +
  geom_line() + geom_point() +
  ggtitle("Average Test Scores vs AVLT Iteration") +
  labs(y="Score", col="Word List", x="AVLT")
  

## ===============================================================
## WORD LISTS VS CHANGE IN TEST SCORES============================
## ===============================================================
# get the average test scores per change in iteration 
# for both word list groups
change_scores_word_list <- amyloid %>%
  group_by(wordlist) %>%
  summarise(
    t1t2_mean = mean(t1t2, na.rm = TRUE),
    t2t3_mean = mean(t2t3, na.rm = TRUE),
    t3t4_mean = mean(t3t4, na.rm = TRUE),
    t4t5_mean = mean(t4t5, na.rm = TRUE),
    t5t6_mean = mean(t5t6, na.rm = TRUE),
    t6t7_mean = mean(t6t7, na.rm = TRUE),
  )

# transform data
change_scores <- as.data.frame(t(change_scores_word_list))

# remove the word list row (first row)
change_scores = change_scores[-1,]

# combine both scores columns into one
change_scores <- data.frame(score = c(change_scores[,"V1"], 
                                      change_scores[,"V2"]))

# populate test iteration and word list accordingly
change_scores <- change_scores %>% mutate(
  test_iteration = if_else(row_number() %% 6 == 0, 6.5, 
                           (row_number() %% 6)+.5),
  word_list = if_else(row_number() < 7, 1, 2),
)

# plot
ggplot(data=change_scores, 
       aes(x=test_iteration, 
           y=score, 
           group=word_list, 
           color=as.factor(word_list))) +
  geom_line() + geom_point() +
  ggtitle("Avg Change in Test Scores vs AVLT Interval ") +
  labs(x="AVLT Interval", y="Score", 
       col="Word List") 

## ===============================================================
## PROPORTION RIGHT/WRONG FOR CIRCLING TEST ======================
## ===============================================================
# The proportion of right to wrong for the circling test
# group by amyloid positivy
# display proportion drec_hits and drec_fa per group

# get average values of drec hits and fa
proportion_correct <- amyloid %>%
  group_by(abeta6mcut) %>%
  summarise(
    drec_hits_mean = mean(drec_hits, na.rm = TRUE),
    drec_fa_mean = mean(drec_fa, na.rm = TRUE),
  )

# transform data
proportion_correct <- as.data.frame(t(proportion_correct))

# remove the abeta6mcut row (first row)
proportion_correct = proportion_correct[-1,]

# combine both scores columns into one
proportion_correct <- data.frame(Score = c(proportion_correct[,"V1"], 
                                           proportion_correct[,"V2"]))

# populate drec and amyloid positivity accordingly
proportion_correct <- proportion_correct %>% mutate(
  drec = if_else(row_number() %% 2 == 0, "False Alarm", "Hit"),
  amyloid_positivity = if_else(row_number() < 3, "Negative", "Positive"),
)

# convert scores from char to numeric
proportion_correct$Score <- as.numeric(as.character(proportion_correct$Score)) 
sapply(proportion_correct, class)

# plot
ggplot(proportion_correct, aes(fill=drec, y=Score, x=amyloid_positivity)) +
  geom_bar(position="fill", stat="identity") +
  ggtitle("Word Recognition Hits/False Alarms") +
  labs(x="Amyloid Positivity", y="Relative Score",
       col="Recognized Words")