library(tidyverse)

amyloid <- read_csv('./Data/Amyloid.csv')
View(amyloid)

# 1 -> negative
# 2 -> positive

# 1 -> female
# 2 -> male

# Preprocessing
amyloid <- amyloid %>%
            mutate(
                   month = if_else(month < 0, NA_real_, month),
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
                  )

# Colnames
colnames(amyloid)

amyloid %>% group_by(abeta6mcut) %>% summarise(counts = n())


# Summary stats demo stuff... not pro level...YET!!
amyloid %>%
  group_by(abeta6mcut) %>%
  summarise(
    mean_age  = mean(age), 
    mean_edu = mean(edu), 
    mean_test1 = mean(t1sum),
    mean_test2 = mean(t2sum), 
    mean_test3 = mean(t3sum),
    mean_test4 = mean(t4sum),
    mean_test5 = mean(t5sum),
    mean_test6 = mean(t6sum),
    mean_test7 = mean(t7sum,  na.rm = TRUE),
    drec_hits = mean(drec_hits),
    drec_fa = mean(drec_fa, na.rm = TRUE),
    )

na_t7 <- amyloid %>%
  filter(is.na(t7sum))
