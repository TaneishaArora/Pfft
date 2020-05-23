# CORRELATION BETWEEN VARIABLES
library(tidyverse)
library(ggplot2)
library(reshape2)


# Generate heatmap from provided correlation matrix
generate_corr_map <- function(corrmat){
  corrmat[lower.tri(corrmat)] <- NA
  melted_cormat <- melt(corrmat, na.rm = TRUE)
  
  heat_map <- 
    ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
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
six <- amyloid %>% filter(month == 6)
twelve <- amyloid %>% filter(month == 12)
eighteen <- amyloid %>% filter(month == 18)
twenty_four <- amyloid %>% filter(month == 24)
thirty_six <- amyloid %>% filter(month == 36)

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

baseline_positive_numeric <- baseline %>% 
  filter(abeta6mcut == 2) %>%
  select(
    numeric_features
  )

baseline_negative_numeric <- baseline %>% 
  filter(abeta6mcut == 1) %>%
  select(
    numeric_features
  )

baseline_corr <- round(cor(baseline_numeric, use = "complete.obs"), 2)

baseline_am_positive_corr <- round(cor(baseline_positive_numeric, use = "complete.obs"), 2)

baseline_am_negative_corr <- round(cor(baseline_negative_numeric, use = "complete.obs"), 2)


generate_corr_map(baseline_corr)
generate_corr_map(baseline_am_positive_corr)
generate_corr_map(baseline_am_negative_corr)

# Significant features

# might want to create a 2 category feature based on the most important genotype (has it or doesnt)
# same with dx
# also maybe age
test_dx <- lm(abeta6m ~ 1 + genotype + t1t5 + sex + dx, data = baseline)
test_d <- lm(abeta6m ~ 1 + t1t5 + genotype + dx, data = baseline)

model <- glm(abeta6mcut ~ 1 + t1t5 + sex + dx + genotype + age, data = baseline, family = binomial, na.action = na.omit)

