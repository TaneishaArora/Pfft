amyloid_wide_month <- amyloid %>%
      mutate(month0 = month == 0,
             month6 = month == 6, 
             month12 = month == 12,
             month18 = month == 18,
             month24 = month == 24,
             month36 = month == 36)

# rids with visits 0, 6, and 12
rid_0to12 <- amyloid_wide_month %>% 
  group_by(rid) %>%
  summarise(visited0612 = any(month0) & any(month6) & any(month12)) %>% 
  select(rid)
     
# time between visits is not uniform...adjust for this