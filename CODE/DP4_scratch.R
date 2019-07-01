test <- norm_build1 %>% ungroup() %>% 
  mutate(
  dist_point = case_when(
    dist_point == 'lo1' ~ 'A',
    dist_point == 'lo2' ~ 'B',
    dist_point == 'med' ~ 'C',
    dist_point == 'hi1' ~ 'D',
    dist_point == 'hi2' ~ 'E',
    TRUE ~ NA_character_
  )
) #%>% select(agestrat, dist_point, IRS_hi1)

# test <- norm_build1 %>% ungroup() %>% select(agestrat, dist_point, IRS_hi1)

solution <- complete(test, agestrat, dist_point) %>%
  fill(
    TOTALscore:IRS_hi2
    ) %>% 
  mutate(
    dist_point = case_when(
      dist_point == 'A' ~ 'lo1',
      dist_point == 'B' ~ 'lo2',
      dist_point == 'C' ~ 'med',
      dist_point == 'D' ~ 'hi1',
      dist_point == 'E' ~ 'hi2',
      TRUE ~ NA_character_
    )
  ) %>% group_by(agestrat)

rm(norm_build1)
norm_build1 <- solution


# solution <- complete(norm_build1, nesting(agestrat, dist_point)) %>%
#   fill(IRS_hi1) 
# 

norm_build1 <- solution %>% mutate(IRS = case_when(dist_point == 'lo1' ~ IRS_lo1,
                                                       dist_point == 'lo2' ~ IRS_lo2,
                                                       dist_point == 'med' ~ IRS_med,
                                                       dist_point == 'hi1' ~ IRS_hi1,
                                                       dist_point == 'hi2' ~ IRS_hi2,
                                                       TRUE ~ NA_real_),
                                       RSD = case_when(dist_point == 'lo1' ~ lead(lead(IRS))-IRS,
                                                       dist_point == 'lo2' ~ lead(IRS)-IRS,
                                                       dist_point == 'med' ~ IRS,
                                                       dist_point == 'hi1' ~ IRS-lag(IRS),
                                                       dist_point == 'hi2' ~ IRS-lag(lag(IRS)),
                                                       TRUE ~ NA_real_),
                                       lohi_value = case_when(dist_point == 'lo1' ~ lo1,
                                                              dist_point == 'lo2' ~ lo2,
                                                              dist_point == 'hi1' ~ hi1,
                                                              dist_point == 'hi2' ~ hi2,
                                                              TRUE ~ 50)) %>% 
  select(agestrat, lo1, lo2, hi1, hi2, dist_point, lohi_value, IRS, RSD) %>% left_join(perc_z, by = 'lohi_value') %>% 
  mutate(std_RSD = RSD/z_score, SD = case_when(dist_point == 'lo1' | dist_point == 'hi1' ~ (std_RSD+lead(std_RSD))/2,
                                               TRUE ~ NA_real_),
         median = case_when(dist_point == 'med' ~ IRS,
                            TRUE ~ NA_real_),
         lo_SD = case_when(dist_point == 'lo1' ~ SD,
                           TRUE ~ NA_real_),
         hi_SD = case_when(dist_point == 'hi1' ~ SD,
                           TRUE ~ NA_real_))
