test <- scale_CV_lookup %>%
  transmute(
    PHY_CI90_UB = PHY + PHY_CV90,
    PHY_CI90_UB_trunc = case_when(
      PHY_CI90_UB > 160 ~ 160,
      TRUE ~ PHY_CI90_UB
    ) 
    ) 



    
scale_CI_lookup <- scale_acr %>%
  map_dfc(~ scale_CV_lookup %>% 
            # dplyr::transmute() is similar to mutate(), but it drops the input
            # columns after creating the new var
            transmute(
              # Next four operations lines get upper, lower bounds of CIs as numbers
              !!str_c(.x, '_CI90_LB_pre') := !!sym(.x) - !!sym(str_c(.x, '_CV90')),
              !!str_c(.x, '_CI90_UB_pre') := !!sym(.x) + !!sym(str_c(.x, '_CV90')), 
              !!str_c(.x, '_CI95_LB_pre') := !!sym(.x) - !!sym(str_c(.x, '_CV95')),
              !!str_c(.x, '_CI95_UB_pre') := !!sym(.x) + !!sym(str_c(.x, '_CV95')), 
              # Next four operations truncate UB at 160, LB at 40, and coerce
              # both to character
              !!str_c(.x, '_CI90_LB') := as.character(case_when(
                !!sym(str_c(.x, '_CI90_LB_pre')) < 40 ~ 40,
                TRUE ~ !!sym(str_c(.x, '_CI90_LB_pre'))
              )),
              !!str_c(.x, '_CI90_UB') := as.character(case_when(
                !!sym(str_c(.x, '_CI90_UB_pre')) > 160 ~ 160,
                TRUE ~ !!sym(str_c(.x, '_CI90_UB_pre'))
              )),
              !!str_c(.x, '_CI95_LB') := as.character(case_when(
                !!sym(str_c(.x, '_CI95_LB_pre')) < 40 ~ 40,
                TRUE ~ !!sym(str_c(.x, '_CI95_LB_pre'))
              )),
              !!str_c(.x, '_CI95_UB') := as.character(case_when(
                !!sym(str_c(.x, '_CI95_UB_pre')) > 160 ~ 160,
                TRUE ~ !!sym(str_c(.x, '_CI95_UB_pre'))
              )),
              # Next two operations yield the formatted, truncated CIs as strings
              !!str_c(.x, '_CI90') :=
                str_c(!!sym(str_c(.x, '_CI90_LB')), !!sym(str_c(.x, '_CI90_UB')), sep = ' - '),
              !!str_c(.x, '_CI95') :=
                str_c(!!sym(str_c(.x, '_CI95_LB')), !!sym(str_c(.x, '_CI95_UB')), sep = ' - ')
            )
          ) %>%
  # At this point the object has only the new columns; all input columns have
  # been dropped by transmute(). Now bind_cols joins the new cols with the
  # original input set. select() then pares to only those columsn needed in the
  # final OES output.
  bind_cols(scale_CV_lookup, .) %>% 
  select(form:COM, ends_with('CI90'), ends_with('CI95'))
