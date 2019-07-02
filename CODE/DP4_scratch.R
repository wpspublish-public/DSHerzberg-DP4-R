# truncate possible values of median_sm at min_raw, max_raw.
mutate_at(
  vars(median_sm), ~ case_when(
    .x < min_raw ~ min_raw,
    .x > max_raw ~ max_raw,
    TRUE ~ .x
  )
) %>% 
# Ensure that median_sm is never less than its lagging value, or greater than
# its leading value. This must be done in separate tests, to ensure that the
# corrections don't cancel each other out, or reverse each other.
mutate_at(
  vars(median_sm), ~ case_when(
    .x < lag(.x) ~ lag(.x),
    TRUE ~ .x
  )
) %>% 
mutate_at(
  vars(median_sm), ~ case_when(
    .x > lead(.x) ~ lead(.x),
    TRUE ~ .x
  )
)
