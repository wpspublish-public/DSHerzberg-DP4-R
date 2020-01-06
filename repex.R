suppressMessages(library(tidyverse))

# create data
table_names <- c('dfA', 'dfB', 'dfC')

dfA <- tibble(a = 1:3, b = 4:6, c = 7:9)
dfB <- tibble(a = 10:12, b = 13:15, c = 16:18)
dfC <- tibble(a = 19:21, b = 22:24, c = 25:27)
df_list <- list(dfA, dfB, dfC) %>% setNames(table_names)

# function: this this expresses the required transformations for a single df
# dfA_mod <- df_list$dfA %>% 
#   mutate(name = 'dfA') %>%
#   select(name, everything()) 

# stack overflow answer: use purr::imap() to apply a function to both a list and
# its index (in a named list the index is the names of the elements)
df_out <- imap(df_list, ~.x %>% mutate(name = .y) %>% select(name, everything()))

# here's how to give the modified dfs new names, and extract them from the list
# using base::list2env
names(df_out) <- paste0(names(df_out), "_mod")
list2env(df_out, .GlobalEnv)

# more options from SO, both stack all three dfs with a new `name` column in the
# far left position
map_dfr(df_list, I, .id = 'name')

bind_rows(df_list, .id = 'name')

