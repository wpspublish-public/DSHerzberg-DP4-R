suppressMessages(library(tidyverse))

Here is data:

```  
col_pre <- c('a', 'b', 'c')
df <- tibble(a1 = 1:3, a2 = 4:6, b1 = 7:9, b2 = 10:12, c1 = 13:15, c2 = 16:18)
```

I want to use `purrr::map()` and `dplyr::mutate()` to create three new columns that are the sums of columns in `df`.
I can use `map()` to iterate over a vector of the a, b, c column prefixes. I figured out the `tidyeval` operations so that 
the code below runs without error.

```
library(tidyverse)
out <- col_pre %>%
  map_df(~ df %>% 
            mutate(!!as.name(paste0(.x, '3')) := !!as.name(paste0(.x, '1')) + !!as.name(paste0(.x, '2')))
  )
```
However, `out` now has six spurious rows:
  
```
     a1    a2    b1    b2    c1    c2    a3    b3    c3
1     1     4     7    10    13    16     5    NA    NA
2     2     5     8    11    14    17     7    NA    NA
3     3     6     9    12    15    18     9    NA    NA
4     1     4     7    10    13    16    NA    17    NA
5     2     5     8    11    14    17    NA    19    NA
6     3     6     9    12    15    18    NA    21    NA
7     1     4     7    10    13    16    NA    NA    29
8     2     5     8    11    14    17    NA    NA    31
9     3     6     9    12    15    18    NA    NA    33
```

What it done is unnecessarily replicate the three rows of the input `df`.

Here is the output I want:
  
```
     a1    a2    b1    b2   c1    c2    a3     b3    c3
1     1     4     7    10    13    16     5    17    29
2     2     5     8    11    14    17     7    19    31
3     3     6     9    12    15    18     9    21    33
```

I have a feeling `purrr::reduce()` could be the solution, but I'm unsure how to apply it'.

Any help is appreciated!
