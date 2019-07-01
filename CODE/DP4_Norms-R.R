# Generate norms for test scores with age-related developmental curve.

#$$$$$$$$$$$NOTE: LIBRARY CODE BELOW NOT PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
suppressMessages(library(here)) # BEST WAY TO SPECIFY FILE PATHS
library(reshape2) # RESHAPE DATA FROM WIDE TO TALL
library(broom) # TIDY MODEL OUTPUTS
suppressMessages(library(moderndive)) # USER-FRIENDLY LINEAR MODELING, REGRESSION AND CORRELATION TOOLS.
# note use of `suppressWarnings` to silence chatter during interactive session
library(magrittr) # PIPE OPERATORS
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc)) # EXTENSIONS TO ggplot2: ADD EQUATIONS AND FIT STATISTICS TO FITTED LINE PLOTS
library(ggrepel) # MORE ggplot2 EXTENSIONS

input_parameters_prompt <- function() {
  writeLines(
    "To initiate the norming process, R requires certain input parameters. Please choose a method for entering these parameters:\n\n1: Interactively at the console\n2: From a file named 'Input-Parameters.R' located in '[PROJECT DIRECTORY]/INPUT-FILES'\n"
  )
  input_choice <- as.numeric(0)
  while (is.na(input_choice) || (!(input_choice %in% 1:2))) {
    input_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(input_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (input_choice == 1) {
        cat("Enter the name of the input file, including the .csv suffix.\nUse exact spelling and capitalization.")
        suppressWarnings(as.character(
          readline(prompt = "Input file: ")
        )) %>% assign('input_file_name', ., envir = .GlobalEnv)
        cat("\nEnter the column name of the score to be normed.\nUse exact spelling and capitalization.")
        suppressWarnings(as.character(
          readline(prompt = "Score name: ")
        )) %>% assign('score_name', ., envir = .GlobalEnv)
        cat("\nEnter the HIGHEST possible raw score for the score to be normed.")
        suppressWarnings(as.integer(
          readline(prompt = "Maximum raw score: ")
        )) %>% assign('max_raw', ., envir = .GlobalEnv)
        cat("\nEnter the LOWEST possible raw score for the score to be normed.")
        suppressWarnings(as.integer(
          readline(prompt = "Minimum raw score: ")
        )) %>% assign('min_raw', ., envir = .GlobalEnv)
        break
      } else if (input_choice == 2) {
        source(here('INPUT-FILES/Input-Parameters.R'))
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
input_parameters_prompt()

suppressMessages(
  read_csv(
    here(
      paste0('INPUT-FILES/', input_file_name)
    )
  )
) %>% 
  group_by(agestrat) %>% 
  assign(paste0(score_name, '_raw_by_agestrat'), ., envir = .GlobalEnv)
num_agestrat <- length(unique(eval(as.name(paste0(score_name, '_raw_by_agestrat')))$agestrat))
#$$$$$$$$$$$

eval(as.name(paste0(score_name, '_raw_by_agestrat'))) %>% count(!!as.name(score_name)) %>% 
  mutate(perc = round(100*(n/sum(n)), 4), cum_per = round(100*(cumsum(n)/sum(n)), 4), lag_tot = lag(!!as.name(score_name)), lag_cum_per = lag(cum_per)) %>% 
  assign(paste0(score_name, '_freq_agestrat'), ., envir = .GlobalEnv)

eval(as.name(paste0(score_name, '_raw_by_agestrat'))) %>% arrange(agestrat) %>% summarise(n = n(),
                                                                                          median = round(median(eval(as.name(score_name))), 2),
                                                                                          mean = round(mean(eval(as.name(score_name))), 2),
                                                                                          sd = round(sd(eval(as.name(score_name))), 2)) %>%
  mutate(ES = round((mean - lag(mean))/((sd + lag(sd))/2),2), group = c(1:num_agestrat)) %>% 
  assign(paste0(score_name, '_desc_agestrat'), ., envir = .GlobalEnv)

agestrat <- eval(as.name(paste0(score_name, '_desc_agestrat'))) %>% pull(agestrat)
max_mean <- max(eval(as.name(paste0(score_name, '_desc_agestrat')))$mean)
max_SD <- max(eval(as.name(paste0(score_name, '_desc_agestrat')))$sd)
scale_y_break_options <- seq(0, max_mean+(2*max_SD), 5)
scale_y_ceiling_mean <- scale_y_break_options[which.min(abs(scale_y_break_options - (max_mean+(2*max_SD))))]
scale_y_ceiling_SD <- scale_y_break_options[which.min(abs(scale_y_break_options - (max_SD+(max_SD/2))))]

# Create Z score table
perc_z <- tribble(
  ~lohi_value,	~z_score,
  5, 1.6449,
  10,	1.2816,
  15,	1.0364,
  20,	0.8416,
  25,	0.6745,
  75,	0.6745,
  80,	0.8416,
  85,	1.0364,
  90,	1.2816,
  95,	1.6449
)

# Plot raw score means, SDs; pause execution for user to examine plot.

#$$$$$$$$$$$NOTE: VALUE LABEL CODE BELOW NOT PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
mean_plot <- ggplot(data = eval(as.name(paste0(score_name, '_desc_agestrat'))), aes(group, mean)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = mean), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_mean, 5), limits = c(0, scale_y_ceiling_mean)) +
  labs(title = "Raw Score Means (with SDs)", x = "Agestrat", y = "Total Score") +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(mean_plot)
eval(as.name(paste0(score_name, '_desc_agestrat'))) %>% print(n = nrow(.))
#$$$$$$$$$$$

mean_plot_prompt <- function() {
  writeLines(c("\n", 
               strrep("\u2500", 80),
               "Examine descriptives (above); graph of raw score means, SDs (plot pane on right).",
               "Then press [enter] to continue.",
               strrep("\u2500", 80)
  ))
  readline()
}
mean_plot_prompt ()

# # generate histograms by agestrat
# hist_plot <- ggplot(data = eval(as.name(paste0(score_name, '_raw_by_agestrat'))), aes(eval(as.name(score_name)))) +
#   geom_histogram(
#     binwidth = .2,
#     col = "red"
#   ) +
#   scale_y_continuous(breaks = seq(0, 20, 1)) +
#   labs(title = "Frequency Distribution") +
#   # stat_function(
#   #   fun = function(x, mean, sd, n){
#   #     n * dnorm(x = x, mean = mean, sd = sd)
#   #   },
#   #   args = with(eval(as.name(paste0(score_name, '_raw_by_agestrat'))), c(mean = mean(eval(as.name(score_name))), sd = sd(eval(as.name(score_name))), n
#   #                     = length(eval(as.name(score_name)))))
#   # ) +
#   theme(panel.grid.minor=element_blank()) +
#   facet_wrap(~agestrat)
#   print(hist_plot)
# 
# # plot means, SDs by agestrat
# mean_plot <- ggplot(data = eval(as.name(paste0(score_name, '_desc_agestrat'))), aes(group, mean)) +
#   geom_point(
#     col = "blue",
#     fill = "blue",
#     alpha = .5,
#     size = 3,
#     shape = 23
#   ) +
#   scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
#   scale_y_continuous(breaks = seq(0, scale_y_ceiling_mean, 5), limits = c(0, scale_y_ceiling_mean)) +
#   labs(title = "Means", x = "Agestrat", y = "Mean Total Score")
#   print(mean_plot)
# 
# sd_plot <- ggplot(data = eval(as.name(paste0(score_name, '_desc_agestrat'))), aes(group, sd)) +
#   geom_point(
#     col = "red",
#     fill = "red",
#     alpha = .5,
#     size = 3,
#     shape = 23
#   ) +
#   scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
#   scale_y_continuous(breaks = seq(0, scale_y_ceiling_SD, 1), limits = c(0, scale_y_ceiling_SD)) +
#   labs(title = "SDs", x = "Agestrat", y = "SD of Total Score")
#   print(sd_plot)

# Generate table of lo1, lo2, hi1, hi2 SD adjustment points by agestrat
full_join(
  eval(as.name(paste0(score_name, '_freq_agestrat'))),
  (
    eval(as.name(paste0(score_name, '_freq_agestrat'))) %>%
      group_by(agestrat) %>%
      summarise(min = min(cum_per)) %>%
      mutate(lo1 = case_when(
        min < 5 ~ 5,
        min < 10 ~ 10,
        min < 15 ~ 15,
        min < 20 ~ 20,
        TRUE ~ 25
      ))
  ),
  by = 'agestrat'
) %>%
  group_by(agestrat) %>% mutate(flag = case_when(cum_per > lo1 &
                                                   cum_per < 95 ~ 1,
                                                 TRUE ~ 0)) %>% filter(flag == 1) %>% summarise(min = min(cum_per),
                                                                                                max = max(cum_per),
                                                                                                lo1 = first(lo1)) %>%
  mutate(
    lo2 = case_when(
      lo1 == 5 & min < 10 ~ 10,
      lo1 == 5 & min < 15 ~ 15,
      lo1 == 5 & min < 20 ~ 20,
      lo1 == 5 & min >= 20 ~ 25,
      lo1 == 10 & min < 15 ~ 15,
      lo1 == 10 & min < 20 ~ 20,
      lo1 == 10 & min >= 20 ~ 25,
      lo1 == 15 & min < 20 ~ 20,
      lo1 == 15 & min >= 20 ~ 25,
      TRUE ~ 25
    ),
    hi1 = case_when(max > 90 ~ 90,
                    max > 85 ~ 85,
                    max > 80 ~ 80,
                    TRUE ~ 75),
    hi2 = 95
  ) %>% select(-min,-max) %>% 
  assign(paste0(score_name, '_age_lo1lo2_hi1hi2'), ., envir = .GlobalEnv)

#$$$$$$$$$$$NOTE: norm_perc_prompt CODE BELOW NOT PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
norm_perc_prompt <- function() {
  print(eval(as.name(paste0(score_name, '_age_lo1lo2_hi1hi2'))), 
        n = nrow(eval(as.name(paste0(score_name, '_age_lo1lo2_hi1hi2')))))
  writeLines(
    c(
      strrep("\u2500", 80),
      "For each age group, R has selected four percentile points (lo1, lo2, hi1, hi2)",
      "that will be used to normalize the raw score distribution.\n",
      "Please review the table of percentile points above.",
      "Then press [enter] to continue.",
      strrep("\u2500", 80)
    )
  )
  readline()
}
norm_perc_prompt ()
#$$$$$$$$$$$


# Join hi-lo norm perc values to freq table, add imputed raw scores (IRS) for
# the five distribution points (lo1, lo2, med, hi1, hi2), and flag the rows that
# contain the correct IRS values for each distribution point (note how ties are
# handled <= cum_per). Drop all rows containing missing values, such that
# remaining rows are only those that contain the five IRS for each agestrat.

norm_build1 <-
  eval(as.name(paste0(score_name, '_freq_agestrat'))) %>% 
  left_join(eval(as.name(paste0(score_name, '_age_lo1lo2_hi1hi2'))), by = 'agestrat') %>% 
  mutate (
    IRS_lo1 = ((lo1 - lag_cum_per) / perc) * (eval(as.name(score_name)) - lag_tot) + lag_tot,
    IRS_lo2 = ((lo2 - lag_cum_per) /
                 perc) * (eval(as.name(score_name)) - lag_tot) + lag_tot,
    IRS_med = ((50 - lag_cum_per) /
                 perc) * (eval(as.name(score_name)) - lag_tot) + lag_tot,
    IRS_hi1 = ((hi1 - lag_cum_per) /
                 perc) * (eval(as.name(score_name)) - lag_tot) + lag_tot,
    IRS_hi2 = ((hi2 - lag_cum_per) /
                 perc) * (eval(as.name(score_name)) - lag_tot) + lag_tot,
    dist_point = case_when(
      lo1 <= cum_per & lo1 > lag_cum_per ~ 'lo1',
      lo2 <= cum_per &
        lo2 > lag_cum_per ~ 'lo2',
      50 <= cum_per &
        50 > lag_cum_per ~ 'med',
      hi1 <= cum_per &
        hi1 > lag_cum_per ~ 'hi1',
      hi2 <= cum_per &
        hi2 > lag_cum_per ~ 'hi2',
      TRUE ~ NA_character_
    )
  ) %>% drop_na() 

# Next code deals with situation where upper age ranges are ceilinged out on the
# score being normed, such that the code to this points doesn't generate hi2
# and/or hi1 dist_points or IRS for those upper ranges. Next section imputes new rows
# for hi1, hi2 (in agestrats where they are missing), and copies in nearest
# values for the remaining columns.

df_interim <- norm_build1 %>% ungroup() %>% 
  mutate(
    dist_point = case_when(
      dist_point == 'lo1' ~ 'A',
      dist_point == 'lo2' ~ 'B',
      dist_point == 'med' ~ 'C',
      dist_point == 'hi1' ~ 'D',
      dist_point == 'hi2' ~ 'E',
      TRUE ~ NA_character_
    )
  ) %>% complete(
  agestrat, dist_point
  ) %>%
  fill(
    !!as.name(score_name):IRS_hi2
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

# Now collapse IRS values into a single row, with the correct IRS paired with
# its dist_point label. Use left_join to look up Z-scores for distribution
# points. Drop columns no longer needed.

norm_build1 <- df_interim %>% mutate(
  IRS = case_when(dist_point == 'lo1' ~ IRS_lo1,
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
rm(df_interim)

# Get summary table
norm_build_med_hilo_sum <-
  norm_build1 %>% summarise(
    lo1 = first(lo1),
    lo2 = first(lo2),
    hi1 = first(hi1),
    hi2 = first(hi2),
    median = first(na.omit(median)),
    lo_SD = first(na.omit(lo_SD)),
    hi_SD = first(na.omit(hi_SD))
  ) %>% mutate(ES = (median - lag(median))/((hi_SD+lag(hi_SD)+lo_SD+lag(lo_SD))/4), group = 1:num_agestrat) %>% select(group, everything())

# PROMPT TO EXAMINE PLOT OF IMPUTED MEDIANS VS. RAW SCORE MEANS.
# NOTE: CODE BELOW NOT PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
raw_means <- eval(as.name(paste0(score_name, '_desc_agestrat'))) %>% pull(mean)
norm_build_med_hilo_sum %>% bind_cols(enframe(raw_means)) %>% rename(mean = value) %>% 
  select(group, agestrat, mean, median) %>% melt(id.vars = c("group", "agestrat"), variable.name = "parameter") %>% 
  assign(paste0(score_name, '_desc_agestrat_means'), ., envir = .GlobalEnv)
raw_vs_imputed_plot <-
  ggplot(data = eval(as.name(paste0(score_name, '_desc_agestrat_means'))), aes(group, value, col = parameter, shape = parameter)) +
  geom_point(
    size = 3
  ) +
  scale_colour_manual(values=c("green", "blue")) +   
  scale_shape_manual(values=c(15, 17)) +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_mean, 5), limits = c(0, scale_y_ceiling_mean)) +
  labs(title = "Raw Score Means vs. Imputed Medians", x = "Agestrat", y = "Total Score")
print(raw_vs_imputed_plot)

raw_vs_imputed_plot_prompt <- function() {
  writeLines(c(
    strrep("\u2500", 80),
    "Examine plot of raw score means vs. imputed medians.",
    "Then press [enter] to continue.",
    strrep("\u2500", 80)
  ))
  readline()
}
raw_vs_imputed_plot_prompt ()
###################

# clean up environment
v <- c(paste0(score_name, '_age_lo1lo2_hi1hi2'), paste0(score_name, '_raw_by_agestrat'))
rm(norm_build1, list = v)

# Plot median, lo_SD, hi_SD with regression lines and fit statistics.

# median.

frm <- c('y ~ x', 'y ~ x + I(x^2)', 'y ~ x + I(x^2) + I(x^3)')
title <-
  c('1st order polynomial',
    '2nd order polynomial',
    '3rd order polynomial')
lo_hi <- c('lo_SD', 'hi_SD')

med_plots <- map2(
  title,
  frm,
  ~
    ggplot(norm_build_med_hilo_sum, aes(x = group, y = median)) +
    geom_point(
      col = 'red',
      fill = 'red',
      alpha = .5,
      size = 3,
      shape = 23
    ) +
    labs(
      x = 'age group'
      ,
      y = 'median'
      ,
      title = paste0('Median: ', .x)
    ) +
    scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
    scale_y_continuous(breaks = seq(0, scale_y_ceiling_mean, 5)) +
    geom_smooth(
      method = 'lm',
      se = FALSE,
      formula = .y
    ) +
    stat_poly_eq(
      formula = .y,
      eq.with.lhs = 'italic(hat(y))~`=`~',
      aes(label = paste(..eq.label.., ..rr.label.., sep = '*plain(\',\')~')),
      parse = TRUE
    )
)
print(med_plots)

# Specify functions to obtain smoothed medians for 1st-, 2nd-, and 3rd-order polynomial smoothing formulas

model_median_1st <- function() {
  lm(median ~ group, data = norm_build_med_hilo_sum) %>% assign('model_median', ., envir = .GlobalEnv)
  get_regression_points(model_median, digits = 6) %>% pull(median_hat) %>% assign('median_sm', ., envir = .GlobalEnv)
}

model_median_2nd <- function() {
  lm(median ~ group + I(group ^ 2), data = norm_build_med_hilo_sum) %>% assign('model_median', ., envir = .GlobalEnv)
  get_regression_points(model_median, digits = 6) %>% pull(median_hat) %>% assign('median_sm', ., envir = .GlobalEnv)
}

model_median_3rd <- function() {
  lm(median ~ group + I(group ^ 2) + I(group ^ 3), data = norm_build_med_hilo_sum) %>% assign('model_median', ., envir = .GlobalEnv)
  get_regression_points(model_median, digits = 6) %>% pull(median_hat) %>% assign('median_sm', ., envir = .GlobalEnv)
}

# Run interactive session allowing user to select median smoothing model at console

model_prompt_median <- function() {
  writeLines(
    "Examine plots (use arrow button to scroll plots) and choose smoothing model for medians:\n\n1: First-order polynomial: y = ax + b\n2: Second-order polynomial: y= ax^2 + bx + c\n3: Third-order polynomial: y= ax^3 + bx^2 + cx + d\n"
  )
  model_choice <- as.numeric(0)
  while (is.na(model_choice) || (!(model_choice %in% 1:3))) {
    model_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(model_choice)) {
      writeLines("Please enter 1, 2, or 3\n")
    } else {
      if (model_choice == 1) {
        model_median_1st()
        break
      } else if (model_choice == 2) {
        model_median_2nd()
        break
      } else if (model_choice == 3) {
        model_median_3rd()
        break
      } else {
        writeLines("Please enter 1, 2, or 3\n")
      }
    }
  }
}
model_prompt_median()


# lo_SD 1st-order polynomial and mean substitution plots

lo_SD_1st_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = lo_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = "lo_SD"
       ,
       title = 'lo_SD 1st order polynomial') +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_SD, 1)) +
  geom_smooth(method = 'lm',
              se = FALSE,
              formula = y ~ x) +
  stat_poly_eq(
    formula = y ~ x,
    eq.with.lhs = 'italic(hat(y))~`=`~',
    aes(label = paste(..eq.label.., ..rr.label.., sep = '*plain(\',\')~')),
    parse = TRUE
  )

mean_lo_SD <- round(mean(norm_build_med_hilo_sum$lo_SD, na.rm = TRUE), 2)
lo_SD_mean_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = lo_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = 'lo_SD'
       ,
       title = 'lo_SD mean substitution') +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_SD, 1)) +
  geom_hline(yintercept = mean_lo_SD) +
  geom_label(aes(5, mean_lo_SD, label = paste0("Mean lo_SD = ", mean_lo_SD)))

print(lo_SD_mean_plot)
print(lo_SD_1st_plot)

# Specify functions to obtain smoothed lo_SDs for 1st-order polynomial and mean substitution smoothing formulas

model_lo_SD_1st <- function() {
  lm(lo_SD ~ group, data = norm_build_med_hilo_sum) %>% assign('model_lo_SD', ., envir = .GlobalEnv)
  get_regression_points(model_lo_SD, digits = 6) %>% pull(lo_SD_hat) %>% assign('lo_SD_sm', ., envir = .GlobalEnv)
}

model_lo_SD_mean <- function() {
  norm_build_med_hilo_sum %>% mutate(mean_lo_SD = mean_lo_SD) %>% pull(mean_lo_SD) %>% assign('lo_SD_sm', ., envir = .GlobalEnv)
  enframe(mean(norm_build_med_hilo_sum$lo_SD), name = NULL, value = "model_lo_SD") %>% assign('model_lo_SD', ., envir = .GlobalEnv)
}

# Run interactive session allowing user to select lo_SD smoothing model at console

model_prompt_lo_SD <- function() {
  writeLines(
    "\nExamine plots (use arrow button to scroll plots)  and choose smoothing model for lo_SDs:\n\n1: First-order polynomial: y = ax + b\n2: Mean substitution: y= mean(x)\n"
  )
  model_choice <- as.numeric(0)
  while (is.na(model_choice) || (!(model_choice %in% 1:2))) {
    model_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(model_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (model_choice == 1) {
        model_lo_SD_1st()
        break
      } else if (model_choice == 2) {
        model_lo_SD_mean()
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
model_prompt_lo_SD()


# hi_SD 1st-order polynomial and mean substitution plots

hi_SD_1st_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = hi_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = "hi_SD"
       ,
       title = 'hi_SD 1st order polynomial') +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_SD, 1)) +
  geom_smooth(method = 'lm',
              se = FALSE,
              formula = y ~ x) +
  stat_poly_eq(
    formula = y ~ x,
    eq.with.lhs = 'italic(hat(y))~`=`~',
    aes(label = paste(..eq.label.., ..rr.label.., sep = '*plain(\',\')~')),
    parse = TRUE
  )

mean_hi_SD <- round(mean(norm_build_med_hilo_sum$hi_SD, na.rm = TRUE), 2)
hi_SD_mean_plot <-
  ggplot(norm_build_med_hilo_sum, aes(x = group, y = hi_SD)) +
  geom_point(
    col = 'red',
    fill = 'red',
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  labs(x = 'age group'
       ,
       y = 'hi_SD'
       ,
       title = 'hi_SD mean substitution') +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_SD, 1)) +
  geom_hline(yintercept = mean_hi_SD) +
  geom_label(aes(5, mean_hi_SD, label = paste0("Mean hi_SD = ", mean_hi_SD)))

print(hi_SD_mean_plot)
print(hi_SD_1st_plot)

# Specify functions to obtain smoothed hi_SDs for 1st-order polynomial and mean substitution smoothing formulas

model_hi_SD_1st <- function() {
  lm(hi_SD ~ group, data = norm_build_med_hilo_sum) %>% assign('model_hi_SD', ., envir = .GlobalEnv)
  get_regression_points(model_hi_SD, digits = 6) %>% pull(hi_SD_hat) %>% assign('hi_SD_sm', ., envir = .GlobalEnv)
}

model_hi_SD_mean <- function() {
  norm_build_med_hilo_sum %>% mutate(mean_hi_SD = mean_hi_SD) %>% pull(mean_hi_SD) %>% assign('hi_SD_sm', ., envir = .GlobalEnv)
  enframe(mean(norm_build_med_hilo_sum$hi_SD), name = NULL, value = "model_hi_SD") %>% assign('model_hi_SD', ., envir = .GlobalEnv)
}

# Run interactive session allowing user to select hi_SD smoothing model at console

model_prompt_hi_SD <- function() {
  writeLines(
    "\nExamine plots (use arrow button to scroll plots)  and choose smoothing model for hi_SDs:\n\n1: First-order polynomial: y = ax + b\n2: Mean substitution: y= mean(x)\n"
  )
  model_choice <- as.numeric(0)
  while (is.na(model_choice) || (!(model_choice %in% 1:2))) {
    model_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(model_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (model_choice == 1) {
        model_hi_SD_1st()
        break
      } else if (model_choice == 2) {
        model_hi_SD_mean()
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
model_prompt_hi_SD()

# Create table needed for next phase of norms process
smooth_med_SD <-
  cbind(norm_build_med_hilo_sum, median_sm, lo_SD_sm, hi_SD_sm) %>% dplyr::select(-(lo1:hi2)) %>%
  mutate(ES_sm = round((median_sm - lag(median_sm)) / ((
    lo_SD_sm + lag(lo_SD_sm) + hi_SD_sm + lag(hi_SD_sm)
  ) / 4), 3))

# PROMPT TO INSPECT PLOT SMOOTHED MEDIANS VS. IMPUTED MEDIANS VS RAW SCORE MEDIANS.
# NOTE: CODE BELOW NOT PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
smooth_med_SD_means <-
  smooth_med_SD %>% bind_cols(enframe(raw_means)) %>% rename(mean = value, smoothed_median = median_sm) %>% 
  select(group, agestrat, mean, median, smoothed_median) %>% melt(id.vars = c("group", "agestrat"), variable.name = "parameter")
raw_vs_imputed_vs_sm_plot <-
  ggplot(data = smooth_med_SD_means, aes(group, value, col = parameter, shape = parameter)) +
  geom_point(
    size = 3
  ) +
  scale_colour_manual(values=c("green", "blue", "red")) +   
  scale_shape_manual(values=c(15, 16, 17)) +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_mean, 5), limits = c(0, scale_y_ceiling_mean)) +
  labs(title = "Raw Score Means vs. Imputed Medians vs. Smoothed Medians", x = "Agestrat", y = "Total Score")
print(raw_vs_imputed_vs_sm_plot)

raw_vs_imputed_vs_sm_plot_prompt <- function() {
  writeLines(c(
    strrep("\u2500", 80),
    "Examine plot of raw score means vs. imputed medians vs smoothed medians.",
    "Then press [enter] to continue.",
    strrep("\u2500", 80)
  ))
  readline()
}
raw_vs_imputed_vs_sm_plot_prompt ()


# PROMPT TO INSPECT PLOT SMOOTHED MEDIANS, lo_SDs, hi_SDs.

###################################################################
# NONE OF CODE BELOW HERE HAS BEEN PROPIGATED TO MARKDOWN OR CASL-2 SCRIPTS
###################################################################
smooth_plot <- ggplot(data = smooth_med_SD, aes(group, median_sm)) +
  geom_point(
    col = "blue",
    fill = "blue",
    alpha = .5,
    size = 3,
    shape = 23
  ) +
  geom_label_repel(aes(label = round(median_sm, 2)), hjust = .7, vjust = -1, label.padding = unit(0.1, "lines"), size = 4, col = "blue") +
  scale_x_continuous(breaks = seq(1, num_agestrat, 1), labels = agestrat) +
  scale_y_continuous(breaks = seq(0, scale_y_ceiling_mean, 5), limits = c(0, scale_y_ceiling_mean)) +
  labs(title = "Smoothed Medians, lo_SDs, hi_SDs", x = "Agestrat", y = "Total Score") +
  geom_errorbar(
    aes(ymin = median_sm - lo_SD_sm, ymax = median_sm + hi_SD_sm),
    col = "red",
    size = 0.2,
    width = 0.2
  ) 
print(smooth_plot)

smooth_plot_prompt <- function() {
  writeLines(c(
    strrep("\u2500", 80),
    "Examine plot of smoothed medians, lo_SDs, hi_SDs.",
    "Then press [enter] to continue.",
    strrep("\u2500", 80)
  ))
  readline()
}
smooth_plot_prompt ()

print(mean_plot)
mean_plot_compare_prompt <- function() {
  writeLines(c( 
    strrep("\u2500", 80),
    "Using arrow button, compare smoothed plot to plot of raw score means and SDs.",
    "Then press [enter] to continue.",
    strrep("\u2500", 80)
  ))
  readline()
}
mean_plot_compare_prompt ()


# Clean up environment
rm(list = ls()[!ls() %in% c("smooth_med_SD", "model_median", "model_lo_SD", "model_hi_SD", "score_name", 
                            "num_agestrat", "max_raw", "min_raw", "scale_y_ceiling_mean", "scale_y_ceiling_SD", "mean_plot")])

# Next code section can deal with situation where smoothed medians have effect
# sizes between adjacent age groups that are too large (ES > .33) for
# well-functioning norms. The interactive feature prompts to look at effect
# sizes and choose whether to continue with existing age-strata or create new
# age groups to compensate for too-large effect sizes. If you choose to create
# new strata, code inserts blank rows between rows that are yielding problematic
# effect sizes, and uses the smoothing models to impute median, lo_SD, hi_SD for
# those new rows.

prep_impute <- function() {
  # flag rows with too-big effect sizes
  invisible(
    smooth_med_SD %>%
      mutate(flag = case_when(ES_sm > .33 ~ "flag",
                              TRUE ~ NA_character_)) %>%
      # insert alternating blank lines
      map_dfr(rbind,
              NA) %>%
      # remove blank lines except those preceeding flagged rows.
      filter(!(is.na(lead(flag)) & is.na(group))) %>%
      # insert ".5" group numbers in blank lines, dummy values.
      mutate_at(vars(group),
                ~ case_when(is.na(.x) ~ lag(.x) + .5,
                            TRUE ~ as.double(.x))) %>% 
      assign('smooth_med_SD', ., envir = .GlobalEnv)
  )
}

preds_impute <- function() {
  # adding `newdata` arg to `get_regression_points` allows you to use previously
  # defined regression equation to get predicted values with new data.
  get_regression_points(model_median, digits = 6, newdata = smooth_med_SD) %>% pull(median_hat)  %>% assign('median_sm_imp', ., envir = .GlobalEnv)
  
  if (is_tibble(model_lo_SD)) {
    rep(as.numeric(model_lo_SD), length(median_sm_imp)) %>% assign('lo_SD_sm_imp', ., envir = .GlobalEnv)
  } else {
    get_regression_points(model_lo_SD, digits = 6, newdata = smooth_med_SD) %>% pull(lo_SD_hat) %>% assign('lo_SD_sm_imp', ., envir = .GlobalEnv)
  }
  
  if (is_tibble(model_hi_SD)) {
    rep(as.numeric(model_hi_SD), length(median_sm_imp)) %>% assign('hi_SD_sm_imp', ., envir = .GlobalEnv)
  } else {
    get_regression_points(model_hi_SD, digits = 6, newdata = smooth_med_SD) %>% pull(hi_SD_hat) %>% assign('hi_SD_sm_imp', ., envir = .GlobalEnv)
  }
}

table_impute <- function() {
  invisible(
    add_column(smooth_med_SD, median_sm_imp, lo_SD_sm_imp, hi_SD_sm_imp) %>%
      select(group, agestrat, median_sm_imp, lo_SD_sm_imp, hi_SD_sm_imp) %>%
      rename(median_sm = median_sm_imp,
             lo_SD_sm = lo_SD_sm_imp,
             hi_SD_sm = hi_SD_sm_imp) %>%
      mutate(ES_sm = round((median_sm - lag(median_sm)) / ((
        lo_SD_sm + lag(lo_SD_sm) + hi_SD_sm + lag(hi_SD_sm)
      ) / 4), 3)) %>%
      # `mutate_at` operates on `agestrat` (identified using `vars`), recodes `NA` to
      # mean of lagging, leading values using `case_when`, when it finds non-missing
      # values (the `TRUE` argument - equivalent to ELSE),  it doesn't change the
      # value: `.x` is a token for any cell value, `as.double` ensures output of
      # `TRUE` is same data type as input.
      mutate_at(
        vars(
          agestrat
        ), ~ case_when(
          is.na(.x) ~ (lag(.x) + lead(.x))/2,
          TRUE ~ as.double(.x)
        )
      ) %>% 
      assign('smooth_med_SD', ., envir = .GlobalEnv)
  )
}


print(smooth_med_SD %>% select(agestrat, median_sm, ES_sm))
implied_AG_prompt <- function() {
  writeLines(
    "\nExamine 'ES_sm' column of above table, looking for effect sizes > .33. Choose approach to age groups:\n\n1: Retain current age groups\n2: Create implied age groups to compensate for ES > .33"
  )
  implied_AG_choice <- as.numeric(0)
  while (is.na(implied_AG_choice) || (!(implied_AG_choice %in% 1:2))) {
    implied_AG_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(implied_AG_choice)) {
      writeLines("Please enter 1 or 2\n")
    } else {
      if (implied_AG_choice == 1) {
        break
      } else if (implied_AG_choice == 2) {
        prep_impute()
        preds_impute()
        table_impute()
        break
      } else {
        writeLines("Please enter 1 or 2\n")
      }
    }
  }
}
implied_AG_prompt()

# next code section can deal with situations where there are score reversals
# either 2SD above or 2SD below the imputed median. Score reversal means that as
# you age upward from one age group to the next, the raw score at the
# distribution extreme decreases, rather than increasing as expected. This
# situation would produce an unexpected result when a child crosses the
# threshold between age groups. If, for example, there was a score reversal two
# SDs below the median, an identical raw score would result in a HIGHER standard
# score when the child ages up one group. This is counter-intuitive; you expect
# the same raw score to receive a LOWER standard score in the older group,
# because the older peers have higher ability in the skill being measured.

# prep table of smoothed medians, SDs for treatment of any score reversals at distribution extremes.
smooth_med_SD_fun <- function() {
  smooth_med_SD %>%
    mutate(
      med_minus_2SD = median_sm - (2 * lo_SD_sm),
      med_plus_2SD = median_sm + (2 * hi_SD_sm),
      diff_minus_2SD = med_minus_2SD - lag(med_minus_2SD),
      diff_plus_2SD = med_plus_2SD - lag(med_plus_2SD),
      med = median_sm
    ) %>%
    select(
      group,
      agestrat,
      diff_minus_2SD,
      med_minus_2SD,
      lo_SD_sm,
      median_sm,
      hi_SD_sm,
      med_plus_2SD,
      diff_plus_2SD
    ) %>%
    # `na.print = NULL` argument below is needed to get `print` to handle `NA` values in input table.
    print(n = nrow(.), na.print = NULL) %>%
    assign('smooth_med_SD', ., envir = .GlobalEnv)
}
smooth_med_SD_fun()

# process lo_SD score reversals
lo_SD_hand_smooth_prompt <- function() {
  writeLines("\nDo you want to adjust lo_SD_sm (standard deviation BELOW the smoothed median)?")
  repeat {
    adjust_lo <-
      suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
    if (adjust_lo %in% c('Y', 'y', 'Yes', 'yes')) {
      writeLines("\nChoose row in which you want to adjust lo_SD_sm.")
      repeat {
        lo_SD_hand_smooth_row_choice <-
          suppressWarnings(as.numeric(readline(prompt = "Enter row number: ")))
        if (lo_SD_hand_smooth_row_choice %in% 1:nrow(smooth_med_SD))
        {
          lo_SD_new_value <- as.numeric(0)
          while (is.na(lo_SD_new_value) ||
                 (!(lo_SD_new_value > 0 &&
                    lo_SD_new_value < 100))) {
            lo_SD_new_value <-
              suppressWarnings(as.numeric(
                readline(prompt = "Enter new value (between 0 and 100) for lo_SD_sm: ")
              ))
            smooth_med_SD %>%
              mutate_at(
                vars(lo_SD_sm),
                ~ case_when(
                  group == lo_SD_hand_smooth_row_choice ~ lo_SD_new_value,
                  TRUE ~ .x
                )
              ) %>%
              assign('smooth_med_SD', ., envir = .GlobalEnv)
          }
          smooth_med_SD_fun()
          writeLines(
            "\nExamine table above to see effect of adjusted lo_SD_sm on value of diff_minus_2SD in same row.
            \nDo you want to adjust lo_SD_sm in another row?"
          )
          repeat {
            adjust_again <-
              suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
            if (adjust_again %in% c('Y', 'y', 'Yes', 'yes')) {
              break
              # break exits one level of nested loops
            }
            else if (adjust_again %in% c('N', 'n', 'No', 'no')) {
              # return() exits all nested loops
              return()
            }
          }
        }
    }
    } else if (adjust_lo %in% c('N', 'n', 'No', 'no')) {
      break
    }
  }
  }

# process hi_SD score reversals
hi_SD_hand_smooth_prompt <- function() {
  writeLines("\nDo you want to adjust hi_SD_sm (standard deviation ABOVE the smoothed median)?")
  repeat {
    adjust_hi <-
      suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
    if (adjust_hi %in% c('Y', 'y', 'Yes', 'yes')) {
      writeLines("\nChoose row in which you want to adjust hi_SD_sm.")
      repeat {
        hi_SD_hand_smooth_row_choice <-
          suppressWarnings(as.numeric(readline(prompt = "Enter row number: ")))
        if (hi_SD_hand_smooth_row_choice %in% 1:nrow(smooth_med_SD))
        {
          hi_SD_new_value <- as.numeric(0)
          while (is.na(hi_SD_new_value) ||
                 (!(hi_SD_new_value > 0 &&
                    hi_SD_new_value < 100))) {
            hi_SD_new_value <-
              suppressWarnings(as.numeric(
                readline(prompt = "Enter new value (between 0 and 100) for hi_SD_sm: ")
              ))
            smooth_med_SD %>%
              mutate_at(
                vars(hi_SD_sm),
                ~ case_when(
                  group == hi_SD_hand_smooth_row_choice ~ hi_SD_new_value,
                  TRUE ~ .x
                )
              ) %>%
              assign('smooth_med_SD', ., envir = .GlobalEnv)
          }
          smooth_med_SD_fun()
          writeLines(
            "\nExamine table above to see effect of adjusted hi_SD_sm on value of diff_minus_2SD in same row.
            \nDo you want to adjust hi_SD_sm in another row?"
          )
          repeat {
            adjust_again <-
              suppressWarnings(as.character(readline(prompt = "Enter Y or N: ")))
            if (adjust_again %in% c('Y', 'y', 'Yes', 'yes')) {
              break
              # break exits one level of nested hiops
            }
            else if (adjust_again %in% c('N', 'n', 'No', 'no')) {
              # return() exits all nested hiops
              return()
            }
          }
        }
    }
    } else if (adjust_hi %in% c('N', 'n', 'No', 'no')) {
      break
    }
  }
  }


# create function to choose to adjust SDs by hand.
hand_smooth_choice_fun <- function() {
  hand_smooth_choice <- as.numeric(0)
  while (is.na(hand_smooth_choice) ||
         (!(hand_smooth_choice %in% 1:2))) {
    hand_smooth_choice <-
      suppressWarnings(as.numeric(readline(prompt = "Enter choice: ")))
    if (is.na(hand_smooth_choice)) {
      writeLines("\nPlease enter 1 or 2")
    } else {
      if (hand_smooth_choice == 1) {
        break
      } else if (hand_smooth_choice == 2) {
        lo_SD_hand_smooth_prompt()
        hi_SD_hand_smooth_prompt()
        break
      } else {
        writeLines("\nPlease enter 1 or 2")
      }
    }
  }
}

# route input tables into code streams depending presense of lo and/or hi score reversals.
if (!any(na.omit(smooth_med_SD$diff_minus_2SD) < 0) &&
    !any(na.omit(smooth_med_SD$diff_plus_2SD) < 0)) {
  writeLines(
    "\nR found no score reversals, going from one age group to the next, at either 2 SD above or 2 SD below the imputed median.
    \nChoose next step:\n\n1: Proceed without adjusting SDs\n2: Make manual adjustments to SDs"
  )
  hand_smooth_choice_fun()
} else if (any(na.omit(smooth_med_SD$diff_minus_2SD) < 0) &&
           !any(na.omit(smooth_med_SD$diff_plus_2SD) < 0)) {
  writeLines(
    '\nR found score reversals, between age groups, at 2 SD below imputed median (see negative values for diff_minus_2SD in table printed above).
    \nChoose next step:\n\n1: Proceed without adjusting SDs\n2: Make manual adjustments to SDs'
  )
  hand_smooth_choice_fun()
} else if (!any(na.omit(smooth_med_SD$diff_minus_2SD) < 0) &&
           any(na.omit(smooth_med_SD$diff_plus_2SD) < 0)) {
  writeLines(
    '\nR found score reversals, between age groups, at 2 SD above imputed median (see negative values for diff_plus_2SD in table printed above).
    \nChoose next step:\n\n1: Proceed without adjusting SDs\n2: Make manual adjustments to SDs'
  )
  hand_smooth_choice_fun()
} else if (any(na.omit(smooth_med_SD$diff_minus_2SD) < 0) &&
           any(na.omit(smooth_med_SD$diff_plus_2SD) < 0)) {
  writeLines(
    '\nR found score reversals, between age groups, at both 2 SD above and 2 SD below imputed median (see negative values for diff_minus_2SD, diff_plus_2SD in table printed above).
    \nChoose next step:\n\n1: Proceed without adjusting SDs\n2: Make manual adjustments to SDs'
  )
  hand_smooth_choice_fun()
}

writeLines(
  '\nPress [Enter] to generate standard-score look-up tables.'
)
invisible(readline())


# Clean up environment
rm(list = ls()[!ls() %in% c("smooth_med_SD", "score_name", "num_agestrat", "max_raw", "min_raw", 
                            "scale_y_ceiling_mean", "scale_y_ceiling_SD", "mean_plot")])

# next code section generates raw-to-SS look-up tables.

# create table with only vars needed to calc standard scores
final_med_SD <- smooth_med_SD %>% select(agestrat, median_sm, lo_SD_sm, hi_SD_sm) %>% mutate_at(vars(agestrat), ~ paste0("mo_", .x))
# define column names for lookup stable by creating charvec of agestrat labels
# agelabels <- final_med_SD %>% pull(agestrat) %>% paste0("mo_", .)
# create empty look up table by binding column of all possible raw scores to set of columns holding numerical NAs, naming each column in set using agelabels charvec
raw_to_SS_lookup_empty <- bind_cols(
  enframe(min_raw:max_raw, name = NULL, value = 'rawscore'),
  data.frame(matrix(NA_real_, nrow = max_raw + 1, ncol = num_agestrat)) %>%
    set_colnames(final_med_SD$agestrat)
)

# fill empty raw-to-SS lookup with standard scores by age strat.
raw_to_SS_lookup <- raw_to_SS_lookup_empty %>%
  # gather collapses the empty wide rawscore by agestrat table into tall table
  # with three columns, rawscore, agestrat, and an empty SS column. Rawscore
  # sequence is repeated down its column, once per agestrat, and value of
  # agestrat column is uniform for the entire rawscore sequence. `-rawscore` is
  # `dplyr::select` code that drops all vars except rawscore.
  gather(agestrat, SS, -rawscore) %>%
  # This tall table can now be joined with `final_med_SD`, which contains
  # smoothed meds and SDs per agestrat, because both tables have an `agestrat`
  # column that can be used as a `by` var. In the newly-constituted tall table,
  # `median_sm`, `lo_SD_sm`, and `hi_SD_sm` cols hold the correct values for
  # each agestrat.
  left_join(final_med_SD, by = 'agestrat') %>%
  # calculate SS above and below the median. `meidan_SM = NULL` drops this now
  # unnecessary column from the piped object.
  mutate(SS = case_when(
    rawscore <= median_sm ~ round(100 + (((rawscore - median_sm) / lo_SD_sm) *15)),
    TRUE ~ round(100 + (((rawscore - median_sm) / hi_SD_sm) *15))
  ), median_sm = NULL, lo_SD_sm = NULL, hi_SD_sm = NULL) %>%
  # truncate SS distribution at 40 and 160.
  mutate_at(
    vars(SS), ~ case_when(
      .x < 40 ~ 40,
      .x > 160 ~ 160,
      TRUE ~ .x
    )
  ) %>%
  # spread converts table back from tall to wide. Resulting table has one row
  # per `rawscore`. Each value of `agestrat` gets its own column, and each of
  # these columns is populated with the value of `SS` that matches `rawscore`,
  # within that `agestrat`.
  spread(agestrat, SS) %>%
  # select reorders vars so to give the correct sequence of `agestrat` going
  # left-to-right. That order of agestrats is given by the char vec
  # `c(final_med_SD$agestrat)`.
  select(rawscore, c(final_med_SD$agestrat))
rm(raw_to_SS_lookup_empty, final_med_SD, smooth_med_SD)

# write final raw-to-SS lookup table to .csv
write_csv(
  raw_to_SS_lookup, here(
    paste0('OUTPUT-FILES/', score_name, '-raw-SS-lookup.csv')
  )
)
