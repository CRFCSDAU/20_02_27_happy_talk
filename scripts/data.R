
  library(readxl)
  library(tidyverse)
  library(testthat)

# Data -------------------------------------------------------------------------
  data <- read_excel("data/corrected Master HappyTalk.xlsx")

  names(data) <- gsub(
    "\\.\\.\\.|\\.\\.|\\.", "_", make.names(tolower(names(data)))
    )

  data <- filter(data, !is.na(age)) # Remove invalid rows

# Changes/Corrections ---------------------------------------------------------- 
  data$group[data$group == 1.11] <- 1
  data$group <- factor(
    data$group, 
    levels = c(0, 1), 
    labels = c("Control", "Active")
    )
  data <- rename(data, arm = group)
 
# New numeric outcomes with character responses as missing   
  data$pls5_ac_pre <- as.numeric(data$pls_ac_score)
  data$pls5_es_pre <- as.numeric(data$pls_es_score)
  data$pls5_to_pre <- as.numeric(data$tl_score)
  
  data$pls5_ac_post <- as.numeric(data$post_pls_ac_score)
  data$pls5_es_post <- as.numeric(data$post_pls_es_score)
  data$pls5_to_post <- as.numeric(data$post_tl_score)
  
# Add QOL data -----
  
  qol <- read_excel("data/corrected Master HappyTalk.xlsx", sheet = 5)
  names(qol) <- gsub("\\.", "_", make.names(tolower(names(qol))))
  qol <- rename(qol, age_qol = age, group_qol = group, gender_qol = gender, 
                post_age_qol = post_age)
  qol <- filter(qol, !is.na(code)) # Remove invalid rows
  
  data <- full_join(data, qol, by = "code")
  
  # with(data, table(gender, gender_qol))
  # with(data, table(age, age_qol))
  # with(data, table(arm, group_qol))
  
  data <- select(data, -age_qol, -group_qol, -gender_qol, -post_age_qol)
  
  data <- rename(data,
    peds_psychosocial_pre = peds_psychosocial, 
    peds_physical_pre = peds_physical, 
    peds_tot_pre = peds_tot, 
    chu9d_tot_pre = chu9d_tot, 
    post_peds_physical = post_peds_physcical
    )
  
# Remove participant values for pedsql for outlier values for now  
  data$peds_physical_pre[data$code == "CMC308"] <- NA
  data$peds_psychosocial_pre[data$code == "CMC308"] <- NA
  data$peds_tot_pre[data$code == "CMC308"] <- NA
  data$post_peds_physical[data$code == "CMC308"] <- NA
  data$post_peds_psychosocial[data$code == "CMC308"] <- NA
  data$post_peds_tot[data$code == "CMC308"] <- NA
  

# Add Responsiveness data  
  
  res <- read_excel("data/corrected Master HappyTalk.xlsx", sheet = 4)
  
  names(res) <- gsub(
    "\\.\\.\\.|\\.\\.|\\.", "_", make.names(tolower(names(res)))
  )
  
  res <- select(
    res, code, 
    res_exp_pre = expansion, 
    res_im_pre = imitation, 
    res_res_pre = responsive_question,
    res_label_pre = label, 
    res_total_pre = total_responsive_behaviours_1, 
    res_rate_pre = rate_min,
    parris_pre = parris, 
    res_exp_post = post_expansion, 
    res_im_post = post_imitation, 
    res_res_post = post_responsive_question, 
    res_label_post = post_label, 
    res_total_post = post_total_responsive_behaviour, 
    res_rate_post = post_rate_per_min, 
    parris_post = post_parris
  )
  
  res <- filter(res, !is.na(code)) # Remove invalid rows
  
  data <- full_join(data, res, by = "code")
  
  
# Long data --------------------------------------------------------------------
  
# Select and reshape function
  
  longify <- function(var, ...){
    select(data, code, contains(var)) %>%
    gather(time, !!var, -code) %>%                       
    mutate(
      time = case_when(
      time == time[grepl("pre", time)] ~ "Pre", 
      time == time[grepl("post", time)] ~ "Post"
      )
    ) %>%
    return()
  }
  
# Take each outcome pre and post, reshape to long, relabel, then join  
  
  tar <- list( # Variables to convert to long
    "pls5_ac", 
    "pls5_es", 
    "pls5_to", 
    "peds_psychosocial", 
    "peds_physical",
    "peds_tot",   
    "chu9d_tot",  
    "res_exp",    
    "res_im",     
    "res_res",    
    "res_label",  
    "res_rate",  
    "res_total",
    "parris"    
  )
  
  long <- full_join(
    reduce(map(tar, longify), left_join, by = c("code", "time")), 
    data, 
    by = c("code")
  ) %>%
    mutate(time = factor(time, levels = c("Pre", "Post")))
  
  
# Tests ------------------------------------------------------------------------  
  expect_equal(nrow(data), 81)
  expect_equal(nrow(data), length(unique(data$code)))
  expect_equal(2, length(table(data$arm)))
  expect_equal(nrow(data), sum(table(data$arm)))
  
  
# Save -------------------------------------------------------------------------
  save(data, long, file = "data.RData")
  # rm(list = ls())
  # load("data.RData")

  
# View -------------------------------------------------------------------------  
  # library(summarytools)
  # view(dfSummary(data))


