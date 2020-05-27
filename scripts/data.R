
  library(readxl)
  library(tidyverse)

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

  
# Long data --------------------------------------------------------------------
  
# Take each outcome pre and post, reshape to long, relabel, then join  
  long <- 
    full_join(
      select(data, code, contains("pls5_ac")) %>% # First outcome
        gather(time, pls5_ac, -code) %>%          # Reshape to long
        mutate(
          time = case_when(
          time == time[grepl("pre", time)] ~ "Pre", 
          time == time[grepl("post", time)] ~ "Post"
          )
        ), 
      select(data, code, contains("pls5_es")) %>% # Second outcome
        gather(time, pls5_es, -code) %>% 
        mutate(
          time = case_when(
            time == time[grepl("pre", time)] ~ "Pre", 
            time == time[grepl("post", time)] ~ "Post"
          )
        ),
      by = c("code", "time")
    ) %>%
    full_join(
      select(data, code, contains("pls5_to")) %>% # Third outcome
        gather(time, pls5_to, -code) %>% 
        mutate(
          time = case_when(
            time == time[grepl("pre", time)] ~ "Pre", 
            time == time[grepl("post", time)] ~ "Post"
          )
        ), 
    by = c("code", "time")
    ) %>%
    full_join(select(data, code:arm), by = "code") %>% # Other key variables
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


