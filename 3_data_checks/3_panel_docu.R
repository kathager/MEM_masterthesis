## ---------------------------
##
## Master thesis
## Document panel structure of LSE (from 2012 onwards)
## Author: Katja Hager
##
## Date Created: Oct 2025
##
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation

library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(Hmisc)
library(stargazer)
theme_set(theme_minimal())
## ---------------------------
firm_panel <- readRDS(file = "output/data/2d_firm_panel.rds")
worker_panel <- readRDS(file = "output/data/2d_worker_panel.rds")

## ---------------------------
#how many workers seen in 2012 can we see in subsequent years?
workers_n <- worker_panel %>%
  filter(!is.na(dist_MW_12)) %>% #gap_2012_std
  group_by(erhebja, avs_nb) %>%
  summarise(exists = 1) %>%
  group_by(erhebja) %>%
  summarise(
    n_workers = n()) %>%
  pivot_wider(names_from = erhebja, values_from = n_workers) %>% 
  mutate(' ' = "#workers")

#firms
firms_n <- firm_panel %>%
  filter(mem==1 & !is.na(gap_2012_std)) %>%
  group_by(erhebja, entid) %>%
  summarise(exists = 1)  %>%
  group_by(erhebja) %>%
  summarise(
    n_firms = n()) %>%
  pivot_wider(names_from = erhebja, values_from = n_firms) %>%
  mutate(' ' = "#firms")

panel_n <- rbind(workers_n, firms_n) %>%
  select(c(' ', "2012", "2014", "2016", "2018")) %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = "'")))

stargazer(panel_n, type = "latex", align = TRUE, title = "ESS panel structure", digits = 0,
          label = "tab:ESS_panel", summary = FALSE, rownames = FALSE, no.space = TRUE, 
          font.size = "small", column.sep.width = "3pt", out = "output/table/ESS_panel.tex")

## ---------------------------
#who are the people and firms still observed (2014, 2016, 2018)
#workers
worker_chars <- worker_panel %>%
  filter(!is.na(dist_MW_12)) %>%
  group_by(erhebja) %>%
  mutate(
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"], na.rm = TRUE)/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"], na.rm = TRUE)/sum(gewicht),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    cba = sum(gewicht[mem_cba=="1"], na.rm = TRUE)/sum(gewicht),
    p10 = wtd.quantile(real_wage, weights = gewicht, probs = 0.1, na.rm = TRUE),
    mean_wage = wtd.mean(real_wage, weight = gewicht),
    percentile = wtd.mean(gross_wage_std <= min_wage, weight = gewicht, na.rm = TRUE) * 100,
 #   min_wage = wtd.mean(min_wage, weight = gewicht, na.rm = TRUE),
    A = sum(gewicht[wage_region == "A"], na.rm = TRUE)/sum(gewicht),
    B = sum(gewicht[wage_region == "B"], na.rm = TRUE)/sum(gewicht),
    C = sum(gewicht[wage_region == "C"], na.rm = TRUE)/sum(gewicht),
    perc_mem = mean(mem)) %>%
  summarise(
    "Mean wage" = mean(mean_wage, na.rm = TRUE),
    "P10 wage" = mean(p10, na.rm = TRUE),
 #   "Minimum wage" = mean(min_wage, na.rm = TRUE),
    "Bite of MW" = mean(percentile, na.rm = TRUE),
    "CBA coverage (%)" = mean(cba, na.rim = TRUE)*100,
    "Men (%)" = mean(perc_male, na.rm = TRUE)*100,
    "CH residents (%)" = mean(perc_resident, na.rm = TRUE)*100,
    "Mean age" =mean(mean_age, na.rm = TRUE),
    "Mean tenure" = mean(mean_tenure, na.rm = TRUE), 
    "Low-skilled (%)" = mean(perc_lowskill, na.rm = TRUE)*100,
    "No management (%)" = mean(perc_nomgt, na.rm = TRUE)*100,
    "Wage region A" = mean(A, na.rm = TRUE)*100,
    "Wage region B" = mean(B, na.rm = TRUE)*100,
    "Wage region C" = mean(C, na.rm = TRUE)*100,
    "MEM sector (%)" = round(mean(perc_mem)*100, 3),
    "N" = n()) %>% ungroup() %>% 
  mutate_if(is.numeric, round, digits = 0)

worker_chars_wide <- worker_chars %>%
  pivot_longer(
    cols = -erhebja,                  # all columns except 'year'
    names_to = "variable", 
    values_to = "value") %>%
  pivot_wider(
    names_from = erhebja,             # each year becomes a column
    values_from = value ) %>% 
  rename(Characteristic  = variable)  %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = "'")))

stargazer(worker_chars_wide, type = "latex",title = "Retained workers in ESS panel", digits = 0,
          label = "tab:ESS_panel_workers", summary = FALSE, rownames = FALSE, no.space = TRUE, 
          font.size = "small", column.sep.width = "3pt", out = "output/table/ESS_panel_workers.tex")

#firms
#what firms are in the market?
firms_chars <- firm_panel %>%
  filter(mem==1 & !is.na(gap_2012_std)) %>%
  group_by(erhebja, entid) %>%
  slice(1) %>%
  group_by(erhebja) %>%
  summarise(
   mean_gap =  round(mean(gap_2012_std), 4),
    avg_p50 = round(mean(P50, na.rm = TRUE), 1), 
   # mean_cv_wage = round(mean(coeff_var, na.rm = TRUE), 1),
    min_wage = round(mean(min_wage_w, na.rm = TRUE), 1),
    atleast1MW = round(mean(atleast1MW, na.rm = TRUE)*100,1),
   cba_cov = round(mean(mem_cba == 1, na.rm = TRUE)*100, 0),
   # mean_Kaitz = round(mean(Kaitz_index),3),
   # avg_mean_wage_change_12 = round(mean(mean_wage_change_12, na.rm = TRUE), 3),
    perc_small = round(mean(firm_size == "small (1-49)", na.rm = TRUE) * 100,1),
    perc_med = round(mean(firm_size == "medium (50-249)", na.rm = TRUE) * 100,1),
    perc_large = round(mean(firm_size == "large (250+)", na.rm = TRUE) * 100,1),
    n_obs = n(),
    .groups = 'drop') 

firms_chars_wide <- firms_chars %>%
  pivot_longer(cols = -erhebja,   # Reshape to wide format
               names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = erhebja, values_from = value) %>%
  mutate(Characteristic = case_when(  # Create proper row labels
    variable == "avg_p50" ~ "Median firm wage",
    variable == "mean_gap"~ "Standardized gap (2012)",
    variable == "min_wage" ~ "Minimum Wage Workers (%)",
    variable == "atleast1MW"~ "At least 1 MW employed (%)",
    variable == "cba_cov"~ "CBA coverage (%)",
    variable == "perc_small" ~ "Small firms (1-49) (%)",
    variable == "perc_med" ~ "Medium firms (50-250) (%)",
    variable == "perc_large" ~ "Large firms (250'+) (%)",
   # variable == "mean_cv_wage" ~ "Firm wage dispersion",
    #variable == "mean_Kaitz" ~ "Kaitz index",
   # variable == "avg_mean_wage_change_12"  ~ "Wage change rel. to 2012",
    variable == "n_obs" ~ "N",
    TRUE ~ variable )) %>%
  select(c(Characteristic, "2012", "2014", "2016", "2018")) 

stargazer(firms_chars_wide, type = "latex",title = "Retained firms in ESS panel",
          digits = 0, label = "tab:ESS_panel_firms", summary = FALSE, rownames = FALSE, no.space = TRUE, 
          font.size = "small", column.sep.width = "3pt", out = "output/table/ESS_panel_firms.tex")

