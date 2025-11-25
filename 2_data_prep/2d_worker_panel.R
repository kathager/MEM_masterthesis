## ---------------------------
##
## Master thesis
## Exploit panel structure of LSE (from 2012 onwards)
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
theme_set(theme_minimal())

## ---------------------------
#2008-2018 in here
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")

#worker-level panel -> from 2012
#wage growth: yearly and w.r.t. 2018

#some firms drop out because of AHV-identifiers
LSE_08_18_panel_worker <- LSE_08_18 %>%
  filter(!is.na(avs_nb)) %>%
  filter(avs_nb != "") %>%
  filter(avs_nb != " ")  #365483 

LSE_08_18_panel_worker <- LSE_08_18_panel_worker %>%
  group_by(avs_nb, entid, erhebja) %>% 
  mutate(count = n()) %>%
  filter(count<2) #365483

table(LSE_08_18_panel_worker$erhebja) #now only from 2012
table(LSE_08_18_panel_worker$main_sector)

# create all possible worker-year combinations -> worker-year-panel
all_workers <- unique(LSE_08_18_panel_worker$avs_nb)
all_years <- unique(LSE_08_18_panel_worker$erhebja)
worker_panel <- expand_grid(avs_nb = all_workers, erhebja = all_years) #empty

#extract workers who appear more than once in a year
workers_several <- LSE_08_18_panel_worker %>%
  group_by(erhebja, avs_nb) %>%
  mutate(count = n(),
         total_empl = sum(ibgr)) %>%
  filter(count>1) 
table(workers_several$erhebja)
summary(workers_several$total_empl)
table(workers_several$erhebja, workers_several$min_wage_worker)

#workers_several_filtered <- workers_several%>%   filter(total_empl<1.5)

#yearly wage statistics -> worker perspective. 
#collapse to worker-year-level
worker_wage <- LSE_08_18_panel_worker %>%
  group_by(avs_nb, erhebja) %>%
  summarise(
    mean_gross = mean(gross_wage_std),
    mean_base = mean(base_wage_gross_std), 
    income = sum(blimok),
    total_empl = sum(ibgr),
    dist_to_MW = mean(dist_to_MW), #[mem==1]
    dist_to_MW_high = mean(dist_to_MW_high),
    min_wage = mean(min_wage),
    min_wage_high = mean(min_wage_high),
    gewicht_tot = mean(gewicht))

worker_panel <-left_join(worker_panel, worker_wage, by = c("erhebja", "avs_nb"))

worker_panel <- worker_panel %>% 
  group_by(avs_nb) %>% arrange(erhebja) %>%
  mutate(
    wage_change_nominal = (mean_gross - lag(mean_gross))/lag(mean_gross),
    wage_change_base = (mean_base - lag(mean_base))/lag(mean_base),
    blimok_change = (income - lag(income))/lag(income) ,
    empl_change = (total_empl - lag(total_empl))/lag(total_empl)           ) %>%
  mutate(
    dist_MW_12   = dist_to_MW[erhebja == 2012],
    dist_MW_12_rel = dist_MW_12/min_wage[erhebja==2012],
    dist_to_MW_high_12 = dist_to_MW_high[erhebja == 2012],
    dist_MW_12_high_rel = dist_to_MW_high_12/min_wage_high[erhebja==2012],
    wage_change_nominal_12 = (mean_gross - mean_gross[erhebja == 2012])/mean_gross[erhebja == 2012],
    wage_change_base_12 = (mean_base - mean_base[erhebja == 2012])/mean_base[erhebja == 2012],
    blimok_change_12 = (income - income[erhebja == 2012])/income[erhebja == 2012],
    empl_change_12 = (total_empl - total_empl[erhebja == 2012])/total_empl[erhebja == 2012]   ) 

summary(worker_panel$dist_MW_12_rel)
#? do we really want to leave the worker-year panel structure? -> yes
#but let's only keep the workers with observations, so we do not have a balanced dataset anymore
worker_panel <- subset(worker_panel, !is.na(mean_gross))

worker_panel <- left_join(LSE_08_18_panel_worker,
                          worker_panel  %>% select(-c("dist_to_MW", "dist_to_MW_high")), 
                          by = c("erhebja", "avs_nb"))

#worker-panel: Has more than one observation per worker. has worker-firm observations, sometimes even several times the same worker within the firm.
saveRDS(worker_panel, file = "output/data/2d_worker_panel.rds")

summary(worker_panel$dist_MW_12[worker_panel$dist_MW_12_rel <= -0.9])

worker_panel <- worker_panel %>% 
  arrange((dist_MW_12_rel))

## ---------------------------
#OLD
#base wage in 2012 (for each worker-firm combination)
base_wage <-  firm_worker_panel %>%
  filter(erhebja==2012) %>%
  group_by(avs_nb, entid) %>%
  mutate(
    wage_nominal_2012 = (gross_wage_std), 
    wage_real_2012 = (real_wage),
    wage_base_2012 = (base_wage_gross_std),
    wage_blimok_2012 = (blimok)) 
