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
#2392 firms in 2012 in MEM
#pre 2012: we can follow establishments (Betriebsstätten), but we cannot follow workers
#from 2012 (entid)
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")

#we just want firms from the MEM and watch sector
LSE_08_18_firm_panel <- LSE_08_18 %>%
  filter(mem==1 | watch == 1) %>%
  mutate(
    above = case_when(
      MW_bins %in% c("[min, -600)", "[-600, -300)", "[-300, 0)") ~0,
      MW_bins %in% c("[0, 300)", "[300, 600)") ~1,
      TRUE~NA_real_))
table(LSE_08_18_firm_panel$erhebja)
table(LSE_08_18_firm_panel$above)

# create all possible firm-year combinations -> firm-year-panel
all_firms <- unique(LSE_08_18_firm_panel$burnr)
all_years <- unique(LSE_08_18_firm_panel$erhebja)
firm_panel <- expand_grid(burnr = all_firms, erhebja = all_years) #empty

##Kaitz-Index
#MW/median_wage -> use low/conservative minimum wage
Kaitz_firms <- LSE_08_18_firm_panel %>%
  group_by(burnr, erhebja) %>%
  filter(!is.na(real_wage) & !is.na(gewicht) & gewicht > 0) %>%
  summarise(
    med_wage = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE), #median wage in that firm
    min_wage = weighted.mean(real_min_wage, weights = gewicht, na.rm = TRUE),     #what is the assigned minimum wage to the workforce (since there are several)
    Kaitz_index = min_wage/med_wage )   #divide
firm_panel <- left_join(firm_panel, Kaitz_firms %>% select(c("Kaitz_index", "burnr", "erhebja")), 
                        by = c("burnr", "erhebja"))

check_12 <- Kaitz_firms %>%
  filter(erhebja==2012) #2'392 firms in 212

#within-firm wage compression / variation
#Variation in wages: Coefficient of Variation
# Coefficient of variation (CV): summary measure of relative dispersion
#CV = [SD(wages)/mean(wages)] * 100
wage_variation <- LSE_08_18_firm_panel %>%
  group_by(erhebja, burnr) %>% #coefficient of variation of wages within each firm-year
  summarise(var_wage = wtd.var(real_wage, weights = gewicht, na.rm = TRUE), 
            mean_wage = weighted.mean(real_wage, weights = gewicht, na.rm = TRUE),
            coeff_var_raw = (sqrt(var_wage) / mean_wage)*100,
            count = n(),
            coeff_var = ifelse(count==1, NA, coeff_var_raw))
#variation = 0 in 44 observations: occurs for firms with 2 obs, 1 firm has 4 obs
summary(wage_variation$coeff_var)
sum(wage_variation$coeff_var[wage_variation$erhebja==2012]!=0, na.rm = TRUE)
sum(wage_variation$coeff_var==0, na.rm = TRUE) #44
summary(wage_variation$count[wage_variation$coeff_var==0])
firm_panel <- left_join(firm_panel, wage_variation %>% select(c("coeff_var", "burnr", "erhebja")),
                        by = c("burnr", "erhebja"))

MW_at_firms <- LSE_08_18_firm_panel %>%
  group_by(burnr, erhebja) %>%
  filter(!is.na(real_wage) & !is.na(gewicht) & gewicht > 0) %>%
  summarise(
    min_wage_w = weighted.mean(min_wage_worker == "Minimum wage worker", 
                               w = gewicht, na.rm = TRUE) * 100,
    atleast1MW = case_when(
      any(min_wage_worker == "Minimum wage worker") ~ 1,
      TRUE ~ 0  ) )  
firm_panel <- left_join(firm_panel, MW_at_firms, by = c("burnr", "erhebja"))

##mean wage per firm (of people below the global median wage) -> outcome for Derenoncourt
mean_wage <- LSE_08_18_firm_panel %>%
  group_by(erhebja) %>%
  mutate(threshold_p50 = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE)) %>%
  group_by(burnr, erhebja) %>%
  filter(real_wage <= threshold_p50) %>%
  filter(!is.na(real_wage) & !is.na(gewicht) & gewicht > 0) %>%
  summarise(
    mean_wage = wtd.mean(real_wage, w = gewicht)) 
firm_panel <- left_join(firm_panel, mean_wage, by = c("burnr", "erhebja"))

#within-firm P10/P20/P30/P40/P50 wage growth
#real wage is 100% FTE (gross_wage_std) but adjusted for inflation
wage_percentiles <- LSE_08_18_firm_panel %>%
  group_by(burnr, erhebja) %>%
  summarise(
    P80 = wtd.quantile(real_wage, weights = gewicht, probs = 0.8, na.rm = TRUE),
    P50 = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE),
    P40 = wtd.quantile(real_wage, weights = gewicht, probs = 0.4, na.rm = TRUE),
    P30 = wtd.quantile(real_wage, weights = gewicht, probs = 0.3, na.rm = TRUE),
    P20 = wtd.quantile(real_wage, weights = gewicht, probs = 0.2, na.rm = TRUE),
    P10 = wtd.quantile(real_wage, weights = gewicht, probs = 0.1, na.rm = TRUE),
    P05 = wtd.quantile(real_wage, weights = gewicht, probs = 0.05, na.rm = TRUE))
firm_panel <- left_join(firm_panel, wage_percentiles, by = c("burnr", "erhebja"))
#changes
firm_panel <- firm_panel %>%
  group_by(burnr) %>% arrange(erhebja) %>%
  mutate(
    P50_change = (P50 - lag(P50))/lag(P50),
    P40_change = (P40 - lag(P40))/lag(P40),
    P30_change = (P30 - lag(P30))/lag(P30),
    P20_change = (P20 - lag(P20))/lag(P20),
    P10_change = (P10 - lag(P10))/lag(P10),
    P05_change = (P05 - lag(P05))/lag(P05),
    mean_wage_change = (mean_wage - lag(mean_wage))/lag(mean_wage),
    var_change = ifelse(lag(coeff_var)!=0, 
                        (coeff_var - lag(coeff_var))/lag(coeff_var), NA)   ) %>%
  mutate(
    P50_change_12 = (P50 - P50[erhebja == 2012])/P50[erhebja == 2012],
    P40_change_12 = (P40 - P40[erhebja == 2012])/P40[erhebja == 2012],
    P30_change_12 = (P30 - P30[erhebja == 2012])/P30[erhebja == 2012],
    P20_change_12 = (P20 - P20[erhebja == 2012])/P20[erhebja == 2012],
    P10_change_12 = (P10 - P10[erhebja == 2012])/P10[erhebja == 2012],
    P05_change_12 = (P05 - P05[erhebja == 2012])/P05[erhebja == 2012],
    mean_wage_change_12 = (mean_wage - mean_wage[erhebja == 2012])/mean_wage[erhebja == 2012],
    var_change_12 = (coeff_var - coeff_var[erhebja == 2012])/coeff_var[erhebja == 2012],
    var_change_12 = ifelse(var_change_12==Inf | var_change_12 == -Inf, NA, var_change_12),
    mean_wage_change_08 = (mean_wage - mean_wage[erhebja == 2008])/mean_wage[erhebja == 2008],
    var_change_08 = (coeff_var - coeff_var[erhebja == 2008])/coeff_var[erhebja == 2008],
    var_change_08 = ifelse(var_change_08==Inf | var_change_08 == -Inf, NA, var_change_08)
    )

firm_panel <- subset(firm_panel, !is.na(P50))

## --------
##within-firm employment by above/below MW -> Cengiz
above_below <- unique(LSE_08_18_firm_panel$above)
firm_panel_cengiz <- expand_grid(burnr = all_firms, erhebja = all_years, above = above_below) #empty
firm_panel_cengiz <- subset(firm_panel_cengiz, !is.na(above))

firm_employment <- LSE_08_18_firm_panel %>%
  group_by(burnr, above, erhebja) %>% 
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE),
    .groups = "drop") 
firm_panel_cengiz <- left_join(firm_panel_cengiz, firm_employment, by = c("erhebja", "burnr", "above"))

#changes
firm_employment_pre <- firm_panel_cengiz %>%
  filter(erhebja<2012) %>%
  group_by(burnr, above) %>%
  arrange(erhebja) %>%
  mutate(
    empl_pre = employment[erhebja == 2008],
    n_workers_pre = n_workers[erhebja == 2008]) %>%
  group_by(burnr, above, erhebja) %>%
  summarise(
    diff_workers = n_workers - n_workers_pre,
    diff_empl = employment - empl_pre,
    diff_type = ifelse(above == 1, "excess_jobs", "lost_jobs"),
    .groups = "drop")
firm_employment_post <- firm_panel_cengiz %>%
  filter(erhebja>=2012) %>%
  group_by(burnr, above) %>%
  arrange(erhebja) %>%
  mutate(
    empl_pre = employment[erhebja == 2012],
    n_workers_pre = n_workers[erhebja == 2012])%>%
  group_by(burnr, above, erhebja) %>%
  summarise(
    diff_workers = n_workers - n_workers_pre,
    diff_empl = employment - empl_pre,
    diff_type = ifelse(above == 1, "excess_jobs", "lost_jobs"),
    .groups = "drop")
firm_employment <- rbind(firm_employment_pre, firm_employment_post)

firm_panel_cengiz <- left_join(firm_panel_cengiz, firm_employment, by = c("burnr", "erhebja", "above"))

#merge to firm panel
firm_panel <- left_join(firm_panel_cengiz, firm_panel, by = c("burnr", "erhebja"))
firm_panel <- subset(firm_panel, !is.na(P50)) #28'946 obs -> each firm is twice in the data

firm_chars <- LSE_08_18_firm_panel %>% select(c(erhebja, burnr, firm_size, cba, noga)) 
firm_chars$avs_nb <- NULL
firm_chars <- firm_chars %>% distinct()

firm_panel_chars <- left_join(firm_panel, firm_chars, by = c("erhebja", "burnr"))
saveRDS(firm_panel_chars, file = "output/data/firm_panel_MEM_watch.rds")


summary(firm_panel_chars$mean_wage) #NAs 1624
sum(!is.na(firm_panel_chars$mean_wage)) #27322
## ---------------------------
#merge to LSE-data
firm_panel <- left_join(LSE_08_18_firm_panel, firm_panel, by = c("burnr", "erhebja", "above")) #one firm has many workers

saveRDS(firm_panel, file = "output/data/2d_firm_panel.rds")

summary(firm_panel$gap_exposure_cons_std[firm_panel$erhebja==2012])
summary(firm_panel$gap_exposure_cons_std[firm_panel$erhebja==2008])

