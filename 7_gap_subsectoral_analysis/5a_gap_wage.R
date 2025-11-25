## ---------------------------
##
## Master thesis
## 2) Aggregation to sub-sectoral level
## Author: Katja Hager
##
## Date Created: Sept 2025
##
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation

library(lifecycle)
library(readxl)
library(tidyverse) #ggplot2 in here
library(data.table)
library(dplyr)
library(readr)
library(Hmisc)
library(stargazer)
library(reshape2)
library(grid)
library(gridExtra)
library(sandwich)
library(lmtest)
library(broom)
library(stringr)
library(estimatr)
library(fixest)
theme_set(theme_minimal())

## ---------------------------
#load file
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds") 

## ---------------------------
#1) Aggregate gap measure to sub-sector 
#(not the standardized ones, but wouldn't matter since we standardize WITHIN a year)
aggregate_gap_noga <- LSE_08_18 %>%
  filter(erhebja %in% c(2012) & mem==1) %>%
  group_by(noga) %>% summarise(
    gap_exposure = wtd.mean(gap_exposure_cons[!is.na(gap_exposure_cons)], 
                            weights = gewicht[!is.na(gap_exposure_cons)]),
    erhebja=2012,
    .groups = "drop") 

sd(aggregate_gap_noga$gap_exposure)


#standardize
aggregate_gap_noga <- aggregate_gap_noga %>%
  mutate(
    gap_exposure_noga = (gap_exposure - ave(gap_exposure, na.rm = TRUE)) / 
      (sd(gap_exposure, na.rm = TRUE))) %>%
  select(-c(erhebja))

summary(aggregate_gap_noga$gap_exposure_noga)
sd(aggregate_gap_noga$gap_exposure_noga)

ggplot(aggregate_gap_noga, aes(x = gap_exposure_noga)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1), bins = 60) + 
  labs(x = "Gap (standardized)", y = "#Sub-sectors")
ggsave("output/grgap_distr_noga_12.pdf", width = 18, height = 6, unit = "cm")

LSE_08_18 <- left_join(LSE_08_18, aggregate_gap_noga %>% select(gap_exposure_noga, noga), 
                       by = c("noga"))

## ---------------------------
#3) Y = yearly wage growth by sub-sectors
#yearly noga-panel
all_noga <- unique(LSE_08_18$noga[LSE_08_18$mem==1])
all_years <- unique(LSE_08_18$erhebja)
noga_panel <- expand_grid(noga = all_noga, erhebja = all_years) #empty

#yearly wage statistics -> sub-sectoral perspective. 
wages_noga <- LSE_08_18 %>%
  filter(mem==1) %>%
  mutate(p50_yearly = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE)) %>%
  group_by(erhebja, noga) %>% summarise(
    mean_wage = wtd.mean(real_wage[real_wage<p50_yearly], weights = gewicht[real_wage<p50_yearly]),
    p20 = wtd.quantile(real_wage, weights = gewicht, probs = 0.2, na.rm = TRUE),
    p15 = wtd.quantile(real_wage, weights = gewicht, probs = 0.15, na.rm = TRUE),
    p05 = wtd.quantile(real_wage, weights = gewicht, probs = 0.05, na.rm = TRUE),
    p10 = wtd.quantile(real_wage, weights = gewicht, probs = 0.1, na.rm = TRUE),
    p07 = wtd.quantile(real_wage, weights = gewicht, probs = 0.07, na.rm = TRUE),
    p08 = wtd.quantile(real_wage, weights = gewicht, probs = 0.08, na.rm = TRUE), 
    p09 = wtd.quantile(real_wage, weights = gewicht, probs = 0.09, na.rm = TRUE),
    p50 = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE),
    p40 = wtd.quantile(real_wage, weights = gewicht, probs = 0.4, na.rm = TRUE),
    p30 = wtd.quantile(real_wage, weights = gewicht, probs = 0.3, na.rm = TRUE),
    p80 = wtd.quantile(real_wage, weights = gewicht, probs = 0.8, na.rm = TRUE),
    noga_size = sum(gewicht, na.rm = TRUE),
    .groups = "drop" ) 

noga_panel <-left_join(noga_panel, wages_noga, by = c("erhebja", "noga"))

noga_panel <- noga_panel %>%
  group_by(noga) %>% arrange(erhebja) %>% mutate(    # Wage changes
    mean_wage_change_12 = mean_wage/mean_wage[erhebja==2012] -1,
    mean_wage_change_pct = mean_wage/lag(mean_wage) -1,
    p20_change_pct = (p20 / lag(p20) - 1) ,
    p10_change_pct = (p10 / lag(p10) - 1),
    p05_change_pct = (p05 / lag(p05) - 1),
    p07_change_pct = (p07 / lag(p07) - 1),
    p08_change_pct = (p08 / lag(p08) - 1),
    p09_change_pct = (p09 / lag(p09) - 1),
    p30_change_pct = (p30 / lag(p30) - 1),
    p40_change_pct = (p40 / lag(p40) - 1),
    p50_change_pct = (p50 / lag(p50) - 1),
    p80_change_pct = (p80 / lag(p80) - 1),
    p05_change_12 = (p05 / p05[erhebja==2012] - 1),
    p10_change_12 = (p10 / p10[erhebja==2012] - 1),
    p15_change_12 = (p15 / p15[erhebja==2012] - 1),
    p20_change_12 = (p20 / p20[erhebja==2012] - 1),
    p30_change_12 = (p30 / p30[erhebja==2012] - 1),
    p40_change_12 = (p40 / p40[erhebja==2012] - 1),
    p50_change_12 = (p50 / p50[erhebja==2012] - 1),
    p80_change_12 = (p80 / p80[erhebja==2012] - 1) )

LSE_08_18 <- left_join(LSE_08_18, noga_panel, by = c("noga", "erhebja"))
saveRDS(LSE_08_18, file = "output/data/5a_noga_analysis.rds")


## ---------------------------
#Derenoncourt (level of wages, and wage change)
#1) log(mean_wage)
derenoncourt <- feols(
  log(mean_wage) ~ i(erhebja, gap_exposure_noga, ref = 2012) | noga +  erhebja + msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = LSE_08_18)

etable(derenoncourt)

coefs_derenoncourt <- tidy(derenoncourt) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_exposure_noga") %>%as.numeric()) %>%
  filter(str_detect(term, ".*gap_exposure_noga")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012)) %>%
  mutate(var = "mean_wage")

ggplot(coefs_derenoncourt, aes(x = year, y = estimate)) + #, colour = var, shape = var
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(1)) +
  geom_hline(yintercept=0, linetype = "dashed")+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))
ggsave("output/graph/ES_derenoncourt_noga.pdf", width = 18, height = 5, unit = "cm")

#wage growth relative to 2012
derenoncourt_growth <- feols(
  mean_wage_change_12 ~ i(erhebja, gap_exposure_noga, ref = 2012) | noga +  erhebja + msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = LSE_08_18)
coefs_derenoncourt_growth <- tidy(derenoncourt_growth) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_exposure_noga") %>%as.numeric()) %>% 
  filter(str_detect(term, ".*gap_exposure_noga")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012)) %>%
  mutate(var = "wage_growth")
ggplot(coefs_derenoncourt_growth, aes(x = year, y = estimate)) + #, colour = var, shape = var
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate") +
  geom_hline(yintercept=0, linetype = "dashed")+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))
ggsave("output/graph/ES_derenoncourt_wagegrowth_noga.pdf", width = 18, height = 5, unit = "cm")

#wage growth yearly
derenoncourt_growth <- feols(
  mean_wage_change_pct ~ i(erhebja, gap_exposure_noga, ref = 2012) | noga +  erhebja + msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = LSE_08_18)
coefs_derenoncourt_growth <- tidy(derenoncourt_growth) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_exposure_noga") %>%as.numeric()) %>% 
  filter(str_detect(term, ".*gap_exposure_noga")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012)) %>%
  mutate(var = "wage_growth")
ggplot(coefs_derenoncourt_growth, aes(x = year, y = estimate)) + #, colour = var, shape = var
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate") +
  geom_hline(yintercept=0, linetype = "dashed")+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))
ggsave("output/graph/ES_derenoncourt_wagegrowth_yearly_noga.pdf", width = 18, height = 5, unit = "cm")

#wage percentiles
derenoncourt_percentiles <- feols(
  c(log(p10), log(p20), log(p50), log(p80)) ~ 
    i(erhebja, gap_exposure_noga, ref = 2012) | noga + erhebja+ msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = LSE_08_18)
coefs_percentiles <- coef(derenoncourt_percentiles) %>% mutate(var = "estimate")
se_percentiles   <- se(derenoncourt_percentiles) %>% mutate(var = "SE")

etable(derenoncourt_percentiles)

percentiles_df <- rbind(coefs_percentiles, se_percentiles) %>% select(-c("id"))  %>% 
  pivot_longer(cols = -c(lhs, var), names_to = "interaction_var", values_to = "value") %>%
  pivot_wider(names_from = var, values_from = value ) %>%
  mutate(year = str_remove_all(interaction_var, "erhebja::|:gap_exposure_noga") %>% as.numeric()) %>% 
  dplyr::bind_rows(
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p10)"),
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p20)"),
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p50)"),
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p80)")    )

ggplot(percentiles_df, aes(x = year, y = estimate, colour = lhs, shape = lhs)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = NULL,shape = NULL) +
  geom_hline(yintercept=0, linetype = "dashed")+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(4)) +  
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))
ggsave("output/graph/ES_derenoncourt_FFE_percentiles_noga.pdf", width = 18, height = 6, unit = "cm")

#percentile growth
derenoncourt_percentile_growth <- feols(
  c(log(p10_change_pct),log(p20_change_pct), log(p50_change_pct), log(p80_change_pct)) ~ 
    i(erhebja, gap_exposure_noga, ref = 2012) | noga + erhebja+ msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = LSE_08_18)
coefs_percentiles_growth <- coef(derenoncourt_percentile_growth) %>% mutate(var = "estimate")
se_percentiles_growth   <- se(derenoncourt_percentile_growth) %>% mutate(var = "SE")

percentiles_growth_df <- rbind(coefs_percentiles_growth, se_percentiles_growth) %>% select(-c("id"))  %>% 
  pivot_longer(cols = -c(lhs, var), names_to = "interaction_var", values_to = "value") %>%
  pivot_wider(names_from = var, values_from = value ) %>%
  mutate(year = str_remove_all(interaction_var, "erhebja::|:gap_exposure_noga") %>% as.numeric())%>% 
  dplyr::bind_rows(
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p10_change_pct)"),
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p20_change_pct)"),
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p50_change_pct)"),
    tibble(interaction_var = "baseline",estimate = 0,SE = 0, year = 2012, lhs="log(p80_change_pct)")    )

ggplot(percentiles_growth_df, aes(x = year, y = estimate, colour = lhs, shape = lhs)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = NULL,shape = NULL) +
  geom_hline(yintercept=0, linetype = "dashed")+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(4)) +  
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))
ggsave("output/graph/ES_derenoncourt_FFE_percentiles_growth_noga.pdf", width = 18, height = 6, unit = "cm")
