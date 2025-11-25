## ---------------------------
##
## Master thesis
## Wage regressions -> wage growth
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
library(sandwich)
library(lmtest)
library(broom)
library(stringr)
library(estimatr)
library(fixest)
theme_set(theme_minimal())
## ---------------------------
#time and individual FE
worker_panel <- readRDS(file = "output/data/2d_worker_panel.rds")
worker_panel$gap_2012_std
worker_panel$dist_MW_12

sd(worker_panel$gap_exposure_cons[firm_panel$erhebja==2012], na.rm = TRUE)
mean(worker_panel$gap_exposure_cons[firm_panel$erhebja==2012], na.rm = TRUE)

## ---------------------------
#x: GAP -> we have no way to check pre-trends of wage GROWTH

#wage_change_nominal_12
wage_change_nominal_12 <- feols(
  wage_change_nominal_12 ~ i(erhebja, gap_2012_std, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel[worker_panel$dist_MW_12<=300, ])
coefs_wage_change_nominal_12 <- tidy(wage_change_nominal_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_2012_std") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*gap_2012_std"))
ggplot(coefs_wage_change_nominal_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))
ggsave("output/graph/ES_WFE_wagegrowth.pdf", width = 18, height = 6, unit = "cm")

#income (blimok)
blimok_change_12 <- feols(
  blimok_change_12 ~ i(erhebja, gap_2012_std, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel[worker_panel$dist_MW_12<=300, ])
coefs_blimok_change_12 <- tidy(blimok_change_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_2012_std") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*gap_2012_std"))
ggplot(coefs_blimok_change_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI") +
  scale_x_continuous(breaks = c(2014, 2016, 2018))

#base wage without any Zulagen
wage_change_base_12 <- feols(
  wage_change_base_12 ~ i(erhebja, gap_2012_std, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel[ worker_panel$dist_MW_12<=300, ])
coefs_wage_change_base_12 <- tidy(wage_change_base_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_2012_std") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*gap_2012_std"))
ggplot(coefs_wage_change_base_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))

#change in employment
empl_change_12<- feols(
  empl_change_12 ~ i(erhebja, gap_2012_std, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel[ worker_panel$dist_MW_12<300, ])
coefs_empl_change_12 <- tidy(empl_change_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_2012_std") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*gap_2012_std"))
ggplot(coefs_empl_change_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = " ",shape = " ") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))

## ---------------------------
#x: dist to MW

#wage_change_nominal_12
wage_change_nominal_12 <- feols(
  wage_change_nominal_12 ~ i(erhebja, dist_MW_12, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel)
coefs_wage_change_nominal_12 <- tidy(wage_change_nominal_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:dist_MW_12") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*dist_MW_12"))
ggplot(coefs_wage_change_nominal_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = " ",shape = " ") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))
ggsave("output/graph/ES_WFE_wagegrowth_distMW.pdf", width = 18, height = 6, unit = "cm")

#income (blimok)
blimok_change_12 <- feols(
  blimok_change_12 ~ i(erhebja, dist_MW_12, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel)
coefs_blimok_change_12 <- tidy(blimok_change_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:dist_MW_12") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*dist_MW_12"))
ggplot(coefs_blimok_change_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = " ",shape = " ") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))

#base wage without any Zulagen
wage_change_base_12 <- feols(
  wage_change_base_12 ~ i(erhebja, dist_MW_12, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel)
coefs_wage_change_base_12 <- tidy(wage_change_base_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:dist_MW_12") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*dist_MW_12"))
ggplot(coefs_wage_change_base_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))

#change in employment
empl_change_12<- feols(
  empl_change_12 ~ i(erhebja, dist_MW_12, ref = 2012) | avs_nb +  erhebja, 
  weights = ~gewicht, cluster = ~burnr, data = worker_panel)
coefs_empl_change_12 <- tidy(empl_change_12) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:dist_MW_12") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*dist_MW_12"))
ggplot(coefs_empl_change_12, aes(x = year, y = estimate)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = " ",shape = " ") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))
