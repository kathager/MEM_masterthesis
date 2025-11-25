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
setFixest_dict(c(wage_region = "Wage Region", erhebja = "Year", mem = "MEM", 
                 noga = "Sub-sector", msreg_an = "Labour Market",
                 arbkto = "Canton", mean_wage = "Mean Firm Wage",
                 real_wage = "Real Wage (gross, FTE)",
                 burnr = "Firm", avs_nb = "Worker",
                 main_MEM = "MEM", natkat2 = "Nationality",
                 geschle = "Gender", transition_2016 = "Transition until 2016",
                 gap_treat = "Gap"))

sector_diff <- function(){
  list(
  geom_point(size = 1.5,position = position_dodge(width = 0.90)),
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  width = 0.4, position = position_dodge(width = 0.90)),
    labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL),
    scale_colour_manual(values = scales::viridis_pal(option = "D")(3)),
    scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)),
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") )}

#cross-sectional worker data
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")
LSE_08_18$main_MEM <- ifelse(LSE_08_18$main_sector == "MEM", 1, 0)
worker_data_lowwage <- LSE_08_18 %>%
  group_by(erhebja, sector) %>%
  mutate(P50 = wtd.quantile(real_wage, weights = gewicht, p = 0.5)) %>%
  filter(real_wage <= P50)

#worker panel data
worker_panel <- readRDS( file = "output/data/2d_worker_panel.rds")
worker_panel$main_MEM <- ifelse(worker_panel$main_sector == "MEM", 1, 0)

worker_panel_lowwage <- worker_panel %>%
  group_by(erhebja, sector) %>%
  mutate(P50 = wtd.quantile(real_wage, weights = gewicht, p = 0.5)) %>%
  filter(real_wage <= P50)

## ----------------------------------
### OUTCOME 1: Worker's wage, depending on region FE
##model 1: no regional FE
base_model <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga,
  cluster = ~noga + gr, weight = ~gewicht,
  data = worker_data_lowwage )

##model 2: arbkto FE
cantonal_model <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + arbkto,
  cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

##model 3: msreg_an FE
labour_region <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an,
  cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_base_model <- tidy(base_model) %>% mutate(var="No Region FE")
coefs_cantonal_model <- tidy(cantonal_model) %>% mutate(var="Cantonal FE")
coefs_labour_region <- tidy(labour_region) %>% mutate(var="Labour Market FE")
coefs_workers <- rbind(coefs_base_model,coefs_cantonal_model,coefs_labour_region) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM") %>% as.numeric())  %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="No Region FE"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="Cantonal FE"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="Labour Market FE"))

ggplot(coefs_workers[coefs_workers$var== "Labour Market FE", ], 
       aes(x = year, y = estimate)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.3, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL)+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
ggsave("output/graph/ES_workers_wage_by_sector.pdf", width = 9, height = 3, unit = "cm")

#for appendix
ggplot(coefs_workers, 
       aes(x = year, y = estimate, colour = var, fill = var, shape = var)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
ggsave("output/graph/ES_workers_wage_byFEmodel_by_sector.pdf", width = 18, height = 4, unit = "cm")

###adding person FE
#worker level: real_wage -> no pretrends possible to be
#from 2012 data, only workers with AHV-Nr. 

##model 1: no regional FE
base_model_panel <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + noga,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_lowwage )

##model 2: arbkto FE
cantonal_model_panel <- feols(
  log(real_wage) ~ i(erhebja,main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + arbkto + noga,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_lowwage )

##model 3: msreg_an FE
labour_region_panel <- feols(
  log(real_wage) ~ i(erhebja,main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + msreg_an + noga,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_lowwage )

coefs_base_model_panel <- tidy(base_model_panel) %>% mutate(var="No Region FE")
coefs_cantonal_model_panel <- tidy(cantonal_model_panel) %>% mutate(var="Cantonal FE")
coefs_labour_region_panel <- tidy(labour_region_panel) %>% mutate(var="Labour Market FE")
coefs_workers_panel <- rbind(coefs_base_model_panel,coefs_cantonal_model_panel,coefs_labour_region_panel) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = as.numeric(str_extract_all(term, "\\d+")))
coefs_workers_panel <- coefs_workers_panel %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="No Region FE"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="Cantonal FE"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="Labour Market FE"))

ggplot(coefs_workers_panel[coefs_workers_panel$var=="Labour Market FE", ], 
       aes(x = year, y = estimate)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
ggsave("output/graph/ES_workers_wage_FE_panel_by_sector.pdf", width = 18, height = 4, unit = "cm")

## ----------------------------------

#wage percentiles -> sector-wide, or sub-sector wide
#or: focus on workers in lowest Q10

worker_data_p10 <- LSE_08_18 %>%
  group_by(erhebja, sector) %>%
  mutate(P10 = wtd.quantile(real_wage, weights = gewicht, p = 0.1)) %>%
  filter(real_wage <= P10) %>%
  rename(low_wage = real_wage)
worker_panel_p10 <- worker_panel %>%
  group_by(erhebja, sector) %>%
  mutate(P10 = wtd.quantile(real_wage, weights = gewicht, p = 0.1)) %>%
  filter(real_wage <= P10) %>%
  rename(low_wage = real_wage)

### OUTCOME 1: Worker's wage, depending on region FE
##model 1: no regional FE
base_model_p10 <- feols(
  log(low_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga,
  cluster = ~noga + gr, weight = ~gewicht,
  data = worker_data_p10 )

##model 2: arbkto FE
cantonal_model_p10 <- feols(
  log(low_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + arbkto,
  cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_p10 )

##model 3: msreg_an FE
labour_region_p10 <- feols(
  log(low_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an,
  cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_p10 )

coefs_base_model_p10 <- tidy(base_model_p10) %>% mutate(var="No Region FE")
coefs_cantonal_model_p10 <- tidy(cantonal_model_p10) %>% mutate(var="Cantonal FE")
coefs_labour_region_p10 <- tidy(labour_region_p10) %>% mutate(var="Labour Market FE")
coefs_workers_p10 <- rbind(coefs_base_model_p10,coefs_cantonal_model_p10,coefs_labour_region_p10) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM") %>% as.numeric())  %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="No Region FE"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="Cantonal FE"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="Labour Market FE"))

ggplot(coefs_workers_p10[coefs_workers_p10$var== "Labour Market FE", ], 
       aes(x = year, y = estimate)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.3, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL)+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
ggsave("output/graph/ES_workers_wage_by_sector_p10.pdf", width = 9, height = 3, unit = "cm")

#for appendix
ggplot(coefs_workers_p10, 
       aes(x = year, y = estimate, colour = var, fill = var, shape = var)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

###adding person FE; from 2012, only workers with AHV-Nr. 
##model 3: msreg_an FE
labour_region_panel_p10 <- feols(
  log(low_wage) ~ i(erhebja,main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + msreg_an + noga,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_p10 )

coefs_labour_region_panel_p10 <- tidy(labour_region_panel_p10)  %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = as.numeric(str_extract_all(term, "\\d+")))  %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012))

ggplot(coefs_labour_region_panel_p10, aes(x = year, y = estimate)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
ggsave("output/graph/ES_workers_wage_FE_panel_by_sector_p10.pdf", width = 18, height = 4, unit = "cm")

#alltogether regression table
etable(base_model, labour_region, labour_region_panel, 
       base_model_p10, labour_region_p10, labour_region_panel_p10,
       keep = "MEM", tex = TRUE, digits = 3,  digits.stats = 3)

## -------------------------------------
##Heterogeneity
worker_data_lowwage$numeric_C <- ifelse(worker_data_lowwage$wage_region=="C", 1, 0)
worker_data_lowwage$numeric_cba <- ifelse(worker_data_lowwage$cba=="CBA covered", 1, 0)
#worker_data_lowwage$transition_2016
worker_data_lowwage$numeric_male <- ifelse(worker_data_lowwage$geschle=="men", 1, 0)
worker_data_lowwage$numeric_resident <- ifelse(worker_data_lowwage$natkat2=="CH/C-permit", 1, 0)
worker_data_lowwage$numeric_young <- ifelse(worker_data_lowwage$age=="Young", 1, 0)
worker_data_lowwage$numeric_new <- ifelse(worker_data_lowwage$tenure=="Short", 1, 0)

##CBA
labour_region_cba <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    i(erhebja, main_MEM,  ref = 2012):numeric_cba | erhebja + noga + msreg_an +
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja,
  cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_labour_region_cba <- tidy(labour_region_cba) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM|:numeric_cba") %>%as.numeric())  %>%
  mutate(interaction_effect = str_detect(term, "numeric_cba")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>% 
  mutate(interaction = ifelse(interaction_effect==TRUE, "diff", "base"), 
         heterogeneity = "CBA")

ggplot(coefs_labour_region_cba, aes(x = year, y = estimate, colour = interaction)) +
  sector_diff() + theme(legend.position = "none")
ggsave("output/graph/ES_workers_wage_by_sector_CBA.pdf", width = 9, height = 3, unit = "cm")


##transition period
labour_region_transition_2016 <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    i(erhebja, main_MEM,  ref = 2012):transition_2016 | erhebja + noga + msreg_an +
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja,
  cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_labour_region_transition_2016 <- tidy(labour_region_transition_2016) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM|:transition_2016") %>%as.numeric())  %>%
  mutate(interaction_effect = str_detect(term, "transition_2016")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>% 
  mutate(interaction = ifelse(interaction_effect==TRUE, "diff", "base"), 
         heterogeneity = "transition_2016")

ggplot(coefs_labour_region_transition_2016, aes(x = year, y = estimate, colour = interaction)) +
  sector_diff() + theme(legend.position = "none")
ggsave("output/graph/ES_workers_wage_by_sector_transition.pdf", width = 9, height = 4, unit = "cm")

##wage region
labour_region_numeric_C <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    i(erhebja, main_MEM,  ref = 2012):numeric_C | erhebja + noga + msreg_an +
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja,
  cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_labour_region_numeric_C <- tidy(labour_region_numeric_C) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM|:numeric_C") %>%as.numeric())  %>%
  mutate(interaction_effect = str_detect(term, "numeric_C")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>% 
  mutate(interaction = ifelse(interaction_effect==TRUE, "diff", "base"), 
         heterogeneity = "numeric_C")

ggplot(coefs_labour_region_numeric_C, aes(x = year, y = estimate, colour = interaction)) +
  sector_diff() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text=element_text(size=10))
ggsave("output/graph/ES_workers_wage_by_sector_numeric_C.pdf", width = 9, height = 4, unit = "cm")


##numeric_male
labour_region_numeric_male <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    i(erhebja, main_MEM,  ref = 2012):numeric_male | erhebja + noga + msreg_an +
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja, cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_labour_region_numeric_male <- tidy(labour_region_numeric_male) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM|:numeric_male") %>%as.numeric())  %>%
  mutate(interaction_effect = str_detect(term, "numeric_male")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>% 
  mutate(interaction = ifelse(interaction_effect==TRUE, "differential", "base"), 
         heterogeneity = "numeric_male")

ggplot(coefs_labour_region_numeric_male, aes(x = year, y = estimate, colour = interaction)) +
  sector_diff()+
  theme(legend.position = "none")
ggsave("output/graph/ES_workers_wage_by_sector_numeric_male.pdf", width = 9, height = 4, unit = "cm")

##numeric_resident
labour_region_numeric_resident <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    i(erhebja, main_MEM,  ref = 2012):numeric_resident | erhebja + noga + msreg_an +
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja, cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_labour_region_numeric_resident <- tidy(labour_region_numeric_resident) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM|:numeric_resident") %>%as.numeric())  %>%
  mutate(interaction_effect = str_detect(term, "numeric_resident")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>% 
  mutate(interaction = ifelse(interaction_effect==TRUE, "differential", "base"), 
         heterogeneity = "numeric_resident")

ggplot(coefs_labour_region_numeric_resident, aes(x = year, y = estimate, colour = interaction)) +
  sector_diff()+  theme(legend.position = "none")
ggsave("output/graph/ES_workers_wage_by_sector_numeric_resident.pdf", width = 9, height = 4, unit = "cm")

##numeric_young
labour_region_numeric_young <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    i(erhebja, main_MEM,  ref = 2012):numeric_young | erhebja + noga + msreg_an +
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja, cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_labour_region_numeric_young <- tidy(labour_region_numeric_young) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM|:numeric_young") %>%as.numeric())  %>%
  mutate(interaction_effect = str_detect(term, "numeric_young")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>% 
  mutate(interaction = ifelse(interaction_effect==TRUE, "differential", "base"), 
         heterogeneity = "numeric_young")

ggplot(coefs_labour_region_numeric_young, aes(x = year, y = estimate, colour = interaction)) +
  sector_diff()+  theme(legend.position = "none")
ggsave("output/graph/ES_workers_wage_by_sector_numeric_young.pdf", width = 9, height = 4, unit = "cm")

##numeric_new
labour_region_numeric_new <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    i(erhebja, main_MEM,  ref = 2012):numeric_new | erhebja + noga + msreg_an +
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja, cluster = ~noga + gr , weight = ~gewicht,
  data = worker_data_lowwage )

coefs_labour_region_numeric_new <- tidy(labour_region_numeric_new) %>%
  mutate(year = str_remove_all(term, "erhebja::|:main_MEM|:numeric_new") %>%as.numeric())  %>%
  mutate(interaction_effect = str_detect(term, "numeric_new")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>% 
  mutate(interaction = ifelse(interaction_effect==TRUE, "differential", "base"), 
         heterogeneity = "numeric_new")

ggplot(coefs_labour_region_numeric_new, aes(x = year, y = estimate, colour = interaction)) +
  sector_diff()+
  theme(legend.position = "inside",
        legend.position.inside = c(0, 0),
        legend.justification.inside = c(0, 0),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text=element_text(size=10))
ggsave("output/graph/ES_workers_wage_by_sector_numeric_new.pdf", width = 9, height = 4, unit = "cm")

#
## -------------- some old stuff --------------------

labour_region_nationality_all <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an,
  cluster = ~noga + gr , weight = ~gewicht, split = ~natkat2,
  data = worker_data_lowwage )
labour_region_allgender <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an,
  cluster = ~noga + gr , weight = ~gewicht, split = ~geschle, 
  data = worker_data_lowwage)
labour_region_nationality_all_FE <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(geschle) + factor(ausbild) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an + avs_nb,
  cluster = ~noga + gr , weight = ~gewicht, split = ~natkat2,
  data = worker_panel_lowwage )
labour_region_allgender_FE <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an + avs_nb,
  cluster = ~noga + gr , weight = ~gewicht, split = ~geschle, 
  data = worker_panel_lowwage)

etable(labour_region_allgender_FE, labour_region_nationality_all_FE,
       keep = "MEM", tex = TRUE,digits = 3,  digits.stats = 3) 

etable(labour_region_nationality_all_FE,
       keep = "MEM", tex = TRUE,digits = 3,  digits.stats = 3)



#with PANEL: worker FE: workers wage by transition period
workers_2016 <- feols(
  log(real_wage) ~ i(erhebja,main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + msreg_an,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_lowwage[worker_panel_lowwage$transition_2016==1, ] )
workers_2018 <- feols(
  log(real_wage) ~ i(erhebja,main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + msreg_an,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_lowwage[worker_panel_lowwage$transition_2016==0, ] )

coefs_2016_workers <- tidy(workers_2016) %>% mutate(var="2016")
coefs_2018_workers <- tidy(workers_2018) %>% mutate(var="2018")
coefs_workers_transition <- rbind(coefs_2016_workers,coefs_2018_workers) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = as.numeric(str_extract_all(term, "\\d+")))  %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="2016"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="2018"))
ggplot(coefs_workers_transition, aes(x = year, y = estimate, colour = var)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
ggsave("output/graph/ES_workers_wage_FE_panel_bytransition_by_sector.pdf", width = 18, height = 4, unit = "cm")

#regression table
labour_region_transition <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an,
  cluster = ~noga + gr , weight = ~gewicht, split = ~transition_2016, 
  data = worker_data_lowwage )
labour_region_transition_FE <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an +avs_nb,
  cluster = ~noga + gr , weight = ~gewicht, split = ~transition_2016, 
  data = worker_panel_lowwage )

## -------------------------------------


#add panel
## differentiate by CBA coverage

labour_region_cba_FE <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an +avs_nb,
  cluster = ~noga + gr , weight = ~gewicht, split = ~cba, 
  data = worker_panel_lowwage)
#workers wage by transition period
workers_cba <- feols(
  log(real_wage) ~ i(erhebja,main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + msreg_an,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_lowwage[worker_panel_lowwage$cba=="CBA covered", ] )
workers_nocba <- feols(
  log(real_wage) ~ i(erhebja,main_MEM,  ref = 2012) + 
    factor(firm_size) | erhebja + avs_nb + msreg_an,
  cluster = ~noga + msreg_an, weight = ~gewicht,
  data = worker_panel_lowwage[worker_panel_lowwage$cba=="Not covered", ] )

coefs_workers_cba <- tidy(workers_cba) %>% mutate(var="CBA covered")
coefs_workers_nocba <- tidy(workers_nocba) %>% mutate(var="Not covered")
coefs_workers_coverage <- rbind(coefs_workers_cba,coefs_workers_nocba) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = as.numeric(str_extract_all(term, "\\d+")))  %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="CBA covered"),
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="Not covered"))
ggplot(coefs_workers_coverage, aes(x = year, y = estimate, colour = var)) +
  geom_point(size = 1.5,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "estimate",  fill = NULL, colour = NULL, shape = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
ggsave("output/graph/ES_workers_wage_FE_panel_bycba_by_sector.pdf", width = 18, height = 4, unit = "cm")


#regression table
labour_region_cba <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an,
  cluster = ~noga + gr , weight = ~gewicht, split = ~cba, 
  data = worker_data_lowwage )
labour_region_cba_FE <- feols(
  log(real_wage) ~ i(erhebja, main_MEM,  ref = 2012) + 
    factor(firm_size) + factor(ausbild) + factor(natkat) + factor(berufst) + 
    alter + dienstja | erhebja + noga + msreg_an +avs_nb,
  cluster = ~noga + gr , weight = ~gewicht, split = ~cba, 
  data = worker_panel_lowwage )
etable(labour_region_cba, labour_region_cba_FE, 
       keep = "MEM", tex = TRUE, digits = 3,  digits.stats = 3)


plot_defaults_2 <- function() {
  list(
    scale_colour_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_y_continuous(labels = percent),
    scale_x_continuous(labels = percent),
    geom_vline(xintercept = 0, linetype = "dashed"),
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))   ) }

## --------------------------- wage growth
#compare workers' wage growth relative to dist_to_MW
#dist_to_MW -> in percent. (wage-MW)/MW = how many percent of the MW is my wage off
#If I earn 330.- less than 3'300, I am 0.1*100 = 10% away from the MW

worker_panel <- readRDS(file = "output/data/2d_worker_panel.rds")
worker_panel$sector_2012 <- ifelse(worker_panel$mem_2012 ==1, "MEM", "Watch")
table(worker_panel$sector_2012)

data_plot <- worker_panel[!is.na(worker_panel$wage_change_nominal_12) &
                            worker_panel$erhebja>2012 &
                            !is.na(worker_panel$dist_MW_12) & 
                            !is.na(worker_panel$sector_2012) &
                            worker_panel$dist_MW_12_rel>=-1, ]
worker_panel <- data_plot[data_plot$mem_2012==1, ]

## ----------------- wage growth ---------------
worker_panel$sector_2012 <- ifelse(worker_panel$mem_2012 ==1, "MEM", "Watch")

data_plot <- worker_panel[!is.na(worker_panel$wage_change_nominal_12) &
                            worker_panel$erhebja>2012 &
                            !is.na(worker_panel$dist_MW_12) & 
                            !is.na(worker_panel$sector_2012) &
                            worker_panel$dist_MW_12_rel>=-1, ]

sum(!is.na(data_plot$wage_change_nominal_12)) #164369
sum(data_plot$wage_change_nominal_12>2) #483

#differentiate by sector
#remove 483 outliers which have a wage change >2
ggplot(data = data_plot[data_plot$dist_MW_12<=10 & data_plot$wage_change_nominal_12<2, ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  geom_point(size = 0.05) + geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Relative distance to minimum wage in 2012 (%)",y = "Wage change", 
       colour = NULL, fill = NULL) +
  plot_defaults_2() +
  facet_grid(~erhebja, scales = "free_y")
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_lm_raw.pdf", width = 18, height = 4, unit = "cm")


ggplot(data = data_plot[data_plot$dist_MW_12<=600 & data_plot$wage_change_nominal_12<2, ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  geom_point(size = 0.1) + geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Relative distance to minimum wage in 2012 (%)",y = "Wage change", 
       colour = NULL, fill = NULL) +
  plot_defaults_2() +
  facet_grid(~erhebja, scales = "free_y")
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_lm_large.pdf", width = 18, height = 4, unit = "cm")


ggplot(data = data_plot[data_plot$dist_MW_12<=10 & 
                          data_plot$wage_change_nominal_12<5 &
                          data_plot$transition_2016 == 1& data_plot$wage_change_nominal_12<2, ],
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  geom_point(size = 0.1) + geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Relative distance to minimum wage in 2012 (%)",y = "Wage change", 
       colour = "Sector", fill = "Sector") +
  plot_defaults_2() +
  facet_grid(~erhebja, scales = "free_y")
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_transition_16.pdf", width = 18, height = 4, unit = "cm")

ggplot(data = data_plot[data_plot$dist_MW_12<=10 & 
                          data_plot$wage_change_nominal_12<5 & 
                          data_plot$transition_2016 == 0 & 
                          data_plot$wage_change_nominal_12<2, ],
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  geom_point(size = 0.1) + geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Relative distance to minimum wage in 2012 (%)",y = "Wage change", 
       colour = "Sector", fill = "Sector") +
  plot_defaults_2() +
  facet_grid(~erhebja, scales = "free_y")
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_transition_18.pdf", width = 18, height = 4, unit = "cm")
