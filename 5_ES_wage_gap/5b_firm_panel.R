## ---------------------------
##
## Master thesis
## Wage regressions
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

setFixest_dict(c(wage_region = "Wage Region", erhebja = "Year", mem = "MEM", 
                 noga = "Sub-sector", msreg_an = "Labour Market",
                 arbkto = "Canton", mean_wage = "Mean Firm Wage",
                 real_wage = "Real Wage (gross, FTE)",
                 burnr = "Firm", avs_nb = "Worker",
                 main_MEM = "MEM", natkat2 = "Nationality",
                 geschle = "Gender", transition_2016 = "Transition until 2016",
                 gap_treat = "Gap", gr = "Greater Region",
                 numeric_cba = "CBA-covered"))


ES_plots <- function() {
  list(
    geom_point(size = 1.5,position = position_dodge(width = 0.6)),
      geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                        ymax = estimate + 1.96*std.error),width = 0.3, position = position_dodge(width = 0.6)),
      labs(x = NULL, y = "Estimate", colour = NULL, fill = NULL),
      scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), expand = expansion(mult = 0.05)),
      geom_hline(yintercept = 0, linetype = "dashed", color = "black")  ) }

## ---------------------------
gap <- readRDS(file = "output/data/2c_gap_exposure_firm.rds")
sd(gap$gap_exposure_cons[gap$erhebja==2012])
mean(gap$gap_exposure_cons[gap$erhebja==2012], na.rm = TRUE)

## ---------------------------
firm_panel <- readRDS(file = "output/data/2d_firm_panel.rds") #2012 onwards

firm_panel$numeric_cba <- ifelse(firm_panel$cba=="CBA covered", 1, 0)
#firm_panel$transition_2016
firm_panel$numeric_male <- ifelse(firm_panel$geschle=="men", 1, 0)
firm_panel$numeric_resident <- ifelse(firm_panel$natkat2=="CH/C-permit", 1, 0)
firm_panel$numeric_young <- ifelse(firm_panel$age=="Young", 1, 0)
firm_panel$numeric_new <- ifelse(firm_panel$tenure=="Short", 1, 0)

firm_employment <- firm_panel %>%
  group_by(burnr, MW_bins_coarse, erhebja) %>% 
  summarise(
    employment_coarse = sum(gewibgrs, na.rm = TRUE),
    workers_coarse = sum(gewicht, na.rm = TRUE) )
firm_panel <- left_join(firm_panel, firm_employment, by = c("burnr", "erhebja", "MW_bins_coarse"))
table(firm_panel$MW_bins_coarse)

firm_employment <- firm_panel %>%
  group_by(burnr, MW_bins_coarsest, erhebja) %>% 
  summarise(
    employment_coarsest = sum(gewibgrs, na.rm = TRUE),
    workers_coarsest = sum(gewicht, na.rm = TRUE) )
firm_panel <- left_join(firm_panel, firm_employment, by = c("burnr", "erhebja", "MW_bins_coarsest"))
table(firm_panel$MW_bins_coarsest)

firm_sample_post <- firm_panel[firm_panel$erhebja>2010, ]
firm_sample_pre <- firm_panel[firm_panel$erhebja<2012, ]

#some checks
#cluster SEs: cluster = ~noga + gr + k_untgroe (stratification)
# Check cluster structure
cluster_structure <- firm_panel %>% filter(mem==1) %>%
  group_by(noga, gr) %>% summarise(n_firmsize = uniqueN(k_untgroe) )
sum(cluster_structure$n_firmsize==1)

## ---------------------------
#log mean wage
##sample split: before and after 2010
derenoncourt <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2012) | burnr+msreg_an +  erhebja, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post)
coefs_derenoncourt <- tidy(derenoncourt) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric(),
    var = "weighted") %>% filter(str_detect(term, ".*gap_treat"))
derenoncourt_pre <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2008) +
    i(erhebja, ref = 2008) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre)
coefs_derenoncourt_pre <- tidy(derenoncourt_pre) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate( year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric(),
    var = "weighted") %>% filter(str_detect(term, ".*gap_treat"))
derenoncourt_all_weighted <- rbind(coefs_derenoncourt, coefs_derenoncourt_pre) %>%
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="weighted"))

derenoncourt <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2012) | burnr+msreg_an +  erhebja, 
  cluster = ~noga + gr, data = firm_sample_post)
coefs_derenoncourt <- tidy(derenoncourt) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric(),
    var = "unweighted") %>% filter(str_detect(term, ".*gap_treat"))
derenoncourt_pre <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2008) +
    i(erhebja, ref = 2008) | burnr+msreg_an, 
  cluster = ~noga + gr,data = firm_sample_pre)
coefs_derenoncourt_pre <- tidy(derenoncourt_pre) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric(),
    var = "unweighted") %>%filter(str_detect(term, ".*gap_treat"))
derenoncourt_all_unweighted <- rbind(coefs_derenoncourt, coefs_derenoncourt_pre)%>%
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var="unweighted"))

ggplot(derenoncourt_all_weighted, aes(x = year, y = estimate)) +
  ES_plots()
ggsave("output/graph/ES_derenoncourt_FFE.pdf", width = 20, height = 3, unit = "cm")

derenoncourt_all_weight <- rbind(derenoncourt_all_unweighted, derenoncourt_all_weighted)
ggplot(derenoncourt_all_weight, aes(x = year, y = estimate, colour = var)) +
  ES_plots() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  theme(legend.position= "inside",
        legend.justification.inside = c(0, 1),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text=element_text(size=10))
ggsave("output/graph/ES_derenoncourt_FFE_weights.pdf", width = 20, height = 3, unit = "cm")

## -------------------------
#wage growth
all_firms <- unique(firm_sample_pre$burnr)
all_years <- unique(firm_sample_pre$erhebja)
firm_panel <- expand_grid(burnr = all_firms, erhebja = all_years) #empty
firm_sample_pre_growth <- left_join(firm_panel, firm_sample_pre,by = join_by(burnr, erhebja))
firm_sample_pre_growth <- firm_sample_pre_growth %>%
  group_by(burnr) %>% arrange(erhebja) %>%
  mutate(mean_wage_change_08 = 
           (mean_wage - mean_wage[erhebja == 2008])/mean_wage[erhebja == 2008])
sum(!is.na(firm_sample_pre_growth$mean_wage_change_08))

derenoncourt_growth <- feols(
  mean_wage_change_12 ~ i(erhebja, gap_treat, ref = 2012) | burnr+msreg_an +  erhebja, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post)
coefs_derenoncourt_growth <- tidy(derenoncourt_growth) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric()) %>% 
  filter(str_detect(term, ".*gap_treat"))
derenoncourt_pre_growth <- feols(
  mean_wage_change_08 ~ i(erhebja, gap_treat, ref = 2008) +
    i(erhebja, ref = 2008) | burnr+msreg_an, 
  weights = ~gewicht,, cluster = ~noga + gr,
  data = firm_sample_pre_growth)
coefs_derenoncourt_pre_growth <- tidy(derenoncourt_pre_growth) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric()) %>%
  filter(str_detect(term, ".*gap_treat"))
derenoncourt_all_growth <- rbind(coefs_derenoncourt_growth, coefs_derenoncourt_pre_growth)

ggplot(derenoncourt_all_growth, aes(x = year, y = estimate)) +
  ES_plots()
ggsave("output/graph/ES_derenoncourt_FFE_mean_wage_change.pdf", width = 20, height = 3, unit = "cm")


## -------------------------
##wage percentiles
derenoncourt_perc <- feols(
  c(log(P10), log(P20), log(P50), log(P80)) ~ 
    i(erhebja, gap_treat, ref = 2012) + i(erhebja, ref = 2012) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post)
coefs_percentiles <- coef(derenoncourt_perc) %>% mutate(var = "estimate")
se_percentiles   <- se(derenoncourt_perc) %>% mutate(var = "std.error")
percentiles_df <- rbind(coefs_percentiles, se_percentiles) %>% select(-c("id"))

derenoncourt_perc_pre <- feols(
  c(log(P10), log(P20), log(P50), log(P80)) ~ 
    i(erhebja, gap_treat, ref = 2008) + i(erhebja, ref = 2008) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre)
coefs_pre_percentiles <- coef(derenoncourt_perc_pre) %>% mutate(var = "estimate")
se_pre_percentiles   <- se(derenoncourt_perc_pre) %>% mutate(var = "std.error")
percentiles_pre_df <- rbind(coefs_pre_percentiles, se_pre_percentiles) %>% select(-c("id"))

percentiles_full_df <- full_join(percentiles_df, percentiles_pre_df, by = c("var", "lhs")) %>% 
  select(c(ends_with("treat"), lhs, var)) %>%
  pivot_longer(cols = -c(lhs, var), names_to = "interaction_var", values_to = "value") %>%
  pivot_wider(names_from = var, values_from = value ) %>%
  mutate(year = str_remove_all(interaction_var, "erhebja::|:gap_treat") %>% as.numeric()) %>%
  select(-c(interaction_var)) %>%
  dplyr::bind_rows(
    tibble(lhs = "log(P10)",estimate = 0,std.error = 0, year = 2012),
    tibble(lhs = "log(P20)",estimate = 0,std.error = 0, year = 2012),
    tibble(lhs = "log(P50)",estimate = 0,std.error = 0, year = 2012),
    tibble(lhs = "log(P80)",estimate = 0,std.error = 0, year = 2012)) %>%
  rename(outcome = lhs)

ggplot(percentiles_full_df, aes(x = year, y = estimate, colour = outcome)) +
  ES_plots() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(4)) 
ggsave("output/graph/ES_derenoncourt_FFE_percentiles.pdf", width = 20, height = 4, unit = "cm")

etable(derenoncourt, derenoncourt_perc,
       keep = "Gap", tex = TRUE, digits = 3,  digits.stats = 3)

## ---------------------------
#wage compression?
derenoncourt_var <- feols(
  coeff_var ~ i(erhebja, gap_treat, ref = 2012) | burnr+msreg_an +  erhebja, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post)
coefs_derenoncourt_var <- tidy(derenoncourt_var) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric(),
         var = "weighted") %>% filter(str_detect(term, ".*gap_treat"))
derenoncourt_pre_var <- feols(
  coeff_var ~ i(erhebja, gap_treat, ref = 2008) +
    i(erhebja, ref = 2008) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre)
coefs_derenoncourt_pre_var <- tidy(derenoncourt_pre_var) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate( year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric(),
          var = "weighted") %>% filter(str_detect(term, ".*gap_treat"))
derenoncourt_coeff_var <- rbind(coefs_derenoncourt_var, coefs_derenoncourt_pre_var) %>%
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, var = "weighted"))

ggplot(derenoncourt_coeff_var, aes(x = year, y = estimate)) +
  ES_plots()
ggsave("output/graph/ES_derenoncourt_FFE_var.pdf", width = 20, height = 3, unit = "cm")


#heterogeneity
#by CBA
derenoncourt_cba <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2012) +
    i(erhebja, gap_treat, ref = 2012):numeric_cba | burnr+msreg_an +  erhebja, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post)
coefs_derenoncourt_cba <- tidy(derenoncourt_cba) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat|:numeric_cba") %>%as.numeric()) 

derenoncourt_pre_cba <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2008) +
    i(erhebja, gap_treat, ref = 2008):numeric_cba  | burnr+msreg_an +erhebja, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre)
coefs_derenoncourt_pre_cba <- tidy(derenoncourt_pre_cba)%>%  
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat|:numeric_cba") %>%as.numeric()) 

derenoncourt_cba_df <- rbind(coefs_derenoncourt_cba, coefs_derenoncourt_pre_cba) %>%
  mutate(interaction_effect = str_detect(term, "numeric_cba")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012, 
           interaction_effect = TRUE))%>%
  mutate(heterogeneity = "CBA")

ggplot(derenoncourt_cba_df[derenoncourt_cba_df$interaction_effect == TRUE, ], 
       aes(x = year, y = estimate)) +  ES_plots() 
ggsave("output/graph/ES_derenoncourt_FFE_cba.pdf", width = 18, height = 6, unit = "cm")

etable(derenoncourt_pre_cba, derenoncourt_cba, tex = TRUE, digits = 3,  digits.stats = 3)

#firm_panel$transition_2016
derenoncourt_transition_2016 <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2012) +
    i(erhebja, gap_treat, ref = 2012):transition_2016 | burnr+msreg_an +  erhebja, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post)
coefs_derenoncourt_transition_2016 <- tidy(derenoncourt_transition_2016) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat|:transition_2016") %>%as.numeric()) 

derenoncourt_pre_transition_2016 <- feols(
  log(mean_wage) ~ i(erhebja, gap_treat, ref = 2008) +
    i(erhebja, gap_treat, ref = 2008):transition_2016  | burnr+msreg_an +erhebja, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre)
coefs_derenoncourt_pre_transition_2016 <- tidy(derenoncourt_pre_transition_2016)%>%  
  mutate(year = str_remove_all(term, "erhebja::|:gap_treat|:transition_2016") %>%as.numeric()) 

derenoncourt_transition_2016_df <- rbind(coefs_derenoncourt_transition_2016, coefs_derenoncourt_pre_transition_2016) %>%
  mutate(interaction_effect = str_detect(term, "transition_2016")) %>%
  dplyr::bind_rows(
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,
           year = 2012, interaction_effect = FALSE),
    tibble(term = "baseline", estimate = 0,std.error = 0,statistic = 0,p.value = 0,
           year = 2012, interaction_effect = TRUE)) %>%
  mutate(heterogeneity = "transition")

ggplot(derenoncourt_transition_2016_df[derenoncourt_transition_2016_df$interaction_effect == TRUE, ], 
       aes(x = year, y = estimate)) +  ES_plots() 
ggsave("output/graph/ES_derenoncourt_FFE_transition.pdf", width = 18, height = 6, unit = "cm")

##together
etable(derenoncourt_transition_2016, derenoncourt_cba, keep = c("numeric", "Transition"))

cba_2016_df <- rbind(derenoncourt_cba_df,derenoncourt_transition_2016_df)
ggplot(cba_2016_df[cba_2016_df$interaction_effect == TRUE, ], 
       aes(x = year, y = estimate, colour = heterogeneity)) +  
  ES_plots() + scale_colour_manual(values = scales::viridis_pal(option = "D")(3))









#

## --------------------------- DELETE since same as Cengiz -------------
#employment
#0) like Cengiz (above/blow MW jobs)
#post
derenoncourt_empl <- feols(
  (diff_empl) ~ i(erhebja, gap_treat, ref = 2012) + i(erhebja, ref = 2012) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post, split = ~above)
coefs_empl <- coef(derenoncourt_empl) %>% mutate(var = "estimate")
se_empl   <- se(derenoncourt_empl) %>% mutate(var = "SE")
empl_df <- rbind(coefs_empl, se_empl) %>% select(-c("id"))

#pre
derenoncourt_empl_pre <- feols(
  diff_empl ~ i(erhebja, gap_treat, ref = 2008) + i(erhebja, ref = 2008)| burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre, split = ~above)
coefs_pre_empl <- coef(derenoncourt_empl_pre) %>% mutate(var = "estimate")
se_pre_empl   <- se(derenoncourt_empl_pre) %>% mutate(var = "SE")
empl_pre_df <- rbind(coefs_pre_empl, se_pre_empl) %>% select(-c("id"))

#together
empl_full_df <- full_join(empl_df, empl_pre_df, by = c("sample.var", "sample", "var")) %>% 
  select(c(ends_with("treat"), sample, var)) %>%
  pivot_longer(cols = -c(sample, var), names_to = "interaction_var", values_to = "value") %>%
  pivot_wider(names_from = var, values_from = value ) %>%
  mutate(year = str_remove_all(interaction_var, "erhebja::|:gap_treat") %>% as.numeric()) 

ggplot(empl_full_df, aes(x = year, y = estimate, colour = sample, fill = sample)) +
  geom_point(size = 2) + geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE),
              alpha = 0.2, colour = NA) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = "Wage bin",fill = "Wage bin") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
ggsave("output/graph/ES_derenoncourt_cengiz_FFE_employment_change.pdf", 
       width = 18, height = 6, unit = "cm")

#1) very coarse wage bins
#post
derenoncourt_empl <- feols(
  log(employment_coarsest) ~ i(erhebja, gap_treat, ref = 2012) + i(erhebja, ref = 2012) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post, split = ~MW_bins_coarsest)
coefs_empl <- coef(derenoncourt_empl) %>% mutate(var = "estimate")
se_empl   <- se(derenoncourt_empl) %>% mutate(var = "SE")
empl_df <- rbind(coefs_empl, se_empl) %>% select(-c("id"))

#pre
derenoncourt_empl_pre <- feols(
  log(employment_coarsest) ~ i(erhebja, gap_treat, ref = 2008) + i(erhebja, ref = 2008)| burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre, split = ~MW_bins_coarsest)
coefs_pre_empl <- coef(derenoncourt_empl_pre) %>% mutate(var = "estimate")
se_pre_empl   <- se(derenoncourt_empl_pre) %>% mutate(var = "SE")
empl_pre_df <- rbind(coefs_pre_empl, se_pre_empl) %>% select(-c("id"))

#together
empl_full_df <- full_join(empl_df, empl_pre_df, by = c("sample.var", "sample", "var")) %>% 
  select(c(ends_with("treat"), sample, var)) %>%
  pivot_longer(cols = -c(sample, var), names_to = "interaction_var", values_to = "value") %>%
  pivot_wider(names_from = var, values_from = value ) %>%
  mutate(year = str_remove_all(interaction_var, "erhebja::|:gap_treat") %>% as.numeric()) 

ggplot(empl_full_df, aes(x = year, y = estimate, colour = sample, fill = sample)) +
  geom_point(size = 2) + geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE),
              alpha = 0.2, colour = NA) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = "Wage bin",fill = "Wage bin") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
ggsave("output/graph/ES_derenoncourt_FFE_employment_coarsest.pdf", 
       width = 18, height = 6, unit = "cm")

#Nr of workes, not employment
#post
derenoncourt_empl <- feols(
  log(workers_coarsest) ~ i(erhebja, gap_treat, ref = 2012) + i(erhebja, ref = 2012) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_post, split = ~MW_bins_coarsest)
coefs_empl <- coef(derenoncourt_empl) %>% mutate(var = "estimate")
se_empl   <- se(derenoncourt_empl) %>% mutate(var = "SE")
empl_df <- rbind(coefs_empl, se_empl) %>% select(-c("id"))

#pre
derenoncourt_empl_pre <- feols(
  log(workers_coarsest) ~ i(erhebja, gap_treat, ref = 2008) + i(erhebja, ref = 2008)| burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga + gr, data = firm_sample_pre, split = ~MW_bins_coarsest)
coefs_pre_empl <- coef(derenoncourt_empl_pre) %>% mutate(var = "estimate")
se_pre_empl   <- se(derenoncourt_empl_pre) %>% mutate(var = "SE")
empl_pre_df <- rbind(coefs_pre_empl, se_pre_empl) %>% select(-c("id"))

#together
empl_full_df <- full_join(empl_df, empl_pre_df, by = c("sample.var", "sample", "var")) %>% 
  select(c(ends_with("treat"), sample, var)) %>%
  pivot_longer(cols = -c(sample, var), names_to = "interaction_var", values_to = "value") %>%
  pivot_wider(names_from = var, values_from = value ) %>%
  mutate(year = str_remove_all(interaction_var, "erhebja::|:gap_treat") %>% as.numeric()) 

ggplot(empl_full_df, aes(x = year, y = estimate, colour = sample, fill = sample)) +
  geom_point(size = 2) + geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE),
              alpha = 0.2, colour = NA) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = "Wage bin",fill = "Wage bin") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(legend.position = "off")
ggsave("output/graph/ES_derenoncourt_FFE_nrworkers_coarsest.pdf", 
       width = 18, height = 6, unit = "cm")

##a bit more refined wage bins
ggplot(empl_full_df[empl_full_df$sample %in% 
                      c("3000+", "[2000, 3000)", "[1200, 2000)", "[600, 1200)"), ], 
       aes(x = year, y = estimate, colour = sample, shape = sample)) +
  geom_point(size = 2,position = position_dodge(width = 0.90)) +
  geom_line(,position = position_dodge(width = 0.90)) +
  geom_errorbar(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE),
                width = 0.4, position = position_dodge(width = 0.90)) +
  labs(x = NULL, y = "Estimate and 95% CI", colour = "Wage bin",shape = "Wage bin") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(5)) +  
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
ggsave("output/graph/ES_derenoncourt_FFE_employment_highwagebin.pdf", width = 18, height = 6, unit = "cm")






