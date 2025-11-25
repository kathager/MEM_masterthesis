## ---------------------------
##
## Master thesis
## Bin by bin employment analysis
## Author: Katja Hager
##
## Date Created: Oct 2025
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

setFixest_dict(c(wage_region = "Wage Region", erhebja = "Year", mem = "MEM", 
                 noga = "Sub-sector", msreg_an = "Labour Market",
                 arbkto = "Canton", mean_wage = "Mean Firm Wage",
                 real_wage = "Real Wage (gross, FTE)",
                 burnr = "Firm", avs_nb = "Worker",
                 main_MEM = "MEM", natkat2 = "Nationality",
                 geschle = "Gender", transition_2016 = "Transition until 2016",
                 gap_treat = "Gap", gr = "Greater Region"))

cengiz_plots <- function() {
  list(
    geom_bar(alpha = 0.6, width = 0.5, stat = "identity", fill = scales::viridis_pal(option = "D")(1),
             position = position_dodge(width = 0.6)), 
    geom_point(stat = "identity", position = position_dodge(width = 0.6)),
    scale_x_continuous(breaks = 1:9, labels = empl_df$sample),
    labs(x = NULL, y = "Estimate", colour = NULL, fill = NULL),
    theme(axis.text.x = element_text(angle = 35, hjust=1)),
    geom_hline(yintercept = 0, linetype = "dashed", color = "black"),
    geom_point(color = "black", show.legend = FALSE, position = position_dodge(width = 0.5)),
    geom_errorbar(aes(ymin = estimate  - 1.96*SE, ymax = estimate  + 1.96*SE),
                  alpha = 1, width = 0.4, colour = "black", position = position_dodge(width = 0.5)))}

cengiz_plots_2 <- function() {
  list(
    geom_bar(alpha = 0.6, width = 0.5, stat = "identity", 
             position = position_dodge(width = 0.5)), 
    scale_x_continuous(breaks = 1:9, labels = empl_df$sample),
    labs(x = NULL, y = "Estimate", colour = NULL, fill = NULL),
    theme(axis.text.x = element_text(angle = 35, hjust=1)),
    geom_hline(yintercept = 0, linetype = "dashed", color = "black"),
    scale_colour_manual(values = scales::viridis_pal(option = "D")(3)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(3)),
    geom_point(color = "black", show.legend = FALSE, position = position_dodge(width = 0.5)),
    geom_errorbar(aes(ymin = estimate  - 1.96*SE, ymax = estimate  + 1.96*SE),
                    alpha = 1, width = 0.4, colour = "black", position = position_dodge(width = 0.5)))}

## -----------
firm_panel <- readRDS(file = "output/data/2d_firm_panel.rds")
firm_panel$numeric_C <- ifelse(firm_panel$wage_region=="C", 1, 0)
firm_panel$numeric_cba <- ifelse(firm_panel$cba=="CBA covered", 1, 0)
#firm_panel$transition_2016
firm_panel$numeric_male <- ifelse(firm_panel$geschle=="men", 1, 0)
firm_panel$numeric_resident <- ifelse(firm_panel$natkat2=="CH/C-permit", 1, 0)
firm_panel$numeric_young <- ifelse(firm_panel$age=="Young", 1, 0)
firm_panel$numeric_new <- ifelse(firm_panel$tenure=="Short", 1, 0)

## ----------- 1 ----------------
#Impact of Minimum Wages on the Wage Distribution
#instead of binary treatment effect, I use the continuous gap measure
all_years <- unique(firm_panel$erhebja)
all_firms <- unique(firm_panel$burnr)
wage_bins <- unique(firm_panel$MW_bins)
firm_panel_cengiz <- expand_grid(burnr = all_firms, erhebja = all_years, MW_bins = wage_bins) #empty

gap_12 <- firm_panel %>%
  filter(erhebja==2012)%>%
  group_by(burnr) %>% summarise(gap = gap_treat[1])

firm_employment <- firm_panel %>%
  group_by(burnr, MW_bins, erhebja) %>% 
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(burnr, erhebja) %>%
  mutate(
    firm_eployment = sum(employment), 
    firm_workers = sum(n_workers),
    work_y = employment/firm_eployment,
    work_y = n_workers/firm_workers )

firm_panel_cengiz <- left_join(firm_panel_cengiz, firm_employment, by = c("erhebja", "burnr", "MW_bins"))
firm_panel_cengiz <- left_join(firm_panel_cengiz, gap_12, by = "burnr")
firm_panel_cengiz <- left_join(firm_panel_cengiz, firm_panel, by = c("erhebja", "burnr", "MW_bins"))

firm_panel_cengiz <- firm_panel_cengiz %>%
  filter(!is.na(gap_treat))

#regression
empl_by_bin <- feols(
  work_y ~ i(post_2013, gap_treat, ref = 0) | burnr+msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

etable(empl_by_bin, tex = TRUE, digits = 3,  digits.stats = 3)

coefs_empl <- coef(empl_by_bin) %>% rename(estimate = `post_2013::1:gap_treat`)
se_empl   <- se(empl_by_bin)  %>%   rename(SE = `post_2013::1:gap_treat`)
empl_df <- left_join(coefs_empl, se_empl, by = c("id", "sample", "sample.var"))

ggplot(empl_df, aes(x = id, y = estimate)) +
  cengiz_plots()
ggsave("output/graph/ES_cengiz_FFE_wage_dist.pdf", width = 20, height = 5, unit = "cm")


#cba-coverage
empl_by_bin_cba <- feols(
  work_y ~ i(post_2013, gap_treat, ref = 0) +
    i(post_2013, gap_treat, ref = 0):numeric_cba | burnr+msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_cba <- coef(empl_by_bin_cba) %>%
  rename(base = `post_2013::1:gap_treat`,
         differential = `post_2013::1:gap_treat:numeric_cba`)  %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_cba   <- se(empl_by_bin_cba)  %>%
  rename(base = `post_2013::1:gap_treat`,
         differential = `post_2013::1:gap_treat:numeric_cba`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_cba <- left_join(coefs_empl_cba, se_empl_cba, 
                         by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_cba, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification = c(1, 1))
ggsave("output/graph/ES_cengiz_FFE_wage_dist_CBA.pdf", width = 20, height = 4, unit = "cm")

#regression by transition period
empl_by_bin_16 <- feols(
  work_y ~ i(post_2013, gap_treat, ref = 0) +
    i(post_2013, gap_treat, ref = 0):transition_2016 | burnr+msreg_an+erhebja, 
  split = ~MW_bins, weights = ~gewicht, cluster = ~noga+gr, data = firm_panel_cengiz)

coefs_empl_16 <- coef(empl_by_bin_16) %>%
  rename(base = `post_2013::1:gap_treat`,
         differential = `post_2013::1:gap_treat:transition_2016`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_16   <- se(empl_by_bin_16)  %>%
  rename(base = `post_2013::1:gap_treat`,
         differential = `post_2013::1:gap_treat:transition_2016`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_16 <- left_join(coefs_empl_16, se_empl_16, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_16, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification = c(1, 1))
ggsave("output/graph/ES_cengiz_FFE_wage_dist_16.pdf", width = 20, height = 4, unit = "cm")


#regression by wage region
#effect of being in wage region C
empl_by_bin_C <- feols(
  work_y ~ i(post_2013, gap_treat, ref = 0) +
    i(post_2013, gap_treat, ref = 0):numeric_C | burnr+msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_C <- coef(empl_by_bin_C) %>% rename(base = `post_2013::1:gap_treat`,
         differential = `post_2013::1:gap_treat:numeric_C`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_C   <- se(empl_by_bin_C)  %>%  rename(base = `post_2013::1:gap_treat`,
         differential = `post_2013::1:gap_treat:numeric_C`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_C <- left_join(coefs_empl_C, se_empl_C, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_C, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0))
ggsave("output/graph/ES_cengiz_FFE_wage_dist_C.pdf", width = 20, height = 4, unit = "cm")


#NR of workers
#regression
empl_by_bin_workers <- feols(
  work_y ~ i(post_2013, gap_treat, ref = 0) | burnr+msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_workers <- coef(empl_by_bin_workers) %>%
  rename(estimate = `post_2013::1:gap_treat`)
se_empl_workers   <- se(empl_by_bin_workers)  %>%
  rename(SE = `post_2013::1:gap_treat`)
empl_df_workers <- left_join(coefs_empl_workers, se_empl_workers, by = c("id", "sample", "sample.var"))

ggplot(empl_df_workers, aes(x = id, y = estimate)) +
  cengiz_plots()
ggsave("output/graph/ES_cengiz_FFE_wage_dist_workers.pdf", width = 20, height = 5, unit = "cm")


## ----------- 2 ----------------
#firm_panel <- readRDS(file = "output/data/2d_firm_panel.rds")
firm_sample_post <- firm_panel[firm_panel$erhebja>2010, ]
firm_sample_pre <- firm_panel[firm_panel$erhebja<2012, ]

#Cengiz et al. (2019): impact of MW on missing / excess jobs over time
#missing: below MW / excess: up until first wage bin
#yearly, by entid
#instead of level of employment, change in employment relative to 2012

##impact of gap_std on these numbers, within group (like wage effects)
#excess/missing jobs within each entid, regress this on gap_std (also defined per entid)

#event study effects
##excess
###post
excess_jobs_post <- feols(
  diff_empl ~ i(erhebja, gap_treat, ref = 2012) + i(erhebja, ref = 2012) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_sample_post[firm_sample_post$diff_type == "excess_jobs", ])
coefs_excess_jobs_post <- tidy(excess_jobs_post) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric()) %>%
  filter(str_detect(term, ".*gap_treat"))
coefs_excess_jobs_post <- coefs_excess_jobs_post %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012))
  
###pre
excess_jobs_pre <- feols(
  diff_empl ~ i(erhebja, gap_treat, ref = 2008) + i(erhebja, ref = 2008) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_sample_pre[firm_sample_pre$diff_type == "excess_jobs", ])
coefs_excess_jobs_pre <- tidy(excess_jobs_pre) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_treat") %>% as.numeric()) %>%
  filter(str_detect(term, ".*gap_treat"))
coefs_excess_jobs_pre <- coefs_excess_jobs_pre %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012))
###together
coefs_excess_jobs <- rbind(coefs_excess_jobs_post, coefs_excess_jobs_pre)
coefs_excess_jobs$var <- "Excess"
ggplot(coefs_excess_jobs, aes(x = year, y = estimate)) +
  geom_point(size = 2) + geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
              alpha = 0.2, colour = NA) +
  labs(x = NULL, y = "Estimate and 95% CI") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
ggsave("output/graph/ES_cengiz_FFE_excess.pdf", width = 18, height = 6, unit = "cm")

##missing
###post
missing_jobs_post <- feols(
  diff_empl ~ i(erhebja, gap_treat, ref = 2012) + i(erhebja, ref = 2012) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_sample_post[firm_sample_post$diff_type == "lost_jobs", ])
coefs_missing_jobs_post <- tidy(missing_jobs_post) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_treat") %>%as.numeric()) %>%
  filter(str_detect(term, ".*gap_treat")) 
coefs_missing_jobs_post <- coefs_missing_jobs_post %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012))
###pre
missing_jobs_pre <- feols(
  diff_empl ~ i(erhebja, gap_treat, ref = 2008) + i(erhebja, ref = 2008) | burnr+msreg_an, 
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_sample_pre[firm_sample_pre$diff_type == "lost_jobs", ])
coefs_missing_jobs_pre <- tidy(missing_jobs_pre) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_treat") %>% as.numeric()) %>%
  filter(str_detect(term, ".*gap_treat"))
coefs_missing_jobs_pre <- coefs_missing_jobs_pre %>% 
  dplyr::bind_rows(
    tibble(term = "baseline",estimate = 0,std.error = 0,statistic = 0,p.value = 0,year = 2012))

###together
coefs_missing_jobs <- rbind(coefs_missing_jobs_post, coefs_missing_jobs_pre)
coefs_missing_jobs$var <- "Missing"
ggplot(coefs_missing_jobs, aes(x = year, y = estimate)) +
  geom_point(size = 2) + geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
              alpha = 0.2, colour = NA) +
  labs(x = NULL, y = "Estimate and 95% CI") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
ggsave("output/graph/ES_cengiz_FFE_missing.pdf", width = 18, height = 6, unit = "cm")


## together
coefs_employment_cengiz <- rbind(coefs_missing_jobs, coefs_excess_jobs)

ggplot(coefs_employment_cengiz, aes(x = year, y = estimate, colour = var, fill = var)) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_line(, show.legend = FALSE) +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
              alpha = 0.3, colour = NA) +
  labs(x = NULL, y = "Estimate", fill = NULL, colour = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))+
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) + 
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification.inside = c(1, 0),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text=element_text(size=8))
ggsave("output/graph/jobs_cengiz_FFE.pdf", width = 20, height = 4, unit = "cm")
