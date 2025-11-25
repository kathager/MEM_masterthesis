## ---------------------------
##
## Master thesis
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
                 gr = "Region", geschle = "Gender", 
                 transition_2016 = "Transition until 2016"))

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

cengiz_ES <- function() {
  list(
    geom_point(size = 2, show.legend = FALSE),
      geom_line(, show.legend = FALSE),
      geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  alpha = 0.3, colour = NA),
      labs(x = NULL, y = "Estimate", fill = NULL, colour = NULL),
      geom_hline(yintercept = 0, linetype = "dashed", color = "black"),
      scale_colour_manual(values = scales::viridis_pal(option = "D")(2)),
      scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) ,
      theme(legend.position = "inside",
            legend.position.inside = c(1, 0),
            legend.justification.inside = c(1, 0),
            legend.background = element_rect(colour = "white"),
            strip.text = element_text(face = "bold"),
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.5, "cm"),
            legend.text=element_text(size=8))  )}


## ---------------------------
firm_panel <- readRDS(file = "output/data/2d_firm_panel.rds") 
firm_panel$numeric_C <- ifelse(firm_panel$wage_region=="C", 1, 0)
firm_panel$numeric_cba <- ifelse(firm_panel$cba=="CBA covered", 1, 0)
#firm_panel$transition_2016
firm_panel$numeric_male <- ifelse(firm_panel$geschle=="men", 1, 0)
firm_panel$numeric_resident <- ifelse(firm_panel$natkat2=="CH/C-permit", 1, 0)
firm_panel$numeric_young <- ifelse(firm_panel$age=="Young", 1, 0)
firm_panel$numeric_new <- ifelse(firm_panel$tenure=="Short", 1, 0)

table(firm_panel$sector)
table(firm_panel$mem)

all_years <- unique(firm_panel$erhebja)
all_firms <- unique(firm_panel$burnr)
wage_bins <- unique(firm_panel$MW_bins)
firm_panel_cengiz <- expand_grid(burnr = all_firms, erhebja = all_years, MW_bins = wage_bins) #empty

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
firm_panel_cengiz <- left_join(firm_panel_cengiz, firm_panel, by = c("erhebja", "burnr", "MW_bins"))

## -------------- 1 --------------
#Impact of Minimum Wages on the Wage Distribution

#regression
empl_by_bin <- feols(
  work_y ~ i(post_2013, mem, ref = 0) | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

etable(empl_by_bin, tex = TRUE, digits = 3,  digits.stats = 3)

coefs_empl <- coef(empl_by_bin) %>%
  rename(estimate = `post_2013::1:mem`)
se_empl   <- se(empl_by_bin)  %>%
  rename(SE = `post_2013::1:mem`)
empl_df <- left_join(coefs_empl, se_empl, by = c("id", "sample", "sample.var"))

ggplot(empl_df, aes(x = id, y = estimate)) +
  cengiz_plots()
ggsave("output/graph/ES_cengiz_wage_dist_watch.pdf", width = 20, height = 5, unit = "cm")


#NR of workers
#regression
empl_by_bin_workers <- feols(
  work_y ~ i(post_2013, mem, ref = 0) | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_workers <- coef(empl_by_bin_workers) %>%
  rename(estimate = `post_2013::1:mem`)
se_empl_workers   <- se(empl_by_bin_workers)  %>%
  rename(SE = `post_2013::1:mem`)
empl_df_workers <- left_join(coefs_empl_workers, se_empl_workers, by = c("id", "sample", "sample.var"))

ggplot(empl_df_workers, aes(x = id, y = estimate)) +
  cengiz_plots()
ggsave("output/graph/ES_cengiz_watch_wage_dist_workers.pdf", width = 20, height = 5, unit = "cm")

#heterogeneity
#cba-coverage
empl_by_bin_cba <- feols(
  work_y ~ i(post_2013, mem, ref = 0) +
    i(post_2013, mem, ref = 0):numeric_cba | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_cba <- coef(empl_by_bin_cba) %>%
  rename(base = `post_2013::1:mem`,
         differential = `post_2013::1:mem:numeric_cba`)  %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_cba   <- se(empl_by_bin_cba)  %>%
  rename(base = `post_2013::1:mem`,
         differential = `post_2013::1:mem:numeric_cba`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_cba <- left_join(coefs_empl_cba, se_empl_cba, 
                         by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_cba, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text=element_text(size=8))
ggsave("output/graph/ES_cengiz_wage_dist_watch_CBA.pdf", width = 20, height = 4, unit = "cm")

#regression by transition period
empl_by_bin_16 <- feols(
  work_y ~ i(post_2013, mem, ref = 0) +
    i(post_2013, mem, ref = 0):transition_2016 | msreg_an+erhebja, 
  split = ~MW_bins, weights = ~gewicht, cluster = ~noga+gr, data = firm_panel_cengiz)

coefs_empl_16 <- coef(empl_by_bin_16) %>%
  rename(base = `post_2013::1:mem`,
         differential = `post_2013::1:mem:transition_2016`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_16   <- se(empl_by_bin_16)  %>%
  rename(base = `post_2013::1:mem`,
         differential = `post_2013::1:mem:transition_2016`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_16 <- left_join(coefs_empl_16, se_empl_16, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_16, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0))
ggsave("output/graph/ES_cengiz_wage_dist_watch_16.pdf", width = 20, height = 4, unit = "cm")


#effect of being in wage region C
empl_by_bin_C <- feols(
  work_y ~ i(post_2013, mem, ref = 0) +
    i(post_2013, mem, ref = 0):numeric_C | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_C <- coef(empl_by_bin_C) %>% rename(base = `post_2013::1:mem`,
                                               differential = `post_2013::1:mem:numeric_C`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_C   <- se(empl_by_bin_C)  %>%  rename(base = `post_2013::1:mem`,
                                              differential = `post_2013::1:mem:numeric_C`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_C <- left_join(coefs_empl_C, se_empl_C, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_C, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0))
ggsave("output/graph/ES_cengiz_watch_wage_dist_C.pdf", width = 20, height = 4, unit = "cm")


#numeric_male <- ifelse(firm_panel$geschle=="men", 1, 0)
empl_by_bin_male <- feols(
  work_y ~ i(post_2013, mem, ref = 0) +
    i(post_2013, mem, ref = 0):numeric_male | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_male <- coef(empl_by_bin_male) %>% rename(base = `post_2013::1:mem`,
                                               differential = `post_2013::1:mem:numeric_male`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_male   <- se(empl_by_bin_male)  %>%  rename(base = `post_2013::1:mem`,
                                              differential = `post_2013::1:mem:numeric_male`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_male <- left_join(coefs_empl_male, se_empl_male, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_male, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0),
        legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.3, "cm"),
      legend.text=element_text(size=8))
ggsave("output/graph/ES_cengiz_watch_wage_dist_male.pdf", width = 20, height = 4, unit = "cm")

#numeric_young <- ifelse(firm_panel$age=="Young", 1, 0)
empl_by_bin_young <- feols(
  work_y ~ i(post_2013, mem, ref = 0) +
    i(post_2013, mem, ref = 0):numeric_young | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_young <- coef(empl_by_bin_young) %>% rename(base = `post_2013::1:mem`,
                                                     differential = `post_2013::1:mem:numeric_young`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_young   <- se(empl_by_bin_young)  %>%  rename(base = `post_2013::1:mem`,
                                                    differential = `post_2013::1:mem:numeric_young`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_young <- left_join(coefs_empl_young, se_empl_young, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_young, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "off")
ggsave("output/graph/ES_cengiz_watch_wage_dist_young.pdf", width = 20, height = 4, unit = "cm")

#numeric_resident <- ifelse(firm_panel$natkat2=="CH/C-permit", 1, 0)
empl_by_bin_resident <- feols(
  work_y ~ i(post_2013, mem, ref = 0) +
    i(post_2013, mem, ref = 0):numeric_resident | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_resident <- coef(empl_by_bin_resident) %>% rename(base = `post_2013::1:mem`,
                                                             differential = `post_2013::1:mem:numeric_resident`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_resident   <- se(empl_by_bin_resident)  %>%  rename(base = `post_2013::1:mem`,
                                                            differential = `post_2013::1:mem:numeric_resident`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_resident <- left_join(coefs_empl_resident, se_empl_resident, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_resident, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "off")
ggsave("output/graph/ES_cengiz_watch_wage_dist_resident.pdf", width = 20, height = 4, unit = "cm")

#numeric_new <- ifelse(firm_panel$tenure=="Short", 1, 0)
empl_by_bin_new <- feols(
  work_y ~ i(post_2013, mem, ref = 0) +
    i(post_2013, mem, ref = 0):numeric_new | msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = firm_panel_cengiz)

coefs_empl_new <- coef(empl_by_bin_new) %>% rename(base = `post_2013::1:mem`,
                                                             differential = `post_2013::1:mem:numeric_new`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "estimate")
se_empl_new   <- se(empl_by_bin_new)  %>%  rename(base = `post_2013::1:mem`,
                                                            differential = `post_2013::1:mem:numeric_new`) %>%
  pivot_longer(cols = c(base, differential), names_to = "interaction", values_to = "SE")

empl_df_new <- left_join(coefs_empl_new, se_empl_new, by = c("id", "sample", "sample.var", "interaction"))  %>%
  mutate(interaction = factor(interaction, levels = c("base", "differential")),
         interaction = fct_relevel(interaction, "base"))

ggplot(empl_df_new, aes(x = id, y = estimate, colour = interaction, fill = interaction)) +
  cengiz_plots_2() +
  theme(legend.position = "off")
ggsave("output/graph/ES_cengiz_watch_wage_dist_new.pdf", width = 20, height = 4, unit = "cm")


## -------------- 2 --------------
#Cengiz et al. (2019): impact of MW on missing / excess jobs over time
#missing: below MW / above: up until first wage bin
#yearly, sector


#event study effects
excess_jobs <- feols(
  diff_empl ~ i(erhebja, mem, ref = 2012) | msreg_an+erhebja, 
  weights = ~gewicht, cluster = ~noga+gr, split= ~diff_type,
  data = firm_panel_cengiz)

coefs_excess_jobs <- coef(excess_jobs) %>% mutate(var = "estimate")
se_excess_jobs   <- se(excess_jobs)  %>% mutate(var = "std.error")

excess <- rbind(coefs_excess_jobs, se_excess_jobs) %>%
  pivot_longer(cols = starts_with("erhebja"),
               names_to = "name",
               values_to = "value") %>%
  mutate(year = str_remove_all(name, "erhebja::|:mem") %>% as.numeric())  %>% 
  select(c("year", "value", "var", "sample")) %>%
  pivot_wider(names_from = "var",values_from = "value")%>%
  dplyr::bind_rows(
    tibble(sample = "excess_jobs",estimate = 0,std.error = 0,year = 2012),
    tibble(sample = "lost_jobs",estimate = 0,std.error = 0,year = 2012)) %>%
  mutate(sample = ifelse(sample=="excess_jobs", "excess jobs", "missing jobs")  )

ggplot(excess, aes(x = year, y = estimate, colour = sample, fill = sample)) +
  cengiz_ES()
ggsave("output/graph/jobs_cengiz_watch.pdf", width = 20, height = 4, unit = "cm")

#NR of workers
excess_jobs_n <- feols(
  diff_workers ~ i(erhebja, mem, ref = 2012) | msreg_an+erhebja, 
  weights = ~gewicht, cluster = ~noga+gr, split= ~diff_type, data = firm_panel_cengiz)

coefs_excess_jobs_n <- coef(excess_jobs_n) %>% mutate(var = "estimate")
se_excess_jobs_n   <- se(excess_jobs_n)  %>% mutate(var = "std.error")

excess_n <- rbind(coefs_excess_jobs_n, se_excess_jobs_n) %>%
  pivot_longer(cols = starts_with("erhebja"),
               names_to = "name", values_to = "value") %>%
  mutate(year = str_remove_all(name, "erhebja::|:mem") %>% as.numeric())  %>% 
  select(c("year", "value", "var", "sample")) %>%
  pivot_wider(names_from = "var", values_from = "value")%>%
  dplyr::bind_rows(
    tibble(sample = "excess_jobs",estimate = 0,std.error = 0,year = 2012),
    tibble(sample = "lost_jobs",estimate = 0,std.error = 0,year = 2012)) %>%
  mutate(sample = ifelse(sample=="excess_jobs", "excess jobs", "missing jobs")  )

ggplot(excess_n, aes(x = year, y = estimate, colour = sample, fill = sample)) +
  cengiz_ES()
ggsave("output/graph/jobs_cengiz_watch_workers.pdf", width = 20, height = 4, unit = "cm")

#event study effects by transition_2016
excess_jobs_16 <- feols(
  diff_empl ~ i(erhebja, mem, ref = 2012) +
    i(erhebja, mem, ref = 2012):transition_2016 | msreg_an+erhebja, 
  weights = ~gewicht, cluster = ~noga+gr, split= ~diff_type,
  data = firm_panel_cengiz)

coefs_excess_jobs_16 <- coef(excess_jobs_16) %>% mutate(var = "estimate")
se_excess_jobs_16   <- se(excess_jobs_16)  %>% mutate(var = "std.error")

excess_16 <- rbind(coefs_excess_jobs_16, se_excess_jobs_16) %>%
  pivot_longer(cols = ends_with("2016"),
               names_to = "name",
               values_to = "value") %>%
  mutate(year = str_remove_all(name, "erhebja::|:mem|:transition_2016") %>% as.numeric())  %>% 
  select(c("year", "value", "var", "sample")) %>%
  pivot_wider(names_from = "var",values_from = "value")%>%
  dplyr::bind_rows(
    tibble(sample = "excess_jobs",estimate = 0,std.error = 0,year = 2012),
    tibble(sample = "lost_jobs",estimate = 0,std.error = 0,year = 2012)) %>%
  mutate(sample = ifelse(sample=="excess_jobs", "excess jobs", "missing jobs")  )

ggplot(excess_16, aes(x = year, y = estimate, colour = sample, fill = sample)) +
  cengiz_ES()
ggsave("output/graph/jobs_cengiz_watch_16.pdf", width = 20, height = 4, unit = "cm")

##
#OLD -------------
table(firm_panel$firm_size)
summary(firm_panel$diff_empl)

noga_chars <- firm_panel %>% 
  group_by(noga, erhebja) %>%
  summarise(
    perc_small = sum(gewicht[firm_size == "small (1-49)"])/sum(gewicht),
    noga_size = sum(employment, na.rm = TRUE),
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"])/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"], na.rm = TRUE)/sum(gewicht, na.rm = TRUE),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    sector = sector[1])

exc_miss_empl_noga <- firm_panel %>%
  filter(!is.na(above))%>%
  group_by(noga, erhebja, above) %>%
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE),
    .groups = "drop"  ) %>% 
  group_by(noga, above) %>%  arrange(erhebja) %>%
  mutate(
    empl_2012 = dplyr::first(employment[erhebja == 2012], default = NA_real_),
    diff_empl = ifelse(!is.na(employment) & !is.na(empl_2012), employment - empl_2012, NA_real_),
    diff_type = ifelse(above == 1, "excess_jobs", "lost_jobs"),
    .groups = "drop") 

exc_miss_empl_noga_wide <- exc_miss_empl_noga %>%
  select(c(diff_empl, above, erhebja, noga)) %>%
  pivot_wider(names_from = above, names_prefix = "jobs_", values_from = diff_empl)

exc_miss_empl_noga_wide <- left_join(exc_miss_empl_noga_wide, noga_chars, 
                                     by = c("noga", "erhebja"))

excess_jobs <- feols(
  jobs_1 ~ i(erhebja, sector=="MEM", ref = 2012) + 
    perc_small + perc_male + perc_resident + perc_lowskill | erhebja, 
  cluster = ~noga, weight = ~noga_size, 
  data = exc_miss_empl_noga_wide)

coefs_excess_jobs <- tidy(excess_jobs) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = as.numeric(str_extract_all(term, "\\d+")) )

ggplot(coefs_excess_jobs, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error)) +
  labs(x = "Year", y = "Estimate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2))


missing_jobs <- feols(
  jobs_0 ~ i(erhebja, sector=="MEM", ref = 2012)+
    perc_small + perc_male + perc_resident + perc_lowskill | erhebja, 
  cluster = ~noga, weight = ~noga_size, 
  data = exc_miss_empl_noga_wide)

coefs_missing_jobs <- tidy(missing_jobs) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = as.numeric(str_extract_all(term, "\\d+")) )

ggplot(coefs_missing_jobs, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error)) +
  labs(x = "Year", y = "Estimate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2))

## together
coefs_missing_jobs$var <- "Missing"
coefs_excess_jobs$var <- "Excess"
coefs_employment_cengiz <- rbind(coefs_missing_jobs, coefs_excess_jobs)

cengiz_plot <- ggplot(coefs_employment_cengiz, 
                      aes(x = year, y = estimate, colour = var, fill = var)) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_line(, show.legend = FALSE) +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error),
              alpha = 0.2, colour = NA) +
  labs(x = NULL, y = "Estimate", fill = "Employment", colour = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))+
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) + 
  theme(legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2))
plot(cengiz_plot)
ggsave("output/graph/jobs_cengiz_by_sector.pdf", width = 18, height = 6, unit = "cm")





#
## ----------- Berger and Lanz --------
#construct post-treatment counter-factual wage distribution
#DiD: watch = control, MEM = treated

#1) pre-treatment employment by wage bin and treatment -> e_pre
empl_pre_treatment <- firm_panel %>%
  filter(erhebja==2012 & sector %in% c("MEM", "Watch")) %>%
  group_by(sector, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  group_by(sector) %>% mutate(
    empl_subsector = sum(employment, na.rm = TRUE), 
    erhebja = 2012)

#2) post-treatment employment by wage bin and treatment
empl_post_treatment <- firm_panel %>%
  filter(erhebja==2018 & sector %in% c("MEM", "Watch")) %>%
  group_by(sector, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  mutate(erhebja = 2018)

#together
empl_treatment <- rbind(empl_pre_treatment, empl_post_treatment)
table(empl_treatment$sector)

empl_treatment <- empl_treatment %>%
  group_by(sector, MW_bins) %>% summarise(
    e_pre = employment[erhebja==2012][1]/empl_subsector[erhebja==2012][1],
    e_post = employment[erhebja==2018][1]/empl_subsector[erhebja==2012][1],
    e_change = (employment[erhebja==2018][1]-employment[erhebja==2012][1])/
      empl_subsector[erhebja==2012][1] ) %>%
  group_by(MW_bins) %>% summarise(
    e_counter = e_pre[sector=="MEM"] + e_change[sector=="Watch"],
    e_actual = e_post[sector=="MEM"],
    diff = e_actual-e_counter,
    var = "sector",
    .groups = "drop") %>%
  arrange(MW_bins) %>%
  mutate(cum_diff = cumsum(diff))

#5) plot
ggplot(data = empl_treatment, aes(x = MW_bins, y = diff)) + 
  geom_bar(alpha = 0.7, width = 0.7, stat = "identity", fill = scales::viridis_pal(option = "D")(1)) +
  geom_point(aes(y=cum_diff)) +
  geom_line(aes(y=cum_diff, group = 1)) +
  labs(x = "Wage bins", y = "Difference in Employment") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("output/graph/BergerLanz_employment_by_sector.pdf", width = 18, height = 6, unit = "cm")


