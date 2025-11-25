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
library(grid)
library(gridExtra)
theme_set(theme_minimal())

plot_defaults_2 <- function() {
  list(
    scale_colour_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_y_continuous(labels = scales::comma),
    scale_x_continuous(labels = scales::comma),
    scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)),
    geom_vline(xintercept = 2013, linetype = "dashed"),
    theme(legend.position = "inside",
          legend.background = element_rect(colour = "white"),
          strip.text = element_text(face = "bold"))) }

##--------
#cross-sectional worker data
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")
LSE_08_18$main_MEM <- ifelse(LSE_08_18$main_sector == "MEM", 1, 0)

plot_data_wage_percentile <- LSE_08_18 %>%
  filter(sector != "Rest") %>%
  group_by(erhebja, sector) %>%
  summarise(P50 = wtd.quantile(real_wage, weights = gewicht, p = 0.5),
         P10 = wtd.quantile(real_wage, weights = gewicht, p = 0.1))

plot_data_wage_mean_50 <- LSE_08_18 %>%
  filter(sector != "Rest") %>%
  group_by(erhebja, sector) %>%
  mutate(P50 = wtd.quantile(real_wage, weights = gewicht, p = 0.5)) %>%
  filter(real_wage <= P50) %>%
  summarise(mean_50 = wtd.mean(real_wage, weight = gewicht))
plot_data_wage_mean_10 <- LSE_08_18 %>%
  filter(sector != "Rest") %>%
  group_by(erhebja, sector) %>%
  mutate(P10 = wtd.quantile(real_wage, weights = gewicht, p = 0.1)) %>%
  filter(real_wage <= P10) %>%
  summarise(mean_10 = wtd.mean(real_wage, weight = gewicht))

plot_data_wage_mean <- left_join(plot_data_wage_mean_50, plot_data_wage_mean_10,
                                 by = c("sector", "erhebja"))

empl_by_wagebin <- LSE_08_18 %>%
  filter(sector != "Rest") %>%
  group_by(MW_bins, sector, erhebja) %>% 
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) 

##-----------------------

#p10 and p50 (our cutoff)
ggplot(data = plot_data_wage_percentile, aes(x=erhebja, colour = sector)) +
  geom_line(aes(y = P10)) + geom_point(aes(y = P10)) +
  geom_line(aes(y = P50)) + geom_point(aes(y = P50)) +
  labs(x = NULL, y = "CHF", colour = NULL) +
  plot_defaults_2() +
  theme(legend.position.inside = c(0.95, 0.5),
        legend.justification.inside = c(0.95, 0.5),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text=element_text(size=13))
ggsave("output/graph/wage_percentiles_by_sector.pdf", width = 16, height = 6, unit = "cm")

#mean wage within this cutoff
ggplot(data = plot_data_wage_mean, aes(x=erhebja, colour = sector)) +
  geom_line(aes(y = mean_10)) + geom_point(aes(y = mean_10)) +
  geom_line(aes(y = mean_50)) + geom_point(aes(y = mean_50)) +
  labs(x = NULL, y = "CHF", colour = NULL) +
  plot_defaults_2()  +
  theme(legend.position = "none")
ggsave("output/graph/wage_means_by_sector.pdf", width = 16, height = 6, unit = "cm")


#employment
ggplot(data = empl_by_wagebin, aes(x=erhebja, y = employment, colour = sector, fill = sector)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "Employment",colour = NULL, fill = NULL) +
  plot_defaults_2() +
  facet_wrap(~MW_bins, scales = "free_y") +
  theme(legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.key.height = unit(0, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text=element_text(size=8))   
ggsave("output/graph/empl_by_wagebin_by_sector.pdf", width = 20, height = 8, unit = "cm")


#cheks for event study

check_regional_var <- firm_panel %>%
  mutate(msreg_an = as.numeric(msreg_an)) %>%
  group_by(arbkto) %>%
  summarise(
    MEM = sum(main_sector == "MEM", na.rm = TRUE),
    Watch = sum(main_sector == "Watch",  na.rm = TRUE) ) %>%
  mutate(ratio = round(Watch/(MEM+Watch), 2)) %>% ungroup() %>%
  arrange(desc(ratio)) %>% rename(Canton = arbkto,"%Watch" = ratio,
                                  "#MEM" = MEM, "#Watch" = Watch)

stargazer(check_regional_var, type = "latex",title = "Regional overlap Watchmaking vs. MEM sector",
          digits = 1, label = "tab:mem_watch_region", summary = FALSE, rownames = FALSE, no.space = TRUE, 
          font.size = "small", column.sep.width = "3pt", out = "output/table/mem_watch_region.tex")

check_MW_regional_var <- firm_panel %>%
  filter(!is.na(wage_region))%>%
  group_by(wage_region) %>%
  summarise(
    MEM = sum(main_sector == "MEM", na.rm = TRUE),
    Watch = sum(main_sector == "Watch",  na.rm = TRUE) ) %>%
  mutate(ratio = round(Watch/(MEM+Watch), 2)) %>% ungroup() %>%
  arrange(desc(ratio)) %>% rename("Wage region" = wage_region,"%Watch" = ratio,
                                  "#MEM" = MEM, "#Watch" = Watch)
stargazer(check_MW_regional_var, type = "latex",title = "Regional overlap Watchmaking vs. MEM sector",
          digits = 0, label = "tab:mem_watch_MWregion", summary = FALSE, rownames = FALSE, no.space = TRUE, 
          font.size = "small", column.sep.width = "3pt", out = "output/table/mem_watch_MWregion.tex")

#time and sub-sector FE to remove unobserved heterogeneity BETWEEN different sub-sectors
#issue when negative variance: multicollinearity or insufficient data

#cluster SEs: cluster = ~noga + gr + k_untgroe (stratification)
# Check cluster structure
cluster_structure <- firm_panel %>% filter(real_wage<7000) %>% 
  group_by(noga, gr) %>% summarise(n_firmsize = uniqueN(k_untgroe) )
sum(cluster_structure$n_firmsize==1)


## ---- OLD ------------
firm_panel <- readRDS(file = "output/data/2d_firm_panel.rds") 

#wage data
wage_parallel_trends <- firm_panel %>%
  filter(sector != "Rest") %>%
  group_by(erhebja, sector) %>%
  summarise(
    counts_n = sum(!is.na(real_wage)),
    p10 = wtd.quantile(real_wage, weights = gewicht, probs = 0.1, na.rm = TRUE), #sector-wide percentiles
    p30 = wtd.quantile(real_wage, weights = gewicht, probs = 0.3, na.rm = TRUE),
    p50 = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE),
    p90 = wtd.quantile(real_wage, weights = gewicht, probs = 0.9, na.rm = TRUE),
    dist_to_MW = wtd.mean(dist_to_MW, weights = gewicht, na.rm = TRUE),
    p50_dist_to_MW = wtd.quantile(dist_to_MW, weights = gewicht, probs = 0.5, na.rm = TRUE),
    mean_firm = wtd.mean(mean_wage, weights = gewicht, na.rm = TRUE), #mean firm wage
    mean_firm_P50 = wtd.mean(P50, weights = gewicht, na.rm = TRUE), #mean firm median wages
    mean_firm_P10 = wtd.mean(P10, weights = gewicht, na.rm = TRUE),
    mean_firm_P20 = wtd.mean(P20, weights = gewicht, na.rm = TRUE),
    mean_firm_P30 = wtd.mean(P30, weights = gewicht, na.rm = TRUE),
    mean_firm_P80 = wtd.mean(P80, weights = gewicht, na.rm = TRUE), #real_wage
    mean_empl = wtd.mean(total_empl, weights = gewicht),
    .groups = "drop" )

##mean of mean firm wage
ggplot(data = wage_parallel_trends , aes(x=erhebja, colour = sector, y = mean_firm)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "CHF", colour = NULL) +
  plot_defaults_2()+
  theme(legend.position = "none")
ggsave("output/graph/mean_firm_wage_by_sector.pdf", width = 16, height = 6, unit = "cm")

#p50_dist_to_MW -> takes into account different wage regions
ggplot(data = wage_parallel_trends , aes(x=erhebja, colour = sector)) +
  geom_line(aes(y = p50_dist_to_MW)) + geom_point(aes(y = p50_dist_to_MW)) +
  geom_line(aes(y = dist_to_MW)) + geom_point(aes(y = dist_to_MW)) +
  labs(x = NULL, y = "CHF", colour = NULL) +
  plot_defaults_2() +
  theme(legend.position = "none")
ggsave("output/graph/dist_to_MW_by_sector.pdf", width = 16, height = 6, unit = "cm")

##mean(P50) of firms: mean_firm_P50, mean_firm_P30, mean_firm_P10
ggplot(data = wage_parallel_trends , aes(x=erhebja, colour = sector, fill = sector)) +
  geom_line(aes(y=mean_firm_P10)) + geom_point(aes(y=mean_firm_P10)) +
  labs(x = NULL, y = "CHF",colour = NULL, fill = NULL) +
  plot_defaults_2() +
  theme(legend.position = "none")
ggsave("output/graph/mean_firm_p10_by_sector.pdf", width = 16, height = 6, unit = "cm")

ggplot(data = wage_parallel_trends , aes(x=erhebja, colour = sector, fill = sector)) +
  geom_line(aes(y=mean_firm_P20)) + geom_point(aes(y=mean_firm_P20)) +
  labs(x = NULL, y = "CHF",colour = NULL, fill = NULL) +
  plot_defaults_2() +
  theme(legend.position = "none")
ggsave("output/graph/mean_firm_p20_by_sector.pdf", width = 16, height = 6, unit = "cm")

ggplot(data = wage_parallel_trends , aes(x=erhebja, colour = sector, fill = sector)) +
  geom_line(aes(y=mean_firm_P30)) + geom_point(aes(y=mean_firm_P30)) +
  labs(x = NULL, y = "CHF",colour = NULL, fill = NULL) +
  plot_defaults_2() +
  theme(legend.position = "none")
ggsave("output/graph/mean_firm_p30_by_sector.pdf", width = 16, height = 6, unit = "cm")

ggplot(data = wage_parallel_trends , aes(x=erhebja, colour = sector, fill = sector)) +
  geom_line(aes(y=mean_firm_P50)) + geom_point(aes(y=mean_firm_P50)) +
  labs(x = NULL, y = "CHF",colour = NULL, fill = NULL) +
  plot_defaults_2()+
  theme(legend.position = "none")
ggsave("output/graph/mean_firm_p50_by_sector.pdf", width = 16, height = 6, unit = "cm")

  
#employment: mean_empl -> for Cengiz! employment study
ggplot(data = wage_parallel_trends, aes(x=erhebja, colour = sector, y = mean_empl)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "Employment (%)", colour = NULL) +
  plot_defaults_2() +
  theme(legend.position = "none")
ggsave("output/graph/mean_employment_by_sector.pdf", width = 16, height = 6, unit = "cm")

empl_by_wagebin <- firm_panel %>%
  group_by(MW_bins, sector, erhebja) %>% 
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) 
ggplot(data = empl_by_wagebin, aes(x=erhebja, y = employment, colour = sector, fill = sector)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "Employment",colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  theme() +
  facet_wrap(~MW_bins, scales = "free_y")
ggsave("output/graph/empl_by_wagebin_by_sector.pdf", width = 20, height = 10, unit = "cm")

ggplot(data = empl_by_wagebin[empl_by_wagebin$MW_bins %notin% c("[1200, 2000)", "[2000, 3000)", "3000+" ) ,], 
       aes(x=erhebja, y = employment, colour = sector, fill = sector)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "Employment",colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  theme() +
  facet_wrap(~MW_bins, scales = "free_y") +
  theme(legend.position = "none")
ggsave("output/graph/empl_by_wagebin_by_sector_zoomed.pdf", width = 20, height = 10, unit = "cm")

empl_by_wagebin_coarse <- firm_panel %>%
  group_by(MW_bins_coarse, sector, erhebja) %>% 
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) 
ggplot(data = empl_by_wagebin_coarse, aes(x=erhebja, y = employment, colour = sector, fill = sector)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "Employment",colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_vline(xintercept = 2013, linetype = "dashed") +
  theme() +
  facet_wrap(~MW_bins_coarse, scales = "free_y")
ggsave("output/graph/empl_by_coarse_wagebin_by_sector.pdf", width = 16, height = 8, unit = "cm")

#cba coverage
## coverage 
firm_panel$cba_numeric <- case_when(
  firm_panel$cba == "CBA covered" ~ 1,
  firm_panel$cba == "Not covered" ~0,
  TRUE ~NA_real_
)
table(firm_panel$cba_numeric, firm_panel$sector)


MEM_cba <- feols(cba_numeric ~ 1, split = ~erhebja, weights = ~gewicht,
                 data = firm_panel[firm_panel$sector=="MEM", ])
tidy_mem_cba <- map_dfr(MEM_cba, tidy, .id = "erhebja") %>%
  mutate(year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% 
           as.numeric(), variable = "MEM")
watch_cba <- feols(cba_numeric ~ 1, split = ~erhebja, weights = ~gewicht,
                 data = firm_panel[firm_panel$sector=="Watch", ])
tidy_watch_cba <- map_dfr(watch_cba, tidy, .id = "erhebja") %>%
  mutate(year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% 
           as.numeric(), variable = "watch")

cba_cov_sector <- rbind(tidy_mem_cba, tidy_watch_cba)
ggplot(data = cba_cov_sector, aes(x=year, colour = variable, fill = variable,
                                  y = estimate)) +
  geom_point(size = 2) + geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
              alpha = 0.5) +
  labs(x =NULL, y = "CBA coverage", colour = NULL, fill = NULL) +
  plot_defaults_2()+
  scale_y_continuous(labels = scales::percent) 
ggsave("output/graph/cba_coverage_by_sector.pdf", width = 16, height = 6, unit = "cm")

##table for composition of labour force

#who are the people and firms still observed (2014, 2016, 2018)
#workers
chars <- firm_panel %>%
  filter(erhebja==2012) %>%
  group_by(sector) %>%
  mutate(
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"], na.rm = TRUE)/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"], na.rm = TRUE)/sum(gewicht),
    mean_employment = wtd.mean(ibgr, weights = gewicht),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    cba = wtd.mean(cba_numeric == 1, weights = gewicht, na.rm = TRUE), #sum(gewicht[cba=="CBA covered"], na.rm = TRUE)/sum(gewicht[cba=="Not covered"], na.rm = TRUE),
    p10 = wtd.quantile(real_wage, weights = gewicht, probs = 0.1, na.rm = TRUE),
    mean_wage = wtd.mean(real_wage, weight = gewicht),
    percentile = wtd.mean(gross_wage_std <= min_wage, weight = gewicht, na.rm = TRUE) * 100,
    #   min_wage = wtd.mean(min_wage, weight = gewicht, na.rm = TRUE),
    A = sum(gewicht[wage_region == "A"], na.rm = TRUE)/sum(gewicht),
    B = sum(gewicht[wage_region == "B"], na.rm = TRUE)/sum(gewicht),
    C = sum(gewicht[wage_region == "C"], na.rm = TRUE)/sum(gewicht)) %>%
  summarise(
    "Mean wage (CHF)" = mean(mean_wage, na.rm = TRUE),
    "P10 wage (CHF)" = mean(p10, na.rm = TRUE),
    #   "Minimum wage" = mean(min_wage, na.rm = TRUE),
    "Bite of MW" = mean(percentile, na.rm = TRUE),
    "Employment rate" = mean(mean_employment, na.rm = TRUE),
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
    "N" = n()) %>% ungroup() %>% 
  mutate_if(is.numeric, round, digits = 2)

chars_wide <- chars %>%
  pivot_longer(
    cols = -sector,                  # all columns except 'year'
    names_to = "variable", 
    values_to = "value") %>%
  pivot_wider(
    names_from = sector,             # each year becomes a column
    values_from = value ) %>% 
  rename(Characteristic  = variable)  %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = "'")))

stargazer(chars_wide, type = "latex",title = "Workers in the Watchmaking vs. MEM sector (2012)", digits = 1,
          label = "tab:watch_MEM_chars", summary = FALSE, rownames = FALSE, no.space = TRUE, 
          font.size = "small", column.sep.width = "3pt", out = "output/table/watch_MEM_chars.tex")

#what firms are in the market?
firms_chars <- firm_panel %>%
  filter(erhebja==2012) %>%
  filter(mem==1 | watch==1) %>%
  group_by(entid, sector) %>%
  slice(1) %>%
  group_by(sector) %>%
  summarise(
    perc_small = round(mean(firm_size == "small (1-49)", na.rm = TRUE) * 100,1),
    perc_med = round(mean(firm_size == "medium (50-249)", na.rm = TRUE) * 100,1),
    perc_large = round(mean(firm_size == "large (250+)", na.rm = TRUE) * 100,1),
    mean_gap =  round(mean(gap_exposure_cons), 4),
    avg_p50 = round(mean(P50, na.rm = TRUE), 1), 
    min_wage = round(mean(min_wage_w, na.rm = TRUE), 1),
    atleast1MW = round(mean(atleast1MW, na.rm = TRUE)*100,1),
    cba_cov = round(mean(cba_numeric == 1, na.rm = TRUE)*100, 0),
    n_obs = n(),
    .groups = 'drop') 

firms_chars_wide <- firms_chars %>%
  pivot_longer(cols = -sector,   # Reshape to wide format
               names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = sector, values_from = value) %>%
  mutate(Characteristic = case_when(  # Create proper row labels
    variable == "avg_p50" ~ "Median firm wage",
    variable == "mean_gap"~ "Gap (2012)",
    variable == "min_wage" ~ "Minimum Wage Workers (%)",
    variable == "atleast1MW"~ "At least 1 MW employed (%)",
    variable == "perc_small" ~ "Small firms (1-49) (%)",
    variable == "perc_med" ~ "Medium firms (50-250) (%)",
    variable == "perc_large" ~ "Large firms (250'+) (%)",
    variable == "n_obs" ~ "N",
    TRUE ~ variable )) %>%
  select(c(Characteristic, MEM, Watch)) %>%
  mutate_if(is.numeric, round, digits = 2)

stargazer(firms_chars_wide, type = "latex",title = "Firms in the Watchmaking vs. MEM sector (2012)",
          digits = 0, label = "tab:mem_watch_firms", summary = FALSE, rownames = FALSE, no.space = TRUE, 
          font.size = "small", column.sep.width = "3pt", out = "output/table/mem_watch_firms.tex")




