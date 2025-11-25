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
LSE_08_18 <- readRDS(file = "output/data/5a_noga_analysis.rds") %>%
  filter(mem==1) %>% mutate(
    above = case_when(
    MW_bins %in% c("[min, -600)", "[-600, -300)", "[-300, 0)") ~0,
    MW_bins %in% c("[0, 300)", "[300, 600)") ~1,
    TRUE~NA_real_))

gap_12 <- LSE_08_18 %>%
  filter(erhebja==2012)%>%
  group_by(noga) %>% summarise(gap = gap_exposure_noga[1])

#Impact of Minimum Wages on the Wage Distribution
#instead of binary treatment effect, I use the continuous gap measure

#by wage bin
sector_employment <- LSE_08_18 %>%
  group_by(noga, MW_bins, erhebja) %>% 
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE),
    .groups = "drop") %>%
  group_by(noga, erhebja) %>%
  mutate(
    firm_eployment = sum(employment), 
    firm_workers = sum(n_workers),
    work_y = employment/firm_eployment,
    work_y = n_workers/firm_workers )

sector_panel_cengiz <- left_join(LSE_08_18, sector_employment, by = c("erhebja", "noga", "MW_bins"))
sector_panel_cengiz <- left_join(sector_panel_cengiz, gap_12, by = "noga")

#excess/missing jobs above/below
all_years <- unique(LSE_08_18$erhebja)
all_sectors <- unique(LSE_08_18$noga)
above_below <- unique(LSE_08_18$above)
sector_cengiz_above <- expand_grid(noga = all_sectors, erhebja = all_years, above = above_below) #empty

above_below_empl <- LSE_08_18 %>%
  group_by(noga, above, erhebja) %>%
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE),
    .groups = "drop") 
sector_cengiz_above <- left_join(sector_cengiz_above, above_below_empl, 
                                 by = c("noga", "above", "erhebja")) %>%
  filter(!is.na(above))

sector_cengiz_above <- sector_cengiz_above %>%
  group_by(noga, above) %>%
  arrange(erhebja) %>%
  mutate(
    empl_pre = employment[erhebja == 2012]) %>%
  group_by(noga, above, erhebja) %>%
  summarise(
    diff_empl = employment - empl_pre,
    diff_type = ifelse(above == 1, "Excess", "Missing"),
    .groups = "drop")

sector_cengiz_above <- left_join(LSE_08_18, sector_cengiz_above, by = c("noga", "erhebja", "above"))
sector_cengiz_above <- left_join(sector_cengiz_above, gap_12, by = "noga")

## ---------- estimations --------
#regression
empl_by_bin <- feols(
  work_y ~ i(post_2013, gap, ref = 0) | noga+msreg_an+erhebja, split = ~MW_bins,
  weights = ~gewicht, cluster = ~noga+gr, 
  data = sector_panel_cengiz)

coefs_empl <- coef(empl_by_bin) %>%
  rename(estimate = `post_2013::1:gap`)
se_empl   <- se(empl_by_bin)  %>%
  rename(SE = `post_2013::1:gap`)
empl_df <- left_join(coefs_empl, se_empl, by = c("id", "sample", "sample.var"))

ggplot(empl_df, aes(x = id, y = estimate)) +
  geom_bar(alpha = 0.6, width = 0.7, stat = "identity", fill = scales::viridis_pal(option = "D")(1)) +
  geom_point(stat = "identity", size = 1) +
  geom_errorbar(aes(ymin = estimate - 1.96*SE, ymax = estimate + 1.96*SE),
                alpha = 1, width = 0.3, colour = "black")+
  scale_x_continuous(breaks = 1:9, labels = empl_df$sample) +
  labs(x = NULL, y = "Estimate")+
  theme(axis.text.x = element_text(angle = 35, hjust=1))
ggsave("output/graph/ES_cengiz_FFE_wage_dist_noga.pdf", width = 20, height = 5, unit = "cm")

#missing / excess jobs
diff_empl_above_below <- feols(
  diff_empl ~ i(erhebja, gap, ref = 2012) | noga+msreg_an+erhebja, 
  split= ~diff_type, 
  weights = ~gewicht, cluster = ~noga+gr, 
  data = sector_cengiz_above)

coefs_diff_empl <- coef(diff_empl_above_below)  %>% mutate(var = "coefficient") %>% select(-c("sample.var"))
se_diff_empl <- se(diff_empl_above_below)  %>% mutate(var = "se") %>% select(-c("sample.var"))

together <- rbind(coefs_diff_empl, se_diff_empl) %>%
  pivot_longer(
    cols = starts_with("erhebja::"), 
    names_to = "Year", 
    values_to = "value"  ) %>%
  mutate( year = as.numeric(str_extract_all(Year, "\\d+"))  ) %>%
  select(-c(Year)) %>%
  pivot_wider(
    names_from = "var",
    values_from = "value"   ) %>%
  dplyr::bind_rows(
    tibble(coefficient = 0,se = 0,year = 2012, id = 1, sample = "Excess"),
    tibble(coefficient = 0,se = 0,year = 2012, id = 2, sample = "Missing"))


ggplot(together, aes(x = year, y = coefficient, colour = sample, fill = sample)) +
  geom_point(size = 1) + geom_line() +
  geom_ribbon(aes(ymin = coefficient - 1.96*se, ymax = coefficient + 1.96*se),
              alpha = 0.3, colour = NA) +
  labs(x = NULL, y = "Estimate", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) +  
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  theme(legend.position = "inside",
        legend.position.inside = c(0, 0),
        legend.justification.inside = c(0, 0),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text=element_text(size=8))
ggsave("output/graph/jobs_cengiz_noga.pdf", width = 20, height = 5, unit = "cm")



##---------OLD-------------


#Cengiz et al. (2019): impact of MW on missing / excess jobs over time
#missing: below MW / excess: up until first wage bin
#yearly, by noga (or treatment group)

##impact of gap_std on these numbers, within group (like wage effects)
#excess/missing jobs within each noga, regress this on gap_std (also defined per noga)
table(LSE_08_18$MW_bins)
exc_miss_empl_noga <- LSE_08_18 %>%
  mutate(
    above = case_when(
      MW_bins %in% c("[min, -600)", "[-600, -300)", "[-300, 0)") ~0,
      MW_bins %in% c("[0, 300)", "[300, 600)") ~1,
      TRUE~NA_real_) ) %>%   filter(!is.na(above))%>%
  group_by(noga, erhebja, above) %>%
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE),
    gap_exposure_noga = gap_exposure_noga[1],
    cluster_3 = cluster_3[1],
    noga_size = noga_size[1], 
    .groups = "drop"  ) %>% group_by(noga, above) %>%  arrange(erhebja) %>%
  mutate(
    empl_2012 = dplyr::first(employment[erhebja == 2012], default = NA_real_),
    diff_empl = ifelse(!is.na(employment) & !is.na(empl_2012),
                       employment - empl_2012, NA_real_),
    diff_type = ifelse(above == 1, "Excess", "Missing"),
    .groups = "drop")

exc_miss_empl_noga_wide <- exc_miss_empl_noga %>%
  select(c(diff_empl, above, erhebja, noga, cluster_3, gap_exposure_noga, noga_size)) %>%
  pivot_wider(names_from = above, names_prefix = "jobs_", values_from = diff_empl)

Excess <- feols(
  jobs_1 ~ i(erhebja, gap_exposure_noga, ref = 2012) | erhebja + noga,
  cluster = ~noga, weights = ~noga_size,
  data = exc_miss_empl_noga_wide[exc_miss_empl_noga_wide$cluster_3 != "Not exposed", ])

coefs_Excess <- tidy(Excess) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_exposure_noga") %>%
      as.numeric()) %>%
  filter(str_detect(term, ".*gap_exposure_noga"))
ggplot(coefs_Excess, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error)) +
  labs(x = "Year", y = "Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2))
ggsave("output/graph/Excess_noga_cluster_3.pdf", width = 18, height = 6, unit = "cm")


missing_jobs <- feols(
  jobs_0 ~ i(erhebja, gap_exposure_noga, ref = 2012) | erhebja + noga,
  cluster = ~noga, weights = ~noga_size,
  data = exc_miss_empl_noga_wide[exc_miss_empl_noga_wide$cluster_3 != "Not exposed", ])

coefs_missing_jobs <- tidy(missing_jobs) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(year = str_remove_all(term, "erhebja::|:gap_exposure_noga")
         %>% as.numeric()) %>% filter(str_detect(term, ".*gap_exposure_noga"))
ggplot(coefs_missing_jobs, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error)) +
  labs(x = "Year", y = "Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(2008, 2018, by = 2))
ggsave("output/graph/missing_jobs_noga_cluster_3.pdf", width = 18, height = 6, unit = "cm")

## together
coefs_missing_jobs$var <- "Missing"
coefs_Excess$var <- "Excess"
coefs_employment_cengiz <- rbind(coefs_missing_jobs, coefs_Excess)

cengiz_plot <- ggplot(coefs_employment_cengiz, aes(x = year, y = estimate, colour = var, fill = var)) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_line(, show.legend = FALSE) +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error),
              alpha = 0.2, colour = NA) +
  labs(x = "Event time", y = "Coefficient", fill = "Employment", colour = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))+
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) + 
  theme(legend.position = c(0.9, 0.2),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"))
plot(cengiz_plot)
ggsave("output/graph/jobs_noga_cengiz_cluster_3.pdf", width = 18, height = 6, unit = "cm")



## ---------------------------
#Berger and Lanz: construct post-treatment counter-factual wage distribution
#DiD: group sub-sectors based on gap. less exposed as control group.

#collapse directly by treatment group, not sub-sectors first
#1) pre-treatment employment by wage bin and treatment -> e_pre
empl_pre_treatment_km <- LSE_08_18 %>%
  filter(erhebja==2012 & !is.na(cluster_3)) %>%
  group_by(cluster_3, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  group_by(cluster_3) %>% mutate(
    empl_subsector = sum(employment, na.rm = TRUE), 
    erhebja = 2012)

#2) post-treatment employment by wage bin and treatment
empl_post_treatment_km <- LSE_08_18 %>%
  filter(erhebja==2018 & !is.na(cluster_3)) %>%
  group_by(cluster_3, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  mutate(erhebja = 2018)

#together
empl_treatment_km <- rbind(empl_pre_treatment_km, empl_post_treatment_km)
table(empl_treatment_km$cluster_3)

empl_treatment_km <- empl_treatment_km %>%
  group_by(cluster_3, MW_bins) %>% summarise(
    e_pre = employment[erhebja==2012][1]/empl_subsector[erhebja==2012][1],
    e_post = employment[erhebja==2018][1]/empl_subsector[erhebja==2012][1],
    e_change = (employment[erhebja==2018][1]-employment[erhebja==2012][1])/
      empl_subsector[erhebja==2012][1] ) %>%
  group_by(MW_bins) %>% summarise(
    e_counter = e_pre[cluster_3=="Highly exposed"] + e_change[cluster_3=="Exposed"],
    e_actual = e_post[cluster_3=="Highly exposed"],
    diff = e_actual-e_counter,
    var = "cluster_3",
    .groups = "drop") %>%
  arrange(MW_bins) %>%
  mutate(cum_diff = cumsum(diff))

#5) plot
ggplot(data = empl_treatment_km, aes(x = MW_bins, y = diff)) + 
  geom_bar(alpha = 0.7, width = 0.7, stat = "identity", fill = scales::viridis_pal(option = "D")(1)) +
  geom_point(aes(y=cum_diff)) +
  geom_line(aes(y=cum_diff, group = 1)) +
  labs(x = "Wage bins", y = "Difference in Employment") 
ggsave("output/graph/BergerLanz_noga_employment.pdf", width = 18, height = 6, unit = "cm")


## ----------------------------





## --------------------------- delete --------------------
#delete -> old

###

noga_gap <- LSE_08_18_noga_cons %>%
  group_by(noga) %>%
  summarise(gap_std = gap_std[1]) %>%
  filter(!is.na(gap_std))

##k-means (distance based clustering)
#create treatment groups based on clustering -> let k-means decide
X <- scale(noga_gap %>% select(gap_std))
set.seed(123)
km <- kmeans(X, centers = 3, nstart = 50)
noga_gap$cluster_3 <- km$cluster #2 highly exposed, 12 middle exposed, 54 not exposed at all
table(noga_gap$cluster_3)
noga_gap$cluster_3 <- case_when(
  noga_gap$cluster_3 == 2 ~ "Highly exposed",
  noga_gap$cluster_3 == 1 ~ "Exposed",
  noga_gap$cluster_3 == 3 ~ "Not exposed" )

km <- kmeans(X, centers = 5, nstart = 50)
noga_gap$cluster_5 <- km$cluster #1 highly exposed, 2 exposed, 10 less // 17 no, 38 not exposed at all
table(noga_gap$cluster_5)
noga_gap$cluster_5 <- case_when(
  noga_gap$cluster_5 == 4 ~ "Highly exposed (1)",
  noga_gap$cluster_5 == 3 ~ "Exposed (2)",
  noga_gap$cluster_5 == 5 ~ "Little exposed (3)" ,
  noga_gap$cluster_5 == 2 ~ "Not exposed (4)" ,
  noga_gap$cluster_5 == 1 ~ "Bottom (5)" )

noga_gap$gap_std <- NULL

noga_gap_cons_kmeans <- noga_gap

#merge treatment status
LSE_cluster_cons <- left_join(LSE_08_18_noga_cons, noga_gap, by = "noga")
LSE_cluster_cons$cluster_5 <- as.factor(LSE_cluster_cons$cluster_5)
LSE_cluster_cons$cluster_5 <- relevel(LSE_cluster_cons$cluster_5, ref = "Highly exposed (1)")
LSE_cluster_cons$cluster_5 <- relevel(LSE_cluster_cons$cluster_5, ref = "Exposed (2)")
LSE_cluster_cons$cluster_5 <- relevel(LSE_cluster_cons$cluster_5, ref = "Little exposed (3)")
LSE_cluster_cons$cluster_5 <- relevel(LSE_cluster_cons$cluster_5, ref = "Not exposed (4)")
LSE_cluster_cons$cluster_5 <- relevel(LSE_cluster_cons$cluster_5, ref = "Bottom (5)")

LSE_cluster_cons$cluster_3 <- as.factor(LSE_cluster_cons$cluster_3)
LSE_cluster_cons$cluster_3 <- relevel(LSE_cluster_cons$cluster_3, ref = "Highly exposed")
LSE_cluster_cons$cluster_3 <- relevel(LSE_cluster_cons$cluster_3, ref = "Exposed")
LSE_cluster_cons$cluster_3 <- relevel(LSE_cluster_cons$cluster_3, ref = "Not exposed")

LSE_cluster_cons$cluster_2 <- case_when(
  LSE_cluster_cons$cluster_3 %in% c("Highly exposed", "Exposed") ~ "Exposed",
  LSE_cluster_cons$cluster_3 == "Not exposed" ~"Not exposed" )
LSE_cluster_cons$cluster_2 <- as.factor(LSE_cluster_cons$cluster_2)
LSE_cluster_cons$cluster_2 <- relevel(LSE_cluster_cons$cluster_2, ref = "Exposed")
LSE_cluster_cons$cluster_2 <- relevel(LSE_cluster_cons$cluster_2, ref = "Not exposed")

LSE_cluster_cons$cluster_3_new <- case_when(
  LSE_cluster_cons$cluster_5 %in% c("Highly exposed (1)", "Exposed (2)") ~"Highly Exposed",
  LSE_cluster_cons$cluster_5 == "Little exposed (3)" ~"Exposed",
  LSE_cluster_cons$cluster_5 %in% c("Not exposed (4)", "Bottom (5)") ~"Not exposed" )
table(LSE_cluster_cons$cluster_3_new)
LSE_cluster_cons$cluster_3_new <- as.factor(LSE_cluster_cons$cluster_3_new)
LSE_cluster_cons$cluster_3_new <- relevel(LSE_cluster_cons$cluster_3_new, ref = "Highly Exposed")
LSE_cluster_cons$cluster_3_new <- relevel(LSE_cluster_cons$cluster_3_new, ref = "Exposed")
LSE_cluster_cons$cluster_3_new <- relevel(LSE_cluster_cons$cluster_3_new, ref = "Not exposed")


#model-based clustering

#cluster_3 tries different #of clusters (up to 9). For each, it fits Gaussian 
#mixtures with different covariance structures. picks best model according to BIC.
#BIC = Bayesian Information Criterion
# Outliers are less likely to dominate because they can be absorbed into a 
# small-variance or small-probability component.
# gap is conceptually a distance measure -> distance-based clustering (k-means) natural. 
# But distribution is skewed with strong outliers -> model-based clustering is more robust.
# cluster_3 allows clusters to differ in variance and can separate outliers into their own component, 
# rather than letting them distort group boundaries.

noga_gap <- LSE_08_18_noga_cons %>%
  group_by(noga) %>%
  summarise(gap_std = gap_std[1]) %>%
  filter(!is.na(gap_std))
X <- scale(noga_gap %>% select(gap_std))
m <- cluster_3(X)
noga_gap$group <- m$classification #6 vs. 29 vs. rest 33
table(noga_gap$group)
noga_gap$cluster_3 <- case_when(
  noga_gap$group == 1 ~ "Not exposed",
  noga_gap$group == 2 ~ "Exposed",
  noga_gap$group == 3 ~ "Strongly exposed"
)
noga_gap$cluster_3 <- as.factor(noga_gap$cluster_3)
noga_gap$cluster_3 <- relevel(noga_gap$cluster_3, ref = "Strongly exposed")
noga_gap$cluster_3 <- relevel(noga_gap$cluster_3, ref = "Exposed")
noga_gap$cluster_3 <- relevel(noga_gap$cluster_3, ref = "Not exposed")

noga_gap_cons <- noga_gap

noga_gap_cons_kmeans_cluster_3 <- left_join(noga_gap_cons_kmeans, noga_gap_cons, by = "noga")

## ------------------------
#compare cons and high gap groups

noga_gap_cons_kmeans_cluster_3$var <- "cons"
noga_gap_kmeans_cluster_3$var <- "high"

total_gap_groups <- rbind(noga_gap_cons_kmeans_cluster_3, noga_gap_kmeans_cluster_3)
total_gap_groups_wide <- total_gap_groups %>%
  pivot_wider(
    id_cols = noga,                   
    names_from = var,                 
    values_from = c(gap_std, cluster_3, cluster_3, cluster_5),  # all columns to reshape
    names_sep = "_" )
## ------------------------

#merge
noga_gap$gap_std <- NULL

#merge treatment status
LSE_cluster_cons <- left_join(LSE_08_18_noga_cons, noga_gap, by = "noga")
table(LSE_cluster_cons$cluster_3)

## -----------------------------------------

##pre-trends
#wage developments over groups
plot_wage_worker <- LSE_cluster_cons %>%
  filter(!is.na(cluster_3)) %>%
  group_by(erhebja, cluster_3) %>%
  summarise( #sub-sectoral wage changes, assigned to each worker. mean change based on worker weights
    p05_change_pct = wtd.mean(p05_change_pct, weights = gewicht, na.rm = TRUE),
    p07_change_pct = wtd.mean(p07_change_pct, weights = gewicht, na.rm = TRUE),
    p09_change_pct = wtd.mean(p09_change_pct, weights = gewicht, na.rm = TRUE),
    p08_change_pct = wtd.mean(p08_change_pct, weights = gewicht, na.rm = TRUE),
    p10_change_pct = wtd.mean(p10_change_pct, weights = gewicht, na.rm = TRUE),
    p20_change_pct = wtd.mean(p20_change_pct, weights = gewicht, na.rm = TRUE),
    p30_change_pct = wtd.mean(p30_change_pct, weights = gewicht, na.rm = TRUE),
    p40_change_pct = wtd.mean(p40_change_pct, weights = gewicht, na.rm = TRUE),
    p50_change_pct = wtd.mean(p50_change_pct, weights = gewicht, na.rm = TRUE),
    p80_change_pct = wtd.mean(p80_change_pct,weights = gewicht,  na.rm = TRUE)
  )

plot_wage_worker$treat <- (plot_wage_worker$cluster_3)
table(plot_wage_worker$treat)


ggplot(data = plot_wage_worker[plot_wage_worker$erhebja>2008, ], aes(x=erhebja, colour = treat)) +
  geom_line(aes(y = p05_change_pct)) +
  geom_point(aes(y = p05_change_pct)) +
  labs(x = "Year", y = "P05 change in %", colour = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))  
ggsave("pretrends_wagegrowth_p05_cluster_3_cons.pdf")

ggplot(data = plot_wage_worker[plot_wage_worker$erhebja>2008, ], aes(x=erhebja, colour = treat)) +
  geom_line(aes(y = p10_change_pct)) +
  geom_point(aes(y = p10_change_pct)) +
  labs(x = "Year", y = "P10 change in %", colour = NULL) +
  theme() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("pretrends_wagegrowth_p10_cluster_3_cons.pdf")

ggplot(data = plot_wage_worker[plot_wage_worker$erhebja>2008, ], aes(x=erhebja, colour = treat)) +
  geom_line(aes(y = p20_change_pct)) +
  geom_point(aes(y = p20_change_pct)) +
  labs(x = "Year", y = "P20 change in %", colour = NULL) +
  theme() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("pretrends_wagegrowth_p20_cluster_3_cons.pdf")

ggplot(data = plot_wage_worker[plot_wage_worker$erhebja>2008, ], aes(x=erhebja, colour = treat)) +
  geom_line(aes(y = p30_change_pct)) +
  geom_point(aes(y = p30_change_pct)) +
  labs(x = "Year", y = "P30 change in %", colour = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("pretrends_wagegrowth_p30_cluster_3_cons.pdf")

ggplot(data = plot_wage_worker[plot_wage_worker$erhebja>2008, ], aes(x=erhebja, colour = treat)) +
  geom_line(aes(y = p50_change_pct)) +
  geom_point(aes(y = p50_change_pct)) +
  labs(x = "Year", y = "P50 change in %", colour = NULL) +
  theme() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))+
  theme(legend.position = "none")
ggsave("pretrends_wagegrowth_p50_cluster_3_cons.pdf")

#legend
my_legend_wage <- ggplot(data = plot_wage_worker[plot_wage_worker$erhebja>2008, ], aes(x=erhebja, colour = treat)) +
  geom_line(aes(y = p50_change_pct)) +
  geom_point(aes(y = p50_change_pct)) +
  labs(x = "Year", y = "P50 change in %", colour = NULL) +
  theme() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  theme(
    legend.background = element_rect(colour = "white"),
    strip.text = element_text(face = "bold"),
    legend.text=element_text(size=12),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(2, "cm")
  )
legend_wage <- cowplot::get_legend(my_legend_wage)
grid.newpage()
grid.draw(legend_wage)
ggsave("pretrends_wagegrowth_legend_cluster_3_cons.pdf", 
       plot = legend, 
       device = "pdf")

## ------------
LSE_cluster_cons <- LSE_cluster_cons %>%
  group_by(erhebja, noga) %>%
  mutate(
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"])/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"])/sum(gewicht),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    mean_small_firms =sum(gewicht[firm_size == "small (1-49)"])/sum(gewicht)
  )

data_treatment_cluster_3<- LSE_cluster_cons %>%
  filter(cluster_3 == "Strongly exposed") %>%
  filter(!is.na(cluster_3))
summary(data_treatment_cluster_3$gap_std)

#control for sub-sectoral charactersitics (same unit of variation as X and Y)
treatment_cluster_3 <- feols(
  p80_change_pct ~ i(erhebja, gap_std, ref = -1) +
    i(erhebja, ref = -1)+ perc_male +perc_lowskill + perc_nomgt + 
    perc_resident + mean_age + mean_tenure+ mean_small_firms | msreg_an, 
  weights = ~gewicht,
  cluster = ~noga, 
  data = data_treatment_cluster_3)

coefs_noga_individ_yearly <- tidy(treatment_cluster_3)%>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_std") %>%
      as.numeric()) %>%
  filter(str_detect(term, ".*gap_std"))

ggplot(coefs_noga_individ_yearly, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error)) +
  labs(x = "Event time", y = "Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  scale_x_continuous(breaks = c(-3, -1, 1, 3, 5))

## ---------------------------------------
## EMPLOYMENT ##
## ---------------------------------------

LSE_cluster_cons$MW_bins <- relevel(LSE_cluster_cons$MW_bins, ref = "3000+")
LSE_cluster_cons$MW_bins <- relevel(LSE_cluster_cons$MW_bins, ref = "[2000, 3000)")
LSE_cluster_cons$MW_bins <- relevel(LSE_cluster_cons$MW_bins, ref = "[1200, 2000)")
LSE_cluster_cons$MW_bins <- relevel(LSE_cluster_cons$MW_bins, ref = "[600, 1200)")
LSE_cluster_cons$MW_bins <- relevel(LSE_cluster_cons$MW_bins, ref = "[0, 600)")
LSE_cluster_cons$MW_bins <- relevel(LSE_cluster_cons$MW_bins, ref = "[-600, 0)")
LSE_cluster_cons$MW_bins <- relevel(LSE_cluster_cons$MW_bins, ref = "[min, -600)")

#Berger and Lanz: construct post-treatment counter-factual wage distribution
#DiD: group sub-sectors based on gap

#collapse directly by treatment group, not sub-sectors first
#1) pre-treatment employment by wage bin and treatment -> e_pre
empl_pre_treatment_cluster_3 <- LSE_cluster_cons %>%
  filter(erhebja==2012 & !is.na(cluster_3)) %>%
  group_by(cluster_3, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  group_by(cluster_3) %>% mutate(
    empl_subsector = sum(employment, na.rm = TRUE), 
    erhebja = 2012)

#2) post-treatment employment by wage bin and treatment
empl_post_treatment_cluster_3 <- LSE_cluster_cons %>%
  filter(erhebja==2018 & !is.na(cluster_3)) %>%
  group_by(cluster_3, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  mutate(erhebja = 2018)

#together
empl_treatment_cluster_3 <- rbind(empl_pre_treatment_cluster_3, empl_post_treatment_cluster_3)
table(empl_treatment_cluster_3$cluster_3)

empl_treatment_cluster_3 <- empl_treatment_cluster_3 %>%
  group_by(cluster_3, MW_bins) %>% summarise(
    e_pre = employment[erhebja==2012][1]/empl_subsector[erhebja==2012][1],
    e_post = employment[erhebja==2018][1]/empl_subsector[erhebja==2012][1],
    e_change = (employment[erhebja==2018][1]-employment[erhebja==2012][1])/
      empl_subsector[erhebja==2012][1] ) %>%
  group_by(MW_bins) %>% summarise(
    e_counter = e_pre[cluster_3=="Strongly exposed"] + e_change[cluster_3=="Exposed"],
    e_actual = e_post[cluster_3=="Strongly exposed"],
    diff = e_actual-e_counter,
    var = "cluster_3",
    .groups = "drop") %>%
  arrange(MW_bins) %>%
  mutate(cum_diff = cumsum(diff))

#5) plot
ggplot(data = empl_treatment_cluster_3, aes(x = MW_bins, y = diff)) + 
  geom_bar(alpha = 0.7, width = 0.7, stat = "identity", fill = scales::viridis_pal(option = "D")(1)) +
  geom_point(aes(y=cum_diff)) +
  geom_line(aes(y=cum_diff, group = 1)) +
  labs(x = "Wage bins", y = "Difference in Employment") 


## first on sub-sector, then on treatment group (not preferred)
#1) pre-treatment employment by wage bin and noga -> E_pre
empl_pre <- LSE_cluster_cons %>%
  filter(erhebja==2012) %>%
  group_by(noga, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  group_by(noga) %>% mutate(empl_subsector = sum(employment, na.rm = TRUE),
                            erhebja = 2012)
#2) post-treatment employment by wage bin and noga
empl_post <- LSE_cluster_cons %>%
  filter(erhebja==2018) %>%
  group_by(noga, MW_bins) %>% summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE)) %>%
  mutate(erhebja = 2018)

#together
empl <- rbind(empl_pre, empl_post)
empl_noga <- empl %>%
  group_by(noga, MW_bins) %>% summarise(
    e_pre = employment[erhebja==2012][1]/empl_subsector[erhebja==2012][1],
    e_change = (employment[erhebja==2018][1]-employment[erhebja==2012][1])/
      empl_subsector[erhebja==2012][1],
    e_post = employment[erhebja==2018][1]/empl_subsector[erhebja==2012][1]) 

#cluster_3
clustered <- LSE_cluster_cons %>%
  group_by(noga) %>%
  summarise(cluster_3 = cluster_3[1])

bin_by_bin_clust <- left_join(empl_noga, clustered, by = "noga")

#3) construct counter-factual & actual employment by bin, 
#then change in employment associated with MW policy, by wage bin
counterfactual_cluster_3 <- bin_by_bin_clust %>%
  filter(!is.na(cluster_3)) %>%
  group_by(MW_bins) %>%
  summarise(
    e_counter = mean(e_pre[cluster_3=="Strongly exposed"], na.rm = TRUE) + 
      mean(e_change[cluster_3=="Exposed"], na.rm = TRUE),
    e_actual = mean(e_post[cluster_3=="Strongly exposed"], na.rm = TRUE),
    diff = e_actual-e_counter,
    var = "cluster_3",
    .groups = "drop") %>%
  arrange(MW_bins) %>%
  mutate(cum_diff = cumsum(diff))

ggplot(data = counterfactual_cluster_3, aes(x = MW_bins)) + 
  geom_bar(aes(y = diff ), alpha = 0.7, width = 0.7, stat = "identity", 
           fill = scales::viridis_pal(option = "D")(1)) +
  geom_point(aes(y=cum_diff)) +
  geom_line(aes(y=cum_diff, group = 1)) +
  labs(x = "Wage bins", y = "Difference in Employment") 

## ----------------------------
#Cengiz et al. (2019): impact of MW on missing / excess jobs over time
#missing: below MW / excess: up until first wage bin
#yearly, by noga (or treatment group)

##impact of gap_std on these numbers, within group (like wage effects)
#excess/missing jobs within each noga, regress this on gap_std (also defined per noga)

exc_miss_empl_noga <- LSE_cluster_cons %>%
  mutate(
    above = case_when(
      MW_bins %in% c("[min, -600)", "[-600, 0)") ~0,
      MW_bins == "[0, 600)" ~1,
      TRUE~NA_real_)
  ) %>%
  filter(!is.na(above))%>%
  group_by(noga, erhebja, above) %>%
  summarise(
    n_workers = sum(gewicht, na.rm = TRUE), 
    employment = sum(gewibgrs, na.rm = TRUE),
    gap_std = gap_std[1],
    cluster_3 = cluster_3[1],
    .groups = "drop"
  ) %>%
  group_by(noga, above) %>%
  arrange(erhebja) %>%
  mutate(
    empl_2012 = dplyr::first(employment[erhebja == -1], default = NA_real_),
    diff_empl = ifelse(!is.na(employment) & !is.na(empl_2012),
                       employment - empl_2012, NA_real_),
    diff_type = ifelse(above == 1, "Excess", "Missing"),
    .groups = "drop")
exc_miss_empl_noga_wide <- exc_miss_empl_noga %>%
  select(c(diff_empl, above, erhebja, noga)) %>%
  pivot_wider(names_from = above, names_prefix = "jobs_", values_from = diff_empl)

subsectors_demograph <- LSE_cluster_cons %>%
  group_by(erhebja, noga) %>%
  mutate(
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"])/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"])/sum(gewicht),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    mean_small_firms =sum(gewicht[firm_size == "small (1-49)"])/sum(gewicht)
  )

empl_cengiz <- left_join(subsectors_demograph, exc_miss_empl_noga_wide, by = c("noga", "erhebja"))
table(empl_cengiz$cluster_3)

Excess <- feols(
  jobs_1 ~ i(erhebja, gap_std, ref = -1) + i(erhebja, ref = -1) +
    perc_male + perc_resident + perc_nomgt + mean_small_firms +
    perc_lowskill +mean_age + mean_tenure | msreg_an, 
  weights = ~gewicht,
  cluster = ~noga, 
  data = empl_cengiz[empl_cengiz$cluster_3 != "Not exposed", ])

coefs_Excess <- tidy(Excess) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_std") %>%
      as.numeric()) %>%
  filter(str_detect(term, ".*gap_std"))
ggplot(coefs_Excess, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error)) +
  labs(x = "Event time", y = "Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = c(-5, -3, -1, 1, 3, 5))
ggsave("Excess_noga_gap_cluster_3_cons.pdf")

missing_jobs <- feols(
  jobs_0 ~ i(erhebja, gap_std, ref = -1) + i(erhebja, ref = -1) +
    perc_male + perc_resident + perc_nomgt + mean_small_firms +
    perc_lowskill +mean_age + mean_tenure  | msreg_an, 
  cluster = ~noga, 
  weights = ~gewicht,
  data = empl_cengiz[empl_cengiz$cluster_3 != "Not exposed", ])
coefs_missing_jobs <- tidy(missing_jobs) %>%
  filter(str_detect(term, "erhebja::.*")) %>%
  mutate(
    year = str_remove_all(term, "erhebja::|:gap_std") %>%
      as.numeric()) %>%
  filter(str_detect(term, ".*gap_std"))
ggplot(coefs_missing_jobs, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error)) +
  labs(x = "Event time", y = "Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = c(-5, -3, -1, 1, 3, 5))
ggsave("missing_jobs_noga_gap_cluster_3_cons.pdf")

## together
coefs_missing_jobs$var <- "Missing"
coefs_Excess$var <- "Excess"
coefs_employment_cengiz <- rbind(coefs_missing_jobs, coefs_Excess)

ggplot(coefs_employment_cengiz, aes(x = year, y = estimate, colour = var, fill = var)) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_line(, show.legend = FALSE) +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error),
              alpha = 0.2, colour = NA) +
  labs(x = "Event time", y = "Coefficient", fill = "Employment", colour = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(-5, -3, -1, 1, 3, 5)) + 
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))+
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) + 
  theme(legend.position = "none")
ggsave("jobs_noga_gap_cluster_3_cons.pdf")

#legend
my_legend <- ggplot(coefs_employment_cengiz, aes(x = year, y = estimate, colour = var, fill = var)) +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
              alpha = 0.2, colour = NA) +
  labs(x = "Event time", y = "Coefficient", fill = "Employment", colour = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))+
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) + 
  theme(legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"), legend.text=element_text(size=10),
        legend.key.height = unit(1, "cm"), legend.key.width = unit(2, "cm"))
legend <- cowplot::get_legend(my_legend)
grid.newpage()
grid.draw(legend)
ggsave("jobs_noga_gap_cluster_3_legend_cons.pdf", 
       plot = legend, 
       device = "pdf")

## -------------------------------## -------------------------------

#chracteristics by treatment group
#also include cba yes/no, and wage levels p10, p30, p50
char_table_noga <- LSE_cluster_cons %>%
  filter(erhebja %in% c(2010, 2012) & !is.na(cluster_3)) %>%
  group_by(noga) %>%
  summarise(
    percentile = wtd.mean(gross_wage_std <= min_wage, 
                          weight = gewicht, na.rm = TRUE) * 100,
    percentil_cons = wtd.mean(gross_wage_std <= min_wage, 
                              weight = gewicht, na.rm = TRUE) * 100,
    noga_size = sum(gewicht, na.rm = TRUE),
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"])/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"])/sum(gewicht),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    cba = sum(gewicht[mem_cba=="1"])/sum(gewicht),
    p10 = wtd.quantile(real_wage, weights = gewicht, 
                       probs = 0.1, na.rm = TRUE),
    min_wage = wtd.mean(min_wage, weight = gewicht, na.rm = TRUE),
    A = sum(gewicht[wage_region == "A"])/sum(gewicht),
    B = sum(gewicht[wage_region == "B"])/sum(gewicht),
    C = sum(gewicht[wage_region == "C"])/sum(gewicht),
    cluster_3 = cluster_3[1]) %>%
  group_by(cluster_3) %>%
  summarise(
    "P10 wage" = mean(p10, na.rm = TRUE),
    "Minimum wage" = mean(min_wage, na.rm = TRUE),
    "Bite of MW" = mean(percentile, na.rm = TRUE),
    "CBA coverage" = mean(cba, na.rim = TRUE)*100,
    "% men" = mean(perc_male, na.rm = TRUE)*100,
    "% CH residents" = mean(perc_resident, na.rm = TRUE)*100,
    "Mean age" =mean(mean_age, na.rm = TRUE),
    "Mean tenure" = mean(mean_tenure, na.rm = TRUE), 
    "% low-skilled" = mean(perc_lowskill, na.rm = TRUE)*100,
    "% no management" = mean(perc_nomgt, na.rm = TRUE)*100,
    "Sub-sector size" = mean(noga_size, na.rm = TRUE),
    "Wage region A" = mean(A, na.rm = TRUE)*100,
    "Wage region B" = mean(B, na.rm = TRUE)*100,
    "Wage region C" = mean(C, na.rm = TRUE)*100,
    "N" = sum(!is.na(cluster_3))) %>% ungroup() %>% #N = (# of sub-sectors)
  mutate_if(is.numeric, round, digits = 0) %>%
  pivot_longer(
    cols = -cluster_3,
    names_to = "variable",
    values_to = "value") %>%
  pivot_wider(
    names_from = cluster_3,
    values_from = value) %>%
  rename(" " = variable) 

stargazer(char_table_noga, type = "latex",
          title = "Characteristics by exposure group (2010, 2012)",
          label = "tab:chars_noga_cluster_3",
          summary = FALSE, rownames = FALSE, align = TRUE, no.space = FALSE,
          font.size = "small", column.sep.width = "3pt", out = "chars_noga_cluster_3.tex")








#-----------------------------
#delete

# info on sub-sectoral level
LSE_08_18_noga_cons <- LSE_08_18 %>%
  group_by(erhebja, noga) %>%
  mutate(
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"])/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"])/sum(gewicht),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    mean_small_firms =sum(gewicht[firm_size == "small (1-49)"])/sum(gewicht),
    noga_size = sum(gewibgrs)
  )

LSE_08_18 <-LSE_08_18%>%
  mutate(
    wage_region = factor(wage_region),
    wage_region = relevel(wage_region, ref = "C"),
    ausbild = factor(ausbild),
    ausbild = relevel((ausbild), ref = "8"),
    natkat = factor(natkat),
    natkat = relevel((natkat), ref = "1")   )