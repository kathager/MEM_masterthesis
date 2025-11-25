## ---------------------------
##
## Master thesis
## Descriptives
#  Graphs and tables: Who are the min wage workers?
## Author: Katja Hager
##
## Date Created: June 2025
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
library(reshape2)
library(stargazer)
theme_set(theme_minimal())
## ---------------------------
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")
## ---------------------------
#table for categorical variables
#MEM overall (cba/noCBA) // MW workers (cba/noCBA)

#statistics for each year
LSE_08_18$ibgr <- as.numeric(LSE_08_18$ibgr)
data_min_wage <- LSE_08_18[LSE_08_18$gross_wage_std<=LSE_08_18$min_wage & 
                             LSE_08_18$sector=="MEM", ]
datasets <- list(full = LSE_08_18[LSE_08_18$sector=="MEM",], MW = data_min_wage)

#Secondary Education (\%)
#1&2&4 + 3 #Uni, PH, Höhere Berufsausbildung // #6 Lehre // 5&7&8 keine oder nicht anerkannte Berufsausbildung, nur Gymi
#Immigration status (\%) natkat
#CH + C-Ausweis: 1, 4 // 2 + 3: Kurzaufenthalt, B-Ausw. // 5: Grenzgänger
#mean employment degree (%) 1 = 100%.

all_statistics <- data.frame()
for (data in c("full", "MW")) {
  sample <- datasets[[data]] 
  
  statistics_overall <- sample  %>%
    group_by(erhebja) %>%
    summarise(
      #gender
      perc_male = round(weighted.mean(geschle=="men", w=gewicht, na.rm=TRUE)*100, 1),
      #age
      mean_age = round(weighted.mean(alter, w = gewicht, na.rm = TRUE), 1),
      #tenure
      mean_tenure = round(weighted.mean(dienstja, w = gewicht,na.rm = TRUE), 1),
      #firm size
      perc_small = round(weighted.mean(firm_size == "small (1-49)", w = gewicht, na.rm = TRUE) * 100,1),
      perc_med = round(weighted.mean(firm_size == "medium (50-249)", w = gewicht, na.rm = TRUE) * 100,1),
      perc_large = round(weighted.mean(firm_size == "large (250+)", w = gewicht, na.rm = TRUE) * 100,1),
      #education(qualification)
      perc_low = round(weighted.mean(qualification== "low skilled", w = gewicht,
                                     na.rm = TRUE) * 100, 1),
      perc_middle = round(weighted.mean(qualification== "middle skilled", w = gewicht,
                                        na.rm = TRUE) * 100, 1),
      perc_high = round(weighted.mean(qualification== "high skilled", w = gewicht,
                                      na.rm = TRUE) * 100 , 1),
      #immigration
      CH_C = round(weighted.mean(natkat2 =="CH/C-permit", w = gewicht,
                                 na.rm = TRUE) * 100, 1),
      temp_L_B = round(weighted.mean(natkat2 =="B-/L-permit", w = gewicht,
                                     na.rm = TRUE) * 100, 1),
      grenzg = round(weighted.mean(natkat2 =="cross-border", w = gewicht,
                                   na.rm = TRUE) * 100, 1),
      #job_position
      mgmnt_high = round(weighted.mean(job_position=="Higher Management", w = gewicht,
                                       na.rm = TRUE) * 100, 1),
      mgmnt_low = round(weighted.mean(job_position=="Lower Management", w = gewicht,
                                      na.rm = TRUE) * 100, 1),
      mgmnt_no = round(weighted.mean(job_position=="No Management", w = gewicht,
                                     na.rm = TRUE) * 100, 1),
      #mean employment
      mean_employment = round(weighted.mean(ibgr, w = gewicht,
                                            na.rm = TRUE)*100, 1),
      #age
      young = round(weighted.mean(age=="Young", w = gewicht, na.rm = TRUE) * 100, 1),
      middle = round(weighted.mean(age=="Middle", w = gewicht, na.rm = TRUE) * 100, 1),
      old = round(weighted.mean(age=="Old", w = gewicht, na.rm = TRUE) * 100, 1),
      
      #tenure
      short = round(weighted.mean(tenure=="Long", w = gewicht, na.rm = TRUE) * 100, 1),
      shortlong = round(weighted.mean(tenure=="Middle", w = gewicht, na.rm = TRUE) * 100, 1),
      long = round(weighted.mean(tenure=="Short", w = gewicht, na.rm = TRUE) * 100, 1),
      
      n_obs = n(),
      .groups = 'drop') %>%
    mutate(group = paste0("overall_", data))
  
  statistics_by_cba <- sample %>%
    group_by(erhebja, cba) %>%
    summarise(
      #gender
      perc_male = round(weighted.mean(geschle=="men", w=gewicht, na.rm=TRUE)*100, 1),
      #age
      mean_age = round(weighted.mean(alter, w = gewicht, na.rm = TRUE), 1),
      #tenure
      mean_tenure = round(weighted.mean(dienstja, w = gewicht,na.rm = TRUE), 1),
      #firm size
      perc_small = round(weighted.mean(firm_size == "small (1-49)", w = gewicht, na.rm = TRUE) * 100,1),
      perc_med = round(weighted.mean(firm_size == "medium (50-249)", w = gewicht, na.rm = TRUE) * 100,1),
      perc_large = round(weighted.mean(firm_size == "large (250+)", w = gewicht, na.rm = TRUE) * 100,1),
      #education(qualification)
      perc_low = round(weighted.mean(qualification== "low skilled", w = gewicht,
                                     na.rm = TRUE) * 100, 1),
      perc_middle = round(weighted.mean(qualification== "middle skilled", w = gewicht,
                                        na.rm = TRUE) * 100, 1),
      perc_high = round(weighted.mean(qualification== "high skilled", w = gewicht,
                                      na.rm = TRUE) * 100 , 1),
      #immigration
      CH_C = round(weighted.mean(natkat2 =="CH/C-permit", w = gewicht,
                                 na.rm = TRUE) * 100, 1),
      temp_L_B = round(weighted.mean(natkat2 =="B-/L-permit", w = gewicht,
                                     na.rm = TRUE) * 100, 1),
      grenzg = round(weighted.mean(natkat2 =="cross-border", w = gewicht,
                                   na.rm = TRUE) * 100, 1),
      #job_position
      mgmnt_high = round(weighted.mean(job_position=="Higher Management", w = gewicht,
                                       na.rm = TRUE) * 100, 1),
      mgmnt_low = round(weighted.mean(job_position=="Lower Management", w = gewicht,
                                      na.rm = TRUE) * 100, 1),
      mgmnt_no = round(weighted.mean(job_position=="No Management", w = gewicht,
                                     na.rm = TRUE) * 100, 1),
      #mean employment
      mean_employment = round(weighted.mean(ibgr, w = gewicht,
                                            na.rm = TRUE)*100, 1),
      #age
      young = round(weighted.mean(age=="Young", w = gewicht, na.rm = TRUE) * 100, 1),
      middle = round(weighted.mean(age=="Middle", w = gewicht, na.rm = TRUE) * 100, 1),
      old = round(weighted.mean(age=="Old", w = gewicht, na.rm = TRUE) * 100, 1),
      
      #tenure
      short = round(weighted.mean(tenure=="Long", w = gewicht, na.rm = TRUE) * 100, 1),
      shortlong = round(weighted.mean(tenure=="Middle", w = gewicht, na.rm = TRUE) * 100, 1),
      long = round(weighted.mean(tenure=="Short", w = gewicht, na.rm = TRUE) * 100, 1),
      n_obs = n(),
      .groups = 'drop') %>%
    mutate(group = paste0(cba, "_", data))
  
  all_statistics <- bind_rows(all_statistics, statistics_overall)
  all_statistics <- bind_rows(all_statistics, statistics_by_cba)
}

#for 2012 (pre-policy)
all_statistics_2012 <- all_statistics[all_statistics$erhebja==2012 & !is.na(all_statistics$erhebja), ]
all_statistics_2012$erhebja <- NULL #drop year

# Create the wide format table
all_statistics_2012_wide <- all_statistics_2012 %>%
  # Select key variables and rename them to match your table labels
  select(group, 
         perc_male, young, middle, old, #gender, age
         short, shortlong, long, #tenure
         perc_small,perc_med, perc_large, #firm_size
         perc_low, perc_middle, perc_high, #education
         CH_C,temp_L_B, grenzg, #nationality
         mgmnt_high, mgmnt_low, mgmnt_no,
         mean_employment, n_obs
  ) %>%
  # Reshape to wide format
  pivot_longer(cols = -group, 
               names_to = "variable", 
               values_to = "value") %>%
  pivot_wider(names_from = group, 
              values_from = value) %>%
  # Create proper row labels
  mutate(Characteristic = case_when(
    variable == "perc_male" ~ "Male",
    variable == "young" ~ "Young (age 18-29 yrs)",
    variable == "middle" ~ "Middle (age 30-49 yrs)",
    variable == "old" ~ "Old (age 50-65 yrs)",
    variable == "short" ~ "Junior (tenure 0-3 yrs)",
    variable == "shortlong" ~ "Mid-level (tenure 4-9 yrs)",
    variable == "long" ~ "Senior (tenure 10+ yrs)",
    variable == "perc_high" ~ "High Skilled",
    variable == "perc_middle" ~ "Middle Skilled",
    variable == "perc_low" ~ "Low Skilled",
    variable == "CH_C" ~ "Permanent residents", 
    variable == "temp_L_B" ~ "Temporary residents", 
    variable == "grenzg" ~ "Cross-border", 
    variable == "perc_small" ~ "Small firms (1-49)",
    variable == "perc_med" ~ "Medium firms (50-249)",
    variable == "perc_large" ~ "Large Firms (250+)",
    variable == "mgmnt_high" ~ "Higher Management" ,
    variable == "mgmnt_low" ~ "Lower Management" ,
    variable == "mgmnt_no" ~ "No Management" ,
    variable == "mean_employment" ~ "Employment" ,
    variable == "n_obs" ~ "Number of observations",
    TRUE ~ variable
  )) %>%
  select(-variable) %>%
  mutate_if(is.numeric, round, digits = 1) %>% #round values!
  # Reorder columns in the desired sequence
  select(Characteristic, 'Not covered_full', 'CBA covered_full', 
         'Not covered_MW', 'CBA covered_MW' )

stargazer(all_statistics_2012_wide,
          type = "latex",
          align = TRUE,  # This might help with alignment
          column.labels = c("CBA-covered", "Non-covered"),
          column.separate = c(2, 2),  # Groups columns
          title = "Worker Characteristics (2012)",
          label = "tab:Workers_2012_characteristics_all",
          summary = FALSE,
          rownames = FALSE,
          digits = 2,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          out = "output/table/Workers_2012_characteristics_all.tex")

## -----------------------------------------------------------------------------
#what firms are in the market?
#Firm size
firm_size_by_cba <- LSE_08_18 %>%
  group_by(erhebja, burnr) %>%
  slice(1) %>%
  group_by(erhebja, cba) %>%
  summarise(perc_small = round(mean(firm_size == "small (1-49)", na.rm = TRUE) * 100,1),
            perc_med = round(mean(firm_size == "medium (50-249)", na.rm = TRUE) * 100,1),
            perc_large = round(mean(firm_size == "large (250+)", na.rm = TRUE) * 100,1),
            n_obs = n(),
            .groups = 'drop') %>%
  mutate(group = cba) %>%
  mutate_if(is.numeric, round, digits = 1)

#Variation in wages: Coefficient of Variation
#Min-wage workers (\%)
wage_var_MW_by_cba <- LSE_08_18 %>%
  group_by(erhebja, burnr) %>%
  #both variance and mean within each firm-year
  mutate(var_wage = wtd.var(real_wage, weights = gewicht, na.rm = TRUE), 
         mean_wage = weighted.mean(real_wage, w = gewicht, na.rm = TRUE),
         cv_wage = sqrt(var_wage) / mean_wage,  # Coefficient of variation
         min_wage_w = weighted.mean(min_wage_worker == "Minimum wage worker", 
                                    w = gewicht, na.rm = TRUE) * 100,
         .groups = 'drop') %>%
  group_by(erhebja, cba) %>%
  summarise(min_wage = mean(min_wage_w, na.rm = TRUE),
            mean_cv_wage = round(mean(cv_wage, na.rm = TRUE), 2),
            .groups = 'drop') %>%
  rename(group = cba) %>%
  mutate_if(is.numeric, round, digits = 2)

table_firms <- full_join(firm_size_by_cba, wage_var_MW_by_cba, by = c("erhebja", "group"))

#Wage below min / gap measure
gap_by_cba <- LSE_08_18 %>%
  group_by(erhebja, burnr) %>%
  slice(1) %>%
  group_by(erhebja, cba) %>%
  summarise(mean_gap = mean(gap_exposure, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(group = cba,
         mean_gap = ifelse(!is.na(mean_gap), mean_gap, 0),
         cba = NULL) %>%
  mutate_if(is.numeric, round, digits = 3)

table_firms <- full_join(table_firms, gap_by_cba, by = c("erhebja", "group"))

#at least 1 MW worker
atleast1MW_by_cba <- LSE_08_18 %>%
  filter(min_wage_worker == "Minimum wage worker") %>%
  group_by(cba, erhebja) %>%
  mutate(total = sum(!is.na(burnr))) %>%
  group_by(erhebja, burnr) %>%
  slice(1) %>%
  group_by(erhebja, cba) %>%
  summarise(atleast1MW = 100*sum(!is.na(burnr))/total[1],
            .groups = 'drop') %>%
  mutate(group = cba, cba = NULL) %>%
  mutate_if(is.numeric, round, digits =1)

table_firms <- full_join(table_firms, atleast1MW_by_cba, by = c("erhebja", "group"))

#for 2012 (pre-policy)
table_firms_2012 <- table_firms[table_firms$erhebja==2012 & !is.na(table_firms$erhebja), ]

#drop year
table_firms_2012$erhebja <- NULL

# Create the wide format table
table_firms_wide_2012 <- table_firms_2012 %>%
  # Select key variables and rename them to match your table labels
  select(group, 
         min_wage,atleast1MW,
         mean_cv_wage, mean_gap,
         perc_small,perc_med,perc_large,
         n_obs) %>%
  # Reshape to wide format
  pivot_longer(cols = -group, 
               names_to = "variable", 
               values_to = "value") %>%
  pivot_wider(names_from = group, 
              values_from = value) %>%
  # Create proper row labels
  mutate(Characteristic = case_when(
    variable == "perc_small" ~ "Small firms (1-49) (%)",
    variable == "perc_med" ~ "Medium firms (50-250) (%)",
    variable == "perc_large" ~ "Large firms (250'+) (%)",
    variable == "mean_cv_wage" ~ "Firm wage dispersion",
    variable == "min_wage" ~ "Minimum Wage Workers (%)",
    variable == "atleast1MW"~ "At least 1 MW employed (%)",
    variable == "mean_gap"~ "Gap measure",
    variable == "n_obs" ~ "Number of observations",
    TRUE ~ variable
  )) %>%
  select(-variable) %>%
  select(Characteristic, 'CBA covered', 'Not covered')

stargazer(table_firms_wide_2012,
          type = "latex",
          title = "Firm Characteristics (2012)",
          label = "tab:firms_2012_characteristics",
          summary = FALSE,
          rownames = FALSE,
          digits = 3,
          align = TRUE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          out = "output/table/firms_2012_characteristics.tex")

## ---------------------------------------------------------------
#in what sectors and regions are the MW workers concentrated?

total_MW <- LSE_08_18 %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(min_wage_worker == "Minimum wage worker") %>% 
  summarise(MW = sum(gewicht, na.rm = TRUE),
            share = MW / total[1])

cba_MW <- LSE_08_18 %>%
  filter(min_wage_worker == "Minimum wage worker") %>% 
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(cba == "CBA covered") %>%
  summarise(cba = sum(gewicht, na.rm = TRUE),
            share = 100*(cba/total[1]))
  

MW_sectors <- LSE_08_18 %>%
  group_by(erhebja, noga) %>%
  mutate(total_weight = sum(gewicht, na.rm = TRUE)) %>%
  filter(min_wage_worker == "Minimum wage worker") %>%
  group_by(erhebja, noga) %>%
  summarise(MW = sum(gewicht, na.rm = TRUE),
            total = total_weight[1],
            share = 100*(MW/total))

MW_sectors_2012 <- MW_sectors %>%
  filter(erhebja == 2012) %>%
  arrange(desc(share))  %>%
  mutate(total = NULL,
         'Absolute MW' = MW,
         'Share MW (%)' = share,
         MW = NULL,
         share = NULL) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  slice_head(n=15)
MW_sectors_2012$erhebja <- NULL

stargazer(MW_sectors_2012,
          type = "latex",
          title = "Sub-sectors with high share of MW workers (2012)",
          label = "tab:MW_sectors_top15_2012",
          summary = FALSE,
          rownames = FALSE,
          digits = 2,
          align = TRUE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          out = "MW_sectors_top15_2012.tex")

## -----------

MW_regions <- LSE_08_18 %>%
  group_by(erhebja, msreg_an) %>%
  mutate(total_weight = sum(gewicht, na.rm = TRUE)) %>%
  filter(min_wage_worker == "Minimum wage worker") %>%
  group_by(erhebja, msreg_an) %>%
  summarise(MW = sum(gewicht, na.rm = TRUE),
            total = total_weight[1],
            share = 100*(MW/total))

MW_regions_2012 <- MW_regions %>%
  filter(erhebja == 2012) %>%
  filter(!is.na(msreg_an)) %>%
  arrange(desc(share))  %>%
  mutate(total = NULL,
         'Absolute MW' = MW,
         'Share MW (%)' = share,
         MW = NULL,
         share = NULL) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  slice_head(n=15)
MW_regions_2012$erhebja <- NULL

stargazer(MW_regions_2012,
          type = "latex",
          title = "Regions with high share of MW workers (2012)",
          label = "tab:MW_regions_top15_2012",
          summary = FALSE,
          rownames = FALSE,
          digits = 2,
          align = TRUE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          out = "MW_regions_top15_2012.tex")

MW_cantons <- LSE_08_18 %>%
  group_by(erhebja, arbkto) %>%
  mutate(total_weight = sum(gewicht, na.rm = TRUE)) %>%
  filter(min_wage_worker == "Minimum wage worker") %>%
  group_by(erhebja, arbkto) %>%
  summarise(MW = sum(gewicht, na.rm = TRUE),
            total = total_weight[1],
            share = 100*(MW/total))
MW_cantons_2012 <- MW_cantons %>%
  filter(erhebja == 2012) %>%
  arrange(desc(share))  %>%
  mutate(total = NULL,
         'Absolute MW' = MW,
         'Share MW (%)' = share,
         MW = NULL,
         share = NULL) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  slice_head(n=5)
MW_cantons_2012$erhebja <- NULL

stargazer(MW_cantons_2012,
          type = "latex",
          title = "Cantons with high share of MW workers (2012)",
          label = "tab:MW_cantons_top5_2012",
          summary = FALSE,
          rownames = FALSE,
          digits = 2,
          align = TRUE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          out = "MW_cantons_top5_2012.tex")



## ---------------------------
#5.3.2 Wage regions
#which have the same information / pattern across regions?
MW_regions <- LSE_08_18 %>%
  filter(erhebja %in% c(2010, 2012))%>%
  group_by(erhebja, msreg_an) %>%
  mutate(total_weight = sum(gewicht, na.rm = TRUE)) %>%
  filter(min_wage_worker == "Minimum wage worker") %>%
  group_by(erhebja, msreg_an) %>%
  summarise(MW = sum(gewicht, na.rm = TRUE),
            total = total_weight[1],
            share = 100*(MW/total))

bite_regional <- LSE_08_18 %>%
  filter(erhebja %in% c(2010, 2012))%>%
  group_by(erhebja, msreg_an) %>%
  summarise(
    med_wage = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE),
    #what is the assigned minimum wage to the workforce (since there are several)
    min_wage = weighted.mean(min_wage_high, weights = gewicht, na.rm = TRUE),
    Kaitz_index = min_wage/med_wage,
    wages_below = sum(dist_to_MW_high[dist_to_MW_high <=0 & cba=="CBA covered"] * 
                        gewicht[dist_to_MW_high <=0 & cba=="CBA covered"]),
    wage_bill = sum(real_wage * gewicht),
    gap = abs(wages_below/wage_bill)*100,
    .groups = "drop") %>%
  group_by(msreg_an) %>%
  mutate (
    gap_10_12_region = mean(gap, na.rm = TRUE))

regional$gap_10_12_region[regional$msreg_an==95] <- 1  

regional_patterns <- left_join(MW_regions, bite_regional, by = c("erhebja", "msreg_an"))
regional_patterns$gap_10_12_region[regional_patterns$msreg_an==95] <- 1

corr_reg <- regional_patterns %>%
  filter(erhebja==2012) %>%
  mutate(
    '# MW' = MW,
    '% MW' = share,
    'Med wage' = med_wage,
    'Min wage' = min_wage,
    Kaitz = Kaitz_index,
    'Gap 2012' = gap,
    'Gap 2010/2012' = gap_10_12_region) %>%
  select(-msreg_an, -erhebja, -wages_below, -wage_bill, -total, -MW, -share, 
         -med_wage, Kaitz_index, -gap, -gap_10_12_region, -min_wage, -Kaitz_index)
corr_reg$erhebja <- NULL

cor_mat <- cor(corr_reg, use = "pairwise.complete.obs")
stargazer(cor_mat,
          type = "latex",
          title = "Regional correlations (2012)",
          label = "tab:reg_corr_2012",
          summary = FALSE,
          rownames = TRUE,
          digits = 3,
          align = TRUE,
          no.space = TRUE,
          font.size = "small",
          column.sep.width = "3pt",
          out = "reg_corr_2012.tex")

