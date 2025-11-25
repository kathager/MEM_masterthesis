## ---------------------------
##
## Master thesis
## Quality checks for variables and different definitions
## Author: Katja Hager
##
## Date Created: September 2025
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
## ---------------------------
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")

#nationality -> how robust are the categories I made?
#share of L-permit in that categroy small, always below 9% // C-permits around 20% of permanent
levels(LSE_08_18$natkat2)
L_permit <- LSE_08_18 %>%
  filter(natkat2=="B-/L-permit") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(natkat==2) %>%
  summarise(total_L = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_L/total, categ = "% L-permit among temorary") %>%
  select(c(share, categ, erhebja))
C_permit <- LSE_08_18 %>%
  filter(natkat2=="CH/C-permit") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(natkat==4) %>%
  summarise(total_C = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_C/total, categ = "% C-permit among permanent") %>%
  select(c(share, categ, erhebja))
permits <- rbind(C_permit, L_permit)

ggplot(data = permits, aes(x = erhebja, y = share, colour = categ)) +
  geom_line() + geom_point() +
  labs(x = "Year", y = "%", colour = NULL) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.6, 0.5),
        legend.background = element_rect(colour = "white"),
        legend.text=element_text(size=8),
        legend.key.height = unit(0.7, "cm"),
        legend.key.width = unit(0.5, "cm"))+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))
ggsave("output/graph/robust_natkat2.pdf", width = 18, height = 10, units = "cm")

## -----------------------------
#firm_size 1 and 3
levels(LSE_08_18$firm_size)
levels(LSE_08_18$k_untgroe)

small_firms <- LSE_08_18 %>%
  filter(firm_size=="small (1-49)") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(k_untgroe=="1-20") %>% #smallest firms
  summarise(total_smallest = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_smallest/total, categ = "% firms<21 among firms<50") %>%
  select(c(share, categ, erhebja))
large_firms <- LSE_08_18 %>%
  filter(firm_size=="large (250+)") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(k_untgroe=="1000+") %>% #largest firms
  summarise(total_large = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_large/total, categ = "% firms>1000 among firms>250") %>%
  select(c(share, categ, erhebja))
firm_size <- rbind(small_firms, large_firms)

ggplot(data = firm_size, aes(x = erhebja, y = share, colour = categ)) +
  geom_line() + geom_point() +
  labs(x = "Year", y = "%", colour = NULL) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.7, 0.5),
        legend.background = element_rect(colour = "white"),
        legend.key.height = unit(0.7, "cm"),
        legend.text=element_text(size=8),
        legend.key.width = unit(0.5, "cm"))+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))
ggsave("output/graph/robust_firm_size.pdf", width = 18, height = 10, units = "cm")

## -----------------------------
#low- and high-skilled
levels(LSE_08_18$qualification)
levels(LSE_08_18$ausbild)

low_skilled_gymi <- LSE_08_18 %>%
  filter(qualification=="low skilled") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(ausbild==5) %>% #nur Gymi
  summarise(total_gymi = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_gymi/total, categ = "% high-school among low-skilled") %>%
  select(c(share, categ, erhebja))
low_skilled_internal <- LSE_08_18 %>%
  filter(qualification=="low skilled") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(ausbild==7) %>% #nur unternehmensinterne Ausbildung
  summarise(total_intern = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_intern/total, categ = "% internal training among low") %>%
  select(c(share, categ, erhebja))
low_skilled_sek <- LSE_08_18 %>%
  filter(qualification=="low skilled") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(ausbild==8) %>% #nur Sek
  summarise(total_sek = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_sek/total, categ = "% secondary edu among low") %>%
  select(c(share, categ, erhebja))
high_skilled_HF <- LSE_08_18 %>%
  filter(qualification=="high skilled") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(ausbild==3) %>% #HF
  summarise(total_HF = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_HF/total, categ = "% HF among high-skilled") %>%
  select(c(share, categ, erhebja))

skills <- rbind(low_skilled_gymi, low_skilled_internal, low_skilled_sek, high_skilled_HF)
ggplot(data = skills, aes(x = erhebja, y = share, colour = categ)) +
  geom_line() + geom_point() +
  labs(x = "Year", y = "%", colour = NULL) +
  theme_minimal() + 
  theme(legend.background = element_rect(colour = "white"),
        legend.key.height = unit(0.7, "cm"),
        legend.text=element_text(size=8),
        legend.key.width = unit(0.5, "cm"))+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(4))
ggsave("output/graph/robust_skill.pdf", width = 20, height = 10, units = "cm")
## -----------------------------
#job_position based on berufst (management)
#low- and high-skilled
levels(LSE_08_18$job_position)
levels(LSE_08_18$berufst)

lower_manag <- LSE_08_18 %>%
  filter(job_position=="Lower Management") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(berufst=="4") %>% #4 Unterstes Kader
  summarise(total_lowest = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_lowest/total, categ = "% lowest management among low") %>%
  select(c(share, categ, erhebja))
high_manag <- LSE_08_18 %>%
  filter(job_position=="Higher Management") %>%
  group_by(erhebja) %>%
  mutate(total = sum(gewicht, na.rm = TRUE)) %>%
  filter(berufst=="2") %>% #2 mittleres Kader
  summarise(total_middle = sum(gewicht, na.rm = TRUE),
            total = mean(total)) %>%
  mutate(share = total_middle/total, categ = "% middle management among high") %>%
  select(c(share, categ, erhebja))

management <- rbind(lower_manag, high_manag)
ggplot(data = management, aes(x = erhebja, y = share, colour = categ)) +
  geom_line() + geom_point() +
  labs(x = "Year", y = "%", colour = NULL) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.9),
        legend.background = element_rect(colour = "white"),
        legend.key.height = unit(0.7, "cm"),
        legend.text=element_text(size=8),
        legend.key.width = unit(0.5, "cm"))+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))
ggsave("output/graph/robust_management.pdf", width = 18, height = 10, units = "cm")

## ----------------------------- ## -----------------------------
#people below 1.05*MW
table(LSE_08_18$min_wage_worker)
data_below_MW <- LSE_08_18[LSE_08_18$min_wage_worker=="Minimum wage worker", ]
data_below_MWhigh <- LSE_08_18[LSE_08_18$min_wage_worker_high=="Minimum wage worker", ]
datasets_MW <- list(low = data_below_MW, high = data_below_MWhigh)

MW_statistics <- data.frame()
for (data in c("low", "high")) { 
  sample <- datasets_MW[[data]]
  
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
      mean_employment = round(weighted.mean(ibgr, w = gewicht, na.rm = TRUE)*100, 1),
      n_obs = n(),
      .groups = 'drop') %>%
    mutate(group = paste0("MW_", data))
  
  MW_statistics <- bind_rows(MW_statistics, statistics_overall)
  
  }

#for 2012 (pre-policy)
MW_statistics_2012 <- MW_statistics[MW_statistics$erhebja==2012 
                                          & !is.na(MW_statistics$erhebja), ]
MW_statistics_2012$erhebja <- NULL #drop year

# Create the wide format table
MW_statistics_2012_wide <- MW_statistics_2012 %>%
  # Select key variables and rename them to match your table labels
  select(group, 
         perc_male, mean_employment, mean_age, mean_tenure,
         perc_small,perc_med, perc_large, #firm_size
         perc_low, perc_middle, perc_high, #education
         CH_C,temp_L_B, grenzg, #nationality
         mgmnt_high, mgmnt_low, mgmnt_no,
         n_obs
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
    variable == "mean_age" ~ "Age",
    variable == "mean_tenure" ~ "Tenure",
    variable == "n_obs" ~ "Number of observations",
    TRUE ~ variable
  )) %>%
  select(-variable) %>%
  mutate_if(is.numeric, round, digits = 1) %>% #round values!
  # Reorder columns in the desired sequence
  select(Characteristic, MW_low, MW_high)

stargazer(MW_statistics_2012_wide,
          type = "latex",
          title = "Definitions of minimum wage workers (2012)",
          label = "tab:MW_high_low_2012",
          digits = 2,
          font.size = "small",
          summary = FALSE,
          rownames = FALSE,
          out = "output/table/MW_high_low_2012.tex")

#check definition of high minimum wage

#sensitivity to dienstja >9 or dienstja > 15
check_tenure <- LSE_08_18 %>%
  filter(min_wage_high !=MW_high_cons) %>% #63477 obs
  filter(gross_wage_std <= 1.25*min_wage_high) #18538
summary(check_tenure$gross_wage_std) #it is relevant for lower wage workers

#correlation between min_wage_high and min_wage
cor(LSE_08_18$min_wage, LSE_08_18$min_wage_high, use = "complete.obs") #0.7342
var(LSE_08_18$min_wage, na.rm = TRUE) #26385
var(LSE_08_18$min_wage_high, na.rm = TRUE) #48505 -> much more variation!

# variation by year
variation_by_year <- LSE_08_18 %>%
  group_by(erhebja) %>%
  summarise(
    var_min_wage = var(min_wage, na.rm = TRUE),
    var_min_wage_high = var(min_wage_high, na.rm = TRUE),
    cor_wages = cor(min_wage, min_wage_high, use = "complete.obs"),
    ratio = var_min_wage/var_min_wage_high,
    .groups = "drop"
  )
print(variation_by_year)
# correlation over time
ggplot(variation_by_year, aes(x = erhebja, y = cor_wages)) +
  geom_line() +
  labs (y = "Correlation", x = "Year")

# variation by year
variation_by_year_weighted <- LSE_08_18 %>%
  filter(!is.na(min_wage), !is.na(min_wage_high), !is.na(gewicht)) %>%
  group_by(erhebja) %>%
  summarise(
    cor_wages = cov.wt(cbind(min_wage, min_wage_high), 
                       wt = gewicht, 
                       cor = TRUE)$cor[1,2],
    .groups = "drop" )
print(variation_by_year_weighted)
# correlation over time
ggplot(variation_by_year_weighted, aes(x = erhebja, y = cor_wages)) +
  geom_line() +
  labs (y = "Correlation", x = "Year")

##----------------------------
#2) cba = 1: check sensitivity of cba=1 definition
cba_firm_raw <- LSE_08_18 %>%
  filter(mem==1) %>%
  mutate(
    gewicht = as.numeric(gewicht)) %>%
  group_by(burnr, erhebja) %>%
  summarise(
    cba_raw = ifelse((any(lohnver == 1)), 1, 0),
    covered_within = wtd.mean(lohnver == 1, weight = gewicht, na.rm=TRUE),
    covered_sum = sum(gewicht[lohnver == 1]),
    total_sum = sum(gewicht),
    mem_cba = ifelse(covered_within<0.25 | cba_raw==0, 0, cba_raw), #assign cba=0 to those obs
    cba = ifelse(mem_cba==1, "covered", "not covered"),
    broad_cba = sum(lohnver!=4 & !is.na(lohnver), na.rm = TRUE) /
      sum(!is.na(lohnver), na.rm = TRUE),
    not_individual = ifelse(broad_cba >=0.5, 1, 0)
  )
not_covered <- cba_firm_raw %>%
  filter(covered_within<=0.25 & cba_raw==1)
sum(!is.na(not_covered$burnr)) #52 obs
length((unique(not_covered$burnr))) #43 firms

not_covered_lower <- cba_firm_raw %>%
  filter(covered_within<=0.2 & cba_raw==1) #45 obs
length((unique(not_covered_lower$burnr))) #37 firms

print(unique(not_covered$erhebja)) #in 2 years: 2008, 2010

CBA_definition_yrly <- LSE_08_18 %>%
  group_by(burnr, erhebja) %>%
  filter(!is.na(mem_cba)) %>%
  mutate(
    cba_raw = ifelse((any(lohnver == 1)), 1, 0),
    covered_within = wtd.mean(lohnver == 1, weight = gewicht, na.rm=TRUE),
    .groups = "drop") %>%
  group_by(erhebja)%>%
  summarise(
    mean_within_raw = round(wtd.mean(covered_within[cba_raw==1], weights = gewicht[cba_raw==1]), 3),
    mean_within = round(wtd.mean(covered_within[mem_cba==1], weights = gewicht[mem_cba==1]),3),
    .groups = "drop") %>%
  rename(Year = erhebja, "any employee covered" = mean_within_raw, 
         "at least 25% covered" = mean_within ) 

stargazer(CBA_definition_yrly,
          type = "latex",
          title = "Average CBA coverage within firms by year",
          label = "tab:cba_coverage",digits = 2,font.size = "small",
          summary = FALSE,rownames = FALSE,
          out = "output/table/cba_lohnver_coverage.tex")


CBA_variation_by_year <- LSE_08_18 %>%
  group_by(erhebja) %>%
  filter(!is.na(mem_cba), !is.na(lohnver), !is.na(gewicht)) %>%
  summarise(
    cor_CBA = cov.wt(cbind(mem_cba==1, lohnver==1), 
                     wt = gewicht, 
                     cor = TRUE)$cor[1,2],
    .groups = "drop"
  ) 
print(CBA_variation_by_year)

CBA_variation <- LSE_08_18 %>%
  filter(!is.na(mem_cba), !is.na(lohnver), !is.na(gewicht)) %>%
  summarise(
    cor_CBA_all = cov.wt(cbind(mem_cba==1, lohnver==1), 
                         wt = gewicht, 
                         cor = TRUE)$cor[1,2],
    cor_CBA_unweigted_all = cor(mem_cba==1, lohnver==1),
    .groups = "drop"
  )
print(CBA_variation)

