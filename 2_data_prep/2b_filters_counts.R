## ---------------------------
##
## Master thesis
## Filter dataset
## Author: Katja Hager
##
## Date Created: June 2025
##
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation

library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(Hmisc)
library(stargazer)

## ---------------------------
LSE_08_18 <- readRDS(file = "output/data/2a_merged_all_sectors.rds")
LSE_08_18$gewicht <- as.numeric(LSE_08_18$gewicht)
LSE_08_18$ibgr <- as.numeric(LSE_08_18$ibgr)
#age is probably a sensitive variable!

## --------------------------
#create wage var (wage for 100% work, gross, nominal)
#use this var to compare worker's wage to minimum wage
#used for filtering

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    blimok = as.numeric(blimok),
    mbls = as.numeric(mbls),
    zulagen = as.numeric(zulagen), #Zulagen für Schicht-, Sonntags- und Nachtarbeit
    xiiimloh = as.numeric(xiiimloh),
    sonderza = as.numeric(sonderza),
    iwaz = as.numeric(iwaz), #individuelle wöchentl. Arbeitszeit -> Stunden/Woche
    bezstd = as.numeric(bezstd) ,
    FTE_hours = case_when(
    !startsWith(as.character(noga), "25")& metal==1 ~ 42,
    TRUE ~40 )) #watch and MEM have 40hrs-week, and other metal sub-sectors also
table(LSE_08_18$FTE_hours)

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    gross_wage_std = case_when(
      iwaz>0 & FTE_hours==42 ~((blimok*42)) / (iwaz),
      bezstd>0& FTE_hours==42~((blimok*42) * (4+(1/3))) / (bezstd),
      iwaz>0 ~((blimok) * 40) / (iwaz),
      bezstd>0 ~((blimok) * 40 * (4+(1/3))) / (bezstd),      
      TRUE ~ NA_real_),
    base_wage_gross_std = ifelse(iwaz>1 | bezstd>1, 
                                 mbls-zulagen - 1/12*xiiimloh - 1/12*sonderza,
                                 NA) )
summary(LSE_08_18$gross_wage_std)

## --------------------------

#for checking the numbers: extract main MEM data
data_MEM <- LSE_08_18 %>%
  filter(mem==1 | mem_2012==1)

data_watch <- LSE_08_18 %>%
  filter(watch==1 | watch_2012==1)

## --------------------------
#initialize empty dataframe
counts <- data.frame(erhebja = numeric(0), count_n = numeric(0), 
                     condition = character(0))


#before filtering
for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "1) MEM", 
                        count_n = sum(data_MEM$erhebja==i))
  counts <- rbind(counts, new_row) #append
}

## --------------------------

#filter 1: NAs
#fehlende Werte bei Lohn (blimok)
data_MEM$blimok <- as.numeric(data_MEM$blimok)
sum(data_MEM$blimok == 0) #14
sum(is.na(data_MEM$gross_wage_std) | data_MEM$gross_wage_std == 0 ) #61 obs

##mostly lohnver is missing -> keep!
table(data_MEM$lohnver) #NA: 30123 + 1359 
sum(data_MEM$lohnver == "-9" | data_MEM$lohnver=="") #31482

#demographics
data_MEM$alter <- as.numeric(data_MEM$alter)
summary(data_MEM$alter)
sum(is.na(data_MEM$alter) | data_MEM$alter <0) #29
table(data_MEM$natkat)
sum(data_MEM$natkat=="-9") #22 obs
data_MEM$dienstja <- as.numeric(data_MEM$dienstja)
summary(data_MEM$dienstja)
sum(is.na(data_MEM$dienstja) | data_MEM$dienstja <0 ) #254 obs
table(data_MEM$geschle) #no NAs

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "2) NAs", 
                        count_n = sum(data_MEM$erhebja==i 
                                      & !is.na(data_MEM$gross_wage_std) & data_MEM$gross_wage_std >0
                                      & !is.na(data_MEM$alter) & data_MEM$alter >0 
                                      & data_MEM$natkat!="-9" & !is.na(data_MEM$natkat)
                                      & !is.na(data_MEM$dienstja) & data_MEM$dienstja >=0 
                                      & !is.na(data_MEM$geschle)))
  counts <- rbind(counts, new_row) #append
}

data_MEM <- data_MEM %>%
  filter(!is.na(gross_wage_std) & gross_wage_std >0
         & !is.na(alter) & alter >0
         & natkat!="-9" & !is.na(natkat)
         & !is.na(dienstja) & dienstja >= 0)

sum(!is.na(data_MEM$gross_wage_std)) #613966 obs

#filter 2: Only employees 19 to 65 years old are included.
#SECO: only employees aged 19-65 are taken into account. https://entsendung.admin.ch/Lohnrechner/home
sum(data_MEM$alter > 65 | data_MEM$alter < 19) #3744

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "3) Age", 
                        count_n = sum(data_MEM$erhebja==i 
                                      & data_MEM$alter <= 65 & data_MEM$alter >= 19))
  counts <- rbind(counts, new_row) #append
}

data_MEM <- data_MEM %>%
  filter(alter <= 65 & alter>=19)

sum(!is.na(data_MEM$alter)) #610222 obs

#filter: exclude Lernende und Praktikanten (Art des Vertrags: FS03 == 6|7)
table(data_MEM$fs03)
sum(data_MEM$fs03 %in% c("6", "7"), na.rm =TRUE) #0


#filter 3: Beschäftigungsgrad pro Job <5% or > 150%, or >200% total employment across several jobs
#individual employment level
data_MEM$ibgr <- as.numeric(data_MEM$ibgr)
sum(is.na(data_MEM$ibgr))#23
summary(data_MEM$ibgr)

data_MEM <- data_MEM %>%
  group_by(erhebja, avs_nb) %>%
  mutate(count = n(),
         total_empl = sum(ibgr),
         total_empl = ifelse(erhebja>2010, total_empl, ibgr)) %>%
  select(-c(count))

#check <- data_MEM %>%   filter(total_empl>2)
#sum(check$gross_wage_std<3800, na.rm = TRUE)
sum(data_MEM$ibgr <0.05, na.rm = TRUE) #903 -> such small jobs we do not want to look at
sum(data_MEM$ibgr >1.5, na.rm = TRUE) #21 -> such large jobs we do not want to look at
sum(data_MEM$total_empl > 2, na.rm = TRUE) #5710

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "4) Employment", 
                        count_n = sum(data_MEM$erhebja==i &  data_MEM$ibgr >= 0.05 & 
                                        data_MEM$ibgr <= 1.5 & data_MEM$total_empl <= 2,
                                      na.rm = TRUE))
  counts <- rbind(counts, new_row) #append
}

data_MEM <- data_MEM %>%
  filter(ibgr >= 0.05 & ibgr <= 1.5 & total_empl <= 2)

sum(!is.na(data_MEM$ibgr)) #603564


#filter 4: Implausible combinations of employment & income (probably internship / apprenticeship or sth like that)
sum(data_MEM$blimok < 1000 & data_MEM$ibgr > 0.8, na.rm = TRUE) #700 obs

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "5) Implausible", 
                        count_n = sum(data_MEM$erhebja==i &  
                                        !(data_MEM$blimok < 1000 & data_MEM$ibgr > 0.8)))
  counts <- rbind(counts, new_row) #append
}

data_MEM <- data_MEM %>%
  filter(!(blimok < 1000 & ibgr > 0.8)) #602864 obs

#filter 5: duplicates -> 2012 drops much because of duplicates
data_MEM <- data_MEM %>%
  distinct() #602k

LSE_pre_2012 <- data_MEM %>%
  filter(erhebja<2012) %>%
  mutate(only_firm_panel = 1)

#from 2012
#127 workers are twice in the data, with the same firm (484 obs)
#do not drop, but use only for firm-level analysis
LSE_from_2012 <- data_MEM %>%
  filter(erhebja>=2012) %>% 
  group_by(avs_nb, entid, erhebja) %>% 
  mutate(count = n(),
         only_firm_panel = ifelse(count>1, 1, 0)) %>%
  select(-c(count))
table(LSE_from_2012$only_firm_panel)

data_MEM <- rbind(LSE_from_2012, LSE_pre_2012) #602177
table(data_MEM$only_firm_panel)

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "6) Duplicates", count_n = sum(data_MEM$erhebja==i))
  counts <- rbind(counts, new_row) #append
}

#filter 6: Hoch- und Tiefstlöhne, gewichteter FTE Medianlohn
#influence of extreme/implausible values is reduced by excluding 0.5% lowest and highest wages 
#(https://entsendung.admin.ch/Lohnrechner/home)
#we cap the top 0.5% since we are interested in the bottom of the wage distribution
#and discard the bottom 0.25%  (since probably outliers?)
yearly_med_wages <- data_MEM %>%
  filter(mem==1) %>%
  group_by(erhebja) %>%
  summarise(
    high_thresh = (wtd.quantile(gross_wage_std, weights = gewicht, probs = c(0.995), na.rm = TRUE)),
    low_thresh = (wtd.quantile(gross_wage_std, weights = gewicht, probs = c(0.0025), na.rm = TRUE)) )

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "7) Wage cap", 
                        count_n = sum(data_MEM$erhebja==i & 
                                        data_MEM$gross_wage_std < 
                                        yearly_med_wages$high_thresh[yearly_med_wages$erhebja==i] &
                                        data_MEM$gross_wage_std > 
                                        yearly_med_wages$low_thresh[yearly_med_wages$erhebja==i],
                                      na.rm = TRUE))
  counts <- rbind(counts, new_row) #append
}

data_MEM <- left_join(data_MEM, yearly_med_wages, by = "erhebja") %>%
  filter(gross_wage_std < high_thresh &
           gross_wage_std > low_thresh)

sum(!is.na(data_MEM$gross_wage_std)) #596897 obs

## --------------------------
#reshape and save counts file
counts_wide <- counts %>%
  pivot_wider(names_from = erhebja,
              values_from= c(count_n)) %>%
  rename(Filter = condition) %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = "'")))

stargazer(counts_wide, type = "latex",
          title = "Number of observations after filtering",
          label = "tab:2b_filters_counts",
          summary = FALSE, rownames = FALSE, digits = 2, no.space = TRUE,
          font.size = "small", column.sep.width = "3pt",
          out = "output/table/2b_filters_counts.tex")

yearly_med_wages_table <- yearly_med_wages %>% mutate(high_thresh = round(as.numeric(high_thresh),0), 
                                                      low_thresh = round(as.numeric(low_thresh),0),
                                                      erhebja = as.character(erhebja)) %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = "'")))
stargazer(yearly_med_wages_table, type = "latex", title = "7) Thresholds for wage cap",
          label = "tab:2b_wage_caps", summary = FALSE, no.space = TRUE,
          font.size = "small", column.sep.width = "3pt", 
          out = "output/table/2b_wage_caps.tex")

## --------------------------
#WATCH sector

#initialize empty dataframe
counts <- data.frame(erhebja = numeric(0), count_n = numeric(0), 
                     condition = character(0))
#before filtering
for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "1) Watch", 
                        count_n = sum(data_watch$erhebja==i))
  counts <- rbind(counts, new_row) }

## --------------------------

#filter 1: NAs
for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "2) NAs", 
                        count_n = sum(data_watch$erhebja==i 
                                      & !is.na(data_watch$gross_wage_std) & data_watch$gross_wage_std >0
                                      & !is.na(data_watch$alter) & data_watch$alter >0 
                                      & data_watch$natkat!="-9" & !is.na(data_watch$natkat)
                                      & !is.na(data_watch$dienstja) & data_watch$dienstja >=0 
                                      & !is.na(data_watch$geschle)))
  counts <- rbind(counts, new_row) #append
}

data_watch <- data_watch %>%
  filter(!is.na(gross_wage_std) & gross_wage_std >0
         & !is.na(alter) & alter >0
         & natkat!="-9" & !is.na(natkat)
         & !is.na(dienstja) & dienstja >= 0)

sum(!is.na(data_watch$gross_wage_std)) #613966 obs

#filter 2: Only employees 19 to 65 years old are included.
#SECO: only employees aged 19-65 are taken into account. https://entsendung.admin.ch/Lohnrechner/home
for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "3) Age", 
                        count_n = sum(data_watch$erhebja==i 
                                      & data_watch$alter <= 65 & data_watch$alter >= 19))
  counts <- rbind(counts, new_row) #append
}

data_watch <- data_watch %>%
  filter(alter <= 65 & alter>=19)

sum(!is.na(data_watch$alter)) #610222 obs

#filter: exclude Lernende und Praktikanten (Art des Vertrags: FS03 == 6|7)
table(data_watch$fs03)
sum(data_watch$fs03 %in% c("6", "7"), na.rm =TRUE) #0

#filter 3: Beschäftigungsgrad pro Job <5% or > 150%, or >200% total employment across several jobs
#individual employment level
data_watch$ibgr <- as.numeric(data_watch$ibgr)

data_watch <- data_watch %>%
  group_by(erhebja, avs_nb) %>%
  mutate(count = n(),
         total_empl = sum(ibgr),
         total_empl = ifelse(erhebja>2010, total_empl, ibgr)) %>%
  select(-c(count))

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "4) Employment", 
                        count_n = sum(data_watch$erhebja==i &  data_watch$ibgr >= 0.05 & 
                                        data_watch$ibgr <= 1.5 & data_watch$total_empl <= 2,
                                      na.rm = TRUE))
  counts <- rbind(counts, new_row) }

data_watch <- data_watch %>%
  filter(ibgr >= 0.05 & ibgr <= 1.5 & total_empl <= 2)

sum(!is.na(data_watch$ibgr)) #603564

#filter 4: Implausible combinations of employment & income (probably internship / apprenticeship or sth like that)
for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "5) Implausible", 
                        count_n = sum(data_watch$erhebja==i &  
                                        !(data_watch$blimok < 1000 & data_watch$ibgr > 0.8)))
  counts <- rbind(counts, new_row) }

data_watch <- data_watch %>%
  filter(!(blimok < 1000 & ibgr > 0.8)) #602864 obs

#filter 5: duplicates -> 2012 drops much because of duplicates
data_watch <- data_watch %>%
  distinct() #602k

LSE_pre_2012 <- data_watch %>%
  filter(erhebja<2012) %>%
  mutate(only_firm_panel = 1)

#from 2012
LSE_from_2012 <- data_watch %>%
  filter(erhebja>=2012) %>% 
  group_by(avs_nb, entid, erhebja) %>% 
  mutate(count = n(),
         only_firm_panel = ifelse(count>1, 1, 0)) %>%
  select(-c(count))
table(LSE_from_2012$only_firm_panel)

data_watch <- rbind(LSE_from_2012, LSE_pre_2012) #602177
table(data_watch$only_firm_panel)

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "6) Duplicates", count_n = sum(data_watch$erhebja==i))
  counts <- rbind(counts, new_row) }

#filter 6: Hoch- und Tiefstlöhne, gewichteter FTE Medianlohn
yearly_med_wages <- data_watch %>%
  filter(watch==1) %>%
  group_by(erhebja) %>%
  summarise(
    high_thresh = (wtd.quantile(gross_wage_std, weights = gewicht, probs = c(0.995), na.rm = TRUE)),
    low_thresh = (wtd.quantile(gross_wage_std, weights = gewicht, probs = c(0.0025), na.rm = TRUE)) )

for (i in seq(2008, 2018, 2)) {
  new_row <- data.frame(erhebja = i, condition = "7) Wage cap", 
                        count_n = sum(data_watch$erhebja==i & 
                                        data_watch$gross_wage_std < 
                                        yearly_med_wages$high_thresh[yearly_med_wages$erhebja==i] &
                                        data_watch$gross_wage_std > 
                                        yearly_med_wages$low_thresh[yearly_med_wages$erhebja==i],
                                      na.rm = TRUE))
  counts <- rbind(counts, new_row) }

data_watch <- left_join(data_watch, yearly_med_wages, by = "erhebja") %>%
  filter(gross_wage_std < high_thresh &
           gross_wage_std > low_thresh)

sum(!is.na(data_watch$gross_wage_std)) #596897 obs

## --------------------------
#reshape and save counts file
counts_wide <- counts %>%
  pivot_wider(names_from = erhebja,
              values_from= c(count_n)) %>%
  rename(Filter = condition) %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = "'")))

stargazer(counts_wide, type = "latex",
          title = "Number of observations after filtering",
          label = "tab:2b_filters_counts_watch",
          summary = FALSE, rownames = FALSE, digits = 2, no.space = TRUE,
          font.size = "small", column.sep.width = "3pt",
          out = "output/table/2b_filters_counts_watch.tex")

yearly_med_wages_table <- yearly_med_wages %>% mutate(high_thresh = round(as.numeric(high_thresh),0), 
                                                      low_thresh = round(as.numeric(low_thresh),0),
                                                      erhebja = as.character(erhebja)) %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = "'")))
stargazer(yearly_med_wages_table, type = "latex", title = "7) Thresholds for wage cap",
          label = "tab:2b_wage_caps_watch", summary = FALSE, no.space = TRUE,
          font.size = "small", column.sep.width = "3pt", 
          out = "output/table/2b_wage_caps_watch.tex")
