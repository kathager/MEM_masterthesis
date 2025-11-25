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
#CBA: Mindestlöhne beziehen sich auf ein volles Arbeitspensum, verstehen sich als Bruttolöhne
#create wage var (wage for 100% work, gross, nominal)
#use this var to compare worker's wage to minimum wage
#used for filtering
LSE_08_18 <- LSE_08_18 %>%
  mutate( blimok = as.numeric(blimok),
    mbls = as.numeric(mbls),
    zulagen = as.numeric(zulagen), #Zulagen für Schicht-, Sonntags- und Nachtarbeit
    xiiimloh = as.numeric(xiiimloh),
    sonderza = as.numeric(sonderza),
    iwaz = as.numeric(iwaz), #individuelle wöchentl. Arbeitszeit -> Stunden/Woche
    bezstd = as.numeric(bezstd)  ) #bezahlte Stunden -> Stunden/Monat

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    gross_wage_std = case_when(
      iwaz>1 ~(blimok * 40) / iwaz, #only if people work more than 1hr per week -> also against measurement error
      bezstd>1 ~((blimok) * 40 * (4+(1/3))) / (bezstd), #only if people work more than 1hr per month -> also against measurement error
      TRUE ~ NA_real_),
    base_wage_gross_std = ifelse(iwaz>1 | bezstd>1, 
                                 mbls-zulagen - 1/12*xiiimloh - 1/12*sonderza,
                                 NA),
    diff_base_blimok = gross_wage_std - base_wage_gross_std )

summary(LSE_08_18$gross_wage_std)
summary(LSE_08_18$base_wage_gross_std)
summary(LSE_08_18$diff_base_blimok)

sum(LSE_08_18$base_wage_gross_std<=0, na.rm = TRUE)

## --------------------------
#for checking the numbers: extract main MEM data
data_MEM <- LSE_08_18 %>%
  filter(mem==1 | mem_2012==1)

data_watch <- LSE_08_18 %>%
  filter(watch==1 | watch_2012==1)

## --------------------------
#filter the total dataset

LSE_08_18 <- LSE_08_18 %>%
  filter(!is.na(gross_wage_std) & gross_wage_std >0
         & !is.na(alter) & alter >0
         & natkat!="-9" & !is.na(natkat)
         & !is.na(dienstja) & dienstja >= 0)  %>%
  filter(alter <= 65 & alter>=19)

LSE_08_18 <- LSE_08_18 %>%
  group_by(erhebja, avs_nb) %>%
  mutate(count = n(),
         total_empl = sum(ibgr),
         total_empl = ifelse(erhebja>2010, total_empl, ibgr)) %>%
  select(-c(count)) %>%
  filter(ibgr >= 0.05 & ibgr <= 1.5 & total_empl <= 2)%>%
  filter(!(blimok < 1000 & ibgr > 0.8)) 

LSE_08_18 <- LSE_08_18 %>%
  distinct()

LSE_pre_2012 <- LSE_08_18 %>%
  filter(erhebja<2012) %>%
  mutate(only_firm_panel = 1)

#from 2012
LSE_from_2012 <- LSE_08_18 %>%
  filter(erhebja>=2012) %>% 
  group_by(avs_nb, entid, erhebja) %>% 
  mutate(count = n(),
         only_firm_panel = ifelse(count>1, 1, 0)) %>%
  select(-c(count))
table(LSE_from_2012$only_firm_panel)

LSE_08_18 <- rbind(LSE_from_2012, LSE_pre_2012)

table(LSE_08_18$mem, LSE_08_18$watch)

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    sector = case_when(
      mem==1 ~"MEM",
      watch==1 ~"Watch",
      TRUE ~"Rest")  ) 

#med wages
LSE_08_18 <- LSE_08_18 %>%
  group_by(erhebja, sector) %>%
  mutate(
    high_thresh = wtd.quantile(gross_wage_std, weights = gewicht, probs = c(0.995), na.rm = TRUE),
    low_thresh = wtd.quantile(gross_wage_std, weights = gewicht, probs = c(0.0025), na.rm = TRUE) )

LSE_08_18 <- LSE_08_18 %>% filter(
  gross_wage_std < high_thresh &
    gross_wage_std > low_thresh)

#save filtered LSE data
saveRDS(LSE_08_18, file = "output/data/2b_LSE_filtered.rds")


#filter not only for gross_wage_std, but also for blimok (e.g. blimok < 500)?? -> random jobs.
summary(LSE_08_18$low_thresh)
summary(LSE_08_18$gross_wage_std)

summary(LSE_08_18$blimok)
table(LSE_08_18$sector[LSE_08_18_check$blimok<=500 & LSE_08_18_check$gross_wage_std>5000])

