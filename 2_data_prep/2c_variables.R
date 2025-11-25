## ---------------------------
##
## Master thesis
## Create variables: min wage; distance to min wage; low-wage work; wage bins
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

## ---------------------------
#load filtered MEM-sector data
LSE_08_18 <- readRDS(file = "output/data/2b_LSE_filtered.rds")

## ---------------------------
summary(LSE_08_18$diff_base_blimok)

table(LSE_08_18$geschle)
LSE_08_18$geschle <- as.factor(LSE_08_18$geschle)
levels(LSE_08_18$geschle)
LSE_08_18$geschle <- recode_factor(LSE_08_18$geschle, "1" = "men", "2" = "women")

LSE_08_18$geschle <- relevel(as.factor(LSE_08_18$geschle), ref = "men")
summary(LSE_08_18$geschle)
## ---------------------------
#size of firm
#2008, 2010: untgroe -> continuous
#2012-2018: k_untgroe -> categorical
table(LSE_08_18$k_untgroe) #small values -> small firms

LSE_08_18$untgroe <- as.numeric(LSE_08_18$untgroe)
summary(LSE_08_18$untgroe)

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    k_untgroe = case_when(
      untgroe<=20 & !is.na(untgroe) ~ "1",
      untgroe>20 & untgroe <= 49 & !is.na(untgroe) ~ "2",
      untgroe>49 & untgroe <= 250 & !is.na(untgroe) ~ "3",
      untgroe>250 & untgroe <= 1000 & !is.na(untgroe) ~ "4",
      untgroe>1000 & !is.na(untgroe) ~ "5",
      TRUE ~ as.character(k_untgroe)  
    ))
table(LSE_08_18$k_untgroe)

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    firm_size = case_when(
      k_untgroe %in% c("1", "2") ~ "1", 
      k_untgroe %in% c("3") ~ "2",
      k_untgroe %in% c("4", "5") ~ "3"  )
  )
table(LSE_08_18$firm_size)

LSE_08_18$firm_size <- as.factor(LSE_08_18$firm_size)

LSE_08_18$k_untgroe <- recode_factor(LSE_08_18$k_untgroe, 
                                     "1" = "1-20", 
                                     "2" = "20-49",
                                     "3" = "50-250",
                                     "4" = "250-1000",
                                     "5" = "1000+")
summary(LSE_08_18$k_untgroe)

LSE_08_18$firm_size <- recode_factor(LSE_08_18$firm_size, 
                                     "1" = "small (1-49)", 
                                     "2" = "medium (50-249)",
                                     "3" = "large (250+)")
summary(LSE_08_18$firm_size)

## ---------------------------
##MEM-CBA coverage
#define coverage: in MEM sector AND covered by collective bargaining

##variable of interest: lohnver
table(LSE_08_18$lohnver) 
LSE_08_18$lohnver[LSE_08_18$lohnver==""] <- "-9"
LSE_08_18$lohnver[LSE_08_18$lohnver=="-9"] <- NA
LSE_08_18$lohnver <- as.factor(LSE_08_18$lohnver)

levels(LSE_08_18$lohnver)

##coverage status of each firm, in a year
#reminder: burnr is "Betriebsstätte" for 2008-2010, and firm from 2012
cba_firm_raw_MEM <- LSE_08_18 %>%
  filter(mem==1) %>%
  mutate(
    gewicht = as.numeric(gewicht)) %>%
  group_by(burnr, erhebja) %>%
  summarise(
    cba_raw = ifelse((any(lohnver == 1)), 1, 0),
    covered_within = wtd.mean(lohnver == 1, weight = gewicht, na.rm=TRUE),
    covered_sum = sum(gewicht[lohnver == 1]),
    total_sum = sum(gewicht),
    cba = ifelse(covered_within<0.25 | cba_raw==0 , 0, cba_raw),
    broad_cba = sum(lohnver!=4 & !is.na(lohnver), na.rm = TRUE) /
      sum(!is.na(lohnver), na.rm = TRUE),
    not_individual = ifelse(broad_cba >=0.5, 1, 0) )

cba_firm_raw_watch <- LSE_08_18 %>%
  filter(watch==1) %>%
  mutate(
    gewicht = as.numeric(gewicht)) %>%
  group_by(burnr, erhebja) %>%
  summarise(
    cba_raw = ifelse((any(lohnver == 1)), 1, 0),
    covered_within = wtd.mean(lohnver == 1, weight = gewicht, na.rm=TRUE),
    covered_sum = sum(gewicht[lohnver == 1]),
    total_sum = sum(gewicht),
    cba = ifelse(covered_within<0.25 | cba_raw==0 , 0, cba_raw),
    broad_cba = sum(lohnver!=4 & !is.na(lohnver), na.rm = TRUE) /
      sum(!is.na(lohnver), na.rm = TRUE),
    not_individual = ifelse(broad_cba >=0.5, 1, 0) )

cba_firms_raw <- rbind(cba_firm_raw_watch, cba_firm_raw_MEM)

LSE_08_18 <- left_join(LSE_08_18, 
                       cba_firms_raw %>%select(c("cba", "burnr", "erhebja", "not_individual")), 
                       by = c("burnr", "erhebja"))

#check distribution (unweighted)
table(LSE_08_18$cba, LSE_08_18$erhebja)  
table(LSE_08_18$erhebja, LSE_08_18$lohnver)

## ---------------------------
#qualification level (relevant also for min wage)
table(LSE_08_18$ausbild)
LSE_08_18$ausbild[LSE_08_18$ausbild=="" | LSE_08_18$ausbild==-9] <- NA

#1&2&4 + 3 #Uni, PH, #3 #Höhere Berufsausbildung
#6 Lehre
# 5& 7&8 keine oder nicht anerkannte Berufsausbildung, nur Gymi
# ausbild level 1-3: Uni bis Höhere Berufsausbildung -> "qualifiziert
# ausbild = 4, 5: Lehrerpatent, Matura -> probably not "qualifiziert" in MEM sector
# ausbild = 6: EFZ, aber auch EBA -> nicht zwingend "qualifiziert", but maybe yes
# ausbild = 7, 8: "unqualifiziert"
# ausbild = 9: not in LSE Codebook

LSE_08_18 <- LSE_08_18 %>%
  mutate(qualification = case_when(
    ausbild %in% c(1, 2, 3, 4) ~ 1, #high skilled
    ausbild == 6 ~ 2, #middle qualified, incl. Berufslehre -> relevant for MW
    ausbild %in% c(5, 7, 8) ~ 3, #low skilled
    TRUE ~ 3  # for all other cases, assign to low-skilled
  ))
table(LSE_08_18$qualification, LSE_08_18$erhebja)

LSE_08_18$qualification[LSE_08_18$qualification==1] <- "high skilled" 
LSE_08_18$qualification[LSE_08_18$qualification==2] <- "middle skilled" 
LSE_08_18$qualification[LSE_08_18$qualification==3] <- "low skilled" 

LSE_08_18$qualification <- relevel(as.factor(LSE_08_18$qualification), 
                                   ref = "high skilled")
levels(LSE_08_18$qualification)

## ---------------------------
#position held (BERUFST) -> relevant for min wage
#1 Oberes Kader, 2 Mittleres Kader, 3 Unteres Kader, 4 Unterstes Kader, 5 Ohne Kaderfunktion
table(LSE_08_18$berufst, LSE_08_18$erhebja)
#Kaderfunktion -> CHF 300.- higher MW
LSE_08_18$berufst <- relevel(as.factor(LSE_08_18$berufst), ref = "5")
LSE_08_18$berufst[LSE_08_18$berufst == "" | LSE_08_18$berufst == "-9"] <- NA
levels(LSE_08_18$berufst)

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    job_position = case_when(
      berufst %in% c("1", "2") ~ "Higher Management",
      berufst %in% c("3", "4") ~ "Lower Management",
      berufst %in% c("5") ~ "No Management"),
    job_position = as.factor(job_position)
  )
table(LSE_08_18$job_position)

## ---------------------------
#years of experience (dienstja) and age categories
LSE_08_18 <- LSE_08_18 %>%
  mutate(
    dienstja = as.numeric(dienstja),
    tenure = case_when(
      dienstja <=3 ~ "Short",
      dienstja >3 & dienstja <=9 ~"Middle",
      dienstja >9 ~ "Long",
      TRUE ~ NA),
    alter = as.numeric(alter),
    age = case_when(
        alter >=18 & alter <=29 ~ "Young",
        alter >29 & alter <=50 ~"Middle",
        alter >50 & alter<=65 ~ "Old",
        TRUE ~ NA)  )

table(LSE_08_18$age)
table(LSE_08_18$tenure)

## ---------------------------
##define minimum wage
#assign CHF 300.- higher min wage to workers with high education, position, tenure, ...
##minimum wages by qualification level + years of experience + firm position 
LSE_08_18 <- LSE_08_18 %>%
  mutate(
    min_wage_high = ifelse(qualification %in% c(1, 2) #education
                                | dienstja > 9 
                                | berufst %in% c(1, 2, 3, 4), 
                                min_wage + 300, 
                                min_wage),
    MW_high_cons = ifelse(qualification %in% c(1, 2) #education
                                | dienstja > 15 
                                | berufst %in% c(1, 2, 3, 4), 
                                min_wage + 300, 
                                min_wage))
table(LSE_08_18$min_wage)
table(LSE_08_18$min_wage_high)
table(LSE_08_18$MW_high_cons)

sum(LSE_08_18$mem==0) #23210
sum(is.na((LSE_08_18$min_wage_high))) #23210 -> good

## ---------------------------
##min wage worker
LSE_08_18$min_wage_worker <- as.factor(ifelse(LSE_08_18$gross_wage_std<=1.05*LSE_08_18$min_wage,
                                              "Minimum wage worker", "Regular worker"))
LSE_08_18$min_wage_worker_high <- as.factor(ifelse(LSE_08_18$gross_wage_std<=1.05*LSE_08_18$min_wage_high,
                                              "Minimum wage worker", "Regular worker"))

LSE_08_18$min_wage_worker_basewage <- as.factor(ifelse(LSE_08_18$base_wage_gross_std<=1.05*LSE_08_18$min_wage,
                                              "Minimum wage worker", "Regular worker"))

## ---------------------------
##wage variables: real wages, wage cushions, distance to MW
#real wages / real minimum wages: base 100%: 01.12.2010 -> real wage w.r.t. 01.12.2010
LSE_08_18 <- LSE_08_18 %>%
  mutate(
    #real wages
    real_wage = (gross_wage_std/`Dez 2010=100`)*100,
    real_min_wage = (min_wage/`Dez 2010=100`)*100,
    real_min_wage_high = (min_wage_high/`Dez 2010=100`)*100,
    #wage cushion
    cushion = ifelse(gross_wage_std !=0, log(gross_wage_std) - log(min_wage), NA),
    cushion_high = ifelse(gross_wage_std !=0, log(gross_wage_std) - log(min_wage_high), NA),
    #distance to min wage
    dist_to_MW = gross_wage_std - min_wage,
    dist_to_MW_high = gross_wage_std - min_wage_high,
    dist_base_to_MW = base_wage_gross_std - min_wage)
   
#real wages      
summary(LSE_08_18$real_wage)
#wage cushions
summary(LSE_08_18$cushion)

sum(is.na(LSE_08_18$dist_to_MW))
## ---------------------------
##wage bins 
summary(LSE_08_18$dist_to_MW) 
summary(LSE_08_18$dist_to_MW_high)

sum(LSE_08_18$dist_to_MW<300 & LSE_08_18$erhebja==2012, na.rm = TRUE) #6978
sum(LSE_08_18$dist_to_MW<0 & LSE_08_18$erhebja==2012, na.rm = TRUE) #4101
sum(LSE_08_18$dist_to_MW<=-300 & LSE_08_18$erhebja==2012, na.rm = TRUE) #2691
sum(LSE_08_18$dist_to_MW<=-600 & LSE_08_18$erhebja==2012, na.rm = TRUE) #1742

sum(LSE_08_18$dist_to_MW_high<300 & LSE_08_18$erhebja==2012, na.rm = TRUE) #8133
sum(LSE_08_18$dist_to_MW_high<0 & LSE_08_18$erhebja==2012, na.rm = TRUE) #4806
sum(LSE_08_18$dist_to_MW_high<=-300 & LSE_08_18$erhebja==2012, na.rm = TRUE) #3079
sum(LSE_08_18$dist_to_MW_high<=-600 & LSE_08_18$erhebja==2012, na.rm = TRUE) #2047

#make bins and then barplot, but different regions next to each other
LSE_08_18 <- LSE_08_18 %>%
  mutate(
    MW_bins = cut(dist_to_MW,
                      breaks = c(-Inf, -600, -300, 0, 300, 600, 1200,
                                 2000, 3000, Inf),
                      include.lowest = TRUE,
                      labels = c("[min, -600)", "[-600, -300)", "[-300, 0)", "[0, 300)", 
                                 "[300, 600)", "[600, 1200)", "[1200, 2000)", 
                                 "[2000, 3000)", "3000+") ),
    MW_bins_high = cut(dist_to_MW_high,
                       breaks = c(-Inf, -600, -300, 0, 300, 600, 1200,
                                  2000, 3000, Inf),
                       include.lowest = TRUE,
                       labels = c("[min, -600)", "[-600, -300)", "[-300, 0)","[0, 300)", 
                                  "[300, 600)", "[600, 1200)", "[1200, 2000)", 
                                  "[2000, 3000)", "3000+") ),
    MW_bins_coarse = cut(dist_to_MW,
                  breaks = c(-Inf, 0, 600, Inf),
                  include.lowest = TRUE,
                  labels = c("[min, 0)", "[0, 600)", "600+") ),
    MW_bins_coarsest = cut(dist_to_MW,
                  breaks = c(-Inf, -600, -300, 0, 300, 600, 1200,
                             2000, 3000, Inf),
                  include.lowest = TRUE,
                  labels = c("[min, -600)", "[-600, -300)", "[-300, 0)", "[0, 300)", 
                             "[300, 600)", "[600, 1200)", "[1200, 2000)", 
                             "[2000, 3000)", "3000+") ))

#when evauating: do not use not simple frequency -> weight the frequency! 
#personenbezogen, eher inkl. employment

##----------------------------
##nationality
table(LSE_08_18$natkat)
LSE_08_18$natkat[LSE_08_18$natkat == "-9" | LSE_08_18$natkat == "6"] <- NA
LSE_08_18$natkat <- as.factor(LSE_08_18$natkat)

#group together
LSE_08_18$natkat2 <- NA
LSE_08_18$natkat2[LSE_08_18$natkat %in% c(1, 4)] <- "CH/C-permit"
LSE_08_18$natkat2[LSE_08_18$natkat%in% c(2, 3)] <- "B-/L-permit" #temporary and seasonal work
LSE_08_18$natkat2[LSE_08_18$natkat==5] <- "cross-border" #Grenzgänger*innen

LSE_08_18$natkat2 <- relevel(as.factor(LSE_08_18$natkat2), ref = "CH/C-permit")
levels(LSE_08_18$natkat2)
table(LSE_08_18$natkat2)


## ---------------------------
#get ready for event study analysis
## 2016 for all districts but 2018 for Moesa and canton of ticino
#Moesa: msreg_an = 69 // TI: msreg_an = 80, 81, 82 -> c(69,80, 81, 82)
##create years_since_policy differently for regions, take end of transition period as policy time 

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    post_2013 = ifelse(erhebja >= 2013, 1, 0),
    transition_2016 = case_when(
      !(msreg_an %in% c(69, 80, 81, 82)) ~ 1,
      msreg_an %in% c(69,80, 81, 82)~ 0  ))

summary(LSE_08_18$transition_2016)

##----------------------------
LSE_08_18$gewibgrs <- as.numeric(LSE_08_18$gewibgrs)
LSE_08_18$wage_region <- as.factor(LSE_08_18$wage_region)
LSE_08_18$cba <- as.factor(LSE_08_18$cba)
levels(LSE_08_18$cba) <- c("Not covered", "CBA covered")
table(LSE_08_18$cba)

## ---------------------------
# create firm-level gap measure -> for all firms, independent of coverage
#burnr: 2008, 2010 // entid: 2012-2018

LSE_08_18 <- LSE_08_18 %>%
  group_by(erhebja, sector) %>%
  mutate(
    high_thresh_base = wtd.quantile(base_wage_gross_std, weights = gewicht, probs = c(0.995), na.rm = TRUE),
    low_thresh_base = wtd.quantile(base_wage_gross_std, weights = gewicht, probs = c(0.0025), na.rm = TRUE) )

LSE_08_18 <- LSE_08_18 %>%
  mutate(base_wage_gross_std = ifelse(base_wage_gross_std<=low_thresh_base, NA, base_wage_gross_std))
summary(LSE_08_18$base_wage_gross_std)

gap_exposure_firm <- LSE_08_18 %>%
  filter(mem==1 | watch==1) %>%
  group_by(burnr, erhebja) %>%
  #filter(!is.na(dist_to_MW) & !is.na(gewicht) & gewicht > 0) %>%
  summarise(
    #weighted sum of negative distance to min wage
    wages_below = sum(dist_to_MW_high[dist_to_MW_high <=0]*gewicht[dist_to_MW_high <=0 ]),
    wages_below_low = sum(dist_to_MW[dist_to_MW <=0]*gewicht[dist_to_MW <=0]),
    wages_below_base = sum(dist_base_to_MW[dist_base_to_MW <=0]*gewicht[dist_base_to_MW <=0]),
    cba = cba[1],
    #weighted sum of total paybill
    wage_bill = sum(gross_wage_std * gewicht),
    wage_bill_base = sum(base_wage_gross_std * gewicht),
    #divide
    gap_exposure = abs(wages_below/wage_bill),
    gap_exposure_cons = abs(wages_below_low/wage_bill),
    gap_exposure_base = abs(wages_below_base/wage_bill_base),
    .groups = "drop")

#standardize firm-level gap within each year -> not good for taking averages across years then
gap_exposure_firm <- gap_exposure_firm %>%
  group_by(erhebja) %>%
  mutate(
    gap_exposure_std = (gap_exposure - ave(gap_exposure, na.rm = TRUE)) / 
      (sd(gap_exposure, na.rm = TRUE)),
    gap_exposure_cons_std = (gap_exposure_cons - ave(gap_exposure_cons, na.rm = TRUE))/
      (sd(gap_exposure_cons, na.rm = TRUE)),
    gap_exposure_base_std = (gap_exposure_base - ave(gap_exposure_base, na.rm = TRUE))/
      (sd(gap_exposure_base, na.rm = TRUE))   )
saveRDS(gap_exposure_firm, file = "output/data/2c_gap_exposure_firm.rds")

#merge yearly firm-level gap to firm panel
LSE_08_18 <- left_join(LSE_08_18, gap_exposure_firm %>% select(c(starts_with("gap"), "burnr", "erhebja")), 
                       by = c("burnr", "erhebja"))

#time and firm FE needed for Derenoncourt (2025)
#pre 2012: burnr
#gap2008 (burnr) for pre-policy period? we need to choose an exposure measure
gap_2012 <- gap_exposure_firm %>%
  filter(erhebja==2012) %>%
  rename(gap_treat = gap_exposure_cons_std,
         gap_treat_highMW = gap_exposure_std,
         gap_treat_base = gap_exposure_base_std)
gap_2012$erhebja <- NULL
gap_2008 <- gap_exposure_firm %>%
  filter(erhebja==2008) %>%
  rename(gap_treat = gap_exposure_cons_std,
         gap_treat_highMW = gap_exposure_std,
         gap_treat_base = gap_exposure_base_std)
gap_2008$erhebja <- NULL
exposure <- rbind(gap_2012, gap_2008)

LSE_08_18 <- left_join(LSE_08_18, exposure %>% 
                         select(c(gap_treat, gap_treat_highMW, gap_treat_base, burnr)), 
                       by = c("burnr"))
summary(LSE_08_18$gap_treat[LSE_08_18$erhebja==2012])


## ---------------------------
#people might have more than one job / work in more than 1 sector / after 2012, not work in MEM at all
LSE_08_18 <- LSE_08_18 %>%
  group_by(avs_nb, erhebja) %>%
  mutate(
    income = ifelse(erhebja>=2012& avs_nb!="", sum(blimok), NA), 
    nr_jobs = ifelse(erhebja>=2012& avs_nb!="", n(), NA)) %>%
  group_by(avs_nb, erhebja, sector) %>% mutate(
    perc_sector = ifelse(erhebja>=2012 & avs_nb!="", sum(ibgr), NA))

summary(LSE_08_18$perc_sector[LSE_08_18$sector == "MEM"])
summary(LSE_08_18$perc_sector[LSE_08_18$sector == "Watch"])
summary(LSE_08_18$income)
summary(LSE_08_18$nr_jobs)

LSE_08_18 <- LSE_08_18 %>%
  mutate(
    main_sector = case_when(
      ibgr >=0.8 & sector == "MEM" ~ "MEM",
      ibgr >=0.8 & sector == "Watch" ~ "Watch",
      TRUE ~ NA) ) %>%
  group_by(avs_nb, erhebja) %>% mutate(
    main_sector = case_when(
      erhebja>=2012& avs_nb!="" & sum(blimok[sector=="MEM"]) > income/nr_jobs ~ "MEM", 
      erhebja>=2012& avs_nb!="" & sum(blimok[sector == "Watch"]) > income/nr_jobs ~ "Watch",
      TRUE ~ main_sector) )
table(LSE_08_18$main_sector)
table(LSE_08_18$sector)

##----------------------------
##noga number (sub-sector) of each firm, in a year
#1 firm has in one year more than one noga code: 
#burnr = +rsO9TAfMgkiXiCG17dgegxh/uoVu6XrAypsRuVK/zA= & erhebja == 2014
firm_noga <- LSE_08_18 %>%
  group_by(burnr, erhebja) %>%
  summarise(
    n_unique_noga = n_distinct(noga),
    noga_codes = list(unique(noga)),
    .groups = "drop"  ) %>%
  filter(n_unique_noga > 1)

list <- LSE_08_18 %>%
  select(c(erhebja, burnr, noga, gewicht))%>%
  filter(burnr %in% firm_noga$burnr) %>%
  group_by(burnr, erhebja) %>% 
  mutate(n_unique_noga = n_distinct(noga)) %>%
  filter(n_unique_noga>1) %>%
  group_by(noga, burnr, erhebja) %>%
  summarise(people = sum(gewicht, na.rm = TRUE),
            n_distinct = n_unique_noga[1],
            .groups = "drop") %>% 
    ungroup() %>% group_by(burnr, erhebja) %>% 
  arrange(desc(people)) %>% slice(1) %>% 
  rename(noga_unique = noga)

LSE_08_18 <- left_join(LSE_08_18, list %>% select(erhebja, burnr, noga_unique),
                       by = c("burnr", "erhebja")) %>%
  mutate(noga_raw = noga,
         noga = ifelse(is.na(noga_unique), noga, noga_unique),
         noga_unique = NULL)

##----------------------------
#save working file merged
saveRDS(LSE_08_18, file = "output/data/2c_variables.rds")

##----------- notes ----------

#wtd.var(LSE_08_18$blimok, weights)            # Weighted variance  
#wtd.cor(LSE_08_18$blimok, y, weights)         # Weighted correlation
#wtd.table(LSE_08_18$blimok, weights)          # Weighted frequency table

##weighting
# gewibgrs: "What's the wage distribution in full-time equivalent terms?"
# gewicht: "What's the wage distribution experienced by actual workers?"

#A part-time worker earning 4000 CHF (50% employment) has blimok = 8000 CHF (full-time equivalent)
# gewicht would give this person full weight in determining the median full-time wage
# gewibgrs gives them 0.5 weight

'''
Gewichtungsfaktor ohne individuellen Beschäftigungsgrad („gewicht“) -> Die 
daraus resultierenden Aussagen sind personenbezogen.

Gewichtungsfaktor mit individuellem Beschäftigungsgrad („gewibgrs“). Eine Arbeitnehmerin 
mit einer 50-Prozent-Stelle erhält nur halb so viel Gewicht wie ihre Vollzeit 
erwerbstätige Kollegin -> Die daraus resultierenden Aussagen sind stellenbezogen.

Die Schichtung auf Unternehmensebene erfolgte seit dem Jahr 2002 nach 3 Kriterien: 
Unternehmensgrösse (#Beschäftigte gemäss BUR), Branchenzugehörigkeit (NOGA-Abteilung), 
Regionszugehörigkeit (7 Grossregionen + 6 Kantone).
'''
