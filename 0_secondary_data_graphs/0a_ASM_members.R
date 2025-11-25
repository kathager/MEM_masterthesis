## ---------------------------
## Master thesis
## Author: Katja Hager
## Date Created: August 2025
#
#Swissmem and ASM members, based on Unia data
#goal: Firma, Ort, Kanton, Swissmem, ASM
#2006-2019
#
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation
library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(tibbletime)
theme_set(theme_minimal())
## -------------------------------
#load files

X2006_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2006 _asm-mitgliederverzeichnis (stand 8. mai 2006).xls")
X2007_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2007_Swissmem und ASM Mitglieder.xlsx")
X2008_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2008_Swissmem Mitglieder.xlsx", 
                                 col_types = c("text", "text", "text"))
X2009_asm_swissmem  <- read_excel("raw_data/ASM_Mitglieder/2009_Swissmem_ASM.xlsx")
X2010_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2010_ASM Mitglieder.xls", 
                                 col_types = c("text", "skip", "text", "text"))
X2011_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2011_Swissmem und ASM Mitglieder.xls")
X2012_asm_swissmem<- read_excel("raw_data/ASM_Mitglieder/2012_Swissmem und ASM Mitglieder.xlsx")
X2013_asm_swissmem  <- read_excel("raw_data/ASM_Mitglieder/2013_Swissmem und ASM Mitglieder.xlsx")
X2014_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2014_Swissmem und ASM Mitglieder.xlsx")
X2015_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2015_Swissmem und ASM Mitglieder.xls")
X2016_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2016_Swissmem und ASM Mitgliederverzeichnis.xlsx")
X2017_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2017_Swissmem und ASM Mitglieder.xlsx")
X2018_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2018_asm-mitgliederliste per 1. januar 2018.xlsx")

X2019_asm <- read_excel("raw_data/ASM_Mitglieder/2019_asm-mitglieder per 1.1.2019.xlsx", 
                                    skip = 1)
X2019_asm$ASM <- "x"
X2019_swissmem <- read_excel("raw_data/ASM_Mitglieder/2019_swissmem-mitglieder per 1.1.2019.xlsx", 
                                skip = 1)
X2019_swissmem$Swissmem <- "x"
X2019_asm_swissmem <- full_join(X2019_swissmem, X2019_asm, by = c("Firma", "Kanton", "Ort"))
X2020_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2020 ASM Mitgliederverzeichnis-liste des membres.xlsx", 
                                 col_types = c("text", "text", "numeric", "text", "text", "skip"),
                                 col_names = c("Firma", "Strasse", "PLZ", "Ort", 
                                              "Kanton"), skip = 7)

X2021_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2021 ASM Mitgliederverzeichnis-liste des membres.xlsx", 
                                 col_types = c("text", "text", "numeric", "text", "text", "skip"),
                                 col_names = c("Firma", "Strasse", "PLZ", "Ort", 
                                               "Kanton"), skip = 7)

X2022_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2022 ASM Mitgliederverzeichnis-liste des membres.xlsx", 
                                 col_names = c("Firma", "Strasse", "PLZ", "Ort", 
                                               "Kanton", "Mail"), skip = 7)
X2023_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2023_Mitgliederliste ASM.xlsx", 
                                    col_types = c("skip", "skip", "skip", 
                                                  "text", "skip", "text", "numeric", 
                                                  "text"), skip = 2,
                                    col_names = c("Firma", "Strasse", "PLZ", "Ort"))

X2024_asm_swissmem <- read_excel("raw_data/ASM_Mitglieder/2024_ASM Mitgliederverzeichnis_liste des membres ASM.xlsx", 
                                 col_types = c("skip", "skip", "skip", 
                                               "text", "skip", "text", "numeric", 
                                               "text"), skip = 2,
                                 col_names = c("Firma", "Strasse", "PLZ", "Ort"))


#get columns into form: Firma, Ort, Kanton, Swissmem, ASM, year
X2010_asm_swissmem$ASM <- "x"
X2020_asm_swissmem$ASM <- "x"
X2021_asm_swissmem$ASM <- "x"
X2022_asm_swissmem$ASM <- "x"
X2023_asm_swissmem$ASM <- "x"
X2024_asm_swissmem$ASM <- "x"

#create column "year"
for (years in c(2006, 2007, 2010, 2018, 2019, 2020, 2021, 2022, 2023, 2024)) {
  year_str <- as.character(years)
  filename <- paste("X", year_str, "_asm_swissmem", sep = "")
  current_dataset <- get(filename)
  
  current_dataset$year <- years
  
  assign(filename, current_dataset)
}

#split columns to extract info, add column "year"
for (years in c(2008, 2009, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) {
  #split Firma by , to generate column "location"
  year_str <- as.character(years)
  filename <- paste("X", year_str, "_asm_swissmem", sep = "")
  current_dataset <- get(filename)
  
  location_firm_list <- strsplit(current_dataset$Firma, split = ", ")
  
  print(location_firm_list[[1]][2])
  
  Firma <- list()
  location <- list()
  for (i in 1:length(location_firm_list)) {
    Firma[i] <- location_firm_list[[i]][1]
    location[i] <- location_firm_list[[i]][2]
  }
  #split location by / to generate Ort and Kanton
  location_list <- strsplit(unlist(location), split = " / ")
  
  Ort <- list()
  Kanton <- list()
  for (i in 1:length(location_list)) {
    Ort[i] <- location_list[[i]][1]
    Kanton[i] <- location_list[[i]][2]
  }
  
  #merge to dataset
  current_dataset$Firma <- unlist(Firma)
  current_dataset$Ort <- unlist(Ort)
  current_dataset$Kanton <- unlist(Kanton)
  
  current_dataset$year <- years
  
  assign(filename, current_dataset)
}

#append datasets
all_data <- data.frame('Firma'=character(), 'Kanton'=character(), 'Ort'=character(), 
                       'year'=double(), 'ASM'=character(), 'Swissmem'=character())

for (i in seq(2006, 2024, by = 1)) {
    year_str <- as.character(i)
    filename <- paste("X", year_str, "_asm_swissmem", sep = "")
    current_dataset <- get(filename)
    
    all_data <- bind_rows(all_data, current_dataset)
}

all_data$ASM[!is.na(all_data$ASM)] <- "x"
all_data$Swissmem[!is.na(all_data$Swissmem)] <- "x"

all_data <- all_data %>%
  filter(!is.na(Ort))

all_data <- all_data %>%
  mutate(
    across(c(Ort, Kanton), trimws),
    Ort = case_when(
      Ort == "Biel" | Ort == "Bienne"~ "Biel/Bienne",
      Ort == "Küssnacht"~"Küssnacht (SZ)",
      Ort == "Küsnacht"~"Küsnacht (ZH)",
      Ort == "Wetzikon"~"Wetzikon (ZH)",
      Ort == "Wohlen"~"Wohlen (AG)",
      Ort == "8640 Rapperswil"~"Rapperswil",
      Ort == "8404 Winterthur"~"Winterthur",
      Ort == "6072 Sachseln/ OW"~"Sachseln",
      Ort == "St-Imier"~"Saint-Imier",
      Ort == "Bürglen UR"~"Bürglen (UR)",
      Ort == "Näfels" | Ort == "Oberurnen" ~"Glarus Nord",
      Ort == "Ennenda" | Ort == "Netstal" ~"Glarus",
      Ort == "Effretikon"|Ort == "Kyburg"~"Illnau-Effretikon",  
      Ort == "Samstagern"~"Richterswil",
      .default = Ort #TRUE ~ Ort
    ))

saveRDS(all_data, file = "output/data/ASM_members.Rds")
## ------------------
#plots
all_data <- readRDS(file = "output/data/ASM_members.Rds")

#plot data
plot_data <- all_data %>%
  group_by(year) %>%
  summarise(count_ASM = sum(!is.na(ASM)),
            count_Swissmem = sum(!is.na(Swissmem))) %>%
  mutate(count_Swissmem = ifelse(year %in% c(2006, 2010, 2018, 2020, 2021, 2022, 2023, 2024), 
                                 NA, count_Swissmem))

#data from a scanned PDF, Excel not possible
#cannot differentiate between ASM/Swissmem members, just total count
year_col <- c(2000, 2001, 2002, 2003, 2004, 2005)
ASM_col <- c(607, 595, 598, 610, 621, 604)
pdf_data <- bind_cols(year = year_col, count_ASM = ASM_col, count_Swissmem = NA)

plot_data <- bind_rows(plot_data, pdf_data)

ggplot(data = plot_data, aes(x=year)) +
  geom_line(aes(y = count_Swissmem, color = "Swissmem")) +
  geom_point(aes(y = count_Swissmem, color = "Swissmem"), size = 1) +
  geom_line(aes(y = count_ASM, color = "ASM")) +
  geom_point(aes(y = count_ASM, color = "ASM"), size = 1) +
  labs(x = NULL, y = "# firms", colour = NULL)+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = 2018), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/ASM_Swismem.pdf", width = 20, height = 5, units = "cm")

#differentiate between new firms who join and firms who leave
firm_status_data <- all_data %>% 
  filter(!is.na(ASM) & !is.na(Firma)) %>%
  mutate(
    company = str_to_lower(Firma),
    company = str_replace_all(company, "[[:punct:]]", ""),  # remove punctuation
    company = str_replace_all(company, "\\s+", "") )  %>%        # remove all whitespace )
  group_by(company) %>%
  arrange(year) %>%
  summarise(first_joined = ifelse(year[1] == 2006, NA, year[1]),
         last_year = year[n()],  #get last year for each firm
         left_per = ifelse(last_year == 2024, NA, last_year+1),
         Firma = Firma[1]) %>%
  select(c(Firma, first_joined, last_year, left_per, company))

#firms leaving
leave_firms <- firm_status_data %>%
  group_by(left_per) %>%
  summarise(firms_left_n = sum(!is.na(Firma))) %>%
  rename(year = left_per) %>%
  filter(!is.na(year))

#firms joining
join_firms <- firm_status_data %>%
  group_by(first_joined) %>%
  summarise(first_joined_n = sum(!is.na(Firma))) %>%
  rename(year = first_joined) %>%
  filter(!is.na(year))

firms_come_and_go <- full_join(leave_firms, join_firms, by = "year")

ggplot(data = firms_come_and_go, aes(x=year)) +
  geom_line(aes(y = first_joined_n, color = "join")) +
  geom_point(aes(y = first_joined_n, color = "join")) +
  geom_line(aes(y = firms_left_n, color = "leave")) +
  geom_point(aes(y = firms_left_n, color = "leave")) +
  labs(x = NULL, y = "# ASM mutations", colour = NULL)+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2008, 2013, 2018, 2023)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.5, 1),
        legend.justification.inside = c(0.5, 1),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.size =  unit(2, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.text=element_text(size=12)) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = 2018), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/ASM_Swismem_mutations.pdf", width = 20, height = 4, units = "cm")

#opting out: ratio of firms last seen in 2012, divided by total ASM firms in 2012
leave_firms$year <- leave_firms$year-1
opt_out_usage <- full_join(leave_firms, plot_data, by = "year")

opt_out_usage <- opt_out_usage %>%
  filter(!is.na(firms_left_n)) %>%
  mutate(ratio = firms_left_n/count_ASM)

ggplot(data = opt_out_usage[opt_out_usage$year %in% 2011:2019, ], aes(x=year, y = ratio)) +
  geom_line() +
  labs(x = NULL, y = "Opting out")+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(2008, 2013, 2018, 2023)) +
  geom_vline(aes(xintercept = 2012), color = "grey", linewidth = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = 2017), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/ASM_opting_out.pdf", width = 18, height = 6, units = "cm")

###############################################
#do firms really opt out or do they change their name?
library(writexl)
check_opting_out <- all_data %>% 
  filter(!is.na(ASM) & !is.na(Firma)) %>%
  mutate(
    company = str_to_lower(Firma),
    company = str_replace_all(company, "[[:punct:]]", ""),  # remove punctuation
    company = str_replace_all(company, "\\s+", "") )  %>%        # remove all whitespace )
  group_by(company) %>%
  arrange(year) %>%
  mutate(first_joined = ifelse(year[1] == 2006, NA, year[1]),
            last_year = year[n()],  #get last year for each firm
            left_per = ifelse(last_year == 2024, NA, last_year+1)) %>%
  select(c(Firma, year, first_joined, last_year, company))
write_xlsx(check_opting_out, "output/data/check_opting_out.xlsx")

#manually added to Excel
opting_out_zefix <- read_excel("output/data/check_opting_out_added.xlsx") %>%
  mutate(company = str_to_lower(Firma),
         company = str_replace_all(company, "[[:punct:]]", ""),  # remove punctuation
         company = str_replace_all(company, "\\s+", ""),
         succ_company = str_to_lower(Successor_firm),
         succ_company = str_replace_all(succ_company, "[[:punct:]]", ""), 
         succ_company = str_replace_all(succ_company, "\\s+", ""))  %>% 
  group_by(company) %>%
  arrange(year) %>%
  summarise(first_joined = ifelse(year[1] == 2006, NA, year[1]),
         last_year = year[n()],
         opted_out = opted_out[1]) %>%
  group_by(last_year) %>%
  summarise(firms_left_refined = sum(opted_out=="yes", na.rm = TRUE) ) %>%
  rename( year=last_year)
      
#opting out: ratio of firms last seen in 2012, divided by total ASM firms in 2012
#last seen in 2012, so in 2013 NOT CBA covered anymore (left per 2013)
#check: in 2012 and 2017 many opt-outs?
opt_out_refined <- left_join(opting_out_zefix, opt_out_usage, by = "year") %>%
  filter(!is.na(firms_left_refined)) %>%
  mutate(ratio = firms_left_refined/count_ASM) %>%
  filter(year %in% 2011:2019)

ggplot(data = opt_out_refined, aes(x=year, y = ratio)) +
  geom_line() + geom_point() +
  labs(x = NULL, y = "firms opting out")+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  geom_vline(aes(xintercept = 2012), color = "grey", linewidth = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = 2017), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/ASM_opting_out_refined.pdf", width = 20, height = 4, units = "cm")

###############################################
#find municipality number in BFS Raumgliederung
geographical_levels <- read_excel("raw_data//Raumgliederungen.xlsx", 
                                  col_names = c("municipality_nr", "Ort", "cantonal_nr", "Kanton", 
                                                "district_nr", "district", "msreg_an"), 
                                  col_types = c("numeric", "text", "text", "text", "numeric", 
                                                "text", "numeric"))


all_data <- right_join(geographical_levels, all_data, by = c("Ort")) %>%
  group_by(Firma) %>%
  arrange(Firma, desc(year))%>%
  fill(municipality_nr, .direction = "down")

sum(all_data$year == 2013 & is.na(all_data$municipality_nr))  

ASM_2013 <- all_data %>%
  filter(year==2012) %>%
  group_by(msreg_an) %>%
  summarise(firms = sum(!is.na(Firma) & !is.na(ASM))) %>%
  slice(1:20)

overlap_cov <- intersect(treated_MS_cov, ASM_2013$msreg_an)
overlap_gap <- intersect(treated_MS, ASM_2013$msreg_an)
