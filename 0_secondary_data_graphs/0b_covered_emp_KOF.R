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
getwd()

library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(tibbletime)
theme_set(theme_minimal())
## -------------------------------
#Anzahl unterstellte MA gem. KOF
covered_emp <- read_excel("raw_data/key_0047.xlsx")

#Total Mitarbeitende gem. Erwerbstätigenstatistik (ETS)
#Als Grundlage nur "Verarbeitendes Gewerbe/Herstellung von Waren"
#MEM zwar drin, aber auch Herstellung von Nahrungs- und Futtermitteln, Getränkeherstellung, Tabakverarbeitung, etc.
#-> wir unterschätzen coverage massiv
#https://dievolkswirtschaft.ch/de/2021/07/gesamtarbeitsvertraege-trotzen-dem-strukturwandel/

total_emp_ETS <- read_excel("raw_data/je-d-03.02.01.08.xlsx", sheet = "Jahreswerte", skip = 2)
total_emp_ETS <- total_emp_ETS %>%
  rename("Wirtschaftssektor" = ...1) %>%
  rename("Wirtschaftsabschnitt" = ...2) %>%
  filter(!is.na(Wirtschaftsabschnitt)) %>%
  filter(Wirtschaftsabschnitt == "Verarbeitendes Gewerbe/Herstellung von Waren") %>%
  slice(1) %>%
  select(-c(Wirtschaftssektor, Wirtschaftsabschnitt)) %>%
  mutate(across(starts_with("19") | starts_with("20"),
                ~ round(as.numeric(.) * 1000, 2))) 
total_emp_ETS_long <- pivot_longer(total_emp_ETS, cols = everything(), 
                                   names_to = "year", values_to = "nr_employees") %>%
  mutate(year = as.numeric(year))

## coverage ratio
CBA_coverage_ETS <- left_join(total_emp_ETS_long, covered_emp, by = "year") %>%
  select(c("year", "nr_employees", "covered_employees")) %>% mutate(
    covered_employees = as.numeric(covered_employees)) %>%
  mutate(coverage = ifelse(!is.na(covered_employees), covered_employees / nr_employees, NA))

#time plot
ggplot(data = CBA_coverage_ETS[CBA_coverage_ETS$year>=2000, ], aes(x=year, y = coverage)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "covered workers", colour = NULL)+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = 2018), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/CBA_coverage_KOF_ETS.pdf", width = 20, height = 4, unit = "cm")

#zoomed 2008-2018
ggplot(data = CBA_coverage_ETS[CBA_coverage_ETS$year %in% c(2008, 2010, 2012, 2014, 2016, 2018), ], 
       aes(x=year, y = coverage)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "% covered workers", colour = NULL)+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/CBA_coverage_KOF_ETS_zoomed.pdf", width = 20, height = 4, unit = "cm")

## ---

#Total Mitarbeitende MEM gem. BESTA (ungenau) -> wir unterschätzen coverage, Trend sollte ok (?)
total_emp <- read_excel("raw_data/px-x-0602000000_101_20251001-160534.xlsx")
total_emp <- total_emp %>%
  filter(!is.na(...5)) %>% #number of workers, not FTE
  select(-c(`Beschäftigte nach Wirtschaftsabteilung, Beschäftigungsgrad und Geschlecht`, ...3)) 
total_emp$...2[1] <- "sector"
total_emp$...4[1] <- "unit"
colnames(total_emp) <- as.character(total_emp[1, ])
total_emp <- total_emp[-1, ] %>% filter(unit == "Total,saisonbereinigt") %>% select(-unit)

total_emp_long <- pivot_longer(total_emp, cols = c(starts_with("19"), starts_with("20")), 
                               names_to = "year", values_to = "nr_employees")
total_emp_wide <- pivot_wider(total_emp_long, names_from = sector, values_from = nr_employees)

total_emp_wide <- total_emp_wide %>%
  mutate(
    `24-25 Herstellung von Metallerzeugnissen` = as.numeric(`24-25 Herstellung von Metallerzeugnissen`),
    `27 Herstellung von elektrischen Ausrüstungen`=as.numeric(`27 Herstellung von elektrischen Ausrüstungen`),
    `28 Maschinenbau`= as.numeric(`28 Maschinenbau`),
    `29-30 Fahrzeugbau`= as.numeric(`29-30 Fahrzeugbau`),
    `31-33 Sonstige Herstellung von Waren, Rep. und Inst.`= as.numeric(`31-33 Sonstige Herstellung von Waren, Rep. und Inst.`))

total_emp_wide$MEM <- rowSums(total_emp_wide[sapply(total_emp_wide, is.numeric)], na.rm = TRUE)

total_emp_wide <- total_emp_wide %>%
  mutate(year = as.character(year)) %>% 
  separate(year, into = c("year", "quarter"), sep = "Q", fill = "right", remove = FALSE) 

total_emp_wide <- total_emp_wide %>%
  group_by(year) %>%arrange(quarter) %>%
  slice(1) %>% mutate(year = as.numeric(year))

## coverage ratio
CBA_coverage <- left_join(total_emp_wide, covered_emp, by = "year") %>%
  select(c("year", "covered_employees", "MEM")) %>% mutate(
    covered_employees = as.numeric(covered_employees)
  ) %>%
  mutate(coverage = ifelse(!is.na(covered_employees), covered_employees / MEM, NA))

#time plot
ggplot(data = CBA_coverage[CBA_coverage$year>=2000 & CBA_coverage$year<=2023, ], 
       aes(x=year, y = coverage)) +
  geom_line() +
  geom_point(size=1) +
  labs(x = NULL, y = "% covered workers", colour = NULL)+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = 2018), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/CBA_coverage_KOF_BESTA.pdf", width = 20, height = 4, unit = "cm")

#zoomed: 2008-2018
ggplot(data = CBA_coverage[CBA_coverage$year %in% c(2008, 2010, 2012, 2014, 2016, 2018), ], 
       aes(x=year, y = coverage)) +
  geom_line() +
  geom_point(size = 1) +
  labs(x = NULL, y = "coverage", colour = NULL)+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed")
ggsave("output/graph/CBA_coverage_KOF_BESTA_zoomed.pdf", width = 16, height = 5, unit = "cm")
