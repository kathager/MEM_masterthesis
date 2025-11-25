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

setwd("C:/Users/khage/Desktop/Uni/01_Masterarbeit/LSE/")
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

getwd()

## ---------------------------
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")

med_wage_2012 <- wtd.quantile(LSE_08_18$real_wage[LSE_08_18$erhebja == 2012], 
                              weights = LSE_08_18$gewicht[LSE_08_18$erhebja == 2012], 
                              probs=0.5)
table(LSE_08_18$min_wage_worker)
##-------------------------------
#separated by CBA coverage

#age
ggplot(LSE_08_18[LSE_08_18$erhebja==2012 & LSE_08_18$real_wage <= 16000 & !is.na(LSE_08_18$cba) , ], 
       aes(x = alter, weight = gewicht, colour = cba, fill = cba)) +
  geom_density(linewidth = .8, adjust = 1.1, alpha = 0.5) + 
  labs(x = "Age",y = "Density",
       color = NULL,fill = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(18, 35, 50, 65), labels = scales::comma) +
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/age_distribution_cba_2012.pdf")

ggplot(LSE_08_18[LSE_08_18$erhebja==2012 & LSE_08_18$real_wage <= 18000 
                 & LSE_08_18$dienstja<45 & !is.na(LSE_08_18$dienstja) & !is.na(LSE_08_18$cba), ], 
       aes(x = dienstja, weight = gewicht, colour = cba, fill = cba)) +
  geom_density(linewidth = .8, adjust = 1.1, alpha = 0.5) + 
  labs(x = "Tenure",
       y = "Density",
       color = NULL,
       fill = NULL) +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_continuous(breaks = c(1, 5, 10, 20), labels = scales::comma) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  theme(legend.position = "none") +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/tenure_distribution_CBA_2012.pdf")

#nationality: weighted counts (2012 and 2018)
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012 & 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$natkat2)& 
                          !is.na(LSE_08_18$cba), ], 
       aes(x = cba , weight = gewicht, fill =natkat2)) +
  geom_bar(position = "dodge", width = 0.5) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(4))
ggsave("output/graph/nationality_counts_cba_12.pdf")

#job position
LSE_08_18$job_position <- relevel(LSE_08_18$job_position, ref = "Lower Management")
LSE_08_18$job_position <- relevel(LSE_08_18$job_position, ref = "No Management")
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012& 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$job_position)& 
                          !is.na(LSE_08_18$cba), ], 
       aes(x = cba , weight = gewicht, fill = job_position)) +
  geom_bar(position = "dodge", width = 0.5) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/job_position_counts_cba_12.pdf")

#qualification (=education)
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012& 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$qualification)& 
                          !is.na(LSE_08_18$cba), ], 
       aes(x = cba, weight = gewicht, fill =qualification)) +
  geom_bar(position = "dodge", width = 0.5) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/qualification_counts_cba_2012.pdf")

#gender
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012& 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$geschle)& 
                          !is.na(LSE_08_18$cba), ], 
       aes(x = cba , weight = gewicht, fill = geschle)) +
  geom_bar(position = "dodge", width = 0.5) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/gender_counts_cba_12.pdf")

## ------------------------------------
firm_data <- LSE_08_18 %>%
  group_by(erhebja, burnr) %>%
  summarise(firmsize = sum(gewicht, na.rm = TRUE) )

firm_data_cba <- LSE_08_18 %>%
  filter(!is.na(cba)) %>%
  group_by(erhebja, burnr, cba) %>%
  summarise(firmsize = sum(gewicht, na.rm = TRUE)  )

#firm size
ggplot(data = firm_data[firm_data$erhebja==2012, ], 
       aes(x = firmsize)) +
  geom_histogram(bins = 80) + 
  labs(x = "#workers in a firm", y = "# of firms", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.8, 0.8),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/firm_size_counts_firmlevel_MEM_12.pdf")

ggplot(data = firm_data_cba[firm_data_cba$erhebja==2012, ], 
       aes(x = firmsize, fill = cba )) +
  geom_histogram(bins = 80) + 
  labs(x = NULL, y = "#workers in a firm", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.8, 0.8),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/firm_size_counts_firmlevel_cba_12.pdf")

#firm size: where are people employed?
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012& 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$firm_size), ], 
       aes(x = firm_size , weight = gewicht)) +
  geom_bar(position = "dodge", width = 0.5) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.8, 0.8),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(1))
ggsave("output/graph/firm_size_counts_MEM_12.pdf")

ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012& 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$firm_size)&
                          !is.na(LSE_08_18$cba), ], 
       aes(x = cba , weight = gewicht, fill = firm_size)) +
  geom_bar(position = "dodge", width = 0.5) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.8, 0.8),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/firm_size_counts_cba_12.pdf")

## -------------------
##separated by min wage worker

ggplot(LSE_08_18[LSE_08_18$erhebja==2012 & LSE_08_18$real_wage <= 18000 , ], 
       aes(x = alter, weight = gewicht, colour = min_wage_worker, fill = min_wage_worker)) +
  geom_density(linewidth = .8, adjust = 1.1, alpha = 0.7) + 
  labs(x = "Age",y = "Density",
       color = NULL,fill = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(18, 35, 50, 65), labels = scales::comma) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.38, 1),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"))+
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2))+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2))
#geom_vline(aes(xintercept = 0), color = "darkgrey", linewidth = 0.7, linetype = "dashed") 
ggsave("output/graph/age_distribution_all_2012_2018.pdf")

ggplot(LSE_08_18[LSE_08_18$erhebja==2012 & LSE_08_18$real_wage <= 18000 , ], 
       aes(x = dienstja, weight = gewicht, colour = min_wage_worker, fill = min_wage_worker)) +
  geom_density(linewidth = .8, adjust = 1.1, alpha = 0.6) + 
  labs(x = "Tenure",
       y = "Density",
       color = NULL,
       fill = NULL) +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_continuous(breaks = c(1, 5, 10, 20), labels = scales::comma) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"))
#geom_vline(aes(xintercept = 0), color = "darkgrey", linewidth = 0.7, linetype = "dashed") 
ggsave("output/graph/tenure_distribution_all_2012_2018.pdf")

#nationality: weighted counts (2012 and 2018)
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012 & 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$natkat2), ], 
       aes(x = min_wage_worker , weight = gewicht, fill =natkat2)) +
  geom_bar(position = "dodge") + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.9),
        legend.justification.inside = c(0.3, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(0.7, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(4))
ggsave("output/graph/nationality_counts_MW_12.pdf")

#job position
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012& 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$job_position), ], 
       aes(x = min_wage_worker , weight = gewicht, fill = job_position)) +
  geom_bar(position = "dodge") + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.9),
        legend.justification.inside = c(0.3, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(0.7, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/job_position_counts_MW_12.pdf")

#qualification (=education)
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012& 
                          LSE_08_18$real_wage <= 18000 & !is.na(LSE_08_18$qualification), ], 
       aes(x = min_wage_worker, weight = gewicht, fill =qualification)) +
  geom_bar(position = "dodge") + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.15, 0.9),
        legend.justification.inside = c(0.3, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(0.7, "cm")) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/qualification_counts_12_18.pdf")

##-------------------------------
#within MW workers, differentiate by CBA coverage
levels(LSE_08_18$min_wage_worker)

data_MW <- LSE_08_18 %>%
  filter(min_wage_worker=="Minimum wage worker")

ggplot(data_MW[data_MW$erhebja==2012 & data_MW$real_wage <= 18000 , ], 
       aes(x = alter, weight = gewicht, colour = cba, fill = cba)) +
  geom_density(linewidth = .8, adjust = 1.1, alpha = 0.5) + 
  labs(x = "Age",y = "Density",
       color = NULL,fill = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(18, 35, 50, 65), labels = scales::comma) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.9),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  theme(legend.position = "none") +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/age_distribution_cba_MW_2012.pdf")

ggplot(data_MW[data_MW$erhebja==2012 & data_MW$real_wage <= 18000 & data_MW$dienstja<45 , ], 
       aes(x = dienstja, weight = gewicht, colour = cba, fill = cba)) +
  geom_density(linewidth = .8, adjust = 1.1, alpha = 0.5) + 
  labs(x = "Tenure",y = "Density",
       color = NULL,fill = NULL) +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_continuous(breaks = c(1, 5, 10, 20), labels = scales::comma) +
  theme_minimal() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.97, 1),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.text=element_text(size=22),
        legend.key.height = unit(1.5, "cm"),
        legend.key.width = unit(1.5, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/tenure_distribution_CBA_MW_2012.pdf")

#nationality: weighted counts (2012 and 2018)
ggplot(data = data_MW[data_MW$erhebja==2012 & 
                        data_MW$real_wage <= 18000 & !is.na(data_MW$natkat2), ], 
       aes(x = cba , weight = gewicht, fill =natkat2)) +
  geom_bar(position = "fill", width = 0.4) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(4))
ggsave("output/graph/nationality_counts_cba_MW_12.pdf")

#job position
data_MW$job_position <- relevel(data_MW$job_position, ref = "Lower Management")
data_MW$job_position <- relevel(data_MW$job_position, ref = "No Management")
ggplot(data = data_MW[data_MW$erhebja==2012& 
                        data_MW$real_wage <= 18000 & !is.na(data_MW$job_position), ], 
       aes(x = cba , weight = gewicht, fill = job_position)) +
  geom_bar(position = "fill", width = 0.4) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.8, 0.8),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/job_position_counts_cba_MW_12.pdf")

#qualification (=education)
ggplot(data = data_MW[data_MW$erhebja==2012& 
                        data_MW$real_wage <= 18000 & !is.na(data_MW$qualification), ], 
       aes(x = cba, weight = gewicht, fill =qualification)) +
  geom_bar(position = "fill", width = 0.4) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  theme(legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +  
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/qualification_counts_cba_MW_2012.pdf")

#gender
ggplot(data = data_MW[data_MW$erhebja==2012& 
                        data_MW$real_wage <= 18000 & !is.na(data_MW$geschle), ], 
       aes(x = cba , weight = gewicht, fill = geschle)) +
  geom_bar(position = "fill", width = 0.5) + 
  labs(x = NULL, y = "# of workers", fill = NULL) +
  #scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(.9, .9),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm")) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/gender_counts_cba_MW_12.pdf")

share_MW <- LSE_08_18 %>%
  group_by(erhebja) %>%
  summarise(
    share_MW_covered = sum(gewicht[min_wage_worker=="Minimum wage worker" & 
                                     cba == "CBA covered"], na.rm = TRUE)/
      sum(gewicht[min_wage_worker=="Minimum wage worker"], na.rm = TRUE), 
    share_MW = sum(gewicht[min_wage_worker=="Minimum wage worker"], na.rm = TRUE)/
      sum(gewicht, na.rm = TRUE)
  )

ggplot(data = share_MW, 
       aes(x=erhebja, y = share_MW_covered)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "% CBA-covered", colour = NULL) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2008, 2012, 2016)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.5, 0.5),
        legend.justification.inside = c(0.5, 0.5),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"))
ggsave("output/graph/share_MW_cba_covered.pdf")

## -------------------------------
#maps: share of MW workers, and absolute number of MW workers

library(bfsMaps)
# set the path to the map root directory
options(bfsMaps.base="raw_data/BFS_maps_2022")

LSE_08_18 <- readRDS(file = "output/data/2d_variables.rds")
MW_regions <- LSE_08_18 %>%
  group_by(erhebja, msreg_an) %>%
  mutate(total_weight = sum(gewicht, na.rm = TRUE)) %>%
  filter(min_wage_worker == "Minimum wage worker") %>%
  group_by(erhebja, msreg_an) %>%
  summarise(MW = sum(gewicht, na.rm = TRUE),
            total = total_weight[1],
            share = 100*(MW/total))

# 1) start with a cantons map
#graphics.off()
pdf("MW_workers_share_2012.pdf")
kant.map <- GetMap("kant.map")
par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=TRUE)
b <- PlotKant(1:26, col= "white", 
              border="grey40")

#2) Add MS regions 
share_vec <- MW_regions$share[MW_regions$erhebja==2012&!is.na(MW_regions$msreg_an)]
share_id <- MW_regions$msreg_an[MW_regions$erhebja==2012&!is.na(MW_regions$msreg_an)]

# Create 5 colors
cols <- colorRampPalette(colors = c("white", "#44015490"))(100)
PlotMSRe(share_id,  
         col = FindColor(share_vec, 
                         cols = cols, 
                         min.x = min(share_vec, na.rm = TRUE), 
                         max.x = max(share_vec, na.rm = TRUE)), 
         border = "grey60",
         main = "Median wage",
         add=TRUE)

# reoutline the cantons
PlotKant(1:26, add=TRUE, border="grey30", lwd=1)
# add the waters
AddLakes(1:2, col=rgb(235, 247, 253, max=255), border=rgb(0,166,235, max=255))
# Add legend
min_share <- min(share_vec, na.rm = TRUE)
max_share <- max(share_vec, na.rm = TRUE)
mid_share <- (min_share + max_share)/2

ColorLegend(x="left", width=10000, labels=round(c(min_share, mid_share, max_share), 0), 
            cols=cols, cex=0.6, adj=c(0.9,0.5), frame="grey", inset=c(-0.09, 0), title = "% MW workers")

dev.off()

#absolute number of MW workers
# 1) start with a cantons map
#graphics.off()
pdf("MW_workers_absolute_2012.pdf")
kant.map <- GetMap("kant.map")
par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=TRUE)
b <- PlotKant(1:26, col= "white", 
              border="grey40")

#2) Add MS regions 
share_vec <- MW_regions$MW[MW_regions$erhebja==2012&!is.na(MW_regions$msreg_an)]
share_id <- MW_regions$msreg_an[MW_regions$erhebja==2012&!is.na(MW_regions$msreg_an)]

# Create 5 colors
cols <- colorRampPalette(colors = c("white", "#44015490"))(10)
PlotMSRe(share_id,  
         col = FindColor(share_vec, 
                         cols = cols, 
                         min.x = min(share_vec, na.rm = TRUE), 
                         max.x = max(share_vec, na.rm = TRUE)), 
         border = "grey60",
         main = "Median wage",
         add=TRUE)

# reoutline the cantons
PlotKant(1:26, add=TRUE, border="grey30", lwd=1)
# add the waters
AddLakes(1:2, col=rgb(235, 247, 253, max=255), border=rgb(0,166,235, max=255))
# Add legend
min_share <- min(share_vec, na.rm = TRUE)
max_share <- max(share_vec, na.rm = TRUE)
mid_share <- (min_share + max_share)/2

#ColorLegend(x="left", width=10000, labels=round(c(min_share, mid_share, max_share), 0), 
#            cols=cols, cex=0.6, adj=c(0.9,0.5), frame="grey", inset=c(-0.09, 0), title = "% MW workers")

dev.off()
