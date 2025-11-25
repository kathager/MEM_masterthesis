## ---------------------------
##
## Master thesis
## Look at distribution of firm-level gap
## Author: Katja Hager
##
## Date Created: Oct 2025
##
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation

library(lifecycle)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(Hmisc)
theme_set(theme_minimal())

## ---------------------------
#load data
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")
gap_exposure_firm <-  readRDS(file = "output/data/2c_gap_exposure_firm.rds")

## ----------------------
summary(LSE_08_18$gap_exposure_std)
summary(LSE_08_18$gap_exposure_cons_std)

#conservative gap, non-standardized
ggplot(gap_exposure_firm[gap_exposure_firm$erhebja==2012, ], aes(x = gap_exposure_cons)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),bins = 80) +   
  scale_y_continuous(labels = scales::comma) + labs(x = "Gap", y = "#Firms")
ggsave("output/graph/gap_cons_distr_2012.pdf", width = 18, height = 6, unit = "cm")

#2393 -> total
sum(!is.na((gap_exposure_firm$gap_exposure_cons[gap_exposure_firm$erhebja==2012])))
#2384 -> 9 firms less
sum(!is.na((gap_exposure_firm$gap_exposure_cons[gap_exposure_firm$erhebja==2012 & 
                                                  gap_exposure_firm$gap_exposure_cons<0.5])))

ggplot(gap_exposure_firm[gap_exposure_firm$erhebja==2012&  
                           gap_exposure_firm$gap_exposure_cons<0.5& gap_exposure_firm$gap_exposure_cons>0, ], 
       aes(x = gap_exposure_cons)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + labs(x = "Gap", y = "#Firms") 
ggsave("output/graph/gap_cons_distr_2012_zoomed.pdf", width = 18, height = 6, unit = "cm")


## ---


summary(gap_exposure_firm$gap_exposure[gap_exposure_firm$erhebja==2012])
#2393 -> total
sum(!is.na((gap_exposure_firm$gap_exposure[gap_exposure_firm$erhebja==2012])))
#2383 -> 10 firms less
summary(gap_exposure_firm$gap_exposure[!is.na(gap_exposure_firm$gap_exposure) & 
                                                gap_exposure_firm$erhebja==2012 & 
                                         gap_exposure_firm$gap_exposure>=0.5])

#graphs
#0) distribution of firm-level gap in 2012
ggplot(gap_exposure_firm[gap_exposure_firm$erhebja==2012, ], aes(x = gap_exposure)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "Gap", y = "#Firms")
ggsave("output/graph/gap_distr_2012.pdf")
ggplot(gap_exposure_firm[gap_exposure_firm$erhebja==2012 &  gap_exposure_firm$gap_exposure<0.5& 
                           gap_exposure_firm$gap_exposure>0, ], 
       aes(x = gap_exposure)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "Gap", y = "#Firms") 
ggsave("output/graph/gap_distr_2012_zoomed.pdf") #10 firms with gap>0 less for better visualization

# worker-based distribution of gap in 2012
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012, ], 
       aes(x = gap_exposure, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers")
ggsave("output/graph/gap_distr_workers_2012.pdf")
ggplot(LSE_08_18[LSE_08_18$erhebja==2012 &  LSE_08_18$gap_exposure<0.5& 
                   LSE_08_18$gap_exposure>0, ], 
       aes(x = gap_exposure, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers") 
ggsave("output/graph/gap_distr_workers_2012_zoomed.pdf")




# worker-based distribution of gap in 2012
ggplot(data = LSE_08_18[LSE_08_18$erhebja==2012, ], 
       aes(x = gap_exposure_cons, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers")
ggsave("output/graph/gap_cons_distr_workers_2012.pdf")
ggplot(LSE_08_18[LSE_08_18$erhebja==2012 &  LSE_08_18$gap_exposure_cons<0.5& 
                   LSE_08_18$gap_exposure_cons>0, ], 
       aes(x = gap_exposure_cons, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers") 
ggsave("output/graph/gap_cons_distr_workers_2012_zoomed.pdf")



## ----------------------
#both for 2010 and 2012
#maybe show the difference, since also different definition in ESS
#firm-level gap in 2010 + 2012
ggplot(gap_exposure_firm[gap_exposure_firm$erhebja %in% c(2010, 2012) & 
                           gap_exposure_firm$gap_exposure<0.5 & 
                           gap_exposure_firm$gap_exposure>0, ], 
       aes(x = gap_exposure, colour = erhebja)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "Gap", y = "#Firms") +
  facet_wrap(~erhebja, scales = "free_y")
ggsave("output/graph/gap_distr_2010_2012_zoomed.pdf")
#workers in 2010 + 2012
ggplot(LSE_08_18[LSE_08_18$erhebja %in% c(2010, 2012) & 
                   LSE_08_18$gap_exposure<0.5 & 
                   LSE_08_18$gap_exposure>0, ], 
       aes(x = gap_exposure, colour = erhebja, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers") +
  facet_wrap(~erhebja, scales = "free_y")
ggsave("output/graph/gap_workers_distr_2010_2012_zoomed.pdf")


ggplot(gap_exposure_firm[gap_exposure_firm$erhebja  %in% c(2010, 2012)& 
                           gap_exposure_firm$gap_exposure_cons<0.5 & 
                           gap_exposure_firm$gap_exposure_cons>0, ], 
       aes(x = gap_exposure_cons, colour = erhebja)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "Gap", y = "#Firms") +
  facet_wrap(~erhebja, scales = "free_y")
ggsave("output/graph/gap_cons_distr_2010_2012_zoomed.pdf")

#workers in 2010 + 2012
ggplot(LSE_08_18[LSE_08_18$erhebja %in% c(2010, 2012) & 
                   LSE_08_18$gap_exposure_cons<0.5 & 
                   LSE_08_18$gap_exposure_cons>0, ], 
       aes(x = gap_exposure_cons, colour = erhebja, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers") +
  facet_wrap(~erhebja, scales = "free_y")
ggsave("output/graph/gap_cons_workers_distr_2010_2012_zoomed.pdf")

sd(gap_exposure_firm$gap_exposure[gap_exposure_firm$erhebja ==2012], na.rm = TRUE)
sd(gap_exposure_firm$gap_exposure[gap_exposure_firm$erhebja ==2010], na.rm = TRUE)

## ----------------------
#differentiate by CBA coverage
ggplot(gap_exposure_firm[gap_exposure_firm$erhebja  %in% c(2012)& 
                           gap_exposure_firm$gap_exposure<0.5 & 
                           gap_exposure_firm$gap_exposure>0, ], 
       aes(x = gap_exposure, colour = mem_cba)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "Gap", y = "#Firms") +
  facet_wrap(~mem_cba, scales = "free_y")
ggsave("output/graph/gap_distr_cba_2012_zoomed.pdf")

ggplot(gap_exposure_firm[gap_exposure_firm$erhebja  %in% c(2012)& 
                           gap_exposure_firm$gap_exposure_cons<0.5 & 
                           gap_exposure_firm$gap_exposure_cons>0, ], 
       aes(x = gap_exposure_cons, colour = mem_cba)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "Gap", y = "#Firms") +
  facet_wrap(~mem_cba, scales = "free_y")
ggsave("output/graph/gap_cons_distr_cba_2012_zoomed.pdf")

#workers
ggplot(LSE_08_18[LSE_08_18$erhebja %in% c(2012) & 
                   LSE_08_18$gap_exposure<0.5 & 
                   LSE_08_18$gap_exposure>0, ], 
       aes(x = gap_exposure, colour = mem_cba, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers") +
  facet_wrap(~mem_cba, scales = "free_y")
ggsave("output/graph/gap_cba_workers_distr_2012_zoomed.pdf")

ggplot(LSE_08_18[LSE_08_18$erhebja %in% c(2012) & 
                   LSE_08_18$gap_exposure_cons<0.5 & 
                   LSE_08_18$gap_exposure_cons>0, ], 
       aes(x = gap_exposure_cons, colour = mem_cba, weight = gewicht)) +
  geom_histogram(fill = scales::viridis_pal(option = "A")(1), 
                 color = scales::viridis_pal(option = "D")(1),
                 bins = 80) + 
  labs(x = "gap", y = "#workers") +
  facet_wrap(~mem_cba, scales = "free_y")
ggsave("output/graph/gap_cons_cba_workers_distr_2012_zoomed.pdf")
