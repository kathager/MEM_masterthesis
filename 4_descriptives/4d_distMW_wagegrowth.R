## ---------------------------
##
## Master thesis
## Exploit panel structure of LSE (from 2012 onwards) -> worker-level
## Author: Katja Hager
##
## Date Created: Oct 2025
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
library(scales)
library(fixest)
library(broom)
theme_set(theme_minimal())

plot_defaults_3 <- function() {
  list(
    geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95, linewidth = 0.5),
    scale_colour_manual(values = scales::viridis_pal(option = "D")(3)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(3)),
    scale_y_continuous(labels = scales::percent),
    scale_x_continuous(labels = scales::percent),
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)),
    geom_point(size=0.05))}

plot_defaults_2 <- function() {
  list(
    geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95, linewidth = 0.5),
    scale_colour_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_y_continuous(labels = scales::percent),
    scale_x_continuous(labels = scales::percent),
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)),
    geom_point(size = 0.08) ) }

legend <- function() {
  list(
    theme(legend.position = "inside",
          legend.position.inside = c(0, 0),
          legend.justification.inside = c(0, 0),
          legend.background = element_rect(colour = "white"),
          strip.text = element_text(face = "bold"),
          legend.text=element_text(size=10),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.3, "cm"))  )}

## ---------------------------
#compare workers' wage growth relative to dist_to_MW
#dist_to_MW -> in percent. (wage-MW)/MW = how many percent of the MW is my wage off
#If I earn 330.- less than 3'300, I am 0.1*100 = 10% away from the MW

worker_panel_full <- readRDS(file = "output/data/2d_worker_panel.rds")
worker_panel_full$main_sector[is.na(worker_panel_full$main_sector)] <- "Rest"

worker_panel_full <- worker_panel_full %>%
  mutate(
    sector_2012 = ifelse(mem_2012 ==1, "MEM", "Watch")   ) %>%  #sector_2012 = which dataset
  group_by(avs_nb) %>%
  mutate(
    main_MEM_2012 = ifelse(any(main_sector=="MEM" & erhebja==2012), 1, 0)   )

table(worker_panel_full$main_MEM_2012)
table(worker_panel_full$sector_2012)

data_MEM_watch <- worker_panel_full[!is.na(worker_panel_full$wage_change_nominal_12) &
                                worker_panel_full$erhebja>2012 &
                                !is.na(worker_panel_full$dist_MW_12) & 
                                !is.na(worker_panel_full$sector_2012), ]
summary(data_MEM_watch$wage_change_nominal_12)s
sum(data_MEM_watch$wage_change_nominal_12>= 1.5 | data_MEM_watch$wage_change_nominal_12<= -1.5) #754 obs
data_MEM_watch <- data_MEM_watch %>%
  filter(wage_change_nominal_12<=1.5)

data_MEM <- worker_panel_full[!is.na(worker_panel_full$wage_change_nominal_12) &
                                 worker_panel_full$erhebja>2012 &
                                 !is.na(worker_panel_full$dist_MW_12) & 
                                 !is.na(worker_panel_full$sector_2012) &
                                 worker_panel_full$mem_2012==1, ]

summary(data_MEM$dist_MW_12_rel)
summary(data_MEM$wage_change_nominal_12)
sum(!is.na(data_MEM$wage_change_nominal_12)) #128774
sum(data_MEM$wage_change_nominal_12>= 1.5 | data_MEM$wage_change_nominal_12<= -1.5) #635 obs
table((data_MEM$wage_change_nominal_12>= 1.5 | data_MEM$wage_change_nominal_12<= -1.5), data_MEM$erhebja)

worker_panel <- data_MEM[data_MEM$wage_change_nominal_12<1.5,]

table(worker_panel$main_sector, worker_panel$erhebja)
table(worker_panel$main_MEM_2012)

worker_panel <- worker_panel %>% ungroup() %>%
  mutate(
    mem_stay = case_when(main_sector=="MEM" & main_MEM_2012==1 ~ "Stayed",
                          TRUE ~ "Left" )   )
table(worker_panel$mem_stay)

## -------- chapter descriptives -------
#wage_change_nominal_12

#1) estimate the slope until MW
model <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), weights = ~gewicht,
            data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model <- tidy(model) %>% filter(str_detect(term, "erhebja::.*")) %>%
  mutate(erhebja = str_remove_all(term, "erhebja::|:dist_MW_12_rel") %>% as.numeric()) %>% select(estimate, erhebja)

ggplot(data = worker_panel[worker_panel$dist_MW_12_rel<=0,], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, 
                                weight = gewicht, colour = factor(erhebja))) + 
  geom_label(data = coefs_model, 
            aes(x =-0.55, y = 1.5, label = paste0("β = ", round(estimate, 2))),
            colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white") +
  geom_vline(xintercept =0, linetype = "dashed") +
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", colour = NULL) +
  plot_defaults_3() +
  facet_grid(~erhebja, scales = "free_y") + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed.pdf", width = 18, height = 8, unit = "cm")

#not zoomed
ggplot(data = worker_panel[worker_panel$dist_MW_12_rel<=2, ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, colour = factor(erhebja))) + 
  labs(x = "relativ distance to minimum wage in 2012",y = "wage change", colour = NULL) +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0) + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95, linewidth = 0.5) +
  theme(legend.position = "none") + 
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  geom_point(size = 0.01)
ggsave("output/graph/dist_MW_vs_wage_change_12.pdf", width = 18, height = 8, unit = "cm")

#differentiate by CBA
#1) estimate the slope until MW
model_cba <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                    weights = ~gewicht,
                    split = ~cba,
                    data = worker_panel[worker_panel$dist_MW_12_rel<=0 &
                                          !is.na(worker_panel$cba), ])
coefs_model_cba <- coef(model_cba) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(cba=sample) 

ggplot(data = worker_panel[worker_panel$dist_MW_12_rel<=0 & 
                             !is.na(worker_panel$cba), ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(cba), fill = factor(cba))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) +
  plot_defaults_2() +
  geom_vline(xintercept =0, linetype = "dashed", linewidth = 0.5) +
  geom_label(data = coefs_model_cba, 
             aes(x = ifelse(cba == "CBA covered", -0.5, -0.51),
                 y = ifelse(cba == "CBA covered", 1.5, 1.35), 
                 label = ifelse(
                   cba == "CBA covered",
                   paste0("β_cba = ", round(value, 2)),
                   paste0("β_no = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE,fill = "white") +
  legend() +  facet_grid(~erhebja, scales = "free_y")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_cba.pdf", width = 18, height = 8, unit = "cm")


#differentiate by stayer/leaver
#1) estimate the slope until MW
model_stay <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                    weights = ~gewicht,
                    split = ~mem_stay,
                    data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_stay <- coef(model_stay) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(mem_stay=sample) %>%
  mutate(mem_stay = ifelse(mem_stay=="Stayed", 1, 0))

ggplot(data = worker_panel[!is.na(worker_panel$wage_change_nominal_12) &
                             worker_panel$dist_MW_12<=0, ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(mem_stay), fill = factor(mem_stay))) + 
  geom_label(data = coefs_model_stay, 
             aes(x = ifelse(mem_stay == 1, -0.535, -0.53),
                 y = ifelse(mem_stay == 1, 1.5, 1.35), 
                 label = ifelse(mem_stay == 1,
                   paste0("β_stay = ", round(value, 2)),
                   paste0("β_leave = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white") +
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) +
  plot_defaults_3()+
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed") +
  legend() 
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_stay_leave.pdf", width = 18, height = 8, unit = "cm")


#by transition_2016
worker_panel$transition <- case_when(
  worker_panel$transition_2016==1 ~"2016",
  worker_panel$transition_2016==0 ~"2018" )

model_transition <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                    weights = ~gewicht,
                    split = ~transition,
                    data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_transition <- coef(model_transition) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(transition=sample)

ggplot(data = worker_panel[worker_panel$dist_MW_12<=0,  ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(transition), fill = factor(transition))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) + 
  geom_label(data = coefs_model_transition, 
             aes(x = ifelse(transition == "2016", -0.535, -0.53),
                 y = ifelse(transition == "2016", 1.5, 1.35), 
                 label = ifelse(transition == "2016",
                                paste0("β_2016 = ", round(value, 2)),
                                paste0("β_2018 = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  plot_defaults_3()+  legend() +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_transition_2016.pdf", width = 18, height = 8, unit = "cm")

#by gender
model_gender <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                          weights = ~gewicht,
                          split = ~geschle,
                          data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_gender <- coef(model_gender) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(geschle=sample)

ggplot(data = worker_panel[worker_panel$dist_MW_12<=0,  ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(geschle), fill = factor(geschle))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) + 
  geom_label(data = coefs_model_gender, 
             aes(x = ifelse(geschle == "women", -0.53, -0.53),
                 y = ifelse(geschle == "women", 1.5, 1.35), 
                 label = ifelse(geschle == "women",
                                paste0("β_women = ", round(value, 2)),
                                paste0("β_men = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  plot_defaults_3()+  legend() +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_gender.pdf", width = 18, height = 8, unit = "cm")

#by nationality
model_national <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                      weights = ~gewicht,
                      split = ~natkat2,
                      data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_national <- coef(model_national) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(natkat2=sample)

ggplot(data = worker_panel[worker_panel$dist_MW_12<=0 & 
                             !is.na(worker_panel$natkat2),  ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(natkat2), fill = factor(natkat2))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) + 
  geom_label(data = coefs_model_national, 
             aes(x = case_when(
               natkat2 == "CH/C-permit"~ -0.53,
               natkat2 == "B-/L-permit"~ -0.53,
               natkat2 == "cross-border"~ -0.53),
             y = case_when(
               natkat2 == "CH/C-permit"~ 1.5,
               natkat2 == "B-/L-permit"~ 1.35,
               natkat2 == "cross-border"~ 1.2), 
             label = case_when(
               natkat2 == "CH/C-permit"~  paste0("β_resident = ", round(value, 2)),
               natkat2 == "B-/L-permit"~ paste0("β_temp = ", round(value, 2)),
               natkat2 == "cross-border"~ paste0("β_cross = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  plot_defaults_3()+  legend() +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_national.pdf", width = 18, height = 8, unit = "cm")

#age
table(worker_panel$age)
model_age <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                        weights = ~gewicht,
                        split = ~age,
                        data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_age <- coef(model_age) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(age=sample)

ggplot(data = worker_panel[worker_panel$dist_MW_12<=0 & 
                             !is.na(worker_panel$natkat2),  ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(age), fill = factor(age))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) + 
  geom_label(data = coefs_model_age, 
             aes(
               x = case_when(
                 age == "Middle"~ -0.53,
                 age == "Old"~ -0.53,
                 age == "Young"~ -0.53),
               y = case_when(
                 age == "Middle"~ 1.5,
                 age == "Old"~ 1.35,
                 age == "Young"~ 1.2), 
               label = case_when(
                 age == "Middle"~  paste0("β_middle = ", round(value, 2)),
                 age == "Old"~ paste0("β_old = ", round(value, 2)),
                 age == "Young"~ paste0("β_young = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  plot_defaults_3()+  legend() +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_age.pdf", width = 18, height = 8, unit = "cm")



#tenure
table(worker_panel$tenure)
model_tenure <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                   weights = ~gewicht,
                   split = ~tenure,
                   data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_tenure <- coef(model_tenure) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(tenure=sample)

ggplot(data = worker_panel[worker_panel$dist_MW_12<=0 & 
                             !is.na(worker_panel$natkat2),  ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(tenure), fill = factor(tenure))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) + 
  geom_label(data = coefs_model_tenure, 
             aes(
               x = case_when(
                 tenure == "Long"~ -0.53,
                 tenure == "Middle"~ -0.53,
                 tenure == "Short"~ -0.53),
               y = case_when(
                 tenure == "Long"~ 1.5,
                 tenure == "Middle"~ 1.35,
                 tenure == "Short"~ 1.2), 
               label = case_when(
                 tenure == "Long"~  paste0("β_long = ", round(value, 2)),
                 tenure == "Middle"~ paste0("β_middle = ", round(value, 2)),
                 tenure == "Short"~ paste0("β_short = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  plot_defaults_3()+  legend() +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_tenure.pdf", width = 18, height = 8, unit = "cm")

#qualification
table(worker_panel$qualification)
model_qualification <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                      weights = ~gewicht,
                      split = ~qualification,
                      data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_qualification <- coef(model_qualification) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(qualification=sample)

ggplot(data = worker_panel[worker_panel$dist_MW_12<=0 & 
                             !is.na(worker_panel$natkat2),  ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(qualification), fill = factor(qualification))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) + 
  geom_label(data = coefs_model_qualification, 
             aes(
               x = case_when(
                 qualification == "high skilled"~ -0.535,
                 qualification == "low skilled"~ -0.535,
                 qualification == "middle skilled"~ -0.535),
               y = case_when(
                 qualification == "high skilled"~ 1.5,
                 qualification == "low skilled"~ 1.35,
                 qualification == "middle skilled"~ 1.2), 
               label = case_when(
                 qualification == "high skilled"~  paste0("β_high = ", round(value, 2)),
                 qualification == "low skilled"~ paste0("β_low = ", round(value, 2)),
                 qualification == "middle skilled"~ paste0("β_middle = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  plot_defaults_3()+  legend() +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_qualification.pdf", width = 18, height = 8, unit = "cm")

#wage region
table(worker_panel$wage_region)
model_wage_region <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                             weights = ~gewicht,
                             split = ~wage_region,
                             data = worker_panel[worker_panel$dist_MW_12_rel<=0, ])
coefs_model_wage_region <- coef(model_wage_region) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(wage_region=sample)

ggplot(data = worker_panel[worker_panel$dist_MW_12<=0 & 
                             !is.na(worker_panel$natkat2),  ], 
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(wage_region), fill = factor(wage_region))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL) + 
  geom_label(data = coefs_model_wage_region, 
             aes(
               x = case_when(
                 wage_region == "A"~ -0.535,
                 wage_region == "B"~ -0.535,
                 wage_region == "C"~ -0.535),
               y = case_when(
                 wage_region == "A"~ 1.5,
                 wage_region == "B"~ 1.35,
                 wage_region == "C"~ 1.2), 
               label = case_when(
                 wage_region == "A"~  paste0("β_A = ", round(value, 2)),
                 wage_region == "B"~ paste0("β_B = ", round(value, 2)),
                 wage_region == "C"~ paste0("β_C = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  plot_defaults_3()+  legend() +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_zoomed_wage_region.pdf", width = 18, height = 8, unit = "cm")




## -------------

#blimok_change_12
ggplot(data = worker_panel[!is.na(worker_panel$wage_change_nominal_12) &
                                      worker_panel$dist_MW_12<=300 & 
                                      worker_panel$erhebja>2012, ], 
       aes(x=dist_MW_12, y = blimok_change_12, , weight = gewicht, colour = factor(erhebja))) + #
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Distance to minimum wage in 2012",y = "%change in earnings", colour = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0) +
  theme(legend.position = "none")
ggsave("output/graph/dist_MW_vs_blimok_change_12_zoomed.pdf")

ggplot(data = worker_panel[!is.na(worker_panel$wage_change_nominal_12) & 
                                      worker_panel$erhebja>2012, ], 
       aes(x=dist_MW_12, y = blimok_change_12, weight = gewicht, colour = factor(erhebja))) + 
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Distance to minimum wage in 2012",y = "%change in earnings", colour = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0) +
  theme(legend.position = "none")
ggsave("output/graph/dist_MW_vs_blimok_change_12.pdf")



#dist_to_MW_HIGH
ggplot(data = worker_panel[!is.na(worker_panel$wage_change_nominal_12) &
                             !is.na(worker_panel$dist_to_MW_high) &
                             !is.na(worker_panel$erhebja) &
                             worker_panel$dist_to_MW_high<=300 &
                             worker_panel$dist_to_MW_high>=-1000 &
                             worker_panel$erhebja>2012, ], 
       aes(x=dist_to_MW_high, y = wage_change_nominal_12, weight = gewicht, colour = factor(erhebja))) + #
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Distance to minimum wage in 2012",y = "%change in earnings", colour = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0) +
  theme(legend.position = "none")

ggplot(data = worker_panel[!is.na(worker_panel$wage_change_nominal_12) &
                             !is.na(worker_panel$dist_to_MW_high) &
                             !is.na(worker_panel$erhebja) &
                             worker_panel$dist_to_MW_high<=300 &
                             worker_panel$dist_to_MW_high>=-1000 &
                             worker_panel$erhebja>2012, ], 
       aes(x=dist_to_MW_high, y = blimok_change_12, weight = gewicht, colour = factor(erhebja))) + #
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  labs(x = "Distance to minimum wage in 2012",y = "%change in earnings", colour = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0) +
  theme(legend.position = "none")


## ---------------------------
#in each year: who is in the data?
#statistics for each year (2014, 2016, 2018)

worker_panel_chars <- worker_panel %>%
  filter(erhebja>2012) %>%
  group_by(erhebja) %>%
  filter(!is.na(wage_change_nominal_12)) %>%
  mutate(
    perc_male = sum(gewicht[geschle == "men"])/sum(gewicht),
    perc_lowskill = sum(gewicht[qualification == "low skilled"])/sum(gewicht),
    perc_nomgt = sum(gewicht[job_position == "No Management"])/sum(gewicht),
    perc_resident = sum(gewicht[natkat2 == "CH/C-permit"])/sum(gewicht),
    mean_age = wtd.mean(alter, weight = gewicht),
    mean_tenure = wtd.mean(dienstja, weight = gewicht),
    cba = sum(gewicht[mem_cba=="1"])/sum(gewicht),
    p10 = wtd.quantile(real_wage, weights = gewicht, 
                       probs = 0.1, na.rm = TRUE),
    percentile = wtd.mean(gross_wage_std <= min_wage, weight = gewicht, na.rm = TRUE) * 100,
    min_wage = wtd.mean(min_wage, weight = gewicht, na.rm = TRUE),
    A = sum(gewicht[wage_region == "A"])/sum(gewicht),
    B = sum(gewicht[wage_region == "B"])/sum(gewicht),
    C = sum(gewicht[wage_region == "C"])/sum(gewicht)) %>%
  group_by(erhebja) %>%
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
    "Wage region A" = mean(A, na.rm = TRUE)*100,
    "Wage region B" = mean(B, na.rm = TRUE)*100,
    "Wage region C" = mean(C, na.rm = TRUE)*100,
    "N" = sum(!is.na(avs_nb))) %>% ungroup() %>% 
  mutate_if(is.numeric, round, digits = 0) 

## ---------------------------
%>%
  pivot_longer(
    cols = -mclust,
    names_to = "variable",
    values_to = "value") %>%
  pivot_wider(
    names_from = mclust,
    values_from = value) %>%
  rename(" " = variable) 


data_2014 <- worker_panel[worker_panel$erhebja==2014 & 
                                     !is.na(worker_panel$wage_change_nominal_12), ]
data_2016 <- worker_panel[worker_panel$erhebja==2016 &
                                     !is.na(worker_panel$wage_change_nominal_12), ]
data_2018 <- worker_panel[worker_panel$erhebja==2018 &
                                     !is.na(worker_panel$wage_change_nominal_12), ]

datasets <- list('2014' = data_2014, 2016 = data_2016, 2018 = data_2018)

all_statistics <- data.frame()
for (data in c("yr_2014", "yr_2016", "yr_2018")) {
  sample <- datasets[[data]] 
  
  statistics_overall <- sample  %>%
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
      
      #policy
      coverage = round(weighted.mean(mem_cba==1, w = gewicht, na.rm = TRUE) * 100, 1),
      
      
      n_obs = n(),
      .groups = 'drop') %>%
    mutate(group = paste0("Year ", data))
  
  all_statistics <- bind_rows(all_statistics, statistics_overall)
}


# Create the wide format table
all_statistics_wide <- all_statistics %>%
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
          out = "Workers_2012_characteristics_all.tex")

