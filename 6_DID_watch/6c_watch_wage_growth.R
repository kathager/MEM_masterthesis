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
    geom_point(size=0.01))}

plot_defaults_2 <- function() {
  list(
    geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.95, linewidth = 0.5),
    scale_colour_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_y_continuous(labels = scales::percent),
    scale_x_continuous(labels = scales::percent),
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)),
    geom_point(size = 0.01) ) }

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

data_MEM_watch <- worker_panel_full[!is.na(worker_panel_full$wage_change_nominal_12) &
                                      worker_panel_full$erhebja>2012 &
                                      !is.na(worker_panel_full$dist_MW_12) & 
                                      !is.na(worker_panel_full$sector_2012), ]
summary(data_MEM_watch$wage_change_nominal_12)
sum(data_MEM_watch$wage_change_nominal_12>= 1.5 | data_MEM_watch$wage_change_nominal_12<= -1.5) #754 obs
data_MEM_watch <- data_MEM_watch %>% filter(wage_change_nominal_12<=1.5)

summary(data_MEM_watch$dist_MW_12_rel)
summary(data_MEM_watch$wage_change_nominal_12)
sum(!is.na(data_MEM_watch$wage_change_nominal_12)) #128774

## ---------------------------
#differentiate by sector
model_sector_post <- feols(wage_change_nominal_12~dist_MW_12_rel, 
                           weights = ~gewicht,
                           split = ~sector_2012,
                           data = data_MEM_watch[data_MEM_watch$dist_MW_12<=0, ])
coefs_model_sector_post <- coef(model_sector_post) %>%
  rename(sector_2012 = sample,
         value = dist_MW_12_rel) %>%
  select(c(value, sector_2012))

ggplot(data = data_MEM_watch[data_MEM_watch$dist_MW_12<=0, ],
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL)  +
  plot_defaults_2() + legend() +
  geom_vline(xintercept =0, linetype = "dashed") + 
  geom_label(data = coefs_model_sector_post, 
             aes(x = ifelse(sector_2012 == "MEM", -0.53, -0.53),
                 y = ifelse(sector_2012 == "MEM", 1.5, 1.35), 
                 label = ifelse(sector_2012 == "MEM",
                                paste0("β_mem = ", round(value, 2)),
                                paste0("β_watch = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_lm_2018.pdf", width = 10, height = 6, unit = "cm")


ggplot(data = data_MEM_watch[data_MEM_watch$dist_MW_12<=600, ],
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL)  +  
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95, linewidth = 0.5)+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2))+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  geom_point(size = 0.08)+   legend()
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_auto_2018.pdf", width = 10, height = 6, unit = "cm")


#transition -> do not show
model_sector_transition16 <- feols(wage_change_nominal_12~dist_MW_12_rel, 
                                   weights = ~gewicht,
                                   split = ~sector_2012,
                                   data = data_MEM_watch[data_MEM_watch$dist_MW_12<=0 & 
                                                           data_MEM_watch$erhebja==2018 &
                                                           data_MEM_watch$transition_2016==1 | 
                                                           data_MEM_watch$dist_MW_12<=0 & 
                                                           data_MEM_watch$erhebja==2018 &
                                                           data_MEM_watch$sector_2012=="Watch", ])
coefs_model_sector_trans <- coef(model_sector_transition16) %>%
  rename(sector_2012 = sample,
         value = dist_MW_12_rel) %>%
  select(c(value, sector_2012))

ggplot(data = data_MEM_watch[data_MEM_watch$dist_MW_12<=0 & 
                               data_MEM_watch$erhebja==2018 &
                               data_MEM_watch$transition_2016==1 | 
                               data_MEM_watch$dist_MW_12<=0 & 
                               data_MEM_watch$erhebja==2018 &
                               data_MEM_watch$sector_2012=="Watch", ],
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL)  +
  plot_defaults_2() + legend() +
  geom_vline(xintercept =0, linetype = "dashed") + 
  geom_label(data = coefs_model_sector_trans, 
             aes(x = ifelse(sector_2012 == "MEM", -0.53, -0.53),
                 y = ifelse(sector_2012 == "MEM", 1.5, 1.35), 
                 label = ifelse(sector_2012 == "MEM",
                                paste0("β_mem = ", round(value, 2)),
                                paste0("β_watch = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_lm_2018_trans.pdf")



#yearly
model_sector <- feols(wage_change_nominal_12~i(erhebja, dist_MW_12_rel), 
                      weights = ~gewicht,
                      split = ~sector_2012,
                      data = data_MEM_watch[data_MEM_watch$dist_MW_12_rel<=0, ])
coefs_model_sector <- coef(model_sector) %>%
  pivot_longer(cols = starts_with("erhebja::"), 
               names_to = "year", 
               values_to = "value"  )  %>%
  mutate(erhebja = sapply(str_extract_all(year, "\\d+"), `[`, 1))  %>%
  select(-c(year)) %>%
  rename(sector_2012=sample)

ggplot(data = data_MEM_watch[data_MEM_watch$dist_MW_12<=0, ],
       aes(x=dist_MW_12_rel, y = wage_change_nominal_12, weight = gewicht, 
           colour = factor(sector_2012), fill = factor(sector_2012))) + 
  labs(x = "relative distance to minimum wage in 2012",y = "wage change", 
       colour = NULL, fill = NULL)  +
  plot_defaults_2() + legend() +
  geom_label(data = coefs_model_sector, 
             aes(x = ifelse(sector_2012 == "MEM", -0.53, -0.53),
                 y = ifelse(sector_2012 == "MEM", 1.5, 1.35), 
                 label = ifelse(sector_2012 == "MEM",
                                paste0("β_mem = ", round(value, 2)),
                                paste0("β_watch = ", round(value, 2)))),
             colour = "black", size = 2.5, inherit.aes = FALSE, fill = "white")+
  facet_grid(~erhebja, scales = "free_y") + 
  geom_vline(xintercept =0, linetype = "dashed")
ggsave("output/graph/dist_MW_vs_wage_change_12_sectors_lm_raw.pdf")



