## ---------------------------
##
## Master thesis
## Variable: Lohnverhandlung
## Check plausibility and evolution of coverage
## Author: Katja Hager
##
## Date Created: June 2025
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

library(fixest) #feol()
library(broom) #tidy

library(sandwich)
library(lmtest)
library(stringr)
library(estimatr)
theme_set(theme_minimal())
## ---------------------------
#load merged and filtered file with created variables -> only MEM sector
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds")
##-------------------------------
#check plausibilty / # obs

#weighted counts
ggplot(data = LSE_08_18[LSE_08_18$lohnver != "-9", ], 
       aes(x = erhebja, weight = gewicht, fill = lohnver)) +
  geom_bar(position = "dodge") + 
  labs(x = NULL, y = "# workers", fill = "Level") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(4))
ggsave("output/graph/lohnver_level_counts_by_year.pdf")

#unweighted (just number of observations)
ggplot(data = LSE_08_18[LSE_08_18$lohnver != "-9", ], 
       aes(x = erhebja, fill = lohnver)) +
  geom_bar(position = "dodge") + 
  labs(x = NULL, y = "# workers", fill = "Level") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))+
  theme_minimal() +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(4))
ggsave("output/graph/lohnver_level_counts_by_year_unweighted.pdf")

table(LSE_08_18$lohnver, LSE_08_18$erhebja)

## ---------------------------
## coverage 
LSE_08_18$mem_numeric <- case_when(
  LSE_08_18$cba == "CBA covered" ~ 1,
  LSE_08_18$cba == "Not covered" ~0,
  TRUE ~NA_real_ )

# overall MEM Sector, yearly coverage, Worker-level
mem_cba_cov_gewicht <- feols(mem_numeric ~ 1, split = ~erhebja,
                             weights = ~gewicht,data = LSE_08_18)
tidy_mem_cba_cov_gewicht <- map_dfr(mem_cba_cov_gewicht, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>%as.numeric(),
    erhebja = NULL, gewicht = "gewicht")

mem_cba_cov_gewibgrs <- feols(mem_numeric ~ 1, split = ~erhebja,
                              weights = ~gewibgrs, data = LSE_08_18)
tidy_mem_cba_cov_gewibgrs <- map_dfr(mem_cba_cov_gewibgrs, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% as.numeric(),
    erhebja = NULL, gewicht = "gewibgrs")

mem_cba_cov_unweighted <- feols(mem_numeric ~ 1, split = ~erhebja, data = LSE_08_18)
tidy_mem_cba_cov_unweighted <- map_dfr(mem_cba_cov_unweighted, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% as.numeric(),
    erhebja = NULL, gewicht = "unweighted")

tidy_mem_cba_cov <- rbind(tidy_mem_cba_cov_unweighted, tidy_mem_cba_cov_gewibgrs,
                          tidy_mem_cba_cov_gewicht)

##CBA coverage in MEM sector, Worker-level
ggplot(data = tidy_mem_cba_cov, aes(x=year, y=estimate, colour = gewicht, fill = gewicht)) +
  geom_point(size = 2) + geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
              alpha = 0.5) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016,2018)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "coverage", colour = NULL, fill = NULL) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed")+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0,0),
        legend.justification.inside = c(0, 0),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"))
ggsave("output/graph/coverage_MEM_weights.pdf", width = 16, height = 5, unit = "cm")

## ---------------------------
#broad coverage
table(LSE_08_18$not_individual)
unconditional_model_broad <- feols(not_individual ~ 1, split = ~erhebja,
                                   weights = ~gewicht, data = LSE_08_18)
tidy_results_unconditional_br <- map_dfr(unconditional_model_broad, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% as.numeric(),
    erhebja = NULL)
ggplot(tidy_results_unconditional_br, aes(x = year, y = estimate)) +
  geom_point(size = 2) + geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), 
              alpha = 0.5, fill = "grey") +
  labs(x = NULL, y = "Broad CBA coverage") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016,2018)) +
  scale_y_continuous(labels = scales::percent)
ggsave("output/graph/coverage_MEM_broad.pdf", width = 18, height = 8, unit = "cm")


LSE_08_18$firm_collective <- case_when(
  LSE_08_18$lohnver == "2" ~ 1,
  LSE_08_18$lohnver %in% c("1", "3", "4") ~0,
  TRUE ~NA_real_ )
firm_collective <- feols(firm_collective ~ 1, split = ~erhebja,
                                   weights = ~gewicht, data = LSE_08_18)
coefs_firm_collective <- map_dfr(firm_collective, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% as.numeric(),
    erhebja = NULL, variable = "firm-level")

LSE_08_18$collective_3 <- case_when(
  LSE_08_18$lohnver == "3" ~ 1,
  LSE_08_18$lohnver %in% c("1", "2", "4") ~0,
  TRUE ~NA_real_ )
collective_3 <- feols(collective_3 ~ 1, split = ~erhebja,
                         weights = ~gewicht, data = LSE_08_18)
coefs_collective_3 <- map_dfr(collective_3, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% as.numeric(),
    erhebja = NULL, variable = "outside CBA")

LSE_08_18$NAs <- case_when(
  LSE_08_18$lohnver == "-9" | is.na(LSE_08_18$lohnver) ~ 1,
  LSE_08_18$lohnver %in% c("1", "2", "3", "4") ~0)
lohnver_NAs <- feols(NAs ~ 1, split = ~erhebja,
                      weights = ~gewicht, data = LSE_08_18)
coefs_lohnver_NAs <- map_dfr(lohnver_NAs, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>% as.numeric(),
    erhebja = NULL, variable = "NA")

lohnver_categories <- rbind(tidy_mem_cba_cov_gewicht %>% select(-c(gewicht)), coefs_firm_collective, 
                      coefs_collective_3, coefs_lohnver_NAs)

ggplot(lohnver_categories, aes(x = year, y = estimate, colour = variable, fill = variable)) +
  geom_point(size = 2) + geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), 
              alpha = 0.5) +
  labs(x = NULL, y = "Workers (%)", colour = "Level of wage bargaining", fill = "Level of wage bargaining") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016,2018)) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept = 2013), color = "grey", linewidth = 0.7, linetype = "dashed")+
  scale_colour_manual(values = scales::viridis_pal(option = "D")(4)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(4)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.7),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 14) ,
        legend.title = element_text(size = 15))
ggsave("output/graph/categories_lohnver.pdf", width = 18, height = 8, unit = "cm")


##the coverage we find in the data is below the coverage reported by social partners!
# understating coverage means over-estimating the effect of cba-coverage


##----------------------------
#Mit welchen Demographics korreliert die GAV-Abdeckung, über die Zeit?
levels(LSE_08_18$natkat2)  #reference level is first
levels(LSE_08_18$firm_size)
levels(LSE_08_18$geschle)
levels(LSE_08_18$qualification)
levels(LSE_08_18$berufst)

#separate regressions by year
CBA_model_yrly <- feols(mem_numeric ~ factor(natkat2) + factor(geschle) + 
                     factor(qualification) + alter + dienstja + 
                       factor(firm_size)  + factor(job_position),
                   split = ~erhebja,  weights = ~gewicht, data = LSE_08_18)

#demographics: age, gender, nationality
#labour market characteristics: education, tenure, firm-size
#general: intercept
tidy_results <- map_dfr(CBA_model_yrly, tidy, .id = "erhebja") %>%
  mutate( year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>%
      as.numeric(), erhebja = NULL, var =str_match(term, "\\(([^)]*)\\)(.*)"),
      var = ifelse(is.na(var), term, var)) %>%
  mutate(variable = var[,2],
         var_categ = var[,3]) %>%
  select(c(estimate, std.error, year, variable, var_categ)) %>%
  filter(variable != "Intercept") %>%
  mutate(variable = case_when(
    variable== "natkat2" ~ "Nationality",
    variable == "geschle" ~ "Gender", 
    variable == "qualification" ~ "Education",
    variable %in% c("alter") ~ "Age",
    variable %in% c("dienstja") ~ "Tenure",
    variable %in% c("firm_size") ~ "Firm Size",
    variable %in% c("job_position") ~ "Job Position"),
    var_category = case_when(
      variable %in% c("Nationality", "Gender", "Age") ~ "Demographics",
      variable %in% c("Education", "Tenure", "Firm Size","Job Position") ~ "Labour"))

#intercept increases: male, Swiss, high-skilled employee in a small firm 
#has higher prob of being cba-covered

#nationality
ggplot(tidy_results[tidy_results$variable == "Nationality", ], 
       aes(x = year, y = estimate, colour = var_categ, fill =  var_categ)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), alpha = 0.5) +
  labs(x = NULL, y = "Coefficient", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2))  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.95),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 18) ,
        legend.title = element_text(size = 20))
ggsave("output/graph/cba_vs_nationality.pdf", width = 18, height = 8, unit = "cm")

#gender
ggplot(tidy_results[tidy_results$variable == "Gender", ], 
       aes(x = year, y = estimate, colour = var_categ, fill =  var_categ)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), alpha = 0.5) +
  labs(x = NULL, y = "Coefficient", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(1)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(1))  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.95),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 18) ,
        legend.title = element_text(size = 20))
ggsave("output/graph/cba_vs_gender.pdf", width = 18, height = 8, unit = "cm")

#age
ggplot(tidy_results[tidy_results$variable == "Age", ], 
       aes(x = year, y = estimate, colour = variable, fill =  variable)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), alpha = 0.5) +
  labs(x = NULL, y = "Coefficient", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(1)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(1))  +
  theme(legend.position = "none")
ggsave("output/graph/cba_vs_age.pdf", width = 18, height = 8, unit = "cm")

#Education
ggplot(tidy_results[tidy_results$variable == "Education", ], 
       aes(x = year, y = estimate, colour = var_categ, fill =  var_categ)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), alpha = 0.5) +
  labs(x = NULL, y = "Coefficient", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2))  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.95),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 18) ,
        legend.title = element_text(size = 20))
ggsave("output/graph/cba_vs_education.pdf", width = 18, height = 8, unit = "cm")

#Tenure
ggplot(tidy_results[tidy_results$variable == "Tenure", ], 
       aes(x = year, y = estimate, colour = variable, fill =  variable)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), alpha = 0.5) +
  labs(x = NULL, y = "Coefficient", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(1)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(1))  +
  theme(legend.position = "none")
ggsave("output/graph/cba_vs_tenure.pdf", width = 18, height = 8, unit = "cm")

#Job Position
ggplot(tidy_results[tidy_results$variable == "Job Position", ], 
       aes(x = year, y = estimate, colour = var_categ, fill =  var_categ)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), alpha = 0.5) +
  labs(x = NULL, y = "Coefficient", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2))  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.15),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 18) ,
        legend.title = element_text(size = 20))
ggsave("output/graph/cba_vs_job_position.pdf", width = 18, height = 8, unit = "cm")

#Firm Size
ggplot(tidy_results[tidy_results$variable == "Firm Size", ], 
       aes(x = year, y = estimate, colour = var_categ, fill =  var_categ)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), alpha = 0.5) +
  labs(x = NULL, y = "Coefficient", colour = NULL, fill = NULL) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(2)) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(2))  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.95),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 18) ,
        legend.title = element_text(size = 20))
ggsave("output/graph/cba_vs_firm_size.pdf", width = 18, height = 8, unit = "cm")



## --------------------------- 
#coverage on firm level

firm_panel <- LSE_08_18 %>%
  group_by(burnr, erhebja) %>%
  summarise(
    cba_covered = mean(mem_cba),
    firm_weight = sum(gewicht),
    firm_weight_hrs = sum(gewibgrs),
    not_individual = mean(not_individual),
    .groups = "drop"
  ) 
summary(firm_panel$not_individual)
summary(firm_panel$cba_covered)

##narrow
unconditional_model <- feols(cba_covered ~ 1, 
                             split = ~erhebja,
                             weights = ~firm_weight,
                             data = firm_panel)
coefplot(unconditional_model)
tidy_unconditional_model <- map_dfr(unconditional_model, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>%
      as.numeric(),
    erhebja = NULL,
    definition = "narrow")
ggplot(tidy_unconditional_model, aes(x = year, y = estimate)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), 
              alpha = 0.5, fill = "grey") +
  labs(x = NULL, y = "CBA coverage") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2010, 2014, 2018))
ggsave("output/graph/coverage_MEM_firm.pdf")


##broad
unconditional_model_br <- feols(not_individual ~ 1, 
                                split = ~erhebja,
                                weights = ~firm_weight,
                                data = firm_panel)
coefplot(unconditional_model_br)
tidy_unconditional_model_br <- map_dfr(unconditional_model_br, tidy, .id = "erhebja")%>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>%
      as.numeric(),
    erhebja = NULL,
    definition = "broad")

ggplot(tidy_unconditional_model_br, aes(x = year, y = estimate)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), 
              alpha = 0.5, fill = "grey") +
  labs(x = NULL, y = "Collective") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2010, 2014, 2018))
ggsave("output/graph/coverage_MEM_firm_broad.pdf")


firm_both_definitions <- rbind(tidy_unconditional_model, tidy_unconditional_model_br)
ggplot(firm_both_definitions, aes(x = year, y = estimate, colour = definition)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), 
              alpha = 0.5, fill = "grey") +
  labs(x = NULL, y = "CBA coverage") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2010, 2014, 2018))
ggsave("output/graph/coverage_MEM_firm_both_definitions.pdf")

#coverage on firm level is way below official numbers -> we mis-measure something
#10 pp below true numbers

#either the variable lohnver is not the best, 
#or we systematically have less firms present which are covered by CBA

## -----------------
#cluster at noga level
CBA_model_noga <- feols(mem_cba ~ factor(natkat2) + factor(geschle) + 
                          factor(qualification) + alter + dienstja + 
                          factor(firm_size),
                        weights = ~gewicht,
                        cluster = ~noga,
                        data = LSE_08_18)
tidy_results_noga <- map_dfr(CBA_model_noga, tidy, .id = "erhebja") %>%
  mutate(
    year = str_remove(erhebja, "sample.var: erhebja; sample: ") %>%
      as.numeric(),
    erhebja = NULL,
    var = str_remove(term, "factor\\([^)]*\\)"),
    var_group = case_when(
      var %in% c("B-permit", "cross-border", "L-permit") ~ "Nationality",
      var == "women" ~ "Gender", 
      var %in% c("low skilled", "middle skilled") ~ "Education",
      var %in% c("alter") ~ "Age",
      var %in% c("dienstja") ~ "Tenure",
      var %in% c("medium (50-249)", "large (250+)") ~ "Firm Size",
      var %in% "(Intercept)" ~ "Intercept",
      TRUE ~ "Other"),
    var_category = case_when(
      var_group %in% c("Nationality", "Gender", "Age") ~ "Demographics",
      var_group %in% c("Education", "Tenure", "Firm Size") ~ "Labour",
      var_group %in% "Intercept"  ~ "Intercept"),
    var = case_when(
      var == "alter" ~ "age",
      var == "dienstja" ~ "tenure",
      TRUE ~ var)
  )

ggplot(tidy_results_noga, aes(x = year, y = estimate)) +
  geom_point(size = 1) +
  geom_line()+
  geom_ribbon(aes(ymin = estimate - 1.96*std.error, 
                  ymax = estimate + 1.96*std.error), 
              alpha = 0.3, fill = "grey") +
  labs(x = NULL, y = "Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  facet_wrap(~var) +
  scale_x_continuous(breaks = c(2010, 2014, 2018))



## ---------------------------
#Nested model comparison (econometrics way): Fit the full model, 
# then remove one variable/block and compare fit statistics
#relative importance analysis
library(dominanceanalysis)

#logistic regression: binary classification model -> discriminative
CBA_model_logit <- glm(mem_cba ~ factor(natkat2) + factor(geschle) + 
                         factor(qualification) + alter + dienstja + 
                         factor(firm_size) +
                         factor(msreg_an) + factor(noga),
                       family = binomial,
                       weights = gewicht,
                       data = LSE_08_18)

da <- dominanceAnalysis(CBA_model_logit)

print(da)
plot(da)

#SHAP
library(shapviz)

# wrap into a Predictor object
sv <- kernelshap(
  object = CBA_model_logit,
  X = LSE_08_18 %>% select(-mem_cba),
  bg_X = LSE_08_18 %>% select(-mem_cba) %>% dplyr::slice_sample(n = 200)  # background sample
)
# global importance plot
sv_importance(sv)   # bar plot of mean |SHAP|

# beeswarm plot (like SHAP summary in Python)
sv_importance(sv, kind = "beeswarm")


# compute SHAP values
shapley <- Shapley$new(pred, x.interest = LSE_08_18[1, -which(names(LSE_08_18)=="mem_cba")])
# global importance
shap <- Shapley$new(pred, sample.size = 100)  # sample subset if data huge

## ---------------------------
# packages
library(purrr)
library(tidyr)

#predictors
preds <- c("alter","dienstja","qualification","geschle","firm_size",
           "natkat2","noga","msreg_an")

df <- LSE_08_18 %>%
  select(all_of(preds))

is_num  <- function(x) is.numeric(x)
is_fac  <- function(x) is.factor(x)

# single pair association
assoc_pair <- function(x, y) {
  if (is_num(x) && is_num(y)) {
    return(abs(cor(x, y, use = "pairwise.complete.obs")))
  } else if (is_num(x) && is_fac(y)) {
    # correlation ratio η (numeric ~ factor)
    fit <- lm(x ~ y)
    as.numeric(DescTools::EtaSq(aov(fit), type = 1)["y", "eta.sq"])
  } else if (is_fac(x) && is_num(y)) {
    fit <- lm(y ~ x)
    as.numeric(DescTools::EtaSq(aov(fit), type = 1)["x", "eta.sq"])
  } else { # factor vs factor: Cramér’s V
    tbl <- table(x, y, useNA = "no")
    suppressWarnings(DescTools::CramerV(tbl, bias.correct = TRUE))
  }
}

# full association matrix
mat <- matrix(NA_real_, nrow = length(preds), ncol = length(preds),
              dimnames = list(preds, preds))
for (i in seq_along(preds)) {
  for (j in seq_along(preds)) {
    if (j >= i) {
      mat[i, j] <- assoc_pair(df[[preds[i]]], df[[preds[j]]])
    } else {
      mat[i, j] <- mat[j, i]
    }
  }
}
mat  # 0..1 scale, treats noga/msreg_an as single variables

# optional: heatmap
# install.packages("pheatmap")
pheatmap::pheatmap(mat, cluster_rows = TRUE, cluster_cols = TRUE)

## ---------------------------
#SHAP

library(shapviz)
sv <- kernelshap(
  object = mem_model,
  X = LSE_08_18 %>% select(-mem_cba),
  bg_X = LSE_08_18 %>% select(-mem_cba) %>% dplyr::slice_sample(n = 200)  # background sample
)








#SHAP values for Overall Feature Importance (but need to train model first)
library(fastshap)
library(xgboost)

# Prepare your 2012 data
data_2012 <- LSE_08_18[LSE_08_18$erhebja == 2012, ] %>%
  select(mem_cba, alter, dienstja, geschle, natkat2, qualification, firm_size, berufst) %>%
  na.omit()

#not happy to transform categorical in numerical vars
data_prepared <- data_2012 %>%
  mutate(
    geschle_num = as.numeric(as.factor(geschle)),
    natkat2_num = as.numeric(as.factor(natkat2)),
    qualification_num = as.numeric(as.factor(qualification)),
    firm_size_num = as.numeric(as.factor(firm_size)),
    berufst_num = as.numeric(as.factor(berufst))
  ) %>%
  select(mem_cba, alter, dienstja, geschle_num, natkat2_num, 
         qualification_num, firm_size_num, berufst_num)

X <- as.matrix(data_prepared) 
X <- subset(X, select = -mem_cba)# All except mem_cba
y <- data_prepared$mem_cba

sample_IDs <- sample(c(TRUE,FALSE), nrow(X), replace=TRUE, prob=c(0.7,0.3))

# creating training dataset -> select indices created above
train_X <- X[sample_IDs, ]
train_y  <- y[sample_IDs]
# creating testing dataset
test_X  <- X[!sample_IDs, ]
nrow(train_X)
test_y  <- y[!sample_IDs]

pos_weight <- sum(y == 0) / sum(y == 1)  # Ratio of negative to positive class

#train model
mem_model <- xgboost(
  data = train_X, label = train_y,
  objective = "binary:logistic", # logistic regression for classification
  eval_metric = "auc",
  nrounds = 100,
  max_depth = 6,
  eta = 0.05, #step size of each boosting step
  scale_pos_weight = pos_weight,  # Handle imbalance
  verbose = FALSE
)

#get predictions + accuracy score
predictions <- predict(mem_model, test_X)
pred_class <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(pred_class == test_y) #70%

# ===== 3. CALCULATE SHAP VALUES =====
# Calculate SHAP values for all observations
library(SHAPforxgboost)
shap_values <- shap.values(xgb_model = mem_model, X_train = X)

feature_importance <- shap_values$mean_shap_score %>%
  abs() %>%
  sort(decreasing = TRUE)
for(i in 1:length(feature_importance)) {
  print(paste(i, ".", names(feature_importance)[i], ":", 
              round(feature_importance[i], 4)))
}

shap.plot.summary.wrap1(mem_model, X = X, top_n = 8)

shap.plot.summary(shap_values$shap_score, X, top_n = 8)


## ---------------------------
#PCA
library(PCAmixdata)
library(FactoMineR)
## ---------------------------
#random forest
library(psych)
library(randomForest)
demo_vars <- LSE_08_18[, c("alter", "dienstja", "geschle", "natkat2", 
                           "qualification", "firm_size", "mem_cba",
                           "berufst")]


# Random Forest to predict mem_cba 
#wich var is most important to determine cba-status==1
rf_model <- randomForest(factor(mem_cba) ~ alter + dienstja + geschle + 
                           natkat2 + qualification + firm_size + berufst,
                         data = LSE_08_18[LSE_08_18$erhebja==2012, ],
                         importance = TRUE,
                         na.action = na.omit)
varImpPlot(rf_model)

# Get importance values
importance(rf_model)


## ------------------- 
#notes / to delete
# (lohnverh == 1 / N)
# Gewichtungsfaktor ohne individuellen Beschäftigungsgrad („gewicht“) -> Die 
# daraus resultierenden Aussagen sind personenbezogen -> needed for coverage ratio

#weighted by gewicht
coverage_weighted <- LSE_08_18 %>%
  filter(!is.na(mem_cba)) %>%
  group_by(erhebja) %>%
  summarise(
    count_N = sum(gewicht, na.rm = TRUE),
    collective_N = sum(gewicht[mem_cba ==1]),
    coverage = wtd.mean(mem_cba, weights= gewicht, na.rm = TRUE),
    weight = "gewicht")

#weighted by gewibgrs
coverage_gewibgrs <- LSE_08_18 %>%
  filter(!is.na(mem_cba)) %>%
  group_by(erhebja) %>%
  summarise(
    count_N = sum(gewibgrs, na.rm = TRUE),
    collective_N = sum(gewibgrs[mem_cba ==1]),
    coverage = wtd.mean(mem_cba, weights= gewibgrs, na.rm = TRUE),
    weight = "gewibgrs")

#unweighted -> raw counts
coverage_unweighted <- LSE_08_18 %>%
  filter(!is.na(mem_cba)) %>%
  group_by(erhebja) %>%
  summarise( 
    count_N = sum(!is.na(lohnver), na.rm = TRUE),
    collective_N = sum(mem_cba ==1),
    coverage = mean(mem_cba),
    weight = "unweighted")

##broader definition of coverage: all but individual wage agreements
#weighted by gewicht
coverage_weighted_broad <- LSE_08_18 %>%
  filter(lohnver != "-9") %>%
  group_by(erhebja) %>%
  summarise(
    count_N = sum(gewicht, na.rm = TRUE),
    collective_N = sum(gewicht[lohnver %in% c("1", "2", "3")]),
    coverage = collective_N/count_N,
    weight = "gewicht")

#weighted by gewibgrs
coverage_gewibgrs_broad <- LSE_08_18 %>%
  filter(lohnver != "-9") %>%
  group_by(erhebja) %>%
  summarise(
    count_N = sum(gewibgrs, na.rm = TRUE),
    collective_N = sum(gewibgrs[lohnver %in% c("1", "2", "3")]),
    coverage = collective_N/count_N,
    weight = "gewibgrs")

#unweighted -> raw counts
coverage_unweighted_broad <- LSE_08_18 %>%
  filter(lohnver != "-9") %>%
  group_by(erhebja) %>%
  summarise( 
    count_N = sum(!is.na(lohnver), na.rm = TRUE),
    collective_N = sum(lohnver %in% c("1", "2", "3")),
    coverage = collective_N/count_N,
    weight = "unweighted")

coverage <- full_join(coverage_unweighted, coverage_weighted,
                      by = c("erhebja", "weight", "coverage", "count_N", "collective_N"))
coverage <- full_join(coverage, coverage_gewibgrs,
                      by = c("erhebja", "weight", "coverage", "count_N", "collective_N"))

coverage_broad <- full_join(coverage_unweighted_broad, coverage_weighted_broad, 
                            by = c("erhebja", "weight", "coverage", "count_N", "collective_N"))
coverage_broad <- full_join(coverage_broad, coverage_gewibgrs_broad, 
                            by = c("erhebja", "weight", "coverage", "count_N", "collective_N"))


saveRDS(coverage, file = "7_yearly_CBA_coverage.rds")
saveRDS(coverage_broad, file = "7_yearly_CBA_coverage_broad.rds")


coverage <- readRDS("7_yearly_CBA_coverage.rds")
coverage_broad <- readRDS("7_yearly_CBA_coverage_broad.rds")

##CBA coverage in MEM sector, Worker-level
ggplot(data = coverage, 
       aes(x=erhebja, y=coverage*100, colour = weight)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "CBA coverage (%)") +
  theme_minimal() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.37),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"))
ggsave("output/data/coverage_MEM_weights.pdf")

##broad coverage
ggplot(data = coverage_broad, 
       aes(x=erhebja, y=coverage*100, colour = weight)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "CBA coverage (%)") +
  theme_minimal() +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95, 0.25),
        legend.justification.inside = c(0.9, 0.9),
        legend.background = element_rect(colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.3, "cm"))
ggsave("output/data/coverage_MEM_weights_broad.pdf")

