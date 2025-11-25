## ---------------------------
## Master thesis
## Wage developments/distributions/compliance/regional in MEM sector

#chapter "Wages" in descriptive part

## Author: Katja Hager
## Date Created: July 2025
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
library(stargazer)
theme_set(theme_minimal())

library(DescTools)
library(bfsMaps)
options(bfsMaps.base="raw_data/BFS_maps_2022")

## ---------------------------

#load merged and filtered file with created variables
LSE_08_18 <- readRDS(file = "output/data/2c_variables.rds") %>%
  filter(mem==1)

##--------------------------------------------
levels(LSE_08_18$min_wage_worker)
data_MW <- LSE_08_18 %>%
  filter(min_wage_worker=="Minimum wage worker")

#for wage development
MEM_overall <- LSE_08_18 %>%
  filter(!is.na(real_wage) & !is.na(gewicht) & gewicht > 0) %>% ungroup() %>%
  select(erhebja, gross_wage_std, real_wage, gewibgrs, gewicht) %>%
  pivot_longer(cols = c(gross_wage_std, real_wage), names_to = "wage_type", values_to = "wage") %>%
  pivot_longer(cols = c(gewibgrs, gewicht), names_to = "weight_type", values_to = "weight") %>%
  group_by(erhebja, wage_type, weight_type) %>%
  summarise(
    counts_n = sum(!is.na(wage)),
    p10 = wtd.quantile(wage, weights = weight, probs = 0.1, na.rm = TRUE),
    p50 = wtd.quantile(wage, weights = weight, probs = 0.5, na.rm = TRUE),
    p90 = wtd.quantile(wage, weights = weight, probs = 0.9, na.rm = TRUE),
    .groups = "drop" ) %>%
  mutate(wage_type = ifelse(wage_type == "gross_wage_std", "nominal", "real"))

##gender
MEM_overall_gender <- LSE_08_18 %>%
  filter(!is.na(gross_wage_std) & !is.na(gewicht) & gewicht > 0) %>% ungroup() %>%
  select(erhebja, gross_wage_std, real_wage, gewibgrs, gewicht, geschle) %>%
  pivot_longer(cols = c(gross_wage_std, real_wage), names_to = "wage_type", values_to = "wage") %>%
  pivot_longer(cols = c(gewibgrs, gewicht), names_to = "weight_type", values_to = "weight") %>%
  group_by(erhebja, wage_type, weight_type, geschle) %>%
  summarise(
    counts_n = sum(!is.na(wage)),
    p10 = wtd.quantile(wage, weights = weight, probs = 0.1, na.rm = TRUE),
    p50 = wtd.quantile(wage, weights = weight, probs = 0.5, na.rm = TRUE),
    p90 = wtd.quantile(wage, weights = weight, probs = 0.9, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(wage_type = ifelse(wage_type == "gross_wage_std", "nominal", "real"))

##nationality
MEM_overall_nationality <- LSE_08_18 %>%
  filter(!is.na(gross_wage_std) & !is.na(gewicht) & gewicht > 0) %>%ungroup() %>%
  select(erhebja, gross_wage_std, real_wage, gewibgrs, gewicht, natkat2) %>%
  pivot_longer(cols = c(gross_wage_std, real_wage), names_to = "wage_type", values_to = "wage") %>%
  pivot_longer(cols = c(gewibgrs, gewicht), names_to = "weight_type", values_to = "weight") %>%
  group_by(erhebja, wage_type, weight_type, natkat2) %>%
  summarise(
    counts_n = sum(!is.na(wage)),
    p10 = wtd.quantile(wage, weights = weight, probs = 0.1, na.rm = TRUE),
    p50 = wtd.quantile(wage, weights = weight, probs = 0.5, na.rm = TRUE),
    p90 = wtd.quantile(wage, weights = weight, probs = 0.9, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(wage_type = ifelse(wage_type == "gross_wage_std", "nominal", "real"))

##CBA-coverage
MEM_CBA <- LSE_08_18 %>%
  filter(!is.na(gross_wage_std) & !is.na(gewicht) & gewicht > 0 & !is.na(cba)) %>% ungroup() %>%
  select(erhebja, gross_wage_std, real_wage, gewibgrs, gewicht, cba) %>%
  pivot_longer(cols = c(gross_wage_std, real_wage), names_to = "wage_type", values_to = "wage") %>%
  pivot_longer(cols = c(gewibgrs, gewicht), names_to = "weight_type", values_to = "weight") %>%
  group_by(erhebja, wage_type, weight_type, cba) %>%
  summarise(
    counts_n = sum(!is.na(wage)),
    p10 = wtd.quantile(wage, weights = weight, probs = 0.1, na.rm = TRUE),
    p50 = wtd.quantile(wage, weights = weight, probs = 0.5, na.rm = TRUE),
    p90 = wtd.quantile(wage, weights = weight, probs = 0.9, na.rm = TRUE),
    mean = wtd.mean(wage, weights = weight),
    .groups = "drop") %>%
  mutate(wage_type = ifelse(wage_type == "gross_wage_std", "nominal", "real"))

#by wage region 
MEM_overall_MWregion <- LSE_08_18 %>%
  filter(!is.na(gross_wage_std) & !is.na(gewicht) & gewicht > 0) %>% ungroup() %>%
  select(erhebja, gross_wage_std, real_wage, gewibgrs, gewicht, wage_region) %>%
  pivot_longer(cols = c(gross_wage_std, real_wage), names_to = "wage_type", values_to = "wage") %>%
  pivot_longer(cols = c(gewibgrs, gewicht), names_to = "weight_type", values_to = "weight") %>%
  group_by(erhebja, wage_type, weight_type, wage_region) %>%
  summarise(
    counts_n = sum(!is.na(wage)),
    p10 = wtd.quantile(wage, weights = weight, probs = 0.1, na.rm = TRUE),
    p50 = wtd.quantile(wage, weights = weight, probs = 0.5, na.rm = TRUE),
    p90 = wtd.quantile(wage, weights = weight, probs = 0.9, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(wage_type = ifelse(wage_type == "gross_wage_std", "nominal", "real"))

med_wage_2012 <- wtd.quantile(LSE_08_18$real_wage[LSE_08_18$erhebja==2012], 
                              weights = LSE_08_18$gewicht[LSE_08_18$erhebja==2012], 
                              probs = 0.5)

two_colours <- function() {
  list(
    scale_colour_manual(values = scales::viridis_pal(option = "D")(2)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(2)) ) }

three_colours <- function() {
  list(
    scale_colour_manual(values = scales::viridis_pal(option = "D")(3)),
    scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) ) }

legend <- function() {
  list(
    theme(legend.position = "inside",
          legend.background = element_rect(colour = "white"),
          strip.text = element_text(face = "bold"),
          legend.text=element_text(size=10),
          legend.key.height = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm")) ) }

plot_density <- function() {
  list(
    two_colours(),
    scale_y_continuous(labels = scales::percent),
    geom_density(linewidth = .8, adjust = 1.1, alpha = 0.5),
    legend())}

plot_evolution <- function() {
  list(
    labs(x = NULL, y = "CHF", colour = NULL),
    scale_y_continuous(labels = scales::comma),
    scale_x_continuous(breaks = c(2008, 2012, 2016)),
    legend()  ) }

plot_default_hist <- function() {
  list(
    two_colours(),
    geom_histogram(bins = 45, alpha = 0.5, position = "identity", linewidth = .8),
    scale_x_continuous(breaks = c(-2000, 0, 2000, 4000), labels = scales::comma),
    scale_y_continuous(labels = scales::comma),
    geom_vline(aes(xintercept = 0), color = "darkgrey", linewidth = 0.7, linetype = "dashed"),
    legend()  )}

## ---------------------------
#union wage premium
wage_premia <- MEM_CBA %>%
  filter(wage_type=="real" & weight_type == "gewicht") %>%
  group_by(erhebja) %>%
  mutate(wage_premium = (mean[cba == "CBA covered"]-mean[cba == "Not covered"] )/mean[cba == "CBA covered"],
         wage_premium_10 = (p10[cba == "CBA covered"]-p10[cba == "Not covered"] )/p10[cba == "CBA covered"],
         wage_premium_50 = (p50[cba == "CBA covered"]-p50[cba == "Not covered"] )/p50[cba == "CBA covered"])

## ---------------------------
#wage distribution (density)

#separated by CBA coverage
ggplot(LSE_08_18[LSE_08_18$erhebja==2012 & LSE_08_18$real_wage <= 16000 &
                   !is.na(LSE_08_18$cba), ], 
       aes(x = real_wage, weight = gewicht, colour = cba, fill = cba)) +
  labs(x = "Gross monthly wage (CHF)",
       y = "Density", colour = NULL, fill = NULL) +
  theme(legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1)) +
  plot_density() +
  scale_x_continuous(breaks = c(2500, med_wage_2012, 9500, 13500), labels = scales::comma)
ggsave("output/graph/wage_distribution_CBA_2012.pdf", width = 10, height = 6, unit = "cm")

ggplot(data_MW[data_MW$erhebja==2012 & data_MW$real_wage <= 16000 &
                 !is.na(data_MW$cba), ], 
       aes(x = real_wage, weight = gewicht, colour = cba, fill = cba)) +
  labs(x = "Gross monthly wage (CHF)",
       y = "Density", colour = NULL, fill = NULL) +
  scale_x_continuous(breaks = c(1700, 2500, 3300, 4000), labels = scales::comma) +
  plot_density() +
  theme(legend.position = "none")
ggsave("output/graph/wage_distribution_CBA_MW_2012.pdf", width = 10, height = 6, unit = "cm")

## ---------------------------
##median wages
med_wages <- LSE_08_18 %>%
  group_by(erhebja, msreg_an) %>%
  summarise(med_wage = wtd.quantile(real_wage, weights = gewicht, 
                                    probs = 0.5, na.rm = TRUE))

# 1) start with a cantons map
#graphics.off()
pdf("median_wage_map_2012.pdf", width = 10, height = 8)
kant.map <- GetMap("kant.map")
par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=TRUE)
b <- PlotKant(1:26, col= "white", 
              border="grey40")

#2) Add MS regions 
share_vec <- med_wages$med_wage[med_wages$erhebja==2012&!is.na(med_wages$msreg_an)]
share_id <- med_wages$msreg_an[med_wages$erhebja==2012&!is.na(med_wages$msreg_an)]

# Create 5 colors
cols <- colorRampPalette(colors = c("white", "#44015490"))(9)
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
            cols=cols, cex=0.7, inset=c(0, 0), 
            title = "Median wage (CHF)")

dev.off()

## ---------------------------
## trend in wages -> wage percentiles

##overall MEM sector AND CBA-covered / non-covered
#differentiate by gender: sex in (c("1", "2")),  #1=men, 2=women
#differentiate by nationality: LSE_08_18$natkat
#p10, p50, p90; counts_n; gewicht, gewibgrs; real_wage and gross_wage_std

#Figure 1: Evolution of lowest and median wages (real/nominal, CBA coverage)
#differentiate by real vs. nominal
ggplot(data = MEM_overall[MEM_overall$weight_type == "gewicht", ], 
       aes(x=erhebja, colour = wage_type)) +
  geom_line(aes(y = p10),linewidth = .8) +  geom_point(aes(y = p10)) +
  geom_line(aes(y = p50),linewidth = .8) +  geom_point(aes(y = p50)) +
  annotate("text", x = 2016, y = 4420, label = "P10", size = 4) +
  annotate("text", x = 2016, y = 6080, label = "P50", size = 4) +
  plot_evolution() +
  theme(legend.position.inside = c(0.4, 0.5),
        legend.justification.inside = c(0.4, 0.5))+
  three_colours()
ggsave("output/graph/wage_percentiles_overall_real_nominal.pdf", width = 10, height = 5, unit = "cm")

#differentiate by CBA coverage
ggplot(data = MEM_CBA[MEM_CBA$wage_type == "real" & MEM_CBA$weight_type == "gewicht", ], 
       aes(x=erhebja, colour = cba)) +
  geom_line(aes(y = p10), linewidth = .8) +  geom_point(aes(y = p10)) +
  geom_line(aes(y = p50), linewidth = .8) +  geom_point(aes(y = p50)) +
  annotate("text", x = 2016, y = 4550, label = "P10", size = 4) +
  annotate("text", x = 2016, y = 6200, label = "P50", size = 4) +
  labs(x = NULL, y = "CHF", colour = NULL) +
  plot_evolution() +  two_colours() +
  theme(legend.position.inside = c(0.4, 0.5),
        legend.justification.inside = c(0.4, 0.5))
ggsave("output/graph/wage_percentiles_cba_coverage.pdf", width = 10, height = 5, unit = "cm")

## ---------------------------
#bite of minimum wage -> dist_to_MW

ggplot(LSE_08_18[LSE_08_18$erhebja%in% c(2012) & LSE_08_18$real_wage <= 9000 
                 & !is.na(LSE_08_18$cba), ], 
       aes(x = dist_to_MW, weight = gewicht, fill = cba, color = cba)) +
  labs(x = "Distance to minimum wage (CHF)", y = "Number of workers",
       color = NULL, fill = NULL) +
  legend() + plot_default_hist() +
  theme(legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.text=element_text(size=10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"))
ggsave("output/graph/dist_to_MW_2012_hist_cba.pdf", width = 10, height = 6, unit = "cm")

ggplot(LSE_08_18[LSE_08_18$erhebja%in% c(2012) & LSE_08_18$real_wage <= 9000 
                 & !is.na(LSE_08_18$cba), ], 
       aes(x = dist_to_MW, weight = gewicht, fill = cba, color = cba)) +
  labs(x = "Distance to minimum wage (CHF)", y = "Density",
       color = NULL, fill = NULL) +
  scale_x_continuous(breaks = c(-2000, 0, 2000, 4000), labels = scales::comma) +
  plot_density() + legend() +
  geom_vline(aes(xintercept = 0), color = "darkgrey", linewidth = 0.7, linetype = "dashed") +
  theme(legend.position = "none")
ggsave("output/graph/dist_to_MW_2012_density_cba.pdf", width = 10, height = 6, unit = "cm")

## ---------------------------
#compliance -> dist_to_MW in 2018

ggplot(LSE_08_18[LSE_08_18$erhebja%in% c(2018) & LSE_08_18$real_wage <= 9000 
                 & !is.na(LSE_08_18$cba), ], 
       aes(x = dist_to_MW, weight = gewicht, fill = cba, color = cba)) +
  labs(x = "Distance to minimum wage (CHF)", y = "Number of workers",
       color = NULL, fill = NULL) +
  legend() + plot_default_hist() +
  theme(legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1),
        legend.text=element_text(size=10),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"))
ggsave("output/graph/dist_to_MW_2018_hist_cba.pdf", width = 10, height = 6, unit = "cm")

ggplot(LSE_08_18[LSE_08_18$erhebja%in% c(2018) & LSE_08_18$real_wage <= 9000 
                 & !is.na(LSE_08_18$cba), ], 
       aes(x = dist_to_MW, weight = gewicht, fill = cba, color = cba)) +
  labs(x = "Distance to minimum wage (CHF)", y = "Density",
       color = NULL, fill = NULL) +
  scale_x_continuous(breaks = c(-2000, 0, 2000, 4000), labels = scales::comma) +
  plot_density() + legend() +
  geom_vline(aes(xintercept = 0), color = "darkgrey", linewidth = 0.7, linetype = "dashed") +
  theme(legend.position = "none")
ggsave("output/graph/dist_to_MW_2018_density_cba.pdf", width = 10, height = 6, unit = "cm")

#in numbers -> MW-worker = 1.05 * MW
MW_numbers <- LSE_08_18 %>%
  filter(erhebja %in% c(2012, 2018) & !is.na(cba)) %>%
  group_by(cba, erhebja) %>%
  summarise(
    obs = sum(gross_wage_std<=min_wage, na.rm = TRUE),
    count = sum(gewicht[gross_wage_std<=min_wage], na.rm = TRUE),
    density = 100*sum(gewicht[gross_wage_std<=min_wage], na.rm = TRUE)/
      sum(gewicht, na.rm = TRUE)   )
## ---------------------------
##by wage region A, B, C
#overall wage developments (by wage region A, B, C)
ggplot(data = MEM_overall_MWregion[MEM_overall_MWregion$wage_type == "real" & 
                                     MEM_overall_MWregion$weight_type == "gewicht" & 
                                     !is.na(MEM_overall_MWregion$wage_region), ], 
       aes(x=erhebja)) +
  geom_line(aes(y = p10)) +  geom_point(aes(y = p10)) +
  geom_line(aes(y = p50)) +  geom_point(aes(y = p50)) +
  #annotate("text", x = 2016, y = 4500, label = "P10", size = 3) +
  #annotate("text", x = 2016, y = 6100, label = "P50", size = 3) +
  labs(x = NULL, y = "P10 & P50 (CHF)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2008, 2012, 2016)) +
  facet_grid(~ wage_region, scales = "free_y")
ggsave("output/graph/wage_percentiles_cba_MWregion.pdf", width = 10, height = 6, unit = "cm")

ggplot(LSE_08_18[LSE_08_18$erhebja%in% c(2012) & LSE_08_18$real_wage <= 16000 
                 & !is.na(LSE_08_18$wage_region), ], 
       aes(x = dist_to_MW, weight = gewicht, fill = wage_region, color = wage_region)) +
  labs(x = "Distance to minimum wage (CHF)", y = "Density",
       color = NULL, fill = NULL) +
  scale_x_continuous(breaks = c(0, 4500, 9000, 14000), labels = scales::comma) +
  plot_density() +
  geom_vline(aes(xintercept = 0), color = "darkgrey", linewidth = 0.7, linetype = "dashed") +
  legend() + three_colours() +
  theme(legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1))
ggsave("output/graph/dist_to_MW_2012_density_MWregion.pdf", width = 10, height = 6, unit = "cm")

## ---------------------------
#Regional Bite of minimum wage: KAITZ and gap measure

regional <- LSE_08_18 %>%
  filter(erhebja %in% c(2012))%>%
  group_by(msreg_an) %>%
  summarise(
    med_wage = wtd.quantile(real_wage, weights = gewicht, probs = 0.5, na.rm = TRUE),
    #what is the assigned minimum wage to the workforce (since there are several)
    min_wage = weighted.mean(min_wage, weights = gewicht, na.rm = TRUE),
    Kaitz_index = min_wage/med_wage*100, #the lower, the more exposed
    wages_below = sum(dist_to_MW[dist_to_MW <=0] * gewicht[dist_to_MW <=0 ]),
    wage_bill = sum(real_wage * gewicht),
    gap = abs(wages_below/wage_bill)*100, #the higher, the more exposed
    .groups = "drop") 


# 1) start with a cantons map
#graphics.off()
pdf("output/graph/kaitz_index_map_2012.pdf", width = 10, height = 8)
kant.map <- GetMap("kant.map")
par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=TRUE)
b <- PlotKant(1:26, col= "white", 
              border="grey40")

#2) Add MS regions 
share_vec <- regional$Kaitz_index[!is.na(regional$msreg_an)]
share_id <- regional$msreg_an[!is.na(regional$msreg_an)]
# Create 5 colors
cols <- colorRampPalette(colors = c("#44015490", "white"))(15)
PlotMSRe(share_id,  
         col = FindColor(share_vec, 
                         cols = cols, 
                         min.x = min(share_vec, na.rm = TRUE), 
                         max.x = max(share_vec, na.rm = TRUE)), 
         border = "grey60",
         main = "KAITZ index",
         add=TRUE)

PlotKant(1:26, add=TRUE, border="grey30", lwd=1)
AddLakes(1:2, col=rgb(235, 247, 253, max=255), border=rgb(0,166,235, max=255))
dev.off()

# 1) start with a cantons map
#graphics.off()
pdf("output/graph/gap_exposure_map_2012.pdf", width = 10, height = 8)
kant.map <- GetMap("kant.map")
par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=TRUE)
b <- PlotKant(1:26, col= "white", 
              border="grey40")

#2) Add MS regions 
share_vec <- regional$gap[!is.na(regional$msreg_an)]
share_id <- regional$msreg_an[!is.na(regional$msreg_an)]

# Create 5 colors
cols <- colorRampPalette(colors = c("white", "#44015490"))(15)
PlotMSRe(share_id,  
         col = FindColor(share_vec, 
                         cols = cols, 
                         min.x = min(share_vec, na.rm = TRUE), 
                         max.x = max(share_vec, na.rm = TRUE)), 
         border = "grey60", main = "Gap exposure", add=TRUE)

PlotKant(1:26, add=TRUE, border="grey30", lwd=1)
AddLakes(1:2, col=rgb(235, 247, 253, max=255), border=rgb(0,166,235, max=255))

dev.off()

## ---------------------------

##gender differences
ggplot(data = MEM_overall_gender[MEM_overall_gender$wage_type == "nominal" & MEM_overall_gender$weight_type == "gewicht", ], 
       aes(x = erhebja)) + 
  geom_line(mapping = aes(y = p10)) +
  geom_line(mapping = aes(y = p50)) +
  geom_point(mapping = aes(y = p10)) +
  geom_point(mapping = aes(y = p50)) +
  labs(x = NULL, y = "Wage percentiles (p10 & p50)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2008, 2012, 2016)) +
  facet_grid(~geschle) +
  scale_fill_manual(values = scales::viridis_pal(option = "D")(3)) +
  scale_colour_manual(values = scales::viridis_pal(option = "D")(3))
ggsave("output/graph/wage_percentiles_gender.pdf", width = 10, height = 6, unit = "cm")

#C-permit and CH together
##nationality differences
ggplot(data = MEM_overall_nationality[MEM_overall_nationality$wage_type == "real" & 
                                          MEM_overall_nationality$weight_type == "gewicht" &
                                          !is.na(MEM_overall_nationality$natkat2), ], 
       aes(x = erhebja)) + 
  geom_line(mapping = aes(y = p10)) +
  geom_point(mapping = aes(y = p10)) +
  geom_line(mapping = aes(y = p50)) +
  geom_point(mapping = aes(y = p50)) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(2008, 2012, 2016)) +
  labs(x = NULL, y = "Wage percentiles") +
  facet_grid(~natkat2)
ggsave("output/graph/wage_percentiles_nationality.pdf", width = 10, height = 6, unit = "cm")


## ---------------------------
##robustness checks
# plot wages over time. time: x-axis, wage: y-axis.
#differentiate by: region, cba/non-cba; and overall.

#weighting for wages doesn't really matter -> pretty close. for both p10 and p50 -> use gewibgrs
ggplot(data = MEM_overall[MEM_overall$wage_type == "real", ], 
       aes(x=erhebja, colour = weight_type)) +
  geom_line(aes(y = p10)) +
  geom_point(aes(y = p10)) +
  labs(x = NULL, y = "P10 (CHF)", colour = "Weight")+
  theme_minimal()
ggplot(data = MEM_overall[MEM_overall$wage_type == "real", ], 
       aes(x=erhebja, colour = weight_type)) +
  geom_line(aes(y = p50)) +
  geom_point(aes(y = p50)) +
  labs(x = NULL, y = "P50 (CHF)", colour = "Weight")+
  theme_minimal() +
  

###differentiate by CBA coverage
#high wages (p90)
ggplot(data = MEM_CBA[MEM_CBA$wage_type == "nominal" & MEM_CBA$weight_type == "gewicht", ], 
       aes(x=erhebja, y=p90, colour = cba)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "P90 (CHF)") +
  theme_minimal()







#demographics: time trends
#median age evolution
#median tenure evolution
#foreigner ratio evolution
#gender ratio evolution
#share of firm_size (small firms relative to all firms, ...)