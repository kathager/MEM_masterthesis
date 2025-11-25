## ---------------------------
## Master thesis
## Author: Katja Hager
## Date Created: June 2025
#
## Inflation to get real wages
#
# BFS LIK
# https://www.bfs.admin.ch/bfs/de/home/statistiken/preise/landesindex-konsumentenpreise/
# detailresultate.assetdetail.35952432.html
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation

library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(tibbletime)

search()
getwd()
################################################################################

## load BFS file with inflation time series
inflation <- read_excel("raw_data/su-d-05.02.10.xlsx", range = "A5:N1300", 
                        col_types = c("date", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"), 
                        col_names = c("Date",	"Jun 1914=100",	"Aug 1939=100",
                                      "Sep 1966=100",	"Sep 1977=100",	"Dez 1982=100",
                                      "Mai 1993=100",	"Mai 2000=100",	"Dez 2005=100",
                                      "Dez 2010=100",	"Dez 2015=100",	"Dez 2020=100",
                                      "% m-1",	"% m-12"))

#goal: keep only October-value in each year, make "Datum / Date" year only
#october: 1922-10-01 --> xxxx-10-01

inflation <- as_tbl_time(inflation, index = Date)

inflation_oct <- inflation %>%
  filter(month(Date) == 10) %>%
  mutate(erhebja = year(Date)) %>%
  select(erhebja, "% m-12", "Dez 2010=100")

saveRDS(inflation_oct, file = "output/data/inflation_oct.Rds")
