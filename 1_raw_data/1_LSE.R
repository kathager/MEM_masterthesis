## ---------------------------
##
## Master thesis
## LSE-Data preparation
## read in years 2008 - 2010 and append
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

search()
getwd()
## ---------------------------
###LSE Data
##get data into data frame

###2008 -> base case
data <- read.csv("raw_data/LSE2008_210314_pseudo.csv", header = TRUE)
col_names_08 <- unlist(strsplit(names(data), split = "\\."))
col_names_08 <- lapply(col_names_08, tolower) #all column names to lower case
rows <- strsplit(data[[1]], split = ";") #vector, each entry is a row. Split each vector entry at semi-column
rows_df_08 <- data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(rows_df_08) <- col_names_08

nrow(rows_df_08) #number of obs: 1743764
ncol(rows_df_08) #number of vars: 45

# Initialize combined dataframe
all_data <- rows_df_08

## loop through other years
#different years have different variables -> get a list of all unique var names

###column names
col_names_list <- list()
col_names_list[["08"]] <- col_names_08  # Add the 2008 data

for (i in seq(10, 18, by = 2)) {
  year_str <- as.character(i)
  print(paste("Year: 20", year_str, sep = ""))
  
  filename <- paste("raw_data/LSE20", year_str, "_210314_pseudo.csv", sep = "")
  data <- read.csv(filename, header = TRUE)
  
  col_names <- unlist(strsplit(names(data), split = "\\."))
  col_names_list[[year_str]] <- col_names  #store in list
}
#View(col_names_list)
col_names_list <- lapply(col_names_list, tolower)
all_columns <- unique(unlist(col_names_list))
print(paste("Total unique columns:", length(all_columns)))

###append datasets and fill in NAs if a column is missing

#years 2010 to 2018, in steps of two years (LSE is bi-yearly panel)
for (i in seq(10, 18, by = 2)) {
  year_str <- as.character(i)
  print(paste("Year: 20", year_str, sep = ""))
  
  filename <- paste("raw_data/LSE20", year_str, "_210314_pseudo.csv", sep = "")
  data <- read.csv(filename, header = TRUE)
  
  col_names <- unlist(strsplit(names(data), split = "\\."))
  col_names <- lapply(col_names, tolower)

  rows <- strsplit(data[[1]], split = ";") #vector, each entry is a row. Split each vector entry at semi-column
  rows_df <- data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  
  colnames(rows_df) <- col_names
  
  # Append to combined dataframe
  #bind_rows (dplyr) automatically handles mismatched columns by filling with NAs
  all_data <- bind_rows(all_data, rows_df) 
}


#save working file LSE
saveRDS(all_data, file = "output/data/1_LSE_08_18.rds")

