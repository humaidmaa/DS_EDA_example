# Diamond Analysis
# Mansour Alhumaid
# 05.10.2020
# A small case study for EDA and stats

# load packages
library(tidyverse)
library(rio)

# Read in the data (csv format):
# Newer methods from tidyr package
jems <- read_csv("data/diamonds.csv")

# Get familiar with our data:
summary(jems)
names(jems)
glimpse(jems)

# more detail:
attributes(jems)
typeof(jems)