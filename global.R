library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)

#setwd("/Users/nataliapiedrahita/Food and Agriculture Organization/sdgcheckR-master 2/trial")

source("R/cleanInputFileCols.R")

template <- read_xlsx("the_template.xlsx", sheet= 1)

template <- cleanInputFileCols(template)


#Note to self, if won't run on shiny server, open in regular notepad, save as "UTF-8"
geo <- read_csv("lookup_geoAreas.csv")
geo$Name <- trimws(geo$Name)
geo$GeoAreaName <- trimws(geo$GeoAreaName)


#these are the currently accepted options of runits, and nature. Edit look-up table as more are added
units <- read.csv("lookup_units.csv",
                  stringsAsFactors = F,
                  header = T)

nature <- read.csv("lookup_nature.csv",
                   stringsAsFactors = F,
                   header = T)


#x <- readxl::read_xlsx("/Users/michaelrahija/Dropbox/ad_hoc_R/sdgcheckr/real_data/SDG 2.5.2 2020.xlsx")

regions <-read_csv("regional_groupings.csv")



