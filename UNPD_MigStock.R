# TITLE ##############################################################
#
# FILENAME.r
#
# title_end ################################################################

rm(list=ls())


library(RColorBrewer)
library(scales)
library(stringr)
library(grid) # for function arrow() and waffle
library(gridExtra)
library(gtable)
library(knitr)
library(tidyverse)
library(waffle)
library(RColorBrewer)
library(readxl)
library(writexl)

# global -----------------------------------------------------------------------
`%out%` <- function(a,b) ! a %in% b


# input/output paths ----
path.basic <- str_c("C:/Users",
                    str_sub(getwd(), str_locate(getwd(), "C:/Users/")[2], str_locate(getwd(), "/D")[1]),
                    "Dropbox")
# path.basic.sp <- "C:/Users/jbeise/UNICEF/DA-Mortality and Demographics - Migration and Displacement - Documents/Migration and Displacement"
path.basic.sp <- "C:/Users/jbeise/OneDrive - UNICEF (1)/Migration and Displacement"


# sharepoint
input <-  file.path(path.basic.sp, "Data")
#  input <- file.path(path.basic, "Data")

output <- "C:/Users/jbeise/OneDrive - UNICEF (1)/Downloads_OneDrive/#Output"


# Notes to regions ----
# Development group: loctype.id == 5 
# SDG: loctype.id == 12
# Income groups: loctype.id == 13

load(file.path(input,"UNPD/UNMigrantStock2020/UN_MigrantStockByOriginAndDestination.Rdata"))
load(file.path(input,"UNPD/UNMigrantStock2020/UN_MigrantStock.Rdata"))
load(file.path(input,"UNPD/UNMigrantStock2020/UN_MigrantStockAge.Rdata"))
load(file.path(input,"UNPD/UNMigrantStock2020/UN_MigrantStockAge0to17.Rdata"))

load(file.path(input,"unhcr/GlobalTrends2021/unhcr2021.Rdata"))

# 01 - 9 Facts
mig.stock %>% 
  select(area:iso3) %>% 
  filter(year == 2020 & area.id <900) %>% 
  filter(sex == "both") %>% 
  arrange(desc(pop.mig)) %>% 
  left_join(mig.stock.ref %>% 
              filter(indic == "ref_pct") %>% 
              select(area.id, year, ref.pct = pop.mig),
            by = c("area.id", "year"))
  
unique(mig.stock.ref$indic)

mig.stock.0to17 %>% 
  filter(year == 2020 & area.id <900) %>% 
  filter(sex == "both") %>% 
  arrange(desc(pop.mig.0to17)) %>% 
  left_join(mig.stock.ref %>% 
              filter(indic == "ref_pct") %>% 
              select(area.id, year, ref.pct = pop.mig),
            by = c("area.id", "year"))
  
