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

# load(file.path(input,"UNPD/WPP2022/wpp_5y_age_group.rdata"))
# load(file.path(input,"UNPD/WPP2022/wpp_age.Rdata"))
# load(file.path(input,"UNPD/WPP2022/wpp_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_dem.rdata"))
# load(file.path(input,"UNPD/WPP2022/wpp_fertility.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_location.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_unicef.rdata"))

# ++++ Projects ++++ ------

# 02 - countries and areas ----
wpp.ctries <- wpp.location %>% 
  filter(loctype.id == 4) %>% 
  select(area:iso3)
write_xlsx(wpp.ctries, path = file.path(output, "wpp.ctries.2022.xlsx"))

wpp.unicef.regions <- wpp.dem.unicef %>%
  filter(year == 2023) %>% 
  select(area, area.id, poptotal.thsd) %>% 
  print(n = 44)
write_xlsx(wpp.unicef.regions, path = file.path(output, "wpp.unicef.regions.xlsx"))



# 01 - sex ratio ----
wpp.dem %>% 
  filter(year == 2022 & area.id < 900) %>% 
  select(area:year, pop.sex.ratio) %>% 
  arrange(desc(pop.sex.ratio))
