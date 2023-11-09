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

load(file.path(input,"UNRWA/UNRWA2022/unrwa2022.Rdata"))


# 01 ----
df.unrwa %>% 
  arrange(desc(year))

df.unrwa.all.2022 %>% 
  arrange(desc(year), (age))

