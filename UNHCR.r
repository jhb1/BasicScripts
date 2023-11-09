# TITLE ##############################################################
#
# FILENAME.r
#
# title_end ################################################################

rm(list=ls())


library(RColorBrewer)
library(scales)
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

load(file.path(input,"unhcr/GlobalTrends2022/unhcr2022.Rdata"))
load("C:/Users/jbeise/OneDrive - UNICEF (1)/Migration and Displacement/Data/UNHCR/GlobalTrends2022/unhcr2022_demographic_estimate.Rdata")



# 03 children ----
unhcr.gt.estimate.country.origin %>% 
  select(year, starts_with("origin"), starts_with("children_")) %>% 
  arrange(desc(children_mean)) %>% 
  mutate(pct.of.global = children_mean / sum(children_mean) * 100)

# 02 Ctries by asylum and origin -----
# asylum
unhcr.gt.tab12.new %>% 
  select(area.unhcr:pop.type, pop.ref.asy, pop.ref.data.available.asy, pop.ref.0to17.asy,coverage.sex.age.asy) %>% 
  mutate(pop.ref.0to17.asy.pct = pop.ref.0to17.asy / pop.ref.data.available.asy) %>% 
  filter(iso3 == "ETH")
# origin
unhcr.gt.tab13.new %>% 
  select(iso3, area.unhcr:pop.type, pop.ref.orig, pop.ref.data.available.orig, pop.ref.0to17.male.orig, pop.ref.0to17.fem.orig,coverage.sex.age.orig) %>% 
  mutate(pop.ref.0to17.orig = pop.ref.0to17.male.orig + pop.ref.0to17.fem.orig) %>% 
  select(-pop.ref.0to17.male.orig, -pop.ref.0to17.fem.orig) %>% 
  filter(iso3 == "UKR")

# 01 Age Coverage ----
# by ctries
unhcr.gt.tab12.new %>% 
  filter(iso3 != "XXX") %>%  # only countries
  select(year, area.unhcr, pop.type, coverage.sex.age.asy) %>% 
  mutate(age.cov.ge50pct = ifelse(coverage.sex.age.asy >= 0.5, 1, 0)) %>% 
  mutate(ctry.cnt = 1) %>% 
  arrange(desc(coverage.sex.age.asy)) %>% 
  print(n = 600) %>% 
  select(-area.unhcr) %>% 
  group_by(pop.type) %>% 
  summarize(ctries.cov.ge50pct.pct = mean(age.cov.ge50pct) * 100,
            ctries.cov.ge50pct.cnt = sum(age.cov.ge50pct),
            ctry.cnt = sum(ctry.cnt),
  ) %>% 
  mutate(ctries.cov.lt50pct.pct = 100-ctries.cov.ge50pct.pct) %>% 
  mutate(ctries.cov.lt50pct.cnt = ctry.cnt - ctries.cov.ge50pct.cnt)
  
# by population
unhcr.gt.tab12.new %>% 
  filter(iso3 == "XXX") %>%  
  select(year, area.unhcr, pop.type, pop.ref.asy, coverage.sex.age.asy) %>% 
  arrange(desc(pop.ref.asy))


  





