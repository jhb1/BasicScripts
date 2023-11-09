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
# mutate(area = str_replace(area, "Swaziland", "Eswatini"))
`%out%` <- function(a,b) ! a %in% b


# input/output paths ----
path.basic <- str_c("C:/Users",
                    str_sub(getwd(), str_locate(getwd(), "C:/Users/")[2], str_locate(getwd(), "/D")[1]),
                    "Dropbox")
# path.basic.sp <- "C:/Users/jbeise/UNICEF/DA-Mortality and Demographics - Migration and Displacement - Documents/Migration and Displacement"
path.basic.sp <- "C:/Users/jbeise/OneDrive - UNICEF (1)/Migration and Displacement"

# dropbox ----
input <-  file.path(path.basic.sp, "Data")
#  input <- file.path(path.basic, "Data")

output <- file.path(path.basic, "FOLDERNAME/data/output")
codes <- file.path(path.basic, "FOLDERNAME/data/codes")

load(file.path(input,"IDMC/IDMC2023/idmc_2022.Rdata"))  
# load(file.path(input,"UNPD/WPP2022/wpp_5y_age_group.rdata"))
# load(file.path(input,"UNPD/WPP2022/wpp_age.Rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_dem.rdata"))
# load(file.path(input,"UNPD/WPP2022/wpp_fertility.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_location.rdata"))
# load(file.path(input,"UNPD/WPP2022/wpp_unicef.rdata"))

# isocodes ----
wpp.location %>% 
  select(area, iso3) %>%
  filter(area == "Togo")


# share sex in population -----
wpp.pop.sex.pct <- wpp.age.group %>% 
  select(area, area.id, region, subregion, iso3, year, poptotal.thsd, pop0to17.thsd) %>% 
  inner_join(wpp.age.group.female %>% 
               select(area.id, year, poptotal.female.thsd = poptotal.thsd, pop0to17.female.thsd = pop0to17.thsd),
             by = c ("area.id", "year")) %>% 
  inner_join(wpp.age.group.male %>% 
               select(area.id, year, poptotal.male.thsd = poptotal.thsd, pop0to17.male.thsd = pop0to17.thsd),
             by = c ("area.id", "year")) %>% 
  mutate(poptotal.female.pct = poptotal.female.thsd/poptotal.thsd * 100) %>% 
  mutate(pop0to17.female.pct = pop0to17.female.thsd/pop0to17.thsd * 100) %>% 
  mutate(poptotal.male.pct = poptotal.male.thsd/poptotal.thsd * 100) %>% 
  mutate(pop0to17.male.pct = pop0to17.male.thsd/pop0to17.thsd * 100)

# Share of 0to17 among female population
wpp.age.group.pct.female %>% 
  filter(area %in% c( "Southern Asia", "India", "Afghanistan") & year == 2022) %>% 
  select(area:year, year, poptotal.pct, pop0to17.pct)
  
# 7) Number weather-related displacements by year -----
# weather-related
idmc.disaster.2022 %>% 
  select(year, hazard.cat, idp.dis.new, idp.dis.new.0to17) %>% 
  filter(hazard.cat == "Weather related") %>% 
  group_by(year) %>% 
  summarize(idp.dis.new = sum(idp.dis.new, na.rm = T),
            idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T)
  ) %>% 
  arrange(desc(year))

# comparing last 5 year with previous 5 year
idmc.disaster.2022 %>% 
  select(year, hazard.cat, idp.dis.new, idp.dis.new.0to17) %>% 
  filter(hazard.cat == "Weather related") %>% 
  filter(year >= 2013) %>% 
  mutate(period = ifelse(year >= 2018, "2018_22", "2013-17")) %>% 
  group_by(period) %>% 
  summarize(idp.dis.new = sum(idp.dis.new, na.rm = T),
            idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T)
  ) 

# last 10 years
# comparing last 5 year with previous 5 year
idmc.disaster.2022 %>% 
  select(year, hazard.cat, idp.dis.new, idp.dis.new.0to17) %>% 
  filter(hazard.cat == "Weather related") %>% 
  filter(year >= 2013) %>% 
  mutate(period = "2013-2022") %>% 
  group_by(period) %>% 
  summarize(idp.dis.new = sum(idp.dis.new, na.rm = T),
            idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T)
  )


# by haz.cat and haz.type
idmc.disaster.2022 %>% 
  select(year, hazard.cat, hazard.type, idp.dis.new, idp.dis.new.0to17) %>% 
  group_by(year, hazard.cat, hazard.type) %>% 
  summarize(idp.dis.new = sum(idp.dis.new, na.rm = T),
            idp.dis.new.0to17 = sum(idp.dis.new.0to17, na.rm = T)
            ) %>% 
  arrange(desc(year), hazard.cat, desc(idp.dis.new)) %>% 
  group_by(year, hazard.cat) %>% 
  mutate(pct.within.cat = idp.dis.new / sum(idp.dis.new) * 100) %>% 
  print(n = 20) 


# 6) Sanya, G20 meeting, South Asia, Jul 2023 -----
g20 <- idmc.disaster.2022 %>% 
  select(iso3, year, idp.dis.new, idp.dis.new.0to17 ,hazard.cat ,hazard.type) %>% 
  filter(hazard.cat == "Weather related") %>% 
  filter(year >= 2016) %>% 
  left_join(wpp.location %>% 
              select(area, iso3, subregion), by = c("iso3")) %>% 
  filter(year >= 2018) %>% 
  filter(subregion == "Southern Asia") %>% 
  group_by(subregion, area, iso3, year, hazard.cat, hazard.type) %>% 
  summarise(nd = sum(idp.dis.new, na.rm = T),
            nd.0to17 = sum(idp.dis.new.0to17, na.rm = T)) %>% 
  ungroup()

# number female displaced in SAsia by weather-related event by year since 2018, by year
g20.1 <- g20 %>% 
  left_join(wpp.pop.sex.pct %>%
              select(iso3, year, poptotal.female.pct, pop0to17.female.pct), by = c("iso3", "year")) %>% 
  mutate(nd.female.total = nd * poptotal.female.pct / 100) %>% 
  mutate(nd.female.0to17 = nd.0to17 * pop0to17.female.pct / 100) %>% 
  select(subregion, year, hazard.cat, starts_with("nd")) %>% 
  group_by(subregion, year, hazard.cat) %>% 
  summarise(nd = sum(nd, na.rm = T),
            nd.female = sum(nd.female.total, na.rm = T),
            nd.female.0to17 = sum(nd.female.0to17, na.rm = T)
            )

# number female displaced in SAsia by weather-related event by year since 2018, accumulated
g20.1 %>% 
  group_by(subregion, hazard.cat) %>% 
  summarise(nd = sum(nd, na.rm = T),
            nd.female = sum(nd.female, na.rm = T),
            nd.female.0to17 = sum(nd.female.0to17, na.rm = T)) %>% 
  select(-ends_with("pct"))


# number female displaced in PAK in 2022 by weather-related event
g20 %>% 
  filter(iso3 == "PAK" & year == 2022) %>% 
  left_join(wpp.pop.sex.pct %>%
              select(iso3, year, poptotal.female.pct, pop0to17.female.pct), by = c("iso3", "year")) %>% 
  mutate(nd.female.total = nd * poptotal.female.pct / 100) %>% 
  mutate(nd.female.0to17 = nd * pop0to17.female.pct / 100) %>% 
  select(-ends_with("pct"))

# SAsia countries 
unique(g20$area)
  
# Share of India's girls and adolescents 
wpp.age.group.female %>% 
  filter(area.id == 900 | area == "India") %>% 
  filter(year %in% c(2010, 2022, 2023, 2030)) %>% 
  select(area, year, pop0to19.thsd) %>% 
  pivot_wider(names_from = area, values_from = pop0to19.thsd) %>% 
  mutate(india.pct = India/WORLD * 100) %>% 
  mutate(india.pct = WORLD/India)



.1# 5) Disaster Displacement by hazard category and type -----
# share weather-related 
idmc.disaster.2022 %>% 
  select(iso3, year, idp.dis.new, hazard.cat ,hazard.type) %>% 
  filter(hazard.cat == "Weather related") %>% 
  filter(year >= 2016) %>% 
  # group_by(year, hazard.cat, hazard.type) %>% 
  group_by(hazard.cat, hazard.type) %>% 
  summarise(nd = sum(idp.dis.new, na.rm = T)) %>% 
  group_by(hazard.cat) %>% 
  mutate(nd.weather = sum(nd)) %>% 
  mutate(haz.cat.report = ifelse(str_length(hazard.type) < 10, 1,0)) %>% 
  group_by(hazard.cat , nd.weather, haz.cat.report) %>% 
  summarise(nd = sum(nd, na.rm = T)) %>% 
  mutate(share.pct = nd / nd.weather * 100)

# hazard categories and types and pct displacements
unique(idmc.disaster.2022$hazard.cat)

idmc.disaster.2022 %>% 
  select(year, hazard.cat, hazard.type, idp.dis.new) %>% 
  group_by(year, hazard.cat, hazard.type) %>% 
  summarize(idp.dis.new = sum(idp.dis.new, na.rm = T)) %>% 
  arrange(desc(year), hazard.cat, desc(idp.dis.new)) %>% 
  group_by(year, hazard.cat) %>% 
  mutate(pct.within.cat = idp.dis.new / sum(idp.dis.new) * 100) %>% 
  print(n = 20) 


# 4) IRQ ----
idmc.conf.dis.2022 %>% 
  filter(year == 2022 & iso3 == "IRQ") %>% 
  select(iso3:year,idp.conf.stock,  idp.conf.stock.0to17, idp.conf.new.0to17)

# 3) PR WRD 06/2023 ----
idmc.conf.dis.2022 %>% 
  filter(year == 2022) %>% 
  select(iso3:year, idp.dis.stock.0to17, idp.dis.new.0to17) %>% 
  mutate(idp.dis.stock.0to17.global = sum(idp.dis.stock.0to17, na.rm = T)) %>% 
  mutate(idp.dis.new.0to17.global = sum(idp.dis.new.0to17, na.rm = T)) 

# by year and haz.cat
idmc.disaster.2022 %>% 
  # filter(year == 2022) %>% 
  select(iso3:year, hazard.cat, hazard.type, idp.dis.new, idp.dis.new.0to17) %>% 
  group_by(year, hazard.cat) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% 
  arrange(desc(year), desc(idp.dis.new))

# by year and haz.cat and haz.type
idmc.disaster.2022 %>% 
  # filter(year == 2022) %>% 
  select(iso3:year, hazard.cat, hazard.type, idp.dis.new, idp.dis.new.0to17) %>% 
  group_by(year, hazard.cat, hazard.type) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% 
  arrange(desc(year), desc(hazard.cat), desc(idp.dis.new))


idmc.disaster.2022 %>% 
 filter(year == 2022) %>% 
 filter(hazard.cat == "Weather related") %>% 
 select(iso3:year, hazard.cat, hazard.type, idp.dis.new,pop0to17.pct, idp.dis.new.0to17) %>% 
 mutate(idp.dis.new.0to17.global = sum(idp.dis.new.0to17, na.rm = T)) 

idmc.disaster.2022 %>% 
  filter(year == 2022) %>% 
  filter(hazard.cat != "Weather related") %>% 
  select(iso3:year, hazard.cat, hazard.type, idp.dis.new,pop0to17.pct, idp.dis.new.0to17) %>% 
  mutate(idp.dis.new.0to17.global = sum(idp.dis.new.0to17, na.rm = T)) 

25.8 / 43.3

  

# 2) UKR ----
idmc.conf.dis.2022 %>% 
  filter(iso3 == "UKR") %>% 
  filter(year >= 2020) %>% 
  select(iso3:year, idp.conf.stock, idp.conf.stock.0to17, idp.conf.new, idp.conf.new.0to17)

# 1) GDA and Tess on Mexico ----
idmc.conf.dis.2022 %>% 
  filter(iso3 == "MEX") %>% 
  filter(year >= 2012) %>% 
  select(iso3:year, idp.conf.stock, idp.conf.stock.0to17, idp.conf.new, idp.conf.new.0to17) %>% 
  mutate(x = sum(idp.conf.new.0to17))

idmc.conf.dis.2022 %>% 
  filter(iso3 == "MEX") %>% 
  filter(year >= 2021) %>% 
  select(iso3:year, idp.conf.stock, idp.conf.stock.0to17, idp.conf.new, idp.conf.new.0to17) %>% 
  mutate(nd.all = sum(idp.conf.new)) %>% 
  mutate(nd.18 = sum(idp.conf.new.0to17))


