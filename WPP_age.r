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

# load(file.path(input,"UNPD/WPP2022/wpp_5y_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_unicef.rdata"))

load(file.path(input,"UNPD/WPP2022/wpp_5y_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_age.Rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_dem.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_fertility.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_location.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_unicef.rdata"))


# ++++ Projects ++++ ------
# >>>> 8) EAPRO request
unique(wpp.age.group.unicef$area)

wpp.age.group.unicef %>%
  filter(year == 2023) %>% 
  select(area:year, poptotal.thsd, pop0to17.thsd) %>% 
  filter(str_detect(area, "Pacific")) %>% 
  filter(!str_detect(area, "EAPRO"))

unique(wpp.age.group$area)

wpp.age.group %>%
  filter(year == 2023) %>% 
  select(area:year, poptotal.thsd, pop0to17.thsd) %>% 
  filter(str_detect(area, "Pacific")) %>% 
  filter(!str_detect(area, "EAPRO")) 


  

# >>>> 7) UN Groups: ESCAP members (including non-Asian members such as USA, FRA etc) ----
input.wpp <- file.path(input, "UNPD/WPP2022/source_data")
path.file <- file.path(input.wpp, "WPP2022_SA3_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx")
wpp.age.group.sa <-  read_excel(path = path.file, sheet = "Estimates and Medium variant", skip = 16) %>% 
  select(area = starts_with("Type of aggr"),
         area.id = starts_with("Location code"),
         year = starts_with("Year"),
         SDMX_code = "SDMX code**",
         poptotal.thsd ='Total',
         pop0to1.thsd = `0-1`,
         pop0to4.thsd = `0-4`,
         pop0to14.thsd = `0-14`,
         pop0to17.thsd = `0-17`,
         pop0to19.thsd = `0-19`,
         pop0to24.thsd = `0-24`,
         everything(),
         -c(Index, Variant, Notes, Type, starts_with("Parent")))  %>% 
  filter(str_sub(area, 1,5) == "ESCAP") %>%
  mutate_at(vars(-c(area:year)), as.numeric)

unique(wpp.age.group.sa$area)

# ESCAP total
wpp.age.group.sa %>% 
  filter(area.id == 98400) %>%
  filter(year == 2023) %>% 
  select(area:year, poptotal.thsd, pop0to17.thsd) %>% 
  mutate(pop0to17.bil = pop0to17.thsd / 1000000)

# all ESCAP groups
wpp.age.group.sa %>% 
  filter(str_detect(area, "ESCAP")) %>%
  filter(year == 2023) %>% 
  select(area:year, poptotal.thsd, pop0to17.thsd) %>% 
  mutate(pop0to17.bil = pop0to17.thsd / 1000000) %>% 
  arrange(desc(pop0to17.bil)) %>% 
  print(n=33)

wpp.age.group.sa %>% 
  filter(str_detect(area, "Asia") | str_detect(area, "PAC")) %>% 
  filter(area.id != 98400) %>%
  filter(year == 2023) %>% 
  select(area:year, poptotal.thsd, pop0to17.thsd) %>% 
  mutate(all.under18 = sum(pop0to17.thsd))

wpp.age.group %>% 
  filter(year == 2023 & area.id == 900) %>% 
  select(area:year, poptotal.thsd, pop0to17.thsd) %>% 
  mutate(share.0to17 = 1316000 / pop0to17.thsd * 100)


# >>> 6) MENARO ----
menar.ctries <- (c("MAR", "DZA", "TUN", 'LBY', "EGY", "SDN", "DJI",
                    "JOR", "LBN", "PSE", "SYR", "IRQ", "IRN", 
                   "KWT", 'BHR',"QAT", "ARE", "OMN", "YEM", "SAU"))



# 0to17
wpp.age.group.unicef %>% 
  select(starts_with("area")) %>% 
  unique(.) %>% 
print(n=22)

wpp.age.group.unicef %>% 
  filter(area.id %in% c(2037, 1590)) %>% 
  filter(year == 2023) %>% 
  select(area:year, pop0to17.thsd)

wpp.age.group %>% 
  filter(iso3 %in% menar.ctries) %>% 
  filter(year %in% c(2023, 2050)) %>% 
  select(area:year, pop0to17.thsd) %>% 
  pivot_wider(names_from = year, names_prefix = "y", values_from = pop0to17.thsd) %>% 
  mutate(change.pct =  ((y2050 / y2023) - 1) * 100) %>% 
  arrange(desc(change.pct))%>% 
  mutate(mena.2050 = sum(y2050))

# 15to24
wpp.age.group %>% 
  filter(iso3 %in% menar.ctries) %>% 
  filter(year %in% c(2023, 2050)) %>% 
  select(area:year, pop15to24.thsd) %>% 
  pivot_wider(names_from = year, names_prefix = "y", values_from = pop15to24.thsd) %>% 
  mutate(change.pct =  ((y2050 / y2023) - 1) * 100) %>% 
  arrange(desc(change.pct)) %>% 
  mutate(mena.2050 = sum(y2050))

S# >>> 5) IRQ
IRQ_pop <- wpp.age.group %>% 
  filter(iso3 == "IRQ") %>% 
  select(area:year, pop0to17.thsd)

ggplot(data=IRQ_pop, 
       aes(x=year, y=pop0to17.thsd/1000)) +
  theme_classic() +
  geom_line(size = 0.8, linetype = "solid") +
  scale_x_continuous(breaks=c(1950, 1975, 2000, 2020, 2030, 2050, 2075, 2100)) +
  # scale_y_continuous(breaks=seq(0, 800, 200)) +
  labs(x=NULL, 
       y="Population (in millions)", 
       title = 'Trend of child population (0-17)', 
       # subtitle = 'By SDG region, 1950-2050'
  ) +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))

wpp.age.group %>% 
  filter(iso3 == "IRQ") %>% 
  filter(year %in% c(1970, 2020, 2023, 2030, 2050)) %>% 
  select(area:year, poptotal.thsd,pop0to17.thsd, pop15to24.thsd,pop0to24.thsd) %>% 
  pivot_longer(names_to = "age.grp", names_prefix = "pop", , cols = starts_with("pop"), values_to = "pop.thsd") %>% 
  pivot_wider(names_from = year, names_prefix = "y", values_from = pop.thsd) %>% 
  mutate(d.2020_50.pct = (y2050/y2020 - 1) * 100) %>% 
  mutate(d.1970_20.factor = (y2020/y1970))
  

# >>> 6) "Statement on demographic trends for Africa" - 29 Aug 2023 ----
# Share of globl children living in Africa
wpp.age.group %>% 
  filter(year %in% c(2023, 2030, 2050, 2063) &  area.id %in% c(900, 903)) %>% 
  select(area, year, pop0to17.thsd) %>% 
  mutate(area = str_to_title(area)) %>% 
  pivot_wider(names_from = area, values_from = pop0to17.thsd) %>% 
  mutate(Afr.m = round(Africa / 1000, 0)) %>% 
  mutate(Afr.pct = Africa / World * 100)


# >>> 5) NYT on aging population ----
wpp.age.group.pct %>% 
  filter(year > 2020 &  area.id < 900) %>% 
  select(area:year, pop0to14.pct, pop15to64.pct, pop65plus.pct) %>% 
  # filter(pop0to14.pct >= 25) %>% 
  filter(pop65plus.pct >= 25) %>% 
  mutate(min.u15 = min(pop0to14.pct)) %>% 
  mutate(max.u15 = max(pop0to14.pct)) %>% 
  mutate(min.working = min(pop15to64.pct)) %>% 
  mutate(max.working = max(pop15to64.pct))%>% 
  mutate(min.older = min(pop65plus.pct)) %>% 
  mutate(max.older = max(pop65plus.pct))

# >>> 4) Tableau -----
load(file.path(input,"UNPD/WPP2022/wpp_age.Rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_location.rdata"))

output4 <- "C:/Users/jbeise/OneDrive - UNICEF (1)/Downloads_OneDrive/Tableau/02_trials"

# Version 1
tableau_age.grp.ctries <- wpp.age.group %>% 
  filter(area.id < 900) %>% 
  select(area:pop0to24.thsd) %>% 
  left_join(wpp.location %>% 
              select(area.id, region, region.id, subregion, subregion.id),
            by = "area.id")

write_xlsx(tableau_age.grp.ctries, path = file.path(output4, "tableau_age.grp.xlsx"))

# Version 2
tableau_age.grp.ctries.v2 <- wpp.age.group %>% 
  filter(area.id < 900) %>% 
  select(area:pop0to24.thsd, pop15to64.thsd, pop65plus.thsd, -SDMX_code) %>% 
  pivot_longer(
    cols = starts_with("pop"),
    names_to = "age.grp",
    names_prefix = "pop",
    values_to = "pop.thsd"
  ) %>% 
  mutate(age.grp = str_replace(age.grp, "total", "all")) %>% 
  mutate(age.grp = str_replace(age.grp, "to", "-")) %>% 
  mutate(age.grp = str_remove(age.grp, ".thsd")) %>% 
  mutate(age.grp = str_replace(age.grp, "all", "total")) %>% 
  mutate(projection = if_else(year <= 2022, "estimate", "projection")) %>% 
  mutate(population = pop.thsd * 1000) %>% 
  left_join(wpp.location %>% 
              select(iso3, area.id, region, region.id, subregion, subregion.id),
            by = "area.id")

write_xlsx(tableau_age.grp.ctries.v2, path = file.path(output4, "tableau_age.grp.2.xlsx"))


# >>> 3) GER report -----
load(file.path(input,"UNPD/WPP2022/wpp_dem.rdata"))
# TFR
wpp.dem %>% 
  filter(loctype.id == 4 & year == 2022) %>% 
  select(area:year, poptotal.thsd, tfr) %>% 
  mutate(tfr.rplc = ifelse(tfr < 2.1,"lt","ge")) %>% 
  group_by(tfr.rplc) %>% 
  summarise(poptotal.thsd = sum(poptotal.thsd, na.rm = T)) %>% 
  pivot_wider(names_from = tfr.rplc, values_from = poptotal.thsd) %>% 
  mutate(pct = lt / (lt + ge) * 100)

# Pop growth
wpp.dem %>% 
  filter(area.id == 900 & year <=  2030) %>% 
  select(area:year, pop.growth.rate)
  
# Pop 65+
wpp.age.group %>% 
  filter(area.id == 900 & year %in% c(2022, 2050)) %>% 
  select(area:year, pop65plus.thsd)

# >>> 2) SDG Report ----
# Extracted to SDG_Report_2023.R
load(file.path(input,"UNPD/WPP2022/wpp_dem.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_location.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_unicef.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_5y_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_age.Rdata"))



# Pop0to17 ----
# N ----
pop0to17_sdg <- wpp.age.group %>% 
  filter(loctype == "SDG region" | area.id == 900) %>% 
  select(area:year, loctype, loctype.id, pop0to17.thsd)
# mutate(area = factor(area, levels= c("WORLD","Africa","Asia", "Latin America and the Caribbean",
#                                      "Europe", "Northern America", "Oceania") %>% toupper(),
#                      labels = c("WORLD","Africa","Asia","Latin America and the Caribbean",
#                                 "Europe", "Northern America", "Oceania")))

pop0to17_sdg %>% 
  filter(year %in% c(1990, 2000, 2022)) %>% 
  filter(area.id == 900) %>% 
  arrange(year)

pop0to17_sdg %>% 
  filter(year > 2050) %>% 
  filter(area.id == 900) %>% 
  arrange(year) %>% 
  print(n=50)

pop0to17.ctries <- wpp.age.group %>% 
  filter(area.id < 900 & year == 2022) %>% 
  select(area, year, pop0to17.thsd) %>% 
  arrange(desc(pop0to17.thsd))

pop0to17.ctries


pop0to17_sdg_wide <- pop0to17_sdg %>% 
  select(area, year, pop0to17.thsd) %>% 
  pivot_wider(
    names_from = "area",
    values_from = "pop0to17.thsd")

pop0to17_sdg %>% 
  filter(year== 2022) %>% 
  arrange(desc(pop0to17.thsd))

# region pct ----
pop0to17.pct_sdg <- wpp.age.group.pct %>% 
  filter(loctype == "SDG region" | area.id == 900) %>% 
  select(area:year, loctype, loctype.id, pop0to17.pct)
# mutate(area = factor(area, levels= c("WORLD","Africa","Asia", "Latin America and the Caribbean",
#                                      "Europe", "Northern America", "Oceania") %>% toupper(),
#                      labels = c("WORLD","Africa","Asia","Latin America and the Caribbean",
#                                 "Europe", "Northern America", "Oceania")))

pop0to17.pct_sdg %>% 
  filter(year == 2022)


ggplot(data=pop0to17_sdg 
       %>% filter(area.id != 900 & year <= 2050), 
       aes(x=year, y=pop0to17.thsd/1000, color=area)) +
  theme_classic() +
  geom_line(size = 0.8, linetype = "solid") +
  scale_x_continuous(breaks=c(1950, 1975, 2000, 2020, 2030, 2050)) +
  # scale_y_continuous(breaks=seq(0, 800, 200)) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')) +
  labs(x=NULL, y="Child population (in millions)", 
       # title = 'Trend of child population (0-17)', 
       # subtitle = 'By SDG region, 1950-2050'
       ) +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))


# Life cycle age groups ----
pop.agegrps.sdg <- wpp.age %>% 
  filter(loctype == "SDG region" | area.id == 900, age == 0) %>% 
  select(area:year, loctype, loctype.id, pop0.thsd = pop.thsd) %>% 
  left_join(wpp.age.group %>% 
              select(area.id, year, pop0to17.thsd), by = c("area.id", "year")) %>%
  left_join(wpp.5y.age.group %>% 
              select(area.id, year, pop0to4.thsd:pop15to19.thsd), by = c("area.id", "year")) %>% 
  mutate(pop1to4.thsd = pop0to4.thsd - pop0.thsd) %>% 
  select(area:loctype.id, pop0.thsd, pop1to4.thsd, everything(), -pop0to4.thsd, -pop0to17.thsd ) %>% 
  pivot_longer(cols = starts_with("pop"), names_to = "age.grp", values_to = "pop.thsd" ) %>% 
  mutate(age.grp = str_remove(age.grp, ".thsd")) %>% 
  mutate(age.grp = str_replace(age.grp, "to", "-")) %>% 
  mutate(age.grp = str_replace(age.grp, "pop", "Age ")) %>% 
  mutate(age.grp = factor(age.grp, levels = rev(c("Age 0", "Age 1-4", "Age 5-9", "Age 10-14", "Age 15-19"))))


ggplot(data=pop.agegrps.sdg %>% filter(area.id == 900 & year <= 2050), 
       aes(x=year, y=pop.thsd/1000, fill = age.grp)) +
  theme_classic() +
  geom_area(position = position_stack()) +
  # geom_area(position = position_stack(reverse = T)) +
  scale_x_continuous(breaks=c(1950, 1975, 2000, 2020, 2030, 2050)) +
  # scale_y_continuous(breaks=seq(0, 800, 200)) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')) +
  labs(x=NULL, y="Child population (in millions)", 
       # title = 'Trend of child population (0-17)', 
       # subtitle = 'By SDG region, 1950-2050'
  ) +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))


# countries ----
pop0to17.ctries <- wpp.age.group %>% 
  filter(area.id < 900 & year == 2022) %>% 
  select(area, year, pop0to17.thsd) %>% 
  arrange(desc(pop0to17.thsd))

pop0to17.ctries

pop0to19.ctries <- wpp.age.group %>% 
  filter(area.id < 900 & year %in% c(2021,2022)) %>% 
  select(area, area.id, iso3, year, pop0to19.thsd) %>% 
  arrange(area, year)

write_csv(pop0to19.ctries, file= file.path(output, "pop0to19.ctries.csv"))

# Poptotal ----
poptotal_sdg <- wpp.dem %>% 
  filter(loctype == "SDG region" | area.id == 900, year <= 2100) %>% 
  select(area:year, loctype, loctype.id, poptotal.thsd)
# mutate(area = factor(area, levels= c("WORLD","Africa","Asia", "Latin America and the Caribbean",
#                                      "Europe", "Northern America", "Oceania") %>% toupper(),
#                      labels = c("WORLD","Africa","Asia","Latin America and the Caribbean",
#                                 "Europe", "Northern America", "Oceania")))

poptotal_sdg %>% 
  filter(year == 2022)

poptotal_sdg_wide <- poptotal_sdg %>% 
  select(area, year, poptotal.thsd) %>% 
  pivot_wider(
    names_from = "area",
    values_from = "poptotal.thsd"
  )

ggplot(data=poptotal_sdg 
       %>% filter(area.id != 900), aes(x=year, y=poptotal.thsd/1000000, color=area)) +
  theme_classic() +
  geom_line(size = 0.8, linetype = "solid") +
  scale_x_continuous(breaks=seq(1950, 2100, 25)) +
  # scale_y_continuous(breaks=seq(0, 800, 200)) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')) +
  labs(x=NULL, y="Total population (in billions)", 
       # title = 'Trend of total population', subtitle = 'By SDG region, 1950-2100'
       ) +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))
ggsave(file.path(output,'poptotal_SDG.png'),height = 4,width = 7, device = 'png')


# TFR ----
wpp.dem %>% 
  filter(area.id == 900, year <= 2100) %>% 
  select(area:year, loctype, loctype.id, tfr)

# Child population growth - region ----
pop0to17.aagr <- wpp.age.group %>% 
  select(area, area.id, loctype.id, year, pop0to17.thsd) %>% 
  arrange(area, year) %>% 
  mutate(pop0to17.aagr1 = (log(pop0to17.thsd / lag(pop0to17.thsd, 1)) / 1) * 100) %>% 
  mutate(pop0to17.aagr5 = (log(pop0to17.thsd / lag(pop0to17.thsd, 5)) / 5) * 100) %>% 
  mutate(pop0to17.aagr10 = (log(pop0to17.thsd / lag(pop0to17.thsd, 10)) / 10) * 100) %>% 
  filter(year >= 1960)


#SSA
pop0to17.aagr %>% 
  filter(area.id == 1834 & year > 2015) %>% 
  select(area:year, pop0to17.thsd, pop0to17.aagr1) %>% 
  mutate(pop0to17.annual = pop0to17.thsd * pop0to17.aagr1 / 100 ) %>% 
  print(n=50)

#EURNAM
pop0to17.aagr %>% 
  filter(area.id == 1829 & year > 2015) %>% 
  select(area:year, pop0to17.thsd, pop0to17.aagr1) %>% 
  mutate(pop0to17.annual = pop0to17.thsd * pop0to17.aagr1 / 100 ) %>% 
  print(n=50)

# Child population growth - countries ----
pop0to17.ctries.growth <- wpp.age.group %>% 
  select(area, area.id, year, pop0to17.thsd) %>% 
  arrange(area, year) %>% 
  filter(area.id < 900 & year %in% c(2022, 2030, 2050)) %>% 
  pivot_wider(names_from = year, names_prefix = "y",values_from = pop0to17.thsd) %>% 
  mutate(change.2030.pct = (y2030/y2022 -1) * 100) %>% 
  mutate(change.2050.pct = (y2050/y2022 -1) * 100) %>% 
  arrange(desc(change.2050.pct)) %>% 
  left_join(wpp.age.group %>% 
              filter(year == 2022) %>% 
              select(area.id, poptotal.thsd.2022 = poptotal.thsd), by = "area.id") %>% 
  select(area, area.id, poptotal.thsd.2022, everything())

  


# Save data ----
write_xlsx(list(poptotal_wide = poptotal_sdg_wide,
                pop0to17_long = pop0to17_sdg , 
                pop0to17_wide = pop0to17_sdg_wide,
                pop0to17_ctries = pop0to17.ctries,
                pop0to17_aagr = pop0to17.aagr,
                pop0to17.ctries.growth = pop0to17.ctries.growth), 
           path = file.path(output,'Population_sdg.xlsx'))

# >>> 1) JP 16 May 2023: Child population trends for different regions ----

# data input
load(file.path(input,"UNPD/WPP2022/wpp_age_group.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_location.rdata"))
load(file.path(input,"UNPD/WPP2022/wpp_unicef.rdata"))

# Age 0 and 1 and combined
x <- wpp.age %>% 
  filter(area == 900 & year == 2022) %>% 
  filter(age < 2) %>% 
  select(area:year, age, pop.thsd)

sum(x$pop.thsd)

wpp.age.group.pct %>% 
  filter(area.id == 900, year ==2022) %>% 
  select(area:year, pop0to17.pct)

# SDG - SSA
data_sdg <- wpp.age.group %>% 
  filter(loctype == "SDG region" , year < 2051) %>% 
  select(area:year, loctype, loctype.id, pop0to17.thsd)
  # mutate(area = factor(area, levels= c("WORLD","Africa","Asia", "Latin America and the Caribbean",
  #                                      "Europe", "Northern America", "Oceania") %>% toupper(),
  #                      labels = c("WORLD","Africa","Asia","Latin America and the Caribbean",
  #                                 "Europe", "Northern America", "Oceania")))

data_sdg %>% 
  filter(year == 2022)

data_sdg.wide <- data_sdg %>% 
  select(-area.id) %>% 
  pivot_wider(
    names_from = "area",
    values_from = "pop0to17.thsd"
  )

write.csv(data_sdg.wide, file = file.path(output,'data_sdg.csv'), row.names = F)
write_xlsx(list(sdg_long = data_sdg , sdg_wide = data_sdg.wide), path = file.path(output,'data_sdg.xlsx'))


ggplot(data=data_sdg %>% filter(area.id != 900), aes(x=year, y=pop0to17.thsd/1000, color=area)) +
  theme_classic() +
  geom_line(size = 0.8, linetype = "solid") +
  scale_x_continuous(breaks=c(1950, 1975, 2000, 2020, 2030, 2050)) +
  # scale_y_continuous(breaks=seq(0, 800, 200)) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')) +
  labs(x=NULL, y="Child population (in millions)", title = 'Trend of child population (0-17)', subtitle = 'By SDG region, 1950-2050') +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))
ggsave(file.path(output,'Pop0to17_SDG.png'),height = 4,width = 8, device = 'png')

# M49 - SSA
data_m49 <- wpp.age.group %>% 
  filter(loctype.id == 2 , year < 2051) %>% 
  select(area:year, pop0to17.thsd)
# mutate(area = factor(area, levels= c("WORLD","Africa","Asia", "Latin America and the Caribbean",
#                                      "Europe", "Northern America", "Oceania") %>% toupper(),
#                      labels = c("WORLD","Africa","Asia","Latin America and the Caribbean",
#                                 "Europe", "Northern America", "Oceania")))
# write.csv(data1.3, file = file.path(output,'data1_children.csv'), row.names = F)


ggplot(data=data_m49 %>% filter(area.id != 900), aes(x=year, y=pop0to17.thsd/1000, color=area)) +
  theme_classic() +
  geom_line(size = 0.8, linetype = "solid") +
  scale_x_continuous(breaks=c(1950, 1975, 2000, 2020, 2030, 2050)) +
  scale_y_continuous(breaks=seq(0, 1500, 500)) +
  # scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')) +
  labs(x=NULL, y="Child population (in millions)", title = 'Trend of child population (0-17)', subtitle = 'By region, 1950-2050') +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))
ggsave(file.path(output,'Under18_M49.png'),height = 4,width = 8, device = 'png')


# UNICEF - SSA
# Exploration
wpp.age.group.unicef %>% 
  select(area, area.id, SDMX_code) %>% 
  filter(str_sub(area, 1, 10) == "UNICEF Reg") %>% 
  unique(.) %>% 
  print(n=40)

# data
data_unicef <- wpp.age.group.unicef %>% 
  filter(str_sub(area, 1, 10) == "UNICEF Reg") %>% 
  filter(area.id %out% c(2039, 2040)) %>% 
  filter(year < 2051) %>% 
  select(area:year, pop0to17.thsd) %>% 
  mutate(area = str_sub(area, 17, 50))
# mutate(area = factor(area, levels= c("WORLD","Africa","Asia", "Latin America and the Caribbean",
#                                      "Europe", "Northern America", "Oceania") %>% toupper(),
#                      labels = c("WORLD","Africa","Asia","Latin America and the Caribbean",
#                                 "Europe", "Northern America", "Oceania")))
# write.csv(data1.3, file = file.path(output,'data1_children.csv'), row.names = F)


ggplot(data=data_unicef %>% filter(area.id != 900), aes(x=year, y=pop0to17.thsd/1000, color=area)) +
  theme_classic() +
  geom_line(size = 0.8, linetype = "solid") +
  scale_x_continuous(breaks=c(1950, 1975, 2000, 2020, 2030, 2050)) +
  # scale_y_continuous(breaks=seq(0, 2, 0.25)) +
  # scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')) +
  labs(x=NULL, y="Child population (in millions)", title = 'Trend of child population (0-17)', subtitle = 'By UNICEF region, 1950-2050') +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))
ggsave(file.path(output,'Under18_UNICEF.png'),height = 4,width = 8, device = 'png')

# WB income
# Exploration
wpp.age.group %>% 
  select(area, area.id, loctype,loctype.id) %>% 
  filter(area.id > 900) %>% 
  filter(loctype.id == 13) %>% 
  unique(.) %>% 
  print(n=100)

data_wb <- wpp.age.group %>% 
  filter(loctype.id == 13 , year < 2051) %>% 
  filter(area.id %out% c(1517, 1518)) %>% 
  select(area:year, pop0to17.thsd) %>% 
  mutate(area = factor(area, levels= c("High-income countries", "Upper-middle-income countries", "Lower-middle-income countries", "Low-income countries")))
# c("WORLD","Africa","Asia", "Latin America and the Caribbean",
#                                      "Europe", "Northern America", "Oceania") %>% toupper(),
#                      labels = c("WORLD","Africa","Asia","Latin America and the Caribbean",
#                                 "Europe", "Northern America", "Oceania")))
# write.csv(data1.3, file = file.path(output,'data1_children.csv'), row.names = F)


ggplot(data=data_wb %>% filter(area.id != 900), aes(x=year, y=pop0to17.thsd/1000, color=area)) +
  theme_classic() +
  geom_line(size = 0.8, linetype = "solid") +
  scale_x_continuous(breaks=c(1950, 1975, 2000, 2020, 2030, 2050)) +
  scale_y_continuous(breaks=seq(0, 1500, 500)) +
  # scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')) +
  labs(x=NULL, y="Child population (in millions)", title = 'Trend of child population (0-17)', subtitle = 'By Worldbank income groups, 1950-2050') +
  geom_vline(xintercept = 2022, colour="grey50", linetype = "dashed") +
  theme(axis.text = element_text(size = 8, color = 'grey50'), axis.title = element_text(size = 8, color = 'grey50'),
        legend.title=element_blank(), 
        legend.position="right", 
        axis.ticks = element_line(colour = 'grey50', size = 0.4), 
        axis.line = element_line(colour = 'grey50', size = 0.4),plot.title = element_text(color = '#6A1E74'),
        plot.subtitle = element_text(color = '#6A1E74'))
ggsave(file.path(output,'Under18_WB.png'),height = 4,width = 8, device = 'png')






