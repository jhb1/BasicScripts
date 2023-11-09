# title_start ################################################
# ReadIn_Eurostat.R
#
# Using Eurostat package
#
# title_end ################################################


rm(list=ls())

library(tidyverse)
library(readxl)
library(readr)
library(stringr)
library(countrycode)   # contains "codelist_panel"
library(eurostat)
library(lubridate)     # for today()



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

# explore EUROSTAT db ----
search_eurostat("Population on 1 January by age, sex and broad group of country of birth",  fixed = TRUE)
search_eurostat("unaccompanied",  fixed = TRUE) %>% 
  select(title, code) %>% 
  print(n=50)

# codelist_panel
unique(codelist_panel$year)

ctries <- codelist_panel %>% 
  filter(year == 2020)

# 02 - UASC (9 facts) ----
eur_asyunaa <- get_eurostat("migr_asyunaa")


# Total EU27+
eur01 <- eur_asyunaa %>% 
  mutate(year = as.numeric(format(time, "%Y"))) %>% 
  filter(citizen == "TOTAL" & sex == "T" & age == "TOTAL") %>% 
  filter(geo != "EU27_2020") %>% 
  arrange(desc(values)) %>% 
  group_by(year) %>% 
  summarise(eur27plus = sum(values), 
            N = n()) %>% 
  ungroup()

# In UK: In 2022 there were 5,242 applications from unaccompanied asylum-seeking children (UASC) (see "https://www.gov.uk/government/publications/illegal-migration-bill-factsheets/illegal-migration-bill-children-factsheet#:~:text=In%202022%20there%20were%205%2C242,were%20aged%2016%20or%2017.")

eur01 %>% 
  mutate(UK = if_else(year == "2022", 5242, NA)) %>% 
  mutate(eur27plus_UK = eur27plus + UK)
  

#ctries
eur_asyunaa %>% 
  mutate(year = format(time, "%Y")) %>% 
  filter(citizen == "TOTAL" & sex == "T" & age == "TOTAL") %>% 
  filter(year == 2022) %>% 
  select(geo) %>% 
  left_join(ctries %>% 
              select(geo = eurostat, iso3c, ctry=country.name.en),
            by = "geo") %>% 
  arrange(ctry) %>% 
  print(n=40)

# chart
ggplot(data=eur01, aes(x=year, y=eur27plus)) +
  geom_col() +
  theme_classic()



# 01 - migrant -----
euro.pop.mig.2020 <- get_eurostat("migr_pop4ctb") %>% 
  rename('iso2' = geo, 'year' = time, 'pop' = values)  %>% 
  select(iso2, year, everything()) %>% 
  mutate(year = as.integer(substr(year,1,4))) %>%  # year had char type
  spread(key = c_birth, value = pop) %>% 
  mutate(age = str_replace(age, "Y_LT1", "Y0")) %>% 
  mutate(age.single = parse_number(age)) %>%
  mutate(for.born.pct = FOR / NAT * 100) %>% 
  arrange(iso2, desc(year), desc(sex), age.single)  
  # left_join(codelist_panel %>% 
  #             filter(year == max(year)) %>% 
  #             select(iso2 = eurostat, iso3 = iso3c, area.id = un), by = "iso2")





# (migr_asypenctzm) Persons subject of asylum applications pending at the end of the month by citizenship, age and sex - monthly data ----
euro.migr_asypenctzm.2022 <- get_eurostat("migr_asypenctzm",
                                          filters=list(time="2022-12",
                                                       sex="T",
                                                       age=c("TOTAL", "Y_LT18")),
                                          time_format = "raw",
                                          cache=F) |> 
  rename('iso2' = geo)  |> 
  group_by(iso2) |> 
  summarise(AS_TOTAL=sum(values[age=="TOTAL"], na.rm=TRUE),
            AS_children=sum(values[age=="Y_LT18"], na.rm=TRUE),
            .groups="drop") |> 
  mutate(AS_children_perc=AS_children/AS_TOTAL, year = 2022) 
  