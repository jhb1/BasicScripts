

library("data.table")
library("ggplot2")

USERPROFILE <- Sys.getenv("USERPROFILE")


# Part I. the dots --------------------------------------------------------
load("Pop0to19_SDG.Rdata")
dt1 <- setDT(pop.0to19.sdg.wide.years)

dt1[, label:= format(round(`year.2022`/1E3, 1), nsmall = 1)]

colors_SDG <- c("Sub-Saharan Africa"            = "#E2231A",
                "Central and Southern Asia"     = "#F26A21",
                "Eastern and South-Eastern Asia"   = "#FFC20E",
                "Northern Africa and Western Asia" = "#00833D",
                "Latin America and the Caribbean"  = "#80BD41",
                "Europe and Northern America"   = "#1CABE2",
                "Australia and New Zealand"     = "#0058AB",
                "Oceania (exc. Australia and New Zealand)" = "#6A1E74"
)

dt1[, Region:= dplyr::recode(area, "Australia/New Zealand" = "Australia and New Zealand",
                      "Oceania (excluding Australia and New Zealand)" = "Oceania (exc. Australia and New Zealand)")]
all(names(colors_SDG) %in% dt1$Region)
names(colors_SDG)[!names(colors_SDG) %in% dt1$Region]

dt1 <- dt1[Region!="WORLD"]
Region_order <- dt1[order(-year.2022), Region]
dt1[, Region:= factor(as.factor(Region), levels = Region_order)]
dt1$scenario <- " "
ggplot(data = dt1, aes(x = Region, y = scenario, 
                       color = Region, size = year.2022))+
  geom_point() + 
  # scale_size(range = c(3, 30)) + 
  scale_size_area(max_size=30, guide = FALSE) + # I think this is better
  scale_color_manual(values = colors_SDG, guide = "none") +
  scale_x_discrete(labels = scales::wrap_format(20)) + 
  # scale_y_discrete(labels = age_label) +
  geom_text(aes(label = label), size = 3, nudge_y = -0.2, color = "black") + 
  labs(x = "", y = "") + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

ggsave(filename = file.path("Figure map population 1-19 by SDG region.png"), width = 10, height = 3)
ggsave(filename = file.path("Figure map population 1-19 by SDG region.pdf"), width = 10, height = 3)


# Part II. the maps -------------------------------------------------------

# load map shape files
map.sp.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/sp") # location of the spatial polgons
# Location on Dropbox: https://www.dropbox.com/sh/1rehdfe11834r3b/AAB9tkDp2o6r4F3R1qn5Nvx-a?dl=0
# 

world.robin <- readRDS(file.path(map.sp.dir, "sp.world.robin.rds"))
lks <- readRDS(file.path(map.sp.dir, "sp.lks.rds")) # lakes
bnd <- readRDS(file.path(map.sp.dir, "sp.bnd.rds")) # borderline
bnd.line <- bnd[bnd$CARTOGRAPH=="International boundary line",]
bnd.dash <- bnd[bnd$CARTOGRAPH=="Dashed boundary line" | bnd$CARTOGRAPH=="Undetermined international dashed boundary line",]
bnd.dot <- bnd[bnd$CARTOGRAPH=="Dotted boundary line" | bnd$CARTOGRAPH=="Dotted boundary line (Abyei)",]
bnd.ssd <- bnd[bnd$BDY_CNT01=="SDN" & bnd$BDY_CNT02=="SSD",] # Specify SSD-SDN boundaries and plot later to resolve issue of not showing in the original script 

# load data 
dc <- fread(file.path(USERPROFILE, "Dropbox/UN IGME data/2022 Round Estimation/Code/input/country.info.CME.csv"))
dc[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
dc_regions <- dc[, .(ISO3Code, SDG_region)]
stopifnot(all(dc_regions$SDG_region %in% names(colors_SDG)))
dc_regions[, fill_color := dplyr::recode(SDG_region, !!!colors_SDG)]

dc$ISO3Code[!dc$ISO3Code %in% world.robin $ISO3_CODE]

# Join color to the df
world.robin.df <- broom::tidy(world.robin, region = "ISO3_CODE")
world.robin.df <- sp::merge(world.robin.df, dc_regions, by.x = "id", by.y = "ISO3Code", all = TRUE)
world.robin.df$fill_color[is.na(world.robin.df$fill_color)] <- "gray"
setorder(world.robin.df, id, order)
BDLINE_WIDTH <- 0.2 
ggplot() +
  geom_polygon(data = world.robin.df, aes(x = long, y = lat, group = group, fill = fill_color), colour = NA) +
  geom_polygon(data = lks, aes(x = long, y = lat, group = group), fill="white", colour = "white") +
  
  geom_path(data = bnd.line, aes(x = long, y = lat, group = group), linetype = "solid",  size = BDLINE_WIDTH, colour = "grey") +
  geom_path(data = bnd.dash, aes(x = long, y = lat, group = group), linetype = "dashed", size = BDLINE_WIDTH, colour = "grey") +
  geom_path(data = bnd.dot,  aes(x = long, y = lat, group = group), linetype = "dotted", size = BDLINE_WIDTH, colour = "grey") +
  geom_path(data = bnd.ssd,  aes(x = long, y = lat, group = group), linetype = "dashed", size = BDLINE_WIDTH, colour = "grey") +
  
  scale_fill_identity() +  # fill as it is 
  ggthemes::theme_map() +
  theme(legend.position = "none",
        legend.direction = "horizontal",
        legend.justification = c("center"),
        legend.title = element_text(size=9),
        legend.text = element_text(size=9),
        plot.title = element_blank())  
  # guides(fill = guide_legend(nrow = 1))

ggsave(filename = "UNIGME_SDG_Simple_map.pdf", height = 4.5, width = 10)
ggsave(filename = "UNIGME_SDG_Simple_map.png", height = 4.5, width = 10)
