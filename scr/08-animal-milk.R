#===============================================================================
# 2021-04-08 -- 30dcc
# 8. Animal. NUTS-2 milk production
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(janitor)
library(cowplot)
library(rcartocolor)
library(sf)
library(rmapshaper)
library(eurostat)

# the built-in dataset of EU boundaries
gd <- eurostat_geodata_60_2016 %>%
    clean_names() %>%
    st_transform(crs = 3035)

# country borders
bord <- gd %>%
    filter(levl_code == 0) %>%
    ms_innerlines()

# background
back <- gd %>%
    filter(levl_code == 0) %>%
    ms_dissolve()

# get teh dataset
df <- get_eurostat("agr_r_milkpr")

# map
gd %>%
    right_join(df, c("id" = "geo")) %>%
    filter(levl_code == 2, time == "2019-01-01") %>%
    ggplot()+
    geom_sf(data = back, color = NA, fill = "#000066")+
    geom_sf(aes(fill = values/1e3), color = NA)+
    geom_sf(data = bord, color = "#ffffff", size = .3)+
    scale_fill_carto_c(
        palette = "ag_Sunset", direction = 1,
        guide = guide_colorbar(
            barheight = 7, barwidth = 3.5, title.position = "top"
        )
    )+
    coord_sf(datum = NA, ylim = c(15e5,55e5), xlim = c(20e5, 75e5))+
    theme_map()+
    theme(
        legend.position = c(.7, .7),
        plot.background = element_rect(fill = "#000044"),
        text = element_text(family = "mono", face = 2, color = "#ffffff")
    )+
    labs(
        title = "Milk production in regions of Europe",
        subtitle = "Eurostat NUTS-2 regions, 2019",
        fill = "m tons",
        caption = "@ikashnitsky"
    )


ggsave(
    "out/08-animal.png",
    width = 5.2, height = 4.7, type = "cairo-png"
)
