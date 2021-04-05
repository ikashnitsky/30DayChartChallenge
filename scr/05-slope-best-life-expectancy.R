#===============================================================================
# 2021-04-05 -- 30dcc
# 5. Slope. Recreate the Oeppen-Vaupel top performing life expectancy line
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(ggdark)
library(glue)
library(hrbrthemes)
library(cowplot)
library(paletteer)

# Here I'm using a locally stored copy of HMD life tables
# You can get one here:
# https://github.com/OxfordDemSci/ex2020/blob/master/dat/hmdhfd/lt-1x1.rds
# Or use the functions provided here to quickly deal with a local copy of the
# whole HMD
# https://gist.github.com/ikashnitsky/0f93062f2b67eeac69949554027fa84f
# Or yet another version is to use a package that allows to download specific
# datasets from HMD -- HMDHFDplus or MortalitySmooth
# https://ikashnitsky.github.io/2017/data-acquisition-three/
load("~/github/ex-delta/dat/lt1x1.rda")

# # For replication purposes here goes commented code to download a github dump
# # of this data
# data_url <- "https://github.com/OxfordDemSci/ex2020/raw/af53970602891d6126eb32bf0efa956bdd9d3343/dat/hmdhfd/lt-1x1.rds"
# t <- tempfile()
# download.file(data_url, destfile = t, extra="L")
# # https://stackoverflow.com/a/59653993/4638884
# lt1x1 <- readRDS(file = t)
# unlink(t)

lt1x1 %>%
    filter(age == 0, sex == "f", year > 1839) %>%
    ggplot(aes(year, ex))+
    geom_line(
        aes(group = country), size = .1, color = "#bababa", lineend = "round"
    )+
    geom_point(
        data = . %>% group_by(year) %>% filter(ex == max(ex)) %>%
            # arrange countries by first appearance
            mutate(cntryear = paste(year, country)) %>%
            arrange(cntryear) %>%
            mutate(
                country = country %>% as_factor %>%
                    fct_recode(
                        Belrus = "BLR",
                        Denmark = "DNK",
                        `Hong Kong` = "HKG",
                        Island = "ISL",
                        Japan = "JPN",
                        Norway = "NOR",
                        `New Zealand` = "NZL_NM",
                        Sweden = "SWE"
                    )
            ),
        aes(color = country),
        size = 1
    )+
    stat_smooth(
        data = . %>% group_by(year) %>% filter(ex == max(ex)),
        geom = "line", method = "lm", se = F
    )+
    coord_cartesian(ylim = c(0, 90))+
    scale_x_continuous(position = "top")+
    scale_y_continuous(expand = c(0, 0))+
    dark_theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(1, .12),
        legend.justification = c(1, .5),
        plot.title = element_textbox(family =  "Roboto Slab", face = 2)
    )+
    scale_colour_paletteer_d(
        "RColorBrewer::Paired",
        guide = guide_legend(direction = "horizontal")
    )+
    labs(
        title = "Best recorded female life expectancy in the world",
        subtitle = "Replicating Figure 1 from: Oeppen, J., & Vaupel, J. W. (2002). Broken limits to life expectancy. Science, 296(5570), 1029–1031. https://doi.org/10.1126/science.1069675
" %>% str_wrap(65),
        y = "Life expectancy, years",
        x = NULL,
        color = NULL,
        caption = "Data: Human Mortality Database | Design: @ikashnitsky"
    )


ggsave(
    "out/05-slope-life-expectancy.png",
    width = 5, height = 4.5, type = "cairo-png"
)
