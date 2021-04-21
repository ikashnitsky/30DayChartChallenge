#===============================================================================
# 2021-04-21 -- 30dcc
# 21. Downwards. Average author's contribution to a paper
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(lubridate)
library(hrbrthemes)
library(ggdark)
library(ggtext)

# The data manually downloaded from Scopus
# All papers ever published by Demopgraphic Research
demres <- read_csv("dat/10-scopus-demres.csv") %>%
    janitor::clean_names()

nauth <- demres %>%
    transmute(
        year,
        nauth = author_s_id %>% str_extract_all(";") %>% nchar()
    ) %>%
    group_by(year) %>%
    summarise(
        avg_ctb = 1 / mean(nauth),
        n = n()
    )

nauth %>%
    ggplot(aes(year, avg_ctb))+
    stat_smooth(method = "lm", se = F, color = "#ffffff", size = 1)+
    geom_point(aes(size = n), color = "#F5C710FF")+
    scale_size_area(max_size = 2, guide = NULL)+
    scale_x_continuous(position = "top") +
    scale_y_percent(limits = c(0, .15001), expand = c(0, 0))+
    dark_theme_minimal(base_family =  font_rc, base_size = 14) +
    theme(
        axis.title.y = element_textbox_simple(
            width = grid::unit(2, "in"),
            hjust = 0,
            family = "Roboto Slab",
            size = 16,
            face = 2
        ),
        plot.caption = element_text(size = 7),
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.grid.minor.x  = element_blank(),
        panel.grid = element_line(color = "#eaeaea77")
    ) +
    labs(
        x = NULL,
        y = "Average author's<br><span style = 'color:#F5C710;'>contribution</span> to a<br>paper published in<br>Demographic Research",
        caption = "\nNotes: A simplistic assumption of eqaul authors' contributions is taken;\nSize of the points reflects the number of papers published in a given year\nData: Scopus, all Demographic Research papers | Design: @ikashnitsky"
    )

ggsave(
    "out/21-downwards-average-ctb.png",
    width = 5, height = 2.8125, type = "cairo"
)
