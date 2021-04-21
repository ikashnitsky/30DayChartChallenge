#===============================================================================
# 2021-04-20 -- 30dcc
# 20. Increasing. Altmetic attention to the papers
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(lubridate)
library(hrbrthemes)
library(ggdark)
library(ggtext)

# beware, the raw dataset weights more than 0.5GB
# thus the code will be only replicable after the load() back data
df <- read_csv("~/data/altmertic/all-altmetric-2021-04-12.csv") %>%
    janitor::clean_names()

yearly <- df %>%
    mutate( year = publication_date %>% year) %>%
    filter(
        year %>% is_weakly_greater_than(2000),
        year %>% is_weakly_less_than(2020)
    ) %>%
    group_by(year = year %>% paste) %>%
    summarise(mean_score = altmetric_attention_score %>% mean) %>%
    ungroup() %>%
    mutate(year = year %>% as.numeric)

save(yearly, file = "dat/20-yearly-attention.rda")
load("dat/20-yearly-attention.rda") # if replicating, reload from here

yearly %>%
    ggplot(aes(year, mean_score)) +
    geom_path(lineend = "round", color = "#F5C710FF", size = 1)+
    scale_x_continuous(position = "top") +
    scale_y_continuous(limits = c(0, 150), expand = c(0, 0))+
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
        y = "Average yearly<br><span style = 'color:#F5C710;'>attention score</span><br>of a published<br>scientific paper",
        caption = "\nData: Altmetric, top 1 million records export | Design: @ikashnitsky"
    )



ggsave(
    "out/20-upwards-altmetric-yearly.png",
    width = 5, height = 2.8125, type = "cairo"
)
