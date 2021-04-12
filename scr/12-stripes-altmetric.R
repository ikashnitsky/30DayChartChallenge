#===============================================================================
# 2021-04-12 -- 30dcc
# 12. Strips. Altmetric attention -- top 1M export
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(lubridate)
library(hrbrthemes)

# Start dates
# January 1, 1983 -- internet born
# March 21, 2006 -- Twitter launched

df <- read_csv("~/data/altmertic/all-altmetric-2021-04-12.csv") %>%
    janitor::clean_names()

df %>% tail %>% view()


monthly <- df %>%
    mutate(
        year = publication_date %>% year,
        month = publication_date %>% month,
        year_month = paste0(year, "_", month)
    ) %>%
    # group_by(year, month, year_month) %>%
    group_by(year, month, year_month) %>%
    summarise(
        n_papers = n(),
        cum_attn = altmetric_attention_score %>% sum,
        mean_attn = altmetric_attention_score %>% mean
    ) %>%
    ungroup()

# palette
# https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/
col_strip <- RColorBrewer::brewer.pal(11, "RdBu")


monthly %>%
    filter(
        year > 2005 &  year < 2022,
        !year_month %in% paste(2021, 5:12, sep = "_"),
        !year_month %in% paste(2006, 1:3, sep = "_")
    ) %>%
    ggplot(aes(year_month, y = 1, fill = mean_attn))+
    geom_col(color = NA, width = 1)+
    scale_fill_gradientn(colours = col_strip %>% rev, guide = NULL)+
    theme_void(base_family = font_rc)+
    labs(
        title = "Monthly average attention score of a published scientific paper",
        subtitle = "Altmetric data, April 2006 — April 2021, top 1 million noticed papers of all time export",
        caption = "#30DayChartChallenge 12 Stripes | @ikashnitsky"
    )+
    theme(
        text = element_text(color = "#dadada"),
        plot.title = element_text(family = "Roboto Slab"),
        plot.margin = c(.2,0,.2,0) %>% unit("lines")
    )

out <- last_plot()

ggsave(
    "out/12-stripes-altmetric.png",
    width = 8, height = 4.5, type = "cairo-png"
)
