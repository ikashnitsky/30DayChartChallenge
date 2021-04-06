#===============================================================================
# 2021-04-06 -- 30dcc
# 4. Magic. Replicate in code
# https://twitter.com/ikashnitsky/status/1378673154730647555
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(cowplot)
library(sysfonts)
library(extrafont)
library(xkcd)

# get the fonts
font_add_google(name = "Caveat", family = "Caveat")
font_add_google(name = "Permanent Marker", family = "Permanent Marker")
font_add_google(name = "Frijole", family = "Frijole")
font_import(
    pattern = c("[X/x]kcd", "Frijole", "Permanent Marker", "Caveat"),
    prompt = FALSE
)

# generate data for illustration purposes
df <- tibble::tribble(
                      ~plot,         ~axis_text,
                 "vertical",       "Unbearable",
                 "vertical",       "Impossible",
                 "vertical", "To read properly",
                 "vertical",             "Text",
                 "vertical",   "That everybody",
                 "vertical",            "Hates",
               "horizontal",             "Dear",
               "horizontal",           "Reader",
               "horizontal",    "I really care",
               "horizontal",            "About",
               "horizontal",        "Your neck",
               "horizontal",           "Health"
               ) %>%
    group_by(plot) %>%
    mutate(
        nlet = axis_text %>% nchar(),
        axis_text_num = axis_text %>% seq_along()
    )


# prepare datasets for mapping with xkcd
mapping <- aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax)

df_v <- df %>% filter(plot == "vertical") %>%
    mutate(
        xmin = axis_text_num - .2,
        xmax = axis_text_num + .2,
        ymin = 0,
        ymax = nlet
    )

df_h <- df %>% filter(plot == "horizontal")  %>%
    mutate(
        xmin = axis_text_num - .2,
        xmax = axis_text_num + .2,
        ymin = 0,
        ymax = nlet
    )

# verical bars plot
set.seed(2601)
df_v %>%
    ggplot(aes(axis_text_num, nlet))+
    theme_xkcd()+
    xkcdaxis(
        xrange = c(.5, df_v$axis_text %>% seq_along %>% max),
        yrange = c(0, df_v$nlet %>% max)
    )+
    xkcdrect(data = df_v, mapping)+
    scale_x_continuous(breaks = 1:6, labels = df_v$axis_text)+
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Caveat")
    )+
    labs(
        x = NULL,
        y = "Crack your damn\nneck variable"
    )

one <- last_plot()

# horizontal bars plot
set.seed(911)
df_h %>%
    ggplot(aes(7 - axis_text_num, nlet))+
    theme_xkcd()+
    xkcdaxis(
        xrange = c(.5, df_h$axis_text %>% seq_along %>% max),
        yrange = c(0, df_h$nlet %>% max)
    )+
    xkcdrect(data = df_v, mapping)+
    scale_x_continuous(breaks = 1:6, labels = rev(df_h$axis_text))+
    labs(
        x = NULL,
        y = "Perfectly readable\nvariable descpription"
    )+
    coord_flip()+
    theme(
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 20, color = 1),
        axis.ticks = element_blank(),
        text = element_text(family = "Caveat", color = 1)
    )

two <- last_plot()

# assamble the put plot
ggdraw()+
    # title
    annotate(
        "text", x = .5, y = .98,
        label = "rotate the damn plot" %>% toupper(),
        family = "Permanent Marker",
        vjust = 1, size = 10
    )+
    # subtitle
    annotate(
        "text", x = .5, y = .87,
        label = "the single easiest and most useful dataviz" %>% toupper(),
        family = "Permanent Marker",
        vjust = 1, size = 5
    )+
    # trick
    annotate(
        "text", x = .82, y = .77,
        label = "trick" %>% toupper(),
        family = "Frijole",
        vjust = 1, size = 10, angle = 15
    )+
    # caption right
    annotate(
        "text", x = .98, y = .02,
        label = "#30DayChartChallenge | @ikashnitsky",
        family = "Permanent Marker",
        hjust = 1, vjust = 0, size = 3
    )+
    # caption left
    annotate(
        "text", x = .02, y = .02,
        label = "Data: nchar() of the corresponding labels",
        family = "Permanent Marker",
        hjust = 0, vjust = 0, size = 3
    )+
    # inset the two plots
    draw_plot(one, x = .05, width = .4, y = .1, height = .6)+
    draw_plot(two, x = .55, width = .4, y = .1, height = .5)

out <- last_plot()

ggsave(
    "out/04-magic.png", out,
    width = 5.4, height = 4, type = "cairo-png"
)
