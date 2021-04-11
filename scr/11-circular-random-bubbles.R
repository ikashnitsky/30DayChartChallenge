#===============================================================================
# 2021-04-11 -- 30dcc
# 11. Circular. Random bubbles
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(hrbrthemes)

n <- 100

set.seed(126)

tibble(
    x = runif(n),
    y = runif(n),
    size = runif(n, min = 3, max = 20),
    # generate lighter colors
    color = rgb(runif(n, min = .5), runif(n, min = .5), runif(n, min = .5))
) %>%
    ggplot(aes(x, y, size = size, color = color)) +
    geom_point()+
    scale_color_identity()+
    scale_size_identity()+
    coord_cartesian(c(0, 1), c(0, 1))+
    theme_void()+
    theme(
        plot.background = element_rect(color = NA, fill = "#444444"),
        text = element_text(family = font_rc, color = "#eaeaea")
    )+
    labs(
        caption = "#30DayChartChallenge 11 Circular | @ikashnitsky"
    )

ggsave(
    "out/11-bubbles.png",
    width = 7, height = 5, type = "cairo-png"
)
