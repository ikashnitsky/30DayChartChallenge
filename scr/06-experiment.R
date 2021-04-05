#===============================================================================
# 2021-04-06 -- 30dcc
# 6. Experiment. Try recreating the mind-carcking effect of
# https://twitter.com/ikashnitsky/status/1359218142409601029
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(cowplot)
library(babynames)


# generate the dataset: 7x7 facets, 11 age groups, 100 to 0 variable

# to give some names to our facet let's draw some random names
set.seed(911)
names <- sample(babynames$name %>% unique, 49)

random_100 <- function(name, agr) {
    y = rpois(100, agr^2)
    return(tibble(name, agr, y))
}

df <- crossing(name = names, agr = 1:11) %>%
    pmap_df(.f = random_100) %>%
    group_by(name) %>%
    mutate(y = y %>% scales::rescale(c(0, 100)))


df %>%
    ggplot(aes(agr, 100-y))+
    stat_smooth(color = "#666666", se = F, size = .3)+
    geom_point(color = "#366491", size = .5)+
    scale_x_continuous(breaks = seq(1, 11, 2))+
    scale_y_continuous(breaks = seq(0, 100, 20))+
    facet_wrap(~name, ncol = 7)+
    theme_classic(base_family = font_rc)+
    theme(
        strip.background = element_blank(),
        plot.background = element_rect(color = "#666666", size = .5, fill = NA)
    )+
    labs(
        x = "Age group",
        y = "Percent"
    )

gg <- last_plot()

canvas <- ggplot()+
    labs(
        title = "No, the figure is aligned properly on the page. No, your table is not swinging",
        caption = "#30DayChartChallenge 6 Experiment | @ikashnitsky"
    )+
    theme_classic(base_family = font_rc)+
    theme(
        axis.line = element_blank(),
        plot.title = element_text(face = 2)
    )

out <- ggdraw(canvas)+
    draw_plot(gg, x = .01, width = .98, y = .04, height = .92)

ggsave(
    "out/06-experiment.png", out,
    width = 9, height = 7, type = "cairo-png"
)
