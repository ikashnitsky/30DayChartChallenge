#===============================================================================
# 2021-04-19 -- 30dcc
# 19. Global change. Global male life expectancy convergence
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(ggdark)
library(wpp2015)
library(ggridges)

# teaser -- wpp convergence in male e0 ------------------------------------

# get the UN country names
data(UNlocations)

countries <- UNlocations %>% pull(name) %>% paste

# data on male life expectancy at birth
data(e0M)

e0M %>%
    filter(country %in% countries) %>%
    select(-last.observed) %>%
    gather(period, value, 3:15) %>%
    ggplot(aes(x = value, y = period %>% fct_rev()))+
    geom_density_ridges(aes(fill = period), color = "#eaeaea", size = .25)+
    geom_vline(xintercept = 0, color = "#eaeaea")+
    scale_fill_viridis_d(
        option = "mako", begin = .2
    )+
    scale_x_continuous(position = "top", expand = c(0,0))+
    labs(
        x = NULL, y = NULL,
        title = "Global convergence in mortality",
        subtitle = "Male life expectancy at birth",
        caption = "Data: UN WPP 2015 | Design: @ikashnitsky"
    )+
    dark_theme_minimal(base_family =  font_rc, base_size = 14)+
    theme(
        legend.position = "none",
        plot.title = element_text(family = "Roboto Slab", size = 20, face = 2),
        axis.text = element_text(face = 2),
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.grid.minor.x  = element_blank(),
        panel.grid = element_line(color = "#eaeaea77")
    )

ggsave(
    "out/19-global-change-mortality.png",
    width = 6, height = 4.5, type = "cairo"
)


# females

# data on male life expectancy at birth
data(e0F)

e0F %>%
    filter(country %in% countries) %>%
    select(-last.observed) %>%
    gather(period, value, 3:15) %>%
    ggplot(aes(x = value, y = period %>% fct_rev()))+
    geom_density_ridges(aes(fill = period), color = "#eaeaea", size = .25)+
    geom_vline(xintercept = 0, color = "#eaeaea")+
    scale_fill_viridis_d(
        option = "rocket", begin = .2
    )+
    scale_x_continuous(position = "top", expand = c(0,0))+
    labs(
        x = NULL, y = NULL,
        title = "Global convergence in mortality",
        subtitle = "Female life expectancy at birth",
        caption = "Data: UN WPP 2015 | Design: @ikashnitsky"
    )+
    dark_theme_minimal(base_family =  font_rc, base_size = 14)+
    theme(
        legend.position = "none",
        plot.title = element_text(family = "Roboto Slab", size = 20, face = 2),
        axis.text = element_text(face = 2),
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.grid.minor.x  = element_blank(),
        panel.grid = element_line(color = "#eaeaea77")
    )

ggsave(
    "out/19-global-change-mortality-female.png",
    width = 6, height = 4.5, type = "cairo"
)
