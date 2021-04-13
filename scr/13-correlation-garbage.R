#===============================================================================
# 2021-04-13 -- 30dcc
# 13. Correlation. Litter collection by school kids
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(ggdark)

# data. Litter collected by kids of Henriette Hoerluchs School on 2021-04-12
garbage <- tibble::tribble(
                    ~class, ~weight,
                      "0A",  "51.5",
                      "0B",  "10.5",
                      "1A",  "6.14",
                      "1B",   "3.6",
                      "2A", "16.62",
                      "2B", "17.23",
                      "3A",   "6.7",
                      "3B",   "5.1",
                      "4A",  "30.2",
                      "4B",  "44.8",
                      "5A",  "37.7",
                      "5B",  "44.2",
                      "6A",  "51.0",
                      "6B",  "31.5",
                      "7A",  "45.7",
                      "7B",  "36.2",
                      "8A",  "50.5",
                      "8B",  "11.8",
                      "9A",  "28.4",
                      "9B",   "4.0",
               "Preschool", "4.549",
                      "P1", "1.273",
                      "P2",  "1.87",
                   "P3+P4",   "7.8",
                      "P5",  "14.7",
                      "P6",  "31.4",
                      "S7",  "79.0",
                      "S8",     "-",
                      "S9",  "20.2",
                     "S10",  "97.0",
                     "S11",  "13.0"
               )

# clean up data
clean <- garbage %>%
    mutate(
        part = case_when(
            str_sub(class, 1, 1) %in% 0:9 ~ "dan",
            TRUE ~ "int"
        ),
        age = class %>%
            str_extract("\\(?[0-9,.]+\\)?") %>%
            replace_na(0) %>%
            as.numeric,
        weight = weight %>% as.numeric
    ) %>%
    drop_na()

clean %>%
    ggplot(aes(age, weight))+
    stat_smooth(method = "lm", se = FALSE, color = 8)+
    geom_point(aes(color = part))+
    geom_text(
        data = . %>% select(part) %>% distinct(),
        aes(color = part),
        label = c("Danish part", "International part"),
        x = c(3, 11), y = c(35, 70), hjust = 1, vjust = 0,
        size = 4, family = "Roboto Slab"
    )+
    scale_color_manual(values = c(5, 7), guide = NULL)+
    scale_x_continuous(breaks = 0:11)+
    dark_theme_minimal(base_family = font_rc)+
    theme(
        plot.title = element_text(family = "Roboto Slab", face = 2)
    )+
    labs(
        x = "School class",
        y = "Collected litter, kg",
        title = "Litter collected by school students",
        subtitle = "2021-04-11, Henriette Hoerluchs School, Odense, Denmark",
        caption = "#30DayChartChallenge 13 Correlation | @ikashnitsky"
    )

ggsave(
    "out/13-correlation-litter.png",
    width = 5, height = 4, type = "cairo-png"
)
