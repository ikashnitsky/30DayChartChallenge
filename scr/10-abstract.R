#===============================================================================
# 2021-04-10 -- 30dcc
# 10. Abstract. Word cloud from DemRes abstracts
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(cowplot)
library(tidytext)
library(wordcloud2)

# The data manually downloaded from Scopus
# All papers ever published by Demopgraphic Research
demres <- read_csv("dat/10-scopus-demres.csv") %>%
    janitor::clean_names()


# Idea weight words by paper citations per year, add one to not exclude
# non-cited papers

df <- demres %>%
    transmute(
        # remove the last part after © with author's names
        abstract = abstract %>% str_remove_all("©.*$"),
        weight = cited_by / (2021-year) + 1
    ) %>%
    unnest_tokens(word, abstract) %>%
    # filter out stop words
    filter(!word %in% stop_words$word) %>%
    # filter out the abstract structure subheadings
    filter(
        !word %in% c(
            "background", "objective", "methods",
            "results", "conclusions", "contribution"
        )
    ) %>%
    # filter out all entries that contain numbers
    filter(!str_detect(word, "[0-9]+")) %>%
    # calculate weighted words
    group_by(word) %>%
    mutate(n = n()) %>%
    summarise(frec = (weight*n) %>% mean(na.rm = TRUE))

# slice most frequent
df_plot <- df %>%
    arrange(frec %>% desc) %>%
    slice(1:200)

# plot
set.seed(126)
wordcloud2(
    # hardcoded fix, #1 word is not ploltted properly
    df_plot %>% add_row(word = "Demographic Research", frec = 10e3),
    color = "random-light",
    backgroundColor = "#444444",
    fontFamily = "Roboto Slab",
    shape = "circle"
)

# saved manually using the Export in View Pane
