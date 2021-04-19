#===============================================================================
# 2021-04-18 -- 30dcc
# 18. Connections. Between top twitter users
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

library(tidyverse)
library(magrittr)
library(lubridate)
library(hrbrthemes)
library(ggdark)
library(ggtext)
library(cowplot)
library(rtweet)
library(rvest)

# define the page to load
top50_raw <- read_html("https://en.wikipedia.org/wiki/List_of_most-followed_Twitter_accounts") %>%
    #list all tables on the page
    html_nodes(css = "table") %>%
    # select the one containing needed key words
    extract2(., str_which(string = . , pattern = "Rank")) %>%
    # convert to a table
    html_table(fill = T) %>%
    janitor::clean_names()

# clean the table
top50 <- top50_raw %>%
    mutate(
        account_name = account_name %>%
            str_remove("@") %>%
            str_remove("\\[[^\\]]*\\]")
    ) %>%
    # remove Trump
    filter(! account_name == "realDonaldTrump")

# top 20 persons, not orgs
top20 <- top50 %>%
    filter(
        account_name %in% c("BarackObama", "justinbieber", "katyperry", "rihanna", "Cristiano", "taylorswift13", "ladygaga", "ArianaGrande", "TheEllenShow", "KimKardashian", "narendramodi", "selenagomez", "jtimberlake", "britneyspears", "ddlovato", "BillGates", "neymarjr", "shakira", "jimmyfallon", "elonmusk")
    )

# a function to use with mutate
follow_each_other <- function(source, target) {
    ilook <- lookup_friendships(source = source, target = target)
    if (dim(ilook) == c(0,0)){follow = NA} else {
        follow <- ilook %>%
            filter(relationship == "source", variable == "following") %>%
            pull(value)
    }
    return(tibble(source = source, target = target, follow = follow))
}

# lookup following table
foo <- crossing(source = top20$account_name, target = top20$account_name) %>%
    filter(! source == target)

# look up 380 connections (took ~15min)
# top20follow <- foo %>% pmap_df(.f = follow_each_other)

# limited to 180 requests in 15 min -- split in parts
part_1 <- foo %>% slice(1:180) %>% pmap_df(.f = follow_each_other) # 00:20
part_2 <- foo %>% slice(181:360) %>% pmap_df(.f = follow_each_other) # 00:36
part_3 <- foo %>% slice(361:380) %>% pmap_df(.f = follow_each_other)

top20follow <- bind_rows(part_1, part_2, part_3)

save(top20follow, file = "dat/top20follow.rda")

load("dat/top20follow.rda")

top20follow %>%
    left_join(
        top20 %>% distinct(account_name, followers_millions),
        c("source" = "account_name")
    ) %>%
    rename(followers_source = followers_millions) %>%
    left_join(
        top20 %>% distinct(account_name, followers_millions),
        c("target" = "account_name")
    ) %>%
    rename(followers_target = followers_millions) %>%
    ungroup() %>%
    mutate(
        source = source %>% as_factor %>%
            fct_reorder(followers_source)
    ) %>%
    mutate(
        target = target %>% as_factor %>%
            fct_reorder(followers_target)%>% fct_rev()
    ) %>%
    # plot
    ggplot(aes(target, source))+
    geom_tile(aes(fill = follow))+
    geom_tile(
        data = . %>% distinct(source),
        aes(x = source), fill = "#08a0e9"
    )+
    geom_text(
        data = . %>% distinct(source, followers_source),
        aes(label = followers_source %>% round, x = source),
        size = 2, color = "#ffffff", fontface = 2
    )+
    scale_fill_manual(
        values = c("#00000000", "#dfcd00"), guide = NULL
    )+
    scale_x_discrete(position = "top")+
    coord_fixed()+
    dark_theme_minimal(base_family = font_rc)+
    theme(
        plot.title = element_markdown(family = "Roboto Slab", face = 2),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5, face = 2, color = "#08a0e9"),
        axis.text.y = element_text(face = 2, color = "#dfcd00"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank()
    )+
    labs(
        # y = "Person is following",
        # x = "Person is followed",
        x = NULL, y = NULL,
        title = "Do top 20 followed persons on<br>Twitter <span style = 'color:#dfcd00;'>follow</span> each other?",
        subtitle = "<br>Follows can be asymmetric.<br><br><span style = 'color:#dfcd00;'>Rows</span> represent the <span style = 'color:#dfcd00;'>source</span> of the follow:<br>reading the matrix per row shows who<br>the person follows.<br><br><span style = 'color:#08a0e9;'>Columns</span> are <span style = 'color:#08a0e9;'>target</span> of the follow:<br>read vertically to see by who among<br>the top-20 the index person in followed.<br><br>Numbers on the diagonal show the total<br>number of <span style = 'color:#08a0e9;'>person's followers</span>, in millions.",
        caption = "<br>Data: Wikipedia, Twitter | Graphic: <span style = 'color:#dfcd00;'>@ikashnitsky</span>"
    )

out <- last_plot()

ragg::agg_png(
    "out/18-connections-twitter.png",
    width = 4*300, height = 7*300,
    background = "#444444", res = 300
)
out
dev.off()

# ggsave(
#     "out/18-connections-twitter.png",
#     width = 4, height = 7, type = "cairo-png"
# )
