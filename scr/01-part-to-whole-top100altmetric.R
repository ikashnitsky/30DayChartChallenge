#===============================================================================
# 2021-04-01 -- 30dcc
# 1. Part-to-whole
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

# Visualize Altmetric top-100 papers using ternary diagrams -- contributions to the overall score by Twitter, News, and everything else.

library(tidyverse)
library(magrittr)
library(ggdark)
library(ggtext)
library(glue)
library(hrbrthemes)
library(cowplot)


load("~/github/top100altmetric/dat/raw.rda")
load("~/github/top100altmetric/out/altmetric_weights.rda")

top19 <- top100[["2019"]] %>%
    janitor::clean_names()

top19ctb <- top19 %>%
    transmute(
        title,
        news = news_mentions,
        blog = blog_mentions,
        policy = policy_mentions,
        twitter = twitter_mentions,
        patent = patent_mentions,
        wikipedia = wikipedia_mentions,
        peer_review = peer_review_mentions,
        weibo = weibo_mentions,
        google = google_mentions,
        linked_in = linked_in_mentions,
        reddit = reddit_mentions,
        pinterest = pinterest_mentions,
        f1000 = f1000_mentions,
        q_a = q_a_mentions,
        video = video_mentions,
        syllabi = syllabi_mentions
    ) %>%
    pivot_longer(cols = news:syllabi, names_to = "source") %>%
    left_join(altmetric_weights) %>%
    mutate(
        value = value %>% replace_na(0),
        wt_score = value * weight
    ) %>%
    mutate(
        tern = source %>% as_factor() %>%
            fct_collapse(
                news = "news", twitter = "twitter", other_level = "other"
            )
    ) %>%
    group_by(title, tern) %>%
    summarise(score = wt_score %>% sum(na.rm = T)) %>%
    ungroup() %>%
    group_by(title) %>%
    transmute(tern, ctb = score %>% prop.table) %>%
    ungroup() %>%
    pivot_wider(names_from = tern, values_from = ctb)


df <- top19 %>% left_join(top19ctb, by = "title")



library(ggtern)
library(tricolore)

# nice own dark theme for ggtern
devtools::source_gist("bc775488577325e218729b250c876e0b")

# calculate ternary colors
tern <- Tricolore(
    df, p1 = "news", p2 = "other", p3 = "twitter",
    show_data = F, breaks = 5,
    contrast = .5, lightness = 1, chroma = 1, hue = 10/12
)

# add hex colors to the dataset
df$color <- tern$rgb

# save the output dataframe temporaily
save(df, file = "tmp/01-temp.rda")

# color legend
tern$key +
    tern_limit(T = 1.1, L = 1.1, R = 1.1)+
    geom_Tline(Tintercept = 0, color = 5)+
    geom_Rline(Rintercept = 0, color = 6)+
    geom_Lline(Lintercept = 0, color = 7)+
    geom_point(
        data = df,
        aes(news, other, z = twitter, size = altmetric_attention_score),
        shape = 1
    )+
    scale_size_area()+
    own_tern_theme

gg_tern <- last_plot()

ggsave(
    "tmp/01-element-tern.pdf", gg_tern,
    width = 5, height = 4, device = cairo_pdf
)



# Restart the session to get rid of ggtern
rstudioapi::restartSession()
pacman::p_unload("all")

library(tidyverse)
library(magrittr)
library(ggdark)
library(ggtext)
library(glue)
library(hrbrthemes)

load("tmp/01-temp.rda")


df %>%
    ggplot(aes(seq_along(title), altmetric_attention_score,
               color = color, size = altmetric_attention_score))+
    geom_point()+
    scale_size_area(max_size = 10)+
    scale_color_identity()+
    scale_y_comma(limits = c(0, 15000))+
    scale_x_continuous(breaks = c(1, 25, 50, 100))+
    dark_theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "none",
        plot.title = element_markdown(family = "Roboto Slab"),
        plot.subtitle = element_markdown()
    )+
    labs(
        title = "It's all about <span style = 'color:#28E2E5;'>Twitter</span> and <span style = 'color:#CD0BBC;'>Newspapers</span>",
        subtitle = "Contribution of <span style = 'color:#28E2E5;'>Twitter</span>, <span style = 'color:#CD0BBC;'>News articles</span>,<br>and <span style = 'color:#F5C710;'>Other</span> sources<br>into the attention score<br>of the Top-100<br>noticed papers of 2019",
        caption = "\nData: Altmetric | Design: @ikashnitsky",
        x = "Rank of the papers",
        y = "Altmetric attention score"
    )

gg_dots <- last_plot()


ggsave(
    "tmp/01-element-dots.pdf", gg_dots,
    width = 7, height = 6, device = cairo_pdf
)
