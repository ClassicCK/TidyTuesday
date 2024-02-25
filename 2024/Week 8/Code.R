# load dependencies
library(ggplot2)
library(showtext)
library(tidyverse)
library(ggtext)
library(plyr)
library(extrafont)

# ------ Data Manipulation ------ 
# load data
data <- tidytuesdayR::tt_load('2024-02-20')

grants <- data$isc_grants

cumulative <- grants %>%
  select(year, group, funded) %>%
  arrange(group, year) %>%
  group_by(year, group) %>%
  mutate(cumulative_funded = cumsum(funded)) %>%
  filter(cumulative_funded == max(cumulative_funded)) %>%
  select(year, group, cumulative_funded) %>%
  ungroup()

cumulative$group <- as.factor(cumulative$group)

# ------ Typography ------ 
grant_colors <- c("1" = "#B5E5CF", "2" = "#B99095")

font_add_google("Rubik", "title_font")
font_add_google("Roboto Mono", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 
title_text <- "The #rstats behind ISC Grants"
subtitle_text <- "Since 2016, the R Consortium Infrastructure Steering Committee (ISC) has awarded 
more grant money in the <b><span style='color:#B99095;'>Fall</span></b> funding cycle than 
in the <b><span style='color:#3D5B59;'>Spring</span></b> towards open-source projects advancing 
technical and social infrastructure."

caption_text <- c("Graphic: Christopher L. Kilner, PhD", "@ClassicCK", "Data: R Consortium")

# ------ Plot ------ 
ggplot(cumulative, aes(x = year, y = cumulative_funded, color = group)) +
  geom_step(size = 2) + 
  scale_color_manual(values = c("1" = "#3D5B59", "2" = "#B99095")) +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = "M", prefix = "$"),
    breaks = seq(0, 1400000, by = 300000)) +
  annotate("curve", x = 2017.6, xend = 2018, y = 1050000, yend = 920000,
           curvature = -0.25,
           arrow = arrow(length = unit(0.2, 'cm'))) +
  annotate("text", x = 2016, y = 1050000, hjust = 0,
           label = "Jump in Funding Due To \nTwo Awarded $50K Grants", fontface = 2, family= body_font) + 
  labs(title = title_text, 
       subtitle = subtitle_text,
       caption = caption_text) +
  theme_minimal() +
  theme(
    # Axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = body_font, 
                               face = "bold",
                               size=14),
    axis.text.y = element_text(family = body_font, 
                               face = "bold",
                               size=14),
    
    # Legend
    legend.position = "none",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(20, 0, 5, 0),
                                 size = 24,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(75, "lines")),
    # SUB-TITLE
    plot.subtitle = element_textbox(margin = margin(5, 0, 30, 0),
                                    size = 18,
                                    color = "grey30",
                                    family = body_font,
                                    width = unit(52, "lines"), 
                                    lineheight = 1.1),
    
    # Caption
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = c(0, 0.5, 1),
                                family=body_font,
                                face="plain",
                                size=10, 
                                color="grey70",
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color="#f8f8f8", fill="#f8f8f8"),
    plot.margin = margin(20, 40, 20, 40)) +
  guides(colour = guide_legend(nrow = 1))

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("2024/Week 8/Twitter.png", height = 9, width = 12, dpi=320)  
showtext_auto(FALSE)
