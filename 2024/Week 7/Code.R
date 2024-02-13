# load dependencies
library(ggplot2)
library(showtext)
library(tidyverse)
library(ggtext)
library(ggbump)
library(plyr)
library(cowplot)

# ------ Data Manipulation ------ 
# load data
data <- tidytuesdayR::tt_load('2024-02-13')

historical_spending <- data$historical_spending
gifts_age <- data$gifts_age
gifts_gender <- data$gifts_gender

rank_data <- gifts_age[,-2] %>%
  pivot_longer(cols = -Age, names_to = "Category", values_to = "Percentage") %>%
  mutate(Category = sub("GreetingCards", "Greeting Cards", Category)) %>%
  mutate(Category = sub("GiftCards", "Gift Cards", Category)) %>%
  mutate(Category = sub("EveningOut", "Evening Out", Category)) %>%
  group_by(Age) %>%
  mutate(rank = rank(-Percentage, ties.method = "random")) %>% 
  ungroup()

order <- rank_data[rank_data$Age == "65+",4]

# Create the initial data frame
gender_data <- gifts_gender[,-2] %>%
  pivot_longer(cols = -Gender, names_to = "Category", values_to = "Percentage") %>%
  mutate(
    Category = sub("GreetingCards", "Greeting Cards", Category),
    Category = sub("GiftCards", "Gift Cards", Category),
    Category = sub("EveningOut", "Evening Out", Category),
    rank = c(order$rank, order$rank),
    Percentage = ifelse(Gender == "Men", Percentage * -1, Percentage * 1),
    SymbolCount = floor(abs(Percentage / 5)),  # Calculate the number of symbols needed for each percentage
    Character = ifelse(Gender == "Men", "p", "u")
  ) %>%
  ungroup()

gender_data_expanded <- ddply(gender_data, .(Category, rank), function(d) {
  # Calculate the number of symbols for males and females
  male = sum(d$SymbolCount[d$Gender == "Men"])
  female = sum(d$SymbolCount[d$Gender == "Women"])
  
  # Create sequences for males and females
  male_seq = seq(-male, -1)
  female_seq = seq(1, female)
  
  # Combine the male and female sequences and data
  data.frame(Gender = c(rep("Men", male), rep("Women", female)),
             x = c(male_seq, female_seq), 
             rank = d$rank[1])
}) %>%
  mutate(Percentage = ifelse(Gender == "Men", x + 1, x),
         Character = ifelse(Gender == "Men", "p", "u"))

valentine_colors <- c("Candy" = "#CB76BC",          # Sweet pink
                      "Flowers" = "#9A162E",        # Floral pink
                      "Jewelry" = "#DD385D",        # Rose gold
                      "Greeting Cards" = "#F46A8A",  # Cream
                      "Evening Out" = "#FB96AC",      # Burgundy
                      "Clothing" = "#FFC1CE",        # Soft red
                      "Gift Cards" = "#FDCAEC")       # Silver
  
# ------ Typography ------ 

font_add_google("Pacifico", "title_font")
font_add_google("Roboto Mono", "body_font")
link = "http://img.dafont.com/dl/?f=wm_people_1";
download.file(link, "wmpeople1.zip", mode = "wb");
unzip("wmpeople1.zip");
font_add("wmpeople1", "wmpeople1.TTF")
font_add('fa6-brands', '/Users/Christopher/Downloads/fontawesome-free-6.5.1-web/webfonts/fa-brands-400.ttf') 
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# Load the image
twitter_logo <- png::readPNG("/Users/Christopher/Downloads/x-twitter.png")
twitter_grob <- grid::rasterGrob(twitter_logo, interpolate = TRUE)

github_logo <- png::readPNG("/Users/Christopher/Downloads/github.png")
github_grob <- grid::rasterGrob(github_logo, interpolate = TRUE)

# ------ Texts ------ 
title_text <- "<b><span style='color:#FB96AC;'>Oh, to be </span></b><b><span style='color:#9A162E;'>Young </span></b><b><span style='color:#FB96AC;'>and </span></b><b><span style='color:#9A162E;'>in Love</span></b>"
subtitle_text <- "As preferences shift with age, the honeymoon of <b><span style='color:#DD385D;'>Jewelry</span></b> gives 
way to the practicality of <b><span style='color:#F46A8A;'>Greeting Cards</span></b>, 
while <b><span style='color:#CB76BC;'>Candy</span></b> maintains its sweet spot as the gift of choice for <b><span style='color:#7CB8D7;'>men</span></b>
and <b><span style='color:#F068A8;'>women</span></b> across all generations."

caption_text <- c("Graphic: Christopher L. Kilner, PhD", "@ClassicCK", "Data: National Retail Federation")

# ------ Plot ------ 
p2 <- ggplot(gender_data_expanded, aes(x = Percentage, y = rank)) +
  geom_text(aes(label = Character, color = Gender),
            family = "wmpeople1", size = 7) +
  scale_color_manual(values = c("Men" = "#7CB8D7", "Women" = "#F068A8")) +
  scale_y_reverse() +
  labs("Percentage", y = "Rank", color = "Gender") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste0(abs(x) * 5, "%")) +
  theme_void() +
  labs(x = "Valentines") +
  theme(
    # Axis titles
    axis.title.x = element_text(size = 12, color = "grey30", family = body_font, margin = margin(t = 10)),
    axis.title.y = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = body_font, 
                               face = "bold",
                               size=14),
    axis.text.y = element_blank(),
    
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
    plot.caption = element_text(hjust = c(0, 0.5, 1),
                                family=body_font,
                                face="plain",
                                size=10, 
                                color="grey70",
                                margin=margin(10,0,0,0)),
    
    plot.background = element_rect(color="#f8f8f8", fill="#f8f8f8"),
    plot.margin = margin(0, 10, 10, 15)) + 
  guides(colour = guide_legend(nrow = 1))



p1 <- ggplot(rank_data, aes(x = Age, y = rank, group = Category, color = Category)) +
  geom_bump(aes(Age, rank, group = Category, color = Category), 
    smooth = 14, size = 7.2) + 
  geom_label(data = rank_data %>% filter(Age == "65+", Category == "Candy"),
             aes(x = Age, y = rank, fill = Category, label = Category), color = "#CB76BC", family = body_font, hjust = 1, nudge_x = 0.38, size = 4,
             label.size = 0) +
  geom_label(data = rank_data %>% filter(Age == "65+", Category == "Clothing"),
             aes(x = Age, y = rank, fill = Category, label = Category), color = "#FFC1CE", family = body_font, hjust = 1, nudge_x = 0.5, size = 4,
             label.size = 0) +
  geom_label(data = rank_data %>% filter(Age == "65+", Category == "Flowers"),
             aes(x = Age, y = rank, fill = Category, label = Category), color = "#9A162E", family = body_font, hjust = 1, nudge_x = 0.25, size = 4,
             label.size = 0) +
  geom_label(data = rank_data %>% filter(Age == "65+", Category == "Jewelry"),
             aes(x = Age, y = rank, fill = Category, label = Category), color = "#DD385D", family = body_font, hjust = 1, nudge_x = 0.25, size = 4,
             label.size = 0) +
  geom_label(data = rank_data %>% filter(Age == "18-24"),
             aes(x = Age, y = rank, fill = Category, label = Category), color = "#f8f8f8", family = body_font, hjust = 0, nudge_x = -0.25, size = 4,
             label.size = 0) +
  geom_label(data = rank_data %>% filter(Age == "65+"),
             aes(x = Age, y = rank, fill = Category, label = Category), color = "#f8f8f8", family = body_font, hjust = 1, nudge_x = 0.78, size = 4,
             label.size = 0) +
  theme_void() +
  scale_y_reverse() +
  scale_color_manual(values = valentine_colors) +
  scale_fill_manual(values = valentine_colors) +
  labs(y = paste0("<- Less Popular", strrep(" ", 6), "Gifts", strrep(" ", 6), "More Popular ->"),
       x = paste0("<- Younger", strrep(" ", 15), "Cupid's Age", strrep(" ", 15), "Older ->")) +
  theme(
    # Axis titles
    axis.title.x = element_text(size = 12, color = "grey30", family = body_font, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, color = "grey30", angle = 90, family = body_font), #margin = margin(r = 10)),
    
    # Axis text
    axis.text.x = element_text(family = body_font, 
                               face = "bold",
                               size=14),
    axis.text.y = element_blank(),
    
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
    plot.caption = element_text(hjust = c(0, 0.5, 1),
                                family=body_font,
                                face="plain",
                                size=10, 
                                color="grey70",
                                margin=margin(10,0,0,0)),
    
    plot.background = element_rect(color="#f8f8f8", fill="#f8f8f8"),
    plot.margin = margin(0, 0, 10, 10)) +
  guides(colour = guide_legend(nrow = 1))

plots <- plot_grid(p1, p2, nrow = 1, rel_widths = c(3, 1))

plots + 
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text) +
  theme(
    # Axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, color = "grey30", angle = 90, family = body_font, margin = margin(r = 10)),
    
    # Axis text
    axis.text.x = element_text(family = body_font, 
                               face = "bold",
                               size=14),
    axis.text.y = element_blank(),
    
    # Legend
    legend.position = "none",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(20, 0, 5, 165),
                                 size = 32,
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
    plot.caption = element_text(hjust = c(0, 0.5, 1),
                                family=body_font,
                                face="plain",
                                size=10, 
                                color="grey70",
                                margin=margin(10,0,0,0)),
    
    plot.background = element_rect(color="#f8f8f8", fill="#f8f8f8"),
    plot.margin = margin(10, 10, 10, 10)) +
  guides(colour = guide_legend(nrow = 1)) +
  annotation_custom(twitter_grob, xmin = 0.447, xmax = 0.467, ymin = -0.022, ymax = -0.042) +
  annotation_custom(github_grob, xmin = 0.533, xmax = 0.553, ymin = -0.022, ymax = -0.042)


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("2024/Week 7/Twitter.png", height = 9, width = 12, dpi=320)  
showtext_auto(FALSE)
