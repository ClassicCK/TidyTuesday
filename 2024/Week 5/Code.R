# load dependencies
library(ggplot2)
library(showtext)
library(tidyverse)
library(ggtext)

# function to set political party in presidency during year
get_presidential_party <- function(year) {
  if (year >= 1887 & year < 1889 | year >= 1897 & year < 1913 | year >= 1921 & year < 1923 | 
      year >= 1929 & year < 1933 | year >= 1953 & year < 1961 | year >= 1969 & year < 1974 | 
      year >= 1981 & year < 1993 | year >= 2001 & year < 2009 | year >= 2017 & year < 2021) {
    return('Republican')
  } else if (year >= 1889 & year < 1897 | year >= 1913 & year < 1921 | year >= 1923 & year < 1929 |
             year >= 1933 & year < 1953 | year >= 1961 & year < 1969 | year >= 1974 & year < 1981 | 
             year >= 1993 & year < 2001 | year >= 2009 & year < 2017 | year >= 2021 & year <= 2023) {
    return('Democrat')
  } else {
    return(NA)  # For any unexpected year
  }
}

# ------ Data Manipulation ------ 
# load data
data <- tidytuesdayR::tt_load('2024-01-30')


# merge data for analysis and tidy up NA's
groundhog.data <- merge(data$predictions, data$groundhogs, by = "id") %>%
  filter(!is.na(shadow)) %>%
  filter(type == "Groundhog") %>%
  president_party = sapply(year, get_presidential_party) %>%
  group_by(year) %>%
  summarise(shadow_seen = sum(shadow == TRUE, na.rm = TRUE),
    total = n(),
    proportion = shadow_seen / total,
    president = first(president_party)) %>%
  filter(total > 1) %>%
  ungroup()

# create two datasets, one for each party
dem_data <- groundhog.data %>% filter(president == "Democrat")
rep_data <- groundhog.data %>% filter(president == "Republican")

# calculate mean/median probabilities for each party
dem_prob <- dem_data %>%
  summarise(mean_prob = mean(proportion),
            median_prob = median(proportion))

rep_prob <- rep_data %>%
  summarise(mean_prob = mean(proportion),
            median_prob = median(proportion))

# calculate density for each party

densities <- do.call(rbind, lapply(split(groundhog.data, groundhog.data$president), function(subdata) {
  d <- density(subdata$proportion)
  data.frame(president = unique(subdata$president),
             median = median(subdata$proportion),
             mean = mean(subdata$proportion),
             density_median = approx(d$x, d$y, xout = median(subdata$proportion))$y,
             density_mean = approx(d$x, d$y, xout = mean(subdata$proportion))$y)
}))

density_dem <- densities[1,]
density_rep <- densities[2,]
density_rep$density_median <- density_rep$density_median * -1
density_rep$density_mean <- density_rep$density_mean * -1

# ------ Typography ------ 

font_add_google("Rubik", "title_font")
font_add_google("Roboto Mono", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Shadows of Partisanship: Groundhog Day in America 2024"
subtitle_text <- "Groundhogs Gauge the Political Climate: 6 degrees of Warning? Under <b><span style='color:#0015BC;'>Democratic Presidents</b></span>, Punxsutawney Phil pumps the breaks on warming, while those <b><span style='color:#e9141d;'>Republican regimes</b></span> hit that greenhouse gas accelerator."
caption_text <- c("Graphic: Christopher L. Kilner, PhD", "Data: groundhog-day.com")

# ------ Plot ------ 
ggplot() +
  geom_density(data = dem_data, aes(x = proportion, y = ..density..), fill = "#0015BC", color = "#0015BC", alpha = 0.5) +
  geom_density(data = rep_data, aes(x = proportion, y = -..density..), fill = "#e9141d", color = "#e9141d", alpha = 0.5) +
  geom_segment(data = density_dem, aes(x = mean, xend = mean, y = 0, yend = density_mean), color = "#0015BC", linetype = "solid") +
  geom_segment(data = density_rep, aes(x = mean, xend = mean, y = 0, yend = density_mean), color = "#e9141d", linetype = "solid") +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x = paste0("← Spring is Sprung", strrep(" ", 30), "Probability of Seeing its Shadow", strrep(" ", 30), "Winter is Coming →"),
       y = "Density") +
  theme_minimal() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), color = "grey", linetype = "solid") +
  theme(
    # Axis titles
    axis.title.x = element_text(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 10)),
    axis.title.y = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = body_font, 
                               face = "bold",
                               size=12),
    axis.text.y = element_text(family = body_font, 
                               face = "bold",
                               size=12),
    
    # Legend
    legend.position = "none",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(20, 0, 5, 0),
                                 size = 20,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(45, "lines")),
    # SUB-TITLE
    plot.subtitle = element_textbox(margin = margin(5, 0, 30, 0),
                                    size = 14,
                                    color = "grey30",
                                    family = body_font,
                                    width = unit(42, "lines")),
    
    # Caption
    plot.caption = element_text(hjust = c(0, 1),
                                family=body_font,
                                face="plain",
                                size=9, 
                                color="grey70",
                                margin=margin(10,0,0,0)),
    
    plot.background = element_rect(color="#f8f8f8", fill="#f8f8f8"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  scale_y_continuous(labels = abs, breaks = waiver())

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("week-05.png", height = 9,
       width = 16, dpi=320)  
showtext_auto(FALSE)



