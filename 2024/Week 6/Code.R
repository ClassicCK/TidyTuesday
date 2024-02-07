# load dependencies
library(ggplot2)
library(showtext)
library(tidyverse)
library(ggtext)
library(grid)
library(magrittr)
library(ggparliament)
library(ggpol)
library(ggplotify)

# ------ Data Manipulation ------ 
# load data
data <- tidytuesdayR::tt_load('2024-02-06')

heritage <- data$heritage

# Set the main color of the country flags with and without transparency
colors <- c("Denmark" = "#C8102E", "Norway" = "#00205B", "Sweden" = "#fecc00")
colors_transparent <- c("Denmark" = "#C8102E80", "Norway" = "#00205B80", "Sweden" = "#fecc0080")

# Calculate the difference for 2022
heritage$`2022` <- heritage$`2022` - heritage$`2004`

# Now proceed with your long format transformation and plotting
heritage_long <- heritage %>%
  pivot_longer(cols = -country, names_to = "year", values_to = "seats") %>%
  mutate(
    party = as.factor(country),
    color = ifelse(year == "2004", colors_transparent[country], colors[country]), 
    party_short = as.factor(toupper(substr(party, 1, 3)))
  )

# ------ Typography ------ 

font_add_google("Rubik", "title_font")
font_add_google("Roboto Mono", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Nordic World Heritage Sites"
subtitle_text <- "Change in the Nordic Parliament: While <b><span style='color:#fecc00;'>Sweden</b></span> retains the 
most UNESCO World Heritage Sites, as of 2022 <b><span style='color:#C8102E;'>Denmark</b></span> and 
<b><span style='color:#00205B;'>Norway</b></span> now account for a majority (55%) of World Heritage Sites, 
with <b><span style='color:#C8102E;'>Denmark</b></span> seeing a <b><span style='color:#C8102E;'>150%</b></span> 
increase between 2004 and 2022 in these sites deisgnated for their cultural, historical, scientific, or other forms of signficance."
caption_text <- c("Graphic: Christopher L. Kilner, PhD", "@ClassicCK", "Data: UNESCO")

# ------ Plot ------ 
# Main plot
main_plot <- ggplot(heritage_long) + 
  geom_parliament(aes(seats = seats, fill = party, color = color)) + 
  scale_fill_manual(values = heritage_long$color, labels = heritage_long$party) +
  scale_color_manual(values = heritage_long$color, labels = heritage_long$party) +
  coord_fixed() + 
  theme_void()  +
  theme(legend.position = "none") + labs(title = title_text,
                                         subtitle = subtitle_text,
                                         caption = caption_text) +
  theme(
    # Axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    
    # Axis text
    axis.text.x = element_blank(),
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
    
    plot.background = element_rect(color="#f8f8f8", fill="#f8f8f8"))# +
    # annotate(geom = "curve", 
    #        x = -1.2, y = -0.2, xend = 1.2, yend = -0.2, 
    #        curvature = -1.25, size = 1, color = "grey70", linetype = "dashed")

# Legend 
# Define the colors and countries
countries <- c("DEN", "NOR", "SWE")
colors <- c("#C8102E", "#00205B", "#fecc00") # Replace with actual flag colors in hex
colors_transparent <- c("#C8102E80", "#00205B80", "#fecc0080")


# Create a data frame for the legend
legend_data <- expand.grid(country = countries, year = c("2004", "2022")) %>%
  mutate(
    color = ifelse(year == "2004", 
                   setNames(colors_transparent, countries)[country], 
                   setNames(colors, countries)[country]),
    x = match(country, countries), # x position
    y = as.numeric(year) # y position
  )

# Order factors for plotting
legend_data$country <- factor(legend_data$country, levels = countries)
legend_data$year <- factor(legend_data$year, levels = c("2004", "2022"))

# Plot the custom legend
legend_plot <- ggplot(legend_data, aes(x = country, y = year, fill = I(color))) +
  geom_point(shape = 21, size = 8, color = legend_data$color) + 
  scale_x_discrete(limits = countries) + # Set discrete limits for countries
  scale_y_discrete(limits = c("2004", "2022")) + # Set discrete limits for years
  guides(fill = FALSE) + # No need for a fill guide
  theme_minimal() + # A minimal theme
  theme( # Axis titles
    axis.title.x = element_text(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 10)),
    axis.title.y = element_blank(),
    
    # Axis text
    axis.text.x = element_text(family = body_font, 
                               face = "bold",
                               size=14),
    axis.text.y = element_text(family = body_font, 
                               face = "bold",
                               size=14),
    legend.position = "none", # Remove default legend
    axis.ticks = element_blank(), # Remove axis ticks
    plot.margin = unit(c(1, 1, 1, 1), "lines"), # Adjust plot margin
    panel.grid.major = element_blank(), # No grid lines
    panel.grid.minor = element_blank(), # No grid lines
    panel.background = element_rect(fill = '#f8f8f8', colour = '#f8f8f8') # White background for the plot panel
  ) +
  labs(x = NULL, y = NULL) # Remove axis titles

# Convert plots to grobs
main_plot_grob <- ggplotify::as.grob(main_plot)
legend_plot_grob <- ggplotify::as.grob(legend_plot)

# Set Location
xmin <- unit(0.35, "npc")  # Normalized Parent Coordinates (0 = left, 1 = right)
xmax <- unit(0.65, "npc")  # Adjust as needed
ymin <- unit(0, "npc")  # 1 = top, 0 = bottom
ymax <- unit(0.25, "npc")    # Adjust as needed

# Create a new ggplot with the main plot grob and add the legend grob on top
combined_plot <- ggplot() + 
  annotation_custom(main_plot_grob) +
  annotation_custom(legend_plot_grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +
  theme_void() + 
  theme(plot.background = element_rect(color="#f8f8f8", fill="#f8f8f8"),
                                       plot.margin = margin(20, 20, 20, 20))

# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("2024/Week 6/Twitter.png", height = 9, width = 12, dpi=320)  
showtext_auto(FALSE)
