# Load packages
library(tidyverse)
library(janitor)
library(here)
library(showtext)
library(ggtext)
library(glue)
library(sf)
library(rnaturalearth)
library(patchwork)

# Question:
# Which hazards/storms in Puerto Rico are most frequent, and which events cause the most property damage per event?

# Data Loading & Wrangling
# Save all data files
storm_files <- list.files(
  path = "C:/Documents/MEDS/EDS240 - data visualization/eds240-infographic/data",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read in all data files and merge as one dataframe
storm_data <- storm_files %>%
  map_dfr(
    ~ read_csv(.x, col_types = cols(.default = col_character()))
  )

# Clean data
# Keep only the events of interest
storm_events <- c("Tropical Depression", "Tropical Storm", 
                  "Hurricane (Typhoon)", "Heavy Rain",
                  "Flood", "Flash Flood", "Coastal Flood",
                  "Drought", "Excessive Heat", "Waterspout")

storm_clean <- storm_data %>% 
  # Convert variables to numeric
  mutate(across(.cols = c(DEATHS_DIRECT, DEATHS_INDIRECT, INJURIES_DIRECT, INJURIES_INDIRECT, DAMAGE_PROPERTY_NUM, DAMAGE_CROPS_NUM, MAGNITUDE, BEGIN_RANGE, END_RANGE), ~ parse_number(.x))) %>%
  
  # Standardize time strings
  mutate(
    BEGIN_TIME = str_pad(BEGIN_TIME, width = 4, side = "left", 
                         pad = "0"),
    END_TIME = str_pad(END_TIME,   width = 4, side = "left", 
                       pad = "0")
  ) %>% 
  
  # Convert dates and derive time features
  mutate(BEGIN_DATE = mdy(BEGIN_DATE),
         END_DATE = mdy(END_DATE),
         YEAR = year(BEGIN_DATE),
         BEGIN_HOUR = as.integer(substr(BEGIN_TIME, 1, 2)),
         END_HOUR = as.integer(substr(END_TIME, 1, 2))) %>% 
  
  # Select only the variables that will be used for analysis
  select(EVENT_ID, EPISODE_ID,
         STATE_ABBR, CZ_NAME_STR, CZ_TYPE, CZ_FIPS,
         EVENT_TYPE, MAGNITUDE, MAGNITUDE_TYPE,
         BEGIN_DATE, END_DATE, YEAR, BEGIN_HOUR, END_HOUR,
         DEATHS_DIRECT, DEATHS_INDIRECT,
         INJURIES_DIRECT, INJURIES_INDIRECT,
         DAMAGE_PROPERTY_NUM, DAMAGE_CROPS_NUM,
         SOURCE, FLOOD_CAUSE
  ) %>% 
  
  # Standardize variable names
  clean_names() %>% 
  
  # Filter for events of interest
  filter(event_type %in% storm_events)

# Plot 4
# Create dataframe to keep only events where median property damage values are greater than 0
storm_summary <- storm_clean %>%
  group_by(event_type) %>%
  summarise(
    events = n(),
    median_damage = median(damage_property_num[damage_property_num > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(median_damage))

# Convert data to long format
storm_long <- storm_summary %>%
  pivot_longer(
    cols = c(events, median_damage),
    names_to = "metric",
    values_to = "value"
  )



#----------------------- Plot-Making------------------------------------------

# Define storm palette
storm_palette <- c(
  events = "#AEB4BC",    
  median_damage = "#0B3C5D")

bg <- "#F4F6F8"

# Add font
font_add_google("Inter", "inter")

# font awesome icon
font_add(family = "fa-brands",
         regular = here::here("fonts", "Font Awesome 7 Brands-Regular-400.otf"))


# title
title <- glue("Storm 
<span style='color:#8A8F98;'>frequency</span> 
vs. 
<span style='color:#0B3C5D;'>property damage</span> 
in Puerto Rico")

subtitle <- "The most frequent events are not always the most damaging."

#icons + username
github_icon <- "&#xf092"
github_username <- "mmorenorolon"


caption <- glue::glue(
  "Data Source: NOAA Storm Events Database<br>
  <span style='font-family:fa-brands fa-square-github;'>{github_icon};</span>
  {github_username}"
)

annotation <-  glue("Economic vulnerability in Puerto Rico 
                    is shaped both by persistent everyday 
                    hazards and by episodic extreme events.")

storm_plot <- ggplot(storm_long, aes(x = value, y = event_type, color = metric)) +
  
  # Connecting line between frequency and damage
  geom_line(aes(group = event_type),
            color = "grey75",
            linewidth = 0.6) +
  
  # Points
  geom_point(size = 3.8) +
  
  # Add log scale to show difference between points
  scale_x_log10(breaks = c(100, 1000, 10000, 100000, 1000000),
                labels = c("100", "1k", "10k", "100k", "1M")) +
  
  scale_y_discrete(
    labels = c("Tropical Storm" = "Tropical storm",
               "Hurricane (Typhoon)" = "Hurricane",
               "Heavy Rain" = "Heavy rain",
               "Flood" = "Flood",
               "Flash Flood" = "Flash flood",
               "Drought" = "Drought",
               "Coastal Flood" = "Coastal flood")) +
  
  # Manual colors
  scale_color_manual(values = storm_palette) +
  
  labs(title = title, subtitle = subtitle, caption = caption, 
       x = NULL, y = NULL)  +
   
  # Annotation
  annotate("text",
           x = 10000,
           y = "Flood",
           label = annotation,
           hjust = 0,
           size = 3.5,
           family = "inter",
           color = "#0B3C5D") +
  
  theme_classic(base_family = "inter", base_size = 17) +
  
  theme(plot.title.position = "plot",

        plot.title = ggtext::element_markdown(family = "inter",
                                          face = "bold",
                                          size = rel(0.98),
                                          lineheight = 1.2),
        
        plot.subtitle = element_text(family = "inter",
                                 size = rel(0.9),
                                 margin = margin(b = 8)),
        
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        
        plot.background = element_rect(fill = bg),
        
        panel.background = element_rect(fill = bg),
        
        axis.text.x = element_text(family = "inter",
                             size = rel(0.7)),
        
        axis.text.y = element_markdown(family = "inter",
                                       size = rel(0.7)),
        
        axis.title.x = element_text(family = "inter",
                                size = rel(0.75), 
                                margin = margin(t = 15)),
        axis.title.y = element_blank(),
        
        plot.caption = ggtext::element_markdown(family = "inter",
                                            face = "italic",
                                            size = rel(0.53)),
        
        legend.position = "none")

showtext_auto(enable = TRUE)
storm_plot
showtext_auto(enable = FALSE)


#..............inset PR map.............

adm1 <- rnaturalearth::ne_download(
  scale = 10,
  type = "admin_1_states_provinces",
  category = "cultural",
  returnclass = "sf"
)

# Filter Puerto Rico (try both name fields)
pr_sf <- adm1 %>%
  filter(name == "Puerto Rico" | name_en == "Puerto Rico")

pr_map <- ggplot(pr_sf) +
  geom_sf(fill = "white", color = "#4E514D", linewidth = 0.4) +
  coord_sf(datum = NA) +
  theme_void()

storm_plot_inset <- storm_plot +
  patchwork::inset_element(
    pr_map,
    left = 0.05, bottom = 0.02, right = 0.3, top = 0.26,
    align_to = "full"
  )

showtext_auto(enable = TRUE)
storm_plot_inset



#..............enable {showtext} for newly opened GD.............
showtext_auto(enable = TRUE)

#...................set resolution to match GD...................
showtext_opts(dpi = 300)

#..............write plot to file (aka save as png)..............
ggsave(
  filename = here::here("images", "storm_plot.png"),
  plot = storm_plot,
  device = ragg::agg_png,
  width = 8, 
  height = 7,
  unit = "in",
  dpi = 300
)

#...............turn off {showtext} text rendering...............
showtext_auto(enable = FALSE)
