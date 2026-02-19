library(tidyverse)
library(janitor)
library(here)
library(showtext)
library(ggtext)
library(glue)
library(gghighlight)


storm_scatter <- storm_clean %>%
  group_by(event_type) %>%
  summarise(
    # Count the number of events per event type
    events = n(),
    # Calculate the median property damage
    median_damage = median(damage_property_num, na.rm = TRUE),
    # Keep only property damage numbers that are not zero
    median_damage_nz = median(damage_property_num[damage_property_num > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter out any nonzero values
  filter(!is.na(median_damage_nz))

  
ggplot(storm_scatter,
       aes(x = events, y = median_damage_nz, label = event_type)) +
  
  geom_point() +
  
  gghighlight(
    event_type %in% c("Hurricane (Typhoon)", "Drought"),
    use_direct_label = TRUE,
    label_key = event_type) +
  
  # Set log scale and add a dollar format for values in the y-axis
  scale_y_log10(labels = scales::dollar_format()) +
  scale_x_continuous(labels = scales::label_comma()) +
  # Label all point on the chart
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  # Label the chart
  labs(
    x = "Number of events",
    y = "Median property damage per event"
  ) +
  # Add chart theme
  theme_classic(base_size = 12, base_family = "inter")
  