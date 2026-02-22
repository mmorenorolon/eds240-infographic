library(tidyverse)
library(janitor)
library(here)
library(showtext)
library(ggtext)
library(glue)
library(sysfonts)
library(scales)

#--- Data Wrangling ---
event_families <- storm_data %>%
  mutate(
    DAMAGE_PROPERTY_NUM = as.numeric(DAMAGE_PROPERTY_NUM),
    EVENT_TYPE = as.character(EVENT_TYPE),
    
    hazard_family = case_when(
      EVENT_TYPE %in% c("Flood", "Flash Flood", "Coastal Flood", "Heavy Rain", "Debris Flow") ~ "Flood-related",
      EVENT_TYPE %in% c("Hurricane (Typhoon)", "Tropical Storm", "Tropical Depression") ~ "Tropical cyclones",
      EVENT_TYPE %in% c("Thunderstorm Wind", "Strong Wind", "High Wind", "Tornado", "Funnel Cloud") ~ "Severe wind",
      EVENT_TYPE %in% c("Excessive Heat", "Drought", "Extreme Cold/Wind Chill") ~ "Heat & drought",
      EVENT_TYPE %in% c("Rip Current", "High Surf", "Waterspout", "Seiche") ~ "Coastal & marine",
      TRUE ~ "Other"
    )
  )


# Find shares for events vs damage
waffle_df <- event_families %>%
  group_by(hazard_family) %>%
  summarise(
    events = n(),
    damage = sum(DAMAGE_PROPERTY_NUM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    event_share = events / sum(events),
    damage_share = damage / sum(damage),
    hazard_family = factor(hazard_family, levels = names(pal))
  )

# Convert waffle shares to 100
to_waffle_counts <- function(df, share_col){
  out <- df %>%
    dplyr::transmute(
      hazard_family,
      n = round(100 * .data[[share_col]])
    )
  
  # force total to 100 by adjusting the largest group
  diff <- 100 - sum(out$n)
  if (diff != 0) {
    idx <- which.max(out$n)
    out$n[idx] <- out$n[idx] + diff
  }
  out
}

events_100 <- to_waffle_counts(waffle_df, "event_share")
damage_100 <- to_waffle_counts(waffle_df, "damage_share")




# --- Fonts ---
font_add_google("Inter", "inter")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# --- Palette ---
pal <- c(
  base = "#6B7280",       # muted gray
  highlight = "#0B3C5D",  # storm blue
  bg = "#F6F8FA",
  grid = "#E6E9EF",
  text = "#111827"
)


# --- text ---
title_text <- "In the Puerto Rico, tropical storms are more common than other cyclones."
subtitle_text <- "Tropical Storm is a cyclone with maximum sustained winds of 39 to 73 mph (34 to 63 knots).\nshowing number of cyclones for each year since 2010"
caption_text <- "Data: NOAA Storm Events Database"

# --- Plot ---

p_events <- waffle::waffle(
  parts = setNames(events_100$n, events_100$hazard_family),
  rows = 10,
  colors = pal,
  title = "What Puerto Rico experiences most",
  xlab = "Each square = 1% of all recorded storm events (1996–2025)"
) +
  theme_minimal(base_family = "inter", base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 10, color = "#4B5563"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )

p_damage <- waffle::waffle(
  parts = setNames(damage_100$n, damage_100$hazard_family),
  rows = 10,
  colors = pal,
  title = "What costs the most (property damage)",
  xlab = "Each square = 1% of total reported property damage (nominal $)"
) +
  theme_minimal(base_family = "inter", base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 10, color = "#4B5563"),
    legend.position = "none",
    panel.grid = element_blank()
  )

waffles <- p_events + p_damage + plot_layout(guides = "collect") &
  theme(
    plot.background = element_rect(fill = "#F6F8FA", color = NA),
    panel.background = element_rect(fill = "#F6F8FA", color = NA)
  )

waffles

# Check if imbalances are persuassive or if we need to split categories more
waffle_df %>% arrange(desc(events))
waffle_df %>% arrange(desc(damage))


#--- Save plot ---
showtext_opts(dpi = 320)
ggsave(here("images","storms-waffle.png"), dpi=320, width = 12, height = 9)
showtext_auto(FALSE)