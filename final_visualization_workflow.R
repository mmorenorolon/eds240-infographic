library(lubridate)
library(ggplot2)
library(scales)
library(viridis)
library(tidyverse)
library(janitor)
library(here)
library(showtext)
library(ggtext)
library(glue)
library(sysfonts)
library(waffle)


#----------------------- raw data --------------------------------------

# Save all data files
storm_files <- list.files(
  path = "C:/Documents/MEDS/EDS240 - data visualization/eds240-infographic/data",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read in all data files and merge as one dataframe
storm_raw <- storm_files %>%
  map_dfr(
    ~ read_csv(.x, col_types = cols(.default = col_character()))
  )

storm_clean <- storm_raw %>% 
  # Convert variables to numeric
  mutate(across(.cols = c(DEATHS_DIRECT, DEATHS_INDIRECT, INJURIES_DIRECT, INJURIES_INDIRECT, DAMAGE_PROPERTY_NUM, DAMAGE_CROPS_NUM, MAGNITUDE, BEGIN_RANGE, END_RANGE), ~ parse_number(.x))) %>%
  
  # Standardize time strings
  mutate(
    BEGIN_TIME = str_pad(BEGIN_TIME, width = 4, side = "left", 
                         pad = "0"),
    END_TIME = str_pad(END_TIME,   width = 4, side = "left", 
                       pad = "0")) %>% 
  
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
         SOURCE, FLOOD_CAUSE) %>% 
  
  # Standardize variable names
  clean_names()


#-------------------- theme colors -------------------------------------

bg <- "#F4F6F8"

grid <- "#E5E7EB"   # subtle gridline color

#--------------------- Waffle chart ------------------------------------
pal_waffle <- c(
  "Flood-related"      = "#003049",
  "Tropical cyclones"  = "#1A5E7A",
  "Coastal & marine"   = "#2A9D8F",
  "Severe wind"        = "#8FB8D0",
  "Fire & lightning"   = "#D62828",
  "Heat & drought"     = "#F77F00",
  "Other rare"         = "#EAE2B7"
)

levels_waffle <- c(
  "Flood-related",
  "Fire & lightning",
  "Heat & drought",
  "Severe wind",
  "Coastal & marine",
  "Tropical cyclones",
  "Other rare"
)

legend_labels <- stringr::str_to_title(levels_waffle)

pr_events_collapsed <- storm_clean %>%
  mutate(
    event_type = as.character(event_type),
    hazard_cat = case_when(
      event_type %in% c("Flood", "Flash Flood", "Coastal Flood", "Heavy Rain", "Debris Flow") ~ "Flood-related",
      event_type %in% c("Hurricane (Typhoon)", "Tropical Storm", "Tropical Depression") ~ "Tropical cyclones",
      event_type %in% c("Thunderstorm Wind", "Strong Wind", "High Wind", "Tornado", "Funnel Cloud") ~ "Severe wind",
      event_type %in% c("Excessive Heat", "Drought", "Extreme Cold/Wind Chill") ~ "Heat & drought",
      event_type %in% c("Rip Current", "High Surf", "Waterspout", "Seiche") ~ "Coastal & marine",
      event_type %in% c("Wildfire", "Lightning") ~ "Fire & lightning",
      TRUE ~ "Other rare"
    ))

# Summarize to 100%
pr_event_props <- pr_events_collapsed %>%
  count(hazard_cat) %>%
  mutate(prop = n / sum(n),
         parts = round(prop * 100)) %>%
  arrange(desc(parts))

# Force total squares to be exactly 100
diff <- 100 - sum(pr_event_props$parts)

if (diff != 0) {
  pr_event_props$parts[which.max(pr_event_props$parts)] <-
    pr_event_props$parts[which.max(pr_event_props$parts)] + diff
}

pr_event_props <- pr_event_props %>%
  mutate(hazard_cat = factor(hazard_cat, levels = levels_waffle)) %>%
  arrange(hazard_cat)

waffle_vector <- setNames(pr_event_props$parts, pr_event_props$hazard_cat)

waffle_plot <- waffle::waffle(
  parts = waffle_vector,
  legend_pos = "right",
  size = 0.5,
  rows = 20,
  colors = pal_waffle[names(waffle_vector)]
) +
  #scale_fill_manual(values = pal_waffle, labels = legend_labels) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(
      fill = bg, 
      color = NA),
    panel.background = element_rect(
      fill = bg, 
      color = NA),
    plot.title = element_text(face = "bold", size = 18, 
                              #color = text
                              ),
    plot.subtitle = element_text(size = rel(0.87), 
                                 #color = subtext, 
                                 margin = margin(b = 10)),
    plot.caption = element_text(size = 9.5, 
                                #color = subtext, 
                                margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 9, 
                               #color = subtext, 
                               lineheight = 1.1),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(16, 16, 16, 16),
    guides(fill = guide_legend(byrow = TRUE))
  )

waffle_plot

ggsave(here("images",
            "storms-waffle.pdf"), 
       plot = waffle_plot,
       dpi=300, 
       width = 8, 
       height = 5)



#------------------------------ Heatmap ------------------------------------
heatmap_events <- c(
  "Flash Flood",
  "Heavy Rain",
  "Flood",
  "Coastal Flood",
  "Thunderstorm Wind",
  "High Wind",
  "Strong Wind",
  "Lightning",
  "Hail",
  "Funnel Cloud",
  "Tornado",
  "Waterspout",
  "Debris Flow",
  "High Surf",
  "Tropical Depression",
  "Tropical Storm",
  "Hurricane (Typhoon)"
)

heatmap_filtered <- storm_clean %>% 
  filter(event_type %in% heatmap_events)

heatmap_df <- heatmap_filtered %>%
  mutate(
    month = month(begin_date)
  ) %>%
  filter(!is.na(year), !is.na(month)) %>%
  count(year, month, name = "events") %>%
  complete(
    year  = full_seq(year, 1),
    month = 1:12,
    fill  = list(events = 0)
  )

# Save min and max values for annotations
yr_min <- min(heatmap_df$year, na.rm = TRUE)
yr_max <- max(heatmap_df$year, na.rm = TRUE)

# Helper positions ----------------------------------------------------------
# months (numeric): Jan=1 ... Dec=12
y_jun <- 6
y_nov <- 11

# We'll annotate outside the panel, so give ourselves left/top "breathing room"
x_bracket <- yr_min - 1.1
x_bracket_tick <- yr_min - 0.55   # small ticks extend toward plot

heatmap_plot <- ggplot(heatmap_df, aes(x = year, y = month, fill = events)) +
  
  # Heatmap tiles (no vertical gridlines)
  geom_tile(width = 1, height = 1, color = NA) +
  
  # Subtle horizontal separators only
  geom_hline(yintercept = seq(1.5, 11.5, by = 1),
             color = "white", linewidth = 0.25,
             alpha = 0.45) +
  
  # --- Jun–Oct bracket OUTSIDE plot (left of y labels) --------------------
annotate("segment",
         x = x_bracket, xend = x_bracket,
         y = y_jun, yend = y_nov,
         linewidth = 0.9, color = "#0B3C5D") +
  
  annotate("segment",
           x = x_bracket, xend = x_bracket_tick,
           y = y_jun, yend = y_jun,
           linewidth = 0.9, color = "#0B3C5D") +
  
  annotate("segment",
           x = x_bracket, xend = x_bracket_tick,
           y = y_nov, yend = y_nov,
           linewidth = 0.9, color = "#0B3C5D") +
  
  # X scale + add left padding for bracket
  scale_x_continuous(
    breaks = pretty_breaks(n = 8),
    limits = c(yr_min - 1.5, yr_max + 0.5),
    expand = c(0, 0)
  ) +
  
  # Months on y, reversed so Dec at top
  scale_y_reverse(
    breaks = 1:12,
    labels = month.abb,
    limits = c(13.2, 0.5),
    expand = c(0, 0)
  ) +
  
  # Blue-themed palette (close to your dumbbell vibe)
  scale_fill_viridis_c(
    option = "mako",
    begin  = 0.25,
    end    = 1,
    labels = comma,
    direction = -1
  ) +
  
  labs(
    x = NULL, y = NULL,
    fill = NULL
  ) +
  
  # Legend top + horizontal
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = unit(3.8, "in"),
      barheight = unit(0.18, "in")
    )
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "#F6F8FA", color = NA),
    panel.background = element_rect(fill = "#F6F8FA", color = NA),
    panel.grid = element_blank(),
    
    # Monospace axis labels so characters align (x + y)
    axis.text.x = element_text(family = "mono", color = "#374151"),
    axis.text.y = element_text(family = "mono", color = "#374151"),
    
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(color = "#111827"),
    legend.text  = element_text(color = "#374151"),
    
    # Extra margins so outside annotations don't get clipped
    plot.margin = margin(16, 18, 14, 70),
    legend.margin = margin(t = 2)
  ) +
  
  # IMPORTANT: allow drawing outside the panel
  coord_cartesian(clip = "off")

heatmap_plot

ggsave(here("images",
            "heatmap_final.pdf"), 
       plot = heatmap_plot,
       dpi= 300, 
       width = 8, 
       height = 5)


#------------------------- Scatter Plot --------------------------------------
pal_scatter <- c(
  "Flash Flood"           = "#003049",
  "Heavy Rain"            = "#2A9D8F",
  "Flood"                 = "#003049",
  "Coastal Flood"         = "#2A9D8F",
  "Debris Flow"           = "#1A5E7A",
  "Tropical Storm"        = "#1A5E7A",
  "Hurricane (Typhoon)"   = "#003049"
)

scatter_events <- c(
  "Flash Flood",
  "Heavy Rain",
  "Flood",
  "Coastal Flood",
  "Debris Flow",
  "Tropical Storm",
  "Hurricane (Typhoon)"
)
  
scatter_df <- storm_clean %>%
  group_by(event_type) %>%
  summarise(
    events = n(),
    total_damage = sum(damage_property_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(event_type %in% scatter_events)

# median reference lines
x_ref <- median(scatter_df$events, na.rm = TRUE)
y_ref <- median(scatter_df$total_damage, na.rm = TRUE)


scatter_plot <- ggplot(scatter_df, 
                        aes(x = events,
                            y = total_damage,
                            label = event_type,
                            colour = event_type)) +
  
  geom_vline(xintercept = x_ref, color = "#9CA3AF", linewidth = 0.4,
             linetype = "dashed") +
  geom_hline(yintercept = y_ref, color = "#9CA3AF", linewidth = 0.4,
             linetype = "dashed") +
  
  geom_point(size = 4.9, alpha = 0.9) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
                     expand = expansion(mult = c(0.03, 0.08))) +
  
  # Set log scale and add a dollar format for values in the y-axis
  scale_y_log10(breaks = scales::breaks_log(n = 5),
                labels = scales::label_dollar()) +
  
  scale_color_manual(values = pal_scatter) +
  
    # Label all point on the chart
  ggrepel::geom_text_repel( size = 3.5,
                            max.overlaps = 20,
                            box.padding = 0.4,
                            point.padding = 0.3,
                            min.segment.length = 0) +
  # Label the chart
  labs(
    x = "Number of events",
    y = "Total recorded property damage ($, log scale)"
  ) +
  # Add chart theme
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(fill = bg, color = NA),
        plot.background = element_rect(fill = bg, color = NA),
        
        # Monospace axis labels so characters align (x + y)
        axis.text.x = element_text(family = "mono", color = "#374151"),
        axis.text.y = element_text(family = "mono", color = "#374151"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        legend.position = "none",
        
        plot.margin = margin(16, 16, 16, 16)
        )

scatter_plot

ggsave(here("images",
            "scatter_final.pdf"), 
       plot = scatter_plot,
       dpi= 300, 
       width = 8, 
       height = 5)
