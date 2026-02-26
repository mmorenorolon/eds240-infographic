library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(viridis)

# Heatmap dataframe
heat_df <- storm_data %>%
  
  # Extract year and month from beginning date 
  mutate(begin_date = mdy(BEGIN_DATE),
         year = year(begin_date),
         month = month(begin_date),  
         # Label months with month name abbreviations 
         month_name = factor(month.abb[month], levels = month.abb)) %>%
  
  # Remove NAs from year and month columns
  filter(!is.na(year), !is.na(month)) %>%
  
  # Count events for each year and month
  count(year, month, month_name, name = "events") %>%
  
  # Replace NAs with 0 for each event
  complete(year = full_seq(year, 1),
           month = 1:12,
           fill = list(events = 0)) %>%
  
  # Save month_name as a factor and abbreviate month names, sort month names
  mutate(month_name = factor(month.abb[month], levels = month.abb))

# Save minimum and maximum year for annotations
yr_min <- min(heat_df$year, na.rm = TRUE)
yr_max <- max(heat_df$year, na.rm = TRUE)


# Build Heatmap
use_magma <- TRUE

p <- ggplot(heat_df, aes(x = year, y = month_name, fill = events)) +
  
  # Hurricane season shading  Aug–Oct band across all years
  annotate("rect", xmin = yr_min - 0.5, xmax = yr_max + 0.5,
           ymin = which(levels(heat_df$month_name) == "Aug") - 0.5,
           ymax = which(levels(heat_df$month_name) == "Oct") + 0.5,
           fill = "white", alpha = 0.8) +
  
  # Create heatmap tiles
  geom_tile(width = 1, height = 1, color = NA) +
  
  # Add thin white lines between months
  geom_hline(yintercept = seq(1.5, 11.5, by = 1), color = "white", linewidth = 0.35, alpha = 0.7) +
  
  # Add thinner lines to divide years 
  geom_vline(xintercept = seq(yr_min + 0.5, yr_max - 0.5, by = 1),
             color = "white", linewidth = 0.2, alpha = 0.25) +
  
  # Add lines to highlight extreme years (Hurricane Maria and Hurricane Fiona)
  geom_vline(xintercept = 2017, linewidth = 0.7, color = "white", 
             alpha = 0.8) +
  geom_vline(xintercept = 2022, linewidth = 0.7, color = "white", 
             alpha = 0.8) +
  
  annotate("label",
    x = 2017, y = "Oct",
    label = "Maria (2017)",
    family = "inter",
    size = 3.2,
    fill = "white", alpha = 0.9,
    color = "#111827",
    label.r = unit(0.15, "lines")) +
  
  annotate("label", x = 2022, y = "Sep", label = "Fiona (2022)",
           family = "inter", size = 3.2, fill = "white", alpha = 0.9,
           color = "#111827", label.r = unit(0.15, "lines")) +
  
  # Label Hurricane Season
  annotate("text", x = yr_min + 1, y = "Aug", 
           label = "Hurricane season (Aug–Oct)", family = "inter",
           size = 3.4, hjust = 0, color = "white", alpha = 0.9) +
  
  # Customize x-axis scale 
  #scale_y_continuous(breaks = pretty_breaks(n = 8), 
                     #expand = expansion(mult = c(0.01, 0.02))) +
  
  labs(title = "Storm event frequency by month and year",
       caption = "Data Source:NOAA Storm Events Database",
       x = NULL, y = NULL, fill = "Events") +
  
  # Customize theme elements
  theme_minimal(base_family = "inter", base_size = 13) +
  theme(plot.background = element_rect(fill = "#F6F8FA", color = NA),
        panel.background = element_rect(fill = "#F6F8FA", color = NA),
        panel.grid = element_blank(),
        
        axis.text.x = element_text(color = "#374151"),
        axis.text.y = element_text(color = "#374151"),
        
        legend.title = element_text(color = "#374151"),
        legend.text = element_text(color = "#374151"),
        
        plot.title = element_text(face = "bold", 
                                  size = 18, 
                                  color = "#111827"),
        plot.subtitle = element_text(size = 11.5, 
                                     color = "#374151", 
                                     margin = margin(b = 8)),
        plot.margin = margin(14, 14, 14, 14)) +
  scale_fill_viridis_c(option = "magma",
                       begin = 0.1,
                       end = 0.95,
                       labels = scales::comma)


p
