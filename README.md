# Visualizing Storm Impacts in Puerto Rico Using NOAA's Storm Events Database

## Table of Contents
- [Project Description](#projectdescription)
- [Background](#background)
- [Repository Structure](#repositorystructure)
- [License](#license)

## Project Description

This repository supports an infographic project that examines how storm events shape risk across Puerto Rico. Using the NOAA National Centers for Environmental Information (NCEI) Storm Events Database, the project analyzes long‑term patterns in hazard frequency, seasonality, and severity. The resulting visualizations highlight which hazards occur most often, when they cluster throughout the year, and which events cause the greatest damage.

The goal is to provide a clear, data‑driven understanding of Puerto Rico’s storm‑hazard landscape that supports climate vulnerability research, risk communication, and resilience planning.

## Background

This project is part of a broader exploration of environmental risk and climate vulnerability in Puerto Rico. The accompanying written analysis (not included in this repository) provides a theoretical and legal framework for understanding how storm hazards intersect with issues of environmental justice, infrastructure resilience, and disaster response.

## About the Data

The analysis uses the NOAA Storm Events Database (Puerto Rico subset, 1996–2025), accessed through the National Centers for Environmental Information (NCEI). The dataset includes:

- event types (flood, flash flood, hurricane, heavy rain, etc.)

- event dates and locations

- recorded property damage

- injuries and fatalities

- narrative descriptions

Only events reported for Puerto Rico were included. Data cleaning steps involved standardizing municipality names, filtering incomplete records, and grouping event types into broader hazard categories for clarity. All storm‑event data are publicly available through NOAA NCEI.

## Methods & Workflow
This project follows the following workflow:

1. Data Processing in R

- Cleaning and validating the Puerto Rico subset
- Normalizing event types
- Summarizing frequency, seasonality, and damage

2. Chart Creation in R

- Waffle chart (hazard composition)
- Heatmap (monthly event frequency, 1996–2025)
- Scatter plot (damage vs. frequency)

6. Chart Refinement in Affinity Designer

- Adjusting spacing, typography, and color
- Ensuring visual consistency across plots
- Exporting high‑resolution assets
- Applying a unified typographic and color system
- Final layout and export

## Repository Structure

```
eds240-infographic
├───data
├───drafting-viz_files
│       └───quarto-html
├───fonts
├───icons
└───images
```

## License

This repository is managed under an MIT License. 
