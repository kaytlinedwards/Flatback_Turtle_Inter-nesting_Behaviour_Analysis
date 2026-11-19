#Peak Island Flatback Turtle Tracking Analysis
# Data Import and Inspection

library(tidyverse)
library(readxl)
library(here)
library(geosphere)

# Import original satellite-tracking data
# Research data are stored locally and excluded from GitHub.

turtle_raw <- read_csv(
  here("data", "PeakData_EV5502.csv"),
  show_col_types = FALSE
)

# Inspect imported dataset
glimpse(turtle_raw)

# Summarize the variables and missing values
summary(turtle_raw)

#Remove rows with missing tracking data
turtle_clean <- turtle_raw %>%
  filter(
    !is.na(lat),
    !is.na(lon),
    !is.na(Date),
    !is.na(Time),
    !is.na(Turtleid),
    !is.na(ClutchInterval)
  )

# Check remaining rows after cleaning
nrow(turtle_clean)

# Combine Date and Time into one timestamp
turtle_clean <- turtle_clean %>%
  mutate(
    datetime = as.POSIXct(
      paste(Date, Time),
      format = "%d/%m/%Y %H:%M:%S",
      tz = "UTC"
    )
  )

# Check the new timestamp column
glimpse(turtle_clean)

# Check for dates that failed to convert
sum(is.na(turtle_clean$datetime))

# Sort observations by turtle ID and timestamp
turtle_clean <- turtle_clean %>%
  arrange(Turtleid, datetime)

# Inspect the chronological order
turtle_clean %>%
  select(Turtleid, datetime) %>%
  print(n = 15)

# Calculate time between consecutive observations
turtle_clean <- turtle_clean %>%
  group_by(Turtleid) %>%
  mutate(
    time_since_previous_min = as.numeric(
      difftime(datetime, lag(datetime), units = "mins")
    )
  ) %>%
  ungroup()

# Inspect the calculated intervals
turtle_clean %>%
  select(Turtleid, datetime, time_since_previous_min) %>%
  print(n = 15)

# Calculate distance between consecutive turtle observations
turtle_clean <- turtle_clean %>%
  group_by(Turtleid) %>%
  mutate(
    distance_since_previous_m = geosphere::distHaversine(
      cbind(lag(lon), lag(lat)),
      cbind(lon, lat)
    )
  ) %>%
  ungroup()

# Inspect calculated distances
turtle_clean %>%
  select(Turtleid, datetime,
         time_since_previous_min,
         distance_since_previous_m) %>%
  print(n = 15)


# Convert distance from metres to kilometres
turtle_clean <- turtle_clean %>%
  mutate(
    distance_since_previous_km = distance_since_previous_m / 1000
  )

# Inspect distances in kilometres
turtle_clean %>%
  select(Turtleid, datetime,
         distance_since_previous_m,
         distance_since_previous_km) %>%
  print(n = 15)


# Calculate estimated movement speed in km/h
turtle_clean <- turtle_clean %>%
  mutate(
    speed_kmh = if_else(
      time_since_previous_min > 0,
      distance_since_previous_km / (time_since_previous_min / 60),
      NA_real_
    )
  )

# Inspect calculated movement speeds
turtle_clean %>%
  select(
    Turtleid,
    datetime,
    time_since_previous_min,
    distance_since_previous_km,
    speed_kmh
  ) %>%
  print(n = 15)


# Summarise estimated movement speeds
summary(turtle_clean$speed_kmh)

# Count observations with missing speed values
sum(is.na(turtle_clean$speed_kmh))

# Identify the 10 highest estimated speeds
turtle_clean %>%
  arrange(desc(speed_kmh)) %>%
  select(
    Turtleid,
    datetime,
    time_since_previous_min,
    distance_since_previous_km,
    speed_kmh
  ) %>%
  print(n = 10)


# Display the 10 highest speeds without truncating columns
turtle_clean %>%
  arrange(desc(speed_kmh)) %>%
  select(
    Turtleid,
    time_since_previous_min,
    distance_since_previous_km,
    speed_kmh
  ) %>%
  head(10) %>%
  print(width = Inf)



# Summarise tracking and movement patterns for each turtle
turtle_summary <- turtle_clean %>%
  group_by(Turtleid) %>%
  summarise(
    observations = n(),
    
    tracking_days = as.numeric(
      difftime(max(datetime), min(datetime), units = "days")
    ),
    
    median_interval_min = median(
      time_since_previous_min, na.rm = TRUE
    ),
    
    cumulative_distance_km = sum(
      distance_since_previous_km, na.rm = TRUE
    ),
    
    median_speed_kmh = median(
      speed_kmh, na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# View summary table
print(turtle_summary, width = Inf)


# Plot median observation interval for each turtle
ggplot(turtle_summary,
       aes(x = factor(Turtleid),
           y = median_interval_min)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Observation Intervals of Peak Island Flatback Turtles",
    x = "Turtle ID",
    y = "Median time between observations (minutes)"
  ) +
  theme_minimal()

# Plot median estimated movement speed for each turtle
ggplot(turtle_summary,
       aes(x = factor(Turtleid),
           y = median_speed_kmh)) +
  geom_col(fill = "seagreen3") +
  labs(
    title = "Estimated Movement Speed of Peak Island Flatback Turtles",
    x = "Turtle ID",
    y = "Median estimated speed (km/h)"
  ) +
  theme_minimal()




# Summarise movement patterns within each clutch interval
clutch_summary <- turtle_clean %>%
  group_by(Turtleid, ClutchInterval) %>%
  summarise(
    observations = n(),
    
    mean_interval_min = mean(
      time_since_previous_min, na.rm = TRUE
    ),
    
    median_interval_min = median(
      time_since_previous_min, na.rm = TRUE
    ),
    
    mean_distance_km = mean(
      distance_since_previous_km, na.rm = TRUE
    ),
    
    median_distance_km = median(
      distance_since_previous_km, na.rm = TRUE
    ),
    
    mean_speed_kmh = mean(
      speed_kmh, na.rm = TRUE
    ),
    
    median_speed_kmh = median(
      speed_kmh, na.rm = TRUE
    ),
    
    cumulative_distance_km = sum(
      distance_since_previous_km, na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# View expanded summary
View(clutch_summary)



# Visualise observation intervals with a zoomed y-axis
ggplot(
  turtle_clean %>% filter(!is.na(time_since_previous_min)),
  aes(
    x = factor(Turtleid),
    y = time_since_previous_min
  )
) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_cartesian(ylim = c(0, 400)) +
  labs(
    title = "Distribution of Observation Intervals",
    subtitle = "Y-axis zoomed to 0–400 minutes",
    x = "Turtle ID",
    y = "Time between observations (minutes)"
  ) +
  theme_minimal()


# Compare mean and median intervals across clutch intervals
clutch_plot <- clutch_summary %>%
  select(
    Turtleid,
    ClutchInterval,
    mean_interval_min,
    median_interval_min
  ) %>%
  pivot_longer(
    cols = c(mean_interval_min, median_interval_min),
    names_to = "statistic",
    values_to = "interval_min"
  )

# Plot mean and median observation intervals
ggplot(
  clutch_plot,
  aes(
    x = ClutchInterval,
    y = interval_min,
    colour = statistic,
    group = statistic
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ Turtleid) +
  scale_x_continuous(breaks = 2:5) +
  scale_colour_manual(
    values = c(
      mean_interval_min = "steelblue",
      median_interval_min = "seagreen3"
    ),
    labels = c(
      mean_interval_min = "Mean",
      median_interval_min = "Median"
    )
  ) +
  labs(
    title = "Observation Intervals Across Clutch Intervals",
    x = "Clutch interval",
    y = "Time between observations (minutes)",
    colour = "Statistic"
  ) +
  theme_minimal()

# Compare mean and median movement speeds across clutch intervals

speed_plot <- clutch_summary %>%
  select(
    Turtleid,
    ClutchInterval,
    mean_speed_kmh,
    median_speed_kmh
  ) %>%
  pivot_longer(
    cols = c(mean_speed_kmh, median_speed_kmh),
    names_to = "statistic",
    values_to = "speed_kmh"
  )

# Visualise movement speeds across clutch intervals

ggplot(
  speed_plot,
  aes(
    x = ClutchInterval,
    y = speed_kmh,
    colour = statistic,
    group = statistic
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ Turtleid) +
  scale_x_continuous(breaks = 2:5) +
  scale_colour_manual(
    values = c(
      mean_speed_kmh = "steelblue",
      median_speed_kmh = "seagreen3"
    ),
    labels = c(
      mean_speed_kmh = "Mean",
      median_speed_kmh = "Median"
    )
  ) +
  labs(
    title = "Estimated Movement Speed Across Clutch Intervals",
    x = "Clutch interval",
    y = "Estimated movement speed (km/h)",
    colour = "Statistic"
  ) +
  theme_minimal()

# Compare mean and median distances across clutch intervals

distance_plot <- clutch_summary %>%
  select(
    Turtleid,
    ClutchInterval,
    mean_distance_km,
    median_distance_km
  ) %>%
  pivot_longer(
    cols = c(mean_distance_km, median_distance_km),
    names_to = "statistic",
    values_to = "distance_km"
  )

# Visualise distances across clutch intervals

ggplot(
  distance_plot,
  aes(
    x = ClutchInterval,
    y = distance_km,
    colour = statistic,
    group = statistic
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ Turtleid) +
  scale_x_continuous(breaks = 2:5) +
  scale_colour_manual(
    values = c(
      mean_distance_km = "steelblue",
      median_distance_km = "seagreen3"
    ),
    labels = c(
      mean_distance_km = "Mean",
      median_distance_km = "Median"
    )
  ) +
  labs(
    title = "Distance Between Observations Across Clutch Intervals",
    x = "Clutch interval",
    y = "Distance between observations (km)",
    colour = "Statistic"
  ) +
  theme_minimal()

# Export cleaned turtle tracking data for ArcGIS Pro 

write_csv(
  turtle_clean,
  here("data", "Peak_TurtleTracking_Cleaned.csv"),
  na = ""
)

# Export summary tables
write_csv(
  turtle_summary,
  here("data", "Peak_TurtleSummary.csv")
)

write_csv(
  clutch_summary,
  here("data", "Peak_ClutchSummary.csv")
)

nrow(read_csv(here("data", "Peak_TurtleTracking_Cleaned.csv")))