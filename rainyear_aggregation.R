
# Attach required packages
library(tidyverse)
library(janitor)
library(lubridate)

# Require that a data frame (DATA) contain a row with VALUE in the COL_IND column.
#   If it does not already exist, add a new row with only NA values in other columns
require_rows_exist <- function(value, data, col_ind)
{
  # Ensure input data frame is not empty
  if(nrow(data) < 1 | ncol(data) < 1)
    stop("require_rows_exist: Input dataframe is empty!")
  
  # Check if value exists in data frame
  if(sum(na.omit(data[,1] == value) < 1))
  {
    # If value was NOT in the data frame, add a new row with the same structure
    new_data <- data[1,]*NA
    new_data[1,col_ind] <- value
    data <- rbind(data,new_data)
  }
  
  return(data)
}

generate_rain_data <- function(filename)
{
  # Load Data and create Day-of-Year values
  rain_data <- read_csv(filename, skip=9) %>%
    select(c("station id", "water year", "year", "month", "day", "daily rain")) %>%
    janitor::clean_names() %>% 
    drop_na() %>%
    mutate(day_of_year = lubridate::yday(paste(year, month, day, sep="-")))
  rain_data$day_of_water_year = (rain_data$day_of_year %% 244) - (366-244)*((rain_data$day_of_year %/% (244)) - 1) + 1
  
  # Annual rain values
  annual_rain <- rain_data %>% 
    group_by(water_year) %>% 
    summarize(water_year = water_year[[1]],
              total_rain = sum(daily_rain))
  lapply # Fill gap years with NAs 
  
  # Minimum and Maximum Dates
  min_year <- min(rain_data$water_year)
  min_doy <- min(rain_data[rain_data$year==min_year,]$day_of_water_year)
  max_year <- max(rain_data$water_year)
  max_doy <- max(rain_data[rain_data$year==min_year,]$day_of_water_year)
  
  # Generate Cumulative Precipitation Totals
  cumulative_data <- data.frame(water_year = integer(0),
                                day_of_water_year = integer(0),
                                rain_cumulative = double(0))
  for(year in min_year:max_year)
  {
    daily_sum <- data.frame(water_year = rep(year, 366),
                            day_of_water_year = 1:366,
                            rain_cumulative = rep(0,366))
    for(day in 1:366)
    {
      daily_sum[day,]$rain_cumulative = sum(rain_data[rain_data$day_of_water_year < day & rain_data$year==year,]$daily_rain)
    }
    
    cumulative_data <- rbind(cumulative_data, daily_sum)
  }
  
  return(list(rain_data, annual_rain, cumulative_data))
  
}


# Generate Rain Outputs
santa_barbara_rain <- generate_rain_data(here::here("data","234dailys.csv"))
goleta_rain <- generate_rain_data(here::here("data","474dailys.csv"))
santa_ynez_mtn_rain <- generate_rain_data(here::here("data","255dailys.csv"))
refugio_pass_rain <- generate_rain_data(here::here("data","429dailys.csv"))
guadalupe_rain <- generate_rain_data(here::here("data","352dailys.csv"))
santa_ynez_mouth_rain <- generate_rain_data(here::here("data","361dailys.csv"))
figueroa_mtn_rain <- generate_rain_data(here::here("data","352dailys.csv"))
goleta_fire_station_rain <- generate_rain_data(here::here("data","440dailys.csv"))

# Generate Annual Plots
annual_plot <- ggplot() + 
  geom_line(data=santa_barbara_rain[[2]],
            aes(x=water_year, y=total_rain),
            col="blue") + 
  geom_point(data=santa_barbara_rain[[2]],
             aes(x=water_year, y=total_rain), 
             col="blue", size=2) +
  geom_line(data=goleta_rain[[2]],
            aes(x=water_year, y=total_rain), 
            col="purple") +  
  geom_point(data=goleta_rain[[2]],
             aes(x=water_year, y=total_rain), 
             col="purple", size=2) + 
  geom_line(data=goleta_fire_station_rain[[2]],
            aes(x=water_year, y=total_rain), 
            col="magenta") +  
  geom_point(data=goleta_fire_station_rain[[2]],
             aes(x=water_year, y=total_rain), 
             col="magenta", size=2) + 
  geom_line(data=santa_ynez_mtn_rain[[2]],
            aes(x=water_year, y=total_rain), 
            col="green") +  
  geom_point(data=santa_ynez_mtn_rain[[2]],
             aes(x=water_year, y=total_rain), 
             col="green", size=2) + 
  geom_line(data=refugio_pass_rain[[2]],
            aes(x=water_year, y=total_rain), 
            col="green3") +  
  geom_point(data=refugio_pass_rain[[2]],
             aes(x=water_year, y=total_rain), 
             col="green3", size=2) + 
  scale_x_continuous(limits=c(1996,2020)) + 
  theme_bw() + 
  labs(x = "Water Year",
       y = "Rainfall (inches)",
       title = "Annual Precipitation on South Coast of Santa Barbara County",
       caption = "Annual total precipitation in inches. Santa Barbara, Goleta 1, and Goleta 2 are in blue, red, and magenta. Two from the Santa Ynez Mtns are in \ngreen, from elevations of 795 and 1005 m.")
ggsave(plot=annual_plot, filename=here::here("output_imagery","annual.png"),
       width=8, height=6)

# Generate Day-of-Year Plots
#   2019
daily_2019_plot <- ggplot() + 
  geom_line(data=filter(santa_barbara_rain[[3]], water_year==2019),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="blue") + 
  geom_line(data=filter(goleta_rain[[3]], water_year==2019),
            aes(x=day_of_water_year, y=rain_cumulative), 
            col="purple") +  
  geom_line(data=filter(goleta_fire_station_rain[[3]], water_year==2019),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="magenta") + 
  geom_line(data=filter(santa_ynez_mtn_rain[[3]], water_year==2019),
            aes(x=day_of_water_year, y=rain_cumulative), 
            col="green") +  
  geom_line(data=filter(refugio_pass_rain[[3]], water_year==2019),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="green4") +
  theme_bw() + 
  labs(x = "Water Year",
       y = "Rainfall (inches)",
       title = "Daily Cumulative Precipitation on South Coast of Santa Barbara County in 2019",
       caption = "Daily cumulative precipitation in inches. Santa Barbara, Goleta 1, and Goleta 2 are in blue, red, and magenta. Two from the Santa Ynez Mtns are \nin green, from elevations of 795 and 1005 m.")
ggsave(plot=daily_2019_plot, filename=here::here("output_imagery","2019.png"),
       width=8, height=6)
#   2018
daily_2018_plot <- ggplot() + 
  geom_line(data=filter(santa_barbara_rain[[3]], water_year==2018),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="blue") + 
  geom_line(data=filter(goleta_rain[[3]], water_year==2018),
            aes(x=day_of_water_year, y=rain_cumulative), 
            col="purple") +  
  geom_line(data=filter(goleta_fire_station_rain[[3]], water_year==2018),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="magenta") + 
  geom_line(data=filter(santa_ynez_mtn_rain[[3]], water_year==2018),
            aes(x=day_of_water_year, y=rain_cumulative), 
            col="green") +  
  geom_line(data=filter(refugio_pass_rain[[3]], water_year==2018),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="green4") + 
  theme_bw() + 
  labs(x = "Water Year",
       y = "Rainfall (inches)",
       title = "Daily Cumulative Precipitation on South Coast of Santa Barbara County in 2018",
       caption = "Daily cumulative precipitation in inches. Santa Barbara, Goleta 1, and Goleta 2 are in blue, red, and magenta. Two from the Santa Ynez Mtns are \ningreen, from elevations of 795 and 1005 m.") +
  theme(plot.caption = element_text(hjust = 0))
ggsave(plot=daily_2018_plot, filename=here::here("output_imagery","2018.png"),
       width=8, height=6)
#   2015
daily_2015_plot <- ggplot() + 
  geom_line(data=filter(santa_barbara_rain[[3]], water_year==2015),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="blue") + 
  geom_line(data=filter(goleta_rain[[3]], water_year==2015),
            aes(x=day_of_water_year, y=rain_cumulative), 
            col="purple") +  
  geom_line(data=filter(goleta_fire_station_rain[[3]], water_year==2015),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="magenta") + 
  geom_line(data=filter(santa_ynez_mtn_rain[[3]], water_year==2015),
            aes(x=day_of_water_year, y=rain_cumulative), 
            col="green") +  
  geom_line(data=filter(refugio_pass_rain[[3]], water_year==2015),
            aes(x=day_of_water_year, y=rain_cumulative),
            col="green4") + 
  theme_bw() + 
  labs(x = "Water Year",
       y = "Rainfall (inches)",
       title = "Daily Cumulative Precipitation on South Coast of Santa Barbara County in 2015",
       caption = "Daily cumulative precipitation in inches. Santa Barbara, Goleta 1, and Goleta 2 are in blue, red, and magenta. Two from the Santa Ynez Mtns are \nin green, from elevations of 795 and 1005 m.") + 
  theme(plot.caption = element_text(hjust = 0))
ggsave(plot=daily_2015_plot, filename=here::here("output_imagery","2015.png"),
       width=8, height=6)
# Generate Some Daily Statistics
daily_statistics_goleta <- goleta_rain[[3]] %>%
  group_by(day_of_water_year) %>%
  summarize(mean = mean(rain_cumulative),
            stdev = sd(rain_cumulative),
            min = min(rain_cumulative),
            max = max(rain_cumulative))
daily_statistics_mtns <- santa_ynez_mtn_rain[[3]] %>%
  group_by(day_of_water_year) %>%
  summarize(mean = mean(rain_cumulative),
            stdev = sd(rain_cumulative),
            min = min(rain_cumulative),
            max = max(rain_cumulative))
# Plot them
daily_stats_plot <- ggplot() + 
  geom_ribbon(data=daily_statistics_goleta, 
              aes(x=day_of_water_year, y=mean, ymin=mean-stdev, ymax=mean+stdev), 
              linetype=2, alpha=0.1, col="blue", fill="blue") + 
  geom_line(data=daily_statistics_goleta,
            aes(x=day_of_water_year, y=mean), col="blue3") + 
  geom_ribbon(data=daily_statistics_mtns, 
              aes(x=day_of_water_year, y=mean, ymin=mean-stdev, ymax=mean+stdev), 
              linetype=2, alpha=0.1, col="green", fill="green") + 
  geom_line(data=daily_statistics_mtns,
            aes(x=day_of_water_year, y=mean), col="green3") +
  theme_bw() + 
  labs(x = "Day of Water Year",
       y = "Rain (inches)",
       title = "Cumulative Rain in Goleta",
       caption = "Expected cumulative rain in Goleta (blue) and Santa Ynez Mountains (green). Average values are solid lines. Shaded regions \naround averages represent a 1.0 standard deviation offset.. ") + 
  theme(plot.caption = element_text(hjust = 0))
ggsave(plot=daily_stats_plot, filename=here::here("output_imagery","daily_stats.png"),
       width=8, height=6)

