library(fpp3)
library(rvest)
library(janitor)
library(lubridate)
library(tidyverse)

html_link <- "https://en.wikipedia.org/wiki/Super_Bowl_television_ratings"

# Data Extraction -----------------------------------------------------------------------------
SuperBowl_tb <-  read_html(html_link) |> 
   html_element("table") |> 
   html_table()

# Data Cleansing ------------------------------------------------------------------------------
# Data needs to be parsed and cleaned before any analysis.
SuperBowl_tb <- SuperBowl_tb |> 
  clean_names() |> 
  filter(row_number() > 1) |> 
  rename(households_rating = households,
         households_share = households_2,
         x18_49_demographic_rating = x18_49_demographic,
         x18_49_demographic_share = x18_49_demographic_2,
         avg_cost_of_30_second_ad_original = avg_cost_of_30_second_ad,
         avg_cost_of_30_second_ad_adjusted = avg_cost_of_30_second_ad_2)

SuperBowl_tb <- SuperBowl_tb |> 
  mutate(avg_viewers_millions = str_remove(avg_viewers_millions, "\\[.*"),
         total_viewers_millions = str_remove(total_viewers_millions, "\\[.*"),
         households_rating = str_remove(households_rating, "\\[.*"),
         households_share = str_remove(households_share, "\\[.*"),
         x18_49_demographic_rating = str_remove(x18_49_demographic_rating, "\\[.*"),
         x18_49_demographic_share = str_remove(x18_49_demographic_share, "\\[.*"),
         avg_cost_of_30_second_ad_original = str_remove(avg_cost_of_30_second_ad_original, "\\[.*"), 
         avg_cost_of_30_second_ad_adjusted = str_remove(avg_cost_of_30_second_ad_adjusted, "\\[.*"),
         date = parse_date(date, format = "%B %d, %Y"),
         avg_viewers_millions = parse_number(avg_viewers_millions),
         total_viewers_millions = parse_number(total_viewers_millions, na = "Unknown"),
         households_rating = parse_number(households_rating),
         households_share = parse_number(households_share),
         x18_49_demographic_rating = parse_number(x18_49_demographic_rating, na = "Unknown"),
         x18_49_demographic_share = parse_number(x18_49_demographic_share, na = "Unknown"), 
         avg_cost_of_30_second_ad_original = parse_number(avg_cost_of_30_second_ad_original),
         avg_cost_of_30_second_ad_adjusted = parse_number(avg_cost_of_30_second_ad_adjusted)
         )

# We are missing information about the last Super Bowl, and few observations on total viewers.
SuperBowl_tb |>
  filter(row_number() != max(row_number())) |> 
  select(network, avg_viewers_millions, total_viewers_millions, households_rating, households_share, 
         avg_cost_of_30_second_ad_original, avg_cost_of_30_second_ad_adjusted) |> 
  skimr::skim()

SuperBowl_tb <- SuperBowl_tb |> 
  filter(row_number() != max(row_number())) |> 
  mutate(total_viewers_millions = if_else(is.na(total_viewers_millions),
                                          mean(total_viewers_millions, na.rm = TRUE), total_viewers_millions))

# Data completed with the average viewer’s value.
SuperBowl_tb |>
  filter(row_number() != max(row_number())) |> 
  select(network, avg_viewers_millions, total_viewers_millions, households_rating, households_share, 
         avg_cost_of_30_second_ad_original, avg_cost_of_30_second_ad_adjusted) |> 
  skimr::skim()

# Correlation matrix, as expected the cost of the ad is strongly related to the viewers. 
# Now, we need to know how.
SuperBowl_tb |> 
  select(avg_viewers_millions, households_rating, households_share, 
         avg_cost_of_30_second_ad_original) |> 
  cor()

# Forecasting the viewership ------------------------------------------------------------------
# One of the important things is to know the viewership for the next Super Bowl, with this, 
# companies can predict if it is worthy or not to have an ad.
# For forecasting first I need to create a time series tibble with a period-index of a year.
SuperBowl_ts <- SuperBowl_tb |> 
  mutate(year = year(date),
         avg_cost_of_30_second_ad_original = avg_cost_of_30_second_ad_original / 1e6) |> 
  as_tsibble(index = year)

## Plot data ----------------------------------------------------------------------------------
# The first step is to plot the original data, so we can decide which method can be used to 
# forecast the viewership for the next Super Bowl.
# The pattern is an upward trend with no seasonality.
# The last years, we see that the SB has lower viewership than in previous years.

# I am considering 3 methods to produce the forecast. 
# Linear regression,probably with an approach using piecewise with several knots
# Exponential smoothing.
# Finally moving averages. 
SuperBowl_ts |> 
  ggplot(aes(x = date, y = avg_viewers_millions)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE)

## Forecasting Linear -------------------------------------------------------------------------
# The model explain 80% of the variance according to the R squared criterion.
# From the simple linear regression, we have:
# Variance is "constant" but is in a great amount.
# Mean around 0.
# Correlated lags.
# So there is more information that can be used to improve the forecast.
model_lm_simple <- SuperBowl_ts |> 
  model(tslm_simple = TSLM(formula = avg_viewers_millions ~ trend()))

model_lm_simple |> 
  report()

model_lm_simple |> 
  gg_tsresiduals()

model_lm_simple |> 
  augment() |> 
  ggplot(aes(year)) +
  geom_line(aes(y = avg_viewers_millions, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

# Forecasting Linear Piecewise ----------------------------------------------------------------
# The model explain 90% of the variance according to the R squared criterion.
# Constant variance.
# Some correlation, but not that much.
# Normally distributed residuals.
model_lm_piecew <- SuperBowl_ts |> 
  model(tslm_piecew = TSLM(formula = avg_viewers_millions ~ trend(knots = c(1990, 2005, 2018))))

model_lm_piecew |> 
  report()

model_lm_piecew |> 
  gg_tsresiduals()

model_lm_piecew |> 
  augment() |> 
  ggplot(aes(year)) +
  geom_line(aes(y = avg_viewers_millions, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

## Forecasting Exponential Smoothing ----------------------------------------------------------
# Using the Exponential Smoothing, we see a nice fit capturing most the original data.
# Using the tsresiduals() function, we can see that the residuals have:
# Mean around 0.
# Constant variance.
# Not correlated lags.

model_ets <- SuperBowl_ts |> 
  model(ets = ETS(formula = avg_viewers_millions ~ error("A") + trend("A") + season("N")))

model_ets |> 
  glance()

model_ets |> 
  gg_tsresiduals()

model_ets |> 
  augment() |> 
  ggplot(aes(year)) +
  geom_line(aes(y = avg_viewers_millions, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

## Forecasting Moving Averages ----------------------------------------------------------------
model_ma <- SuperBowl_ts |> 
  mutate(MA_3 = slider::slide_dbl(avg_viewers_millions, mean,
                                  .before = 3, .after = -1, .complete = TRUE))

model_ma |> 
  ggplot(aes(year)) +
  geom_line(aes(y = avg_viewers_millions, colour = "Data")) +
  geom_line(aes(y = MA_3, colour = "Fitted"))

# Forecasting Prediction ----------------------------------------------------------------------
# The simple linear model is optimistic giving a prediction.
# The piece wise approach give the pessimistic prediction.
# The exponential smoothing is between both.
# The moving average is not a defined model, so all the calculations are manual.
# Model   : Point Estimate - Prediction
# Simple  ; 116
# Piece  : 90.3
# ETS     : 102
# MA_3    : 102
model_lm_simple |> 
  forecast(h = "1 year")

model_lm_piecew |> 
  forecast(h = "1 year")

model_ets |> 
  forecast(h = "1 year")

model_ma |> 
  tail(3) |> 
  as_tibble() |> 
  summarise(forecast = sum(MA_3) / n())

model_lm_simple |> 
  forecast(h = "1 year") |> 
  autoplot(SuperBowl_ts) +
  labs(title = "Prediction for the viewership of the Super Bowl (2023)",
       subtitle = "Simple linear model. [Prediction of 116 million people]",
       x = "Average viewers (millions)",
       y =  "Year")

model_lm_piecew |> 
  forecast(h = "1 year") |> 
  autoplot(SuperBowl_ts) +
  labs(title = "Prediction for the viewership of the Super Bowl (2023)",
       subtitle = "Linear piece wise model. [Prediction of 90.3 million people]",
       x = "Average viewers (millions)",
       y =  "Year")

model_ets |> 
  forecast(h = "1 year") |> 
  autoplot(SuperBowl_ts) +
  labs(title = "Prediction for the viewership of the Super Bowl (2023)",
       subtitle = "Exponential Smoothing model. [Prediction of 102 million people]",
       x = "Average viewers (millions)",
       y =  "Year")

model_ma |> 
  autoplot(avg_viewers_millions) +
  annotate(geom = "point", x = 2023, y = 102, colour = "blue") + 
  labs(title = "Prediction for the viewership of the Super Bowl (2023)",
       subtitle = "3 Moving Average model. [Prediction of 102 million people]",
       x = "Average viewers (millions)",
       y =  "Year")
