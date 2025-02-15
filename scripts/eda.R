library(tidyverse)
library(janitor)
library(usethis)
library(tsibble)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

my_data_raw <- read_csv("inputs/MyEBirdData.csv") |> 
  clean_names()

glimpse(my_data_raw)

my_data <- my_data_raw |> 
  rename(obs_date = date) |> 
  mutate(obs_date_ym = yearmonth(obs_date),
         obs_date_y = year(obs_date),
         obs_date_m = month(obs_date, label = TRUE, abbr = TRUE),
         obs_date_w = isoweek(obs_date))

my_data

glimpse(my_data)

#count of observations

obs_data <- my_data |> 
  distinct(submission_id, obs_date_ym, obs_date_y, obs_date_m, obs_date_w)

obs_data |> 
  count(obs_date_ym) |> 
  mutate(n_cumsum = cumsum(n)) |> 
  ggplot(aes(obs_date_ym, n_cumsum)) +
  geom_line()

obs_data |> 
  count(obs_date_ym) |> 
  ggplot(aes(obs_date_ym, n)) +
  geom_line()

obs_data |> 
  count(obs_date_y, obs_date_m) |> 
  complete(obs_date_y, obs_date_m) |> 
  replace_na(list(n = 0)) |> 
  ggplot(aes(obs_date_m, obs_date_y, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), transform = "reverse")

obs_data |> 
  count(obs_date_y, obs_date_w) |> 
  complete(obs_date_y, obs_date_w) |> 
  replace_na(list(n = 0)) |> 
  ggplot(aes(obs_date_w, obs_date_y, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

#lifers
lifer_df <- my_data |> 
  select(obs_date, common_name, state_province) |> 
  distinct() |> 
  arrange(state_province, obs_date) |> 
  group_by(common_name, state_province) |> 
  mutate(common_name_cumsum = dense_rank(obs_date)) |> 
  mutate(is_lifer = common_name_cumsum == 1) |> 
  filter(is_lifer == TRUE) |> 
  group_by(state_province) |> 
  mutate(lifer_cumsum = row_number()) |> 
  ungroup() |> 
  mutate(state_province = fct_infreq(state_province))

lifer_df

lifer_df |> 
  ggplot(aes(obs_date, lifer_cumsum, color = state_province, group = state_province)) +
  geom_line()