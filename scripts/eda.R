library(tidyverse)
library(janitor)
library(usethis)
library(tsibble)
library(broom)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

my_data_raw <- read_csv("inputs/MyEBirdData.csv") |> 
  clean_names()

glimpse(my_data_raw)

my_data <- my_data_raw |> 
  clean_names() |> 
  rename(obs_date = date,
         species_count = count) |> 
  mutate(obs_date_ym = yearmonth(obs_date),
         obs_date_yw = yearweek(obs_date),
         obs_date_y = year(obs_date),
         obs_date_m = month(obs_date, label = TRUE, abbr = TRUE) |> as.character() |> factor(levels = month.abb),
         obs_date_w = isoweek(obs_date),
         obs_date_wday = wday(obs_date, label = TRUE, abbr = TRUE),
         obs_date_hour = hour(time)) |> 
  arrange(submission_id)

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

#species detection
species_detection <- my_data |> 
  filter(all_obs_reported == 1) |> 
  select(submission_id,, protocol, state_province, location, starts_with("obs_date"),
         number_of_observers, duration_min, distance_traveled_km, common_name) |> 
  group_by(submission_id, protocol, state_province, location, obs_date_m, obs_date_wday, obs_date_hour,
           number_of_observers, duration_min, distance_traveled_km) |> 
  summarize(species_count = n_distinct(common_name)) |> 
  ungroup() |> 
  mutate(flag_travelling = !is.na(distance_traveled_km)) |> 
  replace_na(list(distance_traveled_km = 0))

species_detection  |> 
  filter(flag_travelling == TRUE) |> 
  # filter(percent_rank(distance_traveled_km) < .99,
  #        percent_rank(duration_min) < .99) |> 
  ggplot(aes(distance_traveled_km, duration_min, size = species_count)) +
  geom_jitter(alpha = .3)
 
species_detection |>
  filter(flag_travelling == TRUE) |> 
  pivot_longer(cols = c(duration_min, distance_traveled_km), names_to = "effort_metric") |> 
  group_by(effort_metric) |> 
  # filter(percent_rank(value) < .99) |> 
  ungroup() |> 
  ggplot(aes(value, species_count)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  facet_wrap(vars(effort_metric), scales = "free_x")

species_detection_model_df <- species_detection |> 
  mutate(flag_my_patch = location == "Backyard") |> 
  mutate(cumulative_checklist_no = dense_rank(submission_id)) |> 
  mutate(hour_block = cut_width(obs_date_hour, width = 6)) |> 
  mutate(flag_my_state = state_province == "US-PA")

species_detection_model_df |> 
  pull(hour_block) |> 
  levels()

data_train <- slice_sample(species_detection_model_df, prop = .8, by = species_count)

data_test <- anti_join(species_detection_model_df, data_train, by = join_by(submission_id))

bind_rows(data_train |> mutate(set = "train"),
          data_test |> mutate(set = "test")) |> 
  ggplot(aes(species_count, fill = set)) +
  geom_histogram()

glimpse(species_detection_model_df)

species_detection_model <- lm(species_count ~ log(distance_traveled_km + 1) * duration_min + obs_date_m + hour_block + flag_my_patch +
                                cumulative_checklist_no + flag_my_state + log(number_of_observers) + protocol,
                              data = data_train)

glance(species_detection_model)

tidy(species_detection_model)

tidy(species_detection_model) |> 
  arrange(desc(estimate)) |> 
  view()

tidy(species_detection_model) |> 
  arrange(desc(estimate)) |> 
  filter(p.value < .05)

augment(species_detection_model) |> 
  ggplot(aes(species_count, .fitted)) +
  geom_abline() +
  geom_jitter(alpha = .3) +
  geom_smooth(aes(color = flag_my_state), method = "lm") +
  #facet_wrap(vars(flag_my_state)) +
  tune::coord_obs_pred()

preds <- predict(species_detection_model, data_test)

data_test_res <- data_test |> 
  mutate(.pred = preds,
         .resid = species_count - .pred)

data_test_res |> 
  summarize(rmse = yardstick::rmse_vec(species_count, .pred))

data_test_res |> 
  ggplot(aes(species_count, .pred)) +
  geom_abline() +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  tune::coord_obs_pred()

data_test_res |> 
  select(where(is.numeric)) |> 
  select(-starts_with("obs_date"), -species_count) |> 
  pivot_longer(-.resid) |> 
  ggplot(aes(value, .resid)) +
  geom_jitter(alpha = .5) +
  geom_smooth() +
  facet_wrap(vars(name), scales = "free_x")

data_test_res |> 
  select(where(is.factor), .resid) |> 
  select(-starts_with("obs_date")) |> 
  pivot_longer(-.resid) |> 
  ggplot(aes(value, .resid, group = value)) +
  geom_boxplot() +
  geom_jitter(alpha = .3) +
  facet_wrap(vars(name), scales = "free_x")
