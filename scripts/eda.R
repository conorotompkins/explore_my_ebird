library(tidyverse)
library(janitor)
library(usethis)
library(tsibble)
library(broom)
library(here)
library(GGally)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

my_data_raw <- here("inputs/MyEBirdData.csv") |>
  read_csv() |>
  clean_names()

glimpse(my_data_raw)

my_data <- my_data_raw |>
  clean_names() |>
  rename(obs_date = date, species_count = count) |>
  mutate(
    obs_date_ym = yearmonth(obs_date),
    obs_date_yw = yearweek(obs_date),
    obs_date_y = year(obs_date),
    obs_date_m = month(obs_date, label = TRUE, abbr = TRUE) |>
      as.character() |>
      factor(levels = month.abb),
    obs_date_w = isoweek(obs_date),
    obs_date_wday = wday(obs_date, label = TRUE, abbr = TRUE),
    obs_date_hour = hour(time)
  ) |>
  arrange(submission_id)

glimpse(my_data)

#complete checklist
my_data |>
  distinct(submission_id, all_obs_reported) |>
  count(all_obs_reported)

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
  ggplot(aes(
    obs_date,
    lifer_cumsum,
    color = state_province,
    group = state_province
  )) +
  geom_line()


lifer_df |>
  select(obs_date, common_name) |>
  distinct() |>
  arrange(common_name, obs_date) |>
  group_by(common_name) |>
  mutate(common_name_cumsum = dense_rank(obs_date)) |>
  mutate(is_lifer = common_name_cumsum == 1) |>
  filter(is_lifer == TRUE) |>
  ungroup() |>
  mutate(lifer_cumsum = row_number())

#species detection
species_detection <- my_data |>
  filter(all_obs_reported == 1) |>
  select(
    submission_id,
    protocol,
    state_province,
    location,
    starts_with("obs_date"),
    number_of_observers,
    duration_min,
    distance_traveled_km,
    common_name
  ) |>
  group_by(
    submission_id,
    protocol,
    state_province,
    location,
    obs_date_m,
    obs_date_wday,
    obs_date_hour,
    number_of_observers,
    duration_min,
    distance_traveled_km
  ) |>
  summarize(species_count = n_distinct(common_name)) |>
  ungroup() |>
  mutate(flag_travelling = !is.na(distance_traveled_km)) |>
  replace_na(list(distance_traveled_km = 0))

species_detection |>
  ggplot(aes(species_count)) +
  geom_histogram()

species_detection |>
  ggplot(aes(species_count)) +
  geom_histogram() +
  scale_x_continuous(transform = "log")

species_detection |>
  ggplot(aes(species_count)) +
  geom_histogram() +
  scale_x_log10()

species_detection |>
  filter(flag_travelling == TRUE) |>
  # filter(percent_rank(distance_traveled_km) < .99,
  #        percent_rank(duration_min) < .99) |>
  ggplot(aes(distance_traveled_km, duration_min, size = species_count)) +
  geom_jitter(alpha = .3)

species_detection |>
  filter(flag_travelling == TRUE) |>
  pivot_longer(
    cols = c(duration_min, distance_traveled_km),
    names_to = "effort_metric"
  ) |>
  group_by(effort_metric) |>
  # filter(percent_rank(value) < .99) |>
  ungroup() |>
  ggplot(aes(value, species_count)) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(effort_metric), scales = "free_x") +
  scale_y_continuous(transform = "log") #+
#scale_x_continuous(transform = "log")

species_detection_model_df <- species_detection |>
  mutate(flag_my_patch = location == "Backyard") |>
  mutate(cumulative_checklist_no = dense_rank(submission_id)) |>
  mutate(
    hour_block = case_when(
      between(obs_date_hour, 0, 3) ~ "12am-3am",
      between(obs_date_hour, 4, 7) ~ "4am-7am",
      between(obs_date_hour, 8, 11) ~ "8am-11am",
      between(obs_date_hour, 12, 15) ~ "12pm-3pm",
      between(obs_date_hour, 16, 19) ~ "4pm-7pm",
      between(obs_date_hour, 20, 23) ~ "8pm-11pm"
    ),
    hour_block = factor(
      hour_block,
      levels = c(
        "12am-3am",
        "4am-7am",
        "8am-11am",
        "12pm-3pm",
        "4pm-7pm",
        "8pm-11pm"
      )
    )
  ) |>
  mutate(flag_my_state = state_province == "US-PA") |>
  mutate(species_count_log10 = log10(species_count))

species_detection_model_df |>
  count(hour_block, obs_date_hour) |>
  arrange(obs_date_hour)

species_detection_model_df |>
  pull(hour_block) |>
  levels()

data_train <- slice_sample(
  species_detection_model_df,
  prop = .8,
  by = species_count_log10
)

data_test <- anti_join(
  species_detection_model_df,
  data_train,
  by = join_by(submission_id)
)

bind_rows(
  data_train |> mutate(set = "train"),
  data_test |> mutate(set = "test")
) |>
  ggplot(aes(species_count_log10, fill = set)) +
  geom_histogram()

glimpse(species_detection_model_df)


lower_gg <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    #geom_point(alpha = .1, size = .1)
    geom_bin_2d(bins = 20) +
    scale_fill_viridis_c()
  p
}

species_detection_model_df |>
  select(where(is.numeric)) |>
  ggpairs(lower = list(continuous = lower_gg))


species_detection_model <- lm(
  species_count ~ log(distance_traveled_km + 1) * duration_min + obs_date_m + hour_block + flag_my_patch + cumulative_checklist_no + flag_my_state + log(number_of_observers) + protocol,
  data = data_train
)

glance(species_detection_model)

tidy(species_detection_model)

tidy(species_detection_model) |>
  arrange(desc(estimate))

tidy(species_detection_model) |>
  arrange(desc(estimate)) |>
  filter(p.value < .05)

augment(species_detection_model) |>
  #mutate(species_count = 10^species_count_log10, .fitted = 10^.fitted) |>
  ggplot(aes(species_count, .fitted)) +
  geom_abline() +
  geom_jitter(alpha = .3) +
  geom_smooth(aes(color = flag_my_state), method = "lm") +
  #facet_wrap(vars(flag_my_state)) +
  tune::coord_obs_pred()

preds <- predict(species_detection_model, data_test)

data_test_res <- data_test |>
  mutate(.pred = preds, .resid = species_count - .pred)

data_test_res |>
  summarize(rmse = yardstick::rmse_vec(species_count, .pred))

data_test_res |>
  ggplot(aes(species_count, .pred)) +
  geom_abline() +
  geom_jitter(alpha = .3) +
  #geom_smooth(method = "lm") +
  tune::coord_obs_pred()

data_test_res |>
  select(where(is.numeric)) |>
  select(-starts_with("obs_date"), -c(species_count, .pred)) |>
  pivot_longer(-.resid) |>
  ggplot(aes(value, .resid)) +
  geom_jitter(alpha = .2) +
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
