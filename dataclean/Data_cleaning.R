library(tidyverse)

years <- 2019:2023
base_url <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_"

atp_raw <- map_dfr(years, function(yr) {
  url <- paste0(base_url, yr, ".csv")
  read_csv(url, show_col_types = FALSE)
})

# Keep only the columns we need, filter to top players and 3 surfaces
atp_clean <- atp_raw |>
  select(winner_name, loser_name, surface, tourney_date, winner_rank, loser_rank, ) |>
  filter(surface %in% c("Clay", "Hard", "Grass")) |>
  drop_na()

# Creating rank difference (loser - winner)
temp = atp_clean |>
  mutate(rank_diff = loser_rank - winner_rank, .keep = "unused")

# Pivoting longer to make every player have a row
long_df = temp |>
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) |>
  mutate(outcome = if_else(role == "winner_name", 1, 0)) |>  # 1 if winner, 0 if loser
  mutate(rank_diff = if_else(outcome == 1, rank_diff, -rank_diff)) |> # Rank_diff now opponent - player
  select(player, outcome, surface, tourney_date, rank_diff) # Selecting only variables of interest
long_df

# Converting the character values to unique numeric integers for jags to use
jags_df = long_df |>
  mutate(across(c(player, surface), as.factor)) |> # removed outcome factor (leave as original 0/1 encoder)
  mutate(across(c(player, surface), as.numeric)) |> 
  select(-tourney_date)

# Players who have played >= 50 games
top_players = jags_df |>
  group_by(player) |>
  summarize(count = n()) |>
  filter(count >= 50) |>
  pull(player)

top_df = jags_df |> 
  filter(player %in% top_players) |>
  mutate(player = as.numeric(as.factor(player))) # re-index


# Players who have played 10 <= games < 50
mid_players = jags_df |>
  group_by(player) |>
  summarize(count = n()) |>
  filter(count >= 10 & count <50) |>
  pull(player)

mid_df = jags_df |> 
  filter(player %in% mid_players) |>
  mutate(player = as.numeric(as.factor(player)))

