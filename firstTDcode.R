### Load in data
pbp <- nflreadr::load_pbp(2019:2022)
positions <- nflreadr::load_rosters(2019:2022)

### Filter to TDs and create first TD columns
pbp2 <- pbp |> 
  filter(!is.na(posteam)) |> 
  group_by(game_id) |> 
  mutate(first_pos_team = first(posteam)) |> 
  filter(touchdown == 1) |> 
  group_by(game_id) |> 
  mutate(
    first_td = first(td_player_id),
    first_td_team = ifelse(posteam == td_team, first(posteam), first(defteam)),
    first_td_team_all = ifelse(defteam != td_team, first(defteam), first(posteam))
  ) |> 
  group_by(game_id) |> 
  slice(1)

### Find number of games by season for each team
games <- pbp |>
  group_by(posteam, season) |> 
  summarise(
    games = n_distinct(game_id)
  ) |> filter(!is.na(posteam))

### Select position data to join with pbp
positions2 <- positions |> 
  select(full_name, position, team, gsis_id, season) |> 
  mutate(
    position = case_when(
      position %in% c('DB', 'DL', 'LB', 'SPEC') ~ 'DEF',
      TRUE ~ position
    )
  )   

pbp3 <- left_join(pbp2, positions2, by = c("first_td" = "gsis_id", "season")) 

### Number of first TDs by offense by run/pass
first_td_offense <- pbp3 |> 
  group_by(first_td_team, season) |> 
  summarise(
    first_td_times = sum(touchdown),
    first_td_rush = sum(rush_touchdown),
    first_td_pass = sum(pass_touchdown)
  )

### Number of first TDs by offense and position
first_td_pos <- pbp3 |> 
  group_by(first_td_team, position, season) |> 
  summarise(
    first_td_times = sum(touchdown),
  ) |> 
  pivot_wider(names_from = position, values_from = first_td_times) 

first_td_pos[is.na(first_td_pos)] <- 0

first_td_off_all <- left_join(first_td_offense, first_td_pos, by = c('first_td_team', 'season')) |> 
  left_join(games, by = c('first_td_team' = 'posteam', 'season'))

### Get first TD percentage by game
first_td_off_all <- first_td_off_all |> 
  mutate(
    first_td_perc = first_td_times/games
  )

first_td_off_all <- left_join(first_td_off_all, teams_colors_logos |> select(team_logo_espn, team_abbr), by = c('first_td_team' = 'team_abbr'))
first_td_off_all <- first_td_off_all |> 
  select(first_td_team, team_logo_espn, first_td_times, first_td_rush, first_td_pass, QB, RB, WR, TE, DEF, first_td_perc, season) |> 
  arrange(-first_td_times)

### Export to local folder
write_csv(first_td_off_all, "offensefirstTD.csv")

### Number of first TDs by defense by run/pass
first_td_defense <- pbp3 |> 
  group_by(first_td_team_all, season) |> 
  summarise(
    first_td_times = sum(touchdown),
    first_td_rush = sum(rush_touchdown),
    first_td_pass = sum(pass_touchdown)
  )

### Number of first TDs by defense and position
first_td_def <- pbp3 |> 
  group_by(first_td_team_all, position, season) |> 
  summarise(
    first_td_times = sum(touchdown),
  ) |> 
  pivot_wider(names_from = position, values_from = first_td_times)

first_td_def[is.na(first_td_def)] <- 0

first_td_def_all <- left_join(first_td_defense, first_td_def, by = c('first_td_team_all', 'season')) |> 
  left_join(games, by = c('first_td_team_all' = 'posteam', 'season'))

first_td_def_all <- first_td_def_all |> 
  mutate(
    first_td_all_perc = first_td_times/games
  )

first_td_def_all <- left_join(first_td_def_all, teams_colors_logos |> select(team_logo_espn, team_abbr), by = c('first_td_team_all' = 'team_abbr'))
first_td_def_all <- first_td_def_all |> 
  select(first_td_team_all, team_logo_espn, first_td_times, first_td_rush, first_td_pass, QB, RB, WR, TE, DEF, first_td_all_perc, season) |> 
  arrange(first_td_times)

write_csv(first_td_def_all, "defensefirstTD.csv")

### Get number of first TDs by player
players <- pbp3 |> 
  group_by(full_name, season) |> 
  summarise(
    offense = first(posteam),
    first_td_times = sum(touchdown),
    first_td_rush = sum(rush_touchdown),
    first_td_pass = sum(pass_touchdown)
  )

### Get overall number of first TDs by offense
first_td_offense <- pbp3 |> 
  group_by(first_td_team, season) |> 
  summarise(
    teamtds = sum(touchdown)
  ) |> rename(offense = "first_td_team")

playersjoin <- left_join(players, first_td_offense, by = c('offense', 'season'))

### Find number of team's first TDs the player scored
playersjoin <- playersjoin |> 
  mutate(
    teamTDperc = first_td_times/teamtds
  ) |> select(-teamtds) |> 
  arrange(-first_td_times)

playersjoin <- left_join(playersjoin, teams_colors_logos |> select(team_abbr, team_logo_espn), by = c('offense' = 'team_abbr')) |> 
  select(full_name, team_logo_espn, first_td_times, first_td_rush, first_td_pass, teamTDperc, season, offense)

write_csv(playersjoin, "playerleaders.csv")
