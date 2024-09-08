library(tidyverse)
library(httr)
library(slider)

USER_ID <- "1128735683528507392"
LEAGUE_ID <- "1128738585244545024"
DRAFT_ID <- "1128738586225946624"
MOCK_DRAFT_ID <- "1136332496670453760"

# NOTES FOR NEXT YEAR
# Print out ESPN's projected team offense rankings. Super helpful for tiebreakers.
# Figure out where the risks are. Print out the QBs for each team.
# Add all the rookie rankings from The Athletic to your draft queue
# Calculate VORP using ADP within the top 100 or so, not just next 15
# Get multiple sources and average them out.
# Consider using VORP based on ranking instead of total points
# Get all the stats I need in one sheet in R! Offense rank, average player rank, ADP, etc
# Make sure to do the data cleaning so the player join key works 100% of the time


# HELPERS

get_drafted_player <- function(player) {
  tibble(
    first_name = player$metadata$first_name,
    last_name = player$metadata$last_name,
    player_name = paste(player$metadata$first_name, player$metadata$last_name),
    team = player$metadata$team,
    position = player$metadata$position
  )
}

mutate_key <- function(tbl) {
  tbl %>%
    mutate(
      key = tolower(player_name),
      key = str_replace_all(key, '[^a-z]', ''),
      key = str_replace_all(key, "jr", ''),
      key = str_replace_all(key, "sr", ''),
      key = str_replace_all(key, "i{2,}", ''),
      key = paste0(key, "__", team))
}

# READ ALL RANKINGS

csv_files <- list.files(path = "~/Repos/misc/fantasy/2024_rankings", pattern = "*.csv", full.names = TRUE)

player_rankings <- 
  csv_files %>%
  map_df(
    ~read_csv(.x) %>%
      rename_all(snakecase::to_snake_case) %>%
      mutate(position = str_remove(basename(.x), ".csv")) %>%
      select(rk, tiers, player_name, team, fantasypts, position)) %>%
  mutate(team = if_else(team == "JAC", "JAX", team)) %>%
  mutate_key() %>%
  mutate(key = if_else(key == "marquisebrown__KC", "hollywoodbrown__KC", key))

# GET DRAFTED PLAYERS

vorp_all <- function(){
  
  r <- GET(paste0("https://api.sleeper.app/v1/draft/", DRAFT_ID, "/picks"))
  
  drafted_players <- 
    r %>%
    content() %>%
    map_dfr(get_drafted_player) %>%
    mutate_key()
  
  # Check for mismatch
  # drafted_players %>%
  #   anti_join(player_rankings, by = "key")
  
  vorp <- player_rankings %>%
    anti_join(drafted_players, by = "key") %>%
    arrange(position, desc(fantasypts)) %>%
    group_by(position) %>%
    arrange(position, desc(fantasypts)) %>%
    mutate(
      avg_next_15 = slide_dbl(fantasypts, ~ mean(.x, na.rm = TRUE), .before = 1, .after = 15),
      n_in_position = n(),
      pts_in_position = sum(fantasypts, na.rm = T)) %>%
    ungroup() %>%
    mutate(net_vorp = fantasypts - ((pts_in_position - fantasypts) / (n_in_position - 1)),
           vorp = fantasypts - avg_next_15) %>%
    arrange(position, desc(vorp)) %>%
    select(-avg_next_15, -n_in_position, -pts_in_position)
  
  vorp %>% 
    filter(
      position != "ALL",
      position != "QB") %>%
    View("ALL")
  
  vorp %>% 
    filter(position == "WR") %>%
    View("WR")
  
  vorp %>% 
    filter(position == "RB") %>%
    View("RB")
  
  vorp %>% 
    filter(position == "QB") %>%
    View("QB")
  
  vorp %>% 
    filter(position == "TE") %>%
    View("TE")
  
  vorp %>% 
    filter(position == "K") %>%
    View("K")
}

vorp_all()
