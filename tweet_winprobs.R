library(tidyverse)
library(espnscrapeR)

options(remove(list=ls()))
options(scipen=9999999)

game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary")

calculate_winprobs <- function(pbp){
  
  n <- nrow(pbp)
  
  pbp %>%
    group_by(game_id) %>%
    mutate(wpa = home_wp - lag(home_wp)) %>%
    ungroup()
  
}

# get live games
live_games <- readRDS(url(
  "http://www.habitatring.com/games_alt.rds"
  )) %>%
  dplyr::filter(

    # hasn't finished yet
    is.na(result),

    # happening today
    gameday == as.character(lubridate::today("US/Pacific"))

  ) %>%
  dplyr::mutate(
    espn = dplyr::case_when(
      # hard code for playoff games not in Lee's file
      game_id == "2021_19_LV_CIN"   ~ "401326627",
      game_id == "2021_19_NE_BUF"   ~ "401326626",
      game_id == "2021_19_PHI_TB"   ~ "401326630",
      game_id == "2021_19_SF_DAL"   ~ "401326629",
      game_id == "2021_19_PIT_KC"   ~ "401326628",
      game_id == "2021_19_ARI_LA"   ~ "401326625",
      TRUE ~ espn
    )
  ) %>%
  rowwise() %>%
  ## Check that drive info exists on ESPN (i.e., that it's started)
  mutate(
    started = !is.null(httr::content(httr::GET(game_url, query = list(event = espn)))[["drives"]][["previous"]])
  ) %>%
  ungroup() %>%
  dplyr::filter(started == 1) %>%
  dplyr::select(game_id, espn, home_team, away_team, week)

if (nrow(live_games) > 0) {
  
  # read file of old plays
  old_plays <- readr::read_csv("data/old_plays.csv") %>%
    mutate(game_id = as.character(game_id),
           play_id = as.character(play_id))
  
  # get updated plays from ongoing games
  plays <- purrr::map_df(1 : nrow(live_games), function(x) {
    
    message(glue::glue("{x}: game {live_games %>% dplyr::slice(x) %>% pull(espn)}"))
    espnscrapeR::get_nfl_pbp(live_games %>% dplyr::slice(x) %>% pull(espn))
    
  })
  
  plays <- plays %>%
    mutate(play_desc = stringr::str_replace(play_desc, "\\([:digit:]*\\:[:digit:]+\\)\\s", "")) %>%
    filter(scoring_play == 1 |
             grepl("Interception", play_type, fixed = T) |
             play_type == "Fumble Recovery (Opponent)" |
             play_type == "Sack" |
             (start_down == 4 & !grepl("Punt", play_type, fixed = T) & 
                play_type != "End of Half" & play_type != "Penalty" & 
                play_type != "End Period" & play_type != "Timeout" &
                play_type != "End of Game" & play_type != "Two-minute warning"))
  
  # save updated list of plays we've done
  plays %>%
    mutate(game_id = as.character(game_id)) %>%
    select(game_id, play_id) %>%
    mutate(old = 1) %>%
    rbind(old_plays) %>%
    distinct() %>%
    readr::write_csv("data/old_plays.csv")
  
  # get plays we haven't tweeted yet
  for_tweeting <- plays %>%
    mutate(game_id = as.character(game_id),
           play_id = as.character(play_id)) %>%
    left_join(old_plays, by = c("game_id","play_id")) %>%
    filter(is.na(old))
  
  # if there are plays to tweet, load the library and tweet
  if (nrow(for_tweeting) > 0) {
    
    suppressMessages(
      library(rtweet)
    )
    
    # do the thing
    for (x in 1:nrow(for_tweeting)) {
      
      df <- for_tweeting %>% dplyr::slice(x)
      play_desc <- df$play_desc %>% substr(1, 200)
      posteam <- df$pos_team_abb
      defteam <- if_else(df$pos_team_abb == df$home_team_abb, df$away_team_abb, df$home_team_abb)
      wpa_direction <- ifelse(df$wpa > 0, "+", "")
      home_team_price <- ifelse(df$home_wp < 0.5, 100/df$home_wp - 100, -100*df$home_wp/(1-df$home_wp))
      away_team_price <- ifelse((1-df$home_wp) < 0.5, 100/(1-df$home_wp) - 100, -100*(1-df$home_wp)/(df$home_wp))
      home_team_price <- ifelse(home_team_price > 0, paste0("+", round(home_team_price)), round(home_team_price))
      away_team_price <- ifelse(away_team_price > 0, paste0("+", round(away_team_price)), round(away_team_price))
      
      text <-
        glue::glue("{df$away_team_abb} {df$away_score} @ {df$home_team_abb} {df$home_score}
        
        {df$pos_team_abb} {df$start_text}
        
        Q{df$quarter} ({df$clock_text}) {play_desc}
        
        {df$home_team_abb}: {round(df$home_wp*100, 1)}% ({home_team_price})
        {df$away_team_abb}: {round(100 - df$home_wp*100, 1)}% ({away_team_price})")
      
      token <- rtweet::create_token(
        app = "nflwinbot",  # the name of the Twitter app
        consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
        consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
        access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
        access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
      )
      
      # Example: post a tweet via the API 
      # The keys will are in your environment thanks to create_token()
      rtweet::post_tweet(text, token = token)
      
    }
    
  }
  
}