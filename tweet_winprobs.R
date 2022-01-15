library(tidyverse)
library(espnscrapeR)

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
    # there's probably a better way to do this but it seems to work
    current_hour = lubridate::hour(lubridate::now()),
    current_minute = lubridate::minute(lubridate::now()),
    game_hour = as.integer(substr(gametime, 1, 2)),
    game_minute = as.integer(substr(gametime, 4, 5)),
    # has already started
    started = dplyr::case_when(
      current_hour > game_hour ~ 1,
      current_hour == game_hour & current_minute >= game_minute + 5 ~ 1,
      TRUE ~ 0
    ),
    #
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
  dplyr::filter(started == 1) %>%
  # dplyr::filter(grepl("2016", game_id, fixed = T)) %>%
  # dplyr::filter(espn == "400927752") %>%
  dplyr::select(game_id, espn, home_team, away_team, week)



if (nrow(live_games) > 0) {
  
  # # get all the 4th down functions here
  # source('scripts/helpers.R')
  
  # get list of old plays before we do anything
  if (file.exists("old_plays.rds")) {
    
    # read the file if it exists
    old_plays <- readRDS("old_plays.rds")
    
    # if it's just an empty df, make a dummy df
    # this prevents errors down the line
    if (!"game_id" %in% names(old_plays)) {
      old_plays <- tibble::tibble(
        "game_id" = as.character("XXXXXX"),
        "play_id" = as.character(0),
        "old" = as.integer(1)
      )
      # if existing plays file looks okay, take game id and index
    } else {
      old_plays <- old_plays %>%
        dplyr::select(game_id, play_id) %>%
        dplyr::mutate(old = 1)
    }
  } else {
    # if file doesn't exist, make the dummy df to prevent join errors later
    # this is so we can remove the file if we want to start over
    old_plays <- tibble::tibble(
      "game_id" = as.character("XXXXXX"),
      "play_id" = as.character(0),
      "old" = as.integer(1)
    )
  }
  
  # get updated plays from ongoing games
  plays <- purrr::map_df(1 : nrow(live_games), function(x) {
    
    message(glue::glue("{x}: game {live_games %>% dplyr::slice(x) %>% pull(espn)}"))
    calculate_winprobs(espnscrapeR::get_nfl_pbp(live_games %>% dplyr::slice(x) %>% pull(espn)))
    
  })
  
  plays <- plays %>%
    filter(scoring_play == 1 |
             grepl("Interception", play_type, fixed = T) |
             play_type == "Fumble Recovery (Opponent)" |
             (start_down == 4 & !grepl("Punt", play_type, fixed = T) & play_type != "Penalty" & play_type != "End Period" & play_type != "End of Game"))
  
  # save updated list of plays we've done
  saveRDS(plays, "old_plays.rds")
  
  # get plays we haven't tweeted yet
  for_tweeting <- plays %>%
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
        play_desc <- df$play_desc %>% substr(1, 100)
        posteam <- df$pos_team_abb
        defteam <- if_else(df$pos_team_abb == df$home_team_abb, df$away_team_abb, df$home_team_abb)
        wpa_direction <- ifelse(df$wpa > 0, "+", "")
        home_team_price <- ifelse(df$home_wp < 0.5, 100/df$home_wp - 100, -100*df$home_wp/(1-df$home_wp))
        away_team_price <- ifelse((1-df$home_wp) < 0.5, 100/(1-df$home_wp) - 100, -100*(1-df$home_wp)/(df$home_wp))
        home_team_price <- ifelse(home_team_price > 0, paste0("+", round(home_team_price)), round(home_team_price))
        away_team_price <- ifelse(away_team_price > 0, paste0("+", round(away_team_price)), round(away_team_price))
        
        # table <- make_table(tableData, df)
        # 
        # table %>% gtsave("bot/post.png")
        
        text <-
          glue::glue("{df$away_team_abb} {df$away_score} @ {df$home_team_abb} {df$home_score}
        
        {df$pos_team_abb} {df$start_text}
        
        Q{df$quarter} {play_desc}
        
        WP effect: {df$home_team_abb} {wpa_direction}{round(df$wpa*100, 1)}
        
        {df$home_team_abb} {round(df$home_wp*100, 1)}% ({home_team_price})
        {df$away_team_abb} {100 - round(df$home_wp*100, 1)}% ({away_team_price})")
        
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
