suppressMessages(library(tidyverse))

options(remove(list=ls()))
options(scipen=9999999)

game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary")

# Scrape ESPN API for play-by-play and win probability
# NOTE: This is adapted from Thomas Mock's espnscrapeR in order to only include
# relevant information (https://jthomasmock.github.io/espnscrapeR/)
get_nfl_pbp <- function(game_id){
  
  game_url <- glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary")
  
  raw_get <- httr::GET(game_url, query = list(event = game_id, enable = "ranks,odds,linescores,logos"))
  
  httr::stop_for_status(raw_get)
  
  raw_json <- httr::content(raw_get)
  
  nfl_pbp <- raw_json[["drives"]][["previous"]] %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    rename(drive_id = id) %>%
    unnest_wider(team) %>%
    select(-shortDisplayName) %>%
    hoist(logos, logo = list(1, "href")) %>%
    rename(
      pos_team_name = name, pos_team_abb = abbreviation,
      pos_team_full = displayName, drive_desc = description
    ) %>%
    unnest_wider(start) %>%
    rename(drive_start_yardline = yardLine, drive_start_text = text) %>%
    hoist(period,
          drive_start_qtr = "number"
    ) %>%
    select(-period, -logos) %>%
    hoist(clock,
          drive_start_clock = "displayValue"
    ) %>%
    unnest_wider(end) %>%
    hoist(period,
          drive_end_qtr = "number"
    ) %>%
    select(-period) %>%
    hoist(clock,
          drive_end_clock = "displayValue"
    ) %>%
    rename(drive_end_yardline = yardLine, drive_end_text = text) %>%
    hoist(timeElapsed, drive_time = "displayValue") %>%
    rename(
      drive_yds = yards, drive_result_score = isScore,
      drive_plays = offensivePlays, drive_result = result
    ) %>%
    select(-shortDisplayResult, -displayResult) %>%
    unnest_longer(plays) %>%
    unnest_wider(plays) %>%
    rename(
      play_id = id, play_desc = text, away_score = awayScore,
      home_score = homeScore, scoring_play = scoringPlay,
      yards_gained = statYardage
    ) %>%
    hoist(type, play_type = "text") %>% select(-type) %>%
    hoist(period, quarter = "number") %>%
    hoist(clock, clock_text = "displayValue") %>%
    select(-priority) %>%
    mutate(across(c(drive_result_score, scoring_play), as.integer)) %>%
    hoist(
      start,
      start_posteam_id = list("team", "id"),
      start_down = "down",
      start_ydstogo = "distance",
      start_yardline = "yardLine",
      start_ydsto_ez = "yardsToEndzone",
      start_text = "downDistanceText",
      start_down_text = "shortDownDistanceText",
      start_possess_text = "possessionText",
    ) %>%
    hoist(
      end,
      end_posteam_id = list("team", "id"),
      end_down = "down",
      end_ydstogo = "distance",
      end_yardline = "yardLine",
      end_ydsto_ez = "yardsToEndzone",
      end_text = "downDistanceText",
      end_down_text = "shortDownDistanceText",
      end_possess_text = "possessionText",
    ) %>%
    relocate(yards_gained, .after = "play_type") %>%
    select(-start, -end, -modified)
  
  game_header <- raw_json %>%
    keep(names(raw_json) %in% "header") %>%
    tibble(data = .) %>%
    unnest_wider(data) %>%
    unchop(competitions) %>%
    rename(game_id = id, game_uid = uid) %>%
    unnest_wider(competitions) %>%
    unnest_wider(season) %>%
    select(-id, -uid) %>%
    rename(season = year, season_type = type) %>%
    select(
      !any_of(
        c(
          "neutralSite", "conferenceCompetition", "boxscoreAvailable",
          "commentaryAvailable", "liveAvailable", "onWatchESPN", "recent",
          "boxscoreSource", "playByPlaySource"
        )
      )
    ) %>%
    hoist(
      competitors,
      home_team_name = list(1, "team", "name"),
      home_team_logo = list(1, "team", "logos", 1, "href"),
      home_team_abb = list(1, "team", "abbreviation"),
      home_team_id = list(1, "team", "id"),
      home_team_location = list(1, "team", "location"),
      home_team_full = list(1, "team", "displayName"),
      home_team_color = list(1, "team", "color"),
      home_team_color_alt = list(1, "team", "alternateColor"),
      home_score_final = list(1, "score"),
      home_win = list(1, "winner"),
      home_record = list(1, "record", 1, "summary"),
      # away team
      away_team_name = list(2, "team", "name"),
      away_team_logo = list(2, "team", "logos", 1, "href"),
      away_team_abb = list(2, "team", "abbreviation"),
      away_team_id = list(2, "team", "id"),
      away_team_location = list(2, "team", "location"),
      away_team_full = list(2, "team", "displayName"),
      away_team_color = list(2, "team", "color"),
      away_team_color_alt = list(2, "team", "alternateColor"),
      away_score_final = list(2, "score"),
      away_win = list(2, "winner"),
      away_record = list(2, "record", 1, "summary"),
    ) %>%
    mutate(home_win = as.integer(home_win),
           away_win = as.integer(away_win),) %>%
    select(!where(is.list), -timeValid)
  
  combo_df <- bind_cols(nfl_pbp, game_header) %>%
    select(
      week, season,
      drive_id, drive_result, season_type, pos_team_abb,
      play_id, play_type,
      yards_gained:scoring_play, start_down:start_text,
      end_down:end_text, game_id, season, home_team_full,
      home_team_abb, home_team_name, home_team_location,
      home_team_logo, home_team_color, 
      away_team_full, away_team_abb, away_team_location,
      away_team_name, away_team_logo, away_team_color
    ) %>%
    mutate(
      quarter_mins = str_extract(clock_text, '[^:]*'),
      quarter_secs = str_extract(clock_text, ':[0-9]*'),
      quarter_secs = substring(quarter_secs, 2, nchar(quarter_secs)),
      game_secs_remaining = case_when(
        quarter < 5 ~ 900*(4 - quarter) + as.numeric(quarter_mins)*60 + as.numeric(quarter_secs),
        TRUE ~ as.numeric(quarter_mins)*60 + as.numeric(quarter_secs)),
      half = case_when(
        quarter == 1 | quarter == 2 ~ 1,
        quarter == 3 | quarter == 4 ~ 2,
        quarter == 5 | quarter == 6 ~ 3,
        TRUE ~ 4
      )
    ) %>%
    group_by(game_id) %>%
    mutate(
      playnum = row_number(),
      game_secs_elapsed = case_when(
        quarter < 5 ~ 3600 - game_secs_remaining,
        quarter > 4 & season_type == 2 ~ 4200 - game_secs_remaining,
        TRUE ~ 3600 + 900*(quarter - 4) - game_secs_remaining)) %>%
    ungroup()
  
  if(raw_json[["header"]][["season"]][["year"]] >= 2015){
    
    wp_df <- raw_json[["winprobability"]] %>%
      tibble(data = .) %>%
      hoist(
        data,
        play_id = "playId",
        home_wp = "homeWinPercentage",
        tie_percentage = "tiePercentage",
        game_sec_remaining = "secondsLeft"
      ) %>%
      mutate(winprob = ifelse(home_wp < 0.5, home_wp - 1, home_wp))
    
    combo_df %>%
      left_join(wp_df, by = "play_id")
    
  } else {
    combo_df
  }
  
  
}

# Create chart for tweet
make_wp_chart <- function(id, play, plays){
  
  # Only chart plays up to the one in question
  chart_data <- plays %>%
    filter(game_id == id, playnum <= play)
  
  x_max <- ifelse(chart_data$quarter[play] > 4, 3600 + 900*(chart_data$quarter[play] - 4), 3600)
  wk <- ifelse(chart_data$season_type[play] == 3, chart_data$week[play] + 18, chart_data$week[play])
  wp_label <- ifelse(chart_data$home_wp[play] > 0.5,
                     paste0(chart_data$home_team_abb[1], ": ", round(chart_data$home_wp[play],3)*100, "%"),
                     paste0(chart_data$away_team_abb[1], ": ", 100 - round(chart_data$home_wp[play],3)*100, "%"))
  label_y = ifelse(chart_data$home_wp[play] > 0.5, chart_data$home_wp[play] + 0.05, chart_data$home_wp[play] - 0.05)
  
  chart_data %>%
    ggplot(aes(game_secs_elapsed, home_wp)) +
    ggimage::geom_image(aes(x = 100, y = 1), image = chart_data$home_team_logo[1], size = 0.25, asp = 2) +
    ggimage::geom_image(aes(x = 100, y = 0), image = chart_data$away_team_logo[1], size = 0.25, asp = 2) +
    geom_ribbon(aes(ymin = home_wp, ymax = 0.5), fill = "grey") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    geom_path(color = ifelse(chart_data$home_wp < 0.5, paste0("#", chart_data$away_team_color[1]), paste0("#", chart_data$home_team_color[1]))) +
    geom_point(color = ifelse(chart_data$home_wp < 0.5, paste0("#", chart_data$away_team_color[1]), paste0("#", chart_data$home_team_color[1]))) +
    scale_y_continuous(breaks = c(0,0.5,1), labels = c("100%","50%","100%")) +
    scale_x_continuous(limits = c(0,x_max), breaks = c(0,900,1800,2700,3600,4500), labels = c("0","15","30","45","60","75")) +
    theme_bw() +
    annotate(geom = "label", x = chart_data$game_secs_elapsed[play], y = label_y, label = wp_label) +
    labs(
      x = "Game minutes elapsed",
      y = "Win probability",
      title = paste0("Win Probability Map for ", chart_data$home_team_abb[1], " vs. ", chart_data$away_team_abb[1],
                     " Wk ", wk, " ", chart_data$season[1]),
      subtitle = paste0("Q", chart_data$quarter[play], " (", chart_data$clock_text[play], ") ",
                        chart_data$away_team_abb[play], " ", chart_data$away_score[play], " @ ",
                        chart_data$home_team_abb[play], " ", chart_data$home_score[play]),
      caption = "Figure: @Tucker_Tnl, Model: ESPN"
    )
  
}

# get live games
live_games <- readRDS(url(
  "http://www.habitatring.com/games_alt.rds"
  )) %>%
  # This is just for manual selection of games
  # dplyr::filter(espn == "400927752" | espn == "400999172" | espn == "400749515")
  dplyr::filter(
    
    # hasn't finished yet
    # is.na(result),
    
    # happening today
    gameday == as.character(lubridate::today("US/Pacific"))
    
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
  old_plays <- readr::read_csv("bot/old_plays.csv") %>%
    mutate(game_id = as.character(game_id),
           play_id = as.character(play_id))
  
  # get updated plays from ongoing games
  plays <- purrr::map_df(1 : nrow(live_games), function(x) {
    
    message(glue::glue("{x}: game {live_games %>% dplyr::slice(x) %>% pull(espn)}"))
    get_nfl_pbp(live_games %>% dplyr::slice(x) %>% pull(espn))
    
  })
  
  # pare down plays to only include important ones
  # and make sure PAT has been attempted before posting score
  important_plays <- plays %>%
    mutate(play_desc = stringr::str_replace(play_desc, "\\([:digit:]*\\:[:digit:]+\\)\\s", "")) %>%
    filter(grepl("Interception", play_type) |
             grepl("Blocked", play_type, ignore.case = T) |
             grepl("FUMBLES", play_desc, ignore.case = T) |
             grepl("Field Goal", play_type, ignore.case = T) |
             (start_down == 4 & !grepl("Punt", play_type, ignore.case = T) & 
                play_type != "End of Half" & play_type != "Penalty" & 
                play_type != "End Period" & play_type != "Timeout" &
                play_type != "End of Half" & play_type != "End of Regulation" &
                play_type != "End of Game" & play_type != "Two-minute warning") |
             (scoring_play == 1  & (grepl("extra point", play_desc, ignore.case = T) |
                                      grepl("kick", play_desc, ignore.case = T) |
                                      grepl("two-point", play_desc, ignore.case = T))))
  
  # save updated list of plays we've done
  important_plays %>%
    mutate(game_id = as.character(game_id)) %>%
    select(game_id, play_id) %>%
    mutate(old = 1) %>%
    rbind(old_plays) %>%
    distinct() %>%
    readr::write_csv("bot/old_plays.csv")
  
  # get plays we haven't tweeted yet
  for_tweeting <- important_plays %>%
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
      game_id <- df$game_id
      playnum <- df$playnum
      
      # Make chart
      wp_chart <- make_wp_chart(game_id, playnum, plays)
      ggsave("bot/wp_chart.png", width = 2500, height = 1500, units = "px", device = "png")
      
      play_desc <- df$play_desc %>% substr(1, 190)
      posteam <- df$pos_team_abb
      defteam <- if_else(df$pos_team_abb == df$home_team_abb, df$away_team_abb, df$home_team_abb)
      # wpa_direction <- ifelse(df$wpa > 0, "+", "")
      home_team_price <- ifelse(df$home_wp < 0.5, 100/df$home_wp - 100, -100*df$home_wp/(1-df$home_wp))
      away_team_price <- ifelse((1-df$home_wp) < 0.5, 100/(1-df$home_wp) - 100, -100*(1-df$home_wp)/(df$home_wp))
      home_team_price <- ifelse(home_team_price > 0, paste0("+", round(home_team_price)), round(home_team_price))
      away_team_price <- ifelse(away_team_price > 0, paste0("+", round(away_team_price)), round(away_team_price))
      
      text <-
        glue::glue("{df$pos_team_abb} {df$start_text}
        
        Q{df$quarter} ({df$clock_text}) {play_desc}
        
        {df$away_team_abb} {df$away_score} @ {df$home_team_abb} {df$home_score}
        
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
      rtweet::post_tweet(text, media = "bot/wp_chart.png", token = token)
    }
    
  }
  
}

print(paste("Bot script successfully run at", lubridate::now()))