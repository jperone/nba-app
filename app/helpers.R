
get_todays_games <- function(start_date, end_date) {
  todays_games_teams <- tibble()
  
  todays_games <- GET(glue("https://www.balldontlie.io/api/v1/games?start_date={start_date}&end_date={end_date}&per_page=100")) %>% 
    content() %>% 
    .$data
  
  for (i in seq_along(todays_games)) {
    todays_games[[i]]$home_team <- todays_games[[i]]$home_team$full_name
    todays_games[[i]]$visitor_team <- todays_games[[i]]$visitor_team$full_name
    
  }
  
  todays_games %<>% 
    roomba(cols = c("id", "date", "home_team", "home_team_score", "visitor_team", "visitor_team_score")) %>% 
    rename(game = id)
  
  todays_games_teams %<>% 
    bind_rows(todays_games)
  
  return(todays_games_teams)
  
}


get_players <- function() {
  
  all_players <- tibble()
  
  total_pages_players <- GET(glue("https://www.balldontlie.io/api/v1/players?per_page=100")) %>% 
    content() %>% 
    .$meta %>% 
    .$total_pages
  
  for (i in 1:total_pages_players) {
    players <- GET(glue("https://www.balldontlie.io/api/v1/players?per_page=100&page={i}")) %>% 
      content() %>% 
      .$data
    
    player_team <- tibble(team_id = rep("", length(players)))
    
    for (i in seq_along(players)) {
      player_team_id <- players[[i]]$team$full_name
      
      player_team$team_id[i] <- player_team_id
    }
    
    players %<>% 
      roomba::roomba(cols = c("id", "first_name", "height_feet", "height_inches", 
                              "last_name", "position", "weight_pounds")) %>% 
      bind_cols(player_team)
    
    all_players %<>% 
      bind_rows(players)
  }
  
  all_players %<>% 
    rename(player = id)
  
  return(all_players)
}


get_games <- function(start_date, end_date) {
  
  games_pages <- GET(glue("https://www.balldontlie.io/api/v1/games?start_date={start_date}&end_date={end_date}&per_page=100")) %>% 
    content() %>% 
    .$meta %>% 
    .$total_pages
  
  games <- tibble()
  
  for (page in 1:games_pages) {
    games_df <- GET(glue("https://www.balldontlie.io/api/v1/games?start_date={start_date}&end_date={end_date}&per_page=100&page={page}")) %>% 
      content() %>% 
      .$data
    
    for (i in seq_along(games_df)) {
      games_df[[i]]$home_team <- games_df[[i]]$home_team$full_name
      games_df[[i]]$visitor_team <- games_df[[i]]$visitor_team$full_name
      
    }
    
    games_df %<>% 
      roomba(cols = c("id", "date", "home_team", "home_team_score", "visitor_team", "visitor_team_score")) %>% 
      rename(game = id)
    
    games %<>% 
      bind_rows(games_df)
  }
  return(games)
  
}

get_stats <- function(players_data, games_data, todays_games_data, start_date, end_date) {
  
  ### stats
  page_num <- GET(glue("https://www.balldontlie.io/api/v1/stats?per_page=100&start_date={start_date}&end_date={end_date}")) %>% 
    content() %>% 
    .$meta %>% 
    .$total_pages
  
  stats <- tibble()
  
  for (i in 1:page_num) {
    stat_curr <- GET(glue("https://www.balldontlie.io/api/v1/stats?per_page=100&page={i}&start_date={start_date}&end_date={end_date}")) %>% 
      content() %>% 
      .$data
    
    for (j in seq_along(stat_curr)) {
      stat_curr[[j]]$game <- stat_curr[[j]]$game$id
      stat_curr[[j]]$player <- stat_curr[[j]]$player$id
      stat_curr[[j]]$team <- stat_curr[[j]]$team$id
      
    }
    
    stat_curr %<>% 
      roomba(cols = c("id", "ast", "blk", "dreb", "fg3_pct", "fg3a", "fg3m", "fg_pct",
                      "fga", "fgm", "ft_pct", "fta", "ftm", "game", "min", "oreb", "pf",
                      "player", "pts", "reb", "stl", "team", "turnover"))
    
    stats %<>% 
      bind_rows(stat_curr)
    
  }
  
  
  stats %<>% 
    left_join(players_data, by = "player") %>% 
    left_join(games_data, by = "game")
  
  all_stats <- stats %>% 
    filter(min != "", min != "0:00") %>% 
    mutate(full_name = paste(first_name, last_name)) %>% 
    separate(min, c("mins", "secs"), sep = ":")
  
  
  agg_stats <- all_stats %>% 
    group_by(full_name) %>% 
    summarise(team = max(team_id),
              minutes = sum(as.numeric(mins)),
              fga = sum(fga),
              fgm = sum(fgm),
              pts = sum(pts),
              # turnovers = sum(turnover),
              # pfs = sum(pf),
              rebs = sum(reb),
              ast = sum(ast),
              blk = sum(blk),
              fg3a = sum(fg3a),
              fg3m = sum(fg3m),
              # fta = sum(fta),
              # ftm = sum(ftm),
              stl = sum(stl)) %>% 
    mutate(stlblk = blk + stl,
           rebast = rebs + ast) %>% 
    filter(team %in% c(todays_games_data$home_team, todays_games_data$visitor_team))
    
  return(agg_stats)
  
}








