
server <- function(input, output, session) {
    players <- read_rds("nba-app/player-list.rds")
    
    date_search_summary_stats_start <- reactive({
        input$date_search_summary_stats[1]
    })
    
    date_search_summary_stats_end <- reactive({
        input$date_search_summary_stats[2]
    })
    
    date_games_search <- reactive({
        input$date_search_games
    })
    
    games <- reactive({
        get_games(start_date = date_search_summary_stats_start(), end_date = date_search_summary_stats_end())
    })
    
    todays_games <- reactive({
        get_todays_games(start_date = date_games_search(), end_date = date_games_search())
    })
    
    player_stats <- reactive({
        get_stats(
            players_data = players, 
            games_data = games(), 
            todays_games_data = todays_games(), 
            start_date = date_search_summary_stats_start(), 
            end_date = date_search_summary_stats_end()
        )
    })
    
    output$player_stats_table <- renderDataTable({
        player_stats() %>% 
            datatable(rownames = F)
    })
}