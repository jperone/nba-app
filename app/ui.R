suppressPackageStartupMessages({
    library(tidyverse)
    library(magrittr)
    library(shiny)
    library(shinydashboard)
    library(shinycssloaders)
    library(shinyFeedback)
    library(DT)
    library(data.table)
    library(highcharter)
    library(glue)
    library(shinyjs)
    library(httr)
    library(shinyWidgets)
    library(jsonlite)
    library(roomba)
    library(lubridate)
})

setwd(here::here())

source("nba-app/helpers.R")

ui <- dashboardPage(
    skin = "red",
    dashboardHeader(
        title = "NBA Projections"
    ),
    dashboardSidebar(
        width = 250,
        sidebarMenu(
            id = "sidebarID",
            menuItem(
                "NBA Projected Stats",
                tabName = "projected-stats",
                icon = icon("book")
            )
            
        )
    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            tabItem(
                "projected-stats",
                dateRangeInput(
                    "date_search_summary_stats", 
                    label = "Date Range for stat search: ",
                    start = Sys.Date() - days(30), 
                    end = Sys.Date() - days(1),
                    min = "2000-01-01",
                    max = Sys.Date()
                ),
                dateInput(
                    "date_search_games",
                    label = "Date for teams in games: ",
                    value = Sys.Date(),
                    min = Sys.Date(),
                    max = Sys.Date() + days(30)
                ),
                br(),
                withSpinner(dataTableOutput("player_stats_table"))
            )
        )
    )
)





