## GLOBAL -----

options(stringsAsFactors = FALSE)
options(shiny.reactlog = TRUE)


library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(fireData)
library(Jmisc)
library(tidyverse)
library(DT)
library(reactlog)

# Source functions
sourceAll('./functions')



## UI -----
ui <- dashboardPage(
  ## HEADER ----
  dashboardHeader(title = 'DRAFT 2020'),
  
  ## SIDEBAR ----
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Dashboard", tabName = "tabDashboard", icon = icon("dashboard")),
      menuItem("Settings",  tabName = "tabSettings",  icon = icon("cogs"))
    )
  ),
  
  ## BODY ----
  dashboardBody(
    tags$head(tags$style(styles)),
    
    tabItems(
      tabItem(
        tabName = "tabDashboard",
        source('./ui/uiDashboard.R', local= TRUE)$value
      ),
      tabItem(
        tabName = "tabSettings",
        source('./ui/uiSettings.R', local= TRUE)$value
      )
    )
    
  )
)

## SERVER -----
server <- function(input, output, session) {
  
  source('./server/serverDashboard.R', local= TRUE)$value
  source('./server/serverSettings.R', local= TRUE)$value
  
}

## RUN -----
shinyApp(ui = ui, server = server)

