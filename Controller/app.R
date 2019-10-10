## GLOBAL -----

library(shiny)
library(miniUI)
library(fireData)
library(Jmisc)
library(tidyverse)

library(DT)

# Source functions
sourceAll('./functions')

## UI ----
ui <- miniPage(
  
  ## TITLE ----
  gadgetTitleBar(
    "Beer Pong Dashboard",
    left  = miniTitleBarButton("btnRefresh", icon("sync"), primary = FALSE),
    right = miniTitleBarButton("btnUndo", icon("undo"), primary = TRUE)
  ),
  miniTabstripPanel(
    
    ## SCORING TAB ----
    miniTabPanel(
      "Scoring", 
      icon = icon("clipboard"),
      miniContentPanel(
        fillCol(
          flex=c(NA,NA,NA,NA,1,NA),
          uiOutput("uiLstGames"),
          textOutput("txtPlayerSelected"),
          hr(),
          dataTableOutput("tblHistory"),
          textOutput("txtCurrentThrow")
        )
      ),
      uiOutput("uiBtnPlayers"),
      miniButtonBlock(
        border = c('top'),
        actionButton(inputId = 'btnHit', label = 'HIT'),
        actionButton(inputId = 'btnMiss', label = 'MISS'),
        actionButton(inputId = 'btnOverthrow', label = 'OVERTHROW')
      ),
      miniButtonBlock(
        border = NULL,
        actionButton(inputId = 'btnTrickHit', label = 'TS HIT'),
        actionButton(inputId = 'btnTrickMiss', label = 'TS MISS')
      ),
      miniButtonBlock(
        border = NULL,
        actionButton(inputId = 'btnBallsBack', label = 'BALLS BACK'),
        actionButton(inputId = 'btnSameCup', label = 'SAME CUP'),
        actionButton(inputId = 'btnRedemption', label = 'REDEMPTION')
      )
    )
  )
)

## SERVER ----
server <- function(input, output, session) {
  
  
  tournamentList <- function(projectURL){
    vTournamentList <- firebaseDownload(projectURL, 'tournaments') %>%
      select(TOURNAMENT_NAME, SELECTED)
    return(vTournamentList)
  } # Returns list of tournaments from firebase
  currentTournament <- function(vTournamentList){
    vCurrentTournament <- vTournamentList[vTournamentList$SELECTED == '1', ]
    return(vCurrentTournament)
  } # Returns the currently selected tournamnet
  tournamentName <- function(vCurrentTournament){
    vTournamentName <- vCurrentTournament$TOURNAMENT_NAME
    return(vTournamentName)
  } # Returns the name of the current tournament
  gameList <- function(projectURL, vTournamentName){
    
    dir <- paste0(vTournamentName,'/games')
    vGameList <- firebaseDownload(projectURL, dir) 
    
    if(is.null(vGameList)){
      vGameList <- tibble(
        GAME_NAME = character(),
        HOME_TEAM  = character(),
        AWAY_TEAM  = character()
      )
    } else {
      vGameList <- vGameList %>%
        select(GAME_NAME, HOME_TEAM, AWAY_TEAM)
    }
    
    return(vGameList)
  } # Returns list of games for given tournament from firebase
  teamList <- function(projectURL, vTournamentName){
    
    dir <- paste0(vTournamentName,'/teams')
    
    vTeamList <- firebaseDownload(projectURL, dir) 
    
    if(is.null(vTeamList)){
      vTeamList <- tibble(
        TEAM_NAME = character(),
        PLAYER_1  = character(),
        PLAYER_2  = character()
      )
    } else {
      vTeamList <- vTeamList %>%
        select(TEAM_NAME, PLAYER_1, PLAYER_2)
    }
    
    return(vTeamList)
  } # Returns list of teams for given tournament from firebase
  gameData <- function(vGameName, vTournamentName){
    path <- paste0(vTournamentName, "/data/", vGameName)
    data <- firebaseDownload(projectURL, path)
    return(data)
  }
  
  addScore <- function(vTournamentName, vGameData, vGameName, vTeamName, vPlayerName, vShotType){
    
    if(vPlayerName == ''){
      return(NULL)
    }
    
    vTime <- as.character(Sys.time())
    
    vNewGameData <- rbind(vGameData, c(vTime, vGameName, vPlayerName, vShotType, vTeamName))
    
    # Save to firebase
    dir <- paste0(vTournamentName,'/data/', vGameName)
    firebaseSave(projectURL, dir, vNewGameData)
    
    return(vNewGameData)
  }
  undoScore <- function(vTournamentName, vGameData, vGameName){
    
    if(nrow(vGameData) == 0){
      return(vGameData)
    }
    
    vNewGameData <- vGameData[1:nrow(vGameData)-1,]
    
    # Save to firebase
    dir <- paste0(vTournamentName,'/data/', vGameName)
    firebaseSave(projectURL, dir, vNewGameData)
    
    return(vNewGameData)
  }
  
  scoreData <- function(projectURL, gameName){
    data <- download(projectURL, paste0("Beer-Pong-Dashboard/data/",gameName))
    
    if(is.null(data)){
      data <- tibble(
        GAME_NAME = character(),
        TEAM_NAME = character(),
        PLAYER_NAME = character(),
        SHOT_TYPE = character(),
        SHOT_TIME = character()
      )
    }
    
    return(data)
  }
  
  tournamentList_r <- reactive({
    vTournamentList <- tournamentList(projectURL)
    return(vTournamentList)
  })
  currentTournament_r <- reactive({
    vCurrentTournament <- currentTournament(tournamentList_r())
    return(vCurrentTournament)
  })
  tournamentName_r <- reactive({
    vTournamentName <- tournamentName(currentTournament_r())
    return(vTournamentName)
  })
  gameList_r <- reactive({
    rv$refreshGameList 
    vGameList <- gameList(projectURL, tournamentName_r())
    return(vGameList)
  })
  teamList_r <- reactive({
    vTeamList <- teamList(projectURL,tournamentName_r())
    return(vTeamList)
  })
  gameData_r <- reactive({
    rv$refresh
    vGameData <- gameData(input$lstGames, tournamentName_r())
  })
  
  t1Name <- reactive({
    gameList_r()$HOME_TEAM[gameList_r()$GAME_NAME == input$lstGames]
  })
  t1Player1 <- reactive({
    teamList_r()$PLAYER_1[teamList_r()$TEAM_NAME == t1Name()]
  })
  t1Player2 <- reactive({
    teamList_r()$PLAYER_2[teamList_r()$TEAM_NAME == t1Name()]
  })
  
  t2Name <- reactive({
    gameList_r()$AWAY_TEAM[gameList_r()$GAME_NAME == input$lstGames]
  })
  t2Player1 <- reactive({
    teamList_r()$PLAYER_1[teamList_r()$TEAM_NAME == t2Name()]
  })
  t2Player2 <- reactive({
    teamList_r()$PLAYER_2[teamList_r()$TEAM_NAME == t2Name()]
  })
  
  output$uiLstGames <- renderUI({
    selectInput("lstGames", NULL, width= '100%', gameList_r()$GAME_NAME)
  })
  output$uiBtnPlayers <- renderUI({
    
    ui <- list(
      miniButtonBlock(
         border = c('top'),
         actionButton(inputId = 'btnT1P2', label = t1Player2()),
         actionButton(inputId = 'btnT2P2', label = t2Player2())
      ),
      miniButtonBlock(
        border = c('bottom'),
        actionButton(inputId = 'btnT1P1', label = t1Player1()),
        actionButton(inputId = 'btnT2P1', label = t2Player1())
      )
    )
    
    return(ui)
    
  })
  output$tblHistory <- renderDataTable({
    req(input$lstGames)
    
    data <- gameData_r()
    if(is.null(data)) return(NULL)
    
    data <- data %>%
      mutate(RN = row_number()) %>%
      arrange(desc(RN)) %>%
      select(RN, PLAYER, SHOT_TYPE) %>%
      rename('#' = RN)
    
    table <- datatable(data,
                       rownames= FALSE,
                       options= list(paging= FALSE, 
                                     searching= FALSE, 
                                     ordering=FALSE, 
                                     info=FALSE,
                                     columnDefs = list(
                                       list(
                                         className = 'dt-center', 
                                         targets = 1:(ncol(data)-1)))
                       ),
                       extensions="Responsive"
    ) %>%
      formatStyle( 0, target= 'row', lineHeight='70%')
    
    return(table)
  })
  output$txtPlayerSelected <- renderText({
    ui <- paste0('Player Selected: ', rv$playerSelected)
    return(ui)
  })
  
  rv <- reactiveValues(teamSelected='', playerSelected = '', refreshGameList = 1, refresh = 1)
  
  observeEvent(input$btnT1P1,{
    rv$teamSelected  <- t1Name()
    rv$playerSelected <- t1Player1()
  })
  observeEvent(input$btnT1P2,{
    rv$teamSelected  <- t1Name()
    rv$playerSelected <- t1Player2()
  })
  observeEvent(input$btnT2P1,{
    rv$teamSelected  <- t2Name()
    rv$playerSelected <- t2Player1()
  })
  observeEvent(input$btnT2P2,{
    rv$teamSelected  <- t2Name()
    rv$playerSelected <- t2Player2()
  })
  
  observeEvent(input$btnUndo,{
    vUndoScore <- undoScore(tournamentName_r(), gameData_r(), input$lstGames)
    rv$refresh <- rv$refresh  + 1 
  })
  observeEvent(input$btnHit,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'HIT')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  observeEvent(input$btnMiss,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'MISS')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  observeEvent(input$btnOverthrow,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'OVERTHROW')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  observeEvent(input$btnTrickHit,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'TRICKSHOT HIT')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  observeEvent(input$btnTrickMiss,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'TIRCKSHOT MISS')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  observeEvent(input$btnBallsBack,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'BALLS BACK')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  observeEvent(input$btnSameCup,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'SAME CUP')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  observeEvent(input$btnRedemption,{
    vAddScore <- addScore(tournamentName_r(), gameData_r(), input$lstGames, rv$teamSelected, rv$playerSelected, 'REDEMPTION')
    if(!is.null(vAddScore)){
      rv$playerSelected <- ''
      rv$refresh <- rv$refresh  + 1 
    }
  })
  
  observeEvent(input$btnRefresh,{
    rv$teamSelected <- ''
    rv$playerSelected <- ''
    rv$refreshGameList <- rv$refreshGameList + 1
    rv$refresh <- rv$refresh + 1
  })
  
  
}

shinyApp(ui, server)
