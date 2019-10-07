


## FUNCTIONS ----
firebaseDownload <- function(projectURL, path = NULL){
  if(is.null(path)) return(NULL)
  data <- suppressWarnings(download(projectURL, paste0('Beer-Pong-Dashboard/',path)))
  return(data)
} # Download data from firebase location
firebaseSave <- function(projectURL, path = NULL, data){
  if(is.null(path)) return(NULL)
  put(data, projectURL, paste0('Beer-Pong-Dashboard/', path))
} # Save data to firebase location

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
addTournament <- function(projectURL, vTournamentName){
  
  # Download current tournament list
  vTournamentList <- tournamentList(projectURL)
  
  # Replace all spaces with underscore
  vTournamentName <- str_replace_all(vTournamentName, "[[:space:]]", "_")
  
  # Remove all non-alphanumeric and underscores
  vTournamentName <- str_replace_all(vTournamentName, "[^[:alnum:]|^[_]]", "")

  # Return unchanged list if name is null
  if(is.null(vTournamentName) | !nchar(vTournamentName)>0){
    return(vTournamentList)
  }
  
  # Check tournament name is unique
  if(!is.na(which(vTournamentList$TOURNAMENT_NAME == vTournamentName)[1])){
   return(vTournamentList)
  }
  
  # Add new tournament to list
  vNewTournamentList <- rbind(vTournamentList, c(vTournamentName, 0))
  
  # Save to firebase
  firebaseSave(projectURL, "tournaments", vNewTournamentList)
  
  return(vNewTournamentList)
}
deleteTournament <- function(projectURL, vTournamentName){
  
  # Download current tournament list
  vTournamentList <- tournamentList(projectURL)
  
  # Select all that dont equal input
  vNewTournamentList <- vTournamentList[vTournamentList$TOURNAMENT_NAME != vTournamentName,]
  
  # Save to firebase
  firebaseSave(projectURL, "tournaments", vNewTournamentList)
  
  return(vNewTournamentList)
  
}
selectTournament <- function(projectURL, vTournamentName){
  
  # Download current tournament list
  vTournamentList <- tournamentList(projectURL)
  
  # Select the tournament based on input
  vTournamentList$SELECTED <- 0
  vTournamentList$SELECTED[vTournamentList$TOURNAMENT_NAME == vTournamentName] <- 1
  
  # Save to firebase
  firebaseSave(projectURL, "tournaments", vTournamentList)
  
  return(vTournamentList)
  
}

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

addPlayer <- function(projectURL, vTournamentName, vPlayerName){
  
  # Download current team list
  vTeamList <- teamList(projectURL, vTournamentName)
  
  # Return unchanged list if name is null
  if(is.null(vPlayerName) | !nchar(vPlayerName)>0){
    return(vTeamList)
  }
  
  # Check player name is unique
  if(!is.na(which(vTeamList$TEAM_NAME == vPlayerName)[1])){
    return(vTeamList)
  }
  
  vNewPlayer <- tibble(
    TEAM_NAME = vPlayerName,
    PLAYER_1  = vPlayerName,
    PLAYER_2  = ""
  )
  
  # Add new player to list
  vNewTeamList <- rbind(vTeamList, vNewPlayer) %>%
    arrange(TEAM_NAME)
  
  # Save to firebase
  dir <- paste0(vTournamentName,'/teams')
  firebaseSave(projectURL, dir, vNewTeamList)
  
  return(vNewTeamList)
}
addTeam <- function(projectURL, vTournamentName, vTeamName, vPlayer1, vPlayer2){
  
  # Download current team list
  vTeamList <- teamList(projectURL, vTournamentName)
  
  # Return unchanged list if name is null
  if(is.null(vTeamName) | !nchar(vTeamName)>0){
    return(vTeamList)
  }
  
  # Check player name is unique
  if(!is.na(which(vTeamList$TEAM_NAME == vTeamName)[1])){
    return(vTeamList)
  }
  
  vPlayers <- sort(c(vPlayer1, vPlayer2))
  
  vNewTeam <- tibble(
    TEAM_NAME = vTeamName,
    PLAYER_1  = vPlayers[1],
    PLAYER_2  = vPlayers[2]
  ) 
  
  # Add new player to list
  vNewTeamList <- rbind(vTeamList, vNewTeam) %>%
    arrange(TEAM_NAME)
  
  # Save to firebase
  dir <- paste0(vTournamentName,'/teams')
  firebaseSave(projectURL, dir, vNewTeamList)
  
  return(vNewTeamList)
}
deleteTeam <- function(projectURL, vTournamentName, vTeamName){
  
  # Download current tournament list
  vTeamList <- teamList(projectURL, vTournamentName)
  
  # Select all that dont equal input
  vNewTeamList <- vTeamList[vTeamList$TEAM_NAME != vTeamName,]
  
  # Save to firebase
  dir <- paste0(vTournamentName,'/teams')
  firebaseSave(projectURL, dir, vNewTeamList)
  
  return(vNewTeamList)
  
}

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
addGame <- function(projectURL, vTournamentName, vGameName, vTeam1, vTeam2){
  
  # Download current game list
  vGameList <- gameList(projectURL, vTournamentName)
  
  # Return unchanged list if name is null
  if(is.null(vGameName) | !nchar(vGameName)>0){
    return(vGameList)
  }
  
  # Check game name is unique
  if(!is.na(which(vGameList$GAME_NAME == vGameName)[1])){
    return(vGameList)
  }
  
  vTeams <- sort(c(vTeam1, vTeam2))
  
  vNewGame <- tibble(
    GAME_NAME = vGameName,
    HOME_TEAM  = vTeams[1],
    AWAY_TEAM  = vTeams[2]
  ) 
  
  # Add new game to list
  vNewGameList <- rbind(vGameList, vNewGame) 
  
  # Save to firebase
  dir <- paste0(vTournamentName,'/games')
  firebaseSave(projectURL, dir, vNewGameList)
  
  return(vNewGameList)
}
deleteGame <- function(projectURL, vTournamentName, vGameName){
  
  # Download current tournament list
  vGameList <- gameList(projectURL, vTournamentName)
  
  # Select all that dont equal input
  vNewGameList <- vGameList[vGameList$TEAM_NAME != vGameName,]
  
  # Save to firebase
  dir <- paste0(vTournamentName,'/games')
  firebaseSave(projectURL, dir, vNewGameList)
  
  return(vNewGameList)
  
}

## REACTIVE FUNCTION ----
tournamentList_r <- reactive({
  refresh$tournamentList # reactiveVal to trigger refresh
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

teamList_r <- reactive({
  refresh$teamList # reactiveVal to trigger refresh
  vTeamList <- teamList(projectURL, tournamentName_r())
  return(vTeamList)
})
gameList_r <- reactive({
  refresh$gameList # reactiveVal to trigger refresh
  vGameList <- gameList(projectURL, tournamentName_r())
  return(vGameList)
})

## REACTIVE VARIABLES ----
refresh <- reactiveValues(tournamentList = 0,teamList = 0, gameList = 0)

## OUTPUTS ----
output$txtTournamentName <- renderText({
  txtTournamentName <- tournamentName_r()
  return(txtTournamentName)
})
output$tblTournamentList <- renderDataTable({
  
  vTournamentList <- tournamentList_r()
  table <- datatable(vTournamentList,
                     rownames= FALSE,
                     options= list(paging= FALSE, 
                                   searching= FALSE, 
                                   ordering=FALSE, 
                                   info=FALSE,
                                   columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(vTournamentList)-1)))
                     ),
                     extensions="Responsive",
                     selection = "single"
  ) %>%
    formatStyle( 0, target= 'row', lineHeight='70%')
  
  return(table)
})
output$tblPlayerList <- renderDataTable({
  
  vTeamList <- teamList_r()%>%
    filter(PLAYER_2=="") %>%
    select(TEAM_NAME) %>%
    rename(PLAYER = TEAM_NAME)
    
  table <- datatable(vTeamList,
                     rownames= FALSE,
                     options= list(paging= FALSE, 
                                   searching= FALSE, 
                                   ordering=FALSE, 
                                   info=FALSE,
                                   columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(vTeamList)-1)))
                     ),
                     extensions="Responsive",
                     selection = "single"
  ) %>%
    formatStyle( 0, target= 'row', lineHeight='70%')
  
  return(table)
})

output$tblTeamList <- renderDataTable({
  
  vTeamList <- teamList_r()%>%
    filter(PLAYER_2!="") %>%
    select(TEAM_NAME, PLAYER_1, PLAYER_2) 
  
  table <- datatable(vTeamList,
                     rownames= FALSE,
                     options= list(paging= FALSE, 
                                   searching= FALSE, 
                                   ordering=FALSE, 
                                   info=FALSE,
                                   columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(vTeamList)-1)))
                     ),
                     extensions="Responsive",
                     selection = "single"
  ) %>%
    formatStyle( 0, target= 'row', lineHeight='70%')
  
  return(table)
})
output$uiLstPlayer1 <- renderUI({
  
  vTeamList <-  teamList_r()
  
  vPlayerList <- vTeamList%>%
    filter(PLAYER_2=="") %>%
    select(TEAM_NAME)
  
  ui <- selectInput(
    inputId= 'lstPlayer1',
    label= 'Player 1:',
    choices = vPlayerList$TEAM_NAME
  )
  return(ui)
})
output$uiLstPlayer2 <- renderUI({
  
  vTeamList <-  teamList_r()
  
  vPlayerList <- vTeamList%>%
    filter(PLAYER_2=="") %>%
    select(TEAM_NAME)
  
  ui <- selectInput(
    inputId= 'lstPlayer2',
    label= 'Player 2:',
    choices = vPlayerList$TEAM_NAME
  )
  
  return(ui)
})

output$tblGameList <- renderDataTable({
  
  vGameList <- gameList_r()
  
  table <- datatable(vGameList,
                     rownames= FALSE,
                     options= list(paging= FALSE, 
                                   searching= FALSE, 
                                   ordering=FALSE, 
                                   info=FALSE,
                                   columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(vGameList)-1)))
                     ),
                     extensions="Responsive",
                     selection = "single"
  ) %>%
    formatStyle( 0, target= 'row', lineHeight='70%')
  
  return(table)
})
output$uiLstTeam1 <- renderUI({
  
  vTeamList <-  teamList_r()
  
  vTeamList <- vTeamList %>%
    select(TEAM_NAME)
  
  ui <- selectInput(
    inputId= 'lstTeam1',
    label= 'Team 1:',
    choices = vTeamList$TEAM_NAME
  )
  return(ui)
})
output$uiLstTeam2 <- renderUI({
  
  vTeamList <-  teamList_r()
  
  vTeamList <- vTeamList %>%
    select(TEAM_NAME)
  
  ui <- selectInput(
    inputId= 'lstTeam2',
    label= 'Team 2:',
    choices = vTeamList$TEAM_NAME
  )
  return(ui)
})

## OBSERVERS ----
observeEvent(input$btnAddTournament, {
  if(!is.null(input$txtNewTournament)){
    addTournament(projectURL, input$txtNewTournament)
    refresh$tournamentList <- refresh$tournamentList + 1  # trigger data refresh
  }
})    # Add tournament button
observeEvent(input$btnSelectTournament, {
  if(!is.null(input$tblTournamentList_rows_selected)){
    selectTournament(projectURL, tournamentList_r()$TOURNAMENT_NAME[input$tblTournamentList_rows_selected])
    refresh$tournamentList <- refresh$tournamentList + 1  # trigger data refresh
  }
}) # Select tournament button
observeEvent(input$btnAddPlayer, {
  if(!is.null(input$txtNewPlayer)){
    addPlayer(projectURL, tournamentName_r(), input$txtNewPlayer)
    refresh$teamList <- refresh$teamList + 1  # trigger data refresh
  }
})        # Add player button
observeEvent(input$btnAddTeam, {
  if(!is.null(input$txtTeamName) & input$lstPlayer1 != input$lstPlayer2){
    addTeam(projectURL, tournamentName_r(), input$txtTeamName, input$lstPlayer1, input$lstPlayer2)
    refresh$teamList <- refresh$teamList + 1  # trigger data refresh
  }
})          # Add team button
observeEvent(input$btnAddGame, {
  if(!is.null(input$txtGameName) & input$lstTeam1 != input$lstTeam2){
    addGame(projectURL, tournamentName_r(), input$txtGameName, input$lstTeam1, input$lstTeam2)
    refresh$gameList <- refresh$gameList + 1  # trigger data refresh
  }
})          # Add game button



# observeEvent(input$btnAddPlayer, {
#   sendSweetAlert(
#     session = session,
#     title = "Success",
#     text = "Player has been added",
#     type = "success"
#   )
# })