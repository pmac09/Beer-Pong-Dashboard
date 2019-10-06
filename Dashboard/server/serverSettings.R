


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

## REACTIVE VARIABLES --
refresh <- reactiveValues(
  tournamentList = 0,
  teamList = 0
)

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


## OBSERVERS ----
observeEvent(input$btnAddTournament, {
  if(!is.null(input$txtNewTournament)){
    addTournament(projectURL, input$txtNewTournament)
    refresh$tournamentList <- refresh$tournamentList + 1  # trigger data refresh
  }
}) # Add tournament button
observeEvent(input$btnSelectTournament, {
  if(!is.null(input$tblTournamentList_rows_selected)){
    selectTournament(projectURL, tournamentList_r()$TOURNAMENT_NAME[input$tblTournamentList_rows_selected])
    refresh_rv(refresh_rv() + 1) # trigger data refresh
  }
}) # Select tournament button
observeEvent(input$btnAddPlayer, {
  if(!is.null(input$txtNewPlayer)){
    addPlayer(projectURL, tournamentName_r(), input$txtNewPlayer)
    refresh$teamList <- refresh$teamList + 1  # trigger data refresh
  }
}) # Add player button

# ## REACTIVE VARIABLES ----
# playerName_r <- reactive({
#   playerName <- input$txtPlayerName
#   return(playerName)
# }) ## Player name textbox
# playerList_r <- reactive({                 
#   input$btnAddPlayer                       # Reactivity trigger for adding a player
#   playerList <- loadPlayerList(projectURL) # Download player list from firebase
#   return(playerList)
# }) ## Returns list of players that have been
# 
# ## RENDERS ----
# output$uiLstTeamPlayer1 <- renderUI({
#   selectInput(inputId= "lstTeamPlayer1", label= "Player 1", choices=playerList_r())
# })
# output$uiLstTeamPlayer2 <- renderUI({
#   selectInput(inputId= "lstTeamPlayer2", label= "Player 2", choices=playerList_r())
# })
# 
# ## FUNCTIONS
# addPlayer <- function(projectURL,  playerName){
#   if(playerName != ""){
#     playerList <- download(projectURL, "Beer-Pong-Dashboard/players")
#     playerList <- sort(unique(c(playerList, toupper(playerName))))
#     put(playerList, projectURL, directory = "Beer-Pong-Dashboard/players")
#   }
# } ## Add player to firebase
# removePlayer <- function(projectURL,  playerName){
#   playerList <- download(projectURL, "Beer-Pong-Dashboard/players")
#   playerList <- playerList[playerList!=toupper(playerName)]
#   put(playerList, projectURL, directory = "Beer-Pong-Dashboard/players")
#   #put(sort(c("GARTER","SPOON","MELONS","KAPPAZ","PMAC","LESTER","RICHO","JMERC")), projectURL, directory = "Beer-Pong-Dashboard/players")
# } ## Add player to firebase
# 
# loadPlayerList <- function(projectURL){
#   playerList <- download(projectURL, "Beer-Pong-Dashboard/players")
#   return(playerList)
# } ## Download player list from firebase
# 
# ## OBSERVES
# observeEvent(input$btnAddPlayer, {
#   addPlayer(projectURL, playerName_r())
# }) ## Click event for add player button
# 
# ## RENDERS ----
# output$tblPlayerList <- renderText({
#   paste0(playerList_r(), collapse = ", ")
# }) ## Render Player List




## TEAMS TAB ----
# 
# teamName_r <- reactive({
#   name <- input$txtTeamName
#   return(name)
# })    ## Team name textbox
# teamPlayer1_r <- reactive({
#   teamPlayer1 <- input$lstTeamPlayer1
#   return(teamPlayer1)
# }) ## Team Player 1 list
# teamPlayer2_r <- reactive({
#   teamPlayer2 <- input$lstTeamPlayer2
#   return(teamPlayer2)
# }) ## Team Player 2 list
# teamList_r <- reactive({
#   input$btnAddTeam               # Reactivity trigger for adding a team
#   teams <- loadTeams(projectURL) # Download player list from firebase
#   return(teams)
# })    ## Team List table
# 
# addTeam <- function(projectURL, teamName, player1, player2){
#   
#   newTeam <- tibble(
#     "TEAM_NAME" = teamName,
#     "PLAYER_1" = player1,
#     "PLAYER_2" = player2
#   )
#   
#   teamList <- download(projectURL, "Beer-Pong-Dashboard/teams")
#   
#   if(is.null(teamList)){
#     teamList <- newTeam
#   } else {
#     teamList <- bind_rows(teamList, newTeam)
#   }
# 
#   put(teamList, projectURL, directory = "Beer-Pong-Dashboard/teams")
# } ## Add team to firebase
# observeEvent(input$btnAddTeam, {
#   addTeam(projectURL, teamName_r(), teamPlayer1_r(), teamPlayer2_r())
# }) ## Click event for add team button
# 
# loadTeams <- function(projectURL){
#   teams <- download(projectURL, "Beer-Pong-Dashboard/teams")
#   return(teams)
# } ## Download team list from firebase
# 
# output$tblTeamList <- renderDataTable({
#   teamList_r()
# }) ## Render Team List
# 
# 
# ## GAMES TAB ----
# 
# output$uiLstHomeTeam <- renderUI({
#   selectInput(inputId= "lstHomeTeam", label= "Home Team", choices=c(teamList_r()$TEAM_NAME, playerList_r()))
# })
# output$uiLstAwayTeam <- renderUI({
#   selectInput(inputId= "lstAwayTeam", label= "Away Team", choices=c(teamList_r()$TEAM_NAME, playerList_r()))
# })
# 
# gameName_r <- reactive({
#   name <- input$txtGameName
#   return(name)
# }) ## Game name textbox
# homeTeam_r <- reactive({
#   team <- input$lstHomeTeam
#   return(team)
# }) ## Home Team
# awayTeam_r <- reactive({
#   team <- input$lstAwayTeam
#   return(team)
# }) ## Away Team
# gameList_r <- reactive({
#   input$btnAddGame               # Reactivity trigger for adding a team
#   games <- loadGames(projectURL) # Download player list from firebase
#   return(games)
# })    ## Team List table
# 
# observeEvent(input$btnAddGame, {
#   addGame(projectURL, gameName_r(), homeTeam_r(), awayTeam_r())
# }) ## Click event for add game button
# 
# loadGames <- function(projectURL){
#   games <- download(projectURL, "Beer-Pong-Dashboard/games")
#   return(games)
# } ## Download team list from firebase
# 
# output$tblGameList <- renderDataTable({
#   gameList_r()
# }) ## Render Team List
# 
# 
# 
# 






# observeEvent(input$btnAddPlayer, {
#   sendSweetAlert(
#     session = session,
#     title = "Success",
#     text = "Player has been added",
#     type = "success"
#   )
# })