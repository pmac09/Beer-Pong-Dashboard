

## REACTIVE VARIABLES ----
playerName_r <- reactive({
  playerName <- input$txtPlayerName
  return(playerName)
}) ## Player name textbox
playerList_r <- reactive({                 
  input$btnAddPlayer                       # Reactivity trigger for adding a player
  playerList <- loadPlayerList(projectURL) # Download player list from firebase
  return(playerList)
}) ## Returns list of players that have been

## RENDERS ----
output$uiLstTeamPlayer1 <- renderUI({
  selectInput(inputId= "lstTeamPlayer1", label= "Player 1", choices=playerList_r())
})
output$uiLstTeamPlayer2 <- renderUI({
  selectInput(inputId= "lstTeamPlayer2", label= "Player 2", choices=playerList_r())
})

## FUNCTIONS
addPlayer <- function(projectURL,  playerName){
  if(playerName != ""){
    playerList <- download(projectURL, "Beer-Pong-Dashboard/players")
    playerList <- sort(unique(c(playerList, toupper(playerName))))
    put(playerList, projectURL, directory = "Beer-Pong-Dashboard/players")
  }
} ## Add player to firebase
removePlayer <- function(projectURL,  playerName){
  playerList <- download(projectURL, "Beer-Pong-Dashboard/players")
  playerList <- playerList[playerList!=toupper(playerName)]
  put(playerList, projectURL, directory = "Beer-Pong-Dashboard/players")
  #put(sort(c("GARTER","SPOON","MELONS","KAPPAZ","PMAC","LESTER","RICHO","JMERC")), projectURL, directory = "Beer-Pong-Dashboard/players")
} ## Add player to firebase

loadPlayerList <- function(projectURL){
  playerList <- download(projectURL, "Beer-Pong-Dashboard/players")
  return(playerList)
} ## Download player list from firebase

## OBSERVES
observeEvent(input$btnAddPlayer, {
  addPlayer(projectURL, playerName_r())
}) ## Click event for add player button

## RENDERS ----
output$tblPlayerList <- renderText({
  paste0(playerList_r(), collapse = ", ")
}) ## Render Player List




## TEAMS TAB ----

teamName_r <- reactive({
  name <- input$txtTeamName
  return(name)
})    ## Team name textbox
teamPlayer1_r <- reactive({
  teamPlayer1 <- input$lstTeamPlayer1
  return(teamPlayer1)
}) ## Team Player 1 list
teamPlayer2_r <- reactive({
  teamPlayer2 <- input$lstTeamPlayer2
  return(teamPlayer2)
}) ## Team Player 2 list
teamList_r <- reactive({
  input$btnAddTeam               # Reactivity trigger for adding a team
  teams <- loadTeams(projectURL) # Download player list from firebase
  return(teams)
})    ## Team List table

addTeam <- function(projectURL, teamName, player1, player2){
  
  newTeam <- tibble(
    "TEAM_NAME" = teamName,
    "PLAYER_1" = player1,
    "PLAYER_2" = player2
  )
  
  teamList <- download(projectURL, "Beer-Pong-Dashboard/teams")
  
  if(is.null(teamList)){
    teamList <- newTeam
  } else {
    teamList <- bind_rows(teamList, newTeam)
  }

  put(teamList, projectURL, directory = "Beer-Pong-Dashboard/teams")
} ## Add team to firebase
observeEvent(input$btnAddTeam, {
  addTeam(projectURL, teamName_r(), teamPlayer1_r(), teamPlayer2_r())
}) ## Click event for add team button

loadTeams <- function(projectURL){
  teams <- download(projectURL, "Beer-Pong-Dashboard/teams")
  return(teams)
} ## Download team list from firebase

output$tblTeamList <- renderDataTable({
  teamList_r()
}) ## Render Team List


## GAMES TAB ----

output$uiLstHomeTeam <- renderUI({
  selectInput(inputId= "lstHomeTeam", label= "Home Team", choices=c(teamList_r()$TEAM_NAME, playerList_r()))
})
output$uiLstAwayTeam <- renderUI({
  selectInput(inputId= "lstAwayTeam", label= "Away Team", choices=c(teamList_r()$TEAM_NAME, playerList_r()))
})

gameName_r <- reactive({
  name <- input$txtGameName
  return(name)
}) ## Game name textbox
homeTeam_r <- reactive({
  team <- input$lstHomeTeam
  return(team)
}) ## Home Team
awayTeam_r <- reactive({
  team <- input$lstAwayTeam
  return(team)
}) ## Away Team
gameList_r <- reactive({
  input$btnAddGame               # Reactivity trigger for adding a team
  games <- loadGames(projectURL) # Download player list from firebase
  return(games)
})    ## Team List table

observeEvent(input$btnAddGame, {
  addGame(projectURL, gameName_r(), homeTeam_r(), awayTeam_r())
}) ## Click event for add game button

loadGames <- function(projectURL){
  games <- download(projectURL, "Beer-Pong-Dashboard/games")
  return(games)
} ## Download team list from firebase

output$tblGameList <- renderDataTable({
  gameList_r()
}) ## Render Team List










# observeEvent(input$btnAddPlayer, {
#   sendSweetAlert(
#     session = session,
#     title = "Success",
#     text = "Player has been added",
#     type = "success"
#   )
# })