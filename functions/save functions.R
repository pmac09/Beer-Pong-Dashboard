
addPlayer <- function(projectURL,  playerName){
  if(playerName != ""){
    playerList <- download(projectURL, "Beer-Pong-Dashboard/players")
    playerList <- sort(unique(c(playerList, toupper(playerName))))
    put(playerList, projectURL, directory = "Beer-Pong-Dashboard/players")
  }
}
  
loadTeams <- function(projectURL){
  teams <- download(projectURL, "Beer-Pong-Dashboard/teams")
  return(teams)
}



addGame <- function(projectURL, gameName, homeTeam, awayTeam){
  
  # Create tibble with input values
  newGame <- tibble(
    "GAME_NAME" = gameName,
    "HOME_TEAM" = homeTeam,
    "AWAY_TEAM" = awayTeam
  )                                                             
  
  # Download games from firebase 
  gameList <- download(projectURL, "Beer-Pong-Dashboard/games") 
  
  # Add new game to game list
  if(is.null(gameList)){
    gameList <- newGame
  } else {
    gameList <- bind_rows(gameList, newGame)
  }
  
  # Upload games to firebase
  put(gameList, projectURL, directory = "Beer-Pong-Dashboard/games")
  
} ## Add game to firebase
removeGame <- function(projectURL, gameName){
  
  # Download games from firebase 
  gameList <- download(projectURL, "Beer-Pong-Dashboard/games") 
  
  # Add new game to game list
  if(!is.null(gameList)){
    
    #Filter for games not matching the entered name
    gameList <- gameList[gameList$GAME_NAME != gameName, ]
     
    # Upload games to firebase
    put(gameList, projectURL, directory = "Beer-Pong-Dashboard/games")
  }
  

  
} ## Remove game to firebase
loadGames <- function(projectURL){
  games <- download(projectURL, "Beer-Pong-Dashboard/games")
  return(games)
}
resetGames <- function(projectURL){
  # Upload NULL to firebase
  put(NULL, projectURL, directory = "Beer-Pong-Dashboard/games")
} ## Remove all games from firebase