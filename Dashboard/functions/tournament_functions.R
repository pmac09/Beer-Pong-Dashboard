
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
deleteTournament <- function(projectURL, vTournamentName){
  
  # Download current tournament list
  vTournamentList <- tournamentList(projectURL)
  
  # Select all that dont equal input
  vNewTournamentList <- vTournamentList[vTournamentList$TOURNAMENT_NAME != vTournamentName,]
  
  # Save to firebase
  firebaseSave(projectURL, "tournaments", vNewTournamentList)
  
  return(vNewTournamentList)
  
}
hardDeleteTournament <- function(projectURL, vTournamentName){
  
  # Download current tournament list
  vTournamentList <- tournamentList(projectURL)
  
  # Select all that dont equal input
  vNewTournamentList <- vTournamentList[vTournamentList$TOURNAMENT_NAME != vTournamentName,]
  
  # Delete from firebase
  delete(vDelete, projectURL, paste0('Beer-Pong-Dashboard/', vTournamentName))
  
  return(vNewTournamentList)
  
}