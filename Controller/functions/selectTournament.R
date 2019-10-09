
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