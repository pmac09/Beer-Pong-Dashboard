tournamentList <- function(projectURL){
  vTournamentList <- firebaseDownload(projectURL, 'tournaments') %>%
    select(TOURNAMENT_NAME, SELECTED)
  return(vTournamentList)
} # Returns list of tournaments from firebase