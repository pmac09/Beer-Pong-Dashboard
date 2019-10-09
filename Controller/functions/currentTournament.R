currentTournament <- function(vTournamentList){
  vCurrentTournament <- vTournamentList[vTournamentList$SELECTED == '1', ]
  return(vCurrentTournament)
} # Returns the currently selected tournamnet