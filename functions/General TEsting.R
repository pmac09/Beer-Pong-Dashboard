projectURL <- 'https://fantasy-banter-082017.firebaseio.com'

vTournamentList <- tournamentList(projectURL)
vTournamentList

vAddTournament <- 'New Tournament'
addTournament(projectURL, vAddTournament)

vDeleteTournament <- 'temp'
deleteTournament(projectURL, vDeleteTournament)
hardDeleteTournament(projectURL, vDeleteTournament)

vSelectTournament <- 'KappazBucks'
selectTournament(projectURL, vSelectTournament)

vCurrentTournament <- currentTournament(vTournamentList)
vCurrentTournament

vTournamentName <- tournamentName(vCurrentTournament)
vTournamentName

vTeamList <- teamList(projectURL, vTournamentName)
vTeamList

vAddPlayer <- 'PMAC'
addPlayer(projectURL, vTournamentName, vAddPlayer)

vTeamName <- 'NEW TEAM'
vPlayer1  <- 'BPLAYER'
vPlayer2  <- 'APLAYER'
addTeam(projectURL, vTournamentName, vTeamName, vPlayer1, vPlayer2)
  
vDeleteTeam <- 'NEW TEAM'
deleteTeam(projectURL, vTournamentName, vDeleteTeam)

vGameList <- gameList(projectURL, vTournamentName)
vGameList

vGameName <- 'NEW GAME'
vTeam1  <- 'BPLAYER'
vTeam2  <- 'APLAYER'
addGame(projectURL, vTournamentName, vGameName, vTeam1, vTeam2)

vGameName <- 'NEW GAME'
deleteGame(projectURL, vTournamentName, vGameName)







test <- data.frame(lapply(vTeamList, function(x) {
                    gsub("/", "&", x)
               }))


dir <- paste0(vTournamentName,'/games')
firebaseSave(projectURL, dir, test)


dir <- paste0(vTournamentName,'/teams')
firebaseSave(projectURL, dir, test)


vTeamList

test <- ldata %>%
  filter(TEAM == 'JAMES NGUYEN & YANG')
