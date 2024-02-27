
vTournamentList <- tournamentList(projectURL)
vTournamentList

vCurrentTournament <- currentTournament(vTournamentList)
vCurrentTournament


players <-c('Pmac'
,'KAPPA'
,'Richo'
,'Spoon'
,'Garter'
, 'Lester'
,'Melons'
,'Robbo'
,'Stellini'
,'Pisa'
,'Longy'
,'Yang'
,'Greeny'
,'Pej'
,'Bespoke'
,'Chopz' 
,'Jonzzy' 
,'D'
,'Johnny'
,'Louie'
,'Deano'
,'Bo'
,'K.P.'
,'James Nguyen')

players <- toupper(players)

sample(players, length(players), replace = FALSE, prob = NULL)

odd(1:length(players))

odd <- function(x) x%%2 != 0
even <- function(x) x%%2 == 0


split(sample(players),rep(1:(length(players)/2),each=2))

option1 <- split(sample(players),rep(1:(length(players)/2),each=2))
option2 <- split(sample(players),rep(1:(length(players)/2),each=2))
option3 <- split(sample(players),rep(1:(length(players)/2),each=2))
option4 <- split(sample(players),rep(1:(length(players)/2),each=2))
option5 <- split(sample(players),rep(1:(length(players)/2),each=2))
option6 <- split(sample(players),rep(1:(length(players)/2),each=2))

bind_cols(option1)
bind_cols(option2)
bind_cols(option3)
bind_cols(option4)
bind_cols(option5)
bind_cols(option6)



teams <- vTeamList %>%
  filter(PLAYER_2 != '') %>%
  select(TEAM_NAME)



R1 <- bind_cols(split(sample(teams$TEAM_NAME),rep(1:(length(teams$TEAM_NAME)/2),each=2)))
R1

temp <- as_tibble(rbind(R1[[1,]], 
      R1[[2,]],
      R1[[5,]],
      R1[[3,]],
      R1[[4,]], 
      R1[[6,]]))

colnames(temp) <- c('HOME_TEAM','AWAY_TEAM')

temp2 <- temp %>%
  mutate(GAME_NAME = paste0('R2 - ', HOME_TEAM, ' vs ', AWAY_TEAM)) %>%
  select(GAME_NAME, HOME_TEAM, AWAY_TEAM)

temp2

vGamesList <- gameList(projectURL,vTournamentName)

temp2<-rbind(vGamesList, temp2)

dir <- paste0(vTournamentName,'/games')
firebaseSave(projectURL, dir, temp2)


