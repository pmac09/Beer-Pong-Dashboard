## FUNCTIONS ----

game <- function(vGameList, vGameName){
  if(is.null(vGameName)){
    game <- NULL
  } else if (vGameName == 'No Games Available'){
    game <- NULL
  } else {
    game <- vGameList[vGameList$GAME_NAME == vGameName,]
  }
  return(game)
} # Return teams from selected game
team <- function(vTeamList, vteamName){
  if(is.null(vteamName)){
    team <- NULL
  } else {
    team <- vTeamList[vTeamList$TEAM_NAME == vteamName,]
  }
  return(team)
} # Returns players from selected team

gameData <- function(vGameName, vTournamentName){
  
  if(is.null(vGameName)) return(NULL)
  
  path <- paste0(vTournamentName, "/data/", vGameName)
  data <- firebaseDownload(projectURL, path)
  
  
  if(is.null(data)){
    return(NULL)
  } else {
    data <- data %>%
      select(DATE_TIME, GAME, TEAM, PLAYER, SHOT_TYPE)
  }
  
  return(data)
}
statsData <- function(game, vGameData){
  
  if(is.null(game)){
    return(NULL)
  }
  
  gameName <- game$GAME_NAME
  homeTeam <- game$HOME_TEAM
  
  # Set target 
  target <- ifelse(substr(gameName,1,1)=='I',6,10)
  noPlayers <- ifelse(substr(gameName,1,1)=='I',2,4)
  
  # Download game data from firebase
  if(is.null(vGameData)){
    return(NULL)
    } else {
      data <- vGameData
    }
    
  # Calcuate cumulative scoring
  data1 <- data[,c("DATE_TIME", "GAME", "TEAM", "PLAYER", "SHOT_TYPE")] %>% # Reorder columns
    mutate(HOME_TEAM = homeTeam) %>%                                        # Add home team
    mutate(SHOT_CHANGE = shotChange(FALSE, SHOT_TYPE)) %>%                  #
    mutate(SCORE_CHANGE = scoreChange(FALSE, SHOT_TYPE)) %>%
    mutate(HOME_SHOT_CHANGE  = shotChange(TEAM!=HOME_TEAM, SHOT_TYPE)) %>%
    mutate(HOME_SHOT_CUMUL   = cumsum(HOME_SHOT_CHANGE)) %>%
    mutate(HOME_SCORE_CHANGE = scoreChange(TEAM!=HOME_TEAM, SHOT_TYPE)) %>%
    mutate(HOME_SCORE_CUMUL  = cumsum(HOME_SCORE_CHANGE)) %>%
    mutate(AWAY_SHOT_CHANGE  = shotChange(TEAM==HOME_TEAM, SHOT_TYPE)) %>%
    mutate(AWAY_SHOT_CUMUL   = cumsum(AWAY_SHOT_CHANGE)) %>%
    mutate(AWAY_SCORE_CHANGE = scoreChange(TEAM==HOME_TEAM, SHOT_TYPE)) %>%
    mutate(AWAY_SCORE_CUMUL  = cumsum(AWAY_SCORE_CHANGE)) %>%
    mutate(TEAM_SCORE_CHANGE = ifelse(HOME_TEAM == TEAM, HOME_SCORE_CHANGE, AWAY_SCORE_CHANGE)) %>%
    mutate(TEAM_SCORE_CUMUL  = ifelse(HOME_TEAM == TEAM, HOME_SCORE_CUMUL,  AWAY_SCORE_CUMUL)) %>%
    mutate(OPPO_SCORE_CHANGE = ifelse(HOME_TEAM == TEAM, AWAY_SCORE_CHANGE, HOME_SCORE_CHANGE)) %>%
    mutate(OPPO_SCORE_CUMUL  = ifelse(HOME_TEAM == TEAM, AWAY_SCORE_CUMUL,  HOME_SCORE_CUMUL)) %>%
    mutate(TARGET = target)
  
  # Calculate target if theres a draw
  if(nrow(data1) >= 2){
  for (row in 2:nrow(data1)) {
    data1$TARGET[row] <- ifelse(data1$HOME_SCORE_CUMUL[row] == data1$AWAY_SCORE_CUMUL[row] & data1$HOME_SCORE_CUMUL[row] == data1$TARGET[row-1],
                                data1$TARGET[row-1] + 3,
                                data1$TARGET[row-1])
  }
  }
    
  data2 <- data1 %>%
    mutate(WINNER = ifelse((TARGET == HOME_SCORE_CUMUL | TARGET == AWAY_SCORE_CUMUL) &
                             HOME_SCORE_CUMUL != AWAY_SCORE_CUMUL &
                             SHOT_TYPE != 'REDEMPTION' &
                             row_number() == nrow(data1), 1,0)) %>%
    mutate(CLUTCH = ifelse(SHOT_TYPE %in% c('BALLS BACK', 'SAME CUP', 'REDEMPTION') | WINNER == 1,1,0)) %>%
    mutate(FANTASY_POINTS_CHANGE = (20 + TEAM_SCORE_CUMUL) * TEAM_SCORE_CHANGE) %>%                                                                                      ## Basic Cup
    mutate(FANTASY_POINTS_CHANGE = ifelse(SHOT_TYPE == 'OVERTHROW', FANTASY_POINTS_CHANGE - ((10 + OPPO_SCORE_CUMUL) * OPPO_SCORE_CHANGE), FANTASY_POINTS_CHANGE)) %>%   ## Overthrow 
    mutate(FANTASY_POINTS_CHANGE = ifelse(SHOT_TYPE == 'TRICKSHOT HIT', FANTASY_POINTS_CHANGE + 15 , FANTASY_POINTS_CHANGE)) %>%                                         ## Trickshot Hit 
    #mutate(FANTASY_POINTS_CHANGE = ifelse(SHOT_TYPE == 'TRICKSHOT MISS', FANTASY_POINTS_CHANGE + 2 , FANTASY_POINTS_CHANGE)) %>%                                         ## Trickshot Miss 
    mutate(FANTASY_POINTS_CHANGE = ifelse(CLUTCH > 0, FANTASY_POINTS_CHANGE + 5 , FANTASY_POINTS_CHANGE)) %>%                                                            ## Clutch 
    mutate(TOTAL_FP_CUMUL = cumsum(abs(FANTASY_POINTS_CHANGE))) %>%
    mutate(TOTAL_FP_PCNT = (maxScore(HOME_SCORE_CUMUL)+maxScore(AWAY_SCORE_CUMUL)) / (maxScore(TARGET) + maxScore(pmin(HOME_SCORE_CUMUL,AWAY_SCORE_CUMUL)))) %>%
    mutate(TOTAL_FP_SCALE = round(noPlayers*75 * TOTAL_FP_PCNT,0)) ### NEED TOO FIX THIS - total score based on number of players
  
  
  players <- sort(unique(data2$PLAYER))
  
  for (i in 1:length(players)){
    
    colName <- paste0('PLAYER',i)
    data2 <- data2 %>%
      mutate(!!paste0(colName,'_FP_CHANGE'):= ifelse(PLAYER == players[i], FANTASY_POINTS_CHANGE, 0)) %>%
      mutate(!!paste0(colName,'_FP_CUMUL'):= cumsum(!!as.name(paste0(colName,'_FP_CHANGE')))) %>%
      mutate(!!paste0(colName,'_FP_SCALE'):= ifelse(TOTAL_FP_CUMUL > 0 , round(pmax(!!as.name(paste0(colName,'_FP_CUMUL')),0) / TOTAL_FP_CUMUL * TOTAL_FP_SCALE,0),0))
  }
  
  return(data2)
}

scoreChange <- function(OTHER_TEAM, SHOT_TYPE){
  case_when(OTHER_TEAM & SHOT_TYPE == 'OVERTHROW'     ~ 1,
            OTHER_TEAM                                ~ 0,
            SHOT_TYPE == 'HIT'                        ~ 1,
            SHOT_TYPE == 'MISS'                       ~ 0,
            SHOT_TYPE == 'OVERTHROW'                  ~ 0,
            SHOT_TYPE == 'TRICKSHOT HIT'              ~ 1,
            SHOT_TYPE == 'TRICKSHOT MISS'             ~ 0,
            SHOT_TYPE == 'BALLS BACK'                 ~ 1,
            SHOT_TYPE == 'SAME CUP'                   ~ 2,
            SHOT_TYPE == 'REDEMPTION'                 ~ 1)
} # Evaluates stats line to determine score increment
shotChange <- function(OTHER_TEAM, SHOT_TYPE){
  case_when(OTHER_TEAM & SHOT_TYPE == 'OVERTHROW'     ~ 0,
            OTHER_TEAM                                ~ 0,
            SHOT_TYPE == 'HIT'                        ~ 1,
            SHOT_TYPE == 'MISS'                       ~ 1,
            SHOT_TYPE == 'OVERTHROW'                  ~ 1,
            SHOT_TYPE == 'TRICKSHOT HIT'              ~ 1,
            SHOT_TYPE == 'TRICKSHOT MISS'             ~ 0,
            SHOT_TYPE == 'BALLS BACK'                 ~ 1,
            SHOT_TYPE == 'SAME CUP'                   ~ 1,
            SHOT_TYPE == 'REDEMPTION'                 ~ 1)
}  # Evaluates stats line to determine shot increment
maxScore <- function(target){  target * 20 + (target*(target+1) / 2)} # Used in Fantasy Points scaling

rawData <- function(vGameList, vTournamentName){
  path <- paste0(vTournamentName, "/data/")
  data <- firebaseDownload(projectURL, path)
  
  if(is.null(data)) return(NULL)
  
  leagueSummary <- tibble()
  leagueStats <- tibble()
  for(i in 1:nrow(vGameList)){
    
    stats <- statsData(vGameList[i,], data[[vGameList[i,]$GAME_NAME]])
    leagueStats <- bind_rows(leagueStats, stats)
    
  }
  
  return(leagueStats) 
}

playerStats <- function(vStatsData){
  
  if(is.null(vStatsData)){
    return(NULL)
  }
  
  data <- vStatsData %>%
    group_by(GAME, TEAM, PLAYER) %>%
    summarise(
      HITS         = sum(ifelse(SCORE_CHANGE>0,1,0)),
      THROWS       = sum(SHOT_CHANGE),
      OVERTHROWS   = sum(ifelse(SHOT_TYPE == 'OVERTHROW',1,0)),
      CLUTCH       = sum(CLUTCH),
      T_HITS       = sum(ifelse(SHOT_TYPE == 'TRICKSHOT HIT',1,0)),
      T_THROWS     = sum(ifelse(SHOT_TYPE %in% c('TRICKSHOT HIT','TRICKSHOT MISS'),1,0)),
      WINNER       = sum(WINNER)
    ) %>%
    arrange(PLAYER) %>%
    mutate(PCNT = ifelse(THROWS>0,round(HITS/THROWS*100,1),0))
  
  if(nrow(vStatsData)<2){
    data$FANTASY <- vStatsData[nrow(vStatsData), grep('PLAYER\\d_FP_SCALE',  names(vStatsData))]
  } else {
    fantasy <- gather(vStatsData[nrow(vStatsData), grep('PLAYER\\d_FP_SCALE',  names(vStatsData))]) %>%
      arrange(key)
    data$FANTASY <- fantasy$value
  }
  
  
  #data$ICON <- c('gun', 'target', 'garbage', 'cone')[1:nrow(data)]
  
  drySpell <- vStatsData %>%
    filter(SHOT_CHANGE == 1) %>%
    arrange(PLAYER, row_number()) %>%
    select(GAME,PLAYER, SHOT_CHANGE,SCORE_CHANGE) %>%
    mutate(THROW_COUNT = cumsum(SCORE_CHANGE) - SCORE_CHANGE) %>%
    group_by(GAME, PLAYER, THROW_COUNT) %>%
    summarise(THROWS = n()) %>%
    ungroup() %>%
    group_by(GAME, PLAYER) %>%
    summarise(DRY_SPELL = max(THROWS)) %>%
    ungroup()
  
  vTeamData <- vStatsData %>%
    group_by(GAME, TEAM) %>%
    summarise(
      PLAYERS      = n_distinct(PLAYER),
      CUPS_FOR     = max(TEAM_SCORE_CUMUL),
      CUPS_AGAINST = max(OPPO_SCORE_CUMUL) + (1 - sum(WINNER))
    ) %>%
    arrange(TEAM) %>%
    ungroup()

  vTeamData2 <- left_join(data, vTeamData[,c(2:5)], by=c('TEAM'='TEAM')) %>%
    mutate(CARRIER = ifelse(CUPS_FOR>0,round(HITS/CUPS_FOR*100,1), 0))

  vTeamData3 <- left_join(vTeamData2, drySpell[,c(2:3)], by=c('PLAYER' = 'PLAYER'))
  
  return(vTeamData3)
}

leagueData <- function(games, vTournamentName){
  
  path <- paste0(vTournamentName, "/data/")
  data <- firebaseDownload(projectURL, path)
  
  if(is.null(data)) return(NULL)
  
  leagueSummary <- tibble()
  leagueStats <- tibble()
  for(i in 1:nrow(games)){
    
    stats <- statsData(games[i,], data[[games[i,]$GAME_NAME]])
    leagueStats <- bind_rows(leagueStats, stats)
    
    stats2 <- playerStats(stats)
    leagueSummary <- bind_rows(leagueSummary, stats2)
  }
 
  return(leagueSummary) 
}
playerData <- function(ldata){
  
  if(is.null(ldata)) return(NULL)
  
  data <- ldata %>%
    group_by(PLAYER) %>%
    summarise(
      P          = n(),
      HITS       = sum(HITS) ,
      THROWS     = sum(THROWS),
      OT         = sum(OVERTHROWS),
      CLT        = sum(CLUTCH),
      T_HITS     = sum(T_HITS),
      T_THROWS   = sum(T_THROWS),
      W          = sum(WINNER),
      FP         = round(mean(FANTASY),1)
    ) %>%
    mutate('T'  = paste0(HITS, " - ", THROWS)) %>%
    mutate('T%' = paste0(ifelse(THROWS>0, round(HITS / THROWS * 100,1), 0),"%")) %>%
    mutate('TS' = paste0(T_HITS, " - ", T_THROWS)) %>%
    arrange(desc(FP)) %>%
    select(PLAYER, P, 'T', 'T%', TS, CLT, W, OT, FP)
  
  return(data)
}
teamData <- function(ldata){
  
  if(is.null(ldata)) return(NULL)
  data <- ldata %>%
    filter(TEAM != PLAYER) %>%
    group_by(TEAM) %>%
    summarise(
      P          = n()/2,
      W          = sum(WINNER),
      HITS       = sum(HITS) ,
      THROWS     = sum(THROWS),
      OT         = sum(OVERTHROWS),
      CLT        = sum(CLUTCH),
      FP         = round(mean(FANTASY),1),
      CF         = sum(CUPS_FOR)/2,
      CA         = sum(CUPS_AGAINST)/2
    ) %>%
    mutate(L    = P - W) %>%
    mutate('T%' = paste0(ifelse(THROWS>0, round(HITS / THROWS * 100,1), 0),"%")) %>%
    mutate(C_PCNT = round(CF / (CA+0.000001),1)) %>%
    mutate('C%' = paste0(round(CF / (CA+0.000001) * 100,1), 0),"%") %>%
    mutate(rank = W + C_PCNT ) %>%
    arrange(desc(rank)) %>%
    select(TEAM, P, W, CF, CA,'C%', 'T%',  CLT, OT, FP) 
  
 return(data)
}
statline <- function(gameData){
  
  if(is.null(gameData)){
    return(NULL)
  }
  
  data <- gameData %>%
    group_by( TEAM, PLAYER) %>%
    summarise(
      HITS         = sum(ifelse(SCORE_CHANGE>0,1,0)),
      THROWS       = sum(SHOT_CHANGE),
      OVERTHROWS   = sum(ifelse(SHOT_TYPE == 'OVERTHROW',1,0)),
      CLUTCH       = sum(CLUTCH),
      T_HITS       = sum(ifelse(SHOT_TYPE == 'TRICKSHOT HIT',1,0)),
      T_THROWS     = sum(ifelse(SHOT_TYPE %in% c('TRICKSHOT HIT','TRICKSHOT MISS'),1,0)),
      WINNER       = sum(WINNER)
    ) %>%
    arrange(PLAYER)
  
  fantasy <- gather(gameData[nrow(gameData), grep('PLAYER\\d_FP_SCALE',  names(gameData))]) %>%
    arrange(key)
  
  data$FANTASY <- fantasy$value
  #data$ICON <- c('gun', 'target', 'garbage', 'cone')[1:nrow(data)]
  
  return(data)
}

displayTeam <- function(teamName, teamScore, scoreColour, player1, player2){
  
  # Handle nulls
  teamName = ifelse(is.null(teamName), "TEAM NAME", teamName)
  teamScore = ifelse(is.null(teamScore), 0, teamScore)
  scoreColour = ifelse(identical(scoreColour, character(0)) , 'default', scoreColour)
  
  ui <- productList(
    headerListItem(
      teamName   = teamName, 
      score      = teamScore, 
      scoreColor = scoreColour
    ),
    displayPlayer(player1),
    displayPlayer(player2)
  )
  
  return(ui)
}
displayPlayer <- function(player){

  print(player$PLAYER)
  
  if(is.null(player)){
    playerPic  = "BLANK.png"
    playerName = "PLAYER"
    lineOne    = "0 - 0 (0%) | FP: 0"
    lineTwo    = "TS: 0 - 0 | CLT: 0 | OT: 0"
    icon       = NULL
  } else if(nrow(player) == 0){
    return(NULL)   
  } else if(is.na(player$PLAYER)[1]){
    return(NULL)
  } else {
    playerPic  = paste0(player$PLAYER, '.jpg')
    playerName = player$PLAYER
    lineOne    = paste0(player$HITS," - ",player$THROWS, " (", ifelse(player$THROWS>0,round(player$HITS/player$THROWS*100,1),0), "%) | FP: ", player$FANTASY)
    lineTwo    = paste0("TS: ",player$T_HITS," - ",player$T_THROWS," | CLT: ",player$CLUTCH," | OT: ",player$OVERTHROWS)
    icon       = NULL #paste0("./icons/", player$ICON ,".gif")
  }
  
  ui <- list(
    playerListItem(
      playerPic  = playerPic, 
      playerName = playerName, 
      lineOne    = lineOne, 
      lineTwo    = lineTwo, 
      icon       = icon
    )
  )
  
  return(ui)
}

uiGameDetails <- function(homeTeam=NULL, awayTeam=NULL, gameData=NULL){

  homeScore <- gameData$HOME_SCORE_CUMUL[nrow(gameData)]
  awayScore <- gameData$AWAY_SCORE_CUMUL[nrow(gameData)]
  
  statline <- statline(gameData)
  
  uiHome <- displayTeam(
    teamName    = homeTeam$TEAM_NAME,
    teamScore   = homeScore,
    scoreColour = case_when(is.null(homeScore)                       ~ 'default',
                            homeScore >  awayScore                   ~ 'success',
                            homeScore <  awayScore                   ~ 'danger',
                            homeScore == awayScore & homeScore == 0  ~ 'default',
                            homeScore == awayScore                   ~ 'warning',
                            TRUE                                     ~ 'default'),
    player1     = statline[statline$PLAYER == homeTeam$PLAYER_1, ],
    player2     = statline[statline$PLAYER == homeTeam$PLAYER_2, ] 
  )
  
  uiAway <- displayTeam(
    teamName  = awayTeam$TEAM_NAME,
    teamScore = awayScore,
    scoreColour = case_when(is.null(awayScore)                       ~ 'default',
                            homeScore <  awayScore                   ~ 'success',
                            homeScore >  awayScore                   ~ 'danger',
                            homeScore == awayScore & homeScore == 0  ~ 'default',
                            homeScore == awayScore                   ~ 'warning',
                            TRUE                                     ~ 'default'),
    player1     = statline[statline$PLAYER == awayTeam$PLAYER_1, ],
    player2     = statline[statline$PLAYER == awayTeam$PLAYER_2, ] 
  )
  #data <- statline(gameData)
  
  ui <- list(
    column(width=6, uiHome),
    column(width=6, uiAway)
  )
  
  return(ui)
}
uiScoreWorm <- function(game, gameData){
  
  if(is.null(game) | is.null(gameData)){
    return(NULL)
  }
  
  homeTeam <- gameData %>%
    group_by(HOME_SHOT_CUMUL) %>%
    summarise(HOME_SCORE = max(HOME_SCORE_CUMUL)) %>%
    rename(SHOT_COUNT = HOME_SHOT_CUMUL)
    #rename(!!game$HOME_TEAM:= HOME_SCORE)
  
  homeTeam <- rbind(homeTeam, c(0,0)) %>%
    arrange(SHOT_COUNT)
  
  awayTeam <- gameData %>%
    group_by(AWAY_SHOT_CUMUL) %>%
    summarise(AWAY_SCORE = max(AWAY_SCORE_CUMUL)) %>%
    rename(SHOT_COUNT = AWAY_SHOT_CUMUL)
    #rename(!!game$AWAY_TEAM:= AWAY_SCORE)
  
  awayTeam <- unique(rbind(awayTeam, c(0,0))) %>%
    arrange(SHOT_COUNT)
  
  chartData <- as_tibble(merge(homeTeam, awayTeam, by = 'SHOT_COUNT', all =TRUE))
  
  vTarget    <- max(gameData$TARGET)
  vHomeScore <- chartData$HOME_SCORE
  vAwayScore <- chartData$AWAY_SCORE
  

  chart <- highchart() %>%
    # hc_title(
    #   text='SCORE WORM',
    #   style=list(fontSize='16px')
    # ) %>%
    hc_xAxis(
      title = list(text =  'Throws'),
      min = 0,
      crosshair = TRUE,
      allowDecimals = FALSE
    ) %>%
    hc_yAxis(
      title = list(text = 'Cups'),
      min = 0,
      max = vTarget,
      tickInterval = 1,
      plotLines = list(
        list(color = 'red',
             dashStyle = "ShortDash",
             width = 2,
             value = vTarget,
             label = list(text = "Target", align = "left", style = list(color= 'red', fontWeight= 'bold')))
      )
    ) %>%
    hc_legend(
      padding= 3,
      verticalAlign= 'top',
      align= 'right'
    ) %>%
    hc_tooltip(
      shared = TRUE,
      headerFormat= "<b>Throw {point.key}</b><br>"
    ) %>%
    hc_plotOptions(
      series= list(animation=list(duration=0))
    ) %>%
    hc_add_series_list(
      list(
        list(#LHS
          name = game$HOME_TEAM,
          data = vHomeScore,
          type = 'spline',
          zIndex = 2
        ),
        list(#LHS
          name = game$AWAY_TEAM,
          data = vAwayScore,
          type = 'spline',
          zIndex = 3
        # ),
        # list(#LHS
        #   name = 'LEAGUE',
        #   data = vLeagueScore,
        #   type = 'spline',
        #   marker = list(enabled = FALSE),
        #   zIndex = 1,
        #   color = 'orange'
        )
      )
    )
  
  return(chart)
}
uiFantasyWorm <- function(gameData){
  
  if(is.null(gameData)){
    return(NULL)
  }
  
  players <- sort(unique(gameData$PLAYER))
  
  fantasyData <- gameData[,grep('PLAYER\\d_FP_SCALE',  names(gameData))]
  names(fantasyData) <- sort(unique(gameData$PLAYER))
  
  fantasyPlayers <- lapply(fantasyData, function(x){
    list(#LHS
      name = 'name',
      data = x,
      type = 'spline'
    )
  })
  for(i in 1:ncol(fantasyData)){
    fantasyPlayers[[i]]$name <- players[i]
  }
  fantasyPlayers <- unname(fantasyPlayers)  
  
  chart <- highchart() %>%
    # hc_title(
    #   text='SCORE WORM',
    #   style=list(fontSize='16px')
    # ) %>%
    hc_xAxis(
      title = list(text =  'Throws'),
      min = 0,
      crosshair = TRUE,
      allowDecimals = FALSE
    ) %>%
    hc_yAxis(
      title = list(text = 'Fantasy Points'),
      min = 0,
      #max = vTarget,
      #tickInterval = 1,
      plotLines = list(
        list(color = 'red',
             dashStyle = "ShortDash",
             width = 2,
             value = 75,
             label = list(text = "Average", align = "left", style = list(color= 'red', fontWeight= 'bold')))
      )
    ) %>%
    hc_legend(
      padding= 3,
      verticalAlign= 'top',
      align= 'right'
    ) %>%
    hc_tooltip(
      shared = TRUE,
      headerFormat= "<b>Throw {point.key}</b><br>"
    ) %>%
    hc_plotOptions(
      series= list(animation=list(duration=0))
    ) %>%
    hc_add_series_list(
      fantasyPlayers
    )
  
  return(chart)
}

## REACTIVE FUNCTIONS ----
rv <- reactiveValues(gameData = 0)
game_r <- reactive({
  game <- game(gameList_r(), input$lstGameSelected)
  return(game)
}) 
statsData_r <- reactive({
  
  if(input$cbxLive){
    invalidateLater(5*1000)
    print('update')
  }
  
  statsData <- statsData(game_r(), gameData(game_r()$GAME_NAME, tournamentName_r()))
  return(statsData)
}) 
leagueData_r <- reactive({
  input$lstGameSelected
  data <- leagueData(gameList_r(), tournamentName_r())
  return(data)
})

## UI RENDERS ----
output$uiLstGameSelected <- renderUI({

  # Get games table
  games <- gameList_r()
  
  # Handle null games
  gamesList <- c("No Games Available", gameList_r()$GAME_NAME)
  
  # Build UI
  ui <- selectInput(
    inputId= "lstGameSelected",
    label  = NULL,
    choices= gamesList
  )
  
  return(ui)
}) # Render game list 
output$uiGameDetails <- renderUI({
  req(input$lstGameSelected) # Require dropdown list be generated

  #Generate UI
  ui <- uiGameDetails(
    homeTeam = team(teamList_r(), game_r()$HOME_TEAM),
    awayTeam = team(teamList_r(), game_r()$AWAY_TEAM),
    gameData = statsData_r())
  return(ui)
}) # Render left team side of stats sheet
output$uiScoreWorm <- renderHighchart({
  chart <- uiScoreWorm(game_r(), statsData_r())
  return(chart)
})
output$uiFantasyWorm <- renderHighchart({
  chart <- uiFantasyWorm(statsData_r())
  return(chart)
})

output$tblPlayerStats <- renderDataTable({
  
  data <- playerData(leagueData_r())
  if(is.null(data)) return(NULL)
  
  table <- datatable(data,
                     rownames= FALSE,
                     options= list(paging= FALSE, 
                                   searching= FALSE, 
                                   ordering=FALSE, 
                                   info=FALSE,
                                   columnDefs = list(
                                     list(
                                       className = 'dt-center', 
                                       targets = 1:(ncol(data)-1)))
                                   ),
                     extensions="Responsive"
                     ) %>%
    formatStyle( 0, target= 'row', lineHeight='70%')
  
  return(table)
  
})
output$tblTeamStats <- renderDataTable({
  
  data <- teamData(leagueData_r())
  if(is.null(data)) return(NULL)
  
  table <- datatable(data,
                     rownames= FALSE,
                     options= list(paging= FALSE, 
                                   searching= FALSE, 
                                   ordering=FALSE, 
                                   info=FALSE,
                                   columnDefs = list(list(className = 'dt-center', targets = 1:(ncol(data)-1)))
                     ),
                     extensions="Responsive"
  ) %>%
    formatStyle( 0, target= 'row', lineHeight='70%')
  
  return(table)
  
})

output$uiFantasyPig <- renderUI({
  
  data <- leagueData_r()
  
  first <- NULL
  second <- NULL
  third <- NULL
  
  if(!is.null(data)){
    
    smy <- data %>%
      arrange(desc(FANTASY), desc(HITS))
    
    first <- ifelse(!is.na(smy$FANTASY[1]),  paste0(smy$FANTASY[1],' - ' ,smy$PLAYER[1]),'')
    second <- ifelse(!is.na(smy$FANTASY[2]), paste0(smy$FANTASY[2],' - ' ,smy$PLAYER[2]),'')
    third <- ifelse(!is.na(smy$FANTASY[3]),  paste0(smy$FANTASY[3],' - ' ,smy$PLAYER[3]),'')
  }
  
  ui <- leaderBox(title= "FANTASY PIG", 
                  icon=icon('piggy-bank'), 
                  color='green', 
                  width = 4,
                  first = first,
                  second = second,
                  third = third)
  return(ui)
})
output$uiRobinHood <- renderUI({
  
  data <- leagueData_r()
  
  first <- NULL
  second <- NULL
  third <- NULL
  
  if(!is.null(data)){
    
    smy <- data %>%
      arrange(desc(PCNT), desc(HITS))
    
    first <- ifelse(!is.na(smy$PCNT[1]),  paste0(smy$PCNT[1],'% - ' ,smy$PLAYER[1]),'')
    second <- ifelse(!is.na(smy$PCNT[2]), paste0(smy$PCNT[2],'% - ' ,smy$PLAYER[2]),'')
    third <- ifelse(!is.na(smy$PCNT[3]),  paste0(smy$PCNT[3],'% - ' ,smy$PLAYER[3]),'')
  }
  
  ui <- leaderBox(title= "ROBIN HOOD", 
                  icon=icon('crosshairs'), 
                  color='green', 
                  width = 4,
                  first = first,
                  second = second,
                  third = third)
  

  return(ui)
})
output$uiAtlas <- renderUI({
  
  data <- leagueData_r()
  
  first <- NULL
  second <- NULL
  third <- NULL
  
  if(!is.null(data)){
    
    smy <- data %>%
      filter(PLAYERS > 1) %>%
      arrange(desc(CARRIER), desc(HITS))
    
    first <- ifelse(!is.na(smy$CARRIER[1]),  paste0(smy$CARRIER[1],'% - ' ,smy$PLAYER[1]),'')
    second <- ifelse(!is.na(smy$CARRIER[2]), paste0(smy$CARRIER[2],'% - ' ,smy$PLAYER[2]),'')
    third <- ifelse(!is.na(smy$CARRIER[3]),  paste0(smy$CARRIER[3],'% - ' ,smy$PLAYER[3]),'')
  }
  
  ui <- leaderBox(title= "ATLAS", 
                  icon=icon('hands'), 
                  color='orange', 
                  width = 4,
                  first = first,
                  second = second,
                  third = third)
  
  return(ui)
})
output$uiSpud <- renderUI({
  
  data <- leagueData_r()
  
  first <- NULL
  second <- NULL
  third <- NULL
  
  if(!is.null(data)){
    
    smy <- data %>%
      arrange(FANTASY, HITS)
    
    first <- ifelse(!is.na(smy$FANTASY[1]),  paste0(smy$FANTASY[1],' - ' ,smy$PLAYER[1]),'')
    second <- ifelse(!is.na(smy$FANTASY[2]), paste0(smy$FANTASY[2],' - ' ,smy$PLAYER[2]),'')
    third <- ifelse(!is.na(smy$FANTASY[3]),  paste0(smy$FANTASY[3],' - ' ,smy$PLAYER[3]),'')
  }
  
  ui <- leaderBox(title= "SPUD", 
                  icon=icon('poop'), 
                  color='red', 
                  width = 4,
                  first = first,
                  second = second,
                  third = third)
  
  return(ui)
})
output$uiDrySpell <- renderUI({
  
  data <- leagueData_r()
  
  first <- NULL
  second <- NULL
  third <- NULL
  
  if(!is.null(data)){
    
    smy <- data %>%
      arrange(desc(DRY_SPELL), THROWS)
    
    first <- ifelse(!is.na(smy$DRY_SPELL[1]),  paste0(smy$DRY_SPELL[1],' - ' ,smy$PLAYER[1]),'')
    second <- ifelse(!is.na(smy$DRY_SPELL[2]), paste0(smy$DRY_SPELL[2],' - ' ,smy$PLAYER[2]),'')
    third <- ifelse(!is.na(smy$DRY_SPELL[3]),  paste0(smy$DRY_SPELL[3],' - ' ,smy$PLAYER[3]),'')
  }
  
  ui <- leaderBox(title= "DRY SPELL", 
                  icon=icon('tint'), 
                  color='blue', 
                  width = 4,
                  first = first,
                  second = second,
                  third = third)
  
  return(ui)
})
