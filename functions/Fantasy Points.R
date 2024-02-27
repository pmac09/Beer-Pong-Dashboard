scoreChange <- function(OTHER_TEAM, SHOT_TYPE){
  case_when(OTHER_TEAM & SHOT_TYPE == 'OVERTHROW'     ~ 1,
            OTHER_TEAM                                ~ 0,
            SHOT_TYPE == 'HIT'                        ~ 1,
            SHOT_TYPE == 'MISS'                       ~ 0,
            SHOT_TYPE == 'CAUGHT'                     ~ 0,
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
            SHOT_TYPE == 'CAUGHT'                     ~ 1,
            SHOT_TYPE == 'OVERTHROW'                  ~ 1,
            SHOT_TYPE == 'TRICKSHOT HIT'              ~ 1,
            SHOT_TYPE == 'TRICKSHOT MISS'             ~ 0,
            SHOT_TYPE == 'BALLS BACK'                 ~ 1,
            SHOT_TYPE == 'SAME CUP'                   ~ 1,
            SHOT_TYPE == 'REDEMPTION'                 ~ 1)
}  # Evaluates stats line to determine shot increment


gameName <- 'RR5'

data <- as_tibble(download(projectURL, paste0("Beer-Pong-Dashboard/data/",gameName)))

data$HOME_TEAM <- data$TEAM[1]

data2 <- data %>%
  select(DATE_TIME, GAME, TEAM, PLAYER, SHOT_TYPE, HOME_TEAM) %>%
  mutate(SHOT_CHANGE = shotChange(FALSE, SHOT_TYPE)) %>%
  mutate(SCORE_CHANGE = scoreChange(FALSE, SHOT_TYPE)) %>%
  mutate(HOME_SHOT_CHANGE = shotChange(TEAM!=HOME_TEAM, SHOT_TYPE)) %>%
  mutate(HOME_SCORE_CHANGE = scoreChange(TEAM!=HOME_TEAM, SHOT_TYPE)) %>%
  mutate(AWAY_SHOT_CHANGE = shotChange(TEAM==HOME_TEAM, SHOT_TYPE)) %>%
  mutate(AWAY_SCORE_CHANGE = scoreChange(TEAM==HOME_TEAM, SHOT_TYPE)) %>%
  mutate(CUMUL_HOME_SHOT = cumsum(HOME_SHOT_CHANGE)) %>%
  mutate(CUMUL_HOME_SCORE = cumsum(HOME_SCORE_CHANGE)) %>%
  mutate(CUMUL_AWAY_SHOT= cumsum(AWAY_SHOT_CHANGE)) %>%
  mutate(CUMUL_AWAY_SCORE = cumsum(AWAY_SCORE_CHANGE)) %>%
  mutate(TARGET = ifelse(substr(GAME,1,1)=='I',6,10))

for (row in 2:nrow(data2)) {
  data2$TARGET[row] <- ifelse(data2$CUMUL_HOME_SCORE[row] == data2$CUMUL_AWAY_SCORE[row] & data2$CUMUL_HOME_SCORE[row] == data2$TARGET[row-1],
                              data2$TARGET[row-1] + 3,
                              data2$TARGET[row-1])
}

data3 <- data2 %>%
  mutate(WINNER = ifelse((TARGET == CUMUL_HOME_SCORE | TARGET == CUMUL_AWAY_SCORE) &
                           CUMUL_HOME_SCORE != CUMUL_AWAY_SCORE &
                           SHOT_TYPE != 'REDEMPTION' &
                           row_number() == nrow(data2), 1,0)) %>%
  mutate(CLUTCH = ifelse(SHOT_TYPE %in% c('BALLS BACK', 'SAME CUP', 'REDEMPTION') | WINNER == 1,1,0))



data4 <- data3 %>%
  mutate(TEAM_SCORE_CHANGE = ifelse(HOME_TEAM == TEAM, HOME_SCORE_CHANGE, AWAY_SCORE_CHANGE)) %>%
  mutate(TEAM_SCORE_CUMUL  = ifelse(HOME_TEAM == TEAM, CUMUL_HOME_SCORE,  CUMUL_AWAY_SCORE)) %>%
  mutate(OPPO_SCORE_CHANGE = ifelse(HOME_TEAM == TEAM, AWAY_SCORE_CHANGE, HOME_SCORE_CHANGE)) %>%
  mutate(OPPO_SCORE_CUMUL  = ifelse(HOME_TEAM == TEAM, CUMUL_AWAY_SCORE,  CUMUL_HOME_SCORE))

# Fantasy Points Allocation
maxScore <- function(target){  target * 10 + (target*(target+1) / 2)}

data5 <- data4 %>%
  mutate(FANTASY_POINTS_CHANGE = (10 + TEAM_SCORE_CUMUL) * TEAM_SCORE_CHANGE) %>%                                                                                      ## Basic Cup
  mutate(FANTASY_POINTS_CHANGE = ifelse(SHOT_TYPE == 'OVERTHROW', FANTASY_POINTS_CHANGE - ((10 + OPPO_SCORE_CUMUL) * OPPO_SCORE_CHANGE), FANTASY_POINTS_CHANGE)) %>%   ## Overthrow 
  mutate(FANTASY_POINTS_CHANGE = ifelse(SHOT_TYPE == 'TRICKSHOT HIT', FANTASY_POINTS_CHANGE + 15 , FANTASY_POINTS_CHANGE)) %>%                                         ## Trickshot Hit 
 #mutate(FANTASY_POINTS_CHANGE = ifelse(SHOT_TYPE == 'TRICKSHOT MISS', FANTASY_POINTS_CHANGE + 2 , FANTASY_POINTS_CHANGE)) %>%                                         ## Trickshot Miss 
  mutate(FANTASY_POINTS_CHANGE = ifelse(CLUTCH > 0, FANTASY_POINTS_CHANGE + 5 , FANTASY_POINTS_CHANGE)) %>%                                                            ## Clutch 
  mutate(TOTAL_FP_CUMUL = cumsum(FANTASY_POINTS_CHANGE)) %>%
  mutate(TOTAL_FP_PCNT = (maxScore(CUMUL_HOME_SCORE)+maxScore(CUMUL_AWAY_SCORE)) / (maxScore(TARGET) + maxScore(pmin(CUMUL_HOME_SCORE,CUMUL_AWAY_SCORE)))) %>%
  mutate(TOTAL_FP_SCALE = round(150 * TOTAL_FP_PCNT,0)) ### NEED TOO FIX THIS - total score based on number of players

players <- unique(data5$PLAYER)

for (i in 1:length(players)){
  
  colName <- paste0('PLAYER',i)
  
  data5 <- data5 %>%
    mutate(!!paste0(colName,'_FP_CHANGE'):= ifelse(PLAYER == players[i], FANTASY_POINTS_CHANGE, 0)) %>%
    mutate(!!paste0(colName,'_FP_CUMUL'):= cumsum(!!as.name(paste0(colName,'_FP_CHANGE')))) %>%
    mutate(!!paste0(colName,'_FP_SCALE'):= ifelse(TOTAL_FP_CUMUL > 0 , round(pmax(!!as.name(paste0(colName,'_FP_CUMUL')),0) / TOTAL_FP_CUMUL * TOTAL_FP_SCALE,0),0))
            
  
}



