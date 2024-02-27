

smy <- leagueStats %>%
  select(PLAYER, TARGET, TEAM_SCORE_CUMUL, SCORE_CHANGE) %>%
  mutate(REM_CUPS = TARGET - (TEAM_SCORE_CUMUL - SCORE_CHANGE)) %>% 
  group_by(PLAYER, REM_CUPS) %>%
  summarise(
    THROWS = n(),
    HITS   = sum(SCORE_CHANGE)
  ) %>%
  mutate('T%' = round(HITS/THROWS*100,1))
%>%
  select(PLAYER, REM_CUPS, 'T%') 

         
smy2 <- spread(smy, PLAYER, 'T%') %>%
  arrange(desc(REM_CUPS))

margin <- leagueStats %>%
  mutate(REM_CUPS = TARGET - (TEAM_SCORE_CUMUL - SCORE_CHANGE)) %>% 
  mutate(MARGIN = (TEAM_SCORE_CUMUL - SCORE_CHANGE) - OPPO_SCORE_CUMUL) %>%
  mutate(POS = ifelse(MARGIN < 0 , 'DOWN', ifelse(MARGIN >0, 'UP', 'EVEN'))) %>%
  group_by(PLAYER, POS) %>%
  summarise(
    THROWS = n(),
    HITS   = sum(SCORE_CHANGE)
  ) %>%
  mutate('T%' = round(HITS/THROWS*100,1)) %>%
  select(PLAYER, POS, 'T%') 


margin2 <- spread(margin, PLAYER, 'T%') %>%
  arrange(POS)
margin2


tif <- leagueStats %>%
  mutate(REM_CUPS = TARGET - (TEAM_SCORE_CUMUL - SCORE_CHANGE)) %>% 
  mutate(MARGIN = (TEAM_SCORE_CUMUL - SCORE_CHANGE) - OPPO_SCORE_CUMUL) %>%
  mutate(TYPE = ifelse(TEAM == PLAYER, "SINGLE", "TEAM")) %>%
  mutate(POS = ifelse(MARGIN < 0 , 'DOWN', ifelse(MARGIN >0, 'UP', 'EVEN'))) %>%
  group_by(TYPE, PLAYER) %>%
  summarise(
    THROWS = n(),
    HITS   = sum(SCORE_CHANGE)
  ) %>%
  mutate('T%' = round(HITS/THROWS*100,1)) %>%
  select(TYPE, PLAYER, 'T%') 


tif2 <- spread(tif, PLAYER, 'T%') %>%
  arrange(desc(TYPE))
tif2

