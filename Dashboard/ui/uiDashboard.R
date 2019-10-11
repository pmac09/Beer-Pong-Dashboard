fluidRow(
  column(## LEFT SIDE -----
         width = 5,
         
         ## GAME STATS
         box(
           width=NULL,
           fluidRow(
             column(width=9, uiOutput('uiLstGameSelected')),
             column(width=3, switchInput(inputId = "cbxLive",label = "LIVE", labelWidth = "80px")),
             uiOutput('uiGameDetails')
           )
         ),
         
         ## GAME CHARTS
         box(
           width=NULL,
           tabsetPanel(
             type = "tabs",
             tabPanel("Score Worm", highchartOutput('uiScoreWorm')), 
             tabPanel("Fantasy Worm", highchartOutput('uiFantasyWorm'))
           )
         )
  ), ## END LEFT SIDE 
  
  column(## RIGHT SIDE -----
         width = 7, 
         
         uiOutput('uiFantasyPig'),
         uiOutput('uiRobinHood'),
         
         leaderBox(title= "STREAKER",    icon=icon('angle-double-right'), color='green', width = 4),
         uiOutput('uiAtlas'),
         uiOutput('uiSpud'),
         uiOutput('uiDrySpell'),
         
         ## PLAYER STATS
         box(
           width=12,
           tabsetPanel(
             type = "tabs",
             tabPanel("Player Stats", dataTableOutput('tblPlayerStats')), 
             tabPanel("Team Stats", dataTableOutput('tblTeamStats'))
           )
         )
         
  )
)