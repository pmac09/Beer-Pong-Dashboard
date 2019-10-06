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
         
         leaderBox(title= "FANTASY PIG", icon=icon('piggy-bank'), color='green', width = 4),
         leaderBox(title= "ROBIN HOOD",  icon=icon('crosshairs'), color='green', width = 4),
         leaderBox(title= "STREAKER",    icon=icon('angle-double-right'), color='green', width = 4),
         leaderBox(title= "ATLAS",       icon=icon('hands'), color='orange', width = 4),
         leaderBox(title= "LIABILITY",   icon=icon('exclamation-triangle'), color='red', width = 4),
         leaderBox(title= "DRY SPELL",   icon=icon('tint'), color='blue', width = 4, third= "NAme is way too long so it should cut off"),
         
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