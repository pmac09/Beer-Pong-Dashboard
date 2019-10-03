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
             tabPanel("Score Worm", "Score Worm"),
             tabPanel("Fantasy Worm", "Fantasy Worm")
           )
         )
  ), ## END LEFT SIDE 
  
  column(## RIGHT SIDE -----
         width = 7, 
         
         infoBox(title= "TEST",width = 4,"More testing"),
         infoBox(title= "TEST",width = 4,"More testing"),
         infoBox(title= "TEST",width = 4,"More testing"),
         infoBox(title= "TEST",width = 4,"More testing"),
         infoBox(title= "TEST",width = 4,"More testing"),
         infoBox(title= "TEST",width = 4,"More testing"),
         
         box(
           title= "Right Side",
           status= "warning",
           width= 6
         ),
         box(
           title= "Right Side",
           status= "warning",
           width= 6
         ),
         box(
           title= "Player Stats",
           status= "info",
           width= 12
         )
  )
)