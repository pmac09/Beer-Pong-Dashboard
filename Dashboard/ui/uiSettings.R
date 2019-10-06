fluidRow(
  column(
    width=12,
    
    useSweetAlert(),
    
    box(
      width=NULL,
      tabsetPanel(
        type = "tabs",
        
        ## TOURNAMENT ----
        tabPanel(
          "Tournaments",
          hr(),
          h3("Tournaments"),
          hr(),
          column(width = 6, 
                 DT::dataTableOutput("tblTournamentList"),
                 hr(),
                 actionButton('btnSelectTournament', "Select Tournament")),
          column(width = 6,
            textInput('txtNewTournament', label= "New Tournament", width = '100%'),
            actionButton('btnAddTournament', "Add Tournament")
            )
        ),
        
        ## PLAYERS ----
        tabPanel(
          "Players",
          hr(),
          h3("Players"),
          hr(),
          column(width = 6, 
                 DT::dataTableOutput("tblPlayerList"),
                 hr()
                 ),
          column(width = 6,
                 textInput('txtNewPlayer', label= "New Player", width = '100%'),
                 actionButton('btnAddPlayer', "Add Player")
          )
        )
        
      )
    )
  )
)