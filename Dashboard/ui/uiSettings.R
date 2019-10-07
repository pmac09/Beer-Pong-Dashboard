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
        ),
        
        ## TEAM ----
        tabPanel(
          "Teams",
          hr(),
          h3("Teams"),
          hr(),
          column(width = 6, 
                 DT::dataTableOutput("tblTeamList"),
                 hr()
          ),
          column(width = 6,
                 uiOutput('uiLstPlayer1'),
                 uiOutput('uiLstPlayer2'),
                 textInput('txtTeamName', label= "Team Name", width = '100%'),
                 actionButton('btnAddTeam', "Add Team")
          )
        ),
        
        ## GAME ----
        tabPanel(
          "Games",
          hr(),
          h3("Games"),
          hr(),
          column(width = 6, 
                 DT::dataTableOutput("tblGameList"),
                 hr()
          ),
          column(width = 6,
                 uiOutput('uiLstTeam1'),
                 uiOutput('uiLstTeam2'),
                 textInput('txtGameName', label= "Game Name", width = '100%'),
                 actionButton('btnAddGame', "Add Game")
          )
        )
        
      )
    )
  )
)