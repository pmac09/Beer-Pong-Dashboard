fluidRow(
  column(
    width=12,
    
    useSweetAlert(),
    
    box(
      width=NULL,
      tabsetPanel(
        type = "tabs",
        
        ## PLAYER ----
        tabPanel(
          "Players", 
          hr(),
          
          column(
            width= 6,
            textInput(inputId= "txtPlayerName", label= "Player Name"),
            actionButton(inputId= "btnAddPlayer", label= "Add Player")
          ),
          column(
            width= 6,
            h4("Player List"),
            textOutput("tblPlayerList")
          )
        ),
        
        ## TEAMS ----
        tabPanel(
          "Teams", 
          hr(),
          h4("Teams List"),
          #textOutput("tblPlayerList"),
          hr(),
          DT::dataTableOutput("tblTeamList"),
          hr(),
          textInput(inputId= "txtTeamName", label= "Team Name"),
          uiOutput("uiLstTeamPlayer1"),
          uiOutput("uiLstTeamPlayer2"),
          actionButton(inputId= "btnAddTeam", label= "Add Team")
        ),
        
        ## GAMES ----
        tabPanel(
          "Games", 
          hr(),
          h4("Games List"),
          hr(),
          DT::dataTableOutput("tblGameList"),
          hr(),
          textInput(inputId= "txtGameName", label= "Game Name"),
          uiOutput("uiLstHomeTeam"),
          uiOutput("uiLstAwayTeam"),
          actionButton(inputId= "btnAddGame", label= "Add Game")
        ),
        tabPanel(
          "Reset",
          hr(),
          "Reset Everything"
        )
      )
    )
  )
)