
headerListItem <- function(teamName= NULL, score= NULL, scoreColor= 'default'){
  
  cl <- "label pull-right"
  if (!is.null(scoreColor)){cl <- paste0(cl, " label-", scoreColor)}
    
  shiny::tags$li(
    shiny::tags$span(class = "product-title", teamName, 
      shiny::tags$span(class = cl, score))
  )
}

playerListItem <- function(playerPic= NULL, playerName= NULL, lineOne= NULL, lineTwo= NULL, icon= NULL){
  shiny::tags$li(
    class = "item", 
    shiny::tags$div(class= "product-img", shiny::tags$img(src= playerPic, alt= "Product Image")), 
    shiny::tags$div(class = "product-info", 
                    shiny::tags$span(class = "product-title", playerName, shiny::tags$span(class = "label pull-right", tags$div(title="tooltip", shiny::tags$img(src = icon)))), 
                    shiny::tags$span(class = "product-description2", lineOne),
                    shiny::tags$span(class = "product-description2", lineTwo)
    )
  )
}



