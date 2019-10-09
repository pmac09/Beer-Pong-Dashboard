firebaseSave <- function(projectURL, path = NULL, data){
  if(is.null(path)) return(NULL)
  put(data, projectURL, paste0('Beer-Pong-Dashboard/', path))
} # Save data to firebase location
