
firebaseDownload <- function(projectURL, path = NULL){
  
  message(path)
  data <- suppressWarnings(download(projectURL, paste0('Beer-Pong-Dashboard/',path)))
  return(data)
} # Download data from firebase location

firebaseSave <- function(projectURL, path = NULL, data){
  if(is.null(path)) return(NULL)
  put(data, projectURL, paste0('Beer-Pong-Dashboard/', path))
} # Save data to firebase location

