firebaseDownload <- function(projectURL, path = NULL){
  if(is.null(path)) return(NULL)
  data <- suppressWarnings(download(projectURL, paste0('Beer-Pong-Dashboard/',path)))
  return(data)
} # Download data from firebase location