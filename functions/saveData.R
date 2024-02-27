library(fireData)

## Get Project URL from environment variables
projectURL <- Sys.getenv('DATABASE_URL')

## Save data to Firebase
put(playerList, projectURL, directory = "Beer-Pong-Dashboard/players")

## Download data from database
test <- download(projectURL, "Beer-Pong-Dashboard")


