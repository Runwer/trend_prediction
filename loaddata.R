loaddata <- function(data){
    tempread <- read.csv(data)
    tempread[,2] <- as.Date(tempread[,2], format="%d/%m/%y")
    tempread
}