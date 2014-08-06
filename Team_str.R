##team_str returns a vector where the first number is the teams offense strenth and the second 
##is the teams defense strength
team_str <- function(data, team, date=format(Sys.time(), "%Y-%m-%d"), history=38){
    
    sub_data <- subset(data, data$Date <= date)
    sub_team <- subset(sub_data, sub_data$HomeTeam == team|sub_data$AwayTeam == team)
    team_sel <- tail(sub_team, history)
    vecl <- nrow(team_sel)

    rownames(team_sel) <- 1:nrow(team_sel)
    sumT = 0
    sumO = 0
    for(i in 1:vecl){
        if(team_sel$HomeTeam[i] == team){
            sumT = sumT + as.numeric(team_sel$FTHG[i])
            sumO = sumO + as.numeric(team_sel$FTAG[i])

        }
        if(team_sel$AwayTeam[i] == team){
            sumT = sumT + as.numeric(team_sel$FTAG[i])
            sumO = sumO + as.numeric(team_sel$FTHG[i])
        }
    }

    team_s <- c(sumT/vecl, sumO/vecl)
    
}