team_str <- function(data, team, date=format(Sys.time(), "%Y-%m-%d"), history=38){
    
    sub_data <- subset(data, data$Date <= date)
    sub_team <- subset(sub_data, sub_data$HomeTeam == team|sub_data$AwayTeam == team)
    team_sel <- tail(sub_team, history)
    vecl <- nrow(team_sel)
    print(vecl)
    rownames(team_sel) <- 1:nrow(team_sel)
    sumT = 0
    for(i in 1:vecl){
        if(team_sel$HomeTeam[i] == team){
            sumT = sumT + as.numeric(team_sel$FTHG)
            print(as.numeric(team_sel$FTHG))
        }
        if(team_sel$AwayTeam[i] == team){
            sumT = sumT + as.numeric(team_sel$FTAG)
        }
    }
    
    
    print(sumT)

                                          
    ##Subset games with team_h as HomeTeam or AwayTeam
        ##Select 'history' last games in data and calculate mean
}