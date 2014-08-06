team_df<-function(data, team){
    ## Create Dataframe for team
    games_temp<-data.frame(Team=as.character())
    
    ## find number of observations/games in data
    l_vec=length(data[,1])
    
    ## Find unique teams and number of teams
    teams <- unique(data$HomeTeam)
    team_nr <- length(teams)    
    
    ## Input data of games to Dataframe - with teams as observations. Go through all teams with "i"
    ## and then go through each game to match
    for(i in 1:team_nr){
        for(u in 1:l_vec){
            if(data$HomeTeam[u] == teams[i]){
                team[i](teams[i])
                print(data[u,5])
            }
        }
        
        ##if(data$Hometeam == )
    }
    
    team_temp
}