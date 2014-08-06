team_df<-function(data, team){
    ## Create Dataframe for games
    ##      games_temp<-data.frame()
    print(data[,2])
    games_temp <- as.data.frame(matrix(0, ncol = 30, nrow = 1))
    colnames(games_temp) <- c("Div","Date","Hometeam","AwayTeam")  
    ## find number of observations/games in data
    l_vec=length(data[,1])
    
    ## Input data of games to Dataframe - u is used to set next row in dataframe
    u <- 1
    for(i in 1:l_vec){
        if(data$HomeTeam[i] == team){
            ##for(y in 1:6)
            ##games_temp[u,y]<-data[i,y]
            games_temp[u,2]<- as.Date(data[i,2])
            
            u<-u+1
        }
        
        ##if(data$Hometeam == )
    }
    print(games_temp)
}