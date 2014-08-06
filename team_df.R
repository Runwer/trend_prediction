team_df<-function(data, team){
    ## Create Dataframe for games
    ##      games_temp<-data.frame()
    games_temp <- as.data.frame(matrix(0, ncol = 27, nrow = 1))
    games_temp[,2] <- as.Date(games_temp[,2], format='%d/%m/%Y', origin="01-01-01")
    colnames(games_temp) <- c("Div","Date","Team","Opponent", "FTTG", "FTOG", "HTTG", "HTOG", "TS", "OS", 
                              "TTS", "OTS", "THW", "OHW", "TC", "OC", "BbMxH", "BbAvH", "BbMxD", "BbAvD", 
                              "BbMxA", "BbAvA", "BbMx.2.5", "BbAv.2.5", "BbMx.2.5.1", "BbAv.2.5.1", "HorA")  
    ## find number of observations/games in data
    l_vec=length(data[,1])
    
    ## Input data of games to Dataframe - u is used to set next row in dataframe
    u <- 1
    for(i in 1:l_vec){
        if(data$HomeTeam[i] == team){
            games_temp[u,1]<-data[i,1]
            games_temp[u,2]<- as.Date(data[i,2], format='%d/%m/%Y')
            for(y in 3:4){
                games_temp[u,y]<-as.character(data[i,y])
                print(y)
                print(data[i,y])
            }
            for(y in 5:6){
                games_temp[u,y]<-as.numeric(data[i,y])
            }
            o <- 7
            for(y in 8:9){
                games_temp[u,o]<-as.numeric(data[i,y])
                o = o+1
            }
            games_temp$TS[u] <- data$HS[i] 
            games_temp$HorA[u] <- "H"
            u<-u+1
        
        }
        
        if(data$AwayTeam[i] == team){
            games_temp[u,1]<-data[i,1]
            games_temp[u,2]<- as.Date(data[i,2], format='%d/%m/%Y')
            for(y in 3:4){
                games_temp[u,y]<-as.character(data[i,y])
                print(y)
                print(data[i,y])
            }
            for(y in 5:6){
                games_temp[u,y]<-as.numeric(data[i,y])
            }
            o <- 7
            for(y in 8:9){
                games_temp[u,o]<-as.numeric(data[i,y])
                o = o+1
            }
            games_temp$OS[u] <- data$HS[i] 
            games_temp$HorA[u] <- "A"
            u<-u+1            
            
        }

        
    }
    print(games_temp)
}