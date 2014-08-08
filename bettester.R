bettester <- function(data, start=101, reps=100000){
    ##find length of test
    total_len <- nrow(data)

    win <-     matrix(c(0), nrow=2, ncol=2)
    colnames(win) <- c("Winnings", "Bet fee")
    rownames(win) <- c("Result", "Goals")
    
    for(i in start:total_len ){
        
        ##Get odds
        odds_r <- c(data$BbMxH[i], data$BbMxD[i], data$BbMxA[i])
        odds_goals <- c(data$BbMx.2.5.1, data$BbMx.2.5)
        
        play_sug <- game_bets(data, data$HomeTeam[i], data$AwayTeam[i], date=data$Date[i], 
                  odds_res = odds_r, odds_25 = odds_goals, reps = reps)
        
        print(play_sug)
        
        #Kontroller samlet spil om der er gevinst
        win[1,2] <- win[1,2] + Reduce("+", play_sug[[1]])
        win[2,2] <- win[2,2] + Reduce("+", play_sug[[2]])

        if(pr09$FTR[i] == "H"){
            result = 1
        }else if(pr09$FTR[i] == "D"){
            result = 2
        }else if(pr09$FTR[i] == "A"){
            result = 3
        }else{
            print("ERROR")
        }
        
        if(pr09$FTHG[i]+pr09$FTAG[i]>2.5){
            goals = 2
        }else if(pr09$FTHG[i]+pr09$FTAG[i]<2.5){
            goals = 1
        }else{
            print("ERROR")
        }
        
        win[1,1] <- win[1,1] + as.numeric(play_sug[[1]][result])*odds_r[result]
        win[2,1] <- win[2,1] + as.numeric(play_sug[[2]][goals])*odds_goals[goals]
        
        #Tilføj til win matrix
    }
    win
} 