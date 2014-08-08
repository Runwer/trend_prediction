game_bets<-function(data, team_h, team_a, date=format(Sys.time(), "%Y-%m-%d"), history=38, 
                    odds_res, odds_25, reps=1000){
    ##calculate results (win and <>2,5 goals)
    out <- game_rand(data, team_h, team_a, date, history, reps=reps)
    H = 0
    D = 0
    A = 0
    Gl2_5 = 0
    Gm2_5 = 0
    
    for(i in 1:12){
        for(u in 1:12){
            if(i>u){
                H = H + out[i,u]
            }else if(i==u){
                D = D + out[i,u]
            }else if(i<u){
                A = A + out[i,u]
            }
            
            if(i+u >4.5){
                Gl2_5 = Gl2_5 + out[i,u]    
            }else{
                Gm2_5 = Gm2_5 + out[i,u]
            }
                
        }
    }
    
    pred_res = c(1/H, 1/D, 1/A)
    pred_25 = c(1/Gl2_5, 1/Gm2_5)
    print(pred_res)
    print(pred_25)
    
    play_res = c(0,0,0)
    play_goals = c(0,0)
    
    for(i in 1:3){
        if(pred_res[i]<odds_res[i]*0.9){
            play_res[i]=100
        }else if(pred_res[i]<odds_res[i]){
            play_res[i]=25
        }
    }
    
    for(i in 1:2){
        if(pred_25[i]<odds_25[i]*0.9){
            play_goals[i]=100
        }else if(pred_25[i]<odds_25[i]){
            play_goals[i]=25
        }
    }
    
    play_ls <- list()
    play_ls[[1]] <- as.list(play_res)
    play_ls[[2]] <- as.list(play_goals)
    play_ls
    
}