##Calculate the expected probability of outcome in a game between team a and b. 
##Based on "history" number of games from given date
game_calc <- function(data, team_h, team_a, date=format(Sys.time(), "%Y-%m-%d"), history=38){
    
    ##First create dataframe
    teams_str <- as.data.frame(matrix(NA, nrow=2, ncol=3))
    colnames(teams_str) <- c("team o str", "team d str", "calc str")
    rownames(teams_str) <- c(team_h, team_a)
    
    ##Get numeric strength of team_h with the function team_str.R
    source("Team_str.R")
    teams_str[1,c(1,2)] <- team_str(data, team_h, date, history)
            
    ##Get numeric strength of team_a with 'history' number of matches as background.
    teams_str[2,c(1,2)] <- team_str(data, team_a, date, history)
    
        ##To do Create function to calculate homefield advantage
    adv = 1.149
        ##To do Create function to calculate avr. of entire league
    leag_avr = 1.362 
    
    ##calculate balanced steam strength weighed against oppon. Secondly make sure Team 1 is stronger 
    ##than Team 2 or swap
    teams_str[1,3] <- teams_str[1,1] * adv * (teams_str[2,2]/leag_avr) 
    teams_str[2,3] <- teams_str[2,1] * (2-adv) * (teams_str[1,2]/leag_avr)
    if(teams_str[2,3]>teams_str[1,3]){
        temp<-teams_str
        teams_str[1,1:3]<-temp[2,1:3]
        teams_str[2,1:3]<-temp[1,1:3]
        rnam <- row.names(teams_str)
        rownames(teams_str) <- c(rnam[2],rnam[1])
    }
    
        ##To do Create function to find average goals after 80 minutes
    eighty = 0.8236
        ##To do create models for scenarios
    m00 = c(0.082, 1, 1)
    mD = c(0.21, 1, 1)
    m1up = c(0.15, 1.15, 0.85)
    m1d =c(0.2435, 1.1, 0.9)
    m2up =c(0.17, 1.4, 0.6)
    m2d =c(0.205, 1.4, 0.6)
    m3up =c(0.18, 1.7, 0.3)
    m4up =c(0.18, 1.4, 0.6)
    
    
            
    ##Create probability matrix
    prob80 <- matrix(c(1:81), nrow=9, ncol=9)
    probfin <- matrix(c(1:81), nrow=9, ncol=9)



    #function to calc probability matrix of first 80minutes
    probmat <- function(lam1, lam2){
        probm <- matrix(c(1:81), nrow=9, ncol=9)
        for(h in 1:9){
            for(a in 1:9){
                probm[h,a] = dpois(h-1, lam1)*dpois(a-1, lam2)
            }
        }
        probm
    }
    
    #Create lambda for first 80 min
    l1 <- teams_str[1,3]*eighty
    l2 <- teams_str[2,3]*eighty
    
    prob80min<-probmat(l1,l2)
    ## to verify model: print(sum(prob80min[1,1]+prob80min[2,2]+prob80min[3,3]))
    
    ## Scenario loop
    ##0-0
    prob00 <- probmat((teams_str[1,3]*m00[1]*m00[2]), (teams_str[2,3]*m00[1]))
    probfin <- prob80min[1,1] * prob00
    
    ##Draw
    
    for(i in 2:9){
        probD <- probmat((teams_str[1,3]*mD[1]*mD[2]), (teams_str[2,3]*mD[1]*mD[3]))
        probtemp <- (prob80min[i,i] * probD)       
        for(i)
        
        probfin <- proptemp[+ probfin
    }
    
    for(i in 2:9){
        probD <- probmat((teams_str[1,3]*m1up[1]*m1up[2]), (teams_str[2,3]*m1up[1]*m1up[3]))
        probfin <- (prob80min[i,i-1] * probD)+probfin
    }
    
    colnames(probfin) <- c(0:8)
    rownames(probfin) <- c(0:8)
    probfin
}