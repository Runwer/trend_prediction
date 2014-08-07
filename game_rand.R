##Calculate the expected probability of outcome in a game between team a and b. 
##Based on "history" number of games from given date
game_rand <- function(data, team_h, team_a, date=format(Sys.time(), "%Y-%m-%d"), history=38, reps=100){
    
    ##Create final dataframe
    probfin <- matrix(c(0), nrow=12, ncol=12)
    colnames(probfin) <- c(0:11)
    rownames(probfin) <- c(0:11)
    

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
    m3d =c(0.18, 1.4, 0.6)
    
    for(i in 1:reps){
        #Create lambda for first 80 min
        l1 <- teams_str[1,3]*eighty
        l2 <- teams_str[2,3]*eighty

    
        ##random 80 minut result
        
        H = rpois(1, l1)
        A = rpois(1, l2)

        if(H==A & H==0){
            l1 = (teams_str[1,3]*m00[1]*m00[2])
            l2 = (teams_str[2,3]*m00[1]*m00[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else if(H==A & H!=0){
            l1 = (teams_str[1,3]*mD[1]*mD[2])
            l2 = (teams_str[2,3]*mD[1]*mD[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else if(H==A+1){
            l1 = (teams_str[1,3]*m1up[1]*m1up[2])
            l2 = (teams_str[2,3]*m1up[1]*m1up[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else if(H==A+2){
            l1 = (teams_str[1,3]*m2up[1]*m2up[2])
            l2 = (teams_str[2,3]*m2up[1]*m2up[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else if(H>A+2){
            l1 = (teams_str[1,3]*m3up[1]*m3up[2])
            l2 = (teams_str[2,3]*m3up[1]*m3up[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else if(H==A-1){
            l1 = (teams_str[1,3]*m1d[1]*m1d[2])
            l2 = (teams_str[2,3]*m1d[1]*m1d[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else if(H==A-2){
            l1 = (teams_str[1,3]*m2d[1]*m2d[2])
            l2 = (teams_str[2,3]*m2d[1]*m2d[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else if(H<A-2){
            l1 = (teams_str[1,3]*m3d[1]*m3d[2])
            l2 = (teams_str[2,3]*m3d[1]*m3d[3])
            H = rpois(1, l1)+H
            A = rpois(1, l2)+A
        }else{
            print("error!!!!")
        }
        probfin[H+1,A+1] <- probfin[H+1,A+1]+1
        
    }

    print(probfin)
}