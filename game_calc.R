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
    
    teams_str[1,3] <- teams_str[1,1] * adv * (teams_str[2,2]/leag_avr) 
    teams_str[2,3] <- teams_str[2,1] * (2-adv) * (teams_str[1,2]/leag_avr)
    
        ##To do Create function to find average goals after 80 minutes
    eighty = 0.8236
        
    ##Create probability matrix
    probm <- matrix(c(1:81), nrow=9, ncol=9)
    
    for(h in 1:9){
        for(a in 1:9){
            l1 <- teams_str[1,3]*eighty
            l2 <- teams_str[2,3]*eighty
            probm[h,a] = dpois(h-1, l1)*dpois(a-1, l2)
        }
    }
    probm
    
}