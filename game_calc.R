game_calc <- function(data, team_h, team_a, date=Sys.Date, history=38){
    
    ##Get numeric strength of team_h with the function team_str.R
        ##First create dataframe
    teams_str <- as.data.frame(matrix(NA, nrow=2, ncol=2))
    colnames(teams_str) <- c("team o str", "team d str")
    rownames(teams_str) <- c(team_h, team_a)
    
    source("Team_str.R")
    teams_str[1,] <- team_str(data, team_h, date, history)
            
    ##Get numeric strength of team_a with 'history' number of matches as background.
    teams_str[2,] <- team_str(data, team_a, date, history)

    teams_str
    
}