## This is the function to calculate the similarity score for all peaks

getPeakScore <- function(runPeaks = list(), deltaRI = 20, weight = 2/3, 
                         plot = FALSE) {
    
    ## check if a single run is provided
    if (missing(runPeaks)) {
        stop('At list information from one single run should be provided!')
    }
    
    ## calculate the score
    # initialize
    num.run <- length(runPeaks)
    num.compound <- length(runPeaks[[1]])
    
    Scores <- matrix(, nrow = num.compound, ncol = num.run)

    # get the scores for each target per run
    for (i in 1:num.run) {        
    
        for (j in 1:num.compound) {
            
            spApex <- runPeaks[[i]][[j]]$intApex
            spArea <- runPeaks[[i]][[j]]$area
            sp <- runPeaks[[i]][[j]]$sp
            ri <- runPeaks[[i]][[j]]$ri
            ri0 <- runPeaks[[i]][[j]]$ri0
                        
            ScoreApex <- getScore(trueSpec = spApex, refSpec = sp,
                            trueRI = ri, refRI = ri0, deltaRI = deltaRI)
            ScoreArea <- getScore(trueSpec = spArea, refSpec = sp,
                            trueRI = ri, refRI = ri0, deltaRI = deltaRI)
        
            Scores[j, i] <- weight * ScoreApex + (1-weight) * ScoreArea
        }        
    }    


    ## plot histogram of the scores
    if (plot) {
        scorePlot <- ggplot(melt(Scores, value.name = "Scores"), 
                            aes(x = Scores)) + geom_histogram(binwidth = 0.02, 
                            color = "darkblue", fill = "blue") + 
                            scale_x_continuous(limits=c(0,1))
    
        print(scorePlot)
    }
    
    # return the output, e.g. the scores
    return(Scores)    
}