## This function output the data processing results

writeResult <- function(runPeaks = list(), output.file.name = 'results.csv') {
    
    if (missing(runPeaks)) {
        stop('Please provide the run peaks object!')
    }
    
    # number of targets and runs
    Lcomp <- length(runPeaks[[1]])
    Lrun <- length(runPeaks)
    
    # initialization
    Compound <- character(); Mass <- character(); Intensity <- character();
    LibRI <- numeric(); LibRT <- numeric()

    # targets information from library
    for (j in 1:Lcomp) {
        # info for each target
        Compound[j] <- runPeaks[[1]][[j]]$compound
        LibRT[j] <- round(runPeaks[[1]][[j]]$rt0, 2)
        LibRI[j] <- round(runPeaks[[1]][[j]]$ri0, 2)
        Mass[j] <- paste(as.character(runPeaks[[1]][[j]]$ms), 
                                 collapse = '   ')
        Intensity[j] <- paste(as.character(runPeaks[[1]][[j]]$sp), 
                                collapse = '   ')
    }
    
    # table of target info
    results <- data.frame(Compound, LibRT, LibRI, Mass, Intensity, 
                          stringsAsFactors = FALSE)
                
    # info for each run
    DF <- 1:Lcomp
    RT <- matrix(, nrow = Lcomp, ncol = Lrun)
    apexScore <- matrix(, nrow = Lcomp, ncol = Lrun)
    areaScore <- matrix(, nrow = Lcomp, ncol = Lrun)
    RI <- numeric(); Apex <- numeric(); Area <- numeric(); 
    #apexScore <- numeric(); areaScore <- numeric();
    
    # target information from analysis results
    for (i in 1:Lrun) {
        for (j in 1:Lcomp) {
            RT[j, i] <- round(runPeaks[[i]][[j]]$rtApex, 2)
            RI[j] <- round(runPeaks[[i]][[j]]$RI, 1)
            Apex[j] <- runPeaks[[i]][[j]]$intApex[1]
            Area[j] <- runPeaks[[i]][[j]]$area[1]
            apexScore[j, i] <- round(runPeaks[[i]][[j]]$scoreApex, 3)
            areaScore[j, i] <- round(runPeaks[[i]][[j]]$scoreArea, 3)
            
        }
        
        df <- data.frame(RT = RT[, i], RI, Apex, Area, 
                         apexScore = apexScore[, i], areaScore = areaScore[, i])
        names(df) <- sapply(names(df), paste, names(runPeaks)[i], sep = ':')
        DF <- cbind(DF, df)
    }
    
    DF <- DF[, -1]
    
    # including average RT
    for (j in 1:Lcomp) {
        results$AvRT[j] <- round(mean(RT[j, ], na.rm = TRUE), 2)
        results$AvApexScore[j] <- round(mean(apexScore[j, ], na.rm = TRUE), 2)
        results$AvAreaScore[j] <- round(mean(areaScore[j, ], na.rm = TRUE), 2)
    }
    
    # integrate the results in one table
    results <- cbind(results, DF)
        
    # write results to a csv file to the provided file name
    write.csv(results, file = output.file.name)
}