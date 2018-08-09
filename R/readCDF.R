## This function reads CDF files

readCDF <- function(path = getwd()) {
    
    file.name <- list.files(path = path, pattern = ".CDF", full.names = FALSE)
    
    if (!length(file.name)) {
        stop("No CDF files found!")
    }
        
    for (i in 1:length(file.name)) {
        
        CDFdata <- openMSfile(file.path(path, file.name[i]), backend = "netCDF") 
        pk <- peaks(CDFdata)
        hd <- header(CDFdata)
        rt <- hd$retentionTime
        sc <- hd$seqNum
        tic <- hd$totIonCurrent
        
        Run <- list(rt = rt, sc = sc, tic = tic)
        Run$pk <- pk
        Run$file.name <- file.name[i]
        
        saveRDS(Run, file = paste(file.name[i], ".rds", sep = ""))
        #save(Run, file=paste(file.name[i], ".rda", sep=""), compress="xz")
    }
    
    # return the file names, can be used with getPeak()
    return(file.name)
}