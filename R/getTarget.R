## This function reads the targets and gets their info

getTarget <- function(Method = "target", target.file = character(),  
                      library.file = character(), path = getwd(),
                      library.path = getwd(), Library = list(), 
                      target.table = list(), deltaRI = numeric(), 
                      deltaRT = numeric(), Save = FALSE) {
    
    
    ## method = target
    if (grepl("target", Method, ignore.case = TRUE)) {
        
        if (missing(target.file)) {
            stop("When Method = \"target\", a target file should be provided")
        }
        
        Targets <- readMSL(file.name = target.file, path = path)
        
        L <- length(Targets$compound)
        quantFrag <- rep(1, L)
        Targets$quantFrag <- quantFrag
    }
    
    ## method = library
    else if (grepl("library", Method, ignore.case = TRUE)) {
        
        if ((missing(library.file) & missing(Library)) | missing(target.table)) {
            stop(paste("When Method = \"library\", a library file and a", 
                       "target.table should be provided!"))
        }
        #  optimization
        else {
            # check if library file is provided
            if (!missing(library.file)) {
                Library <- readMSL(file.name = library.file, 
                                    path = library.path, Save = FALSE)
            }
            # get the targets
            Targets <- optFrag(Library = Library, target.table = target.table, 
                                        deltaRI = deltaRI, deltaRT = deltaRT)
        }
    }
        
    ## method = combined
    else if (grepl("combin", Method, ignore.case = TRUE)) {
        
        if (missing(target.file) | (missing(library.file) & missing(Library))) {
            stop(paste("When Method = \"combined\", a target file and a library" 
                       , "file should be provided!"))
        }
        #  optimization
        else {
        # read the library and the targets
            # check if library file is provided
            if (!missing(target.file)) {
                Library <- readMSL(file.name = library.file, 
                                   path = library.path, Save = FALSE)
            }
            
            # get the targets
            Targets <- readMSL(file.name = target.file, path = path)
            
            # optimize
            Targets <- optFrag(Targets = Targets, Library = Library,
                               deltaRI = deltaRI, deltaRT = deltaRT)
        }
    }    
    
    ##
    else {
        stop(paste("Unknown Method", Method, sep = " "))
    }
    
    
    ## saving and returning the result
    if (Save & length(target.file)) {
        file.name <- paste(target.file, ".rds", sep = "")
        saveRDS(Targets, file = file.name, compress = "xz")
    } 
    
    return(Targets)     
}