## This is the function to calculate the RI of an RT

# this function returns a function which can be used to calculate the RI
# before that, getRI should be called with the RItable
# RItable is the table of RTs and correponding RIs of RI standards measured
# using an RI run
# 
# example:
# calcRI <- getRI(RItable)

getRI <- function(RItable = data.frame()) {
    
    calibRI <- function(rt = 0, ri = 0){
        
        RI <- RItable$ri
        RT <- RItable$rt
        
        L <- length(RT)
        
        if (!missing(rt)) {
            if (rt < 0) {
                return(0)            
            }
            
            ind <- which(RT == rt)
            if (length(ind)) {
                return(RI[ind])   
            }
            
            if (rt < RT[1]) {
                lb <- 1
                ub <- 2
            } else if (rt > RT[L]) {
                lb <- L-1
                ub <- L
            } else {
                lb <- max(which(rt >= RT))
                ub <- min(which(rt <= RT))
            }
            
            ri <- (RI[ub] - RI[lb]) / (RT[ub] - RT[lb]) * (rt - RT[lb]) + RI[lb]
            
            ri <- max(ri, 0)
            
            return(ri)
        }
        else {
            if (ri < 0) {
                return(0)            
            }
            
            ind <- which(RI == ri)
            if (length(ind)) {
                return(RT[ind])   
            }
            
            if (ri < RI[1]) {
                lb <- 1
                ub <- 2
            } else if (ri > RI[L]) {
                lb <- L-1
                ub <- L
            } else {
                lb <- max(which(ri >= RI))
                ub <- min(which(ri <= RI))
            }
            
            rt <- (RT[ub] - RT[lb]) / (RI[ub] - RI[lb]) * (ri - RI[lb]) + RT[lb]
            
            rt <- max(rt, 0)
            
            return(rt)            
        }
    }
}