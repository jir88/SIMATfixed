## This function retrieves the EIC of one peak in one run

getEIC <- function(Run = list(), compound = "Analyte", ms0 = numeric(),
                   sp0 = numeric(), rt0 = numeric(), drt = 10/60, dsc = 10/2,
                   ri0 = 0, weight = 2/3, deltaRI = 20, calibRI = NULL,
                   smooth_span = 0.05) {

  ## check if minimum required inputs are present
  if (missing(Run) | missing(ms0) | missing(sp0) | missing(rt0)) {
      stop("One of the required inputs is missing!")
  }

  ## function to find the peak locations
  # x: the EIC intensities
  # rt_idx: which index is closest to the expected RT?
  # Threshold: minimum peak slope between the first and second scans AFTER
  #   the peak apex
  # delta: 
  findPeaks <- function (x, rt_idx, Threshold = 0, delta = 3) {

        # span <- 0.05
        t <- 1:length(x)
        # smooth the EIC a little bit
        x_smooth <- suppressWarnings(loess(x ~ t, span = smooth_span)$fitted)
        if(any(is.nan(x_smooth))) {
          stop("EIC smoothing generated NaNs! Span is too small.")
        }

        # find indices where slope goes from positive to negative (peaks)
        pks <- which(diff(sign(diff(x_smooth,na.pad = FALSE)),na.pad = FALSE) < 0) + 2

        # if a threshold slope has been supplied
        if (!missing(Threshold)) {
          # check slope between 1st and 2nd scans AFTER the apex
            pks<- pks[x_smooth[pks - 1] - x_smooth[pks] > Threshold]
        }
        # if we didn't find any peaks
        if(length(pks) == 0) {
          # return empty vector
          return(integer())
        }


        for (i in 1:length(pks)) {
            pks[i] <- pks[i] - (delta + 1) +
            which.max(x_smooth[max(1, pks[i] - delta):min(length(x_smooth), pks[i] + delta)])
        }

        return(pks)
    }


    ## set initial values for some parameters
    # set dms as the tolerance of mass (in Dalton)
    dms <- 0.4
    # number of initially detected peaks and the top ones in the EIC
    topPeaks <- 20
    # relative intensity of the peak to the highest intensity peak for selection
    # a peak should be at least 1/RelativeInt of the highest peak to be selected
    relativeInt <- 1000

    ## get run info, i.e. scans (mass, intensity pairs) and retention times
    scans <- Run$pk
    rt <-Run$rt

    ## scan index and rt values for search window around the peak
    # expected RT based on RI if provided
    if (is.null(calibRI)) {
        expRT <- rt0
    } else {
        expRT <- calibRI(ri = ri0)
    }

    # pull out the scans within the retention time window of the expected RT
    sc <- which(rt/60 < expRT + drt & rt/60 > expRT - drt)
    RT <- rt[sc]
    # extract scans and their mass and intensities from the run
    p <- scans[sc]

    # if no scans are within the RT window, return a blank EIC
    if(length(sc) == 0) {
      warning(paste0("\"", compound, "\" expected retention time ", expRT,
                     " min. outside retention time range in file: ", Run$file.name, "\n"))
      ## create the output object as a data frame
      peakEIC <- list(rtApex = NA_real_, intApex = NA_real_, RI = NA_real_,
                      scoreApex = 0, scoreArea = 0)
      peakEIC$area <- 0
      peakEIC$EIC <- matrix(0, nrow = length(ms0), ncol = length(sc))
      peakEIC$RT <- RT
      peakEIC$ms <- ms0
      peakEIC$sp <- sp0
      peakEIC$rt0 <- rt0
      peakEIC$ri0 <- ri0
      peakEIC$compound <- compound
      
      # return the output object
      return(peakEIC)
    }

    ## peak detection
    # initial values
    indMS <- numeric(); EIC <- matrix(0, nrow = length(ms0), ncol = length(sc));
    apex <- numeric(); baseLine <- numeric(); area <- rep(0, length(ms0));
    intApex <- rep(0, length(ms0))

    # find the scan related to the quantifier fragment mass
    for (s in 1:length(sc)) {
        indMS <- which(abs(p[[s]][,"mz"] - ms0[1]) <= dms)
        ind <- which.min(abs(p[[s]][indMS, "mz"] - ms0[1]))

        if (length(ind)) {
            EIC[1, s] <- p[[s]][indMS[ind], "intensity"]
        } else {
            EIC[1, s] <- 0
        }
    }

    # if the quantifier mass was never scanned, return a blank EIC
    if(diff(range(EIC)) == 0) {
      warning(paste0("\"", compound, "\" quantifier fragment ", ms0[1],
                     " m/z not found in file: ", Run$file.name, "\n"))
      ## create the output object as a data frame
      peakEIC <- list(rtApex = NA_real_, intApex = NA_real_, RI = NA_real_,
                      scoreApex = 0, scoreArea = 0)
      peakEIC$area <- 0
      peakEIC$EIC <- EIC
      peakEIC$RT <- RT
      peakEIC$ms <- ms0
      peakEIC$sp <- sp0
      peakEIC$rt0 <- rt0
      peakEIC$ri0 <- ri0
      peakEIC$compound <- compound

      # return the output object
      return(peakEIC)
    }

    # locate the top candidate peak

    scPeaks <- findPeaks(EIC[1, ], #Threshold = min(EIC[1, ])/20,
                         rt_idx = which.min(abs(RT - rt0*60)))

    # if no peak is found, return area of zero
    if(length(scPeaks) == 0) {
      ## create the output object as a data frame
      peakEIC <- list(rtApex = NA_real_, intApex = NA_real_, RI = NA_real_,
                      scoreApex = 0, scoreArea = 0)
      peakEIC$area <- 0
      peakEIC$EIC <- EIC
      peakEIC$RT <- RT
      peakEIC$ms <- ms0
      peakEIC$sp <- sp0
      peakEIC$rt0 <- rt0
      peakEIC$ri0 <- ri0
      peakEIC$compound <- compound

      # return the output object
      return(peakEIC)
    }
    scPeaks <- scPeaks[scPeaks > 0]
    indApex <- scPeaks[sort.int(EIC[1, scPeaks], decreasing = TRUE,
                                                    index.return = TRUE)$ix]
    topPeaks <- min(topPeaks, length(scPeaks))
    apex <- EIC[1, indApex[1:topPeaks]]
    locs <- indApex[1:topPeaks]

    # picking the best candidate
    nPeaks <- length(locs)
    score <- 0; scoreApex <- 0; scoreArea <- 0; tempApex <- 0; tempArea <- 0
    tempEIC <- numeric(); rtApex <- rt0

    for (i in 1:nPeaks) {
        indMin <- max(locs[i] - dsc, 1)
        indMax <- min(locs[i] + dsc, length(sc))
        baseLine[1] <- min(EIC[1, ])
        area[1] <- sum(EIC[1, indMin:indMax] - baseLine[1])

        if (length(locs)) {
            intApex[1] <- apex[i]
        }

        # get qualifier fragments data
        if (length(ms0) > 1) {
            for (fr in 2:length(ms0)) {
                for (s in 1:length(sc)) {
                    indMS <- which(abs(p[[s]][,"mz"] - ms0[fr]) <= dms)
                    ind <- which.min(abs(p[[s]][indMS, "mz"] - ms0[fr]))

                    if (length(ind)) {
                        EIC[fr, s] <- p[[s]][indMS[ind], "intensity"]
                    } else {
                        EIC[fr, s] <- 0
                    }
                }

                baseLine[fr] <- min(EIC[fr, ])
                area[fr] <- sum(EIC[fr, indMin:indMax] - baseLine[fr])

                if (length(locs)) {
                    intApex[fr] <- EIC[fr, locs[i]]
                }
            }
        }

        # check similarity score of the candidates and the relative intensities
        # no RI calibration
        if (is.null(calibRI)) {
            ri <- 0

            tempScoreApex <- getScore(trueSpec = intApex, refSpec = sp0)
            tempScoreArea <- getScore(trueSpec = area, refSpec = sp0)
        }
        # calibration, calcute RI
        else {
            ri <- calibRI(rt = RT[locs[i]]/60)

            tempScoreApex <- getScore(trueSpec = intApex, refSpec = sp0,
                                    trueRI = ri, refRI = ri0, deltaRI = deltaRI)
            tempScoreArea <- getScore(trueSpec = area, refSpec = sp0,
                                    trueRI = ri, refRI = ri0, deltaRI = deltaRI)
        }

        # combined score
        tempScore <- weight * tempScoreApex + (1 - weight) * tempScoreArea

        # pick the best candidate
        if ( (tempScore >= score) & (intApex[1] > tempApex[1]/relativeInt) ) {
            score <- tempScore
            scoreApex <- tempScoreApex
            scoreArea <- tempScoreArea
            tempArea <- area
            tempApex <- intApex
            rtApex <- RT[locs[i]]
            RI <- ri
            tempEIC <- EIC
        }
    }

    ## get the final results
    area <- tempArea
    intApex <- tempApex
    EIC <- tempEIC

    ## create the output object as a data frame
    peakEIC <- list(rtApex = rtApex/60, intApex = intApex, RI = RI,
                          scoreApex = scoreApex, scoreArea = scoreArea)
    peakEIC$area <- area
    peakEIC$EIC <- EIC
    peakEIC$RT <- RT
    peakEIC$ms <- ms0
    peakEIC$sp <- sp0
    peakEIC$rt0 <- rt0
    peakEIC$ri0 <- ri0
    peakEIC$compound <- compound

    # return the output object
    return(peakEIC)
}
