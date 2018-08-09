## function to perform unit test for writeMSL

test_writeResult <- function() {
    
#     data(Run)
#     data(Targets)
#     runPeaks <- getPeak(Run = Run, Targets = Targets)                                                

    checkException(writeResult(output.file.name = 'results.csv'), 
                   "Please provide the run peaks object!")   
}