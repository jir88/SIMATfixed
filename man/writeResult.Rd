%% writeMSL function %%
\name{writeResult}
\alias{writeResult}

\title{Write analysis results to file.}

\description{This funtion gets the anaylsis results and writes them in a csv format. 
}

\usage{
writeResult(runPeaks = list(), output.file.name = 'results.csv', 
            output.file.path = getwd())
}

\arguments{
    \item{runPeaks}{a list including the analysis results and targets information, e.g. acquired by \code{getPeak} function.}
    
    \item{output.file.name}{a character object, i.e. string, of the name of the output file in csv format.}
    
    \item{output.file.path}{a character object, i.e. string, of the path for saving the output file.}
}

\details{As an input, the \code{runPeaks} object is a list of lists where each list, is the information for one run. The information for each run includes target information, and analysis results, e.g. apex and area location in time and their related intensities as well as estimated RI.
}

\value{A logical value}

\seealso{\code{\link{readCDF}}, \code{\link{getPeak}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{no notes for this function}

\examples{
    # load an RData file including a single run data acquired by readCDF
    data(Run)
    
    # load targets information
    data(Targets)
    
    # get all the corresponding peaks of the target list
    runPeaks <- getPeak(Run = Run, Targets = Targets)
    
    # write analysis results into a csv file
    writeResult(runPeaks = runPeaks)
}