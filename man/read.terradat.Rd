\name{read.terradat}
\alias{read.terradat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.terradat(filepath, gdb, tablelist, all.tables = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
%%     ~~Describe \code{filepath} here~~
}
  \item{gdb}{
%%     ~~Describe \code{gdb} here~~
}
  \item{tablelist}{
%%     ~~Describe \code{tablelist} here~~
}
  \item{all.tables}{
%%     ~~Describe \code{all.tables} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filepath, gdb, tablelist, all.tables = FALSE) 
{
    if (all.tables) {
        tablelist <- c("tblApplicationConstants", "tblEcolSites", 
            "tblESDDominantPerennialHeight", "tblGapDetail", 
            "tblGapHeader", "tblLines", "tblLPIDetail", "tblLPIHeader", 
            "tblPeople", "tblPhotos", "tblPlantDenDetail", "tblPlantDenHeader", 
            "tblPlantDenQuads", "tblPlantDenSpecies", "tblPlotCustomLookup1", 
            "tblPlotCustomLookup2", "tblPlotCustomLookup3", "tblPlotFormDefaults", 
            "tblPlotHistory", "tblPlotNotes", "tblQualDetail", 
            "tblQualHeader", "tblSageEval", "tblSageLek", "tblSageRange", 
            "tblSites", "tblSoilPitHorizons", "tblSoilPits", 
            "tblSoilStabDetail", "tblSoilStabHeader", "tblSoilSurface", 
            "tblSpecies", "tblSpeciesGrowthHabit", "tblSpecRichAbundance", 
            "tblSpecRichDetail", "tblSpecRichHeader")
    }
    terradat <- lapply(X = tablelist, FUN = function(filepath, 
        gdb, X) {
        return(test <- data.table::as.data.table(arcgisbinding::arc.select(arcgisbinding::arc.open(paste(filepath, 
            gdb, X, sep = "/")))))
    }, filepath = filepath, gdb = gdb) \%>\% setNames(tablelist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
