\name{gather.soil.stability}
\alias{gather.soil.stability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gather.soil.stability(filepath, gdb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
%%     ~~Describe \code{filepath} here~~
}
  \item{gdb}{
%%     ~~Describe \code{gdb} here~~
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
function (filepath, gdb) 
{
    library(arcgisbinding)
    arcgisbinding::arc.check_product()
    soil.stability.detail <- read.geodatabase(filepath, gdb, 
        feature.name = "tblSoilStabDetail")
    soil.stability.header <- read.geodatabase(filepath, gdb, 
        feature.name = "tblSoilStabHeader")
    soil.stability.detail <- soil.stability.detail[!is.na(soil.stability.detail$PrimaryKey), 
        ]
    gathered <- soil.stability.detail \%>\% dplyr::select(., match = -dplyr::starts_with("In"), 
        -dplyr::starts_with("Dip"), -dplyr::starts_with("DateLoaded"), 
        -OBJECTID) \%>\% tidyr::gather(., key = variable, value = value, 
        -PrimaryKey, -RecKey, -BoxNum)
    gathered <- gathered[!is.na(gathered$value), ]
    gathered$key <- stringr::str_extract(string = gathered$variable, 
        pattern = "^[A-z]+")
    gathered <- subset(gathered, select = -c(variable, BoxNum))
    gathered <- unique(gathered)
    soil.stability.detail.tidy <- lapply(X = as.list(unique(gathered$key)), 
        FUN = function(k = as.list(unique(gathered$key)), df = gathered) {
            df[df$key == k, ] \%>\% mutate(id = 1:n()) \%>\% tidyr::spread(key = key, 
                value = value) \%>\% select(-id)
        }) \%>\% purrr::reduce(merge)
    soil.stability.detail.tidy$Rating <- as.numeric(soil.stability.detail.tidy$Rating)
    soil.stability.tall <- merge(soil.stability.header, soil.stability.detail.tidy, 
        allow.cartesian = TRUE)
    return(soil.stability)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
