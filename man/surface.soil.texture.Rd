\name{surface.soil.texture}
\alias{surface.soil.texture}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
surface.soil.texture(filepath, gdb, soil.texture.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
%%     ~~Describe \code{filepath} here~~
}
  \item{gdb}{
%%     ~~Describe \code{gdb} here~~
}
  \item{soil.texture.file}{
%%     ~~Describe \code{soil.texture.file} here~~
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
function (filepath, gdb, soil.texture.file) 
{
    library(arcgisbinding)
    arcgisbinding::arc.check_product()
    texture.lut <- read.csv(soil.texture.file, fileEncoding = "UTF-8-BOM")
    soil.pit.horizons <- read.geodatabase(filepath = filepath, 
        gdb = gdb, feature.name = "tblSoilPitHorizons")
    soil.pit.horizons.unique <- unique(soil.pit.horizons[, -c("OBJECTID", 
        "DateLoadedInDb")])
    soil.surface.texture <- soil.pit.horizons.unique[soil.pit.horizons.unique$HorizonDepthUpper == 
        0, c("PrimaryKey", "Texture")]
    soil.surface.texture <- merge(soil.surface.texture, texture.lut, 
        by.x = "Texture", by.y = "abbreviation")
    soil.surface.texture <- soil.surface.texture[, -"Texture"] \%>\% 
        unique()
    return(soil.surface.texture)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
