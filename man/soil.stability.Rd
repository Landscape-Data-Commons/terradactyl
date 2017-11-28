\name{soil.stability}
\alias{soil.stability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
soil.stability(soil.stability.tall, all = TRUE, cover = TRUE, uncovered = TRUE, all.cover.type = TRUE, tall = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{soil.stability.tall}{
%%     ~~Describe \code{soil.stability.tall} here~~
}
  \item{all}{
%%     ~~Describe \code{all} here~~
}
  \item{cover}{
%%     ~~Describe \code{cover} here~~
}
  \item{uncovered}{
%%     ~~Describe \code{uncovered} here~~
}
  \item{all.cover.type}{
%%     ~~Describe \code{all.cover.type} here~~
}
  \item{tall}{
%%     ~~Describe \code{tall} here~~
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
function (soil.stability.tall, all = TRUE, cover = TRUE, uncovered = TRUE, 
    all.cover.type = TRUE, tall = FALSE) 
{
    soil.stability.rating <- list()
    if (all == TRUE) {
        soil.stability.rating[["all"]] <- soil.stability.tall \%>\% 
            dplyr::group_by(PrimaryKey) \%>\% dplyr::summarize(rating = mean(Rating)) \%>\% 
            dplyr::mutate(Veg = "all") \%>\% as.data.frame()
    }
    if (cover == TRUE) {
        soil.stability.rating[["covered"]] <- soil.stability.tall \%>\% 
            subset(Veg != "NC") \%>\% dplyr::group_by(PrimaryKey) \%>\% 
            dplyr::summarize(rating = mean(Rating)) \%>\% dplyr::mutate(Veg = "covered") \%>\% 
            as.data.frame()
    }
    if (uncovered == TRUE) {
        soil.stability.rating[["unconvered"]] <- soil.stability.tall \%>\% 
            subset(Veg == "NC") \%>\% dplyr::group_by(PrimaryKey) \%>\% 
            dplyr::summarize(rating = mean(Rating)) \%>\% dplyr::mutate(Veg = "uncovered") \%>\% 
            as.data.frame()
    }
    if (all.cover.type == TRUE) {
        soil.stability.rating[["all.cover.types"]] <- soil.stability.tall \%>\% 
            dplyr::group_by(PrimaryKey, Veg) \%>\% dplyr::summarize(rating = mean(Rating)) \%>\% 
            as.data.frame()
    }
    soil.stability.rating.all <- do.call("rbind", soil.stability.rating)
    if (!tall) {
        soil.stability.rating.all <- soil.stability.rating.all \%>\% 
            spread(key = Veg, value = rating)
    }
    return(soil.stability.rating.all)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
