\name{mean.height}
\alias{mean.height}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mean.height(lpi.height.tall, type = "mean", omit.zero = FALSE, by.line = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lpi.height.tall}{
%%     ~~Describe \code{lpi.height.tall} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{omit.zero}{
%%     ~~Describe \code{omit.zero} here~~
}
  \item{by.line}{
%%     ~~Describe \code{by.line} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (lpi.height.tall, type = "mean", omit.zero = FALSE, 
    by.line = FALSE, ...) 
{
    grouping.variables <- rlang::quos(...)
    if (by.line) {
        level <- rlang::quos(PrimaryKey, LineKey)
    }
    else {
        level <- rlang::quos(PrimaryKey)
    }
    if (omit.zero) {
        lpi.height.tall <- lpi.height.tall \%>\% dplyr::filter(Height != 
            0)
    }
    if (type == "mean") {
        summary <- lpi.height.tall \%>\% dplyr::filter(!is.na(Height)) \%>\% 
            dplyr::group_by(!(!(!level)), !(!(!grouping.variables))) \%>\% 
            dplyr::summarize(mean.height = mean(Height))
    }
    if (type == "max") {
        lpi.height.tall.spread <- lpi.height.tall \%>\% tidyr::spread(key = type, 
            value = Height)
        lpi.height.tall.spread$max <- pmax(lpi.height.tall.spread$herbaceous, 
            lpi.height.tall.spread$woody, na.rm = TRUE)
        summary <- lpi.height.tall.spread \%>\% dplyr::group_by(!(!(!level)), 
            !(!(!grouping.variables))) \%>\% dplyr::summarize(max.height = mean(max))
    }
    return(summary)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
