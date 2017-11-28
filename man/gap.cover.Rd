\name{gap.cover}
\alias{gap.cover}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gap.cover(gap.tall, tall = FALSE, breaks = c(20, 25, 50, 100, 200), type = "canopy")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gap.tall}{
%%     ~~Describe \code{gap.tall} here~~
}
  \item{tall}{
%%     ~~Describe \code{tall} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (gap.tall, tall = FALSE, breaks = c(20, 25, 50, 100, 
    200), type = "canopy") 
{
    gap.tall$LineLengthAmount[gap.tall$Measure == 1] <- 100 * 
        gap.tall$LineLengthAmount[gap.tall$Measure == 1]
    if (unique(gap.tall$Measure) \%in\% 2) {
        gap.tall$LineLengthAmount[gap.tall$Measure == 2] <- gap.tall$LineLengthAmount[gap.tall$Measure == 
            2] * 2.54
        gap.tall$Gap[gap.tall$Measure == 2] <- gap.tall$Gap[gap.tall$Measure == 
            2] * 2.54
        gap.tall$GapMin[gap.tall$Measure == 2] <- gap.tall$MinGap[gap.tall$Measure == 
            2] * 2.54
    }
    if (type == "canopy") {
        gap.tall <- subset(gap.tall, RecType != "B")
    }
    if (type == "basal") {
        gap.tall <- subset(gap.tall, RecType != "C")
    }
    gap.tall <- gap.tall \%>\% dplyr::distinct(PrimaryKey, LineKey, 
        .keep_all = TRUE) \%>\% dplyr::group_by(PrimaryKey) \%>\% 
        unique() \%>\% dplyr::summarize(total.line.length = sum(LineLengthAmount)) \%>\% 
        merge(gap.tall, ., allow.cartesian = TRUE)
    gap.tall$interval <- cut(gap.tall$Gap, breaks = unique(c(breaks, 
        gap.tall$LineLengthAmount)), right = FALSE)
    gap.summary <- gap.tall \%>\% dplyr::filter(!is.na(interval)) \%>\% 
        dplyr::group_by(PrimaryKey, total.line.length, interval) \%>\% 
        dplyr::summarize(n = length(Gap), length = sum(Gap)) \%>\% 
        dplyr::mutate(., percent = 100 * (length/total.line.length))
    percent <- gap.summary \%>\% dplyr::select(., -n, -length) \%>\% 
        tidyr::spread(key = interval, value = percent, fill = 0)
    n <- gap.summary \%>\% dplyr::select(., -percent, -length) \%>\% 
        tidyr::spread(key = interval, value = n, fill = 0)
    length <- gap.summary \%>\% dplyr::select(., -n, -percent) \%>\% 
        tidyr::spread(key = interval, value = length, fill = 0)
    if (!tall) {
        gap.summary <- list(percent = percent, n = n, length = length)
    }
    else {
        gap.summary <- percent \%>\% tidyr::gather(key = Gap.Class, 
            value = percent, -PrimaryKey, -total.line.length)
        gap.summary <- n \%>\% tidyr::gather(key = Gap.Class, value = n, 
            -PrimaryKey, -total.line.length) \%>\% merge(gap.summary, 
            allow.cartesian = TRUE)
        gap.summary <- length \%>\% tidyr::gather(key = Gap.Class, 
            value = length, -PrimaryKey, -total.line.length) \%>\% 
            merge(gap.summary, allow.cartesian = TRUE)
    }
    return(gap.summary)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
