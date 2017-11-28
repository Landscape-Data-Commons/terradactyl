\name{pct.cover.litter}
\alias{pct.cover.litter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pct.cover.litter(lpi.tall, tall = FALSE, by.year = FALSE, by.line = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lpi.tall}{
%%     ~~Describe \code{lpi.tall} here~~
}
  \item{tall}{
%%     ~~Describe \code{tall} here~~
}
  \item{by.year}{
%%     ~~Describe \code{by.year} here~~
}
  \item{by.line}{
%%     ~~Describe \code{by.line} here~~
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
function (lpi.tall, tall = FALSE, by.year = FALSE, by.line = FALSE) 
{
    summary <- pct.cover(lpi.tall, tall = TRUE, hit = "any", 
        by.year = by.year, by.line = by.line, code) \%>\% subset(., 
        indicator \%in\% c("L", "WL", "NL"))
    if (!tall) {
        summary <- tidyr::spread(summary, key = indicator, value = percent) \%>\% 
            tidyr::replace_na(replace = setNames(as.list(rep.int(0, 
                times = length(unique(names(.)[!(names(.) \%in\% 
                  c("PrimaryKey", "PlotKey", "PlotID", "LineKey", 
                    "LineID"))])))), unique(names(.)[!(names(.) \%in\% 
                c("PrimaryKey", "LineKey"))])))
    }
    return(summary)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
