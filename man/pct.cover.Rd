\name{pct.cover}
\alias{pct.cover}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pct.cover(lpi.tall, tall = FALSE, hit = "any", by.year = FALSE, by.line = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lpi.tall}{
%%     ~~Describe \code{lpi.tall} here~~
}
  \item{tall}{
%%     ~~Describe \code{tall} here~~
}
  \item{hit}{
%%     ~~Describe \code{hit} here~~
}
  \item{by.year}{
%%     ~~Describe \code{by.year} here~~
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
function (lpi.tall, tall = FALSE, hit = "any", by.year = FALSE, 
    by.line = FALSE, ...) 
{
    grouping.variables <- rlang::quos(...)
    if (by.line) {
        level <- rlang::quos(PrimaryKey, LineKey)
    }
    else {
        level <- rlang::quos(PrimaryKey)
    }
    lpi.tall <- dplyr::filter(.data = lpi.tall, !is.na(code), 
        code != "", code != "None", !is.na(PrimaryKey), !is.na(LineKey), 
        !is.na(RecKey))
    lpi.tall$code <- toupper(lpi.tall$code)
    point.totals <- lpi.tall \%>\% dplyr::group_by(!(!(!level))) \%>\% 
        dplyr::summarize(point.count = sum(tail(sort(PointNbr), 
            length(unique(PrimaryKey, LineKey)))))
    lpi.tall <- merge(x = lpi.tall, y = point.totals, all.x = TRUE, 
        allow.cartesian = TRUE)
    lpi.tall <- dplyr::mutate(.data = lpi.tall, layer = factor(layer, 
        levels = c("TopCanopy", unique(lpi.tall$layer)[grepl(unique(lpi.tall$layer), 
            pattern = "^Lower[1-7]")], "SoilSurface"))) \%>\% dplyr::arrange(layer)
    if (hit == "basal") {
        hit <- "any"
        lpi.tall <- dplyr::filter(.data = lpi.tall, layer == 
            "SoilSurface")
    }
    summary <- switch(hit, any = {
        summary <- lpi.tall \%>\% dplyr::group_by(PrimaryKey, LineKey, 
            PointNbr, point.count, !(!(!grouping.variables))) \%>\% 
            dplyr::summarize(present = if (any(!is.na(code) & 
                code != "")) {
                1
            } else {
                0
            }) \%>\% tidyr::unite(indicator, !(!(!grouping.variables)), 
            sep = ".") \%>\% dplyr::ungroup() \%>\% dplyr::group_by(!(!(!level)), 
            indicator) \%>\% dplyr::summarize(percent = 100 * sum(present, 
            na.rm = TRUE)/first(point.count)) \%>\% dplyr::filter(!grepl(indicator, 
            pattern = "^[NA.]{0,100}NA$"))
    }, first = {
        summary <- lpi.tall \%>\% dplyr::filter(!(code \%in\% c("", 
            NA, "None"))) \%>\% dplyr::group_by(PrimaryKey, LineKey, 
            PointNbr, point.count) \%>\% dplyr::summarize(code = first(code)) \%>\% 
            merge(x = dplyr::distinct(dplyr::select(lpi.tall, 
                PrimaryKey, LineKey, PointNbr, code, !(!(!grouping.variables)))), 
                y = ., all.y = TRUE) \%>\% tidyr::unite(indicator, 
            !(!(!grouping.variables)), sep = ".") \%>\% dplyr::ungroup() \%>\% 
            dplyr::group_by(!(!(!level)), indicator) \%>\% dplyr::summarize(percent = 100 * 
            n()/first(point.count)) \%>\% dplyr::filter(!grepl(indicator, 
            pattern = "^[NA.]{0,100}NA$"))
    })
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
