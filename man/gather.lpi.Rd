\name{gather.lpi}
\alias{gather.lpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gather.lpi(filepath, gdb, species.characteristics = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
%%     ~~Describe \code{filepath} here~~
}
  \item{gdb}{
%%     ~~Describe \code{gdb} here~~
}
  \item{species.characteristics}{
%%     ~~Describe \code{species.characteristics} here~~
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
function (filepath, gdb, species.characteristics = TRUE) 
{
    library(arcgisbinding)
    arcgisbinding::arc.check_product()
    lpi.detail <- read.geodatabase(filepath, gdb, feature.name = "tblLPIDetail")
    lpi.header <- read.geodatabase(filepath, gdb, feature.name = "tblLPIHeader")
    lpi.hits.tall <- data.table::melt(data = lpi.detail, id.vars = c("PrimaryKey", 
        "PointLoc", "PointNbr", "RecKey"), measure.vars = c("TopCanopy", 
        "SoilSurface", colnames(lpi.detail)[grepl(pattern = "^Lower[1-7]$", 
            x = colnames(lpi.detail))]), variable.name = "layer", 
        value.name = "code", na.rm = TRUE)
    lpi.chkbox.tall <- data.table::melt(data = lpi.detail, id.vars = c("PrimaryKey", 
        "PointLoc", "PointNbr", "RecKey"), measure.vars = colnames(lpi.detail)[grepl(pattern = "^Chkbox", 
        x = colnames(lpi.detail))], variable.name = "layer", 
        value.name = "chckbox")
    lpi.chkbox.tall <- lpi.chkbox.tall[!(lpi.chkbox.tall$chckbox \%in\% 
        c("ChckboxWoody", "ChckboxHerbaceous")), ]
    lpi.chkbox.tall$layer <- stringr::str_replace_all(string = lpi.chkbox.tall$layer, 
        pattern = "^Chkbox", replacement = "")
    lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Top"] <- "TopCanopy"
    lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Soil"] <- "SoilSurface"
    lpi.tall <- merge(x = lpi.header, y = merge(x = lpi.hits.tall, 
        y = lpi.chkbox.tall, all.x = TRUE, by = c("PrimaryKey", 
            "PointLoc", "PointNbr", "RecKey", "layer"), allow.cartesian = TRUE), 
        by = c("PrimaryKey", "RecKey"), all = TRUE, allow.cartesian = TRUE)
    lpi.tall <- lpi.tall[!is.na(lpi.tall$PrimaryKey), ]
    if (species.characteristics) {
        species <- read.geodatabase(filepath = filepath, gdb = gdb, 
            feature.name = "tblSpecies")
        growth.habit <- read.geodatabase(filepath = filepath, 
            gdb = gdb, feature.name = "tblSpeciesGrowthHabit")
        species <- merge(x = species, y = growth.habit, by.x = "GrowthHabitCode", 
            by.y = "Code", all.x = TRUE, allow.cartesian = TRUE)
        lpi.tall <- merge(x = lpi.tall, y = species, by.x = "code", 
            by.y = "SpeciesCode", all.x = TRUE, allow.cartesian = TRUE)
    }
    return(lpi.tall)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
