\name{gather.height}
\alias{gather.height}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gather.height(filepath, gdb, species.characteristics = TRUE)
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
    lpi.detail <- read.geodatabase(filepath, gdb, feature.name = "tblLPIDetail")
    lpi.header <- read.geodatabase(filepath, gdb, feature.name = "tblLPIHeader")
    lpi.header <- subset(lpi.header, select = c(PrimaryKey, LineKey:CheckboxLabel))
    lpi.height.tall.woody <- dplyr::select(.data = lpi.detail, 
        PrimaryKey, PointLoc, PointNbr, RecKey, dplyr::matches("Woody$")) \%>\% 
        dplyr::mutate(type = "woody")
    names(lpi.height.tall.woody) <- stringr::str_replace_all(string = names(lpi.height.tall.woody), 
        pattern = "Woody$", replacement = "")
    lpi.height.tall.herb <- dplyr::select(.data = lpi.detail, 
        PrimaryKey, PointLoc, PointNbr, RecKey, dplyr::matches("Herbaceous$")) \%>\% 
        dplyr::mutate(type = "herbaceous")
    names(lpi.height.tall.herb) <- stringr::str_replace_all(string = names(lpi.height.tall.herb), 
        pattern = "Herbaceous$", replacement = "")
    lpi.height <- rbind(lpi.height.tall.woody, lpi.height.tall.herb) \%>\% 
        merge(x = ., y = lpi.header, all = TRUE, allow.cartesian = TRUE) \%>\% 
        subset(., !is.na(Height))
    if (species.characteristics) {
        species <- read.geodatabase(filepath = filepath, gdb = gdb, 
            feature.name = "tblSpecies")
        names(species)[names(species) == "SpeciesCode"] <- "Species"
        growth.habit <- read.geodatabase(filepath = filepath, 
            gdb = gdb, feature.name = "tblSpeciesGrowthHabit")
        names(growth.habit)[names(growth.habit) == "Code"] <- "GrowthHabitCode"
        species.habit <- merge(x = species, y = growth.habit, 
            by = "GrowthHabitCode", all.x = TRUE, allow.cartesian = TRUE)
        lpi.habit.height <- merge(x = lpi.height, y = species.habit, 
            by = "Species", all.x = TRUE, allow.cartesian = TRUE)
        lpi.habit.height <- unique(lpi.habit.height)
        lpi.habit.height <- lpi.habit.height[!is.na(lpi.habit.height$PrimaryKey), 
            ]
        return(lpi.habit.height)
    }
    lpi.height <- unique(lpi.height)
    lpi.height <- lpi.height[!is.na(lpi.habit.height$PrimaryKey), 
        ]
    return(lpi.height)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
