#' Calculate the vegetation height
#' @description
#' @param lpi.tall A tall/long-format data frame. Use the data frame \code{"height"} from the \code{gather.lpi()} output.
#' @param ... One or more bare variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param omit.zero Logical. If \code{TRUE} the results omit height measurements of \code {0}
#' @param type Indicatoes the type of indicator, \code{"max"} or \code{"mean"}.
#' @export


mean.height<-function(lpi.height.tall,
                      type="mean",
                      omit.zero=FALSE,
                      ...){
  ## Get a list of the variables the user wants to group by.
  grouping.variables <- rlang::quos(...)

  # For grouping by line
  if (by.line) {
    level <- rlang::quos(PrimaryKey, LineKey)
  } else {
    level <- rlang::quos(PrimaryKey)
  }

  #If height of zer is dropped by the calculation, filter out zeros
  if(omit.zero){
    lpi.height.tall<-lpi.height.tall %>% dplyr::filter(Height!=0)
  }

  # Calculate mean height by grouping variable, if type=mean
  if (type=="mean"){
    summary<-lpi.height.tall %>% dplyr::filter(!is.na(Height)) %>%
      dplyr::group_by(!!!level,!!!grouping.variables) %>%
      dplyr::summarize(mean.height=mean(Height))
  }
  # Calculate the max height by grouping variable, if type="max"
  if (type=="max"){
    lpi.height.tall.spread<-lpi.height.tall%>% tidyr::spread(key = type, value=Height)
    lpi.height.tall.spread$max<- pmax(lpi.height.tall.spread$herbaceous, lpi.height.tall.spread$woody, na.rm = TRUE)
    summary<-lpi.height.tall.spread%>%
      dplyr::group_by(!!!level, !!!grouping.variables) %>%
      dplyr::summarize(max.height=mean(max))
  }


  return(summary)
}

