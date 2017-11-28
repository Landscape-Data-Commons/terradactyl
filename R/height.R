#' Calculate the vegetation height
#' Mean or max
#' @description
#' @param lpi.tall A tall/long-format data frame. Use the data frame \code{"height"} from the \code{gather.lpi()} output.
#' @param ... One or more bare variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param omit.zero Logical. If \code{TRUE} the results omit height measurements of \code {0}
#' @param method Indicatoes the type of indicator, \code{"max"}, which yields the average maximum height (of the herbaceous or woody heights) on the plot or \code{"mean"} which yields the mean height by functional group (woody/herbaceous).
#' @param by.line Logical.
#' @export


mean.height<-function(lpi.height.tall,
                      method="grouped",
                      omit.zero=FALSE,
                      by.line=FALSE,
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
  if (method=="grouped"){
    summary<-lpi.height.tall %>% dplyr::filter(!is.na(Height)) %>%
      dplyr::group_by(!!!level,!!!grouping.variables) %>%
      dplyr::summarize(mean.height=mean(Height))
  }
  # Calculate the max height by grouping variable, if type="max"
  if (method=="max"){
    lpi.height.tall.spread<-lpi.height.tall%>% tidyr::spread(key = type, value=Height)
    lpi.height.tall.spread$max<- pmax(lpi.height.tall.spread$herbaceous, lpi.height.tall.spread$woody, na.rm = TRUE)
    summary<-lpi.height.tall.spread%>%
      dplyr::group_by(!!!level, !!!grouping.variables) %>%
      dplyr::summarize(max.height=mean(max))
  }


  return(summary)
}

test.height<-function(mean.height){
  possible.error<-mean.height %>% subset(mean.height>1000)


}
