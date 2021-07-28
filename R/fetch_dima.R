#' Fetch DIMA data from the Landscape Data Commons API (requires internet connection)
#'
#' @description Given a table name and optional field query, download DIMA data from the API.
#' @param endpoint Character string or list. Name of the requested table e.g. "tblPlots". 
#' Provide a list of character strings to download more than one table. 
#' @param values Optional. JSON query as character string. If a list of endpoints is provided, the filter column must be present in all requested tables.
#' @param verbose If true, print the URL of the requested record.
#' @return A data frame containing DIMA data of the requested table, or a list
#' of data frames containing the requested tables. 
#' @examples 
#' data_allplots <- fetch_dima(endpoint = "tblPlots", values = NULL)
#' 
#' data_JERplots <- fetch_dima(endpoint = "tblPlots", values = "ProjectKey=JER")
#' 
#' data_gap <- fetch_dima(endpoint = list("tblGapHeader", "tblGapDetail"), 
#'                        values = "PrimaryKey=15050113465465692020-09-15")

## Fetch data for a single table
#' @rdname fetch_dima
#' @export fetch_dima_single
fetch_dima_single <- function(endpoint, values = NULL, verbose = T){
  if(is.null(values)){
    url <- paste0("https://dima.landscapedatacommons.org/api/",endpoint)
  } else {
    url <- paste0("https://dima.landscapedatacommons.org/api/",endpoint,"?",values)
  }
  if(verbose) print(paste("Accessing", url))
  get_url <- GET(url) 
  flat_get <- content(get_url, "text", encoding = "UTF-8")
  jsonize <- fromJSON(flat_get, flatten = TRUE)
  df <- as.data.frame(jsonize)
  return(df)
}

## wrapper, automatically detecting if multiple tables are requested
#' @export fetch_dima
#' @rdname fetch_dima
fetch_dima <- function(endpoint,values=NULL) {
  
  if(class(endpoint) == "list"){
    out <- lapply(endpoint, fetch_dima_single, values = values)
    names(out) <- endpoint
  } else {
    out <- fetch_dima_single(endpoint = endpoint, values = values)
  }
  
  return(out)
}
