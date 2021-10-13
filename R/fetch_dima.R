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
#' data_allplots <- fetch_api(endpoint = "tblPlots", values = NULL)
#' 
#' data_JERplots <- fetch_api(endpoint = "tblPlots", values = "ProjectKey=JER")
#' 
#' data_gap <- fetch_api(endpoint = list("tblGapHeader", "tblGapDetail"), 
#'                        values = "PrimaryKey=15050113465465692020-09-15")

## Fetch data for a single table
#' @rdname fetch_api
#' @export fetch_api_single
fetch_api_single <- function(api, endpoint, values = NULL, verbose = T){

  if(is.null(values)){
    url <- paste0("https://", api, ".landscapedatacommons.org/api/",endpoint)
  } else {
    url <- paste0("https://", api, ".landscapedatacommons.org/api/",endpoint,"?",values)
  }
  if(verbose) print(paste("Accessing", url))
  get_url <- httr::GET(url) 
  flat_get <- httr::content(get_url, "text", encoding = "UTF-8")
  jsonize <- jsonlite::fromJSON(flat_get, flatten = TRUE)
  df <- as.data.frame(jsonize)
  return(df)
}

## wrapper, automatically detecting if multiple tables are requested
#' @export fetch_api
#' @rdname fetch_api
fetch_api <- function(api, endpoint, values=NULL) {
  api <- tolower(api)
  if(class(endpoint) == "list"){
    print(paste("Fetching", length(endpoint), "tables"))
    out <- lapply(endpoint, fetch_api_single, values = values, api = api)
    names(out) <- endpoint
  } else {
    out <- fetch_api_single(endpoint = endpoint, values = values, api = api)
  }
  
  return(out)
}