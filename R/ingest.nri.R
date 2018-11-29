# Read NRI text files into a gdb
#' @description Ingestion script to convert NRI text file format to database format
#' @param dsn Vector of file paths to the folders where files are held. The last folder in the directory will be used to set the DBKey.
#' @param out String. Filepath to the output database
#' @param dsn.type Type of database. Defaults to geodatabase
#' @return List of tables and if specified a populated database
#'

#' @export nri_field_names
#' @rdname ingest_nri

nri_field_names <- function(dsn) {

  # Get a list of all data dump field name files
  data.dump.files <- lapply(X = dsn, FUN = function(X) {
    file.names <- list.files(X, full.names = TRUE) %>% subset(grepl(x = ., pattern = ".csv"))
  }) %>% unlist()

  # Read each file name in and append into a master list, retaining the DBKey along the way
  field.names <- lapply(X = as.list(data.dump.files), FUN = function(X) {
    field.names <- read.csv(X, stringsAsFactors = FALSE)
    field.names$DBKey <- basename(dirname(X))
    return(field.names)
  }) %>% dplyr::bind_rows() %>% dplyr::select(-X)

  # We store field names as all upper case
  names(field.names) <- toupper(names(field.names))

  # Change the field types as assigned by the NRI database to R readable format
  field.names$DATA.TYPE <- str_replace_all(
    field.names$DATA.TYPE,
    c(
      "NUMBER" = "numeric",
      "VARCHAR2" = "character",
      "CHAR" = "character"
    )
  )

  # Reformat DBKEY to DBKey
  field.names <- field.names %>% dplyr::mutate(DBKey = DBKEY) %>% dplyr::select(-DBKEY)

  # Merge the original nri.data.column.explataions table saved in the package
  nri.data.column.explanations <- dplyr::full_join(terradactyl::nri.data.column.explanations, field.names) %>% distinct()

  # Look for instances where field of the same name may have different data types assigned
  disjointed.data.types <- nri.data.column.explanations %>% dplyr::group_by(FIELD.NAME) %>% dplyr::summarise(n = n_distinct(DATA.TYPE))

  if (any(disjointed.data.types$n > 1)) {
    stop("There are mismatched data types identified between input files and the original field name explanations. Please check in the input file for consistencies.")
  }

  # Save the new file
  devtools::use_data(nri.data.column.explanations, overwrite = TRUE, internal = TRUE)
  devtools::use_data(nri.data.column.explanations, overwrite = TRUE)

  # Reload package
  devtools::load_all()
}

#' @export read.nri.txt
#' @rdname ingest_nri


read.nri.txt <- function(table.name, dsn, DBKey = "auto") {

  # read text file to table
  data <- lapply(X = dsn, function(X) {
    # set the DBKey, if "auto" we'll populate from the folder. Otherwise, we'll use the text specified.
    file.DBKey <- basename(X)

    # specify file
    file <- paste(X, tolower(table.name), ".txt", sep = "")

    # Check that the file exists
    if (!file.exists(file)) {
      # if the dsn doesn't exist
      warning(paste("Table", tolower(table.name)), " does not exist in ", X)
      return(data.frame())
    } else {
      # Read the table from the dsn
      # Set the colClasses, which is the in nri.column.explanations
      colClasses <- terradactyl::nri.data.column.explanations %>%
        subset(TABLE.NAME == toupper(table.name) & DBKey == file.DBKey, select = DATA.TYPE) %>%
        unlist() %>%
        as.vector()

      # Add NA for an extra field that may be added because of an extra separator column
      field.count <- max(readr::count_fields(file = file,
                                             tokenizer = readr::tokenizer_delim("|")))
      base.length <- length(colClasses)

      # There may be more or fewer columns than expected by the explanations
      # Adjust accordingly by adding NA columns if there are more fields in the
      #file than expected or subsetting if fewer
      if (field.count < base.length) {
        warning("Table ", X, " cannot be read in because it does not have the expected number of fields")
        return(data.frame())
      } else{
        colClasses <- c(colClasses,
                        rep(times = max(field.count - base.length, 0), NA))


        # Read the text file
        data <- read.delim(
          file = file,
          stringsAsFactors = FALSE,
          strip.white = TRUE,
          header = FALSE,
          colClasses = colClasses,
          sep = "|",
          flush = TRUE,
          na.strings = c("", "."), quote = ""
        ) # Na strings may be blank or periods


        # Add field names
        # Get the field names for the appropriate table as a vector
        colnames <- terradactyl::nri.data.column.explanations %>%
          subset(TABLE.NAME == toupper(table.name) & DBKey == file.DBKey,
                 select = FIELD.NAME) %>%
          unlist() %>%
          as.vector()
        # Subset the colnames to the length of the field names for the data
        colnames <- colnames[1:ncol(data)] %>% na.omit()
        # Assign field names
        names(data) <- colnames

        # If there is an NA field at the end, because there is an extra "|" at the end of the file, let's remove it
        data <- data[, !is.na(colnames(data))]

        # If there is a STATE field, make sure it is 2 digits by adding leading 0
        if ("STATE" %in% colnames){
          data <- data %>%
            dplyr::mutate(STATE = stringr::str_pad(string = STATE,
                                                   width = 2,
                                                   side = "left",
                                                   pad = "0"))
        }

        # If there is a COUNTY field, make sure it is 3 digits by adding leading 0
        if ("COUNTY" %in% colnames){
          data <- data %>%
            dplyr::mutate(COUNTY = stringr::str_pad(string = COUNTY,
                                                    width = 3,
                                                    side = "left",
                                                    pad = "0"))
        }


        # If this isn't a supporting table, build a PrimaryKey and
        # DBKey for each record, and add FIPSPSUPNT for use by CEAP-GL
        if ("SURVEY" %in% colnames) {
          data <- data %>% dplyr::mutate(
            PrimaryKey = paste(SURVEY, STATE, COUNTY, PSU, POINT, sep = ""),
            DBKey = file.DBKey,
            FIPSPSUPNT = paste(STATE, COUNTY, PSU, POINT, sep = "")
          )
        }

        # if the table contains Longitude field, add - sign to indicate western
        # hemisphere
        if (c("FIELD_LONGITUDE", "TARGET_LONGITUDE") %in% colnames) {
          data <- data %>%
            dplyr::mutate(FIELD_LONGITUDE =  stringr::str_c("-", FIELD_LONGITUDE,
                                                   sep = "") ,
                          TARGET_LONGITUDE = stringr::str_c("-", TARGET_LONGITUDE)
            )
        }

      }

      return(data)
    }

  })

  # Merge all data from different files into a single data frame
  df <- dplyr::bind_rows(data) %>% dplyr::distinct()

  return(df)
}

#' @export ingest_nri
#' @rdname ingest_nri
ingest_nri <- function(dsn,
                       out,
                       dsn.type = "gdb",
                       rda = TRUE,
                       uid = "",
                       pwd = "",
                       connection = "") {
  # Get the list of unique table names
  table.names <- terradactyl::nri.data.column.explanations$TABLE.NAME %>%
    unique() %>% toupper()

  # Read  tables into the database
  all.tables <- lapply(X = tolower(table.names), function(X) {
    print(X)
    # read all files for the table and merge
    data <- terradactyl::read.nri.txt(dsn = dsn, table.name = X)

    return(data)
  })

  # Set names for tables
  names(all.tables) <- table.names

  # If rda is true, then save an Rda file as well
  if (rda) {
    # Save .Rda file
    save(all.tables,
      file = paste(gsub(pattern = basename(out),
                        replacement = "",
                        x = out),
                   "NRI.Rda",
                   sep = "")
    ) # saved to the gdb file location
  }

  # if dsn.type=="gdb" write files to geodatabase
  ###There may be an error here--check#####
  if (dsn.type == "gdb") {
    # write to geodatabase
    lapply(
      X = names(all.tables[table.names]), FUN =
        function(X) {
          print(X)
          arcgisbinding::arc.write(path = paste(out, X, sep = ""),
                                   data = as.data.frame(all.tables[X]),
                                   overwrite = TRUE)
        }
    )

    arcgisbinding::arc.write(
      path = paste(out, toupper(X), sep = ""),
      data = data,
      overwrite = TRUE
    )
  }
  if (dsn.type == "SQL") {
    # Open connection to SQL database

    con <- odbc::dbConnect(odbc::odbc(),
                           Driver = "SQL Server",
                           Server = "jornada-sqlsrv2.jrn.nmsu.edu",
                           Database = "lcd",
                           UID = "JRN\\samccord",
                           PWD = rstudioapi::askForPassword("Database password"),
                           Port = 1433)

  }

  if (dsn.type %in% c("mdb", "accdb")){
    # Set the access connection based on the 32 bit or 64 bit R
    switch(R.Version()$arch,
           "x86_64" = {
             channel <- RODBC::odbcConnectAccess2007(out)
           },
           "i386" = {
             channel <- RODBC::odbcConnectAccess(out)
           })


    #Write all tables to Access
    sapply(
      X = names(all.tables[table.names]),
        function(X) {
          print(X)
          dat <- as.data.frame(all.tables[[X]])
          RODBC::sqlSave(channel = channel,
                         dat = dat,
                         tablename = X,
                         verbose = TRUE,
                         rownames = FALSE,
                         )
        }

          )
    # Close the database channel when complete
    RODBC::odbcClose(channel)
        }

  return(all.tables)
}


# Subset database
#' @export nri_subset
#' @rdname ingest_nri

nri_subset <- function(data, PK.subset, out) {
  subset.data <- all.tables[which(sapply(all.tables, `[[`,"PrimaryKey" ) %in% PK.subset)]

  test <- rlist::list.filter(all.tables[3:4], PrimaryKey %in% PK.subset)

  # subset data
  subset.data <- lapply(X = all.tables[1:13],
                        function(X) {

                          # Check for data in tables
                          if (length(X[[1]])==0){

                            #Return blank table if no data
                            as.data.frame(X[[1]])

                          } else {
                            #If PrimaryKey is in the names of the table, then subset
                            if ("PrimaryKey" %in% names(X[[1]])){

                              assign(names(X),
                                     subset(X[[1]],PrimaryKey %in% PK.subset))

                            } else {

                              as.data.frame(X[[1]])
                            }
                          }
                          }
                        )



  names(subset.data) <- names(all.tables)

  # write to geodatabase
  if (grepl(out, ".gdb")) {
    lapply(
      X = names(subset.data),
      function(X) arcgisbinding::arc.write(path = paste(out, X, sep = "/"),
                                           data = subset.data[[X]], overwrite = TRUE)
    )
  }

  return(subset.data)

}


