# Retrieve NRI table names
#' Retrieve NRI table names from text
#' @description Retrieves the table names using the specified file referring to the folder containing NRI txt files
#' @param nri_path Folder path. The path where the nri txt files are stored.
#' @returns Character string of the nri table names.
#'
#' @export table_name
table_name <- function(nri_path){
  nri_files <- data.frame(file_path = list.files(path = nri_path,
                                                 pattern = ".txt",
                                                 recursive = T,
                                                 include.dirs = T))
  table_name <- gsub(".txt","",nri_files$file_path)
  return(table_name)
}



# Read NRI tables
#' Read NRI txt files using table_name
#' @description Assign the column header to the NRI text files and save
#' @param table_name Character string of the nri table names
#' @param dsn File path where NRI text files stored
#' @param GL_schema_path Tables and names from NRI Grazing Land Guide
#' @returns NRI files with column name assigned
#'
#' @export read_nri_text
read_nri_text <- function(table_name, dsn, DBKey = "auto", GL_schema_path) {
  # set up table/column names
  schema <- readxl::read_xlsx(GL_schema_path, sheet = 2) |>
    # remove TABLE names
    subset(`Field name` !="TABLE")
  table_names <- schema$`Table name` %>%
    unique() %>%
    toupper()

  for(table in table_name){
  # read text file to table
  data <- lapply(X = dsn, function(X) {
    # set the DBKey, if "auto" we'll populate from the folder. Otherwise, we'll use the text specified.
    file.DBKey <- basename(X)

    # specify file
    file <- paste(X, tolower(table), ".txt", sep = "")

    # Check that the file exists
    if (!file.exists(file)) {
      # if the dsn doesn't exist
      warning(paste("Table", tolower(table_name)), " does not exist in ", X)
      return(data.frame())
    } else {

      # Read the table from the dsn
      # Set the colClasses, which is the in nri.column.explanations
      colClasses <- schema %>%
        subset(`Table name` == toupper(table_name),
          select = `Data type`
        ) %>%
        unlist() %>%
        as.vector()

      # Add NA for an extra field that may be added because of an extra separator column
      field.count <- max(readr::count_fields(
        file = file,
        tokenizer = readr::tokenizer_delim("|")
      ))
      base_length <- length(colClasses)

      # There may be more or fewer columns than expected by the explanations
      # Adjust accordingly by adding NA columns if there are more fields in the
      # file than expected or subsetting if fewer
      if (field.count < base_length) {
        warning(
          "Table ", X,
          " cannot be read in because it does not have the expected number of fields"
        )
        return(data.frame())
      } else {
        colClasses <- c(
          colClasses,
          rep(times = max(field.count - base_length, 0), NA)
        )

        # Read the text file
        data <- read.delim(
          file = file,
          stringsAsFactors = FALSE,
          strip.white = TRUE,
          header = FALSE,
          # colClasses = colClasses,
          sep = "|",
          flush = TRUE,
          na.strings = c("", "."), quote = ""
        ) # Na strings may be blank or periods

        # Add field names
        # Get the field names for the appropriate table as a vector
        colnames <- schema %>%
          subset(`Table name` == toupper(table_name) & `Field name` != "TABLE",# & DBKey == file.DBKey,
            select = `Field name`
          ) %>%
          unlist() %>%
          as.vector()
        # Subset the colnames to the length of the field names for the data
        colnames <- colnames[1:ncol(data)] %>% na.omit()
        # Assign field names
        names(data) <- colnames

        # If there is an NA field at the end, because there is an extra "|" at
        # the end of the file, let's remove it
        data <- data[, !is.na(colnames(data))]

        # If there is a STATE field, make sure it is 2 digits by adding leading 0
        if ("STATE" %in% colnames) {
          data <- data %>%
            dplyr::mutate(STATE = stringr::str_pad(
              string = STATE,
              width = 2,
              side = "left",
              pad = "0"
            ))
        }

        # If there is a COUNTY field, make sure it is 3 digits by adding leading 0
        if ("COUNTY" %in% colnames) {
          data <- data %>%
            dplyr::mutate(COUNTY = stringr::str_pad(
              string = COUNTY,
              width = 3,
              side = "left",
              pad = "0"
            ))
        }


        # If this isn't a supporting table, build a PrimaryKey and
        # DBKey for each record, and add FIPSPSUPNT for use by CEAP-GL
        # if the table contains Longitude field, add - sign to indicate western
        # hemisphere
        UID <- data.frame()
        if (all(c("FIELD_LONGITUDE", "TARGET_LONGITUDE") %in% colnames)) {
          data <- data %>%
            dplyr::mutate(
              FIELD_LONGITUDE = stringr::str_c("-", FIELD_LONGITUDE,
                                               sep = ""
              ),
              TARGET_LONGITUDE = stringr::str_c("-", TARGET_LONGITUDE)
            )

          data$PSU_POINT <- paste0(data$PSU, "_", data$POINT)


          # pool of characters to generate the UID
          pool <- c(0:9, letters, LETTERS)

          # unqiue PSU_POINT combo shares a UID

          UID_lookup <- data %>%
            distinct(PSU_POINT) %>% #if same PSU_POINT then same UID, throw away all other cols
            rowwise() %>%
            mutate(UID_Value = paste(sample(pool, 28, replace = TRUE), collapse = "")) %>%
            ungroup()

          # join the lookup back to the original data to keep EVERY column including location
          UID <- data %>%
            left_join(UID_lookup, by = "PSU_POINT")

          # drop the cols except the sensitive info
          UID <- UID %>% dplyr::select(UID_Value, PSU_POINT, TARGET_LATITUDE, TARGET_LONGITUDE,
                                       FIELD_LATITUDE, FIELD_LONGITUDE)
          #keep our look up table

          #instead have uid to diff path created in the funct
          # create a dir to save nonsenesitive data to
          sensitive_data <- file.path(path_parent, "sensitive_data")

          # set up directories if not yet in parent folder
          if(!dir.exists(sensitive_data)) dir.create(sensitive_data)

          #also keep the senesitive lat lon here and remove lat lon from point
          write.csv(UID, paste0(sensitive_data,"/UID.csv"))

          # now remove the sensitive cols
          # unique key any pattern
          pattern <- "latitude|longitude"

          # Subset the dataframe to keep only columns that DO NOT match the pattern
          data <- data[, !grepl(pattern, names(data), ignore.case = TRUE)]

        }

        if ("SURVEY" %in% colnames) {
          UID <- read.csv(paste0(path_parent,"/sensitive_data/UID.csv"))
          data$PSU_POINT <- paste0(data$PSU, "_", data$POINT)

          data <- data %>%
            # Join ONLY the PSU_POINT and the UID_Value column
            left_join(UID %>% select(PSU_POINT, UID_Value), by = "PSU_POINT") %>%
            mutate(
              PrimaryKey = paste(SURVEY, STATE, COUNTY, UID_Value, sep = ""),
              DBKey = file.DBKey
            ) %>%
            select(-UID_Value)

          #remove sensitive data
          data$PSU <- NULL
          data$POINT <- NULL
          data$PSU_POINT <- NULL

          # need to remove unique point key but has different upper or lower case and may not
          # be in every table
          # unique key any pattern
          pattern <- "unique.*key" #even if has a word like point in between

          # Subset the dataframe to keep only columns that DO NOT match the pattern
          data <- data[, !grepl(pattern, names(data), ignore.case = TRUE)]

        }
      }

      return(data)
    }
  })
  }

  # Merge all data from different files into a single data frame
  df <- dplyr::bind_rows(data) %>% dplyr::distinct()

  return(df)
}

