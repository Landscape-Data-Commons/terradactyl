# #### TESTING #######
#
# stipa_data_example_path <- "~/Projects/terradactyl/testing/stipa_lpi_qc_feb2026/data"
#
# # Get the list of all the Stipa text files
# # Combine them into a single data frame
# # Split them into a named list of per-form data frames
# data_combined <- list.files(path = stipa_data_example_path,
#                         pattern = ".txt$",
#                         recursive = TRUE,
#                         full.names = TRUE,
#                         include.dirs = FALSE) |>
#   lapply(X = _,
#                      sep = "\t",
#                      skip = 7,
#                      header = TRUE,
#                      FUN = read.table) |>
#   dplyr::bind_rows()
#
# form_names <- unique(data_combined$Form) |>
#   stringr::str_replace_all(string = _,
#                            pattern = "[ \\+]",
#                            replacement = "_")
#
# data_list <- lapply(X = setNames(object = unique(data_combined$Form),
#                                  nm = form_names),
#                     data = stipa_file,
#                     FUN = function(X, data){
#                       dplyr::filter(.data = data,
#                                     Form == X) |>
#                         dplyr::distinct()
#                     })
#
#
# data <- dplyr::filter(.data = stipa_file,
#                       Form == "lpi+height",
#                       Attribute %in% c("soil surface",
#                                        "lower layer 1",
#                                        "lower layer 2",
#                                        "lower layer 3"),
#                       !Values %in% (dplyr::filter(.data = stipa_file,
#                                                  Form == "lpi+height",
#                                                  Attribute %in% c("top layer code")) |>
#                         dplyr::pull(.data = _,
#                                     Values) |>
#                         unique())) |>
#   dplyr::pull(.data = _,
#               Values) |>
#   unique() |> dput()
#
# biocrust_codes <- c("IC",
#                     "FC",
#                     "LCC",
#                     "LCC/PC",
#                     "DCC",
#                     "LCCBF/PC",
#                     "PC",
#                     "DCC/PC",
#                     "DCCBF/PC",
#                     "IC/PC",
#                     "COLLE2",
#                     "CLAV",
#                     "CB",
#                     "LCC/CC",
#                     "LCCBF/CC",
#                     "IC/CC",
#                     "COLLE2/CC",
#                     "DCCBF/CC",
#                     "CC",
#                     "COLLE2/PC",
#                     "DCC/CC",
#                     "CLAV/PC")
#
#
# biocrust_codes[nchar(biocrust_codes) >= 3]
#
#
# uid_var = "Plot"
# variable_var = "Attribute"
# value_var = "Values"
# topcanopy_string = "top layer"
# lowerlayer_string = "lower layer"
# lowerlayer_regex = TRUE
# soilsurface_string = "soil surface"
# additional_vars = c("PlotID" = "Plot.name",
#                     "Observation",
#                     "Date")
#
# test_df <- data.frame(var_name = c("top canopy",
#                                    "soil surface",
#                                    "lower layer 1",
#                                    "lower layer 2",
#                                    "lower layer 3"))
#
# lowerlayer_string <- c("lower layer 1",
#                        "lower layer 2",
#                        "lower layer 3")
#
# dplyr::mutate(.data = test_df,
#               var_name = dplyr::replace_values(x = var_name,
#                                                from = lowerlayer_string,
#                                                to = paste0("Lower",
#                                                            seq_len(length.out = length(lowerlayer_string)))))
#
# #### FUNCTIONS ####
# # Convert Stipa data into a format that we can jam into the workflow
# # @param data Whatever
# # @param uid_var Character string. The name of the variable that is uniquely identifying for plots. The values from this variable will be in the variable PrimaryKey in the output. Defaults to \code{"Plot"}.
# translate_stipa_lpi <- function(data,
#                                 uid_var = "Plot",
#                                 variable_var = "Attribute",
#                                 value_var = "Values",
#                                 topcanopy_string = "top layer",
#                                 lowerlayer_string = "lower layer",
#                                 lowerlayer_regex = TRUE,
#                                 soilsurface_string = "soil surface",
#                                 additional_vars = c("PlotID" = "Plot.name",
#                                                     "Observation",
#                                                     "Date")) {
#   # Sort out the incoming variables.
#   # Using this to rename things internally so that they're easy to
#   data <- dplyr::select(.data = data,
#                         tidyselect::all_of(x = c("PrimaryKey" = uid_var,
#                                                  "var_name" = variable_var,
#                                                  "values" = value_var,
#                                                  additional_vars)))
#
#   # Rework the variable names based on the topcanopy-, lowerlayer-, and
#   # soilsurface-related arguments
#   data <- dplyr::mutate(.data = data,
#                         var_name = dplyr::replace_values(x = var_name,
#                                                          topcanopy_string ~ "TopCanopy",
#                                                          soilsurface_string ~ "SoilSurface"))
#   if (length(lowerlayer_string) == 1 & lowerlayer_regex) {
#     data <- dplyr::mutate(.data = data,
#                           var_name = dplyr::case_when(stringr::str_detect(string = var_name,
#                                                                           pattern = lowerlayer_string) ~ paste0("Lower",
#                                                                                                                 stringr::str_extract(string = var_name,
#                                                                                                                                      pattern = "\\d+")),
#                                                       .default = var_name))
#   } else {
#     data <- dplyr::mutate(.data = test_df,
#                           var_name = dplyr::replace_values(x = var_name,
#                                                            from = lowerlayer_string,
#                                                            to = paste0("Lower",
#                                                                        seq_len(length.out = length(lowerlayer_string)))))
#   }
#
#   # Split out the Observation variable and use it to make LineKey, PointNbr, and
#   # RecKey
#   # LineKey is the within-plot uniquely identifying variable for the transects
#   # PointNbr is the within-transect numeric unique ID for each pin drop where
#   # the first pin drop is 1 and each increments by 1 working down the line
#   # RecKey is the globally-unique identifying variable for each transect made
#   # by combining the PrimaryKey and the LineKey
#   data <- tidyr::separate_wider_delim(data = data,
#                                       cols = Observation,
#                                       delim = "|",
#                                       names = c("LineKey",
#                                                 "PointNbr")) |>
#     dplyr::mutate(.data = _,
#                   RecKey = paste(PrimaryKey,
#                                  LineKey,
#                                  sep = "_"))
#
#
#
#
#   widening_vars <- unique(data$var_name)[stringr::str_detect(string = unique(data$var_name),
#                                                              # pattern = "height|species|notes$")]
#                                                              pattern = "notes$")]
#
#   wide_data <- dplyr::select(.data = test,
#                              tidyselect::all_of(x = c("PrimaryKey",
#                                                       "LineKey",
#                                                       "RecKey",
#                                                       "PointNbr",
#                                                       "var_name",
#                                                       "value"))) |>
#     dplyr::filter(.data = _,
#                   var_name %in% widening_vars) |>
#     dplyr::distinct() |>
#     dplyr::mutate(.data = _,
#                   Attribute = dplyr::case_match(.x = Attribute,
#                                                 "herbaceous height" ~ "herbaceous",
#                                                 "woody height" ~ "woody",
#                                                 "lpi+height notes" ~ "notes")) |>
#     tidyr::pivot_wider(data = _,
#                        names_from = Attribute,
#                        values_from = Values)
#
#   layer_values <- unique(data$Attribute)[stringr::str_detect(string = unique(data$Attribute),
#                                                              pattern = "surface|code")]
#
#   layer_data <- dplyr::select(.data = test,
#                               tidyselect::all_of(x = c("PrimaryKey",
#                                                        "LineKey",
#                                                        "RecKey",
#                                                        "PointNbr",
#                                                        "Attribute",
#                                                        "Values"))) |>
#     dplyr::filter(.data = _,
#                   Attribute %in% layer_values) |>
#     dplyr::mutate(.data = _,
#                   layer = dplyr::case_match(.x = Attribute,
#                   ))
# }
#
#
# translate_stipa_height <- function(data){
#
#   height <- stipa_file |> subset(Form == "lpi+height" & Attribute %in% c("woody height", "herbaceous height", "woody species", "herbaceous species")) |>
#     dplyr::select(.data = _,
#                   -tidyselect::any_of(x = c("Attribute.order",
#                                             "Form"))) |>
#     dplyr::distinct()
#
#   # split Observation by | to LineKey and PointNbr
#   height[c("LineKey", "PointNbr")] <-  stringr::str_split_fixed(height$Observation, pattern = "\\|", n=2)
#
#   # We're handling species and the heights manually and using a full_join to combine
#   # them.
#   # #Then we throw away variables we don't want and that can produce duplicate
#   # records before removing "dead" from species names and using that info to
#   # create the chckbox variable for live/dead
#   height <- dplyr::full_join(
#     # The species codes
#     x = dplyr::filter(.data = height,
#                       stringr::str_detect(string = Attribute,
#                                           pattern = "species")) |>
#       dplyr::mutate(.data = _,
#                     Species = Values,
#                     type = stringr::str_remove(string = Attribute,
#                                                pattern = "species") |>
#                       stringr::str_trim(string = _)) |>
#       dplyr::select(.data = _,
#                     -tidyselect::any_of(x = c("Values",
#                                               "Attribute",
#                                               "Date"))),
#     # The heights themselves
#     y = dplyr::filter(.data = height,
#                       stringr::str_detect(string = Attribute,
#                                           pattern = "height")) |>
#       dplyr::mutate(.data = _,
#                     Height = Values,
#                     type = stringr::str_remove(string = Attribute,
#                                                pattern = "height") |>
#                       stringr::str_trim(string = _)) |>
#       dplyr::select(.data = _,
#                     -tidyselect::any_of(x = c("Values",
#                                               "Attribute",
#                                               "Date"))),
#     relationship = "one-to-one",
#     by = c("Plot",
#            "Plot.name",
#            "Observation",
#            "type",
#            # "Date",
#            "LineKey",
#            "PointNbr")) |>
#     # add checkbox and other needed fields
#     dplyr::mutate(.data = _,
#                   RecKey = LineKey,
#                   chckbox = stringr::str_detect(string = Species,
#                                                 pattern = "dead"),
#                   Species = Species |> stringr::str_remove("\\|dead|dead\\|"))
#
#   # Add any notes in!!!!
#   height <- dplyr::left_join(x = height,
#                              y = dplyr::filter(.data = stipa_file,
#                                                Attribute %in% "lpi+height notes") |>
#                                # add notes field
#                                dplyr::mutate(.data = _,
#                                              Notes = Values) |>
#                                # remove redundant fields that will mess with the join
#                                dplyr::select(.data = _,
#                                              tidyselect::any_of(x = c("Notes",
#                                                                       "Plot",
#                                                                       "Plot.name",
#                                                                       "Observation"))),
#                              relationship = "many-to-one",
#                              by = c("Plot",
#                                     "Plot.name",
#                                     "Observation"))
#
#   # reconcile height unknown codes
#   height_code_updated <- dplyr::left_join(x = height,
#                                           y = unknown_spp |>
#                                             dplyr::select(.data = _,
#                                                           PrimaryKey, Species, `specimen code`),
#                                           by = c("Species", "PrimaryKey"),
#                                           relationship = "many-to-one")
#
#   height_code_updated <- height_code_updated |>
#     dplyr::mutate(Species = dplyr::coalesce(`specimen code`, Species)) |> dplyr::select(-`specimen code`)
#
#   height <- height_code_updated
# }
