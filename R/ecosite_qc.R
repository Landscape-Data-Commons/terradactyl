#' Normalize ecological site IDs to match AIM data format
#' @param ecosite Ecological site ID (character string)
#' @return A character string with updated format

#' @export ecosite_qc
#' @rdname ecosite_qc
ecosite_qc <- function(ecosite){
  
  # prefix with R if the ecosite begins with a digit
  ecosite_addR <- dplyr::case_when(stringr::str_detect(ecosite, "^[0-9]") ~
                                paste0("R", ecosite),
                              TRUE ~ ecosite) %>%
    stringr::str_trim()
  
  # QC to remove errors known in the LDC data
  # recode NRI codes to unknown
  # -XE - No Eco Site Established code, XW- Water, XR- Road, XI - Inaccessible, or XN - Not eligible
  ecosite_QC = dplyr::recode(
    ecosite_addR,
    XE = "UNKNOWN",
    XW = "UNKNOWN Water",
    XR = "UNKNOWN Road",
    XI = "UNKNOWN",
    XN = "UNKNOWN",
    NANAXE = "UNKNOWN",
    NANAXW = "UNKNOWN Water",
    NANAXR = "UNKNOWN Road",
    NANAXN = "UNKNOWN",
    NANAXI = "UNKNOWN",
    NANATX = "UNKNOWN",
    NANANA = "UNKNOWN",
    Unknown = "UNKNOWN",
    `Not available` = "UNKNOWN",
    none = "UNKNOWN",
    Unknowon = "UNKNOWN",
    `Not available on WSS` = "UNKNOWN",
    ORUNKNOWN = "UNKNOWN")
  
  return(ecosite_QC)
}
