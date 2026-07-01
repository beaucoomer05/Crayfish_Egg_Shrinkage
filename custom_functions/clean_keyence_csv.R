#' Clean csv files from Keyence measurements.
#'
#' @param file_path character Input vector.
#'
#' @returns A tibble.
#' @export 
#'
#' @examples
#' # 1. Get your files
#' file_paths <- list.files(path = "data/raw", pattern = "\\.csv$", full.names = TRUE)
#' 
#' # 2. Process and combine everything into one master tibble
#' compiled_data <- suppressWarnings(
#'  file_paths %>% 
#'    set_names(basename(.)) %>% 
#'    map(clean_keyence_csv) %>% 
#'    list_rbind(names_to = "source_file")
#' )

clean_keyence_csv <- function(file_path) {
  
  # Optional: Extract the saved date from row 2 before skipping it
  file_lines <- readr::read_lines(file_path, n_max = 2)
  saved_date <- stringr::str_extract(file_lines[2], "\\d{1,2}/\\d{1,2}/\\d{4}")
  
  # Read the file skipping the first 4 metadata rows
  readr::read_csv(file_path, skip = 4, show_col_types = FALSE) %>% 
    # Drop rows where "No." is missing or isn't a plain number
    dplyr::filter(!is.na(`No.`), stringr::str_detect(`No.`, "^\\d+$")) %>% 
    # Convert "No." to a numeric type now that it is clean
    dplyr::mutate(
      `No.` = as.numeric(`No.`),
      date_saved = lubridate::mdy(saved_date) # Add the file date to every row
    )
}
