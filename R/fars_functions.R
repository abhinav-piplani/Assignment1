#' CSV File Reader.
#' @description  Reads a csv file, given the filename.
#' @note Requires 'readr' and 'dplyr' packages to be loaded first.
#' @param filename This should be a character value. If the file is in the present working directory, then the
#'                 name of the file which is to be read, otherwise the full path to acces the file (with file name).
#'
#' @return The data present in the file.This is an object of class 'tbl_df'.
#' @examples
#' ## Considering your data is in the present working directory
#' library(readr)
#' library(dplyr)
#' mydata <- fars_read("accident_2013.csv.bz2")
#' str(mydata)
#'
#' ## The following will throw error in case extension (.csv.bz2) is missing in the argument.
#' mydata <- fars_read("accident_2013")
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
  #  readr::read_csv(filename, progress = FALSE)

    readr::read_csv(system.file(file.path("exdata",filename), package="Assignment1"), progress = FALSE)

  })
  dplyr::tbl_df(data)
}




#' File Name Generator.
#' @description  Generates a filename (character) in the format similar to other files given as a part of the 1st
#'               assignment of the course 'Building R Packages" on Coursera.
#' @param year The year for which the filename has to be generated.This should be an integer value.
#'
#' @return The filename in the above mentioned format for the year which has been supplied. (An object of class
#'         character).
#' @examples
#' myfilename <- make_filename(2020)
#' str(myfilename)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}




#' Subsetting 'Year' & 'Month' variables from the data.
#' @description Subsets Year & Month variables from the data, if the file is present in the working directory
#'             (in case year = 2013, 2014, 2015), otherwise prints 'Invalid year'.
#' @note Requires 'readr' and 'dplyr' packages to be loaded first.
#' @param years The year for which the Year & Month variables are to be subsetted from the data. This should
#'              be integer.
#'
#' @return The data present in the file (in case year = 2013, 2014, 2015). This is an object of class 'tbl_df'.
#'          In case the file is not present corresponding to the year supplied in the argument, it returns a
#'          list of length 1 with object as NULL.
#'
#' @examples
#' ## Considering your data is in the present working directory
#' ## The following would work fine and would return the subsetted data.
#' library(readr)
#' library(dplyr)
#' mydata <- fars_read_years(2014)
#' str(mydata)
#'
#' ## The following would work fine and would return a list of length 1 with NULL object.
#' mydata <- fars_read_years(2021)
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}





#' Summarize 'Year' & 'Month' variables from the data.
#' @description Summarizes Year & Month variables from the data, if the file is present in the working directory
#'             (in case year = 2013, 2014, 2015), otherwise prints 'Invalid year'
#' @note Requires 'readr', 'dplyr' and 'tidyr' packages to be loaded first.
#' @param years The year for which the Year & Month variables are to be Summarized from the data. This should
#'              be integer.
#'
#' @return The count of number of observations present in the file (in case year = 2013, 2014, 2015) for each month.
#'         This is an object of class 'tbl_df'. In case the file is not present corresponding to the year supplied
#'          in the argument, it throws an error.
#'
#' @examples
#' ## Considering your data is in the present working directory
#' ## The following would work fine and would return the subsetted data.
#' library(readr)
#' library(dplyr)
#' library(tidyr)
#' mydata <- fars_summarize_years(2014)
#' str(mydata)
#'
#' ## The following would also throw an error.
#' mydata <- fars_summarize_years(2021)
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}




#' Plots accidents on a state map.
#' @description Plots accidents occuring in a particular state on the state map (US).
#' @note Requires 'readr', 'dplyr', 'tidyr' and 'maps' packages to be loaded first.
#' @param state.num The state number corresponding to a state in the US. This should be integer between 1 & 56
#'                  ( both lower & upper limits included).
#' @param years The year for which the accident data should be take (can take any value from 2013, 2014, 2015)
#'
#' @return A plot of accidents occuring in a particular state on the state map (US). A NULL object is returned.
#'
#' @examples
#' ## Considering your data is in the present working directory
#' ## The following would work fine and would return the subsetted data.
#' library(readr)
#' library(dplyr)
#' library(tidyr)
#' library(maps)
#' mydata <- fars_map_state(55, 2013)
#' str(mydata)
#'
#' ## The following would also throw an error.
#' mydata <- fars_map_state(550, 2013) # Invalid state number
#' mydata <- fars_map_state(55, 2010) # Invalid data file
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
