#' CSV File Reader.
#' @description  Reads a csv file, given the filename.
#' @note Requires 'readr' and 'dplyr' packages to be loaded first.
#' @param filename This should be a character value. If the file is in the present working directory, then the
#'                 name of the file which is to be read, otherwise the full path to acces the file (with file name).
#'
#' @return The data present in the file.This is an object of class 'tbl_df'.
#' @examples
#' ## Considering your data is in the present working directory
#' mydata <- fars_read("accident_2013.csv.bz2")
#' str(mydata)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
#  if(!file.exists(filename))
  #  stop("file '", filename, "' does not exist")


 # data <- suppressMessages({
  #  readr::read_csv(filename, progress = FALSE)
    #baseLoc <- system.file(package="Assignment1")
   # extPath <- file.path(baseLoc, "data")
    #system.file("exdata", "accident_2013.csv.bz2.rda", package = "Assignment1")
   #readr::read_csv(system.file(file.path("extdata",filename), package="Assignment1"), quote = "", progress = FALSE)

  data <-  readr::read_csv(system.file("extdata",filename, package="Assignment1"))
    #  readr::read_csv(system.file("extdata",filename, package="Assignment1"))
   # system.file("extdata", sprintf("accident_%d.csv.bz2", year), package ="Assignment1")
    #readr::read_csv(paste0(extPath,"/",filename))

    #
 # })
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
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df mutate select %>%
#' @importFrom stats setNames
#' @examples
#' ## Considering your data is in the present working directory
#' ## The following would work fine and would return the subsetted data.
#' mydata <- fars_read_years(2014)
#' str(mydata)
#'
#' @export
fars_read_years <- function(years) {
 # lapply(years, function(year) {
    file <- make_filename(years)
    tryCatch({
      dat <- fars_read(file)
      yr <- "year"
      mnth <- "MONTH"

      indexx <- which(names(dat) == 'year')
      indexx2 <- which(names(dat) == 'MONTH')

      dat %>% dplyr::mutate_(.dots = setNames(years,yr)) %>%   #dat[,indexx]
        dplyr::select_( mnth ,  yr) #dat[,indexx2]   dat[,indexx]
    }, error = function(e) {
      warning("invalid year: ", years)
      return(NULL)
   # }

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
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @examples
#' ## Considering your data is in the present working directory
#' ## The following would work fine and would return the subsetted data.
#' mydata <- fars_summarize_years(2014)
#' str(mydata)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  yr <- "year"
  mnth <- "MONTH"
  n <- "n"
  indexx <- which(names(dat_list) == 'year')
  indexx2 <- which(names(dat_list) == 'MONTH')
  dat_list <- dplyr::mutate_(dat_list, yr =  years) %>%  # dat[,indexx]
    dplyr::select_( mnth , yr ) #dat[,indexx2] , dat[,indexx]

  dat_list <-   dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(yr, mnth) %>% #dat[,indexx],dat[,indexx2]
    dplyr::summarize_(n = 'n()') %>%
    tidyr::spread_(yr, "n")  #dat[,indexx]
  return(dat_list)
}




#' Plots accidents on a state map.
#' @description Plots accidents occuring in a particular state on the state map (US).
#' @note Requires 'readr', 'dplyr', 'tidyr' and 'maps' packages to be loaded first.
#' @param state.num The state number corresponding to a state in the US. This should be integer between 1 & 56
#'                  ( both lower & upper limits included).
#' @param year The year for which the accident data should be take (can take any value from 2013, 2014, 2015)
#'
#' @return A plot of accidents occuring in a particular state on the state map (US). A NULL object is returned.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df bind_rows group_by summarize filter %>%
#' @importFrom tidyr spread
#' @examples
#' ## Considering your data is in the present working directory
#' ## The following would work fine and would return the subsetted data.
#' mydata <- fars_map_state(55, 2013)
#' str(mydata)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  dataa <- fars_read(filename)
  state.num <- as.integer(state.num)
  st <- "STATE"
  indexx <- which(names(dataa) == 'STATE')
  if(!(state.num %in% unique(as.data.frame( dataa[,st])[,1])  ) )
    stop("invalid STATE number: ", state.num)
  dataa.sub <- dataa %>% dplyr::filter(dataa[,indexx] == state.num)  #  st
  #dplyr::filter_(dataa,st == state.num)
  if(nrow(dataa.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(dataa.sub$LONGITUD) <- dataa.sub$LONGITUD > 900
  is.na(dataa.sub$LATITUDE) <- dataa.sub$LATITUDE > 90
  with(dataa.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
# accident_2013.csv.bz2 <- readr::read_csv("F:/Coursera/Mastering Software Development in R/3 Building R Packages/2 Week2/Package Building/Assignment1/inst/exdata/accident_2013.csv.bz2")
# accident_2014.csv.bz2 <- readr::read_csv("F:/Coursera/Mastering Software Development in R/3 Building R Packages/2 Week2/Package Building/Assignment1/inst/exdata/accident_2014.csv.bz2")
# accident_2015.csv.bz2 <- readr::read_csv("F:/Coursera/Mastering Software Development in R/3 Building R Packages/2 Week2/Package Building/Assignment1/inst/exdata/accident_2015.csv.bz2")
# devtools::use_data(accident_2013.csv.bz2)
# devtools::use_data(accident_2014.csv.bz2)
# devtools::use_data(accident_2015.csv.bz2)

# year <- as.integer(year)
# file <- sprintf("accident_%d.csv.bz2", year)
# system.file("extdata", file, package="Assignment1")
