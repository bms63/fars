#' Read in file with data
#'
#' Function takes in a csv from the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{Fatality Analysis Reporting System} 
#' called a FARS file for a user-specified year.
#'
#' @param filename A \code{string} with filename to be read
#'
#' @return A dataframe created from csv file or will return an error if file does not exist
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv")
#' }
#' @import dplyr
#' @import readr
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' User-defined filename
#' 
#' Functions creates a user-defined filename by appending the year to the filename
#'
#' @param year An \code{integer}
#'
#' @return Creates a filename \code{string} with year given by the user

#'
#' @examples
#' \dontrun{
#' make_filename(2015)
#' }
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Tables of Yearly Accident Data
#'
#' The function will generate a list of dpylr dataframes where each table represents a year of the data.
#' Must be specified by the user.
#'
#' @param years \code{integers}
#'
#' @return Returns a \code{list} of \code{data.frame}s, each item in list corresponds to one of the years in the FARS data.
#'
#' @examples
# \dontrun{
#' # invalid input, year doesn't exist
#' fars_read_years(c(2013, 2014, 1900))
#' }
#' 
#' @import dplyr
#' 
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

#' Table of Yearly by Monthly Accident Data
#' 
#' Summarizes observations by months for the user-defined years in a table
#'
#' @param years \code{integers}
#'
#' @return Creates a \code{tibble} of where columns is years and where rows represents months of the data
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#' 
#' @import dplyr
#' @import tidyr
#' 
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots observations of data
#' 
#' User provides a state and the function will create a plot of observations of accidents
#'
#' @param state.num \code{Integer} that represents the state
#' @param year integer
#' @inheritParams make_filename
#'
#' @return Returns a plot of the state with plot accidents.  Returns error if state number is not valid.  
#' Returns a message if no data is avaialable for that state.
#'
#' @examples
#' \dontrun{
#' fars_map_state(50,2013)
#' }
#' 
#' @import dplyr
#' @import maps
#' @import graphics 
#' 
#' 
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
