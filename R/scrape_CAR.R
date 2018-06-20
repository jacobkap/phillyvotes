# Bug on page 1734 of file 2012 General RTC 3 CAR 1 of 2.pdf
# There is a timestamp at 11/06/12-19:312:25
# with an impossible millisecond time. ALl other times have only
# 2 digits for milliseconds. Also some voters who start after 8PM.
#
# Page 1030-1040 of file 2012 General RTC 5 CAR 1 of 2.pdf,
# machine number 021617, has timestamps of all 0. For example,
# 00/00/00-00:00:00. These data still have voters casting ballots.

scrape_CAR <- function(file){
  library(dplyr)
  ### Convert PDF to lines of text ###
  # suppressWarnings() simply hides and error that cannot be
  # addressed but doesn't impact the output
  data <- suppressMessages(pdftools::pdf_text(file))
  data <- unlist(strsplit(data, split = "\n"))
  data <- trimws(data)
  data <- tolower(data)

  pdf_page <- data
  pdf_page[!grepl("page.* [0-9]", pdf_page)] <- NA
  pdf_page <- readr::parse_number(pdf_page)


  precinct <- data
  precinct[grep("precinc", precinct, invert = TRUE)] <- NA
  precinct <- gsub("[a-z].* ", "", precinct)
  time     <- data
  time     <- gsub("precin.*", NA, time)



  ### Serial Number ###
  serial <- gsub(".*(^[0-9]{6}).*", "\\1", data)
  serial[!grepl("^[0-9]{6}", serial)] <- NA

  data          <- data.frame(precinct,
                             time,
                             serial,
                             pdf_page,
                             stringsAsFactors = FALSE)
  data$action   <- gsub(".*:[0-9]* +", "", data$time)
  data$time     <- gsub("^[0-9]{6} +", "", data$time)
  data$time     <- gsub(" .*", "", data$time)
  data$time[data$time == data$serial] <- NA

  data$precinct <- zoo::na.locf(data$precinct,
                                na.rm = FALSE)
  data$pdf_page     <- zoo::na.locf(data$pdf_page,
                                na.rm = FALSE,
                                fromLast = TRUE)
  data$precinct <- stringr::str_trim(data$precinct)
  data$serial <- zoo::na.locf(data$serial,
                              na.rm = FALSE)
  data$ward     <- gsub("-.*", "", data$precinct)
  data$division <- gsub(".*-", "", data$precinct)
  data$ward     <- as.numeric(as.character(data$ward))
  data$division <- as.numeric(as.character(data$division))

  data          <- data[data$action %in% "voter cast ballot",]
  data          <- data[!is.na(data$time),]

  data$time     <- lubridate::mdy_hms(data$time)
  data$hour     <- lubridate::hour(data$time)
  data$year     <- lubridate::year(data$time)
  data$file     <- gsub(".*/|.pdf", "", file)

  data <-
    data %>%
    dplyr::select(file,
                  pdf_page,
                  serial,
                  precinct,
                  ward,
                  division,
                  action,
                  time,
                  hour,
                  year)
  return(data)

}
