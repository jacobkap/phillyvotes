scrape_CAR <- function(file){
  ### Convert PDF to lines of text ###
  # suppressWarnings() simply hides and error that cannot be
  # addressed but doesn't impact the output
  data <- suppressMessages(pdftools::pdf_text(file))
  data <- unlist(strsplit(data, split = "\n"))
  data <- trimws(data)
  data <- tolower(data)


  data     <- data[grepl("precinct|voter cast ballot", data)]
  precinct <- data
  precinct[grep("precinc", precinct, invert = TRUE)] <- NA
  time     <- data
  time     <- gsub("precin.*", NA, time)

  data         <- data.frame(precinct,
                             time,
                             stringsAsFactors = FALSE)
  data$action   <- gsub(".*:[0-9]* +", "", data$time)
  data$time     <- gsub(" .*", "", data$time)
  data$time     <- lubridate::mdy_hms(data$time)
  data$hour     <- lubridate::hour(data$time)

  data$precinct <- zoo::na.locf(data$precinct,
                                na.rm = FALSE)
  data$ward     <- gsub(".* (.*)-.*", "\\1", data$precinct)
  data$district <- gsub(".*-", "", data$precinct)
  data          <- data[!is.na(data$time),]
  data$precinct <- NULL


}
