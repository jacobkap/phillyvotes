

scrape_CAR <- function(file) {
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

  data$action   <- tolower(data$action)
  data$action   <- gsub("ada voter enabled", "voter enabled", data$action)
  data          <- data[data$action %in% c("voter cast ballot", "voter enabled"), ]
  data          <- data[!is.na(data$time),]
  data$time     <- lubridate::mdy_hms(data$time)


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
                  year)

  new_record_rows <- grep("voter cast ballot", data$action, ignore.case = TRUE)
  new_record_rows <- new_record_rows[data$action[(new_record_rows - 1)] == "voter enabled"]

  data_rows <- c(new_record_rows - 1, new_record_rows)
  data_rows <- sort(data_rows)
  data      <- data[data_rows, ]
  rownames(data) <- 1:nrow(data)


  if (data$action[1] == "voter cast ballot") {
    data <- data[-1, ]
  }
  if (data$action[nrow(data)] == "voter enabled") {
    data <- data[-nrow(data), ]
  }

  start_data         <- data[data$action %in% "voter enabled", ]
  names(start_data)  <- gsub("time", "start_time", names(start_data))
  names(start_data)  <- gsub("action", "start_action", names(start_data))
  submit_data        <- data[data$action %in% "voter cast ballot", ]
  names(submit_data) <- gsub("time", "submit_time", names(submit_data))
  names(submit_data) <- gsub("action", "submit_action", names(submit_data))


  if (all(start_data$serial != submit_data$serial)) {
    message("\nSerials not all same")
  }
  if (all(start_data$precinct != submit_data$precinct)) {
    message("Location not all same")
  }
  submit_data                <- submit_data[, c("submit_time",
                                                "submit_action")]

  data                       <- cbind(start_data, submit_data)
  data$hour                  <- lubridate::hour(data$submit_time)
  data$vote_time_seconds     <- as.numeric(data$submit_time - data$start_time)
  data$vote_time_minutes     <- data$vote_time / 60 # Makes the units be minutes

  return(data)

}
