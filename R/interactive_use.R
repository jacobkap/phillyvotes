#
#main_folder    <- "C:/Users/user/Dropbox/R_project/phillyvotes/data/PDF_data"
# folders <- list.files(main_folder, full.names = TRUE)
# for (folder in folders) {
#   philadelphia_votes(folder)
# }
philadelphia_votes <- function(folder) {
  library(readr)
  library(dplyr)
  setwd(folder)
  folder_name <- gsub(".*/20", "20", folder)
  message("\n")
  message(folder_name)
  folder_name <- gsub(" ", "_", folder_name)
  folder_name <- paste0("election_", folder_name)
  BIR_name    <- paste0(tolower(folder_name), "_bir")
  CAR_name    <- paste0(tolower(folder_name), "_car")
  files <- list.files(full.names = TRUE)

  files <- files[grep("\\.PDF$", files, ignore.case = TRUE)]
  BIR_files <- files[grep("BIR", files, ignore.case = TRUE)]
  CAR_files <- files[grep("CAR", files, ignore.case = TRUE)]

  # Sets up progress bar
  pb = txtProgressBar(min = 0, max = length(BIR_files), initial = 0)
  BIR_results <- data.table::data.table()
  for (i in seq_along(BIR_files)) {
    message(BIR_files[i])
    BIR_results <- data.table::rbindlist(list(BIR_results,
                                              data.table::data.table(philly_votes(BIR_files[i]))))
    setTxtProgressBar(pb, i)
  }
  BIR_results <- data.frame(BIR_results)
  BIR_results <- remove_duplicate_voters(BIR_results)
  rownames(BIR_results) <- 1:nrow(BIR_results)

  # Removes all write-ins and court order, as requested.
  BIR_results <- BIR_results[grep("court order|write in", BIR_results$candidate,
                                  invert = TRUE, ignore.case = TRUE),]

  # This special election has too long a name so it's cut off partially.
  # This fixes it so it divides the name into the 3 offices as it should be.
  if (folder == "C:/Users/user/Dropbox/R_project/phillyvotes/data/PDF_data/2012 Primary") {
    BIR_results <- fix_2012_primary_special(BIR_results)
  }


  # Sets up progress bar
  pb = txtProgressBar(min = 0, max = length(CAR_files), initial = 0)
  CAR_results <- data.table::data.table()
  for (i in seq_along(CAR_files)) {
#    message(CAR_files)
    CAR_results <- data.table::rbindlist(list(CAR_results,
                                              data.table::data.table(scrape_CAR(CAR_files[i]))))
    setTxtProgressBar(pb, i)
  }
  CAR_results           <- data.frame(CAR_results)
  CAR_results           <- fix_CAR_time(CAR_results)
  rownames(CAR_results) <- 1:nrow(CAR_results)

  setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
  assign(BIR_name, BIR_results) # Change name
  save( list = BIR_name,
        file = paste0(BIR_name, ".rda"))

  assign(CAR_name, CAR_results) # Change name
  save( list = CAR_name,
        file = paste0(CAR_name, ".rda"))
}



remove_duplicate_voters <- function(data) {
  duplicates <-
    data %>%
    dplyr::group_by(file, uniqueID) %>%
    dplyr::summarize(num_files = dplyr::n())
  duplicates <- duplicates$uniqueID[duplicated(duplicates$uniqueID)]
  duplicates <- data[data$uniqueID %in% duplicates,]

  duplicates <-
    duplicates %>%
    dplyr::group_by(file, uniqueID)  %>%
    dplyr::summarize(num_rows = dplyr::n()) %>%
    dplyr::mutate(temp = paste(uniqueID, num_rows))
  duplicates <- duplicates[duplicated(duplicates$temp),]
  duplicates$uniqueID_with_pdf <- paste(duplicates$uniqueID, duplicates$file)


  data <-
    data %>%
    dplyr::filter(!uniqueID_with_pdf %in% duplicates$uniqueID_with_pdf )

  return(data)
}

remove_court_order_and_writein <- function(data) {
  data <- data[grep("court order|write in", data$candidate,
                    invert = TRUE, ignore.case = TRUE),]
  return(data)
}

fix_2012_primary_special <- function(data) {
  office <- "Special Election- Representative in the General Assembly"
  data$category[data$candidate %in% c("Ed Neilson",
                                      "David M Kralle")] <- paste0(office,
                                                                   " - 169th District")
  data$category[data$candidate %in% c("Barbara Hankinson",
                                      "Harold James")] <- paste0(office,
                                                                 " - 186th District")
  data$category[data$candidate %in% c("Gary Williams",
                                      "T Milton Street",
                                      "Steve Crum")] <- paste0(office,
                                                               " - 197th District")

  return(data)
}


fix_CAR_time <- function(data) {
  # Fixes issue where some machines start at 7PM (or around then)
  # instead of 7AM
  wrong_start_machines <-
    data %>%
    dplyr::group_by(serial) %>%
    dplyr::summarize(time = min(submit_time)) %>%
    dplyr::filter(lubridate::hour(time) > 18)

  if (nrow(wrong_start_machines) > 0) {
    data$submit_time[data$serial %in% wrong_start_machines$serial] <-
      data$submit_time[data$serial %in% wrong_start_machines$serial] - as.difftime(12, unit = "hours")
    data$start_time[data$serial %in% wrong_start_machines$serial] <-
      data$start_time[data$serial %in% wrong_start_machines$serial] - as.difftime(12, unit = "hours")
    data$hour                  <- lubridate::hour(data$submit_time)
    data$vote_time_seconds     <- as.numeric(data$submit_time - data$start_time)
    data$vote_time_minutes     <- data$vote_time_seconds / 60 # Makes the units be minutes
  }

  return(data)
}
