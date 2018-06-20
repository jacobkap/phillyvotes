# This is the part where the "public" (i.e. not a programmer) uses
# the function. It has an interactive part where it asks for them
# # to select the folder with the voting files.


# main_folder    <- "C:/Users/user/Dropbox/R_project/phillyvotes/data/PDF_data"
# folders <- list.files(main_folder, full.names = TRUE)
# for (folder in folders) {
#   philadelphia_votes(folder)
# }
philadelphia_votes <- function(folder) {
  library(readr)
  setwd(folder)
  folder_name <- gsub(".*/20", "20", folder)
  message(folder_name)
  folder_name <- gsub(" ", "_", folder_name)
  folder_name <- paste0("election_", folder_name)
  BIR_name    <- paste0(tolower(folder_name), "_bir")
  CAR_name    <- paste0(tolower(folder_name), "_car")
  files <- list.files(full.names = TRUE)

  files <- files[grep("\\.PDF$", files, ignore.case = TRUE)]
  BIR_files <- files[grep("BIR", files, ignore.case = TRUE)]
  CAR_files <- files[grep("CAR", files, ignore.case = TRUE)]

  # # Sets up progress bar
  # pb = txtProgressBar(min = 0, max = length(BIR_files), initial = 0)
  # BIR_results <- data.table::data.table()
  # for (i in seq_along(BIR_files)) {
  #   BIR_results <- data.table::rbindlist(list(BIR_results,
  #                                         data.table::data.table(philly_votes(BIR_files[i]))))
  #   setTxtProgressBar(pb, i)
  # }
  # BIR_results <- data.frame(BIR_results)
  # rownames(BIR_results) <- 1:nrow(BIR_results)


  # Sets up progress bar
  pb = txtProgressBar(min = 0, max = length(CAR_files), initial = 0)
  CAR_results <- data.table::data.table()
  for (i in seq_along(CAR_files)) {
    CAR_results <- data.table::rbindlist(list(CAR_results,
                                              data.table::data.table(scrape_CAR(CAR_files[i]))))
    setTxtProgressBar(pb, i)
  }
  CAR_results <- data.frame(CAR_results)
  rownames(CAR_results) <- 1:nrow(CAR_results)

   setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
  # assign(BIR_name, BIR_results) # Change name
  # save( list = BIR_name,
  #       file = paste0(BIR_name, ".rda"))
  # do.call("write_csv", list(as.name(BIR_name),
  #                           path = paste0(BIR_name, ".csv")))

  assign(CAR_name, CAR_results) # Change name
  save( list = CAR_name,
        file = paste0(CAR_name, ".rda"))
  do.call("write_csv", list(as.name(CAR_name),
                            path = paste0(CAR_name, ".csv")))
}


