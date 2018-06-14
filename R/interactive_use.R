# This is the part where the "public" (i.e. not a programmer) uses
# the function. It has an interactive part where it asks for them
# # to select the folder with the voting files.
# main_folder    <- "C:/Users/user/Dropbox/R_project/phillyvotes/data/PDF_data"
# folders <- list.files(main_folder, full.names = TRUE)
# for (folder in folders[15:17]) {
#   philadelphia_votes(folder)
# }
philadelphia_votes <- function(folder) {
  setwd(folder)
  folder_name <- gsub(".*/20", "20", folder)
  message(folder_name)
  folder_name <- gsub(" ", "_", folder_name)
  folder_name <- paste0("election_", folder_name)
  files <- list.files(full.names = TRUE)

  files <- files[grep("\\.PDF$", files, ignore.case = TRUE)]
  files <- files[grep("BIR", files, ignore.case = TRUE)]

  # Sets up progress bar
  pb = txtProgressBar(min = 0, max = length(files), initial = 0)
  results <- data.table::data.table()
  for (i in seq_along(files)) {
    results <- data.table::rbindlist(list(results,
                                          data.table::data.table(philly_votes(files[i]))))
    setTxtProgressBar(pb, i)
  }
  results <- data.frame(results)

  rownames(results) <- 1:nrow(results)

  setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data/")
  assign(folder_name, results) # Change name
  save( list = folder_name,
        file = paste0(folder_name, ".rda"))
}


# # Check data
# setwd("C:/Users/user/Dropbox/R_project/phillyvotes/data/clean_data")
# files <- list.files()
# load(files[1])
# dim(election_2012_General)
# summary(election_2012_General)
