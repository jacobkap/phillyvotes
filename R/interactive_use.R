# This is the part where the "public" (i.e. not a programmer) uses
# the function. It has an interactive part where it asks for them
# to select the folder with the voting files.

philadelphia_votes <- function() {
  cat(paste("Please navigate to the folder with the voting files and select",
      "the files that you want. NOTE: selecting PDF files that are not voting",
      "files may give wrong data!",
      "\n\nA window should have opened where you can select the files from. If you",
      "don't see it, try minimizing open windows on your screen (such as R).\n"))
  files <- tcltk::tk_choose.files(default = "", caption = "Select files",
                           multi = TRUE, filters = NULL, index = 1)
  files <- files[grep("\\.PDF$", files, ignore.case = TRUE)]

  results <- data.frame()
  message(paste0("\nCollecting and organizing data now.\n\n",
                "This process may take up to 15 minutes. Please be patient. ",
                "Thank you."))

  # Sets up progress bar
  pb = txtProgressBar(min = 0, max = length(files), initial = 0)
  for (i in seq_along(files)) {
    results <- dplyr::bind_rows(results, philly_votes(files[i]))
    setTxtProgressBar(pb, i)
  }
  rownames(results) <- 1:nrow(results)
  return(results)
}

 all_votes <- philadelphia_votes()
