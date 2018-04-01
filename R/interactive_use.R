# This is the part where the "public" (i.e. not a programmer) uses
# the function. It has an interactive part where it asks for them
# to select the folder with the voting files.

philadelphia_votes <- function(folder = NULL) {
  if (is.null(folder)) {
    cat(paste("Please navigate to the folder with the voting files and select",
              "the files that you want. NOTE: selecting PDF files that are not voting",
              "files may give wrong data!",
              "\n\nA window should have opened where you can select the files from. If you",
              "don't see it, try minimizing open windows on your screen (such as R).\n"))
    files <- tcltk::tk_choose.files(default = "", caption = "Select files",
                                    multi = TRUE, filters = NULL, index = 1)
  } else {
    files <- list.files(folder, full.names = TRUE)
  }
  files <- files[grep("\\.PDF$", files, ignore.case = TRUE)]

  message(paste0("\nCollecting and organizing data now.\n\n",
                 "This process may take up to 20 minutes. ",
                 "Please be patient. Thank you."))

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
  return(results)
}
# folder = "C:/Users/user/Dropbox/R_project/phillyvotes/data"
# all_votes <- philadelphia_votes(folder)
# all_votes <- all_votes[!all_votes$category %in%
#                          unique(grep("INSPECTOR|JUDGE OF ELECTION",
#                                  unique(all_votes$category),
#                                                      value = TRUE,
#                                  ignore.case = TRUE)), ]
#  all_votes <- all_votes[!is.na(all_votes$category), ]
#  all_votes <- all_votes[, c("category", "candidate", "uniqueID")]
#  save(all_votes, file = "all_votes.rda")

# JUDGE OF THE MUNICIPAL COURT-REP
# This produces a vector of the names of offices where a person can vote for
# more than one candidate - useful for making the options a user can select
# for the num_selected_graph
num_selected_prep <- function(data) {
  num_selected <- list()
  for (office in unique(data$category)) {
    temp <- data %>% dplyr::filter(category == office  &
                                     tolower(candidate) != "write in") %>%
      group_by(uniqueID) %>%
      summarize(count = n()) %>%
      group_by(count) %>%
      tally()
    if (nrow(temp) > 1) {
      num_selected[[length(num_selected) + 1]] <-
        num_selected_graph(df = temp,
                           office = office)
      names(num_selected)[length(num_selected)] <- office
    }
  }
  return(num_selected)
}
# num_selected <- num_selected_prep(all_votes)
# save(num_selected, file = "num_selected.rda")



# setwd("C:/Users/user/Dropbox/R_project/phillyvotes/shiny_data")
# cond_tables <- cond_table_prep(all_votes)
# save(cond_tables, file = "cond_tables.rda")
cond_table_prep <- function(data) {
  cond_tables <- list()
  for (office in names(num_selected)) {
    cond_table <- conditional_table(data, office)

    cond_tables[[length(cond_tables) + 1]] <- cond_table
    names(cond_tables)[length(cond_tables)] <- office
  }
  return(cond_tables)
}

