categories = c("CITY CONTROLLER-DEM",
             "DISTRICT ATTORNEY-DEM")
conditional_table(all_votes, categories)

conditional_table <- function(data, categories) {
  votes <- data[data$category %in% categories, c("candidate", "category",
                                                         "uniqueID")]
  votes <- fastDummies::dummy_cols(votes, select_columns = "candidate")
  votes <- votes[votes$candidate != "Write In", ]
  candidate_cols <- names(votes)[grepl("candidate_", names(votes))]

  unique_votes <- votes %>%
    group_by(uniqueID) %>%
    summarise_at(candidate_cols,
                 .funs = "sum")
  unique_votes$uniqueID <- NULL
  names(unique_votes) <- gsub("\\s", "_", names(unique_votes))

  # Make the table
  results <- data.frame(matrix(nrow = length(unique(votes$candidate[votes$category %in% categories[1]])),
                               ncol = length(unique(votes$candidate)) + 1 ))
  # Sort the candidates by voting counts (in decreasing order)
  # To be used as rownames for the table
  results[,1] <- names(sort(table(votes$candidate[votes$category %in% categories[1]]), decreasing = TRUE))


  # Puts the voting data set into the proper order - winner, 2nd place, etc.
  col_names <- c(results[, 1], unique(votes$candidate[!votes$candidate %in% results[, 1]]))
  col_names <- paste0("candidate_", col_names)
  col_names <- gsub(" ", "_", col_names)
  unique_votes <- unique_votes[, col_names]
  for (i in 1:nrow(results)) {
    results[i, 2:ncol(results)] <-
      unique_votes %>%
      filter(get(names(unique_votes)[i]) == 1)  %>%
      summarise_all(.funs = sum)
  }

  names(results) <- c("", trimws(gsub("candidate|_", " ",
                                               names(unique_votes))))

  return(results)

}
