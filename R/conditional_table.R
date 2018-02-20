categories = c(             "JUDGE OF THE COMMONWEALTH COURT-DEM"
)
x = conditional_table(all_votes, categories)
y = conditional_table(all_votes, categories)
x
y
#cond
#save(cond, file = "cond.rda")

conditional_table <- function(data, categories) {
  votes <- data[data$category %in% categories, c("candidate", "category",
                                                         "uniqueID")]
  votes <- votes[!grepl("write in", votes$candidate, ignore.case = TRUE), ]
  votes <- fastDummies::dummy_cols(votes, select_columns = "candidate")

  candidate_cols <- names(votes)[grepl("candidate_", names(votes))]

  unique_votes <- votes %>%
    group_by(uniqueID) %>%
    summarise_at(candidate_cols,
                 .funs = "sum")
  unique_votes$uniqueID <- NULL
  names(unique_votes) <- gsub("\\s", "_", names(unique_votes))

  # Make the table
  results <- data.frame(matrix(nrow = length(candidate_cols),
                               ncol = length(candidate_cols) + 1 ))
  # Sort the candidates by voting counts (in decreasing order)
  # To be used as rownames for the table
  results[,1] <- names(sort(table(votes$candidate), decreasing = TRUE))


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
  cand_names <- results[, 1]
  results[, 1] <- NULL
  results[lower.tri(results)] <- NA
  results <- data.frame(cbind(cand_names, results))



  return(results)

}
