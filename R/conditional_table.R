
conditional_table <- function(all_data, data) {

  fix_cap <- function(words) {
    words <- tolower(words)
    words <- strsplit(words, " ")[[1]]
    words <- paste(toupper(substring(words, 1,1)),
                   substring(words, 2),
                   sep = "", collapse = " ")
    return(words)
  }

  # Makes sure all candidates are an option even if they received no votes
  all_data <- all_data[!duplicated(paste(all_data$category, all_data$candidate)),]
  all_data$uniqueID <- paste("to_remove", 1:nrow(all_data))
  data <- bind_rows(all_data, data)

  data <- data[data$candidate != "Write In", ]


  data$candidate <- gsub(" ", "_", data$candidate)
  data$candidate <- paste(data$candidate, data$category)
  data$candidate <- gsub("\\s", "_", data$candidate)
  data <- fastDummies::dummy_cols(data, select_columns = "candidate")

  candidate_cols <- names(data)[grepl("candidate_", names(data))]

  # Subtracts 1 to control for the added vote from up top
  for (col in candidate_cols) {
    data[grep("to_remove", data$uniqueID), col] <- 0
  }

  unique_votes <- data %>%
    group_by(uniqueID) %>%
    summarise_at(candidate_cols,
                 .funs = "sum")
  unique_votes$uniqueID <- NULL

  # Shows how many people that voter voted for
  unique_votes$total_votes <-  rowSums(unique_votes)
  max_possible_votes <- max(unique_votes$total_votes)

  # if (any(num_choices != "All")) {
  #   unique_votes <-
  #     unique_votes %>%
  #     filter(total_votes <= max(num_choices))
  # }

  names(unique_votes) <- gsub("\\s", "_", names(unique_votes))


  # Make the table
  results <- data.frame(matrix(nrow = length(candidate_cols),
                               ncol = length(candidate_cols) + 1 ))
  # Sort the candidates by voting counts (in decreasing order)
  # To be used as rownames for the table
  name_col_order <- names(sort(table(data$candidate), decreasing = TRUE))

  # Puts the candidates in order by most common office then within each office
  # by most common candidate. Most common office is based on which office
  # has the most common candidate
  most_common_office <- unique(data$category[data$candidate == name_col_order[1]])
  other_office <- unique(data$category[data$category != most_common_office])
  name_col_order <- names(sort(table(data$candidate[data$category == most_common_office]),
                               decreasing = TRUE))
  name_col_order <- c(name_col_order,
                      names(sort(table(data$candidate[data$category != most_common_office]),
                                 decreasing = TRUE)))
  results[,1] <- name_col_order


  # Puts the voting data set into the proper order - winner, 2nd place, etc.
  col_names <- c(results[, 1], unique(data$candidate[!data$candidate %in% results[, 1]]))
  col_names <- paste0("candidate_", col_names)
  unique_votes <- unique_votes[, col_names]
  for (i in 1:nrow(results)) {
    results[i, 2:ncol(results)] <-
      unique_votes %>%
      filter(get(names(unique_votes)[i]) == 1)  %>%
      summarise_all(.funs = sum)
  }


  names(results) <- c("", trimws(gsub("candidate_", " ",
                                      names(unique_votes))))

  candidates <- names(unique_votes)


  # Shows how many votes the candidate got among those who only vote for that
  # candidate
  unique_votes$total_votes <-  rowSums(unique_votes)
  for (temp_cand in candidates) {
    temp_cand_short <- gsub("candidate_", "", temp_cand)
    temp_row <- which(results[, 1] == temp_cand_short)
    temp_col <- which(names(results) == temp_cand_short)

    temp <-
      unique_votes %>%
      filter(total_votes == 1) %>%
      select(temp_cand)
    if (nrow(temp) > 0) {
      temp <-
        temp %>%
        sum()
    } else { temp <- 0 }
    results[temp_row, temp_col] <- temp
  }

  cand_names <- results[, 1]
  results[, 1] <- NULL
  total_row <- data.frame(t(colSums(unique_votes)))
  total_row <- total_row[, -ncol(total_row)]
  names(total_row) <- names(results)


  # Blanks out the bottom triangle of the table
  results[lower.tri(results)] <- ""
  results <- rbind(results, total_row)



  results[, 1] <- gsub("_", " ", results[, 1])
  names(results) <- gsub("_", " ", names(results))
  cand_names <- gsub("_", " ", cand_names)
  rownames(results) <- c(cand_names, "Total")

  results <-
    results %>%
    mutate_all(as.character)

  #  prettifies results numbers - adds commas where appropriate
  results[] <- sapply(results,  prettyNum, big.mark = ",")
  results[] <- sapply(results,  trimws)
  results[] <- sapply(results,  function(x) ifelse(x == "NA", "", x))

  # Adds the candidate names as the first column
  names_col <- data.frame(c(names(results), "Total"),
                          stringsAsFactors = FALSE)
  names(names_col) <- ""
  results <- bind_cols(names_col, results)


    office_row <- vector(mode = "character", length = ncol(results))
    office_row[2] <- most_common_office
    office_row[office_row == ""] <- "to_remove"


    if (length(other_office) > 0) {
    office_row[length(unique(data$candidate[data$category ==
                                              most_common_office])) + 2] <- other_office
    }

    temp <- data.frame(t(names(results)), stringsAsFactors = FALSE)
    names(temp) <- names(results)
    results <- bind_rows(temp, results)
    names(results) <- office_row

  offices_to_remove <- paste0(" ", unique(data$category), collapse = "|")
  if (length(unique(data$category)) == 1) {
  names(results) <- gsub(offices_to_remove, "", names(results))
  }
  results[1, ] <- gsub(offices_to_remove, "", results[1, ])
  results[, 1] <- gsub(offices_to_remove, "", results[, 1])
  # results[1, 1] <- ""
  results <- janitor::clean_names(results)
  names(results) <- gsub("_", " ", names(results))

  names(results) <- gsub(" democrat", " - democrat", names(results))
  names(results) <- gsub(" republican", " - republican", names(results))
  names(results) <- gsub("the - ", "the ", names(results))
  results[1, ] <- gsub(" democrat", " - democrat", results[1, ])
  results[1, ] <- gsub(" republican", " - republican", results[1, ])

  names(results) <- sapply(names(results), fix_cap)

 # if (length(unique(data$category)) == 1) {
  #  results <- rbind(names(results), results)
#  }

  return(results)

}
