# cond_table_final(all_votes, "Judge of the Commonwealth Court-Dem")

cond_table_final <- function(data, category) {
  cond_table <- conditional_table(data, category)
  kablize(cond_table$results, cond_table$results_percent, category)
}

conditional_table <- function(data, categories) {
  votes <- data %>% dplyr::filter(category %in% categories)
    #,tolower(candidate) != "write in"

  votes$candidate <- gsub(" ", "_", votes$candidate)
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
  unique_votes <- unique_votes[, col_names]
  for (i in 1:nrow(results)) {
    results[i, 2:ncol(results)] <-
      unique_votes %>%
      filter(get(names(unique_votes)[i]) == 1)  %>%
      summarise_all(.funs = sum)
  }
  names(results) <- c("", trimws(gsub("candidate_", " ",
                                      names(unique_votes))))


  # Shows how many people that voter voted for
  candidates <- names(unique_votes)
  unique_votes$total_votes <-  rowSums(unique_votes)

  # Shows how many votes the candidate got among those who only vote for that
  # candidate
  for (temp_cand in candidates) {
    temp_cand_short <- gsub("candidate_", "", temp_cand)
    temp_row <- which(results[, 1] == temp_cand_short)
    temp_col <- which(names(results) == temp_cand_short)
    results[temp_row, temp_col] <- unique_votes %>%
      filter(total_votes == 1) %>%
      select(temp_cand) %>%
      sum()
  }

  cand_names <- results[, 1]
  results[, 1] <- NULL
  col_sums <- colSums(results)
  results_percent <- results
  for (i in 1:ncol(results_percent)) {
    results_percent[, i] <- round(results_percent[, i] /
                                    col_sums[i], 3) * 100
  }



  results[, 1] <- gsub("_", " ", results[, 1])
  names(results) <- gsub("_", " ", names(results))
  cand_names <- gsub("_", " ", cand_names)
  rownames(results) <- cand_names


  # prettifies results numbers - adds commas where appropriate
  results[] <- sapply(results,  prettyNum, big.mark = ",")
  return(setNames(list(results, results_percent),
                  c("results", "results_percent")))

}

cont_to_categories <- function(.data) {
  temp <- .data
  temp[.data <= 20] <- "0-20"
  temp[.data > 20 & .data <= 40] <- "21-40"
  temp[.data > 40 & .data <= 60] <- "41-60"
  temp[.data > 60 & .data <= 80] <- "61-80"
  temp[.data > 80 & .data <= 100] <- "81-100"
  .data <- temp
  .data <- factor(.data, levels = c("0-20",
                                    "21-40",
                                    "41-60",
                                    "61-80",
                                    "81-100"),
                  labels = c("#ffffff",
                             "#bae4b3",
                             "#74c476",
                             "#31a354",
                             "#006d2c"))
  return(.data)
}

kablize <- function(results, results_percent, categories) {
  for (i in 1:ncol(results)) {
    results[, i] <- kableExtra::cell_spec(results[, i],
                                          format = "html",
                                          background = cont_to_categories(results_percent[, i]),
                                          color = ifelse(results_percent[, i] > 60, "white", "black"),
                                          font_size = 20)
  }

  header <- c(1, ncol(results))
  names(header) <- c(" ", categories)

  knitr::kable(results, "html", escape = F) %>%
    kableExtra::kable_styling("striped", full_width = F) %>%
    kableExtra::kable_styling(bootstrap_options =
                                c("striped", "hover")) %>%
    kableExtra::add_header_above(header) %>%
    kableExtra::footnote(general =
                           paste0("Cells are color-coded by what proportion ",
                                          "of voters in that column voted for the ",
                                          "row's candidate. For example, a dark green ",
                                          "in row 1 column 2 means that many people ",
                                          "who voted for the candidate in column 2 ",
                                          "also voted for the candidate in row 1"))


}


make_legend <- function() {
legend_data <- data.frame(Shading = c("0-20%",
                                      "21-40%",
                                      "41-60%",
                                      "61-80%",
                                      "81-100%"),
                          colors = c("#ffffff",
                                     "#bae4b3",
                                     "#74c476",
                                     "#31a354",
                                     "#006d2c"))
my_hist <- ggplot(legend_data, aes(colors, fill = Shading)) +
  geom_bar() +
  scale_fill_manual(values = c("#ffffff",
                               "#bae4b3",
                               "#74c476",
                               "#31a354",
                               "#006d2c"),
                    name = "% Common Vote") +
  theme_bw(base_size = 25) +
  theme(legend.direction = "horizontal")



#Extract Legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- g_legend(my_hist)
grid.draw(legend)
grid.arrange(legend,
             heights = c(1, 1),widths = c(3,4,-4))
}
