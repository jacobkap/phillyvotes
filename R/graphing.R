#
# results_barplot(data = all_votes,
#                 office = "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 percent = TRUE)
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM")
#
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 percent = TRUE, top_n = 3)
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 top_n = 3)
#
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 percent = FALSE, top_n = 3)
# results_barplot(all_votes, "JUDGE OF THE COURT OF COMMON PLEAS-DEM",
#                 top_n = 15)

results_barplot <- function(data, office,
                            percent = FALSE,
                            top_n = 6) {



  df <- data %>% dplyr::filter(category == office) %>%
    dplyr::group_by(candidate) %>%
    dplyr::summarise(votes = n()) %>%
    dplyr::arrange(desc(votes))

  total_votes <- sum(df$votes)
  df <- df %>% dplyr::top_n(votes, n = top_n)
  df$pretty_votes <- prettyNum(df$votes, big.mark = ",")

  ylabel <- "# of Votes"
  if (percent)  {
    df$votes <- round((df$votes / total_votes * 100))
    ylabel <- "% of Votes"
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(candidate, votes), y = votes)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(office) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                      face = "bold")) +
    ggplot2::ylab(ylabel) +
    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "black", size = 16)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = "black", size = 16))


  if (percent) {
    p + ggplot2::geom_text(ggplot2::aes(label = paste0(votes, "%")),
                           color = "white", hjust = 1.3,
                           size = 10)
  } else {
    p + ggplot2::geom_text(ggplot2::aes(label = pretty_votes),
                           color = "white", hjust = 1.3,
                           size = 9)
  }
  return(p)
}

#num_selected_graph(all_votes, "JUDGE OF THE SUPERIOR COURT-DEM")
num_selected_graph <- function(data = NULL, df = NULL, office) {
  if (is.null(df)) {
    df <- data %>% dplyr::filter(category == office  &
                                   tolower(candidate) != "write in") %>%
      group_by(uniqueID) %>%
      summarize(count = n()) %>%
      group_by(count) %>%
      tally()
  }

  df$percent <- round((df$n / sum(df$n) * 100))

  p <-  ggplot2::ggplot(data = df, ggplot2::aes(x = reorder(count, -percent), y = percent)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(office, subtitle = paste0("Max number of selections: ", max(df$count))) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                      face = "bold")) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 15,
                                                         face = "bold")) +
    ggplot2::xlab("Number of Selections Made") +
    ggplot2::ylab("%") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%")),
                       color = "white", vjust = 1.5, size = 7.5) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "black", size = 16)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = "black", size = 16))

  return(p)

}
