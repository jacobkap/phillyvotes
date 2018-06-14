
# results_barplot(all_votes, "Judge of the Superior Court - Democrat",
#                 location = 10)

results_barplot <- function(data,
                            office,
                            percent = FALSE,
                            top_n = 6,
                            location = "All") {

  if (location != "All") {
    data <-
      data %>%
      filter(ward == location)
  }

  df <-
    data %>% dplyr::filter(category == office) %>%
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

  title <- office
  if (location != "All") {
    title <- paste0(title, ", Ward ", location)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(candidate, votes),
                                        y = votes)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                      face = "bold")) +
    ggplot2::ylab(ylabel) +
    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "black",
                                                       size = 15)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = "black",
                                                       size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(text = ggplot2::element_text(size = 15))


  return(p)
}

#num_selected_graph(all_votes, office = "Judge of the Superior Court - Democrat",
#                   location = "All")
num_selected_graph <- function(data = NULL,
                               df = NULL,
                               office,
                               undervotes = TRUE,
                               location = NULL) {
  if (is.null(df)) {
    df <- data %>% dplyr::filter(category == office  &
                                   tolower(candidate) != "write in")

    if (location != "All") {
      df <-
        df %>%
        filter(ward == location)
    }
    df <-
      df %>%
      group_by(uniqueID) %>%
      summarize(count = n()) %>%
      group_by(count) %>%
      tally()
  }

  if (location != "All") {
    total_uniqueIDs <-
      data %>%
      filter(ward == location)
    total_uniqueIDs <- length(unique(total_uniqueIDs$uniqueID))
  } else {
    total_uniqueIDs <- length(unique(data$uniqueID))
  }


  zero_choices <- tibble(count = 0,
                         n = (total_uniqueIDs - sum(df$n)))
  if (undervotes) {
    df <- dplyr::bind_rows(df, zero_choices)
  }

  df$percent <- round((df$n / sum(df$n) * 100))

  title = office
  if (location != "All") {
    title <- paste0(title, ", Ward ", location)
  }

  p <-  ggplot2::ggplot(data = df, ggplot2::aes(x = reorder(count, -percent), y = percent)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title, subtitle = paste0("Max number of selections: ", max(df$count))) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                      face = "bold")) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 15,
                                                         face = "bold")) +
    ggplot2::xlab("Number of Selections Made") +
    ggplot2::ylab("%") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%")),
                       color = "white", vjust = 1.4, size = 8.5) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "black", size = 16)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = "black", size = 16))

  return(p)

}
