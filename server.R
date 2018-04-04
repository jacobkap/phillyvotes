shinyServer(function(input, output) {

test <- data.frame(cand = c("City Controller-Dem", "City Controller-Rep"),
                   val = c(5, 7))

# This dynamically changes the # of Candidate slider on the Election Results
# tab based on the actually max number of candidates for that ballot
# position.
  output$num_of_results <- renderUI({
    max_val <- results_prep$max_candidates[results_prep$Office ==
                                             input$results_office]

    real_value <- max_val
    if (real_value > 6) real_value <- 6

    sliderInput("num_of_results",
                label = h3("Number of candidates to show"), min = 1,
                max = max_val,
                value = real_value,
                step = 1)
  })

  output$results_plot <- renderPlot({
    if (is.null(input$num_of_results)) {
      top_n_value = 3
    } else {
      top_n_value = input$num_of_results
    }
    results_barplot(data = all_votes,
                    office = input$results_office,
                    percent = input$percent,
                    top_n = top_n_value)
  })



  output$num_voters_graph <- renderPlot({
    num_selected[[input$num_office]]
  })

  output$conditional_table <- function() {
    cond_table = cond_tables[[input$cond_office]]
    kablize(cond_table$results, cond_table$results_percent, input$cond_office)
  }

  output$cond_legend <- renderPlot({
    make_legend()
  })
})
