shinyServer(function(input, output) {

  output$results_plot <- renderPlot({

    results_barplot(data = all_votes, office = input$results_office,
                    percent = input$percent,
                    top_n = input$num_of_results)
  })

  output$num_voters_graph <- renderPlot({
    num_selected_graph(data = all_votes, office = input$num_office)
  })

  output$conditional_table <- function() {
    cond_table = conditional_table(all_votes, input$cond_office)
    kablize(cond_table$results, cond_table$results_percent, input$cond_office)
  }

  output$cond_legend <- renderPlot({
    make_legend()
  })
})
