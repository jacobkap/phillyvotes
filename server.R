shinyServer(function(input, output, session) {

  old_ward <- "All"
  output$results_map <- renderLeaflet({
    wards_map()
  })
  output$choices_map <- renderLeaflet({
    wards_map()
  })
  output$cond_table_map <- renderLeaflet({
    wards_map()
  })


  observe({
    # Update the select a ward dropdown!
    updateSelectizeInput(session,
                      inputId = "ward_choice_elect_results",
                      choices = c("All", 1:66),
                      selected = input$results_map_shape_click$id)
  })

  observe({
    # Update the select a ward dropdown!
    updateSelectizeInput(session,
                      inputId = "ward_choice_choices_per_office",
                      choices = c("All", 1:66),
                      selected = input$choices_map_shape_click$id)
  })
  observe({
    # Update the select a ward dropdown!
    updateSelectizeInput(session,
                      inputId = "ward_choice_cond_table",
                      choices = c("All", 1:66),
                      selected = input$cond_table_map_shape_click$id)
  })





  # This dynamically changes the # of Candidate slider on the Election Results
  # tab based on the actually max number of candidates for that ballot
  # position.
  output$num_of_results <- renderUI({
    max_val <- results_prep$max_candidates[results_prep$Office ==
                                             input$results_office]

    real_value <- max_val
    if (real_value > 6) real_value <- 6

    sliderInput("num_of_results",
                label = "Number of candidates to show",
                min = 1,
                max = max_val,
                value = real_value,
                step = 1)
  })

  output$cond_table_num_selected <- renderUI({

    max_val <- max_choices$max_choices[max_choices$office ==
                                             input$cond_office]

    sliderInput(
    'cond_table_num_selected',
    'Show only voters who voted for this many choices',
    min = 1,
    max = max_val,
    value = c(1, max_val),
    step = 1)
  })


  output$results_plot <-  renderPlot({
    if (is.null(input$num_of_results)) {
      top_n_value = 3
    } else {
      top_n_value = input$num_of_results
    }




    results_barplot(data = all_votes,
                    office = input$results_office,
                    percent = input$percent,
                    top_n = top_n_value,
                    location = input$ward_choice_elect_results)


  })



  output$num_voters_graph <- renderPlot({
    num_selected_graph(data = all_votes,
                       office = input$num_office,
                       location = input$ward_choice_choices_per_office)
  })

  output$conditional_table <- function() {

    cond_table_final(data = all_votes,
                     categories = input$cond_office,
                     location = input$ward_choice_cond_table,
                     num_choices = input$cond_table_num_selected)
#    cond_table = cond_tables[[input$cond_office]]
#    kablize(cond_table$results, cond_table$results_percent, input$cond_office)
  }

  output$cond_legend <- renderPlot({
    make_legend()
  })
})
