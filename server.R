#========================================================================================================================================================
server <- function(input, output, session) {
  
  #Shiny manager
  #==================================
  #Check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  #Print shiny manager variables
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  #ValueBox (advanced)
  #==================================
  count <- reactiveVal(0)
  
  observeEvent(input$counter_button, {
    count(count() + 1)
    
    if (count() == "1") {
      toastr_success("Test123")
      
    } else if (count() == "2") {
      toastr_info("Test123")
      
    } else if (count() == "3") {
      toastr_warning("Test123")
      
    } else if (count() == "4") {
      toastr_error("Test123")
    }

    # tryCatch(
    #   {},
    #   error = function(e) {
    #      toastr_error(title = "Database error", conditionMessage(e))
    # }
    # )
  })
  
  callModule(
    valueBoxModule,
    "betterBox",
    value = count
  )
  
  #Sweet Alert
  #==================================
  observeEvent(input$ActionButton01, {
    sendSweetAlert(
      session = session,
      title = "Information",
      text = "Hi there!",
      type = "info"
    )
  })
  
  #Radar-Chart
  #==================================
  output$Plotly01 <- renderPlotly({
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'lines+markers',
      fill = 'toself'
      # r = ~value,
      # theta = ~key,
      # split = ~ean
    )
    fig <- fig %>%
      add_trace(
        r = c(39, 28, 8, 7, 28, 39),
        theta = c('A','B','C', 'D', 'E', 'A'),
        name = 'Group A'
      )
    fig <- fig %>%
      add_trace(
        r = c(1.5, 10, 39, 31, 15, 1.5),
        theta = c('A','B','C', 'D', 'E', 'A'),
        name = 'Group B'
      )
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,50)
          )
        )
      )
    
    Sys.sleep(2)
    
    fig
  })

  #Visualize selected rows
  #==================================
  output$DT01 = DT::renderDataTable({
    mtcars[, c('hp', 'mpg')]
  }, server = TRUE)
  
  output$Plotly02 = renderPlotly({
    
    mtcars2 = mtcars[, c('hp', 'mpg')]
    vRowsSelected = input$DT01_rows_selected
    vRowsCurrentPage = input$DT01_rows_current
    vRowsFiltered = input$DT01_rows_all    
    
    # Dynamic legend text
    # vSearchText = input$DT01_search
    # txt = if (is.null(vSearchText) || vSearchText == '') 'Filtered data' else {
    #   sprintf('Data matching "%vSearchText"', vSearchText)
    # }
    
    if (length(vRowsSelected)) {
      
      vPlot <- ggplot() +
        geom_point(mtcars2[-vRowsSelected, ], mapping = aes(x = hp, y = mpg), colour = "black", size = 2) +
        geom_point(mtcars2[vRowsSelected, ], mapping = aes(x = hp, y = mpg), colour = "red", size = 2) 
      
    } else {
      
      vPlot <- ggplot(mtcars2, aes(x = hp, y = mpg)) +
        geom_point(colour = "black", size = 2)
    }
    
    if (length(vRowsFiltered) > 0 && length(vRowsFiltered) < nrow(mtcars2)) {
      
      vPlot <- vPlot +
        geom_point(mtcars2[vRowsFiltered, ], mapping = aes(x = hp, y = mpg), colour = "red", size = 4, shape = 1) 
    }
      
    ggplotly(vPlot)
  })
  
  #Editable DT table
  #==================================
  x = reactiveValues(df = NULL)
  
  observe({
    df <- mtcars[, c('hp', 'mpg')]
    #df$Date = Sys.time() + seq_len(nrow(df))
    x$df <- df
  })

  output$DT02 = DT::renderDataTable({
    x$df
  }, selection = 'none', editable = TRUE) #rownames = FALSE
  
  proxy = dataTableProxy('DT02')
  
  observeEvent(input$DT02_cell_edit, {
    info = input$DT02_cell_edit
    str(info)
    i = info$row
    j = info$col #+1 (if rownames = FALSE)
    v = info$value
    x$df[i, j] <<- DT::coerceValue(v, x$df[i, j])
    replaceData(proxy, x$df, resetPaging = FALSE)  #rownames = FALSE
  })

  output$print <- renderPrint({
    x$df
  })
  
  #Busy Indicator
  #==================================
    observeEvent(input$uploadFilesBtn, {
      withBusyIndicatorServer("uploadFilesBtn", {
        Sys.sleep(1)
        if (input$select == "error") {
          stop("choose another option")
        }
      })
    })

  #Formattable DT
  #staticRender_cb <- JS('function(){debugger;HTMLWidgets.staticRender();}') 
  #==================================
  output$DT03 = renderUI({
    
    iris$NewFeature_Line =  apply(iris[, 1:4], 1, FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "line"))))
    iris$NewFeature_Bar =  apply(iris[, 1:4], 1, FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "bar"))))
    
    t_Formattable <- formattable(head(iris), 
                                 align = c("l",rep("r", NCOL(iris) - 1)),
                                 list(`Species` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                                      `Sepal.Width` = formatter("span", 
                                                                x ~ icontext(ifelse(x > 3.5, "ok", "remove"), ifelse(x > 3.5, "Yes", "No")), 
                                                                style = x ~ style(color = ifelse(x > 3.5, "green", "red"))),
                                      `Petal.Length` = color_bar("#FA614B"), 
                                      `Petal.Width` = formatter("span", 
                                                                style = x ~ style(color = ifelse(x > 0.3, "green", "red"))),
                                      `Sepal.Length` = formatter("span",
                                                                 style = x ~ style(color = ifelse(x > 5, "green", "red")),                                    
                                                                 x ~ icontext(sapply(x, function(x) if (x > 5) "arrow-up" else if (x <= 5) "arrow-down" else ""), x))))
    
    Table <- t_Formattable %>% 
      format_table() %>%
      htmltools::HTML() %>%
      div() %>%
      spk_add_deps() #%>%
      #{column(width=6, .)}
    
    Table
    
    #out = as.htmlwidget(t_Formattable)
    #out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
    #out

    #=======
    #Different Plot types
    # df = data.frame("Type" = c("bar", "line", "bullet", "pie", "tristate", "discrete"),
    #                 Sparkline = c(as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bar"))), 
    #                               as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "line"))), 
    #                               as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bullet"))), 
    #                               as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "pie"))), 
    #                               as.character(htmltools::as.tags(sparkline(c(-1,0,1,1,1,-1,0,2), type = "tristate"))), 
    #                               as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "discrete")))))
    # out = as.htmlwidget(formattable(df))
    # out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
    #out
    
    #=======
    #Converting to DT
    #as.datatable()
    
  })
  
  #Interactive k-Means Clustering
  #==================================
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$PlotKMeans <- renderPlot({
    
    if (!is.na(input$clusters)) {
      
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    }
    
  })
  
  #Loading Button
  #==================================
  # reset the loadingButton to its active state after 3 seconds
  observeEvent(input$myLoadingButton, {
    Sys.sleep(3)
    resetLoadingButton("myLoadingButton")
  })
  
  observeEvent(input$myCancelButton, {
    Sys.sleep(3)
    resetLoadingButton("myCancelButton")
  })
  
  #Feedback
  #==================================
  observeEvent(input$myInput, {
    
    if (nchar(input$myInput) > 3) {
      
      #showFeedback(
      #showFeedbackSuccess(
      #showFeedbackDanger(
      showFeedbackWarning(
        inputId = "myInput",
        text = "too many chars",
        icon = shiny::icon("warning-sign", lib = "glyphicon")
        #icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
        #icon = shiny::icon("ok", lib = "glyphicon")
      )  
    } else {
      hideFeedback("myInput")
    }
    
  })
  
  #==================================
  # output$InfoBox01 <- renderInfoBox({
  #   infoBox("Test123", value = 10, subtitle = "Test",
  #           icon = shiny::icon("bar-chart"), color = "aqua", width = 4,
  #           href = NULL, fill = FALSE)
  #   })
  # 
  # output$ValueBox01 <- renderValueBox({
  #   valueBox(
  #     width = 2,
  #     value = "5.00",
  #     subtitle = NULL,  
  #     icon = tags$i(class = "fas fa-thumbs-up", style="font-size: 32px; color: white") #thumbs-up can be replaced with other icons (e.g. robot)
  #   )
  # })
    
  # addTooltip(session, id = 'TestPanel02', title = "Lets delay this appearing for 1s and force disappear after 5s",
  #            placement = "bottom", trigger = "hover") #options = list(delay = list(show=0, hide=3000)))
  
  #==================================
}

#=======================================================================================================================================================================