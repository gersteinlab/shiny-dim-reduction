# The goal of this app is to perform dimensionality reduction.
# setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
# source("pipeline.R", encoding="UTF-8")

source("options.R", encoding="UTF-8")

# ------------------------------------------
# REACTIVE SERVER: INITIALIZATION, OBSERVERS
# ------------------------------------------

server <- function(input, output, session) {
  # pushes the subtitle to the right
  shinyjs::addClass(id = "central_nav", class = "navbar-right")
  shinyjs::hide("legend_out_spin")
  
  # performs setup for authentication
  auth_default <- 1
  authenticated <- reactiveVal(auth_default)
  if (!auth_default)
    showModal(authenticator_modal())
  shinyjs::runjs(no_autofill)
  addClass("password", "my-hidden-text")
  
  # if the user attempts to log in ...
  observeEvent(input$attempt_login, {
    notif("Attempting authentication ...", 1, "default")
    
    if (length(input$username) == 1 && 
        (input$username %in% names(user_credentials))
        && checkpw(input$password, user_credentials[[input$username]]))
    {
      notif("Authentication was successful - welcome!", 3, "message")
      removeModal()
      authenticated(1)
    }
    else
    {
      Sys.sleep(0.3)
      notif("Authentication was unsuccessful.", 2, "error")
    }
  })
  
  # toggles password visibility
  observeEvent(input$toggle_password, {
    if (input$toggle_password %% 2 == 1)
    {
      updateTextInput(session, "password",
                      label="Password (is visible)",
                      placeholder = "Please enter your password ...")
      removeClass("password", "my-hidden-text")
    }
    else
    {
      updateTextInput(session, "password",
                      label = "Password (is invisible)",
                      placeholder = "")
      addClass("password", "my-hidden-text")
    }
  })
  
  # shows instructions
  observeEvent(input$instructions, {
    showModal(modalDialog(
      title = HTML("<b>Instructions</b>"), 
      HTML(instructions)
    ))
  })
  
  # shows citations
  observeEvent(input$citations, {
    showModal(modalDialog(
      title = HTML("<b>Citations</b>"), 
      HTML(citations)
    ))
  })
  
  # toggles legend table
  observeEvent(input$sMenu, {
    if ("Embed Legend" %in% input$sMenu)
      shinyjs::hide("legend_out_spin")
    else
      shinyjs::show("legend_out_spin")
  })
  
  # constants for reactive plotting
  default <- 1
  
  running <- reactiveVal(default)
  if (default)
    shinyjs::hide("start")
  else
    shinyjs::hide("stop")
  
  # starts the reactive plotting algorithm
  observeEvent(input$start, {
    shinyjs::show("stop")
    running(1)
    shinyjs::hide("start")
  })
  
  # suspends the reactive plotting algorithm
  observeEvent(input$stop, {
    shinyjs::show("start")
    running(0)
    shinyjs::hide("stop")
    
    if (input$plotPanels == "ggplot2")
      ggplot2_current(ggplot2_data())
    if (input$plotPanels == "plotly2")
      plotly2_current(plotly2_data())
    if (input$plotPanels == "plotly3")
      plotly3_current(plotly3_data())
    if (input$plotPanels == "boxplot")
      beeswarm_current(beeswarm_data())
    legend_current(legend_data())
  })
  
  # manages height and numPlots
  numPlots <- reactiveVal(1)
  height <- reactive({
    if (range_invalid(input$height, 1, 4000))
    {
      notif("Warning: Graph height is not in [1, 4000].", 6, "warning")
      return(graph_height)
    }
    
    round(input$height, digits=0)
  })
  
  # validates input
  upse_feat <- reactive({
    if (range_invalid(input$set_feat_upse, pc_cap, 2^24))
    {
      notif("Warning: Maximum Features is not in [pc_cap,2^24].", 6, "warning")
      return(max_upse)
    }
    
    round(input$set_feat_upse, digits=0)
  })
  
  # validates input
  heat_feat <- reactive({
    if (range_invalid(input$set_feat_heat, pc_cap, 2^24))
    {
      notif("Warning: Maximum Features is not in [pc_cap,2^24].", 6, "warning")
      return(max_heat)
    }
    
    round(input$set_feat_heat, digits=0)
  })
  
  # validates input
  dend_feat <- reactive({
    if (range_invalid(input$set_feat_dend, pc_cap, 2^24))
    {
      notif("Warning: Maximum Features is not in [pc_cap,2^24].", 6, "warning")
      return(max_dend)
    }
    
    round(input$set_feat_dend, digits=0)
  })
  
  # validates input
  observeEvent(input$set_f1, {
    if (range_invalid(input$set_f1, 0, 1))
      notif("Warning: Fraction of Samples is not in [0,1].", 6, "warning")
  })
  
  # validates input
  observeEvent(input$set_f2, {
    if (range_invalid(input$set_f2, 0, num_filters))
      notif("Warning: Number of Characteristics is not in [0,num_filters].", 6, "warning")
  })
  
  # -------
  # OUTPUTS
  # -------
  
  # renders ggplot2 output
  output$ggplot2UI <- renderUI({
    plotOutput("ggplot2_out", width="100%", height=height()) %>% my_spin()
  })
  
  # renders plotly2 output
  output$plotly2UI <- renderUI({
    plotlyOutput("plotly2_out", width="100%", height=height()) %>% my_spin()
  })
  
  # renders plotly3 output
  output$plotly3UI <- renderUI({
    plotlyOutput("plotly3_out", width="100%", height=height()) %>% my_spin()
  })
  
  # renders beeswarm output
  output$beeswarmUI <- renderUI({
    plotOutput("beeswarm_out", width="100%", height=height()) %>% my_spin()
  })
  
  # renders the numeric data table reactively
  output$num_data_table <- renderDT({
    if (!authenticated())
      return(my_datatable(NULL))
    
    my_datatable(data.frame(downloadData()))
  })
  
  # renders the metadata table interactively
  output$metadata_table <- renderDT({
    if (!authenticated())
      return(my_datatable(NULL))
    
    my_datatable(data.frame(order()[keep(),]))
  })
  
  # renders the title in accessible plain text
  output$plainTitleUI <- renderUI({
    if (title_access())
      return("")
    return(HTML(sprintf("<h3><b>Title:</b> %s</h3>", title_text())))
  })
  
  # renders a non-embedded legend nicely
  legend_current <- reactiveVal()
  output$legend_out <- renderDT({
    if (!authenticated() || legend()) 
      return(NULL)
    
    if (running())
      return(my_datatable(legend_data()))
    else
      return(my_datatable(legend_current()))
  })
  
  # renders the ggplot2 data reactively
  ggplot2_current <- reactiveVal()
  output$ggplot2_out <- renderPlot({
    if (!authenticated())
    {
      downloadData(NULL)
      return(ggplot2_null())
    }
    
    num <- isolate(numPlots())
    numPlots(num+1)
    
    if (notify())
      plot_start(num)
    start <- my_timer()
    
    if (running())
      target <- ggplot2_data()
    else
      target <- ggplot2_current()
    
    if (is.null(target))
    {
      if (notify())
        plot_fail()
      return(ggplot2_null())
    }
    
    if (notify())
      plot_success(my_timer(start))
    target
  })
  
  # renders the plotly2 data reactively
  plotly2_current <- reactiveVal()
  output$plotly2_out <- renderPlotly({
    if (!authenticated())
    {
      downloadData(NULL)
      return(ggplot2_null())
    }
    
    num <- isolate(numPlots())
    numPlots(num+1)
    
    if (notify())
      plot_start(num)
    start <- my_timer()
    
    if (running())
      target <- plotly2_data()
    else
      target <- plotly2_current()
    
    if (is.null(target))
    {
      if (notify())
        plot_fail()
      return(ggplot2_null())
    }
    
    if (notify())
      plot_success(my_timer(start))
    target
  })
  
  # renders the plotly3 data reactively
  plotly3_current <- reactiveVal()
  output$plotly3_out <- renderPlotly({ 
    if (!authenticated())
    {
      downloadData(NULL)
      return(ggplot2_null())
    }
    
    num <- isolate(numPlots())
    numPlots(num+1)
    
    if (notify())
      plot_start(num)
    start <- my_timer()
    
    if (running())
      target <- plotly3_data()
    else
      target <- plotly3_current()
    
    if (is.null(target))
    {
      if (notify())
        plot_fail()
      return(ggplot2_null())
    }
    
    if (notify())
      plot_success(my_timer(start))
    target
  })
  
  # renders the beeswarm data reactively
  beeswarm_current <- reactiveVal()
  output$beeswarm_out <- renderPlot({
    if (!authenticated())
      return(ggplot2_null())
    
    num <- isolate(numPlots())
    numPlots(num+1)
    
    if (notify())
      plot_start(num)
    start <- my_timer()
    
    if (running())
      target <- beeswarm_data()
    else
      target <- beeswarm_current()
    
    if (is.null(target))
    {
      if (notify())
        plot_fail()
      return(ggplot2_null())
    }
    
    if (notify())
      plot_success(my_timer(start))
    target
  })
  
  # download button for data
  downloadData <- reactiveVal()
  output$downloadData <- downloadHandler(
    filename = function() {
      sprintf("%s_num_data.csv", repStr(title_text(), " ", "_"))
    },
    content = function(file) {
      if (!authenticated())
        return(NULL)
      write.csv(downloadData(), file)
    }
  )
  
  # download button for metadata
  output$downloadMetadata <- downloadHandler(
    filename = function() {
      sprintf("%s_metadata.csv", repStr(title_text(), " ", "_"))
    },
    content = function(file) {
      if (!authenticated())
        return(NULL)
      write.csv(order()[keep(),], file)
    }
  )
  
  # download button for instructions
  output$downloadInstructions <- downloadHandler(
    filename = function(){
      "instructions.txt"
    },
    content = function(file){
      writeLines(regStr(instructions, "<[^>]*>", ""), file)
    }
  )
  
  # download button for citations
  output$downloadCitations <- downloadHandler(
    filename = function(){
      "citations.txt"
    },
    content = function(file){
      writeLines(regStr(citations, "<[^>]*>", ""), file)
    }
  )
  
  # ----------------
  # INPUT PROCESSING
  # ----------------
  
  # Reactive variables corresponding to parsed input
  title_access <- reactive("Embed Title" %in% input$sMenu)
  legend <- reactive("Embed Legend" %in% input$sMenu)
  boost <- reactive("Boost Graphics" %in% input$sMenu)
  notifq <- reactive("Notifications" %in% input$sMenu)
  not_rev <- reactive("Uninverted Colors" %in% input$sMenu)
  
  subi <- reactive(parse_opt(input[[sprintf("subsetby_%s", input$category)]]))
  feat <- reactive(rem_perc(input$features))
  per_ind <- reactive(which(as.character(perplexity_types) == input$perplexity))
  notify <- reactive(notifq() && running())
  
  # filter-related reactives from user input selections
  order <- reactive(order_total[[input$category]])
  colorby <- reactive(input[[sprintf("colorby_%s", input$category)]])
  shapeby <- reactive(input[[sprintf("shapeby_%s", input$category)]])
  labelby <- reactive(input[[sprintf("labelby_%s", input$category)]])
  filterby <- reactive(input[[sprintf("filterby_%s", input$category)]])
  thre_ind <- reactive({
    which(thre_seqs[[input$scale]][[input$category]] == 
            input[[get_thre(input$category, input$scale)]])
  })
  
  # reactives that follow from filter-related reactives
  colors <- reactive(order()[keep(), colorby()])
  shapes <- reactive(order()[keep(), shapeby()])
  labels <- reactive(sprintf("%s: %s", labelby(), order()[keep(), labelby()]))
  my_chars <- reactive(parse_opt(input[[get_select(input$category, filterby())]]))
  
  # calculate which samples to keep
  keep <- reactive({
    keep <- rep(TRUE, nrow(order()))

    for (char in names(outline[[input$category]]))
      keep <- keep & (
        order()[[char]] %in% parse_opt(input[[get_select(input$category, char)]]))

    keep
  })
  
  # the number of features before dimensionality reduction
  features <- reactive({
    if (subi() == "Total")
      num_feat <- categories[[input$category]]
    else
    {
      for (dec_group in decorations)
      {
        if (input$category %in% dec_group$Categories)
          num_feat <- length(dec_group$Subsets[[subi()]])
      }
    }
    calc_feat(pc_cap, feat()/100, num_feat)
  })
  
  # the title of the plot
  title_text <- reactive({
    if (input$embedding == "Sets")
    {
      return(sprintf(
        "%s-Grouped Features on %s.%s (%s Features, %s Characteristics)", 
        filterby(), input$category, subi(), nrow(downloadData()), ncol(downloadData())
      ))
    }
    
    nei <- ifelse(
      (input$embedding == "UMAP" || input$embedding == "PHATE" || 
         input$visualize == "tSNE") && (input$visualize != "Summarize"), 
      sprintf(", %s Neighbors", input$perplexity), "")
    
    if (grepl("Transpose", input$category, fixed = TRUE))
      notif("This matrix has been transposed. Therefore, the current
            samples represent original features and the current
            features represent original samples.", 6, "warning")
    
    sprintf("%s%s on %s.%s (%s Samples, %s Features%s)", 
            ifelse(input$embedding == "PHATE", "", 
                   repStr(input$visualize, vis_options, vis_nouns)), 
            input$embedding, input$category, subi(), sum(keep()), features(), nei)
  })
  
  # the title of the plot
  title <- reactive({
    if (title_access())
      return(title_text())
    return("")
  })
  
  # used for custom color schemes
  paint <- reactive({
    if (input$palette=="Custom" && !is.null(custom_color_scales) &&
        colorby() %in% names(custom_color_scales) && input$visualize != "Summarize")
    {
      new <- custom_color_scales[[colorby()]] %>% sort_by_names() %>% unlist()
      present <- TRUE
      
      for (color in colors())
        if (!(color %in% names(new)))
          present <- FALSE
      
      if (present)
        return(new)
    }
    
    num <- length(unique(colors()))
    
    if (input$embedding == "Sets")
      num <- 5
    else
    {
      if (input$visualize == "Summarize")
      {
        if (input$embedding == "PCA")
          return("#00356B")
        if (input$embedding == "VAE")
          return(c("#C90016", "#00356B"))
        if (input$embedding == "UMAP")
          num <- 6
      }
    }
    return(color_seq(num, input$palette, not_rev()))
  })
  
  # shapes for ggplot2, plotly
  shape_num <- reactive({
    if (input$embedding == "Sets")
      return(NULL)
    
    if (input$visualize == "Summarize")
    {
      if (input$embedding == "PCA")
        return(1)
      if (input$embedding == "VAE")
        return(2)
      if (input$embedding == "UMAP")
        return(6)
    }
    
    length(unique(shapes()))
  })
  
  # ---------------
  # PLOT GENERATION
  # ---------------
  
  # generates ggplot2 data
  ggplot2_data <- reactive({
    if (input$embedding == "VAE" && which(nor_options == input$normalize) > 2)
      return(NULL)
    
    if (input$embedding == "Sets")
    {
      if (is.null(my_chars())) 
        return(NULL)
      
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", thre_ind(), 
                      which(sca_options == input$scale), filterby(), input$category)
      
      data <- load_db(addr, aws_bucket)[,my_chars(),drop=FALSE] %>% 
        get_safe_sub(subi(), ., decorations, input$category, 1)
      
      downloadData(data)
      
      if (nrow(data) > upse_feat())
        data <- data[1:upse_feat(),,drop=FALSE]
      
      data <- data %>% set_f1_f2(input$set_f1, input$set_f2) %>% num_nan_binary()
      
      if (ncol(data) < 1 || nrow(data) < 8)
        return(NULL)
      
      if (ncol(data) == 1)
        return(venn1_custom(data, legend()))
      
      if (ncol(data) == 2)
        return(venn2_custom(data, legend()))
      
      return(upset_custom(data, legend()))
    }
    
    addr <- make_aws_name(input$category, subi(),
                          input$scale, input$normalize, 
                          feat(), input$embedding, input$visualize, 2, per_ind())
    
    data <- load_db(addr, aws_bucket)
    
    if (input$embedding == "PHATE")
    {
      data <- data[keep(),,drop=FALSE]
      downloadData(data)
      
      return(ggplot2_2d(
        data[,1], data[,2], pc("1"), pc("2"),
        colors(), shapes(), paint(), shape_num(), title(), legend()))
    }
    
    if (input$visualize == "Summarize")
    {
      downloadData(data)
      
      if (input$embedding == "PCA")
        return(ggplot2_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", 
          rep("Cumulative Variance", pc_cap), 
          rep("Cumulative Variance", pc_cap),
          paint(), shape_num(), title(), legend()
        ) + geom_smooth(se=FALSE, method="gam", formula = y ~ s(log(x))))
      
      if (input$embedding == "VAE")
        return(ggplot2_2d(
          data[,"Training Iterations"], data[,"Loss Value"],
          "Number of Training Iterations", "Loss Function Output",
          data[,"Loss Type"], 
          data[,"Loss Type"],
          paint(), shape_num(), title(), legend()
        ) + geom_smooth(se=FALSE, method="gam", formula = y ~ s(log(x))))
      
      if (input$embedding == "UMAP")
        return(ggplot2_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", 
          data[,3], 
          data[,3],
          paint(), shape_num(), title(), legend()
        ) + geom_line())
    }
    
    data <- data[keep(),,drop=FALSE]
    downloadData(data)
    
    if (input$visualize == "Explore")
    {
      return(ggplot2_2d(
        data[,input$pc1], data[,input$pc2], pc(input$pc1), pc(input$pc2), 
        colors(), shapes(), paint(), shape_num(), title(), legend()))
    }
    
    if (input$visualize == "tSNE")
    {
      return(ggplot2_2d(
        data[,1], data[,2], pc("1"), pc("2"), 
        colors(), shapes(), paint(), shape_num(), title(), legend()))
    }
  })
  
  # generates plotly2 data
  plotly2_data <- reactive({
    if (input$embedding == "VAE" && which(nor_options == input$normalize) > 2)
      return(NULL)
    
    if (input$embedding == "Sets")
    {
      if (is.null(my_chars())) 
        return(NULL)
      
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", thre_ind(), 
                      which(sca_options == input$scale), filterby(), input$category)
      
      data <- load_db(addr, aws_bucket)[,my_chars(),drop=FALSE] %>% 
        get_safe_sub(subi(), ., decorations, input$category, 1)
      
      downloadData(data)
      
      if (nrow(data) > heat_feat())
        data <- data[1:heat_feat(),,drop=FALSE]
      
      data <- data[base::order(rowSums(data),decreasing=T),] %>% 
        set_f1_f2(input$set_f1, input$set_f2)
      
      if (ncol(data) < 1 || nrow(data) < 1)
        return(NULL)
      
      return(plotly_heatmap_variance(data, paint(), title(), legend(), boost()))
    }
    
    addr <- make_aws_name(input$category, subi(),
                          input$scale, input$normalize, 
                          feat(), input$embedding, input$visualize, 2, per_ind())
    
    data <- load_db(addr, aws_bucket)
    
    if (input$embedding == "PHATE")
    {
      data <- data[keep(),,drop=FALSE]
      
      downloadData(data)
      
      return(plotly_2d(
        data[,1], data[,2], pc("1"), pc("2"), "markers",
        colors(), labels(), paint(), title(), legend()
      ))
    }
    
    if (input$visualize == "Summarize")
    {
      downloadData(data)
      
      if (input$embedding == "PCA")
        return(plotly_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", "markers",
          rep("Cumulative", pc_cap), 
          sprintf("%s: %s", "Variance", rep("Cumulative", pc_cap)), 
          paint(), title(), legend()
        ))
      
      if (input$embedding == "VAE")
        return(plotly_2d(
          data[,"Training Iterations"], data[,"Loss Value"], 
          "Number of Training Iterations", "Loss Function Output", "markers",
          data[,"Loss Type"], 
          sprintf("%s: %s", "Loss Type", data[,"Loss Type"]), 
          paint(), title(), legend()
        ))
      
      if (input$embedding == "UMAP")
        return(plotly_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", "markers",
          data[,3], 
          sprintf("%s: %s", "Embedding", data[,3]),
          paint(), title(), legend()
        ))
    }
    
    data <- data[keep(),,drop=FALSE]
    downloadData(data)
    
    if (input$visualize == "Explore")
    {
      return(plotly_2d(
        data[,input$pc1], data[,input$pc2], pc(input$pc1), pc(input$pc2), "markers", 
        colors(), labels(), paint(), title(), legend()
      ))
    }
    
    if (input$visualize == "tSNE")
    {
      return(plotly_2d(
        data[,1], data[,2], pc("1"), pc("2"), "markers", 
        colors(), labels(), paint(), title(), legend()
      ))
    }
  })
  
  # generates plotly3 data
  plotly3_data <- reactive({
    if (input$embedding == "VAE" && which(nor_options == input$normalize) > 2)
      return(NULL)
    
    if (input$embedding == "Sets")
    {
      if (is.null(my_chars())) 
        return(NULL)
      
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", thre_ind(), 
                      which(sca_options == input$scale), filterby(), input$category)
      
      data <- load_db(addr, aws_bucket)[,my_chars(),drop=FALSE] %>% 
        get_safe_sub(subi(), ., decorations, input$category, 1)
      
      downloadData(data)
      
      if (nrow(data) > dend_feat())
        data <- data[1:dend_feat(),,drop=FALSE]
      
      data <- data %>% set_f1_f2(input$set_f1, input$set_f2)
      
      if (ncol(data) < 1 || nrow(data) < 1)
        return(NULL)
      
      return(plotly_heatmap_dendrogram(data, paint(), title(), legend(), boost()))
    }
    
    addr <- make_aws_name(input$category, subi(),
                          input$scale, input$normalize, 
                          feat(), input$embedding, input$visualize, 3, per_ind())
    
    data <- load_db(addr, aws_bucket)
    
    if (input$embedding == "PHATE")
    {
      data <- data[keep(),,drop=FALSE]
      downloadData(data)
      
      return(plotly_3d(
        data[,1], data[,2], data[,3], pc("1"), pc("2"), pc("3"),
        colors(), labels(), paint(), title(), legend()
      ))
    }
    
    if (input$visualize == "Summarize")
    {
      downloadData(data)
      
      if (input$embedding == "PCA")
        return(plotly_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", "lines+markers",
          rep("Cumulative", pc_cap), 
          sprintf("%s: %s", "Variance", rep("Cumulative", pc_cap)), 
          paint(), title(), legend()
        ))
      
      if (input$embedding == "VAE")
        return(plotly_2d(
          data[,"Training Iterations"], data[,"Loss Value"], 
          "Number of Training Iterations", "Loss Function Output", "lines+markers",
          data[,"Loss Type"], 
          sprintf("%s: %s", "Loss Type", data[,"Loss Type"]), 
          paint(), title(), legend()
        ))
      
      if (input$embedding == "UMAP")
        return(plotly_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", "lines+markers",
          data[,3], 
          sprintf("%s: %s", "Embedding", data[,3]),
          paint(), title(), legend()
        ))
    }
    
    data <- data[keep(),,drop=FALSE]
    downloadData(data)
    
    if (input$visualize == "Explore")
    {
      return(plotly_3d(
        data[,input$pc1], data[,input$pc2], data[,input$pc3], 
        pc(input$pc1), pc(input$pc2), pc(input$pc3), 
        colors(), labels(), paint(), title(), legend()
      ))
    }
    
    if (input$visualize == "tSNE")
    {
      return(plotly_3d(
        data[,1], data[,2], data[,3],
        pc("1"), pc("2"), pc("3"), 
        colors(), labels(), paint(), title(), legend()
      ))
    }
  })
  
  # generates beeswarm data
  beeswarm_data <- reactive({
    if (!(input$visualize == 'Explore' && 
          (input$embedding %in% c('PCA', 'VAE', 'UMAP'))))
      return(NULL)
    
    addr <- make_aws_name(input$category, subi(),
                          input$scale, input$normalize, 
                          feat(), input$embedding, input$visualize, 2, per_ind())
    
    data <- load_db(addr, aws_bucket)[keep(),input$pc1]
    
    if (length(data) < 1)
      return(NULL)
    
    data  <- cbind.data.frame(data, colors())
    pc_name <- pc(input$pc1)
    colnames(data) <- c(pc_name, colorby())
    temp <- unique(colors())
    
    names <- 1:length(temp)
    if (legend())
      names <- temp
    
    boxplot_beeswarm(data, get(pc_name) ~ get(colorby()), 
                     colorby(), pc_name, names, 
                     sprintf("%s44", substr(paint(), start=1, stop=7)), 
                     paint())
  })
  
  # generates data to accompany graphs
  legend_data <- reactive({
    temp <- unique(colors())
    
    if (length(temp) < 1)
      return(NULL)
    
    table <- cbind.data.frame(1:length(temp), temp)
    colnames(table) <- c("Number", "Value")
    
    table
  })
  
  # -----------
  # BOOKMARKING
  # -----------
  
  # kill app when window closes ... disable if testing bookmarks
  session$onSessionEnded(stopApp)
  
  # exclude all inputs from bookmarking
  setBookmarkExclude(bookmark_exclude_vector)
  
  # store compressed data on bookmarking
  onBookmark(function(state) {
    # subsets
    target <- my_empty_list(name_cat)
    
    for (cat in name_cat)
      target[[cat]] <- match(input[[sprintf("subsetby_%s", cat)]], 
                             sub_groups[[cat]]) %>% indices_fifstr()
    
    subset_c <- paste(unlist(target), collapse=sep_chars[3])
    
    # colors, shapes, labels, filters
    color_shape_label_filter <- rep("", 4)
    types <- c("color", "shape", "label", "filter")
    
    for (i in 1:4)
    {
      target <- my_empty_list(name_cat)
      
      for (cat in name_cat)
        target[[cat]] <- match(input[[sprintf("%sby_%s", types[i], cat)]], 
                               names(outline[[cat]])) %>% indices_fifstr()
      
      color_shape_label_filter[i] <- paste(unlist(target), collapse=sep_chars[3])
    }
    
    # selections
    select_array_c <- my_empty_list(name_cat)
    for (cat in name_cat)
    {
      chars <- outline[[cat]]
      select_array_c[[cat]] <- lapply(names(chars), function(char){
        match(input[[get_select(cat, char)]], chars[[char]]) %>% indices_fifstr()
      })
    }
    select_array_c <- encode_lol(select_array_c)
    
    # thresholds
    thre_array_c <- my_empty_list(name_cat)
    for (cat in name_cat)
    {
      target <- my_empty_list(sca_options)
      for (sca in sca_options)
        target[[sca]] <- encode_num(input[[get_thre(cat, sca)]])
      
      thre_array_c[[cat]] <- target
    }
    thre_array_c <- encode_lol(thre_array_c)
    
    # encoded
    encoded_form <- list(
      "complex"=c(
        subset_c, 
        color_shape_label_filter, 
        select_array_c, 
        thre_array_c
      ),
      sMenu=input$sMenu,
      height=input$height,
      category=input$category, 
      scale=input$scale, 
      normalize=input$normalize, 
      features=input$features, 
      embedding=input$embedding, 
      visualize=input$visualize, 
      perplexity=input$perplexity, 
      set_feat_upse=input$set_feat_upse,
      set_feat_heat=input$set_feat_heat,
      set_feat_dend=input$set_feat_dend,
      palette=input$palette, 
      plotPanels=input$plotPanels, 
      set_f1=input$set_f1, 
      set_f2=input$set_f2, 
      pc1=input$pc1, 
      pc2=input$pc2, 
      pc3=input$pc3
    )
    
    # get the vector of all session IDs
    num_sessions <- 0
    if (length(get_bucket(aws_bucket, prefix="Sessions/num_sessions.rds")) > 0)
      num_sessions <- load_db("Sessions/num_sessions.rds", aws_bucket)
    
    # find a session ID that is not used
    i <- 1
    while (i %in% num_sessions)
      i <- i+1
    
    # add the session ID to the list and save the session
    save_db(c(num_sessions, i), aws_bucket, "Sessions/num_sessions.rds")
    save_db(encoded_form, aws_bucket, sprintf("Sessions/session_%s.rds", i))
    
    # the bookmark is simply the numerical ID for the session
    state$values$user_id <- i
  })
  
  # restore compressed data when link is followed
  onRestore(function(state) {
    # get the vector of all session IDs
    num_sessions <- 0
    if (length(get_bucket(aws_bucket, prefix="Sessions/num_sessions.rds")) > 0)
      num_sessions <- load_db("Sessions/num_sessions.rds", aws_bucket)
    
    # if the ID is invalid, load nothing
    id <- state$values$user_id
    if (!(id %in% num_sessions))
      return(NULL)
    
    # otherwise, load the appropriate item
    data <- load_db(sprintf("Sessions/session_%s.rds", id), aws_bucket)
    complex <- data$complex
    data$complex <- NULL
    
    # subsets, colors, shapes, filters, selections, thresholds
    subsets <- strsplit(complex[[1]], sep_chars[3])[[1]] %>% as.list()
    names(subsets) <- name_cat
    colors <- strsplit(complex[[2]], sep_chars[3])[[1]] %>% as.list()
    names(colors) <- name_cat
    shapes <- strsplit(complex[[3]], sep_chars[3])[[1]] %>% as.list()
    names(shapes) <- name_cat
    labels <- strsplit(complex[[4]], sep_chars[3])[[1]] %>% as.list()
    names(labels) <- name_cat
    filters <- strsplit(complex[[5]], sep_chars[3])[[1]] %>% as.list()
    names(filters) <- name_cat
    
    checkboxes <- decode_lol(complex[[6]], bookmark_cat)
    thres <- decode_lol(complex[[7]], bookmark_thre)
    
    for (cat in name_cat)
    {
      chars <- outline[[cat]]
      
      # subsets
      updatePickerInput(
        session, inputId = sprintf("subsetby_%s", cat), 
        selected = sub_groups[[cat]][fifstr_indices(subsets[[cat]])]
      )
      
      # colors
      updatePickerInput(
        session, inputId = sprintf("colorby_%s", cat), 
        selected = names(chars)[fifstr_indices(colors[[cat]])]
      )
      
      # shapes
      updatePickerInput(
        session, inputId = sprintf("shapeby_%s", cat), 
        selected = names(chars)[fifstr_indices(shapes[[cat]])]
      )
      
      # labels
      updatePickerInput(
        session, inputId = sprintf("labelby_%s", cat), 
        selected = names(chars)[fifstr_indices(labels[[cat]])]
      )
      
      # filters
      updatePickerInput(
        session, inputId = sprintf("filterby_%s", cat), 
        selected = names(chars)[fifstr_indices(filters[[cat]])]
      )
      
      # selections
      for (char in names(chars))
        updatePickerInput(
          session, inputId = get_select(cat, char), 
          selected = chars[[char]][fifstr_indices(checkboxes[[cat]][[char]])]
        ) 
      
      # thresholds
      for (sca in sca_options)
        updateSliderInput(
          session, inputId = get_thre(cat, sca), value = 
            as.numeric(thres[[cat]][[sca]]) / 10000)
    }
    
    # simpler
    updatePickerInput(session, inputId = "sMenu", selected = data[["sMenu"]])
    updateNumericInput(session, inputId = "height", value = data[["height"]])
    updatePickerInput(session, inputId = "category", selected = data[["category"]])
    updatePickerInput(session, inputId = "scale", selected = data[["scale"]])
    updatePickerInput(session, inputId = "normalize", selected = data[["normalize"]])
    updatePickerInput(session, inputId = "features", selected = data[["features"]])
    updatePickerInput(session, inputId = "embedding", selected = data[["embedding"]])
    updatePickerInput(session, inputId = "visualize", selected = data[["visualize"]])
    updatePickerInput(session, inputId = "perplexity", selected = data[["perplexity"]])
    updateNumericInput(session, inputId = "set_feat_upse", value = data[["set_feat_upse"]])
    updateNumericInput(session, inputId = "set_feat_heat", value = data[["set_feat_heat"]])
    updateNumericInput(session, inputId = "set_feat_dend", value = data[["set_feat_dend"]])
    updatePickerInput(session, inputId = "palette", selected = data[["palette"]])
    updateTabsetPanel(session, inputId = "plotPanels", selected = data[["plotPanels"]])
    updateNumericRangeInput(session, inputId = "set_f1", value = data[["set_f1"]])
    updateNumericRangeInput(session, inputId = "set_f2", value = data[["set_f2"]])
    updateSliderInput(session, inputId = "pc1", value = data[["pc1"]])
    updateSliderInput(session, inputId = "pc2", value = data[["pc2"]])
    updateSliderInput(session, inputId = "pc3", value = data[["pc3"]])
  })
}

# -----------
# RUN THE APP
# -----------
shinyApp(ui = ui, server = server, enableBookmarking = "url")