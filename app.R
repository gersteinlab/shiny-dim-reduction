# The goal of this app is to perform dimensionality reduction.
# setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
# source("pipeline.R", encoding="UTF-8")

source("options.R", encoding="UTF-8")

# ------------------------------------------
# REACTIVE SERVER: INITIALIZATION, OBSERVERS
# ------------------------------------------

server <- function(input, output, session) {
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
    
    if (my_auth(input$username, input$password, user_credentials))
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
  
  # conditions that are too long to calculate otherwise
  output$visualize_cond <- reactive({
    input$embedding %in% c("PCA", "VAE", "UMAP")
  })
  
  output$perplexity_cond <- reactive({
    input$embedding == "PHATE" || input$visualize == "tSNE" || (
      input$embedding == "UMAP" && input$visualize != "Summarize")
  })
  
  output$set_feat_upse_cond <- reactive({
    input$plotPanels == pan_options[1]
  })
  
  output$set_feat_heat_cond <- reactive({
    input$plotPanels == pan_options[2]
  })
  
  output$set_feat_dend_cond <- reactive({
    input$plotPanels == pan_options[3]
  })
  
  output$nintersect_cond <- reactive({
    input$plotPanels == pan_options[1] && input$embedding == "Sets"
  })
  
  output$pc_sliders_cond <- reactive({
    input$visualize == "Explore" && (input$embedding %in% c("PCA", "VAE", "UMAP"))
  })
  
  output$pc_slider2_cond <- reactive({
    input$plotPanels %in% pan_options[1:3]
  })
  
  output$pc_slider3_cond <- reactive({
    input$plotPanels == pan_options[3]
  })
  
  output$shape_opts_cond <- reactive({
    input$plotPanels == pan_options[1]
  })
  
  output$label_opts_cond <- reactive({
    input$plotPanels %in% pan_options[2:3]
  })
  
  for (cond in c(
    "visualize_cond",
    "perplexity_cond",
    "set_feat_upse_cond",
    "set_feat_heat_cond",
    "set_feat_dend_cond",
    "nintersect_cond", 
    "pc_sliders_cond",
    "pc_slider2_cond",
    "pc_slider3_cond",
    "shape_opts_cond",
    "label_opts_cond"))
  {
    outputOptions(output, cond, suspendWhenHidden = FALSE)
  }
  
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
    
    if (input$plotPanels == pan_options[1])
      ggplot2_current(ggplot2_data())
    if (input$plotPanels == pan_options[2])
      plotly2_current(plotly2_data())
    if (input$plotPanels == pan_options[3])
      plotly3_current(plotly3_data())
    if (input$plotPanels == pan_options[4])
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
  nintersect <- reactive({
    if (range_invalid(input$nintersect, 3, 2^num_filters))
    {
      notif("Warning: Number of Intersections is not in [3,2^num_filters].", 6, "warning")
      return(40)
    }
    
    round(input$nintersect, digits=0)
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
    filename = "instructions.txt",
    content = function(file){
      writeLines(print_instructions, file)
    }
  )
  
  # download button for citations
  output$downloadCitations <- downloadHandler(
    filename = "citations.txt",
    content = function(file){
      writeLines(print_citations, file)
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
  
  subi <- reactive(parse_opt(input[[id_subset(input$category)]]))
  feat <- reactive(rem_perc(input$features))
  per_ind <- reactive(which(perplexity_types == input$perplexity))
  notify <- reactive(notifq() && running())
  
  # filter-related reactives from user input selections
  order <- reactive(order_total[[input$category]])
  colorby <- reactive(input[[id_color(input$category)]])
  shapeby <- reactive(input[[id_shape(input$category)]])
  labelby <- reactive(input[[id_label(input$category)]])
  filterby <- reactive(input[[id_filter(input$category)]])
  thre_ind <- reactive({
    which(thre_seqs[[input$scale]][[input$category]] == 
            input[[id_thre(input$category, input$scale)]])
  })
  
  # reactives that follow from filter-related reactives
  colors <- reactive(order()[keep(), colorby()])
  shapes <- reactive(order()[keep(), shapeby()])
  labels <- reactive(sprintf("%s: %s", labelby(), order()[keep(), labelby()]))
  my_chars <- reactive(parse_opt(input[[id_select(input$category, filterby())]]))
  
  # calculate which samples to keep
  keep <- reactive({
    keep <- rep(TRUE, nrow(order()))
    
    for (char in selected_chars[[input$category]])
      keep <- keep & (
        order()[[char]] %in% parse_opt(input[[id_select(input$category, char)]]))
    
    keep
  })
  
  # the number of features before dimensionality reduction
  num_feat <- reactive({
    ifelse(
      subi() == "Total",
      categories[[input$category]],
      length(get_decor_subset(input$category, subi()))
    ) %>% calc_feat(pc_cap, feat()/100, .)
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
            input$embedding, input$category, subi(), sum(keep()), num_feat(), nei)
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
      new <- custom_color_scales[[colorby()]] %>% unlist()
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
    
    color_seq(num, input$palette, !not_rev())
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
      
      data <- load_db(addr)[,my_chars(),drop=FALSE] %>% 
        get_safe_sub(input$category, subi(), 1)
      
      downloadData(data)
      
      data <- truncate_rows(data, upse_feat()) %>%
        set_f1_f2(input$set_f1, input$set_f2) %>% num_nan_binary()
      
      if (ncol(data) < 1 || nrow(data) < 8)
        return(NULL)
      
      if (ncol(data) == 1)
        return(venn1_custom(data, legend()))
      
      return(upset_custom(data, legend(), nintersect()))
    }
    
    addr <- make_aws_name(input$category, subi(),
                          input$scale, input$normalize, 
                          feat(), input$embedding, input$visualize, 2, per_ind())
    
    data <- load_db(addr)
    
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
      
      data <- load_db(addr)[,my_chars(),drop=FALSE] %>% 
        get_safe_sub(input$category, subi(), 1)
      
      downloadData(data)
      
      data <- truncate_rows(data, heat_feat()) %>% 
        sort_row_sums() %>% set_f1_f2(input$set_f1, input$set_f2)
      
      if (ncol(data) < 1 || nrow(data) < 1)
        return(NULL)
      
      return(plotly_heatmap_variance(data, paint(), title(), legend(), boost()))
    }
    
    addr <- make_aws_name(input$category, subi(),
                          input$scale, input$normalize, 
                          feat(), input$embedding, input$visualize, 2, per_ind())
    
    data <- load_db(addr)
    
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
      
      data <- load_db(addr)[,my_chars(),drop=FALSE] %>% 
        get_safe_sub(input$category, subi(), 1)
      
      downloadData(data)

      data <- truncate_rows(data, dend_feat()) %>% set_f1_f2(input$set_f1, input$set_f2)
      
      if (ncol(data) < 1 || nrow(data) < 1)
        return(NULL)
      
      return(plotly_heatmap_dendrogram(data, paint(), title(), legend(), boost()))
    }
    
    addr <- make_aws_name(input$category, subi(),
                          input$scale, input$normalize, 
                          feat(), input$embedding, input$visualize, 3, per_ind())
    
    data <- load_db(addr)
    
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
    
    data <- load_db(addr)[keep(),input$pc1]
    
    if (length(data) < 1)
      return(NULL)
    
    data  <- cbind.data.frame(data, colors())
    pc_name <- pc(input$pc1)
    colnames(data) <- c(pc_name, colorby())
    temp <- unique(colors())
    
    names <- 1:length(temp)
    if (legend())
      names <- temp
    
    boxplot_beeswarm(data, get(pc_name) ~ get(colorby()), colorby(), pc_name, 
                     names, make_transparent(paint()), paint(), title())
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
    # retrieve all input types accordingly
    session_data <- list(
      "pickerInput"=list(),
      "numericInput"=list(),
      "numericRangeInput"=list(),
      "sliderInput"=list(),
      "tabsetPanel"=list()
    )
    
    for (id in picker_input_ids)
      session_data[["pickerInput"]][[id]] <- input[[id]]
    
    for (id in numeric_input_ids)
      session_data[["pickerInput"]][[id]] <- input[[id]]
    
    for (id in numeric_range_input_ids)
      session_data[["pickerInput"]][[id]] <- input[[id]]
    
    for (id in slider_input_ids)
      session_data[["pickerInput"]][[id]] <- input[[id]]
    
    for (id in tabset_panel_ids)
      session_data[["pickerInput"]][[id]] <- input[[id]]
    
    # get the vector of all session IDs
    num_sessions <- 0
    if (length(get_bucket(aws_bucket, prefix="Sessions/num_sessions.rds")) > 0)
      num_sessions <- load_db("Sessions/num_sessions.rds")
    
    # find a session ID that is not used
    i <- 1
    while (i %in% num_sessions)
      i <- i+1
    
    # add the session ID to the list and save the session
    save_db(c(num_sessions, i), "Sessions/num_sessions.rds")
    save_db(session_data, sprintf("Sessions/session_%s.rds", i))
    
    # the bookmark is simply the numerical ID for the session
    state$values$user_id <- i
  })
  
  # restore compressed data when link is followed
  onRestore(function(state) {
    # get the vector of all session IDs
    num_sessions <- 0
    if (length(get_bucket(aws_bucket, prefix="Sessions/num_sessions.rds")) > 0)
      num_sessions <- load_db("Sessions/num_sessions.rds")
    
    # if the ID is invalid, load nothing
    id <- state$values$user_id
    if (!(id %in% num_sessions))
      return(NULL)
    
    # otherwise, load the appropriate item
    session_data <- load_db(sprintf("Sessions/session_%s.rds", id))
    
    # update all input types accordingly
    picker_input_data <- session_data[["pickerInput"]]
    for (name in names(picker_input_data))
      updatePickerInput(session, name, selected = picker_input_data[[name]])
    
    numeric_input_data <- session_data[["numericInput"]]
    for (name in names(numeric_input_data))
      updateNumericInput(session, name, value = numeric_input_data[[name]])
    
    numeric_range_input_data <- session_data[["numericRangeInput"]]
    for (name in names(numeric_range_input_data))
      updateNumericRangeInput(session, name, value = numeric_range_input_data[[name]])
    
    slider_input_data <- session_data[["sliderInput"]]
    for (name in names(slider_input_data))
      updateSliderInput(session, name, value = slider_input_data[[name]])
    
    tabset_panel_data <- session_data[["tabsetPanel"]]
    for (name in names(tabset_panel_data))
      updateTabsetPanel(session, name, selected = tabset_panel_data[[name]])
  })
}

# -----------
# RUN THE APP
# -----------
shinyApp(ui = ui, server = server, enableBookmarking = "url")