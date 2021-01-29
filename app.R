# The goal of this app is to perform dimensionality reduction.
# setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
# source("pipeline.R", encoding="UTF-8")

source("options.R", encoding="UTF-8")

# should data be retrieved from AWS or from ref_loc?
use_local <- FALSE
if (Sys.getenv('SHINY_PORT') == "")
  use_local <- readline("Do you wish to run this app locally? (Y/N)") == 'Y'

loader <- function(file)
{
  if (use_local)
    return(readRDS(sprintf("%s/%s", ref_loc, file)))
  load_db(file)
}

finder <- function(file)
{
  if (use_local)
    return(file.exists(sprintf("%s/%s", ref_loc, file)))
  length(get_bucket(aws_bucket, prefix=file)) > 0
}

saver <- function(object, file)
{
  if (use_local)
    save_ref(object, sprintf("%s/%s", ref_loc, file))
  else
    save_db(object, file)
}

# is the user authenticated by default?
auth_default <- TRUE 

# should plots respond to user inputs by default?
run_default <- TRUE 

server <- function(input, output, session) {
  # --------------
  # AUTHENTICATION
  # --------------
  authenticated <- reactiveVal(auth_default)
  if (!auth_default)
    showModal(authenticator_modal())
  shinyjs::runjs(no_autofill) # prevent google from autocompleting passwords
  addClass("password", "my-hidden-text") # hide password text to start
  
  # handle login attempts
  observeEvent(input$attempt_login, {
    notification("Attempting authentication ...", 3, "default")
    
    if (my_auth(input$username, input$password, user_credentials))
    {
      notification("Authentication was successful - welcome!", 3, "message")
      removeModal()
      authenticated(TRUE)
    }
    else
    {
      Sys.sleep(0.5) # prevent repeated attempts too quickly
      notification("Authentication was unsuccessful.", 6, "error")
      authenticated(FALSE)
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
  
  # ----------------
  # DYNAMIC UI LOGIC
  # ----------------
  observeEvent(input$instructions, {
    showModal(modalDialog(title = HTML("<b>Instructions</b>"), HTML(instructions)))
  })
  
  observeEvent(input$citations, {
    showModal(modalDialog(title = HTML("<b>Citations</b>"), HTML(citations)))
  })
  
  output$downloadInstructions <- downloadHandler(
    filename = "instructions.txt",
    content = function(file){
      writeLines(print_instructions, file)
    }
  )
  
  output$downloadCitations <- downloadHandler(
    filename = "citations.txt",
    content = function(file){
      writeLines(print_citations, file)
    }
  )
  
  # logical conditions too complicated to hard-code
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
  
  for (cond in output_conditions)
    outputOptions(output, cond, suspendWhenHidden = FALSE)
  
  # ---------------------
  # TOGGLE INPUT UPDATING
  # ---------------------
  running <- reactiveVal(run_default)
  if (run_default)
    shinyjs::hide("start")
  else
    shinyjs::hide("stop")
  
  observeEvent(input$start, {
    shinyjs::show("stop")
    running(TRUE)
    shinyjs::hide("start")
  })
  
  observeEvent(input$stop, {
    shinyjs::show("start")
    running(FALSE)
    shinyjs::hide("stop")
  })
  
  observeEvent(input$randomize, {
    pick_random_input(session, "category", name_cat)
    pick_random_input(session, "scale", sca_options)
    pick_random_input(session, "normalize", nor_options[1:2])
    pick_random_input(session, "features", fea_options)
    pick_random_input(session, "embedding", emb_options)
    pick_random_input(session, "visualize", vis_options)
    pick_random_input(session, "perplexity", perplexity_types)
  })
  
  # a copy of all reactives that can stop running
  iplot <- reactiveValues()
  
  observe({
    if (running()) 
      for (id in bookmarkable_ids)
        iplot[[id]] <- input[[id]]
  })
  
  # console output
  output$console_out <- renderPrint({
    for (id in iplot$console)
    {
      print_clean(sprintf("Value of %s:", id))
      print(input[[id]])
      print_clean("")
    }
  })
  
  # -------------------
  # NOTIFICATION SYSTEM
  # -------------------
  
  # backbone of notification system with user-specified duration
  notif <- function(message, form)
  {
    notification(message, isolate(iplot$notif_time), form)
  }
  
  # warning for invalid numeric inputs
  range_invalid_notif <- function(name, min, max)
  {
    notif(sprintf("Warning: %s is not in [%s, %s].", name, min, max), "warning")
  }
  
  # the number of plots that have entered the queue for generation
  num_plots <- reactiveVal(1)
  # prep a (possibly NULL) plot for rendering and send notifications
  prep_plot <- function(target)
  {
    if (!isolate(authenticated()))
      return(ggplot2_null())
    
    num <- isolate(num_plots())
    notif(sprintf("Generating Plot #%s:<br>
Please suspend plotting or wait for plotting to
finish before attempting a new configuration.", num), "default")
    num_plots(num+1)
    
    start <- my_timer()
    
    if (is.null(target))
    {  
      notif("Plot generation failed.<br>
Possible reasons:<br>
(1) invalid configuration<br>
(2) empty dataset", "error")
      return(ggplot2_null())
    }
    
    notif(sprintf("Plot generation was successful.<br>
Seconds elapsed: %s", my_timer(start)), "message")
    
    target
  }
  
  # ----------------
  # INPUT PROCESSING
  # ----------------
  
  height <- reactive({
    if (range_invalid(iplot$height, 1, 4000))
    {
      range_invalid_notif("Graph Height", 1, 4000)
      return(graph_height)
    }
    round(iplot$height, digits=0)
  })
  
  upse_feat <- reactive({
    if (range_invalid(iplot$set_feat_upse, pc_cap, 2^24))
    {
      range_invalid_notif("Number of Features for Sets", pc_cap, 2^24)
      return(max_upse)
    }
    round(iplot$set_feat_upse, digits=0)
  })
  
  heat_feat <- reactive({
    if (range_invalid(iplot$set_feat_heat, pc_cap, 2^24))
    {
      range_invalid_notif("Number of Features for Sets", pc_cap, 2^24)
      return(max_heat)
    }
    round(iplot$set_feat_heat, digits=0)
  })
  
  dend_feat <- reactive({
    if (range_invalid(iplot$set_feat_dend, pc_cap, 2^24))
    {
      range_invalid_notif("Number of Features for Sets", pc_cap, 2^24)
      return(max_dend)
    }
    round(iplot$set_feat_dend, digits=0)
  })
  
  nintersect <- reactive({
    if (range_invalid(iplot$nintersect, 3, max_set_col_num))
    {
      range_invalid_notif("Number of Columns", 3, max_set_col_num)
      return(def_set_col_num)
    }
    round(iplot$nintersect, digits=0)
  })
  
  bar_frac <- reactive({
    if (range_invalid(iplot$bar_frac, 0, 1))
    {
      range_invalid_notif("Bar Fraction", 0, 1)
      return(def_bar_frac)
    }
    iplot$bar_frac
  })
  
  title_access <- reactive("Embed Title" %in% iplot$sMenu)
  legend <- reactive("Embed Legend" %in% iplot$sMenu)
  boost <- reactive("Boost Graphics" %in% iplot$sMenu)
  not_rev <- reactive("Uninverted Colors" %in% iplot$sMenu)
  
  cati <- reactive(iplot$category)
  subi <- reactive(parse_opt(iplot[[id_subset(cati())]]))
  feat <- reactive(rem_perc(iplot$features))
  
  per_ind <- reactive(
    which(perplexity_types == iplot$perplexity)
  )
  sca_ind <- reactive(
    which(sca_options == iplot$scale)
  )
  thre_ind <- reactive(
    which(thre_seqs[[iplot$scale]][[cati()]] == iplot[[id_thre(cati(), iplot$scale)]])
  )
  
  colorby <- reactive(iplot[[id_color(cati())]])
  shapeby <- reactive(iplot[[id_shape(cati())]])
  labelby <- reactive(iplot[[id_label(cati())]])
  filterby <- reactive(iplot[[id_filter(cati())]])
  
  # calculate which samples to keep after considering all metadata filters
  order <- reactive(order_total[[cati()]])
  keep <- reactive({
    keep <- rep(TRUE, nrow(order()))
    
    for (char in selected_chars[[cati()]])
      keep <- keep & (
        order()[[char]] %in% parse_opt(iplot[[id_select(cati(), char)]]))
    
    keep
  })
  metadata <- reactive(order()[keep(),,drop=FALSE])
  
  colors <- reactive(metadata()[, colorby()])
  shapes <- reactive(metadata()[, shapeby()])
  labels <- reactive(sprintf("%s: %s", labelby(), metadata()[, labelby()]))
  my_chars <- reactive(parse_opt(iplot[[id_select(cati(), filterby())]]))
  
  # the number of features before dimensionality reduction
  num_feat <- reactive({
    ifelse(
      subi() == "Total",
      categories[[cati()]],
      length(get_decor_subset(cati(), subi()))
    ) %>% calc_feat(pc_cap, feat()/100, .)
  })
  
  # numeric data for displaying / downloading
  num_data <- reactiveVal()
  
  # the title of the plot, to be embedded or displayed as plain text
  title_text <- reactive({
    if (iplot$embedding == "Sets")
    {
      return(sprintf(
        "%s-Grouped Features on %s.%s (%s Features, %s Characteristics)", 
        filterby(), cati(), subi(), nrow(num_data()), ncol(num_data())
      ))
    }
    
    nei <- ifelse(
      (iplot$embedding == "UMAP" || iplot$embedding == "PHATE" || 
         iplot$visualize == "tSNE") && (iplot$visualize != "Summarize"), 
      sprintf(", %s Neighbors", iplot$perplexity), "")
    
    if (grepl("Transpose", cati(), fixed = TRUE))
      notif("This matrix has been transposed. Therefore, the current
            samples represent original features and the current
            features represent original samples.", "warning")
    
    sprintf("%s%s on %s.%s (%s Samples, %s Features%s)", 
            ifelse(iplot$embedding == "PHATE", "", 
                   repStr(iplot$visualize, vis_options, vis_nouns)), 
            iplot$embedding, cati(), subi(), sum(keep()), num_feat(), nei)
  })
  
  title_embed <- reactive({
    if (!title_access())
      return(NULL)
    title_text()
  })
  
  paint <- reactive({
    # for custom color schemes
    if (iplot$palette == "Custom" && iplot$visualize != "Summarize" && 
        length(custom_color_scales) > 0 && colorby() %in% names(custom_color_scales))
    { 
      new <- custom_color_scales[[colorby()]] %>% unlist()
      
      if (check_custom_colors(colors(), names(new)))
        return(new)
    }
    
    # otherwise pick a built-in palette
    num <- length(unique(colors()))
    
    if (iplot$embedding == "Sets")
      num <- 5
    else
    {
      if (iplot$visualize == "Summarize")
      {
        if (iplot$embedding == "PCA")
          return(single_color_seq)
        if (iplot$embedding == "VAE")
          return(double_color_seq)
        if (iplot$embedding == "UMAP")
          num <- 6
      }
    }
    
    color_seq(num, iplot$palette, !not_rev())
  })
  
  shape_num <- reactive({
    if (iplot$embedding == "Sets")
      return(NULL)
    
    if (iplot$visualize == "Summarize")
    {
      if (iplot$embedding == "PCA")
        return(1)
      if (iplot$embedding == "VAE")
        return(2)
      if (iplot$embedding == "UMAP")
        return(6)
    }
    
    length(unique(shapes()))
  })
  
  # ---------------
  # PLOT GENERATION
  # ---------------
  
  ggplot2_data <- reactive({
    if (iplot$embedding == "VAE" && which(nor_options == iplot$normalize) > 2)
      return(NULL)
    
    if (iplot$embedding == "Sets")
    {
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", thre_ind(), 
                      sca_ind(), filterby(), cati())
      
      data <- loader(addr)[,my_chars(),drop=FALSE] %>% get_row_sub(cati(), subi())
      
      num_data(data)
      
      data <- truncate_rows(data, upse_feat()) %>%
        set_f1_f2(iplot$set_f1, iplot$set_f2) %>% num_nan_binary()
      
      if (ncol(data) == 1)
        return(venn1_custom(data, legend()))
      
      if (!legend())
        colnames(data) <- 1:ncol(data)
      
      return(upset_custom(data, nintersect(), bar_frac(), !legend()))
    }
    
    addr <- make_aws_name(cati(), subi(), iplot$scale, iplot$normalize, 
                          feat(), iplot$embedding, iplot$visualize, 2, per_ind())
    
    data <- loader(addr)
    
    if (iplot$embedding == "PHATE")
    {
      data <- data[keep(),,drop=FALSE]
      num_data(data)
      
      return(ggplot2_2d(
        data[,1], data[,2], pc("1"), pc("2"),
        colors(), shapes(), paint(), shape_num(), title_embed(), legend()))
    }
    
    if (iplot$visualize == "Summarize")
    {
      num_data(data)
      
      if (iplot$embedding == "PCA")
        return(ggplot2_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", 
          rep("Cumulative Variance", pc_cap), 
          rep("Cumulative Variance", pc_cap),
          paint(), shape_num(), title_embed(), legend()
        ) + geom_smooth(se=FALSE, method="gam", formula = y ~ s(log(x))))
      
      if (iplot$embedding == "VAE")
        return(ggplot2_2d(
          data[,"Training Iterations"], data[,"Loss Value"],
          "Number of Training Iterations", "Loss Function Output",
          data[,"Loss Type"], 
          data[,"Loss Type"],
          paint(), shape_num(), title_embed(), legend()
        ) + geom_smooth(se=FALSE, method="gam", formula = y ~ s(log(x))))
      
      if (iplot$embedding == "UMAP")
        return(ggplot2_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", 
          data[,3], 
          data[,3],
          paint(), shape_num(), title_embed(), legend()
        ) + geom_line())
    }
    
    data <- data[keep(),,drop=FALSE]
    num_data(data)
    
    if (iplot$visualize == "Explore")
    {
      return(ggplot2_2d(
        data[,iplot$pc1], data[,iplot$pc2], pc(iplot$pc1), pc(iplot$pc2), 
        colors(), shapes(), paint(), shape_num(), title_embed(), legend()))
    }
    
    if (iplot$visualize == "tSNE")
    {
      return(ggplot2_2d(
        data[,1], data[,2], pc("1"), pc("2"), 
        colors(), shapes(), paint(), shape_num(), title_embed(), legend()))
    }
  })
  
  plotly2_data <- reactive({
    if (iplot$embedding == "VAE" && which(nor_options == iplot$normalize) > 2)
      return(NULL)
    
    if (iplot$embedding == "Sets")
    {
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", thre_ind(), 
                      sca_ind(), filterby(), cati())
      
      data <- loader(addr)[,my_chars(),drop=FALSE] %>% get_row_sub(cati(), subi())
      
      num_data(data)
      
      data <- truncate_rows(data, heat_feat()) %>% 
        sort_row_sums() %>% set_f1_f2(iplot$set_f1, iplot$set_f2)
      
      return(plotly_heatmap_variance(data, paint(), title_embed(), legend(), boost()))
    }
    
    addr <- make_aws_name(cati(), subi(), iplot$scale, iplot$normalize, 
                          feat(), iplot$embedding, iplot$visualize, 2, per_ind())
    
    data <- loader(addr)
    
    if (iplot$embedding == "PHATE")
    {
      data <- data[keep(),,drop=FALSE]
      
      num_data(data)
      
      return(plotly_2d(
        data[,1], data[,2], pc("1"), pc("2"), "markers",
        colors(), labels(), paint(), title_embed(), legend()
      ))
    }
    
    if (iplot$visualize == "Summarize")
    {
      num_data(data)
      
      if (iplot$embedding == "PCA")
        return(plotly_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", "markers",
          rep("Cumulative", pc_cap), 
          sprintf("%s: %s", "Variance", rep("Cumulative", pc_cap)), 
          paint(), title_embed(), legend()
        ))
      
      if (iplot$embedding == "VAE")
        return(plotly_2d(
          data[,"Training Iterations"], data[,"Loss Value"], 
          "Number of Training Iterations", "Loss Function Output", "markers",
          data[,"Loss Type"], 
          sprintf("%s: %s", "Loss Type", data[,"Loss Type"]), 
          paint(), title_embed(), legend()
        ))
      
      if (iplot$embedding == "UMAP")
        return(plotly_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", "markers",
          data[,3], 
          sprintf("%s: %s", "Embedding", data[,3]),
          paint(), title_embed(), legend()
        ))
    }
    
    data <- data[keep(),,drop=FALSE]
    num_data(data)
    
    if (iplot$visualize == "Explore")
    {
      return(plotly_2d(
        data[,iplot$pc1], data[,iplot$pc2], pc(iplot$pc1), pc(iplot$pc2), "markers", 
        colors(), labels(), paint(), title_embed(), legend()
      ))
    }
    
    if (iplot$visualize == "tSNE")
    {
      return(plotly_2d(
        data[,1], data[,2], pc("1"), pc("2"), "markers", 
        colors(), labels(), paint(), title_embed(), legend()
      ))
    }
  })
  
  plotly3_data <- reactive({
    if (iplot$embedding == "VAE" && which(nor_options == iplot$normalize) > 2)
      return(NULL)
    
    if (iplot$embedding == "Sets")
    {
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", thre_ind(), 
                      sca_ind(), filterby(), cati())
      
      data <- loader(addr)[,my_chars(),drop=FALSE] %>% get_row_sub(cati(), subi())
      
      num_data(data)
      
      data <- truncate_rows(data, dend_feat()) %>% set_f1_f2(iplot$set_f1, iplot$set_f2)
      
      return(plotly_heatmap_dendrogram(data, paint(), title_embed(), legend(), boost()))
    }
    
    addr <- make_aws_name(cati(), subi(), iplot$scale, iplot$normalize, 
                          feat(), iplot$embedding, iplot$visualize, 3, per_ind())
    
    data <- loader(addr)
    
    if (iplot$embedding == "PHATE")
    {
      data <- data[keep(),,drop=FALSE]
      num_data(data)
      
      return(plotly_3d(
        data[,1], data[,2], data[,3], pc("1"), pc("2"), pc("3"),
        colors(), labels(), paint(), title_embed(), legend()
      ))
    }
    
    if (iplot$visualize == "Summarize")
    {
      num_data(data)
      
      if (iplot$embedding == "PCA")
        return(plotly_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", "lines+markers",
          rep("Cumulative", pc_cap), 
          sprintf("%s: %s", "Variance", rep("Cumulative", pc_cap)), 
          paint(), title_embed(), legend()
        ))
      
      if (iplot$embedding == "VAE")
        return(plotly_2d(
          data[,"Training Iterations"], data[,"Loss Value"], 
          "Number of Training Iterations", "Loss Function Output", "lines+markers",
          data[,"Loss Type"], 
          sprintf("%s: %s", "Loss Type", data[,"Loss Type"]), 
          paint(), title_embed(), legend()
        ))
      
      if (iplot$embedding == "UMAP")
        return(plotly_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", "lines+markers",
          data[,3], 
          sprintf("%s: %s", "Embedding", data[,3]),
          paint(), title_embed(), legend()
        ))
    }
    
    data <- data[keep(),,drop=FALSE]
    num_data(data)
    
    if (iplot$visualize == "Explore")
    {
      return(plotly_3d(
        data[,iplot$pc1], data[,iplot$pc2], data[,iplot$pc3], 
        pc(iplot$pc1), pc(iplot$pc2), pc(iplot$pc3), 
        colors(), labels(), paint(), title_embed(), legend()
      ))
    }
    
    if (iplot$visualize == "tSNE")
    {
      return(plotly_3d(
        data[,1], data[,2], data[,3],
        pc("1"), pc("2"), pc("3"), 
        colors(), labels(), paint(), title_embed(), legend()
      ))
    }
  })
  
  # generates beeswarm data
  beeswarm_data <- reactive({
    if (!(iplot$embedding %in% c('PCA', 'VAE', 'UMAP')))
      return(NULL)
    
    if (iplot$visualize != 'Explore')
      return(NULL)
    
    addr <- make_aws_name(cati(), subi(), iplot$scale, iplot$normalize, 
                          feat(), iplot$embedding, iplot$visualize, 2, per_ind())
    
    data <- loader(addr)[keep(),iplot$pc1]
    data <- cbind.data.frame(colors(), data)
    colnames(data) <- c(colorby(), pc(iplot$pc1))
 
    boxplot_beeswarm(data, paint(), title_embed(), legend())
  })
  
  legend_data <- reactive({generate_legend_table(colors())})
  
  # -------------------
  # INDIRECT UI OUTPUTS
  # -------------------
  output$title_out <- renderMenu({
    if (!authenticated())
      return(NULL)
    if (title_access())
      return(NULL)
    sprintf("<h3><b>Title:</b> %s</h3>", title_text()) %>% HTML()
  })
  
  output$ggplot2_out <- renderPlot({prep_plot(ggplot2_data())})
  output$plotly2_out <- renderPlotly({prep_plot(plotly2_data())})
  output$plotly3_out <- renderPlotly({prep_plot(plotly3_data())})
  output$beeswarm_out <- renderPlot({prep_plot(beeswarm_data())})
  
  output$num_data_table <- renderDT({
    if (!authenticated())
      return(my_datatable(NULL))
    
    my_datatable(data.frame(num_data()))
  })
  
  output$download_num_data <- downloadHandler(
    filename = function() {
      sprintf("%s_num_data.csv", repStr(title_text(), " ", "_"))
    },
    content = function(file) {
      if (!authenticated())
        return(NULL)
      
      write.csv(num_data(), file)
    }
  )
  
  output$metadata_table <- renderDT({
    if (!authenticated())
      return(my_datatable(NULL))
    
    my_datatable(data.frame(metadata()))
  })
  
  output$download_metadata <- downloadHandler(
    filename = function() {
      sprintf("%s_metadata.csv", repStr(title_text(), " ", "_"))
    },
    content = function(file) {
      if (!authenticated())
        return(NULL)
      
      write.csv(metadata(), file)
    }
  )
  
  output$legend_out <- renderDT({
    if (!authenticated() || legend()) 
      return(NULL)
    
    my_datatable(legend_data())
  })
  
  # -----------------
  # DIRECT UI OUTPUTS
  # -----------------
  
  output$ggplot2UI <- renderUI({
    plotOutput("ggplot2_out", width="100%", height=height()) %>% my_spin()
  })
  
  output$plotly2UI <- renderUI({
    plotlyOutput("plotly2_out", width="100%", height=height()) %>% my_spin()
  })
  
  output$plotly3UI <- renderUI({
    plotlyOutput("plotly3_out", width="100%", height=height()) %>% my_spin()
  })
  
  output$beeswarmUI <- renderUI({
    plotOutput("beeswarm_out", width="100%", height=height()) %>% my_spin()
  })
  
  output$num_dataUI <- renderUI({
    DTOutput("num_data_table", width="100%", height=height()) %>% my_spin()
  })
  
  output$metadataUI <- renderUI({
    DTOutput("metadata_table", width="100%", height=height()) %>% my_spin()
  })
  
  # -----------
  # BOOKMARKING
  # -----------
  
  session$onSessionEnded(stopApp)
  setBookmarkExclude(bookmark_exclude_vector)
  
  onBookmark(function(state) {
    session_data <- session_data_template
    
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
    if (finder("Sessions/num_sessions.rds"))
      num_sessions <- loader("Sessions/num_sessions.rds")
    
    # find a session ID that is not used
    i <- smallest_missing(num_sessions)
    
    # add the session ID to the list and save the session
    saver(c(num_sessions, i), "Sessions/num_sessions.rds")
    saver(session_data, sprintf("Sessions/session_%s.rds", i))
    
    # the bookmark is simply the numerical ID for the session
    state$values$user_id <- i
  })
  
  # restore compressed data when link is followed
  onRestore(function(state) {
    # get the vector of all session IDs
    num_sessions <- 0
    if (finder("Sessions/num_sessions.rds"))
      num_sessions <- loader("Sessions/num_sessions.rds")
    
    # if the ID is invalid, load nothing
    id <- state$values$user_id
    if (!(id %in% num_sessions))
      return(NULL)
    
    # otherwise, load the appropriate item
    session_data <- loader(sprintf("Sessions/session_%s.rds", id))
    
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