# The goal of this app is to perform dimensionality reduction.

# source("~/Justin-Tool/shiny-dim-reduction/build.R")
source("inherit.R", encoding="UTF-8")
absolute_begin <- my_timer()
source("functions.R", encoding="UTF-8")
source("interface.R", encoding="UTF-8")
source("options.R", encoding="UTF-8")

# -----------
# ASSEMBLE UI
# -----------

ui <- function(request){
  dashboardPage(
    skin="blue",
    dashboardHeader(title=app_title,titleWidth="100%"),
    dashboardSidebar(
      width=300,
      sidebarMenu(
        id = "sidebar_menu",
        menuItem(
          startExpanded=TRUE,
          "Data Selection",
          select_panel("category", "Category", cat_groups),
          sub_panels_ui(cat_groups, sub_groups),
          select_panel("embedding", "Method of Dimensionality Reduction", emb_options),
          conditionalPanel(
            condition = "
  input.embedding == 'PCA' || input.embedding == 'VAE' || input.embedding == 'UMAP'", 
            select_panel("visualize", "Method of Visualization", vis_options)
          ),
          conditionalPanel(
            condition = "input.embedding == 'Sets'",
            conditionalPanel(
              condition = "input.plotPanels == 'ggplot2'",
              select_panel("upsetpref", "Method of Visualization", ups_options)),
            conditionalPanel(
              condition = "input.plotPanels == 'plotly2' || input.plotPanels == 'plotly3'",
              select_panel("dendrogram", "Method of Visualization", den_options))
          )
        ),
        menuItem(
          "Settings", 
          check_panel("sMenu", "Settings", my_settings),
          select_panel("palette", "Color Palette", pal_options),
          numericInput("height", "Graph Height", value=graph_height, min=1, max=4000),
          select_panel("scale", "Scale", sca_options),
          conditionalPanel(
            condition = "input.embedding != 'Sets'",
            select_panel("normalize", "Normalization", nor_options),
            select_panel("features", "Percentage of Features Used", fea_options)
          )
        ),
        menuItem(
          "Data Selection",
          conditionalPanel(
            condition = "input.visualize == 'Summarize' && (input.embedding == 'PCA' ||
    input.embedding == 'VAE' || input.embedding == 'UMAP')",
            "No data selection can be performed under these conditions.
            Please switch to a non-summary plot."
          ),
          conditionalPanel(
            condition = "input.visualize == 'Explore' && (input.embedding == 'PCA' ||
    input.embedding == 'VAE' || input.embedding == 'UMAP')",
            sliderInput("pc1", "Displayed Component 1", 
                        min=1, max=pc_cap, value=1, step=1, ticks = FALSE),
            conditionalPanel(
              condition = "input.plotPanels == 'ggplot2' || 
        input.plotPanels == 'plotly2' || input.plotPanels == 'plotly3'",
              sliderInput("pc2", "Displayed Component 2", 
                          min=1, max=pc_cap, value=2, step=1, ticks = FALSE)
            ),
            conditionalPanel(
              condition = "input.plotPanels == 'plotly3'",
              sliderInput("pc3", "Displayed Component 3", 
                          min=1, max=pc_cap, value=3, step=1, ticks = FALSE)
            )
          ),
          perplexity_ui(perplexity_types),
          sets_ui(thre_panels_ui(thre_opts), max_cat_num),
          do.call(conditionalPanel, c(
            condition = "input.embedding != 'Sets' && (input.embedding == 'PHATE' || 
          input.visualize != 'Summarize')",
            color_panels_ui(color_opts)
          )),
          do.call(conditionalPanel, c(
            condition = "input.embedding != 'Sets' && input.plotPanels == 'ggplot2' &&
      (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
            shape_panels_ui(shape_opts)
          )),
          do.call(conditionalPanel, c(
            condition = "input.embedding != 'Sets' && input.plotPanels != 'beeswarm' &&
            input.plotPanels != 'ggplot2' &&
      (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
            label_panels_ui(label_opts)
          )),
          do.call(conditionalPanel, c(
            condition = "input.visualize != 'Summarize' || 
          input.embedding == 'Sets' || input.embedding == 'PHATE'",
            filter_panels_ui(filter_opts), 
            select_opts
          ))
        )
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(my_css_styling)),
      box(
        title = "Controls",
        collapsible = TRUE, collapsed=TRUE, width="100%",
        action("start", "Start Plotting", "chart-bar", "#FFF", "#0064C8", "#00356B"),
        action("stop", "Stop Plotting", "ban", "#FFF", "#C90016", "#00356B"),
        bookmarkButton(),
        downloadButton('downloadData', 'Numeric Data'),
        downloadButton('downloadMetadata', 'Metadata')
      ),
      tabBox(
        width="100%",
        id = 'plotPanels',
        tabPanel("ggplot2", uiOutput("ggplot2UI")),
        tabPanel("plotly2", uiOutput("plotly2UI")),
        tabPanel("plotly3", uiOutput("plotly3UI")),
        tabPanel("beeswarm", uiOutput("beeswarmUI")),
        tabPanel("Numeric Data", id="num_data", 
                 DTOutput("num_data_table", width="100%") %>% my_spin()),
        tabPanel("Metadata", id="metadata", 
                 DTOutput("metadata_table", width="100%") %>% my_spin())
      ),
      box(
        title = "Documentation",
        collapsible = TRUE, collapsed=TRUE, width="100%",
        action("instructions", "Instructions", "book", "#FFF", "#9400D3", "#00356B"),
        action("citations", "Citations", "book", "#FFF", "#9400D3", "#00356B"),
        downloadButton('downloadInstructions', 'Instructions'),
        downloadButton('downloadCitations', 'Citations')
      ),
      uiOutput("plainTitleUI"),
      div(id="legend_out_spin", DTOutput("legend_out", width="100%") %>% my_spin()),
      hr(),
      "Developed by Justin Chang at the Gerstein Lab, 
        under the mentorship of Joel Rozowsky."
    )
  )
}

# ------------------------------------------
# REACTIVE SERVER: INITIALIZATION, OBSERVERS
# ------------------------------------------

server <- function(input, output, session) {
  # pushes the subtitle to the right
  shinyjs::addClass(id = "central_nav", class = "navbar-right")
  shinyjs::hide("legend_out_spin")
  
  # records the boot time of the program
  notif(sprintf("Reactive initialization complete.<br>Seconds elapsed: %s", 
                my_timer(absolute_begin)), 8, "warning")
  
  # performs setup for authentication
  auth_default <- 0
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
        && input$password == user_credentials[[input$username]])
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
    if (input$toggle_password %%2 == 1)
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
    if (input$plotPanels == "beeswarm")
      beeswarm_current(beeswarm_data())
    legend_current(legend_data())
  })
  
  # manages height and numPlots
  numPlots <- reactiveVal(1)
  height <- reactive({
    if (input$height < 1 || input$height > 4000)
    {
      notif("Warning: Graph height is not in [1, 4000].")
      return(graph_height)
    }
    
    round(input$height, digits=0)
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
    return(HTML(sprintf("<br><b>Intended Title:</b><br>%s", title_text())))
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
  notifq <- reactive("Notifications" %in% input$sMenu)
  memguard <- reactive("Limit Memory Use" %in% input$sMenu)
  not_rev <- reactive("Uninverted Colors" %in% input$sMenu)
  dend <- reactive("Correlation" %in% input$dendrogram)
  upse <- reactive("Frequency" %in% input$upsetpref)
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
  thre <- reactive(input[[get_thre(input$category, input$scale)]])
  
  # reactives that follow from filter-related reactives
  colors <- reactive(order()[keep(), colorby()])
  shapes <- reactive(order()[keep(), shapeby()])
  labels <- reactive(order()[keep(), labelby()])
  my_chars <- reactive(parse_opt(input[[get_select(input$category, filterby())]]))
  my_subset <- reactive(get_my_subset(decorations, input$category, subi()))
  
  # calculate which samples to keep
  keep <- reactive({
    keep <- rep(TRUE, nrow(order()))
    
    chars <- outline[[input$category]]
    for (char in names(chars))
    {
      keep <- (
        order()[[char]] %in% parse_opt(input[[get_select(input$category, char)]])
      ) & keep
    }
    
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
    if (input$embedding == "Sets")
    {
      truncated <- FALSE
      if (is.null(thresholds) || is.null(my_chars())) 
        return(NULL)
      
      thre_temp <- thresholds[[input$scale]][[input$category]]
      diff <- (thre_temp[2] - thre_temp[1])/10
      chord <- round(0:10*diff+thre_temp[1], 4)
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", 
                      which(chord == thre()), 
                      which(sca_options == input$scale), filterby(), input$category)
      
      if (length(addr) < 1)
        return(NULL)
      
      data <- load_db(addr, aws_bucket)[,my_chars(),drop=FALSE]
      
      if (subi() != "Total" && !is.null(my_subset()))
        data <- data[rownames(data) %in% my_subset(),,drop=FALSE]
      
      if (ncol(data) < 1 || nrow(data) < 8)
        return(NULL)
      
      if (memguard() && (nrow(data) * ncol(data) > max_points))
      {
        truncated <- TRUE
        data <- data[base::order(rowSums(data),decreasing=T),]
        data <- data[1:floor(max_points/ncol(data)),]
      }
      
      data <- data %>% frac_convert(input$set_f1[1], input$set_f1[2]) %>% 
        rowSum_filter_bin(input$set_f2[1], input$set_f2[2]) %>% data.frame()
      
      if (nrow(data) < 8)
        return(NULL)
      
      if (memguard() && (nrow(data) * ncol(data) > max_heatma))
      {
        truncated <- TRUE
        data <- data[base::order(rowSums(data),decreasing=T),]
        data <- data[1:floor(max_heatma/ncol(data)),]
      }
      
      downloadData(data)
      
      if (truncated)
        truncated_msg()
      
      if (ncol(data) == 1)
        return(venn1_custom(data, legend()))
      
      if (ncol(data) == 2)
        return(venn2_custom(data, legend()))
      
      return(upset_custom(data, legend(), ifelse(upse(), "freq", "degree")))
    }
    
    addr <- make_aws_name(make_file_name(
      input$scale, input$normalize, 
      feat(), input$embedding, input$visualize, 2, per_ind()), subi(), input$category)
    
    data <- load_db(addr, aws_bucket)
    
    if (input$embedding == "PHATE")
    {
      data <- data[keep(),]
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
    
    data <- data[keep(),]
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
    if (input$embedding == "Sets")
    {
      truncated <- FALSE
      if (is.null(thresholds) || is.null(my_chars())) 
        return(NULL)
      
      thre_temp <- thresholds[[input$scale]][[input$category]]
      diff <- (thre_temp[2] - thre_temp[1])/10
      chord <- round(0:10*diff+thre_temp[1], 4)
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", 
                      which(chord == thre()), 
                      which(sca_options == input$scale), filterby(), input$category)
      
      if (length(addr) < 1)
        return(NULL)
      
      data <- load_db(addr, aws_bucket)[,my_chars(),drop=FALSE]
      
      if (subi() != "Total" && !is.null(my_subset()))
        data <- data[rownames(data) %in% my_subset(),,drop=FALSE]
      
      if (ncol(data) < 1 || nrow(data) < 1)
        return(NULL)
      
      if (memguard() && (nrow(data) * ncol(data) > max_points))
      {
        truncated <- TRUE
        data <- data[base::order(rowSums(data),decreasing=T),]
        data <- data[1:floor(max_points/ncol(data)),]
      }
      
      data <- data %>% frac_bound(input$set_f1[1], input$set_f1[2]) %>% 
        rowSum_filter_dat(input$set_f2[1], input$set_f2[2])
      
      if (dend())
      {
        if (memguard() && (nrow(data) > max_dendro))
        {
          truncated <- TRUE
          data <- data[base::order(rowSums(data),decreasing=T),]
          data <- data[1:max_dendro,]
        }
        
        downloadData(data)
        
        if (truncated)
          truncated_msg()
        
        return(plotly_heatmap_dendrogram(data, paint(), title(), legend(), FALSE))
      } 
      else
      {
        data <- data[base::order(rowSums(data),decreasing=T),]
        
        if (memguard() && (nrow(data) * ncol(data) > max_heatma))
        {
          truncated <- TRUE
          data <- data[1:floor(max_heatma/ncol(data)),]
        }
        
        downloadData(data)
        
        if (truncated)
          truncated_msg()
        
        return(plotly_heatmap_variance(data, paint(), title(), legend(), FALSE))
      }
    }
    
    addr <- make_aws_name(make_file_name(
      input$scale, input$normalize, 
      feat(), input$embedding, input$visualize, 2, per_ind()), subi(), input$category)
    
    data <- load_db(addr, aws_bucket)
    
    if (input$embedding == "PHATE")
    {
      data <- data[keep(),]
      
      downloadData(data)
      
      return(plotly_2d(
        data[,1], data[,2], pc("1"), pc("2"), "markers",
        colors(), shapes(), sprintf("%s: %s", labelby(), labels()),
        paint(), shape_num(), title(), legend()))
    }
    
    if (input$visualize == "Summarize")
    {
      downloadData(data)
      
      if (input$embedding == "PCA")
        return(plotly_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", "markers",
          rep("Cumulative Variance", pc_cap), 
          rep("Cumulative Variance", pc_cap), 
          data[,"Variance"],
          paint(), shape_num(), title(), legend()))
      
      if (input$embedding == "VAE")
        return(plotly_2d(
          data[,"Training Iterations"], data[,"Loss Value"], 
          "Number of Training Iterations", "Loss Function Output", "markers",
          data[,"Loss Type"], 
          data[,"Loss Type"], 
          data[,"Loss Value"],
          paint(), shape_num(), title(), legend()))
      
      if (input$embedding == "UMAP")
        return(plotly_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", "markers",
          data[,3], 
          data[,3],
          as.numeric(data[,2]), 
          paint(), shape_num(), title(), legend()))
    }
    
    data <- data[keep(),]
    downloadData(data)
    
    if (input$visualize == "Explore")
    {
      return(plotly_2d(
        data[,input$pc1], data[,input$pc2], pc(input$pc1), pc(input$pc2), "markers", 
        colors(), shapes(), sprintf("%s: %s", labelby(), labels()),
        paint(), shape_num(), title(), legend()))
    }
    
    if (input$visualize == "tSNE")
    {
      return(plotly_2d(
        data[,1], data[,2], pc("1"), pc("2"), "markers", 
        colors(), shapes(), sprintf("%s: %s", labelby(), labels()),
        paint(), shape_num(), title(), legend()))
    }
  })
  
  # generates plotly3 data
  plotly3_data <- reactive({
    if (input$embedding == "Sets")
    {
      truncated <- FALSE
      if (is.null(thresholds) || is.null(my_chars())) 
        return(NULL)
      
      thre_temp <- thresholds[[input$scale]][[input$category]]
      diff <- (thre_temp[2] - thre_temp[1])/10
      chord <- round(0:10*diff+thre_temp[1], 4)
      addr <- sprintf("Sets/Sets-%s_%s_%s_%s.rds", 
                      which(chord == thre()), 
                      which(sca_options == input$scale), filterby(), input$category)
      
      if (length(addr) < 1)
        return(NULL)
      
      data <- load_db(addr, aws_bucket)[,my_chars(),drop=FALSE]
      
      if (subi() != "Total" && !is.null(my_subset()))
        data <- data[rownames(data) %in% my_subset(),,drop=FALSE]
      
      if (ncol(data) < 1 || nrow(data) < 1)
        return(NULL)
      
      if ((memguard() && (nrow(data) * ncol(data) > max_points)))
      {
        truncated <- TRUE
        data <- data[1:floor(max_points/ncol(data)),]
      }
      
      data <- data %>% frac_bound(input$set_f1[1], input$set_f1[2]) %>% 
        rowSum_filter_dat(input$set_f2[1], input$set_f2[2])
      
      if (dend())
      {
        if (memguard() && (nrow(data) > max_dendro))
        {
          truncated <- TRUE
          data <- data[base::order(rowSums(data),decreasing=T),]
          data <- data[1:max_dendro,]
        }
        
        downloadData(data)
        
        if (truncated)
          truncated_msg()
        
        return(plotly_heatmap_dendrogram(data, paint(), title(), legend(), TRUE))
      } 
      else
      {
        data <- data[base::order(rowSums(data),decreasing=T),]
        
        if (memguard() && (nrow(data) * ncol(data) > max_heatma))
        {
          truncated <- TRUE
          data <- data[1:floor(max_heatma/ncol(data)),]
        }
        
        downloadData(data)
        
        if (truncated)
          truncated_msg()
        
        return(plotly_heatmap_variance(data, paint(), title(), legend(), TRUE))
      }
    }
    
    addr <- make_aws_name(make_file_name(
      input$scale, input$normalize, 
      feat(), input$embedding, input$visualize, 3, per_ind()), subi(), input$category)
    
    data <- load_db(addr, aws_bucket)
    
    if (input$embedding == "PHATE")
    {
      data <- data[keep(),]
      downloadData(data)
      
      return(plotly_3d(
        data[,1], data[,2], data[,3], pc("1"), pc("2"), pc("3"),
        colors(), shapes(), sprintf("%s: %s", labelby(), labels()),
        paint(), shape_num(), title(), legend()))
    }
    
    if (input$visualize == "Summarize")
    {
      downloadData(data)
      
      if (input$embedding == "PCA")
        return(plotly_2d(
          data[,"Components"], data[,"Variance"], 
          "Number of Components", "Variance Captured", "lines+markers",
          rep("Cumulative Variance", pc_cap), 
          rep("Cumulative Variance", pc_cap), 
          data[,"Variance"],
          paint(), shape_num(), title(), legend()))
      
      if (input$embedding == "VAE")
        return(plotly_2d(
          data[,"Training Iterations"], data[,"Loss Value"], 
          "Number of Training Iterations", "Loss Function Output", "lines+markers",
          data[,"Loss Type"], 
          data[,"Loss Type"], 
          data[,"Loss Value"],
          paint(), shape_num(), title(), legend()))
      
      if (input$embedding == "UMAP")
        return(plotly_2d(
          as.numeric(data[,1]), as.numeric(data[,2]), 
          "Number of Components", "Number of Noisy Samples", "lines+markers",
          data[,3], 
          data[,3],
          as.numeric(data[,2]), 
          paint(), shape_num(), title(), legend()))
    }
    
    data <- data[keep(),]
    downloadData(data)
    
    if (input$visualize == "Explore")
    {
      return(plotly_3d(
        data[,input$pc1], data[,input$pc2], data[,input$pc3], 
        pc(input$pc1), pc(input$pc2), pc(input$pc3), 
        colors(), shapes(), sprintf("%s: %s", labelby(), labels()),
        paint(), shape_num(), title(), legend()))
    }
    
    data <- data[keep(),]
    downloadData(data)
    
    if (input$visualize == "tSNE")
    {
      return(plotly_3d(
        data[,1], data[,2], data[,3],
        pc("1"), pc("2"), pc("3"), 
        colors(), shapes(), sprintf("%s: %s", labelby(), labels()),
        paint(), shape_num(), title(), legend()))
    }
  })
  
  # generates beeswarm data
  beeswarm_data <- reactive({
    if (!(input$visualize == 'Explore' && 
          (input$embedding %in% c('PCA', 'VAE', 'UMAP'))))
      return(NULL)
    
    addr <- make_aws_name(make_file_name(
      input$scale, input$normalize, 
      feat(), input$embedding, input$visualize, 2, per_ind()), subi(), input$category)
    
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
    
    # controls
    sMenu_c <- 16*(title() != "")+8*legend()+4*notifq()+2*memguard()+not_rev()
    
    # options
    category_c <- which(name_cat == input$category)
    scale_c <- which(sca_options == input$scale)
    normalize_c <- which(nor_options == input$normalize)
    features_c <- which(fea_options == add_perc(feat()))
    embedding_c <- which(emb_options == input$embedding)
    visualize_c <- which(vis_options == input$visualize)
    
    if (is.null(perplexity_types))
      perplexity_c <- 1
    else
      perplexity_c <- which(perplexity_types == input$perplexity)
    
    upsetpref_c <- which(ups_options == input$upsetpref)
    dendrogram_c <- which(den_options == input$dendrogram)
    palette_c <- which(unlist(pal_options) == input$palette)
    plotPanels_c <- which(pan_options == input$plotPanels)
    
    if (length(plotPanels_c) < 1)
      plotPanels_c <- 1
    
    # numbers
    set_f1_c <- encode_num(input$set_f1)
    set_f2_c <- encode_num(input$set_f2)
    pc1_c <- input$pc1
    pc2_c <- input$pc2
    pc3_c <- input$pc3
    
    # complete the bookmark, d = data
    state$values$d <- c(
      subset_c, color_shape_label_filter, select_array_c, thre_array_c, sMenu_c, 
      category_c, scale_c, normalize_c, features_c, embedding_c, 
      visualize_c, perplexity_c, upsetpref_c, dendrogram_c, 
      palette_c, plotPanels_c, 
      set_f1_c, set_f2_c, pc1_c, pc2_c, pc3_c
    ) %>% paste(collapse=sep_chars[1])
  })
  
  # restore compressed data when link is followed, d = data
  onRestored(function(state) {
    data <- strsplit(state$values$d, sep_chars[1])[[1]]
    
    # subsets, colors, shapes, filters, selections, thresholds
    subsets <- strsplit(data[1], sep_chars[3])[[1]] %>% as.list()
    names(subsets) <- name_cat
    colors <- strsplit(data[2], sep_chars[3])[[1]] %>% as.list()
    names(colors) <- name_cat
    shapes <- strsplit(data[3], sep_chars[3])[[1]] %>% as.list()
    names(shapes) <- name_cat
    labels <- strsplit(data[4], sep_chars[3])[[1]] %>% as.list()
    names(labels) <- name_cat
    filters <- strsplit(data[5], sep_chars[3])[[1]] %>% as.list()
    names(filters) <- name_cat
    
    checkboxes <- decode_lol(data[6], bookmark_cat)
    thres <- decode_lol(data[7], bookmark_thre)
    
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
    
    # controls 
    sMenu_d <- data[8] 
    bits <- sMenu_d %>% as.numeric() %>% intToBits() %>% 
      as.numeric() %>% head(length(my_settings)) %>% rev()
    updatePickerInput(session, inputId = "sMenu", 
                      selected = my_settings[which(bits == 1)])
    
    # options
    category_d <- data[9] %>% as.numeric()
    updatePickerInput(session, inputId = "category", 
                      selected = name_cat[category_d])
    scale_d <- data[10] %>% as.numeric()
    updatePickerInput(session, inputId = "scale", 
                      selected = sca_options[scale_d])
    normalize_d <- data[11] %>% as.numeric()
    updatePickerInput(session, inputId = "normalize", 
                      selected = nor_options[normalize_d])
    features_d <- data[12] %>% as.numeric()
    updatePickerInput(session, inputId = "features", 
                      selected = fea_options[features_d])
    embedding_d <- data[13] %>% as.numeric()
    updatePickerInput(session, inputId = "embedding", 
                      selected = emb_options[embedding_d])
    visualize_d <- data[14] %>% as.numeric()
    updatePickerInput(session, inputId = "visualize", 
                      selected = vis_options[visualize_d])
    perplexity_d <- data[15] %>% as.numeric()
    updatePickerInput(session, inputId = "perplexity", 
                      selected = perplexity_types[perplexity_d])
    upsetpref_d <- data[16] %>% as.numeric()
    updatePickerInput(session, inputId = "upsetpref",
                      selected = ups_options[upsetpref_d])
    dendrogram_d <- data[17] %>% as.numeric()
    updatePickerInput(session, inputId = "dendrogram",
                      selected = den_options[dendrogram_d])
    palette_d <- data[18] %>% as.numeric()
    my_pal <- unlist(pal_options)
    names(my_pal) <- NULL
    updateTabsetPanel(session, inputId = "palette",
                      selected = my_pal[palette_d])
    plotPanels_d <- data[19] %>% as.numeric()
    updateTabsetPanel(session, inputId = "plotPanels",
                      selected = pan_options[plotPanels_d])
    
    # numbers
    updateSliderInput(session, inputId = "set_f1",
                      value = decode_num(data[20]))
    updateSliderInput(session, inputId = "set_f2",
                      value = decode_num(data[21]))
    updateSliderInput(session, inputId = "pc1",
                      value = data[22] %>% as.numeric())
    updateSliderInput(session, inputId = "pc2",
                      value = data[23] %>% as.numeric())
    updateSliderInput(session, inputId = "pc3",
                      value = data[24] %>% as.numeric())
  })
}

# -----------
# RUN THE APP
# -----------
shinyApp(ui = ui, server = server, enableBookmarking = "url")