# The purpose of this file is to run an application for
# visualizing dimensionality reduction results.

source("install.R")
source_app("dashboard.R")

server <- function(input, output, session) {
  if (run_default)
    shinyjs::hide("start")
  else
    shinyjs::hide("stop")

  if (!auth_default)
    showModal(authenticator_modal())

  # --------------
  # AUTHENTICATION
  # --------------
  authenticated <- reactiveVal(auth_default)
  shinyjs::runjs(no_autofill) # prevent google from autocompleting passwords
  addClass("password", "my-hidden-text") # hide password text to start

  # handle login attempts
  observeEvent(input$attempt_login, {
    notification("Attempting authentication ...", 3, "default")

    if (my_auth(input$username, input$password))
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
                      label = "Password (is visible)",
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
    if (!authenticated())
      return(ggplot2_null())

    num <- isolate(num_plots())

    if (num == 1)
      cat_f("APP LAUNCH TIME: %.1f (sec)\n", net_time())

    notif(sprintf("Generating Plot #%s:<br>
Please suspend plotting or wait for plotting to
finish before attempting a new configuration.", num), "default")
    num_plots(num + 1)

    start <- Sys.time()

    if (is.null(target))
    {
      notif("Plot generation failed.<br>
Possible reasons:<br>
(1) invalid configuration<br>
(2) empty dataset", "error")
      return(ggplot2_null())
    }

    notif(sprintf("Plot generation was successful.<br>
Seconds elapsed: %.2f", time_diff(start)), "message")

    target
  }

  # ---------------------
  # COMPLEX UI CONDITIONS
  # ---------------------
  output$visualize_cond <- reactive({
    input$embedding %in% c("PCA", "VAE", "UMAP")
  })

  output$perplexity_cond <- reactive({
    input$embedding == "UMAP" || input$embedding == "PHATE" ||
      input$visualize == "tSNE"
  })

  output$set_feat_upse_cond <- reactive({
    input$plotPanels == "Static 2D"
  })

  output$set_feat_heat_cond <- reactive({
    input$plotPanels == "Interactive 2D"
  })

  output$set_feat_dend_cond <- reactive({
    input$plotPanels == "Interactive 3D"
  })

  output$nintersect_cond <- reactive({
    input$plotPanels == "Static 2D" && input$embedding == "Sets"
  })

  output$pc_sliders_cond <- reactive({
    input$visualize == "Explore" && (
      input$embedding %in% c("PCA", "VAE", "UMAP"))
  })

  output$pc_slider2_cond <- reactive({
    input$plotPanels %in% c("Static 2D", "Interactive 2D", "Interactive 3D")
  })

  output$pc_slider3_cond <- reactive({
    input$plotPanels == "Interactive 3D"
  })

  output$shape_opts_cond <- reactive({
    input$plotPanels == "Static 2D" && sep_colors()
  })

  output$label_opts_cond <- reactive({
    input$plotPanels %in% c("Interactive 2D", "Interactive 3D") && sep_colors()
  })

  for (cond in output_conditions)
    outputOptions(output, cond, suspendWhenHidden = FALSE)

  # ---------------------
  # HANDLE BUTTON PRESSES
  # ---------------------
  running <- reactiveVal(run_default)

  observeEvent(input$start, {
    shinyjs::hide("start")
    shinyjs::show("stop")
    running(TRUE)
  })

  observeEvent(input$stop, {
    shinyjs::hide("stop")
    shinyjs::show("start")
    running(FALSE)
  })

  # dynamic input picker IDs
  app_state_selected <- reactiveVal(app_cat_selected)

  # update app state
  observeEvent(input$rowby, {
    cat <- isolate(input$category)
    app_state <- app_state_selected()
    app_state[[cat]]$rowby <- input$rowby
    app_state_selected(app_state)
  }, ignoreInit = TRUE)

  observeEvent(input$colby, {
    cat <- isolate(input$category)
    app_state <- app_state_selected()
    app_state[[cat]]$colby <- input$colby
    app_state_selected(app_state)
  }, ignoreInit = TRUE)

  observeEvent(input$colorby, {
    cat <- isolate(input$category)
    app_state <- app_state_selected()
    app_state[[cat]]$colorby <- input$colorby
    app_state_selected(app_state)
  }, ignoreInit = TRUE)

  observeEvent(input$shapeby, {
    cat <- isolate(input$category)
    app_state <- app_state_selected()
    app_state[[cat]]$shapeby <- input$shapeby
    app_state_selected(app_state)
  }, ignoreInit = TRUE)

  observeEvent(input$labelby, {
    cat <- isolate(input$category)
    app_state <- app_state_selected()
    app_state[[cat]]$labelby <- input$labelby
    app_state_selected(app_state)
  }, ignoreInit = TRUE)

  observeEvent(input$filterby, {
    cat <- isolate(input$category)
    app_state <- app_state_selected()
    app_state[[cat]]$filterby <- input$filterby
    app_state_selected(app_state)
  }, ignoreInit = TRUE)

  observeEvent(input$selectby, {
    cat <- isolate(input$category)
    app_state <- app_state_selected()
    fil <- app_state[[cat]]$filterby
    app_state[[cat]]$selectby[[fil]] <- as.character(input$selectby)
    app_state_selected(app_state)
    # NULL is a problem
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$threby, {
    cat <- isolate(input$category)
    sca <- isolate(input$scaling)
    app_state <- app_state_selected()
    app_state[[cat]]$threby[[sca]] <- input$threby
    app_state_selected(app_state)
  }, ignoreInit = TRUE)

  update_dynam_picker <- function(inputId, label, selected, choices)
  {
    updatePickerInput(
      session,
      inputId = inputId,
      label = label,
      selected = selected,
      choices = choices
    )
  }

  # update input pickers
  observeEvent(input$category, {
    app_state <- app_state_selected()

    cat <- input$category
    sca <- input$scaling

    row_axs <- get_row_axs(cat)
    col_axs <- get_col_axs(cat)

    row_choices <- app_row_choices[[row_axs]]
    col_choices <- app_col_choices[[col_axs]]
    cat_choices <- app_cat_choices[[cat]]

    cat_selected <- app_state[[cat]]
    filterby <- cat_selected$filterby

    update_dynam_picker(
      "rowby",
      sprintf("Sample Subset (%s)", cat),
      selected = cat_selected$rowby,
      choices = row_choices$rowby
    )

    update_dynam_picker(
      "colby",
      sprintf("Feature Subset (%s)", cat),
      selected = cat_selected$colby,
      choices = col_choices$colby
    )

    update_dynam_picker(
      "colorby",
      sprintf("Color By (%s)", cat),
      selected = cat_selected$colorby,
      choices = row_choices$full_chas
    )

    update_dynam_picker(
      "shapeby",
      sprintf("Shape By (%s)", cat),
      selected = cat_selected$shapeby,
      choices = row_choices$full_chas
    )

    update_dynam_picker(
      "labelby",
      sprintf("Label By (%s)", cat),
      selected = cat_selected$labelby,
      choices = row_choices$full_chas
    )

    update_dynam_picker(
      "filterby",
      sprintf("Filter By (%s)", cat),
      selected = filterby,
      choices = row_choices$safe_chas
    )

    update_dynam_picker(
      "selectby",
      sprintf("Selections (%s, %s)", cat, filterby),
      selected = cat_selected$selectby[[filterby]],
      choices = row_choices$selectby[[filterby]]
    )

    update_dynam_picker(
      "threby",
      sprintf("Threshold (%s, %s)", cat, sca),
      selected = cat_selected$threby[[sca]],
      choices = cat_choices$threby[[sca]]
    )
  })

  observeEvent(input$scaling, {
    app_state <- app_state_selected()

    cat <- input$category
    sca <- input$scaling

    cat_choices <- app_cat_choices[[cat]]

    cat_selected <- app_state[[cat]]
    filterby <- cat_selected$filterby

    update_dynam_picker(
      "threby",
      sprintf("Threshold (%s, %s)", cat, sca),
      selected = cat_selected$threby[[sca]],
      choices = cat_choices$threby[[sca]]
    )
  })

  observeEvent(input$filterby, {
    app_state <- app_state_selected()

    cat <- input$category
    sca <- input$scaling

    row_axs <- get_row_axs(cat)

    row_choices <- app_row_choices[[row_axs]]
    cat_choices <- app_cat_choices[[cat]]

    cat_selected <- app_state[[cat]]
    filterby <- cat_selected$filterby

    update_dynam_picker(
      "selectby",
      sprintf("Selections (%s, %s)", cat, filterby),
      selected = cat_selected$selectby[[filterby]],
      choices = row_choices$selectby[[filterby]]
    )
  })

  # a copy of all reactives that can stop running
  iplot <- reactiveValues()

  observe({
    if (running())
    {
      app_state <- app_state_selected()

      for (id in bookmarkable_ids)
        iplot[[id]] <- input[[id]]

      for (id in dynam_picker_input_ids)
        iplot[[id]] <- app_state[[iplot$category]][[id]]
    }
  })

  rowi <- reactive({
    parse_opt(iplot$rowby)
  })
  coli <- reactive({
    parse_opt(iplot$colby)
  })
  thre <- reactive(as.numeric(iplot$threby))

  colorby <- reactive(iplot$colorby)
  shapeby <- reactive({
    if (!sep_colors())
      return(colorby())
    iplot$shapeby
  })
  labelby <- reactive({
    if (!sep_colors())
      return(colorby())
    iplot$labelby
  })
  filterby <- reactive({
    iplot$filterby
  })

  observeEvent(input$request_analysis, {
    if (use_local_storage)
    {
      notif("Cannot make custom requests while offline.", "error")
      return()
    }

    showModal(modalDialog(
      title = HTML("<b>Request Custom Analysis</b>"), easyClose = TRUE,
      select_panel("req_emb", "Desired Embedding", emb_options),
      hr(style = "border-top: 1px solid #000000;"),
      select_panel("req_cat", "Desired Category", name_cat),
      conditionalPanel(
        condition = "input.req_emb != 'Sets'",
        select_panel("req_row", "Desired Sample Subset", NULL),
        select_panel("req_col", "Desired Feature Subset", NULL),
        select_panel("req_nor", "Desired Normalization", nor_options)
      ),
      select_panel("req_sca", "Desired Scaling", sca_options),
      conditionalPanel(
        condition = "input.req_emb == 'PCA' || input.req_emb == 'VAE' || input.req_emb == 'UMAP'",
        select_panel("req_vis", "Desired Visualization", vis_options)
      ),
      numericInput("req_com", "Desired Component", 10, min = 3, step = 1),
      conditionalPanel(
        condition = "(input.req_emb == 'PCA' || input.req_emb == 'VAE' || input.req_emb == 'UMAP') &&
        input.req_vis == 'tSNE'",
        numericInput("req_dim", "Desired Dimension", 2, min = 2, max = 3, step = 1)
      ),
      conditionalPanel(
        condition = "input.req_emb == 'PHATE' || input.req_vis == 'tSNE' ||
          (input.req_emb == 'UMAP' && input.req_vis != 'Summarize')",
        numericInput("req_per", "Desired Perplexity", 10, min = 0, max = 100, step = 1)
      ),
      conditionalPanel(
        condition = "input.req_emb == 'VAE'",
        numericInput("req_bat", "Desired Batch Size", 64, min = 1, step = 1)
      ),
      conditionalPanel(
        condition = "input.req_emb == 'Sets'",
        numericInput("req_thr", "Desired Threshold", 0.5, min = 0, max = 1, step = 0.1^num_digits),
        select_panel("req_cha", "Desired Characteristic", NULL)
      ),
      textInput("req_aut", "Author Name"),
      footer = tagList(
        action("submit_request", "Submit", "cloud", "#FFF", "#0064C8", "#00356B"),
        modalButton("Dismiss")
      )
    ))
  })

  observeEvent(input$req_cat, {
    updatePickerInput(session, "req_row", choices = app_row_choices[[row_axs]]$rowby)
    updatePickerInput(session, "req_col", choices = app_col_choices[[col_axs]]$colby)
    updatePickerInput(session, "req_cha", choices = app_row_choices[[row_axs]]$safe_chas)
  })

  user_requests <- reactiveVal(default_user_requests)

  observeEvent(input$submit_request, {
    test <- make_requests(
      cat = input$req_cat, row = parse_opt(input$req_row),
      col = parse_opt(input$req_col), sca = input$req_sca,
      nor = input$req_nor, emb = input$req_emb, vis = input$req_vis, com = input$req_com,
      dim = input$req_dim, per = input$req_per, bat = input$req_bat, thr = input$req_thr,
      cha = parse_opt(input$req_cha), aut = input$req_aut
    )

    if (length(test) > 0) # valid request!
    {
      u_requests <- make_requests()
      if (find_aws_s3("Sessions/user_requests.rds"))
        u_requests <- load_aws_s3("Sessions/user_requests.rds")

      if (test$FILE_LOCATION %nin% u_requests$FILE_LOCATION &&
          test$FILE_LOCATION %nin% app_requests$FILE_LOCATION)
      {
        test$REQUEST_ID <- get_request_id()
        u_requests <- rbind_req(u_requests, test)
        save_aws_s3(u_requests, "Sessions/user_requests.rds")
        user_requests(u_requests)
        notif("Your request has been submitted!", "message")
      }
      else
        notif("Cannot make duplicate requests - please reach out to the ERCC
              if you believe this message is in error.", "warning")
    }
    else
      notif("Failed to make request - check your inputs!", "error")
  })

  observeEvent(input$instructions, {
    showModal(modalDialog(
      title = HTML("<b>Instructions</b>"), easyClose = TRUE, HTML(instructions)))
  })

  observeEvent(input$citations, {
    showModal(modalDialog(
      title = HTML("<b>Citations</b>"), easyClose = TRUE, HTML(citations)))
  })

  observeEvent(input$refresh, {
    if (!use_local_storage && find_aws_s3("Sessions/user_requests.rds"))
      user_requests(load_aws_s3("Sessions/user_requests.rds"))
  })

  # ----------------
  # INPUT PROCESSING
  # ----------------

  width <- reactive({
    if (range_invalid(iplot$width, 1, 4000))
      return("100%")

    round(iplot$width, digits=0)
  })

  height <- reactive({
    if (range_invalid(iplot$height, 1, 4000))
    {
      range_invalid_notif("Graph Height", 1, 4000)
      return(graph_height)
    }
    round(iplot$height, digits=0)
  })

  text_scale <- reactive({
    if (range_invalid(iplot$text_scale, 0.01, 100))
    {
      range_invalid_notif("Text Scale", 0.01, 100)
      return(1)
    }
    round(iplot$text_scale, digits=2)
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
  sep_colors <- reactive("Separate Colors" %in% iplot$sMenu)
  boost <- reactive("Boost Graphics" %in% iplot$sMenu)
  not_rev <- reactive("Uninverted Colors" %in% iplot$sMenu)

  curr_adr <- reactiveVal("")

  cati <- reactive(iplot$category)
  peri <- reactive(iplot$perplexity)
  bati <- reactive(iplot$batch_size)

  # calculate which samples to keep after considering all metadata filters
  order <- reactive({
    get_row_axis(cati())$metadata
  })
  row_order <- reactive({
    subset_by_row(order(), cati(), rowi())
  })
  keep <- reactive({
    keep <- rep(TRUE, nrow(row_order()))
    row_axs <- get_row_axs(cati())

    for (char in app_row_choices[[row_axs]]$safe_chas)
    {
      cur_filter <- row_order()[[char]] %in%
        parse_opt(iplot$selectby[[char]])
      keep <- keep & cur_filter
    }

    keep
  })
  metadata <- reactive(row_order()[keep(),,drop=FALSE])

  colors <- reactive({
    metadata()[, colorby()]
  })
  shapes <- reactive(metadata()[, shapeby()])
  labels <- reactive(sprintf("%s: %s", labelby(), metadata()[, labelby()]))
  my_chars <- reactive({
    app_state <- app_state_selected()
    x <- app_state[[cati()]]$selectby[[filterby()]]
    parse_opt(x)
  })

  # the number of features before dimensionality reduction
  num_feat <- reactive({
    get_col_subset_lengths(cati())[[coli()]]
  })

  # numeric data for displaying / downloading
  num_data <- reactiveVal()

  # the title of the plot, to be embedded or displayed as plain text
  title_text <- reactive({
    if (iplot$embedding == "Sets")
    {
      return(sprintf(
        "%s-Grouped Features on %s [%s (%s), %s (%s)]",
        filterby(), cati(), rowi(), nrow(num_data()), coli(), ncol(num_data())
      ))
    }

    nei <- ifelse(
      iplot$embedding == "UMAP" || iplot$embedding == "PHATE" ||
         iplot$visualize == "tSNE",
      sprintf(", %s Neighbors", iplot$perplexity), "")

    # make a noun for the visualization
    vis_as_noun <- ""
    if (iplot$embedding != "PHATE")
      vis_as_noun <- rep_str(iplot$visualize, vis_options,
                             c("Exploration of ", "Summary of ", "tSNE of "))

    sprintf("%s%s on %s [%s (%s), %s (%s)]%s", vis_as_noun,
            iplot$embedding, cati(), rowi(), sum(keep()), coli(), num_feat(), nei)
  })

  title_embed <- reactive({
    if (!title_access())
      return(NULL)
    title_text()
  })

  paint <- reactive({
    custom_color_scales <- get_row_axis(cati())$color_scales

    if (iplot$palette == "Custom") # requesting a custom color palette
    {
      if (iplot$visualize != "Summarize" && length(custom_color_scales) > 0)
      {
        # for scatterplots, see if colorby() has a corresponding palette available
        if (iplot$embedding != "Sets" && colorby() %in% names(custom_color_scales))
        {
          new <- custom_color_scales[[colorby()]]

          if (check_custom_colors(colors(), names(new)))
            return(new)
        }

        # for sets, see if filterby() has a corresponding palette available
        if (iplot$embedding == "Sets" && filterby() %in% names(custom_color_scales))
        {
          new <- custom_color_scales[[filterby()]] %>% unlist()

          if (check_custom_colors(colors(), names(new)))
            return(new)
        }
      }

      # if no custom palette works, do rainbow for scatterplots and inferno for sets
      if (iplot$embedding == "Sets")
        return(color_seq(5, "Inferno", not_rev()))
      return(color_seq(length(unique(colors())), "Rainbow", !not_rev()))
    }

    # if a builtin palette is requested, just call color_seq
    num <- length(unique(colors()))

    if (iplot$embedding == "Sets")
      num <- 5

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
    if (iplot$embedding == "VAE" && which(nor_options == iplot$normalization) > 2)
      return(NULL)

    if (iplot$embedding == "Sets")
    {
      addr <- make_sets_name(cati(), iplot$scaling, thre(), filterby())
      curr_adr(addr)
      data <- load_store(addr)
      if (is.null(data))
        return(NULL)
      data <- data[,my_chars(),drop=FALSE] %>% get_row_sub(cati(), coli())

      num_data(data)

      data <- truncate_rows(data, upse_feat()) %>%
        set_f1_f2(iplot$set_f1, iplot$set_f2) %>% num_nan_binary()

      if (ncol(data) == 1)
        return(venn1_custom(data, legend()))

      if (!legend())
        colnames(data) <- seq_len(ncol(data))

      return(upset_custom(data, nintersect(), bar_frac(), !legend(), text_scale()))
    }

    if (iplot$embedding == "PHATE")
    {
      addr <- make_phate_name(cati(), rowi(), coli(), iplot$scaling, iplot$normalization, 2, peri())
      curr_adr(addr)
      data <- load_store(addr)
      data <- data[keep(), , drop = FALSE]

      num_data(data)

      return(ggplot2_2d(
        data[,1], data[,2], colors(), shapes(), paint(), NULL,
        legend(), title_embed(), pc("1"), pc("2")))
    }

    addr <- make_pvu_name(
      cati(), rowi(), coli(), iplot$scaling, iplot$normalization,
      iplot$embedding, iplot$visualize,
      pc_cap, 2, peri(), bati())
    curr_adr(addr)

    data <- load_store(addr)

    if (is.null(data))
      return(NULL)

    if (iplot$visualize == "Summarize")
    {
      num_data(data)

      if (iplot$embedding == "PCA")
        return(ggplot2_pca_sum(data, pc_cap, legend(), title_embed()))

      if (iplot$embedding == "VAE")
        return(ggplot2_vae_sum(data, !not_rev(), legend(), title_embed()))

      if (iplot$embedding == "UMAP")
        return(ggplot2_umap_sum(knn_label_matrix(data, colors()), paint(), title_embed()))
    }

    data <- data[keep(), , drop = FALSE]
    num_data(data)

    if (iplot$visualize == "Explore")
    {
      return(ggplot2_2d(
        data[,iplot$pc1], data[,iplot$pc2], colors(), shapes(), paint(), NULL,
        legend(), title_embed(), pc(iplot$pc1), pc(iplot$pc2)))
    }

    if (iplot$visualize == "tSNE")
    {
      return(ggplot2_2d(
        data[,1], data[,2], colors(), shapes(), paint(), NULL,
        legend(), title_embed(), pc("1"), pc("2")))
    }
  })

  plotly2_data <- reactive({
    if (iplot$embedding == "VAE" && which(nor_options == iplot$normalization) > 2)
      return(NULL)

    if (iplot$embedding == "Sets")
    {
      addr <- make_sets_name(cati(), iplot$scaling, thre(), filterby())
      curr_adr(addr)
      data <- load_store(addr)
      if (is.null(data))
        return(NULL)
      data <- data[,my_chars(),drop=FALSE] %>% get_row_sub(cati(), coli())

      num_data(data)

      data <- truncate_rows(data, heat_feat()) %>%
        sort_row_sums() %>% set_f1_f2(iplot$set_f1, iplot$set_f2)

      return(plotly_heatmap_variance(data, paint(), title_embed(), legend(), boost()))
    }

    if (iplot$embedding == "PHATE")
    {
      addr <- make_phate_name(cati(), rowi(), coli(), iplot$scaling, iplot$normalization, 2, peri())
      curr_adr(addr)
      data <- load_store(addr)
      data <- data[keep(),,drop=FALSE]

      num_data(data)

      return(plotly_2d(
        data[,1], data[,2], colors(), labels(), paint(),
        FALSE, legend(), title_embed(), pc("1"), pc("2")))
    }

    addr <- make_pvu_name(
      cati(), rowi(), coli(), iplot$scaling, iplot$normalization, iplot$embedding, iplot$visualize,
      pc_cap, 2, peri(), bati())
    curr_adr(addr)

    data <- load_store(addr)

    if (is.null(data))
      return(NULL)

    if (iplot$visualize == "Summarize")
    {
      num_data(data)

      if (iplot$embedding == "PCA")
        return(plotly_pca_sum(data, pc_cap, FALSE, legend(), title_embed()))

      if (iplot$embedding == "VAE")
        return(plotly_vae_sum(data, FALSE, !not_rev(), legend(), title_embed()))

      if (iplot$embedding == "UMAP")
        return(plotly_umap_sum(knn_label_matrix(data, colors()), paint(),
                               title_embed(), legend(), FALSE, boost()))
    }

    data <- data[keep(),,drop=FALSE]
    num_data(data)

    if (iplot$visualize == "Explore")
    {
      return(plotly_2d(
        data[,iplot$pc1], data[,iplot$pc2], colors(), labels(), paint(),
        FALSE, legend(), title_embed(), pc(iplot$pc1), pc(iplot$pc2)))
    }

    if (iplot$visualize == "tSNE")
    {
      return(plotly_2d(
        data[,1], data[,2], colors(), labels(), paint(),
        FALSE, legend(), title_embed(), pc("1"), pc("2")))
    }
  })

  plotly3_data <- reactive({
    if (iplot$embedding == "VAE" && which(nor_options == iplot$normalization) > 2)
      return(NULL)

    if (iplot$embedding == "Sets")
    {
      addr <- make_sets_name(cati(), iplot$scaling, thre(), filterby())
      curr_adr(addr)
      data <- load_store(addr)
      if (is.null(data))
        return(NULL)
      data <- data[,my_chars(),drop=FALSE] %>% get_row_sub(cati(), coli())

      num_data(data)

      data <- truncate_rows(data, dend_feat()) %>% set_f1_f2(iplot$set_f1, iplot$set_f2)

      return(plotly_heatmap_dendrogram(data, paint(), title_embed(), legend(), boost()))
    }

    if (iplot$embedding == "PHATE")
    {
      addr <- make_phate_name(cati(), rowi(), coli(), iplot$scaling, iplot$normalization, 3, peri())
      curr_adr(addr)
      data <- load_store(addr)
      data <- data[keep(),,drop=FALSE]

      num_data(data)

      return(plotly_3d(
        data[,1], data[,2], data[,3], colors(), labels(), paint(),
        legend(), title_embed(), pc("1"), pc("2"), pc("3")))
    }

    addr <- make_pvu_name(
      cati(), rowi(), coli(), iplot$scaling, iplot$normalization, iplot$embedding, iplot$visualize,
      pc_cap, 3, peri(), bati())
    curr_adr(addr)

    data <- load_store(addr)

    if (is.null(data))
      return(NULL)

    if (iplot$visualize == "Summarize")
    {
      num_data(data)

      if (iplot$embedding == "PCA")
        return(plotly_pca_sum(data, pc_cap, TRUE, legend(), title_embed()))

      if (iplot$embedding == "VAE")
        return(plotly_vae_sum(data, TRUE, !not_rev(), legend(), title_embed()))

      if (iplot$embedding == "UMAP")
        return(plotly_umap_sum(knn_label_matrix(data, colors()), paint(),
                               title_embed(), legend(), TRUE, boost()))
    }

    data <- data[keep(),,drop=FALSE]
    num_data(data)

    if (iplot$visualize == "Explore")
    {
      return(plotly_3d(
        data[,iplot$pc1], data[,iplot$pc2], data[,iplot$pc3],
        colors(), labels(), paint(), legend(), title_embed(),
        pc(iplot$pc1), pc(iplot$pc2), pc(iplot$pc3)))
    }

    if (iplot$visualize == "tSNE")
    {
      return(plotly_3d(
        data[,1], data[,2], data[,3], colors(), labels(), paint(),
        legend(), title_embed(),  pc("1"), pc("2"), pc("3")))
    }
  })

  # generates beeswarm data
  beeswarm_data <- reactive({
    if (!(iplot$embedding %in% c('PCA', 'VAE', 'UMAP')))
      return(NULL)

    addr <- make_pvu_name(
      cati(), rowi(), coli(), iplot$scaling, iplot$normalization, iplot$embedding, "Explore",
      pc_cap, 3, peri(), bati())
    curr_adr(addr)

    data <- load_store(addr)[keep(),iplot$pc1]
    data <- cbind.data.frame(colors(), data)
    colnames(data) <- c(colorby(), pc(iplot$pc1))

    boxplot_beeswarm(data, paint(), title_embed(), legend())
  })

  # -----------------
  # DOWNLOAD HANDLERS
  # -----------------

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

  output$download_num_data <- downloadHandler(
    filename = function() {
      sprintf("num_data_%s.csv", rep_str(title_text(), " ", "_"))
    },
    content = function(file) {
      if (!authenticated())
        return(NULL)

      write.csv(num_data(), file)
    }
  )

  output$download_metadata <- downloadHandler(
    filename = function() {
      sprintf("metadata_%s.csv", rep_str(title_text(), " ", "_"))
    },
    content = function(file) {
      if (!authenticated())
        return(NULL)

      write.csv(metadata(), file)
    }
  )

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

  output$requests_out <- renderDT({
    if (!authenticated())
      return(my_datatable())

    my_datatable(present_requests(app_requests))
  }, server = table_server_render)

  output$user_requests_out <- renderDT({
    if (!authenticated())
      return(my_datatable())

    my_datatable(present_requests(user_requests()))
  }, server = table_server_render)

  output$ggplot2_out <- renderPlot({prep_plot(ggplot2_data())})
  output$plotly2_out <- renderPlotly({prep_plot(plotly2_data())})
  output$plotly3_out <- renderPlotly({prep_plot(plotly3_data())})
  output$beeswarm_out <- renderPlot({prep_plot(beeswarm_data())})

  output$num_data_table <- renderDT({
    if (!authenticated())
      return(my_datatable())

    my_datatable(data.frame(num_data()))
  }, server = table_server_render)

  output$metadata_table <- renderDT({
    if (!authenticated())
      return(my_datatable())

    my_datatable(data.frame(metadata()))
  }, server = table_server_render)

  output$legend_out <- renderDT({
    if (!authenticated())
      return(my_datatable())

    my_datatable(generate_legend_table(colors()))
  }, server = table_server_render)

  output$console_out <- renderPrint({
    if ("address" %in% iplot$console)
      sprintf_clean("address=%s", format_print_simple(curr_adr()))
    if ("num_data" %in% iplot$console)
      sprintf_clean("dim(num_data)=%s", format_print_simple(dim(num_data())))
    if ("metadata" %in% iplot$console)
      sprintf_clean("dim(metadata)=%s", format_print_simple(dim(metadata())))
    if ("app_requests" %in% iplot$console)
      sprintf_clean("dim(app_requests)=%s", format_print_simple(dim(app_requests)))
    if ("user_requests" %in% iplot$console)
      sprintf_clean("dim(user_requests)=%s", format_print_simple(dim(user_requests())))

    for (id in intersect(setdiff(iplot$console, "console"), names(input)))
    {
      sprintf_clean("%s=%s", id, format_print_simple(input[[id]]))
    }
  })

  # -----------------
  # DIRECT UI OUTPUTS
  # -----------------

  output$requestsUI <- renderUI({
    DTOutput("requests_out") %>% my_spin()
  })

  output$pendingRequestsUI <- renderUI({
    DTOutput("user_requests_out") %>% my_spin()
  })

  output$ggplot2UI <- renderUI({
    plotOutput("ggplot2_out", width=width(), height=height()) %>% my_spin()
  })

  output$plotly2UI <- renderUI({
    plotlyOutput("plotly2_out", width=width(), height=height()) %>% my_spin()
  })

  output$plotly3UI <- renderUI({
    plotlyOutput("plotly3_out", width=width(), height=height()) %>% my_spin()
  })

  output$beeswarmUI <- renderUI({
    plotOutput("beeswarm_out", width=width(), height=height()) %>% my_spin()
  })

  output$legendUI <- renderUI({
    if (legend() || !(iplot$plotPanels %in% c(
      "Static 2D", "Interactive 2D", "Interactive 3D", "Boxplot")))
      return(NULL)
    DTOutput("legend_out") %>% my_spin()
  })

  output$num_dataUI <- renderUI({
    DTOutput("num_data_table") %>% my_spin()
  })

  output$metadataUI <- renderUI({
    DTOutput("metadata_table") %>% my_spin()
  })

  # -----------
  # BOOKMARKING
  # -----------

  session$onSessionEnded(function() {
    stopApp()
  })
  setBookmarkExclude(bookmark_exclude_vector)

  onBookmark(function(state) {
    session_data <- session_data_template

    for (id in picker_input_ids)
      session_data[["pickerInput"]][[id]] <- input[[id]]

    for (id in numeric_input_ids)
      session_data[["numericInput"]][[id]] <- input[[id]]

    for (id in numeric_range_input_ids)
      session_data[["numericRangeInput"]][[id]] <- input[[id]]

    for (id in slider_input_ids)
      session_data[["sliderInput"]][[id]] <- input[[id]]

    for (id in tabset_panel_ids)
      session_data[["tabsetPanel"]][[id]] <- input[[id]]

    # get the vector of all session IDs
    num_sessions <- 0
    if (find_store("Sessions/num_sessions.rds"))
      num_sessions <- load_store("Sessions/num_sessions.rds")

    # find a session ID that is not used
    i <- smallest_missing(num_sessions)

    # add the session ID to the list and save the session
    save_store(c(num_sessions, i), "Sessions/num_sessions.rds")
    save_store(session_data, sprintf("Sessions/session_%s.rds", i))

    # the bookmark is simply the numerical ID for the session
    state$values$user_id <- i
  })

  # restore compressed data when link is followed
  onRestore(function(state) {
    # get the vector of all session IDs
    num_sessions <- 0
    if (find_store("Sessions/num_sessions.rds"))
      num_sessions <- load_store("Sessions/num_sessions.rds")

    # if the ID is invalid, load nothing
    id <- state$values$user_id
    if (!(id %in% num_sessions))
      return(NULL)

    # otherwise, load the appropriate item
    session_data <- load_store(sprintf("Sessions/session_%s.rds", id))

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

cat_f("APP BUILD TIME: %.1f (sec)\n", net_time())

# -----------
# RUN THE APP
# -----------
shinyApp(ui = ui, server = server, enableBookmarking = "url")
