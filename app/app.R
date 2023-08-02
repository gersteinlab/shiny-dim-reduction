# The purpose of this file is to run an application for
# visualizing dimensionality reduction results.

source("install.R")
source_app("dashboard.R")

server <- function(input, output, session) {
  if (run_default)
    shinyjs::hide("start")
  else
    shinyjs::hide("stop")

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
(1) invalid parameters<br>
(2) nonexistent analysis", "error")
      return(default_plot)
    }

    notif(sprintf("Plot generation was successful.<br>
Seconds elapsed: %.1f", time_diff(start)), "message")

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
    input$plotPanels == "Static 2D" &&
      "Separate Colors" %in% input$sMenu
  })

  output$label_opts_cond <- reactive({
    input$plotPanels %in% c("Interactive 2D", "Interactive 3D") &&
      "Separate Colors" %in% input$sMenu
  })

  for (cond in output_conditions)
    outputOptions(output, cond, suspendWhenHidden = FALSE)

  # ----------------------
  # INPUT TO DYNAMIC STATE
  # ----------------------

  # dynamic input picker IDs
  dynam_state <- do.call(reactiveValues, app_cat_selected)

  # syntactic sugar
  iso_cat <- function()
  {
    isolate(input$category)
  }

  observeEvent(input$rowby, {
    dynam_state$rowby[[iso_cat()]] <- input$rowby
  }, ignoreInit = TRUE)

  observeEvent(input$colby, {
    dynam_state$colby[[iso_cat()]] <- input$colby
  }, ignoreInit = TRUE)

  observeEvent(input$colorby, {
    dynam_state$colorby[[iso_cat()]] <- input$colorby
  }, ignoreInit = TRUE)

  observeEvent(input$shapeby, {
    dynam_state$shapeby[[iso_cat()]] <- input$shapeby
  }, ignoreInit = TRUE)

  observeEvent(input$labelby, {
    dynam_state$labelby[[iso_cat()]] <- input$labelby
  }, ignoreInit = TRUE)

  observeEvent(input$filterby, {
    dynam_state$filterby[iso_cat()] <- input$filterby
  }, ignoreInit = TRUE)

  observeEvent(input$selectby, {
    cat <- iso_cat()
    fil <- dynam_state$filterby[[cat]]
    # handle NULL
    safe_selectby <- as.character(input$selectby)
    dynam_state$selectby[[cat]][[fil]] <- safe_selectby
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$threby, {
    cat <- iso_cat()
    sca <- isolate(input$scaling)
    dynam_state$threby[[cat]][[sca]] <- input$threby
  }, ignoreInit = TRUE)

  # ----------------------
  # DYNAMIC STATE TO INPUT
  # ----------------------

  # simplified version of updatePickerInput
  simple_picker_update <- function(
    inputId, label = NULL, selected = NULL, choices = NULL)
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
    cat <- input$category
    row_choices <- get_app_row_choices(cat)
    filterby <- dynam_state$filterby[[cat]]

    simple_picker_update(
      "rowby",
      sprintf("Sample Subset (%s)", cat),
      selected = dynam_state$rowby[[cat]],
      choices = row_choices$rowby
    )

    simple_picker_update(
      "colby",
      sprintf("Feature Subset (%s)", cat),
      selected = dynam_state$colby[[cat]],
      choices = get_app_col_choices(cat)$colby
    )

    simple_picker_update(
      "colorby",
      sprintf("Color By (%s)", cat),
      selected = dynam_state$colorby[[cat]],
      choices = row_choices$safe_chas
    )

    simple_picker_update(
      "shapeby",
      sprintf("Shape By (%s)", cat),
      selected = dynam_state$shapeby[[cat]],
      choices = row_choices$safe_chas
    )

    simple_picker_update(
      "labelby",
      sprintf("Label By (%s)", cat),
      selected = dynam_state$labelby[[cat]],
      choices = row_choices$full_chas
    )

    simple_picker_update(
      "filterby",
      sprintf("Filter By (%s)", cat),
      selected = filterby,
      choices = row_choices$safe_chas
    )

    simple_picker_update(
      "selectby",
      sprintf("Selections (%s, %s)", cat, filterby),
      selected = dynam_state$selectby[[cat]][[filterby]],
      choices = row_choices$selectby[[filterby]]
    )

    sca <- input$scaling
    simple_picker_update(
      "threby",
      sprintf("Threshold (%s, %s)", cat, sca),
      selected = dynam_state$threby[[cat]][[sca]],
      choices = app_cat_choices[[cat]]$threby[[sca]]
    )
  })

  observeEvent(input$scaling, {
    cat <- input$category
    sca <- input$scaling

    simple_picker_update(
      "threby",
      sprintf("Threshold (%s, %s)", cat, sca),
      selected = dynam_state$threby[[cat]][[sca]],
      choices = app_cat_choices[[cat]]$threby[[sca]]
    )
  })

  observeEvent(input$filterby, {
    cat <- input$category
    row_choices <- get_app_row_choices(cat)
    filterby <- dynam_state$filterby[[cat]]

    simple_picker_update(
      "selectby",
      sprintf("Selections (%s, %s)", cat, filterby),
      selected = dynam_state$selectby[[cat]][[filterby]],
      choices = row_choices$selectby[[filterby]]
    )
  })

  # UPDATE NORMALIZATION
  observeEvent(input$embedding, {
    updatePickerInput(
      session, "normalization",
      choices = nor_options_by_emb(input$embedding)
    )
  })

  # ----------------
  # RUNNING PLOTTING
  # ----------------
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

  # a copy of all reactives that can stop running
  iplot <- reactiveValues()

  observe({
    if (running())
    {
      for (id in bookmarkable_ids)
        iplot[[id]] <- input[[id]]

      # dynamic input propagates too
      cat <- iplot$category
      iplot$rowby <- dynam_state$rowby[[cat]]
      iplot$colby <- dynam_state$colby[[cat]]
      iplot$colorby <- dynam_state$colorby[[cat]]
      iplot$shapeby <- dynam_state$shapeby[[cat]]
      iplot$labelby <- dynam_state$labelby[[cat]]
      iplot$filterby <- dynam_state$filterby[[cat]]
      iplot$selectby <- dynam_state$selectby[[cat]]
      iplot$threby <- dynam_state$threby[[cat]][[iplot$scaling]]
    }
  })

  rowi <- reactive(parse_opt(iplot$rowby))
  coli <- reactive(parse_opt(iplot$colby))
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

  # --------------
  # PRESENT MODALS
  # --------------
  observeEvent(input$draft_request, {
    showModal(draft_request_modal)
  })

  observeEvent({list(input$draft_request, input$req_cat)}, {
    if (is.null(input$req_cat))
      return(NULL)
    row_choices <- get_app_row_choices(input$req_cat)
    col_choices <- get_app_col_choices(input$req_cat)
    updatePickerInput(
      session, "req_row",
      choices = row_choices$rowby)
    updatePickerInput(
      session, "req_col",
      choices = col_choices$colby)
    updatePickerInput(
      session, "req_cha",
      choices = row_choices$safe_chas)
  }, ignoreInit = TRUE)

  observeEvent({list(input$draft_request, input$req_emb)}, {
    if (is.null(input$req_emb))
      return(NULL)
    updatePickerInput(
      session, "req_nor",
      choices = nor_options_by_emb(input$req_emb)
    )
  }, ignoreInit = TRUE)

  user_requests <- reactiveVal(get_requests(user_req_file))

  observeEvent(input$submit_request, {
    tryCatch({
      test <- make_requests(
        cat = input$req_cat, row = parse_opt(input$req_row),
        col = parse_opt(input$req_col), sca = input$req_sca,
        nor = input$req_nor, emb = input$req_emb, vis = input$req_vis, com = input$req_com,
        dim = input$req_dim, per = input$req_per, bat = input$req_bat, thr = input$req_thr,
        cha = parse_opt(input$req_cha), aut = input$req_aut
      )
    }, error = function(e){
      notif("Failed to make request - check your inputs!", "error")
    })

    user_requests(get_requests(user_req_file))
    u_requests <- user_requests()
    all_prev_locations <- c(
      app_requests$FILE_LOCATION,
      u_requests$FILE_LOCATION
    )

    if (test$FILE_LOCATION %in% all_prev_locations)
      notif("Cannot make duplicate requests - please reach out to the ERCC
              if you believe this message is in error.", "warning")
    else
    {
      u_requests <- rbind_req2(u_requests, test)
      save_store(u_requests, user_req_file)
      user_requests(u_requests)
      notif("Your request has been submitted!", "message")
    }
  })

  observeEvent(input$refresh, {
    user_requests(get_requests(user_req_file))
  })

  observeEvent(input$notes, {
    showModal(notes_modal)
  })

  output$cat_notes_text <- renderText(
    categories[[input$cat_notes]]$note
  )

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
  peri <- reactive(as.integer(iplot$perplexity))
  bati <- reactive(as.integer(iplot$batch_size))

  # calculate which samples to keep after considering all metadata filters
  order <- reactive({
    get_row_axis(cati())$metadata
  })
  row_order <- reactive({
    if (rowi() == "Total")
      return(order())
    get_row_sub(cati(), rowi()) %>% subset_by_row(order(), .)
  })
  keep <- reactive({
    keep <- rep(TRUE, nrow(row_order()))
    row_axs <- get_row_axs(cati())

    for (char in get_app_row_choices(cati())$safe_chas)
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
    x <- dynam_state$selectby[[cati()]][[filterby()]]
    parse_opt(x)
  })

  # the number of features before dimensionality reduction
  num_feat <- reactive({
    get_col_sub_lengths(cati())[[coli()]]
  })

  # numeric data for displaying / downloading
  num_data <- reactiveVal()

  # the title of the plot, to be embedded or displayed as plain text
  title_text <- reactive({
    if (iplot$embedding == "Sets")
    {
      row_opt <- get_opt(rowi(), nrow(num_data()))
      col_opt <- get_opt(coli(), ncol(num_data()))
      return(sprintf(
        "%s-Grouped Features on %s [%s, %s]",
        filterby(), cati(), row_opt, col_opt
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

    row_opt <- get_opt(rowi(), sum(keep()))
    col_opt <- get_opt(coli(), num_feat())
    sprintf("%s%s on %s [%s, %s]%s", vis_as_noun,
            iplot$embedding, cati(), row_opt, col_opt, nei)
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

          if (all(colors() %in% names(new)))
            return(new)
        }

        # for sets, see if filterby() has a corresponding palette available
        if (iplot$embedding == "Sets" && filterby() %in% names(custom_color_scales))
        {
          new <- custom_color_scales[[filterby()]] %>% unlist()

          if (all(colors() %in% names(new)))
            return(new)
        }
      }

      # if no custom palette works, do rainbow for scatterplots and inferno for sets
      if (iplot$embedding == "Sets")
        return(make_color_seq(5L, "Inferno") %>%
                 rev_color_seq(!not_rev()))

      return(num_unique(colors()) %>%
               make_color_seq("Rainbow") %>%
               rev_color_seq(!not_rev()))
    }

    # if a builtin palette is requested, just call make_color_seq
    num <- colors() %>% num_unique()
    if (iplot$embedding == "Sets")
      num <- 5

    make_color_seq(num, iplot$palette) %>% rev_color_seq(!not_rev())
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

    num_unique(shapes())
  })

  # ---------------
  # PLOT GENERATION
  # ---------------

  ggplot2_data <- reactive({
    if (iplot$embedding == "Sets")
    {
      addr <- name_sets_file(cati(), iplot$scaling, thre(), filterby())
      curr_adr(addr)
      data <- load_store(addr)
      if (is.null(data))
        return(NULL)
      data <- data[, my_chars(), drop = FALSE]
      if (coli() != "Total")
        data <- get_col_sub_names(cati(), coli()) %>%
        subset_by_row_names(data, .)

      num_data(data)

      data <- truncate_rows(data, upse_feat()) %>%
        set_f1_f2(iplot$set_f1, iplot$set_f2) %>% num_nan_binary()

      if (ncol(data) == 1)
        return(venn1_custom(nrow(data), ifelse(legend(), colnames(data)[1], "")))

      if (!legend())
        colnames(data) <- seq_len(ncol(data))

      return(upset_custom(data, nintersect(), bar_frac(), !legend(), text_scale()))
    }

    if (iplot$embedding == "PHATE")
    {
      addr <- name_phate_file(
        cati(), rowi(), coli(), iplot$scaling, iplot$normalization, 2, peri())
      curr_adr(addr)
      data <- load_store(addr)
      data <- data[keep(), , drop = FALSE]

      num_data(data)

      return(ggplot2_2d(
        data[,1], data[,2], colors(), shapes(), paint(),
        legend(), title_embed(), pc("1"), pc("2")))
    }

    addr <- name_pvu_file(
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
        data[,iplot$pc1], data[,iplot$pc2], colors(), shapes(), paint(),
        legend(), title_embed(), pc(iplot$pc1), pc(iplot$pc2)))
    }

    if (iplot$visualize == "tSNE")
    {
      return(ggplot2_2d(
        data[,1], data[,2], colors(), shapes(), paint(),
        legend(), title_embed(), pc("1"), pc("2")))
    }
  })

  plotly2_data <- reactive({
    if (iplot$embedding == "Sets")
    {
      addr <- name_sets_file(cati(), iplot$scaling, thre(), filterby())
      curr_adr(addr)
      data <- load_store(addr)
      if (is.null(data))
        return(NULL)
      data <- data[, my_chars(), drop = FALSE]
      if (coli() != "Total")
        data <- get_col_sub_names(cati(), coli()) %>%
        subset_by_row_names(data, .)

      num_data(data)

      data <- truncate_rows(data, heat_feat()) %>%
        sort_row_sums() %>% set_f1_f2(iplot$set_f1, iplot$set_f2)

      return(plotly_heatmap_variance(data, paint(), title_embed(), legend(), boost()))
    }

    if (iplot$embedding == "PHATE")
    {
      addr <- name_phate_file(cati(), rowi(), coli(), iplot$scaling, iplot$normalization, 2, peri())
      curr_adr(addr)
      data <- load_store(addr)
      data <- data[keep(),,drop=FALSE]

      num_data(data)

      return(plotly_2d(
        data[,1], data[,2], colors(), labels(), paint(),
        FALSE, legend(), title_embed(), pc("1"), pc("2")))
    }

    addr <- name_pvu_file(
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
    if (iplot$embedding == "Sets")
    {
      addr <- name_sets_file(cati(), iplot$scaling, thre(), filterby())
      curr_adr(addr)

      data <- load_store(addr)
      if (is.null(data))
        return(NULL)

      data <- subset_by_col(data, my_chars())
      if (coli() != "Total")
        data <- get_col_sub_names(cati(), coli()) %>%
        subset_by_row_names(data, .)

      num_data(data)

      data <- truncate_rows(data, dend_feat()) %>% set_f1_f2(iplot$set_f1, iplot$set_f2)

      return(plotly_heatmap_dendrogram(data, paint(), title_embed(), legend(), boost()))
    }

    if (iplot$embedding == "PHATE")
    {
      addr <- name_phate_file(
        cati(), rowi(), coli(),
        iplot$scaling, iplot$normalization, 3, peri())
      curr_adr(addr)

      data <- load_store(addr)
      if (is.null(data))
        return(NULL)

      data <- data[keep(),,drop=FALSE]

      num_data(data)

      return(plotly_3d(
        data[,1], data[,2], data[,3], colors(), labels(), paint(),
        legend(), title_embed(), pc("1"), pc("2"), pc("3")))
    }

    addr <- name_pvu_file(
      cati(), rowi(), coli(), iplot$scaling,
      iplot$normalization, iplot$embedding, iplot$visualize,
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

    data <- subset_by_row(data, keep())
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

    addr <- name_pvu_file(
      cati(), rowi(), coli(), iplot$scaling,
      iplot$normalization, iplot$embedding, "Explore",
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

  output$download_num_data <- downloadHandler(
    filename = function() {
      sprintf("num_data_%s.csv", rep_str(title_text(), " ", "_"))
    },
    content = function(file) {
      write.csv(num_data(), file)
    }
  )

  output$download_metadata <- downloadHandler(
    filename = function() {
      sprintf("metadata_%s.csv", rep_str(title_text(), " ", "_"))
    },
    content = function(file) {
      write.csv(metadata(), file)
    }
  )

  # -------------------
  # INDIRECT UI OUTPUTS
  # -------------------
  output$title_out <- renderMenu({
    if (title_access())
      return(NULL)
    sprintf("<h3><b>Title:</b> %s</h3>", title_text()) %>% HTML()
  })

  output$requests_out <- renderDT({
    my_datatable(present_requests(app_requests))
  }, server = table_server_render)

  output$user_requests_out <- renderDT({
    my_datatable(present_requests(user_requests()))
  }, server = table_server_render)

  output$ggplot2_out <- renderPlot({prep_plot(ggplot2_data())})
  output$plotly2_out <- renderPlotly({prep_plot(plotly2_data())})
  output$plotly3_out <- renderPlotly({prep_plot(plotly3_data())})
  output$beeswarm_out <- renderPlot({prep_plot(beeswarm_data())})

  output$num_data_table <- renderDT({
    my_datatable(data.frame(num_data()))
  }, server = table_server_render)

  output$metadata_table <- renderDT({
    my_datatable(data.frame(metadata()))
  }, server = table_server_render)

  output$legend_out <- renderDT({
    my_datatable(generate_legend_table(colors()))
  }, server = table_server_render)

  output$console_out <- renderPrint({
    if ("address" %in% iplot$console)
      cat_f("address: %s\n", format_print_simple(curr_adr()))
    if ("num_data" %in% iplot$console)
      cat_f("dim(num_data): %s\n", format_print_simple(dim(num_data())))
    if ("metadata" %in% iplot$console)
      cat_f("dim(metadata): %s\n", format_print_simple(dim(metadata())))
    if ("app_requests" %in% iplot$console)
      cat_f("dim(app_requests): %s\n", format_print_simple(dim(app_requests)))
    if ("user_requests" %in% iplot$console)
      cat_f("dim(user_requests): %s\n", format_print_simple(dim(user_requests())))

    for (id in intersect(setdiff(iplot$console, "console"), names(input)))
      cat_f("%s: %s\n", id, format_print_simple(input[[id]]))
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

  name_session <- function(id_n)
  {
    sprintf("Sessions/session_%s.rds", id_n)
  }

  get_session_data <- function()
  {
    session_data <- list()

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

    session_data[["dynamInput"]] <- reactiveValuesToList(dynam_state)

    session_data
  }

  set_session_data <- function(session_data)
  {
    if (is.null(session_data))
      return(NULL)

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

    for (dynam_id in dynam_picker_input_ids)
      dynam_state[[dynam_id]] <- session_data[["dynamInput"]][[dynam_id]]
  }

  onBookmark(function(state) {
    # get the vector of all session IDs
    num_sessions <- load_store("Sessions/num_sessions.rds", 0L)

    # find a session ID that has not yet been used
    id_n <- smallest_missing(num_sessions)

    # add the session ID to the list and save the session
    save_store(c(num_sessions, id_n), "Sessions/num_sessions.rds")
    save_store(get_session_data(), name_session(id_n))

    # the bookmark is simply the numerical ID for the session
    state$values$user_id <- id_n
  })

  # restore compressed data when link is followed
  onRestore(function(state) {
    # get the vector of all session IDs
    num_sessions <- load_store("Sessions/num_sessions.rds", 0L)

    id_n <- state$values$user_id
    # if the id is valid, load session data
    if (id_n %in% num_sessions)
      name_session(id_n) %>% load_store() %>% set_session_data()
  })
}

cat_f("APP BUILD TIME: %.1f (sec)\n", net_time())

# -----------
# RUN THE APP
# -----------
shinyApp(ui = ui, server = server, enableBookmarking = "url")
