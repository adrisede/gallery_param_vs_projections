library(shiny)
library(tibble)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(scrattch.io)
library(DT)
options(shiny.reactlog = TRUE)


# Load data

tsne_params <- readRDS("//allen/programs/celltypes/workgroups/mct-t200/Adriana/gallery_parameters_vs_projections/tsne_params.rda")
samples <- readRDS("//allen/programs/celltypes/workgroups/mct-t200/Adriana/gallery_parameters_vs_projections/samples_in_tsne.rda")
umap_params <- readRDS("//allen/programs/celltypes/workgroups/mct-t200/Adriana/gallery_parameters_vs_projections/uwot_params_genes.rda")
tome <- "//allen/programs/celltypes/workgroups/rnaseqanalysis/shiny/tomes/facs/mouse_V1_ALM_20180520/transcrip.tome"
anno <- scrattch.io::read_tome_anno(tome, groups = "^cluster")


# match samples with anno data
anno <- anno[match(samples, anno$sample_name),]


#############################
#     User Interface        #
#############################

# Define UI for application, use bootstrap's grid system to layout app
ui <- dashboardPage(
  
  # theme
  #theme = shinytheme("flatly"),
  
  # Application title and message icon info
  dashboardHeader(
    title = "Projection parameter gallery",
    titleWidth = 500,
    dropdownMenu(
      type = "messages",
      messageItem(
        from = "Reference dataset:",
        message = "mouse_V1_ALM_20180520", 
        icon = icon("info"),
        time = "2019-04-04"
      ), 
      messageItem(
        from = "Data:", 
        message = "23833 samples, 133 clusters",
        icon = icon("info"),
        time = "2019-04-04"
      )
    )
  ),
  
  # remove sidebar
  dashboardSidebar(disable = TRUE),
  # split window by = widths
  dashboardBody(
    # make dropdown boxes smaller
    tags$style(
      type = 'text/css',
      ".selectize-input { padding: 2px; min-height: 0;} .selectize-dropdown { line-height: 10px; }"
    ),
    
    # fix color of title
    tags$head(tags$style(
      HTML(
        '
        .skin-blue .main-header .logo {
        background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
        background-color: #3c8dbc;
        }
        '
      )
    )),
    
    # left panel
    box(
      width = 6,
      #title = "Parameters",
      #solidHeader = TRUE,
      status = "primary",
      uiOutput("select_projection_method_left"),
      conditionalPanel(
        condition = "input.proj_method_left == 'tsne'",
        fluidRow(column(6, uiOutput("select_initial_dims_left")),
                 column(6, uiOutput("select_perplexity_left"))),
        fluidRow(column(6, uiOutput("select_theta_left")),
                 column(6, uiOutput("select_eta_left"))),
        uiOutput("select_exaggeration_factor_left")
                # fluidRow(column(6, uiOutput("select_exaggeration_factor_left")),
        #          column(6, uiOutput("empty")))
      ),
      conditionalPanel(
        condition = "input.proj_method_left == 'umap'",
        fluidRow(column(6, uiOutput("select_neighbors_left")),
                 column(6, uiOutput("select_metric_left"))),
        fluidRow(column(6, uiOutput("select_scale_left")),
                 column(6, uiOutput("select_init_left")))
      )
    ),
    
    # right panel
    box(
      width = 6,
      #title = "Parameters",
      #solidHeader = TRUE,
      status = "primary",
      uiOutput("select_projection_method_right"),
      conditionalPanel(
        condition = "input.proj_method_right == 'tsne'",
        fluidRow(column(6, uiOutput("select_initial_dims_right")),
                 column(6, uiOutput("select_perplexity_right"))),
        fluidRow(column(6, uiOutput("select_theta_right")),
                 column(6, uiOutput("select_eta_right"))),
        uiOutput("select_exaggeration_factor_right")
                # fluidRow(column(6, uiOutput("select_exaggeration_factor_right")),
        #          column(6, uiOutput("empty")))
      ),
      conditionalPanel(
        condition = "input.proj_method_right == 'umap'",
        fluidRow(column(6, uiOutput("select_neighbors_right")),
                 column(6, uiOutput("select_metric_right"))),
        fluidRow(column(6, uiOutput("select_scale_right")),
                 column(6, uiOutput("select_init_right")))
      )
    ),
    
    # left plot
    box(width = 6,
        plotOutput(
          "plot_left", brush = brushOpts(id = "plot_left_brush")
        )),
    
    # right plot
    box(width = 6,
        plotOutput(
          "plot_right", brush = brushOpts(id = "plot_right_brush")
        )),
    
    # info table left
    box(width = 6,
        collapsible = TRUE, 
        dataTableOutput("table_left")),
    
    # info table right
    box(width = 6,
        collapsible = TRUE,
        dataTableOutput("table_right"))
  )
)


#################
#     SERVER    #
#################


# Define server logic required to listen and react
# use unique input id's
# to read reactive values it requires a function to listen to the reactive elements

server <- function(input, output) {
  
  
  #######################
  ##     left BLOCK    ##
  #######################
  
  ### 1)  TSNE PARAMS 
  
  # original rendering
  output$select_projection_method_left <- renderUI({
    id <- "proj_method_left"
    label <- "Projection Method"
    
    selectInput(
      inputId = id,
      label = label,
      # ui label displayed matched to data label
      choices = c("t-SNE" = "tsne", "UMAP" = "umap")
    )
  })
  
  
  output$select_initial_dims_left <- renderUI({
    id <- "initial_dims_left"
    label <- "Initial dimensions"
    choices <- unique(tsne_params$initial_dims)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  
  output$select_perplexity_left <- renderUI({
    id <- "perplexity_left"
    label <- "Perplexity"
    choices <- unique(tsne_params$perplexity)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  
  output$select_theta_left <- renderUI({
    id <- "theta_left"
    label <- "Theta"
    choices <- unique(tsne_params$theta)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  output$select_eta_left <- renderUI({
    id <- "eta_left"
    label <- "Eta"
    choices <- unique(tsne_params$eta)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  output$select_exaggeration_factor_left <- renderUI({
    id <- "exaggeration_factor_left"
    label <- "Exaggeration factor"
    choices <- unique(tsne_params$exaggeration_factor)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  ### 2)  umap PARAMS 
  
  output$select_neighbors_left <- renderUI({
    id <- "neighbors_left"
    label <- "Number of neighbors"
    choices <- unique(umap_params$n_neighbors)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  
  output$select_metric_left <- renderUI({
    id <- "metric_left"
    label <- "Metric"
    choices <- unique(umap_params$metric)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  output$select_scale_left <- renderUI({
    id <- "scale_left"
    label <- "Scale"
    choices <- unique(umap_params$scale)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  output$select_init_left <- renderUI({
    id <- "init_left"
    label <- "Init"
    choices <- unique(umap_params$init)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices)
  })
  
  
  #########################
  ## SERVER RIGHT BLOCK  ##
  #########################
  
  
  output$select_projection_method_right <- renderUI({
    id = "proj_method_right"
    label = "Projection Method"
    
    selectInput(
      inputId = id,
      label = label,
      # ui label displayed matched to data label
      choices = c("t-SNE" = "tsne", "UMAP" = "umap"),
      selected = "tsne"
    )
  })
  
  
  output$select_initial_dims_right <- renderUI({
    id <- "initial_dims_right"
    label <- "Initial dimensions"
    choices <- unique(tsne_params$initial_dims)
    
    selectInput(
      inputId = id,
      label = label,
      # ui label displayed matched to data label
      choices = choices,
      selected = choices[2]
    )
  })
  
  output$select_perplexity_right <- renderUI({
    id <- "perplexity_right"
    label <- "Perplexity"
    choices <- unique(tsne_params$perplexity)
    
    selectInput(
      inputId = id,
      label = label,
      # ui label displayed matched to data label
      choices = choices,
      selected = choices[2]
    )
  })
  
  output$select_theta_right <- renderUI({
    id <- "theta_right"
    label <- "Theta"
    choices <- unique(tsne_params$theta)
    
    selectInput(
      inputId = id,
      label = label,
      # ui label displayed matched to data label
      choices = choices,
      selected = choices[2]
    )
  })
  
  output$select_eta_right <- renderUI({
    id <- "eta_right"
    label <- "Eta"
    choices <- unique(tsne_params$eta)
    
    selectInput(
      inputId = id,
      label = label,
      # ui label displayed matched to data label
      choices = choices,
      selected = choices[2]
    )
  })
  
  output$select_exaggeration_factor_right <- renderUI({
    id <- "exaggeration_factor_right"
    label <- "Exaggeration factor"
    choices <- unique(tsne_params$exaggeration_factor)
    
    selectInput(
      inputId = id,
      label = label,
      # ui label displayed matched to data label
      choices = choices,
      selected = choices[2]
    )
  })
  
  
  ### 2)  umap PARAMS 
  
  output$select_neighbors_right <- renderUI({
    id <- "neighbors_right"
    label <- "Number of neighbors"
    choices <- unique(umap_params$n_neighbors)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices,
                selected = choices[2])
  })
  
  
  output$select_metric_right <- renderUI({
    id <- "metric_right"
    label <- "Metric"
    choices <- unique(umap_params$metric)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices,
                selected = choices[2])
  })
  
  output$select_scale_right <- renderUI({
    id <- "scale_right"
    label <- "Scale"
    choices <- unique(umap_params$scale)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices,
                selected = choices[2])
  })
  
  output$select_init_right <- renderUI({
    id <- "init_right"
    label <- "Init"
    choices <- unique(umap_params$init)
    
    selectInput(inputId = id,
                label = label,
                # ui label displayed matched to data label
                choices = choices,
                selected = choices[2])
  })
  
  
  ########################
  ## REACTIVE BLOCK left##
  ########################
  
  # make reactive objects for the selections
  
  data_left <- reactive({
    
    req(input$initial_dims_left)
    req(input$perplexity_left)
    req(input$theta_left)
    req(input$eta_left)
    req(input$exaggeration_factor_left)
    #req(input$select_projection_method_left)
    
    all_data <- tsne_params
    
    selected_data <- all_data %>%
      dplyr::filter(
        initial_dims == input$initial_dims_left,
        perplexity == input$perplexity_left,
        theta == input$theta_left,
        eta == input$eta_left,
        exaggeration_factor == input$exaggeration_factor_left
      )
    
    selected_data <- selected_data[1,]
    
    selected_coords <- selected_data$tsne_coords
    
    # make df for ggplot
    selected_coords <- as.data.frame(selected_coords)
    names(selected_coords) <- c("x", "y")
    
    # join sample annots with coordinates
    selected_coords <- cbind(anno, selected_coords)
    
    selected_coords
    
    # if (input$select_projection_method_left == 'umap') {
    #   req(input$neighbors_left)
    #   req(input$metric_left)
    #   req(input$scale_left)
    #   req(input$init_left)
    # 
    #     all_data <- umap_params
    # 
    #     selected_data <- all_data %>%
    #     dplyr::filter(
    #       n_neighbors == input$neighbors_left,
    #       metric == input$metric_left,
    #       scale == input$scale_left,
    #       init == input$init_left
    #       )
    # 
    #     selected_data <- selected_data[1,]
    # 
    #     selected_coords <- selected_data$UMAP
    # 
    #     # make df for ggplot
    #     selected_coords <- as.data.frame(selected_coords)
    #     names(selected_coords) <- c("x", "y")
    # 
    #     # join sample annots with coordinates
    #     selected_coords <- cbind(anno, selected_coords)
    # 
    #     selected_coords
    # 
    #     }
    
  })
  
  selected_samples_left <- reactive({
    req(data_left())
    
    selected_data <-
      brushedPoints(data_left(), input$plot_left_brush)
    
    selected_data$sample_name
    
  })
  
  output$table_left <- renderDataTable({
    req(data_left())
    req(selected_samples_left())
    
    data <- data_left() %>%
      dplyr::filter(sample_name %in% selected_samples_left()) %>%
      dplyr::select(sample_name, cluster_label)
    
    datatable(data,
              rownames = FALSE,
              # rename columns
              colnames = c("Sample" = 1, "Cluster" = 2),
              # remove search bar and limit output
              options = list(#dom = 't',
                pageLength = 3)
    )
  })
  
  data_left_with_selection <- reactive({
    req(data_left())
    
    selected_coords <- data_left()
    
    if (isTruthy(selected_samples_right())) {
      selected_coords$selected <- selected_coords$sample_name %in% selected_samples_right()
      selected_coords$alpha <- ifelse(selected_coords$selected,
                                      1, 0.0001)
    } else {
      selected_coords$selected <- FALSE
      selected_coords$alpha <- 1
    }
    
    selected_coords
  })
  
  
  
  ##########################
  ## REACTIVE BLOCK RIGHT ##
  ##########################
  
  
  data_right <- reactive({
    
    req(input$initial_dims_right)
    req(input$perplexity_right)
    req(input$theta_right)
    req(input$eta_right)
    req(input$exaggeration_factor_right)
    
    all_data <- tsne_params
    
    selected_data <- all_data %>%
      dplyr::filter(
        initial_dims == input$initial_dims_right,
        perplexity == input$perplexity_right,
        theta == input$theta_right,
        eta == input$eta_right,
        exaggeration_factor == input$exaggeration_factor_right
      )
    
    selected_data <- selected_data[1,]
    
    selected_coords <- selected_data$tsne_coords
    
    # make df for ggplot
    selected_coords <- as.data.frame(selected_coords)
    names(selected_coords) <- c("x", "y")
    
    # run this when we have the correct tsne to match colors properly
    selected_coords <- cbind(anno, selected_coords)
    
    selected_coords
    
    # if (input$proj_method_right == "umap") {
    # 
    #   req(input$neighbors_right)
    #   req(input$metric_right)
    #   req(input$scale_right)
    #   req(input$init_right)
    # 
    #   all_data <- umap_params
    #   selected_data <- all_data %>%
    #     dplyr::filter(
    #       n_neighbors == input$neighbors_right,
    #       metric == input$metric_right,
    #       scale == input$scale_right,
    #       init == input$init_right)
    # 
    #   selected_data <- selected_data[1,]
    #   selected_coords <- selected_data$UMAP
    # 
    #   # make df for ggplot
    #   selected_coords <- as.data.frame(selected_coords)
    #   names(selected_coords) <- c("x", "y")
    # 
    #   # run this when we have the correct tsne to match colors properly
    #   selected_coords <- cbind(anno, selected_coords)
    # 
    #   selected_coords
    # }
    
  })
  
  selected_samples_right <- reactive({
    req(data_right())
    
    selected_data <- brushedPoints(data_right(), input$plot_right_brush)
    
    selected_data$sample_name
    
  })
  
  output$table_right <- renderDataTable({
    req(data_right())
    req(selected_samples_right())
    
    data <- data_right() %>%
      dplyr::filter(sample_name %in% selected_samples_right()) %>%
      dplyr::select(sample_name, cluster_label)
    
    datatable(
      data,
      rownames = FALSE,
      colnames = c("Sample" = 1, "Cluster" = 2),
      options = list(#dom = 't',
        pageLength = 3
      )
    )
    
  })
  
  data_right_with_selection <- reactive({
    req(data_right())
    
    selected_coords <- data_right()
    
    if (isTruthy(selected_samples_left())) {
      selected_coords$selected <-
        selected_coords$sample_name %in% selected_samples_left()
      selected_coords$alpha <- ifelse(selected_coords$selected,
                                      1, 0.0001)
    } else {
      selected_coords$selected <- FALSE
      selected_coords$alpha <- 1
    }
    
    selected_coords
  })
  
  
  
  ##########################
  ##   RENDER PLOT LEFT   ##
  ##########################
  
  output$plot_left <- renderPlot({
    # require
    req(data_left())
    
    plot_data <- data_left_with_selection()
    
    ggplot(plot_data) +
      geom_point(aes(
        x = x,
        y = y,
        color = cluster_color,
        alpha = alpha
      ),
      show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_void()
  })
  
  
  ###########################
  ##   RENDER PLOT RIGHT   ##
  ###########################
  
  output$plot_right <- renderPlot({
    # require
    req(data_right())
    
    plot_data <- data_right_with_selection()
    
    ggplot(plot_data) +
      geom_point(aes(
        x = x,
        y = y,
        color = cluster_color,
        alpha = alpha
      ),
      show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_void()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
