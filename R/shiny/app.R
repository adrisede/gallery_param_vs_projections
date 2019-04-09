library(shiny)
library(shinyjs)
library(tibble)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(scrattch.io)
library(shinyBS)
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
  
  
  # Application title and message icon info
  dashboardHeader(
    title = "Projection parameter gallery",
    titleWidth = 310,
    dropdownMenu(
      type = "messages",
      messageItem(
        from = "Reference dataset:",
        message = "mouse_V1_ALM_20180520",
        icon = icon("info")
        #time = "2019-04-04"
      ),
      messageItem(
        from = "Data:",
        message = "23833 samples, 133 clusters",
        icon = icon("info")
        #time = "2019-04-04"
      )
    )
  ),
  
  # remove sidebar
  dashboardSidebar(#disable = TRUE,
    width = 200,
    sidebarMenu(
      menuItem("Cluster Samples", tabName = "clustersamples", icon = icon("chart-area")),
      menuItem("Sample summary", tabName = "summary", icon = icon("table")),
      menuItem("Cluster genes", tabName = "clustergenes", icon = icon("chart-area"))
    )
  ),
  
  # split window by = widths
  dashboardBody(
    
    useShinyjs(),
    
    tabItems(
      tabItem(tabName = "clustersamples", 
              # make dropdown boxes smaller
              tags$style(
                type = 'text/css',
                ".selectize-input { padding: 2px; min-height: 0;} .selectize-dropdown { line-height: 10px; }"
              ),
              
              # fix color of title
              tags$head(tags$style(
                HTML(
                  '
        /* logo * /
        .skin-blue .main-header .logo {
        background-color: #3c8dbc;
        }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
        background-color: #3c8dbc;
        }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
        background-color: #3c8dbc;
        }

        /* main sidebar */
        .skin-blue .main-sidebar {
        background-color: #ecf0f5;
        }
                  
        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #3c8dbc;
        }
                  
        /* other links in the sidebarmenu */
        /* .skin-blue .main-sidebar .sidebar .sidebar-menu a{  */
        /* background-color: #f4f6f7;*/
        /* color: #f4f6f7;*/
        /* } */
                  
        /* other links in the sidebarmenu when hovered */
        /* .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{ */
        /* background-color: #f4f6f7; */
        /* } */
                  
        /* toggle button when hovered  */
        /* .skin-blue .main-header .navbar .sidebar-toggle:hover{ */
        /* background-color: #f4f6f7; */
        /* } */
                  
        /* body */
        /content-wrapper, .right-side {
        background-color: #f4f6f7;
        }

        .skin-blue .wrapper {
         background-color: #f4f6f7;
        }

        '))),
              
              # left panel
              box(
                width = 6,
                collapsible = TRUE, 
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
                  #uiOutput("select_exaggeration_factor_left"),
                  fluidRow(column(6, uiOutput("select_exaggeration_factor_left")),
                           column(6, " ")),
                  bsTooltip("select_initial_dims_left", 
                            "Num. dimensions that should be retained in initial PCA step",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_perplexity_left", 
                            "Number of effective nearest neighbors",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_theta_left", 
                            "Speed/accuracy trade-off (increase for less accuracy)",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_eta_left", 
                            "Learning rate (epsilon)",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_exaggeration_factor_left", 
                            "Factor used to multiply the matrix in the first part of the optimization",
                            "right", trigger = "focus", options = list(container = "body"))
                  
                ),
                conditionalPanel(
                  condition = "input.proj_method_left == 'umap'",
                  fluidRow(column(6, uiOutput("select_neighbors_left")),
                           column(6, uiOutput("select_metric_left"))),
                  fluidRow(column(6, uiOutput("select_scale_left")),
                           column(6, uiOutput("select_init_left"))),
                  # fluidRow(column(6, " "),
                  # column(6, " ")),
                  bsTooltip("select_neighbors_left", 
                            "Size of local neighborhood used for manifold approximation",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_metric_left", 
                            "Distance metric to find nearest neighbors",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_scale_left", 
                            "Scaling to apply (determines how clustered the embedded points are)",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_init_left", 
                            "Init for coordinates: Laplacian (w/o noise, modified), Gaussian or scaled",
                            "right", trigger = "focus", options = list(container = "body"))
                )
              ),
              
              # right panel
              box(
                width = 6,
                collapsible = TRUE, 
                #title = "Parameters",
                #solidHeader = TRUE,
                status = "info",
                uiOutput("select_projection_method_right"),
                conditionalPanel(
                  condition = "input.proj_method_right == 'tsne'",
                  fluidRow(column(6, uiOutput("select_initial_dims_right")),
                           column(6, uiOutput("select_perplexity_right"))),
                  fluidRow(column(6, uiOutput("select_theta_right")),
                           column(6, uiOutput("select_eta_right"))),
                  #uiOutput("select_exaggeration_factor_right")
                  fluidRow(column(6, uiOutput("select_exaggeration_factor_right")),
                           column(6, " "))
                ),
                conditionalPanel(
                  condition = "input.proj_method_right == 'umap'",
                  fluidRow(column(6, uiOutput("select_neighbors_right")),
                           column(6, uiOutput("select_metric_right"))),
                  fluidRow(column(6, uiOutput("select_scale_right")),
                           column(6, uiOutput("select_init_right")))
                  # fluidRow(column(6, " "),
                  # column(6, " "))
                )
              ),
              
              # left plot
              box(width = 6,
                  status = "primary",
                  withSpinner(plotOutput(
                    "plot_left", brush = brushOpts(id = "plot_left_brush")), 
                    type = 7, size = 0.4
                  )),
              
              # right plot
              box(width = 6,
                  status = "info",
                  withSpinner(plotOutput(
                    "plot_right", brush = brushOpts(id = "plot_right_brush")), 
                    type = 7, size = 0.4
                  )),
              
              # info table left
              box(width = 6,
                  status = "primary",
                  collapsible = TRUE,
                  #collapsed = TRUE,
                  dataTableOutput("table_left")),
              
              # info table right
              box(width = 6,
                  status = "info",
                  collapsible = TRUE,
                  #collapsed = TRUE,
                  dataTableOutput("table_right"))
      ),
      
      
      # second tab content
      tabItem(tabName = "summary", 
              
              fluidRow(column(4, valueBox("Dataset", "mouse_V1_ALM_20180520", icon = icon("database"), width = 11)),
                       column(4, valueBox("Size", "23822 samples, 133 clusters", icon = icon("box"), width = 11))), 
              
              box(width = 8,
                  height = 1040,
                  status = "primary",
                  plotOutput("summary"))),
      
      
      # third tab content
      tabItem(tabName = "clustergenes",
              
              # left panel
              box(
                width = 6,
                collapsible = TRUE, 
                #title = "Parameters",
                #solidHeader = TRUE,
                status = "primary",
                uiOutput("select_projection_method_left_genes"),
                conditionalPanel(
                  condition = "input.proj_method_left_genes == 'tsne'",
                  fluidRow(column(6, uiOutput("select_initial_dims_left_genes")),
                           column(6, uiOutput("select_perplexity_left_genes"))),
                  fluidRow(column(6, uiOutput("select_theta_left_genes")),
                           column(6, uiOutput("select_eta_left_genes"))),
                  #uiOutput("select_exaggeration_factor_left"),
                  fluidRow(column(6, uiOutput("select_exaggeration_factor_left_genes")),
                           column(6, " ")),
                  bsTooltip("select_initial_dims_left_genes", 
                            "Num. dimensions that should be retained in initial PCA step",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_perplexity_left_genes", 
                            "Number of effective nearest neighbors",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_theta_left_genes", 
                            "Speed/accuracy trade-off (increase for less accuracy)",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_eta_left_genes", 
                            "Learning rate (epsilon)",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_exaggeration_factor_left_genes", 
                            "Factor used to multiply the matrix in the first part of the optimization",
                            "right", trigger = "focus", options = list(container = "body"))
                  
                ),
                conditionalPanel(
                  condition = "input.proj_method_left_genes == 'umap'",
                  fluidRow(column(6, uiOutput("select_neighbors_left_genes")),
                           column(6, uiOutput("select_metric_left_genes"))),
                  fluidRow(column(6, uiOutput("select_scale_left_genes")),
                           column(6, uiOutput("select_init_left_genes"))),
                  # fluidRow(column(6, " "),
                  # column(6, " ")),
                  bsTooltip("select_neighbors_left_genes", 
                            "Size of local neighborhood used for manifold approximation",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_metric_left_genes", 
                            "Distance metric to find nearest neighbors",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_scale_left_genes", 
                            "Scaling to apply (determines how clustered the embedded points are)",
                            "right", trigger = "focus", options = list(container = "body")),
                  bsTooltip("select_init_left_genes", 
                            "Init for coordinates: Laplacian (w/o noise, modified), Gaussian or scaled",
                            "right", trigger = "focus", options = list(container = "body"))
                )
              ),
              
              # right panel
              box(
                width = 6,
                collapsible = TRUE, 
                #title = "Parameters",
                #solidHeader = TRUE,
                status = "info",
                uiOutput("select_projection_method_right_genes"),
                conditionalPanel(
                  condition = "input.proj_method_right_genes == 'tsne'",
                  fluidRow(column(6, uiOutput("select_initial_dims_right_genes")),
                           column(6, uiOutput("select_perplexity_right_genes"))),
                  fluidRow(column(6, uiOutput("select_theta_right_genes")),
                           column(6, uiOutput("select_eta_right_genes"))),
                  #uiOutput("select_exaggeration_factor_right")
                  fluidRow(column(6, uiOutput("select_exaggeration_factor_right_genes")),
                           column(6, " "))
                ),
                conditionalPanel(
                  condition = "input.proj_method_right_genes == 'umap'",
                  fluidRow(column(6, uiOutput("select_neighbors_right_genes")),
                           column(6, uiOutput("select_metric_right_genes"))),
                  fluidRow(column(6, uiOutput("select_scale_right_genes")),
                           column(6, uiOutput("select_init_right_genes")))
                  # fluidRow(column(6, " "),
                  # column(6, " "))
                )
              ),
              
              # left plot
              box(width = 6,
                  status = "primary",
                  withSpinner(plotOutput(
                    "plot_left_genes", brush = brushOpts(id = "plot_left_brush_genes")), 
                    type = 7, size = 0.4
                  )),
              
              # right plot
              box(width = 6,
                  status = "info",
                  withSpinner(plotOutput(
                    "plot_right_genes", brush = brushOpts(id = "plot_right_brush_genes")), 
                    type = 7, size = 0.4
                  )),
              
              # info table left
              box(width = 6,
                  status = "primary",
                  collapsible = TRUE,
                  #collapsed = TRUE,
                  dataTableOutput("table_left_genes")),
              
              # info table right
              box(width = 6,
                  status = "info",
                  collapsible = TRUE,
                  #collapsed = TRUE,
                  dataTableOutput("table_right_genes"))
      )
      
    )
    
    
  )
)

  





#################
#     SERVER    #
#################


# Define server logic required to listen and react
# use unique input id's
# to read reactive values it requires a function to listen to the reactive elements

server <- function(input, output, session) {

  # collapse sidebar when loading
  addClass(selector = "body", class = "sidebar-collapse")

  
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
      choices = c("t-SNE" = "tsne", "UMAP" = "umap"),
      selected = "tsne"
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
      #choices <- choices
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
    
    if (input$proj_method_left == 'tsne') {
      
    req(input$initial_dims_left)
    req(input$perplexity_left)
    req(input$theta_left)
    req(input$eta_left)
    req(input$exaggeration_factor_left)

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
    
  } else if (input$proj_method_left == 'umap') {
    
    req(input$neighbors_left)
    req(input$metric_left)
    req(input$scale_left)
    req(input$init_left)

      all_data <- umap_params

      selected_data <- all_data %>%
      dplyr::filter(
        n_neighbors == input$neighbors_left,
        metric == input$metric_left,
        scale == input$scale_left,
        init == input$init_left
        )

      selected_data <- selected_data[1,]

      selected_coords <- selected_data$UMAP

      # make df for ggplot
      selected_coords <- as.data.frame(selected_coords)
      names(selected_coords) <- c("x", "y")

      # join sample annots with coordinates
      selected_coords <- cbind(anno, selected_coords)

      selected_coords

      }

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
              options = list(
                dom = 'tp', 
                pagingType = 'simple_numbers',
                responsive = TRUE,
                pageLength = 4)
    )
  })
  
  data_left_with_selection <- reactive({
    req(data_left())
    
    selected_coords <- data_left()
    
    if (isTruthy(selected_samples_right())) {
      selected_coords$selected <- selected_coords$sample_name %in% selected_samples_right()
      selected_coords$alpha <- ifelse(selected_coords$selected,
                                      1, 0.01)
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
    
    if (input$proj_method_right == 'tsne') {
    
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
    
    } else if (input$proj_method_right == "umap") {

      req(input$neighbors_right)
      req(input$metric_right)
      req(input$scale_right)
      req(input$init_right)

      all_data <- umap_params
      selected_data <- all_data %>%
        dplyr::filter(
          n_neighbors == input$neighbors_right,
          metric == input$metric_right,
          scale == input$scale_right,
          init == input$init_right)

      selected_data <- selected_data[1,]
      selected_coords <- selected_data$UMAP

      # make df for ggplot
      selected_coords <- as.data.frame(selected_coords)
      names(selected_coords) <- c("x", "y")

      # run this when we have the correct tsne to match colors properly
      selected_coords <- cbind(anno, selected_coords)

      selected_coords
    }
    
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
      options = list(
        dom = 'tp', 
        pagingType = 'simple_numbers',
        responsive = TRUE,
        pageLength = 4)
    )
    
  })
  
  data_right_with_selection <- reactive({
    req(data_right())
    
    selected_coords <- data_right()
    
    if (isTruthy(selected_samples_left())) {
      selected_coords$selected <-
        selected_coords$sample_name %in% selected_samples_left()
      selected_coords$alpha <- ifelse(selected_coords$selected,
                                      1, 0.01)
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
      scale_alpha_identity() +
      #scale_fill_identity() +
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
      scale_alpha_identity() +
      #scale_fill_identity() +
      theme_void()
  })
  
  
  # plot pop-up
  addPopover(session, "plot_left", 
             title = "Select samples",
content = paste0("Select a plot region to retrieve sample info and 
                 highlight their location on the adjacent plot. 
                 Click in white space to reset selection."), 
            trigger = 'hover', placement = "top")
  
  
  # reactive for summary data
  summary <- reactive({
    
    anno_min <- select(anno, cluster_label, cluster_color) %>% 
      unique()
    
    summary <- as.data.frame(with(anno, table(cluster_label))) %>%
      left_join(anno_min, by = "cluster_label") %>%
      arrange(desc(Freq)) 
    
    summary
  })
  
  
  # summary tab plot  
  output$summary <- renderPlot({
    
    req(summary())
    
    summary <- summary()
    
    ggplot(summary, aes(x = reorder(cluster_label, Freq), 
                        y = Freq, 
                        color = cluster_color, 
                        fill = cluster_color,
                        alpha = 0.9)) +
      geom_bar(stat = "identity") + 
      geom_text(aes(label = Freq), 
                hjust = -0.5, 
                position = position_dodge(width = 0.8), 
                size = 2) +
      theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 1)) +
      labs(y = "Number of samples within cluster", x = "Cluster name") +
      scale_color_identity() +
      scale_fill_identity() +
      coord_flip() +
      scale_alpha(guide = "none") +
      theme_classic()
    
  }, height = 1000, width = 550)  
  
  
}





# Run the application
shinyApp(ui = ui, server = server)
