library(shiny)
library(tidyverse)
library(data.table)
library(bslib)
library(shinybusy)
library(shinyWidgets)
library(bsicons)
library(shinyalert)
library(readxl)
library(ggnewscale)
library(ggpubr)
library(scales)
theme_set(theme_bw())

# UI ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "Rum Jungle Water & Load Balance Model",
  # Sidebar --------------
  # This will be displayed on each page
  sidebar = sidebar(
    a(href = 'http://www.rgc.ca',
      img(src = 'logo.png',
          title = "", height = "70px"),
      style = "padding-top:0px; padding-bottom:0px;"),
    helpText("Updated for v3.44", br(), 
             "For queries contact: hsharif@robertsongeo.com"),
    
    ### `ava_xlsx` ----
    fileInput(
      inputId = "ava_xlsx",
      label = "Uploading GoldSim Output:",
      placeholder = "RJ WLBM Master Spreadsheet - v**.xlsx",
      accept = c(".xlsx")
    ),
    
    ### `date_range` ----
    dateRangeInput(inputId = "date_range", 
                   label = "Date Interval of Interest (2024-2039):",
                   start = "2025-07-01", end = "2028-06-30", min = "2024-07-01",
                   max = "2039-01-01"),
    
    h5("Flowsheet Settings"),
    ### `wyear_month` ----
    tooltip(
      selectInput(
        inputId = "wyear_month",
        label = "Water Year Start:",
        choices = month.abb,
        selected = "Jul"
      ),
      "Specifying beging month for water year.For example, May is equivalent to May 1st to Apr 30th."
    ),
    
    ### `var_flowchart` ----
    selectInput(
      inputId = "var_flowchart",
      label = "Variable of Interest:",
      choices = c(
        "Water", "SO4", "Ca", "Mg", "Al", "Fe", "Cu",
        "Co", "Mn", "Ni", "U", "Zn", "Acidity"
      ),
      multiple = FALSE,
      selected = "Water"
    ),
    
    ### `flowchart_unit` ----
    selectInput(
      inputId = "flowchart_unit",
      label = "Change Unit",
      choices = c("L/s", "ML/year")
    ),
    
    ### `up_flowchart` ----
    actionBttn(
      inputId = "up_flowchart",
      label = "Update Flowchart",
      style = "simple",
      color = "danger",
      size = "sm",
      icon = icon("rotate", class = "fa-light")
    ),
    
    h5("Daily Plot Settings"),
    ### `storage_select` ----
    selectInput(
      inputId = "storage_select",
      label = "Storage of Interest:",
      choices = c(
        "Main Pit", "Intermediate Pit", "Main WRD", "Intermediate WRD",
        "Dyson's WRD", "EWSF", "Pit Water Treatment Plant",
        "Groundwater Treatment Plant", "EBFR US", "Diversion Channel",
        "EBFR DS", "GS8150097"
      ),
      multiple = FALSE
    ),
    
    ### `chosen_flux` ----
    selectInput(
      inputId = "chosen_flux",
      label = "Select Flow Path",
      choices = c(),
      multiple = FALSE
    ),
    
    ### `consit` ----
    selectInput(
      inputId = "consit",
      label = "Constituent of Interest:",
      choices = c(
        "SO4", "Ca", "Mg", "Al", "Fe", "Cu",
        "Co", "Mn", "Ni", "U", "Zn", "Acidity"
      ),
      multiple = TRUE,
      selected = "SO4",
      selectize = TRUE
    ),
    
    ### `consit_var` ----
    selectInput(
      inputId = "consit_var",
      label = "Variable Type:",
      choices = c(
        "Concentration", "Load"
      ),
      multiple = FALSE
    ),
    
    ### `placeholder_consit_unit` ----
    uiOutput(
      outputId = "placeholder_consit_unit"
    ),
    
    ### `add_flow` -----
    checkboxInput(
      inputId = "add_flow",
      label = "Add Discharge Series"
    ),
    
    ### `download_plot` ----
    ### `plot_width` ----
    ### `plot_height` ----
    numericInput(
      inputId = "plot_width",
      label = "Exported Plot Width (in):",
      min = 1,
      value = 7
    ),
    numericInput(
      inputId = "plot_height",
      label = "Exported Plot Height (in):",
      min = 1,
      value = 5
    ),
    downloadButton(
      outputId = "download_plot",
      label = "Download Plot"
    ),
    
    ### Sidebar Settings 
    width = 400,
    open = "always"
  ),
  
  ### Flow Chart page -----
  nav_panel(
    title = "Model Structure",
    icon = icon("lightbulb", class = "fa-regular"),
    
    ### `flowchart` ----
    uiOutput(
      outputId = "flowchart",
      fill = TRUE
    )
    
  ),
  
  ### `plt` ----
  nav_panel(
    title = "Daily Series Plot",
    icon = icon("chart-line", class = "fa-regular"),
    card(
      plotOutput(outputId = "plt")
    )
  ),
  
  ### Dark/Light mode switch
  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item(
      a(icon("building", class = "fa-light"), "Team",
        href = "https://www.rgc.ca", 
        target = "_blank")
    )
  ),
  nav_item(
    input_dark_mode(id = "mode")
  )
)



# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  my_comma <- scales::label_comma(accuracy = 0.1)
  
  options(shiny.maxRequestSize=100*1024^2)
  
  # Color palette for constituents
  palette12 <- c(
    "#E31A1C", "green1", "#6A3D9A", "#FF7F00", "gold1", "deeppink1",
    "palegreen2", "darkorange4", "#FDBF6F", "khaki2", "maroon", "orchid1"
  )
  
  # Function to scale secondary axis
  scale_function <- function(x, scale, shift){
    return ((x)*scale - shift)
  }
  
  # Function to scale secondary variable values
  inv_scale_function <- function(x, scale, shift){
    return ((x + shift)/scale)
  }
  
  shinyalert(
    title = 'Welcome to the Dashboard!',
    text = "Please upload the GoldSim output. Once uploaded, you can generate flowsheets of your interest and visualize daily flow, load, and concentrations for flow lines.",
    type = "",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE
  )
  
  # Reactive All WLBM Data ----
  all_wlbm_data <- reactiveValues(
    main_pit = NULL,
    interm_pit = NULL,
    main_wrd = NULL,
    interm_wrd = NULL,
    dysons_wrd = NULL,
    ewsf = NULL,
    gwtp = NULL,
    pwtp = NULL,
    ebfr_us = NULL,
    ebfr_ds = NULL,
    div_channel = NULL,
    gs = NULL,
    ponds_water = NULL,
    ponds_load = NULL
  )
  
  # ObserveEvent 00: Selecting Model Version
  observeEvent(input$ava_xlsx, {
    req(input$ava_xlsx)
    
    # check if all excel sheets exist
    available_sheets <- excel_sheets(input$ava_xlsx$datapath)
    needed_sheets <- c(
      "Main_Pit_Flow_Inputs", "Main_Pit_Flow_Outputs", "Main_Pit_Load_Inputs", 
      "Main_Pit_Load_Outputs", "Interm_Pit_Flow_Inputs", "Interm_Pit_Flow_Outputs",
      "Interm_Pit_Load_Inputs", "Interm_Pit_Load_Outputs", "MainWRD_Q_Inputs", 
      "MainWRD_Q_Outputs", "MainWRD_Load_Inputs", "MainWRD_Load_Outputs", 
      "IntermWRD_Q_Inputs", "IntermWRD_Q_Outputs", "IntermWRD_Load_Inputs", 
      "IntermWRD_Load_Outputs", "IntermWRD_Q_Inputs", "IntermWRD_Q_Outputs",
      "IntermWRD_Load_Inputs", "IntermWRD_Load_Outputs", "DysonsWRD_Q_Inputs",
      "DysonsWRD_Q_Outputs", "DysonsWRD_Load_Inputs", "DysonsWRD_Load_Outputs",
      "EWSF_Q_Inputs", "EWSF_Q_Outputs", "EWSF_Load_Inputs", "EWSF_Load_Outputs",
      "GWTP_Q_Inputs", "GWTP_Q_Outputs", "GWTP_Load_Inputs", "GWTP_Load_Outputs",
      "PWTP_Q_Inputs", "PWTP_Q_Outputs", "PWTP_Load_Inputs", "PWTP_Load_Outputs",
      "EBFR_US_Q_Inputs", "EBFR_US_Q_Outputs", "EBFR_US_Load_Inputs",
      "EBFR_US_Load_Outputs", "EBFR_DS_Q_Inputs", "EBFR_DS_Q_Outputs", 
      "EBFR_DS_Load_Inputs", "EBFR_DS_Load_Outputs", "DiversionChannel_Q_Inputs",
      "DiversionChannel_Q_Outputs", "Diversion_Load_Inputs",
      "Diversion_Load_Outputs", "GS097_Q_Inputs", "GS097_Q_Outputs",
      "GS097_Load_Inputs", "GS097_Load_Outputs"
    )
    if (sum(!(needed_sheets %in% available_sheets) != 0)){
      showNotification("Selected model results is corrupted!",
                       duration = 8, type = "error")
      return()
    }
    
    show_modal_spinner(spin = "hollow-dots",
                       text = "Processing WLBM results ...")
    # somewhat heavy calculations/reading relevant flow, concentration and load data
    # flow m3/s
    # load kg/day
    # concentration mg/L
    # concentration = load * 1e6 / (flow * 24 * 3600 * 1e3) 
    # = load * 1e3 / (flow * 24 * 3600)
    
    # sheet names:
    # Main Pit: Main_Pit_Flow_Inputs, Main_Pit_Flow_Outputs, Main_Pit_Load_Inputs,
    #           Main_Pit_Load_Outputs
    # Intermediate Pit: Interm_Pit_Flow_Inputs, Interm_Pit_Flow_Outputs,
    #                   Interm_Pit_Load_Inputs, Interm_Pit_Load_Outputs
    # Main WRD: MainWRD_Q_Inputs, MainWRD_Q_Outputs, MainWRD_Load_Inputs,
    #           MainWRD_Load_Outputs
    # Intermediate WRD: IntermWRD_Q_Inputs, MainWRD_Q_Outputs, MainWRD_Load_Inputs,
    #                   IntermWRD_Load_Outputs
    # Dysons's WRD: DysonsWRD_Q_Inputs, DysonsWRD_Q_Outputs, DysonsWRD_Load_Inputs,
    #          DysonsWRD_Load_Outputs
    # EWSF: EWSF_Q_Inputs, EWSF_Q_Outputs, EWSF_Load_Inputs,
    #       EWSF_Load_Outputs
    # GWTP: GWTP_Q_Inputs, GWTP_Q_Outputs, GWTP_Load_Inputs,
    #       GWTP_Load_Outputs
    # PWTP: PWTP_Q_Inputs, PWTP_Q_Outputs, PWTP_Load_Inputs,
    #       PWTP_Load_Outputs
    
    # ObserveEvent
    ponds_water_data <- read_excel(
      input$ava_xlsx$datapath,
      sheet = "Ponds_Water",
      col_types = c("date", rep("numeric", 6))
    ) %>%
      na.omit() %>%
      mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
      dplyr::filter(row_number() != n())
    
    rename_vec <- 
    ponds_load_data <- read_excel(
      input$ava_xlsx$datapath,
      sheet = "Ponds_Load",
      col_types = c("date", rep("numeric", 72))
    ) %>%
      na.omit() %>%
      mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
      dplyr::filter(row_number() != n())
      
    
    main_pit_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Main_Pit_Flow_Inputs",
        col_types = c("date", rep("numeric", 12))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Main_Pit_Flow_Outputs",
        col_types = c("date", rep("numeric", 5))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Main_Pit_Load_Inputs",
        col_types = c("date", rep("numeric", 144))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Main_Pit_Load_Outputs",
        col_types = c("date", rep("numeric", 48))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    interm_pit_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Interm_Pit_Flow_Inputs",
        col_types = c("date", rep("numeric", 8))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Interm_Pit_Flow_Outputs",
        col_types = c("date", rep("numeric", 6))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Interm_Pit_Load_Inputs",
        col_types = c("date", rep("numeric", 96))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Interm_Pit_Load_Outputs",
        col_types = c("date", rep("numeric", 60))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    main_wrd_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "MainWRD_Q_Inputs",
        col_types = c("date", rep("numeric", 4))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "MainWRD_Q_Outputs",
        col_types = c("date", rep("numeric", 5))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "MainWRD_Load_Inputs",
        col_types = c("date", rep("numeric", 48))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "MainWRD_Load_Outputs",
        col_types = c("date", rep("numeric", 48))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    interm_wrd_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "IntermWRD_Q_Inputs",
        col_types = c("date", rep("numeric", 2))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "IntermWRD_Q_Outputs",
        col_types = c("date", rep("numeric", 3))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "IntermWRD_Load_Inputs",
        col_types = c("date", rep("numeric", 24))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "IntermWRD_Load_Outputs",
        col_types = c("date", rep("numeric", 24))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    dysons_wrd_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "DysonsWRD_Q_Inputs",
        col_types = c("date", rep("numeric", 3))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "DysonsWRD_Q_Outputs",
        col_types = c("date", rep("numeric", 5))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "DysonsWRD_Load_Inputs",
        col_types = c("date", rep("numeric", 36))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "DysonsWRD_Load_Outputs",
        col_types = c("date", rep("numeric", 48))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    ewsf_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EWSF_Q_Inputs",
        col_types = c("date", rep("numeric", 3))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EWSF_Q_Outputs",
        col_types = c("date", rep("numeric", 6))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EWSF_Load_Inputs",
        col_types = c("date", rep("numeric", 36))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EWSF_Load_Outputs",
        col_types = c("date", rep("numeric", 60))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    gwtp_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GWTP_Q_Inputs",
        col_types = c("date", rep("numeric", 5))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GWTP_Q_Outputs",
        col_types = c("date", rep("numeric", 3))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GWTP_Load_Inputs",
        col_types = c("date", rep("numeric", 60))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GWTP_Load_Outputs",
        col_types = c("date", rep("numeric", 48))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    pwtp_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "PWTP_Q_Inputs",
        col_types = c("date", rep("numeric", 1))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "PWTP_Q_Outputs",
        col_types = c("date", rep("numeric", 3))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "PWTP_Load_Inputs",
        col_types = c("date", rep("numeric", 12))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "PWTP_Load_Outputs",
        col_types = c("date", rep("numeric", 48))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Adjusted Upstream Runoff to EBRF US %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ebfr_us_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_US_Q_Inputs",
        col_types = c("date", rep("numeric", 10))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()) %>%
        mutate(
          T_Runoff_EBFR_US = T_Runoff_EBFR_US + T_EWSF_E_Runoff_not_collected +
            T_WWSF_Q_to_EBFR_Above_GS200
        ),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_US_Q_Outputs",
        col_types = c("date", rep("numeric", 2))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_US_Load_Inputs",
        col_types = c("date", rep("numeric", 132))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()) %>%
        mutate(
          `T_Runoff_EBFR_US_CT[SO4]` = `T_Runoff_EBFR_US_CT[SO4]` + 
            `T_EWSF_E_RunoffNotCollected_CT[SO4]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[SO4]`,
          `T_Runoff_EBFR_US_CT[Ca]` = `T_Runoff_EBFR_US_CT[Ca]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Ca]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Ca]`,
          `T_Runoff_EBFR_US_CT[Mg]` = `T_Runoff_EBFR_US_CT[Mg]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Mg]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Mg]`,
          `T_Runoff_EBFR_US_CT[Al]` = `T_Runoff_EBFR_US_CT[Al]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Al]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Al]`,
          `T_Runoff_EBFR_US_CT[Fe]` = `T_Runoff_EBFR_US_CT[Fe]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Fe]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Fe]`,
          `T_Runoff_EBFR_US_CT[Cu]` = `T_Runoff_EBFR_US_CT[Cu]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Cu]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Cu]`,
          `T_Runoff_EBFR_US_CT[Co]` = `T_Runoff_EBFR_US_CT[Co]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Co]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Co]`,
          `T_Runoff_EBFR_US_CT[Mn]` = `T_Runoff_EBFR_US_CT[Mn]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Mn]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Mn]`,
          `T_Runoff_EBFR_US_CT[Ni]` = `T_Runoff_EBFR_US_CT[Ni]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Ni]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Ni]`,
          `T_Runoff_EBFR_US_CT[U]` = `T_Runoff_EBFR_US_CT[U]` + 
            `T_EWSF_E_RunoffNotCollected_CT[U]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[U]`,
          `T_Runoff_EBFR_US_CT[Zn]` = `T_Runoff_EBFR_US_CT[Zn]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Zn]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Zn]`,
          `T_Runoff_EBFR_US_CT[Acidity]` = `T_Runoff_EBFR_US_CT[Acidity]` + 
            `T_EWSF_E_RunoffNotCollected_CT[Acidity]` +
            `T_WWSF_QtoEBFR_Above_GS200_CT[Acidity]`
        ),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_US_Load_Outputs",
        col_types = c("date", rep("numeric", 24))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    ebfr_ds_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_DS_Q_Inputs",
        col_types = c("date", rep("numeric", 8))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_DS_Q_Outputs",
        col_types = c("date", rep("numeric", 1))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_DS_Load_Inputs",
        col_types = c("date", rep("numeric", 108))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "EBFR_DS_Load_Outputs",
        col_types = c("date", rep("numeric", 12))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    div_channel_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "DiversionChannel_Q_Inputs",
        col_types = c("date", rep("numeric", 11))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "DiversionChannel_Q_Outputs",
        col_types = c("date", rep("numeric", 2))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Diversion_Load_Inputs",
        col_types = c("date", rep("numeric", 144))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "Diversion_Load_Outputs",
        col_types = c("date", rep("numeric", 24))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    gs_data <- list(
      inflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GS097_Q_Inputs",
        col_types = c("date", rep("numeric", 2))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      outflow = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GS097_Q_Outputs",
        col_types = c("date", rep("numeric", 1))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x) / (24 * 3600)) %>%
        dplyr::filter(row_number() != n()),
      
      inload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GS097_Load_Inputs",
        col_types = c("date", rep("numeric", 24))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n()),
      
      outload = read_excel(
        input$ava_xlsx$datapath,
        sheet = "GS097_Load_Outputs",
        col_types = c("date", rep("numeric", 12))
      ) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date)) %>%
        mutate_if(is.numeric, function(x) (lead(x, 1) - x)) %>%
        dplyr::filter(row_number() != n())
    )
    
    all_wlbm_data$main_pit <- main_pit_data
    all_wlbm_data$interm_pit <- interm_pit_data
    all_wlbm_data$main_wrd <- main_wrd_data
    all_wlbm_data$interm_wrd <- interm_wrd_data
    all_wlbm_data$dysons_wrd <- dysons_wrd_data
    all_wlbm_data$ewsf <- ewsf_data
    all_wlbm_data$gwtp <- gwtp_data
    all_wlbm_data$pwtp <- pwtp_data
    all_wlbm_data$ebfr_us <- ebfr_us_data
    all_wlbm_data$ebfr_ds <- ebfr_ds_data
    all_wlbm_data$div_channel <- div_channel_data
    all_wlbm_data$gs <- gs_data
    all_wlbm_data$ponds_water <- ponds_water_data
    all_wlbm_data$ponds_load <- ponds_load_data
    remove_modal_spinner()
  }, priority = 11)
  
  
  # Read flowsheet location table ----
  flowsheet_loc <- fread("Flowsheet_Location.csv") %>%
    drop_na(X, Y) %>%
    dplyr::filter(Flow_Name != "",
                  !is.na(Flow_Name))
  
  # Read look-up table to adjust load column names ----
  lookup_df <- fread("Flow_to_Load_LookupTable.csv") %>%
    mutate_if(is.character, function(x) ifelse(x == "", NA, x))
  
  cts <- c("SO4", "Ca", "Mg", "Al", "Fe", "Cu",
           "Co", "Mn", "Ni", "U", "Zn", "Acidity")
  rename_vec <- lookup_df %>%
    drop_na(`Load New Name`, `Load Name`) %>%
    pull(`Load Name`)
  rename_vec_names <- lookup_df %>%
    drop_na(`Load New Name`, `Load Name`) %>%
    pull(`Load New Name`)
  rename_vec_vf <- paste0(
    rep(rename_vec, each = 12),
    "[",
    rep(cts, length(rename_vec)),
    "]"
  )
  names(rename_vec_vf) <- paste0(
    rep(rename_vec_names, each = 12),
    "[",
    rep(cts, length(rename_vec_names)),
    "]"
  )
  
  # Fixing missing _CT
  removed_load_rename_vec <- paste0(
    rep(c("T_Load_Removed_by_PWTP",
          "T_Load_Removed_by_GWTP",
          "T_GW_Load_Build_Up_EBFR_US",
          "T_GW_Load_Build_Up_Diversion",
          "T_GW_Load_Build_Up_EBFR_DS"),
        each = 12),
    "[",
    rep(cts, 5),
    "]"
  )
  names(removed_load_rename_vec) <- paste0(
    rep(c("T_Load_Removed_by_PWTP",
          "T_Load_Removed_by_GWTP",
          "T_GW_Load_Build_Up_EBFR_US",
          "T_GW_Load_Build_Up_Diversion",
          "T_GW_Load_Build_Up_EBFR_DS"),
        each = 12),
    "_CT[",
    rep(cts, 5),
    "]"
  )
  
  # Storage renaming 
  storage_rename_vec <- paste0(
    rep(c("Total_Mass_Main_Pit_CT",
          "Intermediate_Pit_Water_CT.Mass_in_Pathway",
          "Main_WRD_Pond_CT.Mass_in_Pathway",
          "Intermediate_WRD_Pond_CT.Mass_in_Pathway"
          ), each = 12),
    "[",
    rep(cts, 4),
    "]"
  )
  names(storage_rename_vec) <- paste0(
    rep(c("Main_Pit_CT",
          "Intermediate_Pit_CT",
          "Main_WRD_CT",
          "Intermediate_WRD_CT"
    ), each = 12),
    "[",
    rep(cts, 4),
    "]"
  )
  
  # Create all summary table for flowsheet ----
  # Daily values of all flowpath and loads in m3/day and kg/day
  flowsheet_dfs <- reactiveValues(flow = NULL, load = NULL,
                                  storage_flow = NULL, storage_load = NULL)
  observeEvent(input$ava_xlsx$datapath, {
    show_modal_spinner(spin = "fingerprint",
                       color = "blue2",
                       text = "Cleaning WLBM Results ...")
    # if one of them is available the rest also will be available
    req(all_wlbm_data$main_pit$inflow)
    
    # m3/day water flows
    flow_df <- all_wlbm_data$main_pit$inflow %>%
      left_join(all_wlbm_data$main_pit$outflow, by = "Date") %>%
      left_join(all_wlbm_data$interm_pit$inflow, by = "Date") %>%
      left_join(all_wlbm_data$interm_pit$outflow, by = "Date") %>%
      left_join(all_wlbm_data$main_wrd$inflow, by = "Date") %>%
      left_join(all_wlbm_data$main_wrd$outflow, by = "Date") %>%
      left_join(all_wlbm_data$interm_wrd$inflow, by = "Date") %>%
      left_join(all_wlbm_data$interm_wrd$outflow, by = "Date") %>%
      left_join(all_wlbm_data$gwtp$inflow, by = "Date") %>%
      left_join(all_wlbm_data$gwtp$outflow, by = "Date") %>%
      left_join(all_wlbm_data$pwtp$inflow, by = "Date") %>%
      left_join(all_wlbm_data$pwtp$outflow, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_us$inflow, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_us$outflow, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_ds$inflow, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_ds$outflow, by = "Date") %>%
      left_join(all_wlbm_data$div_channel$inflow, by = "Date") %>%
      left_join(all_wlbm_data$div_channel$outflow, by = "Date") %>%
      left_join(all_wlbm_data$gs$inflow, by = "Date") %>%
      left_join(all_wlbm_data$gs$outflow, by = "Date") %>%
      rename_with(~ gsub("\\.x$", "", .), everything()) %>%
      pivot_longer(cols = !Date, names_to = "Flow_Name", values_to = "Flow") %>%
      mutate(Flow = Flow * 24 * 3600) %>%
      dplyr::filter(Flow_Name %in% flowsheet_loc$Flow_Name)
    
    # kg/day loads for constituents
    load_df <- all_wlbm_data$main_pit$inload %>%
      left_join(all_wlbm_data$main_pit$outload, by = "Date") %>%
      left_join(all_wlbm_data$interm_pit$inload, by = "Date") %>%
      left_join(all_wlbm_data$interm_pit$outload, by = "Date") %>%
      left_join(all_wlbm_data$main_wrd$inload, by = "Date") %>%
      left_join(all_wlbm_data$main_wrd$outload, by = "Date") %>%
      left_join(all_wlbm_data$interm_wrd$inload, by = "Date") %>%
      left_join(all_wlbm_data$interm_wrd$outload, by = "Date") %>%
      left_join(all_wlbm_data$gwtp$inload, by = "Date") %>%
      left_join(all_wlbm_data$gwtp$outload, by = "Date") %>%
      left_join(all_wlbm_data$pwtp$inload, by = "Date") %>%
      left_join(all_wlbm_data$pwtp$outload, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_us$inload, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_us$outload, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_ds$inload, by = "Date") %>%
      left_join(all_wlbm_data$ebfr_ds$outload, by = "Date") %>%
      left_join(all_wlbm_data$div_channel$inload, by = "Date") %>%
      left_join(all_wlbm_data$div_channel$outload, by = "Date") %>%
      left_join(all_wlbm_data$gs$inload, by = "Date") %>%
      left_join(all_wlbm_data$gs$outload, by = "Date") %>%
      rename_with(~ gsub("\\.x$", "", .), everything()) %>%
      dplyr::select(-ends_with(".y")) %>%
      rename(any_of(rename_vec_vf)) %>%
      rename(any_of(removed_load_rename_vec)) %>%
      pivot_longer(cols = !Date, names_to = c("Flow_Name", "Constituent"),
                   values_to = "Load", names_pattern = "(.*)_CT\\[(.*)\\]") %>%
      dplyr::filter(Flow_Name %in% flowsheet_loc$Flow_Name)
    
    storageflow_df <- all_wlbm_data$ponds_water %>%
      dplyr::select(c(Date, Main_Pit_Water, Intermediate_Pit_Water,
                      Main_WRD_Pond, Intermediate_WRD_Pond)) %>%
      rename(Main_Pit = Main_Pit_Water,
             Intermediate_Pit = Intermediate_Pit_Water,
             Main_WRD = Main_WRD_Pond,
             Intermediate_WRD = Intermediate_WRD_Pond) %>%
    pivot_longer(cols = !Date, names_to = "Flow_Name", values_to = "Flow") %>%
    mutate(Flow = Flow * 24 * 3600)
    
    storageload_df <- all_wlbm_data$ponds_load %>%
      dplyr::select(c("Date", 
                      starts_with(c("Total_Mass_Main_Pit_CT",
                                    "Intermediate_Pit_Water_CT.Mass_in_Pathway",
                                    "Main_WRD_Pond_CT.Mass_in_Pathway",
                                    "Intermediate_WRD_Pond_CT.Mass_in_Pathway")))) %>%
      rename(any_of(storage_rename_vec)) %>%
      pivot_longer(cols = !Date, names_to = c("Flow_Name", "Constituent"),
                   values_to = "Load", names_pattern = "(.*)_CT\\[(.*)\\]")
      
    flowsheet_dfs$flow <- flow_df
    flowsheet_dfs$storage_flow <- storageflow_df
    flowsheet_dfs$load <- load_df
    flowsheet_dfs$storage_load <- storageload_df
    remove_modal_spinner()
  })
  
  # ObserveEvent 01-01: Populate unit options ----
  # Reactive adjustments
  observeEvent(input$var_flowchart, {
    req(input$var_flowchart)
    if (input$var_flowchart == "Water") {
      ### `flowchart_unit` ----
      updateSelectInput(
        session, 
        "flowchart_unit",
        label = "Change Unit",
        choices = c("L/s", "ML/year"),
        selected = "L/s"
      )
    } else {
      updateSelectInput(
        session, 
        "flowchart_unit",
        label = "Change Unit",
        choices = c("kg/year", "t/year"),
        selected = "kg/year"
      )
    }
  })
  
  # ObserveEvent 01-02 Flowchart ----
  # For now it only shows water flux in L/s
  # Filter all_wlbm data based on dates
  # Calculate Annual Flows in L/s
  # Find the location of flow sheet
  # Add title and values
  observeEvent(
    input$up_flowchart,
    {
      req(flowsheet_dfs$flow, flowsheet_dfs$load, flowsheet_dfs$storage_flow,
          flowsheet_dfs$storage_load, input$date_range, input$wyear_month,
          input$var_flowchart, input$flowchart_unit)
      month_idx <- which(month.abb == input$wyear_month)
      tlt <- paste0(input$var_flowchart, " Flowsheet (",
                    input$flowchart_unit, ")")
      subtlt <- paste0("Water year: ", input$wyear_month, "-",
                      ifelse(month_idx - 1  == 0, "Dec", month.abb[month_idx-1]),
                      ". Incomplete years ignored.")
      show_modal_spinner(spin = "cube-grid",
                         text = "Populating Flowsheet ...")
      
      if (input$var_flowchart == "Water") {
        summary_df <- flowsheet_dfs$flow %>%
          dplyr::filter(Date >= input$date_range[1],
                        Date <= input$date_range[2]) %>%
          mutate(Year = year(Date),
                 Month = month(Date),
                 Water_Year = ifelse(Month < month_idx, Year - 1, Year)) %>%
          group_by(Flow_Name, Water_Year) %>%
          summarise(
            num_obs = n(),
            WaterYear_Flow_m3y = sum(Flow),
            .groups = "drop"
          ) %>%
          dplyr::filter(num_obs >= 365) %>% # only complete years
          group_by(Flow_Name) %>%
          summarise(
            Mean_WaterYear_Flow_Ls = mean(WaterYear_Flow_m3y * 1000 / (num_obs*24*3600)),
            Mean_WaterYear_Flow_MLy = mean(WaterYear_Flow_m3y * 1000 / 1e6),
            .groups = "drop"
          ) %>%
          left_join(flowsheet_loc, by = "Flow_Name")
        
        storage_df <- flowsheet_dfs$storage_flow %>%
          dplyr::filter(Date >= input$date_range[1],
                        Date <= input$date_range[2]) %>%
          mutate(Year = year(Date),
                 Month = month(Date),
                 Water_Year = ifelse(Month < month_idx, Year - 1, Year)) %>%
          group_by(Flow_Name, Water_Year) %>%
          summarise(
            num_obs = n(),
            WaterYear_Flow_m3y = sum(Flow),
            .groups = "drop"
          ) %>%
          dplyr::filter(num_obs >= 365) %>% # only complete years
          group_by(Flow_Name) %>%
          summarise(
            Mean_WaterYear_Flow_Ls = mean(WaterYear_Flow_m3y * 1000 / (num_obs*24*3600)),
            Mean_WaterYear_Flow_MLy = mean(WaterYear_Flow_m3y * 1000 / 1e6),
            .groups = "drop"
          ) %>%
          left_join(flowsheet_loc, by = "Flow_Name")
        
        if (input$flowchart_unit == "L/s") {
          summary_df$WaterYear_Value <- summary_df$Mean_WaterYear_Flow_Ls
          storage_df$WaterYear_Value <- storage_df$Mean_WaterYear_Flow_Ls
          
        } else {
          summary_df$WaterYear_Value <- summary_df$Mean_WaterYear_Flow_MLy
          storage_df$WaterYear_Value <- storage_df$Mean_WaterYear_Flow_MLy
        }
        
      } else {
        summary_df <- flowsheet_dfs$load %>%
          dplyr::filter(Constituent == input$var_flowchart,
                        Date >= input$date_range[1],
                        Date <= input$date_range[2]) %>%
          mutate(Year = year(Date),
                 Month = month(Date),
                 Water_Year = ifelse(Month < month_idx, Year - 1, Year)) %>%
          group_by(Flow_Name, Water_Year) %>%
          summarise(
            num_obs = n(),
            WaterYear_Load_kgy = sum(Load),
            .groups = "drop"
          ) %>%
          dplyr::filter(num_obs >= 365) %>% # only complete years
          group_by(Flow_Name) %>%
          summarise(
            Mean_WaterYear_Load_kgy = mean(WaterYear_Load_kgy),
            Mean_WaterYear_Load_ty = mean(WaterYear_Load_kgy / 1000),
            .groups = "drop"
          ) %>%
          left_join(flowsheet_loc, by = "Flow_Name")
        
        storage_df <- flowsheet_dfs$storage_load %>%
          dplyr::filter(Constituent == input$var_flowchart,
                        Date >= input$date_range[1],
                        Date <= input$date_range[2]) %>%
          mutate(Year = year(Date),
                 Month = month(Date),
                 Water_Year = ifelse(Month < month_idx, Year - 1, Year)) %>%
          group_by(Flow_Name, Water_Year) %>%
          summarise(
            num_obs = n(),
            WaterYear_Load_kgy = sum(Load),
            .groups = "drop"
          ) %>%
          dplyr::filter(num_obs >= 365) %>% # only complete years
          group_by(Flow_Name) %>%
          summarise(
            Mean_WaterYear_Load_kgy = mean(WaterYear_Load_kgy),
            Mean_WaterYear_Load_ty = mean(WaterYear_Load_kgy / 1000),
            .groups = "drop"
          ) %>%
          left_join(flowsheet_loc, by = "Flow_Name")
        
        if (input$flowchart_unit == "kg/year") {
          summary_df$WaterYear_Value <- summary_df$Mean_WaterYear_Load_kgy
          storage_df$WaterYear_Value <- storage_df$Mean_WaterYear_Load_kgy
          
        } else {
          summary_df$WaterYear_Value <- summary_df$Mean_WaterYear_Load_ty
          storage_df$WaterYear_Value <- storage_df$Mean_WaterYear_Load_ty
        }
      }
      
      
      plt <- ggplot() +
        background_image(png::readPNG("www/RJ WLBM Flowsheet With Backgound.png")) +
        geom_text(data = summary_df, aes(x = X, y = Y, 
                                         label = my_comma(WaterYear_Value)),
                  size = 8/.pt) +
        geom_text(
          data = storage_df %>%
            mutate(deparsed_label = sapply(my_comma(WaterYear_Value), deparse)), 
          aes(
            x = X, y = Y, 
            label = paste('Delta', "==", deparsed_label), 
                          
          ),
          parse = TRUE, size = 10/.pt, color = "red3"
        ) +
        scale_x_continuous(limits = c(0, 6021),
                           breaks = seq(0, 6021, 100),
                           minor_breaks = seq(0, 6021, 50)) +
        scale_y_continuous(limits = c(0, 4153),
                           breaks = seq(0, 4153, 100),
                           minor_breaks = seq(0, 4153, 50)) +
        labs(title = tlt,
             subtitle = subtlt) +
        theme_void() +
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5, size = 16))
      
      ggsave("www/RJ WLBM Generated Flowsheet.pdf", plt, width = 20.07, height = 14)
      output$flowchart <- renderUI({
        tags$iframe(style="height:100%; width:100%",
                    src="RJ WLBM Generated Flowsheet.pdf")
      })
      remove_modal_spinner()
      showNotification("Flowsheet Updated!",
                       duration = 5, type = "message")
      
    }
  )
  
  # Reactive WLBM Data ----
  wlbm_df <- reactiveValues(inflow = NULL, outflow = NULL,
                            inload = NULL, outload = NULL,
                            inconc = NULL, outconc = NULL)
  
  # ObserveEvent 02: Selection of Storage & Constituents ----
  # Populate the fluxes & `wlbm_df`
  observeEvent(list(input$storage_select, input$consit, input$ava_xlsx), {
    req(input$storage_select, input$consit,
        all_wlbm_data$main_pit, all_wlbm_data$interm_pit, all_wlbm_data$main_wrd,
        all_wlbm_data$interm_wrd, all_wlbm_data$dysons_wrd, all_wlbm_data$ewsf,
        all_wlbm_data$gwtp, all_wlbm_data$pwtp, all_wlbm_data$ebfr_us,
        all_wlbm_data$ebfr_ds, all_wlbm_data$div_channel, all_wlbm_data$gs)
    selected_element <- switch (
      input$storage_select,
      "Main Pit" = "main_pit",
      "Intermediate Pit" = "interm_pit",
      "Main WRD" = "main_wrd",
      "Intermediate WRD" = "interm_wrd",
      "Dyson's WRD" = "dysons_wrd",
      "EWSF" = "ewsf",
      "Pit Water Treatment Plant" = "pwtp",
      "Groundwater Treatment Plant" = "gwtp",
      "EBFR US" = "ebfr_us",
      "EBFR DS" = "ebfr_ds",
      "Diversion Channel" = "div_channel",
      "GS8150097" = "gs"
    )
    
    lookup_df_flt <- lookup_df %>%
      dplyr::filter(Element == input$storage_select) %>%
      drop_na(`Load New Name`, `Load Name`)
    
    inflow_df <- all_wlbm_data[[selected_element]][["inflow"]]
    outflow_df <- all_wlbm_data[[selected_element]][["outflow"]]
    inload_df <- all_wlbm_data[[selected_element]][["inload"]]
    outload_df <- all_wlbm_data[[selected_element]][["outload"]]
    
    if (length(lookup_df_flt$`Load Name`[which(lookup_df_flt$Type == "In")]) >= 1) {
      load_new_name_in <- paste0(
        rep(lookup_df_flt$`Load Name`[which(lookup_df_flt$Type == "In")], 
            each = length(input$consit)),
        "[", input$consit, "]"
      )
      names(load_new_name_in) <- paste0(
        rep(lookup_df_flt$`Load New Name`[which(lookup_df_flt$Type == "In")], 
            each = length(input$consit)),
        "[", input$consit, "]"
      )
      
      inload_df <- inload_df %>%
        rename(all_of(load_new_name_in))
    }
    
    if (length(lookup_df_flt$`Load Name`[which(lookup_df_flt$Type == "Out")]) >= 1) {
      load_new_name_out <- paste0(
        rep(lookup_df_flt$`Load Name`[which(lookup_df_flt$Type == "Out")], 
            each = length(input$consit)),
        "[", input$consit, "]"
      )
      names(load_new_name_out) <- paste0(
        rep(lookup_df_flt$`Load New Name`[which(lookup_df_flt$Type == "Out")], 
            each = length(input$consit)),
        "[", input$consit, "]"
      )
      
      outload_df <- outload_df %>%
        rename(all_of(load_new_name_out))
    }
    
    inflow_cols <- colnames(inflow_df)
    outflow_cols <- colnames(outflow_df)
    inload_cols <- colnames(inload_df)
    outload_cols <- colnames(outload_df)
    
    # check having load estimations
    # ignoring two only load flow paths for PWTP and GWTP
    # and also three others in EBFR DS/US and Div channel that only has loads
    # if one of the constituents are there, all of the 12 will be there.
    inflow_cols_flt <- inflow_cols[which(paste0(inflow_cols, "_CT[",
                                                input$consit[1],
                                                "]") %in% inload_cols)]
    outflow_cols_flt <- outflow_cols[which(paste0(outflow_cols, "_CT[",
                                                  input$consit[1],
                                                  "]") %in% outload_cols)]
    
    # setting up reactive values
    wlbm_df$inflow <- inflow_df %>%
      dplyr::select(all_of(c("Date", inflow_cols_flt)))
    
    wlbm_df$outflow <- outflow_df %>%
      dplyr::select(all_of(c("Date", outflow_cols_flt)))
    
    wlbm_df$inload <- inload_df
    wlbm_df$outload <- outload_df
    
    # Calculating concentrations
    inconc_df <- data.frame(
      Date = inload_df$Date
    )
    outconc_df <- data.frame(
      Date = outload_df$Date
    )
    
    for (cnc in input$consit) {
      for (p in inflow_cols_flt) {
        inconc_df$var <- ifelse(
          is.nan(
            (inload_df %>% pull(paste0(p, "_CT[", cnc, "]"))) / 
              (inflow_df %>% pull(p)) * 1000 / (24*3600)
          ) |
            is.infinite(
              (inload_df %>% pull(paste0(p, "_CT[", cnc, "]"))) / 
                (inflow_df %>% pull(p)) * 1000 / (24*3600)
            ),
          0,
          (inload_df %>% pull(paste0(p, "_CT[", cnc, "]"))) / 
            (inflow_df %>% pull(p)) * 1000 / (24*3600)
        )
        colnames(inconc_df)[ncol(inconc_df)] <- paste0(p, "_CT[", cnc, "]")
      }
    }
    
    for (cnc in input$consit) {
      for (p in outflow_cols_flt) {
        outconc_df$var <- ifelse(
          is.nan(
            (outload_df %>% pull(paste0(p, "_CT[", cnc, "]"))) / 
              (outflow_df %>% pull(p)) * 1000 / (24*3600)
          ) |
            is.infinite(
              (outload_df %>% pull(paste0(p, "_CT[", cnc, "]"))) / 
                (outflow_df %>% pull(p)) * 1000 / (24*3600)
            ),
          0,
          (outload_df %>% pull(paste0(p, "_CT[", cnc, "]"))) / 
            (outflow_df %>% pull(p)) * 1000 / (24*3600)
        )
        colnames(outconc_df)[ncol(outconc_df)] <- paste0(p, "_CT[", cnc, "]")
      }
    }
    
    wlbm_df$inconc <- inconc_df
    wlbm_df$outconc <- outconc_df
  },
  priority = 10)
  
  # Observe 02-1: Populating Flow Paths ----
  observe({
    req(wlbm_df$inflow, wlbm_df$outflow)
    flow_columns <- c(
      colnames(wlbm_df$inflow)[2:ncol(wlbm_df$inflow)],
      colnames(wlbm_df$outflow)[2:ncol(wlbm_df$outflow)]
    )
    updateSelectInput(session, "chosen_flux", 
                      choices = flow_columns)
  },
  priority = 9)
  
  # ObserveEvent 03: Populate unit options ----
  observeEvent(input$consit_var, {
    req(input$consit_var)
    if (input$consit_var == "Load") {
      ### `chosen_unit` ----
      output$placeholder_consit_unit <- renderUI({
        selectInput(
          inputId = "chosen_unit",
          label = "Change Unit",
          choices = c("Kg", "Tonne")
        )
      })
    } else {
      output$placeholder_consit_unit <- renderUI({
        selectInput(
          inputId = "chosen_unit",
          label = "Change Unit",
          choices = c("mg/L")
        )
      })
    }
  }, priority = 9)
  
  # Rendering First Flow Chart ----
  output$flowchart <- renderUI({
    tags$iframe(style="height:100%; width:100%",
                src="RJ WLBM Flowsheet Adjusted.pdf")
  })
  
  # ObserveEvent 04: Date Labels ----
  date_labs <- reactiveValues(breaks_major = NULL, labels = NULL,
                              breaks_minor = NULL)
  observeEvent(input$date_range, {
    req(input$date_range[1], input$date_range[2])
    diff_days <- as.numeric(input$date_range[2] - input$date_range[1])
    if (diff_days >= 1461) {
      # equal or more than four years
      date_labs$breaks_major <- "12 months"
      date_labs$labels <- "%Y-%b"
      date_labs$breaks_minor <- "3 months"
      
    } else if (diff_days >= 730) {
      # equal or more than two years
      date_labs$breaks_major <- "6 months"
      date_labs$labels <- "%Y-%b"
      date_labs$breaks_minor <- "3 months"
      
    } else if (diff_days >= 365){
      # equal or more than one year
      date_labs$breaks_major <- "4 months"
      date_labs$labels <- "%Y-%b"
      date_labs$breaks_minor <- "2 months"
      
    } else if (diff_days >= 180){
      # equal or more than half year
      date_labs$breaks_major <- "2 months"
      date_labs$labels <- "%Y-%b"
      date_labs$breaks_minor <- "1 months"
    } else if (diff_days >= 90) {
      # equal or more than quarter year
      date_labs$breaks_major <- "30 days"
      date_labs$labels <- "%Y-%b-%d"
      date_labs$breaks_minor <- "15 days"
    } else {
      date_labs$breaks_major <- "15 days"
      date_labs$labels <- "%Y-%b-%d"
      date_labs$breaks_minor <- "5 days"
    }
  }, priority = 10)
  
  
  # Observer 05: Filter Date, Variable Type & Unit Then Plot -----
  # Also need to add the discharge series to the plot
  # Reactive DF Plot ----
  df_plot <- reactiveValues(df1 = NULL, df2 = NULL)
  
  # Reactive Status Check for Updating Chosen Flux ----
  check_status <- reactive({
    req(input$consit)
    if (
      paste0(input$chosen_flux, "_CT[", input$consit[1], "]") %in% 
      colnames(wlbm_df$inload) |
      paste0(input$chosen_flux, "_CT[", input$consit[1], "]") %in% 
      colnames(wlbm_df$outload)
    ) {
      TRUE
    } else {
      NULL
    }
  })
  
  observe({
    req(check_status())
    
    if (input$consit_var == "Load") {
      df_main <- wlbm_df$inload %>%
        left_join(wlbm_df$outload, by = "Date") %>%
        dplyr::select(
          c(
            "Date",
            paste0(rep(input$chosen_flux, each = length(input$consit)),
                   "_CT[", input$consit, "]")
          )
        )
      
      df_main <- pivot_longer(df_main, cols = c(2:ncol(df_main)),
                              names_to = "consit", values_to = "value") %>%
        mutate(consit = str_extract(consit, "\\[(.*?)\\]")) %>%
        mutate(consit = str_replace_all(consit, "[\\[\\]]", ""))
      
      if (input$chosen_unit == "Kg") {
        df_plot$df1 <- df_main %>%
          dplyr::filter(Date >= input$date_range[1],
                        Date <= input$date_range[2])
        
      } else {
        # kg to tonne
        df_main <- df_main %>%
          mutate(value =  value/1000) %>%
          dplyr::filter(Date >= input$date_range[1],
                        Date <= input$date_range[2])
        
        df_plot$df1 <- df_main %>%
          dplyr::filter(Date >= input$date_range[1],
                        Date <= input$date_range[2])
      }
      
    } else {
      
      df_main <- wlbm_df$inconc %>%
        left_join(wlbm_df$outconc, by = "Date") %>%
        dplyr::select(
          c(
            "Date",
            paste0(rep(input$chosen_flux, each = length(input$consit)),
                   "_CT[", input$consit, "]")
          )
        )
      
      df_main <- pivot_longer(df_main, cols = c(2:ncol(df_main)),
                              names_to = "consit", values_to = "value") %>%
        mutate(consit = str_extract(consit, "\\[(.*?)\\]")) %>%
        mutate(consit = str_replace_all(consit, "[\\[\\]]", ""))
      
      df_plot$df1 <- df_main %>%
        dplyr::filter(Date >= input$date_range[1],
                      Date <= input$date_range[2])
    }
    
    df_plot$df2 <- wlbm_df$inflow %>%
      left_join(wlbm_df$outflow, by = "Date") %>%
      dplyr::select(
        c("Date", input$chosen_flux)
      ) %>%
      dplyr::filter(Date >= input$date_range[1],
                    Date <= input$date_range[2])
  }, priority = 0)
  
  
  # Reactive Status Check for Updating DF Plot ----
  check_status_consit <- reactive({
    req(df_plot$df1, df_plot$df2, input$consit)
    if (
      sum(input$consit %in% unique(df_plot$df1$consit)) == length(input$consit) &
      length(input$consit) == length(unique(df_plot$df1$consit))
    ) {
      TRUE
    } else {
      NULL
    }
  })
  
  # Reactive Plot Generation ----
  out_plt <- reactive({
    req(df_plot$df1, df_plot$df2, check_status_consit)
    
    main_lab <- ""
    if (input$chosen_unit == "Tonne") {
      main_lab <- "Daily Load, t/day"
    } else if (input$chosen_unit == "Kg") {
      main_lab <- "Daily Load, kg/day"
    } else {
      main_lab <- "Daily Concentration, mg/L"
    }
    
    flux_name <- strsplit(input$chosen_flux, split = "T_")[[1]][2] %>%
      str_replace_all(., "_", " ")
    
    if (input$add_flow) {
      df_q <- df_plot$df2
      colnames(df_q)[ncol(df_q)] <- "Discharge"
      df_q$Discharge <- df_q$Discharge * 1000
      
      # adding secondary flow axis
      max_first  <- max(df_plot$df1$value)
      max_second <- max(df_q %>% pull(2))
      min_first  <- min(df_plot$df1$value)
      min_second <- min(df_q %>% pull(2))
      scale = (max_second - min_second)/(max_first - min_first)
      shift = min_first - min_second
      
      if (max_first == 0) {
        scale = 1
        shift = 0
        return(
          ggplot(data = df_q, aes(x = Date)) +
            geom_line(data = df_plot$df1,
                      aes(y = value, color = consit),
                      linewidth = 0.6, alpha = 0.75) +
            scale_color_manual(values = palette12[c(1:length(input$consit))],
                               guide = guide_legend(order = 1,
                                                    alpha = 1)) +
            labs(color = "") +
            new_scale_color() +
            scale_x_date(date_breaks = date_labs$breaks_major,
                         date_minor_breaks = date_labs$breaks_minor,
                         date_labels = date_labs$labels) +
            geom_line(data = df_q,
                      aes(y = inv_scale_function(Discharge, scale, shift),
                          color = "Water"),
                      linetype = "dashed", linewidth = 0.6) +
            scale_color_manual(
              values = c("Water" = rgb(0.2, 0.2, 0.8, alpha = 0.75)),
              guide = guide_legend(order = 2, alpha = 1)
            ) +
            labs(x = "", y = main_lab, color = "",
                 caption = paste("Flow Path:", flux_name)) +
            scale_y_continuous(limits = c(min_second, max_second),
                               sec.axis = sec_axis(~scale_function(., scale, shift),
                                                   name = expression("Daily Average Discharge (L/s)"))) +
            theme_bw() +
            theme(axis.title.y = element_text(face = "plain"),
                  legend.position = "top", legend.direction = "horizontal",
                  plot.caption = element_text(face = "italic",
                                              size = 10, vjust = 0, hjust = 0))
        )
        
      } else {
        return(
          ggplot(data = df_q, aes(x = Date)) +
            geom_line(data = df_plot$df1,
                      aes(y = value, color = consit),
                      linewidth = 0.6, alpha = 0.75) +
            scale_color_manual(values = palette12[c(1:length(input$consit))],
                               guide = guide_legend(order = 1,
                                                    alpha = 1)) +
            labs(color = "") +
            new_scale_color() +
            scale_x_date(date_breaks = date_labs$breaks_major,
                         date_minor_breaks = date_labs$breaks_minor,
                         date_labels = date_labs$labels) +
            geom_line(data = df_q,
                      aes(y = inv_scale_function(Discharge, scale, shift),
                          color = "Water"),
                      linetype = "dashed", linewidth = 0.6) +
            scale_color_manual(
              values = c("Water" = rgb(0.2, 0.2, 0.8, alpha = 0.75)),
              guide = guide_legend(order = 2, alpha = 1)
            ) +
            labs(x = "", y = main_lab, color = "",
                 caption = paste("Flow Path:", flux_name)) +
            scale_y_continuous(limits = c(min_first, max_first),
                               sec.axis = sec_axis(~scale_function(., scale, shift),
                                                   name = expression("Daily Average Discharge (L/s)"))) +
            theme_bw() +
            theme(axis.title.y = element_text(face = "plain"),
                  legend.position = "top", legend.direction = "horizontal",
                  plot.caption = element_text(face = "italic",
                                              size = 10, vjust = 0, hjust = 0))
        )
      }
      
      
    } else {
      return(
        df_plot$df1 %>%
          ggplot(aes(x = Date, y = value, color = consit)) +
          geom_line(linewidth = 0.6, alpha = 0.75) +
          scale_color_manual(values = palette12[c(1:length(input$consit))],
                             guide = guide_legend(order = 1,
                                                  alpha = 1)) +
          scale_x_date(date_breaks = date_labs$breaks_major,
                       date_minor_breaks = date_labs$breaks_minor,
                       date_labels = date_labs$labels) +
          labs(x = "", y = main_lab, color = "",
               caption = paste("Flow Path:", flux_name)) +
          theme_bw() +
          theme(axis.title.y = element_text(face = "plain"),
                legend.position = "top", legend.direction = "horizontal",
                plot.caption = element_text(face = "italic",
                                            size = 10, vjust = 0, hjust = 0)) 
      )
    }
  })
  
  # Showing the plot ----
  output$plt <- renderPlot({
    out_plt()
  }, res = 130)
  
  # Downloading the plot
  output$download_plot <- downloadHandler(
    filename = function() { "output_plot.png" },
    content = function(file) {
      png(file, width = input$plot_width, height = input$plot_height,
          res = 300, units = "in")
      plot(out_plt())
      dev.off()
    }
  )
}

shinyApp(ui, server)



