# Setup ----------------------------------------------------------


## Function to check if packages are loaded
check_packages <- function(packages) {
  for (pkg in packages) {
    if (!pkg %in% installed.packages()[, "Package"]) {
      stop(paste("Package", pkg, "is not installed."))
    }
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is not loaded."))
    }
    cat(paste(pkg, "is loaded.\n"))
  }
}

# Start of the app
cat("App started, loading libraries...\n")

# Shiny packages
library(shiny)
library(shinyjs)
library(shinycssloaders)

# Tidyverse packages
library(dplyr)
library(ggplot2)
library(readr)

# For map plot
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Other
library(gtsummary)
library(gt)
library(glue)
library(plotly)

# Check if packages are loaded
cat("Checking if all required packages are loaded...\n")
required_packages <- c(
  "shiny", "shinyjs", "shinycssloaders",
  "dplyr", "ggplot2", "readr",
  "sf", "rnaturalearth", "rnaturalearthdata",
  "gtsummary", "gt", "glue", "plotly"
)

# Validate that all required packages are installed and loaded
tryCatch({
  check_packages(required_packages)
  cat("All required packages are successfully loaded.\n")
}, error = function(e) {
  cat("Error in loading packages:", e$message, "\n")
  stop("Aborting app startup due to package loading error.")
})
flush.console()

## Global options ----------------------------------------------------------


# loading spinner aestestics
options(spinner.color = "#4682B4", spinner.color.background = "#ffffff", spinner.size = 2)


## Load objects   ----------------------------------------------------------

# Helper function to check if an object is loaded correctly
check_object <- function(object) {
  object_name <- deparse(substitute(object))
  if (is.null(object)) {
    stop(glue("{object_name} is NULL. Check if the file path is correct and the file exists."))
  } else {
    cat(glue("{object_name} loaded successfully.\n"))
  }
}


### Sample descriptives   ----------------------------------------------------------

res_tbl1 <- readRDS(file.path("rds_objects", "Table1.rds")) 

check_object(res_tbl1)
if (is.null(res_tbl1)) {
  stop("res_tbl1 is NULL. Check if the file path is correct and the file exists.")
}

res_grp_tbl1 <- readRDS(file.path("rds_objects", "Table1_group.rds")) 
check_object(res_grp_tbl1)
if (is.null(res_grp_tbl1)) {
  stop("res_grp_tbl1 is NULL. Check if the file path is correct and the file exists.")
}

epm_tbl1 <- readRDS(file.path("rds_objects", "EPM_Table1.rds"))
check_object(epm_tbl1)
if (is.null(epm_tbl1)) {
  stop("epm_tbl1 is NULL. Check if the file path is correct and the file exists.")
}

nonres_tbl <- readRDS(file.path("rds_objects", "nonres_tbl.rds"))
check_object(nonres_tbl)
if (is.null(nonres_tbl)) {
  stop("nonres_tbl is NULL. Check if the file path is correct and the file exists.")
}

# Map data

# Get csv with lat and long
mapdat <- read.csv2(file.path("csv_objects", "map_data.csv"))
check_object(mapdat)
if (is.null(mapdat)) {
  stop("mapdat is NULL. Check if the file path is correct and the file exists.")
}

# natural earth maps
sweden_map <- ne_countries(scale = "medium", country = "Sweden", returnclass = "sf")
check_object(sweden_map)

# Convert to sf object
uni_st <- st_as_sf(mapdat, coords = c("Long", "Lat"), crs = 4326)

stopifnot(all(c("University", "N") %in% names(uni_st)))

coords <- sf::st_coordinates(uni_st)

uni_st$lon <- coords[, 1]
uni_st$lat <- coords[, 2]

uni_st$hover_text <- paste0(
  "<b>", uni_st$University, "</b><br>",
  "N: ", uni_st$N
)


### Means and SDs fields and reviewers ------------------------------------------

all <- readRDS(file.path("rds_objects", "difference_tbl.rds"))
check_object(all)
if (is.null(all)) {
  stop("all is NULL. Check if the file path is correct and the file exists.")
}


all <- all %>%  select(practice, category, starts_with("Researchers"), starts_with("Difference_Researcher"), starts_with("EPM"), starts_with("Difference_EPM")) %>% arrange(practice)


### Regressions ----------------------------------------------------------


fits_std <- readRDS(file.path("rds_objects", "fits_std2.rds"))
check_object(fits_std)
if (is.null(fits_std)) {
  stop("fits_std is NULL. Check if the file path is correct and the file exists.")
}

fits_unstd <- readRDS(file.path("rds_objects", "fits_unstd2.rds"))
check_object(fits_unstd)
if (is.null(fits_unstd)) {
  stop("fits_unstd is NULL. Check if the file path is correct and the file exists.")
}

fits_uw_std <- readRDS(file.path("rds_objects", "fits_uw_std2.rds"))
check_object(fits_uw_std)
if (is.null(fits_uw_std)) {
  stop("fits_uw_std is NULL. Check if the file path is correct and the file exists.")
}

fits_uw_unstd <- readRDS(file.path("rds_objects", "fits_uw_unstd2.rds"))
check_object(fits_uw_unstd)
if (is.null(fits_uw_unstd)) {
  stop("fits_uw_unstd is NULL. Check if the file path is correct and the file exists.")
}



### Means and SDs subfields -------------------------------------------------------------

sub_tbl_med <- readRDS(file.path("rds_objects", "sub_tbl_med.rds")) 
check_object(sub_tbl_med)
if (is.null(sub_tbl_med)) {
  stop("sub_tbl_med is NULL. Check if the file path is correct and the file exists.")
}

sub_tbl_nat <- readRDS(file.path("rds_objects", "sub_tbl_nat.rds"))
check_object(sub_tbl_nat)
if (is.null(sub_tbl_nat)) {
  stop("sub_tbl_nat is NULL. Check if the file path is correct and the file exists.")
}

sub_tbl_soc <- readRDS(file.path("rds_objects", "sub_tbl_soc.rds"))
check_object(sub_tbl_soc)
if (is.null(sub_tbl_soc)) {
  stop("sub_tbl_soc is NULL. Check if the file path is correct and the file exists.")
}

sub_tbl_hum <- readRDS(file.path("rds_objects", "sub_tbl_hum.rds"))
check_object(sub_tbl_hum)
if (is.null(sub_tbl_hum)) {
  stop("sub_tbl_hum is NULL. Check if the file path is correct and the file exists.")
}


### Filter objects

items <- all$practice

categories <- unique(all$category)

items_cats <- all %>% select(practice, category)


### UMAP coordinates ---------------------------------------------------------


umap_data <- read.csv(file.path("csv_objects", "research_field_coordinates.csv"))
check_object(umap_data)
if (is.null(umap_data)) {
  stop("umap_data is NULL. Check if the file path is correct and the file exists.")
}

# Define color palette for research areas
umap_area_colors <- c(
  "Medical Sciences" = "#E74C3C",
  "Social Sciences" = "#3498DB",
  "Natural Sciences" = "#27AE60",
  "Humanities" = "#9B59B6"
)


# User interface ----------------------------------------------------------


ui <- fluidPage(
  
  titlePanel("Researchers' and ethics reviewers' attitudes to RM and QRPs"),
  
  ## Styling and functionality  --------------------------------------------------------
  
  tags$head(
    tags$script(
      HTML('$(document).on("shiny:busy", function() {
             $("#loading0").show();
           });

           $(document).on("shiny:idle", function() {
             $("#loading0").hide();
           });' ,
           
           '$(document).on("shiny:busy", function() {
             $("#loading1").show();
           });

           $(document).on("shiny:idle", function() {
             $("#loading1").hide();
           });' ,
           
           '$(document).on("shiny:busy", function() {
             $("#loading2").show();
           });

           $(document).on("shiny:idle", function() {
             $("#loading2").hide();
           });' ,
           
           '$(document).on("shiny:busy", function() {
             $("#loading3").show();
           });

           $(document).on("shiny:idle", function() {
             $("#loading3").hide();
           });' ,
           
           
           '$(document).on("shiny:busy", function() {
             $("#loading4").show();
           });

           $(document).on("shiny:idle", function() {
             $("#loading4").hide();
           });' ,
           
           '$(document).on("shiny:busy", function() {
             $("#loading5").show();
           });

           $(document).on("shiny:idle", function() {
             $("#loading5").hide();
           });' ,
           
           
           '$(document).ready(function() {
            // Function to re-render plot on window resize
            $(window).on("resize", function() {
              Shiny.setInputValue("windowResized", Math.random()); // Trigger Shiny input
            });
          });
          
          // Function to handle window resize
          $(window).on("resize", function() {
          // Show message when resizing
          $("#rescaleMessage").text("Rescaling plot...");
          // Delay to avoid flickering messages
          setTimeout(function() {
          $("#rescaleMessage").text("");
          }, 2000); // Adjust delay time as needed
          });
            '
      )
    ),
    tags$style(HTML("
      .navbar-default {
        background-color: white;
      }
      /* Styles for the navbar links (The pages) */
      .navbar-default .navbar-nav > li > a {
        color: darkgrey;
      }
      .navbar-default .navbar-nav > li > a:hover {
        background-color: white !important;
        color: lightgrey;
      }
      .navbar-default .navbar-nav > .clicked > a {
        border-color: black !important;
        background-color: white !important; /* Background color for clicked but not active link */
        color: black !important; /* Text color for clicked but not active link */
      }
      
      .navbar-default .navbar-nav > .active > a {
        border-color: black !important;
        background-color: white !important;; /* Background color of active navbar link */
        color: black !important; /* Text color of active navbar link */
      }
      
      /* Styles for the navbar brand (Select page) */
      .navbar-default .navbar-brand {
        color: black; /* Text color */
      }
      .navbar-default .navbar-brand:hover {
        color: black; /* Text color of the navbar brand on hover */
      }
       /* Styles for the navbar toggle button (for collapsed view) */
      .navbar-default .navbar-toggle {
        border-color: black;
      }
      .navbar-default .navbar-toggle .icon-bar {
        background-color: white;
      }
      }
      
      /* Custom CSS for centering plotly within wellPanel */
      .centered-plot {
        height: 100%; /* Set the height to fill the wellPanel */
        justify-content: center;
        align-items: center;
      }
      .centered-plot .plot-container {
        width: 100%; /* Ensure plotly plot fills the container */
        height: 100%; /* Ensure plotly plot fills the container */
      }
      
    ")
    ),
  ),
  
  ## Navigation bar  --------------------------------------------------------
  
  navbarPage(
    "Select page:", # Title of the navigation bar
    
    ## "Project info" page  --------------------------------------------------------
    
    # First tab panel for the front page content
    tabPanel(
      "Project info", # Title of the tab panel
      
      # Welcome page content
      mainPanel(
        #wellPanel(style = "background: white",
        fluidRow(
          column(width = 6, offset = 0, align = "left", 
                 wellPanel(style = "background: white ; height: 625px",
                           htmlOutput("centered_text_left")),
          ),
          column(width = 6, offset = 0, align = "center", 
                 wellPanel(style = "background: white ; height: 625px",
                           htmlOutput("centered_text_right"))
          )
          #)
        ),
        
        width = 12
      ),
      hr(),
      p(
        "All content on this dashboard is made public under CC BY-NC 4.0 license. The dashboard was created by Amanda M. Lindkvist."
      )
    ),
    
    ## "Sample descriptives" page -------------------------------------------------------
    
    tabPanel(
      "Sample descriptives",
      sidebarLayout(
        sidebarPanel(style = "background: white",
                     # Add a checkbox for hiding the sidebar
                     checkboxInput("hide_sidebar_page1", "Hide Sidebar", value = FALSE),
                     
                     conditionalPanel(style = "background: white",
                                      condition = "!input.hide_sidebar_page1",
                                      
                                      wellPanel(style = "background: white",
                                                # Sidebar filters
                                                selectInput("filter_desctbl", "Choose descriptive table to display:", 
                                                            choices = c("Researchers", "Researchers by field", "Ethics reviewers", "Researcher response rate")),
                                      ),
                                      wellPanel(style = "background: white",
                                                tags$p("The map below shows number of responses from each Swedish university. 
                   Hover over a point to see the name of the university and the number of responses from there.
                   Use the plot menu to zoom and pan.")
                                      ), 
                                      wellPanel(style = "background: white", 
                                                tags$div(id = "loading4", style = "display:none; text-align:center;",
                                                         "Loading plot... Please wait."),
                                                #Display the plot
                                                div(id = "rescaleMessage", style = "text-align:center"),
                                                div(class = "centered-plot",
                                                    plotlyOutput("map_plot", width = "100%", height = "100%"))
                                                
                                      ),
                                      width = 4
                     ),
        ),
        mainPanel(
          # Use withSpinner to display a spinner while loading with a loading message
          tags$br(),
          tags$div(id = "loading0", style = "display:none;",
                   "Loading table... Please wait."),
          # Display the gt table
          withSpinner(gt_output("table_sample_desc")),
          width = 8,
          style = "text-align: center;"
        )
      ),
      hr(),
      p(
        "All content on this dashboard is made public under CC BY-NC 4.0 license. The dashboard was created by Amanda M. Lindkvist."
      )
    ),
    
    

    ## "Researchers' and ethics reviewers' attitudes" page -------------------------------------------
    
    
    tabPanel(
      "Researchers' and ethics reviewers' attitudes",
      sidebarLayout(
        sidebarPanel(style = "background: white",
                     # Add a checkbox for hiding the sidebar
                     checkboxInput("hide_sidebar_page2", "Hide Sidebar", value = FALSE),
                     conditionalPanel(style = "background: white",
                                      condition = "!input.hide_sidebar_page2",
                                      # Sidebar filters
                                      wellPanel(style = "background: white",
                                                selectInput("cat_filter", "Filter by practice category:", 
                                                            choices = c("All categories", categories)),
                                                selectInput("item_filter", "Filter by practice:", 
                                                            choices = c("All practices", items)),
                                                # selectInput("group_filter", "Show all groups, only researchers or only the full researcher group:",
                                                #             choices = c("All groups", "Researchers", "Only all researchers")),
                                                selectInput("diff_filter", "Show largest differences between group means:", 
                                                            choices = c("Yes", "No")), 
                                      ),
                                      wellPanel(style = "background: white",
                                                # Additional text below the sidebar
                                                tags$p("Click on the column you want to sort the table by.",
                                                       tags$br(), " ",
                                                       tags$br(), "The rating scale ranged from 1 = Completely unacceptable to 7 = Completely acceptable, with 4 = Neutral as the midpoint.",
                                                       tags$br(), " ",
                                                       tags$br(), "Means (M) and standard deviations (SD) were weighted for the full researcher sample. 
                   The weighting variable included age, academic field, employer, employee category and country of birth.",
                                                       tags$br(), " ")
                                      )
                     ),
                     width = 3
        ),
        mainPanel(
          # Use withSpinner to display a spinner while loading with a loading message
          tags$br(),
          tags$div(id = "loading2", style = "display:none; text-align:center;",
                   "Loading table... Please wait."),
          # Display the gt table
          withSpinner(gt_output("table")),
          width = 9
        )
      ),
      hr(),
      p(
        "All content on this dashboard is made public under CC BY-NC 4.0 license. The dashboard was created by Amanda M. Lindkvist."
      )
    ),
    

    
    ## "Predictors of researcher attitudes" page  ------------------------------------
    
    tabPanel(
      "Predictors of researcher attitudes",
      sidebarLayout(
        sidebarPanel(style = "background: white",
                     # Add a checkbox for hiding the sidebar
                     checkboxInput("hide_sidebar_page3", "Hide Sidebar", value = FALSE),
                     
                     conditionalPanel(
                       condition = "!input.hide_sidebar_page3",
                       wellPanel(style = "background: white",
                                 # Sidebar filters
                                 selectInput("filter_regwgt", "Choose weighted or unweighted regression:", 
                                             choices = c("Weighted", "Unweighted")),
                                 selectInput("filter_regstd", "Choose outcome format:", 
                                             choices = c("Standardized", "Unstandardized")),
                                 # Sidebar text
                                 tags$p("Click the legend below the plot to select the 'Regression model outcome' that you want visible. 
                 Hover over the plot to view exact estimates with corresponding Confidence Intervals.",
                                        tags$br(), " ",
                                        tags$br(), "Outcome variables are (standardized) composite scores based on the mean acceptability scores of all 52
                 practices (All practices) or the practices included in a specific category, calculated for each respondent. 
                 In the case of missing data, the score was calculated using the items without missing data.",
                                        tags$br(), " ",
                                        tags$br(), "In the weighted linear regressions, the weighting variable included age, academic field, employer, employee category and country of birth.")
                       )
                     ),
                     width = 3
        ),
        mainPanel(style = "height: 900px",
                  div(class = "centered-plot",
                      plotlyOutput("reg_plot"), 
                  ),
                  width = 9 
        )
      ),
      hr(),
      p(
        "All content on this dashboard is made public under CC BY-NC 4.0 license. The dashboard was created by Amanda M. Lindkvist."
      )
    ),
    
    ## "Researcher attitudes by subfield" page -------------------------------
    
    
    tabPanel(
      "Researcher attitudes, by subfield",
      sidebarLayout(
        sidebarPanel(
          style = "background: white",
          # Add a checkbox for hiding the sidebar
          checkboxInput("hide_sidebar_page4", "Hide Sidebar", value = FALSE),
          conditionalPanel(
            style = "background: white",
            condition = "!input.hide_sidebar_page4",
            # Sidebar filters
            wellPanel(
              style = "background: white",
              selectInput("area_filter4", "Filter by academic field:",
                          choices = c("Medical and Health sciences", "Natural sciences", "Social sciences", "Humanities")),
              selectInput("cat_filter_page4", "Filter by practice category:",
                          choices = c("All categories", categories)),
              selectInput("item_filter_page4", "Filter by practice:",
                          choices = c("All practices", items))
            ),
            wellPanel(style = "background: white",
                      # Additional text below the sidebar
                      tags$p("Click on the column you want to sort the table by.",
                             tags$br(), " ",
                             tags$br(),"The rating scale ranged from 1 = Completely unacceptable to 7 = Completely acceptable, with 4 = Neutral as the midpoint.",
                             tags$br(), " ")
            )
          ),
          width = 3
        ),
        mainPanel(
          # Use withSpinner to display a spinner while loading with a loading message
          tags$br(),
          tags$div(
            id = "loading4",
            style = "display:none; text-align:center;",
            "Loading table... Please wait."
          ),
          # Display the gt table
          withSpinner(gt_output("subfield_tbl")),
          width = 9
        )
      ),
      hr(),
      p(
        "All content on this dashboard is made public under CC BY-NC 4.0 license. The dashboard was created by Amanda M. Lindkvist."
      )
    ),
    
    
    
    ## "Subfield attitude similarity" page ---------------------------------------
    
    tabPanel(
      "Subfield attitude similarity",
      sidebarLayout(
        sidebarPanel(
          style = "background: white",
          # Add a checkbox for hiding the sidebar
          checkboxInput("hide_sidebar_page5", "Hide Sidebar", value = FALSE),
          conditionalPanel(
            style = "background: white",
            condition = "!input.hide_sidebar_page5", 
          wellPanel(style = "background: white",
            tags$p("Click on a research field in the right-hand legend to hide or display the points from that field.",
                   tags$br(), " ",
                   tags$br(), "The two-dimensional layout was generated using Uniform Manifold Approximation and Projection (UMAP) and illustrates similarity between research fields and subfields in acceptability ratings of the 52 items.",
                   tags$br(), " ",
                   tags$br(), "Only subfields with n > 5 respondents were included in the analysis.")
            )
          ),
          width = 3
        ),
        mainPanel(
          style = "height: 700px",
          div(
            class = "centered-plot",
            plotlyOutput("umap_plot")
          ),
          width = 9
        )
      ),
      hr(),
      p(
        "All content on this dashboard is made public under CC BY-NC 4.0 license. The dashboard was created by Amanda M. Lindkvist."
      )
    )
    
  
  )
)



# Server / Output ----------------------------------------------------------


server <- function(input, output, session) {

  cat("Server function is starting...\n")
  flush.console()
  shinyjs::useShinyjs()
  
  
  ## "Project info" page  --------------------------------------------------------
  
  
  output$centered_text_left <- renderUI({
    tagList(
      h2("Project information"),
      p(
        "This dashboard presents the findings of a research project about attitudes towards research misconduct (RM) and questionable research practices (QRPs)."
      ),
      p(
        "With this project, we provide a comprehensive overview of attitudes towards research integrity among researchers 
        and highlight differences and similarities in perceptions of research integrity between groups of researchers, with a primary focus on discipline-specific differences.
        Additionally, we highlight differences and similarities between ethical perceptions held by members of ethical review committees 
        to those held by the wider researcher community. "
      ),
      br(), 
      h3("Data details"),
      p(
        "The survey was conducted using a total survey sampling approach, 
        sampling the full population of active researchers and PhD students within Sweden, 
        response rate = 33.2%, total N = 11,050. 
        The data collection from researchers was conducted with the help of Statistics Sweden. 
        Demographic and occupational variables were collected from national registries and connected to individual survey responses by Statistics Sweden. 
        Members of the Ethical Review Authority in Sweden were invited through e-mail to the web-based survey, response rate = 34.5%, total N = 144. 
        The 'Sample Descriptives' page gives an overview of both samples."),
      p(
        "The survey items reported in this dashboard are attiudinal measures towards 52 different research practices, 
        including clear research misconduct (Falsifying data and Plagiarism) and a diverse set of QRPs." ),
      br()
    )
  })
  
  
  output$centered_text_right <- renderUI({
    tagList(
      h2("Links"),
      h4(
        "Article:",
        a(
          "Lindkvist, A. M., Koppel, L., Andersson, D., Västfjäll, D., & Tinghög, G. (2026).
        Is research ethics discipline-specific? A survey of researchers’ and ethics reviewers’ views on research misconduct and questionable practices.
        Research Policy.",
          href = "https://doi.org/10.1016/j.respol.2026.105435",
          target = "_blank"
        )
      ),
      br(),
      br(),
      h4(
        "OSF project page:",
        a(
          "https://osf.io/hw8zf/",
          href = "https://osf.io/hw8zf/",
          target = "_blank"
        )
      ),
      h4(
        "Dashboard Github page:",
        a(
          "https://github.com/AmandaLink/Attitudes_RM-QRPs",
          href = "https://github.com/AmandaLink/Attitudes_RM-QRPs",
          target = "_blank"
        )
      ),
      br(),
      br(),
      h2("Contributors:"),
      h5(
        "Amanda M. Lindkvist (",
        a(
          "Linköping University profile, ",
          href = "https://liu.se/en/employee/amali09",
          target = "_blank"
        ),
        a(
          "ORCID",
          href = "https://orcid.org/0000-0002-3984-5081",
          target = "_blank"
        ),
        ")"
      ),
      h5(
        "Lina Koppel (",
        a(
          "Linköping University profile, ",
          href = "https://liu.se/en/employee/linko72",
          target = "_blank"
        ),
        a(
          "ORCID",
          href = "https://orcid.org/0000-0002-6302-0047",
          target = "_blank"
        ),
        ")"
      ),
      h5(
        "David Andersson (",
        a(
          "Linköping University profile",
          href = "https://liu.se/en/employee/davan75",
          target = "_blank"
        ),
        ")"
      ),
      h5(
        "Daniel Västfjäll (",
        a(
          "Linköping University profile, ",
          href = "https://liu.se/en/employee/danva85",
          target = "_blank"
        ),
        a(
          "ORCID",
          href = "https://orcid.org/0000-0003-2873-4500",
          target = "_blank"
        ),
        ")"
      ),
      h5(
        "Gustav Tinghög (",
        a(
          "Linköping University profile, ",
          href = "https://liu.se/en/employee/gusti22",
          target = "_blank"
        ),
        a(
          "ORCID",
          href = "https://orcid.org/0000-0002-8159-1249",
          target = "_blank"
        ),
        ")"
      ),
      br(),
      br(),
      br(),
      p(" "),
      p(" ")
    )
  })
  
  
  
  ## "Sample descriptives" page -------------------------------------------------------
  
  
  # Define reactive values to store the gt tables
  rv <- reactiveValues(
    Researchers = res_tbl1,
    Researchers_by_field = res_grp_tbl1,
    Ethical_reviewers = epm_tbl1,
    Non_response = nonres_tbl
  )
  
  # Define a reactive expression to select the appropriate gt table based on the input
  selected_table_desc <- reactive({
    
    
    switch(input$filter_desctbl,
           "Researchers" = rv$Researchers,
           "Researchers by field" = rv$Researchers_by_field,
           "Ethics reviewers" = rv$Ethical_reviewers,
           "Researcher response rate" = rv$Non_response)
  })
  
  # Render the gt table based on the selected option
  output$table_sample_desc <- render_gt({
    selected_table_desc()
  })



  output$map_plot <- renderPlotly({
    
    plotly::plot_ly() %>%
      plotly::add_sf(
        data = sweden_map,
        color = I("gray70"),
        stroke = I("grey50"),
        type = "scatter",
        inherit = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip"   # THIS disables "trace 0" on polygon hover
      ) %>%
      plotly::add_markers(
        data = uni_st,
        x = ~lon,
        y = ~lat,
        text = ~hover_text,
        hoverinfo = "text",
        marker = list(
          size = 10,
          color = "steelblue",
          line = list(color = "black", width = 1)
        ),
        showlegend = FALSE
      ) %>%
      plotly::layout(
        geo = list(
          scope = "europe",
          projection = list(type = "mercator"),
          showland = TRUE
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        showlegend = FALSE
      )
    
  })
  
  

  
  
  ## "Researchers' and ethics reviewers' attitudes" page -------------------------------------------
  
  
  
  ## Create data filters 
  filtered_data <- reactive({
    
    filtered <- all
    
    if (input$diff_filter == "No") {
      filtered <- all %>% select(-starts_with("Diff"))
    }
    
    
    if (input$cat_filter != "All categories") {
      filtered <- filtered %>% dplyr::filter(category == input$cat_filter)
    }
    
    if (input$item_filter != "All practices") {
      filtered <- filtered %>% dplyr::filter(practice == input$item_filter)
    }
    
    
    filtered 
  })
  
  
  # Limit practice choices to those in selected category filter
  observe({
    selected_category <- input$cat_filter

    if (selected_category == "All categories") {
      updateSelectInput(session, "item_filter",
                        choices = c("All practices", items))
    } else {
      filtered_choices <- items_cats$practice[items_cats$category == selected_category]
      updateSelectInput(session, "item_filter",
                        choices = c("All practices", unique(filtered_choices)))
    }
  })
  
  observe({
    selected_item <- input$item_filter

    # If "All categories" is not selected when a practice is chosen, set "All categories" in "cat_filter"
    if (selected_item != "All practices") {
      updateSelectInput(session, "cat_filter", selected = "All categories")
    }
  })

  observe({
    selected_cat <- input$cat_filter

    # If "All practices" is not selected when a category is chosen, set "All practices" in "item_filter"
    if (selected_cat != "All categories") {
      updateSelectInput(session, "item_filter", selected = "All practices")
    }
  })
  



  
  ## Render the gt table
  output$table <- render_gt({
    
    # Get the filtered data
    filtered <- filtered_data() %>% select(-category)
    
    gt_table <- gt(filtered) %>% 
      #tab_spanner_delim(delim = "_" ) %>% 
      fmt_number(columns = c(ends_with("M")), decimals = 2)  %>% 
      fmt_number(columns = c(ends_with("SD")), decimals = 2)  %>% 
      cols_merge(c(Researchers_Overall_M, Researchers_Overall_SD), pattern = "{1} ({2})") %>%
      cols_merge(c(Researchers_Medical_M, Researchers_Medical_SD), pattern = "{1} ({2})") %>%
      cols_merge(c(Researchers_Social_M, Researchers_Social_SD), pattern = "{1} ({2})") %>%
      cols_merge(c(Researchers_Natural_M, Researchers_Natural_SD), pattern = "{1} ({2})") %>%
      cols_merge(c(Researchers_Humanities_M, Researchers_Humanities_SD), pattern = "{1} ({2})") %>%
      cols_merge(c(EPM_Overall_M, EPM_Overall_SD), pattern = "{1} ({2})") %>%
      cols_merge(c(EPM_Medical_M, EPM_Medical_SD), pattern = "{1} ({2})") %>%
      cols_merge(c(EPM_Other_M, EPM_Other_SD), pattern = "{1} ({2})") %>%
      cols_label(practice = "Practice") %>% 
      cols_label(
        Researchers_Overall_M = html("Full sample<br>Weighted<br>M (SD)"),
        Researchers_Medical_M = html("Medical<br>M (SD)"),
        Researchers_Social_M = html("Social<br>M (SD)"),
        Researchers_Natural_M = html("Natural<br>M (SD)"),
        Researchers_Humanities_M = html("Humanities<br>M (SD)"),
        EPM_Overall_M = html("Full sample<br>M (SD)"),
        EPM_Medical_M = html("Medical<br>M (SD)"),
        EPM_Other_M = html("Other<br>M (SD)")
      )  
    
      
      # Conditionally format decimals and add labels if difference columns exist
      if ("Difference_Researchers_Means" %in% names(filtered)) {
        gt_table <- gt_table %>%
          fmt_number(columns = c(starts_with("Diff")), decimals = 2) %>% 
          cols_label(Difference_Researchers_Means = html("Largest &#916M<br> Fields")) %>%
          cols_label(Difference_Researchers_SDs = html("Largest &#916SD<br> Fields")) %>%
          cols_label(Difference_EPM_Means = html("&#916M<br> Review fields")) %>%
          cols_label(Difference_EPM_SDs = html("&#916SD<br> Review fields"))
      }
    

      gt_table %>% data_color(
        columns = c(ends_with("_M")),
        palette = "Oranges", 
        reverse = TRUE, 
        dom = c(0, 5)
      ) %>%
      # rm_spanners(levels = 2) %>% 
    opt_interactive(use_compact_mode = TRUE) %>% 
      tab_spanner(
        label = "Ethics Reviewers",
        columns = c(starts_with("EPM"))
      ) %>%
      tab_spanner(
        label = "Researchers",
        columns = c(starts_with("Res"))
      ) %>%
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_column_labels())  %>% 
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_column_spanners()) %>% 
      cols_width(
        c(practice) ~ px(350),  # Set specific columns to a width of 100px
        c(everything()) ~ px(100)     # Other columns get 200px width
      )
    
  })
  

  
  ## "Predictors of researcher attitudes" page  ---------------------------------------------------------
  
  
  
  # Define reactive values for which plot data to use 
  rv2 <- reactiveValues(
    Weighted_Standardized = fits_std,
    Weighted_Unstandardized = fits_unstd,
    Unweighted_Standardized = fits_uw_std,
    Unweighted_Unstandardized = fits_uw_unstd
  )
  
  # Define a reactive expression to select the appropriate gt table based on the input
  selected_reg <- reactive({
    switch(paste(input$filter_regwgt, input$filter_regstd, sep = "_"),
           "Weighted_Standardized" = rv2$Weighted_Standardized,
           "Weighted_Unstandardized" = rv2$Weighted_Unstandardized,
           "Unweighted_Standardized" = rv2$Unweighted_Standardized,
           "Unweighted_Unstandardized" = rv2$Unweighted_Unstandardized
    )
  })
  
  
  output$reg_plot <- renderPlotly({
    
    
    # Create a scatter plot with error bars
    plot <- plot_ly(height = 800) 
    
    plot <- add_trace(plot, data = selected_reg() %>%  dplyr::filter(model == "All practices"), name = "All practices", visible = TRUE,
                      x = ~ estimate, y = ~ brackets, type = 'scatter', mode = 'markers', 
                      error_x = list(type = 'data', array = ~ conf.high - estimate, arrayminus = ~ estimate - conf.low))
    
    plot <- add_trace(plot, data = selected_reg() %>%  dplyr::filter(model != "All practices") ,visible = "legendonly",
                      color = ~ model, x = ~ estimate, y = ~ brackets, type = 'scatter', mode = 'markers',
                      error_x = list(type = 'data', array = ~ conf.high - estimate, arrayminus = ~ estimate - conf.low))
    
    # Customize layout
    plot <- plot %>% layout(
      xaxis = list(title = 'Regression estimate, 95% Confidence Interval', hoverformat = '.2f', range = c(-0.6, 0.6)),
      yaxis = list(title = '', categoryorder = 'array', categoryarray = ~ rev(brackets), hoverformat = '.2f'),
      legend = list(
        title = list(text = '<b> Regression model outcome </b>', font = list(size = 15)),  # Legend title font size
        font = list(size = 14),  # Legend item font size
        orientation = 'h'
      )
    )
    
    
    return(plot)
    
  })
  
  ## "Researcher attitudes by subfield" page -------------------------------
  
  
  ## Create reactive data for table
  
  subfield_dat <- reactive({
    
    # Filter by research area, select dataframe
    subfield_dat <- switch(input$area_filter4,
                           "Medical and Health sciences" = sub_tbl_med,
                           "Natural sciences" = sub_tbl_nat,
                           "Social sciences" = sub_tbl_soc,
                           "Humanities" = sub_tbl_hum)
    
    if (input$item_filter_page4 != "All practices") {
      subfield_dat <- subfield_dat %>% filter(Practice == input$item_filter_page4)
    }
    
    
    if (input$cat_filter_page4 != "All categories") {
      subfield_dat <- subfield_dat %>% filter(QRP_cat == input$cat_filter_page4)
    }
    
    subfield_dat 
  })
  
  #Limit practice choices to those in selected category filter
  observe({
    selected_category <- input$cat_filter_page4
    
    if (selected_category == "All categories") {
      updateSelectInput(session, "item_filter_page4",
                        choices = c("All practices", items))
    }
    else {
      filtered_choices <- items_cats$practice[items_cats$category == selected_category]
      updateSelectInput(session, "item_filter_page4",
                        choices = c("All practices", unique(filtered_choices)))
    }
    
  })
  
  
  output$subfield_tbl <- render_gt({
    
    # Get the filtered data
    subdat <- subfield_dat() %>% select(-QRP_cat)
    
    # Create the table
    gt(subdat) %>%
      tab_spanner_delim(delim = "_" ) %>%
      fmt_number(
        columns = c(everything()),
        decimals = 2
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_column_labels()
      ) %>%
      opt_interactive(use_compact_mode = TRUE) %>% 
      cols_width(
        c(Practice) ~ px(400),  # Set specific columns to a width of 100px
        c(everything()) ~ px(120)     # Other columns get 200px width
      )
    
  }
  
  )
  
  
  ## "Subfield attitude similarity" page ---------------------------------------------------------
  
  output$umap_plot <- renderPlotly({
    
    
    umap_plot <- plot_ly(
      data = umap_data,
      x = ~x,
      y = ~y,
      color = ~area,
      colors = umap_area_colors,
      type = "scatter",
      mode = "markers+text",
      text = ~label,
      textposition = "top center",
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "Respondents: %{marker.size}<extra></extra>"
      ),
      marker = list(
        size = ~n,
        sizemode = "diameter",
        sizeref = 2 * max(umap_data$n) / (10^2),
        opacity = 0.7,
        line = list(width = 0)
      ),
      height = 700
    ) %>%
      layout(
        legend = list(title = list(text = "Field")),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        margin = list(l = 20, r = 20, t = 20, b = 20)
      )
    
    return(umap_plot)
  })
  
  
}

# Run the shiny app -------------------------------------------------------

shinyApp(ui, server)

