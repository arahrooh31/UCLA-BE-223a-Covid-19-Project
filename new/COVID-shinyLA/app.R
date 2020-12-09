#
# This is a Shiny web application for COVID prediction on the community/individual level. 
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(warn = -1)
# load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(evaluate)) install.packages("evaluate", repos = "http://cran.us.r-project.org")	

# Step 1: Pull and clean useful COVID data -- data-preprocessing
# source("~/Downloads/new/code/community_wide_clean.R")
# Step 2: Source in functions and settings used in app building
source("~/Downloads/new/code/app_functions.R")
source("~/Downloads/new/code/app_settings.R")


#========================
#     APP FUNCTIONS     #
#========================

# Define UI for application that draws a histogram
ui = fluidPage(
  
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVID-19 in LA county", id = "nav",
             
             tabPanel("COVID-19 Mapper",
                      div(class = "outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width = "100%", height = "100%"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        bottom = 30, left = 55, width = 380, fixed = TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(tags$i(h3("Summary data in LA county")), style = "color:#045a8d"),
                                        h5(textOutput("clean_date_reactive"), align = "left"),
                                        h3(textOutput("total_case_count"), align = "right"),
                                        h4(textOutput("total_death_count"), align = "right"),
                                        h4(textOutput("reactive_community_count"), align = "right"),
                                        plotOutput("epi_curve", height = "180px", width = "100%"),
                                        plotOutput("cumulative_plot", height = "180px", width = "100%"),
                                        fluidRow(
                                          align = "center",
                                          radioButtons("plot_content", inline = T, label = "",
                                                       choiceNames = list("Cases", "Deaths"), choiceValues = list("cases", "deaths"))
                                        ),
                                        fluidRow(
                                          align = "center",
                                          sliderInput("plot_date", 
                                                      label = NULL,
                                                      min = as.Date(cv_min_date, "%Y-%m-%d"),
                                                      max = as.Date(current_date, "%Y-%m-%d"),
                                                      value = as.Date(current_date),
                                                      timeFormat = "%d %b", 
                                                      animate = animationOptions(interval = 3000, loop = FALSE))
                                        ),
                                        span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between areas")), style = "color:#045a8d")
                          )
                      )
             ),
             
             tabPanel("Region Details",
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(
                            'input.dataset == "COVID-19 Cases"',
                            h6("To update: please select places(s) from drop-down menues. Cities/Communities with at least 1 confirmed cases are included."),
                            pickerInput("region_select", "City/Community:",
                                        choices = as.character(community_today$place_ID),
                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                        selected = as.character(community_today$place_ID)[1:5],
                                        multiple = TRUE),
                            h5("Selected city/community:"),
                            verbatimTextOutput("community_selected"),
                            span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions. Reported cases are subject to significant variation in testing policy and capacity between regions/areas.")), style = "color:#045a8d")
                          ),
                          conditionalPanel(
                            'input.dataset == "Demographics"',
                            h5("Select city/community:"),
                            pickerInput("region_select_2", "City/Community:",
                                        choices = as.character(community_demographic_short$COMMUNITY),
                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                        selected = as.character(community_demographic_short$COMMUNITY)[1:10],
                                        multiple = TRUE),
                            checkboxGroupInput("show_vars", "Columns to show:", names(community_demographic_short), selected = names(community_demographic_short)[1:7])
                          )
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            id = 'dataset',
                            tabPanel("COVID-19 Cases", 
                                     fluidRow(
                                       align = "center",
                                       radioButtons("plot_content_region", inline = T, label = "",
                                                    choiceNames = list("Cumulative cases (over time)", "New cases (over time)", "New cases (7-day moving average)"), choiceValues = list("cumulative", "new", "new_7MA"))
                                     ),
                                     plotlyOutput("community_cases_plot"),
                                     fluidRow(
                                       align = "center",
                                       sliderInput("plot_date_end", 
                                                   label = NULL,
                                                   min = as.Date(cv_min_date, "%Y-%m-%d"),
                                                   max = as.Date(current_date, "%Y-%m-%d"),
                                                   value = as.Date(current_date),
                                                   width = "80%",
                                                   timeFormat = "%d %b", 
                                                   animate = animationOptions(interval = 3000, loop = FALSE))
                                     ),
                                     downloadButton("downloadCOVID", "Download time-series COVID-19 data by community")
                            ),
                            tabPanel("Demographics", 
                                     DT::dataTableOutput("community_demographic_short"),
                                     # Download Button
                                     downloadButton("downloadDemo", "Download raw deomographic data")
                                     )
                          )
                        )
                      )
             ),
             
             tabPanel("Risk modeling",
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(
                            'input.predict == "Community Risk"',
                            h6("To update: please select places(s) from drop-down menues."),
                            pickerInput("region_select_riskCommunity", "City/Community:",
                                        choices = as.character(community_risk_combined$Community),
                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                        selected = as.character(community_risk_combined$Community)[1:10],
                                        multiple = TRUE),
                            h5("Selected city/community:"),
                            verbatimTextOutput("community_selected_risk")
                            ),
                          conditionalPanel(
                            'input.predict == "Individual Risk"',
                            h5("Select city/community:"),
                            pickerInput("region_select_riskIndividual", "City/Community:",
                                        choices = as.character(community_risk_combined$Community),
                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                        selected = as.character(save_place_ID_and_outbreak_risk$community)[1],
                                        multiple = FALSE),
                            pickerInput("age_select_riskIndividual", "Age:",
                                        choices = c("age_18_less", "age_19_to_49", "age_50_to_64", "age_65_up"),
                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                        selected = "age_18_less",
                                        multiple = FALSE),
                            pickerInput("race_select_riskIndividual", "Race:",
                                        choices = c("Asian", "Black", "Latino", "White", "Other"),
                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                        selected = "Asian",
                                        multiple = FALSE),
                            checkboxInput("button", "Show community predicted-risk map")
                          ),
                          conditionalPanel(
                            'input.predict == "About the model"'
                            
                          )
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            id = 'predict',
                            tabPanel("Community Risk",
                                     DT::dataTableOutput("community_risk_pred")
                            ),
                            tabPanel("Individual Risk", 
                                     h3(textOutput("individual_risk_community"), align = "middle"),
                                     h3(textOutput("individual_risk"), align = "middle"),
                                     leafletOutput("community_risk_map")
                            ),
                            tabPanel("About the model"
                                     
                            )
                            
                          )
                        )
                      )

             ),

             tabPanel("About this site"
             )
  )          
)

# Define server logic required to draw a histogram
server = function(input, output, session) {
  
  #==========================
  # COVID-19 mapper tab 
  # Reactive databases
  reactive_db_LA = reactive({
    LA_county %>% filter(Date == input$plot_date)
  })
  reactive_db_community = reactive({
    community_today %>% filter(geo_merge %in% community_location$label)
  })
  reactive_db_community_newToday = reactive({
    cv_cases_longitudinal %>% filter(date == input$plot_date)
  })
  reactive_polygons = reactive({
    community_location[community_location$label %in% community_today$geo_merge, ]
  })
  
  # Outputs
  output$clean_date_reactive = renderText({
    format(input$plot_date,"%d %B %Y")
  })
  output$total_case_count = renderText({
    paste0(prettyNum(reactive_db_LA()$TotalCases, big.mark = ","), " cases")
  })
  output$total_death_count = renderText({
    paste0(prettyNum(reactive_db_LA()$TotalDeaths, big.mark = ","), " deaths")
  })
  output$reactive_community_count = renderText({
    paste0(length(reactive_db_community_newToday()$place_ID), " citis/communities affected")
  })
  output$cumulative_plot = renderPlot({
    cumulative_plot(LA_county, input$plot_date, as.character(input$plot_content))
  })
  output$epi_curve = renderPlot({
    new_cases_plot(LA_county, input$plot_date, as.character(input$plot_content))
  })
  
  # Map function
  output$mymap = renderLeaflet({
    basemap
  })
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_community()$cumulative_cases_today)) %>%
      addCircleMarkers(data = reactive_db_community(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cumulative_cases_today)^(1/3),
                       fillOpacity = 0.1, color = county_col[1], 
                       label = sprintf("<strong>%s (cumulative till TODAY)</strong><br/>Confirmed cases: %s<br/>Deaths: %s<br/>Person tested: %s<br/>Adjusted positive testing rate: %s", reactive_db_community()$geo_merge, prettyNum(reactive_db_community()$cumulative_cases_today, big.mark = ","), prettyNum(reactive_db_community()$cumulative_deaths_today, big.mark = ","), prettyNum(reactive_db_community()$cumulative_persons_tested_today, big.mark = ","), prettyNum(reactive_db_community()$adj_test_pos_rate, big.mark = ",")) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = county_col[1]),
                         textsize = "15px", direction = "auto"))
  })
  
  #==========================
  # Region info tab
  # Reactive databases
  # create dataframe with selected communities
  community_reactive_db = reactive({
    cv_cases_longitudinal %>% filter(place_ID %in% as.character(input$region_select)) %>% filter(date <= input$plot_date_end)
  })
  community_demo_db = reactive({
    community_demographic_short %>% filter(COMMUNITY %in% as.character(input$region_select_2) )
  })
  # Outputs
  # country-specific plots
  output$community_cases_plot = renderPlotly({
    community_cases_plot(community_reactive_db(), plot_content = input$plot_content_region, input$plot_date_end)
  })
  output$community_selected = renderText({
    paste(input$region_select, collapse = '\n')
  })
  output$downloadCOVID = downloadHandler(
    filename = function() {
      "COVID_TimeSeries_communityLA.csv"
    },
    content = function(file) {
      write.csv(cv_cases_longitudinal, file, row.names = FALSE)
    }
  )
  output$community_demographic_short = DT::renderDataTable({
    DT::datatable(community_demo_db()[, input$show_vars, drop = FALSE])
  })
  output$downloadDemo = downloadHandler(
    filename = function() {
      "demographic_communityLA.csv"
    },
    content = function(file) {
      write.csv(community_demographic, file, row.names = FALSE)
    }
  )
  
  #==========================
  # Risk modeling tab
  # Reactive databases
  # create dataframe with selected communities  
  community_risk_db = reactive({
    community_risk_combined %>% filter(Community %in% as.character(input$region_select_riskCommunity) )
  })
  reactive_polygons_risk = reactive({
    community_location[community_location$label %in% community_risk_to_plot$geo_merge, ]
  })
  community_riskPlot_db = reactive({
    community_risk_to_plot %>% filter(geo_merge %in% community_location$label )
  })
  # Results for individual predictions
  from_BBN = build_individual_BBN()
  individual_results_db = reactive({
    make_prediction_on_one_person(from_BBN[[1]], from_BBN[[2]],input$region_select_riskIndividual, 
                                  input$age_select_riskIndividual, 
                                  input$race_select_riskIndividual)
  })
    
  # Outputs
  # country-specific plots  
  output$community_selected_risk = renderText({
    paste(input$region_select_riskCommunity, collapse = '\n')
  })
  output$community_risk_pred = DT::renderDataTable({
    DT::datatable(community_risk_db())
  })
  output$individual_risk_community = renderText({
    paste0("The community's prediction of outbreak in one week is: ", individual_results_db()[[1]])
  })
  output$individual_risk = renderText({
    paste0("The individual's risk of getting infected in one week under conditions selected is: ", as.character(round(individual_results_db()[[2]],4)*100), "%")
  })

  # Map function
  output$community_risk_map = renderLeaflet({
    riskmap
  })
  observeEvent(input$button, {
    leafletProxy("community_risk_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons_risk(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal_risk(community_riskPlot_db()$pred_prob)) %>%
      addCircleMarkers(data = community_riskPlot_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(pred_prob)*(2.5)^3,
                       fillOpacity = 0.1, color = "steelblue",
                       label = sprintf("<strong>%s (Prediction as of TODAY)</strong><br/>Outbreak risk in 1 week: %s", community_riskPlot_db()$geo_merge, prettyNum(community_riskPlot_db()$pred_prob, digits = 2)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "steelblue"),
                         textsize = "15px", direction = "auto"))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

