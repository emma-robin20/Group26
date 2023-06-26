library(shiny)
library(shinydashboard)
library(visNetwork)
library(ggplot2)
library(tidyr)
source("sick_simulation.R")
shiny::shinyOptions(error = "browser")

# User-Interface 
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "ISYE 6644: Group 26"),
  shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    shinydashboard::sidebarMenu(
      sidebarMenu(
        menuItem("Network", tabName = "network"),
        menuItem("Monte Carlos", tabName = "monte_carlos"),
        menuItem("Timeline", tabName = "areachart"),
        menuItem("Settings", tabName = "settings")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("network",
              fluidRow(
                  shinydashboard::box(
                    width = 12,
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    collapsed = FALSE,
                    sliderInput(
                      inputId = "day_slider",
                      label = "Day",
                      min = 1,
                      max = 20,
                      value = 1,
                      width = "100%",
                      animate = animationOptions(loop = FALSE, interval = 1000)
                    ),
                    visNetworkOutput("network_vis"),
                    actionButton(inputId = "save_data_one", label = "Save Data")
                  )
              )
      ),
      tabItem("monte_carlos",
              fluidRow(
                shinydashboard::box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  numericInput(inputId = "num_runs",
                               label = "Number of Runs",
                               min = 0,
                               max = 100000,
                               value = 1000),
                ),
                shinydashboard::box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  actionButton(inputId = "run_monte_carlos",
                               label = "Run Monte Carlos!")
                ),
                shinydashboard::box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  actionButton(inputId = "save_data",
                               label = "Save Data")
                )
              )
      ),
      tabItem("areachart",
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  collapsed = FALSE,
                  # sliderInput(
                  #   inputId = "day_slider",
                  #   label = "Day",
                  #   min = 1,
                  #   max = 20,
                  #   value = 1,
                  #   width = "100%",
                  #   animate = animationOptions(loop = FALSE, interval = 1000)
                  # ),
                  plotOutput("area_chart"),
                  plotOutput("hist_chart")
                  # actionButton(inputId = "save_data_one", label = "Save Data")
                )
              )
      ),
      tabItem("settings",
              fluidRow(
                shinydashboard::box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  sliderInput(
                    inputId = "infection_rate",
                    label = "Infection Rate (%)",
                    min = 1,
                    max = 100,
                    value = 2,
                    step = 1)
                ),
                shinydashboard::box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  numericInput(
                    inputId = "children_amount",
                    label = "Number of Children",
                    min = 1,
                    max = 1000,
                    value = 31,
                    step = 1)
                )
              ),
              fluidRow(
                shinydashboard::box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  checkboxInput(inputId = "immunization_on",
                              label = "Immunizations?",
                              value = FALSE)
                ),
                shinydashboard::box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  sliderInput(
                    inputId = "immunization_prob",
                    label = "Immunization Probability",
                    min = 1,
                    max = 100,
                    value = 50,
                    step = 1)
                )
              )
      )
    )
  )
)

# 
server <- function(input, output, session) {
  
  simulationOutputs <- reactiveVal(value = data.frame())
  
  observeEvent(c(input$infection_rate, 
                 input$children_amount, 
                 input$immunization_on,
                 input$immunization_prob),{
      simulationOutputs(run_sim(
        rate = input$infection_rate,
        children_number = input$children_amount,
        immunization = input$immunization_on,
        immunization_rate = input$immunization_prob
      ))
  })
  
  
  
  monteCarlosData <- reactiveVal(value = data.frame())
  observeEvent(input$run_monte_carlos,{
    all_data <- data.frame()
    for(i in 1:input$num_runs){
      print(i)
      temp <- simulationOutputs(run_sim(
        rate = input$infection_rate,
        children_number = input$children_amount,
        immunization = input$immunization_on,
        immunization_rate = input$immunization_prob
      ))
      all_data <- rbind(all_data, temp)
    }
    monteCarlosData(all_data)
    shinyalert::shinyalert("Finished!",
                           "Monte Carlos Run!", 
                           type = "success")
  })
  
  observeEvent(input$map_slider, {
    updateSliderInput(
      session, 'map_slider',
      value = input$map_slider+1,
      min = 1,
      max = simulationOutputs()$day
    )
  })
  
  update_net_vis <- reactive({
    req(nrow(simulationOutputs()) > 0)
    req(input$day_slider)
    out <- simulationOutputs()[which(
      simulationOutputs()$day == input$day_slider),]
    return(out)
  })
  
  observeEvent(update_net_vis(), {
    temp <- update_net_vis()
    # browser()
    if(nrow(temp) > 0){
      nodes <- getNodes(infected = temp$infected_e)
      edges <- getEdges(df = temp)
      
      output$network_vis <- renderVisNetwork({
        visNetwork(nodes, edges) %>%
          visEdges(arrows = "from") %>%
          visGroups(groupname = "Infected", color = "red") %>% 
          visGroups(groupname = "Not Infected", color = "gray") %>%         
          visOptions(highlightNearest = TRUE,
                     selectedBy = "label")  %>%
           # visOptions(manipulation = TRUE) %>%
           visIgraphLayout(layout = "layout_nicely", randomSeed = 1234)
      })
    }
  })
  
  output$area_chart <-renderPlot({
    simulationOutputs() %>% 
      dplyr::group_by(day) %>%
      dplyr::summarize(num_inf = sum(infected_e)) %>%
      ggplot()+geom_area(aes(day, num_inf), fill = "red", alpha=0.6) +
      labs(title="Total Students Infected with Flu Over Time",
           x ="Days", y = "Number of Students Infected")+
      theme_classic()
  })
  
  output$hist_chart <-renderPlot({
    simulationOutputs() %>% 
      dplyr::filter(simulationOutputs()$day == simulationOutputs()$day_infected) %>%
      dplyr::group_by(day) %>%
      dplyr::summarize(num_inf = sum(infected_e)) %>%
      ggplot()+
      geom_area(aes(day, num_inf), fill = "red", alpha=0.6) +
      labs(title="Count of Students Infected with Flu Per Day",
           x ="Days", y = "Count of Students Infected") +
      theme_classic()
  })
  
  
  observeEvent(input$save_data_one,{
    write.csv(
      simulationOutputs(),
      "data/One Run.csv"
    )
  })
  
  observeEvent(input$save_data,{
    write.csv(
      monteCarlosData(),
      "data/Monte Carlos.csv"
    )
  })
}

shinyApp(ui, server)