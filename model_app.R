library(shiny)
library(shinydashboard)
library(visNetwork)
library(ggplot2)
library(tidyr)
source("sick_simulation.R")
shiny::shinyOptions(error = "browser")

# User-Interface 
ui <- shinydashboard::dashboardPage(#skin = "#003057",
  # setSliderColor(c("#B3A369", "#B3A369","#B3A369","#B3A369"),c(1,2,3,4)),
  
  shinydashboard::dashboardHeader(title = "ISYE 6644: Group 26"),
  shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    shinydashboard::sidebarMenu(
      sidebarMenu(
        menuItem("Infection Network", tabName = "network"),
        menuItem("Infection Timeline", tabName = "areachart"),
        menuItem("Monte Carlos", tabName = "monte_carlos")      )
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
                      max = 31,
                      value = 1,
                      width = "100%",
                      animate = animationOptions(loop = FALSE, interval = 1000)
                    ),
                    visNetworkOutput("network_vis"),
                    actionButton(inputId = "save_data_one", label = "Save Data")
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
      tabItem("monte_carlos",
              sidebarPanel(
                numericInput(
                  inputId = "children_amount",
                  label = "Number of Children",
                  min = 1,
                  max = 1000,
                  value = 31,
                  step = 1),
                
                sliderInput(
                  inputId = "infection_rate",
                  label = "Infection Rate (%)",
                  min = 1,
                  max = 100,
                  value = 2,
                  step = 1)),
              numericInput(
                inputId = "days_amount",
                label = "Number of Days a Student Can Affect Another",
                min = 1,
                max = 1000,
                value = 3,
                step = 1),
                
              sidebarPanel(
                sliderInput(
                  inputId = "immunization_prob",
                  label = "Immunization Probability",
                  min = 1,
                  max = 100,
                  value = 50,
                  step = 1),
                checkboxInput(inputId = "immunization_on",
                              label = "Immunizations?",
                              value = FALSE)),
              sidebarPanel(
                numericInput(inputId = "num_runs",
                               label = "Number of Runs",
                               min = 0,
                               max = 100000,
                               value = 1000),
                actionButton(inputId = "run_monte_carlos",
                             label = "Run Monte Carlos!"),
                ),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                sliderInput(
                  inputId = "bins",
                  label = "Number of Bins",
                  min = 1,
                  max = 50,
                  value = 25))
            
               
                ) ,
              
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  plotOutput("monte_carlo_outputs")
                )
              )
      )
    ),
      tags$head(tags$style(HTML('
      
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #003057;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #003057;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #003057;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #003057;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #A28D5B;
        }
                              
                              /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #B3A369;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #A28D5B;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #B3A369;
         }
                              
      /* change color of sliders */
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
        background: #B3A369;
          border-top: 1px solid #A28D5B ;
        border-bottom: 1px solid #A28D5B ;}
        
        /* changes the colour of the number tags */
          .irs-from, .irs-to, .irs-single { background: #B3A369 }
          
      /* change color of sliders */
      .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
        background: #B3A369;
          border-top: 1px solid #A28D5B ;
        border-bottom: 1px solid #A28D5B ;}
        
        /* changes the colour of the number tags */
          .irs-from, .irs-to, .irs-single { background: #B3A369 }
        
        
        /* change color of sliders */
      .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {
        background: #B3A369;
          border-top: 1px solid #A28D5B ;
        border-bottom: 1px solid #A28D5B ;}
        
        /* changes the colour of the number tags */
          .irs-from, .irs-to, .irs-single { background: #B3A369 }
         
         /* change color of sliders */
      .js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {
        background: #B3A369;
          border-top: 1px solid #A28D5B ;
        border-bottom: 1px solid #A28D5B ;}
        
        /* changes the colour of the number tags */
          .irs-from, .irs-to, .irs-single { background: #B3A369 }                       
                                ')))
    )
    
    
  )


# 
server <- function(input, output, session) {
  
  simulationOutputs <- reactiveVal(value = data.frame())
  
  observeEvent(c(input$infection_rate, 
                 input$children_amount, 
                 input$immunization_on,
                 input$immunization_prob,
                 input$days_amount),{
      simulationOutputs(run_sim(
        rate = input$infection_rate,
        children_number = input$children_amount,
        immunization = input$immunization_on,
        immunization_rate = input$immunization_prob,
        days_spread = input$days_amount
      ))
  })
  
  
  
  monteCarlosData <- reactiveVal(value = data.frame())
  observeEvent(input$run_monte_carlos,{
    all_data <- data.frame()
    for(i in 1:input$num_runs){
      print(i)
      temp <- run_sim(
        rate = input$infection_rate,
        children_number = input$children_amount,
        immunization = input$immunization_on,
        immunization_rate = input$immunization_prob,
        days_spread = input$days_amount
      )
      temp$run <- i
      all_data <- rbind(all_data, temp)
    }
    monteCarlosData(all_data)
    shinyalert::shinyalert("Finished! Your distrobution graph is ready to view",
                           "Monte Carlos Run!", 
                           type = "success")
  })
  
  output$monte_carlo_outputs <- renderPlot({
    df <- monteCarlosData()
    if(nrow(df) == 0){
      return(NULL)
    }
    rounds <- max(df$run)
    df2 <- data.frame(
      "TotalRounds" = numeric(rounds),
      stringsAsFactors = FALSE
    )

    for(i in 1:rounds){
      temp <- df[which(df$run == i),]
      r <- max(temp$day)
      df2$TotalRounds[i] <- r
    }
    
    bins <- input$bins +1
    
    ggplot(df2, aes(x=TotalRounds)) + 
      geom_histogram( color="#A28D5B", fill="#B3A369", bins = bins) +
      # geom_density(alpha=.2, fill="gray") +
      labs(title="Expected Number of Days for Epidemic to Last",
           x ="Number of Runs", y = "Number of Days") +
      theme_classic()
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
    if(nrow(temp) > 0){
      nodes <- getNodes(infected = temp$infected_e)
      edges <- getEdges(df = temp)
      
      output$network_vis <- renderVisNetwork({
        visNetwork(nodes, edges) %>%
          visEdges(arrows = "from") %>%
          visIgraphLayout(layout = "layout_in_circle") %>%
          visGroups(groupname = "Infected", color = "red") %>% 
          visGroups(groupname = "Not Infected", color = "gray") %>%         
          visOptions(highlightNearest = TRUE,
                     selectedBy = "label")  %>%
           visIgraphLayout(layout = "layout_nicely", randomSeed = 1234)
      })
    }
  })
  
  output$area_chart <-renderPlot({
    simulationOutputs() %>% 
      dplyr::group_by(day) %>%
      dplyr::summarize(num_inf = sum(infected_e)) %>%
      ggplot()+geom_area(aes(day, num_inf), fill = "#003057", alpha=0.6) +
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
      geom_area(aes(day, num_inf), fill = "#003057", alpha=0.6) +
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