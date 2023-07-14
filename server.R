#
# A Shiny web application to visualize NHS Workforce Statistics. February 2023
#
# Author: Prajwal Khairnar
# 
# Data Source: https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/february-2023
#    
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(fontawesome)
library(dplyr)
library(plotly)
library(magrittr)
library(igraph)
library(visNetwork)



data_staff = read.csv(file = 'D:/Work/Projects/NHS Workforce/data/NHS Workforce Statistics, February 2023 staff excluding medical.csv')

data_staff_network <- data_staff %>%
  select(c(NHSE_Region_Name, ICS.name, Org.name)) %>%
  group_by(NHSE_Region_Name, ICS.name) %>%
  distinct() %>%
  summarise(Organisation_count = n()) %>%
  set_colnames(c("Region", "ICS", "Organisations"))

data_staff_org = data_staff_network %>%
  group_by(Region) %>%
  summarise("Organisations" = sum(Organisations))




regions_tooltip = paste(unique(data_staff$NHSE_Region_Name), collapse = ", ")
ics_tooltip = paste(unique(data_staff$ICS.name), collapse = ", ")

main_staff_group_tooltip = lapply(unique(data_staff$Main.Staff.Group), function(x) substring(x, first = 5))
main_staff_group_tooltip = paste(main_staff_group_tooltip, collapse = ", ")

staff_group1_tooltip = lapply(unique(data_staff$Staff.Group.1), function(x) substring(x, first = 5))
staff_group1_tooltip = paste(staff_group1_tooltip, collapse = ", ")

staff_group2_tooltip = lapply(unique(data_staff$Staff.Group.2), function(x) substring(x, first = 5))
staff_group2_tooltip = paste(staff_group2_tooltip, collapse = ", ")
staff_group2_tooltip = gsub("'","",staff_group2_tooltip)

care_setting_tooltip = lapply(unique(data_staff$Care.Setting), function(x) substring(x, first = 5))
care_setting_tooltip = paste(care_setting_tooltip, collapse = ", ")

level_tooltip = lapply(unique(data_staff$Level), function(x) substring(x, first = 5))
level_tooltip = paste(level_tooltip, collapse = ", ")
level_tooltip = gsub("'","",level_tooltip)



# Define server
shinyServer(function(input, output, session) {
  
  
  output$Plot <- renderText({input$region})
  output$Summary <- renderText({"Summary Tab"})
  output$Network <- renderText({"Network Tab"})
  
  
  output$help_text <- renderMenu({
    
    # Invalidate (and re-run) this code once every second
    # invalidateLater(1000)
    
    # sidebarMenu(
    #   menuItem(Sys.time())
    # )
    
    
    
    helpText("Note: Data has been last updated in February 2023")
    # tags$a(href="www.rstudio.com", "Click here!")
    # HTML("ABCD<br><a href = 'www.google.com'>google<a>")
    
    
  })
  
  
  output$data_source = renderUI({ 
    HTML(paste0('<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>&emsp;&emsp;&ensp;
                <a href = "https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/february-2023">Data Source: NHS Digital </a>'))
  })
  
  
  output$valuebox_help_text = renderUI({ 
    HTML(paste0('<br>Please hover over the boxes to see additional information.<br><br>'))
  })
  
  output$valueBox_Region <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$NHSE_Region_Name)), 
             subtitle = "Regions",
             color = "navy",
             icon = icon("fa-solid fa-map", style="color: #5475a0; opacity:0.5;", lib = 'font-awesome'))
  })
  
  # Invalid color: Orange. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  
  output$valueBox_ICS <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$ICS.name)), 
             subtitle = "ICS", 
             color = "navy",
             icon = icon("fa-solid fa-building", style="color: #5475a0; opacity:0.5;", lib = 'font-awesome'))
  })
  
  
  output$valueBox_Org <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$Org.name)), 
             subtitle = "Organisations", 
             color = "navy",
             icon = icon("fa-regular fa-hospital", style="color: #5475a0; opacity:0.5;", lib = 'font-awesome'))
  })
  
  
  output$valueBox_Main_Staff_Group <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$Main.Staff.Group)), 
             subtitle = "Main Staff Groups", 
             color = "yellow",
             icon = icon("fa-sharp fa-light fa-users", lib = "font-awesome"))
  })
  
  output$valueBox_Staff_Group_1 <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$Staff.Group.1)), 
             subtitle = "Staff Groups 1", 
             color = "yellow",
             icon = icon("fa-sharp fa-light fa-user", lib = "font-awesome"))
  })
  
  output$valueBox_Staff_Group_2 <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$Staff.Group.2)), 
             subtitle = "Staff Groups 2", 
             color = "yellow",
             icon = icon("fa-sharp fa-light fa-user", lib = "font-awesome"))
  })
  
  
  output$valueBox_Care_Setting <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$Care.Setting)), 
             subtitle = "Care Settings", 
             color = "yellow",
             icon = icon("gears"))
  })
  
  
  output$valueBox_Level <- renderValueBox({
    
    
    valueBox(value = length(unique(data_staff$Level)), 
             subtitle = "Levels", 
             color = "yellow",
             icon = icon("fa-solid fa-sitemap", lib = "font-awesome"))
  })
  
  
  
  
  # Reactive function to generate plot based on inputs
  output$plot <- renderPlotly({
    
    data_staff_plot <- reactive({
      
      if(is.null(input$region)){
        
        data_staff
        
      } else {
        
        data_staff %>%
          filter(NHSE_Region_Name %in% input$region)
      }
      
      
    })
    
    
    
    p1 <- ggplot(data_staff_plot(), aes(x = AfC.Band, fill = AfC.Band)) +
      geom_bar( position = "dodge") +
      labs(x = "AFC Band", y = "Count") +
      ggtitle("Countplot of AFC Band") +
      theme_minimal()  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.title = element_blank(),
            legend.position = "bottom")
    
    ggplotly(p1) %>%
      layout(legend = list(orientation = "h", y = -1))
    
  })
  
  
  
  output$org_plot <- renderPlotly({
    
    
    data_staff_org_plot <- reactive({
      
      if(input$filter == ""){
        
        data_staff_network %>%
          group_by(Region) %>%
          summarise("Organisations" = sum(Organisations))
        
      } else {
        
        data_staff_network %>%
          filter(Region == input$filter) %>%
          group_by(ICS) %>%
          summarise("Organisations" = sum(Organisations))
      }
      
    })
    
    
    
    org_plot = ggplot(data_staff_org_plot(), aes(x = get(names(data_staff_org_plot())[1]), y = Organisations)) +
      geom_bar(stat = "identity") +
      labs(x = "Region/ICS", y = "Number of Organizations") +
      ggtitle("Number of Organizations in Each Region") + 
      theme_minimal()  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.title = element_blank(),
            legend.position = "bottom")
    
    ggplotly(org_plot) %>%
      layout(legend = list(orientation = "h", y = -1))
    
  })
  
  
  output$staff_network_text = renderUI({ 
    HTML(paste0('<br>Please zoom in/out to see the detailed network graph.<br>Regions - ICS - Number of organisations.'))
  })
  
  
  staff_network_graph <- reactive({
    
    
    graph <- graph_from_data_frame(data_staff_network[, c("Region", "ICS")], directed = FALSE)
    
    # Set node colors and sizes based on the number of organisations
    node_colors <- ifelse(V(graph)$name %in% data_staff_network$ICS, "lightblue", "lightgray")
    node_sizes = c()
    for (name in V(graph)$name){
      if(name %in% data_staff_network$ICS){
        node_sizes = c(node_sizes,data_staff_network$Organisations[data_staff_network$ICS == name]*3)
      }
      else{
        node_sizes = c(node_sizes,10)
      }
    }
    
    node_labels = c()
    for(i in 1:length(V(graph)$name)){
      if(i > 7){
        node_labels = c(node_labels, paste(V(graph)$name[i], "(", node_sizes[i]/3 , ")" , sep = ""))
      }
      else{
        node_labels = c(node_labels, paste(V(graph)$name[i]))
      }
    }
    
    
    
    # Create a visNetwork object
    visGraph <- visNetwork(nodes = data.frame(id = V(graph)$name, color = node_colors, size = node_sizes, label = node_labels),  
                           edges = get.data.frame(graph), 
                           width = "100%", 
                           height = "500px")
    
    # Customize the visual properties of the network
    visGraph <- visIgraphLayout(visGraph, layout = "layout_with_fr")
    visGraph <- visNodes(visGraph, 
                         shape = "dot", 
                         font = list(face = "Arial", size = 14, color = "black", align = "center"), 
                         borderWidth = 2, 
                         shadow = TRUE)
    
    visGraph <- visEdges(visGraph, width = 2, shadow = TRUE, color = list(color = "gray"))
    # Return the visNetwork object
    visGraph
    
    
    
  })
  
  # Render the network graph in the output container
  output$staff_network_graph <- renderVisNetwork({
    staff_network_graph()
  })
  
  # custom_palette <- colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))(14)
  
  
  
  
  
})
