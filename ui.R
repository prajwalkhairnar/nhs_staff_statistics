#
# A Shiny web application to visualize NHS Workforce Statistics. February 2023
#
# Author: Prajwal Khairnar
# 
# Data Source: https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/february-2023
#    
#



data_staff = read.csv(file = 'data/NHS Workforce Statistics, February 2023 staff excluding medical.csv')

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




dashboardPage(
  
  dashboardHeader(title = "NHS Workforce Statistics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Staff (Excluding Medical)", 
               tabName = "staff_exc_med", 
               icon = icon("fa-sharp fa-light fa-users", lib = "font-awesome"))
    ),
    sidebarMenuOutput("help_text"),
    htmlOutput("data_source")
  ),
  dashboardBody(
    
    
    tabItems(
      tabItem(
        tabName = "staff_exc_med",
        
        tabsetPanel(type = "tabs",
                    tabPanel("Summary", 
                             
                             htmlOutput("valuebox_help_text"),
                             
                             fluidRow(
                               
                               valueBoxOutput("valueBox_Region", width = 3),
                               bsTooltip(id = "valueBox_Region", 
                                         title = regions_tooltip, 
                                         placement = "bottom"),
                               
                               
                               valueBoxOutput("valueBox_ICS", width = 3),
                               bsTooltip(id = "valueBox_ICS", 
                                         title = ics_tooltip,
                                         placement = "bottom"),
                               
                               
                               valueBoxOutput("valueBox_Org", width = 3)
                               
                             ),
                             
                             fluidRow(
                               
                               valueBoxOutput("valueBox_Main_Staff_Group", width = 3),
                               bsTooltip(id = "valueBox_Main_Staff_Group", 
                                         title = main_staff_group_tooltip,
                                         placement = "bottom"),
                               
                               
                               valueBoxOutput("valueBox_Staff_Group_1", width = 3),
                               bsTooltip(id = "valueBox_Staff_Group_1", 
                                         title = staff_group1_tooltip,
                                         placement = "bottom"),
                               
                               
                               valueBoxOutput("valueBox_Staff_Group_2", width = 3),
                               bsTooltip(id = "valueBox_Staff_Group_2", 
                                         title = staff_group2_tooltip,
                                         placement = "bottom"),
                               
                               
                               valueBoxOutput("valueBox_Care_Setting", width = 3),
                               bsTooltip(id = "valueBox_Care_Setting", 
                                         title = care_setting_tooltip,
                                         placement = "bottom"),
                               
                               
                               valueBoxOutput("valueBox_Level", width = 3),
                               bsTooltip(id = "valueBox_Level", 
                                         title = level_tooltip,
                                         placement = "bottom")
                               
                             )
                             
                             
                             
                             
                             
                    ),
                    tabPanel("Plots", 
                             
                             
                             
                             
                             
                             
                             verbatimTextOutput("Plot"),
                             
                             
                             
                             
                             titlePanel(" "),
                             fluidRow(
                               
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   
                                   
                                   selectInput("region", HTML("Select Region<br/> (defaults to all regions)"),
                                               choices = unique(data_staff$NHSE_Region_Name),
                                               multiple = TRUE)
                                   
                                   
                                 ),
                                 mainPanel(
                                   
                                   column(
                                     width = 12,
                                     plotlyOutput("plot")
                                     
                                   )
                                   
                                   
                                 ),
                                 
                               ),
                               
                               
                               
                               
                             ),
                             fluidRow(
                               
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   
                                   
                                   selectInput("filter", HTML("Select Region<br/> (defaults to all regions)"),
                                               choices = c("", unique(data_staff$NHSE_Region_Name)),
                                               selected = NULL
                                               # multiple = TRUE
                                   )
                                   
                                   
                                 ),
                                 mainPanel(
                                   
                                   column(
                                     width = 12,
                                     plotlyOutput("org_plot")
                                     
                                   ),
                                   
                                   
                                   
                                 ),
                                 
                               ),
                               
                               
                               
                               
                             )
                             
                             
                             
                    ),
                    tabPanel("Region Network", 
                             
                             
                             htmlOutput("staff_network_text"),
                             
                             visNetworkOutput("staff_network_graph", height = "600px")
                    )
                    
        )
        
        
      )  # End Tab item
      
      
      
      
      
      
    ) # End Tab ITEMS
    
    
  ) # End Dashboard Body
  
) # End Dashboard Page

