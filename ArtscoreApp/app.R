#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)

Habitattypernes_scoringer <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                                          sheet = "bilag 1 artsscorer") %>% 
    clean_names() %>% 
    dplyr::mutate(dansk_navn = str_remove_all(dansk_navn, "\\r\\n"), 
                  videnskabeligt_navn = str_remove_all(videnskabeligt_navn, "\\r\\n")) %>% 
    pivot_longer(stenstrand_12:skov_91, names_to = "Habitat")
    

Habitat_types <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx", 
                            sheet = "middelscorer og gnsn artsantal") %>% 
    clean_names() %>% 
    drop_na %>% dplyr::mutate(x2 = str_remove_all(x2, "\\*"), 
                              x2 = str_remove_all(x2, "\\)"), 
                              x2 = str_remove_all(x2, "\\(")) %>% 
    rename(habitat_name = x2) %>% 
    mutate(Code = case_when(habitatnaturtype %in% c("1330", "1340") ~ "strandeng_13",
                            habitatnaturtype %in% c("2130", "2140", "2180", "2190", "2250", "2310", 
                                                    "2320", "2330") ~ "klitter_21",
                            habitatnaturtype %in% c("4010", "4030") ~ "hede_40",
                            habitatnaturtype %in% c("6120", "6210", "6230") ~ "overdrev_62",
                            habitatnaturtype %in% c("6410") ~ "ferskeng_64",
                            habitatnaturtype %in% c("7110", "7120", "7140", "7150") ~ "hoj_mose_71",
                            habitatnaturtype %in% c("7210", "7220", "7230") ~ "lav_mose_72",
                            habitatnaturtype %in% c("9110", "9120", "9130", "9150", "9160", "9170", "9190", "91D0", 
                                                    "91E0") ~ "skov_91")) %>% 
    relocate(Code, .after = habitatnaturtype)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Artscore calculation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # 1 species
            
            selectInput("Names", label = h3("Input scientific or common names"), 
                        choices = list("Scientific names" = 1, "Common names" = 2), 
                        selected = 1),
            uiOutput("SelectSpecies"),
            # 2 habitats
            selectInput("Habs", label = h3("Input habitat code or name"), 
                        choices = list("Habitat code" = 1, "Habitat name" = 2), 
                        selected = 1),
            uiOutput("SelectHab")
            
        ),

        # 3 Filtered
        mainPanel(
           dataTableOutput("SpeciesScores")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    get_choice <- reactive({input$Names})
    
    output$SelectSpecies <- renderUI({
        if(get_choice() == 1){
            selectizeInput(inputId = "SpeciesListSc",
                           label = "Select all species",
                           multiple = TRUE,
                           choices = unique(Habitattypernes_scoringer$videnskabeligt_navn) %>%  
                               sort())   
        }
        else if(get_choice() == 2){
            selectizeInput(inputId = "SpeciesListCom",
                           label = "Select all species",
                           multiple = TRUE,
                           choices = unique(Habitattypernes_scoringer$dansk_navn) %>% sort())   
        }
    })
    # 1 species
    
    Habs <- reactive({input$Habs})

    output$SelectHab <- renderUI({
        if(Habs() == 1){
         selectizeInput(inputId = "HabsCode",
                     label = "Select your habitat",
                     multiple = FALSE,
                     choices = sort(unique(Habitat_types$habitatnaturtype)))   
        }
        else if(Habs() == 2){
            selectizeInput(inputId = "HabsName",
                           label = "Select your habitat",
                           multiple = FALSE,
                           choices = sort(unique(Habitat_types$habitat_name)))   
        }
    })
    
## 3 Filtered
    
    output$SpeciesScores <- renderDataTable({
        if(get_choice() == 1){
            req(input$SpeciesListSc)
            Results <- Habitattypernes_scoringer %>% 
                dplyr::filter(videnskabeligt_navn %in% input$SpeciesListSc)
        } else if(get_choice() == 2){
            req(input$SpeciesListCom)
            Results <- Habitattypernes_scoringer %>% 
                dplyr::filter(dansk_navn %in% input$SpeciesListCom)
        }
        
        if(Habs() == 1){
            req(input$HabsCode)
            SelectHabitat <- Habitat_types %>% 
                dplyr::filter(habitatnaturtype %in% input$HabsCode) %>% 
                pull(Code)
        }
        else if(Habs() == 2){
            req(input$HabsName)
            SelectHabitat <- Habitat_types %>% 
                dplyr::filter(habitat_name %in% input$HabsName) %>% 
                pull(Code)   
        }
        Results <- Results %>% 
            dplyr::filter(Habitat == SelectHabitat)
        Results
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
