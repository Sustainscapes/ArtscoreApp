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
                                        sheet = "bilag 1 artsscorer") %>% clean_names()


Species_Latin <- unique(Habitattypernes_scoringer$videnskabeligt_navn) %>%  sort()

Species_Danish <- unique(Habitattypernes_scoringer$dansk_navn) %>% sort()

Habitat_types <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx", 
                            sheet = "middelscorer og gnsn artsantal") %>% 
    clean_names() %>% 
    drop_na %>% dplyr::mutate(x2 = str_remove_all(x2, "\\*"), 
                              x2 = str_remove_all(x2, "\\)"), 
                              x2 = str_remove_all(x2, "\\(")) %>% 
    rename(habitat_name = x2)

Habitat_codes <- Habitat_types$habitatnaturtype %>% 
    unique() %>% 
    sort()

Habitat_names <- Habitat_types$habitat_name %>% 
    unique() %>% 
    sort()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Artscore calculation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Names", label = h3("Input scientific or common names"), 
                        choices = list("Scientific names" = 1, "Common names" = 2), 
                        selected = 1),
            uiOutput("SelectSpecies"),
            selectInput("Habs", label = h3("Input habitat code or name"), 
                        choices = list("Habitat code" = 1, "Habitat name" = 2), 
                        selected = 1),
            uiOutput("SelectHab")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("SpeciesScores")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$SelectHab <- renderUI({
        Names <- input$Habs 
        if(Names == 1){
         selectizeInput(inputId = "HabsCode",
                     label = "Select your habitat",
                     multiple = FALSE,
                     choices = Habitat_codes)   
        }
        else if(Names == 2){
            selectizeInput(inputId = "HabsName",
                           label = "Select your habitat",
                           multiple = FALSE,
                           choices = Habitat_names)   
        }
    })
    
    get_choice <- reactive({input$Names})
    
    output$SelectSpecies <- renderUI({
        Names <- get_choice() 
        if(Names == 1){
            selectizeInput(inputId = "SpeciesListSc",
                           label = "Select all species",
                           multiple = TRUE,
                           choices = Species_Latin)   
        }
        else if(Names == 2){
            selectizeInput(inputId = "SpeciesListCom",
                           label = "Select all species",
                           multiple = TRUE,
                           choices = Species_Danish)   
        }
    })
    
    #output$SpeciesScores <- tableOutput({
    #    Names <- get_choice() 
    #    if(Names == 1){
    #        ScoreTable <- Habitattypernes_scoringer %>% 
    #            dplyr::filter(videnskabeligt_navn %in% input$SpeciesListSc)
    #    } else if(Names == 2){
    #        ScoreTable <- Habitattypernes_scoringer %>% 
    #            dplyr::filter(dansk_navn %in% input$SpeciesListCom)
    #    }
    #    ScoreTable
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
