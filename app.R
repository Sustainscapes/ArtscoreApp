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
library(Artscore)
library(shinymeta)

data("Habitat_List")
data("Species_List")


# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
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
          h2("Artsindex"),
          tableOutput("Table2"),
          h2("Strukturindeks"),
          uiOutput("Structure")
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
                           choices = unique(Species_List$Scientific_name) %>%  
                               sort())   
        }
        else if(get_choice() == 2){
            selectizeInput(inputId = "SpeciesListCom",
                           label = "Select all species",
                           multiple = TRUE,
                           choices = unique(Species_List$Danish_name) %>% sort())   
        }
    })
    # 1 species
    
    Habs <- reactive({input$Habs})

    output$SelectHab <- renderUI({
        if(Habs() == 1){
         selectizeInput(inputId = "HabsCode",
                     label = "Select your habitat",
                     multiple = FALSE,
                     choices = sort(unique(Habitat_List$Code)))   
        }
        else if(Habs() == 2){
            selectizeInput(inputId = "HabsName",
                           label = "Select your habitat",
                           multiple = FALSE,
                           choices = sort(unique(Habitat_List$habitat_name)))   
        }
    })
    
## 3 Filtered
    
    Table1 <- reactive({
        if(get_choice() == 1){
            req(input$SpeciesListSc)
            Species <- data.frame(ScientificName = input$SpeciesListSc,
                                  Common_name = rep(NA, length(input$SpeciesListSc)))
        } else if(get_choice() == 2){
            req(input$SpeciesListCom)
          Species <- data.frame(ScientificName = rep(NA, length(input$SpeciesListCom)),
                                Common_name = input$SpeciesListCom)
        }
        
        if(Habs() == 1){
            req(input$HabsCode)
          SelectHabitat <- data.frame(Habitat_name = NA,
                                      Habitat_code = input$HabsCode)
        }
        else if(Habs() == 2){
            req(input$HabsName)
            SelectHabitat <- data.frame(Habitat_name = input$HabsName,
                                        Habitat_code = NA)
        }
      list(Species, SelectHabitat)
    })
    
    
    
    
    
  output$Table2 <- renderTable(digits = 3, {
    if(!is.na(unique(Table1()[[1]]$ScientificName)) & !is.na(Table1()[[2]]$Habitat_code)){
      Artscore(ScientificName = Table1()[[1]]$ScientificName,
               Habitat_code = Table1()[[2]]$Habitat_code)
    } else if(!is.na(unique(Table1()[[1]]$ScientificName)) & !is.na(Table1()[[2]]$Habitat_name)){
      Artscore(ScientificName = Table1()[[1]]$ScientificName,
               Habitat_name = Table1()[[2]]$Habitat_name)
    } 
    else if(!is.na(unique(Table1()[[1]]$Common_name)) & !is.na(Table1()[[2]]$Habitat_code)){
      Artscore(Common_name = Table1()[[1]]$Common_name,
               Habitat_code = Table1()[[2]]$Habitat_code)
    } else if(!is.na(unique(Table1()[[1]]$Common_name)) & !is.na(Table1()[[2]]$Habitat_name)){
      Artscore(Common_name = Table1()[[1]]$Common_name,
               Habitat_name = Table1()[[2]]$Habitat_name)
    }
    
      #Artscore(ScientificName = ifelse(is.na(unique(Table1()[[1]]$ScientificName)), NULL, Table1()[[1]]$ScientificName), Common_name = ifelse(is.na(unique(Table1()[[1]]$Common_name)), NULL, Table1()[[1]]$Common_name), 
      #         Habitat_name = ifelse(is.na(Table1()[[2]]$Habitat_name), NULL, Table1()[[2]]$Habitat_name), Habitat_code = ifelse(is.na(Table1()[[2]]$Habitat_code), NULL, Table1()[[2]]$Habitat_code))  
    })
  
  output$Structure <- renderUI({
    if(!is.na(Table1()[[2]]$Habitat_code)){
      Tab <- GetWeights(Habitat_code = Table1()[[2]]$Habitat_code)
    } else if(!is.na(Table1()[[2]]$Habitat_name)){
      Tab <- GetWeights(Habitat_name = Table1()[[2]]$Habitat_name)
    }
    
    Subs <- unique(Tab$Subvariables)
    
    Radios <- list()
      for(i in 1:length(Subs)){
        Options <- Tab %>% 
          dplyr::filter(Subvariables == Subs[i]) %>% 
          pull(Scores)
        Radios[[i]] <- radioButtons(inputId = paste0("Radio1", i),
                                    label = Subs[i],
                                    choices = Options,
                                    selected = character(0))
      }
  
    Radios
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
