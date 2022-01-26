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

library(readxl)

Habitattypernes_scoringer <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                                          sheet = "bilag 1 artsscorer") %>% 
    clean_names() %>% 
    dplyr::mutate(dansk_navn = str_remove_all(dansk_navn, "\\r\\n"), 
                  videnskabeligt_navn = str_remove_all(videnskabeligt_navn, "\\r\\n")) %>% 
    pivot_longer(stenstrand_12:skov_91, names_to = "Habitat", values_to = "Artscore")
    

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
        mainPanel(h2("Raw data"),
           dataTableOutput("SpeciesScores"),
           h2("Calculated values"),
           dataTableOutput("Table2")
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
    
    Table1 <- reactive({
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
    
    
    output$SpeciesScores <- renderDataTable({
      Table1()
    })
    
    middelscore <- reactive(mean(Table1()$Artscore))
    Summscore <- reactive(sum(Table1()$bilagsart_1_ikke_bilagsart_0))
    
    HabitatScores <- reactive({
        if(Habs() == 1){
            req(input$HabsCode)
            SelectHabitat <- Habitat_types %>% 
                dplyr::filter(habitatnaturtype %in% input$HabsCode)
        }
        else if(Habs() == 2){
            req(input$HabsName)
            SelectHabitat <- Habitat_types %>% 
                dplyr::filter(habitat_name %in% input$HabsName)
        }
    })
    
    output$Table2 <- renderDataTable({
        data.frame(Symbol = c("m", "a(b)", "s", "m(a)", "A(s)", "a(t)", "n(a)", "d", "A(d)", NA), 
                   Beskrivelse = c("Justeret middelscore for prøvefeltet", "Antal (a) bidragsarter (b) (antal med artsscore over 0).", "Artssum", "Gennemsnit justeret middel score", "Artssscoreindex", "antal plantearter i prøvefeltet (uden mosser mm)", "middel artsantal i naturtypen", "diversitetsparameter", "Artsdiversitetsindex", "Artsindex (vægtning af artscore- og artsdiversitetsindeks)"), 
                   Formel = c(NA, NA, "m*a(b)", NA, "A(s) = 1/((1+exp(m(a)))*exp(1,60(1-m)))", NA, NA, "d = 0,8*m(a)*n(a)", "A(d) = (a(b)/a(t))*(1-(1/exp(s/d)))", "0,75*A(s)+0,25*A(d)"), 
                   Scores = c(middelscore(), Summscore(), middelscore()*Summscore(), HabitatScores()$gennemsnitlig_middelscore,
                              1/((1+exp(HabitatScores()$gennemsnitlig_middelscore))*exp(1.60*(1-middelscore()))), 
                              nrow(Table1()),
                             HabitatScores()$gennemsnitligt_artsantal,
                             HabitatScores()$gennemsnitlig_middelscore*HabitatScores()$gennemsnitligt_artsantal*0.8,
                             (Summscore()/nrow(Table1()))*(1-(1/exp((middelscore()*Summscore())/(HabitatScores()$gennemsnitlig_middelscore*HabitatScores()$gennemsnitligt_artsantal*0.8)))),    
                             0.75*(1/((1+exp(HabitatScores()$gennemsnitlig_middelscore))*exp(1.60*(1-middelscore()))))+0.25*((Summscore()/nrow(Table1()))*(1-(1/exp((middelscore()*Summscore())/(HabitatScores()$gennemsnitlig_middelscore*HabitatScores()$gennemsnitligt_artsantal*0.8)))))))
    })
    #
}

# Run the application 
shinyApp(ui = ui, server = server)
