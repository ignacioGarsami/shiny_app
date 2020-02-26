library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinyjs)

data(esoph)

esoph$alcgp = lapply(strsplit(as.character(esoph$alcgp), "/"), `[[`, 1)
esoph$alcgp = lapply(strsplit(as.character(esoph$alcgp), "g"), `[[`, 1)


esoph$alcgp[esoph$alcgp == '120+'] <- '120-999'

esoph$alcgp = unlist(esoph$alcgp)

esoph$tobgp = lapply(strsplit(as.character(esoph$tobgp), "/"), `[[`, 1)
esoph$tobgp = lapply(strsplit(as.character(esoph$tobgp), "g"), `[[`, 1)


esoph$tobgp[esoph$tobgp == '30+'] <- '30-999'

esoph$tobgp = unlist(esoph$tobgp)


View(esoph)
headerRow = div(useShinyjs(),
                id='Header',
                selectInput(
                    'selGroup',
                    label = 'Select the age group',
                    multiple = TRUE,
                    choices = esoph$agegp,
                    selected = unique(esoph$agegp)
                ),
                selectInput(
                    'selAlc',
                    label = 'Select the alcohol consumption group',
                    multiple = TRUE,
                    choices = esoph$alcgp,
                    selected = unique(esoph$alcgp)[1]
                ),
                selectInput(
                    'selTob',
                    label = 'Select the tobacco consumption group',
                    multiple = TRUE,
                    choices = esoph$tobgp,
                    selected = unique(esoph$tobgp)[1]
                )
                
            )

headerRow_min = div(useShinyjs(),
                id='HeaderMin',
                selectInput(
                    'selGroup',
                    label = 'Select the age group',
                    multiple = TRUE,
                    choices = esoph$agegp,
                    selected = unique(esoph$agegp)
                )
                
)

mapPanel = tabPanel('Data',
                    downloadButton('downloadData', 'Download'),
                    tableOutput('mapTable')
                    )

Graphs = navbarMenu("Visualizations",
                    tabPanel('Cancer Incidences per age group',
                             headerRow_min,
                             h2('Cancer incidences per age group'),
                             plotly::plotlyOutput('IncidenceRate')),
                    tabPanel('Cancer incidences per substance consumption',
                             h2('Cancer incidences per substance consumption'),
                             plotly::plotlyOutput('AlcoholAgeGP'),
                             plotly::plotlyOutput('TobaccoAgeGP')))

ui = navbarPage("Esophageal cancer statistics", Graphs, mapPanel, header = headerRow, id = 'navBar')

server = function(input,output) {
    
    #This is a function, (because of reactive)
    esoph_filtered_incidence = reactive({esoph %>% filter(agegp %in% input$selGroup)})
    esoph_filtered = reactive({esoph %>% filter(agegp %in% input$selGroup & tobgp %in% input$selTob & alcgp %in% input$selAlc)})
    
    
    observe( if(input$navBar == "Data" || input$navBar == 'Cancer Incidences per age group'){shinyjs::hide('Header')}else{ shinyjs::show('Header')})
    
    
    output$IncidenceRate = plotly::renderPlotly({
        req(input$selGroup)
        esoph_filtered_incidence() %>%
            ggplot(mapping =  aes(x = agegp, y = ncases)) + 
            geom_bar(stat = 'identity', position = position_dodge()) + 
            labs(title = 'Esophageal cancer patients by age group', x = 'Age group', y = 'Number of cases') +
            theme_classic()
    }) 
    
    
    output$AlcoholAgeGP = plotly::renderPlotly({
        req(input$selGroup)
        req(input$selAlc)
        esoph_filtered() %>%
            ggplot(mapping =  aes(x = alcgp, y = ncases, fill = agegp)) + 
            geom_bar(stat = 'identity', position = position_dodge()) + 
            labs(title = 'Esophageal cancer patients by alcohol consumption', x = 'Alcohol consumption', y = 'Number of cases') +
            theme_classic()
    }) 
    
    output$TobaccoAgeGP = plotly::renderPlotly({
        req(input$selGroup)
        req(input$selTob)
        esoph_filtered() %>%
            ggplot(mapping =  aes(x = tobgp, y = ncases, fill = agegp)) + 
            geom_bar(stat = 'identity', position = position_dodge()) + 
            labs(title = 'Esophageal cancer patients by tobacco consumption', x = 'Tobacco consumption', y = 'Number of cases') +
            theme_classic()
    }) 
    
    output$mapTable = renderTable({
        esoph
    })
    
    output$downloadData = downloadHandler(
                            filename = function() {
                                paste('Esoph-data', '.csv', sep='')
                            },
                            content = function(con) {
                                write.csv(esoph, con)
                            }
                         )
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
