library(shiny)
library(gapminder)
library(dplyr)
library(ggplot2)


gapminder = gapminder %>% mutate_at(c('year','country'), as.factor)

headerRow = div(
                selectInput(
                    'selYear',
                    label = 'Select the year',
                    multiple = TRUE,
                    choices = gapminder$year
                ),
                selectInput(
                    'selCountry',
                    label= 'Select the country',
                    multiple = TRUE,
                    choices = gapminder$country
                )
            )

dataPanel = tabPanel('Data',
                     #selectInput('selYear',label = 'Select year',multiple = TRUE,choices = gapminder$year),
                     tableOutput('dataTable'))

plotPanel = tabPanel('Plot', 
                     #selectInput('selCountry', label = 'Select country', multiple = TRUE, choices = gapminder$country),
                     plotOutput('plotData')
                     )

ui = navbarPage("Shiny app", dataPanel, plotPanel,header =  headerRow)

server = function(input,output) {
    
    #This is a function, (because of reactive)
    gapminder_filtered = reactive({ gapminder %>% filter(year %in% input$selYear, country %in% input$selCountry)})
    
    output$dataTable = renderTable({
        req(input$selYear)
        gapminder_filtered()
    })
    
    
    output$plotData = renderPlot({
        req(input$selCountry)
        req(input$year)
        gapminder_filtered() %>%
        ggplot(mapping =  aes(x = country, y = pop, fill = year)) + 
            geom_bar(stat = 'identity', position = position_dodge()) + 
            theme_classic()
    })    
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
