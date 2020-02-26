library(shiny)
library(gapminder)
library(dplyr)
library(ggplot2)
library(stringr)


gapminder = gapminder %>% mutate_at(c('year','country'), as.factor)

headerRow = div(
                selectInput(
                    'selYear',
                    label = 'Select the year',
                    multiple = TRUE,
                    choices = gapminder$year,
                    selected = gapminder$year[1:3]
                ),
                selectInput(
                    'selCountry',
                    label= 'Select the country',
                    multiple = TRUE,
                    choices = gapminder$country,
                    selected = unique(gapminder$country)[1:2]
                )
            )

dataPanel = tabPanel('Data',
                     #selectInput('selYear',label = 'Select year',multiple = TRUE,choices = gapminder$year),
                     tableOutput('dataTable'))



plotPanel = tabPanel('Plot', 
                     #selectInput('selCountry', label = 'Select country', multiple = TRUE, choices = gapminder$country),
                     fluidRow(column(width = 8, plotOutput('plotData', hover =  hoverOpts(id = "plot_hover", delayType = "throttle"))),
                              column(width = 4, h2('Information'),
                                     div("Country: ", textOutput('txtCountry', inline = TRUE)),
                                     div("Year: ", textOutput('txtYear', inline = TRUE)),
                                     div('Population: ', textOutput('txtPop', inline = TRUE)))
                                     #verbatimTextOutput("plot_hoverinfo"))
                            )
                     )

plotlyPanel = tabPanel("plotlyData",
                       plotly::plotlyOutput('plotlyData') #if we load plotly as library, it substitutes ggplot
                       )

ui = navbarPage("Shiny app", dataPanel, plotPanel,plotlyPanel, header =  headerRow)

server = function(input,output) {
    
    #This is a function, (because of reactive)
    gapminder_filtered = reactive({ gapminder %>% filter(year %in% input$selYear, country %in% input$selCountry)})
    
    output$dataTable = renderTable({
        req(input$selYear)
        gapminder_filtered()
    })
    
    output$plotData = renderPlot({
        req(input$selCountry)
        req(input$selYear)
        gapminder_filtered() %>%
        ggplot(mapping =  aes(x = country, y = pop, fill = year)) + 
            geom_bar(stat = 'identity', position = position_dodge()) + 
            theme_classic()
    })  
    
    output$plot_hoverinfo <- renderPrint({
        cat("Mouseover information :\n")
        str(input$plot_hover)
    })
    
    countryIndex = reactive({
        req(input$plot_hover$x)
        round(input$plot_hover$x)
    })
    
    yearIndex = reactive({
        req(input$plot_hover$x)
        ceiling((input$plot_hover$x - round(input$plot_hover$x) + 0.5) * length(input$selYear))
    })
    
    
    countryName = reactive({
        req(countryIndex() > 0 & countryIndex() <= length(input$selCountry))
        input$selCountry[countryIndex()]
    })
    
    yearName = reactive({
        req(yearIndex() > 0 & yearIndex() <= length(input$selYear))
        str_sort(input$selYear)[yearIndex()]
    })
    
    output$txtCountry = renderText(countryName())
    output$txtYear = renderText(yearName())
    output$txtPop = renderText(gapminder_filtered() %>% filter(year == yearName() & country == countryName()) %>% pull(pop))
    
    output$plotlyData = plotly::renderPlotly({
        req(input$selCountry)
        req(input$selYear)
        gapminder_filtered() %>%
            ggplot(mapping =  aes(x = country, y = pop, fill = year)) + 
            geom_bar(stat = 'identity', position = position_dodge()) + 
            theme_classic()
    })  
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
