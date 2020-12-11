#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)
load("twdkills.Rdata")

kills.long  = melt(kills, id.vars = 'Season', variable.name = "Character", value.name = "Kills" )
kills.long[is.na(kills.long)] = 0

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel('Walking Dead Kills'), window = 'CS614 Lecture 7',
                sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput( 'character_in', label = "Character", 
                                            choices = levels(kills.long$Character)
                        ), 
                        br(),
                        radioButtons("show_brplt", 'Show Bar Chart', selected= FALSE, choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE)),
                        br(),
                        textInput("y_lbl", label = "y_axis for Line Graph", value = "")
                        
                    ),
                    
                    mainPanel(plotOutput('kills_plt'),
                              br(),
                              plotOutput('bar_plt'))
                )
)

server <- function(input, output) {
    filtered  = reactive({
        kills.long %>% filter(Character %in% input$character_in)
    })
    
    output$kills_plt <- renderPlot({
        ggplot(data= filtered(), aes(y = Kills, x= Season)) + geom_point(aes(color = Character)) + geom_line(aes(color = Character)) +
            labs(y = input$y_lbl)
    })
    
    output$bar_plt <- renderPlot({
        
        if(as.logical(input$show_brplt)  & length(input$character_in) != 0){
            ggplot(data = filtered(), aes(y=Kills, fill = Character, x = Character)) + geom_col() + facet_wrap(~Season) + labs(x = "")
        } else{
            NULL
            }
    })
}

shinyApp(ui = ui, server = server)