require(shiny)

ui <- fluidPage(
  titlePanel("BC Liquir Store Prices", window = "CS614 Lecture 7"),
  
  sidebarLayout(
    sidebarPanel(sliderInput('priceInput','Price', min=0, max=100, value=c(10,20), pre='$'),
                 radioButtons("typeInput",'Product Type', choices = c('BEER','Refreshment','SPIRITS','WINE'), selected = 'BEER'),
                 selectInput('countryInput', 'Country', choices = c('CANADA','FRANCE','ITALY'))),
    mainPanel(plotOutput('myplot1'),
              br(),
              textOutput('nrow'),
              br(),
              tableOutput('mytable'))
  )
)

server <- function(input,output){}

shinyApp(ui=ui, server = server)
