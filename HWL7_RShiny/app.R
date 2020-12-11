library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)

load("./Data/HW5-Data.RData")

#Data Pre-Processing####################################################

#Helper function, used to extract time of session from POSIX datetime
  #Input: POSIX datetime
  #Output: Minute count (numeric) of minutes past 00:00:00
as_min = function(t){
  hr = as.numeric(hour(t))*60
  return(as.numeric(minute(t))+ hr)
}

#Helper function, used to extract earliest and latest session POSIX timestamps for a given user ID
  #input User ID
  #Output (min,max) vector of earliest and latest logged_at_local timestamps
get_date_range = function(id){
  min_date = min((medtn %>% filter(ID==id))$logged_at_local)
  max_date = max((medtn %>% filter(ID==id))$logged_at_local)
  return(c(min_date, max_date))
}


#Split logged_at_local:POSIXct column into Day:chr (date) and Time:num columns
medtn_dt = medtn %>% mutate(day = date(logged_at_local),
                            time = as_min(logged_at_local) 
                            )
#Extract user ID's
id_nums  = medtn_dt$ID %>% unique()




#Define Page############################################################

ui <- fluidPage( titlePanel('Meditation Data'), window = 'Meditation Data',
                 sidebarLayout(
                   #Contains inputs
                    sidebarPanel(
                      #Drop Down Menu of Participant ID's
                      selectInput(
                        inputId = 'id',
                        label = 'Participant ID',
                        choices = id_nums
                      ),
                      br(),
                      #Date Range Select
                      dateRangeInput(
                        inputId = 'dates',
                        label = 'Date Range'
                      ),
                      br(),
                      #Shape-mapping Radio Button Select
                      radioButtons('shp', 
                                   'Select Shape Mapping', 
                                   choices = c('Session Type', 'Session Completed'),
                                   selected = 'Session Type' ),
                      br(),
                      radioButtons('complete_only',
                                   'Exclude incomplete sessions in Bar Plot?',
                                   choices = c('Yes', 'No'),
                                   selected = 'Yes')
                    ),
                    #Displays output
                    mainPanel(
                      #Scatter plot
                      plotOutput('dateVsStart_plt', brush = brushOpts(id="brsh")),
                      br(),
                      #text summary of brush-selected area from scatter plot
                      textOutput('selected_summary'),
                      br(), br(),
                      plotOutput('type_preference_bar')
                    )
                 )
              )

server <- function(input, output, session){
  
  #filter meditation dataframe by chosen participant ID
  filtered = reactive({ medtn_dt %>% filter(ID == input$id) })
  
  #update start and end dates for dateRangeInput (inputId='dates')
  observe({
    rng = get_date_range(input$id)
    updateDateRangeInput(session, 'dates',label = 'Date Range', start = rng[1], end = rng[2])
  })
  
  
  #define Date vs Start Time scatter plot output
  output$dateVsStart_plt <- renderPlot({
    plot_mappings = NULL
    
    #set scatterplot color and shape aesthetic maps
    if(input$shp == 'Session Type'){
        plot_mappings = geom_point(aes(shape = session_type, color = duration_in_sec ), size=3) 
    }
    else{
        plot_mappings = geom_point(aes(shape = completed, color = duration_in_sec ), size=3) 
    }
    
    #finish defining scatterplot
    ggplot(data = filtered(), aes(y=time,x=day)) + 
      plot_mappings +
      #set y-axis breakpoints and labels
      scale_y_continuous(breaks = seq(0,1440,by=240), labels = c('12am','4am','8am','12pm','4pm','8pm','12am'))+
      #Set y-axis and x-axis labels
      labs(y="Start Time", x = "Date") +
      #Change colors for duration_in_sec mapping
      scale_color_gradient(low = 'yellow', high = 'orangered')+
      xlim(input$dates[1],input$dates[2])
  })
  
  #define brush-selected text output
  output$selected_summary = renderText({
    num_selected = nrow(brushedPoints(filtered(),input$brsh))
    
    #default text output
    if(num_selected == 0){
      return("Please click and drag on the graph to select data")
    }
    #text output when user selects populated area on scatterplot
    else{
      
      selected = brushedPoints(filtered(),input$brsh)
      session_count  = nrow(selected)
      ave_session_length = mean(selected$duration_in_sec)/60
      ave_session_length = round(ave_session_length, digits = 2)
      
      format(c("There were ", session_count, " total observations selected. ",
        "The average duration for these observations is ", ave_session_length," min."))
    }
  })
  
  #Define barchart of session_types (height corresponds to number of sessions)
  output$type_preference_bar = renderPlot({
    session_type_data = filtered()
    #if only completed sessions should be counted
    if(input$complete_only == 'Yes'){
      session_type_data <- session_type_data %>% filter(completed == "true")
    }
    ggplot(data=session_type_data, aes(session_type)) + geom_bar(stat='count') + labs(y="Number of Sessions", x= "Session Type")
  })
}

shinyApp(ui=ui, server=server)

