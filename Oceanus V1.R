



# Cleaning the "Films" Dataframe"  ----------------------------------------


library(tidyverse)
library(shiny)
library(ggplot2)


films <- read.csv("C:\\Users\\Kai\\Desktop\\cleanfilms.csv", fileEncoding = 'UTF-8-BOM', stringsAsFactors = T)

colnames(films)[2] <- c("Length")
films <- films[,-10] #remove the "The" Column. 


#Adding a decade column: 

films  <- films %>% mutate(Decade = case_when(Year>=1920 & Year<1930 ~ "1920s",
                                              Year>=1930 & Year<1940 ~ "1930s",
                                              Year>=1940 & Year<1950 ~ "1940s",
                                              Year>=1950 & Year<1960 ~ "1950s",
                                              Year>=1960 & Year<1970 ~ "1960s",
                                              Year>=1970 & Year<1980 ~ "1970s",
                                              Year>=1980 & Year<1990 ~ "1980s",
                                              Year>=1990 & Year<2000 ~ "1990s"))


films  <- films %>% mutate(Length_Range = case_when(Length<40 ~ "Less than 40 minutes",
                                                    Length>=40 & Length<=60 ~ "40-60 minutes",
                                                    Length>60 & Length<=120 ~ "1:00-1:20",
                                                    Length>80 & Length<=100 ~ "1:20-1:40",
                                                    Length>100 & Length<=120 ~ "1:40-2:00",
                                                    Length>120 & Length<=140 ~ "2:00-2:20",
                                                    Length>140 & Length<=160 ~ "2:20-2:40",
                                                    Length>160 & Length<=180 ~ "2:40-3:00",
                                                    Length > 180 ~ "3 hours +"))


#Converting Decade,Length_Range, and Genre to factors. 
films$Decade <- as.factor(films$Decade)
films$Length_Range <- as.factor(films$Length_Range)
films$Subject <- as.factor(films$Subject)
str(films) #to check the above has worked. 
orig_films <- films[,c(1:9)] #The original, but cleaned dataframe. 


# The R Shiny App  --------------------------------------------------------

ui = fluidPage(
    titlePanel(h1("Films", align = "center")),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("Year",
                           "Year:",
                           c("All",
                             unique(as.character(films$Year))))
        ),
        column(4,
               selectInput("Length",
                           "Length:",
                           c("All",
                             unique(as.character(films$Length_Range))))
        ),
        column(4,
               selectInput("Genre",
                           "Genre:",
                           c("All",
                             unique(as.character(films$Genre))))
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table")
)



server = function(input, output) {
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- films
        if (input$Year != "All") {
            data <- data[data$Year == input$Year,]
        }
        if (input$Genre != "All") {
            data <- data[data$Subject == input$Genre,]
        }
        if (input$Length != "All") {
            data <- data[data$Length_Range == input$Length,]
        }
        data
    }))
    
}





# Create Shiny app ----
shinyApp(ui = ui, server = server)


