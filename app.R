library(shiny)
library(tidyverse)
library(plotly)
# install.packages("DT")
library(DT)


#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)

#####Make your app

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(    
    # input and diplay for page 1 tab1 and page 1 tab 2
            sliderInput(
                inputId = "Ideo_choice",
                label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
                min = 1,
                max = 5,
                value = 3
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Tab1", plotOutput(outputId = "distplot_tab1")), 
                tabPanel("Tab2", plotOutput(outputId = "distplot_tab2"))
            )
        )
    ),
    # input and display for page 2
    sidebarLayout(
        sidebarPanel(    
            checkboxGroupInput(inputId = "checked_genders", label="Select Gender",choices=c(1,2),)
        ),
        mainPanel(plotOutput(outputId = "plot3")
                  )
    )
)
    
    
    
    server<-function(input,output){
        
        # making page 1 tab 1 geom_bar pid7
        output$distplot_tab1 <- renderPlot({
            dat_selec <- filter(dat, ideo5==input$Ideo_choice)
            ggplot(data=dat_selec, aes(x=pid7)) + xlim(0,8) + ylim(0,125) +
                geom_bar()+labs(x="7 Point Party ID, 1=Very D, 7= Very R")
        })
        # making page 1 tab 2 geom_bar trumsupport
            output$distplot_tab2 <- renderPlot({
                dat_selec <- filter(dat, ideo5==input$Ideo_choice)
                ggplot(data=dat_selec, aes(x=CC18_308a)) + xlim(0,5) + ylim(0,150) +
                    geom_bar()+labs(x="Trump Support", y = "Count")
            
        })
        
        # building plot 3 
            output$plot3 <- renderPlot({
               
                ggplot(filter(dat, gender %in% input$checked_genders),aes(x=educ,y=pid7)) +
                    geom_point() +
                    geom_jitter()+
                    geom_smooth(method=lm,se=TRUE)
            })
        
    }      
        #####Hint: when you make the data table on page 3, you may need to adjust the height argument in the dataTableOutput function. Try a value of height=500
        
        
        



shinyApp(ui = ui,server = server)