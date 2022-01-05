require(shiny)
library(shiny)
require(dplyr)
library(dplyr)
require(tidyr)
library(tidyr)
require(stringr)
library(stringr)
require(ggplot2)
library(ggplot2)


setwd("~/Documents/School/Year2/DAC/assignments/week3")
dat <- read.csv("disruptions-2019-Q3.csv",header = T,na.strings = c("", "NA"))

#exclude all empty strings
dat <- dat %>% na.omit()

#remove unnecessary data
dat <- dat%>%transmute(start_time=start_time,
                       cause_group=cause_group,
                       duration_minutes = duration_minutes)


# drop the time of delays, leave only dates and convert variable type to date
reg <-"(\\d{2})[-](\\d{2})[-](\\d{4})"
dat[,"start_time"]<-str_extract(dat[,"start_time"],reg)
dat<-dat %>% mutate(start_time=as.Date(start_time,format="%d-%m-%Y"))

# find earliest and latest start date
earliest<- min(dat[,"start_time"])
latest<- max(dat[,"start_time"])

# Group causes
reasons <- c("accidents",
             "engineering work",
             "external",
             "infrastructure",
             "logistical",
             "rolling stock",
             "staff",
             "unknown",
             "weather")


ui <- fluidPage(
  titlePanel("NS Train Delays",windowTitle = "NS Train Delays"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates","Delays between:",
                     format = "dd-mm-yyyy",start =earliest , 
                     end =latest, min = earliest,
                     max = latest),
      checkboxGroupInput("causes","Causes",choiceNames = reasons,
                         choiceValues = reasons)),
    
    mainPanel(h2("Plot of average delay against percentage by group"),
              plotOutput("averages")))
)



server <- function(input, output) {
  #calculate percentages
  percent <- reactive({
    if (is.null(input$causes)) {
      return(NULL)
    } 
    
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    groups <- input$causes
    
    total<- dat%>%filter(between(start_time,start_date,end_date))
    total<- total%>%count(cause_group)
    
    out <- dat%>%filter(between(start_time,start_date,end_date),
                        cause_group %in% groups)
    
    out<- out%>%count(cause_group)
    out[,"n"]<-100*(out[,"n"]/sum(total[,"n"]))
    
    return(out)
  })
  
  
  
  #calculate averages
  avgs <- reactive({
    if (is.null(input$causes)) {
      return(NULL)
    } 
    
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    groups <- input$causes
    
    
    out <- dat%>%filter(between(start_time,start_date,end_date),
                        cause_group %in% groups)
    
    out<- out%>%group_by(cause_group)
    out<- out%>%summarise(average=mean(duration_minutes))
    
    return(out)
  })
  
  # join percentages and averages into one dataframe
  join <- function(x,y) {
    
    d<-left_join(x,y, by="cause_group")
    
    if(any(is.na(d[,"cause_group"]))){
      return(NULL)
    }
    return(d)
    
  }
  
  output$averages<- renderPlot({
    if (is.null(avgs())) {
      return()
    }
    
    y<-avgs()
    x<-percent()
    
    d<-join(x,y)
    
    if(is.null(d)){
      return()
    }
    
    max_avg <-max(d[,"average"])+50
    max_per <-max(d[,"n"])+10
    
    plt<-ggplot(d,aes(n,average,label =cause_group,
                      color=cause_group))+geom_text(size=6, hjust=0.5, vjust=-0.5)
    plt<-plt + geom_point() +ylim(0,max_avg)+ xlim(0,max_per) 
    plt<- plt+xlab('Percentage of delays / %')
    plt<- plt+ylab('Average delay / minutes')
    plt<-plt+ggtitle(paste("From",paste(format(input$dates[1],"%d-%m-%Y"),
                                        format(input$dates[2],"%d-%m-%Y"),sep=" to ")))
    plt<-plt+theme(legend.position = "none",plot.title = 
                     element_text(size = 25, face = "bold"),
                   axis.title.y = element_text(size = 20),
                   axis.title.x = element_text(size = 22),
                   axis.text.x =element_text(size = 16),
                   axis.text.y =element_text(size = 16))
    plt
  })
}
shinyApp(ui = ui, server = server)