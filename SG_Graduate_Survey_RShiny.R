library(tidyverse)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(dashboardthemes)

##############################
# Quick Cleaning

# read data file
dat1<-read_csv("graduate-employment-survey-ntu-nus-sit-smu-suss-sutd.csv")
#str(dat1)
#summary(dat1)
# take note of the formats

# remove na that has been explicitly spelt out.
# normal remove NAs won't work.
dat1=filter(dat1, employment_rate_overall != "na")

# read mapping files
dat2<-read_csv("Reclassified_Schools.csv")
dat4<-read_csv("Reclassified_University.csv")

# merge with mapping files
dat5<-merge(x = dat1, y = dat2, by = "degree", all.x = TRUE)
dat3<-merge(x = dat5, y = dat4, by = "university", all.x = TRUE)

# transform to numeric from characters 
dat3=transform(dat3
               ,employment_rate_overall = as.numeric(employment_rate_overall)
               ,employment_rate_ft_perm = as.numeric(employment_rate_ft_perm)	
               ,basic_monthly_mean = as.numeric(basic_monthly_mean)	
               ,basic_monthly_median = as.numeric(basic_monthly_median)	
               ,gross_monthly_mean = as.numeric(gross_monthly_mean)	
               ,gross_monthly_median = as.numeric(gross_monthly_median)	
               ,gross_mthly_25_percentile = as.numeric(gross_mthly_25_percentile)	
               ,gross_mthly_75_percentile= as.numeric(gross_mthly_75_percentile)
)

# reconfirm
#summary(dat3)
#str(dat3)
dat3 <- subset (dat3, select = -school)
# for filters
xyear <- dat3 %>% select(year) %>% distinct
xuni <- dat3 %>% select(Reclassified_University) %>% distinct
xschool <- dat3 %>% select(Reclassified_School) %>% distinct
xdegree <- dat3 %>% select(degree) %>% distinct
# data for data boxes of latest median employment rate and gross median monthly
box1=dat3 %>% 
  filter(year==2018)%>% 
  summarise(median_employment_rate_overall=median(employment_rate_overall))
box2=dat3 %>% 
  filter(year==2018)%>% 
  summarise(median_gross_monthly_median=median(gross_monthly_median))
# data for line charts of YoY median employment rate and gross median monthly
line1=dat3 %>%
  group_by(year)%>%
  summarise(median_employment_rate_overall=median(employment_rate_overall))
line2=dat3 %>%
  group_by(year)%>%
  summarise(median_gross_monthly_median=median(gross_monthly_median))
# data for bar charts of latest median employment rate and gross median monthly by UNIVERSITY
bar1=dat3 %>%
  filter(year==2018)%>% 
  group_by(university)%>%
  summarise(median_employment_rate_overall=median(employment_rate_overall))%>%
  arrange(desc(median_employment_rate_overall))
bar2=dat3 %>%
  filter(year==2018)%>% 
  group_by(university)%>%
  summarise(median_gross_monthly_median=median(gross_monthly_median))%>%
  arrange(desc(median_gross_monthly_median))
# data for line charts of YoY median employment rate and gross median monthly by UNIVERSITY
line3=dat3 %>%
  group_by(year,university)%>%
  summarise(median_employment_rate_overall=median(employment_rate_overall))
line4=dat3 %>%
  group_by(year,university)%>%
  summarise(median_gross_monthly_median=median(gross_monthly_median))
# data for bar charts of latest median employment rate and gross median monthly by TOP 5 SCHOOL
bar3=dat3 %>%
  filter(year==2018)%>% 
  group_by(Reclassified_School)%>%
  filter(!is.na(Reclassified_School))%>% 
  summarise(median_employment_rate_overall=median(employment_rate_overall))%>%
  arrange(desc(median_employment_rate_overall))%>%
  slice(1:5)
bar4=dat3 %>%
  filter(year==2018)%>% 
  group_by(Reclassified_School)%>%
  filter(!is.na(Reclassified_School))%>% 
  summarise(median_gross_monthly_median=median(gross_monthly_median))%>%
  arrange(desc(median_gross_monthly_median))%>%
  slice(1:5)
# data for scatter chart of latest median employment rate and gross median monthly by SCHOOLS
scat1=dat3 %>%
  filter(year==2018)%>% 
  group_by(Reclassified_School)%>%
  filter(!is.na(Reclassified_School))%>% 
  summarise(median_employment_rate_overall=median(employment_rate_overall), median_gross_monthly_median=median(gross_monthly_median))

##############################
# Dashboard UI

ui <- dashboardPage(
  dashboardHeader(title = "SG Graduate Survey"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "page1"),
      menuItem("University View", tabName = "page2"),
      menuItem("School View", tabName = "page3"),
      menuItem("Details", tabName = "page4"),
      menuItem("Course Recommender", tabName = "page5")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem(tabName = "page1", h2("Overview"),
        fluidRow(
          hr(),
          column(12,
                 fluidRow(
                   column(6,infoBoxOutput("info_box1")
                          #fluidRow(6,infoBoxOutput("info_box1"))
                          )
                  ,column(6,infoBoxOutput("info_box2"))
                 ),
            hr(),
                fluidRow(column(6,plotOutput("linechart1"))
                         ,column(6,plotOutput("linechart2")))
          )
        )
      ),
      tabItem(tabName = "page2", h2("University View"), 
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(6,plotOutput("barchart1")
                                ),
                         column(6,plotOutput("barchart2")
                                )
                          ),
                       hr(),
                       fluidRow(
                         column(6,plotOutput("linechart3")
                                ),
                         column(6,plotOutput("linechart4")
                                )
                         )
                )
              )
      ),
      tabItem(tabName = "page3", h2("School View"), 
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(6,plotOutput("barchart3")
                         ),
                         column(6,plotOutput("barchart4")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,plotOutput("scatterplot1")
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "page4", h2("Details"), 
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(3,                       
                                selectInput("select1", h5("University"), choices=list("NTU","NUS","SMU","SIT","SUTD","SUSS"), selected = "NUS", multiple = TRUE)
                         ),
                         column(3,selectInput("select2", h5("Year"), xyear, selected=c(2018,2017,2016,2015,2014,2013), multiple = TRUE)
                         ),
                         column(3,selectInput("select3", h5("School"), xschool, selected="Technology, Computing and Information Systems", multiple = TRUE)
                         ),
                         column(3,selectInput("select4", h5("Degree"), xdegree, selected="Bachelor of Computing (Computer Science)", multiple = TRUE)
                         )
                       ),
                       hr(),
                       fluidRow(   
                         column(12,
                                DTOutput("datatable1")
                                )
                       )
                )     
                       )
      ),
      tabItem(tabName = "page5", h2("Course Recommender"), 
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(3,                       
                                numericInput("select5", h5("Expected Completion Year (2022 - 2030)"), 
                                            2022, min= 2019, max = 2030, step = 1)
                         ),
                         column(3,numericInput("select6", h5("Minimum Expected Salary (1.8k - 10.0k)"), 2500, min= 1800, max = 10000, step = 100)
                         ),
                         column(3,""
                         ),
                         column(3,""
                         )
                       ),
                       hr(),
                       fluidRow(   
                         column(12,
                                DTOutput("datatable2")
                         )
                       )
                )
              )
      )
    )
  )
)
##############################
# Dashboard Server

server <- function(input, output) {
  # latest median employment rate and gross median monthly
  output$info_box1 <- renderInfoBox({
    infoBox("Employment Rate", box1,icon = icon("user"), color='purple')
  })
  output$info_box2 <- renderInfoBox({
    infoBox("Gross Median (SGD)", box2,icon = icon("dollar"), color='purple')
  })  
  # line charts of YoY median employment rate and gross median monthly
  output$linechart1 <- renderPlot({
    ggplot(data=line1, aes(x=year, y=median_employment_rate_overall)) +
      geom_line(aes(size=0.8), color=rgb(195, 177, 225,maxColorValue = 255))+
      geom_point()+
      labs(title="YoY Median Employment Rate", subtitle="", y="Median Employment Rate", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_label(aes(label=median_employment_rate_overall))
  })
  output$linechart2 <- renderPlot({
    ggplot(data=line2, aes(x=year, y=median_gross_monthly_median)) +
      geom_line(aes(size=0.8), color=rgb(195, 177, 225,maxColorValue = 255))+
      geom_point()+
      labs(title="YoY Gross Median Monthly (SGD)", subtitle="", y="Gross Median Monthly (SGD)", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_label(aes(label=median_gross_monthly_median))
  })  
  # bar charts of latest median employment rate and gross median monthly by UNIVERSITY
  output$barchart1 <- renderPlot({
    ggplot(data=bar1, aes(x=reorder(university,median_employment_rate_overall), y=median_employment_rate_overall, fill=university)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title="Latest Median Employment Rate by University", subtitle="", y="Median Employment Rate", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_text(aes(label=median_employment_rate_overall))
  })  
  output$barchart2 <- renderPlot({
    ggplot(data=bar2, aes(x=reorder(university,median_gross_monthly_median), y=median_gross_monthly_median, fill=university)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title="Latest Gross Median Monthly (SGD) by University", subtitle="", y="Gross Median Monthly (SGD)", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_text(aes(label=median_gross_monthly_median))
  })
  # line charts of YoY median employment rate and gross median monthly by UNIVERSITY
  output$linechart3 <- renderPlot({
  ggplot(data=line3, aes(x=year, y=median_employment_rate_overall, color=university)) +
    geom_line()+
    geom_point()+
      labs(title="YoY Median Employment Rate by University", subtitle="", y="Median Employment Rate", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_text(aes(label=median_employment_rate_overall),vjust=-.5)
  })  
  output$linechart4 <- renderPlot({
  ggplot(data=line4, aes(x=year, y=median_gross_monthly_median, color=university)) +
    geom_line()+
    geom_point()+
      labs(title="YoY Gross Median Monthly (SGD) by University", subtitle="", y="Gross Median Monthly (SGD)", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_text(aes(label=median_gross_monthly_median),vjust=-.5)
  })
  # bar charts of latest median employment rate and gross median monthly by TOP 5 SCHOOL 
  output$barchart3 <- renderPlot({
  ggplot(data=bar3, aes(x=reorder(Reclassified_School,median_employment_rate_overall), y=median_employment_rate_overall, fill=Reclassified_School)) +
    geom_bar(stat="identity") + coord_flip()+
      labs(title="Latest Median Employment Rate by Top 5 Schools", subtitle="", y="Median Employment Rate", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_text(aes(label=median_employment_rate_overall))
  })
  output$barchart4 <- renderPlot({    
  ggplot(data=bar4, aes(x=reorder(Reclassified_School,median_gross_monthly_median), y=median_gross_monthly_median, fill=Reclassified_School)) +
    geom_bar(stat="identity") + coord_flip()+
      labs(title="Latest Gross Median Monthly (SGD) by Top 5 Schools", subtitle="", y="Gross Median Monthly (SGD)", x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)), legend.position = "none")+
      geom_text(aes(label=median_gross_monthly_median))
  })
  # scatter chart of latest median employment rate and gross median monthly by SCHOOLS 
  output$scatterplot1 <- renderPlot({
    ggplot(data=scat1, aes(x=median_employment_rate_overall, y=median_employment_rate_overall)) +
    geom_point(aes(color = Reclassified_School, size=30))+
      labs(title="Latest Gross Median Monthly (SGD) and Median Employment Rate by Schools", subtitle="", y="Gross Median Monthly (SGD)", x = "Median Employment Rate")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color=	rgb(128,128,128,maxColorValue = 255)))
  })
  # data table
  output$datatable1 <- renderDataTable({
    plot_dat1<-filter(dat3, (Reclassified_University %in% input$select1 & year %in% input$select2 & Reclassified_School %in% input$select3 )
                      | (Reclassified_University %in% input$select1 & year %in% input$select2 & Reclassified_School %in% input$select3 & degree %in% input$select4)
                      )%>% 
      select(c("Reclassified_University","university","year","Reclassified_School","degree","employment_rate_overall","basic_monthly_median","gross_monthly_median")) %>% 
      rename(Uni = Reclassified_University, School = Reclassified_School) 
    plot_dat1
  })
  # list of schools based on expected gross monthly median and graduation year
  output$datatable2 <- renderDataTable({
    plot_dat2<-dat3 %>%
      filter(year==2018)%>% 
      filter(!is.na(Reclassified_School)) %>% 
      mutate(future_gross_monthly_median = gross_monthly_median * (1.022^(input$select5 - 2018)),x=gross_monthly_median*2) %>% 
      filter(future_gross_monthly_median>=input$select6) %>% 
      arrange(desc(future_gross_monthly_median)) %>%
      select(c("Reclassified_University","university","year","Reclassified_School","degree","future_gross_monthly_median","employment_rate_overall","gross_monthly_median")) %>% 
      rename(Uni = Reclassified_University, School = Reclassified_School, y2018_gross_monthly_median=gross_monthly_median, y2018_employment_rate_overall=employment_rate_overall) 
    plot_dat2
  })
}
##############################
# Run the application 
shinyApp(ui = ui, server = server)

