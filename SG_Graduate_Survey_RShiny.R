library(tidyverse)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(dashboardthemes)
library(stringr)

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
xyear <- dat3 %>% select(year) %>% distinct %>% dplyr::arrange(year)
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

# Overview - data for line charts of YoY median employment rate and gross median monthly
line1=dat3 %>%
  group_by(year)%>%
  summarise(median_employment_rate_overall=median(employment_rate_overall))
line2=dat3 %>%
  group_by(year)%>%
  summarise(median_gross_monthly_median=median(gross_monthly_median))

# University view - data for bar charts of latest median employment rate and gross median monthly by UNIVERSITY
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

# University view - data for line charts of YoY median employment rate and gross median monthly by UNIVERSITY
line3=dat3 %>%
  filter(university!='Singapore University of Technology and Design') %>%
  group_by(year,university)%>%
  summarise(median_employment_rate_overall=median(employment_rate_overall),.groups = 'keep')
line4=dat3 %>%
  filter(university!='Singapore University of Technology and Design') %>%
  group_by(year,university)%>%
  summarise(median_gross_monthly_median=median(gross_monthly_median),.groups = 'keep')

# School view - data for bar charts of latest median employment rate and gross median monthly by TOP 5 SCHOOL
bar3=dat3 %>%
  filter(year==2018)%>% 
  group_by(Reclassified_School)%>%
  filter(!is.na(Reclassified_School))%>% 
  summarise(median_employment_rate_overall=median(employment_rate_overall))%>%
  arrange(desc(median_employment_rate_overall))#%>%
  #slice(1:5)
bar4=dat3 %>%
  filter(year==2018)%>% 
  group_by(Reclassified_School)%>%
  filter(!is.na(Reclassified_School))%>% 
  summarise(median_gross_monthly_median=median(gross_monthly_median))%>%
  arrange(desc(median_gross_monthly_median))#%>%
  #slice(1:5)

# School view - data for scatter chart of latest median employment rate and gross median monthly by SCHOOLS
scat1=dat3 %>%
  filter(year==2018)%>% 
  group_by(Reclassified_School)%>%
  filter(!is.na(Reclassified_School))%>% 
  summarise(median_employment_rate_overall=median(employment_rate_overall), median_gross_monthly_median=median(gross_monthly_median))

##############################
#######   Dashboard UI  ######
##############################

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
      theme = "onenote"
    ),
    tabItems(
      tabItem(tabName = "page1", h2("Overview"),
              h5("Overview of employment rate and gross monthly salary over the year."),
        fluidRow(
          hr(),
          column(12,
                 fluidRow(
                   column(1,"")
                   ,column(4,infoBoxOutput("info_box1",width = 12)
                          #fluidRow(6,infoBoxOutput("info_box1"))
                          )
                   ,column(2,"")
                  ,column(4,infoBoxOutput("info_box2",width = 12))
                  ,column(1,"")
                 ),
            hr(),
                fluidRow(column(6,plotOutput("linechart1"))
                         ,column(6,plotOutput("linechart2")))
          )
        )
      ),
      tabItem(tabName = "page2", h2("University View"), 
              h5("Overview of employment rate and gross monthly salary based on University."),
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(6,plotlyOutput("barchart1")
                                ),
                         column(6,plotlyOutput("barchart2")
                                )
                          ),
                       hr(),
                       fluidRow(
                         column(6,plotlyOutput("linechart3")
                                ),
                         column(6,plotlyOutput("linechart4")
                                )
                         )
                )
              )
      ),
      tabItem(tabName = "page3", h2("School View"), 
              h5("Overview of employment rate and gross monthly salary based on schools."),
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(6,plotlyOutput("barchart3")
                         ),
                         column(6,plotlyOutput("barchart4")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,plotlyOutput("scatterplot1")
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "page4", h2("Details"), 
              h5("Historical data and filtering search page."),
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
              h5("Course Recommendation based on your Choice of University, Expected Completion Year and Minimum Expected Salary."),
              h5("The future gross monthly salary is calculated based on compounded annual growth rate formula on historical data of year 2013-2018."),
              fluidRow(
                hr(),
                column(12,
                       fluidRow(
                         column(3,selectInput("select7", h5("Choice of Universities:"), 
                                              choices=list("NTU","NUS","SMU","SIT","SUTD","SUSS"), 
                                              selected = list("NTU","NUS"), 
                                              multiple = TRUE)
                         ),
                         column(3,                   
                                numericInput("select5", h5("Expected Completion Year (2022 - 2030)"), 
                                            2022, min= 2019, max = 2030, step = 1)
                         ),
                         column(3,numericInput("select6", h5("Minimum Expected Salary (1.8k - 10.0k)"), 2500, min= 1800, max = 10000, step = 100)
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
###    Dashboard Server    ###
##############################

server <- function(input, output) {
  # latest median employment rate and gross median monthly
  output$info_box1 <- renderInfoBox({
    infoBox("Latest Median Employment Rate", box1,icon = icon("user"), color='purple')
  })
  
  output$info_box2 <- renderInfoBox({
    infoBox("Latest Median Gross Monthly Salary (SGD)", box2,icon = icon("dollar"), color='purple')
  })  
  
  # Overview - line chart of YoY median employment rate
  output$linechart1 <- renderPlot({
    ggplot(data=line1, aes(x=year, y=median_employment_rate_overall)) +
      geom_line(size=3, color=rgb(99, 85, 140,maxColorValue = 255))+
      geom_point()+
      labs(title="YoY Median Employment Rate",
           y = "Employment Rate", 
           x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)), 
            text=element_text(size=12,face='bold'),
            legend.position = "none")+
      geom_label(aes(label=median_employment_rate_overall))
  })
  
  # Overview - line chart for gross median monthly
  output$linechart2 <- renderPlot({
    ggplot(data=line2, aes(x=year, y=median_gross_monthly_median)) +
      geom_line(size=3, color=rgb(99, 85, 140,maxColorValue = 255))+
      geom_point()+
      labs(title="YoY Median Gross Monthly Salary (SGD)",
           y = "Gross Monthly Salary (SGD)", 
           x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)), 
            text=element_text(size=12,face='bold'),
            legend.position = "none")+
      geom_label(aes(label=median_gross_monthly_median))
  })  
  
  # University view - bar charts 1 of latest median employment rate
  output$barchart1 <- renderPlotly({
    gb<-ggplot(data=bar1, aes(x=reorder(str_wrap(university,width=30),median_employment_rate_overall), 
                              y=median_employment_rate_overall, 
                              fill=university)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title="Latest Employment Rate by University",
           y = "Employment Rate", 
           x = "University")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)), 
            text=element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label=median_employment_rate_overall),size=3)+
      aes(text=paste("</br>",university,
                     "</br> Employment rate: ",median_employment_rate_overall))
    ggplotly(gb,tooltip = 'text') %>% style(hoverlabel=list(align='left'))
  })  
  
  # University view - bar chart 2 for latest gross median monthly
  output$barchart2 <- renderPlotly({
    gb<-ggplot(data=bar2, aes(x=reorder(str_wrap(university,width=30),median_gross_monthly_median), 
                              y=median_gross_monthly_median, 
                              fill=university)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title="Latest Gross Monthly Salary (SGD) by University", 
           y = "Gross Monthly Salary (SGD)", 
           x = "University")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)),
            text=element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label=median_gross_monthly_median),size=3)+
      aes(text=paste("</br>",university,
                     "</br> Gross Monthly Salary: ",median_gross_monthly_median))
    ggplotly(gb,tooltip = 'text') %>% style(hoverlabel=list(align='left'))
  })
  
  # University view - line charts 1 of YoY median employment rate
  output$linechart3 <- renderPlotly({
    gl <- ggplot(data=line3, aes(x=year, y=median_employment_rate_overall, color=university)) +
      geom_line(size=0.5)+
      geom_point(size=0.7)+
      labs(title="YoY Employment Rate by University", 
           y = "Employment Rate", 
           x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)), 
            text=element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label=median_employment_rate_overall),vjust=-.5,size=3) +
      aes(text=university)
    ggplotly(gl,tooltip='text')
  }) 
  
  # University view - line chart 2 for gross median monthly
  output$linechart4 <- renderPlotly({
    gl <- ggplot(data=line4, aes(x=year, y=median_gross_monthly_median, color=university)) +
      geom_line(size=0.5)+
      geom_point(size=0.7)+
      labs(title="YoY Gross Monthly Salary (SGD) by University",
           y = "Gross Monthly Salary(SGD)", 
           x = "Year")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)),
            text=element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label=median_gross_monthly_median),vjust=-.5,size=3) +
      aes(text=university)
    ggplotly(gl,tooltip='text')
  })
  
  # School view- bar charts 1 of latest median employment rate 
  output$barchart3 <- renderPlotly({
    gb <- ggplot(data=bar3, aes(x=reorder(str_wrap(Reclassified_School,width = 30),median_employment_rate_overall), 
                                y=median_employment_rate_overall, 
                                fill=Reclassified_School)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title = "Latest Employment Rate by Schools",
           y = "Employment Rate", 
           x = "Schools")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)),
            text = element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label = median_employment_rate_overall),size=3)+
      aes(text=paste("</br>",Reclassified_School,
                     "</br> Employment rate: ",median_employment_rate_overall))
    ggplotly(gb,tooltip = 'text') %>% style(hoverlabel=list(align='left'))
  })
  
  # School view - bar charts 2 for latest gross median monthly
  output$barchart4 <- renderPlotly({    
    gb <- ggplot(data=bar4, aes(x=reorder(str_wrap(Reclassified_School,width = 30),median_gross_monthly_median), 
                                y=median_gross_monthly_median, 
                                fill=Reclassified_School)) +
      geom_bar(stat="identity") + coord_flip()+
      labs(title="Latest Gross Monthly Salary (SGD) by Schools",
            y = "Gross Monthly Salary(SGD)",
            x = "Schools")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)),
            text=element_text(size=9,face='bold'),
            legend.position = "none")+
      geom_text(aes(label=median_gross_monthly_median),size=3)+
      aes(text=paste("</br>",Reclassified_School,
                     "</br> Gross Monthly Salary: ",median_gross_monthly_median))
    ggplotly(gb,tooltip = 'text') %>% style(hoverlabel=list(align='left'))
  })
  
  # School view - scatter chart of latest median employment rate and gross median monthly 
  output$scatterplot1 <- renderPlotly({
    gp <- ggplot(data=scat1, aes(x=median_employment_rate_overall, 
                                 y=median_gross_monthly_median, 
                                 color = Reclassified_School)) +
      geom_point(size=4)+
      labs(title="Latest Gross Median Salary (SGD) and Employment Rate by Schools",
            y = "Gross Monthly Salary(SGD)", 
            x = "Employment Rate",
            color="Schools")+
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color=	rgb(30, 0, 50,maxColorValue = 255)),
            text=element_text(size=9,face='bold'))+
      aes(text=paste('</br>',Reclassified_School,
                     '</br> Salary(month): ',median_gross_monthly_median,
                     '</br> Employment rate: ',median_employment_rate_overall))
    ggplotly(p=gp,tooltip = 'text')
  })
  
  # Details - data table
  output$datatable1 <- renderDT({
    plot_dat1<-filter(dat3, (Reclassified_University %in% input$select1 & year %in% input$select2 & Reclassified_School %in% input$select3 )
                      | (Reclassified_University %in% input$select1 & year %in% input$select2 & Reclassified_School %in% input$select3 & degree %in% input$select4)
                      )%>% 
      select(c("Reclassified_University","university","year","Reclassified_School","degree","employment_rate_overall","basic_monthly_median","gross_monthly_median")) %>% 
      rename(Uni = Reclassified_University, 
             School = Reclassified_School,
             University = university,
             Year=year,
             Degree=degree,
             `Overall Employment Rate` = employment_rate_overall,
             `Basic Salary (Month)` = basic_monthly_median,
             `Gross Salary (Month)` = gross_monthly_median) %>%
      dplyr::arrange(Uni,School,Degree,desc(Year))
    plot_dat1 %>%
      datatable(.) %>%
      formatStyle(columns=6, background = styleColorBar(range(0:100),'lightgreen',angle = -90))%>%
      formatStyle(columns=7, background = styleColorBar(range(0:plot_dat1[which.max(plot_dat1[,7]),7]),'lightblue',angle = -90))%>%
      formatStyle(columns=8, background = styleColorBar(range(0:plot_dat1[which.max(plot_dat1[,8]),8]),'lightsalmon',angle = -90))
  })
  
  # Course recommender - list of schools based on expected gross monthly median and graduation year
  output$datatable2 <- renderDT({
    plot_dat2<-dat3 %>%
      filter(year==2018 & Reclassified_University %in% input$select7)%>% 
      filter(!is.na(Reclassified_School)) %>% 
      mutate(future_gross_monthly_median = round(gross_monthly_median * (1.022^(input$select5 - 2018))),x=gross_monthly_median*2) %>% 
      filter(future_gross_monthly_median>=input$select6) %>% 
      arrange(desc(future_gross_monthly_median)) %>%
      select(c("Reclassified_University","university","Reclassified_School","degree","employment_rate_overall","gross_monthly_median","future_gross_monthly_median")) %>% 
      rename(Uni = Reclassified_University, 
             School = Reclassified_School, 
             University = university,
             Degree=degree,
             `Overall Employment Rate, 2018`= employment_rate_overall,
             `Gross Salary (Month), 2018`= gross_monthly_median,
             `Future Gross Salary (Month)` = future_gross_monthly_median)
    plot_dat2 %>%
      datatable(.) %>%
      formatStyle(columns=5, background = styleColorBar(range(0:100),'lightgreen',angle = -90))%>%
      formatStyle(columns=6, background = styleColorBar(range(0:plot_dat2[which.max(plot_dat2[,6]),6]),'lightblue',angle = -90))%>%
      formatStyle(columns=7, background = styleColorBar(range(0:plot_dat2[which.max(plot_dat2[,7]),7]),'salmon',angle = -90))
  })
}

##############################
# Run the application 
shinyApp(ui = ui, server = server)
