#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("ggpubr")
#library("shiny")
#library("shiny","xlsx","dplyr","stringr","ggplot2","flextable","readxl","tidyr","ggridges","rmarkdown","RColorBrewer","directlabels","gridExtra"),#"ggpubr")
# pkgs <- c("dplyr","stringr","ggplot2","flextable","readxl","tidyr","ggridges","rmarkdown","RColorBrewer","directlabels","gridExtra")
# for (i in pkgs) {
#     #if (!i %in% installed.packages()) {
#         #install.packages(i)
#     #}
#     library(i, character.only = TRUE)
# }


library(shiny)
#library(xlsx)
library(dplyr)
library(ggplot2)
library(stringr)
library(flextable)
library(readxl)
library(tidyr)
library(ggridges)
library(directlabels)
library(gridExtra)
library(rmarkdown)
library(RColorBrewer)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(ggnewscale)
library(grid)




#../data
MyPalette<-c("#008dc9ff", "#d86422ff", "#20313bff", "#d4aa7dff", "#197278ff","#686868", "#f2545bff", "#90a9b7ff", "#224870ff", "#66a182ff", "#885053ff")

# StringencyIndex<-read.csv("input_to_update/StringencyIndex.csv") %>% mutate(Date=as.Date(parse_date_time(Date,c("dmy", "ymd","mdy")))) %>% 
#   mutate(ADM0NAME=if_else(ADM0NAME=='Bosnia and Herzegovina','Bosnia And Herzegovina',ADM0NAME)) %>% 
#   arrange(ADM0NAME)


LegendTimeLine<-read.csv('input_permanent/LegendTimeLine.csv')
  
# MainDataset<-read.csv("input_to_update/qry_covid_running_cases_country_date.CSV") %>%
#   mutate(ADM0NAME=str_to_title(ADM0NAME),
#          DateReport1=as.Date(parse_date_time(DateReport1,c("dmy", "ymd","mdy")))) %>%
#   left_join(StringencyIndex,by = c("ADM0NAME" = "ADM0NAME", "DateReport1" = "Date")) %>% 
#   mutate(ADM0NAME=if_else(ADM0NAME=='Kosovo','Kosovo(1)',ADM0NAME)) 



PopulationDataset<-read.csv('input_permanent/ref_Country.csv') %>% select(ADM0NAME,UNPOP2019) %>% mutate(ADM0NAME=str_to_title(ADM0NAME))




GlobalDataset_<-read.csv('input_to_update/GlobalDataset.csv') %>% mutate(DateReport1=as.Date(DateReport1)) %>% 
  mutate(ADM0NAME=if_else(ADM0NAME=='Kosovo','Kosovo(1)',ADM0NAME)) %>% 
  arrange(ADM0NAME) %>% 
  filter(!is.na(DateReport1))

CurrentDate<-max(unique(GlobalDataset_$DateReport1))

minDate<-function(country){
  Date <- min((GlobalDataset_ %>% filter(ADM0NAME==country))$DateReport1)
  return(Date)
}


Dataset_Cases_Normal<-GlobalDataset_ %>% select(ADM0NAME,DateReport1,GlobalIndex,Masks,Schools,Businesses,Gatherings,Movements,Borders,
                                                RealValue=NewCases,SplineValue=Spline_3DaysAverageCases)

Dataset_Deaths_Normal<-GlobalDataset_ %>% select(ADM0NAME,DateReport1,GlobalIndex,Masks,Schools,Businesses,Gatherings,Movements,Borders,
                                                 RealValue=NewDeaths,SplineValue=Spline_3DaysAverageDeaths)

Dataset_Cases_Log<-GlobalDataset_ %>% select(ADM0NAME,DateReport1,GlobalIndex,Masks,Schools,Businesses,Gatherings,Movements,Borders,
                                             RealValue=log_cases,SplineValue=Spline_3DaysAverage_logCases_)

Dataset_Deaths_Log<-GlobalDataset_ %>% select(ADM0NAME,DateReport1,GlobalIndex,Masks,Schools,Businesses,Gatherings,Movements,Borders,
                                              RealValue=log_deaths,SplineValue=Spline_3DaysAverage_logDeaths_)
  
Dataset_ToPlot<-function(ctr,Log,CasesOrDeaths,StartDate,EndDate){
  
  if (CasesOrDeaths=='Cases' & Log=='False'){
    Dataset<-Dataset_Cases_Normal
  }
  
  if (CasesOrDeaths=='Cases' & Log=='True'){
    Dataset<-Dataset_Cases_Log
  }
  
  if (CasesOrDeaths=='Deaths' & Log=='False'){
    Dataset<-Dataset_Deaths_Normal
  }
  
  if (CasesOrDeaths=='Deaths' & Log=='True'){
    Dataset<-Dataset_Deaths_Log
  }
  
  Dataset<-Dataset %>% filter(ADM0NAME == ctr) %>% 
    filter(DateReport1>=StartDate,DateReport1<=EndDate)
  
  return(Dataset)
}



BigPlot<-function(ctry,CasesOrDeaths,par1,RealValues,StartDate,EndDate,Log){
  

  CountryDataset<-Dataset_ToPlot(ctry,Log,CasesOrDeaths,StartDate,EndDate)
  
  if(CasesOrDeaths=="Cases")
  {ttl<-"Daily Cases"}
  if(CasesOrDeaths=="Deaths")
  {ttl<-"Daily Deaths"}

  #CountryDataset<-GlobalCountryDataset(ctry,CasesOrDeaths,par1,EndDate,Log)
  
  plot<-ggplot(CountryDataset)+
    theme_minimal()+
    labs(x="Date of report",y=ttl,linetype=1)+
    geom_line(aes(x=DateReport1,y=SplineValue),color='black',show.legend = FALSE)+
    scale_x_date(date_breaks = "15 days",date_labels =  "%d-%b",limits=c(StartDate,EndDate))+
    scale_y_continuous(position='left',sec.axis = dup_axis())+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.y.left=element_blank(),axis.title.y.right=element_blank())
    #scale_x_continuous(breaks=seq(-50,100,10))+
    #scale_y_continuous(labels=function(x) round(10^x))+
  
  if(Log=="True"){
    plot<-plot+scale_y_continuous(labels=function(x) round(10^x),sec.axis = dup_axis())}
  
  
  if(RealValues=="Yes" & Log=='True')
  {plot<-plot+geom_point(aes(x=DateReport1,y=RealValue),color='black',shape=3,alpha=0.5,size=0.5,show.legend=FALSE)}
  
  if(RealValues=="Yes" & Log=='False')
  {plot<-plot+geom_point(aes(x=DateReport1,y=RealValue),color='black',shape=3,alpha=0.5,size=0.5,show.legend=FALSE)}
  
  return(plot)
}

Max_14DaysIncidence<-function() {
  CurrentDate<-max(GlobalDataset_$DateReport1)
  FourteenDaysIncidence_Dataset_ <- data.frame()
  for (ctr in unique(GlobalDataset_$ADM0NAME)){
    FourteenDaysIncidence_Dataset<-data.frame(nrow=1)
    FourteenDaysIncidence_Dataset$ADM0NAME[1]<-as.character(ctr)
    FourteenDaysIncidence_Dataset$Cumul14Days[1]<-((GlobalDataset_ %>% filter(ADM0NAME==ctr & DateReport1==CurrentDate))$TotalCases - (GlobalDataset_ %>% filter(ADM0NAME==ctr & DateReport1==CurrentDate-14))$TotalCases)
    FourteenDaysIncidence_Dataset_<-bind_rows(FourteenDaysIncidence_Dataset,FourteenDaysIncidence_Dataset_)
  }
  FourteenDaysIncidence_Dataset_ <- FourteenDaysIncidence_Dataset_ %>% 
    left_join(PopulationDataset,by='ADM0NAME') %>% 
    mutate(Incidence=Cumul14Days/UNPOP2019*100000) %>% 
    arrange(desc(Incidence)) %>% filter(ADM0NAME %in% unique(GlobalDataset_$ADM0NAME)) %>% top_n(1)
  
  return(FourteenDaysIncidence_Dataset_$ADM0NAME)
}


Test<-data.frame(Text=c('Schools','Businesses','Gatherings','Movements','Borders','PHSM Severity Index'),
                 hor=c(0.5,0.5,0.5,0.5,0.5,0.5),
                 ver=c(0.16,0.32,0.48,0.64,0.70,0.86))

PlotTimeLine<-function(ctry,CasesOrDeaths,StartDate,EndDate,Log,Measure){

  CountryDataset<-Dataset_ToPlot(ctry,Log,CasesOrDeaths,StartDate,EndDate) %>% select(-c(SplineValue,RealValue))
  
  Dataset<-CountryDataset %>% 
    pivot_longer(cols=c(GlobalIndex:Borders)) %>% 
    mutate(Height=if_else(name=='GlobalIndex',100,50,50))
  
  if (Measure=='All measures together'){
  plot_timeline<-ggplot(Dataset,aes(x=DateReport1,y=Height))+
    geom_point(aes(color=value),alpha=0)+
    scale_color_gradient(high = 'black', low = 'white',breaks=c(0,100),labels=c('No measures','Most severe measures'))+
    scale_alpha_continuous(limits = c(0,100),range = c(0, 1))+
    geom_tile(stat='identity',aes(x=DateReport1,y=Height,alpha=value,fill=factor(name,levels=c('Masks',"Schools",'Businesses','Gatherings','Movements','Borders',"GlobalIndex"))),position='stack')+
    annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 50, label = "PHSM Severity Index",color='white')+
    annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 125, label = "International Travel",color='white')+
    annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 175, label = "Movements",color='white')+
    annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 225, label = "Gatherings",color='white')+
    annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 275, label = "Businesses",color='white')+
    annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 325, label = "Schools",color='white')+
    annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 375, label = "Masks",color='white')+
    annotate('text', x = EndDate, y=50, label = 'PHSM Severity Index', color='#04319D',hjust = 0)+
    annotate("text", x = EndDate, y = 125, label = "International Travel",color='#AD0C00',hjust=0)+
    annotate("text", x = EndDate, y = 175, label = "Movements",color='#0A698A',hjust=0)+
    annotate("text", x = EndDate, y = 225, label = "Gatherings",color='#A1078B',hjust=0)+
    annotate("text", x = EndDate, y = 275, label = "Businesses",color='#3C8E05',hjust=0)+
    annotate("text", x = EndDate, y = 325, label = "Schools",color='#D0C600',hjust=0)+
    annotate("text", x = EndDate, y = 375, label = "Masks",color='#E57E00',hjust=0)+
  scale_fill_manual(breaks=c('Masks',"Schools",'Businesses','Gatherings','Movements','Borders',"GlobalIndex"),
                    labels=c('Masks','Schools measures', 'Workplace measures','Restrictions on gatherings','Stay-at-home requirements', 'International travel restrictions','Global Severity Index'),
                    values=c('#E57E00','#D0C600','#3C8E05','#A1078B','#0A698A','#AD0C00','#04319D'))+
  scale_x_date(date_breaks = "15 days",date_labels =  "%d-%b",limits=c(StartDate,EndDate))+
  labs(y='Measure Severity',x='',color='PHSM Severity Index Scale')+
  scale_y_continuous(position='left')+
    coord_cartesian(clip = 'off')+
  guides(alpha=FALSE,fill=FALSE,nrow=2)+
  theme_minimal()+
  theme(plot.margin = unit(c(1,5,1,1),"lines"),axis.text.y=element_blank(),axis.text.x = element_text(angle = 90),axis.ticks.y=element_blank(),legend.title=element_text(vjust=0.85),legend.box="vertical",legend.position='bottom',legend.key.width = unit(1.5, "cm"),legend.spacing.y = unit(0.5, 'cm'),legend.spacing.x = unit(1, 'cm'))}
  
  if (Measure!='All measures together'){
    lbl<-case_when(Measure=='Schools'~ 'Schools',
                   Measure=='Masks' ~ 'Masks',
                   Measure=='Businesses' ~ 'Businesses',
                   Measure=='Gatherings' ~ 'Gatherings',
                   Measure=='Movements' ~ 'Movements',
                   Measure=='Borders' ~ 'International Travel')
    clr<-case_when(Measure=='Schools'~ '#D0C600',
                     Measure=='Masks' ~ '#E57E00',
                     Measure=='Businesses' ~ '#3C8E05',
                     Measure=='Gatherings' ~ '#A1078B',
                     Measure=='Movements' ~ '#0A698A',
                     Measure=='Borders' ~ '#AD0C00')
    
    Dataset<-Dataset %>% filter(name==Measure)
    
    plot_timeline<-ggplot(Dataset,aes(x=DateReport1,y=Height))+
      #geom_point(aes(color=value),alpha=0)+
      #scale_color_gradient(high = 'black', low = 'white',breaks=c(0,100),labels=c('No measures','Most severe measures'))+
      scale_alpha_continuous(limits = c(0,100),range = c(0, 1))+
      geom_tile(stat='identity',aes(x=DateReport1,y=Height,alpha=value),fill=clr)+
      annotate("text", x = StartDate+(EndDate-StartDate)*1.9/3, y = 50, label = lbl,color='white')+
      annotate('text', x = EndDate, y=50, label = lbl, color=clr,hjust = 0)+
      scale_x_date(date_breaks = "15 days",date_labels =  "%d-%b",limits=c(StartDate,EndDate))+
      labs(y='Measure Severity',x='',color='PHSM Severity Index Scale')+
      scale_y_continuous(position='left')+
      coord_cartesian(clip = 'off')+
      guides(alpha=FALSE,fill=FALSE,nrow=2)+
      theme_minimal()+
      theme(plot.margin = unit(c(1,5,1,1),"lines"),axis.text.y=element_blank(),axis.text.x = element_text(angle = 90),
            axis.ticks.y=element_blank(),legend.title=element_text(vjust=0.85),legend.box="vertical",
            legend.position='bottom',legend.key.width = unit(1.5, "cm"),legend.spacing.y = unit(0.5, 'cm'),
            legend.spacing.x = unit(1, 'cm'))}
  
  return(plot_timeline)
  }


TrickLegend<-ggplot(LegendTimeLine)+
  geom_tile(stat='identity',aes(x=x,y=Height,alpha=Alpha,fill=factor(Index,levels=c('Masks',"Schools",'Businesses','Gatherings','Movements','Borders',"GlobalIndex"))),position='stack')+
  scale_fill_manual(breaks=c('Masks','Schools','Businesses','Gatherings','Movements','Borders','GlobalIndex'),
                    labels=c('Masks','Schools measures', 'Workplace measures', 'Restrictions on gatherings', 'Stay-at-home requirements', 'International travel restrictions','Global Severity Index'),
                    values=c('#E57E00','#D0C600','#3C8E05','#A1078B','#0A698A','#AD0C00','#04319D'))+
  theme_void()+
  theme(legend.position="none",plot.title=element_text(hjust=0.5,size=10))+
  scale_y_continuous(limits=c(-5,NA))+
  scale_x_continuous(limits=c(-100,200))+
  annotate("text", x = -10, y = -3, label = "No measures")+
  annotate("text", x = 110, y = -3, label = "Most severe measures")+
  scale_alpha_continuous(range = c(0, 1))+
  labs(title='PHSM Severity Index Scale')
  


css <- 
  "
#dd {font-size: 10px;valign=0}
#DD {font-size: 10px;valign=0;font-weight:bold}
#tab {background-color:#EEF0F2;padding:10px;margin:10px 100px 5px 100px;border-radius: 10px}
#internaltab {background-color:white;padding:2px 5px 0px 5px;border-radius: 10px}
#window {background-color:white;padding:5px 5px 5px 5px;margin:5px 5px 5px 5px;border-radius: 5px;}
"


#Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(css),
    tags$style(HTML("
      p {font-size:8pt},

      .selectize-input {
        font-size: 8pt;
        padding: 1px;
        min-height: 10px;
      }
      
      .selectize-dropdown { font-size: 10px; line-height: 10px; padding:0}
      
      .shiny-date-input {
        font-size: 8pt;
        padding: 1px;
        min-height: 20px;
      }
      
      .form-control  {font-size:10px; height: 30px}
      
      

      "))
  ),
  
  
  
  titlePanel(h4("Country Analysis: Daily Cases and Deaths over Severity of Public Health and Social Measures (PHSM)")),
  paste0('Last updated on ',format(max(GlobalDataset_$DateReport1),'%d-%m-%Y')),
  plotOutput("BeautifulPlot",height=400),
  
  br(),
  # Application title
  
  # Sidebar with a slider input for number of bins 
  div(id='tab',fluidRow(
        column(4,
           div(id="dd",selectInput("country","Select the country",unique(GlobalDataset_$ADM0NAME),multiple=FALSE,selected=Max_14DaysIncidence())),
           div(id="dd",selectInput("Measure","Select the PHSM of interest",c("All measures together",'Masks',"Schools","Movements","Gatherings","Businesses","International Travel"),multiple=FALSE)),
           #div(id="dd",dateInput("cutDate","Select the maximum date:",value=max(MainDataset$DateReport1))),
           div(id="dd",uiOutput("slider"))),
        
        column(4,
           div(id="dd",selectInput("CasesOrDeaths","Cases or Deaths",c("Cases","Deaths"),multiple=FALSE)),
           div(id="dd",selectInput("scale","Select the scale to display the chart",c("Normal scale","Log scale"))),
           p("The chart displays by default smoothed curves built on reported values."),
           div(id="dd",checkboxInput("RealVal","Display reported (unsmoothed) values as well",FALSE))),
        
        column(4,div(id="DD",'Legend'),
               div(id='window',plotOutput("Legend",height=75))))),
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
 
  RealValues <- reactive({
    if(input$RealVal==TRUE)
    {"Yes"}
    else{"No"}
  })
  

  Log <- reactive({
    if(input$scale=="Log scale")
    {"True"}
    else{"False"}
  })
  
  Measure<- reactive({
    case_when(input$Measure=='Schools'~ 'Schools',
              input$Measure=='Masks' ~ 'Masks',
              input$Measure=='Businesses' ~ 'Businesses',
              input$Measure=='Gatherings' ~ 'Gatherings',
              input$Measure=='Movements' ~ 'Movements',
              input$Measure=='International Travel' ~ 'Borders',
              input$Measure=='All measures together' ~ 'All measures together')
  })
  
  output$slider <- renderUI({
    dateRangeInput("date_range","Date Range",start=minDate(input$country), end=CurrentDate)
  })
  
  dimensions <- reactive({
    if (input$Measure=='All measures together'){
      c(6,4)}
    else {c(6,2)}
  })
  
  
  
  output$BeautifulPlot<-renderPlot({
    shiny::validate(
      need(length(input$date_range[1])>0, 'Loading...'))
    
    plot<-BigPlot(input$country,input$CasesOrDeaths,0.5,RealValues(),input$date_range[1],input$date_range[2],Log())

    plot_timeline<-PlotTimeLine(input$country,input$CasesOrDeaths,input$date_range[1],input$date_range[2],Log(),Measure())
    
    plots_option2<-plot_grid(plot+theme(legend.position='none'),plot_timeline+theme(legend.position='none'),align='v',axis='lr',nrow=2,rel_heights=dimensions())
    #legends_option2<-plot_grid(get_legend(plot_timeline))
    #plot_legend<-plot_grid(TrickLegend)
    #plot_grid(plots_option2,plot_legend,nrow=2,rel_heights=c(8,2),rel_widths=c(10,5))
    plot_grid(plots_option2)
  })
  
  output$Legend<-renderPlot({
    TrickLegend
  })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
