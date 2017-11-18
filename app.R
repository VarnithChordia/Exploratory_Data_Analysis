
library(shiny)
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(pastecs)
library(psych)
library(corrplot)
library(gridExtra)
library(plotly)
library(descr)
library(e1071)

ui<-fluidPage(
  navbarPage(
    "Exploratory Data Analysis",
    tabPanel('Instuctions',mainPanel(
      h1("Workings of the Exploratory data analysis App:"),
      br(),
      div("The exploratory data analysis is a step prior to model building phase.Summarizing the main characteristics with visual methods helps the modeler in making more informed decisions.
          The laborious task of producing aggregations,distributions and applying general statistical methods is an important step, help improve accuracy of predictions several fold. This application uses 
          visualizations and descriptive statistics to explore relationship within the dataset. The application contains the following tabs -"),
      br(),
      strong("1.Load File-"),
      span('Upload a file,of maximum size 30 Mb.Choose an appropriate seperator and click on check boxes'),
      strong('- Headers & StringsAsFactors.'),
      span('Then click on Submit!'),
      span(span('This opens three tabs-'),strong('(i) About the file-'),span('Location and Size'),strong('(ii) Data-'),span('Displaying the top few observations'),strong('(iii) Convert_Variables-'),span('Converts selected numeric variables to factors.')),
      div(strong("2.Summary-"),span('This provides a missing value plot and summary statistics of variables in the dataset.')),
      div(strong('3.Univariate Analysis-'),span('This tab displays distributions of variables present in the dataset.Density plot for numeric variables and bar charts for factor variables.A grid plot is made,with a maximum of 4 different plots.')),
      div(strong('4.Binning-'),span('In this tab transformation of continous variables into binned variables with equally sized bins is possible.Using percentile cutoffs,the variable can be split.Binning is useful for outlier treatment and strength of association tests.')),
      div(strong('5.Bivariate_Analysis-'),span('Explores relationship between two variables at a time,producing scatterplots,boxplots,stacked-barcharts and correlation analysis.This mode also creates cross tables and chisq statistic')),
      div(strong('6.Cluster_Analysis-'),span('K-means clustering can be performed on the dataset.This is a partioning approach,where the data are partitioned based on iterative algorithm.Clusters must be pre specified before.'))
      
      )
      
      
    ),
    tabPanel("Load File",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
                 helpText("Default max. file size is 30MB"),
                 tags$hr(),
                 h5(helpText("Select the read.table parameters below")),
                 checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                 checkboxInput(inputId = "stringAsFactors", "StringAsFactors", FALSE),
                 br(),
                 radioButtons(inputId = 'sep', label = 'Separator', 
                              choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                 actionButton("goButton","Submit!")
               ),
               mainPanel(
                 uiOutput("tb")
               )
             )
             
    ),
    
    tabPanel("Summary",
             mainPanel(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               plotOutput("EDA4"),
               br(),
               br(),
               dataTableOutput("TA")
             )
    ),
    
    
    
    
    
    tabPanel("Univariate analysis",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("Univariate")
               ),
               mainPanel(
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ),
                 plotOutput("a")
               ))
    ),
    
    
    tabPanel("Binning",
             sidebarLayout(
               sidebarPanel("Binning Parameters",
                            uiOutput("Binning")
                            
                            
               ),
               mainPanel(
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ),
                 plotOutput("hist"),
                 tableOutput("Table1")
               ))
    ),
    
    
    
    tabPanel("Bivariate Analysis",
             tabsetPanel(type="tabs",tabPanel("Plot",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  uiOutput("Bivariate"),
                                                  uiOutput("FVariable")
                                                ),
                                                
                                                mainPanel(
                                                  tags$style(type="text/css",
                                                             ".shiny-output-error { visibility: hidden; }",
                                                             ".shiny-output-error:before { visibility: hidden; }"
                                                  ),
                                                  plotlyOutput("EDA1"),
                                                  br(),
                                                  br(),
                                                  verbatimTextOutput("tt1"),
                                                  tableOutput("EDA2"),
                                                  br(),
                                                  plotlyOutput("EDA3")
                                                  
                                                  
                                                ))),
                         tabPanel("Correlation Analysis",
                                  mainPanel(
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"
                                    ),
                                    plotOutput("Corr")
                                    
                                    
                                  )
                                  
                         )
                         
             )),
    
    
    
    tabPanel("K-Means Clustering",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("Cluster")
               ),
               mainPanel(
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ),
                 plotOutput("Clusterplot")
               ))
    )
    
  ))



options(shiny.maxRequestSize=50*1024^2)

server <- function(input, output) {
  
  values <- reactiveValues(df_data = NULL)
  
  observeEvent(input$goButton, {
    file1<-input$file
    if(is.null(file1)){return()} 
    values$df_data <- read.table(file=file1$datapath,sep=input$sep,header=input$header,stringsAsFactors = input$stringAsFactors)
    
  })
  
  observeEvent(input$goButton1, {
    values$df_data[,which(colnames(values$df_data) %in% input$var3)]<-lapply(values$df_data[,which(colnames(values$df_data) %in% input$var3)],function(x){as.factor(as.character(x))})
    
    
  })
  
  output$filedf<-renderTable({
    if(is.null(values$df_data)){return()}
    input$file
  })
  
  output$table<-renderTable({
    if(is.null(values$df_data)){return()}
    head(values$df_data)
  })
  
  output$text1<-renderPrint({
    if(is.null(values$df_data)){return()}
    if (input$goButton1){
      str(values$df_data)
    }
  })
  
  
  output$tb<-renderUI({
    if (input$goButton){
      if(is.null(values$df_data)){return()}
      else
        tabsetPanel("",tabPanel("About file",tableOutput("filedf")),
                    tabPanel("Data",tableOutput("table")),tabPanel("Convert_Variables","",
                                                                   sidebarLayout(
                                                                     sidebarPanel(
                                                                       selectizeInput('var3',label='Variables to be converted to factors',choices=names(values$df_data),multiple=T),
                                                                       actionButton("goButton1","Submit!")
                                                                     ),
                                                                     mainPanel(
                                                                       verbatimTextOutput("text1")
                                                                     )))
        )
    }
  })
  
  
  numeric_df<-reactive({
    t_numeric<-values$df_data[sapply(values$df_data,function(x)is.numeric(x))]
    t_numeric
  })
  
  factor_df<-reactive({
    t_factor<-values$df_data[sapply(values$df_data,function(x)is.factor(x))]
    t_factor[,sapply(t_factor,function(x){length(unique(x))<7})]
  })
  
  ##Missing_Value
  
  Missing_data<-reactive({
    if(is.null(values$df_data)){return()}
    x<-as.data.frame(apply(values$df_data,2,function(x)(sum(is.na(x))/nrow(values$df_data))))
    colnames(x)[1]<-"Missing_Value_percent"
    x$variables<-rownames(x)
    x<-x %>% filter(Missing_Value_percent>0)
    x
  })
  
  
  
  output$EDA4<-renderPlot({  
    if(is.null(values$df_data)){return()}
    
    p<-ggplot(Missing_data(),aes(reorder(x=variables,Missing_Value_percent),y=Missing_Value_percent))+geom_col(fill="red") +coord_flip()+
      theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1),
            axis.text.x = element_text(vjust = 0),plot.title = element_text(hjust = 0.5))+
      labs(title = "Variable vs Missing Value",x="Variables")+scale_y_continuous(labels=scales::percent)+
      geom_text(aes(y = Missing_Value_percent+.005,    
                    label = paste0(round(Missing_Value_percent*100,2), '%')),
                size = 5)
    
    print(p)
  })
  
  
  
  output$TA<-renderDataTable({
    if(is.null(values$df_data)){return()}
    s<-psych::describe(numeric_df())
    s$vars<-rownames(s)
    s
  })
  
  ##Univariate
  
  output$Univariate<-renderUI({
    if (is.null(values$df_data)){return()}
    if  (input$goButton){
      selectizeInput('vars',label='Select A Variable',choices=names(values$df_data),multiple=T,selected=NULL)
    }
  })
  
  
  
  p1<-reactive({
    if(is.null(values$df_data)){return()}
    if(is.factor(values$df_data[,which(colnames(values$df_data) %in% input$vars[1])])){
      ggplot(values$df_data,aes_string(input$vars[1]))+geom_bar(fill="#4271AE",color="#1F3552",aes(y = (..count..)/sum(..count..)))+
        labs(title=paste("BarChart of",input$vars[1]),y="Percent")+
        theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 25))+scale_y_continuous(labels=scales::percent)
    }
    else {
      ggplot(values$df_data,aes_string(input$vars[1]))+geom_histogram(fill="#4271AE",color="#1F3552",aes(y=..density..))+
        theme_classic()+labs(title=paste("Density plot of",input$vars[1]),y="Density")+
        theme(plot.title = element_text(hjust = 0.5)) +geom_line(aes(y=..density..), color='red', lwd = 1, stat = 'density')+
        stat_function(fun=dnorm,color = 'red',args=list(mean=mean(input$vars[1],na.rm=T),sd=sd(input$vars[1],na.rm=T)))+
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma) 
    }
  })
  
  p2<-reactive({
    if (length(input$vars)>1){
      if(is.factor(values$df_data[,which(colnames(values$df_data) %in% input$vars[2])])){
        ggplot(values$df_data,aes_string(input$vars[2]))+geom_bar(fill="#4271AE",color="#1F3552",aes(y = (..count..)/sum(..count..)))+
          labs(title=paste("BarChart of",input$vars[2]),y="Percent")+
          theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 25))+
          scale_y_continuous(labels=scales::percent)
      }
      
      else{
        ggplot(values$df_data,aes_string(input$vars[2]))+geom_histogram(fill="#4271AE",color="#1F3552",aes(y=..density..))+
          theme_classic()+labs(title=paste("Density plot of",input$vars[2]),y="Density")+
          theme(plot.title = element_text(hjust = 0.5))+geom_line(aes(y=..density..), color='red', lwd = 1, stat = 'density')+
          stat_function(fun=dnorm,color = 'red',args=list(mean=mean(input$vars[2],na.rm=T),sd=sd(input$vars[2],na.rm=T)))+
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(labels = scales::comma) 
      }
    }
    else{
      return(NULL)
    }
  })
  
  
  p3<-reactive({
    if (length(input$vars)>2){
      
      if(is.factor(values$df_data[,which(colnames(values$df_data) %in% input$vars[3])])){
        ggplot(values$df_data,aes_string(input$vars[3]))+geom_bar(fill="#4271AE",color="#1F3552",aes(y = (..count..)/sum(..count..)))+
          labs(title=paste("BarChart of",input$vars[3]),y="Percent")+
          theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 25))+scale_y_continuous(labels=scales::percent)
      }
      
      else{
        ggplot(values$df_data,aes_string(input$vars[3]))+geom_histogram(fill="#4271AE",color="#1F3552",aes(y=..density..))+
          theme_classic()+labs(title=paste("Density plot of",input$vars[3]),y="Density")+
          theme(plot.title = element_text(hjust = 0.5))+geom_line(aes(y=..density..), color='red', lwd = 1, stat = 'density')+
          stat_function(fun=dnorm,color = 'red',args=list(mean=mean(input$vars[3],na.rm=T),sd=sd(input$vars[3],na.rm=T)))+
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(labels = scales::comma) 
      }
    }
    else{
      return(NULL)
    }
    
  })
  
  
  
  p4<-reactive({
    if (length(input$vars)>3){
      if(is.factor(values$df_data[,which(colnames(values$df_data) %in% input$vars[4])])){
        ggplot(values$df_data,aes_string(input$vars[4]))+geom_bar(fill="#4271AE",color="#1F3552",aes(y = (..count..)/sum(..count..)))+
          labs(title=paste("BarChart of",input$vars[4]),y="Percent")+
          theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 25))+scale_y_continuous(labels=scales::percent) 
      }
      else{
        ggplot(values$df_data,aes_string(input$vars[4]))+geom_histogram(fill="#4271AE",color="#1F3552",aes(y=..density..))+
          scale_x_continuous(labels = scales::comma)+
          theme_classic()+labs(title=paste("Density plot of",input$vars[4]),y="Density")+
          theme(plot.title = element_text(hjust = 0.5))+geom_line(aes(y=..density..), color='red', lwd = 1, stat = 'density')+
          stat_function(fun=dnorm,color = 'red',args=list(mean=mean(input$vars[4],na.rm=T),sd=sd(input$vars[4],na.rm=T)))+
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(labels = scales::comma) 
      }
    }
    
    else{
      return(NULL)
    }
  })
  
  
  
  
  output$a<-renderPlot({
    ptlist<-list(p1(),p2(),p3(),p4())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
    if (length(ptlist)<=2) {
      grid.arrange(grobs=ptlist,ncol=length(ptlist))
    }
    else{
      grid.arrange(grobs=ptlist,ncol=2,nrow=2)
    }
  },height = 600,width=800)
  
  
  
  ##Binning
  
  output$Binning<-renderUI({
    if (is.null(values$df_data)){return()}
    if  (input$goButton){
      tagList(selectizeInput('Numerics',label='Select A Variable',choices=names(numeric_df()),selected=NULL),
              sliderInput('Slide',label='Select the number of equally sized buckets te create',min=2,max=10,value=2,step=1)
      )
      
    }
  })
  
  
  output$hist<-renderPlot({
    if(is.null(values$df_data)){return()}
    a<-quantile(values$df_data[,input$Numerics],prob=seq(0,1,length=input$Slide+1),na.rm = T)
    p<-ggplot(values$df_data,aes_string(input$Numerics))+geom_histogram(fill="#4271AE",color="#1F3552")+
      geom_vline(xintercept =a[2:(length(a)-1)],color="red",alpha=1)+scale_x_continuous(labels = scales::comma)+
      theme_classic()+labs(title=paste("Histogram of",input$Numerics),y="Frequency")+
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  )
  
  
  
  
  output$Table1<-renderTable({
    if(is.null(values$df_data)){return()}
    d<-values$df_data[,which(colnames(values$df_data) %in%input$Numerics)]
    a<-quantile(d,prob=seq(0,1,length=input$Slide+1),na.rm = T)
    q<-cut2(d,cuts=as.numeric(a))
    binder<-as.data.frame(cbind(d,q))
    s<-binder %>% group_by(q) %>% summarise(Bin.start=min(d),Bin.end=max(d),Num_of_Obs=n(),Prop_of_Obs=(n()/nrow(values$df_data))*100) %>% rename(Bin=q)
    s
  }
  )
  
  
  
  ##Bivariate
  
  output$Bivariate<-renderUI({
    if (is.null(values$df_data)){return()}
    if  (input$goButton){
      selectizeInput('var1',label='Select any two  variables',multiple=T,choices=names(values$df_data))
    }
  })
  
  
  output$FVariable<-renderUI({
    if (is.null(values$df_data)){return()}
    if  (input$goButton){
      if((is.numeric(eval(Final_train1()))==TRUE) & (is.factor(eval(Final_train2()))==TRUE)){
        selectizeInput('vari',label='Select a factor Variable',choices=names(factor_df()),selected=NULL)
      }
      if((is.factor(eval(Final_train1()))==TRUE) & (is.numeric(eval(Final_train2()))==TRUE)){
        selectizeInput('vari',label='Select a factor Variable',choices=names(factor_df()),selected=NULL)
      }
    }
  })
  
  Final_train1<-reactive({values$df_data[,which(colnames(values$df_data)==input$var1[1])]})
  Final_train2<-reactive({values$df_data[,which(colnames(values$df_data)==input$var1[2])]})
  
  output$EDA1 <- renderPlotly({
    
    
    
    if((is.numeric(eval(Final_train1()))==TRUE) & (is.numeric(eval(Final_train2()))==TRUE)){
      
      
      
      
      
      a<-ggplot(values$df_data,aes_string(input$var1[1],input$var1[2]))+geom_point()+theme(plot.subtitle = element_text(vjust = 1), 
                                                                                           plot.caption = element_text(vjust = 1), panel.background = element_rect(fill = "antiquewhite"),
                                                                                           panel.grid.major = element_line(colour = "gray98"),panel.grid.minor = element_line(colour = "gray98")) +
        labs(title=paste(input$var1[1],"vs",input$var1[2],sep=" ")) +scale_y_continuous(labels = scales::comma)+scale_x_continuous(labels = scales::comma)
      
      
      print(ggplotly(a))
      
      
    }
    
    else if((is.factor(eval(Final_train1()))==TRUE) & (is.numeric(eval(Final_train2()))==TRUE)) {
      
      a<-ggplot(values$df_data,aes_string(input$var1[1],input$var1[2]))+geom_boxplot()+theme(plot.subtitle = element_text(vjust = 1), 
                                                                                             plot.caption = element_text(vjust = 1), panel.background = element_rect(fill = "antiquewhite"),
                                                                                             panel.grid.major = element_line(colour = "gray98"),
                                                                                             panel.grid.minor = element_line(colour = "gray98"),axis.text.x = element_text(angle = 25))+
        labs(title=paste(input$var1[1],"vs",input$var1[2],sep=" ")) +scale_y_continuous(labels = scales::comma)
      
      print(ggplotly(a))
      
      
    }
    
    
    
    else if((is.numeric(eval(Final_train1()))==TRUE) & (is.factor(eval(Final_train2()))==TRUE)) {
      
      a<-ggplot(values$df_data,aes_string(input$var1[2],input$var1[1]))+geom_boxplot()+
        theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), 
              panel.background = element_rect(fill = "antiquewhite"),panel.grid.major = element_line(colour = "gray98"),
              panel.grid.minor = element_line(colour = "gray98"),axis.text.x = element_text(angle = 25)) +
        labs(title=paste(input$var1[2],"vs",input$var1[1],sep=" ")) +scale_y_continuous(labels = scales::comma)
      
      print(ggplotly(a))
    }
    
    else if((is.factor(eval(Final_train1()))==TRUE) & (is.factor(eval(Final_train2()))==TRUE)) {
      
      a<-ggplot(values$df_data,aes_string(input$var1[1],fill=input$var1[2]))+geom_bar(stat="count",position ="stack")+theme(plot.subtitle = element_text(vjust = 1), 
                                                                                                                            plot.caption = element_text(vjust = 1), panel.background = element_rect(fill = "antiquewhite"),
                                                                                                                            panel.grid.major = element_line(colour = "gray98"),panel.grid.minor = element_line(colour = "gray98"),
                                                                                                                            axis.text.x = element_text(angle = 25))+labs(title=paste(input$var1[2],"vs",input$var1[1],sep=" "),y="Frequency")+scale_y_continuous(labels = scales::comma)
      
      print(ggplotly(a))
      
      
    }  
    
    
    
    
    
  })
  
  
  
  
  output$Corr<-renderPlot({
    corrplot(cor(na.omit(numeric_df())),method="number")
    
  },height =800,width=1000)
  
  
  output$EDA2<-renderTable({
    
    Final_train<-reactive({
      t<-data.frame(Final_train1(),Final_train2())
      t
    })
    
    
    if((is.factor(eval(Final_train1()))==TRUE) & (is.numeric(eval(Final_train2()))==TRUE)) {
      
      Final_train() %>% dplyr::group_by(Final_train1..) %>% dplyr::summarise(.,Min=min(Final_train2..,na.rm=T),Max=max(Final_train2..,na.rm=T),
                                                                             Mean=mean(Final_train2..,na.rm=T),Median=median(Final_train2..,na.rm=T),Sum=sum(Final_train2..,na.rm=T),count=n())
      
    }
    
    else if((is.numeric(eval(Final_train1()))==TRUE) & (is.factor(eval(Final_train2()))==TRUE)) {
      
      Final_train() %>% dplyr::group_by(Final_train2..) %>% dplyr::summarise(.,Min=min(Final_train1..,na.rm=T),Max=max(Final_train1..,na.rm=T),
                                                                             Mean=mean(Final_train1..,na.rm=T),Median=median(Final_train1..,na.rm=T),Sum=sum(Final_train1..,na.rm=T),count=n())
      
      
    }
    
    
  })
  
  output$EDA3 <- renderPlotly({
    
    if((is.factor(eval(Final_train1()))==TRUE) & (is.numeric(eval(Final_train2()))==TRUE)) {
      
      a<-ggplot(values$df_data,aes_string(input$var1[1],input$var1[2],fill=input$vari))+geom_bar(stat="identity",position ="stack")+theme(plot.subtitle = element_text(vjust = 1), 
                                                                                                                                          plot.caption = element_text(vjust = 1), panel.background = element_rect(fill = "antiquewhite"),panel.grid.major = element_line(colour = "gray98"),
                                                                                                                                          panel.grid.minor = element_line(colour = "gray98"),axis.text.x = element_text(angle = 25))+
        labs(title=paste(input$var1[2],"vs",input$var1[1],sep=" "))+
        scale_y_continuous(labels = scales::comma)
      
      print(ggplotly(a))
    }
    
    else if((is.numeric(eval(Final_train1()))==TRUE) & (is.factor(eval(Final_train2()))==TRUE)) {
      
      a<-ggplot(values$df_data,aes_string(input$var1[2],input$var1[1],fill=input$vari))+geom_bar(stat="identity",position ="stack")+theme(plot.subtitle = element_text(vjust = 1), 
                                                                                                                                          plot.caption = element_text(vjust = 1), panel.background = element_rect(fill = "antiquewhite"),panel.grid.major = element_line(colour = "gray98"),
                                                                                                                                          panel.grid.minor = element_line(colour = "gray98"),axis.text.x = element_text(angle = 25))+
        labs(title=paste(input$var1[2],"vs",input$var1[1],sep=" "))+
        scale_y_continuous(labels = scales::comma)
      
      print(ggplotly(a))
    }
    
    
    
    
  })
  
  output$Corr<-renderPlot({
    corrplot(cor(na.omit(numeric_df())),method="number")
    
  },height =1000,width=1200)
  
  
  output$tt1<-renderPrint({
    if((is.factor(eval(Final_train1()))==TRUE) & (is.factor(eval(Final_train2()))==TRUE)){
      K<-crosstab(Final_train1(),Final_train2(),prop.r = T,prop.c = T,chisq = T)
      K
    }
  })
  
  output$Cluster<-renderUI({
    if (is.null(values$df_data)){return()}
    if  (input$goButton){
      tagList( selectizeInput('XCluster',label='Select X axis Variable',choices=names(numeric_df()),selected=NULL),
               selectizeInput('YCluster',label='Select Y axis Variable',choices=names(numeric_df()),selected=NULL),
               sliderInput('ClusterSlide',label='Select the number of clusters',min=2,max=9,value=2,step=1))  
    }
  })
  
  
  clust_data<-reactive({
    na.omit(numeric_df()[,which(colnames(numeric_df()) %in% c(input$XCluster,input$YCluster))])
    
  })
  
  
  
  clust<-reactive({
    
    clusters<-kmeans(clust_data()[,which(colnames(clust_data()) %in% c(input$XCluster,input$YCluster))],centers=input$ClusterSlide)
    a<-data.frame(clusters$cluster)
    colnames(a)<-"Cluster_Type"
    a
  })
  
  
  
  clust_data1<-reactive({
    s<-clust_data()
    c<-clust()
    cbind(s,c) 
    
  })
  
  
  output$Clusterplot<-renderPlot({
    w<-factor(clust_data1()$Cluster_Type)
    names(w)<-"Cluster"
    ggplot(clust_data1(),aes_string(input$XCluster,input$YCluster,color=w))+geom_point()+labs(color="Cluster")+theme_classic()
  })
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)



