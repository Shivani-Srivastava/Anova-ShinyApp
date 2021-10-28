
library(shiny)
library(foreign)
library(tidyverse)
library(psych)
library(pastecs)
library(ggpubr)

shinyUI(fluidPage(

    headerPanel('Analysis of Variance'),
    titlePanel(title = div(img(src="logo.png",align='right')), "ANOVA"),

    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload input data (csv file with header)", placeholder = "mtcars.csv"),  
            
            htmlOutput("yvarselect"),
            htmlOutput("xvarselect"),
            htmlOutput("fxvarselect"),
            
        ),

        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        tabPanel("Overview",
                                 
                                 h4(p("Background on ANOVA")),
                                 p("The analysis of variance can be used to describe otherwise complex 
  relations among variables. Analysis of variance (ANOVA) is a collection of 
  statistical models and their associated estimation procedures 
  (such as the variation among and between groups) used to analyze the differences
  among means. ANOVA was developed by the statistician Ronald Fisher. 
  ANOVA is based on the law of total variance, where the observed variance in a 
  particular variable is partitioned into components attributable 
  to different sources of variation. In its simplest form, ANOVA provides a 
  statistical test of whether two or more population means are equal, 
  and therefore generalizes the t-test beyond two means. "),
                                 br(),
                                 h4(p("Download Sample Input File")),
                                 downloadButton('downloadData', 'Download Example file'),
                                 p("Please note that download will not work with RStudio interface. 
  Download will work only in web-browsers. So open this app in a web-browser
  and then download the example file. 
  For opening this app in web-browser click on 
  \"Open in Browser\" as shown below -"),
                                 img(src = "example1.png")
                                 
                        ),
                        
            tabPanel("Data", DT::dataTableOutput('contents')),
            
            tabPanel("Summary", 
                     h4(p("Summary for Chosen Y variable(s):")),
                     verbatimTextOutput('summaryY'), 
                     h4(p("Summary for Chosen X Variable(s):")),
                     DT::dataTableOutput('summaryX'), 
                     
                     h4(p("Cross Reference Table:")),
                     verbatimTextOutput('Xref_Table')),
            
            tabPanel("Regression Results",
                     h4(p("Anova Results")),
                     DT::dataTableOutput('AnovaRes1'),
                     h4(p("OLS Results")),
                     DT::dataTableOutput('OLSResult')),
            
            tabPanel("Visualization"
                     ,plotOutput('Plot1')
                     )
        )
    )
)))
