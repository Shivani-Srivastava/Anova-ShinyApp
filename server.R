
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("foreign")){install.packages("foreign")}
if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("psych")){install.packages("psych")}
if(!require("ggpubr")){install.packages("ggpubr")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("DT")){install.packages("DT")}

library(shiny)
library(foreign)
library(tidyverse)
library(psych)
library(pastecs)
library(tibble)
library(ggpubr)
library(ggplot2)

shinyServer(function(input, output) {
    
    
    myData <- reactive({
        inFile <- input$file
        if (is.null(inFile)) return(NULL)
        data <- read.csv(inFile$datapath, header = TRUE)
        data
    })
    
    output$yvarselect <- renderUI({
        if (is.null(input$file)) {return(NULL)}
        
        selectInput("yAttr", "Select Y variable",multiple = TRUE,
                    selectize = TRUE,
                    colnames(myData()))
        
    })
    
    output$xvarselect <- renderUI({
        if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)
        
        selectInput("xAttr", label = "Select X variables",multiple = TRUE,
                    selectize = TRUE,
                    selected = setdiff(colnames(myData()),input$yAttr),choices = setdiff(colnames(myData()),input$yAttr)
        )
        
    })
    
    output$fxvarselect <- renderUI({
        if (identical(myData(), '') || identical(myData(),data.frame())) return(NULL)
        
        selectInput("fxAttr", label = "Select X(Factor) variables",multiple = TRUE,
                    selectize = TRUE,
                    selected = setdiff(colnames(myData()),input$yAttr),choices = setdiff(colnames(myData()),input$yAttr)
        )
        
    })
    
    
    output$contents <- DT::renderDataTable({
        DT::datatable(head(myData()))       
    })
    
    output$summaryY <- renderPrint({
        df <- myData()
        print("Summary for Selected Y variable(s).")
        summary(df[,input$yAttr])
        
    })
    
    output$summaryX <- renderPrint({
        df <- myData()
        print("Summary for Selected X variable(s).")
        summary(df[,input$xAttr])
        
    })
    
    
    
    output$xref_tbl <- renderPrint(
        {if (length(input$fxAttr) > 1) {xref_tbl = table(input$fxAttr)}; 
            return(xref_tbl)}
    
    )
    
    output$OLSResult <- renderPrint({
        x <-input$xAttr
        y <- input$yAttr
        fx <- input$fxAttr
        
        # Reformulate command concatenates termlabels with + in between 
        # with response as dependent variable as a formula; 
        # to force factor variables to be read as.factor() 
        # is attached to the list of fx variables
        
        
        for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
        #f <- as.formula(paste(y, paste(x, collapse = '+'), sep = " ~ "))
        
        #f <- reformulate(termlabels = c(x), response = y)
        
        f <- paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+"))
        
        fit_ols <- summary(lm(f, myData()))
        
        print(fit_ols)
    })
    
    
    
    output$AnovaRes1 <- renderPrint({if(is.null(input$file)){return(print("File is null"))} else{
        if (length(input$yAttr) == 1){
            if (length(input$xAttr) ==1){
                cat('\n\n ----- Single Y and X: Running 1-way ANOVA -----\n\n')
                
                #n <- names(myData())
                #f <- as.formula(paste(y, "~", paste(n[!n %in% y], collapse = " + ")))
                
                
                x <-input$xAttr
                y <- input$yAttr
                fx <- input$fxAttr
                for (i1 in 0:length(fx)){fx[i1] <- paste('as.factor(', fx[i1], ')')}
                
                f <- reformulate(termlabels = c(x), response = y)
                
                fit_1way = aov(f, data = as.data.frame(myData()))
                Anova1 = summary(fit_1way)
                
                print(Anova1)
            }else{
                cat('\n\n ----- Single Y, many Xs: Running ANCOVA ------\n\n')
                
                x <-input$xAttr
                y <- input$yAttr
                fx <- input$fxAttr
                #for (i1 in 0:length(fx)){fx[i1] <- paste('as.factor(', fx[i1], ')')}
                for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
                #f <- as.formula(paste(y, "~", paste(fx[!fx %in% x], collapse = " + ")))
                f <- reformulate(termlabels = c(x), response = y)
                
                fit_ancova = aov(f, data = as.data.frame(myData()))
                
                a0 = car::Anova(fit_ancova, type="III")
                a0_df = as.data.frame(a0)
                colsum0 = sum(a0$`Sum Sq`)
                a0_df$percent_var_expl = 100*a0$`Sum Sq`/colsum0 
                a0_df = round(a0_df,3)
                Anova1 = a0_df 
                
                print(Anova1)
            }
        }else{
            cat('\n\n ----- Multiple Ys: Running MANOVA ------\n\n')
            x <-input$xAttr
            y <- input$yAttr
            fx <- input$fxAttr
            #for (i1 in 0:length(fx)){fx[i1] <- paste('as.factor(', fx[i1], ')')}
            
            Y <- as.matrix(cbind(myData()[,y]))
            
            X = myData()[,x]
            print(class(X))
            for (i0 in (which(x %in% fx == TRUE))){X[,i0] <- as.factor(X[,i0])}
            
            myData1 = data.frame(Y,X)
            
            #print(head(myData1))
            
            #for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
            
            #f <- reformulate(termlabels = c(x))
            #f <- str_split_fixed(f,'~',1)[2]
            
            #dependentV <- paste(colnames(Y), sep = "+")
            
            #f <- paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+"))
            
            #rhs = paste(x, collapse = "+")
            #print(rhs)
            #lhs = paste(input$yAttr, collapse = "+")
            #ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = mydata())
            
            fit_manova = manova(Y ~ X, data = as.data.frame(X))
            
            Anova1 = summary(fit_manova)
            print(Anova1)
        }
    
    }})
    
    
    
    
    output$Plot1 <- renderPlot({if(is.null(input$file)){return(NULL)} else{
        if (length(input$yAttr) == 1){
            if (length(input$xAttr) ==1){
                cat('\n\n ----- Single Y and X: Running 1-way ANOVA -----\n\n')
                #y = as.matrix(input$yAttr)
                
                x <- input$xAttr
                y <- input$yAttr
                fx <- input$fxAttr
                
                y_name = colnames(y)
                x_name = colnames(x)
                
                
                ggplot(as.data.frame(myData()), aes(myData()[,x], myData()[,y])) +
                    geom_boxplot() + labs(x=input$xAttr, y=input$yAttr)  
                
               
            }else{
                cat('\n\n ----- Single Y, many Xs: Running ANCOVA ------\n\n')
                
                n1 = length(input$fxAttr)
                library(ggpubr)
                plots_store_list = vector(mode="list", length=n1)          
                
                
                for (i0 in 1:n1){
                    x = input$fxAttr[i0] 
                    x_name = input$fxAttr[i0]
                    y_name = input$yAttr
                    plots_store_list[[i0]] = ggplot(as.data.frame(myData()), aes(myData()[,x[i0]], myData()[,y])) +
                    geom_boxplot() + labs(x=input$fxAttr[i0], y=input$yAttr)
                }
                
                plot_list <- ""
                for (i2 in 1:n1){
                    plot_list <- paste(plot_list, "plots_store_list[",i2,"]",",")
                }
                #ggarrange(plot_list)
                plot_list1 = paste0(plot_list, "#"); plot_list1 
                plot_list1 = str_replace_all(plot_list1, ",#", "") 
                ggarrange(plot_list1)
        }}
        else{
            cat('\n\n ----- Multiple Ys: Running MANOVA ------\n\n')
            #fit_manova = manova(as.matrix(input$yAttr) ~., data=input$xAttr)
            #Anova1 = summary(fit_manova)
            
            n1 = length(input$fxAttr)
            n2 = length(input$yAttr)
            plot_store_manova = vector(mode="list", length=n1*n2)
            i0 = 0
            for (i1 in 1:n2){
            y1 = y[,i1]; y_name = colnames(input$yAttr)[i1]
            for (i2 in 1:n1){
            x = as.factor(input$fxAttr[,i2])
            x_name = colnames(input$fxAttr)[i2]
            plot_store_manova[[i0 + 1]] = ggplot_aov(y1, x, y_name, x_name)
            i0 = i0 + 1        } # i2 loop ends
            
        } # i1 loop ends
        
        print(plot_store_manova)
        }}
    })
    
    output$downloadData <- downloadHandler(
        filename = function() { "mtcars.csv" },
        content = function(file) {
            write.csv(read.csv("data/mtcars.csv"), file, row.names=F, col.names=F)
        }
    )
    
    }
    
)
