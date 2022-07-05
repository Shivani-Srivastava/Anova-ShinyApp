
library(shiny)
library(foreign)
library(tidyverse)
library(psych)
library(pastecs)
library(tibble)
library(ggpubr)
library(DT)
library(ggplot2)
library(ggplotify)

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
        
        selectInput("fxAttr", label = "Select X(Non-Metric) variables",multiple = TRUE,
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
    
    output$summaryX <- DT::renderDataTable({
        df <- myData()
        print("Summary for Selected X variable(s).")
        #summary(df[,input$xAttr])
        
        DT::datatable(do.call(cbind, lapply(df[, input$xAttr], summary)))
        
    })
    
    
    output$Xref_Table <- renderPrint({
        {if (length(input$fxAttr) > 1) {xref_tbl = table(myData()[,input$fxAttr])}
            return(xref_tbl)}
    })
    
    output$OLSResult <- DT::renderDataTable({
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
        
        fit_ols <- t(fit_ols)
        
        DT::datatable(round(fit_ols$coefficients,3))
    })
    
    
    
    output$AnovaRes1 <- DT::renderDataTable({if(is.null(input$file)){return(print("File is null"))} else{
        if (length(input$yAttr) == 1){
            if (length(input$xAttr) ==1){
                cat('\n\n ----- Single Y and X: Running 1-way ANOVA -----\n\n')
                
                
                x <-input$xAttr
                y <- input$yAttr
                fx <- input$fxAttr
                for (i1 in 0:length(fx)){fx[i1] <- paste('as.factor(', fx[i1], ')')}
                
                f <- reformulate(termlabels = c(x), response = y)
                
                fit_1way = aov(f, data = as.data.frame(myData()))
                Anova1 = summary(fit_1way)
                
                DT::datatable(Anova1$coefficients)
            }else{
                cat('\n\n ----- Single Y, many Xs: Running ANCOVA ------\n\n')
                
                x <-input$xAttr
                y <- input$yAttr
                fx <- input$fxAttr
                
                for (i0 in (which(x %in% fx == TRUE))){x[i0] <- paste('as.factor(',x[i0],')')}
                
                f <- reformulate(termlabels = c(x), response = y)
                
                fit_ancova = aov(f, data = as.data.frame(myData()))
                
                a0 = car::Anova(fit_ancova, type="III")
                a0_df = as.data.frame(a0)
                colsum0 = sum(a0$`Sum Sq`)
                a0_df$percent_var_expl = 100*a0$`Sum Sq`/colsum0 
                a0_df = round(a0_df,3)
                Anova1 = a0_df 
                
                DT::datatable(Anova1)
            }
        }else{
            cat('\n\n ----- Multiple Ys: Running MANOVA ------\n\n')
            x <-input$xAttr
            y <- input$yAttr
            fx <- input$fxAttr
            
            Y <- as.matrix(cbind(myData()[,y]))
            
            X = myData()[,x]
            
            print(class(X))
            
            for (i0 in (which(x %in% fx == TRUE))){X[,i0] <- as.factor(X[,i0])}
            
            myData1 = data.frame(Y,X)
            
            
            f <- reformulate(termlabels = c(x))
            f <- str_split_fixed(f,'~',1)[2]
            
            #dependentV <- paste(colnames(Y), sep = "+")
            
            #f <- paste(paste(y, collapse = "+"),'~', paste(x, collapse = "+"))
            
            
            fit_manova = manova(Y ~ f, data = as.data.frame(X))
            
            Anova1 = summary(fit_manova)
            print(Anova1)
        }
    
    }})
    
    
    
    
    output$Plot1 <- renderPlot({if(is.null(input$file)){return(NULL)} else{
        if (length(input$yAttr) == 1){
            if (length(input$xAttr) ==1){
                
                cat('\n\n ----- Single Y and X: Running 1-way ANOVA -----\n\n')
                
                
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
                
                plots_store_list = vector(mode="list", length=n1)          
                
                
                for (i0 in 1:n1){
                    x = input$fxAttr[i0] 
                    x_name = input$fxAttr[i0]
                    y_name = input$yAttr
                    plots_store_list[[i0]] = as.grob(ggplot(as.data.frame(myData()), aes(x = as.factor(myData()[,x]), y = myData()[,y_name])) +
                    geom_boxplot() + labs(x=input$fxAttr[i0], y=input$yAttr))
                    
                }
                
                ggarrange(plotlist = plots_store_list)
        }}
        else{
            cat('\n\n ----- Multiple Ys: Running MANOVA ------\n\n')
            
            n1 = length(input$fxAttr)
            n2 = length(input$yAttr)
            
            plot_store_manova = vector(mode="list", length=n1*n2)
            
            
            y <- input$yAttr
            
            
            i0 = 0
            
            for (i1 in 1:n2){
                y1 = y[,i1]; y_name = colnames(input$yAttr)[i1]
                for (i2 in 1:n1){
                    x = as.factor(myData()[,input$fxAttr[i2]])
                    x_name = input$fxAttr[i2]
                    plot_store_manova[[i0 + 1]] = as.grob(ggplot(as.data.frame(myData())), aes(x = as.factor(myData()[,x]), y = myData()[,y])
                                                  + geom_boxplot() + labs(x=x_name, y=y_name))
                
                i0 = i0 + 1        } # i2 loop ends
            
        } # i1 loop ends
        
        ggarrange(plotlist = plot_store_manova)
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
