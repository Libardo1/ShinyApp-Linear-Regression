
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)

source("misc.R")

shinyServer(function(input, output, session) {

######################### Data panel #########################

###### sever interface
  dataInput <- reactive({
    sessionEnvir <- sys.frame()
    inFile <- input$file1
    if (is.null(inFile)) return(NULL) else{
      if (input$type=="rdat") {
        load(inFile$datapath, sessionEnvir)
        datn <- unlist(strsplit(inFile[["name"]], "\\."))[[1]]
        eval(parse(text = datn), sessionEnvir)
      }else if (input$type=="csv") {
        read.csv(inFile$datapath, na=input$missing)
      }else{
        read.delim(inFile$datapath, na=input$missing)
      }
    }
  })

  output$datafactor_ui <- renderUI({
    wellPanel(
      h5("Factor Variable:"),
      selectInput("factor", "", c("NULL", names(rslt$dat))),
      radioButtons('factlevel', 'Factor level',
                   c(Default='asis',
                     'As data order'='asdata',
                     'Specified order'='speciord'),
                   'asis'),
      textInput("ordlevl",  "", value = ""),
      helpText("Enter levels such as Y, C, B etc.."),
      checkboxInput("ordfct", "Ordered Factor?", value = FALSE),
      actionButton("fctrun", "Run"))
  }) # end of output$datafactor_ui

  output$datacalcu_ui <- renderUI({
    wellPanel(
      h5("Calculate New Variable:"),
      textInput("nvar",  "", value = ""),
      helpText("Enter an expression such as Y=A+B, Y=paste(A,B) etc.."),
      "Numeric factor to numeric:",
      selectInput("num", "", c("NULL", names(rslt$dat[!sapply(rslt$dat, is.numeric)]))),
      "Variable to date:",
      selectInput("date","", c("NULL",  names(rslt$dat))),
      textInput("format",  "Date Format:", value = ""),
      actionButton("calcurun", "Run"))
  }) # end of output$datacalcu_ui

  output$datareshapeIDvar_ui <- renderUI({
    selectizeInput("rsdid",  "Identifier variables:", choices = names(rslt$dat),
                   multiple = TRUE, options = list(create = TRUE))
  }) # end of output$datareshapeIDvar_ui
  
###### Sever programe

  rslt <- reactiveValues()
  
  observe({ # For reshape the data
    if (input$reshaperun == 0)
      return(rslt$dat <- dataInput())
    isolate({
      # print(c("1", input$reshaperun))
      #      idn <- unlist(strsplit(input$rsdid, "\\,"))
      #      for (i in 1:5) idn <- sub("^ ","", idn)
      #      for (i in 1:5) idn <- sub(" $","", idn)
      #      if (all(!is.na(suppressWarnings(as.numeric(idn))))) idn <- names(dataInput())[as.numeric(idn)]
      idn <- input$rsdid
      if (all(!is.na(suppressWarnings(as.numeric(idn))))) idn <- names(dataInput())[as.numeric(idn)]
      fo <- formula(paste(paste(idn, collapse="+"), "~", input$rsdvar))
      rslt$dat <- switch(input$rsformat,
                         "long"= melt(data=rslt$dat, id.vars = idn, variable.name = input$rsdvar, value.name = input$rsdval, na.rm = input$rsdna),
                         "wide" = dcast(data=rslt$dat, formula=fo, value.var = input$rsdval))
    })  # end of isolate
  }) # end of observe reshape

  observe({ # For make factors in the data
    if (is.null(input$fctrun) || input$fctrun == 0)
      return()
    isolate({
      # print(c("2", input$factor, input$fctrun))
      lvelord <- unlist(strsplit(input$ordlevl, ","))
      for (i in 1:5) lvelord <- sub("^ ","", lvelord)
      for (i in 1:5) lvelord <- sub(" $","", lvelord)
      rslt$dat[,input$factor] <- switch(input$factlevel,
                                        "asis"=factor(rslt$dat[,input$factor]),
                                        "asdata"=factor(rslt$dat[,input$factor],
                                                        levels=unique(rslt$dat[,input$factor])),
                                        "speciord"= factor(rslt$dat[,input$factor], levels=lvelord))
      if (input$ordfct) rslt$dat[,input$factor] <- ordered(rslt$dat[,input$factor])
    })  # end of isolate
  }) # end of observe factor

  observe({ # For calculate new variable in the data
    if (is.null(input$calcurun) || input$calcurun == 0)
      return()
    isolate({
      # print(c("3", input$num, input$date, input$nvar, input$calcurun))
      if (input$nvar!="") {
        foterm <- unlist(strsplit(input$nvar, "="))
        nvar <- foterm[1]
        for (i in 1:5) nvar <- sub("^ ","", nvar)
        for (i in 1:5) nvar <- sub(" $","", nvar)
        print(c("3",foterm))
        rslt$dat[,nvar] <- with(rslt$dat, try(eval(parse(text=paste(foterm[-1], collapse="="))), TRUE))
      }
      if (input$num!="NULL") {
        if (class(rslt$dat[,input$num])=="Date") rslt$dat[, input$num] <- as.numeric(rslt$dat[,input$num])
        else rslt$dat[, input$num] <- as.numeric(as.character(rslt$dat[,input$num]))
      }
      if (input$date!="NULL") {
        rslt$dat[,input$date] <- switch(class(rslt$dat[,input$date]),
                                        "numeric"=as.Date(rslt$dat[,input$date], origin = "1970-01-01"),
                                        "integer"=as.Date(rslt$dat[,input$date], origin = "1970-01-01"),
                                        "character"=as.Date(rslt$dat[,input$date], format=input$format),
                                        "POSIXct"=as.Date(rslt$dat[,input$date], tz = "UTC"))
      }
    })  # end of isolate
  }) # end of observe calculate

  observe(rslt$datainfo <- datainfo(rslt$dat))    # Obtain data info

###### Output

 output$datastr <- renderPrint({
    if (is.null(rslt$dat))  return()  else{
      if (input$datainfo=="structure") str(rslt$dat) else summary(rslt$dat)
    }
  })

  # Show the first "n" observations
  output$view <- renderDataTable({
    validate(
      need(input$obs > 0, "Please select the number of observations to view")
    )
    head(rslt$dat, n = input$obs)
  }, options = list(bSortClasses = TRUE))

  # save data at *.RData format
  saveddata <- reactiveValues()
  observe({
    if(!is.null(rslt$dat))
      isolate(
        saveddata <<- rslt$dat
      )
  })

  output$savedata <- downloadHandler(
    filename <- function(){
      paste("saveddata.RData")
    },
    content = function(file) {
      save(saveddata, file = file)
    }
  )# end of save data

######################### EDA panel #########################

###### sever interface

  output$edaplot_ui <- renderUI({
    switch(input$plottype1,
           "Scatter, Box or Summary"=wellPanel(tabsetPanel(tabPanel("Variables", value = 21,
                                                                    selectInput("x", "X:", c("NULL", names(rslt$dat))),
                                                                    selectInput("y", "Y:", c("NULL", names(rslt$dat))),
                                                                    selectInput("f", "Color:", c("NULL", names(rslt$dat))),
                                                                    selectInput('z', 'Facet Wrap',
                                                                                c(NULL='.', names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                                                                    selectInput('z2', 'Facet Row',
                                                                                c(NULL='.', names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                                                                    selectInput('z1', 'Facet Column',
                                                                                c(NULL='.', names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                                                                    textInput("pltmain",  "Main title:", value = ""),
                                                                    textInput("pltxlab",  "X label:", value = ""),
                                                                    textInput("pltylab",  "Y label:", value = ""),
                                                                    checkboxInput("pltxlog", "Log10 X?", value = FALSE),
                                                                    checkboxInput("pltylog", "Log10 Y?", value = FALSE),
                                                                    textInput("pltsize",  "Plot size (height x width):", value = "600x600"),
                                                                    checkboxInput("hidedplot", "Hide plot?", value = FALSE)),

                                                           tabPanel("Type",  value = 22,
                                                                    checkboxGroupInput("plotcheck", "",
                                                                                       choices = list("Point"=1, "Line"=2, "Box"=3, "Loess smooth"=4, "lm smooth"=5,
                                                                                                      "Jitter"=6, "Mean"=7),
                                                                                       selected = 1),
                                                                    checkboxInput("smoothse", "Hide se?", value = FALSE)),
                                                           id = "plotinputPanel"),
                                               actionButton("dplotrun", "Run")),
           "Matrix"=wellPanel(h5("Pairs Matrix Plot"),
                              textInput("mpltvar",  "Select variables:", value = ""),
                              selectInput('mpltgrp', 'Group by colour:', c("NULL", names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                              textInput("mpltsubset",  "Subset:", value = NULL),
                              checkboxInput("mpltupd", "Show upper diagnal panel?", value = FALSE),
                              textInput("pltmain",  "Main title:", value = ""),
                              textInput("pltsize",  "Plot size (height x width):", value = "600x600"),
                              checkboxInput("hidedplot", "Hide plot?", value = FALSE),
                              actionButton("dplotrun", "Run")),

           "Histogram"=wellPanel(tabsetPanel(tabPanel("Variables", value = 23,
                                                      selectInput("histx", "X:", c("NULL", names(rslt$dat[!sapply(rslt$dat, is.factor)]))),
                                                      selectInput("histf", "Color:", c("NULL", names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                                                      selectInput('z', 'Facet Wrap',
                                                                  c(NULL='.', names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                                                      selectInput('z2', 'Facet Row',
                                                                  c(NULL='.', names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                                                      selectInput('z1', 'Facet Column',
                                                                  c(NULL='.', names(rslt$dat[sapply(rslt$dat, is.factor)]))),
                                                      textInput("pltmain",  "Main title:", value = ""),
                                                      textInput("pltxlab",  "X label:", value = ""),
                                                      checkboxInput("pltxlog", "Log10 X?", value = FALSE),
                                                      textInput("pltsize",  "Plot size (height x width):", value = "600x600"),
                                                      checkboxInput("hidedplot", "Hide plot?", value = FALSE)),
                                             tabPanel("Type",  value = 24,
                                                      radioButtons("plothisttype", "", c("Count" = "count", "Density" = "density", "Dot"="dot"), inline = TRUE),
                                                      radioButtons("plothistposition", "Position", c("Dodge"="dodge", "Stack" = "stack", "Identity" = "identity"), inline = TRUE),
                                                      checkboxInput("histcurve", "Density curve on?", value = FALSE)),
                                             id = "plothistPanel"),
                                 actionButton("dplotrun", "Run")))
  }) # end of output$edaplot_ui

  output$edatabulate_ui <- renderUI({
    wellPanel(helpText("Create a count (or mean) table up to three ways."),
              selectInput("response", "Response:", c("NULL", names(rslt$dat[!sapply(rslt$dat, is.factor)]))),
              selectInput("tabvar1", "Variable One:", c("NULL", names(rslt$dat))),
              selectInput("tabvar2", "Variable Two:", c("NULL", names(rslt$dat))),
              selectInput("tabvar3", "Variable Three:", c("NULL", names(rslt$dat))),
              checkboxInput("hidetabu", "Hide output?", value = FALSE),
              actionButton("tabrun", "Run"))
  }) # end of output$edatabulate_ui
  
###### Sever programe

  tabudat <- reactiveValues()
  
  observe({ # For tabulate the data
    if (is.null(input$tabrun) || input$tabrun == 0)
      return()
    isolate({
      # print(c("4", input$tabvar1, input$tabvar2, input$tabvar3, input$response, input$tabrun))
      if (all(input$tabvar1=="NULL", input$tabvar2=="NULL", input$tabvar3=="NULL")) rslt$tabu <- "NULL"
      else{
        if (input$response=="NULL") {
          fo <- as.formula(paste("~", paste(input$tabvar1, input$tabvar2, input$tabvar3, sep="+"), sep=""))
          tabout <- xtabs(fo, rslt$dat)
        }else{
          fo1 <- as.formula(paste(input$response, "~", paste(input$tabvar1, input$tabvar2, input$tabvar3, sep="+"), sep=""))
          fo2 <- as.formula(paste("~", paste(input$tabvar1, input$tabvar2, input$tabvar3, sep="+"), sep=""))
          tabout <- round(xtabs(fo1, rslt$dat)/xtabs(fo2, rslt$dat), 4)
        }
        if (sum(c(input$tabvar1, input$tabvar2, input$tabvar3)=="NULL")==2) rslt$tabu <- tabout else
          rslt$tabu <- ftable(tabout)
      }
      tabudat[[paste("Tabu", input$tabrun, sep="_")]] <- rslt$tabu
    }) # end of isolate
  }) # end of observe tabulate

  datplt <- reactiveValues()
  
  observe({ # For plot the data
    if (is.null(input$dplotrun) || input$dplotrun == 0)
      return()
    isolate({
      # print(c("5", input$x, input$y, input$f, input$z, input$histx, input$dplotrun))
      if (input$plottype1=="Scatter, Box or Summary") {
        if (!(input$x %in% names(rslt$dat))) rslt$dplot <- NULL
        else{
          # print(c(6, names(rslt$dat)))
          p <- ggplot(rslt$dat, aes_string(x=input$x, y=input$y))
          if (!is.null(input$f) && input$f != 'NULL') {
            if (class(rslt$dat[,input$f])%in% c("factor", "character") && nlevels(rslt$dat[,input$f]) < 10)
              p <- p + aes_string(color=input$f)+scale_fill_manual(values=colors_pc)+
              scale_colour_manual(values=colors_pc)
            else p <- p + aes_string(color=input$f)
          }else  p <- p + aes(color=colors_pc[1])+ scale_colour_manual(values=colors_pc[1])

          if (!is.null(input$z) && input$z != '.') p <- p +  facet_wrap(as.formula(paste('~', input$z)))
          facets <- paste(input$z2, '~', input$z1)
          if (facets != '. ~ .') p <- p + facet_grid(facets)

          outlins <- ifelse(any(input$plotcheck%in%c(1,6)), 0, 1)
          for (i in as.numeric(input$plotcheck))
            p <- p + switch(i, geom_point(), geom_line(), geom_boxplot(outlier.size = outlins),
                            geom_smooth(se=!input$smoothse),  geom_smooth(method=lm, se=!input$smoothse), geom_jitter(alpha=I(0.4)),
                            stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, position= position_dodge(.15)))
          if (7 %in% as.numeric(input$plotcheck)) p <- p + stat_summary(fun.y = mean, geom="line", position= position_dodge(.15)) +
            stat_summary(fun.y = mean, geom="point", size=1.2, position= position_dodge(.15))
          if (input$pltxlog) p <- p + scale_x_log10()
          if (input$pltylog) p <- p + scale_y_log10()
          xlab <- ifelse(input$pltxlab=="", input$x, input$pltxlab)
          ylab <- ifelse(input$pltylab=="", input$y, input$pltylab)
          p <- p+labs(title =paste(input$pltmain, "\n", sep=""), x=paste("\n", xlab, sep=""), y=paste(ylab, "\n", sep=""))+
            theme_bw(12)
          if (input$f == 'NULL') p <- p+theme(legend.position ="none") else rslt$dplot <- p
          if (class(p)[1]=="gg") rslt$dplot <- datplt[[paste("SBS", input$dplotrun, sep="_")]] <- p else return()
        }
      }

      if (input$plottype1=="Histogram") {
        if (!(input$histx %in% names(rslt$dat))) rslt$dplot <- NULL
        else{
          if (!is.null(input$histf) && input$histf != 'NULL')
            p <- ggplot(rslt$dat, aes_string(x=input$histx, fill=input$histf, group=input$histf, colour=input$histf))
          else p <- ggplot(rslt$dat, aes_string(x=input$histx))+aes(fill="#7CB5EC", colour="#7CB5EC")

          if (!is.null(input$z) && input$z != '.') p <- p +  facet_wrap(as.formula(paste('~', input$z)))
          facets <- paste(input$z2, '~', input$z1)
          if (facets != '. ~ .') p <- p + facet_grid(facets)

          histdat <- na.omit(rslt$dat[, input$histx])
          breaks <- pretty(range(histdat), n = nclass.FD(histdat), min.n = 1)
          bwidth <- breaks[2]-breaks[1]
          dz <- 40/length(histdat)

          p <- switch(input$plothisttype, "count" = p + geom_histogram(binwidth=bwidth, position=input$plothistposition, alpha=I(0.8)),
                      "density" = p + geom_histogram(aes(y=..density..), binwidth=bwidth, position=input$plothistposition, alpha=I(0.8)),
                      "dot" = p + geom_dotplot(stackgroups = TRUE, binwidth = bwidth, method = "histodot", dotsize = dz))
          if (input$histcurve && input$plothisttype=="density") p <- p + geom_line(stat="density")

          if (input$pltxlog) p <- p + scale_x_log10()
          xlab <- ifelse(input$pltxlab=="", input$histx, input$pltxlab)
          p <- p+ theme_hc()+labs(title =paste(input$pltmain, "\n", sep=""), x=paste("\n", xlab, sep=""))
          if (input$histf == 'NULL' || nlevels(rslt$dat[,input$histf]) < 11)
            p <- p+ scale_fill_manual(values=colors_hc)+scale_colour_manual(values=colors_hc)

          if (input$histf == 'NULL') p <- p + theme(legend.position ="none") else rslt$dplot <- p
          if (class(p)[1]=="gg") rslt$dplot <- datplt[[paste("hist", input$dplotrun, sep="_")]] <- p else return()
        }
      }

      if (input$plottype1=="Matrix") {
        mpltvarid <- unlist(strsplit(input$mpltvar, "\\,"))
        for (i in 1:5) mpltvarid <- sub("^ ","", mpltvarid)
        for (i in 1:5) mpltvarid <- sub(" $","", mpltvarid)
        if (all(!is.na(as.numeric(mpltvarid)))) mpltvarid <- as.numeric(mpltvarid)

        #input$mpltsubset
        updp <- "blank"
        if(input$mpltupd) updp <- list(params=list(corSize=3.5))

        if(!is.null(input$mpltgrp) && input$mpltgrp!="NULL") {
          matrixplot <- ggpairs(rslt$dat, columns=mpltvarid, color=input$mpltgrp,
                                lower=list(continuous="smooth", params=c(shape=1, size=1.5)),
                                diag=list(continuous="bar"),
                                upper=updp, axisLabels='show', title = input$pltmain)
          if (class(matrixplot)[1]=="ggpairs") rslt$dplot <- datplt[[paste("matrix", input$dplotrun, sep="_")]] <- matrixplot else return()

        }else{
          matrixplot <- ggpairs(rslt$dat, columns=mpltvarid,
                                lower=list(continuous="smooth", params=c(colour="green3")),
                                diag=list(continuous="bar", params=c(colour="blue")),
                                upper=updp, axisLabels='show', title = input$pltmain)
          if (class(matrixplot)[1]=="ggpairs") rslt$dplot <- datplt[[paste("matrix", input$dplotrun, sep="_")]] <- matrixplot else return()
        }
      }
    }) # end of isolate
  }) # end of observe plot
  
###### Output

  output$datastr1 <- renderPrint({
    if (!is.null(rslt$dat) && !input$hideDataInfo1) datainfo(rslt$dat) else return()
  })

  # Show tabulate output
  output$tabu <- renderPrint({
    if (!is.null(input$hidetabu) && !input$hidetabu) rslt$tabu else NULL
  })

  # show data plot
  widthSize <- function() {
    as.numeric(sub(" ","", unlist(strsplit(input$pltsize, "x"))))[2]
  }
  heightSize <- function() {
    as.numeric(sub(" ","", unlist(strsplit(input$pltsize, "x"))))[1]  #if (!is.null(input$pltsize))
  }

  output$dataplotout <- renderPlot({
    if (!is.null(input$hidedplot) && !input$hidedplot) print(rslt$dplot) else NULL
  }, height = heightSize, width = widthSize)

######################### Model panel #########################

###### sever interface

  output$subset_ui <- renderUI({
    wellPanel(h5('Data Subset'),
              selectizeInput('varselect', 'Select variables:', choices = names(rslt$dat),
                             multiple = TRUE, options = list(create = TRUE)),
              textInput("filterdata",  "Filter data by:", value = ""),
              actionButton("subsetrun", "Run"))
  }) # end of output$subset_ui

  output$model_ui <- renderUI({
    switch(input$modletype,
           "lm"=wellPanel(tabsetPanel(tabPanel("Model", value = 27,
                                               selectizeInput('modelresponse', 'Response',
                                                              c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)])),
                                                              options = list(create = TRUE)),
                                               textInput("modelterm",  "Model terms:", value = ""),
                                               selectInput('modelweights', 'Weights',
                                                           c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)])))),

                                      tabPanel("Output",  value = 28,
                                               checkboxGroupInput("modellmcheck", "",
                                                                  choices = list("anova"=1, "summary"=2, "confint"=3, "drop1"=4),
                                                                  selected = c(1,2))),
                                      id = "modellmPanel"),
                          actionButton("modelrun", "Run")),

           "glm"=wellPanel(tabsetPanel(tabPanel("Model", value = 29,
                                                selectizeInput('modelresponse', 'Response', choices =names(rslt$modeldata),
                                                               multiple = TRUE, options = list(maxItems = 2, create = TRUE)),
                                                textInput("modelterm",  "Model terms:", value = ""),
                                                selectInput('modelfamily', 'Family',
                                                            c("NULL", "binomial", "gaussian", "Gamma", "inverse.gaussian", "poisson", "quasi", "quasibinomial", "quasipoisson")),
                                                selectInput('modelfamilylink', 'Link',
                                                            c("NULL", "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse")),
                                                selectInput('modelfamilyvariance', 'Variance function for quasi family',
                                                            c("NULL", "constant", "mu(1-mu)", "mu", "mu^2", "mu^3")),
                                                selectInput('modelweights', 'Weights',
                                                            c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)]))),
                                                selectInput('modeloffset', 'Offset',
                                                            c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)])))),
                                       tabPanel("Output",  value = 210,
                                                checkboxGroupInput("modellmcheck", "",
                                                                   choices = list("anova"=1, "summary"=2, "confint"=3, "drop1"=4),
                                                                   selected = c(1,2))),
                                       id = "modelglmPanel"),
                           actionButton("modelrun", "Run")),

           "lme, lmer, gls"=wellPanel(radioButtons('mixmodeltype', 'Selected model:', c(lme='lme', lmer='lmer', gls='gls'), inline=TRUE),
                                      tabsetPanel(tabPanel("Model", value = 211,
                                                           selectizeInput('modelresponse', 'Response',
                                                                          c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)])),
                                                                          options = list(create = TRUE)),
                                                           textInput("modelfixterm",  "Fixed terms:", value = ""),
                                                           textInput("modelrandterm",  "Random/correlation terms:", value = ""),
                                                           helpText("e.g. 1|A, x|A, (1|A)+(1|B)..."),
                                                           wellPanel("For lme and gls models:",
                                                                     selectInput('corrstr', ' Correlation structure',
                                                                                 c("NULL", "compound symmetry", "AR1", "AR2", "MA1", "MA2", "ARMA(1,1)")),
                                                                     selectInput('heterostr', 'Heterogeneous structure',
                                                                                 c("NULL", names(rslt$modeldata[sapply(rslt$modeldata, is.factor)]), "Fitted"))),
                                                           radioButtons('lmemethod', 'Method:', c(REML='REML', ML='ML'), inline=TRUE)),
                                                  tabPanel("Output",  value = 212,
                                                           checkboxGroupInput("modellmcheck", "",
                                                                              choices = list("anova"=1, "summary"=2, "confint"=3, "drop1"=4),
                                                                              selected = c(1,2))),
                                                  id = "modellmePanel"),
                                      actionButton("modelrun", "Run")),

           "glmer"=wellPanel(tabsetPanel(tabPanel("Model", value = 213,
                                                  selectizeInput('modelresponse', 'Response', choices =names(rslt$modeldata),
                                                                 multiple = TRUE, options = list(maxItems = 2, create=TRUE)),
                                                  textInput("modelfixterm",  "Fixed terms:", value = ""),
                                                  textInput("modelrandterm",  "Random terms:", value = ""),
                                                  helpText("e.g. 1|A, x|A, (1|A)+(1|B)..."),
                                                  selectInput('modelfamily', 'Family',
                                                              c("NULL", "binomial", "gaussian", "Gamma", "inverse.gaussian", "poisson", "quasi", "quasibinomial", "quasipoisson")),
                                                  selectInput('modelfamilylink', 'Link',
                                                              c("NULL", "logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse")),
                                                  selectInput('modelfamilyvariance', 'Variance function for quasi family',
                                                              c("NULL", "constant", "mu(1-mu)", "mu", "mu^2", "mu^3")),
                                                  selectInput('modelweights', 'Weights',
                                                              c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)]))),
                                                  selectInput('modeloffset', 'Offset',
                                                              c("NULL", names(rslt$modeldata[!sapply(rslt$modeldata, is.factor)])))),
                                         tabPanel("Output",  value = 214,
                                                  checkboxGroupInput("modellmcheck", "",
                                                                     choices = list("anova"=1, "summary"=2), selected = c(1,2))),
                                         id = "modellmePanel"),
                             actionButton("modelrun", "Run"))


    )# end of switch
  })# end of output$model_ui
  
###### Sever programe

  mod <- reactiveValues()

  observe({ # For subset
    rslt$modeldata <- rslt$dat
    if (is.null(input$subsetrun) || input$subsetrun == 0)
      return()
    isolate({
      if (!is.null(input$varselect) && input$varselect!="") {
        idn <- input$varselect
        if (all(!is.na(suppressWarnings(as.numeric(input$varselect))))) idn <- as.numeric(input$varselect)
        rslt$modeldata <- try(rslt$modeldata[, idn], TRUE)
      }
      if (!is.null(input$filterdata) && input$filterdata!="")
        rslt$modeldata <- try(droplevels(with(rslt$modeldata, rslt$modeldata[eval(parse(text=input$filterdata)),])), TRUE)
    }) # end of isolate
  }) # end of subset

  observe({ # For Model
    if (is.null(input$modelrun) || input$modelrun == 0)
      return()
    isolate({
      if (input$modletype=="lm") {
        lmmod <- try(lm(formula=as.formula(paste(input$modelresponse, '~', input$modelterm)),
                        weights = eval(parse(text = input$modelweights)), data=rslt$modeldata), TRUE)
        if (class(lmmod)[1]=="lm") rslt$mod <- mod[[paste("lm", input$modelrun, sep="_")]] <- lmmod else return()
      }

      if (input$modletype=="glm") {
        if (input$modelfamilylink=="NULL") {
          familyfun <- input$modelfamily
        }else if (input$modelfamily=="quasi") {
          familyfun <- paste("quasi", "(variance=", input$modelfamilyvariance, ", link=", input$modelfamilylink, ")", sep="")
        }else familyfun <- paste(input$modelfamily, "(",input$modelfamilylink, ")", sep="")

        if (length(input$modelresponse)==1) {
          glmmod <- try(glm(formula=as.formula(paste(input$modelresponse, '~', input$modelterm)), family=eval(parse(text=familyfun)),
                            weights = eval(parse(text = input$modelweights)), offset = eval(parse(text = input$modeloffset)), data=rslt$modeldata), TRUE)
        }else glmmod <- try(glm(formula=as.formula(paste("cbind(rslt$modeldata[,input$modelresponse[1]], rslt$modeldata[,input$modelresponse[2]]-rslt$modeldata[,input$modelresponse[1]])",
                                                         '~', input$modelterm)), family=eval(parse(text=familyfun)), weights = eval(parse(text = input$modelweights)),
                                offset = eval(parse(text = input$modeloffset)), data=rslt$modeldata), TRUE)

        if (class(glmmod)[1]=="glm") rslt$mod <- mod[[paste("glm", input$modelrun, sep="_")]] <- glmmod else return()
      }

      if (input$modletype=="lme, lmer, gls") {
        fixterm <<- formula(paste(input$modelresponse, '~', input$modelfixterm))
        if (input$modelrandterm!="") randomterm <<- formula(paste('~', input$modelrandterm))
        corrterm <<- input$modelrandterm
        if (input$heterostr=="NULL") {
          heterowts <<- "NULL"
        }else if(input$heterostr=="Fitted") {
          heterowts <<- "varPower(form = ~ fitted(.))"
        }else heterowts <<- paste('varIdent(form=~1|', input$heterostr, ')')
        corrstrin <<- switch(input$corrstr, "NULL"="NULL",
                             "compound symmetry"=paste("corCompSymm(form=~", corrterm, ")", sep=""),
                             "AR1"=paste("corAR1(form=~", corrterm, ")", sep=""),
                             "AR2"=paste("corARMA(p=2,q=0, form=~", corrterm, ")", sep=""),
                             "MA1"=paste("corARMA(p=0,q=1, form=~", corrterm, ")", sep=""),
                             "MA2"=paste("corARMA(p=0,q=2, form=~", corrterm, ")", sep=""),
                             "ARMA(1,1)"=paste("corARMA(p=1,q=1, form=~", corrterm, ")", sep=""))

        methodin <<- input$lmemethod

        lmemod <- switch(input$mixmodeltype,
                         "lme"= try(lme(fixed=fixterm, random=randomterm, na.action=na.exclude, method=methodin,
                                        correlation=eval(parse(text=corrstrin)),
                                        weights =eval(parse(text=heterowts)),
                                        control=lmeControl(opt="optim"),
                                        data=rslt$modeldata), TRUE),
                         "lmer"=try(lmer(formula(paste(input$modelresponse, '~', input$modelfixterm, '+ (', input$modelrandterm, ')')), REML=(input$lmemethod=="REML"),
                                         data=rslt$modeldata), TRUE),
                         "gls"=try(gls(model=fixterm, na.action=na.exclude, method=methodin,
                                       correlation=eval(parse(text=corrstrin)),
                                       weights =eval(parse(text=heterowts)),
                                       data=rslt$modeldata),TRUE))
        if (class(lmemod)[1] %in% c("lmerMod", "lme", "gls")) rslt$mod <- mod[[paste(input$mixmodeltype, input$modelrun, sep="_")]] <- lmemod else return()
      }

      if (input$modletype=="glmer") {
        if (input$modelfamilylink=="NULL") {
          familyfun <- input$modelfamily
        }else if (input$modelfamily=="quasi") {
          familyfun <- paste("quasi", "(variance=", input$modelfamilyvariance, ", link=", input$modelfamilylink, ")", sep="")
        }else familyfun <- paste(input$modelfamily, "(",input$modelfamilylink, ")", sep="")

        if (length(input$modelresponse)==1) {
          glmemod <-try(glmer(formula(paste(input$modelresponse, '~', input$modelfixterm, '+ (', input$modelrandterm, ')')),
                              family=eval(parse(text=familyfun)), weights = eval(parse(text = input$modelweights)),
                              offset = eval(parse(text = input$modeloffset)), data=rslt$modeldata), TRUE)
        }else glmemod <-try(glmer(formula=as.formula(paste("cbind(rslt$modeldata[,input$modelresponse[1]], rslt$modeldata[,input$modelresponse[2]]-rslt$modeldata[,input$modelresponse[1]])",
                                                           '~', input$modelfixterm, '+ (', input$modelrandterm, ')')), family=eval(parse(text=familyfun)), weights = eval(parse(text = input$modelweights)),
                                  offset = eval(parse(text = input$modeloffset)), data=rslt$modeldata), TRUE)

        if (class(glmemod)[1] == "glmerMod") rslt$mod <- mod[[paste("glmer", input$modelrun, sep="_")]] <- glmemod else return()
      }

      if (!is.null(input$mixmodeltype) && input$mixmodeltype %in% c("lme", "gls")) {
        rslt$anova <- try(anova(rslt$mod), TRUE)
      }else{
        anovatry <- try(Anova(rslt$mod), TRUE)
        if (class(anovatry)[1]=="anova") rslt$anova <- anovatry else rslt$anova <- NULL
      }

      summarymod <- try(summary(rslt$mod), TRUE)
      if (!is.null(input$mixmodeltype) && input$mixmodeltype=="lme") summarymod$corFixed <- matrix("omit")
      if (!is.null(input$mixmodeltype) && input$mixmodeltype=="gls") summarymod$corBeta <- matrix("omit")
      rslt$summary <- summarymod

      if (!is.null(input$mixmodeltype) && input$mixmodeltype %in% c("lme", "gls")){
        rslt$confint <- try(intervals(rslt$mod), TRUE)
        rslt$drop1 <- NULL
      }else{
        if (3 %in% input$modellmcheck) {
          if (!is.null(input$modelfamily) && input$modelfamily=="binomial") {
            rslt$confint <- list("Parameters"=try(confint(rslt$mod), TRUE), "Odds Ratio"= exp(cbind(OR = coef(rslt$mod), confint(rslt$mod))))
          }else rslt$confint <- try(confint(rslt$mod), TRUE)
        }
        rslt$drop1 <- drop1(rslt$mod, test= "Chisq")
      }

      rslt$modterm <- attr(terms(rslt$mod),"term.labels")
    }) # end of isolate
  }) # end of observe model
  
###### Output

  output$datastr2 <- renderPrint({
    if (!is.null(rslt$modeldata) && !input$hideDataInfo2) datainfo(rslt$modeldata) else return()
  })

  output$anova <- renderTable({
    if (1 %in% input$modellmcheck) rslt$anova else NULL
  })

  output$summary <- renderPrint({
    if (2 %in% input$modellmcheck) {
      print(rslt$summary, corr=FALSE)
      if (!is.null(rslt$mod) && input$modletype %in% c("glm", "glmer")) print(family(rslt$mod))
    }else NULL
  })

  output$confint <- renderPrint({
    if (3 %in% input$modellmcheck) rslt$confint else NULL
  })

  output$drop1 <- renderPrint({
    if (4 %in% input$modellmcheck) rslt$drop1 else NULL
  })

  output$residualplot <- renderPlot({
    residplot(model=rslt$mod, newwd=FALSE)
  }, height = 600, width = 600)
  
######################### Model comparison panel #########################  

###### sever interface

  output$modelcomparison_ui <- renderUI({
    wellPanel(selectizeInput('fittedmodels', h5('Select two to four models'), choices = names(mod), multiple = TRUE, options = list(maxItems = 4)),
              checkboxInput("hideModelInfo", "Hide model comparison?", value = FALSE),
              selectInput('fittedmodel', h5('Select a fitted Model'), c("NULL", names(mod))),
              checkboxGroupInput("fittedmodelcheck", "", choices = list("Residual Plot"=1, "Summary"=2, "anova"=3), selected = 1))
  }) # end of output$edatabulate_ui
  
###### Output

  output$anova1 <- renderTable({
    if (!is.null(mod[[input$fittedmodel]]) && 3 %in% input$fittedmodelcheck) {
      if (!is.null(input$mixmodeltype) && input$mixmodeltype %in% c("lme", "gls")) {
        try(anova(mod[[input$fittedmodel]]), TRUE)
      }else Anova(mod[[input$fittedmodel]])
    }else NULL
  })

  output$summary1 <- renderPrint({
    if (!is.null(mod[[input$fittedmodel]]) && 2 %in% input$fittedmodelcheck) {
      sumry <- summary(mod[[input$fittedmodel]])
      if (!is.null(input$mixmodeltype) && input$mixmodeltype=="lme") sumry$corFixed <- matrix("omit")
      if (!is.null(input$mixmodeltype) && input$mixmodeltype=="gls") sumry$corBeta <- matrix("omit")
      sumry <- print(sumry, corr=FALSE)
    }else NULL
  })

  output$residualplot1 <- renderPlot({
    if (!is.null(mod[[input$fittedmodel]]) && 1 %in% input$fittedmodelcheck) residplot(model=mod[[input$fittedmodel]], newwd=FALSE)
  }, height = 600, width = 600)

  output$anova2 <- renderPrint({
    if (length(reactiveValuesToList(mod)) >= 1 && !input$hideModelInfo) {
      Modelinfo <- t(sapply(reactiveValuesToList(mod), function(x) c(AIC(x), BIC(x), logLik(x))))
      colnames(Modelinfo) <- c("AIC", "BIC", "LogLiklihood")
      cat("All Models Info:\n\n")
      print(Modelinfo)
      cat("\nANOVA:\n\n")
      if (length(input$fittedmodels)==2) {
        anovaOut <- anova(mod[[input$fittedmodels[1]]], mod[[input$fittedmodels[2]]])
        rownames(anovaOut) <- input$fittedmodels
        anovaOut
      }else if (length(input$fittedmodels)==3) {
        anovaOut <- anova(mod[[input$fittedmodels[1]]], mod[[input$fittedmodels[2]]], mod[[input$fittedmodels[3]]])
        rownames(anovaOut) <- input$fittedmodels
        anovaOut
      }else if (length(input$fittedmodels)==4) {
        anovaOut <- anova(mod[[input$fittedmodels[1]]], mod[[input$fittedmodels[2]]],
                          mod[[input$fittedmodels[3]]], mod[[input$fittedmodels[4]]])
        rownames(anovaOut) <- input$fittedmodels
        anovaOut
      }else NULL
    }else NULL
  })

######################### Predict means panel #########################   

###### sever interface

  output$predictmean_ui <- renderUI({
    wellPanel(tabsetPanel(tabPanel("Means", value = 215,
                                   selectizeInput("pdmodelterm", "Model factor terms:",
                                               choices = c("NULL", rslt$modterm), options = list(create=TRUE)),
                                   textInput("pdcov",  "Estimate predicted means at covariate value:", value = ""),
                                   numericInput("pdlevel", "Significant level:", 0.05, min = 0, max = 1),
                                   numericInput("pdDF", "Specified degree of freedom:", NULL, min = 0, max = 100),
                                   checkboxInput("pdpair", "Show pairwise comparisons?", value = FALSE),
                                   selectInput("adjp", "Adjust p-value by:",
                                               c("none", "tukey", p.adjust.methods[-8])),
                                   textInput("pdatvar", "Show pairwise comparisons at each level of:", value = ""),
                                   checkboxInput("pmplot", "Show p-value matrix plot?", value = FALSE),
                                   textInput("pdtrans",  "Obtain back transformed means by function: y = ", value = ""),
                                   helpText("i.e. x^2, sqrt(x), exp(x), log(x) ..."),
                                   actionButton("predictrun", "Run")),
                          tabPanel("Plots", value = 216,
                                   checkboxInput("hidepredictmeans", "Hide predicted means?", value = FALSE),
                                   wellPanel(radioButtons('mplottype', 'mean plot type:', c(Line='line', Bar='bar', Interval='interval'), inline=TRUE),
                                             selectizeInput("mplotord",  "Plot order(i.e 2,1 or 3,1,2...):", choices = 1:3, multiple = TRUE),
                                             textInput("mplottitle",  "Plot tittle:", value = ""),
                                             textInput("mpltsize",  "Plot size (height x width):", value = "600x600"))
                          ),
                          tabPanel("Covariate Mean", value = 217,
                                   selectInput("covmodelterm", "Model covariate terms:",
                                               c("NULL", rslt$modterm)),
                                   checkboxInput("covtrillis", "Show a trillis plot?", value = TRUE),
                                   checkboxInput("covci", "Show confidence interval?", value = TRUE),
                                   checkboxInput("covpoint", "Show data points?", value = TRUE),
                                   numericInput("covjitter", "Degree of jittering:", 0, min = 0, max = 3),
                                   selectizeInput("covplotord",  "Plot order(i.e 2,1 or 3,1,2...):", choices = 1:3, multiple = TRUE),
                                   textInput("covplottitle",  "Plot tittle:", value = ""),
                                   textInput("covtrans",  "Obtain back transformed means by function: y = ", value = ""),
                                   helpText("i.e. x^2, sqrt(x), exp(x), log(x) ..."),
                                   textInput("covnresponse",  "New response name", value = ""),
                                   textInput("covpltsize",  "Plot size (height x width):", value = "600x600")),
                          id = "predictmeanPanel") # end of tabsetPanel
    )
  }) # end of output$predictmean_ui
  
###### Sever programe

  observe({ # For predictmeans
    if (is.null(input$predictrun) || input$predictrun == 0)
      return()
    isolate({
      if (is.na(input$pdDF)) Df <- NULL else Df <- input$pdDF
      if (input$pdatvar=="") atvar <- NULL else atvar <- sub(" ", "", unlist(strsplit(input$pdatvar, "\\,")))
      if (input$pdcov=="") covvalue <- NULL else covvalue <- as.numeric(sub(" ", "", unlist(strsplit(input$pdcov, "\\,"))))
      if (input$pdtrans=="") bkf <- NULL else bkf <- function(x) eval(parse(text=input$pdtrans))

      if (!is.null(input$pdmodelterm) && !input$pdmodelterm%in%c("NULL", "")) {
        rslt$predictm <- try(predictmeans(model=rslt$mod, modelterm=input$pdmodelterm, pairwise=input$pdpair, atvar=atvar, adj=input$adjp, Df=Df,
                                          level=input$pdlevel, covariate=covvalue, prtnum=FALSE, trans = bkf, plot=FALSE), TRUE) 
      }else rslt$predictm <- NULL

      rslt$pdmt <- input$pdmodelterm
      rslt$covvalue <- covvalue
      rslt$pair <- input$pdpair
      rslt$level <- input$pdlevel
      rslt$bkf <- bkf
      rslt$atvar <- atvar
      rslt$Df <- Df
      rslt$adj <- input$adjp

    }) # end of isolate
  }) # end of observe predictmeans
  
###### Output

  output$predictm <- renderPrint({
    if (is.null(rslt$predictm) || input$hidepredictmeans)  return()  else{
      if (input$pdcov=="") covvalue <- NULL else covvalue <- as.numeric(sub(" ", "", unlist(strsplit(input$pdcov, "\\,"))))
      aa <- Kmatrix(model=rslt$mod, modelterm=input$pdmodelterm, covariate = covvalue, prtnum=TRUE)
      print(rslt$predictm[1:4])
      if ("Back Transformed Means" %in%  names(rslt$predictm)) {
        print("Back Transformed Means")
        print(rslt$predictm$`Back Transformed Means`)
      }
    }
  })

  output$predictmcp <- renderPrint({
    if (is.null(rslt$predictm) || !input$pdpair)  return()  else {
      "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
      nindx <- names(rslt$predictm)%w/o%c(names(rslt$predictm)[1:4], "Back Transformed Means")
      rslt$predictm[nindx]
    }
  })

  output$predictbktran <- renderPrint({
    if (!is.null(rslt$predictm) && "Back Transformed Means" %in%  names(rslt$predictm)) print(rslt$predictm$`Back Transformed Means`)
    else return()
  })

  widthSizem <- function() {
    as.numeric(sub(" ","", unlist(strsplit(input$mpltsize, "x"))))[2]  #if (!is.null(input$mpltsize) && input$mpltsize!="")
  }
  heightSizem <- function() {
    as.numeric(sub(" ","", unlist(strsplit(input$mpltsize, "x"))))[1]
  }

  output$predictmeansmplot <- renderPlot({
    if (is.null(rslt$predictm))  return()  else {
      if (is.null(input$mplotord)) morder <- NULL else morder <- as.numeric(input$mplotord)
      rslt$morder <- morder
      rslt$mptype <- input$mplottype
      
      switch(input$mplottype,
        "bar"=try(predictmeans(model=rslt$mod, modelterm=input$pdmodelterm, covariate = rslt$covvalue, prtnum=FALSE, mplot = FALSE, barplot = TRUE,
                         bkplot=FALSE, plotord = morder, plottitle = input$mplottitle, Df=rslt$Df, level = input$pdlevel, newwd = FALSE), TRUE),
        "line"=try(predictmeans(model=rslt$mod, modelterm=input$pdmodelterm, covariate = rslt$covvalue, prtnum=FALSE, mplot = TRUE, barplot = FALSE,
                         bkplot=FALSE, plotord = morder, plottitle = input$mplottitle, Df=rslt$Df, level = input$pdlevel, newwd = FALSE), TRUE),
        "interval"=try(predictmeans(model=rslt$mod, modelterm=input$pdmodelterm, covariate = rslt$covvalue, prtnum=FALSE, mplot = FALSE, barplot = FALSE,
                         pplot=FALSE, bkplot=TRUE, trans = rslt$bkf, plottitle = input$mplottitle, Df=rslt$Df, level = input$pdlevel, newwd = FALSE), TRUE))               
    }
  }, height = heightSizem, width = widthSizem)
  
  output$predictmeansPMplot <- renderPlot({
    if (!is.null(rslt$predictm) && input$pmplot){
      try(predictmeans(model=rslt$mod, modelterm=input$pdmodelterm, covariate = rslt$covvalue,
        pairwise = TRUE, atvar = rslt$atvar, adj = input$adjp, prtnum=FALSE, mplot = FALSE,
        bkplot=FALSE, Df=rslt$Df, level = input$pdlevel, newwd = FALSE), TRUE)
    }
  }, height = 600, width = 600)

  widthSizecov <- function() {
    as.numeric(sub(" ","", unlist(strsplit(input$covpltsize, "x"))))[2]
  }
  heightSizecov <- function() {
    as.numeric(sub(" ","", unlist(strsplit(input$covpltsize, "x"))))[1]
  }

  output$predictmeanscovplot <- renderPlot({
    if (input$covmodelterm =="NULL")  return()  else{
      if (is.null(input$covplotord)) covplotord <- NULL else covplotord <- as.numeric(input$covplotord)
      if (input$covtrans=="") covtrans <- NULL else covtrans <- function(x) eval(parse(text=input$covtrans))

      rslt$covariatemeansplot <- try(covariatemeans(model=rslt$mod, modelterm=input$pdmodelterm, covariate=input$covmodelterm, level=input$pdlevel,
                         Df=rslt$Df, trans=covtrans, responsen=input$covnresponse, trillis=input$covtrillis, plotord=covplotord,
                         mtitle=input$covplottitle, ci=input$covci, point=input$covpoint, jitterv=input$covjitter, newwd=FALSE), TRUE)
      print(rslt$covariatemeansplot)
    }
  }, height=heightSizecov, width=widthSizecov)
  
######################### Down load report ######################### 
 
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd') 
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  ) # end of downloadHandler 
  
})
Enter file contents here
