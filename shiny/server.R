server <- function(input, output, session) {

    options(scipen=999)
    values <- reactiveValues()
    values$show = FALSE

    showNotification(
        HTML("<a style=\"color:white;\"
            href=\"https://github.com/yizhenn/sigma-nexus\" target=\"blank\" rel=\"noopener\">source code</a>"), 
        duration = NULL, 
        type = "message", 
        closeButton = FALSE)

    deaORsfa = reactive({
        deaORsfa = input$deaORsfa
    })

    observeEvent(input$calculateWEFE, {
        updateNavbarPage(session, "mainPage", selected = "model")
    })

    observeEvent(input$file1, {
        inFile <<- input$file1
        mytable <<- read.csv(inFile$datapath, header = TRUE)
        mergedTable <<- reactive({mytable})
        fileColumns <<- names(mytable)[-1]
        updateSelectInput(session, 'continuousInputSelected', NULL, fileColumns)
        updateSelectInput(session, 'categoricalInputSelected', NULL, fileColumns)
        updateSelectInput(session, 'inputSelected', NULL, fileColumns)
        updateSelectInput(session, 'outputSelected', NULL, fileColumns, selected = '')
        updateSelectInput(session, 'sfaEfficiencyLevels', NULL, fileColumns)
    })

    sfaProductionFunction = c('Cobb-Douglas', 'Translog')
    sfaModel = c('Error Components Frontier', 'Efficiency Effects Frontier')

    output$featureSelection = renderUI({
        if(deaORsfa() == "dea") {
             tagList(
                selectInput(
                    'firstFeatureSelected',
                    HTML('Choose an orientation', as.character(actionButton("helpDeaFirst", "Help"))),
                    choices = c('input', 'output')
                ),
                selectInput(
                    'secondFeatureSelected',
                    HTML('Choose RTS', as.character(actionButton("helpDeaSecond", "Help"))),
                    choices = c('crs', 'vrs')
                )
             )
        } else {
            tagList(
                selectInput(
                    'firstFeatureSelected',
                    HTML('Choose a production function', as.character(actionButton("helpSfaFirst", "Help"))),
                    choices = sfaProductionFunction
                ),
                selectInput(
                    'secondFeatureSelected',
                    HTML('Choose a model', as.character(actionButton("helpSfaSecond", "Help"))),
                    choices = sfaModel
                )
            )
        }
    })

    observeEvent(input$helpCsv, {
        showModal(modalDialog(
            HTML("Data in csv format<br/>
            - ID variable required<br/>
            - length of variables names up to 12 characters for better visualization of the results summary</br>
            - production data: output and inputs variables<br/>
            - data on other components of the composite indicator<br/>
            - cross-sectional or pooled data allowed<br/>
            <br/>
            Preliminary data processing:<br/>
            - removing missing values;<br/>
            - removing negative values and zeroes for output and inputs (especially if parametric approach to
            technical efficiency estimation to be used)<br/>
            - checking data format: numeric and character (categorical variables) data allowed"),
            easyClose = TRUE
        ))
    })


    observeEvent(input$helpOutput, {
        showModal(modalDialog(
            HTML("- Continuous variable<br/>
            - One desirable output<br/>
            Examples: crop output, livestock output, agricultural revenue in constant prices etc<br/>
            - No negative values<br/>
            - No zero values"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpInput, {
        showModal(modalDialog(
            HTML("- Continuous variables<br/>
                - Multiple inputs<br/>
                Examples: land, capital, labour, fertilizer etc<br/>
                - No negative values<br/>
                - No zero values (if SFA model is used)"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpContinuousInput, {
        showModal(modalDialog(
            HTML("Multiple contextual variables allowed: additional factors that influence the
            production process<br/>
            Examples: continuous (precipitation, temperature etc)"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpCategoricalInput, {
        showModal(modalDialog(
            HTML("Multiple contextual variables allowed<br/>
            Examples: categorical (region, agro-climatic zone etc)"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpDeaOrSfa, {
        showModal(modalDialog(
            HTML("Please check manuals for R Packages used<br/>
            - DEA <a href=\"https://cran.r-project.org/web/packages/Benchmarking/Benchmarking.pdf\" target=\"blank\" rel=\"noopener\">Package Benchmarking</a><br/>
            - SFA <a href=\"https://cran.r-project.org/web/packages/frontier/frontier.pdf\" target=\"blank\" rel=\"noopener\">Package frontier</a><br/>
            <img src='ModelImage1.png' style=\"max-width: 100%\"><br/>
            <img src='ModelImage2.png' style=\"max-width: 100%\">"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpDeaFirst, {
        showModal(modalDialog(
            "The choice of input- or output-oriented models depends upon the production process
            characterizing the farm: minimize the use of inputs to produce a given level of output or
            maximize the level of output given levels of the inputs.",
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpDeaSecond, {
        showModal(modalDialog(
            "Two scale assumptions are generally employed: constant returns to scale (CRS), and variable
            returns to scale (VRS). CRS reflects the fact that output will change by the same proportion as
            inputs are changed (e.g. a doubling of all inputs will double output); VRS reflects the fact that
            production technology may exhibit increasing and decreasing returns to scale.",
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpSfaFirst, {
        showModal(modalDialog(
            HTML("<img src='helpSfaFirst.png' style=\"max-width: 100%\">"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpSfaSecond, {
        showModal(modalDialog(
            "Error Components Frontier (see Battese and Coelli 1992)
            Efficiency Effects Frontier (see Battese and Coelli 1995)",
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpSfaEfficiencyLevels, {
        showModal(modalDialog(
            HTML("- Additional factors (z) that should explain the efficiency level<br/>
            - Continuous variables are allowed<br/>
            - Required when Efficiency effects production frontier model is chosen<br/>
            - Examples: farmer’s education, operational subsidies, share of rented land etc"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpGoodPerformance, {
        showModal(modalDialog(
            HTML("- Composite indicator components with larger values indicating better performance<br/>
            - Technical efficiency is one of good performance indicators (calculated (in the previous step) values of technical efficiency are automatically added to your dataset)"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpBadPerformance, {
        showModal(modalDialog(
           HTML("- Composite indicator components with larger values indicating worse performance<br/>
           - Examples: CO2 emissions, pesticides pollution etc")
        ))
    })

    observeEvent(input$helpBoD, {
        showModal(modalDialog(
            HTML("The Benefit of the Doubt approach allows to endogenously determine weights by the observed performances and benchmark is a linear combination of the observed best performances.<br/>
            “Benefit of the doubt weights” are such that your overall relative performance index is as high as possible.<br/>
            <i>Source: F. Vidoli and E. Fusco (2018) Compind: Composite indicators functions based on frontiers in R.</i><br/>
            Please check manual for R Package used
            <a href=\"https://cran.r-project.org/web/packages/Compind/Compind.pdf\" target=\"blank\" rel=\"noopener\">Package ‘Compind’</a><br/>
            <img src='BodImage1.png' style=\"max-width: 100%\"><br/>
            <img src='BodImage2.png' style=\"max-width: 100%\">"),
            easyClose = TRUE
        ))
    })

    output$modelOptions = renderUI({
        if(deaORsfa() == 'sfa' & !is.null(input$secondFeatureSelected)) {
            if(input$secondFeatureSelected == 'Efficiency Effects Frontier') {
                selectInput(
                    'sfaEfficiencyLevels',
                    HTML('Choose Z variables', as.character(actionButton("helpSfaEfficiencyLevels", "Help"))),
                    choices = fileColumns,
                    multiple = TRUE
                )
            }
        }
    })

    output$ciBodOptions = renderUI({
        MInput = numericInput(
            "MNumericInput",
            HTML("M", as.character(actionButton("helpMInput", "Help"))),
            NULL
            )
        BInput = numericInput(
            "BNumericInput",
            HTML("B", as.character(actionButton("helpBInput", "Help"))),
            NULL
            )
        upperWeightInput = sliderInput(
            "upperWeightInput",
            "Upper weights constraint",
            min = 0,
            max = 1,
            value = 0
            )
        lowerWeightInput = sliderInput(
            "lowerWeightInput",
            "Lower weights constraint",
            min = 0,
            max = 1,
            value = 0
            )
        
        if(input$bodFrontierSelected == "ci_bod") {
            return(NULL)
        } else if (input$bodFrontierSelected == "ci_rbod") {
            tagList(
                MInput, BInput
            )
        } else if (input$bodFrontierSelected == "ci_bod_constr") {
            tagList(
                upperWeightInput, lowerWeightInput
            )
        } else if (input$bodFrontierSelected == "ci_bod_constr_bad") {
            tagList(
                lowerWeightInput
            )
        } else {
            tagList(
                MInput, BInput, lowerWeightInput
            )
        }
    })

    observeEvent(input$helpMInput, {
        showModal(modalDialog(
            HTML("\"M\" define the number of elements in each of the bootstrapped samples"),
            easyClose = TRUE
        ))
    })

    observeEvent(input$helpBInput, {
        showModal(modalDialog(
            HTML("\"B\" the number of bootstrap replicates"),
            easyClose = TRUE
        ))
    })

    interactionTerms = function(inputs, prefix, middle, suffix) { 
        startIndex = 1
        result = ''
        while (startIndex < (length(inputs))) {
            for(input in startIndex:((length(inputs))-1)) {
                print(paste0(prefix, inputs[startIndex], middle, inputs[input+1], suffix))
                result = paste0(result, prefix, inputs[startIndex], middle, inputs[input+1], suffix)
            }
            
            startIndex = startIndex + 1
        }
        result = substring(result, 1, nchar(result)-1)
        return (result)
    }

    eff_Crs = c()

    observeEvent(input$run, {
        disable('run')
        withCallingHandlers(tryCatch({
            outputSelected = input$outputSelected
            inputSelected = input$inputSelected
            continuousInputSelected = input$continuousInputSelected
            categoricalInputSelected = input$categoricalInputSelected
            contexturalInputs = ""
            if(!is.null(continuousInputSelected)) {
                contexturalInputs = paste0(contexturalInputs, "+", paste(continuousInputSelected, collapse = '+'))
            }
            if(!is.null(categoricalInputSelected)) {
                contexturalInputs = paste0(contexturalInputs, "+factor(", paste(categoricalInputSelected, collapse = ')+factor('), ')')
            }

            xMat <- mytable[,inputSelected]
            yVec <- matrix(mytable[,outputSelected])
            modelSelected = input$deaORsfa

            firstFeatureSelected = input$firstFeatureSelected
            secondFeatureSelected = input$secondFeatureSelected

            if(modelSelected == 'dea') {
                deaCrsIn <- Benchmarking::dea(
                    xMat,
                    yVec,
                    RTS = secondFeatureSelected,
                    ORIENTATION = substring(firstFeatureSelected, 1, nchar(firstFeatureSelected)-3), 
                    SLACK=TRUE)

                output$summaryOutput = renderPrint({
                    summary(deaCrsIn)
                })

                eff_Crs <<- Benchmarking::efficiencies(deaCrsIn)
                output$histo = renderPlot({
                    hist(eff_Crs)
                })

                output$graph = renderPlot({
                    Benchmarking::dea.plot(xMat, yVec, ORIENTATION = 'in-out')
                })

            } else {
                hide('graph', deaORsfa() == 'sfa')
                sfaEfficiencyLevels = input$sfaEfficiencyLevels

                if(firstFeatureSelected == sfaProductionFunction[1]) {
                    transformInputs = paste0('log(', paste(inputSelected, collapse = ')+log('), ')', contexturalInputs)
                    transformOutput = paste0('log(', outputSelected, ')')
                    if(secondFeatureSelected == sfaModel[1]) {
                        CD_SFA<-sfa(as.formula(paste(transformOutput, transformInputs, sep=' ~ ')), data=mytable)
                        output$summaryOutput = renderPrint({
                            summary(CD_SFA)
                        })
                        eff_Crs <<- frontier::efficiencies(CD_SFA)
                        
                        eff_CD = as.data.frame(eff_Crs)
                        eff_CD$ID = rownames(eff_CD)
                        inFile$ID = rownames(inFile)
                        undt =  merge(inFile, eff_CD, by="ID")

                        output$histo = renderPlot({
                            hist(eff_Crs)
                        })
                    } else {
                        transformEfficiencyLevels = if(is.null(sfaEfficiencyLevels)) "" else paste0('|', paste(sfaEfficiencyLevels, collapse='+'))
                        inputsEfficiencyLevels = paste0(transformInputs, transformEfficiencyLevels)

                        CD_SFA<-sfa(as.formula(paste(transformOutput, inputsEfficiencyLevels, sep=' ~ ')), data=mytable)
                        output$summaryOutput = renderPrint({
                            summary(CD_SFA)
                        })
                        eff_Crs <<- frontier::efficiencies(CD_SFA)
                        output$histo = renderPlot({
                            hist(eff_Crs)
                            
                        })
                    }
                } else {
                    dataT = mytable  %>% mutate_at(vars(c(outputSelected, inputSelected)),
                                        .funs = list(ln= ~log(./mean(.))))
                    transformInputs = paste0(paste(inputSelected, collapse = '_ln+'), '_ln+')
                    transformInputs05 = paste0('I(0.5*', paste(inputSelected, collapse = '_ln^2)+I(0.5*'), '_ln^2)+')
                    transformInputsInteraction = interactionTerms(inputSelected, 'I(', '_ln*', '_ln)+')
                    transformOutput = paste0(outputSelected, '_ln')
                    
                    if(secondFeatureSelected == sfaModel[1]) {
                        TL_SFA<-sfa(as.formula(paste0(transformOutput, ' ~ ', transformInputs, transformInputs05, transformInputsInteraction, contexturalInputs)), data=dataT)
                        
                        output$summaryOutput = renderPrint({
                            summary(TL_SFA)
                        })
                        eff_Crs <<- frontier::efficiencies(TL_SFA)
                        output$histo = renderPlot({
                            hist(eff_Crs)
                        })
                    } else {
                        transformEfficiencyLevels = if(is.null(sfaEfficiencyLevels)) "" else paste0('|', paste(sfaEfficiencyLevels, collapse='+'))
                        TL_SFA_Z<-sfa(as.formula(paste0(transformOutput, ' ~ ', transformInputs, transformInputs05, transformInputsInteraction, transformEfficiencyLevels, contexturalInputs)), data=dataT)
                        
                        output$summaryOutput = renderPrint({
                            summary(TL_SFA_Z)
                        })
                        eff_Crs <<- frontier::efficiencies(TL_SFA_Z)
                        output$histo = renderPlot({
                            hist(eff_Crs)
                        })
                    }

                }
            }
            print("pre")
            eff_CrsID = as.data.frame(eff_Crs)
            eff_CrsID$ID = rownames(eff_CrsID)
            effColumnName <<- if(deaORsfa()=="dea") "eff_Crs" else "efficiency"
            mytable$ID = rownames(mytable)
            mergedTable <<- merge(mytable, eff_CrsID, by="ID")

            newFileColumns = c(fileColumns, effColumnName)
            updateSelectInput(session, 'goodPerformanceSelected', NULL, newFileColumns)
            updateSelectInput(session, 'badPerformanceSelected', NULL, newFileColumns)

            enable('updatedDataset')
            updateTabsetPanel(session, "tabset", selected = 'techEfficiency')
            print("post")
        },
        error=function(cond) {
            message("Here's the original error message:")
            message(cond)
            showModal(modalDialog(
                HTML(paste0('<span style="color: red;">Error</span><span>: ',cond$message,'</span>')),
                easyClose = TRUE
            ))
            return(NA)
        })
        ,
        warning=function(cond) {
            message("Here's the original warning message:")
            message(cond)
            showModal(modalDialog(
                HTML(paste0('<span style="color: red;">Warning</span><span>: ',cond,'</span>')),
                easyClose = TRUE
            ))
            invokeRestart("muffleWarning")
            # print("pre")
            # eff_CrsID = as.data.frame(eff_Crs)
            # eff_CrsID$ID = rownames(eff_CrsID)
            # effColumnName <<- if(deaORsfa()=="dea") "eff_Crs" else "efficiency"
            # mytable$ID = rownames(mytable)
            # mergedTable <<- merge(mytable, eff_CrsID, by="ID")

            # newFileColumns = c(fileColumns, effColumnName)
            # updateSelectInput(session, 'goodPerformanceSelected', NULL, newFileColumns)
            # updateSelectInput(session, 'badPerformanceSelected', NULL, newFileColumns)

            # enable('updatedDataset')
            # updateTabsetPanel(session, "tabset", selected = 'techEfficiency')
            # print("post")
            return(NULL)
        })
        # finally={
            enable('run')
        # })
    })

    observeEvent(input$updatedDataset, {
        showModal(modalDialog(
            class="scroll-vertical",
            renderDataTable(
            mergedTable, options = list(lengthChange = FALSE)),
            easyClose = TRUE
        ))
    })


    observeEvent(input$runComposite, {
        disable('runComposite')
        goodPerformanceSelected = input$goodPerformanceSelected
        badPerformanceSelected = input$badPerformanceSelected
        print(goodPerformanceSelected)
        print("---")
        print(badPerformanceSelected)
        
        numberOfGoodPerformanceSelected = length(input$goodPerformanceSelected)
        numberOfBadPerformanceSelected = length(input$badPerformanceSelected)
        
        indic_onlgood = data.frame(
            Good_performance = mergedTable[,goodPerformanceSelected], 
            Efficiency = mergedTable[,effColumnName]
        )
        
        indic_both = data.frame(mergedTable[,c(badPerformanceSelected, goodPerformanceSelected)])
        Composite_indicators = NULL
        
        MNumericInput = input$MNumericInput
        BNumericInput = input$BNumericInput
        upperWeightInput = input$upperWeightInput
        lowerWeightInput = input$lowerWeightInput


        if(input$bodFrontierSelected == "ci_bod") {
            Composite_indicators = Compind::ci_bod(indic_onlgood)$ci_bod_est
        } else if (input$bodFrontierSelected == "ci_rbod") {
            Composite_indicators = Compind::ci_rbod(
                indic_onlgood, 
                M=MNumericInput, 
                B=BNumericInput)$ci_rbod_est
        } else if (input$bodFrontierSelected == "ci_bod_constr") {
             Composite_indicators = Compind::ci_bod_constr(
                indic_both, 
                up_w=upperWeightInput,
                low_w=lowerWeightInput)$ci_bod_constr_est
        } else if (input$bodFrontierSelected == "ci_bod_constr_bad") {
            Composite_indicators = Compind::ci_bod_constr_bad(
                indic_both, 
                ngood=numberOfGoodPerformanceSelected, 
                nbad=numberOfBadPerformanceSelected,
                low_w=lowerWeightInput, 
                pref=NULL)$ci_bod_constr_bad_est
        } else {
            Composite_indicators = Compind::ci_rbod_constr_bad(
                indic_both, 
                ngood=numberOfGoodPerformanceSelected, 
                nbad=numberOfBadPerformanceSelected, 
                low_w=lowerWeightInput, 
                pref=NULL, 
                M=MNumericInput, 
                B=BNumericInput)$ci_rbod_constr_bad_est
        }

        mergedTable$Composite_indicators = Composite_indicators
        completedData <<- mergedTable

        output$summaryCompositeOutput = renderPrint({
            summary(Composite_indicators)
        })

        output$histoComposite = renderPlot({
            hist(Composite_indicators, xlab="Composite indicator value")
        })

        output$summaryComponentsGoodOutput = renderPrint({
           stat.desc(indic_onlgood)
        })

        output$summaryComponentsBothOutput = renderPrint({
           stat.desc(indic_both)
        })

        enable('runComposite')
        values$show = TRUE
        updateTabsetPanel(session, "tabset", selected = 'compositeIndicator')
    })

    observe({
        if(values$show == TRUE){
            enable("downloadDataset")
        }else{
            disable("downloadDataset")
        }
    })

    output$downloadDataset <- downloadHandler(
        filename = function() {
            "downloadData.csv"
        },
        content = function(file) {
            write.csv(completedData, file)
        }
    )
}

