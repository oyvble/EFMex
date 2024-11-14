#shiny::runApp('C:/Users/oyvbl/Dropbox/git/EFMex/R')
#library(euroformix); source("C:/Users/oyvbl/Dropbox/git/EFMex/R/functions.R")

#SERVER: INDICATES functionalities for showing/visualizing data
server = function(input, output, session) {

  hideTab(inputId = "sidePanelID", target = "Conds")
  hideTab(inputId = "sidePanelID", target = "Sheet")

  #Result list:
  resLRtable <- resHypCalcs <- mleFit <- NULL #store result table (and other object)
  resLRtableList <- NULL #results from manual calculations
    
  #SHOWING PER-MARKER PANEL DEPENDING ON CHOSEN KIT: dynamic update (server side)
  output$setPerDyeSettings <- renderUI({
    kitinfo = euroformix::getKit(input$kit)
    markerDyes = unique(subset(kitinfo,select=c(Color,Marker)))
    dyes = unique(markerDyes$Color)
    tagList(
      fluidRow(
        column(4, 
          lapply(seq_along(dyes), function(i) numericInput(paste0("AT_",i), label = paste0("AT (",dyes[i],")"), value = input$AT, min=0, step=1)
        )),
        column(4, 
          lapply(seq_along(dyes), function(i) numericInput(paste0("pC_",i), label = paste0("pC (",dyes[i],")"), value = input$pC, min=0, step=0.01)
        )),
        column(4, 
          lapply(seq_along(dyes), function(i) numericInput(paste0("lam_",i), label = paste0("lambda (",dyes[i],")"), value = input$lambda, min=0, step=0.01)
        ))
      )
    )
  })
  
  #Used for dynamic hiding panels (settings panel)  
  output$hide_moresettingspanel <- eventReactive(input$showMoreSettings_but, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_moresettingspanel", suspendWhenHidden = FALSE)
  output$hide_dyesettingspanel <- eventReactive(input$showDyeSettings_but, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_dyesettingspanel", suspendWhenHidden = FALSE) 
  
  
  #observeEvent(input$showSettings, {
  output$downloadSettings <- downloadHandler(
    filename = function() {
      paste0("Settings.csv") 
    },
    content = function(file) {
      if(is.null(mleFit)) return(NULL)
      model = mleFit[[1]]$model
      txt = "Model settings used in calculations:"
      txt <- paste0(txt,"\nEvidence(s)=",paste0(names(model$samples),collapse="/"))
      txt <- paste0(txt,"\nMarkers=",paste0(names(model$popFreq),collapse="/"))
      txt <- paste0(txt,"\nDetection threshold=",paste0(model$threshT,collapse="/"))
      txt <- paste0(txt,"\nFst-correction=",paste0(model$fst,collapse="/"))
      txt <- paste0(txt,"\nProbability of drop-in=",paste0(model$prC,collapse="/"))
      txt <- paste0(txt,"\nHyperparam lambda=",paste0(model$lambda,collapse="/"))
      txt <- paste0(txt,"\nDegradation:", ifelse(model$DEG,"YES","NO")) 
      txt <- paste0(txt,"\nBackward Stutter:",ifelse(model$BWS,"YES","NO")) 
      txt <- paste0(txt,"\nForward Stutter:",ifelse(model$FWS,"YES","NO"))
      txt <- paste0(txt,"\nBackward Stutter prop. prior=",paste0(deparse(eval(model$priorBWS)),collapse="") )
      txt <- paste0(txt,"\nForward Stutter prop. prior=",paste0(deparse(eval(model$priorFWS)),collapse="") )
      write.table(txt, file, sep=";", col.names=FALSE, row.names=FALSE, quote=FALSE,na="")
    }
  ) 
  
  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Result.csv") 
      #paste0(input$downloadData,".csv") 
    },
    content = function(file) {
      if(is.null(resLRtable)) return(NULL)
      nAddCols = ncol(resHypCalcs) - ncol(resLRtable)
      addCols = replicate(nAddCols, rep(NA,nrow(resLRtable))) #columns to add
      outTab = cbind(resLRtable, addCols)
      outTab = rbind(outTab,NA,colnames(resHypCalcs))
      outTab = rbind(outTab,resHypCalcs)
      write.table(outTab, file, sep=";", row.names=FALSE, quote=FALSE,na="")
    }
   ) 
  
  #download after importing from text/excel file (listed multiple results)
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0("Result.csv") 
    },
    content = function(file) {
      if(is.null(resLRtableList)) return(NULL)
      write.table(resLRtableList[[1]], file, sep=";", row.names=FALSE, quote=FALSE,na="")
    }
  ) 
  
  
  observeEvent(input$showData, {
    popFile = input$popFile$datapath
    refFile = input$refFile$datapath
    evidFile = input$evidFile$datapath
    refTable = NULL
    if(!is.null(popFile)) output$showFreqTable = DT::renderDT(euroformix::tableReader(popFile))
    if(!is.null(refFile)) {
      refTable = euroformix::tableReader(refFile)
      output$showRefTable  = renderTable(refTable)
    }
    if(!is.null(evidFile)) {
      AT = inputNumeric(input$AT)
      kit = input$kit
      mixTable = euroformix::tableReader(evidFile)
      output$showEvidTable = renderTable(mixTable)
      samples = euroformix::sample_tableToList(mixTable)
      refData =  euroformix::sample_tableToList(refTable)
      gg = euroformix::plotEPG2(samples,input$kit,refData,AT = AT)
      #output$showEPG = plotly::renderPlotly(gg)
    }
  })
  
  #event to show conditional references in a separate panel
  observeEvent(input$showCondRefs, {
    refFile=input$refFile
    if(is.null(refFile)) return(NULL)
    refData = importData(refFile$datapath)
    refNames = names(refData)
    #rint(refNames)
    updateCheckboxGroupInput(session =  session,inputId =  "checkRefs",  choices = refNames)
    showTab("sidePanelID", target = "Conds", select = TRUE)
  })  
  
  #event to show panel where user can import their own sheet for calculation
  observeEvent(input$showSheetImport, {
    showTab("sidePanelID", target = "Sheet", select = TRUE)
  })  

  observeEvent(input$calcFromExcelFile,{
    fn = input$sheetFile$datapath #obtain 
    if(is.null(fn)) return(NULL)
    sheets = readxl::excel_sheets(fn)
    calcList = list()
    for(i in seq_along(sheets)) {
      sheet = sheets[i]
      tab = readxl::read_excel(fn,sheet=sheet) 
      resTable = calcFromTable(tab) #do calculations
      calcList[[sheet]] <- resTable <- getTableFormat(resTable) #format table

      #Insert data into new tables:
      outID = paste0("sheet",i) #create a key
      insertTab(inputId="tabsetFromManual", 
                tabPanel(sheet, tableOutput(outID)),
                select = i==1) #select first only   
      output[[outID]] = renderTable(resTable)
      #output[[outputID]] = renderTable(resTable) #update content
    }
    resLRtableList <<- calcList #store calcs

  })
  
  
  observeEvent(input$calcFromTextFile,{
    print("NOT IMPLEMENTED")
  })

  #Trigger main calculations (loads settings)
  observeEvent(input$calcButton,{
    
    #Obtain settings
    kit = input$kit
    fst = inputNumeric(input$fst)
    AT = inputNumeric(input$AT)
    pC = inputNumeric(input$pC)
    lambda = inputNumeric(input$lambda)
    
    #Obtain dye specific settings (if used)
    kitinfo = euroformix::getKit(kit)
    markerDyes = unique(subset(kitinfo,select=c(Color,Marker)))
    dyes = unique(markerDyes$Color)
    AT_dyeID = paste0("AT_",seq_along(dyes)) #ID for AT
    pC_dyeID = paste0("pC_",seq_along(dyes)) #ID for pC
    lam_dyeID = paste0("lam_",seq_along(dyes)) #ID for lambda
    ATvec <- setNames(rep(AT,nrow(markerDyes)),markerDyes$Marker)
    pCvec <- setNames(rep(pC,nrow(markerDyes)),markerDyes$Marker)
    lambdavec <- setNames(rep(lambda,nrow(markerDyes)),markerDyes$Marker)
    for(i in seq_along(dyes)) {
      locs = markerDyes$Marker[markerDyes$Color==dyes[i]] #obtain loci to insert
      if(!isEmpty(input[[AT_dyeID[i]]])) ATvec[locs] = inputNumeric(input[[AT_dyeID[i]]])
      if(!isEmpty(input[[pC_dyeID[i]]])) pCvec[locs] = inputNumeric(input[[pC_dyeID[i]]])
      if(!isEmpty(input[[lam_dyeID[i]]])) lambdavec[locs] = inputNumeric(input[[lam_dyeID[i]]])
    }

    #MUST SPECIAL HANDLE IF VALUES ARE NOT CHANGED:
    if( all(ATvec==ATvec[1]) && ATvec[1]!=AT ) ATvec[seq_along(ATvec)] = AT
    if( all(pCvec==pCvec[1]) && pCvec[1]!=pC ) pCvec[seq_along(pCvec)] = pC
    if( all(lambdavec==lambdavec[1]) && lambdavec[1]!=lambda ) lambdavec[seq_along(lambdavec)] = lambda
    
    #Model config:
    NOC = inputNumeric(input$NOC)
    useDEG = inputLogic(input$useDEG)
    useBW = inputLogic(input$useBW)
    useFW = inputLogic(input$useFW)

    #Obtain (other) settings
    seed = inputNumeric(input$seed,TRUE)
    nDone = inputNumeric(input$nDone)
    steptol = inputNumeric(input$steptol)

    minF = inputNumeric(input$minF,TRUE)
    normalize=inputLogic(input$normalize)
    adjQbp=inputLogic(input$adjQbp)

    priorBWS <- priorFWS <- NULL
    if(!isEmpty(input$priorBWS)) priorBWS = inputFun(input$priorBWS)
    if(!isEmpty(input$priorFWS)) priorFWS = inputFun(input$priorFWS)

    #READ FROM DATA FILE:
    popFile=input$popFile
    evidFile=input$evidFile
    refFile=input$refFile
    if(is.null(popFile) || is.null(evidFile) || is.null(refFile) ) return(NULL)
    evidData = importData(evidFile$datapath)
    refData = importData(refFile$datapath)
    popFreq = euroformix::freqImport(popFile$datapath)[[1]]
    condRefs = input$checkRefs #obtain name of checked references
    
    samples = evidData
    print("CALCULATING...")
    time = system.time({
    calcObj = calculateExhaustive(samples,refData, popFreq, NOC, kit,  condRefs, #data settings
                                   modelSetting = list(fst=fst, AT=ATvec, pC=pCvec, lambda=lambdavec), #model settings
                                   modelConfig = list(degrade=useDEG, stutterBW=useBW,stutterFW=useFW,priorBWS=priorBWS,priorFWS=priorFWS), #model config
                                   optimConfig = list(seed=seed,nDone=nDone, steptol=steptol, minF=minF, normalize=normalize,adjQbp=adjQbp), #optim config
                                   storeModelFit=FALSE, verbose = TRUE)  #additional
    })[3]
    print(paste0("COMPLETED: Calculations took ",round(time)," seconds"))
    #print(calcObj$LRtable)
    #print(calcObj$hypCalcs)
    resLRtable <<- getTableFormat(calcObj$LRtable)
    resHypCalcs <<- getTableFormat(calcObj$hypCalcs)
    mleFit <<- calcObj$mleFit #store fitted object (used to export results)
    
    output$showLRtable = renderTable(resLRtable)
    output$showCalcs = DT::renderDT(resHypCalcs)
  })

}