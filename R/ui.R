#User interface of the MPSprot Shiny App
#UI: DATA management + GUI visualization


ui <- function() {
  
  kit0 = "Fusion 6C" #default kit  #"ESX17" # 
  width = 2 #width of sidebar panel
  acceptFile = c("text/csv","text/comma-separated-values,text/plain",".csv") #file format accepted
  txtBoolVal = setNames(c(TRUE,FALSE),c("YES","NO"))
  
  titleTxt1 = paste0("EFMex v",packageVersion("EFMex"))
  titleTxt2 = paste0("(using EFM v",packageVersion("euroformix"),")")
  titleTxt = paste(titleTxt1,titleTxt2)
  
  fluidPage(
    shinybusy::add_busy_spinner(spin = "cube-grid",color = "#1D6FB3",position=c("full-page"),height = "120px", width = "120px"),
    titlePanel(titleTxt),
  
    #Construcing a tabulator Layout
    tabsetPanel(
      tabPanel( "Settings",
                
          fluidRow(
            column(3, 
              wellPanel(
                h4("Load manual calculations"),
                actionButton("showSheetImport", "Go to import sheet"),
    
                h4("Data settings"),
                selectInput("kit", "Select kit:",choices=euroformix::getKit(),selected =kit0),
                numericInput("fst", "Theta correction: ", min=0, max=1, value = 0.01, step = 0.01),
                numericInput("AT", "Analytical Threshold:", min=0, value = 50, step=1),
                numericInput("pC", "Drop-in probability",min=0,max=1, value = 0.05, step=0.01),
                numericInput("lambda", "Drop-in hyperparam (lambda)",min=0,max=1, value = 0.01, step=0.01),
                
                actionButton("showDyeSettings_but", "Set per dye"),
                actionButton("showMoreSettings_but", "Show more settings")
                
            )),
             column(6, conditionalPanel("output.hide_dyesettingspanel",
               wellPanel(          
                 h4("Set per dye (channel)"),
                 uiOutput("setPerDyeSettings") #update dynamically #renderUI
              ))),
              column(2, offset=0, conditionalPanel("output.hide_moresettingspanel",
               wellPanel(          
                 h4("More settings"),
                 numericInput("nDone", "Num. Optims.:", min=0, value = 3, step=1),
                 numericInput("steptol", "StepTolerance:", min=0, value = 1e-4, step=1e-4),
                 numericInput("minF", "Minimum Freq.:", min=0, value = NA, step=0.001),
                 radioButtons("normalize", "Normalize Freq.",choices=txtBoolVal, selected = txtBoolVal[1], inline = TRUE),
                 radioButtons("adjQbp", "Adjust Q-frag.",choices=txtBoolVal, selected = txtBoolVal[2], inline = TRUE),
                 textInput("priorBWS","BW stutter prior. function(x) = ", width = 250),
                 textInput("priorFWS","FW stutter prior. function(x) = ", width = 250),
                 numericInput("seed", "Set seed:", min=0, value = NA, step=1)
               ))),
        )
      ),
      
      tabPanel( "Data" ,
          sidebarLayout(
            sidebarPanel(
              h4("Import files"),
              fileInput("popFile", "Choose Population frequency File", multiple = FALSE, accept = acceptFile),
              fileInput("evidFile", "Choose Evidence File(s)", multiple = FALSE, accept = acceptFile),
              fileInput("refFile", "Choose Reference File(s)", multiple = FALSE, accept = acceptFile),
              actionButton("showData", "Press to show data"),
              
              h4("Conditonal Refs"),
              actionButton("showCondRefs", "Press to condition"),
              
              #checkboxInput("somevalue", "Some value", FALSE),
              #https://stackoverflow.com/questions/50215353/r-shiny-hide-show-all-checked-checkboxes
              
              width=width ),
            mainPanel(
                tabsetPanel(
                  tabPanel("Evids", tableOutput("showEvidTable")),
                  tabPanel("Refs", tableOutput("showRefTable")),
                  tabPanel("Freqs", dataTableOutput("showFreqTable"))
                  #tabPanel("EPG",plotly::plotlyOutput('showEPG'))
                )
            )
         )
      ),
      tabPanel( "Conds" ,
                sidebarLayout(
                  sidebarPanel(
                    h4("Conditional refs"),
                    checkboxGroupInput("checkRefs",""),
                    width=width),
                  mainPanel=NULL
                )
      ),
      tabPanel( "Model",
          sidebarLayout(
            sidebarPanel(
              #h4("Conditioning"),
              #sliderInput("condRefs", "Select references to condition on", min = 0, max = maxNOC, value = 3),
              h4("Configuration"),
              #sliderInput("NOC", "Number of contributors: ", min = 1, max = maxNOC, value = 3),
              numericInput("NOC", "Number of contributors: ", min=1, max=10, value = 3, step = 1),
              radioButtons("useDEG", "Degradation",choices=txtBoolVal, selected = txtBoolVal[1], inline = TRUE),
              radioButtons("useBW", "Backward-stutter",choices=txtBoolVal, selected=txtBoolVal[2],inline = TRUE),
              radioButtons("useFW", "Forward-stutter",choices=txtBoolVal, selected=txtBoolVal[2],inline = TRUE),
              
              h4("Calculate"),
              actionButton("calcButton", "Press to Calculate", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              
              h4("Download"),
              downloadButton("downloadData", "Download results"),
              
              h4("More"),
              downloadButton("downloadSettings", "Download settings"),
              
              width=width), 
            mainPanel(
              tabsetPanel(
                tabPanel("LR summary", tableOutput("showLRtable")),
                tabPanel("Details", dataTableOutput("showCalcs"))
              )
            )
          )
      ),
      tabPanel( "Sheet" ,
                sidebarLayout(
                  sidebarPanel(
                    h4("Import manual sheet"),
                    fileInput("sheetFile","Import file", multiple = FALSE),
                    actionButton("calcFromExcelFile", "Calc from Excel"),
                    #actionButton("calcFromTextFile", "Calc from Text"),
                    
                    h4("Download"),
                    downloadButton("downloadData2", "Download results"),
                    width=width),
                  mainPanel(
                    tabsetPanel(NULL,id="tabsetFromManual")
                  )
                )
      ),
    id = "sidePanelID")
  ) #end fluid page
}#end ui
