# Required Packages
library(shiny)
library(shinydashboard)
#For working with dataframes
library(dplyr)
# For working with text
library(tidyr)
#Dates Transformations
library(lubridate)
#rownames to column in df
library(tibble)
# For plots
library(plotly)
library(ggplot2)
library(DT)

#Saving the path
path="D:/Abbhiraami/Cue_Analytics"
RScripts=paste0(path,"/Analytics_Code")
input_path=paste0(path,"/Cue_Files")
output_path=paste0(path,"/Result_files")

#Reading the datafiles
# The below code runs the preprocessing codes and creates a file called MasterFile 
# in the input_path Which is the input file for this dashboard 
# (line number: 298 in the Cue_FromTo_Return_01042020.R )
#source(paste0(RScripts,"/Cue_FromTo_Return_01042020.R"))
msg=read.csv(paste0(input_path,"/MasterFile.csv"))
msg$Date=as.Date(msg$Date,format="%Y-%m-%d")

######### Sidebar Menu ###############
sidebar<- dashboardSidebar(width = 250,
                           sidebarMenu(id="tabs", selected = "sidebar_menu",
                                       menuItem("Date Range",tabName = "DatePicker"),
                                       dateRangeInput(
                                         inputId = "daterange",
                                         label = "Select Date",
                                         start = as.Date(min(msg$Date), format = '%Y-%m-%d'),
                                         end = as.Date(max(msg$Date), format = '%Y-%m-%d'),
                                         min = min(msg$Date),
                                         max = max(msg$Date)
                                       ),
                                       actionButton("submit_btn", "Submit")
                           )
)

############### Dashboard body #############
body<-dashboardBody(
  tabItems(
    tabItem(tabName = "DatePicker",
            tabsetPanel(type = "tabs", selected = "cash", id = "intabset1",
                        tabPanel("Cash", value = "cash",
                                 fluidRow(column(6,h3("Profit-Loss")),column(6,h3("Outcome"))),
                                 fluidRow(column(6,plotlyOutput("ProfitTableC")),column(6,plotlyOutput("ProfitTableC1"))),
                                 # fluidRow(column(12,h3("Outcome"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableC1"))),
                                 fluidRow(column(12,h3("Stock Performance"))),
                                 fluidRow(column(12,DT::dataTableOutput("StockTableC"))),
                                 fluidRow(h2(" ")),
                                 fluidRow(column(1, offset = 11,actionButton("Button1", "Next"))),
                                 fluidRow()
                        ),
                        tabPanel("Futures", value = "futures",
                                 fluidRow(column(6,h3("Profit-Loss")),column(6,h3("Outcome"))),
                                 fluidRow(column(6,plotlyOutput("ProfitTableF")),column(6,plotlyOutput("ProfitTableF1"))),
                                 # fluidRow(column(12,h3("Profit-Loss"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableF"))),
                                 # fluidRow(column(12,h3("Outcome"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableF1"))),
                                 fluidRow(column(12,h3("Stock Performance"))),
                                 fluidRow(column(12,DT::dataTableOutput("StockTableF"))),
                                 fluidRow(h2(" ")),
                                 fluidRow(column(2,offset = 0, actionButton("Button11", "Back")),
                                          column(1, offset = 9,actionButton("Button2", "Next"))),
                                 fluidRow()
                        ),
                        tabPanel("Options", value = "options",
                                 fluidRow(column(6,h3("Profit-Loss")),column(6,h3("Outcome"))),
                                 fluidRow(column(6,plotlyOutput("ProfitTableO")),column(6,plotlyOutput("ProfitTableO1"))),
                                 # fluidRow(column(12,h3("Profit-Loss"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableO"))),
                                 # fluidRow(column(12,h3("Outcome"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableO1"))),
                                 fluidRow(column(12,h3("Stock Performance"))),
                                 fluidRow(column(12,DT::dataTableOutput("StockTableO"))),
                                 fluidRow(h2(" ")),
                                 fluidRow(column(2,offset = 0, actionButton("Button21", "Back")),
                                          column(1, offset = 9,actionButton("Button3", "Next"))),
                                 fluidRow()
                        ),
                        tabPanel("Index", value = "index",
                                 fluidRow(column(6,h3("Profit-Loss")),column(6,h3("Outcome"))),
                                 fluidRow(column(6,plotlyOutput("ProfitTableI")),column(6,plotlyOutput("ProfitTableI1"))),
                                 # fluidRow(column(12,h3("Profit-Loss"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableI"))),
                                 # fluidRow(column(12,h3("Outcome"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableI1"))),
                                 fluidRow(column(12,h3("Stock Performance"))),
                                 fluidRow(column(12,DT::dataTableOutput("StockTableI"))),
                                 fluidRow(h2(" ")),
                                 fluidRow(column(2,offset = 0, actionButton("Button31", "Back")),
                                          column(1, offset = 9,actionButton("Button4", "Next"))),
                                 fluidRow()
                        ),
                        tabPanel("Overall", value = "overall",
                                 fluidRow(column(6,h3("Profit-Loss")),column(6,h3("Outcome"))),
                                 fluidRow(column(6,plotlyOutput("ProfitTableV")),column(6,plotlyOutput("ProfitTableV1"))),
                                 # fluidRow(column(12,h3("Outcome"))),
                                 # fluidRow(column(8,plotlyOutput("ProfitTableV1"))),
                                 fluidRow(column(12,h3("Segment-wise Performance"))),
                                 fluidRow(column(12,DT::dataTableOutput("StockTableV"))),
                                 fluidRow(h2(" ")),
                                 fluidRow(column(2,offset = 0, actionButton("Button5", "Back"))),
                                 fluidRow()
                        )
            )
    )
  )
)
################# User-Interface #######################
ui<- dashboardPage(
  dashboardHeader(title = "BULLSEYE HOLDINGS",titleWidth = 1305),
  sidebar,body
  
)
################# Server ######################
server<-function(input, output, session){
  
  ## Code for the reactive object "Action Buttons"
  observeEvent(input$Button1, {
    updateTabsetPanel(session, "intabset1", selected = "futures")
  })
  observeEvent(input$Button2, {
    updateTabsetPanel(session, "intabset1", selected = "options")
  })
  observeEvent(input$Button11, {
    updateTabsetPanel(session, "intabset1", selected = "cash")
  })
  
  observeEvent(input$Button21, {
    updateTabsetPanel(session, "intabset1", selected = "futures")
  })
  
  observeEvent(input$Button3, {
    updateTabsetPanel(session, "intabset1", selected = "index")
  })
  
  observeEvent(input$Button31, {
    updateTabsetPanel(session, "intabset1", selected = "options")
  })
  observeEvent(input$Button4, {
    updateTabsetPanel(session, "intabset1", selected = "overall")
  })
  observeEvent(input$Button5, {
    updateTabsetPanel(session, "intabset1", selected = "cash")
  })
  
###### Factor re-levelling & Color standardization #######  
  # Leveling
  msg$PnL=factor(msg$PnL,levels=c("Profit","Loss"))
  msg$Outcome=factor(msg$Outcome,levels=c("Buy","Sell","Alert","Tgt1","Tgt2","TSL","Profit Exit","Loss Exit","SL"))
  # Outcome color assignment---Color Standardization
  colors1=c("Blue","red4","yellow","lightgreen","darkgreen","orange","tan3","Maroon","red")
  names(colors1)=levels(msg$Outcome)
  # PnL color assignment----Color Standardization  
  colors <- c("darkgreen","red")
  names(colors)=levels(msg$PnL)
  
  ########### Cash Codes ############
  mesg <- eventReactive(input$submit_btn,{
    msg %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Cash")
  })
  Cprof<-reactive({mesg() %>%group_by(Outcome) %>%
      summarise(Count=n())})
  
  Cprof1<-reactive({mesg() %>%group_by(PnL) %>%
      summarise(Count=n())})
  
  Cstck<-reactive({mesg() %>% group_by(Date,Segment,Status,Symbol,Outcome,Return) %>%tally() %>% 
      pivot_wider(names_from = Outcome, values_from = n,  values_fill = list(n = 0)) %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Cash") %>% 
      group_by(Symbol) %>% 
      summarise(Tgt2=ifelse("Tgt2"%in%colnames(.),sum(Tgt2),0),
                TSL=ifelse("TSL"%in%colnames(.),sum(TSL),0),
                `Profit Exit`=ifelse("Profit Exit"%in%colnames(.),sum(`Profit Exit`),0),
                SL=ifelse("SL"%in%colnames(.),sum(SL),0),
                `Loss Exit`=ifelse("Loss Exit"%in%colnames(.),sum(`Loss Exit`),0),
                Total=(Tgt2+TSL+`Profit Exit`+SL+`Loss Exit`),
                Profit=(Tgt2+TSL+`Profit Exit`),
                Percent=100*round(Profit/Total,2),
                Return=round(sum(Return),2)) %>% 
      arrange(desc(Return),desc(Percent))})
  
  output$ProfitTableC<- renderPlotly({
    p1 <- plot_ly(Cprof1(), labels = ~PnL, values = ~Count, type = 'pie',
                  insidetextfont = list(color = '#FFFFFF'),
                  marker = list(colors =colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p1})
  
  output$ProfitTableC1<- renderPlotly({
    p2<-plot_ly(Cprof(), labels = ~Outcome, values = ~Count, type = 'pie',
                insidetextfont = list(color = '#FFFFFF'),
                marker = list(colors = colors1 ,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    p2
    
  })
  
  
  output$StockTableC<-DT::renderDataTable(datatable(Cstck()))
  
  ################ Futures codes ##############
  mesgF <- eventReactive(input$submit_btn,{
    msg %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Futures")
  })
  Fprof<-reactive({mesgF() %>%group_by(Outcome) %>%
      summarise(Count=n())})
  Fstck<-reactive({mesgF() %>% group_by(Date,Segment,Status,Symbol,Outcome,Return) %>%tally() %>% 
      pivot_wider(names_from = Outcome, values_from = n,  values_fill = list(n = 0)) %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Futures") %>% 
      group_by(Symbol) %>% 
      summarise(Tgt2=ifelse("Tgt2"%in%colnames(.),sum(Tgt2),0),
                TSL=ifelse("TSL"%in%colnames(.),sum(TSL),0),
                `Profit Exit`=ifelse("Profit Exit"%in%colnames(.),sum(`Profit Exit`),0),
                SL=ifelse("SL"%in%colnames(.),sum(SL),0),
                `Loss Exit`=ifelse("Loss Exit"%in%colnames(.),sum(`Loss Exit`),0),
                Total=(Tgt2+TSL+`Profit Exit`+SL+`Loss Exit`),
                Profit=(Tgt2+TSL+`Profit Exit`),
                Percent=100*round(Profit/Total,2),
                Return=round(sum(Return),2)) %>% 
      arrange(desc(Return),desc(Percent))})
  
  Fprof1<-reactive({mesgF() %>%group_by(PnL) %>%
      summarise(Count=n())})
  
  # Fstck<-reactive({mesgF() %>% group_by(Symbol) %>%
  #     summarise(Count=n(),Return=sum(Return)) %>% arrange(desc(Return))})
  # 
  output$ProfitTableF<- renderPlotly({
    p1 <- plot_ly(Fprof1(), labels = ~PnL, values = ~Count, type = 'pie',
                  insidetextfont = list(color = '#FFFFFF'),
                  marker = list(colors = list(colors),
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p1})
  
  output$ProfitTableF1<- renderPlotly({
    p2<-plot_ly(Fprof(), labels = ~Outcome, values = ~Count, type = 'pie',
                insidetextfont = list(color = '#FFFFFF'),
                marker = list(colors = colors1 ,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    p2
    
  })  
  
  output$StockTableF<-DT::renderDataTable(datatable(Fstck()))
  
  ############### Options Codes ################
  mesgO <- eventReactive(input$submit_btn,{
    msg %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Options")
  })
  Oprof<-reactive({mesgO() %>%group_by(Outcome) %>%
      summarise(Count=n()) })
  Ostck<-reactive({mesgO() %>% group_by(Date,Segment,Status,Symbol,Outcome,Return) %>%tally() %>% 
      pivot_wider(names_from = Outcome, values_from = n,  values_fill = list(n = 0)) %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Options") %>% 
      group_by(Symbol) %>% 
      summarise(Tgt2=ifelse("Tgt2"%in%colnames(.),sum(Tgt2),0),
                TSL=ifelse("TSL"%in%colnames(.),sum(TSL),0),
                `Profit Exit`=ifelse("Profit Exit"%in%colnames(.),sum(`Profit Exit`),0),
                SL=ifelse("SL"%in%colnames(.),sum(SL),0),
                `Loss Exit`=ifelse("Loss Exit"%in%colnames(.),sum(`Loss Exit`),0),
                Total=(Tgt2+TSL+`Profit Exit`+SL+`Loss Exit`),
                Profit=(Tgt2+TSL+`Profit Exit`),
                Percent=100*round(Profit/Total,2),
                Return=round(sum(Return),2)) %>% 
      arrange(desc(Return),desc(Percent))})
  
  Oprof1<-reactive({mesgO() %>%group_by(PnL) %>%
      summarise(Count=n())})
  
  
  output$ProfitTableO<- renderPlotly({
    p1 <- plot_ly(Oprof1(), labels = ~PnL, values = ~Count, type = 'pie',
                  insidetextfont = list(color = '#FFFFFF'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p1})
  
  output$ProfitTableO1<- renderPlotly({
    p2<-plot_ly(Oprof(), labels = ~Outcome, values = ~Count, type = 'pie',
                insidetextfont = list(color = '#FFFFFF'),
                marker = list(colors = colors1 ,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    p2
    
  })
  
  
  output$StockTableO<-DT::renderDataTable(datatable(Ostck()))
  
  ####################### Index codes ########################
  mesgI <- eventReactive(input$submit_btn,{
    msg %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Index")
  })
  Iprof<-reactive({mesgI() %>%group_by(Outcome) %>%
      summarise(Count=n())})
  Istck<-reactive({mesgI() %>% group_by(Date,Segment,Status,Symbol,Outcome,Return) %>%tally() %>% 
      pivot_wider(names_from = Outcome, values_from = n,  values_fill = list(n = 0)) %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3,Segment=="Index") %>% 
      group_by(Symbol) %>% 
      summarise(Tgt2=ifelse("Tgt2"%in%colnames(.),sum(Tgt2),0),
                TSL=ifelse("TSL"%in%colnames(.),sum(TSL),0),
                `Profit Exit`=ifelse("Profit Exit"%in%colnames(.),sum(`Profit Exit`),0),
                SL=ifelse("SL"%in%colnames(.),sum(SL),0),
                `Loss Exit`=ifelse("Loss Exit"%in%colnames(.),sum(`Loss Exit`),0),
                Total=(Tgt2+TSL+`Profit Exit`+SL+`Loss Exit`),
                Profit=(Tgt2+TSL+`Profit Exit`),
                Percent=100*round(Profit/Total,2),
                Return=round(sum(Return),2)) %>% 
      arrange(desc(Return),desc(Percent))})
  
  Iprof1<-reactive({mesgI() %>%group_by(PnL) %>%
      summarise(Count=n())})
  
  
  
  output$ProfitTableI<- renderPlotly({
    p1 <- plot_ly(Iprof1(), labels = ~PnL, values = ~Count, type = 'pie',
                  insidetextfont = list(color = '#FFFFFF'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p1})
  
  output$ProfitTableI1<- renderPlotly({
    p2<-plot_ly(Iprof(), labels = ~Outcome, values = ~Count, type = 'pie',
                insidetextfont = list(color = '#FFFFFF'),
                marker = list(colors = colors1 ,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>% 
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    p2
    
  })
  
  
  output$StockTableI<-DT::renderDataTable(datatable(Istck()))
  
  ################ Segment-wise performance #############
  mesgV <- eventReactive(input$submit_btn,{
    msg %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3)
  })
  Vprof<-reactive({mesgV() %>%group_by(Segment,Outcome) %>%
      summarise(Count=n())})
  
  Vprof1<-reactive({mesgV() %>%group_by(Segment,PnL) %>%
      summarise(Count=n())})
  
  Vstck<-reactive({mesgV() %>% group_by(Date,Segment,Status,Symbol,Outcome,Return) %>%tally() %>% 
      pivot_wider(names_from = Outcome, values_from = n,  values_fill = list(n = 0)) %>% filter((Date >= input$daterange[1] & Date <= input$daterange[2]),Status==3) %>% 
      group_by(Segment) %>% 
      summarise(Tgt2=ifelse("Tgt2"%in%colnames(.),sum(Tgt2),0),
                TSL=ifelse("TSL"%in%colnames(.),sum(TSL),0),
                `Profit Exit`=ifelse("Profit Exit"%in%colnames(.),sum(`Profit Exit`),0),
                SL=ifelse("SL"%in%colnames(.),sum(SL),0),
                `Loss Exit`=ifelse("Loss Exit"%in%colnames(.),sum(`Loss Exit`),0),
                Total=(Tgt2+TSL+`Profit Exit`+SL+`Loss Exit`),
                Profit=(Tgt2+TSL+`Profit Exit`),
                Percent=100*round(Profit/Total,2),
                Return=round(sum(Return),2)) %>% 
      arrange(desc(Return),desc(Percent))})
  
  output$ProfitTableV<- renderPlotly({
    p1 <- plot_ly(Vprof1(), labels = ~PnL, values = ~Count, type = 'pie',
                  insidetextfont = list(color = '#FFFFFF'),
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p1})
  
  output$ProfitTableV1<- renderPlotly({
    p2<-plot_ly(Vprof(), labels = ~Outcome, values = ~Count, type = 'pie',
                insidetextfont = list(color = '#FFFFFF'),
                marker = list(colors = colors1 ,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    p2
    
  })
  
  
  output$StockTableV<-DT::renderDataTable(datatable(Vstck()))
}

shinyApp(ui = ui, server = server)
