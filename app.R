library(shiny)
library(shinydashboard)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

#Post the path to your ledger-file here
#filepath<-"/path/to/ledger-file"
filepath<-"/Users/phil/Dropbox/Schnucki/Journal_ledger.txt"

# of course, if the .ledgerrc is modified accordingly, there's no need for the f-option
start<-paste("/usr/local/bin/ledger -f ",filepath," ",sep="")

get_options<-function(interval,begin,end){
  int<-switch (interval,
    "original" = "",
    "daily" = "-D",
    "weekly" = "-W",
    "monthly" = "-M"
  )
  paste(int,"-b",begin,"-e",end)
}

account_list<-function(account){
  accounts<-system(paste(start,"accounts", account), intern=T)
  accounts_filtered<-c("------",gsub(ifelse(account=="",":.*.",paste(account,":",sep="")),"",accounts)[!duplicated(gsub(ifelse(account=="",":.*.",paste(account,":",sep="")),"",accounts))])
  gsub(":.*.","",accounts_filtered)[!duplicated(gsub(":.*.","",accounts_filtered))]
  }

form<-function(command,amount)
  paste("--",command,"-format \"%(date)\t%(account)\t%(",amount,")\\n\"",sep = "")

cmd_ledger<-function(command,options,account,amount) {
  data<-unlist(strsplit(system(paste(start,command,options,account,form(command,amount)), intern=T),split="\t"))
  dates<-data[seq(1, length(data), by = 3)]
  accounts<-data[seq(2, length(data), by = 3)]
  values<-data[seq(3, length(data), by = 3)]
  data_final<-data.frame(as.Date(dates,format = "%Y/%m/%d"),
                         gsub(ifelse(account=="",account,paste(account,":",sep = "")),"",accounts),
                         as.numeric(gsub("€","",values)))
  names(data_final)<-c("Date","Account","Values")
  data_final
}

draw_balance<-function(commando,options,account,amount){
  cmd<-cmd_ledger(commando,options,account,amount)
  sep<-cmd[!grepl(":",cmd$Account) & cmd$Account!=account & cmd$Account!="" & cmd$Account!="Equity",2:3]
  ifelse(nrow(sep)>0,barplot(sep[[2]],names.arg = sep[[1]]),barplot(cmd[[3]],names.arg = cmd[[2]]))
}

draw_balance_pie<-function(commando,options,account,amount){
  cmd<-cmd_ledger(commando,options,account,amount)
  sep<-cmd[!grepl(":",cmd$Account) & cmd$Account!=account & cmd$Account!="" & cmd$Account!="Equity",2:3]
  pie(abs(sep[[2]]), sep[[1]], radius = 1)
}

draw_reg<-function(commando,options,account,amount){
  cmd<-cmd_ledger(commando,options,account,amount)
  sep<-cmd[cmd$Account!="",]
  plot(as.Date(sep[[1]],format="%y-%m-%d"),sep[[3]],xlab="Datum",ylab=account)
}

draw_devel<-function(commando, options, account, amount){
  test<-cmd_ledger(commando,options,account, amount)
  lab<-unlist(strsplit("Assets Liabilities",split = " "))
  barplot(test$Date[!duplicated(test$Date)],cumsum(sapply(X=test$Date[!duplicated(test$Date)], FUN=function(X) sum(test$Values[test$Date==X]))),xlab="Date",ylab=paste(lab[1],"+",lab[2]))
}

ui <- dashboardPage(
  dashboardHeader(title = "LedgeR"),
  dashboardSidebar(
    sidebarMenu(selectInput('account', 'Account level one', account_list(""))),
    sidebarMenu(uiOutput('account2')),
    sidebarMenu(uiOutput('account3')),
    sidebarMenu(selectInput('interval','Interval',c("original","daily","weekly","monthly"))),
    sidebarMenu(dateInput('begin','Start date', value=Sys.Date()-365)),
    sidebarMenu(dateInput('end','End date', value=Sys.Date())),    
    radioButtons('cumulative', "Cumulative Amounts?", c("amount", "display_total"), selected = "display_total", inline = FALSE)
  ),

  dashboardBody(
    # Boxes need to be put in a row (or column)
    navbarPage("",
      tabPanel("balances",
               box(plotOutput("plot1", height = 400)),
               column(6, DT::dataTableOutput('x1'))
      ),
      tabPanel("register",
               box(plotOutput("plot2", height = 400)),
               column(6, DT::dataTableOutput('x2'))
      )
    )
#     fluidRow(
#         box(plotOutput("plot1", height = 400)),
#         #box(verbatimTextOutput("test"))
#         #box(plotOutput("plot2", height = 400)),
#         column(6, DT::dataTableOutput('x1')),
#         box(plotOutput("plot2", height = 400)),
#         #box(verbatimTextOutput("test"))
#         #box(plotOutput("plot2", height = 400)),
#         column(6, DT::dataTableOutput('x2'))
#       #box(dataTableOutput("table"),width = 6,height = 400)
#       )
#     fluidRow(
#       dataTableOutput(outputId="table")
      
      
#    )
  )
)

server <- function(input, output) {

  output$plot1 <- renderPlot({
    draw_balance("balance",get_options(input$interval,input$begin,input$end),ifelse(input$account=="------","",paste(input$account,ifelse(input$account2=="------","",":"),ifelse(input$account2=="------","",input$account2),sep = "")),"display_total")
  })
  output$plot2 <- renderPlot({
    draw_reg("register",get_options(input$interval,input$begin,input$end),ifelse(input$account=="------","",paste(input$account,ifelse(input$account2=="------","",":"),ifelse(input$account2=="------","",input$account2),ifelse(input$account3=="------","",paste(":",input$account3,sep="")),sep = "")),input$cumulative)
  })  
  output$account2<-renderUI({
    selectInput('account2', 'Account level two', account_list(ifelse(input$account=="------","",input$account)))
  })
  output$account3<-renderUI({
    selectInput('account3', 'Account level three', account_list(ifelse(input$account=="------" | input$account2=="------","",paste(input$account,":",input$account2,sep=""))))
  })
  output$table<-
    renderDataTable({
      cmd_ledger("balance",
                 get_options(input$interval,input$begin,input$end),
                 ifelse(input$account=="------","",
                        paste(input$account,ifelse(input$account2=="------","",":"),
                              ifelse(input$account2=="------","",input$account2),
                              ifelse(input$account3=="------","",
                                     paste(":",input$account3,sep="")),sep = "")),
                 input$cumulative)
    })
  output$x1 <- DT::renderDataTable({
    cmd<-cmd_ledger("balance",get_options(input$interval,input$begin,input$end),ifelse(input$account=="------","",paste(input$account,ifelse(input$account2=="------","",":"),ifelse(input$account2=="------","",input$account2),ifelse(input$account3=="------","",paste(":",input$account3,sep="")),sep = "")),input$cumulative)
    sep<-cmd[!grepl(":",cmd$Account) & cmd$Account!=account & cmd$Account!="" & cmd$Account!="Equity",2:3]
    sep
    }, server = FALSE)
  output$x2 <- DT::renderDataTable({
    cmd<-cmd_ledger("register",get_options(input$interval,input$begin,input$end),ifelse(input$account=="------","",paste(input$account,ifelse(input$account2=="------","",":"),ifelse(input$account2=="------","",input$account2),ifelse(input$account3=="------","",paste(":",input$account3,sep="")),sep = "")),input$cumulative)
    sep<-cmd[cmd$Account!="",]
    sep
  }, server = FALSE)
  
  #output$test<-renderText({paste(input$account,ifelse(input$account2=="------","",":"),ifelse(input$account2=="------","",input$account2),sep = "")})
}


shinyApp(ui, server)