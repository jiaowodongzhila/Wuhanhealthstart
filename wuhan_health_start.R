###部署到云端
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='wmy17801084708',
                          token='F95EC044CE9BF32AE8AE493EAAE0C659',
                          secret='xJySpjB3rLbBGWOqTf58wM87Cskgsol49P8MG4WJ')



## app.R ##
setwd("C:/Users/Administrator/Desktop/shiny")
data <- readxl::read_xlsx("data - 副本.xlsx")
dataformal <- data[,c(1:6)]
dataformal$地区[dataformal$地区=="江汉"] <- 1
dataformal$地区[dataformal$地区=="汉阳"] <- 2
dataformal$地区 <- as.numeric(dataformal$地区 )
###Map
library(leaflet)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(shinymanager)

# 提供的幼儿园数据
kindergartens <- data.frame(
  name = c("六渡桥幼儿园", 
           "尚秀幼儿园", "95133机场幼儿园",
           "大兴路幼儿园", "总工会幼儿园",
           "童话幼儿园（南苑）", "北湖幼儿园",
           "晶晶锦绣幼儿园", "星连星幼儿园", "七色光幼儿园",
           "花前树下幼儿园", "五洲幼儿园", "大桥局幼儿园",
           "知音幼儿园", "二桥幼儿园", "黄陂街幼儿园", "稚雅幼儿园", 
           "二桥（琴台园区)幼儿园", "馨苑幼儿园", "新星港湾幼儿园", 
           "华苑幼儿园", "启慧幼儿园", "红苗幼儿园",
           "童趣幼儿园", "稚美幼儿园", "新稚雅幼儿园", "振兴幼儿园","海虹景幼儿园",
           "沿江一号幼儿园","童乐幼儿园","惠雯幼儿园"),
  lng = c(114.28, 
          114.28, 114.25, 
          114.29, 114.27, 114.26, 114.27,
          114.25, 114.26, 114.27, 114.24, 114.26, 114.27,
          114.22, 114.21, 114.29, 114.26, 114.25, 114.25, 114.20, 
          114.26, 114.28, 114.28, 114.24, 114.28, 114.27,
          114.25, 114.28, 114.29,114.26, 114.25),
  lat = c(30.57, 
          30.59, 30.59, 
          30.57, 30.59, 30.62, 30.60, 
          30.62, 30.63, 30.54, 30.56, 30.63, 30.55, 
          30.57, 30.57, 30.57, 30.62, 30.55, 30.63, 30.56, 
          30.63, 30.61, 30.58, 30.61, 30.58, 30.61, 
          30.60, 30.61, 30.57, 30.63, 30.57)
)

mapplot1 <- leaflet(kindergartens) %>%
  addTiles() %>%
  addMarkers(
    lng = ~lng,
    lat = ~lat,
    popup = ~name,
    label = ~name,
    clusterOptions = markerClusterOptions()
  )

###
credentials <- data.frame(
  user = c("whhealthstart", "shinymanager"), # mandatory
  password = c("whhealthstart", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)


###Shiny
library(shiny)
library(shinydashboard)
ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")
)

ui <- secure_app(
  dashboardPage(skin = "yellow",
                dashboardHeader(title = "Wuhan Health Start Project",titleWidth = "350"),
                dashboardSidebar(
                  sidebarMenu(
                    id = "menu",
                    menuItem("地区", tabName = "location", icon = icon("dashboard"),
                             menuSubItem("江汉", tabName = "Location1"),
                             menuSubItem("汉阳", tabName = "Location2"),
                             menuSubItem("全部", tabName = "Location3")
                    )
                  )
                ),
                dashboardBody(
                  fluidRow(
                    infoBoxOutput("box1"),
                    infoBoxOutput("box2"),
                    infoBoxOutput("box3"),
                    infoBoxOutput("box4"),
                    infoBoxOutput("box5"),
                    infoBoxOutput("box6")
                  ),
                  fluidRow(
                    box(width = NULL, solidHeader = TRUE,
                        leafletOutput("map1", height = 500))
                  ),
                  fluidRow(fluidRow(
                    box(width = 6,height = "500px", status = "info",plotOutput("plot1")),
                    box(width = 6, height = "500px",tableOutput("table1")) # Add tableOutput here
                  ),
                  box(width = 6,height = "500px", status = "info",plotOutput("plot2"))
                  )
                )
  )
)

server <- function(input, output,session) { 
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  # your classic server logic
  output$map1 <- renderLeaflet({
    leaflet(kindergartens) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        popup = ~name,
        label = ~name,
        clusterOptions = markerClusterOptions()
      )
  })
  
  filtered_table1 <- reactive({
    req(input$menu )  # make sure input$tabName is available
    
    if (input$menu  == "Location1") {
      subset(data, 地区 == "江汉")
    } else if (input$menu  == "Location2") {
      subset(data, 地区 == "汉阳")
    } else if (input$menu  == "Location3"){
      data
    } 
  })
  output$table1 <- renderTable({  # Change output ID here
    filtered_table1()
  })
  
  ###plot2
  filtered_plot2 <- reactive({
    req(input$menu )  # make sure input$tabName is available
    
    if (input$menu  == "Location1") {
      datasubset <- subset(dataformal, 地区 == 1)
      transposed_data <- datasubset %>%
        pivot_longer(-幼儿园, names_to = "变量", values_to = "数值")      
      names(transposed_data)[2] <- "Factor"
      names(transposed_data)[3] <- "Value"
      transposed_data <- transposed_data[-which(transposed_data$Factor=="地区"),]
      ggplot(transposed_data, aes(x =幼儿园 , y = Value, fill =Factor )) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "幼儿园", y = "Value") +
        scale_fill_discrete(name = "Factor") 
    } 
    else if (input$menu  == "Location2") {
      datasubset <- subset(dataformal, 地区 == 2)
      transposed_data <- datasubset %>%
        pivot_longer(-幼儿园, names_to = "变量", values_to = "数值")      
      names(transposed_data)[2] <- "Factor"
      names(transposed_data)[3] <- "Value"
      transposed_data <- transposed_data[-which(transposed_data$Factor=="地区"),]
      ggplot(transposed_data, aes(x =幼儿园 , y = Value, fill =Factor )) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "幼儿园", y = "Value") +
        scale_fill_discrete(name = "Factor") 
    } 
    else if (input$menu  == "Location3"){
      datasubset <- dataformal
      transposed_data <- datasubset %>%
        pivot_longer(-幼儿园, names_to = "变量", values_to = "数值")      
      names(transposed_data)[2] <- "Factor"
      names(transposed_data)[3] <- "Value"
      transposed_data <- transposed_data[-which(transposed_data$Factor=="地区"),]
      ggplot(transposed_data, aes(x =幼儿园 , y = Value, fill =Factor )) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "幼儿园", y = "Value") +
        scale_fill_discrete(name = "Factor") 
      
    } 
  })
  
  output$plot2 <- renderPlot({  # Change output ID here
    filtered_plot2()
  })
  
  ###plot1
  filtered_plot1 <- reactive({
    req(input$menu )  # make sure input$tabName is available
    
    if (input$menu  == "Location1") {
      datasubset <- subset(dataformal, 地区 == 1)
      datasubset$"体格缺失率" <- (datasubset$小班人数-datasubset$体格测量)*100/datasubset$小班人数
      datasubset$"心理缺失率" <- (datasubset$小班人数-datasubset$心理测量)*100/datasubset$小班人数
      datasubset$"问卷缺失率" <- (datasubset$小班人数-datasubset$小班问卷)*100/datasubset$小班人数
      transposed_data <- datasubset %>%
        pivot_longer(-幼儿园, names_to = "变量", values_to = "数值")      
      transposed_data <- transposed_data[which(transposed_data[[2]] %in% c("体格缺失率","心理缺失率","问卷缺失率")),]
      ggplot(transposed_data, aes(x =幼儿园 , y = 数值, fill =变量 )) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "幼儿园", y = "数值") +
        scale_fill_discrete(name = "变量") 
    } 
    else if (input$menu  == "Location2") {
      datasubset <- subset(dataformal, 地区 == 2)
      datasubset$"体格缺失率" <- (datasubset$小班人数-datasubset$体格测量)*100/datasubset$小班人数
      datasubset$"心理缺失率" <- (datasubset$小班人数-datasubset$心理测量)*100/datasubset$小班人数
      datasubset$"问卷缺失率" <- (datasubset$小班人数-datasubset$小班问卷)*100/datasubset$小班人数
      transposed_data <- datasubset %>%
        pivot_longer(-幼儿园, names_to = "变量", values_to = "数值")      
      transposed_data <- transposed_data[which(transposed_data[[2]] %in% c("体格缺失率","心理缺失率","问卷缺失率")),]
      ggplot(transposed_data, aes(x =幼儿园 , y = 数值, fill =变量 )) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "幼儿园", y = "数值") +
        scale_fill_discrete(name = "变量") 
    } 
    else if (input$menu  == "Location3"){
      datasubset <- dataformal
      datasubset$"体格缺失率" <- (datasubset$小班人数-datasubset$体格测量)*100/datasubset$小班人数
      datasubset$"心理缺失率" <- (datasubset$小班人数-datasubset$心理测量)*100/datasubset$小班人数
      datasubset$"问卷缺失率" <- (datasubset$小班人数-datasubset$小班问卷)*100/datasubset$小班人数
      transposed_data <- datasubset %>%
        pivot_longer(-幼儿园, names_to = "变量", values_to = "数值")      
      transposed_data <- transposed_data[which(transposed_data[[2]] %in% c("体格缺失率","心理缺失率","问卷缺失率")),]
      ggplot(transposed_data, aes(x =幼儿园 , y = 数值, fill =变量 )) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "幼儿园", y = "数值") +
        scale_fill_discrete(name = "变量") 
    } 
  })
  
  output$plot1 <- renderPlot({  # Change output ID here
    filtered_plot1()
  })
  ###infobox
  output$box1 <- renderInfoBox({
    infoBox(
      "体格测量人数", 
      sum(data$体格测量), 
      icon = icon("user")
    )
  })
  
  output$box2 <- renderInfoBox({
    infoBox(
      "心理测量人数", 
      sum(data$心理测量), 
      icon = icon("heart"),color = "red"
    )
  })
  
  output$box3 <- renderInfoBox({
    infoBox(
      "问卷回收数", 
      sum(data$小班问卷,na.rm = T),  
      icon = icon("envelope") ,color = "green"
    )
  })
  
  output$box4 <- renderInfoBox({
    infoBox(
      "体格缺失率", 
      round((sum(data$小班人数) -sum(data$体格测量))*100/sum(data$小班人数),3) ,
      icon = icon("user")
    )
  })
  
  output$box5 <- renderInfoBox({
    infoBox(
      "心理缺失率", 
      round((sum(data$小班人数) -sum(data$心理测量))*100/sum(data$小班人数),3) ,
      icon = icon("heart"),color = "red"
    )
  })
  
  output$box6 <- renderInfoBox({
    data2 <- data[-which(is.na(data$小班问卷)),]
    question_missing_rate <- round((sum(data2$小班人数) - sum(data2$小班问卷)) * 100 / sum(data2$小班人数), 3)
    infoBox(
      "问卷缺失率", 
      question_missing_rate,
      icon = icon("envelope"),color = "green"
    )
  })
  
}

shinyApp(ui, server)

