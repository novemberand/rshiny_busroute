# Load packages 
library(shiny)
library(tidyverse)
library(stringr)
library(ggrepel)

# Load data
load("daramg.RData")
new_daramg <- read.csv("new_daramg.csv", header = TRUE)
p <- ggplot() + 
  geom_polygon(data = map, aes(x = long, y = lat, group = group),
               fill = '#FFFFF6', colour = "grey", alpha=.4) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

color_df <- data.frame(
  color = c('#344e5c','#003f5c','#2f4b7c','#665191','#a05195',
               '#d45087','#f95d6a','#ef4648','#ff7c43'),
  bus = c("8221", "8441", "8552", "8761", "8771", "8551", "8331", "1", "2"))


# User interface --------------------------------------------------
ui <- fluidPage(
  titlePanel("다람쥐 버스 기존노선과 신규제안 노선"),
  
  sidebarLayout(
    sidebarPanel(
 
      # 노선 선택 ------
      helpText("다람쥐버스 노선"),
      
      selectInput("route",
                  label = "노선번호 선택",
                  choices = c("8221 : 장안2동 주민센터-답십리역", 
                              "8441 : 은곡마을 - 수서역",
                              "8552 : 신림복지관 - 신림역",
                              "8761 : 신촌로터리 - 국회의사당",
                              "8771 : 구산중 - 녹번역",
                              "8551 : 봉천역 - 노량진역",
                              "8331 : 마천사거리 - 잠실역",
                              "신규노선 제안 1 : 은평구",
                              "신규노선 제안 2 : 서초구"),
                  
                  selected = "8221 : 장안2동 주민센터-답십리역"),
      
      # 승하차 구분 -----
      radioButtons("board", 
                   label = "승하차 구분",
                   choices = list("승차" = "s", "하차" = "h"),
                                  selected = "s"),
      
      # 평균 승하차 인원 수 정거장 필터링 -----
      sliderInput("mean", 
                  label = "정거장별 평균 승하차 인원 수",
                  min = 0, max = 4120, value = c(20, 388)),
      
      img(src = "daramg.png", height = 150, width = 200)),
    
    
    mainPanel(
      
      
      fluidRow(  
        column(10,
               h4(strong("다람쥐 버스 기존노선과 신규제안 노선 정거장 지도 시각화")),
               
               p("현재 운영되고 있는 다람쥐버스 기존 7개 노선의 정거장과
                 새롭게 제안하는 두가지 신규 노선의 정거장을 지도로 시각화하여 표시하였습니다."),
               
               plotOutput("map", width ="80%"))),
      
      fluidRow(
        column(12,
               h4(strong("노선별 정거장 승하차 승객수 살펴보기")),
               
               p("선택된 승하차 인원수에 해당하는 정거장이 승객 수가 많은 순으로 정렬됩니다."),
               
               dataTableOutput("station"))),
        

      
      )
  )
)


# Server logic --------------------------------------------------------
server <- function(input, output){
  
  stationInput <- reactive({
    new_daramg %>% filter(노선번호 == input$route)

  })
  
  colorInput <- reactive({
    as.vector(color_df[color_df$bus == parse_number(input$route), "color"])
  })
  
  output$map <- renderPlot({
    p + 
      geom_point(data = stationInput(), 
                 aes(x=X좌표, y=Y좌표), color = colorInput()) +
      #geom_text_repel(data =  stationInput(),
      #                aes(x=X좌표, y=Y좌표, label = 역명, color=colorInput()),
      #                max.overlaps=Inf) + 
      theme(legend.position = "none")
  })
  
  output$station <- renderDataTable({
    stationInput() %>%
      filter(board == input$board,
             mean >= input$mean[1], mean <= input$mean[2]) %>%
      select(역명, mean) %>% 
      arrange(desc(mean))
  }, 
  options = list(
    pageLength = 5))
  
}

# Run app -----------------------------------------------------------
shinyApp(ui, server)
