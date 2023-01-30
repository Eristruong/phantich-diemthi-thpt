
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Phân tích điểm thi THPT', titleWidth = 315),
    dashboardSidebar(
      width = 215,
      sidebarUserPanel(
        name     = 'Phúc Trương',
        image    = 'profile.jpg', 
        subtitle = h6('Data analyze')),
      sidebarMenu(
        menuItem(tabName = 'home',        'Home'             , icon = icon('fas fa-home')),
        menuItem(tabName = 'dataset',   'Load dataset'     , icon = icon('fas fa-table')),
        menuItem(tabName = 'chars', 'Biểu đồ '         , icon = icon('fas fa-chart-line')),
        menuItem(tabName = 'regression', 'Mô hình hồi quy'  , icon = icon('fas fa-chart-bar')),
        menuItem(tabName = 'compare',       'So sánh'          , icon = icon('fas fa-sort-amount-up')),
        menuItem(tabName = 'about',       'About'            , icon = icon('fas fa-info-circle'))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = 'home', h1('Tong quan ung dung'),
          fluidRow(
            box(
              outputId = 'home1',
              width = 12,
              h4(textOutput('home1')))
          ),
          fluidRow(
            
          )
        ),
        tabItem(
          tabName = 'dataset', h1('Dataset diem thi'),
          fluidRow(
            column(4,
                   fileInput("upload", NULL, accept = c(".csv", ".tsv")),
                 
            ),
            column(
              8,
              actionButton("btnClean", "Lam sach du lieu", class = "btn btn-success"),
              actionButton("btnXeploai", "Xep loai", class = "btn btn-success"),
              actionButton("btnXettn", "Xet tot nghiep", class = "btn btn-success")

            )
            
          ),
          fluidRow(
            column(
              2,
              radioButtons("typequery", "Truy van dataset", c("mon co diem cao nhat", "mon co diem thap nhat", "test"),)
            ),
            column(
              10,
              box(
                width = 15,
                dataTableOutput("table")
              )
            )
          )
          
        ),
        tabItem(
          tabName = "chars", h1("Biểu đồ"),
          fluidRow(
            column(
              6,
              box(
                title = "biểu đồ pho diem", width = NULL, background = "green",
                "CHỌN BIỂU ĐỒ",
                selectizeInput(
                  inputId = 'typechars',"",
                  choices = c("Bar plot", "Pie Chart", "Histogram","Line Graph"))
              )
            ),
            column(
              6,
              box(
                title = "biểu đồ phân bố điểm thi các môn", width = NULL, background = "purple",
                "CHỌN BIỂU ĐỒ",
                selectizeInput(
                  inputId = 'typechars2',"",
                  choices = c("Mon toan", "Mon van", "Mon li","Mon hoa","Mon sinh","Mon tieng anh"))
              )
            )
          ),
          fluidRow(
            tabBox(
              id = "tabbox",
              width = 20,
              tabPanel('Bieu do', plotOutput('Bieudo')),
              tabPanel('Pho diem', plotlyOutput('Phodiem')),
              tabPanel('Table', DT::dataTableOutput('Tablepanel'))
            )
          )
        ),
        tabItem(
          tabName = "regression", h1("Hồi quy tuyến tính"),
          fluidRow(
            column(
              2,
              selectInput("cols11", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
              selectInput("cols12", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
              radioButtons("regmethod", "Select Method:", choices = c("Fit", "Summary")), 
              hr()
            ),
            column(
              10,
              box(
                div(
                  verbatimTextOutput("regout")
                ),
                div(
                  plotOutput("regplot")
                )
              )
            )
          )
        )
        
        
        
        
      )
      
    )
  )
)
