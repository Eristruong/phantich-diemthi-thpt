#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(formattable)
library(plotly)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  cleanData <- reactive({
    cleanData <- na.omit(data())
    cleanData
  })
  dataxeploai <- reactive({
    cleanData1 <- cleanData()
    data_xeploai <- Arrange(cleanData1)
    data_xeploai
  })
  dataxettn <- reactive({
    dataxettn1 <- dataxeploai()
    dataxettn1$Result <- ifelse(dataxettn1$Means>=4,"Tot nghiep","Rot")
    data.frame(dataxettn1)
  })

  observeEvent(input$upload,{
    output$table <- renderDataTable(data(), options = list(pageLength = 10))
  })

  observeEvent(input$btnClean, {
    if(req(input$btnClean)){
      output$table <- renderDataTable(cleanData(), options = list(pageLength = 10))
    }
  })
  # xep loai hoc tap dua theo diem trung binh
  observeEvent(input$btnXeploai, {
    if(req(input$btnXeploai)){
      output$table <- renderDataTable(dataxeploai(), options = list(pageLength = 10))
    }
})
  # xet tot nghiep dua theo diem trung binh
  observeEvent(input$btnXettn, {
    if(req(input$btnXettn)){
      output$table <- renderDataTable(dataxettn(), options = list(pageLength = 8))
    }
    
  })
  observeEvent(input$typequery, {
    if(input$typequery == "mon co diem cao nhat"){
      data_new4 <- dataxettn()
      data_new4$Max_point<-apply(data_new4[,5:10],1,max)
      data_new4$Name_max_point<-colnames(data_new4[,5:10])[apply(data_new4[,5:10],1,which.max)]
      
      output$table <- renderDataTable(data_new4, options = list(pageLength = 8))
    }else if(input$typequery == "mon co diem thap nhat"){
      data_new5 <- dataxettn()
      data_new5$Min_point<-apply(data_new5[,5:10],1,min)
      data_new5$Name_min_point<-colnames(data_new5[,5:10])[apply(data_new5[,5:10],1,which.min)]
      output$table <- renderDataTable(data_new5, options = list(pageLength = 8))
    }
    
  })
  # Xuat bieu do pho diem thi cac mon
  observeEvent(input$typechars2,{
    data_new5 <- dataxettn()
    phodiemplot <- function(xx, title){
      phodiemmon <- data_new5 %>%
        dplyr::group_by(phodiem = cut(xx, breaks = seq(0, 10, 0.1))) %>%
        dplyr::summarise(soluong = n())
      
      p <- ggplot(phodiemmon, aes(x = phodiem, y = soluong)) +
        geom_col() + geom_text(aes(label = signif(soluong, digits = 3)), vjust = 4, colour = "black") + geom_bar(stat = "identity",  lwd = 0.4, fill = 4) + ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text=element_text(size=15),
              axis.title=element_text(size=20),
              plot.title = element_text(size=20)) +
        theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
        xlab("Phổ điểm") + ylab("Số lượng điểm")
       fig <- ggplotly(p) 
    }
    
    if(input$typechars2 == "Mon toan"){
      data_new5 <- dataxettn()
      p <- phodiemplot(data_new5$maths, "Biểu đồ phân bố điểm thi môn toán")
      output$Phodiem <- renderPlotly(p)
      output$Tablepanel <- DT::renderDataTable({
        phodiemmon <- data_new5 %>%
          dplyr::group_by(phodiem = cut(data_new5$maths, breaks = seq(0, 10, 0.3))) %>%
          dplyr::summarise(soluong = n())
        phodiemmon
      })
      
    }else if(input$typechars2 == "Mon van"){
      data_new5 <- dataxettn()
      p <- phodiemplot(data_new5$Literature, "Biểu đồ phân bố điểm thi môn văn")
      output$Phodiem <- renderPlotly(p)
      output$Tablepanel <- DT::renderDataTable({
        phodiemmon <- data_new5 %>%
          dplyr::group_by(phodiem = cut(data_new5$Literature, breaks = seq(0, 10, 0.3))) %>%
          dplyr::summarise(soluong = n())
        phodiemmon
      })
      
    }else if(input$typechars2 == "Mon li"){
      data_new5 <- dataxettn()
      p <- phodiemplot(data_new5$Physics, "Biểu đồ phân bố điểm thi môn lí")
      output$Phodiem <- renderPlotly(p)
      output$Tablepanel <- DT::renderDataTable({
        phodiemmon <- data_new5 %>%
          dplyr::group_by(phodiem = cut(data_new5$Physics, breaks = seq(0, 10, 0.3))) %>%
          dplyr::summarise(soluong = n())
        phodiemmon
      })
      
    }else if(input$typechars2 == "Mon hoa"){
      data_new5 <- dataxettn()
      p <- phodiemplot(data_new5$Chemistry, "Biểu đồ phân bố điểm thi môn hóa")
      output$Phodiem <- renderPlotly(p)
      output$Tablepanel <- DT::renderDataTable({
        phodiemmon <- data_new5 %>%
          dplyr::group_by(phodiem = cut(data_new5$Chemistry, breaks = seq(0, 10, 0.3))) %>%
          dplyr::summarise(soluong = n())
        phodiemmon
      })
      
    }else if(input$typechars2 == "Mon sinh"){
      data_new5 <- dataxettn()
      p <- phodiemplot(data_new5$Biology, "Biểu đồ phân bố điểm thi môn sinh")
      output$Phodiem <- renderPlotly(p)
      output$Tablepanel <- DT::renderDataTable({
        phodiemmon <- data_new5 %>%
          dplyr::group_by(phodiem = cut(data_new5$Biology, breaks = seq(0, 10, 0.3))) %>%
          dplyr::summarise(soluong = n())
        phodiemmon
      })
      
    }else {
      p <- phodiemplot(data_new5$Eng, "Biểu đồ phân bố điểm thi môn anh")
      output$Phodiem <- renderPlotly(p)
      output$Tablepanel <- DT::renderDataTable({
        phodiemmon <- data_new5 %>%
          dplyr::group_by(phodiem = cut(data_new5$Eng, breaks = seq(0, 10, 0.3))) %>%
          dplyr::summarise(soluong = n())
        phodiemmon
      })
    }
    
  })
  # xuat bieu do tròn
  observeEvent(input$typechars, {
    if(req(input$typechars == "Pie Chart")){
      output$Bieudo <- renderPlot({
        data_new5 <- dataxettn()
        newTable <- table(data_new5$classification)
        ## phần trăm 
        totalPercent <- table(data_new5$classification) / nrow(data_new5)
        percentResult <- percent(totalPercent)
        percentResult
        ResultTable <- data.frame(
          "brand" = newTable,
          "percentt" = percentResult
        )
        ResultTable = subset(ResultTable, select = -c(3))
        brand <- ResultTable$brand.Var1
        percentt <- ResultTable$percentt.Freq
        
        ggplot(ResultTable, aes(x="", y=percentt, fill=brand)) +
          geom_bar(stat="identity", color = "white") +
          coord_polar("y", start=0) +
          geom_text(aes(label = paste0(percentt, "")), position = position_stack(vjust=0.5)) +
          labs(x = NULL, y = NULL) +
          theme_classic() +
          theme(axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank()) +
          scale_fill_brewer(palette="Greens", name = "danh hieu") + ggtitle("Phan bo loai danh hieu") + theme(plot.title = element_text(hjust = 0.5, size = 20),axis.title = element_blank(),
                                                                                                              axis.text = element_blank(),
                                                                                                              axis.ticks = element_blank(),
                                                                                                              panel.grid=element_blank(),
                                                                                                              panel.border = element_blank()) 

      })
      
      output$Tablepanel <- DT::renderDataTable({
        data_new5 <- dataxettn()
        newTable <- table(data_new5$classification)
        ## phần trăm 
        totalPercent <- table(data_new5$classification) / nrow(data_new5)
        percentResult <- percent(totalPercent)
        ResultTable <- data.frame(
          "brand" = newTable,
          "percentt" = percentResult
        )
        ResultTable = subset(ResultTable, select = -c(3))
        ResultTable
      })
      
    }
  })
  
   #xuat bieu do cot
  observeEvent(input$typechars, {
    if(req(input$typechars == "Bar plot")){
      output$Bieudo <- renderPlot({
        data_new5 <- dataxettn()
        # tinh diem trung binh các môn dựa trên điểm cua tat cac cac mon thi ve bieu do diem thi trung binh cua tat ca các môn học
        subjectsMean <- c(mean(data_new5$maths), mean(data_new5$Literature), mean(data_new5$Physics), mean(data_new5$Chemistry), mean(data_new5$Biology), mean(data_new5$Eng))
        # or
        subjectsMean <- apply(data_new5[,5:10],2,mean) 
        
        df <- data.frame(sname = colnames(data_new5[,5:10]),
                         smean = subjectsMean)
        library(ggplot2)
        
        ggplot(df, aes(x = sname, y = smean, fill = sname)) +
          geom_col() + geom_text(aes(label = signif(smean, digits = 3)), vjust = -1, colour = "black") + guides(fill = guide_legend(title = "Môn học"))
        
        
       
      })
      
      output$Tablepanel <- DT::renderDataTable({
        
        data_new5 <- dataxettn()
        # tinh diem trung binh các môn dựa trên điểm cua tat cac cac mon thi ve bieu do diem thi trung binh cua tat ca các môn học
        subjectsMean <- c(mean(data_new5$maths), mean(data_new5$Literature), mean(data_new5$Physics), mean(data_new5$Chemistry), mean(data_new5$Biology), mean(data_new5$Eng))
        # or
        subjectsMean <- apply(data_new5[,5:10],2,mean) 
        
        df <- data.frame(smean = subjectsMean)
        df
        
      })
      
    }
  })
  
  observeEvent(input$typechars, {
    if(req(input$typechars == "Histogram")){
      data_new5 <- dataxettn()
      output$Bieudo <- renderPlot({
        hist.plotter <- function(xx, xlabel, title){
          p <- ggplot(data_new5, aes(x = xx)) + 
            geom_histogram(colour = 4, fill = "white", binwidth = 0.15) + ggtitle(title)+theme(plot.title = element_text(hjust = 0.5)) + xlab(xlabel) + ylab("So luong") + 
            theme(axis.text=element_text(size=20),
                  
                  axis.title=element_text(size=20),
                  
                  plot.title = element_text(size=20))
          p
        }
        p1 <- hist.plotter(data_new5$maths, "Maths", "Maths Histogram")
        p2 <- hist.plotter(data_new5$Literature, "Literature", "Literature Histogram")
        p3 <- hist.plotter(data_new5$Physics, "Physics", "Physics Histogram")
        p4 <- hist.plotter(data_new5$Chemistry, "Chemistry", "Chemistry Histogram")
        p5 <- hist.plotter(data_new5$Biology, "Biology", "Biology Histogram")
        p6 <- hist.plotter(data_new5$Eng, "English", "English Histogram")
        
        
        library(gridExtra)
        p <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
        p
        
      })
      output$Tablepanel <- DT::renderDataTable({
        Data_Frame <- data.frame (
          PhoDiem = c(" 0 - 2", " 2 - 4", " 4 - 6", " 6 - 8","8 - 10"),
          Maths = tinhphodiemmon(data_new5$maths), 
          Literature = tinhphodiemmon(data_new5$Literature),
          Physics = tinhphodiemmon(data_new5$Physics),
          Chemistry = tinhphodiemmon(data_new5$Chemistry),
          Biology = tinhphodiemmon(data_new5$Biology),
          Eng = tinhphodiemmon(data_new5$Eng)
        )
        # Print the data frame  
        Data_Frame
      })
      
    }
  })
  #backend cua mo hinh hoi quy tuyen tinh
  observeEvent(input$upload, {
    
    updateSelectInput(session, inputId = "cols11", choices = names(dataxettn()))
    updateSelectInput(session, inputId = "cols12", choices = names(dataxettn()))
  })
  reganal <- reactive({
    df <- dataxettn()
    var1 <- df[, input$cols11]
    var2 <- df[, input$cols12]
    rego <- lm(var1 ~ var2, data = df)
    return(list(fit = rego, fitsum = summary(rego), anov = anova(rego)))
    
  })
  output$regout <- renderPrint({
    if (input$regmethod == "Fit"){
      reganal()$fit
    } else if(input$regmethod == "Summary"){
      reganal()$fitsum
    } else if(input$regmethod == "ANOVA"){
      reganal()$anov
    }
  })
  output$regplot <- renderPlot({
    df <- dataxettn()
    var1 <- df[, input$cols11]
    var2 <- df[, input$cols12]
    plot(var1, var2); abline(lm(var1 ~ var2, data = df), col = "red", lwd=2)
  })
  
})

Arrange <- function(data_new1){
  
  data_new1$Means <- apply(data_new1[,5:10],1,mean)
  data_new1$Means[2] <- 7.000001
  
  data_new1$classification <- 0
  for (i in 1:nrow(data_new1)) {
    
    if(data_new1$Means[i] >= 7) {
      data_new1$classification[i] <- "Giỏi"
    } else if(data_new1$Means[i] >= 5) {
      data_new1$classification[i] <- "Khá"
    } else {
      data_new1$classification[i] <- "́Trung bình"
    }
    
  }
  return(data_new1)
  
}

#tính phổ điểm  của từng môn
tinhphodiemmon <- function(subjectData){
  
  VectorResult <- 1:5
  
  for (i in 1:5) {
    
    if (i < 2) {
      VectorResult[i] <- sum(subjectData < 2)
    }
    else if(i < 3 ){
      VectorResult[i] <- sum(subjectData >= 2 & subjectData <4)
    }
    else if(i < 4){
      VectorResult[i] <- sum(subjectData >= 4 & subjectData <6)
    }
    else if(i < 5){
      VectorResult[i] <- sum(subjectData >= 6 & subjectData <8)
    }
    else{
      VectorResult[i] <- sum(subjectData >= 8 & subjectData <= 10)
    }
    
    
  }
  return(VectorResult)
  
  
}
