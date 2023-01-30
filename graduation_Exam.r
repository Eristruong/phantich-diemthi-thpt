# dataset gom cac bien(cot) la year (nam thi), Birth_year (nam sinh), City(Dia chi), SBD(So bao danh),
#maths, Miterature, Physics, Chemistry, Biology, Eng lan luot la cac mon thi toan, van, ly, hoa, sinh, anh
# import data
data1 <- read.csv(file = "DiemTN.csv", header = TRUE, sep = ",",
                  na.strings = c(""))
#View(data1)

#nrow(data1) # xem hang 
#ncol(data1) # xem cot

# kiem tra du lieu rong
#sum(is.na(data1))

# kiem tra cot rong
#colSums(is.na(data1))

# Kiem tra hang co du lieu rong
#missingdata <- data1[!complete.cases(data1), ]
#sum(is.na(missingdata))

# xu ly du lieu bi thieu
# xoa moi NA ra khoi du lieu
cleanData <- na.omit(data1)
sum(is.na(cleanData))

# Them cot moi co truong la diem trung binh cua cac mon cua hoc sinh
data_new1 <- cleanData
data_new1$Means <- apply(cleanData[,5:10],1,mean)
data_new1

# ham xep loai theo diem trung binh
Arrange <- function(data_new1){
  
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
# xep loai hoc tap dua theo diem trung binh
data_new1$Means[2] <- 7.000001
data_new2 <- Arrange(data_new1)
data_new2
#

# xet tot nghiep dua theo diem trung binh
data_new3 <- data_new2
data_new3$Result <- ifelse(data_new1$Means>=4,"Tot nghiep","Rot")
data_new3

# tao cot lay ten va diem cao nhat
data_new4 <- data_new3
data_new4$Max_point<-apply(cleanData[,5:10],1,max)
data_new4$Name_max_point<-colnames(cleanData[,5:10])[apply(cleanData[,5:10],1,which.max)]

# tao cot lay ten va diem thap nhat
data_new5 <- data_new4
data_new5$Min_point<-apply(cleanData[,5:10],1,min)
data_new5$Name_min_point<-colnames(cleanData[,5:10])[apply(cleanData[,5:10],1,which.min)]
data_new5

#truy van nhung ca nhan duoc cong nhan tot nghiep
result <- data_new5[data_new5$Result == "Tot nghiep", ]
#thong ke ca nhan co diem trung binh tren 5 va sap xep gia tri giam dan
data_new6 <- data_new5[data_new5$Means > 5, ]
data_new6 <- data_new6[order(data_new6$Means, decreasing = FALSE), ]

# tinh diem trung binh các môn dựa trên điểm cua tat cac cac mon thi ve bieu do diem thi trung binh cua tat ca các môn học
subjectsMean <- c(mean(data_new5$maths), mean(data_new5$Literature), mean(data_new5$Physics), mean(data_new5$Chemistry), mean(data_new5$Biology), mean(data_new5$Eng))
# or
subjectsMean <- apply(data_new5[,5:10],2,mean) 

df <- data.frame(sname = colnames(data_new5[,5:10]),
                 smean = subjectsMean)
library(ggplot2)

ggplot(df, aes(x = sname, y = smean, fill = sname)) +
  geom_col() + geom_text(aes(label = signif(smean, digits = 3)), vjust = -1, colour = "black") + guides(fill = guide_legend(title = "Môn học"))


#### bai 5 
# tinh trung binh cong cua cot Mean
mean(data_new5$Means)
#Lay diem cao nhat tu cot Mean
max(data_new5$Means, na.rm = TRUE)
#Lay diem thap nhat tu cot Mean
min(data_new5$Means, na.rm = TRUE)


mean_all <- apply(data_new5[,5:10],2,mean) 
#Diem trung binh mon cao nhat trong tat ca cac mon
max(mean_all, na.rm = TRUE)
#Diem trung binh mon thap nhat trong tat ca cac mon
min(mean_all, na.rm = TRUE)

## CÂU 1
#thông kế số lượng và giá trị phần trăm của danh hiệu 
## số lượng danh hiêu theo loại giỏi, khá, trung bình
#bieu do tron xep loai danh hieu
library("formattable")
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


library(readxl)
library(ggplot2)
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

##CÂU 2
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

#bieu do histogram pho diem cac mon
library(ggplot2)
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


#deloy web app for final exam

library(shiny)
ui <- fluidPage(
  "Hello, world!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)


