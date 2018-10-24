if(!("png" %in% installed.packages())){install.packages("png");library("png")}else{library("png")}
if(!("abind" %in% installed.packages())){install.packages("abind");library("abind")}else{library("abind")}
if(!("dplyr" %in% installed.packages())){install.packages("dplyr");library("dplyr")}else{library("dplyr")}
if(!("lubridate"%in%installed.packages())){install.packages("lubridate");library("lubridate")}else{library("lubridate")}

Start_Date <- "2018/01/01"
End_Date <- "2018/05/31"
setwd("~")
Eqn <- function(L = 1, xN = 0, k=1, x){
  return(L/(1+exp(1)^(-k*(x-xN))))
}
Class <- as.vector(read.table(file = "Classes/COMM107.txt", sep = "\n")$V1)

x <- as.vector(strsplit(Class[1], split = "\\|")[[1]])
Holding <- data.frame(x, row.names = c("Date", "Assignment Name", "Time", "Effort"))
colnames(Holding)[1] <- 1
Blank_Slate <- data.frame(t(Holding))
for(each in 2:length(Class)){
  x <- as.vector(strsplit(Class[each], split = "\\|")[[1]])
  Holding <- data.frame(x, row.names = c("Date", "Assignment Name", "Time", "Effort"))
  colnames(Holding)[1] <- each
  Holding <- data.frame(t(Holding))
  
  Blank_Slate <- as.data.frame(abind(Blank_Slate, Holding, along = 1))
}
Blank_Slate[,1] <- as.POSIXct(toupper(Blank_Slate[,1]), format = "%b %d, %Y")
write.csv(x = Blank_Slate, file = "Classes/COMM107.csv")

Days <- as.POSIXct(seq(from = as.POSIXct(Start_Date), to = as.POSIXct(End_Date), by = "day"))
Stress <- rep(x = 0, length(Days))
StressCalendar <- data.frame(Days, Stress)

#Calculate Stress Levels

for(row in 1:nrow(Blank_Slate)){
  Effort = as.numeric(Blank_Slate$Effort[row])
  Time = as.numeric(Blank_Slate$Time[row])
  length <- (-30:30)
  Stress_Over_Time <- c()
  for(each in length){
    Stress_Over_Time <- c(Stress_Over_Time, Eqn(L = (Effort+Time*.25), k = .2, x = each))
  }
  
  Days <- ceiling(Time*.9+Effort*.19)
  
  v <- round(length(Stress_Over_Time)/(Days))
  bucket_values <- c()
  buckets = 0
  while(buckets < Days){
    bucket_values <- c(bucket_values, max(Stress_Over_Time[((buckets)*v):((buckets+1)*v)], na.rm = T))
    buckets <- buckets+1
  }
  
  Matched_Day <- match(x = as.Date(Blank_Slate$Date[row]), table = as.Date(StressCalendar$Days))
  #Fill in stress chart backwards
  Corrosponding <- rep(0, length(Stress))
  Corrosponding[(Matched_Day-Days+1):Matched_Day] <- bucket_values
  Stress <- Stress+Corrosponding
}


plot(Stress, type = "l", lwd = 3)



# This now assumes you have all of the classes you want
 

StressCalendar$Stress <- sqrt(Stress)

# Figure out how to save it here, without the CSV losing all the dates



# Begin constructing the Calendar
Months <- unique(month(StressCalendar$Days))
Month_Names <- month.name[Months]
for(each in Months){
  Matching_Dates <- StressCalendar[month(StressCalendar$Days)==each,]
  Matching_Dates$Weekday <- weekdays(Matching_Dates$Days)
  #Start on sunday
  while(weekdays(Matching_Dates$Days[1]) != "Sunday"){
    b <- Matching_Dates[1,]
    b[1,1] <- Matching_Dates$Days[1]-(Matching_Dates$Days[2]-Matching_Dates$Days[1])
    b[1,2] <- Matching_Dates$Stress[1]-Matching_Dates$Stress[1]
    b[1,3] <- weekdays(Matching_Dates$Days[1]-(Matching_Dates$Days[2]-Matching_Dates$Days[1]))
    rownames(b)[1] <- as.numeric(rownames(b)[1])-1
    Matching_Dates <- as.data.frame(abind(b, Matching_Dates, along = 1), stringsAsFactors = F)
    Matching_Dates[,1] <- as.POSIXct(Matching_Dates[,1])
    Matching_Dates[,2] <- as.numeric(Matching_Dates[,2])
  }
  while(weekdays(Matching_Dates$Days[nrow(Matching_Dates)]) != "Saturday"){
    b <- Matching_Dates[nrow(Matching_Dates),]
    b[1,1] <- Matching_Dates$Days[nrow(Matching_Dates)]+(Matching_Dates$Days[nrow(Matching_Dates)]-Matching_Dates$Days[nrow(Matching_Dates)-1])
    b[1,2] <- 0
    b[1,3] <- weekdays(b$Days[1])
    rownames(b)[1] <- as.numeric(rownames(b)[1])+1
    Matching_Dates <- as.data.frame(abind(Matching_Dates, b, along = 1), stringsAsFactors = F)
    Matching_Dates[,1] <- as.POSIXct(Matching_Dates[,1])
    Matching_Dates[,2] <- as.numeric(Matching_Dates[,2])
  }
  max_stress <- max(Matching_Dates$Stress)
  #Picture Logic 
  C_Month <- readPNG(source = paste("PNGs/Month_", each, ".png", sep = ""))[,,1:3]
  C_Month <- abind(C_Month, readPNG("PNGs/T.png")[,,1:3], along = 1)
  for(Row in 1:ceiling(nrow(Matching_Dates)/7)){
    Week <- readPNG("PNGs/Week.png")[,,1:3]
    for(Day in 1:7){
      RowinMatchingDates <- (Row-1)*7+Day
      starting_index <- 1+(Day-1)*150
      ending_index <- Day*150
      Date <- readPNG("PNGs/Date.png")[,,1:3]
      if(month(Matching_Dates$Days[RowinMatchingDates])==Months[each]){
        Date[1:30, 1:30,1:3] <- readPNG(source = paste("PNGs/", day(Matching_Dates[RowinMatchingDates,1]), ".png", sep = ""))[,,1:3]
        Date[,,2:3] <- Date[,,2:3]*(max_stress-Matching_Dates$Stress[RowinMatchingDates])/max_stress
      } else {
        Date[,,1:3] <- Date[,,1:3]*.5
      }
      Week[,starting_index:ending_index,] <- Date
    }
    C_Month <- abind(C_Month, Week, along = 1)
  }
  writePNG(image = C_Month, target = paste(month.name[Months[each]], ".png", sep = ""))
}


 