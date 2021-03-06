
rm(list= ls())

# Loading the library

library(tidyverse)
library(readxl)

# Load the data

path = "C:/Users/mpez/Documents/Projects/GFK-Norm"
setwd(path)

data <- read_excel("GfK Norm FullData.xlsx")
data <- as.data.frame(data)
data <- data[which(!is.na(data$`Time to First Fixation`)),]

# Calculate the percentage of first and last gaze points.

  
Min_gaze <- data %>%
  group_by(Stores, Participants) %>%
  summarise(AOI_min = AOI[which.min(`Time to First Fixation`)])


#a <- aggregate(`Time to First Fixation` ~ Stores+ Participants, data= data, FUN = min, na.action = na.omit)


AOI_reach <- Min_gaze %>%
  group_by(Stores) %>%
  mutate(GrpCount =n())%>%
  group_by(Stores, AOI_min, GrpCount) %>%
  summarise(Count = n(), percentage = round((Count/GrpCount[1L])*100 , 2))%>%
  arrange(Stores, desc(Count))

second_data <- data %>%
  select (Stores, Participants, AOI, `Time to First Fixation`) %>%
  group_by(Stores, Participants) %>%
  summarize(AOI_2nd = AOI[sort(`Time to First Fixation`, index.return  = T)$ix[2]])

AOI_reach_2nd <- second_data %>%
  group_by(Stores) %>%
  mutate(GrpCount =n())%>%
  group_by(Stores, AOI_2nd, GrpCount) %>%
  summarise(Count = n(), percentage = round((Count/GrpCount[1L])*100 , 2))%>%
  arrange(Stores, desc(Count))

#################


  
  

Min_data <- data %>%
            select (Data[1], Participants, AOI, `Time To First Fixation`, `First Fixation Duration`) %>%
            group_by(Shelfs, Participants) %>%
            summarize(AOI_min = AOI[which.min(`Time To First Fixation`)], Fixation_Duration_first = `First Fixation Duration`[which.min(`Time To First Fixation`)],

                                           AOI_max = AOI[which.max(`Time To First Fixation`)], Fixation_Duration_last = `First Fixation Duration`[which.max(`Time To First Fixation`)])



second_data <- data %>%
  select (Shelfs, Participants, AOI, `Time To First Fixation`) %>%
  group_by(Shelfs, Participants) %>%
  summarize(AOI_2nd = AOI[sort(`Time To First Fixation`, index.return  = T)$ix[2]])

n <- length(data$`Time To First Fixation`, na.rm = T)


length(x[!is.na(x)])


# the input vector 'x' must not contain -Inf value 
topMaxUsingWhichMax <- function(x, N) {
  vals <- c()
  for(i in 1:min(N, length(x))) {
    idx      <- which.max(x)
    vals     <- c(vals, x[idx]) # copy-on-modify (this is not an issue because idxs is relative small vector)
    x[idx]   <- -Inf            # copy-on-modify (this is the issue because data vector could be huge)
  }
  vals
}


I think the best solution in clean R is to use partial base::sort.

topMaxUsingPartialSort <- function(x, N) {
  N <- min(N, length(x))
  x[x >= -sort(-x, partial=N)[N]][1:N]
}

for (i in 1:n)
second_data <- data %>%
  select (Shelfs, Participants, AOI, `Time To First Fixation`) %>%
  group_by(Shelfs, Participants) %>%
  summarize(AOI_2nd = AOI[sort(`Time To First Fixation`, index.return  = T)$ix[2]])

table(Min_data[which(Min_data$Shelfs == "Snapshot_Shelf_1"),]$AOI_min)
table(Min_data[which(Min_data$Shelfs == "Snapshot_Shelf_2"),]$AOI_min)


table(Min_data[which(Min_data$Shelfs == "Snapshot_Shelf_1"),]$AOI_max)
table(Min_data[which(Min_data$Shelfs == "Snapshot_Shelf_2"),]$AOI_max)


first_point <- Min_data %>%
              group_by(Shelfs, AOI_min) %>%
              summarize(count = n(), percentage = round(((count/30)*100), digits = 2))%>%
              arrange(Shelfs, desc(count))

last_point<- Min_data %>%
            group_by(Shelfs, AOI_max) %>%
            summarize(count = n(), percentage = round(((count/30)*100), digits = 2))%>%
            arrange(Shelfs, desc(count))


write.csv(AOI_reach_2nd, file = "Second_gaze.csv",row.names=FALSE)
write.csv(last_point, file = "Last_gaze.csv",row.names=FALSE)

