
# Calculate the percentage of first and last gaze points.

Min_data <- data %>%
            select (Shelfs, Participants, AOI, `Time To First Fixation`, `First Fixation Duration`) %>%
            group_by(Shelfs, Participants) %>%
            summarize(AOI_min = AOI[which.min(`Time To First Fixation`)], Fixation_Duration_first = `First Fixation Duration`[which.min(`Time To First Fixation`)],
                      AOI_max = AOI[which.max(`Time To First Fixation`)], Fixation_Duration_last = `First Fixation Duration`[which.max(`Time To First Fixation`)])

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

write.csv(first_point, file = "First_gaze.csv",row.names=FALSE)
write.csv(last_point, file = "Last_gaze.csv",row.names=FALSE)
