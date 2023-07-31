## Code utilized for Capstone Analysis 


### Data cleaning for wellness questionniare 
setwd('C:\\Users\\mpaus\\Documents\\Sports Science XC Well and Strava\\WellnessQuestionnaire')

df <- read_excel("XC-Wellness-Capstone.xlsx")

df_date <- df %>%
  mutate(Date=as.Date(Date,format= "%Y-%m-%d"),
         Last=word(Name, 1, sep = ", "),
         First=word(Name, 2, sep= ", "),
         Last = ifelse(Last == "" & First == "", " ",
                       ifelse(Last == "" & First == "", "", Last)))

df_mr <- df_date %>%
  filter(Date > "2023-02-04")

df_well <- df_mr %>%
  select(-'Body Map Pain',-'Body Map Soreness',-"Snacks",-'Nap_Quantity',-'Pace',-'Nap',-'Urine Color') %>%
  filter(Bad_stress != "NA") %>%
  filter(Name != "NA")

df_sore <- df_mr %>%
  select(-'Snacks',-'Sleep_Duration_h',-'RPE_Physical',-'Pace_numeric',-'Nap',-'Nap_Quantity',-'Miles',-'Meals',-'Good_stress',-'Fatigue',-'Drinks',-'Cross_Train_Duration_h',-'Bad_stress',-'Pace',-'Urine Color')

colnames(df_sore) <- c("Date","Name","Pain","Sore","Last","First")  

df_sorefinal <- df_sore %>%
  filter(Name != "NA")%>%
  mutate(Pain= as.numeric(Pain),
         Sore= as.numeric(Sore),
         Sore= replace_na(Sore, 0),
         Pain= replace_na(Pain, 0))

str(df_well)
str(df_sorefinal)

df_well <- df_well %>%
  mutate(Bad_stress= as.numeric(Bad_stress),
         Cross_Train_Duration_h= as.numeric(Cross_Train_Duration_h),
         Drinks=as.numeric(Drinks),
         Good_stress=as.numeric(Good_stress),
         Fatigue=as.numeric(Fatigue),
         Meals=as.numeric(Meals),
         Miles=as.numeric(Miles),
         Pace_numeric=as.numeric(Pace_numeric),
         RPE_Physical=as.numeric(RPE_Physical),
         Sleep_Duration_h=as.numeric(Sleep_Duration_h))


df_sorefinalclean <- df_sorefinal %>%
  dplyr::group_by(Last,Date)%>%
  dplyr::summarise_all(funs(mean),na.rm= TRUE) %>%
  select(-'Name',-"First")


df_merge <- merge(df_well, df_sorefinalclean,
                  by=c("Last","Date"))

df_mergefinal <- df_merge %>%
  dplyr::group_by(Last, Date)%>%
  dplyr::summarise(Avg_Bad= mean(Bad_stress),
                   Avg_Cross= mean(Cross_Train_Duration_h),
                   Avg_Drinks= mean(Drinks),
                   Avg_Fatigue= mean(Fatigue),
                   Avg_Good=mean(Good_stress),
                   Avg_Meals=mean(Meals),
                   Avg_Miles=mean(Miles),
                   Avg_Pace_Well=mean(Pace_numeric),
                   Avg_RPE=mean(RPE_Physical),
                   Avg_Sleep=mean(Sleep_Duration_h),
                   Avg_Pain=mean(Pain),
                   Avg_Sore=mean(Sore))

### Reading data from pre workout questionnaires 

df_sheetLR <- read_excel("Data Sheet.xlsx", sheet= "LR Sunday")
df_sheetWk <- read_excel("Data Sheet.xlsx", sheet = "Tuesday Workout ")
df_sheetRec <- read_excel("Data Sheet.xlsx", sheet = "Recovery Thursday")
df_sheetRace <- read_excel("Data Sheet.xlsx", sheet ="Race day ")

df_sheetcombine <- rbind(df_sheetLR, df_sheetRace, df_sheetRec, df_sheetWk)

df_sheetfinal <- df_sheetcombine %>%
  mutate(Date= as.Date(Date, format="%Y-%m-%d"),
         Last=word(Name, 2, sep = " "),
         First=word(Name, 1, sep= " "),
         Last = ifelse(Last == "" & First == "", "",
                       ifelse(Last == "" & First == "", "", Last)))%>%
  select(-'Name')

colnames(df_sheetfinal) <- c("Pre_Sore", "Pre_Fatigue","Date","Type","Last","First")


### Reading and data cleaning for all athletes

setwd('C:\\Users\\mpaus\\Documents\\Sports Science XC Well and Strava\\Strava\\Mileage Folder\\Dec-Feb Mileage\\Athlete Name')

Athlete1<- ldply(.data = list.files(pattern = "*.csv"),
                 .fun = read_csv)

df_strava <- rbind(Athlete1,Athlete2,etc)

colnames(df_strava)<- c( "Athlete", "Activity" ,"Type" ,"Location" ,"Name" ,   
                         "Date" ,"Distance", "Pace"   ,  "Unit"   ,  "Duration",
                         "Elev" ,    "Calo"    , "EstPace",  "EstSpeed")


### Combining all strava data

df_stravaclean <- df_strava %>%
  select(-'Athlete',-'Activity',-'Location',-'Unit',-'Elev',-'Calo',-'EstPace',-'EstSpeed')%>%
  filter(Name != "NA",
         Name !="Michael Paustian",
         Name !="Brad Herbster",
         Type != "Weight Training")%>%
  mutate(date2 = as.Date(word(Date, 1, sep = " ")),
         Last = word(Name, 2, sep= " "),
         First = word(Name, 1, sep= " "),
         Last = ifelse(Last == "Miller" & First == "Luke", "L Miller",
                       ifelse(Last == "Miller" & First == "Jack", "J Miller", Last)))%>%
  select(-'Name',-"Date",-"First",-"Type")%>%
  filter(date2 >"2023-02-04",
         date2 <"2023-02-12",
         date2 != "2023-02-06",
         date2 != "2023-02-08")


colnames(df_stravaclean) <- c("Distance","Pace","Time","Date", "Last")

setwd('C:\\Users\\mpaus\\Documents\\Sports Science XC Well and Strava\\Strava\\Mileage Folder\\Capstone Data')

df_stravaother <- read_excel("Other Strava Data.xlsx")

df_stravaotherclean <- df_stravaother %>%
  select(-'Athlete',-'Activity',-'"Type"',-'Location',-'Unit',-'Elev',-'Calo',-'EstPace',-'EstSpeed',-'\"Type\"')%>%
  filter(Name != "NA",
         Name !="",
         Name !="" )%>%
  mutate(date2 = as.Date(word(Date, 1, sep = " ")),
         Last = word(Name, 2, sep= " "),
         First = word(Name, 1, sep= " "),
         Last = ifelse(Last == "" & First == "", "",
                       ifelse(Last == " " & First == "", " ", Last)),
         Pace2 = word(Pace, 2, sep= " "))%>%
  select(-"Name",-"Date",-"Pace",-'First')


colnames(df_stravaotherclean) <- c("Distance","Time","Date","Last", "Pace")


df_stravafinal <- rbind(df_stravaclean, df_stravaotherclean)

### Data Analysis 

colnames(df_Stravarealfinal) <- c("Last", "Date","Distance","Time","Pace_Numeric")

df_stravagrouprealfinal <- df_Stravarealfinal %>%
  dplyr::group_by(Last, Date)%>%
  dplyr::summarise(Total_Distancestrava= sum(Distance),
                   Total_Time= sum(Time),
                   Avg_Pace_Numeric= sum(Total_Time/Total_Distancestrava)/60)


df_SDsheetstrava <- df_sheetfinal_Stravagrouprealfinal %>%
  dplyr::group_by(Type)%>%
  dplyr::summarise(Mean_Distance= mean(Total_Distancestrava),
                   Avg_Pace=mean(Avg_Pace_Numeric),
                   SD_Dis=sd(Total_Distancestrava),
                   SD_Pace=sd(Avg_Pace_Numeric))                   


df_Sheet_Stravadata <- read_excel("sheetstrava Data.xlsx")%>%
  select("Last","Date","Type","Pace_Numeric")%>%
  filter(Last != "NA")

df_sheetstravafinal <- df_Sheet_Stravadata %>%
  dplyr::group_by(Last,Type)%>%
  dplyr::summarise(Avg_Pace=mean(Pace_Numeric))


df_Pace <- df_sheetstravafinal %>%
  dplyr::group_by(Type)%>%
  dplyr::summarise(Avg_Pace_Numeric=mean(Avg_Pace),
                   Sd_Pace=sd(Avg_Pace))

### Statistical Analysis 

#ANOVA
##sheetsstravafinal with fast paces
SummaryDistanceType <- summary(aov(Total_Distancestrava~Type,data=df_sheetfinal_Stravagrouprealfinal))
SummaryRPEType <- summary(aov(Avg_RPE~Type,data=df_mergewellnesssheet))
SummaryPaceType <- summary(aov(Avg_Pace~Type, data=df_sheetstravafinal))

SummaryDistanceRPE <- summary(aov(Total_Distancestrava~Avg_RPE, data = dfall))

aov(Avg_Bad~Type, data=df_mergewellnesssheet)
aov(Avg_Good~Type, data=df_mergewellnesssheet)
aov(Avg_Sleep~Type,data=df_mergewellnesssheet)
aov(Avg_Sore~Type,data=df_mergewellnesssheet)


SummaryRPEType
SummaryDistanceType
SummaryPaceType



#Pair Wise T-test -- Bonferroni correction 

df_sheetfinal_Stravagrouprealfinal %>%
  pairwise_t_test(Total_Distancestrava ~ Type,  p.adjust.method = "bonferroni")

df_mergewellnesssheet%>%
  pairwise_t_test(Avg_RPE~Type, p.adjust.method = "bonferroni")

df_mergesheetfastpace %>%
  pairwise_t_test(Pace_Numeric~Type.x, p.adjust.method = "bonferroni")


df_Pace %>%
  pairwise_t_test(Avg_Pace_Numeric~Type, p.adjust.method = "bonferroni")

df_mergewellnesssheet %>%
  pairwise_t_test(Pre_Fatigue~Type, p.adjust.method = "bonferroni")


df_fatiguerepeatanova()

summary(aov(`Avg_Fatigue` ~ factor(Type) + Error(factor(Last)/Type), data = df_mergewellnesssheet))
summary(aov(`Pre_Fatigue` ~ factor(Type) + Error(factor(Last)/Type), data = df_mergewellnesssheet))
summary(aov(Avg_Sore~ factor(Type) + Error(factor(Last)/Type), data = df_mergewellnesssheet))
summary(aov(Pre_Sore~ factor(Type) + Error(factor(Last)/Type), data = df_mergewellnesssheet))

#ICC 
view(df_merge_stravapace)
view(df_merge_stravamiles)

ICC(df_merge_stravamile)
ICC(df_merge_stravapace)


### Data Visualization/Merging 

# df_Sheet_Stravadata= Fast Paces 
# df_mergewellnessshee = df_merge, df_sheetfinal
# df_SDsheetstrava = df_sheetfinal and df_stravagrouprealfinal
# df_merge and df_stravafinal


######### Merge Wellness and Pre Run Survey

df_merge_sheet <- merge(df_mergefinal, df_sheetfinal,
                        by=c("Last","Date"))

df_realfinal <- merge(df_merge_sheet, df_stravagrouprealfinal,
                      by=c("Last","Date")) %>%
  select(-'First')


df_sheetfinal_Stravagrouprealfinal <- merge(df_sheetfinal, df_stravagrouprealfinal,
                                            by=c("Last","Date"))%>%
  select(-'Pre_Sore',-'Pre_Fatigue',-'First')

dfall <- merge(df_sheetfinal_Stravagrouprealfinal, df_mergefinal,
               by=c("Last","Date"))

df_mergewellnesssheet <- merge(df_mergefinal,df_sheetfinal,
                               by=c("Last","Date"))%>%
  select(-"First")

df_merge_stravamiles <- merge(df_mergefinal, df_stravagrouprealfinal,
                              by=c("Last","Date")) 


df_merge_stravamile <- merge(df_mergefinal, df_stravagrouprealfinal,
                             by=c("Last","Date")) %>%
  select("Avg_Miles","Total_Distancestrava")


df_merge_stravapace <- merge(df_merge_stravapace, df_sheetfinal,
                             by=c("Last","Date")) %>%
  select("Avg_Pace_Numeric","Avg_Pace_Well")

df_merge_strava_sheet <- merge(df_merge_strava, df_sheetfinal,
                               by=c("Last","Date"))


df_fatiguerepeatanova <- df_mergewellnesssheet %>%
  dplyr::group_by(Type)%>%
  dplyr::summarise(Pre_Fatigue_Avg=mean(Pre_Fatigue),
                   SD_PreFatigue=sd(Pre_Fatigue))


df_mergewellnesssheetgraph <- df_mergewellnesssheet %>%
  dplyr::group_by(Type)%>%
  dplyr::summarise(Mean_RPE=mean(Avg_RPE),
                   SD_RPE=sd(Avg_RPE),
                   Mean_Bad=mean(Avg_Bad),
                   SD_Bad=sd(Avg_Bad),
                   Mean_Fatigue=mean(Avg_Fatigue),
                   SD_Fatigue=sd(Avg_Fatigue),
                   Mean_Good=mean(Avg_Good),
                   SD_Good=sd(Avg_Good),
                   Mean_Sleep=mean(Avg_Sleep),
                   SD_Sleep=sd(Avg_Sleep),
                   Mean_Sore=mean(Avg_Sore),
                   SD_Sore=sd(Avg_Sore),
                   Mean_PreSore=mean(Pre_Sore),
                   SD_PreSore=sd(Pre_Sore),
                   Mean_PreFatigue=mean(Pre_Fatigue),
                   SD_PreFatigue=sd(Pre_Fatigue))


df_mergesheetfastpace <- merge(df_sheetfinal,df_Sheet_Stravadata,
                               by=c("Last","Date"))%>%
  select(-"Type.y",-"First")


df_Pace <- df_mergesheetfastpace %>%
  dplyr::group_by(Type.x)%>%
  dplyr::summarise(Avg_Pace_Numeric= mean(Pace_Numeric),
                   Sd_Pace= sd(Pace_Numeric))


### Graphs

Avg_Distance_Picture <- df_SDsheetstrava %>%
  ggplot(aes(x=Type, y= Mean_Distance , fill= Type))+
  geom_bar(stat= 'identity')+
  geom_errorbar(aes(x=Type, ymin= Mean_Distance - SD_Dis, ymax=Mean_Distance + SD_Dis), width= .2)+
  labs(x= "Type", y= "Distance (Miles)" )+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","white"))+
  theme(plot.caption= element_text(size= 18), legend.position = "NA",
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))

Avg_Pace_Picture <- df_SDsheetstrava %>%
  ggplot()+
  geom_bar(aes(x=Type, y= Avg_Pace, fill= Type ), stat= 'identity')+
  geom_errorbar(aes(x=Type, ymin= Avg_Pace - SD_Pace, ymax=Avg_Pace + SD_Pace), width= .2)+
  labs(x= "Type", y= "Pace Numeric")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","white"))+
  theme(plot.title= element_text(size= 18), legend.position = "NA",
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))


Avg_RPE_Picture <- df_mergewellnesssheetgraph %>%
  ggplot(aes(x=Type, y= Mean_RPE , fill= Type))+
  geom_bar(stat= 'identity')+
  geom_errorbar(aes(x=Type, ymin= Mean_RPE - SD_RPE, ymax=Mean_RPE+SD_RPE), width= .2)+
  labs(x= "Type",y= "RPE (0-10)")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","white"))+
  theme(plot.caption= element_text(size= 18), legend.position = "NA",
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))

Bad_Stress <- df_mergewellnesssheetgraph %>%
  ggplot()+
  geom_bar(aes(x=Type, y= Mean_Bad , fill= Type), stat= 'identity')+
  geom_errorbar(aes(x=Type, ymin= Mean_Bad - SD_Bad, ymax=Mean_Bad+SD_Bad), width= .2)+
  labs(x= "Type",y= "Bad Stress (0-10)")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","black"))+
  theme(legend.position = "NA",
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))

Good_Stress <- df_mergewellnesssheetgraph %>%
  ggplot()+
  geom_bar(aes(x=Type, y= Mean_Good , fill= Type), stat= 'identity')+
  geom_errorbar(aes(x=Type, ymin= Mean_Good - SD_Good, ymax=Mean_Good+SD_Good), width= .2)+
  labs(x= "Type", y= "Good Stress (0-10)")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","black"))+
  theme(legend.position = "NA",
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))

Sleep <- df_mergewellnesssheetgraph %>%
  ggplot()+
  geom_bar(aes(x=Type, y= Mean_Sleep , fill= Type), stat= 'identity')+
  geom_errorbar(aes(x=Type, ymin= Mean_Sleep - SD_Sleep, ymax=Mean_Sleep+SD_Sleep), width= .2)+
  labs(x= "Type", y= "Hours of Sleep")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","black"))+
  theme(legend.position = "NA",
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))

Sore <- df_mergewellnesssheetgraph %>%
  ggplot()+
  geom_bar(aes(x=Type, y= Mean_Sore , fill= Type), stat= 'identity')+
  geom_errorbar(aes(x=Type, ymin= Mean_Sore - SD_Sore, ymax=Mean_Sore+SD_Sore), width= .2)+
  labs(x= "Type", y= "Soreness (0-10)")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","black"))+
  theme(plot.title= element_text(size= 23, face= "bold"), legend.position = "NA",
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))

PreFatigue <- df_fatiguerepeatanova %>%
  ggplot(aes(x=Type, y=Pre_Fatigue_Avg, fill = Type))+
  geom_bar(stat='identity')+
  geom_errorbar(aes(x=Type, ymin=Pre_Fatigue_Avg - SD_PreFatigue, ymax=Pre_Fatigue_Avg+SD_PreFatigue), width= .2)+
  labs(x= "Type", y= "Fatigue (0-10)")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","white"))+
  theme(plot.caption= element_text(size= 18), legend.position = "NA",
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))


Avg_Fast_Pace_Picture <- df_Pace %>%
  ggplot(aes(x=Type.x, y= Avg_Pace_Numeric, fill= Type.x))+
  geom_bar(stat= 'identity')+
  geom_errorbar(aes(x=Type.x, ymin=Avg_Pace_Numeric - Sd_Pace, ymax=Avg_Pace_Numeric+Sd_Pace), width= .2)+
  labs(x= "Type", y= "Minutes per Mile")+
  scale_fill_manual(values=c("#003594", "#FFB81C","grey51","white"))+
  theme(plot.caption= element_text(size= 18), legend.position = "NA",
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size= 18),
        axis.title.y = element_text(size= 18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size= 18))

Mean_Distance_Pictures <- Avg_Distance_Picture+
  geom_signif(comparisons = list(c("Long Run", "Race")),
              annotations = "*",
              y_position = c(15,15))+
  geom_signif(comparisons = list(c("Long Run", "Recovery")),
              annotations = "*",
              y_position = c(16,16))+
  geom_signif(comparisons = list(c("Long Run", "Workout")),
              annotations = "*",
              y_position = c(17,17))

Avg_RPE_Pictures <- Avg_RPE_Picture+
  geom_signif(comparisons = list(c("Long Run", "Race")),
              annotations = "*",
              y_position = c(8.5,8))+
  geom_signif(comparisons = list(c("Long Run", "Recovery")),
              annotations = "*",
              y_position = c(9,9))+
  geom_signif(comparisons = list(c("Race", "Recovery")),
              annotations = "*",
              y_position = c(7.5,7.5))+
  geom_signif(comparisons = list(c("Recovery", "Workout")),
              annotations = "*",
              y_position = c(6.5,6.5))

PreFatigues <- PreFatigue +
  geom_signif(comparisons = list(c("Race","Recovery")),
              annotations = "*",
              y_position = c(5,5))

Avg_Fast_Pace_Picture+
  geom_signif(comparisons = list(c("Long Run", "Race")),
              annotations = "*",
              y_position = c(7.5,7.5))+
  geom_signif(comparisons = list(c("Race", "Recovery")),
              annotations = "*",
              y_position = c(7.5,7.5))+
  geom_signif(comparisons = list(c("Long Run", "Workout")),
              annotations = "*",
              y_position = c(8,8))+
  geom_signif(comparisons = list(c("Race", "Workout")),
              annotations = "***",
              y_position = c(7,7))+
  geom_signif(comparisons = list(c("Recovery", "Workout")),
              annotations = "*",
              y_position = c(7,7))

#Correlation_Plots_Distance <- 
df_merge_strava_sheet %>%
  ggplot(aes(x=Avg_Miles, y= Total_Distancestrava))+
  geom_point()+
  geom_smooth(method= "lm")+
  labs(title = "Athletes Report Mileage Accurately", 
       x= "Mileage from Strava", 
       y= "Mileage Reported to Wellness Questionniare" )+
  theme(plot.title= element_text(size= 16, face= "bold"))+
  facet_grid(~Type)

#Correlation_Plots_Pace <- 
df_merge_strava_sheet %>%
  ggplot(aes(x=Avg_Pace_Well, y=Avg_Pace_Numeric))+
  geom_point()+
  geom_smooth(method= "lm")+
  labs(title = "Pace Reported Does Not Match Actual Pace", 
       subtitle = "Long Run Only Exception", x= "Pace from Strava", 
       y= "Pace Reported to WQ" )+
  theme(plot.title= element_text(size= 16, face= "bold"))+
  facet_grid(~Type)

Bland_Pace <- bland.altman.plot(df_merge_strava_sheet$Avg_Pace_Well,df_merge_strava_sheet$Avg_Pace_Numeric, graph.sys = "ggplot2")+
  labs(x="Pace of Runs (mins/mile)",y="Pace Reported to Questionnaire - Strava")+
  theme(axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size = 18))

Bland_Distance <- bland.altman.plot(df_merge_strava_sheet$Avg_Miles, df_merge_strava_sheet$Total_Distancestrava, graph.sys = "ggplot2")+
  labs(x="Distance of runs (Miles)", y= "Distance Reported to Questionnaire - Strava")+
  theme(axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=18),
        axis.text.x = element_text(size= 18),
        axis.text.y = element_text(size = 18))

Bland_Distance
Bland_Pace


## Statistical Analysis
blandr.statistics(df_merge_strava_sheet$Avg_Pace_Numeric, df_merge_strava_sheet$Avg_Pace_Well, sig.level = 0.95)
Bland_Pace <- blandr.draw(df_merge_strava_sheet$Avg_Pace_Numeric, df_merge_strava_sheet$Avg_Pace_Well, sig.level= 0.95)
blandr.statistics(df_merge_strava_sheet$Avg_Miles, df_merge_strava_sheet$Total_Distancestrava, sig.level= 0.95 )
Bland_Mile <- blandr.draw(df_merge_strava_sheet$Avg_Miles, df_merge_strava_sheet$Total_Distancestrava, sig.level= 0.95 )+ gg


view(df_merge_strava_sheet)

Bland_Pace
Bland_Distance

Mean_Distance_Picture
Fastest_Pace_Picture

ggsave('FinalwriteupDistance.png', Mean_Distance_Pictures, width=10, height = 8,
       units = 'in', dpi= 400)
ggsave('FinalwriteupPace.png', Avg_Fast_Pace_Pictures, width=10, height = 8,
       units = 'in', dpi= 400)
ggsave('FinalwriteupRPE.png', Avg_RPE_Pictures, width=10, height = 8,
       units = 'in', dpi= 400)
ggsave('PreFatigue.png', PreFatigues, width= 10, height = 8,
       units = 'in', dpi=400)
ggsave('BadStress.png', Bad_Stress, width=10, height = 8,
       units = 'in', dpi = 400)
ggsave('GoodStress.png', Good_Stress, width=10, height = 8,
       units = 'in', dpi = 400)
ggsave('Sleep.png', Sleep, width=10, height = 8,
       units = 'in', dpi = 400)
ggsave('Sore.png', Sore, width=10, height = 8,
       units = 'in', dpi = 400)




PreFatigue





