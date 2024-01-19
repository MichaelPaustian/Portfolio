setwd('C:\\Users\\mpaus\\Documents\\Sports Science XC Well and Strava\\Strava\\Mileage Folder\\Year Review\\')

df <- ldply(.data = list.files(pattern = "*.csv"),
                         .fun = read_csv)%>%
  select('Type','Activity','Location','Name','Date','Distance','Pace','Duration','EstPace')%>%
  mutate(DateReal=word(Date, 1, sep = " "),
         Date=as.Date(DateReal, format = "%Y-%m-%d"),
         Month= months.Date(Date),
         Year= word(Date, 1, sep = "-"))%>%
  select(-'DateReal')%>%
  filter(Name == "",
         Type %in% c("unknown","Run"))



df2 <- df %>%
  distinct(Activity, .keep_all = TRUE ) %>%
  filter(Date < "2023-4-1",
         Date >"2022-5-31")

dffinal <- df2 %>%
  select(-'Location',-'Activity')%>%
  dplyr::group_by(Name, Date)%>%
  dplyr::summarise(Total_Distance=round(sum(Distance),0),
                   Total_Time=sum(Duration),
                   Pace=round((Total_Time/Total_Distance),0))%>%
  mutate(Week = ifelse(Date > "2022-5-31" & Date < "2022-6-5", "June 1-4",
                       ifelse(Date > "2022-6-4" & Date < "2022-6-12", "June 5-11",
                              ifelse(Date >"2022-6-11" & Date < "2022-6-19", "June 12-18",
                                     ifelse(Date >"2022-6-18" & Date < "2022-6-26", "June 19-25",
                                            ifelse(Date > "2022-6-25" & Date < "2022-7-3", "June 26-July 2",
                                                   ifelse(Date > "2022-7-2" & Date < "2022-7-10", "July 3-9",
                                                          ifelse(Date > "2022-7-9" & Date < "2022-7-17", "July 10-16",
                                                                 ifelse(Date > "2022-7-16" & Date < "2022-7-24", "July 17-23",
                                                                        ifelse(Date > "2022-7-23" & Date < "2022-7-31", "July 24-30",
                                                                               ifelse(Date > "2022-7-30" & Date < "2022-8-7", "July 31-Aug 6",
                                                                                      ifelse(Date > "2022-8-6" & Date < "2022-8-14", "Aug 7-13",
                                                                                             ifelse(Date> "2022-8-13" & Date < "2022-8-21", "Aug 14-20",
                                                                                                    ifelse(Date > "2022-8-20" & Date < "2022-8-28", "Aug 21-27",
                                                                                                           ifelse(Date > "2022-8-27" & Date < "2022-9-4", "Aug 28-Sep 3",                                                 
                                                                                                                  ifelse(Date > "2022-9-3" & Date < "2022-9-11", "Sep 4-10",
                                                                                                                         ifelse(Date > "2022-9-10" & Date < "2022-9-18", "Sep 11-17",
                                                                                                                                ifelse(Date > "2022-9-17" & Date < "2022-9-25", "Sep 18-24",
                                                                                                                                       ifelse(Date > "2022-9-24" & Date < "2022-10-2", "Sep 25-Oct 1",
                                                                                                                                              ifelse(Date > "2022-10-1" & Date < "2022-10-9", "Oct 2-8",
                                                                                                                                                     ifelse(Date > "2022-10-8" & Date < "2022-10-16", "Oct 9-15",
                                                                                                                                                            ifelse(Date > "2022-10-15" & Date < "2022-10-23", "Oct 16-22",
                                                                                                                                                                   ifelse(Date > "2022-10-22" & Date < "2022-10-30", "Oct 23-29",
                                                                                                                                                                          ifelse(Date > "2022-10-29" & Date < "2022-11-6", "Oct 30- Nov 5",
                                                                                                                                                                                 ifelse(Date > "2022-11-5" & Date < "2022-11-13", "Nov 6-12",
                                                                                                                                                                                        ifelse(Date > "2022-11-12" & Date < "2022-11-20", "Nov 13-19",
                                                                                                                                                                                               ifelse(Date > "2022-11-19" & Date < "2022-11-27", "Nov 20-26",
                                                                                                                                                                                                      ifelse(Date > "2022-11-26" & Date < "2022-12-4", "Nov 27-Dec 3",
                                                                                                                                                                                                             ifelse(Date > "2022-12-3" & Date < "2022-12-11", "Dec 4-10",
                                                                                                                                                                                                                    ifelse(Date > "2022-12-10" & Date < "2022-12-18", "Dec 11-17",
                                                                                                                                                                                                                           ifelse(Date > "2022-12-17" & Date < "2022-12-25", "Dec 18-24",
                                                                                                                                                                                                                                  ifelse(Date > "2022-12-24" & Date < "2023-1-1", "Dec 25-31",
                                                                                                                                                                                                                                         ifelse(Date > "2022-12-31" & Date < "2023-1-8", "Jan 1-7",
                                                                                                                                                                                                                                                ifelse(Date > "2023-1-7" & Date < "2023-1-15", "Jan 8-14",
                                                                                                                                                                                                                                                       ifelse(Date > "2023-1-14" & Date < "2023-1-22", "Jan 15-21",
                                                                                                                                                                                                                                                              ifelse(Date > "2023-1-21" & Date < "2023-1-29", "Jan 22-28",
                                                                                                                                                                                                                                                                     ifelse(Date > "2023-1-28" & Date < "2023-2-5", "Jan 29-Feb 4",
                                                                                                                                                                                                                                                                            ifelse(Date > "2023-2-4" & Date < "2023-2-12", "Feb 5-11",
                                                                                                                                                                                                                                                                                   ifelse(Date > "2023-2-11" & Date < "2023-2-19", "Feb 12-18",
                                                                                                                                                                                                                                                                                          ifelse(Date > "2023-2-18" & Date < "2023-2-26", "Feb 19-25",
                                                                                                                                                                                                                                                                                                 ifelse(Date > "2023-2-25" & Date < "2023-3-5", "Feb 26-Mar 4",
                                                                                                                                                                                                                                                                                                        ifelse(Date > "2023-3-4" & Date < "2023-3-12", "Mar 5-11",
                                                                                                                                                                                                                                                                                                               ifelse(Date> "2023-3-11" & Date < "2023-3-19", "Mar 12-18",
                                                                                                                                                                                                                                                                                                                      ifelse(Date>"2023-3-18" & Date <"2023-3-26","Mar 19-25", "Mar 26-31"))))))))))))))))))))))))))))))))))))))))))),
         Month = months.Date(Date),
         Week_Day=weekdays.Date(Date))

dfWeek <- dffinal %>%
  dplyr::group_by(Week) %>%
  dplyr::summarise(Mileage_For_Week= round(sum(Total_Distance),0),
                   Avg_Pace=round(mean(Pace/60),2)) 

dfmonth <- dffinal %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(Mileage_For_Month= round(sum(Total_Distance),0))


