### Combine csv files to one dataframe which add the file-name-column-----

CombineCSV = function(filepaths){
  All_df = data.frame()
  
  csv_lists = list.files(filepaths, pattern = '*.csv', full.names = T)
  for (csv_list in csv_lists){
    df = read.csv(csv_list)
    csv_name = basename(csv_list)
    df["Sample_ID"] = csv_name
    All_df = rbind(All_df, df)
  }
  All_df
}

 
# filepaths = "C:\\Users\\yukin\\Desktop\\Temp\\YH114_Processed_data"
# DF =CombineCSV(filepaths)


### Add Type, Day, No information to df----

ModifyDf = function(df, Type1, Type2,Type3="foo", Type4="foofoo"){
  df2 = df %>% mutate(Type = case_when(
    str_detect(string = Sample_ID, pattern = Type1) ~ Type1,
    str_detect(string = Sample_ID, pattern = Type2) ~ Type2,
    str_detect(string = Sample_ID, pattern = Type3) ~ Type3,
    str_detect(string = Sample_ID, pattern = Type4) ~ Type4,
    
    TRUE ~ 'NA'
  ))
  
  df2 = df2 %>% mutate(Day = case_when(
    str_detect(string = Sample_ID, pattern = "Day1") ~ "Day1",
    str_detect(string = Sample_ID, pattern = "day1") ~ "Day1",
    
    str_detect(string = Sample_ID, pattern = "Day2") ~ "Day2",
    str_detect(string = Sample_ID, pattern = "day2") ~ "Day2",
    
    str_detect(string = Sample_ID, pattern = "Day3") ~ "Day3",
    str_detect(string = Sample_ID, pattern = "day3") ~ "Day3",
    
    TRUE ~ 'NA'
  ))
  
  df2 = df2 %>% mutate(No = case_when(
    str_detect(string = Sample_ID, pattern = "_01") ~ "01",
    str_detect(string = Sample_ID, pattern = "_02") ~ "02",
    str_detect(string = Sample_ID, pattern = "_03") ~ "03",  
    str_detect(string = Sample_ID, pattern = "_04") ~ "04",
    str_detect(string = Sample_ID, pattern = "_05") ~ "05",
    str_detect(string = Sample_ID, pattern = "_06") ~ "06",
    str_detect(string = Sample_ID, pattern = "_07") ~ "07",
    str_detect(string = Sample_ID, pattern = "_08") ~ "08",
    str_detect(string = Sample_ID, pattern = "_09") ~ "09",
    str_detect(string = Sample_ID, pattern = "_10") ~ "10",
    
    TRUE ~ 'NA'
  ))
  df2 = df2 %>% mutate(LarvaeID = paste(!!!rlang::syms(c("Type", "No")), sep = "_"))
  
  
  df2
}


### To extract cells by their Area and Circrarity
ExtractCell = function(df, AreaMin, AreaMax, CircMin){
  library(tidyverse)
  df2 = df %>% filter(df$Area >= AreaMin)
  df2 = df2 %>% filter(df2$Area <= AreaMax)
  df2 = df2 %>% filter(df2$Circ. >= CircMin)
  
  df2
}




RelCellN=function(df, Cellmin,Type1, Type2, Type3="foo", Type4="foofoo"){
  No_day1 = df %>% filter(Day == "Day1"| Day == "day1") %>% group_by(LarvaeID) %>% summarise(CellN_day1 =n())
  No_day2 = df %>% filter(Day == "Day2"| Day == "day2") %>% group_by(LarvaeID) %>% summarise(CellN_day2 =n())
  No_day3 = df %>% filter(Day == "Day3"| Day == "day3") %>% group_by(LarvaeID) %>% summarise(CellN_day3 =n())
  
  No_day1 = No_day1 %>% filter(CellN_day1 >= Cellmin)
  
  No_day1to2 = left_join(No_day1,No_day2)
  No_day1to3 = left_join(No_day1to2, No_day3)

  
  #NA -> 0
  No_day1to3[is.na(No_day1to3)] = 0
  
  No_day1to3 = No_day1to3 %>% mutate(RelCellN_Day2Day1 = 100*CellN_day2/CellN_day1)
  No_day1to3 = No_day1to3 %>% mutate(RelCellN_Day3Day1 = 100*CellN_day3/CellN_day1)
  
  
  No_day1to3 = No_day1to3 %>% mutate(Type = case_when(
    str_detect(string = LarvaeID, pattern = Type1) ~ Type1,
    str_detect(string = LarvaeID, pattern = Type2) ~ Type2,
    str_detect(string = LarvaeID, pattern = Type3) ~ Type3,
    str_detect(string = LarvaeID, pattern = Type4) ~ Type4,
    
    TRUE ~ 'NA'
  ))
  
  No_day1to3
}


