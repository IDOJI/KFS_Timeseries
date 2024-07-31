# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
## 🟨Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE, quietly = T)
    }
  }
}

List.list = list()
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer")
List.list[[2]] = stat = c("fda", "MASS")
List.list[[3]] = data_handling = c("tidyverse", "dplyr", "clipr", "tidyr", "readr", "caret", "readxl")
List.list[[4]] = qmd = c("janitor", "knitr")
List.list[[5]] = texts = c("stringr")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)



## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select






## 🟧Loading my functions ======================================================
# R 함수 파일들을 로드하는 함수
load_functions <- function(path_functions) {
  list.files(path_functions, pattern = "\\.R$", full.names = TRUE) %>%
    walk(~try(source(.x), silent = TRUE))
}

# 주어진 경로에서 자동으로 R 폴더를 찾고 함수를 읽는 함수
load_r_functions_from_path <- function(paths) {
  walk(paths, ~{
    # 주어진 경로가 디렉토리인지 확인
    if (dir.exists(.x)) {
      # R 폴더 경로 생성
      r_folder_path <- file.path(.x, "R")
      # R 폴더가 존재하는지 확인
      if (dir.exists(r_folder_path)) {
        load_functions(r_folder_path)
        message("R 폴더의 함수들을 로드했습니다: ", r_folder_path)
      } else {
        message("R 폴더가 존재하지 않습니다: ", r_folder_path)
      }
    } else {
      message("유효한 디렉토리가 아닙니다: ", .x)
    }
  })
}

path_packages = c("/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/GitHub/refineR",
                  "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/GitHub/StatsR")

# 함수 호출
load_r_functions_from_path(path_packages)




# 🟥 Define functions #####################################################################################################
## 🟧 모든 연도 존재 =====================================================================================================
check_all_years = function(x){
  x = x %>% as.numeric
  x_max = max(x)
  x_min = min(x)
  x_seq = x_min:x_max
  
  
  not_included = x_seq[x_seq %in% x]
  
  if(length(not_included) > 0){
    
    print("The vector has all years")
    
  }else{
    
    print("These years are not included")
    
  }
}



# 🟥 Data Load #####################################################################################################
# 연보 이름
path_year_names = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_20240415.xlsx"
ynames = read.xlsx(path_year_names) %>% suppressWarnings()
class(ynames)


path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx"
data = read.xlsx(path_data)




# 🟥 연보 체크 #####################################################################################################
## 🟧 연보 연도 체크 #####################################################################################################
ynames$연보.년도 %>% table



## 🟧 remove cols #####################################################################################################
ynames$최종수정일시 %>% table
ynames$최종수정자ID %>% table
ynames$최초등록일시 %>% table
ynames$최초등록자ID %>% table
# 제거할 열 목록 정의
cols_to_remove <- c("최종수정일시", "최종수정자ID", "최초등록일시", "최초등록자ID")

# 열 제거
ynames <- ynames %>% select(-all_of(cols_to_remove))



### 🟨 상위연보 ID 변경 및 추가 ===================================================================================
x1 = c("YRBK_005006040101", "YRBK_005006040102")
x2 = c("YRBK_0003010201", "YRBK_0003010202", "YRBK_0003010203")


# `연보.ID`가 x1에 속하는 행의 `상위.연보.ID`를 변경
ynames <- ynames %>%
  mutate(상위.연보.ID = ifelse(연보.ID %in% x1, "YRBK_0050060401", 상위.연보.ID))

# `연보.ID`가 x2에 속하는 행의 `상위.연보.ID`를 변경
ynames <- ynames %>%
  mutate(상위.연보.ID = ifelse(연보.ID %in% x2, "YRBK_00030102", 상위.연보.ID))

# ynames %>% filter(연보.ID %in% x2) %>% View
# ynames %>% filter(연보.ID %in% x1) %>% View


# 🟥 각 레벨 별 연보 추출 #########################################################################
## 🟧 L1 ######################################################################################################
L1 = ynames %>% 
  filter(grepl(pattern = "통계연보", `연보명(한글)`) | grepl(pattern = "통계요람", `연보명(한글)`)) %>% 
  mutate(L1 = paste(`연보명(한글)`, `연보명(영문)`, sep = "_")) %>% 
  rename("year" := "연보.년도") %>% 
  relocate(year, L1) %>% 
  arrange(year) %>% 
  select(all_of(c("year", "L1", "연보.ID"))) %>% 
  rename("ID_L1" = "연보.ID") %>% 
  rename("NAME_L1" = "L1")

names(L1)
head(L1)
View(L1)

ynames_L1_removed = ynames %>% filter(!연보.ID %in% L1$ID_L1)
sum(ynames_L1_removed$연보.ID %in% L1$ID_L1)


# Check ID char number
L1$ID_L1 %>% nchar %>% unique





## 🟧 L2 ######################################################################################################
L2 = ynames_L1_removed %>% 
  filter(상위.연보.ID %in% L1$ID_L1) %>% 
  mutate(L2 = paste0(`연보명(한글)`, "_", `연보명(영문)`)) %>%
  select(-`연보명(한글)`) %>%
  select(-`연보명(영문)`) %>% 
  select(-연보.년도) %>% 
  rename(unit_L2 = 단위명) %>% 
  rename(비고_L2 = 비고) %>% 
  relocate(L2) %>% 
  rename("ID_L2" = "연보.ID") %>% 
  rename("NAME_L2" = "L2") %>% 
  rename("ID_L1" = "상위.연보.ID")

names(L2)
head(L2)

ynames_L2_removed = ynames_L1_removed %>% filter(!연보.ID %in% L2$ID_L2)

sum(ynames_L2_removed$연보.ID %in% L2$ID_L2)






## 🟧 L3 ######################################################################################################
L3 = ynames_L2_removed %>% 
  filter(상위.연보.ID %in% L2$ID_L2) %>% 
  mutate(L3 = paste0(`연보명(한글)`, "_", `연보명(영문)`)) %>%
  select(-`연보명(한글)`) %>%
  select(-`연보명(영문)`) %>% 
  select(-연보.년도) %>% 
  rename(unit_L3 = 단위명) %>% 
  rename(비고_L3 = 비고) %>% 
  relocate(L3) %>% 
  rename("ID_L3" = "연보.ID") %>% 
  rename("NAME_L3" = "L3") %>% 
  rename("ID_L2" = "상위.연보.ID")

names(L3)
head(L3)
View(L3)

ynames_L3_removed = ynames_L2_removed %>% filter(!연보.ID %in% L3$ID_L3)



# Check ID char number
L3$ID_L3 %>% nchar %>% unique





## 🟧 L4 ######################################################################################################
L4 = ynames_L3_removed %>% 
  filter(상위.연보.ID %in% L3$ID_L3) %>% 
  mutate(L4 = paste0(`연보명(한글)`, "_", `연보명(영문)`)) %>%
  select(-`연보명(한글)`) %>%
  select(-`연보명(영문)`) %>% 
  select(-연보.년도) %>% 
  rename(unit_L4 = 단위명) %>% 
  rename(비고_L4 = 비고) %>% 
  relocate(L4) %>% 
  rename("ID_L4" = "연보.ID") %>% 
  rename("NAME_L4" = "L4") %>% 
  rename("ID_L3" = "상위.연보.ID")

names(L4)
head(L4)
View(L4)

ynames_L4_removed = ynames_L3_removed %>% filter(!연보.ID %in% L4$ID_L4)

dim(ynames_L4_removed)
names(ynames_L4_removed)






## 🟧 L5 ######################################################################################################
L5 = ynames_L4_removed %>% 
  filter(상위.연보.ID %in% L4$ID_L4) %>% 
  mutate(L5 = paste0(`연보명(한글)`, "_", `연보명(영문)`)) %>%
  select(-`연보명(한글)`) %>%
  select(-`연보명(영문)`) %>% 
  select(-연보.년도) %>% 
  rename(unit_L5 = 단위명) %>% 
  rename(비고_L5 = 비고) %>% 
  relocate(L5) %>% 
  rename("ID_L5" = "연보.ID") %>% 
  rename("NAME_L5" = "L5") %>% 
  rename("ID_L4" = "상위.연보.ID")

names(L5)
head(L5)
dim(L5)

ynames_L5_removed = ynames_L4_removed %>% filter(!연보.ID %in% L5$ID_L5)
dim(ynames_L5_removed)
head(ynames_L5_removed)










# 🟥 연보 합치기  ######################################################################################################
## 🟧 열이름 체크 ===========================================================================
L1 %>% names
L2 %>% names
L3 %>% names
L4 %>% names
L5 %>% names




## 🟧 데이터프레임 합치기 ===========================================================================
dim(L2)
# 모든 행을 살리면서 데이터프레임 병합
combined.df <- merge(L1, L2, by = "ID_L1", all.x = TRUE, all.y = TRUE) %>% 
  merge(., L3, by = "ID_L2", all.x = TRUE, all.y = TRUE) %>% 
  merge(., L4, by = "ID_L3", all.x = TRUE, all.y = TRUE) %>% 
  merge(., L5, by = "ID_L4", all.x = TRUE, all.y = TRUE) %>% 
  arrange(year, 
          NAME_L1, NAME_L2, NAME_L3, NAME_L4, NAME_L5,
          ID_L1, ID_L2, ID_L3, ID_L4, ID_L5,
          unit_L2, unit_L3, unit_L4, unit_L5) %>% 
  relocate(year, 
           NAME_L1, NAME_L2, NAME_L3, NAME_L4, NAME_L5,
           ID_L1, ID_L2, ID_L3, ID_L4, ID_L5,
           unit_L2, unit_L3, unit_L4, unit_L5)


View(combined.df)


# 🟥 합친 연보 체크  ######################################################################################################
## 🟧 L5 ######################################################################################################
combined.df$NAME_L5 %>% is.na %>% sum
combined.df %>% filter(!NAME_L5 %>% is.na) %>% nrow
dim(L5)


## 🟧 L1, L2 ######################################################################################################
combined.df %>% filter(NAME_L1 %>% is.na) %>% nrow
combined.df %>% filter(NAME_L2 %>% is.na) %>% nrow



## 🟧 L3 ######################################################################################################
L3_na = combined.df %>% filter(NAME_L3 %>% is.na)
# appendix이므로 문제 없음


## 🟧 L4 ######################################################################################################
L4_na = combined.df %>% filter(NAME_L4 %>% is.na)
L4_na %>% View


## 🟧 Check non Year ######################################################################################################
combined.df$year %>% is.na %>% sum



## 🟧 각 연도 별 챕터 이름 통일성 체크 ######################################################################################################
### 🟨 각각 체크 =============================================================================
years = combined.df %>% pull(year) %>% unique
years

for(i in seq_along(years)){
  
  ith_year = years[i]
  
  ith_year.df = combined.df %>% filter(year %in% ith_year)
  
  ith_year.df$NAME_L2 %>% unique
  
  print(ith_year)
}




### 🟨 1979년 =============================================================================
ith_yb.df = combined.df %>% filter(year %in% "1979")
View(ith_yb.df)



### 🟨 1982년 =============================================================================
ith_yb.df = combined.df %>% filter(year %in% "1982")
View(ith_yb.df)




### 🟨 1992년 =============================================================================
ith_yb.df = combined.df %>% filter(year %in% "1992")
View(ith_yb.df)




### 🟨 2019년 =============================================================================
ith_yb.df = combined.df %>% filter(year %in% "2009")
View(ith_yb.df)




## 🟧 Check nchar of IDs ######################################################################################################
### 🟨 L1 ===============================================================================================
combined.df$ID_L1 %>% nchar %>% unique




### 🟨 L2 ===============================================================================================
combined_2.df = combined.df


combined.df$ID_L2 %>% nchar %>% unique

combined.df %>% filter(ID_L2 %>% nchar == 13)


# ID_L2의 길이가 13인 행 찾기
ID_L2_13.df <- combined.df %>% filter(nchar(ID_L2) == 13)
combined.df_2 = combined.df %>% filter(nchar(ID_L2) != 13)


# Check L5
ID_L2_13.df$NAME_L5 %>% is.na %>% sum == nrow(ID_L2_13.df)
ID_L2_13.df$ID_L5 %>% is.na %>% sum == nrow(ID_L2_13.df)
ID_L2_13.df$비고_L5 %>% is.na %>% sum == nrow(ID_L2_13.df)


# replace
name_1 = c("NAME_L3", "NAME_L4", "NAME_L5")
name_2 = c("NAME_L2", "NAME_L3", "NAME_L4")
# View(ID_L2_13.df[, name_1])
# View(ID_L2_13.df[, name_2])
ID_L2_13.df[, name_1] = ID_L2_13.df[, name_2]
# View(ID_L2_13.df[, name_1])


id_1 = c("ID_L3", "ID_L4", "ID_L5")
id_2 = c("ID_L2", "ID_L3", "ID_L4")
ID_L2_13.df[, id_1] = ID_L2_13.df[, id_2]

unit_1 = c("unit_L3", "unit_L4", "unit_L5")
unit_2 = c("unit_L2", "unit_L3", "unit_L4")
ID_L2_13.df[, unit_1] = ID_L2_13.df[, unit_2]

비고_1 = c("비고_L3", "비고_L4", "비고_L5")
비고_2 = c("비고_L2", "비고_L3", "비고_L4")
ID_L2_13.df[, 비고_1] = ID_L2_13.df[, 비고_2]


ID_L2_13.df$NAME_L2 = ID_L2_13.df$ID_L2 = ID_L2_13.df$unit_L2 = ID_L2_13.df$비고_L2 = NA
# View(ID_L2_13.df)


# L2_ID 추출
ID_L2_13.df$ID_L2 = substr(ID_L2_13.df$ID_L3, 1, 11)


# 대응하는 L2 찾기
ID_L2_13.df_combined = combined.df_2 %>% 
  filter(ID_L2 %in% ID_L2_13.df$ID_L2)

# Name_L2 
ID_L2_13.df$NAME_L2 =  ID_L2_13.df_combined$NAME_L2 %>% unique
ID_L2_13.df$비교_L2 =  ID_L2_13.df_combined$비교_L2 %>% unique
ID_L2_13.df$unit_L2 =  ID_L2_13.df_combined$unit_L2 %>% unique
View(ID_L2_13.df)

# 연보 합치기
combined.df_2 %>% dim
combined.df %>% dim
dim(ID_L2_13.df)
combined.df_3 = rbind(combined.df_2, ID_L2_13.df) %>% 
  arrange(year, ID_L1, ID_L2, ID_L3, ID_L4, ID_L5)


### 🟨 L3 ===============================================================================================
combined.df_3$ID_L1 %>% nchar %>% unique
combined.df_3$ID_L2 %>% nchar %>% unique
combined.df_3$ID_L3 %>% nchar %>% unique
combined.df_3$ID_L4 %>% nchar %>% unique
combined.df_3$ID_L5 %>% nchar %>% unique



## 🟧 연도 다시 체크 ######################################################################################################
### 🟨 1979 ==========================================================================================
year_1979 = combined.df_3 %>% filter(year == "1979")
year_1979$NAME_L2 %>% unique
# year_1979 %>% View

# 데이터 프레임 combined.df_3를 로드하고 수정
combined.df_3 <- combined.df_3 %>%
  mutate(NAME_L2 = ifelse(year == "1979" & NAME_L2 == "59. 국민총생산과 임업생산_59. Gross National Product & Forestry Product", 
                          "VIII.국민계정과 생산가격지수_VIII.National Accounts and Index Number of Products Price", 
                          NAME_L2))

# combined.df_3 %>% filter(year == "1979") %>% View




### 🟨 1982 ==========================================================================================
combined.df_3 %>% filter(year == "1982") %>% View
combined.df_4 <- combined.df_3 %>%
  mutate(NAME_L2 = ifelse(year == "1982" & NAME_L2 == "임야면적 및 임목축척_Forest Land Area & Growing Stock", 
                          "II. 임야면적 및 임목축척_II. Forest Land Area & Growing Stock", 
                          NAME_L2))

# combined.df_4 %>% View
combined.df_4 %>% filter(year == "1982") %>% View




### 🟨 1992 ==========================================================================================
# combined.df_4 %>% filter(year == "1992") %>% View
# combined.df_4 <- combined.df_3 %>%
#   mutate(NAME_L2 = ifelse(year == "1982" & NAME_L2 == "임야면적 및 임목축척_Forest Land Area & Growing Stock", 
#                           "II. 임야면적 및 임목축척_II. Forest Land Area & Growing Stock", 
#                           NAME_L2))
# 
# # combined.df_4 %>% View
# combined.df_4 %>% filter(year == "1982") %>% View



### 🟨 2019 ==========================================================================================
combined.df_4 %>% filter(year == "2019") %>% View
test = combined.df_4 %>% filter(year == "2019")
test $NAME_L2 %>% unique

combined.df_5 <- combined.df_4 %>%
  mutate(NAME_L2 = ifelse(year == "2019" & NAME_L2 == "NA_NA",
                          "IV. 산림자원 조성_IV. Silviculture",
                          NAME_L2)) %>% 
  mutate(NAME_L2 = ifelse(year == "2019" & NAME_L2 == "NA_Ⅶ. Forest Service",
                          "Ⅶ. 산림서비스_Ⅶ. Forest Service",
                          NAME_L2))


### 🟨 2015 ==========================================================================================
# combined.df_5 %>% filter(NAME_L2 == "임산물 시장_Forest Products Market") %>% View
combined.df_5 %>% filter(year == "2015") %>% View





## 🟧 L3==NA 제외 ######################################################################################################
# combined.df_5 %>% filter(ID_L3 %>% is.na) %>% View
combined.df_6 = combined.df_5 %>% filter(! ID_L3 %>% is.na)


## 🟧 2021 ######################################################################################################
combined.df_7 = combined.df_6 %>% 
  mutate(NAME_L2 = ifelse(year == "2021" & NAME_L2 == "Ⅴ. 산림경영 기반_NA",
                          "Ⅴ. 산림경영 기반_Forest Management",
                          NAME_L2))


combined.df_8 = combined.df_7 %>% 
  mutate(NAME_L2 = ifelse(year == "2021" & NAME_L2 == "Ⅸ. 부     록_Ⅸ. Appendix",
                          "Ⅸ. 부록_Ⅸ. Appendix",
                          NAME_L2))




# 🟥 Combine & export the data  ######################################################################################################
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/1.Rearrange YB names"
write.csv(combined.df_8, paste0(path_save, "/rearranged_yb_names.csv"), row.names = FALSE)

data = read.csv(paste0(path_save, "/rearranged_yb_names.csv"))



# 🟥NAME L5 체크  ######################################################################################################
# data_L5 = data %>% filter(!is.na(NAME_L5))
# View(data_L5 )
# 
# data_L5 %>% filter(ID_L4 == "YRBK_0013020401") %>% View
# 






















