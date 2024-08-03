# 🟥 2000년 연보에서 수종별조림실적 없는 데이터 ==========================================
## 🟧 @데이터 확인 ===================================================================================
# 2000년도 없는 것 확인
# 이전연도 : "1999_YRBK_00290304"
# id_2000 = "YRBK_00300305" # 내가 임의로 지정한 ID
# # "YRBK_00300305"
# path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx"
# raw_data = read.xlsx(path_data)
# raw_data %>% filter(연보.ID == id_2000)
# 
# hdr  = read.xlsx("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/0.Raw Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_HDR_20240415.xlsx")
# hdr %>% filter(연보.ID == id_2000)
# hdr %>% filter(연보.ID == "YRBK_0030030501")
# 
# data = readRDS("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/8.3.Combined Data.rds")
# data[names(data) == id_2000][[1]] %>% View
# 
# yb = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/4.Added YB ID.csv")
# yb_2000 = yb %>% filter(year == 2000)
# yb_2000$Categorized_L3_New %>% unique
# yb_2000 %>% filter(Categorized_L3_New == "수종별 조림실적_Plantation forest by tree species") %>% View
# 
# # 정리된 엑셀 불러오기
# path_seedling = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___그루수.xlsx"
# seedling = readxl::read_xlsx(path_seedling)
# 
# seedling_2 = extract_unique_years(seedling)
# View(seedling_2)



## 🟧 @임의로 복사한 파일이름 수정  ===================================================================================
# # 필요 패키지 로드
# library(stringr)
# 
# # 디렉토리 경로
# dir_path <- "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/@완료/완료"
# 
# # 디렉토리에서 "copy"가 포함된 CSV 파일 목록 가져오기
# file_names <- list.files(dir_path, pattern = "copy.*\\.csv$")
# 
# # 파일 이름 변경
# for (file_name in file_names) {
#   # 새로운 파일 이름 생성
#   new_file_name <- file_name %>%
#     str_replace("2001", "2000") %>%
#     str_remove(" copy")
#   
#   # 파일 이름 변경
#   file.rename(file.path(dir_path, file_name), file.path(dir_path, new_file_name))
# }



## 🟧 손으로 복붙한 데이터 합치기 ==============================================================================================
# path_data_2000 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/2000_YRBK_0030030501.xlsx"
# data_2000 = path_data_2000 %>% 
#   readxl::read_xlsx() %>% 
#   mutate_if(is.character, ~na_if(., "NA")) %>% # 데이터 프레임 data_2000에서 문자열 "NA"를 실제 NA로 변환
#   mutate_at(vars(5:ncol(.)), as.numeric) %>% # 5번째 열부터 numeric
#   slice(-6)
# 
# data_2000_selected = data_2000[,5:ncol(data_2000)]
# data_2000_summed = colSums(data_2000_selected, na.rm=T) 
# data_2000_summed_df = matrix(data_2000_summed, nrow = 1) %>% 
#   as.data.frame %>% 
#   setNames(names(data_2000_summed))
# 
# combined = cbind(Classification = 1999, data_2000_summed_df)
# 
# path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species"
# 
# # 데이터 프레임을 엑셀 파일로 저장
# library(openxlsx)
# write.xlsx(combined, file = file.path(path_save, "2000_YRBK_0030030501_summed.xlsx"))



# 🟥 1982~ 면적 등 계산 ======================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___면적.xlsx"
data = read.xlsx(path_data)

# 열이름에 활엽수, 침엽수 추가
data %>% names

# 침엽수 : 



# 🟥 특정 연도별  ======================================================================================
path_area = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___면적.xlsx"
library(dplyr)
library(openxlsx)

# Load data and select specific columns
area <- read.xlsx(path_area) %>%
  select(1:5) %>%
  rename(year = 구분_Classification) %>%
  mutate(Remarks = NA) %>%
  mutate(Remarks = ifelse(is.na(.[, 4]) & is.na(.[, 5]) & !is.na(.[, 3]), "Only total values present in the original data", Remarks)) %>% 
  mutate(across(2, ~ str_extract(., "^\\d{4}"))) %>%  # 2번째 열에서 연도만 남기기
  mutate(across(3:5, as.numeric)) %>%  # 3,4,5번째 열은 numeric으로 변환
  arrange(year, ID)

latest_values <- area %>%
  group_by(year) %>%
  slice_tail(n = 1) %>%
  ungroup()

View(latest_values)

# 모든 연도가 있는지 확인
check_continuous_years(latest_values$year)


path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species"
write.xlsx(latest_values, file.path(path_save, "수종별조림실적_면적(1960~2021).xlsx"))




# 🟥  기타 면적 계산  ======================================================================================
path_area = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species___면적.xlsx"
area = read.xlsx(path_area)
View(area)

# 217번째 행에서 NA가 아닌 열의 이름
non_na_columns <- names(data)[!is.na(data[217, ])]
print(non_na_columns)
names(data)[!is.na(data[218, ])]
# 활엽수 : 
# 침엽수 : 리기다, 낙엽송, 잣나무, 삼나무, 









# 🟥 데이터 합치기  ======================================================================================
## 🟧 1.Total만 존재하는 경우 ===================================================================================
### 🟨 데이터 로드 ======================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@완료)수종별 조림실적Plantation forest by tree species/@완료/1.Total 면적만 존재"
data.list = lapply(list.files(path_data, full.names=T), read.csv) %>% 
  setNames(tools::file_path_sans_ext(list.files(path_data)))

# 1,2,3,4,5 
# year, NAME_L1, NAME_L2, NAME_L3, NAME_L4, unit_L1, unit_L2, unit_L3, unit_L4, unit_L5 
# 특정 열 선택
### 🟨 특정 열들만 추출 ======================================================
# 데이터 프레임 리스트에서 필요한 열을 선택하는 함수 정의
select_columns <- function(df) {
  df %>%
    select(1:5, year, NAME_L1, NAME_L2, NAME_L3, NAME_L4, unit_L2, unit_L3, unit_L4, unit_L5)
}

# 데이터 리스트에 함수 적용하여 각 데이터 프레임에서 필요한 열만 추출
selected_data_list <- lapply(data.list, select_columns)

sapply(selected_data_list, function(x){
  names(x)[1:5]
})
selected_data_list[[1]] %>% names




### 🟨 열이름 통일 ======================================================
# 통일할 열 이름 정의
column_names <- c("ID", "행", "구분_Classification", "계_면적", "계_수량", 
                  "year", "NAME_L1", "NAME_L2", "NAME_L3", "NAME_L4", 
                  "unit_L2", "unit_L3", "unit_L4", "unit_L5")

# 모든 데이터프레임의 열 이름을 통일
standardize_column_names <- function(df) {
  names(df) <- column_names
  return(df)
}

# 각 데이터프레임에 함수 적용
standardized_data_list <- lapply(selected_data_list, standardize_column_names)

# 데이터프레임들 결합
combined_data <- do.call(rbind, standardized_data_list)




