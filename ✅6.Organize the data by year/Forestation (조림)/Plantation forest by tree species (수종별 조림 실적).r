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

path_2000 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/2000_YRBK_0030030501.xlsx"


# area
path_area = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species/수종별 조림실적Plantation forest by tree species_면적.xlsx"
area = readxl::read_xlsx(path_area)
area_2 = extract_unique_years(area)
View(area_2)
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data_by ID/@_조림/(@부분완료)수종별 조림실적Plantation forest by tree species"
write.csv(area_2, file.path(path_save, "Completed_수종별조림실적_Area.csv"), row.names = F)



