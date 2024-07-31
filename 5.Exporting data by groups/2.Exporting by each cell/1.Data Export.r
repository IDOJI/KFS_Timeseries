# 🟥 Load data ##########################################################################
path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/3.Data with HDR/8.3.Combined Data.rds"
data = readRDS(path_data)
id = data %>% names


yb = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/4.Added YB ID.csv")


path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data by each cell"



# 🟥 ⭐️(완료) 각 데이터의 "행" 열 존재 확인 ##########################################################################
## 🟧 추출 ==============================================================================
# check_rowcol = sapply(data, function(x){
#   names(x)[2]
# }) %>% unname %>% unique

## 🟧 확인 ==============================================================================
# print(check_rowcol)
# 전부 L3 존재










# 🟥 ⭐️(완료) 각 데이터의 Categorized_L3 열이름 확인 ##########################################################################
## 🟧 추출 ==============================================================================
# check_L3 = sapply(data, function(x){
#   x$Categorized_L3[1]
# }) %>% unname
# 
# 
## 🟧 확인 ==============================================================================
# check_L3 %>% is.na %>% sum
# # 전부 L3 존재 


# 🟥 ⭐️(완료) 각 데이터의 Categorized_New_2 열 존재 확인 ##########################################################################
## 🟧 추출 ==============================================================================
# check_2 = sapply(data, function(x){
#   which(names(x) == "Categorized_New_2") %>% length
# }) %>% unname %>% unique


## 🟧 확인 ==============================================================================
# print(check_2==1)
# 전부 존재 



# 🟥 제외 항목 ====================================================================================
# "4.Exported Data_by ID" 결과로 확인
L3 = sapply(data, function(x){
  x$Categorized_L3_New %>% unique
}) %>% unname %>% unique

grep("학교 숲", L3, value=T)

exclude = c("학교 숲 조성현황_Establishment of Forests within Schools")


# 데이터에서 제외
data_new = lapply(data, function(x){
  if(!unique(x$Categorized_L3_New) %in% exclude){
    return(x)
  }
}) %>% setNames(names(data)) %>% remove_null_elements
data = data_new


# yb에서 제외
L3 = unique(yb$Categorized_L3_New)
L3 = L3[!L3 %in% exclude]
L3_removed = remove_non_korean_characters(L3)




# 🟥 각 데이터 추출해서 내보내기 ###################################################################################################
## 🟧 모든 데이터프레임에 대해 함수 적용 =====================================================================================================
### 🟨 최중요 데이터 먼저 내보내기 ===================================================================================
text = c("수묘표","조림", "종자", "가로수", "숲가꾸기", "벌채", "피해","재해", "복원", "도시숲", "목재", "국내재", "목질", "부산물", "원목", "축적", "면적", "화전")
include = lapply(text, function(x){
  filter_includes(L3_removed, x)
}) %>% setNames(text) %>% unlist %>% unique %>% sort

inclde = include[!include  %in% "구시군별면적및행정단위"]
# df = data[[100]]
# View(df)
error = lapply(data, 
               process_and_export, 
               path = path_save,
               include = include)



### 🟨 축적만 내보내기 ===================================================================================
text = c("축적")
include = lapply(text, function(x){
  filter_includes(L3_removed, x)
}) %>% setNames(text) %>% unlist %>% unique %>% sort

inclde = include[!include  %in% "구시군별면적및행정단위"]
# df = data[[100]]
# View(df)
error = lapply(data, 
               process_and_export, 
               path = path_save,
               include = include)



  
