# 🟥 데이터 로드 =================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/숲 가꾸기Forest tending"
data.list = lapply(list.files(path_data, full.names=T, pattern = "\\.csv$"), read.csv) %>% 
  setNames(list.files(path_data, pattern = "\\.csv$"))




# 🟨 3번째 열이름을 구분으로 통일 =================================================================
library(purrr)

# 각 데이터 프레임의 3번째 열 이름 추출
third_column_names <- map(data.list, function(df) {
  if (ncol(df) >= 3) {
    return(names(df)[3])
  } else {
    return(NA) # 3번째 열이 없는 경우 NA 반환
  }
})

# 결과 확인
print(third_column_names)




# 각 데이터 프레임의 3번째 열 이름을 "구분"으로 변경
data.list <- map(data.list, function(df) {
  if (ncol(df) >= 3) {
    colnames(df)[3] <- "Classification"
  }
  return(df)
})

# 결과 확인: 각 데이터 프레임의 열 이름
print(map(data.list, names))



# # 🌫️ unit  변경 =================================================================================
# ## 🟨 1983 ================================================================================
# data_1983 = data.list$`1983_YRBK_00130306.csv`
# data_1984 = data.list$`1984_YRBK_00140306.csv`
# View(data_1983)
# View(data_1984)




data.list$`1999_YRBK_00290306.csv` %>% View


# 🌫️ 데이터 수정 =================================================================================
sub_data = data.list$`1999_YRBK_00290306.csv`
sub_data$천연림보육[1] = NA
data.list$`1999_YRBK_00290306.csv` = sub_data


# 🟦 데이터 합치기=================================================================================
data_combined = bind_rows(data.list)
View(data_combined)





# 🟦 연도 행만 추출====================================================================================
data_combined_2 = filter_by_year(data_combined) %>% relocate(year, .after = 3)
View(data_combined_2)
data_combined_2$Classification %>% unique



# 🟧 열 합치기====================================================================================
data_combined_2 %>% names

## 🟨 구분 =========================================================================================
data_combined_2$구분 = NULL


## 🟨 "계"  =========================================================================================
# 옮기기
data_combined_2 = data_combined_2 %>% 
  relocate(contains("계"), .after = last_col())
names(data_combined_2)

# 삭제
data_combined_3 = data_combined_2 %>% 
  select(-contains("계"))

names(data_combined_3)



## 🟨 풀베기  =========================================================================================
grep("풀베기", names(data_combined_3), value = T)
data_combined_4 = combine_columns_by_keywords(data_combined_3, target_strings = "풀베기",  new_column_name = "풀베기_new")
View(data_combined_4)




## 🟨 덩굴제거  =========================================================================================
grep("덩굴", names(data_combined_4), value = T)
data_combined_5 = combine_columns_by_keywords(data_combined_4, target_strings = "덩굴", new_column_name = "덩굴제거_new")
names(data_combined_5)


## 🟨 기타  =========================================================================================
grep("기타", names(data_combined_5), value = T)
data_combined_6 = combine_columns_by_keywords(data_combined_5, target_strings = "기타", new_column_name = "기타_new")
names(data_combined_6)



## 🟨 천연림보육 =========================================================================================
grep("천연림보육", names(data_combined_6), value = T)
data_combined_7 = combine_columns_by_keywords(data_combined_6, target_strings = "천연림보육", new_column_name = "천연림보육_new")




## 🟨 공익림가꾸기 =========================================================================================
data_old = data_combined_7
key = "공익림가꾸기"
grep(key, names(data_old), value = T)
data_combined_8 = combine_columns_by_keywords(data_old, target_strings = key, new_column_name = paste0(key, "_new"))



## 🟨 간벌 =========================================================================================
data_old = data_combined_8
names(data_old)
key = "간벌"
grep(key, names(data_old), value = T)
data_combined_9 = combine_columns_by_keywords(data_old, target_strings = key, new_column_name = paste0(key, "_new"))



## 🟨 경제림 솎아베기 =========================================================================================
data_old = data_combined_9
names(data_old)
key = c("경제림", "솎아베기")
col_name = "솎아베기_경제림"
multi_grep(key, data_old)
data_combined_10 = combine_columns_by_keywords(data_old, target_strings = key, new_column_name = paste0(col_name, "_new"))



## 🟨 어린나무가꾸기 =========================================================================================
data_old = data_combined_10
names(data_old)
key = c("어린나무", "가꾸기")
col_name = "어린나무가꾸기"
multi_grep(key, data_old)
data_combined_11 = combine_columns_by_keywords(data_old, target_strings = key, new_column_name = paste0(col_name, "_new"))
View(data_combined_11 )



## 🟨 덩굴제거 만경류 =========================================================================================
data_old = data_combined_11
names(data_old)
# key = c("어린나무", "가꾸기")
# col_name = "어린나무가꾸기"
# multi_grep(key, data_old)
data_combined_12 = combine_columns_by_keywords(data_old, target_columns = c("만경류제거", "덩굴제거_new"), new_column_name = "덩굴제거")







## 🟨 보식 메워심기 =========================================================================================
data_old = data_combined_12
names(data_old)
# key = c("어린나무", "가꾸기")
# col_name = "어린나무가꾸기"
# multi_grep(key, data_old)
data_combined_13 = combine_columns_by_keywords(data_old, target_columns = c("보식", "메워심기"), new_column_name = "메워심기_new")





## 🟨 비배 비료주기 =========================================================================================
data_old = data_combined_13
names(data_old)
# key = c("어린나무", "가꾸기")
# col_name = "어린나무가꾸기"
# multi_grep(key, data_old)
data_combined_14 = combine_columns_by_keywords(data_old, target_columns = c("비배", "비료주기"), new_column_name = "비료주기_new")




## 🟨 솎아베기 , 간벌 =========================================================================================
data_old = data_combined_14
names(data_old)
grep("솎아", names(data_old), value=T)
# key = c("어린나무", "가꾸기")
# col_name = "어린나무가꾸기"
# multi_grep(key, data_old)
data_combined_15 = combine_columns_by_keywords(data_old, target_columns = c("숲가꾸기_솎아베기", "솎아베기", "간벌_new"), new_column_name = "솎아베기_new")
data_combined_15 = data_combined_15 %>% 
  relocate(무육, .after = last_col()) %>% 
  relocate(year, .after = Classification)
names(data_combined_15)






# 🟩 데이터 추출 =====================================================================================
names(data_combined_15)
data.list = list()
## 🟪 풀베기 ================================================================================
k=1
col_name = "풀베기_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name
View(data.list[[k]])

## 🟪 천연림보육 ================================================================================
k=2
names(data_combined_15)
col_name = "천연림보육_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name


## 🟪 공익림가꾸기 ================================================================================
k=3
names(data_combined_15)
col_name = "공익림가꾸기_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name



## 🟪 솎아베기 경제림 ================================================================================
k=4
names(data_combined_15)
col_name = "솎아베기_경제림_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name


## 🟪 어린나무가꾸기 ================================================================================
k=5
names(data_combined_15)
col_name = "어린나무가꾸기_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name



## 🟪 덩굴제거 ================================================================================
k=6
names(data_combined_15)
col_name = "덩굴제거"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name



## 🟪 메워심기 ================================================================================
k=7
names(data_combined_15)
col_name =  "메워심기_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name



## 🟪 비료주기 ================================================================================
k=8
names(data_combined_15)
col_name =  "비료주기_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name



## 🟪 솎아베기 ================================================================================
k=9
names(data_combined_15)
col_name =  "솎아베기_new"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name



## 🟪 무육 ================================================================================
k=10
names(data_combined_15)
col_name =  "무육"
data.list[[k]] = select_columns_with_additional(data_combined_15, col_name)
names(data.list)[k] = col_name
data.list$풀베기_new %>% View
data.list[[k]] %>% View




# 🟥 마지막 열 옮기기 ====================================================================================
data.list_2 = lapply(data.list, move_last_column_after_year)
data.list_2
names(data.list_2)



# 🟥 열이름 변천 반영 ====================================================================================
## 🌫️ 새 데이터 저장 리스트  ==============================================================
data.list_new = list()



## 🌫️ 무육 -> 어린나무가꾸기 ==============================================================
sub_1 = data.list_2$무육
sub_2 = data.list_2$어린나무가꾸기_new
View(data.list_2$무육)
View(sub_1)
View(sub_2)

names(sub_1)[5] = names(sub_2)[5]

combined_sub = rbind(sub_1, sub_2)
View(combined_sub)

combined_sub_2 = combined_sub %>% 
  filter(!is.na(어린나무가꾸기_new)) %>% 
  arrange(year, Classification)
View(combined_sub_2 )

data.list_2$무육 = NULL
data.list_2$어린나무가꾸기_new = NULL

data.list_new[["어린나무가꾸기"]] = combined_sub_2





## 🌫️ 풀베기 -> 조림지가꾸기_풀베기  ==============================================================
sub = data.list_2$풀베기_new
check_continuous_years(sub$Classification)
data.list_2$풀베기_new = NULL

data.list_new[["조림지가꾸기_풀베기"]] = sub




## 🌫️ 만경류제거 ->덩굴제거 ->  조림지가꾸기_덩굴제거  ==============================================================
names(data.list_2)
sub = data.list_2$덩굴제거
check_continuous_years(sub$Classification)
data.list_2$덩굴제거 = NULL
View(sub)
data.list_new[["조림지가꾸기_덩굴제거"]] = sub




## 🌫️ 간벌 -> 솎아베기 -> 숲가꾸기_솎아베기 -> 큰나무가꾸기_경제림가꾸기_솎아베기  ==============================================================
names(data.list_2)
sub_1 = data.list_2$솎아베기_new
sub_2 = data.list_2$솎아베기_경제림_new

names(sub_1)[5]
names(sub_2)[5]
new_name = "큰나무가꾸기_경제림가꾸기_솎아베기"
names(sub_1)[5] = names(sub_2)[5] = new_name
combined = rbind(sub_1, sub_2)
check_continuous_years(combined$Classification)

data.list_2$솎아베기_new = data.list_2$솎아베기_경제림_new = NULL

data.list_new[[new_name]] = combined %>% arrange(Classification)
View(data.list_new[[new_name]])



## 🌫 천연림보육-> 숲가꾸기_천연림보육 -> 큰나무가꾸기_경제림가꾸기_천연림보육 ===================================================
names(data.list_2)
sub_1 = data.list_2$천연림보육_new

new_name = "큰나무가꾸기_경제림가꾸기_천연림보육"
names(sub_1)[5]
names(sub_1)[5] =  new_name

combined = rbind(sub_1)
check_continuous_years(combined$Classification)

data.list_2$천연림보육_new = NULL

data.list_new[[new_name]] = combined



## 🌫 큰나무가꾸기_공익림가꾸기 ===================================================
names(data.list_2)
sub_1 = data.list_2$공익림가꾸기_new

new_name = "큰나무가꾸기_공익림가꾸기"
names(sub_1)[5]
names(sub_1)[5] =  new_name

combined = rbind(sub_1)
check_continuous_years(combined$Classification)

data.list_2$공익림가꾸기_new = NULL

data.list_new[[new_name]] = combined




## 🌫 보식 -> 메워심기 ===================================================
names(data.list_2)
sub_1 = data.list_2$메워심기_new

new_name = "메워심기"
names(sub_1)[5]
names(sub_1)[5] =  new_name

combined = rbind(sub_1)
check_continuous_years(combined$Classification)
combined$year %>% unique %>% sort
View(combined)
data.list_2$메워심기_new = NULL
data.list_new[[new_name]] = combined




## 🌫 비배 -> 비료주기 ===================================================
names(data.list_2)
sub_1 = data.list_2$비료주기_new

new_name = "비료주기"
names(sub_1)[5]
names(sub_1)[5] =  new_name

combined = rbind(sub_1)
check_continuous_years(combined$Classification)
combined$year %>% unique %>% sort
View(combined)

data.list_new[[new_name]] = combined

data.list_new$큰나무가꾸기_경제림가꾸기_솎아베기 %>% View

# 🟦 없는 연도 제거 ====================================================================================
## 🟩 메워심기 ==========================================================================
# 2009 연보까지
sub = data.list_new$메워심기
sub_new = sub %>% filter(year <= 2009)
data.list_new$메워심기 = sub_new



## 🟩 비료주기 ==========================================================================
# 2003 연보까지
sub = data.list_new$비료주기
sub_new = sub %>% filter(year <= 2003)
View(sub_new)
data.list_new$비료주기 = sub_new



## 🟩 덩굴제거 ==========================================================================
# 1985년도부터
sub = data.list_new$조림지가꾸기_덩굴제거
sub_new = sub %>% filter(year >= 1985)
View(sub_new)
data.list_new$조림지가꾸기_덩굴제거 = sub_new



## 🟩 천연림보육 ==========================================================================
# 1989년도부터
sub = data.list_new$큰나무가꾸기_경제림가꾸기_천연림보육
sub_new = sub %>% filter(year >= 1989)
View(sub_new)
data.list_new$큰나무가꾸기_경제림가꾸기_천연림보육 = sub_new



## 🟩 공익림가꾸기 ==========================================================================
sub = data.list_new$큰나무가꾸기_공익림가꾸기
sub_new = sub %>% filter(year >= 2014)
View(sub_new)
data.list_new$큰나무가꾸기_공익림가꾸기 = sub_new




# 🟥 값이 NA인 행들 제거 ====================================================================================
data.list_new_2 = list()
save = data.list_new
for(i in seq_along(data.list_new)){
  
  ith_data = data.list_new[[i]]
  data.list_new_2[[i]] = ith_data %>% filter(!is.na(.[[5]]))
  
}
names(data.list_new_2) = names(data.list_new)
# data.list_new_2$큰나무가꾸기_경제림가꾸기_솎아베기 %>% View
data.list_new = data.list_new_2
data.list_new[[1]] %>% View




# 🟥 각 연도 당 최신 연보의 값을 사용 & unit 보정 ====================================================================================
## 🟥 실제 연보의 unit 반영 ====================================================================================
year_1 = 1983:2001 %>% as.character()
unit_1 = rep("1000ha", times = length(year_1))
year_2 = 2002:2022 %>% as.character()
unit_2 = rep("ha", times = length(year_2))


# 함수 정의
change_value_by_units <- function(df) {
  # year_1 벡터 정의
  year_1 <- 1983:2001 %>% as.character()
  
  # df의 Classification 열과 year_1 비교
  df <- df %>%
    mutate(across(5, ~ifelse(year %in% year_1, . * 1000, .)))
  
  return(df)
}








## 🟪 어린나무가꾸기 ========================================================================
k = 1 
kth_name = names(data.list_new)[k]
View(data.list_new[[kth_name]] )
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub




## 🟪 "조림지가꾸기_풀베기" ========================================================================
k = 2
kth_name = names(data.list_new)[k]
print(kth_name)
View(data.list_new[[kth_name]])
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub




## 🟪 "조림지가꾸기_덩굴제거" ========================================================================
k = 3
kth_name = names(data.list_new)[k]
print(kth_name)
View(data.list_new[[kth_name]])
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub



## 🟪 "큰나무가꾸기_경제림가꾸기_솎아베기" ========================================================================
k = 4
kth_name = names(data.list_new)[k]
print(kth_name)
View(data.list_new[[kth_name]])
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub




## 🟪 "큰나무가꾸기_경제림가꾸기_천연림보육" ========================================================================
k = 5
kth_name = names(data.list_new)[k]
print(kth_name)
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub




## 🟪 "큰나무가꾸기_공익림가꾸기" ========================================================================
k = 6
kth_name = names(data.list_new)[k]
print(kth_name)
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub




## 🟪 ""메워심기" ========================================================================
k = 7
kth_name = names(data.list_new)[k]
print(kth_name)
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub




## 🟪 "8."비료주기" ========================================================================
k = 8
kth_name = names(data.list_new)[k]
print(kth_name)
sub = data.list_new[[kth_name]] %>% 
  unique_by_classification %>% 
  change_value_by_units
View(sub)
names(sub)[5] = kth_name
check_continuous_years(sub$Classification)
data.list_new[[k]] = sub






# 🟨 없는 연도 행 추가  ====================================================================================
years_all = 1968:2021 %>% as.character
data.list_new$어린나무가꾸기$Classification %in% years_all


# 함수 호출
data.list_processed <- process_data_list_adding_excluded_years(data_list = data.list_new,
                                                               years_all)

data.list_processed[[1]] %>% View
data.list_processed[[2]] %>% View
data.list_processed[[3]] %>% View
data.list_processed[[4]] %>% View
data.list_processed[[5]] %>% View # Classfication 1982 부터 존재
data.list_processed[[6]] %>% View
data.list_processed[[7]] %>% View
data.list_processed[[8]] %>% View





# 🟦 export ====================================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/exported_new"
for(i in seq_along(data.list_processed)){
  
  write.csv(data.list_processed[[i]], file.path(path_save, paste0(names(data.list_processed)[i], ".csv")), row.names = F)
  
}


















# 🟩 데이터 합치기 ====================================================================================
# data_final = read.xlsx("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/7.최종데이터/data1.xlsx")
# View(data_final)
path_data ="/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/exported_new"

data.list_processed = lapply(list.files(path_data, full.names = T), read.csv) %>% setNames(list.files(path_data) %>% gsub("\\.csv$", "", .))
names(data.list_processed )
names(data.list_processed) <- c(
  "매워심기", 
  "비료주기", 
  "어린나무가꾸기", 
  "조림지가꾸기_덩굴제거", 
  "조림지가꾸기_풀베기", 
  "큰나무가꾸기_경제림가꾸기_솎아베기", 
  "큰나무가꾸기_경제림가꾸기_천연림보육", 
  "큰나무가꾸기_공익림가꾸기"
)

# data.list_processed[[3]] %>% View


## 🟨 데이터 합치기 ======================================================================
combined_data = data.list_processed %>% lapply(function(x){
  x[[5]]
}) %>% 
  do.call(cbind, .) %>% 
  cbind(year = data.list_processed[[1]][["Classification"]], .) %>% 
  as_tibble %>% 
  mutate_all(~replace(., is.na(.), 0)) # NA  -> 0

# View(combined_data)

## 🟨 rename ======================================================================

## 🟨 Export ======================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/exported_new"
file_name = "foresttending_combined.xlsx"
file_path = file.path(path_save, file_name)
write.xlsx(combined_data, file_path)

# 
# 
# ## 🟦 열들 비교 ======================================================================
# path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/7.최종데이터"
# names(data_final)
# class(data_final)
# names(data_final)
# highlight_differences(data_final, 
#                       col_1 = c("youtending", 
#                                 "thinning", 
#                                 "nattending"), 
#                       col_2 = c("youtending_new", 
#                                 "thinning_new", 
#                                 "nattending_new"),
#                       path_save,
#                       "comparison.xlsx")
# 






test = read.csv("/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/숲가꾸기/exported_new/큰나무가꾸기_경제림가꾸기_솎아베기.csv")
View(test)


p = plot_time_series(values = test[[5]], labels = test[[3]])
path_save = "/Users/Ido/Downloads"
ggsave(file.path(path_save, "plot.png"), plot =  p , width = 10, height = 5, bg = "white")



