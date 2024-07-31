# 🟥 데이터 로드 =======================================================================
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names"
path_data = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/2.L2 Categorized data.csv"
data = read.csv(path_data)
L2_categories = data$Categorized_L2 %>% unique
# grep("임산물", data$NAME_L3, value=T) %>% unique




# 🟥 특정 L2 제외 =======================================================================
data_filtered = data %>% filter(!Categorized_L2 %in% c("교육 훈련_Education and Training",                                            
                                                       "국민계정과 생산가격지수_National Accounts and Index number of Products Price",
                                                       "부록_Appendix" ,
                                                       "재정과 금융_Finances and Loans",
                                                       "국제 산림 통계_International Statistics"))



# 🟥 L3 텍스트 필터링해서 그룹화 =====================================================================================================
# L3 추출
text_data = data_filtered$NAME_L3
combined.list = list()


# 여기서 total 파일 전체 실행
print(text_data)


# 이름 중복 확인-> 완료
length(names(combined.list) ) == names(combined.list) %>% unique %>% length


# 길이 1인 원소들 -> 완료
find_empty_names(combined.list)


# 저장 
path_file_name = file.path(path_save, "3.1.Combined L3 data.rds")
# saveRDS(combined.list, path_file_name)
combined.list = readRDS(path_file_name)





# 🟥 L3 Group 변수 생성 =====================================================================================================
# combined.list의 각 원소의 이름
combined_names <- names(combined.list)

# data_filtered에 새로운 열 Categorized_L3를 생성
data_filtered$Categorized_L3 <- NA

# combined.list의 각 원소를 순회하며 NAME_L3에 해당하는 그룹 이름을 부여
for (name in combined_names) {
  
  matching_indices <- data_filtered$NAME_L3 %in% combined.list[[name]]
  data_filtered$Categorized_L3[matching_indices] <- name
}

data_filtered = data_filtered %>% arrange(Categorized_L3) %>% relocate(Categorized_L3)
# View(data_filtered)

# NA인 카테고리 L3 확인
data_filtered %>% filter(is.na(Categorized_L3)) %>% pull(NAME_L3)



# 🟥 내보내기 =====================================================================================================
path_file_name = file.path(path_save, "3.2.Combined L3 data with L3 Category.csv")
write.csv(data_filtered, path_file_name, row.names = F)




















