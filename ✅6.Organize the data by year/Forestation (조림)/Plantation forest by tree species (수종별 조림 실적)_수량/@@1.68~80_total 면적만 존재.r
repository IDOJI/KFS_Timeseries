# 🟨 데이터수정함수 =============================================================================================
update_column_names <- function(df) {
  conifer_trees <- c("리기다송", "낙엽송", "잣나무", "해송", "은행",
                     "삼나무", "편백", "소나무", "리기테다", "테다", 
                     "테다")
  
  broad_trees <- c("산오리", "사방오리", "물갬나무", "아까시아", 
                   "상수리", "밤나무", "감나무", 
                   "고염나무", "호도나무", "대추나무", "옻나무", 
                   "오동", "유동", "삼지목", "대나무", 
                   "굴참나무", "가래나무", "은사시", "개량포플러", "은사시")
  
  names(df) = sapply(names(df), function(name) {
    tree_name <- gsub("_본수.*", "", name) %>% 
      gsub("수량_", "", .)
    if (tree_name %in% broad_trees) {
      paste0("활엽수_", name)
    } else if (tree_name %in% conifer_trees) {
      paste0("침엽수_", name)
    } else {
      name
    }
  })
  return(df)
}

modify_data = function(df){
  # df = x
  id = unique(df[["ID"]])
  
  if(id == "YRBK_00020309"){
    
    df[["total_seedling"]] = c(236861, 166383, 418555, 91614, 368390, 294389, 288085, 591032, 452442)
    df[["리기다송_본수"]] = c(58723, 66618, 96968, 18617, 44620, 60898, 35247, 623058, 215016)
    df[["낙엽송_본수"]] = c(41964, 35514, 27668, 18990, 40113, 29717, 44425, 53461, 35362)
    df[["잣나무_본수"]] = c(1167, 3312, 2014, 2247, 2869, 3079, 2988, 6706, 6493)
    df[["해송_본수"]] = c(19470, 24476, 13881, 14822, 24247, 12195, 4648, 5979, 12489)
    df[["삼나무_본수"]] = c(5395, 2004, 3498, 959, 6242, 8452, 9607, 13148, 4969)
    df[["편백_본수"]] = c(4121, 152, NA, 1196, 2474, 1800, 10100, 4890, 12180)
    df[["테다_본수"]] = c(NA, NA, NA, NA, NA, NA, NA, NA, 7744)
    df[["개량포푸라_본수"]][nrow(df)] = 4956
    df[["밤나무_본수"]][nrow(df)] = 1585
    df[["소나무_본수"]] = c(NA, NA, NA, NA, NA, NA, 140, NA, NA)
    df[["리기테다_본수"]] = c(NA, NA, NA, 40, NA, 350, 437, 2574, 2760)
    df[["굴참나무_본수"]] = NULL
    df[["가래나무_본수"]] = NULL
    df[["기타_본수"]] = c(10672, 1387, 724, 705, 66, 397, 740, 11, 644)
    
  }else if(id %in% c("YRBK_00050309", "YRBK_00070309")){
    
    df = df %>% 
      mutate(리기다송_본수 = ifelse(리기다송_본수 == 686902, 68690.2, 리기다송_본수))
    
  }
  
  
  
  if(id == "YRBK_00050309"){
    # x = combined_data[[5]]
    # names(df)
    df = df %>% 
      mutate(기타_본수 = rowSums(select(., "기타_본수_1", "기타_본수_2"), na.rm = T)) %>% 
      select(-"기타_본수_1", -"기타_본수_2")
  }
  
  # 개량 포푸라 이름 바꾸기
  if(grepl("개량포푸라", names(df)) %>% sum > 0){
    names(df)[grep("개량포푸라", names(df))] = "개량포플러"
  }
  
  # 기타 이름 바꾸기
  if(grepl("기타", names(df)) %>% sum > 0){
    names(df)[grep("기타", names(df))] = "기타_본수"
  }
  
  
  return(df)
}







# 🟥 데이터 정리 =============================================================================================
path_data = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/1.68~80"
# combined_data[[2]] %>% View
combined_data <- path_data %>% 
  list.files(full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  setNames(tools::file_path_sans_ext(list.files(path_data))) %>% 
  lapply(function(x) {
    # print(x$ID %>% unique)
    x = x %>% 
      rename_with(~ gsub("_NA", "_본수", .)) %>%  # 열 이름에서 "_NA"를 "_본수"로 대체
      rename(classification = names(.)[3]) %>%  # 세 번째 열 이름을 classification으로 변경
      select(-2) %>%  # 두 번째 열 제거
      select(-contains("면적")) %>%  # "면적"이 포함된 열 제거
      filter(grepl("^\\d{4}$", classification)) %>%   # classification 열에 연도만 있는 행만 필터링
      select(-all_of(grep("L1|L2|L3|L4|L5", names(.)))) %>% 
      relocate(year, .after = classification) %>% 
      rename(total_seedling = 계_수량) %>% 
      modify_data %>% # 오류 데이터 수정
      update_column_names %>% # 열이름 수정 함수
      # 데이터 합계
      mutate(total_seedling_direct = rowSums(select(., 5:ncol(.)), na.rm = T)) %>%
      mutate(diff_abs = abs(total_seedling_direct - total_seedling)) %>%
      relocate(total_seedling_direct, diff_abs, .after = total_seedling) %>% 
      mutate(
        conifer_total_seedling = NA %>% as.numeric,
        broad_total_seedling = NA %>% as.numeric
      ) %>% 
      # 침엽수, 활엽수 열들 합
      mutate(
        # "침엽수_"로 시작하는 열들의 합을 계산하여 새로운 열에 저장
        conifer_total_seedling_direct = rowSums(select(., starts_with("침엽수_")), na.rm = TRUE),
        # "활엽수_"로 시작하는 열들의 합을 계산하여 새로운 열에 저장
        broad_total_seedling_direct = rowSums(select(., starts_with("활엽수_")), na.rm = TRUE)
      ) %>% 
      select(-all_of(starts_with("활엽수_"))) %>% 
      select(-all_of(starts_with("침엽수_")))
  }) %>% 
  do.call(rbind, .) %>% 
  mutate(year = as.character(year)) %>% 
  arrange(classification, year) %>% 
  # 이상한 데이터 제거
  filter(!(year == "1979" & classification == "1966")) %>% 
  filter(!(classification == "1966" & year == "1980")) %>% 
  group_by(classification) %>%
  filter(year == max(year)) %>%
  ungroup() 
# %>% 
  # mutate(total_seedling_direct_new = conifer_total_seedling_direct + broad_total_seedling_direct + 기타_본수) %>% 
  # mutate(diff_abs_2 = abs(total_seedling_direct_new - total_seedling)) %>% View




# 🟥 export  ===================================================================
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/4.Exported Data_by ID_2/조림/수종별 조림실적Plantation forest by tree species/Combined"
write.xlsx(combined_data, file.path(path_save, "1.Combined_68~80.xlsx"))
# year_rows$year





