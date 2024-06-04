# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())

Sys.setlocale("LC_ALL", "en_US.UTF-8")

## 🟩Install and loading Packages ================================
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
load_functions = function(path_functions){
  list.files(path_functions, full.names = T) %>%
    purrr::walk(source)
}
path_list = list()
path_list[1] = "/Users/Ido/Library/CloudStorage/Dropbox/1.GitHub/R___refineR/R"
path_list[2] = "/Users/Ido/Library/CloudStorage/Dropbox/1.GitHub/R___StatsR/R"
Load = sapply(path_list, load_functions)




# 🟥 Define a clustering function #####################################################################################################
## 🟧 필요한 패키지 로드 ##############################################################################################################
library(tm)
library(proxy)
library(cluster)
library(factoextra)
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
library(tm)
library(cluster)
library(factoextra)





## 🟧 텍스트 클러스터링 함수 정의 ##############################################################################################################
text_clustering <- function(text_data, special_cases = list(), k_min = 2, k_max = length(text_data)-1) {
  # 전처리된 텍스트 데이터를 저장할 벡터 생성
  cleaned_vector <- text_data
  
  # 특별 케이스를 적용
  for (case in names(special_cases)) {
    cleaned_vector <- gsub(case, special_cases[[case]], cleaned_vector)
  }
  
  # 숫자, 밑줄, 마침표 제거
  cleaned_vector <- gsub("[0-9_.]", "", cleaned_vector)
  
  # 텍스트 데이터를 코퍼스로 변환 (cleaned_vector 사용)
  corpus <- Corpus(VectorSource(cleaned_vector))
  
  # 용어 문서 행렬 생성
  tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf)))
  tdm_matrix <- as.matrix(tdm)
  tdm_matrix <- t(tdm_matrix)
  
  # TF-IDF 가중치 부여
  tfidf_transform <- weightTfIdf(tdm)
  tdm_matrix <- as.matrix(tfidf_transform)
  tdm_matrix <- t(tdm_matrix)
  
  # 사용되지 않는 열 제거
  non_zero_columns <- apply(tdm_matrix, 2, function(col) sum(col != 0)) > 1
  tdm_matrix <- tdm_matrix[, non_zero_columns]
  
  # 데이터 포인트의 수가 클러스터 수보다 많은지 확인
  num_data_points <- nrow(tdm_matrix)
  if (k_max > num_data_points) {
    stop("k_max is greater than the number of distinct data points.")
  }
  
  # 실루엣 점수를 계산하여 최적의 클러스터 수 선택
  silhouette_score <- function(k) {
    km <- kmeans(tdm_matrix, centers = k, nstart = 25)
    ss <- silhouette(km$cluster, dist(tdm_matrix))
    mean(ss[, 3])
  }
  
  k_values <- k_min:k_max
  avg_sil <- sapply(k_values, silhouette_score)
  
  best_k <- k_values[which.max(avg_sil)]
  print(paste("Best number of clusters:", best_k))
  
  # K-means 클러스터링 수행
  km <- kmeans(tdm_matrix, centers = best_k, nstart = 25)
  
  # 원래의 text_data와 클러스터 할당을 데이터 프레임으로 저장
  data_clusters <- data.frame(text = text_data, cluster = km$cluster)
  data_clusters <- data_clusters[order(data_clusters$cluster), ]
  
  # 클러스터별 텍스트 목록 생성 (원래의 text_data 사용)
  clusters_list <- lapply(unique(data_clusters$cluster), function(cluster) {
    data_clusters$text[data_clusters$cluster == cluster]
  })
  
  # 클러스터링 결과 시각화
  plot_cluster <- fviz_cluster(km, data = tdm_matrix, geom = "point", labelsize = 5, ggtheme = theme_minimal())
  
  tdm_matrix_pca <- prcomp(tdm_matrix, scale. = TRUE)
  tdm_matrix_pca_data <- as.data.frame(tdm_matrix_pca$x)
  tdm_matrix_pca_data$cluster <- as.factor(km$cluster)
  
  plot_pca <- fviz_pca_ind(tdm_matrix_pca, geom = "point", habillage = tdm_matrix_pca_data$cluster, 
                           addEllipses = TRUE, ellipse.level = 0.95, ggtheme = theme_minimal())
  
  # 결과 반환
  return(list(clusters = clusters_list, plot_cluster = plot_cluster, plot_pca = plot_pca, tdm_matrix = tdm_matrix, data_clusters = data_clusters))
}






## 🟧 클러스터링 결과 합치는 함수 정의 ##############################################################################################################
merge_clusters <- function(clustering_result, combined.list) {
  # 기존 클러스터링 결과 가져오기
  data_clusters <- clustering_result$data_clusters
  tdm_matrix <- clustering_result$tdm_matrix
  
  # 새로운 클러스터링 그룹 할당
  data_clusters$new_cluster <- NA
  for (i in 1:length(combined.list)) {
    data_clusters$new_cluster[data_clusters$cluster %in% combined.list[[i]]] <- i
  }
  
  # 새로운 클러스터 리스트 생성
  new_clusters_list <- lapply(seq_along(combined.list), function(kth_cluster) {
    data_clusters %>% filter(new_cluster == kth_cluster) %>% pull(text)
  }) %>% setNames(names(combined.list))
  
  
  # PCA 시각화 준비
  tdm_matrix_pca <- prcomp(tdm_matrix, scale. = TRUE)
  tdm_matrix_pca_data <- as.data.frame(tdm_matrix_pca$x)
  tdm_matrix_pca_data$cluster <- as.factor(data_clusters$new_cluster)
  
  plot_pca <- ggplot(tdm_matrix_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2) +
    stat_ellipse(level = 0.95) +
    theme_minimal() +
    labs(title = "PCA of Merged Clusters")
  
  # 클러스터 시각화 준비
  plot_cluster <- ggplot(tdm_matrix_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(title = "Merged Clusters Visualization")
  
  return(list(clusters = new_clusters_list, plot_cluster = plot_cluster, plot_pca = plot_pca, data_clusters = data_clusters))
}








## 🟧 사용 예시 ##############################################################################################################
# 예시 데이터
# text_data <- c("예시 문장1", "예시 문장2", "예시 문장3", "예시 문장4", "예시 문장5", 
#                "예시 문장6", "예시 문장7", "예시 문장8", "예시 문장9", "예시 문장10",
#                "예시 문장11", "예시 문장12", "예시 문장13", "예시 문장14", "예시 문장15",
#                "예시 문장16", "예시 문장17", "예시 문장18", "예시 문장19", "예시 문장20")
# 
# result_1 <- text_clustering(text_data, k_min = 8, k_max = 8)
# result_1$clusters
# result_1$data_clusters
# # 결과 출력 및 사용자 정의 클러스터 합치기
# combined.list <- list(c(1, 2, 3, 4, 7), c(5, 6), 8)
# merged_result <- merge_clusters(result_1, combined.list)
# 
# # 병합된 클러스터 결과 출력
# print(merged_result$clusters)

# 시각화 출력
# print(merged_result$plot_cluster)
# print(merged_result$plot_pca)





## 🟧 여러 개의 원소에 union을 적용하는 함수 정의 ##############################################################################################################
# union_multiple 함수 정의
union_multiple <- function(...) {
  lists <- list(...)
  result <- Reduce(union, lists)
  return(result)
}



## 🟧 텍스트 필터링 함수 ##############################################################################################################
# 대소문자 구별 없이 필터링하는 함수 정의
# 대소문자 구별 없이 필터링하는 함수 정의
filter_text_data <- function(text_data, include = NULL, exclude = NULL) {
  if (!is.null(include)) {
    for (inc in include) {
      text_data <- text_data[grep(inc, text_data, ignore.case = TRUE)]
    }
  }
  
  if (!is.null(exclude)) {
    for (exc in exclude) {
      text_data <- text_data[!grepl(exc, text_data, ignore.case = TRUE)]
    }
  }
  
  return(text_data)
}


## 🟧 중복항목확인 함수 ##############################################################################################################
find_duplicates <- function(list) {
  any_duplicates_found <- FALSE
  for (category in names(list)) {
    duplicated_items <- duplicated(list[[category]]) | duplicated(list[[category]], fromLast = TRUE)
    if (any(duplicated_items)) {
      any_duplicates_found <- TRUE
      cat("Category:", category, "\n")
      cat("Duplicates:", list[[category]][duplicated_items], "\n\n")
    }
  }
  
  if (!any_duplicates_found) {
    cat("전체 리스트에서 중복 항목 없음\n")
  }
}






# 🟥 Data Load #####################################################################################################
path_id = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data/4.L3 Re-Categorized data.csv"
id = read.csv(path_id)
# id %>% View

path_data = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_DATA_20240415.xlsx"
data = read.xlsx(path_data)


path_header = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/Data/DB_20240415/TB_NDI_MRV_STTST_YRBK_OF_FRSTR_HDR_20240415.xlsx"
header = read.xlsx(path_header)



# 🟥 id L3 확인 #####################################################################################################
id$Categorized_L3 %>% table %>% names






# 🟥 raw 데이터 정리(리스트화) #####################################################################################################
## 🟧 데이터 체크 #####################################################################################################
id %>% filter(연보.ID =="YRBK_0049040802")
header %>% filter(연보.ID =="YRBK_0049040802")
data %>% filter(연보.ID =="YRBK_0049040802") %>% View
# test %>% filter(연보.ID == "YRBK_0049040802")



## 🟧 연보ID에 따라 데이터 리스트화 #####################################################################################################
# # split 함수를 사용하여 "연보.ID"로 그룹화
# grouped_data <- split(data, data$연보.ID)
# 
# # 각 "연보.ID"를 리스트의 이름으로 설정
# result_list <- lapply(names(grouped_data), function(id) {
#   return(grouped_data[[id]])
# })
# names(result_list) <- names(grouped_data)
# 
# length(result_list) == data$연보.ID %>% unique %>% length

  
## 🟧 임시 save #####################################################################################################
path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
# saveRDS(result_list, paste0(path_save, "/5.Raw Data as a list.rds"))
data.list = readRDS(list.files(path_save, pattern = "Raw Data as", full.names=T)) 








# 🟥 header 추가 #####################################################################################################
## 🟧 함수 정의 ===========================================================================================
# 모든 행의 원소가 NA인 열을 제거하는 함수 정의
remove_na_columns <- function(df) {
  df_clean <- df[, colSums(is.na(df)) < nrow(df)]
  return(df_clean)
}
# 헤더 행 합치기 함수
combine_columns <- function(df) {
  # 필요한 패키지 로드
  if(!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  library(dplyr)
  
  # 첫 번째, 두 번째 열 제외한 나머지 열을 결합
  df %>%
    group_by(연보.ID, 행) %>%
    summarise(across(starts_with("열"), ~ paste(.[1], .[2], sep = "_")), .groups = 'drop')
}
# 열이 존재하는지 확인하는 함수
remove_column_if_exists <- function(data, column_name) {
  if (column_name %in% colnames(data)) {
    data <- data %>% dplyr::select(-all_of(column_name))
  }
  return(data)
}




## 🟧 각 ID에 대해 합치기 ===========================================================================================
# Extract ID
yb_id = id %>% 
  filter(!is.na(Categorized_L3)) %>% 
  pull(연보.ID) %>% 
  unique

# 확인용
check_id = c()

# 결과 저장용
combined.list = list()

# Add header
for(i in seq_along(yb_id)){
  ### 🟩 extract ID =================================================================
  ith_id = yb_id[i]
  # ith_id = "YRBK_00010302"
  # ith_id = "YRBK_0049040802"
  # ith_id =  "YRBK_00010801"
  
  ### 🟩 hdr =================================================================
  ith_hdr <- header %>% 
    filter(연보.ID == ith_id) %>% 
    remove_na_columns() %>% 
    remove_column_if_exists("언어.코드")
  # '헤더행' 열이 존재하면 이름을 변경
  if ("헤더행" %in% colnames(ith_hdr)) {
    ith_hdr <- ith_hdr %>% 
      rename(행 = 헤더행) %>% 
      combine_columns()
  }
  
  # "연보.ID"와 "행" 열 제외
  ith_hdr_sub <- ith_hdr[, !(names(ith_hdr) %in% c("연보.ID", "행"))]
  
  # "NA_NA" 값 제외하고 문자열 합치기
  combined <- apply(ith_hdr_sub, 2, function(col) {
    paste(col[col != "NA_NA"], collapse = "_")
  }) %>% unname
  
  # combined 값을 데이터 프레임 형식으로 변환
  combined_df <- as.data.frame(t(combined), stringsAsFactors = FALSE)
  colnames(combined_df) <- names(ith_hdr_sub)
  
  # 원본 데이터 프레임에 반영
  ith_hdr = ith_hdr[1,]
  ith_hdr[1, !(names(ith_hdr) %in% c("연보.ID", "행"))] <- combined_df
  
  
  
  ### 🟩 data =================================================================
  ith_data = data.list[names(data.list)==ith_id]
  
  if(length(ith_data)!=0 && class(ith_data) == "list"){
    ith_data = data.list[names(data.list)==ith_id][[1]] %>% 
      remove_na_columns() %>% 
      rename(행 := 데이터행) %>% 
      arrange(행)
    
    
    # 열 이름 얻기
    all_columns <- union(names(ith_hdr), names(ith_data))
    
    # 존재하지 않는 열을 추가
    for (col in all_columns) {
      if (!col %in% names(ith_hdr)) {
        ith_hdr[[col]] <- NA
      }
      if (!col %in% names(ith_data)) {
        ith_data[[col]] <- NA
      }
    }
    
    # 열 이름 정렬
    ith_hdr <- ith_hdr %>% select(all_of(all_columns))
    ith_data <- ith_data %>% select(all_of(all_columns))
    
    # 데이터프레임 합치기
    combined_df <- rbind(ith_hdr, ith_data)
    
    # 첫 두 열은 그대로 두고, 나머지 열을 알파벳 순서로 정렬
    sorted_columns <- c(names(combined_df)[1:2], sort(names(combined_df)[-c(1, 2)]))
    
    # 정렬된 열 순서로 데이터프레임 재구성
    sorted_combined_df <- combined_df %>% select(all_of(sorted_columns))
    
    
    # 첫 번째 행을 열 이름으로 설정
    new_colnames <- as.character(sorted_combined_df[1, ])
    colnames(sorted_combined_df) <- new_colnames
    
    # 첫 번째 행 제거
    sorted_combined_df <- sorted_combined_df[-1, ]
    names(sorted_combined_df)[1:2] = c("연보.ID", "행")
    
    # 결과 저장
    combined.list[[i]] = sorted_combined_df
    
    cat("\n", crayon::green(i), crayon::red(ith_id), crayon::green("is done!"),"\n")
  }else{
    
    check_id = c(check_id, ith_id)
    
  }
  

}


## 🟧 연보.ID를 리스트의 이름으로 설정 ===========================================================================================
names(combined.list) <- sapply(combined.list, function(df) df$연보.ID[1])
combined.list$YRBK_00010201
combined.list$YRBK_00010302
combined.list$YRBK_00010312
combined.list$YRBK_00020309





## 🟧 연보.ID이름에 L3 그룹 이름 추가 ===========================================================================================
# 새로운 이름 생성 함수
new_names <- sapply(names(combined.list), function(연보.ID) {
  L3_category <- id %>%
    filter(연보.ID == !!연보.ID) %>%
    select(Categorized_L3) %>%
    unique() %>%
    pull(Categorized_L3) %>%
    str_extract("^[^_]+")
  
  
  combined_names <- id %>%
    filter(연보.ID == !!연보.ID) %>%
    pull(combined_names)
  
  # 새로운 이름 생성
  paste0(연보.ID, "___", L3_category, "______", combined_names)
}) %>% unname


head(new_names)
names(combined.list) <- new_names





## 🟧 결과 저장 ===========================================================================================
path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
saveRDS(combined.list, paste0(path_save, "/5.Combined hdr data.rds"))




