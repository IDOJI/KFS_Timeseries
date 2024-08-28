# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
## 🟨Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # packages : a vector of package names to install and load
  # load : load the packages after installation?
  
  for(pkg in packages) {
    # Check if the package is installed
    if (!require(pkg, character.only = TRUE)) {
      # If not installed, install the package
      install.packages(pkg)
    }
    
    # If load is TRUE, load the package
    if(load){
      library(pkg, character.only = TRUE, quietly = TRUE)
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

## 🟧 L3 추출 함수 정의 ==================================================================
extract_L3_values <- function(data, include_keywords, exclude_keywords = NULL) {
  # L2 열에서 포함할 키워드를 포함한 고유한 원소 추출
  matched_L2_values <- unique(data$L2[grep(paste(include_keywords, collapse = "|"), data$L2)])
  
  # 제외할 키워드가 주어지면 해당 키워드를 포함하는 원소 제거
  if (!is.null(exclude_keywords)) {
    exclude_pattern <- paste(exclude_keywords, collapse = "|")
    matched_L2_values <- matched_L2_values[!grepl(exclude_pattern, matched_L2_values)]
  }
  
  # 추출된 L2 값을 갖는 행들의 L3 값 추출
  matched_L3_values <- unique(data$L3[data$L2 %in% matched_L2_values])
  
  return(matched_L3_values)
}










