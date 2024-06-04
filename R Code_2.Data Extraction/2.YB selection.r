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


# 🟥 Data Load #####################################################################################################
# 파일 경로를 변수에 저장
file_path = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data/1.Sorted_YB_Names.csv"
data = read.csv(file_path)
L2 = data$L2 %>% table %>% names
length(L2)




# 🟥 clustering L2 category #####################################################################################################
k=22
clutered_L2 = text_clustering(L2, k_min = k, k_max = k)
clutered_L2$plot_cluster
clutered_L2$clusters




# 🟥 L2 카테고리 묶기 #####################################################################################################
## 🟧 데이터 ======================================================================================
text_data = L2

## 🟧 결과 저장 리스트 ======================================================================================
combined.list = list()


## 🟩 국민 계정과 생산가격지수 ======================================================================================
combined.list$`국민계정과 생산가격지수_National Accounts and Index number of Products Price` =
  union_multiple(filter_text_data(text_data, c("national", "accounts", "index", "number", "products", "price")),
                 filter_text_data(text_data, c("국민계정", "생산가격지수")))
## 🟩 조림/보호/산림의 건강 및 다양성 ======================================================================================
combined.list$`조림/보호/산림의 건강 및 다양성_Reforestation/protection/Forest Health and Diversity` =
  union_multiple(filter_text_data(text_data, c("forest", "health", "diversity")),
                 filter_text_data(text_data, c("산림", "건강", "다양성")),
                 filter_text_data(text_data, c("forest", "protection"), "reforestation"),
                 filter_text_data(text_data, c("산림", "보호"), "조림"),
                 filter_text_data(text_data, c("reforestation", "protection")),
                 filter_text_data(text_data, c("조림", "보호")),
                 filter_text_data(text_data, c("조림", "사방")))
# clustering_조림 = text_clustering(extract_L3_values(data, c("조림", "사방")),
#                                 k_min=30, k_max=30)
## 🟩 국토와 자연환경 ======================================================================================
combined.list$`국토와 자연환경_Land & Natural Environment` =
  union_multiple(filter_text_data(text_data, c("land", "natural", "environment")),
                 filter_text_data(text_data, c("국토", "자연", "환경")))
# clustering_국토 = text_clustering(extract_L3_values(data, c("국토", "환경")),
#                                 k_min=30, k_max=30)
# clustering_국토$clusters
## 🟩 국제통계 ======================================================================================
combined.list$`국제 산림 통계_International Statistics` =
  union_multiple(filter_text_data(text_data, c("international", "statistics")),
                 filter_text_data(text_data, c("국제", "통계")),
                 filter_text_data(text_data, c("국제", "산림")))
## 🟩 임야면적 및 임목축적 ======================================================================================
combined.list$`임야/산림면적 및 임목축적_Forest Land Area & Growing Stock` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "land", "area", "growing", "stock"),
                                  "국유림"),
                 filter_text_data(text_data, 
                                  c("면적", "임목", "축적"), 
                                  "국유림"),
                 filter_text_data(text_data, 
                                  c("연도별", "임상", "면적"), 
                                  "국유림"))
## 🟩 임산물 가격 및 기타 가격 ======================================================================================
combined.list$`임산물 가격 및 기타가격_Price of forest Products & Major Commodities` =
  union_multiple(filter_text_data(text_data, c("price", "forest", "product", "major", "commodities")),
                 filter_text_data(text_data, c("임산물", "기타","가격")))
## 🟩 임산물 시장 ======================================================================================
combined.list$`임산물시장_Forest Product Market` =
  union_multiple(filter_text_data(text_data, c("market", "forest", "product")),
                 filter_text_data(text_data, c("임산물", "시장")),
                 filter_text_data(text_data, c("임산물", "무역", "가공")))
# clustering_1 = text_clustering(extract_L3_values(data, c("임산물", "시장")),
#                              k_min=30, k_max=30)
# clustering_1$clusters
# filter_text_data(text_data, c("marketing", "forest", "product"))
# clustering_2 = text_clustering(extract_L3_values(data, c("임산물", "무역", "가공", "유통")),
#                              k_min=30, k_max=30)
# clustering_2$clusters
## 🟩 부록 ======================================================================================
combined.list$`부록_Appendix` =
  union_multiple(filter_text_data(text_data, c("appendix")),
                 filter_text_data(text_data, c("부록")))
## 🟩 임야/산림 자원 (forest resources) ======================================================================================
combined.list$`임야/산림 자원 조성_Silviculture` =
  union_multiple(filter_text_data(text_data, c("silviculture")),
                 filter_text_data(text_data, 
                                  c("임야", "자원", "조성")),
                 filter_text_data(text_data, 
                                  c("산림", "자원", "조성")),
                 filter_text_data(text_data, 
                                  c("forest", "resources"), 
                                  c("Silviculture", "조성")))
# clustering_1 = text_clustering(extract_L3_values(data, c("산림", "자원", "조성", "이용")),
#                                k_min=20, k_max=20)
# clustering_1$clusters
# clustering_2 = text_clustering(extract_L3_values(data, c("Silviculture")),
#                                k_min=20, k_max=20)
# clustering_2$clusters
# filter_text_data(text_data, c("forest", "resource"))
# clustering_2 = text_clustering(extract_L3_values(data, c("forest", "resource")),
#                              k_min=30, k_max=30)
# clustering_2$clusters
## 🟩 용도별 산지 이용 구분 ======================================================================================
combined.list$` 용도별 산지 이용구분 조사실적_Forest Land Use Classification` =
  union_multiple(filter_text_data(text_data, 
                                  c("용도별", "산지")))
## 🟩 임야/산림 경영 ======================================================================================
combined.list$`임업/산림 경영_Forest Management` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "management")),
                 filter_text_data(text_data, 
                                  c("산림", "경영")),
                 filter_text_data(text_data, 
                                  c("임업", "경영")),
                 filter_text_data(text_data, 
                                  c("국유림", "관리")))
# clustering_용도별 = text_clustering(extract_L3_values(data, c("산지",  "용도별")),
#                              k_min=1, k_max=1)
# clustering_용도별$clusters
# clustering = text_clustering(extract_L3_values(data, c("forest",  "management"), "national"),
#                              k_min=30, k_max=30)
# clustering$clusters
# clustering_4 = text_clustering(extract_L3_values(data, c("national", "forest",  "management", "경영")),
#                              k_min=30, k_max=30)
# clustering_4$clusters
## 🟩 임야/산림 서비스 ======================================================================================
combined.list$`임야/산림 서비스_Forest Service` =
  union_multiple(filter_text_data(text_data, c("forest", "service")),
                 filter_text_data(text_data, c("산림", "서비스")),
                 filter_text_data(text_data, c("임야", "서비스")))
# clustering_5 = text_clustering(extract_L3_values(data, c("forest", "service")),
#                              k_min=30, k_max=30)
# clustering_5$clusters
## 🟩 임업 생산 ======================================================================================
combined.list$`임업생산_Forest production` =
  union_multiple(filter_text_data(text_data, c("forest", "production")),
                 filter_text_data(text_data, c("임업", "생산")))
# clustering_6 = text_clustering(extract_L3_values(data, c("국민", "forest", "총생산")),
#                              k_min=30, k_max=30)
# clustering_6$clusters
# clustering_7 = text_clustering(extract_L3_values(data, c("forest", "총생산"), "국민"),
#                                k_min=30, k_max=30)
# clustering_7$clusters
## 🟩 교육 훈련 ======================================================================================
combined.list$`교육 훈련_Education and Training` =
  union_multiple(filter_text_data(text_data, c("education", "training")),
                 filter_text_data(text_data, c("교육", "훈련")))
## 🟩 임산 무역 가공 및 유통 ======================================================================================
combined.list$`임산 무역 가공 및 유통_Trede Processing and marketing of forest products` =
  union_multiple(filter_text_data(text_data, c("trade", "processing", "marketing", "forest", "products")),
                 filter_text_data(text_data, c("임산", "무역", "가공", "유통")),
                 filter_text_data(text_data, c("임산", "무역", "가공")))
# clustering_8 = text_clustering(extract_L3_values(data, c("trade", "processing")),
#                                k_min=30, k_max=30)
# clustering_8$clusters
## 🟩 사유림 관리 ======================================================================================
combined.list$`사유림 관리_Private Forest Administration` =
  union_multiple(filter_text_data(text_data, c("private", "forest")),
                 filter_text_data(text_data, c("사유림", "관리")),
                 filter_text_data(text_data, c("사유림")))
# clustering_사유림 = text_clustering(extract_L3_values(data, c("사유림", "관리")),
#                                k_min=30, k_max=30)
# clustering_사유림$clusters
## 🟩 재정과 금융 ======================================================================================
combined.list$`재정과 금융_Finances and Loans` =
  union_multiple(filter_text_data(text_data, c("finance", "loan")),
                 filter_text_data(text_data, c("재정", "금융")))



## 🟧 필터링 결과 확인 ======================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=5
# 에러 발생 시 print(unfiltered_data)를 실행
tryCatch({
  # 클러스터링 함수 실행
  clustering_result <- text_clustering(unfiltered_data, k_min = k, k_max = k)
  print(clustering_result$cluster)  # 클러스터링 결과 출력
}, error = function(e) {
  # 에러가 발생하면 unfiltered_data를 출력
  print(unfiltered_data)
})

if (length(unfiltered_data) > 0) {
  cat("필터링되지 않은 원소가 존재합니다:\n")
  print(unfiltered_data)
} else {
  cat("모든 원소가 필터링되었습니다.\n")
}


# 🟥 중복원소 확인 ==============================================================================
find_duplicates(combined.list)
names(combined.list)


# 🟥 하나의 데이터 프레임으로 합치기 ==============================================================================
# 새로운 열 생성
combined_data = data
combined_data$Categorized_L2 <- NA

# 각 카테고리 이름을 해당 L2 값에 할당
for (category_name in names(combined.list)) {
  category_values <- combined.list[[category_name]]
  combined_data$Categorized_L2[combined_data$L2 %in% category_values] <- category_name
}
combined_data = combined_data %>% relocate(Categorized_L2)



# 🟥 Export ==============================================================================
path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
write.csv(combined_data, paste0(path_save, "/2.L2 Categorized data.csv"), row.names=F)










