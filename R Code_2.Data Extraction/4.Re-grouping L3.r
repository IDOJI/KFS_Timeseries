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
path_data = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data/3.L3 Categorized data.rds"
data = readRDS(path_data)




# 🟥 Select L2 by our objective categories #####################################################################################################
## 🟧 전체 카테고리 ===================================================================================
print(data %>% names)
cat(paste0('"', data %>% names, '"', collapse = ", "))

categories <- c("1.용도별 산지 이용 구분_Forest Land Use Classification", 
                "2.교육 훈련_Education and Training", 
                "3.국민계정과 생산가격지수_National Accounts and Index number of Products Price", 
                "4.국제 산림 통계_International Statistics", 
                "5.국토와 자연환경_Land & Natural Environment", 
                "6.부록_Appendix", 
                "7.사유림 관리_Private Forest Administration", 
                "8.임산 무역 가공 및 유통_Trede Processing and marketing of forest products", 
                "9.임산물 가격 및 기타가격_Price of forest Products & Major Commodities", 
                "10.임산물시장_Forest Product Market", 
                "11.임야/산림 서비스_Forest Service", 
                "12.임야/산림 자원 조성_Silviculture", 
                "13.임야/산림면적 및 임목축적_Forest Land Area & Growing Stock", 
                "14.임업/산림 경영_Forest Management", 
                "15.임업생산_Forest production", 
                "16.재정과 금융_Finances and Loans", 
                "17.조림/보호/산림의 건강 및 다양성_Reforestation/protection/Forest Health and Diversity")


all(categories %in% names(data))
categories[!categories %in% names(data)]



## 🟧 포함 =====================================================================================
include <- c("7.사유림 관리_Private Forest Administration",
             "8.임산 무역 가공 및 유통_Trede Processing and marketing of forest products",
             "9.임산물 가격 및 기타가격_Price of forest Products & Major Commodities",
             "10.임산물시장_Forest Product Market",
             "11.임야/산림 서비스_Forest Service",
             "12.임야/산림 자원 조성_Silviculture",
             "13.임야/산림면적 및 임목축적_Forest Land Area & Growing Stock",
             "14.임업/산림 경영_Forest Management",
             "15.임업생산_Forest production",
             "17.조림/보호/산림의 건강 및 다양성_Reforestation/protection/Forest Health and Diversity"
              )



## 🟧 제외항목들 L3그룹 이름들 내보내기 =====================================================================================
exclude = c("1.용도별 산지 이용 구분_Forest Land Use Classification",
            "2.교육 훈련_Education and Training",
            "3.국민계정과 생산가격지수_National Accounts and Index number of Products Price",
            "4.국제 산림 통계_International Statistics",
            "5.국토와 자연환경_Land & Natural Environment",
            "6.부록_Appendix",
            "16.재정과 금융_Finances and Loans"
            )


path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data/L2_Excluded"

# 특수문자 및 공백을 제거하는 함수
clean_name <- function(name) {
  gsub("[[:punct:] ]", "", name)
}

# 각 리스트 항목을 CSV 파일로 내보내기
for (name in exclude) {
  if (!is.null(data[[name]])) {
    wb <- createWorkbook()
    for (sheet_name in names(data[[name]])) {
      clean_sheet_name <- clean_name(sheet_name)
      # 시트 이름 길이를 31자로 제한
      if (nchar(clean_sheet_name) > 31) {
        clean_sheet_name <- substr(clean_sheet_name, 1, 31)
      }
      addWorksheet(wb, clean_sheet_name)
      writeData(wb, clean_sheet_name, data[[name]][[sheet_name]])
    }
    # 파일명에서 영어와 특수문자 제거
    clean_file_name <- clean_name(name)
    saveWorkbook(wb, paste0(path_save, "/", clean_file_name, ".xlsx"), overwrite = TRUE)
  }
}







# # 최대 길이를 계산하여 벡터들을 같은 길이로 맞추는 함수
# pad_vectors <- function(lst) {
#   max_length <- max(sapply(lst, length))
#   lapply(lst, function(x) {
#     length(x) <- max_length
#     return(x)
#   })
# }
# 
# # 반복문을 통해 각 원소를 개별 CSV 파일로 내보내기
# for (name in names(excluded)) {
#   # 각 원소 추출
#   item <- excluded[[name]]
#   
#   # 벡터 길이 맞추기
#   padded_item <- pad_vectors(item)
#   
#   # 데이터 프레임으로 변환
#   df <- as.data.frame(padded_item)
#   
#   # 파일 이름 생성 (공백 및 특수 문자 처리 필요 시 추가 처리)
#   file_name <- paste0(name, ".csv")
#   
#   # CSV 파일로 내보내기
#   write.csv(df, file = paste0(path_save, "/", file_name), row.names = FALSE, na = "")
# }






# 🟥 Categorize by L3 #####################################################################################################
## 🟧 합칠 원소들의 이름과 합쳐진 결과의 이름을 인풋으로 받는 함수 정의 =====================================================================================
merge_selected_data <- function(data, merge_names, result_name) {
  # 새로운 리스트 생성
  merged_data <- list()
  
  # 새로운 리스트에 기존의 원소들을 복사
  for (name in names(data)) {
    if (!(name %in% merge_names)) {
      merged_data[[name]] <- data[[name]]
    }
  }
  
  # 합칠 원소들의 데이터를 하나의 벡터로 합침
  merged_vector <- unlist(data[merge_names])
  
  # 합쳐진 결과의 이름을 새로운 리스트에 추가
  merged_data[[result_name]] <- merged_vector
  
  return(merged_data)
}


## 🟧 선택 L2 그룹들의 L3들 그룹이름 추출 =====================================================================================
selected_data = data[include]

L3_group_names = sapply(selected_data, names) %>% unlist
# L3_group_names = sapply(data, names) %>% unlist

# 상위 리스트를 없애고 하위 리스트들로 새로 데이터를 구성하는 함수 정의
flatten_selected_data <- function(data) {
  flat_data <- list()
  for (top_name in names(data)) {
    for (sub_name in names(data[[top_name]])) {
      flat_data[[sub_name]] <- data[[top_name]][[sub_name]]
    }
  }
  return(flat_data)
}
flattened_data = flatten_selected_data(selected_data)
L3_group_names = names(flattened_data)




## 🟧 L3 그룹 클러스터링 =====================================================================================
length(L3_group_names)
k = 150
clustered_L3_group = text_clustering(L3_group_names, k_min = k, k_max = k)
clustered_L3_group$clusters


# save data
flattened_data_2 = flattened_data



## 🟧 그룹 병합 =====================================================================================
### 🟦 사유림 ========================================================================
#### 🟪 사유림 소유 형태별 필지수 ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c("사유림 소유형태별 필지수 현황_Private Forest Lots By Ownership",
                                                       "사유림 소유형태별 필지수현황_Status of block of Forest Private Land by Ownership"),
                                       result_name = "사유림 소유형태별 필지수 현황_Private Forest Lots By Ownership")

#### 🟪 사유림 소유형태별 산림 면적  ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "사유림 소유형태별 산림면적_Private Forest Land Area by Ownership",
                                         "사유림 소유형태별 산림면적_Private Forest Land Area by Ownership",
                                         "다. 기타_Others___사유임야 소유형태별 임야/산림면적표_Table of Private Forest Area by Ownership Form"
                                       ),
                                       result_name = "사유림 소유형태별 산림면적_Private Forest Land Area by Ownership")
#### 🟪 "사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province"  ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province",
                                         "사유림 소재ㆍ부재 산주현황_Status of Resident and Non-resident Forest Owners by Province",
                                         "사유림 소재ㆍ부재 산주 현황_Status of Resident and Absentee Forest Owner",
                                         "사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province"
                                       ),
                                       result_name = "사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province")

#### 🟪 "사유림 소유규모별 산주현황_Private Forest Owners by Size"  ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "사유림 소유규모별 산주현황_Private Forest Owners by Size",
                                         "사유림 소유규모별 산주현황_Private Forest Owners by Size"
                                       ),
                                       result_name = "사유림 소유규모별 산주현황_Private Forest Owners by Size")
#### 🟪 "사유림 소유형태별 산주현황_Private Forest Owners By Ownership" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "사유림 소유형태별 산주현황_Private Forest Owners By Ownership",
                                         "사유림 소유형태별 산주현황_Private Forest Owners by Size"
                                       ),
                                       result_name = "사유림 소유형태별 산주현황_Private Forest Owners By Ownership")
#### 🟪 "사유림 협업 경영 사업_Activities of Private Forest Cooperatives" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "사유림 협업 경영 사업_Activities of Private Forest Cooperatives",
                                         "사유림 협업경영사업_Activities of Private Forest Cooperatives___관리기관별_Management offices",
                                         "사유림 협업경영사업_Activities of Private Forest Cooperatives"
                                       ),
                                       result_name = "사유림 협업 경영 사업_Activities of Private Forest Cooperatives")




### 🟦 생산실적 ========================================================================
#### 🟪 "제재목 생산 및 수급실적_Production and Supply of Sawnlog"  ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "제재목 생산 및 수급실적_Production and Supply of Sawnlog",
                                         "제재목 생산실적_Production and Supply of Sawnwood"
                                       ),
                                       result_name = "제재목 생산 및 수급실적_Production and Supply of Sawnlog")


#### 🟪 "지류 생산실적_Paper Production" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "지류 생산실적_Paper Production",
                                         "지류생산실적_Paper Production"
                                       ),
                                       result_name = "지류 생산 실적_Paper Production")


#### 🟪 "펄프 생산실적_Pulp Production"  ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "펄프 생산실적_Pulp Production", 
                                         "펄프 생산실적_Pulp Production",
                                         "펄프 생산실적_Pulp Production"
                                       ),
                                       result_name = "펄프 생산실적_Pulp Production")

#### 🟪 "관상수 생산실적_Ornamental Tree Production" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "관상수 생산실적_Ornamental Tree Production",
                                         "관상수 생산실적_Ornamental Tree Production",
                                         "관상수 생산실적_Ornamental Tree Production"
                                       ),
                                       result_name = "관상수 생산실적_Ornamental Tree Production")


#### 🟪 "칩 생산실적_Chip Production" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "칩 생산실적_Chip Production" ,
                                         "칩 생산실적_Chip Production" ,
                                         "칩 생산실적_Chip Production"
                                       ),
                                       result_name = "칩 생산실적_Chip Production")

#### 🟪 "목재가공품 생산 및 공급_Production and Supply of Processed Wood" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "목재가공품 생산 및 공급_Production and Supply of Processed Wood",
                                         "목재가공품 생산 및 공급_Production and Supply of Processed Wood"
                                       ),
                                       result_name = "목재가공품 생산 및 공급_Production and Supply of Processed Wood")

#### 🟪 "포플러 제품생산 및 공급실적_Production and Supply of Popular Products" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "포플러 제품생산 및 공급실적_Production and Supply of Popular Products",
                                         "포플러 제품 생산 및 공급 실적_Production and Supply of Popular Products",
                                         "포플러 제품 생산 및 공급실적_Production and Supply of Popular Products",
                                         "포플러 제품생산 및 공급실적_Production and Supply of Popular Products"
                                       ),
                                       result_name = "포플러 제품생산 및 공급실적_Production and Supply of Popular Products")



### 🟦 조림 ========================================================================
#### 🟪 "숲가꾸기/육림_Forest Tending" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "육림/숲가꾸기_Forest Tending",
                                         "숲가꾸기/육림_Forest Tending"
                                       ),
                                       result_name = "숲가꾸기/육림_Forest Tending")

#### 🟪 "가로수 심기 현황_Planting of Roadside Tree" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "가로수 심기현황_Plantation of Roadside Trees",  
                                         "가로수 심기 현황_Planting of Roadside Tree",    
                                         "가로수 나무심기 상황_Planting of Roadside Tree"
                                       ),
                                       result_name = "가로수 심기 현황_Planting of Roadside Tree")
#### 🟪 "조림활착상황_Survival Rates of Reforestation" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "조림활착상황_Survival Rates of Reforestation",
                                         "조림 활착상황_Survival Rate of Reforestation"
                                       ),
                                       result_name = "조림활착상황_Survival Rates of Reforestation")
#### 🟪 "재원별 조림실적_Accomplishment of Reforestation by Investments" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "재원별 조림실적_Accomplishment of Reforestation by Investments",
                                         "재원별 조림실적_Plantation by Fund"
                                       ),
                                       result_name = "재원별 조림실적_Accomplishment of Reforestation by Investments")

#### 🟪 "수종별 조림실적_Accomplishment of Reforestation by Species" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "수종별 조림실적_Accomplishment of Reforestation by Species",
                                         "수종별 조림실적_Plantation by Tree Species"
                                       ),
                                       result_name = "수종별 조림실적_Accomplishment of Reforestation by Species")
#### 🟪 "소유별 조림실적_Accomplishment of Reforestation by Ownership" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "소유별 조림실적_Accomplishment of Reforestation by Ownership",
                                         "소유별 조림실적_Plantation Area by Ownership"
                                       ),
                                       result_name = "소유별 조림실적_Accomplishment of Reforestation by Ownership")
#### 🟪 "조림, 사방과 보호 투자실적_Investment Accomplishment of Reforestation, Erosion Control and Protection" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "조림, 사방과 보호 투자실적_Investment Accomplishment of Reforestation, Erosion Control and Protection",
                                         "사방사업실적_Accomplishment of Erosion Control"
                                       ),
                                       result_name = "조림, 사방과 보호 투자실적_Investment Accomplishment of Reforestation, Erosion Control and Protection")



### 🟦 임산물 ========================================================================
#### 🟪 주요임산물 수출실적 ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "주요 임산물 수출실적_Exports of Major Forest Products", 
                                         "주요 임산물 수출 실적_Exports of Major Forest Products"
                                       ),
                                       result_name = "주요 임산물 수출 실적_Exports of Major Forest Products")
#### 🟪 주요 임산물 수입 실적_Imports of Major Forest Products" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "주요 임산물 수입실적_Imports of Major Forest Products" ,
                                         "주요 임산물 수입 실적_Imports of Major Forest Products"
                                       ),
                                       result_name = "주요 임산물 수입 실적_Imports of Major Forest Products")
#### 🟪 "주요 임산물 가격_Prices of Major Forest Products" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "주요 임산물 가격_Prices of Major Forest Products",
                                         "주요 임산물 가격_Prices of Major Forest Products"
                                       ),
                                       result_name = "주요 임산물 가격_Prices of Major Forest Products")


#### 🟪 임산물 남북교역 현황_Trade of Forest Products between South and North Korea" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "임산물 남북교역 현황_Trade of Forest Products between South and North Korea",
                                         "임산물 남북교역 현황_Trade and Forest Products between South and North Korea"
                                       ),
                                       result_name = "임산물 남북교역 현황_Trade of Forest Products between South and North Korea")
#### 🟪 "임산물 생산실적_Production of Forest Products" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "임산물 생산실적_Production of Forest Products",
                                         "임산물 생산량_Production of Forest Products"
                                       ),
                                       result_name = "임산물 생산실적_Production of Forest Products")
#### 🟪 "임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported",
                                         "임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported"
                                       ),
                                       result_name = "임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported")

### 🟦 산림 면적 & 임목축적  ========================================================================
#### 🟪 "산림청소관 국유림 관리 기관별, 임상별 산림면적_Forest Land Area by Management Agencies and Forest Type of National Forest under Forestry Administration" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "산림청소관 국유림 관리 기관별, 임상별 산림면적_Forest Land Area by Management Agencies and Forest Type of National Forest under Forestry Administration",
                                         "산림청소관 국유림 관리 기관별, 임상별 산림면적_Forest Land Area Forest Type of National Forest by Management Agencies and under Forestry Administration"
                                       ),
                                       result_name = "산림청소관 국유림 관리 기관별, 임상별 산림면적_Forest Land Area by Management Agencies and Forest Type of National Forest under Forestry Administration")

#### 🟪 "임상별, 영급별 임야면적 및 임목축적_Forest Area and Growing Stock by Forest Type and Age-Classes" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "임상별, 영급별 임야면적 및 임목축적_Forest Area and Growing Stock by Forest Type and Age-Classes",
                                         "임상별, 영급별 임야면적_Forest Area by Forest Type and Age-Classes"             
                                       ),
                                       result_name = "임상별, 영급별 임야면적 및 임목축적_Forest Area and Growing Stock by Forest Type and Age-Classes")


#### 🟪 "소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "소유별 임목축적_Forest growing stock by ownership",                          
                                         "소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership"
                                       ),
                                       result_name = "소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership")

#### 🟪 "연도별 ha당 평균임목축적_Mean Growing Stock per ha by Year" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "연도별 ha당 평균임목축적_Mean Growing Stock per ha by Year",
                                         "연도별 ㏊당 평균임목축적_Mean Growing Stock Per ㏊ by Year"
                                       ),
                                       result_name = "연도별 ha당 평균임목축적_Mean Growing Stock per ha by Year")
#### 🟪 "소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership",
                                         "소유별 임야면적_Area of forest land by ownership"
                                       ),
                                       result_name = "소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership")




### 🟦 "영림서등관리 국유림 기관별， 임상별  ========================================================================
#### 🟪 산림면적_Forest Area by National Forest management Authorities and Forest type" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "영림서등관리 국유림 기관별， 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type",
                                         "영림서등관리 국유림 기관별， 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type"
                                       ),
                                       result_name = "영림서등관리 국유림 기관별， 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type")

#### 🟪 임목축적_Growing Stock by National Forest management Authorities and Forest type" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "영림서등관리 국유림 기관별, 임상별 임목축적_Growing Stock by National Forest management Authorities and Forest type",
                                         "영림서등관리 국유림 기관별, 임상별 임목축적_Growing Stock by National Forest management Authorities and Forest type"
                                       ),
                                       result_name = "영림서등관리 국유림 기관별, 임상별 임목축적_Growing Stock by National Forest management Authorities and Forest type")


### 🟦 휴양림 ========================================================================
#### 🟪 "휴양림 조성현황_Establishment of Recreational Forest" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "휴양림 조성현황_Establishment of Recreational Forest",
                                         "휴양림 조성현황_Establishment of Recreation Forests",
                                         "휴양림 조성현황_Status of Recreational Forest"
                                       ),
                                       result_name = "휴양림 조성현황_Establishment of Recreational Forest")

#### 🟪 "휴양림 이용현황_Visitors to Recreational Forest" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "휴양림 이용현황_Visitors to Recreational Forest",
                                         "휴양림 이용현황_Visitors to Recreational Forests",
                                         "휴양림 운영 및 이용현황_Number of Visitors to Recreation Forests"
                                       ),
                                       result_name = "휴양림 이용현황_Visitors to Recreational Forest")



### 🟦 산림 ========================================================================
#### 🟪 "산림 형질변경 허가 상황_Permission for Conversion of Forest to Other Uses" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "산림 형질변경 허가 상황_Permission for Conversion of Forest to Other Uses",
                                         "산림 형질변경 허가 상황_Permission for Conversion of Forest to Other Uses"
                                       ),
                                       result_name = "산림 형질변경 허가 상황_Permission for Conversion of Forest to Other Uses")



### 🟦 목재 ========================================================================
#### 🟪 "용도별 국내재 공급 실적_Domestic Timber Supply by Use" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "용도별 국내재 공급 실적_Domestic Timber Supply by Use",
                                         "용도별 국내재 공급실적_Domestic Timber Supply by Uses"
                                       ),
                                       result_name = "용도별 국내재 공급 실적_Domestic Timber Supply by Use")

#### 🟪 "용도별 외재 도입 실적_Timber Imports By Use" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "용도별 외재 도입 실적_Timber Imports By Use",
                                         "용도별 외재도입실적_Timber Import by Use"
                                       ),
                                       result_name = "용도별 외재 도입 실적_Timber Imports By Use")
#### 🟪 "산지별 외재 도입 실적_Timber Imports by Origin" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "산지별 외재 도입 실적_Timber Imports by Origin",
                                         "산지별 외재도입실적_Timber Imports by Origin"
                                       ),
                                       result_name = "산지별 외재 도입 실적_Timber Imports by Origin")
#### 🟪 "목재 수급 계획 및 공급 실적_Facts of Demand and Supply for Timber" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "목재 수급 계획 및 공급 실적_Facts of Demand and Supply for Timber",
                                         "목재 수급 실적_Demand and Supply of Timber"
                                       ),
                                       result_name = "목재 수급 계획 및 공급 실적_Facts of Demand and Supply for Timber")


#### 🟪 "목재 및 목제품 제조업 실태_Status of Wood and Wood Products Manufacture" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "목재가공품 생산 및 공급_Production and Supply of Processed Wood",
                                         "나무 및 나무제품 제조업_Manufacture of Wood and Wood Products",
                                         "목재 및 목제품 제조업 실태_Status of Wood and Wood Products Manufacture"
                                       ),
                                       result_name = "목재 및 목제품 제조업 실태_Status of Wood and Wood Products Manufacture")



### 🟦 임업노동력현황_Number Of Workers Engaged In Forestry ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "임업노동력현황_Number Of Workers Engaged In Forestry",           
                                         "임업 노동력 현황_Number of Forestry Workers Engaged in Forestry"
                                         
                                       ),
                                       result_name = "임업 노동력 현황_Number of Forestry Workers Engaged in Forestry")










### 🟦 "독림가 및 임업후계자 현황_Outstanding Forest Managers and Forest Successors" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "독림가 및 임업후계자 현황_Devoted Forest Managers and Forest Successors",
                                         "독림가 및 임업후계자 현황_Outstanding Forest Managers and Forest Successors",
                                         "독림가 현황_Details of Sincere Forest Manager"
                                       ),
                                       result_name = "독림가 및 임업후계자 현황_Outstanding Forest Managers and Forest Successors")

### 🟦 "산지 이용 구분_Classification of Forest Land Utilization" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "산지이용구분현황_Classification of Forest Land Utilization",
                                         "산지 이용 구분_Classification of Forest Land Utilization"  
                                       ),
                                       result_name = "산지 이용 구분_Classification of Forest Land Utilization")


### 🟦 "양묘 시업 상황_Operation of Tree Seeding Production" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "양묘 시업 상황_Operation of Tree Seeding Production",
                                         "양묘시업상황_Operation of Tree Seeding Production" 
                                       ),
                                       result_name = "양묘 시업 상황_Operation of Tree Seeding Production")












### 🟦 "해외 산림개발 진출현황_Oversea Forest Development" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "해외 산림개발 진출현황_Oversea Forest Development",
                                         "해외산림개발진출현황_Overseas Forest Development"
                                       ),
                                       result_name = "해외 산림개발 진출현황_Oversea Forest Development")



### 🟦 "임도시설 현황_Status of Forest Road" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "임도시설 현황_Status of Forest Road",
                                         "임도시설 현황_Construction of Forest Road",
                                         "임도시설 현황_Status of Forest Road"
                                       ),
                                       result_name = "임도시설 현황_Status of Forest Road")








### 🟦 "국유림직영벌채사업생산 및 매각실적_Production and Sales of Timber by Government Felling" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "국유림직영벌채사업생산 및 매각실적_Production and Sales of Timber by Government Felling",
                                         "관행작벌 사업생산 및 매각실적_Production and Sale of Timber by Government Felling"
                                       ),
                                       result_name = "국유림직영벌채사업생산 및 매각실적_Production and Sales of Timber by Government Felling")



### 🟦 지수 ========================================================================
#### 🟪 "생산자 물가지수_Producer price index" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "생산자 물가지수_Producer price index",
                                         "생산자 물가지수_Producer Price Indexes"
                                       ),
                                       result_name = "생산자 물가지수_Producer price index")

#### 🟪 소비자, 수출, 수입 물가지수_Consumer, exportation, importaton price index" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "소비자, 수출, 수입 물가지수_Consumer, exportation, importaton price index",
                                         "소비자 물가지수_Index Numbers of Consumer Price"
                                       ),
                                       result_name = "소비자, 수출, 수입 물가지수_Consumer, exportation, importaton price index")



### 🟦 "산림사업 융자규모_Loans for Forestry Business" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "산림사업 융자규모_Loans for Forestry Business",
                                         "산림사업 융자실적_Loans for Forest Activities"
                                       ),
                                       result_name = "산림사업 융자규모_Loans for Forestry Business")

### 🟦 "산림복지서비스 제공자 등록 현황_Enrollment of Forest Welfare Service Voucher-available-facilities" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "산림복지서비스 제공자 등록 현황_Enrollment of Forest Welfare Service Voucher-available-facilities",
                                         "산림복지전문업 등록현황_Status of Job Startups on Forest welfare"
                                       ),
                                       result_name = "산림복지서비스 제공자 등록 현황_Enrollment of Forest Welfare Service Voucher-available-facilities")
### 🟦 "국내총생산과 임업_Gross Domestic Product and Forestry" ========================================================================
flattened_data_2 = merge_selected_data(flattened_data_2, 
                                       merge_names = c(
                                         "국내총생산과 임업_Gross Domestic Product and Forestry",
                                         "국내총생산과 임업생산_Gross Domestic Product and Forestry Product",
                                         "국내 총 생산과 임산물생산_Gross Domestic Product and Forest Products"
                                       ),
                                       result_name = "국내총생산과 임업_Gross Domestic Product and Forestry")










# 🟥 결과 확인 #####################################################################################################
length(flattened_data_2)
grouped_L3 = names(flattened_data_2)
clustered_L3 = text_clustering(grouped_L3, k_min = 200, k_max = 200)
clustered_L3$clusters






# 🟥 새로운 열 생성 #####################################################################################################
path_L2_Categorized_data = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data/2.L2 Categorized data.csv"
L2_Categorized_data = read.csv(path_L2_Categorized_data)


# L3 값을 기준으로 각 카테고리 이름을 찾아서 새로운 열 Categorized_L3에 할당
L3_Categorized_data <- L2_Categorized_data %>%
  rowwise() %>%
  mutate(Categorized_L3 = {
    found_category <- NA
    for (category_name in names(flattened_data_2)) {
      if (L3 %in% flattened_data_2[[category_name]]) {
        found_category <- category_name
        break
      }
    }
    found_category
  }) %>%
  ungroup() %>% 
  relocate(Categorized_L3)


L3_category = L3_Categorized_data$Categorized_L3 %>% table %>% names



# 🟥 Export by group #####################################################################################################
# 데이터 저장 경로
path_save <- "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data/L3_Included"

# 한국어만 추출하는 함수
extract_korean <- function(text) {
  str_extract_all(text, "[가-힣]+") %>%
    unlist() %>%
    paste(collapse = "")
}

# 데이터를 그룹별로 나누고 csv 파일로 저장하는 함수
save_grouped_csv <- function(data, path_save) {
  # 그룹별로 데이터를 나눕니다
  grouped_data <- data %>%
    group_by(Categorized_L3) %>%
    arrange(combined_names) %>%
    group_split()
  
  # 각 그룹별로 csv 파일을 생성합니다
  for (group in grouped_data) {
    group_name <- unique(group$Categorized_L3)
    group_sorted <- group %>%
      select(combined_names) %>%
      arrange(combined_names)
    
    # 파일 이름에서 한국어만 추출
    file_name <- extract_korean(group_name)
    file_path <- file.path(path_save, paste0(file_name, ".csv"))
    
    # 디렉토리가 존재하지 않으면 생성
    if (!dir.exists(path_save)) {
      dir.create(path_save, recursive = TRUE)
    }
    
    # CSV 파일로 저장
    write.csv(group_sorted, file_path, row.names = FALSE)
  }
}

# 함수 호출
save_grouped_csv(L3_Categorized_data, path_save)



# 🟥 Export df #####################################################################################################
path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
write.csv(L3_Categorized_data, paste0(path_save, "/4.L3 Re-Categorized data.csv"), row.names = F)







