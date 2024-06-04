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








# 🟥 Data Load #####################################################################################################
path_data = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data/2.L2 Categorized data.csv"
data = read.csv(path_data)





# 🟥 Check L2 Categories #####################################################################################################
L2_categories = data$Categorized_L2 %>% table %>% names

cat(L2_categories, sep = "\n")






# 🟥 Check the specific keywords for each L2 category #####################################################################################################
i=3
L2_categories[i]
data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names %>% head(100)




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









# 🟥 Results list #####################################################################################################
results.list = list()






# 🟥 Clustering #####################################################################################################
## 🟧 1.용도별 산지 이용 구분 (Forest Land Use Classification) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=1
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


results.list$`1.용도별 산지 이용 구분_Forest Land Use Classification` = text_data





## 🟧 2.교육 및 훈련 #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=2
L2_categories[i]
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)



### 🟩 텍스트 필터링해서 그룹화 =====================================================================================================
combined.list = list()
combined.list$`임업관련학과 졸업생 현황_Status of Department and Graduates of Forestry` = 
  filter_text_data(text_data, include = "졸업생")
combined.list$`임업관련 학위 취득 현황_Status of Degree on Forestry` =
  filter_text_data(text_data, include = "취득")
combined.list$`임업교육 훈련현황_Status of Forestry Education and Trainig` =
  filter_text_data(text_data, include = "훈련")




### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)
if (length(unfiltered_data) > 0) {
  cat("필터링되지 않은 원소가 존재합니다:\n")
  print(unfiltered_data)
} else {
  cat("모든 원소가 필터링되었습니다.\n")
}


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])






## 🟧 3.국민계정과 생산가격지수 #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=3
L2_categories[i]
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)



### 🟩 텍스트 필터링해서 그룹화 =====================================================================================================
combined.list = list()
combined.list$`직종별 임금_Wages on Occupation` =
  filter_text_data(text_data, include = c("직종별", "임금"))
combined.list$`국민/국내총생산과 임업생산_Gross National Product & Forestry Product` = 
  union(filter_text_data(text_data, include = c("임업", "생산"), exclude = c("생산지수", "어업")),
    filter_text_data(text_data, include = c("국내총", "생산"), exclude = c("경제활동별", "산업별")))
combined.list$`산업별 국민총생산_Gross national product by sector_` = 
  filter_text_data(text_data, include = c("산업별", "생산"))
combined.list$`소비자 물가 지수_Consumer Price Indexes ` = 
  filter_text_data(text_data, include = c("소비자", "물가"))
combined.list$`농가판매 및 구입가격 지수_Price Index of Commodities Received and Paid by Farmers` =
  filter_text_data(text_data, include = c("농가", "판매"))
combined.list$`농촌물가 및 임료금 지수_Index Number of Prices Wages and Charges in Rural Areas`=
  union(filter_text_data(text_data, include = c("농촌", "물가")),
    filter_text_data(text_data, include = c("농촌", "임료금")))
combined.list$`학력 및 성별 임금_Wages by Educational Attainment and Sex`=
  filter_text_data(text_data, include = c("성별", "학력"))
combined.list$`농업 임업 및 어업 총생산_estimated income for agriculture, forestry and fishery`=
  filter_text_data(text_data, include = c("어업", "총"))
combined.list$`농림업 생산지수_ndex number of agricultural and forestry production`=
  filter_text_data(text_data, include = c("농림업", "생산"))
combined.list$`도매물가지수_Index Number of Wholesale Prices`=
  filter_text_data(text_data, include = c("도매", "물가"))
combined.list$`경제활동별 국내 총생산_Gross Domestic Product by Kind of Economic Activity`=
  filter_text_data(text_data, include = c("경제", "별"))
combined.list$`산업별 취업자_Persons Employed by Industry`=
  filter_text_data(text_data, include = c("산업", "별", "취업"))
combined.list$`생산자 물가지수_Producer Price Indexes`=
  filter_text_data(text_data, include = c("생산자", "물가"))
combined.list$`시도별 지가 변동률_Floating Rate of Land Price by Province`=
  union(filter_text_data(text_data, include = c("지가", "변동")),
    filter_text_data(text_data, include = c("지기", "변동")))



### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=10
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])











## 🟧 4.국제산림 통계 (International Forest Statistics) #######################################################################################################################
# extract the data
i=4
L2_categories[i]
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


# 텍스트 클러스터링
k=10
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



# 텍스트 필터링 해서 그룹화
combined.list <- list()

#### 🟦 북한 ==================================================================================
##### 🟪 북한의 산림현황 ===============================================================================
combined.list$`북한의 산림현황_Status of National Land Use of North Korea` =
  union_multiple(filter_text_data(text_data, c("land use", "north", "korea")),
                 filter_text_data(text_data, c("산림", "현황", "북한")))
##### 🟪 임산물 남북교역 현황 ===============================================================================
combined.list$`임산물 남북교역 현황_Forest Product Trade between South and North Korea` =
  union_multiple(filter_text_data(text_data, c("임산물", "남북", "교역")))

#### 🟦 국가별 ==================================================================================
##### 🟪 국가별 토지면적 ===============================================================================
combined.list$`국가별 토지면적_Land area of specifed countries` =
  union_multiple(filter_text_data(text_data, c("국가", "토지", "면적")),
                 filter_text_data(text_data, c("국가", "국토", "면적")))
##### 🟪 국가별 임목축적 ===============================================================================
combined.list$`국가별 임목축적_Forest growing stock country` =
  union_multiple(filter_text_data(text_data, c("국가", "임목", "축적")),
                 filter_text_data(text_data, c("forest", "stock", "growing")))
#### 🟦 산림 ==================================================================================
##### 🟪 산림현황 ===============================================================================
combined.list$`OECD 국가의 산림현황_State of Forest in OECD Countries` =
  union_multiple(filter_text_data(text_data, c("OECD", "산림", "현황")))
##### 🟪 세계 산림자원의 추정 ===============================================================================
combined.list$`세계 산림 자원의 추정_Estimate of World Forest Resources` =
  union_multiple(filter_text_data(text_data, c("estimate", "world", "forest", "resources")),
                 filter_text_data(text_data, c("세계", "산림", "자원", "추정")),
                 filter_text_data(text_data, c("assumption", "world", "forest", "resources")),
                 filter_text_data(text_data, c("세계", "산림자원", "추정")))
##### 🟪 소유별 산림면적 ===============================================================================
combined.list$`소유별 산림면적_Forest land area by ownership` =
  union_multiple(filter_text_data(text_data, c("forest", "land area", "ownership")))

#### 🟦 조림 ==================================================================================
##### 🟪 해외 조림 현황 ===============================================================================
combined.list$`해외조림 현황_Status of Overseas Forest Plantation` =
  union_multiple(filter_text_data(text_data, c("overseas", "forest", "plantation")))
##### 🟪 해외 산림 개발 진출 현황 ===============================================================================
combined.list$`해외산림 개발 진출현황_Status of Overseas Forest Development projects` =
  union_multiple(filter_text_data(text_data, c("overseas", "forest", "development", "project")))
#### 🟦 임산물 ==================================================================================
##### 🟪 주요 임산물 생산 수출 수입 ===============================================================================
combined.list$`국가별 임산물 생산 수출 수입 실적_Accomplishment of Import and Export of Forest Products by Countries` =
  union_multiple(filter_text_data(text_data, c("accomplishment", "import", "export", "forest", "products", "countries")),
                 filter_text_data(text_data, c("임산물", "생산", "수출", "수입", "실적")))
##### 🟪 주요 임산물 생산량 ===============================================================================
combined.list$`국가별 주요 임산물 생산량_Production of major forest Products` =
  union_multiple(filter_text_data(text_data, c("production", "major", "forest products")),
                 filter_text_data(text_data, c("주요", "임산물", "생산")))
##### 🟪 주요 임산물 소비량 ===============================================================================
combined.list$`국가별 주요 임산물 소비량_Consumption of major forest Products` =
  union_multiple(filter_text_data(text_data, c("Consumption", "major", "forest products")),
                 filter_text_data(text_data, c("주요", "임산물", "소비")))
##### 🟪 밤, 호두, 개암  ===============================================================================
combined.list$`주요 국가 밤, 호두, 개암 생산량_Chestnut,Walnut,Hazelnut Production of Major Countries` =
  union_multiple(filter_text_data(text_data, c("chestnut", "walnut", "production")))

#### 🟦 목재 관련  ==================================================================================
##### 🟪 갱목 생산량   ===============================================================================
combined.list$`주요 국가 갱목 생산량_Pitprop Production of Major Countries` =
  union_multiple(filter_text_data(text_data, c("pitprop", "production", "major", "countries")),
                 filter_text_data(text_data, c("주요", "국가", "갱목", "생산량")))
##### 🟪 원목 생산 소비  ===============================================================================
combined.list$`주요국의 원목 생산 및 소비_Roundwood Production and Consumption in Major Countries` =
  union_multiple(filter_text_data(text_data, c("roundwood", "production")))
##### 🟪 산업용재 생산 소비 ===============================================================================
combined.list$`세계 주요국의 산업용재생산 및 소비_Industrial Round-Wood Production and Consumption of Major Countries` =
  union_multiple(filter_text_data(text_data, c("industrial", "round-wood", "production", "consumption", "major", "countries")),
                 filter_text_data(text_data, c("주요", "산업", "용재", "생산", "소비")))
##### 🟪 주요국가 용재 생산 ===============================================================================
combined.list$`주요국가 용재생산량_Round wood Production of Major Countries` =
  union_multiple(filter_text_data(text_data, c("round", "wood", "production", "major", "countries")),
                 filter_text_data(text_data, c("주요", "국가", "용재", "생산량")))
##### 🟪 제재목 생산 소비 ===============================================================================
combined.list$`주요국의 제재목 생산 및 소비_Sawnwood Production and Consumption in Major Countries` =
  union_multiple(filter_text_data(text_data, c("sandwood", "production")),
                 filter_text_data(text_data, c("제재목", "생산")))
##### 🟪 제재 및 합판용재 생산량 ===============================================================================
combined.list$`주요국가별 제재 및 합판용재생산량_Sawlogs and Veneer Logs Production of Major Countries` =
  union_multiple(filter_text_data(text_data, c("sawlogs", "veneer", "logs", "production", "major", "countries")),
                 filter_text_data(text_data, c("주요", "제재", "합판", "용재", "생산량")))
##### 🟪 목탄 생산 소비 ===============================================================================
combined.list$`주요국의 목탄 생산 및 소비_Wood Charcoal Production and Consumption of Major Countries` =
  union_multiple(filter_text_data(text_data, c("chrcoal", "production")),
                 filter_text_data(text_data, c("목탄", "생산")))
##### 🟪 인조판/목질패널 생산 소비 ===============================================================================
combined.list$`주요국의 인조판/목질패널 생산 및 소비_Wood-Based Panels Production And Consumption Of Major Countries` =
  union_multiple(filter_text_data(text_data, c("wood-based", "panel", "production")),
                 filter_text_data(text_data, c("목질", "패널", "소비")))
##### 🟪 인조판/목질패널 수입 수출 ===============================================================================
combined.list$`주요국의 인조판/목질패널 수입 및 수출_Wood-Based Panels Imports And Exports Of Major Countries` =
  union_multiple(filter_text_data(text_data, c("wood-based", "panel", "import")),
                 filter_text_data(text_data, c("목질", "패널", "수입")))
##### 🟪 합판 수출 ===============================================================================
combined.list$`세계 주요국의 합판 수출량_Plywood Exports by Major Countries` =
  filter_text_data(text_data, c("plywood", "export", "major"))
##### 🟪 합판 생산 ===============================================================================
combined.list$`세계 주요국가 합판생산량_Plywood Production of Major Countries` =
  union_multiple(filter_text_data(text_data, 
                                  c("plywood", "production", "major", "countries"),
                                  "용재"),
                 filter_text_data(text_data, 
                                  c("주요", "합판", "생산량"),
                                  "용재"))
##### 🟪 합판 수입량 ===============================================================================
combined.list$`세계 주요국의 합판수입량_Plywood Imports by Major Countries` =
  union_multiple(filter_text_data(text_data, c("plywood", "imports", "major", "countries")),
                 filter_text_data(text_data, c("세계", "주요", "합판", "수입량")))
##### 🟪 단판 생산 소비 ===============================================================================
combined.list$`주요국의 단판 생산 및 소비_Veneer Sheets Production and Consumption of Major Countries` =
  union_multiple(filter_text_data(text_data, c("veneer", "sheets", "production", "consumption", "major", "countries")),
                 filter_text_data(text_data, c("단판", "생산", "소비")))
##### 🟪 단판 수입 ===============================================================================
combined.list$`세계 주요국의 단판수입량_Veneer Sheets Imports by Major Contries` =
  union_multiple(filter_text_data(text_data, c("veneer", "sheet", "import")),
                 filter_text_data(text_data, c("단판", "수입")))
##### 🟪 단판 수출 ===============================================================================
combined.list$`주요 국가별 단판 수출량_Exports of Veneer Sheets by Major Countries` =
  union_multiple(filter_text_data(text_data, c("veneer", "sheet", "export")),
                 filter_text_data(text_data, c("단판", "수출")))
##### 🟪 연료 생산 소비 ===============================================================================
combined.list$`주요국의 연료 생산 및 소비_Fuel wood production and consumption of major countries` =
  union_multiple(filter_text_data(text_data, c("fuel", "wood", "production")),
                 filter_text_data(text_data, c("연료", "생산")))
##### 🟪 펄프 용재 생산량  ===============================================================================
combined.list$`주요 국가별 펄프용재 생산량_Pulpwood and Particles Production of Major Countries` =
  union_multiple(filter_text_data(text_data, c("pulpwood", "particles", "production")),
                 filter_text_data(text_data, c("펄프", "용재", "생산")))
##### 🟪 펄프 생산량  ===============================================================================
combined.list$`주요 국가 펄프 생산량_Pulp and Particle Production of Major Countries` =
  union_multiple(filter_text_data(text_data, c("pulp", "particle", "production", "major", "countries")),
                 filter_text_data(text_data, c("국가", "펄프", "생산량")))
##### 🟪 펄프 및 지류 생산 수출 수입  ===============================================================================
combined.list$`국가별 펄프 및 지류의 생산 수출수입 실적_Accomplishment of production import and export of pulp and paper by countries` =
  union_multiple(filter_text_data(text_data, c("import", "pulp", "paper")),
                 filter_text_data(text_data, c("펄프", "지류", "수입")))
##### 🟪 주요 목재 가공품 생산량  ===============================================================================
combined.list$`주요목재 및 가공품 생산량_Production of Principal Timber & Processed Wood` =
  union_multiple(filter_text_data(text_data, c("production", "principal", "timber", "processed", "wood")),
                 filter_text_data(text_data, c("주요", "목재", "가공품", "생산량")))


#### 🟦 인구  ==================================================================================
combined.list$`세계 주요국가 인구_Population in specified countries` =
  filter_text_data(text_data, c("세계", "국가", "인구"))
#### 🟦 물가지수 및 취업지수  ==================================================================================
combined.list$`물가지수_Price Index Number 및 취업지수_Index Number of Employment` =
  union_multiple(filter_text_data(text_data, c("price", "index", "number", "employment")),
                 filter_text_data(text_data, c("물가지수", "취업지수")),
                 filter_text_data(text_data, c("물가지수")),
                 filter_text_data(text_data, c("취업지수")))









### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
unfiltered_data <- setdiff(text_data, unlist(combined.list))


k=10
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)

### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])







## 🟧 5.국토와 자연환경 (Land & Natural Environment) #######################################################################################################################
# extract the data
i=5
L2_categories[i]
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


# 텍스트 클러스터링
k=20
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



# 텍스트 필터링 해서 그룹화
combined.list <- list()
combined.list$`대한민국의 위치_Location of Repubic of Korea` =
  union_multiple(filter_text_data(text_data, c("location", "korea")),
                 filter_text_data(text_data, c("대한민국", "위치")))
combined.list$`국토이용 상황_Status of national land utilization` =
  union_multiple(filter_text_data(text_data, c("national", "land", "utilization")),
                 filter_text_data(text_data, c("국토", "이용")))
combined.list$`측후소일람표_List of meteorological stations` = 
  union_multiple(filter_text_data(text_data, c("meteorological", "station")),
                 filter_text_data(text_data, c("측후소")))
combined.list$`시도별 면적 및 행정단위_Area and administrative unit by province and cities` = 
  union_multiple(filter_text_data(text_data, c("area", "administrative", "unit")),
                 filter_text_data(text_data, c("면적", "행정", "단위")),
                 filter_text_data(text_data, c("면적", "행정구역")))
combined.list$`강수량표_Precipitation` = 
  union_multiple(filter_text_data(text_data, c("precipitation")),
                 filter_text_data(text_data, c("강수량")))
combined.list$`일조 시간_Hours of Sunshine`=
  union_multiple(filter_text_data(text_data, c("sunshine")),
                 filter_text_data(text_data, c("일조", "시간")))
combined.list$`인구변동 추이_Population Trend`=
  union_multiple(filter_text_data(text_data, c("population", "trend")),
                 filter_text_data(text_data, c("인구", "추이")),
                 filter_text_data(text_data, c("인구", "추세")),
                 filter_text_data(text_data, c("인구", "추계")))
combined.list$`상대습도_Relative humidity`=
  union_multiple(filter_text_data(text_data, c("relative", "humidity")),
                 filter_text_data(text_data, c("상대", "습도")))
combined.list$`평균 기온_Average Temperature`=
  union_multiple(filter_text_data(text_data, c("average", "temper")),
                 filter_text_data(text_data, c("평균", "기온")))
combined.list$`지상기상관측지점_List of the Surface Synoptic Stations`=
  union_multiple(filter_text_data(text_data, c("surface", "synoptic", "station")),
                 filter_text_data(text_data, c("지상", "기상", "관측")))


# 필터링되지 않은 원소 확인
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)
if (length(unfiltered_data) > 0) {
  cat("필터링되지 않은 원소가 존재합니다:\n")
  print(unfiltered_data)
} else {
  cat("모든 원소가 필터링되었습니다.\n")
}

### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)

# save the results
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])






## 🟧 6.부록(Appendix) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=6
L2_categories[i]
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=15
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()

combined.list$`임업기계ㆍ장비 보유현황_Forest Machinery and Equipments` =
  union_multiple(filter_text_data(text_data, c("machinery", "equipment")),
                 filter_text_data(text_data, c("기계", "장비")))

combined.list$`도량형환산표_Conversion table of weight and measures` =
  union_multiple(filter_text_data(text_data, c("conversion", "table", "measure")),
                 filter_text_data(text_data, c("도량형", "환산표")))

combined.list$`계량 단위 환산표_Conversion Factor for Weight and Measure` = 
  union_multiple(filter_text_data(text_data, c("conversion", "factor", "weight", "measure")),
                 filter_text_data(text_data, c("계량", "단위")))

combined.list$`목재단위환산표_Conversion Factors for Timber Measurement` = 
  union_multiple(filter_text_data(text_data, c("timber", "measurement", "factor")),
                 filter_text_data(text_data, c("목재", "단위", "환산")))

combined.list$`외국의 원목측정법_Measure method of roundwood` = 
  union_multiple(filter_text_data(text_data, c("roundwood", "method")),
                 filter_text_data(text_data, c("외국", "원목")))


combined.list$`우리 나라 명산 현황_Status of Renowned Mountains` =
  union_multiple(
    filter_text_data(text_data, c("renowned", "mountain")),
    filter_text_data(text_data, c("명산", "우리나라"))
  )

combined.list$`산림관계세입예산_Forest Estimated Revenue` =
  union_multiple(filter_text_data(text_data, c("forest", "revenue")),
                 filter_text_data(text_data, c("산림", "관계", "세입")))

combined.list$`산림임업교육 훈련현황` =
  union_multiple(filter_text_data(text_data, c("임업", "훈련")),
                 filter_text_data(text_data, c("교육", "훈련")))

combined.list$`국립 및 도립공원 현황_Status of National and Provincial Parks`=
  union_multiple(filter_text_data(text_data, c("national", "park")),
                 filter_text_data(text_data, c("국립", "공원")),
                 filter_text_data(text_data, c("도립", "공원")))

combined.list$`산림관계단체현황_Status of Private Forestry Organizations` =
  union_multiple(filter_text_data(text_data, c("private", "forestry", "organization")),
                 filter_text_data(text_data, c("산림", "관계", "단체")))

combined.list$`산림관계 공무원 정원수_Number of Forest Government Employees` =
  union_multiple(filter_text_data(text_data, c("number", "forest", "employee")),
                 filter_text_data(text_data, c("산림", "공무원", "정원")))

combined.list$`산림조합 임직원상황_Staffing status of Forestry Association Union` =
  union_multiple(filter_text_data(text_data, c("staff", "forestry", "union")),
                 filter_text_data(text_data, c("산림", "조합", "임직원")),
                 filter_text_data(text_data, c("임업", "조합", "임직원")))

combined.list$`멸종위기 및 보호야생 동·식물 지정현황_Status of designated endangered and protected wildlife species` =
  union_multiple(filter_text_data(text_data, c("designated", "endangered", "wildlife")),
                 filter_text_data(text_data, c("멸종", "위기", "지정")),
                 filter_text_data(text_data, c("희귀", "특별", "보호", "지정")))

combined.list$`산림사업 융자실적_Loans on forestry activities` =
  union_multiple(filter_text_data(text_data, c("loans", "forestry", "activities")),
                 filter_text_data(text_data, c("산림", "융자", "실적")))


combined.list$`백두대간보호지역 행정구역 현황_Protected Areas of Baekdu Daegan Mountains` =
  union_multiple(filter_text_data(text_data, c("protected", "areas", "Baekdu")),
                 filter_text_data(text_data, c("백두", "대간", "보호", "구역")))

combined.list$`산림청 기구_Organization chart of F.A` =
  union_multiple(filter_text_data(text_data, c("organization", "chart")),
                 filter_text_data(text_data, c("산림청", "기구")))


combined.list$`산림사업용 묘목규격_Standards of seedling for forest activities` =
  union_multiple(filter_text_data(text_data, c("standard", "seedling")),
                 filter_text_data(text_data, c("산림", "묘목", "규격")))


combined.list$`임업관련 학과 및 졸업생 현황_Status of Forest Schools and Number of Graduates` =
  union_multiple(filter_text_data(text_data, c("status", "forest", "school", "number", "graduate")),
                 filter_text_data(text_data, c("졸업", "학과", "임업")))


combined.list$`산림관계 세출예산_Forest Estimated Expenditures` =
  union_multiple(filter_text_data(text_data, c("estimated", "expenditure")),
                 filter_text_data(text_data, c("세출", "산림", "예산")))


combined.list$`산림조합중앙회 및 산림조합 정원현황_Employees of national forestry cooperatives federation` =
  union_multiple(filter_text_data(text_data, c("employee", "forestry", "cooperative", "foundation")),
                 filter_text_data(text_data, c("산림", "조합", "정원")))

combined.list$`환율표_Exchange rates` =
  union_multiple(filter_text_data(text_data, c("exchange", "rate")),
                 filter_text_data(text_data, c("환율표")))

combined.list$`산림계 조직상황_Status of Village Forest Association` =
  union_multiple(filter_text_data(text_data, c("status", "forest", "association")),
                 filter_text_data(text_data, c("산림", "조직", "상황")))

combined.list$`연도별 건축허가 실적_Building Construction Permits by Year` =
  union_multiple(filter_text_data(text_data, c("Building", "construction", "permit")),
                 filter_text_data(text_data, c("연도", "건축", "허가")))


combined.list$`산림관계 공무원 교육실적_Forestry officials training` =
  union_multiple(filter_text_data(text_data, c("forestry", "official", "training")),
                 filter_text_data(text_data, c("산림", "공무원", "교육")))


### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=10
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])










## 🟧 7.사유림 (Private Forest) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=7
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=3
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()

combined.list$`사유림 소유규모별 산주현황_Private Forest Owners by Size` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owner", "size")),
                 filter_text_data(text_data, c("사유림", "산주", "규모")))

combined.list$`사유림 소유형태별 산주현황_Private Forest Owners By Ownership` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owner", "ownership", "산주현황")),
                 filter_text_data(text_data, c("사유림", "산주", "형태")))

combined.list$`사유림 소유형태별 산림면적_Private Forest Land Area by Ownership` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "ownership", "land")),
                 filter_text_data(text_data, c("사유림", "소유", "형태", "산림")))


combined.list$`사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province` =
  union_multiple(filter_text_data(text_data, c("resident", "absolute", "forest", "owner", "province")),
                 filter_text_data(text_data, c("사유림", "부재", "산주")))

combined.list$`사유림 소유형태별 필지수 현황_Private Forest Lots By Ownership` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "ownership", "lots")),
                 filter_text_data(text_data, c("사유림", "형태", "필지수")))

combined.list$`전국_Country` =
  union_multiple(filter_text_data(text_data, c("전국", "country"), c("사유림")),
                 filter_text_data(text_data, c("전국", "nation"), c("사유림")))




### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=1
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)

### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])






## 🟧 8.임산 무역 및 가공 유통 (Trade and Processing of Forest Products) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=8
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=20
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters %>% sapply(., head)




### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()


combined.list$`해외 산림개발 진출현황_Oversea Forest Development` =
  union_multiple(filter_text_data(text_data, c("oversea", "forest", "development")),
                 filter_text_data(text_data, c("해외", "산림개발", "진출", "현황")))

combined.list$`주요 임산물 수출실적_Exports of Major Forest Products` =
  union_multiple(filter_text_data(text_data, c("exports", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "수출", "실적")))

combined.list$`주요 임산물 수입실적_Imports of Major Forest Products` =
  union_multiple(filter_text_data(text_data, c("imports", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "수입", "실적")))

combined.list$`주요화물별 철도수송량_Railway Freight Transportation by Commodity` =
  union_multiple(filter_text_data(text_data, c("railway", "freight", "transportation", "commodity")),
                 filter_text_data(text_data, c("주요", "화물", "철도", "수송", "량")))

combined.list$`주요화물별 선박수송량_Marine Transportation by Commodity` =
  union_multiple(filter_text_data(text_data, c("marine", "transportation", "commodity")),
                 filter_text_data(text_data, c("주요", "화물", "선박", "수송", "량")))




### 🟦 외재 도입액 ============================================================================================
#### 🟪 지역별 ============================================================================================
combined.list$`지역별 외재도입액_Value of Imported Timber by Countries` =
  union_multiple(filter_text_data(text_data, c("value", "imported", "timber", "countries")),
                 filter_text_data(text_data, c("지역별", "외재", "도입", "액")))
#### 🟪 용도별 ============================================================================================
combined.list$`용도별 외재도입액_Value of Imported Timber by Use` =
  union_multiple(filter_text_data(text_data, c("value", "imported", "timber", "use")),
                 filter_text_data(text_data, c("용도별", "외재", "도입", "액")))

### 🟦 외재 실적 ============================================================================================
#### 🟪 지역별 ============================================================================================
combined.list$`지역별 외재도입실적_Timber Import by Source` =
  union_multiple(filter_text_data(text_data, 
                                  c("timber", "import", "source"),
                                  "용도별"),
                 filter_text_data(text_data, 
                                  c("지역별", "외재", "도입", "실적"), 
                                  "용도별"))
#### 🟪 용도별 ============================================================================================
combined.list$`용도별 외재도입실적_Timber Import by Use` =
  union_multiple(filter_text_data(text_data, c("timber", "import", "use"), c("value")),
                 filter_text_data(text_data, c("용도별", "외재", "도입", "실적"), c("value")))
#### 🟪 산지별 ============================================================================================
combined.list$`산지별 외재도입실적_Timber Imports by Origin` =
  union_multiple(filter_text_data(text_data, c("timber", "imports", "origin")),
                 filter_text_data(text_data, c("산지별", "외재", "도입", "실적")))


### 🟦 제재 공장 ============================================================================================
combined.list$`제재공장 실태_Status of Sawmills` =
  union_multiple(filter_text_data(text_data, c("status", "sawmills")),
                 filter_text_data(text_data, c("제재", "공장", "실태")))

### 🟦 포플러 제품 생산 ============================================================================================
combined.list$`포플러 제품생산 및 공급실적_Production and Supply of Popular Products` =
  union_multiple(filter_text_data(text_data, c("production", "supply", "popular", "products")),
                 filter_text_data(text_data, c("production", "suppy", "popular", "products")),
                 filter_text_data(text_data, c("포플러", "제품", "생산", "공급", "실적")),
                 filter_text_data(text_data, c("포플라", "제품", "생산", "공급", "실적")))


### 🟦 목재 가공품 ============================================================================================
combined.list$`나무 및 나무제품 제조업_Manufacture of Wood and Wood Products` =
  union_multiple(filter_text_data(text_data, c("manufacture", "wood", "wood", "products")),
                 filter_text_data(text_data, c("나무", "나무제품", "제조업")))
combined.list$`목재가공품 생산 및 공급_Production and Supply of Processed Wood` =
  union_multiple(filter_text_data(text_data, c("production", "supply", "processed", "wood")),
                 filter_text_data(text_data, c("목재가공품", "생산", "공급")))
combined.list$`목재 및 목제품 제조업 실태_Status of Wood and Wood Products Manufacture` =
  union_multiple(filter_text_data(text_data, c("status", "wood", "wood", "products", "manufacture")),
                 filter_text_data(text_data, c("목재", "목제품", "제조업", "실태")))



### 🟦 생산 실적 ============================================================================================
#### 🟪 지류 ============================================================================================
combined.list$`지류생산실적_Paper Production` =
  union_multiple(filter_text_data(text_data, c("paper", "production")),
                 filter_text_data(text_data, c("지류", "생산", "실적")))

#### 🟪 칩 ============================================================================================
combined.list$`칩 생산실적_Chip Production` =
  union_multiple(filter_text_data(text_data, c("chip", "production")),
                 filter_text_data(text_data, c("칩", "생산", "실적")))

#### 🟪 펄프 ============================================================================================
combined.list$`펄프 생산실적_Pulp Production` =
  union_multiple(filter_text_data(text_data, c("pulp", "production")),
                 filter_text_data(text_data, c("펄프", "생산", "실적")),
                 filter_text_data(text_data, c("팔프", "생산", "실적")))

#### 🟪 종이 ============================================================================================
combined.list$`종이 및 종이제품 제조업_Manufacture of Paper and Paper Products` =
  union_multiple(filter_text_data(text_data, c("manufacture", "paper", "paper", "products")),
                 filter_text_data(text_data, c("종이", "종이제품", "제조업")))



#
combined.list$`임산물 남북교역 현황_Trade of Forest Products between South and North Korea` =
  union_multiple(filter_text_data(text_data, c("trade", "forest", "products", "South", "North", "Korea")),
                 filter_text_data(text_data, c("임산물", "남북교역", "현황")))

# 가격
combined.list$`연도별 목재가격_Lumber & Wood Price by Year` =
  union_multiple(filter_text_data(text_data, c("lumber", "wood", "price", "year")),
                 filter_text_data(text_data, c("연도별", "목재", "가격")))

combined.list$`주요 임산물 가격_Prices of Major Forest Products` =
  union_multiple(filter_text_data(text_data, c("prices", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "가격")))



#
combined.list$`임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported` =
  union_multiple(filter_text_data(text_data, c("forest", "products", "marketing", "facilities", "low", "temperature", "storage", "houses", "financially", "supported")),
                 filter_text_data(text_data, c("임산물", "유통시설", "저온저장고", "지원", "현황")))



### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=10
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])







## 🟧 9.임산물 가격 및 기타 가격 (Price of Forest Products & Major Commodities) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=9
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=5
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters %>% sapply(., head)




### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()


combined.list$`가계용품 농가구입가격_Price of Household Goods Paid by Farmers` =
  union_multiple(filter_text_data(text_data, c("price", "household", "goods", "paid", "farmers")),
                 filter_text_data(text_data, c("가계용품", "농가", "구입", "가격")))

combined.list$`주요상품 도매가격_Wholesale Prices of Major Commodities` =
  union_multiple(filter_text_data(text_data, c("wholesale", "prices", "major", "commodities")),
                 filter_text_data(text_data, c("주요상품", "도매", "가격")))

combined.list$`묘목가격표_Price List of Seedlings` =
  union_multiple(filter_text_data(text_data, c("price", "list", "seedlings")),
                 filter_text_data(text_data, c("묘목", "가격표")))

combined.list$`제조업 생산종업원의 월당 급여액 및 출근 일수_Monthly Earnings and Man-Days of Production Workers in Manufacturing` =
  union_multiple(filter_text_data(text_data, c("monthly", "earnings", "man-days", "production", "workers", "manufacturing")),
                 filter_text_data(text_data, c("제조업", "생산종업원", "월당", "급여액", "출근", "일수")))

combined.list$`종자가격표_Price List of Seed` =
  union_multiple(filter_text_data(text_data, c("price", "list", "seed"), "seedling"),
                 filter_text_data(text_data, c("종자", "가격표")))

combined.list$`묘목가격표_Price List of Seedling` =
  union_multiple(filter_text_data(text_data, c("price", "list", "seedling")),
                 filter_text_data(text_data, c("묘목", "가격표")))





### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=2
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])









## 🟧 10.임산물 시장 (Forest Product Market) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=10
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=20
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters %>% sapply(., function(x){head(x,3)})




### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()


combined.list$`임산물 가격 동향_Prices of Major Forest Products` =
  union_multiple(filter_text_data(text_data, c("prices", "major", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "가격", "동향")))

combined.list$`국내총생산과 임업_Gross Domestic Product and Forestry` =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "forestry")),
                 filter_text_data(text_data, c("국내", "총생산","임업")))

combined.list$`국내 총 생산과 임산물생산_Gross Domestic Product and Forest Products` =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "forest", "products")),
                 filter_text_data(text_data, c("국내", "총", "생산", "임산물", "생산")))



# 
combined.list$`생산자 물가지수_Producer price index` =
  union_multiple(filter_text_data(text_data, c("producer", "price", "index")),
                 filter_text_data(text_data, c("물가", "지수", "생산자")))
combined.list$`소비자, 수출, 수입 물가지수_Consumer, exportation, importaton price index` =
  union_multiple(filter_text_data(text_data, c("consumer", "price", "index")),
                 filter_text_data(text_data, c("importation", "price", "index")),
                 filter_text_data(text_data, c("exportation", "price", "index")),
                 filter_text_data(text_data, c("물가", "지수","소비자")),
                 filter_text_data(text_data, c("물가", "지수","수출")),
                 filter_text_data(text_data, c("물가", "지수","수입")))



# 
combined.list$`용도별 국내재 공급실적_Domestic Timber Supply by Uses` =
  union_multiple(filter_text_data(text_data, c("domestic", "timber", "supply", "uses")),
                 filter_text_data(text_data, c("용도별", "국내재", "공급", "실적")))
combined.list$`입목벌채 허가실적_Permission on Annual Timber Cutting` =
  union_multiple(filter_text_data(text_data, c("permission", "annual", "timber", "cutting")),
                 filter_text_data(text_data, c("입목벌채", "허가", "실적")))
combined.list$`수목굴취 허가(신고) 실적_Permission(Reporting) of Tree Transplanting` =
  union_multiple(filter_text_data(text_data, c("permission", "reporting", "tree", "transplanting")),
                 filter_text_data(text_data, c("수목굴취", "허가", "신고", "실적")))


#
combined.list$`칩 생산실적_Chip Production` =
  union_multiple(filter_text_data(text_data, c("chip", "production")),
                 filter_text_data(text_data, c("칩", "생산", "실적")))
combined.list$`지류 생산실적_Paper Production` =
  union_multiple(filter_text_data(text_data, c("paper", "production")),
                 filter_text_data(text_data, c("지류", "생산", "실적")))
combined.list$`펄프 생산실적_Pulp Production` =
  union_multiple(filter_text_data(text_data, c("pulp", "production")),
                 filter_text_data(text_data, c("펄프", "생산", "실적")))


# 
combined.list$`목재펠릿 생산 실적_Wood Pellet Production` =
  union_multiple(filter_text_data(text_data, c("wood", "pellet", "production")),
                 filter_text_data(text_data, c("목재펠릿", "생산", "실적")))
combined.list$`목질패널 생산 및 공급_Production and Supply of Processed Wood-based Panel` =
  union_multiple(filter_text_data(text_data, c("production", "supply", "processed", "wood-based", "panel")),
                 filter_text_data(text_data, c("목질패널", "생산", "공급")))
combined.list$`제재목 생산실적_Production and Supply of Sawnwood` =
  union_multiple(filter_text_data(text_data, c("production", "supply", "sawnwood")),
                 filter_text_data(text_data, c("제재목", "생산", "실적")))
combined.list$`관상수 생산실적_Ornamental Tree Production` =
  union_multiple(filter_text_data(text_data, c("ornamental", "tree", "production")),
                 filter_text_data(text_data, c("관상수", "생산", "실적")))
combined.list$`원목 수급실적_Timber Demand and Supply` =
  union_multiple(filter_text_data(text_data, c("timber", "demand", "supply")),
                 filter_text_data(text_data, c("원목", "수급", "실적")))
combined.list$`국유림 목재 매각 실적_National Forest Wood Sales Results` =
  union_multiple(filter_text_data(text_data, c("national", "forest", "wood", "sales", "results")),
                 filter_text_data(text_data, c("국유림", "목재", "매각", "실적")))




# 산림
combined.list$`산림벌채 면적 및 벌채량_Area and Volume of Annual Cut` =
  union_multiple(filter_text_data(text_data, c("area", "volume", "annual", "cut")),
                 filter_text_data(text_data, c("산림벌채", "면적", "벌채량")))


# 임산물
combined.list$`임산물 수출실적_Export of Forest Products` =
  union_multiple(filter_text_data(text_data, c("export", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "수출", "실적")))

combined.list$`임산물 수입실적_Import of Forest Products` =
  union_multiple(filter_text_data(text_data, c("import", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "수입", "실적")))

combined.list$`임산물 생산실적_Production of Forest Products` =
  union_multiple(filter_text_data(text_data, c("production", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "생산", "실적")))





### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])



## 🟧 11.산림 서비스 (Forest Service) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=11
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=20
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()

# 산림 복지
combined.list$`산림복지전문업 등록현황_Status of Job Startups on Forest welfare` =
  union_multiple(filter_text_data(text_data, c("job", "startup", "forest", "welfare")),
                 filter_text_data(text_data, c("산림", "복지", "전문")))


# 교육
combined.list$`숲사랑 소년단 육성현황_The Number of Green Rangers` =
  union_multiple(filter_text_data(text_data, c("number", "green", "ranger")),
                 filter_text_data(text_data, c("숲사랑", "소년")))


combined.list$`산림교육전문가 양성기관 현황_Status of Forest Guide Training Organizations` =
  union_multiple(filter_text_data(text_data, c("forest", "guide", "training")),
                 filter_text_data(text_data, c("산림", "교육", "전문가")))

combined.list$`산림교육 수혜인원 현황_The Number of Forest Education Recipients` =
  union_multiple(filter_text_data(text_data, c("number", "forest", "education", "recipient")),
                 filter_text_data(text_data, c("산림", "교육", "수혜")))

combined.list$`숲 해설가 수혜인원 현황_Status of Beneficiaries of Forest Guide` =
  union_multiple(filter_text_data(text_data, c("beneficiaries", "forest", "guide")),
                 filter_text_data(text_data, c("해설가", "수혜", "인원")))



# 
combined.list$`수목원, 산림박물관, 자생식물원 등록 현황_Establishment of Arboretums, Forest Museum, and Botanical Garden` =
  union_multiple(filter_text_data(text_data, c("establishment", "arboretum", "forest", "museum", "botanical")),
                 filter_text_data(text_data, c("수목원", "박물관")))


# 숲 운영
combined.list$`치유의 숲 운영 현황_Management of Healing Forest` =
  union_multiple(filter_text_data(text_data, c("healing", "management", "forest")),
                 filter_text_data(text_data, c("치유", "숲", "운영")))

combined.list$`휴양림 운영 및 이용현황_Number of Visitors to Recreation Forests` =
  union_multiple(filter_text_data(text_data, c("number", "recreation", "forest")),
                 filter_text_data(text_data, c("휴양림", "운영")))

# 숲 조성
combined.list$`산림욕장 조성현황_Establishment of forest bathing facilities` =
  union_multiple(filter_text_data(text_data, c("establishment", "forest", "bathing", "facilities")),
                 filter_text_data(text_data, c("산림", "욕장", "조성")))

combined.list$`명상숲 조성현황_The Number of Meditation Forests` =
  union_multiple(filter_text_data(text_data, c("number", "Meditation", "forest")),
                 filter_text_data(text_data, c("명상", "숲", "조성")))

combined.list$`학교 숲 조성현황_Establishment of Forests within Schools` =
  union_multiple(filter_text_data(text_data, c("establishment", "school", "forest")),
                 filter_text_data(text_data, c("학교", "숲", "조성")))

combined.list$`도시숲 조성현황_Establishment of Urban Forests` =
  union_multiple(filter_text_data(text_data, c("establishment", "urban", "forest")),
                 filter_text_data(text_data, c("도시숲", "조성")))

combined.list$`휴양림 조성현황_Establishment of Recreation Forests` =
  union_multiple(filter_text_data(text_data, c("establishment", "recreation", "forest")),
                 filter_text_data(text_data, c("휴양림", "조성")))

combined.list$`정원 조성 및 운영현황_Create Garden and Manage Garden` =
  union_multiple(filter_text_data(text_data, c("create garden", "manage garden")),
                 filter_text_data(text_data, c("정원 조성")))

# 전통 마을
combined.list$`전통마을 숲 조성현황_Establishment of Traditional Village Forests` =
  union_multiple(filter_text_data(text_data, c("establishment", "traditional", "forest")),
                 filter_text_data(text_data, c("전통", "마을", "숲", "조성")))

combined.list$`전통마을 숲 복원현황_Restoration of Traditional Village Forests` =
  union_multiple(filter_text_data(text_data, c("restoration", "traditional", "forest")),
                 filter_text_data(text_data, c("전통", "마을", "숲", "복원")))


# 가로수
combined.list$`가로수 심기현황_Plantation of Roadside Trees` =
  union_multiple(filter_text_data(text_data, c("roadside", "tree", "plantation")),
                 filter_text_data(text_data, c("가로수", "심기")))


# 
combined.list$`산림복지서비스 제공자 등록 현황_Enrollment of Forest Welfare Service Voucher-available-facilities` =
  union_multiple(filter_text_data(text_data, c("enrollment", "forest", "welfare", "service")),
                 filter_text_data(text_data, c("산림", "복지", "서비스", "제공")))




### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=10
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)



### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])









## 🟧 12.산림자원조성 (Silviculture) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=12
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=20
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()


# 휴양림
combined.list$`휴양림 이용현황_Visitors to Recreational Forest` =
  union_multiple(filter_text_data(text_data, c("visitors", "recreational", "forest")),
                 filter_text_data(text_data, c("휴양림", "이용", "현황")))

combined.list$`휴양림 조성현황_Establishment of Recreational Forest` =
  union_multiple(filter_text_data(text_data, c("establishment", "recreational", "forest")),
                 filter_text_data(text_data, c("휴양림", "조성", "현황")))


# 
combined.list$`산림 형질변경 허가 상황_Permission for Conversion of Forest to Other Uses` =
  union_multiple(filter_text_data(text_data, c("permission", "conversion", "forest", "other", "uses")),
                 filter_text_data(text_data, c("산림", "형질변경", "허가", "상황")))

combined.list$`임도시설 현황_Construction of Forest Road` =
  union_multiple(filter_text_data(text_data, c("construction", "forest", "road")),
                 filter_text_data(text_data, c("임도시설", "현황")))



# 숲가꾸기
combined.list$`육림/숲가꾸기_Forest Tending` =
  union_multiple(filter_text_data(text_data, c("forest", "tending")),
                 filter_text_data(text_data, c("육림")),
                 filter_text_data(text_data, c("숲", "가꾸기")))

combined.list$`가로수 심기 현황_Planting of Roadside Tree` =
  union_multiple(filter_text_data(text_data, c("planting", "roadside", "tree")),
                 filter_text_data(text_data, c("가로수", "심기", "현황")))


# 조림지
combined.list$`인공 조림지 현황_Area of Plantation Forests` =
  union_multiple(filter_text_data(text_data, c("area", "plantation", "forests")),
                 filter_text_data(text_data, c("인공", "조림지", "현황")))

# 
combined.list$`조림 활착상황_Survival Rate of Reforestation` =
  union_multiple(filter_text_data(text_data, c("survival", "rate", "reforestation")),
                 filter_text_data(text_data, c("조림", "활착", "상황")))


# 조림 실적 
combined.list$`수종별 조림실적_Plantation by Tree Species` =
  union_multiple(filter_text_data(text_data, c("plantation", "tree", "species")),
                 filter_text_data(text_data, c("수종별", "조림", "실적")))

combined.list$`재원별 조림실적_Plantation by Fund` =
  union_multiple(filter_text_data(text_data, c("plantation", "fund")),
                 filter_text_data(text_data, c("재원별", "조림", "실적")))

combined.list$`소유별 조림실적_Plantation Area by Ownership` =
  union_multiple(filter_text_data(text_data, c("plantation", "area", "ownership")),
                 filter_text_data(text_data, c("소유별", "조림", "실적")),
                 filter_text_data(text_data, c("소유별", "조림")))


# 
combined.list$`양묘 시업 상황_Operation of Tree Seeding Production` =
  union_multiple(filter_text_data(text_data, c("operation", "tree", "seeding", "production")),
                 filter_text_data(text_data, c("양묘", "시업", "상황")))


combined.list$`종자 및 묘목 생산현황_Status of Seed and Seedling Production` =
  union_multiple(filter_text_data(text_data, c("status", "seed", "seedling", "production")),
                 filter_text_data(text_data, c("종자", "묘목", "생산", "현황")),
                 filter_text_data(text_data, c("seedling", "production", "tree", "species")),
                 filter_text_data(text_data, c("수종별", "묘목", "생산", "현황")),
                 filter_text_data(text_data, c("seed", "production", "tree", "species")),
                 filter_text_data(text_data, c("수종별", "종자", "생산", "현황")))


combined.list$`산지이용구분현황_Classification of Forest Land Utilization` =
  union_multiple(filter_text_data(text_data, c("classification", "forest", "land", "utilization")),
                 filter_text_data(text_data, c("산지", "이용", "구분", "현황")))


combined.list$`독림가 및 임업후계자 현황_Devoted Forest Managers and Forest Successors` =
  union_multiple(filter_text_data(text_data, c("devoted", "forest", "managers", "forest", "successors")),
                 filter_text_data(text_data, c("독림가", "임업", "후계자", "현황")))

combined.list$`사유림 협업경영사업_Activities of Private Forest Cooperatives___관리기관별_Management offices` =
  union_multiple(filter_text_data(text_data, c("activities", "private", "forest", "cooperatives")),
                 filter_text_data(text_data, c("사유림", "협업", "경영")))







### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=10
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])




## 🟧 13.임야면적 및 임목축적 (Forest Land Area & Growing Stock) #######################################################################################################################
### 🟩 extract the data ######################################################################################
i=13
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=30
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters %>% sapply(., function(x){head(x,3)})




### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()

# 보전 임지 
combined.list$`보전ㆍ준보전임지 지정현황_Area of Reserve and Semi-Reserve Forest` =
  union_multiple(filter_text_data(text_data, c("area", "reserve", "semi-reserve", "forest")),
                 filter_text_data(text_data, c("보전", "준보전임지", "지정", "현황")))





#### 🟦 관리기관별 ===================================================================================================
##### 🟪 관리기관별, 영급별 산림면적ㆍ임목축적 ===========================================================================================================
combined.list$`관리기관별, 영급별 산림면적ㆍ임목축적_Forest Land Area and Growing Stock by Management Agencies and Age Class` =
  union_multiple(filter_text_data(text_data,
                                  c("forest land area", "growing stock", "management agencies", "age class"),
                                  "지종별"),
                 filter_text_data(text_data, 
                                  c("관리기관별", "영급별", "산림", "면적", "임목", "축적"), 
                                  "지종별"))
##### 🟪 관리기관별, 임상별 산림면적 임목축적 ===========================================================================================================
combined.list$`관리기관별, 임상별 산림면적 임목축적_Forest Land Area and Growing Stock  by Management Authorities and Forest Type` =
  union_multiple(filter_text_data(text_data, c("forest", "land area", "management", "authorities", "Growing Stock", "forest type")),
                 filter_text_data(text_data, c("관리기관별", "임상별", "산림", "면적", "임목", "축적")),
                 filter_text_data(text_data, c("관리기관별", "임상별", "산림", "면적")),
                 filter_text_data(text_data, c("관리기관별", "임상별", "임목", "축적")))





#### 🟦 소관별 지종별 임야면적 및 임목축적 ===================================================================================================
##### 🟪 -행정구역별 =================================================================================================================================
combined.list$`소관별ㆍ지종별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Authorities Concerned and Land Classification` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "growing stock", "authorities", "concerned", "land classification"), "행정구역"),
                 filter_text_data(text_data, c("소관별", "지종별", "임야면적", "임목축적"), "행정구역"))


##### 🟪 +행정구역별 =================================================================================================================================
combined.list$`행정구역별 소관별ㆍ지종별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Authorities Concerned and Land Classification` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "growing stock", "authorities", "concerned", "land classification", "행정")),
                 filter_text_data(text_data, c("행정구역별", "소관별", "지종별", "임야면적", "임목축적")))




#### 🟦 용도별 산지 이용구분 조사 실적 ===================================================================================================
combined.list$`산지 이용 구분_Classification of Forest Land Utilization` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "use", "classification")),
                 filter_text_data(text_data, c("classification", "forest", "land", "utilization")),
                 filter_text_data(text_data, c("산지", "이용", "구분")))






#### 🟦 행정구역별 임야/산림면적 및 임목축적 ===================================================================================================
##### 🟪 임야면적 및 임목축적 ===================================================================================================
combined.list$`행정구역별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Administrative Districts` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "land", "area", "growing", "stock", "administrative", "districts"),
                                  c("영림서편", "소유별", "임상별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "면적", "임목축적"),
                                  c("영림서편", "소유별", "임상별")))

##### 🟪 임상별 임목축적 ===================================================================================================
combined.list$`행정구역별, 임상별, 임목축적_Forest Growing Stock by Forest Type and Administrative District` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "growing", "stock", "forest", "type", "administrative", "district"),
                                  c("영림서편", "소유별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "임상별", "임목축적"),
                                  c("영림서편", "소유별")))



##### 🟪 소관별 임야면적 ===================================================================================================
combined.list$`행정구역별 소관별 임야면적_Forest Land Area by Ownership and Administrative District` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "land", "area", "ownership", "administrative", "district"),
                                  c("영림서편", "소유별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "소관별", "임야면적"),
                                  c("영림서편", "소유별")))


##### 🟪 행정구역별 소관별/소유별 임목축적 ===================================================================================================
combined.list$`행정구역별, 소관별/소유별, 임목축적_Growing Stock by Ownership and Administrative District` =
  union_multiple(filter_text_data(text_data, 
                                  c("growing stock", "ownership", "administrative", "district"),
                                  c("영림서편", "임상별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "소관별", "임목", "축적"),
                                  c("영림서편", "소유별", "임상별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "소유별", "임목", "축적"),
                                  c("영림서편", "임상별")))




##### 🟪 행정구역별 임상별 임야면적 ==============================================================================
combined.list$`행정구역별 임상별 임야면적_Forest Land Area by Forest Type and Administrative District` =
  union_multiple(filter_text_data(text_data, 
                                  c("land area", "forest type", "administrative district"),
                                  c("소유별", "영급별")),
                 filter_text_data(text_data, 
                                  c("행정구역별", "임상별", "면적"), 
                                  c("소유별", "영급별")))





#### 🟦 소유별 ===================================================================================================
##### 🟪 소유 현황 ===================================================================================================
combined.list$`산림소유 현황_Status of Forest Ownership` =
  union_multiple(filter_text_data(text_data, c("status", "forest", "ownership"), "사유림"),
                 filter_text_data(text_data, c("산림", "소유", "현황"), "사유림"))




##### 🟪 소유별 지종별 임야/산림면적 및 임목축적 ===================================================================================================
combined.list$`소유별, 지종별 임야/산림면적 및 임목축적_Forest area and growing stock by ownership and land classification` =
  union_multiple(filter_text_data(text_data, c("forest", "area", "growing", "stock", "ownership", "land", "classification")),
                 filter_text_data(text_data, c("소유별", "지종별", "임야", "면적", "임목", "축적")))


##### 🟪 소유별 임목축적 (-소관별, -영급별) ===================================================================================================
combined.list$`소유별 임목축적_Forest growing stock by ownership` =
  union_multiple(filter_text_data(text_data, c("forest", "growing stock", "ownership"),c("area", "proportion", "구성", "administrative", "age class")),
                 filter_text_data(text_data, c("소유별", "임목", "축적"), c("면적", "구성", "소관별", "영급별")))
combined.list$`소유별 임목축적구성_Proportion of Forest Growing Stock by Ownership` =
  union_multiple(filter_text_data(text_data, c("proportion", "forest", "growing", "stock", "ownership")),
                 filter_text_data(text_data, c("소유별", "임목", "축적", "구성")))


##### 🟪 소유별 임목축적 ===================================================================================================
combined.list$`소유별 임목축적_Forest growing stock by ownership` =
  union_multiple(filter_text_data(text_data, c("forest", "growing stock", "ownership"),c("area", "proportion", "구성")),
                 filter_text_data(text_data, c("소유별", "임목", "축적"), c("면적", "구성")))


##### 🟪 소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership ===================================================================================================
combined.list$`소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest land area", "growing stock", "ownership"), 
                                  c("영급별", "임상별")),
                 filter_text_data(text_data, 
                                  c("소유별", "임야", "면적", "임목", "축적"), 
                                  c("영급별", "임상별")))


##### 🟪 소유별 임야면적_Area of forest land by ownership ===================================================================================================
combined.list$`소유별 임야면적_Area of forest land by ownership` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest land area", "ownership"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림")),
                 filter_text_data(text_data, 
                                  c("소유별", "임야", "면적"), 
                                  c("행정구역", "영급별", "임상별", "임목", "사유림")))


##### 🟪 소유별 임야면적 구성 ===================================================================================================
combined.list$`소유별 임야면적 구성_Proportion of Forest Land Area by Ownership` =
  union_multiple(filter_text_data(text_data, c("proportion", "forest", "land", "area", "ownership")),
                 filter_text_data(text_data, c("소유별", "임야", "면적", "구성")))


##### 🟪 소유별 임상별 임야면적 및 임목축적 ===================================================================================================
combined.list$`소유별ㆍ임상별 임야면적_Forest Land Area Ownership and Forest Type` =
  union_multiple(filter_text_data(text_data, c("forest land area", "ownership", "forest type"), "administrative"),
                 filter_text_data(text_data, c("소유별", "임상별", "임야", "면적")),
                 filter_text_data(text_data, c("소유별", "임상별", "임목", "축적")))


##### 🟪 소유별 임야면적 및 임목축적===================================================================================================
combined.list$`소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest land area", "by ownership", "Growing stock"), 
                                  c("임상별","administrative", "지종별")),
                 filter_text_data(text_data, 
                                  c("소유별", "임야", "면적", "임목", "축적"), 
                                  c("임상별", "행정별", "지종별")))

##### 🟪 소유별 영급별 임야면적 ===================================================================================================
combined.list$`소유별 영급별 임야면적_Area of Forest Land by Age Class and Ownership` =
  union_multiple(filter_text_data(text_data, 
                                  c("area", "forest", "land", "age", "class", "ownership"), 
                                  c("축적", "임상별")),
                 filter_text_data(text_data, 
                                  c("소관별", "영급별", "면적"), 
                                  c("축적", "임상별")),
                 filter_text_data(text_data, 
                                  c("소유별", "영급별", "면적"), 
                                  c("축적", "임상별")))



#### 🟦 사유림 ===================================================================================================
##### 🟪 사유림 소유 규모 ===================================================================================================
combined.list$`사유림 소유 규모_Ownership Scale of Private Forest` =
  union_multiple(filter_text_data(text_data, c("ownership", "scale", "private", "forest"), "산주"),
                 filter_text_data(text_data, c("사유림", "소유", "규모"), "산주"))
##### 🟪 사유림 소유 형태별 필지수 ===================================================================================================
combined.list$`사유림 소유형태별 필지수현황_Status of block of Forest Private Land by Ownership` =
  union_multiple(filter_text_data(text_data, c("status", "block", "forest", "private", "land", "ownership")),
                 filter_text_data(text_data, c("사유림", "소유형태별", "필지수", "현황")))
##### 🟪 사유림 소유규모별 산주현황 ===================================================================================================
combined.list$`사유림 소유규모별 산주현황_Private Forest Owners by Size` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owners", "size")),
                 filter_text_data(text_data, c("사유림", "소유", "규모","산주", "현황")))

##### 🟪 사유림 소유형태별 산주현황 ===================================================================================================
combined.list$`사유림 소유형태별 산주현황_Private Forest Owners by Size` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owners", "by owernership")),
                 filter_text_data(text_data, c("사유림", "소유형태별", "산주", "현황")),
                 filter_text_data(text_data, c("사유림", "소유", "형태", "산주", "현황")))
##### 🟪 사유림 소재 부재 산주현황 ===================================================================================================
combined.list$`사유림 소재ㆍ부재 산주 현황_Status of Resident and Absentee Forest Owner` =
  union_multiple(filter_text_data(text_data, c("status", "resident", "absentee", "forest", "owner")),
                 filter_text_data(text_data, c("사유림", "소재", "부재", "산주", "현황")))
##### 🟪 사유림 소유형태별 산림면적 ===================================================================================================
combined.list$`사유림 소유형태별 산림면적_Private Forest Land Area by Ownership` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "land", "area", "ownership")),
                 filter_text_data(text_data, c("사유림", "소유형태별", "산림면적")))
##### 🟪 사유림 소유형태별 임야/산림면적표 ===================================================================================================
combined.list$`다. 기타_Others___사유임야 소유형태별 임야/산림면적표_Table of Private Forest Area by Ownership Form` =
  union_multiple(filter_text_data(text_data, c("table", "private", "forest", "area", "ownership", "form")),
                 filter_text_data(text_data, c("사유임야", "소유형태별", "임야", "면적표")),
                 filter_text_data(text_data, c("사유임야", "소유형태별", "산림", "면적표")))




#### 🟦 영림서 ===================================================================================================
##### 🟪 영림서등관리 국유림 기관별, 임상별 산림면적 ===================================================================================================
combined.list$`영림서등관리 국유림 기관별， 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type` =
  union_multiple(filter_text_data(text_data, c("forest", "area", "national", "forest", "management", "authorities", "forest type")),
                 filter_text_data(text_data, c("영림서등관리", "국유림", "기관별", "임상별", "산림", "면적")))


##### 🟪 영림서등관리 국유림 기관별, 임상별 임목축적 ===================================================================================================
combined.list$`영림서등관리 국유림 기관별, 임상별 임목축적_Growing Stock by National Forest management Authorities and Forest type` =
  union_multiple(filter_text_data(text_data, c("growing stock", "national", "forest", "management", "authorities", "forest", "type")),
                 filter_text_data(text_data, c("영림서등관리", "국유림", "기관별", "임상별", "임목", "축적")))


##### 🟪 영림서등관리 국유림 기관별, 지종별 산림면적 및 임목축적 ===================================================================================================
combined.list$`영림서등관리 국유림 기관별, 지종별, 산림면적 및 임목축적_Forest Land Area and Growing Stock by National Forest management Authorities and Land Classification` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "growing", "stock", "national", "forest", "management", "authorities", "land", "classification")),
                 filter_text_data(text_data, c("영림서등관리", "국유림", "기관별", "지종별", "산림면적", "임목축적")))



##### 🟪 영림서편 지종별ㆍ임야면적 및 임목축적 ===================================================================================================
combined.list$`영림서편 지종별ㆍ임야면적 및 임목축적_Details by National Forest Station Forest Area and Growing Stock by Land Classification` =
  union_multiple(filter_text_data(text_data, c("details", "national", "forest", "station", "forest area", "growing stock", "land classification")),
                 filter_text_data(text_data, c("영림서편", "지종별", "임야", "면적", "임목", "축적")))


##### 🟪 영림서편 임상별 영급별 임야면적 및 임목축적 ===================================================================================================
combined.list$`영림서편 임상별, 영급별 임야면적_Details by National Forest Station / Forest Area and Growing Stock by Forest Type and Age-Classes` =
  union_multiple(filter_text_data(text_data, c("National Forest Station", "forest area", "Growing stock", "forest", "type", "age-classes")),
                 filter_text_data(text_data, c("영림서편", "임상별", "영급별", "임야", "면적", "임목","축적")),
                 filter_text_data(text_data, c("영림서편", "임상별", "영급별", "임야", "면적")),
                 filter_text_data(text_data, c("영림서편", "임상별", "영급별", "임목", "축적")))






#### 🟦 산림청소관 국유림 관리 ===================================================================================================
##### 🟪 산림청소관 국유림 관리 기관별, 임상별 산림면적 ===================================================================================================
combined.list$`산림청소관 국유림 관리 기관별, 임상별 산림면적_Forest Land Area by Management Agencies and Forest Type of National Forest under Forestry Administration` =
  union_multiple(filter_text_data(text_data, c("forest", "land area", "management agencies", "forest type", "national", "under", "forestry", "administration")),
                 filter_text_data(text_data, c("산림청소관", "국유림", "관리", "기관별", "임상별", "면적")))



#### 🟦 평균임목축적 ===================================================================================================
combined.list$`시ㆍ도ㆍ서별 1ha당 평균임목축적_Average Growing Stock per 1ha by City Province and National Forest Station` =
  union_multiple(filter_text_data(text_data, c("average", "growing", "stock", "1ha", "city", "province", "national", "forest", "station")),
                 filter_text_data(text_data, c("시", "도", "서별", "1ha당", "평균", "임목", "축적")))
combined.list$`연도별 ha당 평균임목축적_Mean Growing Stock per ha by Year` =
  union_multiple(filter_text_data(text_data, c("mean", "growing", "stock", "ha", "year")),
                 filter_text_data(text_data, c("연도별", "ha당", "평균", "임목", "축적")))



#### 🟦 연도별 ===================================================================================================
##### 🟪 연도별 산림면적 및 임목축적 ===================================================================================================
combined.list$`연도별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Year` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "growing", "stock", "year")),
                 filter_text_data(text_data, c("연도별", "산림면적", "임목", "축적")))
##### 🟪 연도별 평균 임목축적 ===================================================================================================
combined.list$`연도별 ㏊당 평균임목축적_Mean Growing Stock Per ㏊ by Year` =
  union_multiple(filter_text_data(text_data, c("mean", "growing", "stock", "per", "ha", "year")),
                 filter_text_data(text_data, c("연도별", "㏊당", "평균", "임목축적")))
##### 🟪 연도별 임상별 임야면적 ===================================================================================================
combined.list$`연도별 임상별 임야면적_Forest Land Area by Forest Type and Year` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "forest", "type", "year"), "축적"),
                 filter_text_data(text_data, c("연도별", "임상별", "임야면적"), "축적"))






#### 🟦 영급별 ===================================================================================================
##### 🟪 임야면적 ===================================================================================================
combined.list$`영급별 임야면적_Area of Forest Land by Age Class` =
  union_multiple(filter_text_data(text_data, 
                                  c("area", "forest", "land", "age", "class"),
                                  c("임상별", "축적", "소관별", "소유별")),
                 filter_text_data(text_data, 
                                  c("영급별", "임야면적"),
                                  c("임상별", "축적", "소관별", "소유별")))

##### 🟪 임목 축적 ===================================================================================================
combined.list$`영급별 임목축적_Forest growing stock by age class` =
  union_multiple(filter_text_data(text_data, 
                                  c("Forest growing stock", "age", "class"),
                                  c("임급별", "area", "소관별", "소유별")),
                 filter_text_data(text_data, 
                                  c("영급별", "임목", "축적"),
                                  c("임급별", "면적", "소관별", "소유별")))







#### 🟦 임상별 ===================================================================================================
##### 🟪 임상별 임야면적 ===================================================================================================
combined.list$`임상별 임야면적_forest land area by forest type` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "forest", "type"), c("propotion", "proportion", "영급", "소유별", "연도별", "기관별", "행정구역")),
                 filter_text_data(text_data, c("임상별", "임야", "면적"),  c("구성", "영급별", "소유별", "연도별", "기관별", "행정구역")))

##### 🟪 임상별 임목축적 ===================================================================================================
combined.list$`임상별 임목축적_Forest Growing Stock by Forest Type` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "growing", "stock", "forest", "type"),
                                  c("관리청등", "행정구역별", "영림서", "관리기관별", "영급별", "소유별", "관리 기관별")),
                 filter_text_data(text_data, 
                                  c("임상별", "임목축적"), 
                                  c("관리청등", "행정구역별", "영림서", "관리기관별", "영급별", "소유별", "관리 기관별")))
##### 🟪 임상별 임야면적구성 ===================================================================================================
combined.list$`임상별 임야면적구성_Proportion of forest land area by forest type` =
  union_multiple(filter_text_data(text_data, c("proportion", "forest", "land", "area", "forest", "type")),
                 filter_text_data(text_data, c("임상별", "임야", "면적", "구성")))
##### 🟪 임상별 임목축적구성 ===================================================================================================
combined.list$`임상별 임목축적구성_Proportion of forest growing stock by forest type` =
  union_multiple(filter_text_data(text_data, c("proportion", "forest growing stock", "forest type"), c("age class")),
                 filter_text_data(text_data, c("임상별", "임목", "축적", "구성"), c("영급별")))



##### 🟪 임상별 영급별 임야면적 및 임목축적 ===================================================================================================
combined.list$`임상별, 영급별 임야면적 및 임목축적_Forest Area and Growing Stock by Forest Type and Age-Classes` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest area", "growing stock", "forest", "type", "age", "class"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("임상별", "영급별", "면적", "임목축적"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("Forest area and growing stok by forest type and age-classes"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("임상별ㆍ영급별 임야면적 및 임목축적"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("Forest Area by Forest Type and Age-Classes"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("임상별, 영급별 임야면적 및 임목축적"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("Forest Area and Growing Stock by Forest Type and Age-Classes"),
                                  c("영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("12. 임상별, 영급별 임야면적 및 임목축적_12. Forest Area and Growing Stock by Forest Type and Age-Classes___Ｏ 동부영림서_Ｏ Eastern N.F.S"),
                                  c("영림서편", "소유별", "행정")))



##### 🟪 임상별 영급별 임야면적 =========================================================================================
combined.list$`임상별, 영급별 임야면적_Forest Area by Forest Type and Age-Classes` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "area", "forest", "type", "age-classes"),
                                  c("축적", "영림서편", "소유별", "행정")),
                 filter_text_data(text_data, 
                                  c("임상별", "영급별", "임야면적"), 
                                  c("축적", "영림서편", "소유별", "행정")))




#### 🟦 산림청소관 ======================================================================================
##### 🟪 산림청소관 국유림 관리 기관별 임상별 산림면적 및 임목축적 ===================================================================================================
combined.list$`산림청소관 국유림 관리 기관별, 임상별 산림면적_Forest Land Area Forest Type of National Forest by Management Agencies and under Forestry Administration` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "area", "forest", "type", "national", "forest", "management", "agencies", "under", "forestry", "administration")),
                 filter_text_data(text_data, c("산림청소관", "국유림", "관리", "기관별", "임상별", "산림", "면적")))


##### 🟪 산림청소관 국유림 관리 기관별 지종별 산림면적 및 임목축적 ===================================================================================================
combined.list$`산림청소관 국유림 관리기관별, 지종별 산림면적 및 임목축적_Forest Land Area and Growing Stock Classification of National Forest by Management Agencies and Land under Forestry Administration` =
  union_multiple(filter_text_data(text_data, c("forest land area", "growing stock", "classification", "national", "forest", "management", "agencies", "land", "under", "forestry", "administration")),
                 filter_text_data(text_data, c("산림청소관", "국유림", "관리기관별", "지종별", "산림면적", "임목축적")))










### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
unfiltered_data <- setdiff(text_data, unlist(combined.list))


k=15
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])










## 🟧 14.산림/임업 경영  (Forest Management) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=14
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=20
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()




#### 🟦 산림계 현황과 위탁림 실태  ==================================================================================
combined.list$`산림계 현황과 위탁림 실태_Status of Village Forestry Association and the Consigned Forests` =
  union_multiple(filter_text_data(text_data, c("status", "village", "forestry", "association", "consigned", "forests")),
                 filter_text_data(text_data, c("산림계", "현황", "위탁림", "실태")),
                 filter_text_data(text_data, c("산림계", "현황")))


#### 🟦 국유림 대부(사용허가)) 현황  ==================================================================================
##### 🟪 불요존 (disposable) =============================================================================================
combined.list$`불요존 국유림 대부(사용허가)현황_Status of land lease out of disposable national forest` =
  union_multiple(filter_text_data(text_data, include = c("lease out", " disposable")),
        filter_text_data(text_data, include = c("lease out", " dispensable")),
        filter_text_data(text_data, c("status", "land", "lease", "disposable", "national", "forest"), "indis"),
        filter_text_data(text_data, c("불요존", "국유임", "대부", "현황")),
        filter_text_data(text_data, c("불요존", "국유림", "대부", "현황")))


##### 🟪 요존 (indisposable) =============================================================================================
combined.list$`요존 국유림 대부(사용허가) 현황_Status of Land Lease out of Indispensable National Forest` = 
  union_multiple(filter_text_data(text_data, 
                         c("lease out", " indispensable")),
                filter_text_data(text_data, 
                                 c("status", "land", "lease", " indisposable", "forest"), 
                                 " disposable"),
                filter_text_data(text_data, 
                                 c("land", "lease", " indisposable", "national", "forest"), 
                                 " disposable"),
                filter_text_data(text_data, 
                                 c("요존", "국유임", "대부", "현황"), 
                                 "불요존"),
                filter_text_data(text_data, 
                                 c("요존", "국유림", "대부", "현황"), 
                                 "불요존"))


#### 🟦 국유림 분수림  ==================================================================================
##### 🟪 불요존 =============================================================================================
combined.list$`불요존 국유림 분수림 현황_Status of Porfit-sharing Forest of Disposable National Forest` =
  union_multiple(filter_text_data(text_data, include = c("profit", "sharing", " disposable")),
                 filter_text_data(text_data, include = c("불요존", "분수림")),
                 filter_text_data(text_data, include = c("profit", "sharing", " dispensable")),
                 filter_text_data(text_data, include = c("profit-sharing", " dispensable")),
                 filter_text_data(text_data, c("status", "profit-sharing", "forest", " disposable", "national", "forest")),
                 filter_text_data(text_data, c("불요존", "국유임", "분수림", "현황")),
                 filter_text_data(text_data, c("불요존", "국유림", "분수림", "현황")))


#### 🟦 경계 측량 및 표주 설치  ==================================================================================
combined.list$`국유임야 경계측량 및 표주설치 실적_Boundary Survey and Landmark in National Forest` =
  union_multiple(filter_text_data(text_data, c("boundary", "survey", "landmark", "national", "forest")),
                 filter_text_data(text_data, c("국유임", "측량", "표주", "실적")),
                 filter_text_data(text_data, c("국유림", "측량", "표주", "실적")),
                 filter_text_data(text_data, c("boundary", "survey", "landmark", "national", "forest")),
                 filter_text_data(text_data, c("국유임야", "경제", "측량", "표주", "설치", "실적")))


#### 🟦 국유림 직영 벌채 사업 생산  ==================================================================================
combined.list$`국유림직영벌채사업생산 및 매각실적_Production and Sales of Timber by Government Felling` = 
  union(filter_text_data(text_data, include = c("production", "timber", "government")),
        filter_text_data(text_data, include = c("직영", "벌채")))


#### 🟦 국유림 부산물 처분 실적  ==================================================================================
combined.list$`국유임야 부산물 처분실적_Sales of Minor Forest Products from National Forest` =
  union_multiple(filter_text_data(text_data, c("sales", "minor", "forest", "products", "national", "forest")),
                 filter_text_data(text_data, c("국유임", "부산물", "처분", "실적")),
                 filter_text_data(text_data, c("국유림", "부산물", "처분", "실적")))


#### 🟦 국유 재산 취득 및 처분  ==================================================================================
combined.list$`국유 재산 취득 및 처분_Acquisition And Disposal Of National Forest Land`=
  union_multiple(filter_text_data(text_data, include = c("acquisition", " disposal")),
        filter_text_data(text_data, include = c("국유", "재산","취득")),
        filter_text_data(text_data, c("disposal", "purchase", "forest", "land")),
        filter_text_data(text_data, c("국유재산", "취득", "처분")))



#### 🟦 관행 작벌  ==================================================================================
combined.list$`관행작벌 사업생산 및 매각실적_Production and Sale of Timber by Government Felling` =
  union_multiple(filter_text_data(text_data, c("production", "sale", "timber", "government", "felling")),
                 filter_text_data(text_data, c("관행작벌", "사업", "생산", "매각", "실적")))


#### 🟦 임도시설  ==================================================================================
combined.list$`임도시설 현황_Status of Forest Road` =
  union_multiple(filter_text_data(text_data, c("status", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설", "현황")),
                 filter_text_data(text_data, c("construction", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설")))



#### 🟦 기타 ===================================================================================================
combined.list$`기타` =
  union_multiple(filter_text_data(text_data, c("Ｏ 강릉영림서_Ｏ Kangnung N.F.O.")),
                 filter_text_data(text_data, c("Ｏ 공주영림서_Ｏ Kongju N.F.O.")),
                 filter_text_data(text_data, c("Ｏ 남원영림서_Ｏ Namwon N.F.O.")),
                 filter_text_data(text_data, c("Ｏ 안동영림서_Ｏ Andong N.F.O.")),
                 filter_text_data(text_data, c("Ｏ 원주영림서_Ｏ Wonju N.F.O.")),
                 filter_text_data(text_data, c("Ｏ 임목육종연구소_Ｏ Forest G.R.I.")),
                 filter_text_data(text_data, c("Ｏ 임업연구원_Ｏ Forestry R.I")))
#### 🟦 귀산촌인 연령별 현황 ===================================================================================================
combined.list$`귀산촌인 연령별 현황_People Returning to Mountain Villages by Age` =
  union_multiple(filter_text_data(text_data, c("people", "return", "mountain", "age")),
                 filter_text_data(text_data, c("산촌", "연령")))
#### 🟦 산림관계 세입 세출 예산 ===================================================================================================
combined.list$`산림관계 세입·세출 예산_Tax Revenue and Expenditure of Korea Forest Service` =
  union_multiple(filter_text_data(text_data, c("tax", "revenue", "expenditure", "forest")),
                 filter_text_data(text_data, c("산림", "세출")),
                 filter_text_data(text_data, c("산림", "세입")))
#### 🟦 임가경제 주요지표 ===================================================================================================
combined.list$`임가경제 주요지표_Main indicators of forest household economy` =
  union_multiple(filter_text_data(text_data, c("main", "indicator", "household", "economy", "forest")),
                 filter_text_data(text_data, c("임가", "경제", "주요", "지표")))
#### 🟦 재배작물별 재배면적 ===================================================================================================
combined.list$`재배작물별 재배면적_Cultivated Area by Growing Crops` =
  union_multiple(filter_text_data(text_data, c("cultivated", "area", "crop")),
                 filter_text_data(text_data, c("재배", "작물", "재배", "면적")))
#### 🟦 산림사업 융자 ===================================================================================================
##### 🟪 실적 =====================================================================
combined.list$`산림사업 융자실적_Loans for Forest Activities` =
  union_multiple(filter_text_data(text_data, c("loan", "forest", "activities")),
                 filter_text_data(text_data, c("산림", "융자", "실적")))
##### 🟪 규모 =====================================================================
combined.list$`산림사업 융자규모_Loans for Forestry Business` =
  union_multiple(filter_text_data(text_data, c("loan", "forest", "business")),
                 filter_text_data(text_data, c("산림", "융자", "규모")))
#### 🟦 영림단 ===================================================================================================
combined.list$`영림단 운영현황_Forest Management Units` =
  union_multiple(filter_text_data(text_data, c("forest", "management", "unit")),
                 filter_text_data(text_data, c("영림단")))
#### 🟦 임가 현황 ===================================================================================================
##### 🟪 개인 =====================================================================
combined.list$`개인 임가 현황_Private Forest Households` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "household")),
                 filter_text_data(text_data, c("개인", "임가")))
##### 🟪 전 겸업별 =====================================================================
combined.list$`전ㆍ겸업별 임가 현황_Forest Households by Fall and Part Time` =
  union_multiple(filter_text_data(text_data, c("forest", "household", "part", "time")),
                 filter_text_data(text_data, c("겸업별", "임가", "현황")))
##### 🟪 경영형태별 =====================================================================
combined.list$`경영형태별 임가 현황_Forest Households by management type` =
  union_multiple(filter_text_data(text_data, c("forest", "household", "management")),
                 filter_text_data(text_data, c("경영", "형태", "임가")))
##### 🟪 생산형태별 =====================================================================
combined.list$`생산형태별 임가 현황_Forest Households by Production Type` =
  union_multiple(filter_text_data(text_data, c("forest", "households", "production", "type")),
                 filter_text_data(text_data, c("생산", "형태", "임가")))
#### 🟦 사유림 ===================================================================================================
##### 🟪 소유 규모별 산주 =====================================================================
combined.list$`소유 규모별 사유림 산주현황_Status of Private Forest Land Owners by Land Area Size` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "land", "owner", "area", "size")),
                 filter_text_data(text_data, c("사유림", "소유", "규모", "산주")))
##### 🟪 지역별 산주 현황 =====================================================================
combined.list$`지역별 사유림 산주현황_Ownership of Private Forests by Regions` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "region", "ownership")),
                 filter_text_data(text_data, c("지역", "사유림", "산주")))
##### 🟪 산주 거주지 =====================================================================
combined.list$`임야소재지별 사유림 개인산주 거주지 현황_Location of Residence of Private Forest Land Owner by Location of Forest Land` =
  union_multiple(filter_text_data(text_data, c("location", "residence", "private", "forest", "land", "owner")),
                 filter_text_data(text_data, c("임야", "소재", "사유림", "개인", "산주", "거주지")))
##### 🟪 소재 부재 산주 현황  =====================================================================
combined.list$`사유림 소재ㆍ부재 산주현황_Status of Resident and Non-resident Forest Owners by Province` =
  union_multiple(filter_text_data(text_data, c("resident", "forest", "owner", "province")),
                 filter_text_data(text_data, c("사유림", "소재", "산주"), c("거주지")))
##### 🟪 협업 경영 사업 =====================================================================
combined.list$`사유림 협업경영사업_Activities of Private Forest Cooperatives` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "cooperation", "activities")),
                 filter_text_data(text_data, c("사유림", "협업", "경영", "사업")))



#### 🟦 임업 기계 장비 보유 ===================================================================================================
combined.list$`임업기계 · 장비  보유현황_Forest Machinery and Equipment` =
  union_multiple(filter_text_data(text_data, c("forest", "machinery", "equipment")),
                 filter_text_data(text_data, c("임업", "기계", "장비")))



#### 🟦 임가 및 임가 인구 ===================================================================================================
combined.list$`임가 및 임가 인구_Number of Households and Population engaged in the Forestry Sector` =
  union_multiple(filter_text_data(text_data, c("number", "household", "engaged", "forestry")),
                 filter_text_data(text_data, c("임가", "인구")))


#### 🟦 산림사업 고용 현황 ===================================================================================================
combined.list$`산림사업 고용현황_Status of Employment in Forestry` =
  union_multiple(filter_text_data(text_data, c("employment", "forestry")),
                 filter_text_data(text_data, c("산림", "고용")))


#### 🟦 임업 노동력 ===================================================================================================
combined.list$`임업노동력현황_Number Of Workers Engaged In Forestry` =
  union_multiple(filter_text_data(text_data, c("number", "worker", "engaged", "forestry")),
                 filter_text_data(text_data, c("임업", "노동력", "사유림", "개인", "산주", "거주지")))




#### 🟦 경영계획 ===================================================================================================
##### 🟪 산림/영림계획 작성 현황 ======================================================================================================
combined.list$`산림/영림경영계획 작성현황_Preparation of Forest Management Plan` =
  union_multiple(filter_text_data(text_data, c("preparation", "forest", "management", "plan")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "작성")),
                 filter_text_data(text_data, c("영림", "계획", "작성")))
##### 🟪 산림/영림계획 편성 실적 ======================================================================================================
combined.list$`영림계획 편성실적_Accomplishment of Forest Management Plan by Ownership` =
  union_multiple(filter_text_data(text_data, c("accomplishment", "forest", "management", "plan")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "편성", "실적")),
                 filter_text_data(text_data, c("영림", "계획", "편성", "실적")))
##### 🟪 국유림 및 민유림 경영계획 ======================================================================================================
combined.list$`국유림 및 민유림 경영계획 편성실적_Accomplishment of Working Plan Preparation` =
  union_multiple(filter_text_data(text_data, c("accomplishment", "working", "plan", "preparation")),
                 filter_text_data(text_data, c("국유림", "민유림", "경영계획", "편성", "실적")))
#### 🟦 산촌생태마을 조성 ===================================================================================================
combined.list$`산촌생태마을 조성현황_Status of Mountain Village Development` =
  union_multiple(filter_text_data(text_data, c("mountain", "village", "development")),
                 filter_text_data(text_data, c("산촌", "생태마을", "조성")))
#### 🟦 해외산림개발 진출 ===================================================================================================
combined.list$`해외산림개발진출현황_Overseas Forest Development` =
  union_multiple(filter_text_data(text_data, c("overseas", "forest", "development")),
                 filter_text_data(text_data, c("해외", "산림", "개발", "진출")))
#### 🟦 독림가 및 임업 후계자 ===================================================================================================
combined.list$`독림가 및 임업후계자 현황_Outstanding Forest Managers and Forest Successors` =
  union_multiple(filter_text_data(text_data, c("outstanding", "forest", "managers", "successors")),
                 filter_text_data(text_data, c("독림가", "임업", "후계자")))
#### 🟦 화전정리 ===================================================================================================
combined.list$`화전정리실적_Arrangement of Shifting Cultivations` =
  union_multiple(filter_text_data(text_data, c("arrangement", "shifting", "cultivations")),
                 filter_text_data(text_data, c("화전", "정리", "실적")))
#### 🟦 영림서 ===================================================================================================
##### 🟪 영림서등관리 국유림 기관별, 임상별 산림면적 ===================================================================================================
combined.list$`영림서등관리 국유림 기관별， 임상별 산림면적_Forest Area by National Forest management Authorities and Forest type` =
  union_multiple(filter_text_data(text_data, c("forest", "area", "national", "forest", "management", "authorities", "forest type")),
                 filter_text_data(text_data, c("영림서등관리", "국유림", "기관별", "임상별", "산림", "면적")))


##### 🟪 영림서등관리 국유림 기관별, 임상별 임목축적 ===================================================================================================
combined.list$`영림서등관리 국유림 기관별, 임상별 임목축적_Growing Stock by National Forest management Authorities and Forest type` =
  union_multiple(filter_text_data(text_data, c("growing stock", "national", "forest", "management", "authorities", "forest", "type")),
                 filter_text_data(text_data, c("영림서등관리", "국유림", "기관별", "임상별", "임목", "축적")))




### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=1
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

### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])











## 🟧 15.임업생산 (Forest Production) #######################################################################################################################
### 🟩 extract the data ######################################################################################
i=15
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=20
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters %>% sapply(., function(x){head(x,3)})




### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()



#### 🟦 국내생산과 임엄생산 ===================================================================================================
##### 🟪 경제활동별 국내총생산 ===================================================================================================
combined.list$`경제활동별 국내총생산_Gross Domestic Product By Kind Of Economic Activity` =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "kind", "economic", "activity")),
                 filter_text_data(text_data, c("경제활동별", "국내총생산")))
##### 🟪 국내총생산과 임업생산 ===================================================================================================
combined.list$`국내총생산과 임업생산_Gross Domestic Product and Forestry Product` =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "forestry", "product")),
                 filter_text_data(text_data, c("국내총생산", "임업생산")))
##### 🟪 국민총생산과 임업생산 ===================================================================================================
combined.list$`국민총생산과 임업생산_Gross National Product & Forestry Product` =
  union_multiple(filter_text_data(text_data, c("gross", "national", "product", "forestry", "product")),
                 filter_text_data(text_data, c("국민총생산", "임업생산")))
##### 🟪 산업별 국민총생산 ===================================================================================================
combined.list$`산업별 국민총생산_Gross National Product by Sector` =
  union_multiple(filter_text_data(text_data, c("gross", "national", "product", "sector")),
                 filter_text_data(text_data, c("산업별", "국민총생산")))



#### 🟦 임목벌채 ===================================================================================================
##### 🟪 임목 벌채 공급 계획 및 실적 ===================================================================================================
combined.list$`임목벌채 공급계획 및 실적_Cutting Plan and Supply of Timber` =
  union_multiple(filter_text_data(text_data, c("cutting", "plan", "supply", "timber")),
                 filter_text_data(text_data, c("임목", "벌채", "공급","계획", "실적")),
                 filter_text_data(text_data, c("cutting", "plan", "production", "timber")))
##### 🟪 임목 벌채 허가 실적 ===================================================================================================
combined.list$`임목벌채 허가실적_Permit of Annual Tree Cutting` =
  union_multiple(filter_text_data(text_data, c("permit", "annual", "tree", "cutting")),
                 filter_text_data(text_data, c("임목", "벌채", "허가", "실적")))
##### 🟪 임목 벌채 실적 ===================================================================================================
combined.list$`임목 벌채실적_Timber Production` =
  union_multiple(filter_text_data(text_data, c("timber", "production", "벌채"), c("permit", "생산량표", "임산물", "계획","배정량")),
                 filter_text_data(text_data, c("임목", "벌채", "실적"), c("허가", "생산량표", "임산물", "계획", "배정량")))
##### 🟪 임목 벌채 배정량 및 생산 실적 ===================================================================================================
combined.list$`임목벌채 배정량 및 생산실적_Allocation of Cutting Amount of Wood and Production` =
  union_multiple(filter_text_data(text_data, c("allocation", "cutting", "amount", "wood", "production")),
                 filter_text_data(text_data, c("임목", "벌채", "배정량", "생산", "실적")),
                 filter_text_data(text_data, c("임목", "별채", "배정량", "생산", "실적")))
##### 🟪 각종 지장목 벌채 실적 ===================================================================================================
combined.list$`각종 지장목 벌채실적_Thinning and Cutting of Interfering Trees` =
  union_multiple(filter_text_data(text_data, c("thinning", "cutting", "interfering", "trees")),
                 filter_text_data(text_data, c("지장목", "벌채", "실적")))




#### 🟦 목재 ===================================================================================================
##### 🟪 목재 수급계획 및 공급 실적 ===================================================================================================
combined.list$`목재 수급 계획 및 공급 실적_Facts of Demand and Supply for Timber` =
  union_multiple(filter_text_data(text_data, c("facts", "demand", "supply", "for", "timber")),
                 filter_text_data(text_data, c("목재", "수급", "계획", "공급", "실적")))



##### 🟪 목재 수급 실적 ===================================================================================================
combined.list$`목재 수급 실적_Demand and Supply of Timber` =
  union_multiple(filter_text_data(text_data, 
                                  c("demand", "supply", "timber"), 
                                  c("resource", "자원", "계획")),
                 filter_text_data(text_data, 
                                  c("목재", "수급", "실적"), 
                                  c("자원별", "계획")))


##### 🟪 목재생산 및 공급 실적 ===================================================================================================
combined.list$`목재생산 및 공급실적_Timber Production and Supply` =
  union_multiple(filter_text_data(text_data, c("timber", "production", "supply")),
                 filter_text_data(text_data, c("목재", "생산", "공급", "실적")))



##### 🟪 연도별 목재 가격 ===================================================================================================
combined.list$`연도별 목재가격_Lumber & Wood Price by Year` =
  union_multiple(filter_text_data(text_data, c("lumber", "wood", "price", "year")),
                 filter_text_data(text_data, c("연도별", "목재가격")))
##### 🟪 자원별 목재 수급 현황 ===================================================================================================
combined.list$`자원별 목재 수급현황_Wood Demand and Supply by Resources` =
  union_multiple(filter_text_data(text_data, c("wood", "demand", "supply", "resources")),
                 filter_text_data(text_data, c("자원별", "목재", "수급")),
                 filter_text_data(text_data, c("timber", "demand", "supply", "resources")))

##### 🟪 목재 가공품 생산 및 공급 ===================================================================================================
combined.list$`목재가공품 생산 및 공급_Production and Supply of Processed Wood` =
  union_multiple(filter_text_data(text_data, c("목재가공품", "생산", "공급")))




#### 🟦 외재 도입 ===================================================================================================
##### 🟪 산지별 외재 도입 실적 ===================================================================================================
combined.list$`산지별 외재 도입 실적_Timber Imports by Origin` =
  union_multiple(filter_text_data(text_data, c("산지별", "외재", "도입", "실적")))
##### 🟪 용도별 외재 도입 실적 ===================================================================================================
combined.list$`용도별 외재 도입 실적_Timber Imports By Use` =
  union_multiple(filter_text_data(text_data, c("용도별", "외재", "도입", "실적")))



#### 🟦 공급실적 ===================================================================================================
##### 🟪 용도별 국내재 공급 실적 ===================================================================================================
combined.list$`용도별 국내재 공급 실적_Domestic Timber Supply by Use` =
  union_multiple(filter_text_data(text_data, c("timber", "supply", "domestic", "use")),
                 filter_text_data(text_data, c("용도별", "국내재", "공급", "실적")))


##### 🟪 용도별 원목 공급 실적 ===================================================================================================
combined.list$`용도별 원목 공급 실적_Timber Supply by Use` =
  union_multiple(filter_text_data(text_data, c("timber", "supply", "use"), "domestic"),
                 filter_text_data(text_data, c("용도별", "원목", "공급", "실적"), "국내재"))


#### 🟦 생산실적 ===================================================================================================
##### 🟪 관상수 실적 ===================================================================================================
combined.list$`관상수 생산실적_Ornamental Tree Production` =
  union_multiple(filter_text_data(text_data, c("ornamental", "tree", "production")),
                 filter_text_data(text_data, c("관상수", "생산실적")))


##### 🟪 원목 생산 실적 ===================================================================================================
combined.list$`원목 생산실적_Pound Wood Production` =
  union_multiple(filter_text_data(text_data, c("pound", "wood", "production")),
                 filter_text_data(text_data, c("원목", "생산실적")))

##### 🟪 제재목  ===================================================================================================
combined.list$`제재목 생산 및 수급실적_Production and Supply of Sawnlog` =
  union_multiple(filter_text_data(text_data, c("production", "supply", "sawnlog")),
                 filter_text_data(text_data, c("제재목", "생산", "수급", "실적")),
                 filter_text_data(text_data, c("production", "supply", "sawnwood")),
                 filter_text_data(text_data, c("제재목", "생산", "수급실적")))


##### 🟪 포플러 제품 생산 및 공급 실적 ===================================================================================================
combined.list$`포플러 제품 생산 및 공급실적_Production and Supply of Popular Products` =
  union_multiple(filter_text_data(text_data, c("production", "supply", "popular", "products")),
                 filter_text_data(text_data, c("포플러", "제품", "생산", "공급실적")))
##### 🟪 칩 ===================================================================================================
combined.list$`칩 생산실적_Chip Production` =
  union_multiple(filter_text_data(text_data, c("chip", "production")),
                 filter_text_data(text_data, c("칩", "생산실적")))
##### 🟪 지류 ===================================================================================================
combined.list$`지류 생산실적_Paper Production` =
  union_multiple(filter_text_data(text_data, c("paper", "production")),
                 filter_text_data(text_data, c("지류", "생산실적")))
##### 🟪 펄프 ===================================================================================================
combined.list$`펄프 생산실적_Pulp Production` =
  union_multiple(filter_text_data(text_data, c("pulp", "production")),
                 filter_text_data(text_data, c("펄프", "생산실적")))






#### 🟦 임산물 ===================================================================================================
##### 🟪 임산물 생산량 ===================================================================================================
combined.list$`임산물 생산량_Production of Forest Products` =
  union_multiple(filter_text_data(text_data, c("production", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "생산량")))
##### 🟪 임산물 생산액의 구성 ===================================================================================================
combined.list$`임산물 생산액의 구성_Forest Products Value` =
  union_multiple(filter_text_data(text_data, c("forest", "products", "value"), c("export", "import")),
                 filter_text_data(text_data, c("임산물", "생산액", "구성")))
##### 🟪 임산물 유통 시설 및 저온저장고 ===================================================================================================
combined.list$`임산물 유통시설 및 저온저장고 지원현황_Forest Products Marketing Facilities and Low Temperature Storage Houses Financially Supported` =
  union_multiple(filter_text_data(text_data, c("forest", "products", "marketing", "facilities", "low", "temperature", "storage", "houses", "financially", "supported")),
                 filter_text_data(text_data, c("임산물", "유통시설", "저온저장고", "지원", "현황")))
##### 🟪 임산물 남북 교역  ===================================================================================================
combined.list$`임산물 남북교역 현황_Trade and Forest Products between South and North Korea` =
  union_multiple(filter_text_data(text_data, c("trade", "forest", "products", "south", "north", "korea")),
                 filter_text_data(text_data, c("임산물", "남북교역", "현황")))
##### 🟪 주요 임산물 가격  ===================================================================================================
combined.list$`주요 임산물 가격_Prices of Major Forest Products` =
  union_multiple(filter_text_data(text_data, c("prices", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "가격")))
##### 🟪 주요 임산물 수출 실적  ===================================================================================================
combined.list$`주요 임산물 수출 실적_Exports of Major Forest Products` =
  union_multiple(filter_text_data(text_data, c("exports", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "수출", "실적")))
##### 🟪 주요 임산물 수입 실적  ===================================================================================================
combined.list$`주요 임산물 수입 실적_Imports of Major Forest Products` =
  union_multiple(filter_text_data(text_data, c("imports", "major", "forest", "products")),
                 filter_text_data(text_data, c("주요", "임산물", "수입", "실적")))



#### 🟦 각종 지수 ===================================================================================================
##### 🟪 생산자 물가 지수  ===================================================================================================
combined.list$`생산자 물가지수_Producer Price Indexes` =
  union_multiple(filter_text_data(text_data, c("producer", "price", "indexes")),
                 filter_text_data(text_data, c("생산자", "물가지수")))
##### 🟪 도매물가지수  ===================================================================================================
combined.list$`도매물가지수_Index Numbers of Wholesale Price` =
  union_multiple(filter_text_data(text_data, c("index", "numbers", "wholesale", "price")),
                 filter_text_data(text_data, c("도매물가지수")))
##### 🟪 농촌물가지수  ===================================================================================================
combined.list$`농촌물가지수_Index Number of Prices in Rural Areas` =
  union_multiple(filter_text_data(text_data, c("index", "number", "prices", "rural", "areas")),
                 filter_text_data(text_data, c("농촌", "물가지수")))
##### 🟪 소비자물가지수  ===================================================================================================
combined.list$`소비자 물가지수_Index Numbers of Consumer Price` =
  union_multiple(filter_text_data(text_data, c("index", "numbers", "consumer", "price")),
                 filter_text_data(text_data, c("소비자", "물가지수")))

#### 🟦 제재 공장 실태 ===================================================================================================
# combined.list$`제재공장 실태_Status of sawmill` =
#   union_multiple(filter_text_data(text_data, c("status", "sawmill")),
#                  filter_text_data(text_data, c("제재공장", "실태")))




#### 🟦 기타 ===================================================================================================
##### 🟪 산림소유 규모별 개인 임가 현황  ===================================================================================================
combined.list$`산림소유 규모별 개인 임가 현황_Private Forest Households by Size of Forest Area` =
  union_multiple(filter_text_data(text_data, c("private", "forest", "households", "size", "forest", "area")),
                 filter_text_data(text_data, c("산림", "소유", "규모별", "개인", "임가", "현황")))





### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
unfiltered_data <- setdiff(text_data, unlist(combined.list))


k=7
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])






## 🟧 16.재정과 금융 (Finances and Loans) #######################################################################################################################
### 🟩 extract the data ######################################################################################
i=16
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=3
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters %>% sapply(., function(x){head(x,3)})




### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()



#### 🟦 농산촌저축현황 ===================================================================================================
combined.list$`농산촌저축현황_Savings in Rural Area` =
  union_multiple(filter_text_data(text_data, c("savings", "rural", "area")),
                 filter_text_data(text_data, c("농산촌", "저축", "현황")))

#### 🟦 산림사업융자실적 ===================================================================================================
combined.list$`산림사업융자실적_Loans for Forestry Activities` =
  union_multiple(filter_text_data(text_data, c("loans", "forestry", "activities")),
                 filter_text_data(text_data, c("산림사업", "융자", "실적")))

#### 🟦 산림관계세입예산 ===================================================================================================
combined.list$`산림관계세입예산_Forest Estimated Revenue` =
  union_multiple(filter_text_data(text_data, c("forest", "estimated", "revenue")),
                 filter_text_data(text_data, c("산림", "관계", "세입", "예산")))

#### 🟦 산림관계세출예산 ===================================================================================================
combined.list$`산림관계세출예산_Forest Estimated Expenditures` =
  union_multiple(filter_text_data(text_data, c("forest", "estimated", "expenditures")),
                 filter_text_data(text_data, c("산림", "관계", "세출", "예산")))

#### 🟦 중앙정부 재정수지 및 보전재원 ===================================================================================================
combined.list$`중앙정부 재정수지 및 보전재원_Consolidated Central Government Financing` =
  union_multiple(filter_text_data(text_data, c("consolidated", "central", "government", "financing")),
                 filter_text_data(text_data, c("중앙정부", "재정수지", "보전재원")))






### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
unfiltered_data <- setdiff(text_data, unlist(combined.list))


k=7
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])







## 🟧 17. 조림/보호/산림의 건강 및 다양성 (Forest Health and Diversity) #######################################################################################################################
### 🟩 extract the data =====================================================================================================
i=17
L2_categories[i] %>% cat
text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
length(text_data)


### 🟩 텍스트 클러스터링 =====================================================================================================
k=12
clustered_data = text_clustering(text_data, k_min = k, k_max = k)
clustered_data$clusters



### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
combined.list <- list()



#### 🟦 조림 ===================================================================================================
##### 🟪 가로수 심기 ===================================================================================================
combined.list$`가로수 나무심기 상황_Planting of Roadside Tree` =
  union_multiple(filter_text_data(text_data, c("planting", "roadside", "tree")),
                 filter_text_data(text_data, c("planting", "roadisde", "tree")),
                 filter_text_data(text_data, c("가로수", "나무", "심기")))
##### 🟪 활착 상황 ===================================================================================================
combined.list$`조림활착상황_Survival Rates of Reforestation` =
  union_multiple(filter_text_data(text_data, c("survival", "rates", "reforestation")),
                 filter_text_data(text_data, c("조림", "활착")))
##### 🟪 숲가꾸기  ===================================================================================================
combined.list$`숲가꾸기/육림_Forest Tending` =
  union_multiple(filter_text_data(text_data, c("forest", "tending")),
                 filter_text_data(text_data, c("육림")),
                 filter_text_data(text_data, c("숲", "가꾸기")))



#### 🟦 조림 면적 ===================================================================================================
##### 🟪 용도별 조림면적구성 ===================================================================================================
combined.list$`용도별 조림면적구성_Constitution of Reforestation Area by Use` =
  union_multiple(filter_text_data(text_data, c("constitution", "reforestation", "area", "use")),
                 filter_text_data(text_data, c("용도별", "조림", "면적", "구성")),
                 filter_text_data(text_data, c("reforestation", "objective")),
                 filter_text_data(text_data, c("용도별", "조림", "면적", "구성")))




#### 🟦 조림 실적 ===================================================================================================
##### 🟪 민유림 조림 실적  ===================================================================================================
###### 🟨 민유림 조림 ==========================================================================================================
combined.list$`민유림 조림 실적_Accomplishment of reforestation in non-national forest` =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest"), 
                                  c("individual", "ownership", "sidy", "species", "owner", "seed")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적"), 
                                  c("자력", "소유", "보조", "수종", "종자")))
###### 🟨 민유림 수종별 조림 ==========================================================================================================
combined.list$`수종별 민유림 조림 실적_Accomplishment of reforestation in non-national forest by species` =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest", "species"), 
                                  c("individual", "ownership", "sidy", "owner", "seed")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적", "수종"), 
                                  c("자력", "소유", "보조", "종자")))
###### 🟨 자력 ==========================================================================================================
combined.list$`민유림자력조림실적_Accomplishment of individual reforestation in non-national forest` =
  union_multiple(filter_text_data(text_data, c("reforestation", "non-national", "forest", "individual"), "species"),
                 filter_text_data(text_data, c("민유림", "조림", "실적", "자력"), "수종별"))
###### 🟨 수종별 자력 ==========================================================================================================
combined.list$`수종별 민유림자력조림실적_Accomplishment of individual reforestation in non-national forest by species` =
  union_multiple(filter_text_data(text_data, c("reforestation", "non-national", "forest", "species", "individual")),
                 filter_text_data(text_data, c("민유림", "조림", "실적", "수종별", "자력")))
###### 🟨 수종별 보조 ==========================================================================================================
combined.list$`수종별 민유림 보조조림실적_Accomplishment of reforestation under government subsidy in non-national forest by species` =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest", "government","sidy", "species")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적", "보조", "수종")))

###### 🟨 보조조림 ==========================================================================================================
combined.list$`민유림 보조조림실적_Reforestation in Non-national Forest by Government Subsidy` =
  union_multiple(filter_text_data(text_data, 
                                  c("reforestation", "non-national", "forest", "sidy"),
                                  c("species")),
                 filter_text_data(text_data, 
                                  c("민유림", "조림", "실적", "보조"),
                                  c("수종별")))
###### 🟨 소유별 ==========================================================================================================
# combined.list$`소유별 민유림자력조림실적_Accomplishment of individual reforestation in non-national forest by onwership` =
#   union_multiple(filter_text_data(text_data, c("reforestation", "non-national", "forest", "individual", "species")),
#                  filter_text_data(text_data, c("민유림", "조림", "실적", "소유별", "자력")))


##### 🟪 국유림조림실적 ===================================================================================================
combined.list$`국유림 조림실적_Reforestation in National Forest` =
  union_multiple(filter_text_data(text_data, c("reforestation", "national", "forest"), "민유림"),
                 filter_text_data(text_data, c("국유림", "조림", "실적"), "민유림"))
##### 🟪 재원별 조림 실적 ===================================================================================================
combined.list$`재원별 조림실적_Accomplishment of Reforestation by Investments` =
  union_multiple(filter_text_data(text_data, c("accomplishment", "reforestation", "investments")),
                 filter_text_data(text_data, c("재원별", "조림", "실적")))
##### 🟪 사업별 조림 실적 ===================================================================================================
combined.list$`사업별 조림실적_Accomplishment of Reforestation by Project` =
  union_multiple(filter_text_data(text_data, c("accomplishment", "reforestation", "project")),
                 filter_text_data(text_data, c("사업별", "조림", "실적")))


##### 🟪 수종별 조림 실적  ===================================================================================================
combined.list$`수종별 조림실적_Accomplishment of Reforestation by Species` =
  union_multiple(filter_text_data(text_data, 
                                  c("accomplishment", "reforestation", "species"),
                                  "non-national"),
                 filter_text_data(text_data, 
                                  c("수종별", "조림", "실적"), 
                                  "민유림"))
##### 🟪 조림과 보호 투자 실적 ===================================================================================================
combined.list$`조림, 사방과 보호 투자실적_Investment Accomplishment of Reforestation, Erosion Control and Protection` =
  union_multiple(filter_text_data(text_data, c("investment", "accomplishment", "reforestation", "protection")),
                 filter_text_data(text_data, c("조림", "보호", "투자", "실적")))
##### 🟪 조림용 종자 채취 실적 ===================================================================================================
combined.list$`조림용 종자채취실적_Seed Collection for Plantation` =
  union_multiple(filter_text_data(text_data, 
                                  c("seed", "collection", "plantation"),
                                  c("private")),
                 filter_text_data(text_data, 
                                  c("조림용", "종자", "채취", "실적"), 
                                  "민유림"))
combined.list$`민유림조림용 종자채취실적_Seed Collection for Plantation in private forest` =
  union_multiple(filter_text_data(text_data, 
                                  c("private", "seed", "collection", "plantation")),
                 filter_text_data(text_data, 
                                  c("조림용", "종자", "채취", "실적", "민유림")))
##### 🟪 식재조림실적 ===================================================================================================
combined.list$`식재조림실적_Accomplishment of Plantation` =
  union_multiple(filter_text_data(text_data, 
                                  c("accomplishment", "plantation"),
                                  "project"),
                 filter_text_data(text_data, 
                                  c("식재", "조림", "실적"),
                                  "사업별"))
combined.list$`사업별 식재조림실적_Accomplishment of Plantation by project` =
  union_multiple(filter_text_data(text_data, 
                                  c("accomplishment", "plantation", "project")),
                 filter_text_data(text_data, 
                                  c("식재", "조림", "실적", "사업별")))
##### 🟪 소유별조림실적 ===================================================================================================
combined.list$`소유별 조림실적_Accomplishment of Reforestation by Ownership` =
  union_multiple(filter_text_data(text_data, 
                                  c("accomplishment", "reforestation", "ownership"),
                                  c("non-national", "private")),
                 filter_text_data(text_data, 
                                  c("소유별", "조림", "실적"),
                                  c("민유림")))
combined.list$`소유별 민유림 조림실적_Reforestation in non-national forest by Ownership` =
  union_multiple(filter_text_data(text_data, 
                                  c("non-national", "accomplishment", "reforestation", "ownership")),
                 filter_text_data(text_data, 
                                  c("민유림", "소유별", "조림", "실적")))

#### 🟦 생산 ===================================================================================================
##### 🟪 관상수 =========================================================================================================
combined.list$`관상수 생산실적_Ornamental Tree Production` =
  union_multiple(filter_text_data(text_data, c("ornamental", "tree", "production")),
                 filter_text_data(text_data, c("관상수", "생산", "실적")))
##### 🟪 묘목 =========================================================================================================
combined.list$`묘목생산실적_Tree Seedling Production` =
  union_multiple(filter_text_data(text_data, 
                                  c("tree", "seedling", "production"),
                                  "operation"),
                 filter_text_data(text_data, 
                                  c("묘목", "생산", "실적"), 
                                  "양묘"))





#### 🟦 자연 보호 ===================================================================================================
##### 🟪 동식물 보호 구역  ===================================================================================================
combined.list$`야생동ㆍ식물보호구역 지정현황_Status of Designated Protected Zones for Wildlife Animals and Plants` =
  union_multiple(filter_text_data(text_data, c("designated", "protected", "zones", "wildlife", "animals", "plants")),
                 filter_text_data(text_data, c("designated", "protected", "wildlife", "zone")),
                 filter_text_data(text_data, c("야생", "식물", "보호구역", "지정", "현황")))
##### 🟪 산림 보호 구역  ===================================================================================================
combined.list$`산림보호구역 지정 현황_Forest Protected Areas` =
  union_multiple(filter_text_data(text_data, c("forest", "protected", "area")),
                 filter_text_data(text_data, c("보안림", "면적")),
                 filter_text_data(text_data, c("산림", "보호구역", "지정", "현황")))
##### 🟪 보안림 면적  ===================================================================================================
combined.list$`보안림 면적_Reserved Forest Area` =
  union_multiple(filter_text_data(text_data, 
                                  c("reserved", "forest", "area"), 
                                  c("genetic", "generic")),
                 filter_text_data(text_data, 
                                  c("보안림", "면적"), 
                                  "유전"),
                 filter_text_data(text_data, 
                                  c("protection", "forest", "area"),
                                  "유전"))
##### 🟪  천연보호림 및 보호수 현황 ===================================================================================================
combined.list$`천연보호림 및 보호수 현황_Status of Natural Forest Reserve and Nurse-Tree` =
  union_multiple(filter_text_data(text_data, c("status", "natural", "forest", "reserve", "nurse-tree")),
                 filter_text_data(text_data, c("천연", "보호림","보호수", "현황")),
                 filter_text_data(text_data, c("보호수", "현황")))
##### 🟪  조수 보호구 현황 ===================================================================================================
combined.list$`조수 보호구 현황_The Status of Sanctuary` =
  union_multiple(filter_text_data(text_data, c("status", "sanctuary")),
                 filter_text_data(text_data, c("조수", "보호구")))
##### 🟪  천연기념물로 지정된 조수ㅡ ===================================================================================================
combined.list$`천연기념물로 지정된 조수_Wildlife Designated as Natural Monument` =
  union_multiple(filter_text_data(text_data, c("wildlife", "designated", "natural", "monument")),
                 filter_text_data(text_data, c("천연", "기념물", "지정", "조수")),
                 filter_text_data(text_data, c("natural", "monuments", "designated", "birds", "mammals")))
##### 🟪  천연기념물 번식지 및 도래지 ===================================================================================================
combined.list$`천연기념물의 번식지 및 도래지_Breeding and Wintering Grounds Designated as Natural Monuments` =
  union_multiple(filter_text_data(text_data, c("breeding", "wintering", "grounds", "designated", "natural", "monuments")),
                 filter_text_data(text_data, c("천연기념물", "번식지", "도래지")))
##### 🟪 야생조수 인공사육   ===================================================================================================
combined.list$`야생조수 인공사육 현황_Status of Wildlife Rearing` =
  union_multiple(filter_text_data(text_data, c("status", "wildlife", "rearing")),
                 filter_text_data(text_data, c("야생조수", "인공사육")))
##### 🟪 맹수류 사육   ===================================================================================================
combined.list$`맹수류 사육 현황_Status of Fierce Animal Rearing` =
  union_multiple(filter_text_data(text_data, c("status", "fierce", "animal", "rearing")),
                 filter_text_data(text_data, c("맹수류", "사육", "현황")))


##### 🟪 산림 복원  ===================================================================================================
combined.list$`산림복원 현황_Status of Forest Restoration` =
  union_multiple(filter_text_data(text_data, c("forest", "restoration")),
                 filter_text_data(text_data, c("산림", "복원")))


##### 🟪 산림의 타용도 전용 허가  ===================================================================================================
combined.list$`산림의 타용도 전용허가 현황_Status of Forest Land Conversion` =
  union_multiple(filter_text_data(text_data, c("forest", "land", "conversion")),
                 filter_text_data(text_data, c("산림", "전용", "허가")))
##### 🟪 산림 형질 변경 허가   ===================================================================================================
combined.list$`산림 형질변경 허가 상황_Permission for Conversion of Forest to Other Uses` =
  union_multiple(filter_text_data(text_data, c("permission", "conversion", "forest", "other", "uses")),
                 filter_text_data(text_data, c("산림", "형질", "변경", "허가")))
##### 🟪 산지일시 사용 허가  ===================================================================================================
combined.list$`산지일시사용허가 · 신고 현황_Status of Temporary Forest Land Conversion Permit · Report` =
  union_multiple(filter_text_data(text_data, c("temporary", "forest", "land", "conversion", "permit", "report")),
                 filter_text_data(text_data, c("산지", "일시", "사용", "허가", "신고")))




#### 🟦 휴양림 ===================================================================================================
##### 🟪 조성현황   ===================================================================================================
combined.list$`휴양림 조성현황_Status of Recreational Forest` =
  union_multiple(filter_text_data(text_data, c("status", "recreational", "forest")),
                 filter_text_data(text_data, c("휴양림", "조성", "현황")))
##### 🟪 이용 현황   ===================================================================================================
combined.list$`휴양림 이용현황_Visitors to Recreational Forests` =
  union_multiple(filter_text_data(text_data, c("visitors", "recreational", "forests")),
                 filter_text_data(text_data, c("휴양림", "이용", "현황")))




#### 🟦 피해 ===================================================================================================
##### 🟪 산림 피해지 벌채  ===================================================================================================
combined.list$`산림피해지 벌채현황_Area and Volume of Damages` =
  union_multiple(filter_text_data(text_data, c("area", "volume", "damages")),
                 filter_text_data(text_data, c("산림", "피해지", "벌채")))
##### 🟪 불법 산림 훼손 피해  ===================================================================================================
combined.list$`불법 산림훼손 피해현황_Damages from Illegal Forest Activities` =
  union_multiple(filter_text_data(text_data, c("damages", "illegal", "forest", "activities")),
                 filter_text_data(text_data, c("불법", "산림", "훼손", "피해")))


##### 🟪 동물 기상 산림 피해 ===================================================================================================
###### 🟨 면적 구성 ===================================================================================================
combined.list$`동물 및 기상적 산림피해발생 면적의 구성_Proportion of Forest Damage by Animal and Meteorological Cause` =
  union_multiple(filter_text_data(text_data, c("proportion", "forest", "damage", "animal", "meteorological", "cause")),
                 filter_text_data(text_data, c("동물", "기상적", "산림", "피해", "발생", "면적", "구성")))
###### 🟨 방제 상황 ===================================================================================================
combined.list$`동물 및 기상적 산림피해발생 및 방제상황_Prevention of Forest Damage by Animal and Meteorological Cause` =
  union_multiple(filter_text_data(text_data, c("prevention", "forest", "damage", "animal", "meteorological", "cause")),
                 filter_text_data(text_data, c("동물", "기상적", "산림", "피해", "발생", "방제", "상황")),
                 filter_text_data(text_data, c("동물", "기상적", "산림", "피해", "발생", "상황")))


##### 🟪 산화/산불 피해 상황 ===================================================================================================
combined.list$`산화/산불피해 상황_Status of Forest Fire Damage` =
  union_multiple(filter_text_data(text_data, c("status", "forest", "fire", "damage")),
                 filter_text_data(text_data, c("산화", "피해")),
                 filter_text_data(text_data, c("산불", "피해")),
                 filter_text_data(text_data, c("damage", "forest", "fires")))
##### 🟪 산림 훼손 허가 상황 ===================================================================================================
combined.list$`산림 훼손 허가 상황_Permission Status of Forest Exploitation` =
  union_multiple(filter_text_data(text_data, c("permission", "status", "forest", "exploitation")),
                 filter_text_data(text_data, c("산림", "훼손", "허가", "상황")))
##### 🟪 인위적 산림피해상황 ===================================================================================================
combined.list$`인위적 산림피해상황_Status of Artificial Forest Damage` =
  union_multiple(filter_text_data(text_data, c("status", "artificial", "forest", "damage")),
                 filter_text_data(text_data, c("인위적", "산림", "피해")))




##### 🟪 병해충 ===================================================================================================
###### 🟨 자재 소비 상황 ===================================================================================================
combined.list$`산림병해충방제 자재 소비상황_Consumption of Pesticide for Prevention and Control of Forest Disease and Pest` =
  union_multiple(filter_text_data(text_data, c("consumption", "pesticide", "prevention", "control", "forest", "disease", "pest")),
                 filter_text_data(text_data, c("산림", "해충", "자재", "소비")),
                 filter_text_data(text_data, c("방제", "자재", "소비")))
###### 🟨 발생 및 방제 상황 ===================================================================================================
combined.list$`산림병해충 발생 및 방제상황_5. Forest Damage Occurrence and Prevention by Forest Pest Insect and Disease` =
  union_multiple(filter_text_data(text_data, 
                                  c("disease", "prevention", "forest", "disease", "pest"), 
                                  c("소비", "자재")),
                 filter_text_data(text_data, 
                                  c("산림", "해충", "방제", "상황"), 
                                  c("소비", "자재")),
                 filter_text_data(text_data, 
                                  c("forest", "damage", "occurrence", "prevention", "insect", "disease"),  
                                  c("소비", "자재")),
                 filter_text_data(text_data, c("산림", "병해충", "발생", "방제"),  
                                  c("소비", "자재")),
                 filter_text_data(text_data, c("산림", "해충", "발생", "방제"),  
                                  c("소비", "자재")))

#### 🟦 수렵 ===================================================================================================
combined.list$`사냥터운영상황_Operation of Hunting Ground` =
  union_multiple(filter_text_data(text_data, c("operation", "hunting", "ground")),
                 filter_text_data(text_data, c("사냥터", "운영")),
                 filter_text_data(text_data, c("수렵장", "운영")))


#### 🟦 사유림 협업 경영 사업 ===================================================================================================
combined.list$`사유림 협업 경영 사업_Activities of Private Forest Cooperatives` =
  union_multiple(filter_text_data(text_data, c("activities", "private", "forest", "cooperatives")),
                 filter_text_data(text_data, c("사유림", "협업", "경영", "사업")))




#### 🟦 독림가 ===================================================================================================
combined.list$`독림가 현황_Details of Sincere Forest Manager` =
  union_multiple(filter_text_data(text_data, c("details", "sincere", "forest", "manager")),
                 filter_text_data(text_data, c("독림가", "현황")))
#### 🟦 사방사업실적 ===================================================================================================
combined.list$`사방사업실적_Accomplishment of Erosion Control` =
  union_multiple(filter_text_data(text_data, c("accomplishment", "erosion", "control")),
                 filter_text_data(text_data, c("사방", "사업", "실적")),
                 filter_text_data(text_data, c("erosion", "control", "projects")))




#### 🟦 임업 노동력 ===================================================================================================
combined.list$`임업 노동력 현황_Number of Forestry Workers Engaged in Forestry` =
  union_multiple(filter_text_data(text_data, c("number", "forestry", "workers", "engaged", "forestry")),
                 filter_text_data(text_data, c("임업", "노동력", "현황")))
#### 🟦 임도시설 ===================================================================================================
combined.list$`임도시설 현황_Status of Forest Road` =
  union_multiple(filter_text_data(text_data, c("status", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설", "현황")))
#### 🟦 수묘표 ===================================================================================================
combined.list$`수묘표_Nursery Practice` =
  union_multiple(filter_text_data(text_data, c("nursery", "practice")),
                 filter_text_data(text_data, c("수묘", "표")))
#### 🟦 영림단 조직 ===================================================================================================
combined.list$`영림단 조직 현황_Units of Forest Craft Workers` =
  union_multiple(filter_text_data(text_data, c("units", "forest", "craft", "workers")),
                 filter_text_data(text_data, c("영림단", "조직")))
#### 🟦 양묘 시업 ===================================================================================================
combined.list$`양묘시업상황_Operation of Tree Seeding Production` =
  union_multiple(filter_text_data(text_data, c("operation", "tree", "seeding", "production")),
                 filter_text_data(text_data, c("양묘", "시업", "상황")))





### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
filtered_data <- unlist(combined.list)
unfiltered_data <- setdiff(text_data, filtered_data)

k=10
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


### 🟩 중복원소 확인 =====================================================================================================
find_duplicates(combined.list)


### 🟩 save the results =====================================================================================================
results.list[[i]] = combined.list
names(results.list)[i] = paste0(i, ".", L2_categories[i])



# length(L2_categories)
# 
# 
# ## 🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥🟥=======================================================================
# ## 🟧 12.산림자원 (Forest Resources) #######################################################################################################################
# ### 🟩 extract the data =====================================================================================================
# i=18
# L2_categories[i] %>% cat
# text_data = data %>% filter(Categorized_L2 == L2_categories[i]) %>% pull(L3) %>% table %>% names
# length(text_data)
# 
# 
# ### 🟩 텍스트 클러스터링 =====================================================================================================
# k=20
# clustered_data = text_clustering(text_data, k_min = k, k_max = k)
# clustered_data$clusters
# 
# 
# 
# ### 🟩 텍스트 필터링 해서 그룹화 =====================================================================================================
# combined.list <- list()
# 
# 
# combined.list$`산지이용 구분 현황_Status of Forest Land Use Classification` =
#   union_multiple(filter_text_data(text_data, c("forest", "land", "use", "classification")),
#                  filter_text_data(text_data, c("산지", "이용", "구분", "현황")))
# 
# 
# combined.list$`산지이용 구분 현황_Status of Forest Land Use Classification` =
#   union_multiple(filter_text_data(text_data, c("forest", "land", "use", "classification")),
#                  filter_text_data(text_data, c("산지", "이용", "구분", "현황")))
# 
# combined.list$`산림의 타용도별 감소현황_Status of Forest Land Use Change` =
#   union_multiple(filter_text_data(text_data, c("forest", "land", "use", "change")),
#                  filter_text_data(text_data, c("산림", "타용도", "감소", "현황")))
# 
# combined.list$`산지일시사용허가 · 신고 현황_Status of Temporary Forest Land Conversion Permit · Report` =
#   union_multiple(filter_text_data(text_data, c("temporary", "forest", "land", "conversion", "permit", "report")),
#                  filter_text_data(text_data, c("산지", "일시", "사용", "허가", "신고", "현황")))
# 
# combined.list$`소유별 산림면적 및 산림률_Forest Land Area and % of Land per Hectare by Ownership` =
#   union_multiple(filter_text_data(text_data, c("forest", "land", "area", "ownership", "percentage")),
#                  filter_text_data(text_data, c("소유", "산림", "면적", "산림률")))
# 
# combined.list$`산림부문 온실가스 흡수량_GHG Removals in the Forestry Sector` =
#   union_multiple(filter_text_data(text_data, c("GHG", "removals", "forestry", "sector")),
#                  filter_text_data(text_data, c("산림", "온실가스", "흡수량")))
# 
# 
# 
# 
# # 산림면적 및 임목축적 
# ## 임상별, 지종별, 영급별
# combined.list$`임상별ㆍ지종별ㆍ영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Forest Types, Land Classes and Age Classes` =
#   union_multiple(filter_text_data(text_data, c("forest type", "land class", "age class", "land area")),
#                  filter_text_data(text_data, c("임상별", "지종별", "영급별", "임목", "축적")))
# ## 임상별 영급별 (-지종별, -산지구분별)
# combined.list$`임상별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Forest Types and Age Classes` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("forest type", "age classe", "forest area", "growing stock"), 
#                                   c("land class", "land classification")),
#                  filter_text_data(text_data, 
#                                   c("임상별", "영급별", "산림", "면적", "임목", "축적"), 
#                                   c("지종별", "산지구분별")))
# ## 관리기관별, 임상별
# combined.list$`산림청소관 국유림 관리기관별, 임상별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Forest Type and Forest Management offices under Korea Forest Service` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("Forest Management offices", "forest type", "land area", "forest type"), 
#                                   c("land classification")),
#                  filter_text_data(text_data, 
#                                   c("관리기관별", "임상별", "국유림", "면적", "산림"), 
#                                   c("산지구분별")))
# ## 관리기관별,영급별
# combined.list$`관리기관별 영급별 산림면적ㆍ임목축적_Forest Land Area And Growing Stock By Management Agencies And Age Class` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("age class", "Forest Management offices", "forest land area", "growing stock", "management agencies")),
#                  filter_text_data(text_data, 
#                                   c("영급별", "관리기관별", "산림", "면적", "임목", "축적")))
# ## 소유별 
# combined.list$`소유별 산림면적 및 산림율_Forest Land Area and % of Land Area by Ownership` =
#   union_multiple(filter_text_data(text_data, c("Forest Land Area", "Ownership")),
#                  filter_text_data(text_data, c("Forest Area", "Ownership", "%")),
#                  filter_text_data(text_data, c("소유별", "산림", "면적", "산림율")))
# combined.list$`소유별 임목축적 및 ha당 평균축적_Forest Growing Stock by Ownership` =
#   union_multiple(filter_text_data(text_data, c("forest growing stock", "ownership", "ha")),
#                  filter_text_data(text_data, c("소유별", "임목", "축적", "ha당", "평균", "축적")),
#                  filter_text_data(text_data, c("소유별", "임목", "축적", "축적")))
# 
# ## 수종별
# combined.list$`주요 수종별 산림면적 및 임목축적 현황_Forest Land Area and Growing Stock by Tree Species` =
#   union_multiple(filter_text_data(text_data, c("tree", "species", "forest land area", "growing stock")),
#                  filter_text_data(text_data, c("주요", "수종별", "산림", "면적", "임목", "축적")))
# ## 임상별, 산지구분별, 영급별
# combined.list$`임상별 · 산지구분별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Forest Types, Forest Land Classifications and Age Classes` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("land classification", "forest type", "age classe", "forest area", "growing stock"), 
#                                   c("land class")),
#                  filter_text_data(text_data, 
#                                   c("산지구분별", "임상별", "영급별", "산림", "면적", "임목", "축적"), 
#                                   c("지종별")))
# ## 연도별, 임상별
# combined.list$`연도별·임상별 산림면적 및 임목축적_Forest Land Area, Growing Stock by Year and Forest Types` =
#   union_multiple(filter_text_data(text_data, c("year", "forest type","forest land area", "growing stock")),
#                  filter_text_data(text_data, c("연도별", "임상별", "산림", "면적", "임목", "축적")))
# ## 연도별 (-임상별, -기관별)
# combined.list$`연도별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Year` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("year", "forest land area", "growing stock"),
#                                   c("forest type", "Management Agencies")),
#                  filter_text_data(text_data, 
#                                   c("연도별", "산림", "면적", "임목", "축적"),
#                                   c("임상별", "기관별")))
# ## 연도별,기관별 (-임상별)
# combined.list$`연도별 기관별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Year and Management Agencies` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("year", "Management Agencies", "forest land area", "growing stock"),
#                                   c("forest type")),
#                  filter_text_data(text_data, 
#                                   c("연도별", "기관별", "산림", "면적", "임목", "축적"),
#                                   c("임상별")))
# ## 행적 구역별 (-임상별)
# combined.list$`행정구역별 산림면적 및 임목축적_Forest Land Area And Growing Stock By Administrative Districts` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("forest land area", "growing stock", "administrative districts"),
#                                   c("forest type")),
#                  filter_text_data(text_data, 
#                                   c("행정구역별", "산림", "면적", "임목", "축적"),
#                                   c("임상별")))
# ## 행적 구역별 + 임상별
# combined.list$`행정구역별 임상별 영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Administrative Districts, Forest Type and Age Class` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("forest type","forest land area", "growing stock", "administrative districts")),
#                  filter_text_data(text_data, 
#                                   c("임상별", "행정구역별", "산림", "면적", "임목", "축적")))
# ## 지종별, 영급별
# combined.list$`지종별ㆍ영급별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Land Class and Age Class` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("land class", "age class", "land area"),
#                                   c("forest type", "Land Classifications")),
#                  filter_text_data(text_data, 
#                                   c("지종별", "영급별", "임목", "축적"),
#                                   c("임상별", "산지구분별")))
# ## 산지구분별, 영급별
# combined.list$`산지구분별·영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Provinces, Forest Land Classifications and Age Classes` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("Land Classifications", "age class", "land area"),
#                                   c("forest type")),
#                  filter_text_data(text_data, 
#                                   c("산지구분별", "영급별", "임목", "축적"),
#                                   c("임상별", "지종별")))
# ## 지역별
# combined.list$`지역별 산림면적 및 축적_Forest Area and Growing Stock by Province` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("forest area", "growing stock", "province"),
#                                   c("age class", "land classification")),
#                  filter_text_data(text_data, 
#                                   c("지역별", "산림", "면적", "축적"), 
#                                   c("영급별", "산지구분별")))
# # 산림청소관 국유림 관리기관별, 지종별
# combined.list$`산림청소관 국유림 관리기관별, 지종별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Land Class and Forest Management Offices under Korea Forest Service` =
#   union_multiple(filter_text_data(text_data, c("forest land area", "growing stock", "land class", "forest management office", "Korea Forest Service")),
#                  filter_text_data(text_data, c("산림청", "국유림", "관리기관별", "지종별", "산림면적", "임목축적")))
# # 산림청소관 국유림 관리기관별, 임상별
# combined.list$`산림청 소관 국유림, 임상별 산림면적 및 임목축적_Forest Area and Growing Stock of National Forests under the Jurisdiction of Korea Forest Service by Forest Types` =
#   union_multiple(filter_text_data(text_data, c("forest land area", "growing stock", "Forest Type", "forest management office", "Korea Forest Service")),
#                  filter_text_data(text_data, c("산림청", "국유림", "관리기관별", "임상별", "산림면적", "임목축적")))
# # 산림청소관 국유림 관리기관별, 산지구분별
# combined.list$`산림청소관 국유림 관리기관별, 임상별 산림면적 및 임목축적_9. Forest Land Area and Growing Stock by Forest Type and Forest Management offices under Korea Forest Service` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("forest land area", "growing stock", "Forest land classification", "Korea Forest Service", "Forest Management office"),
#                                   c("Age Classes")),
#                  filter_text_data(text_data, 
#                                   c("산림청", "국유림", "산지구분별", "산림면적", "임목축적"),
#                                   c("영급별")))
# # 산림청 소관 국유림 지종별 (-산지구분별, -영급별)
# combined.list$`산림청 소관 국유림, 지종별 산림면적 및 임목축적_Forest Land Area and Growing Stock of National Forests under Korea Forest Service by Land Classes` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("forest", "land", "area", "growing", "stock", "national", "forests", "Korea", "Forest", "Service", "land", "classes"),
#                                   c("Forest Type","Forest Land Classification", "Age class")),
#                  filter_text_data(text_data, 
#                                   c("산림청", "소관", "국유림", "지종별", "산림면적", "임목축적"),
#                                   c("산지구분별", "영급별", "임상별")))
# # 산림청 소관 국유림 임상별 (-산지구분별, -영급별, -관리기관별)
# combined.list$`산림청 소관 국유림, 임상별 산림면적 및 임목축적_Forest Area and Growing Stock of National Forests under the Jurisdiction of Korea Forest Service by Forest Types` =
#   union_multiple(filter_text_data(text_data, 
#                                   c("forest", "area", "growing", "stock", "national", "forests", "jurisdiction", "Korea", "Forest", "Service", "forest type"),
#                                   c("Forest Management Office")),
#                  filter_text_data(text_data, 
#                                   c("산림청", "소관", "국유림", "임상별", "산림면적", "임목축적"),
#                                   c("관리기관별")))
# 
# 
# 
# 
# # 
# combined.list$`인공 조림지 현황_Area of plantation forests` =
#   union_multiple(filter_text_data(text_data, c("area", "plantation", "forests")),
#                  filter_text_data(text_data, c("인공", "조림지", "현황")))
# 
# combined.list$`소유별 조림면적_Plantation forest area by ownership` =
#   union_multiple(filter_text_data(text_data, c("plantation", "forest", "area", "ownership")),
#                  filter_text_data(text_data, c("소유별", "조림", "면적")))
# 
# combined.list$`숲 가꾸기_Forest tending` =
#   union_multiple(filter_text_data(text_data, c("forest", "tending")),
#                  filter_text_data(text_data, c("숲", "가꾸기")))
# 
# 
# 
# # 조림 실적
# ## 재원별
# combined.list$`재원별 조림실적_Planted area by financial sources` =
#   union_multiple(filter_text_data(text_data, c("planted", "area", "financial", "sources")),
#                  filter_text_data(text_data, c("재원별", "조림", "실적")))
# ## 수종별 
# combined.list$`수종별 조림실적_Plantation forest by tree species` =
#   union_multiple(filter_text_data(text_data, c("plantation", "forest", "tree", "species")),
#                  filter_text_data(text_data, c("수종별", "조림", "실적")))
# 
# 
# # 
# combined.list$`양묘사업 및 생산현황_Status of seedling plantation and production` =
#   union_multiple(filter_text_data(text_data, c("status", "seedling", "plantation", "production")),
#                  filter_text_data(text_data, c("양묘사업", "생산", "현황")))
# 
# 
# 
# # 
# combined.list$`조림 활착상황_Survival rate of reforestation` =
#   union_multiple(filter_text_data(text_data, c("survival", "rate", "reforestation")),
#                  filter_text_data(text_data, c("조림", "활착", "상황")))
# 
# combined.list$`산지 이용 구분 현황_Status of Forest Land Classification` =
#   union_multiple(filter_text_data(text_data, c("forest", "land", "classification"), c("growing stock")),
#                  filter_text_data(text_data, c("산지", "이용", "구분", "현황"), c("축적")))
# 
# combined.list$`사회공헌형 산림탄소상쇄사업 현황_Status of Forest Carbon Offset Projects - Social Contribution Type` =
#   union_multiple(filter_text_data(text_data, c("forest", "carbon", "offset", "projects", "social", "contribution")),
#                  filter_text_data(text_data, c("사회공헌형", "산림", "탄소", "상쇄", "사업", "현황")))
# 
# combined.list$`국유재산 취득 및 처분 현황_Acquisition and Disposal of National Forest` =
#   union_multiple(filter_text_data(text_data, c("acquisition", "disposal", "national", "forest")),
#                  filter_text_data(text_data, c("국유재산", "취득", "처분", "현황")))
# 
# 
# 
# # 대부/사용허가
# combined.list$`indisposable 국유림 사용허가 현황_Status of Lease Permission on Indispensable National Forest` =
#   union_multiple(filter_text_data(text_data, c("lease", "permission", "indispensable", "national", "forest")),
#                  filter_text_data(text_data, c("국유림", "사용", "허가"), c("dispensable")),
#                  filter_text_data(text_data, c("국유림", "사용", "허가"), c("disposable")))
# 
# combined.list$`disposable 국유림 대부현황_Status of Lease Permission on Disposable National Forest` =
#   union_multiple(filter_text_data(text_data, c("lease", "permission", "disposable", "national", "forest"), c("indispensable")),
#                  filter_text_data(text_data, c("lease", "permission", "disposable", "national", "forest"), c("indisposable")),
#                  filter_text_data(text_data, c("국유림", "대부", "현황"), c("indispensable", "indisposable")),
#                  filter_text_data(text_data, c("국유림", "사용", "허가"), c("indispensable", "indisposable")))
# 
# 
# # 전용
# combined.list$`산림의 타용도 전용현황_Status of Forest Land Conversion` =
#   union_multiple(filter_text_data(text_data, c("forest", "land", "conversion")),
#                  filter_text_data(text_data, c("산림", "타용도", "전용", "현황")))
# 
# 
# # 분수림
# combined.list$`국유림 분수림 현황_Status of Profit-sharing on Disposable National Forest` =
#   union_multiple(filter_text_data(text_data, c("profit-sharing", "disposable", "national", "forest")),
#                  filter_text_data(text_data, c("국유림", "분수림", "현황", "disposable")))
# 
# 
# 
# 
# 
# ### 🟩 필터링되지 않은 원소 확인 =====================================================================================================
# filtered_data <- unlist(combined.list)
# unfiltered_data <- setdiff(text_data, filtered_data)
# 
# k=10
# # 에러 발생 시 print(unfiltered_data)를 실행
# tryCatch({
#   # 클러스터링 함수 실행
#   clustering_result <- text_clustering(unfiltered_data, k_min = k, k_max = k)
#   print(clustering_result$cluster)  # 클러스터링 결과 출력
# }, error = function(e) {
#   # 에러가 발생하면 unfiltered_data를 출력
#   print(unfiltered_data)
# })
# 
# if (length(unfiltered_data) > 0) {
#   cat("필터링되지 않은 원소가 존재합니다:\n")
#   print(unfiltered_data)
# } else {
#   cat("모든 원소가 필터링되었습니다.\n")
# }
# 
# 
# ### 🟩 중복원소 확인 =====================================================================================================
# find_duplicates(combined.list)
# 
# 
# ### 🟩 save the results =====================================================================================================
# results.list[[i]] = combined.list
# names(results.list)[i] = paste0(i, ".", L2_categories[i])
# 


















































































# 🟥 Export the results #####################################################################################################
path_save = "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/DataAnalysis/KFS_Timeseries/rearranged data"
saveRDS(results.list, paste0(path_save, "/3.L3 Categorized data.rds"))







