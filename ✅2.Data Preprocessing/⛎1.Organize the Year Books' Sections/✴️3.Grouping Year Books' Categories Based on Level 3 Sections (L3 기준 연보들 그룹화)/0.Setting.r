# rm(list=ls())


# 🟥 Load Functions & Packages ##########################################################################
## 🟨Install and loading Packages ================================
# rm(list = ls())
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



# 🟥 함수 정의 ##########################################################################
###  🟨 영어 알파벳을 제거하는 함수 =======================================================================
remove_english_letters <- function(strings) {
  # 정규 표현식을 사용하여 영어 알파벳을 제거
  return(gsub("[a-zA-Z]", "", strings))
}

###  🟨 한글 이외의 문자를 제거하는 함수 =======================================================================
remove_non_korean_characters <- function(strings) {
  # 정규 표현식을 사용하여 한글을 제외한 모든 문자 제거
  return(gsub("[^가-힣]", "", strings))
}


###  🟨 공백을 제거하는 함수 =======================================================================
remove_whitespace <- function(strings) {
  # 정규 표현식을 사용하여 공백 제거
  return(gsub("\\s", "", strings))
}


###  🟨 슬래시를 대시로 바꾸는 함수 =======================================================================
replace_slash_with_dash <- function(strings) {
  # 정규 표현식을 사용하여 슬래시를 대시로 대체
  return(gsub("/", "-", strings))
}



###  🟨 문자열  ====================================================================================
fit_length = function(x.vec, fit.num){
  if(class(x.vec)=="numeric"){
    x.vec = as.character(x.vec)
  }
  
  New_x.vec = sapply(x.vec, function(y){
    if(nchar(y)>fit.num){
      stop("fit.num should larger!")
    }else{
      while(nchar(y) != fit.num){
        y = paste("0", y, collapse = "", sep = "")
      }
      return(y)
    }
  })
  
  return(New_x.vec)
}





###  🟨 "_NA" 문자열을 제거하는 함수 =======================================================================
remove_na_suffix <- function(strings) {
  # 정규 표현식을 사용하여 "_NA" 제거
  return(gsub("_NA", "", strings))
}



###  🟨 콜론을 =로 바꾸는 함수 =======================================================================
replace_colon_with_underscore <- function(strings) {
  # 정규 표현식을 사용하여 콜론을 언더스코어로 대체
  return(gsub(":", "=", strings))
}


###  🟨 "단위-"를 제거하는 함수 =======================================================================
remove_unit_prefix <- function(strings) {
  # 정규 표현식을 사용하여 "단위-" 제거
  return(gsub("단위-", "", strings))
}




## 🟧 함수 정의 ================================================================================
viewer = function(data.list, element){
  elements_full = sapply(data.list, function(x){
    x[1,3] %>% unlist
  }) %>% unname
  
  
  
  selected = data.list[which(elements_full %in% element)]
  if(length(selected) == 1){
    return(selected[[1]])
  }else{
    return(selected)
  }
}

extract_id = function(data){
  
  extract_id_df = function(df){
    df[,1] %>% unlist %>% unname %>% unique  
  }
  
  if(is.data.frame(data)){
    data %>% extract_id_df %>% return
  }else if(is.list(data)){
    sapply(data, extract_id_df) %>% unlist %>% unname %>% return
  }
}


exclude_element = function(vec, ex){
  vec[!vec %in% ex]
}


check_yb = function(data, ybid){
  selected = data[names(data)%in%ybid]
  if(length(selected)==0){
    cat("no data")
  }else if(length(selected)==1){
    selected[[1]]
  }else{
    selected
  }
}


add_to_check_id <- function(element, selected) {
  if (is.null(check_id[[element]])) {
    check_id[[element]] <<- extract_id(selected)
  } else {
    check_id[[element]] <<- c(check_id[[element]], extract_id(selected))
  }
}




### 🟨 특정 열을 numeric으로 변환하는 함수 =============================================
convert_to_numeric_if_possible <- function(column) {
  numeric_column <- suppressWarnings(as.numeric(column))
  if (all(is.na(numeric_column) == is.na(column))) {
    return(numeric_column)
  } else {
    return(column)
  }
}

### 🟨 지정된 인덱스부터 특정 이전 인덱스까지의 열들을 numeric으로 변환하는 함수 =============================================
convert_columns_to_numeric <- function(df, start_col_index, end_col_index) {
  # 유효한 인덱스인지 확인합니다.
  if (start_col_index <= end_col_index) {
    # 지정된 범위의 열들을 numeric으로 변환합니다.
    for (i in start_col_index:end_col_index) {
      df[[i]] <- convert_to_numeric_if_possible(df[[i]])
    }
  } else {
    stop("시작 인덱스가 종료 인덱스보다 작거나 같아야 합니다.")
  }
  
  return(df)
}

### 🟨 에러 다음 연보 이름=============================================
get_next_name <- function(name, data_list) {
  # 입력한 이름의 인덱스 찾기
  index <- which(names(data_list) %in% name)
  
  
  # 인덱스 유효성 검사
  return(names(data_list)[1 + index])
}





# 🟥 ith_L2_filtered 데이터 프레임에 그룹 변수를 추가하는 함수 =======================================================================
add_group_variable <- function(df, groups_list) {
  df <- df %>% mutate(Categorized_L3 = NA)  # 새로운 그룹 변수를 NA로 초기화
  
  for(group_name in names(groups_list)) {
    df <- df %>% mutate(Categorized_L3 = ifelse(NAME_L3 %in% groups_list[[group_name]], group_name, Categorized_L3))
  }
  
  df = df %>% relocate(Categorized_L3) %>% arrange(Categorized_L3)
  
  return(df)
}


# 🟥 수치 데이터를 추출하고 파일로 저장하는 함수 =======================================================================
process_and_export <- function(df, path = ".") {
  # df = data[[ind+1]]
  # View(df)
  # 공통 열 이름
  common_cols <- c("Categorized_L3", "year", "ID", "unit_L2", "unit_L3", "unit_L4", "unit_L5")
  
  # 공통 열에서 유일한 값을 추출
  categorized_L3 <- unique(df$Categorized_L3) %>% remove_non_korean_characters 
  year <- unique(df$year)
  id <- unique(df$ID)
  
  # View(df)
  
  # 특수한 L3는 L4로
  # "구ㆍ시ㆍ군편_Details by GuㆍSiㆍGun"
  # "가. 전국 및 시ㆍ도ㆍ서편_Details by country province city and N.F.S."
  if(grepl("전국", categorized_L3) | grepl("군편", categorized_L3)){
    categorized_L3 <- unique(df$Categorized_L4) %>% remove_non_korean_characters 
  }
  
  
  
  
  ### 🟧 unit 값 설정 =========================================================================
  # unit 값 추출 (NA가 아닌 값을 선택)
  unit_L2 <- unique(na.omit(df$unit_L2))
  unit_L3 <- unique(na.omit(df$unit_L3))
  unit_L4 <- unique(na.omit(df$unit_L4))
  unit_L5 <- unique(na.omit(df$unit_L5))
  # View(df)
  
  
  # 우선순위대로 유닛 값을 설정
  unit <- ifelse(length(unit_L2) > 0, unit_L2,
                 ifelse(length(unit_L3) > 0, unit_L3,
                        ifelse(length(unit_L4) > 0, unit_L4, unit_L5))) %>% 
    remove_whitespace %>% 
    replace_slash_with_dash %>% 
    replace_colon_with_underscore %>% 
    remove_unit_prefix %>% 
    as.character
  
  
  
  
  
  ### 🟧 수치 데이터 열의 인덱스 계산=========================================================================
  start_col <- which(names(df) == "행") + 2
  end_col <- which(names(df) == "Categorized_New") - 1
  
  
  ### 🟧 numeric 변환 =========================================================================
  df = convert_columns_to_numeric(df, start_col_index = 4, end_col_index = end_col)
  
  
  
  ### 🟧 마지막 열 확인 =========================================================================
  if(is.character(df[,end_col])){
    end_col = end_col - 1
  }
  
  
  
  
  
  ### 🟧 새로운 경로 =========================================================================
  path_new = paste0(path, "/", categorized_L3, "/", year)
  dir.create(path_new, showWarnings = F, recursive = T)
  
  
  
  # 수치 데이터를 반복하여 파일로 저장
  error_log <- list()
  tictoc::tic()
  for (i in 1:nrow(df)) {
    for (j in start_col:end_col) {
      
      value <- df[i, j] %>% unlist
      
      # NA는 0으로
      if(is.na(value)){
        value = 0
      }
      
      # 파일 이름 생성
      file_name <- sprintf("%s_%s___%s___%s___%s___%s.csv",
                           year,
                           categorized_L3,
                           names(df)[j] %>% remove_na_suffix %>% replace_slash_with_dash %>% remove_non_korean_characters ,
                           df[i, start_col - 1] %>% remove_non_korean_characters ,
                           unit,
                           id)
      
      
      
      # 파일 저장 경로
      file_path <- file.path(path_new, file_name)
      
      # 파일로 저장
      tryCatch({
        write_csv(data.frame(Value = value), file_path)
      }, error = function(e) {
        error_log[[length(error_log) + 1]] <- list(file_path = file_path, error = e$message)
      })
      
      
    }
  }
  cat("\n", crayon::green("Exported : "), crayon::bgMagenta(id),"\n")
  tictoc::toc()
  return(error_log)
}


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


## 🟧  NULL 원소 위치를 추출하는 함수 정의 ==================================================================
find_null_positions <- function(lst) {
  # NULL 위치를 저장할 벡터 초기화
  null_positions <- which(sapply(lst, is.null))
  
  # NULL 위치 벡터 반환
  return(null_positions)
}


## 🟧  길이가 0인 원소의 위치를 찾고 해당 원소 이름을 반환하는 함수 정의 ==================================================================
find_empty_names <- function(lst) {
  # 길이가 0인 원소의 위치를 저장할 벡터 초기화
  empty_positions <- which(sapply(lst, function(x) length(x) == 0))
  
  # 해당 위치의 리스트 원소 이름을 반환
  empty_names <- names(lst)[empty_positions]
  
  return(empty_names)
}


# 🟥 모든 문자열을 포함하는 행 추출  ========================================================================================================
extract_strings <- function(data, column_name, string_vector) {
  # 데이터 프레임의 해당 열 추출
  column_data <- data[[column_name]]
  
  # 모든 문자열을 포함하는 행 추출
  result <- column_data[sapply(column_data, function(x) all(sapply(string_vector, function(y) grepl(y, x))))]
  
  return(result)
}



# 🟥 조건 만족 문자열 추출  ========================================================================================================
filter_values <- function(df, col_name, include = NULL, exclude = NULL) {
  if (!col_name %in% colnames(df)) {
    stop("The specified column does not exist in the dataframe.")
  }
  
  # Extract the column as a vector
  column_vector <- df[[col_name]]
  
  # Include filter
  include_filter <- rep(TRUE, length(column_vector))
  if (!is.null(include)) {
    for (pattern in include) {
      include_filter <- include_filter & grepl(pattern, column_vector)
    }
  }
  
  # Exclude filter
  exclude_filter <- rep(TRUE, length(column_vector))
  if (!is.null(exclude)) {
    for (pattern in exclude) {
      exclude_filter <- exclude_filter & !grepl(pattern, column_vector)
    }
  }
  
  # Combine filters
  final_filter <- include_filter & exclude_filter
  
  # Extract unique values that match the filter
  unique_values <- unique(column_vector[final_filter])
  
  if (length(unique_values) == 0) {
    message("No values meet the criteria.")
    return(NULL)
  }
  
  return(unique_values)
}



# 🟥 클러스터링 함수  ========================================================================================================

library(tm)
library(cluster)
library(factoextra)

text_clustering <- function(text_data, k_min = 2, k_max = NULL) {
  # 전처리된 텍스트 데이터를 저장할 벡터 생성
  cleaned_vector <- text_data
  
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
  
  # k_max가 NULL이면 k_min과 동일하게 설정
  if (is.null(k_max)) {
    k_max <- k_min
  }
  
  # 데이터 포인트의 수가 클러스터 수보다 많은지 확인
  num_data_points <- nrow(tdm_matrix)
  if (k_max > num_data_points) {
    k_max <- num_data_points
  }
  
  if (k_min > num_data_points) {
    stop("k_min is greater than the number of distinct data points.")
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
