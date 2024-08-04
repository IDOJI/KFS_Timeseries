
plot_time_series_ggplot <- function(time_series) {
  # 입력 데이터의 유효성 검사
  if (!is.numeric(time_series)) {
    stop("Input must be a numeric vector")
  }
  
  # 데이터 프레임 생성
  data_frame <- data.frame(Time = seq_along(time_series), Value = time_series)
  
  # ggplot을 사용한 시계열 플롯 생성
  p <- ggplot(data_frame, aes(x = Time, y = Value)) +
    geom_line(color = "blue", size = 1) +  # 선 그래프 추가
    geom_point(color = "red", size = 2) +  # 포인트 추가
    labs(title = "Time Series Plot", x = "Time", y = "Value") +
    theme_minimal() +  # 미니멀 테마 사용
    theme(plot.title = element_text(hjust = 0.5))  # 제목 가운데 정렬
  
  # 그래프 출력
  print(p)
}

add_prefix_to_columns <- function(df, target_string, prefix) {
  # 열 이름을 가져오기
  column_names <- colnames(df)
  
  # target_string을 포함하는 열 이름 인덱스 찾기
  target_indices <- grep(target_string, column_names)
  
  # 해당 열 이름들 앞에 prefix 추가
  new_column_names <- column_names
  new_column_names[target_indices] <- paste0(prefix, column_names[target_indices])
  
  # 데이터프레임의 열 이름을 업데이트
  colnames(df) <- new_column_names
  
  return(df)
}



rename_column_in_list_korean_pine <- function(conifer_list) {
  renamed_list <- lapply(conifer_list, function(df) {
    # 열 이름이 "짓나무_면적"인 경우 "잣나무_면적"으로 변경
    colnames(df) <- gsub("^짓나무_면적$", "잣나무_면적", colnames(df))
    return(df)
  })
  return(renamed_list)
}




get_unique_column_names <- function(conifer_list, col_position) {
  unique_names <- sapply(conifer_list, function(x) {
    if(col_position <= ncol(x)) {
      return(names(x)[col_position])
    } else {
      return(NA)
    }
  }) %>% unique
  return(unique_names[!is.na(unique_names)])
}

rearrange_columns <- function(df) {
  # 열 이름 가져오기
  column_names <- colnames(df)
  
  # "삼나무_"로 시작하는 열과 "리기다_"로 시작하는 열의 인덱스 구하기
  samnamu_indices <- grep("^삼나무_", column_names)
  rigida_indices <- grep("^리기다_", column_names)
  
  if (length(samnamu_indices) == 0 || length(rigida_indices) == 0) {
    # "삼나무_" 또는 "리기다_"로 시작하는 열이 없으면 데이터프레임 반환
    return(df)
  }
  
  # "리기다_"로 시작하는 마지막 열 인덱스 구하기
  last_rigida_index <- max(rigida_indices)
  
  # 새로운 열 순서 생성
  new_order <- c(
    setdiff(seq_len(last_rigida_index), samnamu_indices),
    samnamu_indices,
    setdiff((last_rigida_index + 1):ncol(df), samnamu_indices)
  )
  
  # 열 순서 재정렬
  rearranged_df <- df[, new_order]
  
  return(rearranged_df)
}

library(dplyr)

# 두 열을 합치는 함수 정의
merge_columns <- function(df, col1, col2, new_col_name) {
  df <- df %>%
    mutate(
      !!new_col_name := case_when(
        is.na(!!sym(col1)) & !is.na(!!sym(col2)) ~ !!sym(col2),
        !is.na(!!sym(col1)) & is.na(!!sym(col2)) ~ !!sym(col1),
        is.na(!!sym(col1)) & is.na(!!sym(col2)) ~ NA,
        !!sym(col1) == !!sym(col2) ~ !!sym(col1),
        TRUE ~ stop(paste("Error: Values in columns", col1, "and", col2, "are different and both are non-NA"))
      )
    )
  return(df)
}


compare_columns <- function(df_list, start_col, end_col) {
  # 각 데이터프레임의 열 이름을 저장할 리스트 초기화
  column_names_list <- list()
  
  # 데이터프레임 리스트에서 각 데이터프레임에 대해 열 이름 추출
  for (df in df_list) {
    column_names <- colnames(df)[start_col:end_col]
    column_names_list <- append(column_names_list, list(column_names))
  }
  
  # 첫 번째 데이터프레임의 열 이름을 기준으로 동일성 판단
  reference <- column_names_list[[1]]
  identical_columns <- sapply(column_names_list, function(cols) identical(cols, reference))
  
  # 결과 메시지 출력 및 차이점 표시
  for (i in seq_along(identical_columns)) {
    if (identical_columns[i]) {
      message(sprintf("데이터프레임 %d: 지정된 열 이름이 동일합니다.", i))
    } else {
      message(sprintf("데이터프레임 %d: 지정된 열 이름이 다릅니다.", i))
      message(sprintf(" - 기준 열 이름: %s", paste(reference, collapse = ", ")))
      message(sprintf(" - 현재 열 이름: %s", paste(column_names_list[[i]], collapse = ", ")))
      
      # 다른 부분 강조 표시
      for (j in seq_along(reference)) {
        if (j > length(column_names_list[[i]]) || reference[j] != column_names_list[[i]][j]) {
          message(sprintf("   차이점: 기준 %s <-> 현재 %s", 
                          reference[j], 
                          if (j > length(column_names_list[[i]])) "없음" else column_names_list[[i]][j]))
        }
      }
    }
  }
  
  # 동일성 여부 반환
  return(identical_columns)
}


remove_na_columns <- function(df) {
  # "구분"이라는 열의 인덱스를 찾음
  indices <- which(names(df) == "구분")
  
  # 인덱스를 기반으로 열을 검사하고, 모두 NA인 경우 제거
  for (index in indices) {
    # index = 12
    if (all(is.na(df[[index]]))) {
      df[[index]] <- NULL
    }
  }
  # names(df)
  return(df)
}

# 함수 정의
rename_columns <- function(data_list) {
  # 각 데이터프레임에서 열 이름 변경
  modified_list <- lapply(data_list, function(df) {
    if (ncol(df) >= 4) {  # 데이터프레임에 열이 4개 이상 있는지 확인
      names(df)[3] <- "구분"
      names(df)[4] <- "합계_면적"
    } else {
      message("데이터프레임에 충분한 열이 없습니다.")
    }
    return(df)
  })
  
  return(modified_list)
}

# 사용 예시
# renamed_data_list <- rename_columns(broadleaf_data_by_year)
# 확인하기 위해 첫 번째 데이터프레임의 열 이름을 출력
# print(names(renamed_data_list[[1]]))

compare_column_names <- function(data_list, end_column_index) {
  # 각 데이터프레임에서 3번째 열부터 지정된 끝 열까지의 열 이름을 추출
  column_names_list <- lapply(data_list, function(df) {
    if (ncol(df) >= end_column_index) {
      names(df)[3:end_column_index]
    } else {
      NA  # 데이터프레임에 열이 부족한 경우 NA 반환
    }
  })
  
  # 열 이름 목록 확인
  print(column_names_list)
  
  # 모든 열 이름이 동일한지 확인
  # 먼저 NA가 아닌 열 이름만 선택
  valid_column_names_list <- column_names_list[!is.na(column_names_list)]
  
  # 모든 요소가 동일한지 비교
  all_equal <- all(sapply(valid_column_names_list, function(x) identical(x, valid_column_names_list[[1]])))
  
  if (all_equal) {
    cat("모든 데이터프레임의 3번째 열부터", end_column_index, "번째 열까지의 열 이름이 동일합니다.\n")
  } else {
    cat("각 데이터프레임의 3번째 열부터", end_column_index, "번째 열까지의 열 이름이 동일하지 않습니다.\n")
    
    # 동일하지 않은 경우, 어떤 차이가 있는지 확인
    for (i in 1:length(valid_column_names_list)) {
      if (!identical(valid_column_names_list[[i]], valid_column_names_list[[1]])) {
        cat("데이터프레임", i, "의 열 이름이 다릅니다:\n")
        print(valid_column_names_list[[i]])
      }
    }
  }
}

# 사용 예시
# compare_column_names(broadleaf_data_by_year, 19)

# 사용 예시
# compare_column_names(broadleaf_data_by_year, 19)

combine_columns <- function(df, col1, col2, new_col_name, after_col) {
  # 임시로 기존 열 이름 변경
  temp_col1 <- paste0(col1, "_temp")
  temp_col2 <- paste0(col2, "_temp")
  
  # 기존 열 이름 변경
  names(df)[names(df) == col1] <- temp_col1
  names(df)[names(df) == col2] <- temp_col2
  
  # 새로 추가할 열 계산
  new_column <- mapply(function(v1, v2) {
    if (is.na(v1) && is.na(v2)) {
      return(NA)
    } else if (is.na(v1)) {
      return(v2)
    } else if (is.na(v2)) {
      return(v1)
    } else {
      stop(paste("Error: Both columns", col1, "and", col2, "have non-NA values."))
    }
  }, df[[temp_col1]], df[[temp_col2]])
  
  # 새로운 열 추가 (임시로 마지막에 추가)
  df[[new_col_name]] <- new_column
  
  # 기존 열 제거
  df <- df[, !(names(df) %in% c(temp_col1, temp_col2))]
  
  # 열 이름 목록
  col_names <- names(df)
  
  # after_col의 위치 찾기
  after_index <- which(col_names == after_col)
  if (length(after_index) == 0) {
    stop(paste("Error: Column", after_col, "not found in the data frame."))
  }
  
  # 열 순서 재배치
  col_order <- c(col_names[1:after_index], new_col_name, col_names[(after_index + 1):(ncol(df) - 1)])
  
  # 새 열을 지정된 위치로 이동
  df <- df[, col_order]
  
  return(df)
}



check_continuous_years <- function(years) {
  # years 벡터의 최소와 최대 값을 구합니다.
  min_year <- min(years, na.rm = TRUE)
  max_year <- max(years, na.rm = TRUE)
  
  # 모든 연도가 연속적으로 있는지 확인합니다.
  all_years <- seq(min_year, max_year)
  is_continuous <- all(all_years %in% years)
  
  return(is_continuous)
}


extract_unique_years <- function(df) {
  # Define a helper function to extract year from a string
  extract_year <- function(x) {
    year <- sub(".*?(\\d{4}).*", "\\1", x)
    return(year)
  }
  
  # Extract years from the third column
  df$Year <- sapply(df[[3]], extract_year)
  
  # Get unique years
  unique_years <- unique(df$Year)
  
  # Create an empty dataframe to store the results
  result <- data.frame()
  
  # Define a small tolerance for numerical comparison
  tolerance <- .Machine$double.eps^0.5
  
  # Loop through each unique year
  for (year in unique_years) {
    # Subset the dataframe for the current year
    year_data <- df[df$Year == year, ]
    
    # Calculate the most frequent value in the 4th column
    freq_table <- table(year_data[[4]])
    max_freq_value <- as.numeric(names(freq_table)[which.max(freq_table)])
    
    # Find the first row with the most frequent value in the 4th column
    if (is.numeric(year_data[[4]])) {
      selected_row <- year_data[abs(year_data[[4]] - max_freq_value) < tolerance, ][1, ]
    } else {
      selected_row <- year_data[year_data[[4]] == max_freq_value, ][1, ]
    }
    
    # Append the selected row to the result dataframe
    result <- rbind(result, selected_row)
  }
  
  # Remove the Year column if not needed
  result$Year <- NULL
  
  return(result)
}

# Usage example:
# seedling_2 = extract_unique_years(seedling)
# Usage example:
# seedling_2 = extract_unique_years(seedling)

# Usage example:
# seedling_2 = extract_unique_years(seedling)

# Usage example:
# seedling_2 = extract_unique_years(seedling)

# Usage example:
# seedling_2 = extract_unique_years(seedling)

# Usage example:
# unique_data <- extract_unique_years(seedling)
# Usage example:
# unique_data <- extract_unique_years(seedling)
# Usage example:
# unique_data <- extract_unique_years(seedling)

# rm(list=ls())
# 🟥 data load ###############################################################################################################
# yb = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/4.Added YB ID.csv")
# path_from_upper = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/4.Exported Data by each cell"
# path_to_upper = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/5.Combined_Data"
# 
# 
filter = dplyr::filter


library(dplyr)
library(readr)

# 함수 정의
library(dplyr)

library(readr)

# 함수 정의
library(dplyr)
library(readr)

# 함수 정의
library(dplyr)
library(readr)

# 함수 정의
library(dplyr)
library(readr)

# 함수 정의
# 필요한 패키지 로드
library(dplyr)

# 함수 정의
# 필요한 패키지 로드
library(dplyr)

# 함수 정의
# 필요한 패키지 로드
library(dplyr)

# 함수 정의
# 함수 정의
process_1997_files <- function(directory, include_1, include_2, exclude) {
  # 파일 목록 가져오기 (파일명만)
  file_names <- list.files(directory, full.names = FALSE)
  
  # include_1 필터링 (하나라도 포함하는 파일들)
  filtered_files <- file_names[grepl(include_1, file_names)]
  
  
  # include_2 필터링 (모든 값을 포함하는 파일들)
  include_2_filtered <- include_1_filtered[sapply(include_1_filtered, function(file) all(sapply(include_2, function(pattern) grepl(pattern, file))))]
  
  # exclude 필터링 (하나라도 포함하는 파일 제거)
  final_files <- include_2_filtered[!sapply(include_2_filtered, function(file) any(sapply(exclude, function(pattern) grepl(pattern, file))))]
  
  # 데이터프레임 생성
  data_list <- lapply(final_files, function(file) {
    full_path <- file.path(directory, file)
    value <- read.csv(full_path, header = FALSE)[1, 1]
    data.frame(file_name = file, value = as.numeric(value))
  })
  
  # 데이터프레임 합치기
  result_df <- do.call(rbind, data_list)
  
  return(result_df)
}


# 파일 이름을 반환하는 함수 정의
find_file_by_value <- function(target_value) {
  # 각 파일에 대해 2행 3열의 값을 확인
  for (file in files) {
    data <- read.csv(file, header = TRUE)
    if (nrow(data) >= 2 && ncol(data) >= 3 && data[1, 3] == target_value) {
      return(basename(file))
    }
  }
  return(NULL)  # 값을 찾지 못한 경우
}
# 🟥 Load Functions & Packages ###############################################################################################
# rm(list = ls())
# 문자열 벡터에서 특정 문자열 패턴을 포함하는 값을 추출하는 함수 정의

# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
# 필요한 라이브러리 로드
library(stringr)

# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
# 필요한 라이브러리 로드
library(stringr)

# 문자열 벡터에서 특정 문자열 패턴을 포함하고 특정 문자열 패턴을 제외하는 값을 추출하는 함수 정의
filter_strings <- function(strings, include = NULL, exclude = NULL, or = FALSE) {
  # include에 있는 패턴을 포함하는 값들만 필터링
  if (!is.null(include)) {
    if (or) {
      strings <- strings[sapply(strings, function(x) any(sapply(include, function(pat) grepl(pat, x))))]
    } else {
      strings <- strings[sapply(strings, function(x) all(sapply(include, function(pat) grepl(pat, x))))]
    }
  }
  
  # exclude에 있는 패턴을 제외
  if (!is.null(exclude)) {
    strings <- strings[!sapply(strings, function(x) any(sapply(exclude, function(pat) grepl(pat, x))))]
  }
  
  return(strings)
}
filter = dplyr::filter

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
List.list[[5]] = texts = c("stringr", "stringi")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")
List.list[[10]] = others = c("beepr", "pander")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)



## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select






## 🟧Loading my functions ======================================================
# # R 함수 파일들을 로드하는 함수
# load_functions <- function(path_functions) {
#   list.files(path_functions, pattern = "\\.R$", full.names = TRUE) %>%
#     walk(~try(source(.x), silent = TRUE))
# }
# 
# # 주어진 경로에서 자동으로 R 폴더를 찾고 함수를 읽는 함수
# load_r_functions_from_path <- function(paths) {
#   walk(paths, ~{
#     # 주어진 경로가 디렉토리인지 확인
#     if (dir.exists(.x)) {
#       # R 폴더 경로 생성
#       r_folder_path <- file.path(.x, "R")
#       # R 폴더가 존재하는지 확인
#       if (dir.exists(r_folder_path)) {
#         load_functions(r_folder_path)
#         message("R 폴더의 함수들을 로드했습니다: ", r_folder_path)
#       } else {
#         message("R 폴더가 존재하지 않습니다: ", r_folder_path)
#       }
#     } else {
#       message("유효한 디렉토리가 아닙니다: ", .x)
#     }
#   })
# }
# 
# path_packages = c("/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/GitHub/refineR",
#                   "/Users/Ido/Library/CloudStorage/GoogleDrive-clair.de.lune.404@gmail.com/My Drive/GitHub/StatsR")
# 
# # 함수 호출
# load_r_functions_from_path(path_packages)






# 🟥 Define functions #####################################################################################################
## 🟧20121, 20122 파일이름 수정함수=====================================================================
# 필요한 라이브러리 로드
library(stringr)

# 파일 이름을 수정하는 함수 정의
rename_files_2014 <- function(directory_path) {
  # 디렉토리 내의 파일 목록을 가져옴
  file_list <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)
  
  # 각 파일에 대해 반복
  for (file_path in file_list) {
    # 파일 이름 추출
    file_name <- basename(file_path)
    
    # 파일 이름에서 특정 패턴 추출
    old_value <- str_extract(file_name, "(?<=___)\\d{5}(?=___본)")
    
    if (!is.na(old_value)) {
      # old_value를 새 값으로 변환
      new_value <- switch(old_value,
                          "20122" = "2013",
                          "20121" = "2012",
                          old_value)
      
      if (new_value != old_value) {
        # 새로운 파일 이름 생성
        new_file_name <- str_replace(file_name, old_value, new_value)
        
        # 파일 경로 수정
        new_file_path <- file.path(directory_path, new_file_name)
        
        # 파일 이름 변경
        if (file.rename(file_path, new_file_path)) {
          cat("Renamed file:", file_name, "to", new_file_name, "\n") # 변경된 경우 메시지 출력
        }
      }
    }
  }
}




## 🟧 키워드 추출 =====================================================================
# path = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/Data/4.Exported Data/숲가꾸기육림"
# 함수 정의
extract_regions <- function(path, exclude = NULL) {
  # 연도별 폴더 목록 가져오기
  path <- process_string_vector(path)
  year_folders <- list.files(path)
  
  # 경로에 폴더가 없으면 메시지 출력 후 종료
  if (length(year_folders) == 0) {
    stop("No files found in the specified path. Please re-enter the path.")
  }
  
  # 빈 벡터 초기화
  all_values <- c()
  
  # 각 연도 폴더별로 파일 목록 가져오기
  for (year in year_folders) {
    year_path <- file.path(path, year)
    files <- list.files(year_path)
    
    # 파일 이름에서 특정 위치의 문자열 추출
    extracted_values <- str_extract(files, "(?<=___).+?(?=___)")
    
    # 추출한 값을 all_values 벡터에 추가
    all_values <- c(all_values, extracted_values)
  }
  
  # 고유한 값만 추출
  unique_values <- unique(all_values)
  
  # 제외할 문자열 벡터가 주어진 경우 해당 문자열을 제외
  if (!is.null(exclude)) {
    exclude_pattern <- paste(exclude, collapse = "|")
    unique_values <- unique_values[!grepl(exclude_pattern, unique_values)]
  }
  
  unique_values <- process_string_vector(unique_values) %>% unique %>% sort
  return(unique_values)
}


## 🟧 어떤 연도들이 존재하는지 확인 =================================================================================
check_years_names <- function(years) {
  library(crayon)
  # 입력된 연도 문자열을 정수형으로 변환
  years <- as.integer(years)
  
  # 연도가 없으면 메시지 출력 후 종료
  if (length(years) == 0) {
    cat(red("No years provided.\n"))
    return()
  }
  
  # 연도를 정렬
  sorted_years <- sort(years)
  
  # 중복 연도 확인
  duplicated_years <- duplicated(sorted_years)
  if (any(duplicated_years)) {
    dup_years <- sorted_years[duplicated_years]
    cat(red("\nThe following years are duplicated: "), yellow(paste(dup_years, collapse = ", ")), "\n")
  }
  
  # 연도의 범위 확인
  year_range <- seq(min(sorted_years), max(sorted_years))
  
  # 누락된 연도 확인
  missing_years <- setdiff(year_range, sorted_years)
  if (length(missing_years) > 0) {
    cat(red("\nFrom "), yellow(min(sorted_years)), red(" to "), yellow(max(sorted_years)), red(" the following years are missing: "), yellow(paste(missing_years, collapse = ", ")), "\n")
  } else {
    # 중복이 없는 경우에만 연속성 메시지 출력
    if (!any(duplicated_years)) {
      cat(green("\nThe years are continuous from "), yellow(min(sorted_years)), green(" to "), yellow(max(sorted_years)), green(" without any duplicates.\n"))
    }
  }
}





# 🟧 키워드의 파일 위치 ================================================================================
library(stringr)
library(stringi)

extract_and_find_keywords <- function(path, search_keyword = NULL, exclude = NULL) {
  # 연도별 폴더 목록 가져오기
  path <- process_string_vector(path)
  year_folders <- list.files(path)
  
  # 빈 벡터 초기화
  all_values <- c()
  keyword_map <- list()
  
  # 각 연도 폴더별로 파일 목록 가져오기
  for (year in year_folders) {
    year_path <- file.path(path, year)
    files <- list.files(year_path)
    
    # 파일 이름에서 특정 위치의 문자열 추출
    extracted_values <- str_extract(files, "(?<=\\d{4}_).+?(?=___)")
    
    # 추출한 값을 정규화하여 all_values 벡터에 추가
    normalized_values <- stri_trans_nfc(extracted_values)
    all_values <- c(all_values, normalized_values)
    
    # 키워드와 해당 파일의 맵핑을 리스트에 저장
    for (i in seq_along(normalized_values)) {
      keyword <- normalized_values[i]
      file_name <- files[i]
      if (!is.null(keyword)) {
        if (is.null(keyword_map[[keyword]])) {
          keyword_map[[keyword]] <- list()
        }
        keyword_map[[keyword]] <- append(keyword_map[[keyword]], list(list(year = year, file = file_name)))
      }
    }
  }
  
  # 고유한 값만 추출
  unique_values <- unique(all_values)
  
  # 제외할 문자열 벡터가 주어진 경우 해당 문자열을 제외
  if (!is.null(exclude)) {
    exclude_pattern <- paste(stri_trans_nfc(exclude), collapse = "|")
    unique_values <- unique_values[!grepl(exclude_pattern, unique_values)]
  }
  
  unique_values <- process_string_vector(unique_values) %>% unique %>% sort
  
  # 검색 키워드가 주어진 경우 해당 키워드의 소스를 반환
  if (!is.null(search_keyword)) {
    if (!is.null(keyword_map[[search_keyword]])) {
      sources <- keyword_map[[search_keyword]]
      for (source in sources) {
        cat("Year:", source$year, "File:", source$file, "\n")
      }
    } else {
      cat("Keyword not found.\n")
    }
  }
  
  return(unique_values)
}







## 🟧 연도 키워드  추출 함수 =================================================================================

extract_years <- function(keywords) {
  # 정규 표현식을 사용하여 4자리 숫자만 추출
  years <- keywords[grepl("^[0-9]{4}$", keywords)] %>% sort
  return(years)
}






## 🟧 키워드 추출 =====================================================================
extract_keywords = function(path, exclude = NULL) {
  # 연도별 폴더 목록 가져오기
  path = process_string_vector(path)
  year_folders <- list.files(path)
  
  # 빈 벡터 초기화
  all_values <- c()
  
  # 각 연도 폴더별로 파일 목록 가져오기
  for (year in year_folders) {
    year_path <- file.path(path, year)
    files <- list.files(year_path)
    
    # 파일 이름에서 특정 위치의 문자열 추출
    extracted_values <- str_extract(files, "(?<=\\d{4}_).+?(?=___)")
    
    # 추출한 값을 정규화하여 all_values 벡터에 추가
    all_values <- c(all_values, stri_trans_nfc(extracted_values))
  }
  
  # 고유한 값만 추출
  unique_values <- unique(all_values)
  
  # 제외할 문자열 벡터가 주어진 경우 해당 문자열을 제외
  if (!is.null(exclude)) {
    exclude_pattern <- paste(stri_trans_nfc(exclude), collapse = "|")
    unique_values <- unique_values[!grepl(exclude_pattern, unique_values)]
  }
  unique_values = process_string_vector(unique_values) %>% unique %>% sort
  return(unique_values)
}




## 🟧 문자열 처리 함수 =====================================================================
process_string_vector <- function(string_vector) {
  # 개별 문자열을 처리하는 함수
  process_single_string <- function(input_string) {
    # 인코딩을 UTF-8로 변환 후 정규화 (NFC) 
    # (정규화 안 하면 맥에서 동작 안 함)
    input_string <- stri_trans_nfc(stri_enc_toutf8(input_string))
    
    # "___"로 나누기
    # split_parts <- str_split(input_string, "___")[[1]]
    
    # 두 번째 "___" 뒷부분 제거
    # if (length(split_parts) > 2) {
    #   result <- str_c(split_parts[1:2], collapse = " ")
    # } else {
    #   result <- str_c(split_parts, collapse = " ")
    # }
    # 
    # 결과를 native 인코딩으로 변환
    result <- enc2native(input_string)
    
    return(result)
  }
  
  sapply(string_vector, process_single_string) %>% unname %>% return()
}





## 🟧 특수문자 제거 =====================================================================
remove_special_characters <- function(input_string) {
  # 한국어, 알파벳, 숫자, 언더스코어를 제외한 모든 특수문자 제거
  output_string <- stri_replace_all_regex(input_string, "[^가-힣a-zA-Z0-9_]", "")
  return(output_string)
}


## 🟧 인코딩 변환 함수 정의 =====================================================================
convert_to_utf8 <- function(input_string) {
  # 인코딩 감지
  encoding <- stri_enc_detect(input_string)
  
  # 인코딩 감지 결과가 없거나 비어 있는 경우 기본 인코딩 설정
  if (length(encoding) == 0 || is.null(encoding[[1]]$Encoding[1])) {
    detected_encoding <- "CP949"  # 또는 "EUC-KR" 등으로 변경 가능
  } else {
    detected_encoding <- encoding[[1]]$Encoding[1]
  }
  
  # 인코딩 변환
  output_string <- stri_encode(input_string, from = detected_encoding, to = "UTF-8")
  
  return(output_string)
}


## 🟧 경로의 깨진 한글 복구 =====================================================================
restore_broken_path <- function(broken_path) {
  # 각 부분을 분리하여 리스트로 저장
  path_parts <- unlist(strsplit(broken_path, split = "/"))
  
  # 각 부분의 인코딩을 UTF-8로 복구
  restored_parts <- sapply(path_parts, function(part) {
    # UTF-8로 재인코딩
    tryCatch(
      stri_encode(part, from = "unknown", to = "UTF-8"),
      error = function(e) part # 에러가 발생하면 원래 부분을 그대로 사용
    )
  })
  
  # 복구된 경로 생성
  restored_path <- paste(restored_parts, collapse = "/")
  
  return(restored_path)
}




## 🟧 모든 연도가 전부 존재하는지 확인하는 함수 =====================================================================
check_years_df <- function(df) {
  library(dplyr)
  library(crayon)  # 색상 출력을 위해 crayon 패키지 사용
  
  # 데이터프레임의 클래스를 명확하게 지정
  df <- as.data.frame(df)
  
  # 열 이름이 문자열인지 확인하고 factor가 아닌지 확인
  df$file <- as.character(df$file)
  
  # "해당 파일 없음"이 아닌 행의 year 열을 추출
  valid_years <- df %>% 
    filter(file != "해당 파일 없음") %>% 
    pull(year)
  
  # 문자열을 정수형으로 변환
  years <- as.integer(valid_years)
  
  # NA 값 제거
  years <- years[!is.na(years)]
  
  # 연도가 없으면 메시지 출력 후 종료
  if (length(years) == 0) {
    cat(red("No valid years provided.\n"))
    return()
  }
  
  # 연도를 정렬
  sorted_years <- sort(years)
  
  # 중복 연도 확인
  duplicated_years <- duplicated(sorted_years)
  if (any(duplicated_years)) {
    dup_years <- sorted_years[duplicated_years]
    cat(red("\nThe following years are duplicated: "), yellow(paste(dup_years, collapse = ", ")), "\n")
  }
  
  # 연도의 범위 확인
  year_range <- seq(min(sorted_years), max(sorted_years))
  
  # 누락된 연도 확인
  missing_years <- setdiff(year_range, sorted_years)
  if (length(missing_years) > 0) {
    cat(red("\nFrom "), yellow(min(sorted_years)), red(" to "), yellow(max(sorted_years)), red(" the following years are missing: "), yellow(paste(missing_years, collapse = ", ")), "\n")
  } else {
    # 중복이 없는 경우에만 연속성 메시지 출력
    if (!any(duplicated_years)) {
      cat(green("\nThe years are continuous from "), yellow(min(sorted_years)), green(" to "), yellow(max(sorted_years)), green(" without any duplicates.\n"))
    }
  }
}


## 🟧 시계열 데이터 플롯 =====================================================================
# 패키지 설치 및 로드 함수
install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# timeseries_plot 함수
# 필요한 패키지 설치 및 로드
install_and_load("extrafont")


# 글꼴 가져오기 및 로드 (한글 글꼴 설정)
try({
  suppressWarnings({
    font_import(paths = c("/Library/Fonts/", "/System/Library/Fonts", "/System/Library/Fonts/Supplemental", "~/Library/Fonts/"), 
                pattern = "NanumGothic", prompt = FALSE)
  })
  loadfonts(device = "win", quiet = TRUE)  # Windows의 경우
  loadfonts(device = "postscript", quiet = TRUE)  # macOS 및 다른 경우
}, silent = TRUE)

timeseries_plot <- function(result_data) {
  if (Sys.info()["sysname"] == "Windows") {
    windowsFonts(Malgun = windowsFont("Malgun Gothic"))
    font_family <- "Malgun"
  } else if (Sys.info()["sysname"] == "Darwin") {
    quartzFonts(MyFont = quartzFont(c("AppleGothic", "AppleGothic", "AppleGothic", "AppleGothic")))
    font_family <- "MyFont"
  } else {
    font_family <- "sans"
  }
  
  file_name <- result_data$File[1] %>% gsub("______", "___", .)
  parts <- unlist(strsplit(file_name, "___"))
  description <- parts[2]
  
  # unit_new와 Value_new 열이 존재하면 이를 사용
  if ("unit_new" %in% names(result_data) & "Value_new" %in% names(result_data)) {
    unit <- sub(".csv", "", parts[4])  # unit_new 열이 존재할 때 파일 이름의 다른 부분 사용
    value_column <- "Value_new"
  } else {
    unit <- sub(".csv", "", parts[3])
    value_column <- "Value"
  }
  
  plot_title <- sprintf("%s (%s)", description, unit)
  
  p = ggplot(result_data, aes(x = Year, y = .data[[value_column]])) +
    geom_line(color = "#00BFC4", linewidth = 1) +
    geom_point(color = "#F8766D", size = 3) +
    labs(title = plot_title, x = "Year", y = "Value") +
    theme_minimal(base_size = 14) +
    theme(text = element_text(family = font_family),  # 폰트 패밀리를 모든 텍스트에 적용
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    scale_x_continuous(breaks = result_data$Year) %>% suppressWarnings()
  
  return(p)
}


## 🟧 단위 변환 함수 =====================================================================
# 범용 단위 변환 함수
convert_units <- function(data) {
  # data = result_data_2
  # 🟨 면적
  if (unique(grepl("면적", unique(data$Sub_Category)))) {
    # 헥타르 관련 단위 확인
    is_hectare <- grepl("천헥타|1,000ha|단위=헥타|단위=ha|ha|면적=헥타", data$unit)
    
    # 헥타르 관련 단위인 경우 처리
    if (any(is_hectare)) {
      # 새로운 단위를 "ha"로 설정
      data$unit_new <- "ha"
      
      # 값 조정: "1000ha" 단위는 값에 1000을 곱하고, 그 외는 그대로 유지
      data$Value_new <- ifelse(grepl("천헥타|1,000ha", data$unit), data$Value * 1000, data$Value)
      
      return(data)
    }  
  # 🟨 수량
  } else if(unique(grepl("수량", unique(data$Sub_Category)))){
    is_quantity = grepl("수량|1,000본", data$unit)
     
    # 수량관련 단위인 경우 처리
    if (any(is_quantity)) {
      # 새로운 단위를 "그루"로 설정
      data$unit_new <- "그루"
      
      data$Value_new <- ifelse(grepl("수량|1,000본", data$unit), data$Value * 1000, data$Value)
      
      return(data)
    }   
    
  }else {
    # 헥타르, 수량 관련 단위가 아닌 경우 원래 데이터 그대로 반환
    data$unit_new <- data$unit
    data$Value_new <- data$Value
    return(data)
  }
}




## 🟧 파일을 필터링하고 데이터를 복사하는 함수 =====================================================================
filter_files_by_patterns <- function(files, include, exclude) {
  # 맨 앞의 "연도_" 제거
  files_without_year <- sapply(files, function(file) sub("^[0-9]{4}_", "", file)) %>% unname
  
  # 두 번째 "___" 문자열 이전까지 추출
  files_cleaned <- sapply(files_without_year, function(x) {
    parts <- unlist(strsplit(x, "___"))
    paste(parts[1:2], collapse = "___")
  }) %>% unname
  
  
  # 필터링 적용
  filtered_files <- files[sapply(files_cleaned, function(file) {
    includes = all(sapply(include, function(pattern) grepl(pattern, file)))
    excludes = any(sapply(exclude, function(pattern) grepl(pattern, file)))
    includes && !excludes
  })]
  
  return(filtered_files)
}






## 🟧🟩 결과 출력 및 파일 전송 함수 =====================================================================

# 
# # 데이터 복사
# copy_data_by_year <- function(yb, path_from, 
#                               path_to, 
#                               save_file_name,
#                               include_1, 
#                               exclude_1, 
#                               include_2 = NULL, 
#                               exclude_2 = NULL, 
#                               include_3 = NULL, 
#                               exclude_3 = NULL, 
#                               include_4 = NULL, 
#                               exclude_4 = NULL, 
#                               message = TRUE,
#                               remove.files = FALSE) {
#   # 깨진 한글 경로 복구
#   path_from <- restore_broken_path(path_from)
#   path_to <- restore_broken_path(path_to)
#   dir.create(path_to, showWarnings = FALSE, recursive = T)
#   
#   # path_from에서 폴더 리스트 읽기
#   years <- as.numeric(fs::dir_ls(path_from, type = "directory") %>% basename())
#   
#   all_selected_files <- list()
#   
#   # 인코딩 변환
#   include_1 <- process_string_vector(include_1)
#   exclude_1 <- process_string_vector(exclude_1)
#   
#   if (!is.null(include_2) && !is.null(exclude_2)) {
#     include_2 <- process_string_vector(include_2)
#     exclude_2 <- process_string_vector(exclude_2)
#   }
#   
#   if (!is.null(include_3) && !is.null(exclude_3)) {
#     include_3 <- process_string_vector(include_3)
#     exclude_3 <- process_string_vector(exclude_3)
#   }
#   
#   if (!is.null(include_4) && !is.null(exclude_4)) {
#     include_4 <- process_string_vector(include_4)
#     exclude_4 <- process_string_vector(exclude_4)
#   }
#   
#   for (year in years) {
#     # year = 1992
#     # year = 2015
#     # 파일 리스트
#     year_path <- file.path(path_from, as.character(year))
#     files <- fs::dir_ls(year_path, type = "file") %>% 
#       basename() %>% 
#       process_string_vector # 문자열 인코딩 수정
#     
#     # include_1과 exclude_1 조건에 맞는 파일 선택
#     selected_files_1 <- filter_files_by_patterns(files, include_1, exclude_1)
#     #여기
#     # include_2와 exclude_2 조건에 맞는 파일 선택, NULL이 아닐 때만 실행
#     selected_files_2 <- if (!is.null(include_2) | !is.null(exclude_2)) {
#       filter_files_by_patterns(files, include_2, exclude_2)
#     } else {
#       character(0) # 빈 문자열 벡터
#     }
#     
#     # include_3와 exclude_3 조건에 맞는 파일 선택, NULL이 아닐 때만 실행
#     selected_files_3 <- if (!is.null(include_3) | !is.null(exclude_3)) {
#       filter_files_by_patterns(files, include_3, exclude_3)
#     } else {
#       character(0) # 빈 문자열 벡터
#     }
#     
#     # include_4와 exclude_4 조건에 맞는 파일 선택, NULL이 아닐 때만 실행
#     selected_files_4 <- if (!is.null(include_4) | !is.null(exclude_4)) {
#       filter_files_by_patterns(files, include_4, exclude_4)
#     } else {
#       character(0) # 빈 문자열 벡터
#     }
#     
#     # 모든 선택된 파일들을 결합하여 중복 제거
#     all_selected_files[[as.character(year)]] <- unique(c(selected_files_1, selected_files_2, selected_files_3, selected_files_4))
#   }
#   
#   test =  all_selected_files %>% unlist
#   if(length(test)>0){
#     # 결과 출력 및 데이터 카피
#     print_and_transfer_files(yb,
#                              all_selected_files,
#                              path_from, 
#                              path_to, 
#                              save_file_name, 
#                              remove.files, 
#                              message)  
#   }else{
#     cat("\n", crayon::bgRed("No files were selected!"),"\n")
#   }
# }
# 


library(dplyr)

extract_and_export_csv <- function(csv_file_path, row_value, export_path) {
  dir.create(export_path, showWarnings = F)
  # CSV 파일 읽기
  df <- read.csv(csv_file_path, stringsAsFactors = FALSE)
  
  # 3번째 열의 값이 row_value와 일치하는 행의 위치 찾기
  row_index <- which(df[, 3] == row_value)
  if (length(row_index) == 0) {
    stop("No matching row found.")
  }
  
  # Categorized_L3_New 열의 위치 찾기
  cat_col_index <- which(names(df) == "Categorized_L3_New")
  if (length(cat_col_index) == 0) {
    stop("No matching column named 'Categorized_L3_New'.")
  }
  
  # 파일 이름 구성 요소 추출
  file_name <- basename(csv_file_path)
  year <- substr(file_name, 1, 4)
  yearbook_id <- sub(".*(_YRBK_.*)\\.csv$", "\\1", file_name)
  unit <- unique(df$unit_L3)
  
  # 4번째 열부터 Categorized_L3_New 열의 이전 열까지의 열 이름들을 사용
  for (col_index in 4:(cat_col_index - 1)) {
    col_name <- names(df)[col_index]
    value <- df[row_index, col_index]
    
    # 새로운 파일 이름 생성
    new_file_name <- paste0(year, "_", col_name, "___", row_value, "___", unit, "___", yearbook_id, ".csv")
    
    # 새로운 데이터프레임 생성
    output_df <- data.frame(Value = value)
    
    # CSV 파일 내보내기
    write.csv(output_df, file.path(export_path, new_file_name), row.names = FALSE)
    
    cat("File saved as:", file.path(export_path, new_file_name), "\n")
  }
}
export_data <- function(path_export_from, path_export_to) {
  # 필요한 라이브러리 로드
  library(dplyr)
  library(readr)
  library(fs)
  
  # CSV 파일 목록 가져오기
  file_list <- list.files(path_export_from, pattern = "\\.csv$", full.names = TRUE)
  
  # 각 파일 처리
  for (file in file_list) {
    # CSV 파일 읽기
    df <- read_csv(file)
    
    # 데이터프레임의 year 열에서 연도 값 추출
    year <- unique(df$year)
    
    # 3번째 열에서 연도가 포함된 행 선택
    year_rows <- df %>%
      filter(grepl("^(\\d{4}|\\d{4}합계)$", df[[3]]))
    
    # 데이터 열 범위 선택
    data_cols <- which(names(df) == "Categorized_L3_New") - 1
    
    # 데이터 셀 추출 및 CSV로 내보내기
    for (i in seq_len(nrow(year_rows))) {
      year_row <- year_rows[i, ]
      year_value <- year_row[[3]]
      name_l4 <- ifelse(is.na(year_row$NAME_L4), "", unique(year_row$NAME_L4))
      id <- unique(year_row$ID)
      
      # 연도별 폴더 생성
      year_folder <- file.path(path_export_to, year)
      dir_create(year_folder)
      
      for (col_idx in 4:data_cols) {
        col_name <- names(df)[col_idx]
        cell_value <- year_row[[col_idx]]
        
        # 파일명 구성
        if (name_l4 != "") {
          file_name <- paste0(year, "_", paste(col_name, year_value, name_l4, id, sep = "___"))
        } else {
          file_name <- paste0(year, "_", paste(col_name, year_value, id, sep = "___"))
        }
        file_name <- paste0(file_name, ".csv")
        
        # 파일 내보내기
        write_csv(data.frame(cell_value), file.path(year_folder, file_name))
      }
    }
  }
}


remove_specific_strings <- function(vector) {
  # "_NA"와 "수량_" 문자열을 빈 문자열로 대체
  modified_vector <- gsub("_NA", "", vector)
  modified_vector <- gsub("수량_", "", modified_vector)
  
  return(modified_vector)
}



list_empty_folders <- function(directory) {
  # 모든 폴더를 재귀적으로 찾기
  folders <- list.dirs(directory, recursive = TRUE, full.names = TRUE)
  
  # 빈 폴더를 저장할 벡터 초기화
  empty_folders <- c()
  
  for (folder in folders) {
    # 현재 폴더의 파일 목록 가져오기
    files_in_folder <- list.files(folder, all.files = TRUE, no.. = TRUE)
    
    # 파일이 전혀 없는 폴더인지 확인
    if (length(files_in_folder) == 0) {
      empty_folders <- c(empty_folders, folder)
    }
  }
  
  return(empty_folders)
}

# 데이터 복사
copy_data_by_year <- function(yb, path_from, 
                              path_to, 
                              save_file_name,
                              include.list, 
                              exclude.list = NULL, 
                              message = TRUE,
                              remove.files = FALSE) {
  # 깨진 한글 경로 복구
  path_from <- restore_broken_path(path_from)
  path_to <- restore_broken_path(path_to)
  dir.create(path_to, showWarnings = FALSE, recursive = TRUE)
  
  # path_from에서 폴더 리스트 읽기
  years <- as.numeric(fs::dir_ls(path_from, type = "directory") %>% basename())
  
  all_selected_files <- list()
  
  # 인코딩 변환
  include.list <- lapply(include.list, process_string_vector)
  
  # exclude.list가 NULL이면 include.list의 길이만큼 NULL 값으로 채우기
  if (is.null(exclude.list)) {
    exclude.list <- vector("list", length(include.list))
  } else {
    exclude.list <- lapply(exclude.list, process_string_vector)
    
    # exclude.list의 길이가 include.list의 길이보다 작으면 부족한 만큼 NULL 추가
    if (length(exclude.list) < length(include.list)) {
      exclude.list <- c(exclude.list, vector("list", length(include.list) - length(exclude.list)))
    }
  }
  
  
  for (year in years) {
    print(year)
    # year = years[1]
    # year = "1982"
    # 파일 리스트
    year_path <- file.path(path_from, as.character(year))
    
    path_files = fs::dir_ls(year_path, type = "file")
    
    files <- fs::dir_ls(year_path, type = "file") %>% 
      basename() %>% 
      process_string_vector # 문자열 인코딩 수정
    
    # 각 include와 exclude 조건에 맞는 파일 선택
    selected_files <- list()
    
    for (m in seq_along(include.list)) {
      # m=1
      
      include <- include.list[[m]]
      
      if(c(is.null(exclude.list[[m]]) || (length(exclude.list[[m]]) == 0))){
        
        exclude = NULL
        
      }else{
        
        exclude <- exclude.list[[m]]  
      }
      
      selected_files[[m]] <- filter_files_by_patterns(files, include, exclude)
      
    }
    
    # 모든 선택된 파일들을 결합하여 중복 제거
    all_selected_files[[as.character(year)]] <- unique(unlist(selected_files))
  }
  
  
  test <- all_selected_files %>% unlist
  
  if (length(test) > 0 && !all(is.na(test)) && !is.null(test)) {
    # 결과 출력 및 데이터 카피
    print_and_transfer_files(yb,
                             all_selected_files,
                             path_from, 
                             path_to, 
                             save_file_name, 
                             remove.files, 
                             message)  
  } else {
    cat("\n", crayon::bgRed("No files were selected!"),"\n")
    # stop("")
  }
}

# 
# print_and_transfer_files <- function(yb,
#                                      all_selected_files,
#                                      path_from, 
#                                      path_to, 
#                                      save_file_name, 
#                                      remove.files = F, 
#                                      message = TRUE) {
#   ### 🟨 데이터 불러오기 ======================================================================
#   results_df <- do.call(rbind, lapply(names(all_selected_files), function(year) {
#     files <- all_selected_files[[year]]
#     if (length(files) == 0) {
#       # 파일이 없을 경우
#       data.frame(year = year, file = "해당 파일 없음")
#     } else {
#       # 파일이 있을 경우
#       data.frame(year = year, file = files)
#     }
#   }))
#   
#   
#   
#   
#   ### 🟨 데이터 체크 ======================================================================
#   # 파일 확인 여부를 묻는 메시지
#   if (message) {
#     # Display datatable
#     DT::datatable(results_df, options = list(pageLength = 5)) %>% print
#     results_df %>% as_tibble %>% check_years(., message)
#     
#     # Prompt user for confirmation
#     cat("\n", crayon::green("These are the data selected. Do you confirm it? (yes/no) "), "\n")
#     user_input <- tolower(readline())
#   } else {
#     user_input <- "yes"
#   }
#   
#   
#   
#   
#   
#   ### 🟨 하나의 데이터프레임으로 합치기 ======================================================================
#   if (user_input == "no") {
#     beepr::beep(sound=10)
#     stop("Check your input for every argument!") 
#   } else if (user_input %in% c("yes", "y")) {
#     # 결과 데이터프레임 초기화
#     result_data <- data.frame(Year = integer(), File = character(), Value = numeric(), stringsAsFactors = FALSE)
#     
#     # 각 연도별 파일 데이터 읽기
#     for (year in names(all_selected_files)) {
#       year_path <- file.path(path_from, year)
#       files <- all_selected_files[[year]]
#       
#       for (file in files) {
#         if (file != "해당 파일 없음") {
#           # 파일에서 데이터 읽기
#           file_data <- read.csv(file.path(year_path, file), header = TRUE, stringsAsFactors = FALSE)
#           
#           # 첫 번째 값 추출
#           value <- file_data[1, 1]  # 첫 번째 행, 첫 번째 열
#           
#           # 결과 데이터프레임에 추가
#           result_data <- data.frame(Year = year, File = file, Value = value) %>% 
#             rbind(result_data, .) %>% 
#             as_tibble %>% 
#             mutate(Value = as.numeric(Value)) %>% 
#             mutate(Year = as.numeric(Year))
#         }
#       }
#     }
#     
#     ### 🟨 데이터프레임 수정 ======================================================================
#     result_data_2 <- result_data %>%
#       mutate(File_temp = sub("^[0-9]{4}_", "", File)) %>% # 연도
#       mutate(File_temp = sub("\\.csv$", "", File_temp)) %>% # csv 제거
#       separate(File_temp, into = c("Sub_Category", "Classification", "unit", "ybid"), 
#                sep = "___") %>% 
#       convert_units() %>% # 단위 수정
#       mutate(File = result_data$File) %>%   # 원래의 File 열 추가
#       suppressWarnings()
#     
#     
#     # 데이터프레임 재정렬
#     result_data_2 <- result_data_2 %>%
#       select(ybid, Year, File, Sub_Category, Classification, 
#              unit, Value, unit_new, Value_new) %>% 
#       mutate(Category = basename(path_from)) %>% 
#       relocate(Category, .before = "Sub_Category")
#     
#     
#     
#     
#     
#     ### 🟨 결과 출력 ======================================================================
#     if(message){
#       print(result_data_2)
#       cat("\n", crayon::bgMagenta("Unit"), crayon::green("before"), "\n")
#       print(result_data_2$unit)
#       cat("\n", crayon::bgMagenta("Unit"), crayon::green("after"), "\n")
#       print(result_data_2$unit_new)
#       
#     }
#     
#     
#     
#     ### 🟨 outlier 체크 ======================================================================
#     # time series plot
#     if (message) {
#       p <- timeseries_plot(result_data_2)
#       suppressWarnings(print(p))
#       cat("\n", crayon::green("Did you check outliers? (yes/no) "), "\n")
#       user_input <- tolower(readline())
#     } else {
#       p <- timeseries_plot(result_data_2)
#       user_input <- "yes"
#     }
#     
#     
#     
#     ### 🟨 최종 데이터프레임 옮기기 ======================================================================
#     if (user_input == "yes") {
#       # 성공 메시지
#       selected_yb = yb %>% filter(ID %in% result_data_2$ybid) %>% select(year, NAME_L1, NAME_L2, NAME_L3, NAME_L4, NAME_L5) %>% arrange(year)
#       result_data_3 = result_data_2 %>% 
#         select(-File) %>% 
#         left_join(., selected_yb, c("Year" = "year"))
#       
#       # View(result_data_3)
#       
#       write.csv(result_data_3, file.path(path_to, paste0(save_file_name, ".csv")), row.names=F)
#       ggsave(
#         filename = file.path(path_to, paste0(save_file_name, ".png")),
#         plot = p,
#         bg = "white",
#         width = 10,  # 너비 (인치)
#         height = 6  # 높이 (인치)
#       )
#       cat("\n", crayon::green("Data processed successfully!"), "\n")  
#       # beepr::beep(2)
#       # dev.off()
#       
#       #### 🟩 파일 제거 옵션 ======================================================================
#       if(remove.files){
#         
#         
#       }
#       
#     }else{
#       cat("\n", crayon::red("Data copy cancelled."), "\n")
#       # beepr::beep(sound=10)
#     }
#   }
# }


print_and_transfer_files <- function(yb,
                                     all_selected_files,
                                     path_from, 
                                     path_to, 
                                     save_file_name, 
                                     remove.files = F, 
                                     message = TRUE) {
  # path_to = path_to_upper
  ### 🟨 데이터 불러오기 ======================================================================
  results_df <- do.call(rbind, lapply(names(all_selected_files), function(year) {
    files <- all_selected_files[[year]]
    if (length(files) == 0) {
      # 파일이 없을 경우
      data.frame(year = year, file = "해당 파일 없음")
    } else {
      # 파일이 있을 경우
      data.frame(year = year, file = files)
    }
  }))
  
  ### 🟨 데이터 체크 ======================================================================
  # 파일 확인 여부를 묻는 메시지
  # message =T
  if (message) {
    # Display datatable
    DT::datatable(results_df, options = list(pageLength = 5)) %>% print
    results_df %>% as_tibble %>% check_years_df
    
    # Prompt user for confirmation
    cat("\n", crayon::green("These are the data selected. Do you confirm it? (yes/no) "), "\n")
    user_input <- tolower(readline())
  } else {
    user_input <- "yes"
  }
  
  
  
  ### 🟨 하나의 데이터프레임으로 합치기 ======================================================================
  if (user_input == "no") {
    # beepr::beep(sound=10)
    stop("Check your input for every argument!") 
  } else if (user_input %in% c("yes", "y")) {
    # 결과 데이터프레임 초기화
    result_data <- data.frame(Year = integer(), File = character(), Value = numeric(), stringsAsFactors = FALSE)
    
    # 각 연도별 파일 데이터 읽기
    for (year in names(all_selected_files)) {
      year_path <- file.path(path_from, year)
      files <- all_selected_files[[year]]
      
      for (file in files) {
        if (file != "해당 파일 없음") {
          # 파일에서 데이터 읽기
          file_data <- read.csv(file.path(year_path, file), header = TRUE, stringsAsFactors = FALSE)
          
          # 첫 번째 값 추출
          value <- file_data[1, 1]  # 첫 번째 행, 첫 번째 열
          
          # 결과 데이터프레임에 추가
          result_data <- data.frame(Year = year, File = file, Value = value) %>% 
            rbind(result_data, .) %>% 
            as_tibble %>% 
            mutate(Value = as.numeric(Value)) %>% 
            mutate(Year = as.numeric(Year))
        }
      }
    }
    
    ### 🟨 데이터프레임 수정 ======================================================================
    result_data_2 <- result_data %>%
      mutate(File_temp = sub("^[0-9]{4}_", "", File)) %>% # 연도
      mutate(File_temp = sub("\\.csv$", "", File_temp)) %>% # csv 제거
      separate(File_temp, into = c("Sub_Category", "Sub_Sub_Category", "unit", "ybid"), 
               sep = "___") %>% 
      convert_units() %>% # 단위 수정
      # mutate(File = result_data$File) %>%   # 원래의 File 열 추가
      # suppressWarnings() %>%
      select(ybid, Year, File, Sub_Category, Sub_Sub_Category, 
             unit, Value, unit_new, Value_new) %>% 
      mutate(Category = basename(path_from)) %>% 
      relocate(Category, .before = "Sub_Category")
    # View(result_data_2 )
    
    
    
    ### 🟨 결과 출력 ======================================================================
    if (message) {
      print(result_data_2)
      cat("\n", crayon::bgMagenta("Unit"), crayon::green("before"), "\n")
      print(result_data_2$unit)
      cat("\n", crayon::bgMagenta("Unit"), crayon::green("after"), "\n")
      print(result_data_2$unit_new)
    }
    
    ### 🟨 outlier 체크 ======================================================================
    # time series plot
    if (message) {
      timeseries_plot(result_data_2)
      p <- timeseries_plot(result_data_2)
      print(p)
      suppressWarnings(print(p))
      cat("\n", crayon::green("Did you check outliers? (yes/no) "), "\n")
      user_input <- tolower(readline())
    } else {
      timeseries_plot(result_data_2)
      p <- timeseries_plot(result_data_2)
      print(p)
      user_input <- "yes"
    }
    
    ### 🟨 최종 데이터프레임 옮기기 ======================================================================
    if (user_input == "yes") {
      # 성공 메시지
      selected_yb <- yb %>% filter(ID %in% result_data_2$ybid) %>% select(year, NAME_L1, NAME_L2, NAME_L3, NAME_L4, NAME_L5) %>% arrange(year)
      result_data_3 <- result_data_2 %>% 
        left_join(., selected_yb, c("Year" = "year"))
      
      write.csv(result_data_3, file.path(path_to, paste0(save_file_name, ".csv")), row.names = FALSE)
      ggsave(
        filename = file.path(path_to, paste0(save_file_name, ".png")),
        plot = p,
        bg = "white",
        width = 10,  # 너비 (인치)
        height = 6  # 높이 (인치)
      )
      cat("\n", crayon::green("Data processed successfully!"), "\n")  
      
      #### 🟩 파일 제거 옵션 ======================================================================
      if (remove.files) {
        for (year in names(all_selected_files)) {
          year_path <- file.path(path_from, year)
          files <- all_selected_files[[year]]
          
          for (file in files) {
            if (file != "해당 파일 없음") {
              file_path <- file.path(year_path, file)
              if (file.exists(file_path)) {
                file.remove(file_path)
              }
            }
          }
        }
      }
      
    } else {
      cat("\n", crayon::red("Data copy cancelled."), "\n")
    }
  }
}



# 🟧 모든 연도 합치는 함수 (최빈값 기준) ================================================================
process_and_export_most_frequent_value_rows <- function(path_to, output_file) {
  # 지정된 경로에서 파일 리스트 가져오기
  files <- list.files(path_to, full.names = TRUE)
  
  # "연도.csv" 형태의 파일만 필터링
  csv_files <- files[grepl("_[0-9]{4}\\.csv$", files)]
  
  # 파일을 연도 순서대로 정렬
  csv_files <- csv_files[order(as.numeric(gsub(".*_([0-9]{4})\\.csv$", "\\1", csv_files)))]
  
  # 파일이 하나만 존재하는 경우 함수 실행 생략
  if (length(csv_files) <= 1) {
    cat("\n", crayon::red("파일이 하나만 존재하거나 없습니다. 함수 실행을 생략합니다."), "\n")
    return(NULL)
  }
  
  # 각 파일을 읽고 Value_new의 최빈값 행만 추출
  most_frequent_value_rows <- lapply(csv_files, function(file) {
    df <- read_csv(file, show_col_types = FALSE)
    # 최빈값 계산
    most_frequent_value <- as.numeric(names(sort(table(df$Value_new), decreasing = TRUE)[1]))
    most_frequent_rows <- df %>% filter(Value_new == most_frequent_value)
    # Classification 열을 기준으로 가장 오래된 연도 선택
    selected_row <- most_frequent_rows %>% arrange(Sub_Sub_Category) %>% slice(1)
    # Sub_Sub_Category 열을 character로 변환
    selected_row <- selected_row %>% mutate(Sub_Sub_Category = as.character(Sub_Sub_Category))
    return(selected_row)
  })
  
  # 모든 추출한 행들을 하나의 데이터프레임으로 합치기
  combined_df <- bind_rows(most_frequent_value_rows)
  
  # Sub_Sub_Category와 Value_new가 제대로 읽히도록 형 변환
  combined_df <- combined_df %>%
    mutate(Sub_Sub_Category = as.factor(Sub_Sub_Category), Value_new = as.numeric(Value_new))
  # View(combined_df)
  # 연도 순서 확인
  years = combined_df$Sub_Sub_Category %>% gsub("까지", "", .) %>% as.numeric %>% sort
  all_years_present <- all(diff(years) == 1)
  first_year <- min(years)
  last_year <- max(years)
  
  if (all_years_present) {
    output_file <- paste0(output_file, "_complete")
    message <- sprintf("모든 연도가 1년 차이로 존재합니다. (%d년부터 %d년까지)", first_year, last_year)
  } else {
    output_file <- paste0(output_file, "_incomplete")
    message <- sprintf("연도 사이에 누락된 연도가 있습니다. (%d년부터 %d년까지)", first_year, last_year)
  }
  
  # 지정한 파일 이름으로 데이터프레임 내보내기
  write_csv(combined_df, file.path(path_to, paste0(output_file, ".csv")))
  
  # ggplot을 이용한 timeseries plot 생성
  p <- ggplot(combined_df, aes(x = Sub_Sub_Category, y = Value_new, group = 1)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(title = "Time Series of Value_new",
         x = "Sub_Sub_Category",
         y = "Value_new") +
    scale_x_discrete(expand = c(0, 0))
  
  # plot을 PNG 형식으로 저장
  ggsave(filename = file.path(path_to, paste0(output_file, ".png")), plot = p, bg = "white", width = 10, height = 6)
  
  # 메시지 출력
  cat("\n", crayon::magenta(message), "\n")
}





## 🟧 경로의 추출할 항목들 확인 ====================================================================================================
# 함수 정의
extract_unique_categories <- function(path) {
  # 지정된 경로에서 파일 리스트 가져오기 (재귀적으로 하위 폴더 포함)
  files <- list.files(path, full.names = TRUE, recursive = TRUE)
  
  # 파일 이름만 추출
  file_names <- basename(files)
  
  # "연도_"와 첫 번째 "___" 사이의 문자열 추출
  categories <- str_extract(file_names, "(?<=^[0-9]{4}_)[^_]+")
  
  # 중복 제거 후 반환
  unique_categories <- unique(categories)
  
  return(unique_categories)
}


## 🟧 csv 파일들 처리 ====================================================================================================
library(dplyr)

process_csv_files <- function(path, colnames, which_year) {
  # 파일 리스트 불러오기
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  
  # 결과를 저장할 리스트
  results <- list()
  
  for (file in files) {
    # CSV 파일 읽기
    data <- read.csv(file)
    
    # 열 이름 확인
    col_names <- colnames(data)
    
    # 3번째 열의 값 추출
    third_column <- data[[3]]
    
    # 연도만 추출 (정규표현식 사용)
    years <- third_column[grep("\\d{4}", third_column)]
    
    # which_year에 해당하는 행 추출
    rows_with_year <- data[grep(which_year, third_column), ]
    
    # colnames 내 모든 문자열을 포함하는 열 찾기 (4번째 열부터 Categorized_L3_New 이전까지)
    start_col <- 4
    end_col <- which(col_names == "Categorized_L3_New") - 1
    
    if (length(end_col) == 0) {
      stop("Categorized_L3_New 열이 존재하지 않습니다.")
    }
    
    # colnames 내 모든 문자열을 포함하는 열 찾기
    matching_cols <- col_names[start_col:end_col]
    target_cols <- matching_cols[sapply(matching_cols, function(x) all(sapply(colnames, function(y) grepl(y, x))))]
    
    if (length(target_cols) == 0) {
      warning(paste("파일", file, "에서 모든 colnames에 해당하는 열을 찾지 못했습니다."))
      next
    }
    
    # 결과 저장
    results[[file]] <- list(
      rows_with_year = rows_with_year,
      target_cols = target_cols
    )
  }
  
  return(results)
}

## 🟧 연도 합계 파일이 아닌 파일들 삭제 ====================================================================================================
delete_non_year_files <- function(directory) {
  # 모든 csv 파일을 재귀적으로 찾기
  files <- list.files(directory, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  # 연도 또는 연도를 포함한 문자열을 판단하기 위한 정규 표현식
  year_regex <- "^[0-9]{4}.*$"
  
  # 파일 삭제 카운터
  deleted_files <- 0
  
  for (file in files) {
    # 파일 이름에서 확장자 제거
    file_name <- basename(file)
    file_name <- sub("\\.csv$", "", file_name)
    
    # 파일 이름을 "___"로 분할
    parts <- unlist(strsplit(file_name, "___"))
    
    # 두 번째 부분이 연도 또는 연도를 포함한 문자열인지 확인
    if (length(parts) >= 2) {
      second_part <- parts[2]
      if (!grepl(year_regex, second_part)) {
        # 연도 또는 연도를 포함한 문자열이 아니면 파일 삭제
        file.remove(file)
        deleted_files <- deleted_files + 1
        cat("Deleted:", file, "\n")
      }
    }
  }
  
  cat("Total files deleted:", deleted_files, "\n")
}

## 🟧 연도별 내보내고 최빈값으로 합치기 ====================================================================================================
each_year_total_copy_data_by_year_by_max_value_rows = function(yb, 
                                                               path_from, 
                                                               path_to,
                                                               L3, 
                                                               item,
                                                               years_regions = c(),
                                                               total_include.list,
                                                               total_exclude.list, 
                                                               message = T,
                                                               remove.files = F){
  # path_to = path_to_upper
  if(length(years_regions) > 0){
    for(k in seq_along(total_include.list)){
      # k=1
      # k=i=1
      # i=26
      tryCatch({
        cat(crayon::blue("\nStarting outer loop with k = "), k, "\n")
        for(i in seq_along(years_regions)){
          cat(crayon::blue(k, i, "\n"))
          tryCatch({
            cat(crayon::blue("  Starting inner loop with k = "), k, " and i = ", i, "\n")
            
            ith_year = years_regions[i]
            
            # change
            kth_include.list = lapply(total_include.list[[k]], function(x){ c(x, ith_year) })
            kth_exclude.list = total_exclude.list[[k]]
            kth_sub_category = names(total_include.list)[k]
            kth_path_to = file.path(path_to, kth_sub_category) # path 설정
            
            # function
            ith_results = copy_data_by_year(yb,
                                            path_from,
                                            path_to = kth_path_to,
                                            save_file_name = paste0(L3, "___", kth_sub_category, "___", ith_year),
                                            include.list = kth_include.list,
                                            exclude.list = kth_exclude.list,
                                            message,
                                            remove.files)
            
            cat(crayon::blue("  Finished inner loop with k = "), k, " and i = ", i, "\n")
          }, error = function(e) {
            cat(crayon::red("Error in inner loop with k = "), k, " and i = ", i, ": ", e$message, "\n")
            stop(e)
          })
        }
        # remove.files=F
        tryCatch({
          # 각 추출된 연도별 데이터 합치기 (최빈값 기준)
          process_and_export_most_frequent_value_rows(path_to = kth_path_to, output_file = paste0(L3, "___", kth_sub_category, "___Total"))
          
          cat(crayon::green("Exporting is done: "), crayon::bgMagenta(paste0(L3, "_", kth_sub_category)), "\n")
        }, error = function(e) {
          cat(crayon::red("Error in process_and_export_most_frequent_value_rows with k = "), k, ": ", e$message, "\n")
          stop(e)
        })
        
        cat(crayon::blue("Finished outer loop with k = "), k, "\n")
      }, error = function(e) {
        cat(crayon::red("Error in outer loop with k = "), k, " and last i = ", if (exists("i")) i else "not started", ": ", e$message, "\n")
        stop(e)
      })
    }
  }else{
    for(k in seq_along(total_include.list)){
      # k=1
      # k=i=1
      # i=26
      tryCatch({
        cat(crayon::blue("\nStarting outer loop with k = "), k, "\n")
        tryCatch({
          
          # change
          kth_include.list = total_include.list[[k]]
          kth_exclude.list = total_exclude.list[[k]]
          kth_sub_category = names(total_include.list)[k]
          # kth_path_to = file.path(path_to, kth_sub_category) # path 설정
          
          # function
          kth_results = copy_data_by_year(yb,
                                          path_from,
                                          path_to = path_to,
                                          save_file_name = paste0(kth_sub_category),
                                          include.list = kth_include.list,
                                          exclude.list = kth_exclude.list,
                                          message,
                                          remove.files)
          
          
        }, error = function(e) {
          cat(crayon::red("Error in inner loop with k = "), k, " and i = ", i, ": ", e$message, "\n")
          stop(e)
        })
        
        # remove.files=F
        # tryCatch({
        #   # 각 추출된 연도별 데이터 합치기 (최빈값 기준)
        #   process_and_export_most_frequent_value_rows(path_to = kth_path_to, output_file = paste0(L3, "___", kth_sub_category, "___Total"))
        #   
        #   cat(crayon::green("Exporting is done: "), crayon::bgMagenta(paste0(L3, "_", kth_sub_category)), "\n")
        # }, error = function(e) {
        #   cat(crayon::red("Error in process_and_export_most_frequent_value_rows with k = "), k, ": ", e$message, "\n")
        #   stop(e)
        # })
        
        cat(crayon::blue("Finished outer loop with k = "), k, "\n")
      }, error = function(e) {
        cat(crayon::red("Error in outer loop with k = "), k, " and last i = ", if (exists("i")) i else "not started", ": ", e$message, "\n")
        stop(e)
      })
    }
  }
}
# message=F


## 🟧 리스트 2개 비교 =============================================================
# 두 개의 리스트를 비교하여 길이와 원소 이름이 동일한지 확인하는 함수
# 두 개의 리스트를 비교하여 길이와 원소 이름이 동일한지 확인하는 함수
library(crayon)

# 두 개의 리스트를 비교하여 길이와 원소 이름이 동일한지 확인하는 함수
compare_lists <- function(list1, list2) {
  # 두 리스트의 길이 비교
  if (length(list1) != length(list2)) {
    cat(red("The lists have different lengths.\n"))
    return(FALSE)
  }
  
  # 두 리스트의 원소 이름 비교
  names1 <- names(list1)
  names2 <- names(list2)
  
  if (is.null(names1) || is.null(names2)) {
    cat(red("One or both of the lists do not have names.\n"))
    return(FALSE)
  }
  
  if (!all(names1 == names2)) {
    cat(red("The lists have different names.\n"))
    return(FALSE)
  }
  
  cat(green("The lists have the same length and names.\n"))
  return(TRUE)
}



## 🟧 csv 파일 필 ========================================================================
get_filtered_csv_files <- function(path, include = NULL, exclude = NULL) {
  # Load necessary library
  library(stringr)
  
  # List all CSV files recursively
  all_files <- list.files(path, pattern = "\\.csv$", recursive = TRUE, full.names = FALSE)
  
  # Extract the specific part of the file name
  extract_name_part <- function(filename) {
    parts <- str_split(filename, "___")[[1]]
    if (length(parts) >= 2) {
      return(paste(parts[1], parts[2], sep = "___"))
    } else {
      return(NA)
    }
  }
  
  extracted_names <- sapply(all_files, extract_name_part)
  valid_files <- !is.na(extracted_names)
  extracted_names <- extracted_names[valid_files]
  all_files <- all_files[valid_files]
  
  # Filter based on include criteria (all strings in include must be present)
  if (!is.null(include)) {
    include_indices <- sapply(include, function(inc) grepl(inc, extracted_names))
    include_indices <- apply(include_indices, 1, all)
  } else {
    include_indices <- rep(TRUE, length(extracted_names))
  }
  
  # Filter based on exclude criteria (any string in exclude must not be present)
  if (!is.null(exclude)) {
    exclude_indices <- sapply(exclude, function(exc) grepl(exc, extracted_names))
    exclude_indices <- apply(exclude_indices, 1, any)
  } else {
    exclude_indices <- rep(FALSE, length(extracted_names))
  }
  
  final_indices <- include_indices & !exclude_indices
  filtered_files <- all_files[final_indices]
  
  # Return only the names of the filtered files
  return(basename(filtered_files))
}
