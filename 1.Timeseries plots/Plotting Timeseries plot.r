# 🟥 Load Functions & Packages ##########################################################################
## 🟨Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE)
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

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)


## 🟨Loading my functions ======================================================
# Check my OS
os <- Sys.info()["sysname"]
if(os ==  "Darwin"){
  
  path_OS = "/Users/Ido" # mac
  
}else if(os ==  "Window"){
  
  path_OS = "C:/Users/lleii"  
  
}
path_Dropbox = paste0(path_OS, "/Dropbox")
path_GitHub = list.files(path_Dropbox, pattern = "GitHub", full.names = T)
path_GitHub_Code = paste0(path_GitHub, "/GitHub___Code")
Rpkgs = c("ADNIprep", "StatsR", "refineR", "dimR")
Load = sapply(Rpkgs, function(y){
  list.files(path = path_GitHub_Code, pattern = y, full.names = T) %>% 
    paste0(., "/", y,"/R") %>% 
    list.files(., full.names = T) %>% 
    purrr::walk(source)
})








# 🟥 Plotting Time-series on the specified variables #######################################
## 🟨 Load data =================================================================
### 🟧 Load data ==============================================
path_data = "/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/✴️DataAnalysis___KFS_Timeseries/Data/2-16%7E18.xlsx"
path_data_new = "/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/✴️DataAnalysis___KFS_Timeseries/Data/2-16%7E18_Selected.xlsx"
data = readxl::read_xlsx(path_data_new, col_names = T)

# 각 행에서 모든 열이 NA인지 확인
complete_rows <- apply(data, 1, function(row) all(is.na(row)))

# 모든 열이 NA가 아닌 행 선택
data <- data[!complete_rows, ]





### 🟧 Extract colnames ==============================================
col_names = data[1,] %>% unlist %>% unname %>% na.omit %>% as.vector

# \n을 기준으로 분할하여 영문과 한글 부분을 분리
split_names <- strsplit(col_names, "\n")

# 영문 부분을 ()의 괄호 안에 넣은 새로운 벡터 생성
new_col_names <- sapply(split_names, function(x) {
  if(length(x) == 2) {
    return(paste(x[2]))
  } else {
    return(x)
  }
})


# what value
Value = data[2,2] %>% unlist %>% unname
Value = "Estimated annual carbon sequestration(연간예상 탄소 흡수량)"




### 🟧 Split Data by year =======================================================
# Remove rows
data_split = data[-c(1:3),] %>% as.data.frame



# Remove the first row
row_names = data_split[[1]]
row_names[1] = "Total"
data_split = data_split[,-1]



# colnames
names(data_split) = new_col_names


# Grouping Index
group1_indices <- seq(1, length(row_names), by = 3)
group2_indices <- seq(2, length(row_names), by = 3)
group3_indices <- seq(3, length(row_names), by = 3)



# Extract elements by group
data_split_1 = data_split[group1_indices, ]
data_split_2 = data_split[group2_indices, ]
data_split_3 = data_split[group3_indices, ]


# change rownames
rownames(data_split_1) = row_names[group1_indices]
rownames(data_split_2) = row_names[group1_indices]
rownames(data_split_3) = row_names[group1_indices]

# Combine Results
result <- list("year_total" = data_split_1, 
               "transactional" = data_split_2,
               "nontransactional" = data_split_3)




### 🟧 Split total value =======================================================
# Extract the first row(Total value)
Total = lapply(result, function(df){
  as_tibble(df)[1,]
}) %>% setNames(paste0("Total___", names(result)))


# Remove the first row
remove_Total = lapply(result, function(df){
  df[-1,]
}) %>% setNames(names(result))


# Combine
combined.list = c(Total, remove_Total)




## 🟨 Generate Year variable ===============================================
combined.list$year_total = combined.list$year_total %>% rownames_to_column("Year")
combined.list$transactional = combined.list$transactional %>% rownames_to_column("Year")
combined.list$nontransactional = combined.list$nontransactional %>% rownames_to_column("Year")


## 🟨 Results list for saving ===============================================
Results.list = list()



## 🟨 💎Plots: Each variable with zeros ===============================================
### 🟧 save list =========================================================================
results_1.list = list()


### 🟧 Total =========================================================================
total = combined.list$year_total
categories = names(total)[-1]
results_1.list[[1]] = plots_each_total = lapply(categories, function(ith_category){
  ggplot___lines(df = total[,c("Year", ith_category)],
                 col_names = NULL,
                 x_col = "Year",
                 point = T,
                 show.legend = T,
                 title = paste0("Time-series: ", ith_category), 
                 xlab = "Year",
                 ylab = "Estimated sequestration",
                 color_legend_title = "Variable")
}) %>% setNames(categories)
names(results_1.list)[1] = "1.Each___Total"




### 🟧 transactional =========================================================================
transactional = combined.list$transactional
categories = names(transactional)[-1]
results_1.list[[2]] = plots_each_transactional = lapply(categories, function(ith_category){
  ggplot___lines(df = transactional[,c("Year", ith_category)],
                 col_names = NULL,
                 x_col = "Year",
                 point = T,
                 show.legend = T,
                 title = paste0("Time-series: ", ith_category), 
                 xlab = "Year",
                 ylab = "Estimated sequestration",
                 color_legend_title = "Variable")
}) %>% setNames(categories)
names(results_1.list)[2] = "2.Each_Transactional"



### 🟧 nontransactional =========================================================================
nontransactional = combined.list$nontransactional
categories = names(nontransactional)[-1]
results_1.list[[3]] = plots_each_nontransactional = lapply(categories, function(ith_category){
  ggplot___lines(df = nontransactional[,c("Year", ith_category)],
                 col_names = NULL,
                 x_col = "Year",
                 point = T,
                 show.legend = T,
                 title = paste0("Time-series: ", ith_category), 
                 xlab = "Year",
                 ylab = "Estimated sequestration",
                 color_legend_title = "Variable")
}) %>% setNames(categories)
names(results_1.list)[3] = "3.Each_NonTransactional"



### 🟧 Final Results =========================================================================
Results.list[[1]] = results_1.list
names(Results.list)[1] = "1.Each Variable"



## 🟨 💎Plots: All variables ===============================================
### 🟧 save list =========================================================================
result_2.list = list()



### 🟧 Total =========================================================================
result_2.list[[1]] = ggplot___lines(df = combined.list$year_total,
                                   col_names = NULL,
                                   x_col = "Year",
                                   point = T,
                                   show.legend = T,
                                   title = paste0("All Variables: Total"), 
                                   xlab = "Year",
                                   ylab = "Estimated sequestration",
                                   color_legend_title = "Variable")
names(result_2.list)[1] = "1.All_Total"





### 🟧 transactional =========================================================================
result_2.list[[2]] = ggplot___lines(df = combined.list$transactional,
                                   col_names = NULL,
                                   x_col = "Year",
                                   point = T,
                                   show.legend = T,
                                   title = paste0("All Variables: Transactional"), 
                                   xlab = "Year",
                                   ylab = "Estimated sequestration",
                                   color_legend_title = "Variable")
names(result_2.list)[2] = "2.All_Transactional"


### 🟧 nontransactional =========================================================================
result_2.list[[3]] = ggplot___lines(df = combined.list$nontransactional,
                                   col_names = NULL,
                                   x_col = "Year",
                                   point = T,
                                   show.legend = T,
                                   title = paste0("All Variables: NonTransactional"), 
                                   xlab = "Year",
                                   ylab = "Estimated sequestration",
                                   color_legend_title = "Variable")
names(result_2.list)[3] = "3.All_NonTransactional"


### 🟧 combine =========================================================================
Results.list[[2]] = result_2.list
names(Results.list)[2] = "2.All variables"



## 🟨 💎bar plot: summed-up Comparison ===============================================
### 🟧 result list ========================================================================
results_3.list = list()

### 🟧 Total ========================================================================
results_3_Total.list = list()
#### 🟩 Data setting ========================================================================
input_vector = combined.list$Total___year_total %>% unlist
xlab = names(input_vector)
input_vector = as.numeric(input_vector) %>% setNames(xlab) %>% sort



#### 🟩 log Y ========================================================================
results_3_Total.list[[1]] = ggplot___barplot(input_vector = input_vector,
                                     log_y = F,
                                     xlab = names(input_vector),
                                     title = "Comparison of Tatal value",
                                     xlab_title = "Total value",
                                     ylab_title = "Estimated sequestration",
                                     adding.values = TRUE)
names(results_3_Total.list)[1] = "Comparison of Tatal value"


#### 🟩 log Y ========================================================================
results_3_Total.list[[2]] = ggplot___barplot(input_vector = input_vector,
                                     log_y = T,
                                     xlab = names(input_vector),
                                     title = "Comparison of Tatal value (log transformation)",
                                     xlab_title = "Total value",
                                     ylab_title = "Estimated sequestration",
                                     adding.values = TRUE)
names(results_3_Total.list)[2] = "Comparison of Tatal value_Log"


#### 🟩 Combine ========================================================================
results_3.list[[1]] = results_3_Total.list
names(results_3.list)[1] = "1.Total"



### 🟧 Transactional ========================================================================
#### 🟩 result list ========================================================================
results_3_trans.list = list()

#### 🟩 Data setting ========================================================================
input_vector = combined.list$Total___transactional %>% unlist
xlab = names(input_vector)
input_vector = as.numeric(input_vector) %>% setNames(xlab) %>% sort



#### 🟩 Y ========================================================================
results_3_trans.list[[1]] = ggplot___barplot(input_vector = input_vector,
                                     log_y = F,
                                     xlab = names(input_vector),
                                     title = "Comparison of Tatal Trasactional Value",
                                     xlab_title = "Total Trasactional Value",
                                     ylab_title = "Estimated sequestration",
                                     adding.values = TRUE)
names(results_3_trans.list)[1] = "Comparison of Tatal Transctional value"





#### 🟩 log Y ========================================================================
results_3_trans.list[[2]] = ggplot___barplot(input_vector = input_vector,
                                     log_y = T,
                                     xlab = names(input_vector),
                                     title = "Comparison of Tatal value (log transformation)",
                                     xlab_title = "Total Trasactional Value",
                                     ylab_title = "Estimated sequestration",
                                     adding.values = TRUE)
names(results_3_trans.list)[2] = "Comparison of Tatal Transactional value_Log"



#### 🟩 combine ========================================================================
results_3.list[[2]] = results_3_trans.list
names(results_3.list)[2] = "2.Transactional"



### 🟧 NonTransactional ========================================================================
#### 🟩 save list ========================================================================
results_3_non.list = list()



#### 🟩 Data setting ========================================================================
input_vector = combined.list$Total___nontransactional %>% unlist
xlab = names(input_vector)
input_vector = as.numeric(input_vector) %>% setNames(xlab) %>% sort



#### 🟩 Y ========================================================================
results_3_non.list[[1]] = ggplot___barplot(input_vector = input_vector,
                                     log_y = F,
                                     xlab = names(input_vector),
                                     title = "Comparison of Tatal Nontrasactional Value",
                                     xlab_title = "Total Nontrasactional Value",
                                     ylab_title = "Estimated sequestration",
                                     adding.values = TRUE)
names(results_3_non.list)[1] = "Comparison of Tatal Nontransctional value"


#### 🟩 log Y ========================================================================
results_3_non.list[[2]] = ggplot___barplot(input_vector = input_vector,
                                      log_y = T,
                                      xlab = names(input_vector),
                                      title = "Comparison of Tatal Nontrasactional Value",
                                      xlab_title = "Total Nontrasactional Value",
                                      ylab_title = "Estimated sequestration",
                                      adding.values = TRUE)
names(results_3_non.list)[2] = "Comparison of Tatal Nontransctional value"



#### 🟩 combine ========================================================================
results_3.list[[3]] = results_3_non.list
names(results_3.list)[3] = "3.Nontrasactional"


  

### 🟧 Combine ========================================================================
Results.list[[3]] = results_3.list
names(Results.list)[3] = "3.Summed_Bar"  



## 🟨 Save the results =======================================================================
path_save = "/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/✴️DataAnalysis___KFS_Timeseries/Results"
saveRDS(object = Results.list, file = paste0(path_save, "/1.Timeseries_plots.rds"))




















