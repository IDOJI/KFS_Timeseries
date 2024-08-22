# 🟥 데이터 로드 ==================================================================================
path_2020 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_2/comparison 2020.csv"
path_2021 = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_2/comparison 2021.csv"

data_2020 = read.csv(path_2020)
data_2021 = read.csv(path_2021)



# 🟨 데이터 합치기 ==================================================================================
names(data_2020)
names(data_2021)

data_combined = rbind(data_2020, data_2021)
View(data_combined)



# 🟨 어린나무 가꾸기, 큰나무 가꾸기만 남기기==================================================================================
data_combined_2 = data_combined %>% filter(forest_tending %in% c("어린나무가꾸기", "큰나무가꾸기"))

data_young = data_combined_2 %>% filter(forest_tending == "어린나무가꾸기")
data_big = data_combined_2 %>% filter(forest_tending  == "큰나무가꾸기")
View(data_big)



# 🟩 Bland-Altman test ==================================================================================
## 🌫️ young =======================================================================================
# install.packages("devtools")
# devtools::install_github("deepankardatta/blandr")
require(blandr)
data = data_young

# data :  ha
dig = data$total_work_area_digital_ha
yb = data$total_work_area_yb

# Perform Bland-Altman analysis and display the statistics
stats <- blandr.statistics(dig, yb)


# Display a Bland-Altman plot
blandr::blandr.display.and.draw(method1 = dig,
                                method2 = yb, 
                                method1name = "Digital", 
                                method2name = "YearBook", 
                                sig.level = 0.95, 
                                annotate = T, 
                                ciDisplay = T)



# Plot using ggplot2
blandr.plot.ggplot(stats)





# my function
labels = paste(data$regions, 
               data$year,
               sep = "_")

bland_altman_with_ci_and_labels(method1 = dig,
                         method2 = yb, 
                         labels = labels, 
                         title = "Bland-Altman Plot with C.I. : 어린나무가꾸기")


# 생성된 플롯을 파일로 저장
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_2"
ggsave(filename = file.path(path_save, "bland_altman_plot_young.png"), 
       plot = last_plot(), 
       width = 4, height = 5, dpi = 300, bg = "white")


## 🌫 big =======================================================================================
# install.packages("devtools")
# devtools::install_github("deepankardatta/blandr")
require(blandr)
data = data_big 

# data :  ha
dig = data$total_work_area_digital_ha
yb = data$total_work_area_yb

# Perform Bland-Altman analysis and display the statistics
stats <- blandr.statistics(dig, yb)


# Display a Bland-Altman plot
blandr::blandr.display.and.draw(method1 = dig,
                                method2 = yb, 
                                method1name = "Digital", 
                                method2name = "YearBook", 
                                sig.level = 0.95, 
                                annotate = T, 
                                ciDisplay = T)



# Plot using ggplot2
blandr.plot.ggplot(stats)





# my function
labels = paste(data$regions, 
               data$year,
               sep = "_")
bland_altman_with_ci_and_labels(method1 = dig,
                                method2 = yb, 
                                labels = labels, 
                                title = "Bland-Altman Plot with C.I. : 큰가꾸기")


# 생성된 플롯을 파일로 저장
path_save = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/5.디지털숲가꾸기/Exported_2"
ggsave(filename = file.path(path_save, "bland_altman_plot_big.png"), 
       plot = last_plot(), 
       width = 4, height = 5, dpi = 300, bg = "white")


