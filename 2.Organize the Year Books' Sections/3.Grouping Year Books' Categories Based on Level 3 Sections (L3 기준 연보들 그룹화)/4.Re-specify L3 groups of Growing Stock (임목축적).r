# 🟥 data   ========================================================================================================
data = read.csv("/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names/3.3.Exclude L3.csv")
# View(data)



# 🟥 "다. 기타_Others___사유임야 소유형태별 임야/산림면적표_Table of Private Forest Area by Ownership Form"    ========================================================================================================
data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3 == "다. 기타_Others___사유임야 소유형태별 임야/산림면적표_Table of Private Forest Area by Ownership Form", "사유림 소유형태별 산림면적_Private Forest Land Area by Ownership", Categorized_L3)) %>% 
  relocate(Categorized_L3_New)




# 🟥 산지이용구분  ========================================================================================================
data  = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3 %in% c("산지 이용 구분_Classification of Forest Land Utilization", "산지 이용 구분 현황_Status of Forest Land Use Classification"), "산지 이용 구분 현황_Status of Forest Land Use Classification", Categorized_L3_New))



# 🟥 임상별 영급별 산림면적 및 임목축적 ========================================================================================================
data  = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3 %in% c("임상별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Forest Types and Age Classes",
                                                           "임상별, 영급별 임야면적 및 임목축적_Forest Area and Growing Stock by Forest Type and Age-Classes"), "임상별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Forest Types and Age Classes", Categorized_L3_New))






# 🟥 임목축적만 확인  ========================================================================================================
## 🟧 L3 체크   ========================================================================================================
# data$Categorized_L2 %>% unique
data_sub = data %>% filter(Categorized_L2 %in% "임야/산림면적 및 임목축적_Forest Land Area & Growing Stock")
data_sub$Categorized_L3_New %>% unique





## 🟧 "영림서편_Details by National Forest Station"   ========================================================================================================
item = "영림서편_Details by National Forest Station"
data_sub = data %>% filter(Categorized_L3 == item)

# 각 항목들 확인
L4 = data_sub$NAME_L4 %>% unique
data_sub$NAME_L5 %>% unique
data_sub$year %>% unique
L4_clustering = text_clustering(L4, 7, 7)[[1]]




### 🟨 행정구역별 소관별 지종별 임야면적, 임목축적 ================================================================================================
L4_items = c("5. 행정구역별, 소관별, 지종별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Administrative districts Authorities concerned and Land Classification"   ,
          "5. 행정구역별, 소관별, 지종별, 임야면적 및 임목축적_Forest Land Area and Growing Stock by Administrative districts Authorities Concerned and Land Classification",
          "5. 행정구역별, 소관별, 지종별 임야면적 및 임목축적_5. Forest Land Area and Growing Stock by Administrative districts Authorities Concerned and Land Classification",
          "5. 행정구역별,소관별,지종별 임야면적 및 임목축적_5. Forest Land Area and Growing Stock by Administrative district Authorities Concerned and Land Classification")




include = c("행정", "소관", "지종", "축적", "면적")
exclude = NULL
filter_values(data, "Categorized_L3_New", include, exclude)

new_L3 = "행정구역별,소관별,지종별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Administrative district Authorities Concerned and Land Classification"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New


# New L4 
L4 = L4[!L4 %in% L4_items]
k=4
text_clustering(L4, k, k)[[1]]




### 🟨 임상별 영급별 임야면적, 임목축적 ================================================================================================
L4_items = c("4. 임상별, 영급별 임야면적 및 임목축적_4. Forest Area and Growing Stock by Forest Type and Age-Classes",
             "4. 임상별, 영급별 임야면적_4. Forest Area by Forest Type and Age-Classes",
             "4. 임상별, 영급별 임야면적 및 임목축적_Forest Area and Growing Stock by Forest Type and Age-Classes",
             "4. 임상별, 영급별 임야면적 및 임목축적_4. Forest Area by Forest Type and Age-Classes",
             "4. 임상별 영급별 임야면적_Forest Area by Forest Type and Age-Classes",
             "4. 임상별 영급별 임야면적_4. Forest Area by Forest Type and Age-Classes")
          

include = c("임상별", "영급별", "축적", "면적")
exclude = c("행정", "구성", "지종별", "산지")
filter_values(data, "Categorized_L3_New", include, exclude)


new_L3 = "임상별 · 영급별 산림면적 및 임목축적_Forest Area and Growing Stock by Forest Types and Age Classes"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New



# New L4 
L4 = L4[!L4 %in% L4_items]
k=2
text_clustering(L4, k, k)[[1]]





### 🟨 지종별 임야면적, 임목축적 ================================================================================================
L4_items = c("5. 지종별 임야면적 및 임목축적_5. Forest Area and Growing Stock by Land Classification" ,  
             "5. 지종별ㆍ임야면적 및 임목축적_5. Forest Area and Growing Stock by Land Classification")


include = c("지종별", "축적", "면적")
exclude = c("행정", "구성", "산지", "임상별", "행정", "기관별", "도별", "소유별", "소관별", "영급별", "소관")
filter_values(data, "Categorized_L3_New", include, exclude)


new_L3 = "지종별ㆍ임야면적 및 임목축적_Forest Area and Growing Stock by Land Classification"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New


# New L4 
L4 = L4[!L4 %in% L4_items]
# k=2
# text_clustering(L4, k, k)[[1]]











### 🟨 "소관별 지종별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Authorities Concerned and Land Classification" ================================================================================================
L4_items = c("5. 소관별 지종별 임야면적 및 임목축적_5. Forest Land Area and Growing Stock by Authorities Concerned and Land Classification")

include = c("소관별", "지종별", "축적", "면적")
exclude = c("행정", "구성", "지종별", "산지")
filter_values(data, "Categorized_L3_New", include, exclude)


new_L3 = "소관별 지종별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Authorities Concerned and Land Classification"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New


# New L4 
# L4 = L4[!L4 %in% L4_items]
# k=2
# text_clustering(L4, k, k)[[1]]



## 🟧 "가. 전국 및 시ㆍ도ㆍ서편_Details by country province city and N.F.S."===========================================================
item = "가. 전국 및 시ㆍ도ㆍ서편_Details by country province city and N.F.S."
data_sub = data %>% filter(Categorized_L3 == item)

# 각 항목들 확인
L4 = data_sub$NAME_L4 %>% unique
data_sub$NAME_L5 %>% unique
data_sub$year %>% unique
# k = 100
# L4_clustering = text_clustering(L4, k, k)[[1]]






### 🟨 행정구역별 임야면적 및 임목축적_3. Forest Land Area and Growing Stock by Administative Districts" ================================================================================================
L4_items <- c(
  "3. 행정구역별 임야면적 및 임목축적_3. Forest Land Area and Growing Stock by Administative Districts", 
  "3. 행정구역별 임야면적 및 임목축적_3. Forest Land Area and Growing Stock by administrative districts",
  "3. 행정구역별 임야면적 및 임목축적_Forest Land Area and Growing Stock by administrative districts",   
  "3. 행정구역별 임야면적 및 임목축적_Forest Land Area and Growing stock by administrative districts",   
  "3. 행정구역별 임야면적 및 임목축적_3. Forest Land Area and Growing Stock by Administrative Districts",
  "3. 행정구역별 임야면적 및 임목축적_3. Forest Land Area and Growing stock by administrative districts"
)


include = c("행정", "축적", "면적")
exclude = c("구성", "지종별", "산지")
filter_values(data, "Categorized_L3_New", include, exclude)


new_L3 = "행정구역별 산림면적 및 임목축적_Forest Land Area and Growing Stock by Administrative Districts" 

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New




## 🟧 "구ㆍ시ㆍ군편_Details by GuㆍSiㆍGun" ===========================================================
item = "구ㆍ시ㆍ군편_Details by GuㆍSiㆍGun"
data_sub = data %>% filter(Categorized_L3 == item)

# 각 항목들 확인
L4 = data_sub$NAME_L4 %>% unique
data_sub$NAME_L5 %>% unique
data_sub$year %>% unique
# k = 100
# L4_clustering = text_clustering(L4, k, k)[[1]]






### 🟨 "6. 임상별 임야면적 및 임목축적_6. Forest Land Area and Growing Stock by Forest type" ================================================================================================
L4_items <- c(
  "6. 임상별 임야면적 및 임목축적_6. Forest Land Area and Growing Stock by Forest type"
)


include = c("임상별", "축적", "면적")
exclude = c("구성", "지종별", "산지", "행정", "영급", "연도별", "소관", "기관")
filter_values(data, "Categorized_L3_New", include, exclude)


new_L3 = "임상별 임야면적 및 임목축적_Forest Land Area and Growing Stock by Forest type"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New




### 🟨 "7. 소유별 임야면적 및 임목축적_7. Forest Land Area and Growing Stock by ownership"  ================================================================================================
L4_items <- c(
  "7. 소유별 임야면적 및 임목축적_7. Forest Land Area and Growing Stock by ownership" 
)


include = c("소유별", "축적", "면적")
exclude = c("구성", "지종별", "산지", "행정", "영급", "연도별", "소관", "기관")
filter_values(data, "Categorized_L3_New", include, exclude)


new_L3 = "소유별 임야면적 및 임목축적_Forest Land Area and Growing Stock by ownership" 

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New










## 🟧 "기타_Others" ===============================================================================
item = "기타_Others"
data_sub = data %>% filter(Categorized_L3_New == item)

# 각 항목들 확인
L4 = data_sub$NAME_L4 %>% unique
data_sub$NAME_L5 %>% unique
data_sub$year %>% unique
# k = 4
# L4_clustering = text_clustering(L4, k, k)[[1]]



### 🟨 "사유림 소유 규모_Ownership Scale of Private Forest"================================================================================================
L4_items <- c(
  "6. 사유임야 소유 규모_6. Ownership scale of private Forest",
  "6. 사유임야소유규모_Ownership Scale of Private Forest",
  "6. 사유임야 소유 규모_Ownership scale of private Forest",
  "6. 사유임야 소유 규모_6. Ownership Scale of Private Forest",
  "6. 사유임야 소유 규모_6. Ownership Scale of private Forest"
)


include = c("사유", "소유", "규모")
exclude = c("구성", "지종별", "산지", "행정", "영급", "소관", "기관")
filter_values(data, "Categorized_L3_New", include, exclude)

new_L3 = "사유림 소유 규모_Ownership Scale of Private Forest"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New





### 🟨 "연도별 임상별 임야면적_9. Forest Land Area by Forest type and year" ================================================================================================
L4_items <- c(
  "9. 연도별 임상별 임야면적_9. Forest Land Area by Forest type and year",
  "9. 연도별, 임상별 임야면적_Forest Land Area by Forest type and year",
  "9. 연도별 임상별 임야면적_Forest Land Area by Forest type and year"
)

include = c("임상별", "연도별", "면적")
exclude = c("구성", "지종별", "산지", "행정", "영급", "소관", "기관")
filter_values(data, "Categorized_L3_New", include, exclude)

new_L3 = "연도별 임상별 임야면적_Forest Land Area by Forest Type and Year" 

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New








### 🟨 사유임야 소유형태별 임야면적표_8. Table of Private Forest Area by Ownership Form"================================================================================================
L4_items <- c(
  "8. 사유임야 소유형태별 임야면적표_8. Table of Private Forest Area by Ownership Form",
  "8. 사유임야소유형태별 임야면적표_Table of Private Forest Area by Ownership Form",
  "8. 사유임야 소유형태별 임야면적표_Table of Private Forest Area by Ownership Form"
)


include = c("사유", "소유", "형태", "면적")
exclude = c("구성", "지종별", "산지", "행정", "영급", "소관", "기관")
filter_values(data, "Categorized_L3_New", include, exclude)

new_L3 = "사유림 소유형태별 산림면적_Private Forest Land Area by Ownership"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New





### 🟨  "7. 용도별 산지 이용구분 조사실적_7. Forest Land Use Classification" ================================================================================================
L4_items <- c(
  "7. 용도별 산지 이용구분 조사실적_7. Forest Land Use Classification",
  "7. 용도별 산지이용구분조사실적_Forest Land Use Classification",
  "7. 용도별 산지 이용구분 조사실적_Forest Land Use Classification"
)

include = c("산지", "이용", "용도별")
exclude = c( "지종별")
filter_values(df = data, col_name = "Categorized_L3_New", include, exclude)

data = data %>% mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% c("산지 이용 구분 현황_Status of Forest Land Use Classification",
                                                                             "산지이용 구분 현황_Status of Forest Land Use Classification" ), "산지 이용 구분 현황_Status of Forest Land Use Classification", Categorized_L3_New))

new_L3 = "산지 이용 구분 현황_Status of Forest Land Use Classification"

data = data %>% 
  mutate(Categorized_L3_New = ifelse(Categorized_L3_New %in% item & NAME_L4 %in% L4_items, new_L3, Categorized_L3_New))


# Check
new_L3 %in% data$Categorized_L3_New




## 🟧 L3 체크   ========================================================================================================
# data$Categorized_L2 %>% unique
data_sub = data %>% filter(Categorized_L2 %in% "임야/산림면적 및 임목축적_Forest Land Area & Growing Stock")
data_sub$Categorized_L3_New %>% unique







# 🟥 Export ========================================================================================================
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names"
write.csv(data, file.path(path_save, "3.4.Categorized L3 New.csv"), row.names=F)





