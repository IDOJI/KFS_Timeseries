# 🟥 Data Load #####################################################################################################
file_path = "/Users/Ido/Documents/GitHub/KFS_Timeseries_Data/2.Rearrange YB names/1.rearranged_yb_names.csv"
data = read.csv(file_path)
# View(data)
L2 = data$NAME_L2 %>% unique
length(L2)
# View(data)
data$NAME_L3 %>% unique






# 🟥 clustering L2 category #####################################################################################################
k=60
clutered_L2 = text_clustering(L2, k_min = k, k_max = k)
clutered_L2$plot_cluster
cluster = clutered_L2$clusters













# 🟥 L2 카테고리 묶기 #####################################################################################################
## 🟧 데이터 ======================================================================================
text_data = L2

## 🟧 결과 저장 리스트 ======================================================================================
combined.list = list()


## 🟩 국제산림통계 ======================================================================================
combined.list$`국제 산림 통계_International Statistics` =
  union_multiple(filter_text_data(text_data, c("international", "statistics")),
                 filter_text_data(text_data, c("국제", "통계")),
                 filter_text_data(text_data, c("국제", "산림")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]



## 🟩 조림/보호 ======================================================================================
combined.list$`조림/보호/사방_Reforestation/protection/Erosion Control` =
  union_multiple(filter_text_data(text_data, c("reforestation", "protection")),
                 filter_text_data(text_data, c("조림", "보호")),
                 filter_text_data(text_data, c("조림", "사방")),
                 filter_text_data(text_data, c("Forest", "protection", "damage")),
                 filter_text_data(text_data, c("산림", "보호", "피해")),
                 filter_text_data(text_data, c("산림", "보호")),
                 filter_text_data(text_data, c("Forest", "health", "diversity")),
                 filter_text_data(text_data, c("산림", "건강", "다양성")))

text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 3, k_max = 3)[[1]]




## 🟩 국토와 자연환경 ======================================================================================
combined.list$`국토와 자연환경_Land & Natural Environment` =
  union_multiple(filter_text_data(text_data, c("land", "natural", "environment")),
                 filter_text_data(text_data, c("국토", "자연", "환경")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]






## 🟩 임야/산림 경영/관리 ======================================================================================
combined.list$`임업/산림 경영/관리_Forest Management` =
  union_multiple(filter_text_data(text_data, 
                                  c("forest", "management"), c("국유", "national")),
                 filter_text_data(text_data, 
                                  c("산림", "경영"), "국유"),
                 filter_text_data(text_data, 
                                  c("임업", "경영"), "국유"))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]




## 🟩 임산물무역 가공 및 유통 ======================================================================================
combined.list$`임산 무역 가공 및 유통_Trede Processing and marketing of forest products` =
  union_multiple(filter_text_data(text_data, c("trade", "processing", "marketing", "forest", "products")),
                 filter_text_data(text_data, c("임산", "무역", "가공", "유통")),
                 filter_text_data(text_data, c("임산", "무역", "가공")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]







## 🟩 부록 ======================================================================================
combined.list$`부록_Appendix` =
  union_multiple(filter_text_data(text_data, c("appendix")),
                 filter_text_data(text_data, c("부록")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]






## 🟩 국민 계정과 생산가격지수 ======================================================================================
combined.list$`국민계정과 생산가격지수_National Accounts and Index number of Products Price` =
  union_multiple(filter_text_data(text_data, c("national", "accounts", "index", "number", "products", "price")),
                 filter_text_data(text_data, c("국민계정", "생산가격지수")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]




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
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]






## 🟩 임산물 가격 및 기타 가격 ======================================================================================
combined.list$`임산물 가격 및 기타가격_Price of forest Products & Major Commodities` =
  union_multiple(filter_text_data(text_data, c("price", "forest", "product", "major", "commodities")),
                 filter_text_data(text_data, c("임산물", "기타","가격")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]







## 🟩 ⭐️임산물 시장 ======================================================================================
combined.list$`임산물시장_Forest Product Market` =
  union_multiple(filter_text_data(text_data, c("market", "forest", "product"), c("trede", "trade")),
                 filter_text_data(text_data, c("임산물", "시장"), c("trede", "무역")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]







## 🟩 ⭐️임업 생산 및 공급 ======================================================================================
combined.list$`임업생산_Forest production` =
  union_multiple(filter_text_data(text_data, c("forest", "production")),
                 filter_text_data(text_data, c("임업", "생산")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]






## 🟩 재정과 금융 ======================================================================================
combined.list$`재정과 금융_Finances and Loans` =
  union_multiple(filter_text_data(text_data, c("finance", "loan")),
                 filter_text_data(text_data, c("재정", "금융")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 30, k_max = 30)[[1]]








## 🟩 ⭐️국유림관리 ======================================================================================
combined.list$`국유림관리_National Forest Management` =
  union_multiple(filter_text_data(text_data, c("National", "forest", "Management")),
                 filter_text_data(text_data, c("국유", "관리")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







## 🟩 ⭐️임야/산림 서비스 ======================================================================================
combined.list$`임야/산림 서비스_Forest Service` =
  union_multiple(filter_text_data(text_data, c("forest", "service")),
                 filter_text_data(text_data, c("산림", "서비스")),
                 filter_text_data(text_data, c("임야", "서비스")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 3, k_max = 3)[[1]]







## 🟩 ⭐️교육 훈련 ======================================================================================
combined.list$`교육 훈련_Education and Training` =
  union_multiple(filter_text_data(text_data, c("education", "training")),
                 filter_text_data(text_data, c("교육", "훈련")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 3, k_max = 3)[[1]]




## 🟩 사유림 관리 ======================================================================================
combined.list$`사유림 관리_Private Forest Administration` =
  union_multiple(filter_text_data(text_data, c("private", "forest")),
                 filter_text_data(text_data, c("사유림", "관리")),
                 filter_text_data(text_data, c("사유림")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
text_clustering(text_data, k_min = 3, k_max = 3)[[1]]








## 🟩 산림자원 ======================================================================================
combined.list$`산림자원_Forest Resources` =
  union_multiple(filter_text_data(text_data, c("forest", "resource")),
                 filter_text_data(text_data, c("산림", "자원")))
text_data = L2
text_data = text_data[!text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 3, k_max = 3)[[1]]





# 🟥 하나의 데이터 프레임으로 합치기 ==============================================================================
# 새로운 열 생성
combined_data = data
combined_data$Categorized_L2 <- NA

# 각 카테고리 이름을 해당 L2 값에 할당
for (category_name in names(combined.list)) {
  category_values <- combined.list[[category_name]]
  combined_data$Categorized_L2[combined_data$NAME_L2 %in% category_values] <- category_name
}
combined_data = combined_data %>% relocate(Categorized_L2) %>% arrange(Categorized_L2)

class(combined_data)
View(combined_data)

combined_data$Categorized_L2 %>% is.na %>% sum



# 🟥 check ==============================================================================
# L3 = combined_data$NAME_L3
# L3[grep("숲", L3)]


# 🟥 Export ==============================================================================
path_save = "/Users/Ido/Documents/DataAnalysis/KFS_Timeseries/2.Rearrange YB names"
write.csv(combined_data, paste0(path_save, "/2.L2 Categorized data.csv"), row.names=F)










