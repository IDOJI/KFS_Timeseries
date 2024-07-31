## 🟧 국유림 관리 =====================================================================================================
#### 🟦 소유별 산림/영림계획 작성 현황 ===================================================================================================
name = "소유별 산림/영림경영계획 작성현황_Preparation of Forest Management Plan"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("preparation", "forest", "management", "plan", "소유별")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "작성", "소유별")),
                 filter_text_data(text_data, c("영림", "계획", "작성", "소유별")))
print(combined.list[[name]])
text_data = text_data[! text_data %in% unlist(combined.list)]
# 




#### 🟦 국유림 직영 벌채 사업 생산  ==================================================================================
name = "국유림직영벌채사업생산 및 매각실적_Production and Sales of Timber by Government Felling"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, include = c("production", "timber", "government", "felling")),
                 filter_text_data(text_data, include = c("직영", "벌채", "felling")),
                 filter_text_data(text_data, include = c("관행", "작벌", "매각실적")),
                 filter_text_data(text_data, include = c("국유림", "벌채", "매각")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





#### 🟦 임야 매각 및 매수/ 국유재산 취득 처분  ==================================================================================
name = "임야매각 및 매수/국유재산취득 및 처분_Disposal & purchase of Forest Land"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, include = c("disposal", "purchase", "land", "forest"), "national"),
                 filter_text_data(text_data, include = c("임야", "매각", "매수"), "national"),
                 filter_text_data(text_data, include = c("국유", "재산", "취득"), "national"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 국유림 대부(사용허가)) 현황  ==================================================================================
##### 🟪 불요존 (disposable) =============================================================================================
name = "불요존 국유림 대부(사용허가)현황_Status of land lease out of disposable national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, include = c("lease out", " disposable")),
                 filter_text_data(text_data, include = c("lease out", " dispensable")),
                 filter_text_data(text_data, c("status", "land", "lease", "disposable", "national", "forest"), "indis"),
                 filter_text_data(text_data, c("불요존", "국유임", "대부", "현황")),
                 filter_text_data(text_data, c("불요존", "국유림", "대부", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 요존 (indisposable) =============================================================================================
name = "요존 국유림 대부(사용허가) 현황_Status of Land Lease out of Indispensable National Forest"
combined.list[[name]] = 
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
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 산림/영림계획 편성 실적  ==================================================================================
name = "영림계획 편성실적_Accomplishment of Forest Management Plan by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("accomplishment", "forest", "management", "plan")),
                 filter_text_data(text_data, c("산림", "경영", "계획", "편성", "실적")),
                 filter_text_data(text_data, c("영림", "계획", "편성", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]







#### 🟦 국유림 부산물 처분 실적  ==================================================================================
name = "국유임야 부산물 처분실적_Sales of Minor Forest Products from National Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("sales", "minor", "forest", "products", "national", "forest")),
                 filter_text_data(text_data, c("국유임", "부산물", "처분", "실적")),
                 filter_text_data(text_data, c("국유림", "부산물", "처분", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]








#### 🟦 국유림 분수림  ==================================================================================
##### 🟪 불요존 =============================================================================================
name = "불요존 국유림 분수림 현황_Status of Porfit-sharing Forest of Disposable National Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, include = c("profit", "sharing", " disposable")),
                 filter_text_data(text_data, include = c("불요존", "분수림")),
                 filter_text_data(text_data, include = c("profit", "sharing", " dispensable")),
                 filter_text_data(text_data, include = c("profit-sharing", " dispensable")),
                 filter_text_data(text_data, c("status", "profit-sharing", "forest", " disposable", "national", "forest")),
                 filter_text_data(text_data, c("불요존", "국유임", "분수림", "현황")),
                 filter_text_data(text_data, c("불요존", "국유림", "분수림", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 4, k_max = 4)[[1]]






#### 🟦 임도시설  ==================================================================================
name = "임도시설 현황_Status of Forest Road"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("status", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설", "현황")),
                 filter_text_data(text_data, c("construction", "forest", "road")),
                 filter_text_data(text_data, c("임도", "시설")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 4, k_max = 4)[[1]]










