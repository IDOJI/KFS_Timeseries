# 🟥 임업생산 =====================================================================================================
#### 🟦 국내생산과 임업생산 ===================================================================================================
##### 🟪 경제활동별 국내총생산 ===================================================================================================
name = "경제활동별 국내총생산_Gross Domestic Product By Kind Of Economic Activity"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "kind", "economic", "activity")),
                 filter_text_data(text_data, c("경제활동별", "국내총생산")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]





#### 🟦 임목벌채 ===================================================================================================
##### 🟪 임목 벌채 공급 계획 및 실적 ===================================================================================================
name = "임목벌채 공급계획 및 실적_Cutting Plan and Supply of Timber"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("cutting", "plan", "supply", "timber")),
                 filter_text_data(text_data, c("임목", "벌채", "공급", "계획", "실적")),
                 filter_text_data(text_data, c("cutting", "plan", "production", "timber")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]



##### 🟪 임목 벌채 허가 실적 ===================================================================================================
name = "임목벌채 허가실적_Permit of Annual Tree Cutting"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("permit", "annual", "tree", "cutting")),
                 filter_text_data(text_data, c("임목", "벌채", "허가", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 임목 벌채 실적 ===================================================================================================
name = "임목 벌채실적_Timber Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "production", "벌채"), c("permit", "생산량표", "임산물", "계획", "배정량")),
                 filter_text_data(text_data, c("임목", "벌채", "실적"), c("허가", "생산량표", "임산물", "계획", "배정량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 임목 벌채 배정량 및 생산 실적 ===================================================================================================
name = "임목벌채 배정량 및 생산실적_Allocation of Cutting Amount of Wood and Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("allocation", "cutting", "amount", "wood", "production")),
                 filter_text_data(text_data, c("임목", "벌채", "배정량", "생산", "실적")),
                 filter_text_data(text_data, c("임목", "별채", "배정량", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 간벌 및 각종 지장목 벌채 실적_Thinning and cutting of interfering trees ===================================================================================================
name = "간벌 및 각종 지장목 벌채 실적_Thinning and cutting of interfering trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("thinning", "cutting", "interfering", "trees"), c("국유림", "민유", "무육")),
                 filter_text_data(text_data, c("지장목", "벌채", "실적"), c("국유림", "민유", "무육")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]







#### 🟦 목재 ===================================================================================================
##### 🟪 목재 수급계획 및 공급 실적 ===================================================================================================
name = "목재 수급 계획 및 공급 실적_Facts of Demand and Supply for Timber"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("facts", "demand", "supply", "for", "timber")),
                 filter_text_data(text_data, c("목재", "수급", "계획", "공급", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




##### 🟪 목재생산 및 공급 실적 ===================================================================================================
name = "목재생산 및 공급실적_Timber Production and Supply"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "production", "supply")),
                 filter_text_data(text_data, c("목재", "생산", "공급", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]





##### 🟪 자원별 목재 수급 현황 ===================================================================================================
name = "자원별 목재 수급현황_Wood Demand and Supply by Resources"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("wood", "demand", "supply", "resources")),
                 filter_text_data(text_data, c("자원별", "목재", "수급")),
                 filter_text_data(text_data, c("timber", "demand", "supply", "resources")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 공급실적 ===================================================================================================
##### 🟪 용도별 원목 공급 실적 ===================================================================================================
name = "용도별 원목 공급 실적_Timber Supply by Use"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "supply", "use"), "domestic"),
                 filter_text_data(text_data, c("용도별", "원목", "공급", "실적"), "국내재"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]




# 🟥 텍스트 필터링 해서 그룹화 =====================================================================================
#### 🟦 생산실적 ===================================================================================================
##### 🟪 원목 생산 실적 ===================================================================================================
name = "원목 생산실적_Pound Wood Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("pound", "wood", "production")),
                 filter_text_data(text_data, c("원목", "생산실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]








#### 🟦 임산물 ===================================================================================================
##### 🟪 임산물 생산액의 구성 ===================================================================================================
name = "임산물 생산액의 구성_Forest Products Value"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "products", "value"), c("export", "import")),
                 filter_text_data(text_data, c("임산물", "생산액", "구성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]






#### 🟦 기타 ===================================================================================================
##### 🟪 산림소유 규모별 개인 임가 현황  ===================================================================================================
name = "산림소유 규모별 개인 임가 현황_Private Forest Households by Size of Forest Area"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "households", "size", "forest", "area")),
                 filter_text_data(text_data, c("산림", "소유", "규모별", "개인", "임가", "현황")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]






