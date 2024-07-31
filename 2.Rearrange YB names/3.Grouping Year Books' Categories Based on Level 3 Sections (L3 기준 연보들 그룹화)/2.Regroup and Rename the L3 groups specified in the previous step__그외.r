
# 🟥 작업1 ======================================================================================================================
## 🟦용도밸 내재 공급 실적 ===========================================================================================================
name = "용도별 내재 공급실적_Domestic Timber Supply by Use"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("domestic", "timber", "supply", "by", "use")),
                 filter_text_data(text_data, c("용도별", "내재", "공급")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 10, k_max = 10)[[1]]



## 🟦 독림가 내역_7. Details of Sincere Forest Manager"    ===========================================================================================================
name = "독림가 내역_Details of Sincere Forest Manager"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("details", "of", "sincere", "forest")),
                 filter_text_data(text_data, c("독림가", "내역")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




## 🟦 수종별 종자 생산 현황_Seed Production by Tree Species===========================================================================================
name ="수종별 종자 생산현황_Seed Production by Tree Species"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Seed", "Production", "by", "Tree", "Species")),
                 filter_text_data(text_data, c("수종별", "종자", "생산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 산림면적 및 산림비율_Forest Area and % of Forest Area by Ownership===========================================================================================
name = "산림면적 및 산림비율_Forest Area and % of Forest Area by Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Area", "and", "%", "by", "Ownership")),
                 filter_text_data(text_data, c("소유별", "산림면적", "산림비율")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 재배작물별 재배면적_Cultivated Area by Growing Crops===========================================================================================
name = "재배작물별 재배면적_Cultivated Area by Growing Crops"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Cultivated", "Area", "by", "Growing", "Crops")),
                 filter_text_data(text_data, c("재배작물별", "재배면적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




# 🟥 작업2 ======================================================================================================================
## 🟦 임업기계 장비 보유현황_Forest Machinery and Equipment===========================================================================================
name = "임업기계 장비 보유현황_Forest Machinery and Equipment"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Machinery", "and", "Equipment")),
                 filter_text_data(text_data, c("임업기계", "장비", "보유현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 임가경제 주요지표_Main Indicators of Forest Household Economy===========================================================================================
name = "임가경제 주요지표_Main Indicators of Forest Household Economy"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Main", "Indicators", "of", "Forest", "Household", "Economy")),
                 filter_text_data(text_data, c("임가경제", "주요지표")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 개인 임가 현황_Private Forest Households===========================================================================================
name = "개인 임가 현황_Private Forest Households"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Private", "Forest", "Households")),
                 filter_text_data(text_data, c("개인", "임가", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 종자 및 묘목 생산현황_Status of Seed and Seedling Production===========================================================================================
name = "종자 및 묘목 생산현황_Status of Seed and Seedling Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Status", "of", "Seed", "and", "Seedling", "Production")),
                 filter_text_data(text_data, c("종자", "및", "묘목", "생산현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림사업 융자실적_Loans for Forest Activities===========================================================================================
name = "산림사업 융자실적_Loans for Forest Activities"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Loans", "for", "Forest", "Activities")),
                 filter_text_data(text_data, c("산림사업", "융자실적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





## 🟦 귀산촌인 연령별 현황_People Returning to Mountain Villages by Age===========================================================================================
name = "귀산촌인 연령별 현황_People Returning to Mountain Villages by Age"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("People", "Returning", "to", "Mountain", "Villages", "by", "Age")),
                 filter_text_data(text_data, c("귀산촌인", "연령별", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림사업 융자규모_Loans for Forestry Business===========================================================================================
name = "산림사업 융자규모_Loans for Forestry Business"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Loans", "for", "Forestry", "Business")),
                 filter_text_data(text_data, c("산림사업", "융자규모")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 행정구역별 면적 및 행정구역수_Area and number of administrative districts===========================================================================================
name = "행정구역별 면적 및 행정구역수_Area and number of administrative districts"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Area", "and", "number", "of", "administrative", "districts")),
                 filter_text_data(text_data, c("행정구역", "구역", "면적", "수")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





## 🟦 사유림 산주 현황_Ownership of Private Forests by Provinces===========================================================================================
name = "사유림 산주 현황_Ownership of Private Forests by Provinces"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Ownership", "of", "Private", "Forests", "by", "Provinces")),
                 filter_text_data(text_data, c("사유림", "산주", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]








# 🟥 작업3 ======================================================================================================================
## 🟦 국유림 관리_National forest management===========================================================================================
name = "국유림 관리_National forest management"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("National", "forest", "management"), "non"),
                 filter_text_data(text_data, c("국유림", "관리")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





## 🟦 산림계 현황과 위탁림 실태_Status of Village Forestry Association and the consigned forests===========================================================================================
name = "산림계 현황과 위탁림 실태_Status of Village Forestry Association and the consigned forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Status", "of", "Village", "Forestry", "Association", "consigned", "forests")),
                 filter_text_data(text_data, c("산림계", "현황", "위탁림", "실태")),
                 filter_text_data(text_data, c("산림계", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림사업 고용현황_Status of Employment in Forestry===========================================================================================
name = "산림사업 고용현황_Status of Employment in Forestry"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Status", "of", "Employment", "in", "Forestry")),
                 filter_text_data(text_data, c("산림사업", "고용현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 전·겸업별 임가 현황_Forest Households by Full and Part Time===========================================================================================
name = "전·겸업별 임가 현황_Forest Households by Full and Part Time"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Households", "by", "Full", "and", "Part", "Time")),
                 filter_text_data(text_data, c("겸업별", "임가", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 임가 및 임가 인구_Number of Households and Population engaged in the Forestry sector===========================================================================================
name = "임가 및 임가 인구_Number of Households and Population engaged in the Forestry sector"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Number", "of", "Households", "Population", "engaged", "Forestry", "sector")),
                 filter_text_data(text_data, c("임가", "및", "임가", "인구")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산촌지역 연령별 인구 현황_Population Status by Age in Mountain Village===========================================================================================
name = "산촌지역 연령별 인구 현황_Population Status by Age in Mountain Village"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Population", "Status", "by", "Age", "Mountain", "Village")),
                 filter_text_data(text_data, c("산촌지역", "연령별", "인구", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





## 🟦 산림관계 세입 세출 예산_Forest Budget for Revenue and Expenditure===========================================================================================
name = "산림관계 세입 세출 예산_Forest Budget for Revenue and Expenditure"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Budget", "for", "Revenue", "and", "Expenditure")),
                 filter_text_data(text_data, c("산림관계", "세입", "세출", "예산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 년도별 임상별 임야면적_Forest Land Area by Forest type and year===========================================================================================
name = "년도별 임상별 임야면적_Forest Land Area by Forest type and year"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Land", "Area", "by", "Forest", "type", "year")),
                 filter_text_data(text_data, c("년도", "임상별", "임야면적")),
                 filter_text_data(text_data, c("연도", "임상별", "임야면적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 임상별 임야면적 구성_Proportion of forest land area by forest type===========================================================================================
name = "임상별 임야면적 구성_Proportion of forest land area by forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Proportion", "of", "forest", "land", "area", "by", "forest", "type")),
                 filter_text_data(text_data, c("임상별", "임야면적", "구성")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 구ㆍ시ㆍ군별 면적 및 행정단위_Area and administrative unit by Gu, Si and Gun===========================================================================================
name = "구ㆍ시ㆍ군별 면적 및 행정단위_Area and administrative unit by Gu, Si and Gun"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Area", "and", "administrative", "unit", "by", "Gu", "Si", "Gun")),
                 filter_text_data(text_data, c("구ㆍ시ㆍ군별", "면적", "및", "행정단위")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 전·겸업별 임가현황_Forest Households by Type of Work===========================================================================================
name = "전·겸업별 임가현황_Forest Households by Type of Work"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Households", "by", "Type", "of", "Work")),
                 filter_text_data(text_data, c("전·겸업별", "임가현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 민유림 관리_Non-national forest management===========================================================================================
name = "민유림 관리_Non-national forest management"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Non-national", "forest", "management")),
                 filter_text_data(text_data, c("민유림", "관리")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]



## 🟦 국유임야경제측량 및 표주설치실적_Boundary survey and land mark in national forest===========================================================================================
name = "국유임야경제측량 및 표주설치실적_Boundary survey and land mark in national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Boundary", "survey", "and", "land", "mark", "in", "national", "forest")),
                 filter_text_data(text_data, c("국유임야경제측량", "표주설치실적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 시ㆍ도별 면적 및 행정단위_Area and administrative unit by province===========================================================================================
name = "시ㆍ도별 면적 및 행정단위_Area and administrative unit by province"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Area", "and", "administrative", "unit", "by", "province")),
                 filter_text_data(text_data, c("시ㆍ도별", "면적", "및", "행정단위")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





# 🟥 작업4 ======================================================================================================================
## 🟦 화전지 정리실적_Resettlement of Shifting Cultivation===========================================================================================
name = "화전지 정리실적_Resettlement of Shifting Cultivation"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Resettlement", "of", "Shifting", "Cultivation")),
                 filter_text_data(text_data, c("화전", "정리")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산촌마을 조성현황_Status of Mountain Village Development===========================================================================================
name = "산촌마을 조성현황_Status of Mountain Village Development"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Status", "of", "Mountain", "Village", "Development")),
                 filter_text_data(text_data, c("산촌마을", "조성현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 영림단 운영현황_Forest Management Units===========================================================================================
name = "영림단 운영현황_Forest Management Units"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Management", "Units")),
                 filter_text_data(text_data, c("영림단", "운영현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 국유림 및 민유림 경영계획 편성 실적_Accomplishment of working plan drawing up of national and non-national forest===========================================================================================
name = "국유림 및 민유림 경영계획 편성 실적_Accomplishment of working plan drawing up of national and non-national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Accomplishment", "of", "working", "plan", "drawing", "up", "of", "national", "and", "non-national", "forest")),
                 filter_text_data(text_data, c("국유림", "및", "민유림", "경영계획", "편성", "실적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 임상별 임목축적_Forest growing stock by forest type===========================================================================================
name = "임상별 임목축적_Forest growing stock by forest type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "growing", "stock", "by", "forest", "type")),
                 filter_text_data(text_data, c("임상별", "임목축적")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 국유림 간벌 및 각종지장목 벌채실적_Thinning and cutting of interfering trees in national forest===========================================================================================
name = "국유림 간벌 및 각종지장목 벌채실적_Thinning and cutting of interfering trees in national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Thinning", "cutting", "interfering", "trees", "national", "forest")),
                 filter_text_data(text_data, c("국유림", "지장목", "벌채")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 경영형태별 임가 현황_Forest Households by Type of Economic Activity===========================================================================================
name = "경영형태별 임가 현황_Forest Households by Type of Economic Activity"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Households", "Type", "Economic", "Activity")),
                 filter_text_data(text_data, c("경영형태별", "임가", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]





# 🟥 작업4 ======================================================================================================================
## 🟦 개인산주 임야소재지별 거주지 현황_Location of Residence of Private Forest Land Owner by Location of Forest Land===========================================================================================
name = "개인산주 임야소재지별 거주지 현황_Location of Residence of Private Forest Land Owner by Location of Forest Land"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Location", "Residence", "Private", "Forest", "Land", "Owner")),
                 filter_text_data(text_data, c("개인", "산주", "소재지", "거주지", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림관계세출예산_Forest Estimated Expenditures===========================================================================================
name = "산림관계세출예산_Forest Estimated Expenditures"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Estimated", "Expenditures")),
                 filter_text_data(text_data, c("산림관계", "세출예산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 산림관계 세입예산_Forest Estimated Revenue===========================================================================================
name = "산림관계 세입예산_Forest Estimated Revenue"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Estimated", "Revenue")),
                 filter_text_data(text_data, c("산림관계", "세입예산")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 민유림야 성림무육 및 각종지장목 벌채 허가실적_Amount of thinning and cutting of interfering trees authorized in non-national forest===========================================================================================
name = "민유림야 성림무육 및 각종지장목 벌채 허가실적_Amount of thinning and cutting of interfering trees authorized in non-national forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Amount", "thinning", "cutting", "interfering", "trees", "authorized", "non-national", "forest")),
                 filter_text_data(text_data, c("민유", "지장목", "벌채")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]






## 🟦 경영형태별 임가 현황_Forest Households by management type===========================================================================================
name = "경영형태별 임가 현황_Forest Households by management type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Households", "by", "management", "type")),
                 filter_text_data(text_data, c("경영형태별", "임가", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 생산형태별 임가 현황_Forest Households by Production Type===========================================================================================
name = "생산형태별 임가 현황_Forest Households by Production Type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Households", "by", "production", "type")),
                 filter_text_data(text_data, c("생산형태", "임가", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 생산형태별 임가 현황_Forest Households by Production Type===========================================================================================
name = "생산형태별 임가 현황_Forest Households by Production Type"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("Forest", "Households", "by", "production", "type")),
                 filter_text_data(text_data, c("생산형태", "임가", "현황")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




## 🟦 성림 무육 및 각종 지장목 벌채 실적_Ⅳ-4 Tending operation and cutting of interfering trees===========================================================================================
name = "성림 무육 및 각종 지장목 벌채 실적_Tending operation and cutting of interfering trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("tending", "operation", "cutting", "interfering", "tree")),
                 filter_text_data(text_data, c("성림", "지장목", "벌채")))
print(combined.list[[name]])
text_data = text_data[!text_data %in% unlist(combined.list)]




