### 🟨 국토와 자연환경 ================================================================================
#### 🟦 대한민국 위치==================================================================================
name = "대한민국의 위치_Location of Repubic of Korea"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("location", "korea")),
                 filter_text_data(text_data, c("대한민국", "위치")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 국토 이용 상황 ==================================================================================
name = "국토이용 상황_Status of national land utilization"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("national", "land", "utilization")),
                 filter_text_data(text_data, c("국토", "이용")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 측후소 일람표 ==================================================================================
name = "측후소일람표_List of meteorological stations"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, c("meteorological", "station")),
                 filter_text_data(text_data, c("측후소")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 시도별 면적 및 행정 단위 ==================================================================================
name = "시도별 면적 및 행정단위_Area and administrative unit by province and cities"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, c("area", "administrative", "unit"), c("축적", "Gu")),
                 filter_text_data(text_data, c("면적", "행정", "단위"), c("면적", "축적", "Gu")),
                 filter_text_data(text_data, c("면적", "행정구역"), c("축적", "면적", "Gu")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




#### 🟦 강수량표 ==================================================================================
name = "강수량표_Precipitation"
combined.list[[name]] = 
  union_multiple(filter_text_data(text_data, c("precipitation")),
                 filter_text_data(text_data, c("강수량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# combined.list[[name]] %in% text_data



#### 🟦 일조시간 ==================================================================================
name = "일조 시간_Hours of Sunshine"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("sunshine")),
                 filter_text_data(text_data, c("일조", "시간")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 인구변동 추이 ==================================================================================
name = "인구변동 추이_Population Trend"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("population", "trend")),
                 filter_text_data(text_data, c("인구", "추이")),
                 filter_text_data(text_data, c("인구", "추세")),
                 filter_text_data(text_data, c("인구", "추계")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 상대습도 ==================================================================================
name = "상대습도_Relative humidity"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("relative", "humidity")),
                 filter_text_data(text_data, c("상대", "습도")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 평균기온  ==================================================================================
name = "평균 기온_Average Temperature"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("average", "temper")),
                 filter_text_data(text_data, c("평균", "기온")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]


#### 🟦 지상기상 관측지검 ==================================================================================
name = "지상기상관측지점_List of the Surface Synoptic Stations"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("surface", "synoptic", "station")),
                 filter_text_data(text_data, c("지상", "기상", "관측")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]







#### 🟦 지상기상 관측지검 ==================================================================================
name = "지상기상관측지점_List of the Surface Synoptic Stations"
combined.list[[name]]=
  union_multiple(filter_text_data(text_data, c("surface", "synoptic", "station")),
                 filter_text_data(text_data, c("지상", "기상", "관측")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]








