# 🟥 산림서비스 =====================================================================================================
# 산림 복지
name = "산림복지전문업 등록현황_Status of Job Startups on Forest welfare"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("job", "startup", "forest", "welfare")),
                 filter_text_data(text_data, c("산림", "복지", "전문")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



# 교육
name = "숲사랑 소년단 육성현황_The Number of Green Rangers"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "green", "ranger")),
                 filter_text_data(text_data, c("숲사랑", "소년")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "산림교육전문가 양성기관 현황_Status of Forest Guide Training Organizations"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("forest", "guide", "training")),
                 filter_text_data(text_data, c("산림", "교육", "전문가")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "산림교육 수혜인원 현황_The Number of Forest Education Recipients"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "forest", "education", "recipient")),
                 filter_text_data(text_data, c("산림", "교육", "수혜")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "숲 해설가 수혜인원 현황_Status of Beneficiaries of Forest Guide"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("beneficiaries", "forest", "guide")),
                 filter_text_data(text_data, c("해설가", "수혜", "인원")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 
name = "수목원, 산림박물관, 자생식물원 등록 현황_Establishment of Arboretums, Forest Museum, and Botanical Garden"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "arboretum", "forest", "museum", "botanical")),
                 filter_text_data(text_data, c("수목원", "박물관")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 숲 운영
name = "치유의 숲 운영 현황_Management of Healing Forest"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("healing", "management", "forest")),
                 filter_text_data(text_data, c("치유", "숲", "운영")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "휴양림 운영 및 이용현황_Number of Visitors to Recreation Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "recreation", "forest"), "조성"),
                 filter_text_data(text_data, c("휴양림", "운영"), "조성"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 숲 조성
name = "명상숲 조성현황_The Number of Meditation Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("number", "Meditation", "forest")),
                 filter_text_data(text_data, c("명상", "숲", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "학교 숲 조성현황_Establishment of Forests within Schools"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "school", "forest")),
                 filter_text_data(text_data, c("학교", "숲", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




name = "도시숲 조성현황_Establishment of Urban Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "urban", "forest")),
                 filter_text_data(text_data, c("도시숲", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







name = "휴양림 및 산림욕장 조성현황_Establishment of Recreation Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "recreation", "forest")),
                 filter_text_data(text_data, c("휴양림", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



name = "산림욕장 조성현황_Establishment of Forest Bathing Facilities"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "forest", "bathing", "facilities")),
                 filter_text_data(text_data, c("산림", "욕장", "조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







name = "정원 조성 및 운영현황_Create Garden and Manage Garden"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("create garden", "manage garden")),
                 filter_text_data(text_data, c("정원 조성")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 전통 마을
name = "전통마을 숲 조성현황_Establishment of Traditional Village Forests"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("establishment", "traditional", "forest")),
                 filter_text_data(text_data, c("전통", "마을", "숲")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





# 가로수
name = "가로수 심기현황_Plantation of Roadside Trees"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("roadside", "tree", "plantation")),
                 filter_text_data(text_data, c("가로수", "심기")),
                 filter_text_data(text_data, c("planting", "roadside", "tree")),
                 filter_text_data(text_data, c("planting", "roadisde", "tree")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



# 
name = "산림복지서비스 제공자 등록 현황_Enrollment of Forest Welfare Service Voucher-available-facilities"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("enrollment", "forest", "welfare", "service")),
                 filter_text_data(text_data, c("산림", "복지", "서비스", "제공")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]



