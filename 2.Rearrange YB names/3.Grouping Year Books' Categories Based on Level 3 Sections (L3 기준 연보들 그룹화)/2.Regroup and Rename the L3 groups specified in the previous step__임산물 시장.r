# 🟥 임산물 시장 =====================================================================================================
# 국내총생산과 임업
name = "국내총생산과 임업_Gross Domestic Product and Forestry"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "forestry")),
                 filter_text_data(text_data, c("국내", "총생산","임업")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]






# 국내 총 생산과 임산물생산
name = "국내 총 생산과 임산물생산_Gross Domestic Product and Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("gross", "domestic", "product", "forest", "products")),
                 filter_text_data(text_data, c("국내", "총", "생산", "임산물", "생산")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





# 용도별 국내재 공급실적
name = "용도별 국내재 공급실적_Domestic Timber Supply by Uses"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("domestic", "timber", "supply", "uses")),
                 filter_text_data(text_data, c("용도별", "국내재", "공급", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 입목벌채 허가실적
name = "입목벌채 허가실적_Permission on Annual Timber Cutting"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("permission", "annual", "timber", "cutting")),
                 filter_text_data(text_data, c("입목벌채", "허가", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 수목굴취 허가(신고) 실적
name = "수목굴취 허가(신고) 실적_Permission(Reporting) of Tree Transplanting"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("permission", "reporting", "tree", "transplanting")),
                 filter_text_data(text_data, c("수목굴취", "허가", "신고", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 목재펠릿 생산 실적
name = "목재펠릿 생산 실적_Wood Pellet Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("wood", "pellet", "production")),
                 filter_text_data(text_data, c("목재펠릿", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 목질패널 생산 및 공급
name = "목질패널 생산 및 공급_Production and Supply of Processed Wood-based Panel"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "supply", "processed", "wood-based", "panel")),
                 filter_text_data(text_data, c("목질패널", "생산", "공급")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]








# 제재목 생산실적
name = "제재목 생산실적_Production and Supply of Sawnwood"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "supply", "sawnwood")),
                 filter_text_data(text_data, c("제재목", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





# 관상수 생산실적
name = "관상수 생산실적_Ornamental Tree Production"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("ornamental", "tree", "production")),
                 filter_text_data(text_data, c("관상수", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 목재수급실적_Demand and Supply of Timber
name = "목재수급실적_Demand and Supply of Timber"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "demand", "supply"), c("자원별", "facts")),
                 filter_text_data(text_data, c("원목", "수급", "실적"), c("자원별", "facts")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]







# 목재수급실적_Demand and Supply of Timber
name = "자원별 목재수급실적_Demand and Supply of Timber by resources"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("timber", "demand", "supply", "자원별"), c("facts")),
                 filter_text_data(text_data, c("원목", "수급", "실적", "자원별"), c("facts")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 국유림 목재 매각 실적
name = "국유림 목재 매각 실적_National Forest Wood Sales Results"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("national", "forest", "wood", "sales", "results")),
                 filter_text_data(text_data, c("국유림", "목재", "매각", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 산림벌채 면적 및 벌채량
name = "산림벌채 면적 및 벌채량_Area and Volume of Annual Cut"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("area", "volume", "annual", "cut")),
                 filter_text_data(text_data, c("산림벌채", "면적", "벌채량")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 임산물 수출실적
name = "임산물 수출실적_Export of Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("export", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "수출", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 임산물 수입실적
name = "임산물 수입실적_Import of Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("import", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "수입", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]





# 임산물 생산실적
name = "임산물 생산실적_Production of Forest Products"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("production", "forest", "products")),
                 filter_text_data(text_data, c("임산물", "생산", "실적")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 물가 지수
name = "물가 지수_Price index"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("price", "index"), "생산자"),
                 filter_text_data(text_data, c("물가", "지수"), "생산자"),
                 filter_text_data(text_data, c("물  가  지  수"), "생산자"))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]




# 생산자 물가 지수
name = "생산자 물가 지수_Producer Price index"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("price", "index", "생산자")),
                 filter_text_data(text_data, c("물가", "지수", "생산자")),
                 filter_text_data(text_data, c("물  가  지  수",  "생산자")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]
# text_clustering(text_data, k_min = 5, k_max = 5)[[1]]






