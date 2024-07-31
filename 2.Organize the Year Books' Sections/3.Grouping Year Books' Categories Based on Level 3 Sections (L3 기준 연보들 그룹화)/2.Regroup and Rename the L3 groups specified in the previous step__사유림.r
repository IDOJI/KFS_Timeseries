# 🟥 사유림 =====================================================================================================
name = "사유림 소유규모별 산주현황_Private Forest Owners by Size"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owner", "size")),
                 filter_text_data(text_data, c("사유림", "산주", "규모")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]




name = "사유림 소유형태별 산주현황_Private Forest Owners By Ownership"
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "owner", "ownership", "산주현황")),
                 filter_text_data(text_data, c("사유림", "산주", "형태")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]





name = "사유림 소유형태별 산림면적_Private Forest Land Area by Ownership"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "ownership", "land")),
                 filter_text_data(text_data, c("사유림", "소유", "형태", "산림")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]






name = "사유림 소재ㆍ부재 산주현황_Status of Resident and Absentee Forest Owners by Province"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("resident", "absolute", "forest", "owner", "province")),
                 filter_text_data(text_data, c("사유림", "부재", "산주")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]










name = "사유림 소유형태별 필지수 현황_Private Forest Lots By Ownership"
print(combined.list[[name]])
combined.list[[name]] =
  union_multiple(filter_text_data(text_data, c("private", "forest", "ownership", "lots")),
                 filter_text_data(text_data, c("사유림", "형태", "필지수")))
print(combined.list[[name]])
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
text_data = text_data[! text_data %in% unlist(combined.list)]

