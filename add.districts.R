zimbabwe <-read.csv("ZimbabweMeaslesData.csv")

                                        # Set province names
province <- rep(NA,nrow(zimbabwe))
"harare"              -> province[zimbabwe$district %in% "harare"]
"bulawayo"            -> province[zimbabwe$district %in% "bulawayo"]
"manicaland"          -> province[zimbabwe$district %in% c("buhera","chimanimani","chipinge","makoni","mutare","mutasa","nyanga")]
"mashonaland central" -> province[zimbabwe$district %in% c("bindura","guruve","mazowe","mbire","mukumbura","muzarabani","rushinga")]
"mashonaland east"    -> province[zimbabwe$district %in% c("chikomba","goromonzi","hwedza","marondera","mudzi","murehwa","mutoko","seke","uzumba-maramba-pfungwe")]
"mashonaland west"    -> province[zimbabwe$district %in% c("chegutu","hurungwe","kadoma","kariba","makonde","zvimba")]
"masvingo"            -> province[zimbabwe$district %in% c("bikita","chiredzi","chivi","gutu","masvingo","mwenezi","zaka")]
"matabeleland north"  -> province[zimbabwe$district %in% c("binga","bubi","hwange","lupane","nkayi","tsholotsho","umguza")]
"matabeleland south"  -> province[zimbabwe$district %in% c("beitbridge","bulilima","mangwe","gwanda","insiza","matobo","umzingwane")]
"midlands"            -> province[zimbabwe$district %in% c("chirumhanzu","gokwe north","gokwe south","gweru","kwekwe","mberengwe","shurugwi","zvishavane")]

zimbabwe$province <- province

write.csv(zimbabwe, file="ZimbabweMeaslesDataWithProvinces.csv", row.names=F)

