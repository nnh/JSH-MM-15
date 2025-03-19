
#######################
#jshmm-15 データクリーニング用プログラム
#修正履歴
# 2017/06/20 :新規作成(kazumi takeuchi)
# 2018/05/28 :downloaded date上書き、データのアウトプットfile名の導出を修正(Agata.K)
# 2018/06/14 :条件式を加える箇所の式が働いておらず、as.characterを加えた式へ修正,downloaded date上書き（Agata.K）
# 2021/06/01 :パス参照に関する処理を見直し（Agata.K）
# 2021/01/13 :最終解析に伴い、集計内容更新（Agata.K）
# 2022/04/01 :差分結果出力箇所（# 日付差分のみ）について、差分が0の場合も表示する様に変更（Agata.K）
# 2025/03/19 :pushテスト
#######################

# Setting
DLDate <- "_220304_1026"
prtpath <- "C:/Users/KumikoAgata/Box/Datacenter/Trials/JSH/MM-15/11.03.09 最終解析用データセット/データクリーニング/R/program/first"
setwd(paste0(prtpath, "/rawdata"))

list <- list.files()
file.name <- sub("_220304_1026.*", "", list)  # downloaded date Set
df.name <- sub("_MM-15", "", file.name)
df.name <- sub(".*_", "", df.name)

for (i in 1:length(list)) {
  assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}

# 必要なラベルの取り出し
registration1 <- registration[, c("症例登録番号", "症例登録日")]                      #registration
baselineEx1 <- baselineEx[, c("症例登録番号", "診断日")]                              #baselineEx
baseline1 <- baseline[, c("症例登録番号", "診断日")]                                  #baseline
remitherapy1 <- remitherapy[, c("症例登録番号", "初回寛解導入療法.開始日")]           #remitherapy
salvagetherapy1_1 <- salvagetherapy1[, c("症例登録番号", "初回サルベージ療法開始日")] #salvagetherapy1
#  (2021.01.13 Agata.K Add)
transplant1 <- transplant[, c("症例登録番号", "初回移植日")] #transplant
constherapy1 <- constherapy[, c("症例登録番号", "地固め治療開始日")]  #constherapy
maintetherapy1 <- maintetherapy[, c("症例登録番号", "維持治療開始日")] #maintetherapy
initialtreatment1 <- initialtreatment[, c("症例登録番号", "初期治療中のbest.response", "奏効判定日", "本観察期間中にPDの判定がありましたか", "初回PDの判定日", "初期治療.最終投与日")] #initialtreatment
salvagetherapy1out_1 <- salvagetherapy1out[, c("症例登録番号", "field1057", "初回サルベージ治療.奏効判定日", "初回サルベージ治療後の初回再発または増悪の有無", "初回サルベージ治療後の初回再発または増悪の判定日", "初回サルベージ療法開始後の最終無増悪生存確認日")] #salvagetherapy1out
salvagetherapy2_1 <- salvagetherapy2[, c("症例登録番号", "X2回目のサルベージ治療開始日")] #salvagetherapy2
salvagetherapy3_1 <- salvagetherapy3[, c("症例登録番号", "X3回目のサルベージ治療開始日")] #salvagetherapy3
radiationtherapy1 <- radiationtherapy[, c("症例登録番号", "放射線治療.照射開始日")] #radiationtherapy
transition11_1 <- transition11[, c("症例登録番号", "移行した病型の診断日")] #transition11
transition22_1 <- transition22[, c("症例登録番号", "移行した病型の診断日")] #transition22
secondcancer1 <- secondcancer[, c("症例登録番号", "二次発がん診断日")]  #secondcancer
discontinuation1 <- discontinuation[, c("症例登録番号", "中止日", "中止理由")] #discontinuation
followup1 <- followup[,c ("症例登録番号", "最終生存確認日", "死亡日", "死亡の原因")] #followup

# 新たな名前をつける
names(baselineEx1)[2] <- c("症候性以外診断日")
names(baseline1)[2] <- c("症候性診断日")
#  (2021.01.13 Agata.K Add)
names(remitherapy1)[2] <- c("寛解導入療法")
names(transplant1)[2] <- c("初回移植")
names(constherapy1)[2] <- c("地固め治療")
names(maintetherapy1)[2] <- c("維持治療")
names(initialtreatment1)[2:6] <- c("奏効", "奏効判定", "PD判定", "初回PD判定", "初期治療最終投与")
names(salvagetherapy1_1)[2] <- c("初回サルベージ")
names(salvagetherapy1out_1)[2:6] <- c("初サ奏効判定", "初回サルベージ奏効", "初サ増悪有無", "初回サルベージ増悪", "初回サルベージ生存")
names(salvagetherapy2_1)[2] <- c("2回サルベージ")
names(salvagetherapy3_1)[2] <- c("3回サルベージ")
names(radiationtherapy1)[2] <- c("照射開始")
names(transition11_1)[2] <- c("症候性以外病型移行")
names(transition22_1)[2] <- c("症候性病型移行")
names(secondcancer1)[2] <- c("二次発がん")

# 症候性以外_割付:症候性以外→True／左記以外→FALSE#  (2021.01.13 Agata.K Add)
allocation$症候性以外_割付 <- ifelse(allocation$診断分類 == "症候性以外", TRUE, FALSE)

# 症候性以外診断日確認:症候性以外診断日<=2015/12/31の時、「←」
baselineEx1$症候性以外診断日確認 <- ifelse(as.Date(baselineEx1$症候性以外診断日) <= as.Date("2015-12-31"), "←","")

# 症候性_割付:症候性、症候性以外から症候性移行→True／左記以外→FALSE#  (2021.01.13 Agata.K Add)
allocation$症候性_割付 <- ifelse((allocation$診断分類 != "症候性以外") | (allocation$病型移行報告書.症候性以外.症候性骨髄腫. == "症候性骨髄腫に移行した"), TRUE,FALSE)

# 症候性診断日確認:症候性診断日<=2015/12/31の時、「←」
baseline1$症候性診断日確認 <- ifelse(as.Date(baseline1$症候性診断日) <= as.Date("2015-12-31"), "←","")

# マージする（key=症例登録番号） (2021.01.13 Agata.K)
m_regst_alo <- merge(registration1, allocation, by="症例登録番号", all.x = T)
m_base <- merge(m_regst_alo, baseline1, by="症例登録番号", all.x = T)
m_baseE <- merge(m_base, baselineEx1, by="症例登録番号",all.x = T)
m_remi <- merge(m_baseE, remitherapy1, by="症例登録番号", all.x = T)
m_tran <- merge(m_remi, transplant1, by="症例登録番号", all.x = T)
m_cons <- merge(m_tran, constherapy1, by="症例登録番号", all.x = T)
m_maint <- merge(m_cons, maintetherapy1, by="症例登録番号", all.x = T)
m_init <- merge(m_maint, initialtreatment1, by = "症例登録番号",all.x = T)
m_salv <- merge(m_init, salvagetherapy1_1, by = "症例登録番号", all.x = T)
m_salvout <- merge(m_salv, salvagetherapy1out_1, by = "症例登録番号", all.x = T)
m_salv2 <- merge(m_salvout, salvagetherapy2_1, by = "症例登録番号", all.x = T)
m_salv3 <- merge(m_salv2, salvagetherapy3_1, by = "症例登録番号", all.x = T)
m_rad <- merge(m_salv3, radiationtherapy1, by = "症例登録番号", all.x = T)
m_tra11 <- merge(m_rad, transition11_1, by = "症例登録番号", all.x = T)
m_tra22 <- merge(m_tra11, transition22_1, by = "症例登録番号", all.x = T)
m_scan <- merge(m_tra22, secondcancer1, by = "症例登録番号", all.x = T)
m_discon <- merge(m_scan, discontinuation1, by = "症例登録番号", all.x = T)
m_fllow <- merge(m_discon, followup1, by = "症例登録番号", all.x = T)
m_data <- m_fllow

# 処理が必要なもの
# 初期治療中のbest.response:bestresponsがPDで、奏効判定日 != 初回PDの判定日の場合「←」
m_data$初期治療中のbest.response <- ifelse(m_data$奏効=="PD" & as.Date(m_data$奏効判定) != as.Date(m_data$初回PD判定), "←", "")
# 初回PD判定確認:PD判定がいいえで、初回サルベージ入力ありの場合、「←」
m_data$初回PD判定確認 <- ifelse((m_data$PD判定 == "いいえ") & !(is.na(m_data$初回サルベージ)), "←", "")
# 初期治療最終投与_初回PD判定_1年確認:初期治療最終投与日ー初回PD判定日
m_data$初期治療最終投与_初回PD判定_1年確認 <- ifelse(as.Date(m_data$初期治療最終投与) - as.Date(m_data$初回PD判定) > 365, "←", "")
# 初回サルベージ奏効_初回サルベージ増悪_SD以上:初回サルベージ奏効判定日 ー初回サルベージ治療増悪判定日
m_data$初回サルベージ奏効_初回サルベージ増悪_SD以上 <- ifelse((m_data$初サ奏効判定 <= 5) & (m_data$初サ増悪有無 == "あり") & (as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪)>=0), "←", "")
# 初回サルベージ奏効_初回サルベージ増悪_PD:初回サルベージ奏効判定日 ー初回サルベージ治療増悪判定日
m_data$初回サルベージ奏効_初回サルベージ増悪_PD <- ifelse((m_data$初サ奏効判定 ==6) & (m_data$初サ増悪有無 == "あり") & (as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪) <=-14), "←", "")

# 日付差分のみ（2022/04/01について「0」も表示するように修正）
# 症候性以外_症候性診断日:症候性以外診断日ー症候性診断日
m_data$症候性以外_症候性診断日 <- ifelse(as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性診断日) <0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性診断日))
# 症候性以外_照射開始:症候性以外診断日ー放射線照射開始日
m_data$症候性以外_照射開始 <- ifelse(as.Date(m_data$症候性以外診断日) - as.Date(m_data$照射開始) <0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$照射開始))
# 症候性以外_症候性以外病型移行:症候性以外診断日ー症候性以外病型移行日
m_data$症候性以外_症候性以外病型移行 <- ifelse(as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性以外病型移行) <0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性以外病型移行))
# 照射開始_最終生存確認:照射開始ー最終生存確認
m_data$照射開始_最終生存確認 <- ifelse(as.Date(m_data$照射開始) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$照射開始) - as.Date(m_data$最終生存確認日))
# 症候性以外病型移行_最終生存確認:症候性以外病型移行ー最終生存確認
m_data$症候性以外病型移行_最終生存確認 <- ifelse(as.Date(m_data$症候性以外病型移行) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$症候性以外病型移行) - as.Date(m_data$最終生存確認日))
# 症候性以外_最終生存確認:症候性以外診断日ー最終生存確認日
m_data$症候性以外_最終生存確認 <- ifelse(as.Date(m_data$症候性以外診断日) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$最終生存確認日))
# 照射開始_死亡日:照射開始ー死亡日
m_data$照射開始_死亡日 <- ifelse(as.Date(m_data$照射開始) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$照射開始) - as.Date(m_data$死亡日))
# 症候性以外病型移行_死亡日:症候性以外病型移行ー死亡日
m_data$症候性以外病型移行_死亡日 <- ifelse(as.Date(m_data$症候性以外病型移行) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$症候性以外病型移行) - as.Date(m_data$死亡日))
# 症候性以外_死亡日:症候性以外診断日ー死亡日
m_data$症候性以外_死亡日 <- ifelse(as.Date(m_data$症候性以外診断日) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$死亡日))

# 症候性_寛解導入療法:症候性診断日 ー 初回寛解導入開始日
m_data$症候性_寛解導入療法 <- ifelse(as.Date(m_data$症候性診断日) - as.Date(m_data$寛解導入療法) <0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$寛解導入療法))
# 寛解導入療法_奏効判定:初回寛解導入開始日 ー 奏効判定日
m_data$寛解導入療法_奏効判定 <- ifelse(as.Date(m_data$寛解導入療法) - as.Date(m_data$奏効判定) <0, "", as.Date(m_data$寛解導入療法) - as.Date(m_data$奏効判定))
# 初回移植_地固め治療:初回移植日ー地固め治療開始日
m_data$初回移植_地固め治療 <- ifelse(as.Date(m_data$初回移植) - as.Date(m_data$地固め治療) <0, "", as.Date(m_data$初回移植) - as.Date(m_data$地固め治療))
# 初回移植_維持治療:初回移植日ー維持治療開始日
m_data$初回移植_維持治療 <- ifelse(as.Date(m_data$初回移植) - as.Date(m_data$維持治療) <0, "", as.Date(m_data$初回移植) - as.Date(m_data$維持治療))
# 初回移植_初回PD判定:初回移植日ー初回PD判定日
m_data$初回移植_初回PD判定 <- ifelse(as.Date(m_data$初回移植) - as.Date(m_data$初回PD判定) <0, "", as.Date(m_data$初回移植) - as.Date(m_data$初回PD判定))
# 奏効判定_初期治療最終投与:奏効判定日 ー 初期治療最終投与日
m_data$奏効判定_初期治療最終投与 <- ifelse(as.Date(m_data$奏効判定) - as.Date(m_data$初期治療最終投与) <0, "", as.Date(m_data$奏効判定) - as.Date(m_data$初期治療最終投与))
# 初期治療最終投与_初回PD判定:初期治療最終投与日ー初回PD判定日
m_data$初期治療最終投与_初回PD判定 <- ifelse(as.Date(m_data$初期治療最終投与) - as.Date(m_data$初回PD判定) <0, "", as.Date(m_data$初期治療最終投与) - as.Date(m_data$初回PD判定))
# 初回PD判定_初回サルベージ:初回PD判定日 ー 初回サルベージ開始日
m_data$初回PD判定_初回サルベージ <- ifelse(as.Date(m_data$初回PD判定) - as.Date(m_data$初回サルベージ) <0, "", as.Date(m_data$初回PD判定) - as.Date(m_data$初回サルベージ))
# 初回サルベージ_初回サルベージ奏効:初回サルベージ開始日 ー初回サルベージ治療_奏効判定日
m_data$初回サルベージ_初回サルベージ奏効 <- ifelse(as.Date(m_data$初回サルベージ) - as.Date(m_data$初回サルベージ奏効) <0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$初回サルベージ奏効))
# 初回サルベージ奏効_初回サルベージ増悪:初回サルベージ奏効判定日 ー初回サルベージ治療増悪判定日
m_data$初回サルベージ奏効_初回サルベージ増悪 <- ifelse(as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪) <0, "", as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪))
# 初回サルベージ奏効_初回サルベージ生存:初回サルベージ奏効判定日 ー初回サルベージ療法_生存確認日
m_data$初回サルベージ奏効_初回サルベージ生存 <- ifelse(as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ生存) <0, "", as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ生存))
# 初回サルベージ_2回サルベージ:初回サルベージ開始日 ー 2回目サルベージ
m_data$初回サルベージ_2回サルベージ <- ifelse(as.Date(m_data$初回サルベージ) - as.Date(m_data$`2回サルベージ`) <0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$`2回サルベージ`))
# 2回サルベージ_3回サルベージ:2回目サルベージ開始日 ー 3回目サルベージ
m_data$`2回サルベージ_3回サルベージ` <- ifelse(as.Date(m_data$`2回サルベージ`) - as.Date(m_data$`3回サルベージ`) <0, "", as.Date(m_data$`2回サルベージ`) - as.Date(m_data$`3回サルベージ`))
# 症候性_症候性病型移行:症候性診断日 ー 症候性病型移行日
m_data$症候性_症候性病型移行 <- ifelse(as.Date(m_data$症候性診断日) - as.Date(m_data$症候性病型移行) <0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$症候性病型移行))
# 症候性_二次発がん:症候性診断日 ー 二次発がん
m_data$症候性_二次発がん <- ifelse(as.Date(m_data$症候性診断日) - as.Date(m_data$二次発がん) <0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$二次発がん))
# 症候性_最終生存確認:症候性診断日 ー 最終生存確認日
m_data$症候性_最終生存確認 <- ifelse(as.Date(m_data$症候性診断日) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$最終生存確認日))
# 寛解導入療法_最終生存確認:初回寛解導入開始日 ー 最終生存確認日
m_data$寛解導入療法_最終生存確認 <- ifelse(as.Date(m_data$寛解導入療法) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$寛解導入療法) - as.Date(m_data$最終生存確認日))
# 初回移植_最終生存確認:初回移植日 ー 最終生存確認日
m_data$初回移植_最終生存確認 <- ifelse(as.Date(m_data$初回移植) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$初回移植) - as.Date(m_data$最終生存確認日))
# 奏効判定_最終生存確認:奏効判定日 ー 最終生存確認日
m_data$奏効判定_最終生存確認 <- ifelse(as.Date(m_data$奏効判定) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$奏効判定) - as.Date(m_data$最終生存確認日))
# 初期治療最終投与_最終生存確認:初期治療最終投与日 ー 最終生存確認日
m_data$初期治療最終投与_最終生存確認 <- ifelse(as.Date(m_data$初期治療最終投与) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$初期治療最終投与) - as.Date(m_data$最終生存確認日))
# 初回サルベージ_最終生存確認:初回サルベージ開始日 ー 最終生存確認日
m_data$初回サルベージ_最終生存確認 <- ifelse(as.Date(m_data$初回サルベージ) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$最終生存確認日))
# 症候性病型移行_最終生存確認:症候性病型移行日 ー 最終生存確認日
m_data$症候性病型移行_最終生存確認 <- ifelse(as.Date(m_data$症候性病型移行) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$症候性病型移行) - as.Date(m_data$最終生存確認日))
# 二次発がん_最終生存確認:二次発がん ー 最終生存確認日
m_data$二次発がん_最終生存確認 <- ifelse(as.Date(m_data$二次発がん) - as.Date(m_data$最終生存確認日) <0, "", as.Date(m_data$二次発がん) - as.Date(m_data$最終生存確認日))
# 症候性_死亡日:症候性診断日 ー 死亡日
m_data$症候性_死亡日 <- ifelse(as.Date(m_data$症候性診断日) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$死亡日))
# 寛解導入療法_死亡日:初回寛解導入開始日 ー 死亡日
m_data$寛解導入療法_死亡日 <- ifelse(as.Date(m_data$寛解導入療法) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$寛解導入療法) - as.Date(m_data$死亡日))
# 初回移植_死亡日:初回移植日 ー 死亡日
m_data$初回移植_死亡日 <- ifelse(as.Date(m_data$初回移植) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$初回移植) - as.Date(m_data$死亡日))
# 奏効判定_死亡日:奏効判定日 ー 死亡日
m_data$奏効判定_死亡日 <- ifelse(as.Date(m_data$奏効判定) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$奏効判定) - as.Date(m_data$死亡日))
# 初期治療最終投与_死亡日:初期治療最終投与日 ー 死亡日
m_data$初期治療最終投与_死亡日 <- ifelse(as.Date(m_data$初期治療最終投与) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$初期治療最終投与) - as.Date(m_data$死亡日))
# 初回サルベージ_死亡日:初回サルベージ開始日 ー 死亡日
m_data$初回サルベージ_死亡日 <- ifelse(as.Date(m_data$初回サルベージ) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$死亡日))
# 症候性病型移行_死亡日:症候性病型移行日 ー 死亡日
m_data$症候性病型移行_死亡日 <- ifelse(as.Date(m_data$症候性病型移行) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$症候性病型移行) - as.Date(m_data$死亡日))
# 二次発がん_死亡日:二次発がん ー 死亡日
m_data$二次発がん_死亡日 <- ifelse(as.Date(m_data$二次発がん) - as.Date(m_data$死亡日) <0, "", as.Date(m_data$二次発がん) - as.Date(m_data$死亡日))
# 最終生存確認_中止日:最終生存確認日 ー 中止日
m_data$最終生存確認_中止日 <- as.Date(m_data$最終生存確認日) - as.Date(m_data$中止日)
# 死亡日_中止日:死亡日 ー 中止日
m_data$死亡日_中止日 <- as.Date(m_data$死亡日) - as.Date(m_data$中止日)

# データ整列
m_data <- m_data[, c(1,2,5,9,10,6,7,8,11:14,16,37,18,38,19,20,22,24:36,42:57,39,58:60,40,41,61:83)]

#データのアウトプット
output_data <- paste("jsh_mm-15_datacleaninng", DLDate, ".csv", sep = "")
setwd(paste0(prtpath, "/output"))
write.csv(m_data, output_data, row.names = F, na = "")
