#jshmm-15 データクリーニング用プログラム
#kazumi takeuchi
#2017/6/20
#2018/05/28:# downloaded date上書き、データのアウトプットfile名の導出を修正
#2018/06/14:#条件式を加える 箇所の式が働いておらず、as.characterを加えた式へ修正,# downloaded date上書き
#2021/06/01:パス参照に関する処理を見直し（Agata.K）
#test

#setwd("../rawdata") 2021/6/1修正
prtpath <- "//192.168.200.222/Datacenter/Trials/JSH/MM-15/04.03.02 定期モニタリングレポート/第5回/01_クリーニング/20210601"
rawdatapath <- paste0(prtpath, "/rawdata")
outputpath <-  paste0(prtpath, "/output")

setwd(rawdatapath)
list <- list.files()
file.name <- sub("_210601_1118.*", "", list)  # downloaded date
df.name <- sub(".*_", "", file.name)

for (i in 1:length(list)) {
  assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}

#必要なラベルの取り出し

registration1 <- registration[, c("症例登録番号", "症例登録日")]
baseline1 <- baseline[, c("症例登録番号", "診断日")]
baselineEx1 <- baselineEx[, c("症例登録番号", "診断日")]
remitherapy1 <- remitherapy[, c("症例登録番号", "初回寛解導入療法.開始日")]
initialtreatment1 <- initialtreatment[, c("症例登録番号", "奏効判定日", "初期治療.最終投与日")]
salvagetherapy1_1 <- salvagetherapy1[, c("症例登録番号", "初回サルベージ療法開始日")]
followup1 <- followup[,c ("症例登録番号", "最終生存確認日", "死亡日")]

#新たな名前をつける
names(baseline1)[2] <- c("症候性骨髄腫_診断日")
names(baseline1)
names(baselineEx1)[2] <- c("症候性骨髄腫以外_診断日")
names(baselineEx1)

#マージする（key=症例登録番号）
m_re_base1 <- merge(registration1, baseline1, by="症例登録番号", all.x = T)
m_re_base1_baseEx1 <- merge(m_re_base1, baselineEx1, by="症例登録番号",all.x = T)
m_re_base1_baseEx1_remi1 <- merge(m_re_base1_baseEx1, remitherapy1, by="症例登録番号", all.x = T)
m_re_base1_baseEx1_remi1_initial1 <- merge(m_re_base1_baseEx1_remi1, initialtreatment1, by = "症例登録番号",all.x = T)
m_re_base1_baseEx1_remi1_initial1_salvage1_1 <- merge(m_re_base1_baseEx1_remi1_initial1, salvagetherapy1_1, by = "症例登録番号", all.x = T)
m_re_base1_baseEx1_remi1_initial1_salvage1_1_follow1 <- merge(m_re_base1_baseEx1_remi1_initial1_salvage1_1, followup1, by = "症例登録番号", all.x = T)
m_data <- m_re_base1_baseEx1_remi1_initial1_salvage1_1_follow1
#条件式を加える
m_data$diff_症候性_診断日_初回寛解導入.開始日 <- as.Date(as.character(m_data$"症候性骨髄腫_診断日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"初回寛解導入療法.開始日", format = "%Y/%m/%d"))
m_data$diff_初回寛解導入.開始日_奏効判定日 <- as.Date(as.character(m_data$"初回寛解導入療法.開始日",format = "%Y/%m/%d")) - as.Date(as.character(m_data$"奏効判定日", format = "%Y/%m/%d"))
m_data$diff_奏効判定日_初期治療.最終投与日 <- as.Date(as.character(m_data$"奏効判定日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"初期治療.最終投与日", format = "%Y/%m/%d"))
m_data$diff_初期治療.最終投与日_初回サルベージ開始日 <- as.Date(as.character(m_data$"初期治療.最終投与日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"初回サルベージ療法開始日", format = "%Y/%m/%d"))
m_data$diff_症候性以外_診断日_最終生存確認日 <- as.Date(as.character(m_data$"症候性骨髄腫以外_診断日",format = "%Y/%m/%d")) - as.Date(as.character(m_data$"最終生存確認日", format = "%Y/%m/%d"))
m_data$diff_症候性_診断日_最終生存確認日 <- as.Date(as.character(m_data$"症候性骨髄腫_診断日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"最終生存確認日", format = "%Y/%m/%d"))
m_data$diff_初回寛解導入.開始日_最終生存確認日 <- as.Date(as.character(m_data$"初回寛解導入療法.開始日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"最終生存確認日", format = "%Y/%m/%d"))
m_data$diff_奏効判定日_最終生存確認日 <- as.Date(as.character(m_data$"奏効判定日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"最終生存確認日", format = "%Y/%m/%d"))
m_data$diff_初期治療.最終投与日_最終生存確認日 <- as.Date(as.character(m_data$"初期治療.最終投与日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"最終生存確認日", format = "%Y/%m/%d"))
m_data$diff_初回サルベージ開始日_最終生存確認日 <- as.Date(as.character(m_data$"初回サルベージ療法開始日",format = "%Y/%m/%d")) - as.Date(as.character(m_data$"最終生存確認日", format = "%Y/%m/%d"))

m_data$diff_症候性以外_診断日_死亡日 <- as.Date(as.character(m_data$"症候性骨髄腫以外_診断日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"死亡日", format = "%Y/%m/%d"))
m_data$diff_症候性_診断日_死亡日 <- as.Date(as.character(m_data$"症候性骨髄腫_診断日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"死亡日", format = "%Y/%m/%d"))
m_data$diff_初回寛解導入.開始日_死亡日 <- as.Date(as.character(m_data$"初回寛解導入療法.開始日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"死亡日", format = "%Y/%m/%d"))
m_data$diff_奏効判定日_死亡日 <- as.Date(as.character(m_data$"奏効判定日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"死亡日", format = "%Y/%m/%d"))
m_data$diff_初期治療.最終投与日_死亡日 <- as.Date(as.character(m_data$"初期治療.最終投与日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"死亡日", format = "%Y/%m/%d"))
m_data$diff_初回サルベージ開始日_死亡日 <- as.Date(as.character(m_data$"初回サルベージ療法開始日", format = "%Y/%m/%d")) - as.Date(as.character(m_data$"死亡日", format = "%Y/%m/%d"))

m_data$devi_diff_症候性_診断日_初回寛解導入.開始日 <- ifelse(m_data$diff_症候性_診断日_初回寛解導入.開始日 <= 0, "" , m_data$diff_症候性_診断日_初回寛解導入.開始日 )
m_data$devi_diff_初回寛解導入.開始日_奏効判定日 <- ifelse(m_data$diff_初回寛解導入.開始日_奏効判定日 <= 0, "", m_data$diff_初回寛解導入.開始日_奏効判定)
m_data$devi_diff_奏効判定日_初期治療.最終投与日 <- ifelse(m_data$diff_奏効判定日_初期治療.最終投与日 <= 0, "", m_data$diff_奏効判定日_初期治療.最終投与日)
m_data$devi_diff_初期治療.最終投与日_初回サルベージ開始日 <- ifelse(m_data$diff_初期治療.最終投与日_初回サルベージ開始日 <= 0, "", m_data$diff_初期治療.最終投与日_初回サルベージ開始日)
m_data$devi_diff_症候性以外_診断日_最終生存確認日 <- ifelse(m_data$diff_症候性以外_診断日_最終生存確認日 <= 0, "", m_data$diff_症候性以外_診断日_最終生存確認日)
m_data$devi_diff_症候性_診断日_最終生存確認日 <- ifelse(m_data$diff_症候性_診断日_最終生存確認日 <= 0, "", m_data$diff_症候性_診断日_最終生存確認日)
m_data$devi_diff_初回寛解導入.開始日_最終生存確認日 <- ifelse(m_data$diff_初回寛解導入.開始日_最終生存確認日 <= 0, "", m_data$diff_初回寛解導入.開始日_最終生存確認日)
m_data$devi_diff_奏効判定日_最終生存確認日 <- ifelse(m_data$diff_奏効判定日_最終生存確認日 <= 0, "", m_data$diff_奏効判定日_最終生存確認日)
m_data$devi_diff_初期治療.最終投与日_最終生存確認日 <- ifelse(m_data$diff_初期治療.最終投与日_最終生存確認日 <= 0, "", m_data$diff_初期治療.最終投与日_最終生存確認日)
m_data$devi_diff_初回サルベージ開始日_最終生存確認日 <- ifelse(m_data$diff_初回サルベージ開始日_最終生存確認日 <= 0, "", m_data$diff_初回サルベージ開始日_最終生存確認日)
m_data$devi_diff_症候性以外_診断日_死亡日 <- ifelse(m_data$diff_症候性以外_診断日_死亡日 <= 0, "", m_data$diff_症候性以外_診断日_死亡日)
m_data$devi_diff_症候性_診断日_死亡日 <- ifelse(m_data$diff_症候性_診断日_死亡日 <= 0, "", m_data$diff_症候性_診断日_死亡日)
m_data$devi_diff_初回寛解導入.開始日_死亡日 <- ifelse(m_data$diff_初回寛解導入.開始日_死亡日 <= 0, "", m_data$diff_初回寛解導入.開始日_死亡日)
m_data$devi_diff_奏効判定日_死亡日 <- ifelse(m_data$diff_奏効判定日_死亡日 <= 0, "", m_data$diff_奏効判定日_死亡日)
m_data$devi_diff_初期治療.最終投与日_死亡日 <- ifelse(m_data$diff_初期治療.最終投与日_死亡日  <= 0, "", m_data$diff_初期治療.最終投与日_死亡日)
m_data$devi_diff_初回サルベージ開始日_死亡日 <- ifelse(m_data$diff_初回サルベージ開始日_死亡日 <= 0, "", m_data$diff_初回サルベージ開始日_死亡日)

#データのアウトプット
output_data <- paste("jsh _mm-15_datacleaninng", "_210601_1118", ".csv", sep = "") # downloaded date

#setwd("../output")2021/6/1
setwd(outputpath)

write.csv(m_data, output_data, row.names = F, na = "")
