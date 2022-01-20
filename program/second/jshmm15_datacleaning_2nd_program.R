##################################################
# Program : jshmm15_datacleaning_2nd_program.R
# Study : JSH-MM-15
# Writer : Akihiro Sano
# Date：2022/1/6
# 2022/1/17：定モニ用データセット仕様書変更のため修正
# 2022/1/20：軽微な修正
##################################################

setwd("//172.16.0.222/Stat/Trials/JSH/JSH-MM-15_DC/rawdata/最新（ANSI）")
list <- list.files()  # get the file name

file.name <- sub("_220112_1230.*", "", list)  # chnage downloaded date → ""

df.name <- sub(".*_", "", file.name)  # regular expression substitution

# name it again(allocation).
df.name[1] <- c("allocation")

for (i in 1:length(list)){  
  assign(df.name[i], read.csv(list[i], as.is =T, na.strings = c("")))  
}

#  take out the labels(extraction)
registration1 <- registration[, c("症例登録番号", "症例登録日")]
baseline1 <- baseline[, c("症例登録番号", "診断日")]  # 症候性骨髄腫baseline
baselineEx1 <- baselineEx[, c("症例登録番号", "診断日")]  # 症候性骨髄腫以外baseline
remitherapy1 <- remitherapy[, c("症例登録番号", "初回寛解導入療法.開始日")]
transplant1 <- transplant[, c("症例登録番号", "初回移植日")]  
constherapy1 <- constherapy[, c("症例登録番号", "地固め治療開始日")]　　
maintetherapy1 <- maintetherapy[, c("症例登録番号", "維持治療開始日")]  
initialtreatment1 <- initialtreatment[, c("症例登録番号", "奏効判定日", "初回PDの判定日", "初期治療.最終投与日", "初期治療中のbest.response")]  # 初期治療中のbest.respones追加
salvagetherapy1_1 <- salvagetherapy1[, c("症例登録番号", "初回サルベージ療法開始日")]　　
salvagetherapy1out1 <- salvagetherapy1out[, c("症例登録番号", "初回サルベージ治療.奏効判定日", "初回サルベージ治療後の初回再発または増悪の判定日", "初回サルベージ療法開始後の最終無増悪生存確認日", "初回サルベージ中の奏効.best.response.", "初回サルベージ治療後の初回再発または増悪の有無" )]  # 初回サルベージ中の奏効.best.response追加
salvagetherapy2_1 <- salvagetherapy2[, c("症例登録番号", "X2回目のサルベージ治療開始日")]  
salvagetherapy3_1 <- salvagetherapy3[, c("症例登録番号", "X3回目のサルベージ治療開始日")]
radiationtherapy1 <- radiationtherapy[, c("症例登録番号", "放射線治療.照射開始日")] 
transition11_1 <- transition11[, c("症例登録番号", "移行した病型の診断日")] 
transition22_1 <- transition22[, c("症例登録番号", "移行した病型の診断日")]
secondcancer1 <- secondcancer[, c("症例登録番号", "二次発がん診断日")]
discontinuation1 <- discontinuation[, c("症例登録番号", "中止日", "中止理由")]
followup1 <- followup[, c("症例登録番号", "最終生存確認日", "死亡日", "死亡の原因")]


# give it new names
names(baseline1)[2] <- c("症候性診断日")
names(baselineEx1)[2] <- c("症候性以外診断日")
# add from here down
names(remitherapy1)[2] <- c("寛解導入療法")
names(transplant1)[2] <- c("初回移植")
names(constherapy1)[2] <- c("地固め治療")
names(maintetherapy1)[2] <- c("維持治療")
names(initialtreatment1)[2:4] <- c("奏効判定", "初回PD判定", "初期治療最終投与")
names(salvagetherapy1_1)[2] <- c("初回サルベージ")
names(salvagetherapy1out1)[2:4] <- c("初回サルベージ奏効", "初回サルベージ増悪", "初回サルベージ生存")
names(salvagetherapy2_1)[2] <- c("2回サルベージ")
names(salvagetherapy3_1)[2] <- c("3回サルベージ")
names(radiationtherapy1)[2] <- c("照射開始")
names(transition11_1)[2] <- c("症候性以外病型移行")
names(transition22_1)[2] <- c("症候性病型移行")
names(secondcancer1)[2] <- c("二次発がん")

# edit allocation
allocation[, 4] <- ifelse(allocation$診断分類 == "症候性以外", TRUE, FALSE)
allocation[, 5] <- ifelse(allocation$診断分類 == "症候性骨髄腫（症候性骨髄腫、非分泌型骨髄腫、多発性形質細胞腫、形質細胞白血病）" | allocation$病型移行報告書.症候性以外.症候性骨髄腫. == "症候性骨髄腫に移行した", TRUE, FALSE)  # 確認用
names(allocation)[4] <- c("症候性以外_割付")
names(allocation)[5] <- c("症候性_割付")
allocation1 <- allocation[, c("症例登録番号", "症候性以外_割付", "症候性_割付")]

# merge
# registration1, allocation1, baseline1, baselineEx1, remitherapy1, transplant1
m_re1_alloc1 <- merge(registration1, allocation1, by = "症例登録番号", all.x =T)
m_re1_alloc1_base1 <- merge(m_re1_alloc1, baseline1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1 <- merge(m_re1_alloc1_base1, baselineEx1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1 <- merge(m_re1_alloc1_base1_baseEx1, remitherapy1, by = "症例登録番号", all.x =T)
m_re1_alloc1_base1_baseEx1_remi1_trans1 <- merge(m_re1_alloc1_base1_baseEx1_remi1, transplant1, by = "症例登録番号", all.x = T)

# constherapy1 maintetherapy1 initialtreatment1 salvagetherapy1_1 salvagetherapy1out1 salvagetherapy2_1 salvagetherapy3_1
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1, constherapy1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1, maintetherapy1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1, initialtreatment1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1, salvagetherapy1_1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1, salvagetherapy1out1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1, salvagetherapy2_1, by = "症例登録番号", all.x = T )
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1, salvagetherapy3_1, by = "症例登録番号", all.x = T)


# radiationtherapy1 transition11_1 transition22_1 secondcancer1 discontinuation1 followup1
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1, radiationtherapy1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1, transition11_1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1, transition22_1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1_sec1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1, secondcancer1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1_sec1_dis1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1_sec1, discontinuation1, by = "症例登録番号", all.x = T)
m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1_sec1_dis1_fo1 <- merge(m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1_sec1_dis1, followup1, by = "症例登録番号", all.x = T)
m_data <- m_re1_alloc1_base1_baseEx1_remi1_trans1_cons1_main1_initial1_salv1_1_salv1out1_salv2_1_salv3_1_rad1_trans11_1_trans22_1_sec1_dis1_fo1


# diff
# as.chracterおよび、format = %Y/%m/%dははずしている
# 症候性以外骨髄腫
m_data$症候性以外_症候性診断日 <- 
  ifelse(
    as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性診断日) <=0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性診断日)
  )
m_data$症候性以外_照射開始 <- 
  ifelse(
    as.Date(m_data$症候性以外診断日) - as.Date(m_data$照射開始) <=0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$照射開始)
  )
m_data$症候性以外_症候性以外病型移行 <- 
  ifelse(
    as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性以外病型移行) <=0, "", (as.Date(m_data$症候性以外診断日) - as.Date(m_data$症候性以外病型移行, format = "%Y/%m/%d"))
  )
m_data$照射開始_最終生存確認 <- 
  ifelse(
    as.Date(m_data$照射開始) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$照射開始) - as.Date(m_data$最終生存確認日)
  )
m_data$症候性以外病型移行_最終生存確認 <-
  ifelse(
    as.Date(m_data$症候性以外病型移行) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$症候性以外病型移行) - as.Date(m_data$最終生存確認日)
  )
m_data$症候性以外_最終生存確認 <- 
  ifelse(
    as.Date(m_data$症候性以外診断日) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$最終生存確認日)
  )
m_data$照射開始_死亡日 <- 
  ifelse(
    as.Date(m_data$照射開始) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$照射開始) - as.Date(m_data$死亡日)
  )
m_data$症候性以外病型移行_死亡日 <- 
  ifelse(
    as.Date(m_data$症候性以外病型移行) - as.Date(m_data$死亡日) <= 0, "", as.Date(m_data$症候性以外病型移行) - as.Date(m_data$死亡日)
  )
m_data$症候性以外_死亡日 <- 
  ifelse(
    as.Date(m_data$症候性以外診断日) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$症候性以外診断日) - as.Date(m_data$死亡日)
  )
# 症候性骨髄腫
m_data$症候性_寛解導入療法 <- 
  ifelse(
    as.Date(m_data$症候性診断日) - as.Date(m_data$寛解導入療法) <=0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$寛解導入療法)
  )
m_data$寛解導入療法_奏効判定 <- 
  ifelse(
    as.Date(m_data$寛解導入療法) - as.Date(m_data$奏効判定) <=0, "", as.Date(m_data$寛解導入療法) - as.Date(m_data$奏効判定)
  )
m_data$初回移植_地固め治療 <- 
  ifelse(
    as.Date(m_data$初回移植) - as.Date(m_data$地固め治療) <=0, "", as.Date(m_data$初回移植) - as.Date(m_data$地固め治療)
  )
m_data$初回移植_維持治療 <- 
  ifelse(
    as.Date(m_data$初回移植) - as.Date(m_data$維持治療) <=0, "", as.Date(m_data$初回移植) - as.Date(m_data$維持治療)
  )
m_data$初回移植_初回PD判定 <- 
  ifelse(
    as.Date(m_data$初回移植) - as.Date(m_data$初回PD判定) <=0, "", as.Date(m_data$初回移植) - as.Date(m_data$初回PD判定)
  )
m_data$奏効判定_初期治療最終投与 <- 
  ifelse(
    as.Date(m_data$奏効判定) - as.Date(m_data$初期治療最終投与) <=0, "", as.Date(m_data$奏効判定) - as.Date(m_data$初期治療最終投与)
  )
m_data$初期治療最終投与_初回PD判定 <- 
  ifelse(
    as.Date(m_data$初期治療最終投与) - as.Date(m_data$初回PD判定) <=0, "", as.Date(m_data$初期治療最終投与) - as.Date(m_data$初回PD判定)
  )
m_data$初回PD判定_初回サルベージ <- 
  ifelse(
    as.Date(m_data$初回PD判定) - as.Date(m_data$初回サルベージ) <=0, "", as.Date(m_data$初回PD判定) - as.Date(m_data$初回サルベージ)
  )
m_data$初回サルベージ_初回サルベージ奏効 <- 
  ifelse(
    as.Date(m_data$初回サルベージ) - as.Date(m_data$初回サルベージ奏効) <=0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$初回サルベージ奏効)
  )
m_data$初回サルベージ奏効_初回サルベージ増悪 <- 
  ifelse(
    as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪) <=0, "", as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪)
  )
m_data$初回サルベージ奏効_初回サルベージ生存 <- 
  ifelse(
    as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ生存) <=0, "", as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ生存)
  )
m_data$初回サルベージ_2回サルベージ <- 
  ifelse(
    as.Date(m_data$初回サルベージ) - as.Date(m_data$`2回サルベージ`) <=0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$`2回サルベージ`)
  )
m_data$`2回サルベージ_3回サルベージ` <- 
  ifelse(
    as.Date(m_data$`2回サルベージ`) - as.Date(m_data$`3回サルベージ`) <=0, "", as.Date(m_data$`2回サルベージ`) - as.Date(m_data$`3回サルベージ`)
  )
m_data$症候性_症候性病型移行 <- 
  ifelse(
    as.Date(m_data$症候性診断日) - as.Date(m_data$症候性病型移行) <=0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$症候性病型移行)
  )
m_data$症候性_二次発がん <- 
  ifelse(
    as.Date(m_data$症候性診断日) - as.Date(m_data$二次発がん) <=0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$二次発がん)
  )
m_data$症候性_最終生存確認 <- 
  ifelse(
    as.Date(m_data$症候性診断日) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$最終生存確認日)
  )
m_data$寛解導入療法_最終生存確認 <-
  ifelse(
    as.Date(m_data$寛解導入療法) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$寛解導入療法) - as.Date(m_data$最終生存確認日)
  )
m_data$初回移植_最終生存確認 <- 
  ifelse(
    as.Date(m_data$初回移植) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$初回移植) - as.Date(m_data$最終生存確認日)
  )
m_data$奏効判定_最終生存確認 <- 
  ifelse(
    as.Date(m_data$奏効判定) - as.Date(m_data$最終生存確認) <=0, "", as.Date(m_data$奏効判定) - as.Date(m_data$最終生存確認)
  )
m_data$初期治療最終投与_最終生存確認 <- 
  ifelse(
    as.Date(m_data$初期治療最終投与) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$初期治療最終投与) - as.Date(m_data$最終生存確認日)
  )
m_data$初回サルベージ_最終生存確認 <- 
  ifelse(
    as.Date(m_data$初回サルベージ) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$最終生存確認日)
  )
m_data$症候性病型移行_最終生存確認 <- 
  ifelse(
    as.Date(m_data$症候性病型移行)- as.Date(m_data$最終生存確認) <=0, "", as.Date(m_data$症候性病型移行)- as.Date(m_data$最終生存確認)
  )
m_data$二次発がん_最終生存確認 <- 
  ifelse(
    as.Date(m_data$二次発がん) - as.Date(m_data$最終生存確認日) <=0, "", as.Date(m_data$二次発がん) - as.Date(m_data$最終生存確認日)
  )
m_data$症候性_死亡日 <- 
  ifelse(
    as.Date(m_data$症候性診断日) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$症候性診断日) - as.Date(m_data$死亡日)
  )
m_data$寛解導入療法_死亡日 <- 
  ifelse(
    as.Date(m_data$寛解導入療法) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$寛解導入療法) - as.Date(m_data$死亡日)
  )
m_data$初回移植_死亡日 <- 
  ifelse(
    as.Date(m_data$初回移植) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$初回移植) - as.Date(m_data$死亡日)
  )
m_data$奏効判定_死亡日 <- 
  ifelse(
    as.Date(m_data$奏効判定) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$奏効判定) - as.Date(m_data$死亡日)
  )
m_data$初期治療最終投与_死亡日 <- 
  ifelse(
    as.Date(m_data$初期治療最終投与) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$初期治療最終投与) - as.Date(m_data$死亡日)
  )
m_data$初回サルベージ_死亡日 <- 
  ifelse(
    as.Date(m_data$初回サルベージ) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$初回サルベージ) - as.Date(m_data$死亡日)
  )
m_data$症候性病型移行_死亡日 <- 
  ifelse(
    as.Date(m_data$症候性病型移行) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$症候性病型移行) - as.Date(m_data$死亡日)
  )
m_data$二次発がん_死亡日 <- 
  ifelse(
    as.Date(m_data$二次発がん) - as.Date(m_data$死亡日) <=0, "", as.Date(m_data$二次発がん) - as.Date(m_data$死亡日)
  )
# diff(calculate the number of days)
m_data$最終生存確認_中止日 <- as.Date(m_data$最終生存確認日) - as.Date(m_data$中止日)
m_data$死亡日_中止日 <- as.Date(m_data$死亡日) - as.Date(m_data$中止日)

# processing
m_data$症候性以外診断日確認 <- ifelse(
  as.Date(m_data$症候性以外診断日) -as.Date("2015-12-31") <=0, "←",""
)
m_data$症候性診断日確認 <- ifelse(
  as.Date(m_data$症候性診断日) - as.Date("2015-12-31") <=0, "←", ""
)
m_data$初回PD判定確認 <- ifelse(
  is.na(m_data$初回PD判定)  & !(is.na(m_data$初回サルベージ)), "←", ""
)
m_data$初期治療中のbest.response <- ifelse(
  m_data$初期治療中のbest.response == "PD" & as.Date(m_data$奏効判定) - as.Date(m_data$初回PD判定) !=0, "←", ""
)
m_data$初期治療最終投与_初回PD判定_1年確認 <-　ifelse(
  as.Date(m_data$初期治療最終投与) - as.Date(m_data$初回PD判定) >= 365, "←", ""
) 
m_data$初回サルベージ奏効_初回サルベージ増悪_SD以上 <- ifelse(
                                          m_data$初回サルベージ中の奏効.best.response. %in% c("CR", "PR", "SD", "sCR", "VGPR") & m_data$初回サルベージ治療後の初回再発または増悪の有無 == "あり" &  as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪) >=0, "←", "")
m_data$初回サルベージ奏効_初回サルベージ増悪_PD <- ifelse(
                                        m_data$初回サルベージ中の奏効.best.response. == "PD" & m_data$初回サルベージ治療後の初回再発または増悪の有無 == "あり" & as.Date(m_data$初回サルベージ奏効) - as.Date(m_data$初回サルベージ増悪) <= 0-14, "←", ""
)
# reordering
m_data <- m_data[, c(1:3, 6, 74, 4, 5, 75, 7:11, 14, 12, 76, 13, 15:18, 21:47, 77, 48:50, 78, 79, 51:73)]
View(m_data)
# output
output_data <- paste("jsh_mm15_dataclelaning", "_20220120_1238", ".csv", sep = "")
setwd("//172.16.0.222/Stat/Trials/JSH/JSH-MM-15_DC/output/second")
write.csv(m_data, output_data, row.names = F, na = "")
