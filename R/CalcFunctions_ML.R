Rel <- function(data) {
  dat <- data %>% 
    select(-uid) %>% 
    mutate_all(as.numeric) %>% 
    alpha()
  
  dat$total
}

Descript <- function(data){
  data %>% 
    group_by(uid) %>% 
    mutate_all(as.numeric) %>% 
    gather(key = item, value = value, -uid) %>% 
    summarise(score = mean(value)) %>% 
    ungroup() %>% 
    summarise(mean = mean(score),
              sd = sd(score),
              min = min(score),
              max = max(score))
}

Histo <- function(data, variable) {
  data %>% 
    ggplot(aes_string(x = variable, color = variable)) +
    geom_histogram() +
    scale_color_brewer(palette = "Set1") +
    xlab(renamevarnames(variable))
}

ConfDec <- function(data1, data2) {
  left_join(data1, data2) %>% 
  gather(key = variable, value = value, -uid) %>% 
  separate(variable, c("ADR", "IND", "var", "item"), "_") %>% 
  select(-ADR, -IND) %>% 
  spread(key = var, value = value)
}

HalfSplit <- function(data1, data2) {
  left_join(data1, data2) %>% 
    gather(key = variable, value = value, -uid) %>% 
    mutate(variable = str_replace(variable, "_S", "__S")) %>% 
    separate(variable, c("ADR", "IND", "var", "item"), "_") %>% 
    select(-ADR, -IND) %>% 
    spread(key = var, value = value) %>% 
    mutate(item = as.numeric(item),
           split = ifelse(item %% 2 ==0, "Even", "Odd"))
}

SplitCorr <- function(data) {
  data %>% 
    group_by(split) %>% 
    nest() %>% 
    mutate(splitbiasscores = map(data, ~ group_by(., uid) %>% 
                                   summarise(meanconf = mean(confidence),
                                             meanacc = mean(Score),
                                             meanbias = meanconf - meanacc*100)%>% 
                                   select(uid, meanbias))) %>% 
    select(-data) %>% 
    unnest() %>% 
    spread(key = split, value = meanbias) %>% 
    select(-uid) %>% 
    correlate()
}

VarianceFlag <- function(data) {
  data1 <- data %>% 
  group_by(uid) %>% 
  summarise(varianceconf = sd(confidence),
            variancedec = sd(decision)) %>% 
  mutate(Flag = ifelse(varianceconf*variancedec == 0, "FLAG", "OK")) %>% 
  filter(Flag == "FLAG")
  
  if (nrow(data1) == 0) {"OK"} else {(data1)}
}

POSTCalc <- function(data) {
  data %>% 
    group_by(uid) %>% 
    nest() %>% 
    mutate(glm = map(data, ~coef(summary(glm(decision ~ confidence, data=., family="binomial")))),
           POST = map(glm, ~as.data.frame(.) %>% 
                        select(Estimate) %>% 
                        t() %>% 
                        as.data.frame() %>% 
                        rename(Int = '(Intercept)'))) %>% 
    select(uid, POST) %>% 
    unnest() %>% 
    mutate(POST = (-1 * Int) / confidence) %>% 
    select(uid, POST) %>% 
    mutate(POST = ifelse(uid %in% VarianceFlag(data)$uid, NA, POST),
           POST = ifelse(uid %in% FilterCheck(data)$uid, NA, POST))
}

FilterCheck <- function(data) {
  data %>%
    group_by(uid, decision) %>% 
    summarise(check = mean(confidence)) %>% 
    mutate(decision = ifelse(decision == 0, "No", "Yes")) %>% 
    spread(decision, check) %>% 
    mutate(check = Yes - No > 0) %>% 
    filter(check == FALSE)
}

#Sourced from https://my.ilstu.edu/~wjschne/444/IndependentSamples.html#(1)
#Joel Schneider
t.report <- function(tt){
  tvalue <- tt$statistic %>% formatC(digits = 2, format = "f")
  pvalue <- tt$p.value %>% formatC(digits = 2, format = "f")
  if (round(tt$parameter, 0) == tt$parameter) {
    df <- tt$parameter
  } else {
    df <- formatC(digits = 2, format = "f")
  }
  if (tt$p.value < 0.0005) {
    pvalue <- " < 0.001" 
  } else { 
    if (tt$p.value < 0.005) {
      pvalue <- paste0(" = ",tt$p.value %>% formatC(digits = 3, format = "f"))
    } else {
      pvalue <- paste0(" = ",tt$p.value %>% formatC(digits = 2, format = "f"))
    }
  } 
  paste0("*t*(",df,") = ",tvalue, ", *p*", pvalue)
}

sigselector <- function(var) {
  data_joined_analysis_wfac %>% 
    select(var, hb_var, bias, -matches("_Bias"), 
           Conf:dmtdeci,
           age, gender, aus_born, aus_years, eng_fl,
           FLANK_congrtdiff, RUNL_Acc, TS_switchrtdiff,
           GDMS_Rational:DOI_Score) %>% 
    select_if(is.numeric) %>% 
    corstarsl() %>% 
    select(var) %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    gather(key = key, value = val, -rowname) %>% 
    filter(str_detect(val, "\\*")) %>% 
    select(rowname) %>% 
    filter(!rowname %in% hb_var) %>% 
    unique()
}

var <- "BASRR"

sigselectorall <- function(var) {
  data_joined_analysis_wfac %>% 
    select(var, everything()) %>% 
    select_if(is.numeric) %>% 
    corstarsl() %>% 
    select(var) %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    gather(key = key, value = val, -rowname) %>% 
    filter(str_detect(val, "\\*")) %>% 
    select(rowname, val) %>% 
    unique()
}

fitind <- function(cfa) {
  dat <- lavInspect(cfa, what="fit") %>% 
    as.data.frame() %>% 
    rownames_to_column()
  
  colnames(dat) <- c("fit", "value")
  
  dat %>% 
    spread(fit, value) %>% 
    mutate(chisqoverdf = chisq/df) %>% 
    select(chisq, df, chisqoverdf, gfi, tli, cfi, rmsea, aic)
}
?case_when
renamevarnames <- function(column) {
  case_when(column == "age"~ "Age", 
                              column == "gender"~ 'Gender', 
                              column == "Course"~ "Course",
                              column == "Faculty"~ "Faculty", 
                              column == "CourseType"~ "Course Type",
                              column == "major"~ "Course Major", 
                              column == "minor"~ "Course Minor", 
                              column == "higheststud"~ "Intended Highest Study", 
                              column == "aus_born"~ "Born in Aus", 
                              column == "aus_years"~ "Years in Aus", 
                              column == "eng_fl"~ "English as a First Language", 
                              column == "dic_use"~ "Frequency of Dictionary use",
                              column == "EAT_Acc"~ "EAT Accuracy", 
                              column == "BNT_Acc"~ "BNT Accuracy", 
                              column == "RAPM_Acc"~ "RAPM Accuracy", 
                              column == "CUBEtotal"~ "Cube Comparisons Accuracy", 
                              column == "SUD_Acc"~ "SUD Task Accuracy", 
                              column == "VIS_DSTotal"~ "Visual Search Task Accuracy",
                              column == "EAT_Conf"~ "EAT Confidence", 
                              column == "BNT_Conf"~ "BNT Confidence", 
                              column == "RAPM_Conf"~ "RAPM Confidence", 
                              column == "ADR_Conf"~ "ADR Confidence", 
                              column == "CRT_Conf"~ "CRT Confidence", 
                              column == "MDMT_Conf"~ "MDMT Confidence",
                              column == "CUBEuncertain"~ "Cube Comparisons Uncertainty", 
                              column == "CUBE_Unc_Log"~ "Cube Comparisons Uncertainty (Log)", 
                              column == "SUD_Unc"~ "SUD Task Uncertainty",                   
                              column == "SUD_Unc_Log"~ "SUD Task Uncertainty (Log)", 
                              column == "Vis_UncTotal"~ "Visual Search Task Uncertainty",
                              column == "VIS_Unc_Log"~ "Visual Search Task Uncertainty (Log)",
                              column == "EAT_POST"~ "EAT Control Threshold", 
                              column == "BNT_POST"~ "BNT Control Threshold", 
                              column == "RAPM_POST"~ "RAPM Control Threshold", 
                              column == "ADR_POST"~ "ADR Control Threshold", 
                              column == "CRT_POST"~ "CRT Control Threshold", 
                              column == "MDMT_POST"~ "MDMT Control Threshold",
                              column == "cc_TRUE_RT_Log"~ "Flanker cc RT", 
                              column == "ci_TRUE_RT_Log"~ "Flanker ci RT", 
                              column == "ic_TRUE_RT_Log"~ "Flanker ic RT", 
                              column == "ii_TRUE_RT_Log"~ "Flanker ii RT", 
                              column == "letter_switch_TRUE_RT_Log"~ "Switching LetterSwitch RT", 
                              column == "letter_nonswitch_TRUE_RT_Log"~ "Switching LetterNonSwitch RT", 
                              column == "number_switch_TRUE_RT_Log"~ "Switching NumberSwitch RT", 
                              column == "number_nonswitch_TRUE_RT_Log"~ "Switching NumberNonSwitch RT", 
                              column == "TS_TRUE_RT_Switch_Log"~ "Switching Switch RT", 
                              column == "TS_TRUE_RT_NonSwitch_Log"~ "Switching NonSwitch RT",
                              column == "ci_FALSE_Perc"~ "Flanker ci Error", 
                              column == "ii_FALSE_Perc"~ "Flanker ii Error", 
                              column == "ic_FALSE_Perc"~ "Flanker ic Error", 
                              column == "cc_FALSE_Perc"~ "Flanker cc Error",
                              column == "letter_switch_FALSE_Perc"~ "Switching LetterSwitch Error", 
                              column == "letter_nonswitch_FALSE_Perc"~ "Switching LetterNonSwitch Error",
                              column == "number_switch_FALSE_Perc"~ "Switching NumberSwitch Error", 
                              column == "number_nonswitch_FALSE_Perc"~ "Switching NumberNonSwitch Error", 
                              column == "FLANK_TotalErrors"~ "Flanker Total Error", 
                              column == "FLANK_TRUE_RT_Mean_Log"~ "Flanker Overall RT",
                              column == "TS_TotalErrors"~ "Switching Total Error",
                              column == "TS_TRUE_RT_Mean_Log"~ "Switching Overall RT",
                              column == "FLANK_TRUE_RT_congruent_Log"~ "Flanker c RT",
                              column == "FLANK_TRUE_RT_incongruent_Log"~ "Flanker i RT",
                              column == "TS_switchrtdiff"~ "Switching S-NS RT Difference", 
                              column == "FLANK_congrtdiff"~ "Flanker i-c RT Difference",
                              column == "RUNL_Acc"~ "Running Letters Accuracy",
                              column == "ADR_Acc"~ "Applying Decision Rules Accuracy", 
                              column == "RISK_Score"~ "Risky Gambles Score", 
                              column == "RTF_Score"~ "Resistance to Framing Score", 
                              column == "SN_RankScore"~ "Recognising Social Norms Rank Score", 
                              column == "CRT_Acc"~ "CRT Accuracy", 
                              column == "CRP_Score"~ "CRP Score",
                              column == "ADR_Bias"~ "ADR Bias", 
                              column == "CRT_Bias"~ "CRT Bias", 
                              column == "BNT_Bias"~ "BNT Bias", 
                              column == "MDMT_Bias"~ "MDMT Bias", 
                              column == "EAT_Bias"~ "EAT Bias", 
                              column == "RAPM_Bias"~ "RAPM Bias",
                              column == "MDMT_Acc"~ "MDMT Accuracy",
                              column == "MDMT_Competence"~ "MDMT Competence", 
                              column == "EAT_Competence"~ "EAT Competence", 
                              column == "BNT_Competence"~ "BNT Competence",
                              column == "RAPM_Competence"~ "RAPM Competence",
                              column == "ADR_Competence"~ "ADR Competence",
                              column == "CRT_Competence"~ "CRT Competence",
                              column == "MDMT_Optimality"~ "MDMT Optimality", 
                              column == "EAT_Optimality"~ "EAT Optimality", 
                              column == "BNT_Optimality"~ "BNT Optimality",
                              column == "RAPM_Optimality"~ "RAPM Optimality",
                              column == "ADR_Optimality"~ "ADR Optimality",
                              column == "CRT_Optimality"~ "CRT Optimality",
                              column == "MDMT_Recklessness"~ "MDMT Recklessness", 
                              column == "EAT_Recklessness"~ "EAT Recklessness", 
                              column == "BNT_Recklessness"~ "BNT Recklessness", 
                              column == "RAPM_Recklessness"~ "RAPM Recklessness",
                              column == "ADR_Recklessness"~ "ADR Recklessness",
                              column == "CRT_Recklessness"~ "CRT Recklessness",
                              column == "MDMT_Hesitancy"~ "MDMT Hesitancy", 
                              column == "EAT_Hesitancy"~ "EAT Hesitancy", 
                              column == "BNT_Hesitancy"~ "BNT Hesitancy", 
                              column == "RAPM_Hesitancy"~ "RAPM Hesitancy", 
                              column == "ADR_Hesitancy"~ "ADR Hesitancy", 
                              column == "CRT_Hesitancy"~ "CRT Hesitancy",
                              column == "MDMT_Decisiveness"~ "MDMT Decisiveness",
                              column == "EAT_Decisiveness"~ "EAT Decisiveness",
                              column == "BNT_Decisiveness"~ "BNT Decisiveness",
                              column == "RAPM_Decisiveness"~ "RAPM Decisiveness",
                              column == "ADR_Decisiveness"~ "ADR Decisiveness",
                              column == "CRT_Decisiveness"~ "CRT Decisiveness",
                              column == "GDMS_Rational"~ "GDMS Rational", 
                              column == "GDMS_Intuitive"~ "GDMS Intuitive",
                              column == "GDMS_Dependent"~ "GDMS Dependent", 
                              column == "GDMS_Avoidant"~ "GDMS Avoidant", 
                              column == "GDMS_Spontaneous"~ "GDMS Spontaneous",
                              column == "BASDRIVE"~ "BAS Drive", 
                              column == "BASFS"~ "BAS Fun Seeking", 
                              column == "BASRR"~ "BAS Reward Responsiveness", 
                              column == "BIS"~ "BIS", 
                              column == "NFC_Total"~ "NFC Total",
                              column == "IPIP_Extraversion"~ "IPIP Extraversion", 
                              column == "IPIP_Agreeableness"~ "IPIP Agreeableness", 
                              column == "IPIP_Conscientiousness"~ "IPIP Conscientiousness", 
                              column == "IPIP_Neuroticism"~ "IPIP Neuroticism",
                              column == "IPIP_Intellect"~ "IPIP Intellect",
                              column == "EAT_ReactT"~ "EAT ResponseT", 
                              column == "BNT_ReactT"~ "BNT ResponseT", 
                              column == "CRT_ResponseRT"~ "CRT ResponseT", 
                              column == "CUBE_RT"~ "Cube Comparisons ResponseT", 
                              column == "ADR_ReactT"~ "ADR ResponseT", 
                              column == "FLANK_RT"~ "Flanker RT",
                              column == "RAPM_ReactT"~ "RAPM ResponseT", 
                              column == "RUNL_ReactT"~ "Running Letters ResponseT", 
                              column == "TASKSW_ReactT"~ "Switching RT", 
                              column == "SUD_ReactT"~ "SUD Task ResponseT", 
                              column == "RISK_ReactT"~ "Risky Gambles ResponseT", 
                              column == "RTFN_RT"~ "Resistance to Framing-Neg ResponseT", 
                              column == "RTFP_RT"~ "Resistance to Framing-Pos ResponseT",
                              column == "SN_RespT"~ "Recognising Social Norms ResponseT", 
                              column == "CRP_RespT"~ "Consistency in Risk Perception ResponseT", 
                              column == "MDMT_Resp_RT"~ "MDMT ResponseT", 
                              column == "DOI_TimeTotal"~ "DOI Total Time", 
                              column == "NFC_RespT"~ "NFC ResponseT", 
                              column == "VIS_RespT"~ "Visual Search ResponseT",
                              column == "DOI_Score"~ "DOI Score",
                              column == "UncwC"~ "Uncertainty Factor",
                              column == "UncwP"~ "Uncertainty Factor(P)",
                              column == "POST"~ "Control Threshold Factor",
                              column == "Conf"~ "Confidence Factor",
                              column == "Conf2"~ "Confidence Factor",
                              column == "bias"~ "Bias Factor",
                              column == "int"~"Intelligence",
                              column == "IntwDiff"~"Intelligence Factor",
                              column == "IntwRT"~"Intelligence Factor",
                              column == "EF_Diff"~"Executive Functions w Diff",
                              column == "EF_RT"~"Executive Functions w RT",
                              column == "Unc"~"Uncertainty Factor",
            TRUE ~ as.character(column))
  
}

whichvarsig <- function(data, column) {
  d <- data %>% 
    select(column, everything()) %>% 
    select_if(is.numeric) %>% 
    corstarssigcheck() %>% 
    select(column) %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    filter(str_detect(!!sym(column), "\\*")) %>% 
    select(rowname) %>% 
    unique()
  
  return(d$rowname)
}

simplenumber <- function(num) {
  num %>% 
    round(., 2) %>% format(nsmall = 2) %>% str_replace(., "0.", ".")
}
