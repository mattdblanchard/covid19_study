



{
med <- c("medic|doc|nurs|x-ray|nutse")
research <- c("research|scientist|science|phd|laboratory|intern\\ at\\ a\\ lab")
academic <- c("professor|lectur|mathematician|adacemic")
edu <- "tutor|teach|school|notetaker\\ at\\ sydney\\ uni"
hosp <- c("food|bar|wait|rest|cook")
write <- "writ"
it <- "web|technical|technician|it|programmer"
warehouse <- "ware|online\\ fulfilment|nightfill"
trans <- "transport|driver|valet|logistic"
retail <- "retail|supermark|storewoman|store\\ manager|shelfstacker|coles|sales\\ assis|sale|liquorland|online\\ shop|virtual\\ assistant"
arts <- "actor|artist|sound\\ designer|music|dj"
finance <- "finance|stock\\ trader|mortgage"
law <- "solicitor|law|judge|barirster|legal"
admin <- "office|admin|clerk"
none <- "none|not\\ working|unemploy|looking\\ for\\ work|no\\ job"
professional <- "marketing|bookkeeper|actuar|office|project\\ mana|project\\ coor"
home <- "house-maker|housewife|housekeeper|mom|mum|dad|homemaker|home\\ maker|home"
}

scores %>% 
  filter(Class == 1) %>% 
  select(Occupation) %>%  table()
  mutate(Occupation = tolower(Occupation),
         Occupation = ifelse(str_detect(Occupation, med), "medicine",
                      ifelse(str_detect(Occupation, research), "research",
                      ifelse(str_detect(Occupation, academic), "academic", 
                      ifelse(str_detect(Occupation, "engineer"), "engineer",
                      ifelse(str_detect(Occupation, law), "lawyer",
                      ifelse(str_detect(Occupation, finance), "finance",
                      ifelse(str_detect(Occupation, edu), "education", 
                      ifelse(str_detect(Occupation, hosp), "hospitality", 
                      ifelse(str_detect(Occupation, write), "writer", 
                      ifelse(str_detect(Occupation, warehouse), "warehouse", 
                      ifelse(str_detect(Occupation, trans), "transport", 
                      ifelse(str_detect(Occupation, retail), "retail", 
                      ifelse(str_detect(Occupation, arts), "arts", 
                      ifelse(str_detect(Occupation, "retire"), "retired", 
                      ifelse(str_detect(Occupation, none), "unemployed", 
                      ifelse(str_detect(Occupation, "librar"), "librarian",
                      ifelse(str_detect(Occupation, professional), "professional",
                      ifelse(str_detect(Occupation, home), "home_duties",
                      ifelse(str_detect(Occupation, it), "IT", Occupation))))))))))))))))))))


table(c$Occupation)











