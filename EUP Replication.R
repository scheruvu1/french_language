####Sivaram Cheruvu
####Emory University
####"How do Institutional Constraints Affect Judicial Decision-Making: The European
####Court of Justice's French Language Mandate" Replication file
####Email: Sivaram.Cheruvu@emory.edu
####Twitter: @SivCheruvu

#Clear Workspace
rm(list=ls(all=TRUE))

#Load Packages
library(MatchIt) #To conduct matching analysis
library(arm) #To conduct matching analysis
library(lubridate) #To format dates
library(ggplot2) #Plotting
library(sandwich) #Clustered Standard Errors
library(multiwayvcov) #Clustered Standard Errors
library(stargazer) #Regression Tables
library(tidyr) #plotting and data cleaning
library(dplyr) #data cleaning
library(directlabels) #adding labels to plots

#Clear Console
cat("\014")


#Set working directory
setwd("~/Google Drive/Publications/French Language")

#Read data
dat <- read.csv("EUP_replication.csv")

#Creating Matched Data Set
set.seed(12345) #Setting seed for replicability
matched.model <- matchit(JR_Francophone ~ judges, 
                         data = dat, method = "exact", distance = "logit") #Matched model equation
processed.dat <- match.data(matched.model) #Creating Dataset

##################Regressions####################

####Case Duration Models
#Note: Models take some time to run due to judges fixed-effects and clustering of standard errors by judges

#Unmatched Models
reg_caseduration_1 <- lm(case_duration ~ JR_Francophone + factor(judges) + factor(year_case), data = dat)
caseduration_se_1 <- sqrt(diag(vcovPL(reg_caseduration_1, cluster = dat$judges,  adjust = FALSE)))

reg_caseduration_2 <- lm(case_duration ~ JR_Francophone + factor(year_case) +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language, data = dat)
caseduration_se_2 <- sqrt(diag(vcovPL(reg_caseduration_2, cluster = dat$judges,  adjust = FALSE)))

reg_caseduration_3 <- lm(case_duration ~ JR_Francophone + factor(year_case)  +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language + JR_experience + JR_Any_Judge, data = dat)
caseduration_se_3 <- sqrt(diag(vcovPL(reg_caseduration_3, cluster = dat$judges,  adjust = FALSE)))

#Matched Models
matched_caseduration_1 <- lm(case_duration ~ JR_Francophone + factor(judges) + factor(year_case), data = processed.dat)
matched_caseduration_se_1 <- sqrt(diag(vcovPL(matched_caseduration_1, cluster = processed.dat$judges,  adjust = FALSE)))


matched_caseduration_2 <- lm(case_duration ~ JR_Francophone + factor(year_case) +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language, data = processed.dat)
matched_caseduration_se_2 <- sqrt(diag(vcovPL(matched_caseduration_2, cluster = processed.dat$judges,  adjust = FALSE)))


matched_caseduration_3 <- lm(case_duration ~ JR_Francophone + factor(year_case)  +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_caseduration_se_3 <- sqrt(diag(vcovPL(matched_caseduration_3, cluster = processed.dat$judges,  adjust = FALSE)))

#Creating Regression Table
#Note regression tables in the paper are slightly altered for aesthetic reasons
#Omitting filing language coefficients for cases with more than 1 filing language for presentation purposes
caseduration <- stargazer(reg_caseduration_1, matched_caseduration_1, reg_caseduration_2, matched_caseduration_2, reg_caseduration_3, matched_caseduration_3,
                          omit = c("year_case", "judges","languageDanish, Dutch, English, French, German",
                                   "languageDanish, English, French, Italian", "languageDanish, English, German",
                                   "languageDanish, French, German", "languageDanish, German", "languageDutch, English, French, German, Italian",
                                   "languageDutch, English, Italian", "languageDutch, French", "languageDutch, French, German",
                                   "languageDutch, French, German, Italian", "languageDutch, German", "languageDutch, Italian",
                                   "languageEnglish, French", "languageEnglish, French, German", "languageEnglish, French, Italian",
                                   "languageEnglish, German", "languageEnglish, Spanish", "languageEnglish, Swedish",
                                   "languageFinnish, Spanish", "languageFrench, German", "languageFrench, Italian, Spanish",
                                   "languageFrench, Spanish", "languageGerman, Italian", "languageGerman, Spanish",
                                   "languageDanish, English", "languageDutch, English", "languageItalian, Spanish"), 
                          add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                           c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                           c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                           c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                          se = list(caseduration_se_1,matched_caseduration_se_1,caseduration_se_2,matched_caseduration_se_2,caseduration_se_3,matched_caseduration_se_3),
                          star.cutoffs = c(0.05, 0.01, 0.001),
                          covariate.labels = c("\\textsc{Francophone JR}", "\\textsc{Article 256}", 
                                               "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                               "\\textsc{Article 265}", "\\textsc{Article 267}",
                                               "\\textsc{Article 268}", "\\textsc{Article 270}",
                                               "\\textsc{Filing Language Croatian}","\\textsc{Filing Language Czech}", 
                                               "\\textsc{Filing Language Danish}", "\\textsc{Filing Language Dutch}", 
                                               "\\textsc{Filing Language English}", "\\textsc{Filing Language Estonian}",
                                               "\\textsc{Filing Language Finnish}", "\\textsc{Filing Language French}",
                                               "\\textsc{Filing Language German}", "\\textsc{Filing Language Greek}", 
                                               "\\textsc{Filing Language Hungarian}", "\\textsc{Filing Language Italian}", 
                                               "\\textsc{Filing Language Latvian}", "\\textsc{Filing Language Lithuanian}",
                                               "\\textsc{Filing Language Maltese}", "\\textsc{Filing Language Polish}",
                                               "\\textsc{Filing Language Portuguese}", "\\textsc{Filing Language Romanian}", 
                                               "\\textsc{Filing Language Slovak}", "\\textsc{Filing Language Slovenian}",
                                               "\\textsc{Filing Language Spanish}", "\\textsc{Filing Language Swedish}",
                                               "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}",
                                               "\\textit{Constant}"),
                          notes = c("Standard errors clustered at the chamber-level are in 
                                    parentheses."),
                          notes.align = "l",  
                          keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(caseduration, sep = '\n', file = "caseduration_table.tex")

####Rate of Production Models

#Unmatched Models
reg_rate_1 <- lm(rate_of_production ~ JR_Francophone + factor(judges) + factor(year_case), data = dat)
rate_se_1 <- sqrt(diag(vcovPL(reg_rate_1, cluster = dat$judges, adjust = FALSE)))

reg_rate_2 <- lm(rate_of_production ~ JR_Francophone + factor(year_case) + factor(judges) + 
                   art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language, data = dat)
rate_se_2 <- sqrt(diag(vcovPL(reg_rate_2, cluster = dat$judges, adjust = FALSE)))

reg_rate_3 <- lm(rate_of_production ~ JR_Francophone + factor(year_case)  +
                   factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language + JR_experience + JR_Any_Judge, data = dat)
rate_se_3 <- sqrt(diag(vcovPL(reg_rate_3, cluster = dat$judges, adjust = FALSE)))

#Matched Models

matched_rate_1 <- lm(rate_of_production ~ JR_Francophone + factor(judges) + factor(year_case), data = processed.dat)
matched_rate_se_1 <- sqrt(diag(vcovPL(matched_rate_1, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_2 <- lm(rate_of_production ~ JR_Francophone + factor(year_case) + factor(judges) + 
                       art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language, data = processed.dat)
matched_rate_se_2 <- sqrt(diag(vcovPL(matched_rate_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_3 <- lm(rate_of_production ~ JR_Francophone + factor(year_case)  +
                       factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_rate_se_3 <- sqrt(diag(vcovPL(matched_rate_3, cluster = processed.dat$judges, adjust = FALSE)))

rate <- stargazer(reg_rate_1, matched_rate_1, reg_rate_2, matched_rate_2, reg_rate_3, matched_rate_3,
                  omit = c("year_case", "judges","languageDanish, Dutch, English, French, German",
                           "languageDanish, English, French, Italian", "languageDanish, English, German",
                           "languageDanish, French, German", "languageDanish, German", "languageDutch, English, French, German, Italian",
                           "languageDutch, English, Italian", "languageDutch, French", "languageDutch, French, German",
                           "languageDutch, French, German, Italian", "languageDutch, German", "languageDutch, Italian",
                           "languageEnglish, French", "languageEnglish, French, German", "languageEnglish, French, Italian",
                           "languageEnglish, German", "languageEnglish, Spanish", "languageEnglish, Swedish",
                           "languageFinnish, Spanish", "languageFrench, German", "languageFrench, Italian, Spanish",
                           "languageFrench, Spanish", "languageGerman, Italian", "languageGerman, Spanish",
                           "languageDanish, English", "languageDutch, English", "languageItalian, Spanish"), 
                  add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                   c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                   c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                   c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                  se = list(rate_se_1,matched_rate_se_1,rate_se_2,matched_rate_se_2,rate_se_3,matched_rate_se_3),
                  star.cutoffs = c(0.05, 0.01, 0.001),
                  covariate.labels = c("\\textsc{Francophone JR}", "\\textsc{Article 256}", 
                                       "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                       "\\textsc{Article 265}", "\\textsc{Article 267}",
                                       "\\textsc{Article 268}", "\\textsc{Article 270}",
                                       "\\textsc{Filing Language Croatian}","\\textsc{Filing Language Czech}", 
                                       "\\textsc{Filing Language Danish}", "\\textsc{Filing Language Dutch}", 
                                       "\\textsc{Filing Language English}", "\\textsc{Filing Language Estonian}",
                                       "\\textsc{Filing Language Finnish}", "\\textsc{Filing Language French}",
                                       "\\textsc{Filing Language German}", "\\textsc{Filing Language Greek}", 
                                       "\\textsc{Filing Language Hungarian}", "\\textsc{Filing Language Italian}", 
                                       "\\textsc{Filing Language Latvian}", "\\textsc{Filing Language Lithuanian}",
                                       "\\textsc{Filing Language Maltese}", "\\textsc{Filing Language Polish}",
                                       "\\textsc{Filing Language Portuguese}", "\\textsc{Filing Language Romanian}", 
                                       "\\textsc{Filing Language Slovak}", "\\textsc{Filing Language Slovenian}",
                                       "\\textsc{Filing Language Spanish}", "\\textsc{Filing Language Swedish}",
                                       "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}",
                                       "\\textit{Constant}"),
                  notes = c("Standard errors clustered at the chamber-level are in 
                            parentheses."),
                  notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(rate, sep = '\n', file = "rate_table.tex")

####Lexical Diversity Models

#Unmatched Models

reg_lexical_1 <- lm(Maas ~ JR_Francophone + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(year_case), data = dat) 
lexical_se_1 <- sqrt(diag(vcovPL(reg_lexical_1, cluster = dat$judges, adjust = FALSE)))


reg_lexical_2 <- lm(Maas ~ JR_Francophone + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(year_case) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = dat) 
lexical_se_2 <- sqrt(diag(vcovPL(reg_lexical_2, cluster = dat$judges, adjust = FALSE)))


reg_lexical_3 <- lm(Maas ~ JR_Francophone + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(year_case) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                      JR_experience + JR_Any_Judge, data = dat) 
lexical_se_3 <- sqrt(diag(vcovPL(reg_lexical_3, cluster = dat$judges, adjust = FALSE)))

#Matched Models

matched_lexical_1 <- lm(Maas ~ JR_Francophone + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(year_case), data = processed.dat) 
matched_lexical_se_1 <- sqrt(diag(vcovPL(matched_lexical_1, cluster = processed.dat$judges, adjust = FALSE)))

matched_lexical_2 <- lm(Maas ~ JR_Francophone + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(year_case) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = processed.dat) 
matched_lexical_se_2 <- sqrt(diag(vcovPL(matched_lexical_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_lexical_3 <- lm(Maas ~ JR_Francophone + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(year_case) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                          JR_experience + JR_Any_Judge, data = processed.dat) 

matched_lexical_se_3 <- sqrt(diag(vcovPL(matched_lexical_3, cluster = processed.dat$judges, adjust = FALSE)))

lexical <- stargazer(reg_lexical_1, matched_lexical_1, reg_lexical_2, matched_lexical_2, reg_lexical_3, matched_lexical_3,
                     omit = c("year_case", "judges","languageDanish, Dutch, English, French, German",
                              "languageDanish, English, French, Italian", "languageDanish, English, German",
                              "languageDanish, French, German", "languageDanish, German", "languageDutch, English, French, German, Italian",
                              "languageDutch, English, Italian", "languageDutch, French", "languageDutch, French, German",
                              "languageDutch, French, German, Italian", "languageDutch, German", "languageDutch, Italian",
                              "languageEnglish, French", "languageEnglish, French, German", "languageEnglish, French, Italian",
                              "languageEnglish, German", "languageEnglish, Spanish", "languageEnglish, Swedish",
                              "languageFinnish, Spanish", "languageFrench, German", "languageFrench, Italian, Spanish",
                              "languageFrench, Spanish", "languageGerman, Italian", "languageGerman, Spanish",
                              "languageDanish, English", "languageDutch, English", "languageItalian, Spanish",
                              "JR_Country", "advocate_general"), 
                     add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                      c("Member State Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                      c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                      c("Advocate-General Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                      c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                      c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                     se = list(lexical_se_1,matched_lexical_se_1,lexical_se_2,matched_lexical_se_2,lexical_se_3,matched_lexical_se_3),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     covariate.labels = c("\\textsc{Francophone JR}", "\\textsc{Article 256}", 
                                          "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                          "\\textsc{Article 265}", "\\textsc{Article 267}",
                                          "\\textsc{Article 268}", "\\textsc{Article 270}",
                                          "\\textsc{Filing Language Croatian}","\\textsc{Filing Language Czech}", 
                                          "\\textsc{Filing Language Danish}", "\\textsc{Filing Language Dutch}", 
                                          "\\textsc{Filing Language English}", "\\textsc{Filing Language Estonian}",
                                          "\\textsc{Filing Language Finnish}", "\\textsc{Filing Language French}",
                                          "\\textsc{Filing Language German}", "\\textsc{Filing Language Greek}", 
                                          "\\textsc{Filing Language Hungarian}", "\\textsc{Filing Language Italian}", 
                                          "\\textsc{Filing Language Latvian}", "\\textsc{Filing Language Lithuanian}",
                                          "\\textsc{Filing Language Maltese}", "\\textsc{Filing Language Polish}",
                                          "\\textsc{Filing Language Portuguese}", "\\textsc{Filing Language Romanian}", 
                                          "\\textsc{Filing Language Slovak}", "\\textsc{Filing Language Slovenian}",
                                          "\\textsc{Filing Language Spanish}", "\\textsc{Filing Language Swedish}",
                                          "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}",
                                          "\\textit{Constant}"),
                     notes = c("Standard errors clustered clustered at the chamber-level are in parentheses."),
                     notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(lexical, sep = '\n', file = "lexical_table.tex")

##########################PLOTS AND FIGURES###########################

#Figure 1
num_cases <- qplot(dat$year_judgment, geom="histogram", bins = 25,
      main = "Number of Cases Per Year", xlab = "Year", ylab = "Number of Cases", col = I("black")) + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), text = element_text(size=18)) +
  scale_x_continuous(breaks = seq(1954,2014,10), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) 

#Figure 2
dat_plot <- dat
dat_plot$JR_Country <- as.character(dat_plot$JR_Country)
dat_plot$JR_Country <- ifelse(dat_plot$JR_Belgium_French == 1, "Belgium-French", dat_plot$JR_Country)
dat_plot$JR_Country <- ifelse(dat_plot$JR_Country == "Belgium", "Belgium-Flemish", dat_plot$JR_Country)
dat_plot <- select(dat_plot, case_duration, year_case, JR_Country) %>% filter(case_duration < 1000) %>% filter(JR_Country!="None")
dat_plot <- dat_plot %>% group_by(JR_Country, year_case)  %>%
  summarise("Average_Case_Duration" = mean(case_duration))
dat_plot <- dat_plot %>% ungroup(JR_Country,year_case) %>% complete(JR_Country,year_case,fill = list(Average_Case_Duration = NA))
p <- ggplot(dat_plot, aes(x=year_case, y = Average_Case_Duration)) + geom_line() + 
  ylab("Average Case Duration (Days)") + xlab("Year") + ggtitle("Average Case Duration per year by Country Judge-Rapporteur")
p <- p + facet_wrap(~JR_Country) + theme_bw() + theme(plot.title = element_text(hjust = 0.5),panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = seq(1960,2010,10), limits = c(1960,2015))

#Figure 3
judges <- dplyr::select(dat, CELEX_number, date_case, date_judgment, judge_rapporteur,judges,JR_Francophone,JR_Country,
                 case_duration,art_256,art_258,art_263,art_265,art_267,art_268,art_270) %>% arrange(judges) %>% filter(art_267 == 1)

effect_plot_1 <- dplyr::select(dat, CELEX_number, date_case, date_judgment, judge_rapporteur,judges,JR_Francophone,JR_Country,
                        case_duration,art_256,art_258,art_263,art_265,art_267,art_268,art_270) %>% arrange(judges) %>% 
  filter(judges == "Grévisse, Moitinho de Almeida, Zuleeg")
effect_plot_1$article <- ifelse(effect_plot_1$art_256 == 1, "Article 256", 
                                ifelse(effect_plot_1$art_258 == 1, "Article 258",
                                       ifelse(effect_plot_1$art_263 == 1,"Article 263",
                                              ifelse(effect_plot_1$art_267 == 1,"Article 267",
                                                     ifelse(effect_plot_1$art_268 == 1,"Article 268",
                                                            ifelse(effect_plot_1$art_270 == 1, "Article 270",0)))))) 
effect_plot_1 <- effect_plot_1 %>% filter(article =="Article 267") %>% dplyr::select(CELEX_number, chamber, date_case, date_judgment, judge_rapporteur,judges,JR_Francophone,JR_Country,
                                                                              case_duration, article)
effect_plot_1 <- effect_plot_1 %>% arrange(judge_rapporteur,JR_Country) 
effect_plot_1$judge_rapporteur <- ifelse(effect_plot_1$judge_rapporteur == "Grévisse", "Grévisse (France)",
                                         ifelse(effect_plot_1$judge_rapporteur == "Zuleeg", "Zuleeg (Germany)",
                                                ifelse(effect_plot_1$judge_rapporteur == "Moitinho de Almeida", "Moitinho de Almeida (Portugal)",effect_plot_1$judge_rapporteur))) 
effect_plot_1_mean <- mean(effect_plot_1$case_duration)
effect_plot_1 <- effect_plot_1 %>% group_by(judge_rapporteur) %>%
  summarise(mean = mean(case_duration, na.rm = TRUE),
            sd = sd(case_duration, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 
effect_plot_1$JR_Francophone = ifelse(effect_plot_1$judge_rapporteur == "Grévisse (France)","Francophone","Non-Francophone")
effect_plot_1 <- effect_plot_1 %>% arrange(mean)
effect_plot_1$judge_rapporteur <- factor(effect_plot_1$judge_rapporteur, levels = effect_plot_1$judge_rapporteur)
effect_plot_1$chamber <- "Chamber 1 (1987 - 1994)"

effect_plot_2 <- dplyr::select(dat, CELEX_number, date_case, date_judgment, judge_rapporteur,judges,JR_Francophone,JR_Country,
                        case_duration,art_256,art_258,art_263,art_265,art_267,art_268,art_270) %>% arrange(judges) %>% 
  filter(judges == "Mancini, Murray, Schockweiler")
effect_plot_2$article <- ifelse(effect_plot_2$art_256 == 1, "Article 256", 
                                ifelse(effect_plot_2$art_258 == 1, "Article 258",
                                       ifelse(effect_plot_2$art_263 == 1,"Article 263",
                                              ifelse(effect_plot_2$art_267 == 1,"Article 267",
                                                     ifelse(effect_plot_2$art_268 == 1,"Article 268",
                                                            ifelse(effect_plot_2$art_270 == 1, "Article 270",0)))))) 
effect_plot_2 <- effect_plot_2 %>% filter(article =="Article 267") %>% dplyr::select(CELEX_number, chamber, date_case, date_judgment, judge_rapporteur,judges,JR_Francophone,JR_Country,
                                                                              case_duration, article)
effect_plot_2 <- effect_plot_2 %>% arrange(judge_rapporteur,JR_Country) 
effect_plot_2$judge_rapporteur <- ifelse(effect_plot_2$judge_rapporteur == "Mancini", "Mancini (Italy)",
                                         ifelse(effect_plot_2$judge_rapporteur == "Murray", "Murray (Ireland)",
                                                ifelse(effect_plot_2$judge_rapporteur == "Schockweiler", "Schockweiler (Luxembourg)",effect_plot_2$judge_rapporteur))) 
effect_plot_2_mean <- mean(effect_plot_2$case_duration)
effect_plot_2 <- effect_plot_2 %>% group_by(judge_rapporteur) %>%
  summarise(mean = mean(case_duration, na.rm = TRUE),
            sd = sd(case_duration, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 
effect_plot_2$JR_Francophone = ifelse(effect_plot_2$judge_rapporteur == "Schockweiler (Luxembourg)","Francophone","Non-Francophone")
effect_plot_2 <- effect_plot_2 %>% arrange(mean)
effect_plot_2$judge_rapporteur <- factor(effect_plot_2$judge_rapporteur, levels = effect_plot_2$judge_rapporteur)
effect_plot_2$chamber <- "Chamber 2 (1990 - 1995)"

facet_means <- data.frame(chamber = c("Chamber 1 (1987 - 1994)","Chamber 2 (1990 - 1995)"), Z = c(effect_plot_1_mean,effect_plot_2_mean))
effect_plot <- rbind(effect_plot_1,effect_plot_2)
effect_plot$chamber <- factor(effect_plot$chamber, levels = c("Chamber 1 (1987 - 1994)","Chamber 2 (1990 - 1995)"))
effect_plot$JR_Francophone <- factor(effect_plot$JR_Francophone)

p1 <- ggplot(effect_plot, aes(x=judge_rapporteur, y=mean, group = judge_rapporteur, colour = JR_Francophone)) +
  geom_linerange(aes(y = mean, ymin = lower.ci,ymax = upper.ci),lwd = 1) + 
  geom_pointrange(aes(y = mean, ymin = lower.ci,ymax = upper.ci), lwd = 1/2, shape = 21, fill = "WHITE") + 
  theme_bw() + ggtitle("Comparing Francophone and Non-Francophone Judge-Rapporteurs on Article 267 Cases within Chamber") + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + xlab("Judge Rapporteur")  +
  ylab("Average Case Duration (days)") +  geom_hline(data = facet_means, aes(yintercept = Z),colour = gray(1/2), lty = 2) +
  facet_wrap(~chamber, scales = "free_x", drop = TRUE, ncol = 2) + 
  theme(legend.key = element_rect(colour = "transparent", fill = "transparent")) + scale_color_discrete(name="") +
  theme(legend.position="none", text = element_text(size=11))
p1

#Figure 4
cases_per_year <- dat %>% group_by(year_case,JR_Francophone) %>% 
  summarize("Number_of_Cases" = length(CELEX_number)) %>% mutate(percentage = Number_of_Cases / sum(Number_of_Cases))
cases_per_year$JR_Francophone <- ifelse(cases_per_year$JR_Francophone == 1, "Francophone", "Non-Francophone")
cases_per_year$JR_Francophone <- as.factor(cases_per_year$JR_Francophone)
p <- ggplot(cases_per_year, aes(x = year_case, y= percentage, linetype = JR_Francophone)) + geom_line() +
  theme_classic() + xlab("Year") + ylab("Percent of Cases") + xlim(1964,2014) + 
  scale_linetype_discrete(guide = 'none') + scale_y_continuous(labels = scales::percent) +
  geom_dl(aes(label = JR_Francophone), method = list(dl.trans(x= x-2.7,y = y + 1), "last.points", cex = 1)) +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), text = element_text(size=18)) + ggtitle("Judge-Rapporteur Cases Per Year")
p 

#Figure 5
legal_procedure_plot <- dat
legal_procedure_plot$article <- ifelse(legal_procedure_plot$art_256 == 1, "Article 256", 
                                       ifelse(legal_procedure_plot$art_258 == 1, "Article 258",
                                              ifelse(legal_procedure_plot$art_263 == 1,"Article 263",
                                                     ifelse(legal_procedure_plot$art_267 == 1,"Article 265",
                                                            ifelse(legal_procedure_plot$art_267 == 1,"Article 267",
                                                                   ifelse(legal_procedure_plot$art_268 == 1,"Article 268",
                                                                          ifelse(legal_procedure_plot$art_270 == 1, "Article 270",0))))))) 
legal_procedure_plot <- legal_procedure_plot %>% filter(article %in% c("Article 256","Article 258","Article 265"))
legal_procedure_plot <- legal_procedure_plot %>% group_by(year_case, article, JR_Francophone) %>% 
  summarize("Number_of_Cases" = length(CELEX_number)) %>% mutate(percentage = Number_of_Cases / sum(Number_of_Cases))
legal_procedure_plot$JR_Francophone <- ifelse(legal_procedure_plot$JR_Francophone == 1, "Francophone", "Non-Francophone")
legal_procedure_plot$JR_Francophone <- as.factor(legal_procedure_plot$JR_Francophone)
p <- ggplot(legal_procedure_plot, aes(x = year_case, y= percentage,linetype = JR_Francophone)) + 
  geom_line() + theme_bw() + xlab("Year") + ylab("Percent of Cases") + 
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position=c(.1, .1)) + scale_linetype_discrete(name="") + xlim(c(1970,2016)) +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), text = element_text(size=18)) + 
  ggtitle("Judge-Rapporteur Cases Per Year by Legal Procedure") + 
  facet_wrap(~article, ncol = 3) + guides(linetype = guide_legend(reverse=T)) 
p

#Figure 6
a <- as.data.frame(processed.dat$case_duration)
a$variable <- "Case Duration (Days)"
colnames(a)[colnames(a) == "processed.dat$case_duration"] <- "Value"
b <- as.data.frame(processed.dat$word_count)
b$variable <- "Word Count"
colnames(b)[colnames(b) == "processed.dat$word_count"] <- "Value"
c <- as.data.frame(processed.dat$Maas)
c$variable <- "Lexical Diversity (Maas)"
colnames(c)[colnames(c) == "processed.dat$Maas"] <- "Value"
combhist <- rbind(a,b,c)
rm(a,b,c)
p <- ggplot(combhist,aes(x=Value)) + geom_histogram(colour="black") + 
  facet_wrap(~variable, scales = "free_x") +
  theme_bw() + ylab("Number of Cases") + xlab("")  +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank(), text = element_text(size=18)) + 
  ggtitle("Distribution of Case Duration, Lexical Diversity and Word Count in Matched Dataset")
p


##########################SUPPLEMENTARY INFORMATION AND ROBUSTNESS CHECKS###########################

############Log case duration##########

#Case Duration
reg_caseduration_log_1 <- lm(log(case_duration) ~ JR_Francophone + factor(judges) + factor(year_case), data = dat)

reg_caseduration_log_2 <- lm(log(case_duration) ~ JR_Francophone + factor(year_case) +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language, data = dat)

reg_caseduration_log_3 <- lm(log(case_duration) ~ JR_Francophone + factor(year_case)  +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language + JR_experience + JR_Any_Judge, data = dat)

matched_caseduration_log_1 <- lm(log(case_duration) ~ JR_Francophone + factor(judges) + factor(year_case), data = processed.dat)

matched_caseduration_log_2 <- lm(log(case_duration) ~ JR_Francophone + factor(year_case) +
                                   factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                                   language, data = processed.dat)

matched_caseduration_log_3 <- lm(log(case_duration) ~ JR_Francophone + factor(year_case)  +
                                   factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                                   language + JR_experience + JR_Any_Judge, data = processed.dat)


caseduration_log <- stargazer(reg_caseduration_log_1, matched_caseduration_log_1, reg_caseduration_log_2, matched_caseduration_log_2, reg_caseduration_log_3, matched_caseduration_log_3,
                              omit = c("year_case", "judges","languageDanish, Dutch, English, French, German",
                                       "languageDanish, English, French, Italian", "languageDanish, English, German",
                                       "languageDanish, French, German", "languageDanish, German", "languageDutch, English, French, German, Italian",
                                       "languageDutch, English, Italian", "languageDutch, French", "languageDutch, French, German",
                                       "languageDutch, French, German, Italian", "languageDutch, German", "languageDutch, Italian",
                                       "languageEnglish, French", "languageEnglish, French, German", "languageEnglish, French, Italian",
                                       "languageEnglish, German", "languageEnglish, Spanish", "languageEnglish, Swedish",
                                       "languageFinnish, Spanish", "languageFrench, German", "languageFrench, Italian, Spanish",
                                       "languageFrench, Spanish", "languageGerman, Italian", "languageGerman, Spanish",
                                       "languageDanish, English", "languageDutch, English", "languageItalian, Spanish"), 
                              add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                               c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                               c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                               c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                              star.cutoffs = c(0.05, 0.01, 0.001),
                              covariate.labels = c("\\textsc{Francophone JR}", "\\textsc{Article 256}", 
                                                   "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                                   "\\textsc{Article 265}", "\\textsc{Article 267}",
                                                   "\\textsc{Article 268}", "\\textsc{Article 270}",
                                                   "\\textsc{Filing Language Croatian}","\\textsc{Filing Language Czech}", 
                                                   "\\textsc{Filing Language Danish}", "\\textsc{Filing Language Dutch}", 
                                                   "\\textsc{Filing Language English}", "\\textsc{Filing Language Estonian}",
                                                   "\\textsc{Filing Language Finnish}", "\\textsc{Filing Language French}",
                                                   "\\textsc{Filing Language German}", "\\textsc{Filing Language Greek}", 
                                                   "\\textsc{Filing Language Hungarian}", "\\textsc{Filing Language Italian}", 
                                                   "\\textsc{Filing Language Latvian}", "\\textsc{Filing Language Lithuanian}",
                                                   "\\textsc{Filing Language Maltese}", "\\textsc{Filing Language Polish}",
                                                   "\\textsc{Filing Language Portuguese}", "\\textsc{Filing Language Romanian}", 
                                                   "\\textsc{Filing Language Slovak}", "\\textsc{Filing Language Slovenian}",
                                                   "\\textsc{Filing Language Spanish}", "\\textsc{Filing Language Swedish}",
                                                   "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}",
                                                   "\\textit{Constant}"),
                              notes = c("Standard errors clustered clustered at the chamber-level are in parentheses."),
                              notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(caseduration_log, sep = '\n', file = "caseduration_log_table.tex")

#######Robustness Check including only Judges from France######

#Matching

set.seed(12345)

#Creating Data Set

matched.model <- matchit(JR_France ~ judges, 
                         data = dat, method = "exact", distance = "logit")
processed.dat <- match.data(matched.model)
summary(matched.model)


#Case Duration
reg_caseduration_1 <- lm(case_duration ~ JR_France + factor(judges) + factor(year_case), data = dat)
caseduration_se_1 <- sqrt(diag(vcovPL(reg_caseduration_1, cluster = dat$judges,  adjust = FALSE)))

reg_caseduration_2 <- lm(case_duration ~ JR_France + factor(year_case) +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language, data = dat)
caseduration_se_2 <- sqrt(diag(vcovPL(reg_caseduration_2, cluster = dat$judges,  adjust = FALSE)))

reg_caseduration_3 <- lm(case_duration ~ JR_France + factor(year_case)  +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language + JR_experience + JR_Any_Judge, data = dat)
caseduration_se_3 <- sqrt(diag(vcovPL(reg_caseduration_3, cluster = dat$judges,  adjust = FALSE)))

matched_caseduration_1 <- lm(case_duration ~ JR_France + factor(judges) + factor(year_case), data = processed.dat)
matched_caseduration_se_1 <- sqrt(diag(vcovPL(matched_caseduration_1, cluster = processed.dat$judges,  adjust = FALSE)))

matched_caseduration_2 <- lm(case_duration ~ JR_France + factor(year_case) +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language, data = processed.dat)
matched_caseduration_se_2 <- sqrt(diag(vcovPL(matched_caseduration_2, cluster = processed.dat$judges,  adjust = FALSE)))

matched_caseduration_3 <- lm(case_duration ~ JR_France + factor(year_case)  +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_caseduration_se_3 <- sqrt(diag(vcovPL(matched_caseduration_3, cluster = processed.dat$judges,  adjust = FALSE)))

caseduration_France <- stargazer(reg_caseduration_1, matched_caseduration_1, reg_caseduration_2, matched_caseduration_2, reg_caseduration_3, matched_caseduration_3,
                                 omit = c("year_case", "judges","languageDanish, Dutch, English, French, German",
                                          "languageDanish, English, French, Italian", "languageDanish, English, German",
                                          "languageDanish, French, German", "languageDanish, German", "languageDutch, English, French, German, Italian",
                                          "languageDutch, English, Italian", "languageDutch, French", "languageDutch, French, German",
                                          "languageDutch, French, German, Italian", "languageDutch, German", "languageDutch, Italian",
                                          "languageEnglish, French", "languageEnglish, French, German", "languageEnglish, French, Italian",
                                          "languageEnglish, German", "languageEnglish, Spanish", "languageEnglish, Swedish",
                                          "languageFinnish, Spanish", "languageFrench, German", "languageFrench, Italian, Spanish",
                                          "languageFrench, Spanish", "languageGerman, Italian", "languageGerman, Spanish",
                                          "languageDanish, English", "languageDutch, English", "languageItalian, Spanish"), 
                                 add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                  c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                  c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                                  c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                                 se = list(caseduration_se_1,matched_caseduration_se_1,caseduration_se_2,matched_caseduration_se_2,caseduration_se_3,matched_caseduration_se_3),
                                 star.cutoffs = c(0.05, 0.01, 0.001),
                                 covariate.labels = c("\\textsc{France JR}", "\\textsc{Article 256}", 
                                                      "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                                      "\\textsc{Article 265}", "\\textsc{Article 267}",
                                                      "\\textsc{Article 268}", "\\textsc{Article 270}",
                                                      "\\textsc{Filing Language Croatian}","\\textsc{Filing Language Czech}", 
                                                      "\\textsc{Filing Language Danish}", "\\textsc{Filing Language Dutch}", 
                                                      "\\textsc{Filing Language English}", "\\textsc{Filing Language Estonian}",
                                                      "\\textsc{Filing Language Finnish}", "\\textsc{Filing Language French}",
                                                      "\\textsc{Filing Language German}", "\\textsc{Filing Language Greek}", 
                                                      "\\textsc{Filing Language Hungarian}", "\\textsc{Filing Language Italian}", 
                                                      "\\textsc{Filing Language Latvian}", "\\textsc{Filing Language Lithuanian}",
                                                      "\\textsc{Filing Language Maltese}", "\\textsc{Filing Language Polish}",
                                                      "\\textsc{Filing Language Portuguese}", "\\textsc{Filing Language Romanian}", 
                                                      "\\textsc{Filing Language Slovak}", "\\textsc{Filing Language Slovenian}",
                                                      "\\textsc{Filing Language Spanish}", "\\textsc{Filing Language Swedish}",
                                                      "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}",
                                                      "\\textit{Constant}"),
                                 notes = c("Standard errors clustered clustered at the chamber-level are in parentheses."),
                                 notes.align = "l",  
                                 keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(caseduration_France, sep = '\n', file = "caseduration_France_table.tex")

#Rate of Production
reg_rate_1 <- lm(rate_of_production ~ JR_France + factor(judges) + factor(year_case), data = dat)
rate_se_1 <- sqrt(diag(vcovPL(reg_rate_1, cluster = dat$judges, adjust = FALSE)))

reg_rate_2 <- lm(rate_of_production ~ JR_France + factor(year_case) + factor(judges) + 
                   art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language, data = dat)
rate_se_2 <- sqrt(diag(vcovPL(reg_rate_2, cluster = dat$judges, adjust = FALSE)))

reg_rate_3 <- lm(rate_of_production ~ JR_France + factor(year_case)  +
                   factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language + JR_experience + JR_Any_Judge, data = dat)
rate_se_3 <- sqrt(diag(vcovPL(reg_rate_3, cluster = dat$judges, adjust = FALSE)))

matched_rate_1 <- lm(rate_of_production ~ JR_France + factor(judges) + factor(year_case), data = processed.dat)
matched_rate_se_1 <- sqrt(diag(vcovPL(matched_rate_1, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_2 <- lm(rate_of_production ~ JR_France + factor(year_case) + factor(judges) + 
                       art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language, data = processed.dat)
matched_rate_se_2 <- sqrt(diag(vcovPL(matched_rate_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_3 <- lm(rate_of_production ~ JR_France + factor(year_case)  +
                       factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_rate_se_3 <- sqrt(diag(vcovPL(matched_rate_3, cluster = processed.dat$judges, adjust = FALSE)))

rate_France <- stargazer(reg_rate_1, matched_rate_1, reg_rate_2, matched_rate_2, reg_rate_3, matched_rate_3,
                         omit = c("year_case", "judges","languageDanish, Dutch, English, French, German",
                                  "languageDanish, English, French, Italian", "languageDanish, English, German",
                                  "languageDanish, French, German", "languageDanish, German", "languageDutch, English, French, German, Italian",
                                  "languageDutch, English, Italian", "languageDutch, French", "languageDutch, French, German",
                                  "languageDutch, French, German, Italian", "languageDutch, German", "languageDutch, Italian",
                                  "languageEnglish, French", "languageEnglish, French, German", "languageEnglish, French, Italian",
                                  "languageEnglish, German", "languageEnglish, Spanish", "languageEnglish, Swedish",
                                  "languageFinnish, Spanish", "languageFrench, German", "languageFrench, Italian, Spanish",
                                  "languageFrench, Spanish", "languageGerman, Italian", "languageGerman, Spanish",
                                  "languageDanish, English", "languageDutch, English", "languageItalian, Spanish"), 
                         add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                          c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                          c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                          c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                         se = list(rate_se_1,matched_rate_se_1,rate_se_2,matched_rate_se_2,rate_se_3,matched_rate_se_3),
                         star.cutoffs = c(0.05, 0.01, 0.001),
                         covariate.labels = c("\\textsc{France JR}", "\\textsc{Article 256}", 
                                              "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                              "\\textsc{Article 265}", "\\textsc{Article 267}",
                                              "\\textsc{Article 268}", "\\textsc{Article 270}",
                                              "\\textsc{Filing Language Croatian}","\\textsc{Filing Language Czech}", 
                                              "\\textsc{Filing Language Danish}", "\\textsc{Filing Language Dutch}", 
                                              "\\textsc{Filing Language English}", "\\textsc{Filing Language Estonian}",
                                              "\\textsc{Filing Language Finnish}", "\\textsc{Filing Language French}",
                                              "\\textsc{Filing Language German}", "\\textsc{Filing Language Greek}", 
                                              "\\textsc{Filing Language Hungarian}", "\\textsc{Filing Language Italian}", 
                                              "\\textsc{Filing Language Latvian}", "\\textsc{Filing Language Lithuanian}",
                                              "\\textsc{Filing Language Maltese}", "\\textsc{Filing Language Polish}",
                                              "\\textsc{Filing Language Portuguese}", "\\textsc{Filing Language Romanian}", 
                                              "\\textsc{Filing Language Slovak}", "\\textsc{Filing Language Slovenian}",
                                              "\\textsc{Filing Language Spanish}", "\\textsc{Filing Language Swedish}",
                                              "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}",
                                              "\\textit{Constant}"),
                         notes = c("Standard errors clustered clustered at the chamber-level are in parentheses."),
                         notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(rate_France, sep = '\n', file = "rate_France_table.tex")

#Lexical Diversity
reg_lexical_1 <- lm(Maas ~ JR_France + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(date_case), data = dat) 
lexical_se_1 <- sqrt(diag(vcovPL(reg_lexical_1, cluster = dat$judges, adjust = FALSE)))

reg_lexical_2 <- lm(Maas ~ JR_France + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(date_case) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = dat) 
lexical_se_2 <- sqrt(diag(vcovPL(reg_lexical_2, cluster = dat$judges, adjust = FALSE)))


reg_lexical_3 <- lm(Maas ~ JR_France + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(date_case) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                      JR_experience + JR_Any_Judge, data = dat) 
lexical_se_3 <- sqrt(diag(vcovPL(reg_lexical_3, cluster = dat$judges, adjust = FALSE)))

matched_lexical_1 <- lm(Maas ~ JR_France + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(date_case), data = processed.dat) 
matched_lexical_se_1 <- sqrt(diag(vcovPL(matched_lexical_1, cluster = processed.dat$judges, adjust = FALSE)))


matched_lexical_2 <- lm(Maas ~ JR_France + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(date_case) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = processed.dat) 
matched_lexical_se_2 <- sqrt(diag(vcovPL(matched_lexical_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_lexical_3 <- lm(Maas ~ JR_France + factor(advocate_general) + factor(judges) + factor(JR_Country) + factor(date_case) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                          JR_experience + JR_Any_Judge, data = processed.dat) 
matched_lexical_se_3 <- sqrt(diag(vcovPL(matched_lexical_3, cluster = processed.dat$judges, adjust = FALSE)))

lexical_France <- stargazer(reg_lexical_1, matched_lexical_1, reg_lexical_2, matched_lexical_2, reg_lexical_3, matched_lexical_3,
                            omit = c("year_case", "judges","languageDanish, Dutch, English, French, German",
                                     "languageDanish, English, French, Italian", "languageDanish, English, German",
                                     "languageDanish, French, German", "languageDanish, German", "languageDutch, English, French, German, Italian",
                                     "languageDutch, English, Italian", "languageDutch, French", "languageDutch, French, German",
                                     "languageDutch, French, German, Italian", "languageDutch, German", "languageDutch, Italian",
                                     "languageEnglish, French", "languageEnglish, French, German", "languageEnglish, French, Italian",
                                     "languageEnglish, German", "languageEnglish, Spanish", "languageEnglish, Swedish",
                                     "languageFinnish, Spanish", "languageFrench, German", "languageFrench, Italian, Spanish",
                                     "languageFrench, Spanish", "languageGerman, Italian", "languageGerman, Spanish",
                                     "languageDanish, English", "languageDutch, English", "languageItalian, Spanish",
                                     "JR_Country", "advocate_general"), 
                            add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                             c("Member State Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                             c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                             c("Advocate-General Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                             c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                             c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                            se = list(lexical_se_1,matched_lexical_se_1,lexical_se_2,matched_lexical_se_2,lexical_se_3,matched_lexical_se_3),
                            star.cutoffs = c(0.05, 0.01, 0.001),
                            covariate.labels = c("\\textsc{France JR}", "\\textsc{Article 256}", 
                                                 "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                                 "\\textsc{Article 265}", "\\textsc{Article 267}",
                                                 "\\textsc{Article 268}", "\\textsc{Article 270}",
                                                 "\\textsc{Filing Language Croatian}","\\textsc{Filing Language Czech}", 
                                                 "\\textsc{Filing Language Danish}", "\\textsc{Filing Language Dutch}", 
                                                 "\\textsc{Filing Language English}", "\\textsc{Filing Language Estonian}",
                                                 "\\textsc{Filing Language Finnish}", "\\textsc{Filing Language French}",
                                                 "\\textsc{Filing Language German}", "\\textsc{Filing Language Greek}", 
                                                 "\\textsc{Filing Language Hungarian}", "\\textsc{Filing Language Italian}", 
                                                 "\\textsc{Filing Language Latvian}", "\\textsc{Filing Language Lithuanian}",
                                                 "\\textsc{Filing Language Maltese}", "\\textsc{Filing Language Polish}",
                                                 "\\textsc{Filing Language Portuguese}", "\\textsc{Filing Language Romanian}", 
                                                 "\\textsc{Filing Language Slovak}", "\\textsc{Filing Language Slovenian}",
                                                 "\\textsc{Filing Language Spanish}", "\\textsc{Filing Language Swedish}",
                                                 "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}",
                                                 "\\textit{Constant}"),
                            notes = c("Standard errors clustered clustered at the chamber-level are in parentheses."),
                            notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(lexical_France, sep = '\n', file = "lexical_France_table.tex")

########Robustness Check including only Judges from Luxembourg######

#Matching

set.seed(12345)

#Creating Data Set

matched.model <- matchit(JR_Lux ~ judges, 
                         data = dat, method = "exact", distance = "logit")
processed.dat <- match.data(matched.model)
summary(matched.model)


#Case Duration
reg_caseduration_1 <- lm(case_duration ~ JR_Lux + factor(judges) + factor(year_case), data = dat)
caseduration_se_1 <- sqrt(diag(vcovPL(reg_caseduration_1, cluster = dat$judges,  adjust = FALSE)))

reg_caseduration_2 <- lm(case_duration ~ JR_Lux + factor(year_case) +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language, data = dat)
caseduration_se_2 <- sqrt(diag(vcovPL(reg_caseduration_2, cluster = dat$judges,  adjust = FALSE)))

reg_caseduration_3 <- lm(case_duration ~ JR_Lux + factor(year_case)  +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language + JR_experience + JR_Any_Judge, data = dat)
caseduration_se_3 <- sqrt(diag(vcovPL(reg_caseduration_3, cluster = dat$judges,  adjust = FALSE)))

matched_caseduration_1 <- lm(case_duration ~ JR_Lux + factor(judges) + factor(year_case), data = processed.dat)
matched_caseduration_se_1 <- sqrt(diag(vcovPL(matched_caseduration_1, cluster = processed.dat$judges,  adjust = FALSE)))


matched_caseduration_2 <- lm(case_duration ~ JR_Lux + factor(year_case) +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language, data = processed.dat)
matched_caseduration_se_2 <- sqrt(diag(vcovPL(matched_caseduration_2, cluster = processed.dat$judges,  adjust = FALSE)))


matched_caseduration_3 <- lm(case_duration ~ JR_Lux + factor(year_case)  +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_caseduration_se_3 <- sqrt(diag(vcovPL(matched_caseduration_3, cluster = processed.dat$judges,  adjust = FALSE)))

caseduration_Lux <- stargazer(reg_caseduration_1, matched_caseduration_1, reg_caseduration_2, matched_caseduration_2, reg_caseduration_3, matched_caseduration_3,
                              omit = c("year_case", "judges", "language", "Constant"), 
                              add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                               c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                               c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                               c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                              se = list(caseduration_se_1,matched_caseduration_se_1,caseduration_se_2,matched_caseduration_se_2,caseduration_se_3,matched_caseduration_se_3),
                              star.cutoffs = c(0.05, 0.01, 0.001),
                              covariate.labels = c("\\textsc{Luxembourg JR}", "\\textsc{Article 256}", 
                                                   "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                                   "\\textsc{Article 265}", "\\textsc{Article 267}",
                                                   "\\textsc{Article 268}", "\\textsc{Article 270}",
                                                   "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}"),
                              notes = c("Standard errors clustered clustered at the chamber-level are in 
                                        parentheses."),
                              notes.align = "l",  
                              keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(caseduration_Lux, sep = '\n', file = "caseduration_Lux_table.tex")

#Rate of Production
reg_rate_1 <- lm(rate_of_production ~ JR_Lux + factor(judges) + factor(year_case), data = dat)
rate_se_1 <- sqrt(diag(vcovPL(reg_rate_1, cluster = dat$judges, adjust = FALSE)))

reg_rate_2 <- lm(rate_of_production ~ JR_Lux + factor(year_case) + factor(judges) + 
                   art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language, data = dat)
rate_se_2 <- sqrt(diag(vcovPL(reg_rate_2, cluster = dat$judges, adjust = FALSE)))

reg_rate_3 <- lm(rate_of_production ~ JR_Lux + factor(year_case)  +
                   factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language + JR_experience + JR_Any_Judge, data = dat)
rate_se_3 <- sqrt(diag(vcovPL(reg_rate_3, cluster = dat$judges, adjust = FALSE)))

matched_rate_1 <- lm(rate_of_production ~ JR_Lux + factor(judges) + factor(year_case), data = processed.dat)
matched_rate_se_1 <- sqrt(diag(vcovPL(matched_rate_1, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_2 <- lm(rate_of_production ~ JR_Lux + factor(year_case) + factor(judges) + 
                       art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language, data = processed.dat)
matched_rate_se_2 <- sqrt(diag(vcovPL(matched_rate_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_3 <- lm(rate_of_production ~ JR_Lux + factor(year_case)  +
                       factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_rate_se_3 <- sqrt(diag(vcovPL(matched_rate_3, cluster = processed.dat$judges, adjust = FALSE)))

rate_Lux <- stargazer(reg_rate_1, matched_rate_1, reg_rate_2, matched_rate_2, reg_rate_3, matched_rate_3,
                      omit = c("year_case", "judges", "language", "Constant"), 
                      add.lines = list(c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                       c("Year Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                       c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                       c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                      se = list(rate_se_1,matched_rate_se_1,rate_se_2,matched_rate_se_2,rate_se_3,matched_rate_se_3),
                      star.cutoffs = c(0.05, 0.01, 0.001),
                      covariate.labels = c("\\textsc{Luxembourg JR}", "\\textsc{Article 256}", 
                                           "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                           "\\textsc{Article 265}", "\\textsc{Article 267}",
                                           "\\textsc{Article 268}", "\\textsc{Article 270}",
                                           "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}"),
                      notes = c("Standard errors clustered clustered at the chamber-level are in 
                                parentheses."),
                      notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(rate_Lux, sep = '\n', file = "rate_Lux_table.tex")

#Lexical Diversity
reg_lexical_1 <- lm(Maas ~ JR_Lux + factor(advocate_general) + factor(judges) + factor(JR_Country), data = dat) 
lexical_se_1 <- sqrt(diag(vcovPL(reg_lexical_1, cluster = dat$judges, adjust = FALSE)))

reg_lexical_2 <- lm(Maas ~ JR_Lux + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = dat) 
lexical_se_2 <- sqrt(diag(vcovPL(reg_lexical_2, cluster = dat$judges, adjust = FALSE)))


reg_lexical_3 <- lm(Maas ~ JR_Lux + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                      JR_experience + JR_Any_Judge, data = dat) 
lexical_se_3 <- sqrt(diag(vcovPL(reg_lexical_3, cluster = dat$judges, adjust = FALSE)))

matched_lexical_1 <- lm(Maas ~ JR_Lux + factor(advocate_general) + factor(judges) + factor(JR_Country), data = processed.dat) 
matched_lexical_se_1 <- sqrt(diag(vcovPL(matched_lexical_1, cluster = processed.dat$judges, adjust = FALSE)))

matched_lexical_2 <- lm(Maas ~ JR_Lux + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = processed.dat) 

matched_lexical_se_2 <- sqrt(diag(vcovPL(matched_lexical_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_lexical_3 <- lm(Maas ~ JR_Lux + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                          JR_experience + JR_Any_Judge, data = processed.dat) 
matched_lexical_se_3 <- sqrt(diag(vcovPL(matched_lexical_3, cluster = processed.dat$judges, adjust = FALSE)))

lexical_Lux <- stargazer(reg_lexical_1, matched_lexical_1, reg_lexical_2, matched_lexical_2, reg_lexical_3, matched_lexical_3,
                         omit = c("judges", "language", "Constant", "advocate_general", "JR_Country"), 
                         add.lines = list(c("Equation", "4", "4", "5","5", "6", "6"),
                                          c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                          c("Member State Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                          c("Advocate-General Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                          c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                          c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                         se = list(lexical_se_1,matched_lexical_se_1,lexical_se_2,matched_lexical_se_2,lexical_se_3,matched_lexical_se_3),
                         star.cutoffs = c(0.05, 0.01, 0.001),
                         covariate.labels = c("\\textsc{Luxembourg JR}", "\\textsc{Article 256}", 
                                              "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                              "\\textsc{Article 265}", "\\textsc{Article 267}",
                                              "\\textsc{Article 268}", "\\textsc{Article 270}",
                                              "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}"),
                         notes = c("Standard errors clustered clustered at the chamber-level are in 
                                   parentheses."),
                         notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(lexical_Lux, sep = '\n', file = "lexical_Lux_table.tex")

########Robustness Check including only Belgian-French Judges######

set.seed(12345)

#Creating Data Set

matched.model <- matchit(JR_Belgium_French ~ judges, 
                         data = dat, method = "exact", distance = "logit")
processed.dat <- match.data(matched.model)
summary(matched.model)


#Case Duration
reg_caseduration_1 <- lm(case_duration ~ JR_Belgium_French + factor(judges) + factor(year_case), data = dat)
caseduration_se_1 <- sqrt(diag(vcovPL(reg_caseduration_1, cluster = dat$judges,  adjust = FALSE)))

reg_caseduration_2 <- lm(case_duration ~ JR_Belgium_French + factor(year_case) +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language, data = dat)
caseduration_se_2 <- sqrt(diag(vcovPL(reg_caseduration_2, cluster = dat$judges,  adjust = FALSE)))



reg_caseduration_3 <- lm(case_duration ~ JR_Belgium_French + factor(year_case)  +
                           factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                           language + JR_experience + JR_Any_Judge, data = dat)
caseduration_se_3 <- sqrt(diag(vcovPL(reg_caseduration_3, cluster = dat$judges,  adjust = FALSE)))

matched_caseduration_1 <- lm(case_duration ~ JR_Belgium_French + factor(judges) + factor(year_case), data = processed.dat)
matched_caseduration_se_1 <- sqrt(diag(vcovPL(matched_caseduration_1, cluster = processed.dat$judges,  adjust = FALSE)))

matched_caseduration_2 <- lm(case_duration ~ JR_Belgium_French + factor(year_case) +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language, data = processed.dat)
matched_caseduration_se_2 <- sqrt(diag(vcovPL(matched_caseduration_2, cluster = processed.dat$judges,  adjust = FALSE)))

matched_caseduration_3 <- lm(case_duration ~ JR_Belgium_French + factor(year_case)  +
                               factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                               language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_caseduration_se_3 <- sqrt(diag(vcovPL(matched_caseduration_3, cluster = processed.dat$judges,  adjust = FALSE)))

caseduration_Belgium_French <- stargazer(reg_caseduration_1, matched_caseduration_1, reg_caseduration_2, matched_caseduration_2, reg_caseduration_3, matched_caseduration_3,
                                         omit = c("year_case", "judges", "language", "Constant"), 
                                         add.lines = list(c("Equation", "1", "1", "2","2", "3", "3"),
                                                          c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                          c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                          c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                                          c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                                         se = list(caseduration_se_1,matched_caseduration_se_1,caseduration_se_2,matched_caseduration_se_2,caseduration_se_3,matched_caseduration_se_3),
                                         star.cutoffs = c(0.05, 0.01, 0.001),
                                         covariate.labels = c("\\textsc{Belgium-French JR}", "\\textsc{Article 256}", 
                                                              "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                                              "\\textsc{Article 265}", "\\textsc{Article 267}",
                                                              "\\textsc{Article 268}", "\\textsc{Article 270}",
                                                              "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}"),
                                         notes = c("Heteroskedasticity-robust standard errors clustered clustered at the chamber-level are in 
                                                   parentheses."),
                                         notes.align = "l",  
                                         keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(caseduration_Belgium_French, sep = '\n', file = "caseduration_Belgium_French_table.tex")

#Rate of Production
reg_rate_1 <- lm(rate_of_production ~ JR_Belgium_French + factor(judges) + factor(year_case), data = dat)
rate_se_1 <- sqrt(diag(vcovPL(reg_rate_1, cluster = dat$judges, adjust = FALSE)))

reg_rate_2 <- lm(rate_of_production ~ JR_Belgium_French + factor(year_case) + factor(judges) + 
                   art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language, data = dat)
rate_se_2 <- sqrt(diag(vcovPL(reg_rate_2, cluster = dat$judges, adjust = FALSE)))

reg_rate_3 <- lm(rate_of_production ~ JR_Belgium_French + factor(year_case)  +
                   factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                   language + JR_experience + JR_Any_Judge, data = dat)
rate_se_3 <- sqrt(diag(vcovPL(reg_rate_3, cluster = dat$judges, adjust = FALSE)))

matched_rate_1 <- lm(rate_of_production ~ JR_Belgium_French + factor(judges) + factor(year_case), data = processed.dat)
matched_rate_se_1 <- sqrt(diag(vcovPL(matched_rate_1, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_2 <- lm(rate_of_production ~ JR_Belgium_French + factor(year_case) + factor(judges) + 
                       art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language, data = processed.dat)
matched_rate_se_2 <- sqrt(diag(vcovPL(matched_rate_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_rate_3 <- lm(rate_of_production ~ JR_Belgium_French + factor(year_case)  +
                       factor(judges) + art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 +
                       language + JR_experience + JR_Any_Judge, data = processed.dat)
matched_rate_se_3 <- sqrt(diag(vcovPL(matched_rate_3, cluster = processed.dat$judges, adjust = FALSE)))

rate_Belgium_French <- stargazer(reg_rate_1, matched_rate_1, reg_rate_2, matched_rate_2, reg_rate_3, matched_rate_3,
                                 omit = c("year_case", "judges", "language", "Constant"), 
                                 add.lines = list(c("Equation", "1", "1", "2","2", "3", "3"),
                                                  c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                  c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                  c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                                  c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                                 se = list(rate_se_1,matched_rate_se_1,rate_se_2,matched_rate_se_2,rate_se_3,matched_rate_se_3),
                                 star.cutoffs = c(0.05, 0.01, 0.001),
                                 covariate.labels = c("\\textsc{Belgium-French JR}", "\\textsc{Article 256}", 
                                                      "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                                      "\\textsc{Article 265}", "\\textsc{Article 267}",
                                                      "\\textsc{Article 268}", "\\textsc{Article 270}",
                                                      "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}"),
                                 notes = c("Standard errors clustered clustered at the chamber-level are in 
                                           parentheses."),
                                 notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(rate_Belgium_French, sep = '\n', file = "rate_Belgium_French_table.tex")

#Lexical Diversity
reg_lexical_1 <- lm(Maas ~ JR_Belgium_French + factor(advocate_general) + factor(judges) + factor(JR_Country), data = dat) 
lexical_se_1 <- sqrt(diag(vcovPL(reg_lexical_1, cluster = dat$judges, adjust = FALSE)))

reg_lexical_2 <- lm(Maas ~ JR_Belgium_French + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = dat) 
lexical_se_2 <- sqrt(diag(vcovPL(reg_lexical_2, cluster = dat$judges, adjust = FALSE)))

reg_lexical_3 <- lm(Maas ~ JR_Belgium_French + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                      art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                      JR_experience + JR_Any_Judge, data = dat) 
lexical_se_3 <- sqrt(diag(vcovPL(reg_lexical_3, cluster = dat$judges, adjust = FALSE)))

matched_lexical_1 <- lm(Maas ~ JR_Belgium_French + factor(advocate_general) + factor(judges) + factor(JR_Country), data = processed.dat) 
matched_lexical_se_1 <- sqrt(diag(vcovPL(matched_lexical_1, cluster = processed.dat$judges, adjust = FALSE)))

matched_lexical_2 <- lm(Maas ~ JR_Belgium_French + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language, data = processed.dat) 
matched_lexical_se_2 <- sqrt(diag(vcovPL(matched_lexical_2, cluster = processed.dat$judges, adjust = FALSE)))

matched_lexical_3 <- lm(Maas ~ JR_Belgium_French + factor(advocate_general) + factor(judges) + factor(JR_Country) + 
                          art_256 + art_258 + art_263 + art_265 + art_267 + art_268 + art_270 + language + 
                          JR_experience + JR_Any_Judge, data = processed.dat) 
matched_lexical_se_3 <- sqrt(diag(vcovPL(matched_lexical_3, cluster = processed.dat$judges, adjust = FALSE)))

lexical_Belgium_French <- stargazer(reg_lexical_1, matched_lexical_1, reg_lexical_2, matched_lexical_2, reg_lexical_3, matched_lexical_3,
                                    omit = c("judges", "language", "Constant", "advocate_general", "JR_Country"), 
                                    add.lines = list(c("Equation", "4", "4", "5","5", "6", "6"),
                                                     c("Chamber Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                     c("Member State Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                     c("Advocate-General Fixed Effects?", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                                     c("Case Controls?","No", "No", "Yes","Yes", "Yes", "Yes"),
                                                     c("Judge Personal Controls","No", "No", "No","No", "Yes", "Yes")),
                                    se = list(lexical_se_1,matched_lexical_se_1,lexical_se_2,matched_lexical_se_2,lexical_se_3,matched_lexical_se_3),
                                    star.cutoffs = c(0.05, 0.01, 0.001),
                                    covariate.labels = c("\\textsc{Belgium-French JR}", "\\textsc{Article 256}", 
                                                         "\\textsc{Article 258}", "\\textsc{Article 263}", 
                                                         "\\textsc{Article 265}", "\\textsc{Article 267}",
                                                         "\\textsc{Article 268}", "\\textsc{Article 270}",
                                                         "\\textsc{JR CJEU Experience}", "\\textsc{JR Previous Judicial Experience}"),
                                    notes = c("Heteroskedasticity-robust standard errors clustered clustered at the chamber-level are in 
                                              parentheses."),
                                    notes.align = "l", keep.stat = c("n", "rsq"), type = "latex", style = "default", digits = 3, no.space = TRUE)
cat(lexical_Belgium_French, sep = '\n', file = "lexical_Belgium_French_table.tex")