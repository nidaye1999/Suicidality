data=read.csv("result/data.csv", row.names=1, na.strings="", stringsAsFactors=T)

data=data[data$LC_Category!="Eating+", ]
data$LC_Category=factor(droplevels(data$LC_Category), levels = c("HC", "Dep+Anx", "Substance+"))
data$Employed=factor(data$Employed, levels = c("Yes", "No"))
data$Medicated=factor(data$Medicated, levels = c("Un-medicated", "Medicated"))
data$RaceEthnicity=factor(data$RaceEthnicity, levels = c("White", "Non-white"))


combine.no.rm=data[, !colnames(data) %in% c("behavior_T4", "ideation_T4", "behavior_T0", "ideation_T0")]
suppressPackageStartupMessages(library(VIM))
png("result/missing.png",width=10, height=7.5, units = "in", res = 200)
aggr(combine.no.rm, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=colnames(combine.no.rm), cex.axis=.7,cex.numbers=0.5,gap=3, ylab=c("Missing data","Pattern"), oma = c(10, 4.1, 4.1, 2.1))
dev.off()

# behavior=na.omit(data[, !colnames(data) %in% c("ideation", "combine")])
# ideation=na.omit(data[, !colnames(data) %in% c("behavior", "combine")])
combine=na.omit(combine.no.rm)

suppressPackageStartupMessages(library(table1))
table=table1(~.|LC_Category, data=combine, overall="Total")
table

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(psych))
table(combine$suicidality_T0)
table(combine$suicidality_T4)
confusionMatrix(combine$suicidality_T4, combine$suicidality_T0)
phi(confusionMatrix(combine$suicidality_T4, combine$suicidality_T0)$table)


Stepwise.Logistic.Regression=function(nothing, fullmod){
  print("-----forwards-----")
  forwards = step(nothing,direction="forward",scope=list(upper=fullmod,lower=nothing))
  print("-----backwards-----")
  backwards = step(fullmod, direction = "backward",scope=list(upper=fullmod,lower=nothing))
  print("-----bothways-----")
  bothways = step(nothing,direction="both",scope=list(upper=fullmod,lower=nothing))
  print("-----formula-----")
  print(formula(forwards))
  print(AIC(forwards))
  print(formula(backwards))
  print(AIC(backwards))
  print(formula(bothways))
  print(AIC(bothways))
}

# # behavior
# Stepwise.Logistic.Regression(glm(behavior ~ 1, data=behavior, family=binomial), glm(behavior~., data=behavior, family=binomial))
# 
# # ideation
# Stepwise.Logistic.Regression(glm(ideation ~ 1, data=ideation, family=binomial), glm(ideation~., data=ideation, family=binomial))
# 
# combine
# Stepwise.Logistic.Regression(glm(combine ~ 1, data=combine, family=binomial), glm(combine~., data=combine, family=binomial))

# suppressPackageStartupMessages(library(caret))
# logit.model=glm(combine ~ PANASX_NegAffect + LC_Category + log.IP10 + PROMIS_AlcoUseTscore, data=combine, family=binomial)
# print(summary(logit.model))
# print(varImp(logit.model))



suppressPackageStartupMessages(library(GGally))
p=ggpairs(combine, diag=list(continuous=wrap("barDiag", bins=50)), upper = list(continuous = wrap("cor", size = 8), combo = "facetdensity"), lower = list(continuous = wrap("points", size = 0.1), combo = "box"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("result/pair.png", p, dpi=200, height = 60, width = 60, limitsize = F)

for (var in colnames(combine)){
  if (is.factor(combine[[var]])){
    print(var)
    print(table(combine[[var]]))
  }
}

suppressPackageStartupMessages(library(caretStack))
suppressPackageStartupMessages(library(parallel))
combine.one.hot=data.frame(predict(dummyVars("~.", data=combine[,-c(1)], fullRank = T), newdata=combine[,-c(1)]))
combine.one.hot=cbind(suicidality_T4=combine$suicidality_T4, combine.one.hot)
write.csv(combine.one.hot, "result/data_one_hot.csv")
write.table(data.frame(colnames(combine.one.hot)[-c(1)]), "result/predictor_var.csv", row.names = F, col.names = F)

Stepwise.Logistic.Regression(glm(suicidality_T4 ~ 1, data=combine.one.hot, family=binomial), glm(suicidality_T4~., data=combine.one.hot, family=binomial))
logit.model=glm(suicidality_T4 ~ suicidality_T0.suicidal + PANASX_NegAffect + Employed.No + Medicated.Medicated + log.IP10 + RRS_Score, data=combine.one.hot, family=binomial)
print(summary(logit.model))
print(varImp(logit.model))

predict_two(combine.one.hot, "suicidality_T4", targetType = "binary", predictor_var_file_list=c("result/predictor_var.csv"), rdata_prefix="combine", outDir="result/stack", metric="AUC", ncore=detectCores()-2, methods=c('svmRadial', 'cforest', 'glmnet'), cmp.grp="suicidal")


source("effect_size.R")
for (var_name in names(combine.one.hot)[-c(1)]){
  print(var_name)
  effect_size(NULL, combine.one.hot, var_name, predict_function=logit_pred)
}
