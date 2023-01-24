#############
#Normality testing
#############

library(nortest)
StatsMiniCourseE1 <- read.delim("Exercise1Data.txt", header = T)

shapiro.test(StatsMiniCourseE1$numberOfDifferentSpecies)

hist(StatsMiniCourseE1$numberOfDifferentSpecies

#############
#Data transformation
#############

StatsMiniCourseE1$logTransformed <- (1+(log(StatsMiniCourseE1$numberOfDifferentSpecies))) #Log

StatsMiniCourseE1$log10Transformed <- (1+(log10(StatsMiniCourseE1$numberOfDifferentSpecies))) #Log  base 10

StatsMiniCourseE1$squareRootTransformed <- sqrt(StatsMiniCourseE1$numberOfDifferentSpecies) #Square root

StatsMiniCourseE1$SpP3 <- (StatsMiniCourseE1$numberOfDifferentSpecies)^3 #Raise to the power of 3

StatsMiniCourseE1$SpP9 <- (StatsMiniCourseE1$numberOfDifferentSpecies)^(1/9) #Takes the ninth root

StatsMiniCourseE1$SpEX <- exp(StatsMiniCourseE1$numberOfDifferentSpecies) #Raises constant e to the power of selected variable

StatsMiniCourseE1$SpAB <- abs(StatsMiniCourseE1$numberOfDifferentSpecies) #Finds the absolute value

StatsMiniCourseE1$SpSI <- sin(StatsMiniCourseE1$numberOfDifferentSpecies) #Sine

#############
#T-test for pairwise, normal data (independent samples)
#############

boxplot(changeInBodyWeight ~ gender, cex.lab = 1.5, cex.axis = 1.5, xlab = 'Gender', ylab = 'Change in body weight (mg)', data = StatsMiniCourseE2)

indep_T_test <- t.test(changeInBodyWeight ~ gender, var.equal = FALSE, alternative = "two.sided", data = StatsMiniCourseE2)

indep_T_test

#############
#T-test for pairwise, normal data (dependent samples)
#############

boxplot(leafLength ~ time, cex.lab = 1.5, cex.axis = 1.5, xlab = 'Treatment', ylab = 'Leaf length (cm)', data = StatsMiniCourseE3)

paired_T_test <- t.test([Dependent Variable] ~ [Independent variable], var.equal = FALSE, alternative = "greater", paired = TRUE,  data = StatsMiniCourseE3)

paired_T_test

#############
#Pairwise test for non-paramentric data (independent samples)
#############

boxplot(totalLength ~ gender, cex.lab=1.5, cex.axis=1.5, xlab= 'Gender', ylab= 'Body length (mm)', data = StatsMiniCourseE4)

indep_wilcox <- wilcox.test(totalLength ~ gender, alternative = "less", correct = FALSE, data =StatsMiniCourseE4)      #To perform test

indep_wilcox

#############
#Pairwise test for non-parametric data (dependent samples)
#############

boxplot(weight ~ time, cex.lab = 1.5, cex.axis = 1.5, xlab = 'Treatment', ylab = 'Body weight (g)', data =StatsMiniCourseE5)

paired_wilcox <- wilcox.test(weight ~ time, alternative = "greater", correct = FALSE, paired = TRUE, data = StatsMiniCourseE5)      #To perform test

paired_wilcox

#############
#ANOVA for parametric data across multiple groups
#############

library(nortest)
library(car)
library(lattice)
library(multcomp)

#Normality testing

ANOVA <- aov(flipperLength ~ species, data = StatsMiniCourseE6)   #Prelim ANOVA for normality testing
par(mfrow = c(1, 2))        #Visual inspection
hist(ANOVA$residuals)       #Visual inspection
qqPlot(ANOVA$residuals, id = FALSE)       #Visual inspection
shapiro.test(ANOVA$residuals)             #Normality test

#Testing for homogeneity

par(mfrow = c(1, 2))    #Visual inspection
boxplot(flipperLength ~ species, data = StatsMiniCourseE6)      #Visual inspection
dotplot(flipperLength ~ species, data = StatsMiniCourseE6)      #Visual inspection
leveneTest(flipperLength ~ species, data = StatsMiniCourseE6)   #Perform testing

#Perform test (homogenous groups)

boxplot(flipperLength ~ species, data = StatsMiniCourseE6)
ANOVAresults <- aov(flipperLength ~ species, data = StatsMiniCourseE6)
ANOVAresults
summary(ANOVAresults)
TukeyHSD(ANOVAresults)

#Perform test (heterogenous groups)

oneway.test(flipperLength ~ species, var.equal = FALSE, data = StatsMiniCourseE6)
TukeyHSD(ANOVAresults)


#############
#ANOVA for non-parametric data across multiple groups
#############

library(nortest)
library(car)
library(lattice)
library(multcomp)
library(FSA)

#Normality testing

prelimANOVA <- aov(count ~ genus, data = StatsMiniCourseE7)     #Prelim model for normality testing
par(mfrow = c(1, 2))        #Visual inspection
hist(prelimANOVA$residuals)       #Visual inspection
qqPlot(prelimANOVA$residuals, id = FALSE)     #Visual inspection
shapiro.test(prelimANOVA$residuals)       #Normality testing

#Testing for homogeneity

par(mfrow = c(1, 2))    #Visual inspection
boxplot(count ~ genus, data = StatsMiniCourseE7)      #Visual inspection
dotplot(count ~ genus, data = StatsMiniCourseE7)     #Visual inspection
leveneTest(count ~ genus, data = StatsMiniCourseE7)   #Perform testing

#Perform test (homogenous groups)

boxplot(count ~ genus, data = StatsMiniCourseE7)
kruskal.test(count ~ genus, data = StatsMiniCourseE7)
dunnTest(count ~ genus, data = StatsMiniCourseE7)


#############
#ANCOVA for parametric data across multiple groups
#############

library(nortest)
library(car)
library(lattice)
library(multcomp)
library(ggpubr)
library(rstatix)
library(emmeans)

#Normality testing

ANOVA <- aov(postTest ~ group, data = StatsMiniCourseE9)   #Prelim ANOVA for normality testing
par(mfrow = c(1, 2))        #Visual inspection
hist(ANOVA$residuals)       #Visual inspection
qqPlot(ANOVA$residuals, id = FALSE)       #Visual inspection
shapiro.test(ANOVA$residuals)

#Testing for homogeneity

par(mfrow = c(1, 2))    #Visual inspection
boxplot(postTest ~ group, data = StatsMiniCourseE9)      #Visual inspection
dotplot(postTest ~ group, data = StatsMiniCourseE9)      #Visual inspection
leveneTest(postTest ~ group, data = StatsMiniCourseE9)   #Perform testing

#Testing for linearity between outcome and covariate

ggscatter(StatsMiniCourseE9, x = "preTest", y = "postTest", color = "group", add = "reg.line" )+ stat_regline_equation( aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group) )

#Testing homogeneity of slopes

StatsMiniCourseE9 %>% anova_test(postTest ~ group* preTest)

#Perform test (homogenous groups)

boxplot(postTest ~ group, data = StatsMiniCourseE9)
res.aov <- StatsMiniCourseE9 %>% anova_test(postTest ~ preTest + group)
get_anova_table(res.aov)
pwc <- StatsMiniCourseE9 %>% emmeans_test(postTest ~ group, covariate = preTest, p.adjust.method = "bonferroni" )
pwc


#############
#Correlation test for bivariate data
#############

#Both columns are paramentric

PearsonCorTest <- cor.test(StatsMiniCourseE8$mpg, StatsMiniCourseE8$wt, method = "pearson")
PearsonCorTest

plot(StatsMiniCourseE8$mpg, StatsMiniCourseE8$wt, main = 'Visualisation of results', cex.lab = 1.5, cex.axis = 1.5,pch = 16, ylab = "wt", xlab = "mpg")
abline(lm(StatsMiniCourseE8$wt ~ StatsMiniCourseE8$mpg))

#One or both columns are non-paramentric

SpearmanCorTest <- cor.test(StatsMiniCourseE8$mpg, StatsMiniCourseE8$hp, method = "spearman")
SpearmanCorTest

plot(StatsMiniCourseE8$mpg, StatsMiniCourseE8$hp, main = 'Visualisation of results', cex.lab = 1.5, cex.axis = 1.5,pch = 16, ylab = "hp", xlab = "mpg")
abline(lm(StatsMiniCourseE8$hp ~ StatsMiniCourseE8$mpg))



##############END##############
