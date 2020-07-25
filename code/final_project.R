# STAT 149 Final Project
# Jordan Turley


# Libraries and functions -------------------------------------------------
library(car)
library(mgcv)
library(rpart)
library(rpart.plot)

evaluate = function (true, pred) {
        no.c = sum(pred == 0)
        yes.c = sum(pred == 1)
        acc = sum(true == pred) / length(pred)
        
        return(list(no = no.c, yes = yes.c, accuracy = acc))
}

# Get the file containing na.convert.mean for handling missing data
source(file.choose())

# Read, view, process data ------------------------------------------------
chd.risk = read.csv(file.choose())
View(chd.risk)
summary(chd.risk)

# Convert the string variables to factor variables
chd.risk$education = factor(chd.risk$education)
chd.risk$sex = factor(chd.risk$sex)
chd.risk$smoker = factor(chd.risk$smoker)
chd.risk$OnBPMeds = factor(chd.risk$OnBPMeds)
chd.risk$PrevStroke = factor(chd.risk$PrevStroke)
chd.risk$Hyp = factor(chd.risk$Hyp)
chd.risk$Diab = factor(chd.risk$Diab)
chd.risk$CHD_Risk = factor(chd.risk$CHD_Risk)
summary(chd.risk)

# Use the na.convert.mean function to handle our missing data
chd.risk.na <- na.convert.mean(chd.risk)
summary(chd.risk.na)

# Plot variables ----------------------------------------------------------
# Plot each quantitative variable
plot(chd.risk$CHD_Risk, chd.risk$age,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Age',
     main = 'Age vs. 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$CHD_Risk, chd.risk$cigsPerDay,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Number of Cigaretts Smoked per Day',
     main = 'Number of Cigarettes Smoked per Day vs. 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$CHD_Risk, chd.risk$totChol,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Total Cholesterol Level, mg/dL',
     main = 'Total Cholesterol Level vs. 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$CHD_Risk, chd.risk$sysBP,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Systolic Blood Pressure, mmHg',
     main = 'Systolic Blood Pressure vs. 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$CHD_Risk, chd.risk$diaBP,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Diastolic Blood Pressure, mmHg',
     main = 'Diastolic Blood Pressure vs. 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$CHD_Risk, chd.risk$BMI,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Body Mass Index, kg/m?',
     main = 'Body Mass Index vs. 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$CHD_Risk, chd.risk$heartRate,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Heart Rate, beats per minute',
     main = 'Heart Rate vs. 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$CHD_Risk, chd.risk$glucose,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Glucose Level, mg/dL',
     main = 'Glucose Level vs. 10 Year Risk of Coronary Heart Disease')

# Plot each categorical variable
plot(chd.risk$CHD_Risk,
     xlab = '10 Year Risk of Coronary Heart Disease', ylab = 'Count',
     main = 'Distribution of 10 Year Risk of Coronary Heart Disease')
plot(chd.risk$education, chd.risk$CHD_Risk,
     xlab = 'Education Level', ylab = '10 Year Risk of Coronary Heart Disease',
     main = '10 Year Risk of Coronary Heart Disease vs. Education Level')
plot(chd.risk$sex, chd.risk$CHD_Risk,
     xlab = 'Sex', ylab = '10 Year Risk of Coronary Heart Disease',
     main = '10 Year Risk of Coronary Heart Disease vs. Sex')
plot(chd.risk$smoker, chd.risk$CHD_Risk,
     xlab = 'Smoker/Nonsmoker', ylab = '10 Year Risk of Coronary Heart Disease',
     main = '10 Year Risk of Coronary Heart Disease vs. Smoker/Nonsmoker')
plot(chd.risk$OnBPMeds, chd.risk$CHD_Risk,
     xlab = 'Blood Pressure Medication', ylab = '10 Year Risk of Coronary Heart Disease',
     main = '10 Year Risk of Coronary Heart Disease vs. Blood Pressure Medication')
plot(chd.risk$PrevStroke, chd.risk$CHD_Risk,
     xlab = 'Had a Stroke', ylab = '10 Year Risk of Coronary Heart Disease',
     main = '10 Year Risk of Coronary Heart Disease vs. Had a Stroke')
plot(chd.risk$Hyp, chd.risk$CHD_Risk,
     xlab = 'Hypertension', ylab = '10 Year Risk of Coronary Heart Disease',
     main = '10 Year Risk of Coronary Heart Disease vs. Hypertension')
plot(chd.risk$Diab, chd.risk$CHD_Risk,
     xlab = 'Diabetes', ylab = '10 Year Risk of Coronary Heart Disease',
     main = '10 Year Risk of Coronary Heart Disease vs. Diabetes')

# Check collinearity and train/test split ---------------------------------
vif.model = lm(as.numeric(CHD_Risk) ~ ., data = chd.risk.na)
vif(vif.model)

# Split into training and test
set.seed(42)
test.indices = sample(seq_len(nrow(chd.risk.na)), size = nrow(chd.risk.na) * 0.2)
chd.train = chd.risk.na[-test.indices, ]
chd.test = chd.risk.na[test.indices, ]

# Logistic regression -----------------------------------------------------
# Create base logistic regression
chd.lr.full = glm(CHD_Risk ~ ., family = binomial, data = chd.train)
summary(chd.lr.full)

# Interact sex with all variables and test with LRT
chd.lr.inter = glm(CHD_Risk ~ . + education * . + sex * . + smoker * . + OnBPMeds * .
                           + PrevStroke * . + Hyp * . + Diab * .,
                   family = binomial, data = chd.train)
summary(chd.lr.inter)

anova(chd.lr.full, chd.lr.inter, test = 'Chi')

## Analysis of deviance to remove some variables
chd.lr.1 = glm(CHD_Risk ~ . - education, family = binomial, data = chd.train)
anova(chd.lr.full, chd.lr.1, test = 'Chi')

summary(chd.lr.1)

# Check smoker variables
chd.lr.2 = glm(CHD_Risk ~ . - education - cigsPerDay - smoker,
               family = binomial, data = chd.train)
anova(chd.lr.1, chd.lr.2, test = 'Chi')

# Check blood pressure variables
chd.lr.3 = glm(CHD_Risk ~ . - education - sysBP - diaBP,
               family = binomial, data = chd.train)
anova(chd.lr.1, chd.lr.3, test = 'Chi')

# Check BMI, heartRate, and OnBPMeds
chd.lr.4 = glm(CHD_Risk ~ . - education - BMI - heartRate - OnBPMeds,
               family = binomial, data = chd.train)
anova(chd.lr.1, chd.lr.4, test = 'Chi')

summary(chd.lr.4)

# Check hypertension and diabetic
chd.lr.5 = glm(CHD_Risk ~ . - education - BMI - heartRate - OnBPMeds - Hyp - Diab,
               family = binomial, data = chd.train)
anova(chd.lr.4, chd.lr.5, test = 'Chi')

summary(chd.lr.5)

# Finally, check the NA variables
chd.lr.na = glm(CHD_Risk ~ . - education - BMI - heartRate - OnBPMeds - Hyp - Diab -
                        cigsPerDay.na - totChol.na - BMI.na - heartRate.na - glucose.na,
                family = binomial, data = chd.train)
anova(chd.lr.5, chd.lr.na, test = 'Chi')

summary(chd.lr.na)

# We will use chd.lr.5
chd.lr.reduced = chd.lr.5
summary(chd.lr.reduced)

# Test the reduced model vs the full model
anova(chd.lr.full, chd.lr.reduced, test = 'Chi')

# Generalized additive model ----------------------------------------------
chd.gam.full = gam(CHD_Risk ~ s(age) + s(cigsPerDay) + s(totChol) + s(sysBP) + s(diaBP)
                   + s(BMI) + s(heartRate) + s(glucose)
                   + education + sex + smoker + OnBPMeds + PrevStroke + Hyp + Diab
                   + cigsPerDay.na + totChol.na + BMI.na + heartRate.na + glucose.na,
                   family = binomial, data = chd.train)
summary(chd.gam.full)

# Compare full LR model to full GAM model
anova(chd.lr.full, chd.gam.full, test = 'Chisq')

chd.gam.reduced  = gam(CHD_Risk ~ s(age) + s(cigsPerDay) + s(totChol) + s(sysBP) + s(diaBP)
                       + s(glucose) + sex + smoker + PrevStroke
                       + cigsPerDay.na + totChol.na + BMI.na + heartRate.na + glucose.na,
                       family = binomial, data = chd.train)
summary(chd.gam.reduced)

anova(chd.gam.full, chd.gam.reduced, test = 'Chisq')

# Compare full smoother to reduced smoother, then compare smoother to logistic regression
anova(chd.gam.reduced, chd.lr.reduced, test = 'Chisq')

# Plot the reduced smoother variables
plot(chd.gam.reduced, residuals = TRUE)

# Classification tree -----------------------------------------------------
chd.tree.full = rpart(CHD_Risk ~ ., method = 'class',
                      parms=list(split="information"), data = chd.train, cp = 0.001)
prp(chd.tree.full, main = 'Full Classification Tree')
printcp(chd.tree.full)
plotcp(chd.tree.full)

chd.tree.pruned = prune(chd.tree.full, cp = 0.0044)
prp(chd.tree.pruned, main = 'Pruned Classification Tree')

# Evaluate models ---------------------------------------------------------
lr.full.pred = round(predict(chd.lr.full, newdata = chd.test, type = 'response'))
lr.reduced.pred = round(predict(chd.lr.reduced, newdata = chd.test, type = 'response'))
gam.full.pred = round(predict(chd.gam.full, newdata = chd.test, type = 'response'))
gam.reduced.pred = round(predict(chd.gam.reduced, newdata = chd.test, type = 'response'))
tree.full.pred = as.numeric(predict(chd.tree.full, newdata = chd.test, type = 'class')) - 1
tree.pruned.pred = as.numeric(predict(chd.tree.pruned, newdata = chd.test, type = 'class')) - 1

chd.risk.test.true = as.numeric(chd.test$CHD_Risk) - 1
sum(chd.risk.test.true == 0)
sum(chd.risk.test.true == 1)

lr.full.eval = evaluate(chd.risk.test.true, lr.full.pred)
lr.reduced.eval = evaluate(chd.risk.test.true, lr.reduced.pred)
gam.full.eval = evaluate(chd.risk.test.true, gam.full.pred)
gam.reduced.eval = evaluate(chd.risk.test.true, gam.reduced.pred)
tree.full.eval = evaluate(chd.risk.test.true, tree.full.pred)
tree.pruned.eval = evaluate(chd.risk.test.true, tree.pruned.pred)

lr.full.eval
lr.reduced.eval
gam.full.eval
gam.reduced.eval
tree.full.eval
tree.pruned.eval











