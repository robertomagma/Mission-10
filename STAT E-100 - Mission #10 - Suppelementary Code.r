# STAT E-100
# 2022 FA
# Mission #10

# Data Dictionary:
# https://cran.r-project.org/web/packages/Stat2Data/Stat2Data.pdf

# Is there a significant correlation between the number of hospitals and the number of physicians?
a_correlation <- cor(x = MetroHealth83$NumMDs, y = MetroHealth83$NumBeds)
a_correlation # 0.945
cor.test(x = MetroHealth83$NumMDs, y = MetroHealth83$NumBeds) # 0.945

# Is there a significant linear association between Religiosity and GDP?

# https://www.datasciencemadesimple.com/get-the-list-of-column-names-of-dataframe-in-r-2/
colnames(ReligionGDP)

plot(x = ReligionGDP$Religiosity, y = ReligionGDP$GDP)
abline(lm(GDP ~ Religiosity, data = ReligionGDP))

cor(x = ReligionGDP$Religiosity, y = ReligionGDP$GDP)
cor.test(x = ReligionGDP$Religiosity, y = ReligionGDP$GDP)

slr_1 <- lm(GDP ~ Religiosity, data = ReligionGDP)
summary(slr_1)

# QQ-plot: assumption of normality
qqnorm(slr_1$residuals)
qqline(slr_1$residuals, col = 2,lwd=2,lty=2)

# Histogram of residuals: assumption of normality
x2 <- seq(min(slr_1$residuals), max(slr_1$residuals), length = 40)
fun <- dnorm(x2, mean = mean(slr_1$residuals), sd = sd(slr_1$residuals))
hist(slr_1$residuals, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
lines(x2, fun, col = 2, lwd = 2)

# Equal variance assumption
library("car")
ncvTest(slr_1)

# K-S test of normality
ks.test(slr_1$residuals, "pnorm", mean=mean(slr_1$residuals), sd=sd(slr_1$residuals))

# Is there a significant linear association between the number of doctors and the number of beds?

slr_2 <- lm(NumBeds ~ NumMDs, data = MetroHealth83)
summary(slr_2)
plot(x = MetroHealth83$NumMDs, y = MetroHealth83$NumBeds)
abline(lm(NumBeds ~ NumMDs, data = MetroHealth83))

# QQ-plot: assumption of normality
qqnorm(slr_2$residuals)
qqline(slr_2$residuals, col = 2,lwd=2,lty=2)
# Histogram of residuals: assumption of normality
x2 <- seq(min(slr_2$residuals), max(slr_2$residuals), length = 40)
fun <- dnorm(x2, mean = mean(slr_2$residuals), sd = sd(slr_2$residuals))
hist(slr_2$residuals, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
lines(x2, fun, col = 2, lwd = 2)

# K-S test of normality
ks.test(slr_2$residuals, "pnorm",
        mean=mean(slr_2$residuals),
        sd=sd(slr_2$residuals))

# Equal variance assumption
install.packages("car")
library("car")
ncvTest(slr_2)

# Residual scatterplot
res <- resid(slr_2)
plot(fitted(slr_2), res) #produce residual vs. fitted plot
abline(0,0) #add a horizontal line at 0 