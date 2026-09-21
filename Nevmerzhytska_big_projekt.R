setwd("~/Downloads/semestr 4/Statistika/archive")
library(tidyverse)
library(tidyr)
 shp <- read_csv("student_habits_performance.csv")
 names(shp)
shp$social_media_h <- shp$social_media_hours + shp$netflix_hours
shp$social_media_hours <- NULL
shp$netflix_hours <- NULL
shp$internet_quality <- NULL
shp$parental_education_level <- NULL
shp$extracurricular_participation <- NULL
shp$age <- NULL
shp$gender <- NULL
View(shp)
names(shp)
str(shp)
# ukol 2 
# cast 1 
summary(shp %>% select(where(is.numeric)))
names(shp %>% select(where(is.numeric)))
par(mfrow = c(1, 2))
hist(shp$study_hours_per_day, main = "study_hours", xlab = "hodiny studia", ylab = "pocet", col = "lightgray")

boxplot(shp$study_hours_per_day, main = "Hodiny studia", ylab = "hodiny", col = "lightgray",notch = TRUE)

var(shp$study_hours_per_day, na.rm = TRUE)  
sd(shp$study_hours_per_day, na.rm = TRUE) 
IQR(shp$study_hours_per_day, na.rm = TRUE)
hist(shp$attendance_percentage, main = "attendance", xlab = "dochazka (%)", ylab = "pocet", col = "lightgray")
boxplot(shp$attendance_percentage, main = "attendance", ylab = "dochazka (%)", col = "lightgray",notch = TRUE)
var(shp$attendance_percentage, na.rm = TRUE)  
sd(shp$attendance_percentage, na.rm = TRUE) 
IQR(shp$attendance_percentage, na.rm = TRUE)
hist(shp$sleep_hours, main = "sleep", xlab = "hodiny spanku", ylab = "pocet", col = "lightgray")
boxplot(shp$sleep_hours, main = "sleep_hours", ylab = "hodiny spanku", col = "lightgray",notch = TRUE)
var(shp$sleep_hours, na.rm = TRUE)  
sd(shp$sleep_hours, na.rm = TRUE) 
IQR(shp$sleep_hours, na.rm = TRUE)

hist(shp$social_media_h, main = "social media", xlab = "cas (h)", ylab = "pocet", col = "lightgray")
boxplot(shp$social_media_h, main = "social media", ylab = "cas (h)", col = "lightgray",notch = TRUE)
var(shp$social_media_h, na.rm = TRUE)  
sd(shp$social_media_h, na.rm = TRUE) 
IQR(shp$social_media_h, na.rm = TRUE)

hist(shp$exam_score, main = "exam score", xlab = "body", ylab = "pocet", col = "lightgray")
boxplot(shp$exam_score, main = "exam score", ylab = "pocet", col = "lightgray",notch = TRUE)
var(shp$exam_score, na.rm = TRUE)  
sd(shp$exam_score, na.rm = TRUE) 
IQR(shp$exam_score, na.rm = TRUE)

barplot(table(shp$exercise_frequency), main = "exercise", xlab = "cvicení", ylab = "pocet", col = "lightgray")
var(shp$exercise_frequency, na.rm = TRUE)  
sd(shp$exercise_frequency, na.rm = TRUE) 
IQR(shp$exercise_frequency, na.rm = TRUE)

barplot(table(shp$mental_health_rating), main = "mental health", xlab = "zdravi", ylab = "pocet", col = "lightgray")
var(shp$mental_health_rating, na.rm = TRUE)  
sd(shp$mental_health_rating, na.rm = TRUE) 
IQR(shp$mental_health_rating, na.rm = TRUE)
par(mfrow = c(1, 1))
diet_tab <- table(shp$diet_quality)
diet_tab
barplot(diet_tab, main = "diet quality", ylab = "Pocet", col = "lightgray")

job_tab <- table(shp$part_time_job)
job_tab
barplot(job_tab, main = "part time job", ylab = "Pocet", col = "lightgray")
#cast 2
shapiro.test(shp$exam_score)
mean(shp$exam_score, na.rm = TRUE)
median(shp$exam_score, na.rm = TRUE)
var(shp$exam_score, na.rm = TRUE)
sd(shp$exam_score, na.rm = TRUE)
IQR(shp$exam_score, na.rm = TRUE)
install.packages("BSDA")
library(BSDA)
SIGN.test(shp$exam_score, md = 70)
  



#UKOL 3 
# CAST 1 
job_tab
binom.test(785, 1000, 0.8)
# CAST 2 
diet_tab
diet_tab_celkem <- sum(diet_tab)
hypoteza <- c(0.45, 0.35, 0.2)
oc_cet <-  diet_tab_celkem * hypoteza
srovnani <- as.table(rbind(diet_tab, oc_cet))
rownames(srovnani) <- c("Pozorovane", "Ocekavane")
srovnani
par(mar = c(3, 4, 2, 2) + 2, xpd = TRUE)
barplot(srovnani, 
        beside = TRUE, 
        col = c('lightblue', 'darkblue'), 
        main = "Porovnani cetnosti")
legend('bottomright', legend = c("pozorovane", "ocekavane"), fill = c('lightblue', 'darkblue'),
       inset = c(-0.1, -0.4),horiz = TRUE)
test <- chisq.test(diet_tab, p = hypoteza)
print(test)
# CAST 3
par(mfrow = c(1, 2))
hist(shp$sleep_hours, main = "sleep", xlab = "hodiny spanku", ylab = "pocet", col = "lightyellow")
qqnorm(shp$sleep_hours, main = "Normal Q-Q Plot pro hodiny spanku")
qqline(shp$sleep_hours, col = "red", lwd = 1.7, xpd = FALSE)
par(mfrow = c(1, 1))
shapiro.test(shp$sleep_hours)
PearsonTest(shp$sleep_hours)
n <- length(shp$sleep_hours)
pocet_trid <- ceiling(2 * (n^(2/5)))
pocet_trid
m <- mean(shp$sleep_hours) 
s <- sd(shp$sleep_hours)
int <- qnorm(seq(0, 1, by = 1/pocet_trid), mean = m, sd = s)
kategorie <- cut(shp$sleep_hours, int)
poz_cetnosti <- table(kategorie)
oc_cetnosti <- rep(n / pocet_trid, pocet_trid)
cetnosti_srovnani <- as.table(rbind(poz_cetnosti, oc_cetnosti))
par(mar = c(3, 4, 2, 2) + 2, xpd = TRUE)
barplot(cetnosti_srovnani, beside = TRUE, col = c('lightblue', 'darkblue'), main = "Porovnani cetnosti (Normalita)", las = 2)
legend('topright', legend = c("Pozorovane", "Ocekavane"), fill = c('lightblue', 'darkblue'), inset = c(-0.1, 0),horiz = TRUE)
test_statistika <- sum(((poz_cetnosti - oc_cetnosti)^2) / oc_cetnosti)
print(test_statistika)
p_hodnota <- 1 - pchisq(test_statistika, pocet_trid - 3)
print(p_hodnota)
# UKOL 4
# CAST 1 
shp$sm_kategorie <- cut(shp$social_media_h, 
                        breaks = c(-Inf, 2, 4, 6, Inf), 
                        labels = c("0-2h", "2-4h", "4-6h", "> 6h"))
shp$mh_kategorie <- cut(shp$mental_health_rating, 
                        breaks = c(0, 2, 4, 6, 8, 10), 
                        labels = c("1-2", "3-4", "5-6", "7-8", "9-10"))

tabulka <- table(shp$mh_kategorie, shp$sm_kategorie)
print(addmargins(tabulka))
tab_relativni <- round(prop.table(tabulka, margin = 2), 2)
print(tab_relativni)
par(mar = c(3, 4, 2, 2) + 2, mfrow = c(1, 2), xpd = TRUE)
barplot(tabulka, beside = TRUE, las = 1, 
        col = c("darkred", "red", "orange", "yellowgreen", "green"), 
        ylab = "cetnost", main = "Absolutni cetnosti")
legend('topright', legend = rownames(tabulka), fill = c("darkred", "red", "orange", "yellowgreen", "green"), title="Mental h", inset = c(-0.47, -0.1),horiz = F)
barplot(tab_relativni, beside = TRUE, las = 1, 
        col = c("darkred", "red", "orange", "yellowgreen", "green"), 
        ylab = "proporce", main = "Relativni struktura")

par(mfrow = c(1, 1))
test_sm <- chisq.test(tabulka)
print(test_sm)
print(test_sm$expected)
# CAST 2
plot(shp$study_hours_per_day, shp$exam_score,
     main = "Vztah: Doba studia vs Vysledek zkousky",
     xlab = "Doba studia (hodiny denne)",
     ylab = "Vysledek zkousky",
     pch = 16, 
     col = "steelblue")

par(mfrow = c(1, 2))
hist(shp$study_hours_per_day, main = "Rozlozeni: Doba studia", xlab = "Hodiny", col = "lightblue")
hist(shp$exam_score, main = "Rozložení: Vysledek zkousky", xlab = "Skore", col = "lightcoral")
par(mfrow = c(1, 1))

shapiro.test(shp$study_hours_per_day)
shapiro.test(shp$exam_score)
cor.test(shp$study_hours_per_day, shp$exam_score, method = "spearman")




# UKOL 5 
# CAST 1 

# 1) Modelovani linearniho vztahu
model1 <- lm(exam_score ~ study_hours_per_day, data = shp)
summary(model1)
par(mfrow = c(1, 4))
plot(model1) 
rezidua <- residuals(model1)
par(mfrow = c(1, 2))
hist(rezidua, main = "Histogram rezidui", xlab = "Rezidua", col = "lightyellow")
qqnorm(rezidua, main = "Normal Q-Q Plot pro rezidua")
qqline(rezidua, col = "red", lwd = 1.7, xpd = FALSE)

par(mfrow = c(1, 1))

shapiro.test(rezidua)

plot(shp$study_hours_per_day, shp$exam_score,
     main = "Vztah: Doba studia vs Vysledek zkousky",
     xlab = "Doba studia (hodiny denne)",
     ylab = "Vysledek zkousky",
     pch = 16, 
     col = "steelblue")
abline(model1, col = "red", lwd = 2, xpd = FALSE)
#CAST 2 
model2 <- lm(exam_score ~ study_hours_per_day + part_time_job, data = shp)
summary(model2)
model3 <- lm(exam_score ~ study_hours_per_day * part_time_job, data = shp)
summary(model3)
plot() 

anova(model1, model2)
anova(model2, model3)

shp$col <- ifelse(shp$part_time_job == "Yes", "red", "blue")
bez_prace <- which(shp$part_time_job == "No")
s_praci <- which(shp$part_time_job == "Yes")
par(mfrow = c(1, 2))
plot(shp$study_hours_per_day, shp$exam_score, col = shp$col,
     xlab = "Doba studia (hodiny)", ylab = "Vysledek zkousky",
     main = "Aditivni model")
#legend("topleft", col = c("blue", "red"), legend = c("No (Bez brigady)", "Yes (S brigadou)"), pch = 1)

line_bez_a <- cbind(shp$study_hours_per_day[bez_prace], model2$fit[bez_prace])
line_bez_a <- line_bez_a[order(line_bez_a[,1]),]
points(line_bez_a, type = "l", col = "blue", lwd = 2, xpd = FALSE)

line_s_a <- cbind(shp$study_hours_per_day[s_praci], model2$fit[s_praci])
line_s_a <- line_s_a[order(line_s_a[,1]),]
points(line_s_a, type = "l", col = "red", lwd = 2, xpd = FALSE)

plot(shp$study_hours_per_day, shp$exam_score, col = shp$col,
     xlab = "Doba studia (hodiny)", ylab = "Vysledek zkousky",
     main = "Model s interakci")

line_bez_i <- cbind(shp$study_hours_per_day[bez_prace], model3$fit[bez_prace])
line_bez_i <- line_bez_i[order(line_bez_i[,1]),]
points(line_bez_i, type = "l", col = "blue", lwd = 2, xpd = FALSE)

line_s_i <- cbind(shp$study_hours_per_day[s_praci], model3$fit[s_praci])
line_s_i <- line_s_i[order(line_s_i[,1]),]
points(line_s_i, type = "l", col = "red", lwd = 2, xpd = FALSE)
par(mfrow = c(1, 1))

rezidua3 <- residuals(model3)
par(mfrow = c(1, 2), xpd = FALSE)
hist(rezidua3, main = "Histogram rezidui (Model 3)", xlab = "Rezidua", col = "lightgreen")
qqnorm(rezidua3, main = "Q-Q Plot (Model 3)")
qqline(rezidua3, col = "red", lwd = 2)
par(mfrow = c(1, 1))
shapiro.test(rezidua3)






# UKOL 6 
# CAST 1 
boxplot(study_hours_per_day ~ part_time_job, data = shp, 
        col = c("lightblue", "lightcoral"), 
        main = "Doba studia podle brigady",
        xlab = "Brigada", 
        ylab = "Doba studia (hodiny denně)",
        las = 1)

# 2. Ověření předpokladů: Normalita (tvar rozdělení)
par(mfrow = c(1, 2))
hist(shp$study_hours_per_day[which(shp$part_time_job == "No")], 
     col = "lightblue", main = "Bez brigady", xlab = "Doba studia")
hist(shp$study_hours_per_day[which(shp$part_time_job == "Yes")], 
     col = "lightcoral", main = "S brigadou", xlab = "Doba studia")
par(mfrow = c(1, 1))

shapiro.test(shp$study_hours_per_day[which(shp$part_time_job == "No")])
shapiro.test(shp$study_hours_per_day[which(shp$part_time_job == "Yes")])

var.test(study_hours_per_day ~ part_time_job, data = shp)
t.test(study_hours_per_day ~ part_time_job, data = shp, var.equal = TRUE)

rozdil <- shp$social_media_h - shp$study_hours_per_day
hist(rozdil, col = "lightgreen", 
     main = "Rozdil: Site vs. Uceni", 
     xlab = "Rozdil v hodinach (Site - Uceni)")

shapiro.test(rozdil)
t.test(shp$social_media_h, shp$study_hours_per_day, paired = TRUE)
# CAST 2  
# ==========================================
# UKOL 6 - CAST 2: Analýza rozptylu (ANOVA)
# ==========================================

# 1. Vizualizace dat pomoci boxplotu
boxplot(exam_score ~ diet_quality, data = shp, 
        col = c("lightgreen", "lightblue", "salmon"),
        main = "Skore zkousky podle kvality stravy",
        xlab = "Kvalita stravy (diet_quality)", 
        ylab = "Skore ze zkoušky",
        las = 1)

# 2. Ověření normality pro každou skupinu (Shapiro-Wilkův test)
shapiro.test(shp$exam_score[which(shp$diet_quality == "Fair")])
shapiro.test(shp$exam_score[which(shp$diet_quality == "Good")])
shapiro.test(shp$exam_score[which(shp$diet_quality == "Poor")])

# 3. Ověření shody rozptylů pro 3 skupiny (Bartlettův test)
bartlett.test(exam_score ~ diet_quality, data = shp)
kruskal.test(exam_score ~ diet_quality, data = shp)







