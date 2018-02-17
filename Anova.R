data <- read.csv("anova_example.csv")

# creating dummy variables
data$a2 <- c(rep(0,45))
data$a2 <- ifelse(data$a==2,1,0)
data$a3 <- c(rep(0,45))
data$a3 <- ifelse(data$a==3,1,0)
data$b2 <- c(rep(0,45))
data$b2 <- ifelse(data$b==2,1,0)
data$b3 <- c(rep(0,45))
data$b3 <- ifelse(data$b==3,1,0)

# converting to effect coded variables
data$a2 <- ifelse(data$a==1,-1,data$a2)
data$a3 <- ifelse(data$a==1,-1,data$a3)
data$b2 <- ifelse(data$b==1,-1,data$b2)
data$b3 <- ifelse(data$b==1,-1,data$b3)

# creating interaction terms
data$a2xb2 <- data$a2*data$b2
data$a2xb3 <- data$a2*data$b3
data$a3xb2 <- data$a3*data$b2
data$a3xb3 <- data$a3*data$b3

# regression
reg_full <- lm(outcome~a2+a3+b2+b3+a2xb2+a2xb3+a3xb2+a3xb3, data=data)
summary(reg_full)
reg_noint <- lm(outcome~a2+a3+b2+b3, data=data)
summary(reg_noint)
reg_noa <- lm(outcome~b2+b3+a2xb2+a2xb3+a3xb2+a3xb3, data=data)
summary(reg_noa)
reg_nob <- lm(outcome~a2+a3+a2xb2+a2xb3+a3xb2+a3xb3, data=data)
summary(reg_nob)

# F for main effect of interaction
anova(reg_full,reg_noint)

# F for main effect of a
anova(reg_full,reg_noa)

# F for main effect of b
anova(reg_full,reg_nob)