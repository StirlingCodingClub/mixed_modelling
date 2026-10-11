# install.packages("nlme");
library(nlme);
dung <- read.csv("dung.csv");

mod_lm <- lm(formula = 1 + pr_dung ~ flies, data = dung);

ones <- rep(x = 1, times = dim(dung)[1]);
xx   <- dung$flies;

Y <- matrix(data = dung$pr_dung, ncol = 1);
X <- as.matrix(cbind(ones, xx));
B <- as.matrix(mod$coefficients, ncol = 1);
e <- residuals(mod);

X %*% B + e;

















# Take a look at the small data set (small so we can see the maths)
print(dung);

mod  <- lme(pr_dung ~ 1 + flies, random = ~ 1|oven_block, data = dung);

# The R output
summary(mod);





# The underlying model
beta <- as.matrix(fixed.effects(mod));
uu   <- as.matrix(random.effects(mod));
ee   <- as.matrix(residuals(mod));

X    <- cbind(rep(x = 1, times = dim(dung)[1]), dung$flies);
Z    <- matrix(data = 0, nrow = dim(dung)[1], ncol = dim(uu)[1]);

for(i in 1:dim(Z)[1]){
  Z[i, dung$oven_block[i]] <- 1;
}

Y <- (X %*% beta) + (Z %*% uu) + ee;

# Compare the two
cbind(dung$pr_dung, Y);

