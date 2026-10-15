# POWER ANALYSIS: A QUICK DEMO FOR PSYCHOLOGY RESEARCH
# Run section by section in RStudio. No external dataset is needed.
#
# Power = P(reject H0 | a specified population effect and study design).
# Planning questions: required N, power at a fixed N, or detectable effect size.
# All effects below are teaching examples, not recommended research defaults.

# Run once if these packages are not installed
# install.packages(c("pwr", "lme4", "simr"))
library(pwr)
library(lme4)
library(simr)

alpha <- 0.05
target_power <- 0.80
nsim_demo <- 100

# Basic power analysis with pwr package
# These are convenient for simple designs.

# Independent t-test
pwr.t.test(d = 0.50, power = 0.80, sig.level = 0.05,
           type = "two.sample", alternative = "two.sided")

# Paired t-test
pwr.t.test(d = 0.50, power = 0.80, sig.level = 0.05,
           type = "paired", alternative = "two.sided")

# For a one-sample t-test, use type = "one.sample".

# Other questions: power at a fixed N, and sensitivity at a fixed N.
pwr.t.test(n = 50, d = 0.50, sig.level = .05,
           type = "two.sample", alternative = "two.sided")

pwr.t.test(n = 50, power = 0.80, sig.level = .05,
           type = "two.sample", alternative = "two.sided")

# "Manual" power curves
n_seq <- seq(10, 250, by = 10)
d_seq <- c(0.2, 0.5, 0.8)
grid <- expand.grid(n = n_seq, d = d_seq)
grid$power <- mapply(function(n, d) {
  pwr.t.test(n = n, d = d, sig.level = .05,
             type = "two.sample", alternative = "two.sided")$power}, grid$n, grid$d)

grid$d <- factor(grid$d, levels = c(0.2, 0.5, 0.8))

CurveColors <- c("firebrick", "steelblue", "sandybrown")
plot(grid$n, grid$power, type = "p",
     col = CurveColors[grid$d],
     pch = 19,
     xlab = "Sample Size", ylab = "Power", 
     main = "")
legend("bottomright", 
       legend = levels(grid$d), 
       col = CurveColors, 
       pch = 19)
abline(h = 0.8, col = "purple", lty = 4, lwd = 3)
abline(h = 0.95, col = "plum", lty = 4, lwd = 3)

# One-way between-grou ANOVA
# Cohen's f = SD of group means to grand mean / within-group SD.
# Or f = sqrt(eta2/(1-eta2))
pwr.anova.test(k = 3, f = 0.25, power = 0.80, sig.level = 0.05)

# Correlation
p_cor <- pwr.r.test(r = 0.30, power = 0.8, sig.level = 0.05, alternative = "two.sided")

# Multiple regression
# f2 = R2 / (1 - R2)
pwr.f2.test(u = 3, f2 = 0.15, power = 0.8, sig.level = 0.05)

# For model comparison based power analysis
# u = number of ADDED slopes;
# f^2 = (R^2_full - R^2_reduced) / (1 - R^2_full)




# Simulation based on hypothetical effects or pilot data with simr

# Simulation recipe:
# 1. Specify a population model, design, and hypothesis test.
# 2. Generate new responses and random effects under those assumptions.
# 3. Refit the model and test the effect in each simulated study.
# 4. Estimate power as the proportion of significant tests.

target_n <- 20

simDT <- data.frame(
  id = 1:target_n,
  group = rep(c("C", "E"), each = target_n/2),
  score = rnorm(target_n, mean = 10, sd = 5)
)

simLM <- lm(score ~ group, data = simDT)

coef(simLM) <- c("(Intercept)" = 10, "groupE" = 3)

sigma(simLM) <- 5

powerSim(simLM, nsim = 100)
powerSim(simLM,  test = fixed("groupE", method = "t"), nsim = 100)

model_max_n <- extend(simLM, along = "id", n = 400)

extended_data <- getData(model_max_n)
table(extended_data$group)

curve_apriori <- powerCurve(model_max_n,
                            test = fixed("groupE", method = "t"),
                            along = "id",
                            breaks = seq(from = 20, to = 400, by = 20),
                            nsim = 100, alpha = 0.05,
                            seed = 42)
curve_apriori
plot(curve_apriori)
abline(h = 0.95, col = "red", lty = 4, lwd = 3)

