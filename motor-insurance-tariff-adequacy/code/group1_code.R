#  Simulation Methods in Finance and Insurance                                

#### Setup ####

# install.packages(c("fitdistrplus", "MASS", "ggplot2", "dplyr", "actuar"))

library(fitdistrplus)
library(MASS)
library(ggplot2)
library(dplyr)
library(actuar)
library(gridExtra)

# Reproducibility
set.seed(123)
if (!file.exists("DATA_SET_1.csv")) 
  stop("DATA_SET_1.csv not found in the working directory. Please place the file in: ", getwd())

#### 1 - Data import and cleaning ####

# Read raw data as character so we can clean the columns ourselves
df_raw <- read.csv("DATA_SET_1.csv", sep = ";", header = TRUE,
                   stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                   check.names = FALSE)

# Remove possible whitespace from column names
colnames(df_raw) <- trimws(colnames(df_raw))

# Cleaning
clean_numeric <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\s+", "", x)        # remove spaces (thousand separator)
  x <- gsub(",", "", x)           # remove possible commas
  x[x == "-" | x == ""] <- NA     # treat "-" / "" as NA
  as.numeric(x)
}

amount_cols <- c("CLM_AMT_1", "CLM_AMT_2", "CLM_AMT_3",
                 "CLM_AMT_4", "CLM_AMT_5", "CLM_AMT_6")

df <- df_raw
for (cc in c(amount_cols, "PREMIUM")) df[[cc]] <- clean_numeric(df[[cc]])

# Convert categorical variables to factors
df$CAR_USE  <- factor(df$CAR_USE)
df$CAR_TYPE <- factor(df$CAR_TYPE)
df$GENDER   <- factor(df$GENDER)
df$AREA     <- factor(df$AREA)

# Quick structure inspection
cat("=== Raw data overview ===\n")
cat("Rows:", nrow(df), "  Columns:", ncol(df), "\n\n")
print(summary(df[, c("CLM_FREQ", "PREMIUM", "FEES", "AGE")]))

# ---- Cleaning rules for claim amounts ---------------------------------------
# Data contains 2 negative entries (-500 and -10) that are clearly errors 
# one extreme value (50,000) that is more than 50 times the second-largest claim
# We exclude them from the modelling data set

# Long-form table with one row per claim
claims_long <- do.call(rbind, lapply(amount_cols, function(cc) {
  data.frame(id = df$id, col = cc, amount = df[[cc]])
}))
claims_long <- claims_long[!is.na(claims_long$amount), ]

cat("\n=== Claim amount diagnostics ===\n")
cat("Total claim records :", nrow(claims_long), "\n")
cat("Negative amounts    :", sum(claims_long$amount < 0), "\n")
cat("Amounts >= 5000     :", sum(claims_long$amount >= 5000), "\n")
cat("Range of full data  :", range(claims_long$amount), "\n")

# Apply the cleaning rule to the long-form table
claims_long_clean <- claims_long[claims_long$amount > 0 &
                                 claims_long$amount < 5000, ]
claim_amounts <- claims_long_clean$amount     # vector of severities to model

# Recompute, on the wide table, the *consistent* cleaned frequency / total
clean_amount <- function(x) ifelse(!is.na(x) & x > 0 & x < 5000, x, NA)
df$tot_claims <- rowSums(sapply(amount_cols, function(cc) clean_amount(df[[cc]])),
                        na.rm = TRUE)
df$n_claims_clean <- rowSums(!is.na(sapply(amount_cols, function(cc) clean_amount(df[[cc]]))))

clm_count <- df$n_claims_clean   # cleaned number of claims per policy

cat("Clean severity records:", length(claim_amounts), "\n")
cat("Mean severity         :", round(mean(claim_amounts), 2), "\n")
cat("SD severity           :", round(sd(claim_amounts), 2), "\n\n")


#### 2 - Exploratory data analysis ####

cat("=== Exploratory data analysis ===\n")
cat("Frequency distribution (number of policies by claim count):\n")
print(table(clm_count))

cat("\nMean of CLM_FREQ :", round(mean(clm_count), 4), "\n")
cat("\nVariance of CLM_FREQ :", round(var(clm_count), 4), "\n")
cat("Variance / mean ratio        :", round(var(clm_count)/mean(clm_count), 4))

# close to 1 -> maybe Poisson

# Bar plot of claim counts (ggplot, as suggested in the case study brief)
p1 <- ggplot(data.frame(n = clm_count), aes(x = factor(n))) +
        geom_bar(fill = "steelblue", colour = "black") +
        labs(x = "Number of claims (CLM_FREQ)",
             y = "Number of policies",
             title = "Distribution of claim counts (after cleaning)") +
        theme_minimal()


# Histogram of severity
p2 <- ggplot(data.frame(amount = claim_amounts), aes(x = amount)) +
        geom_histogram(fill = "lightblue", colour = "black", bins = 40) +
        labs(x = "Claim amount", y = "Frequency",
             title = "Histogram of claim severity") +
        theme_minimal()

# pie chart of car type distribution
car_type_counts <- as.data.frame(table(df$CAR_TYPE))
colnames(car_type_counts) <- c("Type", "Count")
car_type_counts$Pct <- round(car_type_counts$Count / sum(car_type_counts$Count) * 100, 1)
car_type_counts$Label <- paste0(car_type_counts$Type, "\n", car_type_counts$Pct, "%")

p3 <- ggplot(car_type_counts, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y") +
  labs(title = "Distribution of car types", fill = "CAR_TYPE") +
  geom_text(aes(label = paste0(Pct, "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# pie chart of area distribution
area_counts <- as.data.frame(table(df$AREA))
colnames(area_counts) <- c("Area", "Count")
area_counts$Pct <- round(area_counts$Count / sum(area_counts$Count) * 100, 1)
area_counts$Label <- paste0(area_counts$Area, "\n", area_counts$Pct, "%")

p4 <- ggplot(area_counts, aes(x = "", y = Count, fill = Area)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y") +
  scale_fill_manual(values = c("Urban" = "tomato", "Rural" = "seagreen")) +
  labs(title = "Urban vs Rural distribution", fill = "AREA") +
  geom_text(aes(label = paste0(Pct, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# pie chart car use
urban_use <- as.data.frame(table(df$CAR_USE[df$AREA == "Urban"]))
rural_use  <- as.data.frame(table(df$CAR_USE[df$AREA == "Rural"]))
colnames(urban_use) <- colnames(rural_use) <- c("Use", "Count")

urban_use$Pct <- round(urban_use$Count / sum(urban_use$Count) * 100, 2)
rural_use$Pct  <- round(rural_use$Count  / sum(rural_use$Count)  * 100, 2)

p5 <- ggplot(urban_use, aes(x = "", y = Count, fill = Use)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(Pct, "%")),
            position = position_stack(vjust = 0.5), size = 4, colour = "black") +
  labs(title = "URBAN", fill = "CAR_USE") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p6 <- ggplot(rural_use, aes(x = "", y = Count, fill = Use)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(Pct, "%")),
            position = position_stack(vjust = 0.5), size = 4, colour = "black") +
  labs(title = "Rural", fill = "CAR_USE") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Save the two plots
ggsave("plot_freq_bar.png", p1, width = 6, height = 4)
ggsave("plot_severity_hist.png", p2, width = 6, height = 4)
ggsave("plot_car_type_pie.png", p3, width = 6, height = 5)
ggsave("plot_area_pie.png", p4, width = 6, height = 5)
ggsave("plot_caruse_urban.png", p5, width = 5, height = 5)
ggsave("plot_caruse_rural.png", p6, width = 5, height = 5)


#### 3 - Frequency model selection ####

# Test 3 candidate distributions
# Poisson, Negative Binomial and Geometric. 
# comparing them with AIC (smaller=better) and a chi-square test (chapter 2)

cat("=== Frequency model selection ===\n")

fit_pois <- fitdist(clm_count, "pois", method = "mle")
fit_nb   <- fitdist(clm_count, "nbinom", method = "mle")
fit_geom <- fitdist(clm_count, "geom", method = "mle")

cat("Poisson  : lambda =", round(fit_pois$estimate, 4),
    "  AIC =", round(fit_pois$aic, 2), "\n")
cat("Neg.Bin. : size   =", round(fit_nb$estimate["size"], 3),
    " mu =", round(fit_nb$estimate["mu"], 4),
    "  AIC =", round(fit_nb$aic, 2), "\n")
cat("Geometric: prob   =", round(fit_geom$estimate, 4),
    "  AIC =", round(fit_geom$aic, 2), "\n")
# Poisson has the smaller AIC
# Poisson also smaller BIC 

# Chi-square for Poisson
lambda_hat <- as.numeric(fit_pois$estimate)
n_obs      <- length(clm_count)
obs_table  <- as.numeric(table(factor(clm_count, levels = 0:6)))
exp_table  <- dpois(0:6, lambda_hat) * n_obs
exp_table[7] <- exp_table[7] + (1 - ppois(6, lambda_hat)) * n_obs
# Combine cells if expected count < 5
obs_grouped <- c(obs_table[1:5], sum(obs_table[6:7]))
exp_grouped <- c(exp_table[1:5], sum(exp_table[6:7]))
chi_stat    <- sum((obs_grouped - exp_grouped)^2 / exp_grouped)
df_chi      <- length(obs_grouped) - 1 - 1  # degrees of freedom: k-1-m
chi_p       <- pchisq(chi_stat, df_chi, lower.tail = FALSE)
cat("\nChi-square for Poisson : stat =", round(chi_stat, 4),
    "  df =", df_chi, "  p =", round(chi_p, 4), "\n")

# Visual diagnostics for Poisson
plot(fit_pois)
pdf("plot_freq_diagnostics.pdf", width = 7, height = 5); plot(fit_pois); dev.off()

# Chosen model: Poisson


#### 4 - Severity model selection ####

# Candidates: Gamma, Lognormal, Weibull, Normal
cat("\n=== Severity model selection ===\n")

fit_gamma   <- fitdist(claim_amounts, "gamma",   method = "mle")
fit_lnorm   <- fitdist(claim_amounts, "lnorm",   method = "mle")
fit_weibull <- fitdist(claim_amounts, "weibull", method = "mle")
fit_norm    <- fitdist(claim_amounts, "norm",    method = "mle")
# exponential
rate_exp_hat <- 1 / mean(claim_amounts)
ll_exp       <- sum(dexp(claim_amounts, rate = rate_exp_hat, log = TRUE))
aic_exp      <- -2 * ll_exp + 2 * 1   # 1 estimated parameter
fit_exp <- list(estimate = c(rate = rate_exp_hat),
                aic      = aic_exp,
                loglik   = ll_exp)
# Pareto
mu_sev  <- mean(claim_amounts)
var_sev <- var(claim_amounts)
alpha0  <- 2 * var_sev / (var_sev - mu_sev^2)
if (!is.finite(alpha0) || alpha0 <= 2) alpha0 <- 5   # safe fallback
theta0  <- mu_sev * (alpha0 - 1)
fit_pareto <- fitdist(claim_amounts, "pareto",
                      method = "mle",
                      start  = list(shape = alpha0, scale = theta0))

aic_table <- data.frame(
  model = c("Gamma", "Lognormal", "Weibull", "Normal", "Exponential", "Pareto"),
  AIC   = c(fit_gamma$aic, fit_lnorm$aic, fit_weibull$aic,
            fit_norm$aic, fit_exp$aic, fit_pareto$aic)
)
print(aic_table) # gamma lowest, then normal

# Kolmogorov-Smirnov goodness-of-fit test for each candidate
ks_gamma <- suppressWarnings(ks.test(claim_amounts, "pgamma",
                                     shape = fit_gamma$estimate["shape"],
                                     rate  = fit_gamma$estimate["rate"]))
ks_lnorm <- suppressWarnings(ks.test(claim_amounts, "plnorm",
                                     meanlog = fit_lnorm$estimate["meanlog"],
                                     sdlog   = fit_lnorm$estimate["sdlog"]))
ks_norm  <- suppressWarnings(ks.test(claim_amounts, "pnorm",
                                     mean = fit_norm$estimate["mean"],
                                     sd   = fit_norm$estimate["sd"]))

cat("\nK-S test p-values:\n")
cat("  Gamma     :", round(ks_gamma$p.value, 4), "\n")
cat("  Lognormal :", round(ks_lnorm$p.value, 4), "\n")
cat("  Normal    :", round(ks_norm$p.value, 4),  "\n")
# Chosen model: Gamma 
# also Normal good candidate (really similar), but gamma has positive values

# Diagnostic plots
plot(fit_gamma)
pdf("plot_sev_diagnostics.pdf", width = 8, height = 7); plot(fit_gamma); dev.off()

# Save the parameters
shape_g <- as.numeric(fit_gamma$estimate["shape"])
rate_g  <- as.numeric(fit_gamma$estimate["rate"])
scale_g <- 1 / rate_g


#### 5 - Monte Carlo estimation (Frequency) ####

# We simulate a large number of Poisson(lambda_hat) draws and study the
# convergence of the running mean to the theoretical expectation lambda_hat

cat("\n=== Monte Carlo estimation – frequency ===\n")
N <- 100000

set.seed(123)
freq_sim <- rpois(N, lambda_hat)

mc_freq_mean <- mean(freq_sim)
mc_freq_var  <- var(freq_sim)
mc_freq_se   <- sd(freq_sim) / sqrt(N)
# SE of the mean estimator
mc_freq_se_theoretical <- sqrt(lambda_hat / N)

cat("MC mean      :", round(mc_freq_mean, 4),
    "  (empirical mean was", round(mean(clm_count), 4), ")\n")
cat("MC variance  :", round(mc_freq_var, 4),
    "  (theoretical lambda =", round(lambda_hat, 4), ")\n")
cat("Empirical SE :", format(mc_freq_se, scientific = TRUE),  "\n")
cat("Theoretical SE:", format(mc_freq_se_theoretical, scientific = TRUE), "\n")
# estimator is unbiased
cat("MSE          :", format(mc_freq_var/N, scientific = TRUE), "\n")

# Convergence plot
running_mean_freq <- cumsum(freq_sim) / seq_along(freq_sim)
plot(running_mean_freq, type = "l", col = "steelblue", lwd = 0.8,
     xlab = "Number of simulations n", ylab = "Running mean",
     main = "Convergence of the MC frequency estimator")
abline(h = lambda_hat, col = "red", lty = 2, lwd = 2)
legend("topright",
       legend = c("Running mean",
                  paste0("MLE lambda = ", round(lambda_hat, 4))),
       col = c("steelblue", "red"), lty = c(1, 2))
pdf("plot_freq_convergence.pdf", width = 7, height = 4)
  plot(running_mean_freq, type = "l", col = "steelblue", lwd = 0.8,
       xlab = "Number of simulations n", ylab = "Running mean",
       main = "Convergence of the MC frequency estimator")
  abline(h = lambda_hat, col = "red", lty = 2, lwd = 2)
  legend("topright",
         legend = c("Running mean",
                    paste0("MLE lambda = ", round(lambda_hat, 4))),
         col = c("steelblue", "red"), lty = c(1, 2))
dev.off()


#### 6 - Monte Carlo estimation (Severity) ####

cat("\n=== Monte Carlo estimation – severity (Gamma) ===\n")

set.seed(123)
sev_sim <- rgamma(N, shape = shape_g, rate = rate_g)

mc_sev_mean <- mean(sev_sim)
mc_sev_var  <- var(sev_sim)
mc_sev_se   <- sd(sev_sim) / sqrt(N)
mc_sev_var_theo <- shape_g * scale_g^2
mc_sev_se_theo  <- sqrt(mc_sev_var_theo / N)

cat("MC mean       :", round(mc_sev_mean, 4),
    "  (empirical mean =", round(mean(claim_amounts), 4), ")\n")
cat("MC variance   :", round(mc_sev_var, 4),
    "  (theoretical =", round(mc_sev_var_theo, 4), ")\n")
cat("Empirical SE  :", round(mc_sev_se, 5), "\n")
cat("Theoretical SE:", round(mc_sev_se_theo, 5), "\n")

# estimator is unbiased
cat("MSE          :", format(mc_sev_var/N, scientific = TRUE), "\n")

# Convergence plot
running_mean_sev <- cumsum(sev_sim) / seq_along(sev_sim)
plot(running_mean_sev, type = "l", col = "steelblue", lwd = 0.8,
     xlab = "Number of simulations n", ylab = "Running mean",
     main = "Convergence of the MC severity estimator (Gamma)")
abline(h = mean(claim_amounts), col = "red", lty = 2, lwd = 2)
pdf("plot_sev_convergence.pdf", width = 7, height = 4)
  plot(running_mean_sev, type = "l", col = "steelblue", lwd = 0.8,
       xlab = "Number of simulations n", ylab = "Running mean",
       main = "Convergence of the MC severity estimator (Gamma)")
  abline(h = mean(claim_amounts), col = "red", lty = 2, lwd = 2)
dev.off()


#### 7 - Variance reduction techniques ####

cat("\n=== Variance reduction techniques ===\n")

# Antithetic for frequency
set.seed(123)
n_anti <- N / 2
U      <- runif(n_anti)
freq_anti1 <- qpois(U,     lambda_hat)
freq_anti2 <- qpois(1 - U, lambda_hat)
freq_anti  <- (freq_anti1 + freq_anti2) / 2

var_crude_freq_mean <- var(freq_sim) / N             # variance of crude estimator (mean)
var_anti_freq_mean  <- var(freq_anti) / n_anti       # variance of antithetic estimator (mean)
red_freq <- 1 - var_anti_freq_mean / var_crude_freq_mean
corr_freq_anti <- cor(freq_anti1, freq_anti2)

cat("Antithetic frequency:\n")
cat("   Var(crude mean estimator)  :", format(var_crude_freq_mean, scientific = TRUE), "\n")
cat("   Var(anti  mean estimator)  :", format(var_anti_freq_mean,  scientific = TRUE), "\n")
cat("   Variance reduction         :", round(red_freq * 100, 2), "%\n")
cat("   Correlation Cor(f(U),f(1-U)):", round(corr_freq_anti, 4), "\n")

# Antithetic for severity 
set.seed(123)
U <- runif(n_anti)
sev_anti1 <- qgamma(U,     shape = shape_g, rate = rate_g)
sev_anti2 <- qgamma(1 - U, shape = shape_g, rate = rate_g)
sev_anti  <- (sev_anti1 + sev_anti2) / 2

var_crude_sev_mean <- var(sev_sim) / N
var_anti_sev_mean  <- var(sev_anti) / n_anti
red_sev_anti       <- 1 - var_anti_sev_mean / var_crude_sev_mean
corr_sev_anti      <- cor(sev_anti1, sev_anti2)

cat("\nAntithetic severity:\n")
cat("   Var(crude mean estimator) :", format(var_crude_sev_mean, scientific = TRUE), "\n")
cat("   Var(anti  mean estimator) :", format(var_anti_sev_mean,  scientific = TRUE), "\n")
cat("   Variance reduction        :", round(red_sev_anti * 100, 2), "%\n")
cat("   Correlation Cor(f(U),f(1-U)):", round(corr_sev_anti, 4), "\n")

# Control variate for severity
# We use the Normal RV obtained from the same uniform, because the
# fitted Gamma distribution is essentially symmetric (shape ≈ 183)
set.seed(123)
U <- runif(N)
X <- qgamma(U, shape = shape_g, rate = rate_g)        # severity draws
mu_n <- mean(claim_amounts)
sd_n <- sd(claim_amounts)
Y <- qnorm(U, mean = mu_n, sd = sd_n)                  # control variate

# Optimal regression coefficient
b_opt <- cov(X, Y) / var(Y)
X_cv  <- X - b_opt * (Y - mu_n)

var_cv_sev_mean    <- var(X_cv) / N
red_sev_cv         <- 1 - var_cv_sev_mean / var_crude_sev_mean
corr_sev_cv        <- cor(X, Y)

cat("\nControl variate severity (Normal control):\n")
cat("   Var(CV  mean estimator)   :", format(var_cv_sev_mean,    scientific = TRUE), "\n")
cat("   Var(crude  mean estimator) :", format(var_crude_sev_mean,  scientific = TRUE), "\n")
cat("   Variance reduction        :", round(red_sev_cv * 100, 2), "%\n")
cat("   Correlation Cor(X,Y)      :", round(corr_sev_cv, 4), "\n")


#### 8 - Aggregate loss distribution and risk premium (Compound Poisson) ####

# S = sum_{j=1}^{N} Y_j with N ~ Poisson and Y_j ~ Gamma

cat("\n=== Aggregate loss distribution and risk premium ===\n")

set.seed(123)
N_sim   <- 100000
ns      <- rpois(N_sim, lambda_hat)
total_n <- sum(ns)
all_y   <- rgamma(total_n, shape = shape_g, rate = rate_g)
total_S <- numeric(N_sim)
idx     <- 1
for (i in seq_len(N_sim)) {
  k <- ns[i]
  if (k > 0) {
    total_S[i] <- sum(all_y[idx:(idx + k - 1)])
    idx <- idx + k
  }
}

for (i in seq_len(N_sim)) {
  k <- ns[i]
  if (k > 0) {
    total_S[i] <- sum(rgamma(k, shape = shape_g, rate = rate_g))
    idx <- idx + k
  }
}

# moments of S: E[S] = E[N]*E[Y], Var[S] = E[N]*E[Y^2]
EY  <- shape_g * scale_g
EY2 <- shape_g * (shape_g + 1) * scale_g^2
ES_theo  <- lambda_hat * EY
VS_theo  <- lambda_hat * EY2

cat("E[S] simulated   :", round(mean(total_S), 4), "\n")
cat("E[S] theoretical :", round(ES_theo, 4), "\n")
cat("Var[S] simulated :", round(var(total_S), 2), "\n")
cat("Var[S] theoretical:", round(VS_theo, 2), "\n")

# Risk premium = total claim amount / # of policy holders
risk_premium_data <- sum(df$tot_claims) / nrow(df)
risk_premium_mc   <- mean(total_S)

cat("Risk premium (data) :", round(risk_premium_data, 4), "\n")
cat("Risk premium (MC)   :", round(risk_premium_mc,   4), "\n")
cat("Mean premium paid    :", round(mean(df$PREMIUM), 4), "\n")
cat("Mean fees           :", round(mean(df$FEES), 4), "\n")
cat("Margin per policy   :",
    round(mean(df$PREMIUM) - risk_premium_mc - mean(df$FEES), 4), "\n")
# margin = avg_premium - risk_premium - avg_fees
cat("Margin (% of premium):",
    round((mean(df$PREMIUM) - risk_premium_mc - mean(df$FEES))/mean(df$PREMIUM)*100, 2),
    "%\n")

# Aggregate loss distribution plot
hist(total_S, breaks = 80, freq = FALSE,
     col = "lightblue", border = "black",
     xlab = "Aggregate claim amount per policy S",
     main = "Simulated aggregate loss distribution")
abline(v = mean(total_S), col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste0("E[S] = ", round(mean(total_S), 1)),
       col = "red", lty = 2)
pdf("plot_aggregate.pdf", width = 7, height = 4.5)
  hist(total_S, breaks = 80, freq = FALSE,
       col = "lightblue", border = "black",
       xlab = "Aggregate claim amount per policy S",
       main = "Simulated aggregate loss distribution")
  abline(v = mean(total_S), col = "red", lwd = 2, lty = 2)
  legend("topright", legend = paste0("E[S] = ", round(mean(total_S), 1)),
         col = "red", lty = 2)
dev.off()


#### 9 - Solvency II (Value-at-Risk at 99.5%) ####

#   Method 1: empirical quantile of simulated S at level 0.995
#   Method 2: Normal approximation E[S] + z(0.995) * SD(S)

cat("\n=== VaR (Solvency II at 99.5%) ===\n")

# Method 1
VaR_995_per_policy <- as.numeric(quantile(total_S, 0.995))
VaR_99_per_policy  <- as.numeric(quantile(total_S, 0.99))

# Method 2
mu_S <- mean(total_S); sd_S <- sd(total_S)
VaR_995_normal     <- mu_S + qnorm(0.995) * sd_S

cat("VaR 99%   – Method 1 (empirical quantile)   :", round(VaR_99_per_policy,  2), "\n")
cat("VaR 99.5% – Method 1 (empirical quantile)   :", round(VaR_995_per_policy, 2), "\n")
cat("VaR 99.5% – Method 2 (normal approximation) :", round(VaR_995_normal,     2), "\n")
# better method 1, no normal approx needed and also underestimates the tail
# S is right skewed

# Aggregate portfolio simulation
# total portfolio loss is itself compound-Poisson
N_pol <- nrow(df)
set.seed(123)
N_sim_pf <- 100000
pf_total <- numeric(N_sim_pf)
for (s in seq_len(N_sim_pf)) {
  k_tot <- rpois(1, N_pol * lambda_hat)
  if (k_tot > 0) pf_total[s] <- sum(rgamma(k_tot, shape = shape_g, rate = rate_g))
}

E_pf       <- mean(pf_total)
VaR_pf_995 <- as.numeric(quantile(pf_total, 0.995))
SCR        <- VaR_pf_995 - E_pf

cat("\nPortfolio level:\n")
cat("  E[total losses]    :", round(E_pf, 0), "\n")
cat("  VaR 99.5% (total)  :", round(VaR_pf_995, 0), "\n")
cat("  SCR = VaR - E[S]   :", round(SCR, 0), "\n")


#### 10 - Combined ratio, tariff adequacy and reinsurance ####

# Combined ratio = (total claims + total admin costs) / total premium
# < 1 is good

cat("\n=== Combined ratio & tariff adequacy ===\n")

total_premium <- sum(df$PREMIUM)
total_claims  <- sum(df$tot_claims)
total_fees    <- sum(df$FEES)
combined_ratio <- (total_claims + total_fees) / total_premium

cat("Total premium :", format(total_premium, big.mark=","), "\n")
cat("Total claims  :", format(total_claims,  big.mark=","), "\n")
cat("Total fees    :", format(total_fees,    big.mark=","), "\n")
cat("Combined ratio:", round(combined_ratio, 4))
# profitable

max_red_pct <- (1 - combined_ratio) * 100
# max tariff reduction with ratio smaller than 1


#### 11 - Sensitivity analysis (impact of a tariff reduction) ####

# Function to calculate combined ratio

calc_combined_ratio <- function(data, label){
  total_premium <- sum(data$PREMIUM, na.rm = TRUE)
  total_claims  <- sum(data$tot_claims, na.rm = TRUE)
  total_fees    <- sum(data$FEES, na.rm = TRUE)
  
  combined_ratio <- (total_claims + total_fees) / total_premium
  
  cat("\n==============================\n")
  cat("Portfolio:", label, "\n")
  cat("==============================\n")
  
  cat("Total premium :", format(total_premium, big.mark=","), "\n")
  cat("Total claims  :", format(total_claims,  big.mark=","), "\n")
  cat("Total fees    :", format(total_fees,    big.mark=","), "\n")
  cat("Combined ratio:", round(combined_ratio, 4), "\n")
  
  if(combined_ratio < 1){
    cat("Status: Profitable\n")
  } else {
    cat("Status: Unprofitable\n")
  }
  
  red_grid <- seq(0, 10, by = 1) / 100
  
  cr_grid <- (total_claims + total_fees) /
    (total_premium * (1 - red_grid))
  
  res <- data.frame(
    reduction_pct = red_grid * 100,
    combined_ratio = round(cr_grid, 4),
    status = ifelse(cr_grid < 1,
                    "Profitable",
                    "Unprofitable")
  )
  
  cat("\nSensitivity analysis:\n")
  print(res)
  
  return(res)
}

# Full portfolio
res_all <- calc_combined_ratio(df, "All")

# SUV
df_SUV <- subset(df, CAR_TYPE == "SUV")
res_SUV <- calc_combined_ratio(df_SUV, "SUV")
# Non-SUV
df_non_SUV <- subset(df, !(CAR_TYPE %in% c("SUV")))
res_non_SUV <- calc_combined_ratio(df_non_SUV, "Non-SUV")


#### EXTRA - Empirical vs fitted distributions plots ####

# ---- (a) Severity: empirical density vs all fitted candidates ---------------
# Parametri stimati (già calcolati nella sezione 4)
shape_ga <- as.numeric(fit_gamma$estimate["shape"])
rate_ga  <- as.numeric(fit_gamma$estimate["rate"])
mlog_ln  <- as.numeric(fit_lnorm$estimate["meanlog"])
slog_ln  <- as.numeric(fit_lnorm$estimate["sdlog"])
shape_wb <- as.numeric(fit_weibull$estimate["shape"])
scale_wb <- as.numeric(fit_weibull$estimate["scale"])
mu_no    <- as.numeric(fit_norm$estimate["mean"])
sd_no    <- as.numeric(fit_norm$estimate["sd"])
rate_ex  <- as.numeric(fit_exp$estimate["rate"])
shape_pa <- as.numeric(fit_pareto$estimate["shape"])
scale_pa <- as.numeric(fit_pareto$estimate["scale"])

# Griglia su cui valutare le densità
x_grid <- seq(min(claim_amounts), max(claim_amounts), length.out = 500)

dens_df <- data.frame(
  x         = x_grid,
  Gamma     = dgamma(x_grid,   shape = shape_ga, rate = rate_ga),
  Lognormal = dlnorm(x_grid,   meanlog = mlog_ln, sdlog = slog_ln),
  Weibull   = dweibull(x_grid, shape = shape_wb, scale = scale_wb),
  Normal    = dnorm(x_grid,    mean = mu_no, sd = sd_no),
  Exponential = dexp(x_grid,   rate = rate_ex),
  Pareto    = actuar::dpareto(x_grid, shape = shape_pa, scale = scale_pa)
)

# Da wide a long per ggplot
dens_long <- reshape(dens_df, varying = list(2:ncol(dens_df)),
                     v.names = "density",
                     timevar = "Distribution",
                     times   = colnames(dens_df)[-1],
                     direction = "long")

p_sev_fit <- ggplot() +
  geom_histogram(data = data.frame(amount = claim_amounts),
                 aes(x = amount, y = after_stat(density)),
                 bins = 40, fill = "grey85", colour = "grey40") +
  geom_line(data = dens_long,
            aes(x = x, y = density,
                colour = Distribution, linetype = Distribution),
            linewidth = 0.9) +
  scale_colour_manual(values = c(
    "Gamma"       = "blue",
    "Lognormal"   = "red",
    "Weibull"     = "darkorange",
    "Normal"      = "darkgreen",
    "Exponential" = "purple",
    "Pareto"      = "brown")) +
  scale_linetype_manual(values = c(
    "Gamma"       = "solid",
    "Lognormal"   = "dotted",
    "Weibull"     = "dashed",
    "Normal"      = "longdash",
    "Exponential" = "dotdash",
    "Pareto"      = "twodash")) +
  labs(title = "Empirical density of claim severity vs fitted distributions",
       x = "Claim amount", y = "Density") +
  theme_minimal() +
  theme(legend.position = "right")

print(p_sev_fit)
ggsave("plot_severity_fits.png", p_sev_fit, width = 8, height = 5)


# ---- (b) Frequency: empirical PMF vs all fitted candidates ------------------
# Parametri stimati (già calcolati nella sezione 3)
lambda_p <- as.numeric(fit_pois$estimate["lambda"])
size_nb  <- as.numeric(fit_nb$estimate["size"])
mu_nb    <- as.numeric(fit_nb$estimate["mu"])
prob_geo <- as.numeric(fit_geom$estimate["prob"])

k_grid <- 0:max(clm_count)

# Frequenze empiriche (proporzioni)
emp_freq <- as.numeric(table(factor(clm_count, levels = k_grid))) / length(clm_count)

pmf_df <- data.frame(
  k          = k_grid,
  Empirical  = emp_freq,
  Poisson    = dpois(k_grid,   lambda = lambda_p),
  NegBinom   = dnbinom(k_grid, size = size_nb, mu = mu_nb),
  Geometric  = dgeom(k_grid,   prob = prob_geo)
)

# Plot: barre = empirico, punti+linea = distribuzioni fittate
pmf_long_fit <- reshape(pmf_df[, c("k", "Poisson", "NegBinom", "Geometric")],
                        varying = list(2:4),
                        v.names = "prob",
                        timevar = "Distribution",
                        times   = c("Poisson", "NegBinom", "Geometric"),
                        direction = "long")

p_freq_fit <- ggplot() +
  geom_col(data = pmf_df, aes(x = k, y = Empirical),
           fill = "grey80", colour = "grey40", width = 0.7) +
  geom_point(data = pmf_long_fit,
             aes(x = k, y = prob, colour = Distribution, shape = Distribution),
             size = 3) +
  geom_line(data = pmf_long_fit,
            aes(x = k, y = prob, colour = Distribution, linetype = Distribution),
            linewidth = 0.8) +
  scale_colour_manual(values = c("Poisson"  = "blue",
                                 "NegBinom" = "red",
                                 "Geometric"= "darkgreen")) +
  scale_linetype_manual(values = c("Poisson"  = "solid",
                                   "NegBinom" = "dashed",
                                   "Geometric"= "dotted")) +
  scale_shape_manual(values = c("Poisson" = 16, "NegBinom" = 17, "Geometric" = 15)) +
  scale_x_continuous(breaks = k_grid) +
  labs(title = "Empirical claim frequency vs fitted distributions",
       x = "Number of claims", y = "Probability") +
  theme_minimal() +
  theme(legend.position = "right")

print(p_freq_fit)
ggsave("plot_frequency_fits.png", p_freq_fit, width = 8, height = 5)


# ---- (c) CDF empirica vs fitted (severity) — utile per discutere il KS -----
ecdf_sev <- ecdf(claim_amounts)
cdf_df <- data.frame(
  x         = x_grid,
  Gamma     = pgamma(x_grid,   shape = shape_ga, rate = rate_ga),
  Lognormal = plnorm(x_grid,   meanlog = mlog_ln, sdlog = slog_ln),
  Weibull   = pweibull(x_grid, shape = shape_wb, scale = scale_wb),
  Normal    = pnorm(x_grid,    mean = mu_no, sd = sd_no),
  Exponential = pexp(x_grid,   rate = rate_ex),
  Pareto    = actuar::ppareto(x_grid, shape = shape_pa, scale = scale_pa)
)
cdf_long <- reshape(cdf_df, varying = list(2:ncol(cdf_df)),
                    v.names = "cdf",
                    timevar = "Distribution",
                    times   = colnames(cdf_df)[-1],
                    direction = "long")

p_sev_cdf <- ggplot() +
  stat_ecdf(data = data.frame(amount = claim_amounts),
            aes(x = amount), geom = "step",
            colour = "black", linewidth = 0.6) +
  geom_line(data = cdf_long,
            aes(x = x, y = cdf, colour = Distribution, linetype = Distribution),
            linewidth = 0.8) +
  labs(title = "Empirical CDF of claim severity vs fitted distributions",
       x = "Claim amount", y = "F(x)") +
  theme_minimal()

print(p_sev_cdf)
ggsave("plot_severity_cdf_fits.png", p_sev_cdf, width = 8, height = 5)

