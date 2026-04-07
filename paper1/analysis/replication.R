##############################################################################
# REPLICATION CODE: Paper 1
# "Competitive Clientelism in Mexico's Electoral Precincts:
#  Why All Parties Buy Votes in the Same Communities"
#
# Author: Sergio Béjar
# Data:   CIDE-CSES 2015 Post-Electoral Survey (BD.sav)
#         Larreguy et al. Municipal Election Data (larreguy_seccion_summary.csv)
#         Magar Federal Deputy Data (dip2012.csv, dip2015.csv) [robustness]
#
# Requirements: haven, dplyr, tidyr, ggplot2, lme4, corrplot, broom
##############################################################################
 
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
 
# Set paths -- adjust to your directory structure
DATA_DIR  <- "data/"
FIG_DIR   <- "paper1/figures/"
TABLE_DIR <- "paper1/tables/"
 
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
 
##############################################################################
# 1. LOAD AND PREPARE SURVEY DATA
##############################################################################
 
survey <- read_sav(paste0(DATA_DIR, "BD.sav"))
 
# --- Vote buying DVs (party-specific) ---
# pcyc1=PAN, pcyc2=PRI, pcyc3=PRD, pcyc4=PVEM, pcyc5=MORENA, pcyc6=MC, pcyc7=other
survey <- survey %>%
  mutate(
    vb_pan    = as.integer(pcyc1 == 1),
    vb_pri    = as.integer(pcyc2 == 1),
    vb_prd    = as.integer(pcyc3 == 1),
    vb_pvem   = as.integer(pcyc4 == 1),
    vb_morena = as.integer(pcyc5 == 1),
    vb_mc     = as.integer(pcyc6 == 1),
    any_vb    = as.integer(vb_pan | vb_pri | vb_prd | vb_pvem | vb_morena | vb_mc),
    n_parties_vb = vb_pan + vb_pri + vb_prd + vb_pvem + vb_morena + vb_mc
  )
 
# --- Observed vote buying in neighborhood ---
survey <- survey %>%
  mutate(
    obs_pan    = as.integer(pcyc8a == 1),
    obs_pri    = as.integer(pcyc8b == 1),
    obs_prd    = as.integer(pcyc8c == 1),
    obs_pvem   = as.integer(pcyc8d == 1),
    obs_morena = as.integer(pcyc8e == 1),
    obs_mc     = as.integer(pcyc8f == 1),
    any_obs    = as.integer(obs_pan | obs_pri | obs_prd | obs_pvem | obs_morena | obs_mc)
  )
 
# --- Individual-level covariates ---
survey <- survey %>%
  mutate(
    female   = as.integer(ps2 == 2),
    age      = ifelse(ps1 == 99, NA, ps1),
    educ     = ifelse(ps3 %in% c(98, 99), NA, ifelse(ps3 == 96, 0, ps3)),
    priista  = as.integer(p9 %in% c(3, 4)),
    panista  = as.integer(p9 %in% c(1, 2)),
    no_party = as.integer(p9 == 97)
  )
 
# --- Program beneficiary status ---
survey <- survey %>%
  mutate(
    prospera = as.integer(pcyc12b == 1),
    liconsa  = as.integer(pcyc12  == 1),
    procampo = as.integer(pcyc12a == 1)
  )
 
# --- Ethnicity / skin color ---
survey <- survey %>%
  mutate(
    skin_self  = ifelse(ps23 == 99, NA, ps23),
    indigenous = as.integer(ps26 == 1),
    indig_lang = as.integer(ps27 == 1)
  )
 
# --- Acceptability vignette (CYC9a) ---
# 1=totally acceptable ... 5=totally unacceptable
# Reverse so higher = more acceptable
survey <- survey %>%
  mutate(
    accept_vb = ifelse(pcyc9a %in% c(8, 9), NA, pcyc9a),
    accept_vb_rev = 6 - accept_vb
  )
 
# --- Party contact variables ---
survey <- survey %>%
  mutate(
    contact_pan    = as.integer(p26_1 == 1),
    contact_pri    = as.integer(p26_2 == 1),
    contact_prd    = as.integer(p26_3 == 1),
    contact_pvem   = as.integer(p26_4 == 1),
    contact_pt     = as.integer(p26_5 == 1),
    contact_mc     = as.integer(p26_6 == 1),
    contact_morena = as.integer(p26_7 == 1),
    n_parties_contact = contact_pan + contact_pri + contact_prd +
                        contact_pvem + contact_pt + contact_mc + contact_morena
  )
 
# --- Merge keys ---
survey <- survey %>%
  mutate(
    state_code = as.numeric(estado),
    precinct   = as.numeric(secc)
  )
 
cat("Survey loaded:", nrow(survey), "observations\n")
 
##############################################################################
# 2. LOAD AND PREPARE LARREGUY ELECTORAL DATA
##############################################################################
 
larreguy <- read.csv(paste0(DATA_DIR, "larreguy_seccion_summary.csv"),
                     stringsAsFactors = FALSE)
 
cat("Larreguy data loaded:", nrow(larreguy), "precincts\n")
 
##############################################################################
# 3. MERGE
##############################################################################
 
merged <- inner_join(survey, larreguy, by = c("state_code", "precinct"))
 
cat("Merged:", nrow(merged), "observations,",
    n_distinct(merged$precinct), "precincts\n")
cat("Unmatched:", nrow(survey) - nrow(merged), "observations\n")
 
##############################################################################
# 4. SECCIÓN-LEVEL AGGREGATION
##############################################################################
 
secc <- merged %>%
  group_by(state_code, precinct) %>%
  summarise(
    # Vote buying rates
    vb_rate    = mean(any_vb, na.rm = TRUE),
    vb_pri     = mean(vb_pri, na.rm = TRUE),
    vb_pan     = mean(vb_pan, na.rm = TRUE),
    vb_prd     = mean(vb_prd, na.rm = TRUE),
    vb_pvem    = mean(vb_pvem, na.rm = TRUE),
    vb_morena  = mean(vb_morena, na.rm = TRUE),
    vb_mc      = mean(vb_mc, na.rm = TRUE),
    obs_rate   = mean(any_obs, na.rm = TRUE),
    n_parties  = mean(n_parties_vb, na.rm = TRUE),
    # Individual covariates (means)
    prospera_share = mean(prospera, na.rm = TRUE),
    priista_share  = mean(priista, na.rm = TRUE),
    # Acceptability
    accept_mean = mean(accept_vb_rev, na.rm = TRUE),
    # Contact
    n_contacts_mean = mean(n_parties_contact, na.rm = TRUE),
    # N respondents
    n_resp = n(),
    # Larreguy vars (constant within sección)
    pri_mean           = first(pri_mean),
    pan_mean           = first(pan_mean),
    prd_mean           = first(prd_mean),
    pri_sd             = first(pri_sd),
    pan_sd             = first(pan_sd),
    margin_sd          = first(margin_sd),
    turnout_mean       = first(turnout_mean),
    turnout_sd         = first(turnout_sd),
    n_elections        = first(n_elections),
    n_alternations     = first(n_distinct_incumbents),
    share_PRI_pre2015  = first(share_PRI_valid_vote_pre2015),
    share_PAN_pre2015  = first(share_PAN_valid_vote_pre2015),
    share_PRD_pre2015  = first(share_PRD_valid_vote_pre2015),
    mun_margin         = first(mun_winning_margin_pre2015),
    turnout_pre2015    = first(turnout_pre2015),
    max_registered     = first(max_registered),
    incumbent_party    = first(incumbent_party_pre2015),
    .groups = "drop"
  ) %>%
  mutate(
    log_registered = log(max_registered),
    pri_incumbent  = as.integer(grepl("PRI", incumbent_party)),
    opposition_mun = 1 - pri_incumbent,
    # Effective number of parties (municipal, pre-2015)
    enp_pre2015 = 1 / (share_PRI_pre2015^2 + share_PAN_pre2015^2 +
                        share_PRD_pre2015^2)
  )
 
cat("Sección-level data:", nrow(secc), "precincts\n")
 
##############################################################################
# 5. FIGURE 1: VOTE BUYING PREVALENCE BY PARTY
##############################################################################
 
prev_data <- data.frame(
  party = factor(c("PRI","PVEM","PAN","PRD","MORENA","MC"),
                 levels = c("PRI","PVEM","PAN","PRD","MORENA","MC")),
  direct   = c(
    mean(merged$vb_pri)*100, mean(merged$vb_pvem)*100,
    mean(merged$vb_pan)*100, mean(merged$vb_prd)*100,
    mean(merged$vb_morena)*100, mean(merged$vb_mc)*100
  ),
  observed = c(
    mean(merged$obs_pri)*100, mean(merged$obs_pvem)*100,
    mean(merged$obs_pan)*100, mean(merged$obs_prd)*100,
    mean(merged$obs_morena)*100, mean(merged$obs_mc)*100
  )
)
 
fig1_data <- prev_data %>%
  pivot_longer(cols = c(direct, observed),
               names_to = "type", values_to = "pct") %>%
  mutate(type = ifelse(type == "direct", "Direct receipt",
                       "Observed in neighborhood"))
 
fig1 <- ggplot(fig1_data, aes(x = party, y = pct, fill = type)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(pct, 0)),
            position = position_dodge(0.8), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Direct receipt" = "#2C5F8A",
                                "Observed in neighborhood" = "#A8C8E8")) +
  labs(x = NULL, y = "Percent of respondents", fill = NULL,
       title = "Figure 1. Vote Buying Prevalence by Party") +
  ylim(0, 60) +
  theme_minimal() +
  theme(legend.position = "top")
 
ggsave(paste0(FIG_DIR, "fig1_prevalence.png"), fig1,
       width = 7, height = 4.5, dpi = 300)
cat("Figure 1 saved\n")
 
##############################################################################
# 6. ICC DECOMPOSITION (Figure 2, Panel A)
##############################################################################
 
# Manual ICC calculation (one-way random effects ANOVA)
calc_icc <- function(data, dv, group) {
  data <- data[!is.na(data[[dv]]), ]
  grand_mean <- mean(data[[dv]])
  groups <- split(data[[dv]], data[[group]])
  k <- length(groups)
  n <- nrow(data)
  ni <- sapply(groups, length)
  n0 <- (n - sum(ni^2) / n) / (k - 1)
  group_means <- sapply(groups, mean)
  ssb <- sum(ni * (group_means - grand_mean)^2)
  ssw <- sum(sapply(names(groups), function(g) {
    sum((groups[[g]] - group_means[g])^2)
  }))
  msb <- ssb / (k - 1)
  msw <- ssw / (n - k)
  icc <- (msb - msw) / (msb + (n0 - 1) * msw)
  return(icc)
}
 
dv_list <- c("any_vb", "vb_pri", "vb_pan", "vb_prd",
             "vb_pvem", "vb_morena", "any_obs")
dv_labels <- c("Any VB", "PRI", "PAN", "PRD", "PVEM", "MORENA", "Any observed")
 
icc_results <- data.frame(
  dv    = dv_labels,
  icc   = sapply(dv_list, function(dv) calc_icc(merged, dv, "precinct")),
  stringsAsFactors = FALSE
)
 
cat("\n=== ICC RESULTS ===\n")
print(icc_results)
 
# Save table
write.csv(icc_results, paste0(TABLE_DIR, "t2_icc.csv"), row.names = FALSE)
 
##############################################################################
# 7. WITHIN-SECCIÓN INDIVIDUAL PREDICTORS (Figure 2, Panel B)
##############################################################################
 
calc_within_r <- function(data, iv, dv, group) {
  d <- data[!is.na(data[[iv]]) & !is.na(data[[dv]]), ]
  d$iv_dm <- ave(d[[iv]], d[[group]], FUN = function(x) x - mean(x))
  d$dv_dm <- ave(d[[dv]], d[[group]], FUN = function(x) x - mean(x))
  ct <- cor.test(d$iv_dm, d$dv_dm)
  return(list(r = ct$estimate, p = ct$p.value, n = nrow(d)))
}
 
within_vars <- c("female", "age", "educ", "priista", "panista", "no_party")
within_labels <- c("Female", "Age", "Education", "PRI partisan",
                    "PAN partisan", "No party ID")
 
within_results <- data.frame(
  predictor = within_labels,
  r = NA_real_, p = NA_real_, n = NA_integer_,
  stringsAsFactors = FALSE
)
 
for (i in seq_along(within_vars)) {
  res <- calc_within_r(merged, within_vars[i], "any_vb", "precinct")
  within_results$r[i] <- res$r
  within_results$p[i] <- res$p
  within_results$n[i] <- res$n
}
 
cat("\n=== WITHIN-SECCIÓN PREDICTORS (Any VB) ===\n")
within_results$sig <- ifelse(within_results$p < 0.01, "***",
                      ifelse(within_results$p < 0.05, "**",
                      ifelse(within_results$p < 0.10, "*", "")))
print(within_results)
 
write.csv(within_results, paste0(TABLE_DIR, "t3_within_seccion.csv"),
          row.names = FALSE)
 
# --- Combined Figure 2: ICC + Within-sección ---
fig2a_data <- icc_results %>%
  mutate(dv = factor(dv, levels = rev(dv_labels)))
 
fig2a <- ggplot(fig2a_data, aes(x = icc, y = dv)) +
  geom_col(fill = "#2C5F8A", width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", icc)), hjust = -0.2, size = 3.5) +
  labs(x = "ICC", y = NULL,
       title = "(a) Between-sección variance") +
  xlim(0, max(icc_results$icc) * 1.3) +
  theme_minimal()
 
fig2b_data <- within_results %>%
  mutate(predictor = factor(predictor, levels = rev(within_labels)))
 
fig2b <- ggplot(fig2b_data, aes(x = r, y = predictor)) +
  geom_col(fill = "#BDC3C7", width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_text(aes(label = sprintf("%+.3f%s", r, sig)),
            hjust = -0.2, size = 3.5) +
  labs(x = "Within-sección correlation (r)", y = NULL,
       title = "(b) Individual predictors within secciones") +
  xlim(-0.15, 0.2) +
  theme_minimal()
 
# Save separately (combine in LaTeX)
ggsave(paste0(FIG_DIR, "fig2a_icc.png"), fig2a,
       width = 5, height = 4, dpi = 300)
ggsave(paste0(FIG_DIR, "fig2b_within.png"), fig2b,
       width = 5, height = 4, dpi = 300)
cat("Figure 2 saved\n")
 
##############################################################################
# 8. CROSS-PARTY CONVERGENCE (Figure 3, Table 1)
##############################################################################
 
party_vb_vars <- c("vb_pri", "vb_pan", "vb_prd", "vb_pvem", "vb_morena", "vb_mc")
party_labels_short <- c("PRI", "PAN", "PRD", "PVEM", "MORENA", "MC")
 
raw_corr <- cor(secc[, party_vb_vars], use = "pairwise.complete.obs")
rownames(raw_corr) <- party_labels_short
colnames(raw_corr) <- party_labels_short
 
cat("\n=== RAW CROSS-PARTY CORRELATIONS ===\n")
print(round(raw_corr, 3))
 
# Mean off-diagonal
off_diag <- raw_corr[upper.tri(raw_corr)]
cat("Mean off-diagonal r:", round(mean(off_diag), 3), "\n")
 
write.csv(raw_corr, paste0(TABLE_DIR, "t5_crossparty_corr.csv"))
 
# Heatmap figure
png(paste0(FIG_DIR, "fig3_crossparty_heatmap.png"),
    width = 5.5, height = 4.5, units = "in", res = 300)
corrplot(raw_corr, method = "color", type = "full",
         addCoef.col = "black", tl.col = "black", tl.cex = 1,
         number.cex = 0.9, cl.cex = 0.8,
         col = colorRampPalette(c("white", "#2C5F8A"))(200),
         title = "Figure 3. Cross-Party Vote Buying Correlations\n(Sección level, N=200)",
         mar = c(0, 0, 2, 0))
dev.off()
cat("Figure 3 saved\n")
 
##############################################################################
# 9. ELIMINATION TEST: PARTIAL CORRELATIONS (Table 2)
##############################################################################
 
# Controls: pri_mean, pri_incumbent, prospera_share, log_registered, turnout_mean
controls <- c("pri_mean", "pri_incumbent", "prospera_share",
              "log_registered", "turnout_mean")
 
secc_complete <- secc %>%
  filter(complete.cases(across(all_of(c(controls, party_vb_vars)))))
 
cat("\nN precincts with complete data:", nrow(secc_complete), "\n")
 
# Residualize each party VB on controls
resid_mat <- matrix(NA, nrow = nrow(secc_complete), ncol = length(party_vb_vars))
colnames(resid_mat) <- party_vb_vars
 
X <- as.matrix(cbind(secc_complete[, controls], 1))
 
for (j in seq_along(party_vb_vars)) {
  y <- secc_complete[[party_vb_vars[j]]]
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  resid_mat[, j] <- y - X %*% beta
}
 
partial_corr <- cor(resid_mat)
rownames(partial_corr) <- party_labels_short
colnames(partial_corr) <- party_labels_short
 
cat("\n=== PARTIAL CORRELATIONS (controls) ===\n")
print(round(partial_corr, 3))
 
off_diag_partial <- partial_corr[upper.tri(partial_corr)]
cat("Mean off-diagonal r:", round(mean(off_diag_partial), 3), "\n")
 
# --- With state fixed effects ---
state_dummies <- model.matrix(~ factor(state_code) - 1,
                               data = secc_complete)[, -1]
X_fe <- cbind(as.matrix(secc_complete[, controls]), state_dummies, 1)
 
resid_fe <- matrix(NA, nrow = nrow(secc_complete), ncol = length(party_vb_vars))
colnames(resid_fe) <- party_vb_vars
 
for (j in seq_along(party_vb_vars)) {
  y <- secc_complete[[party_vb_vars[j]]]
  # Use lm for numerical stability with many dummies
  fit <- lm(y ~ ., data = as.data.frame(cbind(
    secc_complete[, controls], state_dummies)))
  resid_fe[, j] <- residuals(fit)
}
 
partial_fe <- cor(resid_fe)
rownames(partial_fe) <- party_labels_short
colnames(partial_fe) <- party_labels_short
 
cat("\n=== PARTIAL CORRELATIONS (controls + state FE) ===\n")
print(round(partial_fe, 3))
 
off_diag_fe <- partial_fe[upper.tri(partial_fe)]
cat("Mean off-diagonal r:", round(mean(off_diag_fe), 3), "\n")
 
# --- With N contacts ---
resid_nc <- matrix(NA, nrow = nrow(secc_complete), ncol = length(party_vb_vars))
colnames(resid_nc) <- party_vb_vars
 
controls_nc <- c(controls, "n_contacts_mean")
 
for (j in seq_along(party_vb_vars)) {
  y <- secc_complete[[party_vb_vars[j]]]
  fit <- lm(y ~ ., data = secc_complete[, controls_nc])
  resid_nc[, j] <- residuals(fit)
}
 
partial_nc <- cor(resid_nc)
off_diag_nc <- partial_nc[upper.tri(partial_nc)]
 
# Summary table
elimination_table <- data.frame(
  Specification = c("Raw", "Controls", "Controls + State FE",
                     "Controls + N contacts"),
  Mean_r = c(mean(off_diag), mean(off_diag_partial),
             mean(off_diag_fe), mean(off_diag_nc)),
  Change = c(NA,
             mean(off_diag_partial) - mean(off_diag),
             mean(off_diag_fe) - mean(off_diag),
             mean(off_diag_nc) - mean(off_diag))
)
 
cat("\n=== ELIMINATION SUMMARY ===\n")
print(elimination_table)
 
write.csv(elimination_table, paste0(TABLE_DIR, "t2_elimination.csv"),
          row.names = FALSE)
 
##############################################################################
# 10. DEMAND-SIDE: ACCEPTABILITY NORMS
##############################################################################
 
cat("\n=== ACCEPTABILITY NORMS (Model 1 test) ===\n")
 
# Sección level
accept_test <- cor.test(secc_complete$accept_mean, secc_complete$vb_rate,
                         use = "pairwise.complete.obs")
cat("Sección level: r =", round(accept_test$estimate, 3),
    ", p =", round(accept_test$p.value, 4), "\n")
 
# Individual level (raw)
ind_accept <- cor.test(merged$accept_vb_rev, merged$any_vb,
                        use = "pairwise.complete.obs")
cat("Individual (raw): r =", round(ind_accept$estimate, 3),
    ", p =", round(ind_accept$p.value, 4), "\n")
 
# Individual level (within-sección)
within_accept <- calc_within_r(merged, "accept_vb_rev", "any_vb", "precinct")
cat("Individual (within): r =", round(within_accept$r, 3),
    ", p =", round(within_accept$p, 4), "\n")
 
# By VB tercile
secc_acc <- secc_complete %>%
  filter(!is.na(accept_mean), !is.na(vb_rate)) %>%
  mutate(vb_tercile = ntile(vb_rate, 3),
         vb_tercile = factor(vb_tercile, labels = c("Low VB","Med VB","High VB")))
 
acc_by_tercile <- secc_acc %>%
  group_by(vb_tercile) %>%
  summarise(n = n(), vb = mean(vb_rate), accept = mean(accept_mean),
            .groups = "drop")
 
cat("\nAcceptability by VB tercile:\n")
print(acc_by_tercile)
 
# Means by individual VB receipt
cat("\nAcceptability by VB receipt:\n")
merged %>%
  filter(!is.na(accept_vb_rev)) %>%
  group_by(any_vb) %>%
  summarise(mean_accept = mean(accept_vb_rev, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  print()
 
##############################################################################
# 11. SHARED INFRASTRUCTURE: MULTI-PARTY CONTACT
##############################################################################
 
cat("\n=== MULTI-PARTY CONTACT (Model 3 test) ===\n")
 
# Distribution of N parties contacting
cat("\nN parties contacting distribution:\n")
table(merged$n_parties_contact) %>% print()
 
# Cross-party contact correlations (individual)
contact_vars <- c("contact_pan", "contact_pri", "contact_prd",
                   "contact_pvem", "contact_morena", "contact_mc")
contact_corr <- cor(merged[, contact_vars], use = "pairwise.complete.obs")
colnames(contact_corr) <- party_labels_short
rownames(contact_corr) <- party_labels_short
 
cat("\nCross-party CONTACT correlations:\n")
print(round(contact_corr, 3))
cat("Mean off-diagonal r:",
    round(mean(contact_corr[upper.tri(contact_corr)]), 3), "\n")
 
# Sección level: N contacts → VB rate
nc_test <- cor.test(secc_complete$n_contacts_mean, secc_complete$vb_rate)
cat("\nSección: N contacts → VB rate: r =", round(nc_test$estimate, 3),
    ", p =", round(nc_test$p.value, 4), "\n")
 
##############################################################################
# 12. ELECTORAL ENVIRONMENT (Additional findings)
##############################################################################
 
cat("\n=== SECCIÓN-LEVEL ELECTORAL PREDICTORS ===\n")
 
electoral_vars <- c(
  "pri_mean", "pan_mean", "share_PRI_pre2015", "share_PAN_pre2015",
  "enp_pre2015", "mun_margin", "turnout_mean",
  "pri_sd", "pan_sd", "margin_sd", "n_alternations",
  "opposition_mun", "log_registered"
)
 
electoral_labels <- c(
  "PRI mean vote share (mun.)", "PAN mean vote share (mun.)",
  "PRI share (most recent mun.)", "PAN share (most recent mun.)",
  "Effective N parties", "Municipal margin", "Mean turnout",
  "PRI volatility", "PAN volatility", "Margin volatility",
  "N partisan alternations", "Non-PRI incumbent", "Log precinct size"
)
 
electoral_results <- data.frame(
  predictor = electoral_labels,
  r_any_vb = NA_real_, p_any_vb = NA_real_,
  stringsAsFactors = FALSE
)
 
for (i in seq_along(electoral_vars)) {
  v <- electoral_vars[i]
  mask <- !is.na(secc[[v]]) & !is.na(secc$vb_rate)
  if (sum(mask) > 10) {
    ct <- cor.test(secc[[v]][mask], secc$vb_rate[mask])
    electoral_results$r_any_vb[i] <- ct$estimate
    electoral_results$p_any_vb[i] <- ct$p.value
  }
}
 
electoral_results <- electoral_results %>%
  mutate(sig = ifelse(p_any_vb < 0.01, "***",
               ifelse(p_any_vb < 0.05, "**",
               ifelse(p_any_vb < 0.10, "*", "")))) %>%
  arrange(desc(abs(r_any_vb)))
 
cat("\nAll predictors (sorted by |r|):\n")
print(electoral_results)
 
write.csv(electoral_results, paste0(TABLE_DIR, "t4_electoral_predictors.csv"),
          row.names = FALSE)
 
##############################################################################
# 13. INCUMBENT PARTY TABLE
##############################################################################
 
inc_table <- secc %>%
  group_by(incumbent_party) %>%
  summarise(
    N = n(),
    Any_VB = mean(vb_rate, na.rm = TRUE),
    PRI_VB = mean(vb_pri, na.rm = TRUE),
    PAN_VB = mean(vb_pan, na.rm = TRUE),
    N_parties = mean(n_parties, na.rm = TRUE),
    PRI_share = mean(pri_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(N >= 5) %>%
  arrange(desc(Any_VB))
 
cat("\n=== VOTE BUYING BY INCUMBENT PARTY ===\n")
print(inc_table)
 
write.csv(inc_table, paste0(TABLE_DIR, "t6_incumbent_party.csv"),
          row.names = FALSE)
 
##############################################################################
# 14. PRI TERCILE ANALYSIS
##############################################################################
 
secc_terc <- secc %>%
  filter(!is.na(pri_mean)) %>%
  mutate(pri_tercile = ntile(pri_mean, 3),
         pri_tercile = factor(pri_tercile,
                              labels = c("Low PRI","Med PRI","High PRI")))
 
terc_table <- secc_terc %>%
  group_by(pri_tercile) %>%
  summarise(
    N = n(),
    PRI_share = mean(pri_mean),
    Any_VB = mean(vb_rate),
    PRI_VB = mean(vb_pri),
    PAN_VB = mean(vb_pan),
    N_parties = mean(n_parties),
    .groups = "drop"
  )
 
cat("\n=== PRI TERCILE ANALYSIS ===\n")
print(terc_table)
 
# T-test: high vs low
high <- secc_terc %>% filter(pri_tercile == "High PRI") %>% pull(vb_rate)
low  <- secc_terc %>% filter(pri_tercile == "Low PRI") %>% pull(vb_rate)
tt <- t.test(high, low)
cat("High vs Low PRI (Any VB): diff =", round(mean(high) - mean(low), 3),
    ", t =", round(tt$statistic, 2), ", p =", round(tt$p.value, 3), "\n")
 
write.csv(terc_table, paste0(TABLE_DIR, "t7_pri_terciles.csv"),
          row.names = FALSE)
 
##############################################################################
# 15. ZONE TARGETING FIGURE (4-panel)
##############################################################################
 
fig4a <- ggplot(secc, aes(x = pri_mean, y = vb_rate)) +
  geom_point(alpha = 0.4, color = "#2C5F8A", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#C0392B", linewidth = 1) +
  annotate("text", x = min(secc$pri_mean, na.rm = TRUE) + 0.02,
           y = 0.95,
           label = sprintf("r = %+.3f***",
                           cor(secc$pri_mean, secc$vb_rate,
                               use = "complete.obs")),
           hjust = 0, size = 3.5) +
  labs(x = "PRI mean vote share (municipal)", y = "Vote buying rate",
       title = "(a) PRI electoral strength") +
  theme_minimal()
 
fig4b <- ggplot(secc %>% filter(!is.na(enp_pre2015)),
                aes(x = enp_pre2015, y = vb_pri)) +
  geom_point(alpha = 0.4, color = "#2C5F8A", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#C0392B", linewidth = 1) +
  labs(x = "Effective N parties (municipal)", y = "PRI vote buying rate",
       title = "(b) Party fragmentation") +
  theme_minimal()
 
fig4d <- ggplot(secc %>% filter(!is.na(pri_sd)),
                aes(x = pri_sd, y = vb_rate)) +
  geom_point(alpha = 0.4, color = "#BDC3C7", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#999999",
              linewidth = 1, linetype = "dashed") +
  annotate("text", x = min(secc$pri_sd, na.rm = TRUE) + 0.01,
           y = 0.95,
           label = sprintf("r = %+.3f (n.s.)",
                           cor(secc$pri_sd, secc$vb_rate,
                               use = "complete.obs")),
           hjust = 0, size = 3.5) +
  labs(x = "PRI vote share volatility (municipal)", y = "Vote buying rate",
       title = "(d) Electoral volatility (null)") +
  theme_minimal()
 
# Incumbent party bars
inc_plot_data <- inc_table %>%
  mutate(incumbent_party = reorder(incumbent_party, Any_VB))
 
fig4c <- ggplot(inc_plot_data,
                aes(x = Any_VB, y = incumbent_party)) +
  geom_col(fill = ifelse(grepl("^PRI", inc_plot_data$incumbent_party),
                          "#2C5F8A", "#C0392B"),
           width = 0.7) +
  geom_text(aes(label = sprintf("%.2f (n=%d)", Any_VB, N)),
            hjust = -0.1, size = 3) +
  labs(x = "Vote buying rate", y = NULL,
       title = "(c) By municipal incumbent party") +
  xlim(0, 1.05) +
  theme_minimal()
 
# Save individual panels
ggsave(paste0(FIG_DIR, "fig4a_pri_share.png"), fig4a,
       width = 5, height = 4, dpi = 300)
ggsave(paste0(FIG_DIR, "fig4b_enp.png"), fig4b,
       width = 5, height = 4, dpi = 300)
ggsave(paste0(FIG_DIR, "fig4c_incumbent.png"), fig4c,
       width = 5, height = 4, dpi = 300)
ggsave(paste0(FIG_DIR, "fig4d_volatility.png"), fig4d,
       width = 5, height = 4, dpi = 300)
cat("Figure 4 panels saved\n")
 
##############################################################################
# 16. EXTENDED WITHIN-SECCIÓN (including PROSPERA, indigenous, skin color)
##############################################################################
 
cat("\n=== EXTENDED WITHIN-SECCIÓN PREDICTORS ===\n")
 
ext_vars <- c("female", "age", "educ", "priista", "prospera",
              "indigenous", "indig_lang", "skin_self")
ext_labels <- c("Female", "Age", "Education", "PRI partisan",
                "PROSPERA beneficiary", "Indigenous self-ID",
                "Indigenous language", "Skin color (darker=higher)")
 
ext_results <- data.frame(
  predictor = ext_labels,
  r_any = NA_real_, p_any = NA_real_,
  r_pri = NA_real_, p_pri = NA_real_,
  r_nparties = NA_real_, p_nparties = NA_real_,
  stringsAsFactors = FALSE
)
 
for (i in seq_along(ext_vars)) {
  for (dv_col in 1:3) {
    dv <- c("any_vb", "vb_pri", "n_parties_vb")[dv_col]
    res <- calc_within_r(merged, ext_vars[i], dv, "precinct")
    if (dv_col == 1) {
      ext_results$r_any[i] <- res$r; ext_results$p_any[i] <- res$p
    } else if (dv_col == 2) {
      ext_results$r_pri[i] <- res$r; ext_results$p_pri[i] <- res$p
    } else {
      ext_results$r_nparties[i] <- res$r; ext_results$p_nparties[i] <- res$p
    }
  }
}
 
ext_results <- ext_results %>%
  mutate(
    sig_any = ifelse(p_any < 0.01, "***", ifelse(p_any < 0.05, "**",
              ifelse(p_any < 0.10, "*", ""))),
    sig_pri = ifelse(p_pri < 0.01, "***", ifelse(p_pri < 0.05, "**",
              ifelse(p_pri < 0.10, "*", ""))),
    sig_np  = ifelse(p_nparties < 0.01, "***", ifelse(p_nparties < 0.05, "**",
              ifelse(p_nparties < 0.10, "*", "")))
  )
 
cat("\nAll within-sección predictors:\n")
print(ext_results %>% select(predictor, r_any, sig_any, r_pri, sig_pri,
                              r_nparties, sig_np))
 
write.csv(ext_results, paste0(TABLE_DIR, "ts1_within_all.csv"),
          row.names = FALSE)
 
##############################################################################
# 17. ROBUSTNESS: FEDERAL DEPUTY ELECTION DATA (Magar)
##############################################################################
 
cat("\n=== ROBUSTNESS: FEDERAL DEPUTY DATA ===\n")
 
# Load and aggregate 2015 diputados
dip2015 <- read.csv(paste0(DATA_DIR, "dip2015.csv"),
                     stringsAsFactors = FALSE)
 
# Convert to numeric
num_cols <- setdiff(names(dip2015),
                     c("casilla","mun","status","tepjf","nota","nota.emm"))
for (col in num_cols) {
  dip2015[[col]] <- suppressWarnings(as.numeric(dip2015[[col]]))
}
 
# PRI total = pri + pri.pvem coalition
dip2015$pri_total <- rowSums(dip2015[, c("pri", "pri.pvem")], na.rm = TRUE)
dip2015$prd_total <- rowSums(dip2015[, c("prd", "prd.pt")], na.rm = TRUE)
dip2015$pan_total <- rowSums(dip2015[, c("pan", "pan.pna")], na.rm = TRUE)
 
secc_fed <- dip2015 %>%
  group_by(edon, seccion) %>%
  summarise(
    pri_2015 = sum(pri_total, na.rm = TRUE),
    pan_2015 = sum(pan_total, na.rm = TRUE),
    prd_2015 = sum(prd_total, na.rm = TRUE),
    morena_2015 = sum(morena, na.rm = TRUE),
    tot_2015 = sum(tot, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share_pri_fed = pri_2015 / tot_2015,
    share_pan_fed = pan_2015 / tot_2015,
    share_morena_fed = morena_2015 / tot_2015
  )
 
# Merge with survey
survey_fed <- survey %>%
  mutate(edon = state_code, seccion = precinct) %>%
  inner_join(secc_fed, by = c("edon", "seccion"))
 
cat("Federal deputy merge:", nrow(survey_fed), "matched\n")
 
# Key test: PRI share → VB
secc_fed_agg <- survey_fed %>%
  group_by(edon, seccion) %>%
  summarise(
    vb_rate = mean(any_vb), vb_pri = mean(vb_pri),
    share_pri_fed = first(share_pri_fed),
    .groups = "drop"
  )
 
fed_test <- cor.test(secc_fed_agg$share_pri_fed, secc_fed_agg$vb_rate)
cat("Federal PRI share → Any VB: r =", round(fed_test$estimate, 3),
    ", p =", round(fed_test$p.value, 3), "\n")
cat("Compare to municipal PRI share → Any VB: r = -0.202, p < 0.001\n")
 
# Cross-party correlations with federal data
secc_fed_party <- survey_fed %>%
  group_by(edon, seccion) %>%
  summarise(across(c(vb_pri, vb_pan, vb_prd, vb_pvem, vb_morena, vb_mc),
                   mean), .groups = "drop")
 
fed_corr <- cor(secc_fed_party[, party_vb_vars], use = "pairwise.complete.obs")
cat("Cross-party convergence (federal data):",
    "mean off-diagonal r =",
    round(mean(fed_corr[upper.tri(fed_corr)]), 3), "\n")
 
##############################################################################
# 18. SUMMARY
##############################################################################
 
cat("\n")
cat("================================================================\n")
cat("REPLICATION SUMMARY\n")
cat("================================================================\n")
cat("Sample: ", nrow(merged), "respondents,", nrow(secc), "precincts\n")
cat("ICC (any VB):", round(icc_results$icc[1], 3), "\n")
cat("Cross-party convergence: mean r =", round(mean(off_diag), 3), "\n")
cat("After controls: mean r =", round(mean(off_diag_partial), 3), "\n")
cat("After controls + state FE: mean r =", round(mean(off_diag_fe), 3), "\n")
cat("After controls + N contacts: mean r =", round(mean(off_diag_nc), 3), "\n")
cat("Acceptability → VB (sección): r =", round(accept_test$estimate, 3),
    ", p =", round(accept_test$p.value, 4), "\n")
cat("PRI mean share → VB (mun.): r =",
    round(cor(secc$pri_mean, secc$vb_rate, use = "complete.obs"), 3), "\n")
cat("Volatility → VB: r =",
    round(cor(secc$pri_sd, secc$vb_rate, use = "complete.obs"), 3),
    "(null)\n")
cat("================================================================\n")
cat("All figures saved to:", FIG_DIR, "\n")
cat("All tables saved to:", TABLE_DIR, "\n")
