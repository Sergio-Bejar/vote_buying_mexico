##############################################################################
# REPLICATION CODE: Paper 2
# "From Contact to Gift: Party Organizations and the Vote Buying
#  Pipeline in Mexico"
#
# Author: Sergio Béjar
# Data:   CIDE-CSES 2015 Post-Electoral Survey (BD.sav)
#
# Requirements: haven, dplyr, tidyr, ggplot2
##############################################################################
 
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
 
# Set paths -- adjust to your directory structure
DATA_DIR  <- "data/"
FIG_DIR   <- "paper2/figures/"
TABLE_DIR <- "paper2/tables/"
 
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
 
##############################################################################
# 1. LOAD AND PREPARE DATA
##############################################################################
 
survey <- read_sav(paste0(DATA_DIR, "BD.sav"))
 
# --- Vote buying DVs ---
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
 
# --- Party contact: any mode (p26_X: 1=yes, 2=no) ---
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
 
# --- Contact by mode (a=face-to-face, b=postal, c=phone,
#     d=text/SMS, e=email, f=social media) ---
# These are only asked of respondents who were contacted (p26_X==1)
# 1=yes used this mode, 2=no
survey <- survey %>%
  mutate(
    # PRI modes
    pri_f2f    = as.integer(p26_2a == 1),
    pri_postal = as.integer(p26_2b == 1),
    pri_phone  = as.integer(p26_2c == 1),
    pri_text   = as.integer(p26_2d == 1),
    pri_email  = as.integer(p26_2e == 1),
    pri_social = as.integer(p26_2f == 1),
    # PAN modes
    pan_f2f    = as.integer(p26_1a == 1),
    pan_postal = as.integer(p26_1b == 1),
    pan_phone  = as.integer(p26_1c == 1),
    pan_text   = as.integer(p26_1d == 1),
    pan_email  = as.integer(p26_1e == 1),
    pan_social = as.integer(p26_1f == 1),
    # PRD modes
    prd_f2f    = as.integer(p26_3a == 1),
    # PVEM modes
    pvem_f2f   = as.integer(p26_4a == 1),
    # MORENA modes
    morena_f2f = as.integer(p26_7a == 1),
    # MC modes
    mc_f2f     = as.integer(p26_6a == 1)
  )
 
# --- Rally attendance ---
survey <- survey %>%
  mutate(
    attended_rally = as.integer(p50_5 == 1),
    rally_help     = as.integer(p51c == 1),
    rally_recruit  = ifelse(p51b %in% c(8, 9), NA, p51b)
    # 1=neighbor, 2=community leader, 3=party member, 4=other
  )
 
# --- Individual covariates (for within-sección comparison) ---
survey <- survey %>%
  mutate(
    female     = as.integer(ps2 == 2),
    age        = ifelse(ps1 == 99, NA, ps1),
    educ       = ifelse(ps3 %in% c(98, 99), NA, ifelse(ps3 == 96, 0, ps3)),
    priista    = as.integer(p9 %in% c(3, 4)),
    prospera   = as.integer(pcyc12b == 1),
    indig_lang = as.integer(ps27 == 1),
    secc       = as.numeric(secc)
  )
 
cat("Data loaded:", nrow(survey), "observations\n\n")
 
##############################################################################
# 2. TABLE 1: CONTACT RATES BY PARTY AND MODE
##############################################################################
 
cat("================================================================\n")
cat("TABLE 1: CONTACT RATES BY PARTY AND MODE\n")
cat("================================================================\n\n")
 
# Party contact rates (any mode)
party_ids   <- c(2, 1, 4, 3, 7, 6)
party_names <- c("PRI", "PAN", "PVEM", "PRD", "MORENA", "MC")
contact_cols <- c("contact_pri", "contact_pan", "contact_pvem",
                  "contact_prd", "contact_morena", "contact_mc")
 
contact_rates <- sapply(contact_cols, function(x) mean(survey[[x]], na.rm = TRUE))
names(contact_rates) <- party_names
cat("Contact rates (any mode):\n")
print(round(contact_rates * 100, 1))
 
# Mode breakdown among contacted (PRI focus for main text, all for table)
mode_names <- c("Face-to-face", "Postal mail", "Phone",
                "Text/SMS", "Email", "Social media")
mode_suffixes <- c("a", "b", "c", "d", "e", "f")
 
t1_list <- list()
for (i in seq_along(party_ids)) {
  pid <- party_ids[i]
  pname <- party_names[i]
  contact_var <- contact_cols[i]
  contacted <- survey[survey[[contact_var]] == 1, ]
  n_cont <- nrow(contacted)
 
  mode_pcts <- sapply(mode_suffixes, function(suf) {
    var <- paste0("p26_", pid, suf)
    if (var %in% names(contacted)) {
      return(mean(contacted[[var]] == 1, na.rm = TRUE) * 100)
    } else {
      return(NA)
    }
  })
 
  t1_list[[pname]] <- data.frame(
    Party = pname,
    N_contacted = n_cont,
    Pct_contacted = round(n_cont / nrow(survey) * 100, 1),
    Face_to_face = round(mode_pcts[1], 1),
    Postal = round(mode_pcts[2], 1),
    Phone = round(mode_pcts[3], 1),
    Text_SMS = round(mode_pcts[4], 1),
    Email = round(mode_pcts[5], 1),
    Social_media = round(mode_pcts[6], 1),
    stringsAsFactors = FALSE
  )
}
 
t1 <- bind_rows(t1_list)
cat("\nContact mode breakdown (% among contacted):\n")
print(t1)
write.csv(t1, paste0(TABLE_DIR, "t1_contact_modes.csv"), row.names = FALSE)
 
##############################################################################
# 3. TABLE 2: CONTACT → VOTE BUYING (party-specific conversion)
##############################################################################
 
cat("\n================================================================\n")
cat("TABLE 2: CONTACT-TO-GIFT CONVERSION BY PARTY\n")
cat("================================================================\n\n")
 
vb_cols <- c("vb_pri", "vb_pan", "vb_pvem", "vb_prd", "vb_morena", "vb_mc")
 
t2_list <- list()
for (i in seq_along(party_names)) {
  pname <- party_names[i]
  cv <- contact_cols[i]
  vv <- vb_cols[i]
 
  contacted     <- survey[survey[[cv]] == 1, ]
  not_contacted <- survey[survey[[cv]] == 0, ]
 
  vb_if_contact <- mean(contacted[[vv]], na.rm = TRUE)
  vb_if_no      <- mean(not_contacted[[vv]], na.rm = TRUE)
  diff <- vb_if_contact - vb_if_no
 
  tt <- t.test(contacted[[vv]], not_contacted[[vv]])
  sig <- ifelse(tt$p.value < 0.01, "***",
         ifelse(tt$p.value < 0.05, "**",
         ifelse(tt$p.value < 0.10, "*", "")))
 
  t2_list[[pname]] <- data.frame(
    Party = pname,
    N_contacted = nrow(contacted),
    VB_if_contacted = round(vb_if_contact, 3),
    VB_if_not = round(vb_if_no, 3),
    Difference = paste0(sprintf("%+.3f", diff), sig),
    Conversion_rate = sprintf("%.1f%%", vb_if_contact * 100),
    stringsAsFactors = FALSE
  )
 
  cat(sprintf("  %s: contacted=%.3f (n=%d), not=%.3f (n=%d), diff=%+.3f%s\n",
              pname, vb_if_contact, nrow(contacted),
              vb_if_no, nrow(not_contacted), diff, sig))
}
 
t2 <- bind_rows(t2_list)
write.csv(t2, paste0(TABLE_DIR, "t2_conversion.csv"), row.names = FALSE)
 
##############################################################################
# 4. TABLE 3: CONTACT MODE → PRI VB (among PRI-contacted)
##############################################################################
 
cat("\n================================================================\n")
cat("TABLE 3: WHICH PRI CONTACT MODE PREDICTS GIFT RECEIPT?\n")
cat("================================================================\n\n")
 
pri_contacted <- survey %>% filter(contact_pri == 1)
cat("N contacted by PRI:", nrow(pri_contacted), "\n\n")
 
mode_vars_pri <- c("pri_f2f", "pri_postal", "pri_phone",
                    "pri_text", "pri_email", "pri_social")
 
t3_list <- list()
for (j in seq_along(mode_vars_pri)) {
  mv <- mode_vars_pri[j]
  mn <- mode_names[j]
 
  yes <- pri_contacted[pri_contacted[[mv]] == 1, ]
  no  <- pri_contacted[pri_contacted[[mv]] == 0, ]
 
  vb_yes <- mean(yes$vb_pri, na.rm = TRUE)
  vb_no  <- mean(no$vb_pri, na.rm = TRUE)
  diff <- vb_yes - vb_no
 
  if (nrow(yes) > 5 & nrow(no) > 5) {
    tt <- t.test(yes$vb_pri, no$vb_pri)
    sig <- ifelse(tt$p.value < 0.01, "***",
           ifelse(tt$p.value < 0.05, "**",
           ifelse(tt$p.value < 0.10, "*", "")))
    pval <- tt$p.value
  } else {
    sig <- ""; pval <- NA
  }
 
  t3_list[[mn]] <- data.frame(
    Contact_mode = mn,
    N_using_mode = nrow(yes),
    VB_if_used = round(vb_yes, 3),
    VB_if_not_used = round(vb_no, 3),
    Difference = paste0(sprintf("%+.3f", diff), sig),
    p_value = round(pval, 4),
    stringsAsFactors = FALSE
  )
 
  cat(sprintf("  %s: yes=%.3f (n=%d), no=%.3f (n=%d), diff=%+.3f%s\n",
              mn, vb_yes, nrow(yes), vb_no, nrow(no), diff, sig))
}
 
t3 <- bind_rows(t3_list)
write.csv(t3, paste0(TABLE_DIR, "t3_pri_modes.csv"), row.names = FALSE)
 
##############################################################################
# 5. TABLE 4: IS CONTACT NECESSARY FOR GIFT RECEIPT?
##############################################################################
 
cat("\n================================================================\n")
cat("TABLE 4: PRIOR CONTACT STATUS OF GIFT RECIPIENTS\n")
cat("================================================================\n\n")
 
party_f2f_vars <- c("pri_f2f", "pan_f2f", "pvem_f2f", "prd_f2f",
                     "morena_f2f", "mc_f2f")
 
t4_list <- list()
for (i in seq_along(party_names)) {
  pname <- party_names[i]
  vv <- vb_cols[i]
  cv <- contact_cols[i]
  fv <- party_f2f_vars[i]
 
  recipients <- survey[survey[[vv]] == 1, ]
  n_recip <- nrow(recipients)
 
  if (n_recip < 10) next
 
  had_f2f     <- sum(recipients[[fv]] == 1, na.rm = TRUE)
  had_contact <- sum(recipients[[cv]] == 1, na.rm = TRUE)
  no_contact  <- n_recip - had_contact
 
  t4_list[[pname]] <- data.frame(
    Party = pname,
    N_recipients = n_recip,
    Had_f2f = sprintf("%d (%.0f%%)", had_f2f, had_f2f / n_recip * 100),
    Had_any_contact = sprintf("%d (%.0f%%)", had_contact,
                              had_contact / n_recip * 100),
    No_prior_contact = sprintf("%d (%.0f%%)", no_contact,
                               no_contact / n_recip * 100),
    stringsAsFactors = FALSE
  )
 
  cat(sprintf("  %s: %d recipients → f2f %d (%.0f%%), any contact %d (%.0f%%), no contact %d (%.0f%%)\n",
              pname, n_recip,
              had_f2f, had_f2f / n_recip * 100,
              had_contact, had_contact / n_recip * 100,
              no_contact, no_contact / n_recip * 100))
}
 
t4 <- bind_rows(t4_list)
write.csv(t4, paste0(TABLE_DIR, "t4_contact_necessary.csv"), row.names = FALSE)
 
##############################################################################
# 6. TABLE 5: RALLY ATTENDANCE AND VOTE BUYING
##############################################################################
 
cat("\n================================================================\n")
cat("TABLE 5: RALLY ATTENDANCE AND VOTE BUYING\n")
cat("================================================================\n\n")
 
rally     <- survey %>% filter(attended_rally == 1)
no_rally  <- survey %>% filter(attended_rally == 0)
 
cat(sprintf("  Non-attendees: n=%d, VB=%.3f, N parties=%.2f\n",
            nrow(no_rally), mean(no_rally$any_vb),
            mean(no_rally$n_parties_vb)))
cat(sprintf("  Rally attendees: n=%d, VB=%.3f, N parties=%.2f\n",
            nrow(rally), mean(rally$any_vb),
            mean(rally$n_parties_vb)))
 
tt_rally <- t.test(rally$any_vb, no_rally$any_vb)
cat(sprintf("  Diff: %+.3f, t=%.2f, p=%.4f\n",
            mean(rally$any_vb) - mean(no_rally$any_vb),
            tt_rally$statistic, tt_rally$p.value))
 
# By recruitment channel
cat("\n  Rally recruitment channel:\n")
t5_list <- list()
 
t5_list[["Non-attendees"]] <- data.frame(
  Group = "Non-attendees", N = nrow(no_rally),
  Any_VB = round(mean(no_rally$any_vb), 3),
  N_parties = round(mean(no_rally$n_parties_vb), 2),
  stringsAsFactors = FALSE)
 
t5_list[["Rally attendees"]] <- data.frame(
  Group = "Rally attendees", N = nrow(rally),
  Any_VB = round(mean(rally$any_vb), 3),
  N_parties = round(mean(rally$n_parties_vb), 2),
  stringsAsFactors = FALSE)
 
recruit_labels <- c("Recruited by neighbor", "Recruited by community leader",
                     "Recruited by party member")
for (rv in 1:3) {
  sub <- rally %>% filter(rally_recruit == rv)
  if (nrow(sub) > 5) {
    t5_list[[recruit_labels[rv]]] <- data.frame(
      Group = paste0("  ", recruit_labels[rv]),
      N = nrow(sub),
      Any_VB = round(mean(sub$any_vb), 3),
      N_parties = round(mean(sub$n_parties_vb), 2),
      stringsAsFactors = FALSE)
    cat(sprintf("    %s: n=%d, VB=%.3f, N parties=%.2f\n",
                recruit_labels[rv], nrow(sub),
                mean(sub$any_vb), mean(sub$n_parties_vb)))
  }
}
 
# Transport help
helped     <- rally %>% filter(rally_help == 1)
not_helped <- rally %>% filter(rally_help == 0)
 
t5_list[["Transport help"]] <- data.frame(
  Group = "  Transport help", N = nrow(helped),
  Any_VB = round(mean(helped$any_vb), 3),
  N_parties = round(mean(helped$n_parties_vb), 2),
  stringsAsFactors = FALSE)
 
t5_list[["No transport help"]] <- data.frame(
  Group = "  No transport help", N = nrow(not_helped),
  Any_VB = round(mean(not_helped$any_vb), 3),
  N_parties = round(mean(not_helped$n_parties_vb), 2),
  stringsAsFactors = FALSE)
 
cat(sprintf("\n    Transport help: n=%d, VB=%.3f\n",
            nrow(helped), mean(helped$any_vb)))
cat(sprintf("    No help: n=%d, VB=%.3f\n",
            nrow(not_helped), mean(not_helped$any_vb)))
 
t5 <- bind_rows(t5_list)
write.csv(t5, paste0(TABLE_DIR, "t5_rally.csv"), row.names = FALSE)
 
##############################################################################
# 7. N PARTIES CONTACTING → N PARTIES GIVING
##############################################################################
 
cat("\n================================================================\n")
cat("DOSE-RESPONSE: N PARTIES CONTACTING → N PARTIES GIVING\n")
cat("================================================================\n\n")
 
dose_r <- cor.test(survey$n_parties_contact, survey$n_parties_vb)
cat(sprintf("  r = %+.3f, p = %.4f\n", dose_r$estimate, dose_r$p.value))
 
# Cross-tab: mean VB by N contacted
dose_table <- survey %>%
  group_by(n_parties_contact) %>%
  filter(n() > 10) %>%
  summarise(
    N = n(),
    Any_VB = mean(any_vb),
    Mean_parties_giving = mean(n_parties_vb),
    .groups = "drop"
  )
 
cat("\nDose-response table:\n")
print(dose_table)
 
##############################################################################
# 8. TABLE 6: WITHIN-SECCIÓN — CONTACT AS STRONGEST PREDICTOR
##############################################################################
 
cat("\n================================================================\n")
cat("TABLE 6: WITHIN-SECCIÓN PREDICTORS (contact vs. demographics)\n")
cat("================================================================\n\n")
 
calc_within_r <- function(data, iv, dv, group) {
  d <- data[!is.na(data[[iv]]) & !is.na(data[[dv]]), ]
  d$iv_dm <- ave(d[[iv]], d[[group]], FUN = function(x) x - mean(x))
  d$dv_dm <- ave(d[[dv]], d[[group]], FUN = function(x) x - mean(x))
  ct <- cor.test(d$iv_dm, d$dv_dm)
  return(list(r = ct$estimate, p = ct$p.value, n = nrow(d)))
}
 
within_vars <- c("contact_pri", "pri_f2f", "prospera", "indig_lang",
                  "priista", "age", "educ", "female")
within_labels <- c("PRI contacted (any mode)", "PRI face-to-face",
                    "PROSPERA beneficiary", "Indigenous language",
                    "PRI partisan", "Age", "Education", "Female")
 
# Test against PRI VB (party-specific pipeline)
t6_pri <- data.frame(
  Predictor = within_labels,
  r_pri_vb = NA_real_, p_pri_vb = NA_real_,
  stringsAsFactors = FALSE
)
 
for (i in seq_along(within_vars)) {
  res <- calc_within_r(survey, within_vars[i], "vb_pri", "secc")
  t6_pri$r_pri_vb[i] <- res$r
  t6_pri$p_pri_vb[i] <- res$p
}
 
t6_pri$sig <- ifelse(t6_pri$p_pri_vb < 0.01, "***",
              ifelse(t6_pri$p_pri_vb < 0.05, "**",
              ifelse(t6_pri$p_pri_vb < 0.10, "*", "")))
 
# Also test against any VB
t6_pri$r_any_vb <- NA_real_
t6_pri$p_any_vb <- NA_real_
 
for (i in seq_along(within_vars)) {
  res <- calc_within_r(survey, within_vars[i], "any_vb", "secc")
  t6_pri$r_any_vb[i] <- res$r
  t6_pri$p_any_vb[i] <- res$p
}
 
t6_pri$sig_any <- ifelse(t6_pri$p_any_vb < 0.01, "***",
                  ifelse(t6_pri$p_any_vb < 0.05, "**",
                  ifelse(t6_pri$p_any_vb < 0.10, "*", "")))
 
t6_pri <- t6_pri %>% arrange(desc(abs(r_pri_vb)))
 
cat("Within-sección predictors (sorted by |r| for PRI VB):\n")
print(t6_pri %>% select(Predictor, r_any_vb, sig_any, r_pri_vb, sig))
 
write.csv(t6_pri, paste0(TABLE_DIR, "t6_within_comparison.csv"),
          row.names = FALSE)
 
##############################################################################
# 9. CROSS-PARTY CONTACT CORRELATIONS
##############################################################################
 
cat("\n================================================================\n")
cat("CROSS-PARTY CONTACT CORRELATIONS (individual level)\n")
cat("================================================================\n\n")
 
contact_matrix_vars <- c("contact_pan", "contact_pri", "contact_prd",
                          "contact_pvem", "contact_morena", "contact_mc")
contact_labels <- c("PAN", "PRI", "PRD", "PVEM", "MORENA", "MC")
 
contact_corr <- cor(survey[, contact_matrix_vars], use = "pairwise.complete.obs")
rownames(contact_corr) <- contact_labels
colnames(contact_corr) <- contact_labels
 
cat("Contact correlation matrix:\n")
print(round(contact_corr, 3))
 
off_diag <- contact_corr[upper.tri(contact_corr)]
cat("Mean off-diagonal r:", round(mean(off_diag), 3), "\n")
 
# Distribution of multi-party contact
cat("\nMulti-party contact distribution:\n")
cat(sprintf("  0 parties: %d (%.1f%%)\n",
            sum(survey$n_parties_contact == 0),
            mean(survey$n_parties_contact == 0) * 100))
cat(sprintf("  1 party: %d (%.1f%%)\n",
            sum(survey$n_parties_contact == 1),
            mean(survey$n_parties_contact == 1) * 100))
cat(sprintf("  2 parties: %d (%.1f%%)\n",
            sum(survey$n_parties_contact == 2),
            mean(survey$n_parties_contact == 2) * 100))
cat(sprintf("  3+ parties: %d (%.1f%%)\n",
            sum(survey$n_parties_contact >= 3),
            mean(survey$n_parties_contact >= 3) * 100))
 
##############################################################################
# 10. FIGURE 1: CONTACT RATES AND VB RATES BY PARTY
##############################################################################
 
fig1_data <- data.frame(
  party = factor(rep(party_names, 2),
                 levels = party_names),
  type = rep(c("Contacted by party", "Received gift from party"), each = 6),
  pct = c(
    sapply(contact_cols, function(x) mean(survey[[x]]) * 100),
    sapply(vb_cols, function(x) mean(survey[[x]]) * 100)
  )
)
 
fig1 <- ggplot(fig1_data, aes(x = party, y = pct, fill = type)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(pct, 0)),
            position = position_dodge(0.8), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Contacted by party" = "#2C5F8A",
                                "Received gift from party" = "#E67E22")) +
  labs(x = NULL, y = "Percent of respondents", fill = NULL,
       title = "Figure 1. Party Contact Rates and Vote Buying Rates") +
  ylim(0, 55) +
  theme_minimal() +
  theme(legend.position = "top")
 
ggsave(paste0(FIG_DIR, "fig1_contact_vb_rates.png"), fig1,
       width = 8, height = 4.5, dpi = 300)
cat("\nFigure 1 saved\n")
 
##############################################################################
# 11. FIGURE 2: CONTACT-TO-GIFT CONVERSION BY PARTY
##############################################################################
 
fig2_data <- data.frame(
  party = factor(rep(party_names, 2), levels = party_names),
  status = rep(c("Not contacted", "Contacted"), each = 6),
  vb_rate = NA_real_
)
 
for (i in seq_along(party_names)) {
  cv <- contact_cols[i]; vv <- vb_cols[i]
  fig2_data$vb_rate[i]     <- mean(survey[survey[[cv]] == 0, ][[vv]], na.rm = TRUE)
  fig2_data$vb_rate[i + 6] <- mean(survey[survey[[cv]] == 1, ][[vv]], na.rm = TRUE)
}
 
fig2 <- ggplot(fig2_data, aes(x = party, y = vb_rate, fill = status)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  scale_fill_manual(values = c("Not contacted" = "#BDC3C7",
                                "Contacted" = "#C0392B")) +
  labs(x = NULL, y = "P(gift from party)", fill = NULL,
       title = "Figure 2. Contact-to-Gift Conversion by Party") +
  ylim(0, 0.6) +
  theme_minimal() +
  theme(legend.position = "top")
 
ggsave(paste0(FIG_DIR, "fig2_conversion.png"), fig2,
       width = 8, height = 4.5, dpi = 300)
cat("Figure 2 saved\n")
 
##############################################################################
# 12. FIGURE 3: CONTACT MODES → PRI VB (among PRI-contacted)
##############################################################################
 
fig3_data <- t3 %>%
  mutate(
    diff_num = as.numeric(gsub("[*]", "", Difference)),
    Contact_mode = factor(Contact_mode, levels = Contact_mode[order(diff_num)]),
    sig_color = ifelse(p_value < 0.01, "p < 0.01",
                ifelse(p_value < 0.05, "p < 0.05",
                ifelse(p_value < 0.10, "p < 0.10", "Not sig.")))
  )
 
fig3 <- ggplot(fig3_data, aes(x = diff_num, y = Contact_mode, fill = sig_color)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_text(aes(label = Difference),
            hjust = ifelse(fig3_data$diff_num >= 0, -0.2, 1.2), size = 3.5) +
  scale_fill_manual(values = c("p < 0.01" = "#C0392B", "p < 0.05" = "#E67E22",
                                "p < 0.10" = "#F39C12", "Not sig." = "#BDC3C7")) +
  labs(x = "Difference in PRI VB rate (mode used vs. not used)",
       y = NULL, fill = NULL,
       title = "Figure 3. Which Contact Mode Predicts PRI Gift Receipt?",
       subtitle = sprintf("Among respondents contacted by PRI (N=%d)",
                           nrow(pri_contacted))) +
  theme_minimal() +
  theme(legend.position = "bottom")
 
ggsave(paste0(FIG_DIR, "fig3_contact_modes.png"), fig3,
       width = 7, height = 4.5, dpi = 300)
cat("Figure 3 saved\n")
 
##############################################################################
# 13. FIGURE 4: IS CONTACT NECESSARY? (stacked bar)
##############################################################################
 
fig4_data <- data.frame(
  party = character(), category = character(), share = numeric(),
  stringsAsFactors = FALSE
)
 
for (i in seq_along(party_names)) {
  pname <- party_names[i]; vv <- vb_cols[i]; cv <- contact_cols[i]
  fv <- party_f2f_vars[i]
  recip <- survey[survey[[vv]] == 1, ]
  n <- nrow(recip)
  if (n < 10) next
 
  f2f_pct  <- sum(recip[[fv]] == 1, na.rm = TRUE) / n
  cont_pct <- sum(recip[[cv]] == 1, na.rm = TRUE) / n
  other_pct <- cont_pct - f2f_pct
  no_pct   <- 1 - cont_pct
 
  fig4_data <- bind_rows(fig4_data, data.frame(
    party = rep(pname, 3),
    category = c("Face-to-face contact", "Other contact only", "No prior contact"),
    share = c(f2f_pct, other_pct, no_pct),
    stringsAsFactors = FALSE
  ))
}
 
fig4_data$party <- factor(fig4_data$party, levels = party_names)
fig4_data$category <- factor(fig4_data$category,
                              levels = c("No prior contact",
                                         "Other contact only",
                                         "Face-to-face contact"))
 
fig4 <- ggplot(fig4_data, aes(x = party, y = share, fill = category)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Face-to-face contact" = "#2C5F8A",
                                "Other contact only" = "#5DADE2",
                                "No prior contact" = "#E67E22")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share of gift recipients", fill = NULL,
       title = "Figure 4. Is Contact Necessary for Gift Receipt?",
       subtitle = "Prior contact status of gift recipients") +
  theme_minimal() +
  theme(legend.position = "top")
 
ggsave(paste0(FIG_DIR, "fig4_contact_necessary.png"), fig4,
       width = 8, height = 4.5, dpi = 300)
cat("Figure 4 saved\n")
 
##############################################################################
# 14. FIGURE 5: DOSE-RESPONSE + RALLY ATTENDANCE
##############################################################################
 
# Panel A: dose-response
dose_plot <- survey %>%
  group_by(n_parties_contact) %>%
  filter(n() > 10) %>%
  summarise(
    mean_vb = mean(n_parties_vb),
    se = sd(n_parties_vb) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )
 
fig5a <- ggplot(dose_plot, aes(x = n_parties_contact, y = mean_vb)) +
  geom_col(fill = "#2C5F8A", width = 0.7) +
  geom_errorbar(aes(ymin = mean_vb - 1.96 * se, ymax = mean_vb + 1.96 * se),
                width = 0.2) +
  geom_text(aes(label = sprintf("%.1f", mean_vb)), vjust = -0.8, size = 3) +
  labs(x = "Number of parties that contacted respondent",
       y = "Mean number of parties giving gifts",
       title = "(a) Dose-response: contact → gifts") +
  theme_minimal()
 
# Panel B: rally
rally_plot_data <- data.frame(
  group = factor(c("Non-\nattendees", "Rally\nattendees",
                    "Neighbor\nrecruited", "Transport\nhelp"),
                 levels = c("Non-\nattendees", "Rally\nattendees",
                            "Neighbor\nrecruited", "Transport\nhelp")),
  vb = c(mean(no_rally$any_vb),
         mean(rally$any_vb),
         mean(rally$any_vb[rally$rally_recruit == 1], na.rm = TRUE),
         mean(helped$any_vb)),
  n = c(nrow(no_rally), nrow(rally),
        sum(rally$rally_recruit == 1, na.rm = TRUE), nrow(helped))
)
 
fig5b <- ggplot(rally_plot_data, aes(x = group, y = vb)) +
  geom_col(fill = c("#BDC3C7", "#E67E22", "#C0392B", "#922B21"), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f\n(n=%d)", vb, n)),
            vjust = -0.3, size = 3) +
  labs(x = NULL, y = "Vote buying rate",
       title = "(b) Rally attendance and vote buying") +
  ylim(0, 0.9) +
  theme_minimal()
 
ggsave(paste0(FIG_DIR, "fig5a_dose_response.png"), fig5a,
       width = 5, height = 4, dpi = 300)
ggsave(paste0(FIG_DIR, "fig5b_rally.png"), fig5b,
       width = 5, height = 4, dpi = 300)
cat("Figure 5 panels saved\n")
 
##############################################################################
# 15. FIGURE 6: WITHIN-SECCIÓN — CONTACT AS STRONGEST PREDICTOR
##############################################################################
 
fig6_data <- t6_pri %>%
  mutate(Predictor = factor(Predictor,
                             levels = Predictor[order(r_pri_vb)]),
         sig_color = ifelse(p_pri_vb < 0.01, "p < 0.01",
                     ifelse(p_pri_vb < 0.05, "p < 0.05",
                     ifelse(p_pri_vb < 0.10, "p < 0.10", "Not sig."))))
 
fig6 <- ggplot(fig6_data, aes(x = r_pri_vb, y = Predictor, fill = sig_color)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_text(aes(label = sprintf("%+.3f%s", r_pri_vb, sig)),
            hjust = -0.2, size = 3.5) +
  scale_fill_manual(values = c("p < 0.01" = "#C0392B", "p < 0.05" = "#E67E22",
                                "p < 0.10" = "#F39C12", "Not sig." = "#BDC3C7")) +
  labs(x = "Within-sección correlation with PRI vote buying (r)",
       y = NULL, fill = NULL,
       title = "Figure 6. Contact as the Strongest Individual Predictor",
       subtitle = "Within-sección correlations, controlling for community-level factors") +
  xlim(-0.05, 0.35) +
  theme_minimal() +
  theme(legend.position = "bottom")
 
ggsave(paste0(FIG_DIR, "fig6_within_contact.png"), fig6,
       width = 8, height = 4.5, dpi = 300)
cat("Figure 6 saved\n")
 
##############################################################################
# 16. SUMMARY
##############################################################################
 
cat("\n")
cat("================================================================\n")
cat("PAPER 2 REPLICATION SUMMARY\n")
cat("================================================================\n")
cat("Sample:", nrow(survey), "respondents\n")
cat("\nContact rates: PRI", round(mean(survey$contact_pri) * 100, 1),
    "%, PAN", round(mean(survey$contact_pan) * 100, 1),
    "%, PRD", round(mean(survey$contact_prd) * 100, 1),
    "%, MORENA", round(mean(survey$contact_morena) * 100, 1), "%\n")
cat("\nConversion rates (P(gift|contacted)):\n")
cat("  PRI:", round(mean(survey$vb_pri[survey$contact_pri == 1]) * 100, 1), "%\n")
cat("  PAN:", round(mean(survey$vb_pan[survey$contact_pan == 1]) * 100, 1), "%\n")
cat("  PVEM:", round(mean(survey$vb_pvem[survey$contact_pvem == 1]) * 100, 1), "%\n")
cat("  MORENA:", round(mean(survey$vb_morena[survey$contact_morena == 1]) * 100, 1), "%\n")
cat("\nPhone > F2F for PRI:",
    round(mean(pri_contacted$vb_pri[pri_contacted$pri_phone == 1], na.rm = TRUE) -
          mean(pri_contacted$vb_pri[pri_contacted$pri_f2f == 1], na.rm = TRUE), 3), "\n")
cat("PRI gift recipients with NO contact:",
    sprintf("%.0f%%", mean(survey$contact_pri[survey$vb_pri == 1] == 0) * 100), "\n")
cat("\nRally: attendees", round(mean(rally$any_vb) * 100, 1),
    "% VB vs non-attendees", round(mean(no_rally$any_vb) * 100, 1), "%\n")
cat("N contacts → N gifts: r =", round(dose_r$estimate, 3), "\n")
cat("\nWithin-sección: PRI contact → PRI VB: r =",
    round(t6_pri$r_pri_vb[t6_pri$Predictor == "PRI contacted (any mode)"], 3), "\n")
cat("  (strongest individual predictor in the dataset)\n")
cat("================================================================\n")
cat("All figures saved to:", FIG_DIR, "\n")
cat("All tables saved to:", TABLE_DIR, "\n")
 
