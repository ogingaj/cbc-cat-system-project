# =============================================================================
# CBC Junior Secondary CAT — Step 2: Simulate 500 Response Datasets
# DSAN 6550: Adaptive Measurement with AI — Final Project
# =============================================================================
# Two-stage simulation:
#   Stage 1: 30 Aptitude items  → 2PL IRT → dichotomous responses (0/1)
#   Stage 2: 26 Personality items → GRM   → polytomous responses (1–5)
#
# Key principle: responses are NOT random.
#   - Student ability theta ~ N(0,1)
#   - P(correct) = 2PL function of theta and item parameters
#   - So EASY items (low b) get more correct answers
#   - And HARD items (high b) get fewer correct answers
# =============================================================================

set.seed(42)
N <- 500  # number of simulated respondents

# =============================================================================
# 1. ITEM PARAMETERS
# =============================================================================

# ── Stage 1: Aptitude items (30) ──────────────────────────────────────────────
aptitude_items <- data.frame(
  item_id = c(
    "A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12",
    "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10",
    "C1","C2","C3","C4","C5","C6","C7","C8"
  ),
  domain = c(
    rep("STEM", 12), rep("SocialSciences", 10), rep("Arts", 8)
  ),
  b = c(
    -1.5,-1.2,-0.8,-0.5,-0.2, 0.0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8,
    -1.5,-1.0,-0.5,-0.2, 0.0, 0.3, 0.7, 1.0, 1.3, 1.7,
    -1.5,-1.0,-0.5, 0.0, 0.3, 0.8, 1.2, 1.6
  ),
  a = c(
    1.0,1.1,1.2,1.3,1.4,1.5,1.5,1.6,1.7,1.8,1.8,1.9,
    1.0,1.1,1.3,1.4,1.5,1.5,1.6,1.7,1.8,1.9,
    1.0,1.1,1.3,1.5,1.5,1.6,1.7,1.8
  ),
  stringsAsFactors = FALSE
)

# ── Stage 2: Personality items (26) ────────────────────────────────────────────
personality_items <- data.frame(
  item_id = c(
    "O1","O2","O3","O4","O5","O6",
    "C_1","C_2","C_3","C_4","C_5","C_6",
    "E1","E2","E3","E4","E5","E6",
    "A_1","A_2","A_3","A_4",
    "N1","N2","N3","N4"
  ),
  trait = c(
    rep("Openness", 6), rep("Conscientiousness", 6),
    rep("Extraversion", 6), rep("Agreeableness", 4), rep("Neuroticism", 4)
  ),
  reverse = c(
    FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,   # O
    FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,   # C
    FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,   # E
    FALSE,FALSE,TRUE,FALSE,               # A
    TRUE,FALSE,TRUE,FALSE                 # N
  ),
  a = c(
    1.2,1.3,1.5,1.4,1.6,1.5,
    1.2,1.3,1.5,1.4,1.6,1.5,
    1.2,1.3,1.5,1.4,1.6,1.5,
    1.2,1.4,1.4,1.5,
    1.2,1.3,1.5,1.4
  ),
  b_center = c(
    -1.0,-0.5, 0.0, 0.2, 0.5, 0.8,
    -1.0,-0.5, 0.0, 0.3, 0.5, 0.9,
    -1.0,-0.5, 0.0, 0.2, 0.5, 0.8,
    -0.8,-0.3, 0.3, 0.7,
    -0.8,-0.3, 0.2, 0.6
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# 2. SIMULATE STUDENT ABILITY AND TRAIT PROFILES
# =============================================================================

# General cognitive ability: theta ~ N(0,1)
theta <- rnorm(N, mean = 0, sd = 1)

# Each student has a true trait score on each OCEAN dimension
trait_scores <- data.frame(
  Openness          = rnorm(N, 0, 1),
  Conscientiousness = rnorm(N, 0, 1),
  Extraversion      = rnorm(N, 0, 1),
  Agreeableness     = rnorm(N, 0, 1),
  Neuroticism       = rnorm(N, 0, 1)
)

# =============================================================================
# 3. SIMULATE APTITUDE RESPONSES — 2PL IRT
# =============================================================================
# P(correct | theta, a, b) = 1 / (1 + exp(-a * (theta - b)))
# Response = 1 if Uniform(0,1) < P(correct), else 0

p_correct_2pl <- function(theta, a, b) {
  1 / (1 + exp(-a * (theta - b)))
}

aptitude_responses <- matrix(NA, nrow = N, ncol = nrow(aptitude_items))
colnames(aptitude_responses) <- aptitude_items$item_id

for (i in seq_len(nrow(aptitude_items))) {
  p <- p_correct_2pl(theta, aptitude_items$a[i], aptitude_items$b[i])
  aptitude_responses[, i] <- as.integer(runif(N) < p)
}

aptitude_responses <- as.data.frame(aptitude_responses)

# =============================================================================
# 4. SIMULATE PERSONALITY RESPONSES — GRM
# =============================================================================
# Graded Response Model:
#   4 thresholds divide the trait continuum into 5 response categories (1–5)
#   Thresholds = b_center + c(-1.5, -0.5, 0.5, 1.5)
#   P*(X >= k) = 1 / (1 + exp(-a * (trait - threshold_k)))
#   P(X = k)  = P*(X >= k) - P*(X >= k+1)

grm_category_probs <- function(trait_score, a, b_center) {
  offsets    <- c(-1.5, -0.5, 0.5, 1.5)
  thresholds <- b_center + offsets

  # Cumulative boundary probabilities: P*(X >= k) for k = 2,3,4,5
  p_star <- sapply(thresholds, function(t) {
    1 / (1 + exp(-a * (trait_score - t)))
  })  # N x 4 matrix

  # Category probabilities P(X = k) for k = 1..5
  p_k <- matrix(NA, nrow = length(trait_score), ncol = 5)
  p_k[, 1] <- 1 - p_star[, 1]
  for (k in 2:4) p_k[, k] <- p_star[, k-1] - p_star[, k]
  p_k[, 5] <- p_star[, 4]

  # Clip and renormalize (handle floating point)
  p_k <- pmax(p_k, 0)
  p_k <- p_k / rowSums(p_k)
  p_k
}

simulate_grm <- function(trait_score, a, b_center, reverse = FALSE) {
  p_k <- grm_category_probs(trait_score, a, b_center)
  responses <- sapply(seq_len(nrow(p_k)), function(i) {
    sample(1:5, size = 1, prob = p_k[i, ])
  })
  if (reverse) responses <- 6 - responses
  responses
}

personality_responses <- matrix(NA, nrow = N, ncol = nrow(personality_items))
colnames(personality_responses) <- personality_items$item_id

for (i in seq_len(nrow(personality_items))) {
  ts      <- trait_scores[[personality_items$trait[i]]]
  a       <- personality_items$a[i]
  b_ctr   <- personality_items$b_center[i]
  rev_flg <- personality_items$reverse[i]
  personality_responses[, i] <- simulate_grm(ts, a, b_ctr, reverse = rev_flg)
}

personality_responses <- as.data.frame(personality_responses)

# =============================================================================
# 5. ASSEMBLE DATASETS
# =============================================================================

student_ids <- paste0("S", formatC(1:N, width = 3, flag = "0"))

# Full combined dataset (all 56 items)
full_data <- cbind(
  student_id = student_ids,
  aptitude_responses,
  personality_responses
)

# True latent scores (for validation/calibration reference only)
latent_scores <- data.frame(
  student_id            = student_ids,
  true_theta            = round(theta, 4),
  true_Openness         = round(trait_scores$Openness, 4),
  true_Conscientiousness= round(trait_scores$Conscientiousness, 4),
  true_Extraversion     = round(trait_scores$Extraversion, 4),
  true_Agreeableness    = round(trait_scores$Agreeableness, 4),
  true_Neuroticism      = round(trait_scores$Neuroticism, 4)
)

# =============================================================================
# 6. QUICK VALIDATION CHECKS
# =============================================================================

cat("=============================================================\n")
cat("SIMULATION VALIDATION\n")
cat("=============================================================\n\n")

cat("Theta distribution:\n")
cat(sprintf("  Mean = %.3f  (expected ~0.000)\n", mean(theta)))
cat(sprintf("  SD   = %.3f  (expected ~1.000)\n", sd(theta)))
cat(sprintf("  Min  = %.3f  |  Max = %.3f\n\n", min(theta), max(theta)))

cat("Aptitude P(correct) — verifying easy items have higher rates:\n")
cat(sprintf("  %-6s  %5s  %10s  %8s\n", "Item", "b", "P(correct)", "#Correct"))
cat(paste(rep("-", 38), collapse=""), "\n")
for (i in seq_len(nrow(aptitude_items))) {
  item <- aptitude_items$item_id[i]
  b    <- aptitude_items$b[i]
  p    <- mean(aptitude_responses[[item]])
  n    <- sum(aptitude_responses[[item]])
  cat(sprintf("  %-6s  %5.1f  %10.3f  %8d\n", item, b, p, n))
}

cat("\nP(correct) by domain:\n")
for (dom in c("STEM","SocialSciences","Arts")) {
  items   <- aptitude_items$item_id[aptitude_items$domain == dom]
  mean_p  <- mean(colMeans(aptitude_responses[, items]))
  cat(sprintf("  %-16s: %.3f\n", dom, mean_p))
}

cat("\nPersonality trait mean scores (expected ~3.0):\n")
for (tr in c("Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism")) {
  items   <- personality_items$item_id[personality_items$trait == tr]
  mean_sc <- mean(colMeans(personality_responses[, items]))
  cat(sprintf("  %-20s: %.3f\n", tr, mean_sc))
}

# =============================================================================
# 7. SAVE OUTPUTS
# =============================================================================

write.csv(full_data,             "CBC_CAT_responses_full.csv",        row.names = FALSE)
write.csv(aptitude_responses |> cbind(student_id = student_ids, ...=_),
          "CBC_CAT_aptitude_responses.csv",   row.names = FALSE)
write.csv(cbind(student_id = student_ids, aptitude_responses),
          "CBC_CAT_aptitude_responses.csv",   row.names = FALSE)
write.csv(cbind(student_id = student_ids, personality_responses),
          "CBC_CAT_personality_responses.csv", row.names = FALSE)
write.csv(latent_scores,         "CBC_CAT_latent_scores.csv",         row.names = FALSE)

cat("\nFiles saved:\n")
cat("  CBC_CAT_responses_full.csv         (500 students x 56 items)\n")
cat("  CBC_CAT_aptitude_responses.csv     (500 x 30 aptitude)\n")
cat("  CBC_CAT_personality_responses.csv  (500 x 26 personality)\n")
cat("  CBC_CAT_latent_scores.csv          (true theta + OCEAN scores)\n")
cat("\nSimulation complete!\n")
