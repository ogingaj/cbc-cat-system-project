# =============================================================================
# pathway_utils.R
#
# Two separate functions reflecting a clean design separation:
#
#   1. recommend_pathway_full() — aptitude-only pathway recommendation
#      Uses IRT hybrid scoring: actual responses for administered items,
#      expected P(correct | θ) for unadministered items. Personality plays
#      no role in the recommendation.
#
#   2. describe_ocean_profile() — personality context (descriptive only)
#      Takes OCEAN trait scores and returns trait labels and a plain-language
#      narrative about the student's learning style. Shown alongside the
#      recommendation as supplementary context, not as a scoring input.
# =============================================================================


# =============================================================================
# FUNCTION 1: PATHWAY RECOMMENDATION (aptitude only)
# =============================================================================

recommend_pathway_full <- function(cat_result,     # data.frame: step, item, response, theta, SE
                                   mapping_table,  # item-to-pathway mapping (fallback only)
                                   item_params = NULL) {  # data.frame: item, a, b, pathway
  
  if (!is.null(item_params)) {
    
    # Final ability estimate from the CAT session
    theta_hat <- tail(cat_result$theta, 1)
    
    # Step 1: IRT expected P(correct | θ, a, b) as baseline score for every item
    item_params$score <- 1 / (1 + exp(-item_params$a * (theta_hat - item_params$b)))
    
    # Step 2: Replace expected score with actual response for administered items
    # Correct answer on a hard item (response = 1, P ≈ 0.6) boosts that domain
    # Wrong answer (response = 0) penalises it below the IRT expectation
    for (i in seq_len(nrow(cat_result))) {
      idx <- which(item_params$item == cat_result$item[i])
      if (length(idx) > 0) item_params$score[idx] <- cat_result$response[i]
    }
    
    # Step 3: Average hybrid score per domain across the full item bank
    apt_stem   <- mean(item_params$score[item_params$pathway == "STEM"])
    apt_social <- mean(item_params$score[item_params$pathway == "Social Sciences"])
    apt_arts   <- mean(item_params$score[item_params$pathway == "Arts & Sports"])
    
    # Step 4: Normalize to [0, 1] so domains are on a comparable scale
    apt_all   <- c(apt_stem, apt_social, apt_arts)
    apt_range <- max(apt_all) - min(apt_all)
    if (apt_range > 0) {
      apt_stem   <- (apt_stem   - min(apt_all)) / apt_range
      apt_social <- (apt_social - min(apt_all)) / apt_range
      apt_arts   <- (apt_arts   - min(apt_all)) / apt_range
    } else {
      # No meaningful differentiation — all domains equally likely
      apt_stem <- apt_social <- apt_arts <- 1/3
    }
    
  } else {
    
    # Fallback: proportion correct on administered items only
    # (used when item_params not supplied)
    df         <- merge(cat_result, mapping_table, by.x = "item", by.y = "Item ID")
    n_stem     <- sum(df$`Pathway Signal` == "STEM")
    n_social   <- sum(df$`Pathway Signal` == "Social Sciences")
    n_arts     <- sum(df$`Pathway Signal` == "Arts & Sports")
    apt_stem   <- if (n_stem   > 0) sum(df$response[df$`Pathway Signal` == "STEM"])            / n_stem   else 0
    apt_social <- if (n_social > 0) sum(df$response[df$`Pathway Signal` == "Social Sciences"]) / n_social else 0
    apt_arts   <- if (n_arts   > 0) sum(df$response[df$`Pathway Signal` == "Arts & Sports"])   / n_arts   else 0
  }
  
  scores <- c(STEM              = apt_stem,
              `Social Sciences` = apt_social,
              `Arts & Sports`   = apt_arts)
  
  return(list(
    recommendation = names(scores)[which.max(scores)],
    scores         = scores
  ))
}


# =============================================================================
# FUNCTION 2: OCEAN PERSONALITY PROFILE (descriptive context only)
# =============================================================================

describe_ocean_profile <- function(trait_scores) {  # data.frame: one row of OCEAN scores
  
  O <- trait_scores$openness
  C <- trait_scores$conscientiousness
  E <- trait_scores$extraversion
  A <- trait_scores$agreeableness
  N <- trait_scores$neuroticism
  
  # ── Trait labels ────────────────────────────────────────────────────────────
  # Classify each trait as High / Moderate / Low based on IRT theta scale
  # Thresholds: above 0.5 = High, below -0.5 = Low, otherwise Moderate
  label <- function(x) {
    if      (x >  0.5) "High"
    else if (x < -0.5) "Low"
    else                "Moderate"
  }
  
  labels <- c(
    Openness          = label(O),
    Conscientiousness = label(C),
    Extraversion      = label(E),
    Agreeableness     = label(A),
    Neuroticism       = label(N)
  )
  
  # ── Plain-language narrative ─────────────────────────────────────────────────
  # Describes the student's learning style based on salient traits.
  # Only traits scoring High or Low generate a statement; Moderate traits
  # are unremarkable and do not add noise to the narrative.
  narrative <- character(0)
  
  if (O >  0.5) narrative <- c(narrative, "Curious and imaginative — engages well with novel problems and creative tasks.")
  if (O < -0.5) narrative <- c(narrative, "Prefers structured, familiar tasks over open-ended exploration.")
  
  if (C >  0.5) narrative <- c(narrative, "Organised and self-disciplined — well suited to rigorous, goal-oriented study.")
  if (C < -0.5) narrative <- c(narrative, "May benefit from structured support and clear deadlines.")
  
  if (E >  0.5) narrative <- c(narrative, "Sociable and energetic — likely to thrive in collaborative or performance-based settings.")
  if (E < -0.5) narrative <- c(narrative, "Prefers independent work and quieter learning environments.")
  
  if (A >  0.5) narrative <- c(narrative, "Cooperative and empathetic — strong fit for team-based or community-oriented work.")
  if (A < -0.5) narrative <- c(narrative, "Independent-minded; may prefer competitive or individually-focused settings.")
  
  if (N >  0.5) narrative <- c(narrative, "May experience stress under pressure — benefits from a supportive learning environment.")
  if (N < -0.5) narrative <- c(narrative, "Emotionally stable and resilient under pressure.")
  
  # Fallback if all traits are moderate
  if (length(narrative) == 0) {
    narrative <- "Broadly moderate across all personality dimensions — adaptable to a range of learning environments."
  }
  
  # ── Pathway personality alignment (descriptive, not prescriptive) ───────────
  # Indicates which pathway the student's personality profile most resembles.
  # This is shown as context only — it does not override the aptitude recommendation.
  pers_stem   <- 0.5 * C + 0.5 * O
  pers_social <- 0.4 * A + 0.3 * E + 0.3 * O
  pers_arts   <- 0.5 * O + 0.3 * E + 0.2 * (1 - abs(N))
  
  pers_scores      <- c(STEM = pers_stem, `Social Sciences` = pers_social, `Arts & Sports` = pers_arts)
  personality_lean <- names(pers_scores)[which.max(pers_scores)]
  
  return(list(
    labels           = labels,           # named vector: trait → High/Moderate/Low
    narrative        = narrative,        # character vector of descriptive sentences
    personality_lean = personality_lean  # which pathway personality most resembles
  ))
}