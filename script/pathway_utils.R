# =============================================================================
# pathway_utils.R
# Pathway recommendation logic for the CBC CAT Pathway Advisor
#
# Main function: recommend_pathway_full()
#   - Combines aptitude (70%) and personality (30%) into a composite score
#   - Aptitude uses a hybrid scoring approach:
#       * Administered items  → actual response (0 or 1)
#       * Unadministered items → IRT expected P(correct | θ, a, b)
#     This ensures correct answers on hard items boost the relevant domain,
#     and wrong answers penalize it, regardless of item difficulty.
#   - Personality uses OCEAN trait weights per pathway
#   - Both aptitude and personality scores are normalized to [0, 1]
#     before combining so neither scale dominates
# =============================================================================

recommend_pathway_full <- function(cat_result,     # data.frame: step, item, response, theta, SE
                                   trait_scores,   # data.frame: one row with OCEAN trait scores
                                   mapping_table,  # item-to-pathway mapping (fallback only)
                                   apt_weight  = 0.70,
                                   pers_weight = 0.30,
                                   item_params = NULL) {  # data.frame: item, a, b, pathway
  
  # ---------------------------------------------------------------------------
  # APTITUDE SCORING
  # ---------------------------------------------------------------------------
  
  if (!is.null(item_params)) {
    
    # Final theta estimate from the CAT session
    theta_hat <- tail(cat_result$theta, 1)
    
    # Step 1: Assign every item in the bank its IRT expected score
    # P(correct | θ, a, b) = 1 / (1 + exp(−a(θ − b)))
    # This is the 2PL probability of a correct response given ability θ
    item_params$score <- 1 / (1 + exp(-item_params$a * (theta_hat - item_params$b)))
    
    # Step 2: Replace expected scores with actual responses for administered items
    # A correct response (1) on a hard item scores higher than its expected P,
    # boosting that domain. A wrong response (0) scores lower, penalising it.
    for (i in seq_len(nrow(cat_result))) {
      idx <- which(item_params$item == cat_result$item[i])
      if (length(idx) > 0) item_params$score[idx] <- cat_result$response[i]
    }
    
    # Step 3: Average score per domain across all items in the bank
    # Domains with more correct responses on hard items score higher
    apt_stem   <- mean(item_params$score[item_params$pathway == "STEM"])
    apt_social <- mean(item_params$score[item_params$pathway == "Social Sciences"])
    apt_arts   <- mean(item_params$score[item_params$pathway == "Arts & Sports"])
    
    # Step 4: Normalize aptitude scores to [0, 1]
    # Ensures that the three domains are compared on the same scale
    # before applying the aptitude weight in the composite
    apt_all   <- c(apt_stem, apt_social, apt_arts)
    apt_range <- max(apt_all) - min(apt_all)
    if (apt_range > 0) {
      apt_stem   <- (apt_stem   - min(apt_all)) / apt_range
      apt_social <- (apt_social - min(apt_all)) / apt_range
      apt_arts   <- (apt_arts   - min(apt_all)) / apt_range
    } else {
      # All three domains indistinguishable — let personality decide
      apt_stem <- apt_social <- apt_arts <- 0.5
    }
    
  } else {
    
    # ---------------------------------------------------------------------------
    # FALLBACK: proportion correct on administered items only
    # Used when item_params is not supplied (e.g., demo mode without IRT params)
    # Note: this approach can be biased by item difficulty distribution
    # ---------------------------------------------------------------------------
    df         <- merge(cat_result, mapping_table, by.x = "item", by.y = "Item ID")
    n_stem     <- sum(df$`Pathway Signal` == "STEM")
    n_social   <- sum(df$`Pathway Signal` == "Social Sciences")
    n_arts     <- sum(df$`Pathway Signal` == "Arts & Sports")
    apt_stem   <- if (n_stem   > 0) sum(df$response[df$`Pathway Signal` == "STEM"])            / n_stem   else 0
    apt_social <- if (n_social > 0) sum(df$response[df$`Pathway Signal` == "Social Sciences"]) / n_social else 0
    apt_arts   <- if (n_arts   > 0) sum(df$response[df$`Pathway Signal` == "Arts & Sports"])   / n_arts   else 0
  }
  
  # ---------------------------------------------------------------------------
  # PERSONALITY SCORING
  # OCEAN trait weights reflect empirical associations between traits
  # and success/engagement in each pathway
  # ---------------------------------------------------------------------------
  
  O <- trait_scores$openness
  C <- trait_scores$conscientiousness
  E <- trait_scores$extraversion
  A <- trait_scores$agreeableness
  N <- trait_scores$neuroticism
  
  # STEM:          high conscientiousness (structured problem solving)
  #                + openness (curiosity, abstract thinking)
  pers_stem   <- 0.5 * C + 0.5 * O
  
  # Social Sciences: high agreeableness (empathy, collaboration)
  #                  + extraversion (communication, civic engagement)
  #                  + openness (broad intellectual interest)
  pers_social <- 0.4 * A + 0.3 * E + 0.3 * O
  
  # Arts & Sports:   high openness (creativity, expression)
  #                  + extraversion (performance, teamwork)
  #                  + low neuroticism (emotional regulation under pressure)
  pers_arts   <- 0.5 * O + 0.3 * E + 0.2 * (1 - abs(N))
  
  # Normalize personality scores to [0, 1] so the scale matches aptitude
  pers_all   <- c(pers_stem, pers_social, pers_arts)
  pers_range <- max(pers_all) - min(pers_all)
  if (pers_range > 0) {
    pers_stem   <- (pers_stem   - min(pers_all)) / pers_range
    pers_social <- (pers_social - min(pers_all)) / pers_range
    pers_arts   <- (pers_arts   - min(pers_all)) / pers_range
  }
  
  # ---------------------------------------------------------------------------
  # COMPOSITE SCORE
  # Weighted combination of aptitude and personality per pathway
  # Default: 70% aptitude, 30% personality
  # ---------------------------------------------------------------------------
  final_stem   <- apt_weight * apt_stem   + pers_weight * pers_stem
  final_social <- apt_weight * apt_social + pers_weight * pers_social
  final_arts   <- apt_weight * apt_arts   + pers_weight * pers_arts
  
  scores <- c(STEM             = final_stem,
              `Social Sciences` = final_social,
              `Arts & Sports`   = final_arts)
  
  # Return recommendation, full score breakdown, and component scores for display
  return(list(
    recommendation     = names(scores)[which.max(scores)],
    scores             = scores,
    aptitude_scores    = c(STEM = apt_stem, `Social Sciences` = apt_social, `Arts & Sports` = apt_arts),
    personality_scores = c(STEM = pers_stem, `Social Sciences` = pers_social, `Arts & Sports` = pers_arts)
  ))
}