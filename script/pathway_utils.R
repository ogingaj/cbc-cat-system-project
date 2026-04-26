# =============================================================================
# pathway_utils.R
# Pathway recommendation function — sourced by app.R
# Combines aptitude CAT scores with OCEAN personality profile
# =============================================================================

recommend_pathway_full <- function(cat_result, trait_scores, mapping_table,
                                   apt_weight = 0.70, pers_weight = 0.30) {
  
  # ── merge CAT results with mapping table ──────────────────────────────────
  df <- merge(cat_result, mapping_table, by.x = "item", by.y = "Item ID")
  
  # ── count items administered per pathway ──────────────────────────────────
  n_stem   <- sum(df$`Pathway Signal` == "STEM")
  n_social <- sum(df$`Pathway Signal` == "Social Sciences")
  n_arts   <- sum(df$`Pathway Signal` == "Arts & Sports")
  
  # ── aptitude score per pathway (proportion correct) ───────────────────────
  apt_stem   <- if (n_stem   > 0) sum(df$response[df$`Pathway Signal` == "STEM"])              / n_stem   else 0
  apt_social <- if (n_social > 0) sum(df$response[df$`Pathway Signal` == "Social Sciences"])  / n_social else 0
  apt_arts   <- if (n_arts   > 0) sum(df$response[df$`Pathway Signal` == "Arts & Sports"])    / n_arts   else 0
  
  # ── personality fit score per pathway ─────────────────────────────────────
  # Traits are on a standardised theta scale — weights reflect theoretical
  # alignment of each OCEAN trait to each CBC pathway
  O <- trait_scores$openness
  C <- trait_scores$conscientiousness
  E <- trait_scores$extraversion
  A <- trait_scores$agreeableness
  N <- trait_scores$neuroticism
  
  # STEM:           high conscientiousness + high openness (abstract thinking)
  # Social Sciences: high agreeableness + extraversion + openness (people focus)
  # Arts & Sports:  high openness + extraversion + low neuroticism (performance)
  pers_stem   <- 0.5 * C + 0.5 * O
  pers_social <- 0.4 * A + 0.3 * E + 0.3 * O
  pers_arts   <- 0.5 * O + 0.3 * E + 0.2 * (1 - abs(N))
  
  # ── rescale personality scores to 0–1 ─────────────────────────────────────
  # Needed so personality scores are on the same range as aptitude scores
  pers_all   <- c(pers_stem, pers_social, pers_arts)
  pers_range <- max(pers_all) - min(pers_all)
  
  if (pers_range > 0) {
    pers_stem   <- (pers_stem   - min(pers_all)) / pers_range
    pers_social <- (pers_social - min(pers_all)) / pers_range
    pers_arts   <- (pers_arts   - min(pers_all)) / pers_range
  }
  
  # ── weighted composite ────────────────────────────────────────────────────
  final_stem   <- apt_weight * apt_stem   + pers_weight * pers_stem
  final_social <- apt_weight * apt_social + pers_weight * pers_social
  final_arts   <- apt_weight * apt_arts   + pers_weight * pers_arts
  
  scores <- c(
    STEM               = final_stem,
    `Social Sciences`  = final_social,
    `Arts & Sports`    = final_arts
  )
  
  # ── return recommendation + all score breakdowns ──────────────────────────
  # scores, aptitude_scores, and personality_scores all returned separately
  # so the Shiny dashboard can display the full breakdown
  return(list(
    recommendation     = names(scores)[which.max(scores)],
    scores             = scores,
    aptitude_scores    = c(STEM = apt_stem,   `Social Sciences` = apt_social, `Arts & Sports` = apt_arts),
    personality_scores = c(STEM = pers_stem,  `Social Sciences` = pers_social, `Arts & Sports` = pers_arts)
  ))
}
