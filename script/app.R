# =============================================================================
# app.R — CBC CAT Pathway Recommendation Dashboard
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(fmsb)

source("pathway_utils.R")

# =============================================================================
# PRE-COMPUTE — results for all three demo students
# =============================================================================

student_list <- list(
  "Low Ability"  = list(cat = verb_low$result,  traits = trait_df[low_id,  ]),
  "Mid Ability"  = list(cat = verb_mid$result,  traits = trait_df[mid_id,  ]),
  "High Ability" = list(cat = verb_high$result, traits = trait_df[high_id, ])
)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  
  skin = "black",
  
  # ── header ──────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = "CBC CAT — Pathway Advisor"
  ),
  
  # ── sidebar ─────────────────────────────────────────────────────────────────
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pathway Recommendation", tabName = "recommendation",
               icon = icon("graduation-cap"))
    ),
    
    hr(),
    
    # student selector
    selectInput(
      inputId  = "student",
      label    = "Select Student Profile",
      choices  = names(student_list),
      selected = "Mid Ability"
    ),
    
    hr(),
    
    # aptitude vs personality weight slider
    sliderInput(
      inputId = "apt_weight",
      label   = "Aptitude Weight",
      min = 0.5, max = 1.0, value = 0.7, step = 0.05
    ),
    
    helpText(
      "Remaining weight goes to personality.",
      "Default: 70% aptitude / 30% personality.",
      style = "padding: 0 15px; color: #aaa; font-size: 12px;"
    )
  ),
  
  # ── body ────────────────────────────────────────────────────────────────────
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "recommendation",
              
              # ── row 1: summary value boxes ───────────────────────────────────────
              fluidRow(
                valueBoxOutput("box_recommendation", width = 4),
                valueBoxOutput("box_theta",          width = 4),
                valueBoxOutput("box_items",          width = 4)
              ),
              
              # ── row 2: composite scores + aptitude vs personality breakdown ───────
              fluidRow(
                
                box(
                  title       = "Composite Pathway Scores",
                  status      = "primary",
                  solidHeader = TRUE,
                  width       = 6,
                  plotOutput("plot_composite", height = "280px")
                ),
                
                box(
                  title       = "Aptitude vs Personality Contribution",
                  status      = "primary",
                  solidHeader = TRUE,
                  width       = 6,
                  plotOutput("plot_breakdown", height = "280px")
                )
              ),
              
              # ── row 3: OCEAN radar + CAT session table ────────────────────────────
              fluidRow(
                
                box(
                  title       = "OCEAN Personality Profile",
                  status      = "info",
                  solidHeader = TRUE,
                  width       = 6,
                  plotOutput("plot_ocean", height = "280px")
                ),
                
                box(
                  title       = "CAT Session Summary",
                  status      = "info",
                  solidHeader = TRUE,
                  width       = 6,
                  tableOutput("table_cat")
                )
              )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # ── reactive: recompute when student or weights change ──────────────────────
  rec <- reactive({
    s      <- student_list[[input$student]]
    apt_w  <- input$apt_weight
    pers_w <- round(1 - apt_w, 2)
    
    recommend_pathway_full(
      cat_result    = s$cat,
      trait_scores  = s$traits,
      mapping_table = mapping_table,
      apt_weight    = apt_w,
      pers_weight   = pers_w
    )
  })
  
  # ── reactive: selected student's CAT result ──────────────────────────────────
  cat_res <- reactive({
    student_list[[input$student]]$cat
  })
  
  # ── reactive: selected student's trait scores ────────────────────────────────
  traits <- reactive({
    student_list[[input$student]]$traits
  })
  
  # ── value box: recommended pathway ──────────────────────────────────────────
  output$box_recommendation <- renderValueBox({
    valueBox(
      value    = rec()$recommendation,
      subtitle = "Recommended Pathway",
      icon     = icon("compass"),
      color    = "navy"
    )
  })
  
  # ── value box: final theta and SE ───────────────────────────────────────────
  output$box_theta <- renderValueBox({
    df <- cat_res()
    valueBox(
      value    = round(tail(df$theta, 1), 3),
      subtitle = paste0("Final θ̂  (SE = ", round(tail(df$SE, 1), 3), ")"),
      icon     = icon("chart-line"),
      color    = "blue"
    )
  })
  
  # ── value box: items administered ───────────────────────────────────────────
  output$box_items <- renderValueBox({
    valueBox(
      value    = nrow(cat_res()),
      subtitle = "Items Administered",
      icon     = icon("list-check"),
      color    = "light-blue"
    )
  })
  
  # ── plot: composite pathway scores ──────────────────────────────────────────
  output$plot_composite <- renderPlot({
    
    df <- data.frame(
      pathway  = names(rec()$scores),
      score    = as.numeric(rec()$scores)
    )
    df$pathway  <- factor(df$pathway, levels = df$pathway[order(df$score)])
    df$selected <- df$pathway == rec()$recommendation
    
    ggplot(df, aes(x = pathway, y = score, fill = selected)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(score, 3)), hjust = -0.2, size = 4) +
      scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "#2c7bb6"),
                        guide  = "none") +
      coord_flip() +
      scale_y_continuous(limits = c(0, 1.15)) +
      labs(x = NULL, y = "Composite Score") +
      theme_minimal(base_size = 13)
  })
  
  # ── plot: aptitude vs personality side-by-side ───────────────────────────────
  output$plot_breakdown <- renderPlot({
    
    apt  <- data.frame(pathway = names(rec()$aptitude_scores),
                       score   = as.numeric(rec()$aptitude_scores),
                       type    = "Aptitude")
    pers <- data.frame(pathway = names(rec()$personality_scores),
                       score   = as.numeric(rec()$personality_scores),
                       type    = "Personality")
    df   <- rbind(apt, pers)
    
    ggplot(df, aes(x = pathway, y = score, fill = type)) +
      geom_col(position = "dodge", width = 0.6) +
      scale_fill_manual(
        values = c("Aptitude" = "#2c7bb6", "Personality" = "#abdda4"),
        name   = NULL
      ) +
      labs(x = NULL, y = "Score") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })
  
  # ── plot: OCEAN radar chart ──────────────────────────────────────────────────
  output$plot_ocean <- renderPlot({
    
    t <- traits()
    
    # fmsb needs max row, min row, then data — in that order
    radar_df <- rbind(
      max  = rep(2,  5),
      min  = rep(-2, 5),
      data.frame(
        Openness          = t$openness,
        Conscientiousness = t$conscientiousness,
        Extraversion      = t$extraversion,
        Agreeableness     = t$agreeableness,
        Neuroticism       = t$neuroticism
      )
    )
    
    radarchart(
      radar_df,
      axistype   = 1,
      pcol       = "#2c7bb6",
      pfcol      = adjustcolor("#2c7bb6", alpha.f = 0.3),
      plwd       = 2,
      cglcol     = "grey80",
      cglty      = 1,
      axislabcol = "grey50",
      vlcex      = 0.9,
      title      = paste("OCEAN Profile —", input$student)
    )
  })
  
  # ── table: CAT session step by step ─────────────────────────────────────────
  output$table_cat <- renderTable({
    df           <- cat_res()
    df$theta     <- round(df$theta, 3)
    df$SE        <- round(df$SE,    3)
    names(df)    <- c("Step", "Item", "Response", "θ̂", "SE")
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
}

# =============================================================================
shinyApp(ui = ui, server = server)