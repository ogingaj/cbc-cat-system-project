# =============================================================================
# app.R — CBC CAT Pathway Recommendation Dashboard
# Tab 1: Demo Results | Tab 2: Live Adaptive Test
# =============================================================================

load("cat_objects.RData")
source("pathway_utils.R")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(fmsb)
library(mirt)
library(dplyr)

# =============================================================================
# ITEM BANK — question text, choices, correct answer, IRT parameters
# =============================================================================

item_bank <- list(
  
  # ── STEM ───────────────────────────────────────────────────────────────────
  A1 = list(
    text    = "A maize farmer in Nakuru harvests 240 kg of maize from 3 equal plots. How many kilograms does each plot produce?",
    choices = c(A = "60 kg", B = "80 kg", C = "90 kg", D = "120 kg"),
    correct = "B", a = 1.0, b = -1.5, pathway = "STEM"
  ),
  A2 = list(
    text    = "Look at the number pattern: 2, 4, 8, 16, ___. What is the next number?",
    choices = c(A = "18", B = "24", C = "32", D = "64"),
    correct = "C", a = 1.1, b = -1.2, pathway = "STEM"
  ),
  A3 = list(
    text    = "A water tank holds 500 litres when full. After a family uses 35% of the water, how many litres remain?",
    choices = c(A = "165 litres", B = "175 litres", C = "325 litres", D = "350 litres"),
    correct = "C", a = 1.2, b = -0.8, pathway = "STEM"
  ),
  A4 = list(
    text    = "A plant is placed near a window. After one week, its stem has bent toward the light. This is an example of:",
    choices = c(A = "Photosynthesis", B = "Tropism", C = "Respiration", D = "Osmosis"),
    correct = "B", a = 1.3, b = -0.5, pathway = "STEM"
  ),
  A5 = list(
    text    = "Which of the following completes the sequence? △ ○ △ △ ○ △ △ △ ○ ___",
    choices = c(A = "△", B = "○", C = "△ △ △ △", D = "○ △"),
    correct = "C", a = 1.4, b = -0.2, pathway = "STEM"
  ),
  A6 = list(
    text    = "A car travels 120 km in 2 hours. At the same speed, how long will it take to travel 300 km?",
    choices = c(A = "4 hours", B = "4.5 hours", C = "5 hours", D = "6 hours"),
    correct = "C", a = 1.5, b = 0.0, pathway = "STEM"
  ),
  A7 = list(
    text    = "If all Zs are Ys, and all Ys are Xs, which statement must be true?",
    choices = c(A = "All Xs are Zs", B = "All Xs are Ys", C = "All Zs are Xs", D = "No Xs are Zs"),
    correct = "C", a = 1.5, b = 0.3, pathway = "STEM"
  ),
  A8 = list(
    text    = "A scientist dissolves salt in water and then heats the mixture until the water evaporates completely. What remains?",
    choices = c(A = "Nothing — both disappear", B = "Only water vapour", C = "The salt", D = "A new chemical compound"),
    correct = "C", a = 1.6, b = 0.6, pathway = "STEM"
  ),
  A9 = list(
    text    = "A rectangle has a perimeter of 36 cm. If its length is twice its width, what is the area of the rectangle?",
    choices = c(A = "72 cm²", B = "96 cm²", C = "108 cm²", D = "144 cm²"),
    correct = "A", a = 1.7, b = 0.9, pathway = "STEM"
  ),
  A10 = list(
    text    = "Three gears are connected in a line. Gear A has 8 teeth, Gear B has 16 teeth, and Gear C has 8 teeth. If Gear A rotates 4 times, how many times does Gear C rotate?",
    choices = c(A = "2 times", B = "4 times", C = "8 times", D = "16 times"),
    correct = "B", a = 1.8, b = 1.2, pathway = "STEM"
  ),
  A11 = list(
    text    = "A code uses the rule: each letter is replaced by the letter 3 positions ahead in the alphabet (A→D, B→E...). What does the coded word 'PDWKV' decode to?",
    choices = c(A = "LEARN", B = "MATHS", C = "READS", D = "LOGIC"),
    correct = "B", a = 1.8, b = 1.5, pathway = "STEM"
  ),
  A12 = list(
    text    = "A student records water temperature every 2 minutes: 20°C, 35°C, 50°C, 65°C. If the pattern continues, at what time will it reach 95°C?",
    choices = c(A = "At 8 minutes", B = "At 10 minutes", C = "At 12 minutes", D = "At 14 minutes"),
    correct = "B", a = 1.9, b = 1.8, pathway = "STEM"
  ),
  
  # ── SOCIAL SCIENCES ────────────────────────────────────────────────────────
  B1 = list(
    text    = "The village elder called a baraza (community meeting) to discuss a new road. Many residents came to share their views. What is the main purpose of the baraza?",
    choices = c(A = "To collect taxes", B = "To allow community members to share their views", C = "To elect a new village elder", D = "To announce government decisions"),
    correct = "B", a = 1.0, b = -1.5, pathway = "Social Sciences"
  ),
  B2 = list(
    text    = "When a country's population grows faster than its food production, which of the following is the most likely result?",
    choices = c(A = "The country becomes wealthier", B = "Food prices decrease", C = "Food shortages may occur", D = "People eat more nutritious food"),
    correct = "C", a = 1.1, b = -1.0, pathway = "Social Sciences"
  ),
  B3 = list(
    text    = "A new hospital is built in a remote village in Turkana. Which is the most likely long-term effect on the community?",
    choices = c(A = "More people will move away", B = "School attendance will decrease", C = "Child mortality rates are likely to reduce", D = "Farming activity will increase"),
    correct = "C", a = 1.3, b = -0.5, pathway = "Social Sciences"
  ),
  B4 = list(
    text    = "After independence, many African countries kept the borders drawn by colonial powers, even though these borders split ethnic communities. What is the most likely reason?",
    choices = c(A = "The borders were scientifically accurate", B = "Changing borders would have caused conflict between new nations", C = "African communities preferred to be divided", D = "Colonial powers refused to let borders change"),
    correct = "B", a = 1.4, b = -0.2, pathway = "Social Sciences"
  ),
  B5 = list(
    text    = "A county government decides to build a market near a school. What is the most likely reason for this decision?",
    choices = c(A = "To distract students from studying", B = "To make it easier for teachers to shop", C = "To boost local economic activity and serve the nearby population", D = "To reduce the number of students in the school"),
    correct = "C", a = 1.5, b = 0.0, pathway = "Social Sciences"
  ),
  B6 = list(
    text    = "In a democracy, why is freedom of the press considered important?",
    choices = c(A = "It allows journalists to earn more money", B = "It enables citizens to receive information and hold leaders accountable", C = "It gives the government control over public opinion", D = "It prevents foreign interference"),
    correct = "B", a = 1.5, b = 0.3, pathway = "Social Sciences"
  ),
  B7 = list(
    text    = "Between 1900 and 1950, many Kenyans moved from rural areas to cities (urbanization). Which best explains why urbanization accelerated after independence in 1963?",
    choices = c(A = "Rural areas became more dangerous", B = "The government forced people to move", C = "Cities offered more employment and educational opportunities", D = "Farming became illegal after independence"),
    correct = "C", a = 1.6, b = 0.7, pathway = "Social Sciences"
  ),
  B8 = list(
    text    = "A country imposes a high tax on imported goods. What is the most likely intended effect?",
    choices = c(A = "To encourage citizens to buy more foreign goods", B = "To protect local industries from foreign competition", C = "To reduce the country's tax revenue", D = "To strengthen trade relationships"),
    correct = "B", a = 1.7, b = 1.0, pathway = "Social Sciences"
  ),
  B9 = list(
    text    = "Community A (communal land ownership) had lower inequality than Community B (individual ownership). Which conclusion is best supported?",
    choices = c(A = "Individual land ownership always leads to poverty", B = "Communal land ownership may reduce wealth inequality", C = "Community B residents work harder", D = "Land ownership has no effect on inequality"),
    correct = "B", a = 1.8, b = 1.3, pathway = "Social Sciences"
  ),
  B10 = list(
    text    = "A government requires companies with 50+ employees to hire 30% of staff from people under 30. What is a likely unintended consequence?",
    choices = c(A = "Youth unemployment immediately disappears", B = "Companies may choose to stay small to avoid the requirement", C = "Older workers will receive higher salaries", D = "Foreign companies will invest more"),
    correct = "B", a = 1.9, b = 1.7, pathway = "Social Sciences"
  ),
  
  # ── ARTS & SPORTS ──────────────────────────────────────────────────────────
  C1 = list(
    text    = "A dancer learns a sequence: step right, clap, step left, clap, jump. If the sequence repeats, what is the 8th movement?",
    choices = c(A = "Step right", B = "Clap", C = "Step left", D = "Jump"),
    correct = "C", a = 1.0, b = -1.5, pathway = "Arts & Sports"
  ),
  C2 = list(
    text    = "A painter uses only red and blue paint. Mixing them equally produces purple. If the painter wants a darker purple, what should they add more of?",
    choices = c(A = "Red", B = "Blue", C = "White", D = "Yellow"),
    correct = "B", a = 1.1, b = -1.0, pathway = "Arts & Sports"
  ),
  C3 = list(
    text    = "Look at the shapes: ■ ■ □ ■ ■ □ ■ ■ □ ___. What comes next?",
    choices = c(A = "■", B = "□", C = "■ ■", D = "□ □"),
    correct = "B", a = 1.3, b = -0.5, pathway = "Arts & Sports"
  ),
  C4 = list(
    text    = "A sprinter's finish times over 4 weeks: 14.2s, 13.8s, 13.4s, 13.0s. What is the expected time in week 5 if the pattern continues?",
    choices = c(A = "12.4s", B = "12.6s", C = "12.8s", D = "13.2s"),
    correct = "B", a = 1.5, b = 0.0, pathway = "Arts & Sports"
  ),
  C5 = list(
    text    = "A sculptor makes one straight cut through the middle of a cube. How many faces does each resulting piece have?",
    choices = c(A = "4", B = "5", C = "6", D = "7"),
    correct = "B", a = 1.5, b = 0.3, pathway = "Arts & Sports"
  ),
  C6 = list(
    text    = "A football coach notices her team scores more goals in the second half. Which explanation is most supported by sports science?",
    choices = c(A = "The opposing team always gets tired first", B = "The team benefits from tactical adjustments made at half-time", C = "Football rules give advantages in the second half", D = "The weather always improves in the second half"),
    correct = "B", a = 1.6, b = 0.8, pathway = "Arts & Sports"
  ),
  C7 = list(
    text    = "A designer places an equilateral triangle inside a circle so all three corners touch the circle. What is the relationship between the triangle's center and the circle's center?",
    choices = c(A = "They are at different points", B = "The triangle's center is outside the circle", C = "They are the same point", D = "The circle's center is on the triangle's edge"),
    correct = "C", a = 1.7, b = 1.2, pathway = "Arts & Sports"
  ),
  C8 = list(
    text    = "A choreographer designs a routine where 4 dancers start at the corners of a square stage. Every 8 beats, each dancer moves to the next corner clockwise. After 24 beats, which corner does the dancer who started at the top-left occupy?",
    choices = c(A = "Top-left (back to start)", B = "Top-right", C = "Bottom-right", D = "Bottom-left"),
    correct = "D",   # was "C" — corrected to "D"
    a = 1.8, b = 1.6, pathway = "Arts & Sports"
  )
)

# =============================================================================
# PRE-COMPUTATION
# =============================================================================

# item IDs in model order — use rownames from model to guarantee match
params_live    <- coef(model_2pl, IRTpars = TRUE, simplify = TRUE)$items
all_items_live <- rownames(params_live)

# average OCEAN profile across all 500 simulated students
avg_ocean <- data.frame(
  openness          = mean(trait_df$openness),
  conscientiousness = mean(trait_df$conscientiousness),
  extraversion      = mean(trait_df$extraversion),
  agreeableness     = mean(trait_df$agreeableness),
  neuroticism       = mean(trait_df$neuroticism)
)

# pre-computed demo student results (Tab 1)
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
  
  dashboardHeader(title = "CBC CAT — Pathway Advisor"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Demo Results",  tabName = "demo", icon = icon("chart-bar")),
      menuItem("Take the Test", tabName = "live", icon = icon("pencil"))
    ),
    
    # sidebar controls only shown on Tab 1
    conditionalPanel(
      condition = "input.tabs === 'demo'",
      hr(),
      selectInput("student", "Select Student Profile",
                  choices = names(student_list), selected = "Mid Ability"),
      hr(),
      sliderInput("apt_weight", "Aptitude Weight",
                  min = 0.5, max = 1.0, value = 0.7, step = 0.05),
      helpText("Remaining weight goes to personality.",
               "Default: 70% aptitude / 30% personality.",
               style = "padding: 0 15px; color: #aaa; font-size: 12px;")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── TAB 1: DEMO RESULTS ─────────────────────────────────────────────────
      tabItem(tabName = "demo",
              
              fluidRow(
                valueBoxOutput("box_recommendation", width = 4),
                valueBoxOutput("box_theta",          width = 4),
                valueBoxOutput("box_items",          width = 4)
              ),
              
              fluidRow(
                box(title = "Composite Pathway Scores", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotOutput("plot_composite", height = "280px")),
                box(title = "Aptitude vs Personality Contribution", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotOutput("plot_breakdown", height = "280px"))
              ),
              
              fluidRow(
                box(title = "OCEAN Personality Profile", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotOutput("plot_ocean", height = "280px")),
                box(title = "CAT Session Summary", status = "info",
                    solidHeader = TRUE, width = 6,
                    tableOutput("table_cat"))
              )
      ),
      
      # ── TAB 2: LIVE CAT ─────────────────────────────────────────────────────
      tabItem(tabName = "live",
              uiOutput("cat_ui")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # ── CAT reactive state ──────────────────────────────────────────────────────
  cs <- reactiveValues(
    started      = FALSE,
    complete     = FALSE,
    theta        = 0,
    se           = NA,
    administered = character(0),
    responses    = numeric(0),
    theta_hist   = numeric(0),
    se_hist      = numeric(0),
    current      = NULL
  )
  
  # ── start test ──────────────────────────────────────────────────────────────
  observeEvent(input$start_test, {
    cs$started      <- TRUE
    cs$complete     <- FALSE
    cs$theta        <- 0
    cs$se           <- NA
    cs$administered <- character(0)
    cs$responses    <- numeric(0)
    cs$theta_hist   <- numeric(0)
    cs$se_hist      <- numeric(0)
    # first item: b closest to 0
    cs$current <- rownames(params_live)[which.min(abs(params_live[, "b"]))]
  })
  
  # ── submit answer ───────────────────────────────────────────────────────────
  observeEvent(input$submit_answer, {
    req(input$answer_choice, cs$current)
    
    item_id  <- cs$current
    response <- if (input$answer_choice == item_bank[[item_id]]$correct) 1 else 0
    
    cs$administered <- c(cs$administered, item_id)
    cs$responses    <- c(cs$responses,    response)
    
    # build full response pattern with NA for unadministered items
    pattern        <- rep(NA, length(all_items_live))
    names(pattern) <- all_items_live
    pattern[cs$administered] <- cs$responses
    
    # update theta via EAP
    score    <- fscores(model_2pl,
                        response.pattern = matrix(pattern, nrow = 1),
                        method           = "EAP",
                        full.scores.SE   = TRUE)
    cs$theta      <- score[1, 1]
    cs$se         <- score[1, 2]
    cs$theta_hist <- c(cs$theta_hist, cs$theta)
    cs$se_hist    <- c(cs$se_hist,    cs$se)
    
    # stopping rule: SE < 0.30 or 10 items administered
    if ((!is.na(cs$se) && cs$se < 0.30) || length(cs$administered) >= 10) {
      cs$complete <- TRUE
    } else {
      # next item: max Fisher information at current theta
      unused    <- setdiff(all_items_live, cs$administered)
      info_vals <- sapply(unused, function(item) {
        idx <- which(all_items_live == item)
        iteminfo(extract.item(model_2pl, idx),
                 Theta = matrix(cs$theta, ncol = 1))
      })
      cs$current <- unused[which.max(info_vals)]
    }
  })
  
  # ── retake test ─────────────────────────────────────────────────────────────
  observeEvent(input$retake_test, {
    cs$started  <- FALSE
    cs$complete <- FALSE
  })
  
  # ── dynamic CAT UI ──────────────────────────────────────────────────────────
  output$cat_ui <- renderUI({
    
    # ── start screen ──────────────────────────────────────────────────────────
    if (!cs$started) {
      fluidRow(
        box(title = "CBC Junior Secondary Pathway Assessment",
            status = "primary", solidHeader = TRUE, width = 8,
            p("This adaptive test selects questions based on your responses.
               It will stop automatically once your ability has been estimated
               with sufficient precision."),
            p(strong("Instructions:")),
            tags$ul(
              tags$li("Read each question carefully before answering."),
              tags$li("Select one answer and click Submit to proceed."),
              tags$li("Questions adapt to your ability level."),
              tags$li("The test stops when SE drops below 0.30 or after 10 items.")
            ),
            br(),
            actionButton("start_test", "Start Test",
                         class = "btn-primary btn-lg",
                         icon  = icon("play"))
        )
      )
      
      # ── results screen ─────────────────────────────────────────────────────────
    } else if (cs$complete) {
      
      rec <- recommend_pathway_full(
        cat_result    = data.frame(step     = seq_along(cs$administered),
                                   item     = cs$administered,
                                   response = cs$responses,
                                   theta    = cs$theta_hist,
                                   SE       = cs$se_hist),
        trait_scores  = avg_ocean,
        mapping_table = mapping_table,
        apt_weight    = 0.70,
        pers_weight   = 0.30
      )
      
      tagList(
        fluidRow(
          valueBox(rec$recommendation, "Recommended Pathway",
                   icon = icon("graduation-cap"), color = "navy",  width = 4),
          valueBox(round(tail(cs$theta_hist, 1), 3),
                   paste0("Final θ̂  (SE = ", round(tail(cs$se_hist, 1), 3), ")"),
                   icon = icon("chart-line"),     color = "blue",  width = 4),
          valueBox(length(cs$administered), "Items Administered",
                   icon = icon("list-check"),     color = "light-blue", width = 4)
        ),
        
        fluidRow(
          box(title = "Ability Estimation Trajectory", status = "primary",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_live_theta",   height = "260px")),
          box(title = "Pathway Scores",       status = "primary",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_live_pathway", height = "260px"))
        ),
        
        fluidRow(
          box(title = "Item Curves — Select a Step to View ICC and IIC",
              status = "info", solidHeader = TRUE, width = 12,
              selectInput("selected_step", NULL,
                          choices  = setNames(
                            seq_along(cs$administered),
                            paste0("Step ", seq_along(cs$administered),
                                   ":  ", cs$administered, "  (",
                                   ifelse(cs$responses == 1, "Correct", "Incorrect"), ")")
                          ),
                          selected = 1),
              fluidRow(
                column(6, plotOutput("plot_icc", height = "260px")),
                column(6, plotOutput("plot_iic", height = "260px"))
              )
          )
        ),
        
        fluidRow(
          box(width = 12, background = "black",
              actionButton("retake_test", "Retake Test",
                           class = "btn-warning",
                           icon  = icon("rotate-left")))
        )
      )
      
      # ── question screen ────────────────────────────────────────────────────────
    } else {
      
      item   <- item_bank[[cs$current]]
      n_done <- length(cs$administered)
      
      fluidRow(
        box(
          title       = paste0("Question ", n_done + 1,
                               "  |  Item: ", cs$current,
                               "  |  Domain: ", item$pathway),
          status      = "primary",
          solidHeader = TRUE,
          width       = 8,
          
          # live theta display
          tags$div(
            style = "margin-bottom: 15px; color: #888; font-size: 13px;",
            paste0("Current θ̂ = ",
                   if (n_done == 0) "0.000" else round(cs$theta, 3),
                   "  |  SE = ",
                   if (is.na(cs$se)) "—" else round(cs$se, 3),
                   "  |  Stopping when SE < 0.30")
          ),
          
          # question text
          p(strong(item$text), style = "font-size: 15px; margin-bottom: 20px;"),
          
          # answer choices — cleared after each submission
          radioButtons(
            inputId  = "answer_choice",
            label    = NULL,
            choices  = setNames(
              names(item$choices),
              paste0(names(item$choices), ")  ", item$choices)
            ),
            selected = character(0)
          ),
          
          br(),
          actionButton("submit_answer", "Submit Answer",
                       class = "btn-primary",
                       icon  = icon("arrow-right"))
        ),
        
        # live progress tracker
        box(
          title  = "Live Progress",
          status = "info",
          width  = 4,
          if (n_done > 0) {
            tagList(
              p(strong("Items answered: "), n_done),
              p(strong("Current θ̂: "),     round(cs$theta, 3)),
              p(strong("Current SE: "),     round(cs$se,    3)),
              hr(),
              tableOutput("table_live_progress")
            )
          } else {
            p("Progress will appear here after your first answer.",
              style = "color: #888;")
          }
        )
      )
    }
  })
  
  # ── live progress table ─────────────────────────────────────────────────────
  output$table_live_progress <- renderTable({
    req(length(cs$administered) > 0)
    data.frame(
      Item     = cs$administered,
      Response = ifelse(cs$responses == 1, "✓", "✗"),
      `θ̂`     = round(cs$theta_hist, 3),
      SE       = round(cs$se_hist,    3),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)
  
  # ── results: theta trajectory ───────────────────────────────────────────────
  output$plot_live_theta <- renderPlot({
    req(cs$complete)
    df <- data.frame(step  = seq_along(cs$theta_hist),
                     theta = cs$theta_hist,
                     se    = cs$se_hist)
    ggplot(df, aes(x = step)) +
      geom_ribbon(aes(ymin = theta - se, ymax = theta + se),
                  fill = "steelblue", alpha = 0.2) +
      geom_line(aes(y = theta),  colour = "steelblue", linewidth = 1) +
      geom_point(aes(y = theta), colour = "steelblue", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
      geom_label(aes(y = theta, label = round(theta, 2)),
                 vjust = -0.8, size = 3) +
      scale_x_continuous(breaks = df$step) +
      labs(x = "Step", y = "θ̂") +
      theme_minimal(base_size = 13)
  })
  
  # ── results: pathway scores ─────────────────────────────────────────────────
  output$plot_live_pathway <- renderPlot({
    req(cs$complete)
    rec <- recommend_pathway_full(
      cat_result    = data.frame(step     = seq_along(cs$administered),
                                 item     = cs$administered,
                                 response = cs$responses,
                                 theta    = cs$theta_hist,
                                 SE       = cs$se_hist),
      trait_scores  = avg_ocean,
      mapping_table = mapping_table,
      apt_weight    = 0.70, pers_weight = 0.30
    )
    df          <- data.frame(pathway = names(rec$scores),
                              score   = as.numeric(rec$scores))
    df$pathway  <- factor(df$pathway, levels = df$pathway[order(df$score)])
    df$selected <- df$pathway == rec$recommendation
    ggplot(df, aes(x = pathway, y = score, fill = selected)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(score, 3)), hjust = -0.2, size = 4) +
      scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "#2c7bb6"),
                        guide  = "none") +
      coord_flip() +
      scale_y_continuous(limits = c(0, 1.15)) +
      labs(x = NULL, y = "Score") +
      theme_minimal(base_size = 13)
  })
  
  # ── results: ICC ────────────────────────────────────────────────────────────
  output$plot_icc <- renderPlot({
    req(cs$complete, input$selected_step)
    item_id  <- cs$administered[as.integer(input$selected_step)]
    item_idx <- which(all_items_live == item_id)
    plot(model_2pl, which.items = item_idx, type = "trace",
         main = paste0("ICC — ", item_id))
  })
  
  # ── results: IIC ────────────────────────────────────────────────────────────
  output$plot_iic <- renderPlot({
    req(cs$complete, input$selected_step)
    item_id  <- cs$administered[as.integer(input$selected_step)]
    item_idx <- which(all_items_live == item_id)
    plot(model_2pl, which.items = item_idx, type = "infotrace",
         main = paste0("IIC — ", item_id))
  })
  
  # ==========================================================================
  # TAB 1 SERVER (unchanged)
  # ==========================================================================
  
  rec <- reactive({
    s     <- student_list[[input$student]]
    apt_w <- input$apt_weight
    recommend_pathway_full(cat_result    = s$cat,
                           trait_scores  = s$traits,
                           mapping_table = mapping_table,
                           apt_weight    = apt_w,
                           pers_weight   = round(1 - apt_w, 2))
  })
  
  cat_res <- reactive({ student_list[[input$student]]$cat })
  traits  <- reactive({ student_list[[input$student]]$traits })
  
  output$box_recommendation <- renderValueBox({
    valueBox(rec()$recommendation, "Recommended Pathway",
             icon = icon("compass"), color = "navy")
  })
  
  output$box_theta <- renderValueBox({
    df <- cat_res()
    valueBox(round(tail(df$theta, 1), 3),
             paste0("Final θ̂  (SE = ", round(tail(df$SE, 1), 3), ")"),
             icon = icon("chart-line"), color = "blue")
  })
  
  output$box_items <- renderValueBox({
    valueBox(nrow(cat_res()), "Items Administered",
             icon = icon("list-check"), color = "light-blue")
  })
  
  output$plot_composite <- renderPlot({
    df          <- data.frame(pathway = names(rec()$scores),
                              score   = as.numeric(rec()$scores))
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
        name   = NULL) +
      labs(x = NULL, y = "Score") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })
  
  output$plot_ocean <- renderPlot({
    t        <- traits()
    radar_df <- rbind(
      max  = rep(2,  5),
      min  = rep(-2, 5),
      data.frame(Openness          = t$openness,
                 Conscientiousness = t$conscientiousness,
                 Extraversion      = t$extraversion,
                 Agreeableness     = t$agreeableness,
                 Neuroticism       = t$neuroticism)
    )
    radarchart(radar_df, axistype = 1,
               pcol = "#2c7bb6", pfcol = adjustcolor("#2c7bb6", alpha.f = 0.3),
               plwd = 2, cglcol = "grey80", cglty = 1,
               axislabcol = "grey50", vlcex = 0.9,
               title = paste("OCEAN Profile —", input$student))
  })
  
  output$table_cat <- renderTable({
    df        <- cat_res()
    df$theta  <- round(df$theta, 3)
    df$SE     <- round(df$SE,    3)
    names(df) <- c("Step", "Item", "Response", "θ̂", "SE")
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# =============================================================================
shinyApp(ui = ui, server = server)