# =============================================================================
# app.R — CBC CAT Pathway Advisor
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(mirt)
library(fmsb)

source("pathway_utils.R")
load("cat_objects.RData")

# =============================================================================
# HELPER FUNCTIONS — ICC and IIC curve data for ggplot
# =============================================================================

# Item Characteristic Curve: P(θ) = 1 / (1 + exp(−a(θ − b)))
make_icc_df <- function(a, b, theta_seq = seq(-4, 4, by = 0.05)) {
  data.frame(theta = theta_seq,
             p     = 1 / (1 + exp(-a * (theta_seq - b))))
}

# Item Information Curve: I(θ) = a² × P(θ) × (1 − P(θ))
make_iic_df <- function(a, b, theta_seq = seq(-4, 4, by = 0.05)) {
  p <- 1 / (1 + exp(-a * (theta_seq - b)))
  data.frame(theta = theta_seq,
             info  = a^2 * p * (1 - p))
}

# =============================================================================
# ITEM BANK
# =============================================================================
item_bank <- list(
  A1  = list(text    = "A maize farmer in Nakuru harvests 240 kg of maize from 3 equal plots. How many kilograms does each plot produce?",
             choices = c(A="60 kg", B="80 kg", C="90 kg", D="120 kg"),
             correct = "B", a=1.0, b=-1.5, pathway="STEM"),
  A2  = list(text    = "Look at the number pattern: 2, 4, 8, 16, ___. What is the next number?",
             choices = c(A="18", B="24", C="32", D="64"),
             correct = "C", a=1.1, b=-1.2, pathway="STEM"),
  A3  = list(text    = "A water tank holds 500 litres when full. After a family uses 35% of the water, how many litres remain?",
             choices = c(A="165 litres", B="175 litres", C="325 litres", D="350 litres"),
             correct = "C", a=1.2, b=-0.8, pathway="STEM"),
  A4  = list(text    = "A plant is placed near a window. After one week its stem bent toward the light. This is an example of:",
             choices = c(A="Photosynthesis", B="Tropism", C="Respiration", D="Osmosis"),
             correct = "B", a=1.3, b=-0.5, pathway="STEM"),
  A5  = list(text    = "Which completes the sequence? △ ○ △ △ ○ △ △ △ ○ ___",
             choices = c(A="△", B="○", C="△ △ △ △", D="○ △"),
             correct = "C", a=1.4, b=-0.2, pathway="STEM"),
  A6  = list(text    = "A car travels 120 km in 2 hours. At the same speed, how long to travel 300 km?",
             choices = c(A="4 hours", B="4.5 hours", C="5 hours", D="6 hours"),
             correct = "C", a=1.5, b=0.0, pathway="STEM"),
  A7  = list(text    = "If all Zs are Ys, and all Ys are Xs, which must be true?",
             choices = c(A="All Xs are Zs", B="All Xs are Ys", C="All Zs are Xs", D="No Xs are Zs"),
             correct = "C", a=1.5, b=0.3, pathway="STEM"),
  A8  = list(text    = "A scientist dissolves salt in water and heats until the water evaporates. What remains?",
             choices = c(A="Nothing — both disappear", B="Only water vapour", C="The salt", D="A new chemical compound"),
             correct = "C", a=1.6, b=0.6, pathway="STEM"),
  A9  = list(text    = "A rectangle has perimeter 36 cm. If length is twice width, what is the area?",
             choices = c(A="72 cm²", B="96 cm²", C="108 cm²", D="144 cm²"),
             correct = "A", a=1.7, b=0.9, pathway="STEM"),
  A10 = list(text    = "Gear A (8 teeth) → Gear B (16 teeth) → Gear C (8 teeth). If A rotates 4 times, how many times does C rotate?",
             choices = c(A="2 times", B="4 times", C="8 times", D="16 times"),
             correct = "B", a=1.8, b=1.2, pathway="STEM"),
  A11 = list(text    = "A code replaces each letter with the one 3 positions ahead (A→D, B→E...). What does 'PDWKV' decode to?",
             choices = c(A="LEARN", B="MATHS", C="READS", D="LOGIC"),
             correct = "B", a=1.8, b=1.5, pathway="STEM"),
  A12 = list(text    = "Water temperature every 2 min: 20°C, 35°C, 50°C, 65°C. When will it reach 95°C?",
             choices = c(A="At 8 minutes", B="At 10 minutes", C="At 12 minutes", D="At 14 minutes"),
             correct = "B", a=1.9, b=1.8, pathway="STEM"),
  
  B1  = list(text    = "A village elder calls a baraza to discuss a new road. What is the main purpose?",
             choices = c(A="To collect taxes", B="To allow community members to share their views",
                         C="To elect a new village elder", D="To announce government decisions"),
             correct = "B", a=1.0, b=-1.5, pathway="Social Sciences"),
  B2  = list(text    = "When population grows faster than food production, what is the most likely result?",
             choices = c(A="The country becomes wealthier", B="Food prices decrease",
                         C="Food shortages may occur", D="People eat more nutritious food"),
             correct = "C", a=1.1, b=-1.0, pathway="Social Sciences"),
  B3  = list(text    = "A new hospital is built in a remote Turkana village. What is the most likely long-term effect?",
             choices = c(A="More people will move away", B="School attendance will decrease",
                         C="Child mortality rates are likely to reduce", D="Farming activity will increase"),
             correct = "C", a=1.3, b=-0.5, pathway="Social Sciences"),
  B4  = list(text    = "After independence, many African countries kept colonial borders. What is the most likely reason?",
             choices = c(A="The borders were scientifically accurate",
                         B="Changing borders would have caused conflict between new nations",
                         C="African communities preferred to be divided",
                         D="Colonial powers refused to let borders change"),
             correct = "B", a=1.4, b=-0.2, pathway="Social Sciences"),
  B5  = list(text    = "A county government builds a market near a school. What is the most likely reason?",
             choices = c(A="To distract students from studying", B="To make it easier for teachers to shop",
                         C="To boost local economic activity and serve the nearby population",
                         D="To reduce the number of students in the school"),
             correct = "C", a=1.5, b=0.0, pathway="Social Sciences"),
  B6  = list(text    = "In a democracy, why is freedom of the press important?",
             choices = c(A="It allows journalists to earn more money",
                         B="It enables citizens to receive information and hold leaders accountable",
                         C="It gives the government control over public opinion",
                         D="It prevents foreign interference"),
             correct = "B", a=1.5, b=0.3, pathway="Social Sciences"),
  B7  = list(text    = "Which best explains why urbanization accelerated after Kenya's independence in 1963?",
             choices = c(A="Rural areas became more dangerous", B="The government forced people to move",
                         C="Cities offered more employment and educational opportunities",
                         D="Farming became illegal after independence"),
             correct = "C", a=1.6, b=0.7, pathway="Social Sciences"),
  B8  = list(text    = "A country imposes high tax on imported goods. What is the most likely intended effect?",
             choices = c(A="To encourage citizens to buy more foreign goods",
                         B="To protect local industries from foreign competition",
                         C="To reduce the country's tax revenue", D="To strengthen trade relationships"),
             correct = "B", a=1.7, b=1.0, pathway="Social Sciences"),
  B9  = list(text    = "Community A (communal land) had lower inequality than Community B (individual ownership). Which conclusion is best supported?",
             choices = c(A="Individual land ownership always leads to poverty",
                         B="Communal land ownership may reduce wealth inequality",
                         C="Community B residents work harder",
                         D="Land ownership has no effect on inequality"),
             correct = "B", a=1.8, b=1.3, pathway="Social Sciences"),
  B10 = list(text    = "A government requires 50+ employee companies to hire 30% staff under 30. What is a likely unintended consequence?",
             choices = c(A="Youth unemployment immediately disappears",
                         B="Companies may choose to stay small to avoid the requirement",
                         C="Older workers will receive higher salaries",
                         D="Foreign companies will invest more"),
             correct = "B", a=1.9, b=1.7, pathway="Social Sciences"),
  
  C1  = list(text    = "Dancer sequence: step right, clap, step left, clap, jump. What is the 8th movement?",
             choices = c(A="Step right", B="Clap", C="Step left", D="Jump"),
             correct = "C", a=1.0, b=-1.5, pathway="Arts & Sports"),
  C2  = list(text    = "A painter mixes red and blue equally to get purple. To make a darker purple, add more of:",
             choices = c(A="Red", B="Blue", C="White", D="Yellow"),
             correct = "B", a=1.1, b=-1.0, pathway="Arts & Sports"),
  C3  = list(text    = "Shapes: ■ ■ □ ■ ■ □ ■ ■ □ ___. What comes next?",
             choices = c(A="■", B="□", C="■ ■", D="□ □"),
             correct = "B", a=1.3, b=-0.5, pathway="Arts & Sports"),
  C4  = list(text    = "Sprinter times: 14.2s, 13.8s, 13.4s, 13.0s. Expected time in week 5?",
             choices = c(A="12.4s", B="12.6s", C="12.8s", D="13.2s"),
             correct = "B", a=1.5, b=0.0, pathway="Arts & Sports"),
  C5  = list(text    = "A sculptor cuts straight through the middle of a cube. How many faces does each piece have?",
             choices = c(A="4", B="5", C="6", D="7"),
             correct = "B", a=1.5, b=0.3, pathway="Arts & Sports"),
  C6  = list(text    = "A coach notices her team scores more goals in the second half. Which explanation does sports science best support?",
             choices = c(A="The opposing team always gets tired first",
                         B="The team benefits from tactical adjustments made at half-time",
                         C="Football rules give advantages in the second half",
                         D="The weather always improves in the second half"),
             correct = "B", a=1.6, b=0.8, pathway="Arts & Sports"),
  C7  = list(text    = "An equilateral triangle is inscribed in a circle. What is true of the two centers?",
             choices = c(A="They are at different points", B="The triangle's center is outside the circle",
                         C="They are the same point", D="The circle's center is on the triangle's edge"),
             correct = "C", a=1.7, b=1.2, pathway="Arts & Sports"),
  C8  = list(text    = "4 dancers start at corners of a square. Every 8 beats each moves clockwise to the next corner. After 24 beats, where is the top-left dancer?",
             choices = c(A="Top-left (back to start)", B="Top-right", C="Bottom-right", D="Bottom-left"),
             correct = "D", a=1.8, b=1.6, pathway="Arts & Sports")
)

# =============================================================================
# PRE-COMPUTATION
# =============================================================================

# Item parameter table for IRT hybrid scoring
item_params_df <- do.call(rbind, lapply(names(item_bank), function(id) {
  data.frame(item    = id,
             a       = item_bank[[id]]$a,
             b       = item_bank[[id]]$b,
             pathway = item_bank[[id]]$pathway,
             stringsAsFactors = FALSE)
}))

# Item IDs in model order
params_live    <- coef(model_2pl, IRTpars = TRUE, simplify = TRUE)$items
all_items_live <- rownames(params_live)

# Pre-computed demo student profiles (Tab 1)
student_list <- list(
  "Low Ability"  = list(cat = verb_low$result,  traits = trait_df[low_id,  ]),
  "Mid Ability"  = list(cat = verb_mid$result,  traits = trait_df[mid_id,  ]),
  "High Ability" = list(cat = verb_high$result, traits = trait_df[high_id, ])
)

# Final theta for each demo student — used to place dots on ICC/IIC plots
demo_theta_vals <- sapply(student_list, function(s) tail(s$cat$theta, 1))

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
    conditionalPanel(
      condition = "input.tabs === 'demo'",
      hr(),
      selectInput("student", "Select Student Profile",
                  choices = names(student_list), selected = "Mid Ability"),
      hr(),
      selectInput("demo_item_select", "Select Item for ICC / IIC",
                  choices  = setNames(names(item_bank),
                                      paste0(names(item_bank), " — ",
                                             sapply(item_bank, `[[`, "pathway"))),
                  selected = "A6")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── TAB 1: DEMO RESULTS ─────────────────────────────────────────────────
      tabItem(tabName = "demo",
              
              # Row 1: headline metrics
              fluidRow(
                valueBoxOutput("box_recommendation", width = 4),
                valueBoxOutput("box_theta",          width = 4),
                valueBoxOutput("box_items",          width = 4)
              ),
              
              # Row 2: aptitude scores + ICC
              fluidRow(
                box(title = "Aptitude Pathway Scores", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotOutput("plot_composite", height = "300px")),
                box(title = "Item Characteristic Curve (ICC)", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotOutput("plot_demo_icc", height = "300px"))
              ),
              
              # Row 3: IIC + personality OCEAN
              fluidRow(
                box(title = "Item Information Curve (IIC)", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotOutput("plot_demo_iic", height = "300px")),
                box(title = "Personality Context — OCEAN Profile", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotOutput("plot_ocean", height = "300px"))
              ),
              
              # Row 4: personality narrative
              fluidRow(
                box(title = "Personality Narrative", status = "info",
                    solidHeader = TRUE, width = 12,
                    uiOutput("personality_narrative"))
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
  
  # ── Start test ──────────────────────────────────────────────────────────────
  observeEvent(input$start_test, {
    cs$started      <- TRUE
    cs$complete     <- FALSE
    cs$theta        <- 0
    cs$se           <- NA
    cs$administered <- character(0)
    cs$responses    <- numeric(0)
    cs$theta_hist   <- numeric(0)
    cs$se_hist      <- numeric(0)
    cs$current      <- rownames(params_live)[which.min(abs(params_live[, "b"]))]
  })
  
  # ── Submit answer ───────────────────────────────────────────────────────────
  observeEvent(input$submit_answer, {
    req(input$answer_choice, cs$current)
    
    item_id  <- cs$current
    response <- if (input$answer_choice == item_bank[[item_id]]$correct) 1 else 0
    
    cs$administered <- c(cs$administered, item_id)
    cs$responses    <- c(cs$responses,    response)
    
    pattern        <- rep(NA, length(all_items_live))
    names(pattern) <- all_items_live
    pattern[cs$administered] <- cs$responses
    
    score         <- fscores(model_2pl,
                             response.pattern = matrix(pattern, nrow = 1),
                             method           = "EAP",
                             full.scores.SE   = TRUE)
    cs$theta      <- score[1, 1]
    cs$se         <- score[1, 2]
    cs$theta_hist <- c(cs$theta_hist, cs$theta)
    cs$se_hist    <- c(cs$se_hist,    cs$se)
    
    # Stopping rule: SE < 0.30 or 10 items administered
    if ((!is.na(cs$se) && cs$se < 0.30) || length(cs$administered) >= 10) {
      cs$complete <- TRUE
    } else {
      unused    <- setdiff(all_items_live, cs$administered)
      info_vals <- sapply(unused, function(item) {
        idx <- which(all_items_live == item)
        iteminfo(extract.item(model_2pl, idx),
                 Theta = matrix(cs$theta, ncol = 1))
      })
      cs$current <- unused[which.max(info_vals)]
    }
  })
  
  # ── Retake test ─────────────────────────────────────────────────────────────
  observeEvent(input$retake_test, {
    cs$started  <- FALSE
    cs$complete <- FALSE
  })
  
  # ── Dynamic CAT UI ──────────────────────────────────────────────────────────
  output$cat_ui <- renderUI({
    
    # Start screen
    if (!cs$started) {
      fluidRow(
        box(title = "CBC Junior Secondary Pathway Assessment",
            status = "primary", solidHeader = TRUE, width = 8,
            p("This adaptive test selects questions based on your responses
               and stops once your ability has been estimated with sufficient precision."),
            p(strong("Instructions:")),
            tags$ul(
              tags$li("Read each question carefully."),
              tags$li("Select one answer and click Submit."),
              tags$li("Questions adapt to your ability level in real time."),
              tags$li("The test stops when SE < 0.30 or after 10 items.")
            ),
            br(),
            actionButton("start_test", "Start Test",
                         class = "btn-primary btn-lg", icon = icon("play")))
      )
      
      # Results screen
    } else if (cs$complete) {
      
      cat_df <- data.frame(step     = seq_along(cs$administered),
                           item     = cs$administered,
                           response = cs$responses,
                           theta    = cs$theta_hist,
                           SE       = cs$se_hist)
      
      rec <- recommend_pathway_full(cat_result    = cat_df,
                                    mapping_table = mapping_table,
                                    item_params   = item_params_df)
      
      tagList(
        fluidRow(
          valueBox(rec$recommendation, "Recommended Pathway",
                   icon = icon("graduation-cap"), color = "navy",       width = 4),
          valueBox(round(tail(cs$theta_hist, 1), 3),
                   paste0("Final θ̂  (SE = ", round(tail(cs$se_hist, 1), 3), ")"),
                   icon = icon("chart-line"),     color = "blue",       width = 4),
          valueBox(length(cs$administered), "Items Administered",
                   icon = icon("list-check"),     color = "light-blue", width = 4)
        ),
        
        fluidRow(
          box(title = "Ability Estimation Trajectory", status = "primary",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_live_theta",   height = "260px")),
          box(title = "Aptitude Pathway Scores", status = "primary",
              solidHeader = TRUE, width = 6,
              plotOutput("plot_live_pathway", height = "260px"))
        ),
        
        # ICC and IIC for each administered item
        fluidRow(
          box(title = "Item Curves — Select a Step",
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
              ))
        ),
        
        fluidRow(
          box(title = "Personality Context", status = "info",
              solidHeader = TRUE, width = 12,
              p("A full personality profile requires a separate OCEAN questionnaire,
                 not administered as part of this CAT session."),
              p(em("Future direction: integrate a short BFI-10 pre-assessment so
                    personality is reported as individual context alongside aptitude."),
                style = "color: #888;"))
        ),
        
        fluidRow(
          box(width = 12, background = "black",
              actionButton("retake_test", "Retake Test",
                           class = "btn-warning", icon = icon("rotate-left")))
        )
      )
      
      # Question screen
    } else {
      
      item   <- item_bank[[cs$current]]
      n_done <- length(cs$administered)
      
      tagList(
        # Row 1: question + live progress
        fluidRow(
          box(
            title       = paste0("Question ", n_done + 1,
                                 "  |  Item: ", cs$current,
                                 "  |  Domain: ", item$pathway),
            status      = "primary", solidHeader = TRUE, width = 8,
            tags$div(
              style = "margin-bottom: 15px; color: #888; font-size: 13px;",
              paste0("Current θ̂ = ", round(cs$theta, 3),
                     "  |  SE = ",
                     if (is.na(cs$se)) "—" else round(cs$se, 3),
                     "  |  Stopping when SE < 0.30 or 10 items")
            ),
            p(strong(item$text), style = "font-size: 15px; margin-bottom: 20px;"),
            radioButtons(inputId  = "answer_choice",
                         label    = NULL,
                         choices  = setNames(names(item$choices),
                                             paste0(names(item$choices), ")  ", item$choices)),
                         selected = character(0)),
            br(),
            actionButton("submit_answer", "Submit Answer",
                         class = "btn-primary", icon = icon("arrow-right"))
          ),
          box(title = "Live Progress", status = "info", width = 4,
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
        ),
        
        # Row 2: dynamic ICC and IIC for the current item — updates after each answer
        fluidRow(
          box(title = paste0("ICC — Current Item: ", cs$current),
              status = "primary", solidHeader = TRUE, width = 6,
              plotOutput("plot_live_icc_dynamic", height = "240px")),
          box(title = paste0("IIC — Current Item: ", cs$current),
              status = "primary", solidHeader = TRUE, width = 6,
              plotOutput("plot_live_iic_dynamic", height = "240px"))
        )
      )
    }
  })
  
  # ── Live: dynamic ICC during test ───────────────────────────────────────────
  # Shows the ICC for the current item with:
  #   - sample theta distribution (rug) from all 500 simulated students
  #   - red dot tracking the test-taker's current theta estimate
  output$plot_live_icc_dynamic <- renderPlot({
    req(cs$started, !cs$complete, cs$current)
    
    item      <- item_bank[[cs$current]]
    curve_df  <- make_icc_df(item$a, item$b)
    current_p <- 1 / (1 + exp(-item$a * (cs$theta - item$b)))
    
    ggplot() +
      # Sample distribution rug along bottom
      geom_rug(data    = data.frame(theta = theta_df$theta),
               aes(x   = theta),
               colour  = "grey60", alpha = 0.3, sides = "b") +
      # ICC curve
      geom_line(data      = curve_df, aes(x = theta, y = p),
                colour    = "steelblue", linewidth = 1.2) +
      # Vertical line at current theta
      geom_vline(xintercept = cs$theta,
                 linetype   = "dashed", colour = "#e74c3c", alpha = 0.7) +
      # Red dot at (current theta, P(correct | current theta))
      geom_point(data   = data.frame(theta = cs$theta, p = current_p),
                 aes(x  = theta, y = p),
                 colour = "#e74c3c", size = 5) +
      geom_label(data   = data.frame(theta = cs$theta, p = current_p),
                 aes(x  = theta, y = p,
                     label = paste0("θ̂ = ", round(cs$theta, 2))),
                 vjust  = -0.8, colour = "#e74c3c", size = 3.5) +
      scale_y_continuous(limits = c(0, 1.15)) +
      labs(x = "θ (Ability)", y = "P(Correct)",
           caption = paste0("a = ", item$a, "  |  b = ", item$b,
                            "  |  Pathway: ", item$pathway)) +
      theme_minimal(base_size = 12)
  })
  
  # ── Live: dynamic IIC during test ───────────────────────────────────────────
  output$plot_live_iic_dynamic <- renderPlot({
    req(cs$started, !cs$complete, cs$current)
    
    item        <- item_bank[[cs$current]]
    curve_df    <- make_iic_df(item$a, item$b)
    p_current   <- 1 / (1 + exp(-item$a * (cs$theta - item$b)))
    info_current <- item$a^2 * p_current * (1 - p_current)
    
    ggplot() +
      geom_rug(data    = data.frame(theta = theta_df$theta),
               aes(x   = theta),
               colour  = "grey60", alpha = 0.3, sides = "b") +
      geom_line(data     = curve_df, aes(x = theta, y = info),
                colour   = "#27ae60", linewidth = 1.2) +
      geom_vline(xintercept = cs$theta,
                 linetype   = "dashed", colour = "#e74c3c", alpha = 0.7) +
      geom_point(data   = data.frame(theta = cs$theta, info = info_current),
                 aes(x  = theta, y = info),
                 colour = "#e74c3c", size = 5) +
      geom_label(data   = data.frame(theta = cs$theta, info = info_current),
                 aes(x  = theta, y = info,
                     label = paste0("θ̂ = ", round(cs$theta, 2))),
                 vjust  = -0.8, colour = "#e74c3c", size = 3.5) +
      labs(x = "θ (Ability)", y = "Information",
           caption = paste0("Peak information at b = ", item$b)) +
      theme_minimal(base_size = 12)
  })
  
  # ── Live progress table ─────────────────────────────────────────────────────
  output$table_live_progress <- renderTable({
    req(length(cs$administered) > 0)
    data.frame(Item     = cs$administered,
               Response = ifelse(cs$responses == 1, "✓", "✗"),
               `θ̂`     = round(cs$theta_hist, 3),
               SE       = round(cs$se_hist,    3),
               check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)
  
  # ── Live: theta trajectory (results screen) ─────────────────────────────────
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
      geom_label(aes(y = theta, label = round(theta, 2)), vjust = -0.8, size = 3) +
      scale_x_continuous(breaks = df$step) +
      labs(x = "Step", y = "θ̂") +
      theme_minimal(base_size = 13)
  })
  
  # ── Live: pathway scores (results screen) ───────────────────────────────────
  output$plot_live_pathway <- renderPlot({
    req(cs$complete)
    cat_df <- data.frame(step     = seq_along(cs$administered),
                         item     = cs$administered,
                         response = cs$responses,
                         theta    = cs$theta_hist,
                         SE       = cs$se_hist)
    rec         <- recommend_pathway_full(cat_result    = cat_df,
                                          mapping_table = mapping_table,
                                          item_params   = item_params_df)
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
      labs(x = NULL, y = "Aptitude Score") +
      theme_minimal(base_size = 13)
  })
  
  # ── Live: ICC and IIC for selected administered item (results screen) ────────
  output$plot_icc <- renderPlot({
    req(cs$complete, input$selected_step)
    step     <- as.integer(input$selected_step)
    item_id  <- cs$administered[step]
    item     <- item_bank[[item_id]]
    theta_at_step <- cs$theta_hist[step]
    curve_df <- make_icc_df(item$a, item$b)
    p_at_step <- 1 / (1 + exp(-item$a * (theta_at_step - item$b)))
    
    ggplot() +
      geom_rug(data = data.frame(theta = theta_df$theta),
               aes(x = theta), colour = "grey60", alpha = 0.3, sides = "b") +
      geom_line(data = curve_df, aes(x = theta, y = p),
                colour = "steelblue", linewidth = 1.2) +
      geom_vline(xintercept = theta_at_step,
                 linetype = "dashed", colour = "#e74c3c", alpha = 0.7) +
      geom_point(data = data.frame(theta = theta_at_step, p = p_at_step),
                 aes(x = theta, y = p), colour = "#e74c3c", size = 5) +
      geom_label(data = data.frame(theta = theta_at_step, p = p_at_step),
                 aes(x = theta, y = p,
                     label = paste0("θ̂ = ", round(theta_at_step, 2))),
                 vjust = -0.8, colour = "#e74c3c", size = 3.5) +
      scale_y_continuous(limits = c(0, 1.15)) +
      labs(title = paste0("ICC — ", item_id, "  (",
                          ifelse(cs$responses[step] == 1, "Correct", "Incorrect"), ")"),
           x = "θ", y = "P(Correct)",
           caption = paste0("a = ", item$a, "  |  b = ", item$b)) +
      theme_minimal(base_size = 12)
  })
  
  output$plot_iic <- renderPlot({
    req(cs$complete, input$selected_step)
    step         <- as.integer(input$selected_step)
    item_id      <- cs$administered[step]
    item         <- item_bank[[item_id]]
    theta_at_step <- cs$theta_hist[step]
    curve_df     <- make_iic_df(item$a, item$b)
    p_at_step    <- 1 / (1 + exp(-item$a * (theta_at_step - item$b)))
    info_at_step <- item$a^2 * p_at_step * (1 - p_at_step)
    
    ggplot() +
      geom_rug(data = data.frame(theta = theta_df$theta),
               aes(x = theta), colour = "grey60", alpha = 0.3, sides = "b") +
      geom_line(data = curve_df, aes(x = theta, y = info),
                colour = "#27ae60", linewidth = 1.2) +
      geom_vline(xintercept = theta_at_step,
                 linetype = "dashed", colour = "#e74c3c", alpha = 0.7) +
      geom_point(data = data.frame(theta = theta_at_step, info = info_at_step),
                 aes(x = theta, y = info), colour = "#e74c3c", size = 5) +
      geom_label(data = data.frame(theta = theta_at_step, info = info_at_step),
                 aes(x = theta, y = info,
                     label = paste0("θ̂ = ", round(theta_at_step, 2))),
                 vjust = -0.8, colour = "#e74c3c", size = 3.5) +
      labs(title = paste0("IIC — ", item_id, "  (",
                          ifelse(cs$responses[step] == 1, "Correct", "Incorrect"), ")"),
           x = "θ", y = "Information",
           caption = paste0("Peak at b = ", item$b)) +
      theme_minimal(base_size = 12)
  })
  
  # ==========================================================================
  # TAB 1 SERVER
  # ==========================================================================
  
  rec <- reactive({
    s <- student_list[[input$student]]
    recommend_pathway_full(cat_result    = s$cat,
                           mapping_table = mapping_table,
                           item_params   = item_params_df)
  })
  
  ocean_profile <- reactive({
    describe_ocean_profile(student_list[[input$student]]$traits)
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
      labs(x = NULL, y = "Aptitude Score") +
      theme_minimal(base_size = 13)
  })
  
  # ── Demo ICC: selected item, all three demo students as dots ─────────────────
  # Selected student shown in red; others in grey.
  # Rug plot shows full distribution of 500 simulated student thetas.
  output$plot_demo_icc <- renderPlot({
    req(input$demo_item_select)
    id   <- input$demo_item_select
    item <- item_bank[[id]]
    
    curve_df  <- make_icc_df(item$a, item$b)
    
    # Three demo student positions on the curve
    demo_df <- data.frame(
      student = names(demo_theta_vals),
      theta   = as.numeric(demo_theta_vals)
    )
    demo_df$p        <- 1 / (1 + exp(-item$a * (demo_df$theta - item$b)))
    demo_df$selected <- demo_df$student == input$student
    
    ggplot() +
      # Full sample distribution as rug along x-axis
      geom_rug(data   = data.frame(theta = theta_df$theta),
               aes(x  = theta),
               colour = "grey60", alpha = 0.3, sides = "b") +
      # ICC curve
      geom_line(data     = curve_df, aes(x = theta, y = p),
                colour   = "steelblue", linewidth = 1.2) +
      # Vertical dashed line for selected student
      geom_vline(xintercept = demo_df$theta[demo_df$selected],
                 linetype   = "dashed", colour = "#e74c3c", alpha = 0.6) +
      # All three student dots
      geom_point(data  = demo_df,
                 aes(x = theta, y = p, colour = selected, size = selected)) +
      geom_label(data  = demo_df,
                 aes(x = theta, y = p, label = student),
                 vjust = -0.9, size = 3) +
      scale_colour_manual(values = c("FALSE" = "grey50", "TRUE" = "#e74c3c"),
                          guide  = "none") +
      scale_size_manual(values   = c("FALSE" = 3,        "TRUE" = 5),
                        guide    = "none") +
      scale_y_continuous(limits  = c(0, 1.15)) +
      labs(x       = "θ (Ability)",
           y       = "P(Correct)",
           caption = paste0("a = ", item$a, "  |  b = ", item$b,
                            "  |  Pathway: ", item$pathway)) +
      theme_minimal(base_size = 12)
  })
  
  # ── Demo IIC: same structure as ICC but for item information ─────────────────
  output$plot_demo_iic <- renderPlot({
    req(input$demo_item_select)
    id   <- input$demo_item_select
    item <- item_bank[[id]]
    
    curve_df <- make_iic_df(item$a, item$b)
    
    demo_df <- data.frame(
      student = names(demo_theta_vals),
      theta   = as.numeric(demo_theta_vals)
    )
    p_demo           <- 1 / (1 + exp(-item$a * (demo_df$theta - item$b)))
    demo_df$info     <- item$a^2 * p_demo * (1 - p_demo)
    demo_df$selected <- demo_df$student == input$student
    
    ggplot() +
      geom_rug(data   = data.frame(theta = theta_df$theta),
               aes(x  = theta),
               colour = "grey60", alpha = 0.3, sides = "b") +
      geom_line(data     = curve_df, aes(x = theta, y = info),
                colour   = "#27ae60", linewidth = 1.2) +
      geom_vline(xintercept = demo_df$theta[demo_df$selected],
                 linetype   = "dashed", colour = "#e74c3c", alpha = 0.6) +
      geom_point(data  = demo_df,
                 aes(x = theta, y = info, colour = selected, size = selected)) +
      geom_label(data  = demo_df,
                 aes(x = theta, y = info, label = student),
                 vjust = -0.9, size = 3) +
      scale_colour_manual(values = c("FALSE" = "grey50", "TRUE" = "#e74c3c"),
                          guide  = "none") +
      scale_size_manual(values   = c("FALSE" = 3,        "TRUE" = 5),
                        guide    = "none") +
      labs(x       = "θ (Ability)",
           y       = "Information",
           caption = paste0("Peak information at b = ", item$b,
                            "  |  Pathway: ", item$pathway)) +
      theme_minimal(base_size = 12)
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
               pcol       = "#2c7bb6",
               pfcol      = adjustcolor("#2c7bb6", alpha.f = 0.3),
               plwd       = 2,
               cglcol     = "grey80", cglty = 1,
               axislabcol = "grey50", vlcex = 0.9,
               title      = paste("OCEAN Profile —", input$student))
  })
  
  output$personality_narrative <- renderUI({
    profile <- ocean_profile()
    tagList(
      tags$p(strong("Trait Summary:")),
      tags$p(paste(paste0(names(profile$labels), ": ", profile$labels),
                   collapse = "  |  "),
             style = "font-size: 13px;"),
      hr(),
      tags$p(strong("Learning Style:")),
      tags$ul(lapply(profile$narrative, tags$li)),
      hr(),
      tags$p(strong("Personality alignment: "),
             tags$span(profile$personality_lean,
                       style = "color: #2c7bb6; font-weight: bold;")),
      tags$p(em("Note: personality alignment is descriptive context only and does
                 not influence the aptitude-based pathway recommendation above."),
             style = "color: #888; font-size: 12px;")
    )
  })
}

# =============================================================================
shinyApp(ui = ui, server = server)