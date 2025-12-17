# Eating Disorder Risk Calculator - Shiny App (Shinylive Compatible)
# Based on: Katsiferis et al. (2025) NPJ Mental Health Research
# https://doi.org/10.1038/s44184-025-00179-x

library(shiny)
library(bslib)

# ============================================================================
# MODEL COEFFICIENTS (Prognostic model - predicting EDs by DNBC-18)
# ============================================================================

coefficients <- list(
  intercept = -4.2127,
  sex_male = -2.2274,
  emotional_symptoms = 0.0466,
  body_satisfaction = -0.1257,
  peer_problems = 0.0372,
  hyperactivity = -0.0411,
  child_bmi_7y = 0.0809,
  maternal_bmi = -0.0150,
  conduct_problems = 0.1060,
  paternal_bmi = -0.0080,
  stress = 0.2428
)

# Average population risk
avg_risk <- 0.03562133

# ============================================================================
# UI
# ============================================================================

ui <- page_navbar(
  title = span(style = "font-weight: 600;", "Eating Disorder Risk Calculator"),
  id = "nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    secondary = "#95a5a6",
    success = "#18bc9c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),

  # Calculator Tab
  nav_panel(
    title = "Calculator",

    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        bg = "#f8f9fa",

        # Header
        div(
          style = "text-align: center; padding: 10px 0 20px 0; border-bottom: 1px solid #dee2e6; margin-bottom: 20px;",
          h5(style = "margin: 0; color: #2c3e50; font-weight: 600;", "Enter Child's Information"),
          p(style = "margin: 5px 0 0 0; font-size: 13px; color: #6c757d;", "All measures collected around age 11")
        ),

        # Sex
        radioButtons(
          inputId = "sex",
          label = strong("Sex"),
          choices = c("Female" = "female", "Male" = "male"),
          selected = "female",
          inline = TRUE
        ),

        hr(style = "margin: 15px 0;"),

        # SDQ Measures Section
        h6(style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;", "Behavioral & Emotional Measures"),

        # Emotional symptoms
        sliderInput(
          "emotional",
          label = "Emotional Symptoms (child-reported, SDQ)",
          min = 0, max = 10, value = 2, step = 1
        ),
        p(style = "font-size: 11px; color: #6c757d; margin-top: -10px;",
          "Measures worry, unhappiness, nervousness, fears"),

        # Conduct problems
        sliderInput(
          "conduct",
          label = "Conduct Problems (child-reported, SDQ)",
          min = 0, max = 9, value = 2, step = 1
        ),
        p(style = "font-size: 11px; color: #6c757d; margin-top: -10px;",
          "Measures temper, obedience, fighting behaviors"),

        # Peer problems
        sliderInput(
          "peer_problems",
          label = "Peer Relationship Problems (parent-reported, SDQ)",
          min = 0, max = 10, value = 1, step = 1
        ),
        p(style = "font-size: 11px; color: #6c757d; margin-top: -10px;",
          "Difficulties in social relationships"),

        # Hyperactivity
        sliderInput(
          "hyperactivity",
          label = "Hyperactivity/Inattention (parent-reported, SDQ)",
          min = 0, max = 10, value = 3, step = 1
        ),
        p(style = "font-size: 11px; color: #6c757d; margin-top: -10px;",
          "Restlessness, distractibility, impulsivity"),

        hr(style = "margin: 15px 0;"),

        # Psychological Measures Section
        h6(style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;", "Psychological Measures"),

        # Body satisfaction
        sliderInput(
          "body_satisfaction",
          label = "Body Satisfaction Score",
          min = -4, max = 6, value = 2, step = 1
        ),
        div(
          style = "display: flex; justify-content: space-between; font-size: 11px; color: #6c757d; margin-top: -10px; margin-bottom: 10px;",
          span("More dissatisfied"), span("More satisfied")
        ),

        # Stress
        sliderInput(
          "stress",
          label = "Stress Level (SiC questionnaire)",
          min = 1, max = 4, value = 2, step = 0.1
        ),
        p(style = "font-size: 11px; color: #6c757d; margin-top: -10px;",
          "Physical, psychological, behavioral stress responses"),

        hr(style = "margin: 15px 0;"),

        # Physical Measures Section
        h6(style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;", "Physical Measures"),

        # Child BMI
        sliderInput(
          "child_bmi",
          label = "Child's BMI (at age 7)",
          min = 9.5, max = 37, value = 15.5, step = 0.5
        ),

        # Maternal BMI
        sliderInput(
          "maternal_bmi",
          label = "Maternal BMI (current)",
          min = 14, max = 40, value = 24, step = 0.5
        ),

        # Paternal BMI
        sliderInput(
          "paternal_bmi",
          label = "Paternal BMI (current)",
          min = 15.5, max = 41, value = 26, step = 0.5
        )
      ),

      # Main panel - Results
      div(
        style = "max-width: 800px; margin: 0 auto; padding: 20px;",

        # Results Card
        card(
          card_header(
            class = "bg-primary text-white",
            style = "font-weight: 600;",
            "Risk Assessment Results"
          ),

          card_body(
            style = "padding: 30px;",

            # Main risk display
            uiOutput("main_risk_display"),

            hr(style = "margin: 30px 0;"),

            # Comparison section
            h5(style = "color: #2c3e50; font-weight: 600; margin-bottom: 20px;", "How does this compare?"),
            uiOutput("comparison_section"),

            hr(style = "margin: 30px 0;"),

            # Interpretation
            h5(style = "color: #2c3e50; font-weight: 600; margin-bottom: 20px;", "What does this mean?"),
            uiOutput("interpretation_section")
          )
        ),

        # Disclaimer Card
        card(
          class = "mt-4",
          style = "border-left: 4px solid #f39c12;",
          card_body(
            style = "background-color: #fef9e7;",
            h6(style = "color: #9a7b0a; font-weight: 600; margin-bottom: 10px;", "Important Disclaimer"),
            p(style = "font-size: 13px; color: #7d6608; margin-bottom: 5px;",
              "This calculator is for ", strong("educational and research purposes only"),
              ". It is NOT a diagnostic tool and should NOT replace professional clinical assessment."),
            p(style = "font-size: 13px; color: #7d6608; margin-bottom: 5px;",
              "The model was developed using data from the Danish National Birth Cohort and may not generalize to other populations."),
            p(style = "font-size: 13px; color: #7d6608; margin: 0;",
              "If you have concerns about eating disorders, please consult a healthcare professional.")
          )
        )
      )
    )
  ),

  # About Tab
  nav_panel(
    title = "About",

    div(
      style = "max-width: 900px; margin: 0 auto; padding: 30px;",

      card(
        card_header(class = "bg-primary text-white", "About This Research"),
        card_body(
          style = "padding: 30px;",

          h4(style = "color: #2c3e50;", "The Study"),
          p("This risk calculator is based on research investigating the prediction of eating disorders in adolescents using machine learning and longitudinal cohort data."),

          div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 20px 0;",
            p(style = "margin: 0;",
              strong("Citation: "),
              "Katsiferis A, Joensen A, Petersen LV, Ekstrøm CT, Olsen EM, Bhatt S, Nguyen TL, Strandberg Larsen K. ",
              em("Developing machine learning models of self-reported and register-based data to predict eating disorders in adolescence. "),
              "npj Mental Health Research. 2025;4:65."
            ),
            p(style = "margin-top: 10px;",
              tags$a(href = "https://doi.org/10.1038/s44184-025-00179-x", target = "_blank", "Read Full Paper"),
              " | ",
              tags$a(href = "https://github.com/alkat19/ED_Pred", target = "_blank", "View Code on GitHub")
            )
          ),

          hr(),

          h4(style = "color: #2c3e50;", "Data Source"),
          p("The model was developed using the ", strong("Danish National Birth Cohort (DNBC)"),
            ", one of the largest longitudinal birth cohorts worldwide, following 96,822 children from before birth through young adulthood."),

          div(
            style = "display: flex; gap: 20px; flex-wrap: wrap; margin: 20px 0;",
            div(style = "flex: 1; min-width: 150px; background: #e8f4f8; padding: 15px; border-radius: 8px; text-align: center;",
                p(style = "margin: 0; font-size: 24px; font-weight: bold; color: #2c3e50;", "26,127"),
                p(style = "margin: 0; font-size: 12px; color: #6c757d;", "Participants")),
            div(style = "flex: 1; min-width: 150px; background: #e8f4f8; padding: 15px; border-radius: 8px; text-align: center;",
                p(style = "margin: 0; font-size: 24px; font-weight: bold; color: #2c3e50;", "~100"),
                p(style = "margin: 0; font-size: 12px; color: #6c757d;", "Predictors Evaluated")),
            div(style = "flex: 1; min-width: 150px; background: #e8f4f8; padding: 15px; border-radius: 8px; text-align: center;",
                p(style = "margin: 0; font-size: 24px; font-weight: bold; color: #2c3e50;", "18 years"),
                p(style = "margin: 0; font-size: 12px; color: #6c757d;", "Follow-up Period"))
          ),

          hr(),

          h4(style = "color: #2c3e50;", "The Model"),
          p("This calculator uses a simplified logistic regression model with the top 10 most influential predictors, achieving comparable performance to the full machine learning model:"),

          div(
            style = "display: flex; gap: 20px; flex-wrap: wrap; margin: 20px 0;",
            div(style = "flex: 1; min-width: 200px; background: #d4edda; padding: 15px; border-radius: 8px; text-align: center;",
                p(style = "margin: 0; font-size: 20px; font-weight: bold; color: #155724;", "AUC = 76.9"),
                p(style = "margin: 0; font-size: 12px; color: #155724;", "95% CI: 74.3 - 79.5")),
            div(style = "flex: 1; min-width: 200px; background: #cce5ff; padding: 15px; border-radius: 8px; text-align: center;",
                p(style = "margin: 0; font-size: 20px; font-weight: bold; color: #004085;", "ΔAUC = -1.5"),
                p(style = "margin: 0; font-size: 12px; color: #004085;", "vs. full model (non-significant)"))
          ),

          hr(),

          h4(style = "color: #2c3e50;", "Limitations"),
          tags$ul(
            style = "color: #495057;",
            tags$li("Developed in a Danish population — may not generalize to other settings"),
            tags$li("External validation studies are needed"),
            tags$li("Provides a risk estimate, not a diagnosis"),
            tags$li("Individual predictions carry inherent uncertainty"),
            tags$li("For research and educational purposes only")
          ),

          hr(),

          h4(style = "color: #2c3e50;", "Contact"),
          p("For questions: ", tags$a(href = "mailto:alexandros.katsiferis@sund.ku.dk", "alexandros.katsiferis@sund.ku.dk"))
        )
      )
    )
  ),

  # Predictors Tab
  nav_panel(
    title = "Predictors Explained",

    div(
      style = "max-width: 900px; margin: 0 auto; padding: 30px;",

      h3(style = "color: #2c3e50; margin-bottom: 30px;", "Understanding the Risk Factors"),
      p(style = "font-size: 16px; color: #6c757d; margin-bottom: 30px;",
        "Below is an explanation of each predictor and its relationship to eating disorder risk."),

      # Sex
      card(
        class = "mb-3",
        card_header(style = "background-color: #3498db; color: white; font-weight: 600;", "Sex"),
        card_body(
          p("Being female was the ", strong("strongest predictor"), " of future eating disorders."),
          p("This aligns with epidemiological evidence, though EDs affect all genders and may be underdiagnosed in males."),
          div(style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "Female sex → Higher ED risk")
        )
      ),

      # Emotional Symptoms
      card(
        class = "mb-3",
        card_header(style = "background-color: #9b59b6; color: white; font-weight: 600;", "Emotional Symptoms"),
        card_body(
          p("Measured using the SDQ emotional symptoms subscale (child-reported)."),
          p("Captures worry, unhappiness, nervousness, somatic complaints, and fears."),
          div(style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "Higher emotional symptoms → Higher ED risk")
        )
      ),

      # Body Satisfaction
      card(
        class = "mb-3",
        card_header(style = "background-color: #e74c3c; color: white; font-weight: 600;", "Body Satisfaction"),
        card_body(
          p("Measures satisfaction with physical appearance."),
          p("Body dissatisfaction is a ", strong("key psychological factor"), " — negative body image often precedes and maintains disordered eating."),
          div(style = "background-color: #f8d7da; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "Lower satisfaction → Higher ED risk")
        )
      ),

      # Peer Problems
      card(
        class = "mb-3",
        card_header(style = "background-color: #f39c12; color: white; font-weight: 600;", "Peer Relationship Problems"),
        card_body(
          p("SDQ subscale (parent-reported) measuring social difficulties."),
          p("Includes being bullied, few friends, and social isolation — may contribute to ED risk through low self-esteem."),
          div(style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "More peer problems → Higher ED risk")
        )
      ),

      # Hyperactivity
      card(
        class = "mb-3",
        card_header(style = "background-color: #1abc9c; color: white; font-weight: 600;", "Hyperactivity/Inattention"),
        card_body(
          p("SDQ subscale (parent-reported) measuring restlessness and distractibility."),
          p("Interestingly, ", strong("lower"), " scores predicted higher ED risk — possibly indicating greater self-control capacity predisposing to restrictive behaviors."),
          div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "Lower hyperactivity → Higher ED risk")
        )
      ),

      # Child BMI
      card(
        class = "mb-3",
        card_header(style = "background-color: #34495e; color: white; font-weight: 600;", "Child's BMI at Age 7"),
        card_body(
          p("Higher childhood BMI may create vulnerability through weight stigma, body dissatisfaction, and early dieting attempts."),
          div(style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "Higher childhood BMI → Higher ED risk")
        )
      ),

      # Parental BMI
      card(
        class = "mb-3",
        card_header(style = "background-color: #7f8c8d; color: white; font-weight: 600;", "Parental BMI (Maternal & Paternal)"),
        card_body(
          p(strong("Lower"), " parental BMI predicted higher ED risk — may reflect genetic vulnerability, shared environmental factors, or intergenerational transmission of weight-related attitudes."),
          div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "Lower parental BMI → Higher ED risk")
        )
      ),

      # Conduct Problems
      card(
        class = "mb-3",
        card_header(style = "background-color: #c0392b; color: white; font-weight: 600;", "Conduct Problems"),
        card_body(
          p("SDQ subscale (child-reported) capturing temper, fighting, lying, and stealing."),
          p("May relate to emotional dysregulation and impulsive behaviors manifesting in binge eating."),
          div(style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "More conduct problems → Higher ED risk")
        )
      ),

      # Stress
      card(
        class = "mb-3",
        card_header(style = "background-color: #8e44ad; color: white; font-weight: 600;", "Stress Level"),
        card_body(
          p("From the Stress in Children (SiC) questionnaire — measures physical, psychological, and behavioral stress responses."),
          p("Stress is a well-established ED risk factor; EDs may develop as maladaptive coping mechanisms."),
          div(style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px;",
              strong("Direction: "), "Higher stress → Higher ED risk")
        )
      )
    )
  ),

  # Footer
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://doi.org/10.1038/s44184-025-00179-x",
      target = "_blank",
      style = "color: #95a5a6; font-size: 12px;",
      "Katsiferis et al. (2025) npj Mental Health Research"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # Calculate risk
  predicted_risk <- reactive({
    log_odds <- coefficients$intercept +
      (if (input$sex == "male") coefficients$sex_male else 0) +
      coefficients$emotional_symptoms * input$emotional +
      coefficients$body_satisfaction * input$body_satisfaction +
      coefficients$peer_problems * input$peer_problems +
      coefficients$hyperactivity * input$hyperactivity +
      coefficients$child_bmi_7y * input$child_bmi +
      coefficients$maternal_bmi * input$maternal_bmi +
      coefficients$conduct_problems * input$conduct +
      coefficients$paternal_bmi * input$paternal_bmi +
      coefficients$stress * input$stress

    risk <- 1 / (1 + exp(-log_odds))
    return(risk)
  })

  # Risk category
  risk_category <- reactive({
    risk <- predicted_risk()
    if (risk < 0.02) {
      list(category = "Lower Risk", color = "#18bc9c", bg = "#d1f2eb")
    } else if (risk < 0.05) {
      list(category = "Moderate Risk", color = "#f39c12", bg = "#fef5e7")
    } else {
      list(category = "Elevated Risk", color = "#e74c3c", bg = "#fdedec")
    }
  })

  # Main risk display
  output$main_risk_display <- renderUI({
    risk <- predicted_risk()
    cat_info <- risk_category()

    div(
      style = "text-align: center;",

      # Risk gauge
      div(
        style = paste0(
          "width: 200px; height: 200px; border-radius: 50%; margin: 0 auto 20px auto; ",
          "display: flex; flex-direction: column; justify-content: center; align-items: center; ",
          "background: linear-gradient(135deg, ", cat_info$bg, " 0%, white 100%); ",
          "border: 4px solid ", cat_info$color, "; ",
          "box-shadow: 0 4px 15px rgba(0,0,0,0.1);"
        ),
        span(
          style = paste0("font-size: 42px; font-weight: 700; color: ", cat_info$color, ";"),
          paste0(round(risk * 100, 1), "%")
        ),
        span(
          style = paste0("font-size: 14px; font-weight: 600; color: ", cat_info$color, ";"),
          cat_info$category
        )
      ),

      p(
        style = "color: #6c757d; font-size: 14px; max-width: 400px; margin: 0 auto;",
        "Estimated probability of developing an eating disorder by late adolescence (approximately age 18)"
      )
    )
  })

  # Comparison section
  output$comparison_section <- renderUI({
    risk <- predicted_risk()
    ratio <- risk / avg_risk
    cat_info <- risk_category()

    div(
      div(
        style = "display: flex; gap: 20px; flex-wrap: wrap;",

        # Your risk card
        div(
          style = paste0(
            "flex: 1; min-width: 150px; ",
            "background: linear-gradient(135deg, ", cat_info$bg, " 0%, white 100%); ",
            "padding: 20px; border-radius: 10px; text-align: center; ",
            "border: 2px solid ", cat_info$color, ";"
          ),
          p(style = "margin: 0; font-size: 12px; color: #6c757d;", "Estimated Risk"),
          p(style = paste0("margin: 5px 0; font-size: 32px; font-weight: 700; color: ", cat_info$color, ";"),
            paste0(round(risk * 100, 1), "%"))
        ),

        # Population average card
        div(
          style = "flex: 1; min-width: 150px; background: linear-gradient(135deg, #f8f9fa 0%, white 100%); padding: 20px; border-radius: 10px; text-align: center; border: 2px solid #95a5a6;",
          p(style = "margin: 0; font-size: 12px; color: #6c757d;", "Population Average"),
          p(style = "margin: 5px 0; font-size: 32px; font-weight: 700; color: #95a5a6;",
            paste0(round(avg_risk * 100, 1), "%"))
        )
      ),

      # Comparison text
      div(
        style = "text-align: center; margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 8px;",
        if (ratio > 1.1) {
          p(style = "margin: 0; font-size: 15px;",
            "The estimated risk is ", strong(paste0(round(ratio, 1), "x higher")),
            " than the population average")
        } else if (ratio < 0.9) {
          p(style = "margin: 0; font-size: 15px;",
            "The estimated risk is ", strong(paste0(round(1/ratio, 1), "x lower")),
            " than the population average")
        } else {
          p(style = "margin: 0; font-size: 15px;",
            "The estimated risk is ", strong("similar"), " to the population average")
        }
      )
    )
  })

  # Interpretation section
  output$interpretation_section <- renderUI({
    risk <- predicted_risk()
    cat_info <- risk_category()

    interpretation <- if (risk < 0.02) {
      list(
        main = "This risk estimate is below the population average.",
        detail = "The combination of factors suggests a lower-than-average probability of developing an eating disorder. However, eating disorders can affect anyone regardless of risk profile.",
        action = "Continue to promote healthy attitudes toward food and body image. Maintain open communication about emotional wellbeing."
      )
    } else if (risk < 0.05) {
      list(
        main = "This risk estimate is around or somewhat above the population average.",
        detail = "The combination of factors suggests a moderate probability. This does not mean an eating disorder will develop, but indicates factors that warrant attention.",
        action = "Consider monitoring for early signs of disordered eating. Promoting body acceptance, healthy coping strategies, and strong peer relationships may be beneficial."
      )
    } else {
      list(
        main = "This risk estimate is elevated compared to the population average.",
        detail = "The combination of factors suggests a higher-than-average probability. This is not a diagnosis, but indicates that preventive attention may be warranted.",
        action = "Consider consultation with a healthcare professional. Early intervention focusing on body image, stress management, and emotional regulation may be beneficial."
      )
    }

    div(
      div(
        style = paste0("padding: 20px; border-radius: 8px; background-color: ", cat_info$bg, "; border-left: 4px solid ", cat_info$color, ";"),
        h6(style = paste0("color: ", cat_info$color, "; margin-bottom: 10px;"), interpretation$main),
        p(style = "color: #495057; font-size: 14px; margin-bottom: 15px;", interpretation$detail),
        div(
          style = "background-color: white; padding: 12px; border-radius: 6px;",
          p(style = "margin: 0; font-size: 13px;",
            strong("Suggested consideration: "), interpretation$action)
        )
      ),

      div(
        style = "margin-top: 15px; padding: 15px; background-color: #f8f9fa; border-radius: 8px;",
        p(style = "margin: 0; font-size: 13px; color: #6c757d;",
          strong("Remember: "),
          "Risk estimates represent probabilities, not certainties. Many with elevated risk never develop eating disorders, while others with lower risk may still be affected.")
      )
    )
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
