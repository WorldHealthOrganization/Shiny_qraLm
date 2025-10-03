about_ui <- function(id) {
  shinydashboard::tabItem(
    tabName = id,
    shinydashboard::dashboardBody(
      fluidRow(
        column(
          width = 12,
          # Centered logo and page title
          h1(HTML("<strong>WHO Risk Estimation Tool for <i>Listeria monocytogenes</i> in Foods</strong>"), 
             style = "text-align:center; color: #0093D5; padding-top:20px;"),
          aboutProjectContentUI("about_project_content")
        )
      )
    )
  )
}

about_server <- function(input, output, session, id) {
  output$about_project_content <- renderUI({
    aboutProjectContent()
  })
}

aboutProjectContent <- function() {
  tagList(
    # Using a tabsetPanel to clearly separate sections
    fluidRow(
      column(
        width = 12,
        # tabsetPanel(
        #   tabPanel(
        hr(),
        withTags(
          div(
            p(class = "spaced-paragraph",
              "This WHO Risk Estimation Tool for ",
              em("Listeria monocytogenes"),
              " in Foods was developed to support the quantitative estimation of listeriosis risk per serving several food commodities. It currently supports risk assessments for three food products: Ready to eat (RTE) diced cantaloupe, frozen vegetables, and RTE cold-smoked fish."),
            tags$ul(
              class = "spaced-paragraph",
              tags$li(
                strong("RTE Cantaloupe:"), 
                " The model for RTE cantaloupe includes stages from pre-harvesting, harvesting, cleaning, and washing to processing, cold-chain storage, and consumer handling."
              ),
              tags$li(
                strong("Frozen Vegetables:"), 
                " This model focuses on processing beginning at the freezing facility, with the assumption that incoming vegetables have already been preconditioned (e.g., trimmed, peeled, washed). The model includes blanching, potential cross-contamination during freezing and packaging, and consumer handling activities such as defrosting and cooking."
              ),
              tags$li(
                strong("RTE Cold-Smoked Salmon:"), 
                " The model targets RTE cold-smoked salmon produced via brining, followed by smoking, slicing, and packing. It also accounts for cold-chain storage and consumer handling."
              )
            ),
            p(class = "spaced-paragraph",
              "Stochastic modeling is used to simulate the risk of illness from ",
              em("Listeria monocytogenes,"), 
              " accounting for both within-lot variability (differences between individual units in a single production lot) and between-lot variability (differences across production lots). For each food commodity, the model generates a matrix of production lots, with each lot comprising multiple individual units. Contamination levels are assigned using probability distributions."
            ),
            p(class = "spaced-paragraph",
              "Risk is estimated for each unit, then averaged across the units to produce a lot-level risk. Subsequently, the average or median risk per serving is calculated across all lots, providing an overall estimate used for comparison and scenario analysis."),
            p(
              class = "spaced-paragraph",
              "The updated dose-response model incorporating both the virulence class of the ",
              em("L. monocytogenes"),
              " strain and demographic factors such as age and sex, can be used, which improves the accuracy of risk estimation across population groups and strain types."
            ),
            
            p(
              class = "spaced-paragraph",
              "The tool also allows users to assess the impact of different testing strategies and sample sizes, supporting informed decision-making under varied production and surveillance conditions."
            ),
            
            p(
              class = "spaced-paragraph",
              "This tool is intended to demonstrate the capabilities of the developed models. Due to storage limitations, certain parameters are restricted within this application. To fully explore the functionality and potential of the models, we recommend installing the application locally on your own machine. The source code is openly accessible in the following GitHub repository: ",
              a(href = "https://github.com/WorldHealthOrganization/Shiny_qraLm", 
                "https://github.com/WorldHealthOrganization/Shiny_qraLm", 
                target = "_blank")
            ),
            
            p(
              class = "spaced-paragraph",
              "Full definitions of model parameters and explanations of functions can be found on the ",
              a(href = "https://github.com/WorldHealthOrganization/qraLm",
                "Function reference manual",
                target = "_blank"),
              " for the ",
              code("qraLm"),
              " package."
            )
            
            
          )
        )
      ) # , # End column
      # tabPanel(
      #   "Documentation",
      #   br(),
      #   # Including the README.Rmd content for detailed documentation
      #   includeMarkdown("README.Rmd")
      # )
    ) # End row
  )
}

aboutProjectContentUI <- function(id) {
  uiOutput(id)
}
