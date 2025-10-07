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
        tabsetPanel(
          tabPanel(
            "Tool description", 
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
          ), 
          tabPanel(
            "Approach to development",
            br(),
            withTags(
              div(
                p(class = "spaced-paragraph",  
                  strong("Project overview")
                ),
                p(class = "spaced-paragraph",
                  "In response to a request by the Codex Committee on Food Hygiene (CCFH) at its fifty-second session, formal risk assessment models were developed by the ",
                  a(href = "https://www.who.int/publications/i/item/9789240074507",
                    "Joint FAO/WHO Expert meeting on microbiological risk assessment (JEMRA) of ",
                    em("Listeria monocytogenes"),
                    "in foods: Part 1 formal models (FAO HQ, Rome, Italy: 24 – 28 October 2022)", 
                    target = "_blank"),
                  ". These models took into account the effects of agrifood practices, climate change, and the latent possibility of cross-contamination along the production chain for produce and seafood commodities."
                ),
                
                
                p(class = "spaced-paragraph",
                  "Quantitative risk assessment (QRA) models were commissioned by WHO to a team of risk modellers, who programmed them in open-source software according to the designed formal models and based upon an extensive literature review for data retrieval and JEMRA inputs."
                ),
                p(class = "spaced-paragraph",
                  "The relevant literature, derived from and supporting the development of the models incorporated in the tool, is available in the publicly accessible special issue ",
                  a(href = "https://www.mdpi.com/journal/foods/special_issues/83GCU364KY",
                    "Quantitative Risk Assessment of ",
                    em("Listeria monocytogenes"),
                    " in Foods",
                    target = "_blank"),
                  "."
                ),
                p(class = "spaced-paragraph",  
                  "QRA models developed were reviewed and used by ",
                  a(href="https://openknowledge.fao.org/server/api/core/bitstreams/7be15013-c4a0-4fc3-9088-4db8a2fe6a43/content",
                    "Joint FAO/WHO Expert meeting on microbiological risk assessment of ",
                    em("Listeria monocytogenes"),
                    " in foods: Part 2 risk assessment (WHO HQ, Geneva, Switzerland: 29 May – 2 June 2023)",
                    target = "_blank"),
                  "in order to provide scientific advice to CCFH."
                ),
                p(class = "spaced-paragraph",  
                  "The application is built using the ",
                  a(href = "https://worldhealthorganization.github.io/qraLm/",
                    "qraLm",
                    target = "_blank"),
                  "R package and was developed under the scope of the ",
                  a(href = "https://www.who.int/publications/m/item/jemra-of-listeria-monocytogenes-in-foods",
                    "Joint FAO/WHO Expert Meeting on Microbiological Risk Assessment",
                    target = "_blank"),
                  ". Full definitions of model parameters and explanations of functions can be found on the ",
                  a(href = "https://github.com/WorldHealthOrganization/qraLm/",
                    "Function reference manual",
                    target = "_blank"),
                  "for the qraLm package."
                ),
                
                p(class = "spaced-paragraph",  
                  "For further information, visit the GitHub Repository: ",
                  a(href = "https://github.com/WorldHealthOrganization/Shiny_qraLm",
                    "Shiny_qraLm",
                    target = "_blank")
                ),
                
                p(class = "spaced-paragraph",  
                  strong("Acknowledgements")
                ),
                p(class = "spaced-paragraph",  
                  "This product was commissioned by the Department of Nutrition and Food Safety of the World Health Organization. The models were developed by:",
                  tags$ul(
                    tags$li(
                      "Vasco Cadavez, Laboratório para a Sustentabilidade e Tecnologia em Regiões de Montanha, Campus de Santa Apolónia, Instituto Politécnico de Bragança, 5300-253 Bragança, Portugal,"
                    ),
                    tags$li(
                      "Régis Pouillot, Independent Researcher, 18 rue Mohamed Al Ghazi, Rabat 10170, Morocco,"
                    ),
                    tags$li("Laurent Guillier, Risk Assessment Department, French Agency for Food, Environmental and Occupational Health & Safety (Anses), 14 rue Pierre et Marie Curie, 94701 Maisons-Alfort, France, and"
                    ),
                    tags$li("Ursula Gonzalez Barron, Laboratório para a Sustentabilidade e Tecnologia em Regiões de Montanha, Campus de Santa Apolónia, Instituto Politécnico de Bragança, 5300-253 Bragança, Portugal"
                    )
                  )
                  ),
                  p(class = "spaced-paragraph",      
                    "Special appreciation is extended to the members of the Joint FAO/WHO Expert Group who participated in the Joint FAO/WHO Expert Meetings on Microbiological Risk Assessment of ",
                    em("Listeria monocytogenes"),
                    " in Foods: Part 1 and Part 2, for their valuable contributions to this project."
                  ),
                  p(class = "spaced-paragraph",  
                    strong("Conflicts of Interest:")
                  ),
                  p(class = "spaced-paragraph",
                    "A comprehensive assessment of potential conflicts of interest among all individuals involved in the product development and the expert meeting was performed. No conflicts of interest that could have compromised the independence or impartiality of the tool’s development were identified at the time of the product’s development or the meetings."
                  )
              )
            )
            ) # End withtags
          ) # End tabPanel
        ) # End column
      ) # End row
    ) # End tag
}

aboutProjectContentUI <- function(id) {
  uiOutput(id)
}
