ca_stageSelector_ui <- function(id, ca_stages) {
    ns = NS(id)
    if (isTRUE(getOption("myVerbose"))) print(ns("selection"))
    
    shinyWidgets::pickerInput(
        inputId = ns("selection"),
        label = h3("Select Stage"),
        choices = ca_stages,
        options = list(
            style = "btn-primary"
        ),
        selected = names(ca_stages)[1]
    )
}

ca_stageSelector_server <- function(input, output, session, suffix, ca_stages) {
    ns = NS(suffix)
    id = ns("selector")

    output[[id]] <- renderUI({
        ca_stageSelector_ui(id, ca_stages)
    })
}
