ui <- fluidPage(
    useShinyjs(),
    tags$style(
        "
        .scroll-vertical {
            overflow: auto;
            white-space: nowrap;
        }
        .shiny-notification {
            position:fixed;
            top: 0;
            right: 0;
            background-color:#446fb7; 
            padding:1rem; border-radius:1rem;
        }
        "
    ),
    navbarPage(
        title = "Farm-level Water-Energy-Food-Environment (WEFE) Nexus Index",
        id="mainPage",
        tabPanel("Home", 
            fluidRow(
                column(6,HTML(
                    "The primary objective of the farm-level WEFE Nexus Index is to assess the economic performance
                    of agricultural producers within the Water-Energy-Food-Environment (WEFE) Nexus.<br/>
                    <br/>
                    One way to explicitly place agricultural production within the WEFE Nexus is by developing a
                    composite indicator that will reflect interlinkages between agricultural production and the water,
                    energy, food and ecosystems pillars of the Nexus.<br/>
                    <br/>
                    Each component of the suggested farm-level WEFE Nexus index should reflect at least one bilateral
                    link between Nexus nodes. We suggest technical efficiency levels as one of the component of the
                    composite indicator that reflects link between Water, Energy (inputs) and Food (output). Concerning
                    links of Water, Energy and Food with Environment examples of relevant indicators include GHG
                    emissions from fuels usage and fertilizers application, pesticide pollution, adoption of agri-
                    environmental schemes etc.<br/>
                    <br/>
                    Example of the basic interconnections between crop production and the components of the WEFE Nexus 
                    from an input-output perspective.<br/>
                    <img src='homeImage.png' style=\"max-width: 100%\"><br/>")
                ),
                column(6, HTML(
                    "This application provides the user with a flexible and interactive tool to calculate farm-level values of WEFE Nexus index. </br>
                    </br>
                    <b>The application enables the user to: </b></br>
                    </br>
                    <ol>
                        <li>Estimate technical efficiency: either using a parametric (SFA) or an non-parametric (DEA) approach;</li>
                        <li>Determine endogenous weights using the Benefit of the Doubt approach to compose the farm-level WEFE Nexus index;</li>
                        <li>Calculate the WEFE Nexus index values.</li>
                    </ol></br></br>"),
                    column(3, ""),
                    column(6, align="center", actionLink("calculateWEFE", "Calculate WEFE Nexus Index",  style="color:white; background-color:#446fb7; padding:1.5rem; border-radius:1rem;"))
                    ),
                column(6, align="center", HTML(
                  "</br></br><iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/RLdkI2lrO3c?si=69-nfZFGWACk7Qy1\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share\" referrerpolicy=\"strict-origin-when-cross-origin\" allowfullscreen></iframe>"
                ))
            ),
            HTML(
                "<div style=\"display:flex; justify-content:center;\">
                <div style=\"\"><img src='primaLogo.png'></div>
                <div style=\"font-size:1rem; margin-block:auto;\"> This application is output of the SIGMA Nexus project that has received funding from the PRIMA Foundation
                    and European Union’s H2020 research and innovation program under Grant Agreement No. 1943</div>
                <div style=\"\"><a href=\"https://sigma-nexus.eu/\" target=\"blank\" rel=\"noopener\"><img src='sigmaNexusLogo.png'></a></div>
                </div>"
            )
        ),
        tabPanel("Model",
            value='model', 
            sidebarLayout(
                sidebarPanel(
                    fileInput(
                        "file1", 
                        HTML("Upload data in csv format", as.character(actionButton("helpCsv", "Help"))),
                        accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
                    ),
                    h4("Technical Efficiency"),
                    selectInput(
                        "outputSelected",
                        HTML("Choose an output variable *", as.character(actionButton("helpOutput", "Help"))),
                        ""
                    ),
                    selectInput(
                        "inputSelected",
                        HTML("Choose input variables *", as.character(actionButton("helpInput", "Help"))),
                        "",
                        multiple = TRUE
                    ),
                    selectInput(
                        "continuousInputSelected",
                        HTML("Choose continuous contextual variable", as.character(actionButton("helpContinuousInput", "Help"))),
                        "",
                        multiple = TRUE
                    ),
                    selectInput(
                        "categoricalInputSelected",
                        HTML("Choose categorical contextual variable", as.character(actionButton("helpCategoricalInput", "Help"))),
                        "",
                        multiple = TRUE
                    ),
                    radioButtons(
                        "deaORsfa", 
                        HTML("Model *", as.character(actionButton("helpDeaOrSfa", "Help"))),
                        choices = list("DEA" = "dea", "SFA" = "sfa"),
                        selected = "dea"
                    ),
                    uiOutput("featureSelection"),
                    uiOutput("modelOptions"),

                    actionButton("run", "Run Model"),
                    actionButton("updatedDataset", "Updated dataset"),
                    h4("Other composite indicator components"),
                    h6(HTML("<i>(Based on the idea of <a href=\"https://fvidoli.shinyapps.io/compind_app\" target=\"blank\" rel=\"noopener\"> Compind web app (shinyapps.io)</a>)</i>")),

                    selectInput(
                        "goodPerformanceSelected",
                        HTML("Choose good performance indicators *", as.character(actionButton("helpGoodPerformance", "Help"))),
                        "",
                        multiple = TRUE
                    ),
                    selectInput(
                        "badPerformanceSelected",
                        HTML("Choose bad performance indicators", as.character(actionButton("helpBadPerformance", "Help"))),
                        "",
                        multiple = TRUE
                    ),
                    radioButtons(
                        "bodFrontierSelected", 
                        HTML("Choose BoD model *", as.character(actionButton("helpBoD", "Help"))),
                        choices = list(
                            "BoD (ci_bod)" = "ci_bod",
                            "Robust BoD (ci_rbod)" = "ci_rbod",
                            "Constrained BoD (ci_bod_constr)" = "ci_bod_constr",
                            "Constrained BoD with bad indicators (ci_bod_constr_bad)" = "ci_bod_constr_bad",
                            "Robust Constrained BoD with bad indicators (ci_rbod_constr_bad)" = "ci_rbod_constr_bad"
                            ),
                        selected = "ci_bod"
                    ),
                    uiOutput("ciBodOptions"),
                    actionButton("runComposite", "Run Nexus Index"),
                    downloadButton("downloadDataset", "Download dataset"),
                ),
                mainPanel(
                    tabsetPanel(
                        id = "tabset",
                        tabPanel(
                            "Technicial Efficiency",
                            value = "techEfficiency",
                            verbatimTextOutput("summaryOutput"),
                            fluidRow(
                                column(6,plotOutput("histo")),
                                column(6,plotOutput("graph")),
                            )
                        ),
                        tabPanel(
                            "Other composite indicator components",
                            value = "compositeIndicator",
                            verbatimTextOutput("summaryComponentsGoodOutput"),
                            verbatimTextOutput("summaryComponentsBothOutput")
                        ),
                        tabPanel(
                            "WEFE Nexus Index values", 
                            verbatimTextOutput("summaryCompositeOutput"),
                            fluidRow(
                            column(6, plotOutput("histoComposite"))
                            )
                        )
                    )
                )
            )
        )
    )
)
