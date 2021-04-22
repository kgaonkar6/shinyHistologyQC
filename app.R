
## Advanced App 
library(shiny)
library(DT)
library(tidyverse)
library(shinydashboard)

input_dir <- file.path("input")
path_dx <- read_tsv(file.path(input_dir,"pathology_diagnosis_for_subtyping.tsv")) %>%
    dplyr::select(pathology_diagnosis, broad_histology, short_histology)
path_free_text <- read_tsv(file.path(input_dir,"pathology_free_text_diagnosis_for_subtyping.tsv")) %>%
    dplyr::select(pathology_free_text_diagnosis, broad_histology, short_histology)

source("code/util/check_rows_cols.R")
source("code/util/primary_site_matched_CNS_region.R")

# Define UI ----
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title="Histology QC"),
        dashboardSidebar(
            
            sidebarMenu(
                fileInput("upload", "Upload a pbta histology file")
            ),
            selectInput("prev_release",label="Previous releases",choices = c("release-v18-20201123","20210311-data")),
            selectInput("vars",label="Columns",choices = setdiff(names(latest_hist),
                                                                 c("CNS_region","broad_histology","short_histology"))),
            tags$style(type="text/css", "#report {background-color:orange;color: black;font-family: Courier New;width:100%}"),
            downloadButton("report", "Generate report",class = "butt1")),
        dashboardBody(navbarPage("OpenPBTA",
            tabPanel("summary",
                     title="Column summary",
                        fluidRow(
                            box(title = "Count",
                                dataTableOutput("count")
                            ),
                            box(title = "Change log",
                                dataTableOutput("table"),
                                collapsible = TRUE,
                                collapsed = TRUE),
                            
                            box(title = "Note",
                                textOutput("added")
                            )
                        )
        ),
        tabPanel(
                title="Histology",
                actionButton(inputId = "cns_who", label = "Fill OpenPBTA specific columns", icon = icon("pencil")),
                dataTableOutput("hist")
                )
    )
)
)
)

# Define server logic ----
server <- function(input, output) {
    latest_hist <-  reactive({
        req(input$upload)
        read_tsv(input$upload$datapath)
        })
    
    prev_hist <- reactive({
        if(input$prev_release=="20210311-data"){
            read_tsv("20210311-data/pbta-histologies-base.tsv")
        }else{
            read_tsv("release-v18-20201123/pbta-histologies.tsv")
        }
    })    
        
    output$count <- DT::renderDataTable({
        check_rows(new_hist = latest_hist(),old_hist = prev_hist(),
                   column_name = input$vars,getcount = TRUE)
    })    
    output$added <- renderPrint({
        check_rows(new_hist = latest_hist(),old_hist = prev_hist(),
               column_name = input$vars)
    })

    change_log <- reactive({check_values(new_hist = latest_hist(),old_hist = prev_hist(),
                 column_name = input$vars) %>%
            arrange(typeof_change)
    })
    
    output$table <- DT::renderDataTable(
            DT::datatable(change_log(),
                          filter = 'top',
                          rownames = F,
                          extensions='Buttons',
                          options = list(pageLength = 15,buttons=c("copy","csv","excel"),
                                         dom='Bfrtip')) %>%
                                            formatStyle("typeof_change",
                                                        transform = "rotateX(0deg) rotateY(0deg) rotateZ(0deg)",
                                                        backgroundColor=styleEqual(c("CRITICAL CHANGE","REVIEW NEEDED"),c("salmon","lightblue")))

    )
    
    # don't add cns_region by default
    add_cnsregion_who <- reactiveVal(FALSE)
    # if input add_cnsregion is selected
    observeEvent(input$cns_who , {
        add_cnsregion_who(TRUE) 
    })
    
    
    output$hist <- DT::renderDataTable(
        if (add_cnsregion_who()){
            # Only samples with 'Other' in pathology_diagnosis will be need to be matched by path_free_text
            
            latest_hist <- dplyr::select(latest_hist(),c(-broad_histology,-short_histology))
            latest_hist_other <- latest_hist()  %>%
                dplyr::filter(pathology_diagnosis == "Other") %>%
                left_join(path_free_text,by="pathology_free_text_diagnosis") 
            
            #Remove samples with 'Other' in pathology_diagnosis that was already matched above
            #Adding NAs as well since that could be a value here and if it is not included in this step the sample will be missed out
            latest_hist <- latest_hist()  %>%
                dplyr::filter(pathology_diagnosis != "Other"|
                                  is.na(pathology_diagnosis) |
                                  # add Normals
                                  sample_type=="Normal") %>%
                left_join(path_dx,by="pathology_diagnosis") %>%
                bind_rows(latest_hist_other)
            # add CNS regions
            
            DT::datatable({
                latest_hist <- get_CNS_region(histology=latest_hist(),
                                                        CNS_match_json=file.path(input_dir,"CNS_primary_site_match.json")
                                              
                                              )},
            filter = 'top',
            rownames = F,
            extensions='Buttons',
            options = list(pageLength = 15,buttons=c("copy","csv","excel"),
                           dom='Bfrtip'))
            
        } else{
            DT::datatable(latest_hist(),
                          filter = 'top',
                          rownames = F,
                          extensions='Buttons',
                          options = list(buttons=c("copy","csv","excel"),
                                         dom='Bfrtip'))
        }
    )
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            if(input$prev_release=="20210311-data"){
                prev_hist_file <- "20210311-data/pbta-histologies-base.tsv"
            }else{
                prev_hist_file <- "release-v18-20201123/pbta-histologies.tsv"
            }
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render("QC_histology.Rmd", output_file = file,
                              params=list( latest_release = input$upload$datapath, 
                                           prev_release = prev_hist_file),
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
}

# Run the app ----
shinyApp(ui = ui, server = server)


