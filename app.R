
library(shiny)
library(bs4Dash)
library(tidyverse)
library(MQMF)
library(bslib)
# library(shinythemes)
library(shinyWidgets)
# devtools::install_github("https://github.com/haddonm/MQMF")
library(janitor)
library(FSA)

library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

# vb_dat <- 
#     read_csv("additional_info/Pikeperch age CL.csv") |> 
#     clean_names()


my_ui <-
    dashboardPage(
        header = dashboardHeader(
            usei18n(i18n),
            title = dashboardBrand(
                title = "von Bertalanffy" |> i18n$t(),
                color = "primary",
                image = "https://avatars.githubusercontent.com/u/99745785?s=400&u=eeb3a46b56826f1511e2c387374759c4c5fd109e&v=4"
            )
        ),
        sidebar = dashboardSidebar(
            usei18n(i18n),
            sidebarMenu(
                menuItem("Overview" |> i18n$t(),
                         tabName = "home_tab",
                         icon = icon("home")),
                menuItem("Parameters" |> i18n$t(),
                         tabName = "von_bert",
                         icon = icon("sliders-h", lib = "font-awesome"))
            ),
            br(),
            selectInput("lang_select", 
                        label = NULL, 
                        choices = list("English" = "en", 
                                       "Lietuvių" = "lt"), 
                        selected = "en")
        ),
        body = dashboardBody(
            usei18n(i18n),
            tabItems(
                # Home page tab
                tabItem(tabName = "home_tab",
                        h3("Title of the homepage" |> i18n$t()),
                        p("Info about the website goes here" |> i18n$t()),
                        p("Para 2" |> i18n$t()),
                        uiOutput(outputId = "front_logo")
                        
                ),
                
                tabItem(tabName = "von_bert",
                        h1("von Bertalanffy Growth model" |> i18n$t()),
                        br(),
                        fluidRow(
                            column(width = 4,
                                   # b1
                                   box(title = "Parameter selection" |> i18n$t(),
                                       sliderInput(inputId = "vb_maxage",
                                                   label = "Max age" |> i18n$t(),
                                                   min = 1, 
                                                   max = 50, 
                                                   value = 5,
                                                   step = 0.5, 
                                                   post = " years" |> i18n$t()),
                                       sliderInput(inputId = "vb_k",
                                                   label = "Instantaneous growth rate (K)" |> i18n$t(),
                                                   min = 0.05, 
                                                   max = 0.95, 
                                                   value = 0.2, 
                                                   step = 0.05),
                                       sliderInput(inputId = "vb_linf",
                                                   label =  HTML(paste0("Maximum length" |> i18n$t(), "(L", tags$sub("\u221E"), ')')),
                                                   min = 10, 
                                                   max = 100, 
                                                   value = 20, 
                                                   step = 1, 
                                                   post = "cm"),
                                       sliderInput(inputId = "vb_t0",
                                                   label = HTML(paste0("Age when length = 0cm"  |> i18n$t(), "(t", tags$sub("0"), ')')),
                                                   min = -0.5,
                                                   max = 0, 
                                                   value = -0.15, 
                                                   step = 0.01, 
                                                   post = " years"),
                                       sliderInput(inputId = "vb_cvlen",
                                                   "Amount of variation"  |> i18n$t(),
                                                   min = 0, 
                                                   max = 0.5, 
                                                   value = 0.1, 
                                                   step = 0.01),
                                       width = 12
                                   ), 
                                   box(title = "Upload your own length-age data" |> i18n$t(),
                                       width = 12, 
                                       fileInput("file1", 
                                                 "Choose CSV File" |> i18n$t(),
                                                 multiple = FALSE,
                                                 buttonLabel = "Browse"|> i18n$t(),
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")),
                                       uiOutput(outputId = "select_age_column_ui"),
                                       uiOutput(outputId = "select_length_column_ui"),
                                       actionButton(inputId = "add_points",
                                                    label = "Add data points" |> i18n$t()),
                                       actionButton(inputId = "calc_VB_pars",
                                                   label = "Best parameters" |> i18n$t())),
                                   box(title = "Calculate parameters" |> i18n$t(), 
                                       width = 12, 
                                       tableOutput(outputId = "vb_params_table"))
                            ),
                            column(width = 8,
                                   # b2
                                   box(title = "Growth curve based on von Bertalanffy parameters" |> i18n$t(),
                                       plotOutput("vb_plot1"),
                                       width = 12,
                                       align="center"),
                                   br(),
                                   # b3
                                   box(title = "Distribution of sizes in age groups" |> i18n$t(),
                                       plotOutput("vb_plot2"),
                                       width = 12,
                                       align="center")
                            )
                        )
                )
            )
        )
    )

my_server <- function(input, output, session) {
    
    output$front_logo <- renderUI({
        if(input$lang_select == "en"){
            img(src='logos_english-removebg-preview.png', align = "center", width = "75%")
        } else if(input$lang_select == "lt"){
            img(src='logos_lithuanian-removebg-preview.png', align = "center", width = "75%")
        }
        
    })
    
    observeEvent(input$lang_select,{
        update_lang(session, input$lang_select)
        
    })
    
    ### Two functions to be created
    ### This is function that is implemented in the MQMF package
    vB <- function (p, ages)
    {
        return(p[1] * (1 - exp(-p[2] * (ages - p[3]))))
    }
    
    ### This is a function from another package that allows plotting length distributions, it should be implemented here as well
    age_length <- function (highs, lows, L_a, CVlen)
    {
        lbprobs <- function(mnl, sdl) return(pnorm(highs, mnl, sdl) -
                                                 pnorm(lows, mnl, sdl))
        vlprobs <- Vectorize(lbprobs, vectorize.args = c("mnl",
                                                         "sdl"))
        plba <- t(vlprobs(L_a, L_a * CVlen))
        plba <- plba/rowSums(plba)
        return(plba)
    }
    
    # reading in the age length data (CSV)
    uploaded_data <- reactive({
        
        
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext %in% c("csv"), "Please upload a csv file"  |> i18n$t()))
        
        file$datapath |> 
            read_csv()
        
    })
    
    output$select_age_column_ui <- renderUI({
        
        uploaded_data_colnames <- 
            uploaded_data() %>% 
            colnames()
        
        selectInput("select_age_column",
                    label = NULL,
                    choices = c("Select age column:" |> i18n$t(), 
                                uploaded_data_colnames)
        )
        
    })
    
    
    output$select_length_column_ui <- renderUI({
        
        uploaded_data_colnames <- 
            uploaded_data() %>% 
            colnames()
        
        selectInput("select_length_column",
                    label = NULL,
                    choices = c("Select length column:" |> i18n$t(), 
                                uploaded_data_colnames)
        )
        
    })
    
    output$vb_params_table <- NULL
    
    observeEvent(input$calc_VB_pars,{
        
        age_colname <- sym(input$select_age_column)
        length_colname <- sym(input$select_length_column)
        
        output$vb_params_table <- renderTable({
            
            data_new <- 
                uploaded_data() |> 
                rename(length := !!length_colname,
                       age := !!age_colname)
            
            typical <- 
                FSA::vbStarts(length~age,
                         data = data_new)
            
            vb_func <- length~Linf*(1-exp(-K*(age-t0)))
            
            nls_vb_fit <- 
                nls(formula = vb_func,
                    data = data_new,
                    start = typical)
            
            nls_vb_fit |> 
                broom::tidy()
            
        })
        
    })
    
    
    plot_data <- reactive({
        
        
        MaxAge <- input$vb_maxage
        ### select Von Bertalanfy growth parameters
        vbk <- input$vb_k
        ### select Linf or the asymptotic length
        Linf <- input$vb_linf
        ### select the theoretical age when size is 0. This is only used for fit the curve and usually should be negative. You can explore different options
        t0 <- input$vb_t0
        ### select coefficient of variation in length at age. This information is NOT used in the CPUE based methods, but is important for size based methods. Usually this value is assumed to be around 10%
        CVlen <- input$vb_cvlen
        
        
        
        ## Various vectors that should be created based on this selection
        ##
        ages <- c(0:MaxAge)
        l_a <- vB(c(Linf, vbk, t0), ages)
        
        ## needed for the age-length function
        highs <- c(1:(Linf+30))
        lows <- c(0:(Linf+29))
        plba <- age_length(highs, lows, l_a, CVlen)
        
        
        # and now plots. Feel free to recode this part and make plots look nicer if you like
        # plot number 1 - length - age curve based
        
        # plot(ages, l_a, xlab = "Amžius, metais", ylab = "Ilgis, cm", pch = 19, main = "Augimo kreivė, remiantis Von Bertalanffy parametrais")
        
        
        
        
        
        tibble(ages = ages,
               length_a = l_a) 
    })
    
    vb_plot_1 <- reactive({
        
        
        lab1 <- "Age (years)" |> i18n$t() %>% rlang::sym() %>% as.character()
        lab2 <- "Length (cm)"  |> i18n$t() %>% rlang::sym()%>% as.character()
        
        
        plot_data() %>%
            ggplot(aes(x = ages,
                       y = length_a)) +
            geom_path(linewidth = 1.5, col = "darkblue") +
            xlab(lab1) +
            ylab(lab2) +
            theme_bw(24) +
            theme(plot.title = element_text(hjust = 0.5))+
            ylim(0, input$vb_linf*1.1)
    })
    
    plot.dat <- reactiveValues(layer = NULL)
    vb_line <- reactive({
        geom_path(aes(x = ages,
                      y = length_a),
                  linewidth = 1.5, 
                  col = "darkblue", 
                  data = plot_data())
    })
    
    # observe({
    #     print("render")
    #     output$vb_plot1 <- renderPlot({ plot_data() + 
    #             geom_point(aes(x = !!input$select_age_column,
    #                                                                  y = !!input$select_length_column),
    #                                                              size = 4,
    #                                                              col = "grey",
    #                                                              data = uploaded_data())})
    # })
    
    
    output$vb_plot1 <- renderPlot({ vb_plot_1() + plot.dat$layer + vb_line() })
    
    observeEvent(input$add_points,{
        
        age_colname <- sym(input$select_age_column)
        length_colname <- sym(input$select_length_column)
        # Calculate standard deviation
        if(input$select_age_column != "Select age column:"  |> i18n$t() &
           input$select_length_column != "Select length column:"  |> i18n$t()){
            plot.dat$layer <- geom_point(aes(x = !!age_colname,
                                             y = !!length_colname),
                                         size = 4,
                                         col = "grey",
                                         data = uploaded_data())
            
            
        }
        
    })
    
    
    output$vb_plot2 <- renderPlot({
        
        MaxAge <- input$vb_maxage
        ### select Von Bertalanfy growth parameters
        vbk <- input$vb_k
        ### select Linf or the asymptotic length
        Linf <- input$vb_linf
        ### select the theoretical age when size is 0.
        ###  This is only used for fit the curve and usually should be negative. You can explore different options
        t0 <- input$vb_t0
        ### select coefficient of variation in length at age. 
        ### This information is NOT used in the CPUE based methods, but is important for size based methods. Usually this value is assumed to be around 10%
        CVlen <- input$vb_cvlen
        
        
        ## Various vectors that should be created based on this selection
        ##
        ages <- c(1:MaxAge)
        l_a <- vB(c(Linf, vbk, t0), ages)
        
        ## needed for the age-length function
        highs <- c(1:(Linf+30))
        lows <- c(0:(Linf+29))
        plba <- age_length(highs, lows, l_a, CVlen)
        
        lab1 <- "Length (cm)"  |> i18n$t() %>% rlang::sym() %>% as.character()
        lab2 <- "Frequency or Probability"  |> i18n$t() %>% rlang::sym()%>% as.character()
        
        plba %>%
            t() %>%
            as_tibble() %>%
            select(-V1) %>%
            mutate(x = row_number()) %>%
            pivot_longer(cols = c(everything(), -x)) %>%
            mutate(age_class = as.numeric(str_extract(name, "(\\d)+"))) %>%
            ggplot(aes(x = x, y = value, col = age_class, group = age_class)) +
            geom_line(size = 1.5) +
            scale_colour_gradient(low = "darkblue", high = "pink") +
            theme_bw(24) +
            theme(legend.position = "none") +
            xlab(lab1) +
            ylab(lab2) +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    
    
}

shinyApp(
    ui = my_ui,
    
    server = my_server
)