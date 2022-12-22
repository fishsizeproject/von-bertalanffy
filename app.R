
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


my_ui <-
    dashboardPage(
        header = dashboardHeader(
            title = dashboardBrand(
                title = "von Bertalanffy" |> i18n$t(),
                color = "primary",
                image = "https://avatars.githubusercontent.com/u/99745785?s=400&u=eeb3a46b56826f1511e2c387374759c4c5fd109e&v=4"
            ),
            usei18n(i18n)
        ),
        sidebar = dashboardSidebar(
            usei18n(i18n),
            sidebarMenu(
                # style = "height: 90vh; overflow-y: auto;", 
                menuItem(
                    text = "About the model"  |> i18n$t(), 
                    tabName = "aboutvbms_tab", 
                    icon = icon("home")
                ),
                menuItem("Parameters" |> i18n$t(),
                         tabName = "von_bert",
                         icon = icon("sliders-h", lib = "font-awesome")),
                menuItem(
                    text = "More information" |> i18n$t(),
                    tabName = "home_tab",
                    icon = icon("info-circle")
                )
            ),
            br(),
            selectInput("lang_select",
                        label = NULL,
                        choices = list("English" = "en",
                                       "Lietuvių" = "lt"),
                        selected = "en")
        ),
        body = dashboardBody(
            tags$head(tags$style("p {font-size: 18px}")),
            usei18n(i18n),
            tabItems(
                # Home page tab
                tabItem(
                    tabName = "aboutvbms_tab",
                    fluidRow(
                        div(
                            # width = 12,
                            h1("About von Bertalanffy growth models" |> i18n$t()),
                            textOutput("res"),
                            # title = "About von Bertalanffy growth models" |> i18n$t(),
                            # First paragraph
                            p("One of the most common ways to model fish growth is by using the von Bertalanffy growth curve, which describes individual fish growth as an asymtotic growth function with an annual instantaneous growth rate of K and asymptotic body length of L\u221E."  |> i18n$t()),
                            br(), # line break between paragraphs
                            div(img(src = "TSR.png", 
                                    width = "50%"), 
                                align = "center"),
                            br(),
                            # Second paragraph
                            p("Using this app you can explore how the model works, upload your own dataset and find best parameters (the dataset is not stored anywhere and will be cleared from the memory as soon as you finish your work) and also explore how variation in growth means that with increasing ages it becomes increasingly difficult to separate age groups from length information alone."  |> i18n$t()),
                            br(), 
                            div(img(src = "Cyprinuscarpio_sm.png", 
                                    width = "50%"), 
                                align = "center"),
                            br(),
                            # Third paragraph
                            #div("PARAGRAPH3"  |> i18n$t()),
                            #br(),
                            p("To see our other models"|> i18n$t(), 
                                a(href = "https://fishsizeproject.github.io/models/", "click here" |> i18n$t()),
                                "or to our project website"|> i18n$t(),
                                a(href = "https://sif.lt", "click here" |> i18n$t()), ".")
                        )
                    ),
                ),
                
                
                
                tabItem(tabName = "von_bert",
                        h1("von Bertalanffy growth model" |> i18n$t()),
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
                                                   label =  HTML(paste0("Maximum length" |> i18n$t(), " (L", tags$sub("\u221E"), ')')),
                                                   min = 10, 
                                                   max = 100, 
                                                   value = 20, 
                                                   step = 1, 
                                                   post = "cm"),
                                       sliderInput(inputId = "vb_t0",
                                                   label = HTML(paste0("Age when length = 0cm"  |> i18n$t(), " (t", tags$sub("0"), ')')),
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
                                   box(title = "Choose length-age data source" |> i18n$t(),
                                       width = 12, 
                                       div(uiOutput(outputId = "choose_data_ui"), 
                                           align = "center"),
                                       uiOutput(outputId = "data_cols_select")
                                       # uiOutput(outputId = "select_length_column_ui"),
                                       # actionButton(inputId = "add_points",
                                       #              label = "Add data points" |> i18n$t())
                                   ),
                                   box(title = "Calculate parameters" |> i18n$t(), 
                                       width = 12, 
                                       tableOutput(outputId = "vb_params_table"), 
                                       actionButton(inputId = "move_sliders",
                                                    label = "Adjust parameter sliders" |> i18n$t()),
                                       align = "center")
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
                ),
                
                
                tabItem(
                    tabName = "home_tab",
                    h1("Research for sustainable inland fisheries"  |> i18n$t()),
                    p("The development of this app was done by Nature Research Centre (Lithuania) staff during the project “Advanced models, citizen science and big data for sustainable food production and ecological services of inland aquatic ecosystems”, funded from European Regional Development Fund (project No 01.2.2-LMT-K-718-02-0006) under grant agreement with the Research Council of Lithuania (LMTLT)."  |> i18n$t()),
                    p("You can learn more about our other models, research, courses and engagement events on our website https://sif.lt"  |> i18n$t()),
                    img(src='logos_english-removebg-preview.png', align = "center")
                    
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
    
    observeEvent(input$lang_select, {
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
    
    r <- reactiveValues(
        input_dat = {
            read_csv("additional_info/Pikeperch age CL.csv",
                     show_col_types = FALSE) 
        }, 
        vb_tab = {
            data_new <- 
                read_csv("additional_info/Pikeperch age CL.csv",
                         show_col_types = FALSE)  |> 
                rename(length = Length,
                       age = Age)
            
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
        }, 
        age_col = "Age",
        length_col = "Length"
    )
    
    read_in_dat <- reactive({
        
        req(input$file1)
        
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext %in% c("csv"), "Please upload a csv file"  |> i18n$t()))
        
        file$datapath |> 
            read_csv(show_col_types = FALSE)
    })
    # 
    # 
    # observeEvent(eventExpr = input$update_data, 
    #              handlerExpr = {
    #                  
    #                  r$input_dat <- 
    #                      if(req(input$choose_data) == choice2()){
    #                          
    #                          read_in_dat()
    #                          
    #                      } else {
    #                          read_csv("additional_info/Pikeperch age CL.csv",
    #                                   show_col_types = FALSE) 
    #                      }
    #                  
    #                  r$age_col <- 
    #                      if(req(input$choose_data) == choice2()){
    #                          
    #                          input$select_age_column
    #                          
    #                      } else {
    #                          "Age"
    #                      }
    #                  
    #                  r$length_col <- 
    #                      if(req(input$choose_data) == choice2()){
    #                          
    #                          input$select_length_column
    #                          
    #                      } else {
    #                          "Length"
    #                      }
    #                  
    #              }
    # )
    
    
    observeEvent(eventExpr = input$choose_data, 
                 handlerExpr = {
                     
                     if(req(input$choose_data) == choice1()){
                         
                         r$input_dat <- 
                             read_csv("additional_info/Pikeperch age CL.csv",
                                      show_col_types = FALSE)
                         
                         r$age_col <- "Age"
                         r$length_col <-"Length"
                         
                     }
                     
                 })
    
    
    observeEvent(eventExpr = input$update_data, 
                 handlerExpr = {
                     
                     r$input_dat <- 
                         if(req(input$choose_data) == choice2()){
                             
                             read_in_dat()
                             
                         } else {
                             read_csv("additional_info/Pikeperch age CL.csv",
                                      show_col_types = FALSE) 
                         }
                     
                     r$age_col <- 
                         if(req(input$choose_data) == choice2()){
                             
                             input$select_age_column
                             
                         } else {
                             "Age"
                         }
                     
                     r$length_col <- 
                         if(req(input$choose_data) == choice2()){
                             
                             input$select_length_column
                             
                         } else {
                             "Length"
                         }
                     
                 }
    )
    
    choice1 <- reactive({"Use example data" |> i18n$t() %>% rlang::sym() %>% as.character()})
    choice2 <- reactive({"Upload own data"  |> i18n$t() %>% rlang::sym() %>% as.character()})
    
    
    output$choose_data_ui <- renderUI({
        
        radioGroupButtons(
            inputId = "choose_data",
            label = NULL, 
            choices = c(choice1(), 
                        choice2()),
            status = "primary"
        )
    })
    
    
    output$example_data_head <- renderTable({
        read_csv("additional_info/Pikeperch age CL.csv",
                 show_col_types = FALSE)  |> 
            mutate(Year = as.integer(Year), 
                   Age = as.integer(Age)) |> 
            head() 
    }) 
    
    
    
    output$dropdown_age <- renderUI({
        selectInput("select_age_column",
                    label = NULL,
                    choices = c("Select age column:" |> i18n$t(), 
                                colnames(read_in_dat())))
    })
    
    output$dropdown_length <- renderUI({
        selectInput("select_length_column",
                    label = NULL,
                    choices = c("Select length column:" |> i18n$t(), 
                                colnames(read_in_dat())))
    })
    
    make_upload <- reactive({
        
        if (req(input$choose_data) == choice2()) {
            
            div(
                fileInput("file1", 
                          "Choose CSV File" |> i18n$t(),
                          multiple = FALSE,
                          buttonLabel = "Browse"|> i18n$t(),
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                uiOutput("dropdown_age"),
                uiOutput("dropdown_length"),
                div(
                    actionButton(inputId = "update_data",
                                 label = "Upload data" |> i18n$t()), 
                    align = "center")
            )
            
        } else {
            
            div(
                "Example dataset (first six rows shown only)" |> i18n$t(),
                tableOutput(outputId = "example_data_head"),
                align = "center"
            )
            
            
        } 
        
        
        
    })
    
    output$data_cols_select <- renderUI({
        make_upload()
    })
    
    
    calc_best_vb_params <- eventReactive( r$input_dat, {
        
        
        age_colname <- sym(r$age_col)
        length_colname <-  sym(r$length_col)
        
        data_new <- 
            r$input_dat |> 
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
    
    output$vb_params_table <- renderTable({
        
        col1_name <- "Estimate" |> i18n$t() %>% rlang::sym() %>% as.character()
        col2_name <- "Standard error"  |> i18n$t() %>% rlang::sym()%>% as.character()
        col3_name <- "P-value"  |> i18n$t() %>% rlang::sym()%>% as.character()
        
        calc_best_vb_params() |> 
            # mutate(term = c("Carrying capacity (K)" |> i18n$t(), 
            #                 "MSY proportion of K" |> i18n$t(), 
            #                 "Stock biomass to produce MSY" |> i18n$t())) |> 
            select(!!col1_name := "estimate",
                   !!col2_name := "std.error",
                   !!col3_name := "p.value") |> 
            mutate(my_rownames = c("L<sub>\u221E</sub>", 
                                   "K",
                                   "t<sub>0</sub>")) |> 
            column_to_rownames(var = "my_rownames")
        
    }, 
    rownames = T, 
    sanitize.text.function = function(x) x)
    
    observeEvent(input$move_sliders,{
        
        val_tab <- calc_best_vb_params()
        
        updateSliderInput(session, 
                          inputId = "vb_linf", 
                          value = val_tab$estimate[1])
        
        updateSliderInput(session, 
                          inputId = "vb_k", 
                          value = val_tab$estimate[2])
        
        
        updateSliderInput(session, 
                          inputId = "vb_t0", 
                          value = val_tab$estimate[3])
        
        
    })
    
    
    plot_data <- reactive({
        
        MaxAge <- input$vb_maxage
        ### select von Bertalanfy growth parameters
        vbk <- input$vb_k
        ### select Linf or the asymptotic length
        Linf <- input$vb_linf
        ### select the theoretical age when size is 0.
        ###  This is only used for fit the curve and usually should be negative. 
        ###  You can explore different options
        t0 <- input$vb_t0
        ### select coefficient of variation in length at age. 
        ### This information is NOT used in the CPUE based methods, 
        ### but is important for size based methods. Usually this value is assumed to be around 10%
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
        
        # plot(ages, l_a, 
        # xlab = "Amžius, metais", 
        # ylab = "Ilgis, cm", 
        # pch = 19, 
        # main = "Augimo kreivė, remiantis von Bertalanffy parametrais")
        
        tibble(ages = ages,
               length_a = l_a) 
    })
    
    plot1_background <- reactive({
        
        lab1 <- "Age (years)" |> i18n$t() %>% rlang::sym() %>% as.character()
        lab2 <- "Length (cm)"  |> i18n$t() %>% rlang::sym() %>% as.character()
        
        plot_data() %>%
            ggplot(aes(x = ages,
                       y = length_a)) +
            geom_path(linewidth = 1.5, 
                      col = "darkblue") +
            xlab(lab1) +
            ylab(lab2) +
            theme_bw(24) +
            theme(plot.title = element_text(hjust = 0.5)) +
            ylim(0, input$vb_linf*1.1)
        
    })
    
    plot1_line <- reactive({
        
        geom_path(aes(x = ages,
                      y = length_a),
                  linewidth = 1.5, 
                  col = "darkblue", 
                  data = plot_data())
    })
    
    
    vb_plot1_react <- reactive({
        
        r$input_dat
        age_colname <- sym(r$age_col)
        length_colname <-  sym(r$length_col)
        
        plot1_background() + 
            geom_point(aes(x = !!age_colname,
                           y = !!length_colname),
                       size = 4,
                       col = "grey",
                       data = r$input_dat) +
            plot1_line()  
    })
    
    
    output$vb_plot1 <- renderPlot({ 
        
        vb_plot1_react()
        
    })
    
    vb_plot2_react <-  reactive({
        
        r$input_dat
        
        MaxAge <- input$vb_maxage
        ### select von Bertalanfy growth parameters
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
        
        
        
        age_colname <- sym(r$age_col)
        length_colname <-  sym(r$length_col)
        
        
        plba %>%
            t() %>%
            as_tibble() %>%
            select(-V1) %>%
            mutate(x = row_number()) %>%
            pivot_longer(cols = c(everything(), -x)) %>%
            mutate(age_class = as.numeric(str_extract(name, "(\\d)+"))) %>%
            ggplot(aes(x = x, y = value, col = age_class, group = age_class)) +
            geom_histogram(aes(x = !!length_colname, 
                               y = after_stat(density)), 
                           data = r$input_dat, 
                           inherit.aes = FALSE, 
                           alpha = 0.3, 
                           binwidth = 1, 
                           fill = "grey50",
                           col = "black") +
            geom_line(linewidth = 1.5) +
            scale_colour_gradient(low = "darkblue", high = "pink") +
            theme_bw(24) +
            theme(legend.position = "none") +
            xlab(lab1) +
            ylab(lab2) +
            theme(plot.title = element_text(hjust = 0.5))
        
    })
    
    
    output$vb_plot2 <- renderPlot({
        vb_plot2_react()
        
    })
    
    
    
}

shinyApp(
    ui = my_ui,
    server = my_server
)