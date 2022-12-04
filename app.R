
library(shiny)
library(bs4Dash)
library(tidyverse)
library(MQMF)
library(bslib)
# library(shinythemes)
library(shinyWidgets)
# devtools::install_github("https://github.com/haddonm/MQMF")

my_ui <-
    dashboardPage(
        title = "von Bertalanffy",
        header = dashboardHeader(
            title = dashboardBrand(
                title = "von Bertalanffy",
                color = "primary",
                image = "https://avatars.githubusercontent.com/u/99745785?s=400&u=eeb3a46b56826f1511e2c387374759c4c5fd109e&v=4"
            )
        ),
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem("Overview",
                         tabName = "home_tab",
                         icon = icon("home")),
                menuItem("Parameters",
                         tabName = "von_bert",
                         icon = icon("sliders-h", lib = "font-awesome"))
            )
        ),
        body = dashboardBody(
            tabItems(
                # Home page tab
                tabItem(tabName = "home_tab",
                        h3("Title of the homepage"),
                        p("Info about the website goes here"),
                        p("Para 2"),
                        img(src='logos_english-removebg-preview.png', align = "center")
                        # fluidRow(
                        #     column(
                        #         width = 12,
                        #         
                        #     )
                        # )
                        
                ),
                
                tabItem(tabName = "von_bert",
                        h1("t5_head"),
                        br(),
                        fluidRow(
                            column(width = 4,
                                   # b1
                                   box(title = "t5_b1_title",
                                       sliderInput("vb_maxage",
                                                   "t5_b1_s1",
                                                   min = 1, max = 100, value = 20, step = 1),
                                       sliderInput("vb_k",
                                                   "t5_b1_s2",
                                                   min = 0.05, max = 0.95, value = 0.2, step = 0.05),
                                       sliderInput("vb_linf",
                                                   "t5_b1_s3",
                                                   min = 10, max = 500, value = 100, step = 10),
                                       sliderInput("vb_t0",
                                                   "t5_b1_s4",
                                                   min = -0.5, max = 0, value = -0.15, step = 0.05),
                                       sliderInput("vb_cvlen",
                                                   "t5_b1_s5",
                                                   min = 0, max = 0.5, value = 0.1, step = 0.01),
                                       width = 12
                                   )
                            ),
                            column(width = 8,
                                   # b2
                                   box(title = "t5_b2_title",
                                       plotOutput("vb_plot1"),
                                       width = 12,
                                       align="center"),
                                   br(),
                                   # b3
                                   box(title = "t5_b3_title",
                                       plotOutput("vb_plot2"),
                                       width = 12,
                                       align="center")
                            )
                        )
                )
            )
        )
    )

my_server <- function(input, output) {

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

        output$vb_plot1 <- renderPlot({

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
            ages <- c(1:MaxAge)
            l_a <- vB(c(Linf, vbk, t0), ages)

            ## needed for the age-length function
            highs <- c(1:(Linf+30))
            lows <- c(0:(Linf+29))
            plba <- age_length(highs, lows, l_a, CVlen)


            # and now plots. Feel free to recode this part and make plots look nicer if you like
            # plot number 1 - length - age curve based

            # plot(ages, l_a, xlab = "Amžius, metais", ylab = "Ilgis, cm", pch = 19, main = "Augimo kreivė, remiantis Von Bertalanffy parametrais")


            lab1 <- "p_lab17" %>% rlang::sym() %>% as.character()
            lab2 <- "p_lab18" %>% rlang::sym()%>% as.character()


            tibble(ages = ages,
                   length_a = l_a) %>%
                ggplot(aes(x = ages,
                           y = length_a)) +
                geom_point(size = 4, col = "darkblue") +
                geom_path(size = 1.5, col = "darkblue") +
                xlab(lab1) +
                ylab(lab2) +
                theme_bw(24) +
                theme(plot.title = element_text(hjust = 0.5))+
                ylim(0, input$vb_linf*1.1)




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

            # plot number 2: size distributions in age groups
            ## create a colour gradient - or do it any other way :)
            # lc <- colorRampPalette(c("blue", "orange"), alpha = TRUE)( length(ages) )

            # par(mfrow = c(1,1))
            # plot(plba[2,], type = 'l', xlab = "Ilgis, cm", ylab  = "Dažnumas/tikimybė", col = lc[1], lwd = 2, main = "Ilgio pasiskirstymas amžiaus grupėse")
            # for (i in 3:17) {
            #     points(plba[i,], type = 'l', col = lc[i], lwd = 2)
            # }

            lab1 <- "p_lab19" %>% rlang::sym() %>% as.character()
            lab2 <- "p_lab20" %>% rlang::sym()%>% as.character()

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
# ui <- 
#     
#     fluidPage(
#         # theme = shinytheme("sandstone"),
#         
#         navbarPage(
#             ##########
#             ## Page 1
#             ##########
#             "von Bertalanffy growth",
#             tabPanel(
#                 "Home",
#                 sidebarLayout(
#                     sidebarPanel =        
#                         
#                     sidebarPanel(
#                         # h3(
#                         #     "This is a simple shiny template with the navbar.",
#                         #     style = "padding-bottom: 20px"
#                         # )
#                     ),
#                     mainPanel = 
#                         mainPanel(
#                             h4(" dataset posted on 03.12.2020, by Simon De-Ville, Virginia Stovin, Christian Berretta, JOERG WERDIN, Simon Poë"),
#                             a(
#                                 href="https://figshare.shef.ac.uk/articles/dataset/Hadfield_Green_Roof_5-year_Dataset/11876736", 
#                                 "Click here for data!"
#                             ),
#                             h5("Data collected as part of the EU funded 'Collaborative research and development of green roof system technology' project from the Sheffield, UK, green roof testbeds."),
#                             h5("Data includes 5 years of:"),
#                             tags$li("Rainfall data (1-minute resolution)"),
#                             tags$li("Green roof runoff data for 9 roof configurations (1-minute resolution)"),
#                             tags$li("Soil moisture content at 3 depths for 4 roof configurations (5-minute resolution)"),
#                             tags$li("Climate data sufficient to calculate FAO-56 Penman-Monteith (1-hour resolution)"),
#                             h5("Due to difficulties in monitoring testbed runoff, there are occasions where runoff data is considered invalid. A separate data-file indicates individual storm events where runoff data is considered to be valid."),
#                             a(href="https://github.com/yld-weng/hadfield-green-roof", "Click here for the GitHub repo!")
#                         )
#                 )
#             ),
#             ##########
#             ## Page 2
#             ##########
#             tabPanel(
#                 "Climate Data Table",
#                 sidebarLayout(
#                     sidebarPanel(
#                         dateRangeInput(
#                             'dateRange',
#                             label = paste('Date range'),
#                             start = "2011-03-01", end = "2011-12-31",
#                             min = "2011-03-01", max = "2016-02-28",
#                             separator = " to ", format = "dd/mm/yyyy",
#                             startview = 'month', weekstart = 1
#                         )
#                     ),
#                     mainPanel(
#                         div(DT::dataTableOutput(outputId = "dataTable"))
#                     )
#                 )
#             )
#         )
#     )
# 
# # # Application title
# # titlePanel("Von Bertalanffy"),
# # 
# # sidebarLayout(
# #     
# #     # Sidebar with a slider input
# #     sidebarPanel(
# #         sidebarMenu(
# #                         menuItem("Home",
# #                                  tabName = "home_tab",
# #                                  icon = icon("home")),
# #                         menuItem("von bert",
# #                                  tabName = "von_bert",
# #                                  icon = icon("th"))
# #                     )
# #     ),
# #     
# #     # Show a plot of the generated distribution
# #     mainPanel(
# #         plotOutput("distPlot")
# #     )
# # )
# # 
# # 
# # ----------------------------------------------------------
# # 
# # dashboardPage(
# #     dashboardHeader(title = "Von Bert"),
# #     dashboardSidebar(
# #         sidebarMenu(
# #             menuItem("Home", 
# #                      tabName = "home_tab", 
# #                      icon = icon("home")),
# #             menuItem("von bert",
# #                      tabName = "von_bert", 
# #                      icon = icon("th"))
# #         )
# #     ),
# #     
# #     dashboardBody(
# #         tabItems(
# #             # Home page tab
# #             tabItem(tabName = "home_tab",
# #                     "Info about the website goes here"
# #             ),
# #             
# #             tabItem(tabName = "von_bert",
# #                     h1("t5_head"),
# #                     br(),
# #                     fluidRow(
# #                         column(width = 4, 
# #                                # b1
# #                                box(title = "t5_b1_title",
# #                                    sliderInput("vb_maxage", 
# #                                                "t5_b1_s1",
# #                                                min = 1, max = 100, value = 20, step = 1),
# #                                    sliderInput("vb_k", 
# #                                                "t5_b1_s2",
# #                                                min = 0.05, max = 0.95, value = 0.2, step = 0.05),
# #                                    sliderInput("vb_linf", 
# #                                                "t5_b1_s3",
# #                                                min = 10, max = 500, value = 100, step = 10),
# #                                    sliderInput("vb_t0", 
# #                                                "t5_b1_s4",
# #                                                min = -0.5, max = 0, value = -0.15, step = 0.05),
# #                                    sliderInput("vb_cvlen", 
# #                                                "t5_b1_s5",
# #                                                min = 0, max = 0.5, value = 0.1, step = 0.01),
# #                                    width = 12
# #                                )
# #                         ),
# #                         column(width = 8, 
# #                                # b2
# #                                box(title = "t5_b2_title",
# #                                    plotOutput("vb_plot1"),
# #                                    width = 12,
# #                                    align="center"),
# #                                br(), 
# #                                # b3
# #                                box(title = "t5_b3_title",
# #                                    plotOutput("vb_plot2"),
# #                                    width = 12,
# #                                    align="center")
# #                         )
# #                     )
# #             )
# #         )
# #     )
# # )
# # )
# 
# 
# server <- function(input, output) {
#     
#     
#     
#     ### Two functions to be created 
#     ### This is function that is implemented in the MQMF package 
#     vB <- function (p, ages)
#     {
#         return(p[1] * (1 - exp(-p[2] * (ages - p[3]))))
#     }
#     
#     ### This is a function from another package that allows plotting length distributions, it should be implemented here as well 
#     age_length <- function (highs, lows, L_a, CVlen)
#     {
#         lbprobs <- function(mnl, sdl) return(pnorm(highs, mnl, sdl) -
#                                                  pnorm(lows, mnl, sdl))
#         vlprobs <- Vectorize(lbprobs, vectorize.args = c("mnl",
#                                                          "sdl"))
#         plba <- t(vlprobs(L_a, L_a * CVlen))
#         plba <- plba/rowSums(plba)
#         return(plba)
#     }
#     
#     output$vb_plot1 <- renderPlot({
#         
#         MaxAge <- input$vb_maxage
#         ### select Von Bertalanfy growth parameters 
#         vbk <- input$vb_k
#         ### select Linf or the asymptotic length 
#         Linf <- input$vb_linf
#         ### select the theoretical age when size is 0. This is only used for fit the curve and usually should be negative. You can explore different options 
#         t0 <- input$vb_t0
#         ### select coefficient of variation in length at age. This information is NOT used in the CPUE based methods, but is important for size based methods. Usually this value is assumed to be around 10%
#         CVlen <- input$vb_cvlen
#         
#         
#         
#         ## Various vectors that should be created based on this selection 
#         ##
#         ages <- c(1:MaxAge)
#         l_a <- vB(c(Linf, vbk, t0), ages)
#         
#         ## needed for the age-length function 
#         highs <- c(1:(Linf+30))
#         lows <- c(0:(Linf+29))
#         plba <- age_length(highs, lows, l_a, CVlen)
#         
#         
#         # and now plots. Feel free to recode this part and make plots look nicer if you like 
#         # plot number 1 - length - age curve based 
#         
#         # plot(ages, l_a, xlab = "Amžius, metais", ylab = "Ilgis, cm", pch = 19, main = "Augimo kreivė, remiantis Von Bertalanffy parametrais")
#         
#         
#         lab1 <- "p_lab17" %>% rlang::sym() %>% as.character()
#         lab2 <- "p_lab18" %>% rlang::sym()%>% as.character()
#         
#         
#         tibble(ages = ages, 
#                length_a = l_a) %>% 
#             ggplot(aes(x = ages,
#                        y = length_a)) +
#             geom_point(size = 4, col = "darkblue") +
#             geom_path(size = 1.5, col = "darkblue") +
#             xlab(lab1) +
#             ylab(lab2) +
#             theme_bw(24) +
#             theme(plot.title = element_text(hjust = 0.5))+
#             ylim(0, input$vb_linf*1.1)
#         
#         
#         
#         
#     })
#     
#     output$vb_plot2 <- renderPlot({
#         
#         MaxAge <- input$vb_maxage
#         ### select Von Bertalanfy growth parameters 
#         vbk <- input$vb_k
#         ### select Linf or the asymptotic length 
#         Linf <- input$vb_linf
#         ### select the theoretical age when size is 0. This is only used for fit the curve and usually should be negative. You can explore different options 
#         t0 <- input$vb_t0
#         ### select coefficient of variation in length at age. This information is NOT used in the CPUE based methods, but is important for size based methods. Usually this value is assumed to be around 10%
#         CVlen <- input$vb_cvlen
#         
#         
#         ## Various vectors that should be created based on this selection 
#         ##
#         ages <- c(1:MaxAge)
#         l_a <- vB(c(Linf, vbk, t0), ages)
#         
#         ## needed for the age-length function 
#         highs <- c(1:(Linf+30))
#         lows <- c(0:(Linf+29))
#         plba <- age_length(highs, lows, l_a, CVlen)
#         
#         # plot number 2: size distributions in age groups 
#         ## create a colour gradient - or do it any other way :)
#         # lc <- colorRampPalette(c("blue", "orange"), alpha = TRUE)( length(ages) )
#         
#         # par(mfrow = c(1,1))
#         # plot(plba[2,], type = 'l', xlab = "Ilgis, cm", ylab  = "Dažnumas/tikimybė", col = lc[1], lwd = 2, main = "Ilgio pasiskirstymas amžiaus grupėse")
#         # for (i in 3:17) {
#         #     points(plba[i,], type = 'l', col = lc[i], lwd = 2)
#         # }
#         
#         lab1 <- "p_lab19" %>% rlang::sym() %>% as.character()
#         lab2 <- "p_lab20" %>% rlang::sym()%>% as.character()
#         
#         plba %>% 
#             t() %>% 
#             as_tibble() %>% 
#             select(-V1) %>% 
#             mutate(x = row_number()) %>% 
#             pivot_longer(cols = c(everything(), -x)) %>% 
#             mutate(age_class = as.numeric(str_extract(name, "(\\d)+"))) %>% 
#             ggplot(aes(x = x, y = value, col = age_class, group = age_class)) +
#             geom_line(size = 1.5) +
#             scale_colour_gradient(low = "darkblue", high = "pink") +
#             theme_bw(24) +
#             theme(legend.position = "none") +
#             xlab(lab1) +
#             ylab(lab2) +
#             theme(plot.title = element_text(hjust = 0.5))
#         
#     })
#     
#     
# }
# 
# 
# shinyApp(ui, server)