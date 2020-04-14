

library(shiny)

library(igraph)
library(visNetwork)
library(purrr)
library(ggpubr)
library(tidyverse)
library(janitor)
library(readxl)
library(magrittr)
library(stringr)
library(dplyr)
library(gt)
library(RColorBrewer)
library(ggplot2)

# UI for application 
# use plotOutput function to plot the graph
# I named my image "plot" so that is what I put in the function call

ui <- fluidPage(
    
    navbarPage(tags$b("Social Connectedness in the Class of 2023"),
               
               tabPanel("Demographics",
                        
                        h2(tags$b("Total Sample Size")),
                        
                        p("A total of 386 first-year students answered our survey. 
                          Therefore, we had a 23.39% response rate from the 
                          Class of 2023 (which has a total matriculation of 1650)."), br(),
                        
               ),
               
               tabPanel("Gender",
                        
                        h2(tags$b("Gender Identity Breakdown")),
                        
                        p("58.03% of respondents were female. 41.45% of respondents 
                          were male 0.26% of respondents were genderqueer. 0% of 
                          respondents preferred not to share their gender."), br(),
                        
                        plotOutput("gender_pie", width = 500, height = 500),
               ),
               tabPanel("Dorm",
                        
                        h2(tags$b("Dorm Breakdown")),
                        
                        plotOutput("dorm_stats", width = 500, height = 500)
               ),
               tabPanel("Analyzing the Data",
                        
                        h2(tags$b("Satisfaction of Harvard’s social culture based on whether
                           or not they were listed among other respondents’ closest 4 friends")),
                        
                        # p("In our survey, we asked respondents to list 4 first-years 
                        #   they felt closest to. We also asked each respondent about 
                        #   how satisfied they were with their social connections 
                        #   (Very Dissatisfied, Dissatisfied, Neutral, Satisfied, or Very Satisfied)."), br(),
                        # 
                        # p("To analyze this data, we counted how many times each 
                        #   respondent’s name appeared in other respondents’ top 4 
                        #   closest friends lists. We compared this to each respondent’s 
                        #   satisfaction level"), br(),
                        # 
                        # plotOutput("compare_satisfaction", width = 500, height = 500),
                        
                        p("The horizontal line the graph indicates the mean satisfaction 
                          level across all first-years, which was 0.7797927. 
                          Since the satisfaction levels were scaled as a 0 if neutral 
                          and a 1 if satisfied, our respondents, on average, reported 
                          somewhere between neutral and satisfied (leaning satisfied) 
                          regarding their social connections."), br(),
                        
                        p("Respondents who appeared in other respondents’ top 4 
                        closest friends lists reported a higher level of satisfaction 
                        than respondents who did not appear. In fact, respondents 
                        who did not appear at all in other respondents’ top 4 
                        closest friends lists reported a below average level of 
                        satisfaction, while respondents who did appear reported 
                        an above average level of satisfaction."), br(),
                        
                        plotOutput("satisfaction_scatter_plot", width = 500, height = 500),
                        
                        p("As seen above, the R value is 0.89, which demonstrates 
                        a very strong correlation between satisfaction score 
                        and number of appearances."), br(),
                        
                        h2(tags$b("Do most “socially connected” students appear the most 
                          frequently in top 4 friend lists?")),
                        
                        p("In our survey, we asked respondents to name the student 
                        they perceive to be the “most socially connected” in the 
                        Class of 2023. To create the graph below, we compiled the 
                        \top 10 “most socially connected” students. Then, we counted 
                        how many times those students appeared in other 
                        respondents’ top 4 friend lists."), br(),
                        
                        plotOutput("top_socially_connected_appearance_in_top4", width = 500, height = 500),
                        
                        
                        # plotOutput("helen_plot", width = 500, height = 500),
                        
                        # plotOutput("street_encounter", width = 500, height = 500),
                        
                        plotOutput("social_num", width = 500, height = 500),
                        
                        h3(tags$b("Connections between Athletics and Pre-Orientation")),
                        
                        visNetworkOutput("mark_plot",  width = 500, height = 500)
               )
    )
    
)

# Define server logic
# I store the file in filename using normalizePath
# I named my image "plot" indicated by output$plot
# I used the renderImage function since I wanted to render the plot that I saved
# I do this by creating a list that specifies the image
# The filename variable is stored in source, the file type is an image
# The image's original dimensions are 2099x1499, so I scaled it down by half and rounded

server <- function(input, output) {
    
    output$gender_pie <- renderPlot({
        gender_group <- read_csv("data/gender_group.csv")
        gender_group %>%
            ggplot(aes(x = "", y = n, fill = gender_id)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            theme_void() +
            labs(
                title = "Respondent Sample by Gender"
            )
    })
    
    
    output$dorm_stats <- renderPlot({
        dorm_group <- read_csv("data/dorm_group.csv")
        
        dorm_group %>%
            ggplot(aes(y = n, x = dorm, fill = dorm)) +
            geom_col() + 
            geom_text(aes(label = paste0(perc_dorm, "%")), size = 4) +
            coord_flip() + 
            labs(
                x = "",
                y = "Number of Respondents",
                title = "Respondent Sample by Dorm"
            ) + 
            theme(legend.position = "none")
        
    })
    
    # commented out for now
    
    # output$compare_satisfaction <- renderPlot({
    #     freshmen <- read_excel("Harvard Freshmen Social Connections Survey (Responses).xlsx") %>%
    #         clean_names()
    #     compare_satisfaction <- read_csv("data/compare_satisfaction.csv")
    #     freshmen_satisfaction <- freshmen %>%
    #         nest(top4 = c(know_best_1, know_best_2,
    #                       know_best_3, know_best_4)) %>%
    #         select(name, satisfaction, top4) %>%
    #         mutate(satisfaction_lvl = case_when(satisfaction == "Very Satisfied" ~ 2,
    #                                             satisfaction == "Satisfied" ~ 1,
    #                                             satisfaction == "Neutral" ~ 0,
    #                                             satisfaction == "Dissatisfied" ~ -1,
    #                                             satisfaction == "Very Dissatisfied" ~ -2)) 
    #     
    #     all_freshmen_satisfaction_mean <- freshmen_satisfaction %>%
    #         summarize(mean = mean(satisfaction_lvl)) %>%
    #         pull(mean)
    #     
    #     compare_satisfaction %>%
    #         ggplot(aes(x = appear, y = mean_satis, fill = appear)) +
    #         geom_bar(stat = "identity") + 
    #         guides(fill=FALSE) +
    #         scale_x_discrete(labels = c("No", "Yes")) +
    #         labs(
    #             x = "Did the respondent's name appear in other respondents' top 4 friends lists?",
    #             y = "Mean Satisfaction Score",
    #             title = "Comparing mean satisfaction scores of respondents",
    #             subtitle = "Respondents who appeared more frequently were more satisfied",
    #             caption = "Very Dissatisfied = -2, Dissatisfied = -1, Neutral = 0, Satisfied = 1, Very Satisfied = 2"
    #         ) +
    #         geom_hline(yintercept = all_freshmen_satisfaction_mean) +
    #         theme_classic()
    # })
    
    output$satisfaction_scatter_plot <- renderPlot({
        satisfaction_scatter_tbl <- read_csv("data/satisfaction_scatter_tbl.csv")
        
        satisfaction_scatter_tbl %>%
            ggplot(aes(x = appearances, y = mean)) +
            geom_point(position = "jitter") +
            geom_smooth(method='lm', formula= y~x) +
            stat_cor(label.x = 5, label.y = 4.7) +
            labs(
                x = "Number of appearances in top 4 friend lists",
                y = "Mean satisfaction level (1 = Very Dissatisfied, 5 = Very Satisfied)",
                title = "Number of appearances and satisfaction level of Harvard social culture",
                subtitle = "Respondents who appeared more frequently reported to be more satisfied",
                caption = "Very Dissatisfied = 1, Dissatisfied = 2, Neutral = 3, Satisfied = 4, Very Satisfied = 5"
            ) + 
            theme_classic()
    })
    
    output$top_socially_connected_appearance_in_top4 <- renderPlot({
        well_connected_top_appearances <- read_csv("data/well_connected_top_appearances.csv")
        
        well_connected_top_appearances %>%
            ggplot(aes(x = appearances_in_well_connected, y = appearances_in_top4)) +
            geom_jitter() +
            geom_smooth(method='lm', formula= y~x) +
            stat_cor(label.x = 5, label.y = 4.7) +
            labs(
                x = "Number of appearances in 'most socially connected' question",
                y = "Number of appearances in top 4 friend lists",
                title = "Appearances of 'most socially connected' people in top 4 friend lists",
                subtitle = "Most 'socially connected' does not strongly correlate with 'closeness' with the most people"
            )
    })
    
    # output$street_encounter <- renderPlot({
    #     freshmen <- read_excel("Harvard Freshmen Social Connections Survey (Responses).xlsx") %>%
    #         clean_names()
    #     ggplot(data = freshmen, aes(x = recognize_street)) + geom_bar(fill = "cornsilk1") + labs(
    #         x = "Number of fellow freshmen respondents would recognize if encountered on the street",
    #         title = "Street encounter recognition levels")
    # })
    
    output$social_num <- renderPlot({
        freshmen_mod <- read_csv("data/freshmen_mod.csv")
        
        ggplot(data = freshmen_mod, aes(x = recognize_street, y = satisfaction)) + geom_point(alpha = .2) + geom_jitter() + labs(
            x = "Number of fellow freshmen respondents would recognize if encountered on the street", y = "levels of satisfaction",
            title = "Relationship between satisfaction with social life and number of people they recognize")
    })
    
    
    
    # output$helen_plot <- renderPlot({
    #     survey_data <- read_csv("survey_data_3-31-20.csv") 
    #     library(RColorBrewer)
    #     color  <- brewer.pal(4, "Set3") 
    #     edges_full <- survey_data %>% 
    #         select(name, first, second, third, fourth) %>% 
    #         pivot_longer(cols = c(first, second, third, fourth), names_to = "degree", values_to = "endpoint") %>% 
    #         mutate(colors = case_when(
    #             degree == "first" ~ color[1],
    #             degree == "second" ~ color[2],
    #             degree == "third" ~ color[3],
    #             degree == "fourth" ~ color[4],
    #         ))
    #     
    #     edges <- edges_full %>% 
    #         select(name, endpoint)
    #     edges
    #     nodes <- survey_data %>% 
    #         select(name)
    #     
    #     first <- survey_data %>% 
    #         select(first) %>% 
    #         rename("name" = "first")
    #     second <- survey_data %>% 
    #         select(second) %>% 
    #         rename("name" = "second")
    #     third <- survey_data %>% 
    #         select(third) %>% 
    #         rename("name" = "third")
    #     fourth <- survey_data %>% 
    #         select(fourth) %>% 
    #         rename("name" = "fourth")
    #     
    #     all_names <- full_join(first, full_join(second, full_join(third, fourth, by="name"), by="name"), by="name")
    #     
    #     nodes <- unique(full_join(nodes, all_names, by="name"))
    #     
    #     
    #     
    #     
    #     g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
    #     
    #     
    #     l <-layout_in_circle(g)
    #     l2 <- layout_on_sphere(g)
    #     
    #     
    #     #png("ms_6/helen_plot.png", 1800, 1800) 
    #     plot(g, vertex.label="", layout = l2, edge.width = 1, vertex.size=0.5, edge.color = edges_full$colors)
    #     title("Friend Network",cex.main=3,col.main="black")
    # })
    
    output$mark_plot <- renderVisNetwork({
        nodes2 <- read_csv("data/nodes2.csv")
        edges2 <- read_csv("data/edges2.csv")
        visNetwork(nodes2, edges2)
        
        
    })
    
    
}

# Run the application 

shinyApp(ui = ui, server = server)
