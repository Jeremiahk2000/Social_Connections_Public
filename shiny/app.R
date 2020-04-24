


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
library(shinythemes)



# UI for application 
# use plotOutput function to plot the graph
# I named my image "plot" so that is what I put in the function call

ui <- bootstrapPage(theme = shinytheme("yeti"),
    navbarPage(tags$b("Social Connectedness in the Class of 2023"),

               tabPanel(align = "center", "About",
                        
                        h2(tags$b("Total Sample Size")),
                        
                        p("A total of 413 first-year students answered our survey. 
                          Therefore, we had a 25.03% response rate from the 
                          Class of 2023 (which has a total matriculation of 1650)."), br(),
                        
                        
                        visNetworkOutput("mark_plot", width = "100%", height = "1000px")
                        
               ),
               
               tabPanel("Demographics",
                        
                        h2(tags$b("Gender Identity Breakdown")),
                        
                        p("58.03% of respondents were female. 41.45% of respondents 
                          were male 0.26% of respondents were genderqueer. 0% of 
                          respondents preferred not to share their gender."), br(),
                        
                        img(src='1gender_ratio_all_responses.png', width = "50%"),
                        
                        h2(tags$b("Dorm Breakdown")),
                        
                        img(src="2dorm_ratio_all_responses.png", width = "50%"),
               ),
               tabPanel("Analyzing the Data",
                        
                        
                        p("We asked each respondent how satisfied they were with 
                        their social connections. Each respondent was given 5 options: 
                        Very Dissatisfied, Dissatisfied, Neutral, Satisfied, or Very Satisfied."), br(),
                        
                        p("When coded from -2 to 2 (-2 being Very Dissatisfied, 0 being Neutral, 
                          and 2 being Very Satisfied), the mean satisfaction score 
                          of all respondents was 0.7917676."), br(),
                    
                        
                        h2(tags$b("Satisfaction of Harvard’s social culture based on whether
                           or not they were listed among other respondents’ closest 4 friends")),
                        
                        p("In our survey, we asked respondents to list 4 first-years 
                           they felt closest to. We also asked each respondent about 
                           how satisfied they were with their social connections 
                           (Very Dissatisfied, Dissatisfied, Neutral, Satisfied, or Very Satisfied)."), br(),
                         
                        p("To analyze this data, we counted how many times each 
                           respondent’s name appeared in other respondents’ top 4 
                           closest friends lists. We compared this to each respondent’s 
                          satisfaction level"), br(),
                        
                        img(src="3mean_satisfaction_score_bar_plot.png", width = "50%"),
                        
                        p("First, we compared the satisfaction score mean of 
                          respondents who did appear in others' friend lists, 
                          and the satisfaction score mean of those who did not."), br(),
                        
                        p("The horizontal line the graph indicates the mean satisfaction 
                          level across all first-years, which was 0.7917676. 
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
                        
                        p("Then, we plotted number of appearances against mean 
                        satisfaction score on a scatter plot."), br(),
                        
                        img(src="4appearances_satisfaction_lvl.png", width = "50%"),
                        
                        p("The R value is not statistically significant (p > 0.05), 
                          indicating that there is not a very strong relationship 
                          between satisfaction level and number of individuals who 
                          consider them close friends. This observation suggests that 
                          satisfaction about social relationships may not originate from 
                          someone else's consideration of you as a close friend — perhaps 
                          individuals feel more socially satisfied as a result of 
                          other factors. Another way to interpret this absence of 
                          a finding is that individuals may be unaware of how 
                          close others consider them."), br(),
                        
                        h2(tags$b("Do students who know more first-years feel more satisfied?")),
                        
                        h3(tags$b("How many members of the Class of 2023 would 
                        you recognize on the street?")),
                        
                        p("We also asked respondents how many members of the 
                        2023 class they would recognize on the street. Below were our results:"), br(),
                        
                        img(src="5know_on_street_responses.png", width = "50%"),
                        
                        p("We then observed the mean satisfaction levels 
                        (measured on a scale from -2 to 2) of each group that 
                        stated similar street recognition levels. Below are our 
                        results, graphed with the overall average satisfaction 
                        level of our entire sample (the black horizontal line)."), br(),
                        
                        img(src="6know_on_street_satisfaction.png", width = "50%"),
                        
                        p("As can be observed, the respondents who could recognize 
                        1000+ first-years on the street reported higher satisfaction 
                        level means. The respondents who could recognize 0-50, 
                        on the other hand, reported lower than average satisfaction 
                        means. This result seems to suggest that the more 
                        fellow freshmen the respondent could recognize, the 
                        more satisfied they tended to feel about their social 
                        experience. This correlation is intuitive."), br(),
                        
                        h3(tags$b("How many members of the Class of 2023 would 
                                  you feel comfortable sitting down with at Annenberg?")),
                        
                        
                        p("We also asked respondents how many freshmen they 
                        would feel comfortable sitting down with at Annenberg. Below were our results:"), br(),
                        
                        img(src="7know_in_berg_responses.png", width = "50%"),
                        
                        p("We then observed the mean satisfaction levels 
                        (measured on a scale from -2 to 2) of each group that 
                        stated similar numbers of students they felt comfortable 
                        sitting next to in Annenberg. Below are our results, 
                        graphed with the overall average satisfaction level 
                        of our entire sample (the black horizontal line)."), br(),
                        
                        img(src="8know_in_berg_satisfaction.png", width = "50%"),
                        
                        p("In this question, students who answered differently 
                        did not demonstrate large differences in satisfaction 
                        level. Compared to the street recognition level question, 
                        mean satisfaction scores of Annenberg-comfortable 
                        groups differed less noticeably. This observation 
                        suggests that respondents base personal satisfaction 
                        more heavily on how many students they recognize, 
                        rather than how many students they feel personally close to."), br(),
                        
                        
                        h2(tags$b('Analyzing the "most socially" connected individuals?')),
                        
                        p('In our survey, we asked respondents to name the student 
                          they perceive to be the "most socially connected" in the Class of 2023.'), br(),
                        
                        
                        h3(tags$b('How many times did the "most socially connected" 
                                  students appear in top 4 friend lists??')),
                        
                        p('To create the graph below, we compiled the top 100 
                        "most socially connected" students. Then, we counted 
                        how many times those students appeared in other 
                        respondents top 4 friend lists.'), br(),
                        
                        
                        img(src="9social_connect_top4.png", width = "50%"),
                        
                        p('As seen in the graph above, the number of times 
                        someone was named "most socially connected" had a very 
                        weak positive correlation with the number of times that 
                        person was named a close friend. This observation is 
                        interesting — it indicates that individuals who are 
                        considered socially connected may not always be the 
                        "closest" to the most people. Having a general impression 
                        of someone as a socially connected person does not mean 
                        the most people considers that person a close friend.'), br(),
                        
                        
                        h3(tags$b('Comparing satisfaction levels of top 100 
                        "most socially connected" to top 100 number of appearances in friend lists')),
                        
                        
                        p("Next, we compared the mean satisfaction scores of the 
                        top 100 students who appeared the most in the socially 
                        connected column to the top 100 students who appeared 
                        the most in other respondents' friend lists."), br(),
                        
                        p('* The overall satisfaction score mean of our entire 
                        sample: 0.7917676.'), br(),
                        
                        p('* The satisfaction score mean of top 100 
                        "most socially connected students": 1.2083333.'), br(),
                    
                        p('* The satisfaction score mean of top 100 students with 
                        most appearances in friend lists: 1.04.'),br(),
                        
                        p("Students who are considered more socially connected 
                          and students who appeared most frequently in others' 
                          friend lists had higher-than-average mean satisfaction 
                          scores. Between the two populations, however, the 
                          difference was not significant."), br(),
                        
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
        # change shape, color, and size for each group
        
        visNetwork(nodes2, edges2) %>%
            visGroups(groupname = "Dorms", color = "darkblue", shape = "square", size = 65) %>%
            visGroups(groupname = "Pre-Orientation", color = "darkred", shape = "square", size = 45) %>%
            visGroups(groupname = "Sports", color = "darkgreen", shape = "square", size = 45) %>%
            
            # add functionality to highlight close connections when hovering over node
            
            visOptions(nodesIdSelection = list(enabled = TRUE,
                                               style = "margin-bottom: -30px; visibility: hidden"),
                       highlightNearest = list(enabled = T, degree = 2, hover = T),
                       selectedBy = "group") %>%
            
            # adjust physics to decrease load time
            
            visPhysics(
                solver = "forceAtlas2Based", 
                timestep = 0.5,
                minVelocity = 1,
                maxVelocity = 30,
                forceAtlas2Based = list(gravitationalConstant = -200, damping = 1),
                stabilization = list(iterations = 200, updateInterval = 10),
                adaptiveTimestep = TRUE) %>%
            
            # add legend for groups
            
            visLegend()
        
        
    })
    
    
}

# Run the application 

shinyApp(ui = ui, server = server)
