


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
library(infer)
library(broom)
library(magrittr)
library(readr)
library(gganimate)
library(ggthemes)
library(plyr)




# UI for application 
# use plotOutput function to plot the graph
# I named my image "plot" so that is what I put in the function call

ui <- bootstrapPage(theme = shinytheme("yeti"),
    navbarPage(tags$b("Social Connectedness in the Class of 2023"),
               
               tabPanel(align = "center", "The Survey",
                        h2(tags$b("About the survey")),
                        p("Data for our study was collected via survey. We sent the survey out to the entire class of 2023 and collected responses. Because there was presumably bias associated with who responded and who did not, we cross checked our responses with a random sample of 2023 students. The random sample was a random compilation of 10% of each freshman dorm. In order to protect students’ privacy, we assigned all students an ID number in place of their name."),
                        br(),
                        h2(tags$b("Common Demographics")),
                        plotOutput("dorm_plot"),
                        p("Because Canaday is the largest freshman dorm, it necessarily made up the largest portion of our sample."),
                        br(),
                        plotOutput("gender_plot"),
                        br(),
                        plotOutput("race_plot"),
                        br(),
                        plotOutput("gap_year_plot"),
                        br(),
                        plotOutput("int_plot"),
                        br(),
                        plotOutput("pre_plot"),
                        br(),
                        plotOutput("sports_plot"),
                        h2(tags$b("Survey Questions")),
                        p("1.) Have you taken a gap year?"),
                        br(),
                        p("2.) Which gender identity do you most identify with?"),
                        br(),
                        p("3.) Specify your race and/or ethnicity"),
                        br(),
                        p("4.) Are you classified as an international student?"),
               br(),
               p("5.) Which pre-orientation, if any, did you participate in?"),
               br(),
               p("6.) Are you involved with any sports teams on campus?"),
               br(),
               p("7.) List the four names of the people who you are closest friends with."),
               br(),
               p("8.) How many people would you recognize as a Harvard first-year if you saw them on the street?"),
               br(),
               p("9.) How many first-years do you know by name?"),
               br(),
               p("10.) How many people would you feel comfortable sitting down next to in Annenberg unprompted?"),
               br(),
               p("11.) Name the most socially connected person in the class of 2023."),
               br(),
               p("12.) How satisfied are you with your social connections with first-year students at Harvard?"),
               br(),
               p("If you would like to see our methodology about why we chose these specific questions and what our purpose behind each question was, please follow this link:"),
               strong(tags$a(href="https://docs.google.com/document/d/1vx0WUw2ExeIp8rgt1f23C0LbKV0dPIBXf3JDR6UZJmk/edit?usp=sharing", "Survey Methodology"))
               
               
                        ),
                        
                        
               tabPanel("The Social Web",
                        
                        h2(tags$b("Total Sample Size")),
                        
                        p("A total of 413 first-year students answered our survey. 
                          Therefore, we had a 25.03% response rate from the 
                          Class of 2023 (which has a total matriculation of 1650)."), br(),
                        
                        
                        visNetworkOutput("mark_plot", width = "100%", height = "1000px")
                        
               ),
               
               tabPanel("Analyzing the Data",
                        
                        h2(tags$b("From categorical to numerical data")),
                        br(),
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
                           (Very Dissatisfied, Dissatisfied, Neutral, Satisfied, or Very Satisfied)."), 
                        br(),
                         
                        p("To analyze this data, we counted how many times each 
                           respondent’s name appeared in other respondents’ top 4 
                           closest friends lists. We compared this to each respondent’s 
                          satisfaction level"), 
                        br(),
                        
                        plotOutput("compare_satisfaction"),
                        br(),
                        
                        p("First, we compared the satisfaction score mean of 
                          respondents who did appear in others' friend lists, 
                          and the satisfaction score mean of those who did not."), 
                        br(),

                        p("The horizontal line the graph indicates the mean satisfaction 
                          level across all first-years, which was 0.7917676. 
                          Since the satisfaction levels were scaled as a 0 if neutral 
                          and a 1 if satisfied, our respondents, on average, reported 
                          somewhere between neutral and satisfied (leaning satisfied) 
                          regarding their social connections."), 
                        br(),
                        
                        p("Respondents who appeared in other respondents’ top 4 
                        closest friends lists reported a higher level of satisfaction 
                        than respondents who did not appear. In fact, respondents 
                        who did not appear at all in other respondents’ top 4 
                        closest friends lists reported a below average level of 
                        satisfaction, while respondents who did appear reported 
                        an above average level of satisfaction."), br(),
                        
                        p("Then, we plotted number of appearances against mean 
                        satisfaction score on a scatter plot."), br(),
                        
                        plotOutput("satisfaction_scatter_plot"),
                        
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
                        br(),
                        plotOutput("top_socially_connected_appearance_in_top4"),
                        br(),
                        p("Obviously these numbers are pretty thin, indicating that it is hard to draw conclusions from the students who were listed most often. Therefore, the data we have suggests that students who know more people may have more close friends, but the relationship is certainly not clear."),
                        
                        h3(tags$b("How many members of the Class of 2023 would 
                        you recognize on the street?")),
                        
                        p("We also asked respondents how many members of the 
                        2023 class they would recognize on the street. Below were our results:"), 
                        br(),
                        
 # We need this graph                       
                        
                        p("We then observed the mean satisfaction levels 
                        (measured on a scale from -2 to 2) of each group that 
                        stated similar street recognition levels. Below are our 
                        results, graphed with the overall average satisfaction 
                        level of our entire sample (the black horizontal line)."),
 br(),
                        
                        plotOutput("social_num_street_satisfaction"),
                        
                        p("As can be observed, the respondents who could recognize 
                        1000+ first-years on the street reported higher satisfaction 
                        level means. The respondents who could recognize 0-50, 
                        on the other hand, reported lower than average satisfaction 
                        means. This result seems to suggest that the more 
                        fellow freshmen the respondent could recognize, the 
                        more satisfied they tended to feel about their social 
                        experience. This correlation is intuitive."), 
 br(),
                        
                        h3(tags$b("How many members of the Class of 2023 would 
                                  you feel comfortable sitting down with at Annenberg?")),
                        
                        
                        p("We also asked respondents how many freshmen they 
                        would feel comfortable sitting down with at Annenberg. Below were our results:"), br(),
                       
 # Get the RMD 
                        img(src="7know_in_berg_responses.png", width = "50%"),
                        
                        p("We then observed the mean satisfaction levels 
                        (measured on a scale from -2 to 2) of each group that 
                        stated similar numbers of students they felt comfortable 
                        sitting next to in Annenberg. Below are our results, 
                        graphed with the overall average satisfaction level 
                        of our entire sample (the black horizontal line)."), br(),

 # Get the RMD                        
                        img(src="8know_in_berg_satisfaction.png", width = "50%"),
                        
                        p("In this question, students who answered differently 
                        did not demonstrate large differences in satisfaction 
                        level. Compared to the street recognition level question, 
                        mean satisfaction scores of Annenberg-comfortable 
                        groups differed less noticeably. This observation 
                        suggests that respondents base personal satisfaction 
                        more heavily on how many students they recognize, 
                        rather than how many students they feel personally close to."), br()
                        
                        
                        
                        
               ),
               tabPanel("Most Connected Individuals",
                        h2(tags$b('Analyzing the "most socially" connected individuals?')),
                        
                        p('In our survey, we asked respondents to name the student 
                          they perceive to be the "most socially connected" in the Class of 2023.'), br(),
                        
                        
                        h3(tags$b('How many times did the "most socially connected" 
                                  students appear in top 4 friend lists??')),
                        
                        p('To create the graph below, we compiled the top 100 
                        "most socially connected" students. Then, we counted 
                        how many times those students appeared in other 
                        respondents top 4 friend lists.'), br(),
                        
 # Get the RMD                       
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
                        
                        h3(tags$b('Social Network Analysis Tests')),
                        
                        h4(tags$b("Survey: Top 10 Connected Individuals")),
                        p("These IDs correspond to the top 10 students voted to be the most connected individuals in the freshmen class."),
                        gt_output("most"),
                        p("To assess the accuracy of these predictions, we ran Social Network Analysis tests on the connections listed by respondents. (Connections are counted if the student’s name is written as one of the top 4 closest friends.)"),
                        
                        h4(tags$b("What is Social Network Analysis?")),
                        p("Social Network Analysis is a set of methods used to visualize networks, describe specific characteristics of overall network structure, and build mathematical and statistical models of network structures and dynamics."),
                        p("First, we begin by visualizing our social network graph. A graph consists of edges and nodes. In our case, the nodes are the freshmen, and the edges are connections between students. The graph is colored based on the level of connection (first through fourth)"),
                        plotOutput("helen_plot", width = 500, height = 500),
                        
                        p("Now, onto the analysis. Although visualizing the network can be useful for examining the data at a high level, one of the most important features of social network analysis is the ability to mathematically describe a node’s characteristics on the network. The positions of individuals are best described in terms of centrality. Centrally positioned individuals enjoy a position of privilege over those in the circumference of the network. We ran the three main centrality tests: degree centrality, betweenness centrality, and closeness centrality."),
                        
                        h4(tags$b("Degree Centrality")),
                        p("Degree centrality is the simplest of the tests. It measures the number of connections between a node and all other nodes. Essentially, it calculates the number of connections each student has. Degree centrality assigns an importance score based simply on the number of links held by each node. This test is best used for finding very connected individuals who can quickly connect with the wider network."),
                        p("The following IDs correspond to the 10 students with the highest degree centrality:"),
                        gt_output("degree"),
 
                        h4(tags$b("Closeness Centrality")),
                        p("Closeness centrality is an evaluation of the proximity of a node to all other nodes in a network, not only direct connections. The closeness centrality of a node is defined by the inverse of the average length of the shortest paths to or from all the other nodes in the graph. Closeness centrality can help find good ‘broadcasters’, but in a highly-connected network, often all nodes have a similar score (this is the case for our data). This test is best used for finding the individuals who are best placed to influence the entire network most quickly."),
                        p("The following IDs correspond to the 10 students with the highest closeness centrality:"),
                        gt_output("close"),
 
                        h4(tags$b("Betweenness Centrality")),
                        p("Betweenness centrality measures the number of times a node lies on the shortest path between other nodes.This measure shows which nodes are ‘bridges’ between nodes in a network. It does this by identifying all the shortest paths and then counting how many times each node falls on one. Betweenness is useful for analyzing communication dynamics, but should be used with care. A high betweenness count could indicate someone holds authority over disparate clusters in a network, or just that they are on the periphery of both clusters. This test is best used for finding the individuals who influence the flow around a system"),
                        p("The following IDs correspond to the 10 students with the highest betweenness centrality:"),
                        gt_output("between"),
                        
                        h4(tags$b("Eigenvector Centrality")),
                        p("Eigenvector Centrality measures a node’s influence based on the number of links it has to other nodes in the network, just like degree centrality. The test then goes a step further by also taking into account how well connected a node is, and how many links their connections have, and so on through the network. By calculating the extended connections of a node, we can identify individuals with influence over the whole network, not just those directly connected to it. This test is the best overall evaluation of an individual in a network."),
                        p("The following IDs correspond to the 10 students with the highest eigenvector centrality:"),
                        gt_output("eigen"),

                        h4(tags$b("Top 10 Centrality Comparison")),
                        gt_output("top10"),
                        p("We can see a significant difference in the perceived influential people in the network, versus the centrality test results."),
                        p("Only one individual out of the top voted people actually appears in the top 10 for a centrality test. This is ID 1570, voted 3rd for most connected. They were #1 in degree centrality and #4 in betweenness centrality."),
                        p("From our centrality tests, it is quite clear that the most connected person, according to our survey data, is ID 1401. They were 1st in eigenvector centrality, 3rd in degree centrality, 1st in closeness centrality, and 2nd in betweenness centrality. There is no other individual who was in the top 10 for all centrality tests, and certainly no one who monopolized the top 3 positions across the board."),
                        p("However, ID 1401 was tied for 21st place in voting, with only 2 people who believed them to be the most socially connected person in the class."),
                        p("As such, our social network analysis testing prompts interesting questions and possible conclusions."),
                        p("The disparity in data could be explained by several factors. We must consider how connections have been defined from survey data. We asked respondents to list the 4 people who they feel closest to. As such, our centrality analysis favors people who are trusted and very close to their friends. On the other hand, our survey question asks students who they believe the most socially connected person is. The wording of this question has several implications, as students may vote for people they believe are popular, are very involved in networking. The people who perform the best will likely differ from people who have developed deeper relationships with their peers."),
                        p("Overall, this data opens the question of how we as humans define social connectivity. Do we prioritize close-knit relationships, or developing a broad network? Do we consider someone who knows many people casually as more socially successful than someone who knows less people more deeply? Our survey data can only give us results, but it is up to us as people to apply these findings to our social interactions.")
                        
               ),
               tabPanel("Satisfaction and race",
                        
                        h2(tags$b("Analysis of the data by race")),
                        
                        p("According to Harvard college admission statistics, 14.3% of the class of 2023 is African-American, 25.3% is Asian, 12.2% is Hispanic or Latino, 1.8% is Native American, and 0.6% is Native Hawaiian. While we tried our best to simulate these numbers within our survey, we were unable to satisfactorily replicate Harvard’s admission statistics."), 
                        br(),
                        p("Of survey respondents, 27.95% were Asian / Pacific Islander, 7.71% were African American, 7% were Hispanic or Latino, 37% were white, 18.06% were of mixed race, and 2.27% fell into other categories. Here is the full racial breakdown:"),
                        br(),
                        gt_output("racial_respondent"),
                        br(),
                        p("Given the small sample size of our survey, relative to the size of the class of 2023, we are hesitant to make generalizations about how race affects social relationships. Furthermore, we did not collect evidence of the fundamental factors behind friendship and therefore cannot speculate on the reasons for friendship. We did collect measures of satisfaction, and the data looks remarkably similar across racial groups. The following graphs show social satisfaction across three racial groups: White, Asian / Pacific Islander, and Other."),
                        br(),
                        plotOutput("race_satisfaction"),
                        br(),
                        br(),
                        
                        br(),
                        br(),
                        br(),
                        
                        p("In addition to satisfaction rates mirroring each other, the amount of people each group of students said that they would recognize, under various circumstances, also mirrored one another."),
                        br(),
                        br(),
                        
                        
                        plotOutput("race_know_street"),
                        br(),
                        br(),
                        
                        
                        plotOutput("race_know_berg"),
                        br(),
                        br(),
                        
                        
                        plotOutput("race_know_name"),
                        br(),
                        br(),
                        
                        br(),
                        p("These visualizations lack insight about causal effects, but they do suggest that all Harvard 2023 students, regardless of race, are experiencing a similar social atmosphere. Students across the racial spectrum reported similar satisfaction levels, similar levels of recognition on the street and by name, and racial groups were homogeneous in the reported number of people they would feel comfortable spontaneously sitting with in Annenberg."),
                        p("Regarding who was listed as the survey's number one friend, 150 of respondents both filled out our form and were listed by another person as a number one friend. Of those 150 students, 85 of the students were of the same race as their best friend. Since we do not have the demographic data of students who did not fill out the form, 20.4% of students reported that their best friend was of the same race as them, and this is only the lower bound. This gives credence to the fact that race plays a role when determining the friends we pick, but this same metric is confounded by dorm placement and extracurricular activities."),
                        p("All in all, the data from our survey suggests that students of all races feel similarly about the social condition of Harvard, suggesting that other factors may play a larger role in determining our social abilities than race.")
                
               ),
               
               tabPanel("Comment Analysis",
                        h2(tags$b("Word Cloud- taken from survey comments")),
                        img(src="wordcloud.png", width = "50%", align = "center"),
                        h2(tags$b("Comment Analysis")),
                        p('Four Primary Themes: (out of 81 total comments)'),
                        
                        h3(tags$b("Holding a Positive Outlook on Harvard's Social Environment: 10 comments")),
                        p("Examples:"),
                        p("“I believe Harvard builds community really well, especially for first-years. Having all first-years eat in one dining hall really builds up the freshman community. Our entryways and dorms also provide us with a community right when we arrive on campus. I'm grateful for how attentive Harvard is in helping first-years meet new people.”"),
                        p("“People have been incredibly encouraging and supportive. I really haven’t encountered much of the toxic competitiveness (though I’m well aware it exists), so I feel I’ve been particularly fortunate in that I’ve been able to find a number of people in my life with whom I share genuinely deep bonds with.”"),
                        p("“Everyone has been pretty great and super interesting to meet so far.”"),
                        p("“Absolutely love it”"),
                        p("“I LOVE HARVARD AND MY FRIENDS!!!!!”"),
                        
                        h3(tags$b("Competitive Environment and Transactional Relationships: 15 comments")),
                        p("Examples:"),
                        p("“I feel like it's a little hard to make genuine social connections here because everyone is so busy and many people see connections in a very transactional way.”"),
                        p("“Unfortunately, friendships at Harvard often seem quite superficial.”"),
                        p('“Most people are really concerned with having everyone like them and with "networking," often leading in disingenuous or transactional relationships.”"),
                        p("“There’s also a lot of competitiveness that makes it hard for me to connect with people...Also I realize this is my own specific experience. I’m in too many pre-professional clubs which definitely doesn’t help.”'),
                        p("“It was actually a very personally challenging experience for me... trying to figure out what I wanted to do with my life long-term...whether or not I should ‘sell out’...”"),
                        
                        h3(tags$b("Difficulty Branching Out: Coping with Cliques: 11 comments")),
                        p("Examples:"),
                        p("“Once you make friends initially, it is hard to expand out of that circle.”"),
                        p("“I feel like Harvard is a difficult place to make a ton of friends unless you're already part of a defined group, like a sports team.”"),
                        p("“I find it difficult to connect with people that you aren't naturally in contact with (in your entryway, in your social group, a cappella group, section, etc.)”"),
                        p("“If you don't have time to be in one of the high-time commitment clubs, then it's really hard to make friends. I personally feel super lonely because of my niche interests and my desire to just debate with my peers over various topics.”"),
                        p("“I feel like the social environment kind of changed for me from the beginning of the year until now. Speaking as a freshman, when we first arrived on campus, everyone wanted to know everyone and people were super friendly. As the year went on, this dynamic definitely changed just in the sense that people started to hang out with the same sort of people. Also, I feel like all athletes know each other (I’m not an athlete).”"),
                        
                        h3(tags$b("Self Segregation and Racial Exclusivity: 8 comments")),
                        p("Examples: "),
                        p("“I will say the social environment is very very easy to self-segregate. In my case, I went from having maybe two asian friends in high school to having majority Asian friends in college (this is also where choice of extracurriculars matters a lot, and I am in two orgs that have “Asian american” in their names)”"),
                        p("“It is incredibly racially segregated and as a biracial woman (half black, half white) who identifies much more as black, I've definitely had to choose whether I wanted to hang out with my black friends or everyone else – it feels like there's no bridging that divide (and from my experiences during this aborted first year, it doesn't seem like anyone is particularly interested in trying). I understand why this is, it's just a stark observation that I've made and that has contributed to me often feeling out of place.”"),
                        p("“From my perspective, at Harvard, people are self-segregated into their respective groups. It is easy for people to form groups that are primarily Asian, primarily Hispanic, primarily Athletes, primarily STEM, and etc. It might be a universal trend across multiple schools, but it is a shame that for a school body that is as diverse as Harvard, people still self-segregate into these groups rather than form more diverse communities.”"),
                        
                        h2(tags$b("Summary:")),
                        p("Majority seem to find there to be some level of difficulty, whether in branching out from initial friend groups, a degree of superficiality, racial exclusion, or getting to know people on a deeper level, but as a whole are nonetheless generally satisfied with the overall experience. People typically felt that clubs are where they were able to find their most meaningful connections but found making friends outside of that context more challenging.")
  
                        
            ),
            tabPanel("Creators",
                     h3(tags$b("Purpose of our research")),
                     p("Are the friends we make truly representative of our interests, or are they actually determined by uncontrollable factors like the dorms we live in, our extracurriculars, our race, and where we come from? In seeking to answer this question and others like it, we decided to map and analyze the literal social network of the Harvard class of 2023."), 
                     p("We wanted to know why some people within the class of 2023 seemed to be well connected, while others seemed to be anonymous. At the heart of this project was our interest in the literal web of social connections, but we were also very interested in determining the role our environments play when determining the people we consider friends. By asking students about their demographic background, their four closest friends, and other speculative questions, we created a representative map of social connections, inferencing conclusions about the role of our environment from the available data."),
                     p("This project was initially pitched to us by Preceptor David Kane in preparation for the Government 1005 semester long final project at Harvard University. Preceptor expressed interest in comparing social connections through the freshman class at Harvard to those at Yale, but after determining the resources we had available, we decided to limit the scope of our study to Harvard."),
                     
                     
                     h2(tags$b("The Team")),
                     p("In order to complete this project, we had an amazing team of 6 different student researchers. Each member of the team was responsibile for a distinct portion of the project, but there was also collaboration at every step."),
                     h2(tags$b("Jeremiah Kim")),
                     p("Hi, I am currently pursuing an A.B. in social studies, and I intend to complete a focus field in the political economy of Asia. I use R as an assistant researcher at the Edmond J. Safra Center for Ethics. I am a bass singer for the Harvard Radcliff Collegium Musicum, a staff writer for the Harvard College Law Review, and my contact information is jeremiahkim@college.harvard.edu."),
                     h2(tags$b("Emily Ni")),
                     p("Hello! I am a freshman at Harvard College pursuing an A.B in Economics and Government. In Gov 1005, I’ve enjoyed using R for applications related to data science! My contact information is eni@college.harvard.edu"),
                     h2(tags$b("Kelsey Wu")),
                     p("My name is Kelsey Wu, and I’m planning on studying Government under the Data Science Track and Economics. On campus, I’m involved in Harvard Open Data Project and Harvard Data Analytics Group, sing for the Veritones, and conduct research for HLS. I love trying various noodles, playing around with Final Cut Pro, and spontaneously blasting music with friends. Feel free to contact me at kelseywu@college.harvard.edu"),
                     h2(tags$b("Jack Kelly")),
                     p("I’m Jack Kelly, a freshman from Fairfax County, Virginia currently residing in Belfast, Maine. I am currently planning to concentrate in Government or Economics and am a staff writer for the Harvard College Law Review. I also work part time for HSA at their marketing agency, Studio 67. In my spare time, I can be found listening to podcasts, watching random YouTube videos, and relaxing with friends. Feel free to reach me at jackrandolphkelly@gmail.com"),
                     h2(tags$b("Mark Stephens")),
                     p("I am a first-year at Harvard from San Francisco, CA.  Although undeclared, I plan on studying economics and computer science.  Along with my studies, I also play midfield on the men’s lacrosse team here at Harvard. My contact information is markstephens@college.harvard.edu"),
                     h2(tags$b("Helen Pang")),
                     p("Hi! I’m a first-year just housed in Quincy, planning on studying Computer Science and Statistics. I’m a data research assistant at HKS, and I’m very passionate about exploring new data science software. You can reach me at hpang@college.harvard.edu."))
    
))

# Define server logic
# I store the file in filename using normalizePath
# I named my image "plot" indicated by output$plot
# I used the renderImage function since I wanted to render the plot that I saved
# I do this by creating a list that specifies the image
# The filename variable is stored in source, the file type is an image
# The image's original dimensions are 2099x1499, so I scaled it down by half and rounded

server <- function(input, output) {
  
  

    #Included
    
    output$dorm_plot <- renderPlot({
      freshmen <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>%
        clean_names()
      
      total_respondents <- freshmen %>% nrow()
        
      dorm_plot <- freshmen %>%
        select(dorm) %>%
        count(dorm) %>%
        mutate(perc_dorm = round(n / total_respondents*100, digits = 2))
      
      dorm_plot %>%
        ggplot(aes(y = n, x = dorm, fill = dorm)) +
        geom_col() + 
        geom_text(aes(label = paste0(perc_dorm, "%")), size = 4) +
        coord_flip() + 
        theme_economist() +
        labs(
          x = "",
          y = "Number of Respondents",
          title = "Dorm Distribution in Respondent Sample") + 
        theme(legend.position = "none")
        
    })
    
    #Included
    
    output$gender_plot({
      
      freshmen <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>%
        clean_names()
      
      total_respondents <- freshmen %>% nrow()
      
      gender_plot <- freshmen %>%
        select(gender) %>%
        count(gender) %>%
        mutate(perc_gender = round(n / total_respondents * 100, digits = 2))
      
      level_order <- c('Female', 'Male', 'Genderqueer', 'Prefer not to say')
      
      gender_plot %>%
        ggplot(aes(y = n, x = factor(gender, level = level_order), fill = gender)) +
        geom_bar(stat = "identity", width = 1) + 
        geom_text(aes(label = paste0(perc_gender, "%")), size = 4) + 
        theme_economist() +
        labs(
          x = "",
          y = "Number of Respondents",
          title = "Gender Distribution in Respondent Sample") + 
        theme(legend.position = "none")
    })
    
    #Included
    
    output$race_plot({
      
      freshmen <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>%
        clean_names()
      
      total_respondents <- freshmen %>% nrow()
      
      race_plot <- freshmen %>% 
        mutate(manipulated_race = ifelse(race != "White" & 
                                           race != "Asian / Pacific Islander" & 
                                           race != "Black or African American" & 
                                           race != "Hispanic or Latino", 
                                         "Mixed Race / Other", 
                                         race)) %>% 
        select(manipulated_race) %>% 
        count(manipulated_race) %>% 
        mutate(perc_race = round(n / total_respondents*100, digits = 2))
      
      race_plot %>%
        ggplot(aes(y = n, x = manipulated_race, fill = manipulated_race)) +
        geom_col() + 
        geom_text(aes(label = paste0(perc_race, "%")), size = 4) +
        coord_flip() + 
        theme_economist() +
        labs(
          x = "",
          y = "Number of Respondents",
          title = "Racial Distribution in Respondent Sample") + 
        theme(legend.position = "none")
    })
    
    # Included
    
    output$gap_year_plot({
      
      freshmen <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>%
        clean_names()
      
      total_respondents <- freshmen %>% nrow()
      
      gap_year_plot <- freshmen %>% 
        select(gap_year) %>% 
        count(gap_year) %>% 
        mutate(perc_gap = round(n / total_respondents*100, digits = 2))
      
      gap_year_plot %>%
        ggplot(aes(y = n, x = gap_year, fill = gap_year)) +
        geom_col() + 
        geom_text(aes(label = paste0(perc_gap, "%")), size = 4) +
        theme_economist() +
        labs(
          x = "",
          y = "Number of Respondents",
          title = "Gap Year Student Distribution in Respondent Sample") + 
        theme(legend.position = "none")
    })
    
    # Included
    
    output$int_plot({
      
      freshmen <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>%
        clean_names()
      
      total_respondents <- freshmen %>% nrow()
      
      int_plot <- freshmen %>% 
        select(international) %>% 
        count(international) %>% 
        mutate(perc_int = round(n / total_respondents*100, digits = 2))
      
      int_plot %>%
        ggplot(aes(y = n, x = international, fill = international)) +
        geom_col() + 
        geom_text(aes(label = paste0(perc_int, "%")), size = 4) +
        theme_economist() +
        labs(
          x = "",
          y = "Number of Respondents",
          title = "International Student Distribution in Respondent Sample") + 
        theme(legend.position = "none")
    })
    
    output$pre_plot({
      freshmen <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>%
        clean_names()
      
      total_respondents <- freshmen %>% nrow()
      
      pre_plot <- freshmen %>% 
        mutate(manipulated_pre = ifelse(pre_orientation != "FAP - First-Year Arts Program" & 
                                          pre_orientation != "FCU - Fall Clean-Up with Dorm Crew" & 
                                          pre_orientation != "FIP - First-Year International Program" & 
                                          pre_orientation != "FOP - First-Year Outdoor Program" &
                                          pre_orientation != "FYRE - First-Year Retreat and Experience" &
                                          pre_orientation != "None",
                                        "FYRE & FCU / FIP & FCU", 
                                        pre_orientation)) %>% 
        select(manipulated_pre) %>% 
        count(manipulated_pre) %>% 
        mutate(perc_pre = round(n / total_respondents*100, digits = 2)) %>% 
        na.omit
      
      level_order <- c('None',
                       'FYRE & FCU / FIP & FCU',
                       'FAP - First-Year Arts Program', 
                       'FCU - Fall Clean-Up with Dorm Crew', 
                       'FIP - First-Year International Program', 
                       'FOP - First-Year Outdoor Program',
                       'FYRE - First-Year Retreat and Experience')
      
      pre_plot %>%
        ggplot(aes(y = n, x = factor(manipulated_pre, level = level_order), fill = manipulated_pre)) +
        geom_col() + 
        geom_text(aes(label = paste0(perc_pre, "%")), size = 4) +
        coord_flip() +
        theme_economist() +
        labs(
          x = "",
          y = "Number of Respondents",
          title = "Pre-Orientation Participation Distribution in Respondent Sample") +
        theme(legend.position = "none")
    })
    
    output$sports_plot({
      
      freshmen <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>%
        clean_names()
      
      total_respondents <- freshmen %>% nrow()
      
      sports_plot <- freshmen %>% 
        select(sports) %>% 
        count(sports) %>% 
        mutate(perc_sports = round(n / total_respondents*100, digits = 2))
      
      sports_plot %>%
        ggplot(aes(y = n, x = sports, fill = sports)) +
        geom_col() + 
        geom_text(aes(label = paste0(perc_sports, "%")), size = 4) +
        theme_economist() +
        labs(
          x = "",
          y = "Number of Respondents",
          title = "Sports Participation Distribution in Respondent Sample") +
        theme(legend.position = "none")
    })
    
    output$compare_satisfaction <- renderPlot({
        freshmen <- read_csv("data/freshmen.csv") 
   
        freshmen_satisfaction <- freshmen %>%
            nest(top4 = c(know_best_1, know_best_2,
                          know_best_3, know_best_4)) %>%
            select(name, satisfaction, top4) %>%
            mutate(satisfaction_lvl = case_when(satisfaction == "Very Satisfied" ~ 2,
                                                satisfaction == "Satisfied" ~ 1,
                                                satisfaction == "Neutral" ~ 0,
                                                satisfaction == "Dissatisfied" ~ -1,
                                                satisfaction == "Very Dissatisfied" ~ -2))
        
        # List of all names listed in top 4 friends, with repeats
        
        freshmen_top4_list <- unlist(freshmen_satisfaction$top4)
        
        # Calculate overall satisfaction score mean
        
        all_freshmen_satisfaction_mean <- freshmen_satisfaction %>%
            summarize(mean = mean(satisfaction_lvl)) %>%
            pull(mean)
        
        # Create a tibble for comparing satisfaction levels vs. whether or not
        # they appear.
        
        compare_satisfaction <- freshmen_satisfaction %>%
            mutate(appear = name %in% freshmen_top4_list) %>%
            group_by(appear) %>%
            summarize(mean_satis = mean(satisfaction_lvl))
        
        compare_satisfaction %>%
            ggplot(aes(x = appear, y = mean_satis, fill = appear)) +
            geom_bar(stat = "identity") + 
            guides(fill=FALSE) +
            scale_x_discrete(labels = c("No", "Yes")) +
            labs(
                x = "Did the respondent's name appear in other respondents' top 4 friends lists?",
                y = "Mean Satisfaction Score",
                title = "Comparing mean satisfaction scores of respondents",
                subtitle = "Respondents who appeared in others lists were more satisfied",
                caption = "Very Dissatisfied = -2, Dissatisfied = -1, Neutral = 0, Satisfied = 1, Very Satisfied = 2"
            ) +
            geom_hline(yintercept = all_freshmen_satisfaction_mean) +
            theme_economist()
    })
    
    # Included
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
            theme_economist()
    })
    
    # Included
    output$race_satisfaction <- renderPlot({
      
      data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv", col_types = cols()) %>% 
        mutate(manipulated_race = ifelse(race != "White" & race != "Asian / Pacific Islander" & race != "Black or African American" & race != "Hispanic or Latino", "Other", race)) %>% 
        select(-X1, gender, race, first_meet, second_meet, third_meet, "fourth-meet", 
               id, first_id, second_id, third_id, fourth_id, know_street, know_by_name, know_annenberg, satisfied, manipulated_race)
      
        race_satisfaction <- tempfile(fileext='.gif')
        
        # now make the animation
        p = data %>% 
          select(manipulated_race, satisfied) %>% 
          group_by(manipulated_race) %>% 
          count() %>% 
          mutate(proportion = case_when(
            manipulated_race == "Asian / Pacific Islander" ~ freq / 116,
            manipulated_race == "White" ~ freq / 154, 
            manipulated_race == "Black or African American" ~ freq / 32,
            manipulated_race == "Hispanic or Latino" ~ freq / 29,
            manipulated_race == "Other" ~ freq / 84)) %>% 
          ggplot(aes(x = reorder(satisfied, proportion), y = proportion)) +
          geom_col(fill = "darkred") +
          theme_economist() +
          theme(axis.title.x = element_text(vjust = -0.5)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          transition_states(states = manipulated_race, transition_length = 1.5, state_length = 3, wrap = T) +
          labs(title = "Social Satisfaction levels",
               subtitle = "Racial group: {closest_state}",
               x = "How satisfied are respondents with Harvard's social environment?",
               y = "Proportion of respondents")
        
        
        anim_save("race_satisfaction.gif", animate(p)) # New
        
        # Return a list containing the filename
        list(src = "race_satisfaction.gif",
             contentType = 'image/gif')}, 
      deleteFile = TRUE)
    
    # Included
    
    output$race_know_street <- renderImage({
      
      data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv", col_types = cols()) %>% 
        mutate(manipulated_race = ifelse(race != "White" & race != "Asian / Pacific Islander" & race != "Black or African American" & race != "Hispanic or Latino", "Other", race)) %>% 
        select(gender, race, first_meet, second_meet, third_meet, "fourth-meet", 
               id, first_id, second_id, third_id, fourth_id, know_street, know_by_name, know_annenberg, satisfied, manipulated_race)
      
      race_know_street <- tempfile(fileext='.gif')
      
      s = data %>% 
        group_by(manipulated_race, know_street) %>%
        count() %>% 
        mutate(proportion = case_when(
          manipulated_race == "Asian / Pacific Islander" ~ freq / 116,
          manipulated_race == "White" ~ freq / 154, 
          manipulated_race == "Black or African American" ~ freq / 32,
          manipulated_race == "Hispanic or Latino" ~ freq / 29,
          manipulated_race == "Other" ~ freq / 84)) %>% 
        ggplot(aes(x = know_street, y = proportion)) + 
        geom_col(fill = "red") + 
        theme_economist() +
        theme(axis.title.x = element_text(vjust = -1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        transition_states(states = manipulated_race, transition_length = 1.5, state_length = 3, wrap = T) +
        labs(title = "The Amount of classmates respondents would
       recognize on the street",
             subtitle = "Racial group: {closest_state}",
             x = "Amount of people students would recognize 
       on the street",
             y = "Proportion of respondents")
      
      anim_save("race_know_street.gif", animate(s))
      
      list(src = "race_know_street.gif",
           contentType = 'image/gif')}, 
      deleteFile = TRUE)
    
    # Included
    
    output$race_know_name <- renderImage({
      
      data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv", col_types = cols()) %>% 
        mutate(manipulated_race = ifelse(race != "White" & race != "Asian / Pacific Islander" & race != "Black or African American" & race != "Hispanic or Latino", "Other", race)) %>% 
        select(gender, race, first_meet, second_meet, third_meet, "fourth-meet", 
               id, first_id, second_id, third_id, fourth_id, know_street, know_by_name, know_annenberg, satisfied, manipulated_race)
      
      race_know_name <- tempfile(fileext='.gif')
      
      x = data %>% 
        group_by(manipulated_race, know_by_name) %>%
        count() %>% 
        mutate(proportion = case_when(
          manipulated_race == "Asian / Pacific Islander" ~ freq / 116,
          manipulated_race == "White" ~ freq / 154, 
          manipulated_race == "Black or African American" ~ freq / 32,
          manipulated_race == "Hispanic or Latino" ~ freq / 29,
          manipulated_race == "Other" ~ freq / 84)) %>% 
        ggplot(aes(x = know_by_name, y = proportion)) + 
        geom_col(fill = "steelblue") + 
        theme_economist() +
        theme(axis.title.x = element_text(vjust = -1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        transition_states(states = manipulated_race, transition_length = 1.5, state_length = 3, wrap = T) +
        labs(title = "The Amount of classmates respondents know by name",
             subtitle = "Racial group: {closest_state}",
             x = "Amount of people students know by name",
             y = "Proportion of respondents")
      
      anim_save("race_know_name.gif", animate(x))
      
      list(src = "race_know_name.gif",
           contentType = 'image/gif')}, 
      deleteFile = TRUE)
    
    # Included
    
    output$race_know_berg <- renderImage({
      
      data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv", col_types = cols()) %>% 
        mutate(manipulated_race = ifelse(race != "White" & race != "Asian / Pacific Islander" & race != "Black or African American" & race != "Hispanic or Latino", "Other", race)) %>% 
        select(gender, race, first_meet, second_meet, third_meet, "fourth-meet", 
               id, first_id, second_id, third_id, fourth_id, know_street, know_by_name, know_annenberg, satisfied, manipulated_race)
      
      race_know_berg <- tempfile(fileext='.gif')
      
      b = data %>% 
        group_by(manipulated_race, know_annenberg) %>%
        count() %>% 
        mutate(proportion = case_when(
          manipulated_race == "Asian / Pacific Islander" ~ freq / 116,
          manipulated_race == "White" ~ freq / 154, 
          manipulated_race == "Black or African American" ~ freq / 32,
          manipulated_race == "Hispanic or Latino" ~ freq / 29,
          manipulated_race == "Other" ~ freq / 84)) %>% 
        ggplot(aes(x = know_annenberg, y = proportion)) + 
        geom_col(fill = "purple") + 
        theme_economist() +
        theme(axis.title.x = element_text(vjust = -1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        transition_states(states = manipulated_race, transition_length = 1.5, state_length = 3, wrap = T) +
        labs(title = "The Amount of classmates respondenets would sit next to
       in Annenberg",
             subtitle = "Racial group: {closest_state}",
             x = "Amount of people students would sit next
       to in Annenberg",
             y = "Proportion of respondents")
      
      anim_save("race_know_berg.gif", animate(b))
      
      list(src = "race_know_berg.gif",
           contentType = 'image/gif')}, 
      deleteFile = TRUE)
    
    # Included, but strongly consider getting rid of this graph and section
    
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
            ) +
          theme_economist()
    })
    
    # This is included, but the second part of these analysis is missing
    
    output$social_num_street_satisfaction <- renderPlot({
        freshmen_mod <- read_csv("data/freshmen_mod.csv")
        
        ggplot(data = freshmen_mod, aes(x = recognize_street, y = satisfaction)) + geom_point(alpha = .2) + geom_jitter() + labs(
            x = "Number of fellow freshmen respondents would recognize if encountered on the street", y = "levels of satisfaction",
            title = "Relationship between satisfaction with social life and number of people they recognize") +
          theme_economist()
    })
    
    # Included
    
    output$racial_respondent <- render_gt({
      
      read_this <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") %>% 
        mutate(manipulated_race = ifelse(race != "White" & race != "Asian / Pacific Islander" & race != "Black or African American" & race != "Hispanic or Latino", "Other", race)) %>% 
        select(gender, race, first_meet, second_meet, third_meet, "fourth-meet", 
               id, first_id, second_id, third_id, fourth_id, know_street, know_by_name, know_annenberg, satisfied, manipulated_race)
      
      r <- strsplit(read_this$race, split = ", ")
      race <- data.frame(race = unlist(r))
      
      races <- race %>% 
        group_by(race) %>% 
        count() %>% 
        ungroup() 
      
      total <- sum(races$freq) 
      
      
      racial_respondent <- races %>% 
        mutate(percent_survey = freq / total * 100) %>%
        gt() %>% 
        tab_header(title = "Racial Breakdown of Survey Respondents") %>% 
        fmt_number(decimals = 2, columns = "percent_survey") %>% 
        cols_label(race = "Reported Ethnicity and/or Race", freq = "Total number", percent_survey = "Percent of our survey") %>% 
        tab_footnote(footnote = "These percentages total 99.99% due to rounding",
                     locations = cells_column_labels(columns = vars("percent_survey")))
      
      
      racial_respondent
      
    })
    
    # Included
    
    output$helen_plot <- renderPlot({
        survey_data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") 
        library(RColorBrewer)
        color <- brewer.pal(4, "Set3") 
        
        edges_full <- survey_data %>% 
            select(id, first_id, second_id, third_id, fourth_id) %>% 
            pivot_longer(cols = c(first_id, second_id, third_id, fourth_id), names_to = "degree", values_to = "endpoint") %>% 
            mutate(colors = case_when(
                degree == "first_id" ~ color[1],
                degree == "second_id" ~ color[2],
                degree == "third_id" ~ color[3],
                degree == "fourth_id" ~ color[4]
            ))
        
        edges <- edges_full %>% 
            select(id, endpoint)
        
        nodes <- survey_data %>% 
            select(id) 
        
        first <- survey_data %>% 
            select(first_id) 
        
        second <- survey_data %>% 
            select(second_id) 
        
        third <- survey_data %>% 
            select(third_id) 
        
        fourth <- survey_data %>% 
            select(fourth_id) 
        
        all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
        
        
        nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
        
        
        g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
        
        
        l <- layout_on_sphere(g)
        
        
        #png("ms_6/helen_plot.png", 1800, 1800) 
        plot(g, vertex.label="", layout = l, edge.width = 1, vertex.size=0.5, edge.color = edges_full$colors)
        title("Friend Network",cex.main=3,col.main="black")
        
        legend("bottomright", c("First","Second", "Third", "Fourth"), pch=21,
               col="#777777", pt.bg=edges_full$colors, pt.cex=1, cex=.8)
    })
    
    output$eigen <- render_gt({
      survey_data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv")
      library(RColorBrewer)
      color <- brewer.pal(4, "Set3")
      edges_full <- survey_data %>% 
        select(id, first_id, second_id, third_id, fourth_id) %>% 
        pivot_longer(cols = c(first_id, second_id, third_id, fourth_id), names_to = "degree", values_to = "endpoint") %>% 
        mutate(colors = case_when(
          degree == "first_id" ~ color[1],
          degree == "second_id" ~ color[2],
          degree == "third_id" ~ color[3],
          degree == "fourth_id" ~ color[4]
        ))
      edges <- edges_full %>% 
        select(id, endpoint)
      nodes <- survey_data %>% 
        select(id) 
      first <- survey_data %>% 
        select(first_id) 
      second <- survey_data %>% 
        select(second_id) 
      third <- survey_data %>% 
        select(third_id) 
      fourth <- survey_data %>% 
        select(fourth_id) 
      all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
      nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
      g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
      
      eigen <- eigen_centrality(g)
      eigen <- eigen$vector
      
      eigenvector <- tibble(eigen) %>% 
        mutate(eigen_id = nodes$id) %>% 
        arrange(desc(eigen)) %>% 
        filter(eigen_id != 1654) %>% 
        select(eigen_id, eigen)
      
      eigenvector_10 <- eigenvector %>% 
        head(10)
      
      gt(eigenvector_10) %>% 
        tab_header(title = "Top 10 Eigenvector Centrality") %>% 
        cols_label(eigen_id = "ID",
                   eigen = "Eigenvector Score") 
    })
    output$degree <- render_gt({
      survey_data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv")
      library(RColorBrewer)
      color <- brewer.pal(4, "Set3")
      edges_full <- survey_data %>% 
        select(id, first_id, second_id, third_id, fourth_id) %>% 
        pivot_longer(cols = c(first_id, second_id, third_id, fourth_id), names_to = "degree", values_to = "endpoint") %>% 
        mutate(colors = case_when(
          degree == "first_id" ~ color[1],
          degree == "second_id" ~ color[2],
          degree == "third_id" ~ color[3],
          degree == "fourth_id" ~ color[4]
        ))
      edges <- edges_full %>% 
        select(id, endpoint)
      nodes <- survey_data %>% 
        select(id) 
      first <- survey_data %>% 
        select(first_id) 
      second <- survey_data %>% 
        select(second_id) 
      third <- survey_data %>% 
        select(third_id) 
      fourth <- survey_data %>% 
        select(fourth_id) 
      all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
      nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
      g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
      
      deg <- degree(g, mode="all")
      
      degree <- tibble(deg) %>% 
        mutate(degree_id = nodes$id) %>% 
        arrange(desc(deg)) %>% 
        filter(degree_id != 1654) %>% 
        select(degree_id, deg)
      
      degree_10 <- degree %>% 
        head(10)
      
      gt(degree_10) %>% 
        tab_header(title = "Top 10 Degree Centrality") %>% 
        cols_label(degree_id = "ID",
                   deg = "Number of Connections") 
    })
    output$most <- render_gt({
      survey_data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv")
      library(RColorBrewer)
      color <- brewer.pal(4, "Set3")
      edges_full <- survey_data %>% 
        select(id, first_id, second_id, third_id, fourth_id) %>% 
        pivot_longer(cols = c(first_id, second_id, third_id, fourth_id), names_to = "degree", values_to = "endpoint") %>% 
        mutate(colors = case_when(
          degree == "first_id" ~ color[1],
          degree == "second_id" ~ color[2],
          degree == "third_id" ~ color[3],
          degree == "fourth_id" ~ color[4]
        ))
      edges <- edges_full %>% 
        select(id, endpoint)
      nodes <- survey_data %>% 
        select(id) 
      first <- survey_data %>% 
        select(first_id) 
      second <- survey_data %>% 
        select(second_id) 
      third <- survey_data %>% 
        select(third_id) 
      fourth <- survey_data %>% 
        select(fourth_id) 
      all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
      nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
      g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
      
      most <- survey_data %>% 
        select(most_id) %>% 
        filter(most_id != 1654)
      
      count(most) %>% 
        arrange(desc(freq)) 
      most_10 <- count(most) %>% 
        arrange(desc(freq)) %>% 
        head(10) 
      
      gt(most_10) %>% 
        tab_header(title = "Top 10 Socially Connected Freshman: Survey") %>% 
        cols_label(most_id = "ID",
                   freq = "Number of Votes")
    })
    
    output$between <- render_gt({
      survey_data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv")
      library(RColorBrewer)
      color <- brewer.pal(4, "Set3")
      edges_full <- survey_data %>% 
        select(id, first_id, second_id, third_id, fourth_id) %>% 
        pivot_longer(cols = c(first_id, second_id, third_id, fourth_id), names_to = "degree", values_to = "endpoint") %>% 
        mutate(colors = case_when(
          degree == "first_id" ~ color[1],
          degree == "second_id" ~ color[2],
          degree == "third_id" ~ color[3],
          degree == "fourth_id" ~ color[4]
        ))
      edges <- edges_full %>% 
        select(id, endpoint)
      nodes <- survey_data %>% 
        select(id) 
      first <- survey_data %>% 
        select(first_id) 
      second <- survey_data %>% 
        select(second_id) 
      third <- survey_data %>% 
        select(third_id) 
      fourth <- survey_data %>% 
        select(fourth_id) 
      all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
      nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
      g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
      
      between <- betweenness(g, directed=F, weights=NA, normalized = T)
      
      betweenness <- tibble(between) %>% 
        mutate(between_id = nodes$id) %>% 
        arrange(desc(between)) %>% 
        filter(between_id != 1654) %>% 
        select(between_id, between)
    
      betweenness_10 <- betweenness %>% 
        head(10)
      
      gt(betweenness_10) %>% 
        tab_header(title = "Top 10 Betweenness Centrality") %>% 
        cols_label(between_id = "ID",
                   between = "Betweenness Score") 
    })
    
    output$close <- render_gt({
      survey_data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") 
      library(RColorBrewer)
      color <- brewer.pal(4, "Set3")
      edges_full <- survey_data %>% 
        select(id, first_id, second_id, third_id, fourth_id) %>% 
        pivot_longer(cols = c(first_id, second_id, third_id, fourth_id), names_to = "degree", values_to = "endpoint") %>% 
        mutate(colors = case_when(
          degree == "first_id" ~ color[1],
          degree == "second_id" ~ color[2],
          degree == "third_id" ~ color[3],
          degree == "fourth_id" ~ color[4]
        ))
      edges <- edges_full %>% 
        select(id, endpoint)
      nodes <- survey_data %>% 
        select(id) 
      first <- survey_data %>% 
        select(first_id) 
      second <- survey_data %>% 
        select(second_id) 
      third <- survey_data %>% 
        select(third_id) 
      fourth <- survey_data %>% 
        select(fourth_id) 
      all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
      nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
      g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
      
      close <- closeness(g, mode="all", weights=NA, normalized=T)
      
      closeness <- tibble(close) %>% 
        mutate(close_id = nodes$id) %>% 
        arrange(desc(close)) %>% 
        filter(close_id != 1654) %>% 
        select(close_id, close)
      
      closeness_10 <- closeness %>% 
        head(10)
      
      gt(closeness_10) %>% 
        tab_header(title = "Top 10 Closeness Centrality") %>% 
        cols_label(close_id = "ID",
                   close = "Closeness Score")
    })
    
    output$top10 <- render_gt({
      survey_data <- read_csv("data/FINAL_PUBLIC_DATA-4-23-20.csv") 
      library(RColorBrewer)
      color <- brewer.pal(4, "Set3")
      edges_full <- survey_data %>% 
        select(id, first_id, second_id, third_id, fourth_id) %>% 
        pivot_longer(cols = c(first_id, second_id, third_id, fourth_id), names_to = "degree", values_to = "endpoint") %>% 
        mutate(colors = case_when(
          degree == "first_id" ~ color[1],
          degree == "second_id" ~ color[2],
          degree == "third_id" ~ color[3],
          degree == "fourth_id" ~ color[4]
        ))
      edges <- edges_full %>% 
        select(id, endpoint)
      nodes <- survey_data %>% 
        select(id) 
      first <- survey_data %>% 
        select(first_id) 
      second <- survey_data %>% 
        select(second_id) 
      third <- survey_data %>% 
        select(third_id) 
      fourth <- survey_data %>% 
        select(fourth_id) 
      all_names <- full_join(fourth, full_join(third, full_join(first, second, by = c("first_id"="second_id")), by=c("third_id" = "first_id")), by=c("fourth_id" = "third_id"))
      nodes <- unique(full_join(nodes, all_names, by=c("id"="fourth_id")))
      g <- graph_from_data_frame(d = edges, vertices = nodes, directed=FALSE)
      
      most <- survey_data %>% 
        select(most_id) %>% 
        filter(most_id != 1654)
      
      count(most) %>% 
        arrange(desc(freq)) 
      most_10 <- count(most) %>% 
        arrange(desc(freq)) %>% 
        head(10) 
      
      deg <- degree(g, mode="all")
      degree <- tibble(deg) %>% 
        mutate(degree_id = nodes$id) %>% 
        arrange(desc(deg)) %>% 
        filter(degree_id != 1654) %>% 
        select(degree_id, deg)
      degree_10 <- degree %>% 
        head(10)
      
      close <- closeness(g, mode="all", weights=NA, normalized=T)
      closeness <- tibble(close) %>% 
        mutate(close_id = nodes$id) %>% 
        arrange(desc(close)) %>% 
        filter(close_id != 1654) %>% 
        select(close_id, close)
      closeness_10 <- closeness %>% 
        head(10)
      
      between <- betweenness(g, directed=F, weights=NA, normalized = T)
      betweenness <- tibble(between) %>% 
        mutate(between_id = nodes$id) %>% 
        arrange(desc(between)) %>% 
        filter(between_id != 1654) %>% 
        select(between_id, between)
      betweenness_10 <- betweenness %>% 
        head(10)
      
      eigen <- eigen_centrality(g)
      eigen <- eigen$vector
      eigenvector <- tibble(eigen) %>% 
        mutate(eigen_id = nodes$id) %>% 
        arrange(desc(eigen)) %>% 
        filter(eigen_id != 1654) %>% 
        select(eigen_id, eigen)
      eigenvector_10 <- eigenvector %>% 
        head(10)
      
      
      most_id_10 <- most_10$most_id
      degree_id_10 <- degree_10$degree_id
      close_id_10 <- closeness_10$close_id
      between_id_10 <- betweenness_10$between_id
      eigen_id_10 <- eigenvector_10$eigen_id
      comp <- tibble(most_id_10, degree_id_10, close_id_10, between_id_10, eigen_id_10) 
      
      gt(comp) %>% 
        tab_header(title = "Top 10 ID Comparison") %>% 
        cols_label(most_id_10 = "Survey",
                   eigen_id_10 = "Eigenvector Centrality",
                   degree_id_10 = "Degree Centrality",
                   close_id_10 = "Closeness Centrality",
                   between_id_10 = "Betweenness Centrality") 
    })
    #This is the web on the front of the web page
    
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
