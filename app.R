
#load packages to be used
library(shiny)
library(tidyverse)
library(patchwork) 
#library(ggokabeito)
#library(vdemdata)
library(highcharter)
library(shinythemes)


#load data 

df <- readRDS("data.rds")    

df %>% distinct(country_name) %>% pull() %>% sort() -> cnt

#output text for variable desrciption

question <- c("To what extent does the party show a lacking commitment to democratic norms prior to elections?", 
              "To what extent do representatives of the party use populist rhetoric (narrowly defined)?", 
              "How important is anti-elite rhetoric for this party?",
              "Do leaders of this party glorify the ordinary people and identify themselves as part of them?", 
              "Prior to this election, have leaders of this party used severe personal attacks or tactics of demonization against their opponents?", 
              "Prior to this election, to what extent was the leadership of this political party clearly committed to free and fair elections with multiple parties, freedom of speech, media, assembly and association?", 
              "According to the leadership of this party, how often should the will of the majority be implemented even if doing so would violate the rights of minorities?", 
              "To what extent does the leadership of this party explicitly discourage the use of violence against domestic political opponents?", 
              "Immigration refers to individuals entering the country for an indefinite, long-term or permanent period of time.", 
              "What is this party’s position toward social equality for the lesbian, gay, bisexual, and transgender (LGBT) community?", 
              "To what extent does the party leadership promote the cultural superiority of a specific social group or the nation as a whole?", 
              "To what extent does this party invoke God, religion, or sacred/religious texts to justify its positions?", 
              "What is the share of women in national-level leadership positions of this political party?", 
              "To what extent does this party support the equal participation of women in the labor market?", 
              "Please locate the party in terms of its overall ideological stance on economic issues.", 
              "To what extent does the party promote means-tested or universalistic welfare policies?", 
              "To what extent do the party and its candidates provide targeted and excludable (clientelistic) goods and benefits - such as consumer goods, cash or preferential access to government services - in an effort to keep and gain votes?", 
              "Does this party maintain permanent offices that operate outside of election campaigns at the local or municipal-level?", 
              "To what degree are party activists and personnel permanently active in local communities?", 
              "To what extent does this party maintain ties to prominent social organizations?", 
              "Which of the following options best describes the process by which the party decides on candidates for the national legislative elections?", 
              "To what extent do the elites in this party display disagreement over party strategies?",
              "To what extent is this party a vehicle for the personal will and priorities of one individual leader?"
              ) 

clear <- c("The index is computed as a transformed weighted average of the following input variables: Political opponents; Political pluralism; Minority rights; and Rejection of political violence",
           "The index is computed as the harmonic mean of the variables Anti-elitism and People-centrism",
           "Elites are relatively small groups that have a greater say in society than others, for instance due to their political power, wealth or societal standing. The specific groups considered to be the elite may vary by country and even from party to party within the same country as do the terms used to describe them. In some cases, “elites” can also refer to an international elite.", 
           "Many parties and leaders make reference to the “people”, but only some party leaders describe the ordinary people specifically as a homogenous group and emphasize/claim that they are part of this group and represent it. This means that they do not acknowledge the existence of divergent interests and values in society, but rather suggest that the “people” have a unified political will which should guide all political action. Often this group is glorified and romanticized, describing an ideal-typical ordinary person/commoner, who embodies the national ideal.", 
           "Severe personal attacks and demonization includes dehumanizing opponents or describing them as an existential threat or as subversive, criminal or foreign agents.", 
           "Party leaders show no commitment to such principles if they openly support an autocratic form of government without elections or freedom of speech, assembly and association (e.g. theocracy; single-party rule; revolutionary regime). Party leaders show a full commit- ment to key democratic principles if they unambiguously support freedom of speech, media, assembly and association and pledge to accept defeat in free and fair elections.", 
           "This concerns the rights enshrined in the Universal Declaration of Human Rights, which apply to everyone “without distinction of any kind, such as race, colour, sex, language, religion, political or other opinion, national or social origin, property, birth or other status.” The declaration protects - among others - freedom of speech, property, religion, peaceful assembly and association.", 
           "“Domestic political opponents” refers to all political opponents, with the exception of those who are engaged in an armed conflict with the state. They may be other political parties or other political groups and movements.",
           "Immigration refers to individuals entering the country for an indefinite, long-term or permanent period of time.", 
           "", 
           "This question refers to key non-economic cleavages in society, which could, for example, be based on caste, ethnicity, language, race, region, religion, or some combination thereof. This question further refers to cultural issues related to the national history and identity of a country. This question does not pertain to social groups based on gender or sexual orientation.",
           "", 
           "This question does NOT concern the share of women in the legislature.", 
           "Measures that support the equal participation of women in the labor market include - but are not limited to - legal provisions on equal treatment and pay, parental leave and financial support for child care.", 
           "Parties on the economic left want government to play an active role in the economy. This includes higher taxes, more regulation and government spending and a more generous welfare state. Parties on the economic right emphasize a reduced economic role for government: privatization, lower taxes, less regulation, less government spending, and a leaner welfare state.", 
           "", 
           "In some cases, parties and their candidates deliver targeted and excludable goods and benefits directly to individual voters with the explicit intention to keep or gain votes. In other cases, they rely on brokers or companies as intermediaries. In some countries, candidates promise procurement contracts or favorable regulatory decisions to companies in exchange for ensuring their workers vote for the party/candidate. Such efforts count as an instance of clientelism, if they are clearly targeted at one specific company and excludable. On the other hand, handing out of small gifts can be common in some contexts without the intention to “buy votes” but rather as courtesy or part of what all candidates do (“entry ticket”). Such activities do not count as attempts to “keep or gain votes”.", 
           "By “local or municipal” we mean low level administrative divisions that are ranked below regions, provinces, or states. We refer to offices that ensure professional personal and continued interaction of the party with citizens. Permanent offices operate outside of election campaigns.", 
           "Please consider the degree to which party activists and personnel are active both during election and non-election periods. Party personnel refers to paid staff.", 
           "When evaluating the strength of ties between the party and social organizations please consider the degree to which social organizations contribute to party operations by providing material and personnel resources, propagating the party’s message to its members and beyond, as well as by directly participating in the party’s electoral campaign and/or mobilization efforts. Social organizations include: Religious organizations (e.g. churches, sects, charities), trade unions/syndical organizations or cooperatives, cultural and social associations (e.g. sports clubs, neighborhood associations), political associations (e.g. environmental protection) and professional and business associations. Social organizations do not include paramilitary units or militias.", 
           "If nomination procedures vary across constituencies consider the most common practice.", 
           "Party strategies include election campaign strategy, policy stance, distribution of party financial resources, cooperation with other parties (i.e. coalition formation), and the selection of legislative and presidential candidates as well as the party leader. Party elites are prominent and influential party members such as current and former ministers, members of parliament or the party leadership, regional and municipal leaders, and opinion leaders. They do not necessarily have to be the part of the official party leadership.", 
           ""
            )

response <- c("Interval, from low to high (0-1)", 
              "Interval, from low to high (0-1)",
              "0: Not at all important. The leadership of this party never makes statements against the elite.", 
              "0: Never. The party leadership never glorifies and identifies with the ordinary people.",
              "0: Always. Party leaders always used severe personal attacks or tactics of demonization against their opponents.",
              "0: Not at all committed. The party leadership was not at all committed to free and fair, multi-party elections, freedom of speech, media, assembly and association.", 
              "0: Always. The leadership of this party argues that the will of the majority should always determine policy even if such policy violates minority rights.", 
              "0: Encourages. Leaders of this party often encourage the use of violence against domestic political opponents. ", 
              "0: Strongly opposes. This party strongly opposes all or almost all forms of immigration into the country.", 
              "0: Strongly opposes. This party is strongly opposed to LGBT social equality.", 
              "0: Strongly promotes. The party strongly promotes the cultural superiority of a specific social group or the nation as a whole.",
              "0: Always, or almost always. The party almost always invokes God, religion, or sacred/religious texts to justify its positions.", 
              "0: None.", 
              "0: Strongly opposes. This party strongly opposes all or almost all types of measures that support the equal participation of women in the labor market.",
              "0: Far-left. 1: Left.",
              "0: The party does not support either type of policies and opposes any public welfare policy.",
              "0: Not at all. The party and its candidates do not provide targeted goods and benefits in order to keep and gain votes. ", 
              "0: The party does not have permanent local offices.",  
              "0: There is negligible permanent presence of party activists and personnel in local communities. ", 
              "0: The party does not maintain ties to any prominent social organization. ",
              "0: The party leader unilaterally decides on which candidates will run for the party in national legislative elections.", 
              "0: Party elites display almost complete disagreement over party strategies and many party elites have left the party.",
              "0: The party is not focused on the personal will and priorities of one individual leader."
              )

response2 <- c("", 
               "",
               "1: Not important. The leadership of this party rarely makes statements against the elite.", 
               "1: Usually not. The party leadership generally does not glorify and identify with the ordinary people.",
               "1: Usually. Party leaders usually used severe personal attacks or tactics of demonization against their opponents.",
               "1: Not committed. The party leadership was not committed to free and fair, multi-party elections, freedom of speech, media, assembly and association.",
               "1: Usually. The leadership of this party argues that the will of the majority should usually determine policy even if such policy violates minority rights.",
               "1: Sometimes encourages. Leaders of this party sometimes encourage the use of violence against domestic political opponents and generally refrain from discouraging it.", 
               "1: Opposes. This party opposes most forms of immigration into the country. ", 
               "1: Opposes. This party is opposed to LGBT social equality.", 
               "1: Promotes. The party promotes the cultural superiority of a specific social group or the nation as a whole.",
               "1: Often, but not always. The party often, but not always, invokes God, religion, or religious texts to justify its positions.", 
               "1: Small minority (about 1-15%).", 
               "1: Opposes. This party opposes most types of measures that support the equal participation of women in the labor market.",
               "2: Center-left. 3: Center.",
               "1: The party solely promotes means-tested welfare policies. ",
               "1: A minor extent. The party and its candidates provide targeted goods and benefits to a minor extent in order to keep and gain votes.", 
               "1: The party has permanent local offices in few municipalities.", 
               "1: There is minor permanent presence of party activists and personnel in local communities.", 
               "1: The party maintains weak ties to prominent social organizations.",
               "1: The national party leadership (i.e. an executive committee) collectively decides which candidates will run for the party in national legislative elections.", 
               "1: Party elites display a high level of visible disagreement over party strategies and some of them have left the party.",
               "1: The party is occasionally focused on the personal will and priorities of one individual party leader."
)

response3 <- c("", 
               "",
               "2: Somewhat important. The leadership of this party sometimes makes statements against the elite. ", 
               "2: About half of the time. The party leadership sometimes glorifies and identifies with the ordinary people.",
               "2: About half of the time. Party leaders sometimes used severe personal attacks or tactics of demonization against their opponents.",
               "2: Weakly committed. The party leadership was weakly committed to free and fair, multi- party elections, freedom of speech, media, assembly and association.",
               "2: Half of the time. The leadership of this party argues that the will of the majority should about half of the time determine policy even if such policy violate minority rights.",
               "2: Discourages about half of the time. Leaders of this party occasionally discourage the use of violence against domestic political opponents, and do not encourage it.", 
               "2: Ambiguous/No position. This party has no clear policy with regard to immigration into the country.", 
               "2: Ambiguous/No position. This party has no clear policy with regard to LGBT social equality.", 
               "2: Ambiguous. The party does not take a specific position on the cultural superiority of a specific social group or the nation as a whole.",
               "2: About half of the time. The party about half of the time invokes God, religion, or religious texts to justify its positions.", 
               "2: Medium minority (about 16-25%).", 
               "2: Ambiguous/No position. This party has no clear policy with regard to measures that support the equal participation of women in the labor market.",
               "4: Center-right. 5: Right.",
               "2: The party mainly promotes means-tested policies, but a significant portion (e.g. 1/4 or 1/3) is universalistic and potentially benefits everyone in the population.",
               "2: A moderate extent. The party and its candidates provide targeted goods and benefits to a moderate extent in order to keep and gain votes.", 
               "2: The party has permanent local offices in some municipalities.",
               "2: There is noticeable permanent presence of party activists and personnel in local communities.", 
               "2: The party maintains moderate ties to prominent social organizations.",
               "2: Delegates of local/regional organizations decide which candidates will run for the party in national legislative elections.", 
               "2: Party elites display some visible disagreement over party strategies, but none of them have left the party.",
               "2: The party is somewhat focused on the personal will and priorities of one individual party leader."
)

response4 <- c("", 
               "",
               "3: Important. The leadership of this party often makes statements against the elite.", 
               "3: Usually. The party leadership generally glorifies and identifies with the ordinary people, which they claim to represent.",
               "3: Usually not. Party leaders usually did not use severe personal attacks or tactics of demo- nization against their opponents.",
               "3: Committed. The party leadership was committed to free and fair, multi-party elections, freedom of speech, media, assembly and association.",
               "3: Usually not. The leadership of this party argues that the will of the majority should usually not determine policy if such policy violates minority rights.",
               "3: Generally discourages. Leaders of this party often discourage the use of violence against its domestic political opponents.", 
               "3: Supports. This party supports most forms of immigration into the country.", 
               "3: Supports. This party supports LGBT social equality.", 
               "3: Opposes. The party opposes the promotion of the cultural superiority of a specific social group or the nation as a whole. ",
               "3: Rarely. The party rarely invokes God, religion, or religious texts to justify its positions.", 
               "3: Large minority (about 26-39%).", 
               "3: Supports. This party supports most types of measures that support the equal participation of women in the labor market.",
               "6: Far-right.",
               "3: The party roughly equally supports means-tested and universalistic welfare policies.",
               "3: A large extent. The party and its candidates provide targeted goods and benefits to a sizeable extent in order to keep and gain votes", 
               "3: The party has permanent local offices in most municipalities.", 
               "3: There is significant permanent presence of party activists and personnel in local communities.", 
               "3: The party maintains strong ties to prominent social organizations.",
               "3: All party members decide on which candidates will run for the party in national legislative elections in primaries/caucuses", 
               "3: Party elites display negligible visible disagreement over party strategies.",
               "3: The party is mainly focused on the personal will and priorities of one individual party leader."
)

response5 <- c("", 
               "",
               "4: Very important. The leadership of this party makes statements against the elite whenever possible.", 
               "4: Always. The party leadership always glorifies and identifies with the ordinary people, which they claim to represent.",
               "4: Never. Party leaders never used severe personal attacks or tactics of demonization against their opponents.",
               "4: Fully committed. The party leadership was fully committed to free and fair, multi-party elections, freedom of speech, media, assembly and association",
               "4: Never. The leadership of this party argues that the will of the majority should never determine policy if such policy violates minority rights.",
               "4: Consistently discourages. Leaders of this party consistently reject the use of violence against its domestic political opponents.", 
               "4: Strongly supports. This party strongly supports all or almost all forms of immigration into the country.", 
               "4: Strongly supports. This party strongly supports LGBT social equality.", 
               "4: Strongly opposes. The party strongly opposes the promotion of the cultural superiority of a specific social group or the nation as a whole.",
               "4: Never. The party never invokes God, religion, or religious texts to justify its positions.", 
               "4: Balanced (about 40% or more).", 
               "4: Strongly supports. This party strongly supports all or almost all types of measures that support the equal participation of women in the labor market.",
               "",
               "4: The party mainly promotes universalistic policies, but a significant portion (e.g. 1/4 or 1/3) of its policies are means-tested. 5: The party solely promotes universalistic welfare policies for all groups of the society.",
               "4: As its main effort. The party and its candidates provide targeted goods and benefits to the extent that it constitutes the party’s main effort in order to keep and gain votes.", 
               "4: There is widespread permanent presence of party activists and personnel in local communities.", 
               "4: There is minor permanent presence of party activists and personnel in local communities.", 
               "4: The party controls prominent social organizations.",
               "4: All registered voters decide on which candidates will run for the party in national legislative elections in primaries/caucuses.", 
               "4: Party elites display virtually no visible disagreement over party strategies",
               "4: The party is solely focused on the personal will and priorities of one individual party leader."
)


var_list <- c("v2xpa_antiplural", "v2xpa_popul", "v2paanteli_osp", "v2papeople_osp", "v2paopresp_osp", 
              "v2paplur_osp", "v2paminor_osp", "v2paviol_osp", "v2paimmig_osp", "v2palgbt_osp", 
              "v2paculsup_osp", "v2parelig_osp", "v2pagender_osp", "v2pawomlab_osp", "v2pariglef_osp", 
              "v2pawelf_osp", "v2paclient_osp", "v2palocoff_osp", "v2paactcom_osp", "v2pasoctie_osp", 
              "v2panom_osp", "v2padisa_osp", "v2paind_osp")

var_names <- c("Anti-pluralism Index","Populism Index", "Anti-elitism", "People-centrism", "Political opponents",
               "Political pluralism", "Minority rights", "Rejection of political violence", "Immigration", 
               "LGBT social equality", "Cultural superiority", "Religious principles", "Gender equality",
               "Working women", "Economic left-right scale", "Welfare", "Clientelism", "Local party office", 
               "Local organizational strength", "Affiliate organizations", "Candidate nomination", 
               "Internal cohesion", "Personalization of party")

var_max <- c(1,1,rep(4, 12), 6, 5, rep(4, 7))

question_2 <- c("To which particular group in society does the core membership and supporters of this party belong?", 
                "What were the major sources of party funds for this election campaign?", 
                "Which of the following issues are most relevant for the party’s effort to gain and keep voters?") 

clear_2 <- c("Choose only the key groups. Though you may choose up to three groups, if only one group is most relevant, please only choose that group.", 
             "Choose up to three most important ones. If a main source of funding for this campaign comes from the party’s assets such as properties and stocks, please code where these assets originally came from.",
             "Choose only the key issue(s). Though you may choose up to three issues, if only one issue is most relevant, choose only that issue. Most of these issues have been covered in this survey; if you need additional clarification as to what a category represents, you can return to the relevant question.")


# Define UI for application
ui <- bootstrapPage(
    #tags$head(includeHTML("gtag.html")),
    navbarPage(collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#7e094e" class="active" href="#">V-Party Explorer</a>'), id="nav",
               windowTitle = "V-Party Explorer",
               
    tabPanel("Country time-series",
             
    
        sidebarLayout(
            sidebarPanel(
              tags$strong("Explore time-series"), 
              
              p("Choose a country and variable. 
                Click on parties in the legend to remove/add which to display" 
              ),
                selectInput(
                inputId = "country", 
                label = "Select country", 
                choices = cnt, 
                selected = "Zambia"
                ),
           
               selectInput(
               inputId = "variable", 
               label = "Select variable", 
               choices = c("Anti-pluralism Index" = 1, 
                           "Populism Index" = 2,
                           "Anti-elitism" = 3,
                           "People-centrism" = 4, 
                           "Political opponents" = 5, 
                           "Political pluralism" = 6, 
                           "Minority rights" = 7, 
                           "Rejection of political violence" = 8, 
                           "Immigration" = 9, 
                           "LGBT social equality" = 10, 
                           "Cultural superiority" = 11, 
                           "Religious principles" = 12,
                           "Gender equality" = 13, 
                           "Working women" = 14, 
                           "Economic left-right scale" = 15,
                           "Welfare" = 16, 
                           "Clientelism" = 17,
                           "Local party office" = 18, 
                           "Local organizational strength" = 19, 
                           "Affiliate organizations" = 20, 
                           "Candidate nomination" = 21, 
                           "Internal cohesion" = 22,
                           "Personalization of party" = 23
                                                      ), 
               selected = "Anti-pluralism Index"
                ), 
           
           
           tags$strong("Question:"),
                   
           textOutput(outputId = "var_question"),
           
           tags$strong("Clarification:"),
           
           textOutput(outputId = "var_clear"),
           
           tags$strong("Values:"),
           
           textOutput(outputId = "var_response"), 
           
           textOutput(outputId = "var_response2"), 
           
           textOutput(outputId = "var_response3"),
           
           textOutput(outputId = "var_response4"), 
           
           textOutput(outputId = "var_response5"), 
           
          #checkboxInput(
          #    inputId = "uncertainty", 
          #    label = "Uncertainty estimates",
          #    value = c(0,1)
          #), 
           
          # checkboxInput(
          #     inputId = "country.mean", 
          #     label = "Country mean",
          #     value = c(0,1)
          # ) 
           
           #checkboxInput(
           #    inputId = "country.mean", 
           #    label = "Regional mean",
           #    value = c(0,1)
           #), 
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
           highchartOutput("hc_plot"),
           #downloadButton(outputId = "down", label = "Download the plot")
        )
        
      
    )),


tabPanel("Party info",
         #p("TBD. this tab will allow the user to explore additional party information such as core support groups, issue salience etc."),
         
       
         sidebarLayout(
             sidebarPanel(
               tags$strong("Explore party positions"), 
               
               p("Choose a party and variable. 
                Click on year in the legend to remove/add observations" 
                ), 
               
                 selectInput(
                     inputId = "country2", 
                     label = "Country", 
                     choices = cnt, 
                     selected = "Zambia"
                 ),
                 
                 selectInput(
                     inputId = "party", 
                     label = "Political party:", 
                     ""
                 ),
                 
                 #selectInput(
                 #    inputId = "year", 
                 #    label = "Election year:", 
                 #    ""
                 #),
                 
                 selectInput(
                     inputId = "variable2", 
                     label = "Select variable:", 
                     choices = c("Party support groups" = 1, 
                                 "Party resources" = 2,
                                 "Salience and mobilization" = 3),
                     selected = "Party support groups"
                 ),
                 
                 tags$strong("Question:"),
                 
                 textOutput(outputId = "var_question_2"),
                 
                 tags$strong("Clarification:"),
                 
                 textOutput(outputId = "var_clear_2"),
                 
                 tags$strong("Values:"), 
                 
                 tags$body("1 = YES; 0 = NO. X-axis denotes the share of experts that chose 1")
                 
                 ),
             
             
             
             mainPanel(
                highchartOutput("myPlot2")#,
                 
                 #plotOutput("myPlot3")
                 )
             )
         ),

tabPanel("Variable comparison",
         
         
        sidebarLayout(
            sidebarPanel(
              tags$strong("Explore bi-variate relationships"), 
              
              p("Pick a variable to display on each axis to display developments over time. 
                Click on parties in the legend to remove/add them.
                Hoover over the points to display the year of each observation"), 
              
                selectInput(
                    inputId = "country3", 
                    label = "Select country", 
                    choices = cnt, 
                    selected = "Zambia"
                ),
                
                selectInput(
                    inputId = "variable3x", 
                    label = "Select variable for X-axis", 
                    choices = c("Anti-pluralism Index" = 1, 
                                "Populism Index" = 2,
                                "Anti-elitism" = 3,
                                "People-centrism" = 4, 
                                "Political opponents" = 5, 
                                "Political pluralism" = 6, 
                                "Minority rights" = 7, 
                                "Rejection of political violence" = 8, 
                                "Immigration" = 9, 
                                "LGBT social equality" = 10, 
                                "Cultural superiority" = 11, 
                                "Religious principles" = 12,
                                "Gender equality" = 13, 
                                "Working women" = 14, 
                                "Economic left-right scale" = 15,
                                "Welfare" = 16, 
                                "Clientelism" = 17,
                                "Local party office" = 18, 
                                "Local organizational strength" = 19, 
                                "Affiliate organizations" = 20, 
                                "Candidate nomination" = 21, 
                                "Internal cohesion" = 22,
                                "Personalization of party" = 23
                    ), 
                    selected = "Anti-pluralism Index"
                ),
                
                selectInput(
                  inputId = "variable3y", 
                  label = "Select variable for Y-axis", 
                  choices = c("Anti-pluralism Index" = 1, 
                              "Populism Index" = 2,
                              "Anti-elitism" = 3,
                              "People-centrism" = 4, 
                              "Political opponents" = 5, 
                              "Political pluralism" = 6, 
                              "Minority rights" = 7, 
                              "Rejection of political violence" = 8, 
                              "Immigration" = 9, 
                              "LGBT social equality" = 10, 
                              "Cultural superiority" = 11, 
                              "Religious principles" = 12,
                              "Gender equality" = 13, 
                              "Working women" = 14, 
                              "Economic left-right scale" = 15,
                              "Welfare" = 16, 
                              "Clientelism" = 17,
                              "Local party office" = 18, 
                              "Local organizational strength" = 19, 
                              "Affiliate organizations" = 20, 
                              "Candidate nomination" = 21, 
                              "Internal cohesion" = 22,
                              "Personalization of party" = 23
                  ), 
                  selected = 2,
                ),
            ),
              
        mainPanel(
          highchartOutput("hc_plot3") 
        )
        )
        )
        
))




         
         

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

    df.1 <- reactive({
        df %>% 
            filter(country_name == input$country) 
    })
    
    outVar <- reactive({
        df %>% 
            filter(country_name == input$country2) %>% 
            distinct(v2paenname) %>% pull()
    })
    
    
    df.2 <- reactive({
        df %>% 
            filter(country_name == input$country2) %>% 
            filter(v2paenname == input$party) #%>% 
            #filter(year == input$year)
    })
    
    #outVar2 <- reactive({
    #    df %>% 
    #        filter(country_name == input$country2) %>% 
    #        filter(v2paenname == input$party) %>% 
    #        distinct(year) %>% pull() %>% rev()
    #})
    
    outVar2_num <- reactive({
        input$variable2
    })
    
    
    observe({
        updateSelectInput(session, "party", choices = outVar()) 
        })
    
    #observe({
    #    updateSelectInput(session, "year", choices = outVar2()) 
    #    })
    
    #variable.to.plot <- reactive({
    #    var_list[as.numeric(input$variable] 
    #})
    
    output$var_clear <- renderText({
        clear[as.numeric(input$variable)]
        
    })
    
    output$var_question_2 <- renderText({
        question_2[as.numeric(input$variable2)]
        
    })
    
    output$var_clear_2 <- renderText({
        clear_2[as.numeric(input$variable2)] #what should this be?
        
    })
    
    output$var_response <- renderText({
        response[as.numeric(input$variable)]
        
    })
    
    output$var_response2 <- renderText({
        response2[as.numeric(input$variable)]
        
    })
    
    output$var_response3 <- renderText({
        response3[as.numeric(input$variable)]
        
    })
    
    
    output$var_response4 <- renderText({
        response4[as.numeric(input$variable)]
        
    })
    
    output$var_response5 <- renderText({
        response5[as.numeric(input$variable)]
        
    })
    
    
    output$var_question <- renderText({
        question[as.numeric(input$variable)]
        
    })
    
  ## For Variable relationship  
    df.3 <- reactive({
      df %>% 
        filter(country_name == input$country3) 
    })
    
    
    
  output$hc_plot <- renderHighchart({
      # filter df and select vars based on input 
      df.1() %>% 
          dplyr::select(v2pashname, v2paenname,party_name, year,
                        v2xpa_antiplural, v2xpa_popul, v2paanteli_osp, v2papeople_osp, v2paopresp_osp, 
                        v2paplur_osp, v2paminor_osp, v2paviol_osp, v2paimmig_osp, v2palgbt_osp, 
                        v2paculsup_osp, v2parelig_osp, v2pagender_osp, v2pawomlab_osp, v2pariglef_osp, 
                        v2pawelf_osp, v2paclient_osp, v2palocoff_osp, v2paactcom_osp, v2pasoctie_osp, 
                        v2panom_osp, v2padisa_osp, v2paind_osp
          ) %>% 
          # fix data structure
          gather(key="variable", value="value", -v2pashname, -v2paenname,-party_name, -year) %>% 
          filter(variable == var_list[as.numeric(input$variable)]) %>% 
          filter(!is.na(value)) %>% 
          # draw the plot
          hchart(., "spline", hcaes(year, value, group = party_name)) %>% 
        hc_xAxis(title = FALSE) %>% 
        hc_yAxis(title = FALSE, min = 0, max = var_max[as.numeric(input$variable)]) 

  }) 
 #
 #output$down <- downloadHandler(
 #  filename = "v-party_plot.png", 
 #  content = function(file){
 #    
 #    png(file)
 #    export_hc(output$hc_plot())
 #    dev.off()
 #  }, 
 #  contentType = "image/png"
 #)
 #
    output$myPlot2 <- renderHighchart({
        # filter df and select vars based on input 
        p1 <- df.2() %>% 
            dplyr::select(v2pashname, party_name, year, country_name, 
                          `Aristocracy`=v2pagroup_1, `Agrarian elites`=v2pagroup_2,
                          `Business elites`=v2pagroup_3, `The military`=v2pagroup_4,
                          `An ethnic/racial group(s)`=v2pagroup_5, `Religious group(s)`=v2pagroup_6,
                          `Local elites/chiefs`=v2pagroup_7,`Urban working class`=v2pagroup_8,
                          `Urban middle class`=v2pagroup_9, `Rural working classes`=v2pagroup_10,
                          `Rural middle classes`=v2pagroup_11, `Regional groups or separatists`=v2pagroup_12,
                          `Women`=v2pagroup_13) %>%
            #only display latest year
            #filter(year == max(year)) %>% #
            gather(key="variable", value="Importance", -v2pashname,-party_name, -year, -country_name) %>%
            arrange(-Importance) %>% 
            hchart(., "scatter", hcaes(x=variable, y = Importance, group= year)) %>% 
            hc_chart(inverted = TRUE) %>% 
            hc_yAxis(title = TRUE, min = 0, max = 1) %>% 
            hc_xAxis(title = TRUE)
      
      
        p2 <- df.2() %>% 
           dplyr::select(v2pashname, year, country_name, `Anti-elitism` = v2pasalie_0, `People-centrism` = v2pasalie_1,
                         `Political pluralism` = v2pasalie_2, `Minority rights` = v2pasalie_3, 
                         `Immigration` = v2pasalie_4, `LGBT social equality` = v2pasalie_5,
                         `Cultural superiority` = v2pasalie_6, `Religious principles` = v2pasalie_7,
                         `Gender equality` = v2pasalie_8, `Welfare` = v2pasalie_9, `Economic issues` = v2pasalie_10,
                         `Clientelism` = v2pasalie_11, `Environmental protection` = v2pasalie_12, 
                         `Farmer's issues` = v2pasalie_13, `The leader` = v2pasalie_14,  
                         `Anti-corruption` =v2pasalie_15, `Intimidation/violence` = v2pasalie_16) %>%
          gather(key="variable", value="Importance", -v2pashname, -year, -country_name) %>%
          arrange(-Importance) %>% 
          hchart(., "scatter", hcaes(x=variable, y = Importance, group= year)) %>% 
          hc_chart(inverted = TRUE) %>% 
          hc_yAxis(title = TRUE, min = 0, max = 1) %>% 
          hc_xAxis(title = TRUE)
        
      p3 <- df.2() %>% 
          dplyr::select(v2pashname, year, country_name, 
                        `Formal state subsidies`=v2pafunds_0, 
                        `Large donations individuals`=v2pafunds_1, 
                        `Large donations companies`=v2pafunds_2, 
                        `Large donations civ. soc. org.`=v2pafunds_3, 
                        `Membership fees`=v2pafunds_4, 
                        `Informal state resources`=v2pafunds_5, 
                        `Funds of the party leader`=v2pafunds_6, 
                        `Funds of candidate`=v2pafunds_7) %>%
           gather(key="variable", value="Importance", -v2pashname, -year, -country_name) %>%
          arrange(-Importance) %>% 
          hchart(., "scatter", hcaes(x=variable, y = Importance, group= year)) %>% 
          hc_chart(inverted = TRUE) %>% 
          hc_yAxis(title = TRUE, min = 0, max = 1) %>% 
          hc_xAxis(title = TRUE)
        
        
        #print the plots
        a <- list(p1, p3, p2)
        
        a[[as.numeric(outVar2_num())]]
        
        #p2
        
        })
    
    
  output$hc_plot3 <- renderHighchart({
      # filter df and select vars based on input 
      df.3() %>% 
        dplyr::select(party_name, v2pashname, v2paenname, year, x= var_list[as.numeric(input$variable3x)],
                      y= var_list[as.numeric(input$variable3y)],
                      v2xpa_antiplural, v2xpa_popul, v2paanteli_osp, v2papeople_osp, v2paopresp_osp, 
                      v2paplur_osp, v2paminor_osp, v2paviol_osp, v2paimmig_osp, v2palgbt_osp, 
                      v2paculsup_osp, v2parelig_osp, v2pagender_osp, v2pawomlab_osp, v2pariglef_osp, 
                      v2pawelf_osp, v2paclient_osp, v2palocoff_osp, v2paactcom_osp, v2pasoctie_osp, 
                      v2panom_osp, v2padisa_osp, v2paind_osp
        ) %>% 
        hchart(., "line", hcaes(x= x, y= y, group = party_name)) %>% 
       # hc_add_series("scatter") %>% 
        hc_yAxis(title = list(text= var_names[as.numeric(input$variable3y)]),
                 min = 0, max = var_max[as.numeric(input$variable3y)]) %>% 
        hc_xAxis(title = list(text= var_names[as.numeric(input$variable3x)]),
                 min = 0, max = var_max[as.numeric(input$variable3x)]) %>% 
      hc_tooltip(formatter = JS("function(){
                            return (this.point.v2pashname + ' <br>' + this.point.year)
                            }"))
      
      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
