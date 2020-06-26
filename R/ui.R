
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

ui <- function(){
  
  bootstrapPage('',
                
                tags$style(type = 'text/css',
                           HTML('.navbar {background-color: #79aaaa; font-size: 18px;}
                           .navbar-default .navbar-brand {color: #ffffff; font-size: 20px;}
                           .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {color: #000000; background-color: #8fcbcc;}
                           .well {background-color: #d6e6eb;}'
                           )
                ),
                
                navbarPage(title = "PhenoSpace", id = "tabset", 
                           tabPanel("Home page",
                                    
                                    fluidRow(column(width = 6, offset = 3,
                                                    wellPanel(align = "justify",
                                                              HTML("<h1 align = 'center'>PhenoSpace <i>1.0.0</i> </h1>"),
                                                              br(),
                                                              HTML("<p>PhenoSpace is a shiny application aiming at visualizing the position of any 
                                                                    individual/population/species in the phenotypic space of the 'Global Spectrum of 
                                                                   Plant Form and Function' -
                                                                   <a target='_blank' rel='noopener noreferrer' href='http://doi.org/10.1038/nature16489'> Díaz <i>et al.</i> (2016) <i>Nature</i> 529, 167-171</a>.</p>"),
                                                              p("PhenoSpace has three main functionalities: 
                                                                  (1) the visualization of various avatars of the phenotypic space of the global spectrum
                                                                    using different combinations of traits and/or growth forms 
                                                                    selected from the original data set, 
                                                                    (2) to project trait data from any user-defined data set onto the phenotypic space 
                                                                    of the global spectrum, provided that at least two of the six traits are available,
                                                                    and (3) to download figures and data coordinates produced by the application.
                                                                "),
                                                              fluidRow(actionButton("app", "Access the application"), align = "center"),
                                                              img(src = "plante.png", width = '100%', height = "auto")
                                                    )
                                    )
                                    )
                           ),
                           tabPanel("Shiny application", value = "app",
                                    fluidPage(
                                      p("Visualizing data in the phenotypic space of the 
                                          'Global Spectrum of Plant Form and Function' - Nature 529, 167-171 (2016)"),
                                      fluidRow(
                                        column(width = 4,
                                               tabsetPanel(
                                                 tabPanel("Customize the PCA",
                                                          p(""),
                                                          h4("Selection of traits, growth forms and components"),
                                                          HTML("<p>Dynamic visualization of the principal components
                                                with a customized selection of traits and growth forms using 
                                                the original data set analyzed in Díaz <i>et al.</i> (2016)</p>"),
                                                          wellPanel(
                                                            fluidRow(
                                                              column(width = 6,
                                                                     h4("Traits:"),
                                                                     checkboxInput('LA', 'Leaf area (LA)', TRUE),
                                                                     checkboxInput('Nmass', 'Leaf nitrogen content per unit mass (Nmass)', TRUE),
                                                                     checkboxInput('LMA', 'Leaf mass per area (LMA)', TRUE),
                                                                     checkboxInput('H', 'Plant height (H)', TRUE),
                                                                     checkboxInput('SM', 'Diaspore mass (SM)', TRUE),
                                                                     checkboxInput('SSD', 'Stem specific density (SSD)', TRUE)
                                                              ),
                                                              column(width=6,
                                                                     h4("Growth Forms:"),
                                                                     checkboxInput('herb', 'Herbs (n=1166)', TRUE),
                                                                     checkboxInput('shrub', 'Shrubs (n=173)', TRUE),
                                                                     checkboxInput('tree', 'Trees (n=846)', TRUE),
                                                                     checkboxInput('other', 'Others (n=29)', TRUE)
                                                              )),
                                                            fluidRow(
                                                              h4("Components:"),
                                                              column(width = 6,
                                                                     selectInput("axis1", "Axis 1:", choices = c("PC1","PC2", "PC3", "PC4", "PC5"))
                                                              ),
                                                              column(width = 6,
                                                                     selectInput("axis2", "Axis 2:", choices = c("PC2","PC3", "PC4", "PC5", "PC6"))
                                                              )
                                                            )
                                                          ),
                                                          h4("Density areas and species identification"),
                                                          p("Customize density areas according to several criteria and
                                                            indentify a set of species within the customized PCA"),
                                                          wellPanel(
                                                            fluidRow(
                                                              h4("Colored area"),
                                                              selectInput("theme", "", choices = c("All species", "Growth forms", "Herbs only", "Shrubs only", "Trees only")),
                                                              h4("Find a species point:"),
                                                              column(textInput("find", "Type the first letters:", ""), width=6),
                                                              conditionalPanel(
                                                                condition = "input.find != ''",
                                                                column(selectInput("species", "Select a species name:", choices=c("NA")), width=6)
                                                              ),
                                                              conditionalPanel(
                                                                condition = "output.sp != ''",
                                                                column(actionButton("reset", "Clear all"), width = 12, align = "right")
                                                              )
                                                            ))
                                                 ),
                                                 tabPanel("Project your data",
                                                          p(""),
                                                          p("An extra dataset can be projected in the PCA by uploading a csv file."),
                                                          p("The file can contain values for 2 to 6 traits (one column for each trait) and missing values should be coded NA."),
                                                          p("Column names must be H for Plant height, SSD for Stem specific density,
                                              LA for Leaf area, LMA for Leaf mass per area, Nmass for Mass-based leaf 
                                              nitrogen content and SM for Diaspore mass."),
                                                          HTML("<p> The traits values must be expressed in m (H), mg.mm<sup>-3</sup> (SSD), mm<sup>2</sup> (LA), g.m<sup>-2</sup> (LMA), mg.g<sp>-1</sup> (Nmass) and mg (SM).</p>"),
                                                          p("The csv file can also contain one or several extra columns with qualitative or quantitative values
                                              identified with different colours in the figure."),
                                                          HTML("<p> An example file containing 5 traits for 55 species can be downloaded <a target='blank' href='example.csv'>here</a>. </p>"),
                                                          wellPanel(
                                                            fileInput('file1', 'Choose CSV File',
                                                                      accept=c('text/csv', 
                                                                               'text/comma-separated-values,text/plain', 
                                                                               '.csv')),
                                                            htmlOutput("upload"),
                                                            hr(),
                                                            fluidRow(column(radioButtons('sep', 'Separator',
                                                                                         c(Comma=',',
                                                                                           Semicolon=';',
                                                                                           Tab='\t'),
                                                                                         ','),width = 6),
                                                                     column(radioButtons("dec", "Decimal",
                                                                                         c(Dot='.',
                                                                                           Comma=','),
                                                                                         "."),width = 6)),
                                                            selectInput("colors", "Colors :", 
                                                                        choices=c("NA")
                                                            )
                                                          )
                                                 ),
                                                 tabPanel("Downloads",
                                                          h4("Download the figure:"),
                                                          wellPanel(
                                                            fluidRow(
                                                              column(width = 6,
                                                                     textInput("name_plot", "File name: ", "PhenoSpace"),
                                                                     column(width = 6, 
                                                                            textInput("w_plot", "Width: ", 22),
                                                                            selectInput("unit_plot", "Unit: ", choices=c("cm", "inch"))
                                                                     ),
                                                                     column(width = 6, 
                                                                            textInput("h_plot", "Height: ", 26),
                                                                            conditionalPanel(
                                                                              condition = "input.format_plot == 'png'",
                                                                              textInput("res_plot", "Res (dpi): ", 300)
                                                                            )
                                                                     )
                                                              ),
                                                              column(width = 6,
                                                                     selectInput("format_plot", "Format: ", choices=c("png", "pdf", "svg")),
                                                                     radioButtons('selec_plot', 'Selection',
                                                                                  c('Boxplots and PCA'='All', 'PCA only'='PCA only'), 'All')
                                                              )
                                                            ),
                                                            fluidRow(align = "center", downloadButton("downplot", label = "Download", class = NULL))
                                                          ),
                                                          h4("Download the coordinates of your data set on the PCA:"),
                                                          wellPanel(
                                                            fluidRow(
                                                              textInput("name_csv", "File name: ", "PhenoSpace"),
                                                              column(radioButtons('sep_csv', 'Separator',
                                                                                  c(Semicolon=';',
                                                                                    Comma=',',
                                                                                    Tab='\t'),
                                                                                  ';'),width = 6),
                                                              column(radioButtons("dec_csv", "Decimal",
                                                                                  c(Dot='.',
                                                                                    Comma=','),
                                                                                  "."),width = 6)
                                                            ),
                                                            conditionalPanel(
                                                              condition = "output.file",
                                                              fluidRow(align = "center", downloadButton("downcsv", label = "Download", class = NULL))
                                                            ),
                                                            conditionalPanel(
                                                              condition = "!output.file",
                                                              p("This feature is enabled when a dataset containing at least three traits is succesfully uploaded in the 'Project your data' tab.")
                                                            )
                                                          )
                                                 ))),
                                        column(width = 7,
                                               conditionalPanel(
                                                 condition = "!output.meme",
                                                 plotOutput('PCAPlot',height = 700),
                                                 htmlOutput("sp")
                                               ),
                                               conditionalPanel(
                                                 condition = "output.meme",
                                                 h1("Error, please reconsider your panel selections", align = "center", style="margin:150px 0px 10px 0px"),
                                                 img(src = "meme.jpg", width = '300px', height = "auto"),
                                                 align = "center"
                                               )
                                        )
                                      )    
                                    )
                           ),
                           tabPanel("About",
                                    
                                    fluidRow(column(width = 6, offset = 3,
                                                    wellPanel(align = "justify",
                                                              HTML("<h1 align = 'center'>PhenoSpace <i>1.0.0</i> </h1>"),
                                                              p("22/05/2020", align = "center"),
                                                              HTML("<p align = 'center'><img src = 'GitHub.png' width = '20px' height = 'auto'> <a target='_blank' rel='noopener noreferrer' href='https://github.com/jsegrestin/phenospace'> We are on GitHub </a></p>"),
                                                              HTML("<p><b>Cite the application:</b> Segrestin, J., Sartori, K., Navas, M.-L., Kattge, J., Díaz, S., Garnier, É. (2020) PhenoSpace: a Shiny application to visualize trait data in the phenotypic space of the Global Spectrum of Plant Form and Function </p>")
                                                    )
                                    )
                                    )
                           )
                )
  )
}