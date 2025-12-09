library(shiny)
library(bs4Dash)
library(plotly)
library(rmarkdown)



if (!file.exists("info.html") && file.exists("info.Rmd")) {
  rmarkdown::render(
    "info.Rmd",
    output_format = "html_document",
    output_file   = "info.html"
  )
}

conditions  <- c("FAI", "OA", "Dysplasia", "Labral Tear", "Bursitis")
age_groups  <- c("Adolescent", "Adult", "Older Adult")
sexes       <- c("Male", "Female")

prevalence_data <- expand.grid(
  AgeGroup  = age_groups,
  Sex       = sexes,
  Condition = conditions
)
set.seed(5430)
prevalence_data$Prevalence <- sample(5:30, nrow(prevalence_data), replace = TRUE)

age_sex_to_filename <- function(age, sex) {
  paste0("pathologies/", gsub(" ", "_", age), "_", sex, "_pathology.png")
}
file_exists_in_www <- function(rel_path) {
  file.exists(file.path("www", rel_path))
}
img_if_exists <- function(rel_path, height = NULL, style = NULL) {
  if (!file_exists_in_www(rel_path)) {
    return(div(
      style = "color:#ff6b6b; padding:8px;",
      paste("Image not found in ./www/:", rel_path)
    ))
  }

  if (is.null(height)) height <- "280px"
  if (is.null(style))  style  <- "display:block;margin:auto;max-width:100%;height:auto;"
  tags$img(src = rel_path, height = height, style = style)
}


use_home_folder <- dir.exists("www/home")
home_images <- if (use_home_folder) {
  imgs <- list.files("www/home", pattern = "\\.(png|jpg|jpeg|gif)$", ignore.case = TRUE)
  file.path("home", imgs)
} else {
  c()
}



# UI

ui <- bs4DashPage(
  title = "The Hip Hub",
  skin  = "dark",
  
  header = bs4DashNavbar(title = "The Hip Hub"),
  
  sidebar = bs4DashSidebar(
    skin = "dark",
    sidebarMenu(
      menuItem("Home",                tabName = "home",        icon = icon("home")),
      menuItem("Common Pathologies",  tabName = "pathologies", icon = icon("heartbeat")),
      menuItem("Prevention & Maintenance", tabName = "prevention",  icon = icon("dumbbell")),
      menuItem("Intervention Options",    tabName = "intervention", icon = icon("medkit")),
      menuItem("Assessment",              tabName = "assessment",  icon = icon("stethoscope")),
      menuItem("Documentation",           tabName = "markdown",    icon = icon("file-alt"))
    )
  ),
  
  body = bs4DashBody(
    
    tags$head(tags$style(HTML("
      body { font-size: 18px; line-height: 1.65; }
      .card { border-radius: 12px; }
      .card-header { font-weight: 700; font-size: 20px; letter-spacing: 0.3px; }
      .card-body img { max-width: 100%; height: auto; }
      .hiphub-list li { margin-bottom: 6px; }
      .card-body .plotly html-widget, .card-body .js-plotly-plot { width: 100% !important; }
    "))),
    tabItems(
      
      # ---------------- Home ----------------
      tabItem(tabName = "home",
              fluidRow(
                bs4Card(
                  title = "Welcome to The Hip Hub",
                  width = 12,
                  uiOutput("homeGallery"),
                  p("A comprehensive interactive dashboard synthesizing hip anatomy, pathology, rehab, assessment, and current literature."),
                  p("Use the sidebar to navigate between sections."),
                  hr(),
                  h4("Hip Imaging Overview"),
                  uiOutput("hipImagingGallery") 
                )
              )
      ),
      
      # ------------- Common Pathologies -------------
      tabItem(tabName = "pathologies",
              fluidRow(
                bs4Card(title = "Filters", width = 12,
                        fluidRow(
                          column(6, selectInput("path_age", "Select Age Group", choices = age_groups, selected = "Adult")),
                          column(6, selectInput("path_sex", "Select Sex", choices = sexes, selected = "Female"))
                        )
                )
              ),
              fluidRow(
                bs4Card(title = "Prevalence Plot", width = 6,
                        plotlyOutput("filteredPrevalencePlot")
                ),
                bs4Card(title = "Age × Sex Prevalence Image", width = 6,
                        uiOutput("ageSexPrevalenceImage")
                )
              ),
              fluidRow(
                bs4Card(title = "Evidence Highlights", width = 12,
                        tags$ul(class = "hiphub-list",
                                tags$li(HTML(
                                  'Cam-type FAI more common in males; pincer-type more frequent in females; mixed type prevalent in both. 
                 <a href="https://academic.oup.com/jhps/article/8/3/233/6307732" target="_blank">Zhou et al., 2021</a>'
                                )),
                                tags$li(HTML(
                                  'Meta-analysis (>213k hips): males show more cam/mixed; females more pincer; both sexes exceed MCID 1–5 years post-op. 
                 <a href="https://europepmc.org/article/PMC/PMC10403993" target="_blank">Owen et al., 2023</a> · 
                 <a href="https://www.orthobullets.com/evidence/37547081" target="_blank">Orthobullets</a>'
                                )),
                                tags$li(HTML(
                                  'Global radiographic hip OA prevalence (K-L ≥2) ≈ 8.55%, increases with age, varies by region. 
                 <a href="https://link.springer.com/content/pdf/10.1186/s13075-023-03033-7.pdf" target="_blank">Fan et al., 2023</a>'
                                )),
                                tags$li(HTML(
                                  'U.S. hip OA burden rose ~24–25% (1990–2019). 
                 <a href="https://bmjopen.bmj.com/content/15/10/e096130" target="_blank">Sayyed et al., 2025</a>'
                                ))
                        )
                )
              )
      ),
      
      # ---------------- Prevention ----------------
      tabItem(tabName = "prevention",
              fluidRow(
                bs4Card(title = "Recommended Exercises", width = 6,
                        tags$ul(class = "hiphub-list",
                                tags$li(HTML('Glute bridges & hip abduction strength (GMed/TFL)—progress isometric ➜ banded/DB; emphasize pelvic control. 
                <a href="https://www.jospt.org/doi/pdf/10.2519/jospt.2018.8002" target="_blank">Heerey et al., 2018</a>')),
                                tags$li(HTML('Hip flexion/extension mobility 3–5×/week; pair with neuromuscular control drills. 
                <a href="https://dash.harvard.edu/bitstreams/7312037e-93aa-6bd4-e053-0100007fdf3b/download" target="_blank">Anderson & Madigan, 2014</a>')),
                                tags$li(HTML('Aerobic activity ≥150 min/week; add balance/functional drills. 
                <a href="https://rheumatology.org/osteoarthritis-guideline" target="_blank">ACR/AF 2019</a> · 
                <a href="https://www.esceo.org/sites/esceo/files/pdf/Bannuru_O%26C_OARSIguidelines_2019.pdf" target="_blank">OARSI 2019</a>'))
                        )
                ),
                bs4Card(title = "Lifestyle Factors", width = 6,
                        tags$ul(class = "hiphub-list",
                                tags$li(HTML('Education & self‑management programs (trackers, CBT modules) improve pain/function. 
                <a href="https://www.aafp.org/pubs/afp/issues/2021/0115/p120.html" target="_blank">AAFP 2021</a>')),
                                tags$li(HTML('Weight management: even ~5% loss reduces hip/knee OA pain and improves function. 
                <a href="https://www.aafp.org/pubs/afp/issues/2021/0115/p120.html" target="_blank">AAFP 2021</a>')),
                                tags$li(HTML('Activity pacing & ergonomics (sit‑stand variation, hip‑friendly lifting, stair strategies). 
                <a href="https://oarsi.org/education/oarsi-guidelines" target="_blank">OARSI summary</a>'))
                        )
                )
              ),
              fluidRow(
                bs4Card(title = "ROM & Strength Norms", width = 12, 
                        img_if_exists("rom_distribution.png", height = "360px",
                                      style = "display:block;margin:auto;max-width:100%;height:auto;")
                )
              ),
              fluidRow(
                bs4Card(title = "Evidence Highlights", width = 12,
                        HTML('Core care: exercise, education, weight management. 
                 <a href="https://rheumatology.org/osteoarthritis-guideline" target="_blank">ACR/AF 2019</a> · 
                 <a href="https://www.esceo.org/sites/esceo/files/pdf/Bannuru_O%26C_OARSIguidelines_2019.pdf" target="_blank">OARSI 2019</a> · 
                 <a href="https://www.aafp.org/pubs/afp/issues/2021/0115/p120.html" target="_blank">AAFP 2021</a>')
                )
              )
      ),
      
      # ---------------- Intervention ----------------
      tabItem(tabName = "intervention",
              fluidRow(
                bs4Card(title = "Conservative Treatments", width = 6,
                        tags$ul(class = "hiphub-list",
                                tags$li(HTML('Impairment‑based phases (load mgmt ➜ mobility ➜ strength ➜ RTS). 
                <a href="https://www.jospt.org/doi/pdf/10.2519/jospt.2018.8002" target="_blank">Heerey et al., 2018</a>')),
                                tags$li(HTML('Exercise & tai chi as first‑line symptom management. 
                <a href="https://rheumatology.org/osteoarthritis-guideline" target="_blank">ACR/AF 2019</a>')),
                                tags$li(HTML('Short‑term analgesia (oral/topical NSAIDs) & intra‑articular corticosteroids for flares (clinical judgment). 
                <a href="https://www.aafp.org/pubs/afp/issues/2021/0115/p120.html" target="_blank">AAFP 2021</a>'))
                        )
                ),
                bs4Card(title = "Surgical Options", width = 6,
                        tags$ul(class = "hiphub-list",
                                tags$li("Hip arthroscopy (FAI/labral repair)—consider morphology, symptoms, goals."),
                                tags$li(HTML('Post‑op trajectory: pain/function improve over weeks to months; activity often plateaus by ~7 months. 
                <a href="https://academic.oup.com/jhps/article/12/Supplement_1/i125/8099723" target="_blank">Lutz et al., 2025</a> · 
                <a href="https://journals.sagepub.com/doi/pdf/10.1177/2325967120960689" target="_blank">Ramos et al., 2020</a>')),
                                tags$li("Arthroplasty for end‑stage OA (shared decision‑making).")
                        )
                )
              ),
              fluidRow(
                bs4Card(title = "Rehabilitation Protocols", width = 12,  
                        img_if_exists("rehab_pain_score.png", height = "360px",
                                      style = "display:block;margin:auto;max-width:100%;height:auto;")
                )
              ),
              fluidRow(
                bs4Card(title = "Evidence Highlights", width = 12,
                        tags$ul(class = "hiphub-list",
                                tags$li(HTML('Phase‑based rehab is standard; reporting historically limited. 
                <a href="https://www.frontiersin.org/journals/surgery/articles/10.3389/fsurg.2015.00021/full" target="_blank">Grzybowski et al., 2015</a>')),
                                tags$li(HTML('Strong emphasis on exercise, education, self‑management across major guidelines. 
                <a href="https://rheumatology.org/osteoarthritis-guideline" target="_blank">ACR/AF 2019</a> · 
                <a href="https://www.esceo.org/sites/esceo/files/pdf/Bannuru_O%26C_OARSIguidelines_2019.pdf" target="_blank">OARSI 2019</a>'))
                        )
                )
              )
      ),
      
      # ---------------- Assessment ----------------
      tabItem(tabName = "assessment",
              fluidRow(
                bs4Card(title = "Clinical Tests", width = 12,
                        tags$ul(class = "hiphub-list",
                                tags$li(HTML('FADIR (screening; high sensitivity, limited specificity). 
                <a href="https://www.physio-pedia.com/FADIR_%28Flexion,_Adduction,_Internal_Rotation%29_Test" target="_blank">Physiopedia</a>')),
                                tags$li(HTML('Arlington (higher sensitivity) & Twist (higher specificity) complement FADIR. 
                <a href="https://www.nirschl.com/pdf/two-novel-clinical-tests-for-diagnosis-of-hip-labral-tears.pdf" target="_blank">Adib et al., 2023</a>')),
                                tags$li("FABER, Scour, Thomas (supportive cluster; interpret with imaging and history).")
                        )
                ),
                bs4Card(title = "Strength Testing", width = 12,
                        img_if_exists("hip_strength.png", height = "360px",
                                      style = "display:block;margin:auto;max-width:100%;height:auto;")
                )
              ),
              fluidRow(
                bs4Card(title = "Imaging Modalities", width = 12,
                        tags$ul(class = "hiphub-list",
                                tags$li("Radiographs (AP pelvis/45° Dunn) for morphology/bony features."),
                                tags$li(HTML('MR arthrography for suspected labral pathology. 
                <a href="https://www.physio-pedia.com/FADIR_%28Flexion,_Adduction,_Internal_Rotation%29_Test" target="_blank">Overview</a>')),
                                tags$li("CT for 3D recon—version, coverage, torsion (surgical planning).")
                        )
                )
              ),
              fluidRow(
                bs4Card(title = "FAI Subtype Distribution by Sex", width = 12,
                        img_if_exists("fai_subtype_distribution.png", height = "220px", 
                                      style = "display:block;margin:auto;max-width:100%;height:auto;")
                )
              )
      ),
      
      # ---------------- Documentation ----------------
      tabItem(tabName = "markdown",
              bs4Card(title = "Documentation", width = 12,
                      collapsible = TRUE, collapsed = TRUE,
                      uiOutput("docHtml")
              ),
              bs4Card(title = "References", width = 12,
                      tags$ol(
                        tags$li(a("Owen MM et al., 2023 (Europe PMC)", href="https://europepmc.org/article/PMC/PMC10403993", target="_blank")),
                        tags$li(a("Orthobullets summary of Owen et al., 2023", href="https://www.orthobullets.com/evidence/37547081", target="_blank")),
                        tags$li(a("Zhou et al., 2021 — JHPS", href="https://academic.oup.com/jhps/article/8/3/233/6307732", target="_blank")),
                        tags$li(a("Fan et al., 2023 — Arthritis Research & Therapy (PDF)", href="https://link.springer.com/content/pdf/10.1186/s13075-023-03033-7.pdf", target="_blank")),
                        tags$li(a("ACR/AF Guideline (2019)", href="https://rheumatology.org/osteoarthritis-guideline", target="_blank")),
                        tags$li(a("AAFP synopsis of ACR/AF guideline", href="https://www.aafp.org/pubs/afp/issues/2021/0115/p120.html", target="_blank")),
                        tags$li(a("OARSI guideline (2019) PDF", href="https://www.esceo.org/sites/esceo/files/pdf/Bannuru_O%26C_OARSIguidelines_2019.pdf", target="_blank")),
                        tags$li(a("OARSI guideline overview", href="https://oarsi.org/education/oarsi-guidelines", target="_blank")),
                        tags$li(a("Heerey et al., 2018 — JOSPT", href="https://www.jospt.org/doi/pdf/10.2519/jospt.2018.8002", target="_blank")),
                        tags$li(a("Grzybowski et al., 2015 — Frontiers in Surgery", href="https://www.frontiersin.org/journals/surgery/articles/10.3389/fsurg.2015.00021/full", target="_blank")),
                        tags$li(a("Ramos et al., 2020 — OJSM", href="https://journals.sagepub.com/doi/pdf/10.1177/2325967120960689", target="_blank")),
                        tags$li(a("Lutz et al., 2025 — JHPS", href="https://academic.oup.com/jhps/article/12/Supplement_1/i125/8099723", target="_blank")),
                        tags$li(a("Physiopedia — FADIR overview", href="https://www.physio-pedia.com/FADIR_%28Flexion,_Adduction,_Internal_Rotation%29_Test", target="_blank"))
                      )
              )
      )
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  # Home gallery
  output$homeGallery <- renderUI({
    imgs <- home_images
    if (!use_home_folder && length(imgs) == 0) {
      return(div(
        style = "padding:8px;",
        tags$b("Add your four Home images:"),
        tags$ol(
          tags$li("EITHER place them under ./www/home/ (recommended) — they will auto-load;"),
          tags$li("OR list their exact filenames in the code under `home_images`.")
        )
      ))
    }
    cards <- lapply(imgs, function(rel) {
      bs4Card(width = 6,
              img_if_exists(rel, height = "240px",  
                            style = "display:block;margin:auto;max-width:100%;height:auto;")
      )
    })
    do.call(fluidRow, cards)
  })
  
  # Hip imaging gallery
  output$hipImagingGallery <- renderUI({
    imgs <- c("hip_anatomy.png",
              "hip_anatomy2.png",
              "hip_anatomy3.png",
              "hip anatomy_4.png")

    imgs[4] <- if (file.exists(file.path("www", "hip anatomy_4.png"))) "hip anatomy_4.png" else
      if (file.exists(file.path("www", "hip_anatomy_4.jpeg"))) "hip_anatomy_4.jpeg" else "hip anatomy_4.png"
    
    cards <- lapply(imgs, function(rel) {
      if (!file.exists(file.path("www", rel))) {
        return(div(style = "color:#ff6b6b; padding:8px;", paste("Image not found in ./www/:", rel)))
      }
      bs4Card(width = 6,
              tags$img(src = rel, height = "240px",  # UNIFORM SIZE for hip anatomy images
                       style = "display:block;margin:auto;max-width:100%;height:auto;"),
              footer = tags$small(rel)
      )
    })
    do.call(fluidRow, cards)
  })
  
  output$docHtml <- renderUI({
    if (file.exists("info.html")) {
      includeHTML("info.html")
    } else {
      div("Add info.Rmd to your project root. It will render to info.html on first run.")
    }
  })
  
  filteredPathData <- reactive({
    req(input$path_age, input$path_sex)
    subset(prevalence_data,
           AgeGroup == input$path_age & Sex == input$path_sex)
  })
  
  output$filteredPrevalencePlot <- renderPlotly({
    plot_ly(filteredPathData(),
            x = ~Condition, y = ~Prevalence,
            type = "bar", color = ~Condition) %>%
      layout(
        title = paste("Prevalence of Hip Pathologies in", input$path_age, input$path_sex, "s"),
        yaxis = list(title = "Prevalence (%)")
      )
  })
  
  output$ageSexPrevalenceImage <- renderUI({
    req(input$path_age, input$path_sex)
    rel <- age_sex_to_filename(input$path_age, input$path_sex)
    if (!file_exists_in_www(rel)) {
      return(div(
        style = "color:#ff6b6b; padding:8px;",
        paste("Image not found. Expected:", file.path("www", rel)),
        br(),
        "Ensure all six files exist under ./www/pathologies/:",
        tags$ul(
          tags$li("Adolescent_Male_pathology.png"),
          tags$li("Adolescent_Female_pathology.png"),
          tags$li("Adult_Male_pathology.png"),
          tags$li("Adult_Female_pathology.png"),
          tags$li("Older_Adult_Male_pathology.png"),
          tags$li("Older_Adult_Female_pathology.png")
        )
      ))
    }
    tags$img(src = rel, height = "260px",
             style = "display:block;margin:auto;max-width:100%;height:auto;")
  })
}

# Run App
shinyApp(ui, server)
