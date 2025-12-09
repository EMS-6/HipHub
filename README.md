# HipHub
Hip Health educational tool

# The Hip Hub: Interactive R/Shiny Dashboard

An interactive, evidence-informed dashboard for hip health built with **R/Shiny**, **bs4Dash**, **Plotly**, and **RMarkdown**. It centralizes hip anatomy visuals, prevalence exploration by age and sex, and guideline-backed content for prevention, intervention, and assessment.

## Features
- **Home**: Uniform hip anatomy images in a 2×2 grid.
- **Common Pathologies**: Age × sex filters drive an interactive Plotly bar chart and a matching image.
- **Prevention & Maintenance**: Exercise, mobility, and self-management guidance with citations.
- **Intervention Options**: Impairment-based phases and realistic post-arthroscopy recovery timelines.
- **Assessment**: Clinical tests (FADIR, Arlington, Twist) and imaging workflow (XR → MRA → CT).
- **Documentation**: Auto-rendered `info.Rmd` → `info.html` and an in-app References card.

## Quick Start
```r
# Install dependencies
install.packages(c("shiny", "bs4Dash", "plotly", "rmarkdown"))

# Run the app from the project root
shiny::runApp("app.R")
```

## File Structure
```
project/
├── app.R
├── info.Rmd
├── www/
│   ├── hip_anatomy.png
│   ├── hip_anatomy2.png
│   ├── hip_anatomy3.png
│   ├── hip_anatomy_4.jpeg
│   ├── rom_distribution.png
│   ├── rehab_pain_score.png
│   ├── hip_strength.png
│   ├── fai_subtype_distribution.png
│   └── pathologies/
│       ├── Adolescent_Male_pathology.png
│       ├── Adolescent_Female_pathology.png
│       ├── Adult_Male_pathology.png
│       ├── Adult_Female_pathology.png
│       ├── Older_Adult_Male_pathology.png
│       └── Older_Adult_Female_pathology.png
```

## Assets & Naming
- Home images (root `www/`): `hip_anatomy.png`, `hip_anatomy2.png`, `hip_anatomy3.png`, `hip_anatomy_4.jpeg`.
- Pathology images (in `www/pathologies/`): Age × sex pattern files as listed above.

## Why This Matters
Core care for hip/knee OA emphasizes **exercise**, **education**, and **weight management** (ACR/AF, 2019; OARSI, 2019). Hip OA prevalence rises with age globally (Fan et al., 2023), and U.S. burden has increased notably since 1990 (Sayyed et al., 2025). This dashboard makes those insights approachable and interactive.

## References
- ACR/AF Guideline (2019): <https://rheumatology.org/osteoarthritis-guideline>
- OARSI Guideline (2019): <https://www.esceo.org/sites/esceo/files/pdf/Bannuru_O%26C_OARSIguidelines_2019.pdf>
- Fan et al., 2023: <https://link.springer.com/content/pdf/10.1186/s13075-023-03033-7.pdf>
- Sayyed et al., 2025: <https://bmjopen.bmj.com/content/15/10/e096130>
- Heerey et al., 2018: <https://www.jospt.org/doi/pdf/10.2519/jospt.2018.8002>
- Grzybowski et al., 2015: <https://www.frontiersin.org/journals/surgery/articles/10.3389/fsurg.2015.00021/full>
- Ramos et al., 2020: <https://journals.sagepub.com/doi/pdf/10.1177/2325967120960689>
- Lutz et al., 2025: <https://academic.oup.com/jhps/article/12/Supplement_1/i125/8099723>
- Zhou et al., 2021: <https://academic.oup.com/jhps/article/8/3/233/6307732>
- Owen et al., 2023: <https://europepmc.org/article/PMC/PMC10403993>
- Physiopedia: <https://www.physio-pedia.com/FADIR_%28Flexion,_Adduction,_Internal_Rotation%29_Test>
