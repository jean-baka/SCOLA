#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# "global" variable for currencies
currencies <- readr::read_csv("currency-codes-clean-list.csv", show_col_types = F)

# hardcoded parameters
Qmin = 3000 ; Qmax = 23000
seuil_progressivite_CPS = 80 # points de pourcentage de quotité
points_de_CPS = 7 # modifiable via un numericInput
seuil_warning_pat_immo = 200000
seuil_warning_pat_mobi = 100000


# I am a bit unhappy with the following. There remains a horizontal line at the top of the table.
cote_a_cote <- function(a,b) {
  # warning!! in Bootstrap, we *have to* specify the class "table" for all HTML5 tables,
  # otherwise we can't style them properly in the CSS.
  tags$table(tags$tr(tags$td(a), tags$td(b)), class = "table", class = "table-borderless")
}

format_curr <- function(x) {
  base::format(round(x,2), scientific = F, big.mark = " ") # rounding and preventing scientific notation U+202F est une petite espace insécable
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(), # we use ShinyJS to enable deactivation of widgets
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")),
  
  # Application title
  titlePanel(HTML("SCOLA<em>!</em>"), windowTitle = "SCOLA!"),
  
  # main design: several tabs 
  tabsetPanel(
    tabPanel("Paramètres pays",
             br(),
             "Les revenus, charges et frais de scolarité seront à remplir en monnaie locale. Que votre poste soit ou non en zone euro, si vous voulez remplir tout le dossier en euros, choisissez l'euro comme monnaie locale (auquel cas, le taux de chancellerie égal à 1).",
             br(), br(),
             "Attention : l'IPPA est à renseigner dans tous les cas. Attention également à bien entrer le taux de chancellerie \"dans le bon sens\".",
             br(), br(),
             selectInput("monnaie", "Monnaie d'appel des frais de scolarité (monnaie locale)", choices = currencies$full_name, selected = "EUR – Euro"),
             
             # we will "wake up" the taux de chancellerie input only when the monnaie locale is non-EURO
             div(disabled(numericInput("tauxChanc", "Taux de chancellerie (au 16 septembre 2023 pour la campagne de bourses 2024-2025)", value = 1)), class = "inline"),
             div(textOutput("blurb_tauxChanc"), class = "inline"),
             br(),
             div(numericInput("IPPA",
                          "Indice de parité de pouvoir d'achat (IPPA, base 100 = Paris)",
                          min = 1, max = 1000, step = 1, value = 100), class = "inline"), div("L'IPPA est calculé par la DFAE sur la base de l'indice de coût de la vie calculé par l'agence Mercer (70% du poids de l'indice porte sur un panier de consommation et de services, et 30% sur le coût du logement) pour la ville chef-lieu de votre circonscription. Demandez à votre poste quelle est la valeur de l'indice en vigueur.", class = "inline"),
             
             br()
    ),
    
    
    
    
    tabPanel("Situation familiale",
             br(),
             tags$table(tags$tr(tags$td(radioButtons("type_famille", "Famille", choices = c("monoparentale", "biparentale"), selected = "biparentale")), tags$td("Désormais, le parent d'une famille monoparentale \"vaut\" autant (2 parts) que la somme des deux parents d'une famille biparentale."))),
             numericInput("enfants_charge", "Nombre d'enfants à charge (de moins de 25 ans et sans ressources propres)", value = 1, min = 0, max = 30, step = 1),
             div(numericInput("enfants_handicapes", "Dont enfants handicapés (avec reconnaissance d'invalidité par la MDPH ou bien par les autorités locales + consulat)", value = 0, min = 0, max = 30, step = 1), class = "inline"), div("Chaque enfant handicapé donne droit à une demi-part supplémentaire.", class = "inline"),
             
             br()
             ),
    
    
    
    
    tabPanel("Patrimoine",
             br(),
             "Toutes les informations de patrimoine sont à entrer ci-dessous", strong("en euros."),
             br(), br(),
             "Les patrimoines mobilier et immobilier n'entrent en ligne de compte dans le calcul de la quotité de bourse que via un mécanisme de", strong("seuils d'exclusion du dispositif"), ": si la famille se trouve au-dessus du seuil de", strong("patrimoine mobilier"), "(50 000 EUR ou 100 000 EUR selon les postes)", strong("ou bien"), "au-dessus du seuil de", strong("patrimoine immobilier"), "(150, 200, 250 ou 300 k€ selon les postes), elle n'est pas éligible aux bourses scolaires de l'AEFE. Vérifiez auprès de votre poste les seuils d'exclusion s'appliquant à ce dossier.",
             br(),
             "NB : Le calcul dans ce simulateur se poursuit nonobstant un éventuel dépassement des seuils sous le présent onglet.",
             br(), br(),
             
             div(numericInput("pat_immo", "Patrimoine immobilier", value = 0, min = 0), class = "inline"), div("Indiquer en valeur d'achat (i.e. sans prise en compte d'un paramètre d'inflation) la valeur effectivement acquise du ou des biens immobiliers (i.e. déduire de la valeur du bien les éventuels crédits encore dus). ", class = "inline"),
             
             br(), br(),
             
             div(numericInput("pat_res_princip", "Dont résidence principale", value = 0, min = 0), class = "inline"), div("Un abattement de 20% est effectué sur la valeur de la résidence principale.", class = "inline"),
             br(), br(),
             
             numericInput("pat_mobi", "Patrimoine mobilier (encours bancaires, actions, plans d'épargne, autres liquidités)", value = 0, min = 0),
             
             #br(),br(),
             
             div(numericInput("pat_retraites", "Dont plan(s) de retraite par capitalisation", value = 0, min = 0), class = "inline"), div("Un abattement de 10% est effectué sur la valeur du ou des plan(s) d'épargne retraite.", class = "inline"),
             
             br(), br(),
             
             div(textOutput("warning_pat_immo"), class = "warning"),
             div(textOutput("warning_pat_mobi"), class = "warning"),
             
             br()
             ),
    
    
    tabPanel("Revenus et avantages",
             br(),
             strong("Les revenus et avantages pris en considération sont ceux de l'année (n-1). Par exemple, pour la campagne 2023-2024, on prend en considération les revenus et avantages de l'année 2022."), "Si vous ne disposez que du revenu net de la famille, vous pouvez le rentrer comme revenu brut et laisser zéro en charges.",
             br(), br(),
             strong("Les revenus et avantages sont à rentrer dans la monnaie d'appel des frais de scolarité (monnaie locale)."),
             br(), br(),
             
             div(numericInput("revenu_brut", "Revenus bruts annuels, salariaux et assimilés", value = 0, min = 0), class = "inline"), div("Entrez la somme de vos revenus bruts : salaires, traitements, primes, indemnités salariales, pensions de retraites, allocations CCPAS, aides reçues de la famille, etc.", class = "inline"),
             br(), br(),
             
             div(numericInput("revenu_immo", "Revenus mobiliers et immobiliers", value = 0, min = 0), class = "inline"), div("Le cas échéant, revenus locatifs, revenus de placements financiers, rentes, etc. Il faut rentrer les revenus bruts perçus, et non les revenus nets déclarés à l'administration fiscale.", class = "inline"),
             br(), br(),
             
             div(numericInput("avantage_PAR", "Pension alimentaire reçue", value = 0, min = 0), class = "inline"), div("Le cas échéant, pension alimentaire perçue. Si la pension effectivement perçue est inférieure au montant tel que fixé dans le jugement de séparation ou de divorce, il faudra apporter la preuve de l'engagement d'une procédure de recouvrement contentieux.", class = "inline"),
             br(), br(),
             
             div(numericInput("avantage_LOF", "Avantage en nature : logement mis gratuitement à disposition", value = 0, min = 0), class = "inline"), div("Le cas échéant, valeur locative annuelle du logement mis à disposition (e.g. par l'employeur, par la famille, etc). La prise en compte de cet avantage est limitée à un maximum de 30% du revenu brut annuel de la famille.", class = "inline"),
             
             br(), br(),
             
             div(numericInput("avantage_VOI", "Avantage en nature : voiture de fonction", value = 0, min = 0), class = "inline"), div("Si un véhicule est mis à votre disposition gracieusement, entrez la valeur locale d'achat d'un véhicule neuf de la même catégorie, divisée par 7 (nb d'années d'amortissement théorique).", class = "inline"),
             
             br(), br(),
             
             div(numericInput("avantage_NAT", "Autres avantages en nature", value = 0, min = 0), class = "inline"), div("Par exemple billets d’avion, chauffage, électricité, eau, gaz, abonnement de téléphonie fixe ou mobile, personnel de service, etc, pris en charge par l’employeur.", class = "inline"),
             
             br()
             ),
    
    tabPanel("Charges déductibles",
             br(),
             "En dehors des frais de scolarité (onglet suivant), les seules charges prises en considération par SCOLA sont strictement celles indiquées ci-dessous. Elles sont relatives à l'année (n-1) : par exemple, pour la campagne 2023-2024, on prend en considération les charges de l'année 2022.",
             br(), br(),
             strong("Les charges sont, tout comme les revenus, à rentrer dans la monnaie d'appel des frais de scolarité (monnaie locale)."),
             br(), br(),
             
             numericInput("charges_IMP", "Impôt sur le revenu", value = 0, min = 0),
             
             br(),

             div(numericInput("charges_CHA", "Charges sociales", value = 0, min = 0), class = "inline"), div("Cotisations sociales obligatoires : retraite (\"vieillesse\", \"veuvage\"), assurance chômage, assurance maladie, CSG, CRDS. Par ailleurs, lorsque le système de protection sociale du pays d'accueil apparaît insuffisant (et seulement dans ce cas), les cotisations à d'autres systèmes de protection sociale (e.g. CFE) peuvent être pris en compte. Dans ce cas de figure, seules les cotisations de la caisse couvrant les droits essentiels sont à indiquer ici, et non celles des mutuelles complémentaires.", class = "inline"),
    
             br(), br(),
             
             div(numericInput("charges_PAD", "Pension alimentaire due", value = 0, min = 0), class = "inline"), div("Le cas échéant, pension alimentaire versée par le parent déposant le présent dossier. Ce montant ne sera pris en compte que sur production des pièces justifiant du
versement effectif de la pension alimentaire.", class = "inline"),
             
             br()
             ),
    
    tabPanel("Frais de scolarité",
             br(),
             "Seuls les frais de scolarité annuels (S), les frais d’inscription annuelle (SA) et les droits de première
inscription (S1) sont pris en compte dans le calcul de la quotité théorique de bourse. On parle bien ici des frais de scolarité pour l'année scolaire pour laquelle le dossier est déposé,", strong("exprimés en monnaie locale."),
             
             br(), br(),
             
             div(numericInput("frais_scol_S", "Frais de scolarité annuels (S)", value = 0, min = 0), class = "inline"), div("Frais de scolarité au sens strict, à l’exclusion des frais parascolaires", class = "inline"),
             
             br(), br(),
             
             numericInput("frais_scol_SA", "Frais d'inscription annuelle (SA)", value = 0, min = 0),
             
             br(),
             
             div(numericInput("frais_scol_S1", "Droits de première inscription (S1)", value = 0, min = 0), class = "inline"), div("Le cas échéant uniquement", class = "inline"),
             
             br()
             ),
    
    tabPanel("Quotité théorique et reste à charge",
             
             br(),
             "Voici les résultats (intermédiaires en noir et finaux en vert gras) du calcul de la quotité théorique et du reste à charge pour ce dossier de bourse scolaire.",
             
             br(), br(),
             
             tableOutput("intermediaires"),
             
             div(numericInput("CPS", "Contribution progressive de solidarité (CPS) pour cette campagne", value = points_de_CPS, min = 0), class = "inline"), div(paste0("Cette contribution vise à minorer légèrement (en points de pourcentage) toutes les quotités théoriques à l'exception de celles à 100% : les quotités théoriques inférieures à ", seuil_progressivite_CPS, "% se voient réduites de ce nombre de points, celles entre 99.9% et ", seuil_progressivite_CPS, "% se voient réduites d'une fraction linéaire de ce nombre de points."), strong("Au démarrage de la campagne 2023-2024, l'AEFE avait fixé la CPS à 2 points de pourcentage, mais elle a relevé cette contribution à 7 points lors de la Commission Nationale des Bourses de juin 2023. Ce régime est malheureusement toujours valable pour la campagne 2024-2025."), class = "inline"),
             
             br(), br(),
             
             div(textOutput("quotiteApresContrib"), class = "vertgras"),
             
             div("Le conseil consulaire est libre de proposer une quotité différente de celle ci-dessus, en recommandant une pondération à la hausse (ou à la baisse) après examen du dossier en CCB.", class = "warning"),
             
             br(),
             
             div(numericInput("total_frais_scol", "Pour le calcul du reste à charge, total des frais scolaires et parascolaires inclus dans la demande de bourse", value = 0, min = 0), class = "inline"),
             div(textOutput("localCurrencyCode"), class = "inline"), HTML("&nbsp;&nbsp;"),
             div("(La quotité de bourse calculée s'applique uniformément à chacun des items constitutifs des frais de scolarité.)", class = "inline"),
    
             br(),
             
             textOutput("montantCouvertParBourse"),
             
             br(),
             
             div(textOutput("resteACharge"), class = "vertgras"),
             
             br()
             )
  )# end tabsetPanel
  
) # end fluidPage





# The server
server <- function(input, output, session) {
  
  # monnaie locale
  observe({
    if(input$monnaie != "EUR – Euro")
      enable("tauxChanc") # was born disabled thanks to ShinyJS
    else {
      updateNumericInput(session, "tauxChanc", value = 1)
      disable("tauxChanc")
      }
  })
  
  local_currency_code <- reactive({ stringr::str_sub(input$monnaie, 1, 3) })
  
  output$localCurrencyCode <- renderText({ local_currency_code() })
  
  output$blurb_tauxChanc <- renderText({ paste("EUR pour 1", local_currency_code()) })
  
  # nombre de parts
  P <- reactive({
    validate(
      need(input$enfants_handicapes <= input$enfants_charge, "Le nombre d'enfants handicapés à charge ne peut être supérieur à celui des enfants à charge.")
      )
    2 + 0.5 * input$enfants_charge + 0.5 * input$enfants_handicapes
    })
  
  # patrimoines
  pat_immo <- reactive({
    validate(
      need(input$pat_res_princip <= input$pat_immo, "Le patrimoine immobilier total ne peut être inférieur à la valeur de la résidence principale possédée.")
    )
    input$pat_immo - 0.2 * input$pat_res_princip
  })
  
  output$warning_pat_immo <- renderText({
    ifelse(pat_immo() > seuil_warning_pat_immo, paste("Attention ! Vérifiez le seuil de patrimoine immobilier pour votre poste (souvent", format_curr(seuil_warning_pat_immo), "EUR). Un dossier au-dessus du seuil n'est pas éligible pour les bourses scolaires AEFE."), "")
  })
  
  pat_mobi <- reactive({
    validate(
      need(input$pat_retraites <= input$pat_mobi, "Les plans d'épragne retraite font partie intégrante du patrimoine mobilier et ne peuvent donc lui être supérieurs en valeur.")
    )
    input$pat_mobi - 0.1 * input$pat_retraites
  })


  output$warning_pat_mobi <- renderText({
    # TODO: make sure the text above is displayed in any circumstance (grey text from the "validate" rules)
    ifelse(pat_mobi() > seuil_warning_pat_mobi, paste("Attention ! Vérifiez le seuil de patrimoine mobilier pour votre poste (souvent", format_curr(seuil_warning_pat_mobi), "EUR). Un dossier au-dessus du seuil n'est pas éligible pour les bourses scolaires AEFE."), "")
  })
  
  # calculs pour aboutir à la quotité de bourse
  
  revenu_net <- reactive({
    # that's all in local currency
    revenu <- input$revenu_brut + input$revenu_immo + min(0.3*input$revenu_brut, input$avantage_LOF) + input$avantage_PAR + input$avantage_VOI + input$avantage_NAT
    charges <- input$charges_IMP + input$charges_CHA + input$charges_PAD
    revenu - charges
  })
  
  
  Fs <- reactive({
    input$frais_scol_S + input$frais_scol_SA + input$frais_scol_S1
  })
  
  revenu_reference <- reactive({
    revenu_net() - Fs()
  })
  
  # quotient familial
  Q <- reactive({
    revenu_reference() / P()
  })
  

  ####
  # from now on, we switch to euros
  ####
  
  # quotient familial pondéré
  Qp <- reactive({
    Q() * input$tauxChanc * 100 / input$IPPA
  })

  # quotité théorique partielle de bourse (en points de pourcentage)
  quotite_th <- reactive({
    ifelse(Qp() <= Qmin, 100, ifelse(Qp() >= Qmax, 0, 100 * (1 - (Qp() - Qmin)/(Qmax - Qmin))))
  })

  
  output$intermediaires <- renderTable(
    data.frame(`Variable calculée` = c("Revenu net (Rn) :", "Revenu de référence (R) :", "Quotient familial (Q) :", "Quotient familial pondéré (Qp) :", "Quotité théorique brute (Qt) : "),
               Valeur = c(format_curr(revenu_net()), format_curr(revenu_reference()), format_curr(Q()), format_curr(Qp()), round(quotite_th(), 2)),
               Unité = c(rep(local_currency_code(), 3), "EUR", "%"),
               `Formule de calcul` = c("Rn = revenu brut + avantages - charges",
                                       "R = Rn - { frais de scolarité S, SA et S1 }",
                                       "Q = R / nombreDeParts",
                                       "Qp = Q * tauxDeChancellerie / (IPPA / 100)",
                                       paste0("Qt = 100 * (1 - (Qp - ", Qmin, ") / (", Qmax, " - ", Qmin, "))")),
               check.names = F),
    align = "lrll", rownames = F, colnames = T
  )
  
  
  quotiteApresContrib <- reactive({
    quot = quotite_th()
    ifelse(quot == 100, quot, ifelse(quot <= seuil_progressivite_CPS, max(0, quot - input$CPS), quot - input$CPS * (100 - quot) / (100 - seuil_progressivite_CPS)) )
  })
  
  output$quotiteApresContrib <- renderText({
    paste0("Quotité théorique finale (après contribution) : ", round(quotiteApresContrib(), 2), "%")
  })
  
  montantCouvert <- reactive({
    input$total_frais_scol * quotiteApresContrib() / 100
  })
  
  output$montantCouvertParBourse <- renderText({
    validate(
      need(input$total_frais_scol >= input$frais_scol_S + input$frais_scol_SA + input$frais_scol_S1, "Le total des frais scolaires doit nécessairement être supérieur à la somme des frais S, SA et S1 déclarés sous l'onglet \"Frais de scolarité\".")
    )
    paste("Montant couvert par la bourse scolaire accordée :", format_curr(montantCouvert()), local_currency_code())
  })
  
  output$resteACharge <- renderText({
    if(isTruthy(input$total_frais_scol) & input$total_frais_scol >= input$frais_scol_S + input$frais_scol_SA + input$frais_scol_S1) # trying...
    paste("Reste à charge pour la famille :", format_curr(input$total_frais_scol - montantCouvert()), local_currency_code())
    })
  
} # end of the server


# Run the application 
shinyApp(ui = ui, server = server)


