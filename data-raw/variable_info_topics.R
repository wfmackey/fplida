# Controlled topic tags for the variable-info metadata.

VARIABLE_INFO_TOPIC_VOCABULARY <- c(
  "id",
  "date_time",
  "demographic",
  "family_household",
  "geography",
  "housing",
  "income",
  "taxation",
  "superannuation",
  "employment",
  "payroll",
  "education_training",
  "health",
  "disability_caring",
  "social_security",
  "aged_care",
  "migration_citizenship",
  "travel",
  "births_deaths",
  "business",
  "industry",
  "finance_accounting",
  "innovation_research",
  "digital_technology",
  "trade",
  "intellectual_property",
  "agriculture",
  "energy_environment",
  "legal_insolvency",
  "program_service_delivery",
  "survey_design",
  "data_quality"
)

variable_info_topic_vocabulary <- function() {
  VARIABLE_INFO_TOPIC_VOCABULARY
}

.vi_topic_normalise <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("([a-z])([A-Z])", "\\1 \\2", x, perl = TRUE)
  x <- tolower(gsub("[_./()&-]+", " ", x))
  x <- gsub("[^a-z0-9]+", " ", x)
  paste0(" ", gsub("[[:space:]]+", " ", trimws(x)), " ")
}

.vi_topic_has <- function(text, patterns) {
  any(vapply(patterns, grepl, logical(1), x = text, perl = TRUE))
}

.vi_topic_default_plida <- function(dataset, context) {
  if (dataset == "CORE") {
    if (.vi_topic_has(context, "\\bcore income\\b")) {
      return("income")
    }
    if (.vi_topic_has(context, "\\bcore locations?\\b")) {
      return("geography")
    }
    if (.vi_topic_has(context, "\\bcore relationships?\\b")) {
      return("family_household")
    }
  }

  defaults <- c(
    ACLD = "demographic",
    CENSUS = "demographic",
    LFS = "employment",
    NHS = "health",
    NSMHW = "health",
    PEX = "health",
    SDAC = "disability_caring",
    NACDC = "aged_care",
    APSED = "employment",
    ATO_CR = "taxation",
    ATO_MCS = "superannuation",
    BUSOWN = "business",
    CGT = "taxation",
    ERS = "superannuation",
    JK = "employment",
    JM = "employment",
    PIT_IE = "income",
    PIT_ITR = "income",
    PIT_PS = "income",
    RPS = "housing",
    SAE = "superannuation",
    SMSF = "superannuation",
    STP = "payroll",
    AEDC = "education_training",
    HE = "education_training",
    `A&T` = "education_training",
    AIR = "health",
    MBS = "health",
    PBS = "health",
    DEX = "program_service_delivery",
    DOMINO = "social_security",
    AMEP = "education_training",
    MT_DEMOGS = "migration_citizenship",
    SDB = "migration_citizenship",
    TRAVELLERS = "travel",
    VISA = "migration_citizenship",
    COMBINED = "demographic",
    CORE = "demographic",
    TVA = "education_training",
    NDIS = "disability_caring",
    BIRTHS = "births_deaths",
    DEATHS = "births_deaths",
    MCD = "health"
  )

  unname(defaults[dataset])
}

.vi_topic_default_blade <- function(context) {
  rules <- list(
    survey_design = "\\bsurvey weights?\\b",
    intellectual_property = paste0(
      "\\b(intellectual property|iplord|iprapid|patent|trade mark)\\b"
    ),
    agriculture = "\\b(agriculture|agricultural|farm level)\\b",
    taxation = paste0(
      "\\b(business activity statement|business income tax|capital gains tax|",
      "pay as you go|research and development tax incentive)\\b"
    ),
    payroll = "\\bsingle touch payroll\\b",
    employment = paste0(
      "\\b(employee earnings?|jobkeeper|jobmaker|job vacancy|employment conditions|",
      "workplace agreements?)\\b"
    ),
    innovation_research = paste0(
      "\\b(innovation module|research and development|rdti|berd|pnperd|goverd)\\b"
    ),
    digital_technology = "\\bdigital activity\\b",
    energy_environment = "\\benergy water environment\\b",
    trade = paste0(
      "\\b(international trade|merchandise trade|australian exporters|",
      "trade in services)\\b"
    ),
    legal_insolvency = "\\binsolvency\\b",
    geography = "\\bbusiness locations?\\b",
    finance_accounting = "\\b(economic activity survey|capital expenditure)\\b",
    industry = "\\bemerging industries\\b",
    program_service_delivery = "\\bfamily violence funded agencies\\b",
    date_time = "\\bbusiness birthdate\\b"
  )

  for (tag in names(rules)) {
    if (.vi_topic_has(context, rules[[tag]])) {
      return(tag)
    }
  }
  "business"
}

.vi_topic_tags_one <- function(
    asset, dataset, variable, description, module, product, table) {
  asset <- toupper(trimws(as.character(asset)))
  dataset <- toupper(trimws(as.character(dataset)))
  variable_text <- .vi_topic_normalise(variable)
  description_text <- .vi_topic_normalise(description)
  semantic_text <- paste(variable_text, description_text)
  context <- .vi_topic_normalise(paste(module, product, table))
  tags <- character()

  add_tag <- function(tag, condition) {
    if (isTRUE(condition)) {
      tags <<- c(tags, tag)
    }
  }

  add_tag(
    "id",
    .vi_topic_has(
      variable_text,
      c(
        "\\b(id|key|aeuid|abn|acn|bg id|bn|hash|hashed)\\b",
        "\\b[a-z0-9]+ id\\b"
      )
    ) ||
      (asset == "BLADE" && .vi_topic_has(variable_text, "^ (cn|tn|fn) $")) ||
      .vi_topic_has(
        description_text,
        c(
          "\\b(identifier|linking id|deidentified|de identified)\\b",
          "\\bunique [a-z ]{0,40}(identifier|number|key)\\b"
        )
      )
  )

  add_tag(
    "date_time",
    .vi_topic_has(
      variable_text,
      c(
        "\\b(date|dt|year|yr|month|mth|quarter|qtr|week|day|timestamp|time|period|fy|tsid|dob)\\b",
        "\\b(start|end) (date|dt|year|yr|month|mth)\\b"
      )
    ) ||
      .vi_topic_has(
        description_text,
        c(
          "^ (the )?(date|year|month|quarter|week|day|time|financial year|income year|reference period)\\b",
          "\\b(date|year|month) (of|on|when|in which)\\b",
          "\\b(start|end|birth|death|arrival|departure|grant|lodgement) date\\b"
        )
      )
  )

  person_age <- .vi_topic_has(
    semantic_text,
    c(
      "\\b(age group|age range|age at|age of (the )?(person|respondent|contact|client|child|participant)|person s age)\\b",
      "\\b(agecat|agegrp)\\b"
    )
  ) ||
    (.vi_topic_has(variable_text, "^ age $") &&
       !.vi_topic_has(description_text, "\\bage appropriate development\\b"))
  add_tag(
    "demographic",
    person_age || .vi_topic_has(
      semantic_text,
      c(
        "\\b(sex|gender|indigenous|aboriginal|torres strait|ancestry|ethnicity|religion)\\b",
        "\\b(language spoken|main language|country of birth|birthplace|marital status)\\b",
        "\\b(culturally|linguistically|population status)\\b"
      )
    )
  )

  add_tag(
    "family_household",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(families|family|households?|spouses?|partners?|parents?|children|child)\\b",
        "\\b(dependants?|dependents?)\\b",
        "\\b(relationship|marital|grandparent|kinship|lone parent|couple)\\b"
      )
    )
  )

  add_tag(
    "geography",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(address|postcode|postal area|suburb|geograph|remoteness|aria)\\b",
        "\\b(sa1|sa2|sa3|sa4|lga|mesh block|local government area)\\b",
        "\\b(state or territory|australian state|usual residence|residential location)\\b",
        "\\b(statistical area|greater capital city|region code|region name)\\b"
      )
    )
  )

  add_tag(
    "housing",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(housing|dwelling|rent|rental|tenant|landlord|mortgage|accommodation)\\b",
        "\\b(home owner|home ownership|residential property|body corporate|council rates)\\b"
      )
    )
  )

  add_tag(
    "income",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(income|salary|salaries|wage|wages|earnings|earner|remuneration)\\b",
        "\\b(dividend|interest income|gross payment|net payment|investment income)\\b",
        "\\b(lump sum|royalty|royalties|pension income)\\b"
      )
    )
  )

  add_tag(
    "taxation",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(tax|taxation|taxable|assessable|deduction|offset|refund|levy|payg|gst|bas)\\b",
        "\\b(franking|capital gains?|withheld|withholding|tax file|income tax)\\b"
      )
    )
  )

  add_tag(
    "superannuation",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(superannuation|super fund|smsf|contributions?|rollover|member account|apra)\\b",
        "\\b(accumulation|pension phase|retirement income|annuity|annuities)\\b"
      )
    )
  )

  add_tag(
    "employment",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(employment|employed|employee|employer|occupation|job|labour|labor)\\b",
        "\\b(workforce|workplace|hours worked|payee|payer|vacancy|vacancies|hiring)\\b",
        "\\b(jobkeeper|jobmaker|industrial relation|termination payment)\\b"
      )
    )
  )

  add_tag(
    "payroll",
    .vi_topic_has(
      semantic_text,
      "\\b(payroll|single touch payroll|stp|pay cycle|pay period|gross wages)\\b"
    )
  )

  add_tag(
    "education_training",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(education|schools?|students?|study|courses?|qualifications?|degrees?|training)\\b",
        "\\b(trainee|apprentice|vet|university|literacy|teacher|atar|enrol|enrolled|enrolment|learning)\\b",
        "\\b(australian early development|early development census)\\b"
      )
    )
  )

  add_tag(
    "health",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(health|medical|medicare|mbs|pbs|pharmaceutical|drug|medicine|hospital)\\b",
        "\\b(diseases?|conditions?|diagnosis|diagnoses|diagnosed|diagnostic|doctor|general practitioner|mental health)\\b",
        "\\b(immunisation|immunised|vaccination|vaccinated|vaccine|clinical|treatment|patient|prescriber|practitioner)\\b",
        "\\b(injury|symptom|dental)\\b"
      )
    )
  )

  add_tag(
    "disability_caring",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(disability|disabled|impairment|functional limitation|carer|caring|ndis)\\b",
        "\\b(care need|support need|assistance need|special need)\\b"
      )
    )
  )

  add_tag(
    "social_security",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(centrelink|social security|welfare|jobseeker|austudy|abstudy)\\b",
        "\\b(family tax benefit|parenting payment|carer payment|newstart)\\b",
        "\\b(youth allowance|income support|concession card|clean energy advance)\\b"
      )
    )
  )

  add_tag(
    "aged_care",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(aged care|ageing|aging|nursing home|residential care)\\b",
        "\\b(home care package|dementia|veteran supplement)\\b"
      )
    )
  )

  add_tag(
    "migration_citizenship",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(visa|migration|migrant|citizen|citizenship|permanent resident)\\b",
        "\\b(country of birth|settlement|nationality|humanitarian entrant)\\b"
      )
    )
  )

  add_tag(
    "travel",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(travel|traveller|traveler|border movement|departure|trip|port|flight)\\b",
        "\\b(movement direction|arrival movement)\\b"
      )
    )
  )

  add_tag(
    "births_deaths",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(birth registration|death registration|cause of death|deceased|mortality)\\b",
        "\\b(stillbirth|birth event|death event|underlying cause)\\b"
      )
    )
  )

  add_tag(
    "business",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(business|enterprises?|companies|company|corporations?|firms?|organisations?|organizations?)\\b",
        "\\b(economic unit|management capability|dealroom|startup|abn|acn)\\b"
      )
    )
  )

  add_tag(
    "industry",
    .vi_topic_has(
      semantic_text,
      "\\b(industry|anzsic|economic activity|activity unit|industry sector)\\b"
    )
  )

  add_tag(
    "finance_accounting",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(accounting|balance sheet|assets?|liability|liabilities|turnover|revenue|expenditure)\\b",
        "\\b(capital expenditure|capex|debt|loan|financial statement|operating profit)\\b",
        "\\b(operating loss|equity|cost of goods|cash flow)\\b"
      )
    )
  )

  add_tag(
    "innovation_research",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(innovation|research and development|rdti|berd|pnperd|goverd)\\b",
        "\\b(experimental development|researcher)\\b"
      )
    )
  )

  add_tag(
    "digital_technology",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(digital|technology|internet|ict|software|computer|cyber|online|web|cloud)\\b",
        "\\b(artificial intelligence|machine learning|data analytics|rfid|e commerce)\\b"
      )
    )
  )

  add_tag(
    "trade",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(exports?|exported|exporting|imports?|imported|importing)\\b",
        "\\b(international trade|trade in services|merchandise trade)\\b",
        "\\b(customs|tariff|overseas market|foreign market)\\b"
      )
    )
  )

  add_tag(
    "intellectual_property",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(intellectual property|patent|trade mark|trademark|design right)\\b",
        "\\b(plant breeder|ip right|iplord|iprapid|patent citation)\\b"
      )
    )
  )

  add_tag(
    "agriculture",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(agriculture|agricultural|farms?|crops?|livestock|horticulture|horticultural)\\b",
        "\\b(irrigation|irrigated|hectares?)\\b",
        "\\b(cattle|sheep|wheat|barley|canola|cotton|grapes|pasture|dairy)\\b"
      )
    )
  )

  add_tag(
    "energy_environment",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(energy|electricity|gas|water|waste|emissions?|environment|environmental|fuel)\\b",
        "\\b(renewables?|solar|greenhouse|recycling|sewerage)\\b"
      )
    )
  )

  add_tag(
    "legal_insolvency",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(insolvency|insolvent|bankrupt|liquidation|liquidator|liquidated|legal|court)\\b",
        "\\b(restraint clause|administrator appointed|winding up)\\b"
      )
    )
  )

  add_tag(
    "program_service_delivery",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(program participation|service delivery|service provider|participant plan)\\b",
        "\\b(support plan|claim status|application status|grant status)\\b",
        "\\b(benefit entitlement|client outcome|program outcome|case management)\\b"
      )
    )
  )

  add_tag(
    "survey_design",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(survey weight|weighting|benchmark|replicate weight|stratum|strata)\\b",
        "\\b(sample design|sample weight|person weight|household weight)\\b",
        "\\b(response rate|selection probability|calibration weight)\\b"
      )
    )
  )

  add_tag(
    "data_quality",
    .vi_topic_has(
      semantic_text,
      c(
        "\\b(data quality|imputed|imputation|dummy flag|repair result|edit status)\\b",
        "\\b(validation status|missing indicator|extract version|source system)\\b",
        "\\b(record source|confidentialised|perturbation|quality flag)\\b"
      )
    )
  )

  if (!length(tags)) {
    fallback <- if (asset == "BLADE") {
      .vi_topic_default_blade(context)
    } else {
      .vi_topic_default_plida(dataset, context)
    }
    if (length(fallback) && !is.na(fallback) && nzchar(fallback)) {
      tags <- fallback
    }
  }

  VARIABLE_INFO_TOPIC_VOCABULARY[
    VARIABLE_INFO_TOPIC_VOCABULARY %in% unique(tags)
  ]
}

# Vectorised interface. Scalar inputs are recycled to the longest input.
variable_info_topic_tags <- function(
    asset,
    dataset,
    variable,
    description = "",
    module = "",
    product = "",
    table = "") {
  inputs <- list(
    asset = asset,
    dataset = dataset,
    variable = variable,
    description = description,
    module = module,
    product = product,
    table = table
  )
  input_lengths <- lengths(inputs)
  n <- max(input_lengths)
  if (!n) {
    return(character())
  }
  if (any(!input_lengths %in% c(1L, n))) {
    stop("Inputs must have length one or a common length.", call. = FALSE)
  }
  inputs <- lapply(inputs, rep, length.out = n)

  vapply(seq_len(n), function(i) {
    paste(
      .vi_topic_tags_one(
        asset = inputs$asset[[i]],
        dataset = inputs$dataset[[i]],
        variable = inputs$variable[[i]],
        description = inputs$description[[i]],
        module = inputs$module[[i]],
        product = inputs$product[[i]],
        table = inputs$table[[i]]
      ),
      collapse = ","
    )
  }, character(1))
}
