# data-raw/value_guess_rules.R
#
# Rules for inferring a variable's value domain from its NAME, used to populate
# the `guessed` value_support_status.
#
# A guess is not a source. These rules say "a field called CAMPUS_STATE almost
# certainly holds Australian state codes", which is useful enough to generate a
# plausible column and to tell a reader what shape to expect, but the published
# metadata does not confirm it. Every guessed variable is labelled as such.
#
# Two kinds of guess:
#   * a finite code list  — `values` is non-empty, e.g. state or sex codes
#   * an open typed domain — `values` is empty and `domain` names the type,
#     e.g. an amount or a date. This still improves on "not specified": it
#     drives generation and tells a reader the column is numeric, not coded.
#
# ORDER MATTERS. Rules are tried in order and the first match wins, so specific
# patterns must precede general ones: CAMPUS_POSTCODE must reach the postcode
# rule before any generic `_CODE` rule. The ordering is asserted by
# tests/testthat/test-value-guesses.R.
#
# Patterns match against the UPPERCASED variable name and are PCRE.

# `desc_pattern`, when given, must ALSO match the official description for the
# rule to fire. It exists because some defects are only visible there: a field
# called PROVIDER_STATE whose description reads "State or territory name" holds
# text, not the numeric state codes its name implies.
.guess_rule <- function(id, pattern, domain, type, values = character(0),
                        note = NULL, desc_pattern = NULL) {
  list(id = id, pattern = pattern, domain = domain, type = type,
       values = values, note = note, desc_pattern = desc_pattern)
}

# Shared code lists, taken from variables elsewhere in the registry that DO
# carry a published list for the same concept.
.GUESS_STATE <- c(
  "1: New South Wales", "2: Victoria", "3: Queensland", "4: South Australia",
  "5: Western Australia", "6: Tasmania", "7: Northern Territory",
  "8: Australian Capital Territory", "9: Other Territories"
)
.GUESS_YN <- c("Y: Yes", "N: No")
.GUESS_10 <- c("1: Yes", "0: No")
.GUESS_SEX <- c("1: Male", "2: Female", "9: Not stated")
.GUESS_REMOTENESS <- c(
  "1: Major Cities of Australia", "2: Inner Regional Australia",
  "3: Outer Regional Australia", "4: Remote Australia",
  "5: Very Remote Australia"
)

# BLADE business-survey modules share a missing-data convention: a tick box
# answered 0/1, with sentinel codes for sequencing and non-response.
.GUESS_BCS_YN <- c(
  "0: No", "1: Yes", "7777777: Ticked more than one box",
  "88888888: Missing due to sequencing", "999999999: Missing",
  ".: Not asked"
)
.GUESS_BCS_EXTENT <- c(
  "0: Not at all", "1: A small extent", "2: A moderate extent",
  "3: A major extent", "7777777: Ticked more than one box",
  "999999999: Missing", ".: Not asked"
)

value_guess_rules <- list(

  # -- Survey instrument scales. These are the largest single blocks and the
  # -- most regular, so they run before anything general. --------------------
  .guess_rule(
    "bcs_extent_scale",
    "^(ASS|DIG)[A-Z]+_BCS$|^IM(COL|COM|CUS|FUN|GOS|MGF|SKL|TEC)_(BCS|IM)$",
    "Business survey extent scale (inferred)", "integer", .GUESS_BCS_EXTENT
  ),
  .guess_rule(
    "bcs_ict_extent", "^ITU[A-Z]+_BCS$",
    "Business survey ICT use extent (inferred)", "integer",
    c("0: Not at all", "1: Low to moderate extent", "2: High extent",
      "3: Not applicable", "7777777: Ticked more than one box",
      "88888888: Missing due to sequencing", "999999999: Missing",
      ".: Not asked")
  ),
  .guess_rule(
    "blade_module_tickbox", "_(BCS|IM|DAM)$",
    "Business survey tick box (inferred)", "integer", .GUESS_BCS_YN,
    note = "Business Characteristics Survey, Innovation and Digital Activity modules."
  ),
  .guess_rule(
    "nsmhw_dsm4_flag", "^D_[A-Z]{2,7}[0-9]{2}[A-Z]?$|^DSM[A-Z_]",
    "DSM-IV diagnosis flag (inferred)", "integer", c("1: Yes", "2: No")
  ),
  .guess_rule(
    "nsmhw_icd10_flag", "^I_[A-Z]{2,7}[0-9]{2}[A-Z]?$",
    "ICD-10 diagnosis flag (inferred)", "integer", c("1: Yes", "2: No")
  ),
  .guess_rule(
    "ewes_motivator", "^EMM_",
    "Environmental motivator (inferred)", "integer", c("0: No", "1: Yes")
  ),
  .guess_rule(
    "rd_expenditure", "_(HERD|BERD|GOVERD|PNPERD)$",
    "Research and development expenditure (inferred)", "numeric",
    note = "Open numeric domain, whole dollars."
  ),
  .guess_rule(
    "capex_amount", "_(EAS|CAPEX)$|CAPEX",
    "Capital expenditure (inferred)", "numeric",
    note = "Open numeric domain, whole dollars."
  ),

  # -- Change and movement flags. These carry the name of the thing that
  # -- changed, so they must be caught BEFORE the domain rules: without this,
  # -- STATE_MOVE_1YR_21 ("Usual Address One Year Ago Indicator for State")
  # -- reads as a state code, and CFAGEP_11_16 ("Age Consistency Flag") as an
  # -- age. Both are 0/1.
  .guess_rule(
    "change_or_move_flag",
    "^CF[A-Z]{2,}|(^|_)(MOVE|MOVED|CHG|CHANGE|CHANGED|SAME|DIFF|CONSISTENCY)(_|$)",
    "Change or movement indicator (inferred)", "integer", .GUESS_10,
    note = "Records whether the named attribute changed, not the attribute itself."
  ),

  # -- Imputation flags. These are named after the field they describe, so
  # -- without this rule IMPUTED_COUNTRY_OF_BIRTH reads as a country and
  # -- ORIG_STATE_IMP_FLAG as a state. Both record only WHETHER the value was
  # -- imputed. Must precede every domain rule.
  .guess_rule(
    "imputation_flag",
    "^IMPUTED?_|(^|_)IMP_FLAG($|_)|IMPUTATION|_IMPUTED$",
    "Imputation indicator (inferred)", "integer", .GUESS_10,
    note = "Records whether the named field was imputed, not its value."
  ),

  # -- Fields holding a NAME or free text, not a code. Guessing numeric codes
  # -- for "State or territory name" would be the wrong type as well as the
  # -- wrong values.
  .guess_rule(
    "name_or_text_field",
    "(^|_)(NAME|NM|DESC|DESCRIPTION|TITLE|TEXT|LABEL)($|_)|_NAME$|_DESC$|_DESCRIPTION$|_DS$",
    "Free text (inferred)", "character",
    note = "Holds a name or description rather than a code."
  ),

  # Caught only by the description: PROVIDER_STATE reads as a state code from
  # its name, but its description says "State or territory name", so it holds
  # text. Restricted to the geographic and classification fields where a name
  # variant genuinely exists, to avoid swallowing every description mentioning
  # the word "name".
  .guess_rule(
    "geography_as_name",
    "(?<![A-Z])STATE(?![A-Z])|(^|_)(SA[1-4]|LGA|GCCSA|COUNTRY|SUBURB|LOCALITY)($|_)",
    "Place name as text (inferred)", "character",
    note = "The description says this field holds a name rather than a code.",
    desc_pattern = "\\b(name|names)\\b"
  ),

  # -- Modified Monash Model, a Health Department remoteness classification
  # -- distinct from the ASGS remoteness areas. Must precede the ASGS rule.
  .guess_rule(
    "modified_monash", "(^|_)MMM($|_)|MODIFIED_MONASH",
    "Modified Monash Model category (inferred)", "character",
    c("MM1: Metropolitan area", "MM2: Regional centre",
      "MM3: Large rural town", "MM4: Medium rural town",
      "MM5: Small rural town", "MM6: Remote community",
      "MM7: Very remote community")
  ),

  # -- SUFFIX-ANCHORED RULES RUN FIRST.
  #
  # A variable's trailing token says what it HOLDS; tokens earlier in the name
  # only say what it is about. PRMS_PD_YR_SHARE_AMT_1 is a dollar amount that
  # happens to concern a year, and BRTH_DT_MONTH holds a month, not a date.
  # Without these rules the mid-name YR and DT win and both are mislabelled.
  .guess_rule(
    "money_amount_early", "_AMT$|_AMOUNT$|_AMT_[0-9]+$|_AMOUNT_[0-9]+$",
    "Monetary amount in whole dollars (inferred)", "numeric",
    note = "Open numeric domain. Fields naming a loss, offset or correction may be negative."
  ),
  .guess_rule(
    "indicator_suffix_early",
    "_(IND|INDICATOR|FLAG|FLG|OVERRIDE)$|_(IND|FLAG)_[0-9]+$",
    "Yes/No indicator (inferred)", "character", .GUESS_YN,
    note = "Records whether the named condition holds, not its value."
  ),
  .guess_rule(
    "rate_suffix_early",
    "_(PCT|PCTAGE|PERCENT|PERCENTAGE|RATE|PROP|SHARE)(_[0-9]+)?$",
    "Rate or proportion (inferred)", "numeric",
    note = "Open numeric domain expressed as a percentage or proportion."
  ),
  .guess_rule(
    "count_suffix_early",
    "_(CNT|COUNT|NUM|NBR|QTY|WKS|MTHS|DAYS|HRS|YRS)(_CD)?(_[0-9]+)?$",
    "Count (inferred)", "integer",
    note = "Open non-negative integer domain. The trailing token names the unit counted."
  ),
  # AGE_FY_START is an age at the start of the financial year, not a financial
  # year. A leading AGE token beats a later FY one.
  .guess_rule(
    "age_prefix_early", "^AGE(_|$)",
    "Age in completed years (inferred)", "integer"
  ),
  # These four BLADE fields publish a 3-character month abbreviation in their
  # value_definition, so they are strings, not 1-12. Must precede month_field.
  .guess_rule(
    "month_abbreviation",
    "^(LAUNCH_MONTH|MONTH_ACTIONED|ROUND_MONTH|VALUATION_MONTH)$",
    "Month as a three-letter abbreviation (inferred)", "character",
    c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ),
  .guess_rule(
    "month_suffix_early", "_(MONTH|MTH)$|_(MONTH|MTH)_[0-9]+$",
    "Month of year, 1-12 (inferred)", "integer",
    c("1: January", "2: February", "3: March", "4: April", "5: May",
      "6: June", "7: July", "8: August", "9: September", "10: October",
      "11: November", "12: December"),
    note = "Holds only the month component, even where the name also carries a date token."
  ),
  .guess_rule(
    "year_suffix_early", "_(YEAR|YR)$|_(YEAR|YR)_[0-9]+$",
    "Calendar year (inferred)", "integer",
    note = "Holds only the year component, even where the name also carries a date token."
  ),

  # -- Geography. Specific ASGS levels before anything generic. --------------
  .guess_rule(
    "asgs_meshblock", "(^|_)(MB|MESHBLOCK|MESH_BLOCK)(CD)?([0-9]{4})?($|_)",
    "ASGS mesh block code (inferred)", "character",
    note = "11-digit ASGS mesh block. Too large for a code list."
  ),
  .guess_rule(
    "asgs_sa1", "(^|_)SA1(CD)?([0-9]{4})?($|_)",
    "ASGS Statistical Area Level 1 code (inferred)", "character",
    note = "11-digit SA1. Too large for a code list."
  ),
  .guess_rule(
    "asgs_sa2", "(^|_)SA2(CD|NAME)?([0-9]{4})?($|_)",
    "ASGS Statistical Area Level 2 code (inferred)", "character",
    note = "9-digit SA2. Too large for a code list."
  ),
  .guess_rule(
    "asgs_sa3", "(^|_)SA3(CD|NAME)?([0-9]{4})?($|_)",
    "ASGS Statistical Area Level 3 code (inferred)", "character"
  ),
  .guess_rule(
    "asgs_sa4", "(^|_)SA4(CD|NAME)?([0-9]{4})?($|_)",
    "ASGS Statistical Area Level 4 code (inferred)", "character"
  ),
  .guess_rule(
    "asgs_gccsa", "(^|_)GCCSA(CD|NAME)?([0-9]{4})?($|_)",
    "ASGS Greater Capital City Statistical Area (inferred)", "character"
  ),
  .guess_rule(
    "asgs_lga", "(^|_)LGA(CD|NAME)?([0-9]{4})?($|_)",
    "ASGS Local Government Area (inferred)", "character"
  ),
  .guess_rule(
    "asgs_remoteness", "REMOTENESS|(^|_)RA(CD)?($|_)|(^|_)REMOTE($|_)",
    "ASGS remoteness area (inferred)", "integer", .GUESS_REMOTENESS
  ),
  .guess_rule(
    "au_postcode", "POSTCODE|POSTCD|PSTCD|(^|_)POA(CD)?($|_)",
    "Australian postcode (inferred)", "character",
    note = "Four digits, 0800 to 7999. Too large for a code list."
  ),
  # STATE needs a boundary on BOTH sides. Without the trailing guard it
  # swallows STATEMP ("STATus in EMPloyment") and STATEELECTORATEPUBLIC, which
  # are not state fields at all. POWSTE and USUSTE are genuine, hence the STE
  # alternative.
  .guess_rule(
    "au_state",
    "(?<![A-Z])STATE(?![A-Z])|(^|_)STE($|_|[0-9])|STECD|(^|_)STATE_",
    "Australian state or territory (inferred)", "integer", .GUESS_STATE
  ),
  .guess_rule(
    "sacc_country", "COUNTRY|(^|_)CNTRY|BIRTHPLACE|(^|_)BPL($|_)|SACC",
    "SACC country code (inferred)", "character",
    note = "Standard Australian Classification of Countries, 4-digit."
  ),

  # -- Dates and time. Date before year before generic numeric. --------------
  .guess_rule(
    "date_field", "(^|[^A-Z])DATE($|[^A-Z])|(^|_)DT($|_)|_DT$|^DT_",
    "Calendar date (inferred)", "date",
    note = "Emitted in the source's ddmmmYY form."
  ),
  .guess_rule(
    "financial_year", "(^|_)(FY|FIN_?YEAR|FINYR)($|_)",
    "Australian financial year (inferred)", "character",
    note = "YYYY-YYYY."
  ),
  .guess_rule(
    "calendar_year", "(^|_)(YEAR|YR|CAL_?YEAR)($|_)|_YR$|_YEAR$",
    "Calendar year (inferred)", "integer"
  ),
  # MONTHS (plural) and *AGEINMONTHS are durations, not a month of year.
  .guess_rule(
    "month_field", "(^|_)(MONTH|MTH|MNTH)($|_)(?!S)|_MTH$|_MONTH$",
    "Month of year, 1-12 (inferred)", "integer",
    c("1: January", "2: February", "3: March", "4: April", "5: May",
      "6: June", "7: July", "8: August", "9: September", "10: October",
      "11: November", "12: December")
  ),
  # AMEP panel columns end _YYYY_Q1..Q4 and hold HOURS ATTENDED, not a quarter
  # number. This must precede the quarter rule or 232 columns get 1-4.
  .guess_rule(
    "amep_quarterly_hours", "^[A-Z]+(DL)?_[0-9]{4}_Q[1-4]$",
    "Hours attended in the quarter (inferred)", "numeric"
  ),
  # Deliberately does NOT match a trailing _Q1.._Q4: see amep_quarterly_hours.
  .guess_rule(
    "quarter_field", "(^|_)(QTR|QRTR|QUARTER)($|_)",
    "Quarter of year, 1-4 (inferred)", "integer",
    c("1: Q1", "2: Q2", "3: Q3", "4: Q4")
  ),
  .guess_rule(
    "age_field", "(^|_)AGE($|_)|_AGE$|^AGE_",
    "Age in completed years (inferred)", "integer"
  ),
  .guess_rule(
    "duration_days", "(^|_)(DAYS|NUM_DAYS|DURATION_DAYS)($|_)|_DAYS$",
    "Duration in days (inferred)", "integer"
  ),

  # -- Person demographics. -------------------------------------------------
  .guess_rule(
    "sex_field", "(^|_)(SEX|GENDER)($|_)|_SEX$|^SEX_",
    "Sex (inferred)", "integer", .GUESS_SEX
  ),
  .guess_rule(
    "indigenous_field", "INDIGENOUS|(^|_)INGP($|_)|ATSI",
    "Indigenous status (inferred)", "integer",
    c("1: Non-Indigenous", "2: Aboriginal", "3: Torres Strait Islander",
      "4: Both Aboriginal and Torres Strait Islander", "97: Not stated")
  ),
  .guess_rule(
    "marital_field", "MARITAL|(^|_)MSTAT($|_)|MRTL",
    "Marital status (inferred)", "integer",
    c("1: Never married", "2: Widowed", "3: Divorced", "4: Separated",
      "5: Married", "98: Not applicable", "99: Not stated")
  ),
  .guess_rule(
    "citizenship_field", "CITIZEN|RESIDENCY_STATUS|RESIDENT_STATUS",
    "Citizenship or residency status (inferred)", "integer",
    c("1: Australian", "2: Not Australian", "97: Not stated",
      "99: Overseas visitor")
  ),

  # -- Standard classifications. --------------------------------------------
  .guess_rule(
    "anzsco_field", "ANZSCO|(^|_)OCCP($|_)|OCCUPATION",
    "ANZSCO occupation code (inferred)", "character",
    note = "ANZSCO 2019, six digits. The package bundles the full code list."
  ),
  .guess_rule(
    "anzsic_field", "ANZSIC|(^|_)INDP($|_)|INDUSTRY",
    "ANZSIC industry code (inferred)", "character",
    note = "ANZSIC 2006, division letter or 4-digit class."
  ),
  .guess_rule(
    "asced_field", "ASCED|(^|_)(HEAP|QALLP|QUAL)($|_)|EDUCATION_LEVEL",
    "ASCED education code (inferred)", "character"
  ),
  .guess_rule(
    "atc_field", "(^|_)ATC($|_)|ATC_?CODE",
    "WHO ATC medication code (inferred)", "character"
  ),
  .guess_rule(
    "icd_field", "(^|_)ICD(10|9)?($|_)|CAUSE_OF_DEATH|(^|_)COD($|_)",
    "ICD-10 code (inferred)", "character"
  ),

  # -- Flags and indicators. Must precede the generic code rule. ------------
  .guess_rule(
    "flag_yn", "(^|_)(IND|INDICATOR|FLAG|FLG)$|_IND$|_FLAG$|_FLG$",
    "Yes/No indicator (inferred)", "character", .GUESS_YN
  ),
  .guess_rule(
    "whether_field", "^(WHETHER|HAS|IS)_",
    "Yes/No indicator (inferred)", "integer", .GUESS_10
  ),

  # -- Quantities. Amounts before counts before generic numerics. -----------
  .guess_rule(
    "money_amount", "(^|_)AMT($|_)|_AMT$|^AMT_|AMOUNT",
    "Monetary amount in whole dollars (inferred)", "numeric",
    note = "Open numeric domain. Fields naming a loss, offset or correction may be negative."
  ),
  .guess_rule(
    "count_field", "(^|_)(COUNT|CNT|NUM|NBR)($|_)|_COUNT$|_CNT$",
    "Count (inferred)", "integer",
    note = "Open non-negative integer domain."
  ),
  .guess_rule(
    "rate_pct", "(^|_)(RATE|PCT|PERCENT|PROP|SHARE)($|_)|_PCT$|_RATE$",
    "Rate or proportion (inferred)", "numeric"
  ),
  .guess_rule(
    "weight_field", "(^|_)(WEIGHT|WGT|WT)($|_)|_WT$|^WPM[0-9]|^REPWT",
    "Survey or replicate weight (inferred)", "numeric"
  ),

  # -- A category noun before _ID makes it a CLASSIFICATION code, not an opaque
  # -- key. The VET collection is full of these: PROGRAM_LOE_ID is an ASCED
  # -- level of education, STUDY_REASON_ID an AVETMISS reason code. Treating
  # -- them as surrogate keys would generate meaningless values.
  .guess_rule(
    "classification_code_id",
    "(^|_)(TYPE|STATUS|REASON|OUTCOME|SOURCE|LEVEL|CATEGORY|CLASS|MODE|METHOD|RECOGNITION|PACKAGE|FOE|LOE|LANGUAGE|FUNDING|QUALIFICATION|DELIVERY|OCCUPATION|INDUSTRY|COUNTRY)_?ID$",
    "Source-defined classification code (inferred)", "character",
    note = "A category before _ID marks a classification code, not a surrogate key."
  ),

  # -- Identifiers. Last, because they are the broadest and most opaque. ----
  .guess_rule(
    "identifier_field",
    "(^|_)(ID|KEY|AEUID|ABN|BN|GUID|UUID)$|_ID$|_KEY$|^SYNTHETIC_",
    "Opaque identifier (inferred)", "character",
    note = "Open identifier domain with no finite value list."
  ),
  .guess_rule(
    "code_field", "(^|_)(CD|CODE)$|_CD$|_CODE$",
    "Source-defined code (inferred)", "character",
    note = "The name says it is a code but the source publishes no list."
  )
)
