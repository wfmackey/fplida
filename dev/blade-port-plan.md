# BLADE R->Rust port plan

_Generated from a 7-agent analysis of R/generate_blade.R against the 140-test suite. Stage 0 (helpers) is committed; stages 1-5 remain._

## Architecture

WHAT STAYS IN R (setup/orchestration only): generate_blade() top-level driver, .resolve_blade_tables, metadata CSV loading (.blade_tables/.blade_variables/.blade_key_variables), run_dir resolution, parquet writing (write_product/dataset_dir), and the load-or-create branching for the business spine and link. R remains the IO and metadata boundary; it reads the three CSVs, slices out per-table variable-name lists, calls Rust, and writes parquet. This mirrors the existing thin-wrapper convention (R/extendr-wrappers.R + per-product R wrappers) used by every other generator.

WHAT MOVES TO RUST (all per-row value generation + the spine/link reductions where it pays off): (1) the business-spine builder (.make_blade_business_spine) including the state-aware assigner, tabulate/rowsum reductions, health/profiling group sweeps, and all closed-form identifier/turnover/SISCA formulas; (2) the person-business link builder (.blade_employee_assignments + .make_blade_person_link) and the reconciliation reduction; (3) the generic per-variable classifier (.blade_metadata_value_for) plus its second name-based fallthrough cascade; (4) the six table-specific generators (Table 1/4/6/7/8/27) and the EEH (17) and location (24/25) frames; (5) all low-level deterministic helpers (.stable_name_seed, .blade_numeric_id, .blade_deidentified_id, .blade_amount, .blade_pick_codes, .blade_count_value, .blade_character_code, etc.).

DATA CROSSING THE BOUNDARY: the business spine and link cross as struct-of-arrays — R passes/receives named column vectors via list!()/List, exactly as employment.rs and stp.rs already do. Spine columns go down to Rust as parallel slices (Strings, Integers, Doubles, Nullable). Rust returns a List (data.frame) of typed columns per product; R write_product()s it. The person spine is consumed only when the business spine must be (re)created. Determinism is preserved by porting every (seq*const + seed + salt) %% mod formula in f64 arithmetic with 1-based indices and rem_euclid, never seeding a PRNG (no rand crate needed — confirmed business_pool.rs already follows this RNG-free hashing style for BLADE). The metadata-driven period/tsid/reference-date values are computed in Rust from the Available.Periods strings R passes alongside each variable.

PRECEDENCE INVARIANT: .blade_value_for's branch order (name_map passthrough -> abn/id/bg/version/tsid/quarter -> special-table dispatch -> generic metadata cascade -> name-based fallthrough -> table-class default) is load-bearing and must be reproduced as an ordered Rust match. The generic classifier is a fallback that returns Option (R's NULL) and the caller's second cascade must also be ported or the no-placeholder invariant fails.

## Staged plan

### Stage 0 — Scaffolding + shared helpers + crate deps  _(effort M (~2 days). Highest leverage: every later stage depends on these; they are pure, fully specified by formula, and independently testable against the running R helpers.)_
- **Scope:** Create src/rust/src/blade/{mod.rs,helpers.rs,periods.rs}. Port the leaf deterministic helpers with zero business logic: stable_name_seed, numeric_id, id_number, deidentified_id, financial_year_label, financial_year_code, anzsco_group/normalise_anzsco, normalise_state/industry, amount, pick_codes, valid_response_codes (hand-rolled lookbehind), character_code, count_value, code_name, financial_name, and the period chain (split_periods, period_end_year, latest_period_from_values, tsid_from_period, tsid(table) incl. table-5/table-1 special case, end_year, reference_date). Add `regex`, `once_cell` to Cargo.toml (rayon already present; rand NOT needed). Register `pub mod blade;` in lib.rs and add fns to extendr_module!.
- **Deliverable:** blade/helpers.rs + blade/periods.rs compiling under R CMD INSTALL; one #[extendr] smoke fn (e.g. blade_stable_name_seed__) to unit-test the hash against R values. No R behaviour change yet.

### Stage 1 — Business spine generator (HIGHEST VALUE, most self-contained)  _(effort L (~3-4 days). Self-contained: depends only on Stage-0 helpers and the person spine; produces the analytical source-of-truth every other stage reads. Biggest single win.)_
- **Scope:** Port .make_blade_business_spine + .blade_assign_business_by_state + .blade_default_business_count + .blade_anzsic06 + health/profiling/legal-form/SISCA/turnover/birth-year logic into blade/spine.rs. extendr fn make_blade_business_spine__ returns the full ~80-column frame. Reductions: assign_business_by_state (parallel per-person closed-form), serial tabulate+rowsum into employment_count/annual_wages, employee-representative override, two serial ave(max) sweeps over bg_group (health propagation, profiling propagation + size<2 de-profiling). Keep R wrapper: load person spine, call Rust, write _system/business-spine.parquet. Reconciliation columns stay placeholder (filled in Stage 2).
- **Deliverable:** business-spine.parquet generated by Rust, passing the spine-only tests (L47-70, L122 overflow, L297-319 Table-1-derived columns that read spine values). The aggregate identities sum(employment_count)==sum(employed) and sum(annual_wages)==sum(income[employed]) hold to the cent.

### Stage 2 — Person-business link + reconciliation  _(effort M-L (~3 days). Depends on Stage 1 spine + Stage 0 assigner/helpers. Note salt 11 (spine) vs 31 (link) divergence is intentional and must be preserved.)_
- **Scope:** Port .blade_employee_assignments (primary salt 31, secondary salt 73 with avoid-remap, secondary-fallback for n>=8), .make_blade_person_link (employee-then-owner rbind, owner salt 101, 28-col schema), .blade_assignment_occupation_codes (health overweighting), and .add_blade_link_reconciliation (HashMap<bn,(rows,distinct)> reduction overwriting payg headcount columns) into blade/link.rs. extendr fns make_blade_person_link__ and add_blade_link_reconciliation__. R wrapper writes _system/plida-blade-link.parquet and rewrites the reconciled spine.
- **Deliverable:** Link parquet + reconciled spine passing L83-96 (BN identity, state concordance >0.85, occupation health flag), L122-124 (no NA BN, secondary job present at n=5000), L63 (headcount mismatch present), L49-52 reconciliation columns.

### Stage 3 — Generic metadata classifier + name fallthrough  _(effort L (~4 days). The branch-order fidelity and the PCRE lookbehind reimplementation are the risk; tests are structural so exactness of values is not required, only types/formats/ranges/order.)_
- **Scope:** Port .blade_metadata_value_for (the ordered version/period/date/month/year/anzsic/anzsco/geography/postcode/state/coded-response/numeric-measure/alphanumeric cascade) AND the second name-based fallthrough cascade from .blade_value_for (lines 1860-1971) ending in character_code(width=3). Build VariableSpec{name,lower,item_lower,valid_lower,context,salt} once per column; classify() returns Option<BladeColumn> enum {Int,Dbl,Chr,Date,None}. Geography branch uses location_rows (mb_lookup) passed from R. into blade/classifier.rs.
- **Deliverable:** Generic columns for the many non-special tables fill correctly; the global no-`_[0-9]{6}$`-placeholder invariant holds (the single most important test, L345-347/364-366/386-388/428-434). Coded-response code-frame membership and numeric/count typing pass.

### Stage 4 — Table-specific generators + frame orchestrator  _(effort XL (~5-6 days). Largest surface; depends on Stages 0-3. The EEH frame and location frame are separate code paths; BIT prefix masking and the table-class fallback sets are the fiddly bits.)_
- **Scope:** Port .blade_special_value_for dispatch + the six table generators: Table 1 (.blade_frame_value_for/.blade_role_code/x_gst_bn), 4 (.blade_bas_value_for), 6 (.blade_bit_value_for with c/i/p/t legal-form prefix masking), 7 (.blade_stp_value_for), 8 (.blade_bcs_value_for/.blade_bcs_code), 27 (.blade_birthdate_value_for). Port .make_blade_frame (location_rows precompute + per-variable dispatch), .make_blade_eeh_frame (Table 17, employee-level), .blade_business_location_frame (24/25), .blade_location_lookup_rows, and .select_blade_rows/.select_blade_frame_rows samplers. into blade/tables.rs + blade/eeh.rs + blade/location.rs + blade/sampling.rs. Wire the master .blade_value_for cascade (name_map -> abn/id/bg/version/tsid/quarter -> special -> generic -> fallthrough -> table-class default).
- **Deliverable:** All per-table parquet products generated by Rust passing L297-404 (role codes, GST format, BIT exclusivity, STP super==gross*0.115, BCS code frame, birthdate spikes, EEH eid 15-char + health anzsco prefixes, geography consistency).

### Stage 5 — Orchestration consolidation + key products + build integration  _(effort M (~2-3 days). Mostly glue; the table-specific column drops and the 2N key-row structure are the must-not-miss details.)_
- **Scope:** Port .make_blade_id_bn_key (2N rows over tsid union {'25','21'}), .make_blade_cn_bn_key, .select_key_columns, .blade_table_variable_names table-specific drops (t1 id/x_sisca06/x_anzsic93, t4 month_actioned, t6 cn/fn). Optionally move the per-table dispatch loop into a single generate_blade__ Rust entry returning a List-of-products (tables parallelised via rayon). Verify build_fplida BLADE central-stage integration (worker_results==0, stage_timings$blade set) is undisturbed. R wrapper still resolves tables and writes parquet.
- **Deliverable:** Full generate_blade() Rust-backed; key products + build_fplida test (L407-434) pass, including the global placeholder scan across all abs-blade parquet.

## Rust module layout

- src/rust/src/blade/mod.rs — module root; `pub mod helpers; pub mod periods; pub mod spine; pub mod link; pub mod classifier; pub mod tables; pub mod eeh; pub mod location; pub mod sampling; pub mod keys;`. Holds shared types: BusinessSpine (struct-of-arrays of all ~80 columns), LinkFrame (28-col SoA), LocationRows{mb_code,sa1_code,sa2_code,sa4_code,state}, BladeColumn enum {Int(Vec<Rint>),Dbl(Vec<f64>),Chr(Vec<Option<String>>),Date(Vec<i32>),None}, and the top-level extendr entry fns (re-exported into lib.rs extendr_module!).
- src/rust/src/blade/helpers.rs — pure leaf primitives (Stage 0): stable_name_seed, numeric_id, id_number, deidentified_id, financial_year_label, anzsco_group, normalise_anzsco/state/industry, amount, pick_codes, valid_response_codes (hand-rolled negative-lookbehind scan), character_code, count_value, code_name, financial_name, form_prefix. All free fns, i64/i128 arithmetic, 1-based seq, rem_euclid. No extendr exports except optional smoke-test wrappers.
- src/rust/src/blade/periods.rs — time/metadata derivation: split_periods, period_end_year (YYYY-YY century math with +100 rollover), latest_period_from_values, latest_period (with tables.csv Reference.Period fallback), tsid (incl. table-5->table-1 special case), key_tsid, end_year, reference_date (Date32 day-count; 30-Jun for FY else 31-Dec), financial_year_code, month_abb const array. Caches per-table latest period.
- src/rust/src/blade/spine.rs — make_blade_business_spine: default_business_count, assign_business_by_state (the shared assigner, also used by link), anzsic06, health_occupation_reference const table, the parallel per-business column build, serial tabulate+rowsum reduction, employee-representative override, and the two serial ave(max) bg_group sweeps (health propagation, profiling propagation + size<2 de-profiling). extendr: make_blade_business_spine__.
- src/rust/src/blade/link.rs — employee_assignments (primary/secondary), assignment_occupation_codes (health overweighting), make_blade_person_link (employee+owner blocks), add_blade_link_reconciliation (HashMap<bn,(rows,HashSet<aeuid>)> reduction). extendr: make_blade_person_link__, add_blade_link_reconciliation__. Reuses spine::assign_business_by_state.
- src/rust/src/blade/classifier.rs — VariableSpec struct + classify() ordered cascade (.blade_metadata_value_for) AND the second name-based fallthrough cascade from .blade_value_for. Precompiled regex sets via once_cell::sync::Lazy. period_value helper. Returns BladeColumn (None == R NULL).
- src/rust/src/blade/tables.rs — special_value_for dispatch + the six table generators (Table 1 frame/role-code/gst, 4 BAS, 6 BIT prefix-masked, 7 STP, 8 BCS, 27 birthdate), bcs_code, role_code, bit_form_prefix routing, make_blade_frame orchestrator wiring the full .blade_value_for cascade, and the table-class fallback sets (survey/numeric/char6 const slices).
- src/rust/src/blade/eeh.rs — make_blade_eeh_frame (Table 17 employee-level frame from employee link rows), employee_link_rows filter, eid_eeh via deidentified_id width 14, weekly/hourly/payfreq/rop/agecat/anzscoNN derivations. extendr: make_blade_eeh_frame__.
- src/rust/src/blade/location.rs — business_location_frame (tables 24/25), location_lookup_rows (state-keyed deterministic mesh-block pick from mb_lookup), hashed_arid via deidentified_id width 23 (u128), geocode_precision/address_type draws. extendr: make_blade_location_frame__.
- src/rust/src/blade/sampling.rs — select_blade_rows (salt = stable_name_seed(product)+table*1009, LCG key, order()-equivalent stable rank) and select_blade_frame_rows (no salt, EEH). Returns 1-based sorted index vectors matching R order() semantics.
- src/rust/src/blade/keys.rs — make_blade_id_bn_key (2N rows over tsid union {key_tsid, tsid(8)}), make_blade_cn_bn_key, select_key_columns (metadata Variable.Name order). extendr: make_blade_id_bn_key__, make_blade_cn_bn_key__.

## extendr functions

- `fn make_blade_business_spine__(spine_state: Integers, spine_industry: Nullable<Integers>, spine_sector: Nullable<Integers>, baseline_employed: Integers, baseline_income: Doubles, spine_id: Strings, aeuid_ato: Nullable<Strings>, anzsco_code: Nullable<Strings>, anzsco_title: Nullable<Strings>, seed: i32, n_businesses: Nullable<i32>) -> List // ~80 named typed columns (the full business spine; bg_id/representative_aeuid_ato emitted as '' / Option strings, business_birth_year/exit_year as Vec<Rint>)`
- `fn make_blade_person_link__(spine: List, business_spine: List, seed: i32) -> List // 28 named columns in rbind employee-then-owner order; job_number/primary_job/occupation_health_flag/birth_year/age/sex as Vec<Rint>; annual_wage/PAYG_GROSS_WAGES as Vec<f64> with NA for owners; BN/bn/ABN_HASH_TRUNC the identical cloned String vector`
- `fn add_blade_link_reconciliation__(business_spine: List, link: List) -> List // returns updated headcount columns linked_payg_rows, linked_distinct_persons, payg_actual_hcnt, d_total_payees, payg_link_hcnt_gap, payg_hcnt_delta, payg_headcount_mismatch (employment_count left immutable)`
- `fn make_blade_frame__(variable_names: Strings, items: Strings, valid_responses: Strings, available_periods: Strings, business_rows: List, table_number: i32, product_name: &str, seed: i32, mb_lookup: List) -> List // generic + special-table dispatch for one table; returns one named typed column per variable_name in order`
- `fn make_blade_eeh_frame__(variable_names: Strings, items: Strings, valid_responses: Strings, available_periods: Strings, link: List, business_spine: List, seed: i32) -> List // Table 17 employee-level frame (rows = employee/secondary link rows)`
- `fn make_blade_location_frame__(variable_names: Strings, items: Strings, valid_responses: Strings, business_rows: List, table_number: i32, product_name: &str, seed: i32, mb_lookup: List) -> List // tables 24/25`
- `fn select_blade_rows__(n: i32, table_number: i32, product_name: &str, seed: i32, sample_rate: f64, max_rows: i32) -> Integers // 1-based sorted row indices (order()-equivalent); select_blade_frame_rows__ same minus salt`
- `fn make_blade_id_bn_key__(business_spine: List, key_tsid: &str, tsid8: &str) -> List // 2N rows over tsid union`
- `fn make_blade_cn_bn_key__(business_spine: List, key_tsid: &str) -> List // N rows`
- `fn blade_stable_name_seed__(value: &str) -> i32 // Stage-0 smoke/unit-test wrapper validating the hash against R .stable_name_seed`

## Business-row columns (value-gen inputs)

- blade_business_id: String
- bn: String
- id: String
- bg_id: String
- cn: String
- fn: String
- representative_spine_id: String
- representative_aeuid_ato: Option<String>
- state: i32
- state_code: i32
- anzsic06: String
- industry_division: String
- d_div06: String
- latest_div06: String
- x_anzsic06: String
- x_anzsic93: String
- d_anzsic06: String
- cast_anzsic06: String
- latest_anzsic06: String
- x_sisca08: String
- x_sisca06: String
- x_sisca93: String
- d_sisca08: String
- cast_sisca08: String
- latest_sisca08: String
- sisca_sector: String
- x_sector: i32
- cast_sector: i32
- x_state: i32
- cast_state: i32
- x_st_op: String
- cast_st_op: String
- x_al_st: i32
- x_pcode: String
- cast_pcode: String
- x_npi: i32
- cast_npi: i32
- industry: i32
- sector: i32
- business_birth_year: Rint (nullable)
- business_exit_year: Rint (nullable)
- alive_status: i32
- employment_count: i32
- payg_employee_count: i32
- stp_employee_count: i32
- annual_wages: f64
- hcnt: i32
- fte: f64
- payg_reported_hcnt: i32
- payg_actual_hcnt: i32
- linked_payg_rows: i32
- linked_distinct_persons: i32
- payg_link_hcnt_gap: i32
- payg_hcnt_delta: i32
- payg_headcount_mismatch: i32
- d_total_payees: i32
- turnover: f64
- bas_total_sales: f64
- bas_wages: f64
- bit_total_income: f64
- bit_taxable_income: f64
- gst_payable: f64
- capital_expenditure: f64
- rd_expenditure: f64
- export_value: f64
- import_value: f64
- legal_form: String
- x_tolo: i32
- cast_tolo: i32
- tolo: i32
- private_public: String
- health_industry_flag: i32
- public_health_flag: i32
- representative_anzsco_code: String
- representative_anzsco_title: String
- representative_health_occupation_flag: i32
- is_employing: i32
- is_profiled: i32
- source_person_n: i32

## Shared helpers (Stage 0 — DONE)

- stable_name_seed(s: &str) -> i32 : sum over 1-based char positions of codepoint*(i+1), %% 100000. Iterate chars() (Unicode code points, not bytes) to match utf8ToInt.
- numeric_id(prefix, values: &[f64], width) -> Vec<String> : format!('{}{:0width$.0}', prefix, v.round()) — prefix(1+ chars) + width zero-padded digits. BN width 11 (values=(seq*1000003+seed*9176)%%1e11), id/cn 'E'/'C'+9, bg 'BG'+10, blade_business_id 'B'+9.
- id_number(values: &[&str]) -> Vec<f64> : strip non-digits, parse f64; backfill NA/empty positions with sequential 1..k in order.
- deidentified_id(prefix, values, i_1based, seed, width) -> String : h=stable_name_seed(format!('{}|{}|{}',v,i,seed)); num=((h as i128*1000003 + i*9176 + seed).rem_euclid(10^width)); numeric_id. width 14 -> eid_eeh 'P'+14 (15 chars); width 23 -> hashed_arid 'A'+23 (24 chars, needs u128/i128, 10^23 exceeds u64).
- financial_year_label(start_year) -> String : sprintf('%04d-%02d', y, (y+1)%%100); NA passthrough. Produces 'YYYY-YY' so ^1993/^2001 birthdate regex matches.
- anzsco_group(code, digits) -> String : normalise to 6-char zero-padded (sprintf '%06.0f' of as.numeric, '000000' for NA/empty/'0'), then take first `digits` chars.
- assign_business_by_state(person_state, business_state, seed, salt, same_state_rate, avoid: Option<&[usize]>) -> Vec<usize> : draw[i]=((i+1)*1103515245+seed*1009+salt)%%1000/1000; per-state-group same-state pick via (rank*2654435761+seed*37+salt+st*9176)%%pool_len, else cross-state (rank*2246822519+seed*101+salt+st*3571)%%n_businesses; avoid-remap (assigned%%n)+1 when n>1. ALL products computed in f64 to match R's overflow-free double arithmetic.
- amount(turnover, wages, i_1based, seed, salt, scale) -> f64 : draw=((i*37+seed+salt)%%100)/100; round2(max(turnover*scale*(0.6+draw), wages*0.1)).
- pick_codes(codes, n, seed, salt, include_missing) -> Vec<i32> : partition into missing {7777777,88888888,999999999} vs substantive; out[i]=substantive[((i+1+seed+salt)%%len_sub)]; if include_missing, rows where ((i+1)*41+seed+salt)%%100>=92 get missing[running_rank%%len_miss] — running rank is 1-based over the selected subset, NOT global i.
- valid_response_codes(s) -> Vec<i32> : hand-rolled (regex crate lacks lookbehind) — scan digit runs of len 1..9 whose preceding char is not a digit and which are followed by optional whitespace then '=' or '-'; dedup, parse i32.
- count_value(name_lower, d_total_payees, i_1based, seed, salt) -> i32 : loc|site|premis -> max(1,1+((i+seed+salt)%%6)); manager|director|proprietor|partner -> clamp(base,1,5); else max(0, base+((i+seed+salt)%%4)-1). Branch order load-bearing (loc checked first).
- character_code(n, seed, salt, width, prefix) -> Vec<String> : values=((i+1)*17+seed+salt)%%10^width; format!('{}{:0width$}', prefix, v). Prefix is a letter so output never matches '_[0-9]{6}$'.
- reference_date(table) -> i32 (Date32 day-count) : 30-Jun of end-year when period matches ^[0-9]{4}-[0-9]{2}$, else 31-Dec; fallback year 2024.
- tsid(table) -> String : sprintf('%02d', period_end_year %% 100); table 5 special-cases to table-1 period.
- period_end_year(period) -> Option<i32> : YYYY-YY century arithmetic (+100 rollover guard, e.g. '1999-00'->2000); bare YYYY; else max 4-digit run.
- role_code(i_1based, seed, salt) -> &str : draw=((i*37+seed+salt)%%100); <10 'n', <18 '0', <92 '1', else 'L'.
- bcs_code(i_1based, seed, include_multi) -> i32 : draw=((i*1103515245+seed*97)%%100); <50 0, <78 1, <89 88888888, else 999999999; if include_multi, 78<=draw<84 -> 7777777.
- code_name(lower) -> bool : regex (_cd$|code|type|typ|status|sts|ind$|flag|rt$|rng|marin|cntry|sgmt) — suffix anchors $ must be handled per-alternative.
- form_prefix(legal_form) -> char : Company->'c', Sole trader->'i', Partnership->'p', Trust->'t'.

## Risks

- INTEGER OVERFLOW DIVERGENCE (highest): every multiplicative hash (2654435761, 2246822519, 1103515245, 1000003, 1e11/1e10/1e9 moduli, deidentified_id 10^23) is computed in R as f64 BEFORE %%, silently avoiding i32 overflow. Naive i32/i64 Rust overflows or diverges. Mitigation: use f64 for LCG/index hashes (matches R 53-bit mantissa behaviour exactly, which IS the spec), i128/u128 for deidentified_id width-23 (10^23 > u64). The L122/L124 no-NA-BN and expect_no_warning(L108) overflow tests at n=5000 directly exercise this.
- BRANCH-ORDER FIDELITY: .blade_value_for and .blade_metadata_value_for are long ordered grepl cascades where first-match-wins. name_map passthrough must run first (so id/hcnt/state/turnover/x_sisca08 pass through unchanged); special-table dispatch before generic; coded-response before numeric; numeric before alphanumeric. Any reordering silently re-routes a column (e.g. count->amount) and breaks type/range tests. Mitigation: port as an explicit ordered match with the EXACT R order; cross-check each branch line-by-line.
- PLACEHOLDER INVARIANT (single most load-bearing): no character cell anywhere may match '_[0-9]{6}$' (L345-347/364-366/386-388/428-434, scanned across ALL abs-blade parquet). A NULL return from the generic classifier is NOT terminal — the caller's second name-based fallthrough (lines 1860-1971) ending in character_code(width=3) must also be ported, or unfilled columns leak placeholders. Mitigation: port both cascades; assert prefixes are always letter+digits (no underscore).
- PCRE NEGATIVE LOOKBEHIND: .blade_valid_response_codes uses (?<![0-9])([0-9]{1,9})\s*(=|-) which the `regex` crate cannot express. Mitigation: hand-roll the scan (digit run len 1..9, preceding char not a digit, followed by optional ws then '='/'-'). Code-frame membership tests (L368-372) depend on exact parsing.
- SALT DIVERGENCE (subtle correctness): spine employment_count uses assigner salt 11; link primary uses salt 31; secondary 73; owner 101; select_blade_rows salt = stable_name_seed(product)+table*1009 while select_blade_frame_rows has NO salt. These are intentionally different — 'optimising' by sharing one assignment collapses the hcnt!=linked_distinct_persons gap and breaks L335-336/L63. Mitigation: thread each salt explicitly; document the divergence.
- TABLE-5 / TSID SPECIAL CASE: .blade_tsid(5) deliberately borrows Table 1's period so payg$tsid=='26' matches table1 (L332). Easy to miss. Mitigation: encode the table-5->table-1 redirect in periods.rs.
- ROUNDING SEMANTICS: R round() is round-half-to-even (banker's). The STP super==round(gross*0.115,2) identity (L362-363) requires both sides use the SAME rounding; safe because both derive from one round() call, but a naive (x*100).round()/100 (half-away) could drift a cent. Mitigation: use round-half-to-even for amounts, or ensure derived-equality columns share one computed value.
- R order() SEMANTICS in samplers: .select_blade_rows uses sort(order(key)[1:target]) — ascending stable rank then take first target then sort indices. Mitigation: stable-sort indices by key ascending, take first target, sort. Off-by-one in 1-based seq shifts every draw.
- COLUMN DROPS: table1 must drop id/x_sisca06/x_anzsic93; table4 month_actioned; table6 cn/fn — enforced in .blade_table_variable_names BEFORE generation (the value generators still handle those names if present). Mitigation: keep the drop in the R/Rust variable-name list, not the generator.
- bg_id EMPTY-STRING vs NA: non-profiled businesses get '' not NA; tests filter bg_id!='' and check nchar/regex on the empty case. Producing Option::None breaks grepl and the min(bg_links)>1 group test. Mitigation: emit '' (Vec<String>), reserve None only for genuinely-NA columns (birth/exit year, owner ANZSCO).
- CRATE ADDITIONS: regex + once_cell are not currently in Cargo.toml. Low risk but must be added and build-verified early (Stage 0).

## Verification

"Run devtools::test(filter='generate_blade') (the 6 test_that blocks / 132 assertions in tests/testthat/test-generate_blade.R, plus the BLADE-touching assertions in the build_fplida suite) after EACH stage; the suite is structural (formats, nchar/regex, code-frame membership, ranges, aggregate identities, no-placeholder scan) so it validates a port even though values are not bit-identical to R. Stage 0: add temporary #[extendr] smoke wrappers and assert each ported helper matches the live R helper on a sample of inputs (e.g. blade_stable_name_seed__('x_itip') == .stable_name_seed('x_itip')); no behaviour change so the full suite must stay green. Stage 1 (spine): gate on L47-70 (column presence, BN/id/cn/bg regex+nchar, min(bg_links)>1, any health flag, EXACT sum(employment_count)==sum(employed) and round(sum(annual_wages),2) identities) and L122 (n=5000 no-overflow). Stage 2 (link+reconciliation): L83-96 (BN identity, link$bn subset, state concordance>0.85, occupation health flag), L122-124 (no NA BN, secondary job present), L63 (payg_headcount_mismatch present), L49-52 reconciliation columns. Stage 3 (classifier): the placeholder scan L345-347/364-366/386-388/428-434 plus coded-response L368-372 and numeric/count typing L373-385 on the generic tables. Stage 4 (table generators): L297-404 (Table-1 role codes/GST format/bg groups, BAS exports<=turnover & turnover>=oexp, BIT c/i/p/t exclusivity, STP super==gross*0.115 & double typing, BCS d_gsnewy code frame & locs>=1, birthdate NA/1993/2001 spikes, EEH eid 15-char/no spine_id collision/health anzsco prefixes 25/41/42, geography sa2-firstchar==state). Stage 5: the build_fplida block L407-434 (products include blade, stage_timings$blade set, worker_results length 0, _system + abs-blade outputs exist, global placeholder sum==0) and key-product L245-264 (id-to-bn 2N rows over tsid {'25','21'}, cn-to-bn N rows, metadata column order). Per stage, also diff a Rust-built vs R-built parquet on schema (column names/types/order) with arrow before relying on the structural tests. Keep the R fallback path runnable behind an option (e.g. fplida.blade_engine='r'|'rust') during migration so any failing stage can be A/B-compared against the original R output, then remove once all 6 stages are green."
