* TRI–Compustat firm matching (Stata)
* Purpose: build crosswalk TRI (tri_id) ↔ Compustat (gvkey)
* Inputs: TRI/Compustat firm title CSVs with simkeys
* Output: all_matches_final.dta (crosswalk)
* Author: Sofia Ruano Remírez

///Matching firm names in TRI and COMPUSTAT
clear all
*Importing the CSV files 
// COMPUSTAT
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
import delimited "COMPUSTAT_Clean_Unique_Firm_Names_Simkeys(in).csv", clear
rename v1 company_title
rename v2 simkey
save compustat_firms_simkeys.dta, replace

// TRI
import delimited "TRI_Unique_Firms_FINAL_titles_Simkeys(in).csv", clear
save tri_firms_simkeys.dta, replace

* --- Exploration COMPUSTAT---
use "compustat_firms_simkeys.dta", clear

//Duplicates analysis by simkey
duplicates report simkey
duplicates list simkey
*Comment: we have several duplicates in simkey (maybe due to firm spelling similarity, the matching tool associates two similar names to be the same, and therefore thay have the same simkey). There are cases in which we have 516 duplicates for a specific simkey.

//Duplicates using all variables
duplicates report simkey company_title

//Analyzing for missing values
count if missing(simkey)
count if missing(company_title)
*Comment: no missing values, as assessed in the files before applying the matching tool

* Quick look: simkeys with more than one title
bysort simkey company_title: gen tag = _n==1
bysort simkey: egen n_titles = total(tag)
drop tag n_titles
save, replace
*Comment: this is to eyeball cases in which simkey is repeated using n_titles as auxiliary variable. Indeed, it allows to see from the editor view how many times a simkey is repeated over all the database.

* --- Exploration TRI ---

use "tri_firms_simkeys.dta", clear

//Duplicates on variable simkey
duplicates report simkey
*Comment: as before, we do have repetitions in simkey, even a case in which the simkey is repeted 642 times (maybe bear this in mind for the manual reviewing).

* How many exact duplicate rows are there (same simkey + title)?
duplicates report simkey company_title

* check for missing values
count if missing(simkey)
count if missing(company_title)

* simkeys with more than one title
bysort simkey company_title: gen tag = _n==1
bysort simkey: egen n_titles = total(tag)
drop tag n_titles


/////////////////////////////////////

****Now, merge on simkey (matches only)****
*Start off with safe cases, in which both in TRI and COMPUSTAT there is only a unique value for a specific simkey (no repetitions in simkeys esentially)

* CS summary 
cd"C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
use "compustat_firms_simkeys.dta", clear
rename company_title company_title_cs
duplicates drop simkey company_title_cs, force
bys simkey company_title_cs: gen byte __pair = _n==1
bys simkey: egen n_titles_cs = total(__pair)
keep simkey n_titles_cs
duplicates drop
save "cs_sum.dta", replace

* TRI summary 
use "tri_firms_simkeys.dta", clear
capture rename company_title company_title_tri
duplicates drop simkey company_title_tri, force
bys simkey company_title_tri: gen byte __pair = _n==1
bys simkey: egen n_titles_tri = total(__pair)
keep simkey n_titles_tri
duplicates drop
save "tri_sum.dta", replace

//Classify simkeys: Safe vs Ambigous 
*  Join summaries to see status on both sides
use "cs_sum.dta", clear
merge 1:1 simkey using "tri_sum.dta"
* _merge==3: simkeys present in both datasets

keep if _merge==3
drop _merge

*  Labels
gen byte safe = (n_titles_cs==1 & n_titles_tri==1)
label define yesno 0 "ambiguous" 1 "safe"
label values safe yesno

* Save the classification
tab safe

save "simkey_status.dta", replace


//Build the SAFE 1:1 matches 
* clean COMPUSTAT with names
use "compustat_firms_simkeys.dta", clear
rename company_title company_title_cs
duplicates drop simkey company_title_cs, force
save "cs_clean.dta", replace

* clean TRI with names
use "tri_firms_simkeys.dta", clear
capture rename company_title company_title_tri
duplicates drop simkey company_title_tri, force
save "tri_clean.dta", replace

* keep only SAFE simkeys
use "simkey_status.dta", clear
keep if safe==1
keep simkey
save "safe_keys.dta", replace

* bring the name on each side (1:1)
use "cs_clean.dta", clear
merge m:1 simkey using "safe_keys.dta"
keep if _merge==3
drop _merge
save "cs_safe.dta", replace

use "tri_clean.dta", clear
merge m:1 simkey using "safe_keys.dta"
keep if _merge==3
drop _merge
save "tri_safe.dta", replace

* Join both sides (each simkey has 1 name per side)
use "cs_safe.dta", clear
merge 1:1 simkey using "tri_safe.dta"
keep if _merge==3
drop _merge


* generate column "keep_pair" for the manual review
gen keep_pair = ""   

* export to excel
export excel simkey company_title_cs company_title_tri keep_pair ///
    using "simkey_review_safe.xlsx", firstrow(variables) replace


///STEPPING OVER THE AMBIGOUS CASES
* 1. load simkey status (whether it is safe or ambiguous)
use "simkey_status.dta", clear
keep if safe==0
keep simkey
save "amb_keys.dta", replace

* 2. Amb COMPUSTAT 
use "compustat_firms_simkeys.dta", clear
rename company_title company_title_cs
duplicates drop simkey company_title_cs, force
merge m:1 simkey using "amb_keys.dta"
keep if _merge==3
drop _merge
save "amb_cs.dta", replace

* 3. Amb TRI 
use "tri_firms_simkeys.dta", clear
capture rename company_title company_title_tri
duplicates drop simkey company_title_tri, force
merge m:1 simkey using "amb_keys.dta"
keep if _merge==3
drop _merge
save "amb_tri.dta", replace

* 4. Create all possible combinations of each simkey
use "amb_cs.dta", clear
joinby simkey using "amb_tri.dta"
*Comment: here we create the Cartesian product of each simkey.

///////////////////////
///////////////////////////
//////////////////////////
*PAUSE FOR MANUAL EXCEL REVIEW OF THE 1:1 SIMKEY MERGED CS AND TRI dataset
//////////////////////////
///////////////////////////
//////////////////////
*Once having reviewed manually the firm names matching between TRI and CS, there are some cases that were incorrectly matched, so we keep only the pairs of firm names that were correctly matched.

* Import the reviewed safe cases
import excel using "simkey_review_safe.xlsx", firstrow clear
drop E F
* Keep only confirmed pairs
keep if keep_pair == "YES"
count
*Comment: we have 776 succesful matches of the safe dataset.

* Save the final safe matches
save "simkey_safe_final.dta", replace

////////////////////////////////////////////////////////////7
*Now that we have stored the safe cases, we proceed with the ambigous cases.
*We can use gvkey to identify the uniqueness of each firm in CS. That is: 
*Same simkey + same gvkey = just a duplicate spelling → it is safe to collapse.
*Same simkey, different gvkeys = true ambiguity → needs further review.

* Open original CS database (the one with gvkey + company_title)
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\Company_Names_New"
use "Names_COMPUSTAT_with_gvkey", clear

* Keep only needed variables
keep gvkey company_title
rename company_title company_title_cs

* Merge with ambiguous CS names
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
merge m:1 company_title using "amb_cs.dta"
drop _merge
drop if missing(simkey)
* Now each ambiguous company_title_cs is linked to its gvkey
save "amb_cs_with_gvkey.dta", replace

duplicates report gvkey
*Comment: there are no duplicates in gvkey, so all the firms in the "amb_cs" dataset are different from each other, even though the matching tool assigned the same simkey to several firms with similar spellings.

* sanity checks
assert !missing(gvkey)

* how many distinct gvkeys per simkey?
bys simkey: egen n_gvkeys_cs = nvals(gvkey)
bys simkey: egen n_titles_cs = nvals(company_title_cs)

*IMPORTANT COMMENT: simkey doesn't seem to help to identy for uniqueness for the ambigous cases matching, since there are cases for which, a specific simkey was assigned to 20 different firms, that we can know for a fact are different because they have different gvkeys. Maybe for the amb cases, it is better to treat simkey as a candidate bucket only, as we know it might not be that trusworthy in comparison to the safe cases, in which a unique simkey was assigned in both CS and TRI.

*So our goal could be that, for each simkey bucket, propose the rights CS-TRI pairs.

//So, brief new methodology:

***BLOCK A = CS side: keep only the rows inside ambiguous buckets and make them unique
//We only care about CS rows that belong to ambiguous simkey buckets. Then we ensure no accidental duplicates.
use "amb_cs_with_gvkey.dta", clear

* 1) Keep only rows that belong to an ambiguous simkey bucket
drop if missing(simkey)
*Comment: this is just a mere sanity check, as all observations should and indeed have a simkey value generated by the AI tool.

* 2) Sanity: every CS row must have a gvkey (ID).
assert !missing(gvkey)

* 3) Ensure one row per (simkey, gvkey, name) just in case
bys simkey gvkey company_title_cs: keep if _n==1

save "amb_cs_unique.dta", replace

***BLOCK B: TRI side: keep one row per (simkey, TRI name)
use "amb_tri.dta", clear
rename company_text_id tri_id

duplicates report tri_id
save "amb_tri.dta", replace

***BLOCK C: build candidate pairs (within the simkey bucket)
use "amb_cs_with_gvkey.dta", clear
joinby simkey using "amb_tri.dta"

order simkey gvkey tri_id company_title_cs company_title_tri

duplicates report company_title_tri

* 1) soft name normalization
gen strL cs_norm  = lower(trim(company_title_cs))
gen strL tri_norm = lower(trim(company_title_tri))
replace cs_norm  = itrim(ustrregexra(cs_norm , "[[:punct:]]", ""))
replace tri_norm = itrim(ustrregexra(tri_norm, "[[:punct:]]", ""))

* 2) flag of exact equality
gen byte exact_norm = (cs_norm == tri_norm)
*Comment: we have at first 481 cases of direct exact match by name

keep if exact_norm==1

*
bys simkey gvkey: gen n_tri_for_gv = _N
bys simkey tri_id: gen n_gv_for_tri = _N

keep if n_tri_for_gv==1 & n_gv_for_tri==1

gsort company_title_cs company_title_tri


*save these exact matches
keep simkey gvkey tri_id company_title_cs company_title_tri
save "amb_matches_exact_auto.dta", replace

*keep keys for the antijoin after
preserve
    keep simkey gvkey tri_id
    duplicates drop
    save "amb_matches_exact_keys.dta", replace
restore
*Comment: 0 observations are duplicates.
display as text ">>> Exact saved matches: " _N



///////////////////////////
*Now, we proceed with the non-exact in spelling firm names (antijoin)
*BLOCK 1:
* === unique CS ===
use "amb_cs_with_gvkey.dta", clear
drop if missing(simkey)
assert !missing(gvkey)
bys simkey gvkey company_title_cs: keep if _n==1
save "amb_cs_unique.dta", replace

* === unique TRI  ===
use "amb_tri.dta", clear
assert !missing(tri_id)
bys simkey tri_id company_title_tri: keep if _n==1
save "amb_tri_unique.dta", replace

* === CS×TRI combinations withing each simkey ===
use "amb_cs_unique.dta", clear
joinby simkey using "amb_tri_unique.dta"

* === Take out the already confirmed exact cases by spelling (anti-join with keys) ===
merge 1:1 simkey gvkey tri_id using "amb_matches_exact_keys.dta"
keep if _merge==1
drop _merge

save "amb_candidates_nonexact.dta", replace
display as text ">>> Non-exact Candidates: " _N

*BLOCK 2: Creation of Score based on names f the non-exact spelling cases

use "amb_candidates_nonexact.dta", clear

* basic normalization again
gen strL cs_n  = lower(company_title_cs)
gen strL tri_n = lower(company_title_tri)
replace cs_n  = ustrregexra(cs_n , "[^a-z0-9 ]", " ")
replace tri_n = ustrregexra(tri_n, "[^a-z0-9 ]", " ")
replace cs_n  = itrim(ustrregexra(cs_n , " +", " "))
replace tri_n = itrim(ustrregexra(tri_n, " +", " "))

* similarity token coverage CS→TRI y TRI→CS (0–1) and symmetric average
gen double cov_cs = 0
gen wc_cs = wordcount(cs_n)
forvalues i=1/60 {
    quietly gen tok = word(cs_n, `i')
    replace cov_cs = cov_cs + (strpos(" " + tri_n + " ", " " + tok + " ")>0) if !missing(tok)
    drop tok
}
replace cov_cs = cov_cs / wc_cs if wc_cs>0
drop wc_cs

gen double cov_tri = 0
gen wc_tri = wordcount(tri_n)
forvalues i=1/60 {
    quietly gen tok = word(tri_n, `i')
    replace cov_tri = cov_tri + (strpos(" " + cs_n + " ", " " + tok + " ")>0) if !missing(tok)
    drop tok
}
replace cov_tri = cov_tri / wc_tri if wc_tri>0
drop wc_tri

gen double score = (cov_cs + cov_tri)/2

save "amb_candidates_nonexact.dta", replace

////////////
 *Block 3:
use "amb_candidates_nonexact.dta", clear

* sort by simkey bucket, and within each simkey, by score
gsort simkey gvkey -score company_title_tri

* numerate within each (simkey, gvkey) according to that order
by simkey gvkey: gen r_cs = _n

* Top-K (by default, K=1) 
local K = 1
keep if r_cs <= `K'
*Comment: we keep the top-1 candidate on the TRI side for each gvkey on the CS side according to the calculated score.

gen keep_pair = ""
order simkey gvkey tri_id company_title_cs company_title_tri score r_cs
export excel using "simkey_review_nonexact.xlsx", firstrow(variables) replace
*Comment: * Build non-exact candidates: CS×TRI within each simkey, excluding exact 1:1 matches.
* score = average token overlap between normalized CS and TRI names (0–1).
* r_cs ranks TRI options within each (simkey, gvkey) by score (1 = best).
* Keep top-1 per gvkey and export to Excel for manual YES/NO. (gvkey/tri_id are identifiers only.)

////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
**PAUSE FOR EXCEL MANUAL REVIEW
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////

cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
* 1) import reviewed Excel and keep only YES
import excel using "simkey_review_nonexact.xlsx", firstrow clear
gen byte is_yes = (upper(trim(keep_pair))=="YES")
keep if is_yes
count
*Comment: we have 316 matches for the nonexact cases.

* keep only the keys + names
keep simkey gvkey tri_id company_title_cs company_title_tri
save "amb_matches_nonexact_manual_yes.dta", replace

* 2) combine with the exact 1:1 matches I saved earlier
use "amb_matches_exact_auto.dta", clear
append using "amb_matches_nonexact_manual_yes.dta"

* dedupe just in case
duplicates drop simkey gvkey tri_id, force
save "amb_matches_final.dta", replace

* 3) Sanity checks: one-to-one inside each bucket
bys simkey gvkey: assert _N==1
bys simkey tri_id: assert _N==1
display "Final matches: " _N

////////////////////////////////
**Now that we have the ambigous cases done (exact + nonexact), we go back to the SAFE cases to add gvkey and tri_id so that we can append them with the ambigous ones.
clear all
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"

//add tri_id to SAFE
use "simkey_safe_final.dta", clear              
describe

use "tri_firms_simkeys.dta", clear
describe
rename company_title company_title_tri
save "tri_firms_simkeys.dta", replace

* bring tri_id from the TRI universe (I initially named it as "company_text_id")
use "simkey_safe_final.dta", clear
merge 1:1 simkey company_title_tri using "tri_firms_simkeys.dta"

keep if upper(trim(keep_pair))=="YES"
count
drop _merge
rename company_text_id tri_id
save "simkey_safe_final.dta", replace


//add gvkey to SAFE
* open the master CS with gvkeys file
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\Company_Names_New"
use "Names_COMPUSTAT_with_gvkey.dta", clear
describe   // checking our target variables: company_title and tri_id

* rename a bit the title to company_title_cs so it is equal to the other file name, for the merge
rename company_title company_title_cs
save "Names_COMPUSTAT_with_gvkey.dta", replace

* going back to the SAFE cases (now with tri_id)
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
use "simkey_safe_final.dta", clear   
merge m:1 company_title_cs using "Names_COMPUSTAT_with_gvkey.dta"
keep if _merge ==3
count

drop _merge
drop dup
save "safe_matches_with_ids.dta", replace

**********************************************
* FINAL APPEND: AMB (exact + nonexact) + SAFE
**********************************************
clear all
set more off
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"

* --- 1) AMB: exact ---
use "amb_matches_exact_auto.dta", clear
keep simkey gvkey tri_id company_title_cs company_title_tri
gen source = "amb_exact"
tempfile amb_exact
save `amb_exact'

* --- 2) AMB: non-exact YES) ---
use "amb_matches_nonexact_manual_yes.dta", clear
capture rename company_title company_title_cs
capture rename company_text_id tri_id
keep simkey gvkey tri_id company_title_cs company_title_tri
gen source = "amb_nonexact"
tempfile amb_non
save `amb_non'

* --- 3) SAFE with IDs ---
use "safe_matches_with_ids.dta", clear
keep simkey gvkey tri_id company_title_cs company_title_tri
gen source = "safe"
tempfile safeids
save `safeids'

* --- 4) Unify everything ---
use `amb_exact', clear
append using `amb_non'
append using `safeids'

* --- 5) cleaning and chek-ups ---
duplicates drop simkey gvkey tri_id, force
*Comment: there are no duplicates

gsort company_title_cs

* --- 6) Save master final file ---
save "all_matches_final.dta", replace


*Before merging with Compustat + TRI databases:
*******************************************************
* AMBIGUOS: take K = 2 (second best TRI per gvkey)
*******************************************************
clear all
set more off


cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"

* 0) Non-exact candidates (with score)
use "amb_candidates_nonexact.dta", clear

* 1) Already selected pairs (exact + non-exact YES K=1) in order not to re-propose
tempfile dec_exact dec_non tgv ttri

preserve
    use "amb_matches_exact_auto.dta", clear
    keep simkey gvkey tri_id
    duplicates drop
    save `dec_exact'
restore

preserve
    use "amb_matches_nonexact_manual_yes.dta", clear
    keep simkey gvkey tri_id
    duplicates drop
    save `dec_non'
restore

* 2) Exclude already used gvkeys within their simkey
preserve
    use `dec_exact', clear
    append using `dec_non'
    keep simkey gvkey
    duplicates drop
    save `tgv'
restore
merge m:1 simkey gvkey using `tgv'
keep if _merge==1
drop _merge

* 3) Exclude already used tri_id within their simkey
preserve
    use `dec_exact', clear
    append using `dec_non'
    keep simkey tri_id
    duplicates drop
    save `ttri'
restore
merge m:1 simkey tri_id using `ttri'
keep if _merge==1
drop _merge

* 4) Rank per (simkey, gvkey) and keep K = 2
gsort simkey gvkey -score company_title_tri
by simkey gvkey: gen r_cs = _n
keep if r_cs==2

* 5) export for manual review, as done before
gen keep_pair = ""
order simkey gvkey tri_id company_title_cs company_title_tri score r_cs
export excel using "simkey_review_nonexact_K2.xlsx", firstrow(variables) replace

display as text ">>> Exported: simkey_review_nonexact_K2.xlsx"



//////////////////////////////////
*AFTER MANUAL CHECK
////////////////////////////////

* 1)Keep YES cases when K=2
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
import excel using "simkey_review_nonexact_K2.xlsx", firstrow clear
keep if upper(trim(keep_pair))=="YES"
keep simkey gvkey tri_id company_title_cs company_title_tri
save "amb_matches_nonexact_manual_yes_K2.dta", replace

*Now, add these cases to the matches final file:

*--- 1) open current master
use "all_matches_final.dta", clear
local n0 = _N

*--- 2) Prepare K=2 (same structure + source label)
preserve
    use "amb_matches_nonexact_manual_yes_K2.dta", clear
    capture confirm variable source
    if _rc gen source = "amb_nonexact_K2"
    keep simkey gvkey tri_id company_title_cs company_title_tri source
    tempfile k2
    save `k2'
restore

*--- 3) Add K=2 to master and dedupe by key
append using `k2'
duplicates drop simkey gvkey tri_id, force

*--- 4) Check-ups 1–to-1 within each simkey
bys simkey gvkey: gen _dup_gv = _N
bys simkey tri_id: gen _dup_tri = _N
count if _dup_gv>1 | _dup_tri>1
di as result ">>> Conflicts 1–to–1 after K=2: " r(N)

*--- 5) save updated master crosswalk file
save "all_matches_final.dta", replace


*Comment: here I manually reviewed some isolated cases of duplicates either by gvkey or by tri_id, removed them and now we have the final list of matches.


save "all_matches_final.dta", replace


/////Now, merge with COMPUSTAT AND TRI DATASETS


*Comment: For the merge with TRI_4, I have to create again the variable tri_id on TRI_4 to be able to merge it with our crosswalk , as I didnt save it at the time.
*Thus, some of the steps here are the same I did in August for the clean firm names taks.

* === A) create clean name -> tri_id ===
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\Company_Names_New"
use "TRI_clean_unique_firms_by_nameid.dta", clear   // viene de tu do-file de verano
keep company_clean company_text_id
duplicates drop company_clean, force
rename company_text_id tri_id
save "tri_name_to_id_map.dta", replace

* === B) In TRI_4: construct company_clean exactly in the same way and map tri_id ===
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\TRI"
use "TRI_4.dta", clear

* (1) company_raw with same hierarchy
gen company_raw = standardizedparentcompanyname
replace company_raw = parentcompanyname          if missing(company_raw)
replace company_raw = submittedparentcompanyname if missing(company_raw)
replace company_raw = foreignparentcompanyname   if missing(company_raw)
replace company_raw = name                       if missing(company_raw)
drop if company_raw == "NA"

* (2) normalization identical to the one I did in August
replace company_raw  = strtrim(company_raw)
replace company_raw  = regexr(company_raw, "\s+", " ")

gen company_clean = lower(company_raw)
replace company_clean = ustrregexra(company_clean, "[^a-z0-9&\- ]", "")
replace company_clean = ustrregexra(company_clean, "\s+", " ")
replace company_clean = ustrtrim(company_clean)

foreach s in " inc" " corp" " llc" " ltd" " co" " plc" " lp" " sa" " nv" ///
             " limited" " holdings" " hldgs" " bancorp" " bancshares" ///
             " reit" " bank" " services" " group" " trust" " company" ///
             " incorporated" " corporation" " properties" {
    replace company_clean = subinstr(company_clean, "`s'", "", .)
}
replace company_clean = ustrtrim(company_clean)

* (3) bring tri_id from dictionary
merge m:1 company_clean using ///
    "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\Company_Names_New\tri_name_to_id_map.dta"

tab _merge

drop _merge

* tri_id is now compatible with the crosswalk/intermediate master file
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
save "TRI_4_with_triid.dta", replace

*Comment: we now have tri_id in the original TRI_4 database, that I saved as TRI_4_with_triid, so now we can do the direct merges with COMPUSTAT through gvkey, and TRI through tri_id.

/////////////////////////
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"

use "all_matches_final.dta", clear
drop _dup1 _dup2 

save "crosswalk_cs_tri_final.dta", replace

///////////
*MERGE CROSSWALK WITH COMPUSTAT
//////////
* complete panel (several rows per gvkey)
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project"
use "Compustat firm level data.dta", clear

merge m:1 gvkey using ///
    "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool\crosswalk_cs_tri_final.dta", ///
    keepusing(tri_id company_title_tri)

tab _merge

keep if _merge==3
drop _merge
*Comment: by keeping if _merge==3, we keep the CS dataset only for the firms that are matched with TRI.

cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
save "compustat_with_tri_id.dta", replace


////////////
*MERGE NOW CROSSWALK + COMPUSTAT WITH TRI_4_with_triid
////////////
cd "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool"
use "TRI_4_with_triid.dta", clear

merge m:1 tri_id using ///
    "C:\Users\SRuano\OneDrive - IESE Business School\Desktop\RA\Jieun Project\After Matching tool\crosswalk_cs_tri_final.dta", ///
    keepusing(gvkey company_title_cs company_title_tri)

tab _merge

keep if _merge==3 
drop _merge
*Comment: by keeping if _merge==3, we keep the TRI dataset only for the firms that are matched with CS.
save "TRI_facilities_with_gvkey.dta", replace
