# TRI-Compustat Firm Matching
Stata code to match US TRI facility-level data with Compustat firm-level data for research on environmental and financial linkages.

///////////

This repository contains a Stata do-file that matches firm names between the US EPA Toxic Release Inventory (TRI) and Compustat.  
Both datasets are public and widely used in academic and policy research.  

## Purpose
The goal of the script is to build a consistent crosswalk that links environmental performance at the facility level (TRI) with financial information at the firm level (Compustat).  
This crosswalk allows researchers to study how corporate environmental behaviour relates to firm-level financial outcomes.

## Main tasks performed
- Import TRI and Compustat firm names with AI-generated identifiers (“simkeys”).  
- Detect duplicates and classify “safe” (1:1) vs. “ambiguous” matches.  
- Normalise firm names to improve comparability.  
- Compute similarity scores to rank ambiguous matches.  
- Export candidate pairs for structured manual review.  
- Map unique identifiers (gvkey and tri_id) to ensure one-to-one consistency.  
- Generate a final crosswalk suitable for empirical research.  

## Software
- Stata  

## Example use
The final output is a harmonised dataset that can be merged with TRI and Compustat data to analyse questions such as:
- How do firms’ financial characteristics relate to their environmental performance?  
- Are environmentally intensive firms more vulnerable to global shocks?  
- How resilient are different industries in terms of sustainability and trade dynamics?  
