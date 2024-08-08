This repository contains code for 
1. The preprocessing of EHR data into the included population
2. The preprocessing of EHR data into outcomes
3. Processing the case-review and alert-review forms
4. Generating enrollment and "table 1" type reports. 
5. Running the main (clustered) analysis
6. Generating helpers for secondary analysis
   - a baseline mortality risk for "higher risk" patients.
   - a matching analysis comparing patients-with-case-reviews and control group patients with similar characteristics.
7. A brief analysis of the teamstepps survey data (removed from the manuscript)

This is all R code, using the container created in dockerfile.dockerfile (tagged as cryanking/tectonics_results:1.4 on dockerhub). 
run_scripts.sh is the order in which the scripts were intended to be run.
It was originally R 4.3 based and updated to R 4.4 due to security concerns.
There are minor diferences between this and the actual code, to avoid publishing how study identifiers were generated and a few comments that reference identifiable data.

  
There are a few unresolved issues, but they are all fairly minor.
 - There is a window toward the end of the trial in which a fair number of participants preoperative evaluation forms were not successfully queried; there is no obvious reason for this. It only affects "table 1" type descriptions. We could re-extract this data if there was a clear purpose.
 - The baseline mortality risk ends up being very similar to the real-time ML risk assessment, but is a little different. The real-time assessment used more variables, but because not all these variable were available (see above comment) a procedure-only risk assessment was used. The actual ML scores were saved, but it is much more difficult to make this available.
 - The construction of respiratory failure may include some episodes of peri-procedural intubation if there was no included anesthesia record (like an ICU sedation).
 - Case review forms from before May 2020 are incomplete (a 255 character field was used to hold the JSON package, which was too short). There is no obvious intact version of this data.
 - There is a windows around the server upgrade in May 2020 in which alert reviews were lost.
   
