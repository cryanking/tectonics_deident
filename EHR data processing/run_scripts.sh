#!/bin/bash
cp -r /root/R_groundhog /root/R_groundhog2

export RETICULATE_MINICONDA_ENABLED=FALSE
set -e

R --slave  -e 'source("/code/make_population.R")' > /code/process.rout 2>&1 
echo "initial_ids"


R --slave  -e 'source("/code/make_table1.R")' > /code/process.rout 2>&1 
echo "table 1"

R --slave  -e 'source("/code/make_lab_outcomes.R")' > /code/process.rout 2>&1 
echo "labs import"

R --slave  -e 'source("/code/make_medication_outcomes.R")' > /code/process.rout 2>&1 
echo "medication import"

R --slave  -e 'source("/code/make_flow_outcomes.R")' > /code/process.rout 2>&1 
echo "flo import"

R --slave  -e 'source("/code/make_adt_outcomes.R")' > /code/process.rout 2>&1 
echo "adt import"

R --slave  -e 'source("/code/alert_days.R")' > /code/process.rout 2>&1 
echo "alert records"


R --slave  -e 'source("/code/make_baseline_acuity.R")' > /code/process.rout 2>&1 
echo "baseline acuity"

R --slave  -e 'source("/code/make_final_dataset.R")' --args $salt > /code/process.rout 2>&1 
echo "merge data"

R --slave  -e 'source("/code/main_analysis.R")' > /code/process.rout 2>&1 
echo "main analysis"


R --slave -e 'options(width=2000); sessionInfo(); installed.packages()[, c("Package", "Version", "Built", "LibPath")]' > /research/outputs/r_state.txt 2>&1



