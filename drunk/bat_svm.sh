#!/bin/bash

cd /home/ec2-user/R-testes/drunk/
Rscript /home/ec2-user/R-testes/drunk/svm.R > results/svm/result_$(date -d "today" +"%Y%m%d%H%M").txt 2> results/svm/erro_$(date -d "today" +"%Y%m%d%H%M").txt
