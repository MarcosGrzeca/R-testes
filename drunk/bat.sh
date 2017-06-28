#!/bin/bash

cd /home/ec2-user/R-testes/drunk/
Rscript /home/ec2-user/R-testes/drunk/somente_classificador.R > results/result_$(date -d "today" +"%Y%m%d%H%M").txt 2> results/erro_$(date -d "today" +"%Y%m%d%H%M").txt
