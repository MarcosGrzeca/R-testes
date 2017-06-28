#!/bin/bash

cd /home/ec2-user/R-testes/drunk/
Rscript /home/ec2-user/R-testes/drunk/glm.R > results/glm/result_$(date -d "today" +"%Y%m%d%H%M").txt 2> results/glm/erro_$(date -d "today" +"%Y%m%d%H%M").txt
