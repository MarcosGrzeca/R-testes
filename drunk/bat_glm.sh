#!/bin/bash

cd /home/ec2-user/R-testes/drunk/
Rscript /home/ec2-user/R-testes/drunk/glm.R > resultados/glm/result_$(date -d "today" +"%Y%m%d%H%M").txt 2> resultados/glm/erro_$(date -d "today" +"%Y%m%d%H%M").txt
