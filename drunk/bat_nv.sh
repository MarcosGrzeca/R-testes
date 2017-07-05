#!/bin/bash

cd /home/ec2-user/R-testes/drunk/
Rscript /home/ec2-user/R-testes/drunk/nv.R > resultados/nv/result_$(date -d "today" +"%Y%m%d%H%M").txt 2> resultados/nv/erro_$(date -d "today" +"%Y%m%d%H%M").txt
