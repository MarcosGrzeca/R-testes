#!/bin/bash

cd /home/ec2-user/R-testes/drunk/
Rscript /home/ec2-user/R-testes/drunk/teste_planilha.R > testes/result_$(date -d "today" +"%Y%m%d%H%M").txt 2> testes/erro_$(date -d "today" +"%Y%m%d%H%M").txt
