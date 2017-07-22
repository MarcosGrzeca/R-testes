#!/bin/bash

cd /home/ec2-user/R-testes/icwsm-2016/
Rscript /home/ec2-user/R-testes/icwsm-2016/new.R > resultados/aa_result_$(date -d "today" +"%Y%m%d%H%M").txt 2> resultados/aa_erro_$(date -d "today" +"%Y%m%d%H%M").txt
