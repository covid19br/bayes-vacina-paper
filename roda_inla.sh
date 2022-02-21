#!/bin/bash

N=3
(
for estado in "RO" "AC" "AM" "RR" "PA" "AP" "TO" "MA" "PI" "CE" "RN" "PB" "PE" "AL" "SE" "BA" "MG" "ES" "RJ" "SP" "PR" "SC" "RS" "MS" "MT" "GO" "DF"; do
#for estado in "SP" "MT" "PI" "RN" "RO" "RR" "SE" "AL" "AP"; do
# for estado in "MA" "MS" "RO" "RR" "TO" "AP"; do
	for outcome in "covid"; do
		((i=i%N)); ((i++==0)) && wait
		Rscript modelo_inla_shift.R "$estado" "$outcome" &
	done
done
echo "done"
)
