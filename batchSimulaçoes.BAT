@echo off
::limpa secao
title Simulacao
::titulo barra superior
color f8
::background and text color
FOR /L %%A IN (1,1,1000) do (
	SimulacaoBACKUP.Rexec
	Simulacao.Rexec	
	java -jar beast.jar Aquilegia_simulacoes.xml
	move Aquileguia.log Aquileguia%%A.log
	move Aquileguia_Matrix.log Aquileguia_Matrix%%A.log
	move Matriz.txt Matriz%%A.txt
)
pause