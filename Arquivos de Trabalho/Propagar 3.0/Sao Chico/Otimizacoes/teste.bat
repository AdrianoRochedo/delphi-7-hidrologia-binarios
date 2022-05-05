@echo off
title "Propagar --> LP-Solver"
prompt $G
echo.
echo ATENCAO:
echo A leitura dos resultados pelo Propagar somente estara disponivel apos o termino deste processo.
echo on
"F:\Projetos\Hidrologia\Programas\Redes\Propagar 3.0\Bin\Solvers\LpSolve\LP_Solve.exe"  -min -wafter -s -si -se -presolve -presolverow -presolvecol -presolvel -presolves -presolver -simplexdp -B5 -BB -v5 -t -d -ia -lp "F:\Projetos\Arquivos de Trabalho\Propagar 3.0\Sao Chico\Otimizacoes\teste.LP" > "F:\Projetos\Arquivos de Trabalho\Propagar 3.0\Sao Chico\Otimizacoes\teste.res"
pause
