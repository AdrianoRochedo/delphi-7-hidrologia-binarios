unit wsFuncoesDeDistribuicao;

interface
uses SysUtils,
     wsMatrix,
     wsVec;
     //wsDistribuicoesRandomicas;

(* ======================== Tabelas de distribuicoes ============================*)
function BetaTable(pVec, aVec, bVec: TwsDFVec): TwsGeneral;
function BinomTable(nVec: TwsLIVec; pVec: TwsDFVec): TwsGeneral;
function BinNegTable(xVec: TwsLIVec; rVec, pVec: TwsDFVec): TwsGeneral;
function ExpoTable(pVec, bVec: TwsDFVec): TwsGeneral;
function FTable(pVec, nVec, dVec: TwsDFVec): TwsGeneral;
function GammaTable(xVec, aVec: TwsDFVec): TwsGeneral;
function TStudentTable(pVec, dfVec: TwsDFVec): TwsGeneral;
function GeomTable(xVec: TwsLIVec; pVec: TwsDFVec): TwsGeneral;
function StdNornalTable(zVec: TwsDFVec): TwsGeneral;
function PoissonTable(xVec: TwsLIVec; LamVec: TwsDFVec): TwsGeneral;
function ChiSquareTable(pVec, dfVec: TwsDFVec): TwsGeneral;
function WeibullTable(pVec, aVec, bVec: TwsDFVec): TwsGeneral;
function HypergeomTable(PopVec, SucVec, nVec: TwsLIVec): TwsGeneral;

implementation
Uses Math,
     SysUtilsEx,
     wsConstTypes,
     wsProbabilidade;

{ ====================== Tabelas de Distribuicoes ==========================}
{ Objetivo
    Obter uma tabela de pontos criticos da distribuicao Beta acumulada
  Parametros
    pVec: Vetor com as probabilidades acumuladas para obtencao dos valores criticos
    aVec: Vetor com os valores do parametro alfa
    bVec: Vetor com os valores do parametro beta
  Observacoes
    Se alguma excecao ocorrer, a funcao retorna nil
}
function BetaTable(pVec, aVec, bVec: TwsDFVec): TwsGeneral;
var
  Lin,i,k,j: Integer;
  Dist: TwsProbBeta;
begin
  Dist := nil;
  try
    try
      Result := TWSGeneral.Create(aVec.Len*bVec.Len+1, pVec.Len+2);
      with Result do
        begin
        MLab := 'Valores Críticos Unilaterais da Distribuição Beta';
        ColName[1] := 'Alfa';
        ColName[2] := 'Beta';
        RowName[1] := 'ProbAcum';
        Data[1,1]:=wscMissValue;
        Data[1,2]:=wscMissValue;
        for i := 1 to pVec.Len do
          begin
          ColName[i+2] := 'p_'+IntToStr(i);
          Data[1,i+2]:= pVec[i]
          end;
        end;
      Dist := TwsProbBeta.Create(aVec[1],bVec[1]);
      Dist.Upper := True;
      Lin := 2;
      for i:= 1 to aVec.Len do
        begin
        Dist.Alpha := aVec[i]; // atualiza parametro Alfa
        for k := 1 to bVec.Len do
          begin
          Result[Lin,1] := aVec[i];
          Result[Lin,2] := bVec[k];
          Dist.Beta := bVec[k]; // atualiza parametro Beta
          for j := 3 to pVec.Len+2 do // Para cada probabilidade
            Result[Lin,j] := Dist.Quantil(pVec[j-2]);
          inc(Lin);
          end; // Para cada valor de Beta
        end; // Para cada valor de alfa
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // BetaTable

{ Objetivo
    Obter uma tabela de probabilidades da distribuicao Binomial acumulada
  Parametros
    nVec: Vetor de com os numeros de tentativas
    pVec: Vetor com as probabilidades de sucesso
  Observacoes
    O resultado retorna nil caso alguma excecao ocorra
}
{========================== Tabela da Binomial ===========================}
function BinomTable(nVec: TwsLIVec; pVec: TwsDFVec): TwsGeneral;
var
  i,j,k,n,Lin,Col: Integer;
  Dist: TwsProbBinomial;
begin
  Dist := nil;
  n := 0;
  for i := 1 to nVec.Len do
    Inc(n, nVec[i]+1);
  try
    try
      Result := TWSGeneral.Create(n+1,pVec.Len+2);  //soma 2 para dar espaco para n e x
      with Result do
        begin
        MLab := 'Distribuição Binomial Acumulada';
        ColName[1] := 'N_Exper';
        ColName[2] := 'Ate_x';
        RowName[1] := 'Prob_Suc';
        Data[1,1]:=wscMissValue;
        Data[1,2]:=wscMissValue;
        for i := 1 to pVec.Len do
          begin
          ColName[i+2] := 'p_'+IntToStr(i);
          Data[1,i+2] := pVec[i]
          end;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;

      Dist := TwsProbBinomial.Create(nVec[1], pVec[1]);

      n:= 2; //n sera a linha para armazenar o valor na matriz de Resultados
      for i:= 1 to nVec.Len do //para cada numero de experimentos
        begin
        //Atualiza numero de experimentos
        Dist.NumExp := nVec[i];
        Col := 2;
        for j := 1 to pVec.Len do //para cada probabilidade
          begin
          //Atualiza probabilidade de sucesso
          Dist.ProbSuc := pVec[j];
          //Calcula as probabilidades
          Dist.Prob(nVec[i]);
          //e acessa via propriedade Accum
          Inc(Col);
          for k := 0 to nVec[i] do
            begin
            Lin := n+k;
            Result[Lin,1]:= nVec[i];
            Result[Lin,2]:= k;
            Result[Lin,Col] := Dist.Accum[k+1];
            end; // for k
          end; // for j - proxima probabilidade
          Inc(n, nVec[i]+1); // Linha para o proximo n
        end; // for i
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // BinomTable

{======================= Tabela da Binomial Negativa ========================}
{
  Objetivo
    Obter uma tabela de probabilidades da distribuicao binomial negativa
  Parametros
    xVec: Vetor com os valores de X (Numero de tentativas ate que se alcance
          o numero desejado de sucessos)
    r: Vetor com os numeros de sucessos desejados
    pVec: Vetor com as probabilidades de sucesso
  Observacao
    Se alguma excecao ocorre, o resultado retorna nil
}
function BinNegTable(xVec: TwsLIVec; rVec, pVec: TwsDFVec): TwsGeneral;
var
  j,n,L,Lin,i,k,Col: Integer;
  Dist: TwsProbBinNeg;
begin
  Dist := nil;
  n := 0;
  for i := 1 to xVec.Len do
    Inc(n, xVec[i]+1);
  try
    try
      Result := TWSGeneral.Create(rVec.Len*n+1,pVec.Len+2);  //soma 2 para dar espaco para r e x
      with Result do
        begin
        MLab := 'Distribuição Binomial Negativa Acumulada';
        ColName[1] := 'Num_Suc';
        ColName[2] := 'Ate_x';
        RowName[1] := 'Prob_Suc';
        Data[1,1]:=wscMissValue;
        Data[1,2]:=wscMissValue;
        for i := 1 to pVec.Len do
          begin
          ColName[i+2] := 'p_'+IntToStr(i);
          Data[1,i+2]:=pVec[i] // Armazena as probabilidades na primeira linha
          end;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;
      Dist := TwsProbBinNeg.Create(rVec[1], pVec[1]);
      n:= 1; //n sera a linha para armazenar o valor na matriz de Results
      for i:= 1 to rVec.Len do //para cada numero de sucessos informado
        begin
        //Atualiza numero de sucessos
        Dist.NumSuc := rVec[i];
        for L := 1 to xVec.Len do
          begin
          for j := 1 to pVec.Len do //para cada probabilidade de sucesso
            begin
            //Atualiza probabilidade de sucesso
            Dist.ProbSuc := pVec[j];
            //Calcula as probabilidades para todos os valores de 0 a x
            Dist.Prob(xVec[L]);
            Col := j+2;
            //Descarrega as probabilidades na matriz de Results
            for k := 1 to xVec[L]+1 do
              begin
              Lin := n+k;
              Result[Lin,1]:= xVec[L];
              Result[Lin,2]:= k-1;
              // e faz o acesso via propriedade Accum
              Result[Lin,Col] := Dist.Accum[k];
              end; // esgota as linhas para um determinado x
            end; // para cada p
          Inc(n,xVec[L]+1);
          end; //para cada x
        end; // para cada r
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // BinNegTable

{ Objetivo
   Obtem tabela com valores criticos da distribuicao exponencial para probabilidades
   especificadas
  Parametros
    pVec: Vetor com as probabilidades para obtencao dos valores criticos
    bVec: Vetor com os valores do parametro beta
  Observacoes
    A funcao retorna nil se alguma excecao ocorrer
}
{========================== Tabela da Exponencial ===========================}
function ExpoTable(pVec, bVec: TwsDFVec): TwsGeneral;
var
  n,i,j: Integer;
  Dist: TwsProbExponential;
begin
  //pVec: Valores das probabilidades acumuladas
  //bVec: Valores do parametro beta
  Dist := nil;
  try
    try
      Result := TWSGeneral.Create(bVec.Len+1,pVec.Len+1);
      with Result do
        begin
        MLab := 'Valores Críticos Unilaterais da Distribuição Exponencial';
        ColName[1]:='Lambda';
        Data[1,1] := wscMissValue;
        for j := 2 to pVec.Len+1 do
          begin
          ColName[j] := 'p_'+IntToStr(j-1);
          Data[1,j] := pVec[j-1]
          end
        end;
      // Cria objeto para o calculo dos pontos criticos
      Dist := TwsProbExponential.Create(bVec[1]);
      Dist.Upper := True;
      for j := 1 to bVec.Len do
        begin
        Dist.Lambda := bVec[j];
        n := j+1;
        Result[n,1] := bVec[j];        
        for i := 1 to pVec.Len do
          Result[n,i+1] := Dist.Quantil(pVec[i]);
        end;
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free
  end;
end; // ExpoTable

{ ======================== Tabela da Distribuicao F ======================== }
{ Objetivo
    Obtem tabela com valores criticos da distribuicao F para probabilidades e graus de
    liberdade especificados
  Parametros
    pVec: Vetor com as probabilidades
    nVec: Vetor com os graus de liberdade do numerador
    dVec: Vetor com os graus de liberdade do denominador
  Observacoes
    Se ocorrer uma excecao a funcao retornara nil
}
function FTable(pVec, nVec, dVec: TwsDFVec): TwsGeneral;
var
  Lin,i,k,j: Integer;
  Dist: TwsProbF;
begin
  Dist := nil;
  try
    try
      Result := TWSGeneral.Create(pVec.Len*dVec.Len+1,nVec.Len+2);
      with Result do
        begin
        MLab := 'Valores Críticos Unilaterais da Distribuição F';
        ColName[1] := 'GLDen';
        ColName[2] := 'Alfa';
        RowName[1] := 'GLNum';
        Data[1,1] := wscMissValue;
        Data[1,2] := wscMissValue;
        for j := 3 to nVec.Len+2 do
          begin
          ColName[j] := 'GLNum_'+IntToStr(j-2);
          Data[1,j] := nVec[j-2]
          end
        end;
      // Cria objeto para obtencao dos valores criticos
      Dist := TwsProbF.Create(nVec[1],dVec[1]);
      Dist.Upper := True;
      Lin := 2;
      for i:= 1 to dVec.Len do
        begin
        Dist.DFDen := dVec[i];
        for k := 1 to pVec.Len do
          begin
          Result[Lin,1] :=dVec[i];
          Result[Lin,2] := pVec[k];
          for j := 3 to nVec.Len+2 do
            begin
            Dist.DFNum := nVec[j-2];
            Result[Lin,j] := Dist.Quantil(pVec[k]);
            end; // para cada grau de liberdade do numerador
          inc(Lin);
          end; // para cada probabilidade
        end; // para cada grau de liberdade do denominador
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // FTable

{ ======================== Tabela da Gama Incompleta ======================== }
{ Objetivo
   Obtem tabela da distribuicao gama incompleta para os valores de X e do parametro
   alfa especificados
  Parametros
    xVec: Vetor com os valores de criticos
    aVec: Vetor com os valores do parametro alfa
  Observacoes
    Se alguma excecao ocorrer, a funcao retornara um valor nil
}
function GammaTable(xVec, aVec: TwsDFVec): TwsGeneral;
var
  Lin,i,j: Integer;
  Dist: TwsProbGamma;
begin
  Dist := nil;
  try
    try
      Result := TWSGeneral.Create(xVec.Len+1,aVec.Len+1);
      with Result do
        begin
        MLab := 'Probabilidades Acumuladas da Distribuição Gama Incompleta';
        ColName[1] := 'Ate_x';
        Data[1,1]:=wscMissValue;
        RowName[1] := 'Alfa';
        for j := 2 to aVec.Len+1 do
          begin
          ColName[j] := 'Alfa_'+IntToStr(j-1);
          Data[1,j] := aVec[j-1]
          end;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;
      Dist := TwsProbGamma.Create(aVec[1]);
      //Dist.Upper := True;
      for j := 1 to xVec.Len do
        begin
        Lin := j+1;
        Result[Lin,1]:=j-1;
        for i := 1 to aVec.Len do
          begin
          Dist.Alpha:=aVec[i];
          Result[Lin,i+1]:= Dist.Prob(xVec[j]);
          end;
        end;
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // GammaTable

{ ======================== Tabela da Distribuicao t ======================== }
{ Objetivos
   Obtem uma tabela da distribuicao t de Student para as probabilidades e graus
   de liberadade especificados
  Parametros
    pVec: Probabilidades acumuladas para determinacao dos valores criticos
    dfVec: Vetor com os graus de liberdade
  Observacoes
    Caso ocorra alguma excecao, a funcao retornara valor nil
}
function TStudentTable(pVec, dfVec: TwsDFVec): TwsGeneral;
var
  Lin,i,j: Integer;
  Dist: TwsProbTStudent;
begin
  Dist := nil;
  try
    try
      Result := TWSGeneral.Create(dfVec.Len+1,pVec.Len+1);
      with Result do
        begin
        MLab := 'Valores Críticos Unilaterias da Distribuição t de Student';
        ColName[1] := 'GrausLib';
        RowName[1] := 'Prob_Acum';
        Data[1,1]:=wscMissValue;
        for j := 2 to pVec.Len+1 do
          begin
          ColName[j] := 'p_'+IntToStr(j-1);
          Data[1,j] := pVec[j-1]
          end;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;
      Dist := TwsProbTStudent.Create(dfVec[1]);
      Dist.Upper := True;
      //Dist.OneSided := False;
      for i := 1 to dfVec.Len do
        begin
        Dist.DF:=dfVec[i]; // Atualiza os graus de liberdade
        Lin := i+1;
        Result[Lin,1]:=dfVec[i];
        for j := 1 to pVec.Len do
          Result[Lin, j+1]:= Dist.Quantil(pVec[j]);
        end
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // TStudentTable

{============================= Tabela Distribuicao Geometrica ================ }
{ Objetivo
   Obter tabela da distribuicao geometrica para valores de X e da probabilidade
   de sucesso especificados
  Parametros
   xVec: Vetor com os valores de X
   pVec: Vetor com a sprobabilidades de sucesso
  Observaceos
   Caso ocorra uma excecao, a funcao retornara nil
}
function GeomTable(xVec: TwsLIVec; pVec: TwsDFVec): TwsGeneral;
var
  j,n,Lin,i,k,Col: Integer;
  Dist: TwsProbGeometric;
begin
  n := 0;
  //Valores de X para o calculo das probabilidades
  for i := 1 to xVec.Len do
    Inc(n, xVec[i]+1);
  try
    try
      Result := TwsGeneral.Create(n+1,pVec.Len+1);
      with Result do
        begin
        MLab := 'Tabela da Distribuição Geométrica Acumulada';
        ColName[1] := 'Ate_x';
        RowName[1] := 'ProbSuc';
        Data[1,1]:=wscMissValue;
        //Em pVec estao os valores da probabilidade de sucesso da Geometrica
        for i := 1 to pVec.Len do
          begin
          ColName[i+1] := 'p_'+IntToStr(i);
          Data[1,i+1] := pVec[i];
          end;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;
      //Inicializa objeto
      Dist := TwsProbGeometric.Create(pVec[1]);
      n := 1;
      for i := 1 to pVec.Len do
        begin
        Dist.ProbSuc := pVec[i];
        for j := 1 to xVec.Len do
          begin
          //Calcula todas as probabilidades
          Dist.Prob(xVec[j]);
          //Descarrega as probabilidades na matriz de saida
          Col := j+1;
          for k := 1 to xVec[j]+1 do
            begin
            Lin := n+k;
            Result[Lin,1]:= k-1;
            // e faz o acesso via propriedade Accum
            Result[Lin,Col] := Dist.Accum[k];
            end;
          end;// para cada valor de X
          Inc(n,xVec[i]+1);
      end;
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // GeomTable

{========================= Tabela Distribuicao Normal Padrao  ================ }
{ Objetivo
    Obter tabela de probabilidades da distribuicao normal padrao
  Parametros
    zVec: Vetor com os valores da variavel normal padrao
  Observacoes
    Caso ocorra uma excecao, a funcao retornara nil
}
function StdNornalTable(zVec: TwsDFVec): TwsGeneral;
var
  i: Integer;
  Dist: TwsProbStdNormal;
begin
  try
    try
      Result := TwsGeneral.Create(2,zVec.Len);
      with Result do
        begin
        MLab := 'Probabilidades Superiores da Distribuicao Normal Padrao';
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        RowName[1]:='Valor_x';
        RowName[2]:='Prob_Sup';
        end;
      Dist := TwsProbStdNormal.Create;
      Dist.Upper := True;
      for i := 1 to zVec.len do
        begin
        Result[1,i]:=zVec[i];
        Result[2,i]:=Dist.Prob(zVec[i]);
        end;
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // StdNornalTable

{============================= Tabela Distribuicao de Poisson ================ }
{ Objetivo
    Obter uma tabela de probabilidades acumuladas da distribuição de Poisson. A função
    retornará uma matriz com as probabilidades desejadas em forma de tabela.
  Parâmetros
    xVec  : Vetor com os valores de X
    LamVec: Vetor com os valores do parâmetro Lambda
  Observações
    - Caso ocorra alguma exceção, a função retornará nil.
    - Para cada valor de X, retornarão as probabilidades de 0 até esse valor, para cada
      valor do parâmetro Lambda.
}
function PoissonTable(xVec: TwsLIVec; LamVec: TwsDFVec): TwsGeneral;
var
  i,k,Lin,
  Col,ix : Integer;
  Dist   : TwsProbPoisson;
begin
  try
    try
      k:=0;
      for i:=1 to xVec.Len do
        Inc(k,xVec[i]+1);
      Result := TwsGeneral.Create(k+1,LamVec.Len+1);
      with Result do
        begin
        MLab := 'Tabela da Distribuição Acumulada de Poisson';
        ColName[1] := 'Ate_x';
        RowName[1] := 'Lambda';
        Data[1,1] := wscMissValue;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;
      //Inicializa objeto
      Dist := TwsProbPoisson.Create(LamVec[1]);
      for i:=1 to LamVec.Len do
        begin
        Dist.Lambda:=LamVec[i];
        Result[1,i+1]:=LamVec[i];
        Result.ColName[i+1]:='Lam_'+IntToStr(i);
        Lin:=1;
        //Calcula todas as probabilidades
        for ix:=1 to xVec.Len do
          begin
          Dist.Prob(xVec[ix]);
          Col := i+1;
          for k := 0 to xVec[ix] do
            begin
            Inc(Lin);
            Result[Lin,1]:= k;
            // e faz o acesso via propriedade Accum
            Result[Lin,Col] := Dist.Accum[k+1]
            end
          end // para cada valor de X
        end; // Para cada valor de lambda
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // PoissonTable

{ ======================== Tabela da Distribuicao Qui-Quadrado ======================== }
function ChiSquareTable(pVec, dfVec: TwsDFVec): TwsGeneral;
var
  Lin,i,j: Integer;
  Dist: TwsProbChiSqr;
begin
  try
    try
      Result := TWSGeneral.Create(dfVec.Len+1,pVec.Len+1);
      with Result do
        begin
        MLab := 'Valores Críticos Unilaterais da Distribuição Qui-Quadrado';
        ColName[1] := 'GrausLib';
        RowName[1] := 'Prob_Acum';
        Data[1,1] := wscMissValue;
        for j := 2 to pVec.Len+1 do
          begin
          ColName[j] := 'p_'+IntToStr(j-1);
          Data[1,j] := pVec[j-1]
          end;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;

      Dist := TwsProbChiSqr.Create(dfVec[1]);
      Dist.Upper := True;
      for i := 1 to dfVec.Len do
        begin
        Dist.DF:=dfVec[i]; // Atualiza os graus de liberdade
        Lin := i+1;
        Result[Lin,1]:=dfVec[i];
        for j := 1 to pVec.Len do
          Result[Lin, j+1]:= Dist.Quantil(pVec[j]);
        end;
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // ChiSquareTable

{ ======================== Tabela da Distribuicao de Weibull ======================== }
{ Objetivo
   Obtem tabela de probabilidades da distribuicao de Weibull
  Parametros
    pVec: Vetor com as probabilidades acumuladas
    aVec: Vetor com os valores do parametro alfa
    bVec: Vetor com os valores do parametro beta
  Observacoes
    caso ocorra alguma excecao, a funcao retornara nil
}
function WeibullTable(pVec, aVec, bVec: TwsDFVec): TwsGeneral;
var
  Lin,i,k,j: Integer;
  Dist: TwsProbWeibull;
begin
  try
    try
      Result := TWSGeneral.Create(bVec.Len*aVec.Len+1,pVec.Len+2);
      with Result do
        begin
        MLab := 'Valores Críticos da Distribuição Acumulada de Weibull';
        ColName[1] := 'Lambda';
        ColName[2] := 'Alfa';
        RowName[1] := 'Prob_Acum';
        Data[1,1]:=wscMissValue;
        Data[1,2]:=wscMissValue;
        for j := 3 to pVec.Len+2 do
          begin
          ColName[j] := 'p_'+IntToStr(j-2);
          Data[1,j]:=pVec[j-2]
          end
        end;
      // Cria objeto para obtencao dos valores criticos
      Dist := TwsProbWeibull.Create(aVec[1],bVec[1]);
      Dist.Upper := True;
      Lin := 2;
      for i:= 1 to aVec.Len do
        begin
        Dist.Lambda := aVec[i];
        for k := 1 to bVec.Len do
          begin
          Dist.Alpha := bVec[k];
          Result[Lin,1] := aVec[i];
          Result[Lin,2] := bVec[k];
          for j := 1 to pVec.Len do
            Result[Lin,j+2]:= Dist.Quantil(pVec[j]);
          Inc(Lin)
          end
        end
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // WeibullTable

{ ====================== Tabela da Distribuicao Hipergeometrica ======================}
function HypergeomTable(PopVec, SucVec, nVec: TwsLIVec): TwsGeneral;
{ Objetivo
    Obtem tabela de probabilidades acumuladas da distribuicao hipergeometrica
  Parametros
    PopVec: Vetor com os tamanhos de populacao
    SucVec: Vetor com os tamanhos dos grupos de sucesso
    nVec: Vetor com os tamanhos de amostras
  Observacoes
    Caso ocorra uma excecao, a funcao retornara nil
}
var
  j,n,L,Lin,
  i,k,Col   : Integer;
  Dist      : TwsProbHyperGeom;
begin
  n := 0;
  for i := 1 to nVec.Len do
    Inc(n, nVec[i]+1);
  try
    try
      //soma 3 na dimensao das colunas para dar espaco para N, M e x
      Result := TWSGeneral.Create(PopVec.Len*n+1,SucVec.Len+3);
      with Result do
        begin
        MLab := 'Distribuição Hipergeométrica Acumulada';
        ColName[1] := 'Tam_Popul';
        ColName[2] := 'Tam_Amost';
        ColName[3] := 'Ate_x';
        RowName[1] := 'Grupo_Suc';
        Data[1,1]:=wscMissValue;
        Data[1,2]:=wscMissValue;
        Data[1,3]:=wscMissValue;
        for i := 1 to SucVec.Len do
          begin
          ColName[i+3] := 'M_'+IntToStr(i);
          Data[1,i+3]:=SucVec[i]
          end;
        PrintOptions.ColWidth := 12;
        PrintOptions.ColPrecision := 5;
        end;
      Dist := TwsProbHyperGeom.Create(PopVec[1],SucVec[1],nVec[1]);
      n:=1;
      for i:= 1 to PopVec.Len do //para cada tamanho de populacao
        begin
        //Atualiza parametro N
        Dist.PopSize := PopVec[i];
        for j := 1 to nVec.Len do //para cada tamanho de amostra
          begin
          //Atualiza parametro n
          Dist.SampSize := nVec[j];
          for k := 1 to SucVec.Len do  // para cada tamanho do grupo de sucessos
            begin
            // Atualiza parametro M
            Dist.SucSize := SucVec[k];
            // Calcula as probabilidades para todos os valores de 0 a nVec[j]
            Dist.Prob(nVec[j]);
            Col := k+3;
            //Descarrega as probabilidades calculadas na matriz de Result
            for L := 1 to nVec[j]+1 do
              begin
              Lin := n+L;
              Result[Lin,1]:= PopVec[i];
              Result[Lin,2]:= nVec[j];
              Result[Lin,3]:= L-1;
              // e faz o acesso via propriedade Accum
              Result[Lin,Col] := Dist.Accum[L]
              end;
            end; // for i: para cada tamanho do grupo de sucessos
            Inc(n, nVec[j]+1);
          end // for j: para cada tamanho de amostra
        end; // for i: para cada tamanho de populacao
    except
      Result.Free;
      Result := nil;
    end;
  finally
    Dist.Free;
  end;
end; // HypergeomTable

end.
