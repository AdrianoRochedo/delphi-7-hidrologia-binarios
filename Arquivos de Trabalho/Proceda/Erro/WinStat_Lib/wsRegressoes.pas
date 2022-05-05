unit wsRegressoes;

{ Verificar melhor forma de atender as especificacoes do usuario para saida
  Constroi ?
  Imprime ?
  Armazena ?
  Acho que o melhor e TwsLMManager possuir um array de boleanos (ou ele seria especifico
  de cada classe ?) de estabeleca cada opcao como True ou False. Para cada classe, esse
  array possuiria uma especificacao

  06/03/97   - BugFix em TwsUReg.CrossProduct      - Amauri e Roger

  13/05/99
    - Conserto de problemas nos metodos de selecao de variaveis
    - Modificacao de AddColDesc para AddColEx
    - Adequacao da rotina que imprime estatisticas das variaveis do modelo

  27/11/99 Reformulacao completa do processo para considerar mais que uma resposta no modelo.
    Em vez que criar duas matrizes para os produtos cruzados, obtem apenas uma. Metodo
    CrossProduct e WCrossProduct e a matriz de produtos cruzados foram transferidos para
    TwsLinearModel

  29/11 Modificacao da informacao da variavel resposta para tornar o metodo Coefficients
    compativel com os demais e virtual. Um campo foi adicionado (RespVar). Adequacao de
    todos os metodos relacionados

  08/2000
    Classe basica para analise de regressao. Incorporacao da matriz de incidencia e matriz
    X em todas as classes de analise de regressao. Implementacao da regressao ponderada.
    Implementacao do metodo para obtencao de um subconjunto de estatisticas para diagmostico
}

interface
Uses Classes,
     wsMasterModel,
     wsMatrix,
     wsVec,
     wsGLib,
     wsTabelaDeSimbolos,
     ModuloComum;
Type
{  Heranca
     TwsUReg --> TwsLinearModel --> TObject
   Objetivo
     Executa a analise de regressao linear multipla univariada de acordo com o modelo estabelecido. Dentre outras, obtem
     as estimativas dos coeficientes, quadro da analise, analise de influencia e colinearidade.
}
  TwsUReg = Class(TwsLinearModel)
    private
      MSRes,                  // Quadrado Medio do Residuo
      SSq,                    { Nas primeiras NPred posicoes Sq armazena as SQ Desvios das variáveis
                               preditoras. Nas posicoes NPred+1 até NPred+NResp armazena SQ Desvios
                               das variáveis preditoras.}
      SSC,                    // Soma de quadrados das colunas de C = Inv(X'X)*X'
      H,                      // Diagonal da matriz H
      msr      : TwsVec;      // Quadrado médio do resíduo para uso em rotinas específicas

      MatC     :TwsGeneral;   // Matriz C'=X'Inv(X´X)
      TempXY   :TwsSymmetric; // Copia da matriz de produtos cruzados para uso posterior
      procedure HatMatrix;
      procedure Fit; override;
      Destructor Destroy; override;
    Public
      OutInf,                    // Conjunto de dados para estatisticas de influencia
      CpResid,                   // Conjunto de dados para componente mais resíduo
      ParResid,                  // Conjunto de dados para residuos parciais
      Pred       :TwsDataSet;    // Subconjnto de estatisticas
      XMatExtr   : TwsGeneral;
      RespVar    : Integer;      // Variavel resposta em analise
      Constructor Create(Manager: TwsLMManager; SXY: Boolean=False);
      // Obtem as correlacoes entre as variaveis
      procedure VarCorr;
      // Obtem as correlacoes entre os coeficientes
      procedure CoefCorr;
      // Obtem a matriz de covariancias
      procedure Covar;
      // Obtem os coeficientes de regressao
      procedure Coefficients; override;
      // Obtem a tabela de analise da variacao
      procedure AnovaTable(Desc: Boolean);
      // Analise de colinearidade
      Procedure Collin;
      // Obtem estatisticas descritivas para as variaveis
      procedure GetStat(df: double);
      // Analise de influencia completa por observacao
      Procedure Influence;
      // Analise de influencia com conjunto reduzido de estatisticas
      Procedure Predict;
      // Testes de hipoteses lineares
      procedure LinearHypothesis(C: TwsGeneral);
  End; { TwsUReg }

{  Heranca
     TwsModelSelect --> TwsUReg --> TwsLinearModel --> TObject
   Objetivo
     Classe basica para os processos de selecao de variaveis.
}
  TwsModelSelect = class(TwsUReg)
  private
    FLastR2: double;                               // Valor R2 do passo anterior
    FSummary: TwsDataSet;                         // Resumo do processo de seleção das variáveis
    procedure UpdateSummary(k: Integer); virtual; // Atualiza tabela com resumo do processo
    destructor Destroy; override;
  public
    constructor Create(Manager: TwsLMManager; SaveXY: Boolean);
    // Inclui/exclui uma variável no modelo
    procedure Fit1(k: Integer);
  end; {TwsModelSelect}

{  Heranca
     TwsAllReg --> TwsModelSelect --> TwsUReg --> TwsLinearModel --> TObject
   Objetivo
     Classe para implementacao do processo de selecao Todas as Regressoes Possiveis.
}
  TwsAllReg = class(TwsModelSelect)
  private
   //Atualiza tabela com resumo do processo
    procedure UpdateSummary(k: Integer); override;
  public
    constructor Create(Manager: TwsLMManager; Num, ColOrder: Integer);
    // Executa procedimento Todas as Regressoes Possiveis
    procedure AllRegressions;
  end;

{  Heranca
     TwsStepSelection --> TwsModelSelect --> TwsUReg --> TwsLinearModel --> TObject
   Objetivo
     Classe basica para implementacao do processo de selecao passo a passo.
}
  TwsStepSelection = class(TwsModelSelect)
  private
    FTableIn,                                                     // Tabela das variaveis dentro do modelo
    FTableOut: TwsGeneral;                                        // Tabela das variáveis fora do modelo
    Step: Integer;                                                // Passo do Processo
    //Atualiza tabelas das variáveis dentro e fora
    procedure UpdateInOut;
    //Variavel entra no modelo?
    function VarIn(var k: Integer; const alfa: double): Boolean;
    //Variavel sai do modelo?
    function VarOut(var k: Integer; const alfa: double): Boolean;
    Destructor Destroy; override;
  public
    Constructor Create(Manager: TwsLMManager; SaveXY: Boolean);
  end;{TwsStepSelection}

{  Heranca
     TwsBackwardElimination --> TwsStepSelection --> TwsModelSelect --> TwsUReg --> TwsLinearModel --> TObject
   Objetivo
     Classe para implementacao do processo de selecao passo a passo Backward Elimination.
}
  TwsBackwardElimination = class(TwsStepSelection)
  private
    AlfaOut: double;           // Taxa de erro para retirada de variaveis do modelo
  public
    constructor Create(Manager: TwsLMManager; alfa: double);
    // Executa os metodo Backward Elimination
    procedure Backward;
  end;

  {  Heranca
     TwsForwardSelection --> TwsStepSelection --> TwsModelSelect --> TwsUReg --> TwsLinearModel --> TObject
   Objetivo
     Classe para implementacao do processo de selecao passo a passo forward selection.
}
  TwsForwardSelection = class(TwsStepSelection)
  private
    AlfaIn: double;            // Taxa de erro para retirada de variaveis do modelo
  public
    constructor Create(Manager: TwsLMManager; alfa: double);
    // Executa os metodo Forward Selection
    procedure ForwardSel;
  end;

  {  Heranca
     TwsStepwiseSelection --> TwsStepSelection --> TwsModelSelect --> TwsUReg --> TwsLinearModel --> TObject
   Objetivo
     Classe para implementacao do processo de selecao passo a passo stepwise selection.
}
  TwsStepwiseSelection = class(TwsStepSelection)
  private
    AlfaIn,                 // Taxa de erro para entrada
    AlfaOut: double;         // Taxa de erro para saida
  public
    constructor Create(Manager: TwsLMManager; alfa1,alfa2: double);
    // Executa os metodo Stepwise selection
    procedure Stepwise;
  end;

  {  Heranca
     TwsMReg --> TwsLinearModel --> TObject
   Objetivo
     Classe para implementacao de analise de modelos de regressao multipla.
}
  TwsMReg = Class(TwsLinearModel)
    private
      FXInv,                // Inversa de X´X
      FH,                   // Matriz da SQ&P da Hipotese
      FRes: TwsSymmetric;   // Matriz das SQ&P do Residuo
      FBeta: TwsGeneral;    // Matriz das estimativas dos parametros
      Destructor Destroy; override;
    Public
      // Acesso a matriz de parametros
      Property Beta: TwsGeneral Read FBeta Write FBeta;
      // Cria instancia para regressao multivariada
      Constructor Create(Manager: TwsLMManager);
      // Testes para hipoteses multivariadas especificadas
      Procedure HTest(C, W: TwsGeneral; var ErrCode: Word);
      // Intervalos de confianca simultaneos
      procedure SConfInterv(C, W: TwsGeneral; alpha: double; var ErrCode: Word);
      // Analise de regressao completa. Testa H0: B=0
      Procedure Fit;
  End; { TwsMReg }

implementation

Uses SysUtils, Forms,
     wsFuncoesDeProbabilidade,
     wsFuncoesDeEscalares,
     wsFuncoesDeDataSets,
     wsConstTypes,
     wsMath,
     Dialogs;

{ ===================== Analise de regressao univariada ================= }
Constructor TwsUReg.Create(Manager: TwsLMManager; SXY: Boolean = False);
{ Objetivo
    Cria o objeto para análise de modelos de regressão univariada ponderada. Se for definida
    uma variável peso, a matriz X e a matriz de SQ&P serão obtidas de acordo.
  Parâmetros
    Manager: Objeto gerenciador de modelos lineares
    SXY  : Se True, cria uma cópia da matriz de produtos cruzados para uso posterior. Em
           geral esta opção é utilizada pelos métodos de seleção de variáveis que operam
           sob variadas condições a matriz de produtos cruzados. Isto possibilita que
           esta matriz seja obtida apenas uma vez, já que é um dos pontos críticos da
           análise.
  Campos modificados
    DSToAnalysis
    NumObs
    H
    MatC
    MSRes
    msr
    NPar
    dsWIndex
    wInd
    nPred
    Colop
    FXY
    TempXY
    SSq
    Toler
  Métodos chamados
    Create herdado
    Add
    AddDataSet
    SendMessageToObjects
    GetIncMat
    GetXMat
    WCrossProduct ou CrossProduct
    Add
    GetStat
    VarCorr
    Collin
    Fit
  Saídas
    Conjunto de dados em análise   (I/S)
}
var
  i,nc: integer;
Begin
  Inherited Create(Manager);

  DSToAnalysis := TwsDataSet(Manager.FListFrame[Manager.DataIndex]);

  if Manager.Options[ord(cmPrintData)] then
     Manager.Output.Add(DSToAnalysis);

  if Manager.Options[ord(cmSaveData)] then
     Manager.ObjectCreated(DSToAnalysis);

  NumObs := 0;
  H := nil;
  MatC:=nil;
  OutInf:=nil;
  XMatExtr:=nil;
  Pred:=nil;
  CpResid:=nil;
  ParResid:=nil;

  MSRes  := TwsDFVec.Create(NResp);     // Quadrados medios do residuo
  msr    := TwsDFVec.Create(NResp);     // Auxiliar (QMRes) para os processos de selecao
  NPar   := 1;                          // Numero de parametros do modelo
  GetIncMat;                            // Obtem matriz de incidencia

  if Manager.Options[ord(cmVar_PrintMatInc3)] Then
     Manager.Output.Add(IncMat);

  if WVar then
     dsWIndex := DSToAnalysis.Struct.IndexOf(Manager.WeightVar);

  GetXMat; // Obtem matriz X

  if Manager.Options[ord(cmVar_PrintMatMod3)] then
     Manager.Output.Add(XMat);

  if Manager.Options[ord(cmVar_SaveMatMod3)] then
     Manager.ObjectCreated(XMat);

  if WVar then
    begin
    nc := xMat.NCols-2;                  // menos intercepto e peso
    ColOp:=Index(1,xMat.NCols-1);
    WCrossProduct(ColOp);          // Obtem a matriz de produtos cruzados
    ColOp.Free
    end
  else
    begin
    nc := xMat.NCols-1;                 // menos intercepto
    CrossProduct;                       // Obtem a matriz de produtos cruzados
    wTot:=NumObs
    end;

  NPred := nc-NResp;                    // Numero de variaveis preditoras no modelo
  if SXY then
    FXY.Copy(mtSymmetric, TwsMatrix(TempXY)) // Faz uma copia para uso posterior
  else
    TempXY := nil;

  ColOp := TwsLIVec(VecConst(1,FXY.NCols,False));         // Colunas operadas pelo sweep
  SSq := TwsDFVec.Create(nc);           // Somas de quadrados corrigidas para as variaveis
  Toler := TwsDFVec.Create(NPred+1);    // Tolerancias para o sweep
  ColOp[1] := -1;                       // Coluna da média já operada
  For i:=2 To nc+1 Do                   // Somas de quadrados para todas as variáveis
    SSq[i-1]:=FXY[i,i];
  For i:=1 To NPred Do                  // Valores das tolerancias para o Sweep
    Toler[i+1] := SSq[i]*1.0e-8;
  if Manager.Options[ord(cmEstatist)] Then GetStat(wTot-1);  // Estatisticas descritivas
  if Manager.Options[ord(cmCorrelVar)] Then VarCorr;         // Correlacoes entre as variaveis
  if Manager.Options[ord(cmColinear)] Then Collin;           // Faz analise de colinearidade
  Fit;                                                // Chama o metodo de ajustamento
End; { TwsUReg.Create }

Destructor TwsUReg.Destroy;
{ Objetivo
    Desaloca memória reservada para os campos de TwBaseUReg
  Campos alterados
    MSRes
    msr
    SSq
    SSC
    MatC
    H
    tempXY
    OutInf
    XMatExtr
    Pred
    CPresid
    ParResid
  Métodos chamados
    Destroy herdado
}
Begin
  Inherited Destroy;
  MSRes.Free;
  msr.Free;
  SSq.Free;
  SSC.Free;
  MatC.Free;
  H.Free;
  TempXY.Free;
  if not Manager.Options[ord(cmSaveInfluence)] then
    OutInf.Free;
  XMatExtr.Free;
  if not Manager.Options[ord(cmSavePredic)] then
     Pred.Free;
  CpResid.Free;
  ParResid.Free;
End; { TwsUReg.Destroy }

procedure TwsUReg.VarCorr;
{  Objetivo
     Obtém a matriz de correlações entre as variáveis. Pode ser chamada somente quando o
     modelo possuir intercepto.
   Campos alterados
     Nenhum
   Métodos chamados
     CorrMat
     Add
   Saídas
     Matriz de correlações entre as variáveis (I)
}
var
  XCor: TwsSymmetric;
  i: Integer;
begin
  XCor := TwsSymmetric(FXY.SubSymMat(2,Npred+1));
  XCor.Name:='Variaveis';
  With XCor do
    begin
    CorrMat;
    MLab := 'Correlações entre as variáveis do modelo';
    PrintOptions.ColWidth := 9;
    PrintOptions.ColPrecision := 5;
    for i := 1 to NCols do
      RowName[i] := FXY.ColName[i+1];
    Manager.Output.Add(XCor);
    Free
    End   // with
end; { TwsUReg.VarCorr }

procedure TwsUReg.CoefCorr;
{ Objetivo
    Obtém a matriz de correlações entre os coeficientes.
  Campos modificados
    nenhum
  Métodos chamados
    CorrMat
    Add
  Saídas
    Matriz de correlações entre os coeficientes        (I)
  Observações
    Este método deve ser chamado somente após a execução do método Fit
}
var
  XCor: TwsSymmetric;
  I,p : Word;
Begin
  p:= FXY.Nrows-NResp;
  XCor := TwsSymmetric(FXY.SubSymMat(1,p));
  With XCor do
    begin
    CorrMat;
    Name:='Variaveis';
    MLab := 'Correlações entre os coeficientes';
    PrintOptions.ColWidth := 12;
    PrintOptions.ColPrecision := 5;
    for i := 1 to p do
      RowName[i] := FXY.ColName[i];
    Manager.Output.Add(XCor);
    Free
    End    // with
end; { TwsUReg.CoefCorr }

procedure TwsUReg.Covar;
{ Objetivo
    Obtém a matriz de covariâncias entre os coeficientes.
  Campos modificados
    Nenhum
  Métodos chamados
    Add
  Saídas
    Matriz de covariâncias      (I)
  Observações
    Covar deve ser chamado somente apás a execução do método Fit
}
var
  XCov: TwsSymmetric;
  I,p : Word;
begin
  p:= FXY.Nrows-NResp;
  XCov := TwsSymmetric(FXY.SubSymMat(1,p));
  XCov.ByScalar(MSRes[RespVar],opProd,True,False);
  With XCov do
    begin
    MLab := 'Matriz de covariâncias';
    Name:='Variaveis';
    for i := 1 to p do
      RowName[i] := FXY.ColName[i];
    Manager.Output.Add(XCov);
    Free
    End // with
end; { TwsUReg.Covar }

procedure TwsUReg.Fit;
{ Objetivo
    Ajusta o modelo através da aplicação do Sweep nas colunas das variáveis preditoras.
  Campos  modificados
    . DFRes.
    . FXY
    . NPar
    . msr
    . MSRes
  Métodos chamados
    . RevSweep
    . CoefCorr
    . Add
    . HatMatrix
}
var
  i,j,p: Integer;
begin
  DFRes := NumObs-1; // Graus de liberdade do residuo comeca como total

  p := FXY.NRows-NResp;

  FXY.MLab := 'Inversa, coeficientes e SQ Res';
  // Muda o sinal da primeira coluna para compensar o inicio como SQ&P ajustada
  for j := 2 to FXY.NRows do FXY[j,1] := -FXY[j,1];

  // O loop seguinte e o coracao do processo de ajustamento
  for j := 2 to p do                   // Media já foi ajustada
    begin                              // Opera as colunas pelo Sweep
    FXY.RevSweep(j, Toler[j], ColOp);  // Opera coluna j com a precisao desejada
    if ColOp[j] = -1 then              // Se operou, ajusta num. parametros e GLRes
      begin
      Inc(NPar);
      DFRes := DFRes - 1
      end
    end;

  // Compensa troca de sinal da 1a. coluna das preditoras
  for j := 2 to p do FXY[j,1] := ColOp[j]*FXY[j,1];

  // Obtem quadrados medios do residuo para cada variavel dependente
  For i:=1 To NResp Do
    Begin
    FXY[p+i,1]:=-FXY[p+i,1]; // Compensa troca de sinal para respostas
    MSRes[i]:=FXY[p+i,p+i]/DFRes;
    msr[i] := MSRes[i]
    End;

  FXY.Name:='Variaveis';
 { Ver opcoes de impressao e salvamento }
  if Manager.Options[ord(cmInverse)] Then
     Manager.Output.Add(FXY);

  // Correlacao entre os coeficientes
  if Manager.Options[ord(cmPrintCorrel)] then
     CoefCorr;

  // Se vai fazer analise de influencia, obtem diagonal da matriz H
  HatMatrix;
end; { TwsUReg.Fit }

procedure TwsUReg.Coefficients; {30/08/98 }
{ Objetivo
    Obtém e testa os coeficientes dos termos do modelo de regressão
  Campos modificados
    Nenhum
  Métodos chamados
    Add
    AddDataSet
    SendMessageToObjects
  Saídas
    Conjunto de dados com os resultados  (I/S)
}
var
  CoefTable: TwsDataSet;
  Col      : TwsDataSetCol;
  v        : TwsVec;
  j,p      : Integer;
  Erro     : Word;
  t,b      : double;
begin
  p:=FXY.NRows-NResp+RespVar;
  CoefTable :=TwsDataSet.Create('Coefic');
  with CoefTable do
    begin
    MLab:='Estimativas dos parâmetros, testes e intervalos de confiança para '+
      FXY.RowName[p];
    ColIdentName:='Parametro';
    Struct.AddColEx(TwsNumeric.Create('Estimativa',
      'Valor estimado dos parâmetros de regressão',12,7));                             //1
    Struct.AddColEx(TwsNumeric.Create('ErrPadrao','Erro padrão da estimativa',12,7));   //2
    Struct.AddColEx(TwsNumeric.Create('T','Valor observado da estatística T',10,5));        //3
    Struct.AddColEx(TwsNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que T observado',10,5));     //4
    Struct.AddColEx(TwsNumeric.Create('Inf95',
      'Extremo inferior intervalo de confiança',12,7));                                //6
    Struct.AddColEx(TwsNumeric.Create('Sup95',
      'Extremo superior intervalo de confiança',12,7));                                //7
    Struct.AddColEx(TwsNumeric.Create('EstPadr','Estimativa Padronizada',12,7));       //8
    Struct.AddColEx(TwsNumeric.Create('VIF',
      'Fator de inflacionamento da variância',12,7));                                  //9
    Struct.AddColEx(TwsNumeric.Create('Toler','Tolerância',12,7));                    //10
    for j:=1 to ColOp.Len-NResp do
      // Se a variavel j foi incluida insere informacao sobre coeficiente
      if ColOp[j] = -1 then
        Begin
        v := TwsDFVec.Create(9);
        b:=FXY[p,j];
        v.Name := FXY.ColName[j];
        v[1]:=b;                                     // Coeficiente de regressao
        v[2]:=Sqrt(FXY[j,j]*msr[RespVar]);           // Erro padrao do coeficiente
        v[3]:=b/v[2];                                // Valor de T calculado
        v[4]:=TInt(v[3],DFRes,False,True,Erro);      // Prob. associada
        t:=TInv(0.05,DFRes,1.0e-10,True,False,Erro); // Limites para o intervalo a 5%
        v[5]:=b-t*v[2];                              // extremo inferior
        v[6]:=b+t*v[2];                              // extremo superior
        If j=1 Then
          Begin
          v[7]:=wscMissValue;
          v[8]:=wscMissValue;
          v[9]:=wscMissValue;
          End { If j=1 }
        Else
          Begin
          v[7]:=b*SQRT(SSq[j-1]/SSq[NPred+RespVar]);
          v[8]:=SSC[j]*SSq[j-1];
          v[9]:=ScalarInv(v[8]);
          End; { Else j=1 }
        MAdd(v);
        End; { if ColOp }

    if Manager.Options[ord(cmPrintCoef)] Then
       begin
       Manager.Output.Add(CoefTable);
       if Manager.AppDir <> '' then
          Manager.Output.WriteLink('Veja mais detalhes sobre Coeficientes do Modelo',
            Manager.AppDir + 'Ajuda\Modelos_Lineares\Coeficientes.htm');
       end;
    end; { with CoefTable }

    if Manager.Options[ord(cmSaveCoef)] Then
       Manager.ObjectCreated(CoefTable)
    else
      CoefTable.Free
end; { TwsUReg.Coefficients }

procedure TwsUReg.AnovaTable(Desc: Boolean);    {30/08/98}
{ Objetivo
    Constroi um conjunto de dados que armazena o quadro da análise da variação
  Campos modificados
    Nenhum
  Métodos chamados
    Add
    AddDataSet
    SendMessageToObjects
  Saídas
    Conjunto de dados com o resultado da análise da variação (I/S)
}
var
  Table:TwsDataSet;
  Col  :TwsDataSetCol;
  StAux: TwsGeneral;
  Erro : Word;
  p    : Integer;
  v    : TwsVec;
  x    : double;
begin
  p := FXY.NRows-NResp+RespVar;
  Table:=TwsDataSet.Create('Quadro');
  Table.PrintOptions.PrintDesc := Desc;
  With Table Do
    Begin
    MLab:='Quadro da Análise da Variação para '+FXY.RowName[p];
    ColIdentName:='Fontes';
    Struct.AddColEx(TwsNumeric.Create('GL','Graus de liberdade',6,5));                //1
    Struct.AddColEx(TwsNumeric.Create('SQ','Soma de quadrados'));                     //2
    Struct.AddColEx(TwsNumeric.Create('QM','Quadrado Médio'));                   //3
    Struct.AddColEx(TwsNumeric.Create('F','Valor observado da estatística F',12,7));            //4
    Struct.AddColEx(TwsNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que F observado',11,4));    //5
    v := TwsDFVec.Create(5);
    v.Name := 'Regressão';
    v[1]:=NPar-1;
    v[2]:=SSq[NPred+RespVar]-FXY[p,p];
    x:=v[2];
    v[3]:=v[2]/v[1];
    v[4]:=v[3]/msr[RespVar];
    v[5]:=FInt(v[4],v[1],DFRes,True,Erro);
    MAdd(v);
    v := TwsDFVec.Create(5);
    v.Name := 'Resíduo';
    v[1]:=DFRes;
    v[2]:=FXY[p,p];
    v[3]:=v[2]/v[1];
    v[4]:=wscMissValue;
    v[5]:=wscMissValue;
    MAdd(v);
    v := TwsDFVec.Create(5);
    v.Name := 'Total';
    v[1]:=NumObs-1;
    v[2]:=SSq[NPred+RespVar];
    v[3]:=wscMissValue;
    v[4]:=wscMissValue;
    v[5]:=wscMissValue;
    MAdd(v);
    { Verificar opcoes de impressao e armazenamento }
    if Manager.Options[ord(cmPrintVarAnalysis)] Then
       begin
       // Estatisticas auxiliares
       StAux:=TwsGeneral.Create(1,3);
       with StAux do
         begin
         Name:='Estat';
         MLab:='Estatísticas Auxiliares';
         RowName[1]:='Valor';
         ColName[1]:='DesvPadr'; ColName[2]:='CoefDet';ColName[3]:='CDetAjust';
         PrintOptions.ColWidth := 10;
         PrintOptions.ColPrecision := 5;
         Data[1,1]:=Sqrt(msr[RespVar]);                         // desvio padrao
         Data[1,2]:=1-(FXY[p,p]/SSq[NPred+RespVar]);            // R2
         Data[1,3]:=1-(NumObs-1)*(1-Data[1,2])/DFRes;           // r2 ajustado
         end;
       Manager.Output.Add(Table);
       Manager.Output.Add(StAux);
       if Manager.AppDir <> '' then
          Manager.Output.WriteLink('Veja mais detalhes sobre o Quadro da Análise',
            Manager.AppDir + 'Ajuda\Modelos_Lineares\Quadro da Analise.htm');
       StAux.Free
       end
    End; { With Table }

    if Manager.Options[ord(cmSaveVarAnalysis)] Then
       Manager.ObjectCreated(Table)
    else
       Table.Free;
end; { TwsUReg.AnovaTable }

procedure TwsUReg.Collin;
{ Objetivo
    Faz a análise de colinearidade entre as colunas da matriz X
  Métodos chamados
    Add
    RevSweep
    Eigen
    SortRows
  Saídas
    Conjunto de dados com a análise de colinearidade   (I)
  Observações
    Realiza a análise a partir da matriz de produtos cruzados.
}
var
  A         : TwsSymmetric;
  VarProp,B : TwsGeneral;
  D         : TwsDiagonal;
  v,Coln,Asc: TwsLIVec;
  aux       : TwsDFVec;
  i,j,np    : Integer;
  ErrCode   : Word;
  MaxAuto   : double;
begin
  np := xMat.NCols-NResp;
  A := TwsSymmetric(FXY.SubSymMat(1,np)); // Copia a matriz
  v := TwsLIVec(VecConst(1,np,False));
  v[1]:=-1;
  A.RevSweep(1,1.0e-9,v);                 //Retorna matriz de SQ&P não ajustada para média
  v.Free;
  B:=TwsGeneral.Create(np,np);
  for i := 1 to A.NRows do                // Escala para SQ unitárias para as colunas
    for j := 1 to i do
      begin
      B[i,j] := A[i,j]/Sqrt(A[i,i]*A[j,j]);
      if i <> j then B[j,i] := B[i,j]
      end;
  B.Eigen(D,True,i,True,1.0e-09,ErrCode);
  if ErrCode=0 then
    begin
    MaxAuto := D[1,1];
    aux:=TwsDFVec.Create(np);
    for i := 1 to np do
      begin
      aux[i] := 0;
      if D[i,i] > MaxAuto then MaxAuto := D[i,i];
      for j := 1 to np do
        begin
        B[i,j] := Sqr(B[i,j])/D[j,j];
        aux[i] := aux[i] + B[i,j]                             { v armazena os totais de linhas }
        end;
      end;
    VarProp := TwsGeneral.Create(np, np+2);
    VarProp.MLab:='Diagnóstico de Colinearidade';
    VarProp.Name:='Num';
    for i := 1 to np do
      begin
      VarProp[i,1] := D[i,i];                  // Autovalor
      VarProp[i,2] := Sqrt(MaxAuto/D[i,i]);    // Indice de condicao
      end; { For i }
    VarProp.ColName[1] := 'Autovalor';
    VarProp.ColName[2] := 'IndCond';
    For i:=3 To VarProp.NCols Do
      begin
      VarProp.ColName[i]:= FXY.ColName[i-2];
      for j := 1 to np do
        begin
        VarProp.RowName[j]:=IntToStr(j);
        VarProp[j,i] := B[i-2,j]/aux[i-2]
        end
      end;

    Coln:=TwsLIVec(VecConst(1,1,False));
    Asc := TwsLIVec(VecConst(0,1,False));
    VarProp.SortRows(Coln,Asc);
    Manager.Output.Add(VarProp);
    if Manager.AppDir <> '' then
       Manager.Output.WriteLink('Veja mais detalhes sobre Colinearidade',
         Manager.AppDir + 'Ajuda\Modelos_Lineares\Colinearidade');
    Aux.Free; Coln.Free; Asc.free; VarProp.Free;
    end;
  A.Free; B.Free; D.Free;
end; { TwsUReg.Collin }

procedure TwsUReg.GetStat(df: double);
{ Objetivo
    Obtém um conjunto de estatísticas básicas relativas às variáveis do modelo. As
    estatísticas são: Mínimo, máximo, média, soma de quadrados, variância e desvio padrão
  Campos modificados
    Nenhum
  Métodos chamados
    Add
  Saídas
    Matriz com as estatísticas  (I)
}
var
  i,i1,n: Integer;
  Stat  : TwsGeneral;
begin
  n:=FXY.NRows-1;
  Stat := TwsGeneral.Create(n, 4);
  with Stat do
    begin
    Name := 'Variaveis';
    MLab := 'Estatísticas para as variáveis do modelo';
    ColName[1]:= 'Media'; ColName[2]:= 'SQCorrig';
    ColName[3]:= 'Variancia'; ColName[4]:= 'DesvPadr';
    PrintOptions.ColPrecision := 7;
    for i := 1 to n do
      begin               { Variaveis preditoras }
      i1 := i+1;
      RowName[i] := FXY.ColName[i1];
      Data[i,1] := FXY[i1,1];                  { Media }
      Data[i,2] := FXY[i1,i1];                 { Soma de quadrados }
      Data[i,3] := ScalarDiv(Data[i,2],df);    { Variancia }
      Data[i,4] := ScalarSqrt(Data[i,3]);      { Desvio padrao }
      end;
    end; { With }

  Manager.Output.Add(Stat);
  Stat.Free;
end; { GetStat }

procedure TwsUReg.HatMatrix; { 23/09/98 }
{ Objetivo
    Obtem a diagonal da matriz H e a matriz C = Inv(X'X)*X'. Considera os valores da variável
    peso, se ela existir
  Campos modificados
    SSC
    MatC
    h
  Métodos chamados
    Nenhum
}
var
  i,i2,j,
  p,vObs : Integer;
  x, c   : TwsVec;
  w      : double;
begin
  { Somas de quadrados das colunas de C. Separar Colocar em uma rotina independente }
  p := FXY.NRows;
  SSC:=VecConst(0,NPar);
  MatC:=TwsGeneral.Create(0,NPar);
  MatC.Name:='MatC';
  { Diagonal da matriz H e matriz C'=X*Inv(X'X) }
  h:=TwsDFVec.Create(NumObs);
  vObs:=0;
  For i:=1 To xMat.nRows Do
    Begin
    x:=xMat.Row[i];
    if not x.HaveMissValue then                       // se x tem valor perdido
      begin
      Inc(vObs);
      c:=TwsDFVec.Create(NPar);
      if WVar then
        w:=Sqrt(xMat[i,wInd])
      else
        w:=1;
      For i2:=1 To p-NResp Do
        Begin
        c[i2]:= 0;
        For j := 1 To p-NResp Do
          c[i2] := c[i2]+ w*x[j]*FXY[j,i2];
        End; { For i2 }
      For j:=1 To C.Len Do
        SSC[j]:=SSC[j]+Sqr(c[j]);
      MatC.MAdd(C);
      h[vObs]:=0;
      For j:=1 To p-NResp Do
        h[vObs]:=h[vObs]+c[j]*w*x[j];
      end
    End; { For i }
end; { HatMatrix }

Procedure TwsUReg.Influence; {30/08/98}
{  Objetivo
     Executa análise de influência por observação. Constroi um conjunto de dados com as
     estatisticas de diagnostico, um conjunto de dados com residuos parciais e outro com os
     componentes + residuos. Cria também duas matrizes: numa são armazenados os valores
     extremos das preditoras e noutra os valores limites para interpretação das estatísticas
     de diagnóstico. As estatísticas são
      . Valor estimado
      . Diagonal da matriz H
      . Residuo
      . Resíduo Ponderado
      . Erro padrão do valor estimado
      . Erro padrão do resíduo
      . Resíduo estudentizado internamente
      . Resíduo estudentizado externamente
      . Efeito no ajustamento (padronizado)
      . Distância de Cook
      . Distância de Cook modificada
      . Distância de Mahalanobis
      . Razão de covariâncias
      . Extremo inferior intervalo de confiança (média)
      . Extremo superior intervalo de confiança (média)
      . Extremo inferior intervalo de confiança (predição)
      . Extremo superior intervalo de confiança (predição)
      . Estatística de Andrews e Pregibon
      . Efeito no ajustamento do coeficiente b
  Campos modificados
    OutInf
    CpResid
    ParResid
    XMatExtr
  Métodos chamados
    Add
    AddDataSet
    SedMessageToObjects
  Saídas
    Conjunto de dados com estatísticas de influência  (I/S)
    Valores limites para estatísticas de innfluência  (I  )
}
  Const
    nStat = 19;
    StatName :Array [1..nStat] Of String =
       ('Estim','DiagH','Residuo','ResPond','EPEstim','EPResid','RSInt','RSExt','DFitS',
        'DCook','DCookM','DMahalan','RazCov','MInf95','MSup95','PredI95','PredS95',
        'AndPreg','DFBetaS');
    ps: array[1..nStat-1] of byte =
        (12,9,12,12,12,12,9,9,9,9,9,9,9,12,12,12,12,9);
    pf: array[1..nStat-1] of byte =
        (7,4,7,7,7,7,4,4,4,4,4,4,4,7,7,7,7,4);
    StatLab : Array [1..NStat] of string =
     ('Valor estimado',
      'Diagonal da matriz H',
      'Residuo',
      'Resíduo Ponderado',
      'Erro padrão do valor estimado',
      'Erro padrão do resíduo',
      'Resíduo estudentizado internamente',
      'Resíduo estudentizado externamente',
      'Efeito no ajustamento (padronizado)',
      'Distância de Cook',
      'Distância de Cook modificada',
      'Distância de Mahalanobis',
      'Razão de covariâncias',
      'Extremo inferior intervalo de confiança (média)',
      'Extremo superior intervalo de confiança (média)',
      'Extremo inferior intervalo de confiança (predição)',
      'Extremo superior intervalo de confiança (predição)',
      'Estatística de Andrews e Pregibon',
      'Efeito no ajustamento do coeficiente b');
  Var
   Obj        : TwsNumeric;
   Col        : TwsQualitative;
   Stat,                   // Estatisticas
   X,                      // Linha do conjunto de dados
   CpRes,                  // Componente mais residuo
   Res,                    // Residuos parciais
   C          : TwsVec;     // Linha da matriz C
   Aux, Aux1,
   h1,si,
   r,t,tval   : double;
   i,i2,j1,
   j,k,ly,
   vObs  : Integer;
   s          : string;
   Limits     : TwsGeneral; // Limites para as estatisticas de influência
   Erro       : Word;
   Idx        : TwsLIVec;   // Indices das preditoras em X
   xOk        : boolean;    // True se a linha tem vp
Begin
  ly:=NPar+RespVar;     // Linha da resposta RespVar na matriz FXY
  OutInf.Free;
  // Espacos liberados somente se este metodo for chamado na mesma analise
  CpResid.Free;                   { Conjunto de dados para componente mais resíduo }
  ParResid.Free;                  { Conjunto de dados para residuos parciais }
  { Extr das variav predit nas primeiras duas linhas e dos resíduos parciais nas duas seguintes}
  XMatExtr.Free;
  XMatExtr := Nil;
  { XMatEtr
    Linha 1. Minimos de cada variável preditora
    Linha 2. Máximos de cada variável preditora
    Linha 3. Mínimos dos resíduos parciais
    Linha 4. Máximos dos resíduos parciais
  }
  XMatExtr := TwsGeneral.Create(8, NPred);
  for i := 2 to NPred+1 do
    begin
    k := i-1;
    XMatExtr.ColName[k] := FXY.ColName[i];
    XMatExtr[1,k] := -MinFloatValue;
    XMatExtr[2,k] := MinFloatValue;
    XMatExtr[3,k] := -MinFloatValue;
    XMatExtr[4,k] := MinFloatValue;
    end;

  OutInf:=TwsDataSet.Create(Copy(FXY.RowName[ly],1,4)+'Infl');   // Influencia
  OutInf.MLab:='Valores observados, resíduos e medidas de influência para '+FXY.RowName[ly];

  { Conjunto de dados para armazenar residuos parciais e componente mais residuo }
  { Verificar opcoes para residuos parciais }
  ParResid :=TwsDataSet.Create(Copy(FXY.RowName[ly],1,4)+'ParRes');
  ParResid.MLab:='Resíduos parciais para '+FXY.RowName[ly];

  { Verificar opcoes para comp.+residuos }
  CpResid :=TwsDataSet.Create(Copy(FXY.RowName[ly],1,4)+'CpRes');
  CpResid.MLab:='Componente mais resíduo para '+FXY.RowName[ly];

  // Variavel resposta em analise
  OutInf.Struct.AddColEx(TwsNumeric(CopyDescCol(DSToAnalysis.Struct.Col[RIdx[RespVar]])));
  For i:=2 To NPred+1 Do    { Para cada variavel preditora }
    Begin
    { Inclui variavel preditora em OutInf }
    s:=FXY.ColName[i];
    OutInf.Struct.AddColEx(TwsNumeric.Create(s,'',12,7));
    { e em CpResid }
    CpResid.Struct.AddColEx(TwsNumeric.Create(s,'',12,7));
    { Componente + residuo }
    CpResid.Struct.AddColEx(TwsNumeric.Create(s+'#CR','Componente + Residuo para '+s,12,7));
    { Residuo X.[i] }
    ParResid.Struct.AddColEx(TwsNumeric.Create(s, 'Residuos parciais para '+s,12,7));
    { Residuo Y.[i] }
    ParResid.Struct.AddColEx(TwsNumeric.Create(FXY.RowName[ly]+'#'+s,
      'Resíduos ajustamento para preditoras exceto '+s,12,7));
    End; { For i }

  { Define descritores de cada estatistica }
  For i:=1 To 3 Do
    OutInf.Struct.AddColEx(TwsNumeric.Create(StatName[i],StatLab[i],ps[i],pf[i]));
  if WVar then
    For i:=4 To nStat-1 Do  // Inclui o residuo ponderado
      OutInf.Struct.AddColEx(TwsNumeric.Create(StatName[i],StatLab[i],ps[i],pf[i]))
  else
    For i:=5 To nStat-1 Do
      OutInf.Struct.AddColEx(TwsNumeric.Create(StatName[i],StatLab[i],ps[i],pf[i]));

  { Descritores para DFBetaS }
  For i:=1 To NPar Do
    Begin
    s := FXY.ColName[i];
    OutInf.Struct.AddColEx(TwsNumeric.Create(s+'B','Efeito no ajust. do coef. de '+s,7,4));
    End; { For i }

  Col:=TwsQualitative.Create('TipoObs','Tipo de observação: DI - Disc ou influente, INV - Inválida',4);
  Col.AddLevel('Ok');
  Col.AddLevel('DI');
  Col.AddLevel('INV');
  OutInf.Struct.AddColEx(Col);

  // Limites para as estatisticas de diagnostico
  if Manager.Options[ord(cmPrintInfluence)] or Manager.Options[ord(cmSaveInfluence)] Then
    begin
    Limits := TwsGeneral.Create(9,2); { Limites para estatisticas de influencia }
    with Limits do
      begin
      MLab:='Critérios para localização de observações influentes';
      PrintOptions.ColPrecision := 5;
      Name:='Estatisticas';
      ColName[1]:='LimInf';
      ColName[2]:='LimSup';
      RowName[1] := StatName[7];                          {RSInt}
      Data[1,1]:=-2;
      Data[1,2]:=2;
      RowName[2] := StatName[8];                          {RSExt}
      Data[2,1]:=-2;
      Data[2,2]:=2;
      RowName[3] := StatName[10];                          {DCook}
      Data[3,1]:=wscMissValue;
      Data[3,2]:=FInv(0.5,NPar,DFRes,1.0e-10,True,Erro);
      RowName[4] := StatName[11];                         {DCookM}
      Data[4,1]:=-2;
      Data[4,2]:=2;
      RowName[5] := StatName[9];                          {DFitS}
      t:=2*sqrt(NPar/NumObs);
      Data[5,1]:=-t;
      Data[5,2]:=t;
      RowName[6] := StatName[13];                         {RazCov}
      t:=3*NPar/NumObs;
      Data[6,1]:=1-t;
      Data[6,2]:=1+t;
      RowName[7] := StatName[2];                          {DiagH}
      Data[7,1]:=wscMissValue;
      Data[7,2]:=2*NPar/NumObs;
      RowName[8] := StatName[18];                         {AndPreg}
      Data[8,1]:=1-2*(NPar+1)/NumObs;
      Data[8,2]:=wscMissValue;
      RowName[9] := StatName[19];                         {DFBetaS}
      Data[9,1]:=wscMissValue;
      Data[9,2]:=Sqrt(4/NumObs);
      Manager.Output.Add(Limits);
      end  // with
    end;

  { Obtencao das estatisticas por variavel observacao }
  tval:=TInv(0.05,DFRes,1.0e-10,True,False,Erro);   { Valor t para intv. confiança 5% }
  Idx:=Index(2,NPred+1);                           // Indices das preditoras
  vObs:=0;                                         // vObs controla obs valida
  For i:=1 To xMat.NRows Do
    Begin
    Stat:=TwsDFVec.Create(OutInf.NCols);           // vetor com as estatísticas
    x:=xMat.Row[i];                                // linha i de xMat
    xOk := not x.HaveMissValue;                         // Linha tem valor perdido?
    if xOk Then
      begin
      Inc(vObs);
      C:=MatC.Row[vObs];
      h1:=h[vObs];
      end
    else
      h1:=wscMissValue;
    Stat[1]:=x[ly];                                // valor da resposta
    For i2:=2 To NPred+1 Do                        // valores das preditoras
      Stat[i2]:=x[i2];
    // Somente obtem valor estimado se todos os x sao validos
    if not x.LocMiss(Idx) then
      begin
      Aux:=FXY[ly,1];
      For i2:=2 To NPred+1 Do
        Aux:=Aux+x[i2]*FXY[ly,i2]
      end
    else
      Aux:=wscMissValue;

    r:=ScalarSub(x[ly],Aux);
    si := MSRes[RespVar];
    k:=NPred+2;                                    // preditoras + resposta
    if WVar then                                   // se existe variavel peso
      begin
      r:=ScalarProd(Sqrt(xMat[i,wInd]),r);         // pondera o residuo
      With Stat Do
        Begin
        Data[k]:=Aux;                                              {1. Estim}
        if not FEquals(r,wscMissValue) then
          begin
          Data[k+1]:=h1;                                           {2. DiagH}
          Data[k+2]:=x[ly]-Aux;                                    {3. Residuo}
          Data[k+3]:=r;                                            {4. Residuo ponderado}
          Data[k+4]:=SQRT(h1*si);                                  {5. EPEstim}
          Aux:=1-h1;
          t:=DFRes/(DFRes-1)*si-SQR(r)/((DFRes-1)*Aux);
          Data[k+ 5]:=SQRT(Aux*si);                                {6. EPResid}
          try
            Data[k+ 6]:=r/Data[k+5];                                 {7. RSInt}
            Data[k+ 7]:=r/SQRT(Aux*t);                               {8. RSExt}
            Data[k+ 8]:=Data[k+7]*SQRT(h1/Aux);                      {9. DFitS}
            Data[k+ 9]:=(h1/Aux)*SQR(Data[k+6])/NPar;                {10. DCook}
            Data[k+10]:=Data[k+7]*SQRT((h1/Aux)*(DFRes/NPar));       {11. DCookM}
                                                                     {12. DMahalan}
            Data[k+11]:=(h1-1/NumObs)/Aux*(NumObs*(NumObs-2)/(NumObs-1));
                                                                     {13. RazCov}
            Data[k+12]:=ScalarPower((DFRes-SQR(Data[k+6]))/(DFRes-1),NPar)/Aux;
            Data[k+13]:=Data[k]-tval*Data[k+4];                      {14. MInf95%}
            Data[k+14]:=Data[k]+tval*Data[k+4];                      {15. MSup95%}
            Data[k+15]:=Data[k]-tval*SQRT((1+h1)*si);                {16. PredI95%}
            Data[k+16]:=Data[k]+tval*SQRT((1+h1)*si);                {17. PredS95%}
            Data[k+17]:=Aux*(1-Sqr(Data[k+6])/DFRes);                {18. AndPreg}
          except
            for i2:=6 to 17 do
              Data[k+ i2]:=wscMissValue;
          end;
          j1:=k+nStat-2;
          For i2:=1 To C.Len Do                                    {19... DFBetaS}
            Begin
            j:=j1+i2;
            try
              Data[j]:=C[i2]/SQRT(SSC[i2])*(Data[k+7]/Sqrt(aux));
            except
              Data[j]:=wscMissValue;
            end;
            End
          end
        End; { With Stat }
      end
    else
      With Stat Do
        Begin
        Data[k]:=Aux;                                           {1. Estim}
        if not FEquals(r,wscMissValue) then
          begin
          Data[k+1]:=h1;                                          {2. DiagH}
          Data[k+2]:=r;                                           {3. Residuo}
          Data[k+3]:=SQRT(h1*si);                                 {4. EPEstim}
          Aux:=1-h1;
          try
            t:=DFRes/(DFRes-1)*si-SQR(r)/((DFRes-1)*Aux);
            Data[k+4] :=SQRT(Aux*si);                               {5. EPResid}
            Data[k+5] :=r/Data[k+4];                                {6. RSInt}
            Data[k+6] :=r/SQRT(Aux*t);                              {7. RSExt}
            Data[k+7] :=Data[k+6]*SQRT(h1/Aux);                     {8. DFitS}
            Data[k+8] :=(h1/Aux)*SQR(Data[k+5])/NPar;               {9. DCook}
            Data[k+9] :=Data[k+6]*SQRT((h1/Aux)*(DFRes/NPar));      {10. DCookM}
                                                                    {11. DMahalan}
            Data[k+10] :=(h1-1/NumObs)/Aux*(NumObs*(NumObs-2)/(NumObs-1));
                                                                    {12. RazCov}
            Data[k+11] :=ScalarPower((DFRes-SQR(Data[k+5]))/(DFRes-1),NPar)/Aux;
            Data[k+12]:=Data[k]-tval*Data[k+3];                     {13. MInf95%}
            Data[k+13]:=Data[k]+tval*Data[k+3];                     {14. MSup95%}
            Data[k+14]:=Data[k]-tval*SQRT((1+h1)*si);               {15. PredI95%}
            Data[k+15]:=Data[k]+tval*SQRT((1+h1)*si);               {16. PredS95%}
            Data[k+16]:=Aux*(1-Sqr(Data[k+5])/DFRes);               {17. AndPreg}
          except
            for i2:=4 to 16 do
              Data[k+i2] :=wscMissValue;
          end;

          j1:=k+nStat-3;
          For i2:=1 To C.Len Do                                    {19... DFBetaS}
            Begin
            j:=j1+i2;
            try
              Data[j]:=C[i2]/SQRT(SSC[i2])*(Data[k+6]/Sqrt(aux));
            except
              Data[j]:=wscMissValue;
            end;
            End
          end
        End; { With Stat }

    if FEquals(r,wscMissValue) then
      begin
      Stat[Stat.Len]:=2;
      for i2:=k+1 to Stat.Len-1 do
        Stat[i2]:=wscMissValue;
      end
    else
      //  DiagH                      RSExt                 DFitS
      if (Stat[k+1]>Limits[7,2]) or (Abs(Stat[k+6])>2) or (Abs(Stat[k+7])>Limits[5,2])
          // DCookM
         or (Abs(Stat[k+9])>2) then Stat[Stat.Len]:=1
      else
         Stat[Stat.Len]:=0;

    OutInf.MAdd(Stat);      { Insere no cjto de estatísticas de influência }

    { Componente + Resíduo e Resíduos parciais }
    Res:=TwsDFVec.Create(2*NPred);
    CpRes := TwsDFVec.Create(2*NPred);
    for j := 1 to NPred do
      begin
      i2:=(j-1)*2;
      t:=FXY[ly,j+1];
      Aux1:=x[j+1];              { variavel preditora }
      CpRes[i2+1] := Aux1;
      { Obtem extremos de cada preditora }
      if xOk then
        begin
        if XMatExtr[1, j] > Aux1 then XMatExtr[1, j] := Aux1;
        if XMatExtr[2, j] < Aux1 then XMatExtr[2, j] := Aux1;
        CpRes[i2+2]:=Aux1*t+r;                               { Comp. + residuo }

        Aux1 := C[j+1]/SSC[j+1];
        Res[i2+1]:=Aux1;                  { Residuo parcial X.[j] }
        { Obtem extremos dos residuos de cada preditora ajustada pelas demais }
        if XMatExtr[3, j] > Aux1 then XMatExtr[3, j] := Aux1;
        if XMatExtr[4, j] < Aux1 then XMatExtr[4, j] := Aux1;
        Res[i2+2]:=Aux1*t+r                     { Residuo parcial Y.[j] }
        end
      else
        begin
        CpRes[i2+2]:=wscMissValue;
        Res[i2+2]:=Aux1*t+r                     { Residuo parcial Y.[j] }
        end
      end; { for j }

    ParResid.MAdd(Res);     { Insere no cjto com residuos parciais }
    CpResid.MAdd(CpRes);    { Insere no cjto de componente mais resíduo }
    End; { For i }

  Limits.Free;
  for i := 5 to 8 do
    for j := 1 to NPred do
      XMatExtr[i,j] := XMatExtr[i-4,j]*FXY[ly,j+1];

  if Manager.Options[ord(cmPrintInfluence)] Then
     begin
     Manager.Output.Add(OutInf);
     if Manager.AppDir <> '' then
        Manager.Output.WriteLink('Veja mais detalhes sobre Análise de Influência',
          Manager.AppDir + 'Ajuda\Modelos_Lineares\Influencia.htm');
     end;

  if Manager.Options[ord(cmSaveInfluence)] Then   // Insere uma copia para manter matriz original
     Manager.ObjectCreated(OutInf)

End; { TwsUReg.Influence }

Procedure TwsUReg.Predict;
{  Objetivo
     Constrói um conjunto de dados com valores das variáveis preditoras, resposta e algumas
     estatísticas de diagnóstico. As estatísticas são
      . Valor estimado
      . Diagonal da matriz H
      . Residuo
      . Resíduo Ponderado
      . Resíduo estudentizado externamente
      . Efeito no ajustamento (padronizado)
  Campos modificados
    Pred
  Métodos chamados
    Add
    AddDataSet
    SedMessageToObjects
  Saídas
    Conjunto de dados com estatísticas de influência  (I/S)
}
  Const
    NStat = 6;
    StatName :Array [1..nStat] Of String =
       ('Estim','DiagH','Residuo','ResPond','RSExt','DFitS');
    ps: array[1..nStat] of byte =
        (12,9,12,12,9,9);
    pf: array[1..nStat] of byte =
        (7,4,7,7,4,4);
    StatLab : Array [1..NStat] of string =
     ('Valor estimado',
      'Diagonal da matriz H',
      'Residuo',
      'Resíduo Ponderado',
      'Resíduo estudentizado externamente',
      'Efeito no ajustamento (padronizado)');
  Var
   Stat,                   // Estatisticas
   X          : TwsVec;    // Linha do conjunto de dados
   Idx        : TwsLIVec;  // Indices das preditoras
   Aux,h1,si,
   r,t        : double;
   i,i2,k,
   ly,vObs    : Integer;
   Col        : TwsQualitative;
   Limits     : TwsGeneral;
Begin
  ly:=NPar+RespVar;     // Linha da resposta RespVar na matriz FXY

  Pred.Free;

  Pred:=TwsDataSet.Create(Copy(FXY.RowName[ly],1,4)+'Pred');   // Influencia
  Pred.MLab:='Valores observados, resíduos e medidas de influência para '+FXY.RowName[ly];

  // Variavel resposta em analise
  Pred.Struct.AddColEx(TwsNumeric(CopyDescCol(DSToAnalysis.Struct.Col[RIdx[RespVar]])));
  For i:=2 To NPred+1 Do    { Para cada variavel preditora }
    { Inclui variavel preditora em Pred }
    Pred.Struct.AddColEx(TwsNumeric.Create(FXY.ColName[i],'',12,7));

  { Define descritores para cada estatística }
  For i:=1 To 3 Do
    Pred.Struct.AddColEx(TwsNumeric.Create(StatName[i],StatLab[i],ps[i],pf[i]));
  if WVar then
    For i:=4 To nStat Do  // Inclui o residuo ponderado
      Pred.Struct.AddColEx(TwsNumeric.Create(StatName[i],StatLab[i],ps[i],pf[i]))
  else
    For i:=5 To nStat Do
      Pred.Struct.AddColEx(TwsNumeric.Create(StatName[i],StatLab[i],ps[i],pf[i]));

  Col:=TwsQualitative.Create('TipoObs','Tipo de observação: DI - Disc ou influente, INV - Inválida',4);
  Col.AddLevel('Ok');
  Col.AddLevel('DI');
  Col.AddLevel('INV');
  Pred.Struct.AddColEx(Col);

  // Limites para as estatisticas de diagnostico
  Limits := TwsGeneral.Create(3,2); { Limites para estatisticas de influencia }
  with Limits do
    begin
    MLab:='Critérios para localização de observações influentes';
    Name:='Estatisticas';
    ColName[1]:='LimInf';
    ColName[2]:='LimSup';
    RowName[1] := StatName[5];                          {RSExt}
    PrintOptions.ColPrecision := 5;
    Data[1,1]:=-2;
    Data[1,2]:=2;
    RowName[2] := StatName[6];                          {DFitS}
    t:=2*sqrt(NPar/NumObs);
    Data[2,1]:=-t;
    Data[2,2]:=t;
    RowName[3] := StatName[2];                          {DiagH}
    Data[3,1]:=wscMissValue;
    Data[3,2]:=2*NPar/NumObs;
    Manager.Output.Add(Limits);
    end;  // with

  // Obtencao das estatisticas por variavel observacao
  k:=NPred+2;
  Idx:=Index(2,NPred+1);
  vObs:=0;
  For i:=1 To xMat.NRows Do
    Begin
    Stat:=TwsDFVec.Create(Pred.NCols);           // vetor com as estatísticas
    x:=xMat.Row[i];                              // linha i de xMat
    Stat[1]:=x[ly];                              // valor da resposta
    { Obtem os valores estimados }
    Aux:=FXY[ly,1];
    if x.LocMiss(Idx) then                       // se x tem valor perdido
      Aux:=wscMissValue                          // retorna valor perdido
    else
      For i2:=2 To NPred+1 Do                    // senao, valor estimado
        Aux:=Aux+x[i2]*FXY[ly,i2];
    For i2:=2 To NPred+1 Do                      // valores das preditoras
      Stat[i2]:=x[i2];
    // Se y eh perdido ou tem algum valor perdido em x
    r:=ScalarSub(x[ly],Aux);
    if not FEquals(r,wscMissValue) then
      begin
      Inc(vObs);
      h1:=h[vObs]
      end
    else
      h1:=wscMissValue;
    si := MSRes[RespVar];
//***
    if WVar then
      begin
      r:=ScalarProd(Sqrt(xMat[i,wInd]),r);
      With Stat Do
        Begin
        Data[k   ]:=Aux;                                        {1. Estim}
        Data[k+ 1]:=h1;                                         {2. DiagH}
        Data[k+ 2]:=ScalarSub(x[ly],Aux);
        Data[k+ 3]:=r;                                          {4. Residuo ponderado}
        try
        if not FEquals(r,wscMissValue) then
          begin
          Aux:=1-h1;
          t:=DFRes/(DFRes-1)*si-SQR(r)/((DFRes-1)*Aux);
          Data[k+ 4]:=r/SQRT(Aux*t);                              {8. RSExt}
          Data[k+ 5]:=Data[k+4]*SQRT(h1/Aux);                     {9. DFitS}
          end
        else
          begin
          Data[k+ 4]:=wscMissValue;                              {8. RSExt}
          Data[k+ 5]:=wscMissValue                               {9. DFitS}
          end
        except
          Data[k+ 4]:=wscMissValue;                              {8. RSExt}
          Data[k+ 5]:=wscMissValue                               {9. DFitS}
        end
        End; { With Stat }
      end
    else
      With Stat Do
        Begin
        Data[k   ]:=Aux;                                        {1. Estim}
        Data[k+ 1]:=h1;                                         {2. DiagH}
        Data[k+ 2]:=ScalarSub(x[ly],Aux);                       {3. Residuo}
        try
          if not FEquals(r,wscMissValue) then
            begin
            Aux:=1-h1;
            t:=DFRes/(DFRes-1)*si-SQR(r)/((DFRes-1)*Aux);
            Data[k+ 3]:=r/SQRT(Aux*t);                              {8. RSExt}
            Data[k+ 4]:=Data[k+3]*SQRT(h1/Aux);                     {9. DFitS}
            end
          else
            begin
            Data[k+ 3]:=wscMissValue;                              {8. RSExt}
            Data[k+ 4]:=wscMissValue;                              {9. DFitS}
            end
        except
          Data[k+ 3]:=wscMissValue;                              {8. RSExt}
          Data[k+ 4]:=wscMissValue;                     {9. DFitS}
        end;
        End; { With Stat }
    if FEquals(r,wscMissValue) then
      Stat[Stat.Len]:=2
    else
      //  DiagH                      RSExt                 DFitS
      if (Stat[k+1]>Limits[3,2]) or (Abs(Stat[k+3])>2) or (Abs(Stat[k+4])>Limits[2,2]) then
        Stat[Stat.Len]:=1
      else
         Stat[Stat.Len]:=0;

      Pred.MAdd(Stat);      { Insere no cjto de estatísticas de influência }
    End; { For i }

  Limits.Free;
  Idx.Free;
  if Manager.Options[ord(cmPrintPredic)] Then
     Manager.Output.Add(Pred);

  if Manager.Options[ord(cmSavePredic)] Then   // Insere uma copia para manter matriz original
     Manager.ObjectCreated(Pred)
End; { TwsUReg.Predict }

procedure TwsUReg.LinearHypothesis(C: TwsGeneral);
{ Objetivo
    Executa testes de hipóteses lineares nos modelos de regressão. Faz um teste global e um
    para cada hipótese linear (um grau de liberdade) definida.
  Parâmetros
    C: Matriz cujo número de colunas deve ser o número de parâmetros do modelo e cada linha
       contém os coeficientes dos parâmetros, correspondentes a cada hipótese linear.
  Campos modificados
    Nenhum
  Métodos chamados
    Add
    AddDataSet
    SendMessageToObjects
  Saídas
    Conjunto de dados com informações sobre os testes     (I/S)
}
var
  i,j,k,ly: Integer;
  aux,aux1: double;
  CX      : TwsSymmetric;
  b,v,vaux: TwsDFVec;
  Err     : Word;
  Table   : TwsDataSet;
  Col     : TwsDataSetCol;
begin
  if C.NCols <> NPar then
    Begin
    MessageDlg('Matriz dos coeficientes com dimensão inadequada',mtInformation,[mbOk],0);
    Exit
    end;
  ly:=NPar+RespVar;
  Table:=TwsDataSet.Create('Quadro');
  With Table Do
    Begin
    MLab:='Quadro da Análise da Variação - Hipóteses Lineares - para '+FXY.RowName[ly];
    ColIdentName:='Fontes';
    Struct.AddColEx(TwsNumeric.Create('GL','Graus de liberdade',6,5));                //1
    Struct.AddColEx(TwsNumeric.Create('SQ','Soma de quadrados'));                     //2
    Struct.AddColEx(TwsNumeric.Create('QM','Quadrado Médio'));                   //3
    Struct.AddColEx(TwsNumeric.Create('F','Valor observado da estatística F',12,7));            //4
    Struct.AddColEx(TwsNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que F observado',11,4));    //5
    end; // with Table
  vaux:=TwsDFVec.Create(NPar); b:=TwsDFVec.Create(C.NRows);
  CX:=TwsSymmetric.Create(C.NRows);
  // Constroi as quantidades
  for i:=1 to C.NRows do
    begin
    aux1:=0;
    for j:=1 to NPar do
      begin
      aux1:=aux1+C[i,j]*FXY[ly,j];
      aux:=0;
      for k:=1 to NPar do
        aux:=aux+C[i,k]*FXY[k,j];
      vaux[j]:=aux
      end; // for j
    b[i]:=aux1;
    for j:=1 to i do
      begin
      aux:=0;
      for k:=1 to NPar do
        aux:=aux+vaux[k]*C[j,k];
      CX[i,j]:=aux
      end // for j
    end; // for i
  vaux.Free; vaux:=TwsDFVec.Create(C.NRows);
  // armazena valores da diagonal (para as hipoteses com 1 GL) - CX pode ser invertida
  for i:=1 to C.NRows do
    vaux[i]:=CX[i,i];

  // Testa hipotese conjunta, se for necessario
  if C.NRows > 1 then
    begin
    CX.Inv(Err);
    v := TwsDFVec.Create(6);
    v.Name:='HipGlobal';
    v[1]:=C.NRows;
    v[2]:=CX.TranspMul6(b,Err);
    v[3]:=v[2]/v[1];
    v[4]:=v[3]/msr[RespVar];
    v[5]:=FInt(v[4],v[1],DFRes,True,Err);
    Table.MAdd(v);
    end;
  // Testa hipoteses com um grau de liberdade
  aux:=0; aux1:=0;
  for i:=1 to C.NRows do
    begin
    v := TwsDFVec.Create(5);
    v.Name:=C.RowName[i];
    v[1]:=1;
    v[2]:=ScalarSqr(ScalarDiv(b[i],vaux[i]));
    v[3]:=v[2];
    v[4]:=v[3]/msr[RespVar];
    v[5]:=FInt(v[4],v[1],DFRes,True,Err);
(*
    If v[5]>=0.05
      Then
        v[6]:=0
      Else
        If v[5]>=0.01
          Then
            v[6]:=1
          Else
            v[6]:=2;
*)
    Table.MAdd(v);
    end;
  // opcoes

  Manager.Output.Add(C);
  Manager.Output.Add(Table);
  if Manager.AppDir <> '' then
     Manager.Output.WriteLink('Veja mais detalhes sobre este Hipóteses Lineares',
       Manager.AppDir + 'Ajuda\Modelos_Lineares\Hipoteses Lineares.htm');
{
  if Manager.Options[ord(cmSave)] Then <<<<<
     Manager.ObjectCreated(Table)
  else
     Table.Free;
}
  Table.Free; b.Free; vaux.Free; CX.Free
end; // LinearHypothesis

{ ====================== Classes para selecao de variaveis =========================== }

constructor TwsModelSelect.Create(Manager: TwsLMManager; SaveXY: Boolean); {08/09/98}
{ Objetivo
    Cria classe básica para todos os processos de seleção de modelos
  Parâmetros
    Manager : Gerenciador de modelos lineares
    SaveXY: Cópia da matriz e produtos cruzados. Os algoritmos implmenetados para os
            processos de seleção de variáveis retornam muitas vezes a matriz original.
            Com SaveXY = True a matriz é preservada e não existe a necessidade de nova
            construção dessa matriz, um dos ítens de maior custo computacional na análise
            de regressão
  Campos modificados
    FLastR2
  Métodos chamados
    Create herdado
}
begin
  inherited Create(Manager, SaveXY);
  FLastR2 := 0;
end;

destructor TwsModelSelect.Destroy;
{ Objetivo
    Libera memória ocupada
  Campos liberados
    FSummary
}
begin
  FSummary.Free;
  inherited Destroy
end;{Destroy}

procedure TwsModelSelect.UpdateSummary(k: Integer);   {08/09/98}
{ Objetivo
    Atualizar resumos informativos para cada passo dado no processo
  Parâmetros
    k: Índice da variável a atualizar
  Campos modificados
    FLastR2
    FSummary
  Métodos chamados
    MAdd
}
var
  v: TwsVec;
  p: Integer;
begin
  p := FXY.NRows-NResp+RespVar; // Indice da resposta
  v := TwsDFVec.Create(6);
  v.Name := FXY.ColName[k];
  if ColOp[k] = -1 then
    v[1] := 0
  else
    v[1] := 1;                                     // Situacao
  v[2] := 1 - (FXY[p,p]/SSq[NPred+RespVar]);       // R2
  v[3] := (v[2]-FLastR2)/v[2]*100;                 // Acresc percentual
  FLastR2 := v[2];                                 // Guarda R2 para proxima chamada
  v[4] := 1 - (NumObs-1)*(1-v[2]*v[2])/DFRes;      // R2 ajustado
  v[5] := (FXY[p,p]/MSRes[RespVar])-NumObs+2*NPar; // Cp de Mallows
  v[6] := v[5] - NPar;                             // Cp-p
  FSummary.MAdd(v);
end;{UpdateSummary}

procedure TwsModelSelect.Fit1(k: Integer);  {08/09/98, 23/09/98 }
{ Objetivo
    Ajusta o modelo para a variável especificada. Atualiza MSRes, DFRes e ColOp.
  Parâmetros
    k: Coluna pivô para operação com sweep
  Campos modificados
    FXY
    NPar
    DFRes
    msr
  Métodos chamados
    RevSweep
}
var
  p,OldCol: Integer;
begin
  p:=FXY.NRows-NResp+RespVar;         // Indice da variavel resposta
  OldCol := ColOp[k];
  FXY.RevSweep(k, Toler[k], ColOp);   // Inclui ou exclui do modelo a variavel k
  if ColOp[k] <> OldCol then
    begin                             // Se a coluna foi operada, o sinal será trocado
    NPar := NPar - ColOp[k];          // Acrescenta um parâmetro
    DFRes := DFRes + ColOp[k];        // e um grau de liberdade ao residuo
    end;
  // Obtem quadrado medio do residuo para a variavel dependente
  msr[RespVar] :=FXY[p,p]/DFRes;
end; { TwsModelSelect.Fit1 }

{ ==================== Todas as regressoes possiveis  ==================== }

constructor TwsAllReg.Create(Manager: TwsLMManager; Num,ColOrder: Integer);
{ Objetivo: Cria objeto para o processo de seleção todos as regressões possiveis
  Parâmetros
    Manager   : Objeto gerenciador de modelos lineares
    Num     : Número máximo de modelos que serão armazenados para posterior impressão.
    ColOrder: Os Num modelos que comporão a saída serão ordenados a partir da coluna
              ColOrder. Por default a ordenacao será feita pelo Cp de Mallows ajustado.
  Campos modificados
    FSummary
  Métodos chamados
    Create herdado
}
var
  Col: TwsDataSetCol;
  i: Integer;
  Ascd: Boolean;
  VOrder: String;
begin
  inherited Create(Manager, True);
  case ColOrder of
    1: begin
       Ascd := False;
       VOrder := 'Critério: Coeficiente de Determinação - Decrescente';
       end;

    2: begin
       Ascd := False;
       VOrder := 'Critério: Coeficiente de Determinação Ajustado - Decrescente';
       end;

    3: begin
       Ascd := False;
       VOrder := 'Critério: Estatística F - Decrescente';
       end;

    4: begin
       Ascd := True;
       VOrder := 'Critério: Valor p de F - Crescente';
       end;

    5: begin
       Ascd := True;
       VOrder := 'Critério: Coeficiente de Mallows - Crescente';
       end;

    6: begin
       Ascd := True;
       VOrder := 'Critério: Coeficiente de Mallows Ajustado - Crescente';
       end;
    end; { case }

  FSummary := TwsOrderedDataSet.Create('Resumo', Num, ColOrder, Ascd);

  Manager.Output.BeginText;
  Manager.Output.CenterTitle(3, 'Número Máximo de Modelos: ' + IntToStr(Num));
  Manager.Output.CenterTitle(3, VOrder);
  Manager.Output.EndText;

  { Criar colunas}
  with FSummary do
    begin
    MLab:='Resumo do processo Todas as Regressões Possíveis';
    Struct.AddColEx(TwsNumeric.Create('R2','Coeficiente de determinação',10,5));     //1
    Struct.AddColEx(TwsNumeric.Create('R2Ajust','Coeficiente de determinação ajustado',10,5));//2
    Struct.AddColEx(TwsNumeric.Create('F','Valor observado da estatística F',12,7)); //3
    Struct.AddColEx(TwsNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que F observado',10,5));  //4
    Struct.AddColEx(TwsNumeric.Create('Cp','Coeficiente de Mallows',11,6));           //5
    Struct.AddColEx(TwsNumeric.Create('CpAjust','Coeficiente de Mallows ajustado',11,6));  //6
    Struct.AddColEx(TwsNumeric.Create('NumPar','Número de parâmetros no modelo',11,6));  //6
    for i := 1 to NPred do
      begin
      VOrder := FXY.ColName[i+1];
      Col := TwsQualitative.Create(VOrder, 'Presença (X) ou ausência (.) no modelo de ' + VOrder, Length(VOrder)+1);
      With TwsQualitative(Col) Do
        Begin
        AddLevel('X');
        AddLevel('.');
        End; { With Col }
      Struct.AddColEx(Col);
    end; { for }
  end; { with }
end; { TwsAllreg.Create }

procedure TwsAllReg.UpdateSummary(k: Integer);
{ Objetivo: Atualiza tabela com resumo do processo de seleção todas as regressões possíveis
  Parâmetros
    k: Coluna da variável preditora para atualização
  Campos modificados
    FSummary
  Métodos chamados
    MAdd
}
var
  v: TwsVec;
  SSReg: double;
  j,p: Integer;
  Erro: Word;
begin
  p := FXY.NRows-NResp+RespVar;                    // Indice da resposta
  v := TwsDFVec.Create(NPred+7);
//  v.Name := FXY.ColName[k+1];
  SSReg := SSq[NPred+RespVar] - FXY[p,p];
  v[1] := SSReg/SSq[NPred+RespVar];                // R2
  v[2] := 1 - ((NumObs-1)/DFRes)*(1-v[1]);      // R2 ajustado
  v[3] := (SSReg/(NPar-1))/msr[RespVar];           // f
  v[4] := FInt(v[3],NPar,DFRes,True,Erro);         // Valor p
  v[5] := FXY[p,p]/MSRes[RespVar]-NumObs + 2*NPar; // Cp de Mallows
  v[6] := v[5] - NPar;                             // Cp de Mallows ajustado
  v[7] := NPar;
  for j := 1 to NPred do
    if ColOp[j+1] = -1 then
      v[7+j] := 0                                  // variavel presente
    else
      v[7+j] := 1;                                 // Variavel ausente
  FSummary.MAdd(v);
end; { TwsAllreg.UpdateSummary }

procedure TwsAllReg.AllRegressions;
{ Objetivo
    Executa processo de seleção Todas as regressões possíveis. O algoritmo utilizado está
    descrito em Kennedy & Gentle
  Campos modificados
    FXY
    TempXY
    NPar
    DFRes
    ColOp
  Métodos chamados
    Fit1
    UpdateSummary
    Add
  Saídas
    Conjunto de dados com um resumo dos resultados referentes aos "melhores" modelos,
    segundo o critério estabelecido
}
var
  a  : TwsVec;
  j,k: Integer;
  Err: Word;
begin
  FXY.Free;
  TempXY.Copy(mtSymmetric, TwsMatrix(FXY));
  NPar := 1;
  DFRes := NumObs - 1;
  for j := 2 to FXY.NRows-NResp do
    ColOp[j] := 1;          // Nenhuma variavel no modelo

  a := TwsDFVec.Create(NPred);
  a[1] := 1;
  for j := 0 to NPred-2 do
    a[j+2] := (2 shl j); {2*2^j}

  for j := 1 to (2 shl (NPred-1))-1 do // 2*2^(NPred-1)-1
    begin
    a.IndMinOrMax(True, k);
    Fit1(k+1);
    UpdateSummary(k);
    a[k] := a[k]+(2 shl (k-1))
    end; { For }
  a.Free;

  // Ver opcoes para impressão e salvamento
  if FSummary.NRows > 0 then
     Manager.Output.Add(FSummary);
end;


{ ================ Selecao passo a passo ================= }

Constructor TwsStepSelection.Create(Manager: TwsLMManager; SaveXY: Boolean);
{ Objetivo
    Criação de uma instância instância de uma classe para seleção de modelos de regressão
    passo a passo. Classe abstrata, uma vez que TwsStepSelection é uma classe básica da
    qual derivam as classes que implementam os processos passo a passo. Inicializa uma
    tabela que irá representar as variáveis que estão fora e uma para as variáveis que
    estão no modelo.
  Parâmetros
    Manager : Objeto gerenciador de modelos lineares
    SaveXY: True para armazenamento da matriz de SQ&P para uso posterior
  Campos modifiacdos
    Step
    FSummary
    FTableOut
    FTableIn
  Métodos chamados
    Create herdado
}
var
  Col: TwsDataSetCol;
begin
  inherited Create(Manager, SaveXY);
  Step:=0;

  FSummary := TwsDataSet.Create('Resumo');
  with FSummary do
    begin
    MLab:='Tabela Resumo do processo de Seleção de Variáveis';
    ColIdentName:='Variável';
    Col:=TwsQualitative.Create('Operacao','Operação realizada sobre a variável',10);
    With TwsQualitative(Col) Do
      Begin
      AddLevel('Inclusão');
      AddLevel('Exclusão');
      End; { With Col }
    Struct.AddColEx(Col);
    Struct.AddColEx(TwsNumeric.Create('R2','Coeficiente de determinação',10,5));
    Struct.AddColEx(TwsNumeric.Create('Acresc',
      'Acréscimo percentual no coef. de determinação',10,5));
    Struct.AddColEx(TwsNumeric.Create('R2Ajust',
      'Coeficiente de determinação ajustado',10,5));
    Struct.AddColEx(TwsNumeric.Create('Cp','Coeficiente de Mallows',12,7));
    Struct.AddColEx(TwsNumeric.Create('CpAjust',
      'Coef. Mallows ajustado para núm. de parâmetros',12,7));
    end;

  FTableOut := TwsGeneral.Create(0,3);
  with FTableOut do
    begin
    MLab := 'Variáveis fora do modelo';
    Name := 'Variaveis';
    ColName[1] := 'CorrPar';
    ColName[2] := 'F';
    ColName[3] := 'p';
    end;

  FTableIn := TwsGeneral.Create(0,4);
  with FTableIn do
    begin
    MLab := 'Variáveis incluídas no modelo';
    Name := 'Variaveis';
    ColName[1] := 'Coeficiente';
    ColName[2] := 'ErrPadr';
    ColName[3] := 'F';
    ColName[4] := 'p';
    end;

end; {TwsStepSelection.Create}

destructor TwsStepSelection.Destroy;
{ Objetivo
    Libera memória reservada para os campos do objeto
  Campos liberados
    FTableIn
    FTableOut
  Métodos chamados
    Destroy herdado
}
begin
  FTableIn.Free;
  FTableOut.Free;
  inherited Destroy
end;

function TwsStepSelection.VarIn(var k: Integer; const alfa: double): Boolean;
{ Objetivo
    Retorna True se uma variável irá entrar no modelo
  Parâmetros
    k   : Retorna índice da variável a ser incluída no modelo.
    alfa: Probabilidade considerada para entrada da variável no modelo. Este valor é
          comparado com o menor valor p existente na tabela das variáveis que estão fora
          do modelo (a tabela é ordenada em ordem crescente pela coluna dos valores p). O
          valor da menor probabilidade é comparado com alfa e a função retorna True se esse
          valor for menor que alfa, indicando que essa variável pode ser incluída no modelo,
}
var
  Lin: TwsVec;
begin
  if FTableOut.NRows > 0 then
    begin
    Lin := FTableOut.Row[1];
    Result := Lin[3] < alfa;
    if Result then
      k := FXY.CName.IndexOf(Lin.Name)+1
    end
  else
    begin
    Result:=False;
    k:=-1
    end
end;

function TwsStepSelection.VarOut(var k: Integer; const alfa: double): Boolean;
{ Objetivo
    Retorna True se uma variável será excluída do modelo
  Parâmetros
    k   : Retorna índice da variável a ser excluída do modelo.
    alfa: Probabilidade considerada para retirar a variável do modelo. Este valor é
          comparado com o maior valor p existente na tabela das variáveis que estão dentro
          do modelo (a tabela é ordenada em ordem crescente pela coluna dos valores p). O
          valor da maior probabilidade é comparado com alfa e a função retorna True se esse
          valor for maior que alfa, indicando que essa variável pode ser excluída do modelo,
}
var
  Lin: TwsVec;
begin
  Lin := FTableIn.Row[FTableIn.NRows];
  Result := Lin[4] > alfa;
  if Result then
    k := FXY.CName.IndexOf(Lin.Name)+1
end;

procedure TwsStepSelection.UpdateInOut; {08/09/98}
{ Objetivo
    Atualiza tabelas de variáveis dentro e fora do modelo
  Campos modificados
    FTableIn
    FTableOut
  Métodos chamados
    SortRows
    Add
  Saídas
    Tabelas de variáveis que estão no modelo ou estão fora do modelo
}
var
  Erro  : Word;
  i,p   : Integer;
  v     : TwsVec;
  C, Asc: TwsLIVec;
  df    : double;
begin
  // Limpa as linhas das tabelas
  while FTableIn.NRows > 0 do
    FTableIn.MDelete(1);
  while FTableOut.NRows > 0 do
    FTableOut.MDelete(1);
  p := FXY.NRows-NResp+RespVar;                   // Indice da resposta
  df:=DFRes-1;
  for i := 2 to ColOp.Len-NResp do
    if ColOp[i] = 1 then                          // Variavel fora do modelo
      begin
      v := TwsDFVec.Create(3);
      v.Name := FXY.ColName[i];
      v[1] := Sqr(FXY[p,i])/(FXY[i,i]*FXY[p,p]);  // Coef. r2
      v[2] := df*v[1]/(1-v[1]);                // Estat F
      v[3] := FInt(v[2],1,df,True,Erro);       // Valor p
      FTableOut.MAdd(v)
      end
    else
      begin                                        // Variavel no modelo
      v := TwsDFVec.Create(4);
      v.Name := FXY.ColName[i];
      v[1] := FXY[p,i];                            // Coef. regressao
      v[2] := Sqrt(FXY[i,i]*msr[RespVar]);         // Erro padrao
      v[3] := Sqr(v[1]/v[2]);                      // Estat F
      v[4] := FInt(v[3],1,DFRes,True,Erro);        // Valor p
      FTableIn.MAdd(v)
      end;
  C := TwsLIVec(VecConst(3,1,False));              // coluna 3, 1 coluna
  Asc := TwsLIVec(VecConst(0,1,False));            // Descendente, tamanho 1
  if FTableIn.NRows > 0 then
    begin
    FTableIn.SortRows(C,Asc);
    Manager.Output.Add(FTableIn);
    end;

  C[1] := 3;                                       // Coluna 3
  Asc[1] := 1;                                     // Ascendente
  if FTableOut.NRows > 0 then
    begin
    FTableOut.SortRows(C,Asc);
    Manager.Output.Add(FTableOut);
    end;
  C.Free;
  Asc.Free
end;{TwsStepSelection.UpdateInOut}

{================== Backward Elimination ==================}

constructor TwsBackwardElimination.Create(Manager: TwsLMManager; alfa: double);
{ Objetivo
    Cria objeto que implementa o método de seleção de variáveis backward elimination
  Parâmetros
    Manager: Objeto gerenciador de modelos lineares
    alfa : Nível de significância para eliminação de variáveis do modelo
  Campos modificados
    AlfaOut
  Métodos chamados
    Create herdado
}
begin
  inherited Create(Manager, False);
  Manager.Output.BeginText;
  Manager.Output.CenterTitle(3, 'Nível de significância para exclusão: '+ FloatToStrF(Alfa,ffGeneral,5,10));
  Manager.Output.EndText;
  AlfaOut := Alfa;
end;

procedure TwsBackwardElimination.Backward;{08/09/98}
{ Objetivo
    Executa o processo de seleção de variáveis pelo método backward elimination
  Campos modificados
    FLastR2

  Métodos chamdos
    AnovaTable
    UpdateInOut
    VarOut
    Fit1
    UpdateSummary
    Coefficients
    Add
  Saídas
    Conjunto de dados com o resumo do processo de eliminação backward elimination
}
var
  j,p: Integer;
  Back: Boolean;
begin
  p := FXY.NRows-NResp+RespVar;
  FLastR2 := 1 - (FXY[p,p]/SSq[NPred+RespVar]);
  Back := True;

  Manager.Output.BeginText;
  Manager.Output.CenterTitle(3, 'Passo 0: Todas as variáveis incluídas');
  Manager.Output.EndText;

  AnovaTable(True);
  while Back do
    begin
    UpdateInOut;
    Back := VarOut(j, AlfaOut);
    if Back then
      begin
      Inc(Step);

      Manager.Output.BeginText;
      Manager.Output.CenterTitle(3,'Passo '+IntToStr(Step)+
        ' - Variável excluída: '+FXY.ColName[j]);
      Manager.Output.EndText;

      Fit1(j);
      // Se nPar = 1 todas as variaveis foram eliminadas
      if nPar>1 then
        begin
        AnovaTable(False);
        UpdateSummary(j);
        end
      else
        begin
        UpdateInOut;
        Manager.Output.BeginText;
        Manager.Output.CenterTitle(3,'Todas as variáveis eliminadas');
        Manager.Output.EndText;
        Back:=False;
        end
      end; {if}
    end; {while}
  if nPar>1 then
    begin
    if FSummary.NRows >0 then
       Manager.Output.Add(FSummary);
    Coefficients
    end
end;

{ ======================= Forward Selection ===================== }

constructor TwsForwardSelection.Create(Manager: TwsLMManager; alfa: double);
{ Objetivo
    Cria objeto para implementação do método forward selection
  Parâmetros
    Manager: Objeto gerenciador de modelos lineares
    alfa : Nível mínimo de significância para inclusão no modelo
  Campos modificados
    AlfaIn
  Métodos chamados
    Create herdado
}
begin
  inherited Create(Manager, True);

  Manager.Output.BeginText;
  Manager.Output.CenterTitle(3, 'Probabilidade para Inclusão: ' +
    FloatToStrF(alfa,ffGeneral,9,5));
  Manager.Output.EndText;

  AlfaIn := Alfa;
end;

procedure TwsForwardSelection.ForwardSel;{08/09/98}
{ Objetivo
    Implementa processo de seleção de variáveis forward selection
  Campos modificados
    FXY
    TempXY
    NPar
    DFRes
    ColOp
    Step
  Métodos chamados
    UpdateInOut
    VarIn
    Fit1
    AnovaTable
    Coefficients
    Add
  Saídas
    Conjunto de dados com o resumo do processo de seleção forward selection
}
var
  j   : Integer;
  Forw: Boolean;
  Err : Word;
begin
  FXY.Free;
  // Recupera matriz original de SQ&P
  TempXY.Copy(mtSymmetric,TwsMatrix(FXY));
  NPar := 1;
  DFRes := NumObs - 1;
  // Muda o sinal da primeira coluna para para compensar o inicio como SQ&P ajustada
  for j := 2 to FXY.NRows-NResp do
    ColOp[j] := 1;        // Nenhuma variavel no modelo

  UpdateInOut;
  Forw := VarIn(j, AlfaIn);
  while Forw do
    begin
    Fit1(j);
    Inc(Step);

    Manager.Output.BeginText;
    Manager.Output.CenterTitle(3, 'Passo ' + IntToStr(Step) +
      ' - Variável incluída: ' + FXY.ColName[j]);
    Manager.Output.EndText;

    AnovaTable(False);
    UpdateInOut;
    UpdateSummary(j);
    Forw := VarIn(j,AlfaIn)
    end; {while}

  if FSummary.NRows > 0 then
     Manager.Output.Add(FSummary);

  Coefficients
end; {TwsForwardSelection.Forward}


{ ======================= Stepwise ===================== }

constructor TwsStepwiseSelection.Create(Manager: TwsLMManager; alfa1, alfa2: double);
{ Objeto
    Cria objeto que implmenta processo de seleção de variáveis stepwise selection
  Parâmetros
    Manager: Objeto gerenciador de modelos lineares
    alfa1,
    alfa2: níveis mínimos de significância para entrada e saída de variáveis do modelo,
           respectivamente.
  Campos modificados
    AlfaIn
    AlfaOut
  Métodos chamados
    Create herdado
}
begin
  inherited Create(Manager, True);

  Manager.Output.BeginText;
  Manager.Output.CenterTitle(3, 'Probabilidade para Entrada: ' +
    FloatToStrF(alfa1,ffGeneral,9,5));
  Manager.Output.CenterTitle(3, 'Probabilidade para Saída  : ' +
    FloatToStrF(alfa2,ffGeneral,9,5));
  Manager.Output.EndText;

  AlfaIn := Alfa1;
  AlfaOut := Alfa2;
end;

procedure TwsStepwiseSelection.Stepwise;{08/09/98}
{ Objetivo
    Implementa processo de seleção de variáveis stepwise selection
  Campos modificados
    FXY
    TempXY
    NPar
    DFRes
    ColOp
    Step
  Métodos chamados
    UpdateInOut
    VarIn
    VarOut
    Fit1
    AnovaTable
    Coefficients
    UpdateSummary
    Add
  Saídas
    Conjunto de dados com resumo do processo stepwise selection
}
var
  j,jb,jf         : Integer;
  Forw,Back,
  NextStep,NotSame: Boolean;
begin
  FXY.Free;
  TempXY.Copy(mtSymmetric,TwsMatrix(FXY));  // Recupera matriz original de SQ&P
  NPar := 1;
  DFRes := NumObs - 1;

  for j := 2 to FXY.NRows-NResp do       // Nenhuma variavel no modelo
    ColOp[j] := 1;

  UpdateInOut;                    // Atualiza tabelas de variaveis dentro e fora do modelo

  Forw := VarIn(jf,AlfaIn);
  if Forw then                    // Primeiro passo. Variavel jf pode ser inserida ?
    begin
    Fit1(jf);
    Inc(Step);

    Manager.Output.BeginText;
    Manager.Output.CenterTitle(3, 'Passo: ' + IntToStr(Step) +
      ' - Variável incluída: ' + FXY.ColName[jf]);
    Manager.Output.EndText;

    AnovaTable(False);
    UpdateInOut;
    UpdateSummary(jf)
    end
  else
    begin
    Manager.Output.BeginText;
    Manager.Output.WriteTitle(3, 'Nenhuma variável deverá ser incluída');
    Manager.Output.EndText;
    Exit
    end;

  Forw := VarIn(jf,AlfaIn); // Segundo passo. Tenta inserir nova variavel jf
  if Forw then
    begin
    Fit1(jf);
    Inc(Step);

    Manager.Output.BeginText;
    Manager.Output.CenterTitle(3, 'Passo ' + IntToStr(Step) +
      ' - Variável incluída: ' + FXY.ColName[jf]);
    Manager.Output.EndText;

    AnovaTable(False);
    UpdateInOut;
    UpdateSummary(jf)
    end;

  NextStep := Forw;
  while NextStep do
    begin
    Back := VarOut(jb, AlfaOut);  { Tenta retirar a variavel jb }
    if Back then
      begin
      Fit1(jb);
      Inc(Step);

      Manager.Output.BeginText;
      Manager.Output.CenterTitle(3, 'Passo ' + IntToStr(Step) +
        ' - Variável excluída: ' + FXY.ColName[jb]);
      Manager.Output.EndText;

      NotSame := jb <> jf;
      AnovaTable(False);
      UpdateInOut;
      UpdateSummary(jb)
      end
    else
      begin
      Forw := VarIn(jf, AlfaOut);
      NotSame := jb <> jf;       { Variavel jb que saiu eh a mesma jf que entrou? }
      if (Forw and NotSame) then
        begin
        Fit1(jf);
        Inc(Step);

        Manager.Output.BeginText;
        Manager.Output.CenterTitle(3, 'Passo ' + IntToStr(Step) +
          ' - Variável incluída: ' + FXY.ColName[jf]);
        Manager.Output.EndText;

        AnovaTable(False);
        UpdateInOut;
        UpdateSummary(jf)
        end
      end;  { if Back }
    {Para se: 1) A variavel que saiu e a mesma que entrou;
              2) Nao entrou nem saiu nenhuma variavel}
    NextStep := (Back or Forw) and NotSame;
    end; {while}

  if FSummary.NRows >0 then
     Manager.Output.Add(FSummary);

  Coefficients;
end;

{ ===================== Analise de regressao multivariada ==================}

Constructor TwsMReg.Create(Manager: TwsLMManager);
{ Objetivo
    Cria objeto para análise de regressão multivariada.
  Parâmetros
    Manager: Objeto gerenciador de modelos lineares
  Campos modificados
    DSToAnalysis
    NumObs
    wTot
    NPar
    NPred
    ColOp
    Toler
  Métodos chamados
    Create herdado
    GetIncMat
    GetXMat
    WCrossProduct
    CrossProduct
    Add
    Fit
}
var
  i,nc: Integer;
Begin
  Inherited Create(Manager);

  DSToAnalysis := Manager.FListFrame[Manager.DataIndex];
  if Manager.Options[ord(cmDadosModeloImp)] then         // Imprime
     Manager.Output.Add(DSToAnalysis);
  if Manager.Options[ord(cmDadosModeloSal)] then         // Salva
     Manager.ObjectCreated(DSToAnalysis);

  GetIncMat;

  if Manager.Options[ord(cmVar_PrintMatInc2)] Then
     Manager.Output.Add(IncMat);

  if WVar then
    dsWIndex := DSToAnalysis.Struct.IndexOf(Manager.WeightVar);

  GetXMat;                              // Obtem matriz X

  if Manager.Options[ord(cmVar_PrintMatMod2)] then
     Manager.Output.Add(XMat);

  if Manager.Options[ord(cmVar_SaveMatMod2)] then
     Manager.ObjectCreated(XMat);

  NumObs := 0;
  NPar:=1;                               // Somente o intercepto no modelo

  if WVar then
    begin
    nc := xMat.NCols-2;                  // menos intercepto e peso
    ColOp:=Index(1,xMat.NCols-1);
    WCrossProduct(ColOp);                // Obtem a matriz de produtos cruzados
    ColOp.Free
    end
  else
    begin
    nc := xMat.NCols-1;                 // menos intercepto
    CrossProduct;                       // Obtem a matriz de produtos cruzados
    wTot:=NumObs
    end;
  NPred := nc-NResp;                    // Numero de variaveis preditoras no modelo
  ColOp := TwsLIVec(VecConst(1,NumIdx.Len+1,False));  // Colunas operadas pelo sweep
  Toler := TwsDFVec.Create(NPred+1);    // Tolerancias para o sweep
  ColOp[1] := -1;                       // Coluna da média já operada em CrossProduct
  For i:=1 To NPred Do                  // Valores das tolerancias para o Sweep
    Toler[i+1] := FXY[i+1,i+1]*1.0e-8;
  Fit;                                  // Ajusta o modelo
End; { TwsMReg.Create }

destructor TwsMReg.Destroy;
{ Objetivo
    Libera memória ocupada por campos do objeto
  Campos liberados
    FXInv
    FH
    FRes
    FBeta
  Métodos chamados
    Destroy herdado
}
begin
  FXInv.Free;
  FH.Free;
  FRes.Free;
  FBeta.Free;
  inherited Destroy
end;

procedure TwsMReg.Fit;
{ Objetivo
    Ajusta o modelo proposto
  Campos modificados
    DFRes
    FXInv
    FBeta
    NPar
    FRes
    FH
  Métodos chamados
    RevSweep
    Add
  Saídas
    Matriz X'X inversa
    Matriz das estimativas dos parâmetros
    Matriz das somas de quadrados e produtos do resíduo
    Matriz das somas de quadrados e produtos da regressão
}
var
  Rank,i,j: Integer;
  Err     : Word;
  Mat     : TwsSymmetric;
begin
  // Matriz das somas de quadrados e produtos total
  Mat := TwsSymmetric(FXY.SubSymMat(NPred+2,FXY.NCols));
  Mat.MLab := 'Somas de quadrados e produtos total';
  for i := 1 to Mat.NCols do
    begin
    Mat.ColName[i] := DSToAnalysis.ColName[RIdx[i]];
    Mat.RowName[i] := Mat.ColName[i]
    end;

  DFRes := NumObs-1; // Graus de liberdade do residuo comeca como total
  for i := 2 to NPred+1 do                      // Opera colunas das preditoras pelo sweep
    begin
    FXY.RevSweep(i,Toler[i],ColOp);
    if ColOp[i] = -1 then              // Se operou ajusta num. parametros e GLRes
      begin
      Inc(NPar);
      DFRes := DFRes - 1
      end
    end;
  FXInv := TwsSymmetric(FXY.SubSymMat(1,NPred+1));   // Submatriz inversa de X´X
  FXInv.Name:='XXInv';
  FXInv.MLab := 'Inversa de X''X';
  FXInv.ColName[1]:='Itcp';
  FXInv.RowName[1]:='Itcp';
  for i := 2 to FXInv.NCols do
    begin
    FXInv.ColName[i]:=DSToAnalysis.ColName[CovIdx[i-1]];
    FXInv.RowName[i]:=FXInv.ColName[i]
    end;

  if Manager.Options[ord(cmInversaXX)] then
     Manager.Output.Add(FXInv);

  // Matriz das estimativas dos parametros
  FBeta:=TwsGeneral.Create(NPar,NResp);
  for i:=1 to NResp do
    for j:=1 to NPar do
      FBeta[j,i]:=FXY[NPar+i,j];
  FBeta.Name:='EstParam';
  FBeta.MLab:= 'Estimativas dos parâmetros';
  FBeta.RowName[1] := 'Itcp';
  for i := 2 to NPar do
    FBeta.RowName[i] := DSToAnalysis.ColName[CovIdx[i-1]];
  for i := 1 to NResp do
    FBeta.ColName[i] := DSToAnalysis.ColName[RIdx[i]];

  if Manager.Options[ord(cmEstParametros)] then
     Manager.Output.Add(FBeta);

  // Matriz das somas de quadrados e produtos do residuo
  FRes := TwsSymmetric(FXY.SubSymMat(NPred+2,FXY.NCols));
  FRes.Name:='SQPRes';
  FRes.MLab := 'Somas de Quadrados e Produtos do Residuo';
  for i:=1 to FRes.NCols do
    begin
    FRes.ColName[i]:=DSToAnalysis.ColName[RIdx[i]];
    FRes.RowName[i]:=FRes.ColName[i]
    end;

  if Manager.Options[ord(cmSQP)] then
     Manager.Output.Add(FRes);

  // matriz das somas de quadrados e produtos do residuo
  FH := TwsSymmetric(Mat.ByElement(FRes,opSub,True,Err));
  FH.Name:='SQPReg';
  FH.MLab := 'Somas de Quadrados e Produtos da Regressão';
  for i:=1 to FH.NCols do
    begin
    FH.ColName[i]:=DSToAnalysis.ColName[RIdx[i]];
    FH.RowName[i]:=FH.ColName[i]
    end;

  if Manager.Options[ord(cmSQP)] then
     Manager.Output.Add(FH);

  Mat.Free;
end;

Procedure TwsMReg.HTest(C, W: TwsGeneral; var ErrCode: Word);
{ Objetivo
    Realiza o teste da hipótese C'BW=0
  Parâmetros
    C      : Matriz que estabelece as comparações entre os parâmetros
    W      : Matriz que estabelece as combinações das variáveis respostas
    ErrCode: Código de erro. Se (C.NRows<>FBeta.NRows) ou (FBeta.NCols<>W.NRows) retorna
             o código de erro NImprDim (dimensões impróprias para a operação)
  Campos modificados
    Nenhum
  Métodos chamados
    Add
  Saídas
    Inv(C'*Inv(X'X)*C)                                          (I)
    SQ&P da hipótese (combinada)                                (I)
    SQ&P do re´siduo (combinada)                                (I)
    Autovalores e autovetores                                   (I)
    Conjunto de dados com resultados dos testes multivariados   (I/S)
}
var
  A,EVec   : TwsGeneral;
  G,H,R1,H1: TwsSymmetric;
  EVal     : TwsDiagonal;
  Stat     : TwsDataSet;
  i,Rank   : Integer;
  cnil,wnil: boolean;
begin
  // tratar a situacao onde C e/ou W sao identidades
  // Se C e/ou W forem nil, construir como identidade
  if C=nil then
    begin
    cnil:=True;
    C:=Identity(FBeta.NRows)
    end
  else
    cnil:=False;
  if W=nil then
    begin
    wnil:=True;
    W:=Identity(FBeta.NCols)
    end
  else
    wnil:=False;

  if (C.NRows=FBeta.NRows) and (FBeta.NCols=W.NRows) then
    begin
    A:=TwsGeneral(C.TranspMul2(FBeta,ErrCode));        // C'*Beta
    G:=TwsSymmetric(FXInv.TranspMul5(C,ErrCode));      // C'*Inv(X'X)*C
    G.Inv(ErrCode);
    G.Name:='G';                                 // Inv(C'*Inv(X'X)*C)
    G.MLab:= 'Inv(C''*Inv(X''X)*C)';
    if Manager.Options[ord(cmInversaXX)] then
       Manager.Output.Add(G);

    // Soma de quadrados da hipotese
    H:=TwsSymmetric(G.TranspMul5(A,ErrCode));          // A'*G*A
    for i:=1 to A.NCols do
      begin
      H.RowName[i]:=A.ColName[i];
      H.ColName[i]:=A.ColName[i];
      end;
    A.Free; G.Free;
    H.Name:='H'; H.MLab:='SQP da Hipótese';
    if Manager.Options[ord(cmSQP)] then
       Manager.Output.Add(H);

    // Combinando as variáveis para as somas de quadrados e produtos da hipótese
    H1:=TwsSymmetric(H.TranspMul5(W,ErrCode));         // W'*H*W
    for i:=1 to W.NCols do
      begin
      H1.RowName[i]:=W.ColName[i];
      H1.ColName[i]:=W.ColName[i];
      end;
    H1.Name:='WHW'; H1.MLab:='SQP Regressão W''RW';
    if Manager.Options[ord(cmSQP)] then
       Manager.Output.Add(H1);

    // Somas de quadrados e produtos do resíduo
    R1:=TwsSymmetric(FRes.TranspMul5(W,ErrCode));         // W'*R*W
    for i:=1 to W.NCols do
      begin
      R1.RowName[i]:=W.ColName[i];
      R1.ColName[i]:=W.ColName[i];
      end;
    R1.Name:='WRW'; R1.MLab:='SQP Resíduo';

    if Manager.Options[ord(cmSQP)] then
       Manager.Output.Add(R1);

    // Autovalores e autovetores de Inv(R1)*H1
    EigenProd(H1,R1,EVec,EVal,Rank);
    EVec.Name:='Autovet';
    EVec.MLab:='Autovetores de Inv(R)*H';

    A:=EigenProp(EVal,Rank);

    H1.Free; R1.Free;

    if Manager.Options[ord(cmAutoValAutoVet)] then
       Begin
       Manager.Output.Add(A);
       Manager.Output.Add(EVec);
       End;

    H.Free;

    // Analise canonica ?

    // Obtem as estatisticas multivariadas para a hipótese C'BW=0
    Stat := MultivarStat(EVal.Row[1],NPred,NResp,Rank,C.NCols,W.NCols,DFRes);

    if Manager.Options[ord(cmHipGeralCBWImp)] then
       Manager.Output.Add(Stat); // ver opcoes de impressao e salvamento

    if Manager.Options[ord(cmHipGeralCBWSal)] then
       Manager.ObjectCreated(Stat)
    else
      Stat.Free;

    A.Free; EVal.Free; EVec.Free;
    end
  else
    ErrCode := NImprDim;
  if cnil then C.Free;
  if wnil then W.Free;
end;

procedure TwsMReg.SConfInterv(C, W: TwsGeneral; alpha: double; var ErrCode: Word);
{ Objetivo
    Constrói os intervalos de confiança simultâneos para as comparações c'(i)Bw(j),
    i=1,...,C.NCols, j=1,...,W.NCols, onde B é a matriz de parâmetros, através das
    metodologias F, Bonferroni, Schèffé e Hotelling. Retorna um conjunto de dados com os
    resultados
  Parâmetros:
    C      : Matriz com as comparações entre parâmetros. Cada coluna define uma comparação
    W      : Matriz com as combinações entre variáveis respostas. Cada coluna define uma
             combinação
    ErrCode: Código de erro. Retorna NImprDim se o produto C'BW nao puder ser realizado,
             ou seja, se (C.NRows <> B.NRows) ou (B.NCols <> W.NRows)
  Campos modificados
    Nenhum
  Métodos chamados
    Add
    AddDataSet
    SendMessageToObjects
  Saídas
    Matrizes para combinação de parâmetros e variáveis respostas     (I)
    Conjunto de dados com os resultados dos intervalos de confiança  (I/S)
}
var
  sqc, sqw,
  vaux      : TwsVec;
  i, k      : Integer;
  aux       : double;
  Est       : TwsGeneral;
  Res       : TwsDataSet;
  Col       : TwsDataSetCol;
  wnil,cnil : boolean;
begin
  if C=nil then
    begin
    cnil:=True;
    C:=Identity(FBeta.NRows)
    end;
  if W=nil then
    begin
    wnil:=True;
    W:=Identity(FBeta.NCols)
    end;

  Est := TwsGeneral(FBeta.TranspMul9(C, W, ErrCode)); // Obtem as estimativas
  if ErrCode = 0 then // Se o produto eh valido
    begin
    Res := TwsDataSet.Create('ICSimult');
    with Res do
      begin
      MLab:='Intervalos de confiança - nível ' + FloatToStrF(alpha,ffGeneral,7,4);

      Col:=TwsQualitative.Create('CompPar','Comparação entre os parâmetros',10);      //1
      With TwsQualitative(Col) Do
        for i:=1 to C.NCols do
          if C.ColName[i] <> '' then
	          AddLevel(System.Copy(C.ColName[i],1,8))
	        else
	          AddLevel('Comp'+IntToStr(i));
      Struct.AddColEx(Col);

      Col:=TwsQualitative.Create('CombRes','Combinação das variáveis respostas',10);  //2
      With TwsQualitative(Col) Do
        for i:=1 to W.NCols do
          if W.ColName[i] <> '' then
	          AddLevel(System.Copy(W.ColName[i],1,8))
	        else
	          AddLevel('Comb'+IntToStr(i));
      Struct.AddColEx(Col);

      Col:=TwsQualitative.Create('Metodo','Método de construção do intervalo',9);      //3
      With TwsQualitative(Col) Do
        Begin
        AddLevel('Student');
        AddLevel('Bonferroni');
        AddLevel('Scheffe');
        AddLevel('Hotelling')
        End; { With Col }
      Struct.AddColEx(Col);

      Struct.AddColEx(TwsNumeric.Create('Valor','Valor observado da estatística',12,7));          //4

      Struct.AddColEx(TwsNumeric.Create('Amplitude','Amplitude do intervalo de confiança',12,7));//5

      Struct.AddColEx(TwsNumeric.Create('LimInf','Limite inferior do intervalo',12,7));//6

      Struct.AddColEx(TwsNumeric.Create('LimSup','Limite superior do intervalo',12,7))//7
      end; // with Result

    sqc := TwsDFVec.Create(C.NCols); // dimensao num. de parametros
    for i := 1 to C.NCols do         // para cada comparacao de parametros
      sqc[i] := FXInv.TranspMul11(C,i,ErrCode); // Produto c'Inv(X'X)c

    sqw := TwsDFVec.Create(W.NCols);
    for i := 1 to W.NCols do                  // para cada comparacao de parametros
      sqw[i] := FRes.TranspMul11(W,i,ErrCode);// Produto w'Rw

    for i:=1 to C.NCols do                    // para cada comparacao de parametros
      for k:=1 to W.NCols do                  // para cada combinacao de variaveis
        begin
        // Criterio de Student
       	vaux := TwsDFVec.Create(7);
        vaux[1]:=i-1; // Indice para o contraste i
        vaux[2]:=k-1; // Indice da combinacao k
        vaux[3]:=0; // F
        vaux[4]:=Est[i,k]; // Valor da estatistica
        aux:=FInv(alpha,1,DFRes,1.0e-10,True,ErrCode);
        aux := Sqrt((aux/DFRes)*sqc[i]*sqw[k]);
        vaux[5]:=aux;         // Erro padrao
        vaux[6]:=vaux[4]-aux; // Limite inferior
        vaux[7]:=vaux[4]+aux; // Limite superior
        Res.MAdd(vaux);       // Insere no conjunto de dados

        // Criterio Bonferroni
       	vaux := TwsDFVec.Create(7);
        vaux[1]:=i-1; // Indice para o contraste i
        vaux[2]:=k-1;
        vaux[3]:=1; // Bonferroni
        vaux[4]:=Est[i,k];
        aux:=FInv(alpha/C.NCols,1,DFRes,1.0e-10,True,ErrCode);
        aux := Sqrt((aux/DFRes)*sqc[i]*sqw[k]);
        vaux[5]:=aux;         // Erro padrao
        vaux[6]:=vaux[4]-aux; // Limite inferior
        vaux[7]:=vaux[4]+aux; // Limite superior
        Res.MAdd(vaux);

        // Criterio Scheffe
       	vaux := TwsDFVec.Create(7);
        vaux[1]:=i-1;         // Indice para o contraste i
        vaux[2]:=k-1;
        vaux[3]:=2;           // Scheffe
        vaux[4]:=Est[i,k];
        aux:=FInv(alpha,C.NCols,DFRes,1.0e-10,True,ErrCode);
        aux := Sqrt((C.NCols*aux/DFRes)*sqc[i]*sqw[k]);
        vaux[5]:=aux;         // Erro padrao
        vaux[6]:=vaux[4]-aux; // Limite inferior
        vaux[7]:=vaux[4]+aux; // Limite superior
        Res.MAdd(vaux);

        // Criterio Hotelling
       	vaux := TwsDFVec.Create(7);
        vaux[1]:=i-1;         // Indice para o contraste i
        vaux[2]:=k-1;
        vaux[3]:=3;           // Hotelling
        vaux[4]:=Est[i,k];
        aux:=FInv(alpha,NResp,DFRes-NResp+1,1.0e-10,True,ErrCode);
        aux := Sqrt((NResp*aux/(DFRes-NResp+1))*sqc[i]*sqw[k]);  // <===== verificar
        vaux[5]:=aux;         // Erro padrao
        vaux[6]:=vaux[4]-aux; // Limite inferior
        vaux[7]:=vaux[4]+aux; // Limite superior
        Res.MAdd(vaux);
    	  end // for k
    end; // if ErrCode

  if Manager.Options[ord(cmMatHip)] then
     Begin
     Manager.Output.Add(C);
     Manager.Output.Add(W);
     End;

  if Manager.Options[ord(cmInterConfiancaImp)] then
     Manager.Output.Add(Res);

  if Manager.Options[ord(cmInterConfiancaSal)] then
     Manager.ObjectCreated(Res)
  else
     Res.Free;

  sqc.Free; sqw.Free; Est.Free;
  if cnil then C.Free;
  if wnil then W.Free;
end; // SConfInterv

end.
