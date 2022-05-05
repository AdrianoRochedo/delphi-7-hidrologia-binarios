unit wsTTest;

interface
uses Classes,
     wsConstTypes,
     wsBXML_Output,
     wsMatrix,
     wsVec,
     wsGLib;

type
  TwsOneSample = class
  private
    // armazenam apenas as referencias
    Options: TBits;
    LData: TwsDataSets;
    RVar: TwsLIVec;
    WInd: Integer;
    FOutput: TwsBXML_Output;
    FOnCreatedObject: TwsCreatedObject_Event;

    Procedure SetSaveTest(SaveTest: Boolean);
    Procedure SetListTest(ListTest: Boolean);
    Procedure SetSaveData(SaveData: Boolean);
    Procedure SetListData(ListData: Boolean);
    function GetSaveTest: Boolean;
    function GetListTest: Boolean;
    function GetSaveData: Boolean;
    function GetListData: Boolean;
  public
    constructor Create(DataList: TwsDataSets;
                       RespVar: TwsLIVec;
                       WIndex: Integer;
                       Output: TwsBXML_Output;
                       CreatedObjectEvent: TwsCreatedObject_Event);

    procedure TTest(var hValue: TwsVec; const Alpha: Double; Tail: Integer); virtual;

    property SaveTest : Boolean Read GetSaveTest Write SetSaveTest;
    property ListTest : Boolean Read GetListTest Write SetListTest;
    property SaveData : Boolean Read GetSaveData Write SetSaveData;
    property ListData : Boolean Read GetListData Write SetListData;
  end;

  TwsTwoSample = class(TwsOneSample)
  private
    // armazena apenas referencia
    FVar: TwsLIVec;
  public
    constructor Create(DataList: TwsDataSets;
                       Fac: TwsLIVec;
                       RespVar: TwsLIVec;
                       WIndex: Integer;
                       Output: TwsBXML_Output;
                       CreatedObjectEvent: TwsCreatedObject_Event);
    // teste t
    procedure TTest(var HValue: TwsVec; const Alpha: Double; Tail: Integer); override;
    // teste de casualizacao
    procedure RandTest(var HValue: TwsVec; const Alpha: Double; Tail: Integer;
      nrep: Integer; RTest: Boolean);

  end;

implementation
uses
  wsFuncoesDeProbabilidade,
  wsDistribuicoesRandomicas,
  wsFrequencias,
  wsGraficos,
  Form_Chart;

constructor TwsOneSample.Create(DataList: TwsDataSets;
                                RespVar: TwsLIVec;
                                WIndex: Integer;
                                Output: TwsBXML_Output;
                                CreatedObjectEvent: TwsCreatedObject_Event);
{  Objetivo
     Cria objeto para tratamento de métodos estatísticos aplicáveis a uma amostra
   Parâmetros
     DataList: Lista com os conjuntos de dados. Usualmente será a lista resultante da função
               ModelFrame
     RespVar : Índices das variáveis respostas que serão tratadas
     WIndex  : Índice da variável peso. Um valor nulo ou negativo (default) indica a todos
               os métodos que não existe definição de variável peso.
}
begin
  inherited Create;
  LData := DataList;
  RVar := RespVar;
  WInd := WIndex;
  Options := TBits.Create;
  FOutput := Output;
  FOnCreatedObject := CreatedObjectEvent;
end;
{====================  Métodos privados =================}

Procedure TwsOneSample.SetSaveTest(SaveTest: Boolean);
{Atribui True ou False para a posição 0 de TBits, que controla o armazenamento
do resultado do teste}
Begin
  Options.Bits[0]:= SaveTest;
End;

function TwsOneSample.GetSaveTest: Boolean;
Begin
  Result:=Options.Bits[0];
End;

Procedure TwsOneSample.SetListTest(ListTest: Boolean);
{Atribui True ou False para a posição 1 de TBits, que controla a impressão da
matriz de estatísticas e do resultado do teste}
Begin
  Options.Bits[1]:= ListTest;
End;

function TwsOneSample.GetListTest: Boolean;
Begin
  Result:=Options.Bits[1];
End;

Procedure TwsOneSample.SetSaveData(SaveData: Boolean);
{Atribui True ou False para a posição 2 de TBits, que controla o armazenamento
do conjunto de dados que contém os dados para a análise (retorno de modelframe)}
Begin
  Options.Bits[2]:= SaveData;
End;

function TwsOneSample.GetSaveData: Boolean;
Begin
  Result:=Options.Bits[2];
End;

Procedure TwsOneSample.SetListData(ListData: Boolean);
{Atribui True ou False para a posição 3 de TBits, que controla a impressão
do conjunto de dados que contém os dados para a análise (retorno de modelframe)}
Begin
  Options.Bits[3]:= ListData;
End;

function TwsOneSample.GetListData: Boolean;
Begin
  Result:=Options.Bits[3];
End;

{============================================================}

procedure TwsOneSample.TTest(var HValue: TwsVec; const Alpha: double; Tail: Integer);
{  Objetivo
     Executa as comparações entre a média e um valor padrão através do teste t
   Parâmetros
     HValue: Vetor com os valores da hipótese
     Alpha: Valor entre 0 e 1 que indica o nivel de significância desejado para o teste
     OneSidedTail: 0 teste unilateral à esquerda
                   1 teste unilateral à direita
                   2 teste bilateral
}
var
  i,j,k: Integer;
  Stat : TwsGeneral;    // matriz com as estatisticas
  IStat: TwsLIVec;     // indices para as estatísticas
  TData: TwsDataSet;   // conjunto de dados para saída dos resultados
  v    : TwsVec;
  Col  : TwsDataSetCol;
  aux  : double;
  Erro : Word;
begin
  // 0-Media 2-Desvio padrao 4-Minimo 5-Maximo 7-Valores validos 8-Erro padrao da media
  FOutput.BeginText;
    Foutput.CenterTitle(3,'Teste t: Comparação entre uma média e um padrão');
  FOutput.EndText;

  if WInd<=0 then
    IStat:=TwsLIVec.Create([0,2,4,5,7,8])
  else
    IStat:=TwsLIVec.Create([0,2,4,5,7,8,15]);   // 15-soma dos pesos

  if HValue = nil then
    HValue:=VecConst(0,RVar.Len);

  TData:=TwsDataSet.Create('Teste_t');  // Conjunto de dados para saida dos resultados
  with TData do
    begin
    MLab:='Teste t para média de uma amostra';
    case Tail of
      0: MLab:=MLab+' - Unilateral à esquerda';
      1: MLab:=MLab+' - Unilateral à direita';
      2: MLab:=MLab+' - Bilateral';
      end; // case

    ColIdentName:='Variavel';
    Col:=TwsNumeric.Create('Media','Média amostral');                   { 1 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Val_Hip','Valor da hipótese');              { 2 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Diferenca','Diferença Media-Val_Hip');      { 3 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('T','Valor da estatística T para hipótese H0'); { 4 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que T observado');        { 5 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Alfa','Taxa de erro Tipo I');               { 6 }
    TwsNumeric(Col).Size:=8; TwsNumeric(Col).Precision:=4;
    Struct.AddColEx(Col);
    Col:=TWSQualitative.Create('Decisao','Decisão ao nível alfa');        { 7 }
    With TWSQualitative(Col) Do
      Begin
      Size := 6;
      AddLevel('NaoRejeita');
      AddLevel('Rejeita');
      End; { With Col }
    Struct.AddColEx(Col);
                                                                        { 8 }
    Col:=TWSNumeric.Create('Extr_Inf','EI do IC para média ao nível 100(1-alfa)%');
    Struct.AddColEx(Col);
                                                                        { 9 }
    Col:=TWSNumeric.Create('Extr_Sup','ES do IC para média ao nível 100(1-alfa)%');
    Struct.AddColEx(Col);
    end;

  k:=0;
  for i := 0 to LData.Count-1 do     // para cada conjunto de dados da lista
    begin
    if WInd<=0  then               // Obtem matriz das estatisticas.
      Stat := TwsDataSet(LData[i]).DescStat(RVar,IStat)
    else
      Stat := TwsDataSet(LData[i]).wDescStat(RVar,IStat,WInd);

    Stat.MLab := 'Estatísticas Descritivas';

    if ListData then
       FOutPut.Add(LData[i]);

    if ListTest then
       FOutPut.Add(Stat);

    for j:=1 to RVar.Len do  // para cada variavel resposta
      begin
      Inc(k);
      v:=TwsDFVec.Create(9);
      v.Name:=Stat.RowName[k];
      v[1]:=Stat[k,1];                                  // Media
      v[2]:=hValue[j];                                  // Valor da hipótese
      v[3]:=v[1]-hValue[j];                             // Diferenca
      v[4]:=v[3]/Stat[k,6];                             // Estatistica t
      v[5]:=TInt(Abs(v[4]),Stat[k,5]-1,True,True,Erro); // Probabilidade
      case Tail of
        0: if v[3]>0 then v[5]:=1-v[5];
        1: if v[3]<0 then v[5]:=1-v[5];
        2: v[5]:=2*v[5]
        end; // case
      v[6]:=Alpha;
      if v[5]<alpha then
        v[7]:=1                                         // significativo
      else
        v[7]:=0;                                        // nao significativo
      if WInd<=0 then                                   // n-1
        aux:=TInv(Alpha,Stat[k,5]-1,1.0e-9,True,False,Erro)
      else                                              // soma dos pesos - 1
        aux:=TInv(Alpha,Stat[k,7]-1,1.0e-9,True,False,Erro);
      aux:=aux*Stat[k,6];
      v[8]:=v[1]-aux;                                   // Extremo inferior
      v[9]:=v[1]+aux;                                   // Extremo superior
      TData.MAdd(v)
      end;
    k:=0;
    Stat.Free;

    if SaveData and Assigned(FOnCreatedObject) then
       begin
       FOnCreatedObject(self, LData[i]);
       LData[i] := nil; // Isto evitará a futura destruição
       end;
    end; // for i

  if ListTest then
     FOutPut.Add(TData);

  if SaveTest and Assigned(FOnCreatedObject) then
     FOnCreatedObject(self, TData)
  else
     TData.Free;

  IStat.Free
end; // TTest

constructor TwsTwoSample.Create(DataList: TwsDataSets;
                                Fac, RespVar: TwsLIVec;
                                WIndex: Integer;
                                Output: TwsBXML_Output;
                                CreatedObjectEvent: TwsCreatedObject_Event);
{ Objetivo
    Cria objeto para comparação entre duas médias
  Parâmetros
    DataList: Lista de conjuntos de dados
}
begin
  inherited Create(DataList, RespVar, WIndex, Output, CreatedObjectEvent);
  FVar := Fac
end;

procedure TwsTwoSample.TTest(var HValue: TwsVec; const Alpha: double; Tail: Integer);
{  Objetivo
     Executa as comparações entre duas médias de amostras independentes através do teste t.
     As amostras correspondem aos dois (e somente dois) níveis do fator de índice FVar.
     Através de um teste F bilateral (5%) verifica se as variâncias são heterogêneas. Se
     forem, inclui a aproximação para variâncias heterogêneas, utilizando a aproximação de
     Satterthwaite para os graus de liberdade.
   Parâmetros
     HValue  : Vetor com os valores da hipótese. Se a entrada for nil o vetor é contruído
               e preenchido com zeros em todas as posições.
     Alpha   : Valor entre 0 e 1 que indica o nivel de significância desejado para o teste
     Tail    : 0: hipótese unilateral à esquerda
               1: hipótese unilateral à direita
               2: hipótese bilateral
}
var
  i,j,k,kf,
  kv,kp1,kv1,kv2: Integer;
  FMat,Stat     : TwsGeneral;    // matriz com as estatisticas
  TData         : TwsDataSet;    // conjunto de dados para saída dos resultados
  v             : TwsVec;
  Col           : TwsDataSetCol;
  aux,ndf,ddf,
  ndf1,ndf2,pf  : double;
  Erro          : Word;
begin
  FOutput.BeginText;
    Foutput.CenterTitle(3,'Teste t: Comparação entre duas médias');
  FOutput.EndText;
  TData:=TwsDataSet.Create('Teste_t');
  with TData do
    begin
    MLab:='Teste t para comparação entre médias de duas amostras independentes';
    case Tail of
      0: MLab:=MLab+' - Unilateral à esquerda';
      1: MLab:=MLab+' - Unilateral à direita';
      2: MLab:=MLab+' - Bilateral';
      end; // case

    ColIdentName:='Variável';

    Col:=TwsNumeric.Create('Diferenca','Diferença entre as médias');           { 1 }
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Val_Hip','Valor da hipótese');                     { 2 }
    TwsNumeric(Col).Size:=9; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);
                                                                               { 3 }
    Col:=TWSNumeric.Create('Dif_Hip','Diferença entre as médias e o valor da hipótese');
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Variancia','Variância da comparação');                 { 4 }
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('G_Lib','Graus de liberdade da variância combinada');{ 5 }
    TwsNumeric(Col).Size:=9; TwsNumeric(Col).Precision:=6;
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('T','Valor da estatística T para hipótese');         { 6 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que T observado');       { 7 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=4;
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Alfa','Taxa de erro Tipo I');                       { 8 }
    TwsNumeric(Col).Size:=8; TwsNumeric(Col).Precision:=4;
    Struct.AddColEx(Col);

    Col:=TWSQualitative.Create('Decisao','Decisão ao nível Alfa');           { 9 }
    With TWSQualitative(Col) Do
      Begin
      Size := 7;
      AddLevel('NaoRejeita');
      AddLevel('Rejeita');
      End; { With Col }
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Extr_Inf','EI do IC para diferença ao nível 100(1-alfa)%');{ 10 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Extr_Sup','ES do IC para diferença ao nível 100(1-alfa)%');{ 11 }
    Struct.AddColEx(Col);
    end;

  // Se nenhum valor foi atribuido para hipotese, atribui zero
  if HValue = nil then
     HValue := VecConst(0, RVar.Len);

  // Executa o teste: para cada conjunto, cada fator e cada variável resposta
  for i:=0 to LData.Count-1 do     // para cada conjunto de dados da lista
    begin
    if (WInd < 0) then
       Stat := LData[i].FacVarMean(FVar,1,LData[i].NRows,RVar)
    else
       Stat := LData[i].wFacVarMean(FVar,1,LData[i].NRows,WInd,RVar);

    Stat.MLab := 'Estatísticas Descritivas';

    if ListData then
       FOutPut.Add(LData[i]);

    if ListTest then
       FOutPut.Add(Stat);

    k:=-1;
    for kf := 1 to FVar.Len do       // Para cada fator
      begin
      Col := TwsDataSet(LData[i]).Struct.Col[FVar[kf]];
      if TwsFactor(Col).Levels = 2 then  // Se fator nao tiver 2 niveis, nao faz
        begin
        Inc(k,2);          // k eh a linha da estatistica
        kp1:=k+1;
        for j:=1 to RVar.Len do                     // para cada variavel resposta
          begin
          if WInd<0 then
            kv:=3*j-2 else kv:=4*j-3;  // Se existe peso
          kv1:=kv+1;
          if WInd<0 then
            kv2:=kv+2 else kv2:=kv+3;  // Se existe peso
          v:=TwsDFVec.Create(11);
          v.Name:=LData[i].Struct.Col[RVar[j]].Name;
          v[1]:=Stat[k,kv]-Stat[kp1,kv];            // Diferenca entre as medias
          v[2]:=HValue[j];                          // Valor da hipótese
          v[3]:=v[1]-HValue[j];                     // Diferenca
          ndf:=Stat[k,kv2]-1; ddf:=Stat[kp1,kv2]-1; // gl numerador e denominador
          aux:=(ndf*Stat[k,kv1]+ddf*Stat[kp1,kv1]);
          v[5]:=ndf+ddf;                            // Graus de liberdade combinad
          v[4]:=aux/v[5];                           // Variancia combinada
                                                    // Estatistica T
          v[6]:=v[3]/Sqrt(v[4]*(1/Stat[k,kv2]+1/Stat[kp1,kv2]));
          v[7]:=TInt(Abs(v[6]),v[5],True,True,Erro); // Probabilidade p
          case Tail of
            0: if v[6]>0 then v[7]:=1-v[7];
            1: if v[6]<0 then v[7]:=1-v[7];
            2: v[7]:=2*v[7]
            end; // case

          v[8]:=Alpha;
          if v[7]>=alpha then
            v[9]:=0                                 // nao signifiactivo
          else
            v[9]:=1;                                // significativo
          aux:=TInv(Alpha,v[5],1.0e-9,True,False,Erro);
          aux:=aux*Sqrt(v[4]*(1/Stat[k,kv2]+1/Stat[kp1,kv2]));
          v[10]:=v[1]-aux;                          // Extremo inferior
          v[11]:=v[1]+aux;                          // Extremo superior
          TData.MAdd(v);

          // executa o teste F. F eh a razao entre a maior e a menor variância
          aux:=wsGLib.MaxF(Stat[k,kv1],Stat[kp1,kv1])/wsGLib.MinF(Stat[k,kv1],Stat[kp1,kv1]);
          if Stat[k,kv1]>Stat[kp1,kv1] then // troca os graus de liberdade, se necessario
            begin
            ndf1:=ndf;
            ndf2:=ddf
            end
          else
            begin
            ndf1:=ddf;
            ndf2:=ndf
            end;
          pf:=2*FInt(aux,ndf1,ndf2,True,Erro);      // Duplica para teste bilateral
          if pf<0.05 then                           // Teste F bilateral a 5%
            begin
            FMat:= TwsGeneral.Create(1,6);
            FMat.Name:='Teste_F';
            FMat.PrintOptions.ColPrecision := 8;
            FMat.PrintOptions.ColWidth := 14;
            FMat.MLab :='Teste F bilateral (5%) para as variâncias';
            FMat.RowName[1]:='Valores';
            FMat.ColName[1]:='Var_1';FMat.ColName[2]:='Var_2';
            FMat.ColName[3]:='GL_1';FMat.ColName[4]:='GL_2';
            FMat.ColName[5]:='F';FMat.ColName[6]:='p';
            FMat[1,1]:=Stat[k,kv1];FMat[1,2]:=Stat[kp1,kv1];
            FMat[1,3]:=ndf;FMat[1,4]:=ddf;FMat[1,5]:=aux;FMat[1,6]:=pf;

            FOutPut.Add(FMat);
            FMat.Free;

            // Cria uma nova linha com teste para variâncias heterogêneas
            v:=TwsDFVec.Create(11);
            v.Name:=Col.Name+'_Het';
            v[1]:=Stat[k,kv]-Stat[kp1,kv];          // Diferenca entre as medias
            v[2]:=HValue[j];                        // Valor da hipótese
            v[3]:=v[1]-HValue[j];                   // Diferenca
            // ndf (ddf) agora e a variancia 1 (2) dividida pelas respectivas repeticoes
            ndf:=Stat[k,kv1]/Stat[k,kv2]; ddf:=Stat[kp1,kv1]/Stat[kp1,kv2];
                                                    // Graus de liberdade Satterthwaithe
            v[5]:=Sqr(ndf+ddf)/(Sqr(ndf)/(Stat[k,kv2]-1)+Sqr(ddf)/(Stat[kp1,kv2]-1));
            v[4]:=ndf+ddf;                          // Variancia conjunta
            v[6]:=v[3]/Sqrt(v[4]);                  // Estatistica T
            v[7]:=TInt(Abs(v[6]),v[5],True,True,Erro); // Probabilidade p
            case Tail of
              0: if v[6]>0 then v[7]:=1-v[7];
              1: if v[6]<0 then v[7]:=1-v[7];
              2: v[7]:=2*v[7]
              end; // case
            v[8]:=Alpha;
            if v[7]>=alpha then
              v[9]:=0                                // nao signifiactivo
            else
              v[9]:=1;                               // significativo
            aux:=TInv(Alpha,v[5],1.0e-9,True,False,Erro);
            aux:=aux*Sqrt(v[4]);
            v[10]:=v[1]-aux;                         // Extremo inferior
            v[11]:=v[1]+aux;                         // Extremo superior
            TData.MAdd(v)
            end
          end; // for j
        end //if TwsFactor(Col).Levels = 2
      end;   // for kf
    Stat.Free;

    if SaveData and Assigned(FOnCreatedObject) then
       begin
       FOnCreatedObject(self, LData[i]);
       LData[i] := nil; // Isto evitará a futura destruição
       end;
    end; // for i

  if ListTest then
     FOutPut.Add(TData);

  if SaveTest and Assigned(FOnCreatedObject) then
     FOnCreatedObject(self, TData)
  else
     TData.Free;
end; // TTest

procedure TwsTwoSample.RandTest(var hValue: TwsVec; const Alpha: double;
  Tail: Integer; nrep: Integer; RTest: Boolean);
{  Objetivo
     Executa o teste da casualização
   Parâmetros
     hValue  : Vetor com os valores da hipótese. Se a entrada for nil o vetor é construído
               e preenchido com zeros em todas as posições.
     Alpha   : Valor entre 0 e 1 que indica o nivel de significância desejado para o teste
     Tail    : 0: hipótese unilateral à esquerda
               1: hipótese unilateral à direita;
               2: hipótese bilateral;
     nrep    : número de repetições para as permutações
}

var
  i,k,kf,j,j1,
  n,ha,r,kk    : Integer;
  Col,Colk     : TwsDataSetCol;
  TData,Freq   : TwsDataSet;
  Perc         : TwsGeneral;
  vStat,mx,my,
  vx,vy        : double;
  vobs,v,d,
  x1,x2,xp     : TwsVec;
  U            : TwsRandom;
  Hist         : TfoChart;
  RNames       : TStringList;

  function GetStat(var m1,m2: double; Stat: Integer): double;
  var
    v1,v2: double;
    i1,i2: Integer;
  begin
    case Stat of
      0: Result:=x1.Mean(i1)-x2.Mean(i1);     // diferenca
      1: begin                                // estatistica t (var comb)
         x1.VarMean(m1,v1,i1);
         x2.VarMean(m2,v2,i2);
         v2:=((i1-1)*v1 + (i2-1)*v2)/(i1+i2-2);  // variancia combinada
         Result:=(m1-m2)/sqrt((1/i1+1/i2)*v2)     // estatistica t
         end;
      2: begin                                // estatistica t (behrens-fisher)
         x1.VarMean(m1,v1,i1);
         x2.VarMean(m2,v2,i2);
         Result:=(m1-m2)/sqrt(v1/i1+v2/i2)     // estatistica t
         end;
    end; // case
  end;

  procedure GenRandTest;
  var
    ord   : TwsLIVec;
    gk,k1,
    ii,jj : Integer;
  begin
    // para cada repeticao
    for gk:=2 to nrep do
      begin
      ord:=Index(1,n);
      // preenche valores do primeiro grupo
      for k1:=1 to r do
        begin
        jj:=n-k1+1;
        ii:= U.Range(1,jj);
//        ii:= Trunc(U.Generate*jj)+1; // preenche posicao aleatoriamente
        x1[k1]:=vobs[ord[ii]];
        ord[ii]:=ord[jj];
        end;
      // e o restante
      for k1:=1 to (jj-1) do
        x2[k1]:=vobs[ord[k1]];

      // a estatistica pode variar
      d[gk]:=GetStat(mx,my,0);
      end; // for gk
    ord.Free
  end;

  procedure GenBootTest;
  var
    gk,k1,jj: Integer;
  begin
    // para cada repeticao
    for gk:=2 to nrep do
      begin
      // preenche valores do primeiro grupo com reposicao
      for k1:=1 to r do
        x1[k1]:=vobs[U.Range(1,n)];
      // e do segundo
      for k1:=1 to (n-r) do
        x2[k1]:=vobs[U.Range(1,n)];

      // a estatistica pode variar
      d[gk]:=GetStat(mx,my,0);
      end; // for gk
  end;

begin
  FOutput.BeginText;
    if RTest then
      Foutput.CenterTitle(3,'Teste de Permutação Para Diferença de Médias')
    else
      Foutput.CenterTitle(3,'Teste Bootstrap Para Diferença de Médias');
  FOutput.EndText;

  // Se nenhum valor foi atribuido para hipotese, atribui zero
  if hValue = nil then
     hValue := VecConst(0, RVar.Len);

  // r eh o numero de repeticoes do 1o nivel
  n:=LData[0].nRows;
  r:=0;
  k:=1;
  while LData[0][k,FVar[1]]=0 do
    begin
    Inc(r);
    inc(k)
    end;
  x1:=TwsDFVec.Create(r);
  x2:=TwsDFVec.Create(n-r);

  // d armazenara os desvios
  d:=TwsDFVec.Create(nrep);

  // Cria o gerador de uniformes
  U:=TwsLinear.Create;

  // Executa o teste: para cada conjunto, cada fator e cada variável resposta

  for i:=0 to LData.Count-1 do     // para cada conjunto de dados da lista
    begin
    if ListData then
       FOutPut.Add(LData[i]);
    for kf := 1 to FVar.Len do       // Para cada fator
      begin
      Colk := TwsDataSet(LData[i]).Struct.Col[FVar[kf]];
      // fator deve ter somente dois niveis
      if TwsFactor(Colk).Levels<>2 then
        begin
        hValue.Free;
        U.Free;
        Exit
        end;

      TData:=TwsDataSet.Create('TesteCas');
      with TData do
        begin
        MLab:='Teste de casualização para comparação entre médias de dos conjuntos de valores';
        case Tail of
          0: MLab:=MLab+' - Unilateral à esquerda';
          1: MLab:=MLab+' - Unilateral à direita';
          2: MLab:=MLab+' - Bilateral';
          end; // case
        ColIdentName:='Variável';

        Col:=TwsNumeric.Create(TwsFactor(Colk).LevelNames[0],'Nome do grupo 1');   { 1 }
        Struct.AddColEx(Col);

        Col:=TwsNumeric.Create(TwsFactor(Colk).LevelNames[1],'Nome do grupo 2');   { 2 }
        Struct.AddColEx(Col);

        Col:=TwsNumeric.Create('Dif_Obs','Diferença entre as médias observadas');  { 3 }
        Struct.AddColEx(Col);

        Col:=TWSNumeric.Create('Val_Hip','Valor da hipótese');                     { 4 }
        TwsNumeric(Col).Size:=9; TwsNumeric(Col).Precision:=5;
        Struct.AddColEx(Col);
                                                                                   { 5 }
        Col:=TWSNumeric.Create('Repet','Número de permutações realizadas');
        Struct.AddColEx(Col);

        Col:=TWSNumeric.Create('Nulidade','Número de ocorrências da nulidade');    { 6 }
        Struct.AddColEx(Col);

        Col:=TWSNumeric.Create('Prop','Proporção de ocorrências da nulidade');     { 7  }
        TwsNumeric(Col).Size:=9; TwsNumeric(Col).Precision:=6;
        Struct.AddColEx(Col);

        Col:=TWSNumeric.Create('Alfa','Nível descritivo para decisão');            { 8 }
        TwsNumeric(Col).Size:=8; TwsNumeric(Col).Precision:=4;
        Struct.AddColEx(Col);

        Col:=TWSQualitative.Create('Decisao','Decisão ao nível Alfa');             { 9 }
        With TWSQualitative(Col) Do
          Begin
          Size := 7;
          AddLevel('NaoRejeita');
          AddLevel('Rejeita');
          End; { With Col }
        Struct.AddColEx(Col);
        end;

      for j:=1 to RVar.Len do                     // para cada variavel resposta
        begin
        v:=TwsDFvec.Create(9);
        // copia todos os valores num vetor
        vobs:=LData[i].CopyCol(RVar[j],True);
        // Calcula as médias observadas e a diferenca
        for k:=1 to r do
          x1[k]:=vobs[k];
        kk:=0;
        for k:=r+1 to n do
          begin
          Inc(kk);
          x2[kk]:=vobs[k];
          end;
        if hValue[j]<>0 then
          for k:=1 to x2.Len do
            x2[k]:=x2[k]+hValue[j];

        // a estatistica pode variar
        v[3]:=GetStat(mx,my,0);
        v[1]:=mx;
        v[2]:=my;

        v.Name:=LData[i].Struct.Col[RVar[j]].Name;
        v[4]:=hValue[j];
        v[5]:=nrep;
        d.Name:=v.Name;
        // diferenca entre as observadas faz parte dos valores
        d[1]:=v[3];
        // gera as amostra para teste de casualizacao
        if RTest then
          GenRandTest // Randomization Test
        else
          GenBootTest; // Bootstrap
        ha:=0;
        case Tail of
          0: begin
             for kk:=1 to nrep do     // unilateral esquerda
               if d[kk]<=v[3] then
                 Inc(ha);
             v[6]:=ha;
             v[7]:=ha/nrep;
             end;
          1: begin
             for kk:=1 to nrep do     // unilateral direita
               if d[kk]>=v[3] then
                 Inc(ha);
             v[6]:=ha;
             v[7]:=ha/nrep;
             end;
          2: begin
             for kk:=1 to nrep do     // bilateral
               if d[kk]>=v[3] then
                 Inc(ha);
             if ha<nrep/2 then
               ha:=2*ha
             else
               ha:=2*(nrep-ha);
             v[6]:=ha;
             v[7]:=ha/nrep;
             end;
          end;
        v[8]:=alpha;
        if v[7]>=alpha then
          v[9]:=0
        else
          v[9]:=1;
        TData.MAdd(v);
        vobs.Free;

        d.QuickSort;
        xp:=TwsDFVec.Create([0,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99,1]);
        xp.Name:='Quantis';
        d.Percentis(xp);

        Perc:=TwsGeneral.Create(0,12);
        Perc.Name:='D_Perc';
        Perc.MLab:='Percentis dos desvios';
        with Perc do
          begin
          ColName[1]:='Minimo';
          ColName[2]:='Perc025';
          ColName[3]:='Perc05';
          ColName[4]:='Perc10';
          ColName[5]:='Perc25';
          ColName[6]:='Perc50';
          ColName[7]:='Perc75';
          ColName[8]:='Perc90';
          ColName[9]:='Perc95';
          ColName[10]:='Perc975';
          ColName[11]:='Perc99';
          ColName[12]:='Maximo';
          end;

        Perc.MAdd(xp);
        Freq:=VecToFreq(d);
        RNames:=TStringList.Create;
        RNames.Add(Freq.Struct.Col[4].Name);
        Hist:=ws_HistogramPlot(Freq,Freq.ColName[4],RNames);
        if ListTest then
          begin
          FOutPut.Add(Freq);
          FOutPut.Add(Hist);
          FOnCreatedObject(self, Hist);
          FOutPut.Add(Perc)
          end;
        Freq.Free;
        Perc.Free;
        RNames.Free;
        end // for j
      end;   // for kf

  x1.Free;
  x2.Free;

  d.Free;

  if SaveData and Assigned(FOnCreatedObject) then
     begin
     FOnCreatedObject(self, LData[i]);
     LData[i] := nil // Isto evitará a futura destruição
     end;
  end; // for i

  if ListTest then
     FOutPut.Add(TData);

  if SaveTest and Assigned(FOnCreatedObject) then
     FOnCreatedObject(self, TData)
  else
     TData.Free;
end; // RandTest

end.
