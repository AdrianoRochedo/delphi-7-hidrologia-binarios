unit wsListaDeTermos;

interface
uses Classes,
     wsConstTypes,
     wsGLib,
     wsMatrix,
     wsVec,
     wsTabelaDeSimbolos,
     wsParser,
     wsBXML_Output;
Type
  { Heranca
      TwsTerm --> TObject
    Objetivo
      Abrigar um termo do modelo e todas as quantidades relacionadas. As quantidades
      relacionadas incluem as somas de quadrados (ou SQ&P), os graus de liberdade valor
      esperado, informacoes sobre a estatistica F, componente de variancia e medias de
      mininmos quadrados.
  }
  TwsTerm = Class
  private
    FSSQ: TObject;     { Objeto que guarda as SQ correspondentes as respostas. No caso
                         univariado e um vetor com as somas de quadrados. No caso
                         multivariado e uma matriz simetrica com as SQ&P}
    FRandTerm: boolean;
    function GetAsSSq(i: Integer): double;   // uma soma de quadrados var i
    function GetAsVec: TwsVec;               // vetor de sq
    function GetAsSSP: TwsSymmetric;         // matriz SQ&P
  Public
    Lab,                // Rotulo para o termo
    Alias,              // Utilizado para obtencao de expressoes envolvendo os termos
    NumExp,             // Expressão para o cálculo do numerador de F
    DenExp,             // Expressão para o cálculo do denominador de F
    ExpectMS : String;  // Valor esperado do quadado médio
    DF       : double;   // Graus de liberdade
    FDen,               // Valor do denominador para o teste F uni/multivariado
    FNum,               // Valor do numerador para o teste F uni/multivariado
    VarComp  : TObject; // Valores dos componentes de variância uni/multivariado

    // somente univariado
    MSq,                // Quadrado médio
    FProb,              // Probabilidade para a estatística F
    DenDF,              // Graus de liberdade do denominador calculado
    NumDF,              // Graus de liberdade do numerador calculado
    FValue   : TwsVec;  // Valor da estatística F
    ICol,               // Linha Inicial na matriz U corresopndente ao termo
    LCol     : Word;    // Linha Final na matriz U
    FTest    : Boolean; // Realiza teste F para o termo?
    Effect,             // Indica quais variaveis pertencem ao termo
    Fact,               // Das variaveis quais sao os fatores
    Ctrl     : TBits;   // Array auxiliar para determinacao de valores esperados
    Coef     : double;   // Coeficiente do termo no valor esperado
    LSMeans  : TwsDataSet; // Conjunto de dados com as medias ajustadas

    Destructor Destroy; Override;
    // como soma de quadrados
    procedure SetSSQ(const Value: TObject);
    procedure SetTermType(F: TBits);
    property AsSSq[i: Integer]: double read GetAsSSq;
    // como vetor de SQ
    property AsVec: TwsVec read GetAsVec;
    // como matriz de SQ&P
    property AsSSP: TwsSymmetric read GetAsSSP;
    // le/atribui tipo do termo
    property RandTerm: boolean read FRandTerm;
  End;

  { >>>>>>>>>>>>> Incluir metodo AnovaTable nesta classe
    Este metodo ira retornar um conjunto de dados com tantas linhas quantos sao os termos
    do modelo (incluindo total e residuo)
    Coluna identificadora: Nomes dos termos (FONTES)
    Coluna 1: Graus de liberdade (GL)
    Coluna 2: Somas de quadrados (S.QUADRADOS)
    Coluna 3: Quadrados medios (somente para as linhas onde sera feito o teste F. O teste
              F sera feito se FTest = True ). Se o quadrado medio nao for calculado, colocar
              wscMissValue no seu lugar.(Q.MEDIOS)
    Coluna 4: Teste F. Razao entre quadrados medios. No numerador ira o QM do termo respectivo
              e no denominador ira uma funcao de quadrados medios expressa no campo FDen.
              Por default essa funcao sera RESIDUO. Se o teste nao for realizado atribuir
              wscMissValue. (TESTE F)
              Exemplo:
              Termos: A, B, A:B, C, A:C, B:C, RESIDUO, TOTAL
              Testes F: A - F = QM(A)/QM(A:B)     -->  A / A:B
                        C - F = QM(C)/QM(RESIDUO) -->  C / RESIDUO
    Coluna 5: Probabilidade associada ao valor de F.

    Como outras funcoes de quadrados medios serao calculadas para outros objetivos, seria
    interessante que essa classe tivesse um metodo que retornasse o valor de uma funcao qualquer
    de quadrados medios
  }

  {  Heranca
       TwsTermList --> TStringList
     Objetivo
       Implementa uma lista de termos para o modelo. Entre outros, calcula expressoes de
       quadrados medios, estima graus de liberdade por Satterthwaite, obtem a estatistica
       F de modo automatico ou como especificado pelo analista.
  }
  TwsTermList = Class(TStringList)
  Private
    // matriz com os coeficientes dos componentes de variancia
    FT       : TwsTriangular;
    Parser   : TExpressionParser;

    Function  GetAlias(i: Integer): String;
    // Recupera um termo
    Function  GetTerm(i: Integer): TwsTerm;

    //Ajusta a fração para o formato interno
    Function  AjustFRatio(Const Expr: String; Var Vars: TStrings): String;
    // Estima os graus de liberadade pelo processo de Satertwaithe
    Function  SatDF(k: Integer; Vars: TStrings): double;
    function GetName(i: Integer): String;
    procedure SetName(i: Integer; Const aName: String);
    // Obtem a ordem do termo, ou o numero de variaveis que o compoe.
    Function  GetOrder(Index: Integer): Byte;
    function GetCoef: TwsTriangular;
  Public
    Destructor Destroy; Override;

    // Limpa a lista
    Procedure  Clear; Override;
    // Adiciona um termo
    Procedure  AddTerm(Const Name: String; X: TwsTerm);
    // Insere um termo na posicao especificada
    Procedure  InsertTerm(tp: Integer; Const Name: String; X: TwsTerm);
    // Mostra o conteudo
    Procedure  DisplayTerm(Index,NResp: Integer);
    Procedure  DefineAlias;
    // Executa teste F pelas especificacoes do analista
    Procedure  UserFTest;
    // Executa teste F por procedimento automatico
    procedure  AutoFTest(nr: Integer; IResp:TwsLIVec);
    // Teste multivariado - ainda nao implementado
    procedure  MultAutoFTest(nr: Integer; IResp:TwsLIVec);
    // Obtem valor da variancia a partir de uma expressao de termos
    Procedure  GetVariances(Expr: String; Var Value, DF: TwsVec);
    // Obtem variancia do contraste para estruturas balanceadas
    procedure BContrVar(const FacName: string; FCol,Resp: TwsLIVec; Means: TwsGeneral; DS: TwsDataSet;
      out DF, V: TwsVec);

    // Obtem quadro da analise da variacao
    procedure  AnovaTable(RNames: TStrings; nx: Integer;
                          GMeans: TwsVec; Opt: TBits;
                          Output: TwsBXML_Output;
                          CreatedObject: TwsCreatedObject_Event);
    // Retorna um termo pelo nome
    Function   TermByName(Const Name: String): TwsTerm;
    Function   AliasByTerm(Const Name: String): String;
    // Retorna a lista de variaveis presentes no termo
    function Variables(k: Integer): TStringList;
    // Retorna o termo especificado
    Property   Term[i: Integer]: TwsTerm Read GetTerm {Write SetTerm};
    // Retorna o nome do termo especificado
    Property   Name[Index: Integer]: String Read GetName Write SetName;
    Property   Alias[i: Integer]: String  Read GetAlias;
    // Retorna a ordem do termo
    Property Order[Index: Integer]: Byte Read GetOrder;
    Property CoefComp: TwsTriangular Read GetCoef;
  End;

implementation
uses Math,
     Sysutils,
     SysUtilsEx,
     Forms,
     wsAvaliadorDeExpressoes,
     wsFuncoesDeEscalares,
     wsFuncoesDeProbabilidade,
     Form_Chart,
     wsGraficos,
     Graphics;

{ =========================== TwsTerm ============================}

Destructor TwsTerm.Destroy;
{ Objetivo
    Liberar conteudo ocupado por objeto da classe TwsTerm
  Campos eliminados
    FSSq
    MSq
    Effect
    Fact
    CTRL
    FDen
    FNum
    FProb
    VarComp
    DenDF
    NumDF
    FValue
    LSMeans
}
Begin
  FSSq.Free;
  MSq.Free;
  Effect.Free;
  Fact.Free;
  CTRL.Free;
  FDen.Free;
  FNum.Free;
  FProb.Free;
  VarComp.Free;
  DenDF.Free;
  NumDF.Free;
  FValue.Free;
  LSMeans.Free;
  Inherited Destroy;
End;

function TwsTerm.GetAsSSq(i: Integer): double; // uma soma de quadrados var i
{ Objetivo
    Retorna uma soma de quadrados
  Parâmetros
    i: Variável resposta
}
begin
  if FSSq is TwsVec then
    Result := TwsVec(FSSq)[i]
  else
    Result := TwsSymmetric(FSSq)[i,i];
end;

function TwsTerm.GetAsVec: TwsVec; // vetor de sq para conj. i
{ Objetivo
    Retorna uma soma de quadrados
  Parâmetros
    i: Conjunto de dados de interesse. Informado com base 1
}
begin
  Result := TwsVec(FSSq);
end;

procedure TwsTerm.SetSSq(const Value: TObject);
{ Objetivo
    Seta vetor de SQ ou matriz de SQ&P
  Parâmetros
    Value: Vetor de SQ ou matriz de SQ&P
}
begin
  FSSq.Free;
  FSSq:=Value
end;

function TwsTerm.GetAsSSP: TwsSymmetric;
{ Objetivo
    Retorna matriz de soma de quadrados e produtos
}
begin
  Result := TwsSymmetric(FSSq);
end;

procedure TwsTerm.SetTermType(F: TBits);
{ Objetivo
    Seta o tipo de termo (tfFixed ou tfRandom) dependendo da composição do termo e de
    quais fatores são aleatórios
  Parãmetros
    F: vetor de bits que indica se a variável (fator) na posição relativa é de efeito
       aleatório (True) ou fixo (False);
}
var
  i: integer;
begin
  FRandTerm:=False;   // o termo eh fixo
  i:=0;
  // Termo sera aleatorio se pelo menos 1 fator for aleatorio
  while (not FRandTerm) and (i<F.Size) do
    begin
    FRandTerm:= (F[i] and Fact[i]);
    Inc(i)
    end;
end;

{ =========================== TwsTermList ============================}

  Destructor TwsTermList.Destroy;
  {  Objetivo
       Libera todo conteúdo ocupado pelo objeto
     Métodos chamados
       Clear
       Destroy herdado
  }
  Begin
    Clear;
    Inherited Destroy;
  End;

  Procedure TwsTermList.Clear;
  {  Objetivo
       Libera conteúdo ocupado pelo objeto
     Campos eliminados
       Parser
       Term
     Métodos chamados
       Clear herdado
  }
  Var i : Integer;
  Begin
    FT.Free;
    If Assigned(Parser) Then Parser.Free;
    For i := 0 to Count - 1 do Term[i].Free;
    Inherited Clear;
  end;

  {--------------------------- Private ---------------------------}

  Function TwsTermList.GetTerm(i: Integer): TwsTerm;
  {  Objetivo
       Obter o endereco do termo especifiacdo
     Parâmetros
       i: Índice do termo desejado. Baseado em zero.
  }
  Begin
    Result := TwsTerm(Objects[i]);
  End;

  function TwsTermList.GetName(i: Integer): String;
  { Objetivos
      Retorna o nome do termo
    Parâmetros
      i: Índice do termo desejado. Baseado em 1.
  }
  begin
    Result := Strings[i-1]
  end;

  procedure TwsTermList.SetName(i: Integer; Const aName: String);
  {  Objetivo
       Atrinbuir ao termo o nome especificado
     Parâmetros
       i: Índice do termo, baseado em 1
       aName: Nome a atribuir
  }
  begin
    Strings[i-1]:=aName
  end;

  (*
  Procedure TwsTermList.SetTerm(i: Integer; X: TwsTerm);
  var pX: PwsTerm;
  Begin
    If (i < 0) or (i >= Count) Then
       Raise Exception.Create('Classe: TwsTerm' + #13#10 +
                              'Propriedade: Term[' + IntToStr(i) + ']' + #13#10 +
                              'Erro: Índice inválido')
    Else
       Begin
       pX  := PwsTerm(Objects[i]);
       pX^ := X;
       pX^.Alias := Format('_Term%d_', [i]);
       pX^.FRatio := Strings[i] + ' / RESIDUO';
       End;
  End;
  *)

  Function  TwsTermList.GetAlias(i: Integer): String;
  Begin
    Result := TwsTerm(Objects[i]).Alias;
  End;

  Function TwsTermList.GetOrder(Index: Integer): Byte;
  {  Objetivo
       Retorna a ordem ou o número de variáveis presentes no termo
     Parâmetros
       Index: Índice do termo. Baseado em zero.
  }
  Var i: Integer;
      s: String;
  Begin
    Result := 1;
    s := Strings[Index];
    For i := 1 to Length(s) do If s[i] = '.' Then Inc(Result);
  End;

  function TwsTermList.GetCoef: TwsTriangular;
  { retorna a matriz triangular dos coeficientes dos componentes de variancia}
  begin
  Result:=FT
  end;

 {---------------------------- Public ---------------------------}

  Procedure TwsTermList.DefineAlias;
  var i: Integer;
  Begin
    For i := 0 to count -1 do
      TwsTerm(Objects[i]).Alias := Format('_Term%d_', [i]);
  End;

  Function  TwsTermList.TermByName(Const Name: String): TwsTerm;
  Begin
    Result := TwsTerm(Objects[IndexOf(Name)]);
  End;

  Function  TwsTermList.AliasByTerm(Const Name: String): String;
  Begin
    Result := TwsTerm(Objects[IndexOf(Name)]).Alias;
  End;

  function TwsTermList.Variables(k: Integer): Classes.TStringList;
  { Objetivo
      Retorna uma lista com as variáveis que compoem o termo
    Parâmetros
      k: Índice do termo desejado. Baseado em zero
  }
  var
    st,st1: string;
    i,nk: Integer;
  begin
    st:=Strings[k];
    Result:=TStringList.Create;
    nk:=1;
    repeat
      st1:=StrUntilChar(st,['.'],nk);
      if st1<>'' then Result.Add(st1)
    until st1='';
  end;

  Procedure TwsTermList.AddTerm(Const Name: String; X: TwsTerm);
  {  Objetivo
       Adiciona um termo ao final da lista
     Parâmetros
       Name: nome do termo a adicionar
       X: termo a adicionar
  }
  Begin
    AddObject(UpperCase(Name), TObject(X));
    X.FTest := True;
    X.Alias := Format('_Term%d_', [Count-1]);
  End;

  Procedure  TwsTermList.InsertTerm(tp: Integer; Const Name: String; X: TwsTerm);
  {  Objetivo
       Insere um termo numa posição especificada da lista
     Parâmetros
       tp: posição que o termo irá ocupar na lista
       Name: nome do termo a inserir
       X: termo a inserir
  }
  begin
    Dec(tp);
    InsertObject(tp,Name,X);
    X.FTest:=True;
    X.Alias := Format('_Term%d_', [tp]);
  end;


  Procedure TwsTermList.DisplayTerm(Index,NResp: Integer);
(*
    Function BitsToStr(Bits: TBits): String;
    var i: Byte;
    Begin
      {$IFDEF WIN32}
      SetLength(Result, Bits.Size);
      {$ELSE}
      Result[0] := Chr(Bits.Size);
      {$ENDIF}

      For i := 0 to Bits.Size - 1 do
        If Bits[i] Then
           Result[i+1] := '1'
        Else
           Result[i+1] := '0';
    End;
*)
  Var X: TwsTerm;
  Begin
    X := TwsTerm(Objects[Index]);
    With X do
      Begin
      Clear;
      Add('Nome             : ' + Self[Index]);
      Add('Coeficiente      : ' + FloatToStr(Coef));

      Add('Soma de Quad.    : ' + FloatToStr(AsSSq[NResp]));
      Add('Graus Lib.       : ' + FloatToStr(DF));
      Add('Quad. Médio      : ' + FloatToStr(MSq[NResp]));

      Add('Teste F          : ' + BooleanToStr(FTest));
      Add('Valor F          : ' + FloatToStr(FValue[NResp]));
      Add('Probabilidade    : ' + FloatToStr(FProb[NResp]));

      Add('Expressão Den. F : ' + NumExp);
      Add('Expressão Num. F : ' + DenExp);
      Add('EMS              : ' + ExpectMS);
      End;
  End;

  Function TwsTermList.AjustFRatio(Const Expr: String; Var Vars: TStrings): String;
  { Objetivo
      Define operadores e avalia expressão que envolve termos do modelo
    Parâmetros
      Expr: expressão envolvendo termos do modelo
      Vars: variáveis presentes na expressão
  }
  Var j: Integer;
  Begin
    If Not Assigned(Parser) Then
       begin
       Parser := TExpressionParser.Create();
       { Definindo operadores para expressoes matemáticas simples }
       Parser.AritOpers.Add('+');
       Parser.AritOpers.Add('-');
       Parser.AritOpers.Add('*');
       Parser.AritOpers.Add('/');
       end;

    Parser.Text.Clear;
    Parser.Text.Add(Expr);

    Parser.Scan;
    Vars := Parser.Variables;
    For j := 0 to Parser.TokensCount - 1 do
      If Parser.Token[j].TokenType = ttVariable Then
         Parser.ReplaceToken(j, AliasByTerm(Parser.Token[j].AsString), ttVariable);

    Result := Parser.MakeString;
  End;

// Inclui esta rotina em 24/11/99 - Amauri Modificada pelo Rochedo em 23/11/99

  Procedure TwsTermList.GetVariances(Expr: String; Var Value,DF: TwsVec);
  { Objetivo
      Calcular variâncias envolvidas nos procedimentos de discriminação da variação
    Parâmetros
      Expr : Expressão para a variância (veja observação a seguir)
      Value: Vetor que retorna o valor da expressão para cada variável resposta
      DF   : Vetor que retorna o número de graus de liberdade para cada variância
             calculada pela fórmula de Sattherthwaite para cada resposta
      Obs. : A rotina calcula a variancia como (k/n)*v, sendo k a soma de quadrados dos
             coeficientes, n o número de observações para a média e v a variância. A
             expressão para o cálculo da variância v deve ser indicada no formato exp/d,
             onde exp é uma expressão envolvendo termos do modelo e d é uma constante. Num
             modelo de parcelas divididas em blocos ao acaso, se A está em parcelas, B é
             blocos e C está em subparcelas, então para a comparação das médias de A a
             expressão será A.B (a variância será calculada como (k/n)*QM A.B) e para a
             comparação das médias de A dentro de B a expressão será
             v = (QM A.B + (b-1)*Residuo)/b e a variância calculada será (k/n)*v
    Observação
      Pode ser chamada somente para analises univariadas. Se Expr for um sequência vazia,
      Expr passará a conter a sequência RESIDUO
  }
  Var Eval  : TAvaliator;
      i,j,k : Integer;
      erro  : word;
      T     : TwsTerm;
      vars  : TStrings;
      NVars : Integer;
      B,C,D : double;
      SG    : TStrings;
      Divisor: Integer;
      s1, s2: String;
  Begin
    If Expr = '' Then Expr := 'RESIDUO';
    Divisor := 1;
    i := System.pos('/', Expr);
    if i > 0 then
       try
         SubStrings('/', s1, s2, Expr);
         Expr := SubString(s1, '(', ')');
         Divisor := StrToInt(s2);
       except
         Divisor := 1;
       end;

    NVars := TwsVec(Term[0].AsVec).Len;
    Value := TwsDFVec.Create(nVars);
    DF    := TwsDFVec.Create(nVars);

    Eval  := TAvaliator.Create;

    Try
      For i := 0 to Count - 2 do
        Eval.TabVar.AddFloat(Alias[i], 0);

      Try
        Expr := AjustFRatio(Expr, vars);
        Eval.Expression := Expr;
      Except
        Value.Free;
        DF.Free;
        Raise;
      End;

      For k := 1 to NVars Do
        Begin
        For i := 0 to Count - 2 do
          Begin
          T := Term[i];
          Eval.TabVar.SetFloatValue(Alias[i], {TwsVec(T.SSq)[k]}T.AsSSQ[k]/T.DF);
          End;

        SG := nil; // força a criação pela rotina abaixo
        Split(Expr, SG, ['+']);
        if SG <> nil then
           begin

           try
             // Percorre as sub-expressões
             B := 0;
             C := 0;
             for i := 0 to SG.Count-1 do
               begin
               Eval.Expression := SG[i];
               D:= Eval.Evaluate.AsFloat;
               B := B + D;

               for j := 0 to Count-2 do
                 if System.pos(Alias[j],SG[i])>0 then
                    begin
                    T := Term[j];
                    Break;
                    end;

               C := C + SQR(D)/T.DF;
               end; // for i

           finally
             SG.Free;
             end;

           end; // if SG...

        Value[k] := B/Divisor;
        DF[k]    := SQR(B)/C;
        End; // For k...

    Finally
      Eval.Free;
    End;
  End;


(*  Procedure TwsTermList.GetVariances(Expr: String; Var Value, DF: TwsVec);
  Var Eval  : TAvaliator;
      i,k   : Integer;
      erro  : word;
      T     : TwsTerm;
      vars  : TStrings;
      NVars : Integer;
  Begin
    If Expr = '' Then Expr := 'Residuo';

    NVars := TwsVec(Term[0].SSq).Len;
    Value := TwsDFVec.Create(nVars);
    DF    := TwsDFVec.Create(nVars);

    Eval  := TAvaliator.Create;

    Try
      For i := 0 to Count - 2 do
        Eval.TabVar.AddFloat(Alias[i], 0);

      Try
        Eval.Expression := AjustFRatio(Expr, vars);
      Except
        Value.Free;
        DF.Free;
        Raise;
      End;

      For k := 1 to NVars Do
        Begin
        For i := 0 to Count - 2 do
          Begin
          T := Term[i];
          Eval.TabVar.SetFloatValue(Alias[i], TwsVec(T.SSq)[k] / T.DF);
          End;

        Value[k]        := Eval.Evaluate.Value;
        DF[k]           := SatDF(k, vars);
        End;

    Finally
      Eval.Free;
    End;
  End;
*)

  Procedure TwsTermList.UserFTest;
  { Objetivo
      Executa o teste F a partir das especificações do analista. A expressões que compõem os
      quadrados médios do numerador e denominador são calculadas e utilizadas para ciompor a
      razão F.
  }
  Var Eval  : TAvaliator;
      i,k   : Integer;
      erro  : word;
      T     : TwsTerm;
      vars  : TStrings;
      NVars : Integer;
  Begin
    NVars := TwsVec(Term[0].AsVec).Len;
    Eval  := TAvaliator.Create;

    Try
    For i := 0 to Count - 2 do
      Eval.TabVar.AddFloat(Alias[i], 0);

    For i := 0 to Count - 3 do
      with Term[i] do
        if FDen = Nil then
           begin
           FDen    := TwsDFVec.Create(NVars);
           FNum    := TwsDFVec.Create(NVars);
           FProb   := TwsDFVec.Create(NVars);
           VarComp := TwsDFVec.Create(NVars);
           DenDF   := TwsDFVec.Create(NVars);
           NumDF   := TwsDFVec.Create(NVars);
           FValue  := TwsDFVec.Create(NVars)
           end;

    For k := 1 to NVars Do
      Begin

      For i := 0 to Count - 2 do
        Begin
        T := Term[i];
        Eval.TabVar.SetFloatValue(Alias[i],{TwsVec(T.SSq)[k]}T.AsSSq[k]/T.DF);
        End;

      For i := 0 to Count - 3 do
        With Term[i] do
          If FTest Then
             Begin
             Eval.Expression := AjustFRatio(NumExp, vars);
             TwsVec(FNum)[k] := Eval.Evaluate.AsFloat;
             NumDF[k]        := SatDF(k, vars);
             Eval.Expression := AjustFRatio(DenExp, vars);
             TwsVec(FDen)[k] := Eval.Evaluate.AsFloat;
             DenDF[k]        := SatDF(k, vars);
             FValue[k]       := TwsVec(FNum)[k] / TwsVec(FDen)[k];
             FProb[k]        := wsFuncoesDeProbabilidade.FProb(DF,DenDF[k],FValue[k],erro);
             End
          Else
             begin
             FValue[k] := wscMissValue;
             FProb[k] := wscMissValue;
             end;
      End;

    Finally
      Eval.Free;
    End;
  End;

  Function  TwsTermList.SatDF(k: Integer; Vars: TStrings): double;
  {  Objetivo
       Estima graus de liberdade através da fórmula de Satertwaithe
     Parâmetros
       k: Indica a variável resposta em questão
       Vars: Variáveis envolvidas no cálculo
  }
  var i: Integer;
      x1,xx : double;
      T: TwsTerm;
  Begin
    xx := 0;
    x1 := 0;
    For i := 0 to Vars.Count-1 do
      Begin
      T := TermByName(Vars[i]);
      x1 := x1+T.MSq[k];
      xx := xx + SQR(T.MSq[k]) / T.DF;
      End;

    Result := x1*x1 / xx;
  End;

procedure TwsTermList.AnovaTable(RNames: TStrings; nx: Integer;
                                 GMeans:TwsVec; Opt:TBits;
                                 Output: TwsBXML_Output;
                                 CreatedObject: TwsCreatedObject_Event);
{ Objetivo
    Constrói quadro de análise da variação, imprime estatísticas auxiliares e valores
    esperados de quadrados médios
  Parâmetros
    RNames: Nomes das variaveis respostas
    nx    : Número de covariáveis no modelo
    GMeans: Médias gerais das variáveis numéricas pérsentes no modelo
    Opt   : Opções relacionadas a análise do modelo linear
  Saídas
    Conjunto de dados com o quadro da análise, que, dependendo das opções poderá ser
    impresso ou armazenado
}
var
  ATable : TwsDataSet;
  AuxStat: TwsGeneral;
  T      : TwsTerm;
  i, k   : Integer;
  v      : TwsVec;
  pr     : Boolean;
  st     : string;
begin
  for k := 1 to RNames.Count do { Para todas as variáveis dependentes }
    begin
    ATable := TwsDataSet.Create('Quadro');
    ATable.PrintOptions.PrintDesc := (k=1);
    ATable.MLab := 'Quadro da análise da variação';
    ATable.ColIdentName := 'Fontes';
    ATable.Struct.AddNumeric('GL', 'Graus de liberdade',8,6);                          {1}
    ATable.Struct.AddNumeric('SQ', 'Soma de Quadrados');                               {2}
    ATable.Struct.AddNumeric('QM', 'Quadrado Médio',12,7);                             {3}
    ATable.Struct.AddNumeric('F', 'Valor observado da estatística F',10,5);            {4}
    ATable.Struct.AddNumeric('p',
      'Probabilidade de ocorrer um valor maior que F observado',11,4);                 {5}
    for i := 0 to Count-3 do
      begin
      T := Term[i];
      v := TwsDFVec.Create(5);
      v.Name := Self[i];              // Nome do termo
      v[1] := T.DF;                   // Graus de liberdade
      v[2] := T.AsSSq[k];             // Soma de quadrados
      v[3] := T.MSq[k];               // Quadrado médio
      v[4] := T.FValue[k];            // Estatística F
      v[5] := T.FProb[k];             // Probabilidade
      ATable.MAdd(v)
      end; { for i }
    {Resíduo}
    v := TwsDFVec.Create(6);
    T := Term[Count - 2];
    v.Name := Self[Count - 2];       // Nome do termo
    v[1] := T.DF;                    // Graus de liberdade
    v[2] := T.AsSSq[k];              // Soma de quadrados
    v[3] := v[2]/v[1];               // Quadrado médio
    v[4] := wscMissValue;
    v[5] := wscMissValue;
    ATable.MAdd(v);
    if Opt[ord(cmVar_PrintQuadAnalysis)] then
      begin
      AuxStat := TwsGeneral.Create(1,3);
      with AuxStat do
        begin
        Name := 'Variavel';
        MLab := 'Estatísticas auxiliares';
        PrintOptions.ColWidth := 10;
        PrintOptions.ColPrecision := 7;
        ColName[1] := 'Media_Geral'; ColName[2] := 'Coef_Var'; ColName[3] := 'Desv_Padr';
        end;
      AuxStat.RowName[1] := RNames[k-1];
      AuxStat[1,1] := GMeans[nx+k];
      AuxStat[1,3] := ScalarSqrt(v[3]);
      AuxStat[1,2] := ScalarProd(ScalarDiv(AuxStat[1,3], AuxStat[1,1]), 100);
      end;
    {Total}
    v := TwsDFVec.Create(5);
    T := Term[Count - 1];
    v.Name := Self[Count - 1];       // Nome do termo
    v[1] := T.DF;                    // Graus de liberdade
    v[2] := T.AsSSq[k];        // Soma de quadrados
    v[3] := wscMissValue;
    v[4] := wscMissValue;
    v[5] := wscMissValue;
    ATable.MAdd(v);
    { Verificar opcoes de impressao e armazenamento }
    if Opt[ord(cmVar_PrintQuadAnalysis)] then
      begin
      Output.BeginText;
      Output.CenterTitle(3, 'Variável Resposta: '+RNames[k-1]);
      Output.EndText;
      Output.Add(ATable);
      Output.Add(AuxStat);
      // Nova tabela para descrição das razoes F
      OutPut.BeginText;
        Output.CenterTitle(3, 'Composição dos Testes F e Componentes de Variância');
        Output.BeginTable;
          Output.WriteTableHeader(['Termo','QM Numer','GL Num','QM Den','GL Den','Estatistica F',
            'Tipo','Comp Var']);
          for i := 0 to Count-3 do
            begin
            Output.BeginTableRow;
            T := Term[i];
            if T.FTest then
              begin
              Output.NewTableCell(Self[i],True);
              Output.NewTableCell(FloatToStrF(T.MSq[k],ffGeneral,8,12));
              Output.NewTableCell(FloatToStr(T.DF));
              Output.NewTableCell(FloatToStrF(TwsVec(T.FDen)[k],ffGeneral,8,12));
              Output.NewTableCell(FloatToStrF(T.DenDF[k],ffGeneral,7,12));
              Output.NewTableCell(FloatToStrF(T.FValue[k],ffGeneral,5,9));
              if T.RandTerm then
                begin
                Output.NewTableCell('Aleatório');
                Output.NewTableCell(FloatToStrF(TwsVec(T.VarComp)[k],ffGeneral,8,12))
                end
              else
                begin
                Output.NewTableCell('Fixo');
                Output.NewTableCell('.')
                end
              end; // if
            Output.EndTableRow;
            end; // for i
        Output.EndTable;
      OutPut.EndText;
      end;
    if Opt[ord(cmVar_SaveQuadAnalysis)] then
       CreatedObject(Self, ATable)
    else
       FreeAndNil(aTable);
    AuxStat.Free;
    end; //for k
  // Cria a descrição em forma de tabela
  if Opt[ord(cmVar_PrintExpVal)] then
    begin
    OutPut.BeginText;
    OutPut.CenterTitle(3, 'Razões F e Valores Esperados dos Quadrados Médios');
    // tabela para informacao da composicao do teste F e valores esperados
    Output.BeginTable;
    Output.WriteTableHeader(['Termo', 'QM Numerador', 'QM Denominador', 'Valor Esperado QM']);
    for i := 0 to Count-3 do
      begin
      Output.BeginTableRow;
      T := Term[i];
      if T.FTest then
        begin
        Output.NewTableCell(Self[i],True);
        if T.NumExp <> '' then
           Output.NewTableCell(T.NumExp)
        else
           Output.NewTableCell(Self[i]);
        Output.NewTableCell(T.DenExp);
        if T.ExpectMS <> '' then
           Output.NewTableCell(T.ExpectMS);
        end; // if
      Output.EndTableRow;
      end; // for i
    Output.EndTable;
    OutPut.EndText;
    Output.Add(FT)
    end;

  OutPut.WriteLink('Mais detalhes sobre Quadro da Análise',
     SysUtilsEx.GetApplicationDir + 'Ajuda\Modelos_Lineares\Quadro da Analise.htm');
end; { TwsTermList.AnovaTable }

function TermCand(n: Integer; const TI, TJ: TBits): Boolean;
{Objetivo
   Retorna True se para todas as posições True em TI exista uma posição True correspondente em TJ. O
   parâmetro n é a dimensão dos arrays de bits. Retorna False se em pelo menos uma posição não houver
   concordância
}
var
  i: Integer;
begin
  Result := True;
  for i := 0 to n-1 do
    if TI[i] then
      if not TJ[i] then
         begin
         Result := False;
         Break
         end;
end; {TermCand}
(*
procedure TwsTermList.AutoFTest(nr: Integer; IResp:TwsLIVec);
{ Objetivo
    Obtenção do valor da estatística F de modo automático. Utilizando um algoritmo calcula
    o numerador e denominador adequados para produzir uma razão F correta.
   Parâmetros
     nr: dimensão dos arrays de bits (Ctrl, Effect, Fact)
     IResp: Índices das variáveis respostas no conjunto de dados em análise
}

const
  VarTerm: string = 'Var(%s)';
  FixTerm: string = 'Q(%s)';
  FTerm: string = 'Func(%s)';

var
  i,j,k,NTerms : Integer;
  Erro         : Word;
  MSqRes,DFRes,
  ndf,vdf,x    : double;
  TI, TJ       : TwsTerm;
  st           : string;

begin
  DFRes := Term[Count-2].DF;
  NTerms := Count - 3;   // Numero de termos que serao tratados
  // Inicializa quantidades (dimensao: numero de respostas);
  For i := 0 to NTerms do
    Begin
    TI := Term[i];
    with TI do
      if FDen = nil then
         begin
         FDen    := TwsDFVec.Create(IResp.Len);
         FNum    := TwsDFVec.Create(IResp.Len);
         FProb   := TwsDFVec.Create(IResp.Len);
         VarComp := TwsDFVec.Create(IResp.Len);
         DenDF   := TwsDFVec.Create(IResp.Len);
         NumDF   := TwsDFVec.Create(IResp.Len);
         FValue  := TwsDFVec.Create(IResp.Len)
         end;
    End;
  // Para cada resposta
  For k := 1 to IResp.Len do
    Begin
    MSqRes := Term[Nterms+1].MSq[k];
    { Para cada termo do modelo, comecando pelo ultimo }
    for i := Nterms downto 0 do
      begin
      TI := Term[i];
      with TI do
        begin
        { As duas linhas seguintes diferem quando a analise for multivariada. }
        TwsVec(VarComp)[k] := (MSq[k]-MSqRes)/Coef;   { Componente de variancia para o termo i }
        TwsVec(FDen)[k] := MSqRes; { Denominador da estatistica F }
        vdf:=MSqRes;
        ndf:=0;
        ExpectMS := Format(VarTerm,['Erro']);
        DenExp := '';
        st:=Self[i];
        end;
      // Todos os termos que estao apos o termo i (de ordem superior)
      for j := NTerms downto i+1 do
        begin
        TJ := Term[j];
        if TJ.RandTerm then
          begin
          if TermCand(nr, TI.Fact, TJ.Fact) then     { o termo j eh candidato? }
//            if TermCand(TJ.Effect, TI.Ctrl) then { se for, ira participar? }
             begin
             TI.ExpectMS:=TI.ExpectMS+'+'+FloatToStr(TJ.Coef)+Format(VarTerm,[Self[j]]);
             TI.DenExp := TI.DenExp+Self[j]+',';

             { As duas linhas seguintes diferem quando a analise for multivariada }
             TwsVec(TI.VarComp)[k] := TwsVec(TI.VarComp)[k] - (TJ.Coef/TI.Coef)*TwsVec(TJ.VarComp)[k];

             x:=TJ.Coef*TwsVec(TJ.VarComp)[k];
             TwsVec(TI.FDen)[k] := TwsVec(TI.FDen)[k] + x;
             vdf:=vdf+x;
             ndf:=ndf+Sqr(TJ.MSq[k])/TJ.DF
             end
          end
        else
          if EstaContido(Self[i],Self[j]) then
            st:=st+','+Self[j]
        end; { for j }
      with TI do
        if FTest then
           begin { Se o teste sera feito para o termo i }
           if DenExp = '' then DenExp:='RESIDUO';
           If (DenExp <> 'RESIDUO') Then
              System.Delete(DenExp, Length(DenExp), 1);
           if ndf <> 0 then
              DenDF[k] := Sqr(vdf)/ndf
           else
              DenDF[k] := DFRes;
           // Verifica se o termo eh fixo ou aleatorio
           if TI.RandTerm then
             ExpectMS := ExpectMS+'+'+FloatToStr(Coef)+Format(VarTerm,[st])
           else
             ExpectMS := ExpectMS+'+'+FloatToStr(Coef)+Format(FixTerm,[st]);

           { As duas linhas seguintes diferem quando a analise for multivariada }
           FValue[k] := MSq[k]/TwsVec(FDen)[k];
           FProb[k] := FInt(FValue[k],DF,DenDF[k],True,Erro)
           end
        else
           begin
           FValue[k] := wscMissValue;
           FProb[k] := wscMissValue
           end;
      end { for i }
    End; {for k}
end; { AutoFTest }
*)

procedure TwsTermList.AutoFTest(nr: Integer; IResp:TwsLIVec);
{ Objetivo
    Obtenção do valor da estatística F de modo automático. Utilizando um algoritmo calcula
    o numerador e denominador adequados para produzir uma razão F correta.
   Parâmetros
     nr: dimensão dos arrays de bits (Ctrl, Effect, Fact)
     IResp: Índices das variáveis respostas no conjunto de dados em análise
}

const
  VarTerm: string = 'V(%s)';
  FixTerm: string = 'Q(%s)';
var
  i,j,k,NTerms,
  nt,kk,jj     : Integer;
  Erro         : Word;
  MSqRes,DFRes : Double;
  TI, TJ       : TwsTerm;
  st           : string;
  x            : TwsVec;

  function LineComp(j1: Integer): Integer;
  {Compara linha j1 desde linha j1-1 ate a primeira, da posicao 1 ate j1-1. Se houver uma linha igual,
  retorna o seu indice; senao retorna -1}
  var
    i1,i2: Integer;
    Ok   : Boolean;
  begin
    // para cada linha, da anterior ate a primeira
  Result:=-1;
  for i1:=j1-1 downto 1 do
    begin
    i2:=1; Ok:=True;
    // Testa linha j1 com linha i1
    while Ok and (i2<=j1-1) do
      begin
      Ok:=FEquals(FT[j1,i2],FT[i1,i2]);
      Inc(i2);
      end;
    if Ok then
      begin
      Result:=Count-1-i1;
      Exit
      end
    end // for
  end; // LineComp

  function MSComb(k1: Integer): TwsVec;
  { Obtem a combinação linear das linhas de FT, anteriores a k1, que produz k1. Essa será a combinação
    linear dos quadrados médios cujo valor esperado é igual ao da fonte de variação k1, sob Ho}
  var
    i1,j1,n: Integer;
    w      : Double;
  begin
  n:=k1-1;
  Result:=TwsDFVec.Create(n);
  Result[n]:=FT[k1,n]/FT[n,n];
  for i1:=n-1 downto 1 do
    begin
    w:=0;
    for j1:=i1+1 to n do
      w:=w+FT[j1,i1]*Result[j1];
    Result[i1]:=(FT[k1,i1]-w)/FT[i1,i1]
    end;
  end;

begin
  DFRes := Term[Count-2].DF;
  NTerms := Count-3;   // Numero de termos que serao tratados
  x:=nil;
  // Inicializa quantidades (dimensao: numero de respostas);
  For i := 0 to NTerms do
    Begin
    TI := Term[i];
    with TI do
      if FDen = nil then
         begin
         FDen    := TwsDFVec.Create(IResp.Len);
         FNum    := TwsDFVec.Create(IResp.Len);
         FProb   := TwsDFVec.Create(IResp.Len);
         VarComp := TwsDFVec.Create(IResp.Len);
         DenDF   := TwsDFVec.Create(IResp.Len);
         NumDF   := TwsDFVec.Create(IResp.Len);
         FValue  := TwsDFVec.Create(IResp.Len)
         end;
    End;
  FT:=TwsTriangular.Create(NTerms+2);
  FT.Name:='CoefComp';
  FT.MLab:='Coeficientes dos componentes de variância dos termos';
  nt:=FT.nCols;
  FT[1,1]:=1;
  FT.RowName[1]:='RESIDUO';
  FT.ColName[1]:='RESIDUO';
  // Para cada resposta
  For k := 1 to IResp.Len do
    Begin
    kk:=1;     // kk marca a linha de FT
    MSqRes := Term[NTerms+1].MSq[k];
    { Para cada termo do modelo, comecando pelo ultimo }
    for i := NTerms downto 0 do
      begin
      Inc(kk);
      TI := Term[i];
      with TI do
        begin
        { As duas linhas seguintes diferem quando a analise for multivariada. }
        TwsVec(VarComp)[k] := (MSq[k]-MSqRes)/Coef;   { Componente de variancia para o termo i }
        ExpectMS := Format(VarTerm,['Erro']);
        st:=Self[i];
        FT[kk,1]:=1;
        FT.RowName[kk]:=Self[i];
        FT.ColName[kk]:=Self[i];
        FT[kk,kk]:=Coef;
        end;
      // Todos os termos que estao apos o termo i (de ordem superior)
      for j := NTerms downto i+1 do
        begin
        TJ := Term[j];
        if TJ.RandTerm then
          begin
          if TermCand(nr, TI.Fact, TJ.Fact) then     { o termo j eh candidato? }
            begin
            TI.ExpectMS:=TI.ExpectMS+'+'+FloatToStr(TJ.Coef)+Format(VarTerm,[Self[j]]);
            // Atualiza os componentes de variancia
            TwsVec(TI.VarComp)[k] := TwsVec(TI.VarComp)[k] - (TJ.Coef/TI.Coef)*TwsVec(TJ.VarComp)[k];
            // Atualiza matriz com os coeficientes dos componentes
            FT[kk,nt-j]:=TJ.Coef
            end
          else
            FT[kk,nt-j]:=0
          end
        else
          begin
          // Se estiver contido, efeito esta confundido
          if EstaContido(Self[i],Self[j]) then
            st:=st+','+Self[j];
          FT[kk,nt-j]:=0
          end
        end; { for j }
      with TI do
        begin
        if RandTerm then
          ExpectMS := ExpectMS+'+'+FloatToStr(Coef)+Format(VarTerm,[st])
        else
          ExpectMS := ExpectMS+'+'+FloatToStr(Coef)+Format(FixTerm,[st]);
        // Teste F sera executado para termo i?
        if FTest then
          begin
          // Verifica se existe alguma linha igual (valores esperados iguais sob Ho)
          jj:=LineComp(kk);
          if jj<>-1 then  // se achou, o termo jj sera o residuo
            begin
            TJ := Term[jj];
            DenExp := Self[jj];
            TwsVec(FDen)[k]:=TJ.MSq[k];
            DenDF[k]:=TJ.DF
            end
          else
            begin
            // Monta e resolve o sistema pq nao tem nenhuma linha igual. x retorna a combinacao linear
            x:=MSComb(kk);
            TwsVec(FDen)[k]:=0;
            DenDF[k]:=0;
            DenExp:='';
            for j:=1 to x.Len do
              if not FEquals(x[j],0) then
                begin
                jj:=Count-1-j;
                TJ:=Term[jj];
                // quais quadrados medios compoem o denominador?
                DenExp:=DenExp+FloatToStr(x[j])+Self[jj]+'+';
                // Valor para o denominador
                TwsVec(FDen)[k]:=TwsVec(FDen)[k]+x[j]*TJ.MSq[k];
                // Denominador para Sattert
                DenDF[k]:=DenDF[k]+Sqr(TJ.MSq[k])/TJ.DF
                end;
            System.Delete(DenExp,Length(DenExp),1);
            DenDF[k]:=Sqr(TwsVec(FDen)[k])/DenDF[k];
            end;
          FValue[k] := MSq[k]/TwsVec(FDen)[k];
          FProb[k] := FInt(FValue[k],DF,DenDF[k],True,Erro)
          end
        else
          begin
          FValue[k] := wscMissValue;
          FProb[k] := wscMissValue
          end  // if FTest
        end // with
      end { for i }
    end; {for k}
  x.Free
end; { AutoFTest }


procedure TwsTermList.BContrVar(const FacName: string; FCol,Resp: TwsLIVec; Means: TwsGeneral; DS: TwsDataSet;
  out DF, V: TwsVec);
{ Objetivo
    Obtem a variância do contraste entre médias observadas para o caso balanceado. O algoritmo para
    obtenção foi adaptado de Zonta e Silva (1999)
  Parâmetros
    FacName : Nome do fator cujas médias dos níveis serão comparadas
    FCol : Índices das colunas dos fatores fixados
    Resp : Índice da variáveis respostas
    Means: Matriz que contém as médias e o respectivo número de repetições (aos pares)
    DS   : Conjunto de dados;
    DF   : Retorna o número de graus de liberdade de cada variância
    V    : Retorna a variância do contraste para cada variável resposta
}

  function GetDiv(const stTerm: string; r: Integer): Integer;
  { Retorna o numero de repeticoes dividido pelo numero de niveis dos fatores que nao estao no termo
    nem sao fatores fixados }

    function IndFix(i: Integer): Boolean;
    { Retorna True se i é um indice de fator fixado }
    var
      j: Integer;
    begin
    j:=1;
    Result:=FCol=nil;
    while (not Result) and (j<=FCol.Len) do
      begin
      Result:=(i=FCol[j]);
      Inc(j)
      end;
    end;

  var
    i  : Integer;
    Col: TwsDataSetCol;
  begin
  Result:=r;
  for i:=1 to DS.NCols do
    begin
    Col:=DS.Struct.Col[i];
    if (Col is TwsFactor) and (not EstaContido(Col.Name,stTerm) and (not IndFix(i))) then
      Result := Result div TwsFactor(Col).Levels
    end
  end;

var
  r,k,j,nTerms,
  kk          : Integer;
  MSRes,ndf,x : Double;
  T           : TwsTerm;
begin
  nTerms := Count - 3;   // Numero de termos que serao tratados
  V:=TwsDFVec.Create(Resp.Len);   // vetor das variancias, uma para cada resposta
  DF:=TwsDFVec.Create(Resp.Len);  // vetor dos graus de liberdade, um para cada resposta
  for k := 1 to Resp.Len do
    begin
    r:=Trunc(Means[1,k+1]);  // numero de repeticoes eh constante
    MSRes := Term[nTerms+1].MSq[k];
    ndf:=0;
    x:=0;
    V[k]:=MSRes/r;
    j := nTerms;
    while ((Order[j]>1) and (j>=0)) do
      begin
      T := Term[j];
      if (T.RandTerm and EstaContido(FacName,Self[j])) then
        begin
        kk:=GetDiv(Self[j],r);
        V[k]:=V[k]+TwsVec(T.VarComp)[k]/kk;
        x:=x+T.MSq[k]/kk;
        ndf:=ndf+Sqr(T.MSq[k]/kk)/T.DF
        end;
      Dec(j)
      end;
      if ndf=0 then
        DF[k]:=Term[Count-2].DF
      else
        DF[k]:=Sqr(x)/ndf
    end; // para cada resposta
end; // BContrVar

procedure TwsTermList.MultAutoFTest(nr: Integer; IResp:TwsLIVec);
{ Obtenção das matrizes de SQ&P adequadas à análise multivariada }
  function TermCand(const TI, TJ: TBits): Boolean;
  var
    i: Word;
  begin
    Result := True;
    for i := 0 to nr-1 do
      if TI[i] then
        if not TJ[i] then
           begin
           Result := False;
           Break
           end;
  end; {TermCand}

const
  VarTerm: string = 'Var(%s)';
  FTerm: string = 'Função(%s)';

var
  i,j,NTerms,k1,k2 : Word;
  SI,SJ,SSqRes     : TwsSymmetric;
  DFRes            : double;
  TI,TJ            : TwsTerm;
  st               : string;

begin
  DFRes := Term[Count-2].DF;
  NTerms := Count-3;   // Numero de termos que serao tratados

  For i := 0 to NTerms do
    Begin
    TI := Term[i];
    with TI do
      if FDen = nil then
         begin
         FDen    := TwsSymmetric.Create(IResp.Len);
         FNum    := TwsSymmetric.Create(IResp.Len);
         VarComp := TwsSymmetric.Create(IResp.Len);
         DenDF   := TwsDFVec.Create(1);
         NumDF   := TwsDFVec.Create(1);
         end;
    End;
  SSqRes := Term[Nterms+1].AsSSP;
  { Para cada termo do modelo, comecando pelo ultimo }
  for i := Nterms downto 0 do
    begin
    TI := Term[i];
    with TI do
      begin
      { As duas linhas seguintes diferem quando a analise for multivariada. }
      for k1:=1 to TwsSymmetric(VarComp).NRows do
        for k2:=1 to k1 do
          TwsSymmetric(VarComp)[k1,k2]:=(AsSSP[k1,k2]-SSqRes[k1,k2])/Coef;
      FDen := SSqRes;
      ExpectMS := Format(VarTerm,['Erro']);
      DenDF[1] := 0;
      DenExp := '';
      end;
    for j := NTerms downto i+1 do
      begin
      TJ := Term[j];
      if TermCand(TI.Fact, TJ.Fact) then    { o termo j e candidato? }
        if TermCand(TJ.Effect, TI.Ctrl) then
           begin   { se for, ira participar? }
           TI.ExpectMS:=TI.ExpectMS+' + ' +FloatToStr(TJ.Coef)+Format(VarTerm,[Self[j]]);
           TI.DenExp := Format(FTerm,[Self[j]+',']);
           { As duas linhas seguintes diferem quando a analise for multivariada }
           for k1:=1 to TwsSymmetric(TI.VarComp).NRows do
             for k2:=1 to k1 do
               begin
               TwsSymmetric(TI.VarComp)[k1,k2] := TwsSymmetric(TI.VarComp)[k1,k2] -
                 (TJ.Coef/TI.Coef)*TwsSymmetric(TJ.VarComp)[k1,k2];
               TwsSymmetric(TI.FDen)[k1,k2] := TwsSymmetric(TI.FDen)[k1,k2] -
                 TJ.Coef*TwsSymmetric(TJ.VarComp)[k1,k2];
               end
           end
      end; { for j }
    with TI do
      if FTest then
         begin { Se o teste sera feito para o termo i }
         If (DenExp <> '') and (DenExp[Length(DenExp)-1] = ',') Then
            System.Delete(DenExp, Length(DenExp)-1, 1);
            DenDF[1] := DFRes;
         ExpectMS := ExpectMS+' + '+FloatToStr(Coef)+Format(VarTerm,[Self[i]]);
         end
      else
         begin
//           FValue[k] := wscMissValue;
//           FProb[k] := wscMissValue
         end;
    end { for i }

end; { MultAutoFTest }

End.

