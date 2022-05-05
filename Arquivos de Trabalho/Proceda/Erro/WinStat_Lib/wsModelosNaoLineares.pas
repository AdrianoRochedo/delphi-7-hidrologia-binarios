unit wsModelosNaoLineares;

(*
  IMPLEMENTAÇÃO:
    Alex Dias Gonsales - 10/1998

  MODIFICACOES:

  AUTOR E DATA: ........................
  ESCOPO: ..............................
  OBS.: ................................

  Obs.:
  ok1 - variáveis internas estão sendo liberadas corretamente.
  ver - ver com Amauri.
  ok  - está tudo ok.

  --------------------------------------------------------------------------

  Diretivas de Compilação.

  TESTE - 21/01/99 Impressões no gEditor para teste.


==============================================================================

  FUNÇÃO:

  Estimativa de parâmetros em modelos não lineares.

  FUNCIONAMENTO E UTILIZAÇÃO:

  - Declarar um objeto do tipo TwsNLinMod.
  - Criar uma instância através de TwsNLinDeriv se for utilizar os métodos que
    necessitam de derivadas ou TwsDUD se não for especificar as derivadas.
  - Não será necessário mandar mensagem alguma para o objeto, no momento da
    criação do objeto, parâmetros serão passados ao mesmo, e logo após o objeto
    ser criado a avaliação do modelo será realizada.

  Obs.:

  - Só pode ter uma variável independente(preditora) e uma variável dependente no modelo.

  - Parametro tem que ser um identificador valido e nao pode ser nem nome de
    função nem um nome de alguma coluna do conjunto de entrada (DataSet).

  - Valor inicial para o parametro deve ser um numero real(float) ou inteiro.

  - Epressao da derivada pode conter funções, constantes,
    nomes de parametros e/ou o nome da variável preditora.

  - Variáveis de grupo: só é permitido se a variável for do tipo fator.

  - Segundo a observação acima, automaticamente não é permitido que uma
    variável de grupo faça parte da expressão do modelo. Porém a seguinte
    exceção pode ocorrer:
    - Está aceitando variavel de grupo no lado esquerdo do modelo. Ex.:
      Raiz(dose) ~ altura * b1
      onde dose é uma variável de grupo (e do tipo fator consequentemente).
      Neste caso está sendo usado o valor do índice do nível do valor
      da variável no cálculo (embora isso não tenha significado algum).

------------------- Operadores Reconhecidos -------------------------------

  soma               +
  subtração          -
  multiplicação   * ou #
  divisão            /
  potenciação     ** ou ##

  Obs.: Todos os caracteres '*' (asterisco) entrados na expressão do
        modelo ou nas expressões das derivadas são trocados por '#' antes
        de passar para o avaliador. Isto é necessário porque os operadores
        '*' e '**' deverão ser tratados como operadores elemento a elemento,
        e para o Avaliador, eles não são operadores elemento a elemento.
        Para o Avaliador, os operadores elemento a elemento correspondentes
        são o '#' e o '##', por isso é realizada essa troca.

----------------------- Funções Reconhecidas --------------------------------

  Reconhece todas as funções da unit uEval.


===============================================================================
=========================== Mensagens de Erro =================================
===============================================================================

------------------------------- Modelo ----------------------------------
Pop ~ raiz *ano * b1
Erro na avaliacao do modelo <Pop ~ raiz *ano * b1>.
Falta de "(".

Erro na avaliacao do modelo <Pop ~ ano * b1 +>.
Falta de identificador.

------------------------------- Derivadas -------------------------------

Raiz
Erro na avaliacao da derivada<raiz>.
Falta de "(".

Erro na avaliacao da derivada <altura2>.
Variável <ALTURA2> desconhecida.

*)

{ DEFINE TESTE }

interface
Uses Classes,
     wsConstTypes,
     wsMatrix,
     wsVec,
     wsAvaliadorDeExpressoes,
     wsTabelaDeSimbolos,
     wsBXML_Output,
     wsGLib;

Const
  {Constantes utilizadas na Janela ModelNL3, servem como indice para
   a variavel bitsOpcoes(TBits)}
  cIDSFrame = 1;
  cADSFrame = 2;
  cIMatCov = 3;
  cAMatCov = 4;
  cIEstPar = 5;
  cAEstPar = 6;
  cIIter = 7;
  cAIter = 8;
  cICorr = 9;
  cACorr = 10;

  cGauss = 0;
  cHart  = 1;
  cMarq  = 2;
  cDUD   = 3;

  cNomesMetodos : Array [0..3] of string = ('Gauss-Newton',
                                            'Modificado de Hartley',
                                            'Marquardt',
                                            'DUD - Livre de Derivada');

Type

  {---------------------------------------------------------------------------}

 { Herança
     TwsNLinMod --> TObject
   Objetivo
     Anãlise de modelos não lineares. Dentre suas características, pode-se citar:
       - Indicação de derivadas como expressões, valores iniciais e precisão de convergência
       - Possui varios métodos de ajustamento
       - Expressões das derivadas como funções gerais
       - Os operadores reconhecidos são:
         +    soma
         -    subtração
         #    produto
         ##   potência
 }


  TwsNLinMod = Class
  Private
    bitsOpcoes :TBits;      // Opcoes gerias para saidas (Não liberar)
    CondConverg, { Resultado do processo iterativo. Condicao de convergencia
                  0 - Saiu do processo interativo e convergência foi alcançada.
                  1 - Não houve melhoras nas subiteracoes
                  2 - Saiu do processo interativo e convergência não foi alcançada,
                      esgotou o no. de iterações.}
    iMaxIter : Integer;     // Número máximo de iterações permitidas
    fEps     : Double;       // Precisão do processo iterativo
    DataSet                 // DataSet de onde serão escolhidos os dados e as duas colunas
                            // referentes às variáveis preditora e dependente
             : TwsDataSet;
    sModel,                 // Expressão do modelo
    sExprModel,          // Contém somente a parte da direita do modelo. Após o sinal '~'
    sCondicao               // Condicao para filtro de observacoes
               : string;
    stsVarsGrupo            // Variáveis de grupo
                 : TStrings;
    sDepName,               // Nome da variável dependente. Extraída do modelo
    sPredName               // Nome da variável preditora. Extraída do modelo
                    : String;
    vValoresIniciais        // Contém os valores iniciais dos parametros. Não é liberado.
                            // É só uma referência para o objeto passado como parâmetro
                      : TwsVec;
    mBeta    {Criada no Create com 4 linhas e iNPar colunas e destruída no Destroy.
              Matriz com 4 linhas cada uma contendo a seguinte informação:
              1 - Estimativas. Valores dos parametros. São atualizados a cada iteração.
              2 - Erro padrão.    (calculado em EstimateTable)
              3 - Extr. Inferior. (calculado em EstimateTable)
              4 - Extr. Superior. (calculado em EstimateTable)
              - Cada coluna contém o nome do parametro.
              - TwsNLinMod.EvalModel atualiza os valores dos parâmetros na TabVar
                utilizando os valores contidos na linha 1 de mBeta.
              - EstimateTable calcula as linhas 2, 3 e 4 e imprime ela (sempre).
              - Para cada DSFrame, a linha 1 é reinicializada com os valores de
                vValoresIniciais.
              }
             : TwsGeneral;
    iNPar    // Número de parametros. É igual a mBeta.NCols.
             : Integer;
    Eval     {Objeto que irá avaliar o modelo (sExprModel).
              Criado no Create e destruído no Destroy. O seu atributo TabVar irá conter os
              parâmetros bem como o Vetor com os valores da variável preditora
              (sPredName -> vPred). Para cada DSFrame, é deletado o vPred anterior que está
              na sua TabVar e adicionado um novo}
             : TAvaliator;
    lstDataSets // Lista com os DataSets gerados por ModelFrame.
                // Criado no Create e destruído no Destroy
             : TwsDataSets;
    DSFrame  {Somente uma referência para um dos DataSets gerados por ModelFrame.
              A Coluna 1 contém os dados da variável Dependente e a
              coluna 2 contém os dados da variável Preditora. No final do
              processo interativo o método interno GeraSaidas adiciona duas
              colunas a esta variável, uma com os valores de vDep e outra
              com os valores de vResid. Esta variável não é diretamente liberada
              pois ela é apenas um ponteiro temporário para um dos objetos (DataSet)
              de lstDataSets.}
             : TwsDataSet;
    iNObs    {Número de observações do conjunto sendo analisado (DSFrame).
              Setado em LoopDSFrame.}
             : integer;
    fSSRes,  {Soma dos quadrados dos resíduos.}
    fSSTot   {Soma de quadrados total não corrigido. Calculado em LoopDSFrame.}
             : Double;
    vPred,   {- Vetor que armazena os valores da variável preditora.
              - Criado no create e adicionado em Eval.TabVar.
              - Em TwsNLinDeriv.LoopDSFrame, para cada DSFrame,
                deleta vPred anterior de Eval.TabVar, cria um novo vPred fazendo uma
                cópia da coluna 2 de DSFrame e insere vPred em Eval.TabVar.
                Não é liberado diretamente pois o próprio Eval.TabVar destrói ele.
              Obs.: Os valores de vPred não são modificados durante o processo de iteração.}
    vDep,    {Vetor que armazena os valores da avaliação do modelo.
              Os valores de vDep serão modificados a cada iteração,
              quando for chamado EvalModel.
              Destruido e Criado em EvalModel. Em EvalModel é destruido o
              anterior e criado um novo.
              O último vDep criado em EvalModel é destruído em TwsNLinMod.Destroy.}
    vResid   {Vetor dos resíduos.
              Criado e calculado em EvalModel e atribuído à última linha de mX.
              Não deve ser destruído diretamente pois a própria matriz mX já o faz.}
             : TwsVec;
    mX,      {Matriz das derivadas. Contém iNPar+1 linhas e iNObs Colunas.
              A última linha é um ponteiro para vResid.
              Criada e destruída em LoopDSFrame para cada DSFrame.
              Em EvalDeriv as linhas 1 até iNPar são calculadas.
              Em EvalModel, a última linha de mX recebe vResid. Não devo
              liberar a linha anterior porque isto é feito automaticamente pelo
              prório objeto Matrix.}
    mIter    {Armazena os resultados do processo iterativo.
              Criada no início de LoopDSFrame (com 0 linhas e iNPar + aux colunas) para cada DSFrame.
              Em cada método Fit é adicionada uma linha para cada iteração.
              Destruída em gera_saidas se não foi escolhida a opção para armazenagem.}
             : TwsGeneral;
    mS ,     {Matriz do sistema de equacoes. A ultima linha contem Delta.
              Criada no create (com iNPar+1) e destruida no destroy.}
    mCov,     {Matriz das covariancias.
              Para cada DSFrame, é criada em GetCov e destruida em GeraSaidas.
              Obs.: GetCov é chamada a partir de GeraSaidas.}
    mCorr    {Matriz de correlações.
              Para cada DSFrame, é criada em GetCorr e destruida em GeraSaidas.
              Obs.: GetCorr é chamada a partir de GeraSaidas.}
             :TwsSymmetric;

    // Mecanismo para geração de relatórios
    FOutput: TwsBXML_Output;

    // Evento notificador de criação de objetos
    FOnCreatedObject: TwsCreatedObject_Event;

    Procedure EvalModel;
    Procedure AnovaTable;               // Cria a matriz 'Fontes', imprime e libera em seguida.
    Procedure TranspMul(A: TwsGeneral);
    Procedure GetCov; virtual;          // Cria e calcula mCov.
    Procedure GetCorr;                  // Cria e calcula mCorr.
    Procedure EstimateTable;            // Obtém estimativas e imprime.
    Procedure GeraSaidas(n_saida : integer);
    Procedure LoopDSFrame; virtual; abstract;
    Destructor Destroy; Override;
  Public
    Constructor Create(aDataSet:             TwsDataSet;
                       astsVarsGrupo:        TStrings;
                       asts_parametros:      TStrings;
                       av_valores_iniciais:  TwsVec;
                       const asModel:        String;
                       const asCondicao:     string;
                       aiMaxIter:            integer;
                       afEPS:                Double;
                       abitsOpcoes:          TBits;
                       aOutput:              TwsBXML_Output;
                       aCreatedObject_Event: TwsCreatedObject_Event);
  End; {TwsNLinMod}

  {---------------------------------------------------------------------------}

 { Herança
     TwsNLinMod --> TwsNLinDeriv --> TObject
   Objetivo
     Agrupa métodos de ajustamento que utilizam derivadas: Gauss, modificado de Hartley e
     Marquardt
 }

  TwsNLinDeriv = Class(TwsNLinMod)
  Private
    iMetodo  {Método a ser utilizado para avaliar o modelo.
               cGauss - Gauss-Newton
               cHart  - Hartley
               cMarq  - Marquardt}
             : Integer;
    stsDeriv {Expressões das derivadas parciais em relação aos respectivos Parametros.
             Cada uma das propriedades Object da lista contém um objeto uEval
             que irá avaliar a expressão da derivada. O atributo TabVar destes
             objetos uEval apontam para a TabVar de Eval.
             Criada em TwsNLinDeriv.Create e destruída em TwsNLinDeriv.Destroy.}
             : TStrings;
    Procedure EvalDeriv;             {Avalia todas derivadas e preenche a matriz mX.}
    Procedure LoopDSFrame; override; {Para cada DSFrame faz uma chamada a um dos métodos Fit.}
    Destructor Destroy; Override;
  Public
    // Cria objeto
    Constructor Create(aDataSet:             TwsDataSet;
                       astsVarsGrupo:        Tstrings;
                       asts_parametros:      TStrings;
                       av_valores_iniciais:  TwsVec;
                       astsDeriv:            TStrings;
                       const asModel:        String;
                       const asCondicao:     String;
                       aiMaxIter:            integer;
                       afEPS:                Double;
                       aiMetodo:             integer;
                       abitsOpcoes:          TBits;
                       aOutput:              TwsBXML_Output;
                       aCreatedObject_Event: TwsCreatedObject_Event);

    // Ajustamento pelo método de Gauss-Newton
    Procedure GaussNewtonFit;
    // Ajustamento pelo método modificado de Hartley
    Procedure HartleyFit;
    // Ajustamento pelo método de Marquardt
    procedure MarquardtFit;
  End; {TwsNLinDeriv}

 { Herança
     TwsNLinMod --> TwsDUD --> TObject
   Objetivo
     Ajustamento de modelos lineares que não utiliza derivadas informadas explicitamente. Na
     lingua inglesa é denominado DUD - Doesn't Use Derivatives
 }

  TwsDUD = Class(TwsNLinMod)
  Private
    DP : TwsGeneral;
    Procedure LoopDSFrame; override;
    Procedure GetCov; override;
  Public
    // Cria objeto
    Constructor Create(aDataSet:             TwsDataSet;
                       astsVarsGrupo:        TStrings;
                       asts_parametros:      TStrings;
                       av_valores_iniciais:  TwsVec;
                       const asModel:        String;
                       const asCondicao:     String;
                       aiMaxIter:            integer;
                       afEPS:                double;
                       abitsOpcoes:          TBits;
                       aOutput:              TwsBXML_Output;
                       aCreatedObject_Event: TwsCreatedObject_Event);

    // Destroi objeto da memória
    Destructor Destroy; override;
    // Ajustamento pelo métodd DUD
    Procedure DUDFit;
  End;{TwsDUD}

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
implementation
Uses sysUtils,
     SysUtilsEx,
     wsFuncoesDeProbabilidade,
     wsParser,
     wsFuncoesDeDataSets,
     wsProbabilidade;


(****************************************************************************
ok1*)
Constructor TwsNLinMod.Create(aDataSet:             TwsDataSet;
                              astsVarsGrupo:        TStrings;
                              asts_parametros:      TStrings;
                              av_valores_iniciais:  TwsVec;
                              const asModel:        String;
                              const asCondicao:     string;
                              aiMaxIter:            integer;
                              afEPS:                Double;
                              abitsOpcoes:          TBits;
                              aOutput:              TwsBXML_Output;
                              aCreatedObject_Event: TwsCreatedObject_Event);
{ Objetivo
    Participa da criação do objeto para anãlise de modelos não lineares. Analisa as expressões das
    derivadas segundo a sintaxe pré-estabelecida.
  Parâmetros
    aDataSet> conjunto de dados

}


var sts_Vars  : TStrings; {Conterá os nomes das duas variáveis que serão passadas para ModelFrame.
                Variáveis 'dependente' e 'preditora', nesta ordem respectivamente.}
    parser    : TAbstractParser;
    i, c      : integer;
    s_aux     : string;

  procedure cria_parser(var parser : TAbstractParser);
  var i : integer;
  begin
    {Adiciona os operadores que o parser reconhecerá.}
    parser := TExpressionParser.Create();
    With parser.AritOpers do
      begin
      Add('+');
      Add('-');
{      Add('*');}
      Add('/');
      Add('#');
{      Add('**');}
      Add('##');
      end;
    {Adiciona as funções que o parser reconhecerá.}
    for i := 0 to wsAvaliadorDeExpressoes.TamTab_Fun-1 do
        Parser.Functions.Add(wsAvaliadorDeExpressoes.Tab_Fun[i]);
  end; {cria_parser}

  procedure cria_objetos;
  begin
    {locais}
    sts_Vars    := TStringList.Create;
    cria_parser(parser);
  end;

  procedure destroi_objetos;
  begin
    {locais}
    sts_vars.Free;
    parser.Free;
  end;

Begin {TwsNLinMod.Create}
  cria_objetos;

  FOutput := aOutput;
  FOnCreatedObject := aCreatedObject_Event;  

  DataSet := aDataSet;
  {Troca cada '*' por '#'. Ver comentários no cabeçalho desta Unit.}
  sModel := ChangeChar(asModel, '*', '#');
  sCondicao := asCondicao;
  iMaxIter := aiMaxIter;
  fEPS := afEPS;
  stsVarsGrupo := astsVarsGrupo;
  iNPar := asts_parametros.Count;
  vValoresIniciais := av_valores_iniciais;
  bitsOpcoes := abitsOpcoes;

  {Cria mS}
  mS := TwsSymmetric.Create(iNPar+1);

  {Cria a matriz mBeta}
  mBeta := TwsGeneral.Create(4, iNPar);
  mBeta.Name := 'Estatisticas';
  mBeta.MLab := 'Estimativas, erros padrões e intervalo de confiança (5%) assintóticos';
  mBeta.RowName[1] := 'Estimativas';
  mBeta.RowName[2] := 'Erro padrão';
  mBeta.RowName[3] := 'Extr. Inferior';
  mBeta.RowName[4] := 'Extr. Superior';
  for i := 1 to iNPar do
      mBeta.ColName[i] := asts_parametros[i-1];

  {criar, definir Avaliador !!!!!!!!!!!!!!!!!!!!!!!!!!!!}
  Eval := TAvaliator.Create;
  Eval.Reference:=False; {Sempre cria uma copia dos objetos retornados pelo avaliador.}
  Eval.Print:=True;

  {Inserir os parametros na tabela de simbolos.}
  for i := 1 to iNPar do
      Eval.TabVar.AddFloat(mBeta.ColName[i], 0);

  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {------------------ Análise do Modelo entrado em edModelo -------------------}
  {Separar o modelo em duas partes...}
  sDepName := sysUtilsEx.AllTrim(System.Copy(sModel, 1, System.Pos('~', sModel)-1)); {obs.: Maximo de 256 bytes.}
  sExprModel := sysUtilsEx.AllTrim(System.Copy(sModel, System.Pos('~', sModel)+1,Length(sModel)));
  sts_Vars.Add(sDepName);

  If sDepName = '' Then
  begin
    destroi_objetos;
    Raise Exception.Create('Variável dependente não definida no modelo');
  end;

  {Se a variável dependente for do tipo Factor então dá erro.}
  If DataSet.IsFactor(sDepName) Then
  begin
    destroi_objetos;
    Raise Exception.Create('Variável dependente do tipo Fator');
  end;
  {Obs.: Se for entrada uma expressão na variável dependente, por exemplo,
         'Raiz(y) ~ X + b0' não acusa erro nenhum, mesmo que y seja do tipo Fator.
         Neste caso e' usado o indice do nivel para achar a raiz.}

  parser.Text.Clear;
  parser.Text.Add(sExprModel);
  parser.Scan;

  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {----------------------------- Analizar o modelo ----------------------------}

  {Verifica se todos os parametros foram usados no Modelo.}
  For i := 0 to asts_parametros.Count - 1 do
    begin
      {Se o parametro nao foi usado entao dá mensagem de erro.}
      If parser.Variables.IndexOf(asts_parametros[i]) = -1 Then
      begin
        destroi_objetos;
        Raise Exception.Create('Parametro ' + asts_parametros[i] +
                               ' não utilizado no modelo');
      end;
    end;

  {Deleta os parametros de parser.Variables}
  For i := 0 to asts_parametros.Count - 1 do
  begin
    parser.Variables.Delete(parser.Variables.IndexOf(asts_parametros[i]));
  end;

  {------------------------ Tratamento das variaveis --------------------------}

  c := 0;
  For i := 0 to parser.Variables.Count-1 do
  begin
    inc(c);
    {Se a variável não existir no DataSet...}
    If DataSet.Struct.IndexOf(parser.Variables[i]) = -1 Then
    begin
      s_aux := parser.Variables[i];
      destroi_objetos;
      Raise Exception.Create('Variável '+ s_aux +
                             ' não existe no conjunto de dados');
    end;
    {Se tiver mais de uma variável no modelo então dá erro.}
    If c > 1 Then
    begin
      destroi_objetos;
      Raise Exception.Create('Existe mais de uma variável no modelo');
    end;
    {Se a variável independente for igual à variável dependente então dá mensagem de erro.}
    If parser.Variables[i] = sDepName Then
    begin
      destroi_objetos;
      Raise Exception.Create('Variável independente igual à variável dependente');
    end;
    {Se a variável não for numérica então dá uma mensagem de erro.}
    If DataSet.isFactor(parser.Variables[i]) Then
    begin
      s_aux := parser.Variables[i];
      destroi_objetos;
      Raise Exception.Create('A variável ' + s_aux +
                             ' não é do tipo numérica');
    end;
  end;

  {Se não tiver nenhuma variável no modelo então dá erro.}
  If c = 0 Then
  begin
    destroi_objetos;
    Raise Exception.Create('Ausência da variável independente no modelo');
  end;

  sPredName := parser.Variables[0];
  {Tudo Ok, então adiciona a variável preditora à sts_Vars.}
  sts_Vars.Add(sPredName);

  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {------------------- Gera conjuntos de dados (ModelFrame) -------------------}

  {Gera os conjuntos (lstDataSets) dos quais serão feitas as analises.}
  try
    lstDataSets := ModelFrame(sts_Vars, stsVarsGrupo, sCondicao, DataSet);
  except
     on e: Exception do
        begin
        destroi_objetos;
        raise;
        end;
  end;
  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {Cria e inicializa vPred, e adiciona o mesmo à Tabela De Simbolos(TabVar).
   Se eu não fizer isso, então 'Eval.Epression := sExprModel' vai dizer que a variável
   preditora não existe.}
  vPred := TwsDFVec.Create(0);
  Eval.TabVar.AddVector(sPredName, vPred);

  Try
    Eval.Expression := sExprModel;
  Except
    On E : Exception Do
       begin
       destroi_objetos;
       {Não preciso destruir vPred pois o próprio Eval já destrói ele.}
       Raise Exception.Create('Problemas na avaliação do modelo '+
                              '<' + sExprModel + '>.'#13 + E.Message);
       end;
  End;

End; {TwsNLinMod.Create}

(****************************************************************************
ok1*)
Destructor TwsNLinMod.Destroy;
Begin
  mBeta.Free;
  Eval.Free;
  mS.Free;
  vDep.Free;
  lstDataSets.free; // <<<<<
End; {TwsNLinMod.Destroy}

(****************************************************************************
Atualiza os parametros na TabVar.
Libera vDep anterior e cria um novo. Calcula o modelo (vDep).
Calcula os residuos (cria vResid) e fSSRes.
Atribui vResid à última linha de mX.
ok
*)
Procedure TwsNLinMod.EvalModel;
var i : integer;
    res : TEvalResult;
Begin
  {Atualiza parametros na TabVar}
  for i := 1 to mBeta.NCols do
      Eval.TabVar.SetFloatValue(mBeta.ColName[i], mBeta[1,i]);

  vDep.Free;
  vDep := Nil;

  {Calcula Modelo.}
  res := Eval.Evaluate;
  vDep := res.AsVector;

  {Calcula resíduo e soma dos quadrados dos resíduos.}
  vResid := TwsDFVec.Create(iNObs);
  fSSRes := 0;
  for i := 1 to iNObs do
      begin
      vResid[i] := DSFrame[i,1] - vDep[i];
      fSSRes := fSSRes + Sqr(vResid[i]);
      end;
  mX.Row[mX.NRows] := vResid;

End; {TwsNLinMod.EvalModel}

(*****************************************************************************)
{ faz o produto de uma matriz pela sua transposta.
ver ???}
Procedure TwsNLinMod.TranspMul(A: TwsGeneral);
var
  i,j,k: Integer;
  v: double;
begin
  for i := 1 to A.NRows do
    begin
    for j := 1 to i do
      begin
      v := 0;
      for k := 1 to A.NCols do
        v := v + A[i, k]*A[j, k];
      mS[i,j] := v;
      end
    end;
end; { TwsNLinMod.TranspMul }

(*****************************************************************************)
{ver ???}
Procedure TwsNLinMod.EstimateTable;
var v : TwsVec;
    i : integer;
    f : double;
    erro : word;
Begin
  f := FInv(0.05, 1, iNObs-iNPar, 1.0E-10, True, Erro);
  for i := 1 to iNPar do
    begin
    mBeta[2,i] := sqrt(mCov[i,i]);
    mBeta[3,i] := mBeta[1,i] - sqrt(f * mCov[i,i]);
    mBeta[4,i] := mBeta[1,i] + sqrt(f * mCov[i,i]);
    end;

  FOutput.Add(mBeta); 
End;

(*****************************************************************************)
{ver ???}
Procedure TwsNLinMod.AnovaTable;
var m : TwsGeneral;
Begin
  m := TwsGeneral.Create(3,3);
  m.Name := 'Fontes';
  m.MLab := 'Quadro da análise da variação';
  m.RowName[1] := 'Regressão';
  m.RowName[2] := 'Resíduo';
  m.RowName[3] := 'Total';
  m.ColName[1] := 'GL';
  m.ColName[2] := 'SQ';
  m.ColName[3] := 'QM';
  m[1,1] := iNPar;
  m[1,2] := fSSTot - fSSRes;
  m[1,3] := m[1,2] / iNPar;
  m[2,1] := iNObs - iNPar;
  m[2,2] := fSSRes;
  m[2,3] := fSSRes / m[2,1];
  m[3,1] := iNObs;
  m[3,2] := fSSTot;
  m[3,3] := wscMissValue;
  FOutput.Add(m); 
  m.Free;
End; {TwsNLinMod.AnovaTable}

(****************************************************************************
ver ???*)
procedure TwsNLinMod.GetCov;
var j : integer;
Begin
  {Cria mCov.}
  mCov := TwsSymmetric(mS.SubSymMat(1, iNPar));
  mCov.Name := 'Covar';
  mCov.MLab := 'Covariâncias assintóticas entre as estimativas';
  for j := 1 to iNPar do
      begin
      mCov.RowName[j] := mBeta.ColName[j];
      mCov.ColName[j] := mBeta.ColName[j];
      end;
  mCov.ByScalar(fSSRes/(iNObs-iNPar),opProd,False,False);
End; {TwsNLinMod.GetCov}

(****************************************************************************
ver ???*)
Procedure TwsNLinMod.GetCorr;
var i, j : integer;
Begin
  mCorr := TwsSymmetric.Create(mCov.NCols);
  mCorr.Name := 'Correl';
  mCorr.MLab := 'Correlações assintóticas entre as estimativas';
  for j := 1 to iNPar do
    begin
    mCorr.RowName[j] := mBeta.ColName[j];
    mCorr.ColName[j] := mBeta.ColName[j];
    end;

  for i := 1 to mCov.NCols do
    for j := 1 to i do
      if j <> i then
        mCorr[j,i] := mCov[i,j] / Sqrt( mCov[i,i] * mCov[j,j] );
  for i := 1 to mCorr.NCols do
    mCorr[i,i] := 1;
End; {TwsNLinMod.GetCorr}

(****************************************************************************
ok
Entrada:
n_saida - Número que será adicionado ao final do nome dos conjuntos gerados.
          É o índice de lstDataSets para o qual DSFrame está apontando.

Função:
   Adiciona as colunas 'Ajustado' e 'Residuo' em DSFrame.
   Chama AnovaTable; GetCov; GetCorr e EstimateTable;
   Imprime, Armazena ou Libera: DSFrame, mIter, mCov e mCorr.
*)
Procedure TwsNLinMod.GeraSaidas(n_saida : integer);
var
  j : integer;
Begin
  {Adiciona duas colunas em DSFRame.}
  DSFrame.Struct.AddCol(TwsNumeric.Create('Ajustado', 'Valores ajustados'));
  DSFrame.Struct.AddCol(TwsNumeric.Create('Residuo', 'Resíduos do ajustamento'));
  for j := 1 to DSFrame.NRows do
    begin
    DSFRame[j,3] := vDep[j];
    DSFRame[j,4] := vResid[j];
    end;

  {================ Impressões, Armazenamentos/liberações ================}

  {--------------------- Conjunto de Dados (DSFrame) ---------------------}

  if BitsOpcoes[cIDSFrame] then
     FOutput.Add(DSFrame);

  if BitsOpcoes[cADSFrame] then
     FOnCreatedObject(self, DSFrame.Copy);

  {Obtém quadro da analise da variacao, matriz de Covariancias e correlacoes.}
  AnovaTable;
  GetCov;
  GetCorr;
  EstimateTable;

  {------------------------ Matriz das Iterações ------------------------}

  if BitsOpcoes[cIIter] then
     FOutput.Add(mIter);

  if BitsOpcoes[cAIter] then
     begin
     mIter.Name := mIter.Name + intToStr(n_saida);
     FOnCreatedObject(self, mIter);
     end
  else
    begin
    mIter.Free;
    mIter := Nil;
    end;

  {----------------------- Matriz das Covariancias ----------------------}

  if BitsOpcoes[cIMatCov] then
     FOutput.Add(mCov);

  if BitsOpcoes[cAMatCov] then
     begin
     mCov.Name := mCov.Name + intToStr(n_saida);
     FOnCreatedObject(self, mCov);
     end
  else
    begin
    mCov.Free;
    mCov := Nil;
    end;

  {--------- Matriz das Correlações entre estimativas dos parametros ----}

  if BitsOpcoes[cICorr] then
     FOutput.Add(mCorr);

  if BitsOpcoes[cACorr] then
     begin
     mCorr.Name := mCorr.Name + intToStr(n_saida);
     FOnCreatedObject(self, mCorr);
     end
  else
    begin
    mCorr.Free;
    mCorr := Nil;
    end;

  FOutput.BeginText;
  Case CondConverg of
    0 : FOutput.Warning('Convergência alcançada');
    1 : FOutput.Warning('Não houve melhoria após 10 sub-iterações');
    2 : FOutput.Warning('Número de iterações insuficientes para convergência');
  End;
  FOutput.EndText;

End; {TwsNLinMod.GeraSaidas}

(****************************************************************************
*)
Constructor TwsNLinDeriv.Create(aDataSet:             TwsDataSet;
                                astsVarsGrupo:        Tstrings;
                                asts_parametros:      TStrings;
                                av_valores_iniciais:  TwsVec;
                                astsDeriv:            TStrings;
                                const asModel:        String;
                                const asCondicao:     String;
                                aiMaxIter:            integer;
                                afEPS:                Double;
                                aiMetodo:             integer;
                                abitsOpcoes:          TBits;
                                aOutput:              TwsBXML_Output;
                                aCreatedObject_Event: TwsCreatedObject_Event);

var
  eval_temp : TAvaliator;
  i, j      : Integer;
  st        : string;
Begin
  {Inicializações.}
  eval_temp := Nil;

  Inherited Create(aDataSet, astsVarsGrupo, asts_parametros, av_valores_iniciais,
                   asModel, asCondicao, aiMaxIter, afEPS, abitsOpcoes, aOutput,
                   aCreatedObject_Event);

  iMetodo := aiMetodo;
  stsDeriv := TStringList.Create;
  for i := 0 to astsDeriv.Count - 1 do     {Adiciona à stsDeriv, trocando cada '*' por '#'}
      stsDeriv.Add(ChangeChar(astsDeriv[i], '*', '#'));

  {Criar uma Lista de Uevals para cada expressao de derivada.}
  For i := 0 to stsDeriv.Count - 1 do
      begin
      eval_temp := TAvaliator.Create;
      eval_temp.Reference := False;
      eval_temp.Print := True;
      eval_temp.TabVar := Eval.TabVar;
      Try
        eval_temp.Expression := stsDeriv[i];
      Except
        On E : Exception Do
           begin
           {libera os objetos da lstEvalDeriv}
           {ver com adriano ...}
           Raise Exception.Create('Problemas na avaliação da derivada '+
                                  '<' + stsDeriv[i] + '>.'#13 + E.Message);
           end;
      End;
      stsDeriv.Objects[i] := eval_temp;
      end; {for}

  {-------------------- Imprime cabeçalho da análise --------------------------}

  FOutput.BeginText;
  if DataSet.MLab <> '' then
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name+' - '+DataSet.MLab)
  else
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name);
  if asCondicao <> '' then
    FOutput.WritePropValue('Filtro:', asCondicao);
  if astsVarsGrupo.Count>0 then
    begin
    st:='';
    for i:=0 to astsVarsGrupo.Count-1 do
      st:=st+astsVarsGrupo[i]+', ';
    Delete(st,Length(st)-1,2);
    FOutput.WritePropValue('Variáveis de Grupo:', st);
    end;
  FOutput.WritePropValue('Modelo:', asModel);
  for j := 0 to astsDeriv.Count - 1 do
    FOutput.WritePropValue('Der('+mBeta.ColName[j+1]+'): ', astsDeriv[j]);
  FOutput.WritePropValue('Método de ajustamento:',cNomesMetodos[iMetodo]);
  FOutput.WritePropValue('Número Máximo de Iterações:', IntToStr(aiMaxIter));
  FOutput.WritePropValue('Precisão Para Convergência:', FloatToStr(afEPS));
  FOutput.EndText;

  {Faz a análise para cada DataSet(DSFrame) gerado por ModelFrame.}
  LoopDSFrame;
End; {TwsNLinDeriv.Create}

(****************************************************************************
ok1*)
Destructor TwsNLinDeriv.Destroy;
Begin
  Inherited Destroy;

  {Libera stsDeriv e seus objetos associados.}
  if stsDeriv <> Nil Then
     begin
     While stsDeriv.Count <> 0 do
           begin
           TAvaliator(stsDeriv.objects[0]).free;
           stsDeriv.delete(0);
           end;
     stsDeriv.free;
     end;

End; {TwsNLinDeriv.Destroy}

(**************************************************************************
ok
Para cada DataSet(DSFrame) em lstDataSets faz:
   Faz DSFrame apontar para o DataSet correto.
   Seta iNObs.
   Inicializa mBeta com os valores iniciais.
   Deleta vPred anterior de Eval.TabVar, cria um novo vPred e adiciona em Eval.Tabvar
   Cria mX (a matriz das derivadas).
   Chama EvalModel.
   Chama o método Fit correspondente a iMetodo.
   Chama GeraSaidas(i).
   Libera mX.
*)
Procedure TwsNLinDeriv.LoopDSFrame;
var
  i,j : integer;
begin
  {---------------------- Laço para cada lstDataSets --------------------------}
  for i := 0 to lstDataSets.Count - 1 do
    begin
    DSFrame := lstDataSets[i];
    iNObs := DSFrame.NRows;

    {Inicializa mBeta com os valores iniciais.}
    for j := 1 to iNPar do
      mBeta[1,j] := vValoresIniciais[j];
    mX := TwsGeneral.Create(iNPar + 1, iNObs);

    {Deleta vPred anterior e cria um novo.}
    Eval.TabVar.DeleteVar(sPredName);
    fSSTot := 0;
    vPred := TwsDFVec.Create(iNObs);
    for j := 1 to iNObs do
      begin
      vPred[j] := DSFrame[j, 2];
      fSSTot := fSSTot + sqr(DSFrame[j, 1]);
      end;

    {Inserir vPred na tabela de símbolos com nome sPredName.}
    Eval.TabVar.AddVector(sPredName, vPred);

    // Inicializa matriz de informacoes dos passos e chama o metodo adequado
    Case iMetodo of
      cGauss:
        begin
        mIter := TwsGeneral.Create(0,iNPar+1);
        with mIter do
          begin
          Name := 'GaussNewt';
          MLab := 'Resultados do Processo Iterativo';
          ColName[1] := 'SQRes';
          for j := 1 to iNPar do
            ColName[j+1] := mBeta.ColName[j];
          end;
        GaussNewtonFit;
        end;
      cHart :
        begin
        mIter := TwsGeneral.Create(0,iNPar+1);
        with mIter do
          begin
          Name := 'Hartley';
          MLab := 'Resultados do Processo Iterativo';
          ColName[1] := 'SQRes';
          for j := 1 to iNPar do
            ColName[j+1] := mBeta.ColName[j];
          end;
        HartleyFit;
        end;
      cMarq :
        begin
        mIter := TwsGeneral.Create(0,iNPar+2);
        with mIter do
          begin
          Name := 'Marquardt';
          MLab := 'Resultados do Processo Iterativo';
          ColName[1] := 'Lambda';
          ColName[2] := 'SQRes';
          for j := 1 to iNPar do
            ColName[j+2] := mBeta.ColName[j];
          end;
        MarquardtFit;
        end;
      end; //case

    GeraSaidas(i);
    mX.Free;
    mX := Nil;
    end; {for}
end; {TwsNLinDeriv.LoopDSFrame}


(****************************************************************************
ok
Calcula as expressões das derivadas e atualiza as linhas das derivadas
na matriz mX.
*)
Procedure TwsNLinDeriv.EvalDeriv;
var i, j : integer;
    res  : TEvalResult;
    v    : TwsVec;
Begin
  for i := 0 to stsDeriv.Count - 1 do
      Begin
      res := TAvaliator(stsDeriv.Objects[i]).Evaluate;
      case res.ResultType of
           {Se o resultado da avaliação da derivada for um vetor então eu
            simplesmente atribuo esse vetor à linha i+1 de mX.}
           vtVector:
             {Obs.: O vetor anterior é automaticamente liberado quando eu faço a atribuição.}
             mX.Row[i+1] := res.AsVector;

           {Se o resultado da avaliação da derivada for um número então eu crio
            um vetor com tamanho mX.NCols e inicializo todas as posições dele com
            esse valor que foi retornado e aí atribuo esse vetor à linha i+1 de mX.}
           vtNumeric:
             begin
             v := TwsDFVec.Create(mX.NCols);
             for j := 1 to v.Len do
                 v[j] := res.AsFloat;
             mX.Row[i+1] := v; {mX.Row[i+1] anterior é automaticamente liberada.}
             end;
      end; {case}
      End; {for}
End; {TwsNLinDeriv.EvalDeriv}

(****************************************************************************
ok1
*)
Procedure TwsNLinDeriv.GaussNewtonFit;
var iter,                                 // Número de iterações
    j      : Integer;
    EPS    : double;
    v_iter,                               // Informações sobre uma iteração
    v_tol  : TwsVec;                      // Tolerancia para o sweep
    v      : TwsLIVec;                    // Controla colunas operadas pelo sweep
Begin

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid à última linha de mX.}
  EvalModel;

  {Inicializações, criações.}
  v_iter:=Nil; v_tol:=Nil; v:=Nil;
  v_tol := TwsDFVec.Create(mX.NRows);
  v := TwsLIVec.Create(mX.NRows);

  {Adiciona uma linha em mIter.}
  v_iter := TwsDFVec.Create(iNPar+1);
  v_Iter.Name := 'Passo_0';
  v_iter[1] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
  mIter.MAdd(v_iter);

  {----------------------------- Processo Iterativo ---------------------------}
  EPS := 1;
  CondConverg := 0;
  for iter:=1 to iMaxIter do
    begin
    if EPS <= fEPS then Break;       // Se convergiu cai fora do processo iterativo
    {Calcula as derivadas.}
    EvalDeriv;
    {Atualiza a matriz mS do sistema de equações.}
    TranspMul(mX);{XX'}
    for j := 1 to v_tol.Len do
      begin
      v_tol[j] := 1.0E-9;         //mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;
    for j := 1 to mS.NCols-1 do
      mS.RevSweep(j, v_tol[j], v);
    for j := 1 to iNPar do
      mBeta[1,j] := mBeta[1,j] + mS[mS.NRows,j];

    {determina o maior valor absoluto da ultima linha de mS.}
    EPS := Abs(mS[mS.NRows, 1]);
    for j := 2 to iNPar do
      if EPS < Abs(mS[mS.NRows, j]) then
        EPS := Abs(mS[mS.NRows, j]);

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid à última linha de mX.}
    EvalModel;

    {Adiciona uma linha em mIter.}
    v_iter := TwsDFVec.Create(iNPar+1);
    v_Iter.Name := 'Passo_'+IntToStr(iter);
    v_iter[1] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
    mIter.MAdd(v_iter);
  end;
  if (iter > iMaxIter) then
    CondConverg := 2; // Numero de iteracoes insuficientes

  {Liberações.}
  v_tol.Free;
  v.Free;
End; {GaussNewtomFit}

(****************************************************************************
ok1
*)
Procedure TwsNLinDeriv.HartleyFit;
var
  iter,                                       // Contador para o número de iterações
  subit,                                      // Contador para o número de sub-iterações
  j : Integer;
  erro : word;
  last_sse, EPS, lambda : double;              // Auxiliares para o processo iterativo
  v: TwsLIVec;                                // Controla colunas operadas pelo sweep
  v_iter,                                     // Linha com informações sobre uma iteração
  v_tol: TwsVec;                              // Tolerancias para aplicacao do sweep
  m_old_beta : TwsGeneral;                    // Valores dos parametros no passo anterior
Begin
  {Inicializações, criações.}
  v_iter:=Nil; v:=Nil; v_tol:=Nil; m_old_beta:=Nil;
  v := TwsLIVec.Create(mX.NRows);
  v_tol := TwsDFVec.Create(mX.NRows);
  lambda := 1e-8;

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid à última linha de mX.}
  EvalModel;

  {Adiciona uma linha em mIter.}
  v_iter := TwsDFVec.Create(iNPar+1);
  v_Iter.Name:='Passo_0_0';
  v_iter[1] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
  mIter.MAdd(v_iter);

  {----------------------------- Processo Iterativo ---------------------------}
  EPS := 1;
  CondConverg := 0;
  for iter:=1 to iMaxIter do
    begin
    if EPS <= fEPS then Break;            // Se convergiu cai fora do processo iterativo
    {Calcula as derivadas.}
    EvalDeriv;
    last_sse := fSSRes;
    {Atualiza a matriz mS do sistema de equações.}
    TranspMul(mX);{XX'}
    for j := 1 to iNPar do
      mS[j,j] := mS[j,j] + lambda * mS[j,j];
    for j := 1 to v_tol.Len do
      begin
      v_tol[j] := 1.0E-9;//mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;
    for j := 1 to mS.NCols-1 do
        mS.RevSweep(j, v_tol[j], v);
    mBeta.Copy(mtGeneral,TwsMatrix(m_old_beta));
    for j := 1 to iNPar do
      mBeta[1,j] := mBeta[1,j] + mS[mS.NRows,j];
    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid à última linha de mX.}
    EvalModel;
    EPS := Abs(last_sse - fSSRes)/(fSSRes + 1E-6);
    {Adiciona uma linha em mIter.}
    v_iter := TwsDFVec.Create(iNPar+1);
    v_Iter.Name:='Passo_'+IntToStr(Iter)+'_0';
    v_iter[1] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
    mIter.MAdd(v_iter);
    {Hartley subiterations.}
    for subit:=1 to 10 do
      begin
      if (fSSRes <= last_sse) then Break;                // Se convergiu, cai fora
      for j := 1 to mS.NCols do
        mS[mS.NRows,j] := mS[mS.NRows,j] * 0.5;
      for j := 1 to iNPar do
        mBeta[1,j] := m_old_beta[1,j] + mS[mS.NRows,j];

      {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
      atribui vResid à última linha de mX.}
      EvalModel;

      {Adiciona uma linha em mIter. Referente à sub-iteração.}
      v_iter := TwsDFVec.Create(iNPar+1);
      v_iter[1] := fSSRes;
      for j := 1 to iNPar do
        v_iter[j+1] := mBeta[1,j];
      v_Iter.Name:='Passo_'+IntToStr(Iter)+'_'+IntToStr(Subit);
      mIter.MAdd(v_iter);
      end; {subit}

    {Se esgotou a última sub-iteração e ainda não convergiu então é erro.}
    if (subit > 10) then
      begin
      Eps:=0;
      CondConverg := 1                    // Subiteracoes nao melhoraram o proceso
      end;
    m_old_beta.Free;
    m_old_beta := Nil;
    end;  // for iter
  if (iter > iMaxIter) then CondConverg := 2;           // Iteracoes insuficientes
  {Liberações.}
  v_tol.Free;
  v.Free;
End; {HartleyFit}

(****************************************************************************
ok1
*)
Procedure TwsNLinDeriv.MarquardtFit;
var iter,                            // Contador para o número de iterações
    subit,                           // Contador para o número de sub-iterações
    j : Integer;
    erro : word;
    last_sse, EPS, lambda : double;  // valores intermediarios para o processo iterativo
    v: TwsLIVec;                    // Indicadores de linhas operadas pelo sweep
    v_iter,                         // Uma linha com informações sobre uma iteração
    v_tol: TwsVec;                  // Tolerancias para o sweep
    m_old_beta : TwsGeneral;        // Mantém uma cópia de mBeta
Begin
  {Inicializações, criações.}
  v_iter:=Nil; v:=Nil; v_tol:=Nil; m_old_beta:=Nil;
  v := TwsLIVec.Create(mX.NRows);
  v_tol := TwsDFVec.Create(mX.NRows);

  {Avalia o modelo, atualiza parametros, obtem residuos, etc.}
  EvalModel;

  lambda := 1e-8;
  // Insere linha correspondente ao passo 0 do processo iterativo
  v_iter := TwsDFVec.Create(iNPar+2);
  v_Iter.Name:='Passo_0_0';
  v_iter[1] := lambda;
  v_iter[2] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
  mIter.MAdd(v_iter);

  {----------------------------- Processo Iterativo ---------------------------}
  EPS := 1;
  CondConverg:=0;
  for iter:=1 to iMaxIter do
    begin
    if eps<=FEps then Break;              // Se convergiu, cai fora

    EvalDeriv;                             // Calcula as derivadas
    last_sse := fSSRes;                    // Ultimo valor da SQResiduo
    TranspMul(mX);{XX'}
    for j := 1 to iNPar do                 // X'X com a diagonal inflada
      mS[j,j] := mS[j,j] + lambda*mS[j,j];

    for j := 1 to v_tol.Len do            // Prepara para aplicar o sweep
      begin
      v_tol[j] := 1e-9;        //mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;
    for j := 1 to mS.NCols-1 do           // Aplica o sweep. Solucao na ultima linha
      mS.RevSweep(j, v_tol[j], v);

    mBeta.Copy(mtGeneral,TwsMatrix(m_old_beta));  // Copia estimativas antigas
    for j := 1 to iNPar do                      // Aplica correcao
      mBeta[1,j] := mBeta[1,j] + mS[mS.NRows,j];

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid à última linha de mX.}
    EvalModel;

    if fSSRes < last_sse then lambda := lambda / 10;
    EPS := Abs(last_sse - fSSRes)/(fSSRes + 1E-6);  // Criterio de convergencia

    {Adiciona uma linha em mIter.}
    v_iter := TwsDFVec.Create(iNPar+2);
    v_Iter.Name:='Passo_'+IntToStr(Iter)+'_0';
    v_iter[1] := lambda; v_iter[2] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
    mIter.MAdd(v_iter);

    for subit := 1 to 10 do
      begin
      if (fSSRes <= last_sse) then Break;
      lambda := lambda*10;
      TranspMul(mX);{XX'}
      for j := 1 to iNPar do
        mS[j,j] := mS[j,j] + lambda*mS[j,j];

      for j := 1 to v_tol.Len do
        begin
        v_tol[j] := 1.0E-9; //mS[j,j] * 1.0e-9
        v[j] := 1;
        end;
      for j := 1 to mS.NCols-1 do
        mS.RevSweep(j, v_tol[j], v);

      for j := 1 to iNPar do
        mBeta[1,j] := m_old_beta[1,j] + mS[mS.NRows,j];

      {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
      atribui vResid à última linha de mX.}
      EvalModel;

      {Adiciona uma linha em mIter. Referente à sub-iteração.}
      v_iter := TwsDFVec.Create(iNPar+2);
      v_Iter.Name:='Passo_'+IntToStr(Iter)+'_'+IntToStr(Subit);
      v_iter[1] := lambda;
      v_iter[2] := fSSRes;
      for j := 1 to iNPar do
        v_iter[j+2] := mBeta[1,j];
      mIter.MAdd(v_iter);
      end; // for para cada sub-iteracao

    if subit>10 then
      begin
      CondConverg:=1;
      Eps:=0;
      end;
    m_old_beta.Free;
    m_old_beta := Nil;
    end; // for para cada iteracao

  if iter > iMaxIter then CondConverg:=2;
  v_tol.Free;
  v.Free;
End; {MarquardtFit}


(****************************************************************************
ok.*)
Constructor TwsDUD.Create(aDataSet:             TwsDataSet;
                          astsVarsGrupo:        TStrings;
                          asts_parametros:      TStrings;
                          av_valores_iniciais:  TwsVec;
                          const asModel:        String;
                          const asCondicao:     String;
                          aiMaxIter:            integer;
                          afEPS:                double;
                          abitsOpcoes:          TBits;
                          aOutput:              TwsBXML_Output;
                          aCreatedObject_Event: TwsCreatedObject_Event);

var
  st: string;
  i : Integer; 
Begin
  Inherited Create(aDataSet,astsVarsGrupo,asts_parametros,av_valores_iniciais,
                   asModel,asCondicao,aiMaxIter,afEPS,abitsOpcoes,aOutput,
                   aCreatedObject_Event);

  {-------------------- Imprime cabeçalho da análise --------------------------}
  FOutput.BeginText;
  if DataSet.MLab <> '' then
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name+' - '+DataSet.MLab)
  else
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name);
  if asCondicao <> '' then
    FOutput.WritePropValue('Filtro:', asCondicao);
  if astsVarsGrupo.Count>0 then
    begin
    st:='';
    for i:=0 to astsVarsGrupo.Count-1 do
      st:=st+astsVarsGrupo[i]+', ';
    Delete(st,Length(st)-1,2);
    FOutput.WritePropValue('Variáveis de Grupo:', st);
    end;
  FOutput.WritePropValue('Modelo:', asModel);
  FOutput.WritePropValue('Método de ajustamento:', cNomesMetodos[cDUD]);
  FOutput.WritePropValue('Número Máximo de Iterações:', IntToStr(aiMaxIter));
  FOutput.WritePropValue('Precisão Para Convergência:', FloatToStr(afEPS));
  FOutput.EndText;
  LoopDSFrame;
End;{TwsDUD.Create}


Destructor TwsDUD.Destroy;
begin
  DP.Free;
  inherited Destroy
end;

procedure TwsDUD.GetCov;
var
  j : integer;
  Err: Word;
  mAux: TwsSymmetric;
Begin
  {Cria mCov.}
  mAux := TwsSymmetric(mS.SubSymMat(1, iNPar));
  mCov:=TwsSymmetric(mAux.TranspMul10(DP,Err));
  mAux.Free;
  mCov.Name := 'Covar';
  mCov.MLab := 'Matriz de Covariâncias das estimativas';
  for j := 1 to iNPar do
      begin
      mCov.RowName[j] := mBeta.ColName[j];
      mCov.ColName[j] := mBeta.ColName[j];
      end;
  mCov.ByScalar(fSSRes/(iNObs-iNPar),opProd,False,False);
End; {TwsNLinMod.GetCov}

(****************************************************************************
ok1
*)
Procedure TwsDUD.DUDFit;
var m_old_beta, BB, FF: TwsGeneral;
    i, j, iter, subit : integer;
    erro : word;
    eps, old_fSSRes : double;
    v_iter, {Uma linha com informações sobre uma iteração. Não deverá ser destruído.}
    v_sss, v_delta, old_vDep, v_tol: TwsVec;
    v_ind, v: TwsLIVec;
Begin
  // Cria estruturas de armazenamento temporario
  BB := TwsGeneral.Create(iNPar, iNPar);
  BB.Name := 'BB';
  DP := TwsGeneral.Create(iNPar, iNPar);
  DP.Name := 'DP';
  FF :=  TwsGeneral.Create(iNObs, iNPar);
  FF.Name := 'FF';
  v_delta := TwsDFVec.Create(iNPar);
  v_delta.Name := 'v_delta';
  v_sss := TwsDFVec.Create(iNPar);
  v_sss.Name := 'v_sss';
  v_tol := TwsDFVec.Create(mX.NRows);
  v_tol.Name := 'v_tol';
  v := TwsLIVec.Create(mX.NRows);
  v.Name := 'v';

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid à última linha de mX.}
  EvalModel;


  for i := 1 to iNPar do
    begin
    for j := 1 to iNPar do
        mBeta[1,j] := vValoresIniciais[j];
    mBeta[1,i] := vValoresIniciais[i] * 1.01;

    for j := 1 to iNPar do
      BB[j,i] := mBeta[1,j];

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid à última linha de mX.}
    EvalModel;

    v_sss[i] := fSSRes;
    for j := 1 to iNObs do
        FF[j,i] := vDep[j];
    end;

  v_ind := v_sss.QuickIndx(false);
  v_ind.Name := 'v_ind';

  FF.SortCol(v_ind);
  BB.SortCol(v_ind);

  {----- Prepare for iterations -----}
  {incluí este for.}
  for j := 1 to iNPar do
    mBeta[1,j] := vValoresIniciais[j];

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid à última linha de mX.}
  EvalModel;

  eps := 1;

  {Cria a primeira linha de mIter.}
  v_iter := TwsDFVec.Create(iNPar+2);
  v_Iter.name:='Passo_0_0';
  v_iter[1] := 0;
  v_iter[2] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
  mIter.MAdd(v_iter);

  // Inicia o processo iterativo
  for iter := 1 to iMaxIter do
    begin
    if Eps <= fEps then Break;

    mBeta.Copy(mtGeneral,TwsMatrix(m_old_beta));

    old_fSSRes := fSSRes;
    old_vDep := vDep.Copy(1, vDep.Len);

    for i := 1 to mX.NRows-1 do
      for j := 1 to mX.NCols do
        mX[i,j] := FF[j,i] - vDep[j];

    TranspMul(mX);{XX'}

    for j := 1 to v_tol.Len do
      begin
      v_tol[j] := 1.0E-9;                //mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;

    for j := 1 to mS.NCols-1 do
        mS.RevSweep(j, v_tol[j], v);

    {DP := B - mBeta}
    for i := 1 to iNPar do
      for j := 1 to iNPar do
        DP[i,j] := BB[i,j] - mBeta[1,i];

    {Encontra o vetor de correção. Delta = DP * Alpha}
    for i := 1 to iNPar do
      begin
      v_delta[i] := 0;
      for j := 1 to iNPar do
        v_delta[i] := v_delta[i] + DP[i,j] * mS[mS.NRows,j];
      end;

    {B = B + DELTA}
    for j := 1 to iNPar do
      mBeta[1,j] := mBeta[1,j] + v_delta[j];

    eps:=-1;
    for j:=1 to iNPar do
      if eps<Abs(v_Delta[j]) then eps:=Abs(v_Delta[j]);

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid à última linha de mX.}
    EvalModel;

    {-------------------------- Sub-Iteração -----------------------------}
    {Adiciona uma linha em mIter. Referente à iteração.}
    v_iter := TwsDFVec.Create(iNPar+2);
    v_Iter.Name:='Passo_'+IntToStr(Iter)+'_0';
    v_iter[1] := EPS;
    v_iter[2] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
    mIter.MAdd(v_iter);

    for subit:=1 to 10 do
      begin
      if (fSSRes <= old_fSSRes) then Break;
      for j := 1 to iNPar do
        begin
        v_delta[j] := v_delta[j] * 0.5;
        mBeta[1,j] := m_old_beta[1,j] + v_delta[j];
        end;

      {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
      atribui vResid à última linha de mX.}
      EvalModel;

      {Adiciona uma linha em mIter. Referente à iteração.}
      v_iter := TwsDFVec.Create(iNPar+2);
      v_Iter.Name:='Passo_'+IntToStr(Iter)+'_'+IntToStr(Subit);
      v_iter[1] := EPS;
      v_iter[2] := fSSRes;
      for j := 1 to iNPar do
        v_iter[j+2] := mBeta[1,j];
      mIter.MAdd(v_iter);
      end; // for subit

    {BB}
    for i := 1 to iNPar do
      begin
      for j := 2 to iNPar do
        BB[i, j-1] := BB[i,j];
      BB[i,iNPar] := m_old_beta[1,i];
      end;

    {FF}
    for i := 1 to iNObs do
      begin
      for j := 2 to iNPar do
        FF[i, j-1] := FF[i,j];
      FF[i,iNPar] := old_vDep[i];
      end;

    {v_sss}
    for i := 2 to iNPar do
        v_sss[i-1] := v_sss[i];
    v_sss[iNPar] := old_fSSRes;

    {Liberações.}
    m_old_beta.Free;
    m_old_beta := Nil;
    old_vDep.Free;
    old_vDep := Nil;
  end; // for iter

  BB.Free;
  FF.Free;
  v_delta.Free;
  v_sss.Free;
  v_tol.Free;
  v.Free;

  v_ind.Free;

End;{TwsDUD.DUDFit}

(****************************************************************************
ok1
*)
Procedure TwsDUD.LoopDSFrame;
var i, j, aux : integer;
begin

  {---------------------- Laço para cada lstDataSets --------------------------}
  for i := 0 to lstDataSets.Count - 1 do
      begin

      DSFrame := lstDataSets[i];
      iNObs := DSFrame.NRows;

      {Inicializa mBeta com os valores iniciais.}
      for j := 1 to iNPar do
        mBeta[1,j] := vValoresIniciais[j];

      mX := TwsGeneral.Create(iNPar+1, iNObs);

      {Libera vPred anterior e cria um novo.}
      Eval.TabVar.DeleteVar(sPredName);
      fSSTot := 0;
      vPred := TwsDFVec.Create(iNObs);
      for j := 1 to iNObs do
        begin
        vPred[j] := DSFrame[j, 2];
        fSSTot := fSSTot + sqr(DSFrame[j, 1]);
        end;

      {Inserir vPred na tabela de símbolos com nome sPredName.}
      Eval.TabVar.AddVector(sPredName, vPred);

      {Cria mIter com zero linhas.}
      mIter := TwsGeneral.Create(0,iNPar + 2);
      with mIter do
        begin
        Name := 'DUD';
        MLab := 'Resultados do Processo Iterativo';
        ColName[1] := 'EPS';
        ColName[2] := 'SQRes';
        for j := 1 to iNPar do
          ColName[j+2] := mBeta.ColName[j];
        end;

      DUDFit;

      GeraSaidas(i);

      mX.Free;
      mX := Nil;
      end; {for}
end; {TwsDUD.LoopDSFrame}


End.



(*
Observações:

FDataSet ---> Parte em memória e parte em disco.
Trabalhar com FDataSet quando o DataSet tiver que sofrer alteracoes.
FDataSet.AsDataSet.MAdd;
NovoDS := DS.AsDataSet;
.
.
.
DS := DS.UpDate;


  Obs.:
  Nome de coluna de matriz deve ser um ID válido.
  Nome de coluna (variável) de DataSet deve ser um ID válido.
  Nome de nível de fator não precisa ser um ID válido.


*)

