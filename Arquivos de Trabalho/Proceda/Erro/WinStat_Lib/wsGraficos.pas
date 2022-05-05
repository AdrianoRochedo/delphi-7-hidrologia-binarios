unit wsGraficos;

{
  FINALIDADES: Plotagem de gráficos que se utilizam da Classe TwsDataSet
  DATA DE CRIAÇÃO: .......................................... 01/07/1998
  AUTOR: ..................................... Adriano Rochedo Conceição
  AUTOR: ...................................... Anderson Priebe Ferrugem
  AUTOR: ...................................... José Claudio Vahl Júnior
  ÚLTIMA ALTERAÇÃO .......................................... 04/01/2000

  ROTINAS: .............................................................

  OBS.: Todas as funcões tem um parâmetro chamado Funcs (TFunctions) que
  indica a lista de funções matemáticas que serão calculadas

    NOME: ............................................... ws_ScatterPlot
    PARÂMETROS: ... (1) Conjunto de dados a ser utilizado - [TwsDataSet]
            (2) Nome da variável/coluna independente (eixo X) - [String]
            (3) Nome das variáveis dependentes (eixo Y) - [TStrings]


  NOME: .................................................. ws_DesignPlot
  AUTOR: ...................................... Anderson Priebe Ferrugem
    PARÂMETROS: ... (1) Conjunto de dados a ser utilizado - [TwsDataSet]
            (2) Nome da variável/coluna independente (eixo X) - [String]
                (3) Nome das variáveis dependentes (eixo Y) - [TStrings]
                         (4) Lista de funções matemáticas - [TFunctions]


   NOME: ............................................. ws_HistoGramPlot
   AUTOR: ...................................... Anderson Priebe Ferrugem
    PARÂMETROS: ... (1) Conjunto de dados a ser utilizado - [TwsDataSet]
            (2) Nome da variável/coluna independente (eixo X) - [String]




    COMENTÁRIOS: .......................................................
      Na versão de 16 bits (Delphi I), podemos plotar no máximo 16.000
      pontos para cada série (dados e funções).
      Não há verificação dos tipos de variáveis do Conjunto de Dados.
      O eixo X poderá ser igual a alguma variável do eixo Y.
      Tem que existir pelo menos uma variável X e uma Y.
      O 4. parâmetro é opcional, quando queremos isto usamos (NIL).


   NOME: ................................................ ws_VectorsPlot
   AUTOR: .................................... Adriano Rochedo Conceição
   PARÂMETROS: ........... (x) Array de vetores constituindo as abcissas
               .......... (y) Array de vetores constituindo as ordenadas
               (Tipo) Array de vetores constituindo o tipo de cada série

Todas as rotinas foram modificadas por José Claudio Vahl Júnior
}

interface
uses Forms,
     Contnrs,
     Classes,
     Chart,
     Form_Chart,
     wsMatrix,
     wsVec,
     wsAvaliadorDeExpressoes,
     wsBXML_Output;

Type
  TwsSerieType = (stLine, stArea, stPoint, stBar, stPie,
                  stFastLine, stArrows, stGant, stBubble, stCandle);

  // Armazena janelas que contém gráficos
  // Por default, quando instâncias desta classe são destruídas, os objetos contidos
  // por ela NÃO são destruídos automaticamente.
  TwsGraphics = class
  private
    FList: TObjectList;
    FActiveChart: TChart;
    FActiveForm: TfoChart;
    function getChart(i: Integer): TChart;
    function getForm(i: Integer): TfoChart;
    function getCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Cria um novo gráfico e retorna seu índice
    // Seta a propriedade Active para este novo gráfico
    function New(const Caption, Title: String): Integer;

    // Adiciona um gráfico e retorna sua posição na lista
    function Add(Grafico: TfoChart): Integer;

    // Retorna o formulário que contém o gráfico
    property Form[i: Integer] : TfoChart read getForm;

    // Retorna diretamente o gráfico
    property Chart[i: Integer] : TChart read getChart;

    // Retorna o formulário correntemente selecionado que contém o gráfico
    property ActiveForm: TfoChart read FActiveForm;

    // Retorna diretamente o gráfico correntemente selecionado
    property ActiveChart: TChart read FActiveChart;

    // Retorna o número de gráficos contidos na lista
    property Count : Integer read getCount;
  end;

  {Gráfico de funções}
  Function ws_FunctionPlot(
       Funcs : TFunctions    {Lista de expressões que serão calculadas}
    ): TfoChart;             {Janela que possui o gráfico}

  {Dispersão de pontos bivariado}
  Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     Title   : string = ''
  ): TfoChart; overload;  {Janela que possui o gráfico}

  {Dispersão de pontos univariado}
  Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Y       : TStrings;    {n Colunas, não existe ordenada}
     Title   : string = ''
  ): TfoChart; overload;    {Janela que possui o gráfico}

  Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     Funcs   : TFunctions;   {Lista de expressões que serão calculadas}
     Title   : string = 'Gráfico de Dispersão de Pontos Incluindo Funções'
  ): TfoChart; overload;  {Janela que possui o gráfico}

  {Dispersão de pontos com reta de tendência}
  Function ws_ScatterPlotTendency(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     x1, y1  : Double;      {indica o primeiro ponto de uma reta}
     x2, y2  : Double;      {indica o segundo ponto de uma reta}
     Funcs   : TFunctions;   {Lista de expressões que serão calculadas}
     Title   : string = 'Dispersão de Pontos'
  ): TfoChart;            {Janela que possui o gráfico}

  Function ws_ScatterPlotIndex(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String       {1 coluna}
  ): TfoChart;            {Janela que possui o gráfico}

  Function ws_DesignPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const Y : String;      {1 coluna}
     Factors : TStrings     {n Colunas}
  ): TfoChart;            {Janela que possui o gráfico}

  Function ws_HistogramPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String ;      {1 coluna}
     Colunas : TStrings      {n Colunas}
  ): TfoChart;            {Janela que possui o gráfico}

  Function ws_InteractionPlot(
     DS       : TwsDataSet;  {Origem dos dados}
     Const Y  : String;      {1 coluna, que sera plotado o eixo Y}
     Factors  : TStrings;    {n Colunas}
     out Means: TwsDataSet  // conjunto de dados com as medias
  ): TfoChart;            {Janela que possui o gráfico}

Function ws_BoxPlot(
     Frames      : TwsDataSets;      // Frames com os dados
     Variaveis   : TStrings;         // Variáveis para construcao das caixas
     out StatList: array of TwsGeneral
     ): TwsGraphics;                 // Lista com os gráficos criados

  Function ws_Lines(
    DSets   : TwsDataSets; {Origem dos Dados}
    Const X : String;     {1 coluna}
    Colunas : TStrings    {n Colunas}
  ): TfoChart;           {Janela que possui o grafico}

  Function ws_ColVert(DS: TwsDataSet; Colunas : TStrings; const Names: String=''): TfoChart;

  Function ws_PizzaPlot(DS: TwsDataSet; const Coluna: String; const Names: string=''): TfoChart;

  Function ws_VectorsPlot(
    const x        : array of TwsVec;     // vetores das abcissas
    const y        : array of TwsVec;     // vetores das ordenadas
    const Tipo     : array of TwsSerieType; // tipo de cada série
    var   Erro     : Integer              // 0 - Sem Erros, > 0 Algum Erro
  ): TfoChart;                          // Resultado: Janela que possui o gráfico

Function ws_MatrixPlot(
  x       : TwsVec;
  Y       : TwsGeneral;
  GType   : TwsSerieType;
  var Erro: Integer;
  w: TwsVec=nil;
  z: TwsVec=nil
  ): TfoChart;

Function ws_MatrixPlotTendency(
  x       : TwsVec;
  Y       : TwsGeneral;
  GType   : TwsSerieType;
  x1, y1  : Double;      {indica o primeiro ponto de uma reta}
  x2, y2  : Double;      {indica o segundo ponto de uma reta}
  var Erro: Integer;
  w: TwsVec=nil;
  z: TwsVec=nil
  ): TfoChart;

  {Dispersão de pontos bivariado}
  Function ws_IntervalPlot(
     DS   : TwsDataSet;  // Origem dos dados
     i,j,k: Integer         // coluna do valor inferior, superior e cor
  ): TfoChart;              // Janela que possui o gráfico

  // Gráfico de quantis
  function GraphQuantil(DS: TwsDataSet; const YStr: String): TfoChart;
  // Gráfico de simetria
  function SymmetryPlot(DS: TwsDataSet; const YStr: String): TfoChart;
  // Gráfico quantil-quantil
  function QQGraph(DS: TwsDataSet; const XStr, YStr: String): TfoChart;

implementation
uses teengine,
     Controls,
     SysUtils,
     Graphics,
     Lists,
     Series,
     TeeChart_BoxPlot,
     TeeChart_Intervals,
     wsGLib,
     wsConstTypes;

  {-------------------------  rotinas auxiliares  -------------------------}

Function SelectColor(i: Word; BlackWhite: Boolean = False): TColor;
var xx: Integer;
begin
  if BlackWhite then xx := 14 else xx := 12;
  inc(i, xx);
  case (i mod xx) of
    00: Result := clRed;
    01: Result := clGreen;
    02: Result := clBlue;
    03: Result := clMaroon;
    04: Result := clNavy;
    05: Result := clTeal; 
    06: Result := clFuchsia;
    07: Result := clYellow;
    08: Result := clAqua;
    09: Result := clPurple;
    10: Result := clOlive;
    11: Result := clLime;
    12: Result := clBlack;
    13: Result := clWhite;
    end;
end;
//function GetCaption(DS: TwsDataSet): String;
function GetCaption(DS: TwsDataSet): String;
Begin
  Result := DS.Name;
  If DS.MLab <> '' Then Result := Result + ' (' + DS.MLab + ')';
End;

function sAjusta(const x: double): String;
begin
  if x > 1 then
    Result := FloatToStrF(x, ffFixed, 3, 1)
  else
    Result := FloatToStrF(x, ffGeneral, 5, 3)
end;

{ TwsGraphics }

constructor TwsGraphics.Create;
begin
  inherited Create;
  FList := TObjectList.Create(False);
end;

destructor TwsGraphics.Destroy;
begin
  FList.Free;
  inherited;
end;

function TwsGraphics.Add(Grafico: TfoChart): Integer;
begin
  Result := FList.Add(Grafico);
  FActiveForm := Grafico;
end;

function TwsGraphics.getChart(i: Integer): TChart;
begin
  Result := getForm(i).Chart;
  FActiveChart := Result;
end;

function TwsGraphics.getForm(i: Integer): TfoChart;
begin
  Result := TfoChart(FList[i]);
  FActiveForm := Result;
end;

function TwsGraphics.New(const Caption, Title: String): Integer;
begin
  FActiveForm := TfoChart.Create(Caption);
  FActiveChart := FActiveForm.Chart;
  FActiveChart.Title.Text.Clear;
  FActiveChart.Title.Text.Add(Title);
  Result := Add(FActiveForm);
end;

function TwsGraphics.getCount: Integer;
begin
  Result := FList.Count;
end;

  {---------------------------  ws_FunctionPlot  -------------------------}

Function ws_FunctionPlot(
     Funcs : TFunctions      {Lista de expressões que serão calculadas}
  ): TfoChart;             {Janela que possui o gráfico}
var i      : Integer;
    NP     : Longint;       {Número de pontos usado para o cálculo de funções}
    FM     : Double;        {Fator de multiplicação para cálculo do progresso de funções}
    XValue : Double;        {Valor atual do eixo X}
    YValue : Double;        {Valor atual do eixo Y}
    L      : TLineSeries;  //variável que contém a série
Begin
  Result:= TfoChart.Create('Gráfico de Funções');
  Result.Chart.BottomAxis.LabelsAngle:=325;
  Result.Chart.Color:=clWhite;
  Result.Chart.View3D := False;
  If Funcs <> Nil Then
     For i := 0 to Funcs.Count - 1 do
       Begin
       L:= Result.Series.AddLineSerie(Funcs.Func[i].Eval.Expression, selectColor(i));

       XValue:= Funcs.Func[i].Li;

       FM:= 1 / Funcs.Func[i].Incr;
       NP:= Trunc((Funcs.Func[i].Ls - Funcs.Func[i].Li) * FM);

       While XValue < Funcs.Func[i].LS Do
         Begin
         if not IsMissValue(Funcs.GetValue(XValue, i)) then
           Begin
           YValue:= Funcs.GetValue(XValue, i);
           L.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
           End;
         XValue := XValue + Funcs.Func[i].Incr;
         End;

       L.ShowInLegend:= not(Funcs.Count = 1); //mostra na legenda somente quando tem mais de uma série
       End; {for Funcs.count}

  Result.Caption:= 'Funções definidas pelo usuário';
  Result.Chart.Title.Text.Clear;
  for i:=0 to Funcs.Count-1 do
    Result.Chart.Title.Text.Add(Funcs.Func[i].Eval.Expression);
End;

Function ws_ScatterPlot(
     DS      : TwsDataSet;  // Origem dos dados
     Const X : String;      // 1 coluna
     Y       : TStrings;     // n Colunas
     Title   : string = ''
  ): TfoChart;            // Janela que possui o gráfico
var i, j      : Integer;
    Ind       : TwsLIVec;      // Índice das colunas a serem plotadas
    XValue    : Double;        // Valor atual do eixo X
    YValue    : Double;        // Valor atual do eixo Y
    bAux      : Boolean;       // Variável auxiliar para tratar valor perdido
    xCol      : Word;          // Índice da variável independente
    VRet      : Double;        // recebe o valor da celula no teste de valor perdido
    L         : TPointSeries;  // variável que contém a série
Begin
  {Cria o janela do Gráfico}
  Result:= TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D := False;
  Result.Chart.BottomAxis.LabelsAngle:=325;
  Result.Chart.Color:=clWhite;
  Try
    Ind:= DS.IndexColsFromStrings(Y);
    Screen.Cursor := crHourGlass;
    Try
      xCol:= DS.Struct.IndexOf(X);
      {Plota as colunas}
      For i:= 0 to Y.Count - 1 do
        Begin
        L:= Result.Series.AddPointSerie(Y[i], selectColor(i)); //Adiciona as Séries

        // Insere as observacoes que nao possuem valores perdidos
        For j:= 1 to DS.NRows do
          Begin
          bAux:= True;
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
            YValue:= VRet
          else
            bAux:= False;
          if not DS.IsMissValue(j, xCol, VRet) then
            XValue:= VRet
          else
            bAux:= False;

          if bAux then L.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
          End;

        L.ShowInLegend:= not (Y.Count = 1); //mostra na legenda somente quando tem mais de uma série
        L.Pointer.HorizSize:= 3; L.Pointer.VertSize:= 3;
        End;

      Result.Chart.Title.Text.Clear;
      Result.Chart.Title.Text.Add(DS.MLab);

      if Y.Count = 1 then
        Result.Chart.LeftAxis.Title.Caption := Y[0] //StrListToStr(TStringList(Y));
      else
        Result.Chart.LeftAxis.Title.Caption := 'Y';

      Result.Chart.BottomAxis.Title.Caption := X;

    Finally
      Screen.Cursor := crDefault;
      Ind.Free;
    End;

  Except
    Result.Free;
    Result := nil;
    Raise;
  End;
End; {ScatterPlot bivariado }

Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Y       : TStrings;     {n Colunas}
     Title   : string = ''
  ): TfoChart;            {Janela que possui o gráfico}
var i, j      : Integer;
    Ind       : TwsLIVec;      {Índice das colunas a serem plotadas}
    XValue    : Double;        {Valor atual do eixo X}
    YValue    : Double;        {Valor atual do eixo Y}
    bAux      : Boolean;       {Variável auxiliar para tratar valor perdido}
    xCol      : Word;          {Índice da variável independente}
    VRet      : Double;        {recebe o valor da celula no teste de valor perdido}
    L         : TPointSeries;  //variável que contém a série
Begin
  {Cria o janela do Gráfico}
  Result:= TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D := False;
  Result.Chart.BottomAxis.LabelsAngle:=325;
  Result.Chart.Color:=clWhite;
  Try
    Ind:= DS.IndexColsFromStrings(Y);
    Screen.Cursor := crHourGlass;
    Try
      {Plota as colunas}
      For i:= 0 to Y.Count - 1 do
        Begin
        Randomize;
        L:= Result.Series.AddPointSerie(Y[i], selectColor(i)); //Adiciona as Séries

        // Insere as observacoes que nao possuem valores perdidos
        For j:= 1 to DS.NRows do
          Begin
          bAux:= True;
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
            YValue:= VRet
          else
            bAux:= False;
          if bAux then L.AddXY(YValue, Random, ''{sAjusta(XValue)}, clTeeColor);
          End;

        L.ShowInLegend:= not (Y.Count = 1); //mostra na legenda somente quando tem mais de uma série
        L.Pointer.HorizSize:= 3; L.Pointer.VertSize:= 3;
        End;

      // esconder o eixo Y

      Result.Chart.Title.Text.Clear;
      Result.Chart.Title.Text.Add(DS.MLab);
      if (Y.Count = 1) then
        Result.Chart.BottomAxis.Title.Caption := Y[0]
      else
        Result.Chart.BottomAxis.Title.Caption := 'Valores das Variáveis';

    Finally
      Screen.Cursor := crDefault;
      Ind.Free;
    End;

  Except
    Result.Free;
    Result := nil;
    Raise;
  End;
End; {ws_ScatterPlot univariado}

Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     Funcs   : TFunctions;   {Lista de expressões que serão calculadas}
     Title   : string = 'Gráfico de Dispersão de Pontos Incluindo Funções'
  ): TfoChart;            {Janela que possui o gráfico}
var i, j      : Integer;
    Ind       : TwsLIVec;      {Índice das colunas a serem plotadas}
    XValue    : Double;        {Valor atual do eixo X}
    YValue    : Double;        {Valor atual do eixo Y}
    bAux      : Boolean;       {Variável auxiliar para tratar valor perdido}
    xCol      : Word;          {Índice da variável independente}
    NP        : Longint;       {Número de pontos usado para o cálculo de funções}
    FM        : Double;        {Fator de multiplicação para cálculo do progresso de funções}
    VRet      : Double; //variavel que recebe o valor da celula no teste de valor perdido
    L1        : TLineSeries;  //variável que contém a série
    L2        : TPointSeries;  //variável que contém a série
Begin
  {Cria o janela do Gráfico}
  Result:= TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D := False;
  Result.Chart.BottomAxis.LabelsAngle:=325;
  Result.Chart.Color:=clWhite;
  Try
    Ind := DS.IndexColsFromStrings(Y);
    Screen.Cursor := crHourGlass;
    Try
      xCol := DS.Struct.IndexOf(X);

      {Plota as colunas}
      For i := 0 to Y.Count - 1 do
        Begin
        L2:= Result.Series.AddPointSerie(Y[i], selectColor(i));
        For j := 1 to DS.NRows do
          Begin
          bAux:= True;
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
            YValue:= VRet
          else
            bAux:= False;
          if not DS.IsMissValue(j, xCol, VRet) then
            XValue:= VRet
          else
            bAux:= False;

          if bAux then L2.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
          End;
        L2.ShowInLegend:= not (Y.Count = 1); //mostra na legenda somente quando tem mais de uma série
        End;

      {Plota as funções se definidas}
      If Funcs <> Nil Then
         For i:= 0 to Funcs.Count - 1 do
           Begin
           L1:= Result.Series.AddLineSerie(Funcs.Func[i].Eval.Expression, selectColor(i + Y.Count));
           XValue := Funcs.Func[i].Li;

           FM := 1 / Funcs.Func[i].Incr;
           NP := Trunc((Funcs.Func[i].Ls - Funcs.Func[i].Li) * FM);

           While XValue < Funcs.Func[i].LS Do
             Begin
             YValue:= Funcs.GetValue(XValue, i);
             L1.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
             XValue:= XValue + Funcs.Func[i].Incr;
             End;
           End;

      Result.Caption:= 'Funções definidas pelo usuário';
      Result.Chart.Title.Text.Clear;
      Result.Chart.Title.Text.Add(DS.MLab);
      Result.Chart.BottomAxis.Title.Caption:= X;
      Result.Chart.Title.Text.Add(Title);
    Finally
      Screen.Cursor := crDefault;
      Ind.Free;
    End;

  Except
    Result.Free;
    Result := nil;
    Raise;
  End;
End; {DSScatterPlot}


{---------------------------  ws_ScatterPlotTendency  -------------------------}

{Dispersão de pontos com reta de tendência}
  Function ws_ScatterPlotTendency(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     x1, y1  : Double;      {indica o primeiro ponto de uma reta}
     x2, y2  : Double;      {indica o segundo ponto de uma reta}
     Funcs   : TFunctions;   {Lista de expressões que serão calculadas}
     Title   : String = 'Dispersão de Pontos'
  ): TfoChart;            {Janela que possui o gráfico}

var L : TLineSeries;  //variável que contém a série
begin
  Result:= ws_ScatterPlot(DS, X, Y, Title);
  Result.Chart.BottomAxis.LabelsAngle:=325;
  Result.Chart.Color:=clWhite;
  L:= Result.Series.AddLineSerie('Tendência', clRed);
  L.ShowInLegend:= False;
  L.AddXY(x1, y1, '', clTeeColor);
  L.AddXY(x2, y2, '', clTeeColor);
end;

{---------------------------  ws_DesignPlot  -------------------------}
                              {delineamento}
Function ws_DesignPlot(DS      : TwsDataSet;  {Origem dos dados}
                       Const Y : String;      {1 coluna}
                       Factors : TStrings     {n Colunas}
                      ): TfoChart;          {Janela que possui o gráfico}

var i,j,YCol : Integer;
    IndFactor: TwsLIVec;      //Índice das colunas dos fatores
    YMAX,                     //Valor Maximo de Y
    YMED,                     //Valor Medio  de Y
    YMIN     : double;         //Valor Minimo de Y
    Coluna   : TwsFactor;     //Coluna atual que esta sendo utilizada
    Means    : TwsGeneral;    //Matriz que retorna os valores MAX, MIN, MED,SQD, Repetições
    LL       : TLineSeries;  //variável que contém as séries de pontos
    LP       : TPointSeries;  //variável que contém as séries de pontos
Begin
  {Pega o indice da coluna Y}
  YCol := DS.Struct.IndexOf(Y);

  {IndFactor recebe os indices dos fatores}
  IndFactor := DS.IndexColsFromStrings(Factors);

  if (IndFactor<>nil) and (YCol<>-1) then
    begin
    //Calcula as estatisticas
    Means:=DS.MeanFactor(IndFactor, YCol, YMED, YMIN, YMAX);
    IndFactor.Free;
    {Cria o janela do Gráfico}
    Result := TfoChart.Create(GetCaption(DS));
    LL:= Result.Series.AddLineSerie('Média Geral', clTeeColor);
    LL.AddXY(1, YMED, '', clTeeColor);
    LL.AddXY(Factors.Count, YMED, '', clTeeColor);

    For i:= 1 to Factors.Count  do
      begin
      Coluna:= TwsFactor(DS.Struct.ColbyName(Factors[i-1]));
      LP:= Result.Series.AddPointSerie(Coluna.Name, selectColor(i));
      LP.Pointer.Style:= TSeriesPointerStyle(psDownTriangle);
      For j:= 1 to Coluna.LevelNames.Count Do {Varre os niveis do fator }
        LP.AddXY(i,Means[i,j],'',clTeeColor);
      end; {For i:= 1 to NumFactors}
    Means.Free;

    with Result.Chart do
      begin
      View3D:=False;
      Title.Text.Clear;
      Title.Text.Add(DS.MLab);
      LeftAxis.Title.Caption:= Y;
      BottomAxis.Title.Caption:= 'Fatores';;
      ZoomPercent(85);
      BottomAxis.LabelsAngle:=325;
      Color:=clWhite
      end
    end
End;
  {fim}


{---------------------------  ws_HistoGramPlot  -------------------------}
                                {Histograma}
Function ws_HistoGramPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String ;     {coluna a ser plotada}
     Colunas : TStrings     {n Colunas}
  ): TfoChart;            {Janela que possui o gráfico}
var i,j,cY,xCol : Integer;  {xCol- Índice da variável independente}
    XValue      : Double;        {Valor atual do eixo X}
    YValue      : Double;        {Valor atual do eixo Y}
    L           : TBarSeries;  //variável que contém a série
    st          : String;
    TabNum      : Boolean;
Begin
  {Cria o janela do Gráfico}
  Result := TfoChart.Create('Histograma: ' + GetCaption(DS));
  xCol := DS.Struct.IndexOf(X);
  Try
    Screen.Cursor := crHourGlass;

    {Plota as colunas}
    For i := 0 to Colunas.Count-1 do
      Begin
      L := Result.Series.AddBarSerie(Colunas[i],clTeeColor,ord(bsRectangle),ord(mbSide));
      L.BarWidthPercent:=95;

      cY := DS.Struct.IndexOf(Colunas[i]);
      For j := 1 to DS.NRows do
        Begin
        if (DS.Struct.Col[1] is TwsNumeric) then
          st:=FloatToStrF((DS[j,1]+DS[j,2])/2,ffGeneral,4,4)
        else
          if (DS.Struct.Col[1] is TwsQuantitative) then
            st:=FloatToStrF((DS[j,2]+DS[j,3])/2,ffGeneral,4,4)
          else
            st := TwsFactor(DS.Struct.Col[xCol]).LevelNames[DS.AsInteger[j,xCol]];
        if not DS.IsMissValue(j, cY, YValue) then
           L.AddXY(j, YValue, st, clTeeColor);
        End;
      End;

    //mostra na legenda somente quando tem mais de uma série
    L.ShowInLegend:=(Colunas.Count>1);
    with Result.Chart do
      begin
      BottomAxis.LabelsAngle:=325;
      Color:=clWhite;
      if Colunas.Count=1 then
        LeftAxis.Title.Caption :=Colunas[0];
      BottomAxis.Title.Caption :='Classes';
      View3D:= False;
      Title.Text.Add('Histograma para Feqüência Relativa '+Colunas[0]);
      Title.Text.Add(DS.MLab)
      end;
  Finally
    Screen.Cursor := crDefault;
  End;
End; {ws_HistoGramPlot}


{---------------------------  ws_InteractionPlot  -------------------------}
                                  {Interação}
Function ws_InteractionPlot(
     DS       : TwsDataSet;  // Origem dos dados
     Const Y  : String;      // 1 coluna, que sera plotado o eixo Y
     Factors  : TStrings;    // 2 Colunas
     out Means: TwsDataSet   // conjunto de dados com as medias
  ): TfoChart;            {Janela que possui o gráfico}

{ Objetivo
    Obtem o grafico da interacao entre os dois fatores especificados
  Parametros
    DS: Conjunto de dados com os valores
    Y: Nome da variável resposta relacionada aos fatores
    Factors: Nomes dos dois fatores para o gráfico da interação
}
var
  YCol,                          // Indice da variavel numerica
  NumLin,i,j      : Integer;
  Factor,Asc,RVar : TwsLIVec;
  Vec             : TwsVec;
  C               : TwsDataSetCol;
  FacA,FacB       : TwsFactor;
  L               : TLineSeries; //variável que contém a série
Begin
  // Obtem o indice da coluna de Y
  YCol := DS.Struct.IndexOf(Y);
  if YCol<>-1 then                        // Variavel Y existe no conjunto ?
    begin
    Factor:= TwsLIVec.Create(2);          // Indices dos fatores
    Factor[1]:= DS.Struct.IndexOf(Factors[0]);
    FacA:=TwsFactor(DS.Struct.ColByName(Factors[0]));
    Factor[2]:= DS.Struct.IndexOf(Factors[1]);;
    FacB:=TwsFactor(DS.Struct.ColByName(Factors[1]));

    Asc:= TwsLIVec.Create(2);
    Asc[1]:= 1;                           // Ordenacao em ordem ascendente
    Asc[2]:= 1;

    DS.SortRows(Factor,Asc);              // Ordena para calcular as medias das combinacoes
    Asc.Free;

    RVar := TwsLIVec.Create(1);
    RVar[1] := YCol;
                                          // Fatores + medias + repet
    Means := TwsDataSet.Create('Med_Comb');
    Means.MLab:='Médias de combinações de níveis para '+Y;

    C:=TwsQualitative.Create(Factors[0],FacA.Lab);        { 1 }
    With TwsQualitative(C) Do
      for i:=0 to FacA.Levels-1 do
        AddLevel(FacA.LevelNames[i]);
    Means.Struct.AddColEx(C);

    C:=TwsQualitative.Create(Factors[1],FacB.Lab);        { 2 }
    With TWSQualitative(C) Do
      for i:=0 to FacB.Levels-1 do
        AddLevel(FacB.LevelNames[i]);
    Means.Struct.AddColEx(C);

    C:=TwsNumeric.Create('Medias','Médias de '+Y);        { 3 }
    Means.Struct.AddColEx(C);

    C:=TwsNumeric.Create('Repet','Número de repetições'); { 4 }
    Means.Struct.AddColEx(C);

    NumLin:=1;
    i:=0;
    repeat
      Vec := DS.SortKeyMeans(Factor,RVar,NumLin);
      if Vec <> nil then Means.MAdd(Vec);
    until Vec = nil;

    if (FacA.Levels*FacB.Levels <> Means.NRows) then
      begin
      Means.Free;
      Result:=nil;
      Raise Exception.Create('Todas as caselas necessitam ter observações');
      end;

    {Cria o janela do Gráfico}
    Result := TfoChart.Create(GetCaption(DS));
    NumLin:=0;
    For i:= 1 to FacA.Levels do  {Cria uma serie para cada nivel do fator A}
      Begin
      L:= Result.Series.AddLineSerie(FacA.LevelNames[i-1], selectColor(i-1));
      L.Pointer.Style:= TSeriesPointerStyle(psStar);
      For j:= 1 to FacB.Levels do
        Begin
        Inc(NumLin);
        L.AddXY(Means[NumLin,2],Means[NumLin,3],FacB.LevelNames[j-1],clTeeColor);
        End;
      End;

    with Result.Chart do
      begin
      BottomAxis.LabelsAngle:=325;
      Color:=clWhite;
      View3D:=False;
      Title.Text.Clear;
      BottomAxis.Title.Caption:= Factors[1];
      LeftAxis.Title.Caption:= 'Médias de '+Y
      end
    end;
  RVar.Free
End; {ws_InteractionPlot}

// Resultará em tantos gráficos quanto o número de Variáveis para análise
Function ws_BoxPlot(
     Frames      : TwsDataSets;         // Lista com os conjuntjos de dados
     Variaveis   : TStrings;            // Variaveis para construcao do grafico
     out StatList: array of TwsGeneral  // Lista com as matrizes de estatisticas
     ): TwsGraphics;                  {Lista com os gráficos criados}

        { Código das estatísticas
      Codigo             Estatistica          Posicao
           0:    0   0%  Minimo                     1
           4:  .25  25%      Q1                     2
           5:  .50  50%  Mediana                    3
           6:  .75  75%      Q3                     4
           7:    1 100%  Maximo                     5
           9:            QAmplit                    6
          13:            Cerca Inferior             7
          14:            Cerca Superior             8
          15:            val < cerca inf            9
          16:            val > cerca sup           10
          18:            Valor adjacente inferior  11
          19:            Valor adjacente superior  12
        }

var i, j, k,nBox  : Integer;      // contadores
    DSet          : TwsGeneral;   // matriz que armazena as estatísticas calculadas
    DS            : TwsDataSet;   // recebe um elemento dos frames por vez
    IndCol        : TwsLIVec;     // Índice das Variaveis a serem plotadas
    Stat          : TwsLIVec;     // Vetor com as estatiscas que deverao ser calculadas para o DataSet
    YCol          : Word;
    AOpen, AClose,
    AHigh, ALow,
    Max,Min,Med,
    QAmplit       : Double;
    Extremos      : TDoubleList;
    Tit           : String;       //título da série formatado
    LV            : TBoxPlotWithMinMax_Series; //variável temporária que contém a série de velas
Begin
  {Cria o janela do Gráfico}
  Result := TwsGraphics.Create;
  Try
    Screen.Cursor:= crHourGlass;
    Try
      {Indices das estatisticas que serão calculadas}
      Stat := TwsLIVec.CreateFrom([0,4,5,6,7,9,13,14,15,16,18,19]);

      // Variável que conterá os valores dos pontos extremos
      Extremos := TDoubleList.Create;

      // Cria o gráfico
      Result.New('', '');
      Result.ActiveChart.View3D := False;

      IndCol := Frames[0].IndexColsFromStrings(Variaveis);

      // Existirão tantas séries quanto variáveis
      for k := 0 to Variaveis.Count-1 do
        begin
        // Cria a série BoxPlot
        LV := TBoxPlotWithMinMax_Series.Create(nil);
        LV.ParentChart := Result.ActiveChart;
        LV.Title := Variaveis[k];
        end;
      nBox:=0;
      // Para cada conjunto de dados
      for k := 0 to Frames.Count-1 do
        Begin
        DS := Frames[k];
        DSet := DS.MatOrderStat(IndCol,Stat); StatList[k]:=DSet;
        // Para cada variavel
        For i := 1 to DSet.NRows do   // cada linha de DSet é uma variável
          begin
          AOpen   := DSet[i, 4]; {Q3}
          AClose  := DSet[i, 2]; {Q1}
          Max     := DSet[i, 5]; {Max}
          Min     := DSet[i, 1]; {Min}
          Med     := DSet[i, 3]; {Mediana}
          QAmplit := DSet[i, 6]; {Amplitude}
          ALow    := DSet[i,11]; {valor adjacente inferior}
          AHigh   := DSet[i,12]; {valor adjacente superior}

          Extremos.Clear;

          if DSet[i,9] > 0 then //se existem valores abaixo da cerca inferior
             for j:= 1 to DS.NRows do
               if DS[j,IndCol[i]] < ALow then
                  Extremos.Add(DS[j, IndCol[i]]);

          if DSet[i,10] > 0 then //se exitem valores acima da cerca superior
             for j:= 1 to DS.NRows do
               if DS[j,IndCol[i]] > AHigh then
                  Extremos.Add(DS[j,IndCol[i]]);

          Inc(nBox);
          LV := TBoxPlotWithMinMax_Series(Result.ActiveChart.Series[i-1]);
          LV.AddBox(DS.Group,nBox,aClose,aOpen,aHigh,aLow,Min,Max,Med,Extremos.ToArray);
          end; // for i
        end; // for kk

      // Centraliza um pouco mais os pontos se tenho mais de uma caixa por gráfico
      // Nunca use este método antes de ter adicionado os pontos !!

      with Result.ActiveChart do
        begin
        BottomAxis.LabelsAngle:=325;
        Color:=clWhite;

         ZoomPercent(85);
         if Frames.Count=1 then
            BottomAxis.LabelStyle:=talNone;
         for i:=0 to SeriesCount-1 do
           if nBox < 11 then
             TBoxPlotWithMinMax_Series(Series[i]).BoxWidth := 24;
        Legend.Visible:=True;
        if DSet.nRows=1 then
          begin
          Legend.Visible:=False;
          LeftAxis.Title.Caption:= DSet.RowName[1];
          end;
        end;
    Finally
      Stat.Free;
      Extremos.Free;
      IndCol.Free;
    End;
  Finally
    Screen.Cursor := crDefault;
  End;
End; {ws_BoxPlot}

{---------------------------  ws_ScatterPlotIndex  -------------------------}

Function ws_ScatterPlotIndex(
  DS      : TwsDataSet;  {Origem dos dados}
  Const X : String       {1 coluna}
): TfoChart;           {Janela que possui o gráfico}

var i         : Integer;
    XValue    : Double;        {Valor atual do eixo X}
    YValue    : Double;        {Valor atual do eixo Y}
    bAux      : Boolean;       {Variável auxiliar para tratar valor perdido}
    xCol      : Word;          {Índice da variável independente}
    VRet      : Double;        {recebe o valor da celula no teste de valor perdido}
    L         : TPointSeries;  //variável que contém a série
Begin
  {Testo se as colunas existem, simplesmente faço uma referência}
  DS.Struct.ColByName(X);

  {Cria o janela do Gráfico}
  Result := TfoChart.Create(GetCaption(DS));
  Try
    {Adiciona a série - coluna}
    L:= Result.Series.AddPointSerie(X, clRed);
    Screen.Cursor:= crHourGlass;
    Try
      xCol:= DS.Struct.IndexOf(X);

      {Plota a Variável}
      For i:= 1 to DS.NRows do
        Begin
        bAux:= True;
        if not DS.IsMissValue(i, xCol, VRet) then
          YValue := VRet
        else
          bAux:= False;

        XValue:= i;
        if bAux then L.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
        End;
      L.ShowInLegend:= False;

    with Result.Chart do
      begin
      BottomAxis.LabelsAngle:=325;
      Color:=clWhite;
      View3D:=False;
      Title.Text.Clear;
      Title.Text.Add(DS.MLab);
      BottomAxis.Title.Caption:= 'Índice';
      LeftAxis.Title.Caption:= X;
      end;

    Finally
      Screen.Cursor := crDefault;
    End;

  Except
    Result.Free;
    Result:= nil;
    Raise;
  End;
end;

 {---------------------------  ws_Lines  -------------------------}

Function ws_Lines(
   DSets   : TwsDataSets; {Origem dos Dados}
   Const X : String;      {1 coluna}
   Colunas : TStrings     {n Colunas}
): TfoChart;              {Janela que possui o grafico}
Var i,j,k  : integer;     {Contadoras}
    XValue : Double;      {Valor atual do eixo X}
    YValue : Double;      {Valor atual do eixo Y}
    ind    : TwsLIVec;    {Índice das Colunas a serem plotadas}
    bAux   : Boolean;     {Variável auxiliar para tratar valor perdido}
    xCol   : Word;        {Coluna que está sendo plotada}
    VRet   : Double;      //variavel que recebe o valor da celula no teste de valor perdido
    L      : TLineSeries; //variável que contém a série
    DS     : TwsDataSet;
    st     : string;
Begin
  Result:= TfoChart.Create(GetCaption(DSets[0]));
  Try
    Screen.Cursor:= crHourGlass;
    ind:= DSets[0].IndexColsFromStrings(Colunas);
    xCol:= DSets[0].Struct.IndexOf(X);
    for k:=0 to DSets.Count-1 do
      begin
      DS:=DSets[k];
      For i:= 0 to Colunas.Count - 1 do
        Begin
        st:=DS.MLab+'.'+Colunas[i];
        Delete(st,1,7);
        L:= Result.Series.AddLineSerie(st, gCores[k+i]);

        For j:= 1 to DS.NRows do
          Begin
          bAux:= True;
          //para não plotar o ponto que é valor perdido com o valor anterior
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
            YValue:= VRet
          else
            bAux:= False;
          if not DS.IsMissValue(j, xCol, VRet) then
            XValue:= VRet
          else
            bAux:= False;

          if bAux then L.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
          End;
        L.ShowInLegend:= not(Colunas.Count = 1);
        End;
      end;
    with Result.Chart do
      begin
      BottomAxis.LabelsAngle:=325;
      Color:=clWhite;
      View3D:=False;
      if Colunas.Count=1 then
        LeftAxis.Title.Caption:= Colunas[0]
      else
        LeftAxis.Title.Caption:= 'Y';;
      BottomAxis.Title.Caption:= X
      end;
  Finally
    Screen.Cursor := crDefault;
  End;
End;

 {---------------------------  ws_ColVert  -------------------------}

Function ws_ColVert(DS: TwsDataSet; Colunas : TStrings; const Names: String=''): TfoChart;
{ Objetivo
    Constrói gráficos de colunas verticais
  Parâmetros
    DS: Conjunto de dados
    Colunas: Lista das variáveis. Os retângulos para as diferentes variáveis serão colocados
      lado a lado
    Names: Coluna (do tipo fator) onde estarão os nomes que identificarão cada coluna. Se não
      for alterado o valor default (vazio), cada coluna será identificada com o índice apenas.
}
Var
  i,j,                     // Contadoras
  nCol,                    // coluna com os nomes
  xCol   : Integer;        // coluna com os valores
  XValue,                  // Valor atual do eixo X
  YValue : Double;         // Valor atual do eixo Y
  st     : string;         // Nome para a coluna
  L      : TBarSeries;     //variável que contém a série
Begin
  Result:= TfoChart.Create(GetCaption(DS));
  Try
    Screen.Cursor:= crHourGlass;
    if Names<>'' then
      nCol:=DS.Struct.IndexOf(Names)
    else
      nCol:=-1;
    For i:= 0 to Colunas.Count - 1 do
      Begin
      //Tipo é: TMultiBar=(mbNone,mbSide,mbStacked,mbStacked100)
      //Estilo de barra TBarStyle=(bsRectangle,bsPyramid,bsInvPyramid,bsCilinder,bsEllipse,bsArrow,bsRectGradient);
      L:= Result.Series.AddBarSerie(Colunas[i], clTeeColor, 6, 1);

      xCol:= DS.Struct.IndexOf(Colunas[i]);
      For j:= 1 to DS.NRows do
        Begin
        XValue:= j;
        if nCol=-1 then
          if DS.ColIdentName<>'' then
            st:=DS.RowName[j]
          else
            st:=IntToStr(j)
        else
          st:=TwsFactor(DS.Struct.Col[nCol]).LevelNames[Trunc(DS[j,nCol])];
        if not DS.IsMissValue(j,xCol,YValue) then
          L.AddXY(XValue,YValue,st,clTeeColor);
        End;
      L.ShowInLegend:= (Colunas.Count > 1); //mostra na legenda somente quando tem mais de uma série
      L.BarWidthPercent:= 93; //98
      End;
    with Result.Chart do
      begin
      BottomAxis.LabelsAngle:=325;
      Color:=clWhite;
      Title.Text.Clear;
      View3D:= False;
      Title.Text.Add(DS.MLab);
      if Colunas.Count=1 then
        LeftAxis.Title.Caption:= Colunas[0]
      else
        LeftAxis.Title.Caption:= 'Y';
      end;
  Finally
  Screen.Cursor := crDefault;
  End;
End;

 {---------------------------  ws_PizzaPlot  -------------------------}

Function ws_PizzaPlot(DS: TwsDataSet; const Coluna: String; const Names: string=''): TfoChart;
{ Objetivo
    Constrói gráficos de setores (pizzas)
  Parâmetros
    DS: Conjunto de dados
    Coluna: Nome da variável que forncerá os valores
    Names: Coluna (do tipo fator) onde estarão os nomes que identificarão cada setor. Se não
      for alterado o valor default (vazio), cada setor será identificada com o índice apenas.
}
Var
  i, j     : integer;    // Contadoras
  XValue,                // Valor atual do eixo X
  YValue   : Double;     // Valor atual do eixo Y
  xCol,nCol: Integer;    // Coluna que está sendo plotada
  L        : TPieSeries; // Variável que contém a série
  st       : String;
Begin
  Result:= TfoChart.Create(GetCaption(DS));
  Try
    Screen.Cursor:= crHourGlass;
    L:= Result.Series.AddPieSerie(Coluna);
    xCol:=DS.Struct.IndexOf(Coluna);
    if Names<>'' then
      nCol:=DS.Struct.IndexOf(Names)
    else
      nCol:=-1;
    For j:= 1 to DS.NRows do
      Begin
      XValue:= j;
      if nCol=-1 then
        if DS.ColIdentName<>'' then
          st:=DS.RowName[j]
        else
          st:=IntToStr(j)
      else
        st:=TwsFactor(DS.Struct.Col[nCol]).LevelNames[Trunc(DS[j,nCol])];
      if not DS.IsMissValue(j,xCol,YValue) then
        L.Add(YValue,st,clTeeColor);
      End;
    L.ShowInLegend:= True;
    with Result.Chart do
      begin
      BottomAxis.LabelsAngle:=325;
      Color:=clWhite;
      Title.Text.Clear;
      Title.Text.Add('Gráfico de Pizza para '+Coluna)
      end;
  Finally
    Screen.Cursor := crDefault;
  End;
End;

 {---------------------------  ws_VectorsPlot  -------------------------}

Function ws_VectorsPlot(
  const x        : array of TwsVec;       // vetores das abcissas
  const y        : array of TwsVec;       // vetores das ordenadas
  const Tipo     : array of TwsSerieType; // tipo de cada série
  var   Erro     : Integer                // 0 - Sem Erros, > 0 Algum Erro
  ): TfoChart;                          // Resultado: Janela que possui o gráfico
{  Objetivo
     Faz o gráfico de dispersão bivariado entre os vetores que compõem os arrays x e y
   Parãmetros
     x: array [0..Length(x)] com os endereços dos vetores para o eixo das abscissas
     y: array [0..Length(y)] com os endereços dos vetores para o eixo das ordenadas
     Tipo: array [0..Length(Tipo)] com os tipos de pontos. Os tipos válidos são:
       stLine: para gráfico de linhas entre x[i] x y[i]
       stPoint: para gráfico de pontos entre x[i] x y[i]
       stBar: para gráfico de barras entre x[i] x y[i]
       stPie: para gráfico de pizza entre x[i] x y[i]
     Erro: retorna código de erro
       0: processamento Ok
       1: Não há correspondência na quantidade de vetores em x e y
       2: Os pares de vetores x[i] e y[i] devem ter a mesma dimensão
   Resultado
     Janela gráfica
   Observações
     pode gerar excessão no processamento das janelas gráficas
}

var
  i,j : Integer;
  LX  : TChartSeries;                     // variável que contém a série
Begin
  Erro := 0;

  if (Length(x) <> Length(y)) or (Length(x) <> Length(Tipo)) then
     begin
     Erro := 1;
     exit; // Não há correspondência na quantidade dos parâmetros
     end;

  for i := 0 to High(x) do
    if x[i].Len <> y[i].Len then
       begin
       Erro := 2;
       exit; // Não há correspondência no número de elementos de cada par de vetores
       end;

  {Cria o janela do Gráfico}
  Result:= TfoChart.Create('Gráficos de Vetores');
  Try
    Screen.Cursor := crHourGlass;
    Try
      for i:= 0 to High(Tipo) do
        Begin
        case Tipo[i] of
          stLine     : LX:= Result.Series.AddLineSerie(y[i].Name, clTeeColor);
          stArea     : Raise Exception.Create('Não Implementado para área!');
          stPoint    : LX:= Result.Series.AddPointSerie(y[i].Name, clTeeColor);
          stBar      : LX:= Result.Series.AddBarSerie(y[i].Name, clTeeColor, 1, 0);
          stPie      : LX:= Result.Series.AddPieSerie(y[i].Name);
          stFastLine : Raise Exception.Create('Não Implementado para fastline!');
          stArrows   : Raise Exception.Create('Não Implementado para setas!');
          stGant     : Raise Exception.Create('Não Implementado para gant!');
          stBubble   : Raise Exception.Create('Não Implementado para bolhas!');
          stCandle   : Raise Exception.Create('Não Implementado para candle!');
          End; //case Tipo[i]

        { Insere os valores na serie }
        For j:= 1 to x[i].Len do
          if not (IsMissValue(y[i].Data[j]) or IsMissValue(x[i].Data[j])) then
             LX.AddXY(x[i].Data[j],y[i].Data[j], ''{sAjusta(XValue)}, clTeeColor);
        LX.ShowInLegend := True;
        End; //for i:=1 to High(Tipo)

      with Result.Chart do
        begin
        BottomAxis.LabelsAngle:=325;
        Color:=clWhite;
        View3D:= False;
        Title.Text.Clear
        end;
    Except
      Result.Free;
      Result := nil;
      Erro := 3;
    End;
  Finally
    Screen.Cursor := crDefault;
  End;
end;

 {---------------------------  ws_MatrixPlot  -------------------------}

Function ws_MatrixPlot(x       : TwsVec;
                       Y       : TwsGeneral;
                       GType   : TwsSerieType;
                       var Erro: Integer;
                       w: TwsVec=nil;
                       z: TwsVec=nil): TfoChart;
{ Objetivo
    Constroi gráfico de pontos entre um conjunto de vetores e as linhas da matriz
  Parâmetros
    x    : Vetor das abcissas, único para todas as linhas de Y
    Y    : Matriz geral onde cada linha (ordenadas) é uma séries de pontos
    GType: Tipo comum para todas as séries.
    Erro : retorna código de erro
      0: processamento Ok
      2: x e as linhas de Y não têm o mesmo número de elementos
      3: w e z não têm o mesmo número de elementos. Não impede que o gráfico entre x
        e as linhas de Y seja construído.
      4: Erro na construção do gráfico
    w, z: vetores para construção de séries adicionais, se apontarem para algum endereço
      válido. Necessitam ter o mesmo número de elementos mas podem ter dimensão distinta
      de x e das linhas de Y
  Retorno: janela gráfica
}
var
  i,j : Integer;
  LX  : TChartSeries; //variável que contém a série
Begin
  Erro := 0;
  if x.Len <> Y.NCols then
    begin
    Erro := 2;
    exit; // Não há correspondência no número de elementos de cada par de vetores
    end;
  {Cria o janela do Gráfico}
  Result:= TfoChart.Create('Gráfico de Dispersão');
  Try
    Screen.Cursor := crHourGlass;
    Try
      for i:= 1 to Y.NRows do
        Begin
        case GType of
          stLine     : LX:= Result.Series.AddLineSerie(Y.RowName[i],clTeeColor);
          stArea     : Raise Exception.Create('Não Implementado para área!');
          stPoint    : LX:= Result.Series.AddPointSerie(Y.RowName[i],gCores[i-1]{clTeeColor});
          stBar      : begin
                       LX:= Result.Series.AddBarSerie(Y.RowName[i],clTeeColor,0,1);
                       TBarSeries(LX).BarWidthPercent:=95;
                       end;
          stPie      : LX:= Result.Series.AddPieSerie(Y.RowName[i]);
          stFastLine : Raise Exception.Create('Não Implementado para fastline!');
          stArrows   : Raise Exception.Create('Não Implementado para setas!');
          stGant     : Raise Exception.Create('Não Implementado para gant!');
          stBubble   : Raise Exception.Create('Não Implementado para bolhas!');
          stCandle   : Raise Exception.Create('Não Implementado para candle!');
          End; //case Tipo
        {Plota os Vetores}
        For j:= 1 to Y.NCols do
          if not (IsMissValue(Y[i,j]) or IsMissValue(x[j])) then
             LX.AddXY(x[j],Y[i,j],'',clTeeColor);
        LX.ShowInLegend := Y.RowName[i]<>'';
        End; //for i:=1 to Y.NRows
        // Inclui series adicionais, se for o caso
        if (w<>nil) and (z<>nil) then
          if w.Len = z.Len then
            begin
            LX:= Result.Series.AddPointSerie(z.Name,clTeeColor);
            For j:= 1 to w.Len do
              if not (IsMissValue(w[j]) or IsMissValue(z[j])) then
                 LX.AddXY(w[j],z[j],'',clTeeColor);
            LX.ShowInLegend := True;
            end
          else
            Erro:=3;
      with Result.Chart do
        begin
        BottomAxis.LabelsAngle:=325;
        Color:=clWhite;
        View3D:= False;
        Title.Text.Clear
        end;
    Except
      Result.Free;
      Erro:=4;
      Result := nil;
    End;
  Finally
    Screen.Cursor := crDefault;
  End;
end;

Function ws_MatrixPlotTendency(
  x       : TwsVec;
  Y       : TwsGeneral;
  GType   : TwsSerieType;
  x1, y1  : Double;      {indica o primeiro ponto de uma reta}
  x2, y2  : Double;      {indica o segundo ponto de uma reta}
  var Erro: Integer;
  w: TwsVec=nil;
  z: TwsVec=nil
  ): TfoChart;
var
  L : TLineSeries;  //variável que contém a série
begin
  Result:=ws_MatrixPlot(x,Y,GType,Erro,w,z);
  Result.Chart.BottomAxis.LabelsAngle:=325;
  Result.Chart.Color:=clWhite;
  L:= Result.Series.AddLineSerie('Formato nulo', clTeeColor);
//  L.Pointer.VertSize:=5;
//  L.Pointer.HorizSize:=5;

//  L.ShowInLegend:= False;
  L.AddXY(x1, y1, '', clTeeColor);
  L.AddXY(x2, y2, '', clTeeColor)
end; // MatrixPlotTendency

Function ws_IntervalPlot(
   DS   : TwsDataSet;  // Origem dos dados
   i,j,k: Integer      // coluna do valor inferior, superior e cor
): TfoChart;           // Janela que possui o gráfico
const
  Cor: array[Boolean] of TColor = (clRED, clBLUE);
var
  ii       : Integer;
  p95      : TInterval_Series;
  FMedia_95: TLineSeries;
begin
  {Cria o janela do Gráfico}
  Result:= TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D := False;
  Result.Chart.BottomAxis.Title.Caption:='Limites dos Intervalos a 95%';
  Result.Chart.Color:=clWhite;

  FMedia_95:= Result.Series.AddLineSerie('Média', clGreen);
  FMedia_95.LinePen.Width := 2;
  FMedia_95.ShowInLegend:= False;

  p95:=Result.Series.AddIntervalSerie('Intervalos');
  p95.ShowInLegend:=False;
  for ii:=1 to DS.nRows do
    p95.Add('', k, DS[ii,i],DS[ii,j],Cor[DS[ii,k]<>0]);

  FMedia_95.AddXY(0,Result.Chart.MinYValue(Result.Chart.LeftAxis), '', clTeeColor);
  FMedia_95.AddXY(0,Result.Chart.MaxYValue(Result.Chart.LeftAxis), '', clTeeColor);
end;


function GraphQuantil(DS: TwsDataSet; const YStr: String): TfoChart;
{ Objetivo
    Construir gráfico dos quantis observados (váriavel com valores ordenados) contra
    as respectivas proporções de dados que esses valores representam.
  Parâmetros
    DS  : Conjunto de dados
    YStr: Variável para obtenção dos quantis observados
}
var
  pData,vData: TwsVec;
  k,n        : Integer;
  Graf_Erro  : Integer;
begin
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  pData := TwsDFVec.Create(n);
  vData.QuickSort(True); // Esta ordem deve ser ascendente
  try
    for k := 1 to n do
      pData[k] := (k-0.5)/n; // como valor correspondente para o eixo X
    pData.Name:=YStr;
    Result := ws_VectorsPlot([pData],[vData],[stPoint],Graf_Erro);
    Result.Chart.BottomAxis.LabelsAngle:=325;
    Result.Chart.Color:=clWhite;
    if Graf_Erro = 0 then
       begin
       Result.Chart.LeftAxis.Title.Caption:= YStr;
       Result.Chart.BottomAxis.Title.Caption:= 'Quantis';
       Result.Chart.Title.Text.Add('Gráfico de Quantis');
       end
    else
       raise Exception.Create('Erro na geração dos gráficos: Código ' + intToStr(Graf_Erro));
  finally
    pData.Free; vData.Free;
  end;
end; { GraphQuantil }

function SymmetryPlot(DS: TwsDataSet; const YStr: String): TfoChart;
{ Objetivo
    Construir gráfico para auxílio na detecção de distribuições assimétricas
  Parâmetros
    DS         : Conjunto de dados
    YStr       : Coluna para a qual se deseja fazer o gráfico.
}
var
  vData,xData,yData: TwsVec;
  Md               : Double;
  k,n,n1,j         : Integer;
  Graf_Erro        : Integer;
begin
    vData := DS.CopyCol(DS.Struct.IndexOf(YStr));  // Obtem os valores das colunas
    vData.QuickSort(True);
    n := vData.Len;
    if Odd(n) then
      begin                       // Obtem a mediana
      k := (n+1) div 2;
      Md := vData[k]
      end
    else
      begin
      k := n div 2;
      Md := (vData[k]+vData[k+1])/2
      end;
    xData := TwsDFVec.Create(k);
    yData := TwsDFVec.Create(k);
    n1 := n+1;
    try
      for j := 1 to k do
        begin
        xData[j] := Md - vData[j];       // para o eixo X.
        yData[j] := vData[n1-j]-Md;      // para o eixo Y
        end;
      xData.Name:= 'Simetria';
      yData.Name:= 'Diferenças';
      Result := ws_VectorsPlot([xData,xData],[xData,yData],[stPoint,stPoint],Graf_Erro);
      Result.Chart.BottomAxis.LabelsAngle:=325;
      Result.Chart.Color:=clWhite;
      if Graf_Erro = 0 then
         begin
         Result.Chart.LeftAxis.Title.Caption:= 'Mediana - Y';
         Result.Chart.BottomAxis.Title.Caption:= 'Y - Mediana';
         Result.Chart.Title.Text.Add('Gráfico de Simetria para '+YStr)
         end
      else
         raise Exception.Create('Erro na geração dos gráficos: Código '+intToStr(Graf_Erro));
    finally
      vData.Free;
      xData.Free;
      yData.Free;
    end;
end; { SymmetryPlot }

function QQGraph(DS: TwsDataSet; const XStr, YStr: String): TfoChart;
{ Objetivo
    Construir gráfico dos quantis observados de uma variável contra quantis observados de
    outra.
  Parâmetros
    DS  : Conjunto de dados
    XStr: Nome da variável X
    YStr: Nome da variável Y
}
var
  vXData,vYData: TwsVec;
  Graf_Erro    : Integer;
begin
  vXData := DS.CopyCol(DS.Struct.IndexOf(XStr));// Obtem, a coluna para a variavel X
  vXData.QuickSort(True);                       // Em ordem ascendente
  vYData := DS.CopyCol(DS.Struct.IndexOf(YStr));// Obtem, a coluna para a variavel Y
  vYData.QuickSort(True);                       // Em ordem ascendente
  vXData.Name:='X=Y';
  vYData.Name:=YStr;
  Result := ws_VectorsPlot([vXData,vXData],[vXData,vYData],[stPoint,stPoint],Graf_Erro);
  Result.Chart.BottomAxis.LabelsAngle:=325;
  Result.Chart.Color:=clWhite;
  if Graf_Erro=0 then
     begin
     Result.Chart.Title.Text.Add('Gráfico de Quantis Empíricos');
     Result.Chart.LeftAxis.Title.Caption:= XStr;
     Result.Chart.BottomAxis.Title.Caption:= YStr;
     end;
  vXData.Free;
  vYData.Free
end; { QQGraph }


end.
