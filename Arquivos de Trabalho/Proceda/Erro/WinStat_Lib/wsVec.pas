{$N+,E+,X+}
unit wsVec;

{
  ÚLTIMA ATUALIZAÇÃO:  Rochedo, 17/05/1999

  HISTÓRICO  --------------------------------------------------------------------

  Rochedo, 20/03/1998
  Método: TwsVec.Delete; Abstrato.

  Rochedo, 17/06/1998
  Todas as variaveis inteiras de 2 bytes passam a ter 4 bytes.
  (Integer, Word) --> (Integer)

  Rochedo, 18/05/1999
  Revisao geral nas classes:
    - TwsVec: Base para os demais vetores
    - TwsSFVec: Vetores de Singles (Ponto-flutuante de 4 bytes)
    - TwsDFVec: Vetores de Doubles (Ponto-flutuante de 8 bytes)
    - TwsSIVec: Vetores de Small-Integers (Inteiros de 2 bytes sinalizados)
    - TwsLIVec: Vetores de Long -Integers (Inteiros de 4 bytes sinalizados)

  Rochedo, 11/12/2003
  Metodo TwsVec.Delete corrigido.
}

interface
uses Classes,
     Types,
     Math,
     SysUtils,
     Graphics,
     XML_Interfaces,
     {$ifdef MSXML}
     MSXML4,
     {$endif}
     SysUtilsEx,
     MessageManager,
     wsConstTypes,
     wsGLib;

var
  wsm_VectorChange : Integer; // Mensagem de notificação de mudança

const
  IDSize: Byte = 5;

Type
  Scalar = Double;
  TRuleFunc = function (const x, y: Real): Boolean;

  TwsLIVec = class;

  { Herança
      TwsVec --> TObject
    Objetivo
      A classe TwsVec é a classe básica para implementação de arrays numéricos. Seus descendentes
      possibilitam a construção de arrays de números de ponto flutuante e números inteiros em diversos
       formatos.Incorpora uma funcionalidade extremamente rica que inclui a ordenação, concatenação,
       gravação  e recuperação de disco, impressão, funções, operadores aritméticos, boleanos, de
       comparação e inúmeras outras funções.
  }

  TwsVec = class(T_NRC_InterfacedObject, IArray, IToBXML)
  Private
    // Nome do vetor
    FName     : String;
    // Comprimento ou número de componentes
    FLen      : Integer;
    // Nome do arquivo de saida
    FFileName : String;
    // Vetor modificado?
    FModified : Boolean;
    // Fonte dos dados quando impresso (somente é criado se acessado)
    FFont     : TFont;
    // Usado pela propriedade Font
    function GetFont: TFont;
    // Atribui nome ao vetor
    // Atribui valor
    procedure Put(i: Integer; x: Double); virtual; abstract;
    // Recupera valor
    function  Get(i: Integer): Double; virtual; abstract;
    // recupera real como inteiro
    function  GetAsInteger(i: Integer): Integer; virtual; abstract;
    // Recupera como string
    function GetAsString(i: Integer): String; virtual;
    // Retorna vetor inteiro como string;
    function GetVecAsString: String;
    // Dimensiona espaço que vetor irá ocupar
    procedure SetLength(aLen: Integer); virtual; abstract;
    function GetLength(): Integer;
    procedure InitPrintOptions();
    // IArray interface Links
    function a_Low(): integer;
    function a_High(): integer;
    function a_IsMissValue(i: integer; out x: double): boolean;
    function a_getAsString(i: integer): string;
    function a_getAsInteger(i: integer): integer;
    function a_getAsFloat(i: integer): double;
    procedure a_setAsString(i: integer; value: string);
    procedure a_setAsInteger(i: integer; value: integer);
    procedure a_setAsFloat(i: integer; value: double);

    // IArray interface
    function IArray.Low           = a_Low;
    function IArray.High          = a_High;
    function IArray.IsMissValue   = a_IsMissValue;
    function IArray.getAsString   = a_getAsString;
    function IArray.getAsInteger  = a_getAsInteger;
    function IArray.getAsFloat    = a_getAsFloat;
    procedure IArray.setAsString  = a_setAsString;
    procedure IArray.setAsInteger = a_setAsInteger;
    procedure IArray.setAsFloat   = a_setAsFloat;
  Public
    // Opções para impressão
    PrintOptions: TwsRecPrintOptions;

    Class Function VFromStream(Var Reader:TReader):TwsVec;

    // Participa da criação de um vetor
    constructor Create(ALen: Integer); overload;
    constructor Create(const Items: string); overload;
    constructor Create(const Values: Array of Double); overload;

    // Libera espaço ocupado pelo vetor
    destructor Destroy; override;

//    procedure ShowInColSheet(p:TSpreadSheetBook; c: Integer);

    // IToXML interface
    procedure ToXML(Buffer: TStrings; Ident: Integer);
    function GetClassName: String;

    // IToBXML Interface
    function GetBlockName: String;
    function GetBlockNameComment: String;

    procedure ToBXML(Buffer: TStrings; Ident: Integer);
    {$ifdef MSXML}
    procedure fromXML(no: IXMLDomNode);
    {$endif}

    // Espaço ocupado opr cada componente
    function  SizeOfElements: byte; virtual; abstract;
    // Extremamente ineficiente, evite o uso !!!
    procedure Add(const x: Double);
    // Deleta componentes
    procedure Delete(Start, Amt: LongWord);
    // Concatena componentes
    procedure Append(F: PFArray; Amt: Integer);
    // Insere componentes
    procedure Insert(F: TwsVec; Start, Amt: Integer);
    // troca componentes
    procedure Exchange(i, j: Integer);
    // Ordena pelo método ShellSort
    procedure ShellSort(Ascd: Boolean);
    // Utiliza vetor de índices gerado pelo ShellSort para ordenar
    function  ShellIndx(Ascd: Boolean): TwsLIVec;
    // Ordena pelo método QuickSort recursivo
    procedure QuickSort(Ascd: Boolean=True);
    // Ordena pelo método QuickSort não recursivo
    procedure NRQuick(Ascd: Boolean);
    // Utiliza vetor de índiecs gerado pelo método QuickSort recursivo para ordenar
    function  QuickIndx(Ascd: Boolean): TwsLIVec;
    // Ordena pelo método HeapSort
    procedure HeapSort(Ascd: Boolean);
    // Vetor está ordenado?
    function  Sorted(var i: Integer; Ascd: Boolean): Boolean;
    // Copia conteúdo para um string
    function  ToChar(Start, Amt: Integer; Buffer: TStrings): String;
    // Copia conteúdo de um string
    procedure StrToLine(P: PChar);
    // Verifica se um elemento é valor perdido
    function  IsMissValue(Index: Integer; out x: Double): Boolean;
    // Preenche vetor com um valor
    procedure Fill(Value: Double);
    // Copia os dados do vetor entrado como parâmetro
    procedure Assign(v: TwsVec);
    // Imprime em modo texto
    procedure Print(Buffer: TStrings);
    // Recupera conteúdo em disco
    Class Function LoadFromFile(Const Name: String): TwsVec;
    // Recupera conteúdo no disco
    Procedure LoadFromStream(Var Reader:TReader);
    // Escreve conteúdo no disco
    Procedure SaveToStream(Var Writer:TWriter);
    // recupera conteúdo via um TStrings
    procedure LoadFromStrings(SL: TStrings);
    // recupera conteúdo de arquivo texto
    procedure LoadFromTextFile(const FileName: String);
    // Grava em disco
    Procedure SaveToFile(Const FileName: String);
    // Recupera de arquivo texto
    Procedure FromText(Var Arq: Text); virtual;
    // Escreve conteúdo em arquivo texto
    Procedure ToText(Var Arq: Text; Precision: Byte); virtual;
    // escreve conteúdo no editor de textos
    Procedure ToEditor(Buffer: TStrings; ValoresPorLinha:Cardinal=12; Tamanho:byte=10);
    // Operação por escalar
    function  ByScalar(const x:Double; Op:TwsEnumTypeOp; NewVec,SFirst:Boolean):TwsVec; virtual; abstract;
    // Operação por elemento
    function ByElement(x:TwsVec; Op:TwsEnumTypeOp; NewVec:Boolean; Var ErrCode:Word):TwsVec; virtual;
    // Aplica função
    function Func(fOp:TwsEnumConstFun; NewVec:Boolean): TwsVec;
    // Produto de Kronecker
    function Kronecker(Y: TwsVec): TwsVec;
    // Copia integral
    function Clone(): TwsVec;
    // Cópia
    function Copy(Start, Amt: Integer): TwsVec;
    // Localiza valor
    procedure Locate(x: Double; var Ind: Integer);
    // Localiza valor
    procedure Hunt(x: Double; var jlo: Integer);
    // Localiza por pesquisa sequencial
    function SeqSearch(x: Double; var Ind: Integer): boolean;
    // Índiec do mínimo e do máximio
    procedure IndMinMax(var imin, imax: Integer);
    // valores mínimo e máximo do vetor
    procedure MinMax(var Min, Max: Double);
    // Índice do mínimo ou do máximo
    function  IndMinOrMax(IsMin: Boolean; var ind: Integer): Double;
    // Mínimo ou máximo
    function  MinOrMax(IsMin: Boolean): Double;
    // Média utilizando uma parte dos valores
    function PartialMean(StartCol, EndCol: Integer; out n: Integer): Double;
    // Média e variáncia com uma parte dos valores
    procedure PartialVarMean(SCol,FCol: Integer; var m, v:Double; var n:Integer);
    // Grupo de estatísticas com parte dos valores
    function PartialDescriptive(SCol, FCol: Integer): TwsVec;
    function  PartialVecDSP(StartCol,EndCol:Integer; Media:Double = -1): Double;
    function  PartialCV(StartCol,EndCol:Integer; Media:Double=-1; DSP:Double=-1): Double;
    procedure PartialVec_Mean_DSP_CV(StartCol, EndCol: Integer;
                             out n: Integer; out Mean, DSP, CV: Double);

    // Média e variância
    procedure VarMean(var m, v: Double; var n: Integer);
    // Média
    function  Mean(out n: Integer): Double;
    // Quantil
    function Quantile(p, q: Integer): Double;
    // Percentis
    function Percentis(Perc: TwsVec): TwsVec;
    // Momentos ponderados
    function WMoments(w: TwsVec; var ErrCode: Word): TwsVec;
    // Momentos
    function Moments: TwsVec;
    // Norma euclidiana
    function EuclideanNorm(var n: Integer): Double;
    // Norma do valor absoluto
    function AbsoluteNorm(var n: Integer): Double;
    // Total
    function Total(var n: Integer): Double;
    // Soma de quadrados
    function SumOfSq(var n: Integer): Double;
    // Valores acumulados
    function Accum(NewVec: Boolean): TwsVec;
    // Retorna um vetor com os dados = 1/dado
    function  Inv(NewVec: Boolean): TwsVec;
    // Valores ordenados
    function Sort(NewVec, Ascd: Boolean): TwsVec;
    // Valores com ordem pré-estabelecida
    procedure SortOf(Indx: TwsLIVec; Var ErrCode: Word);
    // Postos com empates
    function RankTie: TwsVec;
    // Postos
    function Rank(Ascd: Boolean; YSort : Boolean): TwsVec;
    // Valor de um polinômio
    function PolValue(const x: Double): Double;
    // Conjunto de valores de um polinômio
    function VecPolValue(x, incr: Double; n: Integer): TwsVec;
    // derivada de um polinômio
    function DerPolValue(var dp: Double; const x: Double): Double;
    // Combinação linear de vetores
    function LinearComb(X:TwsVec; k1, k2:Double; NewVec:Boolean; var ErrCode:Word):TwsVec;
    // Operação elementar do tipo 3
    function ElemOper3(X: TwsVec; k: Double; NewVec: Boolean; var ErrCode: Word): TwsVec;
    // Vetor possui valor perdido nos locais especificados?
    function LocMiss(Loc: TwsLIVec): Boolean;
    // Vetor possui valor perdido?
    function HaveMissValue(): Boolean;
    // Operação de todos os valores com um especificado. Ex.: todos iguais?
    function All(const x: Double; Op: TwsEnumTypeOp): boolean;
    // Operação de alguns os valores com um especificado. Ex.: algum igual?
    function Any(const x: Double; Op: TwsEnumTypeOp): boolean;

    // Retorna os índices dos valores que cumprem a regra
    function  Find    (const x: Real; Rule: TRuleFunc): TwsVec;
    function  FindGT  (const x: Real): TwsVec; // >  x
    function  FindGTE (const x: Real): TwsVec; // >= x
    function  FindLT  (const x: Real): TwsVec; // <  x
    function  FindLTE (const x: Real): TwsVec; // <= x

    // Retorna os índices da sequência que cumpre a regra
    function  FindMinMean (n: Word): TwsVec;
    function  FindMinSun  (n: Word): TwsVec;

    // O nome ja diz tudo
    function  Max : Real;
    function  Min : Real;

    // Retorna um vetor com os dados de índices "Indexs"
    function REC(Indexs: TwsVec): TwsVec;

    // Verifica a existencia de uma definição de fonte
    function HasFont: Boolean;
    // Envia uma notificacao de destruicao
    procedure NotificationAndFree;
    // Notifica uma mudança nas propriedades do vetor
    procedure NotifyChange;
    // Retorna nome do vetor
    property Name     : String  read FName   write FName;
    // Nome do arquivo de saida
    property FileName : String  read FFileName write FFileName;
    // retorna tamanho do vetor
    property Len      : Integer read GetLength write SetLength;
    // Retorna elemento na posição especificada
    // Cuidado: Indice varia de 1 a Len
    property Data [Index: Integer] : Double Read Get Write Put; Default;
    // Retorna elemento como inteiro
    property AsInteger [Index: Integer] : Integer Read GetAsInteger;
    // Retorna elemento como srting
    property AsString  [Index: Integer] : String Read GetAsString;
    // Retorna vetor inteiro como string
    property VecAsString: String Read GetVecAsString;
    // Fonte dos dados quando impresso
    Property Font : TFont read GetFont;
  end;

{============================= TwsSFVec =========================}

  { Herança
      TwsSFVec --> TwsVec
    Objetivo
      Implementa um array de reais de 6 bytes
  }
  TwsSFVec = class(TwsVec)
  Private
    // array com os valores
    FData: Array of Single;
    // Atribui valor à posição especificada
    procedure Put(i: Integer; x: Double); override;
    // recupera valor da posição
    function  Get(i: Integer): Double; override;
    // REcupera valor como inteiro
    function  GetAsInteger(i: Integer): Integer; override;
    // Estabelece o tamanho do vetor
    procedure SetLength(aLen: Integer); override;
  Public
    // Espaço ocupado
    function SizeOfElements(): byte; override;
  end;

{============================= TwsDFVec =========================}

  { Herança
      TwsDFVec --> TwsVec
    Objetivo
      Implementa um array de reais de 8 bytes
  }
  TwsDFVec = class(TwsVec)
  Private
    // array com os valores
    FData: Array of Double;
    // Atribui valor na posição especificada
    procedure Put(i: Integer; x: Double); override;
    // Recupera valor na posição especificada
    function  Get(i: Integer): Double; override;
    // Recupera valor como inteiro
    function  GetAsInteger(i: Integer): Integer; override;
    // Cria espaço para o vetor
    procedure SetLength(aLen: Integer); override;
  Public

    function AsStringF(i,k: Integer): String;
    // Espaço ocupado
    function  SizeOfElements: byte; override;
    // Adiciona um valor ao vetor
    procedure Add(x: Double);
    // Operação com escalar
    function  ByScalar(const x:Double; Op:TwsEnumTypeOp; NewVec,SFirst:Boolean): TwsVec; override;
    // Acesso como srting
    property AsString[Index: Integer]: String Read GetAsString;
  end;

{============================= TwsSIVec =========================}

  { Herança
      TwsSIVec --> TwsVec
    Objetivo
      Implementa um array de inteiros de 2 bytes
  }
  TwsSIVec = class(TwsVec)
  Private
    // array com os elementos
    FData: Array of SmallInt;
    // Atribui valor à posição
    procedure Put(i: Integer; x: Double); override;
    // Recupera valor
    function  Get(i: Integer): Double; override;
    // Recupera valor como inteiro de 2 bytes
    function  GetI(i: Integer): SmallInt;
    // Atribui valor inteiro de 2 bytes
    procedure PutI(i: Integer; x: SmallInt);
    // Recupera como inteiro
    function  GetAsInteger(i: Integer): Integer; override;
    // Aloca espaço
    procedure SetLength(aLen: Integer); override;
    // Recupera valor como string
    function GetAsString(i: Integer): String; override;
  Public
    // Construtor otimizado para o tipo inteiro
    constructor Create(const Values: Array of SmallInt); overload;
    // Espaço ocupado
    function  SizeOfElements: byte; override;
    // Acesso a elemento
    // Cuidado: Indice varia de 1 a Len
    property Data [Index: Integer] : SmallInt Read GetI Write PutI; Default;
  end;

{================================= TwsLIVec ============================= }

  { Herança
      TwsLIVec --> TwsVec
    Objetivo
      Implementa um array de inteiros de 4 bytes
  }
  TwsLIVec = class(TwsVec)
  Private
    // array de armazenamento
    FData: Array of Integer;
    // Atribuição de valor real na posição desejada
    procedure Put(i: Integer; x: Double); override;
    // Recupera como real
    function  Get(i: Integer): Double; override;
    // Recupera como inteiro
    function  GetI(i: Integer): Integer;
    // atribui como inteiro
    procedure PutI(i: Integer; x: Integer);
    // Recupera como inteiro
    function  GetAsInteger(i: Integer): Integer; override;
    // Aloca espaço
    procedure SetLength(aLen: Integer); override;
    // Recupera valor como string
    function  GetAsString(i: Integer): String; override;
  Public
    // Construtor otimizado para o tipo inteiro
    constructor CreateFrom(const Values: Array of Integer);
    // Espaço ocupado
    function  SizeOfElements: byte; override;
    // Acesso a elemento
    // Cuidado: Indice varia de 1 a Len
    property  Data [Index: Integer] : Integer Read GetI Write PutI; Default;
  end;

type
  EImprDim = class(Exception);

const
  VFuncError = 700;
  NImprDim = VFuncError + 1;
  MImprDim = 'Dimensoes incompatíveis para operação';

  function  GetIndex(Const st1: String): TwsLIVec;
  function  Index(Lo, Hi: Integer): TwsLIVec;
  function  VecGenerate(Lo, Hi, Step: Double; Var ErrCode : Word): TwsVec;
  function  VecConst(Value: Double; L: Integer; VR: Boolean=True): TwsVec;
  function  StrToVec(P: PChar): TwsVec;
  function  StrVec(P: string): TwsDFVec;
  function  VecAppend(Var X: TwsVec; Y: TwsVec; NewVec:boolean; Amt: Integer): TwsVec; {Adriano} {08/12/1997}
  function  iVecAppend(Var X:TwsLIVec; Y: TwsLIVec; NewVec:boolean; Amt: Integer): TwsLIVec;
  function  VScalarAppend(y: TwsVec; x: Double; SFirst: Boolean): TwsVec; { ***** Amauri 02/05/97 }
  function  ScalarAppend(y, x: Double): TwsVec;        { ***** Amauri 02/05/97 }
  function  TermKron(C: TList): TwsVec;
  function  BivariateRanks(var x, y, rx, ry: TwsVec): TwsVec;
  Function wsIndexCols(Const Cols: String; Colunas: TStrings): TwsLIVec;

  { operacoes boleanas com vetores }
  function  BVecEQ(v1, v2: TwsVec): Double;
  function  BVecLT(v1, v2: TwsVec): Double;
  function  BVecLE(v1, v2: TwsVec): Double;
  function  BVecGT(v1, v2: TwsVec): Double;
  function  BVecGE(v1, v2: TwsVec): Double;
  function  BVecNE(v1, v2: TwsVec): Double;

  function  BVecScalarAllEQ(v: TwsVec; const x: Double): Double;

  { Todos os elementos sao iguais a x? }
  function  BVecScalarAllLT(v: TwsVec; const x: Double): Double;
  function  BVecScalarAllLE(v: TwsVec; const x: Double): Double;
  function  BVecScalarAllGT(v: TwsVec; const x: Double): Double;
  function  BVecScalarAllGE(v: TwsVec; const x: Double): Double;
  function  BVecScalarAllNE(v: TwsVec; const x: Double): Double;

  function  BVecScalarAnyEQ(v: TwsVec; const x: Double): Double;

  { Algum elemento e igual a x? }
  function  BVecScalarAnyLT(v: TwsVec; const x: Double): Double;
  function  BVecScalarAnyLE(v: TwsVec; const x: Double): Double;
  function  BVecScalarAnyGT(v: TwsVec; const x: Double): Double;
  function  BVecScalarAnyGE(v: TwsVec; const x: Double): Double;
  function  BVecScalarAnyNE(v: TwsVec; const x: Double): Double;
  function  VecMult(v1, v2: TwsVec;Var ErrCode: Word): Double;

  Function  VFromFile(Const Name: String): TwsVec;

  function FAmpVal(const StIni, StEnd, StDelt: String): TwsDFVec;
  function IAmpVal(const StIni, StEnd, StDelt: String): TwsLIVec;

  function VecCopy(V: TwsVec; Start, Amt: Longint): TwsVec;
  function VecScalarProd(y: TwsVec; const x: Double; NewVec: Boolean = False): TwsVec;
  function VecSum(v1, v2: TwsVec; NewVec: Boolean): TwsVec;
  function VecSub(v1, v2: TwsVec; NewVec: Boolean): TwsVec;
  function VecProd(v1, v2: TwsVec; NewVec: Boolean): TwsVec;

  Function PartialCorrelation(X, Y: TwsVec; BeginIndex, EndIndex: Integer): Double;

  function BinCo(n, k: Integer; F: TwsVec): Double;
  function Factrl(n: Integer; F: TwsVec; var FactNTop: Integer): Double;
  function FactLn(n: Integer; F: TwsVec): Double;

  function UnifValues(k,n,c: Integer): TwsDFVec;

  function QNorm(p: TwsVec): TwsDFVec;

  function ValNorm(n: Integer): TwsDFVec;

implementation
uses wsMath,
     wsVars,
     wsFuncoesDeEscalares,
     wsDistribuicoesRandomicas,
     wsProbabilidade;

{ ======================== Classe TwsVec - Inicio ====================== }

{ TwsVec }

constructor TwsVec.Create(ALen: Integer);
{ Objetivo
    Cria vetor
  Parâmetros
    ALen: número de elementos do vetor
  Campos alterados
    FName
    FLen
  Métodos chamados
    SetLength
}
begin
  inherited Create();
  SetLength(aLen);
  InitPrintOptions();
end;

destructor TwsVec.Destroy;
{ Objetivo
    Libera a memória alocada
  Campos liberados
    FName
  Métodos chamados
    Nenhum
}
begin
  if FFont <> nil then FFont.Free;
  inherited;
end;

function TwsVec.GetFont: TFont;
begin
  if FFont = nil then
     begin
     FFont := TFont.Create;
     FFont.Name := 'Arial';
     FFont.Size := 10;
     end;

  Result := FFont;
end;

function TwsVec.HasFont: Boolean;
begin
  Result := FFont <> nil;
end;

procedure TwsVec.NotificationAndFree;
begin
  GetMessageManager.SendMessage(WSM_REMOVER_OBJETO, [Self]); 
  Free;
end;

procedure TwsVec.ShellSort(Ascd: Boolean);
{ Objetivo
    Ordena vetor através do algoritmo ShellSort
  Parâmetros
    Ascd: true para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
    Nebhum
}
var
  Gap,i,j,jj: Integer;
begin
  Gap := Len;
  repeat
    Gap := Gap div 2;
    if Gap > 0 then
      begin
      for i := 1 to Len - Gap do
        begin
        j := i;
        while (j>=1) do
          begin
          jj := j + Gap;
          if (Compare(Data[j],Data[jj],Ascd)=1) then
            Exchange(j,jj)
          else
            j:=0;
          Dec(j,Gap);
          end { while }
        end { for }
      end { if }
  until Gap = 0
end; { ShellSort }

function TwsVec.ShellIndx(Ascd: Boolean):TwsLIVec;
{ Objetivo
    Ordena vetor pelo método ShellSort e retorna vetor de índices das trocas efetuadas. Este
    método é interessante quando outras estruturas necessitam sofrer as mesmas trocas realizadas
    no vetor.
  Parâmetros
    Ascd: True para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
    Exchange
}
var
  Gap,i,j,jj: Integer;
begin
  Result := Index(1, Len);
  Gap := Len;
  repeat
    Gap := Gap div 2;
    if Gap > 0 then
      begin
      for i := 1 to Len - Gap do
        begin
        j := i;
        while (j >= 1) do
          begin
          jj := j + Gap;
          if (Compare(Data[j], Data[jj], Ascd) = 1) then
            begin
            Exchange(j, jj);
            end
          else
            j := 0;
          Dec(j, Gap);
          end { while }
        end { for }
      end { if }
  until Gap = 0;
end; { ShellIndx }

procedure TwsVec.QuickSort(Ascd: Boolean=True);
{ Objetivo
    Ordena vetor através do algoritmo QuickSort
  Parâmetros
    Ascd: true para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
    Nenhum
}

  procedure partic(li,lf : Integer; var l : Integer);

  var l1,l2 : Integer;
      t : Double;
      esq : boolean;
  begin
    l1:=li;
    l2:=lf;
    t:= Data[l1];
    esq:=TRUE;
    while l1<l2 do
      if esq then
        if Compare(Data[l2], t, Ascd)<=0 then
          begin
            Data[l1] := Data[l2];
            inc(l1);
            esq:=FALSE
          end
        else dec(l2)
      else if Compare(Data[l1], t, Ascd)>=0 then
             begin
               Data[l2] := Data[l1];
               dec(l2);
               esq:=TRUE
             end
           else inc(l1);
    l:=l1;
    Data[l] := t
  end;

  procedure quick(li,lf : Integer);
  var
    l : Integer;
  begin
    if li<lf then
      begin
        partic(li,lf,l);
        quick(li,l-1);
        quick(l+1,lf)
      end
  end;

begin
  Quick(1, Len)
end;

function TwsVec.QuickIndx(Ascd: Boolean): TwsLIVec;
{ Objetivo
    Ordena vetor pelo método QuickSort e retorna vetor de índices das trocas efetuadas. Este
    método é interessante quando outras estruturas necessitam sofrer as mesmas trocas realizadas
    no vetor.
  Parâmetros
    Ascd: True para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
    Exchange
}

  procedure partic(li,lf : Integer; var l : Integer);
  var
    l1,l2   : Integer;
    t       : Double;
    esq     : boolean;
  begin
    l1:=li;
    l2:=lf;
    t:=Data[l1];
    esq:=TRUE;
    while l1<l2 do
      if esq then
        if compare(Data[l2],t,Ascd)<=0 then begin
          Data[l1] := Data[l2];
          Result.Exchange(l1,l2);
          inc(l1);
          esq:=FALSE;
        end
        else
          dec(l2)
      else
        if compare(Data[l1],t,Ascd)>=0 then begin
          Data[l2] := Data[l1];
          Result.Exchange(l1,l2);
          dec(l2);
          esq:=TRUE;
        end
        else
          inc(l1);
    l:=l1;
    Data[l] := t;
  end;

  procedure quick(li,lf : Integer);
  var
    l : Integer;
  begin
    if li<lf then
      begin
        partic(li,lf,l);
        quick(li,l-1);
        quick(l+1,lf)
      end
  end;

begin
  Result := Index(1, Len);
  Quick(1, Len);
end;

procedure TwsVec.NRQuick(Ascd: Boolean);
{ Objetivo
    Ordena vetor através do algoritmo QuickSort não recursivo. Se o numero de elementos for menor
    que 10 o algoritmo utilizado é o da Insercao
  Parâmetros
    Ascd: true para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
    Nenhum
}
label 10;
const
  m = 10;
  nstack = 150;
var
  i,j,k,l,ir,jstack: Integer;
  a                : Double;
  istack           : TwsVec;
begin
  istack := TwsDFVec.Create(nstack);
  jstack := 0;
  l := 1;
  ir := Len;
  while True do
    begin
    if ir-l < m then
      begin
      for j := l+1 to ir do
        begin
        a := Data[j];
        for i := j-1 downto 1 do
          begin
          if (Compare(Data[i], a, Ascd) <= 0) then GoTo 10;
          Data[i+1] := Data[i]
          end; { for }
        i := 0;
10:     Data[i+1] := a
        end; { for }
      if jstack = 0 then
        begin
        istack.Free;
        Exit
        end;
      ir := istack.AsInteger[jstack];
      l := istack.AsInteger[jstack-1];
      Dec(jstack, 2)
      end
    else
      begin
      k := (l+ir) div 2;
      Exchange(k, l+1);
      if (Compare(Data[l+1], Data[ir], Ascd) = 1) then Exchange(l+1, ir);
      if (Compare(Data[l], Data[ir], Ascd) = 1) then Exchange(l, ir);
      if (Compare(Data[l+1], Data[l], Ascd) = 1) then Exchange(l, l+1);
      i := l+1;
      j := ir;
      a := Data[l];
      while True do
        begin
        repeat
          Inc(i)
        until (Compare(Data[i], a, Ascd) >= 0);
        repeat
          Dec(j)
        until (Compare(Data[j], a, Ascd) <= 0);
        if j < i then Break;
        Exchange(l, j)
        end; { while }
      Data[l] := Data[j];
      Data[j] := a;
      Inc(jstack, 2);
      if (ir-i+1) >= (j-l) then
        begin
        istack.Data[jstack] := ir;
        istack.Data[jstack-1] := i;
        ir := j-1
        end
      else
        begin
        istack.Data[jstack] := j-1;
        istack.Data[jstack-1] := l;
        l := i
        end { if }
      end { if }
    end; { while }
end;

procedure TwsVec.HeapSort(Ascd: Boolean);
{ Objetivo
    Ordena vetor através do algoritmo HeapSort
  Parâmetros
    Ascd: true para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
    Nenhum
}
var
  l,j,ir,i: Integer;
  rra     : Double;
begin
  l := (Len div 2) + 1;
  ir := Len;
  while True do
    begin
    if l > 1 then
      begin
      Dec(l);
      rra := Data[l]
      end
    else
      begin
      rra := Data[ir];
      Data[ir] := Data[1];
      Dec(ir);
      if ir = 1 then
        begin
        Data[1] := rra;
        Exit
        end
      end; { if }
    i := l;
    j := l + l;
    while j <= ir do
      begin
      if j < ir then
        if (Compare(Data[j], Data[j+1], Ascd) = -1) then
          Inc(j);
      if (Compare(rra, Data[j], Ascd) = -1) then
        begin
        Data[i] := Data[j];
        i := j;
        Inc(j, j)
        end { if }
      else
        j := ir + 1
      end; { while }
    Data[i] := rra
    end; { while }
end;

function TwsVec.Sorted(var i: Integer; Ascd: Boolean): Boolean;
{ Objetivo
    Busca primeiro par não ordenado por meio de uma busca sequencial
  Parâmetros
    i: Se houver par não ordenado, eles serão i e i+1
    Ascd: true para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
    Nenhum
}
begin
  Result := True;
  i := 1;
  Ascd := not Ascd;
  while Result and (i < Len) do
    begin
    Result := (Compare(Data[i], Data[i+1], Ascd) >= 0);
    Inc(i);
    end
end;

procedure TwsVec.Print(Buffer: TStrings);
{ Objetivo
    Imprimir vetor no modo texto
  Campos alterados
    Nenhum
  Métodos chamados
    Nenhum
}
var
  i,j,PrevI,NumCol: Integer;
begin
  If Len<>0 Then
      Begin
      i := 0;
      repeat                          { Imprime tantos valores quanto possivel }
        PrevI := i;
        NumCol := 0;
        j := PrevI + 1;
        repeat                              { Quantas valores serao impressos? }
          Inc(j);
          Inc(i);
          Inc(NumCol);
        until (j > Len) or (NumCol*PrintOptions.ColWidth >= (PrintOptions.LineLen-IDSize));
        if (PrintOptions.LineLen-IDSize < NumCol*PrintOptions.ColWidth) And (j<=Len) then
          begin
          Dec(NumCol);
          Dec(i)
          end;
        { Constroi linha de tamanho MaxLen ou esgotando colunas para saida }
        ToChar(PrevI + 1, NumCol, Buffer);
        Buffer.Add('');
      until i = Len;                               { Esgota todas as colunas }
      End; { If Len<>0 }
  Buffer.Add('')
end; { TwsVec.Print }

function TwsVec.ToChar(Start, Amt: Integer; Buffer: TStrings): string;
{ Objetivo
    Coloca vetor num string
  Parâmetros
    Start: elemento inicial
    Amt: elementos a copiar
}
var
  j   : Integer;
  x   : Double;
  Indx: String;
begin
  if (Start <= Len) and (PrintOptions.ColWidth*Amt <= PrintOptions.LineLen) then
    begin
    Amt := Math.Min(Amt, Len - Start + 1);

    if FName <> '' then
      begin
      Result := LeftPad(FName, IDSize);
      Indx := LeftPad(' ', IDSize)
      end
    else
      begin
      Result := '';
      Indx := ''
      end;

    for j := 0 to Amt-1 do
      begin
      AppendStr(Indx, Format('%*s', [PrintOptions.ColWidth, IntToStr(Start + j)]));

      if not IsMissValue(Start + j, x) then
        AppendStr(Result, Format('%*.*g',[PrintOptions.ColWidth, PrintOptions.ColPrecision, Fuzz(x)]))
      else
        AppendStr(Result, Format('%*s',[PrintOptions.ColWidth, '.']))
      end
    end;

  if PrintOptions.Center then
     begin
     Buffer.Add(StrCenter(Indx, PrintOptions.LineLen));
     Buffer.Add(StrCenter(Result, PrintOptions.LineLen));
     end
  else
     begin
     Buffer.Add(Indx);
     Buffer.Add(Result);
     end;
end; { TwsVec.ToChar }

    // Retorna vetor inteiro como string;
function TwsVec.GetVecAsString: String;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to FLen do
    Result:=Result+getAsString(i)+' ';
  System.Delete(Result,Length(Result),1)
end;


function TwsVec.IsMissValue(Index: Integer; out x: Double): Boolean;
{ Objetivo
    Verifica se valor é perdido
  Parâmetros
    Index: Índice do elemento
    x: retorna o elemento
  Campos alterados
    Nenhum
  Métodos chamados
    Nenhum
}
begin
  x := self[Index];
  Result:=wsGLib.IsMissValue(x)
end;

Class Function TwsVec.LoadFromFile(Const Name: String): TwsVec;
{ Objetivo
    Carrega vetor do disco
  Parâmetros
    Name: nome do arquivo
}
Var Stream  : TFileStream;
    Reader  : TReader;
Begin
  Stream := TFileStream.Create(Name, fmOpenRead);
  Try
    Reader := TReader.Create(Stream, $FF);
    Try
      Reader := TReader.Create(Stream, $FF);
      Result := VFromStream(Reader);
      Result.FileName := Name;
    Finally
      Reader.Free;
    End;
  Finally
    Stream.Free;
  End; { Try }
End;

Procedure TwsVec.SaveToFile(Const FileName: String);
{ Objetivo
    Gravar o vetor num arquivo com o nome especificado.
  Parâmetros
    Name: Nome do arquivo onde o vetor será armazenado
}
Var
  Stream :TFileStream;
  Writer :TWriter;
  st     : string;
Begin
  st:=FileName;
  if ExtractFileExt(st)='' then
    st:=st+'.vet';
  Stream := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  Try
    Writer:=TWriter.Create(Stream,$FF);
    Try
      SaveToStream(Writer);
    Finally
      Writer.Free;
    End; { Try }
  Finally
    Stream.Free;
  End; { Try }
  FModified := False;
End; { TwsVec.SaveToFile }


procedure TwsVec.Fill(Value: Double);
{ Objetivo
    Preencher vetor com valor constante
  Parâmetros
    x: valor a preencher
}
var i: Integer;
begin
  For i := 1 to FLen do Data[i] := Value;
end;

Procedure TwsVec.FromText(Var Arq:Text);
{ Objetivo
    Recupera vetor de um arquivo texto
  Parâmetros
    Arq: arquivo texto
}
Var
  Name : String;
  i    : Integer;
Begin
  ReadLn(Arq, FName);
  i:=1;
  While (i<=Len) And (Not EOLn(Arq)) Do
    Begin
    Data[i]:=StrToFloat(LeDado(Arq));
    Inc(i);
    End;
End; { TwsVec.FromText }

Procedure TwsVec.ToText(Var Arq: Text; Precision: Byte);
{ Objetivo
    Escreve vetor num arquivo texto
  Parâmetros
    Arq: arquivo texto para escrita
    Precision: precisão de escrita (dígitos significativos)
  Observação
    Valores são separados por um ponto-e-vírgula
}
Const
  SEP =';';
Var
   i  :Integer;
Begin
  WriteLn(Arq, FLen, SEP, FName);
  If Len >= 1 Then Write(Arq, FloatToStrF(Data[1],ffGeneral,Precision,4));
  i:=2;

  While (i<=Len) And (Len<>1) Do
    Begin
    Write(Arq,SEP,FloatToStrF(Data[i],ffGeneral,Precision,4));
    inc(i);
    End;

  If Len>0 Then Writeln(Arq);
End; { TwsVec.ToText }

function TwsVec.All(const x: Double; Op: TwsEnumTypeOp): boolean;
{ Objetivo
    Verifica se todos os elementos do vetor atendem ao operador de comparação
  Parâmetros
    x: Valor a ser comparado
    Op: Operador de comparação
}
var
  i: Integer;
begin
  case Op of
    opEQ:
      for i:=1 to Len do
        begin
        Result:=(ScalarEQ(x,Data[i])=ScalarTrue);
        if not Result then Exit
        end;
    opGE:
      for i:=1 to Len do
        begin
        Result:=(ScalarGE(x,Data[i])=ScalarTrue);
        if not Result then Exit
        end;
    opGT:
      for i:=1 to Len do
        begin
        Result:=(ScalarGT(x,Data[i])=ScalarTrue);
        if not Result then Exit
        end;
    opLE:
      for i:=1 to Len do
        begin
        Result:=(ScalarLE(x,Data[i])=ScalarTrue);
        if not Result then Exit
        end;
    opLT:
      for i:=1 to Len do
        begin
        Result:=(ScalarLT(x,Data[i])=ScalarTrue);
        if not Result then Exit
        end;
    opNE:
      for i:=1 to Len do
        begin
        Result:=(ScalarNE(x,Data[i])=ScalarTrue);
        if not Result then Exit
        end;
  end; // case
end; { All }

function TwsVec.Any(const x: Double; Op: TwsEnumTypeOp): boolean;
{ Objetivo
    Verifica se algum elemento do vetor atende ao operador de comparação
  Parâmetros
    x: Valor a ser comparado
    Op: Operador de comparação
}
var
  i: Integer;
begin
  case Op of
    opEQ:
      for i:=1 to Len do
        begin
        Result:=(ScalarEQ(x,Data[i])=ScalarTrue);
        if Result then Exit
        end;
    opGE:
      for i:=1 to Len do
        begin
        Result:=(ScalarGE(x,Data[i])=ScalarTrue);
        if Result then Exit
        end;
    opGT:
      for i:=1 to Len do
        begin
        Result:=(ScalarGT(x,Data[i])=ScalarTrue);
        if Result then Exit
        end;
    opLE:
      for i:=1 to Len do
        begin
        Result:=(ScalarLE(x,Data[i])=ScalarTrue);
        if Result then Exit
        end;
    opLT:
      for i:=1 to Len do
        begin
        Result:=(ScalarLT(x,Data[i])=ScalarTrue);
        if Result then Exit
        end;
    opNE:
      for i:=1 to Len do
        begin
        Result:=(ScalarNE(x,Data[i])=ScalarTrue);
        if Result then Exit
        end;
  end; // case
end; { Any }

Class Function TwsVec.VFromStream(Var Reader: TReader):TwsVec;
{ Objetivo
    Recupera vetor do disco
  Parâmetros
    Reader: objeto de leitura
}
Var NElem: Integer;
Begin
  Reader.ReadSignature;
  NElem  := Reader.ReadInteger;
  Result := TwsDFVec.Create(NElem);
  Result.LoadFromStream(Reader);
End; { TwsVec.VFromStream }

procedure TwsVec.Append(F: PFArray; Amt: Integer);
{ Objetivo
    Concatena valores no vetor
  Parâmetros
    F: endereço da sequência de valores
    Amt: número de valores a concatenar
}
var i, ii: Integer;
begin
  ii := FLen;
  inc(FLen, amt);

  SetLength(FLen);

  For i := (ii + 1) To (ii + Amt) Do
    Data[i] := F^[i - ii];
end;

procedure TwsVec.Insert(F: TwsVec; Start, Amt: Integer);
{ Objetivo
    Insere valores no vetor
  Parâmetros
    F: vetor de valores
    Start: posição inicial
    Amt: número de valores a inserir
}
var i,oldFim:  Integer;
begin
  Dec(Start);
  oldFim := FLen - 1;
  inc(FLen, amt);

  SetLength(FLen);

  For i := oldFim To Start Do
    Data[i + amt] := Data[i];

  For i := Start To (Start + amt - 1) Do
    Data[i] := F[i - Start + 1];
end;

{ Objetivo
    Elimina valores do vetor
  Parâmetros
    Start: índice inicial
    Amt: número de valores a eliminar
}
procedure TwsVec.Delete(Start, Amt: LongWord);
var i: Integer;
begin
  If (Start > FLen) then Exit;

  if (Start + Amt - 1) >= FLen then
     // Remocao dos últimos elementos
     // Basta somente redimensionar o vetor
     FLen := Start - 1
  else
     begin
     for i := (Start + amt) to FLen do
       Data[i - amt] := Data[i];

     Dec(FLen, amt);
     end;

  SetLength(FLen);
end;

procedure TwsVec.StrToLine(P: PChar);
{ Objetivo
    Transforma o conteúdo de um string em elementos do vetor
  Parâmetros
    P: String. Os caracteres separadores dos elementos são #9,#10,#13,' ',',','=','\',
       '"','(',')'
}
const
  BufSize = 150;
var
  j: Integer;
  i: Integer;
  S: String;
  F: PFArray;
begin
  GetMem(F, sf(BufSize));
  j := 0;
  i := 0;
  repeat
    Inc(i);
    S := StrGet(P, j, DelChar);
    if S <> '' then F^[i] := FracToReal(S);
    if (i = BufSize) or (S = '') then
       begin
       Append(F, i-1);
       i := 0
       end
  until (S = '');
  FreeMem(F, sf(BufSize))
end; { StrToLine }

procedure TwsVec.LoadFromStrings(SL: TStrings);
var j: Cardinal;
    i, k: Integer;
    s: String;
begin
  k := 0;
  for i := 0 to SL.Count-1 do inc(k, WordCount(SL[i], [' ', ';', #9]));
  SetLength(k);

  k := 0;
  for i := 0 to SL.Count-1 do
    begin
    j := 1;
    repeat
      S := StrToken(SL[i], j, [' ', ';', #9]);
      if S <> '' then
         begin
         Inc(k);
         if k <= FLen then
         {$ifdef prj_WinStat}
            Data[k] := FracToReal(S);
         {$else}
            Data[k] := StrToFloatDef(S, wscMissValue);
         {$endif}
         end;
    until (S = '');
    end;
end;

Procedure TwsVec.LoadFromStream(Var Reader: TReader);
{ Objetivo
    Recupera conteúdo do disco
  Parâmetros
    Reader: objeto de leitura
}
Var i: Integer;
    x: Double;
Begin
  Name := Reader.ReadString;
  For i := 1 To Len Do
    begin
    Reader.Read(x, SizeOf(Double));
    Data[i] := x;
    end;
End;

Procedure TwsVec.SaveToStream(Var Writer: TWriter);
{ Objetivo
    Grava conteúdo no disco
  Parâmetros
    Writer: objeto de escrita
}
Var i: Integer;
    x: Double;
Begin
  Writer.WriteSignature;
  Writer.WriteInteger(Len);
  Writer.WriteString(Name);
  For i := 1 To FLen Do
    begin
    x := Data[i];
    Writer.Write(x, SizeOf(Double));
    end;
End;

procedure TwsVec.Exchange(i, j: Integer);
{ Objetivo
    Troca o conteúdo das posições especificadas
  Parâmetros
    i, j: índices dos elementos que serão trocados
}
var x: Double;
begin
  x := Data[i];
  Data[i] := Data[j];
  Data[j] := x;
end;

procedure TwsVec.ToEditor(Buffer: TStrings; ValoresPorLinha: Cardinal = 12; Tamanho: byte = 10);
{ Objetivo
    Transfere conteúdo do vetor para o objeto gerenciador de saída
  Parâmetros
    ValoresPorLinha: quantos elementos serão escritos
    Tamanho: largura de impressão de cada elemento
    SL: Lista de strings com rótulos para os elementos
}
var i: Integer;
    s: String;
begin
  s := '';
  for i := 1 to FLen do
    begin
    s := s + RightStr(FloatToStr(Data[i]), Tamanho) + ' ';
    if (i mod ValoresPorLinha) = 0 then
       begin
       Buffer.Add(s);
       s := '';
       if i = FLen then exit;
       end;
    end;

  Buffer.Add(s)
end;

function TwsVec.ByElement(x:TwsVec; Op:TwsEnumTypeOp; NewVec:Boolean; Var ErrCode:Word):TwsVec;
{ Objetivo
    Efetuar operacoes emtre um escalar e os elementos do vetor
  Parâmetros
    x: Vetor para operação
    Op: Operação desejada
    NewVec: True se um novo vetor será criado; False se o resultado será armzenado no
      próprio vetor
  ErrCode: Código de erro. Retorna 0 se Len=x.Len e NImprDim (dimensões impróprias para a
    operação, caso contrário.
  Retorno
    Se NewVec=True retorna a operação num novo vetor; caso contrário no próprio vetor. Se
    ErrCode retorna NImprDim, o retorno será nil.
}
var
  i: Integer;
begin
  ErrCode:=0;
  if (Len=x.Len) then
    begin
    if NewVec then
      Result:=TwsDFVec.Create(FLen)
    else
      Result:=Self;
    case Op of
      opSum:            // Soma
        for i:=1 to FLen do
          Result[i]:=ScalarSum(Self[i],x[i]);
      opSub:            // Subtracao
        for i:=1 to FLen do
          Result[i]:=ScalarSub(Self[i],x[i]);
      opDiv:            // Divisao
        for i:=1 to FLen do
          Result[i]:=ScalarDiv(Self[i],x[i]);
      opProd:           // Produto
        for i:=1 to FLen do
          Result[i]:=ScalarProd(Self[i],x[i]);
      opPower:          // Potência
        for i:=1 to FLen do
          Result[i]:=ScalarPower(Self[i],x[i]);
      opGE:            // Maior ou igual
        for i:=1 to FLen do
          Result[i]:=ScalarGE(Self[i],x[i]);
      opGT:            // Maior que
        for i:=1 to FLen do
          Result[i]:=ScalarGT(Self[i],x[i]);
      opLE:           // Menor ou igual
        for i:=1 to FLen do
          Result[i]:=ScalarLE(Self[i],x[i]);
      opLT:          // Menor que
        for i:=1 to FLen do
          Result[i]:=ScalarLT(Self[i],x[i]);
      opEQ:         // Igual
        for i:=1 to FLen do
          Result[i]:=ScalarEQ(Self[i],x[i]);
      opNE:         // Diferente
        for i:=1 to FLen do
          Result[i]:=ScalarNE(Self[i],x[i]);
      opOR:       // OU
        for i:=1 to FLen do
          Result[i]:=ScalarOR(Self[i],x[i]);
      opAnd:     // E
        for i:=1 to FLen do
          Result[i]:=ScalarAnd(Self[i],x[i]);
      opMax:     // Maior valor
        for i:=1 to FLen do
          Result[i]:=ScalarMax(Self[i],x[i]);
      opMin:    // Menor valor
        for i:=1 to FLen do
          Result[i]:=ScalarMin(Self[i],x[i]);
    end; // case
    end // if
  else
    begin
    ErrCode:=NImprDim;
    Result:=nil
    end
end; // ByElement

function TwsVec.Func(fOp: TwsEnumConstFun; NewVec: Boolean): TwsVec;
{ Objetivo
    Aplica uma função ao vetor
  Parâmetros
    fOp: Função desejada. As possibilidades são:
      cABS - valor absoluto
      cEXP - exponencial
      cAPROXIMA - Aproxima valores dentro de um limite pré-estabelecido
      cINT - Parte inteira do valor
      cLN - Logaritmo neperiano
      cRAIZ - Raiz quadrada
      cARCTAN - Arco tangente
      cARCSEN - Arco seno
      cARCCOS - Arco cosseno
      cSEN - Seno
      cCOS - Cosseno
      cSENH - Seno hiperbólico
      cCOSH - Cosseno hiperbólico
      cTAN - Tangente
      cLOG - Logaritmo decimal
      cANG - Transformação angular
      cLGAMA - Logaritmo da função gama
      cTriGAMA - Trigama (derivada da digama)
      cFLOOR - Maior inteiro
      cCEIL - Menor inteiro
      cINV - Inverso do valor
      cFRAC - Parte fracionária
      cTANH - Tangente hiperbólica
    NewVec: True se o retorno se dará em outro vetor; False se o retorno será no vetor que
      chama.
  Valores perdidos
    Funções aplicadas a valores impróprios ou perdidos retornam valores perdidos
}
var
  i: Integer;
begin
  if NewVec then
    Result := TwsDFVec.Create(Len)
  else
    Result := Self;
  case fOp of
      cABS: // valor absoluto
        for i := 1 to Len do
          Result[i] := ScalarAbs(Self[i]);
      cEXP: // exponencial
        for i := 1 to Len do
          Result[i] := ScalarExp(Self[i]);
      cAPROXIMA: // Aproxima valores dentro de um limite pré-estabelecido
        for i := 1 to Len do
          Result[i] := ScalarFuzz(Self[i]);
      cINT: // Parte inteira do valor
        for i := 1 to Len do
          Result[i] := ScalarInt(Self[i]);
      cLN: // Logaritmo neperiano
        for i := 1 to Len do
          Result[i] := ScalarLn(Self[i]);
      cRAIZ: // Raiz quadrada
        for i := 1 to Len do
          Result[i] := ScalarSqrt(Self[i]);
      cARCTAN: // Arco tangente
        for i := 1 to Len do
          Result[i] := ScalarArcTan(Self[i]);
      cARCSEN: // Arco seno
        for i := 1 to Len do
          Result[i] := ScalarArcSin(Self[i]);
      cARCCOS: // Arco cosseno
        for i := 1 to Len do
          Result[i] := ScalarArcCos(Self[i]);
      cSEN: // Seno
        for i := 1 to Len do
          Result[i] := ScalarSin(Self[i]);
      cCOS: // Cosseno
        for i := 1 to Len do
          Result[i] := ScalarCos(Self[i]);
      cSENH: // Seno hiperbólico
        for i := 1 to Len do
          Result[i] := ScalarSinH(Self[i]);
      cCOSH: // Cosseno hiperbólico
        for i := 1 to Len do
          Result[i] := ScalarCosH(Self[i]);
      cTAN: // Tangente
        for i := 1 to Len do
          Result[i] := ScalarTan(Self[i]);
      cLOG: // Logaritmo decimal
        for i := 1 to Len do
          Result[i] := ScalarLog(Self[i]);
      cANG: // Transformação angular
        for i := 1 to Len do
          Result[i] := ScalarAng(Self[i]);
      cLGAMA: // Logaritmo da função gama
        for i := 1 to Len do
          Result[i] := ScalarLnGamma(Self[i]);
      cTGAMA: // função trigama
        for i := 1 to Len do
          Result[i] := ScalarTriGamma(Self[i]);
      cFLOOR: // Maior inteiro
        for i := 1 to Len do
          Result[i] := ScalarFloor(Self[i]);
      cCEIL: // Menor inteiro
        for i := 1 to Len do
          Result[i] := ScalarCeil(Self[i]);
      cINV: // Inverso do valor
        for i := 1 to Len do
          Result[i] := ScalarInv(Self[i]);
      cFRAC: // Parte fracionária
        for i := 1 to Len do
          Result[i] := ScalarFrac(Self[i]);
      cTANH: // Tangente hiperbólica
        for i := 1 to Len do
          Result[i] := ScalarTanH(Self[i]);
    end; // case fOp
end; // Func

procedure TwsVec.ToXML(Buffer: TStrings; Ident: Integer);
var s, sIdent: String;
    i: integer;
begin
  sIdent := StringOfChar(' ', Ident);

  if FName = '' then
     s := '<Vector>'
  else
     s := '<Vector Name="' + FName + '">';

  s := sIdent + s;
  for i := 1 to Len do
    s := s + '<e>' + SysUtilsEx.AllTrim(getAsString(i)) + '</e>';
  s := s +  '</Vector>';

  Buffer.Add(s);
end;

// Extremamente ineficiente, evite o uso !!!
procedure TwsVec.Add(const x: Double);
begin
  SetLength(FLen + 1);
  Data[FLen-1] := x;
end;

procedure TwsVec.Assign(v: TwsVec);
var i: Integer;
begin
  SetLength(v.Len);
  for i := 0 to FLen-1 do
    Data[i] := v.Data[i];
end;

function TwsVec.GetLength: Integer;
begin
  Result := FLen;
end;

function TwsVec.GetClassName: String;
begin
  Result := self.ClassName;
end;

function TwsVec.GetBlockName: String;
begin
  Result := 'wsVector';
end;

function TwsVec.GetBlockNameComment: String;
begin
  Result := 'Biblioteca para Renderização de Vetores'
end;

procedure TwsVec.ToBXML(Buffer: TStrings; Ident: Integer);
var s: String;
begin
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<' + GetBlockName + ':block>');
  inc(Ident, 2); ToXML(Buffer, Ident);
  Buffer.Add(s + '</' + GetBlockName + ':block>');
end;

procedure TwsVec.NotifyChange;
begin
  getMessageManager.SendMessage(wsm_VectorChange, [self]);
end;

function TwsVec.Clone(): TwsVec;
{ Objetivo
    Gera uma copia do vetor (dados e atributos)
    O resultado sempre será um vetor do tipo da classe que está sendo utilizada.
}
var i: Integer;
begin
  Result := TwsVec(ClassType.NewInstance).Create(self.Len);
  Result.Name := self.Name;
  for i := 1 to self.Len do
    Result.Data[i] := self.Data[i];
end;

function TwsVec.Copy(Start, Amt: Integer): TwsVec;
{ Objetivo
    Gera uma copia dos dados do vetor sem copiar o nome.
    O resultado poderá ser Nil e se não for será um vetor do tipo da
    classe que está sendo utilizada.
  Parâmetros
    Start: Índice inicial para cópia
    Amt: Quantidades de elementos a serem copiados
  Obs.:
  Retorna Nil se Amt < 1 ou Start < 1 ou Start > V.Len
}
var
  i: Integer;
begin
  if Start < 1 then
     begin
     Result := Nil;
     exit;
     end;

  Amt := Math.Min(Amt, Succ(Len - Start));

  if Amt <= 0 then
     begin
     Result := Nil;
     exit;
     end;

  Result := TwsVec(ClassType.NewInstance).Create(Amt);
  Result.Name := Name;
  for i := 0 to Amt-1 do
    Result.Data[i+1] := Self.Data[Start+i];
end; { Copy }

{$ifdef MSXML}
procedure TwsVec.fromXML(no: IXMLDomNode);
var i: Integer;
begin
  self.SetLength(no.childNodes.length);
  self.FName := no.attributes.item[0].text;
  for i := 0 to self.Len-1 do
    self.Data[i+1] := toFloat(no.childNodes.item[i].text);
end;
{$endif MSXML}

function TwsVec.a_getAsFloat(i: integer): double;
begin
  result := self.Get(i);
end;

function TwsVec.a_getAsInteger(i: integer): integer;
begin
  result := self.GetAsInteger(i);
end;

function TwsVec.a_getAsString(i: integer): string;
begin
  result := self.GetAsString(i);
end;

function TwsVec.a_High(): integer;
begin
  result := self.GetLength();
end;

function TwsVec.a_Low(): integer;
begin
  result := 1;
end;

procedure TwsVec.a_setAsFloat(i: integer; value: double);
begin
  self.Put(i, value);
end;

procedure TwsVec.a_setAsInteger(i, value: integer);
begin
  self.Put(i, value);
end;

procedure TwsVec.a_setAsString(i: integer; value: string);
begin
  self.Put(i, StrToFloatDef(value, wscMissValue));
end;

function TwsVec.a_IsMissValue(i: integer; out x: double): boolean;
begin
  result := self.IsMissValue(i, x);
end;
constructor TwsVec.Create(const Items: string);
var sl: TStrings;
     i: integer;
begin
  sl := nil;
  Split(Items, sl, [#9, ';', ' ']);
  setLength(sl.Count);

  if (self is TwsSFVec) or (self is TwsDFVec) then
     for i := 0 to sl.Count-1 do
       Data[i+1] := toFloat(sl[i], wscMissValue)
  else
     for i := 0 to sl.Count-1 do
       Data[i+1] := toInt(sl[i], -1);

  sl.Free();
  InitPrintOptions();
end;

procedure TwsVec.InitPrintOptions();
begin
  PrintOptions.LineLen := 400;
  PrintOptions.MaxIDSize := 8;
  PrintOptions.Center := False;
  PrintOptions.ColPrecision := 10;
  PrintOptions.ColWidth := PrintOptions.ColPrecision+3;
end;

constructor TwsVec.Create(const Values: array of Double);
var i: integer;
begin
  setLength( System.Length(Values) );

  for i := 0 to High(Values) do
    Data[i+1] := Values[i];

  InitPrintOptions();
end;
{ TwsDFVec }

function TwsVec.Kronecker(Y: TwsVec): TwsVec;
{ Objetivo
    Obtém o produto de Kronecker entre dois vetores
  Parâmetros
    Y: Vetor com o qual será realizado o produto de Kronecker
}
var
  i, j, k: Integer;
  aux: Double;
begin
  Result := TwsDFVec.Create(Len * Y.Len);
  k := 0;
  for i := 1 to Len do
    begin
    aux:=Self[i];
    for j := 1 to Y.Len do
      begin
      Inc(k);
      Result[k] := ScalarProd(aux, Y[j])
      end
    end
end; { Kronecker }

procedure TwsVec.Locate(x: Double; var Ind: Integer);
{ Objetivo
    Retorna uma posição no vetor relativa ao um valor especificado
  Parâmetros
    x: Valor a pesquisar
    Ind: Índice relativo ao valor pesquisado. Ind é um valor tal que x esta entre y[Ind] e y[Ind+1]. Se Ind
         retornar 0 ou Len entao x esta fora da amplitude.
  Observações
    Faz uma pesquisa binaria em um vetor ordenado
}
var
  ju, jm, jl: Integer;
  Ascd: Boolean;
begin
  Ascd := Self[FLen] > Self[1];
  jl := 0;
  ju := FLen + 1;
  while (ju - jl) > 1 do
    begin
    jm := (ju + jl) div 2;
    if (x > Self[jm]) = Ascd then
      jl := jm
    else
      ju := jm
    end; { while }
  Ind := jl
end;

function TwsVec.SeqSearch(x: Double; var Ind: Integer): boolean;
{ Objetivo
    Faz uma pesquisa sequencial para verificar se um valor está no vetor. Se estiver,
    retorna a posição no vetor relativa ao um valor especificado
  Parâmetros
    x: Valor a pesquisar
    Ind: Índice relativo ao valor pesquisado. Se o valor não estiver no vetor Ind
    retorna -1.
  Observações
    Faz uma pesquisa sequencial num vetor qualquer
}
var
  i: Integer;
begin
  Result:=False;
  Ind:=-1;
  for i:=1 to FLen do
    if FEquals(x,Data[i]) then
      begin
      Result:=True;
      Ind:=i;
      Break
      end
end;

procedure TwsVec.Hunt(x: Double; var jlo: Integer);
{ Objetivo
    Retorna uma posição no vetor relativa ao um valor especificado
  Parâmetros
    x: Valor a pesquisar
    Ind: Índice relativo ao valor pesquisado. Ind é um valor tal que x esta entre y[jlo] e y[jlo1]. Se Ind
         retornar 0 ou Len entao x esta fora da amplitude.
  Observações
    Faz uma pesquisa binaria
}
var
  jm, jhi, ic: Integer;
  Ascd: Boolean;
label
  1;

procedure FinalPhase;
begin
  while (jhi - jlo) <> 1 do
    begin
    jm := (jhi + jlo) div 2;
    if (x > Self[jm]) = Ascd then
      jlo := jm
    else
      jhi := jm
    end { while }
end; { FinalPhase }

begin
  Ascd := Self[FLen] > Self[1];
  if (jlo = 0) or (jlo > FLen) then
    begin
    jlo := 0;
    jhi := FLen + 1;
    end
  else
    begin
    ic := 1;
    if (x >= Self[jlo]) = Ascd then begin
      if jlo = FLen then Exit;
      jhi := jlo + 1;
      while (x >= Self[jhi]) = Ascd do
        begin
        jlo := jhi;
        Inc(ic, ic);
        jhi := jlo + ic;
        if jhi > FLen then
          begin
          jhi := FLen + 1;
          Goto 1;
          end { if }
        end { while }
      end
    else
      begin
      if jlo = 1 then
        begin
        jlo := 0;
        Exit
        end; { if }
      jhi := jlo;
      jlo := jhi - 1;
      while (x < Self[jlo]) = Ascd do
        begin
        jhi := jlo;
        Inc(ic, ic);
        jlo := jhi - ic;
        if jlo < 1 then
          begin
          jlo := 0;
          Goto 1
          end { if }
        end { while }
      end { if }
    end; { if }
1:FinalPhase
end;

function TwsVec.IndMinOrMax(IsMin: Boolean; var Ind: Integer): Double;
{ Objetivo
    Retorna o indice e o valor do menor ou do maior valor de um vetor
  Parâmetros
    y: Vetor para obtenção dos valores
    IsMin: Se True o mínimo será encontrado, se False o máximo
    Ind: Índice do valor
  Observação
    Faz uma busca sequencial
}
var
  i: Integer;
begin
  Result := Self[1];
  Ind := 1;
  if IsMin then
    begin
    for i := 2 to FLen do
      if Self[i] < Result then
        begin
        Result := Self[i];
        ind := i
        end
    end
  else
    for i := 2 to FLen do
      if Self[i] > Result then
        begin
        Result := Self[i];
        ind := i
        end;
end; { IndMinOrMax }

function TwsVec.MinOrMax(IsMin: Boolean): Double;
{ Objetivo
    Retorna o menor ou o maior valor de um vetor
  Parâmetros
    y: Vetor para obtenção dos valores
    IsMin: Se True o mínimo será encontrado, se False o máximo
  Observação
    Faz uma busca sequencial
}
var
  i: Integer;
begin
  Result := Self[1];
  if IsMin then
    begin
    for i := 2 to FLen do
      if Self[i] < Result then Result := Self[i];
    end
  else
    for i := 2 to FLen do
      if Self[i] > Result then Result := Self[i];
end; { MinOrMax }

procedure TwsVec.MinMax(var Min, Max: Double);
{ Objetivo
    Retorna os valores minimo e maximo do vetor.
  Parâmetros
    y: Vetor de onde serão obtidos os valores
    Min: valor do mínimo
    Max: Valor do máximo
}
var
  i: Integer;
  x1, x2: Double;
begin
  Min := Self[1]; Max := Min;
  i := 2;
  while i < FLen do
    begin
    x1 := Self[i]; x2 := Self[i+1];
    if x1 > x2 then
      begin
      if x1 > Max then Max := x1;
      if x2 < Min then Min := x2;
      end
    else
      begin
      if x2 > Max then Max := x2;
      if x1 < Min then Min := x1;
      end;
    Inc(i, 2)
    end; { while }
  if i = FLen then
    begin
    if Self[i] > Max then
      Max := Self[i]
    else
      if Self[i] < Min then Min := Self[i];
    end
end; { MinMax }

procedure TwsVec.IndMinMax(var imin, imax: Integer);
{ Objetivo
    Retorna os índices dos valores minimo e maximo do vetor.
  Parâmetros
    y: Vetor de onde serão obtidos os valores
    iMin: Índice do mínimo
    iMax: Índice do máximo
}
var
  i: Integer;
  x1, x2,
  Min, Max: Double;
begin
  Min := Self[1]; Max := Min;
  imin := 1;
  imax := imin;
  i := 2;
  while i < FLen do
    begin
    x1 := Self[i]; x2 := Self[i+1];
    if x1 > x2 then begin
      if x1 > Max then
        begin
        Max := x1;
        imax := i
        end;
      if x2 < Min then
        begin
        Min := x2;
        imin := i
        end
    end
    else
      begin
      if x2 > Max then
        begin
        imax := i;
        Max := x2
        end;
      if x1 < Min then
        begin
        imin := i;
        Min := x1
        end;
      end;
    Inc(i, 2)
    end; { while }
  if i = FLen then
    begin
    if Self[i] > Max then
      begin
      Max := Self[i];
      imax := i
      end
    else
      if Self[i] < Min then
        begin
        Min := Self[i];
        imin := i
        end
    end
end; { MinMax }

function TwsVec.PartialMean(StartCol, EndCol: Integer; out n: Integer): Double;
{ Objetivo
    Obtem a média de um subconjunto de valores do vetor especificado
  Parâmetros
    StartCol: Índice inicial
    EndCol: Índice final
    n: Número de observações válidas
}
var j: Integer;
    M, aux: Double;
begin
  Result := 0; n := 0;
  Try
    for j := StartCol to EndCol do
      if not IsMissValue(j, aux) then
        begin
        Inc(n);
        aux := aux - Result;
        Result := Result + aux/n
        end;
    if n = 0 Then Result := wscMissValue;
  Except
    Result := wscMissValue;
  end;
end; { PartialMean }

procedure TwsVec.PartialVarMean(SCol,FCol: Integer; var m, v:Double; var n:Integer);
{ Objetivo
    Determina a media e a variância dos valores do vetor especificado
  Parâmetros
    m: retorna o valor da média
    v: retorna o valor da variância
    n: Número de valores válidos
  Observação
    A média e a variância são calculadas pelo algoritmo das médias provisórias.
}
var
  j: Integer;
  aux: Double;
begin
  m := 0; v := 0; n := 0;
  for j := SCol to FCol do
    if not IsMissValue(j, aux) then
       begin
       Inc(n);
       aux := aux - m;
       m := m + aux/j;
       v := v + ((n-1)/n)*aux*aux
       end;
  if n>1 then
    v := v/(n-1)
end; { PartialVarMean }

procedure TwsVec.VarMean(var m, v: Double; var n: Integer);
{ Objetivo
    Determina a media dos valores do vetor especificado
  Parâmetros
    m: retorna a média dos valores
    v: retorna a variância dos valores
    n: Número de valores válidos
  Observação
    A média é calculada pelo algoritmo das medias provisorias.
}
var
  j: Integer;
  aux: Double;
begin
  PartialVarMean(1,FLen,m,v,n)
end; { VarMean }

function TwsVec.Mean(out n: Integer): Double;
{ Objetivo
    Obtem a média do vetor especificado
  Parâmetros
    n: Número de observações válidas
}
begin
  Result:=PartialMean(1,FLen,n);
end; { Mean }

function TwsVec.PartialDescriptive(SCol, FCol: Integer): TwsVec;
{ Objetivo
    Obter um vetor com algumas estatísticas descritivas referentes aos valores do vetor
  Parâmetros
    SCol: Índice do primeiro elemento que será incluído nos cálculos
    FCol: Índice do último elemento que será incluído nos cálculos
  Retorno
    Retorna um vetor com as estatísticas descritivas nas seguintes posições:
      1: Média
      2: Variância
      3: Desvio Padrão
      4: Coeficiente de variação
      5: Número de valores do vetor
      6: Número de observações válidas
}
var
  m,v: Double;
  n: Integer;
begin
  Result:=TwsDFVec.Create(6);
  Fill(0);
  PartialVarMean(SCol,FCol,m,v,n);
  Result[1]:=m;
  Result[2]:=v;
  Result[3]:=ScalarSqrt(v);
  Result[4]:=ScalarDiv(Result[3],m);
  Result[5]:=FLen;
  Result[6]:=n
end;

function  TwsVec.PartialVecDSP(StartCol,EndCol:Integer; Media:Double = -1): Double;
{ Objetivo
    Obtém média e desvio padrão utilizando parte dos valores do vetor
  Parâmetros
    StartCol: índice do primeiro elemento
    EndCol: Índice do último elemento
    Media: Dependendo do valor inicial (-1), retorna a média
  Retorno
    Desvio padrão
}
var j, n: Integer;
    aux: Double;
begin
  if Media = -1 Then
     Media := PartialMean(StartCol, EndCol, n)
  else
    if wsGLib.IsMissValue(Media) Then
      begin
      Result := wscMissValue;
      exit;
      end;

  try
    Result := 0; n := 0;
    for j := StartCol to EndCol do
      if not Self.IsMissValue(j, aux) then
        begin
        Inc(n);
        Result := Result + ( SQR( Aux - Media ));
        end;

    if n > 1 then
       Result := Sqrt(Result / (n - 1))
    else
       Result := wscMissValue;
  except
    Result := wscMissValue;
  end;
end; {PartialDSP}

function  TwsVec.PartialCV(StartCol,EndCol:Integer; Media:Double=-1; DSP:Double=-1): Double;
{ Objetivo
    Obtém média, desvio padrão e coeficiente de variação utilizando parte dos valores do vetor
  Parâmetros
    StartCol: índice do primeiro elemento
    EndCol: Índice do último elemento
    Media: Dependendo do valor inicial (-1), retorna a média
    DSP: Dependendo do valor inicial (-1), retorna o desvio padrão
  Retorno
    Coeficiente de variação
}
var n: integer;
begin
  if Media = -1 Then
     Media := Self.PartialMean (StartCol, EndCol, {out} n)
  else
    if wsGlib.IsMissValue(Media) or wsGlib.IsMissValue(DSP) Then
      begin
      Result := wscMissValue;
      exit;
      end;

  if DSP = -1 Then
     DSP := Self.PartialVecDSP(StartCol, EndCol, Media);

  try
    Result := DSP / Media * 100
  except
    Result := wscMissValue;
  end;
end; {PartialVecCV}

procedure TwsVec.PartialVec_Mean_DSP_CV(StartCol, EndCol: Integer;
                                 out n: Integer; out Mean, DSP, CV: Double);
{ Objetivo

  Parâmetros

}
var j   : Integer;
    aux : Double;
begin
  Mean := 0; n := 0;

  for j := StartCol to EndCol do
    if not IsMissValue(j, aux) then
      begin
      Inc(n);
      aux := aux - Mean;
      Mean := Mean + aux/n
      end;

  if n = 0 Then
     begin
     Mean := wscMissValue;
     DSP  := wscMissValue;
     CV   := wscMissValue;
     end
  else
     if (n > 1) and (Mean > 0) then
        begin
        DSP := 0; n := 0;

        for j := StartCol to EndCol do
          if not IsMissValue(j, aux) then
            begin
            Inc(n);
            DSP := DSP + ( SQR( Aux - Mean ));
            end;

        DSP := Sqrt(DSP / (n - 1));
        CV := DSP / Mean * 100;
        end
     else
        if n > 1 Then
           begin
           DSP := 0;
           CV  := 0;
           end
        else
           begin
           DSP := wscMissValue;
           CV  := wscMissValue;
           end;
end; { PartialVecMean }

function TwsVec.Quantile(p, q: Integer): Double;
{ Objetivo
    Obtém os quantis associados aos valores de um vetor
  Parâmetros
    p: Ordem do quantil
    q: Tipo de quantil
  Observações
    Encontra o p-esimo quantil q de y. Por ex., Quantile(3, 4) produz o terceiro quartil e
    Quantile(2, 10), o segundo decil. O valor de p devera estar entre 1 e q-1 (inclusive)
    e y devera estar ordenado em ordem crescente
}
var
  i1, i2: Integer;
begin
  i1 := (p*FLen) div q + 1;
  i2 := FLen - ((q - p)*FLen div q);
  Quantile := (Self[i1] + Self[i2])/2;
end; { Quantile }
(*
function TwsVec.Percentis(Perc: TwsVec): TwsVec;
{ Objetivo
    Num vetor já ordenado, obtém os percentis do vetor especificado.
  Parâmetros
    Perc: Na entrada contém as percentagens relativas aos percentis desejado (em valores
    relativos) e na saida armazena os percentis obtidos.
  Observações
  Os valores mais comuns (na entrada de Perc) sao:
    0: Minimo
  .25: 1o. quartil
  .50: mediana
  .75: 3o. quartil
    1: maximo
  Valores perdidos
    Nao considera

}
var
  i, ii: Integer;
  x, f: Double;
begin
  for i := 1 to Perc.Len do
    begin
    f := Perc[i];
    if f < 1 then
      begin
      x := FLen*f;
      ii := Trunc(Int(x));
      f := Frac(x);
      if f=0 then
        Perc[i] := (Self[ii]+Self[ii+1])/2
      else
        Perc[i] := Self[ii+1]
      end
    else
      Perc[i]:=wscMissValue
    end;
  Result:=Perc
end;
*)
function TwsVec.Percentis(Perc: TwsVec): TwsVec;
{ Objetivo
    Num vetor já ordenado, obtém os percentis do vetor especificado.
  Parâmetros
    Perc: Na entrada contém as percentagens relativas aos percentis desejado (em valores
    relativos) e na saida armazena os percentis obtidos.
  Observações
  Os valores mais comuns (na entrada de Perc) sao:
    0: Minimo
  .25: 1o. quartil
  .50: mediana
  .75: 3o. quartil
    1: maximo
  Valores perdidos
    Nao considera
}
var
  i,ii: Integer;
  x,f : Double;
begin
  Result := Perc;
  for i := 1 to Perc.Len do
    begin
    f := Perc[i];
    if (f>=0) and (f <= 1) then
      begin
      if FEquals(f,0) then
        Result[i]:=Self[1]
      else
        if FEquals(f,1) then
          Result[i]:=Self[FLen]
        else
          begin
          x := FLen*f;
          ii := Trunc(Int(x));
          f := Frac(x);
          if FEquals(f,0) then
            Result[i] := (Self[ii]+Self[ii+1])/2
          else
            Result[i] := Self[ii+1]
          end
      end
    else
      Result[i] := wscMissValue
    end;
end;

function TwsVec.WMoments(w: TwsVec; var ErrCode: Word): TwsVec;
{ Objetivo
    Obtem estatisticas descritivas ponderadas a partir do vetor especificado
  Parâmetros
    w: Vetor com os pesos relativos
    ErrCode: Código de erro
      0: Retorno sem erro
      NImprDim: Dimensões impróprias para a operação
  Observação
    As estatísticas obtidas e a sua posição no vetor de saída são:
      1: Numero de observacoes
      2: Soma dos pesos
      3: Media
      4: Soma
      5: Variancia
      6: Desvio padrao
      7: ErrCode padrao da media
      8: CV
      9: Soma de quadrados corrigida
     10: Assimetria
     11: Curtose
  Valores perdidos: Elimina do cálculo
}
var
  n,j           : Integer;
  w1,w0,x,v,wx,
  m1,m2,m3,m4   : Double;
begin
  if FLen <> w.len then
    begin
    ErrCode := NImprDim;
    Exit
    end;
  ErrCode := 0;
  w1:=0; m1:=0; m2:=0; m3:=0; m4:=0; n:=0;
  for j := 1 to FLen do
    begin
    x := Self[j];
    wx := w[j];
    if (not wsGLib.IsMissValue(x)) and (not wsGLib.IsMissValue(wx)) then
      begin
      Inc(n);                                                    { Numero de observacoes }
      w0 := w1;                             { Guarda pesos acumulados ate passo anterior }
      w1 := w1+wx;                                        { Acumula os pesos ate passo j }
      v := (wx/w1)*(x-m1);
      m4 := m4 - 4*v*m3 + 6*v*v*m2 + (w1*w1-3*wx*w0)*Power(v/wx,3)*v*w1*w0; { 4o momento }
      m3 := m3 - 3*v*m2 + (w1 - 2*wx)*Power(v/wx,2)*v*w1*w0;                { 3o momento }
      m2 := m2 + (v/wx)*v*w1*w0;                                            { 2o momento }
      m1 := m1 + v                                                               { media }
      end
    end;
  Result := TwsDFVec.Create(11);
  Result[1] := n;                                           { Numero de observacoes }
  Result[2] := w1;                                                 { Soma dos pesos }
  Result[3] := m1;                                                          { Media }
  Result[4] := w1*m1;                                                       { Total }
  wx := ScalarDiv(m2,w1-1);
  Result[5] := wx;                                                      { Variancia }
  x := ScalarSqrt(wx);
  Result[6] := x;                                                   { Desvio padrao }
  Result[7] := ScalarDiv(x,Sqrt(w1));                        { Erro padrao da media }
  Result[8] := ScalarDiv(x*100,m1);                       { Coeficiente de variacao }
  Result[9] := m2;                                   { Soma de quadrados de desvios }
  Result[10] := ScalarDiv(w1*m3,(w1-1)*(w1-2)*wx*x);             { Coef. assimetria }
                                                          { Coeficiente de curtose}
  Result[11] := ScalarDiv(w1*(w1+1)*m4-3*m2*m2*(w1-1),(w1-1)*(w1-2)*(w1-3)*wx*wx)
end; { WVecMoments }

function TwsVec.Moments: TwsVec;
{ Objetivo
    Obtém estatisticas descritivas a partir do vetor especificado
  Observação
    As estatísticas obtidas e a sua posição no vetor de saída são:
      1: Numero de observacoes
      2: Soma dos pesos
      3: Media
      4: Soma
      5: Variancia
      6: Desvio padrao
      7: Erro padrao da media
      8: CV
      9: Soma de quadrados corrigida
     10: Assimetria
     11: Curtose
  Valores perdidos
    Elimina do cálculo
  Obs.:
    Se n<1 - variancia não é calculada
    Se n<3 - assimetria não é calculada
    Se n<4 - curtose não é calculada
    Algoritmo - SPSSx - Statistical Algorithms pag. 5-7
}
var
  w0,n,j  : Integer;
  x,v,m1,
  m2,m3,m4: Double;
begin
  m1:=0; m2:=0; m3:=0; m4:=0; n:=0;
  for j := 1 to FLen do
    if not IsMissValue(j,x) then
      begin
      Inc(n);                                                    { Numero de observacoes }
      w0 := n-1;
      v := (x-m1)/n;
      m4 := m4-4*v*m3+6*v*v*m2+(n*n-3*w0)*Power(v,4)*n*w0;                { 4o momento }
      m3 := m3-3*v*m2+(n-2)*Power(v,3)*n*w0;                              { 3o momento }
      m2 := m2+v*v*n*w0;                                                  { 2o momento }
      m1 := m1+v                                                               { media }
      end;

  Result := TwsDFVec.Create(11);
  Result[1] := n;                                           { Numero de observacoes }
  Result[2] := n;                                                  { Soma dos pesos }
  Result[3] := m1;                                                          { Media }
  Result[4] := n*m1;                                                        { Total }
  v := ScalarDiv(m2,n-1);
  Result[5] := v;                                                       { Variancia }
  x := ScalarSqrt(v);
  Result[6] := x;                                                   { Desvio padrao }
  Result[7] := ScalarDiv(x,Sqrt(n));                         { Erro padrao da media }
  Result[8] := ScalarDiv(x*100,m1);                       { Coeficiente de variacao }
  Result[9] := m2;                                   { Soma de quadrados de desvios }
  try
    Result[10] := (n*m3)/((n-1)*(n-2)*v*x);                       { Coef. assimetria }
  except
    Result[10]:=wscMissValue;
  end;
  try                                                          { Coeficiente de curtose}
    Result[11] := (n*(n+1)*m4-3*m2*m2*(n-1))/((n-1)*(n-2)*(n-3)*v*v)
  except
    Result[11] := wscMissValue;
  end;
end; { VecMoments }

function TwsVec.EuclideanNorm(var n: Integer): Double;
{ Objetivo
    Obtém a norma Euclidiana do vetor especificado
  Parâmetros
    n: Retorna os valores válidos do vetor
  Observação
    Norma euclidiana é a raiz quadrada da soma de quadrados dos valores
  Valores perdidos
    Não inclui no cálculo
}
begin
  Result := ScalarSqrt(SumOfSq(n))
end;

function TwsVec.AbsoluteNorm(var n: Integer): Double;
{ Objetivo
    Obtém a norma do valor absoluto do vetor especificado
  Parâmetros
    n: Retorna os valores válidos do vetor
  Valores perdidos
    Não inclui
  Observação
    Norma do valor absoluto é a soma dos valores absolutos
}
var
  j: Integer;
  x: Double;
begin
  Result := 0; n := 0;
  for j := 1 to FLen do
    if not IsMissValue(j, x) then
       begin
       Inc(n);
       Result := Result + Abs(x);
       end
end; { VecAbsNorm }

function TwsVec.Total(var n: Integer): Double;
{ Objetivo
    Determina a total dos valores do vetor especificado
  Parâmetros
    y: Vetor com os valores
    n: Número de valores válidos
  Valores perdidos
    Não inclui
}
var
  i: Integer;
  x: Double;
begin
  Result := 0;
  n := 0;
  for i := 1 to FLen do
    if not IsMissValue(i, x) then
      begin
      Inc(n);
      Result := Result + x;
      end
end; { Total }

function TwsVec.SumOfSq(var n: Integer): Double;
{ Objetivo
    Determina a soma de quadrados dos valores do vetor especificado
  Parâmetros
    n: Número de valores válidos
  Valores perdidos
    Não considera, oe seja, a soma de quadrados é calculada somente para os valores válidos
}
var
  j: Integer;
  x: Double;
begin
  Result := 0;
  n := 0;
  for j := 1 to FLen do
    if not IsMissValue(j, x) then
      begin
      Inc(n);
      Result := Result + x*x;
      end
end; { SQ }

function TwsVec.Accum(NewVec: Boolean): TwsVec;
{ Objetivo
    Retorna um vetor com os valores acumulados até a posição considerada
  Parâmetros
    y: Vetor com os valores a acumular
    NewVec: Se True um novo vetor é criado para armazenar os resultados, senão os valores acumulados
            substituem os de y
  Valores perdidos: Se o valor em consideração é perdido entao o valor correspondente do resultado é
            perdido.
}
var
  i: Integer;
  x, aux: Double;
begin
  if NewVec then
    Result := TwsDFVec.Create(FLen)
  else
    Result := Self;
  aux := 0;
  for i := 1 to FLen do
    if not IsMissValue(i, x) then
      begin
      aux := aux + x;
      Result[i] := aux
      end
    else
      Result[i] := wscMissValue
end; { Accum }

function TwsVec.Sort(NewVec, Ascd: Boolean): TwsVec;
{ Objetivo
    Ordena o vetor
  Parâmetros
    NewVec: Se True, o vetor ordenado retorna em outro vetor; caso contrário a ordenação é feita no
      próprio vetor
    Ascd: Se True, a ordenação é ascendente; descendente caso contrário
}
Begin
  if NewVec then
    Result:=Copy(1,FLen)
  else
    Result := Self;
  Result.QuickSort(Ascd)
End; { Sort }

procedure TwsVec.SortOf(Indx: TwsLIVec; Var ErrCode: Word);
{ Objetivo
    Ordena segundo um vetor de índices
  Parâmetros
    Indx: vetor de índices para ordenação
    ErrCode: retorna 0 se a ordenação foi feita sem problemas
}
var
  j: Integer;
  x: TwsVec;
begin
  ErrCode:=0;
  if FLen = Indx.Len then
    begin
    x := Copy(1, FLen);
    for j := 1 to FLen do Self[j] := x[Indx[j]];
    x.Free
    end
  else
    ErrCode:=NImprDim;
end; { SortOf }

function TwsVec.RankTie: TwsVec;
{ Objetivo
    Retorna um vetor com os postos dos valores do vetor especificado considerando empates
}
var
  i, j: Integer;
  u: Double;
begin
  Result := TwsDFVec.Create(FLen);
  for i := 1 to FLen do begin
    Result[i] := 0.5;
    for j := 1 to FLen do begin
      if Self[i] > Self[j] then
        u := 1
      else
        if Self[i] < Self[j] then
          u := 0
        else
          u := 0.5;
      Result[i] := Result[i] + u
    end
  end
end; { RankTie }

function TwsVec.Rank(Ascd: Boolean; YSort : Boolean): TwsVec;
{ Objetivo
    Retorna um vetor com os postos dos valores do vetor especificado (não considera os empates)
  Parâmetros
    Ascd: True se os valores estão ordenados em ordem ascendente (se for o caso)
    YSort: Se True então o vetor y retorna ordenado, caso contrário retorna inalterado
}
var
  Aux: TwsLIVec;
  i  : Integer;
  x  : Double;
begin
  if YSort then
    begin
    Aux:=QuickIndx(Ascd);
    Result := TwsDFVec.Create(Aux.Len);
    for i:=1 to Aux.Len do
      if not wsGLib.IsMissValue(Get(i)) then
        Result[Aux[i]] := i
      else
        Result[Aux[i]] := wscMissValue;
    end
  else
    begin
    Result := Copy(1,FLen);
    Aux:=Result.QuickIndx(Ascd);
    for i:=1 to Aux.Len do
      if not wsGLib.IsMissValue(Result[Aux[i]]) then
        Result[Aux[i]] := i
      else
        Result[Aux[i]] := wscMissValue;
    end;
  Aux.Free;
end;

function TwsVec.PolValue(const x: Double): Double;
{ Objetivo
    Calcular o valor de um polinômio no ponto x
  Parâmetros
    p: Vetor com os coeficientes dos polinômios
    x: Valor para o qual se quer o valor do polinomio
}
var
  i: Integer;
begin
  Result := Self[FLen];
  for i := FLen-1 downto 1 do
    Result := Result*x+Self[i]
end; { PolValue }

function TwsVec.VecPolValue(x, incr: Double; n: Integer): TwsVec;
{ Objetivo
    Retorna um vetor com valores de um polinômio
  Parâmetros
    p: Vetor com os coeficientes dos polinômios
    x: Valor inicial para o qual se quer o valor do polinomio
    incr: Incremento para cada valor
    n: Número de valores a calcular
}
var
  i,j: Integer;
  pol: Double;
begin
  Result := TwsDFVec.Create(n);
  for j := 1 to n do
    begin
    pol := Self[FLen];
    for i  := FLen-1 downto 1 do
      pol := pol*x+Self[i];
    Result[j] := pol;
    x := x + incr
    end
end; { VecPolValue }

function TwsVec.DerPolValue(var dp: Double; const x: Double): Double;
{ Objetivo
    Calcular o valor de um polinômio e de sua derivada no ponto x
  Parâmetros
    p: Vetor com os coeficientes dos polinômios
    dp: retorna o valor da derivada no ponto x
    x: Valor para o qual se quer o valor do polinomio
}
var
  i: Integer;
begin
  Result := Self[FLen];
  dp:=0;
  for i := FLen-1 downto 1 do
    begin
    dp := dp*x+Result;
    Result := Result*x+Self[i];
    end
end; { DerPolValue }

function TwsVec.LinearComb(X:TwsVec; k1, k2:Double; NewVec:Boolean; var ErrCode:Word):TwsVec;
{ Objetivo
    Substitui um vetor pela sua combinação linear com outro
  Parâmetros
    X: Vetores que participam da combinação linear
    k1, k2: constantes que para a combinação linear
    NewVec: Se True se a operação deverá gerar um novo vetor, se False o resultado volta em X1
  Observações: O vetor resultante é k1*X1+k2*X2
}
var
  i: Integer;
begin
  ErrCode := 0;
  if FLen = X.Len then begin
    if NewVec then
      Result := TwsDFVec.Create(FLen)
    else
      Result := Self;
    for i := 1 to Result.Len do
      Result[i]:= k1*Self[i] + k2*X[i];
  end
  else
    ErrCode := NImprDim
end; { LinearComb }

function TwsVec.ElemOper3(X: TwsVec; k:Double; NewVec:Boolean; var ErrCode:Word): TwsVec;
{ Objetivo
    Obtém uma combinação linear do tipo 3 entre dois vetores
  Parâmetros
    X: Vetor para combinação linear
    k: Coeficiente para combinação linear
    NewVec: Se True se a operação deverá gerar um novo vetor, se False o resultado volta em X1
  Observação
    Retorna o resultado de X1+k*X2
}
var
  i: Integer;
begin
  ErrCode := 0;
  if FLen = X.Len then begin
    if NewVec then
      Result := TwsDFVec.Create(FLen)
    else
      Result := Self;
    for i := 1 to Result.Len do
      Result[i]:= Self[i] + k*X[i]
  end
  else
    ErrCode := NImprDim
end; { ElemOper3 }

function TwsVec.LocMiss(Loc: TwsLIVec): Boolean;
{ Objetivo
    Retorna True se o vetor possui valor perdido em alguma posição especificada. False caso contrário
  Parâmetros
    Loc: vetor de posições para verificação de valores perdidos
}
var i,k: Integer;
begin
  Result := False;
  for i := 1 to Loc.Len do
    begin
    k:=Loc[i];
    if ((k>0) and (k<=FLen)) then
      if wsGLib.IsMissValue(Self[k]) then
        begin
        Result := True;
        Break
        end
    end
end; { LocMiss }

function TwsVec.HaveMissValue(): Boolean;
{ Objetivo
    Retorna True se o vetor possui algum valor perdido.
}
var i: Integer;
begin
  Result := False;
  for i := 1 to FLen do
    if wsGLib.IsMissValue(Self[i]) then
       begin
       Result := True;
       Break
       end;
end; { HaveMiss }

function TwsVec.GetAsString(i: Integer): String;
{ Objetivo
    Recupera valor como string
  Parâmetro
    Posição ocupada pelo valor
}
var x: Double;
begin
  if not IsMissValue(i, x) then
     Result := Format('%*.*g', [PrintOptions.ColWidth, PrintOptions.ColPrecision, Fuzz(x)])
  else
     Result := Format('%*s', [PrintOptions.ColWidth, '.'])
end;

function TwsVec.Inv(NewVec: Boolean): TwsVec;
var i: Longint;
    x, aux: double;
begin
  if NewVec then
     Result := TwsDFVec.Create(FLen)
  else
     Result := Self;

  aux := 0;

  for i := 1 to FLen do
    if not IsMissValue(i, x) then
       Result[i] := 1 / Result[i]
    else
       Result[i] := wscMissValue
end; { Inv }

function FuncGT(const x, y: Real): Boolean;
begin
  Result := (x > y);
end;

function FuncGTE(const x, y: Real): Boolean;
begin
  Result := (x >= y);
end;

function FuncLT(const x, y: Real): Boolean;
begin
  Result := (x < y);
end;

function FuncLTE(const x, y: Real): Boolean;
begin
  Result := (x <= y);
end;

function TwsVec.Find(const x: Real; Rule: TRuleFunc): TwsVec;
var n, i: Integer;
    r: Double;
begin
  n := 0;
  for i := 1 to FLen do
    if not IsMissValue(i, r) and Rule(r, x) then
      Inc(n);

  Result := TwsSIVec.Create(n);
  n := 0;
  for i := 1 to FLen do
    if not IsMissValue(i, r) and Rule(r, x) then
      begin
      Inc(n);
      Result[n] := i;
      end;
end;

function TwsVec.FindGT(const x: Real): TwsVec;
begin
  Result := Find(x, @FuncGT);
end;

function TwsVec.FindGTE(const x: Real): TwsVec;
begin
  Result := Find(x, @FuncGTE);
end;

function TwsVec.FindLT(const x: Real): TwsVec;
begin
  Result := Find(x, @FuncLT);
end;

function TwsVec.FindLTE(const x: Real): TwsVec;
begin
  Result := Find(x, @FuncLTE);
end;

function TwsVec.FindMinMean(n: Word): TwsVec;
var i, k, ii: Integer;
    m, Min: Real;
begin
  if (n = 0) or (n > FLen) then
     Raise Exception.Create('Período inválido');

  m := 0;
  for i := 1 to n do m := m + Data[i];
  Min := m / n;
  ii := 1;

  for i := 2 to (FLen - n + 1) do
    begin
    m := 0;
    for k := 0 to n-1 do m := m + Data[i+k];
    m := m / n;
    if m < Min then
       begin
       Min := m;
       ii := i;
       end;
    end;

  Result := TwsSIVec.Create(n);
  for i := 1 to n do
    Result[i] := ii + i - 1;
end;

function TwsVec.FindMinSun(n: Word): TwsVec;
var i, k, ii: Integer;
    m, Min: Real;
begin
  if (n = 0) or (n > FLen) then
     Raise Exception.Create('Período inválido');

  m := 0;
  for i := 1 to n do m := m + Data[i];
  Min := m;
  ii := 1;

  for i := 2 to (FLen - n + 1) do
    begin
    m := 0;
    for k := 0 to n-1 do m := m + Data[i+k];
    if m < Min then
       begin
       Min := m;
       ii := i;
       end;
    end;

  Result := TwsSIVec.Create(n);
  for i := 1 to n do
    Result[i] := ii + i - 1;
end;

function TwsVec.Max: Real;
var i: Integer;
    r: Double;
begin
  r := wscMissValue;

  // procura o primeiro elemento diferente de MissValue
  for i := 1 to FLen do
    if not IsMissValue(i, r) then
       begin
       Result := r;
       Break;
       end;

  for i := i+1 to FLen do
    if not IsMissValue(i, r) and (r > Result) then
       Result := r;
end;

function TwsVec.Min: Real;
var i: Integer;
    r: Double;
begin
  r := wscMissValue;

  // procura o primeiro elemento diferente de MissValue
  for i := 1 to FLen do
    if not IsMissValue(i, r) then
       begin
       Result := r;
       Break;
       end;

  for i := i+1 to FLen do
    if not IsMissValue(i, r) and (r < Result) then
       Result := r;
end;

function TwsVec.REC(Indexs: TwsVec): TwsVec;
var i: Integer;
begin
  Result := TwsVec(ClassType.NewInstance).Create(Indexs.Len);
  for i := 1 to Indexs.Len do
    Result.Data[i] := Data[Indexs.AsInteger[i]];
end;

procedure TwsVec.LoadFromTextFile(const FileName: String);
{ Objetivo
    Carrega vetor de um arquivo texto
  Parâmetros
    FileName: nome do arquivo texto
}
var SL: TStrings;
    j: Cardinal;
    i, k: Integer;
    s: String;
begin
  if not SysUtils.FileExists(FileName) then
     begin
     SetLength(0);
     exit;
     end;

  FLen := 0;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    for i := 0 to SL.Count-1 do inc(FLen, WordCount(SL[i], [' ', ';', #9]));
    SetLength(FLen);

    k := 0;
    for i := 0 to SL.Count-1 do
      begin
      j := 1;
      repeat
        S := SysUtilsEx.StrToken(SL[i], j, [' ', ';', #9]);
        if S <> '' then
           begin
           Inc(k);
           if k <= FLen then Data[k] := FracToReal(S);
           end;
      until (S = '');
      end;

  finally
    SL.Free;
  end;
end;

{ TwsDFVec }


procedure TwsDFVec.Add(x: Double);
{ Objetivo
    Concatena um valor ao vetor. Para cada valor concatenado, a cópia de todo vetor é realizada
  Parâmetros
    x: valor a concatenar
}
var
  OldLen: Integer;
begin
  OldLen:=FLen;
  Inc(FLen);
  SetLength(FLen);
  Put(OldLen+1,x);
end;

function TwsDFVec.SizeOfElements: byte;
{ Objetivo
    Retorna o espaço ocupado por cada elemento do vetor
}
begin
  Result := SizeOf(Double);
end;

procedure TwsDFVec.Put(i: Integer; x: Double);
{ Objetivo
    Atribui valor a posição especificada
  Parâmetros
    i: posição para atribuição
    x: valor para atribuição
}
begin
  Fdata[i-1] := x;
  FModified := True;
end; { TwsDFVec.Put }

function TwsDFVec.Get(i: Integer): Double;
{ Objetivo
    Recupera valor
  Parâmetros
    i: posição do valor
}
begin
  Result := FData[i-1];
end; { TwsDFVec.Get }

function TwsDFVec.GetAsInteger(i: Integer): Integer;
{ Objetivo
    Recupera valor como inteiro
  Parâmetros
    i: posição ocupada pelo valor
}
begin
  Result := Trunc(FData[i-1]);
end;


function TwsDFVec.AsStringF(i,k: Integer): String;
{ Objetivo
    Recupera valor como string
  Parâmetro
    i: Posição ocupada pelo valor
    k: Precisao para escrita
}
var x: Double;
begin
  if not IsMissValue(i, x) then
    Result := Format('%*.*g', [PrintOptions.ColWidth, k, Fuzz(x)])
  else
    Result := Format('%*s', [PrintOptions.ColWidth, '.'])
end;

procedure TwsDFVec.SetLength(aLen: Integer);
{ Objetivo
    Dimensiona espaço ocupado pelo vetor
  Parâmetro
    ALen: número de elementos do vetor
}
begin
  try
    FLen := aLen;
    System.SetLength(FData, aLen);
  except
    Raise Exception.Create('Falta de memória ao alocar o Vetor ' + Name);
  end;
end;

function TwsDFVec.ByScalar(const x:Double; Op:TwsEnumTypeOp; NewVec,SFirst:Boolean): TwsVec; {wsFuncoesDeMatrizes}
{ Objetivo
    Efetuar operacoes emtre um escalar e os elementos do vetor
  Parâmetros
    x: Valor do escalar
    Op: Operação desejada
    NewVec: True se um novo vetor será criado; False se o resultado será armzenado no
      próprio vetor
    SFirst: True se a operação for escalar op elemento; false se for elemento op escalar
}
var
  i: Integer;
begin
  if NewVec then
    Result:=TwsDFVec.Create(FLen)
  else
    Result:=Self;
  case Op of
    opSum:            // Soma
      for i:=1 to FLen do
        Result[i]:=ScalarSum(Self[i],x);
    opSub:            // Subtracao
      for i:=1 to FLen do
        if SFirst then
          Result[i]:=ScalarSub(x,Self[i])
        else
          Result[i]:=ScalarSub(Self[i],x);
    opDiv:            // Divisao
      for i:=1 to FLen do
        if SFirst then
          Result[i]:=ScalarDiv(x,Self[i])
        else
          Result[i]:=ScalarDiv(Self[i],x);
    opProd:           // Produto
      for i:=1 to FLen do
        Result[i]:=ScalarProd(Self[i],x);
    opPower:          // Potência
      for i:=1 to FLen do
        if SFirst then
          Result[i]:=ScalarPower(x,Self[i])
        else
          Result[i]:=ScalarPower(Self[i],x);
    opGE:            // Maior ou igual
      for i:=1 to FLen do
        if SFirst then
          Result[i]:=ScalarGE(x,Self[i])
        else
          Result[i]:=ScalarGE(Self[i],x);
    opGT:            // Maior que
      for i:=1 to FLen do
        if SFirst then
          Result[i]:=ScalarGT(x,Self[i])
        else
          Result[i]:=ScalarGT(Self[i],x);
    opLE:           // Menor ou igual
      for i:=1 to FLen do
        if SFirst then
          Result[i]:=ScalarLE(x,Self[i])
        else
          Result[i]:=ScalarLE(Self[i],x);
    opLT:          // Menor que
      for i:=1 to FLen do
        if SFirst then
          Result[i]:=ScalarLT(x,Self[i])
        else
          Result[i]:=ScalarLT(Self[i],x);
    opEQ:         // Igual
      for i:=1 to FLen do
        Result[i]:=ScalarEQ(Self[i],x);
    opNE:         // Diferente
      for i:=1 to FLen do
        Result[i]:=ScalarNE(Self[i],x);
    opOR:       // OU
      for i:=1 to FLen do
        Result[i]:=ScalarOR(Self[i],x);
    opAnd:     // E
      for i:=1 to FLen do
        Result[i]:=ScalarAnd(Self[i],x);
    opMax:     // Maior valor
      for i:=1 to FLen do
        Result[i]:=ScalarMax(Self[i],x);
    opMin:    // Menor valor
      for i:=1 to FLen do
        Result[i]:=ScalarMin(Self[i],x);
  end; // case
end; // ByScalar

{ TwsSIVec }

function TwsSIVec.Get(i: Integer): Double;
{ Objetivo
    Recupera valor na posição especificada
  Parâmetros
    i: posição ocupada pelo valor
}
begin
  Result := Fdata[i-1];
end;

procedure TwsSIVec.Put(i: Integer; x: Double);
{ Objetivo
    Atribui valor na posição especificada
  Parâmetro
    i: posição ocupada pelo valor
    x: valor a atribuir
}
begin
  Fdata[i-1] := Trunc(x);
  FModified := True;
end;

function TwsSIVec.GetI(i: Integer): SmallInt;
{ Objetivo
    Recupera valor inteiro
  Parâmetros
    i: posição ocupada pelo valor
}
begin
  Result := Fdata[i-1];
end;

procedure TwsSIVec.PutI(i: Integer; x: SmallInt);
{ Objetivo
    Atribui valor na posição especificada
  Parâmetros
    i: posição ocupada pelo valor
    x: valor a atribuir
}
begin
  Fdata[i-1] := x;
end;

function TwsSIVec.GetAsInteger(i: Integer): Integer;
{ Objetivo
    Recupera valor como inteiro
  Parâmetros
    i: posição ocupada pelo valor
}
begin
  Result := GetI(i-1);
end; { TwsDFVec.IGet }

procedure TwsSIVec.SetLength(aLen: Integer);
{ Objetivo
    Dimensiona espaço a ser ocupado pelo vetor
  Parâmetros
    aLen: número de elementos do vetor
}
begin
  try
    FLen := aLen;
    System.SetLength(FData, aLen);
  except
    Raise Exception.Create('Falta de memória ao alocar o Vetor ' + Name);
  end;
end;

function TwsSIVec.SizeOfElements: byte;
{ Objetivo
    Retorna espaço ocupado por cada elemento do vetor
}
begin
  Result := SizeOf(SmallInt);
end;

// Construtor otimizado para o tipo inteiro
constructor TwsSIVec.Create(const Values: array of SmallInt);
var i: integer;
begin
  inherited Create( System.Length(Values) );

  for i := 0 to High(Values) do
    FData[i] := Values[i];

  InitPrintOptions();
end;

function TwsSIVec.GetAsString(i: Integer): String;
begin
  Result := toString(FData[i-1]);
end;

{ TwsLIVec }

function TwsLIVec.Get(i: Integer): Double;
{ Objetivo
    Recupera valor na posição especificada
  Parâmetros
    i: posição ocupada pelo valor
}
begin
  Result := Fdata[i-1];
end;

procedure TwsLIVec.Put(i: Integer; x: Double);
{ Objetivo
    Atribui valor a posição especificada
  Parâmetros
    i: posição ocupada pelo valor
    x: valor a atribuir
}
begin
  Fdata[i-1] := Trunc(x);
  FModified := True;
end;

function TwsLIVec.GetI(i: Integer): Integer;
{ Objetivo
    Recupera valor como inteiro
  Parâmetros
    i: posição ocupada pelo valor
}
begin
  Result := Fdata[i-1];
end;

procedure TwsLIVec.PutI(i: Integer; x: Integer);
{ Objetivo
    Atribui valor a posição especificada
  Parâmetros
    i: posição ocupada pelo valor
    x: valor a atribuir
}
begin
  Fdata[i-1] := x;
end;

function TwsLIVec.GetAsInteger(i: Integer): Integer;
{ Objetivo
    Recupera como inteiro
  Parâmetros
    i: Posição ocupada pelo valor
}
begin
  Result := GetI(i);
end;

procedure TwsLIVec.SetLength(aLen: Integer);
{ Objetivo
    Dimensiona espaço a ser ocupado pelo vetor
  Parâmetros
    aLen: número de elementos do vetor
}
begin
  try
    FLen := aLen;
    System.SetLength(FData, aLen);
  except
    Raise Exception.Create('Falta de memória ao alocar o Vetor ' + Name);
  end;
end;

function TwsLIVec.SizeOfElements(): byte;
{ Objetivo
    Espaço ocupado por cada elemento do vetor
}
begin
  Result := SizeOf(Integer);
end;

// Construtor otimizado para o tipo inteiro
constructor TwsLIVec.CreateFrom(const Values: array of Integer);
var i: integer;
begin
  inherited Create( System.Length(Values) );

  for i := 0 to High(Values) do
    FData[i] := Values[i];

  InitPrintOptions();
end;

function TwsLIVec.GetAsString(i: Integer): String;
begin
  Result := toString(FData[i-1]);
end;

{ TwsSFVec }

function TwsSFVec.Get(i: Integer): Double;
{ Objetivo
    Recupera valor em posição especificada
  Parâmetros
    i: posição do valor
}
begin
  Result := Fdata[i-1];
end;

function TwsSFVec.GetAsInteger(i: Integer): Integer;
{ Objetivo
    Recupera valor como inteiro em posição especificada
  Parâmetros
    i: posição do valor
}
begin
  Result := Trunc(FData[i-1]);
end;

procedure TwsSFVec.Put(i: Integer; x: Double);
{ Objetivo
    Atribui valor em posição especificada
  Parâmetros
    i: posição do valor
}
begin
  Fdata[i-1] := x;
  FModified := True;
end;

procedure TwsSFVec.SetLength(aLen: Integer);
{ Objetivo
    Dimensiona espaço ocupado pelo vetor
  Parâmetros
    aLen: número de elementos do vetor
}
begin
  try
    FLen := aLen;
    System.SetLength(FData, aLen);
  except
    Raise Exception.Create('Falta de memória ao alocar o Vetor ' + Name);
  end;
end;

function TwsSFVec.SizeOfElements: byte;
{ Objetivo
    Espaço ocupado por cada elemento
}
begin
  Result := SizeOf(Single);
end;

{****************************** Rotinas *******************************}

function GetIndex(Const st1: String): TwsLIVec;
{ Objetivo
    Retorna um conjunto de inteiros positivos resultantes da interpretacao do string especificado.
  Parâmetros
    st1: String que estabelece o conjunto de valores inteiros
  Observações
    O string é quebrado por StrToken. O caracter '-' indica a amplitude e a quebra é feita a partir dos caracteres: [#9,#10,#13,' ',',','=','\', '"','(',')'].
  Ex.: GetIndex('1-3,6')=[1 2 3 6]
}
const
  BufLen = 150;
  RangeChar = '-';
var
  j, i, k, Hi, Lo: Cardinal;
  st: String;
  F: PFArray;
begin
  i := 1;
  k := 1;
  Result := TwsLIVec.Create(0);
  GetMem(F, sf(BufLen));
  repeat
    st := StrToken(st1, i, DelChar);
    if st <> '' then begin
      j := System.Pos(RangeChar, st);
      if j <> 0 then
        begin
          try
          Lo := StrToInt(System.Copy(st, 1, j-1));
          Hi := StrToInt(System.Copy(st, j+1, Length(st)));
          except
          Result.Free;
          Result := Nil;
          FreeMem(F, sf(BufLen));
          exit;
          end;
        end
      else
        begin
        Lo := StrToInt(st);
        Hi := Lo
        end; { if }
      if Lo <= Hi then
         for j := Lo to Hi do
           begin
           F^[k] := j;
           Inc(k)
           end
      else
         for j := Lo DownTo Hi do
           begin
           F^[k] := j;
           Inc(k)
           end
    end;
    if (k = BufLen) or (st = '') then begin
      Result.Append(F, k-1);
      k := 1;
    end;
  until st = '';
  FreeMem(F, sf(BufLen));
end; (* GetIndex *)

function Index(Lo, Hi: Integer): TwsLIVec;
{ Objetivo
    Constroi um vetor de indices de passo 1 que vai de Lo a Hi
  Parâmetros
    Lo: Valor inicial do índice
    Hi: Valor final do índice
}
var
  i, j: Integer;
begin
  Result := nil;
  if Hi >= Lo then begin
    Result := TwsLIVec.Create(Hi-Lo+1);
    j := 0;
    for i := Lo to Hi do begin
      Inc(j);
      Result[j] := i
    end
  end
end; { Index }

function TermKron(C: TList): TwsVec;
{ Objetivo
    Retorna o vetor resultante do produto de Kronecker entre os vetores da lista
  Parâmetros
    C: Lista com o conjunto de vetores dos quais serão obtidos os produtos
  Observação
    Rotina utilizada principalmente pela classe TwsLinearModel para obtenção da matriz do modelo

}
var
  i: Integer;
  Temp: TwsVec;
begin
  if C.Count = 1 then
    begin
    Temp := C.Items[0];
    Result := TwsDFVec.Create(Temp.Len);
    for i := 1 to Temp.Len do                         { Copia para o resultado }
      Result[i] := Temp[i];
    Exit
    end;
  Result := TwsVec(C.Items[0]).Kronecker(C.Items[1]);     { Entre os dois primeiros }
  Temp := Result.Copy(1, Result.Len);
  for i := 2 to C.Count - 1 do  { Completa o produto com os demais vetores }
    begin
    Result := Temp.Kronecker(C.Items[i]);
    Temp.Free;
    Temp := Result.Copy(1,Result.Len);
    end;
  Temp.Free;
end; { TermKron }

{ Estudar bem estas duas rotinas }
function VecAppend(Var X: TwsVec; Y: TwsVec; NewVec:boolean; Amt: Integer): TwsVec; {Adriano} {08/12/1997}
{ Objetivo
    Concatena valores de um vetor em outro
  Parâmetros
    X: Vetor onde os valores serão concatenados
    Y: Vetor cujos valores serão concatenados
    NewVec: Se True se a operação deverá gerar um novo vetor, se False o resultado volta em X
    Amt: Quantidades de valores que serão concatenados, inciando pelo primeiro

}
var
  i: Integer;
begin
  Result := TwsDFVec.Create(x.len+Amt);
  for i := 1 to x.len do
      Result[i] := x[i];
  for i := 1 to Amt do
      Result[x.len+i] := y[i];
  if not NewVec then
  begin
    X.Free;       {Verificar se é para tirar (Alex 15/10/97)}
    X := Result;
  end;
end; { VecAppend }

function iVecAppend(Var X:TwsLIVec; Y: TwsLIVec; NewVec:boolean; Amt: Integer): TwsLIVec; {Adriano} {08/12/1997}
{ Objetivo
    Concatena valores de um vetor em outro
  Parâmetros
    X: Vetor onde os valores serão concatenados
    Y: Vetor cujos valores serão concatenados
    NewVec: Se True se a operação deverá gerar um novo vetor, se False o resultado volta em X
    Amt: Quantidades de valores que serão concatenados, inciando pelo primeiro

}
//var
//  i: Integer;
var
  i: Integer;
begin
  Result := TwsLIVec.Create(x.len+Amt);
  for i := 1 to x.len do
      Result[i] := x[i];
  for i := 1 to Amt do
      Result[x.len+i] := y[i];
  if not NewVec then
  begin
    X.Free;       {Verificar se é para tirar (Alex 15/10/97)}
    X := Result;
  end;
(*  Result := VecCreate(x.len+Amt);
  for i := 1 to x.len do Result[i] := x[i];
  for i := 1 to Amt do Result[x.len+i] := y[i];
  if not NewVec then
  begin
    X.Free;       {Verificar se é para tirar (Alex 15/10/97)}
    X := Result;
  end;
*)
end; { VecAppend }

function VScalarAppend(y: TwsVec; x: Double; SFirst: Boolean): TwsVec; { ***** 02/05/97 Amauri }
{ Objetivo
    Concatena um escalar num vetor
  Parâmetros
    y: Vetor onde o valor será concatenado
    x: Valor a concatenar
    SFirst: Se True entao o valor será inserido no final, senão será inserido na primeira posição

}
var
  i, n: Integer;
begin
  n := y.len;
  Result := TwsDFVec.Create(n+1);
  if SFirst then
    with Result do begin
      Data[1] := x;
      for i := 1 to n do
        Data[i+1] := y[i]
    end
  else
    with Result do begin
      for i := 1 to n do Data[i] := y[i];
      Data[n+1] := x;
    end;
end; { ScalarVecAppend }

function ScalarAppend(y, x: Double): TwsVec;          { ***** 02/05/97 Amauri }
{ Objetivo
    Concatena dois escalares construindo um vetor de tamanho 2
  Parâmetros
  x, y: Escalares a concatenar

}
begin
  Result := TwsDFVec.Create(2);
  Result[1] := y; Result[2] := x;
end; { ScalarAppend }

function VecGenerate(Lo, Hi, Step: Double; Var ErrCode : Word): TwsVec;
{ Objetivo
    Gera um novo vetor cujos valores são especificados por um valor inicial, um valor final e um passo
  Parâmetros
    Lo, Hi, Step: Valor inicial, final e passo (diferença entre dois valores consecutivos
    ErrCode: Código de erro
  ErrCode retorna codigo de erro se (Lo - Hi) tem o mesmo sinal de Step.
  Exs.:
  VecGenerate(5 ,10, 2) = [5, 7, 9]
  VecGenerate(5 ,10,-2) = Erro
  VecGenerate(10, 5,-2) = [10, 8, 6]
  VecGenerate(10, 5, 2) = Erro
}
var
  i: Integer;
begin
  ErrCode := 0;
  if ((Lo>Hi) and (Step>0)) or  ((Lo<Hi) and (Step<0) ) then
    begin
    ErrCode := 1; {verificar codigo adequado de erro ????????????????????}
    Result := nil;
    Exit;
    end;
  Result := TwsDFVec.Create(Trunc((Hi-Lo)/Step)+1);
  Result[1] := Lo;
  for i := 2 to Result.Len do
    Result[i] := Result[i-1]+Step;
end; { VecGenerate }

function  VecConst(Value: Double; L: Integer; VR: Boolean=True): TwsVec;
{ Objetivo
    Cria um vetor com valores constantes de tamanho especificado
  Parâmetros
    Value: Valor comum a todos os componentes
    L: Tamanho do vetor
    VR: True se o vetor é de reais, false se for de inteiros
}
var
  aux,i: Integer;
begin
  if VR then
    begin
    Result := TwsDFVec.Create(L);
    for i := 1 to L do
      Result[i] := Value
    end
  else
    begin
    Result := TwsLIVec.Create(L);
    aux:=Trunc(Value);
    for i := 1 to L do
      Result[i] := aux
    end
end;

{ Objetivo
    Obtém um vetor a partir de um string PChar
  Parâmetros
    P: String com os valores
  Observações
    Os delimitadores dos elementos são: [#9,#10,#13,' ','=','\', '"','(',')']
}
function StrToVec(P: PChar): TwsVec;
const
  BufSize = 250;
var
  i, j: Integer;
  S: String;
  F: PFArray;
begin
  GetMem(F, sf(BufSize));
  j := 0;
  i := 0;
  Result := TwsDFVec.Create(0);
  repeat
    Inc(i);
    S:=StrGet(P, j, DelChar);
    if S <> '' then F^[i] := FracToReal(S);
    if (i = BufSize) or (S = '') then begin
      Result.Append(F, i-1);
      i := 0
    end
  until (S = '');
  FreeMem(F, sf(BufSize))
end; { StrToVec }

{ Objetivo
    Obtém um vetor a partir de um string
  Parâmetros
    P: String com os valores
  Observações
    Os delimitadores dos elementos são: [#9,#10,#13,' ',',','=','\', '"','(',')']
===> Cuidar a virgula como delimitador
}
function StrVec(P: string): TwsDFVec;
const
  BufSize = 250;
  DelChar: TCharSet = [#9,#10,#13,' ','=','\', '"','(',')'];
var
  i, j: Cardinal;
  S   : String;
  F   : PFArray;
begin
  if P <> '' then
    begin
    GetMem(F, sf(BufSize));
    j := 1;
    i := 0;
    Result := TwsDFVec.Create(0);
    repeat
      Inc(i);
      S:=StrToken(P,j,DelChar);
      if S <> '' then F^[i] := FracToReal(S);
      if (i=BufSize) or (S='') then
        begin
        Result.Append(F,i-1);
        i := 0
        end
    until (S = '');
    FreeMem(F, sf(BufSize))
    end
  else
    Result:=nil
end; { StrVec }

function BivariateRanks(var x, y, rx, ry: TwsVec): TwsVec;
{ Objetivo
    Retorna os postos bivariados de dos vetores especificados.
  Parâmetros
    x, y: Vetor para obtenção dos postos
    rx, ry: Vetores os retornam os postos com empates

}
var
  i, j: Integer;
  u1, u2: Double;
begin
  Result := TwsDFVec.Create(x.len);
  rx := x.RankTie;
  ry := y.RankTie;
  for i := 1 to x.len do
    begin
    Result[i] := 0.75;
    for j := 1 to x.len do
      begin
      if rx[i] > rx[j] then
        u1 := 1
      else
        if rx[i] < rx[j] then
          u1 := 0
        else
          u1 := 0.5;
      if ry[i] > ry[j] then
        u2 := 1
      else
        if ry[i] < ry[j] then
          u2 := 0
        else
          u2 := 0.5;
      Result[i] := Result[i] + u1*u2
      end
    end
end;

Function wsIndexCols(Const Cols: String; Colunas: TStrings): TwsLIVec;
const
  BuffLen = 500;
  DelChar: TCharSet = [#9,#10,#13,' ',',','=','\', '"','(',')'];
var
  w    :PFArray;
  Ind  :Integer;
  k,l  :Cardinal;
  s    :String;

begin
  GetMem(w, sf(BuffLen));
  Try
    k := 1;
    l := 1;
    Result := TwsLIVec.Create(0);

    s := StrToken(Cols, k, DelChar);
    while (s <> '') do
      begin                               { Obtem as colunas }
      w^[l] := Colunas.IndexOf(s) + 1;

      If w^[l] = 0 Then
         Begin
         Result.Free;
         Raise Exception.CreateFmt('Função: %s'#13 +
                                   MsgInvalidNameVar,
                                   ['wsIndexCols', s]);
         End;

      Inc(l);
      {Se a variavel não existe não faz nada}

      s := StrToken(Cols, k, DelChar);
      if (l = BuffLen) or (s = '') then
         begin
         Result.Append(w, l-1);
         l := 1;
         end

      End; {While}

  Finally
    FreeMem(w, sf(BuffLen));
  End;
end;


function BVecEQ(v1, v2: TwsVec): Double;
{ Objetivo
    Compara todos os elementos dos vetores especificados e retorna 1 se todos os valores sao iguais; 0 caso contrário
  Parâmetros
    v1, v2: vetores cujos valores serão comparados

}
var
  i: Integer;
begin
  Result := ScalarFalse;
  if v1.len = v2.len then begin
    Result := ScalarTrue;
    i := 1;
    while (Result=ScalarTrue) and (i <= v1.len) do begin
      Result := ScalarEQ(v1[i],v2[i]);
      Inc(i)
    end
  end
end; { BVecEQ }

function BVecLT(v1, v2: TwsVec): Double;
{ Retorna 1 se todos os valores de v1 sao menores que os valores de v2; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  if v1.len = v2.len then begin
    Result := ScalarTrue;
    i := 1;
    while (Result=ScalarTrue) and (i <= v1.len) do begin
      Result := ScalarLT(v1[i],v2[i]);
      Inc(i)
    end
  end
end; { BVecLT }

function BVecLE(v1, v2: TwsVec): Double;
{ Retorna 1 se todos os valores de v1 sao menores ou iguais aos valores de v2; 0 caso
  contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  if v1.len = v2.len then begin
    Result := ScalarTrue;
    i := 1;
    while (Result=ScalarTrue) and (i <= v1.len) do begin
      Result := ScalarLE(v1[i],v2[i]);
      Inc(i)
    end
  end
end; { BVecLE }

function BVecGT(v1, v2: TwsVec): Double;
{ Retorna 1 se todos os valores de v1 sao maiores que os valores de v2; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  if v1.len = v2.len then begin
    Result := ScalarTrue;
    i := 1;
    while (Result=ScalarTrue) and (i <= v1.len) do begin
      Result := ScalarGT(v1[i],v2[i]);
      Inc(i)
    end
  end
end; { BVecGT }

function BVecGE(v1, v2: TwsVec): Double;
{ Retorna 1 se todos os valores de v1 sao maiores ou iguais aos valores de v2; 0 caso
  contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  if v1.len = v2.len then begin
    Result := ScalarTrue;
    i := 1;
    while (Result=ScalarTrue) and (i <= v1.len) do begin
      Result := ScalarGE(v1[i],v2[i]);
      Inc(i)
    end
  end
end; { BVecGE }

function BVecNE(v1, v2: TwsVec): Double;
{ Retorna 1 se todos os valores de v1 sao diferentes dos valores de v2; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  if v1.len = v2.len then begin
    Result := ScalarTrue;
    i := 1;
    while (Result=ScalarTrue) and (i <= v1.len) do begin
      Result := ScalarNE(v1[i],v2[i]);
      Inc(i)
    end
  end
end; { BVecNE }

function BVecScalarAllEQ(v: TwsVec; const x: Double): Double;
{ Retorna 1 se todos os valores de v sao iguais aos valores de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarTrue;
  i := 1;
  while (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarEQ(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAllLT(v: TwsVec; const x: Double): Double;
{ Retorna 1 se todos os valores de v sao menores que os valores de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarTrue;
  i := 1;
  while (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarLT(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAllLE(v: TwsVec; const x: Double): Double;
{ Retorna 1 se todos os valores de v sao menores ou iguais aos valores de x; 0 caso
  contrario }
var
  i: Integer;
begin
  Result := ScalarTrue;
  i := 1;
  while (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarLE(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAllGT(v: TwsVec; const x: Double): Double;
{ Retorna 1 se todos os valores de v sao maiores aos valores de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarTrue;
  i := 1;
  while (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarGT(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAllGE(v: TwsVec; const x: Double): Double;
{ Retorna 1 se todos os valores de v sao maiores ou iguais aos valores de x; 0 caso
  contrario }
var
  i: Integer;
begin
  Result := ScalarTrue;
  i := 1;
  while (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarGE(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAllNE(v: TwsVec; const x: Double): Double;
{ Retorna 1 se todos os valores de v sao diferentes dos valores de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarTrue;
  i := 1;
  while (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarNE(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAnyEQ(v: TwsVec; const x: Double): Double;
{ Retorna 1 se algum valor de v e igual ao valor de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  i := 1;
  while not (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarEQ(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAnyLT(v: TwsVec; const x: Double): Double;
{ Retorna 1 se algum valor de v e menor que o valor de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  i := 1;
  while not (Result=ScalarTrue) and (i <= v.len) do begin
    Result := ScalarLT(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAnyLE(v: TwsVec; const x: Double): Double;
{ Retorna 1 se algum valor de v e menor ou igual ao valor de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  i := 1;
  while (Result=ScalarFalse) and (i <= v.len) do begin
    Result := ScalarLE(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAnyGT(v: TwsVec; const x: Double): Double;
{ Retorna 1 se algum valor de v e maior que o valor de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  i := 1;
  while not (Result=ScalarFalse) and (i <= v.len) do begin
    Result := ScalarGT(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAnyGE(v: TwsVec; const x: Double): Double;
{ Retorna 1 se algum valor de v e maior ou igual ao valor de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  i := 1;
  while (Result=ScalarFalse) and (i <= v.len) do begin
    Result := ScalarGE(v[i],x);
    Inc(i)
  end
end;

function BVecScalarAnyNE(v: TwsVec; const x: Double): Double;
{ Retorna 1 se algum valor de v e diferente do valor de x; 0 caso contrario }
var
  i: Integer;
begin
  Result := ScalarFalse;
  i := 1;
  while (Result=ScalarFalse) and (i <= v.len) do begin
    Result := ScalarNE(v[i],x);
    Inc(i)
  end
end;

function VecMult(v1, v2: TwsVec; Var ErrCode: Word): Double;
{ Objetivo
    Retorna a soma de produtos de dois vetores
  Parâmetros
    v1: Primeiro vetor
    v2: Segundo vetor
    ErrCode: Código de erro. Retorna 0 (zero) se o produto é possível (dimensão dos vetores
      deve ser a mesma; NImprDim caso contrário
  Valores perdidos
    Não considera
}
var
  i: Integer;
begin
  ErrCode := 0;
  if (v1.len = v2.len) then
    begin
    Result := 0;
    for i := 1 to v1.len do
      Result := Result + v1[i] * v2[i];
    end
  else
    Begin
    ErrCode := NImprDim;
    Result  := wscMissValue;
    End;
end; { VecMult }

(*
function VecSort(y: TwsVec; NewVec, Ascd: Boolean): TwsVec; { Amauri 06/05/97 }
{ Ordena o vetor y na ordem estabelecida por Ascd (True=Ascendente). Se NewVec=True o
  resultado retorna num novo vetor; caso contrario o vetor y e que sera ordenado.}
Begin
  if NewVec then
    Result:=y.Copy(1,y.Len)
  else
    Result := y;
  Result.QuickSort(Ascd)
End; { VecSort }
*)

Function VFromFile(Const Name:String):TwsVec;
{ Le um vetor armazenado no stream Name }
  Var
    Stream  :TFileStream;
    Reader  :TReader;
  Begin
    Stream:=TFileStream.Create(Name,fmOpenRead);
    Try
      Reader:=TReader.Create(Stream,$FF);
      Try
        Result:=TwsVec.VFromStream(Reader);
      Finally
      End;
    Finally
      Stream .Free;
    End;
  End; { VFromfile }

function FAmpVal(const StIni, StEnd, StDelt: String): TwsDFVec;
{ Objetivo
    Retorna um vetor de valores tipo Double
  Parametros
    stIni: Sequencia que indica o valor inicial do vetor
    StEnd: Sequencia que indica o valor final do vetor
    StDelt: Sequencia que indica o acrescimo para obtencao de cada valor do vetor
  Observacoes
    Se ocorrer alguma excecao, a funcao retornara nil
}
var
  v1, v2, Delt: Double;
  i,n: Integer;
begin
  Result := nil;
  try
    v1 := StrToFloat(StIni);
    v2 := StrToFloat(StEnd);
    delt := StrToFloat(StDelt);
    n := Round((v2 - v1)/delt + 1); // tamanho do vetor
    Result := TwsDFVec.Create(n);
    Result[1]:=v1;
    for i := 2 to n do
      Result[i]:= Result[i-1]+delt;
  Except
    Result.Free;
    Result := nil
  end;
end;

function IAmpVal(const StIni, StEnd, StDelt: String): TwsLIVec;
{ Objetivo
    Retorna um vetor de valores inteiros
  Parametros
    stIni: Sequencia que indica o valor inicial do vetor
    StEnd: Sequencia que indica o valor final do vetor
    StDelt: Sequencia que indica o acrescimo para obtencao de cada valor do vetor
  Observacoes
    Se ocorrer alguma excecao, a funcao retornara nil
}
var
  v1, v2, Delt: Integer;
  i,n: Integer;
begin
  Result := nil;
  try
    v1 := StrToInt(StIni);
    v2 := StrToInt(StEnd);
    delt := StrToInt(StDelt);
    n := Round((v2 - v1)/delt + 1); // tamanho do vetor
    Result := TwsLIVec.Create(n);
    Result[1]:=v1;
    for i := 2 to n do
      Result[i]:= Result[i-1]+delt;
  Except
    Result.Free;
    Result := nil
  end;
end;

function VecCopy(V: TwsVec; Start, Amt: Longint): TwsVec;
var i: Longint;
begin
  if Start < 1 then
     begin
     Result := Nil;
     exit;
     end;

  Amt := Min(Amt, Succ(V.Len - Start));

  if Amt <= 0 then
     begin
     Result := Nil;
     exit;
     end;

  Result := TwsDFVec.Create(amt);
  Result.Name := V.Name;
  for i := 0 to Amt-1 do
    Result[i+1] := V[Start+i];
end; { VecCopy }

function VecScalarProd(y: TwsVec; const x: Double; NewVec: Boolean = False): TwsVec;
var i: Longint;
begin
  if NewVec then
     Result := TwsDFVec.Create(y.len)
  else
     Result := y;

  for i := 1 to Result.Len do
    Result[i] := ScalarProd(y[i], x)
end;

const
  cErroTamanho = 'Os vetores não possuem o mesmo tamanho !';

function VecSum(v1, v2: TwsVec; NewVec: Boolean): TwsVec;
var i: Longint;
begin
  if v1.len = v2.len then
     begin
     if NewVec then
        Result := TwsDFVec.Create(v1.len)
     else
        Result := v1;

     for i := 1 to v1.len do
       Result[i] := ScalarSum(v1[i], v2[i])
     end
  else
     Raise Exception.Create('VecSum: ' + cErroTamanho);
end; { VecSum }

function VecSub(v1, v2: TwsVec; NewVec: Boolean): TwsVec;
var i: Longint;
begin
  if v1.len = v2.len then
     begin
     if NewVec then
        Result := TwsDFVec.Create(v1.len)
     else
        Result := v1;

     for i := 1 to v1.len do
       Result[i] := ScalarSub(v1[i], v2[i])
     end
  else
     Raise Exception.Create('VecSum: ' + cErroTamanho);
end; { VecSub }

function VecProd(v1, v2: TwsVec; NewVec: Boolean): TwsVec;
var i: Longint;
begin
  if v1.len = v2.len then
     begin
     if NewVec then
        Result := TwsDFVec.Create(v1.len)
     else
        Result := v1;

     for i := 1 to v1.len do
       Result[i] := ScalarProd(v1[i], v2[i])
     end
  else
     Raise Exception.Create('VecProd: ' + cErroTamanho);
end; { VecProd }

// Calcula o coeficiente de correlação entre X e Y.
// Estes vetores devem possuir o mesmo número de elementos.
// Os dados entre BeginIndex e EndIndex deverão ser válidos.
// Fórmula utilizada:
//   Rxy = Sxy / (Sx * Sy)
// onde
//   Sxy = Co-variança amostral
//   Sx  = Desvio Padrão de X
//   Sy  = Desvio Padrão de Y
Function PartialCorrelation(X, Y: TwsVec; BeginIndex, EndIndex: Integer): Double;
var Mx, My: Double; // Médias
    i: Integer;
    nx: Integer;
begin
  if X.Len <> Y.Len then
     raise Exception.Create('Correlation: Os vetores devem possuir o mesmo tamanho');

  Mx := X.PartialMean(BeginIndex, EndIndex, nx);
  My := Y.PartialMean(BeginIndex, EndIndex, nx);

  // Cálculo da Co-Variança amostral
  Result := 0;
  for i := BeginIndex to EndIndex do
       Result := Result + (X[i] * Y[i]);
  Result := (Result / nx) - (Mx * My);

  Result := Result / (X.PartialVecDSP(BeginIndex, EndIndex, Mx) *
                      Y.PartialVecDSP(BeginIndex, EndIndex, My));
end;

function BinCo(n, k: Integer; F: TwsVec): Double;
{ Objetivo
    Retorna o valor do coeficiente binomial
  Parametros
    n, k: Valores para o calculo do coeficiente Bin(n, k)
    F: Vetor auxiliar para o calculo dos fatoriais
}
begin
  Result := Round(exp(FactLn(n, F) - FactLn(k, F) - FactLn(n-k, F)))
end; (* BinCo *)

function FactLn(n: Integer; F: TwsVec): Double;
{ Objetivo
    Retorna o logaritmo neperiano do fatorial do valor especificado
  Parametros
    n: Valor para o calculo do fatorial
    F: Vetor auxiliar no calculo do fatorial. O calculo sera feito atraves da funcao LnGamma
       somente se ainda nao estiver disponivel. Na entrada, F devera possuir 100 posicoes
       (para armazenar os fatoriais de 0 a 99), com valores negativos nas posicoes para as
       quais ainda nao existem valores da funcao calculados.
}
begin
  // se n esta entre 0 e 99, podera ter o valor ja disponivel na tabela
  if n <= 99 then
    begin
    if F[n+1] < 0 then  // se valor na posicao e negativo, preenche
      F[n+1] := GammLn(n+1);
    Result := F[n+1] // e retorna
    end
  else
    Result := GammLn(n+1); // senao calcula mas nao guarda
end; (* FactLn *)

function Factrl(n: Integer; F: TwsVec; var FactNTop: Integer): Double;
{ Objetivo
    Obtem o fatorial de n construindo antes uma serie de valores para os fatoriais para
    uma utilizacao sequencial desses valores.
  Parametros
    n: Valor para o qual se deseja calcular o fatorial.
    F: Vetor que armazena todos os fatoriais ja calculados. Sua dimensao deve ser 33. Fatoriais
       de 0 a 32 poderao ser armazenados aqui, para consulta posterior. Se, por exemplo, existirem
       valores para as primeiras 5 posicoes, entao FactNTop = 5 e as cinco primeiras posicoes serao
       [1, 1, 2, 6, 24, ... (da 6a. ate 33a. estarao vazias)] FactNTop: Numero de valores para os
       quais ja existem fatoriais calculados disponiveis na tabela
  Obs.:
    As situacoes sao as seguintes:
      Se (n <= FactNTop) --> retorna valor da tabela
      senao
        se (n<33) completa a tabela ate o valor desejado e retorna o valor desejado
        senao
          calcula o fatorial atraves da funcao LnGamma
}
var
  j: Integer;
begin
  if n >= 0 then
    begin
    if n <= FactNTop then                       // ja esta na tabela
      Result := F[n+1]
    else
      if n < 33 then
        begin                                   // ainda pode completar a tabela
        for j := FactNTop + 1 to n do           // completa a tabela de FactNTop ate n
          F[j+1] := j*F[j];
        FactNTop := n;                          // ajusta novo tananho da tabela
        Result := F[n+1]
        end
      else
        Result := exp(GammLn(n + 1.0))          // senao calcula atraves da funcao LnGamma
    end
  else
    Result := wscMissValue
end; (* Factrl *)

function UnifValues(k,n,c: Integer): TwsDFVec;
{ Objetivo
    Obtém uma coleção de números (inteiros) uniformes na amplitude desejada
  Parâmetros
    n: limite máximo dos valores que serão gerados
    k: número de valores a serem obtidos
    c: os valores obtidos estarão na amplitude de c ate n-c+1. Por exemplo, se c=0
       os valores estarão entre 0 e n-1; se c=1, entre 1 e n e assim por diante.
}
var
  i: Integer;
  U: TwsLinear;
begin
  Result:=TwsDFVec.Create(k);
  U:=TwsLinear.Create;
  for i:=1 to k do
    Result[i] := Int(n*U.Generate)+c;
end;

function QNorm(p: TwsVec): TwsDFVec;
{Objetivo
   Obtém um vetor de quantis normais
 Parâmetros
   p: Vetor de probabilidades para os quais serão obtidos os quantis
 Retorno
   Vetor de dimensão p.Len com os quantis normais  
}
var
  i  : Integer;
  Nor: TwsProbStdNormal;
begin
  Nor:=TwsProbStdNormal.Create;
  Result:=TwsDFVec.Create(p.Len);
  for i:=1 to p.Len do
    Result[i]:=Nor.Quantil(p[i]);
  Nor.Free
end;

function ValNorm(n: Integer): TwsDFVec;
{Objetivo
   Obtém um vetor de valores simulados com distribuição normal
 Parâmetros
   n: número de valores a serem gerados
 Retorno
   Vetor de dimensão n com os valores gerados     
}
var
  i  : Integer;
  Nor: TwsNormal01;
begin
  Nor:=TwsNormal01.Create;
  Result:=TwsDFVec.Create(n);
  for i:=1 to n do
    Result[i]:=Nor.Generate;
  Nor.Free
end;

function SeqBinCoef(n, k: Integer; var Coef: TwsVec): Double;
{ Objetivo
    Calcula os coeficientes binomiais através de um processo de recorrência, obtendo
      Bin(n,x+1) como (n-x)/(x+1)*Bin(n,x) e retorna um vetor com os coeficientes binomiais
      de 0 a k e o valor do coeficiente desejado.
  Parametros
    n, k: Valores para o calculo do coeficiente
    Coef: Vetor de dimensao k+1, que armazena os coeficientes de 0 a k. Na primeira chamada
          Coef deve receber valor nil (importante). A rotina podera ser chamada para completar a
          sequencia de valores, se isso for necessario.
  Observacoes
    * A rotina calcula somente os valores necessarios e utiliza a propriedade do complemento
      para obter os demais valores.
    * A vantagem da utilizacao do processo recorrente e a dispensa da necessidade do calculo
      de fatoriais. A desvantagem e a necessidade da obtencao de todos os valores antes
      daquele de interesse.
}
var
  i,k1,k2,k3,OldLen: Integer;
  aux: TwsVec;
begin
  if ((n > 0) and (k>=0) and (k<=n)) then
      begin
      k2 := k+1;
      if Coef = nil then
         begin                                // um novo vetor sera criado
         Coef := TwsDFVec.Create(k2);
         Coef[1] := 1;
         OldLen := 0;
         end
      else
         begin
         OldLen := Coef.Len;                 // o vetor existente sera completado
         Aux := TwsDFVec.Create(k2);
         for i:=1 to OldLen do               // Copia conteudo antigo
             Aux[i] := Coef[i];
         Coef.Free;                          // Abandona conteudo antigo
         Coef := Aux;                        // e pega o novo
         end;
      // Para calcular somente os valores necessarios
      k3 := Min(n div 2, k)-1;
      for i := OldLen to k3 do              // Calcula um novo vetor ou completa o existente
          begin
          k1:=i+2;
          Coef[k1] := Coef[i+1]*(n-i)/(i+1);     // Calcula utilizando a relacao de recorrencia
          end;
      for i := k3+2 to k do
          Coef[i+1] := Coef[n-i+1]; // Completa utilizando a propriedade da complementariedade
      Result := Coef[k2];
      end
    else
      Result := wscMissValue
end; { SeqBinCo }

initialization
  wsm_VectorChange := getMessageManager.RegisterMessageID('wsm_VectorChange');

end.
