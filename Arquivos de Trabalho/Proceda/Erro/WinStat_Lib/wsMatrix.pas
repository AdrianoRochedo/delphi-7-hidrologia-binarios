unit wsMatrix;

{
    MODIFICACOES

    

        23/09/96   - Inclusao de Modificacoes do Adriano:         - Roger

                        - Inclusao de propriedades
                        - Alteracao em ToChar de alguns Objetos

        09/01/97   - Inclusao de TFGeneral                        - Roger

        17/01      - Altera??o em TMatrix.SetRow. Agora cont?m
                     valida??o de linha e coluna e libera??o de
                     mem?ria em caso de atribui??o de nova linha  - Roger e

                   - Alteracao do nome do metodo Row para
                     PutLine                                      - Roger

        06/02      - BugFix em TMatrix.Print                      - Roger

        06/03      - Inclusao de TMatrix.SaveToFile               - Roger

        19/06      - Tentativa de adi??o de Propriedade Default   - Rochedo
                     Falhou pois ja existe uma na hierarquia da
                     classe.

                   - Inclus?o de WS nos nomes dos tipos           - Roger

        04/05/1998 - Inclus?o da propriedade MODIFIED             - Rochedo


  AUTOR E DATA: ........................ Rochedo, 17/06/1998
  ESCOPO: .............................. Todas as variaveis inteiras de 2 bytes
                                         passam a ter 4 bytes.
                                         (Integer, Word) --> (Integer)

  AUTOR E DATA: ........................ Rochedo, 19/06/1998
  ESCOPO: .............................. Criando o m?todo DeleteCol para TMatrix
  OBS.: ................................ Cuidado ao utilizar DeleteCol em classes
        descendentes de TwsMatrix, pois alguns descs. n?o s?o matrizes quadradas e
        portanto seus valores s?o guardados de forma especial.
        Portanto, por enquanto s? utilizar em descendentes de TwsGeneral  <-------

  22/07/1999 - Rochedo
    - Reformula??o dos m?todos de leitura e salvamento
      - Foram criados 3 m?todos de cada procedimento.
        Leitura (InternalLoad1, InternalLoad2, InternalLoad3).
        Salvamento (InternalSave1, InternalSave2, InternalSave3).
        Para os m?todos de salvamento poder?amos fundir os m?todos InternalSave1/2 num
        s?, mas para manter a simetria com os m?todos de leitura decidi separ?-los.

  AUTOR E DATA: ...... Alex, 22/02/2000
  ESCOPO: ............ Adicionada a linha
                         Result.FModified := False;
                       no m?todo wsMatrix.LoadFromFile.

                       Adicionada a linha
                         FModified := True;
                       no m?todo wsMatrix.SetName

  Autor e data......... Amauri, 22/04/200
  Escopo .............. Revisao profunda das funcoes que foram estabelecidas como m?todos
                        Revisao dos metodos de impressao

  Autor e data......... Adriano, 09/07/2003
  Escopo .............. Implementa??o dos m?todos Save e Load para arquivos ini
}

interface
uses Types,
     Classes,
     IniFiles,
     DateUtils,
     SysUtilsEx,
     MessageManager,
     Math,
     XML_Utils,
     XML_Interfaces,
     {$ifdef Planilha}
     WinUtils,
     Dialogs,                    
     BaseSpreadSheetBook,
     {$endif}
     {$ifdef MSXML}
     MSXML4,
     {$endif}
     wsVec,
     wsCVec,
     wsConstTypes,
     Graphics; // TColor

const
  cLimiteDeLinhasDaPlanilhaF1Book = 16000;

// Identificadores de Mensagens
// Estas vari?veis s?o inicializadas automaticamente na se??o de inicializa??o
var
  wsm_MatrixChange: Integer;            // Mensagem de notifica??o de altera??es

type
  TwsEnumMatType = (mtGeneral,
                    mtSymmetric,
                    mtDiagonal,
                    mtTriangular,
                    mtVandermonde,
                    mtToeplitz,
                    mtDataSet,
                    mtFDataSet,
                    mtNull);

  TwsExtraData = Array[0..1023] of Byte;

  { Declaracoes forward }
  TwsMatrix = class;
  TwsGeneral = class;
  TwsSymmetric = class;
  TwsDiagonal = class;
  TwsTriangular = class;
  TwsDataSet = class;
  TwsDataSets = class;

  TwsStatusChangeSet = set of (scLock, scFileName, scData, scStruct);

  TwsStatusChange_Event = procedure (Sender: TwsMatrix; Status: TwsStatusChangeSet) of object;
  TwsSelectElementColor_Event = procedure (Sender: TwsMatrix; Row, Col: Integer; var Color: TColor) of object;

  { Heran?a
      TwsMatrix --> T_NRC_InterfacedObject
    Objetivo
      Implementar uma classe gen?rica para tratamento de matrizes. Possibilita tratar
      matrizes de v?rios tipos: geral, sim?trica, mtDiagonal, mtTriangular, mtToeplitz e
      mtVandermonde
  }

  TwsMatrix = class(T_NRC_InterfacedObject, IToBXML)
  private
    FLocked: Integer;
    FList: TList;
    FStatusChange: TwsStatusChange_Event;
    FSelectElementColor: TwsSelectElementColor_Event;

    // Grava??o/recupera??o de partes do objeto matriz
    class function  InternalLoad1(Reader: TReader): TwsEnumMatType;
    procedure InternalLoad2(Reader: TReader);
    procedure InternalLoad3(Reader: TReader); virtual; abstract;
    procedure InternalSave1(Writer: TWriter);
    procedure InternalSave2(Writer: TWriter);
    procedure InternalSave3(Writer: TWriter); Virtual; Abstract;
    // Recupera elemento como string
    function GetStr(i,j: Integer): String; virtual;
    // Atribui elemento especificado como string
    Procedure PutStr(i,j: Integer; X: String); virtual;
    // Recupera elemento
    function  Get(i,j: Integer): Double; virtual; abstract;
    // Atribui elemento
    procedure Put(i, j: Integer; x: Double); virtual; abstract;
    function getLocked: Boolean;
    function getGroup: String;
  Protected
    // Objeto alterado?
    FModified: Boolean; {Rochedo, 04/05/1998}
    FName        : String;   { Nome }
    FNCols       : Integer;  { Numero de colunas }
    FCName       : TStrings; { Lista dos nomes de colunas }
    FMatType     : TwsEnumMatType; { Tipo da matriz }
    FMLab        : PString;  { R?tulo da matriz }
    FNRows       : Integer;  { Numero de linhas }
    FVersion     : String [10]; {Versao do arquivo}
    FExtra       : TwsExtraData; {Campo extra para futuro uso.}

    // Retorna sim ou n?o se a descri??o das vari?veis deve ser impressa
    function PrintDescVars: Boolean; virtual;
    // Obtem os dados do campo extra
    function getExtraData(i: Integer): byte;
    // seta os dados do campo extra
    procedure setExtraData(i: integer; Value: byte);
    // atribui n?mero de linhas
    procedure SetNRows(NR: Integer);
    // Atribui nome da matriz
    procedure SetName(Const Name: String); virtual;
    // IToBXML interface - Retorna o nome do bloco BXML
    function GetBlockName: String; virtual;
    // IToBXML interface - Retorna o coment?rio explicativo do bloco BXML
    function GetBlockNameComment: String; virtual;
    // Recupera r?tulo
    Function  GetMLab:String;
    // Atribui r?tulo
    Procedure PutMLab(Const l:String);
    // Recupera endere?o de linha
    function  GetRow(i: Integer): TwsVec;  Virtual;
    // Atribui endere?o de linha
    Procedure SetRow(i: Integer; v: TwsVec);
    // Recupera nome de linha
    function  GetRowName(i: Integer): string; virtual;
    // atribui nome de linha
    procedure SetRowName(i: Integer; const s: string);
    // Recupera nome de coluna
    function  GetColName(i: Integer): string;    Virtual;
    // Recupera nome de coluna
    procedure SetColName(i: Integer; S: String); Virtual;

    // Leitura/grava??o de parte da matriz
    Procedure CNameLoadFromStream(Reader:TReader); Virtual;
    Procedure CNameSaveToStream(Writer:TWriter);   Virtual;

    // Descreve as colunas como xml
    procedure StructColsToXML(Buffer: TStrings; var Ident: Integer); virtual;

    procedure RowsToXML(Writer: TXML_Writer); virtual;
    // Usada na cria??o automatica
    class function UserCreate: TwsMatrix; virtual;
    // Libera conte?do
    destructor  Destroy; override;
    {$ifdef MSXML}
    procedure fromXML_LoadStruct(no: IXMLDomNode); virtual;
    {$endif}
  Public
    // Op??es para impress?o
    PrintOptions: TwsRecPrintOptions;

    FileName    : String[80];      {Rochedo, 04/04/1998}
    { Tag - Pode guardar virtualmente qualquer coisa.
      Pode ser usado como um ponteiro, pois tem 4 bytes de comprimento}
    Tag_1      : Integer;         {Rochedo, 29/05/1998}
    Tag_2      : Integer;         {Rochedo, 29/05/1998}
    // Participa da cria??o de uma matriz
    constructor Create(NR, NC: Integer);

    // IToBXML interface **************************************************************

    // Implementa a defini??o dos dados como xml.
    // Dever? ser sobreescrito, se necess?rio, para matrizes de diferentes formatos.
    procedure toXML(Buffer: TStrings; Ident: Integer); virtual;
    // Implementa a defini??o dos dados como bxml
    procedure toBXML(Buffer: TStrings; Ident: Integer);
    // Retorna o tipo da Classe para o mecanismo de serializa??o em XML
    function GetClassName: String;

    procedure ShowInSheet(Sheet: TBaseSpreadSheetBook); virtual;

    // end IToBXML interface **********************************************************

    {$ifdef MSXML}
    procedure fromXML(no: IXMLDomNode);
    {$endif}

    // Verifica se a coluna esta ordenada
    function SortedCol(IndexCol: Integer; Ascd: Boolean): Boolean;

    // Retorna um valor da matriz na posi??o especificada
    function ColInd(Const s: string): string;
    // Valor nessa posi??o ? perdido?
    function IsMissValue(L, C: Integer; out x: Double): Boolean;
    // Operacao elemento a elemento
    function ByElement(B:TwsMatrix; op: TwsEnumTypeOp; NewMat:Boolean;
      var ErrCode:Word):TwsMatrix; virtual;
    // Transposta
    function Transpose: TwsMatrix; virtual;
    // Produto de matrizes
    function Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix; virtual;
    // Produto de Kronecker
    function Kronecker(X: TwsMatrix): TwsMatrix;
    // Produto de Kronecker por colunas
    function ColKronecker(X:TwsMatrix; i,j:Integer): TwsVec;
    // Produto de Kronecker horizontal (por linhas)
    function HorKronecker(X: TwsMatrix;Var ErrCode: Word): TwsMatrix;
    // Produto de Kronecker por um vetor
    function VecKronecker(x: TwsVec): TwsMatrix;
    // Produto de vetor por matrizes
    function VecMult(v: TwsVec; PreMult: Boolean; Var ErrCode: Word): TwsDFVec; virtual;
    // Operacoes com escalares
    function ByScalar(const x:Double; Op:TwsEnumTypeOp;
      SFirst,NewMat:Boolean):TwsMatrix; virtual; abstract;
    { Aplica fun??es matem?ticas aos elementos da matriz }
    function Func(fOp:TwsEnumConstFun; NewMat:Boolean): TwsMatrix;
    // Copia de matrizes
    procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); virtual;
    // Produto AB'
    function TranspMul1(B:TwsMatrix; var ErrCode:Word): TwsMatrix; virtual;
    // Produto A'B
    function TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix; virtual;
    // Produto AA'
    function TranspMul3: TwsMatrix; virtual;
    // Produto A'A
    function TranspMul4: TwsMatrix; virtual;
    // Produto A'BA
    function TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; virtual;
    // Produto v'Av
    function TranspMul6(v: TwsVec; var ErrCode: Word): Double; virtual;
    // Produto de submatrizes
    function TranspMul7(Lin1, Lin2: Integer): TwsSymmetric;
    // A'B com produto iniciando pela coluna Start
    function TranspMul8(B:TwsMatrix; Start:Integer; var ErrCode:Word):TwsGeneral;
    // retorna a(i)'*B*c(j), esgotando cada par (i,j)
    function TranspMul9(A,C: TwsMatrix; var ErrCode: Word): TwsGeneral;
    function TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; virtual;   { ABA' }
    // Toma coluna C[k] e faz o produto C[i]'*A*C[k]
    function TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double; virtual;
    // Submatriz numa faixa de ?ndices
    function SubMatrix(l, mb, k, nb: Integer): TwsGeneral;
    // Submatriz sim?trica
    function SubSymMat(l,mb:Integer): TwsSymmetric;
    // Submatriz com ?ndices especificados
    function SubIndex(R, C: TwsLIVec): TwsGeneral;
    // Reducao por coluna
    function ColReduc(R: TwsLIVec; Ch: TwsEnumTypeOp): TwsGeneral;
    // Redu??o por linha
    function RowReduc(C: TwsLIVec; Ch: TwsEnumTypeOp): TwsGeneral;
    // Redu??o por linha e coluna
    function RowColReduc(ChRow, ChCol: TwsEnumTypeOp): Double;
    // Estat?sticas descritivas
    function DescStat(Col, Stat: TwsLIVec): TwsGeneral;
    // Estat?sticas descritivas ponderadas
    function WDescStat(Col, Stat: TwsLIVec; WInd: Integer): TwsGeneral;
    // Estatisticas num vetor
    function VDescStat(Col, Stat: TwsLIVec): TwsVec;
    // Estat?sticas ponderadas num vetor
    function VWDescStat(Col, Stat: TwsLIVec; WInd: Integer): TwsVec;
    // Percentil de coluna especificada
    function Perc(const f: Double; Col: Integer): Double;
    // Estat?sticas de ordem
    function MatOrderStat(Col,Stat: TwsLIVec): TwsGeneral;
    // Fun??es aplicadas aos elementos da matriz
    function ScalarFunc(Op: TwsEnumConstFun; var n: Integer): Double;
    // Norma da coluna
    function Norm(j,l1,l2: Integer): Double;
    // Produto interno entre colunas
    function ColProd(j,k,l1,l2: Integer): Double;
    // Transfere o conte?do da matriz para um vetor
    function ToVec: TwsVec; virtual;
    // Transfere todo conte?do da matriz para um vetor
    function GToVec: TwsVec;
    // Transfere conte?do da mtDiagonal para um vetor
    function DiagToVec: TwsVec; virtual;
    // Matriz de postos com empates
    function RankTie(NewMat: Boolean): TwsMatrix;
    // Matriz de postos
    function Rank(NewMat: Boolean; Ascd: Boolean): TwsMatrix;
    // retorna o maximo e o minimo da coluna
    procedure ColExtrems(k: Integer; out Min, Max: Double);
    // Sweep didatico
    procedure G2Sweep(k: Integer; DMin: Double=1E-8); virtual; abstract;
    // Atribui nomes ?s colunas
    procedure SetCName(st: TwsCVec;DelChar :TCharSet);
    // Atribui nomes ?s linhas
    procedure SetRName(st: TwsCVec;DelChar :TCharSet); virtual;
    // Adiciona uma linha com o n. de elementos da matriz
    function AddRow(const Name: string = ''): TwsVec;
    // Insere o vetor na lista sem alterar outros atributos
    Procedure PutLine(V:TwsVec);  Virtual;
    //Insere uma linha (vetor) na posic?o especificada
    procedure MInsert(Pos: Integer; L: TwsVec);
    // Elimina a linha (vetor) especificada
    procedure MDelete(L: Integer); Virtual;
    {Deleta uma coluna de ordem i}
    procedure DeleteCol(i: Integer); Virtual;
    // Troca uma linha por outra
    Procedure MChange(i: Integer; V: TwsVec); virtual;
    // Remove somente os elementos das colunas (vetores) sem afetar os atributos das colunas
    procedure MDeleteCols(Col: TwsLIVec);
    // Deleta uma colunas especificadas
    procedure DeleteCols(Col: TwsLIVec); virtual; abstract;
    // Adiciona uma linha (vetor) e atualiza os respectivos campos
    procedure MAdd(L: TwsVec); Virtual;
    //Troca de posi??o duas linhas (vetores)
    procedure Exchange(i, j: Integer);
    // Copia uma coluna para um vetor
    function CopyCol(Col: Integer; IMiss: Boolean=False): TwsVec;
    // Copia conte?dos dss colunas para uma lista
    function CopyColsToList(Col: TwsLIVec; IMiss: Boolean=False): TList;
    // Verifica se a matriz ? do tipo especificado
    function IsType(AType: TwsEnumMatType): Boolean;
    // Retorna ?ndices de colunas num string
    function IndexColsFromString(const Col: String): TwsLIVec; Virtual;
    // Retorna ?ndices de colunas numa lista
    Function IndexColsFromStrings(Cols: TStrings): TwsLIVec; Virtual;
    // ?ndice da coluna pelo nome
    Function IndexByName(Const Name : String) : Integer; {Virtual}
    // Coloca colunas na ordem estabelecida pelos ?ndices
    procedure SortCol(Indx: TwsLIVec);
    // Impress?o
    procedure List(Buffer: TStrings); virtual;
    // Cabe?alho
    procedure Header(Start, Amt: Integer; Buffer: TStrings); virtual;
    // Atribui largura de impress?o
    procedure SetColWidth(Value: byte);
    // Impress?o
    procedure Print(Buffer: TStrings); virtual;
    // Impress?o de colunas opr condi??o
    procedure ColPrint(const Cols: String; Condicao: String; Buffer: TStrings); virtual; abstract;
    // Elementos da matriz num string
    function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; virtual;
    // Elementos selecionados da matriz num string
    procedure SelectToChar(L, Start, Amt: Integer; Col: TwsLIVec; Buffer: TStrings);
    // Cabe?alho de colunas selecionadas
    procedure SelectHeader(Col: TwsLIVec; Buffer: TStrings); Virtual;
    // Salva a matriz em um arquivo texto
    procedure ToText(var TFile: Text; IsLast: Boolean = True; LSep: Char = ';'; SSep: Char = '/');
    // transforma uma matriz em DataSet
    function ToDataSet: TwsDataSet;
    // Obt?m os elementos da matriz de um arquivo texto
    procedure TextTo(var TFile: Text; Ch: Char);
    // Colunas para impress?o
    function ColsToImp(var k: Integer): Integer; virtual;
    // Colunas selecionadas para impress?o
    function SelectColsToImp(var k: Integer; Col: TwsVec): Integer; Virtual;
    // Invers?o de matrizes
    procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); virtual; abstract;
    // Solu??o de sistemas de equa??es lineares
    procedure Solve(B: TwsMatrix; var ErrCode: Word); virtual; abstract;
    // Todos os valores s?o nulos?
    function All(const x: Double; Op: TwsEnumTypeOp): boolean;
    // Algum valor ? nulo ?
    function Any(const x: Double; Op: TwsEnumTypeOp): boolean;
    // Concatena??o de linhas
    function RowConcat(B: TwsMatrix;Var ErrCode: Word): TwsGeneral;
    // Concatena??o de colunas
    function ColConcat(B: TwsMatrix;Var ErrCode: Word): TwsGeneral;
    // Concatena??o de escalar nas linhas
    function RowScalarConcat(const x: Double; SFirst:boolean): TwsGeneral;
    // Concatena??o de escalar nas colunas
    function ColScalarConcat(const x: Double; SFirst:boolean): TwsGeneral;
    // Concatena??o de vetores nas linhas
    function RowVecConcat(v: TwsVec; NewMat: Boolean; VFirst: Boolean;
      Var ErrCode: Word): TwsGeneral;
    // Concatena??o de vetores nas colunas
    function ColVecConcat(v: TwsVec; VFirst:boolean; Var Erro:Word): TwsGeneral;
    // Forma escalonada da matriz
    function Echelon(NewMat: Boolean; CMax: Integer=-1): TwsGeneral;
    // Ordena??o de todos os elementos da matriz
    function QSort(Asc: Boolean=True): TwsGeneral;
    // Grava??o de matrizes
    Class Function ObjectFromStream(Reader: TReader; Var Erro: Integer): TwsMatrix;
    // Recupera??o de matrizes
    Class Function LoadFromFile(Const FileName: String): TwsMatrix;
    // Grava matriz num arquivo de nome especificado
    Procedure SaveToFile(Const FileName: String);
    // Remove o pr?prio objeto notificando o sistema antes
    procedure NotificationAndFree;
    // Envia uma mensagem de notifica??o de altera??es para o sistema
    procedure NotifyChange(Status: TwsStatusChangeSet);
    // Bloqueia o objeto para altera??o
    procedure Lock;
    // Desbloqueia o objeto para altera??o
    procedure Unlock;

    // Nome da matriz
    property Name: String read FName write SetName;
    // N?mero de linhas
    property nRows: Integer read FNRows write SetNRows;
    // N?mero de colunas
    property nCols: Integer read FNCols write FNCols;
    // Elemento como um string
    Property AsString[Index: Integer; Index: Integer]: String Read GetStr Write PutStr;
    // Tipo da matriz
    property MatType: TwsEnumMatType read FMatType write FMatType;
    // Lista com os nomes das colunas
    property CName: TStrings read FCName write FCName;
    // R?tulo
    Property MLab: String Read GetMLab Write PutMLab;

    // Retorna a identifica??o do grupo se existir
    // A identifica??o do grupo ser? extra?da do R?tulo da Matriz
    // Ex: MLab = "Grupo: X.Y" retornar? o grupo X.Y
    property Group : String read getGroup;

    // Matriz foi modificada?
    Property Modified: Boolean Read FModified;
    // Endere?o da linha especificada
    Property Row[index: Integer]: TwsVec Read GetRow Write SetRow;
    // Nome da linha especificada
    property RowName[Index: Integer]: String read GetRowName write SetRowName;
    // Nome da coluna especificada
    property ColName[Index: Integer]: String read GetColName write SetColName;
    // Elemento da matriz
    property Data[Index: Integer; Index: Integer]: Double read Get write Put; default;
    // Retorna se o objeto est? bloqueado ou n?o
    property Locked: Boolean read getLocked;
    // Para futuro armazenamento de informa??es
    property ExtraData[Index: integer]: byte read getExtraData write setExtraData;

    // Evento que acontece quando o status da matriz muda
    property OnStatusChange : TwsStatusChange_Event read FStatusChange write FStatusChange;

    // Evento que acontece quando um elemento est? para ser definido como XML
    property OnSelectElementColor : TwsSelectElementColor_Event read  FSelectElementColor
                                                                write FSelectElementColor;
  end; { TwsMatrix }

  { Heran?a
      TwsGeneral --> TwsMatrix --> TList
    Objetivo
      Implementar uma classe para matrizes gerais. Uma matriz geral ? uma matriz retangular
      que armazena todos os m x n elementos, onde m ? o n?mero de linhas e n o n?mero de
      colunas. Al?m do armazenamento diferenciado, implementa muitos m?todos t?picos para
      matrizes desse tipo. Como a diferencia??o b?sica dos demais descendentes est? na
      quantidade de elementos armazenados, qualquer tipo descendente poder? ser colocado
      neste formato.
  }
  TwsGeneral = class(TwsMatrix)
  Private
    // Grava??o em disco de parte de uma matriz geral
    procedure InternalSave3(Writer: TWriter); Override;
    // Recupera??o de disco de paret de uma matriz geral
    procedure InternalLoad3(Reader: TReader); Override;
    // M?todo auxiliar para obten??o da autoestrutura de uma matriz
    procedure Tred2(d, e: TwsVec; EgV: Boolean);
    // M?todo auxiliar para obten??o da autoestrutura de uma matriz
    procedure TQLI(d, e: TwsVec; EgV: Boolean; var ErrCode: Word);
    // Recupera??o de um elemento de uma matriz geral
    function  Get(i,j: Integer): Double; override;
    // Escrita de um elemento de uma matriz geral
    procedure Put(i,j: Integer; x: Double); override;
  Protected
    function GetBlockName: String; override;
    function GetBlockNameComment: String; override;
  Public
    // Cria um objeto para armazenar uma matriz geral
    constructor Create(NR, NC: Integer);
    // Escrita na forma de texto
    procedure List(Buffer: TStrings); override;
    // Elimina colunas de uma matriz geral
    procedure DeleteCols(Col: TwsLIVec); override;
    // Produto por escalar de uma matriz geral
    function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst, NewMat: Boolean): TwsMatrix; override;
    // Autoestrutura de uma matriz geral
    procedure Eigen(var B:TwsDiagonal; EgV:Boolean; var r:Integer; Sort:Boolean;
      eps: Double; var ErrCode: Word);
    // C?pia de uma matriz geral para um formato qualquer
    procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
    // Ordena as linhas segundo chaves especificadas
    procedure SortRows(Column, Ascend: TwsLIVec);
    // Rearranja as linhas pelos indices especificados
    procedure SortRowAs(Indx: TwsLIVec; var ErrCode: Word);
    // Troca o conteudo das colunas i e j
    procedure ExchangeCols(j,k: Integer);
    // Operacoes elementares
    procedure ElemOperI(i,j: Integer; ToRows: Boolean=True);
    procedure ElemOperII(i: Integer; const k: Double; ToRows: Boolean=True);
    procedure ElemOperIII(i,j: Integer; const k: Double; ToRows: Boolean=True);

    // Matriz de Hessenberg
    procedure Hessenb;
    // Balanceamento para obten??o de autovalores de matrizes quaisquer
    procedure Balanc;
    // Fatora??o LU
    procedure LU(var Indx: TwsLIVec; var d: Double; Var ErrCode:Word);
    // Determinante a partir de um fator LU
    function  LUDet: Double;
    // Constr?i a transforma??o de Givens
    procedure MakeGiv(v1, v2: Double; var c, s, r: Double);
    // Aplica a transforma??o de Givens
    procedure AppGiv(c, s: Double; var z1, z2: Double);
    // Inversa de uma matriz geral
    procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); Override;
    // Resolve sistema de equa??es lineares onde a matriz dos coeficientes ? geral
    procedure Solve(B: TwsMatrix;var ErrCode: Word); Override;
    // Matriz de covari?ncia a partir da fatora??o por Householder
    procedure HFTCov(p: Integer);
    // Matriz de covari?ncia a partir da fatora??o por Householder com troca de colunas
    procedure HFTICov(P: TwsLIVec);
    // Normaliza coluna
    procedure NormCol(j: Integer);
    // Constroi base ortonormal metodo GS
    function GSBase: TwsGeneral;
    // Constroi base ortonormal metodo MGS
    function MGSBase(NewMat: Boolean=False): TwsGeneral;
    // Projetor ortogonal baseado na matriz
    function HProjX: TwsSymmetric;
    // Projetor no complemento ortogonal baseado na matriz
    function QProjX: TwsSymmetric;
    // procedimento que aplica condicao de Pearce as linhas e colunas
    procedure Pearce(var C,R: TwsGeneral; var CBal,RBal,Orth: boolean);

    // Sweep didatico
    procedure G2Sweep(k: Integer; DMin: Double=1E-8); override;

    // Fatora??o por Gram-Schmidt modificado
    procedure MGS(var T: TwsTriangular; var ErrCode: Word);
    // Fatora??o por Gram-Schmidt modificado numa m?trica distinta da identidade
    procedure WMGS(D: TwsDiagonal; var T: TwsTriangular; var ErrCode: Word);
    // Decomposi??o por valores singulares
    Procedure SVDCmp(Var D:TwsDiagonal; var V:TwsGeneral; var Rank:Integer; Var ErrCode:Word;
      eps: Double = 1.0e-8);
    // Inversa de Moore Penrose
    function MoorePenrose: TwsGeneral;
    // Localiza linha de um elemento para coluna especificada
    function LocateAtCol(x:Double; Col:Integer; Out j:Integer; Asc:Boolean=True):Boolean;
    // Localiza linha de um elemento para linha especificada
    function LocateAtRow(x:Double; Lin:Integer; Out j:Integer; Asc:Boolean=True):Boolean;
    {$ifdef WINSTAT_FULL}
    // Matriz de correla??es
    function Correlations(C: TwsLIVec; var Stat:TwsGeneral;BStat: Boolean;
      var CTest: TwsGeneral): TwsSymmetric;
    // Matriz de correla??es incluindo vari?vel peso
    function WCorrelations(C: TwsLIVec; var Stat:TwsGeneral; BStat: Boolean;
      WInd: Integer; var CTest: TwsGeneral): TwsSymmetric;
    // Matriz de correla??es parciais
    function PartialCorrel(C,CAdj: TwsLIVec; var Stat: TwsGeneral; BStat:Boolean;
      var CTest: TwsGeneral): TwsSymmetric;
    // Matriz de correla??es parciais com pesos
    function WPartialCorrel(C, CAdj: TwsLIVec; var Stat: TwsGeneral; BStat: Boolean;
      WInd: Integer; var CTest: TwsGeneral): TwsSymmetric;
    {$endif}
    // Matriz de produtos cruzados ajustados para a m?dia
    function AdjCrossProd(Col: TwsLIVec; var Mean: TwsVec; var ValObs: Integer):TwsSymmetric;
    // Matriz de produtos (ponderados) cruzados ajustados para a m?dia
    function WAdjCrossProd(Col:TwsLIVec; out Mean: TwsVec; out ValObs: Integer;
      out wa: Double; WIndex: Integer):TwsSymmetric;
    // Matriz de Hessenberg
    function HesQR(MaxIts: Word; var ErrCode: Word): TwsGeneral;
    // Estat?sticas de ordem
    Procedure MatOrder(Col: TStringList);
    // Obtem valores da tabela de Tukey
    Function GetTableValue(nTreat, DF: Double): Double;
    // Matriz de m?dias com chaves ordenadas
    function SortKeyMeans(KeyCol, Col: TwsLIVec; var XRow: Integer): TwsVec;
    // Matriz de m?dias e somas de quadrados com chaves ordenadas
    function KeyGroupMeans(KeyCol, Col:TwsLIVec; GMean: TwsVec; SSQ: TObject; var XRow,
      M: Integer; Covar,Univ: Boolean): TwsVec;
    // Matriz de m?dias com chaves quaisquer
    function KeyMeans(KeyCol, Col: TwsLIVec): TwsVec;
    // Todos os valores de uma coluna s?o iguais?
    function ColConst(k: Integer): Boolean;
    // Inverte uma matriz atrav?s da fatora??o LU
    function LUInv: TwsGeneral;
    // Resolu??o de sistemas de equa??es triangulares
    procedure BackSubst(Indx: TwsLIVec; B: TwsVec);
    // Inverte uma matriz j? fatorada na forma LU
    function LUToInv(Indx: TwsLIVec): TwsGeneral;
    // Decomposi??o de householder
    procedure HouseHolder(p: Integer; out g, h: TwsDFVec; var ErrCode: Word);
    // Fatora??o de Householder Lawson & Hanson
    procedure HouseCol(OnlyApply: Boolean; p,l,Col: Integer; h: TwsVec; cStart,nc: Integer);
    procedure HouseForward(xCol: Integer);
  end; { TwsGeneral }

{ Heran?a
    TwsSymmetric --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes sim?trica. Uma matriz sim?trica se caracteriza por
    ter elementos iguais em posi??es de ?ndices trocados, ou seja, os elementos da tringular
    superior s?o sempre iguais aos da mtTriangular inferior. Assim, nesta implementa??o, somente
    os elementos da mtTriangular inferior s?o armazenados. O m?todo de acesso aos elementos
    est? preparado para retornar o valor correto, mesmo que ele n?o esteja eplicitamente
    armazenado.
}
TwsSymmetric = class(TwsMatrix)
Private
  // Escreve um elemento numa martiz sim?trica
  procedure Put(i,j: Integer; x: Double); override;
  // Obt?m um valor de uma matriz sim?trica
  function  Get(i,j: Integer): Double; override;
  // Inverte uma matriz sim?trica pelo processo de Cholesky
  procedure CholeskyInv(eps: Double=1.0e-8);
  // Escreve parte de uma matriz sim?trica no disco
  procedure InternalSave3(Writer: TWriter); override;
  // Recupera parte de uma matriz sim?trica no disco
  procedure InternalLoad3(Reader: TReader); Override;

  function GetBlockName: String; override;

  function GetBlockNameComment: String; override;

Public
  // Cria um objeto para armazenar uma matriz sim?trica
  constructor Create(NC: Integer);
  // Lista matriz matriz sim?trica na forma de texto
  procedure List(Buffer: TStrings); override;
  // Produto de um escalar por uma matriz sim?trica
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst, NewMat:Boolean): TwsMatrix; override;
  // Matriz transposta
  function Transpose: TwsMatrix; override;
  //A'BA
  function TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto ABA' onde B ? sim?trica
  function TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // C?pia de uma matriz sim?trica para um tipo qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Fatora??o de Cholesky
  function CholeskyFat(var r: Integer; NewMat: Boolean; eps: Double = 1.0e-8): TwsMatrix;
  // Aplica sweep a um conjunto de colunas
  procedure SweepApp(C: TwsLIVec);
  // Aplica sweep a uma sequ?ncia de colunas
  procedure SeqSweep(k1, k2: Integer; v: TwsLIVec);
  // Toler?ncias para aplica??o do sweep
  procedure Tolerance(F: TwsVec; eps: Double=1.0e-9);
  // Aplica sweep a um conjunto de colunas
  procedure Sweep(Col,v: TwsLIVec);
  // Sweep revers?vel
  procedure RevSweep(k: Integer; DMin: Double; v: TwsLIVec);
  // Forma de hermite
  function Hermite: TwsGeneral;
  // Transforma uma matriz ja operada pelo sweep numa forma de hermite
  function SweepToHermite(v: TwsLIVec): TwsGeneral;
  // Inversa generalizada G2
  function G2Inv: TwsGeneral;
  // Obtem uma inversa generalizada G2 a partir de uma matriz j? operada pelo sweep
  function SweepToG2(v: TwsVec): TwsGeneral;
  // Obt?m uma matriz de correla??es
  procedure CorrMat;
  // Inverte uma matriz sim?trica
  procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); Override;
  // Resolve um sistema de equa??es sim?trico
  procedure Solve(B: TwsMatrix; var ErrCode: Word); Override;
end; { TwsSymmetric }

{ Heran?a
    TwsDiagonal --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes diagonais. Uma matriz mtDiagonal se caracteriza por
    ter todos os elementos fora da mtDiagonal nulos. Assim, nesta implementa??o, somente
    os elementos da mtDiagonal s?o armazenados. O m?todo de acesso aos elementos est? preparado
    para retornar o valor correto, mesmo que ele n?o esteja eplicitamente armazenado.
}
TwsDiagonal = class(TwsMatrix)
Private
  // Escreve um elemento numa matriz mtDiagonal
  procedure Put(i,j: Integer; x: Double); override;
  // Recupera um elemento de uma martiz mtDiagonal
  function  Get(i,j: Integer): Double; override;
  // Elemento em formato string
  function GetStr(i,j: Integer): String; override;
  // Grava parte de uma matriz mtDiagonal em disco
  procedure InternalSave3(Writer: TWriter); override;
  // Recupera parte de uma matriz mtDiagonal do disco
  procedure InternalLoad3(Reader: TReader); Override;

  function GetBlockName: String; override;
  function GetBlockNameComment: String; override;
  procedure RowsToXML(Writer: TXML_Writer); override;
Public
  // Cria um objeto para armazenamento de uma matriz mtDiagonal
  constructor Create(NC: Integer);
  // Copia matriz mtDiagonal num vetor
  function ToVec: TwsVec; override;
  // Copia a mtDiagonal para um vetor
  function DiagToVec: TwsVec; override;
  // Opera??o de uma matriz mtDiagonal por elemento com uma matriz qualquer
  function ByElement(B:TwsMatrix; Op:TwsEnumTypeOp; NewMat:Boolean;
    var ErrCode:Word):TwsMatrix; override;
  // Produto AB onde A ? mtDiagonal e B ? uma matriz qualquer
  function Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto Av onde A ? mtDiagonal e v um vetor
  function VecMult(v:TwsVec; PreMult:Boolean; Var ErrCode:Word): TwsDFVec; override;
  // Opera??o de uma matriz mtDiagonal por escalar
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean): TwsMatrix; override;
  // Transposta de uma matriz dagonal
  function Transpose: TwsMatrix; override;
  // C?pia de uma matriz mtDiagonal para uma matriz qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Produto AB' onde A ? mtDiagonal e B qualquer
  function TranspMul1(B:TwsMatrix; var ErrCode:Word): TwsMatrix; override;
  // Produto A'B onde A ? mtDiagonal e B qualquer
  function TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto AA' onde A ? mtDiagonal
  function TranspMul3: TwsMatrix; override;
  // Produto A'A onde A ? mtDiagonal
  function TranspMul4: TwsMatrix; override;
  // Produto A'DA onde D ? mtDiagonal
  function TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto v?Av onde A ? mtDiagonal e v ? vetor
  function TranspMul6(v: TwsVec; var ErrCode: Word): Double; override;
  // Produto ABA' onde B ? mtDiagonal
  function TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Toma coluna B[k] e faz o produto B[k]'*A*B[k] sendo A mtDiagonal e B qualquer
  function TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double; override;
  // Sa?da em modo texto de matriz mtDiagonal
  procedure List(Buffer: TStrings); override;
  // Impress?o em modo texto de matriz mtDiagonal
  procedure Print(Buffer: TStrings); override;
  // Impress?o de matriz mtDiagonal com condi??o estabelecida para colunas
  procedure ColPrint(const Cols: String; Expr: String; Buffer: TStrings); override;
  // Inverte uma matriz mtDiagonal
  procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); Override;
  // Solucao de sistemas de equacoes lineares onde a matriz dos coeficientes ? mtDiagonal
  procedure Solve(B: TwsMatrix; var ErrCode: Word); Override;
  // Elimina colunas especificadas
  procedure DeleteCols(Col: TwsLIVec); override;
  // Mostra matriz numa planilha
  procedure ShowInSheet(Sheet: TBaseSpreadSheetBook); override;

end; { TwsDiagonal }

{ =========================== TwsVandermonde ===============================}

{ Heran?a
    TwsVandermonde --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes de mtVandermonde. Uma matriz de mtVandermonde se
    caracteriza por possuir colunas obtidas como pot?ncias de elementos de uma coluna b?sica.
    Assim os elemntos da primeira coluna correspondem aos valores desse vetor na pot?ncia 0;
    os da segunda na pot?ncia 1 (o pr?prio vetor), e assim por diante. Assim, nesta implementa??o,
    somente os elementos desse vetor s?o armazenados. O m?todo de acesso aos elementos
    est? preparado para retornar o valor correto, mesmo que ele n?o esteja eplicitamente
    armazenado.
}
TwsVandermonde = class(TwsMatrix)
Private
  // Escreve um elemento numa matriz de mtVandermonde
  procedure Put(i,j: Integer; x: Double); override;
  // Recupera um elemento de uma matriz de mtVandermonde
  function  Get(i,j: Integer): Double; override;
  // Atribui identifica??o para linha numa matriz de mtVandermonde
  procedure SetRName(st: TwsCVec;DelChar :TCharSet); override;
  // Obt?m identifica??o para linha numa matriz de mtVandermonde
  function GetRowName(i: Integer): String; override;
  // Grava parte de uma matriz de mtVandermonde em disco
  procedure InternalSave3(Writer: TWriter); override;
  // Pecupera parte de uma matriz de mtVandermonde do disco
  procedure InternalLoad3(Reader: TReader); Override;
  function GetBlockName: String; override;
  function GetBlockNameComment: String; override;
  procedure RowsToXML(Writer: TXML_Writer); override;
Public
  // Somente um vetor ? armazenado neste tipo de matriz
  RName: TStrings;
  // Cria um objeto para armazenamento de uma matriz de mtVandermonde
  constructor Create(NC: Integer);
  // Libera espa?o ocupado por uma matriz de mtVandermonde
  destructor  Destroy; override;
  // Lista  uma matriz de mtVandermonde no modo texto
  procedure   List(Buffer: TStrings); override;
  // TRansforma uma linha de uma matriz de mtVandermonde num string
  function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; override;
  // Impress?o de uma matriz de mtVandermonde com condi??o imposta
  procedure ColPrint(const Cols: String; Expr: string; Buffer: TStrings); override;
  // Opera??o de uma matriz de mtVandermonde por elemento
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix; override;
  // C?pia de uma matriz de mtVandermonde para uma matriz de tipo qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Resolve sistema de equa??es lineaers onde a matriz dos coeficientes ? de mtVandermonde
  procedure Solve(B: TwsMatrix; var ErrCode: Word); override;
end; { TwsVandermonde }

{ Heran?a
    TwsTriangular --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes triangulares inferiores. Uma matriz mtTriangular
    inferior se caracteriza por ter todos os elementos acima da mtDiagonal nulos. Assim, nesta
    implementa??o, somente os elementos da mtDiagonal e abaixo dela s?o armazenados. O m?todo
    de acesso aos elementos est? preparado para retornar o valor correto, mesmo que ele n?o
    esteja eplicitamente armazenado.
}
TwsTriangular = class(TwsMatrix)
Private
  // Escreve um elemento numa matriz mtTriangular
  procedure Put(i,j: Integer; x: Double); override;
  // Recupera um elemento de uma matriz mtTriangular
  function  Get(i,j: Integer): Double; override;
  // Grava parte de uma matriz mtTriangular em disco
  procedure InternalSave3(Writer: TWriter); override;
  // Recupera parte de uma matriz mtTriangular do disco
  procedure InternalLoad3(Reader: TReader); Override;
  function GetBlockName: String; override;
  function GetBlockNameComment: String; override;
Public
  // Cria um objeto para armazenamento de uma matriz mtTriangular
  constructor Create(NC: Integer);
  // Lista conte?do de  uma matriz mtTriangular no formato texto
  procedure   List(Buffer: TStrings); override;
  // Produto AB onde A ? uma matriz mtTriangular e B ? qualquer
  function Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto AB' onde A ? uma matriz mtTriangular e B ? qualquer
  function TranspMul1(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto A'B onde A ? uma matriz mtTriangular e B ? qualquer
  function TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Prodtuto AA' onde A ? uma matriz mtTriangular
  function TranspMul3: TwsMatrix; override;
  // Produto A'A onde A ? uma matriz mtTriangular
  function TranspMul4: TwsMatrix; override;
  // Produto A'BA onde B ? uma matriz mtTriangular e A ? qualquer
  function TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto v'Av onde A ? uma matriz mtTriangular e v ? um vetor
  function TranspMul6(v: TwsVec; var ErrCode: Word): Double; override;
  // Produto ABA' onde B ? uma matriz mtTriangular e A ? qualquer
  function TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Toma coluna B[k] e faz o produto B[i]'*A*B[k] onde A ? uma matriz mtTriangular
  function TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double; override;
  // Produto Av onde A ? uma matriz mtTriangular e v ? um vetor
  function VecMult(v:TwsVec; PreMult:Boolean; Var ErrCode:Word): TwsDFVec; override;
  // Opera??o de A com escalar onde A ? uma matriz mtTriangular
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix; override;
  // C?pia de uma matriz mtTriangular para um formato qualquer
  procedure Copy(MT:TwsEnumMatType; var Matrix:TwsMatrix); override;
  // Inversa de uma matriz mtTriangular
  procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); override;
  procedure FwdSolve(B: TwsGeneral; var ErrCode: Word);
//  function BwdSolve(B: TwsGeneral; var ErrCode: Word): TwsGeneral;

end; { TwsTriangular }

{ ============================== TwsToeplitz ============================ }

{ Heran?a
    TwsToeplitz --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes de mtToeplitz. Existem v?rios tipos de matrizes de mtToeplitz.
    O formato implementada atrav?s desta classe utiliza um vetor de 2*n-1, onde n ? o n?mero
    de linhas e colunas, para armazenar todos os elementos necess?rios. O m?todo de acesso aos
    elementos est? preparado para retornar o valor correto, mesmo que ele n?o esteja eplicitamente
    armazenado. O elemento da posi??o (i, j) est? na posi??o n-j+i do vetor.
}
TwsToeplitz = class(TwsMatrix)
Private
  // Escreve elemento na posi??o correta
  procedure Put(i, j: Integer; x: Double); override;
  // Obt?m elemento para este tipo de matriz
  function  Get(i, j: Integer): Double; override;
  // Acesso a um identificador de linha
  function GetRowName(i: Integer): String; override;
  // Atribui identificador para uma linha
  procedure SetRName(st: TwsCVec;DelChar :TCharSet); override;
  // Escrita em disco
  procedure InternalSave3(Writer: TWriter); override;
  // Leitura em disco
  procedure InternalLoad3(Reader: TReader); Override;
  function GetBlockName: String; override;
  function GetBlockNameComment: String; override;
  procedure RowsToXML(Writer: TXML_Writer); override;
Public
  // Lista de nomes para linhas
  RName: TStrings;
  // Cria objeto para armazenar matriz de mtToeplitz
  constructor Create(NC: Integer);
  // Libera espa?o ocupado por uma matriz de Toepolitz
  destructor Destroy; override;
  // Sa?da no modo texto
  procedure List(Buffer: TStrings); override;
  // Produto por escalar numa matriz de mtToeplitz
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix; override;
  // Copia numa matriz de mtToeplitz em um formato qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Coloca uma linha em formato string para sa?da
  function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; override;
  // Imprime uma matriz com imposi??o de erstri??o para colunas
  procedure ColPrint(const Cols: String; Expr: String; Buffer: TStrings); override;
  // Resolve um sistema linear, onde a matriz dos coeficientes ? de mtToeplitz
  procedure Solve(B: TwsMatrix; var ErrCode: Word); override;
end; { TwsToeplitz }

{ TwsEnumContrType - Tipo de codifoca??o para matriz do modelo
    ctHelm   - Contrastes de Helemrt
    ctPolOrt - Polin?mios ortogonais
    ctUser   - Contrastes estabelecidos pelo usu?rio
    ctDev    - Contrastes do tipo diferen?as em rela??o a um n?vel especificado
    ctExper  - Codifica??o anulando um n?vel especificado
    ctBin    - Codifica??o bin?ria }
  TwsEnumContrType = (ctHelm, ctPolOrt, ctUser, ctDev, ctExper, ctBin);

{ TwsEnumDataType - Tipo vari?veis ou de colunas no conjunto de dados
    Numeric   - vari?vel num?rica
    Quant     - fator quantitativo
    Qualit    - fator qualitativo
    QualitOrd - fator qualitativo ordenado
    Factors   - fator }
  TwsEnumDataType  = (dtNumeric, dtQuant, dtQualit, dtQualitOrd, dtFactors);

{  TFType - tipo de n?vel
     tfFixed - n?veis fixos
     tfRandom - n?veis aleat?rios }
  TwsEnumFType = (tfFixed, tfRandom);

  // Forward declaratioms
  TwsDSStructure = Class;
  TListCols      = Class;

{ ============================ TwsDataSetCol ================================}

  {  Heran?a
       TwsDataSetCol --> TObject
     Objetivos
       TwsDataSetCol ? a classe b?sica para defini??o dos tipos de coluna de um conjunto de
       dados
   }

  TwsDataSetCol = Class(TObject)
  Private
    FName       : String;             // Nome da coluna
    FLabel      : PString;            // R?tulo
    FColType    : TwsEnumDataType;    // Tipo de coluna
    FSize       : Byte;               // Largura de impress?o
    FVersion    : String [10];        // Vers?o do arquivo
    FHeaderFont : TFont;
    FDataFont   : TFont;
    FObject     : TObject;
    FDataSet    : TwsDataSet;
    Function  RLabel: String;
    Procedure WLabel(Const ALabel: String);
    Procedure SetName(Const Value: String);
    // Descreve os dados como XML
    procedure ToXML(Buffer: TStrings; Ident: Integer); virtual;
  Public
    // Organiza a estrutura das colunas
    Struct     : TwsDSStructure;
    // Cria um objeto para armazenar um conjunto de dados
    Constructor Create(Const aname, alab: String; fs: byte);
    // Libera espa?o ocupado por um conjunto de dados
    Destructor  Destroy; Override;
    // Armazenamento em disco
    Procedure  SaveToStream(Writer:TWriter);    Virtual;
    // Leitura de disco
    Procedure  LoadFromStream(Reader:TReader);  Virtual;
    // Cria um r?tulo, dependendo do tipo de coluna
    Function   getColTypeAsString: String;
    // Nome da coluna
    Property Name      : String    Read FName       Write SetName;
    // R?tulo da coluna
    Property Lab       : String    Read RLabel      Write WLabel;
    // Tipo de coluna
    Property ColType   : TwsEnumDataType Read FColType    Write FColType;
    // Tamanho de impress?o
    Property Size      : Byte      Read FSize       Write FSize;

    // O objeto conectado aqui N?O ? destru?do automaticamente
    Property aObject   : TObject   read FObject     write FObject;

    // Indica qual o possuidor desta vari?vel
    Property DataSet : TwsDataSet read FDataSet write FDataSet;

    Property HeaderFont : TFont  read FHeaderFont;
    Property DataFont   : TFont  read FDataFont;
  End; // TwsDataSetCol

{ ============================== TwsNumeric ============================== }

  {  Heran?a
       TwsNumeric --> TwsDataSetCol --> TObject
     Objetivos
       TwsNumeric ? a classe que define uma vari?vel num?rica num conjunto de dados. Basicamente
       incorpora a precis?o (d?gitos significativos) de impress?o dos valores num?ricos.
   }

  TwsNumeric = Class(TwsDataSetCol)
  Private
    // d?gitos significativos
    FPrecision:  Byte;
    // Atribui precis?o
    Procedure SetPrecision(APrecision: Byte);
    // Leitura/grava??o do objeto
    Procedure SaveToStream(Writer: TWriter);     Override;
    Procedure LoadFromStream(Reader: TReader);   Override;
  Public
    // Cria objeto que armazena estrutura de uma coluna num?rica
    Constructor Create(Const aName,aLab: String; fs: byte=15; fp: byte=8);
    // d?gitos significativos
    Property  Precision: Byte Read FPrecision Write SetPrecision;
  End; { TwsNumeric }

{ ================================ TwsFactor ================================ }

  {  Heran?a
       TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsFactor ? a classe b?sica para defini??o de uma vari?vel do tipo fator num conjunto de
       dados. Incorpora v?rias caracter?sticas importantes de uma vari?vel desse tipo: os n?veis
       e os tipos de n?veis. N?o pode ser utilizada diretamente
   }
  TwsFactor = Class(TwsDataSetCol)
  Private
    // Lista com os n?veis
    FLevels    : TStrings;
    // Tipo de contraste
    FContrType : TwsEnumContrType;
    // Tipo de n?vel
    FLevelType : TwsEnumFType;
    FContr: TwsGeneral;
    // recupera n?mero de n?veis
    Function   GetLevels: Integer;
    // Elimina lista de n?veis
    Function   KillLevels: Boolean; virtual;
    // Armazenamento em disco
    Procedure  SaveToStream(Writer: TWriter);    Override;
    // Leitura do disco
    Procedure  LoadFromStream(Reader: TReader);  Override;
    // Descreve os dados como XML
    procedure ToXML(Buffer: TStrings; Ident: Integer); override;
    procedure SetContr(const Value: TwsGeneral);
    procedure setContrTypeAsString(const Value: String);
    function  getContrTypeAsString: String;
  Public
    // Participa da cria??o de um objeto do tipo fator
    Constructor Create(Const AName,ALab: String; fs: Byte=15);
    // Libera espa?o ocupado por um fator
    Destructor Destroy; Override;
    // Insere um n?vel na lista de n?veis
    Function   AddLevel(Level: String): Integer; Virtual;
    // Retorna o ?ndice do n?vel especificado
    Function   LevelToIndex(Const N: String): Integer;
    // Remove os n?veis que n?o aparecem como valores na vari?vel
    Procedure  RemoveInvalidLevels;
    // Remove o N?vel de ?ndice "Index"
    Procedure  RemoveLevel(index: Integer); virtual;
    // Remove matriz de contrastes do usuuario
    procedure ClearContr;
    // Tipo de n?vel na forma de strinng
    Function   getLevelTypeAsString: String;
    // N?veis eliminados?
    Property   LevelsClear: Boolean    Read KillLevels;
    // Lista com os nomes dos n?veis
    Property   LevelNames : TStrings   Read FLevels Write FLevels;
    // Tipo de contraste
    Property   ContrType  : TwsEnumContrType Read FContrType Write FContrType;
    // Tipo de contraste como String
    Property   ContrTypeAsString : String Read getContrTypeAsString Write setContrTypeAsString;
    // Tipo de n?veis
    Property   LevelType  : TwsEnumFType  Read FLevelType Write FLevelType;
    // N?mero de n?veis
    Property   Levels     : Integer   Read GetLevels;
    // Matriz para codifica??o dos n?veis na an?lise de modelos lineares
    Property   Contr      : TwsGeneral Read FContr Write SetContr;
  End; { TwsFactor }

{ =============================== TwsQualitative ============================ }

  {  Heran?a
       TwsQualitative --> TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsQualitative ? a classe que define uma vari?vel do tipo fator qualitativo num conjunto
       de dados.
   }

  TwsQualitative = Class(TwsFactor)
  Private
    // Armazena obeto no disco
    Procedure SaveToStream(Writer:TWriter);    Override;
    // Recupera objeto do disco
    Procedure LoadFromStream(Reader:TReader);  Override;
  Public
    // Cria uum objeto do tipo fator qualitativo
    Constructor Create(Const AName, ALab: String; fs: Byte=15);
  End; { TwsQualitative }

{ ============================== TwsOrdered ============================== }

  {  Heran?a
       TwsOrdered --> TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsOrdered ? a classe que define uma vari?vel do tipo fator qualitativo ordenado num
       conjunto de dados.
   }

  TwsOrdered = Class(TwsFactor)
  Private
    // Armazena objeto no disco
    Procedure SaveToStream(Writer:TWriter);    Override;
    // Recupera objeto do disco
    Procedure LoadFromStream(Reader:TReader);  Override;
  Public
    // Cria um objeto do tipo fator qualitativo ordenado
    Constructor Create(Const AName, ALab: String; fs: Byte=15);
  End; { TwsOrdered }

{ ============================= TwsQuantitative ============================= }

  {  Heran?a
       TwsQuantitative --> TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsQuantitative ? a classe que define uma vari?vel do tipo fator quantitativo num conjunto
       de dados. Acrescenta um vetor com os nomes dos n?veis
   }

  TwsQuantitative = Class(TwsFactor)
  Private
    // Valores dos Niveis
    FLevel: TwsDFVec;
    // Descreve os dados como XML
    procedure ToXML(Buffer: TStrings; Ident: Integer); override;
  Public
    // Cria objeto do tipo fator quantitativo
    Constructor Create(Const AName,ALab: String; fs: Byte=15);
    // Libera mem?ria ocupada por fator quantitativo
    Destructor Destroy; Override;
    // Armazena objeto em disco
    Procedure SaveToStream(Writer: TWriter);    Override;
    // Recupera objeto do disco
    Procedure LoadFromStream(Reader: TReader);  Override;
    // Adiciona n?vel na lista
    Function   AddLevel(Level: String): Integer; Override;
    // Remove o N?vel de ?ndice "Index"
    Procedure  RemoveLevel(index: Integer); Override;
    // Atribui valor de um n?vel quantitativo
    Procedure SetLevelValue(Const Level: String; Const Value: Double);
    // Recupera um valor do n?vel quantitativo
    Function  GetLevelValue(Const Level: String): Double;
    // Elimina lista de n?veis
    Function   KillLevels: Boolean; override;
    // Vetor com os valores dos n?veis
    Property LevelValues: TwsDFVec Read FLevel Write FLevel;
  End; { TwsQuantitative }

{ ---------------------------- TwsListCols ---------------------------------}

{ Objetivo
    Redefine classe TStringList para manter objetos que definem as estruturas das colunas
}
  TListCols = Class(TStringList)
    // Redefine adi??o de novos objetos
    function AddObject(const S: string; AObject: TObject): Integer; Override;
  End;

{ ---------------------------- TwsDSStructured ------------------------------

  {  Heran?a
       TwsDSStructure --> TObject
     Objetivos
       Gerencia a estrutura de um conjunto de dados. Diferentemente de uma matriz geral, um
       conjunto de dados pode modificar os atributos das colunas. Estrutura de um conjunto de
       dados s?o as informa??es relativas ?s colunas do conjunto: seu tipo, nome, etc.
       Facilita os processos de cria??o, inclus?o e exclus?o de colunas
   }
  TwsDSStructure = Class(TObject)
  Private
    // N?mero de fatores
    Function GetFactors: Integer;
    // Descreve os dados como XML
    procedure ToXML(Buffer: TStrings; Ident: Integer);

    procedure SetCol(index: Integer; const Value: TwsDataSetCol);
    Function  GetCol(index: Integer): TwsDataSetCol;
  Protected
    DataSet : TwsDataSet;
    {$ifdef MSXML}
    procedure fromXML(no: IXMLDomNode);
    {$endif}
  Public
    // Lista com as colunas
    Cols: TStrings;
    // Cria objeto descritor
    Constructor Create;
    // Libera espa?o ocupado pelo objeto
    Destructor Destroy; Override;
    // Copia a estrutura inteira
    function Copy: TwsDSStructure;
    // Adiciona uma c?pia de uma coluna j? existente (Vetores)
    Procedure AddCopyCol(Col: TwsDataSetCol); {Rochedo} {23/09/97}
    // Adiciona uma coluna adicionando valores (Vetores)
    Procedure  AddCol(Col: TwsDataSetCol); {Rochedo} {23/09/97}
    // Adiciona uma coluna sem adicionar os espacos para os valores (Vetores)
    Procedure  AddColEx(Col: TwsDataSetCol); {Rochedo} {04/04/98}
    // Adiciona uma vari?vel num?rica
    Procedure  AddNumeric      (Const N, Lab :String;ps: byte=15; pf: byte=8);
    // Adiciona um fator qualitativo
    Procedure  AddQualitative  (Const N, Lab :String; ps: byte=15);
    // Adiciona um fator quantitativo
    Procedure  AddQuantitative (Const N, Lab :String; ps: byte=15);
    // Adiciona um fator qualitativo ordenado
    Procedure  AddOrdered      (Const N, Lab :String; ps: byte=15);
    // Elimina a especifica??o dos artibutos de uma coluna
    Procedure  Delete          (const IndexCol: Integer); overload;
    // Elimina a especifica??o dos artibutos de uma coluna
    Procedure  Delete          (Const N: String); overload;
    // Retorna o endere?o de uma coluna pelo seu ome
    Function   ColByName       (Const N: String): TwsDataSetCol;
    // Retorna o ?ndice de uma coluna pelo seu ome
    Function   IndexOf         (Const N: String): Integer;
    // Retorna um vetor com os ?ndices de uma lista de colunas ou vari?veis
    Function   IndexCols       (Const Cols: String): TwsLIVec;
    // Coluna pelo ?ndice
    Property   Col[index : Integer]: TwsDataSetCol Read GetCol Write SetCol;
    // Quantas colunas do tipo fator
    Property   Factors: Integer Read GetFactors;
  End; { TwsDSStructure }


  TDS_ChooseFont_Event = procedure(Sender: TwsDataSet; R, C: Integer;
                                   Font: TFont; out UseThisFont: Boolean) of object;

  {  Heran?a
       TwsDataSet --> TwsGeneral --> TwsMatrix --> TList
     Objetivos
       Implmenta conjuntos de dados. Os conjuntos de dados diferem das matrizes por possuirem
       colunas de tipos diferentes. Assim, os atributos de cada coluna podem ser muito
       distintos
   }
  TwsDataSet = Class(TwsGeneral, IMatrix)
  Private
    // Nome da coluna identicadora
    FColIdentName: String;
    // Evento para defini??o da fonte correntemente usada
    FChooseFont: TDS_ChooseFont_Event;
    FStruct: TwsDSStructure;
    // Recupera parte do conte?do do disco
    Procedure InternalLoad3(Reader:TReader);       Override;
    // Armazena parte do conte?do em disco
    Procedure InternalSave3(Writer: TWriter);      Override;
    // Armazena defini??o das colunas no disco
    Procedure CNameSaveToStream(Writer:TWriter);   Override;
    // Recupera defini??o das colunas do disco
    Procedure CNameLoadFromStream(Reader:TReader); Override;
    // Atribui nome do conjunto
    procedure SetName(Const Name: String); Override;
    // Escreve valor em posi??o especificada
    Procedure PutStr(i,j: Integer; x: String); override;
    // Coloca conte?do de linha em srting
    function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; Override;
    // Cabe?alho
    Procedure Header(Start, Amt: Integer; Buffer: TStrings); Override;
    // Recupera elemento com string
    Function GetStr(i,j: Integer):String; override;
    // Recupera elemento como string
    Function GetInt(i,j: Integer): Integer;
    // Recupera um elemento como Data/Tempo
    Function GetDT(i, j: Integer): TDateTime;
    // Atribui nome de coluna
    Procedure SetColName(i: Integer; S: String); Override;
    // Recupera nome de coluna
    function  GetColName(i: Integer): string; Override; {Rochedo, 16/05/1998}
    // Obt?m endere?o do conjunto de dados
    Function  GetDataSet: TwsDataSet; Virtual;
    // Recupera elemento de qualquer tipo
    function  GetVar(R, C: Integer): Variant;
    // Escreve elemento de qualquer tipo
    procedure PutVar(R, C: Integer; const Value: Variant);  {Rochedo, 25/05/1998}
    // Obt?m elemento livre de espa?os em branco
    function  GetTrimStr(i,j: Integer): String;  {Rochedo, 11/03/1999}
    // Atribui nome de coluna identificadora
    procedure SetColIdentName(name:string);
    procedure SetStruct(const Value: TwsDSStructure); {Alex, 23/02/2000}
    // IMatrix interfaces link
    function m_RowCount(): integer;
    function m_ColCount(): integer;
    function m_IsMissValue(i, j: integer; out x: double): boolean;
    function m_getAsString(i, j: integer): string;
    function m_getAsInteger(i, j: integer): integer;
    function m_getAsFloat(i, j: integer): double;
    procedure m_setAsString(i, j: integer; value: string);
    procedure m_setAsInteger(i, j: integer; value: integer);
    procedure m_setAsFloat(i, j: integer; value: double);

     // IMatrix interface
    function IMatrix.RowCount      = m_RowCount;
    function IMatrix.ColCount      = m_ColCount;
    function IMatrix.IsMissValue   = m_IsMissValue;
    function IMatrix.getAsString   = m_getAsString;
    function IMatrix.getAsInteger  = m_getAsInteger;
    function IMatrix.getAsFloat    = m_getAsFloat;
    procedure IMatrix.setAsString  = m_setAsString;
    procedure IMatrix.setAsInteger = m_setAsInteger;
    procedure IMatrix.setAsFloat   = m_setAsFloat;
  protected
    // Retorna sim ou n?o se a descri??o das vari?veis deve ser impressa
    function PrintDescVars: Boolean; override;

    // Descreve as colunas como xml
    procedure StructColsToXML(Buffer: TStrings; var Ident: Integer); override;

    // Obtem os valores que ser?o colocados nas c?lulas da planilha
    procedure GetSheetValue(L, C: Integer; var IsNUM: Boolean;
                                           var sValue: String;
                                           var rValue: Real); virtual;
    {$ifdef MSXML}
    procedure fromXML_LoadStruct(no: IXMLDomNode); override;
    {$endif}
  Public
    // Cria um conjunto de dados
    Constructor Create(aname : String='NoName');
    // Cria um conjunto de dados onde todos os dados s?o num?ricos
    // Fill indica se queremos inicializar o conjunto com zeros
    Constructor CreateNumeric(aName: String; nRows, nCols: integer);
    // Cria conjunto de dados com uma estrutura ja pronta
    Constructor CreateStructured(aname : String; nr:Integer; Struct: TwsDSStructure);
    // Cria conjunto de dados a partir de um array de colunas ja disponivel
    Constructor CreateFix(aname : String; nr:Integer; ColT: Array Of TwsEnumDataType);
    // Libera ?rea ocupada por um conjunto de dados
    Destructor Destroy(); Override;
    // Retorna uma instancia lida de uma secao de um arquivo do tipo ini
    // Por enquanto s? l? colunas num?ricas
    class function Load(Ini: TMemIniFile; const Section: string): TwsDataSet;

    // Salva a instancia em uma secao de um arquivo do tipo ini
    // Por enquanto s? salva colunas num?ricas
    procedure Save(Ini: TMemIniFile; const Section: string);

    // Redimensiona a quantidade de linhas do DataSet
    procedure ForceRowsCount(Count: Integer);

    // Prepara e preenche a estrutura com um determinado valor.
    // Se a estrutura foi criada mas n?o existem dados, Fill cria os dados e inicializa-os.
    // Dever?, logicamente, ser chamado sempre ap?s a defini??o da estrutura dos dados.
    procedure Fill(const x: Double);
    // Procura uma data utilizando busca binaria.
    // Os dados da coluna precisam estar ordenados
    // Retorna Falso se o valor nao for encontrado
    // DateTime informa se devemos considerar a parte horaria da data
    function FindDate(const D: TDateTime; inCol: integer; DateTime: boolean; var Index: Integer): Boolean;

    // Obtem ?ndices de colunas a partir de nomes num string
    function  IndexColsFromString(const Cols: String): TwsLIVec; Override;
    // Obtem ?ndices de colunas a partir de nomes numa lista de strings
    Function  IndexColsFromStrings(Cols: TStrings): TwsLIVec; Override;
    // Colunas a imprimir
    Function  ColsToImp(var k: Integer): Integer; override;
    // String e nome de uma coluna?
    Function  Isfactor(Const S: String): Boolean;
    // Cria uma c?pia do conjunto de dados
    Function  Copy: TwsDataSet; Virtual;
    // Cria uma c?pia de colunas especificas do conjunto de dados
    Function  CopyByCols(Col: TwsLIVec): TwsDataSet; Virtual;
    // Imprime conjunto de dados
    Procedure Print(Buffer: TStrings); override;

    {$ifdef Planilha}
    //Mostra o conte?do de um DataSet em uma planilha
    procedure ShowInSheet(Sheet: TBaseSpreadSheetBook); override;

    //Escreve na Linha 1, Coluna <ColSheet> o nome da coluna de ?ndice <indexCol>
    procedure ColNameToSheet(indexCol: Integer; Sheet: TBaseSpreadSheetBook; ColSheet: Integer);

    //Escreva na Linha 1, a partir da coluna 2, os nomes das colunas
    procedure ColNamesToSheet(Sheet: TBaseSpreadSheetBook); overload;

    //Escreva na Linha 1, a partir da coluna 2, os nomes das colunas indexadas por <indexCols>
    procedure ColNamesToSheet(indexCols: TwsLIVec; Sheet: TBaseSpreadSheetBook); overload;
    {$endif}

    function LetterValues(xCol: TwsLIVec): TwsDataSet;
    function StemAndLeaf(xCol: TwsLIVec; Xtrems: Boolean=False): TStrings;
    function RSmoothMedian(xCol: Integer; yCol: TwsLIVec; xSort: boolean=True; S4253: boolean=True): TwsDataSet;
    {$ifdef WINSTAT_FULL}
    // faz imputacao de dados (hidrologicos) por regressao linear
    // Necessita de "wsProbabilidade"
    function LRImput(Col: TwsLIVec; nCond: Integer; rCond: Double; Sim: boolean;
      var RList: TwsDataSets; var lf: TList): TwsDataSet;
    {$endif}

    function DataStd(Col: TwsLIVec; StdType: byte=0; NewData: Boolean=False): TwsDataSet;

    function IndexMat(CIdx: TwsLIVec): TwsGeneral;

    // Obt?m m?dias e outras estat?sticas para n?veis de fatores especificados
    function MeanFactor(IFac:TwsLIVec; VIdx:Integer; var GMean,Min,Max:Double):TwsGeneral;
    // Obt?m m?dias para n?veis de fatores especificados para amplitude de linhas
    function FacMeans(Fac,IRow,LRow: Integer; Col: TwsLIVec): TwsGeneral;
    // Obt?m m?dias e vari?ncias ponderadas para n?veis de fatores especificados para amplitude de linhas
    function wFacVarMean(Fac: TwsLIVec; IRow,LRow,WInd: Integer;Col: TwsLIVec): TwsGeneral;
    // Obt?m m?dias e vari?ncias para n?veis de fatores especificados para amplitude de linhas
    function FacVarMean(Fac: TwsLIVec; IRow,LRow: Integer; Col: TwsLIVec): TwsGeneral;
    // Adiciona coluna
    Procedure AddCol(Const N, Lab: String; ColType: TwsEnumDataType; ps: byte=15; pf: byte=8); Virtual;
    // Adiciona coluna com descritor
    Procedure AddColDesc(Obj: TwsDataSetCol);  Virtual;
    // Remove uma coluna
    procedure DeleteCol(i: Integer); Override;
    // Remove as colunas referenciadas pelos ?ndices passados pelo vetor
    procedure DeleteCols(Col: TwsLIVec); override;
    // Nome da coluna identificadora
    Property ColIdentName: String Read FColIdentName Write SetColIdentName;
    // Retorna elemento especificado como strinng
    Property AsString [Index: Integer; Index: Integer]: String Read GetStr Write PutStr;
    // Retorna elemento como variante
    Property AsVariant[Index: Integer; Index: Integer]: Variant Read GetVar Write PutVar;
    // Retorna qualquer valor do DataSet na forma de uma String sem espa?os
    Property AsTrimString [Index: Integer; Index: Integer]: String Read GetTrimStr;
    // Retorna elemento como inteiro
    Property AsInteger[Index: Integer; Index: Integer]: Integer Read GetInt;
    //Retorna valores no formato Data/Tempo
    Property AsDateTime[Index: Integer; Index: Integer]: TDateTime Read GetDT;
    // Nesta classe retorna a pr?pria inst?ncia do objeto
    Property AsDataSet: TwsDataSet Read GetDataSet;

    // Estrutra das colunas
    Property Struct: TwsDSStructure read FStruct write SetStruct;

    // Eventos
    property OnChooseFont : TDS_ChooseFont_Event read FChooseFont write FChooseFont;
  End; 

{ ============================= TwsOrderedDataSet ========================== }

  {  Heran?a
       TwsOrderedDataSet --> TwsDataSet --> TwsGeneral --> TwsMatrix --> TList
     Objetivos
       Implmenta conjuntos de dados com um n?mero limitado de linhas que est?o ordenadas
       (em ordem ascendente ou descendente) em rela??o a uma coluna especificada.
   }
  TwsOrderedDataSet = Class (TwsDataSet)
  private
    // True se as ordens das linhas for ascendente
    FAscendent : Boolean;
    // N?mero m?ximo de linhas para o conjunto
    FMaxRows   : Integer;
    // Coluna utilizada para ordena??o
    FBasedCol  : Integer;
  Public
    // Cria objeto para armazenar conjuntos de dados ordenados
    Constructor Create(aName: String; aMaxRows, aBasedCol: Integer; Ascend: Boolean);
    // Insere uma linha repeitando a ordena??o
    Procedure MAdd(L: TwsVec); Override;
    // N?mero m?ximo de linhas
    property MaxRows:   Integer read FMaxRows   write FMaxRows;
    // Coluna para ordena??o
    property BasedCol:  Integer read FBasedCol  write FBasedCol;
    // Se True, a ordem das linhas ser? ascendentes
    property Ascendent: Boolean read FAscendent write FAscendent;
  End;

  // Lista de matrizes ou datasets
  TwsDataSets = class
  private
    FList: TList;
    FFreeObjects: Boolean;

    procedure setData(i: Integer; Value: TwsDataSet);
    function getDataSet(i: Integer): TwsDataSet;
    function getCount: Integer;
    destructor Destroy; override;
  public
    constructor Create(aFree: Boolean = True);
    function Add(Obj: TObject): Integer;
    procedure Delete(i: Integer);
    property DataSet[index: Integer]: TwsDataSet read getDataSet write setData; default;
    property Count: Integer read getCount;
    property FreeObjects: Boolean read FFreeObjects write FFreeObjects;
  end;

  TwsMatList = class(TwsDataSets)
  private
    procedure setMatrix(i: Integer; Value: TwsMatrix);
    function getMatrix(i: Integer): TwsMatrix;
    function getGeneral(i: Integer): TwsGeneral;
    function getSymmetric(i: Integer): TwsSymmetric;
    function getDiagonal(i: Integer): TwsDiagonal;
    function getTriangular(i: Integer): TwsTriangular;
    function getVec(i: Integer): TwsVec;
  public
//    property AsMatrix[index: Integer]: TwsMatrix read getMatrix write setMatrix;
//    property AsGeneral[index: Integer]: TwsGeneral read getGeneral;
//    property AsSymmetric[index: Integer]: TwsSymmetric read getSymmetric;
//    property AsDiagonal[index: Integer]: TwsDiagonal read getDiagonal;
//    property AsTriangular[index: Integer]: TwsTriangular read getTriangular;
//    property AsVec[index: Integer]: TwsVec read GetVec;
  end;
// Operacoes que resultam em listas de matrizes

  TwsTrDecomp = class(TwsMatList)
  private
    fRank,                // posto da matriz
    fRIdx : Integer;      // indice da primeira resposta
    fErr  : Word;         // codigo de erro
    fEps  : double;       // precisao
    fA    : TwsSymmetric; // dados originais
    fd    : TwsVec;       // diagonal como vetor
    procedure Decomp;
    procedure CovMat;
    function GetTriang: TwsTriangular;
    function GetTInv: TwsTriangular;
    function GetDiag: TwsDiagonal;
    function GetXInv: TwsSymmetric;
    function GetSS(j: Integer): double;
    function GetCoef(j: Integer): double;
  public
    constructor Create(A: TwsSymmetric; RIdx: Integer=0);
    procedure Solve(B: TwsGeneral);
    function Cholesky: TwsTriangular;
//    function Doolittle: TwsTriangular;
//    function Crout: TwsTriangular;
//    function H(X: TwsGeneral; i: Integer): double
    // fator triangular de A
    property T: TwsTriangular read GetTriang;
    // fator diagonal de A
    property D: TwsDiagonal read GetDiag;
    // inversa do fator triangular
    property TInv: TwsTriangular read GetTInv;
    // inversa de A
    property XInv: TwsSymmetric read GetXInv;
    property Eps: double read fEps write fEps;
    property Rank: Integer read fRank;
    property Err: Word read fErr;
    // obtem somas de quadrados de res?duos
    property Q[index: Integer]: Double read GetSS;
    // obtem coeficientes de regressao
    property Coef[index: Integer]: Double read GetCoef;
  end;

  TwsHouseLSP = class(TwsMatList)
    private
      fRIdx : Integer;
      fError: Word;
      fA    : TwsGeneral;
      procedure Decomp;
      function GetLSSol: TwsGeneral;
      function GetRes(i: Integer): Double;
      function GetReg(i: Integer): Double;
    public
      constructor Create(A: TwsGeneral; RIdx: Integer);
      procedure HFTCov;
      procedure Solve;
      property QR: TwsGeneral read fA;
      property Sol: TwsGeneral read GetLSSol;
      property ResSS[index: Integer]: Double read GetRes;
      property RegSS[index: Integer]: Double read GetReg;
 end;

// Rotinas gerais envolvendo matrizes e conjuntos de dados

  Function CopyDescCol(Const Col: TwsDataSetCol): TwsDataSetCol;
  // Obtencao de matrizes de arquivos texto
  function FromText(var TFile: TextFile; ChLin: Char = ';'; ChEndSec: Char = '/'): TwsMatrix;
  // Transforma um vetor numa matriz diagonal
  function VecToDiag(L: TwsVec): TwsDiagonal;
  // Autoestrutura do produto de duas matrizes
  procedure EigenProd(H,R: TwsSymmetric; out EVec: TwsGeneral; out EVal: TwsDiagonal;
    out Rank: Integer; eps: Double = 1.0e-8);
  // Quantidades obtidas com autovalores
  function EigenProp(V: TwsDiagonal; Rank: Integer): TwsGeneral;

  procedure House1(Build: Boolean; A: TwsGeneral; p, l, m, ia: Integer; var up: Double;
    C: TwsGeneral; ncb, ncp: Integer; var ErrCode: Word);
  procedure House2(Build: Boolean; A: TwsGeneral; p, l, m, ia: Integer; var up: Double;
    C: TwsGeneral; ncb, ncp: Integer; var ErrCode: Word);
  procedure House3(Build: Boolean; A: TwsGeneral; p, l, m, ia: Integer; var up: Double;
    C: TwsGeneral; ncb, ncp: Integer; var ErrCode: Word);
  // Matriz na pot?ncia
  function MatPower(A: TwsMatrix; n: Integer; var ErrCode: Word): TwsMatrix;
  // Produto que substitui uma matriz
  function MultToA(var A: TwsMatrix; B: TwsMatrix; var ErrCode: Word): TwsMatrix;
  // Transforma string numa matriz
  function StrToMat(LN,CN,P: TwsCVec; MT: TwsEnumMatType; ChLin, ChEnd: Char): TwsMatrix;
  // Transforma string numa matriz geral
  function GStrMat(P: String): TwsGeneral;

  // Aplica fun??o a matriz
  procedure FuncApply(fOp: TwsEnumConstFun; var C:TwsMatrix);
  // Produto especial
  procedure Prod1(A, B:TwsMatrix; out C:TwsMatrix);
  // Produto especial
  procedure Prod2(A:TwsMatrix; B:TwsDiagonal; PreMult:Boolean; out C:TwsGeneral);
  // Produto especial
  procedure Prod3(A:TwsMatrix; B:TwsTriangular; PreMult:Boolean; out C:TwsGeneral);
  // Produto especial
  procedure Prod4(A,B:TwsDiagonal; out C:TwsDiagonal);
  // Produto especial
  procedure Prod5(A: TwsDiagonal; B:TwsTriangular; PreMult:Boolean; out C:TwsTriangular);
  // Produto especial
  procedure Prod6(A, B:TwsTriangular; out C:TwsTriangular);
  // Opera??o por elemento especial
  procedure ElemOp1(A,B:TwsMatrix; Op:TwsEnumTypeOp; var C:TwsGeneral);
  // Opera??o por elemento especial
  procedure ElemOp2(A:TwsMatrix; v:TwsVec; Op:TwsEnumTypeOp; DFirst:Boolean; var C:TwsMatrix);
  // Opera??o por elemento especial
  procedure ElemOp3(A,B:TwsMatrix; Op:TwsEnumTypeOp; var C:TwsSymmetric);
  // Invers?o de matrizes
  function InvMat(y: TwsMatrix; NewMat: Boolean; var ErrCode: Word): TwsMatrix;
  // Solu??o de sistemas por Gauss-Jordan
  procedure GJSolve(A, B: TwsGeneral; var ErrCode: Word);
  // Solu??o de sistemas por SVD
  procedure SVDSolve(var A, B: TwsGeneral);
  // Solu??o de sistemas por Householder
  procedure HFTSolve(var A, B, X: TwsGeneral; var H: TwsVec; var ErrCode: Word);
  // Solu??o de sistemas por Householder com troca de colunas
  procedure HFTISolve(var A, B, X: TwsGeneral; var r: Integer; var Indx: TwsLIVec;
    var G, H: TwsVec; var ErrCode: word);
  //// Solu??o de sistemas atrav?s da decomposi??o LU
  procedure LUSolve(A, B: TwsGeneral);
  // Matriz identidade
  function Identity(nc: Integer): TwsGeneral;
  // Matriz de Helmert
  function Helmert(nr: Integer): TwsGeneral;
  // Matriz de reparametrizacao com contastes de Helmert
  function LHelmert(nr: Integer): TwsGeneral;
  // Matriz indicadora classificacoes simples
  function LIndic(nr: Integer): TwsGeneral;

  // Matriz de constrastes com um tratamento controle
  function Control(nr: Integer): TwsGeneral;
  // Matriz de reparametrizacao contrastes com um controle
  function LControl(nr: Integer): TwsGeneral;

  // Codifica??o experimental
  function Experim(nr: Integer): TwsGeneral;
  // Contrastes para an?lise de perfil
  function Profile(nr: Integer): TwsGeneral;
  // Matriz de reparametrizacao por meio de perfis
  function LProfile(nr: Integer): TwsGeneral;

  // Contraste n?vel contra m?dia dos demais
  function MeanTransf(nr: Integer): TwsGeneral;
   // Matriz de reparametrizacao - contraste n?vel contra m?dia dos demais
  function LMeanTransf(nr: Integer): TwsGeneral;

  // Polin?mios ortogonais
  function PolOrth(x: TwsVec;m: Integer): TwsGeneral;
  // Matriz de reparametrizacao utilizando polinomios ortogonais
  function LPolOrth(x: TwsVec; m: Integer): TwsGeneral;

  // Polin?mios ortogonais ponderados
  function WOrthPol(x, w: TwsVec;m: Integer): TwsGeneral;
  // Ajustamento por polin?mios ortogonais
  function WPolinFit(var beta: TwsTriangular;var x,w,y,sq: TwsVec;m: Integer): TwsGeneral;
  // Matriz de Hilbert
  function Hilbert(nc: Integer): TwsGeneral;
  // Matriz de constantes
  function Jota(nr, nc: Integer;MT: TwsEnumMatType;x: Double): TwsMatrix;
  // Constru??o da tabela de Duncan
  function DuncanTable(ntreat: Integer; df: TwsVec): TwsGeneral;
  {$ifdef WINSTAT_FULL}
  // Acesso a tabela de Tukey
  function TukeyTable(nTreat: Integer; df, Alpha: TwsVec): TwsGeneral;
  {$endif}
  // Concatena??o de vetor com vetor
  function HVecVecConcat(v1,v2: TwsVec;var Erro: Word): TwsGeneral; { Alex 09/10/97 }
  // Transforma um PChar numa matriz
  function CharToMat(p:PChar; MT: TwsEnumMatType): TwsMatrix;{ Alex 09/10/97}
  // Atribui nomes de colunas
  procedure SetColName(A: TwsMatrix;C:PChar); { Alex 09/10/97 }
  // estat?sticas (Rochedo)
  function Statistics(A: TwsGeneral; const Col: Array of byte;
    const Stat: Array of TwsEnumStatistics): TwsGeneral;
  // Soma de produtos igual a zero
  function IsOrthogonal(Y: TwsGeneral; w: TwsVec; j,k: Integer): boolean;
  // Verifica se a soma ? zero
  function IsContrast(Y: TwsGeneral; k: Integer): boolean;
  // Verifica se todas as colunas definem pares de contrastes ortogonais
  function AreOrthogonal(Y: TwsGeneral; var ColI, ColJ: Integer): Boolean;
  // Verifica se todas as colunas definem contrastes
  function AreContrast(Y: TwsGeneral; var Col: Integer): boolean;

  Function wsMatrixMax  (M: TwsGeneral; Col, Ini, Fim: Longint): Double;
  Function wsMatrixMin  (M: TwsGeneral; Col, Ini, Fim: Longint): Double;
  Function wsMatrixMean (M: TwsGeneral; Col, Ini, Fim: Longint): Double;

{
 A aus?ncia do par?metro Media significa que a rotina ter? que calcular a media
 EX:
    res := wsMatrixDSP(y, 3, 1, 10); --> a rotina ter? que calcular a media
    res := wsMatrixDSP(y, 3, 1, 10, 4.55); --> a rotina usar? a m?dia ja calculada 4.55
}
  Function wsMatrixVar (M: TwsGeneral; Col, Ini, Fim: Longint; Media: Double = -1): Double;
  Function wsMatrixDSP (M: TwsGeneral; Col, Ini, Fim: Longint; Media: Double = -1): Double;
  function wsMatrixCV  (M: TwsGeneral; Col, Ini, Fim: Longint; Media: Double = -1): Double;
  Function wsMatrixSum (M: TwsGeneral; Col, Ini, Fim: Longint; Var Somados: Longint): Double;

  // Dado um tipo de variavel cria uma instancia nao inicializada
  function CreateDatasetVar(varType: byte): TwsDataSetCol;

  // Obtem esquema de casualizacao
  function TreatRand(n: Integer): TwsGeneral;

implementation
uses SysUtils,
     Windows,
     wsVars,
     wsGLib,
     wsMath,
     {$ifdef WINSTAT_FULL}
     wsFuncoesDeProbabilidade,
     wsProbabilidade,
     wsLund,
     {$endif}
     wsFuncoesDeEscalares;

function RGBColorAsString(Color: TColor): String;
var c: Longint;
begin
  c := ColorToRGB(Color);
  Result := intToHex(getRValue(c), 2) +
            intToHex(getGValue(c), 2) +
            intToHex(getBValue(c), 2);
end;

{ TwsMatrix }

constructor TwsMatrix.Create(NR, NC: Integer);
{ Objetivo
    Criar e inicializar um objeto do tipo TwsMatrix. Esta classe ? um tipo b?sico de matriz e n?o
    deve ser utilizada diretamente. Somemnte os descendentes criam matrizes completas.
  Par?metros
    NR: N?mero de linhas da matriz
    NC: N?mero de colunas. Se nc=0 nenhum nome de coluna ser? criado
  M?todos chamados
    Create herdado
  Campos alterados
    FModified
    FNCols
    FNRows
    PrintOptions
    FCName
}
var
  i: Integer;
begin
  inherited Create;
  FList := TList.Create;
  FVersion := '1.0';
  FileName := '';
  FModified := False;
  FNCols := NC;
  FNRows := NR;

  PrintOptions.LineLen := 400;
  PrintOptions.MaxIDSize := 8;
  PrintOptions.Center := False;
  PrintOptions.ColPrecision := 7;
  PrintOptions.ColWidth := PrintOptions.ColPrecision+8;

  FCName := nil;
  If FNCols > 0 Then
    Begin
    FCName := TStringList.Create;
    TStringList(FCName).Duplicates := dupError; {N?o permite colunas repetidas}
    for i := 1 to FNCols do FCName.Add('Col' + IntToStr(i));
    End;
  FName := '';
  Mlab:='';
end; { TwsMatrix.Create }

destructor TwsMatrix.Destroy;
{ Objetivo
    Liberar mem?ria utilizada pelo objeto da classe
  Campos liberados
    FCName
    Linhas (vetores)
    MLab
  M?todos chamados
    Destroy herdado
}
var
  i: Integer;
begin
  FCName.Free; {Gerenciado diretamente pela classe TwsMatrix}
  for i := 0 to FList.Count-1 do
    TwsVec(FList[i]).Free;
  DisposeStr(FMLab);
  FList.Free;
  Inherited Destroy;
end; { Destroy }

// IToXML interface *************************************************************************

function TwsMatrix.GetBlockName: String;
begin
  Result := 'wsMatrix';
end;

function TwsMatrix.GetBlockNameComment: String;
begin
  Result := 'Estilo de formata??o para Matrizes do tipo TwsMatrix';
end;

procedure TwsMatrix.ToXML(Buffer: TStrings; Ident: Integer);
var x: TXML_Writer;
    i, j: Integer;
    s: String;
    b: Boolean;
    Cor: TColor;
begin
  x := TXML_Writer.Create(Buffer);
  x.IdentSize := Ident + 2;

  x.BeginTag('Matrix',
    ['Name', 'Rows', 'Cols', 'MaxIDSize', 'Precision', 'Label', 'PrintDescVars'],
    [Name, nRows, nCols, PrintOptions.MaxIDSize, PrintOptions.ColPrecision, MLab, PrintDescVars]);

    Ident := x.BeginIdent;
    x.BeginTag('Data');
      Ident := x.BeginIdent;
      StructColsToXML(Buffer, Ident);
      RowsToXML(x);

      x.EndIdent;
    x.EndTag('Data');
    x.EndIdent;
  x.EndTag('Matrix');
  x.Free;
end;

procedure TwsMatrix.ToBXML(Buffer: TStrings; Ident: Integer);
var s: String;
begin
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<' + GetBlockName + ':block>');
  ToXML(Buffer, Ident);
  Buffer.Add(s + '</' + GetBlockName + ':block>');
end;

// Mostra a matriz em uma planilha semelhante as planilhas do Excel
procedure TwsMatrix.ShowInSheet(Sheet: TBaseSpreadSheetBook);
var s: String;
    L, C: Integer;
begin
  Sheet.ActiveSheet.ShowHeaders := False;

  StartWait();
  Sheet.BeginUpdate();
  try
    if Name <> '' then s := ' ' + Name else s := ' Sem Nome';
    if MLab <> '' then
       Sheet.Caption := s + ' - ' + MLab
    else
       Sheet.Caption := s;

    // Nome das colunas
    for C := 1 to nCols do
      begin
      Sheet.ActiveSheet.WriteCenter(1, C+1, ColName[C]);
      Sheet.ActiveSheet.BoldCell(1, C+1);
      end;

    for L := 1 to nRows do
      begin
      if RowName[L]<>'' then s:=RowName[L] else s:=IntToStr(L);
      Sheet.ActiveSheet.Write(L+1,1,True,s);
      for C := 1 to nCols do
        Sheet.ActiveSheet.WriteCenter(L+1,C+1,GetStr(L,C))
      end;
  finally
    Sheet.EndUpdate();
    StopWait();
  end;
end;


function TwsMatrix.GetClassName: String;
begin
  Result := self.ClassName;
end;

// IToXML interface *************************************************************************

procedure TwsMatrix.StructColsToXML(Buffer: TStrings; var Ident: Integer);
var i: Integer;
    s, sIdent: String;
begin
  sIdent := StringOfChar(' ', Ident);
  Buffer.Add(sIdent + '<Cols>');
  sIdent := sIdent + '  ';
  for i := 0 to FCName.Count-1 do
    Buffer.Add(sIdent + '<Col>' + FCName[i] + '</Col>');
  sIdent := StringOfChar(' ', Ident);
  Buffer.Add(sIdent + '</Cols>');
end;

procedure TwsMatrix.RowsToXML(Writer: TXML_Writer);
var i, j: Integer;
    s: String;
    b: Boolean;
    Cor: TColor;
begin
  b := (Assigned(FSelectElementColor));
  for i := 1 to FnRows do
    begin
    if Row[i].Name <> '' then
       s := '<row Label="' + XML_EncodeChars(Row[i].Name, True) + '">'
    else
       s := '<row Label="' + IntToStr(i) + '">';

    for j := 1 to FnCols do
       if b then
          begin
          Cor := clblack;
          FSelectElementColor(Self, i, j, Cor);
          if Cor <> clblack then
             s := s + '<e cor="#' + RGBColorAsString(Cor) + '">'
          else
             s := s + '<e>';

          s := s + XML_EncodeChars(SysUtilsEx.AllTrim(getStr(i, j)), False) + '</e>';
          end
       else
          s := s + '<e>' + XML_EncodeChars(SysUtilsEx.AllTrim(getStr(i, j)), False) + '</e>';

    Writer.Write(s + '</row>');
    end;
end;
procedure TwsMatrix.InternalSave1(Writer: TWriter);
{ Objetivo
    Grava a primeira parte da estrutura de uma matriz (ver comentarios c?digo)
  Par?metros
    Writer: Objeto de escrita em disco
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Writer.WriteSignature;
  Writer.WriteInteger( Integer(FMatType) );
end;

procedure TwsMatrix.InternalSave2(Writer: TWriter);
{ Objetivo
    Grava a segunda parte da estrutura de uma matriz (ver comentarios c?digo)
  Par?metros
    Writer: Objeto de escrita em disco
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  with Writer do
    begin
    WriteString(FVersion);
    Write(FExtra, SizeOf(FExtra));

    WriteString(FName);
    WriteString(MLab);
    WriteInteger(Tag_1);
    WriteInteger(Tag_2);

    WriteInteger(NRows);
    WriteInteger(PrintOptions.MaxIdSize);

    CNameSavetoStream(Writer);
    end;
end;

procedure TwsMatrix.SetName(Const Name: String); {Rochedo, 04/12/1998}
{  Objetivo
     Atribui nome da matriz
   Par?metros
     Name: Nome que ser? atribu?do ? matriz
   M?todos chamados
     Nenhum
   Campos alterados
     FName
     PrintOptions.MaxIdSize
}
Begin
  FModified := True;
  If not SysUtilsEx.IsValidIdent(Name) Then
     FName := GetValidId(Name)
  Else
     FName := Name;
  if Length(FName) > PrintOptions.MaxIdSize then PrintOptions.MaxIdSize := Length(FName)
End;
(*
Function TwsMatrix.GetSize: Integer;
Begin
  {NumeroDeColunas * NumeroDeLinhas * TamanhoDeCadaUm * TamanhoDoFloat (+/-) +
   Tamanho da instancia do Objeto}
  {Implementar o size para os TVecs tambem}
  {FSize :=}
  Result := FSize;
End;
*)
procedure TwsMatrix.SetColWidth(Value: byte);
{ Objetivo
    Atribui um tamanho para impress?o das colunas da matriz
  Par?metros
    Value: largura de impress?o das colunas
  M?todos chamados
    Nenhum
  Campos modificados
    PrintOptions.ColWidth
}
var
  MaxColNameSize, i: Integer;
begin
  MaxColNameSize := 0;
  for i := 1 to FNCols do
    if Length(ColName[i]) > MaxColNameSize then MaxColNameSize := Length(ColName[i]);

  if Value >= MaxColNameSize then
     PrintOptions.ColWidth := Value
  else
     PrintOptions.ColWidth := MaxColNameSize + 1;
end;

Function TwsMatrix.GetMLab :String;
{  Objetivo
     Recupera r?tulo da matriz
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Begin
  Result:=FMLab^;
End; { TwsMatrix.GetMLab }

Procedure TwsMatrix.PutMLab(Const l:String);
{ Objetivo
    Atribui o r?tulo da matriz
 Par?metros
    l: R?tulo a ser atribu?do
}
Begin
  FModified := True;
  If FMLab <> Nil Then DisposeStr(FMLab);
  FMLab := NewStr(l);
End; { TwsMatrix.PutMLab }

Class Function TwsMatrix.ObjectFromStream(Reader: TReader; Var Erro: Integer): TwsMatrix;
 { Objetivo
    Recuperar um objeto TwsMatrix armazenado em disco. Este m?todo n?o deverp? ser utilizado
    diretamente.
  Par?metros
    Reader: Objeto respons?vel pela leitura
    Erro: C?digo de erro. NReadError: Erro de leitura
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Begin
  Case InternalLoad1(Reader) of
    mtGeneral     : Result := TwsGeneral.Create(0,0);
    mtSymmetric   : Result := TwsSymmetric.Create(0);
    mtDiagonal    : Result := TwsDiagonal.Create(0);
    mtTriangular  : Result := TwsTriangular.Create(0);
    mtVandermonde : Result := TwsVandermonde.Create(0);
    mtToeplitz    : Result := TwsToeplitz.Create(0);
    mtDataSet     : Result := TwsDataSet.Create;
    else            Result := UserCreate;
    End;

  Try
    Result.InternalLoad2(Reader);
    Result.InternalLoad3(Reader);
  Except
    Erro := NReadError;
  End;
End; { TwsMatrix.ObjectFromStream }

Class Function TwsMatrix.LoadFromFile(Const FileName: String): TwsMatrix;
 { Objetivo
     Leitura da matriz no disco
   Par?metros
     FuileName: nome do arquivo
  M?todos chamados
    ObjectFromStream
  Campos modificados
    Nenhum
}
Var Stream  : TFileStream;
    Reader  : TReader;
    Erro    : Integer;
Begin
  Erro := 0;
  Stream := TFileStream.Create(FileName, fmOpenRead);
  Try
    Reader := TReader.Create(Stream, $FF);
    Try
      Result := ObjectFromStream(Reader, Erro);
      If Erro <> 0 Then
         Raise Exception.Create('Erro de leitura do arquivo: '#13#10 + FileName);

      Result.FileName := FileName;
    Finally
      Reader.Free;
      End;
  Finally
    Stream.Free;
    Result.FModified := False; //Alexdg (22/02/2000)
    End; { Try }
End; { TwsMatrix.LoadFromFile }

Procedure TwsMatrix.CNameLoadFromStream(Reader: TReader);
{ Objetivo
    Retorna os nomes de colunas de um arquivo.
  Par?metros
    Reader: Objeto respons?vel pela leitura
  M?todos chamados
    Nenhum
  Campos modificados
    FCName
    FNCols
}
var s  : String;
    so : String;
    i  : Integer;
Begin
  With Reader Do
    Begin
    FCName := TStringList.Create;
    TStringList(FCName).Duplicates := dupError; {N?o permite colunas repetidas}
    ReadListBegin;
    While Not EndOfList Do
      Begin
      s := ReadString;
      so := s;

      i := 1;
      While (FCName.IndexOf(s) <> -1) Do
        Begin
        Inc(i);
        s := so + IntToStr(i);
        End;

      FCName.Add(s);
      Inc(FNCols);
      End;
      ReadListEnd;
    End; { With Reader }
End; { TwsMatrix.CNameLoadFromStream }


Procedure TwsMatrix.CNameSaveToStream(Writer:TWriter);
 { Objetivo
    Armazena os nomes das colunas em um arquivo.
  Par?metros
    Writer: Objeto respons?vel pela escrita dos nomes das colunas (armazenadas em CName) no arquivo
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Var
  i  :Integer;
Begin
  With Writer Do
    Begin
      WriteListBegin;
      For i:=0 To FCName.Count-1 Do
        WriteString(FCName.Strings[i]);
      WriteListEnd;
    End; { With Reader }
End; { TwsMatrix.CNameSaveToStream }

Procedure TwsMatrix.SaveToFile(Const FileName: String);
{ Objetivo:
    Gravar o objeto num arquivo com o nome especificado. ? o m?todo que ser? utiizado para
    armazenar objetos da classe TwsMatrix
  Par?metros:
    Name: Nome do arquivo onde o objeto ser? armazenado
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Var Stream: TFileStream;
    Writer: TWriter;
    st: String;
Begin
  st:=FileName;
  if ExtractFileExt(st)='' then
    if Self is TwsDataSet then
      st:=st+'.dst'
    else
      st:=st+'.mat';

  Stream := TFileStream.Create(st, fmCreate or fmOpenWrite);
  Try
    Writer := TWriter.Create(Stream, $FF);
    Try
      InternalSave1(Writer); // Est?tico
      InternalSave2(Writer); // Est?tico
      InternalSave3(Writer); // Virtual

      FModified     := False;
      Self.FileName := st;
    Finally
      Writer.Free;
    End; { Try }
  Finally
    Stream.Free;
  End; { Try }
End; { TwsMatrix.SaveToFile }

procedure TwsMatrix.NotificationAndFree;
begin
  GetMessageManager.SendMessage(WSM_REMOVER_OBJETO, [Self]);
  Free;
end;

// ver quem esta utilizando
function TwsMatrix.ColInd(Const s: string): string;
{ Objetivo
    Retorna um string com os ?ndices dos nomes de colunas especificados.
    Os caracteres separadores dos nomes s?o [#9,#10,#13,' ',',','=','\', '"','(',')'].
    O caracter '-' indica amplitude, ou seja, todas as colunas que est?o entre as duas especificadas.
  Par?metros
    s: string com os nomes das colunas
}
Const
  DelChar: TCharSet = [#9,#10,#13,' ',',','=','\', '"','(',')'];
  RangeChar ='-';
var
    tok   :string;
    pos   :Integer;
    t     :char;
begin
  Result:='';
  pos:=1;
  while pos<=length(s) do
    if obtem_token(s,pos,tok)=0 then
      if length(tok)=1 then
        begin
        t:=tok[1];
        if (t in DelChar) or (t = RangeChar) then
          Result:=Result+tok
        else
          Result:=Result+IntToStr(FCName.IndexOf(s));
        end
      else
        Result:=Result+IntToStr(FCName.IndexOf(tok));
end; { TwsMatrix.ColInd }

procedure TwsMatrix.SetCName(st: TwsCVec; DelChar:TCharSet);
{ Objetivo
    Constroi a lista de nomes de colunas especificados. Se j? existe uma lista de nomes,
    ela sera substituida. Se a quantidade de nomes n?o for suficiente, os nomes ser?o
    completados com 'Col'+n?mero da coluna
  Par?metros
    st     : Objeto string com os nomes
    DelChar: Conjunto de caracteres separadores dos nomes
  M?todos chamados
    Nenhum
  Campos modificados
    FCName
}
var
  i,j: Integer;
  st1: String;
begin
  FModified := True;
  if FCName <> nil then FCName.Free;
  FCName := TStringList.Create;
  // Insere os nomes das colunas
  j := 1; i := 0;
  st1 := st.StrGet(j, DelChar);
  while (st1 <> '') and (i < FNCols) do
    begin
    Inc(i);
    FCName.Add(st1);
    st1 := st.StrGet(j, DelChar);
    end;
  // Completa se for necess?rio
  for i:=FCName.Count+1 to FNCols do
    FCName.Add('Col'+IntToStr(i))
end; { TwsMatrix.SetCName }

// Obtem os dados do campo extra
function TwsMatrix.getExtraData(i: Integer): byte;
begin
  Result := FExtra[i];
end;

// seta os dados do campo extra
procedure TwsMatrix.setExtraData(i: integer; Value: byte);
begin
  FExtra[i] := Value;
end;

procedure TwsMatrix.SetNRows(NR: Integer);
{ Objetivo
    Estabelece o n?mero de linhas da matriz
  Par?metros
    NR: N?mero de linhas
  M?todos chamados
    Nenhum
  Campos modificados
    FNRows
}
begin
  if NR <= MaxListSize then
    FNRows := NR
end;

function TwsMatrix.IsType(AType: TwsEnumMatType): Boolean;
{ Objetivo
    Retorna True se o tipo da matriz ? o especificado
  Par?metros
    AType: Tipo para verfifica??o
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  IsType := AType = FMatType
end;

function TwsMatrix.ByElement(B:TwsMatrix; Op:TwsEnumTypeOp; NewMat:Boolean;
  var ErrCode:Word):TwsMatrix;
{ Objetivo
    Operar as matrizes elemento a elemento
  Par?metros
    Op: Operador desejado. Os operadores dispon?veis s?o:
        opSum,opSub,opDiv,opProd,opPower  // Operadores aritmeticos
        opGE,opGT,opLE,opLT,opEQ,opNE     // Comparacao
        opOR,opAnd                        // Logicos
        opMax,opMin                       // Maximo, minimo
    NewMat  : True, se o resultado vir? numa nova matriz, False para o resultado retornar na
      pr?pria matriz. Uma nova matriz ser? criada sempre que o tipo da matriz que chama for
      diferente da matriz geral. As exce??es ficam para o caso de ambas matrizes sim?tricas
      (qualquer operador) e ambas matrizes diagonais (operador opProd)
    ErrCode: Retorna 0 se a opera??o for poss?vel (matrizes de mesma ordem) e NImprDim -
      dimens?es impr?prias para a opera??o - caso contr?rio.
    Retorno
      Matriz com o resultado. O retorno na pr?pria matriz ser? poss?vel quando a matriz que
      chama ? geral, quando ambas s?o sim?tricas (retorna sim?trica) ou quando ambas s?o diagonais
    Valores perdidos
      N?o trata
}
var
  i,j: Integer;
begin
  ErrCode:=0;
  if (NRows=B.NRows) and (NCols=B.NCols) then
    begin
    case B.MatType of
      mtGeneral,
      mtTriangular,
      mtToeplitz,
      mtVandermonde:
        begin
        if NewMat or (MatType<>mtGeneral) then
          Result:=TwsGeneral.Create(NRows,NCols);
        ElemOp1(Self,B,Op,TwsGeneral(Result));
        end;
      mtSymmetric:
        if MatType = mtSymmetric then
          begin
          if NewMat then
            Result:=TwsSymmetric.Create(NCols)
          else
            Result:=Self;
          ElemOp3(Self,B,Op,TwsSymmetric(Result));
          end
        else
          begin
          if NewMat then
            Result:=TwsGeneral.Create(NRows,NCols);
          ElemOp1(Self,B,Op,TwsGeneral(Result));
          end;
      mtDiagonal:  // testar todas as combinacoes
        begin
          if MatType = mtDiagonal then
            case Op of
              opSum,opSub,opDiv,opProd,opMax,opMin:
                begin
                if NewMat then
                  Copy(mtDiagonal,Result);
                ElemOp2(Result,B.Row[1],Op,False,Result);
                end;
              else  // case
                begin
                Copy(mtGeneral,Result);
                ElemOp1(Self,B,Op,TwsGeneral(Result));
                end
              end // case Op
          else
            begin
            case Op of
              opSum,opSub:
                begin
                Copy(mtGeneral,Result);
                ElemOp2(Result,B.Row[1],Op,False,Result);
                end;
              opProd:
                begin
                Copy(mtDiagonal,Result);
                Result.Row[1].ByElement(B.Row[1],Op,False,ErrCode)
                end;
              else
                ElemOp1(Self,B,Op,TwsGeneral(Result));
              end // case Op
            end
        end; // mtDiagonal
      end; // case B.MatType
    end
  else
    begin
    ErrCode:=NImprDim;
    Result:=nil
    end // if ErrCode
end; // ByElement

function TwsMatrix.Transpose: TwsMatrix;
{ Objetivo
    Retorna a transposta da matriz que chama o m?todo
  Retorno
    Sempre retorna uma matriz geral como a transposta
}
var
  i, j: Integer;
begin
  Result := TwsGeneral.Create(NCols,NRows);
  for j := 1 to Result.NCols do
    Result.ColName[j]:= RowName[j];
  for i := 1 to Result.NRows do
    begin
    Result.RowName[i]:=ColName[i];
    for j := 1 to Result.NCols do
      Result[i,j] := Self[j, i]
    end
end; { Transpose }


function TwsMatrix.Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix;
{ Objetivo
    Retorna o produto entre duas matrizes. Utilizado quando a matriz que chama o m?todo ? geral,
    sim?trica, mtVandermonde ou mtToeplitz
  Par?metros
    B      : Matriz para o (pos)produto com a matriz que chama o m?todo
    ErrCode: C?digo de erro. O produto somente sera poss?vel (ErrCode retorna 0) se o
      n?mero de colunas de quem chama ? igual ao n?mero de linhas de B; caso contr?rio
      ErrCode retorna a constante NImprDim (Dimens?es impr?prias para a opera??o) e a
      fun??o retorna nil
  Retorno
    Retorna sempre uma matriz geral
  Valores perdidos
    N?o trata
}
begin
  ErrCode := 0;
  if (NCols = B.NRows) then
    begin
    case B.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz: Prod1(Self,B,Result);
      mtDiagonal: Prod2(Self,TwsDiagonal(B),False,TwsGeneral(Result));
      mtTriangular: Prod3(Self,TwsTriangular(B),False,TwsGeneral(Result));
    end; // case
    end
  else
    begin
    ErrCode := NImprDim;
    Result:=nil
    end
end;

function TwsMatrix.Kronecker(X: TwsMatrix): TwsMatrix;
{ Objetivo
    Retorna o produto de Kronecker entre duas matrizes
  Par?metros
    X: Matriz com a qual ser? feito o produto de Kronecker
  Retorno
    Se A ? a matriz m x n que chama o m?todo e X uma matriz p x q este m?todo retorna uma
    matriz geral mp x nq com o produto de Kronecker
  Valores perdidos
    Produto inv?lido ? considerado valor perdido
}
var
  m,i,k,l,
  j,ly,my,ny: Integer;
  L2        : TwsVec;
  z         : Double;
begin
  my := NRows*X.NRows;
  ny := NCols*X.NCols;
  Result := TwsGeneral.Create(my, ny);
  ly := 0;
  for m := 1 to NRows do
    begin
    for i := 1 to X.NRows do
      begin
      Inc(ly);
      L2 := Result.Row[ly];
      k := 0;
      l := 1;
      repeat
        z := Self[m,l];
        for j := 1 to X.NCols do
          begin
          Inc(k);
          L2[k] := ScalarProd(z,X[i,j])
          end;
        Inc(l);
      until k = ny;
      end { for }
    end; { for }
end; { Kronecker }

function TwsMatrix.VecKronecker(x: TwsVec): TwsMatrix;
{ Objetivo
    Obt?m o produto de Kronecker de uma matriz por um vetor
  Par?metros
    x: Vetor para o produto
  Retorno
    Se a matriz que chama ? de ordem m x n e o vetor ? de ordem k ent?o o retorno ? uma
    matriz geral de ordem m x nk
  Valores perdidos
    Produto que envolve valor(es) perdidos retorna valor perdido
}
var
  i,j,k,l: Integer;
  aux    : Double;
begin
  Result := TwsGeneral.Create(NRows, NCols*x.Len);
  for i:=1 to NRows do
    begin
    k:=0;
    for j:=1 to x.Len do
      begin
      aux:=x[j];
      for l:=1 to NCols do
        begin
        Inc(k);
        Result[i,k]:=ScalarProd(aux,Self[i,l])
        end
      end // para cada elemento do vetor
    end; // para cada linha da matriz
end;

function TwsMatrix.ColKronecker(X:TwsMatrix; i,j:Integer): TwsVec;
{ Objetivo
    Retorna o produto de Kronecker entre as colunas especificadas
  Par?metros
    X: matriz para o produto
    i,j: ?ndices das colunas da matriz e de X, respectivamente
  Retorno
    Se a matriz que chama ? de ordem m x n e X ? de ordem p x q, retorna um vetor (TwsDFVec)
    de ordem mp, que corresponde ao produto de Kronecker entre a coluna i da matriz e coluna
    j de X
  Valores perdidos
    Produtos envolvendo valores perdidos retornam valores perdidos
}
var
  kk,k,l: Integer;
  z     : Double;
begin
  Result := TwsDFVec.Create(NRows*X.NRows);
  kk := 0;
  for k := 1 to NRows do
    begin
    z := Self[k, i];
    for l := 1 to X.NRows do
      begin
      Inc(kk);
      Result[kk] := ScalarProd(z,X[l, j])
      end
    end;
end; { ColKronecker }

function TwsMatrix.HorKronecker(X: TwsMatrix;Var ErrCode: Word): TwsMatrix;
{ Objetivo
    Faz produto de Kronecker linha por linha
  Par?metros
    X: Matriz para realiza??o do produto
    ErrCode: C?digo de erro. Retorna 0 se o n?mero de linhas da matriz que chama ? igual ao
    n?mero de linhas de X; NImprDim (dimens?es impr?prias para a opera??o) caso contr?rio
  Retorno
    Se A (com dimens?o m x n) ? a matriz que chama e X ? de ordem p x q retorna uma matriz
    geral de dimens?o m x nq
  Valores perdidos
    Produto envolvendo valores perdidos retornam valores perdidos.
}
var
  i,ii,j,
  n,k    : Integer;
  v      : TwsVec;
  aux    : Double;
begin
  ErrCode:=0;
  If (NRows=X.NRows) Then
    Begin
    n:=NCols*X.NCols;
    Result := TwsGeneral.Create(0, n);
    for i := 1 to NRows do
      begin
      v:=TwsDFVec.Create(n);
      ii:=0;
      for j:=1 to NCols do
        begin
        aux:=Self[i,j];
        for k:=1 to X.NCols do
          begin
          Inc(ii);
          v[ii]:=X[i,k]*aux;
          end;
        end;
        Result.MAdd(v);
      end
    End
  Else
    Begin
    ErrCode:=NImprDim;
    Result:=Nil;
    End;
end; { HorKronecker }

function TwsMatrix.VecMult(v:TwsVec; PreMult:Boolean; Var ErrCode:Word): TwsDFVec;
{Objetivo
   Fazer o produto de uma matriz por um vetor
 Par?matros
   A: Vetor para o produto
   PreMult: True se o vetor premultiplicar a matriz; False caso contrario
   ErrCode: C?digo de erro. Retorna 0 se o produto for poss?vel e NImprDim (dimens?es
     impr?prias para a opera??o) se a) n?mero de componentes do vetor diferente do n?mero
     de linhas da matriz (PreMult=True); b) n?mero de colunas da matriz diferente do n?mero
     de componentes do vetor (PreMult=False)
   Retorno
     Retorna sempre um vetor
}
var
  i,j: Integer;
  aux: Double;
begin
  ErrCode:=0;
  if PreMult then { premultiplicacao da matriz pelo vetor }
    if (v.Len = NRows) then
      begin
      Result:=TwsDFVec.Create(NCols);
      for j:=1 to NCols do
        begin
        aux:=0;
        for i:=1 to NRows do
          aux:=aux+v[i]*Self[i,j];
        Result[j]:=aux
        end
      end
    else
      begin
      ErrCode:=NImprDim;
      Result:=nil;
      Exit
      end
  else
    if (v.Len=NCols) then
      begin
      Result:=TwsDFVec.Create(NRows);
      for i:=1 to NRows do
        begin
        aux:=0;
        for j:=1 to NCols do
          aux:=aux+v[j]*Self[i,j];
        Result[i]:=aux
        end
      end
    else
      begin
      ErrCode:=NImprDim;
      Result:=nil;
      Exit
      end
end;

function TwsMatrix.Func(fOp:TwsEnumConstFun; NewMat:Boolean): TwsMatrix;
{ Objetivo
    Aplica fun??es matem?ticas aos elementos da matriz
  Par?metros
    fOp: Fun??o desejada. As possibilidades s?o:
      cABS      - valor absoluto
      cEXP      - exponencial
      cAPROXIMA - Aproxima valores dentro de um limite pr?-estabelecido
      cINT      - Parte inteira do valor
      cLN       - Logaritmo neperiano
      cRAIZ     - Raiz quadrada
      cARCTAN   - Arco tangente
      cARCSEN   - Arco seno
      cARCCOS   - Arco cosseno
      cSEN      - Seno
      cCOS      - Cosseno
      cSENH     - Seno hiperb?lico
      cCOSH     - Cosseno hiperb?lico
      cTAN      - Tangente
      cLOG      - Logaritmo decimal
      cANG      - Transforma??o angular
      cLGAMA    - Logaritmo da fun??o gama
      cTGAMA    - Derivada da digamma
      cFLOOR    - Maior inteiro
      cCEIL     - Menor inteiro
      cINV      - Inverso do valor
      cFRAC     - Parte fracion?ria
      cTANH     - Tangente hiperb?lica
      cAcum     - Valores acumulados
    NewMat: True se o retorno se dar? em outra matriz; False se o retorno ser? na pr?pria
      matriz. Par?metro considerado somente se matriz for geral.
  Valores perdidos
    Fun??es aplicadas a valores impr?prios ou perdidos retornam valores perdidos
  Retorno
    Os valores retornam numa matriz geral
}
begin
  if NewMat or (MatType<>mtGeneral) then
    Copy(mtGeneral,Result)
  else
    Result:=Self;
  FuncApply(fOp,Result)
end;

// Rochedo - 15/12/1998
function TwsMatrix.IsMissValue(L, C: Integer; out x: Double): Boolean;
{ Objetivo
    Verifica se um elemento especificado for valor perdido
  Par?metros
    L, C: Linha e coluna, respectivamente, do elemento
    x   : retorna o valor do elemento
  Retorno
    True se x ? um valor perdido
}
var
  aux: Double;
begin
  x := Get(L, C);
  Result:=wsGLib.IsMissValue(x)
end;

Function TwsMatrix.IndexColsFromStrings(Cols: TStrings): TwsLIVec;
{ Objetivo
    Vetor de ?ndices de colunas especificadas
  Par?metros
    Cols: Lista com os nomes das colunas
  M?todos chamados
    IndexOf
  Campos modificados
    Nenhum
  Observa??es
    Ocorre uma exce??o se algum nome da lista n?o for nome de coluna
}
Var
  i,k: Integer;
Begin
  Result := TwsLIVec.Create(Cols.Count);
  For i := 0 to Cols.Count - 1 do
    Begin
    K := FCName.IndexOf(Cols[i]);
    If K <> -1 Then
       Result[i+1] := K + 1
    Else
       Begin
       Result.Free;
       Raise Exception.CreateFmt('Classe: %s'#13 +
                                 'M?todo: %s'#13 +
                                 'Erro: Vari?vel < %s > n?o existe',
                                 [ClassName, 'IndexColsFromStrings', Cols[i]]);
       End;
    End;
End; {IndexColsFromStrings}

function TwsMatrix.IndexColsFromString(const Col: String): TwsLIVec;
{ Objetivo
    Retorna um array de ?ndices de colunas especificadas na forma de um string
  Par?metros
    Col: string com os nomes de colunas
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Result := wsIndexCols(Col, FCName);
end; { IndexColsFromString }

Function TwsMatrix.IndexByName(Const Name : String) : Integer;
{ Objetivo
    Dado um nome de coluna, retorna o seu ?ndice
  Par?metros
    Name: Nome da coluna para obten??o do ?ndice
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observa??o
    Ocorre uma exce??o se o nome especificado n?o for nome de nenhuma coluna
}
Begin
  Result := FCName.IndexOf(Name);
  If Result > -1 Then
     Inc(Result)
  Else
     Raise Exception.CreateFmt('Classe: %s'#13 +
                               'M?todo: %s'#13 +
                               MsgInvalidNameVar,
                               [ClassName, 'IndexByName', Name])
End;

function TwsMatrix.GetRow(i: Integer): TwsVec;
{ Objetivo
    Dado um ?ndice, retorna o endere?o da linha
  Par?metros
    i: ?ndice da linha. Baseado em 1.
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Result := TwsVec(FList[i-1]);
end;

Procedure TwsMatrix.SetRow(i: Integer; v: TwsVec);
{ Objetivo
    Atribui um vetor como linha da matriz numa posi??o especificada. A linha que existe na posi??o
    ser? descartada.
  Par?metros
    i: Posi??o onde ser? colocada a linha
    v: Vetor que passar? a ser a linha na posi??o i
  M?todos chamados
    Nenhum
  Campos modificados
    Linha i
}
Begin
  FModified := True;
  If (i <= NRows) And (v.Len = NCols) Then
      Begin
      If v <> FList[i-1] Then TwsVec(FList[i-1]).Free;
      FList[i-1] := v;
      End
  Else
      Raise Exception.Create('Linha inv?lida passada para a matriz ' + Name);
End; { TwsMatrix.SetRow }

procedure TwsMatrix.SetRName(st: TwsCVec; DelChar :TCharSet);
{ Objetivo
  Cria nomes de linhas a partir de um objeto string
Par?metros
  st     : objeto string com os nomes de linhas
  DelChar: Conjunto de caracteres separadores de nomes de linhas.
  M?todos chamados
    Nenhum
  Campos modificados
    Nomes das linha
}
var
  st1: string;
  i,j: Integer;
begin
  FModified := True;
  j := 1; i := 0;
  st1 := st.StrGet(j, DelChar);
  while (st1 <> '') and (i < FList.Count) do
    begin
    Inc(i);
    RowName[i] := st1;
    st1 := st.StrGet(j, DelChar);
    end;
  for j:=i+1 to FNRows do
    RowName[j]:='L'+IntToStr(j)
end; { TwsMatrix.SetRName }

function TwsMatrix.GetRowName(i: Integer): string;
{ Objetivo
    Retorna o nome da linha especificada
  Par?metros
    i: ?ndice da linha para obten??o do nome
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Result := Row[i].Name;
end;

procedure TwsMatrix.SetRowName(i: Integer; const s: string);
{ Objetivo
    Atribui nome ? linha com ?ndice especificado e atualiza PrintOptions.MaxIdSize, se necess?rio.
  Par?metros
    i: ?ndice da linha que ter? nome atribu?do
    s: Nome da linha
  M?todos chamados
    Nenhum
  Campos modificados
    PrintOptions.MaxIdSize
}
var
  L: byte;
begin
  FModified := true;
  Row[i].Name := s;
  L := Length(s)+3;
  if L > PrintOptions.MaxIdSize then PrintOptions.MaxIdSize := L
end;

function TwsMatrix.GetColName(i: Integer): string;
{ Objetivo
    Retorna o nome da coluna de ?ndice especificado
  i: ?ndice da coluna
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Try
    Result := FCName[i-1];
  Except
    Result := '?ndice de coluna incorreto';
  End;
end;

procedure TwsMatrix.SetColName(i: Integer; S: string);
{ Objetivo
    Atribui nome ? coluna especificada
  Par?metros
    i: Coluna para a qual o nome ser? atribu?do
    s: Nome da coluna
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  If (i > 0) and (i <= nCols) Then
     Begin
     S := SysUtilsEx.AllTrim(S);
     If Not SysUtilsEx.IsValidIdent(S) Then S := GetValidId(S);
     If (FCName.Objects[i-1] = Nil) Then FCName[i-1] := S;
     if Length(S) >= PrintOptions.ColWidth then PrintOptions.ColWidth := Length(S)+2;
     FModified := True;
     End
  Else
     Raise Exception.CreateFmt('Classe: %s'#13 +
                               'Propriedade: %s'#13 +
                               'Erro: ?ndice de coluna [%d] inv?lido',
                               [ClassName, 'ColName', i]);
end;

function TwsMatrix.CopyCol(Col: Integer; IMiss: Boolean=False): TwsVec;
{ Objetivo
    Retorna um vetor com os valores da coluna especificada.
  Par?metros
    Col  : Coluna a ser copiada no vetor
    IMiss: True se inclui os valores perdidos na copia; False caso contrario
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
var
  i,j : Integer;
  x   : Double;
  Buf : PFArray;
begin
  // Se pode incluir valores perdidos, copia direto
  if iMiss then
     begin
     Result := TwsDFVec.Create(NRows);
     for i := 1 to NRows do
       Result[i] := Data[i,Col];
     end
  else  // senao, utiliza um buffer
    begin
    GetMem(Buf,sf(NRows));    // cria um buffer com a maior dimensao possivel
    Result:=TwsDFVec.Create(0);
    j:=0;
    for i:=1 to NRows do
      if not IsMissValue(i,Col,x) then
        begin
        Inc(j);
        Buf[j]:=x             // copia no buffer somente os valores validos
        end;
    Result.Append(Buf,j);     // e repassa ao vetor
    FreeMem(Buf,sf(NRows))
    end;
  Result.Name:=FCName[Col-1]
end;

function TwsMatrix.CopyColsToList(Col: TwsLIVec; IMiss: Boolean=False): TList;
{ Objetivo
    Cria uma lista com as colunas especificadas. Por default nao copia valores
    perdidos.
  Par?metros
    Col: Indices das colunas que serao copiadas para a lista
    IMiss: True se inclui os valores perdidos na copia; False caso contrario
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
var
  i: Integer;
begin
  Result := TList.Create;
  for i := 1 to Col.Len do
    Result.Add(CopyCol(Col[i], IMiss))
end;

procedure TwsMatrix.MAdd(L: TwsVec);
{ Objetivo
    Concatena uma linha ? matriz.
  Par?metros
    L: Linha a ser concatenada
  M?todos chamados
    PutLine
  Campos modificados
    PrintOptions.MaxIdSize
    FNRows
}
begin
  FModified := true;
  PutLine(L);
  if Length(L.Name) > PrintOptions.MaxIdSize then PrintOptions.MaxIdSize := Length(L.Name);
  Inc(FNRows);
end;

procedure TwsMatrix.Exchange(i, j: Integer);
{ Objetivo
    Troca os endere?os de duas linhas
  Par?metros
    i, j: Linhas que ter?o seus endere?os trocados
  M?todos chamados
    Nenhum
  Campos modificados
    Endere?os das linhas
}
var
  Row: Pointer;
begin
  FModified := true;
  Row := FList[i-1];
  FList[i-1] := FList[j-1];
  FList[j-1] := Row
end;

procedure TwsMatrix.MInsert(Pos: Integer; L: TwsVec);
{ Objetivo
    Insere uma linha na posi??o especificada
  Par?metros
    Pos: Posi??o que ser? ocupada pela nova linha
    L: Linha que ser? inserida
  M?todos chamados
    Insert
  Campos modificados
    PrintOptions.MaxIdSize
    FNRows
}
begin
  FModified := true;
  FList.Insert(Pos-1, L);
  if Length(L.Name) > PrintOptions.MaxIdSize then PrintOptions.MaxIdSize := Length(L.Name);
  Inc(FNRows)
end;

procedure TwsMatrix.MDelete(L: Integer);
{ Objetivo
    Elimina uma linha especificada
  Par?metros
    L: Linha da matriz que ser? eliminada.
  M?todos chamados
    Free
    Delete
  Campos modificados
    FNRows
}
begin
  FModified := true;
  Row[L].Free;
  FList.Delete(L-1);
  Dec(FNRows)
end;

procedure TwsMatrix.DeleteCol(i: Integer);
var linha: Integer;
Begin
  if i <= FnCols Then
     begin
     FModified := true;
     For Linha := 1 to FnRows do Row[Linha].Delete(i, 1);
     Dec(FnCols);
     end;
End;

Procedure TwsMatrix.MChange(i: Integer; V: TwsVec);
{ Objetivo
    Substituir uma linha por outra
  Par?metros
    i: ?ndice da linha a substituir. Baseado em 1.
    v: Vetor que substituir? a linha
  M?todos chamados
    Free
    Insert
  Campos modificados
    Nenhum
}
Begin
  FModified := True;
  TwsVec(FList[i-1]).Free;
  FList[i-1] := V;
End; { TwsMatrix.MChange }

procedure TwsMatrix.MDeleteCols(Col: TwsLIVec);
{ Objetivo
    Elimina colunas da matriz
  Par?metros
    Col: ?ndices das colunas a eliminar
  M?todos chamados
    MChange
  Campos modificados
    FNCols
  Observa??o
    Elimina somente dos elementos localizados nas linhas. A tarefa de eliminar os atributos
    das colunas fica por conta dos descendentes.
}
var
  i,k,j,
  NewLen: Integer;
  v     : TwsVec;
  inds  : TwsLIVec;
  iOk   : Boolean;
begin
  iOk := True;
  i   := 1;

  // verifica se todos os indices de colunas sao validos
  while iOk and (i<=Col.Len) do
    begin
    iOk:=(Col[i]<=NCols) and (Col[i]>0);
    inc(i)
    end;

  if iOk then
     begin
     NewLen := FNCols-Col.Len;
     inds := TwsLIVec.Create(NewLen);

     // Verifica quais vari?veis v?o permanecer
     k := 0;
     for j := 1 to FNCols do    // percorre todas as colunas
       if not Col.SeqSearch(j, i) then  // se a coluna nao for retirada, copia
         begin
         Inc(k);
         inds[k] := j;
         end;

     // Troca os vetores de cada linha
     for i := 1 to FList.Count do
       begin
       v := TwsDFVec.Create(NewLen);
       v.Name:=Row[i].Name;
       for k := 1 to NewLen do
         v[k] := Data[i, inds[k]];
       MChange(i, v);
       end;
     end;

  inds.Free;
end;  // TwsMatrix.MDeleteCols

procedure TwsMatrix.List(Buffer: TStrings);
{ Objetivo:
    Lista descritores pertinentes ? classe TwsMatrix
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Buffer.Add('Nome:    ' + FName);
  Buffer.Add('Linhas:  ' + IntToStr(FNRows));
  Buffer.Add('Colunas: ' + IntToStr(FNCols));
end;

function TwsMatrix.GetStr(i,j: Integer): String;
{ Objetivo
    Recupera um elemento no formato string
  Par?metros
    i, j: ?ndices de linha e coluna do elemento
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observa??o
    Retorna ?.? se for valor perdido. Aplica a fun??o Fuzz para eliminar d?gitos que est?o
    fora da precis?o especificada.
}
var
  x: Double;
begin
  x:=Get(i,j);
  if not wsGLib.IsMissValue(x) then
    Result:=Format('%*.*g',[PrintOptions.ColWidth, PrintOptions.ColPrecision, Fuzz(x)])
  else
    Result:=Format('%*s',[PrintOptions.ColWidth, '-'])
end;

Procedure TwsMatrix.PutStr(i, j: Integer; X: String);
 { Objetivo
     Atribui valor recebido em formato string ? posi??o especificada
   Par?metros
     i, j: ?ndices de linha e coluna
  M?todos chamados
    Put
  Campos modificados
    Nenhum
  }
Begin
  FModified := True;
  X := SysUtilsEx.AllTrim(X);
  Put(i,j,FracToReal(X))
End; { TwsMatrix.PutStr }

function TwsMatrix.ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer;
{ Objetivo
    Transforma os elementos especificados de uma linha num string
  Par?metros
    L: Linha da matriz
    Start: ?ndice do primeiro valor que ser? colocado no string
    Amt: Quantos valores ser?o colocados no string
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
var
  j: Integer;
  S: string;
  x: Double;
begin
  S := GetRow(L).Name;

  if S = '' then
    S := LeftPad('L'+IntToStr(L), PrintOptions.MaxIDSize)   { Amauri 9/1/98 }
  else
    S := LeftPad(S, PrintOptions.MaxIDSize);

  Amt := Math.Min(Amt,FNCols-Start+1);
  for j := 0 to Amt-1 do
    AppendStr(S,GetStr(L,Start+j));

  if PrintOptions.Center Then
     Buffer.Add(StrCenter(S, 110))
  else
     Buffer.Add(S);

  Result:=Length(S)
end; { TwsMatrix.ToChar }

procedure TwsMatrix.SelectToChar(L, Start, Amt: Integer; Col: TwsLIVec; Buffer: TStrings);
{ Objetivo
    Transforma elementos especificados de uma linha num string na sequ?ncia indicada pelos ?ndices informados num vetor
  Par?metros
    L: Linha da matriz
    Start: ?ndice do primeiro valor que ser? colocado no string
    Amt: Quantos valores ser?o colocados no string
    Col: Vetor com os ?ndices dos elementos a serem colocados no string
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
var
  j: Integer;
  S: string;
begin
  S := Row[L].Name;
  if S = '' then
    S := LeftPad(IntToStr(L), PrintOptions.MaxIDSize)
  else
    S := LeftPad(S, PrintOptions.MaxIDSize);

  for j := Start to Amt do
    AppendStr(S,GetStr(L,Col[j]));

  if PrintOptions.Center Then
     Buffer.Add(StrCenter(S, 110))
  else
     Buffer.Add(S);
end; { TwsMatrix.SelectToChar }

procedure TwsMatrix.Header(Start, Amt: Integer; Buffer: TStrings);
{ Objetivo
    Listar os nomes das colunas especificadas
  Par?metros
    Start: ?ndice da coluna inicial
    Amt: N?mero de colunas a listar
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Var
  j: Integer;
  P: string;
Begin
  If MLab <> '' Then
     if PrintOptions.Center Then
        Buffer.Add(StrCenter(MLab, 110))
     else
        Buffer.Add(MLab);

  P := LeftPad(FName, PrintOptions.MaxIDSize);

  if FCName <> Nil then
    for j := 1 to Amt do
      AppendStr(P, Format('%*s', [PrintOptions.ColWidth, ColName[Start+j]]))
  else
    for j := 1 to Amt do
      AppendStr(P, Format('%*s', [PrintOptions.ColWidth, 'Col'+IntToStr(Start+j)]));

  if PrintOptions.Center Then
     begin
     Buffer.Add(StrCenter(P, 110));
     Buffer.Add(StrCenter(StringOfChar('-',Length(P)), 110))
    end
  else
     begin
     Buffer.Add(P);
     Buffer.Add(StringOfChar('-',Length(P)))
     end
End;

procedure TwsMatrix.SelectHeader(Col: TwsLIVec; Buffer: TStrings);
{ Objetivo
    Listar os nomes das colunas especificadas
  Par?metros
    Col: Vetor com os ?ndices das colunas desejadas
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Var
  j  :Integer;
  P: string;
Begin
  if FName <> '' then
    P := LeftPad(FName, PrintOptions.MaxIDSize)
  else
    P:=LeftPad('SNom', PrintOptions.MaxIDSize);

  for j := 1 to Col.Len do
    AppendStr(P, Format('%*s', [PrintOptions.ColWidth, FCName[Col[j]-1]]));

  if PrintOptions.Center Then
     Buffer.Add(StrCenter(P, 110))
  else
     Buffer.Add(P);
End;

function TwsMatrix.ColsToImp(var k: Integer): Integer;
{ Objetivo
    Retorna o n?mero de colunas que poder?o ser impressas. Considera a largura total e a de cada
    elemento na impress?o
  Par?metros
    k: Na entrada indica a ?ltima coluna j? impressa. Na sa?da corresponde ao ?ndice da ?ltima
       coluna que ser? impressa.
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
var
  Len: Integer;
begin
  Len := PrintOptions.LineLen - PrintOptions.MaxIDSize;
  Result := 0;
  repeat                              { Quantas colunas serao impressas? }
    Inc(k);
    Inc(Result);
    until (k >= FNCols) or (Result*PrintOptions.ColWidth > Len);

  if Len < Result*PrintOptions.ColWidth then
     begin
     Dec(k);
     Dec(Result)
     end
end;

function TwsMatrix.SelectColsToImp(var k: Integer; Col: TwsVec): Integer;
{ Objetivo
    Retorna o n?mero de colunas que poder?o ser impressas, dentre as especificadas por um vetor.
    Considera a largura total e a de cada elemento na impress?o
  Par?metros
    k  : Na entrada indica as colunas j? impressas. Na sa?da corresponde ao ?ndice da ?ltima
         coluna que ser? impressa.
    Col: Vetor que indica quais colunas ser?o impressas
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
var
  Len: Integer;
begin
  Len := PrintOptions.LineLen - PrintOptions.MaxIDSize;
  Result := 0;

  repeat                              { Quantas colunas serao impressas? }
    Inc(k);
    Inc(Result);
  until (k >= Col.Len) or (Result*PrintOptions.ColWidth > Len);

  if Len < Result*PrintOptions.ColWidth then
    begin
    Dec(k);
    Dec(Result)
    end
end;

procedure TwsMatrix.Print(Buffer: TStrings);
{ Objetivo
    Imprime a matriz no formato texto
  M?todos chamados
    ColsToImp
    Header
    ToChar
  Campos modificados
    Nenhum
}
var
  i,j,PrevI,
  NumCol,L  : Integer;
begin
  i := 0;
  { Imprime submatrizes com tamanho PrintOptions.LineLen ou menor }
  Try
    Repeat   { NumCol colunas a partir de i+1 }
    PrevI := i;
    NumCol := ColsToImp(i); // Quantas colunas serao impressas ?

    { Imprime o cabecalho }
    Header(PrevI, NumCol, Buffer);

    // Escreve os dados
    for j := 1 to FNRows do
      L := ToChar(j, PrevI+1, NumCol, Buffer);
{
    if PrintOptions.Center then
       Buffer.Add(StrCenter(StringOfChar('-',L), 110))
    else
       Buffer.Add(StringOfChar('-',L));
}
    Buffer.Add('');
    Until i = FNCols; // Esgota todas as colunas 
  Except
    On E: Exception do
       Buffer.Add(Name + ': ' + E.Message);
  End;
  Buffer.Add('');
end; { TwsMatrix.Print }

procedure TwsMatrix.Copy(MT: TwsEnumMatType;var Matrix:TwsMatrix);
{ Objetivo
    Copia parte da matriz. Descendentes implementam restante da c?pia
  Par?metros
    MT: tipo para a qual ser? copiada a matriz. MT dever? um dos tipos
      mtGeneral
      mtSymmetric
      mtDiagonal
      mtTriangular
      mtToeplitz
      mtVandermonde
    Matrix: local onde retorna a c?pia
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  if CName <> nil then
    begin
    Matrix.CName := TStringList.Create;
    Matrix.CName.Assign(CName)
    end;
  Matrix.Name := Name;
end;

function TwsMatrix.TranspMul1(B: TwsMatrix; var ErrCode: Word): TwsMatrix;
{ Objetivo
    Faz o produto da matriz (geral, sim?trica, mtVandermonde ou mtToeplitz) pela transposta da
    outra sem que a transposi??o seja explicitamente realizada
  Par?metros
    B      : Matriz que ser? transposta para o produto
    ErrCode: C?digo de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contr?rio
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Se A ? a matriz que chama o m?todo, retorna uma matriz geral com o produt AB'
  Valores perdidos
    N?o trata
}
var
  i,j,k:Integer;
  aux  :Extended;
begin
  ErrCode := 0;
  if NCols = B.NCols then
    case B.MatType of
      mtGeneral,mtSymmetric,mtToeplitz,mtVandermonde:
        begin
        Result := TwsGeneral.Create(NRows, B.NRows);
        for i := 1 to NRows do
          begin
          for j := 1 to Result.NCols do
            begin
            aux := 0;
            for k := 1 to NCols do
              aux := aux+Get(i,k)*B[j,k];
            Result[i,j]:=aux;
            end
          end
        end;

      mtDiagonal: Prod2(Self,TwsDiagonal(B),False,TwsGeneral(Result));

      mtTriangular:
        begin
        Result := TwsGeneral.Create(NRows, B.NRows);
        for i := 1 to NRows do
          begin
          for j := 1 to Result.NCols do
            begin
            aux := 0;
            for k := 1 to j do
              aux := aux + Get(i,k)*B[j,k];
            Result[i,j]:=aux;
            end
          end
        end
      end // case
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul1 }

function TwsMatrix.TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix;          { A'B }
{ Objetivo
    Produto de uma matriz qualquer transposta por uma outra sem que a transposi??o seja
    explicitamente realizada
  Par?metros
    B: Matriz com a qual o produto ser? realizado
    ErrCode: C?digo de erro. retorna 0 (zero) se NRows = B.NRows e NImprDim (dimens?es
    impr?prias para a opera??o), caso contr?rio
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Sempre uma matriz geral
  Valores perdidos
    N?o considera
}
var
  i,j,k:Integer;
  aux  : Extended;
  L    : TwsVec;
begin
  if NRows = B.NRows then
    begin
    ErrCode := 0;
    Result := TwsGeneral.Create(NCols, B.NCols);
    case B.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        for i := 1 to NCols do
          for j := 1 to B.NCols do
            begin
            aux := 0;
            for k := 1 to B.NRows do
              aux := aux+Self[k,i]*B[k,j];
            Result[i,j]:=aux;
            end
        end;
      mtDiagonal:
        begin
        L:=B.Row[1];
        for i:=1 to NRows do
          for j:=1 to NCols do
            Result[j,i]:=Self[i,j]*L[i]
        end;
      mtTriangular:
        begin
        for i := 1 to NCols do
          for j := 1 to B.NCols do
            begin
            aux := 0;
            for k := j to B.NRows do
              aux := aux + Self[k, i]*B[k, j];
            Result[i,j]:=aux;
            end
        end;
      end // case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul2 }

function TwsMatrix.TranspMul3: TwsMatrix;
{ Objetivo
    Faz o produto da matriz (geral, sim?trica, mtVandermonde ou mtToeplitz) pela sua transposta
    sem que a transposi??o seja explicitamente realizada
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Retorna a matriz sim?trica AA', exceto se for mtDiagonal. Nesse caso, o retorno ? uma
    matriz mtDiagonal
  Valores perdidos
    N?o trata
}
var
  i,j,k: Integer;
  aux  : Extended;
begin
  Result := TwsSymmetric.Create(NRows);
  for i := 1 to NRows do
    begin
    for j := 1 to i do
      begin
      aux := 0;
      for k := 1 to NCols do
        aux := aux+Self[i,k]*Self[j,k];
      Result[i,j]:=aux;
      end
    end
end; { TranspMul3 }

function TwsMatrix.TranspMul4: TwsMatrix;        { A'A }
{ Objetivo
    Produto de uma matriz transposta por si mesma sem que a transposi??o seja explicitamente
    realizada
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Retorna uma matriz sim?trica exceto no caso em que a matriz ? mtDiagonal
  Valores perdidos
    N?o considera
}
var
  i,j,k: Integer;
  aux  : Extended;
begin
  Result := TwsSymmetric.Create(NCols);
  for i := 1 to NCols do
    for j := 1 to i do
      begin
      aux := 0;
      for k := 1 to NRows do
        aux := aux+Self[k, i]*Self[k, j];
      Result[i,j]:=aux
      end;
end; { TranspMul4 }

function TwsMatrix.TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; //A'BA
{ Objetivo
    Dadas as matrizes A e B (que chama o m?todo), retorna o produto A'BA sem efetuar
    a transposicao explicitamente. Matriz B geral, mtVandermonde ou mtToeplitz.
  Parametros
    A: matriz para o produto
    ErrCode: Codigo de erro. Retorna NImprDim (dimensoes improprias para a operacao)
    se (B.NRows <> B.NCols) ou (A.NRows <> B.NRows)
  Retorno
    Retorna uma matriz geral
}
var
  i,j,k: Integer;
  s    : Extended;
  aux  : TwsVec;
begin
  if (NRows=NCols) and (A.NRows=NRows) then
    begin
    ErrCode := 0;
    Result:= TwsGeneral.Create(A.NCols,A.NCols);
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NCols do
          begin
          { Faz o produto da coluna i de A por cada coluna de B e armazena em aux }
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=1 to A.NRows do
              s:=s+A[k,i]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to A.NCols do
            begin
            s:=0;
            for k := 1 to A.NRows
              do s:=s+aux[k]*A[k,j];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end; // mtGeneral, ...

      mtTriangular:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NCols do
          begin
          { Faz o produto da coluna i de A por cada coluna de B e armazena em aux }
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=i to A.NRows do
              s:=s+A[k,i]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to A.NCols do
            begin
            s:=0;
            for k := j to A.NRows
              do s:=s+aux[k]*A[k,j];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end;

      mtDiagonal:
        begin
        aux:=A.Row[1];
        for i:=1 to NRows do
          for j:=1 to NCols do
            Result[i,j]:=aux[i]*aux[j]*Self[i,j]
        end;
      end; //case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end;
end;

function TwsMatrix.TranspMul6(v: TwsVec; var ErrCode: Word): Double;       { v'Av }
{ Objetivo
    Retorna o produto do vetor transposto pela matriz e novamente o vetor
  Par?metros
    v: Vetor para o produto
    ErrCode: C?digo de erro. Se v.Len=NRows=NCols retorna 0; caso contr?rio retorna
    NImprDim (dimens?es impropr?prias para a opera??o
  Retorno
    Se A ? a matriz que chama retorna o escalar correspondente ao produto v'Av; se o produto
    n?o for poss?vel retorna MissValue
  Valores perdidos
    N?o trata
}
var
  i,j,n: Integer;
  aux  : Extended;
begin
  n := v.Len;
  if ((n=NRows) and (n=NCols)) then
    begin
    Result := 0;
    for j := 1 to n do
      begin
      aux := 0;
      for i := 1 to n do
        aux := aux+v[i]*Self[i,j];
      aux := aux*v[j];
      Result := Result+aux
      end;
    end
  else
    begin
    ErrCode := NImprDim;
    Result := wscMissValue
    end
end; { TranspMul6}

function TwsMatrix.TranspMul7(Lin1, Lin2: Integer): TwsSymmetric;
{ Objetivo
    Obt?m o produto de uma submatriz (transposta) de uma matriz pela pr?pria submatriz
  Par?metros
    Lin1, Lin2: Linha inicial e linha final para obten??o da submatriz
  Retorno
    Matriz sim?trica correspondente ao produto
  Valores perdidos
    N?o trata
}
var
  i,j,k: Integer;
  aux  : Extended;
begin
  Result := TwsSymmetric.Create(NCols);
  for i := 1 to NCols do
    for j := 1 to i do
      begin
      aux := 0;
      for k := Lin1 to Lin2 do
        aux := aux + Self[k,i]*Self[k,j];
      Result[i,j] := aux
      end
end; { TranspMul7 }

function TwsMatrix.TranspMul8(B:TwsMatrix; Start:Integer; var ErrCode:Word): TwsGeneral;
{ Objetivo
    Se A ? a matriz que chama o m?todo, obt?m o produto A'B mas iniciando somente na coluna
    start de B.
  Par?metros
    B: matriz com a qual ser? feito o produto
    Start: Coluna de B por o produto iniciar?.
    ErrCode: C?digo de erro. Retorna 0 se NRows = B.NRows; NImprDim caso contr?rio
    (dimens?es impr?prias para a opera??o)
  Retorno
    Retorna uma matriz geral
  Valores perdidos
    N?o trata
}
var
  k1,i,j,k: Integer;
  aux     : Extended;
begin
  if NRows = B.NRows then
    begin
    ErrCode := 0;
    Dec(Start);
    Result := TwsGeneral.Create(NCols, B.NCols - Start);
    for i := 1 to NCols do
      begin
      for j := 1 to Result.NCols do
        begin
        aux := 0;
        k1 := Start+j;
        for k := 1 to NRows do
          aux := aux+Self[k,i]*B[k,k1];
        Result[i,j]:=aux
        end
      end
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul8 }

// retorna a(i)'*B*c(j)
function TwsMatrix.TranspMul9(A,C: TwsMatrix; var ErrCode: Word): TwsGeneral;
{Objetivo
   Retornar uma matriz com os produtos cruzados das linhas de A' por B e pelas
   colunas de C.
 Par?metros
   A, C: matrizes para o produto
   ErrCode: Codigo de erro. Se (A.NRows<>B.NRows) e (B.NCols<>C.NRows) retorna
     codigo NImprDim (dimensoes incompativeis para operacao
 Retorno
   Sempre uma matriz geral
}
var
  i,j,k: Integer;
  s    : Extended;
  aux  : TwsVec;
begin
  if (A.NRows=NRows) and (NCols=C.NRows) then
    begin
    Result:=TwsGeneral.Create(A.NCols,C.NCols);
    aux:=TwsDFVec.Create(C.NRows);
    for i:=1 to A.NCols do
      begin
      for j:=1 to NCols do
        begin
        s:=0;
        for k:=1 to NRows do
          s:=s+A[k,i]*Self[k,j];
        aux[j]:=s
        end;
      for j:=1 to C.NCols do
        begin
        s:=0;
        for k:=1 to C.NRows do
          s:=s+aux[k]*C[k,j];
        Result[i,j]:=s
        end;
      end; // Para cada linha de A'
    aux.free;
    end // if
  else
    begin
    Result := nil;
    ErrCode:=NImprDim
    end;
end; // TranspMul9

function TwsMatrix.TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; //ABA'
{ Objetivo
    Dadas as matrizes A e B (que chama o m?todo), retorna o produto ABA' sem efetuar
      a transposicao explicitamente. Na chamada B.TranspMul10(A,.) B ? uma matriz geral,
      mtVandermonde ou mtToeplitz.
  Parametros
    A: matriz para o produto
    ErrCode: Codigo de erro. Retorna NImprDim (dimensoes improprias para a operacao)
    se (B.NRows <> A.NCols) ou (A.NRows <> B.NCols)
  Retorno
    Sempre retorna uma matriz geral
}
var
  i,j,k: Integer;
  s    : Extended;
  aux  : TwsVec;
begin
  if (NRows=NCols) and (A.NCols=NRows) then
    begin
    ErrCode := 0;
    Result:= TwsGeneral.Create(A.NRows,A.NRows);
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NRows do   // Para cada linha de A
          begin
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=1 to NRows do
              s:=s+A[i,k]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to A.NRows do
            begin
            s:=0;
            for k := 1 to A.NCols
              do s:=s+aux[k]*A[j,k];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end; // mtGeneral, ...

      mtTriangular:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NRows do  // Para cada linha de A
          begin
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:= 1 to i do
              s:=s+A[i,k]*Self[k,j];
            aux[j]:=s;
            end;
          for j:= 1 to A.NRows do // Para cada linha de A (ou coluna de A')
            begin
            s:=0;
            for k := 1 to j do
              s:=s+aux[k]*A[j,k];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end;
      mtDiagonal:
        begin
        aux:=A.Row[1];
        for i:=1 to A.NRows do
          for j:=1 to NCols do
            Result[i,j]:=aux[i]*aux[j]*Self[i,j]
        end;
      end; //case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end;
end;

function TwsMatrix.TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double;
{ Objetivo
    Retorna o produto da coluna k da matriz A pela matriz na forma B[k]'*A*B[k], onde B[k]
    ? a coluna.
  Par?metros
    B: Matriz para o produto
    k: ?ndice da coluna para o produto
    ErrCode: C?digo de erro. Se NRows=NCols=A.NRows retorna 0; caso contr?rio retorna
      NImprDim (dimens?es impropr?prias para a opera??o
  Retorno
    Se o produto n?o for poss?vel, retorna wscMissValue
  Valores perdidos
    N?o trata
}
var
  i,j: Integer;
  aux: Extended;
begin
  if ((B.NRows=NRows) and (NRows=NCols)) then
    begin
    Result := 0;
    case B.MatType of
      mtGeneral, mtSymmetric, mtToeplitz, mtVandermonde:
        for j := 1 to B.NRows do              // para cada coluna da matriz
          begin
          aux := 0;
          for i := 1 to B.NRows do
            aux := aux+B[i,k]*Self[i, j];
          Result := Result + aux*B[j,k];      // Completa o produto
          end;
      mtTriangular:
        for j := k to B.NRows do              // para cada coluna da matriz
          begin
          aux := 0;
          for i := k to B.NRows do
            aux := aux + B[i,k]*Self[i, j];
          Result := Result + aux*B[j,k];      // Completa o produto
          end;
      mtDiagonal:
        Result := Sqr(B[k,k])*Self[k,k];
      end; // case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := wscMissValue
    end
end; { TranspMul11 }


function TwsMatrix.SubMatrix(l,mb,k,nb:Integer):TwsGeneral;
{ Objetivo
    Gera uma submatriz a partir de ?ndices especificados
  Par?metros:
    l : Linha inicial para submatriz
    mb: Linha final
    k : Coluna inicial para submatriz
    nb: Coluna final
  Retorno
    Sempre uma matriz geral
 }
var
  i, j: Integer;
begin
  Dec(l); Dec(k);
  mb := Math.Min(nRows-l, mb);
  nb := Math.Min(nCols-k, nb);
  Result := TwsGeneral.Create(mb, nb);
  if CName <> nil then
     begin
     for i := 0 to nb-1 do
       Result.ColName[i+1]:= ColName[k+i+1] {Rochedo}
     end;
  for i := 1 to mb do
    begin
    Result.RowName[i]:=RowName[l+i];
    for j := 1 to nb do
      Result[i,j] := Self[l+i,k+j]
    end
end; { de SubMatrix }

function TwsMatrix.SubSymMat(l,mb:Integer): TwsSymmetric;
{ Objetivo
    Obtem uma submatriz sim?trica
  Par?metros
    l: Linha inicial para submatriz
    mb: Ordem da submatriz
  Retorno
    Sempre retorna uma matriz sim?trica
}
var
  i,j: Integer;
begin
  Dec(l);
  mb := Math.Min(NRows-l, mb);
  Result := TwsSymmetric.Create(mb);
  if CName <> nil then
    for i := 1 to mb do
      begin
      Result.ColName[i] := ColName[l+i];
      Result.RowName[i] := ColName[l+i]
      end;
  for i := 1 to mb do
    for j := 1 to i do
      Result[i,j] := Self[l+i,l+j];
end; { de SubSymMat }

function TwsMatrix.SubIndex(R, C: TwsLIVec): TwsGeneral;
{ Objetivo
    Obtem uma submatriz com os elementos especificados atrav?s dos vetores R e C
  Par?metros
    R: ?ndices das linhas dos elementos
    C: ?ndices das colunas
  Retorno
    Retorna uma matriz geral de dimens?o R.Len e C.Len. Os elementos da submatriz ser?o os
    elementos que est?o nas posi??es (R[i],C[j])
}
var
  i,j: Integer;
begin
  Result := nil;
  {Verifica se n?o h? valores fora da faixa.}
  for i := 1 to R.Len do
    if (R[i] < 1) or (R[i] > NRows) then
       Exit;
  for j := 1 to C.Len do
    if (C[J] < 1) or (C[J] > NCols) then
      Exit;
  Result := TwsGeneral.Create(R.Len, C.Len);
  if CName <> nil then
    for i := 1 to C.Len do
      Result.ColName[i] := ColName[C[i]];
  for i := 1 to R.Len do
    for j := 1 to C.Len do
      Result[i,j] := Self[R[i],C[j]]
end; { SubIndex }

function TwsMatrix.RowReduc(C: TwsLIVec; Ch: TwsEnumTypeOp): TwsGeneral;
{  Objetivo
     Reduz as linhas a uma de acordo com operadores especificados para determinadas colunas
   Par?metros
     C: ?ndices das coluna de interesse
     Ch:Operador para redu??o das linhas. Os operadores considerados s?o:
       opSum : Soma os elementos
       opSSq : Soma de quadrados dos elementos
       opGT  : Maior valor
       opLT  : Menor valor
       opMean: M?dia
       opQ1  : Primeiro quartil
       opQ2  : Segundo quartil
       opQ3  : Terceiro quartil
    Retorno
      Matriz geral com uma linha. Por exemplo, se A possui 5 colunas, a chamada
      B:=A.RowReduc([2,3,4],opSum) obtem uma matriz de uma linha e tr?s colunas com o total
      da coluna 2 na primeira posi??o, total da 3na segunda e assim por diante.
}
var
  i,j     : Integer;
  Col     : TwsVec;
  aux,aux1: Double;
begin
  Result := TwsGeneral.Create(1, C.Len);
  if CName <> nil then
    for i:=1 to C.Len do
      Result.ColName[i]:=ColName[C[i]];
  case Ch of
    opSum: for j := 1 to C.Len do          // Soma
             begin
             aux:=0;
             for i:=1 to NRows do
               aux:=aux+Self[i,C[j]];
             Result[1,j]:=aux;
             end;
    opSSq: for j := 1 to C.Len do          // Soma de quadrados
             begin
             aux:=0;
             for i:=1 to NRows do
               aux:=aux+Sqr(Self[i,C[j]]);
             Result[1,j]:=aux;
             end;
    opGT: for j := 1 to C.Len do           // Maximo
            begin
            aux:=Self[1,C[j]];
            for i:=2 to NRows do
              if aux>Self[i,C[j]] then
                aux:=Self[i,C[j]];
            end;
    opLT: for j := 1 to C.Len do           // Minimo
            begin
            aux:=Self[1,C[j]];
            for i:=2 to NRows do
              if aux<Self[i,C[j]] then
                aux:=Self[i,C[j]];
            end;
    opMean: for j := 1 to C.Len do         // Media
              begin
              aux:=0;
              for i:=1 to NRows do
                begin
                aux1:=Self[i,C[j]];
                aux1 := aux1 - aux;
                aux := aux + aux1/i
                end;
              end;
    opQ1,opQ2,opQ3:                         // 1o., 2o. ou 3o quartil
      for i := 1 to C.Len do
        begin
        Col := CopyCol(C[i]);
        Col.QuickSort(True);
        if Ch = opQ1 then
          aux:=Col.Len*0.25
        else
          if Ch = opQ2 then
            aux:=Col.Len*0.5
          else
            aux:=Col.Len*0.75;
        j:=Trunc(aux);
        aux:=Frac(aux);
        if aux<1e-9 then
          Result[1,i] := 0.5*(Col[j]+Col[j+1])
        else
          Result[1,i] := Col[j+1];
        Col.Free
        end;
  end { case }
end; { RowReduc }

function TwsMatrix.ColReduc(R: TwsLIVec; Ch: TwsEnumTypeOp): TwsGeneral;
{  Objetivo
     Reduz as colunas a uma de acordo com operadores especificados para determinadas linhas
   Par?metros
     C: ?ndices das linhas de interesse
     Ch:Operador para redu??o das colunas. Os operadores considerados s?o:
       opSum : Soma os elementos
       opSSq : Soma de quadrados dos elementos
       opGT  : Maior valor
       opLT  : Menor valor
       opMean: M?dia
       opQ1  : Primeiro quartil
       opQ2  : Segundo quartil
       opQ3  : Terceiro quartil
    Retorno
      Matriz geral com uma coluna. Por exemplo, se A possui 5 linhas, a chamada
      B:=A.ColReduc([2,3,4],opSum) obt?m uma matriz de uma coluna e tr?s linhas com o total
      da linha 2 na primeira posi??o, total da 3 na segunda e assim por diante.
}
var
  L: TwsVec;
  i,j: Integer;
  aux: Double;
begin
  Result := TwsGeneral.Create(1,R.Len);
  case Ch of
    opSum:
      for i := 1 to R.Len do                     // Soma
        Result[1,i] := Row[R[i]].Total(j);
    opSSq:
      for i := 1 to R.Len do                     // Soma de quadrados
        Result[1,i] := Row[R[i]].SumOfSq(j);
    opGT:
      for i := 1 to R.Len do                     // Maximo
        Result[1,i] := Row[R[i]].MinOrMax(False);
    opLT:
      for i := 1 to R.Len do                     // Minimo
        Result[1,i] := Row[R[i]].MinOrMax(True);
    opMean:
      for i := 1 to R.Len do                     // Media
        Result[1,i] := Row[R[i]].Mean(j);
    opQ1, opQ2, opQ3:                            // 1o., 2o. ou 3o quartil
       for i:=1 to R.Len do
         begin
         L := Row[R[i]].Copy(1, NCols);
         L.QuickSort(True);
          if Ch = opQ1 then
            aux:=L.Len*0.25
          else
            if Ch = opQ2 then
              aux:=L.Len*0.5
            else
              aux:=L.Len*0.75;
          j:=Trunc(aux);
          aux:=Frac(aux);
          if aux<1e-9 then
            Result[1,i] := 0.5*(L[j]+L[j+1])
          else
            Result[1,i] := L[j+1];
         L.Free
         end;
  end; { case }
end; { ColReduc }

function TwsMatrix.RowColReduc(ChRow, ChCol: TwsEnumTypeOp): Double;
{ Objetivo
    Reduz a matriz a um valor segundo os operadores definidos
  Par?metros
    ChRow: Operador de redu??o das linhas
    ChCol: Operador de redu??o das colunas
  Observa??es
    Inicialmente ? aplicado o operador de redu??o das linhas e sobre o resultado ? aplicado
    o redutor de colunas. Os operadores s?o:
      opSum : Total
      opSSq :  Soma de quadrados
      opGT : M?ximo
      opLE : M?nimo
      opMean : M?dia
      opQ1 , opQ2, opQ3 :  1o., 2o. ou 3o quartil.
  Exemplo
    A instru??o x:=A.RowColReduc(opGT,opMean) ir? produzir a m?dia dos m?ximos das colunas
    da matriz A
}
var
  j   : Integer;
  L   : TwsVec;
  Col : TwsLIVec;
  M   :TwsGeneral;
  aux : Double;
begin
  Col := Index(1,NCols);
  M := TwsGeneral(RowReduc(Col,ChRow));
  L:=M.Row[1];
  case ChCol of
    opSum: Result := L.Total(j);
    opSSq: Result := L.SumOfSq(j);
    opGT:  Result := L.MinOrMax(False);
    opLT:  Result := L.MinOrMax(True);
    opMean:Result := L.Mean(j);
    opQ1, opQ2, opQ3:                            { 1o., 2o. ou 3o quartil }
       begin
       L.QuickSort(True);
        if ChCol = opQ1 then
          aux:=L.Len*0.25
        else
          if ChCol = opQ2 then
            aux:=L.Len*0.5
          else
            aux:=L.Len*0.75;
        j:=Trunc(aux);
        aux:=Frac(aux);
        if aux<1e-9 then
          Result := 0.5*(L[j]+L[j+1])
        else
          Result := L[j+1]
       end;
  end; { case }
  M.Free;
  Col.Free
end; { RowColReduc }

function TwsMatrix.DescStat(Col,Stat: TwsLIVec): TwsGeneral;
{ Objetivo:
    Obtem estatisticas descritivas para as colunas especificadas.
  Par?metros
    Col: ?ndices das colunas para as quais ser?o calculadas as estat?sticas
    Stat: Estat?sticas desejadas. Os valores em Stat e as correspondentes estatisticas sao:
      0.  Media
      1.  Variancia
      2.  Desvio padrao
      3.  Total
      4.  Minimo
      5.  Maximo
      6.  Numero de valores
      7.  Numero de valores validos
      8.  Erro padrao da media
      9.  Amplitude
      10. Coeficiente de variacao
      11. Assimetria
      12. Curtose
      13. Soma de quadrados nao corrigida
      14. Soma de quadrados corrigida
  Retorno
    Matriz com as estat?sticas desejadas nas colunas e as colunas nas linhas
  Exemplo
    S := A.DescStat([0,1],[1,2,3]) gerar? uma matriz de duas colunas e tr?s linhas onde na
    primeira coluna est?o as medias e na segunda coluna as vari?ncias das colunas 1, 2 e 3
  Valores perdidos
    S?o excluidos dos calculos
}
var
  i, j: Integer;
  w0, x, v: Double;
  m: array[1..8] of PFArray;
  {Valores basicos armazenados em M:
   1: media
   2: 2o. momento
   3: 3o. momento
   4: 4o. momento
   5: Num. observ. validas
   6: Soma quadrados nao corrigida
   7: Minimo
   8: Maximo
  }
begin
  { Em cada linha uma quantidade e em cada coluna uma variavel }
  for i := 1 to 8 do
    begin
    GetMem(M[i],sf(Col.Len));
    for j := 1 to Col.Len do M[i]^[j] := 0;
    end;
  for j := 1 to Col.Len do
    begin
    M[7]^[j] := -MinFloatValue;
    M[8]^[j] := MinFloatValue
    end;
  for i := 1 to NRows do                     // para cada linha
    for j := 1 to Col.Len do                 // para cada variavel
      if not IsMissValue(i,Col[j],x) then    // Se nao e valor perdido
        begin
        M[5]^[j]:=M[5]^[j]+1;                // Numero de observacoes v?lidas
        w0 := M[5]^[j]-1;
        v := (x-M[1]^[j])/M[5]^[j];
        M[4]^[j]:=M[4]^[j]-4*v*M[3]^[j]+6*v*v*M[2]^[j]         // 4o momento
          + (M[5]^[j]*M[5]^[j]-3*w0)*Power(v,4)*M[5]^[j]*w0;
                                                               // 3o momento
        M[3]^[j]:=M[3]^[j]-3*v*M[2]^[j]+(M[5]^[j]-2)*Power(v,3)*M[5]^[j]*w0;
        M[2]^[j] := M[2]^[j] + v*v*M[5]^[j]*w0;                   { 2o momento }
        M[1]^[j] := M[1]^[j] + v;                                      { M?dia }
        M[6]^[j] := M[6]^[j]+x*x;             { Soma de quadrados nao ajustada }
        if M[7]^[j] > x then M[7]^[j] := x;                     { Valor minino }
        if M[8]^[j] < x then M[8]^[j] := x                      { Valor maximo }
        end;
  Result := TwsGeneral.Create(Col.Len,Stat.Len);   // Matriz que armazenara os resultados
  Result.MLab := 'Estat?sticas descritivas';
  Result.Name := 'Variaveis';
  for j := 1 to Stat.Len do
    Result.ColName[j] := cShortNameEstat[Stat[j]];
  for j := 1 to Col.Len do
    begin
    Result.RowName[j] := ColName[Col[j]];
    for i := 1 to Stat.Len do
      begin
      v := ScalarDiv(M[2]^[j],M[5]^[j]-1);
      x := ScalarSqrt(v);
      case Stat[i] of
        0:  Result[j,i] := M[1]^[j];                                         {Media}
        1:  Result[j,i] := v;                                            {Variancia}
        2:  Result[j,i] := x;                                        {Desvio padrao}
        3:  Result[j,i] := M[1]^[j]*M[5]^[j];                                {Total}
        4:  Result[j,i] := M[7]^[j];                                        {Minimo}
        5:  Result[j,i] := M[8]^[j];                                        {Maximo}
        6:  Result[j,i] := NRows;                                {Numero de valores}
        7:  Result[j,i] := M[5]^[j];                     {Numero de valores validos}
        8:  Result[j,i] := ScalarSqrt(ScalarDiv(v,M[5]^[j])); {Erro padrao da media}
        9: Result[j,i] := M[8]^[j]-M[7]^[j];                             {Amplitude}
        10: Result[j,i] := ScalarProd(100,ScalarDiv(x,M[1]^[j]));{Coeficiente de variacao}
                                                                        {Assimetria}
        11: Result[j,i] := ScalarDiv(M[5]^[j]*M[3]^[j],(M[5]^[j]-1)*(M[5]^[j]-2)*v*x);
                                                                           {Curtose}
        12: Result[j,i]:=ScalarDiv(m[5]^[j]*(m[5]^[j]+1)*m[4]^[j]-3*m[2]^[j]*m[2]^[j]*(m[5]^[j]-1),
              (m[5]^[j]-1)*(m[5]^[j]-2)*(m[5]^[j]-3)*v*v);
        13: Result[j,i]:=M[6]^[j];                 {Soma de quadrados nao corrigida}
        14: Result[j,i]:=M[2]^[j];                     {Soma de quadrados corrigida}
        15: Result[j,i]:=M[5]^[j];                                  {Soma dos pesos}
        end; {case}
      end; { For }
    end;
  for i := 1 to 8 do
    FreeMem(M[i], sf(Col.Len));
end; { DescStat }

function TwsMatrix.wDescStat(Col, Stat: TwsLIVec; WInd: Integer): TwsGeneral;
{ Objetivo:
    Obtem estatisticas descritivas para as colunas especificadas ponderadas.
  Par?metros
    Col: ?ndices das colunas para as quais ser?o calculadas as estat?sticas
    Stat: Estat?sticas desejadas. Os valores em Stat e as correspondentes estatisticas sao:
      0.  Media
      1.  Variancia
      2.  Desvio padrao
      3.  Total
      4.  Minimo
      5.  Maximo
      6.  Numero de valores
      7.  Numero de valores validos
      8.  Erro padrao da media
      9.  Amplitude
      10. Coeficiente de variacao
      11. Assimetria
      12. Curtose
      13. Soma de quadrados nao corrigida
      14. Soma de quadrados corrigida
      15. Soma dos pesos
  Retorno
    Matriz com as estat?sticas ponderadas desejadas nas colunas e as colunas nas linhas
  Exemplo
    S := A.WDescStat([0,1],[1,2,3],5) gerar? uma matriz de duas colunas e tr?s linhas onde
    na primeira coluna est?o as medias e na segunda coluna as vari?ncias das colunas 1, 2
    e 3. Utiliza os valores da coluna 5 para a ponderacao. Somente observa??es com pesos
    estritamente positivos ? que s?o utilizadas.
  Valores perdidos
    S?o excluidos dos calculos
}
var
  i, j: Integer;
  wx, v, v2, w0, w1, wx2, x: Double;
  m: array[1..8] of PFArray;
  { Valores basicos armazenados em M:
    1: media
    2: 2o. momento
    3: 3o. momento
    4: 4o. momento
    5: Num. observ. validas
    6: Soma de quadrados nao corrigida
    7: Minimo
    8: Maximo
  }
begin
  for i := 1 to 8 do
    begin                 { A matriz M guardara as quantidades basicas }
    GetMem(M[i],sf(Col.Len));
    for j := 1 to Col.Len do M[i]^[j] := 0;
    end;
  for j := 1 to Col.Len do
    begin
    M[7]^[j] := -MinFloatValue;
    M[8]^[j] := MinFloatValue
    end;

  w1 := 0;

  for i := 1 to NRows do                                          { Para cada observacao }
    if (not IsMissValue(i,WInd,wx)) then
      if (wx>0) then         { wx e o peso. Nao pode ser valor perdido, negativo ou nulo }
        begin
        wx2:=wx*wx;
        w0 := w1;                                  { w0 acumula pesos ate o passo anterior }
        w1 := w1+wx;                                { w1 acumula pesos ate o passo atual }
        for j := 1 to Col.Len do
          if not IsMissValue(i,Col[j],x) then                { Pula se x e valor perdido }
            begin
            M[5]^[j]:=M[5]^[j]+1;                        { Numero de observacoes validas }
            v := (wx/w1)*(x-M[1]^[j]);
            v2:=v*v;
            M[4]^[j]:=M[4]^[j]-4*v*M[3]^[j]+6*v2*M[2]^[j]+                    { 4o momento }
              ((w1*w1-3*wx*w0)/(wx2*wx))*v2*v2*w0*w1;
            M[3]^[j]:=M[3]^[j]-3*v*M[2]^[j]+(w1*w0/wx2)*(w1-2*wx)*v2*v;       { 3o momento }
            M[2]^[j] := M[2]^[j] + (w1*w0/wx)*v2;                             { 2o momento }
            M[1]^[j] := M[1]^[j] + v;                                              { M?dia }
            M[6]^[j] := M[6]^[j]+wx*x*x;                  { Soma de quadrados nao ajustada }
            if M[7]^[j] > x then M[7]^[j] := x;                             { Valor minino }
            if M[8]^[j] < x then M[8]^[j] := x                              { Valor maximo }
            end { if x <> wscMissValue }
        end; { if (wx>0) }

  Result := TwsGeneral.Create(Col.Len,Stat.Len);
  Result.MLab := 'Estat?sticas descritivas ponderadas';
  Result.Name := 'Estat';
  for j := 1 to Stat.Len do
    Result.ColName[j] := cShortNameEstat[Stat[j]];

  { Obtem as estat?sticas a partir dos momentos }
  for i := 1 to Col.Len do
    begin                        { Para cada variavel }
    Result.RowName[i] := ColName[Col[i]];
    for j := 1 to Stat.Len do
      begin                     { Obtem as estatisticas desejadas}
      v := ScalarDiv(m[2]^[i],w1-1);
      x := ScalarSqrt(v);
      case Stat[j] of
         0: Result[i,j] := m[1]^[i];                     { Media }
         1: Result[i,j] := v;                            { Variancia }
         2: Result[i,j] := x;                            { Desvio padrao }
         3: Result[i,j] := w1*m[1]^[i];                  { Total }
         4: Result[i,j] := m[7]^[i];                     { Minimo}
         5: Result[i,j] := m[8]^[i];                     { Maximo}
         6: Result[i,j] := NRows;                        { Numero de observacoes }
         7: Result[i,j] := m[5]^[i];                     { Num. observacoes validas }
         8: Result[i,j] := ScalarSqrt(ScalarDiv(v,w1));  { Erro padrao da media }
         9: Result[i,j] := m[8]^[i]-m[7]^[i];            { Amplitude total }
                                                         { Coeficiente de variacao }
        10: Result[i,j] := ScalarProd(100,ScalarDiv(x,m[1]^[i]));
                                                         { Coef. assimetria }
        11: Result[i,j] := ScalarDiv(w1*M[3]^[i],(w1-1)*(w1-2)*v*x);
                                                         { Coeficiente de curtose }
        12: Result[i,j] := ScalarDiv(w1*(w1+1)*m[4]^[i]-3*m[2]^[i]*m[2]^[i]*(w1-1),
                           (w1-1)*(w1-2)*(w1-3)*v*v);
        13: Result[i,j] := m[6]^[i];                     { Soma de quadados nao corrigida }
        14: Result[i,j] := m[2]^[i];                     { Soma de quadrados de desvios }
        15: Result[i,j] := w1;                           { Soma dos pesos }
        end; { case }
      end; // for j
    end;
  for i := 1 to 8 do FreeMem(M[i], sf(Col.Len));
end; { WDescStats }

function Exist(v: TwsVec; const x: Double; Len: Integer): Boolean;
{  Objetivo
     Funcao auxiliar para obtencao de vetores de estatisticas. Verifica se o valor x esta em v,
     iniciando a procura de 1 ate Len. Serve para selecionar os indices distintos de Col.
}
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Len do
    if v[i]=x then
      begin
      Result := True;
      Break
      end;
end;

function WhichCol(k: Integer; Lin: TwsLIVec): Integer;
{ Objetivo
    Auxiliar para obten??o do vetor de estat?sticasDetermina o indice de k em Lin. Retorna -1
    se k n?o est? em Lin.
}
var
  i: Integer;
begin
  Result:=-1;
  for i := 1 to Lin.Len do
    if Lin[i] = k then
      begin
      Result := i;
      Break
      end
end;


function TwsMatrix.vDescStat(Col, Stat: TwsLIVec): TwsVec;
{ Objetivo
    Retorna num vetor valores de estatisticas para colunas desejadas
  Par?metros
    Col: ?ndices das colunas para a obten??o das estat?sticas
    Stat: Estat?sticas desejadas. A codifica??o ser?:
      0.  Media
      1.  Variancia
      2.  Desvio padrao
      3.  Total
      4.  Minimo
      5.  Maximo
      6.  Numero de valores
      7.  Numero de valores validos
      8.  Erro padrao da media
      9.  Amplitude
      10. Coeficiente de variacao
      11. Assimetria
      12. Curtose
      13. Soma de quadrados nao corrigida
      14. Soma de quadrados corrigida
  Exemplo
    Se mais de uma estat?stica for desejada para a mesma coluna, essa coluna dever? ser
    repetida juntamente com a estat?stica. Assim, para a instru??o
      v:=A.VDescStat([1,1,1,3],[0,1,2,0])
    se tera o retorno
      v[1],v[2],v[3]: M?dia, vari?ncia e desvio padr?o da primeira coluna e
      v[4]: M?dia da terceira coluna
    Observe-se que, para cada ?ndice de coluna especificada em Col dever? haver uma
    estat?stica correspondente em Stat.
  Valores perdidos
    Excluidos dos calculos. Se todos os valores s?o perdidos, retorna valores perdidos
      em todas as posi??es 
}
var
  i,j,k,nvar: Integer;
  w0,x,v    : Double;
  L1        : TwsLIVec;
  m         : array[1..8] of PFArray;
  OneValObs : Boolean;
  {Valores basicos armazenados em M:
   1: media
   2: 2o. momento
   3: 3o. momento
   4: 4o. momento
   5: Num. observ. validas
   6: Soma quadrados nao corrigida
   7: Minimo
   8: Maximo
  }
begin
  L1 := TwsLIVec.Create(Col.Len);
  L1[1] := Col[1];
  nvar := 2;
  for i := 2 to Col.Len do // L1 armazenara os indices distintos de Col
    if not Exist(L1, Col[i], nvar-1) then
      begin
      L1[nvar] := Col[i];
      Inc(nvar)
      end;

  Dec(nvar);  // nvar eh o numero de colunas distintas presentes em Col
  for i := 1 to 8 do
    begin     // A matriz M guardar? as quantidades b?sicas
    GetMem(M[i],sf(nvar));
    for j := 1 to nvar do M[i]^[j] := 0;
    end;
  for j := 1 to nvar do
    begin
    M[7]^[j] := -MinFloatValue;
    M[8]^[j] :=  MinFloatValue
    end;

  OneValObs:=False;                                 // Pelo menos 1 observacao
  for i := 1 to NRows do
    for j := 1 to nvar do                           // Para cada coluna
      if not IsMissValue(i,L1[j],x) then            // Pula se x e valor perdido
        begin
        OneValObs:=True;
        M[5]^[j] := M[5]^[j] + 1;                   // Numero de observacoes
        w0 := M[5]^[j] - 1;
        v := (x - M[1]^[j]) / M[5]^[j];
                                                    // 4o momento
        M[4]^[j] := M[4]^[j]-4*v*M[3]^[j]+6*v*v*M[2]^[j]+(M[5]^[j]*M[5]^[j]-3*w0)
          *Power(v,4)*M[5]^[j]*w0;
                                                    // 3o momento
        M[3]^[j] := M[3]^[j]-3*v*M[2]^[j]+(M[5]^[j]-2)*Power(v,3)*M[5]^[j]*w0;
        M[2]^[j] := M[2]^[j] + v * v * M[5]^[j] * w0;// 2o momento
        M[1]^[j] := M[1]^[j] + v;                   // M?dia
        M[6]^[j] := M[6]^[j] + x * x;               // Soma de quadrados nao ajustada
        if M[7]^[j] > x then M[7]^[j] := x;         // Valor minino
        if M[8]^[j] < x then M[8]^[j] := x          // Valor maximo
        end;
  if OneValObs then
    begin
    Result := TwsDFVec.Create(Stat.Len);     // Vetor que armazenara os resultados
    for j := 1 to Stat.Len do
      begin
      k := WhichCol(Col[j],L1);  // Indice Col[j] corresponde a que indice em L1 ?
      v := ScalarDiv(M[2]^[k],M[5]^[k]-1);
      x := ScalarSqrt(v);
      case Stat[j] of
        0:  Result[j] := M[1]^[k];                   // Media
        1:  Result[j] := v;                          // Variancia
        2:  Result[j] := x;                          // Desvio padrao
        3:  Result[j] := M[1]^[k]*M[5]^[k];          // Total
        4:  Result[j] := M[7]^[k];                   // Minimo
        5:  Result[j] := M[8]^[k];                   // Maximo
        6:  Result[j] := NRows;                      // Numero de valores
        7:  Result[j] := M[5]^[k];                   // Numero de valores validos
                                                     // Erro padrao da media
        8:  Result[j] := ScalarSqrt(ScalarDiv(v,M[5]^[k]));
        9:  Result[j] := M[8]^[k]-M[7]^[k];          // Amplitude
        10: Result[j] := ScalarDiv(x,M[1]^[k]);      // Coeficiente de variacao
                                                     // Assimetria
        11: Result[j] := ScalarDiv(M[5]^[k]*M[3]^[k],(M[5]^[k]-1)*(M[5]^[k]-2)*v*x);
                                                     // Curtose
        12: Result[j] := ScalarDiv(m[5]^[k]*(m[5]^[k]+1)*m[4]^[k]-3*m[2]^[k]*m[2]^[k]*
                         (m[5]^[k]-1), (m[5]^[k]-1)*(m[5]^[k]-2)*(m[5]^[k]-3)*v*v);

        13: Result[j] := M[6]^[k];                   // Soma de quadrados nao corrigida
        14: Result[j] := M[2]^[k];                   // Soma de quadrados corrigida
        end; {case}
      end {For}
    end
  else
    Result:=VecConst(wscMissValue,Stat.Len);
  L1.Free;
  for i := 1 to 8 do
    FreeMem(M[i], sf(nvar));
end; { VDescStat }


function TwsMatrix.vwDescStat(Col, Stat: TwsLIVec; WInd: Integer): TwsVec;
{ Objetivo
    Retorna num vetor valores de estatisticas ponderadas para colunas desejadas
  Par?metros
    Col: ?ndices das colunas para a obten??o das estat?sticas
    Stat: Estat?sticas desejadas. A codifica??o ser?:
      0.  Media
      1.  Variancia
      2.  Desvio padrao
      3.  Total
      4.  Minimo
      5.  Maximo
      6.  Numero de valores
      7.  Numero de valores validos
      8.  Erro padrao da media
      9.  Amplitude
      10. Coeficiente de variacao
      11. Assimetria
      12. Curtose
      13. Soma de quadrados nao corrigida
      14. Soma de quadrados corrigida
    WInd: ?ndice da vari?vel peso
  Exemplo
    Se mais de uma estat?stica for desejada para a mesma coluna, essa coluna dever? ser
    repetida juntamente com a estat?stica. Assim, para a instru??o
      v:=A.WVDescStat([1,1,1,3],[0,1,2,0],5)
    se tera o retorno
      v[1],v[2],v[3]: M?dia, vari?ncia e desvio padr?o da primeira coluna e
      v[4]: M?dia da terceira coluna
    Todos os c?lculos s?o ponderados pelos valores da 5 coluna
    Observe-se que, para cada ?ndice de coluna especificada em Col dever? haver uma
    estat?stica correspondente em Stat.
    Somente ser?o inclu?das as linhas para as quais o valor da vari?vel peso ? estritamente
    positivo
  Valores perdidos (incluindo a vari?vel peso)
    Excluidos dos calculos
}
var
  i,j,k,nvar: Integer;
  wx,w0,x,v,
  w1,v2,wx2 : Double;
  L1        : TwsLIVec;
  m         : array[1..8] of PFArray;
  OneValObs : Boolean;
  {Valores basicos armazenados em M:
   1: media
   2: 2o. momento
   3: 3o. momento
   4: 4o. momento
   5: Num. observ. validas
   6: Soma quadrados nao corrigida
   7: Minimo
   8: Maximo
  }
begin
  Result := nil;
  L1 := TwsLIVec.Create(Col.Len);
  L1[1] := Col[1];
  nvar := 2;
  for i := 2 to Col.Len do
    begin        { L1 armazenara os indices nao repetidos de Col }
    if not Exist(L1,Col[i],nvar-1) then
      begin
      L1[nvar] := Col[i];
      Inc(nvar)
      end;
    end;

  Dec(nvar);                  { nvar e o numero de variaveis distintas presentes em Col }
  for i := 1 to 8 do
    begin                 { A matriz M guardara as quantidades basicas }
    GetMem(M[i],sf(nvar));
    for j := 1 to nvar do M[i]^[j] := 0;
    end;

  for j := 1 to nvar do
    begin
    M[7]^[j] := -MinFloatValue;
    M[8]^[j] := MinFloatValue
    end;

  w1:=0;
  OneValObs:=False;
  for i := 1 to NRows do
    if not IsMissValue(i,WInd,wx) then
      if wx>0 then
        begin
        wx2:=wx*wx;
        w0 := w1;                               // w0 acumula pesos ate o passo anterior
        w1 := w1+wx;                            // w1 acumula pesos ate o passo atual
        for j := 1 to nvar do
          if not IsMissValue(i,L1[j],x) then
            begin                               // Pula se x e valor perdido
            OneValObs:=True;
            M[5]^[j]:=M[5]^[j]+1;               // Numero de observacoes validas
            v := (wx/w1)*(x-M[1]^[j]);
            v2:=v*v;
                                                // 4o momento
            M[4]^[j]:=M[4]^[j]-4*v*M[3]^[j]+6*v2*M[2]^[j]+((w1*w1-3*wx*w0)/(wx2*wx))*v2*v2*w0*w1;
                                                // 3o momento
            M[3]^[j]:=M[3]^[j]-3*v*M[2]^[j]+(w1*w0/wx2)*(w1-2*wx)*v2*v;
                                                // 2o momento
            M[2]^[j] := M[2]^[j] + (w1*w0/wx)*v2;
            M[1]^[j] := M[1]^[j] + v;           // M?dia
            M[6]^[j] := M[6]^[j]+wx*x*x;        // Soma de quadrados nao ajustada
            if M[7]^[j] > x then M[7]^[j] := x; // Valor minino
            if M[8]^[j] < x then M[8]^[j] := x  // Valor maximo
            end { if x <> wscMissValue }
        end; { if (wx<>..) }
  if OneValObs then
    begin
    Result := TwsDFVec.Create(Stat.Len);          // Vetor que armazenara os resultados
    for j := 1 to Stat.Len do
      begin
      k := WhichCol(Col[j],L1);
      v := ScalarDiv(M[2]^[k],w1-1);
      x := ScalarSqrt(v);
      case Stat[j] of
        0:  Result[j] := M[1]^[k];                     // Media
        1:  Result[j] := v;                            // Variancia
        2:  Result[j] := x;                            // Desvio padrao
        3:  Result[j] := M[1]^[k]*w1;                  // Total
        4:  Result[j] := M[7]^[k];                     // Minimo
        5:  Result[j] := M[8]^[k];                     // Maximo
        6:  Result[j] := NRows;                        // Numero de valores
        7:  Result[j] := M[5]^[k];                     // Numero de valores validos
        8:  Result[j] := ScalarSqrt(ScalarDiv(v,w1));  // Erro padrao da media
        9:  Result[j] := M[8]^[k]-M[7]^[k];            // Amplitude
        10: Result[j] := 100*ScalarDiv(x,M[1]^[k]);    // Coeficiente de variacao
        11: if w1 > 2 then                             // Assimetria
               Result[j] := ScalarDiv(w1*M[3]^[k],(w1-1)*(w1-2)*v*x)
            else
               Result[j]:=wscMissValue;
        12: if w1 > 3 then                             // Curtose
              Result[j] := ScalarDiv(w1*(w1+1)*M[4]^[k]-3*M[2]^[k]*M[2]^[k]*
                (w1-1), (w1-1)*(w1-2)*(w1-3)*v*v)
            else
              Result[j] := wscMissValue;
        13: Result[j] := M[6]^[k];                     // Soma de quadrados nao corrigida
        14: Result[j] := M[2]^[k];                     // Soma de quadrados corrigida
        15: Result[j] := w1                            // Soma dos pesos
        end; {case}
      end {For}
    end
  else
    Result:=VecConst(wscMissValue,Stat.Len);

  L1.Free;
  for i := 1 to 8 do FreeMem(M[i], sf(nvar));
end; { VWDescStat }

function TwsMatrix.Perc(const f: Double; Col: Integer): Double;
{ Objetivo
    Obtem o percentil de uma coluna da matriz
  Pa?metros
    f: Valor do percentil desejado.
    Col: ?ndice da coluna para obten??o do percentil
  Observa??o
    f dever? estar entre 0 e 1 (0 < f < 1). Se n?o estiver, a fun??o retorna wscMissValue
    A fun??o pressup?e que os valores referentes ? coluna desejada est?o ordenados
}
var
  x: Double;
  Lin: Integer;
begin
  if (f>0) and (f<1) then
    begin
    x := NRows*f;
    Lin := Trunc(x);
    if Frac(x)<1.0e-9 then
      Result:=(Get(Lin,Col)+Get(Lin+1,Col))/2
    else
      Result:=Get(Lin+1,Col)
    end
  else
    Result := wscMissValue
end; { Perc }

function TwsMatrix.MatOrderStat(Col,Stat: TwsLIVec): TwsGeneral;
{ Objetivo
    Obtem estatisticas de ordem das colunas especificadas
  Par?metros
    Col: ?ndices das colunas desejadas
    Stat: Estat?sticas de ordem que se deseja calcular. Os c?digos s?o os seguintes:
      Ind  Perc         Nome
        0:    0   0%  Minimo
        1:  .01   1%   Perc1
        2:  .05   5%   Perc5
        3:  .10  10%  Perc10
        4:  .25  25%      Q1
        5:  .50  50% Mediana
        6:  .75  75%      Q3
        7:    1 100%  Maximo
        8:            Amplit
        9:           QAmplit
       10:  .90 90%   Perc90
       11:  .95 95%   Perc95
       12:  .99 99%   Perc99
       13:            Cerca Inferior
       14:            Cerca Superior
       15:            val < cerca inf (discrepantes inferiores)
       16:            val > cerca sup (discrepantes superiores)
       17:    1 100%  n_obs
       18:            Valor adjacente inferior
       19:            Valor adjacente superior
    Obs.: A sa?da ? uma matriz onde cada linha corresponde a uma vari?vel e cada
      coluna a uma estat?stica
}
type
  str6=string[10];
const
  NumStat=20;
  Name: array[0..NumStat-1] of str6 = ('Minimo','Perc1','Perc5','Perc10','Quart25','Mediana',
    'Quart75','Maximo','Amplit','AmpInt', 'Perc90','Perc95','Perc99','CercaInf','CercaSup',
    'DiscInf','DiscSup','nObs','AdjInf','AdjSup');
var
  i,j,kupp,klow: Integer;
  Aux,P        : TwsVec;
  AmpInt,ci,cs : Double;
begin
  Result := TwsGeneral.Create(Col.Len,Stat.Len);
  Result.MLab := 'Quantis dos Dados - '+MLab;
  Result.Name := 'Variaveis';
  for i := 1 to Stat.Len do
    Result.ColName[i] := Name[Stat[i]];
  P:= TwsDFVec.Create(9);
  for j := 1 to Col.Len do
    begin                                            // Para cada coluna
    Aux:=CopyCol(Col[j]);
    Result.RowName[j] := ColName[Col[j]];
    Aux.QuickSort(True);
    P[1]:=0.01; P[2]:=0.05; P[3]:=0.1; P[4]:=0.25; P[5]:=0.5; P[6]:=0.75; P[7]:=0.9;
    P[8]:=0.95; P[9]:=0.99;
    Aux.Percentis(P);
    AmpInt:=P[6]-P[4];                         // Amplitude interquartilica
    ci:=P[4]-AmpInt*1.5;                       // Cerca inferior
    cs:=P[6]+AmpInt*1.5;                       // Cerca superior
    klow := 1;                                 // Numero de valores abaixo da cerca inferior
    while ((Aux[klow] < ci) and (klow<=Aux.Len) or wsGLib.IsMissValue(Aux[klow])) do Inc(klow);
    kupp := Aux.Len;                           // Numero de valores acima da cerca superior
    while ((Aux[kupp]>cs) and (kupp>0) or wsGLib.IsMissValue(Aux[kupp])) do Dec(kupp);
    for i := 1 to Stat.len do
      case Stat[i] of
        0: Result[j,i]:= Aux[1];                     // Minimo
        1: Result[j,i]:= P[1];                       // 1%
        2: Result[j,i]:= P[2];                       // 5%
        3: Result[j,i]:= P[3];                       // 10%
        4: Result[j,i]:= P[4];                       // Q1  25%
        5: Result[j,i]:= P[5];                       // Med 50%
        6: Result[j,i]:= P[6];                       // Q3  75%
        7: Result[j,i]:= Aux[Aux.Len];               // Maximo
        8: Result[j,i]:= Aux[Aux.Len]-Aux[1];        // Amplitude total
        9: Result[j,i]:= AmpInt;                     // Amplitude interquartilica
        10: Result[j,i]:=P[7];                       // 90%
        11: Result[j,i]:= P[8];                      // 95%
        12: Result[j,i]:= P[9];                      // 99%
        13: Result[j,i]:= ci;                        // cerca inferior
        14: Result[j,i]:= cs;                        // cerca superior
        15: Result[j,i]:= klow-1;                    // Valores abaixo da cerca inferior
        16: Result[j,i]:= Aux.Len - kupp;            // Valores acima da cerca superior
        17: Result[j,i]:= Aux[Aux.Len];              // Extremo superior
        18: Result[j,i]:= Aux[klow];                 // Valor adjacente inferior
        19: Result[j,i]:= Aux[kupp]                  // Valor adjacente superior
      end; { case }
    Aux.Free;
    end; { for j - para cada variavel }
  P.Free;
end; { MatOrderStat }

function TwsMatrix.ScalarFunc(Op: TwsEnumConstFun; var n: Integer): Double;
{ Objetivo:
    Obt?m um valor escalar a partir de todos os elementos da matriz especificada.
  Par?metros:
    Op: Tipo de fun??o desejada
      Os tipos aceitos s?o:
        cSoma: Retorna a soma de todos os valores da matriz
        cMEDIA: M?dia
        cABSNORM: Norma do valor absoluto
        cSQUAD: Soma de quadrados
        cENORM: Norma euclidiana
        cTRACO: Tra?o da matriz
        cMax: Maior valor da matriz
        cMin: Menor valor da matriz
     n: Retorna o numero de valores validos presentes na matriz.
}
var
  i,j: Integer;
  x: Double;
begin
  Result := 0; n := 0;
  case Op of
    cSOMA:
      for i := 1 to NRows do
        for j := 1 to NCols do
          if not IsMissValue(i,j,x) then
             begin
             Inc(n);
             Result := Result + x
             end;
    cMEDIA:
      for i := 1 to NRows do
        for j := 1 to NCols do
          if not IsMissValue(i,j,x) then
            begin
            Inc(n);
            x := x - Result;
            Result := Result + x/n
            end;
    cABSNORM:
      for i := 1 to NRows do
        for j := 1 to NCols do
          if not IsMissValue(i,j,x) then
            begin
            Inc(n);
            Result := Result + Abs(x)
            end;
    cSQUAD:
      for i := 1 to NRows do
        for j := 1 to NCols do
          if not IsMissValue(i,j,x) then
            begin
            Inc(n);
            Result := Result + x*x
            end;
    cENORM:
      begin
      for i := 1 to NRows do
        for j := 1 to NCols do
          if not IsMissValue(i,j,x) then
            begin
            Inc(n);
            Result := Result + x*x
            end;
      Result:=Sqrt(Result)
      end;
    cTRACO:
      begin
      j := Math.Min(NRows, NCols);
      for i := 1 to j do
        if not IsMissValue(i,i,x) then
          begin
          Inc(n);
          Result := Result + x
          end
      end;
    cMax:
      begin
      Result := Get(1,1);
      for i := 1 to NRows do
        for j := 1 to NCols do
          if not IsMissValue(i,j,x) then
            begin
            Inc(n);
            Result := Math.Max(Get(i,j),Result)
            end;
      end;
    cMin:
      begin
      Result := Get(1,1);
      for i := 1 to NRows do
        for j := 1 to NCols do
          if not IsMissValue(i,j,x) then
            begin
            Inc(n);
            Result := Math.Min(Get(i,j),Result)
            end;
      end;
  end; // case
end; { ScalarFunc }

// Norma da coluna
function TwsMatrix.Norm(j,l1,l2: Integer): Double;
{ Objetivo
    Obtem a norma de uma coluna
  Par?metros
    j: Coluna para obten??o da norma
    l1, l2: linha inicial e final para obten??o da norma
  Observa??es
    Valores perdidos n?o s?o considerados
}
var
  i: Integer;
  x: Double;
  a: Extended;
begin
  a:=0;
  for i:=l1 to l2 do
    if not IsMissValue(i,j,x) then
      a:=a+Sqr(x);
  Result:=Sqrt(a)
end;

// Produto interno entre colunas
function TwsMatrix.ColProd(j,k,l1,l2: Integer): Double;
{ Objetivo
    Obtem produto interno (soma de produtos) de duas colunas
  Par?metros
    j,k: Colunas para obten??o do produto interno
    l1, l2: linha inicial e final para obten??o do produto interno
  Observa??es
    Valores perdidos n?o s?o considerados
}
var
  i  : Integer;
  x,y: Double;
  a  : Extended;
begin
  a:=0;
  for i:=l1 to l2 do
    if not (IsMissValue(i,j,x) or IsMissValue(i,k,y)) then
      a:=a+x*y;
  Result:=a
end;

function TwsMatrix.ToVec: TwsVec;
{ Objetivo
    Transforma a matriz num vetor copiando, sequencialmente por linhas, os seus valores
    num vetor.
}
var
  i,j,k: Integer;
begin
  Result := TwsDFVec.Create(NRows*NCols);
  k := 1;
  for i := 1 to NRows do
    for j := 1 to NCols do
      begin
      Result[k] := Get(i,j);
      Inc(k)
      end;
end; { ToVec }

function TwsMatrix.GToVec: TwsVec;
{ Objetivo
    Transforma a matriz num vetor copiando, sequencialmente por linhas, os seus valores
    num vetor. Difere de ToVec por copiar sempre a matriz completa, qualquer que seja o tipo
}
var
  i,j,k: Integer;
begin
  Result := TwsDFVec.Create(NRows*NCols);
  k := 1;
  for i := 1 to NRows do
    for j := 1 to NCols do
      begin
      Result[k] := Get(i,j);
      Inc(k)
      end;
end; { ToVec }


function TwsMatrix.DiagToVec: TwsVec;
{ Objetivo
    Transforma a matriz num vetor copiando, sequencialmente por linhas, os seus valores
    num vetor.
}
var
  i,j,k: Integer;
begin
  k:=Min(NRows,NCols);
  Result := TwsDFVec.Create(k);
  for i := 1 to k do
    Result[i] := Get(i,i);
end; { DiagToVec }

function TwsMatrix.RankTie(NewMat: Boolean): TwsMatrix;
{ Objetivo
    Obtem uma matriz com postos com empates dos elementos
  Par?metros
    NewMat: Se True, o resultado retorna numa nova matriz; caso contr?rio, substitui a
    pr?pria matriz
  Observa??es: Inicialmente a matriz ? copiada, sequencialmente por linhas, para um vetor,
    os postos com empates s?o obtidos para esse vetor e posteriormente copiados para a
    matriz resultante
}
var
  i,j,k: Integer;
  Aux,Tie: TwsVec;
begin
  Aux := ToVec;                      // Copia a matriz A para um vetor
  Tie := Aux.RankTie;                // Obtem postos com empates para o vetor
  Aux.Free;
  if (not (MatType = mtGeneral) or NewMat) then
    Result := TwsGeneral.Create(NRows,NCols)
  else
    Result := Self;
  k := 0;
  for i := 1 to NRows do
    for j := 1 to NCols do
      begin
      Inc(k);
      Result[i,j]:= Tie[k]
      end;
  Tie.Free
end; { RankTie }

function TwsMatrix.Rank(NewMat: Boolean; Ascd: Boolean): TwsMatrix;
{ Objetivo
    Obtem uma matriz com postos dos elementos
  Par?metros
    NewMat: Se True, o resultado retorna numa nova matriz; caso contr?rio, substitui a
      pr?pria matriz
    Ascd: True se a ordem ser? ascendente; false caso contr?rio
  Observa??es: Inicialmente a matriz ? copiada, sequencialmente por linhas, para um vetor,
    os postos s?o obtidos para esse vetor e posteriormente copiados para a matriz resultante
}
var
  i,j,k: Integer;
  Aux,Rank: TwsVec;
begin
  Aux := ToVec;                              // Copia a matriz A para um vetor
  Rank := Aux.Rank(Ascd, True);              // Obtem postos para o vetor
  Aux.Free;
  if (not (MatType=mtGeneral) or NewMat) then  // Escolhe onde colocara o resultado
    Result := TwsGeneral.Create(NRows,NCols)
  else
    Result := Self;
  k := 0;
  for i := 1 to NRows do                     // Copia para o resultado
    for j := 1 to NCols do
      begin
      Inc(k);
      Result[i,j]:= Rank[k]
      end;
  Rank.Free
end; { Rank }

procedure TwsMatrix.ColExtrems(k: Integer; out Min, Max: Double);
var
  i     : Integer;
  x1, x2: Double;
begin
  Min := Self[1,k]; Max := Min;
  i := 2;
  while i < nRows do
    begin
    x1 := Self[i,k]; x2 := Self[i+1,k];
    if x1 > x2 then
      begin
      if x1 > Max then
        Max := x1;
      if x2 < Min then
        Min := x2
      end
    else
      begin
      if x2 > Max then
        Max := x2;
      if x1 < Min then
        Min := x1
      end;
    Inc(i, 2)
    end; { while }
  if i = nRows then
    begin
    if Self[i,k] > Max then
      Max := Self[i,k]
    else
      if Self[i,k] < Min then
        Min := Self[i,k]
    end
end; { ColExtrems }

procedure TwsMatrix.SortCol(Indx: TwsLIVec);
{ Objetivo
    Coloca os valores das colunas segundo os ?ndices especificados
  Par?metros
    Indx: ?ndices em que ser?o colocados os valores das colunas
}
var
  i: Integer;
  Erro :Word;
begin
  FModified := true;
  for i := 0 to FList.Count-1 do Row[i+1].SortOf(Indx, Erro)
end; { TwsMatrix.SortCol }

procedure TwsMatrix.ToText(var TFile: Text; IsLast: Boolean = True; LSep: Char = ';'; SSep: Char = '/');
  { Objetivo
    Escreve a matriz num arquivo texto.
  Par?metros
    TFile: Arquivo texto onde ser? escrita a matriz.
    IsLast: Indica se a matriz ? a ?ltima a ser gravada, assim podemos marcar o final do arquivo
    LSep: caracter que ir? indicar o final da linha da matriz
    SSep: Caracter que ir? indicar o final de se??es (Nomes de linhas, colunas, etc.).
    Espa?os em branco separar?o os elementos de cada se??o.
}
var
  aux: String;
  i,j: Integer;
  R  : TwsVec;
begin
  aux:='TIPO=';
  case FMatType of
    mtGeneral:aux:=aux+'GERAL'+SSep;
    mtSymmetric:aux:=aux+'SIMETRICA'+SSep;
    mtDiagonal:aux:=aux+'DIAGONAL'+SSep;
    mtTriangular:aux:=aux+'TRIANGULAR'+SSep;
    mtVandermonde:aux:=aux+'VANDERMONDE'+SSep;
    mtToeplitz:aux:=aux+'TOEPLITZ'+SSep;
    end; // case
  WriteLn(TFile, Aux);

  if CName<>nil then
    begin
    aux:='NOMECOL=';
    for i:=1 to FNCols do
      aux:=aux+' ' + ColName[i];
    aux:=aux+SSep;
    WriteLn(TFile, Aux);
    end;

  aux:=FName+'=';
  R:=GetRow(1);
  for j:=1 to R.Len do
    begin
    aux:=aux+'  ';
    AppendStr(aux, FloatToStrF(R[j], ffGeneral, PrintOptions.ColWidth, PrintOptions.ColPrecision))
    end;
  if FList.Count > 1 then
    aux:=aux+LSep
  else
    aux:=aux+SSep;
  WriteLn(TFile,aux);

  for i:=1 to FList.Count-1 do
    begin
    aux:='';
    R:=GetRow(i+1);
    for j:=1 to R.Len do
      begin
      aux:=aux+'  ';
      AppendStr(aux,FloatToStrF(R[j],ffGeneral, PrintOptions.ColWidth, PrintOptions.ColPrecision))
      end;
    if i<FList.Count-1 then
      aux:=aux+LSep
    else
      aux:=aux+SSep;
    WriteLn(TFile,aux)
    end;

  if IsLast then
     WriteLn(TFile, 'FIM/');
end; { TwsMatrix.ToText }

function TwsMatrix.ToDataSet: TwsDataSet;
{ Objetivo
    Transforma todas as colunas de uma matriz em colunas num?ricas de um conjunto de dados
}
var
  i: Integer;
  v: TwsVec;
begin
  Result:=TwsDataSet.Create(FName+'_CD');
  for i:=1 to NCols do
    Result.Struct.AddColEx(TwsNumeric.Create(ColName[i],'Coluna '+IntToStr(i),14,9));
  for i:=1 to FNRows do
    Result.MAdd(Row[i].Copy(1,FNCols))
end; // ToDataSet

procedure TwsMatrix.TextTo(var TFile: Text; Ch: Char);
{ Objetivo
    Preenche uma matriz com valores de um arquivo texto
  Par?metros
    TFile: Arquivo texto com os valores
    Ch: Caracter delimitador de cada linha. Cada elemento ser? delimitado por um caracter do
    conjunto [#9,#10,#13,' ',',','=','\', '"','(',')'].
}
var
  EndSec: boolean;
  l,k   : Integer;
  Lin   : TwsVec;
begin
  for k := 1 to FList.Count do
    begin
    Lin := Row[k];
    for l := 1 to Lin.Len do
      Lin[l] := FracToReal(StrGetF(TFile, Ch, EndSec, DelChar))
    end
end; { TwsMatrix.TextTo }

function TwsMatrix.All(const x: Double; Op: TwsEnumTypeOp): boolean;
{ Objetivo
    Verificar se todos os elementos da matriz atendem ? condi??o estabelecida por Op.
  Par?metros
    x: Escalar que ser? comparado com os valores da matriz
    Op: Tipo de compara??o desejada
}
var
  i,j: Integer;
begin
  case Op of
    opEQ: // Todos os elementos s?o iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarEQ(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opGE: // Todos os elementos s?o maiores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGE(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opGT: // Todos os elementos s?o maiores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGT(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opLE: // Todos os elementos s?o menores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLE(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opLT: // Todos os elementos s?o menores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLT(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opNE: // Todos os elementos s?o diferentes ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarNE(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    end; // case
end; { All }


function TwsMatrix.Any(const x: Double; Op: TwsEnumTypeOp): boolean;
{ Objetivo
    Verificar se algum elemento da matriz atende ? condi??o estabelecida por Op.
  Par?metros
    x: Escalar que ser? comparado com os valores da matriz
    Op: Tipo de compara??o desejada
}
var
  i,j: Integer;
begin
  case Op of
    opEQ: // Todos os elementos s?o iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarEQ(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opGE: // Todos os elementos s?o maiores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGE(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opGT: // Todos os elementos s?o maiores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGT(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opLE: // Todos os elementos s?o menores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLE(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opLT: // Todos os elementos s?o menores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLT(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opNE: // Todos os elementos s?o diferentes ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarNE(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    end; // case
end; { Any }

function TwsMatrix.RowConcat(B: TwsMatrix;Var ErrCode: Word): TwsGeneral;
{ Objetivo
    Concatena matrizes por linhas. Responde ao operador //.
  Par?metros
    B:       Matriz cujas linhas ser?o inseridas no final
    ErrCode: Retorna zero se a concatena??o for feita com sucesso. NImprDim se o n?mero de
             colunas de A for diferente do de B.
  Resultado
    A chamada A.RowConcat(B,ErrCode) retornar? uma matriz com as linhas de A, seguidas das
      linhas de B
}
var
  i,j: Integer;
  F  : TwsVec;
begin
  ErrCode:=0;
  If NCols = B.NCols Then
    Begin
    Copy(mtGeneral,TwsMatrix(Result));
    for i := 1 to B.NRows do
      begin
      F := TwsDFVec.Create(B.NCols);
      for j := 1 to B.NCols do F[j] := B[i,j];
      F.Name:=B.RowName[i];
      Result.MAdd(F);
      end;
    End
  Else
    Begin
    ErrCode:=NImprDim;
    Result:=Nil;
    End;
end; { RowConcat }

function TwsMatrix.ColConcat(B: TwsMatrix;Var ErrCode: Word): TwsGeneral;
{ Objetivo
    Concatena matrizes por colunas. Responde ao operador ||.
  Par?metros
    B: Matriz cujas colunas serao colocadas por ultimo
    ErrCode: Retorna zero se a concatenacao foi feita com sucesso. NImprDim se o numero de
      linhas de A e B nao for o mesmo.
  Resultado
    A chamada A.ColConcat(B,ErrCode) retornar? uma matriz com as colunas de A, seguidas das
      colunas de B
 }
var
  i, j: Integer;
  F: TwsVec;
begin
  ErrCode:=0;
  If NRows = B.NRows Then
    Begin
    Result := TwsGeneral.Create(0,NCols + B.NCols);
    Result.Name := Name;
    if (CName <> nil) then
      begin
      for i := 1 to NCols do
        Result.ColName[i] := ColName[i];
      if B.CName <> nil then
        for i := 1 to B.NCols do
          Result.ColName[NCols+i] := B.ColName[i]
      else
        for i := 1 to B.NCols do
          Result.ColName[i] := 'Col'+IntToStr(NCols+i)
      end;
    for i := 1 to NRows do
      begin
      F := TwsDFVec.Create(Result.NCols);
      F.Name := RowName[i];
      for j := 1 to NCols do F[j]:=Get(i,j);
      for j := 1 to B.NCols do F[NCols+j] := B[i, j];
      Result.MAdd(F);
      end
    End
  Else
    Begin
    ErrCode:=NImprDim;
    Result:=Nil;
    End;
end; { ColConcat }

function TwsMatrix.RowScalarConcat(const x: Double; SFirst:boolean): TwsGeneral;
{ Objetivo
    Concatena por linha um escalar com a matriz n x 1. Responde ao operador //
  Par?metros
    x     : Escalar a concatenar
    SFirst: Se True ent?o o escalar vai primeiro (em todas as colunas) e os elementos da
            matriz o seguem; se False o escalar ? colocado por ?ltimo (em todas as colunas).
}
var
  i,j: Integer;
begin
  Result := TwsGeneral.Create(NRows+1,NCols);
  for j:=1 to NCols do
    begin { Se tem uma coluna concatena na coluna }
    if SFirst then
      begin  { Escalar vai no inicio? }
      Result[1,j]:=x;
      for i := 1 to NRows do
        Result[i+1,j]:=Get(i,j)
      end
    else
      begin { ou no final ? }
      for i := 1 to NRows do
        Result[i,j]:=Get(i,j);
      Result[NRows+1,j]:=x
      end;
    end
end;{RowScalarConcat}

function TwsMatrix.ColScalarConcat(const x: Double; SFirst:boolean): TwsGeneral;
{ Objetivo
    Concatena por coluna um escalar com uma matriz. Responde ao operador ||.
  Par?metros
    x     : Escalar para concatenar
    SFirst: Se True entao o escalar vai primeiro e os elementos da matriz o seguem; se False o
            escalar e colocado por ultimo.
}
var
  i,j: Integer;
begin
  Result := TwsGeneral.Create(NRows,NCols+1);
  if SFirst then
    for i:=1 to NRows do
      begin  { Escalar vai no inicio? }
      Result[i,1]:=x;
      for j := 1 to NCols do
        Result[i,j+1]:=Get(i,j)
      end
  else
    for i:=1 to NRows do
      begin { ou no final ? }
      for j := 1 to NCols do
        Result[i,j]:=Get(i,j);
      Result[i,NCols+1]:=x
      end;
end; { ColScalarConcat }

function TwsMatrix.RowVecConcat(v: TwsVec; NewMat: Boolean; VFirst: Boolean;
  Var ErrCode: Word): TwsGeneral;
{ Objetivo
    Concatena por linha uma matriz e um vetor. Responde ao operador //
  Par?metros
    v:       Vetor a ser inserido
    NewMat:  Se true (ou se a matriz n?o ? geral), ent?o o vetor ser? inserido na matriz
             resultante da c?pia; se false, o vetor ser? inserido na mesma matriz
    VFirst:  Se true, ent?o o vetor vai na primeira posi??o; se false, na ?ltima
    ErrCode: Retorna zero se a concatena??o foi feita com sucesso; NImprDim se o n?mero
             de elementos do vetor for diferente do n?mero de colunas da matriz.
}
begin
  ErrCode:=0;
  if NCols = v.Len then
    begin
    if (NewMat or (FMatType<>mtGeneral)) then
      Copy(mtGeneral,TwsMatrix(Result))
    else
      Result := TwsGeneral(Self);
    if VFirst then
      Result.MInsert(1,v)
    else
      Result.MAdd(v)
    end
  else
    Begin
    ErrCode:=NImprDim;
    Result:=Nil;
    End;
end;//RowVecConcat

function TwsMatrix.ColVecConcat(v: TwsVec; VFirst:boolean; Var Erro:Word): TwsGeneral;
{ Objetivo
    Concatena por coluna um vetor com uma matriz. Responde ao operador ||.
  Par?metros
    x:       Vetor a concatenar.
    VFirst:  Se true, ent?o o vetor vai primeiro e as colunas da matriz depois.
    ErrCode: Retorna zero se a concatena??o foi feita com sucesso; NImprDim se o n?mero
             de elementos do vetor for diferente do n?mero de linhas da matriz.
}
var
  nc,i,j:Integer;
begin
  Erro:=0;
  if NRows = v.Len then
    begin                                     {a11 a12 a13 ... v1}
    nc := NCols+1;                            {a21 a22 a23 ... v2}
    Result := TwsGeneral.Create(NRows, nc);   {a31 a32 a33 ... v3}
    if VFirst then
      for i := 1 to NRows do
        begin
        Result[i,1] := v[i];
        for j := 2 to nc do
          Result[i,j]:=Get(i,j-1);
        end
    else
      for i := 1 to NRows do
        begin
        for j := 1 to NCols do
          Result[i,j]:=Get(i,j);
        Result[i,nc] := v[i]
        end
    end
  else
    Begin
    Erro:=NImprDim;
    Result:=Nil;
    End;
end;//VecColConcat

function TwsMatrix.Echelon(NewMat: Boolean; CMax: Integer=-1): TwsGeneral;
{ Objetivo:
    Obt?m a forma escalonada da matriz especificada. Algoritmo em Noble (1a. edi??o)
  Par?metros:
    NewMat: False indica que a forma escalonada substiuir? A, caso contr?rio (ou se a
            matriz n?o for geral) uma nova matriz ser? criada.
    CMax:   Indice m?ximo da coluna a ser operada.
}
const
  eps=1e-9;
var
  i,j,p,q: Integer;
  ErrCode: Word;
  L0,L1  : TwsVec;
  x      : Double;

  function Max(Mat: TwsMatrix; k,l: Integer): Integer;
  { Iniciando na linha k+1, em que linha esta o maior valor da coluna l ?}
  var
    kk  : Integer;
    y,y1: Double;
  begin
    y := Abs(Mat[k+1,l]);
    Result := k+1;
    for kk := k+2 to Mat.NRows do
      begin
      y1 := Abs(Mat[kk,l]);
      if y1 > y then
        begin
        y := y1;
        Result := kk
        end
      end
  end;

begin
  if CMax=-1 then
    CMax:=Math.Min(NRows,NCols);
  if ((MatType<>mtGeneral) or NewMat) then
    Copy(mtGeneral,TwsMatrix(Result))
  else
    Result := TwsGeneral(Self);
  i := 1;
  j := 1;
  repeat
    // Em que linha esta o maximo ?
    if i < Result.NRows then
      p := Max(Result,i,j)
    else
      p := i;
    if not FEquals(Get(p,j),0) then
      begin
      if p <> i then
        Result.Exchange(i,p);                  // Troca as linhas i e p
      L0 := Result.Row[i];
      x := L0[j];                               // x eh o elemento pivo
      if not FEquals(x,0) then
        begin
        L0.ByScalar(x,opDiv,False,False);       // Divide a linha i pelo pivo
        for q := 1 to Result.NRows do
          if q <> i then
            begin
            L1 := Result.Row[q];
            x := L1[j];
            L1.ElemOper3(L0,-x,False,ErrCode)   // Substitui linha q pela combinacao linear
            end
        end
      else
        for q:= 1 to L0.Len do
          L0[q]:=0;
      Inc(i);
      Inc(j)
      end
    else
      Inc(j)
  until (i > Result.NRows) or (j > CMax);
  // Troca as linhas para obter forma escalonada reduzida por linhas
  j := 1;
  for i := 1 to Result.NRows do
    begin
    if Get(i,j) <> 1 then
       for p := i+1 to Result.NRows do
         if Get(p,j) = 1 then
           Result.Exchange(p, i);
    Inc(j)
    end
end; // Echelon

function TwsMatrix.QSort(Asc: Boolean=True): TwsGeneral;
{ Objetivo
    Ordena os valores de uma matriz
  Par?metros
    Asc: true (default) para ordem crescente; false, caso contr?rio
  Retorno
    Retorna uma matriz geral, com os dados ordenados no sentido das linhas. Assim, se
    Asc=True, a primeira linha conter? os menores valores e assim por diante.
}
var
  v: TwsVec;
  i,j,k: Integer;
begin
  v:=GToVec;
  v.QuickSort(Asc);
  Result:=TwsGeneral.Create(NRows,NCols);
  k:=0;
  for i:=1 to NRows do
    for j:=1 to NCols do
      begin
      Inc(k);
      Result[i,j]:=v[k]
      end;
  v.Free
end; // QSort

Procedure TwsMatrix.PutLine(V: TwsVec);
{ Objetivo
    Concatenar uma linha na matriz. N?o atualiza o n?mero de linhas.
  Par?metros
    v: Linha a ser concatenada
}
Begin
  FModified := True;
  FList.Add(V);
End; { TwsMatrix.PutLine }

class function TwsMatrix.InternalLoad1(Reader: TReader): TwsEnumMatType;
Begin
  Reader.ReadSignature;
  Result := TwsEnumMatType(Reader.ReadInteger);
end;

procedure TwsMatrix.InternalLoad2(Reader: TReader);
begin
  With Reader Do
    begin
    FVersion := ReadString;
    Read(FExtra, SizeOf(FExtra));

    FName     := ReadString;

    MLab      := ReadString;
    Tag_1     := ReadInteger;
    Tag_2     := ReadInteger;

    NRows     := ReadInteger;
    PrintOptions.MaxIDSize := ReadInteger;

    CNameLoadFromStream(Reader);
    end;
end;

class function TwsMatrix.UserCreate: TwsMatrix;
begin
  Raise Exception.Create('M?todo "UserCreate" n?o definido');
end;

function TwsMatrix.getLocked: Boolean;
begin
  Result := (FLocked > 0);
end;

procedure TwsMatrix.Lock;
begin
  inc(FLocked);
  if FLocked = 1 then
     if Assigned(FStatusChange) then FStatusChange(Self, [scLock]);
end;

procedure TwsMatrix.Unlock;
begin
  dec(FLocked);
  if FLocked = 0 then
     if Assigned(FStatusChange) then FStatusChange(Self, [sclock]);

  if FLocked < 0 then
     begin
     FLocked := 0;
     raise Exception.Create('TwsMatrix: Unlock syncronize error');
     end;
end;

procedure TwsMatrix.NotifyChange(Status: TwsStatusChangeSet);
begin
  getMessageManager.SendMessage(wsm_MatrixChange, [self]);
  if Assigned(FStatusChange) then FStatusChange(Self, Status);
end;

function TwsMatrix.PrintDescVars: Boolean;
begin
  Result := false;
end;

function TwsMatrix.SortedCol(IndexCol: Integer; Ascd: Boolean): Boolean;
{ Objetivo
    Busca primeiro par n?o ordenado por meio de uma busca sequencial
  Par?metros
    i: Se houver par n?o ordenado, eles ser?o i e i+1
    Ascd: true para ordena??o ascendente; false caso contr?rio
  Campos alterados
    FData
  M?todos chamados
    Nenhum
}
var i: Integer;
begin
  Result := True;
  i := 1;
  Ascd := not Ascd;
  while Result and (i < nRows) do
    begin
    Result := (Compare(Data[i, IndexCol], Data[i+1, IndexCol], Ascd) >= 0);
    Inc(i);
    end
end;

function TwsMatrix.getGroup: String;
var i: Integer;
begin
  if MLab <> '' then
     begin
     i := System.Pos('Grupo:', MLab);
     if i > 0 then
        Result := System.Copy(MLab, i+6, Length(MLab))
     else
        Result := '';
     end
  else
     Result := '';
end;

function TwsMatrix.AddRow(const Name: string): TwsVec;
begin
  Result := TwsDFVec.Create(self.nCols);
  Result.Name := Name;
  self.MAdd(Result);
end;

{$ifdef MSXML}
// A matriz dever? estar limpa (sem linhas e colunas)
procedure TwsMatrix.fromXML(no: IXMLDomNode);
var cn: IXMLDomNodeList;
     i: integer;
     c: integer;
     v: TwsDFVec;
begin
  self.Name := no.attributes.item[0].text;
  self.MLab := no.attributes.item[5].text;
  self.PrintOptions.MaxIDSize := toInt(no.attributes.item[3].text);
  self.PrintOptions.ColPrecision := toInt(no.attributes.item[4].text);
  self.PrintOptions.PrintDesc := toBoolean(no.attributes.item[6].text);

  cn := no.childNodes.item[0].childNodes;
  fromXML_LoadStruct(cn.item[0]); // virtual
  for i := 1 to cn.length-1 do
    begin
    v := TwsDFVec.Create(0);
    v.fromXML(cn.item[i]);
    MAdd(v);
    end;
end;

procedure TwsMatrix.fromXML_LoadStruct(no: IXMLDomNode);
var i: Integer;
begin
  FNCols := no.childNodes.length;
  for i := 0 to FNCols-1 do
    FCName.Add(no.childNodes.item[i].text);
end;

{$endif MSXML}
{ TwsGeneral }

constructor TwsGeneral.Create(NR, NC: Integer);
{ Objetivo
    Cria uma matriz geral. ? denominada matriz geral a matriz onde todos os elementos necessitam
    ser explicitamente armazenados.
  Par?metros
    NR: N?mero de linhas.
    NC: N?mero de colunas
   M?todos chamados
     Create herdado
   Campos modificados
     FMatType
     Se NR>0 insere linhas em branco
}
var
  i: Integer;
begin
  inherited Create(NR, NC);
  FMatType := mtGeneral;
  if FNRows > 0 then
    for i := 1 to FNRows do FList.Add(TwsDFVec.Create(FNCols))
end; { TwsGeneral.Create }

function TwsGeneral.GetBlockName: String;
begin
  Result := 'wsGeneral';
end;

function TwsGeneral.GetBlockNameComment: String;
begin
  Result := 'Estilo de formata??o para Matrizes do tipo TwsGeneral';
end;

function TwsGeneral.Get(i,j: Integer): Double;
{ Objetivo
    Obtem um elemento da matriz geral.
  Par?metros
    i: ?ndice da linha
    j: ?ndice da coluna
}
begin
  Get := Row[i].Data[j];
end; { TwsGeneral.Get }

procedure TwsGeneral.Put(i, j: Integer; x: Double);
{ Objetivo
    Atribui um valor a uma posi??o da matriz
  Par?metros
    i: Linha onde o elemsnto ser? colocado
    j: Coluna onde o elemento ser? colocado
    x: Valor a ser atribu?do ? posi??o (i, j)
}
begin
  FModified := True;
  Row[i].Data[j] := x;
end; { TwsGeneral.Put }

procedure TwsGeneral.List(Buffer: TStrings);
{ Objetivo
    Lista os descritores de uma matriz geral
  M?todos chamados
    List herdado
}
var
  Q: TCharStr;
begin
  Buffer.Add(StrPas(StrCopy(Q, 'Tipo:    Geral')));
  inherited List(Buffer)
end;

function TwsGeneral.ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst, NewMat: Boolean): TwsMatrix;
{Objetivo
   Efetuar opera??es entre uma matriz geral e um escalar
 Par?metros
   x: Escalar para a opera??o
   Op: Tipo de opera??o desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas opera??es (soma ou produto) esse par?metro n?o ? utilizado.
     No caso de potencia??o, SFirst=True indica que o escalar estar? na base e o elemento
     na pot?ncia; caso contr?rio, o elemento estar? na base e o escalar na pot?ncia. Para
     o operador ** se SFirst=False, a opera??o corresponder? ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar (observe que, neste caso, a matriz deve
     ser quadrada para que o produto seja sempre definido); caso contr?rio a opera??o n?o
     ? definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz
 Retorno
   Sempre uma matriz geral
 Valores perdidos
   Se houver um valor perdido na opera??o envolvendo um elemento, o resultado ? valor perdido
}
var
  i: Integer;
begin
  if NewMat then
    Self.Copy(mtGeneral,Result)
  else
    Result := Self;
  for i:=1 to Result.NRows do
    Result.Row[i].ByScalar(x,Op,False,SFirst);
end;

procedure TwsGeneral.Eigen(var B: TwsDiagonal; EgV:Boolean; var r:Integer;
  Sort: Boolean; eps: Double; var ErrCode: Word);
{ Objetivo
    Obt?m autoestrutura da matriz
  Par?metros
    B  : matriz mtDiagonal na qual retornar?o os autovalores
    EgV: Se true, na matriz que requisita o m?todo retornar?o os autovetores; se false os
    autovetores n?o ser?o obtidos
    r  : Posto num?rico da matriz, ou o n?mero de autovalores que, em m?dulo, s?o maiores
         que a precis?o estabelecida
    Sort: Se True os autovalores ser?o ordenados em ordem descendente. Os autovetores, se forem
          obtidos, ser?o tamb?m ordenados
    eps  : precis?o para determina??o do posto num?rico
    ErrCode: retorna 0 se nenhum erro ocorreu durante o processo; <>0 caso contr?rio
}
var
  e, d: TwsVec;
  Indx: TwsLIVec;
  i   : Integer;
begin
  ErrCode := 0;
  d := TwsDFVec.Create(NCols);
  e := TwsDFVec.Create(NCols);
  Tred2(d,e,EgV);
  TQLI(d,e,EgV,ErrCode);
  e.Free;
  if ErrCode = 0 then
    begin
    r := NCols;
    if Sort then
      begin
      Indx := d.QuickIndx(False);
      if EgV then SortCol(Indx);
      i:=1;
      while (Abs(d[i])<eps) and (i<=d.Len) do Dec(r);
      Indx.Free;
      end
    else
      for i:=1 to NCols do
        if Abs(d[i])<eps then Dec(r);
    B := VecToDiag(d);
    end
end; { EigenStruct }

procedure TwsGeneral.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    C?pia da matriz
  Par?metros
    MT: Tipo da matriz de retorno. Tipo especificado dever? ser: mtGeneral, mtSymmetric, mtDiagonal,
        mtTriangular, mtVandermonde ou mtToeplitz
    Matrix: Matriz que retorna com a c?pia
}
var
  i,j: Integer;
  L0 : TwsVec;
begin
  case MT of
    mtGeneral:
      begin
      Matrix := TwsGeneral.Create(NRows,NCols);
      for i := 1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j := 1 to NCols do
          Matrix[i,j]:= Self[i,j]
        end
      end;
    mtSymmetric:
      begin
      Matrix := TwsSymmetric.Create(NCols);
      for i:=1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j]:=Self[i,j]
        end
      end;
    mtTriangular:
      begin
      Matrix := TwsTriangular.Create(NCols);
      for i:=1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j] := Self[i,j]
        end
      end;
    mtDiagonal:
      begin
      Matrix := TwsDiagonal.Create(NCols);
      L0 := Matrix.Row[1];
      for j:=1 to NCols do
        L0[j]:= Self[j,j]
      end;
    mtToeplitz,mtVandermonde:
      begin
      Matrix := nil;
      Exit
      end;
  end; { case }
  inherited Copy(MT, Matrix)
end; { Copy }

procedure TwsGeneral.SortRows(Column, Ascend: TwsLIVec);
  { Objetivo
    Ordena as linhas da matriz, em ordem ascendente ou descendente, utilizando as
    colunas indicadas como chaves.
  Par?metros
    Column: Vetor que indica os ?ndices das colunas que funcionar?o como chaves de
        ordena??o.
    Ascend: Vetor de mesma dimens?o de Column que indica se para a coluna correspondente
       a ordena??o ser? ascendente ou descendente. Se para uma coluna a posi??o Ascend
       tiver 0 (zero) na posi??o correspondente a ordem ser? descendente; ascendente se
       tiver um valor diferente de zero.
}
var
  SortCount: Integer;                                { Incremento para as colunas }

  function Less(x, y: TwsVec): Boolean;
  var
    ColPos, j: Integer;
  begin
    for j := 1 to SortCount do begin
      ColPos := Column[j];
      if Ascend[j] = 0 then begin                           { Ordem descendente }
        if x[ColPos] > y[ColPos] then begin
          Less := True;
          Exit
        end
        else
          if x[ColPos] < y[ColPos] then begin
            Less := False;
            Exit
          end
      end
      else begin                                              { Ordem ascendente }
        if x[ColPos] < y[ColPos] then begin
          Less := True;
          Exit
        end
        else
          if x[ColPos] > y[ColPos] then begin
            Less := False;
            Exit
          end
      end
    end;
    Less := False
  end; { Less }

  procedure MatShellSort;
  var
    Gap, i, j, jj: Integer;
  begin
    Gap := FNRows;
    repeat
      Gap := Gap shr 1;
      if Gap > 0 then begin
        for i := 1 to FNRows - Gap do begin
          j := i;
          while j >= 1 do begin
            jj := j + Gap;
            if not Less(Row[j], Row[jj]) then Exchange(j, jj);
            Dec(j, Gap)
          end
        end
      end;
    until Gap = 0
  end; { MatShell }

Begin
  SortCount := 1;
  while SortCount <= Column.Len do begin
    MatShellSort;
    Inc(SortCount);
  end;
End; { TwsGeneral.SortRows }

procedure TwsGeneral.SortRowAs(Indx: TwsLIVec; var ErrCode: Word);
{ Objetivo
    Coloca as linhas segundo os ?ndices especificados
  Par?metros
    Indx: ?ndices em que ser?o colocadas as linhas
}
var
  i: Integer;
  row: array of Pointer;
begin
  ErrCode:=0;
  System.SetLength(row,FList.Count);
  if FList.Count = Indx.Len then
    begin
    FModified := true;
    for i := 1 to FList.Count do
      row[i-1]:=FList.Items[Indx[i]-1];
    for i := 0 to FList.Count-1 do
      FList.Items[i]:=row[i]
    end
  else
    ErrCode:=nImprDim
end; { TwsMatrix.SortRowAs }

procedure TwsGeneral.ExchangeCols(j,k: Integer);
{ Objetivo
    Trocar o conte?do de colunas
  Par?metros
    j, k: Indices de colunas para trocar
}
var
  Temp: Double;
  i   : Integer;
begin
  for i:=1 to nRows do
    begin
    Temp:=Get(i,j);
    Put(i,j,Get(i,k));
    Put(i,k,Temp)
    end;
end; // ExchangeCols

procedure TwsGeneral.ElemOperI(i,j: Integer; ToRows: Boolean=True);
{ Objetivo
    Aplicar sobre a matriz operacao elementar do tipo I (troca de linhas
  Par?metros
    i, j: Linhas (colunas) a trocar
    ToRows: Se True (default) opera sobre linhas; caso contrario, sobre colunas
}
begin
  if ToRows then
    Exchange(i,j)          // aplica a linhas
  else
    ExchangeCols(i,j)      // ou a colunas
end; // ElemOperI

procedure TwsGeneral.ElemOperII(i: Integer; const k: Double; ToRows: Boolean=True);
var
  j: Integer;
begin
  if ToRows then
    for j:=1 to nCols do
      Put(i,j,k*Get(i,j))
  else
    for j:=1 to nRows do
      Put(j,i,k*Get(j,i))
end;

procedure TwsGeneral.ElemOperIII(i,j: Integer; const k: Double; ToRows: Boolean=True);
var
  jj: Integer;
begin
  if ToRows then
    for jj:=1 to nCols do
      Put(i,jj,Get(i,jj)+k*Get(j,jj))
  else
    for jj:=1 to nRows do
      Put(jj,i,Get(jj,i)+k*Get(jj,j))
end;

procedure TwsGeneral.TQLI(d, e: TwsVec; EgV: Boolean; var ErrCode: Word);
  { Objetivo
    Implementa o algoritmo QL com shift implicito para determinar os autovalores
    e autovetores de uma matriz simetrica, tridiagonal ou de uma matriz real e
     simetrica previamente reduzida por Tred2. Se a matriz de autovetores for obtida,
     ela retorna na matriz que aciona este m?todo. Esta matriz deve ser sim?trica
     mas armazenada na forma geral.
  Par?metros
      d: Na entrada cont?m os elementos da mtDiagonal da matriz tridiagonal. Na sa?da
            retorna os autovalores.
      e: Na entrada cont?m os elementos da subdiagonal da matriz tridiagonal, com
            e[1] arbitrario. Na saida e e destruido.
      ErrCode: 0 se nenhum erro ocorreu na obten??o; <> 0 caso contr?rio
   }

label 10, 20;
var
  m,l,iter,i,k : Integer;
  s,r,p,g,f,dd,
  c,b          : Double;
  Lin          : TwsVec;
begin
  ErrCode := 0;
  for i := 2 to FNCols do e[i-1] := e[i];
  e[FNCols] := 0.0;
  for l := 1 to FNCols do
    begin
    iter := 0;
10: for m := l to FNCols-1 do
      begin
      dd := abs(d[m]) + abs(d[m+1]);
      if abs(e[m]) + dd = dd then GoTo 20
      end;
    m := FNCols;
20: if m <> l then
      begin
      if iter = 30 then
        begin
        ErrCode := maConvError;
        Exit
        end;
      Inc(iter);
      g := (d[l+1] - d[l])/(2.0*e[l]);
      r := sqrt(g*g + 1.0);
      g := d[m] - d[l] + e[l] / (g + Sign(r, g));
      s := 1.0;
      c := 1.0;
      p := 0.0;
      for i := m-1 downto l do
        begin
        f := s * e[i];
        b := c * e[i];
        if abs(f) >= abs(g) then
          begin
          c := g / f;
          r := sqrt(c*c + 1.0);
          e[i+1] := f * r;
          s := 1.0 / r;
          c := s * c
          end
        else
          begin
          s := f / g;
          r := sqrt(s*s + 1.0);
          e[i+1] := g * r;
          c := 1.0 / r;
          s := s * c;
          end;
        g := d[i+1] - p;
        r := (d[i] - g) * s + 2.0 * c * b;
        p := s * r;
        d[i+1] := g + p;
        g := c * r - b;
        if EgV then
          for k := 1 to FNCols do
            begin
            Lin := Row[k];
            f := Lin[i+1];
            Lin[i+1] := s * Lin[i] + c * f;
            Lin[i] := c * Lin[i] - s * f
            end
        end;
      d[l] := d[l] - p;
      e[l] := g;
      e[m] := 0.0;
      GoTo 10
      end
    end
end; { TwsGeneral.TQLI }

procedure TwsGeneral.Tred2(d, e: TwsVec; EgV: Boolean);
  { Objetivo
   Faz a redu??o de Householder da matriz, real e sim?trica, que chama o m?todo.
    Na sa?da, a matriz ? substituida pela matriz ortogonal Q que faz a transformacao.
 Par?metros
   d: Retorna os elementos da mtDiagonal da matriz triagonal
   e: Retorna os elementos da subdiagonal da matriz tridiagonal.
   EgV: True se os autovetores ser?o obtidos
}
var
  l,k,j,i  : Integer;
  scale,hh,
  h,g,f    : Double;
  L0, Lin  : TwsVec;
begin
  for i := FNCols downto 2 do
    begin
    Lin := Row[i];
    l := i-1;
    h := 0.0;
    scale := 0.0;
    if l > 1 then
      begin
      for k := 1 to l do
        scale := scale + abs(Lin[k]);
      if scale = 0.0 then
        e[i] := Lin[l]
      else
        begin
        for k := 1 to l do
          begin
          Lin[k] := Lin[k] / scale;
          h := h + Lin[k]*Lin[k]
          end;
        f := Lin[l];
        g := - Sign(sqrt(h), f);
        e[i] := scale * g;
        h := h - f * g;
        Lin[l] := f - g;
        f := 0.0;
        for j := 1 to l do
          begin
          L0 := Row[j];
          if EgV then
            L0[i] := Lin[j] / h;
          g := 0.0;
          for k := 1 to j do
            g := g + Lin[k] * L0[k];
          for k := j + 1 to l do
            g := g + Row[k].Data[j] * Lin[k];
          e[j] := g / h;
          f := f + e[j] * Lin[j]
          end;
        hh := f / (h + h);
        for j := 1 to l do
          begin
          L0 := Row[j];
          f := Lin[j];
          g := e[j] - hh * f;
          e[j] := g;
          for k := 1 to j do
            L0[k] := L0[k] - f*e[k] - g*Lin[k]
          end
        end
      end
    else
      e[i] := Lin[l];
    d[i] := h
    end;
  if EgV then d[1] := 0.0;
  e[1] := 0.0;
  if EgV then
    for i := 1 to FNCols do begin
      Lin := Row[i];
      l := i-1;
      if d[i] <> 0.0 then begin
        for j := 1 to l do begin
          g := 0.0;
          for k := 1 to l do
            g := g + Lin[k] * Row[k].Data[j];
          for k := 1 to l do begin
            L0 := Row[k];
            L0[j] := L0[j] - g * L0[i]
          end
        end
      end;
      d[i] := Lin[i];
      Lin[i] := 1.0;
      for j := 1 to l do begin
        Lin[j] := 0.0;
        Row[j].Data[i] := 0.0
      end
    end
  else
    for i := 1 to FNCols do d[i] := Row[i].Data[i]
end; { TwsGeneral.Tred2 }

procedure TwsGeneral.Balanc;
  {  Objetivo:
     Substitui uma matriz quadrada por outra balanceada que possui os mesmos
     autovalores. Uma matriz sim?trica j? ? balanceada e n?o ? afetada por este
     procedimento
}
const
  radix = 2.0;
var
  last,j,i   : Integer;
  s,r,g,f,c,
  sqrdx      : Double;
  L0, L1     : TwsVec;
begin
  sqrdx := radix*radix;
  repeat
    last := 1;
    for i := 1 to FNCols do
      begin
      L0 := Row[i];
      c := 0.0;
      r := 0.0;
      for j := 1 to FNCols do
        begin
        L1 := Row[j];
        if j <> i then
          begin
          c := c + Abs(L0[i]);
          r := r + Abs(L0[j])
          end
        end;
      if (c <> 0.0) and (r <> 0.0) then
        begin
        g := r/radix;
        f := 1.0;
        s := c + r;
        while c < g do
          begin
          f := f*radix;
          c := c*sqrdx
          end;
        g := r*radix;
        while c > g do
          begin
          f := f/radix;
          c := c/sqrdx
          end;
        if (c + r)/f < 0.95*s then
          begin
          last := 0;
          g := 1.0/f;
          for j := 1 to FNCols do L0[j] := L0[j]*g;
          for j := 1 to FNCols do
            begin
            L1 := Row[j];
            L1[i] := L1[i]*f
            end
          end
        end
      end; { for }
  until last <> 0
end; { TwsGeneral.Balanc }

procedure TwsGeneral.Hessenb;
  {  Objetivo
     Redu??o da matriz, real e n?o sim?trica, para uma forma de Hessenberg. A matriz original ?
     substitu?da pela forma superior de Hessenberg que possui os mesmos autovalores. ? recomend?vel
     mas n?o obrigat?rio que esta rotina seja precedida da aplica??o da rotina Balanc. Na sa?da, os
      elementos da matriz de hessenberg  ocupam as posi??es (i, j) com i <= j+1. Elementos com
      i > j+1 podem ser considerados como nulos mas de fato estas posi??es conter?o valores sem
      nenhum significado.
}
var
  m,j,i: Integer;
  y,x  : Double;
  L0,L1: TwsVec;
begin
  for m := 2 to FNCols-1 do
    begin
    L0 := Row[m];
    x := 0.0;
    i := m;
    for j := m to FNCols do
      begin
      L1 := Row[j];
      if Abs(L1[m-1]) > Abs(x) then
        begin
        x := L1[m-1];
        i := j
        end
      end;
    L1 := Row[i];
    if i <> m then
      begin
      for j := m-1 to FNCols do
        begin
        y := L1[j];
        L1[j] := L0[j];
        L0[j] := y
        end;
      for j := 1 to FNCols do
        begin
        L1 := Row[j];
        y := L1[i];
        L1[i] := L1[m];
        L1[m] := y
        end
      end;
    if x <> 0.0 then
      begin
      for i := m+1 to FNCols do
        begin
        L1 := Row[i];
        y := L1[m-1];
        if y <> 0.0 then
          begin
          y := y/x;
          L1[m-1] := y;
          for j := m to FNCols do
            L1[j] := L1[j] - y*L0[j];
          for j := 1 to FNCols do
            begin
            L1 := Row[j];
            L1[m] := L1[m] + y*L1[i]
            end
          end
        end
      end
    end
end; { TwsGeneral.Hessenb }

procedure TwsGeneral.LU(var Indx: TwsLIVec; var d: Double; Var ErrCode: Word);
  { Objetivo
    Obt?m a decomposicao LU de uma permuta??o de linhas da matriz quadrada que chama o m?todo.
    Na matriz original retornam os fatores L (mtTriangular inferior) e U (mtTriangular superior).
  Par?metros:
    Indx: Registra as trocas de colunas efetuadas pelo pivoteamento parcial
    d: Retorna 1 se o n?mero de trocas ? impar ou -1 se par. ?til no c?lculo de determinantes.
    ErrCode: C?digo de erro. Retora 0 se a fatora??o for realizada com sucesso ou maInvError
        se a matriz for singular
}
var
  k,j,imax,i : Integer;
  sum,dum,big: Double;
  L,vv       : TwsVec;
begin
  Indx := TwsLIVec.Create(FNCols);
  vv := TwsDFVec.Create(FNCols);
  d := 1.0;
  for i := 1 to FNRows do begin
    L := Row[i];
    big := 0;
    for j := 1 to FNCols do
      if Abs(L[j]) > big then
        big := Abs(L[j]);
    if big = 0.0 then
      begin
      ErrCode := maInvError;
      vv.Free;
      Indx.Free;
      Exit
      end;
    vv[i] := 1.0/big;
  end;
  for j := 1 to FNCols do
    begin
    for i := 1 to j-1 do
      begin
      L := Row[i];
      sum := L[j];
      for k := 1 to i-1 do
        sum := sum - L[k]*Get(k,j);
      L[j] := sum;
      end;
    big := 0.0;
    for i := j to FNRows do
      begin
      L := Row[i];
      sum := L[j];
      for k := 1 to j-1 do
        sum := sum - L[k]*Get(k,j);
      L[j] := sum;
      dum := vv[i]*Abs(sum);
      if dum >= big then
        begin
        big := dum;
        imax := i;
        end;
      end;
    if j <> imax then
      begin { Troca linhas imax e j ? }
      Exchange(imax, j);
      d := -d;
      vv[imax] := vv[j];
      end;
    Indx[j] := imax;
    if FEquals(Get(j,j),0) then
      Put(j,j,eps);
    if j <> FNCols then
      begin
      dum := 1.0/Get(j,j);
      for i := j+1 to FList.Count do
        Put(i,j,Get(i,j)*dum)
      end;
    end;
  vv.Free;
end; { TwsGeneral.LU }

function TwsGeneral.LUDet: Double;
{ Objetivo
    Obtem o determinante da matriz j? fatorada artav?s da decomposicao LU.
}
var
  j   : Integer;
  d   : Double;
  Indx: TwsLIVec;
  Erro: Word;
begin
  LU(Indx, d, Erro);
  for j := 1 to FNCols do d := d*Get(j,j);
  Indx.Free;
  LUDet := d
end; { TwsGeneral.LUDet }

procedure TwsGeneral.MakeGiv(v1, v2: Double; var c, s, r: Double);
{ Constroi a transformacao de Givens. Ver L&H
   Par?metros
    v1:
    v2:
    c:
    s:
    r:

}
var
  q, w: Double;
begin
  if Abs(v1) > Abs(v2) then begin
    w := v2/v1;
    q := Sqrt(1.0 + w*w);
    c := Sign(1.0/q, v1);
    s := w*c;
    r := Abs(v1)*q
  end
  else
    if v2 = 0.0 then begin
      c := 1.0;
      s := 0.0;
      r := 0.0
    end
    else begin
      w := v1/v2;
      q := Sqrt(1.0 + w*w);
      s := Sign(1.0/q, v2);
      c := w*s;
      r := Abs(v2)*q
    end
end; { MakeGiv }

procedure TwsGeneral.AppGiv(c, s: Double; var z1, z2: Double);
{ Objetivo
    Aplica a transforma??o numa matriz ap?s a constru??o da transforma??o
    de Givens. Ver L&H

  Par?metros
    c:
    s:
    z1:
    z2:
}

var
  w: Double;
begin
  w := z1*c + z2*s;
  z2 := -z1*s + z2*c;
  z1 := w
end; { AppGiv }

procedure TwsGeneral.Inv(var ErrCode: Word; eps: Double = 1.0e-8);
{ Objetivo
    Inverte a matriz atrav?s do m?todo de Gauss-Jordan com pivoteamento total.
     Na sa?da a matriz armazena a inversa.
  Par?metros
    ErrCode: C?digo de erro. Retorna 0 para invers?o com sucesso e maInvError se a matriz
      for singular
}
var
  big,dum,pivinv  : Double;
  i,icol,irow,j,
  k,l,ll          : Integer;
  IndxC,IndxR,ipiv: TwsLIVec;
  L0,L1,L2        : TwsVec;
begin
  ErrCode := 0;
  IndxC := TwsLIVec.Create(FNCols);
  IndxR := TwsLIVec.Create(FNCols);
  ipiv := TwsLIVec.Create(FNCols);
  for i := 1 to FNCols do
    ipiv[i] := 0;
  for i := 1 to FNCols do
    begin
    big := 0.0;
    for j := 1 to FNRows do
      begin
      if ipiv[j] <> 1 then
        begin
        L0 := Row[j];
        for k := 1 to FNCols do
          if ipiv[k] = 0 then
            begin
            if Abs(L0[k]) >= big then
              begin
              big := Abs(L0[k]);
              irow := j;
              icol := k
              end
            end
          else
            if ipiv[k] > 1 then
              begin
              ErrCode := maInvError;
              IndxC.Free;
              IndxR.Free;
              ipiv.Free;
              Exit
              end
        end
      end;
    ipiv[icol] := ipiv[icol] + 1;
    L0 := Row[irow];
    L1 := Row[icol];
    if irow <> icol then
      for l := 1 to FNCols do
        begin
        dum := L0[l];
        L0[l] := L1[l];
        L1[l] := dum
        end;
    IndxR[i] := irow;
    IndxC[i] := icol;
    if Abs(L1[icol]) < eps then
      begin
      ErrCode := maInvError;
      if L1[icol] = 0 then
        begin
        IndxC.Free;
        IndxR.Free;
        ipiv.Free;
        Exit
        end
      end;
    pivinv := 1.0/L1[icol];
    L1[icol] := 1.0;
    for l := 1 to FNCols do
      L1[l] := L1[l]*pivinv;
    for ll := 1 to FList.Count do
      if ll <> icol then
        begin
        L2 := Row[ll];
        dum := L2[icol];
        L2[icol] := 0.0;
        for l := 1 to FNCols do
          L2[l] := L2[l] - L1[l]*dum;
        end
    end; { for i }
  for l := FNCols downto 1 do
    if IndxR[l] <> IndxC[l] then
      for k := 1 to FList.Count do
        begin
        L0 := Row[k];
        dum := L0[IndxR[l]];
        L0[IndxR[l]] := L0[IndxC[l]];
        L0[IndxC[l]] := dum
        end;
  IndxC.Free;
  IndxR.Free;
  ipiv.Free
end; { GaussInv }

procedure TwsGeneral.HFTCov(p: Integer);
  { Objetivo
      Ap?s a aplica??o da transforma??o de Householder para uma solu??o de m?nimos
      quadrados, o fator triangular fica armazenado na por??o triangular superior
      esquerda da matriz original. Este procedimento retorna a matriz de covari?ncia
      (sem o fator de escala) da solu??o de m?nimos quadrados.
     Par?metros
       p: Ordem da matriz de covari?ncias, que retorna na por??o p x p superior esquerda
}
var
  i,j,l: Integer;
  s    : Double;
  L0   : TwsVec;
begin
  for i := 1 to p do
    begin                  { A matriz de covariancias }
    L0 := Row[i];
    L0[i] := 1/L0[i]
    end;
  if p > 1 then
    begin
    for i := 1 to p-1 do
      begin
      L0 := Row[i];
      for j := i+1 to p do
        begin
        s := 0;
        for l := i to j-1 do s := s + L0[l]*Get(l,j);
        L0[j] := -Get(j,j)*s
        end
      end
    end;
  for i := 1 to p do
    begin
    L0 := Row[i];
    for j := i to p do
      begin
      s := 0;
      for l := j to p do
        s := s + L0[l]*Get(j,l);
      L0[j] := s
      end
    end
end; { TwsGeneral.HFTCov }

procedure TwsGeneral.HFTICov(P: TwsLIVec);
{  Objetivo
     Apos a aplicacao de HFTISol, determina a matriz de covariancias da solucao de minimos
     quadrados.
   Par?metros:
     P: indica as permutacoes possivelmente realizadas durante o processo.
}
var
  j,i,k,l : Integer;
  s       : Double;
  L0,L1,L2: TwsVec;
begin
  HFTCov(0);   // ==========> arrumar isto aqui
  for i := FNCols downto 1 do begin
    L0 := Row[i];
    if (P[i] <> i) then begin
      k := P[i];
      L1 := Row[k];
      s := L0[i];
      L0[i] := L1[k];
      L1[k] := s;
      if i > 1 then
        for l := 1 to i-1 do begin
          L2 := Row[l];
          L2.Exchange(i,k);
        end;
      if k - i <> 1 then
        for l := i+1 to k-1 do begin
          s := L0[l];
          L0[l] := Row[l].Data[k];
          Row[l].Data[k] := s;
        end;
      if k < FNCols then
        for l := k+1 to FNCols do begin
          s := L0[l];
          L0[l] := L1[l];
          L1[l] := s
        end
    end
  end
end; { TwsGeneral.HFTICov }

procedure TwsGeneral.Solve(B: TwsMatrix; var ErrCode: Word);
{ Objetivo
    Obt?m a solu??o de um sistema de equa??es lineares AX = B, atrav?s da decomposi??o LU, onde
    A ? a matriz que chama o m?todo. Na entrada A armazena os coeficientes e na sa?da armazena
    os fatores da decomposi??o LU.
  Par?metros
    B: Na entrada armazena os vetores do segundo membro e na sa?da armazena em cada coluna a
    solu??o do respectivo sistema
    ErrCode: C?digo de erro que retorna da decomposi??o LU.
}
var
  j,ip,ii,i,jb: Integer;
  sum,d       : Double;
  L0,L1,L2    : TwsVec;
  Indx        : TwsLIVec;
begin
  LU(Indx,d,ErrCode);
  ii := 0;
  for jb := 1 to B.FNCols do
    begin
    for i := 1 to FList.Count do
      begin
      L0 := B.Row[i];
      L2 := Row[i];
      ip := Indx[i];
      L1 := B.Row[ip];
      sum := L1[jb];
      L1[jb] := L0[jb];
      if ii <> 0 then
        for j := ii to i-1 do
          sum := sum - L2[j]*Get(i,jb)
      else
        if sum <> 0.0 then ii := i;
      L0[jb] := sum
      end;
    for i := FList.Count downto 1 do
      begin
      L0 := B.Row[i];
      L1 := Row[i];
      sum := L0[jb];
      for j := i+1 to FNCols do
        sum := sum - L1[j]*Get(j,jb);
      L0[jb] := sum/L1[i]
      end
    end
end; { TwsGeneral.Solve }

procedure TwsGeneral.NormCol(j: Integer);
{ Objetivo
    Normaliza coluna especificada (comprimento unit?rio)
  Par?metros
    j: Coluna a ser normalizada
  Observa??o: Valores perdidos s?o desconsiderados
}
var
  i  : Integer;
  T,x: Double;
begin
  T:=Norm(j,1,NRows);
  for i:=1 to NRows do
    if not IsMissValue(i,j,x) then
      Put(i,j,x/T)
end;

function TwsGeneral.GSBase: TwsGeneral;
{ Objetivo
    Utiliza a matriz como base e constroi uma base ortonormal utilizando o m?todo de Gram-
    Schmidt
  Par?metros
    NewMat: False (default) substitui o conte?do atual pela base ortonormal; True a base
    ortonormal ? alocada em uma nova matriz
  Observa??es: Desconsidera os valores perdidos
}
var
  i,j,k: Integer;
  T,x,y: Double;
begin
  Result:=TwsGeneral.Create(NRows,NCols);
  for k:=1 to NCols do
    begin
    for i:=1 to NRows do         // Copia coluna k
      Result[i,k]:=Get(i,k);
    for j:=1 to k-1 do
      begin
      T:=0;
      for i:=1 to nRows do       // Soma de produtos das colunas
        if not (isMissValue(i,k,x) and Result.isMissValue(i,j,y)) then
          T:=T+x*y;
      for i:=1 to NRows do       // Ortogonaliza coluna k
        Result[i,k]:=Result[i,k]-T*Result[i,j]
      end;
    Result.NormCol(k);           // e normaliza
    end;
end; // GSBase

function TwsGeneral.MGSBase(NewMat: Boolean=False): TwsGeneral;
{ Objetivo
    Utiliza a matriz como base e constroi uma base ortonormal utilizando o m?todo
    modificado de Gram-Schmidt
  Par?metros
    NewMat: False (default) substitui o conte?do atual pela base ortonormal; True a base
    ortonormal ? alocada em uma nova matriz
  Observa??o: Valores perdidos s?o desconsiderados
}
var
  i,j,k: Integer;
  t,x,y: Double;
begin
  if NewMat then
    Copy(mtGeneral,TwsMatrix(Result))
  else
    Result:=Self;
  for k:=1 to NCols do            // Para cada coluna da matriz
    begin
    Result.NormCol(k);            // Normaliza coluna j
    for j:=k+1 to NCols do        // Subtrai das colunas restantes
      begin                       // Produto escalar das colunas j e k
      t:=Result.ColProd(k,j,1,NRows);
      for i:=1 to NRows do
        if (not Result.IsMissValue(i,j,x)) and (not Result.IsMissValue(i,k,y)) then
          Result[i,j]:=x-t*y
      end;
    end;
end;

function TwsGeneral.HProjX: TwsSymmetric;
{ Objetivo
    Constroi matriz de proje??o ortogonal. A matriz deve ser de posto coluna completo
  Observa??o: Matriz n?o pode ter valores perdidos
}
var
  S  : TwsSymmetric;
  Err: Word;
begin
  S:=TwsSymmetric(TranspMul4);
  S.Inv(Err);
  if Err=0 then
    Result:=TwsSymmetric(S.TranspMul10(Self,Err))
  else
    Result:=nil;
  S.Free
end;

function TwsGeneral.QProjX: TwsSymmetric;
{ Objetivo
    Constroi matriz de proje??o sobre o complemento ortogonal
  Observa??o: Matriz n?o pode ter valores perdidos
}
var
  i,j: Integer;
begin
  Result:=HProjX;
  if Result<> nil then
    for i:=1 to NRows do
      for j:=1 to i do
        if i=j then
          Result[i,j]:=1-Result[i,j]   // Na diagonal
        else
          Result[i,j]:=-Result[i,j]    // fora da diagonal
end;


procedure TwsGeneral.Pearce(var C,R: TwsGeneral; var CBal,RBal,Orth: boolean);
{ Objetivo
    Procedimento que aplica condicao de Pearce as linhas e colunas
  Par?metros
    C: Matriz que retorna os valores das somas da concorr?ncias ponderadas
       relativas as colunas.
    R: Matriz que retorna os valores das somas das concorr?ncias ponderadas
       relativas as linhas
}
var
  i,j,k,l: Integer;
  tc,tr  : TwsVec;
  a,g    : Double;
begin
  // Verifica balanceamento em relacao as colunas
  k := Trunc(FnCols*(FnCols-1)/2);  // num. de combinacoes
  C:=TwsGeneral.Create(1,k);
  C.Name:='Pearce_Col';
  C.MLab:='Condi??o de Pearce aplicada ?s colunas';
  C.RowName[1]:='Val_Conc';
  tr:=TwsDFVec.Create(FnRows);
  // Obtem os totais de linha
  g:=0;
  for i:=1 to FnRows do
    begin
    a:=0;
    for j:=1 to FnCols do
      a:=a+Data[i,j];
    tr[i]:=a;
    g:=g+a;                  // total geral
    end;
  // Obtem os valores das concorrencias
  l:=1;
  for i:=1 to FnCols-1 do
    begin
    for j:=i+1 to FnCols do
      begin
      a:=0;
      for k:=1 to FnRows do
        a:=a+(Data[k,i]*Data[k,j])/tr[k];
      C[1,l]:=a;
      C.ColName[l]:=ColName[i]+'.'+ColName[j];
      Inc(l)
      end;
    end;
  CBal:=True;
  i:=2;
  a:=C[1,1];
  while CBal and (i<=C.nCols) do
    begin
    CBal:=FEquals(a,C[1,i]);
    Inc(i)
    end;

  // Verifica balanceamento em relacao as linhas
  k := Trunc(FnRows*(FnRows-1)/2);  // num. de combinacoes
  R:=TwsGeneral.Create(1,k);
  R.Name:='Pearce_Lin';
  R.MLab:='Condi??o de Pearce aplicada ?s linhas';
  R.RowName[1]:='Val_Conc';
  tc:=TwsDFVec.Create(FnCols);
  // Obtem os totais de coluna
  for i:=1 to FnCols do
    begin
    a:=0;
    for j:=1 to FnRows do
      a:=a+Data[j,i];
    tc[i]:=a
    end;
  // Obtem os valores das concorrencias
  l:=1;
  for i:=1 to FnRows-1 do
    begin
    for j:=i+1 to FnRows do
      begin
      a:=0;
      for k:=1 to FnCols do
        a:=a+(Data[i,k]*Data[j,k])/tc[k];
      R[1,l]:=a;
      R.ColName[l]:=RowName[i]+'.'+RowName[j];
      Inc(l)
      end;
    end;
  RBal:=True;
  i:=2;
  a:=R[1,1];
  while RBal and (i<=R.nCols) do
    begin
    RBal:=FEquals(a,R[1,i]);
    Inc(i)
    end;
  // Verifica se a classificacao eh ortogonal
  Orth:=True;
  for i:=1 to FnRows do
    begin
    for j:=1 to FnCols do
      begin
      Orth:=FEquals(Data[i,j],tr[i]*tc[j]/g);
      if not Orth then
        Break
      end;
      if not Orth then
        Break
    end;
  tr.Free;
  tc.Free;
end;

procedure TwsGeneral.G2Sweep(k: Integer; DMin: Double=1E-8);
  { Objetivo
      Aplica sweep numa matriz sim?trica armazenada como geral
    Par?metros
      k: coluna para aplica??o do sweep
      DMin: toler?ncia ara aplica??o do sweep
}
var
  i,j: Integer;
  b,d: Double;
begin
  // Passo 1:
  d := Get(k,k);
  if (d < DMin) then
    begin
    // Zera linha e coluna k
    for i:=1 to FNRows do
      begin
      Put(k,i,0);
      Put(i,k,0);
      end;
    Exit;
    end;

  // Passo 2: Divide a linha k por d
  for i:=1 to FNRows do
    Put(k,i,Get(k,i)/d);

  // Passo 3:
  for i:=1 to FNRows do
    begin
    if i <> k then
      begin
      b:=Get(i,k);
      for j := 1 to FNCols do
        begin
        Put(i,j,Get(i,j)-b*Get(k,j));
        Put(i,k,-b/d)
        end
      end
    end;
  Put(k,k,1/d);
end; { TwsGeneral.G2Sweep }

procedure TwsGeneral.MGS(var T: TwsTriangular; var ErrCode: Word);
{ Objetivo
    Obt?m a decomposi??o de uma matriz (A=QT) pelo m?todo modificado de Gram-Schmidt
  Par?metros
    T      : Retorna a matriz mtTriangular
    ErrCode: C?digo de erro. retorna 0 se o processamento foi normal e NSingMat se
      a matriz ? singular
  Retorno
    A matriz que chama o m?todo ser? fatorada e substitu?da pelo fator Q
}
var
  i,j,k  : Integer;
  z,y,x,b: Double;
begin
  ErrCode:=0;
  T:=TwsTriangular.Create(NCols);
  for i:=1 to NCols do
    begin
    x:=0;
    for j:=1 to NRows do
      begin
      y:=Get(j,i);
      x:=x+y*y
      end;
    if x<=1.0e-8 then
      begin
      T.Free;
      ErrCode:=NSingMat;
      Exit
      end;
    b:=Sqrt(x);
    if i<NCols then
      begin
      for k:=i+1 to NCols do
        begin
        y:=0;
        for j:=1 to NRows do
          y:=y+Get(j,i)*Get(j,k);
        z:=y/x;
        T[k,i]:=y/b;
        for j:=1 to NRows do
          Put(j,k,Get(j,k)-z*Get(j,i))
        end
      end;
    T[i,i]:=b;
    for j:=1 to NRows do
      Put(j,i,Get(j,i)/b);
    end; // for i
end;

procedure TwsGeneral.WMGS(D: TwsDiagonal; var T: TwsTriangular; var ErrCode: Word);
{ Objetivo
    Obt?m a decomposi??o de uma matriz (A=QT), onde Q ? ortonormal e T ? mtTriangular, pelo
    m?todo modificado de Gram-Schmidt na m?trica estabelecida pela matriz D
  Par?metros
    D      : Matriz mtDiagonal contendo a m?trica para decomposi??o
    T      : Matriz mtTriangular da decomposi??o
    ErrCode: C?digo de erro. Retorna 0 se o retorno ? feito sem problemas e NSingMat se a
    matriz for singular
  Retorno
    Em A retorna o fator Q
}
var
  i,j,k  : Integer;
  z,y,x,b: Double;
  v      : TwsVec;
begin
  ErrCode:=0;
  if D.NCols<>NRows then
    begin
    ErrCode:=NImprDim;
    Exit
    end;
  T:=TwsTriangular.Create(NCols);
  v:=D.Row[1];
  for i:=1 to NCols do
    begin
    x:=0;
    for j:=1 to NRows do
      begin
      y:=Get(j,i);
      x:=x+y*y*v[j]
      end;
    if x<=1.0e-8 then
      begin
      T.Free;
      ErrCode:=NSingMat;
      Exit
      end;
    b:=Sqrt(x);
    if i<NCols then
      begin
      for k:=i+1 to NCols do
        begin
        y:=0;
        for j:=1 to NRows do
          y:=y+Get(j,i)*Get(j,k)*v[j];
        z:=y/x;
        T[k,i]:=y/b;
        for j:=1 to NRows do
          Put(j,k,Get(j,k)-z*Get(j,i))
        end
      end;
    T[i,i]:=b;
    for j:=1 to NRows do
      Put(j,i,Get(j,i)/b);
    end; // for i
end;

PROCEDURE TwsGeneral.SVDCmp(VAR D: TwsDiagonal; var V:TwsGeneral; var Rank: Integer;
  VAR ErrCode: word; eps: Double = 1.0e-8);
{ Objetivo
    Obter a decomposi??o por valores singulares (A=UDV') de uma matriz
  Par?metros
    D: Matriz que retorna os valores singulares
    V: Matriz da decomposi??o
    ErrCode: C?digo de erro. Retorna NIterMax se o processo iterativo superar 30 itera??es;
    0 caso contr?rio
  Retorno
    A matriz U retorna na pr?pria matriz que chama o m?todo. Na sa?da todas as matrizes
    envolvidas s?o ordenadas em fun??o dos valores singulares, ou seja, os valores
    singulares s?o ordenados em ordem descendente e as colunas das demais matrizes s?o
    reorganizadas para se ajustar a essa ordena??o
}
LABEL 1,2,3;

VAR
   n,m,nm,l,k,j,
   jj,its,i,mnmim: Integer;
   z,y,x,scale,s,
   h,g,f,c,anorm : Double;
   rv1           : PFArray;
   w             : TwsVec;
   Indx          : TwsLIVec;
{
   FUNCTION sign(a,b: Double): Double;
   BEGIN
      IF (b >= 0.0) THEN Result := abs(a) ELSE Result := -abs(a)
   END;
}
   FUNCTION Pythag(a, b: Double): Double;
   VAR
     at, bt: Double;
   BEGIN
     at := Abs(a);
     bt := Abs(b);
     IF at > bt THEN
       Result := at*Sqrt(1.0 + Sqr(bt/at))
     ELSE
       IF bt = 0.0 THEN
         Result := 0.0
       ELSE
         Result := bt*Sqrt(1.0 + Sqr(at/bt))
   END; { Pythag }

BEGIN
   ErrCode := 0;
   m := NRows;
   n := NCols;
   w := TwsDFVec.Create(N);
   GetMem(Rv1, SF(N));
   v := TwsGeneral.Create(N,N);
   g := 0.0;
   scale := 0.0;
   anorm := 0.0;
   FOR i := 1 TO n DO
      BEGIN
      l := i+1;
      rv1^[i] := scale*g;
      g := 0.0;
      s := 0.0;
      scale := 0.0;
      IF (i <= m) THEN
         BEGIN
         FOR k := i TO m DO
           scale := scale+Abs(Get(k,i));
         IF (scale <> 0.0) THEN
            BEGIN
            FOR k := i TO m DO
               BEGIN
               Put(k,i,Get(k,i)/scale);
               s := s+Get(k,i)*Get(k,i)
               END;
            f := Get(i,i);
            g := -sign(sqrt(s),f);
            h := f*g-s;
            Put(i,i,f-g);
            FOR j := l TO n DO
               BEGIN
               s := 0.0;
               FOR k := i TO m DO
                 s := s+Get(k,i)*Get(k,j);
               f := s/h;
               FOR k := i TO m DO
                 Put(k,j,Get(k,j)+f*Get(k,i));
               END;
            FOR k := i TO m DO
              Put(k,i,scale*Get(k,i));
            END;
         END;
      w[i] := scale*g;
      g := 0.0;
      s := 0.0;
      scale := 0.0;
      IF (i <= m) AND (i <> n) THEN
         BEGIN
         FOR k := l TO n DO
           scale := scale+abs(Get(i,k));
         IF (scale <> 0.0) THEN
            BEGIN
            FOR k := l TO n DO
               BEGIN
               Put(i,k,Get(i,k)/scale);
               s := s+Get(i,k)*Get(i,k)
               END;
            f := Get(i,l);
            g := -sign(sqrt(s),f);
            h := f*g-s;
            Put(i,l,f-g);
            FOR k := l TO n DO
              rv1^[k] := Get(i,k)/h;
            FOR j := l TO m DO
               BEGIN
               s := 0.0;
               FOR k := l TO n DO
                 s := s+Get(j,k)*Get(i,k);
               FOR k := l TO n DO
                 Put(j,k,Get(j,k)+s*rv1^[k]);
               END;
            FOR k := l TO n DO
              Put(i,k,scale*Get(i,k));
            END;
         END;
      anorm := max(anorm,(abs(w[i])+abs(rv1^[i])));
      END;

   FOR i := n DOWNTO 1 DO
      BEGIN
      IF (i < n) THEN
         BEGIN
         IF (g <> 0.0) THEN
            BEGIN
            FOR j := l TO n DO
              v[j,i] := (Get(i,j)/Get(i,l))/g ;
            FOR j := l TO n DO
               BEGIN
               s := 0.0;
               FOR k := l TO n DO
                 s := s+Get(i,k)*v[k,j];
               FOR k := l TO n DO
                 v[k,j] := v[k,j]+s*v[k,i];
               END;
            END;
         FOR j := l TO n DO
            BEGIN
            v[i,j] := 0.0;
            v[j,i] := 0.0;
            END
         END;
      v[i,i] := 1.0;
      g := rv1^[i];
      l := i
      END;

   mnmim := Math.Min(m,n);
   FOR i := mnmim DOWNTO 1 DO
      BEGIN
      l := i+1;
      g := w[i];
      FOR j := l TO n DO Put(i,j,0);
      IF (g <> 0.0) THEN
         BEGIN
         g := 1.0/g;
         FOR j := l TO n DO
            BEGIN
            s := 0.0;
            FOR k := l TO m DO
              s := s+Get(k,i)*Get(k,j);
            f := (s/Get(i,i))*g;
            FOR k := i TO m DO
              Put(k,j,Get(k,j)+f*Get(k,i));
            END;
         FOR j := i TO m DO
           Put(j,i,Get(j,i)*g);
         END
      ELSE
         FOR j := i TO m DO
           Put(j,i,0);

      Put(i,i,Get(i,i)+1.0)
      END;

   FOR k := n DOWNTO 1 DO
      BEGIN
      FOR its := 1 TO 30 DO
         BEGIN
         FOR l := k DOWNTO 1 DO
            BEGIN
            nm := l-1;
            IF ((abs(rv1^[l])+anorm) = anorm) THEN GOTO 2;
            IF nm > 0 THEN
               IF ((abs(w[nm])+anorm) = anorm) THEN GOTO 1;
            END;

1:       c := 0.0;
         s := 1.0;
         FOR i := l TO k DO
            BEGIN
            f := s*rv1^[i];
            rv1^[i] := c * rv1^[i];

            IF (abs(f)+anorm) = anorm THEN GOTO 2;
            g := w[i];
            h := pythag(f,g);
            w[i] := h;
            h := 1.0/h;
            c := (g*h);
            s := -(f*h);
            FOR j := 1 TO m DO
               BEGIN
               y := Get(j,nm);
               z := Get(j,i);
               Put(j,nm,(y*c)+(z*s));
               Put(j,i, -(y*s)+(z*c))
               END
            END;

2:       z := w[k];
         IF (l = k) THEN
            BEGIN
            IF (z < 0.0) THEN
               BEGIN
               w[k] := -z;
               FOR j := 1 TO n DO
                 v[j,k] := -v[j,k];
               END;
            GOTO 3
            END;
         IF (its = 30) THEN
            BEGIN
            ErrCode := NIterMax;
            FreeMem(rv1, SF(N));
            Exit;
            END;
         x := w[l];
         nm := k-1;
         y := w[nm];
         g := rv1^[nm];
         h := rv1^[k];
         f := ((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
         g := pythag(f,1.0);
         f := ((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x;
         c := 1.0;
         s := 1.0;
         FOR j := l TO nm DO
            BEGIN
            i := j+1;
            g := rv1^[i];
            y := w[i];
            h := s*g;
            g := c*g;
            z := pythag(f,h);
            rv1^[j] := z;
            c := f/z;
            s := h/z;
            f := (x*c)+(g*s);
            g := -(x*s)+(g*c);
            h := y*s;
            y := y*c;
            FOR jj := 1 TO n DO
               BEGIN
               x := v[jj,j];
               z := v[jj,i];
               v[jj,j] := (x*c)+(z*s);
               v[jj,i] := -(x*s)+(z*c)
               END;
            z := pythag(f,h);
            w[j] := z;
            IF (z <> 0.0) THEN
               BEGIN
               z := 1.0/z;
               c := f*z;
               s := h*z
               END;
            f := (c*g)+(s*y);
            x := -(s*g)+(c*y);
            FOR jj := 1 TO m DO
               BEGIN
               y := Get(jj,j);
               z := Get(jj,i);
               Put(jj,j,(y*c)+(z*s));
               Put(jj,i,-(y*s)+(z*c))
               END
            END;
         rv1^[l] := 0.0;
         rv1^[k] := f;
         w[k] := x
         END;
3:    END;
  Indx := W.QuickIndx(False);

  // Obtem o posto da matriz W cujos valores principais ja estao ordenados
  Rank:=w.len;
  // w[1] e o maior autovalor
  x:=eps*w[1];
  while (w[Rank]<x) and (Rank>0) do
    Dec(Rank);
  { Faz ordenacao correspondente de V e de U }
  V.SortCol(Indx);
  SortCol(Indx);
  Indx.Free;

  D := VecToDiag(w);
  FreeMem(Rv1, SF(N));
END;

function TwsGeneral.MoorePenrose: TwsGeneral;
{ Objetivo
    Obt?m a inversa (generalizada) de Moore Penrose de uma matriz atrav?s da decomposi??o
    por valores singulares.
  Retorno
    Para a matriz A(m x n), a chamada A.MoorePenrose ir? retornar a matriz B(n x m) tal
    que ABA=A. A matriz A n?o ? alterada no processo. Se ocorrer algum problema na SVD
    a fun??o retorna nil
}
var
  i,j,k,Rank: Integer;
  aux       : Double;
  W         : TwsDiagonal;
  Vec       : TwsVec;
  V,mC      : TwsGeneral;
  ErrCode   : Word;
begin
  Copy(mtGeneral,TwsMatrix(mC));
  mC.SVDcmp(W,V,Rank,ErrCode);
  Vec:=W.Row[1];
  if ErrCode = 0 then
    begin
    Result := TwsGeneral.Create(mC.NCols,mC.NRows);
    for i:=1 to V.NRows do
      for j:=1 to mC.NRows do
        begin
        aux:=0;
        for k:=1 to Rank do
          aux:=aux+V[i,k]*mC[j,k]/Vec[k];
        Result[i,j]:=aux;
        end;
    V.Free;
    W.Free;
    mC.Free
    end
  else
    Result:=nil
end; { MoorePenrose }

function TwsGeneral.LocateAtRow(x:Double;Lin:Integer;Out j:Integer;Asc:Boolean=True):Boolean;
{ Objetivo
    Obt?m os ?ndices das colunas entre as quais est? um valor especificado.
  Par?metros
    x  : valor a ser pesquisado
    Lin: Indica em rela??o a que linha ser? feita a pesquisa
    j  : Retorna um valor tal que x estar? entre xx[Lin,j] e xx[Lin,j+1].
         Se n?o existe nenhuma linha j retorna 0;
         Se o valor existe na linha indicada, j retorna o seu ?ndice.
  Retorno
   A funcao retorna True se o valor x esta na tabela e false caso contrario.
}
var
  ju,jm,jl: Integer;
Begin
  if NRows = 0 then
     begin
     Result := False;
     j := 0;
     exit;
     end;
  jl := 0;
  ju := NCols + 1;
  While ju-jl > 1 do
    Begin
    jm := (ju + jl) div 2;
    If (x >= Get(Lin,jm)) = Asc Then
       jl := jm
    Else
       ju := jm
    End;
  if FEquals(Get(Lin, 1),x) then
     j := 1
  else
     begin
     if FEquals(Get(Lin,NCols),x) then
        j := NCols
     else
        j := jl
     end;
  if ((j>0) and (j<=NCols)) then
    Result := FEquals(Get(Lin,j),x);
End;

function TwsGeneral.LocateAtCol(x:Double;Col:Integer;Out j:Integer;Asc:Boolean=True):Boolean;
{ Objetivo
    Obt?m os ?ndices das linhas entre as quais est? um valor especificado.
  Par?metros
    x  : valor a ser pesquisado
    Col: Indica em rela??o a que coluna ser? feita a pesquisa
    j  : Retorna um valor j tal que x estar? entre xx[j,Col] e xx[j+1,Col].
         Se n?o existir nenhuma linha na matriz j retorna 0
         Se o valor de x existir na coluna indicada, j retorna o ?ndice da linha onde est?
         esse valor.
  Retorno
   A funcao retorna True se o valor x esta na tabela e false caso contrario.
}
var
  ju,jm,jl: Integer;
Begin
  if NRows = 0 then
     begin
     Result := False;
     j := 0;
     exit;
     end;
  jl := 0;
  ju := NRows + 1;
  While ju-jl > 1 do
    Begin
    jm := (ju + jl) div 2;
    If (x >= Get(jm,Col)) = Asc Then
       jl := jm
    Else
       ju := jm
    End;
  if FEquals(Get(1, Col),x) then
     j := 1
  else
     begin
     if FEquals(Get(NRows,Col),x) then
        j := NRows
     else
        j := jl
     end;
  if ((j>0) and (j<=NRows)) then
    Result := FEquals(Get(j,Col), x);
End;

{$ifdef WINSTAT_FULL}
function TwsGeneral.Correlations(C: TwsLIVec; var Stat:TwsGeneral; BStat: Boolean;
  var CTest: TwsGeneral): TwsSymmetric;
{ Objetivo
    Dada uma matriz geral, retorna as correla??es (de Pearson) entre as vari?veis
    correspondentes ?s coluna especificadas, as estat?sticas simples para essas vari?veis
    e os testes de hip?teses para as correla??es
  Par?metros
    C    : ?ndices das colunas para determina??o das correla??es
    Stat : Matriz com as estat?sticas simples.
    BStat: Se True, retorna a matriz de estat?sticas em Stat; caso contr?rio Stat retorna
           nil
    Corr : Matriz de correla??es
  Valores perdidos
    Toda observa??o que possuir algum valor perdido em pelo menos uma das vari?veis
    especificada ? eliminada
 }
var
  Mean : TwsVec;
  n,i,j: Integer;
  Erro : Word;
  Col  : TwsDataSetCol;
  sz,z : Double;
begin
  // Obtem produtos matriz dos cruzados
  Result:=AdjCrossProd(C,Mean,n);
  // Matriz das estatisticas
  if BStat then
     begin
     Stat := TwsGeneral.Create(C.Len, 4);
     Stat.Name := 'Estat_' + System.Copy(Name,1 ,4);
     Stat.MLab := 'Estat?sticas descritivas simples';
     Stat.ColName[1] := 'Media'; Stat.ColName[2] := 'Variancia';
     Stat.ColName[3] := 'Desv_Pad'; Stat.ColName[4] := 'Coef_Var';
     for i:= 1 to C.Len do
       begin
       Stat.RowName[i] := ColName[C[i]];
       Stat[i,1] := Mean[i]; Stat[i,2] := ScalarDiv(Result[i,i],n-1);
       Stat[i,3] := ScalarSqrt(Stat[i,2]); Stat[i,4] := ScalarDiv(Stat[i,3],Mean[i]);
       if not FEquals(Stat[i,4],wscMissValue) then
         Stat[1,4]:=Stat[i,4]*100;
       end;
     end
  else
    Stat:=nil;
  for i:= 1 to C.Len do begin
    Result.ColName[i]:=ColName[C[i]];
    Result.RowName[i]:=Result.ColName[i];
  end;
  Mean.Free;
  // Transforma matriz dos produtos cruzados em matriz de correlacoes
  Result.CorrMat;
  Result.Name:='Corr_Mat';
  Result.MLab:='Matriz de correla?oes simples';
  if n<=3 then
    begin
    CTest:=nil;
    Exit
    end;
  // Testes de hip?teses para as correla??es
  CTest := TwsDataSet.Create('Corr_Test');
  with TwsDataSet(CTest) do
    begin
    MLab:='Teste de signific?ncia para os coeficientes de correla??o';
    Col:=TwsQualitative.Create('Var_I','Vari?vel I');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 1. Nome da primeira vari?vel }

    Col:=TwsQualitative.Create('Var_J','Vari?vel J');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 2. Nome da segunda vari?vel }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correla??o de Pearson',10,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determina??o',10,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estat?stica T para hip?tese H0',12,7));     {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hip?tese H0',10,5));     {6}

    Col:=TwsQualitative.Create('Sig','Signific?ncia do teste ao n?vel 5% ou 1%',6);
    With TwsQualitative(Col) Do
      Begin
      AddLevel('NS');
      AddLevel('5%');
      AddLevel('1%');
      End; { With Col }
    Struct.AddColEx(Col);                                                              {7}
    Struct.AddColEx(TwsNumeric.Create('Extr_Inf','Extremo inferior do intervalo 95%',10,5));     {8}
    Struct.AddColEx(TwsNumeric.Create('Extr_Sup','Extremo superior do intervalo 95%',10,5));     {9}
    end; // with CTest

  sz:=1.96/Sqrt(n-3);
  for i:=2 to Result.NRows do
    for j:=1 to i-1 do
      begin
      Mean:=TwsDFVec.Create(9);                  // Aproveita a mesma variavel
      Mean[1]:=i-1;                              // Indice da variavel i
      Mean[2]:=j-1;                              // Indice da variavel j
      Mean[3]:=Result[i,j];                      // Correlacao
      Mean[4]:=Sqr(Mean[3]);                     // Determinacao
      Mean[5]:=Mean[3]*Sqrt((n-2)/(1-Mean[4]));  // Estatistica t
      Mean[6]:=TInt(Mean[5],n-2,False,True,Erro);
      If Mean[6]>=0.05 Then                      // Verifica significancia
        Mean[7]:=0
      Else
        If Mean[6]>=0.01 Then
          Mean[7]:=1
        Else
          Mean[7]:=2;
      z:=0.5*Ln((1+Mean[3])/(1-Mean[3]));      // transformacao de fisher
      Mean[8]:=z-sz;                  // extremo inferior na escala z
      Mean[9]:=z+sz;                  // extremo superior na escala z
      Mean[8]:=(exp(2*Mean[8])-1)/(exp(2*Mean[8])+1);  // inf na escala r
      Mean[9]:=(exp(2*Mean[9])-1)/(exp(2*Mean[9])+1);  // sup na escala r
      CTest.MAdd(Mean)
      end;
end; { Correlations }

function TwsGeneral.WCorrelations(C: TwsLIVec; var Stat:TwsGeneral; BStat: Boolean;
  WInd: Integer; var CTest: TwsGeneral): TwsSymmetric;
{ Objetivo
    Retorna uma matriz sim?trica onde os componentes s?o os coeficientes de correla??o
    simples ponderados entre as colunas especificadas
  Par?metros
    C: Vetor com os ?ndices das colunas para as quais se deseja as correla??es
    Stat: Matriz das estat?sticas simples para as vari?veis
    BStat: se True retorna a matriz de estat?sticas; retorna nil caso contr?rio
    WInd: ?ndice da vari?vel peso
  Valores perdidos
    A observa??o que possuir algum valor perdido (para as vari?veis indicadas ou para a
      vari?vel peso) s?o eliminadas do processo
}
var
  Mean : TwsVec;
  n,i,j: Integer;
  w    : Double;
  Col  : TwsDataSetCol;
  Erro : Word;
begin
  if nRows<=3 then
    begin
    Result:=nil;
    CTest:=nil;
    Stat:=nil;
    Exit
    end;

  Result:=WAdjCrossProd(C,Mean,n,w,WInd);
  for i:= 1 to C.Len do
    begin
    Result.RowName[i] := ColName[C[i]];
    Result.ColName[i] := Result.RowName[i];
    end;
  {opcoes}
  if BStat Then
    begin
    Stat := TwsGeneral.Create(C.Len, 4);
    Stat.Name := 'Estat_' + System.Copy(Name,1,4);
    Stat.MLab := 'Estat?sticas descritivas simples';
    Stat.ColName[1] := 'Media'; Stat.ColName[2] := 'Variancia';
    Stat.ColName[3] := 'Desv_Pad'; Stat.ColName[4] := 'Coef_Var';
    for i:= 1 to C.Len do
      begin
      Stat.RowName[i] := ColName[C[i]];
      Stat[i,1] := Mean[i]; Stat[i,2] := Result[i,i]/(w-1);
      Stat[i,3] := Sqrt(Stat[i,2]); Stat[i,4] := Stat[i,3]/Mean[i];
      end;
    end;
  Mean.Free;

  Result.CorrMat;
  Result.Name := 'WCorr_Mat';
  Result.MLab := 'Matriz de correla??es de Pearson / Peso: '+ColName[WInd];
  CTest := TwsDataSet.Create('WCorr_Test');
  with TwsDataSet(CTest) do
    begin
    MLab:='Teste de signific?ncia para os coeficientes de correla??o';
    Col:=TwsQualitative.Create('Var_I','Vari?vel I');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 1. Nome da primeira vari?vel }

    Col:=TwsQualitative.Create('Var_J','Vari?vel J');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 2. Nome da segunda vari?vel }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correla??o de Pearson',11,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determina??o',11,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estat?stica T para hip?tese H0',12,7));     {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hip?tese H0',11,5));     {6}

    Col:=TwsQualitative.Create('Sig','Signific?ncia do teste ao n?evl 5% ou 1%',6);
    With TwsQualitative(Col) Do
      Begin
      AddLevel('NS');
      AddLevel('5%');
      AddLevel('1%');
      End; { With Col }
    Struct.AddColEx(Col);                                                              {7}
    end; // with CTest

  for i:=2 to Result.NRows do
    for j:=1 to i-1 do
      begin
      Mean:=TwsDFVec.Create(7);                  // Aproveita a mesma variavel
      Mean[1]:=i-1;                              // Indice da variavel i
      Mean[2]:=j-1;                              // Indice da variavel j
      Mean[3]:=Result[i,j];                      // Correlacao
      Mean[4]:=Sqr(Mean[3]);                     // Determinacao
      Mean[5]:=Mean[3]*Sqrt((n-2)/(1-Mean[4]));  // Estatistica t
      Mean[6]:=TInt(Mean[5],n-2,False,True,Erro);
      If Mean[6]>=0.05 Then                      // Verifica significancia
        Mean[7]:=0
      Else
        If Mean[6]>=0.01 Then
          Mean[7]:=1
        Else
          Mean[7]:=2;
      CTest.MAdd(Mean)
      end;
end; { WCorrelations }

function TwsGeneral.PartialCorrel(C, CAdj: TwsLIVec; var Stat: TwsGeneral; BStat:Boolean;
  var CTest: TwsGeneral): TwsSymmetric;
{ Objetivo
    Retorna uma matriz sim?trica onde os componentes s?o os coeficientes de correla??o
    parciais entre as colunas especificadas
  Par?metros
    C: Vetor com os ?ndices das colunas para as quais se deseja as correla??es
    CAdj: Colunas para as quais se deseja calcular as correla??es parciais
    Stat: Matriz de estat?sticas descritivas das colunas especificadas em C
    BStat: Se True obt?m matriz das estat?sticas descritivas, se False, Stat retorna nil
    CTest: Retorna um conjunto de dados com os resultados dos testes realizados
}
var
  Col, ColOp   : TwsLIVec;
  Mean,Aux     : TwsVec;
  SP           : TwsSymmetric;
  i, nvtot, n,
  nvfix, nv, j : Integer;
  DSCol        : TwsDataSetCol;
  Erro         : Word;
begin
  // Observacoes suficientes para analise?
  if (nRows-2) <= CAdj.Len then
    begin
    Result:=nil;
    Stat:=nil;
    CTest:=nil;
    Exit
    end;
  Col := iVecAppend(C, CAdj, True, CAdj.Len);
  nv:=C.Len; nvfix:=CAdj.Len; nvtot:=Col.Len;

  SP:=AdjCrossProd(Col,Mean,n);

  for i:= 1 to Col.Len do
    begin
    SP.RowName[i] := ColName[Col[i]];
    SP.ColName[i] := SP.RowName[i];
    end;

  SP.MLab := 'Matriz de produtos cruzados ajustados para m?dia';

  ColOp := TwsLIVec.Create(nvtot);
  for i := 1 to nvtot do ColOp[i] := 1;

  Aux := TwsDFVec.Create(nvfix);
  for i := 1 to nvfix do
    Aux[i] := SP[nv+i,nv+i]*1.0e-8; { Toler?ncias }   //dah erro aqui

  for i := 1 to nvfix do
    SP.RevSweep(nv+i, Aux[i], ColOp);

  if BStat then
    begin
    Stat := TwsGeneral.Create(C.Len, 4);
    Stat.Name := 'Estat_' + System.Copy(Name,1 ,4);
    Stat.MLab := 'Estat?sticas descritivas simples';
    Stat.ColName[1] := 'Media'; Stat.ColName[2] := 'Variancia';
    Stat.ColName[3] := 'Desv_Pad'; Stat.ColName[4] := 'Coef_Var';
    for i:= 1 to C.Len do
      begin
      Stat.RowName[i] := ColName[C[i]];
      Stat[i,1] := Mean[i]; Stat[i,2] := sp[i,i]/(n-1-CAdj.Len);
      Stat[i,3] := ScalarSqrt(Stat[i,2]);
      Stat[i,4] := ScalarProd(ScalarDiv(Stat[i,3],Mean[i]),100);
      end;
    End;

  Col.Free; ColOp.Free; Mean.Free; Aux.Free;

  Result := TwsSymmetric(SP.SubSymMat(1, nv));

  SP.Free;
  Result.CorrMat;

  CTest := TwsDataSet.Create('WCorr_Test');
  with TwsDataSet(CTest) do
    begin
    MLab:='Teste de signific?ncia para os coeficientes de correla??o';
    DSCol:=TwsQualitative.Create('Var_I','Vari?vel I');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 1. Nome da primeira vari?vel }

    DSCol:=TwsQualitative.Create('Var_J','Vari?vel J');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 2. Nome da segunda vari?vel }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correla??o de Pearson',10,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determina??o',10,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estat?stica T para hip?tese H0',12,7));     {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hip?tese H0',10,5));     {6}

    DSCol:=TwsQualitative.Create('Sig','Signific?ncia do teste ao n?vel 5% ou 1%',6);
    With TwsQualitative(DSCol) Do
      Begin
      AddLevel('NS');
      AddLevel('5%');
      AddLevel('1%');
      End; { With Col }
    Struct.AddColEx(DSCol);                                                            {7}
    end; // with CTest
  for i:=2 to Result.NRows do
    for j:=1 to i-1 do
      begin
      Mean:=TwsDFVec.Create(7);                  // Aproveita a mesma variavel
      Mean[1]:=i-1;                              // Indice da variavel i
      Mean[2]:=j-1;                              // Indice da variavel j
      Mean[3]:=Result[i,j];                        // Correlacao
      Mean[4]:=Sqr(Mean[3]);                     // Determinacao
      Mean[5]:=Mean[3]*Sqrt((n-2-nvfix)/(1-Mean[4]));  // Estatistica t
      Mean[6]:=TInt(Mean[5],n-2-nvfix,False,True,Erro);
      If Mean[6]>=0.05 Then                      // Verifica significancia
        Mean[7]:=0
      Else
        If Mean[6]>=0.01 Then
          Mean[7]:=1
        Else
          Mean[7]:=2;
      CTest.MAdd(Mean)
      end;
end; { PartialCorrel }

function TwsGeneral.WPartialCorrel(C, CAdj: TwsLIVec; var Stat: TwsGeneral; BStat: Boolean;
  WInd: Integer; var CTest: TwsGeneral): TwsSymmetric;
{ Objetivo
    Retorna uma matriz sim?trica onde os componentes s?o os coeficientes de correla??o
    parciais ponderados entre as colunas especificadas
  Par?metros
    C   : Vetor com os ?ndices das colunas para as quais se deseja as correla??es
    CAdj: Colunas para as quais se deseja calcular as correla??es parciais
    Stat: matriz com as estat?sticas descritivas
    BStat: se true, a matriz de estat?sticas descritivas ser? constru?da; false Stat retorna nil
    WInd: ?ndice da vari?vel para pondera??o
    CTest: conjunto de dados com os resultados dos etstes realizados sobre os coeficientes
}
var
  Col, ColOp   : TwsLIVec;
  Mean,Aux     : TwsVec;
  DSCol        : TwsDataSetCol;
  SP           : TwsSymmetric;
  i, nvtot, n,
  nvfix,nv,j   : Integer;
  w            : Double;
  Erro         : Word;
begin
  Col := iVecAppend(C,CAdj,True,CAdj.Len);
  nv:=C.Len; nvfix:=CAdj.Len; nvtot:=Col.Len;

  SP:=WAdjCrossProd(Col,Mean,n,w,WInd);

  for i:= 1 to C.Len do
    begin
    SP.RowName[i] := ColName[C[i]];
    SP.ColName[i] := SP.RowName[i];
    end;
  SP.MLab := 'Matriz de produtos cruzados (ponderados) ajustados para m?dia';

  if BStat then
    begin
    Stat := TwsGeneral.Create(C.Len,4);
    Stat.Name := 'Estat_' + System.Copy(Name,1,4);
    Stat.MLab := 'Estat?sticas descritivas simples';
    Stat.ColName[1] := 'Media'; Stat.ColName[2] := 'Variancia';
    Stat.ColName[3] := 'Desv_Pad'; Stat.ColName[4] := 'Coef_Var';
    for i:= 1 to C.Len do
      begin
      Stat.RowName[i] := ColName[C[i]];
      Stat[i,1] := Mean[i]; Stat[i,2] := sp[i,i]/(w-1-CAdj.Len);
      Stat[i,3] := ScalarSqrt(Stat[i,2]);
      Stat[i,4] := ScalarProd(ScalarDiv(Stat[i,3],Mean[i]),100);
      end;
    End;

  ColOp := TwsLIVec.Create(nvtot);
  for i := 1 to nvtot do ColOp[i] := 1;

  Aux := TwsDFVec.Create(nvfix);
  for i := 1 to nvfix do
    Aux[i] := SP[nv+i,nv+i]*1.0e-8; { Toler?ncias }   //dah erro aqui

  for i := 1 to nvfix do
    SP.RevSweep(nv+i, Aux[i], ColOp);

  Col.Free; ColOp.Free; Mean.Free; Aux.Free;

  Result := TwsSymmetric(SP.SubSymMat(1, nv));
  SP.Free;

  Result.CorrMat;

  CTest := TwsDataSet.Create('WCorr_Test');
  with TwsDataSet(CTest) do
    begin
    MLab:='Teste de signific?ncia para os coeficientes de correla??o';
    DSCol:=TwsQualitative.Create('Var_I','Vari?vel I');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 1. Nome da primeira vari?vel }

    DSCol:=TwsQualitative.Create('Var_J','Vari?vel J');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 2. Nome da segunda vari?vel }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correla??o de Pearson',11,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determina??o',11,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estat?stica T para hip?tese',12,7));        {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hip?tese H0',11,5));     {6}

    DSCol:=TwsQualitative.Create('Sig','Signific?ncia do teste ao n?vel 5% ou 1%',6);
    With TwsQualitative(DSCol) Do
      Begin
      AddLevel('NS');
      AddLevel('5%');
      AddLevel('1%');
      End; { With DSCol }
    Struct.AddColEx(DSCol);                                                            {7}
    end; // with CTest
  for i:=2 to Result.NRows do
    for j:=1 to i-1 do
      begin
      Mean:=TwsDFVec.Create(7);                  // Aproveita a mesma variavel
      Mean[1]:=i-1;                              // Indice da variavel i
      Mean[2]:=j-1;                              // Indice da variavel j
      Mean[3]:=Result[i,j];                      // Correlacao
      Mean[4]:=Sqr(Mean[3]);                     // Determinacao
      Mean[5]:=Mean[3]*Sqrt((n-2-CAdj.Len)/(1-Mean[4]));  // Estatistica t
      Mean[6]:=TInt(Mean[5],n-2-CAdj.Len,False,True,Erro);
      If Mean[6]>=0.05 Then                      // Verifica significancia
        Mean[7]:=0
      Else
        If Mean[6]>=0.01 Then
          Mean[7]:=1
        Else
          Mean[7]:=2;
      CTest.MAdd(Mean)
      end;

end; { WPartialCorrel }
{$endif}

function TwsGeneral.AdjCrossProd(Col: TwsLIVec; var Mean: TwsVec;
  var ValObs: Integer): TwsSymmetric;
{ Objetivo
    Retorna uma matriz (matriz das SQ&P) com as somas de quadrados e produtos ajustados
    para a m?dia.
  Parametros
    Col   : Indica quais as colunas que deverao ser consideradas.
    Mean  : Retorna as medias de cada coluna indicada em Col
    ValObs: N?mero de observa??es v?lidas (que n?o cont?m nenhum valor perdido) incluidas.
  Valores perdidos
    Uma observa??o ? eliminada se em alguma das posicoes estabelecidas por Col houver um
    valor perdido
}
var
  n,i,j,w,
  k,ji,jk : Integer;
  di      : Double;
  x       : TwsVec;
begin
  n := Col.Len;
  Result := TwsSymmetric.Create(n);
  Mean := TwsDFVec.Create(n);
  for i := 1 to n do
    begin
    Mean[i]:=0;
    for j:=1 to i do Result[i,j]:=0
    end; { for i }

  ValObs:=0;
  for j := 1 to NRows do
    begin
    x := Row[j];
    if not x.LocMiss(Col) then        // Se nao houver valores perdidos
      begin
      Inc(ValObs);
      for i := 1 to n do
        begin                        // Atualiza matriz de SQ & P
        ji := Col[i];
        di := (x[ji]-Mean[i]);
        Mean[i] := Mean[i] + di/ValObs;
        for k := 1 to i do
          begin
          jk := Col[k];
          Result[i,k] := Result[i,k] + di*(x[jk]-Mean[k])
          end { for k }
        end { for i }
      end { if VecMiss }
    end { for j }
end; { AdjCrossProd }

function TwsGeneral.WAdjCrossProd(Col: TwsLIVec; out Mean: TwsVec; out ValObs: Integer;
  out wa: Double; WIndex: Integer): TwsSymmetric;
{ Objetivo
    Retorna uma matriz com as somas de quadrados e produtos ajustadas para a m?dia
    referentes ?s colunas indicadas
  Par?metros
    Col   : ?ndices das colunas que dever?o ser consideradas
    Mean  : Retorna as medias de cada coluna indicada em Col
    S     : Retorna a matriz simetrica dos produtos cruzados
    ValObs: N?mero de observa??es v?lidas
    wa    : Soma dos pesos
    WIndex: Indice da variavel peso em X
  Valores perdidos
    Toda observa??o que possuir algum valor perdido (para as vari?veis indicadas ou para a
    vari?vel peso), peso nulo ou negativo ? exclu?da
}
var
  n,i,j,
  k,ji,jk: Integer;
  w,di   : Double;
  x      : TwsVec;
begin
  n := Col.Len;
  Result := TwsSymmetric.Create(n);
  Mean := TwsDFVec.Create(n);
  for i := 1 to n do
    begin
    Mean[i]:=0;
    for j:=1 to i do Result[i,j]:=0
    end; { for i }

  ValObs:=0;
  wa := 0;
  for j := 1 to NRows do
    begin
    x := Row[j];
    if not x.LocMiss(Col) then
      begin
      w :=x[WIndex];
      if (w>0) and not wsGLib.IsMissValue(w) then
        begin
        Inc(ValObs);
        wa := wa+w;
        for i := 1 to n do
          begin                        { Atualiza matriz de SQ & P}
          ji := Col[i];
          di := w*(x[ji]-Mean[i]);
          Mean[i] := Mean[i] + di/wa;
          for k := 1 to i do
            begin
            jk := Col[k];
            Result[i,k] := Result[i,k] + di*(x[jk]-Mean[k])
            end { for k }
          end { for i }
        end
      end { if LocMiss }
    end { for j }
end; { WAdjCrossProd }

function TwsGeneral.HesQR(MaxIts: Word; var ErrCode: Word): TwsGeneral;
{ Objetivo
    Encontra todos os valores de uma matriz superior de Hessenberg
  Par?metros
    MaxIts: N?mero m?ximo de itera??es do processo
    ErrCode: C?digo de erro. Retorna NIterMax se MaxIts n?o for suficiente para a
    converg?ncia do processo, 0 se n?o ocorreu erro
  Retorno
    Na chamada B:=A.(MaxIts,ErrCode), a matriz A na entrada dever? ser a sa?da do algoritmo
    TwsGeneral.Hessenb e seu cnte?do ? destru?do no processo. Em B retorna na primeira linha
    a parte real dos autovalores e na segunda linha a patre complexa.
}
label
  2, 3, 4;
var
  nn,m,l,k,j,i,
  its,mmin      : Integer;
  z,y,x,w,v,u,t,
  s,r,q,p,anorm : Double;
  L0,L1,L2,wr,wi: TwsVec;
  Indx          : TwsLIVec;
begin
  ErrCode := 0;
  wr := TwsDFVec.Create(NCols);
  wi := TwsDFVec.Create(NCols);
  Result := TwsGeneral.Create(0,NCols);
  // Calcula norma da matriz para possivel uso na localizacao de pequenos elementos na mtDiagonal
  anorm := abs(Get(1,1));
  for i := 2 to NCols do
    begin
    L0 := Row[i];
    for j := i-1 to NCols do anorm := anorm + abs(L0[j])
    end;
  nn := NCols;
  t := 0.0;
  // Altera somente por uma mudanca excepcional
  while nn >= 1 do      // Inicia a pesquisa para o proximo autovalor
    begin
    its := 0;
    L0 := Row[nn];
  // Inicia iteracao. Verifica pequenos valores na subdiagonal
2:  for l := nn downto 2 do
      begin
      L1 := Row[l];
      s := abs(Get(l-1, l-1)) + abs(L1[l]);
      if s = 0.0 then s := anorm;
      if abs(L1[l-1]) + s = s then GoTo 3
      end;
    l := 1;
3:  x := L0[nn];
    if l = nn then    // encontrou uma raiz
      begin
      wr[nn] := x + t;
      wi[nn] := 0.0;
      Dec(nn)
      end
    else
      begin
      y := Get(nn-1, nn-1);
      w := L0[nn-1]*Get(nn-1, nn);
      if l = nn-1 then   // duas raizes encontradas
        begin
        p := 0.5*(y - x);
        q := p*p + w;
        z := sqrt(Abs(q));
        x := x + t;
        if q >= 0.0 then   // um par real
          begin
          z := p + Sign(z, p);
          wr[nn] := x + z;
          wr[nn-1] := wr[nn];
          if z <> 0.0 then wr[nn] := x - w/z;
          wi[nn] := 0.0;
          wi[nn-1] := 0.0
          end
        else
          begin                 // um par complexo
          wr[nn] := x + p;
          wr[nn-1] := wr[nn];
          wi[nn] := z;
          wi[nn-1] := -z
          end;
        Dec(nn, 2)
        end
      else
        begin                   // Nao encontrou raiz. Continua iteracao
        if its > MaxIts then
          begin
          wr.Free;
          wi.Free;
          Result.Free;
          result:=nil;
          ErrCode := NIterMax;
          Exit
          end;
        if (its = 10) or (its = 20) then   // Forma mudanca excepcional
          begin
          t := t + x;
          for i := 1 to nn do
            begin
            L1 := row[i];
            L1[i] := L1[i] - x
            end;
          s := abs(L0[nn-1]) + abs(Get(nn-1, nn-2));
          x := 0.75*s;
          y := x;
          w := -0.4375*s*s
          end;
        Inc(its);
        for m := nn-2 downto l do         // Forma mudanca. Olha dois pequenos elementos
          begin                           // consecutivos da subdiagonal
          L1 := Row[m];
          z := L1[m];
          r := x - z;
          s := y - z;
          p := (r*s - w)/Get(m+1, m) + L1[m+1];
          q := Get(m+1, m+1) - z - r - s;
          r := Get(m+2, m+1);
          s := abs(p) + abs(q) + abs(r);   // Escala para previnir underflow ou overflow
          p := p/s;
          q := q/s;
          r := r/s;
          if m = l then Goto 4;
          u := abs(L1[m-1])*(abs(q) + abs(r));
          v := abs(p)*(abs(Get(m-1, m-1)) + abs(z) + abs(Get(m+1, m+1)));
          if u + v = v then Goto 4
          end;
4:      for i := m+2 to nn do
          begin
          L1 := Row[i];
          L1[i-2] := 0.0;
          if i <> m+2 then L1[i-3] := 0.0
          end;
        for k := m to nn-1 do   // Duplica QR sobre as linhas 1 a nn e colunas m a nn
          begin
          L1 := Row[k];
          if k <> m then
            begin
            p := L1[k-1];       // Inicializacao do vetor de Householder
            q := Get(k+1, k-1);
            if k <> nn-1 then
              r := Get(k+2, k-1)
            else
              r := 0.0;
            x := abs(p) + abs(q) + abs(r);   // Escala para previnir underflow ou overflow
            if x <> 0.0 then
              begin
              p := p/x;
              q := q/x;
              r := r/x
              end
            end;
          s := Sign(sqrt(p*p + q*q + r*r), p);
          if s <> 0.0 then
            begin
            if k = m then
              begin
              if l <> m then
                L1[k-1] := -L1[k-1];
              end
            else
              L1[k-1] := -s*x;
            p := p + s;
            x := p/s;
            y := q/s;
            z := r/s;
            q := q/p;
            r := r/p;
            for j := k to nn do            // Modificacao da linha
              begin
              p := L1[j] + q*Get(k+1, j);
              if k <> nn-1 then
                begin
                p := p + r*Get(k+2, j);
                Put(k+2,j,Get(k+2, j) - p*z)
                end;
              Put(k+1,j,Get(k+1, j) - p*y);
              L1[j] := L1[j] - p*x
              end;
            mmin := Min(nn, k+3);
            for i := l to mmin do        // modificacao da coluna
              begin
              L2 := Row[i];
              p := x*L2[k] + y*L2[k+1];
              if k <> nn-1 then
                begin
                p := p + z*L2[k+2];
                L2[k+2] := L2[k+2] - p*r
                end;
              L2[k+1] := L2[k+1] - p*q;
              L2[k] := L2[k] - p
              end
            end
          end; { for }
        Goto 2                         // volta para a proxima iteracao do autovalor atual
        end
      end  { else if ll = nn }
    end; { while }                     // volta para o proximo autovalor
  Indx := wr.QuickIndx(True);          // ordena a parte real
  wi.SortOf(Indx, ErrCode);            // e coluna a parte complexa na mesma ordem
  Indx.Free;
  Result.MAdd(wr);
  Result.MAdd(wi);
end; { HesQR }

Procedure TwsGeneral.MatOrder(Col: TStringList);
{ Objetivo:
    Ordena as linhas da matriz especificada em funcao de chaves estabelecidas.
  Par?metros:
    Col: Especifica as chaves e o tipo de ordenamento (ascendente ou descendente)
      cada componente de Col ter? o nome da coluna que servir? de chave para a ordena??o
      e um indicador (A ou D) para estabelecer a ordena??o ascendente ou descendente,
      separados por um delimitador v?lido [#9,#10,#13,' ',',','=','\', '"','(',')']. Por
      exemplo, Col[0]='Col1 A' e Col[1]='Col2 D' indicam que a ordena??o das linhas ser?
      em fun??o dos valores das colunas Col1 e Col2, ascendente para a primeirab e
      descendente para a segunda.
}
Var Str1,Str2     : String;
    k             : Integer;
    kk            : Cardinal;
    Column,Ascend : TwsLIVec;
begin
  Str1 := '';
  Ascend := TwsLIVec.Create(Col.Count);
  Try
    For k := 0 to Col.Count-1 do
      Begin
      kk := 1;  {O kk tem que estar aqui!}
      // Nome da coluna
      Str2 := SysUtilsEx.StrToken(Col[k], kk, DelChar);
      Str1 := Str1 + Str2 + ' ';

      // Ascendente ou descendente?
      Str2 := SysUtilsEx.StrToken(Col[k], kk, DelChar);
      If Str2 = 'A' Then
         Ascend[k+1] := 1
      Else
         Ascend[k+1] := 0;
      End;
    // Indices das colunas
    Column := IndexColsFromString(Str1);
    Try
      SortRows(Column, Ascend);
    Finally
      Column.Free;
    End;

  Finally
    Ascend.Free;
  End;
end; { MatOrder }

Function TwsGeneral.GetTableValue(nTreat, DF: Double): Double;
{ Objetivo
    Dada a tabela com os valores para o teste de Tukey, obt?m o valor desejado.
  Par?metros
    nTreat: N?mero de tratamentos ou medias a comparar
      Se nTrat < 2 ent?o nTrat = 2
      Se nTrat > ?ltimo valor da tabela, ntrat = ?ltimo valor da tabela
    DF: graus de liberdade do residuo
      Se DF <= 1 ent?o DF = 2
      Se DF > ultimo valor da tabela, DF = ?ltimo valor da tabela
  Observa??o: Se um ou os dois valores dos par?metros n?o est?o na tabela, o valor retornado
    ? uma interpola??o linear dos valores existentes
}
var j1,j2,k1   : Integer;
    Par1, Par2 : Boolean;
    caso       : Byte;
    p0,p1,x1,
    x2,y1,y2   : Double;
Begin
  if ntreat < 2 then ntreat := 2;
  if ntreat > Get(1,NCols) then ntreat := Get(1,NCols);
  if df <= 1 then df := 2;
  if df > Get(NRows,1) then df := Get(NRows,1);
  // Localiza os parametros na tabela
  Par1 := LocateAtRow(nTreat,1,j1);  // j1 eh a linha da tabela corresp. ao
  Par2 := LocateAtCol(DF,1,j2);
  caso := 3;                                       // Nao existe nenhum
  if Par1 and Par2 then
    caso := 0                                     // Ambos existem
  else
    if Par1 then                                // Existe somente o numero de tratamentos
      caso := 1
    else
      if Par2 then
        caso := 2;                                 // Existe somente os graus de liberdade
  case caso of
    0: Result := Get(j2,j1);                       // nTreat e df existem na tabela
    1: begin                                       // Somente nTreat
       p0 := Get(j2,1); p1 := Get(j2+1,1);
       x1 := Get(j2,j1); x2 := Get(j2+1,j1);
       Result := x1 + (df-p0)*(x2-x1)/(p1-p0)      // Faz a interpolacao em relacao aos GL
       end;
    2: begin                                       // Somente df existe
       p0 := Get(1,j1); p1 := Get(1,j1+1);
       x1 := Get(j2, j1); x2 := Get(j2,j1+1);
       Result := x1 + (nTreat-p0)*(x2-x1)/(p1-p0)  // Faz a interpolacao
       end;
    3: begin                                       // Nenhum parametro existe
       p0 := Get(1,j1); p1 := Get(1,j1+1);
       x1 := Get(j2,j1); x2 := Get(j2,j1+1);
       y1 := x1+(ntreat-p0)*(x2-x1)/(p1-p0);     // Interpola primeiro em relacao aos trat

       x1 := Get(j2+1,j1); x2 := Get(j2+1,j1+1);
       y2 := x1+(ntreat-p0)*(x2-x1)/(p1-p0);

       p0 := Get(j2,1); p1 := Get(j2+1,1);
       Result := y1 + (df-p0)*(y2-y1)/(p1-p0);     // Depois em relacao ao residuo
       end;
    end; { case }
End; { GetTableValue }

function TwsGeneral.SortKeyMeans(KeyCol, Col: TwsLIVec; var XRow: Integer): TwsVec;
{  Objetivo
     Calcula a m?dia de colunas da matriz, iniciando na linha especificada. A utiliza??o
     deste m?todo pressup?e qua as linhas da matriz estar?o ordenadas em rela??o ?s colunas
     cujos ?ndices est?o em KeyCol.
   Par?metros
     KeyCol: Indice das colunas que servir?o como chaves. A pesquisa inicia na linha XRow e
             segue enquanto existir concord?ncia nas chaves, ou seja, as linhas que ser?o
             utilizadas ser?o aquelas onde os ?ndices da linha XRow nas colunas especificadas
             por KeyCol se repetem nas linhas seguintes.
     Col:    ?ndices das colunas para as quais ser?o calculadas as m?dias e n?mero de
             repeti??es.
     XRow:   Na entrada indica a linha inicial para processamento. Na sa?da, cont?m a linha
             seguinte ? ?ltima processada. Este par?metro ? conveniente, entre outras coisas,
             para um processamento sequencial das linhas da matriz, atrav?s de uma chamada
             sequencial desta rotina.
   Retorno
     Retorna um vetor onde nas primeiras KeyCol.Len posi??es estar?o as chaves, nas Col.Len
     seguintes as m?dias e na ?ltima ir? retornar o n?mero de repeti??es. Portanto, o
     retorno ser? um vetor de KeyCol.Len + Col.Len + 1 colunas. Retorna nil se nenhuma
     linha for processada.
  }
var
  n,k,k1,rep: Integer;
  Match     : Boolean;
  aux       : Double;

  function KeyMatch(Key: TwsVec): boolean;
  var
    i: Integer;
  begin
    Result := True;
    i := 1;
    while Result and (i <= KeyCol.Len) do
      begin
      Result := FEquals(Key[i], Get(XRow,KeyCol[i]));
      Inc(i)
      end
  end; { KeyMatch }

begin
  Result := nil;
  if XRow <= NRows then
    begin
    n:=KeyCol.Len+Col.Len+1;
    Result := TwsDFVec.Create(n);                   // Vetor das medias e chaves
    for k := KeyCol.Len+1 to Result.Len do
      Result[k] := 0;
    for k := 1 to KeyCol.Len do                     // copia das chaves
      Result[k] := Get(XRow, KeyCol[k]);
    Match := True;
    rep := 0;
    while Match and (XRow <= NRows) do
      begin
      Match := KeyMatch(Result);                    // Chave continua a mesma?
      if Match then
        begin
        for k := 1 to Col.Len do
          begin
          k1 := KeyCol.Len + k;
          aux :=  Get(XRow, Col[k]);
          if not wsGLib.IsMissValue(aux) then
            begin
            Inc(rep);
            aux := aux - Result[k1];
            Result[k1] := Result[k1] + aux/rep
            end // if aux
          end;  // for k
        Inc(XRow);
        end     // if Match
      end;      // while
    Result[n]:=rep
    end
end;

function TwsGeneral.KeyGroupMeans(KeyCol, Col:TwsLIVec; GMean: TwsVec; SSQ: TObject;
  var XRow, M: Integer; Covar,Univ: Boolean): TwsVec;
{  Objetivo
     Calcula a m?dia de colunas da matriz, iniciando na linha especificada. A utiliza??o
     deste m?todo pressup?e qua as linhas da matriz estar?o ordenadas em rela??o ?s colunas
     cujos ?ndices est?o em KeyCol.
   Par?metros
     KeyCol: ?ndices das colunas que servir?o como chaves. As linhas deverao estar ordenadas
             segundo as chaves estabelecidas.
     Col:    ?ndices das colunas para as quais ser?o calculadas as m?dias, somas de
             quadrados e n?mero de repeti??es.
     GMean:  Vetor com as m?dias gerais de todas as linhas processadas. A cada chamada
             deste m?todo, este vetor ? atualizado.
     SSQ:    Somas de quadrados ajustadas para a m?dia de todas as vari?veis especificadas
             em Col. Na primeira chamada este vetor devera estar dimensionado com comprimento
             Col.Len e com zeros em todas as posicoes. Se Univ for True este par?metro
             retornar? um vetor; caso contr?rio retornar? uma matriz sim?trica com as
             somas de quadrados e produtos. A cada chamada deste m?todo este par?metro ?
             atualizado.
     XRow:   Na entrada deve indicar a linha onde ser? iniciado o processamento. Na sa?da
             indicar? a linha seguinte ? ?ltima processada.
     Univ:   True se em SSq dever?o retornar somente as somas de quadrados das vari?veis;
             false para que retorne a matriz das somas de quadrados e produtos. A primeira
             situa??o corresponde ? utiliza??o do m?todo num contexto de an?lise univariada
             enquanto que o segundo, de an?lise multivariada.
     M:      Acumula o n?mero de observa??es v?lidas processadas.
   Retorno:
     Nas primeiras KeyCol.Len colunas estar?o as chaves, nas Col.Len seguintes as m?dias
     e na ?ltima ir? retornar o n?mero de repeti??es. Portanto, o retorno ser? um vetor de
     KeyCol.Len + Col.Len + 1 colunas. Retorna nil se nenhuma linha for processada.
  }
var
  rep,k,k1,n,j: Integer;
  Match       : Boolean;
  aux1        : Double;
  y           : TwsVec;

  function KeyMatch(Key: TwsVec): boolean;
  var i: Integer;
  begin
    Result := True;
    i := 1;
    while Result and (i <= KeyCol.Len) do
      begin
      Result := FEquals(Key[i],Get(XRow, KeyCol[i]));
      Inc(i)
      end
  end; { KeyMatch }

begin
  Result := nil;
  if XRow <= NRows then
    begin
    n := KeyCol.Len + Col.Len + 1;
    Result := TwsDFVec.Create(n);            // Chaves, medias e numero de repeticoes

    for k := KeyCol.Len + 1 to Result.Len do
      Result[k] := 0;

    for k := 1 to KeyCol.Len do              // copia valor das chaves
      Result[k] := Get(XRow, KeyCol[k]);

    Match := True;
    rep := 0;
    while Match and (XRow <= NRows) do
      begin
      Match := KeyMatch(Result);
      if Match then                          // Chave continua a mesma?
        begin
        y := Row[XRow];
        if not y.LocMiss(Col) then           // Observacao nao entra se tiver valor
          begin                              // perdido em alguma posicao
          Inc(rep);
          Inc(M);
          if Univ and (not Covar) then                       // Analise for univariada?
            for k := 1 to Col.Len do
              begin
              k1 := KeyCol.Len + k;          // Result guarda as medias de cada combinacao
              Result[k1] := Result[k1] + (y[Col[k]] - Result[k1])/rep;
              aux1 := y[Col[k]] - GMean[k];  // GMean guarda as medias gerais
              GMean[k] := GMean[k] + aux1/M; // SSQ guarda as sq de cada variavel
              TwsVec(SSQ)[k] := TwsVec(SSQ)[k] + ((M-1)/M)*aux1*aux1;
              end // For k
          else                               // Senao obtem matriz de SQ&P
            for k := 1 to Col.Len do
              begin
              k1 := KeyCol.Len + k;          // Result guarda as medias de cada combinacao
              Result[k1] := Result[k1] + (y[Col[k]] - Result[k1])/rep;
              aux1 := (y[Col[k]]-GMean[k]);
              GMean[k] := GMean[k] + aux1/M; // Atualiza matriz de SQ&P
              for j := 1 to k do
                TwsSymmetric(SSq)[k,j] := TwsSymmetric(SSq)[k,j] + aux1*(y[Col[j]]-GMean[j])
              end // for k
          end; // if not LocMiss(y, Col)
        Inc(XRow);
        end; // if Match
      end; // while
    Result[n]:=rep
    end
end; // KeyGroupMeans

function TwsGeneral.KeyMeans(KeyCol, Col: TwsLIVec): TwsVec;
{  Objetivo
     Calcula m?dia de linhas da matriz para as colunas especificadas.
   Par?metros
     KeyCol: ?ndice das colunas para que servir?o como chaves. Varre todas as linhas da
             matriz para calcular a m?dia correspondentes ? chave. Assim, neste m?todo,
             as linhas n?o necessitar?o estar ordenadas segundo as chaves estabelecidas.
     Col:    ?ndices das colunas para as quais ser?o calculadas as m?dias e o n?mero de
             repeti??es.
  Retorno
   Nas primeiras KeyCol.Len colunas estar?o as chaves, nas Col.Len seguintes as m?dias
   e na ?ltima ir? retornar o n?mero de repeti??es. Portanto, o retorno ser? um vetor de
   KeyCol.Len + Col.Len + 1 colunas.
  }
var
  i,rep,k,k1,n: Integer;
  Match: Boolean;
  aux: Double;

  function KeyMatch(Lin: Integer; Key: TwsVec): boolean;
  var
    i: Integer;
  begin
    Result := True;
    i := 1;
    while Result and (i <= KeyCol.Len) do
      begin
      Result := FEquals(Key[i], Get(Lin,KeyCol[i]));
      Inc(i)
      end
  end; { KeyMatch }

begin
  n :=KeyCol.Len+Col.Len+1;
  Result := TwsDFVec.Create(n);        // Vetor das chaves, medias e numero de repeticoes
  for k := KeyCol.Len+1 to Result.Len do
    Result[k] := 0;
  for k := 1 to KeyCol.Len do          // copia valor das chaves
    Result[k+1] := Get(1, KeyCol[k]);
  rep := 0;
  for i:=1 to NRows do
    begin
    Match := KeyMatch(i,Result);
    if Match then                       // Se concorda com a chave
      for k:=1 to Col.Len do
        begin
        k1 := KeyCol.Len+k;
        aux :=  Get(i,Col[k]);
        if wsGLib.IsMissValue(Aux) then
          begin
          Inc(rep);
          aux := aux - Result[k1];
          Result[k1]:=Result[k1]+aux/rep // Calcula a media, passo a passo
          end
        end
    end; { for i }
  { Se a matriz for a matriz de medias, ajustar o divisor }
  Result[n]:=rep
end;

function TwsGeneral.ColConst(k: Integer): Boolean;
{ Objetivo
    Verificar se todos os valores de uma coluna s?o iguais.
  Par?metros
    k: Coluna onde sera feita a pesquisa
}
var
  i: Integer;
  Aux: Double;
begin
  Aux := Get(1,k);
  Result := FEquals(Aux,Get(2,k));
  i := 2;
  while Result and (i<NRows) do
    begin
    Inc(i);
    Result := FEquals(Aux,Get(i,k))
    end
end; { ColConst }

function TwsGeneral.LUInv: TwsGeneral;
{  Objetivo
     Dada uma matriz geral, retorna a sua inversa utilizando a fatora??o LU
   Observa??es
     Na matriz retornam os fatores LU
}
var
  i,j     : Integer;
  Col     : TwsVec;
  Indx    : TwsLIVec;
  d       : Double;
  ErrCode :word;
begin
  LU(Indx,d,ErrCode);
  Result := TwsGeneral.Create(NCols, NCols);
  Col := TwsDFVec.Create(NCols);
  for j := 1 to NCols do
    begin
    for i := 1 to NCols do Col[i] := 0;
    Col[j] := 1.0;
    BackSubst(Indx, Col);
    for i := 1 to NRows do
      Result[i,j] := Col[i]
    end;
  Col.Free;
  Indx.Free;
end; { LUInv }

procedure TwsGeneral.BackSubst(Indx: TwsLIVec; B: TwsVec);
{ Objetivo
    Resolve um sistema mtTriangular
  Par?metros
    Indx: vetor de ?ndices de trocas realizadas durante a fatora??o da matriz
    B: vetor com os elementos do segundo membro
}
var
  j,ip,ii,i: Integer;
  sum      : Double;
begin
  ii := 0;
  for i := 1 to NRows do
    begin
    ip := Indx[i];
    sum := B[ip];
    B[ip] := B[i];
    if ii <> 0 then
      for j := ii to i-1 do
        sum := sum - Get(i,j)*B[j]
    else
      if sum <> 0.0 then ii := i;
    B[i] := sum
    end;
  for i := NRows downto 1 do
    begin
    sum := B[i];
    for j := i+1 to NCols do
      sum := sum - Get(i,j)*B[j];
    B[i] := sum/Get(i,i)
    end
end; { BackSubst }

function TwsGeneral.LUToInv(Indx: TwsLIVec): TwsGeneral;
{  Objetivo
     Dada uma matriz j? fatorada atrav?s do algoritmo LU, retorna a inversa da matriz
     original
   Par?metros
     Indx: Vetor com os indices das trocas realizadas na fatora??o LU
}
var
  i, j: Integer;
  Col: TwsVec;
begin
  Result := TwsGeneral.Create(NCols, NCols);
  Col := TwsDFVec.Create(NCols);
  for j := 1 to NCols do
    begin
    for i := 1 to NCols do Col[i] := 0;
    Col[j] := 1.0;
    BackSubst(Indx, Col);
    for i := 1 to FList.Count do Result[i,j] := Col[i]
    end;
  Col.Free;
end; { LUToInv }

procedure TwsGeneral.Householder(p: Integer; out g, h: TwsDFVec; var ErrCode: Word);
{ Objetivo
    Aplica as transformacoes de Householder a matriz tal que A=QU
  Par?metros
    p: N?mero de colunas que ser?o operadas pelo procedimento
    g: Vetor que retorna os valores da diagonal de F
    h: Vetor que retorna os valores de f'[i]f[i]
  Sa?da
    * Na chamada A.Householder(p,g,h,ErrCode), ao t?rmino do processo, o conte?do da matriz
    ? o seguinte:
      a11 a12       u11 u12
      a21 a22 --->  f12 u22
      a31 a32       f13 f23
      a41 a42       f14 f24
    g=[f11, f22] e h=[(f1)'*f1, (f2)'*f2]
    sendo que com F ? poss?vel construir a matriz de reflex?o de Householder e obter Q.
    * Se a matriz A nxp for completada ? direita com uma identidade, no lugar da identidade
    retorna a matriz Q.
    * No sistema inconsistente y=Xb + e onde X ? nxp entao se A=[X, y] ent?o, no final
    Q v   ? a solucao de m?nimos quadrados ser? a solu??o de Qb=v e t't ? a soma de
    0 t   quadrados de quadrados do res?duo
}
var
  s        : ShortInt;
  i,j,k    : Integer;
  l,d,m,aux: Double;
  eps      : Double;
begin
  ErrCode :=0;
  eps := 1/(16*16*16*16*16) * NRows;
  g := TwsDFVec.Create(p);
  h := TwsDFVec.Create(p);
  for i:=1 to p do
    begin
    d:=0;
    for k:=i to NRows do
      d:=d+Sqr(Get(k,i));
    aux:=Get(i,i);
    if aux>0 then
      s:=1
    else
      s:=-1;
    l:=s*Sqrt(d);
    h[i]:=d+l*aux;
    // protege a divisao no caso de matrizes de posto coluna incompleto
    if Abs(h[i])<=eps then       // =========> verificar melhor esta situacao !!!!!!!!!!!
      begin
      g.Free;
      h.Free;
      ErrCode:=NHouse;
      Exit
      end;
    g[i]:=l+aux;
    Put(i,i,-l);
    for j:=i+1 to NCols do
      begin
      m:=0;
      for k:=i+1 to NRows do
        m:=m+Get(k,i)*Get(k,j);
      m:=(g[i]*Get(i,j)+m)/h[i];
      Put(i,j,Get(i,j)-g[i]*m);
      for k:=i+1 to NRows do
        Put(k,j,Get(k,j)-Get(k,i)*m)
      end; // for j
    if s>0 then
      for j:=i to NCols do
        Put(i,j,-Get(i,j))
    end; // for i - para cada coluna de
end; // Householder

procedure TwsGeneral.HouseCol(OnlyApply: Boolean; p,l,Col: Integer; h: TwsVec; cStart,nc: Integer);
{ Objetivo
    Constr?i e aplica a transforma de Householder ?s colunas de uma matriz. Algoritmo b?sico
    em Lawson & Hanson (1971), p?g. 57 e implementa??o em Fortran na p?g. 308
  Par?metros
    HTrans: Com HTrans=False o m?todo ir? construir e aplicar a transforma??o de Householder.
            Para HTrans=True, somente a constru??o da aplica??o ser? feita.
    p     : Elemento pivo
    l     : Elemento a partir do qual todos os demais ser?o zerados. Se l<=NRows a
            transforma??o ser? constru?da para zerar os elementos de l a NRows. Se l>NRows,
            retorna a transforma??o identidade
    Col   : Coluna incial da matriz para aplica??o da transforma??o
    nc    : N?mero de colunas para aplica??o da transforma??o
}
function Inner(ci,cj,start: Integer): Double;
{ci, cj: Colunas para o produto interno
 start : Elemento incial
}
var
  i,k: Integer;
begin
  Result:=0;
  for i:=Start to NRows do
    begin
    Result:=Result+Get(i,ci)*Get(i,cj)
    end
end; // Inner

var
  i,j   : Integer;
  s,b,vp: Double;
begin
  if not OnlyApply then
    begin               // Constroi a transformacao
    vp:=Get(p,Col);
    s:=Sqrt(Sqr(vp)+Inner(Col,Col,l));
    if vp>0 then s:=-s;
    h[p]:=vp-s;
    Put(p,Col,vp)
    end;
  b:=vp*h[p];
  if b<>0 then
    begin
    Dec(cStart);
    for j:=1 to nc do
      begin
      s:=(Get(p,j)*h[p]+Inner(Col,cStart+j,l))/b;
      Put(p,j,Get(p,j)+s*h[p]);
      for i:=l to NRows do
        Put(i,j,Get(i,j)+s*Get(i,Col));
      end
    end;
end; // HouseCol

procedure TwsGeneral.HouseForward(xCol: Integer);
{xCol: Coluna inicial do segundo membro}
var
  j: Integer;
  h: TwsDFVec;
begin
  Dec(xCol);
  h:=TwsDFVec.Create(xCol);
  for j:=1 to xCol do
    HouseCol(False,j,j+1,j,h,j+1,xCol-j)
end;

procedure TwsGeneral.DeleteCols(Col: TwsLIVec);
{ Objetivo
    Elimina colunas especificadas
  Par?metros
    Col: ?ndices das colunas a serem eliminadas
  M?todos chamados
    DeleteCols herdado
}
var i: Integer;
begin
  MDeleteCols(Col);
  if CName <> nil then
    for i := Col.Len downto 1 do      // elimina os atributos das colunas
      CName.Delete(Col[i]-1);
  FNCols := FNCols - Col.Len;
end;  // TwsGeneral.DeleteCols

procedure TwsGeneral.InternalSave3(Writer: TWriter);
{ Objetivo
    Grava parte da matriz geral do disco
  Par?metros
    Writer: Objeto de escrita
}
var i: Integer;
begin
  Writer.WriteListBegin;
  For i := 1 To NRows Do
    Row[i].SaveToStream(Writer);
  Writer.WriteListEnd;
end;

procedure TwsGeneral.InternalLoad3(Reader: TReader);
{ Objetivo
    Leitura de parte da matriz geral do disco
  Par?metros
    Reader: Objeto de leitura
}
begin
  Reader.ReadListBegin;
  While Not Reader.EndOfList Do
    PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;
end;

{ TwsSymmetric }

constructor TwsSymmetric.Create(NC: Integer);
{ Inicializa objeto TSimMatrix }
{ Objetivo
    Cria e inicializa um objeto do tipo TwsSymmetric.
  Par?metros
    NC: N?mero de linhas e de colunas. Se NC>0 s?o inseridos tantos vetores quantas s?o as linhas
    (colunas). O primeiro vetor tem tamanho 1, o segundo tamanho 2 e assim por diante. O ?ltimo
    possui tamanho igual ao n?mero de colunas.
}
var
  i: Integer;
begin
  inherited Create(NC, NC);
  FMatType := mtSymmetric;
  If NCols>0
    Then
      for i := 1 to FNCols do FList.Add(TwsDFVec.Create(i))
end; { TwsSymmetric.Create }

function TwsSymmetric.GetBlockName: String;
begin
  Result := 'wsGeneral';
end;

function TwsSymmetric.GetBlockNameComment: String;
begin
  Result := 'Estilo de formata??o para Matrizes do tipo TwsSymmetric';
end;

function TwsSymmetric.Get(i, j: Integer): Double;
{ Objetivo
    Obt?m o elemento na posi??o especificada da matriz. Numa matriz sim?trica
     somente os valores da mtTriangular inferior s?o armazenados. Se um valor da
     mtTriangular superior for solicitado, retorna uma c?pia do elemento
     correspondente na mtTriangular inferior.
  Par?metros:
    i: ?ndice da linha
    j: ?ndice da coluna
}
begin
  if i >= j then
    Get := Row[i].Data[j]
  else
    Get := Row[j].Data[i];
end; { TwsSymmetric.Get }

procedure TwsSymmetric.Put(i, j: Integer; x: Double);
  { Objetivo
    Atribui valor ao elemento na posi??o especificada da matriz. Numa matriz
    sim?trica somente os valores da mtTriangular inferior s?o armazenados.
  Par?metros
    i: ?ndice da linha
    j: ?ndice da coluna
    x: Valor a ser atribu?do ? posi??o (i, j)
}
begin
  FModified := True;
  if i >= j then
    Row[i].Data[j] := x
  else
    Row[j].Data[i] := x;
end; { TwsSymmetric.Put }

procedure TwsSymmetric.List(Buffer: TStrings);
{ Objetivo
    Lista os descritores da matriz.
  M?todos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    Sim?trica');
  inherited List(Buffer)
end;

procedure TwsSymmetric.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    C?pia da matriz
  Par?metros
    MT: Tipo da matriz de retorno
    Matrix: Matriz que retorna com a c?pia. Se tipo depender? de MT (mtGeneral, mtSymmetric,
      mtDiagonal ou mtTriangular). N?o ? poss?vel copiar uma matriz sim?trica como do tipo
      mtVandermonde ou mtToeplitz. Nesse caso, matrix retorna nil
}
var
  i,j: Integer;
  L0 : TwsVec;
begin
  case MT of
    mtGeneral:
      begin
      Matrix := TwsGeneral.Create(NRows, NCols);
      for i := 1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j := 1 to NCols do
          Matrix[i,j]:= Self[i,j]
        end
      end;
    mtSymmetric:
      begin
      Matrix := TwsSymmetric.Create(NCols);
      for i:=1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j]:=Self[i,j]
        end
      end;
    mtTriangular:
      begin
      Matrix := TwsTriangular.Create(NCols);
      for i:=1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j] := Self[i,j]
        end
      end;
    mtDiagonal:
      begin
      Matrix := TwsDiagonal.Create(NCols);
      L0 := Matrix.Row[1];
      for j:=1 to NCols do
        L0[j]:=Self[j,j]
      end;
    mtToeplitz,mtVandermonde:
      begin
      Matrix := nil;
      Exit
      end;
  end; { case }
  inherited Copy(MT, Matrix)
end; { Copy }

function TwsSymmetric.ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat: Boolean): TwsMatrix;
{Objetivo
   Efetuar opera??es entre uma matriz sim?trica e um escalar
 Par?metros
   x: Escalar para a opera??o
   Op: Tipo de opera??o desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas opera??es (soma ou produto) esse par?metro n?o ? utilizado.
     No caso de potencia??o, SFirst=True indica que o escalar estar? na base e o elemento
     na pot?ncia; caso contr?rio, o elemento estar? na base e o escalar na pot?ncia. Para
     o operador ** se SFirst=False, a opera??o corresaponder? ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contr?rio a opera??o n?o ? definida
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz
 Retorno
   Sempre uma matriz sim?trica
 Valores perdidos
   Se houver um valor perdido na opera??o envolvendo um elemento, o resultado ? valor perdido
}
var
  i: Integer;
begin
  if NewMat then
    Self.Copy(mtSymmetric,Result)
  else
    Result := Self;
  for i:=1 to Result.NRows do
    Result.Row[i].ByScalar(x,Op,False,SFirst);
end; // ByScalar

function TwsSymmetric.Transpose: TwsMatrix;
{ Objetivo
    Obtem a transposta da matriz
  Retorno
    Uma vez que a transposta de uma matriz sim?trica ? ela pr?pria, retorna uma c?pia da
    matriz que chamou
}
var
  err: Word;
begin
  Copy(mtSymmetric,Result);
end;

function TwsSymmetric.TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; //A'BA
{ Objetivo
    Dada a matriz geral A e a matriz simetrica B, retorna o produto A'*B*A numa
    matriz sim?trica sem efetuar a transposicao explicitamente
  Parametros
    A: Matriz geral
    B: Matriz sim?trica
    ErrCode: C?digo de erro. Retorna NImprDim (dimens?es impr?prias para a opera??o)
      se (A.NRows <> B.NRows)
  Retorno
    A chamada B.TranspMul5(A, Erro) retorna uma matriz sim?trica A'*B*A, uma vez que B ?
      sim?trica
}
var
  i,j,k: Integer;
  s: Double;
  aux: TwsDFVec;
begin
  if (A.NRows=NRows) then
    begin
    ErrCode := 0;
    Result:= TwsSymmetric.Create(A.NCols);
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NCols do
          begin
          { Faz o produto da coluna i de A por cada coluna de B e armazena em aux }
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=1 to A.NRows do
              s:=s+A[k,i]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to i do
            begin
            s:=0;
            for k := 1 to A.NRows
              do s:=s+aux[k]*A[k,j];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end; // mtGeneral, ...

      mtTriangular:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NCols do
          begin
          { Faz o produto da coluna i de A por cada coluna de B e armazena em aux }
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k := i to A.NRows do
              s:=s+A[k,i]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to i do
            begin
            s:=0;
            for k := j to A.NRows
              do s:=s+aux[k]*A[k,j];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end;

      mtDiagonal:
        begin
        aux:=TwsDFVec(A.Row[1]);
        for i:=1 to NRows do
          for j:=1 to i do
            Result[i,j]:=aux[i]*aux[j]*Self[i,j]
        end;
      end; //case
    end // if
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end;
end;

function TwsSymmetric.TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; //ABA'
{ Objetivo
    Dada a matriz geral A e a matriz simetrica B, retorna o produto A*B*A' numa matriz
      sim?trica sem efetuar a transposicao explicitamente
  Parametros
    A: Matriz geral
    B: Matriz sim?trica
    ErrCode: C?digo de erro. Retorna NImprDim (dimens?es impr?prias para a opera??o)
      se (A.NCols <> B.NRows)
  Retorno
    A chamada B.TranspMul10(A, Erro) retorna uma matriz sim?trica A*B*A', uma vez que B ?
      sim?trica
}
var
  i,j,k: Integer;
  s    : Extended;
  aux  : TwsDFVec;
begin
  if (A.NCols=NRows) then
    begin
    ErrCode := 0;
    Result:= TwsSymmetric.Create(A.NRows);
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NRows do           // Para cada linha de A
          begin
          for j := 1 to NCols do         // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=1 to NRows do
              s:=s+A[i,k]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to i do
            begin
            s:=0;
            for k := 1 to A.NCols
              do s:=s+aux[k]*A[j,k];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end; // mtGeneral, ...

      mtTriangular:
        begin
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NRows do
          begin
          { Faz o produto da coluna i de A por cada coluna de B e armazena em aux }
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k := 1 to i do
              s:=s+A[i,k]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to i do
            begin
            s:=0;
            for k := 1 to i do
              s:=s+aux[k]*A[j,k];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
          aux.Free;
        end;
      mtDiagonal:
        begin
        aux:=TwsDFVec(A.Row[1]);
        for i:=1 to A.NRows do
          for j:=1 to i do
            Result[i,j]:=aux[i]*aux[j]*Self[i,j]
        end;
      end; //case
    end // if
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end;
end; // TranspMul10


function TwsSymmetric.CholeskyFat(var r: Integer; NewMat: Boolean; eps: Double=1.0e-8): TwsMatrix;
{ Objetivo
    Obt?m o fator Triangular de Cholesky
  Par?metros
    r: Retorna o posto da matriz que chama o m?todo
    NewMat: True se o resultado retorna numa nova matriz; se False o fator Triangular ocupa a
            por??o Triangular inferior da matriz
    eps: Precis?o num?rica para determina??o do posto
  Retorno
    A chamada A.CholeskyFat(.,False) transforma A no fator Triangular de Cholesky, armazenado
    na pr?pria matriz (sim?trica) A, enquanto A.CholeskyFat(.,True) retorna uma matriz Triangular
    com o fator de Cholesky, sem alterar a matriz original.
}
var
  i,j,k: Integer;
  s    : Double;
begin
  if NewMat then
    Copy(mtTriangular,Result)
  else
    Result:=Self;
  r := Result.NCols;
  for j := 1 to Result.NCols do
    begin
    if j > 1 then
      for i := j to Result.NRows do
        begin
        s := Result[i,j];
        for k := 1 to j-1 do
          s := s - Result[i,k]*Result[j,k];
        Result[i,j] := s
        end;
    if Result[j,j] < eps then
      begin
      Result[j,j] := 0;
      Dec(r)
      end;
    s := Sqrt(Result[j,j]);
    for i := j to Result.NRows do
      begin
      if s <> 0 then
        Result[i,j] := Result[i,j]/s
      else
        Result[i,j] := 0
      end
    end
end; { CholeskyFat }

procedure TwsSymmetric.CholeskyInv(eps: Double=1.0e-8);
{ Objetivo
    Obt?m a inversa de uma matriz sim?trica a partir do fator de Cholesky. Inicialmente
    a mtTriangular inferior da matriz sim?trica ? operada de modo a tornar-se o fator
    mtTriangular de Cholesky. A partir desse fator ? constru?da a inversa matriz sim?trica.
  Observa??es
    Se A ? uma matriz sim?trica, a chamada A.CholeskyFat() obt?m o fator mtTriangular de A.
      O m?todo CholeskyInv transforma esse fator na inversa de A.
}
var
  i,j,l: Integer;
  s    : Extended;
begin
  for i := 1 to FNCols do
    begin
    if Abs(Get(i,i)) > eps then
      Put(i,i,1/Get(i,i))
    else
      Put(i,i,0)
    end;
  for i := 1 to FNCols-1 do
    for j := i+1 to FNCols do
      begin
      s := 0;
      for l := i to j-1 do
        s := s + Get(l,i)*Get(j,l);
      Put(j,i,-Get(j,j)*s)
      end;
  { Inversa da matriz simetrica }
  for i := 1 to FNCols do
    for j := i to FNCols do
      begin
      s := 0;
      for l := j to FNCols do
        s := s + Get(l,i)*Get(l,j);
      Put(j,i,s);
      end
end; { TwsSymmetric.CholeskyInv }

procedure TwsSymmetric.Inv(var ErrCode: Word; eps: Double=1.0e-8);
{ Objetivo
    Obt?m a inversa da matriz original atrav?s do m?todo de Cholesky
  Par?metros
    ErrCode: Retorna 0 se a invers?o ocorrer sem problemas; maInvError caso a matriz que
      chama seja singular
    eps: Precis?o para indica??o de matriz singular
}
var
  Rank: Integer;
begin
  ErrCode := 0;
  CholeskyFat(Rank,False,eps);
  if Rank = FNCols then
    CholeskyInv
  else
    ErrCode := maInvError
end;

procedure TwsSymmetric.RevSweep(k: Integer; DMin: Double; v: TwsLIVec);
  { Objetivo
      Aplica sweep numa matriz sim?trica
    Par?metros
      k: coluna para aplica??o do sweep
      DMin: toler?ncia ara aplica??o do sweep
      v: vetor de 1?s e -1?s com a mesma dimens?o do n?mero de coluna da matriz. Se v[i]=1
         a coluna i ainda n?o foi operada; -1 a opera??o j? foi realizada
}
var
  i,j  : Integer;
  b,c,d: Double;
begin
  d := Get(k,k);
  if ((v[k] = 1) and (d <= DMin)) then
    Exit;
  for i := 1 to FNRows do
    if i <> k then
      begin
      if i > k then
        b := Get(i,k)/d
      else
        b := v[i]*v[k]*Get(i,k)/d;
      for j := 1 to i do
        if j <> k then
          begin
          if k > j then
            c := Get(k,j)
          else
            c := v[j]*v[k]*Get(k,j);
          Put(i,j,Get(i,j)-b*c)
          end;
      end; {if i<>k}
  for i := 1 to k do
    Put(i,k,-Get(i,k)/d);
  for j := k to FNCols do
    Put(k,j,Get(k,j)/d);
  Put(k,k,1/d);
  v[k] := -v[k]
end; { RevSweep Versao de 30/07/98 }

function TwsSymmetric.Hermite: TwsGeneral;
{ Objetivo
    Obt?m forma de Hermite ap?s aplica??o do sweep
}
var
  i,j: Integer;
  v,C: TwsLIVec;
begin
  C := Index(1, NCols);
  v:=TwsLIVec(VecConst(1,NCols,False));
  Sweep(C, v);
  Result := TwsGeneral.Create(NCols, NCols);
  for i := 1 to NCols do
    begin
    for j := 1 to NCols do
      begin
      if i = j then
        if (v[i] = 1) then
          Result[i,j] := 0
        else
          Result[i,j] := 1
      else
        if (v[i] + v[j] <> 0) then
          Result[i,j] := 0
        else
          if (i <= j) then
            Result[i,j] := Get(j,i)
          else
            Result[i,j] := 0;
      end
    end;
  v.Free;
  C.Free
end; { Hermite }

function TwsSymmetric.SweepToHermite(v: TwsLIVec): TwsGeneral;
{ Objetivo
    Obt?m forma de Hermite em uma matriz j? operada pelo sweep
  Par?metros
    v: vetor de 1?s e -1?s decorrentes da aplica??o do sweep
}
var
  i,j: Integer;
begin
  Result := TwsGeneral.Create(NCols,NCols);
  for i := 1 to NCols do
    for j := 1 to NCols do begin
      if i = j then
        if (v[i] = 1) then
          Result[i,j] := 0
        else
          Result[i,j] := 1
      else
        if (v[i] + v[j] <> 0) then
          Result[i,j] := 0
        else
          if (i <= j) then
            Result[i,j] := Get(j, i)
          else
            Result[i,j] := 0;
    end;
end; { SweepToHermite }

function TwsSymmetric.G2Inv: TwsGeneral;
{ Objetivo
    Obt?m a inversa G2 atrav?s de opera??es pelo sweep
}
var
  i,j: Integer;
  C,v: TwsLIVec;
begin
  C:=Index(1, NCols);
  v:=TwsLIVec(VecConst(1,NCols,False));
  Sweep(C, v);
  Result := TwsGeneral.Create(NCols, NCols);
  for i := 1 to NCols do
    begin
    for j := 1 to NCols do
      if (v[i] + v[j] >= 0) then
        Result[i,j] := 0
      else
        Result[i,j] := Get(i, j);
    end;
  v.Free;
  C.Free
end; { G2Inv }

function TwsSymmetric.SweepToG2(v: TwsVec): TwsGeneral;
{ Objetivo
    Obt?m uma inversa G2 em uma matriz j? operada pelo sweep
  Par?metros
    v: vetor de 1?s e -1?s decorrentes da aplica??o do sweep
}
var
  i,j: Integer;
begin
  Result := TwsGeneral.Create(NCols, NCols);
  for i := 1 to NCols do
    begin
    for j := 1 to NCols do
      if (v[i] + v[j] >= 0) then
        Result[i,j] := 0
      else
        Result[i,j] := Get(i,j);
    end
end; { SweepToG2 }


(*procedure TwsSymmetric.RevSweep(k: Integer; DMin: Double; v: TwsVec);
{ Aplica o Sweep reversivel tendo como pivo a coluna k. DMin e o valor minimo
  para o pivo e v contem 1's na entrada e 1's e -1's na saida. v[k]=-1 indica
  que a coluna k ja foi operada. A matriz deve estar na forma mtSymmetric}

  { Objetivo
    Aplica o Sweep reversivel na coluna pivo especificada.
  Par?metros
    k: ?ndice da coluna pivo
    DMin: Controle para opera??o da coluna pivo.
    v: Na entrada a posi??o k cont?m -1 se a coluna j? foi operada ou 1 se
    ainda n?o foi.
}

var
  i, j: Integer;
  b, c, d: Double;
begin
  d := Data[k,k];
  if ((v[k] = 1) and (d < DMin)) then Exit;
  for i := 1 to Count do begin
    if i <> k then begin
      if i < k then                        { Esta na mtTriangular superior? }
        b := Data[k,i]/d
      else
        b := v[i]*v[k]*Data[i,k]/d;
      for j := i to FNCols do begin
        if j <> k then begin
          if k < j then                    { Esta na mtTriangular superior? }
            c := Data[j,k]
          else
            c := v[j]*v[k]*Data[k,j];
          Data[j,i] := Data[j,i] - b*c
        end
      end
    end
  end;
  for i := 1 to k do Data[k,i] := Data[k,i]/d;
  for j := k to FNCols do
    Data[j,k] := -Data[j,k]/d;
  Data[k,k] := 1/d;
  v[k] := -v[k]
end; { RevSweep }
*)
(*
procedure TwsSymmetric.RevSweep(k: Integer; DMin: Double; v: PSIArray);
var
  i, j: Integer;
  b, c, d: Double;
begin
  d := Data[k, k];
  if (v^[k] = 1) and (d <= DMin) then Exit;
  for i := 1 to FNRows do begin
    if i <> k then begin
      if i < k then b := Data[i, k]/d else b := v^[i]*v^[k]*Data[k, i]/d;
      for j := i to FNCols do begin
        if j <> k then begin
          if k < j then c := Data[k, j] else c := v^[j]*v^[k]*Data[j, k];
          Data[i, j] := Data[i, j] - b*c
        end
      end
    end
  end;
  for i := 1 to k do Data[i, k] := Data[i, k]/d;
  for j := k to FNCols do Data[k, j] := -Data[k, j]/d;
  Data[k, k] := 1/d;
  v^[k] := -v^[k]
end; { RevSweep }
*)

procedure TwsSymmetric.Tolerance(F: TwsVec; eps: Double=1.0e-9);
{ Objetivos
    Obtem as toler?ncias para a aplicacao do m?todo RevSweep.
}
var
  i: Integer;
begin
  for i := 1 to FNCols do F[i] := Get(i,i)*eps;
end;

procedure TwsSymmetric.Sweep(Col,v: TwsLIVec);
  { Objetivo
    Aplica o m?todo RevSweep em colunas especificadas.
  Par?metros
    Col: Colunas onde ser?o aplicados os sweeps
    v: Vetor que cont?m valor -1 se a coluna respectiva j? foi alterada e 1 caso contr?rio. O sinal ? trocado a cada opera??o.
}
var
  k: Integer;
  F: TwsVec;
begin
  F := TwsDFVec.Create(FNCols);
  Tolerance(F);
  for k := 1 to Col.Len do
    RevSweep(Col[k], F[Col[k]], v);
  F.Free
end; { TwsSymmetric.Sweep }

procedure TwsSymmetric.SeqSweep(k1, k2: Integer; v: TwsLIVec);
  { Objetivo
    Aplica o m?todo RevSweep sequencialmente nas colunas especificadas.
  Par?metros
    k1, k2: Colunas inicial e final onde ser?o aplicados os sweeps
    v: Vetor que cont?m valor -1 se a coluna respectiva j? foi alterada e 1
    caso contr?rio. O sinal ? trocado a cada opera??o.
}
var
  k: Integer;
  F: TwsVec;
begin
  F := TwsDFVec.Create(FNCols);
  Tolerance(F);
  for k := k1 to k2 do
    RevSweep(k, F[k], v);
  F.Free
end; { TwsSymmetric.SeqSweep }

procedure TwsSymmetric.SweepApp(C: TwsLIVec);
{ Objetivo
    Aplica o sweep nas colunas especificadas
  Par?metros
    C: Colunas onde o sweep ser? aplicado
}
var
  v: TwsLIVec;
begin
  v:=TwsLIVec(VecConst(1,FNCols,False));
  Sweep(C, v);
  v.Free;
end; { TwsSymmetric.SweepApp }

procedure TwsSymmetric.CorrMat;
{ Objetivo
   Transforma uma matriz de SQ&P em uma matriz de correlacoes
}
var
  i, j: Integer;
begin
  for i := 1 to FNRows do
    for j := 1 to i-1 do
      Self[i,j] := Self[i,j]/Sqrt(Self[i,i]*Self[j,j]);
  for i := 1 to FNCols do Self[i,i] := 1
end; { TwsSymmetric.CorrMat }

procedure TwsSymmetric.Solve(B: TwsMatrix; var ErrCode: Word);
{ Objetivo
    Resolve um sistema sim?trico AX = B, onde A ? a matriz que chama o m?todo,
    atraves da decomposicao de Cholesky. Na entrada A aramzena os coeficientes
    do sistema e na sa?da retorna o fator de Cholesky(na parte mtTriangular inferior
    de uma matriz sim?trica)
  Par?metros
    B: Na entrada, armazena em cada coluna os vetores do segundo membro e, na
      saida, retorna a solucao
    ErrCode: C?digo de erro ====> Observar bem o c?digo de erro em situacoes
      envolvendo Cholesky
}
var
  i,j,jx: Integer;
  s     : Double;
begin
  CholeskyFat(i,False);
  if i < FNCols then
    ErrCode := maCholError;
  for jx := 1 to B.FnCols do
    begin
    { Resolvendo para frente }
    if Get(1,1) <> 0 then
      B[1,jx] := B[1,jx]/Get(1,1)
    else
      B[1,jx] := 0;
    for i := 2 to FNRows do
      begin
      s := B[i,jx];
      for j := 1 to i-1 do
        s := s - Get(i,j)*B[j,jx];
      if Get(i,i) <> 0 then
        B[i,jx] := s/Get(i,i)
      else
        B[i,jx] := 0
      end;

    { Resolvendo para tras }
    if Get(FnCols,FNCols) <> 0 then
      B[FnCols,jx] := B[FnCols,jx]/Get(FnCols,FNCols)
    else
      B[FnCols,jx] := 0;
    for i := FNRows-1 downto 1 do
      begin
      s := B[i,jx];
      for j := i+1 to FNCols do
        s := s - B[j,jx]*Get(j,i);
      if Get(i,i) <> 0 then
        B[i,jx] := s/Get(i,i)
      else
        B[i,jx] := 0
      end
    end;
end; { TwsSymmetric.Solve }

procedure TwsSymmetric.InternalSave3(Writer: TWriter);
var i : integer;
begin
  Writer.WriteListBegin;
  For i := 1 To NRows Do
    Row[i].SaveToStream(Writer);
  Writer.WriteListEnd;
end;

procedure TwsSymmetric.InternalLoad3(Reader: TReader);
begin
  //While Count <> 0 Do Delete(0);   {N?o sei por que ????}
  Reader.ReadListBegin;
  While Not Reader.EndOfList Do
    PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;
end;

{ TwsTriangular }

constructor TwsTriangular.Create(NC: Integer);
{ Objetivo
    Cria e inicializa um objeto matriz mtTriangular com dimens?o especificada.
    Uma matriz mtTriangular somente os valores da mtTriangular inferior s?o armazenados.
    Assim, o primeiro vetor (linha) tem dimens?o um, o segundo dois e assim por diante.
    Se um valor da mtTriangular for necess?rio, o m?todo de acesso retorna 0 (zero).
  Par?metros
    NC: N?mero de linhas e colunas da matriz
}
var
  i: Integer;
begin
  inherited Create(NC, NC);
  FMatType := mtTriangular;
  If NC > 0 Then
     for i := 1 to FNCols do
       FList.Add(TwsDFVec.Create(i))
end; { TwsTriangular.Create }

function TwsTriangular.GetBlockName: String;
begin
  Result := 'wsGeneral';
end;

function TwsTriangular.GetBlockNameComment: String;
begin
  Result := 'Estilo de formata??o para Matrizes do tipo TwsTriangular';
end;


function TwsTriangular.Get(i, j: Integer): Double;
{ Objetivo
    Retornar o valor da posi??o especificada
  Par?metros
    i, j: Linha e coluna, respectivamente, do valor. Se i < j (mtTriangular superior)
    o valor retornado ? zero.
}
begin
  if i >= j then
    Get := Row[i].Data[j]
  else
    Get := 0
end; { TwsTriangular.Get }

procedure TwsTriangular.Put(i, j: Integer; x: Double);
{ Objetivo
    Atribui um valor ? posi??o especificada
  Par?metros
    i, j: Linha e coluna, respectivamente, para atrubui??o do valor.
    x: Valor a ser atribu?do. Atribui somente se i <= j
}
begin
  if i >= j then
     Begin
     Row[i].Data[j] := x;
     FModified := True;
     End;
end; { TwsTriangular.Put }

procedure TwsTriangular.List(Buffer: TStrings);
{ Objetivo
    Lista os descritores de uma matriz geral
  M?todos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtTriangular');
  inherited List(Buffer)
end;

procedure TwsTriangular.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    Copia matriz mtTriangular.
  Par?metros
    MT: tipo para a qual ser? copiada a matriz. MT dever? um dos tipos
      mtGeneral
      mtSymmetric
      mtDiagonal
      mtTriangular
      mtToeplitz
      mtVandermonde
    Matrix: local onde retorna a c?pia
  M?todos chamados
    Copy herdado
  Campos modificados
    Nenhum
}
var
  i,j: Integer;
  L0 : TwsVec;
begin
  case MT of
    mtGeneral:
      begin
      Matrix := TwsGeneral.Create(NRows, NCols);
      for i := 1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j := 1 to NCols do
          Matrix[i,j]:= Self[i,j]
        end
      end;

    mtSymmetric:
      begin
      Matrix := TwsSymmetric.Create(NCols);
      for i:=1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j]:=Self[i,j]
        end
      end;

    mtTriangular:
      begin
      Matrix := TwsTriangular.Create(NCols);
      for i:=1 to NRows do
        begin
        Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j] := Self[i,j]
        end
      end;

    mtDiagonal:
      begin
      Matrix := TwsDiagonal.Create(NCols);
      L0 := Matrix.Row[1];
      for j:=1 to NCols do
        L0[j]:= Self[j,j]
      end;

    mtToeplitz,mtVandermonde:
      begin
      Matrix := nil;
      Exit
      end;
  end; { case }
  inherited Copy(MT, Matrix)
end; { Copy }

function TwsTriangular.TranspMul1(B: TwsMatrix; var ErrCode: Word): TwsMatrix;
{ Objetivo
    Faz o produto da matriz pela transposta da outra sem que a transposi??o seja
    explicitamente realizada
  Par?metros
    B: Matriz que ser? transposta para o produto
    ErrCode: C?digo de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contr?rio
  Retorno
    Se A ? a matriz que chama o m?todo, retorna uma matriz geral com o produt AB'
  Valores perdidos
    N?o trata
}
var
  i,j,k:Integer;
  aux  : Double;
begin
  ErrCode := 0;
  if NCols = B.NCols then
    begin
    case B.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        Result := TwsGeneral.Create(NRows, B.NRows);
        for i := 1 to NRows do
          begin
          for j := 1 to Result.NCols do
            begin
            aux := 0;
            for k := 1 to i do
              aux := aux + Self[i, k]*B[j, k];
            Result[i,j]:=aux;
            end
          end
        end;

      mtDiagonal: Prod5(TwsDiagonal(B),Self,False,TwsTriangular(Result));

      mtTriangular:
        begin
        Result := TwsGeneral.Create(NRows, B.NRows);
        for i := 1 to NRows do
          begin
          for j := 1 to B.NRows do
            begin
            aux := 0;
            for k := 1 to Math.Min(i,j) do
              aux := aux + Self[i, k]*B[j, k];
            Result[i,j]:=aux;
            end
          end
        end
      end // case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul1 }

function TwsTriangular.TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix; {A'B}
{ Objetivo
    Produto de uma matriz traingular transposta por uma outra sem que a transposi??o seja
    explicitamente realizada
  Par?metros
    B: Matriz com a qual o produto ser? realizado
    ErrCode: C?digo de erro. retorna 0 (zero) se NRows = B.NRows e NImprDim (dimens?es
    impr?prias para a opera??o), caso contr?rio
  Retorno
  Valores perdidos
    N?o considera
}
var
  i,j,k:Integer;
  aux  : Double;
begin
  if NRows = B.NRows then
    begin
    ErrCode := 0;
    case B.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        Result := TwsGeneral.Create(NCols, B.NCols);
        for i := 1 to NCols do
          for j := 1 to B.NCols do
            begin
            aux := 0;
            for k := 1 to NRows do
              aux := aux + Self[k, i]*B[k, j];
            Result[i,j]:=aux;
            end
        end;

      mtDiagonal:
        begin
        Result:=Jota(NCols, B.NCols,mtGeneral,0);
        for i:=1 to NRows do
          for j:=i to NCols do
            Result[i,j]:=Self[j,i]*B[j,j]
        end;

      mtTriangular:
        begin
        Result := TwsGeneral.Create(NCols, B.NCols);
        for i := 1 to NCols do
          for j := 1 to B.NCols do
            begin
            aux := 0;
            for k := Math.Max(i,j) to NRows do
              aux := aux + Self[k, i]*B[k, j];
            Result[i,j]:=aux;
            end
        end
      end // case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul2 }

function TwsTriangular.TranspMul3: TwsMatrix;
{ Objetivo
    Faz o produto da matriz (geral, sim?trica, mtVandermonde ou mtToeplitz) pela sua transposta
    sem que a transposi??o seja explicitamente realizada
  Retorno
    Retorna a matriz sim?trica AA', exceto se for mtDiagonal. Nesse caso, o retorno ? uma
    matriz mtDiagonal
  Valores perdidos
    N?o trata
}
var
  i,j,k:Integer;
  aux  : Double;
begin
  Result := TwsSymmetric.Create(NRows);
  for i := 1 to NRows do
    begin
    for j := 1 to i do
      begin
      aux := 0;
      for k := 1 to j do
        aux := aux + Self[i, k]*Self[j, k];
      Result[i,j]:=aux;
      end
    end
end; { TranspMul3 }

function TwsTriangular.TranspMul4: TwsMatrix;        { A'A }
{ Objetivo
    Produto de uma matriz transposta por si mesma sem que a transposi??o seja explicitamente
    realizada
  Retorno
    Retorna uma matriz sim?trica exceto no caso em que a matriz ? mtDiagonal
  Valores perdidos
    N?o considera
}
var
  i,j,k:Integer;
  aux  : Double;
begin
  Result := TwsSymmetric.Create(NCols);
  for i := 1 to NCols do
    for j := 1 to i do
      begin
      aux := 0;
      for k := j to NRows do
        aux := aux + Self[k, i]*Self[k, j];
      Result[i,j]:=aux
      end
end; { TranspMul4 }

function TwsTriangular.TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix;   { A'BA }
{ Objetivo
    Dadas a matriz gerais A qualquer e B mtTriangular (que chama o m?todo), retorna o produto
    A'BA sem efetuar a transposicao explicitamente.
  Parametros
    A: matriz para o produto
    ErrCode: Codigo de erro. Retorna NImprDim (dimensoes improprias para a operacao)
    se (A.NRows <> B.NRowss)
  Retorno
    Retorna uma matriz geral
}
var
  i,j,k:Integer;
  s    : Double;
  aux  : TwsVec;
begin
  if (A.NRows=NRows) then
    begin
    ErrCode := 0;
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        Result:= TwsGeneral.Create(A.NCols,A.NCols);
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NCols do
          begin
          { Faz o produto da coluna i de A por cada coluna de B e armazena em aux }
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=j to A.NRows do
              s:=s+A[k,i]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to A.NCols do
            begin
            s:=0;
            for k := 1 to A.NRows
              do s:=s+aux[k]*A[k,j];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
        aux.Free;
        end; // mtGeneral, ...

      mtDiagonal:
        begin
        aux:=A.Row[1];
        Result := TwsTriangular.Create(A.NRows);
        for i := 1 to A.NRows do // Parac cada linha de A
          for j := 1 to i do     // Para cada coluna de B
            Result[i,j] := aux[i]*aux[j]*Self[i,j];
        end;

      mtTriangular:
        begin
        Result:= TwsGeneral.Create(A.NCols,A.NCols);
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NCols do
          begin
          { Faz o produto da coluna i de A por cada coluna de B e armazena em aux }
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=i to A.NRows do
              s:=s+A[k,i]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to A.NCols do
            begin
            s:=0;
            for k := j to A.NRows
              do s:=s+aux[k]*A[k,j];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
        aux.Free;
        end; // mtTriangular
      end; //case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul5 }

function TwsTriangular.TranspMul6(v: TwsVec; var ErrCode: Word): Double;       { v'Av }
{ Objetivo
    Retorna o produto do vetor transposto pela matriz e novamente o vetor
  Par?metros
    v: Vetor para o produto
    ErrCode: C?digo de erro. Se v.Len=NRows=NCols retorna 0; caso contr?rio retorna
    NImprDim (dimens?es impropr?prias para a opera??o)
  Retorno
    Se A ? a matriz que chama retorna o escalar correspondente ao produto v'Av; se o produto
    n?o for poss?vel retorna wscMissValue
  Valores perdidos
    N?o trata
}
var
  i,j,n: Integer;
  aux  : Double;
begin
  n := v.Len;
  if ((n=NRows) and (n=NCols)) then
    begin
    Result := 0;
    for j := 1 to n do
      begin
      aux := 0;
      for i := j to n do
        aux := aux + v[i]*Self[i, j];
      aux := aux*v[j];
      Result := Result + aux
      end;
    end
  else
    begin
    ErrCode := NImprDim;
    Result := wscMissValue
    end
end; { TranspMul6}

function TwsTriangular.TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix;   { ABA' }
{ Objetivo
    Dadas a matriz gerais A qualquer e B mtTriangular (que chama o m?todo), retorna o produto
    ABA' sem efetuar a transposicao de A explicitamente.
  Parametros
    A: matriz para o produto
    ErrCode: Codigo de erro. Retorna NImprDim (dimensoes improprias para a operacao)
    se (A.NCols <> B.NRows)
  Retorno
    Retorna uma matriz geral exceto quando A ? mtDiagonal, quando o retorno ? mtTriangular
}
var
  i,j,k:Integer;
  s    : Double;
  aux  : TwsVec;
begin
  if (A.NCols=NRows) then
    begin
    ErrCode := 0;
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        Result:= TwsGeneral.Create(A.NRows,A.NRows);
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NRows do  // Para cada linha de A
          begin
          for j := 1 to NCols do // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=j to A.NCols do
              s:=s+A[i,k]*Self[k,j];
            aux[j]:=s;
            end; // para cada coluna de B
          { Faz o produto do vetor aux por cada coluna de A' }
          for j:= 1 to A.NRows do
            begin
            s:=0;
            for k := 1 to A.NCols
              do s:=s+aux[k]*A[j,k];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
        aux.Free;
        end; // mtGeneral, ...

      mtDiagonal:
        begin
        aux:=A.Row[1];
        Result := TwsTriangular.Create(A.NRows);
        for i := 1 to A.NRows do // Parac cada linha de A
          for j := 1 to i do     // Para cada coluna de B
            Result[i,j] := aux[i]*aux[j]*Self[i, j];
        end;

      mtTriangular:
        begin
        Result:= TwsGeneral.Create(A.NRows,A.NRows);
        aux := TwsDFVec.Create(NCols);
        for i:=1 to A.NRows do  // para cada linha de A
          begin
          for j := 1 to i do    // faz o produto para cada coluna de B
            begin
            s:=0;
            for k:=j to i do
              s:=s+A[i,k]*Self[k,j];
            aux[j]:=s;
            end;
          { Faz o produto do vetor aux por cada coluna de A }
          for j:= 1 to A.NRows do
            begin
            s:=0;
            for k := 1 to i
              do s:=s+aux[k]*A[j,k];
            Result[i,j]:=s; // Guarda o resultado
            end
          end; // para cada coluna de A
        aux.Free;
        end; // mtTriangular
      end; //case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul10 }

function TwsTriangular.TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double;
{ Objetivo
    Retorna o produto da coluna k da matriz A pela matriz na forma B[k]'*A*B[k], onde B[k]
    ? a coluna.
  Par?metros
    B: Matriz para o produto
    k: ?ndice da coluna para o produto
    ErrCode: C?digo de erro. Se NRows=NCols=A.NRows retorna 0; caso contr?rio retorna
      NImprDim (dimens?es impropr?prias para a opera??o
  Retorno
    Se o produto n?o for poss?vel, retorna wscMissValue
  Valores perdidos
    N?o trata
}
var
  i,j: Integer;
  aux: Double;
begin
  if ((B.NRows=NRows) and (NRows=NCols)) then
    begin
    Result := 0;
    case B.MatType of
      mtGeneral, mtSymmetric, mtToeplitz, mtVandermonde:
        for j := 1 to B.NRows do              // para cada coluna da matriz
          begin
          aux := 0;
          for i := 1 to B.NRows do
            aux := aux + B[i,k]*Self[i, j];
          Result := Result + aux*B[j,k];      // Completa o produto
          end;
      mtTriangular:
        for j := k to B.NRows do              // para cada coluna da matriz
          begin
          aux := 0;
          for i := k to B.NRows do
            aux := aux + B[i,k]*Self[i, j];
          Result := Result + aux*B[j,k];      // Completa o produto
          end;
      mtDiagonal:
        Result := Sqr(B[k,k])*Self[k,k];
      end // case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := wscMissValue
    end
end; { TranspMul11 }


function TwsTriangular.Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix;
{ Objetivo
    Metodo para o produto de matrizes. Utilizado quando a matriz que chama o metodo ?
    mtTriangular
  Par?metros
    B: Matriz para o (pos)produto com a matriz que chama o m?todo
    ErrCode: C?digo de erro. O produto somente sera poss?vel (ErrCode retorna 0) se o
      n?mero de colunas de quem chama ? igual ao n?mero de linhas de B; caso contr?rio
      ErrCode retorna a constante NImprDim (Dimens?es impr?prias para a opera??o) e a
      fun??o retorna nil
  Retorno
    Se B for
      mtGeneral,Symetric,mtVandermonde,mtToeplitz: retorna matriz geral
      mtDiagonal: retorna matriz mtTriangular
      mtTriangular: Retorna matriz mtTriangular
}
begin
  ErrCode := 0;
  if (NCols = B.NRows) then
    begin
    case B.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz: Prod3(B,Self,True,TwsGeneral(Result));
      mtDiagonal: Prod5(TwsDiagonal(B),Self,False,TwsTriangular(Result));
      mtTriangular:Prod6(Self,TwsTriangular(B),TwsTriangular(Result));
    end; // case
    end
  else
    ErrCode := NImprDim;
end;

function TwsTriangular.VecMult(v:TwsVec; PreMult:Boolean; Var ErrCode:Word): TwsDFVec;
{Objetivo
   Fazer o produto de uma matriz por um vetor
 Par?matros
   A: Vetor para o produto
   PreMult: True se o vetor premultiplicar a matriz; False caso contrario
   ErrCode: C?digo de erro. Retorna 0 se o produto for poss?vel e NImprDim (dimens?es
     impr?prias para a opera??o) se a) n?mero de componentes do vetor diferente do n?mero
     de linhas da matriz (PreMult=True); b) n?mero de colunas da matriz diferente do n?mero
     de componentes do vetor (PreMult=False)
   Retorno
     Retorna sempre um vetor
}
var
  i,j: Integer;
  aux: Double;
begin
  ErrCode:=0;
  if PreMult then { premultiplicacao da matriz pelo vetor }
    if (v.Len = NRows) then
      begin
      Result:=TwsDFVec.Create(NCols);
      for j:=1 to NCols do
        begin
        aux:=0;
        for i:=j to NRows do
          aux:=aux+v[i]*Self[i,j];
        Result[j]:=aux
        end
      end
    else
      begin
      ErrCode:=NImprDim;
      Result:=nil;
      Exit
      end
  else
    if (v.Len=NCols) then
      begin
      Result:=TwsDFVec.Create(NRows);
      for i:=1 to NRows do
        begin
        aux:=0;
        for j:=1 to i do
          aux:=aux+v[j]*Self[i,j];
        Result[i]:=aux
        end
      end
    else
      begin
      ErrCode:=NImprDim;
      Result:=nil;
      Exit
      end
end;

function TwsTriangular.ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst, NewMat:Boolean):TwsMatrix;
{Objetivo
   Efetuar opera??es entre uma matriz mtTriangular e um escalar
 Par?metros
   x: Escalar para a opera??o
   Op: Tipo de opera??o desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas opera??es (soma ou produto) esse par?metro n?o ? utilizado.
     No caso de potencia??o, SFirst=True indica que o escalar estar? na base e o elemento
     na pot?ncia; caso contr?rio, o elemento estar? na base e o escalar na pot?ncia. Para
     o operador ** (se SFirst=False), a opera??o corresponder? ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contr?rio a opera??o n?o ? definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz.
 Retorno
   O retorno ira depender do tipo de opera??o desejada. Para alguns tipos, o retorno ser?
     mtDiagonal enquanto que para outros ser? geral. A rela??o ser?:
     opDiv: Se SFirst=False retorna matriz mtTriangular; geral caso contr?rio
     opProd: mtTriangular
     opPower: Se SFirst=False retorna matriz mtTriangular; geral caso contr?rio
     opSum,opSub,opGE,opGT,opLE,opLT,opEQ,OpNE,OpOR,OpAnd,OpMax,OpMin: Geral
   Nas situa??es onde n?o ? poss?vel o retorno de uma mtDiagonal, sempre retornar? uma matriz
     geral, independemente de NewMat
 Valores perdidos
   Se houver um valor perdido na opera??o envolvendo um elemento, o resultado ? valor perdido
}
var
  i: Integer;
begin
  if NewMat then
    case Op of
      opSum,opSub,opGE,opGT,opLE,opLT,opEQ,opNE,opOR,opAnd,opMax,opMin:
        Self.Copy(mtGeneral,Result);
      opDiv:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Self.Copy(mtTriangular,Result);
      opProd:
        Self.Copy(mtTriangular,Result);
      opPower:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Self.Copy(mtTriangular,Result);
    end // case
  else
    case Op of
      opSum,opSub,opGE,opGT,opLE,opLT,opEQ,OpNE,OpOR,OpAnd,OpMax,OpMin:
        Self.Copy(mtGeneral,Result);
      opDiv:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Result:=Self;
      opProd:
        Result:=Self;
      opPower:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Result:=Self;
    end; // case
  for i:=1 to Result.NRows do
    Result.Row[i].ByScalar(x,Op,False,SFirst);
end;

procedure TwsTriangular.Inv(var ErrCode: Word; eps: Double=1.0e-8);
{ Objetivo
    Inverte a matriz mtTriangular inferior
  Par?metros
    ErrCode: Retorna 0 se a invers?o ocorre sem problemas; maInvError caso contr?rio
    eps: Precis?o num?rica para indica??o de matriz singular. A condi??o para que a matriz
      seja considerada singular ? ter um elemento da mtDiagonal menor que eps.
}
var
  i,j,l: Integer;
  s    : Extended;
begin
  ErrCode:=0;
  for i := 1 to FNCols do
    begin
    if Abs(Get(i,i)) > eps then
      Put(i,i,1/Get(i,i))
    else
      begin // Matriz singular
      ErrCode:=maInvError;
      Exit
      end
    end;
  for i := 1 to FNCols-1 do
    for j := i+1 to FNCols do
      begin
      s := 0;
      for l := i to j-1 do
        s := s + Get(l,i)*Get(j,l);
      Put(j,i,-Get(j,j)*s)
      end;
end; { Inv }

procedure TwsTriangular.FwdSolve(B: TwsGeneral; var ErrCode: Word);
{ Objetivo
    Resolve um sistema triangular para frente, isto ?, da primeira ? ?ltima equa??o 
  Par?metros
    B: Na entrada, armazena em cada coluna os vetores do segundo membro e, na
      saida, retorna a solucao
    ErrCode: C?digo de erro ====> Observar bem o c?digo de erro em situacoes
      envolvendo Cholesky
}
var
  i,j,jx: Integer;
  s     : Double;
begin
  ErrCode:=0;
  for jx := 1 to B.FnCols do
    begin
    { Resolvendo para frente }
    if Get(1,1) <> 0 then
      B[1,jx] := B[1,jx]/Get(1,1)
    else
      begin
      B[1,jx] := 0;
      ErrCode:=maInvError
      end;
    for i := 2 to FNRows do
      begin
      s := B[i,jx];
      for j := 1 to i-1 do
        s := s - Get(i,j)*B[j,jx];
      if Get(i,i) <> 0 then
        B[i,jx] := s/Get(i,i)
      else
        begin
        B[i,jx] := 0;
        ErrCode:=maInvError
        end
      end;
    end
end; // FwdSolve

procedure TwsTriangular.InternalSave3(Writer: TWriter);
var i : integer;
begin
  Writer.WriteListBegin;
  For i := 1 To NRows Do
    Row[i].SaveToStream(Writer);
  Writer.WriteListEnd;
end;

procedure TwsTriangular.InternalLoad3(Reader: TReader);
begin
  //While Count <> 0 Do Delete(0);   {N?o sei por que ????}
  Reader.ReadListBegin;
  While Not Reader.EndOfList Do
    PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;
end;

{ TwsDiagonal }

{ Inicializa objeto TwsDiagonal }

constructor TwsDiagonal.Create(NC: Integer);
{ Objetivo
    Cria uma matriz mtDiagonal de dimens?o especificada. Numa matriz mtDiagonal apenas os elementos
    da mtDiagonal s?o armzenados. O m?todo de acesso se encarrega de zerar o resultado quando se
    desejar algum valor fora da mtDiagonal
  Par?metros
    NC: N?mero de linhas e colunas da matriz. Se NC>0 insere uma linha vazia
  M?todos chamados
    Create herdado
    FMatType
}
begin
  inherited Create(NC, NC);
  FMatType := mtDiagonal;
  If NC > 0 Then
     FList.Add(TwsDFVec.Create(FNCols))
end; { TwsDiagonal.Create }

function TwsDiagonal.GetBlockName: String;
begin
  Result := 'wsGeneral';
end;

function TwsDiagonal.GetBlockNameComment: String;
begin
  Result := 'Estilo de formata??o para Matrizes do tipo TwsDiagonal';
end;

procedure TwsDiagonal.RowsToXML(Writer: TXML_Writer);
var j: Integer;
    s: String;
    b: Boolean;
    Cor: TColor;
    v: TwsVec;
begin
  b := (Assigned(FSelectElementColor));
  if FNRows = 0 then Exit;
  v := Row[1];
  if v.Name <> '' then
     s := '<row Label="' + XML_EncodeChars(v.Name, True) + '">'
  else
     s := '<row Label="1">';

  for j := 1 to FnCols do
    if b then
       begin
       Cor := clblack;
       FSelectElementColor(Self, j, j, Cor);
       if Cor <> clBlack then
          s := s + '<e cor="#' + RGBColorAsString(Cor) + '">'
       else
          s := s + '<e>';
       s := s + SysUtilsEx.AllTrim(getStr(j,j)) + '</e>';
       end
    else
       s := s + '<e>' + SysUtilsEx.AllTrim(getStr(j,j)) + '</e>';
  Writer.Write(s + '</row>');
end;

function TwsDiagonal.Get(i,j: Integer): Double;
{ Objetivo
    Retorna um elemento da matriz mtDiagonal. Uma matriz mtDiagonal possui somente um vetor para
    armazenamento, onde s?o colocados os valores da mtDiagonal.
  Par?metros
    i: ?ndice de linha
    j: ?ndice de coluna do elemento.
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observa??o
    Retorna 0 se i forem diferentes
}
begin
  if i = j then
    Get := Row[1].Data[j]
  else
    Get := 0
end; { TwsDiagonal.Get }

procedure TwsDiagonal.Put(i,j: Integer; x: Double);
  { Objetivo
    Atribui um elemento da matriz mtDiagonal. A atribui??o somente ser? feita se
    for na mtDiagonal
  Par?metros
    i, j: Respectivamente, linha e coluna para atribui??o do elemento.
}
begin
  if i = j then
     Begin
     FModified := True;
     Row[1].Data[j] := x;
     End;
end; { TwsDiagonal.Put }

procedure TwsDiagonal.List(Buffer: TStrings);
{ Objetivo
    Lista os descritores da matriz
  M?todos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtDiagonal');
  inherited List(Buffer)
end;

function TwsDiagonal.GetStr(i,j:Integer): String;
{ Objetivo
    Retorna um elemento como string.
  Par?metros
    i: ?ndice de linha
    j: ?ndice de coluna
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observa??o
    Retorna ?-? se for valor perdido. Aplica a fun??o Fuzz para eliminar d?gitos que est?o
    fora da precis?o especificada.
}
var
  x : Double;
begin
  x:=Row[1].Data[j];
  if not wsGLib.IsMissValue(x) then
    Result:=Format('%*.*g',[PrintOptions.ColWidth, PrintOptions.ColPrecision, Fuzz(x)])
  else
    Result:=Format('%*s',[PrintOptions.ColWidth, '-'])
end;

procedure TwsDiagonal.Print(Buffer: TStrings);
{ Objetivo
    Lista a matriz mtDiagonal com sa?da no modo texto
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
var
  i, j, PrevI,
  NumCol: Integer;
  P     : String;
  Aux   : Double;
begin
  i := 0;
  repeat          { Imprime submatrizes com tantas colunas quanto possivel }
    PrevI := i;
    NumCol := 0;
    j := PrevI + 1;
    repeat                              { Quantas colunas serao impressas? }
      Inc(j);
      Inc(i);
      Inc(NumCol);
    until (j > FNCols) or (NumCol*PrintOptions.ColWidth >= (PrintOptions.LineLen - PrintOptions.MaxIDSize));
    if (PrintOptions.LineLen - PrintOptions.MaxIDSize) <= NumCol*PrintOptions.ColWidth then
      begin
      Dec(NumCol);
      Dec(i)
      end;

    If MLab <> '' Then
       if PrintOptions.Center Then
          Buffer.Add(StrCenter(MLab, 110))
       else
          Buffer.Add(MLab);

    P := LeftPad(FName, PrintOptions.MaxIDSize);
    if FCName <> nil then
      for j := 0 to NumCol-1 do
        AppendStr(P, Format('%*s', [PrintOptions.ColWidth, FCName[PrevI+j]]))
    else
      for j := 0 to NumCol-1 do
        AppendStr(P, Format('%*s', [PrintOptions.ColWidth, 'Col'+IntToStr(PrevI+j+1)]));

    if PrintOptions.Center Then
       Buffer.Add(StrCenter(P, 110))
    else
       Buffer.Add(P);

     { Constroi linha de tamanho MaxLen ou esgotando colunas para saida }
    P := Row[1].Name;
    if P = '' then
      P := LeftPad('Diag', PrintOptions.MaxIDSize)
    else
      P := LeftPad(P, PrintOptions.MaxIDSize);
    for j := 1 to NumCol do
      AppendStr(P, GetStr(1,PrevI+j));

    if PrintOptions.Center Then
       Buffer.Add(StrCenter(P, 110))
    else
       Buffer.Add(P);
  until i = FNCols;                             { Esgota todas as colunas }
  Buffer.Add('')
end; { TwsDiagonal.Print }

procedure TwsDiagonal.ColPrint(const Cols: String; Expr: string; Buffer: TStrings);
{ Objetivo
    Constroi um string com valores das colunas especificadas e o imprime se a linha
     respectiva satisfaz ? condi??o estabelecida
  Par?metros
    Cols: String que estabelece as colunas que ser?o impressas
    Condicao: Express?o que ir? indicar se a linha ser? impressa ou n?o.
     (Ver Express?es)
}
var
  i, j, PrevI,
  NumCol: Integer;
  P     : String;
  Aux   : Double;
  L0: TwsVec;
  C: TwsLIVec;
begin
  if Cols <> '' then
    C := IndexColsFromString(Cols)
  else
    C := Index(1, FNCols);
   { Se a expressao nao e vazia e' processada e colocada no formato pos-fixado }
  i := 0;
  L0 := Row[1];
  repeat              { Imprime submatrizes com tantas colunas quanto possivel }
    PrevI := i;                                       { Ultima coluna impressa }
    NumCol := 0;                                { Numero de colunas a imprimir }
    j := PrevI + 1;
    repeat
      Inc(j);
      Inc(i);
      Inc(NumCol);
    until (j > C.Len) or (NumCol*PrintOptions.ColWidth >= (PrintOptions.LineLen - PrintOptions.MaxIDSize));
    if (PrintOptions.LineLen - PrintOptions.MaxIDSize) <= NumCol*PrintOptions.ColWidth then
      begin
      Dec(NumCol);
      Dec(i)
      end;
    P := Format('%*s', [PrintOptions.MaxIDSize, FName]);
    if FCName = nil then
      for j := 1 to NumCol do
        AppendStr(P, Format('%*s', [PrintOptions.ColWidth, 'Col'+IntToStr(C[PrevI + j])]))
    else
      for j := 1 to NumCol do
        AppendStr(P, Format('%*s',[PrintOptions.ColWidth, FCName.Strings[C[PrevI + j]-1]]));

    if PrintOptions.Center Then
      Buffer.Add(StrCenter(P, 110))
    else
      Buffer.Add(P);

     { Constroi linha de tamanho MaxLen ou esgotando colunas para saida }
    P := L0.Name;
    if P = '' then
      P := LeftPad('Diag', PrintOptions.MaxIDSize)
    else
      P := LeftPad(P, PrintOptions.MaxIDSize);
    for j := 1 to NumCol do
      begin
      Aux := L0[C[PrevI + j]];
      if Aux <> wscMissValue then
        AppendStr(P, Format('%*.*g', [PrintOptions.ColWidth, PrintOptions.ColPrecision, Fuzz(Aux)]))
      else
        AppendStr(P, Format('%*s', [PrintOptions.ColWidth, '-']))
      end;

    if PrintOptions.Center Then
      Buffer.Add(StrCenter(P, 110))
    else
      Buffer.Add(P);

  until i = C.Len;                               { Esgota todas as colunas }
  C.Free;
  Buffer.Add('');
end; { TwsDiagonal.ColPrint }

// Mostra a matriz em uma planilha semelhante as planilhas do Excel
procedure TwsDiagonal.ShowInSheet(Sheet: TBaseSpreadSheetBook);
var s: String;
    C: Integer;
begin
  Sheet.ActiveSheet.ShowHeaders := False;

  StartWait();
  Sheet.BeginUpdate();
  try
    if Name <> '' then s := ' ' + Name else s := ' Sem Nome';
    if MLab <> '' then
       Sheet.Caption := s + ' - ' + MLab
    else
       Sheet.Caption := s;

    // Nome das colunas
    for C := 1 to nCols do
      begin
      Sheet.ActiveSheet.WriteCenter(1, C+1, ColName[C]);
      Sheet.ActiveSheet.BoldCell(1, C+1);
      end;

    Sheet.ActiveSheet.Write(2,1,True,'Diag');
    for C := 1 to nCols do
      Sheet.ActiveSheet.WriteCenter(2,C+1,GetStr(C,C))
  finally
    Sheet.EndUpdate();
    StopWait();
  end;
end;

function TwsDiagonal.ToVec: TwsVec;
{ Objetivo
    Retorna um vetor com os componentes da mtDiagonal
}
var
  i: Integer;
begin
  Result := TwsDFVec.Create(NRows);
  for i := 1 to Result.Len do
    Result[i] := Self[i,i]
end;

function TwsDiagonal.DiagToVec: TwsVec;
{ Objetivo
    Copia todo conte?do (incluindo 0?s fora da mtDiagonal) para um vetor
  M?todos chamados
    ToVec
  Campos modificados
    Nenhum
}
begin
  Result := ToVec
end;

procedure TwsDiagonal.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    C?pia da matriz
  Par?metros
    MT: Tipo da matriz de retorno
    Matrix: Matriz que retorna com a c?pia. Se tipo depender? de MT (mtGeneral, mtSymmetric,
      mtDiagonal, mtTriangular, mtVandermonde ou mtToeplitz)
}
var
  i, j: Integer;
  L0,L1: TwsVec;
begin
  case MT of
    mtGeneral:
      begin
      Matrix := TwsGeneral.Create(NRows, NCols);
      for i := 1 to NRows do
        for j := 1 to NCols do
          Matrix[i,j]:= Self[i,j]
      end;

    mtSymmetric:
      begin
      Matrix := TwsSymmetric.Create(NCols);
      for i:=1 to NRows do
        for j := 1 to i do
         Matrix[i,j]:=Self[i,j];
      end;

    mtTriangular:
      begin
      Matrix := TwsTriangular.Create(NCols);
      for i:=1 to NRows do
        for j := 1 to i do
        Matrix[i,j] := Self[i,j]
      end;

    mtDiagonal:
      begin
      Matrix := TwsDiagonal.Create(NCols);
      L0 := Matrix.Row[1];
      L1 := Row[1];
      for j:=1 to NCols do
        L0[j]:= L1[j]
      end;

    mtToeplitz,mtVandermonde:
      begin
      Matrix := nil;
      Exit
      end;
  end; { case }
  inherited Copy(MT, Matrix)
end; { Copy }

function TwsDiagonal.TranspMul1(B:TwsMatrix; var ErrCode:Word): TwsMatrix; {AB'}
{ Objetivo
    Faz o produto da matriz mtDiagonal pela transposta da outra sem que a transposi??o seja
    explicitamente realizada
  Par?metros
    B: Matriz que ser? transposta para o produto
    ErrCode: C?digo de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contr?rio
  Retorno
    Se A ? a matriz que chama o m?todo, retorna
      - uma matriz geral com o produt AB' se B for geral, sim?trica, mtTriangular, mtVandermonde
        ou mtToeplitz
      - uma matriz mtDiagonal se B for mtDiagonal
  Valores perdidos
    N?o trata
}
var
  i,j:Integer;
  L  : TwsVec;
begin
  ErrCode := 0;
  if NCols = B.NCols then
    begin
    L:=Row[1];
    case B.MatType of
      mtGeneral,mtSymmetric,mtTriangular,mtToeplitz,mtVandermonde:
        begin
        Result := TwsGeneral.Create(NRows, B.NRows);
        for i := 1 to NRows do
          for j := 1 to Result.NCols do
            Result[i,j]:=L[i]*B[j,i];
        end;

      mtDiagonal: Prod4(Self,TwsDiagonal(B),TwsDiagonal(Result));

      end // case
    end // if NCols =
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul1 }

function TwsDiagonal.TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix; { A'B }
{ Objetivo
    Faz o produto da matriz mtDiagonal transposta por uma outra sem que a transposi??o seja
    explicitamente realizada
  Par?metros
    B: Matriz para o produto
    ErrCode: C?digo de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contr?rio
  Retorno
    Se A ? a matriz que chama o m?todo, retorna uma matriz geral com o produt A'B
  Valores perdidos
    N?o trata
}
begin
  if NRows = B.NRows then
    begin
    ErrCode := 0;
    case B.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        Prod2(B,Self,True,TwsGeneral(Result));
      mtDiagonal:
        Prod4(Self,TwsDiagonal(B),TwsDiagonal(Result));
      mtTriangular:
        Prod5(Self,TwsTriangular(B),True,TwsTriangular(Result))
      end // case
    end // if
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; // TranspMul2

function TwsDiagonal.TranspMul3: TwsMatrix;
{ Objetivo
    Faz o produto da matriz (geral, sim?trica, mtVandermonde ou mtToeplitz) pela sua transposta
    sem que a transposi??o seja explicitamente realizada
  Retorno
    Retorna a matriz sim?trica AA', exceto se for mtDiagonal. Nesse caso, o retorno ? uma
    matriz mtDiagonal
  Valores perdidos
    N?o trata
}
var
  i:Integer;
  L1,L2: TwsVec;
begin
  Result:=TwsDiagonal.Create(NRows);
  L1:=Result.Row[1];
  L2:=Row[1];
  for i:=1 to NRows do
    L1[i]:=Sqr(L2[i])
end; { TranspMul3 }

function TwsDiagonal.TranspMul4: TwsMatrix;        { A'A }
{ Objetivo
    Produto de uma matriz transposta por si mesma sem que a transposi??o seja explicitamente
    realizada
  Retorno
    Retorna uma matriz sim?trica exceto no caso em que a matriz ? mtDiagonal
  Valores perdidos
    N?o considera
}
begin
  Prod4(Self,Self,TwsDiagonal(Result));
end; { TranspMul4 }

function TwsDiagonal.TranspMul5(A:TwsMatrix; var ErrCode:Word):TwsMatrix;
{ Objetivo
    Se D ? a matriz mtDiagonal que chama o m?todo, encontra o produto A'DA sem que a
    transposi??o seja explicitamente realizada
  Par?metros
    A: Matriz para o produto
    ErrCode: C?digo de erro. Se NRows = D.NRows retorna 0; caso contr?rio retorna NImprDim
    (dimens?es impr?prias para a opera??o)
  Retorno
    Se A ? mtDiagonal o retorno ser? uma matriz mtDiagonal; caso contr?rio retorna uma matriz
    sim?trica.
  Valores perdidos
    N?o considera
}
var
  i,j,k  :Integer;
  aux    : Double;
  L,L1,L2: TwsVec;
begin
  if A.NRows = NRows then
    begin
    L:=Row[1];
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        Result := TwsSymmetric.Create(A.NCols);
        for i := 1 to A.NCols do
          for j := 1 to i do
            begin
            aux := 0;
            for k := 1 to A.NRows do
              aux := aux + A[k, i]*L[k]*A[k, j];
            Result[i,j]:=aux
            end
        end; // mtGeneral, ...

      mtTriangular:
        begin
        Result := TwsSymmetric.Create(A.NCols);
        for i := 1 to A.NCols do
          for j := 1 to i do
            begin
            aux := 0;
            for k := i to A.NRows do
              aux := aux + A[k,i]*L[k]*A[k,j];
            Result[i,j]:=aux
            end
        end; // mtTriangular

      mtDiagonal:
        begin
        Result := TwsDiagonal.Create(A.NCols);
        L1:=A.Row[1];
        L2:=Result.Row[1];
        for i:=1 to A.NRows do
          L2[i]:=Sqr(L1[i])*L[i];
        end; // mtDiagonal

      end; // case
    end // if
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul5 }

function TwsDiagonal.TranspMul6(v: TwsVec; var ErrCode: Word): Double;       { v'Av }
{ Objetivo
    Retorna o produto do vetor transposto pela matriz e novamente o vetor
  Par?metros
    v: Vetor para o produto
    ErrCode: C?digo de erro. Se v.Len=NRows=NCols retorna 0; caso contr?rio retorna
    NImprDim (dimens?es impropr?prias para a opera??o
  Retorno
    Se A ? a matriz que chama retorna o escalar correspondente ao produto v'Av; se o produto
    n?o for poss?vel retorna wscMissValue
  Valores perdidos
    N?o trata
}
var
  j,n: Integer;
  L  : TwsVec;
begin
  n := v.Len;
  L:=Row[1];
  if ((n=NRows) and (n=NCols)) then
    begin
    Result := 0;
    for j := 1 to n do
      Result := Result+L[j]*Sqr(v[j])
    end
  else
    begin
    ErrCode := NImprDim;
    Result := wscMissValue
    end
end; { TranspMul6}

function TwsDiagonal.TranspMul10(A:TwsMatrix; var ErrCode:Word):TwsMatrix;
{ Objetivo
    Se D ? a matriz mtDiagonal que chama o m?todo, encontra o produto ADA' sem que a
    transposi??o seja explicitamente realizada
  Par?metros
    A: Matriz para o produto
    ErrCode: C?digo de erro. Se A.NCols=NRows retorna 0; caso contr?rio retorna NImprDim
    (dimens?es impr?prias para a opera??o)
  Retorno
    Se A ? mtDiagonal o retorno ser? uma matriz mtDiagonal; caso contr?rio retorna uma matriz
    sim?trica.
  Valores perdidos
    N?o considera
}
var
  i,j,k  :Integer;
  aux    : Double;
  L,L1,L2: TwsVec;
begin
  if A.NCols = NRows then
    begin
    L:=Row[1];
    case A.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz:
        begin
        Result := TwsSymmetric.Create(A.NRows);
        for i := 1 to A.NRows do // para cada linha de A
          for j := 1 to i do
            begin
            aux := 0;
            for k := 1 to A.NCols do
              aux := aux + A[i,k]*L[k]*A[j,k];
            Result[i,j]:=aux
            end
        end; // mtGeneral, ...

      mtTriangular:
        begin
        Result := TwsSymmetric.Create(A.NCols);
        for i := 1 to A.NRows do
          for j := 1 to i do
            begin
            aux := 0;
            for k := 1 to i do
              aux := aux + A[i,k]*L[k]*A[j,k];
            Result[i,j]:=aux
            end
        end; // mtTriangular

      mtDiagonal:
        begin
        Result := TwsDiagonal.Create(A.NCols);
        L1:=A.Row[1];
        L2:=Result.Row[1];
        for i:=1 to A.NRows do
          L2[i]:=Sqr(L1[i])*L[i];
        end; // mtDiagonal
      end; // case
    end // if
  else
    begin
    ErrCode := NImprDim;
    Result := nil
    end
end; { TranspMul10 }

function TwsDiagonal.TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double;
{ Objetivo
    Retorna o produto da coluna k da matriz A pela matriz na forma B[k]'*A*B[k], onde B[k]
    ? a coluna.
  Par?metros
    B: Matriz para o produto
    k: ?ndice da coluna para o produto
    ErrCode: C?digo de erro. Se NRows=NCols=A.NRows retorna 0; caso contr?rio retorna
      NImprDim (dimens?es impropr?prias para a opera??o
  Retorno
    Se o produto n?o for poss?vel, retorna wscMissValue
  Valores perdidos
    N?o trata
}
var
  j: Integer;
  L: TwsVec;
begin
  L:=Row[1];
  if ((B.NRows=NRows) and (B.NRows=NCols)) then
    begin
    Result := 0;
    case B.MatType of
      mtGeneral, mtSymmetric, mtToeplitz, mtVandermonde:
        for j := 1 to B.NRows do
          Result := Result+L[j]*Sqr(B[j,k]);
      mtTriangular:
        for j := k to B.NRows do
          Result := Result+L[j]*Sqr(B[j,k]);
      mtDiagonal:
        Result := L[k]*Sqr(B[k,k]);
      end; // case
    end
  else
    begin
    ErrCode := NImprDim;
    Result := wscMissValue
    end
end; { TranspMul6}

function TwsDiagonal.ByElement(B:TwsMatrix; Op:TwsEnumTypeOp; NewMat:Boolean; var ErrCode:Word): TwsMatrix;
{ Objetivo
    Opera??o elemento a elemento de uma matriz mtDiagonal por uma matriz qualquer
  Par?metros
    B: Matriz para opera??o
    Op: Opera??o desejada
    NewMat: True se resultado retorna na mesma matriz; False caso contr?rio. NewMat
      somente ser? considerado se o retorno (veja abaixo) for mtDiagonal.
    ErrCode: C?digo de erro. Retorna NImprDim (dimen?es impr?prias para a opera??o) se as
      matrizes n?o forem de mesma dimens?o; caso contr?rio retorna 0 (zero)
  Retorno: O retorno depende do tipo da matriz B e do tipo de operador especificado. As
    situa??es s?o:
      opSum,opSub,opDiv:
        Caso B seja
          geral, sim?trica, mtDiagonal, mtTriangular: o resultado tem o mesmo tipo de B
          mtVandermonde, mtToeplitz: o resultado ? sempre uma matriz geral
      opProd, opPower: retorno ? uma matriz mtDiagonal
}
var
  i,j: Integer;
  v  : TwsVec;
begin
  ErrCode:=0;
  if (NRows=B.NRows) and (NCols=B.NCols) then
    case Op of
      opSum:
        case B.MatType of
          mtGeneral,mtToeplitz,mtVandermonde:
            begin
            B.Copy(mtGeneral,Result);
            ElemOp2(Result,Row[1],Op,True,Result);
            end;
          mtSymmetric:
            begin
            B.Copy(mtSymmetric,Result);
            ElemOp2(Result,Row[1],Op,True,Result);
            end;
          mtTriangular:
            begin
            B.Copy(mtTriangular,Result);
            ElemOp2(Result,Row[1],Op,True,Result);
            end;
          mtDiagonal:
            begin
            if NewMat then
              Copy(mtDiagonal,Result)
            else
              Result:=Self;
            Result.Row[1].ByElement(B.Row[1],Op,False,ErrCode)
            end;
        end; // case B.MatType
      opSub:
        case B.MatType of
          mtGeneral,mtSymmetric,mtTriangular,mtToeplitz,mtVandermonde:
            begin                     // Sempre tera que criar nova matriz geral
            Result:=TwsGeneral.Create(NRows,NCols);
            ElemOp1(Self,B,Op,TwsGeneral(Result))  // Operacao com todos os elementos
            end;
          mtDiagonal:
            begin
            if NewMat then
              Copy(mtDiagonal,Result)
            else
              Result:=Self;
            Result.Row[1].ByElement(B.Row[1],Op,False,ErrCode)
            end;
        end; // case B.MatType
      opDiv,opProd,opPower:
        begin
        if NewMat then
          Copy(mtDiagonal,Result)
        else
          Result:=Self;
        v:=TwsDFVec.Create(Math.Min(B.NRows,B.NCols));
        for i:=1 to v.Len do
          v[i]:=B[i,i];
        Result.Row[1].ByElement(v,Op,False,ErrCode);
        v.Free
        end;
      else // de case Op
        begin                     // Sempre tera que criar nova matriz geral
        Result:=TwsGeneral.Create(NRows,NCols);
        ElemOp1(Self,B,Op,TwsGeneral(Result))  // Operacao com todos os elementos
        end;
    end // case Op
  else
    begin
    ErrCode:=NImprDim;
    Result:=nil
    end // if ErrCode
end; // ByElement

function TwsDiagonal.Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix;
{ Objetivo
    Metodo para o produto de matrizes. Utilizado quando a matriz que chama o metodo ?
    mtDiagonal
  Par?metros
    B: Matriz de qualquer tipo para o (pos)produto com a matriz que chama o m?todo
    ErrCode: C?digo de erro. O produto somente sera poss?vel (ErrCode retorna 0) se o
      n?mero de colunas de quem chama ? igual ao n?mero de linhas de B; caso contr?rio
      ErrCode retorna a constante NImprDim (Dimens?es impr?prias para a opera??o) e a
      fun??o retorna nil
  Retorno
    Se B for
      mtGeneral,Symetric,mtVandermonde,mtToeplitz: retorna matriz geral
      mtDiagonal: retorna matriz mtDiagonal
      mtTriangular: Retorna matriz mtTriangular
}
begin
  ErrCode := 0;
  if (NCols = B.NRows) then
    begin
    case B.MatType of
      mtGeneral,mtSymmetric,mtVandermonde,mtToeplitz: Prod2(B,Self,True,TwsGeneral(Result));
      mtDiagonal: Prod4(Self,TwsDiagonal(B),TwsDiagonal(Result));
      mtTriangular: Prod5(Self,TwsTriangular(B),True,TwsTriangular(Result));
    end; // case
    end
  else
    ErrCode := NImprDim;
end;

function TwsDiagonal.VecMult(v:TwsVec; PreMult:Boolean; Var ErrCode:Word): TwsDFVec;
{Objetivo
   Fazer o produto de uma matriz mtDiagonal por um vetor
 Par?matros
   v: Vetor para o produto
   PreMult: True se o vetor premultiplicar a matriz; False caso contrario
   ErrCode: C?digo de erro. Retorna 0 se o produto for poss?vel e NImprDim (dimens?es
     impr?prias para a opera??o) se a) n?mero de componentes do vetor diferente do n?mero
     de linhas da matriz (PreMult=True); b) n?mero de colunas da matriz diferente do n?mero
     de componentes do vetor (PreMult=False)
   Retorno
     Retorna sempre um vetor
}
var
  i: Integer;
begin
  ErrCode:=0;
  if (v.Len=NRows) then
    begin
    Result:=TwsDFVec.Create(NRows);
    for i:=1 to NRows do
      Result[i]:=v[i]*Self[i,i];
    end
  else
    begin
    ErrCode:=NImprDim;
    Result:=nil;
    end
end;

function TwsDiagonal.ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst, NewMat: Boolean): TwsMatrix;
{Objetivo
   Efetuar opera??es entre uma matriz mtDiagonal e um escalar
 Par?metros
   x: Escalar para a opera??o
   Op: Tipo de opera??o desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas opera??es (soma ou produto) esse par?metro n?o ? utilizado.
     No caso de potencia??o, SFirst=True indica que o escalar estar? na base e o elemento
     na pot?ncia; caso contr?rio, o elemento estar? na base e o escalar na pot?ncia. Para
     o operador ** (se SFirst=False), a opera??o corresponder? ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar (observe que, neste caso, a matriz deve
     ser quadrada para que o produto seja sempre definido); caso contr?rio a opera??o n?o
     ? definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz.
 Retorno
   O retorno ira depender do tipo de opera??o desejada. Para alguns tipos, o retorno ser?
     mtDiagonal enquanto que para outros ser? geral. A rela??o ser?:
     opDiv: Se SFirst=False retorna matriz mtDiagonal; geral caso contr?rio
     opProd: mtDiagonal
     opPower: Se SFirst=False retorna matriz mtDiagonal; geral caso contr?rio
     opSum,opSub,opGE,opGT,opLE,opLT,opEQ,OpNE,OpOR,OpAnd,OpMax,OpMin: Geral
   Nas situa??es onde n?o ? poss?vel o retorno de uma mtDiagonal, sempre retornar? uma matrtiz
     geral, independemente de NewMat
 Valores perdidos
   Se houver um valor perdido na opera??o envolvendo um elemento, o resultado ? valor perdido
}
var
  i: Integer;
begin
  if NewMat then
    case Op of
      opSum,opSub,opGE,opGT,opLE,opLT,opEQ,OpNE,OpOR,OpAnd,OpMax,OpMin:
        Self.Copy(mtGeneral,Result);
      opDiv:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Self.Copy(mtDiagonal,Result);
      opProd:
        Self.Copy(mtDiagonal,Result);
      opPower:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Self.Copy(mtDiagonal,Result);
    end // case
  else
    case Op of
      opSum,opSub,opGE,opGT,opLE,opLT,opEQ,OpNE,OpOR,OpAnd,OpMax,OpMin:
        Self.Copy(mtGeneral,Result);
      opDiv:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Result:=Self;
      opProd:
        Result:=Self;
      opPower:
        if SFirst then
          Self.Copy(mtGeneral,Result)
        else
          Result:=Self;
    end; // case
  for i:=0 to Result.FList.Count-1 do
    Result.Row[i+1].ByScalar(x,Op,False,SFirst);
end;

function TwsDiagonal.Transpose: TwsMatrix;
{ Objetivo
    Obtem a transposta da matriz que chama o m?todo
  Retorno
    Uma vez que a transposta de uma matriz mtDiagonal ? ela pr?pria, retorna uma c?pia da
    matriz que chamou
}
begin
  Copy(mtDiagonal,Result);
end;

procedure TwsDiagonal.Inv(var ErrCode: Word; eps: Double=1.0e-8);
{ Objetivo
    Obter a inversa da matriz (inverso dos elementos da mtDiagonal)
  Par?metros
    ErrCode: Retorna 0 se invers?o ? feita sem problemas; maInvError caso contr?rio
    eps: Precis?o para indica??o de matriz singular. A matriz ser? considerada singular se
      algum valor da mtDiagonal for menor ou igual a eps.
}
var
  i : Integer;
  L0: TwsVec;
begin
  ErrCode := 0;
  L0 := Row[1];
  for i := 1 to FNCols do
    if Abs(L0[i]) > eps then
      L0[i] := 1.0/L0[i]
    else
      begin
      ErrCode:=maInvError;
      Exit
      end
end;

procedure TwsDiagonal.Solve(B: TwsMatrix; var ErrCode: Word);
{ Objetivo
    Resolve sistema de equa??es onde a matriz dos coeficientes ? mtDiagonal.
  Par?metros
  B: Matriz onde, na entrada, cada coluna corresponde a um vetor do segundo
   membro do sistema. Na sa?da armazema em cada coluna a solu??o do sistema respectivo.
  ErrCode: C?digo de erro
}
var
  L0,L1: TwsVec;
  i,k  : Integer;
begin
  Inv(ErrCode);
  if ErrCode=0 then
    begin
    L0 := Row[1];
    for i := 1 to FNCols do
      begin
      L1 := B.Row[i];
      for k := 1 to B.FNCols do L1[k] := L1[k]*L0[i]
      end
    end
end;

(*
Procedure TwsDiagonal.SaveToStream(Writer:TWriter);
{ Objetivo
    Escreve a matriz num arquivo atrav?s do objeto de escrita. ? um m?todo virtual
    chamado por SaveToFile.
  Par?metros
    Writer: Objeto respons?vel pela escrita no arquivo
}

Begin
  With Writer Do
    Begin
      WriteSignature;
      WriteInteger(Integer(MatType));
      WriteString(Name);
      WriteInteger(NRows);
      WriteInteger(PrintOptions.MaxIDSize);
      CNameSavetoStream(Writer);
      WriteListBegin;
      Row[1].SaveToStream(Writer);
      WriteListEnd;
    End;
End; { TwsDiagonal.SaveToStream }
*)

procedure TwsDiagonal.InternalSave3(Writer: TWriter);
begin
  Writer.WriteListBegin;
  Row[1].SaveToStream(Writer);
  Writer.WriteListEnd;
end;

procedure TwsDiagonal.InternalLoad3(Reader: TReader);
begin
  Reader.ReadListBegin;
  PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;
end;

procedure TwsDiagonal.DeleteCols(Col: TwsLIVec);
{ Objetivo
    Elimina colunas especificadas
  Par?metros
    Col: ?ndices das colunas a serem eliminadas
  M?todos chamados
    DeleteCols herdado
}
var
  i: Integer;
begin
  MDeleteCols(Col);
  if CName <> nil then
    for i := Col.Len downto 1 do      // elimina os atributos das colunas
      CName.Delete(Col[i]-1);
  FNCols := FNCols - Col.Len;
end;  // TwsDiagonal.DeleteCols

{ =========================== TwsVandermonde ===============================}

constructor TwsVandermonde.Create(NC: Integer);
{ Objetivo
    Cria uma matriz de mtVandermonde. Uma matriz de mtVandermonde pode ser definida apenas com um
    vetor de valores com a mesma dimens?o da matriz. Os demais elementos podem ser obtidos
    atrav?s de pot?ncias desses valores. Desse modo apenas um vetor ? utilizado para armazenar
    essa matriz.
  Par?metros
    NC: Dimens?o da matriz. Se NC>0 insere uma linha (vetor) em branco.
  M?todos chamados
    Cerate herdado
  Campos alterados
    FMatType
    RName
}
begin
  inherited Create(NC, NC);
  FMatType := mtVandermonde;
  RName := nil;
  If NC > 0 Then
     FList.Add(TwsDFVec.Create(FNCols))
end; { TwsVandermonde.Create }

function TwsVandermonde.GetBlockName: String;
begin
  Result := 'wsGeneral';
end;

function TwsVandermonde.GetBlockNameComment: String;
begin
  Result := 'Estilo de formata??o para Matrizes do tipo TwsVandermonde';
end;

procedure TwsVandermonde.RowsToXML(Writer: TXML_Writer);
var i, j: Integer;
    s: String;
    b: Boolean;
    Cor: TColor;
begin
  b := (Assigned(FSelectElementColor));
  if FNRows = 0 then Exit;
  for i:=1 to FnRows do
    begin
    if RName <> nil then
       s := '<row Label="' + XML_EncodeChars(RName[i-1], True) + '">'
    else
       s := '<row Label="'+IntToStr(i)+'">';

    for j := 1 to FnCols do
      if b then
         begin
         Cor := clblack;
         FSelectElementColor(Self, i, j, Cor);
         if Cor <> clBlack then
            s := s + '<e cor="#' + RGBColorAsString(Cor) + '">'
         else
            s := s + '<e>';
         s := s + SysUtilsEx.AllTrim(getStr(i, j)) + '</e>';
         end
      else
         s := s + '<e>' + SysUtilsEx.AllTrim(getStr(i, j)) + '</e>';
    Writer.Write(s + '</row>');
    end
end;

destructor TwsVandermonde.Destroy;
{ Objetivo
    Libera a mem?ria ocupada por uma matriz de mtVandermonde
  M?todos chamados
    Destroy herdado
  Campos modificados
    RName
}
begin
  if RName <> nil then RName.Free;
  inherited Destroy
end; { TwsVandermonde.Destroy }

function TwsVandermonde.Get(i,j: Integer): Double;
{ Objetivo
    Obt?m um elemento da matriz de mtVandermonde na posi??o especificada
}
begin
  if j=1 then
    Result := 1
  else
    if j=2 then
      Result := Row[1].Data[i]
    else
      Result := Power(Row[1].Data[i], j-1)
end; { TwsVandermonde.Get }

function TwsVandermonde.GetRowName(i: Integer): String;
{ Objetivo
    Obt?m o nome da linha espwecificada
  Par?metros
    i: ?ndice da linha
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
begin
  if RName<>nil then
    Result:=RName[i-1]
  else
    Result:='Lin'+IntToStr(i)
end;

procedure TwsVandermonde.SetRName(st: TwsCVec; DelChar:TCharSet);
{ Objetivo:
    Cria nomes de linhas a partir de um string.
  Par?metros
    st     : string com os nomes de linhas
    DelChar: Conjunto de caracteres separadores de nomes de linhas.
}
var
  i,j,L: Integer;
  st1: String;
begin
  if RName <> nil then RName.Free;
  RName := TStringList.Create;
  // Insere os nomes das linhas
  j := 1; i := 0;
  st1 := st.StrGet(j, DelChar);
  while (st1 <> '') and (i < FNRows) do
    begin
    Inc(i);
    RName.Add(st1);
    L := Length(st1)+3;
    if L > PrintOptions.MaxIDSize then PrintOptions.MaxIDSize := L;
    st1 := st.StrGet(j, DelChar);
    end;
  // Completa se for necess?rio
  for i:=RName.Count+1 to FNRows do
    RName.Add('L'+IntToStr(i))
end; { TwsVandermonde.SetRName }

procedure TwsVandermonde.Put(i, j: Integer; x: Double);
  { Objetivo
    Atribui um valor ? posi??o especificada
  Par?metros
  i, j: ?ndice da linha e coluna, respectivamente, para atribui??o. Como
      para este tipo de matriz somente um vetor armazena todos os valores, o
      valor ? atribu?do ? posi??o j do vetor.
}
begin
  FModified := True;
  Row[1].Data[j] := x;
end; { Put }

function TwsVandermonde.ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer;
{ Transforma a linha L da matriz em S iniciando em Start, com Amt colunas,
  cada coluna com dimensao Field e Precision casas decimais }

  { Objetivo
    Transforma os elementos especificados de uma linha num string .
  Par?metros
    L: Linha da matriz
    Start: ?ndice do primeiro valor que ser? colocado no string
    Amt: Quantos valores ser?o colocados no string
}
var
  j: Integer;
  S: string;
  x: Double;
begin
  if RName <> nil then
    S := LeftPad(RName[L-1], PrintOptions.MaxIDSize)
  else
    S := LeftPad('L'+IntToStr(L), PrintOptions.MaxIDSize);

  Amt := Math.Min(Amt, FNCols - Start + 1);

  for j := 0 to Amt-1 do
    AppendStr(S,GetStr(L,Start+j));

  if PrintOptions.Center Then
     Buffer.Add(StrCenter(S, 110))
  else
     Buffer.Add(S);

  Result := Length(S)
end; { TwsVandermonde.ToChar }

procedure TwsVandermonde.ColPrint(const Cols: String; Expr: String; Buffer: TStrings);
{ Imprime somente as linhas indicadas em R e as colunas indicadas em C.
  ColWidth indica a dimensao das colunas e Decim o numero de casas decimais.}

  { Objetivo
    Constroi um string com valores das colunas especifcadas e o imprime se a
    linha respectiva satisfaz ? condi??o estabelecida
  Par?metros
    Cols: String que estabelece as colunas que ser?o impressas
    Condicao: Express?o que ir? indicar se a linha ser? impressa ou n?o.(Ver Express?es)
}
const
  MinColWidth = 5;
var
  i, j, k, PrevI,
  NumCol: Integer;
  P: String;
  Aux: Double;
  C: TwsLIVec;
begin
  if Cols <> '' then                                    { Se indica as colunas }
    C := IndexColsFromString(Cols)
  else                                        { Senao imprime todas as colunas }
    C := Index(1, FNCols);
  i := 0;         { Imprime submatrizes com tantas colunas quanto possivel }
  repeat
    PrevI := i;
    NumCol := 0;                         { Numero de colunas a imprimir }
    j := PrevI+1;                           { Proxima coluna a imprimir }
    repeat                           { Quantas colunas serao impressas? }
      Inc(j);
      Inc(i);
      Inc(NumCol);
      until (j > C.Len) or (NumCol*PrintOptions.ColWidth >= (PrintOptions.LineLen-PrintOptions.MaxIDSize));

    if (PrintOptions.LineLen - PrintOptions.MaxIDSize) <= NumCol*PrintOptions.ColWidth then
       begin
       Dec(NumCol);
       Dec(i)
       end;

    P := LeftPad(FName, PrintOptions.MaxIDSize);
    for j := 1 to NumCol do
      AppendStr(P, Format('%*s', [PrintOptions.ColWidth, FCName[C[PrevI+j]-1]]));

    if PrintOptions.Center Then
       Buffer.Add(StrCenter(P, 110))
    else
       Buffer.Add(P);

    { Constroi linha de tamanho MaxLen ou esgotando colunas para saida }
    j := 0;
    repeat
      Inc(j);
      if RName <> nil then
        P := LeftPad(RName[j-1], PrintOptions.MaxIDSize)
      else
        P := LeftPad(IntToStr(j), PrintOptions.MaxIDSize);

      for k := 1 to NumCol do
        if not IsMissValue(j,C[PrevI+k],Aux) then
          AppendStr(P, Format('%*.*g', [PrintOptions.ColWidth, PrintOptions.ColPrecision, Fuzz(Aux)]))
        else
          AppendStr(P, Format('%*s', [PrintOptions.ColWidth, '-']));

      if PrintOptions.Center Then
         Buffer.Add(StrCenter(P, 110))
      else
         Buffer.Add(P);

      until j = FNRows;                            { Esgota todas as linhas }
    until i = C.Len;                             { Esgota todas as colunas }

  C.Free;
//  Buffer.Add('');
end; { TwsVandermonde.ColPrint }

procedure TwsVandermonde.List(Buffer: TStrings);
{ Objetivo
    Lista os descritores de uma matriz de mtVandermonde
  M?todos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtVandermonde');
  inherited List(Buffer)
end;

procedure TwsVandermonde.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    C?pia da matriz
  Par?metros
    MT: Tipo da matriz de retorno
    Matrix: Matriz que retorna com a c?pia. Se tipo depender? de MT (mtGeneral, mtSymmetric,
      mtDiagonal, mtTriangular, mtVandermonde ou mtToeplitz)
}
var
  i,j  : Integer;
  L0,L1: TwsVec;
begin
  case MT of
    mtGeneral:
      begin
      Matrix := TwsGeneral.Create(NRows, NCols);
      for i := 1 to NRows do
        for j := 1 to NCols do
          Matrix[i,j]:= Self[i,j]
      end;

    mtSymmetric:
      begin
      Matrix := TwsSymmetric.Create(NCols);
      for i:=1 to NRows do
        for j:=1 to i do
          Matrix[i,j]:=Self[i,j]
      end;

    mtTriangular:
      begin
      Matrix := TwsTriangular.Create(NCols);
      for i:=1 to NRows do
        for j:=1 to i do
          Matrix[i,j] := Self[i,j]
      end;

    mtDiagonal:
      begin
      Matrix := TwsDiagonal.Create(NCols);
      L0 := Matrix.Row[1];
      for j:=1 to NCols do
        L0[j]:= Self[j,j]
      end;

    mtVandermonde:
      begin
      Matrix := TwsVandermonde.Create(Ncols);
      Matrix.RowName[1] := RowName[1];
      L0 := Matrix.Row[1];
      L1:=Row[1];
      for i := 1 to NCols do
        L0[i]:=L1[i]
      end;

    mtToeplitz:
      begin
      Matrix := nil;
      Exit
      end;
  end; { case }
  inherited Copy(MT, Matrix)
end; { Copy }

function TwsVandermonde.ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix;
{Objetivo
   Efetuar opera??es entre uma matriz mtTriangular e um escalar
 Par?metros
   x: Escalar para a opera??o
   Op: Tipo de opera??o desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas opera??es (soma ou produto) esse par?metro n?o ? utilizado.
     No caso de potencia??o, SFirst=True indica que o escalar estar? na base e o elemento
     na pot?ncia; caso contr?rio, o elemento estar? na base e o escalar na pot?ncia. Para
     o operador ** (se SFirst=False), a opera??o corresponder? ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contr?rio a opera??o n?o ? definida.
   NewMat: N?o ser? considerado neste caso. Sempre retornar? uma nova matriz.
 Retorno
   O retorno ser? sempre uma matriz geral
 Valores perdidos
   Se houver um valor perdido na opera??o envolvendo um elemento, o resultado ? valor
   perdido
}
var
  i: Integer;
begin
  Self.Copy(mtGeneral,Result);
  for i:=1 to Result.NRows do
    Result.Row[i].ByScalar(x,Op,False,SFirst);
end;

procedure TwsVandermonde.Solve(B: TwsMatrix; var ErrCode: Word);
{ Resolve um sistema onde a matriz dos coeficientes e de mtVandermonde. Em
  B deverao estar os valores do lado direito. Uma segunda segunda linha
  sera inserida para retornar a solucao. A matriz A nao devera estar na
  forma de mtVandermonde e sim como uma matriz com os valores necessarios
  na primeira linha.

  ===> Observar algoritmo na p?gina 55 de Press. Sa?da est? em local
  diferente da entrada. Modificar este algoritmo para que as colunas de B
  armazenem os vetores do segundo membro. Copiar a coluna, resolver e recopiar
  no mesmo local .
   }
var
  k1,k,j,i : Integer;
  xx,t,s,b1: Double;
  L1,L0,w,c: TwsVec;
begin
  c := TwsDFVec.Create(FNCols);
  w := TwsDFVec.Create(FNCols);
  L0 := Row[1];
  L1 := B.Row[1];
  if FNCols = 1 then
    w[1] := L1[1]
  else
    begin
    for i := 1 to FNCols do c[i] := 0;
    c[FNCols] := -L0[1];
    for i := 2 to FNCols do
      begin
      xx := -L0[i];
      for j := FNCols+1-i to FNCols-1 do
        c[j] :=  c[j]+xx*c[j+1];
      c[FNCols] := c[FNCols] + xx
      end; { for }
    for i := 1 to FNCols do
      begin
      xx := L0[i];
      t := 1.0;
      b1 := 1.0;
      s := L1[FNCols];
      k := FNCols;
      for j := 2 to FNCols do
        begin
        k1 := k-1;
        b1 := c[k] + xx*b1;
        s := s + L1[k1]*b1;
        t := xx*t + b1;
        k := k1
        end; { for }
      w[i] := s/t
      end; { for }
    end; { if }
  B.MAdd(w);
  c.Free;
end; { TwsVandermonde.Solve }

(*
Procedure TwsVandermonde.LoadFromStream(Reader:TReader);
{ Objetivo
    Respons?vel de fato pela leitura do objeto no disco. Chamado por MFromStream.
  Par?metros
    Reader: Objeto respons?vel pela leitura do objeto armazenado
}
(*
Begin
  Inherited LoadFromStream(Reader);
  RName := TStringList.Create;
  With Reader Do
    Begin
    ReadListBegin;
    While Not EndOfList Do RName.Add(ReadString);
    ReadListEnd;
    End;

  If RName.Count = 0 Then
      Begin
      RName.Free;
      RName :=Nil;
      End;
End; { TwsVandermonde.LoadFromStream }
*)

(*
Procedure TwsVandermonde.SaveToStream(Writer:TWriter);
{ Objetivo
    Grava o objeto num arquivo. Chamado pelo m?todo SaveToFile.
  Par?metros
    Writer: Objeto respons?vel pela escrita do conte?do da matriz no arquivo
}


Var
  i  :Integer;
Begin
With Writer Do
  Begin
  WriteSignature;
  WriteInteger(Integer(FMatType));
  WriteString(Name);
  WriteInteger(NRows);
  WriteInteger(PrintOptions.MaxIDSize);
  CNameSavetoStream(Writer);

  WriteListBegin;
  Row[1].SaveToStream(Writer);
  WriteListEnd;
  WriteListBegin;
  If RName <> Nil Then
      For i := 0 To RName.Count-1 Do WriteString(Rname.Strings[i]);
  WriteListEnd;
  End; { With Writer }
End; { TwsVandermonde.SaveToStream }
*)

procedure TwsVandermonde.InternalSave3(Writer: TWriter);
var i: Integer;
begin
  with Writer do
    begin
    WriteListBegin;
    Row[1].SaveToStream(Writer);
    WriteListEnd;
    WriteListBegin;
    If RName <> Nil Then
        For i := 0 To RName.Count-1 Do WriteString(Rname.Strings[i]);
    WriteListEnd;
    End; { With Writer }
end;

procedure TwsVandermonde.InternalLoad3(Reader: TReader);
begin
  Reader.ReadListBegin;
  PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;

  RName := TStringList.Create;
  With Reader Do
    Begin
    ReadListBegin;
    While Not EndOfList Do
          RName.Add(ReadString);
    ReadListEnd;
    End;

  If RName.Count = 0 Then
      Begin
      RName.Free;
      RName := Nil;
      End;
end;

{ ============================== TwsToeplitz ============================ }

constructor TwsToeplitz.Create(NC: Integer);
{ Objetivo
    Cria uma matriz de mtToeplitz. Uma matriz de mtToeplitz n x n ? completamente especificada por
    2n-1 valores (armazenados num vetor) e n?o precisa ser necessariamente sim?trica.
  Par?metros
    nc: n?mero de colunas (e linhas) da matriz. Se nc>, insere um vetor de ordem 2*nc-1 em
    branco.
  M?todos chamados
    Create herdado
  Campos alterados
    RName
}
var
  n: Integer;
begin
  inherited Create(NC, NC);
  FMatType := mtToeplitz;
  RName := nil;
  If NC > 0 Then
     Begin
     n := 2*nc-1;
     FList.Add(TwsDFVec.Create(n))
     End;
end; { TwsToeplitz.Create }

function TwsToeplitz.GetBlockName: String;
begin
  Result := 'wsGeneral';
end;

function TwsToeplitz.GetBlockNameComment: String;
begin
  Result := 'Estilo de formata??o para Matrizes do tipo TwsToeplitz';
end;

procedure TwsToeplitz.RowsToXML(Writer: TXML_Writer);
var i, j: Integer;
    s: String;
    b: Boolean;
    Cor: TColor;
begin
  b := (Assigned(FSelectElementColor));
  if FNRows = 0 then Exit;
  for i:=1 to FnRows do
    begin
    if RName <> nil then
       s := '<row Label="' + XML_EncodeChars(RName[i-1], True) + '">'
    else
       s := '<row Label="'+IntToStr(i)+'">';

    for j := 1 to FnCols do
      if b then
         begin
         Cor := clblack;
         FSelectElementColor(Self, i, j, Cor);
         if Cor <> clBlack then
            s := s + '<e cor="#' + RGBColorAsString(Cor) + '">'
         else
            s := s + '<e>';

         s := s + SysUtilsEx.AllTrim(getStr(i,j)) + '</e>';
         end
      else
         s := s + '<e>' + SysUtilsEx.AllTrim(getStr(i,j)) + '</e>';
    Writer.Write(s + '</row>');
    end
end;
destructor TwsToeplitz.Destroy;
{ Objetivo
    Libera espa?o ocupado pelo objeto[
  M?todos chamados
    Desrtoy herdado
  Campos liberados
    RName
}
begin
  if RName <> nil then RName.Free;
  inherited Destroy;
end;

function TwsToeplitz.Get(i, j: Integer): Double;
{ Objetivo
    Retorna o valor na posi??o especificada
  Par?metros
    i, j: Respectivamente, ?ndices da linha e coluna do valor. O valor
    retornado est? na posi??o NCol - j + i
}
begin
  Result := Row[1].Data[FNCols - j + i]
end; { TwsToeplitz.Get }

function TwsToeplitz.GetRowName(i: Integer): String;
{ Objetivo
    Retorna nome da linha especificada
  Par?metros
    i: ?ndice da linha
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
begin
  if RName<>nil then
    Result:=RName[i-1]
  else
    Result:='Lin'+IntToStr(i)
end;

procedure TwsToeplitz.SetRName(st: TwsCVec; DelChar:TCharSet);
  { Objetivo
    Cria nomes de linhas a partir de um string
  Par?metros
    st     : string com os nomes de linhas
    DelChar: Conjunto de caracteres separadores de nomes de linhas.
  M?todos chamados
    Nenhum
  Campos alterados
    RName
    PrintOptions.MaxIDSize
}
var
  i,j,L: Integer;
  st1  : String;
begin
  if RName <> nil then RName.Free;
  RName := TStringList.Create;
  // Insere os nomes das linhas
  j := 1; i := 0;
  st1 := st.StrGet(j, DelChar);
  while (st1 <> '') and (i < FNRows) do
    begin
    Inc(i);
    L := Length(st1)+3;
    if L > PrintOptions.MaxIDSize then PrintOptions.MaxIDSize := L;
    RName.Add(st1);
    st1 := st.StrGet(j, DelChar);
    end;
  // Completa se for necess?rio
  for i:=RName.Count+1 to FNRows do
    RName.Add('L'+IntToStr(i))
end; { TwsToeplitz.SetRName }

procedure TwsToeplitz.Put(i,j: Integer; x: Double);
  { Objetivo
    Atribui um valor na posi??o especificada
  Par?metros
    i, j: ?ndices da linha e coluna
    x: Valor a ser atribu?do. O valor ser? atribu?do ? posi??o NCol - j + i do
    vetor que armazena todos os valores da matriz.
}
begin
  FModified := True;
  Row[1].Data[FNCols - j + i] := x
end; { TwsToeplitz.Put }

procedure TwsToeplitz.List(Buffer: TStrings);
{ Objetivo
    Lista descritores pertinentes ? matriz
  M?todos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtToeplitz');
  inherited List(Buffer)
end;

function TwsToeplitz.ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer;
  { Objetivo
    Transforma os elementos especificados de uma linha num string
  Par?metros
    L: Linha da matriz
    Start: ?ndice do primeiro valor que ser? colocado no string
    Amt: Quantos valores ser?o colocados no string
}
var
  j: Integer;
  S: string;
  x: Double;
begin
  if RName <> nil then
    S := LeftPad(RName[L-1], PrintOptions.MaxIDSize)
  else
    S := LeftPad('L'+IntToStr(L), PrintOptions.MaxIDSize);

  Amt := Math.Min(Amt, FNCols - Start + 1);

  for j := 0 to Amt-1 do
    AppendStr(S,GetStr(L,Start+j));

  if PrintOptions.Center Then
     Buffer.Add(StrCenter(S, 110))
  else
     Buffer.Add(S);

  Result := Length(S)
end; { TwsToeplitz.ToChar }

procedure TwsToeplitz.ColPrint(const Cols: String; Expr: String; Buffer: TStrings);
{ Objetivo
    Constroi um string com valores das colunas especificadas e o imprime se a linha respectiva
    satisfaz ? condi??o estabelecida
  Par?metros
    Cols: String que estabelece as colunas que ser?o impressas
    Expr: Express?o que ir? indicar se a linha ser? impressa ou n?o. (Ver Express?es)
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
var
  i,j,k,PrevI,
  NumCol       : Integer;
  P            : String;
  Aux          : Double;
  C            : TwsLIVec;
begin
  if Cols <> '' then                                    { Se indica as colunas }
    C := IndexColsFromString(Cols)
  else                                        { Senao imprime todas as colunas }
    C := Index(1, FNCols);

  i := 0;             { Imprime submatrizes com tantas colunas quanto possivel }
  repeat
    PrevI := i;
    NumCol := 0;                                { Numero de colunas a imprimir }
    j := PrevI+1;                                  { Proxima coluna a imprimir }
    repeat                                  { Quantas colunas serao impressas? }
      Inc(j);
      Inc(i);
      Inc(NumCol);
      until (j > C.Len) or (NumCol*PrintOptions.ColWidth >= (PrintOptions.LineLen-PrintOptions.MaxIDSize));

    if (PrintOptions.LineLen - PrintOptions.MaxIDSize) <= NumCol*PrintOptions.ColWidth then
      begin
      Dec(NumCol);
      Dec(i)
      end;

    P := LeftPad(FName, PrintOptions.MaxIDSize);
    for j := 1 to NumCol do
      AppendStr(P, Format('%*s', [PrintOptions.ColWidth, FCName[C[PrevI+j]-1]]));

    if PrintOptions.Center Then
       Buffer.Add(StrCenter(P, 110))
    else
       Buffer.Add(P);

   { Constroi linha de tamanho MaxLen ou esgotando colunas para saida }
    j := 0;

    repeat
      Inc(j);
      if RName <> nil then
        P := LeftPad(RName[j-1], PrintOptions.MaxIDSize)
      else
        P := LeftPad(IntToStr(j), PrintOptions.MaxIDSize);

      for k := 1 to NumCol do
        if not IsMissValue(j,C[PrevI+k],Aux) then
          AppendStr(P, Format('%*.*g', [PrintOptions.ColWidth, PrintOptions.ColPrecision, Fuzz(Aux)]))
        else
          AppendStr(P, Format('%*s', [PrintOptions.ColWidth, '-']));

      if PrintOptions.Center Then
         Buffer.Add(StrCenter(P, 110))
      else
         Buffer.Add(P);

      until j = FNRows;                                 { Esgota todas as linhas }
    until i = C.Len;                                   { Esgota todas as colunas }

  C.Free;
  Buffer.Add('');
end; { TwsToeplitz.ColPrint }

procedure TwsToeplitz.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    Retorna uma c?pia de uma matriz de mtToeplitz
  Par?metros
    MT: Tipo da matriz de retorno. Se MT for
        mtGeneral: Matriz de mtToeplitz retornar? numa matriz geral
        mtSymmetric: Matriz de mtToeplitz retornar? numa matriz sim?trica
        mtTriangular: Matriz de mtToeplitz retornar? numa matriz mtTriangular
        mtDiagonal: Matriz de mtToeplitz retornar? somente os elementos da mtDiagonal
        mtToeplitz: Matriz de mtToeplitz retornar? numa matriz de mtToeplitz
        mtVandermonde: N?o ? poss?vel copiar uma matriz de mtToeplitz numa matriz de mtVandermonde
    Matrix: Retorna a c?pia
}
var
  i,j  : Integer;
  L0,L1: TwsVec;
begin
  case MT of
    mtGeneral:
      begin
      Matrix := TwsGeneral.Create(NRows, NCols);
      for i := 1 to NRows do
        begin
        if RName<>nil then
          Matrix.RowName[i] := RowName[i];
        for j := 1 to NCols do
          Matrix[i,j]:= Self[i,j]
        end
      end;
    mtSymmetric:
      begin
      Matrix := TwsSymmetric.Create(NCols);
      for i:=1 to NRows do
        begin
        if RName<>nil then
          Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j]:=Self[i,j]
        end
      end;
    mtTriangular:
      begin
      Matrix := TwsTriangular.Create(NCols);
      for i:=1 to NRows do
        begin
        if RName<>nil then
          Matrix.RowName[i] := RowName[i];
        for j:=1 to i do
          Matrix[i,j] := Self[i,j]
        end
      end;
    mtDiagonal:
      begin
      Matrix := TwsDiagonal.Create(NCols);
      L0 := Matrix.Row[1];
      for j:=1 to NCols do
        L0[j]:= Self[j,j]
      end;

    mtToeplitz:
      begin
      Matrix := TwsToeplitz.Create(NCols);
      L0 := Matrix.Row[1];
      L1 := Row[1];
      for j := 1 to 2*NCols-1 do
        L0[j]:=L1[j]
      end;

    mtVandermonde:
      begin
      Matrix := nil;
      Exit
      end;
  end; { case }
  inherited Copy(MT,Matrix)
end; { Copy }

function TwsToeplitz.ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix;
{Objetivo
   Efetuar opera??es entre uma matriz de mtToeplitz e um escalar
 Par?metros
   x: Escalar para a opera??o
   Op: Tipo de opera??o desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas opera??es (soma ou produto) esse par?metro n?o ? utilizado.
     No caso de potencia??o, SFirst=True indica que o escalar estar? na base e o elemento
     na pot?ncia; caso contr?rio, o elemento estar? na base e o escalar na pot?ncia. Para
     o operador ** (se SFirst=False), a opera??o corresponder? ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contr?rio a opera??o n?o ? definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz.
 Retorno
   O retorno sempre ser? uma mariz de mtToeplitz
 Valores perdidos
   Se houver um valor perdido na opera??o envolvendo um elemento, o resultado ? valor perdido
}
var
  i: Integer;
begin
  if NewMat then
    Self.Copy(mtToeplitz,Result)
  else
    Result:=Self;
  Result.Row[1].ByScalar(x,Op,False,SFirst);
end;

procedure TwsToeplitz.Solve(B: TwsMatrix; var ErrCode: Word);
{ Objetivo
    Resolve sistema de equa??es lineares onde a matriz dos coeficientes ? uma matriz de mtToeplitz.
  Par?metros
    B: Matriz cujas colunas definem um sisema de equa??es lineares do tipo Ax=B[i]
    ErrCode: Retorna 0 se solu??o foi feita sem problemas e maToepError caso contr?rio
}
{ =======> Conferir este algoritmo. P?g. 59 de Press }
label 98, 99;
var
  m2,m1,m,k,j          : Integer;
  sxn,shn,sgn,sgd,sd,
  qt2,qt1,qq,pt2,pt1,pp: Double;
  G,H,F,LA,LQ          : TwsVec;
begin
  G := TwsDFVec.Create(FNCols);
  H := TwsDFVec.Create(FNCols);
  F := TwsDFVec.Create(FNCols);
  LA := Row[1];
  LQ := B.Row[1];
  if (LA[FNCols] = 0.0) then GoTo 99;
  F[1] := LQ[1]/LA[FNCols];
  if (FNCols = 1) then GoTo 99;
  G[1] := LA[FNCols-1]/LA[FNCols];
  H[1] := LA[FNCols+1]/LA[FNCols];
  for m := 1 to FNCols do
    begin
    m1 := m+1;
    sxn := -LQ[m1];
    sd := -LA[FNCols];
    for j := 1 to m do
      begin
      sxn := sxn+LA[FNCols+m1-j]*F[j];
      sd := sd+LA[FNCols+m1-j]*G[m-j+1]
      end;
    if (sd = 0.0) then GoTo 98;
    F[m1] := sxn/sd;
    for j := 1 to m do
      F[j] := F[j]-F[m1]*G[m-j+1];
    if (m1 = FNCols) then GoTo 99;
    sgn := -LA[FNCols-m1];
    shn := -LA[FNCols+m1];
    sgd := -LA[FNCols];
    for j := 1 to m do
      begin
      sgn := sgn+LA[FNCols+j-m1]*G[j];
      shn := shn+LA[FNCols+m1-j]*H[j];
      sgd := sgd+LA[FNCols+j-m1]*H[m-j+1]
      end;
    if ((sd = 0.0) or (sgd = 0.0)) then GoTo 98;
    G[m1] := sgn/sgd;
    H[m1] := shn/sd;
    k := m;
    m2 := (m+1) div 2;
    pp := G[m1];
    qq := H[m1];
    for j := 1 to m2 do
      begin
      pt1 := G[j];
      pt2 := G[k];
      qt1 := H[j];
      qt2 := H[k];
      G[j] := pt1-pp*qt2;
      G[k] := pt2-pp*qt1;
      H[j] := qt1-qq*pt2;
      H[k] := qt2-qq*pt1;
      Dec(k)
      end
    end;
  ErrCode := maToepError;
  GoTo 99;
98: ErrCode := maToepError;
99:
  G.Free;
  H.Free;
  B.MAdd(F)
end; { ToepSolv }

(*
Procedure TwsToeplitz.LoadFromStream(Reader:TReader);
{ Objetivo
    Respons?vel de fato pela leitura do objeto no disco. Chamado por MFromStream.
  Par?metros
    Reader: Objeto respons?vel pela leitura do objeto armazenado
}

Begin
  Inherited LoadFromStream(Reader);
  RName := TStringList.Create;
  With Reader Do
    Begin
    ReadListBegin;
    While Not EndOfList Do RName.Add(ReadString);
    ReadListEnd;
    End;

  If RName.Count = 0 Then
     Begin
     Rname.Free;
     RName:=Nil;
     End;
End; { TwsToeplitz.LoadFromStream }
*)

(*
Procedure TwsToeplitz.SaveToStream(Writer:TWriter);
{ Objetivo
    Grava o objeto num arquivo. Chamado pelo m?todo SaveToFile.
  Par?metros
    Writer: Objeto respons?vel pela escrita do conte?do da matriz no arquivo
}

(*
Var
  i  :Integer;
Begin
With Writer Do
  Begin
  WriteSignature;
  WriteInteger(Integer(MatType));
  WriteString(Name);
  WriteInteger(NRows);
  WriteInteger(PrintOptions.MaxIDSize);
  CNameSavetoStream(Writer);

  WriteListBegin;
  Row[1].SaveToStream(Writer);
  WriteListEnd;
  WriteListBegin;

  If RName <> Nil Then
     For i := 0 To RName.Count-1 Do
        WriteString(Rname.Strings[i]);

  WriteListEnd;
  End; { With Writer }
End; { TwsToeplitz.SaveToStream }
*)

procedure TwsToeplitz.InternalSave3(Writer: TWriter);
var i: Integer;
begin
  with Writer do
    begin
    WriteListBegin;
    Row[1].SaveToStream(Writer);
    WriteListEnd;
    WriteListBegin;

    If RName <> Nil Then
       For i := 0 To RName.Count-1 Do
          WriteString(Rname.Strings[i]);

    WriteListEnd;
    End; { With Writer }
end;

procedure TwsToeplitz.InternalLoad3(Reader: TReader);
begin
  Reader.ReadListBegin;
  PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;

  RName := TStringList.Create;
  With Reader Do
    Begin
    ReadListBegin;
    While Not EndOfList Do
          RName.Add(ReadString);
    ReadListEnd;
    End;

  If RName.Count = 0 Then
     Begin
     Rname.Free;
     RName := Nil;
     End;

end;

{ TListCols }

function TListCols.AddObject(const S: string; AObject: TObject): Integer;
{ Objetivo
    Insere um objeto numa lista de strings. Redefine m?todo para inser??o de um objeto
    do tipo TwsDatasetCol na lista. Verifica se j? existe um objeto com o nome especificado e
    se existir, modifica o nome
  Par?metros
    S: nome do objeto para inser??o
    AObject: Objeto para inser??o
}
Var i  : Integer;
    so : String;

   Function Existe(Const Name: String): Boolean;
   Var i: Integer;
   Begin
     Result := False;
     For i := 0 to Count - 1 do
       If CompareText(TwsDataSetCol(Objects[i]).Name, Name) = 0 Then
          Begin
          Result := True;
          Break;
          End;
   End;

Begin
  i := 1;
  so := TwsDataSetCol(AObject).Name;
  While Existe(TwsDataSetCol(AObject).Name) Do
    Begin
    Inc(i);
    TwsDataSetCol(AObject).Name := so + IntToStr(i);
    End;

  inherited AddObject(S, AObject);
End;

{ TwsDSStructure }

Constructor TwsDSStructure.Create;
{ Objetivo
    Cria objeto gerenciador da estrutura de um conjunto de dados. A lista criada ? do
    do tipo TListCols
  M?todos chamados
    Nenhum
  Campos modificados
    Cols
}
Begin
  inherited Create;
  Cols := TListCols.Create;
End; { TwsDSStructure.Create }

Destructor TwsDSStructure.Destroy;
{ Objetivo
    Libera espa?o ocupado pelo objeto gerenciador da estrutura do conjunto de dados
  Campos liberados
    Cols
}
Begin
  While Cols.Count > 0 Do
    Begin
    TwsDataSetCol(Cols.Objects[0]).Free;
    Cols.Delete(0);
    End; { While }
  Cols.Free;

  inherited;
End; { TwsDSStructure.Destroy }

function TwsDSStructure.Copy: TwsDSStructure;
var i: Integer;
begin
  Result := TwsDSStructure.Create;
  //Result.DataSet := self.DataSet;
  TListCols(Result.Cols).Duplicates := dupError; {N?o permite colunas repetidas}
  For i := 1 to self.Cols.Count do
    Result.AddCopyCol(self.Col[i]);
end;

procedure TwsDSStructure.ToXML(Buffer: TStrings; Ident: Integer);
var i: Integer;
    sIdent: String;
begin
  sIdent := StringOfChar(' ', Ident);
  Buffer.Add(sIdent + '<Variables>');
  sIdent := sIdent + '  ';

  if DataSet.ColIdentName <> '' then
     Buffer.Add(sIdent + '<IdentVar>' +
                            XML_EncodeChars(DataSet.ColIdentName, False) +
                         '</IdentVar>')
  else
     Buffer.Add(sIdent + '<IdentVar/>');

  for i := 1 to Cols.Count do
    begin
    Buffer.Add(sIdent + '<Variable type="' + toString(ord(getCol(i).FColType)) + '">');
    inc(Ident, 4);
    getCol(i).ToXML(Buffer, Ident);
    dec(Ident, 4);
    Buffer.Add(sIdent + '</Variable>');
    end;
  Buffer.Add(StringOfChar(' ', Ident) + '</Variables>');
end;

Function TwsDSStructure.GetCol(index: Integer): TwsDataSetCol;
{ Objetivo
    Recupera endere?o de uma coluna
  Par?metros
    i: ?ndice da coluna. Baseado em 1.
}
Begin
  Try
    Result := TwsDataSetCol(Cols.Objects[index-1]);
  Except
    Raise Exception.CreateFmt(wsMsgInvalidIndexVar, [index]);
  End;
End;

procedure TwsDSStructure.SetCol(index: Integer; const Value: TwsDataSetCol);
{ Objetivo
    Substitui endere?o de uma coluna
  Par?metros
    i: ?ndice da coluna. Baseado em 1.
}
begin
  try
    Cols.Objects[index-1].Free;
    Cols.Objects[index-1] := Value;
  Except
    Raise Exception.CreateFmt(wsMsgInvalidIndexVar, [index]);
  End;
end;

Function TwsDSStructure.GetFactors: Integer;
{ Objetivo
    Retorna o n?mero de fatores presenets no conjunto de dados
}
Var i: Integer;
Begin
  Result := 0;
  For i := 0 to Cols.Count - 1 do
    If Col[i+1].ColType <> dtNumeric Then Inc(Result);
End;

Function TwsDSStructure.ColByName(Const N: String): TwsDataSetCol;
{ Objetivo
    Retorna endere?o da coluna com nome especificado
  Par?metros
    N: nome da coluna desejada
}
Begin
  Try
    Result := TwsDataSetCol(Cols.Objects[IndexOf(SysUtilsEx.AllTrim(N))-1]);
  Except
    Raise Exception.CreateFmt(MsgUnknownVariable, [N]);
  End;
End; { TwsDSStructure.GetColByName }

Function TwsDSStructure.IndexOf(Const N: String): Integer;
{ Objetivo
    Retorna ?ndice da coluna com nome especificado. Baseado em 1.
  Par?metros
    N: Nome da coluna desejada
}
var i: Integer;
Begin
  For i := 0 to Cols.Count - 1 do
    If CompareText(TwsDataSetCol(Cols.Objects[i]).Name, N) = 0 Then
       Begin
       Result := i + 1;
       Exit;
       End;
  Result := -1;
End; { TwsDSStructure.IndexOf }

Function  TwsDSStructure.IndexCols(Const Cols: String): TwsLIVec; {Rochedo} {29/06/1998}
{ Objetivo
    Retorna um vetor com os ?ndices das colunas cujos nomes est?o num string. Os nomes dever?o
    estar separados por v?rgulas.
  Par?metros
    Cols: strinng com os nomes
  M?todos chamados
    strTokenToStrings
    IndexOf
  Campos modificados
    Nenhum
}
Var i,k: Integer;
    C  : TStrings;
Begin
  C := nil;
  Split(Cols, C, [',']);
  Try
    Result := TwsLIVec.Create(C.Count);
    For i := 0 to C.Count - 1 do
      Begin
      K := IndexOf(C[i]);
      If K <> -1 Then
         Result[i+1] := K
      Else
         Begin
         Result.Free;
         Raise Exception.CreateFmt('Classe: %s'#13 +
                                   'M?todo: %s'#13 +
                                   MsgUnknownVariable,
                                   [ClassName, 'IndexCols', C[i]]);
         Result:=nil
         End;
      End;

  Finally
    C.Free;
  End;
End;

Procedure TwsDSStructure.AddCol(Col: TwsDataSetCol); {Rochedo, 12/03/98}
{  Objetivo
     Insere objeto descritor de coluna
   Par?metro
     Col: Objeto a ser adicionado
   M?todos chamados
     AddColDesc
     AddObject
}
Begin
  Col.Struct := Self;
  Col.DataSet := DataSet;
  If DataSet <> Nil Then
     DataSet.AddColDesc(Col)
  Else {Se o dataSet n?o existe}
     Cols.AddObject('', Col);
End;

Procedure TwsDSStructure.AddColEx(Col: TwsDataSetCol); {Rochedo, 04/04/98}
{ Objetivo
    Adiciona objeto descritor sem alterar o conte?do das linhas
  Par?metros
    Col: objeto a adicionar
  M?todos chamados
    AddObject
  Campos modificados
    NCols
}
Begin
  Col.Struct := Self;
  Col.DataSet := DataSet;
  If DataSet <> Nil Then
     Begin
     DataSet.CName.AddObject('', Col);  {Rochedo, 16/05/1998}
     DataSet.nCols := DataSet.nCols + 1;
     End
  Else {Se o dataSet n?o existe}
     Cols.AddObject('', Col);
End; // AddColEx

Procedure TwsDSStructure.AddCopyCol(Col: TwsDataSetCol); {Rochedo, 12/03/98}
{ Objetivo
    Adiciona uma c?pia de um objeto descritor de colunas
  Par?metros
    Col: objeto cuja c?pia ser? adicionada
  M?todos chamados
    AddColDesc
    CopyDescCol
    AddObject
  Campos alterados
    Nenhum
}
var newCol: TwsDataSetCol;
Begin
  NewCol := CopyDescCol(Col);
  NewCol.Struct := Self;
  NewCol.DataSet := DataSet;
  
  If DataSet <> Nil Then
     DataSet.AddColDesc(NewCol)
  Else {Se o dataSet n?o existe}
     Cols.AddObject('', NewCol);
End;

{$ifdef MSXML}
procedure TwsDSStructure.fromXML(no: IXMLDomNode);

  function loadVar(no: IXMLDomNode): TwsDataSetCol;
  begin
    result := CreateDatasetVar( toInt(no.attributes.item[0].text) );
    // result.fromXML(no);

    // por enquanto ...  <<<<< 
    result.FName := no.childNodes.item[0].text;
    result.FSize := toInt(no.childNodes.item[2].text);
  end;

var i: integer;
    s: string;
begin
  s := no.childNodes.item[0].text;
  if s <> '' then DataSet.ColIdentName := s;
  for i := 1 to no.childNodes.length-1 do
    addColEx( loadVar(no.childNodes.item[i]) );
end;
{$endif MSXML}
{ TwsDataSet }

procedure TwsDataSet.SetStruct(const Value: TwsDSStructure);
var i: Integer;
begin
  FStruct.Free;
  FStruct := Value;
  FCName := FStruct.Cols;
  FnCols := FStruct.Cols.Count;
  Value.DataSet := Self;
  for i := 1 to FnCols do
    FStruct.Col[i].DataSet := Self;
end;

function TwsDataSet.PrintDescVars: Boolean;
var i: Integer;
begin
  for i := 1 to FnCols do
    begin
    Result := (Struct.Col[i].Lab <> '');
    if Result then Exit;
    end;
end;

Procedure TwsDataSet.AddColDesc(Obj: TwsDataSetCol);
{=========================> retirar}
Var
  i: Integer;
Begin
  Obj.Struct := Struct;
  Obj.DataSet := Self;
  FModified := True;
  CName.AddObject('', Obj);
  NCols := NCols + 1;
  { Se nao existem dados no conjunto,
    nao adiciona elemento aos vetores. - Roger - 16/10/97 }
  If nRows > 0 Then
     For i := 1 To nRows Do
        TwsDFVec(Row[i]).Add(0);
End; { TwsDataSet.AddColDesc }

procedure TwsDataSet.DeleteCol(i: Integer);
Begin
  if i <= FnCols then
     begin
     Inherited DeleteCol(i);
     Struct.Col[i].Free();
     Struct.Cols.Delete(i-1);
     end;
End;

Procedure TwsDSStructure.AddNumeric(Const N,Lab :String; ps: byte=15; pf: byte=8);
{ Objetivo
    Adiciona um descritor de vari?vel num?rica
  Par?metros
    N: Nome da coluna
    Lab: r?tulo da coluna
    ps: largura de impress?o
    pf: precis?o ou d?gitos significativos para impress?o
  M?todos chamados
    AddCol
    AddObject
  Campos alterados
    Struct
}
var C: TwsDataSetCol;
Begin
  If DataSet <> Nil Then
     DataSet.AddCol(N, Lab, dtNumeric, ps, pf)
  Else
     Begin
     C := TwsNumeric.Create(N, Lab, ps, pf);
     C.Struct := Self;
     Cols.AddObject('', C);
     End;
End; { TwsDSStructure.AddNumeric }

Procedure TwsDSStructure.AddQualitative(Const N,Lab :String; ps: byte=15);
{ Objetivo
    Adiciona um objeto descritor de fator qualitativo
  Par?metros
    N: nome da coluna
    Lab: r?tulo da coluna
    ps: largura de impress?o
  M?todos chamados
    AddCol
    AddObject
  Campos alterados
     Struct
}
var C: TwsDataSetCol;
Begin
  If DataSet <> Nil Then
     DataSet.AddCol(N,Lab,dtQualit,ps)
  Else
     Begin
     C := TwsQualitative.Create(N,Lab,ps);
     C.Struct := Self;
     Cols.AddObject('', C);
     End;
End; { TwsDSStructure.AddQualitative }

Procedure TwsDSStructure.AddQuantitative(Const N,Lab :String; ps: byte=15);
{ Objetivo
    Adiciona um objeto descritor de fator quantitativo
  Par?metros
    N: nome da coluna
    Lab: r?tulo da coluna
    ps: largura de impress?o
  M?todos chamados
    AddCol
    AddObject
  Campos alterados
     Struct
}
var C: TwsDataSetCol;
Begin
  If DataSet <> Nil Then
     DataSet.AddCol(N,Lab,dtQuant,ps)
  Else
     Begin
     C := TwsQuantitative.Create(N,Lab,ps);
     C.Struct := Self;
     Cols.AddObject('', C);
     End;
End; { TwsDSStructure.AddQuantitative }

Procedure TwsDSStructure.AddOrdered(Const N,Lab :String; ps: byte=15);
{ Objetivo
    Adiciona um objeto descritor de fator qualitativo ordenado
  Par?metros
    N: nome da coluna
    Lab: r?tulo da coluna
    ps: largura de impress?o
  M?todos chamados
    AddCol
    AddObject
  Campos alterados
     Nenhum
}
var C: TwsDataSetCol;
Begin
  If DataSet <> Nil Then
     DataSet.AddCol(N,Lab,dtQualitOrd,ps)
  Else
     Begin
     C := TwsOrdered.Create(N,Lab,ps);
     C.Struct := Self;
     Cols.AddObject('', C);
     End;
End; { TwsDSStructure.AddOrdered }

procedure TwsDSStructure.Delete(const IndexCol: Integer);
begin
  If DataSet <> Nil Then
     DataSet.DeleteCol(IndexCol)
  Else
     Begin
     Col[IndexCol].Free;
     Cols.Delete(IndexCol-1);
     End;
end;

Procedure TwsDSStructure.Delete(Const N: String);
{ Objetivo
    Libera espa?o ocupado por um objeto descritor
  Par?metros
    N: nome da coluna
  M?todos chamados
    IndexOf
    Free
    Delete
  Campos alterados
     Nenhum
}
var i: Integer;
Begin
  i := IndexOf(N);
  If i = -1 Then
     Raise Exception.CreateFmt('Classe: %s'#13 +
                               'M?todo: %s'#13 +
                               MsgInvalidNameVar,
                               [ClassName, 'Delete', N]);

  Delete(i);
End; { TwsDSStructure.Delete }

{ TwsDataSet }

Constructor TwsDataSet.Create(aName: String='NoName');
{ Objetivo
    Cria um objeto para armazenar um conjunto de dados
  Par?metros
    aName: nome do conjunto de dados
    nr: n?mero de linhas
    nc: n?mero de colunas
  M?todos chamados
    Create herdado
  Campos alterados
    FMatType
    Name
    ColIdentName
    CName
    Struct
  Observa??o
    Com muita frequ?ncia, este objeto ? criado com nr=0 e nc=0 e as colunas (por meio de
    algum m?todo de adi??o de colunas) e linhas (por meio do m?todo MAdd) s?o adicionados
    posteriormente.
}
Begin
  inherited Create(0,0);
  FMatType := mtDataSet;
  Name := aName;
  SetColIdentName('');
  { Cria o objeto struct, liga o dataset com o struct, e associa cols com CName }
  FStruct := TwsDSStructure.Create;
  FStruct.DataSet := Self;
  FCName.Free;
  FCName := FStruct.Cols;
end; { TwsDataSet.Create }

Constructor TwsDataSet.CreateStructured(aName: String; nr: Integer; Struct: TwsDSStructure);
{ Objetivo
    Cria um objeto conjunto de dados utilizando uma estrutura j? criada.
  Par?metros
    aName: nome do conjunto de dados
    nr: n?mero de linhas
    Struc: estrutura do conjunto
  M?todos chamados
    Create herdado
  Campos alterados
     FMatType
     name
     ColIdentName
     Struct
}
var i: Integer;
Begin
  { CName deve ser passado e depois liberado.
    TwsGeneral.Create cria os vetores com NCols elementos }
  inherited Create(nr, Struct.Cols.Count);
  FMatType := mtDataSet;

  name := aname;
  SetColIdentName('');

  { Associa Structure passado com struct,
    associa cname com cols e liga o dataset com Structure }
  SetStruct(Struct);
End; { TwsDataSet.CreateStructured }

Constructor TwsDataSet.CreateFix(aname: String; nr:Integer; ColT: Array Of TwsEnumDataType);
{ Objetivo
    Cria um objeto conjunto de dados utilizando um array de descritores de colunas j? dispon?vel.
  Par?metros
    aName: nome do conjunto de dados
    nr: n?mero de linhas
    ColT: array de colunas para o conjunto
  M?todos chamados
    Create herdado
  Campos alterados
     FMatType
     name
     ColIdentName
     Struct
}
Var i: Integer;
Begin
  { Se for passado zero no segundo par?metro, CName n?o ? criado. Ver TwsMatrix.Create }
  inherited Create(nr,0);
  FMatType := mtDataSet;
  Name := AName;
  SetColIdentName('');

  { Cria objeto struct, liga o dataset com o struct,
    associa Cols com CName e cria as colunas }
  FStruct := TwsDSStructure.Create;
  FStruct.DataSet := Self;
  FCName.Free;
  FCName := FStruct.Cols;

  With FStruct Do
    For i := Low(ColT) To High(ColT) Do
      Case ColT[i] Of
        dtNumeric   : AddNumeric      ('Col' + IntToStr(i), '');
        dtQualit    : AddQualitative  ('Col' + IntToStr(i), '');
        dtQuant     : AddQuantitative ('Col' + IntToStr(i), '');
        dtQualitOrd : AddOrdered      ('Col' + IntToStr(i), '');
        End; { Case }
End; { TwsDataSet.CreateFix }

Destructor TwsDataSet.Destroy;
{ Objetivo
    Libera espa?o ocupado por um conjunto de dados
  Campos liberados
    Struct
  M?todos chamados
    Detroy herdado
}
Begin
  Struct.Free;
  CName := nil; {Evitar? crash nos demais "CName(s).Free"}
  Inherited Destroy;
End; { TwsDataSet.Destroy }

procedure TwsDataSet.ForceRowsCount(Count: Integer);
var Delta, i: Integer;
begin
  Delta := Count - FnRows; // (+) aumenta   (-) diminui

  if Delta <> 0 then
     if Delta > 0 then
        for i := 1 to Delta do MAdd(TwsDFVec.Create(FNCols))
     else
        for i := Delta to -1 do MDelete(FnRows);
end;

Function  TwsDataSet.Copy: TwsDataSet;
{ Objetivo
    Obt?m uma c?pia do conjunto de dados
  M?todos chamados
     Nenhum
  Campos modificados
     Nenhum
}
Var i    : Integer;
    Linha: TwsVec;
Begin
  Result := TwsDataSet.Create(Name);
  Result.MLab := MLab;
  Result.ColIdentName := ColIdentName;
  Result.PrintOptions.MaxIDSize := PrintOptions.MaxIDSize;
  For i := 1 to nCols do Result.Struct.AddCopyCol(Struct.Col[i]);
  For i := 1 to nRows do
    Begin
    Linha := Row[i];
    Result.PutLine(Linha.Copy(1, Linha.Len));
    End;
  Result.nRows := nRows;
End;

    // Cria uma c?pia de colunas especificas do conjunto de dados
Function  TwsDataSet.CopyByCols(Col: TwsLIVec): TwsDataSet;
{ Objetivo
    Copia colunas especificas do conjunto de dados
  Par?metros
    Col: ?ndices das colunas que ser?o copiadas
}
var
  i,j: Integer;
  v,r: TwsVec;
begin
  Result:=TwsDataSet.Create(Name+'_1');
  Result.MLab:=MLab;
  Result.ColIdentName:=ColIdentName;
  // Copia as definicoes de colunas
  for j := 1 to Col.Len do
    Result.Struct.AddColEx(CopyDescCol(Struct.Col[Col[j]]));
  for i:=1 to nRows do
    begin
    r:=Row[i];
    v:=TwsDFVec.Create(Col.Len);
    v.Name:=r.Name;
    for j:=1 to Col.Len do
      v[j]:=r[Col[j]];
    Result.MAdd(v);
    end;
end; // CopyByCols
procedure TwsDataSet.SetColIdentName(name: string); {Alex, 23/02/2000}
{ Objetivo
    Atribui coluna identificadora
  Par?metros
    name: nome para coluna
  M?todos chamados
    Nenhum
  Campos alterados
    FColIdentName
    PrintOptions.MaxIDSize
}
begin
  FColIdentName := name;
  if Length(FColIdentName) > PrintOptions.MaxIDSize then
     PrintOptions.MaxIDSize := Length(FColIdentName);
end;

procedure TwsDataSet.SetName(Const Name: String);
{ Objetivo
    Atribui nome v?lido para o conjuto de dados
  Par?metros
    Name: nome para i conjunto de dados
  Campos alterados
    FName
}
Begin
  FModified := True;
  If not SysUtilsEx.IsValidIdent(Name) Then
     FName := GetValidId(Name)
  Else
     FName := Name;
End;

function TwsDataSet.ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer;
{ Objetivo
   Transfere linha do conjunto de dados para um string
  Par?metros
    L: ?ndice da linha
    Start: Coluna inicial
    Amt: n?mero de colunas
  M?todos chamados
    Write
  Campos alterados
    Nenhum
}
var
  j: Integer;
  point :TwsFactor;
  S, s1: string;
begin
  S := LeftPad(InttoStr(L), 6);

  s1 := RowName[L];
  if (FColIdentName <> '') then    // S := S + LeftPad(s1, PrintOptions.MaxIDSize+1); {Rochedo} {18/04/1999}
     S := S + LeftPad(s1, PrintOptions.MaxIDSize+1); {Rochedo} {18/04/1999}

  Amt := Math.Min(Amt, NCols - Start + 1);
  for j := 0 to Amt-1 do AppendStr(S, AsString[L,Start+j] + ' ');

  if PrintOptions.Center Then
     Buffer.Add(StrCenter(S, 110))
  else
     Buffer.Add(S);

  Result:=Length(S)
end; { TwsDataSet.ToChar }

Procedure TwsDataSet.Header(Start, Amt: Integer; Buffer: TStrings);
{ Objetivo
    Cria cabe?alho para listagem de conjuntos de dados no formato texto
  Par?metros
    Start: coluna inicial
    Amt: n?mero de colunas
  M?todos chamados
    Write
    WriteCenter
  Campos alterados
    Nenhum
}
Var j, k: Integer;
    L   : Word;
    st,P: string;
    SL  : TStrings;
    Col : TwsDataSetCol;
Begin
  P := '';
  Try
    if PrintOptions.Center then
       Buffer.Add(StrCenter(Name+': '+MLab, 110))
    else
       Buffer.Add(Name+': '+MLab);

    if PrintOptions.PrintDesc then
       begin
       P := Pad('Vari?vel',13);  {13}
       P := P+Pad('R?tulo',50); {63}
       P := P+Pad('Tipo',10);   {73}
       P := P+LeftPad('N?veis',9); {82}
       P := P+LeftPad('T.N?vel',10);{92}

       if PrintOptions.Center Then
         begin
         Buffer.Add(StrCenter(P, 110));
         Buffer.Add(StrCenter(StringOfChar('-', Length(P)), 110));
         end
       Else
         begin
         Buffer.Add(P);
         Buffer.Add(StringOfChar('-', Length(P)))
         end;

       for j := 1 to Amt do
         begin
         Col := Struct.Col[Start+j];
         P := Pad(Col.Name,13);                          // variavel
         P := P+Pad(Col.Lab,50);                         // rotulo
         P := P+Pad(Col.getColTypeAsString,10);                  // tipo
         Case Col.ColType Of
           dtNumeric:
             begin
             P := P + LeftPad('-',9);
             P := P + LeftPad('-',10);
             end;
           dtQuant, dtQualit, dtQualitOrd :
             Begin
             P := P+LeftPad(IntToStr(TwsFactor(Col).LevelNames.Count),9); // niveis
             P := P+LeftPad(TwsFactor(Col).getLevelTypeAsString,10);                   // tipo de nivel
             End;
           end;  { case }

         if PrintOptions.Center Then
            Buffer.Add(StrCenter(P, 110))
         else
            Buffer.Add(P);
         end;
       end; {if PrintDescCols}

     if PrintOptions.Center Then
        Buffer.Add(StrCenter(StringOfChar('-', Length(P)), 110))
     else
        Buffer.Add(StringOfChar('-', Length(P)));

    P := LeftPad('Obs.', 6);
    if FColIdentName <> '' then
       P := P + LeftPad(FColIdentName, PrintOptions.MaxIDSize+1);

    for j := 1 to Amt do
      begin
      st := Struct.Col[Start+j].Name;

      if Struct.Col[Start+j].Size > Length(st) then
         L := Struct.Col[Start + j].Size
      else
         L := Length(st);

      AppendStr(P, LeftPad(st, L) + ' ');
      end;

    if PrintOptions.Center Then
      begin
      Buffer.Add(StrCenter(P, 110));
      Buffer.Add(StrCenter(StringOfChar('-', Length(P)), 110));
      end
    else
      begin
      Buffer.Add(P);
      Buffer.Add(StringOfChar('-', Length(P)))
      end;
  Except
    On E: Exception do
       Buffer.Add(Name + ': ' + E.Message);
  End;
End;

function TwsDataSet.MeanFactor(IFac:TwsLIVec; VIdx:Integer; var GMean,Min,Max:Double): TwsGeneral;
{ Objetivo
    Retorna um conjunto de estat?sticas para cada n?vel de cada fator indicado.
  Par?metros
    IFac : ?ndices dos fatores para cujos n?veis as estat?sticas ser?o calculadas
    VIdx : ?ndice da variavel para a qual ser?o calculadas as estat?sticas
    GMean: M?dia geral global
    Min  : Menor valor global
    Max  : Maior valor global
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
  Retorno
    Retorna uma matriz com as estatisticas. Ter? tantas linhas quantos s?o os fatores e
    tantas colunas quanto for o n?mero de niveis vezes o n?mero de estatisticas, As
    estat?sticas s?o (por n?vel, nessa ordem): m?dia, valor m?nimo, valor m?ximo, soma de
    quadrados de desvios e n?mero de observa??es v?lidas.

  Exemplo: A com 2 niveis e B com 3 n?veis. O reultado ser?:

     Media  Minim  Maxim   SQD   Repet
     -----  -----  -----  -----  -----
  A  A1 A2  A1 A2  A1 A2  A1 A2  A1 A2                  Linha 1
     Media     Minim     Maxim     SQD       Repet
     --------  --------  --------  --------  --------
  B  B1 B2 B3  B1 B2 B3  B1 B2 B3  B1 B2 B3  B1 B2 B3   Linha 2

  Valores perdidos
    Os valores perdidos s?o exclu?dos dos c?lculos

  Observa??o
    Note que o n?mero de colunas depender? do n?mero de n?veis do fator respectivo. Assim,
    o n?mero de colunas poder? variar em fun??o do n?mero de n?veis de cada fator.
 }
const
  NStat = 5; {Media, Minimo, Maximo, Variancia e No. Observ validas}
var
  ValidObs,i,j,
  row,cl,k,kr,niv: Integer;
  x,v            : Double;
  Lin            : TwsVec;
  Lev            : TwsLIVec;
begin
  { Em cada linha uma variavel e em cada coluna uma estatistica de um nivel do fator }
  Result := TwsGeneral.Create(0, 0);
  {Constroi a matriz Stat com zeros nas posicoes. Repare que essas linhas
   possuem linhas de tamanhos diferentes pois o numero de niveis dos fatores e
   diferente}
  x:=Self[1,Vidx];
  Lev:=TwsLIVec.Create(IFac.Len); { Numero de niveis de cada fator }
  for i := 1 to IFac.Len do
    begin
    niv := TwsFactor(Self.Struct.Col[IFac[i]]).LevelNames.Count;
    Lin := TwsDFVec.Create(niv*NStat);
    for j:=1 to niv do
      begin
      Lin[j]:=0;       { Media dos niv niveis }
      Lin[niv+j] := -MinFloatValue;  { Minimos }
      Lin[2*niv+j] := MinFloatValue;  { Maximos }
      Lin[3*niv+j] := 0;   { Variancias }
      Lin[4*niv+j] := 0;   { Repeticoes}
      end;
    Lev[i] := niv;
    Result.MAdd(Lin);
    end;
  GMean := 0;
  Min := x;
  Max := x;
  ValidObs := 0;
  for i := 1 to Self.NRows do                   { Para cada observacao }
    for j := 1 to IFac.Len do                   { Para cada variavel }
      begin
      row := IFac[j];                           { Linha da matriz de estatisticas }
      cl := Self.AsInteger[i,row]+1;            { Nivel do fator }
      x := Self[i,VIdx];                        { Valor da variavel dependente }

      if not wsGLib.IsMissValue(x) then
        begin
        Inc(ValidObs);
        v := (x - GMean)/ValidObs;
        GMean := GMean + v;                     { Atualiza media geral }
        if Min > x then Min := x;               { Atualiza minimo }
        if Max < x then Max := x;               { Atualiza maximo }
        niv:=Lev[j];
        kr:=4*niv+cl;                 { Indice coluna para numero de repeticoes }
        Result[j,kr]:=Result[j,kr]+1;           { Numero de observacoes }
        v := (x-Result[j,cl])/Result[j,kr];
        Result[j,cl] := Result[j,cl] + v;       { M?dia }
        k:=3*niv + cl;
        Result[j,k]:=Result[j,k]+v*v*(Result[j,kr]-1)/Result[j,kr]; { SQ ajustada <== cuidar}
        k := niv+cl;
        if Result[j,k] > x then Result[j,k] := x;  { Minimo }
        k := 2*niv+cl;
        if Result[j,k] < x then Result[j,k] := x;  { Maximo }
        end { if x <> wscMissValue }
      end; { for j}
  Lev.Free
end; { MeanFactor }

function TwsDataSet.FacMeans(Fac,IRow,LRow: Integer; Col: TwsLIVec): TwsGeneral;
{ Objetivo:
    Calcula a media de linhas especificadas de todos os n?veis de uma matriz para as colunas
    especificadas.
  Par?metros:
    Fac : ?ndice do fator para cujos n?veis ser?o calculadas as m?dias.
    IRow: Linha inicial
    LRow: Linha final
    Col : Indices das colunas para as quais serao calculadas as medias
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
  Retorno:
    Retorna uma matriz com Col.Len*2 colunas. Cada par de colunas armazenar? a
    m?dia de cada n?vel do fator Fac e o respectivo n?mero de repeti??es.

  Exemplo:
    Para um fator A com 4 n?veis e duas vari?veis y1 e y2, o resultado final ficaria:

       y1   rep_1    y2  rep_2
  A1  2.5       5   4.9      6
  A2  ...           ...
  A3
  A4
}
var
  n,i,k,
  kv,kk,kv1: Integer;
  aux      : Double;
begin
  { Numero de linhas da matriz }
  n := TwsFactor(Self.Struct.Col[Fac]).LevelNames.Count;
  { Matriz das medias e numero de repeticoes }
  Result := TwsGeneral(Jota(n, 2*Col.Len, mtGeneral, 0));
  Result.Name := 'Medias';
  { Nomes dos n?veis nas linhas da matriz de sa?da }
  for i := 1 to n do
    Result.RowName[i] := TwsFactor(Self.Struct.Col[Fac]).LevelNames[i-1];

  { Nomes das vari?veis dependentes e repeti??es }
  for i := 0 to Col.Len-1 do
    begin
    k:=2*i+1;
    Result.ColName[k] := Self.Struct.Col[Col[i+1]].Name;
    Result.ColName[k+1]:=System.Copy(Result.ColName[k],1,4)+'_n';
    end;

  for i:=IRow to LRow do                    { M?dias desde as linhas IRow at? LRow }
    begin
    kk := Self.AsInteger[i,Fac]+1;          { kk ? o ?ndice do n?vel do fator Fac  }
    for k := 1 to Col.Len do                { Esgota para todas as vari?veis dependentes }
      begin
      kv := 2*(k-1)+1;
      kv1 := kv+1;
      aux :=  Self[i,Col[k]];
      if not wsGLib.IsMissValue(Aux) then
        begin
        Result[kk,kv1] := Result[kk,kv1]+1;                  { N?mero de repeti??es }
        aux := aux - Result[kk,kv];
        Result[kk,kv]:=Result[kk,kv]+aux/Result[kk,kv1]      { Calcula a media }
        end
      end { for k }
    end { for i }
end;

function TwsDataSet.FacVarMean(Fac: TwsLIVec; IRow,LRow: Integer; Col: TwsLIVec): TwsGeneral;
{ Objetivo:
    Retorna a media,a vari?ncia e o n?mero de repeti??es dos n?veis dos fatores para as
    vari?veis especificadas.
  Par?metros:
    Fac : ?ndices do fatores para cujos n?veis ser?o calculadas as estat?sticas.
    IRow: Linha inicial do conjunto de dados
    LRow: Linha final do conjunto de dados
    Col : Indices das colunas para as quais serao calculadas as estat?sticas
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
  Retorno:
    Retorna uma matriz com Col.Len*3 colunas. Cada tr?s colunas armazenar?o a m?dia, a
    vari?ncia e o n?mero de observa??es v?lidas de cada n?vel.

  Exemplo:
  Para um fator A com 4 n?veis, B com 2 n?veis e duas vari?veis preditoras y1 e y2, o
  resultado final ficaria:

         y1   Var_1   rep_1    y2  Var_2  rep_2
    A1  2.5     3,2       5   4,9    0,4      6
    A2  ...           ...
    A3
    A4
    B1
    B2
  }
var
  n,i,k,kv,kv1,
  kv2,kk,kf,kn: Integer;
  aux         : Double;
  s           : string;
  C           : TwsDataSetCol;
begin
  { Numero de linhas da matriz }
  n:=0;
  for i:=1 to Fac.Len do
    Inc(n, TwsFactor(Self.Struct.Col[Fac[i]]).LevelNames.Count);
  { Matriz das medias e numero de repeticoes }
  Result := TwsGeneral(Jota(n, 3*Col.Len, mtGeneral, 0));
  Result.Name := 'Niveis';
  { Nomes dos n?veis nas linhas da matriz de sa?da }
  for kf:=1 to Fac.Len do
    begin
    C:=Self.Struct.Col[Fac[kf]];
    s:=System.Copy(C.Name,1,3)+'-';
    for i := 1 to TwsFactor(C).LevelNames.Count do
      Result.RowName[i] := s+TwsFactor(C).LevelNames[i-1];
    end;

  { Nomes das vari?veis dependentes (para media) e repeti??es }
  for i := 0 to Col.Len-1 do
    begin
    kf:=3*i+1;
    s:=ColName[Col[i+1]];
    Result.ColName[kf] := 'M_'+s;
    Result.ColName[kf+1]:='V_'+s;
    Result.ColName[kf+2]:='n_'+s;

    end;
    for kf:=1 to Fac.Len do
      begin
      kn:=(kf-1)*TwsFactor(Self.Struct.Col[Fac[kf]]).LevelNames.Count;
      for i:=IRow to LRow do                    // M?dias desde as linhas IRow at? LRow
        begin
        kk := kn+Self.AsInteger[i,Fac[kf]]+1;   // kk ? a linha do resultado
        for k := 1 to Col.Len do                // Para todas as vari?veis dependentes
          begin
          kv := 3*k-2;                          // Coluna para a media
          kv1 := kv+1;                          // Coluna para a variancia
          kv2 := kv+2;                          // Coluna para o numero de repeticoes
          aux :=  Self[i,Col[k]];
          if not wsGLib.IsMissValue(Aux) then
            begin
            Result[kk,kv2] := Result[kk,kv2]+1;                        // N?mero de repeti??es
            aux := aux - Result[kk,kv];                                // SQ ajustada
            Result[kk,kv1] := Result[kk,kv1] + aux*aux*(Result[kk,kv2]-1)/Result[kk,kv2];
            Result[kk,kv]:=Result[kk,kv]+aux/Result[kk,kv2]            // Media
            end
          end { for k }
        end { for i }
      end;
  for i:=1 to Result.NRows do                                      // Variancias
    for k:=1 to Col.Len do
      begin
      kv:=3*k-1;
      kv1:=kv+1;
      Result[i,kv]:=ScalarDiv(Result[i,kv],(Result[i,kv1]-1))
      end
end;

function TwsDataSet.wFacVarMean(Fac: TwsLIVec; IRow,LRow,WInd: Integer; Col: TwsLIVec): TwsGeneral;
{ Objetivo:
    Retorna a media, a vari?ncia (ponderadas), o n?mero de repeti??es dos n?veis dos fatores
    e a soma dos pesos para as vari?veis especificadas.
  Par?metros:
    Fac : ?ndices do fatores para cujos n?veis ser?o calculadas as estat?sticas.
    IRow: Linha inicial do conjunto de dados
    ICol: Linha final do conjunto de dados
    WInd: ?ndice da vari?vel peso
    Col : Indices das colunas para as quais serao calculadas as estatisticas
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno:
    Retorna uma matriz com Col.Len*3 colunas. Cada tr?s colunas armazenar?o a m?dia, a
      vari?ncia e o n?mero de observacoes de cada n?vel.
  Exemplo:
  Para um fator A com 4 n?veis, B com 2 n?veis e duas vari?veis preditoras y1 e y2, o
    resultado final ficaria:

         y1   Var_1   rep_1   Peso_1   y2   Var_2  rep_2  Peso_2
    A1  2.5     3,2       5     10,2  4,9     0,4      6     7,2
    A2  ...           ...
    A3
    A4
    B1
    B2
  }
var
  n,i,k,kv,kv1,
  kv2,kv3,kk,kf,kn : Integer;
  aux,wx,w0        : Double;
  s                : string;
  C                : TwsDataSetCol;
begin
  { Numero de linhas da matriz }
  n:=0;
  for i:=1 to Fac.Len do
    Inc(n, TwsFactor(Self.Struct.Col[Fac[i]]).LevelNames.Count);
  { Matriz das medias, vari?ncias, numero de repeticoes e somas dos pesos }
  Result := TwsGeneral(Jota(n, 4*Col.Len, mtGeneral, 0));
  Result.Name := 'PFatEstat';
  { Nomes dos n?veis nas linhas da matriz de sa?da }
  for kf:=1 to Fac.Len do
    begin
    C:=Self.Struct.Col[Fac[kf]];
    s:=System.Copy(C.Name,1,3)+'.';
    for i := 1 to TwsFactor(C).LevelNames.Count do
      Result.RowName[i] := s+TwsFactor(C).LevelNames[i-1];
    end;

  { Nomes das vari?veis dependentes (para media) e repeti??es }
  for i := 0 to Col.Len-1 do
    begin
    kf:=4*i+1;
    s:=ColName[Col[i+1]];
    Result.ColName[kf] := 'Med_'+s;
    Result.ColName[kf+1]:='Var_'+s;
    Result.ColName[kf+2]:='Obs_'+s;
    Result.ColName[kf+3]:='Pes_'+s;
    end;

  for kf:=1 to Fac.Len do
    begin
    kn:=(kf-1)*TwsFactor(Self.Struct.Col[Fac[kf]]).LevelNames.Count;
    for i:=IRow to LRow do                    // M?dias desde as linhas IRow at? LRow
      begin
      kk := kn+Self.AsInteger[i,Fac[kf]]+1;   // kk ? a linha do resultado
      for k := 1 to Col.Len do                // Para todas as vari?veis dependentes
        begin
        aux :=  Self[i,Col[k]];
        wx := Self[i,WInd];
        if (not wsGLib.IsMissValue(Aux) and (wx>0)) then
          begin
          kv := 4*k-3;                          // Coluna para a media
          kv1 := kv+1;                          // Coluna para a variancia
          kv2 := kv+2;                          // Coluna para o numero de repeticoes
          kv3 := kv+3;                          // Coluna para a soma dos pesos
          w0:=Result[kk,kv3];                   // Total dos pesos ate passo anterior
          Result[kk,kv3] := Result[kk,kv3]+wx;  // Acumula os pesos ate passo atual
          Result[kk,kv2] := Result[kk,kv2]+1;   // N?mero de repeti??es
          aux := (wx/Result[kk,kv3])*(aux-Result[kk,kv]);
                                                // SQ ajustada
          Result[kk,kv1] := Result[kk,kv1]+aux*aux*(Result[kk,kv3]*w0/wx);
          Result[kk,kv]:=Result[kk,kv]+aux;     // media
          end
        end // for k
      end // for i
    end;

  for i:=1 to Result.NRows do                  // Variancias
    for k:=1 to Col.Len do
      begin
      kv:=4*k-2;
      kv1:=kv+2;
      Result[i,kv]:=Result[i,kv]/(Result[i,kv1]-1)
      end
end;

{$ifdef WINSTAT_FULL}
function TwsDataSet.LRImput(Col: TwsLIVec; nCond: Integer; rCond: Double; Sim: boolean;
   var RList: TwsDataSets; var lf: TList): TwsDataSet;
{ Objetivo
    Preenchimento de falhas por regress?o simples
  Par?metros
    nCond : n?mero m?nimo de observa??es para predi??o
    rCond : valor m?nimo de r para participa??o da vari?vel
    Sim   : True para inclus?o de erro simlulado na predi??o; false para predi??o pelo modelo
    lf    : Lista de vetores que indicam os ?ndices das falhas existentes, por coluna. ?ndices
            negativos s?o utilizados para indicar falhas preenchidas
}
const
  BufSize = 200;
var
  i,j,k,n,ii,ik: Integer;
               // lista de arrays de inteiros com os indices das falhas
  NorSim       : TwsProbStdNormal;  // simulador normal
  iv,Buf,Asc   : TwsLIVec;
  my,mx,mxy,ql,
  my2,mx2,qr   : Double;
  yv           : TwsVec;
  tb           : TwsDataSet;

  procedure Cross(kk, jj: Integer);
  { Obt?m m?dias, SQ&P de desvios entre valores das colunas kk e jj }
  var
    i1    : Integer;
    x     : TwsVec;
    di,dj,
    aux   : Double;
  begin
    my:=0; mx:=0; mxy:=0; my2:=0; mx2:=0; n:=0;
    for i1 := 1 to NRows do
      begin
      x := Row[i1];
      if not (wsGLib.FEquals(x[kk],wscMissValue) or wsGLib.FEquals(x[jj],wscMissValue)) then
        begin
        Inc(n);
        aux:=(n-1)/n;
        di := x[kk]-my;
        my := my + di/n;
        my2:=my2+aux*di*di;

        dj:=x[jj]-mx;
        mx:=mx+dj/n;
        mx2:=mx2+aux*dj*dj;

        mxy:=mxy+aux*di*dj
        end { if VecMiss }
      end { for i1 }
  end; // Cross

{
  yv[1]: b0
  yv[2]: b1
  yv[3]: r2
  yv[4]: s
  yv[5]: n
  yv[6]: Condicao
}
begin
  Buf:=TwsLIVec.Create(BufSize);
  lf:=TList.Create;
  // Copia as colunas de interesse
  Result:=Self.CopyByCols(Col);
  Result.Name:=Name+'_LR';
  // Obtem numero e indice dos valores perdidos em cada coluna
  for k:=1 to Result.nCols do
    begin
    iv:=TwsLIVec.CreateFrom([0]);
    j:=0;
    for i:=1 to nRows do
      begin
      if wsGLib.FEquals(Result[i,k],wscMissValue) then
        begin
        Inc(j);
        iv[1]:=iv[1]+1;  // acumula numero de valores perdidos
        Buf[j]:=i;       // guarda posicao
        end;
      if ((j=BufSize) or (i=nRows)) and (j>0) then
        IVecAppend(iv,Buf,False,j)
      end; // for i
    iv.Name:=Result.ColName[k];
    lf.Add(iv)           // insere indices na lista
    end;   // for k
  Buf.Free;

  // matrizes para as equacoes de regressao linear
  RList:=TwsDataSets.Create;
  Buf:=TwsLIVec.CreateFrom([3]);  // ordena em funcao do r2
  Asc:=TwsLIVec.CreateFrom([0]);  // ordena em ordem descendente
  NorSim:=TwsProbStdNormal.Create;     // se necessario, simula residuo

  // a resposta eh a coluna k
  for k:=1 to Result.nCols do
    begin
    // Cria conjunto de dados que ira armazenar equacoes
    tb:=TwsDataSet.Create(Result.ColName[k]);
    tb.MLab:='Equa??es de regress?o utilizadas no preenchimento de '+Result.ColName[k];
    tb.Struct.AddNumeric('Coef_Linear','Coeficiente linear da equa??o de regress?o');        //1
    tb.Struct.AddNumeric('Coef_Angular','Declividade da equa??o de regress?o');              //2
    tb.Struct.AddNumeric('Coef_Det','Coeficiente de determina??o entre as vari?veis');       //3
    tb.Struct.AddNumeric('Desv_Padrao','Desvio padr?o das observa??es');                     //4
    tb.Struct.AddNumeric('Num_Obs','N?mero de observa??es v?lidas');                         //5
    tb.Struct.AddQualitative('Condicao','Condi??o da equa??o para participar da estima??o'); //6
    with TwsQualitative(tb.Struct.Col[6]) do
      begin
      AddLevel('Nao');
      AddLevel('Sim');
      end;

    iv:=lf.Items[k-1];
    if iv[1]>0 then       // Coluna k tem valores perdidos?
      begin
      for i:=1 to Result.nCols do
        if Col[i]<>Col[k] then
          begin
          Cross(Col[k],Col[i]);
          yv:=TwsDFVec.Create(6);
          yv.Name:=ColName[Col[i]];
          if n>2 then
            begin
            yv[2]:=mxy/mx2;          // b0
            yv[1]:=my-yv[2]*mx;      // b1
            ql:=Sqr(yv[2])*mx2;
            qr:=my2-ql;
            yv[3]:=ql/my2;           // r2
            yv[4]:=Sqrt(qr/(n-2));   // s
            yv[5]:=n;                // n
            // se coluna i atende as condicoes, participa
            yv[6]:=Ord((n>=nCond) and (yv[3]>=rCond));   // 0 - false, 1 - true
            tb.MAdd(yv)
            end
          else
            begin
            yv[1]:=wscMissValue; yv[2]:=wscMissValue; yv[3]:=wscMissValue;
            yv[4]:=wscMissValue; yv[5]:=n; yv[6]:=0;
            end;
          end;

        // Se tb possui pelo menos uma linha, poder? haver preenchimento
        tb.SortRows(Buf,Asc); // ordena pelo r2 em ordem descendente
        RList.Add(tb);

        ii:=1;                // ii marca a equacao a ser utilizada
        j:=0;                 // j marca o numero de valores estimados
        // encerra se estimou todos os valores ou utilizou todas as equacoes
        while (j<=iv[1]) and (ii<=tb.nRows) do
          begin
          yv:=tb.Row[ii];    // utiliza equacao ii
          if yv[6]>0 then    // equacao ii serve ?
            begin
            ik:=Self.Struct.IndexOf(yv.Name);
            i:=1;
            while (j<iv[1]) and (i<=iv[1]) do  // iv[1] eh o numero de valores perdidos
              if (iv[i+1]>0) then
                begin
                if (not wsGLib.FEquals(Get(iv[i+1],ik),wscMissValue)) then
                  begin // Se o valor da coluna i nao eh perdido, estima o da coluna k
                  qr:=yv[1]+yv[2]*Get(iv[i+1],ik);
                  // Inclui residuo por simulacao?
                  if Sim then
                    qr:=qr+NorSim.RandValue*yv[4];
                  // preenche valor na copia
                  Result[iv[i+1],k]:=qr;
                  iv[i+1]:=-iv[i+1];              // marca observacao ja estimada
                  Inc(j);
                  Inc(i)
                  end // if not
                else
                  Inc(i)
                end
              else
                Inc(i)
            end; // if yv
          Inc(ii)
          end;  // while
      end // if iv[1]
    end; // for k

  NorSim.Free;
  Buf.Free;
  Asc.Free;
end; // LRImput
{$endif}
function TwsDataSet.DataStd(Col: TwsLIVec; StdType: byte=0; NewData: Boolean=False): TwsDataSet;
{ Objetivo
    Padroniza colunas num?ricas
  Par?metros
    StdType: tipo de padroniza??o desejada
      0: Padroniza??o para m?dia 0 e vari?ncia 1
      1: Padroniza??o para vari?ncia 1
      2: Padroniza??o para m?dia 1
    NewData: Se True, cria um novo conjunto de dados; cc transforma o pr?prio conjunto
}
var
  i,k: Integer;
  m,s,x: TwsVec;
  n: TwsLIVec;
  aux,d: Double;
begin
  m:=VecConst(0,Col.Len);
  s:=VecConst(0,Col.Len);
  n:=TwsLIVec(VecConst(0,Col.Len,False));
  if NewData then
    Result:=Self.Copy
  else
    Result:=Self;
  // Obtem media e desvio padrao para cada coluna de interesse
  for i:=1 to Result.nRows do
    begin
    x:=Result.Row[i];
    for k:=1 to Col.Len do
      if not FEquals(x[Col[k]],wscMissValue) then
        begin
        n[k]:=n[k]+1;
        aux:=(n[k]-1)/n[k];
        d := x[Col[k]]-m[k];
        m[k] := m[k] + d/n[k];
        s[k] := s[k] + aux*d*d
        end
    end;
  for k:=1 to Col.Len do
    s[k]:=ScalarSqrt(s[k]/(n[k]-1));
  // Aplica a transformacao
  case StdType of
    0: for i:=1 to Result.nRows do
         begin
         x:=Result.Row[i];
         for k:=1 to Col.Len do
           if not FEquals(x[Col[k]],wscMissValue) then
             Result[i,Col[k]] := ScalarDiv((x[Col[k]]-m[k]),s[k])
         end;
    1: for i:=1 to Result.nRows do
         begin
         x:=Result.Row[i];
         for k:=1 to Col.Len do
           if not FEquals(x[Col[k]],wscMissValue) then
             Result[i,Col[k]] := ScalarDiv(x[Col[k]],s[k])
         end;
    2: for i:=1 to Result.nRows do
         begin
         x:=Result.Row[i];
         for k:=1 to Col.Len do
           if not FEquals(x[Col[k]],wscMissValue) then
             Result[i,Col[k]] := (x[Col[k]]/m[k])*100
         end;
  end; // case
end; // DataStd

function TwsDataSet.ColsToImp(var k: Integer): Integer;
{ Objetivo
    Dadas as defini??es dos atributos das colunas (tamanho, em particular) para sa?da de
    um conjunto de dados, determina quantas colunas poder?o ser impressas.
  Par?metros
    k: Retorna o n?mero de colunas
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
 }
var
  Len      : Integer;
  ActualLen: Integer;
begin
  Try
    Len       := PrintOptions.LineLen - PrintOptions.MaxIDSize;
    Result    := 0;
    ActualLen := 0;

    repeat     { Quantas colunas serao impressas ? }
      Inc(k);
      Inc(Result);
      ActualLen := ActualLen + Struct.Col[k].Size;
    until (k >= NCols) or (ActualLen > Len);

    if Len < ActualLen then
       begin
       Dec(k);
       Dec(Result)
       end
  Except
    Raise Exception.CreateFmt('Classe: %s'#13 +
                              'M?todo: %s'#13 +
                              'Erro: ?ndice [%d] inv?lido',
                              [ClassName, 'ColsToImp', K]);
  End;
end; {TwsDataSet.ColsToImp}

procedure TwsDataSet.Print(Buffer: TStrings);
{ Objetivo
    Imprime o conjunto de dados na janela de texto, de acordo com os atributos das colunas
  M?todos chamados
    Print herdado
  Campos modificados
    PrintOptions.LineLen
}
begin
  If FColIdentName <> '' Then PrintOptions.LineLen := PrintOptions.LineLen - PrintOptions.MaxIDSize;
  Inherited Print(Buffer);
  PrintOptions.LineLen := PrintOptions.LineLen + PrintOptions.MaxIDSize;
end; { TwsDataSet.Print }

Procedure TwsDataSet.CNameLoadFromStream(Reader:TReader);
{ Objetivo
    Carrega do disco a lista de objetos que definem as colunas de um conjunto de dados.
  Par?metros
    Reader: Leitor da lista
}
Var
  Cn    :String;
  Obj   :TwsDataSetCol;
Begin
  With Reader Do
    Begin
    ReadListBegin;
    While Not EndOfList Do
      Begin
      CN := ReadString;
      Obj := CreateDatasetVar(ReadInteger);
      Obj.DataSet := Self;
      Obj.LoadFromStream(Reader);
      CName.AddObject('', Obj); {Rochedo, 16/05/1998}
      NCols := NCols + 1;
      End;
    ReadListEnd;
    End;
End; { TwsDataSet.CNameLoadFromStream }

Procedure TwsDataSet.InternalLoad3(Reader: TReader);
{ Objetivo
    Carrega parte do conjunto de dados do disco
  Par?metros
    reader: objeto que faz a leitura
  M?todos chamados
    InternalLoad3 herdado
  Campos modificados
    FColIdentName
}
Begin
  Inherited InternalLoad3(Reader);
  SetColIdentName(Reader.ReadString);
  FModified := True;
End;

Procedure TwsDataSet.CNameSaveToStream(Writer:TWriter);
{ Objetivo
    Descarrega para o disco a lista de objetos que definem as colunas de um conjunto de
    dados.
  Par?metros
    Reader: Escritor da lista
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Var i  :Integer;
Begin
  With Writer Do
    Begin
    WriteListBegin;
    For i := 1 To Struct.Cols.Count Do
      begin
      WriteString(Struct.Col[i].Name);
      Struct.Col[i].SavetoStream(Writer);
      end;
    WriteListEnd;
    End;
End; { TwsDataSet.CNameSaveToStream }

Procedure TwsDataSet.InternalSave3(Writer: TWriter);
{ Objetivo
    Grava parte do conjunto de dados em disco
  Par?metros
    Writer: objeto para escrita em disco
  M?todos chamados
    InternalSave3 herdado
  Campos alterados
    Nenhum
}
Begin
  Inherited InternalSave3(Writer);
  Writer.WriteString(FColIdentName);
  FModified := False;
End;

procedure TwsDataSet.DeleteCols(Col: TwsLIVec);
{ Objetivo
    Elimina colunas especificadas do conjunto de dados
  Par?metros
    Col: ?ndices das colunas para elimina??o
  M?todos chamados
    DeleteCols herdado
  Campos alterados
    Struct
}
var
  i  : Integer;
  iOk: Boolean;
begin
  MDeleteCols(Col);
  for i := Col.Len downto 1 do        // elimina os atributos das colunas
    begin
    Struct.Col[Col[i]].Free;
    Struct.Cols.Delete(Col[i]-1);
    end;
  FNCols := FNCols - Col.Len;  
end;  // TwsDataSet.DeleteCols
(*
procedure TwsDataSet.DeleteCol(i: Integer);
Begin
  if i <= FnCols then
     begin
     Inherited DeleteCol(i);
     Struct.Col[i].Free;
     Struct.Cols.Delete(i-1);
     end;
End;
*)

Function TwsDataSet.Isfactor(Const S: String): Boolean;
{ Objetivo
    Vari?vel especificada ? do tipo fator?
  Par?metros
    S: nome da vari?vel
  M?todos chamados
    IndexOf
  Campos alterados
    Nenhum
}
var i: Integer;
Begin
  Result := False;
  i := Struct.IndexOf(SysUtilsEx.AllTrim(S));
  If i > -1 Then
     Case Struct.Col[i].ColType Of
       dtQuant, dtQualit, dtQualitOrd:  Result := True;
       End; { Case }
End;

procedure TwsDataSet.SetColName(i: Integer; S: string);
{ Objetivo
    Atribui nome ? coluna especificada
  Par?metros
    i: ?ndice da coluna
    S: Nome a ser atribu?do
  M?todos chamados
    SetColName herdado
  Campos alterados
    Nenhum
}
var Obj: TwsDataSetCol;
begin
  inherited SetColName(i, S);
  Obj := Struct.Col[i];
  with Obj do
    begin
    Obj.Name := S;
    if Length(S) > Obj.Size then Obj.Size := Length(S) + 2;
    end;
End; { TwsDataSet.SetColName }

Function TwsDataSet.GetStr(i,j: Integer): String;
{ Objetivo
    Obt?m o valor de uma vari?vel do conjunto de dados em formato string
  Par?metros
    i: ?ndice de linha
    j: ?ndice de coluna
  M?todos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observa??o
    Se a vari?vel for num?rica, retorna ?.? se for valor perdido. Aplica a fun??o Fuzz para eliminar
    d?gitos que est?o fora da precis?o especificada; se for do tipo fator, retorna o nome do n?vel
    correspondente
}
Var Obj    : TwsDataSetCol;
    ColSize: Word;
    Value  : Double;
Begin
  Obj := Struct.Col[j];
  if Obj.Size > Length(Obj.Name) then
     ColSize := Obj.Size
  else
     ColSize := Length(Obj.Name);

  If not IsMissValue(i,j,Value) Then
     Begin
     Case Obj.ColType Of
       dtNumeric:
         Result := Format('%*.*g',[ColSize, TwsNumeric(Obj).Precision, Fuzz(Value)]);

       dtQuant, dtQualit, dtQualitOrd :
         Result := Format('%s',[LeftPad(TwsFactor(Obj).LevelNames[Trunc(Value)], ColSize)]);
       End
     End
  Else
     Result := Format('%*s', [ColSize, '-'])
End; { TwsDataSet.GetStr }

Function TwsDataSet.GetInt(i, j: Integer): Integer;
{ Objetivo
    Obt?m o valor de uma vari?vel do conjunto de dados como inteiro
  Par?metros
    i: ?ndice de linha
    j: ?ndice de coluna
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
  Observa??o
    Se a vari?vel for do tipo fator, o inteiro que retorna ? o ?ndice
}
Begin
  Result := Trunc(Get(i,j));
End; { TwsDataSet.GetInt }

Function TwsDataSet.GetDT(i, j: Integer): TDateTime;
{ Objetivo
    Obt?m o valor de uma vari?vel do conjunto de dados como Data/Tempo
  Par?metros
    i: ?ndice de linha
    j: ?ndice de coluna
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
  Observa??o
    Se a vari?vel for do tipo fator, o valor retornado n?o tem sentido
}
Begin
  Result := TDateTime(Get(i,j));
End; { TwsDataSet.GetInt }

function TwsDataSet.GetColName(i: Integer): string;
{ Objetivo
    Recupera o niome da coluna especificada
  Par?metros
    i: ?ndice da coluna
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
begin
  Result := Struct.Col[i].Name;
end;

Procedure TwsDataSet.PutStr(i,j: Integer; X: String);
{ Objetivo
    Atribui valor de elemento especificado em formato string
  Par?metros
    i: ?ndice de lina
    j: ?ndice de coluna
  M?todos chamados
    Put
    AddLevel
  Campos alterados
    Nenhum
}
Var Col: TwsFactor;
Begin
  FModified := True;
  X := SysUtilsEx.AllTrim(X);
  Col := TwsFactor(Struct.Col[j]);

  If Col.ColType = dtNumeric Then
     Put(i,j,FracToReal(X))
  Else
     Put(i,j,Col.AddLevel(X));
End; { TwsDataSet.PutStr }

Procedure TwsDataSet.AddCol(Const N, Lab: String; ColType: TwsEnumDataType; ps: byte = 15; pf: byte = 8);
{ Objetivo
    Adiciona descritor de coluna no conjunto de dados
  Par?metros
    N: nome do conjunto de dados
    Lab: r?tulo do conjunto
    ColType: tipo de coluna a inserir
  M?todos chamados
    AddColEx
  Campos alterados
}
Var
  Col : TwsDataSetCol;
  i   : Integer;
Begin
  Case ColType Of
    dtNumeric   : Col := TwsNumeric.Create      (N,Lab,ps,pf);
    dtQualit    : Col := TwsQualitative.Create  (N,Lab,ps);
    dtQuant     : Col := TwsQuantitative.Create (N,Lab,ps);
    dtQualitOrd : Col := TwsOrdered.Create      (N,Lab,ps);
    End; { Case }

  Struct.AddColEx(Col);
End; { TwsDataSet.AddCol }

// Prepara e preenche a estrutura com um determinado valor.
// Se a estrutura foi criada mas n?o existem dados, Fill cria os dados e inicializa-os.
// Dever?, logicamente, ser chamado sempre ap?s a defini??o da estrutura dos dados.
procedure TwsDataSet.Fill(const x: Double);

  procedure FillVector(v: TObject);
  begin
    TwsVec(v).Len := FnCols; // n?o faz nada se igual
    TwsVec(v).Fill(x);
  end;

var i, j: Integer;
    v: TwsDFVec;
begin
  if FnRows = FList.Count then
     for i := 0 to FnRows-1 do
       begin
       TwsVec(FList[i]).Len := FnCols; // n?o faz nada se igual
       TwsVec(FList[i]).Fill(x);
       end
  else
     begin // inicializa os vetores
     for i := 0 to FList.Count-1 do TwsVec(FList[i]).Free;
     FList.Clear;
     for i := 0 to FnRows-1 do
       begin
       v := TwsDFVec.Create(FnCols);
       v.Fill(x);
       FList.Add(v);
       end;
     end;
end;

Function TwsDataSet.GetDataSet: TwsDataSet;
{ Objetivo
    Retorna endere?o do pr?prio conjunto
}
Begin
  Result := Self;
End;

function  TwsDataSet.IndexColsFromString(const Cols: String): TwsLIVec;
{ Objetivo
    Obt?m array de ?ndices de colunas especificadas como string
  Par?metros
    Cols: colunas especificadas como string
  M?todos chamados
    IndexCols
  Campos alterados
    Nenhum
}
Begin
  Result := Struct.IndexCols(Cols);
End;

Function  TwsDataSet.IndexColsFromStrings(Cols: TStrings): TwsLIVec;
{ Objetivo
    Obt?m array de ?ndices de colunas especificadas como lista
  Par?metros
    Cols: Lista de strings
  M?todos chamados
    strTokenFromStrings
    IndexColsFromString
  Campos alterados
    Nenhum
}
var s: String;
Begin
  s := StringsToString(Cols, ',');
  Result := IndexColsFromString(s);
End;

function TwsDataSet.GetVar(R, C: Integer): Variant;
{ Objetivo
    Recupera elemento de acordo com tipo
  Par?metros
    R: ?ndice de linha
    C: ?ndice de coluna
  M?todos chamados
    Get
    AsString
  Campos alterados
    Nenhum
}
begin
  if Struct.Col[C].ColType = dtNumeric then
     Result := Get(R, C)
  else
     Result := AsString[R, C];
end;

procedure TwsDataSet.PutVar(R, C: Integer; const Value: Variant);
{ Objetivo
    Atribui elemento de acordo com tipo
  Par?metros
    R: ?ndice de linha
    C: ?ndice de coluna
    Value: valor a atribuir
  M?todos chamados
    Put
  Campos alterados
    Nenhum
}
begin
  if Struct.Col[C].ColType = dtNumeric then
     Put(R,C,Value)
  else
     AsString[R, C] := Value;
end;

function TwsDataSet.GetTrimStr(i,j: Integer): String;
{ Objetivo
    Recupera elemento na forma de string, tratando valores perdidos
  Par?metros
    i: ?ndice de linhas
    j: ?ndice de colunas
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Var Obj  : TwsDataSetCol;
    Value: Double;
Begin
  Obj := Struct.Col[j];
  If not IsMissValue(i,j,Value) Then
     Case Obj.ColType Of
       dtNumeric:
          Result := FloatToStr(Value);

       dtQuant, dtQualit, dtQualitOrd :
          Result := TwsFactor(Obj).LevelNames[Trunc(Value)];
       End // Case
  Else
     Result := '-';
End; { TwsDataSet.GetTrimStr }

procedure TwsDataSet.StructColsToXML(Buffer: TStrings; var Ident: Integer);
begin
  Struct.ToXML(Buffer, Ident);
end;

// Permite ao chamador verificar o tipo de uma c?lula e ao mesmo
// tempo retornar seu valor
procedure TwsDataSet.GetSheetValue(L, C: Integer;
                                   var IsNUM: Boolean;
                                   var sValue: String;
                                   var rValue: Real);
var x: double;
    s: String;
begin
  if C = 0 then
     begin
     IsNUM := False;
     sValue := IntToStr(L);
     //if (ColIdentName <> '') then sValue := sValue + Self.RowName[L];
     s := Self.RowName[L];
     if (s <> '') then sValue := s + '  (' + sValue + ')';
     end
  else
     if Self.Struct.Col[C].ColType = dtNumeric then
        if not Self.IsMissValue(L, C, x) then
           begin
           IsNUM := True;
           rValue := x;
           end
        else
           begin
           IsNum := False;
           sValue := '----';
           end
     else
        begin
        IsNUM := False;
        sValue := SysUtilsEx.AllTrim(Self.AsString[L, C]);
        end;
end;

{$ifdef Planilha}
// Mostra o Dataset em uma planilha semelhante as planilhas do Excel
procedure TwsDataSet.ShowInSheet(Sheet: TBaseSpreadSheetBook);
var s: String;
    L, C: Integer;
    F, Font: TFont;
    x: double;
    UseThisFont: Boolean;
    IsNUM: Boolean;
    sValue: String;
    rValue: Real;
    NumLinhas: Integer;
begin
  Sheet.ActiveSheet.ShowHeaders := False;
  NumLinhas := self.nRows;

  if Assigned(FChooseFont) then Font := TFont.Create;
  StartWait();
  Sheet.BeginUpdate();
  try
    if Name <> '' then s := ' ' + Name else s := ' Sem Nome';
    if MLab <> '' then
       Sheet.Caption := s + ' - ' + MLab
    else
       Sheet.Caption := s;

    //Sheet.ActiveSheet.SetDefaultFont('Arial', 10);

    // Nome da coluna identificadora
    s := 'Obs.';
    if FColIdentName <> '' then s := s + '/'+FColIdentName;
    Sheet.ActiveSheet.TopLeftText := s;

    ColNamesToSheet(Sheet);

    // Nome das colunas
    for C := 1 to nCols do
      begin
      Sheet.ActiveSheet.WriteCenter(1, C+1, Struct.Col[C].Name);
      Sheet.ActiveSheet.BoldCell(1, C+1);
      end;

    for L := 1 to NumLinhas do
      begin
      // Coluna 0 - Cabecalho das Linhas
      GetSheetValue(L, 0, IsNUM, sValue, rValue);
      if isNUM then
         Sheet.ActiveSheet.WriteCenter(L+1, 1, rValue)
      else
         if sValue <> '' then
            Sheet.ActiveSheet.WriteCenter(L+1, 1, sValue);

      if Row[L].HasFont then F := Row[L].Font else F := Nil;
      for C := 1 to nCols do
        begin
        GetSheetValue(L, C, IsNUM, sValue, rValue);
        if IsNUM then
           Sheet.ActiveSheet.WriteCenter(L+1, C+1, rValue)
        else
           if sValue <> '' then
              Sheet.ActiveSheet.WriteCenter(L+1, C+1, sValue);

        if F = Nil then F := Struct.Col[C].DataFont;
        if Assigned(FChooseFont) then
           begin
           FChooseFont(Self, L, C, Font, UseThisFont);
           if UseThisFont then F := Font;
           end;
        if (F.Name <> 'Arial') or (F.Size <> 10) or (F.Style <> []) then
           Sheet.ActiveSheet.SetCellFont(L+1, C+1, F.Name, F.Size, F.Color,
                                         fsBold in F.Style, fsItalic in F.Style);
        end;
      end;
  finally
    Sheet.EndUpdate();
    StopWait();
    if Assigned(FChooseFont) then Font.Free();
  end;
end;

procedure TwsDataSet.ColNameToSheet(indexCol: Integer; Sheet: TBaseSpreadSheetBook; ColSheet: Integer);
begin
  Sheet.ActiveSheet.BoldCell(1, ColSheet);
  Sheet.ActiveSheet.WriteCenter(1, ColSheet, Struct.Col[IndexCol].Name);
end;

procedure TwsDataSet.ColNamesToSheet(Sheet: TBaseSpreadSheetBook);
var C: Integer;
begin
  for C := 1 to nCols do
    ColNameToSheet(C, Sheet, C+1);
end;

procedure TwsDataSet.ColNamesToSheet(indexCols: TwsLIVec; Sheet: TBaseSpreadSheetBook);
var C: Integer;
begin
  for C := 1 to indexCols.Len do
    ColNameToSheet(indexCols[C], Sheet, C+1);
end;
{$endif Planilha}

function TwsDataSet.RSmoothMedian(xCol: Integer;
                                  yCol: TwsLIVec;
                                  xSort: boolean = True;
                                  S4253: boolean = True): TwsDataSet;
{ Objetivo
    Alisar uma seq??ncia de valores por meio de alisadores n?o lineares. Implementa
    dois alisadores n?o lineares: 4253H-duplo e 3RSSH-duplo
  Par?metros
    xCol: ?ndice da coluna dos valores da vari?vel X, supostamente equidistantes. As linhas do
    conjunto de dados s?o ordenadas em fun??o dos valores dessa vari?vel.
    yCol: ?ndices das colunas da vari?vel Y que ser?o alisadas. Para cada coluna que ser? alisada
    ser? criada uma coluna para armazenar as rugosidades
    S4253HD: alisador que ser? utilizado. Se True (default) o alisador a ser utilizado ser? 4253H-duplo
    e 3RSSH-duplo, se False.
}
const
  nw = 5;
type
  wArray = array[1..nw] of Double;
var
  i,j,LCol: integer;
  v,v1    : TwsVec;
  s       : string;

   procedure S2(Col: Integer;ESave: Double);
   { Alisa atrav?s de corridas de medianas de dimens?o 2. Utilizada para recentrar resultados
       de corridas de medianas de dimens?o 4. ESave restaura o valor original da ?ltima posi??o.
   }
   var
     N,I: Integer;
   begin
     N:=Result.NRows;
     for I:=2 to N-1 do
       Result[I,Col]:=(Result[I+1,Col]+Result[I,Col])/2;
     Result[N,Col]:=ESave
   end; // S2

   procedure MedOf3(const Y1,Y2,Y3: Double; var XMed: Double; var Ch: Boolean);
   { Obt?m a mediana de Y1, Y2 e Y3 e coloca em xMed. Ch retorna True se Y2 n?o ? a mediana
   }
   begin
     XMed:=Y2;
     if (Y2-Y1)*(Y3-Y2)<0 then
       begin
       Ch:=True;
       if (Y3-Y1)*(Y3-Y2)>0 then
         xMed:=Y1
       else
         xMed:=Y3
       end
   end; // MedOf3

   procedure EndPts(Col: Integer);
   { Estima valores alisados para os dois pontos extremos da seq??ncia utilizando a regra da
     extrapola??o do ponto final. Todos os valores da seq??ncia, exceto os extremos, j? foram
     alisados
   }
   var
     y0,yMed: Double;
     Ch     : Boolean;
     N      : Integer;
   begin
     N:=Result.NRows;
     Ch:=False;
     // Extremo inferior
     y0:=3*Result[2,Col]-2*Result[3,Col];
     MedOf3(y0,Result[1,Col],Result[2,Col],yMed,Ch);
     Result[1,Col]:=yMed;
     // Extremo superior
     y0:=3*Result[N-1,Col]-2*Result[N-2,Col];
     MedOf3(y0,Result[N,Col],Result[N-1,Col],yMed,Ch);
     Result[N,Col]:=yMed;
   end; // EndPts

   procedure Split(Col: Integer; var Ch: Boolean);
   { Encontra platos de 2-n?meros na seq??ncia e aplica algoritmo de quebra
   }
   var
     W     : array[1..6] of Double;
     Y1,aux: Double;
     I1,I,
     NM2,N : Integer;
   begin
     N:=Result.NRows;
     NM2:=N-2;
     for I:=1 to 4 do
       W[I+2]:=Result[I,Col];
     // Se Result[1,Col]=Result[2,Col] e <> Result[3,Col], trata primeiros dois como um plato de 2
     // com regra de ponto final
     W[2]:=Result[3,Col];
     I1:=1;
     repeat
       repeat
         if FEquals(W[3],W[4]) then
           if (W[3]-W[2])*(W[5]-W[4])<0 then
             begin
             if I1>=3 then
               begin
               Y1:=3*W[2]-2*W[1];
               MedOf3(Y1,W[3],W[2],aux,Ch);
               Result[I1,Col]:=aux
               end;
             if I1<NM2 then             // 30
               begin
               Y1:=3*W[5]-2*W[6];
               MedOf3(Y1,W[4],W[5],aux,Ch);
               Result[I1+1,Col]:=aux
               end
             end;
         for I:=1 to 5 do         //40
           W[I]:=W[I+1];
         Inc(I1);
         if I1<NM2 then
           W[6]:=Result[I1+3,Col];
       until I1>=NM2;
       W[6]:=W[3];                //60
     until I1>=N
   end; // Split

   procedure Sort(var W: wArray; N: Integer);
   { Ordena array }
   var
     Gap,i,j,jj: Integer;
     Temp      : Double;
   begin
     Gap := N;
     repeat
       Gap := Gap div 2;
       if Gap > 0 then
         begin
         for i := 1 to N - Gap do
           begin
           j := i;
           while (j >= 1) do
             begin
             jj := j + Gap;
             if W[j]>W[jj] then
               begin
               Temp:=W[J];
               W[j]:=W[jj];
               W[jj]:=Temp
               end
             else
               j := 0;
             Dec(j,Gap);
             end { while }
           end { for }
         end { if }
     until Gap = 0
   end; { Sort }

   function Median(var W: wArray; N: Integer): Double;
   { Mediana de um array ordenado }
   var
     M: Integer;
   begin
     M:=N div 2 + 1;
     Result:=(W[M]+W[N-M+1])/2
   end; // Median

   procedure RunMed(Col: Integer; Len: Byte; var W, S: wArray);
   { Alisa seq??ncias por meio de corridas de medianas de dimens?o Len. Ao inv?s de utilizar
     este procedimento para Len=3, utilize S3
     W: array de trabalho no qual os dados estar?o ordenados
     S: Age como uma janela para os dados
   }
   var
     Temp     : Double;
     SavePT,N,
     SmoPT,I,J: Integer;
   begin
     N:=Result.NRows;
     for I:=1 to Len do
       begin
       W[i]:=Result[I,Col];
       S[i]:=Result[I,Col];
       end;
     SavePt:=1;
     SmoPt:=Trunc((Len+2)/2);
     for I:=Len+1 to N do
       begin
       Sort(W, Len);
       Result[SmoPt,Col]:=Median(W,Len);
       Temp:=S[SavePt];
       J:=1;
       while (J<=Len) and (W[j]<>Temp) do
         Inc(J);
       W[J]:=Result[I,Col];
       S[SavePt]:=Result[I,Col];
       SavePt:=SavePt mod Len + 1;
       Inc(SmoPt)
       end;
     Sort(W, Len);
     Result[SmoPt,Col]:=Median(W,Len);
   end; // RunMed

   procedure S4(Col: Integer; var ESave: Double; var W,S: wArray);
   { Alisa atraves de corridas de mediana de tamanho 4
     A dimens?o par para amedianas aumenta a sequencia de sa?da em 1 posi??o no final, pois
     ela n?o pode ser sim?trica. ESave guarda ?ltimo valor, uma vez que n?o existe outra posi??o
     para ele. Y[1] n?o ? alterado.
   }
   var
     EM1: double;
     N  : Integer;
   begin
     N:=Result.NRows;
     ESave:=Result[N,Col];
     EM1:=Result[N-1,Col];
     RunMed(Col,4,W,S);
     Result[2,Col]:=(Result[1,Col]+Result[2,Col])/2;
     Result[N,Col]:=(EM1+ESave)/2
   end; // S4

   procedure S5(Col: Integer; var W,S: wArray);
   { Alisamento por medianas de dimens?o 5 }
   var
     Change     : boolean;
     YMed1,YMed2: Double;
     N          : Integer;
   begin
     N:=Result.NRows;
     Change:=False;
     MedOf3(Result[1,Col],Result[2,Col],Result[3,Col],YMed1,Change);
     MedOf3(Result[N,Col],Result[N-1,Col],Result[N-2,Col],YMed2,Change);
     RunMed(Col,5,W,S);
     Result[2,Col]:=YMed1;
     Result[N-1,Col]:=YMed2;
   end; // S5

   procedure Hann(Col: Integer);
   var
     I     : Integer;
     Y1,Y2,
     Y3    : Double;
   begin
     Y2:=Result[1,Col];
     Y3:=Result[2,Col];
     for I:=2 to Result.NRows-1 do
       begin
       Y1:=Y2;
       Y2:=Y3;
       Y3:=Result[I+1,Col];
       Result[I,Col]:=(Y1+Y2+Y2+Y3)/4;
       end;
   end; // Hann

   procedure S3(Col: Integer; var Ch: boolean);
   { Calcula corridas de medianas de dimens?o 3. Ch retorna True se qualquer mudan?a foi
     realizada
   }
   var
     Y1,Y2,
     Y3,a1 : Double;
     I,N   : Integer;
   begin
     N:=Result.NRows;
     Y2:=Result[1,Col];
     Y3:=Result[2,Col];
     for I:=2 to N-1 do
       begin
       a1:=Result[I,Col];
       Y1:=Y2;
       Y2:=Y3;
       Y3:=Result[I+1,Col];
       MedOf3(Y1,Y2,Y3,a1,Ch);
       Result[I,Col]:=a1
       end;
   end; // S3

   procedure S3R(Col: Integer);
   { Obt?m corridas repetidas de medianas de dimens?o 3 }
   var
     Change: Boolean;
   begin
     repeat
       Change:=False;
       S3(Col,Change)
     until not Change;
     EndPts(Col)
   end; // S3R

   procedure S3RSSH(Col: Integer);
   { Faz o alisamento S3RSSH-Duplo relativo a coluna Col de SR }
   var
     Change: boolean;
   begin
     S3R(Col);
     Change:=False;
     Split(Col,Change);
     if Change then
       begin
       S3R(Col);
       Change:=False;
       Split(Col,Change);
       if Change then S3R(Col)
       end;
     Hann(Col);
     end;  // S3RSSH

   procedure S4253H(Col: Integer);
   { Alisamento por S4253H }
   var
     EndSav   : Double;
     Work,Save: wArray;
     Change   : boolean;
   begin
     Change:=False;
     S4(Col,EndSav,Work,Save);
     S2(Col,EndSav);
     S5(Col,Work,Save);
     S3(Col,Change);
     EndPts(Col);
     Hann(Col)
   end;  // S4253H

begin
  // Se necessario, ordena linhas do conjunto de acordo com a coluna xCol
  if not xSort then
    begin
    v:=TwsLIVec.Create(1); v[1]:=xCol;
    v1:=TwsLIVec.Create(1); v1[1]:=1;
    SortRows(TwsLIVec(v), TwsLIVec(v1));
    v.Free; v1.Free
    end;
  // conjunto de dados para os resultados do alisamento
  Result:=TwsDataSet.Create(Name + '_Alisado');
  // Insere coluna para X
  Result.Struct.AddCopyCol(Struct.Col[xCol]);     // coluna 1
  // Insere colunas para Y - yCol.Len colunas
  For i := 1 to yCol.Len do                       // coluna i+1
    Result.Struct.AddCopyCol(Struct.Col[yCol[i]]);
  // Insere colunas para as rugosidades
  For i := 1 to yCol.Len do                       // coluna yCol.Len+1+i
    begin
    s:=System.Copy(Struct.Col[yCol[i]].Name,1,4)+'_Rug';
    Result.Struct.AddColEx(TwsNumeric.Create(s,
      'Rugosidades para a vari?vel '+Struct.Col[yCol[i]].Name,8,5))
    end;
  // copia valores para alisamento. X n?o ? alterada
  for i:=1 to NRows do
    begin
    v:=TwsDFVec.Create(2*yCol.Len+1);
    v[1]:=Data[i,xCol];
    for j:=1 to yCol.Len do
      v[j+1]:=Data[i,yCol[j]];
    Result.MAdd(v)
    end;
  // Para cada vari?vel Y
  for i:=1 to yCol.Len do
    begin
    LCol:=yCol[i];
    if S4253 then
      S4253H(LCol)
    else
      S3RSSH(LCol);
    Inc(LCol,yCol.Len);                    // coluna da rugosidade
    for j:=1 to NRows do                   // Obtem as rugosidades
      Result[j,LCol]:=Data[j,yCol[i]]-Result[j,yCol[i]];
    if S4253 then                       // Aplica o alisador as rugosidades (duplo)
      S4253H(LCol)
    else
      S3RSSH(LCol);
    for j:=1 to NRows do                         // Obtem resultados finais
      begin
      Result[j,yCol[i]]:=Result[j,yCol[i]]+Result[j,LCol];   // para o alisamento
      Result[j,LCol]:=Data[j,yCol[i]]-Result[j,yCol[i]]; // para as rugosidades
      end
    end
end; // RSmoothMedian

function TwsDataSet.LetterValues(xCol: TwsLIVec): TwsDataSet;
{ Objetivo
    Para o conjunto de valores de cada vari?vel encontra os quantis conhecidos como
    valores letra
  Par?metros
    xCol: ?ndices das vari?veis
  Observa??es
    Com exce??o da mediana, os quantis s?o obtidos aos pares, inferior e superior.
    Para cada conjunto de dados s?o obtidos, no m?nimo, os valores para mediana, quartos
    e extremos. Os demais dependem da quantidade de valores do conjunto, limitados a 64
    divis?es no conjunto. As letras utilizadas para representa??o dos quantis seguem a
    denomina??o utilizada na literatura inglesa. Tamb?m s?o obtidas outras quantidades
    relacionadas: amplitudes e amplitudes m?dias (com exce??o da mediana) e dispers?o
    relativa a distribui??o gaussiana (com exce??o da mediana e dos extremos)
      M - Median (mediana)
      H - Hinges (quartos)
      E - Eights (oitavos)
      D -        (d?cimos-sextos)
      C -        (trig?simos-segundos)
      B -        (sexag?simos-quartos)
}
const
  NMaxLet = 7;
  LetLab: array[1..NMaxLet] of string =
    ('M-Mediana','H-Quartos','E-Oitavos','D-Dec. Sextos','C-Trig. Segundos',
     'B-Sexag. Quartos','E-Extremos');
  Spread: array[1..NMaxLet-2] of double=(1.349,2.301,3.068,3.726,4.308);
var
  NV,NLV,
  J,K,N     : Integer;
  Col       : TwsDataSetCol;
  V,W       : TwsVec;
begin
  Result:=TwsDataSet.Create('VL_'+Name);
  Result.MLab:='Valores Letra Para Vari?veis do Conjunto '+Name;
  Result.ColIdentName:='Valores_Letra';
  Col:=TwsQualitative.Create('Variavel','Vari?veis Especificadas para Valores Letra');
  for J:=1 to xCol.Len do
    TwsQualitative(Col).AddLevel(Struct.Col[xCol[J]].Name);
  Result.Struct.AddColEx(Col);                        // Coluna 1. Nomes de vari?veis
  Result.Struct.AddColEx(TwsNumeric.Create('Profundidade','Profundidade do Valor Letra',8,5)); //2
  Result.Struct.AddColEx(TwsNumeric.Create('Inferior','Valor Pr?ximo ao Extremo Inferior',8,5)); //3
  Result.Struct.AddColEx(TwsNumeric.Create('Superior','Valor Pr?ximo ao Extremo Superior',8,5)); //4
  Result.Struct.AddColEx(TwsNumeric.Create('Media','M?dia dos Limites Inferior e Superior',8,5)); //5
  Result.Struct.AddColEx(TwsNumeric.Create('Amplitude','Limite Superior - Limite Inferior',8,5)); //6
  Result.Struct.AddColEx(TwsNumeric.Create('Disp_Gauss','Dispers?o Gaussiana',8,5)); //7

  for NV:=1 to xCol.Len do
   begin
   W:=CopyCol(xCol[NV]);
   N:=W.Len;
   W.QuickSort(True);
   // Trata a mediana em separado
   J := N div 2 + 1;
   v:=TwsDFVec.Create(7);
   v.Name:=LetLab[1];
   v[1]:=NV-1; v[2]:=(N+1)/2;
   v[3]:=W[J]; v[4]:=W[N-J+1];v[5]:=(W[J]+W[N-J+1])/2;
   v[6]:=wscMissValue;v[7]:=wscMissValue;
   Result.MAdd(v);

   K:=N;
   NLV:=2;
   while (v[2]>2) and (NLV<=NMaxLet-1) do
     begin
     K:= (K+1) div 2;
     J:= K div 2 + 1;
     v:=TwsDFVec.Create(7);
     v.Name:=LetLab[NLV];
     v[1]:=NV-1; v[2]:=(K+1)/2;
     v[3]:=(W[J]+W[K-J+1])/2;
     v[4]:=(W[N-K+J]+W[N-J+1])/2; v[5]:=(v[3]+v[4])/2;
     v[6]:=v[4]-v[3]; v[7]:=v[6]/Spread[NLV-1];
     Result.MAdd(v);
     Inc(NLV);
     end;

   // Trata os extremos separadamente
   v:=TwsDFVec.Create(7);
   v.Name:=LetLab[NMaxLet];
   v[1]:=NV-1; v[2]:=1;
   v[3]:=W[1]; v[4]:=W[N]; v[5]:=(v[3]+v[4])/2;
   v[6]:=v[4]-v[3]; v[7]:=wscMissValue;
   Result.MAdd(v);

   W.Free;
   end;
end;

{ ========================== Versao FORTRAN ===========================}

function TwsDataSet.StemAndLeaf(xCol: TwsLIVec; Xtrems: Boolean=False): TStrings;
{ Objetivo
    Obter o dispositivo de ramo e folhas para a vari?vel num?rica especificada
  Par?metros
    xCol: Vetor com os ?ndices das colunas correspondentes ?s vari?veis de interesse
    Xtrems (default=False): True para fazer a escala em rela??o aos extremos. Se False
      a escala ? feita em rela??o aos valores adjacentes, excluindo os valores discrepantes,
      que s?o listados em ramos separados
  Observa??es
    Constru??o baseada na vers?o FORTRAN, dispon?vel em
      Velleman & Hoaglin - Applications, Basics and Computing of EDA
        p?g. 16 e posteriorea

}
const
  EPS   =1E-9;
  PltWid=90;
  NN    =4;
  NicNos: array[1..NN] of integer = (1,2,5,10);
var
  I,J,IAdjL,IAdjH,N,
  NLins,NLMax,Low,
  HI,PT1,PT2,LinWid,
  Rank,Stem,Cut,SpaCnt : Integer;

  Med,LHing,HHing,
  Step,AdjL,AdjH,
  Fract,Unt,NPW,Leaf : double;

  W                  : TwsVec;
  WI                 : TwsLIVec;
  st                 : string;

  Pula,NegNow,MedYet : boolean;

   function IntFN(const x: double): Integer;
   // Mesmo que fun??o Int mas considera EPS para arredondamento e retorna inteiro
   begin
    Result:=Trunc((1+EPS)*X)
   end;

    procedure YInfo(var IMed,LF,HF,IStep,LAdj,HAdj: double; var lh, hh: integer);
    { Obtem os valores resumo em W ordenado
       IMed - mediana
       LF - quarto inferior
       HF - quarto superior
       IStep - passo
       LAdj - Valor adjacente inferior
       HAdj - Valor adjacente superior
       lh - Indice do valor adjacente inferior
       hh - Indice do valor adjacente superior
    }
    var
      F1,F2: double;
      J,K   : integer;
    begin
      K:=N;
      J:=(K div 2)+1;
      IMed:=(W[J]+W[N-J+1])/2;

      K:=(K+1) div 2;
      J:=(K div 2)+1;
      LF:=(W[J]+W[K-J+1])/2;     // quarto inferior
      HF:=(W[N-K+J]+W[N-J+1])/2; // quarto superior

      IStep:=1.5*(HF-LF);        // passo
      F1:=LF-IStep;              // cerca inferior
      F2:=HF+IStep;              // cerca superior
      lh:=1;                     // Posi??es dos valores adjacentes
      while W[lh] <= F1 do Inc(lh);
      hh:=N;
      while W[hh] >= F2 do Dec(hh);

      LAdj:=W[lh];               // Valores adjacentes
      HAdj:=W[hh]
    end;

    procedure NPosW(HI,LO: double;
                    MaxP: Integer;
                    var PTot: Integer;
                    MZero: Boolean;
                    var Frt,U,NP: double);
    { Encontra comprimento de posi??o ?timo.
      Na entrada
        HI, LO - limite inferior e superior dos dados
        MaxP - N?mero m?ximo de posi??es permitida para plotagem
        MZero: True se se uma posi??o rotulada como -0 ? permitida
      Na sa?da
        PTot - n?mero desejado de posi??es
        U - ? um inteiro pot?ncia de 10 tal que NP=Frt*U
        NP - ? o comprimento ideal para a posi??o de plotagem
      }
    var
      AprxW: double;
      i    : Integer;
    begin
      AprxW:=(HI-LO)/MaxP;
      U:=Power(10,Floor(Log10(AprxW)));
      Frt:=AprxW/U;
      i:=1;
      while (Frt>NicNos[i]) and (i<=NN) do
        Inc(i);
      repeat
        repeat
          Frt:=NicNos[i];
          NP:=Frt*U;
          // Calcula o n?mero de posi??es necess?rias
          PTot:=IntFN(HI/NP)-IntFN(LO/NP)+1;
          // Se -0 ? poss?vel e ( se HI e L0 t?m sinais opostos ou se H1=0)
          // iremos necessitar da linha -0
          if (MZero and ((HI = 0) or (HI*LO<0))) then Inc(PTot);
          // PTot posicoes sao necessarias para plotagem. ? suficiente?
          if (PTot <= MaxP) then Exit;
          // Mais posicoes sao necessarias. Utiliza proximo numero otimo
          Inc(i);
        until i>NN;
        i:=1;
        U:=U*10;
      until True;
    end; // NPosW

    procedure SLTitle(const U: double; const VarName, VarLab: string);
    var
      i,IExpt: integer;
      st, st1: string;
    begin
       st := 'Ramo e Folhas para '+VarName;
       if VarLab <> '' then st := st + ' - ' + VarLab;
       Result.Add(st);
       st := 'Para leitura, multiplique valor por '+FloatToStrF(U,ffGeneral,9,4);
       st1 := '. Exemplo: 1   2 representa ';
       IExpt := IntFN(Log10(U));
       if IExpt >= 0 then
         st1 := st1 + FloatToStrF(12*U,ffGeneral,9,4)
       else
         if IExpt = -1 then
           st1 := st1+'1.2'
         else
           begin
           st1 := st1+'0,';
           IExpt := Abs(IExpt)-2;
           if IExpt <> 0 then
             for i := 1 to IExpt do st1 := st1 + '0';
           st1 := st1 + '12'
           end;
       st := st + st1;
       Result.Add(st)
    end;

    function OutlYP(ifrom,ito: Integer; HI: boolean): string;
    { Imprime valores discrepantes
      ifrom: a partir do ?ndice
      ito: at? o ?ndice
      HI: True se valores s?o superiores ao valor adjacente superior, false se s?o
        inferiores ao valor adjacente inferior
    }
    var
      i: Integer;
    begin
      if HI then Result:='SUP: ' else Result:='INF: ';
      for i:=ifrom to ito do
        begin
        Result:=Result+IntToStr(WI[i]);
        if i<ito then Result:=Result+', '
        end

    end;

    function DepthP(var P1,R: Integer; P2,C,IAdh,H: Integer;var MYet: boolean): string;
    { Obt?m e imprime as profundidades
       P1, P2: Ponteiros para WI e W. Na entrada P1=P2, apontam para o primeiro valor
               ainda n?o impresso. Na sa?da, PT1 aponta para o primeiro valor da proxima
               linha. P2 n?o ? alterado
       C: Maior valor da linha atual (positiva) ou o menor valor acima da linha atual
          (negativa)
       IAdh: aponta para o maior valor adjacente em W e WI
       H: Maior valor que est? sendo mostrado
       R: Na entrada, posto  em rela??o ao limite inferior. Na sa?da ? atualizado para
          incluir a contagem para a linha atual
       MYet: Flag, setado para True quando a mediana j? foi processada.
    }
    var
      i,PTZ,PTX,Depth,LeFont: Integer;
      Pula: boolean;
    begin
      PTX:=P1;
      Pula:=False;
      for i:=PTX to IAdh do
        begin
        P1:=i;
        if (WI[i] > C) or ((C>=0) and (WI[i]=C)) then
          begin
          Pula:=True;
          Break;
          end;
        end;
      if not Pula then  // esgotou o loop sem quebra
        P1:=IAdh+1
      else
        if (C=0) then
          if (HI<=0) then
            P1:=IAdh+1
          else
            begin
            PTZ:=P1;
            while (PTZ <= W.Len) and (W[PTZ]>=0) do        // <<<<< excede o numero de elementos !!!!
              Inc(PTZ);
            P1:=PTZ;
            while (PTZ <= W.Len) and (W[PTZ]>0) do
              Inc(PTZ);
            Inc(P1,IntFN((PTZ-P1)/2))
            end;
      LeFont:=P1-P2;
      Inc(R,LeFont);
      // Onde est? a mediana?
      if MYet then
        Depth:=N-(R-LeFont)    // caso 1: mediana j? passou
      else
        begin
        if FEquals(R,N/2) then
          MYet:=True         // caso 2: mediana esta entre
        else
          if R>=(N+1)/2 then
            begin              // caso 3: mediana esta na linha atual
            Result:='('+IntToStr(LeFont)+')';
            MYet:=True;
            Exit
            end;
        Depth:=Rank       // caso 4: mediana ainda n?o chegou
        end;
      Result:=IntToStr(Depth)
    end;

    function StemP(St,LW: Integer; NNow: boolean): string;
    { Calcula e imprime o ramo
      Na entrada:
        St: limite interno (mais proximo de zero) da linha atual
        LinW: n?mero de poss?veis d?gitos folhas diferentes
        NNow: True, se a linha atual ? negativa
    }
    const
      Ch5: array[1..5] of Char=('*','T','F','S','.');
    var
      NStem,LefDig: Integer;
    begin
      NStem:=St div 10;
      LefDig:=Abs(St-NStem*10);
      // Quantos d?gitos por linha?
      Result:='';
      if LW=2 then
        begin
        if NStem=0 then             // caso 1: dois d?gitos por linha, 5 linhas por ramo
          begin
          if NNow then              // + ou -0
            Result:='-'
          else
            Result:='+'
          end;
        Result:=Result+IntToStr(NStem)+Ch5[LefDig div 2+1];
        end
      else
        if LW=5 then
          begin
          if NStem=0 then           // caso 2: 5 d?gitos poss?veis/ linha; 2 linhas/ramo
            if NNow then
              Result:=Result+'-'
            else
              Result:=Result+'+';
          Result:=Result+IntToStr(NStem);
          if LefDig < 5 then
            Result:=Result+'*'
          else
            Result:=Result+'.';
          end
        else                        // caso 3: 10 d?gitos / linha, 1 linha / ramo
          if (NStem<>0) or (not NNow) then
            Result:=Result+IntToStr(NStem)
          else
            Result:=Result+'-0';
      Result:=Result+' '
    end;

begin
  Result := TStringList.Create;
  // Constr?i um dispositivo para cada coluna
  for j:= 1 to xCol.Len do
    begin
    W:=CopyCol(xCol[j]);
    N:=W.Len;
    W.QuickSort(True);

    // Obt?m valores resumo
    YInfo(Med,LHing,HHing,Step,AdjL,AdjH,IAdjL,IAdjH);

    { Se os valores adjacentes sao iguais, ou se o usuario deseja,
      torna os valores adjacentes iguais aos extremos}
    if FEquals(AdjH,AdjL) or Xtrems then
      begin  // obtem escala para os extremos. Pode construir um display ruim
      IAdjH:=N; IAdjL:=1;
      AdjL:=W[1]; AdjH:=W[N];
      end;
    // Encontra n?mero ?timo de linhas para exibi??o

    NLMax:=IntFN(10*Log10(IAdjH-IAdjL+1));

    // Mesmo que AdjL=AdjH, produz um display
    if FEquals(AdjH, AdjL) then
      begin
      AdjH:=AdjL+1;
      NLMax:=1
      end;

    NPosW(AdjH,AdjL,NLMax,NLins,True,Fract,Unt,NPW);
    { Re-escala tudo de acordo com a unidade. Daqui em diante tudo ? n?mero inteiro
      e os dados est?o na forma SS...SL(.). Observe que IntFN ajusta para EPS para
      o arredondamento correto. Pense nos valores como Inteiro*10^Unt
      convertidos para inteiro na forma S...SL
    }
    // Para contar valores <0, =0 e >0
    WI:=TwsLIVec.Create(N);
    for I:=1 to N do
      WI[i]:=IntFN(W[I]/Unt);
    if not FEquals(Fract,10) then
      begin
      // Se todas as folhas s?o iguais a 0, formato ser? uma linha por ramo
      Pula := False;
      for i:=IAdjL to IAdjH do
        if ((WI[i] Mod 10)<>0) then
          begin
          Pula:=True;
          Break
          end;
      if not Pula then
        begin
        Fract:=10;
        NPW:=Fract*Unt;
        NLins := IntFN(AdjH/NPW)-IntFN(AdjL/NPW)+1;
        if (AdjH*AdjL < 0) or FEquals(AdjH,0) then Inc(NLins)
        end
      end;
    Low:=WI[IAdjL]; HI:=WI[IAdjH];
    // Comprimento da linha agora ? fract
    LinWid:=IntFN(fract);
    // Inicia o processo de impress?o do dispositivo
    SLTitle(Unt,Struct.Col[xCol[j]].Name,Struct.Col[xCol[j]].Lab);

    // Imprime valores discrepantes (abaixo de AdjL), se existirem
    Rank:=IAdjL-1;
    if IAdjL <> 1 then
      Result.Add(OutlYP(1,Rank,False));

    { Inicializa??es para a parte principal do display
      Cut = corte da linha.
        Primeiro n?mero da pr?xima linha dos ramos positivos
        ?ltimo n?mero da linha atual dos ramos negativos
      L4: extremo interno (pr?ximo de zero) da linha atual
      NegNow: flag para os ramos: true s?o negativos, False caso contr?rio
      MedYet: flag para mediana: False  at? a mediana, True depois
      K1, K2, K3 s?o ponteiros em Y() para profundidades, impress?o, zeros
      I1: conta os espa?os utilizados na linha
      P5: conta as folhas naquela linha para a profundidade
      L9: valor coberto por uma linha
      Rank: conta a ordem, L2 manuten??o do d?gito da folha
    }
    Cut:=Floor((1+EPS)*Low/LinWid)*LinWid;
    NegNow:=True;
    Stem:=Cut;
    if Low>=0 then
      begin
      NegNow:=False;
      Stem:=Cut-LinWid
      end;
    MedYet:=False;
    { Dois ponteiros ser?o utilizados. PT1conta os primeiros para as profundidades,
      PT2 segue para imprimir as folhas. Ambos s?o inicializados no mesmo ponto }
    PT1:=IAdjL;
    PT2:=PT1;
    { Loop principal para esgotar todas as linhas do display
      Cut - Primeiro n?mero na pr?xima linha de ramos positivos ou ?ltimo n?mero
            da linha atual de ramos negativos
      Stem - limite interno (pr?ximo de zero) da linha atual
      SpacCnt - Conta os espa?os utilizados na linha
    }
    Result.Add('INICIO');
    for I := 1 to NLins do
      begin
      Cut:=Cut+LinWid;
      if ((Stem<>0) or not NegNow) then
        Inc(Stem,LinWid)
      else
        NegNow:=False;
      SpaCnt:=0;
      { Encontra e imprime as profundidades. Observe que Cut se comporta
        diferentemente para ramos + e -
      }
      st:=DepthP(PT1,Rank,PT2,Cut,IAdjh,HI,MedYet);
      // Imprime label do ramo
      st:=st+'    '+StemP(Stem,LinWid,NegNow);
      // Imprime as folhas
      while PT2<PT1 do
        begin
        Leaf:=Abs(WI[PT2]-IntFN(Stem/10)*10);
        st:=st+FloatToStr(Leaf);
        Inc(Spacnt);
        if Spacnt>=PltWid then  // Terminou espa?o?
          begin
          st:=st+'*';
          PT2:=PT1
          end
        else
          Inc(PT2)
        end;
      Result.Add(st)
      end; // for i
    // Informa que diagrama terminou para aquela variavel
    Result.Add('FIM');
    // Imprime valores maiores que o valor adjacente, se existirem
    if (PT1<=N) then
      Result.Add(OutlYP(PT1,N,True));
    W.Free;
    WI.Free
    end; // for j - para cada vari?vel
end; // StemAndLeaf

function TwsDataSet.IndexMat(CIdx: TwsLIVec): TwsGeneral;
{ Objetivo
    Obtem uma matriz com os possiveis ?ndices das combina??es das vari?veis indicadas.
    Delineada primariamente para impress?o de sa?das de conjuntos de dados onde s?o especificadas
    as vari?veis (do tipo fator) que estarao em colunas
  Par?metros
    CIdx: ?ndices das vari?veis que estar?o nas colunas. Os ?ndices que retornar?o na matriz
          depender?o destes ?ndices de colunas
}

  function SizeGroup(ii: Integer; v: TwsLIVec): Integer;
  var
    jj: Integer;
  begin
  Result:=1;
  for jj:=ii to v.Len do
    Result:=Result*v[jj]
  end;

var
  i,j,n : Integer;
  niv,sg: TwsLIVec;
begin
  // numero de niveis de cada fator
  niv:=TwsLIVec.Create(CIdx.Len);
  // tamanho de cada grupo para gerar os indices
  sg:=TwsLIVec.Create(CIdx.Len);
  n:=1;
  for i:=1 to CIdx.Len do
    begin
    niv[i]:=TwsFactor(Struct.Col[CIdx[i]]).Levels;
    n:=n*niv[i]
    end;
  for i:= 2 to CIdx.Len do
    sg[i-1]:=SizeGroup(i,niv);
  // ultimo grupo sempre sera de tamanho 1
  sg[sg.len]:=1;
  Result:=TwsGeneral.Create(n,CIdx.Len);
  for i:=1 to n do
    for j:=1 to CIdx.Len do
      Result[i,j]:=GL1(i,niv[j],sg[j])-1;

end; // IndexMat

{ ============================ TwsDataSetCol =============================== }

Constructor TwsDataSetCol.Create(Const aName,aLab: String; fs: byte);
{ Objetivo
    Cria um objeto descritor de coluna de conjunto de dados
  Par?metros
    aName: nome da coluna (vari?vel)
    aLab: r?tulo da coluna
    fs: largura de impress?o
  M?todos chamados
    Create herdado
  Campos alterados
    Name
    Size
}
var s: String;
Begin
  Inherited Create;
  FVersion := '1.0';
  s := aName;
  Name := GetValidID(aName);

  if s <> Name then
     lab   := alab + ' - ' + s
  else
     lab := aLab;

  size := fs;

  FHeaderFont := TFont.Create;
  FHeaderFont.Name := 'Arial';
  FHeaderFont.Size := 10;
  FHeaderFont.Style := [fsBold];

  FDataFont := TFont.Create;
  FDataFont.Name := 'Arial';
  FDataFont.Size := 10;
End;

Destructor TwsDataSetCol.Destroy;
{ Objetivo
    Libera espa?o ocupado opr conjunto de dados
  M?todos chamados
    Destroy herdado
  Campos liberados
    FLabel
}
Begin
  Inherited Destroy;
  DisposeStr(FLabel);
  FHeaderFont.Free;
  FDataFont.Free;
End; { TwsDataSetCol.Destroy }

procedure TwsDataSetCol.ToXML(Buffer: TStrings; Ident: Integer);
var s: String;
begin
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<Name>' + FName + '</Name>');
  Buffer.Add(s + '<Type>' + getColTypeAsString + '</Type>');
  Buffer.Add(s + '<Size>' + IntToStr(FSize) + '</Size>');
  if (FLabel <> nil) and (FLabel^ <> '') then
     Buffer.Add(s + '<Label>' + XML_EncodeChars(FLabel^, False) + '</Label>');
end;

Procedure TwsDataSetCol.SetName(Const Value: String);
{ Objetivo
    Atribui nome a conjunto de dados
  Par?metros
    Value: nome do conjunto
  M?todos chamados
    Nenhum
  Campos alterados
    FName
}
var i: Integer;
Begin
  If SysUtilsEx.IsValidIdent(Value) Then
     FName := Value
  Else
     FName := GetValidId(Value);
End;

Function TwsDataSetCol.RLabel: String;
{ Objetivo
    Faz leitura do r?tulo da matriz
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
 Begin
   rlabel := flabel^;
 End; { TwsDataSetCol.RLabel }

Procedure TwsDataSetCol.WLabel(Const alabel: String);
{ Objetivo
    Atribui r?tulo ao conjunto de dados
  M?todos chamados
    Nenhum
  Campos alterados
    FLabel
}
Begin
  flabel := newstr(alabel);
End; { TwsDataSetCol.WLabel }

Procedure TwsDataSetCol.LoadFromStream(Reader:TReader);
{ Objetivo
    Recupera parte da descri??o de coluna do disco
  Par?metros
    Reader: objeto para leitura
  M?todos chamados
    Nenhum
  Campos alterados
    FVersion
    Name
    Lab
    Size
}
Begin
  With Reader Do
    Begin
    FVersion := ReadString;
    Name := ReadString;
    Lab  := ReadString;
    Size := ReadInteger;
    End;
End; { TwsDataSetCol.LoadFromStream }

Procedure TwsDataSetCol.SaveToStream(Writer: TWriter);
{ Objetivo
    Escreve parte do objeto descritor no disco
  Par?metros
    Writer: objeto de escrita
  M?todos chamados
    Nenhum
}
Begin
  With Writer Do
    Begin
    WriteInteger(Integer(ColType));
    WriteString(FVersion);
    WriteString(Name);
    WriteString(Lab);
    WriteInteger(Size);
    End;
End; { TwsDataSetCol.SaveToStream }

Function TwsDataSetCol.getColTypeAsString: String;
{ Objetivo
    Retorna tipo de coluna na forma de string
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Begin
  Case ColType Of
    dtNumeric  : Result := 'Num?rica';
    dtQuant    : Result := 'Fator Quantitativo';
    dtQualit   : Result := 'Fator Qualitativo';
    dtQualitOrd: Result := 'Fator Ordenado';
    End; { Case }
End; { TwsDataSetCol.getColTypeAsString }


{ ============================== TwsNumeric ============================== }

Constructor TwsNumeric.Create(Const aName,aLab: String; fs: byte=15; fp: byte=8);
{ Objetivo
    Cria objeto descritor para coluna num?rica
  Par?metros
    aName: nome da coluna
    aLab: r?tulo para coluna
    fs: largura de impress?o
    fp: precis?o de impress?o (d?gitos significativos)
  M?todos chamados
    Create herdado
  Campos alterados
    FPrecision
    ColType
}
Begin
  Inherited Create(aname, alab, fs);
  Precision := fp;
  ColType := dtNumeric;
End;

Procedure TwsNumeric.SetPrecision(APrecision: Byte);
{ Objetivo
    Atribui precis?o a coluna num?rica
  Par?metros
    APrecision: precis?o desejada. Se for maior que 15, a precis?o ser? 15.
  M?todos chamados
    Nenhum
  Campos alterados
    FPrecision
}
Begin
  If APRecision > 15 Then FPrecision := 15 Else FPrecision := APrecision;
End;

Procedure TwsNumeric.LoadFromStream(Reader:TReader);
{ Objetivo
    Recupera parte do objeto descritor do disco
  Par?metros
    Reader: objeto de leitura
  M?todos chamados
    LoadFromStream herdado
  Campos alterados
    FPrecision
}
Begin
  Inherited LoadFromStream(Reader);
  FPrecision := Reader.ReadInteger;
End; { TwsNumeric.LoadFromStream }

Procedure TwsNumeric.SaveToStream(Writer:TWriter);
{ Objetivo
    Grava parte do objeto nio disco
  Par?metros
    Writer: objeto de escrita
  M?todos chamados
    SaveTostream herdado
  Campos alterados
}
Begin
  Inherited SaveToStream(Writer);
  Writer.WriteInteger(FPrecision);
End; { TwsNumeric.SaveToStream }

constructor TwsDataSet.CreateNumeric(aName: String; nRows, nCols: integer);
var i: Integer;
begin
  inherited Create(nRows, 0);

  FMatType := mtDataSet;
  Name := aName;
  SetColIdentName('');

  { Cria o objeto struct, liga o dataset com o struct, e associa cols com CName }
  FStruct := TwsDSStructure.Create;
  FStruct.DataSet := Self;
  FCName.Free;
  FCName := FStruct.Cols;

  for i := 1 to nCols do
    FStruct.AddNumeric('C' + intToStr(i), '');

  self.Fill(0);
end;

// Retorna uma instancia lida de uma secao de um arquivo do tipo ini
class function TwsDataSet.Load(Ini: TMemIniFile; const Section: string): TwsDataSet;
var i, j, r, c: Integer;
    s, s1, s2: string;
    v: TwsVec;
    SL: TStrings;
begin
  with Ini do
    begin
    Result := TwsDataSet.Create(readString(Section, 'Name', 'SemNome'));

    r := readInteger(Section, 'Rows', 0);
    c := readInteger(Section, 'Cols', 0);

    // Colunas
    for i := 1 to c do
      begin
      s := toString(i);
      j := readInteger(Section, 'ColType' + s, 0);
      s1 := readString (Section, 'ColName' + s, 'C' + toString(i));
      s2 := readString (Section, 'ColLab' + s, '');
      case TwsEnumDataType(j) of
        dtNumeric: result.FStruct.AddNumeric(s1, s2);
        // ...
        end
      end;

    // linhas
    SL := TStringList.Create();
    for i := 1 to r do
      begin
      v := TwsDFVec.Create(c);
      result.MAdd(v);

      s := readString(Section, 'Row' + toString(i), '');
      Split(s, SL, ['|']);

      v.Name := SL[0];
      for j := 1 to SL.Count-1 do
        v[j] := strToFloatDef(SL[j], 0);
      end;
    SL.Free();
    end;
end;

// Salva a instancia em uma secao de um arquivo do tipo ini
// Por enquanto s? salva colunas num?ricas
procedure TwsDataSet.Save(Ini: TMemIniFile; const Section: string);
var i, j: Integer;
    s: string;
    v: TwsVec;
begin
  with Ini do
    begin
    writeString (Section, 'Name', FName);
    writeInteger(Section, 'Rows', FnRows);
    writeInteger(Section, 'Cols', FnCols);

    // Colunas
    for i := 1 to FnCols do
      begin
      s := toString(i);
      writeInteger(Section, 'ColType' + s, ord(FStruct.Col[i].ColType));
      writeString (Section, 'ColName' + s, FStruct.Col[i].Name);
      end;

    // linhas
    for i := 1 to FnRows do
      begin
      v := Row[i];
      s := v.Name;
      for j := 1 to FnCols do
        begin
        s := s + '|' + toString(v[j]);
        writeString(Section, 'Row' + toString(i), s);
        end;
      end;
    end;
end;

{$ifdef MSXML}
procedure TwsDataSet.fromXML_LoadStruct(no: IXMLDomNode);
var i: Integer;
begin
  FStruct.fromXML(no);
  FNCols := no.childNodes.length - 1{identVar};
end;
{$endif MSXML}

type
  TCompareDateFunc = function (const d1, d2: TDateTime): TValueRelationship;

function TwsDataSet.FindDate(const D: TDateTime; inCol: integer; DateTime: boolean; var Index: Integer): Boolean;
var L, H, I, C: Integer;
    CompareFunc: TCompareDateFunc;
begin
  if DateTime then
     CompareFunc := DateUtils.CompareDateTime
  else
     CompareFunc := DateUtils.CompareDate;

  result := false;
  L := 1;
  H := FnRows;
  while L <= H do
    begin
    I := (L + H) shr 1;
    C := CompareFunc(getDT(I, inCol), D);
    if C < 0 then
       L := I + 1
    else
       begin
       H := I - 1;
       if C = 0 then result := true;
       end;
    end;
  Index := L;
end;

function TwsDataSet.m_ColCount(): integer;
begin
  result := self.FNCols;
end;

function TwsDataSet.m_getAsFloat(i, j: integer): double;
begin
  result := self.Get(i, j);
end;

function TwsDataSet.m_getAsInteger(i, j: integer): integer;
begin
  result := self.GetInt(i, j);
end;

function TwsDataSet.m_getAsString(i, j: integer): string;
begin
  result := self.GetTrimStr(i, j);
end;

function TwsDataSet.m_IsMissValue(i, j: integer; out x: double): boolean;
begin
  result := self.IsMissValue(i, j, x);
end;

function TwsDataSet.m_RowCount: integer;
begin
  result := self.FNRows;
end;

procedure TwsDataSet.m_setAsFloat(i, j: integer; value: double);
begin
  self.Data[i, j] := Value;
end;

procedure TwsDataSet.m_setAsInteger(i, j, value: integer);
begin
  self.Data[i, j] := value;
end;

procedure TwsDataSet.m_setAsString(i, j: integer; value: string);
begin
  self.AsString[i, j] := value;
end;
{ TwsFactor }

Constructor TwsFactor.Create(Const AName,ALab: String; fs: Byte=15);
{ Objetivo
    Participa da cria??o de objeto do tipo fator. N?o pode ser chamado diretamente.
  Par?metros
    AName: nome da coluna
    ALab: r?tulo da coluna
    fs: largura de impress?o
  M?todos chamados
    Create herdado
  Campos alterados
    LevelNames
    Contr
    FLevelType
}
Begin
  Inherited Create(aName, aLab, fs);
  LevelNames := TStringList.Create;
  FContr := Nil;
  FLevelType := tfFixed;
End; { TwsFactor.Create }

Destructor TwsFactor.Destroy;
{ Objetivo
    Libera parte do espa?o ocupado por objeto descritor
  M?todos chamados
    Destroy herdado
  Campos liberados
    LevelNames
}
Begin
  Inherited Destroy;
  LevelNames.Free;
End; { TwsFactor.Destroy }

procedure TwsFactor.SetContr(const Value: TwsGeneral);
begin
  if (FContr <> nil) and (FContr <> Value) then FContr.Free;
  FContr := Value;
end;

procedure TwsFactor.ClearContr;
begin
  if FContrType=ctUser then
    begin
    FContr.Free;
    FContr:=nil
  end
end;

procedure TwsFactor.ToXML(Buffer: TStrings; Ident: Integer);
var s: String;
begin
  inherited;
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<ContrType>' + getContrTypeAsString + '</ContrType>');
  Buffer.Add(s + '<LevelType>' + getLevelTypeAsString + '</LevelType>');
  Buffer.Add(s + '<Levels>');
    Inc(Ident, 2);
    StringsToXML(FLevels, Buffer, Ident);
    Dec(Ident, 2);
  Buffer.Add(s + '</Levels>');
end;

procedure TwsFactor.RemoveInvalidLevels();
var sl: TStringList;
    i, k: Integer;
begin
  if FDataSet <> nil then
     begin
     sl := TStringList.Create;
     sl.Duplicates := dupIgnore;

     k := Struct.IndexOf(FName);

     // Obt?m todos os n?veis que aparecem nos dados
     for i := 1 to FDataSet.nRows do
       sl.Add(FDataSet.AsTrimString[i, k]);

     // Remove os n?veis que n?o aparecem
     for i := FLevels.Count-1 downto 0 do
       if sl.IndexOf(FLevels[i]) = -1 then
          RemoveLevel(i);
     end;
end;

procedure TwsFactor.RemoveLevel(index: Integer);
begin
  FLevels.Delete(Index);
end;

Function TwsFactor.GetLevels;
{ Objetivo
    Retorna o n?mero de n?veis
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Begin
  Result := FLevels.Count;
End;

Function TwsFactor.KillLevels: Boolean;
{ Objetivo
    Libera espa?o ocupado pel?a lista com nomes dos n?veis. Retorna true se elimina??o foi
    feita sem problemas.
  M?todos chamados
    Nenhum
  Campos alterados
    RevelNames
}
Begin
  Try
    LevelNames.Free;
    LevelNames := TStringList.Create;
    Result := True;
  Except
    Result := False;
  End;
End;

Procedure TwsFactor.LoadFromStream(Reader:TReader);
{ Objetivo
    Carrega parte do objeto descritor do disco
  Par?metros
    Reader: objeto de leitura
  M?todos chamados
    LoadFromStream herdado
  Campos alterados
    Contr
    LevelNames
    ContrType
    LevelType
}
Var
    Erro  :Integer;
Begin
  Inherited LoadFromStream(Reader);
  FreeAndNil(FContr);

  With Reader Do
    Begin
    ReadListBegin;
    LevelNames.Clear;
    While Not EndOfList Do AddLevel(ReadString);
    ReadListEnd;
    ContrType := TwsEnumContrType(ReadInteger);
    LevelType := TwsEnumFType(ReadInteger);
    if ReadBoolean then
       begin
       FContr := TwsGeneral.Create(0,0);
       FContr.InternalLoad2(Reader);
       FContr.InternalLoad3(Reader);
       end;
    End;
End; { TwsFactor.LoadFromStream }

Procedure TwsFactor.SaveToStream(Writer:TWriter);
{ Objetivo
    Grava parte do objeto descritor em disco
  Par?metros
    Writer: objeto de escrita
  M?todos chamados
    SaveToStream herdado
  Campos alterados
    Nenhum
}
Var
    i  :Integer;
Begin
  Inherited SaveToStream(Writer);
  With Writer Do
    Begin
     WriteListBegin;
     For i := 0 To LevelNames.Count-1 Do WriteString(LevelNames[i]);
     WriteListEnd;
     WriteInteger(Integer(ContrType));
     WriteInteger(Integer(LevelType));
     WriteBoolean(FContr <> nil);
     If FContr <> Nil Then
        begin
        FContr.InternalSave2(Writer);
        FContr.InternalSave3(Writer);
        end;
    End;
End; { TwsFactor.SaveToStream }

Function  TwsFactor.LevelToIndex(Const N: String): Integer;
{ Objetivo
     Retorna ?ndice do nome do n?vel especificado. Baseado em 0.
  Par?metros
     N: nome do n?vel
  M?todos chamados
     IndexOf
  Campos alterados
}
Begin
  Result := LevelNames.IndexOf(SysUtilsEx.AllTrim(N));
End; { TwsFactor.LevelToIndex }

{Se o level j? existe, n?o adiciona e retorna seu ?ndice na lista}
Function TwsFactor.AddLevel(Level: String): Integer;
{ Objetivo
    Se o nome do n?vel ainda n?o existe, insere-o na lista. Retorna o ?ndice do nome na lista.
  Par?metros
    Level: nome do n?vel
  M?todos chamados
    IndexOf
    Add
  Campos alterados
    Size
}
Begin
  Level := SysUtilsEx.AllTrim(Level);
  if Level = '' then Level := '-';
  Result := LevelNames.IndexOf(Level);
  If Result = -1 Then
     begin
     Result := LevelNames.Add(Level);
     Size := Math.Max(Size, Length(Level) + 3);
     end
End;

Function TwsFactor.getLevelTypeAsString: String;
{ Objetivo
    Retorna tipo de n?vel como string
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Begin
  Case FLevelType Of
    tfRandom : Result := 'Aleat?rio';
    tfFixed  : Result := 'Fixo';
    End; { Case }
End; { TwsFactor.getLevelTypeAsString }

Function TwsFactor.getContrTypeAsString:String;
{ Objetivo
    Retorna tipo de contraste como string
  M?todos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Begin
  Case ContrType Of
    ctHelm  : Result := 'Helmert';
    ctPolOrt: Result := 'Polinomios Ortogonais';
    ctUser  : Result := 'Usuario';
    ctDev   : Result := 'Desvio';
    ctExper : Result := 'Experimento';
    ctBin   : Result := 'Binario';
    End; { Case }
End; { TwsFactor.getContrTypeAsString }

procedure TwsFactor.setContrTypeAsString(const Value: String);
begin
  if CompareText(Value, 'Helmert') = 0               then FContrType := ctHelm   else
  if CompareText(Value, 'Polinomios Ortogonais') = 0 then FContrType := ctPolOrt else
  if CompareText(Value, 'Usuario') = 0               then FContrType := ctUser   else
  if CompareText(Value, 'Desvio') = 0                then FContrType := ctDev    else
  if CompareText(Value, 'Experimento') = 0           then FContrType := ctExper  else
  if CompareText(Value, 'Binario') = 0               then FContrType := ctBin;
end;

{ =============================== TwsQualitative ============================ }

Constructor TwsQualitative.Create(Const AName,ALab: String; fs: Byte=15);
{ Objetivo
    Cria objeto descritor de uma vari?vel do tipo fator qualitativo
  Par?metros
    AName: nome da coluna
    ALab: r?tulo da coluna
    fs: largura de impress?o
  M?todos chamados
    Create herdado
  Campos alterados
    ColType
    ContrType
}
Begin
  Inherited Create(aname,alab,fs);
  ColType   := dtQualit;
  ContrType := ctHelm;
End; { TwsQualitative.Create }

Procedure TwsQualitative.LoadFromStream(Reader:TReader);
{ Objetivo
    Carrega parte do objeto descritor do disco
  Par?metros
    Reader: objeto de leitura
  M?todos chamados
    LoadFromStream herdado
  Campos alterados
    Nenhum
}
Begin
  Inherited LoadFromStream(Reader);
End; { TwsQualitative.LoadFromStream }

Procedure TwsQualitative.SaveToStream(Writer:TWriter);
{ Objetivo
    Grava parte do objeto descritor no disco
  Par?metros
    Writer: objeto de escrita
  M?todos chamados
    SaveFromStream herdado
  Campos alterados
    Nenhum
}
Begin
  Inherited SaveToStream(Writer);
End; { TwsQualitative.SaveToStream }

{ ============================== TwsOrdered ============================== }

Constructor TwsOrdered.Create(Const AName,ALab: String; fs: Byte=15);
{ Objetivo
    Cria objeto descritor de uma vari?vel do tipo fator qualitativo ordenado
  Par?metros
    AName: nome da coluna
    ALab: r?tulo da coluna
    fs: largura de impress?o
  M?todos chamados
    Create herdado
  Campos alterados
    ColType
    ContrType
}
Begin
  Inherited Create(aname,alab,fs);
  ColType   := dtQualitOrd;
  ContrType := ctHelm;
  End; { TwsOrdered.Create }

Procedure TwsOrdered.LoadFromStream(Reader:TReader);
{ Objetivo
    Carrega parte do objeto descritor do disco
  Par?metros
    Reader: objeto de leitura
  M?todos chamados
    LoadFromStream herdado
  Campos alterados
    Nenhum
}
Begin
  Inherited LoadFromStream(Reader);
End; { TwsOrdered.LoadFromStream }

Procedure TwsOrdered.SaveToStream(Writer:TWriter);
{ Objetivo
    Grava parte do objeto descritor no disco
  Par?metros
    Writer: objeto de escrita
  M?todos chamados
    SaveFromStream herdado
  Campos alterados
    Nenhum
}
Begin
  Inherited SaveToStream(Writer);
End; { TwsOrdered.SaveToStream }

{ ============================= TwsQuantitative ============================= }

Constructor TwsQuantitative.Create(Const AName,ALab: String; fs: Byte=15);
{ Objetivo
    Cria objeto descritor de uma vari?vel do tipo fator quantitativo
  Par?metros
    AName: nome da coluna
    ALab: r?tulo da coluna
    fs: largura de impress?o
  M?todos chamados
    Create herdado
  Campos alterados
    ColType
    ContrType
    FLevel
}
Begin
  Inherited Create(aname,alab,fs);
  ColType   := dtQuant;
  FLevel    := TwsDFVec.Create(0); {Valores dos n?veis}
  ContrType := ctPolOrt;
End; { TwsQuantitative.Create }

Destructor TwsQuantitative.Destroy;
{ Objetivo
    Libera espa?o ocupado pelo objeto
  M?todos chamados
    Destroy herdado
  Campos alterados
    FLevel
}
Begin
  FLevel.Free;
  inherited Destroy
End;

procedure TwsQuantitative.RemoveLevel(index: Integer);
begin
  inherited;
  FLevel.Delete(index+1, 1);
end;

Function TwsQuantitative.KillLevels: Boolean;
{ Objetivo
    Libera espa?o ocupado pel?a lista com nomes dos n?veis. Retorna true se elimina??o foi
    feita sem problemas.
  M?todos chamados
    Nenhum
  Campos alterados
    RevelNames
}
Begin
  inherited KillLevels;
  Try
    FLevel.Free;
    FLevel := TwsDFVec.Create(0);
    Result := True;
  Except
    Result := False;
  End;
End;

procedure TwsQuantitative.ToXML(Buffer: TStrings; Ident: Integer);
var s: String;
begin
  inherited;
  s := StringOfChar(' ', Ident);
  Buffer.Add(s + '<LevelsValue>');
  Inc(Ident, 2);
    FLevel.ToXML(Buffer, Ident);
  Dec(Ident, 2);
  Buffer.Add(s + '</LevelsValue>');
end;

Procedure TwsQuantitative.LoadFromStream(Reader: TReader);
{ Objetivo
    Carrega parte do objeto descritor do disco
  Par?metros
    Reader: objeto de leitura
  M?todos chamados
    LoadFromStream herdado
  Campos alterados
    FLevel
}
Begin
  Inherited LoadFromStream(Reader);
  With Reader Do
    Begin
    FLevel.Free;
    FLevel := TwsDFVec(TwsVec.VFromStream(Reader));
    End;
End; { TwsQuantitative.LoadFromStream }

Procedure TwsQuantitative.SaveToStream(Writer: TWriter);
{ Objetivo
    Grava parte do objeto descritor no disco
  Par?metros
    Writer: objeto de escrita
  M?todos chamados
    SaveFromStream
    SaveFromStream herdado
  Campos alterados
    Nenhum
}
Begin
  Inherited SaveToStream(Writer);
  FLevel.SaveToStream(Writer);
End; {TwsQuantitative.SaveToStream }

Function  TwsQuantitative.AddLevel(Level: String): Integer;
{ Objetivo
    Insere n?vel na lista de n?veis
  Par?metros
     Level: nome do n?vel
  M?todos chamados
    AddLevel herdado
    Add
  Campos alterados
    FLevel
}
var aux: Integer;
Begin
  aux := LevelNames.Count;
  Result := Inherited AddLevel(Level);
  If aux <> LevelNames.Count Then FLevel.Add(Result);
End;

Procedure TwsQuantitative.SetLevelValue(Const Level: String; Const Value: Double);
{ Objetivo
    Atribui valor para n?vel especificado
  Par?metros
    Level: nome do n?vel
    Value: valor para o n?vel
  M?todos chamados
    IndexOf
  Campos alterados
    FLevel
}
var i: Integer;
Begin
  i := LevelNames.IndexOf(SysUtilsEx.AllTrim(Level));
  If i <> -1 Then
     FLevel[i + 1] := Value
  Else
     Raise Exception.CreateFmt('N?vel <%s> desconhecido', [Level]);
End;

Function TwsQuantitative.GetLevelValue(Const Level: String): Double;
{ Objetivo
    Recupera valor do n?vel quantitativo
  Par?metros
    Level: nome do n?vel
  M?todos chamados
    IndexOf
  Campos alterados
    Nenhum
}
var i: Integer;
Begin
  i := LevelNames.IndexOf(SysUtilsEx.AllTrim(Level));
  If i <> -1 Then
     Result := FLevel[i + 1]
  Else
     Raise Exception.CreateFmt('N?vel <%s> desconhecido', [Level]);
End;

{ ============================= TwsOrderedDataSet ========================== }

Constructor TwsOrderedDataSet.Create(aName: String; aMaxRows, aBasedCol: Integer; Ascend: Boolean);
{ Objetivo
    Cria classe para inser??o ordenada de linhas
  Par?metros
    aMaxRows: N?mero m?ximo de linhas a inserir
    aBasedCols: Coluna na qual a ordena??o ser? baseada
  M?todos chamados
    Create herdado
  Campos modificados
    MaxRows
    BasedCol
    FAscendent
}
begin
  Inherited Create(aName);
  MaxRows := aMaxRows;
  BasedCol := aBasedCol;
  FAscendent := Ascend;
end;

procedure TwsOrderedDataSet.MAdd(L: TwsVec);
{ Objetivo
    Insere uma linha num conjunto de forma ordenada e respeitando n?mero m?ximo de linhas.
    Sobrep?e MAdd herdado
  Par?metros
    L: Linha a ser inserida
  M?todos chamados
    LocateAtCol
    MInsert
    MDelete
}
var Pos: Integer;
    b  : Boolean;
begin
  if NRows < MaxRows then
     begin
     LocateAtCol(L[BasedCol],BasedCol,Pos,FAscendent);
     MInsert(Pos+1,L);
     end
  else
     begin
     if FAscendent then
        b := (L[BasedCol] < Get(nRows, BasedCol))
     else
        b := (L[BasedCol] >= Get(nRows, BasedCol));

     if b then
        begin
        MDelete(FnRows);
        LocateAtCol(L[BasedCol],BasedCol,Pos,FAscendent);
        MInsert(Pos+1,L);
        end;
     end;
end; // MAdd

{*********************************** rotinas **********************************}

Function CopyDescCol(Const Col: TwsDataSetCol): TwsDataSetCol;
Var Erro : Word;
    i    : Integer;
Begin
  Case Col.ColType Of
    dtNumeric:
      Begin
      Result := TwsNumeric.Create(Col.Name, Col.Lab);
      TwsNumeric(Result).Precision := TwsNumeric(Col).Precision;
      End;

    dtQuant:
      Begin
      Result := TwsQuantitative.Create(Col.Name, Col.Lab);
      TwsQuantitative(Result).LevelValues.Free;
      TwsQuantitative(Result).LevelValues := TwsDFVec(TwsQuantitative(Col).LevelValues.Clone);
      End;

    dtQualit:
      Result := TwsQualitative.Create(Col.Name, Col.Lab);

    dtQualitOrd:
      Result := TwsOrdered.Create(Col.Name, Col.Lab);
    End; { Case Obj.ColType }

  If TwsFactor(Result).ColType <> dtNumeric Then
     Begin
     For i := 0 To TwsFactor(Col).LevelNames.Count - 1 Do
       TwsFactor(Result).LevelNames.Add(TwsFactor(Col).LevelNames.Strings[i]);

     TwsFactor(Result).LevelType := TwsFactor(Col).LevelType;

     TwsFactor(Result).ContrType := TwsFactor(Col).ContrType;

     If TwsFactor(Col).Contr <> Nil Then
        TwsFactor(Col).Contr.Copy(mtGeneral, TwsMatrix(TwsFactor(Result).FContr));
     End;
  Result.Size := Col.Size;
End; { CopyDescCol }

function FromText(var TFile: TextFile; ChLin: Char = ';'; ChEndSec: Char = '/'): TwsMatrix;
{ Objetivo
    Retorna uma matriz a partir de um arquivo texto. Pode ser chamada na seq??ncia para ler
    um conjunto qualquer de matrizes gravadas no mesmo arquivo. Cada se??o ? definida por
    uma palavra chave. Todas as se??es dever?o ser encerradas pelo caracter delimitador de
    se??o. As se??es s?o:
      COM=         para definir coment?rios              opcional
      TIPO=        para definir o tipo da matriz         obrigat?rio
                   Os tipos predefinidos s?o (caracteres m?nimos): GE, SI, DI, TR, VA, TO
      NOMELinhas=  para definir os nomes de linhas       opcional
      NOMEColunas= para definir nomes de colunas         opcional
      A matriz em si se constitui numa se??o em separado e dever? se seguir a essas
      defini??es com o nome seguido por um sinal de igual e as linhas separadas pelo
      caracter separador de linhas.

  TFile   : Arquivo texto de onde ser?o lidas as matrizes
  ChLin   : Caracter que indica o final de uma linha da matriz. O caracter v?rgula n?o
            poder? ser utilizado como separador de linha uma vez que ? um separador das
            partes inteira e decimal dos elementos.
  ChEndSec: Caracter separador de se??es

  Exemplos:

  COMENT=Matriz geral/
  Tipo=Geral/
  NomeCol= c1 c2 c3/
  NomeLin=l1 l2 l3/
A=1 2 3;
  3 4 5;
  5 6 7/

  COMENT=Matriz sim?trica/
  Tipo=Simetrica/
B=1;
  2 2;
  3 3 3/

  COMENT=Matriz diagonal/
  Tipo=Diagonal/
D=1 2 3/

  COMENT=Matriz triangular
  Tipo=Triangular/
T=1;
  2 2;
  3 3 3/

  COMENT=Matriz de Vandermonde/
  Tipo=Vandermonde
V= 1 2 3/

  COMENT=Matriz de Toeplitz/
  Tipo=Toeplitz/
O=1 2 3 4 5/

  Observa??es
    Os elementos que comp?em cada se??o dever?o ser separados pelos caracteres
     [#9,#10,#13,' ','\', '"','(',')']
  Retorno
    Retorna a matriz do tipo especificado. Se n?o houver nenhuma matriz para ler ou se
    encontrar a palavra chave FIM, retorna nil
}
  const
    NKey              = 5;
    NType             = 6;
    DelChar: TCharSet = [#9,#10,#13,' ','\', '"','(',')'];
    ChName='=';
  type
    str5=string[5];
    str2=string[2];
  const
    KeyArr : array[1..NKey] of str5=('TIPO', 'NOMEC', 'NOMEL', 'COMEN','FIM');
    TypeArr: array[1..NType] of str2=('GE', 'SI', 'DI', 'TR', 'VA', 'TO');
  var
    AType        : TwsEnumMatType;
    EndMat       : Boolean;
    Key,NameCol,
    NameLin      : TwsCVec;
    Key1         : string;
    ncar         : Integer;

  function IndKey(P: string): Byte;
  var
    j: Byte;
    Q: str5;
  begin
    Result := 0; j := 0;
    { Fazer uma funcao em CVec que copie um pedaco da cadeia }
    Q := System.Copy(P,1,5);
    repeat
      Inc(j);
      if (CompareText(KeyArr[j], Q) = 0) then Result := j
    until (Result > 0) or (j = NKey);
  end; { IndKey }

  function IndType(const P: string): byte;
  var
    j: Byte;
    Q: str5;
  begin
    Result := 0; j := 0;
    Q:=System.Copy(P,1,2);
    repeat
      Inc(j);
      if (CompareText(TypeArr[j],Q)=0) then Result:=j
    until (Result>0) or (j=NType);
  end; { IndType }

begin { FromText }
  EndMat := False;
  NameLin:=nil;   { Nomes das linhas }
  NameCol:=nil;   { Nomes das colunas }
  AType:=mtGeneral;
  while not EndMat do
    begin
    Key := TwsCVec.Create;
    Key.GetUntil(TFile,ChEndSec,[#10,#13]);
{    Key.LTrim([#13,#10,#9,' ']);         Amauri e Roger 06/10/97 - GetUntil ja faz isto }
    ncar := 1;
    Key1:=Key.StrUntilChar([ChName,ChEndSec,#0],ncar);
    Key1:= wsGLib.AllTrim(Key1,[' ',#9]);
    inc(ncar);
    case IndKey(Key1) of
      1: begin{ Definicao de tipo e inicializacao da matriz}
           Key1 := Key.StrUntilChar([ChEndSec,#0],ncar);
           case IndType(Key1) of
             1: AType := mtGeneral;
             2: AType := mtSymmetric;
             3: AType := mtDiagonal;
             4: AType := mtTriangular;
             5: AType := mtVandermonde;
             6: AType := mtToeplitz;
             else
               AType := mtGeneral;
           end; { case }
         end;
      2: begin                          { Le nomes de colunas }
           NameCol := wsCVec.GetUntilChar(Key, [ChEndSec,#0],ncar);
         end;
      3: begin                          { Le nomes de Linhas }
           NameLin := wsCVec.GetUntilChar(Key,[ChEndSec,#0],ncar);
         end;
      4:; { Comentarios. Nao faz nada }
      5: begin                          { Nao retorna nada, quando encontra a palavra FIM }
           Result := nil;
           EndMat := True
         end;
      else
        begin          { Nao encontrou nenhuma chave. Constroi a matriz especificada Key }
        Result := StrToMat(NameLin,NameCol,Key,AType,ChLin,ChEndSec);
        EndMat:=True
        end;
    end; { case }
    Key.Free;
    end; { while }
end; { FromText }

{ ############################### function VecToDiag #############################}
{ Cria uma matriz diagonal com os valores de L na diagonal }
{  Esta fun??o analisa um vetor L e gera uma matriz diagonal cujos componentes da
  diagonal principal s?o os elementos do vetor L e os demais elementos s?o nulos.
     Par?metros:
       L : um vetor do tipo TwsVec no qual seus elementos ser?o a diagonal principal
        de uma matriz Diagonal.  }

function VecToDiag(L: TwsVec): TwsDiagonal;
{ Objetivo
    Constr?i uma matriz diagonal a opartir de um vetor
  Par?metros
    L: vetor cujos elementos compor?o a diagonal da matriz
}
var
  i: Integer;
begin
  Result := TwsDiagonal.Create(0);
  with Result do
    begin
    FList.Add(L);
    CName:=TStringList.Create;
    NRows:=L.Len;
    NCols:=L.Len;
    for i:=1 to NCols do
      CName.Add('Col'+IntToStr(i))
    end
end; { VecToDiag }

(*
procedure SVD(A: TwsGeneral; var V: TwsGeneral; var D: TwsDiagonal; VMat:Boolean;
  var r, ErrCode: Word);
{ Obtem a decomposicao por valores singulares. Em D retornam os valores
  singulares. Se A e a matriz original, em A retornam os autovetores de AA'
  enquanto que em V os autovetores de A'A. Em r retorna o posto numerico de
  A
  Algoritmo do livro Golub & Reinsch }
Label
  1, 2, 3, 4;
const
  Toler = 1.0E-31; { Conferir esta tolerancia }
var
  W, E, Indx: TwsVec;
  i, j, k, l, l1, n, its, m: Integer;
  xeps, c, f, g, h, s, x, y, z: Double;

function Pythag(a, b: Double): Double;
var
  at, bt: Double;
begin
  at := Abs(a);
  bt := Abs(b);
  if at > bt then
    Pythag := at*Sqrt(1.0 + Sqr(bt/at))
  else
    if bt = 0.0 then
      Pythag := 0.0
    else
      Pythag := bt*Sqrt(1.0 + Sqr(at/bt))
end; { Pythag }

{ =========== Inicio de SingValueDecomp ===============}
begin
  ErrCode := 0;
  m := A.Count;
  n := A.NCols;
  E := VecCreate(n);
  W := VecCreate(n);
  { Reducao de Householder a forma bidiagonal }
  g := 0;
  x := 0;
  i := 1;
  repeat
    l := Succ(i);
    E[i] := g;
    s := 0;
    for j := i to m do
      s := s + A[j,i]*A[j,i];
    if s < Toler then
      g := 0
    else begin
      f := A[i,i];
      g := -Sign(Sqrt(s), f);
      h := f*g - s;
      A[i,i] := f - g;
      for j := l to n do begin
        s := 0;
        for k := i to m do
          s := s + A[k,i]*A[k,j];
        f := s / h;
        for k := i to m do
          A[k,j] := A[k,j] + f*A[k,i]
      end { j }
    end; { s }
    W[i] := g;
    s := 0;
    for j := l to n do s := s + A[i,j]*A[i,j];
    if s < Toler then
      g := 0
    else begin
      f := A[i,i+1];
      g := -Sign(Sqrt(s), f);
      h := f*g - s;
      A[i,i+1] := f - g;
      for j := l to n do
        E[j] := A[i,j] / h;
      for j := l to m do begin
        s := 0;
        for k := l to n do
          s := s + A[j,k]*A[i,k];
        for k := l to n do
          A[j,k] := A[j,k] + s*E[k];
      end; { j }
    end; { s }
    x := MaxF(x, Abs(W[i]) + Abs(E[i]));
    Inc(i);
  until i > n;

  { Acumulacao das transf. a direita }
  if VMat then begin
    i := n;
    V := TwsGeneral.Create(n, n);
    repeat
      if g <> 0 then begin
        h := A[i,i+1]*g;
        for j := l to n do
          V[j, i] := A[i,j]/h;
        for j := l to n do begin
          s := 0;
          for k := l to n do
            s := s + A[i,k]*V[k, j];
          for k := l to n do
            V[k,j] := V[k,j] + s*V[k,i]
        end; { j }
      end; { g }
      for j := l to n do begin
        V[i, j] := 0;
        V[j, i] := 0;
      end; { j }
      V[i, i] := 1;
      g := E[i];
      l := i;
      Dec(i);
    until i = 0;
  end;

  { Acumulacao das transformacoes a esquerda }
  i := n;
  repeat
    l := Succ(i);
    g := W[i];
    for j := l to n do
      A[i,j] := 0;
    if g <> 0 then begin
      h := A[i,i]*g;
      for j := l to n do begin
        s := 0;
        for k := l to m do
          s := s + A[k,i]*A[k,j];
        f := s/h;
        for k := i to m do
          A[k,j] := A[k,j] + f*A[k,i]
      end; { j }
      for j := i to m do
        A[j,i] := A[j,i]/g
    end { g }
    else
      for j := i to m do A[j, i] := 0;
    A[i,i] := A[i,i] + 1.0;
    Dec(i);
  until i = 0;

  { Diagonaliza forma bidiagonal }
  k := n;
  xeps := eps*x;
  repeat
    its := 0;
  1: { Testa para divisao }
    l := k;
    repeat
      if Abs(E[l]) <= xeps then GoTo 3;            { Testa Convergencia }
      if Abs(W[l-1]) <= xeps then GoTo 2;          { Cancelamento }
      Dec(l);
    until l = 0;

  { Cancelamento de E[l] se l > 1}
  2: { Cancelamento }
    c := 0;
    s := 1;
    l1 := Pred(l);
    for i := l to k do begin
      f := s*E[i];
      E[i] := c*E[i];
      if Abs(f) <= xeps then GoTo 3;
      g := W[i];
      h := Pythag(f, g);
      W[i] := h;
      c := g/h;
      s := -f/h;
      for j := 1 to m do begin
        y := A[j,l1];
        z := A[j,i];
        A[j,l1] := y*c + z*s;
        A[j,i] := -y*s + z*c;
      end; { j }
    end; { i }

  3: { TestaConvergencia }
    z := W[k];
    if l = k then GoTo 4;
    if its = 30 then begin
      E.Free;
      W.Free;
      ErrCode := NIterMax;
      Exit
    end;
    Inc(its);

    { Shift pelo menor 2x2 inferior }
    x := W[l];
    y := W[k-1];
    g := E[k-1];
    h := E[k];
    f := ((y-z)*(y+z) + (g-h)*(g+h))/(2*h*y);
    g := Pythag(f, 1);
    f := ((x-z)*(x+z) + h*(y/(f + Sign(g, f)) - h))/x;

    { Proxima transformacao QR }
    c := 1;
    s := 1;
    i := Succ(l);
    repeat
      g := E[i];
      y := W[i];
      h := s*g;
      g := c*g;
      z := Pythag(f, h);
      E[i-1] := z;
      c := f/z;
      s := h/z;
      f := x*c + g*s;
      g := -x*s + g*c;
      h := y*s;
      y := y*c;
      if VMat then
        for j := 1 to n do begin
          x := V[j,i-1];
          z := V[j,i];
          V[j,i-1] := x*c + z*s;
          V[j,i] := -x*s + z*c;
        end; { j }
      z := Pythag(f, h);
      W[i-1] := z;
      if z <> 0 then begin
        c := f/z;
        s := h/z
      end;
      f := c*g + s*y;
      x := -s*g + c*y;
      for j := 1 to m do begin
        y := A[j,i-1];
        z := A[j,i];
        A[j,i-1] := y*c + z*s;
        A[j,i] := -y*s + z*c
      end; { j }
      Inc(i);
    until i > k;
    E[l] := 0;
    E[k] := f;
    W[k] := x;

    GoTo 1; { Testa para divisao }

  4: { Convergencia }
    if z < 0 then begin
      W[k] := -z;
      if VMat then
        for j := 1 to n do
          V[j,k] := -V[j,k]
    end; { z }
    Dec(k);
  until k = 0;
  E.Free;

  { Ordena os valores singulares }
  Indx := W.QuickIndx(False);

  { Encontra o pseudoposto }
  r := n;
  while (W[r] < xeps) and (r > 0) do Dec(r);

  { Faz ordenacao correspondente de V e de U }
  if VMat then V.SortCol(Indx);
  A.SortCol(Indx);
  Indx.Free;
  D := VecToDiag(W);
end; { SVD }
*)

procedure EigenProd(H,R: TwsSymmetric; out EVec: TwsGeneral; out EVal: TwsDiagonal;
  out Rank: Integer; eps: Double = 1.0e-8);
{ Objetivo
    Obt?m os autovalores e autovetores de Inv(B)*A. Utiliza o fator de Cholesky
  Par?metros
    H, R: Matrizes para obten??o dos autovalores e autovetores
    EVec: Retorna a matriz dos autovetores
    EVal: Retorna uma matriz diagonal com os autovalores
    r: Para os autovalores ordenados, retorna o ?ndice do ?ltimo autovalor n?o
       nulo.
    eps: Precis?o para obten??o de r
}
var
  IChFac: TwsTriangular;
  A     : TwsGeneral;
  X     : TwsSymmetric;
  Err   : Word;
begin
  IChFac:=TwsTriangular(R.CholeskyFat(Rank,True)); // Fatora a matriz R
  if Rank=R.NCols then                             // Se IChFac for inversivel
    begin
    IChFac.Inv(Err);                               // Inverte o fator triangular
    if Err=0 then
      begin
      X:=TwsSymmetric(H.TranspMul10(IChFac,Err));  // Produto IChFac*H*(IChFac)'
      if Err=0 then
        begin
        X.Copy(mtGeneral,TwsMatrix(A));          // Transforma a matriz sim?trica numa geral
        X.Free;
        A.Eigen(EVal,True,Rank,True,eps,Err);    // Autovalores e autovetores
        EVec:=TwsGeneral(IChFac.TranspMul2(A,Err)); // Autovetores de IChFac*H*(IChFac)'
        IChFac.Free; A.Free;
        end
      end
    end;
end;

function EigenProp(V: TwsDiagonal; Rank: Integer): TwsGeneral;
{ Objetivo
    Recebe uma matriz diagonal com os autovalores ordenados e retorna uma matriz com os
      autovalores, suas diferen?as em rela??o aos subsequentes e as propor??es que
      representam em rela??o ? soma.
  Par?metros
    V: matriz diagonal dos autovalores
    Rank: n?mero de autovalores n?o nulos de V
}
var
  aux, aux1: Double;
  i: Integer;
begin
  aux:=0;
  for i:=1 to Rank do
    aux:=aux+V[i,i];  // Soma dos autovalores de Inv(R)*H
  Result:=TwsGeneral.Create(V.NRows,4);
  Result.Name:='Autoval';
  Result.MLab:='Autovalores de Inv(R)*H';
  Result.ColName[1]:='Auto_Valor'; Result.ColName[2]:='Diferenca';
  Result.ColName[3]:='Proporcao'; Result.ColName[4]:='Prop_Acum';
  aux1:=0;
  for i:=1 to Result.NRows do
    begin
    Result.RowName[i]:=IntToStr(i);
    Result[i,1]:=V[i,i];            // autovalor
    if i=1 then                    // diferenca somente a partir do segundo
      Result[i,2]:=wscMissValue
    else
      Result[i,2]:=Result[i-1,1]-Result[i,1];  // diferenca em relacao ao anterior
    Result[i,3]:=V[i,i]/aux;
    aux1:=aux1+Result[i,3];            // proporcao do autovalor
    Result[i,4]:=aux1                  // proporcao acumulada
    end;
end;


procedure House1(Build: Boolean; A: TwsGeneral; p, l, m, ia: Integer; var up: Double;
  C: TwsGeneral; ncb, ncp: Integer; var ErrCode: Word);
{ Constroi e aplica transformacao de Householder, operando colunas de A e colunas de C.
  Build: Se True constroi e aplica em C; False somente aplica em C.
  A: Matriz sobre a qual a transformacao e construida
  p:
  l:
  m:
  ia:
  up:
  C: Matriz sobre a qual a transformacao e aplicada
  ncb:
  ncp:
  }
  procedure Make;
  var
    j,i : Integer;
    clinv, cl, vp, sm: Double;
  begin
    vp := A[p,ia];
    cl := Abs(vp);
    for i := l to m do
      cl := MaxF(Abs(A[i,ia]), cl);
    if cl <> 0 then begin
      clinv := 1/cl;
      sm := Sqr(vp*clinv);
      for i := l to m do
        sm := sm + Sqr(A[i,ia]*clinv);
      cl := cl*Sqrt(sm);
      if vp > 0 then cl := -cl;
      up := vp-cl;
      A[p,ia]:=cl
    end
  end; { Make }

  procedure Apply;
  var
    i, j, k, ncb1: Integer;
    b, s: Double;
  begin
    b := up*A[p,ia];
    if b < 0 then begin
      b := 1/b;
      ncb1 := ncb-1;
      for j := 1 to ncp do begin
        k := ncb1+j;
        s := C[p,k]*up;
        for i := l to m do
          s := s + C[i,k]*A[i,ia];
          if s <> 0 then begin
            s := s*b;
            C[p,k]:=C[p,k]+s*up;
            for i := l to m do
              C[i,k] := C[i,k] + s*A[i,ia]
          end;
      end { For }
    end
  end; { Apply }

begin { Corpo principal }
  ErrCode := 0;
  if (p>0) and (p<l) and (l<=m) then begin
    if Build then begin
      Make;
      if ncp <> 0 then Apply
    end
    else
      if ncp <> 0 then Apply
  end
  else
    ErrCode := NHouse
end; { House1 }

procedure House2(Build: Boolean; A: TwsGeneral; p, l, m, ia: Integer; var up: Double;
  C: TwsGeneral; ncb, ncp: Integer; var ErrCode: Word);
{ Constroi e aplica transformacao de Householder, operando linhas de A e linhas de C.
  Build: Se True constroi e aplica em C; False somente aplica em C.
  A: Matriz sobre a qual a transformacao e construida
  p:
  l:
  m:
  ia:
  up:
  C: Matriz sobre a qual a transformacao e aplicada
  ncb:
  ncp:
  }
  procedure Make;
  var
    j,i : Integer;
    clinv, cl, vp, sm: Double;
  begin
    vp := A[ia,p];
    cl := Abs(vp);
    for i := l to m do
      cl := MaxF(Abs(A[i,ia]), cl);
    if cl <> 0 then begin
      clinv := 1/cl;
      sm := Sqr(vp*clinv);
      for i := l to m do
        sm := sm + Sqr(A[ia,i]*clinv);
      cl := cl*Sqrt(sm);
      if vp > 0 then cl := -cl;
      up := vp-cl;
      A[p,ia]:=cl
    end
  end; { Make }

  procedure Apply;
  var
    i, j, k, ncb1: Integer;
    b, s: Double;
  begin
    b := up*A[p,ia];
    if b < 0 then begin
      b := 1/b;
      ncb1 := ncb-1;
      for j := 1 to ncp do begin
        k := ncb1+j;
        s := C[k,p]*up;
        for i := l to m do
          s := s + C[k,i]*A[ia,i];
          if s <> 0 then begin
            s := s*b;
            C[k,p]:=C[k,p]+s*up;
            for i := l to m do
              C[k,i] := C[k,i] + s*A[ia,i]
          end
      end { For }
    end
  end; { Apply }

begin { Corpo principal }
  ErrCode := 0;
  if (p>0) and (p<l) and (l<=m) then begin
    if Build then begin
      Make;
      if ncp <> 0 then Apply
    end
    else
      if ncp <> 0 then Apply
  end
  else
    ErrCode := NHouse
end; { House2 }

procedure House3(Build: Boolean; A: TwsGeneral; p, l, m, ia: Integer; var up: Double;
  C: TwsGeneral; ncb, ncp: Integer; var ErrCode: Word);
{ Constroi e aplica transformacao de Householder, operando linhas de A e colunas de C.
  Build: Se True constroi e aplica em C; False somente aplica em C.
  A: Matriz sobre a qual a transformacao e construida
  p:
  l:
  m:
  ia:
  up:
  C: Matriz sobre a qual a transformacao e aplicada
  ncb:
  ncp:
  }
  procedure Make;
  var
    j,i: Integer;
    clinv, cl, vp, sm: Double;
  begin
    vp := A[ia,p];
    cl := Abs(vp);
    for i := l to m do
      cl := MaxF(Abs(A[i,ia]), cl);
    if cl <> 0 then begin
      clinv := 1/cl;
      sm := Sqr(vp*clinv);
      for i := l to m do
        sm := sm + Sqr(A[ia,i]*clinv);
      cl := cl*Sqrt(sm);
      if vp > 0 then cl := -cl;
      up := vp-cl;
      A[p,ia]:=cl
    end
  end; { Make }

  procedure Apply;
  var
    i, j, k, ncb1: Integer;
    b, s: Double;
  begin
    b := up*A[p,ia];
    if b < 0 then begin
      b := 1/b;
      ncb1 := ncb-1;
      for j := 1 to ncp do begin
        k := ncb1+j;
        s := C[p,k]*up;
        for i := l to m do
          s := s + C[i,k]*A[ia,i];
          if s <> 0 then begin
            s := s*b;
            C[p,k]:=C[p,k]+s*up;
            for i := l to m do
              C[i,k] := C[i,k] + s*A[ia,i]
          end
      end { For }
    end
  end; { Apply }

begin { Corpo principal }
  ErrCode := 0;
  if (p>0) and (p<l) and (l<=m) then begin
    if Build then begin
      Make;
      if ncp <> 0 then Apply
    end
    else
      if ncp <> 0 then Apply
  end
  else
    ErrCode := NHouse
end; { House3 }

function MultToA(var A: TwsMatrix; B: TwsMatrix; var ErrCode: Word): TwsMatrix;
{  Objetivo
     Faz o produto entre duas matrizes com o resultado substituindo a primeira. Neste algoritmo
     apenas o dimensionamento de um vetor ? necess?rio para composi??o do produto. Cada linha
     do produto que vai sendo obtida vai substituindo a linha correspondente da primeira martiz.
   Par?metros
     A: Matriz que pr?-multiplica. Na sa?da cont?m o resultado do produto
     B: Matriz que p?s-multiplica. N?o ? alterada pelo produto
     ErrCode: C?digo de erro. Retorna 0 se o produto ? realizado sem problemas
   Observa??es
     Faz produto especial quando ambas as matrizes sao diagonais. Se A for Vandermonde ou Toeplitz
     nao faz o produto.
}
var
  i,j,k: Integer;
  F,F1 : TwsVec;
  Temp : TwsMatrix;
begin
  ErrCode:=0;
  if (A.NCols <> B.NRows) then
    ErrCode:=NImprDim
  else begin { Produto especial para duas diagonais }
    if (A.MatType = mtDiagonal) and (B.MatType = mtDiagonal) then begin
      F := A.Row[1];
      F1 := B.Row[1];
      for j := 1 to A.NCols do
        F[j] := F[j]*F1[j]
    end
    else begin
      { Se A for Vandermonde ou Toeplitz o produto retorna como geral }
      if (A.MatType in [mtVandermonde, mtToeplitz]) then begin
        A.Copy(mtGeneral,Temp);
        A.Free;
        A:=Temp
      end;
      { Obtem o produto por linha }
      for i := 1 to A.NRows do begin
        F := TwsDFVec.Create(B.NCols);
        for j := 1 to B.NCols do begin
          F[j] := 0;
          for k := 1 to A.NCols do
            F[j] := F[j] + A[i, k]*B[k, j]
        end;
        {e substitui em A }
        A.MDelete(i);
        A.MInsert(i, F)
      end
    end;
    A.NCols := B.NCols;
    MultToA := A
  end
end; { MultToA }

function MatPower(A: TwsMatrix; n : Integer; var ErrCode: Word): TwsMatrix;
 { Objetivo
     Produto de uma matriz por ela mesma um n?mero especificado de vezes.
   Par?metros
     A: Matriz para o produto
     n: Pot?ncia da matriz ou n?mero de vezes que A ser? multiplicada por ela mesma
     ErrCode: C?digo de erro. retorna 0 se o produto transcorreu normalmente. O produto de A
     por ela mesma s? ? poss?vel se A for quadrada.
}
var
  l  : Integer;
  Aux: TwsMatrix;
begin
  Result:=nil;
  if A.NRows=A.NCols then
    begin
    A.Copy(A.MatType,Result);
    for l := 2 to n do
      begin
      Aux:=Result;
      Result:= A.Mult(Result,ErrCode);
      Aux.Free
      end
    end
  else
    begin
    ErrCode:=NImprDim;
    Result:=nil
    end;
end;


(*
function KroneckerToA(A, X: TwsMatrix): TwsMatrix;
{ Retorna o produto de Kronecker entre duas matrizes. }
var
  m, i, k, l, j, ly, my, ny: Integer;
  L0, L1, L2: TwsVec;
  z: Double;
begin
  my := A.NRows*X.NRows;
  ny := A.NCols*X.NCols;
  ly := 0;
  for m := 1 to A.NRows do begin
    k := A.NCols;
    L0 := VCopy(A.Row(m), 1, k);
    for i := 1 to X.NRows do begin
      Inc(ly);
      L2 := VecCreate(ny);
      L1 := X.row[i];
      k := 0;
      l := 1;
      repeat
        z := L0[l];
        for j := 1 to X.NCols do begin
          Inc(k);
          L2[k] := z*L1[j]
        end;
        Inc(l);
      until k = ny;
      if ly > A.Count then
        A.MAdd(L2)
      else begin
        A.MDelete(ly);
        A.MInsert(ly, L2)
      end { if }
    end; { for }
    L0.Free;
  end; { for }
  A.NRows := my;
  A.NCols := ny;
  A.MatType := mtGeneral;
  KroneckerToA := A
end; { KroneckerToA }

 function KroneckerToA(A, X: TwsMatrix): TwsMatrix;
{ Obtem o produto de Kronecker entre duas matrizes. O produto substitui A. }
var
  m, i, k, l, j, ly, my, ny: Integer;
  L0, L1, F: PFArray;
  Line: TFVec;
  z: Double;
begin
  my := A.NRows*X.NRows;
  ny := A.NCols*X.NCols;
  ly := 0;
  for m := 1 to A.NRows do begin
    L0 := A.Row(m).Data;
    i := 0;
    repeat
      Inc(i);
      GetMem(F, sf(ny));
      if F <> nil then begin
        L1 := X.row[i].Data;
        k := 0;
        l := 1;
        repeat
          z := L0[l];
          for j := 1 to X.NCols do begin
            Inc(k);
            F^[k] := z*L1[j]
          end;
          Inc(l);
        until k = ny;
        Line := TFVec.Create(F, ny);
        Inc(ly);
        if ly > A.Count then
          A.Insert(Line)
        else begin
          A.AtFree(ly-1);
          A.AtInsert(ly-1, Line)
        end { if }
      end
    until (i = X.NRows) or (F = nil)
  end;
  A.SetDimension(my, ny);
  A.SetType('G');
  KroneckerToA := A
end; { KroneckerToA }
*)

function StrToMat(LN,CN,P: TwsCVec; MT: TwsEnumMatType; ChLin, ChEnd: Char): TwsMatrix;
{ Objetivo
    Constroi uma matriz a partir do (objeto) string especificado.
  Par?metros
     LN   : String com os nomes de linhas. Se n?o houver, deve ser passado com valor nil
     CN   : String com os nomes das colunas. Se n?o houver, deve ser passado com valor nil
     P    : String com os valores no formato Nome=elementos
     MT   : Tipo da matriz a ser criada
     ChLin: Caracter que indica os finais de linhas da matriz
     ChEnd: Caracter que indica o final da matriz
  Exemplo 'G=1 2 3; 4 5 6; 6 7 8/'  - matriz geral
          'S=1; 2 3; 4 5 6/'        - matriz sim?trica
          'T=1; 2 3; 4 5 6/'        - matriz triangular
          'D= 1 2 3/'               - matriz mtDiagonal
          'V= 1 2 3/'               - matriz de mtVandermonde
          'O=1 2 3 4 5/'            - matriz de mtToeplitz
  Observa??o
    Procurar n?o utilizar o caracter ',' (virgula) como separador, uma vez que ele pode
    ser um caracter v?lido na especifica??o de um n?mero.
}
var
  Name   : string;
  ncar   : Integer;
  Lin    : TwsVec;
  CharLin: TwsCVec;
begin
  ncar := 1;
  { 1. Localizar = e retirar o nome da matriz;}
  Name := P.StrUntilChar(['='], ncar);
  { 2. Criar matriz vazia do tipo estabelecido em MT}
  inc(ncar);
  CharLin := wsCVec.GetUntilChar(P, [ChLin,ChEnd,#0],ncar);
  Lin := CharLin.ToFVec;
  CharLin.Free;
  case MT of
    mtGeneral    : Result := TwsGeneral.Create(0,0);
    mtSymmetric  : Result := TwsSymmetric.Create(0);
    mtDiagonal   : Result := TwsDiagonal.Create(0);
    mtTriangular : Result := TwsTriangular.Create(0);
    mtVandermonde: Result := TwsVandermonde.Create(0);
    mtToeplitz   : Result := TwsToeplitz.Create(0);
  end; { case }
  Result.Name:=Name;
  Result.MAdd(Lin);
  { 3. Enquanto nao termina o string }
  while (ncar<P.Len) do
    begin
    {Copia linha em ChLin}
    CharLin := wsCVec.GetUntilChar(P,[ChLin,ChEnd,#0],ncar);
    { Transforma o conteudo em vetor }
    Lin := CharLin.ToFVec;
    CharLin.Free;
    {Insere o vetor na matriz}
    Result.MAdd(Lin);
    end;
  { 4. Atualiza numero de colunas e linhas }
  with Result do
    case MT of
      mtGeneral,mtSymmetric,mtTriangular: NCols := Lin.Len;

      mtDiagonal,mtVandermonde:
        begin
        NRows := Lin.Len;
        NCols := NRows
        end;
      mtToeplitz:
        begin
        NRows := (Lin.Len+1) div 2;
        NCols := NRows
        end;
    end;
  if LN <> nil then Result.SetRName(LN,[' ',',','/',#9,#13,#10]);

  if CN <> nil then
    Result.SetCName(CN,[' ',',','/',#9,#13,#10])
  else
    begin
    Result.CName:=TStringList.Create;
    for ncar:=1 to Result.NCols do
      Result.CName.Add('Col'+IntToStr(ncar))
    end;

end;

function GStrMat(P: String): TwsGeneral;
{ Objetivo
    Retorna uma matriz geral a partir de um string
  Par?metros
    P    : string que contem a matriz geral
    ChDel: caracter que separa os elementos
    ChLin: caracter que separa as linhas
  Resultado
    Retorna uma matriz geral. Retorna nil se algum vetor linha tiver dimens?o diferente
}
var
  S  : string;
  i  : integer;
  j  : Cardinal;
  L  : TwsVec;
  Ok : boolean;
begin
  Result:=TwsGeneral.Create(0,0);
  j:=1;
  S:=SysUtilsEx.StrToken(P,j,[',']);
  if (S <> '') then
    begin
    L:=StrVec(S);
    Result.MAdd(L);
    i:=L.Len
    end;
  Ok:=True;
  while (S <> '') and Ok do
    begin
    S:=SysUtilsEx.StrToken(P,j,[' ']);
    if S <> '' then
      begin
      L:=StrVec(S);
      Ok:=i=L.Len;
      if ok then
        Result.MAdd(L);
      end
    end;
  if not Ok then
    begin
    Result.Free;
    Result:=nil
    end;
end;

{Rochedo} {18/04/1999}
function Statistics(A: TwsGeneral;
                    const Col: Array of byte;
                    const Stat: Array of TwsEnumStatistics): TwsGeneral;
var vC, vS: TwsLIVec;
    i: byte;
begin
  vC := TwsLIVec.Create(Length(Col));
  vS := TwsLIVec.Create(Length(Stat));
  for i := 1 to vC.Len do vC[i] := Col[i-1];
  for i := 1 to vS.Len do vS[i] := integer(Stat[i-1]);
  try
    Result := TwsGeneral(A.DescStat(vC, Vs));
  finally
    vC.Free;
    vS.Free;
  end;
end;

function AreContrast(Y: TwsGeneral; var Col: Integer): boolean;
{ Objetivo
    Retorna True se todas as colunas definem contrastes
  Par?metros
    Y: matriz cujas colunas definem contrastes
    Col: Retorna a ?ltima coluna testada. Se o retorno for False, indica a coluna que
      n?o ? contraste
}
begin
  Result:=True;
  Col:=1;
  while Result and (Col<=Y.NCols) do
    begin
    Result:=IsContrast(Y,Col);
    Inc(Col)
    end;
end;

function AreOrthogonal(Y: TwsGeneral; var ColI, ColJ: Integer): Boolean;
{ Objetivo
    Retorna True se todos os pares de colunas forem ortogonais
  Par?metros
    Y  : matriz que ter? as colunas testadas
    i,j: Se o retorno for False, retorna o par que n?o ? ortogonal
  Observa??o
    Retorna True se todas as somas dos produtos dos elementos das colunas de Y forem nulas
}
var
  s    : Double;
  i,j,k: Integer;
begin
  for i:=1 to Y.NCols-1 do
    for j:=i+1 to Y.NCols do
      begin
      s:=0;
      for k:=1 to Y.NRows do
        s:=s+Y[k,i]*Y[k,j];
      Result:=FEquals(s,0);
      ColI:=i;
      ColJ:=j;
      if not Result then Exit
      end
end;

function IsContrast(Y: TwsGeneral; k: Integer): boolean;
{ Objetivo
    Retorna True se a coluna indicada corresponde a coeficientes de um contraste.
    Os contrastes dever?o estar especificados por coluna. A coluna ? considerada como
    um conjunto de coeoficientes de contrastes se a soma for nula.
  Par?metros
    Y: matriz com os coeficientes
    k: coluna a ser verificada
}
var
  s: Double;
  i: Integer;
begin
  s:=0;
  for i:=1 to Y.NRows do
    s:=s+Y[i,k];
  Result:=FEquals(s,0)
end;

function IsOrthogonal(Y: TwsGeneral; w: TwsVec; j,k: Integer): boolean;
{ Objetivo
    Retorna True se as colunas indicadas correspondem as coeficientes de dois contrastes
    ortogonais. Os contrastes dever?o estar especificados por coluna. Duas colunas s?o
    consideradas como um conjunto de coeoficientes de contrastes ortogonais se a sua soma
    de produtos for nula.
  Par?metros
    Y: matriz com os coeficientes
    w: vetor com pesos para verifica??o da ortogonalidade. Tipicamente w cont?m o n?mero de
    repeti??es de cada m?dia envolvida no contaste
    j,k: colunas a serem verificadas
}
var
  s: Double;
  i: Integer;
begin
  s:=0;
  for i:=1 to Y.NRows do
    s:=s+Y[i,j]*Y[i,k]/w[i];
  Result:=FEquals(s,0)
end;

function TreatRand(n: Integer): TwsGeneral;
var
  u    : TwsVec;
  p,idx: TwsLIVec;
  Err  : Word;
begin
  Result:=TwsGeneral.Create(0,n);
  u:=UnifValues(n,1000,1);
  idx:=u.QuickIndx(True);
  p:=Index(1,n);
  p.SortOf(Idx,Err);
  Result.MAdd(u);
  Result.MAdd(p);
  idx.Free
end;

procedure FuncApply(fOp: TwsEnumConstFun; var C:TwsMatrix);
{ Objetivo
    Aplica uma fun??o a uma matriz
  Par?metros
    fOp: Fun??o desejada. As possibilidades s?o:
      cABS      - valor absoluto
      cEXP      - exponencial
      cAPROXIMA - Aproxima valores dentro de um limite pr?-estabelecido
      cINT      - Parte inteira do valor
      cLN       - Logaritmo neperiano
      cRAIZ     - Raiz quadrada
      cARCTAN   - Arco tangente
      cARCSEN   - Arco seno
      cARCCOS   - Arco cosseno
      cSEN      - Seno
      cCOS      - Cosseno
      cSENH     - Seno hiperb?lico
      cCOSH     - Cosseno hiperb?lico
      cTAN      - Tangente
      cLOG      - Logaritmo decimal
      cANG      - Transforma??o angular
      cLGAMA    - Logaritmo da fun??o gama
      cTGAMA    - Derivada da fun??o digama
      cFLOOR    - Maior inteiro
      cCEIL     - Menor inteiro
      cINV      - Inverso do valor
      cFRAC     - Parte fracion?ria
      cTANH     - Tangente hiperb?lica
      cAcum     - Valores acumulados

    C: Matriz onde retornam os valores da fun??o. Em C dever?o estar os valores originais
  Valores perdidos
    Fun??es aplicadas a valores impr?prios ou perdidos retornam valores perdidos
}
var
  i,j: Integer;
  x  : Double;
begin
  case fOp of
    cABS: // valor absoluto
      for i:=1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarAbs(C[i,j]);
    cEXP: // exponencial
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarExp(C[i,j]);
    cAPROXIMA: // Aproxima valores dentro de um limite pr?-estabelecido
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarFuzz(C[i,j]);
    cINT: // Parte inteira do valor
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarInt(C[i,j]);
    cLN: // Logaritmo neperiano
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarLn(C[i,j]);
    cRAIZ: // Raiz quadrada
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarSqrt(C[i,j]);
    cARCTAN: // Arco tangente
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarArcTan(C[i,j]);
    cARCSEN: // Arco seno
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarArcSin(C[i,j]);
    cARCCOS: // Arco cosseno
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarArcCos(C[i,j]);
    cSEN: // Seno
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarSin(C[i,j]);
    cCOS: // Cosseno
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarCos(C[i,j]);
    cSENH: // Seno hiperb?lico
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarSinH(C[i,j]);
    cCOSH: // Cosseno hiperb?lico
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarCosH(C[i,j]);
    cTAN: // Tangente
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarTan(C[i,j]);
    cLOG: // Logaritmo decimal
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarLog(C[i,j]);
    cANG: // Transforma??o angular
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarAng(C[i,j]);
    cLGAMA: // Logaritmo da fun??o gama
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarLnGamma(C[i,j]);
    cTGAMA: // Logaritmo da fun??o gama
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarTriGamma(C[i,j]);
    cFLOOR: // Maior inteiro
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarFloor(C[i,j]);
    cCEIL: // Menor inteiro
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarCeil(C[i,j]);
    cINV: // Inverso do valor
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarInv(C[i,j]);
    cFRAC: // Parte fracion?ria
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarFrac(C[i,j]);
    cTANH: // Tangente hiperb?lica
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarTanH(C[i,j]);
    cAcum: // Valores acumulados
      begin
      x:=0;
      for i:=1 to C.NRows do
        for j:=1 to C.NCols do
          begin
          x:=ScalarSum(x,C[i,j]);
          C[i,j]:=x
          end;
      end;
  end; // case fOp
end; // FuncApply

procedure Prod1(A, B:TwsMatrix; out C:TwsMatrix);
{ Objetivo
    Efetua o produto matricial onde ambas as matrizes s?o gerais, simetricas, mtVandermonde
    ou mtToeplitz
  Parametros
    A: Primeira matriz do produto
    B: Segunda matriz do produto
    C: Matriz resultante. Sempre sera do tipo Geral
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
}
var
  i,j,k: Integer;
  aux  : Extended;
begin
  C:=TwsGeneral.Create(A.NRows,B.NCols);
  for i:=1 to A.NRows do
    for j:=1 to B.NCols do
      begin
      aux:=0;
      for k:=1 to B.NRows do
        aux:=aux+A[i,k]*B[k,j];
      C[i,j]:=aux
      end;
end;

procedure Prod2(A:TwsMatrix; B:TwsDiagonal; PreMult:Boolean; out C:TwsGeneral);
{ Objetivo
    Efetua o produto matricial onde a primeira matriz e geral, simetrica, de
    mtVandermonde ou mtToeplitz e a outra ? mtDiagonal.
  Par?metros
    A: Primeira matriz do produto. Podera ser geral, simetrica, mtVandermonde ou mtToeplitz
    B: Segunda matriz do produto. Sempre devera ser uma matriz mtDiagonal
    PreMult: True se a matriz mtDiagonal premultiplica a outra matriz; false se posmultiplica
    C: Matriz resultante. Sempre sera do tipo Geral
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
  Retorno
    Retorno ? sempre uma matriz geral
}
var
  i,j,k: Integer;
  aux  : Extended;
begin
  if PreMult then { Reescala das linhas de A }
    begin
    C:=TwsGeneral.Create(B.NRows,A.NCols);
    for i:=1 to B.NRows do
      begin
      aux:=B[i,i];
      for j:=1 to A.NCols do
        C[i,j]:=A[i,j]*aux;
      end
    end
  else            { Reescala da coluna de A }
    begin
    C:=TwsGeneral.Create(A.NRows,B.NCols);
    for j:=1 to A.NCols do
      begin
      aux:=B[j,j];
      for i:=1 to A.NRows do
        C[i,j]:=A[i,j]*aux;
      end
    end
end;

procedure Prod3(A:TwsMatrix; B:TwsTriangular; PreMult:Boolean; out C:TwsGeneral);
{ Objetivo
    Efetua o produto matricial onde uma matriz ? geral, simetrica, de mtVandermonde ou
    mtToeplitz e a outra ? mtTriangular
  Parametros
    A: Primeira matriz do produto, do tipo geral, sim?trica, de mtVandermonde ou mtToeplitz
    B: Segunda matriz do produto, do tipo mtTriangular
    PreMult: True se a matriz mtTriangular (B) premultiplica; false se posmultiplica
    C: Matriz resultante. Sempre sera do tipo Geral
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
  Retorno
    O retorno ? sempre uma matriz geral
}
var
  i,j,k: Integer;
  aux  : Extended;
begin
  if PreMult then   // mtTriangular premultiplica
    begin
    C:=TwsGeneral.Create(B.NRows,A.NCols);
    for i:=1 to B.NRows do
      for j:=1 to A.NCols do
        begin
        aux:=0;
        for k:=1 to i do
          aux:=aux+B[i,k]*A[k,j];
        C[i,j]:=aux
        end
    end
  else             // mtTriangular posmultiplica
    begin
    C:=TwsGeneral.Create(A.NRows,B.NCols);
    for i:=1 to A.NRows do
      for j:=1 to B.NCols do
        begin
        aux:=0;
        for k:=j to A.NCols do
          aux:=aux+A[i,k]*B[k,j];
        C[i,j]:=aux
        end
    end
end;

procedure Prod4(A,B:TwsDiagonal; out C:TwsDiagonal);
{ Objetivo
    Efetua o produto matricial onde ambas matrizes s?o diagonais
  Par?metros
    A: Primeira matriz do produto. Deve ser do tipo mtDiagonal
    B: Segunda matriz do produto. Deve ser do tipo mtDiagonal
    C: Matriz resultante. Sempre sera do tipo mtDiagonal
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
}
var
  i: Integer;
begin
  C:=TwsDiagonal.Create(A.NRows);
  for i:=1 to A.NRows do
    C[i,i]:=A[i,i]*B[i,i];
end;

procedure Prod5(A: TwsDiagonal; B:TwsTriangular; PreMult:Boolean; out C:TwsTriangular);
{ Objetivo
    Efetua o produto matricial entre uma matriz mtDiagonal e outra mtTriangular
  Parametros
    A: Primeira matriz do produto. Deve ser do tipo mtDiagonal
    B: Segunda matriz do produto. Deve ser do tipo mtTriangular
    PreMult: True se a matriz mtDiagonal (A) premultiplica; false se posmultiplica
    C: Matriz resultante. Retorna uma matriz do tipo mtTriangular
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
}
var
  i,j: Integer;
  aux: Double;
begin
  C:=TwsTriangular.Create(A.NRows);
  if PreMult then  { Reescala linhas da matriz mtTriangular }
    for i:=1 to A.NRows do
      begin
      aux:=A[i,i];
      for j:=1 to i do
        C[i,j]:=B[i,j]*aux;
      end
  else             { Reescala colunas da matriz mtTriangular }
    for i:=1 to A.NRows do
      begin
      aux:=A[i,i];
      for j:=i to B.NRows do
        C[j,i]:=B[j,i]*aux;
      end
end;

procedure Prod6(A, B:TwsTriangular; out C:TwsTriangular);
{ Objetivo
    Efetua o produto matricial onde ambas as matrizes s?o triangulares
  Parametros
    A: Primeira matriz do produto. Dever ser do tipo mtTriangular
    B: Segunda matriz do produto. Deve ser do tipo mtTriangular
    C: Matriz resultante. Sempre sera do tipo mtTriangular
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
}
var
  i,j,k: Integer;
  aux  : Extended;
begin
  C:=TwsTriangular.Create(A.NRows);
  for i:=1 to A.NRows do
    for j:=1 to i do
      begin
      aux:=0;
      for k:= j to i do
        aux:=aux+A[i,k]*B[k,j];
      C[i,j]:=aux
      end;
end;

procedure ElemOp1(A,B:TwsMatrix; Op:TwsEnumTypeOp; var C:TwsGeneral);
{  Objetivo
     Opera todos os elementos das matrizes especificadas
   Par?metros
     A, B: Matrizes que ser?o operadas
     Op: Tipo de opera??o a ser executada. Os operadores v?lidos s?o:
         opSum,opSub,opDiv,opProd,opPower:
           Operadores aritmeticos soma, subtra??o, divis?o, produto e pot?ncia
          opGE,opGT,opLE,opLT,opEQ,opNE:
            Operadores para comparacao maior ou igual, maior que, menor ou igual, menor que,
            igual e diferente
          opOR,opAnd:
            Operadores l?gicos OU e E
          opMax,opMin
            M?ximo, minimo
     C: Matriz que ir? armazenar o resultado. Dever? ser criada pela rotina que chama. Na
        chamada ElemOp1(A,B,Op,C) o resultado ser? c[i,j]=a[i,j] Op b[i,j], para todos os
        ?ndices i e j.
   Observa??o
     Esta rotina N?O testa se as matrizes A, B e C t?m a mesma ordem.
}
var
  i,j: Integer;
begin
  case Op of
    opSum:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarSum(A[i,j],B[i,j]);
    opSub:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarSub(A[i,j],B[i,j]);
    opDiv:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarDiv(A[i,j],B[i,j]);
    opProd:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarProd(A[i,j],B[i,j]);
    opPower:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarPower(A[i,j],B[i,j]);
    opGE:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarGE(A[i,j],B[i,j]);
    opGT:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarGT(A[i,j],B[i,j]);
    opLE:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarLE(A[i,j],B[i,j]);
    opLT:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarLT(A[i,j],B[i,j]);
    opEQ:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarEQ(A[i,j],B[i,j]);
    opNE:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarNE(A[i,j],B[i,j]);
    opOR:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarOR(A[i,j],B[i,j]);
    opAnd:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarAnd(A[i,j],B[i,j]);
    opMax:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarMax(A[i,j],B[i,j]);
    opMin:
      for i:=1 to A.NRows do
        for j:=1 to A.NCols do
          C[i,j]:=ScalarMin(A[i,j],B[i,j]);
  end; // case
end;

procedure ElemOp2(A:TwsMatrix; v:TwsVec; Op:TwsEnumTypeOp; DFirst:Boolean; var C:TwsMatrix);
{  Objetivo
     Opera os elementos de um vetor com os elementos da mtDiagonal de uma matriz
   Par?metros
     A: Matriz cujos elementos da mtDiagonal ser?o operados
     v: Vetor cujos elementos ser?o operados com os da mtDiagonal de A
     Op: Tipo de opera??o a ser executada. Os operadores v?lidos s?o:
         opSum,opSub,opDiv,opProd,opPower:
           Operadores aritmeticos soma, subtra??o, divis?o, produto e pot?ncia
          opGE,opGT,opLE,opLT,opEQ,opNE:
            Operadores para comparacao maior ou igual, maior que, menor ou igual, menor que,
            igual e diferente
          opOR,opAnd:
            Operadores l?gicos OU e E
          opMax,opMin
            M?ximo, minimo
     DFirst: Se True ent?o a opera??o ser? a[i,i] Op v[i]; Se False, a opera??o ser?
       v[i] Op a[i,i].
     C: Matriz que ir? armazenar o resultado. Dever? ser criada pela rotina que chama
}
var
  i,n: Integer;
begin
  n := Min(A.NRows, A.NCols);
  case Op of
    opSum:
      for i:=1 to n do
        C[i,i]:=ScalarSum(A[i,i],v[i]);
    opSub:
      if DFirst then
        for i:=1 to n do
          C[i,i]:=ScalarSub(v[i],A[i,i])
      else
        for i:=1 to n do
          C[i,i]:=ScalarSub(A[i,i],v[i]);
    opDiv:
      if DFirst then
        for i:=1 to n do
          C[i,i]:=ScalarDiv(v[i],A[i,i])
      else
        for i:=1 to n do
          C[i,i]:=ScalarDiv(A[i,i],v[i]);
    opProd:
      for i:=1 to n do
        C[i,i]:=ScalarProd(A[i,i],v[i]);
    opPower:
      if DFirst then
        for i:=1 to n do
          C[i,i]:=ScalarPower(v[i],A[i,i])
      else
        for i:=1 to n do
          C[i,i]:=ScalarPower(A[i,i],v[i]);
    opGE:
      if DFirst then
        for i:=1 to n do
          C[i,i]:=ScalarGE(v[i],A[i,i])
      else
        for i:=1 to n do
          C[i,i]:=ScalarGE(A[i,i],v[i]);
    opGT:
      if DFirst then
        for i:=1 to n do
          C[i,i]:=ScalarGT(v[i],A[i,i])
      else
        for i:=1 to n do
          C[i,i]:=ScalarGT(A[i,i],v[i]);
    opLE:
      if DFirst then
        for i:=1 to n do
          C[i,i]:=ScalarLE(v[i],A[i,i])
      else
        for i:=1 to n do
          C[i,i]:=ScalarLE(A[i,i],v[i]);
    opLT:
      if DFirst then
        for i:=1 to n do
          C[i,i]:=ScalarLT(v[i],A[i,i])
      else
        for i:=1 to n do
          C[i,i]:=ScalarLT(A[i,i],v[i]);
    opEQ:
      for i:=1 to n do
        C[i,i]:=ScalarEQ(A[i,i],v[i]);
    opNE:
      for i:=1 to n do
        C[i,i]:=ScalarNE(A[i,i],v[i]);
    opOR:
      for i:=1 to n do
        C[i,i]:=ScalarOR(A[i,i],v[i]);
    opAnd:
      for i:=1 to n do
        C[i,i]:=ScalarAnd(A[i,i],v[i]);
    opMax:
      for i:=1 to n do
        C[i,i]:=ScalarMax(A[i,i],v[i]);
    opMin:
      for i:=1 to n do
        C[i,i]:=ScalarMin(A[i,i],v[i]);
  end; // case
end; // ElemOp2

procedure ElemOp3(A,B:TwsMatrix; Op:TwsEnumTypeOp; var C:TwsSymmetric);
{  Objetivo
     Opera todos os elementos das matrizes sim?tricas especificadas
   Par?metros
     A, B: Matrizes sim?tricas que ser?o operadas
     Op: Tipo de opera??o a ser executada. Os operadores v?lidos s?o:
         opSum,opSub,opDiv,opProd,opPower:
           Operadores aritmeticos soma, subtra??o, divis?o, produto e pot?ncia
          opGE,opGT,opLE,opLT,opEQ,opNE:
            Operadores para comparacao maior ou igual, maior que, menor ou igual, menor que,
            igual e diferente
          opOR,opAnd:
            Operadores l?gicos OU e E
          opMax,opMin
            M?ximo, minimo
     C: Matriz sim?trica que ir? armazenar o resultado. Dever? ser criada pela rotina que chama. Na
        chamada ElemOp1(A,B,Op,C) o resultado ser? c[i,j]=a[i,j] Op b[i,j], para os
        ?ndices i e j, i>=j.
   Observa??o
     Esta rotina N?O testa se as matrizes A, B e C t?m a mesma ordem.
}
var
  i,j: Integer;
begin
  case Op of
    opSum:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarSum(A[i,j],B[i,j]);
    opSub:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarSub(A[i,j],B[i,j]);
    opDiv:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarDiv(A[i,j],B[i,j]);
    opProd:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarProd(A[i,j],B[i,j]);
    opPower:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarPower(A[i,j],B[i,j]);
    opGE:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarGE(A[i,j],B[i,j]);
    opGT:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarGT(A[i,j],B[i,j]);
    opLE:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarLE(A[i,j],B[i,j]);
    opLT:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarLT(A[i,j],B[i,j]);
    opEQ:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarEQ(A[i,j],B[i,j]);
    opNE:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarNE(A[i,j],B[i,j]);
    opOR:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarOR(A[i,j],B[i,j]);
    opAnd:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarAnd(A[i,j],B[i,j]);
    opMax:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarMax(A[i,j],B[i,j]);
    opMin:
      for i:=1 to A.NRows do
        for j:=1 to i do
          C[i,j]:=ScalarMin(A[i,j],B[i,j]);
  end; // case
end; // ElemOp3


function InvMat(y: TwsMatrix; NewMat: Boolean; var ErrCode: Word): TwsMatrix;
{ Objetivo:
   Obt?m a inversa da matriz especificada.
 Par?metros:
   Y: Matriz a ser invertida. Como o m?todo Inv ? virtual, a chamada Y.Inv() ir? executar
      os m?todos espec?ficos de invers?o.
   NewMat: False provocar? a invers?o no local (ou na mesma matriz) e para True a provoca
           a invers?o numa c?pia de Y.
   ErrCode: C?digos que poder?o retornar do processo de invers?o. Veja m?todo Inv de
            TwsMatrix e descendentes.
 Observa??o:
   Se a inversa da matriz Y existir, ser? tal que Y.Inv(Y) = I ,onde Inv(Y) ? a matriz
  inversa de Y e I ? a matriz identidade.
}
begin
  if NewMat then
    y.Copy(y.MatType,Result)
  else
    Result := y;
  Result.Inv(ErrCode)
end;

procedure GJSolve(A, B: TwsGeneral; var ErrCode: word);
{ Objetivo:
    Resolve um sistema de equa??es lineares atrav?s do m?todo de Gauss-Jordan com
    pivoteamento total.
  Par?metros:
    A: Na entrada ? a matriz de coeficientes do sistema linear. Na sa?da, armazena a inversa
       de A.
    B: Matriz do segundo membro. Cada coluna de B especifica um novo sistema de equa??es
       lineares com a mesma matriz de coeficientes A. Na sa?da, armazena as correspondentes
       solu??es dos sistemas.
    ErrCode: C?digo de erro. Retorna 0 se o processo se encerra normalmente; caso contr?rio
       retorna NSingMat (Matriz A ? singular)
}
var
  big, dum, pivinv: Double;
  i, icol, irow, j, k, l, ll: Integer;
  IndxC, IndxR, ipiv: TwsLIVec;
  L0, L1, L2, L3, L5, L6: TwsVec;
begin
  IndxC := TwsLIVec.Create(A.NCols);
  IndxR := TwsLIVec.Create(A.NCols);
  ipiv := TwsLIVec.Create(A.NCols);
  for i := 1 to A.NCols do ipiv[i] := 0;
  for i := 1 to A.NCols do begin
    big := 0.0;
    for j := 1 to A.NRows do begin
      if ipiv[j] <> 1 then begin
        L0 := A.row[j];
        for k := 1 to A.NCols do
          if ipiv[k] = 0 then begin
            if Abs(L0[k]) >= big then begin
              big := Abs(L0[k]);
              irow := j;
              icol := k
            end
          end
          else
            if ipiv[k] > 1 then begin
              IndxC.Free;
              IndxR.Free;
              ipiv.Free;
              ErrCode:=NSingMat;
            end
      end
    end;
    ipiv[icol] := ipiv[icol] + 1;
    L0 := A.Row[irow];
    L1 := A.Row[icol];
    L2 := B.Row[irow];
    L3 := B.Row[icol];
    if irow <> icol then begin
      for l := 1 to A.NCols do begin
        dum := L0[l];
        L0[l] := L1[l];
        L1[l] := dum
      end;
      for l := 1 to B.NCols do begin
        dum := L2[l];
        L2[l] := L3[l];
        L3[l] := dum
      end
    end;
    IndxR[i] := irow;
    IndxC[i] := icol;
    if Abs(L1[icol]) = 0.0 then begin
      IndxC.Free;
      IndxR.Free;
      ipiv.Free;
      ErrCode := NSingMat;
      Exit
    end;
    pivinv := 1.0/L1[icol];
    L1[icol] := 1.0;
    for l := 1 to A.NCols do
      L1[l] := L1[l]*pivinv;
    for l := 1 to B.NCols do L3[l] := L3[l]*pivinv;
    for ll := 1 to A.NRows do
      if ll <> icol then begin
        L5 := A.Row[ll];
        dum := L5[icol];
        L5[icol] := 0.0;
        for l := 1 to A.NCols do L5[l] := L5[l] - L1[l]*dum;
        L6 := B.Row[ll];
        for l := 1 to B.NCols do L6[l] := L6[l] - L3[l]*dum
      end
  end; { for i }
  for l := A.NCols downto 1 do
    if IndxR[l] <> IndxC[l] then
      for k := 1 to A.NRows do begin
        L0 := A.Row[k];
        dum := L0[IndxR[l]];
        L0[IndxR[l]] := L0[IndxC[l]];
        L0[IndxC[l]] := dum
      end;
  IndxC.Free;
  IndxR.Free;
  ipiv.Free
end; { GJSolve }

procedure SVDSolve(var A,B: TwsGeneral);
{  Objetivo
     Dado o sistema de equa??es lineares, retorna a solu??o de m?nimos quadrados atrav?s
     da decomposi??o por valores singulares
   Par?metros
     A: Na entrada ? a matriz dos coeficientes do sistema linear. Na sa?da, retorna a
        matriz decorrente da decomposi??o A=UDV'.
     B: Na entrada possui em cada coluna o vetor do segundo membro de cada sistema linear
        Ax=b(i). Na sa?da, cada coluna cont?m a solu??o de cada sistema
}
var
  i,j,jb,Rank: Integer;
  ErrCode    : Word;
  Diag,c     : TwsVec;
  s          : Double;
  W          : TwsDiagonal;
  V          : TwsGeneral;
begin
  A.SVDcmp(W,V,Rank,ErrCode);
  if ErrCode = 0 then
    begin
    c := TwsDFVec.Create(Rank);
    Diag := W.Row[1];
    for jb := 1 to B.NCols do
      begin
      for j := 1 to Rank do
        begin
        s := 0;
        for i := 1 to A.NRows do
          s := s + A[i,j]*B[i,jb];          { U'b }
        c[j] := s/Diag[j];                  { U'b/w}
        end;
      for i := 1 to A.NCols do
        begin
        s:=0;
        for j:=1 to Rank do
          s:=s+V[i,j]*c[j];
        B[i,jb] := s
        end;
      end;
    W.Free;
    V.Free;
    c.Free
    end
end; { SVDSolve }

procedure HFTSolve(var A, B, X: TwsGeneral; var H: TwsVec; var ErrCode: word);
 { Objetivo:
    Determina a solu??o de m?nimos quadrados de um SEL atrav?s de transforma??es de
    Householder. Rotina adaptada de Lawson & Hanson.
  Par?metros:
    A: Matriz dos coeficientes do sistema. Na sa?da armazena valores que possibilitam
       a obten??o da transforma??o ortogonal
    B: Matriz com os vetores do segundo membro
    X: Matriz que retorna a solu??o do sistema
    H: Na sa?da armazena quantidades que, juntamente com aqueles de A, possibilitam a
       obten??o da transforma??o ortogonal
    X: Retorna a solu??o do sistema
}
var
  i,j,k   : Integer;
  s       : Double;
  L0,L1,L2: TwsVec;
begin
  H := TwsDFVec.Create(A.NCols);
  X := TwsGeneral.Create(A.NCols+1, B.NCols);
  for j := 1 to A.NCols do begin                             { Algoritmo HFT }
    s := H[j];
    House1(True, A, j, j+1, A.FList.Count, j, s, A, j+1, A.NCols-j, ErrCode);
    H[j] := s
  end;
  for j := 1 to A.NCols do begin                            { Algoritmo HS1 }
    s := H[j];
    House1(False, A, j, j+1, A.FList.Count, j, s, B, 1, B.NCols, ErrCode);
    H[j] := s
  end;
  L1 := B.Row[A.NCols];
  L2 := A.Row[A.NCols];
  for k := 1 to B.NCols do begin           { Resolve o sistema mtTriangular }
    L1[k] := L1[k]/L2[A.NCols];
    for i := A.NCols-1 downto 1 do begin
      L0 := A.row[i];
      s := 0;
      for j := i+1 to A.NCols do
        s := s + B[j, k]*L0[j];
      B[i, k] := (B[i, k] - s)/L0[i]
    end
  end;
  L2 := X.Row[A.NCols+1];
  for k := 1 to B.NCols do begin             { Transfere a solucao para X }
    for i := 1 to A.NCols do begin
      L0 := B.row[i];
      X[i, k] := L0[k];
      L0[k] := 0
    end;
    s := 0;
    for i := A.NCols+1 to A.FList.Count do                 { Calcula a SQ Residual }
      s := s + Sqr(B[i, k]);
    L2[k] := s
  end;
  for j := A.NCols downto 1 do begin { B armazena os residuos de cada solucao }
    s := H[j];
    House1(False, A, j, j+1, A.FList.Count, j, s, B, 1, B.NCols, ErrCode);
    H[j] := s
  end
end; { HFTSolve }


procedure HFTISolve(var A, B, X: TwsGeneral; var r: Integer; var Indx: TwsLIVec;
  var G, H: TwsVec; var ErrCode: word);
{ Objetivo:
    Determina a solu??o de m?nimos quadrados de um SEL atrav?s de transforma??es de
    Householder. A diferen?a em rela??o ao procedimento HFTSolve est? no pivoteamento par-
    cial realizado sobre as colunas de A de forma a tratar de forma mais adequada os casos
    de matrizes mal condicionadas. Rotina adaptada de Lawson& Hanson
  Par?metros:
    A: Matriz dos coeficientes do sistema. Na sa?da armazena valores que possibilitam
       a obten??o da transforma??o ortogonal.
    B: Matriz com os vetores do segundo membro.
    X: Matriz que retorna a solu??o do sistema.
    H: Na sa?da armazena quantidades que, juntamente com aqueles de A, possibilitam a
       obten??o da transforma??o ortogonal.
    X: Retorna a solu??o do sistema.
    r: Posto num?rico de A.
    Indx: Vetor que armazena as trocas de colunas realizadas em A.
    G, H: Retornam quantidades que, juntamente com o que retorna em A, possibilitam a
          obten??o da transforma??o ortogonal
    ErrCode: C?digo de erro.
}
const
  Fact: Single = 0.001;
var
  j,i, l, LDiag,
  lmax, jb: Integer;
  s: Double;
  L0, L1, L2: TwsVec;

procedure ComputeSQC;
var
  ll, ii: Integer;
begin
  lmax := j;
  for ll := j to A.NCols do begin
    H[ll] := 0;
    for ii := j to A.FList.Count do
      H[ll] := H[ll] + Sqr(A[ii, ll]);
    if H[ll] > H[LMax] then lmax := ll
  end;
  s := H[LMax]
end; { ComputeSQC }

procedure UpdateSQC;
var
  ll: Integer;
begin
  lmax := j;
  L0 := A.Row[j-1];
  for ll := j to A.NCols do begin
    H[ll] := H[ll] - Sqr(L0[ll]);
    if H[ll] > H[lmax] then lmax := ll
  end;
  if ((s + Fact*H[lmax]) - s) <= 0 then ComputeSQC
end; { ComputeSQC }

begin
  X := TwsGeneral.Create(2, B.NCols);
  Indx := TwsLIVec.Create(A.NCols);
  H := TwsDFVec.Create(A.NCols);
  G := TwsDFVec.Create(A.NCols);
  ldiag := Min(A.FList.Count, A.NCols);
  for j := 1 to ldiag do begin
    if j > 1 then UpdateSQC else ComputeSQC;
    Indx[j] := lmax;
    if Indx[j] <> j then begin      { Se necessario, troca as colunas }
      for i := 1 to A.FList.Count do begin
        L0 := A.row[i];
        s := L0[j];
        L0[j] := L0[lmax];
        L0[lmax] := s
      end;
      H[lmax] := H[j]
    end;
    s := H[j];
    House1(True, A, j, j+1, A.FList.Count, j, s, A, j+1, A.NCols-j, ErrCode);
    H[j] := s;
    s := H[j];
    House1(False, A, j, j+1, A.FList.Count, j, s, B, 1, B.NCols, ErrCode);
    H[j] := s
  end;
  j := 1;
  while (Abs(A[j, j]) > eps) and (j <= ldiag) do Inc(j);
  r := j-1;                                         { r e o pseudoposto }
  L0 := X.row[1];
  for jb := 1 to B.NCols do begin
    s := 0;
    for i := r+1 to A.FList.Count do
      s := s + Sqr(B[i, jb]);
    L0[jb] := Sqrt(s)      { Armazena as normas residuais }
  end;
  if r < A.NCols then                      { Utiliza as linhas como pivos }
    for i := r downto 1 do begin
      s := G[i];
      House2(True, A, i, r+1, A.NCols, i, s, A, 1, i-1, ErrCode);
      G[i] := s
    end;
  L0 := B.Row[r];
  for jb := 1 to B.NCols do begin       { Resolve o sistema mtTriangular }
    L0[jb] := L0[jb]/A[r, r];
    for i := r-1 downto 1 do begin
      L1 := B.row[i];
      L2 := A.row[i];
      s := 0;
      for j := i+1 to r do
        s := s + L2[j]*B[j, jb];
      L1[jb] := (L1[jb] - s)/L2[i]
    end;
    if r < A.NCols then begin                       { Completa a solucao }
      for j := r+1 to A.NCols do B[j, jb] := 0;
      for i := 1 to r do begin
        s:= G[i];
        House3(False, A, i, r+1, A.NCols, i, s, B, jb, 1, ErrCode);
        G[i] := s
      end;
    end;
    for j := ldiag downto 1 do      { Destroca as linhas, se necessario }
      if Indx[j] <> j then begin
        s := B[Indx[j], jb];
        B[Indx[j], jb] := B[j, jb];
        B[j, jb] := s;
      end;
    s := 0;
    for j := 1 to ldiag do               { Encontra a norma da solucao }
      s := s + Sqr(B[j, jb]);
    X[2, jb] := Sqrt(s)        { e armazena na segunda linha de X }
  end
end; { HFTISolv }

procedure LUSolve(A, B: TwsGeneral);
{  Objetivo
     Dado o sistema de equa??es lineares, retorna a solu??o atrav?s da decomposi??o LU
   Par?metros
     A: Na entrada ? a matriz dos coeficientes do sistema linear. Na sa?da, retorna a
        matriz L (mtTriangular inferior da fatora??o LU) na por??o mtTriangular inferior e U
        na por??o mtTriangular superior.
     B: Na entrada possui em cada coluna o vetor do segundo membro de cada sistema linear
        Ax=b(i). Na sa?da, cada coluna cont?m a solu??o de cada sistema
}
var
  j,ip,ii,i,jb: Integer;
  sum,d       : Double;
  Indx        : TwsLIVec;
  L0,L1,L2    : TwsVec;
  Erro : word;
begin
  A.LU(Indx,d,Erro);
  ii := 0;
  for jb := 1 to B.NCols do begin
    for i := 1 to A.NRows do begin
      L0 := B.row[i];
      L2 := A.row[i];
      ip := Indx[i];
      L1 := B.Row[ip];
      sum := L1[jb];
      L1[jb] := L0[jb];
      if ii <> 0 then
        for j := ii to i-1 do
          sum := sum - L2[j]*B[j, jb]
      else
        if sum <> 0.0 then ii := i;
      L0[jb] := sum
    end;
    for i := A.NRows downto 1 do begin
      L0 := B.row[i];
      L1 := A.row[i];
      sum := L0[jb];
      for j := i+1 to A.NCols do
        sum := sum - L1[j]*B[j, jb];
      L0[jb] := sum/L1[i]
    end
  end;
  indx.free;
end; { LUSolve }

function HVecVecConcat(v1,v2: TwsVec; var Erro: Word): TwsGeneral;
{ Objetivo
    Concatena horizontalmente dois vetores numa matriz de duas linhas. Responde ao operador //
  Par?metros
    v1:   Vetor colcado como primeira linha
    v2:   vetor colocado como segunda linha
    Erro: Retorna zero se ambos os vetores possuem o mesmo numero de elementos; NImprDim
          caso contrario.
}
begin
  Erro := 0;
  if v1.len = v2.len then
    begin
    Result := TwsGeneral.Create(0,v1.len);
    Result.MAdd(v1);
    Result.MAdd(v2);
    end
  else
    Erro:=NImprDim
end; {HVecVecConcat}

procedure SetColName(A: TwsMatrix; C: PChar); { Alex 09/10/97 }
{ Objetivo
    Atribui nomes de colunas a uma matriz
  Par?metros
    A: matriz para atribui??o dos nomes de colunas
    C: String (PChar) com os nomes
  Observa??o
    Os nomes de colunas dever?o ter como separadores os caracteres branco, v?rgula, ponto-e-
    v?rgula e aspas
}
var Col: TwsCVec;
begin
  Col := StringToCVec(String(C));
  A.SetCName(Col,[' ',',',';','"']);
  Col.Free;
end;

function CharToMat(p:PChar; MT : TwsEnumMatType): TwsMatrix;
{ Objetivos
    Transforma um string (PChar) numa matriz
  Par?metros
    p: string que cont?m a matriz
    MT: tipo da matriz a ser criada
  Observa??es
    Os elementos da matriz devem ser separados por espa?os, as linhas por v?rgulas e o final
    do string por ponto-e-v?rgula
}
var v: TwsCVec;
begin
  v := StringToCVec(String(p));
  Result := StrToMat(nil, nil, v, MT, ',', ';');
  v.Free;
end;

function Identity(nc: Integer): TwsGeneral;
{  Objetivo
     Gera uma matriz identidade no formato geral
   Par?metros
     nc: Ordem da matriz
}
var i: Integer;
begin
  Result := TwsGeneral(Jota(nc, nc, mtGeneral, 0));
  for i := 1 to nc do Result[i,i] := 1;
end; { Identity }

function DuncanTable(nTreat: Integer; df: TwsVec): TwsGeneral;
{ Objetivo
    Retorna uma tabela apropriada para o teste de Duncan. As taxas de erro consideradas s?o
    0.01, 0.05 e 0.1.
  Par?metros
    ntreat: N?mero de tratamentos (ou de   m?dias)
    df    : Vetor com os graus de liberdade do res?duo
  Sa?da
    Matriz geral onde cada linha corresponde a uma combina??o grau de liberdade x taxa de erro
    especificado em df e
    cada coluna a uma amplitude entre as m?dias, iniciando em 2 e indo at? ntreat
}
const
  Alpha: array[1..3] of Double = (0.05, 0.01, 0.1);
var
  i,j,k,Lin: Integer;
begin
  // Uma linha para cada probabilidade e cada GLRes
  Result:=TwsGeneral.Create(3*df.Len,nTreat+1);
  Result.Name:='Duncan';
  Result.PrintOptions.ColWidth := 10;
  Result.PrintOptions.ColPrecision := 8;
  Result.MLab:='Pontos Percentuais para o Teste de Amplitude M?ltipla de Duncan';
  Result.ColName[1]:='GL_Res';
  Result.ColName[2]:='Alfa';
  for i:=3 to nTreat+1 do
    Result.ColName[i]:='Amp_'+IntToStr(i-1);
  Lin:=0;
  for i:=1 to df.Len do
    for k:=1 to 3 do
      begin
      Inc(Lin);
      Result[Lin,1]:=df[i];
      Result[Lin,2]:=Alpha[k];
      for j:= 3 to nTreat+1 do
        Result[Lin,j] := FDuncan(Alpha[k],j-1,df[i])
      end
end; // DuncanTable

{$ifdef WINSTAT_FULL}
function TukeyTable(nTreat: Integer; df, Alpha: TwsVec): TwsGeneral;
{ Objetivo
    Retorna uma tabela apropriada para o teste de Tukey
  Par?metros
    Tuk   : Matriz com os valores de tabela de Tukey
    trt   : Vetor com o n?mero de tratamentos (ou de m?dias)
    df    : Vetor com os graus de liberdade do res?duo
    alpha: Vetor com as taxas de erro
  Sa?da
    Matriz geral onde cada linha corresponde a uma uma combina??o grau de liberdade x taxa
    de erro. O n?mero de tratamentos (ou amostras) consideradas vai de 2 a nTreat. A ordem
    da matriz, portanto, ser? (df.len*alpha.len) x (nTreat+2)
}
var
  i,j,k,Lin: Integer;
  Erro     : Word;
begin
  Result:=TwsGeneral.Create(Alpha.Len*df.Len,nTreat+1);
  Result.Name:='Tab_Tukey';
  Result.PrintOptions.ColWidth := 10;
  Result.PrintOptions.ColPrecision := 8;
  Result.MLab:='Tabela de Pontos Percentuais para Amplitude Estudentizada (Tukey)';
  Result.ColName[1]:='GL_Res';
  Result.ColName[2]:='Alfa';
  for i:=3 to nTreat+1 do
    Result.ColName[i]:='Trat'+IntToStr(i-1);
  Lin:=0;
  for i:=1 to df.Len do
    for k:=1 to Alpha.Len do
      begin
      Inc(Lin);
      Result[Lin,1]:=df[i];
      Result[Lin,2]:=Alpha[k];
      for j:= 3 to nTreat+1 do
        Result[Lin,j]:=qtrng(1-Alpha[k],df[i],j-1,Erro)
      end;
end; // TukeyTable
{$endif}
(*
function Helmert(nr: Integer): TwsGeneral;
{ Objetivo
    Obtem a matriz de Helmert. A finalidade primaria dessa matriz e proporcionar uma
    codificacao da matriz de tal forma que, se a classificacao for completa e balanceada,
    as colunas de X serao ortogonais
  Par?metros
    nr: Numero de linhas da matriz
}
var
  i, j: Integer;
begin
  Result := TwsGeneral.Create(nr, nr-1);
  with Result do
    begin
    Name:='Helmert';
    for i:=1 to NCols do
      ColName[i]:='Contr_'+IntToStr(i);
    for i := 1 to NRows do
      begin
      RowName[i]:='Nivel_'+IntToStr(i);
      for j := 1 to NCols do
        if i <= j then
          Data[i,j] := -1/j
        else
          if i = j+1 then
            Data[i,j] := 1
          else
            Data[i,j] := 0
      end
    end
end; { Helmert }
*)

function Helmert(nr: Integer): TwsGeneral;
{ Objetivo
    Obtem a matriz de Helmert. Considerando cada coluna como um contraste e cada linha
    como um n?vel, a matriz de Helmert gera contrastes da seguinte forma: para a coluna j,
    estabelece j como valor para a posi??o (i+1, j), -1 para todas as linhas i=j-1, .., 1
    e para todas as linhas i=j+1, ..., nlin. Essa forma de codificacao gera colunas ortogonais
    entre si.
  Par?metros
    nr: Numero de linhas da matriz
  Retorno
    Matriz nr x nr-1 em cujas colunas est?o os contrastes de Helmert
}
var
  i,j: Integer;
  a  : Double;
begin
  Result := TwsGeneral.Create(nr,nr-1);
  with Result do
    begin
    Name:='Helmert';
    MLab:='Matriz de contrastes de Helmert';
    for i:=1 to nRows do
      RowName[i]:='Niv_'+IntToStr(i);
    for i:=1 to nCols do
      begin
      ColName[i]:='C_'+IntToStr(i);
      j:=1;
      a:=-1/i;
      while j<=i do
        begin
        Data[j,i]:=a;
        Inc(j)
        end;
      Data[j,i]:=1;
      Inc(j);
      while j<=nRows do
        begin
        Data[j,i]:=0;
        Inc(j)
        end
      end
    end
end; { Helmert }

function LHelmert(nr: Integer): TwsGeneral;
{ Objetivo
    Obtem uma matriz de reparametriza??o para um modelo de classifica??o simples onde os
    constrastes s?o definidos como constrastes de Helmert. Cada linha define um novo par?metro.
  Par?metros
    nr: Numero de n?veis do fator A ou n?mero de tratamentos
  Retorno
    Matriz nr x nr+1 em cujas linhas (>1) est?o os contrastes de Helmert
}
var
  i,j: Integer;
  a: Double;
begin
  Result := TwsGeneral.Create(nr, nr+1);
  with Result do
    begin
    Name:='LHelmert';
    MLab:='Matriz de reparametriza??o (MCS) com constrastes de Helemert';
    PrintOptions.ColWidth:=9;
    PrintOptions.ColPrecision:=5;
    RowName[1]:='M';
    ColName[1]:='M';
    Data[1,1]:=1;
    for i:=2 to NCols do
      ColName[i]:='t_'+IntToStr(i-1);
    for i:=2 to nCols do
      Data[1,i]:=1/nr;
    for i := 2 to nRows do
      begin
      RowName[i]:='H_'+IntToStr(i-1);
      Data[i,1]:=0;
      a:=-1/(i-1);
      j:=2;
      while (j<(i+1)) do
        begin
        Data[i,j]:=a;
        Inc(j)
        end;
      Data[i,j] := 1;
      Inc(j);
      while j<=nCols do
        begin
        Data[i,j]:=0;
        Inc(j)
        end
      end
    end
end; { LHelmert }

function LIndic(nr: Integer): TwsGeneral;
{ Objetivo
    Obtem uma matriz indicadora para um modelo de classifica??o simples.
  Par?metros
    nr: Numero de n?veis do fator A ou n?mero de tratamentos
  Retorno
    Matriz nr x nr+1 com a indica??o dos efeitos
}
var
  i, j: Integer;
begin
  Result := TwsGeneral.Create(nr, nr+1);
  with Result do
    begin
    Name:='LIndic';
    MLab:='Matriz indicadora MCS';
    PrintOptions.ColWidth:=8;
    PrintOptions.ColPrecision:=4;
    ColName[1]:='M';
    Data[1,1]:=1;
    for i:=2 to NCols do
      ColName[i]:='t_'+IntToStr(i-1);
    for i := 1 to NRows do
      begin
      RowName[i]:='t_'+IntToStr(i);
      Data[i,1]:=1;
      for j := 2 to NCols do
        if j = i+1 then
          Data[i,j] := 1
        else
          Data[i,j] := 0
      end;
    end // with
end; { LIndic }

function Control(nr: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz com valores -1 na ultima linha  e as demais formando uma matriz
    identidade. Primariamente tem por finalidade gerar a codificacao para obtencao da
    matriz X do modelo atraves do contraste entre o n?vel i e o ultimo n?vel.
  Par?metros
    nr: Numero de linhas da matriz. A ordem sera sempre nr x (nr-1)
}
var
  i, j: Integer;
begin
  Result := TwsGeneral.Create(nr, nr-1);
  with Result do
    begin
    Name:='Controle';
    for i:=1 to NCols do
      ColName[i]:='C_'+IntToStr(i);
    for i := 1 to NRows-1 do
      begin
      RowName[i]:='Niv_'+IntToStr(i);
      for j := 1 to NCols do
        if (i=j) then
          Data[i,j]:=1
        else
          Data[i,j]:=0;
      end;
    for i := 1 to NCols do
      Data[nr,i]:=-1;
    RowName[nr]:='Niv_'+IntToStr(nr);
    end
end; { Control }

function LControl(nr: Integer): TwsGeneral;
{ Objetivo
    Obtem de reparametriza??o para MCS utilizando o ?ltimo tratamento como controle.
  Par?metros
    nr: Numero de n?veis do fator A ou n?mero de tratamentos
  Retorno
    Matriz nr x nr+1 com a indica??o dos efeitos
}
var
  i, j: Integer;
begin
  Result := TwsGeneral.Create(nr, nr+1);
  with Result do
    begin
    Name:='LControle';
    MLab:='Matriz de reparametriza??o (MCS) - Desvios com ?ltimo tratamento';
    PrintOptions.ColWidth:=9;
    PrintOptions.ColPrecision:=5;
    ColName[1]:='M';
    RowName[1]:='M';
    Data[1,1]:=1;
    for i:=2 to NCols do
      begin
      ColName[i]:='t_'+IntToStr(i-1);
       Data[1,i]:=1/nr
      end;
    for i := 2 to NRows do
      begin
      RowName[i]:='D_'+IntToStr(i-1);
      Data[i,1]:=0;
      for j := 2 to NCols-1 do
        if j = i then
          Data[i,j] := 1
        else
          Data[i,j] := 0
      end;
    for i:=2 to nRows do
      Data[i,nCols]:=-1
    end // with
end; { LControl }

function Experim(nr: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz com a primeira linha nula e as demais formando uma matriz identidade.
    Primariamente tem por finalidade gerar a codificacao para obtencao da matriz X do
    modelo anulando o primeiro efeito
  Par?metros
    nr: N?mero de linhas da matriz. A ordem sera sempre nr x (nr-1)
}
var
  i, j: Integer;
begin
  Result := TwsGeneral.Create(nr, nr-1);
  Result.Name:='Experim';
  for j:=1  to Result.NCols do
    begin
    Result.ColName[j]:='Bin_'+IntToStr(j);
    Result[1,j]:=0
    end;
  Result.RowName[1]:='Niv_1';
  for i:=2 to Result.NRows do
    begin
    Result.RowName[i]:='Niv_'+IntToStr(i);
    for j:=1 to Result.NCols do
      if (i=j+1) then
        Result[i,j]:=1
      else
        Result[i,j]:=0;
    end
end; { Experim }

function Profile(nr: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz que contrasta a posicao i com a imediatamente subsequente e anula as
    demais posicoes da coluna. Primariamente tem por finalidade gerar a codificacao para
    obtencao da matriz X do modelo para analise de perfil, por comparar somente os
    niveis subsequentes.
  Parametros
    nr: Numero de linhas da matriz. A ordem sera sempre nr x (nr-1)
}
var
  i: Integer;
begin
  Result := TwsGeneral(Jota(nr,nr-1,mtGeneral,0));
    Result.Name:='Perfis';
  for i:=1 to nr-1 do
    begin
    REsult.ColName[i]:='C_'+IntToStr(i);
    Result.RowName[i]:='Niv_'+IntToStr(i);
    Result[i,i]:=1;
    Result[i+1,i]:=-1
    end;
  Result.RowName[nr]:='Niv_'+IntToStr(nr)
end; { Profile }

function LProfile(nr: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz de reparametriza??o com contrastes indicando perfis (contrasta a posicao
    i com a imediatamente subsequente e anula as demais posicoes da coluna.
  Parametros
    nr: Numero de linhas da matriz. A ordem sera sempre nr x (nr+1)
}
var
  i,j,k: Integer;
begin
  Result := TwsGeneral.Create(nr,nr+1);
  with Result do
    begin
    Name:='Perfis';
    MLab:='Matriz de reparametriza??o (MCS) utilizando perfis';
    PrintOptions.ColWidth:=9;
    PrintOptions.ColPrecision:=5;
    ColName[1]:='M';
    RowName[1]:='M';
    Data[1,1]:=1;
    for i:=2 to NCols do
      begin
      ColName[i]:='t_'+IntToStr(i-1);
       Data[1,i]:=1/nr
      end;
    for i := 2 to NRows do
      begin
      RowName[i]:='P_'+IntToStr(i-1);
      Data[i,1]:=0;
      j:=2;
      while j<i do
        begin
        Data[i,j] := 0;
        Inc(j)
        end;
      Data[i,j] := 1;
      Data[i,j+1] := -1;
      for k:=j+2 to nCols do
        Data[i,k]:=0;
      end
    end // with
end; { LProfile }

function MeanTransf(nr: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz que contrasta a posicao i com a media das demais. Primariamente tem
    por finalidade gerar a codificacao para obtencao da matriz X. Difere da matriz de
    Helmert por tomar uma media de todos os demais niveis.
  Parametros
    nr: Numero de linhas da matriz. A ordem sera sempre nr x (nr-1)
}
var
  i,j  : Integer;
  a1,a2: Double;
begin
  Result := TwsGeneral.Create(nr,nr-1);
  a1:=-1/nr;
  a2:=(nr-1)/nr;
  with Result do
    begin
    Name:='Media';
    for i:=1 to nRows do
      RowName[i]:='Niv_'+IntToStr(i);
    for j:=1 to nCols do
      begin
      ColName[j]:='C_'+IntToStr(j);
      for i:=1 to nRows do
        if i=j then
          Data[i,j]:=a2
        else
          Data[i,j]:=a1
      end
    end // with
end; { MeanTransf }

function LMeanTransf(nr: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz de reparametriza??o atrav?s do constraste entre um tratamento e a m?dia
    dos demais.
  Par?metros
    nr: Numero de linhas da matriz. A ordem sera sempre nr x (nr+1)
}
var
  i,j  : Integer;
  a1,a2: Double;
begin
  Result := TwsGeneral.Create(nr,nr+1);
  a1:=1/nr;
  a2:=(nr-1)/nr;
  with Result do
    begin
    Name:='LMedia';
    MLab:='Matriz de reparametriza??o (MCS) utilizando contrastes simples';
    PrintOptions.ColWidth:=9;
    PrintOptions.ColPrecision:=5;
    ColName[1]:='M';
    RowName[1]:='M';
    Data[1,1]:=1;
    for i:=2 to NCols do
      begin
      ColName[i]:='t_'+IntToStr(i-1);
       Data[1,i]:=a1
      end;
    for i := 2 to NRows do
      begin
      RowName[i]:='M_'+IntToStr(i-1);
      Data[i,1]:=0;
      for j:=2 to nCols do
        if j=i then
          Data[i,j]:=a2
        else
          Data[i,j]:=-a1
      end;
    end // with
end; { Profile }

function PolOrth(x: TwsVec; m: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz com valores de polinomios ortogonais
  Par?metros
    x: valores da variavel polinomial. N?o poder? ter valores repetidos
    m: grau do polinomio e numero de colunas da matriz dos polinomios
}
var
  p1,p2       : TwsVec;
  s,c0,c1,c2,
  a,b,c       : Double;
  i,j,n       : Integer;
begin
  n := x.Len;
  Result := TwsGeneral.Create(n, m);
  Result.Name:='PolOrt';
  p1 := TwsDFVec.Create(n);
  p2 := TwsDFVec.Create(n);
  c0:=1/sqrt(n);
  for i:=1 to n do
    begin
    p1[i]:=c0;
    p2[i]:=0;
    Result.RowName[i]:='x_'+IntToStr(i)
    end;
  for j:=1 to m do
    begin
    c0:=0; c1:=0; c2:=0;
    Result.ColName[j]:='Pol_'+IntToStr(j);
    for i:=1 to n do
      begin
      s:=x[i]*p1[i];
      c0:=c0+x[i]*s*p1[i];
      c1:=c1+s*p1[i];
      c2:=c2+s*p2[i];
      end;
    a:=1/sqrt(c0-c1*c1-c2*c2);
    b:=-a*c1;
    c:=a*c2;
    for i:=1 to n do
      begin
      s:=p1[i];
      p1[i]:=(a*x[i]+b)*p1[i]-c*p2[i];
      Result[i,j] := p1[i];
      p2[i]:=s;
      end;
    end;
  p1.Free;
  p2.Free;
end; { PolOrth }

function LPolOrth(x: TwsVec; m: Integer): TwsGeneral;
{ Objetivo
    Gera uma matriz de reparametriza??o com valores de polinomios ortogonais
  Par?metros
    x: valores da variavel polinomial. N?o poder? ter valores repetidos
    m: grau do polinomio e numero de colunas da matriz dos polinomios
}
var
  p1,p2       : TwsVec;
  s,c0,c1,c2,
  a,b,c       : Double;
  i,j,n       : Integer;
begin
  n := x.Len;
  Result := TwsGeneral.Create(m+1,n+1);
  with Result do
    begin
    Name:='L_PolOrt';
    MLab:='Matriz de reparametriza??o utilizando polin?mios ortogonais';
    PrintOptions.ColWidth:=9;
    PrintOptions.ColPrecision:=5;
    p1 := TwsDFVec.Create(n);
    p2 := TwsDFVec.Create(n);
    c0:=1/sqrt(n);
    Data[1,1]:=1;
    ColName[1]:='M';
    for i:=1 to n do
      begin
      p1[i]:=c0;
      p2[i]:=0;
      ColName[i+1]:='t_'+IntToStr(i);
      Data[1,i+1]:=1/n;
      end;
    RowName[1]:='M';

    for j:=2 to m+1 do
      begin
      c0:=0; c1:=0; c2:=0;
      RowName[j]:='Pol_'+IntToStr(j-1);
      Data[j,1]:=0;
      for i:=1 to n do
        begin
        s:=x[i]*p1[i];
        c0:=c0+x[i]*s*p1[i];
        c1:=c1+s*p1[i];
        c2:=c2+s*p2[i];
        end;
      a:=1/sqrt(c0-c1*c1-c2*c2);
      b:=-a*c1;
      c:=a*c2;
      for i:=1 to n do
        begin
        s:=p1[i];
        p1[i]:=(a*x[i]+b)*p1[i]-c*p2[i];
        Data[j,i+1] := p1[i];
        p2[i]:=s;
        end;
      end;
    end; // with
  p1.Free;
  p2.Free;
end; { LPolOrth }


function WOrthPol(x, w: TwsVec; m: Integer): TwsGeneral; { Amauri 26/11/97 }
{ Objetivo
    Gera uma matriz com os valores de polin?mios ortogonais ponderados
  Par?metros
    x: vetor com os valores da variavel polinomial
    w: vetor com os valores da variavel peso
    m: grau do polinomio e numero de colunas da matriz dos polinomios }
var
  p1,p2       : TwsVec;
  s,c0,c1,c2,
  a,b,c       : Double;
  i,j,n       : Integer;
begin
  n := x.Len;
  Result := TwsGeneral.Create(n, m);
  Result.Name:='WPolOrt';
  p1 := TwsDFVec.Create(n);
  p2 := TwsDFVec.Create(n);
  c0:=0;
  for i:=1 to n do c0:=c0+w[i];
  c0:=1/sqrt(c0);
  for i:=1 to n do
    begin
    p1[i]:=c0;
    p2[i]:=0;
    Result.RowName[i]:='Niv_'+IntToStr(i)
    end;
  for j:=2 to m+1 do
    begin
    c0:=0; c1:=0; c2:=0;
    for i:=1 to n do
      begin
      s:=w[i]*x[i]*p1[i];
      c0:=c0+x[i]*s*p1[i];
      c1:=c1+s*p1[i];
      c2:=c2+s*p2[i];
      end;
    a:=1/sqrt(c0-c1*c1-c2*c2);
    b:=-a*c1;
    c:=a*c2;
    for i:=1 to n do
      begin
      s:=p1[i];
      p1[i]:=(a*x[i]+b)*p1[i]-c*p2[i];
      Result[i, j-1] :=  p1[i];
      p2[i]:=s;
      end;
    end;
  p1.Free;
  p2.Free;
end; { WOrthPol }

function WPolinFit(var Beta: TwsTriangular; var x,w,y,sq: TwsVec; m: Integer): TwsGeneral;
{  Objetivo
     Gera uma matriz de polinomios ortogonais
   Par?metros
     Beta: Retorna a matriz dos coeficientes para o polin?mio de grau zero, 1, 2, etc. Os
           coeficientes dos polinomios de cada grau estao localizados nas linhas. O de
           grau 0 na 1a. linha (um valor), o de grau 1 na 2a. linha (dois valores), etc.
      x:   Vetor com os valores da vari?vel polinomial. Dever?o estar ordenados em ordem
           crescente
      w:   Vetor com os valores da vari?vel peso (usualmente o n?mero de repeti??es)
      y:   Vetor com os valores da vari?vel dependente (usualmente as m?dias)
      sq:  Somas de quadrados associadas a cada polin?mio ortogonal
      m:   Grau do polin?mio e n?mero de colunas da matriz dos valores dos polin?mios
}
var
  p1,p2      : TwsVec;
  coef       : TwsTriangular;
  s,c0,c1,c2,
  a,b,c      : Double;
  i,j,k,n    : Integer;
begin
  n := x.Len;
  Result := TwsGeneral.Create(n, m);
  Beta := TwsTriangular.Create(m+1);
  sq := TwsDFVec.Create(m+1);
  p1 := TwsDFVec.Create(n);
  p2 := TwsDFVec.Create(n);
  Coef := TwsTriangular.Create(m+1);
  c0:=0;
  c1:=0;
  for i:=1 to n do
    begin
    c0:=c0+w[i];
    c1:=c1+w[i]*y[i];
    end;
  c0:=1/sqrt(c0);
  for i:=1 to n do
    begin
    p1[i]:=c0;
    p2[i]:=0;
    end;
  sq[1]:=c1*c0;
  coef[1, 1] := c0;
  beta[1, 1] := coef[1, 1]*sq[1];
  for j:=2 to m+1 do
    begin
    c0:=0;
    c1:=0;
    c2:=0;
    for i:=1 to n do
      begin
      s:=w[i]*x[i]*p1[i];
      c0:=c0+x[i]*s*p1[i];
      c1:=c1+s*p1[i];
      c2:=c2+s*p2[i];
      end;
    a:=1/sqrt(c0-c1*c1-c2*c2);
    b:=-a*c1;
    c:=a*c2;
    sq[j]:=0;
    for i:=1 to n do
      begin
      s:=p1[i];
      p1[i]:=(a*x[i]+b)*p1[i]-c*p2[i];
      sq[j]:=sq[j]+w[i]*y[i]*p1[i];
      Result[i, j-1] := p1[i];
      p2[i]:=s;
      end;
    for k:=1 to j do
      begin
      if k=1 then c0:=0 else c0:=coef[j-1, k-1];
      if j=2 then c2:=0 else c2:=coef[j-2, k];
      coef[j, k] := c0*a+coef[j-1, k]*b-c2*c;
      beta[j, k] := Beta[j-1, k] + sq[j]*coef[j, k];
      end;
    sq[j] := sq[j]*sq[j];
    end;
  p1.Free;
  p2.Free;
  coef.Free;
end; { wpolort }

function Hilbert(nc: Integer): TwsGeneral;
{ Objetivo
    Gera a matriz de Hilbert onde o elemento na posi??o (i, j) ? obtido como 1/(i+j-1). A matriz
    de Hilbert ? utilizada como exemplo de matriz mal condicionada.
  Par?metros
    nc: Dimens?o da matriz quadrada
}
var
  i, j: Integer;
  L0: TwsVec;
begin
  Result := TwsGeneral.Create(nc, nc);
  for i := 1 to Result.NCols do
    begin
    L0 := Result.Row[i];
    for j := 1 to Result.NCols do
      L0[j] := 1/(i + j - 1);
    end
end;

function Jota(nr, nc: Integer; MT: TwsEnumMatType; x: Double): TwsMatrix;
{ Objetivo
    Gera uma matriz do tipo especificado onde todos os valores s?o iguais
  Par?metros
    nr: N?mero de linhas
    nc: N?mero de colunas
    MT: Tipo da matriz (mtGeneral, mtSymmetric, mtDiagonal, mtTriangular, mtVandermonde, mtToeplitz)
    x : Valor de todos os elemntos da matriz
}
var
  i,j: Integer;
  F  : TwsVec;
begin
  case MT of
    mtGeneral: begin
               Result := TwsGeneral.Create(nr, nc);
               for i := 1 to Result.NRows do begin
                 F := Result.row[i];
                 for j := 1 to Result.NCols do F[j] := x
               end
           end;
     mtDiagonal: begin
                 Result := TwsDiagonal.Create(nr);
                 F := Result.Row[1];
                 for j := 1 to Result.NCols do F[j] := x
               end;
    mtSymmetric: begin
               Result := TwsSymmetric.Create(nr);
               for i := 1 to Result.NCols do
                 begin
                 F := Result.row[i];
                 for j := 1 to i do F[j] := x
                 end
               end;
    mtTriangular: begin
                  Result := TwsTriangular.Create(nr);
                  for i := 1 to Result.NCols do begin
                    F := Result.row[i];
                    for j := 1 to i do F[j] := x
                  end
                end;
  end; { case }
end;

{======================= cTwsDataSets ========================= }

constructor TwsDataSets.Create(aFree: Boolean = True);
begin
  inherited Create;
  FList := TList.Create;
  FFreeObjects := aFree;
end;

destructor TwsDataSets.Destroy;
var i: Integer;
begin
  if FFreeObjects then
     for i := 0 to FList.Count-1 do TObject(FList[i]).Free;

  FList.Free;
  inherited;
end;

//procedure TwsDataSets.setDataSet(i: Integer; Value: TwsDataSet);
procedure TwsDataSets.setData(i: Integer; Value: TwsDataSet);
begin
  FList[i] := Value;
end;

function TwsDataSets.getDataSet(i: Integer): TwsDataSet;
begin
  Result := TwsDataSet(FList[i]);
end;

//function TwsDataSets.Add(DataSet: TwsDataSet): Integer;
function TwsDataSets.Add(Obj: TObject): Integer;
begin
  Result := FList.Add(Obj);
end;

procedure TwsDataSets.Delete(i: Integer);
begin
  FList.Delete(i);
end;

function TwsDataSets.getCount: Integer;
begin
  Result := FList.Count;
end;

{ ================== TwsMatList ===================== }
function TwsMatList.getMatrix(i: Integer): TwsMatrix;
begin
  Result := TwsMatrix(FList[i]);
end;

procedure TwsMatList.setMatrix(i: Integer; Value: TwsMatrix);
begin
  FList[i] := Value;
end;

function TwsMatList.getGeneral(i: Integer): TwsGeneral;
begin
  Result := TwsGeneral(FList[i]);
end;

function TwsMatList.getSymmetric(i: Integer): TwsSymmetric;
begin
  Result := TwsSymmetric(FList[i]);
end;

function TwsMatList.getDiagonal(i: Integer): TwsDiagonal;
begin
  Result := TwsDiagonal(FList[i]);
end;

function TwsMatList.getTriangular(i: Integer): TwsTriangular;
begin
  Result := TwsTriangular(FList[i]);
end;

function TwsMatList.getVec(i: Integer): TwsVec;
begin
  Result := TwsVec(FList[i]);
end;

{ ===================== Fator triangular =================}

constructor TwsTrDecomp.Create(A: TwsSymmetric; RIdx: Integer=0);
{ Objetivo
    Cria objeto para tratamento da decomposi??o triangular
  Par?metros
    A: Matriz sim?trica. No problema de m?nimos quadrados A ? a matriz de somas de
      quadrados e produtos
    RIdx: ?ndice da vari?vel resposta. Se RIdx = 0 n?o existe vari?vel resposta
  Matrizes criadas
    Fatores L e D - posicoes 0 e 1
    Inversa de L  - posicao 2
    Matriz de covari?ncias, se RIdx>0 - Posicao 3
}
var
  maux: TwsTriangular;
begin
  inherited Create(False);
  fA := A;
  fEps:=1.0E-9;
  fErr:=0;
  fRidx:=RIdx;
  Decomp;
  // Se nao houve erro na decomposicao, obtem inversa do fator triangular L
  if fErr=0 then
    begin
    T.Copy(mtTriangular,TwsMatrix(maux));
    maux.Name:='L_Inv';
    maux.MLab:='Inversa do fator triangular';
    maux.Inv(fErr,fEps);
    Add(maux);        // posicao 2
    if fRIdx>0 then
      CovMat;
    end
end;

function TwsTrDecomp.GetTriang;
begin
  Result:=GetTriangular(0)
end;

function TwsTrDecomp.GetDiag;
begin
  Result:=GetDiagonal(1)
end;

function TwsTrDecomp.GetTInv;
begin
  Result:=GetTriangular(2)
end;

function TwsTrDecomp.GetXInv: TwsSymmetric;
begin
  Result:=GetSymmetric(3)
end;

function TwsTrDecomp.GetSS(j: Integer): double;
begin
  if ((j>0) and (j<=T.nCols)) then
    Result:=D[j,j];
end;

function TwsTrDecomp.GetCoef(j: Integer): double;
begin
  if ((j>0) and (j<fRIdx)) then
    Result:=-TInv[fRidx,j];
end;

procedure TwsTrDecomp.Decomp;
{ Objetivo
    Obt?m os fatores da decomposi??o triangular L e D e insere na lista
  Campos modificados
    Lista de matrizes
    fErr: c?digo de erro
    fd
  Algoritmo
    Kennedy & Gentle (1980), pag. 295
}
var
  i,j,
  n,k : Integer;
  b   : Extended;
  Tr  : TwsTriangular;
  Dg  : TwsDiagonal;
begin
  // posto da matriz
  n:=fA.nCols;
  fRank:=n;
  Tr := TwsTriangular.Create(n);
  Tr.Name:='FatT';
  Tr.MLab:='Fator triangular';
  Dg:=TwsDiagonal.Create(n);
  Dg.Name:='FatD';
  Dg.MLab:='Fator diagonal';

  // elementos da diagonal
  fd:=Dg.Row[1];
  for j:=1 to n do
    begin
    b:=0;
    for k:=1 to j-1 do
      b:=b+Sqr(Tr[j,k])*fd[k];
    fd[j]:= fA[j,j]-b;
    // verifica se valor da diagonal nao eh proximo de zero
    if fd[j]>fEps then
      for i:=j+1 to n do
        begin
        b:=0;
        for k:=1 to j-1 do
          b:=b+Tr[j,k]*Tr[i,k]*fd[k];
        Tr[i,j]:=(fA[i,j]-b)/fd[j]
        end
    else
      Dec(fRank);
    Tr[j,j]:=1
    end;
  if fRank < n then
    fErr:=maInvError;
  Add(Tr);             // posicao 0
  Add(Dg)              // posicao 1
end; // Decomp

procedure TwsTrDecomp.Solve(B: TwsGeneral);
{ Objetivo
    Uma vez obtida a decomposi??o triangular de A, resolve o sistema Ax=B
  Par?metro
    Cada coluna de B representa um sistema com a mesma matriz de coeficientes, ou
    seja A(x1, x2, ...) = (b1, b2, ...)
  Retorno
    Na sa?da B conter? nas colunas a solu??o de cada sistema (x1, x2, ...)
}
var
  i,j,k: Integer;
  y    : Extended;
begin
  if fErr = 0 then
    begin
    // Resolve o sistema Lg = B
    // B eh substituido por g
// n := fRidx-1;
// resolver sistema como no algoritmo de Martin
// evita necessidade de inversao p-ara obtencao das estimativas
    T.FwdSolve(B,fErr);
    // Resolve o sistema (DU)b = g
    // B retorna a solucao do(s) sistema(s)
    for j:=1 to B.nCols do
      for i:=T.nCols downto 1 do
        begin
        y:=B[i,j]/fd[i];
        for k:= i+1 to T.nCols do
          y:=y-T[k,i]*B[k,j];
        B[i,j]:=y
        end
    end;
end; // Solve

function TwsTrDecomp.Cholesky: TwsTriangular;
var
  i,j,n: Integer;
  s    : double;
begin
  n:=fRIdx-1;
  Result:=TwsTriangular.Create(n);
  for i:=1 to n do
    if fd[i]>fEps then
      begin
      s:=Sqrt(fd[i]);
      for j:= 1 to i do
        Result[i,j]:=T[i,j]*s
      end
end;  // Cholesky

procedure TwsTrDecomp.CovMat;
var
  i,j,
  k,n : Integer;
  s   : Extended;
  C   : TwsSymmetric;
  A   : TwsTriangular;
begin
  // n eh a ultima coluna antes da resposta
  n:=fRIdx-1;
  C:=TwsSymmetric.Create(n);
  C.Name:='TD_Cov';
  C.MLab:='Matriz de covari?ncias';
  A:=TInv;
  for j:=1 to n do
    for i:=j to n do
      begin
      s:=0;
      for k:=i to n do
        s:=s+(A[k,j]*A[k,i])/fd[k];
      C[i,j]:=s
      end;
  Add(C)   // posicao 3
end;


{ ================ Minimos quadrados por Householder ================= }
constructor TwsHouseLSP.Create(A: TwsGeneral; RIdx: Integer);
begin
  FRIdx:=RIdx;
  fA:=A;
  inherited Create(False);
  Decomp; // posicoes 0, 1, 2, 3
end;

function TwsHouseLSP.GetLSSol: TwsGeneral;
begin
  Result:=TwsGeneral(FList[4])
end;

function TwsHouseLSP.GetRes(i: Integer): Double;
begin
  Result:=TwsDFVec(FList[3])[i];
end;

function TwsHouseLSP.GetReg(i: Integer): Double;
begin
  Result:=TwsDFVec(FList[2])[i];
end;

procedure TwsHouseLSP.Decomp;
{ Objetivo
    Aplica as transformacoes de Householder a matriz tal que A=QU
  Sa?da
    * Ap?s a chamada de Decomp o conte?do da matriz fA ? o seguinte:
      a11 a12       u11 u12
      a21 a22 --->  f12 u22
      a31 a32       f13 f23
      a41 a42       f14 f24
    g=[f11, f22] e h=[(f1)'*f1, (f2)'*f2]
    sendo que com g e h ? poss?vel construir a matriz de reflex?o de Householder e obter Q.
    * Se a matriz A nxp for completada ? direita com uma identidade, no lugar da identidade
    retorna a matriz Q.
    * No sistema inconsistente y=Xb + e onde X ? nxp entao se A=[X, y] ent?o, no final
    Q v   ? a solucao de m?nimos quadrados ser? a solu??o de Qb=v e t't ? a soma de
    0 t   quadrados de quadrados do res?duo
  Campos modificados
    fA: contem a matriz operada
    g, que contem os termos da diagonal de H, e h, que contem as somas de quadrados das
    colunas de H s?o inseridos na lista nas posi??es 0 e 1.
    Os valores da soma de quadrados da regress?o e dav soma de quadradosm dos residuos
    para cada vari?vel tamb?m s?o obtidos e os respectivos vetores inseridos na lista nas
    posi??es 2 e 3.
}
var
  s        : ShortInt;
  i,j,k,p  : Integer;
  l,aux,eps: Double;

  d,m      : Extended;
  g,h,s2,r : TwsVec;
begin
  fError :=0;
  p:=fRIdx-1; // ultima coluna com preditoras
  eps := 1/(16*16*16*16*16)*fA.NRows;
  g := TwsDFVec.Create(p);
  h := TwsDFVec.Create(p);
  for i:=1 to p do
    begin
    d:=0;
    for k:=i to fA.NRows do
      d:=d+Sqr(fA[k,i]);
    aux:=fA[i,i];
    if aux>0 then
      s:=1
    else
      s:=-1;
    l:=s*Sqrt(d);
    h[i]:=d+l*aux;
    // protege a divisao no caso de matrizes de posto coluna incompleto
    if Abs(h[i])<=eps then
      begin
      g.Free;
      h.Free;
      fError:=NHouse;
      Exit
      end;
    g[i]:=l+aux;
    fA[i,i]:=-l;
    for j:=i+1 to fA.NCols do
      begin
      m:=0;
      for k:=i+1 to fA.NRows do
        m:=m+fA[k,i]*fA[k,j];
      m:=(g[i]*fA[i,j]+m)/h[i];
      fA[i,j]:=fA[i,j]-g[i]*m;
      for k:=i+1 to fA.NRows do
        fA[k,j]:=fA[k,j]-fA[k,i]*m
      end; // for j
    if s>0 then
      for j:=i to fA.NCols do
        fA[i,j]:=-fA[i,j]
    end;  // for i - para cada coluna de;
  Add(g); // posicao 0
  Add(h);  // posicao 1

  // Numero de respostas
  p:=fA.nCols-fRIdx+1;
  // Armazena somas de quadrados do erro para cada resposta
  s2:=TwsDFVec.Create(p);
  // Armazena somas de quadrados da regressao para cada resposta
  r:=TwsDFVec.Create(p);
  // Numero de variaveis
  p:=fRIdx-1;
  k:=0;
  for j:=fRIdx to fA.nCols do
    begin
    m:=0;
    d:=0;
    for i:=1 to p do
      m:=m+Sqr(fA[i,j]);
    for i:=p+1 to fA.nRows do
      d:=d+Sqr(fA[i,j]);
    Inc(k);
    r[k]:=m;       // regss
    s2[k]:=d;      // resss
    end;
  Add(r);       // posicao 2
  Add(s2)       // posicao 3
end; // Decomp

procedure TwsHouseLSP.HFTCov;
  { Objetivo
      Ap?s a aplica??o da transforma??o de Householder para uma solu??o de m?nimos
      quadrados, o fator triangular fica armazenado na por??o triangular superior
      esquerda da matriz original. Este procedimento retorna a matriz de covari?ncia
      (sem o fator de escala) da solu??o de m?nimos quadrados. Como a invers?o ? feita
      no local, deve sempre ser a ultima rotina a ser executada.
}
var
  i,j,
  l,p : Integer;
  s   : Extended;
begin
  // Inverte a triangular superior
  p:=fRIdx-1;
  for i := 1 to p do
    fA[i,i]:=1/fA[i,i];
  if p > 1 then
    begin
    for i := 1 to p-1 do
      for j := i+1 to p do
        begin
        s := 0;
        for l := i to j-1 do
          s := s + fA[i,l]*fA[l,j];
        fA[i,j] := -fA[j,j]*s
        end
    end;
  for i := 1 to p do
    for j := i to p do
      begin
      s := 0;
      for l := j to p do
        s := s + fA[i,l]*fA[j,l];
      fA[i,j] := s
      end
end; { TwsHouseLSP.HFTCov }

procedure TwsHouseLSP.Solve;
  { Objetivo
      Ap?s a aplica??o da transforma??o de Householder para uma solu??o de m?nimos
      quadrados, o fator triangular fica armazenado na por??o triangular superior
      esquerda da matriz original. Este procedimento retorna (na posi??o 2) a
      solu??o de m?nimos quadrados.
}
var
  i,j,
  k,p,nb: Integer;
  s     : Extended;
  B     : TwsGeneral;
begin
  p:=fRIdx-1;
  // numero de colunas de B
  nb:=fA.nCols-fRIdx+1;
  B:=TwsGeneral.Create(p,nb);
  B.Name:='Solucao_MQ';
  B.MLab:='Solu??o de m?nimos quadrados';
  for j:=1 to nb do       // resolve para cada coluna de B
    begin
    for k:=1 to p do
      B[k,j]:=fA[k,p+j];
    B[p,j]:=B[p,j]/fA[p,p];
    for i:=p-1 downto 1 do
      begin
      s:=0;
      for k:=i+1 to p do
        s:=s+fA[i,k]*B[k,j];
      B[i,j]:=(B[i,j]-s)/fA[i,i]
      end
    end; // for j
  Add(B) // posicao 4
end; // Solve
{ ========================= FIM =========================== }

procedure CheckError(const Func: String; M: TwsGeneral; Col, Ini, Fim: Longint);
begin
  if Col > M.NCols then
     Raise Exception.Create(Func + ': ?ndice de Coluna inv?lido');

  if (Fim > M.NRows) or (Ini < 1) then
     Raise Exception.Create(Func + ': ?ndice de Linha inv?lido');
end;

Function wsMatrixMax (M: TwsGeneral; Col, Ini, Fim: Longint): Double;
var i: Longint;
    V: Double;
Begin
  CheckError('wsMatrixMax', M, Col, Ini, Fim);

  Result := wscMissValue;
  For i := Ini to Fim do
    Begin
    V := M[i, Col];
    If V > Result Then Result := V;
    End;

//  if Math.SameValue(Result, Math.MinDouble) then
//     Result := wscMissValue;
End;

Function wsMatrixMin (M: TwsGeneral; Col, Ini, Fim: Longint): Double;
var i: Longint;
    V: Double;
Begin
  CheckError('wsMatrixMin', M, Col, Ini, Fim);

  Result := 10E200;
  For i := Ini to Fim do
    Begin
    V := M[i, Col];
    If (V < Result) and not IsMissValue(V) Then
       Result := V;
    End;
  if Math.SameValue(Result, 10E200) then
     Result := wscMissValue;
End;

Function wsMatrixSum (M: TwsGeneral; Col, Ini, Fim: Longint; Var Somados: Longint): Double;
var i: Longint;
    V: Double;
Begin
  CheckError('wsMatrixSum', M, Col, Ini, Fim);

  Somados := 0;
  Result := 0;
  For i := Ini to Fim do
    Begin
    V := M[i, Col];
    If not IsMissValue(V) Then
       Begin
       Inc (Somados);
       Result := Result + V;
       End;
    End;
  if Somados = 0 then Result := wscMissValue;
End;

Function wsMatrixMean (M: TwsGeneral; Col, Ini, Fim: Longint): Double;
var i, ii: Longint;
Begin
  CheckError('wsMatrixMean', M, Col, Ini, Fim);

  Result := wsMatrixSum(M, Col, Ini, Fim, ii);
  if ii > 0 then
     Result := Result / ii
  else
     Result := wscMissValue;
End;

// {SUM(i=1,N)[(X(i) - Mean)**2]} / N
Function wsMatrixVar  (M: TwsGeneral; Col, Ini, Fim: Longint; Media: Double = -1): Double;
Var i, n      : Longint;
    x         : Double;
begin
  CheckError('wsMatrixVar', M, Col, Ini, Fim);

  Result := 0;
  n := 0;

  if Media = -1 Then
     begin
     Media := wsMatrixMean(M, Col, Ini, Fim);
     if IsMissValue(Media) then
        begin
        Result := wscMissValue;
        Exit;
        end;
     end;

  For i := Ini to Fim do
    Begin
    x := M[i, Col];
    If not IsMissValue(x) Then
       Begin
       Inc(n);
       Result := Result + (SQR(x - Media));
       End;
    End;

  if n > 1 then
     Result := Result / (n-1)
  else
     Result := wscMissValue;
end;

{Desvio Padr?o}
Function wsMatrixDSP (M: TwsGeneral; Col, Ini, Fim: Longint; Media: Double = -1): Double;
Var i, n      : Longint;
    V         : Double;
begin
  CheckError('wsMatrixDSP', M, Col, Ini, Fim);

  Result := 0;
  n := 0;

  if Media = -1 Then
     begin
     Media := wsMatrixMean(M, Col, Ini, Fim);
     if IsMissValue(Media) then
        begin
        Result := wscMissValue;
        Exit;
        end;
     end;

  For i := Ini to Fim do
    Begin
    V := M[i, Col];
    If not IsMissValue(V) Then
       Begin
       Inc(n);
       Result := Result + ( SQR( V - Media ));
       End;
    End;

  if n > 1 then
     Result := Sqrt(Result / (n - 1))
  else
     Result := wscMissValue;
End;

{Coeficiente de varia??o}
function  wsMatrixCV (M: TwsGeneral; Col, Ini, Fim: Longint; Media: Double = -1): Double;
Begin
  CheckError('wsMatrixCV', M, Col, Ini, Fim);

  if Media = -1 Then
     begin
     Media := wsMatrixMean(M, Col, Ini, Fim);
     if IsMissValue(Media) then
        begin
        Result := wscMissValue;
        Exit;
        end;
     end;

  Result := (wsMatrixDSP (M, Col, Ini, Fim, Media) / Media) * 100;
End;

function CreateDatasetVar(varType: byte): TwsDataSetCol;
begin
  case TwsEnumDataType(varType) of
    dtNumeric   : result := TwsNumeric.Create      (NONAME, '');
    dtQuant     : result := TwsQuantitative.Create (NONAME, '');
    dtQualit    : result := TwsQualitative.Create  (NONAME, '');
    dtQualitOrd : result := TwsOrdered.Create      (NONAME, '');
  end;
end;
initialization
  wsm_MatrixChange := getMessageManager.RegisterMessageID('wsm_MatrixChange');

end.
