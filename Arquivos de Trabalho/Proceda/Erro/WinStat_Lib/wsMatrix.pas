unit wsMatrix;

{
    MODIFICACOES

    

        23/09/96   - Inclusao de Modificacoes do Adriano:         - Roger

                        - Inclusao de propriedades
                        - Alteracao em ToChar de alguns Objetos

        09/01/97   - Inclusao de TFGeneral                        - Roger

        17/01      - Alteração em TMatrix.SetRow. Agora contém
                     validação de linha e coluna e liberação de
                     memória em caso de atribuição de nova linha  - Roger e

                   - Alteracao do nome do metodo Row para
                     PutLine                                      - Roger

        06/02      - BugFix em TMatrix.Print                      - Roger

        06/03      - Inclusao de TMatrix.SaveToFile               - Roger

        19/06      - Tentativa de adição de Propriedade Default   - Rochedo
                     Falhou pois ja existe uma na hierarquia da
                     classe.

                   - Inclusão de WS nos nomes dos tipos           - Roger

        04/05/1998 - Inclusão da propriedade MODIFIED             - Rochedo


  AUTOR E DATA: ........................ Rochedo, 17/06/1998
  ESCOPO: .............................. Todas as variaveis inteiras de 2 bytes
                                         passam a ter 4 bytes.
                                         (Integer, Word) --> (Integer)

  AUTOR E DATA: ........................ Rochedo, 19/06/1998
  ESCOPO: .............................. Criando o método DeleteCol para TMatrix
  OBS.: ................................ Cuidado ao utilizar DeleteCol em classes
        descendentes de TwsMatrix, pois alguns descs. não são matrizes quadradas e
        portanto seus valores são guardados de forma especial.
        Portanto, por enquanto só utilizar em descendentes de TwsGeneral  <-------

  22/07/1999 - Rochedo
    - Reformulação dos métodos de leitura e salvamento
      - Foram criados 3 métodos de cada procedimento.
        Leitura (InternalLoad1, InternalLoad2, InternalLoad3).
        Salvamento (InternalSave1, InternalSave2, InternalSave3).
        Para os métodos de salvamento poderíamos fundir os métodos InternalSave1/2 num
        só, mas para manter a simetria com os métodos de leitura decidi separá-los.

  AUTOR E DATA: ...... Alex, 22/02/2000
  ESCOPO: ............ Adicionada a linha
                         Result.FModified := False;
                       no método wsMatrix.LoadFromFile.

                       Adicionada a linha
                         FModified := True;
                       no método wsMatrix.SetName

  Autor e data......... Amauri, 22/04/200
  Escopo .............. Revisao profunda das funcoes que foram estabelecidas como métodos
                        Revisao dos metodos de impressao

  Autor e data......... Adriano, 09/07/2003
  Escopo .............. Implementação dos métodos Save e Load para arquivos ini
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
// Estas variáveis são inicializadas automaticamente na seção de inicialização
var
  wsm_MatrixChange: Integer;            // Mensagem de notificação de alterações

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

  { Herança
      TwsMatrix --> T_NRC_InterfacedObject
    Objetivo
      Implementar uma classe genérica para tratamento de matrizes. Possibilita tratar
      matrizes de vários tipos: geral, simétrica, mtDiagonal, mtTriangular, mtToeplitz e
      mtVandermonde
  }

  TwsMatrix = class(T_NRC_InterfacedObject, IToBXML)
  private
    FLocked: Integer;
    FList: TList;
    FStatusChange: TwsStatusChange_Event;
    FSelectElementColor: TwsSelectElementColor_Event;

    // Gravação/recuperação de partes do objeto matriz
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
    FMLab        : PString;  { Rótulo da matriz }
    FNRows       : Integer;  { Numero de linhas }
    FVersion     : String [10]; {Versao do arquivo}
    FExtra       : TwsExtraData; {Campo extra para futuro uso.}

    // Retorna sim ou não se a descrição das variáveis deve ser impressa
    function PrintDescVars: Boolean; virtual;
    // Obtem os dados do campo extra
    function getExtraData(i: Integer): byte;
    // seta os dados do campo extra
    procedure setExtraData(i: integer; Value: byte);
    // atribui número de linhas
    procedure SetNRows(NR: Integer);
    // Atribui nome da matriz
    procedure SetName(Const Name: String); virtual;
    // IToBXML interface - Retorna o nome do bloco BXML
    function GetBlockName: String; virtual;
    // IToBXML interface - Retorna o comentário explicativo do bloco BXML
    function GetBlockNameComment: String; virtual;
    // Recupera rótulo
    Function  GetMLab:String;
    // Atribui rótulo
    Procedure PutMLab(Const l:String);
    // Recupera endereço de linha
    function  GetRow(i: Integer): TwsVec;  Virtual;
    // Atribui endereço de linha
    Procedure SetRow(i: Integer; v: TwsVec);
    // Recupera nome de linha
    function  GetRowName(i: Integer): string; virtual;
    // atribui nome de linha
    procedure SetRowName(i: Integer; const s: string);
    // Recupera nome de coluna
    function  GetColName(i: Integer): string;    Virtual;
    // Recupera nome de coluna
    procedure SetColName(i: Integer; S: String); Virtual;

    // Leitura/gravação de parte da matriz
    Procedure CNameLoadFromStream(Reader:TReader); Virtual;
    Procedure CNameSaveToStream(Writer:TWriter);   Virtual;

    // Descreve as colunas como xml
    procedure StructColsToXML(Buffer: TStrings; var Ident: Integer); virtual;

    procedure RowsToXML(Writer: TXML_Writer); virtual;
    // Usada na criação automatica
    class function UserCreate: TwsMatrix; virtual;
    // Libera conteúdo
    destructor  Destroy; override;
    {$ifdef MSXML}
    procedure fromXML_LoadStruct(no: IXMLDomNode); virtual;
    {$endif}
  Public
    // Opções para impressão
    PrintOptions: TwsRecPrintOptions;

    FileName    : String[80];      {Rochedo, 04/04/1998}
    { Tag - Pode guardar virtualmente qualquer coisa.
      Pode ser usado como um ponteiro, pois tem 4 bytes de comprimento}
    Tag_1      : Integer;         {Rochedo, 29/05/1998}
    Tag_2      : Integer;         {Rochedo, 29/05/1998}
    // Participa da criação de uma matriz
    constructor Create(NR, NC: Integer);

    // IToBXML interface **************************************************************

    // Implementa a definição dos dados como xml.
    // Deverá ser sobreescrito, se necessário, para matrizes de diferentes formatos.
    procedure toXML(Buffer: TStrings; Ident: Integer); virtual;
    // Implementa a definição dos dados como bxml
    procedure toBXML(Buffer: TStrings; Ident: Integer);
    // Retorna o tipo da Classe para o mecanismo de serialização em XML
    function GetClassName: String;

    procedure ShowInSheet(Sheet: TBaseSpreadSheetBook); virtual;

    // end IToBXML interface **********************************************************

    {$ifdef MSXML}
    procedure fromXML(no: IXMLDomNode);
    {$endif}

    // Verifica se a coluna esta ordenada
    function SortedCol(IndexCol: Integer; Ascd: Boolean): Boolean;

    // Retorna um valor da matriz na posição especificada
    function ColInd(Const s: string): string;
    // Valor nessa posição é perdido?
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
    { Aplica funções matemáticas aos elementos da matriz }
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
    // Submatriz numa faixa de índices
    function SubMatrix(l, mb, k, nb: Integer): TwsGeneral;
    // Submatriz simétrica
    function SubSymMat(l,mb:Integer): TwsSymmetric;
    // Submatriz com índices especificados
    function SubIndex(R, C: TwsLIVec): TwsGeneral;
    // Reducao por coluna
    function ColReduc(R: TwsLIVec; Ch: TwsEnumTypeOp): TwsGeneral;
    // Redução por linha
    function RowReduc(C: TwsLIVec; Ch: TwsEnumTypeOp): TwsGeneral;
    // Redução por linha e coluna
    function RowColReduc(ChRow, ChCol: TwsEnumTypeOp): Double;
    // Estatísticas descritivas
    function DescStat(Col, Stat: TwsLIVec): TwsGeneral;
    // Estatísticas descritivas ponderadas
    function WDescStat(Col, Stat: TwsLIVec; WInd: Integer): TwsGeneral;
    // Estatisticas num vetor
    function VDescStat(Col, Stat: TwsLIVec): TwsVec;
    // Estatísticas ponderadas num vetor
    function VWDescStat(Col, Stat: TwsLIVec; WInd: Integer): TwsVec;
    // Percentil de coluna especificada
    function Perc(const f: Double; Col: Integer): Double;
    // Estatísticas de ordem
    function MatOrderStat(Col,Stat: TwsLIVec): TwsGeneral;
    // Funções aplicadas aos elementos da matriz
    function ScalarFunc(Op: TwsEnumConstFun; var n: Integer): Double;
    // Norma da coluna
    function Norm(j,l1,l2: Integer): Double;
    // Produto interno entre colunas
    function ColProd(j,k,l1,l2: Integer): Double;
    // Transfere o conteúdo da matriz para um vetor
    function ToVec: TwsVec; virtual;
    // Transfere todo conteúdo da matriz para um vetor
    function GToVec: TwsVec;
    // Transfere conteúdo da mtDiagonal para um vetor
    function DiagToVec: TwsVec; virtual;
    // Matriz de postos com empates
    function RankTie(NewMat: Boolean): TwsMatrix;
    // Matriz de postos
    function Rank(NewMat: Boolean; Ascd: Boolean): TwsMatrix;
    // retorna o maximo e o minimo da coluna
    procedure ColExtrems(k: Integer; out Min, Max: Double);
    // Sweep didatico
    procedure G2Sweep(k: Integer; DMin: Double=1E-8); virtual; abstract;
    // Atribui nomes às colunas
    procedure SetCName(st: TwsCVec;DelChar :TCharSet);
    // Atribui nomes às linhas
    procedure SetRName(st: TwsCVec;DelChar :TCharSet); virtual;
    // Adiciona uma linha com o n. de elementos da matriz
    function AddRow(const Name: string = ''): TwsVec;
    // Insere o vetor na lista sem alterar outros atributos
    Procedure PutLine(V:TwsVec);  Virtual;
    //Insere uma linha (vetor) na posicão especificada
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
    //Troca de posição duas linhas (vetores)
    procedure Exchange(i, j: Integer);
    // Copia uma coluna para um vetor
    function CopyCol(Col: Integer; IMiss: Boolean=False): TwsVec;
    // Copia conteúdos dss colunas para uma lista
    function CopyColsToList(Col: TwsLIVec; IMiss: Boolean=False): TList;
    // Verifica se a matriz é do tipo especificado
    function IsType(AType: TwsEnumMatType): Boolean;
    // Retorna índices de colunas num string
    function IndexColsFromString(const Col: String): TwsLIVec; Virtual;
    // Retorna índices de colunas numa lista
    Function IndexColsFromStrings(Cols: TStrings): TwsLIVec; Virtual;
    // Índice da coluna pelo nome
    Function IndexByName(Const Name : String) : Integer; {Virtual}
    // Coloca colunas na ordem estabelecida pelos índices
    procedure SortCol(Indx: TwsLIVec);
    // Impressão
    procedure List(Buffer: TStrings); virtual;
    // Cabeçalho
    procedure Header(Start, Amt: Integer; Buffer: TStrings); virtual;
    // Atribui largura de impressão
    procedure SetColWidth(Value: byte);
    // Impressão
    procedure Print(Buffer: TStrings); virtual;
    // Impressão de colunas opr condição
    procedure ColPrint(const Cols: String; Condicao: String; Buffer: TStrings); virtual; abstract;
    // Elementos da matriz num string
    function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; virtual;
    // Elementos selecionados da matriz num string
    procedure SelectToChar(L, Start, Amt: Integer; Col: TwsLIVec; Buffer: TStrings);
    // Cabeçalho de colunas selecionadas
    procedure SelectHeader(Col: TwsLIVec; Buffer: TStrings); Virtual;
    // Salva a matriz em um arquivo texto
    procedure ToText(var TFile: Text; IsLast: Boolean = True; LSep: Char = ';'; SSep: Char = '/');
    // transforma uma matriz em DataSet
    function ToDataSet: TwsDataSet;
    // Obtém os elementos da matriz de um arquivo texto
    procedure TextTo(var TFile: Text; Ch: Char);
    // Colunas para impressão
    function ColsToImp(var k: Integer): Integer; virtual;
    // Colunas selecionadas para impressão
    function SelectColsToImp(var k: Integer; Col: TwsVec): Integer; Virtual;
    // Inversão de matrizes
    procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); virtual; abstract;
    // Solução de sistemas de equações lineares
    procedure Solve(B: TwsMatrix; var ErrCode: Word); virtual; abstract;
    // Todos os valores são nulos?
    function All(const x: Double; Op: TwsEnumTypeOp): boolean;
    // Algum valor é nulo ?
    function Any(const x: Double; Op: TwsEnumTypeOp): boolean;
    // Concatenação de linhas
    function RowConcat(B: TwsMatrix;Var ErrCode: Word): TwsGeneral;
    // Concatenação de colunas
    function ColConcat(B: TwsMatrix;Var ErrCode: Word): TwsGeneral;
    // Concatenação de escalar nas linhas
    function RowScalarConcat(const x: Double; SFirst:boolean): TwsGeneral;
    // Concatenação de escalar nas colunas
    function ColScalarConcat(const x: Double; SFirst:boolean): TwsGeneral;
    // Concatenação de vetores nas linhas
    function RowVecConcat(v: TwsVec; NewMat: Boolean; VFirst: Boolean;
      Var ErrCode: Word): TwsGeneral;
    // Concatenação de vetores nas colunas
    function ColVecConcat(v: TwsVec; VFirst:boolean; Var Erro:Word): TwsGeneral;
    // Forma escalonada da matriz
    function Echelon(NewMat: Boolean; CMax: Integer=-1): TwsGeneral;
    // Ordenação de todos os elementos da matriz
    function QSort(Asc: Boolean=True): TwsGeneral;
    // Gravação de matrizes
    Class Function ObjectFromStream(Reader: TReader; Var Erro: Integer): TwsMatrix;
    // Recuperação de matrizes
    Class Function LoadFromFile(Const FileName: String): TwsMatrix;
    // Grava matriz num arquivo de nome especificado
    Procedure SaveToFile(Const FileName: String);
    // Remove o próprio objeto notificando o sistema antes
    procedure NotificationAndFree;
    // Envia uma mensagem de notificação de alterações para o sistema
    procedure NotifyChange(Status: TwsStatusChangeSet);
    // Bloqueia o objeto para alteração
    procedure Lock;
    // Desbloqueia o objeto para alteração
    procedure Unlock;

    // Nome da matriz
    property Name: String read FName write SetName;
    // Número de linhas
    property nRows: Integer read FNRows write SetNRows;
    // Número de colunas
    property nCols: Integer read FNCols write FNCols;
    // Elemento como um string
    Property AsString[Index: Integer; Index: Integer]: String Read GetStr Write PutStr;
    // Tipo da matriz
    property MatType: TwsEnumMatType read FMatType write FMatType;
    // Lista com os nomes das colunas
    property CName: TStrings read FCName write FCName;
    // Rótulo
    Property MLab: String Read GetMLab Write PutMLab;

    // Retorna a identificação do grupo se existir
    // A identificação do grupo será extraída do Rótulo da Matriz
    // Ex: MLab = "Grupo: X.Y" retornará o grupo X.Y
    property Group : String read getGroup;

    // Matriz foi modificada?
    Property Modified: Boolean Read FModified;
    // Endereço da linha especificada
    Property Row[index: Integer]: TwsVec Read GetRow Write SetRow;
    // Nome da linha especificada
    property RowName[Index: Integer]: String read GetRowName write SetRowName;
    // Nome da coluna especificada
    property ColName[Index: Integer]: String read GetColName write SetColName;
    // Elemento da matriz
    property Data[Index: Integer; Index: Integer]: Double read Get write Put; default;
    // Retorna se o objeto está bloqueado ou não
    property Locked: Boolean read getLocked;
    // Para futuro armazenamento de informações
    property ExtraData[Index: integer]: byte read getExtraData write setExtraData;

    // Evento que acontece quando o status da matriz muda
    property OnStatusChange : TwsStatusChange_Event read FStatusChange write FStatusChange;

    // Evento que acontece quando um elemento está para ser definido como XML
    property OnSelectElementColor : TwsSelectElementColor_Event read  FSelectElementColor
                                                                write FSelectElementColor;
  end; { TwsMatrix }

  { Herança
      TwsGeneral --> TwsMatrix --> TList
    Objetivo
      Implementar uma classe para matrizes gerais. Uma matriz geral é uma matriz retangular
      que armazena todos os m x n elementos, onde m é o número de linhas e n o número de
      colunas. Além do armazenamento diferenciado, implementa muitos métodos típicos para
      matrizes desse tipo. Como a diferenciação básica dos demais descendentes está na
      quantidade de elementos armazenados, qualquer tipo descendente poderá ser colocado
      neste formato.
  }
  TwsGeneral = class(TwsMatrix)
  Private
    // Gravação em disco de parte de uma matriz geral
    procedure InternalSave3(Writer: TWriter); Override;
    // Recuperação de disco de paret de uma matriz geral
    procedure InternalLoad3(Reader: TReader); Override;
    // Método auxiliar para obtenção da autoestrutura de uma matriz
    procedure Tred2(d, e: TwsVec; EgV: Boolean);
    // Método auxiliar para obtenção da autoestrutura de uma matriz
    procedure TQLI(d, e: TwsVec; EgV: Boolean; var ErrCode: Word);
    // Recuperação de um elemento de uma matriz geral
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
    // Cópia de uma matriz geral para um formato qualquer
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
    // Balanceamento para obtenção de autovalores de matrizes quaisquer
    procedure Balanc;
    // Fatoração LU
    procedure LU(var Indx: TwsLIVec; var d: Double; Var ErrCode:Word);
    // Determinante a partir de um fator LU
    function  LUDet: Double;
    // Constrói a transformação de Givens
    procedure MakeGiv(v1, v2: Double; var c, s, r: Double);
    // Aplica a transformação de Givens
    procedure AppGiv(c, s: Double; var z1, z2: Double);
    // Inversa de uma matriz geral
    procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); Override;
    // Resolve sistema de equações lineares onde a matriz dos coeficientes é geral
    procedure Solve(B: TwsMatrix;var ErrCode: Word); Override;
    // Matriz de covariância a partir da fatoração por Householder
    procedure HFTCov(p: Integer);
    // Matriz de covariância a partir da fatoração por Householder com troca de colunas
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

    // Fatoração por Gram-Schmidt modificado
    procedure MGS(var T: TwsTriangular; var ErrCode: Word);
    // Fatoração por Gram-Schmidt modificado numa métrica distinta da identidade
    procedure WMGS(D: TwsDiagonal; var T: TwsTriangular; var ErrCode: Word);
    // Decomposição por valores singulares
    Procedure SVDCmp(Var D:TwsDiagonal; var V:TwsGeneral; var Rank:Integer; Var ErrCode:Word;
      eps: Double = 1.0e-8);
    // Inversa de Moore Penrose
    function MoorePenrose: TwsGeneral;
    // Localiza linha de um elemento para coluna especificada
    function LocateAtCol(x:Double; Col:Integer; Out j:Integer; Asc:Boolean=True):Boolean;
    // Localiza linha de um elemento para linha especificada
    function LocateAtRow(x:Double; Lin:Integer; Out j:Integer; Asc:Boolean=True):Boolean;
    {$ifdef WINSTAT_FULL}
    // Matriz de correlações
    function Correlations(C: TwsLIVec; var Stat:TwsGeneral;BStat: Boolean;
      var CTest: TwsGeneral): TwsSymmetric;
    // Matriz de correlações incluindo variável peso
    function WCorrelations(C: TwsLIVec; var Stat:TwsGeneral; BStat: Boolean;
      WInd: Integer; var CTest: TwsGeneral): TwsSymmetric;
    // Matriz de correlações parciais
    function PartialCorrel(C,CAdj: TwsLIVec; var Stat: TwsGeneral; BStat:Boolean;
      var CTest: TwsGeneral): TwsSymmetric;
    // Matriz de correlações parciais com pesos
    function WPartialCorrel(C, CAdj: TwsLIVec; var Stat: TwsGeneral; BStat: Boolean;
      WInd: Integer; var CTest: TwsGeneral): TwsSymmetric;
    {$endif}
    // Matriz de produtos cruzados ajustados para a média
    function AdjCrossProd(Col: TwsLIVec; var Mean: TwsVec; var ValObs: Integer):TwsSymmetric;
    // Matriz de produtos (ponderados) cruzados ajustados para a média
    function WAdjCrossProd(Col:TwsLIVec; out Mean: TwsVec; out ValObs: Integer;
      out wa: Double; WIndex: Integer):TwsSymmetric;
    // Matriz de Hessenberg
    function HesQR(MaxIts: Word; var ErrCode: Word): TwsGeneral;
    // Estatísticas de ordem
    Procedure MatOrder(Col: TStringList);
    // Obtem valores da tabela de Tukey
    Function GetTableValue(nTreat, DF: Double): Double;
    // Matriz de médias com chaves ordenadas
    function SortKeyMeans(KeyCol, Col: TwsLIVec; var XRow: Integer): TwsVec;
    // Matriz de médias e somas de quadrados com chaves ordenadas
    function KeyGroupMeans(KeyCol, Col:TwsLIVec; GMean: TwsVec; SSQ: TObject; var XRow,
      M: Integer; Covar,Univ: Boolean): TwsVec;
    // Matriz de médias com chaves quaisquer
    function KeyMeans(KeyCol, Col: TwsLIVec): TwsVec;
    // Todos os valores de uma coluna são iguais?
    function ColConst(k: Integer): Boolean;
    // Inverte uma matriz através da fatoração LU
    function LUInv: TwsGeneral;
    // Resolução de sistemas de equações triangulares
    procedure BackSubst(Indx: TwsLIVec; B: TwsVec);
    // Inverte uma matriz já fatorada na forma LU
    function LUToInv(Indx: TwsLIVec): TwsGeneral;
    // Decomposição de householder
    procedure HouseHolder(p: Integer; out g, h: TwsDFVec; var ErrCode: Word);
    // Fatoração de Householder Lawson & Hanson
    procedure HouseCol(OnlyApply: Boolean; p,l,Col: Integer; h: TwsVec; cStart,nc: Integer);
    procedure HouseForward(xCol: Integer);
  end; { TwsGeneral }

{ Herança
    TwsSymmetric --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes simétrica. Uma matriz simétrica se caracteriza por
    ter elementos iguais em posições de índices trocados, ou seja, os elementos da tringular
    superior são sempre iguais aos da mtTriangular inferior. Assim, nesta implementação, somente
    os elementos da mtTriangular inferior são armazenados. O método de acesso aos elementos
    está preparado para retornar o valor correto, mesmo que ele não esteja eplicitamente
    armazenado.
}
TwsSymmetric = class(TwsMatrix)
Private
  // Escreve um elemento numa martiz simétrica
  procedure Put(i,j: Integer; x: Double); override;
  // Obtém um valor de uma matriz simétrica
  function  Get(i,j: Integer): Double; override;
  // Inverte uma matriz simétrica pelo processo de Cholesky
  procedure CholeskyInv(eps: Double=1.0e-8);
  // Escreve parte de uma matriz simétrica no disco
  procedure InternalSave3(Writer: TWriter); override;
  // Recupera parte de uma matriz simétrica no disco
  procedure InternalLoad3(Reader: TReader); Override;

  function GetBlockName: String; override;

  function GetBlockNameComment: String; override;

Public
  // Cria um objeto para armazenar uma matriz simétrica
  constructor Create(NC: Integer);
  // Lista matriz matriz simétrica na forma de texto
  procedure List(Buffer: TStrings); override;
  // Produto de um escalar por uma matriz simétrica
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst, NewMat:Boolean): TwsMatrix; override;
  // Matriz transposta
  function Transpose: TwsMatrix; override;
  //A'BA
  function TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto ABA' onde B é simétrica
  function TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Cópia de uma matriz simétrica para um tipo qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Fatoração de Cholesky
  function CholeskyFat(var r: Integer; NewMat: Boolean; eps: Double = 1.0e-8): TwsMatrix;
  // Aplica sweep a um conjunto de colunas
  procedure SweepApp(C: TwsLIVec);
  // Aplica sweep a uma sequência de colunas
  procedure SeqSweep(k1, k2: Integer; v: TwsLIVec);
  // Tolerâncias para aplicação do sweep
  procedure Tolerance(F: TwsVec; eps: Double=1.0e-9);
  // Aplica sweep a um conjunto de colunas
  procedure Sweep(Col,v: TwsLIVec);
  // Sweep reversível
  procedure RevSweep(k: Integer; DMin: Double; v: TwsLIVec);
  // Forma de hermite
  function Hermite: TwsGeneral;
  // Transforma uma matriz ja operada pelo sweep numa forma de hermite
  function SweepToHermite(v: TwsLIVec): TwsGeneral;
  // Inversa generalizada G2
  function G2Inv: TwsGeneral;
  // Obtem uma inversa generalizada G2 a partir de uma matriz já operada pelo sweep
  function SweepToG2(v: TwsVec): TwsGeneral;
  // Obtém uma matriz de correlações
  procedure CorrMat;
  // Inverte uma matriz simétrica
  procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); Override;
  // Resolve um sistema de equações simétrico
  procedure Solve(B: TwsMatrix; var ErrCode: Word); Override;
end; { TwsSymmetric }

{ Herança
    TwsDiagonal --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes diagonais. Uma matriz mtDiagonal se caracteriza por
    ter todos os elementos fora da mtDiagonal nulos. Assim, nesta implementação, somente
    os elementos da mtDiagonal são armazenados. O método de acesso aos elementos está preparado
    para retornar o valor correto, mesmo que ele não esteja eplicitamente armazenado.
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
  // Operação de uma matriz mtDiagonal por elemento com uma matriz qualquer
  function ByElement(B:TwsMatrix; Op:TwsEnumTypeOp; NewMat:Boolean;
    var ErrCode:Word):TwsMatrix; override;
  // Produto AB onde A é mtDiagonal e B é uma matriz qualquer
  function Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto Av onde A é mtDiagonal e v um vetor
  function VecMult(v:TwsVec; PreMult:Boolean; Var ErrCode:Word): TwsDFVec; override;
  // Operação de uma matriz mtDiagonal por escalar
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean): TwsMatrix; override;
  // Transposta de uma matriz dagonal
  function Transpose: TwsMatrix; override;
  // Cópia de uma matriz mtDiagonal para uma matriz qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Produto AB' onde A é mtDiagonal e B qualquer
  function TranspMul1(B:TwsMatrix; var ErrCode:Word): TwsMatrix; override;
  // Produto A'B onde A é mtDiagonal e B qualquer
  function TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto AA' onde A é mtDiagonal
  function TranspMul3: TwsMatrix; override;
  // Produto A'A onde A é mtDiagonal
  function TranspMul4: TwsMatrix; override;
  // Produto A'DA onde D é mtDiagonal
  function TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto v´Av onde A é mtDiagonal e v é vetor
  function TranspMul6(v: TwsVec; var ErrCode: Word): Double; override;
  // Produto ABA' onde B é mtDiagonal
  function TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Toma coluna B[k] e faz o produto B[k]'*A*B[k] sendo A mtDiagonal e B qualquer
  function TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double; override;
  // Saída em modo texto de matriz mtDiagonal
  procedure List(Buffer: TStrings); override;
  // Impressão em modo texto de matriz mtDiagonal
  procedure Print(Buffer: TStrings); override;
  // Impressão de matriz mtDiagonal com condição estabelecida para colunas
  procedure ColPrint(const Cols: String; Expr: String; Buffer: TStrings); override;
  // Inverte uma matriz mtDiagonal
  procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); Override;
  // Solucao de sistemas de equacoes lineares onde a matriz dos coeficientes é mtDiagonal
  procedure Solve(B: TwsMatrix; var ErrCode: Word); Override;
  // Elimina colunas especificadas
  procedure DeleteCols(Col: TwsLIVec); override;
  // Mostra matriz numa planilha
  procedure ShowInSheet(Sheet: TBaseSpreadSheetBook); override;

end; { TwsDiagonal }

{ =========================== TwsVandermonde ===============================}

{ Herança
    TwsVandermonde --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes de mtVandermonde. Uma matriz de mtVandermonde se
    caracteriza por possuir colunas obtidas como potências de elementos de uma coluna básica.
    Assim os elemntos da primeira coluna correspondem aos valores desse vetor na potência 0;
    os da segunda na potência 1 (o próprio vetor), e assim por diante. Assim, nesta implementação,
    somente os elementos desse vetor são armazenados. O método de acesso aos elementos
    está preparado para retornar o valor correto, mesmo que ele não esteja eplicitamente
    armazenado.
}
TwsVandermonde = class(TwsMatrix)
Private
  // Escreve um elemento numa matriz de mtVandermonde
  procedure Put(i,j: Integer; x: Double); override;
  // Recupera um elemento de uma matriz de mtVandermonde
  function  Get(i,j: Integer): Double; override;
  // Atribui identificação para linha numa matriz de mtVandermonde
  procedure SetRName(st: TwsCVec;DelChar :TCharSet); override;
  // Obtém identificação para linha numa matriz de mtVandermonde
  function GetRowName(i: Integer): String; override;
  // Grava parte de uma matriz de mtVandermonde em disco
  procedure InternalSave3(Writer: TWriter); override;
  // Pecupera parte de uma matriz de mtVandermonde do disco
  procedure InternalLoad3(Reader: TReader); Override;
  function GetBlockName: String; override;
  function GetBlockNameComment: String; override;
  procedure RowsToXML(Writer: TXML_Writer); override;
Public
  // Somente um vetor é armazenado neste tipo de matriz
  RName: TStrings;
  // Cria um objeto para armazenamento de uma matriz de mtVandermonde
  constructor Create(NC: Integer);
  // Libera espaço ocupado por uma matriz de mtVandermonde
  destructor  Destroy; override;
  // Lista  uma matriz de mtVandermonde no modo texto
  procedure   List(Buffer: TStrings); override;
  // TRansforma uma linha de uma matriz de mtVandermonde num string
  function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; override;
  // Impressão de uma matriz de mtVandermonde com condição imposta
  procedure ColPrint(const Cols: String; Expr: string; Buffer: TStrings); override;
  // Operação de uma matriz de mtVandermonde por elemento
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix; override;
  // Cópia de uma matriz de mtVandermonde para uma matriz de tipo qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Resolve sistema de equações lineaers onde a matriz dos coeficientes é de mtVandermonde
  procedure Solve(B: TwsMatrix; var ErrCode: Word); override;
end; { TwsVandermonde }

{ Herança
    TwsTriangular --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes triangulares inferiores. Uma matriz mtTriangular
    inferior se caracteriza por ter todos os elementos acima da mtDiagonal nulos. Assim, nesta
    implementação, somente os elementos da mtDiagonal e abaixo dela são armazenados. O método
    de acesso aos elementos está preparado para retornar o valor correto, mesmo que ele não
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
  // Lista conteúdo de  uma matriz mtTriangular no formato texto
  procedure   List(Buffer: TStrings); override;
  // Produto AB onde A é uma matriz mtTriangular e B é qualquer
  function Mult(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto AB' onde A é uma matriz mtTriangular e B é qualquer
  function TranspMul1(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto A'B onde A é uma matriz mtTriangular e B é qualquer
  function TranspMul2(B: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Prodtuto AA' onde A é uma matriz mtTriangular
  function TranspMul3: TwsMatrix; override;
  // Produto A'A onde A é uma matriz mtTriangular
  function TranspMul4: TwsMatrix; override;
  // Produto A'BA onde B é uma matriz mtTriangular e A é qualquer
  function TranspMul5(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Produto v'Av onde A é uma matriz mtTriangular e v é um vetor
  function TranspMul6(v: TwsVec; var ErrCode: Word): Double; override;
  // Produto ABA' onde B é uma matriz mtTriangular e A é qualquer
  function TranspMul10(A: TwsMatrix; var ErrCode: Word): TwsMatrix; override;
  // Toma coluna B[k] e faz o produto B[i]'*A*B[k] onde A é uma matriz mtTriangular
  function TranspMul11(B: TwsMatrix; k: Integer; var ErrCode: Word): Double; override;
  // Produto Av onde A é uma matriz mtTriangular e v é um vetor
  function VecMult(v:TwsVec; PreMult:Boolean; Var ErrCode:Word): TwsDFVec; override;
  // Operação de A com escalar onde A é uma matriz mtTriangular
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix; override;
  // Cópia de uma matriz mtTriangular para um formato qualquer
  procedure Copy(MT:TwsEnumMatType; var Matrix:TwsMatrix); override;
  // Inversa de uma matriz mtTriangular
  procedure Inv(var ErrCode: Word; eps: Double=1.0e-8); override;
  procedure FwdSolve(B: TwsGeneral; var ErrCode: Word);
//  function BwdSolve(B: TwsGeneral; var ErrCode: Word): TwsGeneral;

end; { TwsTriangular }

{ ============================== TwsToeplitz ============================ }

{ Herança
    TwsToeplitz --> TwsMatrix --> TList
  Objetivo
    Implementar uma classe para matrizes de mtToeplitz. Existem vários tipos de matrizes de mtToeplitz.
    O formato implementada através desta classe utiliza um vetor de 2*n-1, onde n é o número
    de linhas e colunas, para armazenar todos os elementos necessários. O método de acesso aos
    elementos está preparado para retornar o valor correto, mesmo que ele não esteja eplicitamente
    armazenado. O elemento da posição (i, j) está na posição n-j+i do vetor.
}
TwsToeplitz = class(TwsMatrix)
Private
  // Escreve elemento na posição correta
  procedure Put(i, j: Integer; x: Double); override;
  // Obtém elemento para este tipo de matriz
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
  // Libera espaço ocupado por uma matriz de Toepolitz
  destructor Destroy; override;
  // Saída no modo texto
  procedure List(Buffer: TStrings); override;
  // Produto por escalar numa matriz de mtToeplitz
  function ByScalar(const x:Double; Op:TwsEnumTypeOp; SFirst,NewMat:Boolean):TwsMatrix; override;
  // Copia numa matriz de mtToeplitz em um formato qualquer
  procedure Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix); override;
  // Coloca uma linha em formato string para saída
  function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; override;
  // Imprime uma matriz com imposição de erstrição para colunas
  procedure ColPrint(const Cols: String; Expr: String; Buffer: TStrings); override;
  // Resolve um sistema linear, onde a matriz dos coeficientes é de mtToeplitz
  procedure Solve(B: TwsMatrix; var ErrCode: Word); override;
end; { TwsToeplitz }

{ TwsEnumContrType - Tipo de codifocação para matriz do modelo
    ctHelm   - Contrastes de Helemrt
    ctPolOrt - Polinômios ortogonais
    ctUser   - Contrastes estabelecidos pelo usuário
    ctDev    - Contrastes do tipo diferenças em relação a um nível especificado
    ctExper  - Codificação anulando um nível especificado
    ctBin    - Codificação binária }
  TwsEnumContrType = (ctHelm, ctPolOrt, ctUser, ctDev, ctExper, ctBin);

{ TwsEnumDataType - Tipo variáveis ou de colunas no conjunto de dados
    Numeric   - variável numérica
    Quant     - fator quantitativo
    Qualit    - fator qualitativo
    QualitOrd - fator qualitativo ordenado
    Factors   - fator }
  TwsEnumDataType  = (dtNumeric, dtQuant, dtQualit, dtQualitOrd, dtFactors);

{  TFType - tipo de nível
     tfFixed - níveis fixos
     tfRandom - níveis aleatórios }
  TwsEnumFType = (tfFixed, tfRandom);

  // Forward declaratioms
  TwsDSStructure = Class;
  TListCols      = Class;

{ ============================ TwsDataSetCol ================================}

  {  Herança
       TwsDataSetCol --> TObject
     Objetivos
       TwsDataSetCol é a classe básica para definição dos tipos de coluna de um conjunto de
       dados
   }

  TwsDataSetCol = Class(TObject)
  Private
    FName       : String;             // Nome da coluna
    FLabel      : PString;            // Rótulo
    FColType    : TwsEnumDataType;    // Tipo de coluna
    FSize       : Byte;               // Largura de impressão
    FVersion    : String [10];        // Versão do arquivo
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
    // Libera espaço ocupado por um conjunto de dados
    Destructor  Destroy; Override;
    // Armazenamento em disco
    Procedure  SaveToStream(Writer:TWriter);    Virtual;
    // Leitura de disco
    Procedure  LoadFromStream(Reader:TReader);  Virtual;
    // Cria um rótulo, dependendo do tipo de coluna
    Function   getColTypeAsString: String;
    // Nome da coluna
    Property Name      : String    Read FName       Write SetName;
    // Rótulo da coluna
    Property Lab       : String    Read RLabel      Write WLabel;
    // Tipo de coluna
    Property ColType   : TwsEnumDataType Read FColType    Write FColType;
    // Tamanho de impressão
    Property Size      : Byte      Read FSize       Write FSize;

    // O objeto conectado aqui NÃO É destruído automaticamente
    Property aObject   : TObject   read FObject     write FObject;

    // Indica qual o possuidor desta variável
    Property DataSet : TwsDataSet read FDataSet write FDataSet;

    Property HeaderFont : TFont  read FHeaderFont;
    Property DataFont   : TFont  read FDataFont;
  End; // TwsDataSetCol

{ ============================== TwsNumeric ============================== }

  {  Herança
       TwsNumeric --> TwsDataSetCol --> TObject
     Objetivos
       TwsNumeric é a classe que define uma variável numérica num conjunto de dados. Basicamente
       incorpora a precisão (dígitos significativos) de impressão dos valores numéricos.
   }

  TwsNumeric = Class(TwsDataSetCol)
  Private
    // dígitos significativos
    FPrecision:  Byte;
    // Atribui precisão
    Procedure SetPrecision(APrecision: Byte);
    // Leitura/gravação do objeto
    Procedure SaveToStream(Writer: TWriter);     Override;
    Procedure LoadFromStream(Reader: TReader);   Override;
  Public
    // Cria objeto que armazena estrutura de uma coluna numérica
    Constructor Create(Const aName,aLab: String; fs: byte=15; fp: byte=8);
    // dígitos significativos
    Property  Precision: Byte Read FPrecision Write SetPrecision;
  End; { TwsNumeric }

{ ================================ TwsFactor ================================ }

  {  Herança
       TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsFactor é a classe básica para definição de uma variável do tipo fator num conjunto de
       dados. Incorpora várias características importantes de uma variável desse tipo: os níveis
       e os tipos de níveis. Não pode ser utilizada diretamente
   }
  TwsFactor = Class(TwsDataSetCol)
  Private
    // Lista com os níveis
    FLevels    : TStrings;
    // Tipo de contraste
    FContrType : TwsEnumContrType;
    // Tipo de nível
    FLevelType : TwsEnumFType;
    FContr: TwsGeneral;
    // recupera número de níveis
    Function   GetLevels: Integer;
    // Elimina lista de níveis
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
    // Participa da criação de um objeto do tipo fator
    Constructor Create(Const AName,ALab: String; fs: Byte=15);
    // Libera espaço ocupado por um fator
    Destructor Destroy; Override;
    // Insere um nível na lista de níveis
    Function   AddLevel(Level: String): Integer; Virtual;
    // Retorna o índice do nível especificado
    Function   LevelToIndex(Const N: String): Integer;
    // Remove os níveis que não aparecem como valores na variável
    Procedure  RemoveInvalidLevels;
    // Remove o Nível de Índice "Index"
    Procedure  RemoveLevel(index: Integer); virtual;
    // Remove matriz de contrastes do usuuario
    procedure ClearContr;
    // Tipo de nível na forma de strinng
    Function   getLevelTypeAsString: String;
    // Níveis eliminados?
    Property   LevelsClear: Boolean    Read KillLevels;
    // Lista com os nomes dos níveis
    Property   LevelNames : TStrings   Read FLevels Write FLevels;
    // Tipo de contraste
    Property   ContrType  : TwsEnumContrType Read FContrType Write FContrType;
    // Tipo de contraste como String
    Property   ContrTypeAsString : String Read getContrTypeAsString Write setContrTypeAsString;
    // Tipo de níveis
    Property   LevelType  : TwsEnumFType  Read FLevelType Write FLevelType;
    // Número de níveis
    Property   Levels     : Integer   Read GetLevels;
    // Matriz para codificação dos níveis na análise de modelos lineares
    Property   Contr      : TwsGeneral Read FContr Write SetContr;
  End; { TwsFactor }

{ =============================== TwsQualitative ============================ }

  {  Herança
       TwsQualitative --> TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsQualitative é a classe que define uma variável do tipo fator qualitativo num conjunto
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

  {  Herança
       TwsOrdered --> TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsOrdered é a classe que define uma variável do tipo fator qualitativo ordenado num
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

  {  Herança
       TwsQuantitative --> TwsFactor --> TwsDataSetCol --> TObject
     Objetivos
       TwsQuantitative é a classe que define uma variável do tipo fator quantitativo num conjunto
       de dados. Acrescenta um vetor com os nomes dos níveis
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
    // Libera memória ocupada por fator quantitativo
    Destructor Destroy; Override;
    // Armazena objeto em disco
    Procedure SaveToStream(Writer: TWriter);    Override;
    // Recupera objeto do disco
    Procedure LoadFromStream(Reader: TReader);  Override;
    // Adiciona nível na lista
    Function   AddLevel(Level: String): Integer; Override;
    // Remove o Nível de Índice "Index"
    Procedure  RemoveLevel(index: Integer); Override;
    // Atribui valor de um nível quantitativo
    Procedure SetLevelValue(Const Level: String; Const Value: Double);
    // Recupera um valor do nível quantitativo
    Function  GetLevelValue(Const Level: String): Double;
    // Elimina lista de níveis
    Function   KillLevels: Boolean; override;
    // Vetor com os valores dos níveis
    Property LevelValues: TwsDFVec Read FLevel Write FLevel;
  End; { TwsQuantitative }

{ ---------------------------- TwsListCols ---------------------------------}

{ Objetivo
    Redefine classe TStringList para manter objetos que definem as estruturas das colunas
}
  TListCols = Class(TStringList)
    // Redefine adição de novos objetos
    function AddObject(const S: string; AObject: TObject): Integer; Override;
  End;

{ ---------------------------- TwsDSStructured ------------------------------

  {  Herança
       TwsDSStructure --> TObject
     Objetivos
       Gerencia a estrutura de um conjunto de dados. Diferentemente de uma matriz geral, um
       conjunto de dados pode modificar os atributos das colunas. Estrutura de um conjunto de
       dados são as informações relativas às colunas do conjunto: seu tipo, nome, etc.
       Facilita os processos de criação, inclusão e exclusão de colunas
   }
  TwsDSStructure = Class(TObject)
  Private
    // Número de fatores
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
    // Libera espaço ocupado pelo objeto
    Destructor Destroy; Override;
    // Copia a estrutura inteira
    function Copy: TwsDSStructure;
    // Adiciona uma cópia de uma coluna já existente (Vetores)
    Procedure AddCopyCol(Col: TwsDataSetCol); {Rochedo} {23/09/97}
    // Adiciona uma coluna adicionando valores (Vetores)
    Procedure  AddCol(Col: TwsDataSetCol); {Rochedo} {23/09/97}
    // Adiciona uma coluna sem adicionar os espacos para os valores (Vetores)
    Procedure  AddColEx(Col: TwsDataSetCol); {Rochedo} {04/04/98}
    // Adiciona uma variável numérica
    Procedure  AddNumeric      (Const N, Lab :String;ps: byte=15; pf: byte=8);
    // Adiciona um fator qualitativo
    Procedure  AddQualitative  (Const N, Lab :String; ps: byte=15);
    // Adiciona um fator quantitativo
    Procedure  AddQuantitative (Const N, Lab :String; ps: byte=15);
    // Adiciona um fator qualitativo ordenado
    Procedure  AddOrdered      (Const N, Lab :String; ps: byte=15);
    // Elimina a especificação dos artibutos de uma coluna
    Procedure  Delete          (const IndexCol: Integer); overload;
    // Elimina a especificação dos artibutos de uma coluna
    Procedure  Delete          (Const N: String); overload;
    // Retorna o endereço de uma coluna pelo seu ome
    Function   ColByName       (Const N: String): TwsDataSetCol;
    // Retorna o índice de uma coluna pelo seu ome
    Function   IndexOf         (Const N: String): Integer;
    // Retorna um vetor com os índices de uma lista de colunas ou variáveis
    Function   IndexCols       (Const Cols: String): TwsLIVec;
    // Coluna pelo índice
    Property   Col[index : Integer]: TwsDataSetCol Read GetCol Write SetCol;
    // Quantas colunas do tipo fator
    Property   Factors: Integer Read GetFactors;
  End; { TwsDSStructure }


  TDS_ChooseFont_Event = procedure(Sender: TwsDataSet; R, C: Integer;
                                   Font: TFont; out UseThisFont: Boolean) of object;

  {  Herança
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
    // Evento para definição da fonte correntemente usada
    FChooseFont: TDS_ChooseFont_Event;
    FStruct: TwsDSStructure;
    // Recupera parte do conteúdo do disco
    Procedure InternalLoad3(Reader:TReader);       Override;
    // Armazena parte do conteúdo em disco
    Procedure InternalSave3(Writer: TWriter);      Override;
    // Armazena definição das colunas no disco
    Procedure CNameSaveToStream(Writer:TWriter);   Override;
    // Recupera definição das colunas do disco
    Procedure CNameLoadFromStream(Reader:TReader); Override;
    // Atribui nome do conjunto
    procedure SetName(Const Name: String); Override;
    // Escreve valor em posição especificada
    Procedure PutStr(i,j: Integer; x: String); override;
    // Coloca conteúdo de linha em srting
    function ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer; Override;
    // Cabeçalho
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
    // Obtém endereço do conjunto de dados
    Function  GetDataSet: TwsDataSet; Virtual;
    // Recupera elemento de qualquer tipo
    function  GetVar(R, C: Integer): Variant;
    // Escreve elemento de qualquer tipo
    procedure PutVar(R, C: Integer; const Value: Variant);  {Rochedo, 25/05/1998}
    // Obtém elemento livre de espaços em branco
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
    // Retorna sim ou não se a descrição das variáveis deve ser impressa
    function PrintDescVars: Boolean; override;

    // Descreve as colunas como xml
    procedure StructColsToXML(Buffer: TStrings; var Ident: Integer); override;

    // Obtem os valores que serão colocados nas células da planilha
    procedure GetSheetValue(L, C: Integer; var IsNUM: Boolean;
                                           var sValue: String;
                                           var rValue: Real); virtual;
    {$ifdef MSXML}
    procedure fromXML_LoadStruct(no: IXMLDomNode); override;
    {$endif}
  Public
    // Cria um conjunto de dados
    Constructor Create(aname : String='NoName');
    // Cria um conjunto de dados onde todos os dados são numéricos
    // Fill indica se queremos inicializar o conjunto com zeros
    Constructor CreateNumeric(aName: String; nRows, nCols: integer);
    // Cria conjunto de dados com uma estrutura ja pronta
    Constructor CreateStructured(aname : String; nr:Integer; Struct: TwsDSStructure);
    // Cria conjunto de dados a partir de um array de colunas ja disponivel
    Constructor CreateFix(aname : String; nr:Integer; ColT: Array Of TwsEnumDataType);
    // Libera área ocupada por um conjunto de dados
    Destructor Destroy(); Override;
    // Retorna uma instancia lida de uma secao de um arquivo do tipo ini
    // Por enquanto só lê colunas numéricas
    class function Load(Ini: TMemIniFile; const Section: string): TwsDataSet;

    // Salva a instancia em uma secao de um arquivo do tipo ini
    // Por enquanto só salva colunas numéricas
    procedure Save(Ini: TMemIniFile; const Section: string);

    // Redimensiona a quantidade de linhas do DataSet
    procedure ForceRowsCount(Count: Integer);

    // Prepara e preenche a estrutura com um determinado valor.
    // Se a estrutura foi criada mas não existem dados, Fill cria os dados e inicializa-os.
    // Deverá, logicamente, ser chamado sempre após a definição da estrutura dos dados.
    procedure Fill(const x: Double);
    // Procura uma data utilizando busca binaria.
    // Os dados da coluna precisam estar ordenados
    // Retorna Falso se o valor nao for encontrado
    // DateTime informa se devemos considerar a parte horaria da data
    function FindDate(const D: TDateTime; inCol: integer; DateTime: boolean; var Index: Integer): Boolean;

    // Obtem índices de colunas a partir de nomes num string
    function  IndexColsFromString(const Cols: String): TwsLIVec; Override;
    // Obtem índices de colunas a partir de nomes numa lista de strings
    Function  IndexColsFromStrings(Cols: TStrings): TwsLIVec; Override;
    // Colunas a imprimir
    Function  ColsToImp(var k: Integer): Integer; override;
    // String e nome de uma coluna?
    Function  Isfactor(Const S: String): Boolean;
    // Cria uma cópia do conjunto de dados
    Function  Copy: TwsDataSet; Virtual;
    // Cria uma cópia de colunas especificas do conjunto de dados
    Function  CopyByCols(Col: TwsLIVec): TwsDataSet; Virtual;
    // Imprime conjunto de dados
    Procedure Print(Buffer: TStrings); override;

    {$ifdef Planilha}
    //Mostra o conteúdo de um DataSet em uma planilha
    procedure ShowInSheet(Sheet: TBaseSpreadSheetBook); override;

    //Escreve na Linha 1, Coluna <ColSheet> o nome da coluna de índice <indexCol>
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

    // Obtém médias e outras estatísticas para níveis de fatores especificados
    function MeanFactor(IFac:TwsLIVec; VIdx:Integer; var GMean,Min,Max:Double):TwsGeneral;
    // Obtém médias para níveis de fatores especificados para amplitude de linhas
    function FacMeans(Fac,IRow,LRow: Integer; Col: TwsLIVec): TwsGeneral;
    // Obtém médias e variâncias ponderadas para níveis de fatores especificados para amplitude de linhas
    function wFacVarMean(Fac: TwsLIVec; IRow,LRow,WInd: Integer;Col: TwsLIVec): TwsGeneral;
    // Obtém médias e variâncias para níveis de fatores especificados para amplitude de linhas
    function FacVarMean(Fac: TwsLIVec; IRow,LRow: Integer; Col: TwsLIVec): TwsGeneral;
    // Adiciona coluna
    Procedure AddCol(Const N, Lab: String; ColType: TwsEnumDataType; ps: byte=15; pf: byte=8); Virtual;
    // Adiciona coluna com descritor
    Procedure AddColDesc(Obj: TwsDataSetCol);  Virtual;
    // Remove uma coluna
    procedure DeleteCol(i: Integer); Override;
    // Remove as colunas referenciadas pelos índices passados pelo vetor
    procedure DeleteCols(Col: TwsLIVec); override;
    // Nome da coluna identificadora
    Property ColIdentName: String Read FColIdentName Write SetColIdentName;
    // Retorna elemento especificado como strinng
    Property AsString [Index: Integer; Index: Integer]: String Read GetStr Write PutStr;
    // Retorna elemento como variante
    Property AsVariant[Index: Integer; Index: Integer]: Variant Read GetVar Write PutVar;
    // Retorna qualquer valor do DataSet na forma de uma String sem espaços
    Property AsTrimString [Index: Integer; Index: Integer]: String Read GetTrimStr;
    // Retorna elemento como inteiro
    Property AsInteger[Index: Integer; Index: Integer]: Integer Read GetInt;
    //Retorna valores no formato Data/Tempo
    Property AsDateTime[Index: Integer; Index: Integer]: TDateTime Read GetDT;
    // Nesta classe retorna a própria instância do objeto
    Property AsDataSet: TwsDataSet Read GetDataSet;

    // Estrutra das colunas
    Property Struct: TwsDSStructure read FStruct write SetStruct;

    // Eventos
    property OnChooseFont : TDS_ChooseFont_Event read FChooseFont write FChooseFont;
  End; 

{ ============================= TwsOrderedDataSet ========================== }

  {  Herança
       TwsOrderedDataSet --> TwsDataSet --> TwsGeneral --> TwsMatrix --> TList
     Objetivos
       Implmenta conjuntos de dados com um número limitado de linhas que estão ordenadas
       (em ordem ascendente ou descendente) em relação a uma coluna especificada.
   }
  TwsOrderedDataSet = Class (TwsDataSet)
  private
    // True se as ordens das linhas for ascendente
    FAscendent : Boolean;
    // Número máximo de linhas para o conjunto
    FMaxRows   : Integer;
    // Coluna utilizada para ordenação
    FBasedCol  : Integer;
  Public
    // Cria objeto para armazenar conjuntos de dados ordenados
    Constructor Create(aName: String; aMaxRows, aBasedCol: Integer; Ascend: Boolean);
    // Insere uma linha repeitando a ordenação
    Procedure MAdd(L: TwsVec); Override;
    // Número máximo de linhas
    property MaxRows:   Integer read FMaxRows   write FMaxRows;
    // Coluna para ordenação
    property BasedCol:  Integer read FBasedCol  write FBasedCol;
    // Se True, a ordem das linhas será ascendentes
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
    // obtem somas de quadrados de resíduos
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
  // Matriz na potência
  function MatPower(A: TwsMatrix; n: Integer; var ErrCode: Word): TwsMatrix;
  // Produto que substitui uma matriz
  function MultToA(var A: TwsMatrix; B: TwsMatrix; var ErrCode: Word): TwsMatrix;
  // Transforma string numa matriz
  function StrToMat(LN,CN,P: TwsCVec; MT: TwsEnumMatType; ChLin, ChEnd: Char): TwsMatrix;
  // Transforma string numa matriz geral
  function GStrMat(P: String): TwsGeneral;

  // Aplica função a matriz
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
  // Operação por elemento especial
  procedure ElemOp1(A,B:TwsMatrix; Op:TwsEnumTypeOp; var C:TwsGeneral);
  // Operação por elemento especial
  procedure ElemOp2(A:TwsMatrix; v:TwsVec; Op:TwsEnumTypeOp; DFirst:Boolean; var C:TwsMatrix);
  // Operação por elemento especial
  procedure ElemOp3(A,B:TwsMatrix; Op:TwsEnumTypeOp; var C:TwsSymmetric);
  // Inversão de matrizes
  function InvMat(y: TwsMatrix; NewMat: Boolean; var ErrCode: Word): TwsMatrix;
  // Solução de sistemas por Gauss-Jordan
  procedure GJSolve(A, B: TwsGeneral; var ErrCode: Word);
  // Solução de sistemas por SVD
  procedure SVDSolve(var A, B: TwsGeneral);
  // Solução de sistemas por Householder
  procedure HFTSolve(var A, B, X: TwsGeneral; var H: TwsVec; var ErrCode: Word);
  // Solução de sistemas por Householder com troca de colunas
  procedure HFTISolve(var A, B, X: TwsGeneral; var r: Integer; var Indx: TwsLIVec;
    var G, H: TwsVec; var ErrCode: word);
  //// Solução de sistemas através da decomposição LU
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

  // Codificação experimental
  function Experim(nr: Integer): TwsGeneral;
  // Contrastes para análise de perfil
  function Profile(nr: Integer): TwsGeneral;
  // Matriz de reparametrizacao por meio de perfis
  function LProfile(nr: Integer): TwsGeneral;

  // Contraste nível contra média dos demais
  function MeanTransf(nr: Integer): TwsGeneral;
   // Matriz de reparametrizacao - contraste nível contra média dos demais
  function LMeanTransf(nr: Integer): TwsGeneral;

  // Polinômios ortogonais
  function PolOrth(x: TwsVec;m: Integer): TwsGeneral;
  // Matriz de reparametrizacao utilizando polinomios ortogonais
  function LPolOrth(x: TwsVec; m: Integer): TwsGeneral;

  // Polinômios ortogonais ponderados
  function WOrthPol(x, w: TwsVec;m: Integer): TwsGeneral;
  // Ajustamento por polinômios ortogonais
  function WPolinFit(var beta: TwsTriangular;var x,w,y,sq: TwsVec;m: Integer): TwsGeneral;
  // Matriz de Hilbert
  function Hilbert(nc: Integer): TwsGeneral;
  // Matriz de constantes
  function Jota(nr, nc: Integer;MT: TwsEnumMatType;x: Double): TwsMatrix;
  // Construção da tabela de Duncan
  function DuncanTable(ntreat: Integer; df: TwsVec): TwsGeneral;
  {$ifdef WINSTAT_FULL}
  // Acesso a tabela de Tukey
  function TukeyTable(nTreat: Integer; df, Alpha: TwsVec): TwsGeneral;
  {$endif}
  // Concatenação de vetor com vetor
  function HVecVecConcat(v1,v2: TwsVec;var Erro: Word): TwsGeneral; { Alex 09/10/97 }
  // Transforma um PChar numa matriz
  function CharToMat(p:PChar; MT: TwsEnumMatType): TwsMatrix;{ Alex 09/10/97}
  // Atribui nomes de colunas
  procedure SetColName(A: TwsMatrix;C:PChar); { Alex 09/10/97 }
  // estatísticas (Rochedo)
  function Statistics(A: TwsGeneral; const Col: Array of byte;
    const Stat: Array of TwsEnumStatistics): TwsGeneral;
  // Soma de produtos igual a zero
  function IsOrthogonal(Y: TwsGeneral; w: TwsVec; j,k: Integer): boolean;
  // Verifica se a soma é zero
  function IsContrast(Y: TwsGeneral; k: Integer): boolean;
  // Verifica se todas as colunas definem pares de contrastes ortogonais
  function AreOrthogonal(Y: TwsGeneral; var ColI, ColJ: Integer): Boolean;
  // Verifica se todas as colunas definem contrastes
  function AreContrast(Y: TwsGeneral; var Col: Integer): boolean;

  Function wsMatrixMax  (M: TwsGeneral; Col, Ini, Fim: Longint): Double;
  Function wsMatrixMin  (M: TwsGeneral; Col, Ini, Fim: Longint): Double;
  Function wsMatrixMean (M: TwsGeneral; Col, Ini, Fim: Longint): Double;

{
 A ausência do parâmetro Media significa que a rotina terá que calcular a media
 EX:
    res := wsMatrixDSP(y, 3, 1, 10); --> a rotina terá que calcular a media
    res := wsMatrixDSP(y, 3, 1, 10, 4.55); --> a rotina usará a média ja calculada 4.55
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
    Criar e inicializar um objeto do tipo TwsMatrix. Esta classe é um tipo básico de matriz e não
    deve ser utilizada diretamente. Somemnte os descendentes criam matrizes completas.
  Parâmetros
    NR: Número de linhas da matriz
    NC: Número de colunas. Se nc=0 nenhum nome de coluna será criado
  Métodos chamados
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
    TStringList(FCName).Duplicates := dupError; {Não permite colunas repetidas}
    for i := 1 to FNCols do FCName.Add('Col' + IntToStr(i));
    End;
  FName := '';
  Mlab:='';
end; { TwsMatrix.Create }

destructor TwsMatrix.Destroy;
{ Objetivo
    Liberar memória utilizada pelo objeto da classe
  Campos liberados
    FCName
    Linhas (vetores)
    MLab
  Métodos chamados
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
  Result := 'Estilo de formatação para Matrizes do tipo TwsMatrix';
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
    Grava a primeira parte da estrutura de uma matriz (ver comentarios código)
  Parâmetros
    Writer: Objeto de escrita em disco
  Métodos chamados
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
    Grava a segunda parte da estrutura de uma matriz (ver comentarios código)
  Parâmetros
    Writer: Objeto de escrita em disco
  Métodos chamados
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
   Parâmetros
     Name: Nome que serã atribuído à matriz
   Métodos chamados
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
    Atribui um tamanho para impressão das colunas da matriz
  Parâmetros
    Value: largura de impressão das colunas
  Métodos chamados
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
     Recupera rótulo da matriz
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
}
Begin
  Result:=FMLab^;
End; { TwsMatrix.GetMLab }

Procedure TwsMatrix.PutMLab(Const l:String);
{ Objetivo
    Atribui o rótulo da matriz
 Parâmetros
    l: Rótulo a ser atribuído
}
Begin
  FModified := True;
  If FMLab <> Nil Then DisposeStr(FMLab);
  FMLab := NewStr(l);
End; { TwsMatrix.PutMLab }

Class Function TwsMatrix.ObjectFromStream(Reader: TReader; Var Erro: Integer): TwsMatrix;
 { Objetivo
    Recuperar um objeto TwsMatrix armazenado em disco. Este método não deverpá ser utilizado
    diretamente.
  Parâmetros
    Reader: Objeto responsável pela leitura
    Erro: Código de erro. NReadError: Erro de leitura
  Métodos chamados
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
   Parâmetros
     FuileName: nome do arquivo
  Métodos chamados
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
  Parâmetros
    Reader: Objeto responsável pela leitura
  Métodos chamados
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
    TStringList(FCName).Duplicates := dupError; {Não permite colunas repetidas}
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
  Parâmetros
    Writer: Objeto responsável pela escrita dos nomes das colunas (armazenadas em CName) no arquivo
  Métodos chamados
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
    Gravar o objeto num arquivo com o nome especificado. É o método que será utiizado para
    armazenar objetos da classe TwsMatrix
  Parâmetros:
    Name: Nome do arquivo onde o objeto será armazenado
  Métodos chamados
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
      InternalSave1(Writer); // Estático
      InternalSave2(Writer); // Estático
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
    Retorna um string com os índices dos nomes de colunas especificados.
    Os caracteres separadores dos nomes são [#9,#10,#13,' ',',','=','\', '"','(',')'].
    O caracter '-' indica amplitude, ou seja, todas as colunas que estão entre as duas especificadas.
  Parâmetros
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
    Constroi a lista de nomes de colunas especificados. Se já existe uma lista de nomes,
    ela sera substituida. Se a quantidade de nomes não for suficiente, os nomes serão
    completados com 'Col'+número da coluna
  Parâmetros
    st     : Objeto string com os nomes
    DelChar: Conjunto de caracteres separadores dos nomes
  Métodos chamados
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
  // Completa se for necessário
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
    Estabelece o número de linhas da matriz
  Parâmetros
    NR: Número de linhas
  Métodos chamados
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
    Retorna True se o tipo da matriz é o especificado
  Parâmetros
    AType: Tipo para verfificação
  Métodos chamados
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
  Parâmetros
    Op: Operador desejado. Os operadores disponíveis são:
        opSum,opSub,opDiv,opProd,opPower  // Operadores aritmeticos
        opGE,opGT,opLE,opLT,opEQ,opNE     // Comparacao
        opOR,opAnd                        // Logicos
        opMax,opMin                       // Maximo, minimo
    NewMat  : True, se o resultado virá numa nova matriz, False para o resultado retornar na
      própria matriz. Uma nova matriz será criada sempre que o tipo da matriz que chama for
      diferente da matriz geral. As exceções ficam para o caso de ambas matrizes simétricas
      (qualquer operador) e ambas matrizes diagonais (operador opProd)
    ErrCode: Retorna 0 se a operação for possível (matrizes de mesma ordem) e NImprDim -
      dimensões impróprias para a operação - caso contrário.
    Retorno
      Matriz com o resultado. O retorno na própria matriz será possível quando a matriz que
      chama é geral, quando ambas são simétricas (retorna simétrica) ou quando ambas são diagonais
    Valores perdidos
      Não trata
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
    Retorna a transposta da matriz que chama o método
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
    Retorna o produto entre duas matrizes. Utilizado quando a matriz que chama o método é geral,
    simétrica, mtVandermonde ou mtToeplitz
  Parâmetros
    B      : Matriz para o (pos)produto com a matriz que chama o método
    ErrCode: Código de erro. O produto somente sera possível (ErrCode retorna 0) se o
      número de colunas de quem chama é igual ao número de linhas de B; caso contrário
      ErrCode retorna a constante NImprDim (Dimensões impróprias para a operação) e a
      função retorna nil
  Retorno
    Retorna sempre uma matriz geral
  Valores perdidos
    Não trata
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
  Parâmetros
    X: Matriz com a qual será feito o produto de Kronecker
  Retorno
    Se A é a matriz m x n que chama o método e X uma matriz p x q este método retorna uma
    matriz geral mp x nq com o produto de Kronecker
  Valores perdidos
    Produto inválido é considerado valor perdido
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
    Obtém o produto de Kronecker de uma matriz por um vetor
  Parâmetros
    x: Vetor para o produto
  Retorno
    Se a matriz que chama é de ordem m x n e o vetor é de ordem k então o retorno é uma
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
  Parâmetros
    X: matriz para o produto
    i,j: Índices das colunas da matriz e de X, respectivamente
  Retorno
    Se a matriz que chama é de ordem m x n e X é de ordem p x q, retorna um vetor (TwsDFVec)
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
  Parâmetros
    X: Matriz para realização do produto
    ErrCode: Código de erro. Retorna 0 se o número de linhas da matriz que chama é igual ao
    número de linhas de X; NImprDim (dimensões impróprias para a operação) caso contrário
  Retorno
    Se A (com dimensão m x n) é a matriz que chama e X é de ordem p x q retorna uma matriz
    geral de dimensão m x nq
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
 Parâmatros
   A: Vetor para o produto
   PreMult: True se o vetor premultiplicar a matriz; False caso contrario
   ErrCode: Código de erro. Retorna 0 se o produto for possível e NImprDim (dimensões
     impróprias para a operação) se a) número de componentes do vetor diferente do número
     de linhas da matriz (PreMult=True); b) número de colunas da matriz diferente do número
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
    Aplica funções matemáticas aos elementos da matriz
  Parâmetros
    fOp: Função desejada. As possibilidades são:
      cABS      - valor absoluto
      cEXP      - exponencial
      cAPROXIMA - Aproxima valores dentro de um limite pré-estabelecido
      cINT      - Parte inteira do valor
      cLN       - Logaritmo neperiano
      cRAIZ     - Raiz quadrada
      cARCTAN   - Arco tangente
      cARCSEN   - Arco seno
      cARCCOS   - Arco cosseno
      cSEN      - Seno
      cCOS      - Cosseno
      cSENH     - Seno hiperbólico
      cCOSH     - Cosseno hiperbólico
      cTAN      - Tangente
      cLOG      - Logaritmo decimal
      cANG      - Transformação angular
      cLGAMA    - Logaritmo da função gama
      cTGAMA    - Derivada da digamma
      cFLOOR    - Maior inteiro
      cCEIL     - Menor inteiro
      cINV      - Inverso do valor
      cFRAC     - Parte fracionária
      cTANH     - Tangente hiperbólica
      cAcum     - Valores acumulados
    NewMat: True se o retorno se dará em outra matriz; False se o retorno será na própria
      matriz. Parâmetro considerado somente se matriz for geral.
  Valores perdidos
    Funções aplicadas a valores impróprios ou perdidos retornam valores perdidos
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
  Parâmetros
    L, C: Linha e coluna, respectivamente, do elemento
    x   : retorna o valor do elemento
  Retorno
    True se x é um valor perdido
}
var
  aux: Double;
begin
  x := Get(L, C);
  Result:=wsGLib.IsMissValue(x)
end;

Function TwsMatrix.IndexColsFromStrings(Cols: TStrings): TwsLIVec;
{ Objetivo
    Vetor de índices de colunas especificadas
  Parâmetros
    Cols: Lista com os nomes das colunas
  Métodos chamados
    IndexOf
  Campos modificados
    Nenhum
  Observações
    Ocorre uma exceção se algum nome da lista não for nome de coluna
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
                                 'Método: %s'#13 +
                                 'Erro: Variável < %s > não existe',
                                 [ClassName, 'IndexColsFromStrings', Cols[i]]);
       End;
    End;
End; {IndexColsFromStrings}

function TwsMatrix.IndexColsFromString(const Col: String): TwsLIVec;
{ Objetivo
    Retorna um array de índices de colunas especificadas na forma de um string
  Parâmetros
    Col: string com os nomes de colunas
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Result := wsIndexCols(Col, FCName);
end; { IndexColsFromString }

Function TwsMatrix.IndexByName(Const Name : String) : Integer;
{ Objetivo
    Dado um nome de coluna, retorna o seu índice
  Parâmetros
    Name: Nome da coluna para obtençào do índice
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observação
    Ocorre uma exceção se o nome especificado não for nome de nenhuma coluna
}
Begin
  Result := FCName.IndexOf(Name);
  If Result > -1 Then
     Inc(Result)
  Else
     Raise Exception.CreateFmt('Classe: %s'#13 +
                               'Método: %s'#13 +
                               MsgInvalidNameVar,
                               [ClassName, 'IndexByName', Name])
End;

function TwsMatrix.GetRow(i: Integer): TwsVec;
{ Objetivo
    Dado um índice, retorna o endereço da linha
  Parâmetros
    i: Índice da linha. Baseado em 1.
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Result := TwsVec(FList[i-1]);
end;

Procedure TwsMatrix.SetRow(i: Integer; v: TwsVec);
{ Objetivo
    Atribui um vetor como linha da matriz numa posição especificada. A linha que existe na posição
    será descartada.
  Parâmetros
    i: Posição onde será colocada a linha
    v: Vetor que passará a ser a linha na posição i
  Métodos chamados
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
      Raise Exception.Create('Linha inválida passada para a matriz ' + Name);
End; { TwsMatrix.SetRow }

procedure TwsMatrix.SetRName(st: TwsCVec; DelChar :TCharSet);
{ Objetivo
  Cria nomes de linhas a partir de um objeto string
Parâmetros
  st     : objeto string com os nomes de linhas
  DelChar: Conjunto de caracteres separadores de nomes de linhas.
  Métodos chamados
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
  Parâmetros
    i: Índice da linha para obtenção do nome
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Result := Row[i].Name;
end;

procedure TwsMatrix.SetRowName(i: Integer; const s: string);
{ Objetivo
    Atribui nome à linha com índice especificado e atualiza PrintOptions.MaxIdSize, se necessário.
  Parâmetros
    i: Índice da linha que terá nome atribuído
    s: Nome da linha
  Métodos chamados
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
    Retorna o nome da coluna de índice especificado
  i: Índice da coluna
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
}
begin
  Try
    Result := FCName[i-1];
  Except
    Result := 'Índice de coluna incorreto';
  End;
end;

procedure TwsMatrix.SetColName(i: Integer; S: string);
{ Objetivo
    Atribui nome à coluna especificada
  Parâmetros
    i: Coluna para a qual o nome será atribuído
    s: Nome da coluna
  Métodos chamados
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
                               'Erro: Índice de coluna [%d] inválido',
                               [ClassName, 'ColName', i]);
end;

function TwsMatrix.CopyCol(Col: Integer; IMiss: Boolean=False): TwsVec;
{ Objetivo
    Retorna um vetor com os valores da coluna especificada.
  Parâmetros
    Col  : Coluna a ser copiada no vetor
    IMiss: True se inclui os valores perdidos na copia; False caso contrario
  Métodos chamados
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
  Parâmetros
    Col: Indices das colunas que serao copiadas para a lista
    IMiss: True se inclui os valores perdidos na copia; False caso contrario
  Métodos chamados
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
    Concatena uma linha à matriz.
  Parâmetros
    L: Linha a ser concatenada
  Métodos chamados
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
    Troca os endereços de duas linhas
  Parâmetros
    i, j: Linhas que terão seus endereços trocados
  Métodos chamados
    Nenhum
  Campos modificados
    Endereços das linhas
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
    Insere uma linha na posição especificada
  Parâmetros
    Pos: Posição que será ocupada pela nova linha
    L: Linha que será inserida
  Métodos chamados
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
  Parâmetros
    L: Linha da matriz que será eliminada.
  Métodos chamados
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
  Parâmetros
    i: Índice da linha a substituir. Baseado em 1.
    v: Vetor que substituirá a linha
  Métodos chamados
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
  Parâmetros
    Col: Índices das colunas a eliminar
  Métodos chamados
    MChange
  Campos modificados
    FNCols
  Observação
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

     // Verifica quais variáveis vão permanecer
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
    Lista descritores pertinentes à classe TwsMatrix
  Métodos chamados
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
  Parâmetros
    i, j: índices de linha e coluna do elemento
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observação
    Retorna ´.´ se for valor perdido. Aplica a função Fuzz para eliminar dígitos que estão
    fora da precisão especificada.
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
     Atribui valor recebido em formato string à posição especificada
   Parâmetros
     i, j: índices de linha e coluna
  Métodos chamados
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
  Parâmetros
    L: Linha da matriz
    Start: Índice do primeiro valor que será colocado no string
    Amt: Quantos valores serão colocados no string
  Métodos chamados
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
    Transforma elementos especificados de uma linha num string na sequência indicada pelos índices informados num vetor
  Parâmetros
    L: Linha da matriz
    Start: Índice do primeiro valor que será colocado no string
    Amt: Quantos valores serão colocados no string
    Col: Vetor com os índices dos elementos a serem colocados no string
  Métodos chamados
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
  Parâmetros
    Start: Índice da coluna inicial
    Amt: Número de colunas a listar
  Métodos chamados
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
  Parâmetros
    Col: Vetor com os índices das colunas desejadas
  Métodos chamados
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
    Retorna o número de colunas que poderão ser impressas. Considera a largura total e a de cada
    elemento na impressão
  Parâmetros
    k: Na entrada indica a última coluna já impressa. Na saída corresponde ao índice da última
       coluna que será impressa.
  Métodos chamados
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
    Retorna o número de colunas que poderão ser impressas, dentre as especificadas por um vetor.
    Considera a largura total e a de cada elemento na impressão
  Parâmetros
    k  : Na entrada indica as colunas já impressas. Na saída corresponde ao índice da última
         coluna que será impressa.
    Col: Vetor que indica quais colunas serão impressas
  Métodos chamados
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
  Métodos chamados
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
    Copia parte da matriz. Descendentes implementam restante da cópia
  Parâmetros
    MT: tipo para a qual será copiada a matriz. MT deverá um dos tipos
      mtGeneral
      mtSymmetric
      mtDiagonal
      mtTriangular
      mtToeplitz
      mtVandermonde
    Matrix: local onde retorna a cópia
  Métodos chamados
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
    Faz o produto da matriz (geral, simétrica, mtVandermonde ou mtToeplitz) pela transposta da
    outra sem que a transposição seja explicitamente realizada
  Parâmetros
    B      : Matriz que será transposta para o produto
    ErrCode: Código de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contrário
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Se A é a matriz que chama o método, retorna uma matriz geral com o produt AB'
  Valores perdidos
    Não trata
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
    Produto de uma matriz qualquer transposta por uma outra sem que a transposição seja
    explicitamente realizada
  Parâmetros
    B: Matriz com a qual o produto será realizado
    ErrCode: Código de erro. retorna 0 (zero) se NRows = B.NRows e NImprDim (dimensões
    impróprias para a operação), caso contrário
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Sempre uma matriz geral
  Valores perdidos
    Não considera
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
    Faz o produto da matriz (geral, simétrica, mtVandermonde ou mtToeplitz) pela sua transposta
    sem que a transposição seja explicitamente realizada
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Retorna a matriz simétrica AA', exceto se for mtDiagonal. Nesse caso, o retorno é uma
    matriz mtDiagonal
  Valores perdidos
    Não trata
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
    Produto de uma matriz transposta por si mesma sem que a transposição seja explicitamente
    realizada
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno
    Retorna uma matriz simétrica exceto no caso em que a matriz é mtDiagonal
  Valores perdidos
    Não considera
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
    Dadas as matrizes A e B (que chama o método), retorna o produto A'BA sem efetuar
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
  Parâmetros
    v: Vetor para o produto
    ErrCode: Código de erro. Se v.Len=NRows=NCols retorna 0; caso contrário retorna
    NImprDim (dimensões impropróprias para a operação
  Retorno
    Se A é a matriz que chama retorna o escalar correspondente ao produto v'Av; se o produto
    não for possível retorna MissValue
  Valores perdidos
    Não trata
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
    Obtém o produto de uma submatriz (transposta) de uma matriz pela própria submatriz
  Parâmetros
    Lin1, Lin2: Linha inicial e linha final para obtenção da submatriz
  Retorno
    Matriz simétrica correspondente ao produto
  Valores perdidos
    Não trata
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
    Se A é a matriz que chama o método, obtém o produto A'B mas iniciando somente na coluna
    start de B.
  Parâmetros
    B: matriz com a qual será feito o produto
    Start: Coluna de B por o produto iniciará.
    ErrCode: Código de erro. Retorna 0 se NRows = B.NRows; NImprDim caso contrário
    (dimensões impróprias para a operação)
  Retorno
    Retorna uma matriz geral
  Valores perdidos
    Não trata
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
 Parâmetros
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
    Dadas as matrizes A e B (que chama o método), retorna o produto ABA' sem efetuar
      a transposicao explicitamente. Na chamada B.TranspMul10(A,.) B é uma matriz geral,
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
    é a coluna.
  Parâmetros
    B: Matriz para o produto
    k: Índice da coluna para o produto
    ErrCode: Código de erro. Se NRows=NCols=A.NRows retorna 0; caso contrário retorna
      NImprDim (dimensões impropróprias para a operação
  Retorno
    Se o produto não for possível, retorna wscMissValue
  Valores perdidos
    Não trata
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
    Gera uma submatriz a partir de índices especificados
  Parâmetros:
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
    Obtem uma submatriz simétrica
  Parâmetros
    l: Linha inicial para submatriz
    mb: Ordem da submatriz
  Retorno
    Sempre retorna uma matriz simétrica
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
    Obtem uma submatriz com os elementos especificados através dos vetores R e C
  Parâmetros
    R: Índices das linhas dos elementos
    C: Índices das colunas
  Retorno
    Retorna uma matriz geral de dimensão R.Len e C.Len. Os elementos da submatriz serão os
    elementos que estão nas posições (R[i],C[j])
}
var
  i,j: Integer;
begin
  Result := nil;
  {Verifica se não há valores fora da faixa.}
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
   Parâmetros
     C: Índices das coluna de interesse
     Ch:Operador para redução das linhas. Os operadores considerados são:
       opSum : Soma os elementos
       opSSq : Soma de quadrados dos elementos
       opGT  : Maior valor
       opLT  : Menor valor
       opMean: Média
       opQ1  : Primeiro quartil
       opQ2  : Segundo quartil
       opQ3  : Terceiro quartil
    Retorno
      Matriz geral com uma linha. Por exemplo, se A possui 5 colunas, a chamada
      B:=A.RowReduc([2,3,4],opSum) obtem uma matriz de uma linha e três colunas com o total
      da coluna 2 na primeira posição, total da 3na segunda e assim por diante.
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
   Parâmetros
     C: Índices das linhas de interesse
     Ch:Operador para redução das colunas. Os operadores considerados são:
       opSum : Soma os elementos
       opSSq : Soma de quadrados dos elementos
       opGT  : Maior valor
       opLT  : Menor valor
       opMean: Média
       opQ1  : Primeiro quartil
       opQ2  : Segundo quartil
       opQ3  : Terceiro quartil
    Retorno
      Matriz geral com uma coluna. Por exemplo, se A possui 5 linhas, a chamada
      B:=A.ColReduc([2,3,4],opSum) obtém uma matriz de uma coluna e três linhas com o total
      da linha 2 na primeira posição, total da 3 na segunda e assim por diante.
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
  Parâmetros
    ChRow: Operador de redução das linhas
    ChCol: Operador de redução das colunas
  Observações
    Inicialmente é aplicado o operador de redução das linhas e sobre o resultado é aplicado
    o redutor de colunas. Os operadores são:
      opSum : Total
      opSSq :  Soma de quadrados
      opGT : Máximo
      opLE : Mínimo
      opMean : Média
      opQ1 , opQ2, opQ3 :  1o., 2o. ou 3o quartil.
  Exemplo
    A instrução x:=A.RowColReduc(opGT,opMean) irá produzir a média dos máximos das colunas
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
  Parâmetros
    Col: Índices das colunas para as quais serão calculadas as estatísticas
    Stat: Estatísticas desejadas. Os valores em Stat e as correspondentes estatisticas sao:
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
    Matriz com as estatísticas desejadas nas colunas e as colunas nas linhas
  Exemplo
    S := A.DescStat([0,1],[1,2,3]) gerará uma matriz de duas colunas e três linhas onde na
    primeira coluna estão as medias e na segunda coluna as variâncias das colunas 1, 2 e 3
  Valores perdidos
    São excluidos dos calculos
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
        M[5]^[j]:=M[5]^[j]+1;                // Numero de observacoes válidas
        w0 := M[5]^[j]-1;
        v := (x-M[1]^[j])/M[5]^[j];
        M[4]^[j]:=M[4]^[j]-4*v*M[3]^[j]+6*v*v*M[2]^[j]         // 4o momento
          + (M[5]^[j]*M[5]^[j]-3*w0)*Power(v,4)*M[5]^[j]*w0;
                                                               // 3o momento
        M[3]^[j]:=M[3]^[j]-3*v*M[2]^[j]+(M[5]^[j]-2)*Power(v,3)*M[5]^[j]*w0;
        M[2]^[j] := M[2]^[j] + v*v*M[5]^[j]*w0;                   { 2o momento }
        M[1]^[j] := M[1]^[j] + v;                                      { Média }
        M[6]^[j] := M[6]^[j]+x*x;             { Soma de quadrados nao ajustada }
        if M[7]^[j] > x then M[7]^[j] := x;                     { Valor minino }
        if M[8]^[j] < x then M[8]^[j] := x                      { Valor maximo }
        end;
  Result := TwsGeneral.Create(Col.Len,Stat.Len);   // Matriz que armazenara os resultados
  Result.MLab := 'Estatísticas descritivas';
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
  Parâmetros
    Col: Índices das colunas para as quais serão calculadas as estatísticas
    Stat: Estatísticas desejadas. Os valores em Stat e as correspondentes estatisticas sao:
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
    Matriz com as estatísticas ponderadas desejadas nas colunas e as colunas nas linhas
  Exemplo
    S := A.WDescStat([0,1],[1,2,3],5) gerará uma matriz de duas colunas e três linhas onde
    na primeira coluna estão as medias e na segunda coluna as variâncias das colunas 1, 2
    e 3. Utiliza os valores da coluna 5 para a ponderacao. Somente observações com pesos
    estritamente positivos é que são utilizadas.
  Valores perdidos
    São excluidos dos calculos
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
            M[1]^[j] := M[1]^[j] + v;                                              { Média }
            M[6]^[j] := M[6]^[j]+wx*x*x;                  { Soma de quadrados nao ajustada }
            if M[7]^[j] > x then M[7]^[j] := x;                             { Valor minino }
            if M[8]^[j] < x then M[8]^[j] := x                              { Valor maximo }
            end { if x <> wscMissValue }
        end; { if (wx>0) }

  Result := TwsGeneral.Create(Col.Len,Stat.Len);
  Result.MLab := 'Estatísticas descritivas ponderadas';
  Result.Name := 'Estat';
  for j := 1 to Stat.Len do
    Result.ColName[j] := cShortNameEstat[Stat[j]];

  { Obtem as estatísticas a partir dos momentos }
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
    Auxiliar para obtenção do vetor de estatísticasDetermina o indice de k em Lin. Retorna -1
    se k não está em Lin.
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
  Parâmetros
    Col: Índices das colunas para a obtenção das estatísticas
    Stat: Estatísticas desejadas. A codificação será:
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
    Se mais de uma estatística for desejada para a mesma coluna, essa coluna deverá ser
    repetida juntamente com a estatística. Assim, para a instrução
      v:=A.VDescStat([1,1,1,3],[0,1,2,0])
    se tera o retorno
      v[1],v[2],v[3]: Média, variância e desvio padrão da primeira coluna e
      v[4]: Média da terceira coluna
    Observe-se que, para cada índice de coluna especificada em Col deverá haver uma
    estatística correspondente em Stat.
  Valores perdidos
    Excluidos dos calculos. Se todos os valores são perdidos, retorna valores perdidos
      em todas as posições 
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
    begin     // A matriz M guardará as quantidades básicas
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
        M[1]^[j] := M[1]^[j] + v;                   // Média
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
  Parâmetros
    Col: Índices das colunas para a obtenção das estatísticas
    Stat: Estatísticas desejadas. A codificação será:
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
    WInd: Índice da variável peso
  Exemplo
    Se mais de uma estatística for desejada para a mesma coluna, essa coluna deverá ser
    repetida juntamente com a estatística. Assim, para a instrução
      v:=A.WVDescStat([1,1,1,3],[0,1,2,0],5)
    se tera o retorno
      v[1],v[2],v[3]: Média, variância e desvio padrão da primeira coluna e
      v[4]: Média da terceira coluna
    Todos os cálculos são ponderados pelos valores da 5 coluna
    Observe-se que, para cada índice de coluna especificada em Col deverá haver uma
    estatística correspondente em Stat.
    Somente serão incluídas as linhas para as quais o valor da variável peso é estritamente
    positivo
  Valores perdidos (incluindo a variável peso)
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
            M[1]^[j] := M[1]^[j] + v;           // Média
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
  Paâmetros
    f: Valor do percentil desejado.
    Col: Índice da coluna para obtenção do percentil
  Observação
    f deverá estar entre 0 e 1 (0 < f < 1). Se não estiver, a função retorna wscMissValue
    A função pressupõe que os valores referentes à coluna desejada estão ordenados
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
  Parâmetros
    Col: Índices das colunas desejadas
    Stat: Estatísticas de ordem que se deseja calcular. Os códigos são os seguintes:
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
    Obs.: A saída é uma matriz onde cada linha corresponde a uma variável e cada
      coluna a uma estatística
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
    Obtém um valor escalar a partir de todos os elementos da matriz especificada.
  Parâmetros:
    Op: Tipo de função desejada
      Os tipos aceitos são:
        cSoma: Retorna a soma de todos os valores da matriz
        cMEDIA: Média
        cABSNORM: Norma do valor absoluto
        cSQUAD: Soma de quadrados
        cENORM: Norma euclidiana
        cTRACO: Traço da matriz
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
  Parâmetros
    j: Coluna para obtenção da norma
    l1, l2: linha inicial e final para obtenção da norma
  Observações
    Valores perdidos não são considerados
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
  Parâmetros
    j,k: Colunas para obtenção do produto interno
    l1, l2: linha inicial e final para obtenção do produto interno
  Observações
    Valores perdidos não são considerados
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
  Parâmetros
    NewMat: Se True, o resultado retorna numa nova matriz; caso contrário, substitui a
    própria matriz
  Observações: Inicialmente a matriz é copiada, sequencialmente por linhas, para um vetor,
    os postos com empates são obtidos para esse vetor e posteriormente copiados para a
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
  Parâmetros
    NewMat: Se True, o resultado retorna numa nova matriz; caso contrário, substitui a
      própria matriz
    Ascd: True se a ordem será ascendente; false caso contrário
  Observações: Inicialmente a matriz é copiada, sequencialmente por linhas, para um vetor,
    os postos são obtidos para esse vetor e posteriormente copiados para a matriz resultante
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
    Coloca os valores das colunas segundo os índices especificados
  Parâmetros
    Indx: Índices em que serão colocados os valores das colunas
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
  Parâmetros
    TFile: Arquivo texto onde será escrita a matriz.
    IsLast: Indica se a matriz é a última a ser gravada, assim podemos marcar o final do arquivo
    LSep: caracter que irá indicar o final da linha da matriz
    SSep: Caracter que irá indicar o final de seções (Nomes de linhas, colunas, etc.).
    Espaços em branco separarão os elementos de cada seção.
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
    Transforma todas as colunas de uma matriz em colunas numéricas de um conjunto de dados
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
  Parâmetros
    TFile: Arquivo texto com os valores
    Ch: Caracter delimitador de cada linha. Cada elemento será delimitado por um caracter do
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
    Verificar se todos os elementos da matriz atendem à condição estabelecida por Op.
  Parâmetros
    x: Escalar que será comparado com os valores da matriz
    Op: Tipo de comparação desejada
}
var
  i,j: Integer;
begin
  case Op of
    opEQ: // Todos os elementos são iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarEQ(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opGE: // Todos os elementos são maiores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGE(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opGT: // Todos os elementos são maiores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGT(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opLE: // Todos os elementos são menores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLE(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opLT: // Todos os elementos são menores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLT(x,Self[i,j])=ScalarTrue);
          if not Result then Exit
          end;
    opNE: // Todos os elementos são diferentes ?
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
    Verificar se algum elemento da matriz atende à condição estabelecida por Op.
  Parâmetros
    x: Escalar que será comparado com os valores da matriz
    Op: Tipo de comparação desejada
}
var
  i,j: Integer;
begin
  case Op of
    opEQ: // Todos os elementos são iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarEQ(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opGE: // Todos os elementos são maiores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGE(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opGT: // Todos os elementos são maiores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarGT(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opLE: // Todos os elementos são menores ou iguais ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLE(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opLT: // Todos os elementos são menores ?
      for i:=1 to NRows do
        for j:=1 to NCols do
          begin
          Result := (ScalarLT(x,Self[i,j])=ScalarTrue);
          if Result then Exit
          end;
    opNE: // Todos os elementos são diferentes ?
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
  Parâmetros
    B:       Matriz cujas linhas serão inseridas no final
    ErrCode: Retorna zero se a concatenação for feita com sucesso. NImprDim se o número de
             colunas de A for diferente do de B.
  Resultado
    A chamada A.RowConcat(B,ErrCode) retornará uma matriz com as linhas de A, seguidas das
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
  Parâmetros
    B: Matriz cujas colunas serao colocadas por ultimo
    ErrCode: Retorna zero se a concatenacao foi feita com sucesso. NImprDim se o numero de
      linhas de A e B nao for o mesmo.
  Resultado
    A chamada A.ColConcat(B,ErrCode) retornará uma matriz com as colunas de A, seguidas das
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
  Parâmetros
    x     : Escalar a concatenar
    SFirst: Se True então o escalar vai primeiro (em todas as colunas) e os elementos da
            matriz o seguem; se False o escalar é colocado por último (em todas as colunas).
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
  Parâmetros
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
  Parâmetros
    v:       Vetor a ser inserido
    NewMat:  Se true (ou se a matriz não é geral), então o vetor será inserido na matriz
             resultante da cópia; se false, o vetor será inserido na mesma matriz
    VFirst:  Se true, então o vetor vai na primeira posição; se false, na última
    ErrCode: Retorna zero se a concatenação foi feita com sucesso; NImprDim se o número
             de elementos do vetor for diferente do número de colunas da matriz.
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
  Parâmetros
    x:       Vetor a concatenar.
    VFirst:  Se true, então o vetor vai primeiro e as colunas da matriz depois.
    ErrCode: Retorna zero se a concatenação foi feita com sucesso; NImprDim se o número
             de elementos do vetor for diferente do número de linhas da matriz.
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
    Obtém a forma escalonada da matriz especificada. Algoritmo em Noble (1a. edição)
  Parâmetros:
    NewMat: False indica que a forma escalonada substiuirá A, caso contrário (ou se a
            matriz não for geral) uma nova matriz será criada.
    CMax:   Indice máximo da coluna a ser operada.
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
  Parâmetros
    Asc: true (default) para ordem crescente; false, caso contrário
  Retorno
    Retorna uma matriz geral, com os dados ordenados no sentido das linhas. Assim, se
    Asc=True, a primeira linha conterá os menores valores e assim por diante.
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
    Concatenar uma linha na matriz. Não atualiza o número de linhas.
  Parâmetros
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
  Raise Exception.Create('Método "UserCreate" não definido');
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
    Busca primeiro par não ordenado por meio de uma busca sequencial
  Parâmetros
    i: Se houver par não ordenado, eles serão i e i+1
    Ascd: true para ordenação ascendente; false caso contrário
  Campos alterados
    FData
  Métodos chamados
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
// A matriz deverá estar limpa (sem linhas e colunas)
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
    Cria uma matriz geral. É denominada matriz geral a matriz onde todos os elementos necessitam
    ser explicitamente armazenados.
  Parâmetros
    NR: Número de linhas.
    NC: Número de colunas
   Métodos chamados
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
  Result := 'Estilo de formatação para Matrizes do tipo TwsGeneral';
end;

function TwsGeneral.Get(i,j: Integer): Double;
{ Objetivo
    Obtem um elemento da matriz geral.
  Parâmetros
    i: Índice da linha
    j: Índice da coluna
}
begin
  Get := Row[i].Data[j];
end; { TwsGeneral.Get }

procedure TwsGeneral.Put(i, j: Integer; x: Double);
{ Objetivo
    Atribui um valor a uma posição da matriz
  Parâmetros
    i: Linha onde o elemsnto será colocado
    j: Coluna onde o elemento será colocado
    x: Valor a ser atribuído à posição (i, j)
}
begin
  FModified := True;
  Row[i].Data[j] := x;
end; { TwsGeneral.Put }

procedure TwsGeneral.List(Buffer: TStrings);
{ Objetivo
    Lista os descritores de uma matriz geral
  Métodos chamados
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
   Efetuar operações entre uma matriz geral e um escalar
 Parâmetros
   x: Escalar para a operação
   Op: Tipo de operação desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas operações (soma ou produto) esse parâmetro não é utilizado.
     No caso de potenciação, SFirst=True indica que o escalar estará na base e o elemento
     na potência; caso contrário, o elemento estará na base e o escalar na potência. Para
     o operador ** se SFirst=False, a operação corresponderá ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar (observe que, neste caso, a matriz deve
     ser quadrada para que o produto seja sempre definido); caso contrário a operação não
     é definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz
 Retorno
   Sempre uma matriz geral
 Valores perdidos
   Se houver um valor perdido na operação envolvendo um elemento, o resultado é valor perdido
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
    Obtém autoestrutura da matriz
  Parâmetros
    B  : matriz mtDiagonal na qual retornarão os autovalores
    EgV: Se true, na matriz que requisita o método retornarão os autovetores; se false os
    autovetores não serão obtidos
    r  : Posto numérico da matriz, ou o número de autovalores que, em módulo, são maiores
         que a precisão estabelecida
    Sort: Se True os autovalores serão ordenados em ordem descendente. Os autovetores, se forem
          obtidos, serão também ordenados
    eps  : precisão para determinação do posto numérico
    ErrCode: retorna 0 se nenhum erro ocorreu durante o processo; <>0 caso contrário
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
    Cópia da matriz
  Parâmetros
    MT: Tipo da matriz de retorno. Tipo especificado deverá ser: mtGeneral, mtSymmetric, mtDiagonal,
        mtTriangular, mtVandermonde ou mtToeplitz
    Matrix: Matriz que retorna com a cópia
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
  Parâmetros
    Column: Vetor que indica os índices das colunas que funcionarão como chaves de
        ordenação.
    Ascend: Vetor de mesma dimensão de Column que indica se para a coluna correspondente
       a ordenação será ascendente ou descendente. Se para uma coluna a posição Ascend
       tiver 0 (zero) na posição correspondente a ordem será descendente; ascendente se
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
    Coloca as linhas segundo os índices especificados
  Parâmetros
    Indx: Índices em que serão colocadas as linhas
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
    Trocar o conteúdo de colunas
  Parâmetros
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
  Parâmetros
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
     ela retorna na matriz que aciona este método. Esta matriz deve ser simétrica
     mas armazenada na forma geral.
  Parâmetros
      d: Na entrada contém os elementos da mtDiagonal da matriz tridiagonal. Na saída
            retorna os autovalores.
      e: Na entrada contém os elementos da subdiagonal da matriz tridiagonal, com
            e[1] arbitrario. Na saida e e destruido.
      ErrCode: 0 se nenhum erro ocorreu na obtenção; <> 0 caso contrário
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
   Faz a redução de Householder da matriz, real e simétrica, que chama o método.
    Na saída, a matriz é substituida pela matriz ortogonal Q que faz a transformacao.
 Parâmetros
   d: Retorna os elementos da mtDiagonal da matriz triagonal
   e: Retorna os elementos da subdiagonal da matriz tridiagonal.
   EgV: True se os autovetores serão obtidos
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
     autovalores. Uma matriz simétrica já é balanceada e não é afetada por este
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
     Redução da matriz, real e não simétrica, para uma forma de Hessenberg. A matriz original é
     substituída pela forma superior de Hessenberg que possui os mesmos autovalores. É recomendável
     mas não obrigatório que esta rotina seja precedida da aplicação da rotina Balanc. Na saída, os
      elementos da matriz de hessenberg  ocupam as posições (i, j) com i <= j+1. Elementos com
      i > j+1 podem ser considerados como nulos mas de fato estas posições conterão valores sem
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
    Obtém a decomposicao LU de uma permutação de linhas da matriz quadrada que chama o método.
    Na matriz original retornam os fatores L (mtTriangular inferior) e U (mtTriangular superior).
  Parâmetros:
    Indx: Registra as trocas de colunas efetuadas pelo pivoteamento parcial
    d: Retorna 1 se o número de trocas é impar ou -1 se par. Útil no cálculo de determinantes.
    ErrCode: Código de erro. Retora 0 se a fatoração for realizada com sucesso ou maInvError
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
    Obtem o determinante da matriz já fatorada artavés da decomposicao LU.
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
   Parâmetros
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
    Aplica a transformação numa matriz após a construção da transformação
    de Givens. Ver L&H

  Parâmetros
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
    Inverte a matriz através do método de Gauss-Jordan com pivoteamento total.
     Na saída a matriz armazena a inversa.
  Parâmetros
    ErrCode: Código de erro. Retorna 0 para inversão com sucesso e maInvError se a matriz
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
      Após a aplicação da transformação de Householder para uma solução de mínimos
      quadrados, o fator triangular fica armazenado na porção triangular superior
      esquerda da matriz original. Este procedimento retorna a matriz de covariância
      (sem o fator de escala) da solução de mínimos quadrados.
     Parâmetros
       p: Ordem da matriz de covariâncias, que retorna na porção p x p superior esquerda
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
   Parâmetros:
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
    Obtém a solução de um sistema de equações lineares AX = B, através da decomposição LU, onde
    A é a matriz que chama o método. Na entrada A armazena os coeficientes e na saída armazena
    os fatores da decomposição LU.
  Parâmetros
    B: Na entrada armazena os vetores do segundo membro e na saída armazena em cada coluna a
    solução do respectivo sistema
    ErrCode: Código de erro que retorna da decomposição LU.
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
    Normaliza coluna especificada (comprimento unitário)
  Parâmetros
    j: Coluna a ser normalizada
  Observação: Valores perdidos são desconsiderados
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
    Utiliza a matriz como base e constroi uma base ortonormal utilizando o método de Gram-
    Schmidt
  Parâmetros
    NewMat: False (default) substitui o conteúdo atual pela base ortonormal; True a base
    ortonormal é alocada em uma nova matriz
  Observações: Desconsidera os valores perdidos
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
    Utiliza a matriz como base e constroi uma base ortonormal utilizando o método
    modificado de Gram-Schmidt
  Parâmetros
    NewMat: False (default) substitui o conteúdo atual pela base ortonormal; True a base
    ortonormal é alocada em uma nova matriz
  Observação: Valores perdidos são desconsiderados
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
    Constroi matriz de projeção ortogonal. A matriz deve ser de posto coluna completo
  Observação: Matriz não pode ter valores perdidos
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
    Constroi matriz de projeção sobre o complemento ortogonal
  Observação: Matriz não pode ter valores perdidos
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
  Parâmetros
    C: Matriz que retorna os valores das somas da concorrências ponderadas
       relativas as colunas.
    R: Matriz que retorna os valores das somas das concorrências ponderadas
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
  C.MLab:='Condição de Pearce aplicada às colunas';
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
  R.MLab:='Condição de Pearce aplicada às linhas';
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
      Aplica sweep numa matriz simétrica armazenada como geral
    Parâmetros
      k: coluna para aplicação do sweep
      DMin: tolerância ara aplicação do sweep
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
    Obtém a decomposição de uma matriz (A=QT) pelo método modificado de Gram-Schmidt
  Parâmetros
    T      : Retorna a matriz mtTriangular
    ErrCode: Código de erro. retorna 0 se o processamento foi normal e NSingMat se
      a matriz é singular
  Retorno
    A matriz que chama o método será fatorada e substituída pelo fator Q
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
    Obtém a decomposição de uma matriz (A=QT), onde Q é ortonormal e T é mtTriangular, pelo
    método modificado de Gram-Schmidt na métrica estabelecida pela matriz D
  Parâmetros
    D      : Matriz mtDiagonal contendo a métrica para decomposição
    T      : Matriz mtTriangular da decomposiçào
    ErrCode: Código de erro. Retorna 0 se o retorno é feito sem problemas e NSingMat se a
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
    Obter a decomposição por valores singulares (A=UDV') de uma matriz
  Parâmetros
    D: Matriz que retorna os valores singulares
    V: Matriz da decomposição
    ErrCode: Código de erro. Retorna NIterMax se o processo iterativo superar 30 iterações;
    0 caso contrário
  Retorno
    A matriz U retorna na própria matriz que chama o método. Na saída todas as matrizes
    envolvidas são ordenadas em função dos valores singulares, ou seja, os valores
    singulares são ordenados em ordem descendente e as colunas das demais matrizes são
    reorganizadas para se ajustar a essa ordenação
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
    Obtém a inversa (generalizada) de Moore Penrose de uma matriz através da decomposição
    por valores singulares.
  Retorno
    Para a matriz A(m x n), a chamada A.MoorePenrose irá retornar a matriz B(n x m) tal
    que ABA=A. A matriz A não é alterada no processo. Se ocorrer algum problema na SVD
    a função retorna nil
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
    Obtém os índices das colunas entre as quais está um valor especificado.
  Parâmetros
    x  : valor a ser pesquisado
    Lin: Indica em relação a que linha será feita a pesquisa
    j  : Retorna um valor tal que x estará entre xx[Lin,j] e xx[Lin,j+1].
         Se não existe nenhuma linha j retorna 0;
         Se o valor existe na linha indicada, j retorna o seu índice.
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
    Obtém os índices das linhas entre as quais está um valor especificado.
  Parâmetros
    x  : valor a ser pesquisado
    Col: Indica em relação a que coluna será feita a pesquisa
    j  : Retorna um valor j tal que x estará entre xx[j,Col] e xx[j+1,Col].
         Se não existir nenhuma linha na matriz j retorna 0
         Se o valor de x existir na coluna indicada, j retorna o índice da linha onde está
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
    Dada uma matriz geral, retorna as correlações (de Pearson) entre as variáveis
    correspondentes às coluna especificadas, as estatísticas simples para essas variáveis
    e os testes de hipóteses para as correlações
  Parâmetros
    C    : Índices das colunas para determinação das correlações
    Stat : Matriz com as estatísticas simples.
    BStat: Se True, retorna a matriz de estatísticas em Stat; caso contrário Stat retorna
           nil
    Corr : Matriz de correlações
  Valores perdidos
    Toda observação que possuir algum valor perdido em pelo menos uma das variáveis
    especificada é eliminada
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
     Stat.MLab := 'Estatísticas descritivas simples';
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
  Result.MLab:='Matriz de correlaçoes simples';
  if n<=3 then
    begin
    CTest:=nil;
    Exit
    end;
  // Testes de hipóteses para as correlações
  CTest := TwsDataSet.Create('Corr_Test');
  with TwsDataSet(CTest) do
    begin
    MLab:='Teste de significância para os coeficientes de correlação';
    Col:=TwsQualitative.Create('Var_I','Variável I');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 1. Nome da primeira variável }

    Col:=TwsQualitative.Create('Var_J','Variável J');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 2. Nome da segunda variável }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correlação de Pearson',10,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determinação',10,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estatística T para hipótese H0',12,7));     {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hipótese H0',10,5));     {6}

    Col:=TwsQualitative.Create('Sig','Significância do teste ao nível 5% ou 1%',6);
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
    Retorna uma matriz simétrica onde os componentes são os coeficientes de correlação
    simples ponderados entre as colunas especificadas
  Parâmetros
    C: Vetor com os índices das colunas para as quais se deseja as correlações
    Stat: Matriz das estatísticas simples para as variáveis
    BStat: se True retorna a matriz de estatísticas; retorna nil caso contrário
    WInd: Índice da variável peso
  Valores perdidos
    A observação que possuir algum valor perdido (para as variáveis indicadas ou para a
      variável peso) são eliminadas do processo
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
    Stat.MLab := 'Estatísticas descritivas simples';
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
  Result.MLab := 'Matriz de correlações de Pearson / Peso: '+ColName[WInd];
  CTest := TwsDataSet.Create('WCorr_Test');
  with TwsDataSet(CTest) do
    begin
    MLab:='Teste de significância para os coeficientes de correlação';
    Col:=TwsQualitative.Create('Var_I','Variável I');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 1. Nome da primeira variável }

    Col:=TwsQualitative.Create('Var_J','Variável J');
    for i:=1 to Result.NCols do
      TwsQualitative(Col).AddLevel(Result.ColName[i]);
    Struct.AddColEx(Col);                                 { 2. Nome da segunda variável }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correlação de Pearson',11,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determinação',11,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estatística T para hipótese H0',12,7));     {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hipótese H0',11,5));     {6}

    Col:=TwsQualitative.Create('Sig','Significância do teste ao níevl 5% ou 1%',6);
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
    Retorna uma matriz simétrica onde os componentes são os coeficientes de correlação
    parciais entre as colunas especificadas
  Parâmetros
    C: Vetor com os índices das colunas para as quais se deseja as correlações
    CAdj: Colunas para as quais se deseja calcular as correlações parciais
    Stat: Matriz de estatísticas descritivas das colunas especificadas em C
    BStat: Se True obtém matriz das estatísticas descritivas, se False, Stat retorna nil
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

  SP.MLab := 'Matriz de produtos cruzados ajustados para média';

  ColOp := TwsLIVec.Create(nvtot);
  for i := 1 to nvtot do ColOp[i] := 1;

  Aux := TwsDFVec.Create(nvfix);
  for i := 1 to nvfix do
    Aux[i] := SP[nv+i,nv+i]*1.0e-8; { Tolerâncias }   //dah erro aqui

  for i := 1 to nvfix do
    SP.RevSweep(nv+i, Aux[i], ColOp);

  if BStat then
    begin
    Stat := TwsGeneral.Create(C.Len, 4);
    Stat.Name := 'Estat_' + System.Copy(Name,1 ,4);
    Stat.MLab := 'Estatísticas descritivas simples';
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
    MLab:='Teste de significância para os coeficientes de correlação';
    DSCol:=TwsQualitative.Create('Var_I','Variável I');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 1. Nome da primeira variável }

    DSCol:=TwsQualitative.Create('Var_J','Variável J');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 2. Nome da segunda variável }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correlação de Pearson',10,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determinação',10,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estatística T para hipótese H0',12,7));     {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hipótese H0',10,5));     {6}

    DSCol:=TwsQualitative.Create('Sig','Significância do teste ao nível 5% ou 1%',6);
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
    Retorna uma matriz simétrica onde os componentes são os coeficientes de correlação
    parciais ponderados entre as colunas especificadas
  Parâmetros
    C   : Vetor com os índices das colunas para as quais se deseja as correlações
    CAdj: Colunas para as quais se deseja calcular as correlações parciais
    Stat: matriz com as estatísticas descritivas
    BStat: se true, a matriz de estatísticas descritivas será construída; false Stat retorna nil
    WInd: Índice da variável para ponderação
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
  SP.MLab := 'Matriz de produtos cruzados (ponderados) ajustados para média';

  if BStat then
    begin
    Stat := TwsGeneral.Create(C.Len,4);
    Stat.Name := 'Estat_' + System.Copy(Name,1,4);
    Stat.MLab := 'Estatísticas descritivas simples';
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
    Aux[i] := SP[nv+i,nv+i]*1.0e-8; { Tolerâncias }   //dah erro aqui

  for i := 1 to nvfix do
    SP.RevSweep(nv+i, Aux[i], ColOp);

  Col.Free; ColOp.Free; Mean.Free; Aux.Free;

  Result := TwsSymmetric(SP.SubSymMat(1, nv));
  SP.Free;

  Result.CorrMat;

  CTest := TwsDataSet.Create('WCorr_Test');
  with TwsDataSet(CTest) do
    begin
    MLab:='Teste de significância para os coeficientes de correlação';
    DSCol:=TwsQualitative.Create('Var_I','Variável I');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 1. Nome da primeira variável }

    DSCol:=TwsQualitative.Create('Var_J','Variável J');
    for i:=1 to Result.NCols do
      TwsQualitative(DSCol).AddLevel(Result.ColName[i]);
    Struct.AddColEx(DSCol);                                 { 2. Nome da segunda variável }

    Struct.AddColEx(TwsNumeric.Create('Correl','Valor da correlação de Pearson',11,5));{3}

    Struct.AddColEx(TwsNumeric.Create('Coef_Det','Coeficiente de determinação',11,5)); {4}

    Struct.AddColEx(TwsNumeric.Create('T','Estatística T para hipótese',12,7));        {5}

    Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hipótese H0',11,5));     {6}

    DSCol:=TwsQualitative.Create('Sig','Significância do teste ao nível 5% ou 1%',6);
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
    para a média.
  Parametros
    Col   : Indica quais as colunas que deverao ser consideradas.
    Mean  : Retorna as medias de cada coluna indicada em Col
    ValObs: Número de observações válidas (que não contém nenhum valor perdido) incluidas.
  Valores perdidos
    Uma observação é eliminada se em alguma das posicoes estabelecidas por Col houver um
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
    Retorna uma matriz com as somas de quadrados e produtos ajustadas para a média
    referentes às colunas indicadas
  Parâmetros
    Col   : Índices das colunas que deverão ser consideradas
    Mean  : Retorna as medias de cada coluna indicada em Col
    S     : Retorna a matriz simetrica dos produtos cruzados
    ValObs: Número de observações válidas
    wa    : Soma dos pesos
    WIndex: Indice da variavel peso em X
  Valores perdidos
    Toda observação que possuir algum valor perdido (para as variáveis indicadas ou para a
    variável peso), peso nulo ou negativo é excluída
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
  Parâmetros
    MaxIts: Número máximo de iterações do processo
    ErrCode: Código de erro. Retorna NIterMax se MaxIts não for suficiente para a
    convergência do processo, 0 se não ocorreu erro
  Retorno
    Na chamada B:=A.(MaxIts,ErrCode), a matriz A na entrada deverá ser a saída do algoritmo
    TwsGeneral.Hessenb e seu cnteúdo é destruído no processo. Em B retorna na primeira linha
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
  Parâmetros:
    Col: Especifica as chaves e o tipo de ordenamento (ascendente ou descendente)
      cada componente de Col terá o nome da coluna que servirá de chave para a ordenação
      e um indicador (A ou D) para estabelecer a ordenação ascendente ou descendente,
      separados por um delimitador válido [#9,#10,#13,' ',',','=','\', '"','(',')']. Por
      exemplo, Col[0]='Col1 A' e Col[1]='Col2 D' indicam que a ordenação das linhas será
      em função dos valores das colunas Col1 e Col2, ascendente para a primeirab e
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
    Dada a tabela com os valores para o teste de Tukey, obtém o valor desejado.
  Parâmetros
    nTreat: Número de tratamentos ou medias a comparar
      Se nTrat < 2 então nTrat = 2
      Se nTrat > último valor da tabela, ntrat = último valor da tabela
    DF: graus de liberdade do residuo
      Se DF <= 1 então DF = 2
      Se DF > ultimo valor da tabela, DF = último valor da tabela
  Observação: Se um ou os dois valores dos parâmetros não estão na tabela, o valor retornado
    é uma interpolação linear dos valores existentes
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
     Calcula a média de colunas da matriz, iniciando na linha especificada. A utilização
     deste método pressupõe qua as linhas da matriz estarão ordenadas em relação às colunas
     cujos índices estão em KeyCol.
   Parâmetros
     KeyCol: Indice das colunas que servirão como chaves. A pesquisa inicia na linha XRow e
             segue enquanto existir concordância nas chaves, ou seja, as linhas que serão
             utilizadas serão aquelas onde os índices da linha XRow nas colunas especificadas
             por KeyCol se repetem nas linhas seguintes.
     Col:    Índices das colunas para as quais serão calculadas as médias e número de
             repetições.
     XRow:   Na entrada indica a linha inicial para processamento. Na saída, contém a linha
             seguinte à última processada. Este parâmetro é conveniente, entre outras coisas,
             para um processamento sequencial das linhas da matriz, através de uma chamada
             sequencial desta rotina.
   Retorno
     Retorna um vetor onde nas primeiras KeyCol.Len posições estarão as chaves, nas Col.Len
     seguintes as médias e na última irá retornar o número de repetições. Portanto, o
     retorno será um vetor de KeyCol.Len + Col.Len + 1 colunas. Retorna nil se nenhuma
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
     Calcula a média de colunas da matriz, iniciando na linha especificada. A utilização
     deste método pressupõe qua as linhas da matriz estarão ordenadas em relação às colunas
     cujos índices estão em KeyCol.
   Parâmetros
     KeyCol: Índices das colunas que servirão como chaves. As linhas deverao estar ordenadas
             segundo as chaves estabelecidas.
     Col:    Índices das colunas para as quais serão calculadas as médias, somas de
             quadrados e número de repetições.
     GMean:  Vetor com as médias gerais de todas as linhas processadas. A cada chamada
             deste método, este vetor é atualizado.
     SSQ:    Somas de quadrados ajustadas para a média de todas as variáveis especificadas
             em Col. Na primeira chamada este vetor devera estar dimensionado com comprimento
             Col.Len e com zeros em todas as posicoes. Se Univ for True este parâmetro
             retornará um vetor; caso contrário retornará uma matriz simétrica com as
             somas de quadrados e produtos. A cada chamada deste método este parâmetro é
             atualizado.
     XRow:   Na entrada deve indicar a linha onde será iniciado o processamento. Na saída
             indicará a linha seguinte à última processada.
     Univ:   True se em SSq deverão retornar somente as somas de quadrados das variáveis;
             false para que retorne a matriz das somas de quadrados e produtos. A primeira
             situação corresponde à utilização do método num contexto de análise univariada
             enquanto que o segundo, de análise multivariada.
     M:      Acumula o número de observações válidas processadas.
   Retorno:
     Nas primeiras KeyCol.Len colunas estarão as chaves, nas Col.Len seguintes as médias
     e na última irá retornar o número de repetições. Portanto, o retorno será um vetor de
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
     Calcula média de linhas da matriz para as colunas especificadas.
   Parâmetros
     KeyCol: Índice das colunas para que servirão como chaves. Varre todas as linhas da
             matriz para calcular a média correspondentes à chave. Assim, neste método,
             as linhas não necessitarão estar ordenadas segundo as chaves estabelecidas.
     Col:    Índices das colunas para as quais serão calculadas as médias e o número de
             repetições.
  Retorno
   Nas primeiras KeyCol.Len colunas estarão as chaves, nas Col.Len seguintes as médias
   e na última irá retornar o número de repetições. Portanto, o retorno será um vetor de
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
    Verificar se todos os valores de uma coluna são iguais.
  Parâmetros
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
     Dada uma matriz geral, retorna a sua inversa utilizando a fatoração LU
   Observações
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
  Parâmetros
    Indx: vetor de índices de trocas realizadas durante a fatoração da matriz
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
     Dada uma matriz já fatorada através do algoritmo LU, retorna a inversa da matriz
     original
   Parâmetros
     Indx: Vetor com os indices das trocas realizadas na fatoração LU
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
  Parâmetros
    p: Número de colunas que serão operadas pelo procedimento
    g: Vetor que retorna os valores da diagonal de F
    h: Vetor que retorna os valores de f'[i]f[i]
  Saída
    * Na chamada A.Householder(p,g,h,ErrCode), ao término do processo, o conteúdo da matriz
    é o seguinte:
      a11 a12       u11 u12
      a21 a22 --->  f12 u22
      a31 a32       f13 f23
      a41 a42       f14 f24
    g=[f11, f22] e h=[(f1)'*f1, (f2)'*f2]
    sendo que com F é possível construir a matriz de reflexão de Householder e obter Q.
    * Se a matriz A nxp for completada à direita com uma identidade, no lugar da identidade
    retorna a matriz Q.
    * No sistema inconsistente y=Xb + e onde X é nxp entao se A=[X, y] então, no final
    Q v   é a solucao de mínimos quadrados será a solução de Qb=v e t't é a soma de
    0 t   quadrados de quadrados do resíduo
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
    Constrói e aplica a transforma de Householder às colunas de uma matriz. Algoritmo básico
    em Lawson & Hanson (1971), pág. 57 e implementação em Fortran na pág. 308
  Parâmetros
    HTrans: Com HTrans=False o método irá construir e aplicar a transformação de Householder.
            Para HTrans=True, somente a construção da aplicação será feita.
    p     : Elemento pivo
    l     : Elemento a partir do qual todos os demais serão zerados. Se l<=NRows a
            transformação será construída para zerar os elementos de l a NRows. Se l>NRows,
            retorna a transformação identidade
    Col   : Coluna incial da matriz para aplicação da transformação
    nc    : Número de colunas para aplicação da transformação
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
  Parâmetros
    Col: Índices das colunas a serem eliminadas
  Métodos chamados
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
  Parâmetros
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
  Parâmetros
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
  Parâmetros
    NC: Número de linhas e de colunas. Se NC>0 são inseridos tantos vetores quantas são as linhas
    (colunas). O primeiro vetor tem tamanho 1, o segundo tamanho 2 e assim por diante. O último
    possui tamanho igual ao número de colunas.
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
  Result := 'Estilo de formatação para Matrizes do tipo TwsSymmetric';
end;

function TwsSymmetric.Get(i, j: Integer): Double;
{ Objetivo
    Obtém o elemento na posição especificada da matriz. Numa matriz simétrica
     somente os valores da mtTriangular inferior são armazenados. Se um valor da
     mtTriangular superior for solicitado, retorna uma cópia do elemento
     correspondente na mtTriangular inferior.
  Parâmetros:
    i: Índice da linha
    j: Índice da coluna
}
begin
  if i >= j then
    Get := Row[i].Data[j]
  else
    Get := Row[j].Data[i];
end; { TwsSymmetric.Get }

procedure TwsSymmetric.Put(i, j: Integer; x: Double);
  { Objetivo
    Atribui valor ao elemento na posição especificada da matriz. Numa matriz
    simétrica somente os valores da mtTriangular inferior são armazenados.
  Parâmetros
    i: Índice da linha
    j: Índice da coluna
    x: Valor a ser atribuído à posição (i, j)
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
  Métodos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    Simétrica');
  inherited List(Buffer)
end;

procedure TwsSymmetric.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    Cópia da matriz
  Parâmetros
    MT: Tipo da matriz de retorno
    Matrix: Matriz que retorna com a cópia. Se tipo dependerá de MT (mtGeneral, mtSymmetric,
      mtDiagonal ou mtTriangular). Não é possível copiar uma matriz simétrica como do tipo
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
   Efetuar operações entre uma matriz simétrica e um escalar
 Parâmetros
   x: Escalar para a operação
   Op: Tipo de operação desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas operações (soma ou produto) esse parâmetro não é utilizado.
     No caso de potenciação, SFirst=True indica que o escalar estará na base e o elemento
     na potência; caso contrário, o elemento estará na base e o escalar na potência. Para
     o operador ** se SFirst=False, a operação corresaponderá ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contrário a operação não é definida
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz
 Retorno
   Sempre uma matriz simétrica
 Valores perdidos
   Se houver um valor perdido na operação envolvendo um elemento, o resultado é valor perdido
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
    Uma vez que a transposta de uma matriz simétrica é ela própria, retorna uma cópia da
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
    matriz simétrica sem efetuar a transposicao explicitamente
  Parametros
    A: Matriz geral
    B: Matriz simétrica
    ErrCode: Código de erro. Retorna NImprDim (dimensões impróprias para a operação)
      se (A.NRows <> B.NRows)
  Retorno
    A chamada B.TranspMul5(A, Erro) retorna uma matriz simétrica A'*B*A, uma vez que B é
      simétrica
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
      simétrica sem efetuar a transposicao explicitamente
  Parametros
    A: Matriz geral
    B: Matriz simétrica
    ErrCode: Código de erro. Retorna NImprDim (dimensões impróprias para a operação)
      se (A.NCols <> B.NRows)
  Retorno
    A chamada B.TranspMul10(A, Erro) retorna uma matriz simétrica A*B*A', uma vez que B é
      simétrica
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
    Obtém o fator Triangular de Cholesky
  Parâmetros
    r: Retorna o posto da matriz que chama o método
    NewMat: True se o resultado retorna numa nova matriz; se False o fator Triangular ocupa a
            porção Triangular inferior da matriz
    eps: Precisão numérica para determinação do posto
  Retorno
    A chamada A.CholeskyFat(.,False) transforma A no fator Triangular de Cholesky, armazenado
    na própria matriz (simétrica) A, enquanto A.CholeskyFat(.,True) retorna uma matriz Triangular
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
    Obtém a inversa de uma matriz simétrica a partir do fator de Cholesky. Inicialmente
    a mtTriangular inferior da matriz simétrica é operada de modo a tornar-se o fator
    mtTriangular de Cholesky. A partir desse fator é construída a inversa matriz simétrica.
  Observações
    Se A é uma matriz simétrica, a chamada A.CholeskyFat() obtém o fator mtTriangular de A.
      O método CholeskyInv transforma esse fator na inversa de A.
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
    Obtém a inversa da matriz original através do método de Cholesky
  Parâmetros
    ErrCode: Retorna 0 se a inversão ocorrer sem problemas; maInvError caso a matriz que
      chama seja singular
    eps: Precisão para indicação de matriz singular
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
      Aplica sweep numa matriz simétrica
    Parâmetros
      k: coluna para aplicação do sweep
      DMin: tolerância ara aplicação do sweep
      v: vetor de 1´s e -1´s com a mesma dimensão do número de coluna da matriz. Se v[i]=1
         a coluna i ainda não foi operada; -1 a operação já foi realizada
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
    Obtém forma de Hermite após aplicação do sweep
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
    Obtém forma de Hermite em uma matriz já operada pelo sweep
  Parâmetros
    v: vetor de 1´s e -1´s decorrentes da aplicação do sweep
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
    Obtém a inversa G2 através de operações pelo sweep
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
    Obtém uma inversa G2 em uma matriz já operada pelo sweep
  Parâmetros
    v: vetor de 1´s e -1´s decorrentes da aplicação do sweep
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
  Parâmetros
    k: Índice da coluna pivo
    DMin: Controle para operação da coluna pivo.
    v: Na entrada a posição k contém -1 se a coluna já foi operada ou 1 se
    ainda não foi.
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
    Obtem as tolerâncias para a aplicacao do método RevSweep.
}
var
  i: Integer;
begin
  for i := 1 to FNCols do F[i] := Get(i,i)*eps;
end;

procedure TwsSymmetric.Sweep(Col,v: TwsLIVec);
  { Objetivo
    Aplica o método RevSweep em colunas especificadas.
  Parâmetros
    Col: Colunas onde serão aplicados os sweeps
    v: Vetor que contém valor -1 se a coluna respectiva já foi alterada e 1 caso contrário. O sinal é trocado a cada operação.
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
    Aplica o método RevSweep sequencialmente nas colunas especificadas.
  Parâmetros
    k1, k2: Colunas inicial e final onde serão aplicados os sweeps
    v: Vetor que contém valor -1 se a coluna respectiva já foi alterada e 1
    caso contrário. O sinal é trocado a cada operação.
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
  Parâmetros
    C: Colunas onde o sweep será aplicado
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
    Resolve um sistema simétrico AX = B, onde A é a matriz que chama o método,
    atraves da decomposicao de Cholesky. Na entrada A aramzena os coeficientes
    do sistema e na saída retorna o fator de Cholesky(na parte mtTriangular inferior
    de uma matriz simétrica)
  Parâmetros
    B: Na entrada, armazena em cada coluna os vetores do segundo membro e, na
      saida, retorna a solucao
    ErrCode: Código de erro ====> Observar bem o código de erro em situacoes
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
  //While Count <> 0 Do Delete(0);   {Não sei por que ????}
  Reader.ReadListBegin;
  While Not Reader.EndOfList Do
    PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;
end;

{ TwsTriangular }

constructor TwsTriangular.Create(NC: Integer);
{ Objetivo
    Cria e inicializa um objeto matriz mtTriangular com dimensão especificada.
    Uma matriz mtTriangular somente os valores da mtTriangular inferior são armazenados.
    Assim, o primeiro vetor (linha) tem dimensão um, o segundo dois e assim por diante.
    Se um valor da mtTriangular for necessário, o método de acesso retorna 0 (zero).
  Parâmetros
    NC: Número de linhas e colunas da matriz
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
  Result := 'Estilo de formatação para Matrizes do tipo TwsTriangular';
end;


function TwsTriangular.Get(i, j: Integer): Double;
{ Objetivo
    Retornar o valor da posição especificada
  Parâmetros
    i, j: Linha e coluna, respectivamente, do valor. Se i < j (mtTriangular superior)
    o valor retornado é zero.
}
begin
  if i >= j then
    Get := Row[i].Data[j]
  else
    Get := 0
end; { TwsTriangular.Get }

procedure TwsTriangular.Put(i, j: Integer; x: Double);
{ Objetivo
    Atribui um valor à posição especificada
  Parâmetros
    i, j: Linha e coluna, respectivamente, para atrubuição do valor.
    x: Valor a ser atribuído. Atribui somente se i <= j
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
  Métodos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtTriangular');
  inherited List(Buffer)
end;

procedure TwsTriangular.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    Copia matriz mtTriangular.
  Parâmetros
    MT: tipo para a qual será copiada a matriz. MT deverá um dos tipos
      mtGeneral
      mtSymmetric
      mtDiagonal
      mtTriangular
      mtToeplitz
      mtVandermonde
    Matrix: local onde retorna a cópia
  Métodos chamados
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
    Faz o produto da matriz pela transposta da outra sem que a transposição seja
    explicitamente realizada
  Parâmetros
    B: Matriz que será transposta para o produto
    ErrCode: Código de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contrário
  Retorno
    Se A é a matriz que chama o método, retorna uma matriz geral com o produt AB'
  Valores perdidos
    Não trata
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
    Produto de uma matriz traingular transposta por uma outra sem que a transposição seja
    explicitamente realizada
  Parâmetros
    B: Matriz com a qual o produto será realizado
    ErrCode: Código de erro. retorna 0 (zero) se NRows = B.NRows e NImprDim (dimensões
    impróprias para a operação), caso contrário
  Retorno
  Valores perdidos
    Não considera
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
    Faz o produto da matriz (geral, simétrica, mtVandermonde ou mtToeplitz) pela sua transposta
    sem que a transposição seja explicitamente realizada
  Retorno
    Retorna a matriz simétrica AA', exceto se for mtDiagonal. Nesse caso, o retorno é uma
    matriz mtDiagonal
  Valores perdidos
    Não trata
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
    Produto de uma matriz transposta por si mesma sem que a transposição seja explicitamente
    realizada
  Retorno
    Retorna uma matriz simétrica exceto no caso em que a matriz é mtDiagonal
  Valores perdidos
    Não considera
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
    Dadas a matriz gerais A qualquer e B mtTriangular (que chama o método), retorna o produto
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
  Parâmetros
    v: Vetor para o produto
    ErrCode: Código de erro. Se v.Len=NRows=NCols retorna 0; caso contrário retorna
    NImprDim (dimensões impropróprias para a operação)
  Retorno
    Se A é a matriz que chama retorna o escalar correspondente ao produto v'Av; se o produto
    não for possível retorna wscMissValue
  Valores perdidos
    Não trata
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
    Dadas a matriz gerais A qualquer e B mtTriangular (que chama o método), retorna o produto
    ABA' sem efetuar a transposicao de A explicitamente.
  Parametros
    A: matriz para o produto
    ErrCode: Codigo de erro. Retorna NImprDim (dimensoes improprias para a operacao)
    se (A.NCols <> B.NRows)
  Retorno
    Retorna uma matriz geral exceto quando A é mtDiagonal, quando o retorno é mtTriangular
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
    é a coluna.
  Parâmetros
    B: Matriz para o produto
    k: Índice da coluna para o produto
    ErrCode: Código de erro. Se NRows=NCols=A.NRows retorna 0; caso contrário retorna
      NImprDim (dimensões impropróprias para a operação
  Retorno
    Se o produto não for possível, retorna wscMissValue
  Valores perdidos
    Não trata
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
    Metodo para o produto de matrizes. Utilizado quando a matriz que chama o metodo é
    mtTriangular
  Parâmetros
    B: Matriz para o (pos)produto com a matriz que chama o método
    ErrCode: Código de erro. O produto somente sera possível (ErrCode retorna 0) se o
      número de colunas de quem chama é igual ao número de linhas de B; caso contrário
      ErrCode retorna a constante NImprDim (Dimensões impróprias para a operação) e a
      função retorna nil
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
 Parâmatros
   A: Vetor para o produto
   PreMult: True se o vetor premultiplicar a matriz; False caso contrario
   ErrCode: Código de erro. Retorna 0 se o produto for possível e NImprDim (dimensões
     impróprias para a operação) se a) número de componentes do vetor diferente do número
     de linhas da matriz (PreMult=True); b) número de colunas da matriz diferente do número
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
   Efetuar operações entre uma matriz mtTriangular e um escalar
 Parâmetros
   x: Escalar para a operação
   Op: Tipo de operação desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas operações (soma ou produto) esse parâmetro não é utilizado.
     No caso de potenciação, SFirst=True indica que o escalar estará na base e o elemento
     na potência; caso contrário, o elemento estará na base e o escalar na potência. Para
     o operador ** (se SFirst=False), a operação corresponderá ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contrário a operação não é definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz.
 Retorno
   O retorno ira depender do tipo de operação desejada. Para alguns tipos, o retorno será
     mtDiagonal enquanto que para outros será geral. A relação será:
     opDiv: Se SFirst=False retorna matriz mtTriangular; geral caso contrário
     opProd: mtTriangular
     opPower: Se SFirst=False retorna matriz mtTriangular; geral caso contrário
     opSum,opSub,opGE,opGT,opLE,opLT,opEQ,OpNE,OpOR,OpAnd,OpMax,OpMin: Geral
   Nas situações onde não é possível o retorno de uma mtDiagonal, sempre retornará uma matriz
     geral, independemente de NewMat
 Valores perdidos
   Se houver um valor perdido na operação envolvendo um elemento, o resultado é valor perdido
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
  Parâmetros
    ErrCode: Retorna 0 se a inversão ocorre sem problemas; maInvError caso contrário
    eps: Precisão numérica para indicação de matriz singular. A condição para que a matriz
      seja considerada singular é ter um elemento da mtDiagonal menor que eps.
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
    Resolve um sistema triangular para frente, isto é, da primeira à última equação 
  Parâmetros
    B: Na entrada, armazena em cada coluna os vetores do segundo membro e, na
      saida, retorna a solucao
    ErrCode: Código de erro ====> Observar bem o código de erro em situacoes
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
  //While Count <> 0 Do Delete(0);   {Não sei por que ????}
  Reader.ReadListBegin;
  While Not Reader.EndOfList Do
    PutLine(TwsVec.VFromStream(Reader));
  Reader.ReadListEnd;
end;

{ TwsDiagonal }

{ Inicializa objeto TwsDiagonal }

constructor TwsDiagonal.Create(NC: Integer);
{ Objetivo
    Cria uma matriz mtDiagonal de dimensão especificada. Numa matriz mtDiagonal apenas os elementos
    da mtDiagonal são armzenados. O método de acesso se encarrega de zerar o resultado quando se
    desejar algum valor fora da mtDiagonal
  Parâmetros
    NC: Número de linhas e colunas da matriz. Se NC>0 insere uma linha vazia
  Métodos chamados
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
  Result := 'Estilo de formatação para Matrizes do tipo TwsDiagonal';
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
    armazenamento, onde são colocados os valores da mtDiagonal.
  Parâmetros
    i: Índice de linha
    j: Índice de coluna do elemento.
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observação
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
    Atribui um elemento da matriz mtDiagonal. A atribuição somente será feita se
    for na mtDiagonal
  Parâmetros
    i, j: Respectivamente, linha e coluna para atribuição do elemento.
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
  Métodos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtDiagonal');
  inherited List(Buffer)
end;

function TwsDiagonal.GetStr(i,j:Integer): String;
{ Objetivo
    Retorna um elemento como string.
  Parâmetros
    i: índice de linha
    j: índice de coluna
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observação
    Retorna ´-´ se for valor perdido. Aplica a função Fuzz para eliminar dígitos que estão
    fora da precisão especificada.
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
    Lista a matriz mtDiagonal com saída no modo texto
  Métodos chamados
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
     respectiva satisfaz à condição estabelecida
  Parâmetros
    Cols: String que estabelece as colunas que serão impressas
    Condicao: Expressão que irá indicar se a linha será impressa ou não.
     (Ver Expressões)
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
    Copia todo conteúdo (incluindo 0´s fora da mtDiagonal) para um vetor
  Métodos chamados
    ToVec
  Campos modificados
    Nenhum
}
begin
  Result := ToVec
end;

procedure TwsDiagonal.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    Cópia da matriz
  Parâmetros
    MT: Tipo da matriz de retorno
    Matrix: Matriz que retorna com a cópia. Se tipo dependerá de MT (mtGeneral, mtSymmetric,
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
    Faz o produto da matriz mtDiagonal pela transposta da outra sem que a transposição seja
    explicitamente realizada
  Parâmetros
    B: Matriz que será transposta para o produto
    ErrCode: Código de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contrário
  Retorno
    Se A é a matriz que chama o método, retorna
      - uma matriz geral com o produt AB' se B for geral, simétrica, mtTriangular, mtVandermonde
        ou mtToeplitz
      - uma matriz mtDiagonal se B for mtDiagonal
  Valores perdidos
    Não trata
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
    Faz o produto da matriz mtDiagonal transposta por uma outra sem que a transposição seja
    explicitamente realizada
  Parâmetros
    B: Matriz para o produto
    ErrCode: Código de erro. Retorna 0 (zero) se NCols = B.NCols e NImprDim caso contrário
  Retorno
    Se A é a matriz que chama o método, retorna uma matriz geral com o produt A'B
  Valores perdidos
    Não trata
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
    Faz o produto da matriz (geral, simétrica, mtVandermonde ou mtToeplitz) pela sua transposta
    sem que a transposição seja explicitamente realizada
  Retorno
    Retorna a matriz simétrica AA', exceto se for mtDiagonal. Nesse caso, o retorno é uma
    matriz mtDiagonal
  Valores perdidos
    Não trata
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
    Produto de uma matriz transposta por si mesma sem que a transposição seja explicitamente
    realizada
  Retorno
    Retorna uma matriz simétrica exceto no caso em que a matriz é mtDiagonal
  Valores perdidos
    Não considera
}
begin
  Prod4(Self,Self,TwsDiagonal(Result));
end; { TranspMul4 }

function TwsDiagonal.TranspMul5(A:TwsMatrix; var ErrCode:Word):TwsMatrix;
{ Objetivo
    Se D é a matriz mtDiagonal que chama o método, encontra o produto A'DA sem que a
    transposição seja explicitamente realizada
  Parâmetros
    A: Matriz para o produto
    ErrCode: Código de erro. Se NRows = D.NRows retorna 0; caso contrário retorna NImprDim
    (dimensões impróprias para a operação)
  Retorno
    Se A é mtDiagonal o retorno será uma matriz mtDiagonal; caso contrário retorna uma matriz
    simétrica.
  Valores perdidos
    Não considera
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
  Parâmetros
    v: Vetor para o produto
    ErrCode: Código de erro. Se v.Len=NRows=NCols retorna 0; caso contrário retorna
    NImprDim (dimensões impropróprias para a operação
  Retorno
    Se A é a matriz que chama retorna o escalar correspondente ao produto v'Av; se o produto
    não for possível retorna wscMissValue
  Valores perdidos
    Não trata
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
    Se D é a matriz mtDiagonal que chama o método, encontra o produto ADA' sem que a
    transposição seja explicitamente realizada
  Parâmetros
    A: Matriz para o produto
    ErrCode: Código de erro. Se A.NCols=NRows retorna 0; caso contrário retorna NImprDim
    (dimensões impróprias para a operação)
  Retorno
    Se A é mtDiagonal o retorno será uma matriz mtDiagonal; caso contrário retorna uma matriz
    simétrica.
  Valores perdidos
    Não considera
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
    é a coluna.
  Parâmetros
    B: Matriz para o produto
    k: Índice da coluna para o produto
    ErrCode: Código de erro. Se NRows=NCols=A.NRows retorna 0; caso contrário retorna
      NImprDim (dimensões impropróprias para a operação
  Retorno
    Se o produto não for possível, retorna wscMissValue
  Valores perdidos
    Não trata
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
    Operação elemento a elemento de uma matriz mtDiagonal por uma matriz qualquer
  Parâmetros
    B: Matriz para operação
    Op: Operação desejada
    NewMat: True se resultado retorna na mesma matriz; False caso contrário. NewMat
      somente será considerado se o retorno (veja abaixo) for mtDiagonal.
    ErrCode: Código de erro. Retorna NImprDim (dimenões impróprias para a operação) se as
      matrizes não forem de mesma dimensão; caso contrário retorna 0 (zero)
  Retorno: O retorno depende do tipo da matriz B e do tipo de operador especificado. As
    situações são:
      opSum,opSub,opDiv:
        Caso B seja
          geral, simétrica, mtDiagonal, mtTriangular: o resultado tem o mesmo tipo de B
          mtVandermonde, mtToeplitz: o resultado é sempre uma matriz geral
      opProd, opPower: retorno é uma matriz mtDiagonal
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
    Metodo para o produto de matrizes. Utilizado quando a matriz que chama o metodo é
    mtDiagonal
  Parâmetros
    B: Matriz de qualquer tipo para o (pos)produto com a matriz que chama o método
    ErrCode: Código de erro. O produto somente sera possível (ErrCode retorna 0) se o
      número de colunas de quem chama é igual ao número de linhas de B; caso contrário
      ErrCode retorna a constante NImprDim (Dimensões impróprias para a operação) e a
      função retorna nil
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
 Parâmatros
   v: Vetor para o produto
   PreMult: True se o vetor premultiplicar a matriz; False caso contrario
   ErrCode: Código de erro. Retorna 0 se o produto for possível e NImprDim (dimensões
     impróprias para a operação) se a) número de componentes do vetor diferente do número
     de linhas da matriz (PreMult=True); b) número de colunas da matriz diferente do número
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
   Efetuar operações entre uma matriz mtDiagonal e um escalar
 Parâmetros
   x: Escalar para a operação
   Op: Tipo de operação desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas operações (soma ou produto) esse parâmetro não é utilizado.
     No caso de potenciação, SFirst=True indica que o escalar estará na base e o elemento
     na potência; caso contrário, o elemento estará na base e o escalar na potência. Para
     o operador ** (se SFirst=False), a operação corresponderá ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar (observe que, neste caso, a matriz deve
     ser quadrada para que o produto seja sempre definido); caso contrário a operação não
     é definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz.
 Retorno
   O retorno ira depender do tipo de operação desejada. Para alguns tipos, o retorno será
     mtDiagonal enquanto que para outros será geral. A relação será:
     opDiv: Se SFirst=False retorna matriz mtDiagonal; geral caso contrário
     opProd: mtDiagonal
     opPower: Se SFirst=False retorna matriz mtDiagonal; geral caso contrário
     opSum,opSub,opGE,opGT,opLE,opLT,opEQ,OpNE,OpOR,OpAnd,OpMax,OpMin: Geral
   Nas situações onde não é possível o retorno de uma mtDiagonal, sempre retornará uma matrtiz
     geral, independemente de NewMat
 Valores perdidos
   Se houver um valor perdido na operação envolvendo um elemento, o resultado é valor perdido
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
    Obtem a transposta da matriz que chama o método
  Retorno
    Uma vez que a transposta de uma matriz mtDiagonal é ela própria, retorna uma cópia da
    matriz que chamou
}
begin
  Copy(mtDiagonal,Result);
end;

procedure TwsDiagonal.Inv(var ErrCode: Word; eps: Double=1.0e-8);
{ Objetivo
    Obter a inversa da matriz (inverso dos elementos da mtDiagonal)
  Parâmetros
    ErrCode: Retorna 0 se inversão é feita sem problemas; maInvError caso contrário
    eps: Precisão para indicação de matriz singular. A matriz será considerada singular se
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
    Resolve sistema de equações onde a matriz dos coeficientes é mtDiagonal.
  Parâmetros
  B: Matriz onde, na entrada, cada coluna corresponde a um vetor do segundo
   membro do sistema. Na saída armazema em cada coluna a solução do sistema respectivo.
  ErrCode: Código de erro
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
    Escreve a matriz num arquivo através do objeto de escrita. É um método virtual
    chamado por SaveToFile.
  Parâmetros
    Writer: Objeto responsável pela escrita no arquivo
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
  Parâmetros
    Col: Índices das colunas a serem eliminadas
  Métodos chamados
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
    vetor de valores com a mesma dimensão da matriz. Os demais elementos podem ser obtidos
    através de potências desses valores. Desse modo apenas um vetor é utilizado para armazenar
    essa matriz.
  Parâmetros
    NC: Dimensão da matriz. Se NC>0 insere uma linha (vetor) em branco.
  Métodos chamados
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
  Result := 'Estilo de formatação para Matrizes do tipo TwsVandermonde';
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
    Libera a memória ocupada por uma matriz de mtVandermonde
  Métodos chamados
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
    Obtém um elemento da matriz de mtVandermonde na posição especificada
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
    Obtém o nome da linha espwecificada
  Parâmetros
    i: Índice da linha
  Métodos chamados
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
  Parâmetros
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
  // Completa se for necessário
  for i:=RName.Count+1 to FNRows do
    RName.Add('L'+IntToStr(i))
end; { TwsVandermonde.SetRName }

procedure TwsVandermonde.Put(i, j: Integer; x: Double);
  { Objetivo
    Atribui um valor à posição especificada
  Parâmetros
  i, j: Índice da linha e coluna, respectivamente, para atribuição. Como
      para este tipo de matriz somente um vetor armazena todos os valores, o
      valor é atribuído à posição j do vetor.
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
  Parâmetros
    L: Linha da matriz
    Start: Índice do primeiro valor que será colocado no string
    Amt: Quantos valores serão colocados no string
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
    linha respectiva satisfaz à condição estabelecida
  Parâmetros
    Cols: String que estabelece as colunas que serão impressas
    Condicao: Expressão que irá indicar se a linha será impressa ou não.(Ver Expressões)
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
  Métodos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtVandermonde');
  inherited List(Buffer)
end;

procedure TwsVandermonde.Copy(MT: TwsEnumMatType; var Matrix: TwsMatrix);
{ Objetivo
    Cópia da matriz
  Parâmetros
    MT: Tipo da matriz de retorno
    Matrix: Matriz que retorna com a cópia. Se tipo dependerá de MT (mtGeneral, mtSymmetric,
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
   Efetuar operações entre uma matriz mtTriangular e um escalar
 Parâmetros
   x: Escalar para a operação
   Op: Tipo de operação desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas operações (soma ou produto) esse parâmetro não é utilizado.
     No caso de potenciação, SFirst=True indica que o escalar estará na base e o elemento
     na potência; caso contrário, o elemento estará na base e o escalar na potência. Para
     o operador ** (se SFirst=False), a operação corresponderá ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contrário a operação não é definida.
   NewMat: Não será considerado neste caso. Sempre retornará uma nova matriz.
 Retorno
   O retorno será sempre uma matriz geral
 Valores perdidos
   Se houver um valor perdido na operação envolvendo um elemento, o resultado é valor
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

  ===> Observar algoritmo na página 55 de Press. Saída está em local
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
    Responsável de fato pela leitura do objeto no disco. Chamado por MFromStream.
  Parâmetros
    Reader: Objeto responsável pela leitura do objeto armazenado
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
    Grava o objeto num arquivo. Chamado pelo método SaveToFile.
  Parâmetros
    Writer: Objeto responsável pela escrita do conteúdo da matriz no arquivo
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
    Cria uma matriz de mtToeplitz. Uma matriz de mtToeplitz n x n é completamente especificada por
    2n-1 valores (armazenados num vetor) e não precisa ser necessariamente simétrica.
  Parâmetros
    nc: número de colunas (e linhas) da matriz. Se nc>, insere um vetor de ordem 2*nc-1 em
    branco.
  Métodos chamados
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
  Result := 'Estilo de formatação para Matrizes do tipo TwsToeplitz';
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
    Libera espaço ocupado pelo objeto[
  Métodos chamados
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
    Retorna o valor na posição especificada
  Parâmetros
    i, j: Respectivamente, índices da linha e coluna do valor. O valor
    retornado está na posição NCol - j + i
}
begin
  Result := Row[1].Data[FNCols - j + i]
end; { TwsToeplitz.Get }

function TwsToeplitz.GetRowName(i: Integer): String;
{ Objetivo
    Retorna nome da linha especificada
  Parâmetros
    i: índice da linha
  Métodos chamados
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
  Parâmetros
    st     : string com os nomes de linhas
    DelChar: Conjunto de caracteres separadores de nomes de linhas.
  Métodos chamados
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
  // Completa se for necessário
  for i:=RName.Count+1 to FNRows do
    RName.Add('L'+IntToStr(i))
end; { TwsToeplitz.SetRName }

procedure TwsToeplitz.Put(i,j: Integer; x: Double);
  { Objetivo
    Atribui um valor na posição especificada
  Parâmetros
    i, j: Índices da linha e coluna
    x: Valor a ser atribuído. O valor será atribuído à posição NCol - j + i do
    vetor que armazena todos os valores da matriz.
}
begin
  FModified := True;
  Row[1].Data[FNCols - j + i] := x
end; { TwsToeplitz.Put }

procedure TwsToeplitz.List(Buffer: TStrings);
{ Objetivo
    Lista descritores pertinentes à matriz
  Métodos chamados
    List herdado
}
begin
  Buffer.Add('Tipo:    mtToeplitz');
  inherited List(Buffer)
end;

function TwsToeplitz.ToChar(L, Start, Amt: Integer; Buffer: TStrings): Integer;
  { Objetivo
    Transforma os elementos especificados de uma linha num string
  Parâmetros
    L: Linha da matriz
    Start: Índice do primeiro valor que será colocado no string
    Amt: Quantos valores serão colocados no string
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
    satisfaz à condição estabelecida
  Parâmetros
    Cols: String que estabelece as colunas que serão impressas
    Expr: Expressão que irá indicar se a linha será impressa ou não. (Ver Expressões)
  Métodos chamados
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
    Retorna uma cópia de uma matriz de mtToeplitz
  Parâmetros
    MT: Tipo da matriz de retorno. Se MT for
        mtGeneral: Matriz de mtToeplitz retornará numa matriz geral
        mtSymmetric: Matriz de mtToeplitz retornará numa matriz simétrica
        mtTriangular: Matriz de mtToeplitz retornará numa matriz mtTriangular
        mtDiagonal: Matriz de mtToeplitz retornará somente os elementos da mtDiagonal
        mtToeplitz: Matriz de mtToeplitz retornará numa matriz de mtToeplitz
        mtVandermonde: Não é possível copiar uma matriz de mtToeplitz numa matriz de mtVandermonde
    Matrix: Retorna a cópia
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
   Efetuar operações entre uma matriz de mtToeplitz e um escalar
 Parâmetros
   x: Escalar para a operação
   Op: Tipo de operação desejada
   SFirst: True se a operacao for (escalar Op elemento); false se for (elemento Op escalar).
     Observe que para algumas operações (soma ou produto) esse parâmetro não é utilizado.
     No caso de potenciação, SFirst=True indica que o escalar estará na base e o elemento
     na potência; caso contrário, o elemento estará na base e o escalar na potência. Para
     o operador ** (se SFirst=False), a operação corresponderá ao produto da matriz tantas
     vezes quanto for a parte inteira do escalar; caso contrário a operação não é definida.
   NewMat: True se o resultado deve retornar numa nova matriz; False se o resultado deve
     retornar na mesma matriz.
 Retorno
   O retorno sempre será uma mariz de mtToeplitz
 Valores perdidos
   Se houver um valor perdido na operação envolvendo um elemento, o resultado é valor perdido
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
    Resolve sistema de equações lineares onde a matriz dos coeficientes é uma matriz de mtToeplitz.
  Parâmetros
    B: Matriz cujas colunas definem um sisema de equações lineares do tipo Ax=B[i]
    ErrCode: Retorna 0 se solução foi feita sem problemas e maToepError caso contrário
}
{ =======> Conferir este algoritmo. Pág. 59 de Press }
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
    Responsável de fato pela leitura do objeto no disco. Chamado por MFromStream.
  Parâmetros
    Reader: Objeto responsável pela leitura do objeto armazenado
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
    Grava o objeto num arquivo. Chamado pelo método SaveToFile.
  Parâmetros
    Writer: Objeto responsável pela escrita do conteúdo da matriz no arquivo
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
    Insere um objeto numa lista de strings. Redefine método para inserção de um objeto
    do tipo TwsDatasetCol na lista. Verifica se já existe um objeto com o nome especificado e
    se existir, modifica o nome
  Parâmetros
    S: nome do objeto para inserção
    AObject: Objeto para inserção
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
    Cria objeto gerenciador da estrutura de um conjunto de dados. A lista criada é do
    do tipo TListCols
  Métodos chamados
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
    Libera espaço ocupado pelo objeto gerenciador da estrutura do conjunto de dados
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
  TListCols(Result.Cols).Duplicates := dupError; {Não permite colunas repetidas}
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
    Recupera endereço de uma coluna
  Parâmetros
    i: Índice da coluna. Baseado em 1.
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
    Substitui endereço de uma coluna
  Parâmetros
    i: Índice da coluna. Baseado em 1.
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
    Retorna o número de fatores presenets no conjunto de dados
}
Var i: Integer;
Begin
  Result := 0;
  For i := 0 to Cols.Count - 1 do
    If Col[i+1].ColType <> dtNumeric Then Inc(Result);
End;

Function TwsDSStructure.ColByName(Const N: String): TwsDataSetCol;
{ Objetivo
    Retorna endereço da coluna com nome especificado
  Parâmetros
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
    Retorna índice da coluna com nome especificado. Baseado em 1.
  Parâmetros
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
    Retorna um vetor com os índices das colunas cujos nomes estão num string. Os nomes deverão
    estar separados por vírgulas.
  Parâmetros
    Cols: strinng com os nomes
  Métodos chamados
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
                                   'Método: %s'#13 +
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
   Parâmetro
     Col: Objeto a ser adicionado
   Métodos chamados
     AddColDesc
     AddObject
}
Begin
  Col.Struct := Self;
  Col.DataSet := DataSet;
  If DataSet <> Nil Then
     DataSet.AddColDesc(Col)
  Else {Se o dataSet não existe}
     Cols.AddObject('', Col);
End;

Procedure TwsDSStructure.AddColEx(Col: TwsDataSetCol); {Rochedo, 04/04/98}
{ Objetivo
    Adiciona objeto descritor sem alterar o conteúdo das linhas
  Parâmetros
    Col: objeto a adicionar
  Métodos chamados
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
  Else {Se o dataSet não existe}
     Cols.AddObject('', Col);
End; // AddColEx

Procedure TwsDSStructure.AddCopyCol(Col: TwsDataSetCol); {Rochedo, 12/03/98}
{ Objetivo
    Adiciona uma cópia de um objeto descritor de colunas
  Parâmetros
    Col: objeto cuja cópia será adicionada
  Métodos chamados
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
  Else {Se o dataSet não existe}
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
    Adiciona um descritor de variável numérica
  Parâmetros
    N: Nome da coluna
    Lab: rótulo da coluna
    ps: largura de impressão
    pf: precisão ou dígitos significativos para impressão
  Métodos chamados
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
  Parâmetros
    N: nome da coluna
    Lab: rótulo da coluna
    ps: largura de impressão
  Métodos chamados
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
  Parâmetros
    N: nome da coluna
    Lab: rótulo da coluna
    ps: largura de impressão
  Métodos chamados
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
  Parâmetros
    N: nome da coluna
    Lab: rótulo da coluna
    ps: largura de impressão
  Métodos chamados
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
    Libera espaço ocupado por um objeto descritor
  Parâmetros
    N: nome da coluna
  Métodos chamados
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
                               'Método: %s'#13 +
                               MsgInvalidNameVar,
                               [ClassName, 'Delete', N]);

  Delete(i);
End; { TwsDSStructure.Delete }

{ TwsDataSet }

Constructor TwsDataSet.Create(aName: String='NoName');
{ Objetivo
    Cria um objeto para armazenar um conjunto de dados
  Parâmetros
    aName: nome do conjunto de dados
    nr: número de linhas
    nc: número de colunas
  Métodos chamados
    Create herdado
  Campos alterados
    FMatType
    Name
    ColIdentName
    CName
    Struct
  Observação
    Com muita frequência, este objeto é criado com nr=0 e nc=0 e as colunas (por meio de
    algum método de adição de colunas) e linhas (por meio do método MAdd) são adicionados
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
    Cria um objeto conjunto de dados utilizando uma estrutura já criada.
  Parâmetros
    aName: nome do conjunto de dados
    nr: número de linhas
    Struc: estrutura do conjunto
  Métodos chamados
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
    Cria um objeto conjunto de dados utilizando um array de descritores de colunas já disponível.
  Parâmetros
    aName: nome do conjunto de dados
    nr: número de linhas
    ColT: array de colunas para o conjunto
  Métodos chamados
    Create herdado
  Campos alterados
     FMatType
     name
     ColIdentName
     Struct
}
Var i: Integer;
Begin
  { Se for passado zero no segundo parâmetro, CName não é criado. Ver TwsMatrix.Create }
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
    Libera espaço ocupado por um conjunto de dados
  Campos liberados
    Struct
  Métodos chamados
    Detroy herdado
}
Begin
  Struct.Free;
  CName := nil; {Evitará crash nos demais "CName(s).Free"}
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
    Obtém uma cópia do conjunto de dados
  Métodos chamados
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

    // Cria uma cópia de colunas especificas do conjunto de dados
Function  TwsDataSet.CopyByCols(Col: TwsLIVec): TwsDataSet;
{ Objetivo
    Copia colunas especificas do conjunto de dados
  Parâmetros
    Col: índices das colunas que serão copiadas
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
  Parâmetros
    name: nome para coluna
  Métodos chamados
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
    Atribui nome válido para o conjuto de dados
  Parâmetros
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
  Parãmetros
    L: índice da linha
    Start: Coluna inicial
    Amt: número de colunas
  Métodos chamados
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
    Cria cabeçalho para listagem de conjuntos de dados no formato texto
  Parâmetros
    Start: coluna inicial
    Amt: número de colunas
  Métodos chamados
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
       P := Pad('Variável',13);  {13}
       P := P+Pad('Rótulo',50); {63}
       P := P+Pad('Tipo',10);   {73}
       P := P+LeftPad('Níveis',9); {82}
       P := P+LeftPad('T.Nível',10);{92}

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
    Retorna um conjunto de estatísticas para cada nível de cada fator indicado.
  Parâmetros
    IFac : Índices dos fatores para cujos níveis as estatísticas serão calculadas
    VIdx : Índice da variavel para a qual serão calculadas as estatísticas
    GMean: Média geral global
    Min  : Menor valor global
    Max  : Maior valor global
  Mátodos chamados
    Nenhum
  Campos alterados
    Nenhum
  Retorno
    Retorna uma matriz com as estatisticas. Terá tantas linhas quantos são os fatores e
    tantas colunas quanto for o número de niveis vezes o número de estatisticas, As
    estatísticas são (por nível, nessa ordem): média, valor mínimo, valor máximo, soma de
    quadrados de desvios e número de observações válidas.

  Exemplo: A com 2 niveis e B com 3 níveis. O reultado será:

     Media  Minim  Maxim   SQD   Repet
     -----  -----  -----  -----  -----
  A  A1 A2  A1 A2  A1 A2  A1 A2  A1 A2                  Linha 1
     Media     Minim     Maxim     SQD       Repet
     --------  --------  --------  --------  --------
  B  B1 B2 B3  B1 B2 B3  B1 B2 B3  B1 B2 B3  B1 B2 B3   Linha 2

  Valores perdidos
    Os valores perdidos são excluídos dos cálculos

  Observação
    Note que o número de colunas dependerá do número de níveis do fator respectivo. Assim,
    o número de colunas poderá variar em função do número de níveis de cada fator.
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
        Result[j,cl] := Result[j,cl] + v;       { Média }
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
    Calcula a media de linhas especificadas de todos os níveis de uma matriz para as colunas
    especificadas.
  Parâmetros:
    Fac : Índice do fator para cujos níveis serão calculadas as médias.
    IRow: Linha inicial
    LRow: Linha final
    Col : Indices das colunas para as quais serao calculadas as medias
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
  Retorno:
    Retorna uma matriz com Col.Len*2 colunas. Cada par de colunas armazenará a
    média de cada nível do fator Fac e o respectivo número de repetições.

  Exemplo:
    Para um fator A com 4 níveis e duas variáveis y1 e y2, o resultado final ficaria:

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
  { Nomes dos níveis nas linhas da matriz de saída }
  for i := 1 to n do
    Result.RowName[i] := TwsFactor(Self.Struct.Col[Fac]).LevelNames[i-1];

  { Nomes das variáveis dependentes e repetições }
  for i := 0 to Col.Len-1 do
    begin
    k:=2*i+1;
    Result.ColName[k] := Self.Struct.Col[Col[i+1]].Name;
    Result.ColName[k+1]:=System.Copy(Result.ColName[k],1,4)+'_n';
    end;

  for i:=IRow to LRow do                    { Médias desde as linhas IRow até LRow }
    begin
    kk := Self.AsInteger[i,Fac]+1;          { kk é o índice do nível do fator Fac  }
    for k := 1 to Col.Len do                { Esgota para todas as variáveis dependentes }
      begin
      kv := 2*(k-1)+1;
      kv1 := kv+1;
      aux :=  Self[i,Col[k]];
      if not wsGLib.IsMissValue(Aux) then
        begin
        Result[kk,kv1] := Result[kk,kv1]+1;                  { Número de repetições }
        aux := aux - Result[kk,kv];
        Result[kk,kv]:=Result[kk,kv]+aux/Result[kk,kv1]      { Calcula a media }
        end
      end { for k }
    end { for i }
end;

function TwsDataSet.FacVarMean(Fac: TwsLIVec; IRow,LRow: Integer; Col: TwsLIVec): TwsGeneral;
{ Objetivo:
    Retorna a media,a variância e o número de repetições dos níveis dos fatores para as
    variáveis especificadas.
  Parâmetros:
    Fac : Índices do fatores para cujos níveis serão calculadas as estatísticas.
    IRow: Linha inicial do conjunto de dados
    LRow: Linha final do conjunto de dados
    Col : Indices das colunas para as quais serao calculadas as estatísticas
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
  Retorno:
    Retorna uma matriz com Col.Len*3 colunas. Cada três colunas armazenarão a média, a
    variância e o número de observações válidas de cada nível.

  Exemplo:
  Para um fator A com 4 níveis, B com 2 níveis e duas variáveis preditoras y1 e y2, o
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
  { Nomes dos níveis nas linhas da matriz de saída }
  for kf:=1 to Fac.Len do
    begin
    C:=Self.Struct.Col[Fac[kf]];
    s:=System.Copy(C.Name,1,3)+'-';
    for i := 1 to TwsFactor(C).LevelNames.Count do
      Result.RowName[i] := s+TwsFactor(C).LevelNames[i-1];
    end;

  { Nomes das variáveis dependentes (para media) e repetições }
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
      for i:=IRow to LRow do                    // Médias desde as linhas IRow até LRow
        begin
        kk := kn+Self.AsInteger[i,Fac[kf]]+1;   // kk é a linha do resultado
        for k := 1 to Col.Len do                // Para todas as variáveis dependentes
          begin
          kv := 3*k-2;                          // Coluna para a media
          kv1 := kv+1;                          // Coluna para a variancia
          kv2 := kv+2;                          // Coluna para o numero de repeticoes
          aux :=  Self[i,Col[k]];
          if not wsGLib.IsMissValue(Aux) then
            begin
            Result[kk,kv2] := Result[kk,kv2]+1;                        // Número de repetições
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
    Retorna a media, a variância (ponderadas), o número de repetições dos níveis dos fatores
    e a soma dos pesos para as variáveis especificadas.
  Parâmetros:
    Fac : Índices do fatores para cujos níveis serão calculadas as estatísticas.
    IRow: Linha inicial do conjunto de dados
    ICol: Linha final do conjunto de dados
    WInd: Índice da variável peso
    Col : Indices das colunas para as quais serao calculadas as estatisticas
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Retorno:
    Retorna uma matriz com Col.Len*3 colunas. Cada três colunas armazenarão a média, a
      variância e o número de observacoes de cada nível.
  Exemplo:
  Para um fator A com 4 níveis, B com 2 níveis e duas variáveis preditoras y1 e y2, o
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
  { Matriz das medias, variâncias, numero de repeticoes e somas dos pesos }
  Result := TwsGeneral(Jota(n, 4*Col.Len, mtGeneral, 0));
  Result.Name := 'PFatEstat';
  { Nomes dos níveis nas linhas da matriz de saída }
  for kf:=1 to Fac.Len do
    begin
    C:=Self.Struct.Col[Fac[kf]];
    s:=System.Copy(C.Name,1,3)+'.';
    for i := 1 to TwsFactor(C).LevelNames.Count do
      Result.RowName[i] := s+TwsFactor(C).LevelNames[i-1];
    end;

  { Nomes das variáveis dependentes (para media) e repetições }
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
    for i:=IRow to LRow do                    // Médias desde as linhas IRow até LRow
      begin
      kk := kn+Self.AsInteger[i,Fac[kf]]+1;   // kk é a linha do resultado
      for k := 1 to Col.Len do                // Para todas as variáveis dependentes
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
          Result[kk,kv2] := Result[kk,kv2]+1;   // Número de repetições
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
    Preenchimento de falhas por regressão simples
  Parâmetros
    nCond : número mínimo de observações para predição
    rCond : valor mínimo de r para participação da variável
    Sim   : True para inclusão de erro simlulado na predição; false para predição pelo modelo
    lf    : Lista de vetores que indicam os índices das falhas existentes, por coluna. Índices
            negativos são utilizados para indicar falhas preenchidas
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
  { Obtém médias, SQ&P de desvios entre valores das colunas kk e jj }
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
    tb.MLab:='Equações de regressão utilizadas no preenchimento de '+Result.ColName[k];
    tb.Struct.AddNumeric('Coef_Linear','Coeficiente linear da equação de regressão');        //1
    tb.Struct.AddNumeric('Coef_Angular','Declividade da equação de regressão');              //2
    tb.Struct.AddNumeric('Coef_Det','Coeficiente de determinação entre as variáveis');       //3
    tb.Struct.AddNumeric('Desv_Padrao','Desvio padrão das observações');                     //4
    tb.Struct.AddNumeric('Num_Obs','Número de observações válidas');                         //5
    tb.Struct.AddQualitative('Condicao','Condição da equação para participar da estimação'); //6
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

        // Se tb possui pelo menos uma linha, poderá haver preenchimento
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
    Padroniza colunas numéricas
  Parâmetros
    StdType: tipo de padronização desejada
      0: Padronização para média 0 e variância 1
      1: Padronização para variância 1
      2: Padronização para média 1
    NewData: Se True, cria um novo conjunto de dados; cc transforma o próprio conjunto
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
    Dadas as definições dos atributos das colunas (tamanho, em particular) para saída de
    um conjunto de dados, determina quantas colunas poderão ser impressas.
  Parâmetros
    k: Retorna o número de colunas
  Métodos chamados
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
                              'Método: %s'#13 +
                              'Erro: Índice [%d] inválido',
                              [ClassName, 'ColsToImp', K]);
  End;
end; {TwsDataSet.ColsToImp}

procedure TwsDataSet.Print(Buffer: TStrings);
{ Objetivo
    Imprime o conjunto de dados na janela de texto, de acordo com os atributos das colunas
  Métodos chamados
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
  Parâmetros
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
  Parâmetros
    reader: objeto que faz a leitura
  Métodos chamados
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
  Parâmetros
    Reader: Escritor da lista
  Métodos chamados
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
  Parâmetros
    Writer: objeto para escrita em disco
  Métodos chamados
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
  Parâmetros
    Col: Índices das colunas para eliminação
  Métodos chamados
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
    Variável especificada é do tipo fator?
  Parâmetros
    S: nome da variável
  Métodos chamados
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
    Atribui nome à coluna especificada
  Parâmetros
    i: Índice da coluna
    S: Nome a ser atribuído
  Métodos chamados
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
    Obtém o valor de uma variável do conjunto de dados em formato string
  Parâmetros
    i: índice de linha
    j: índice de coluna
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
  Observação
    Se a variável for numérica, retorna ´.´ se for valor perdido. Aplica a função Fuzz para eliminar
    dígitos que estão fora da precisão especificada; se for do tipo fator, retorna o nome do nível
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
    Obtém o valor de uma variável do conjunto de dados como inteiro
  Parâmetros
    i: índice de linha
    j: índice de coluna
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
  Observação
    Se a variável for do tipo fator, o inteiro que retorna é o índice
}
Begin
  Result := Trunc(Get(i,j));
End; { TwsDataSet.GetInt }

Function TwsDataSet.GetDT(i, j: Integer): TDateTime;
{ Objetivo
    Obtém o valor de uma variável do conjunto de dados como Data/Tempo
  Parâmetros
    i: índice de linha
    j: índice de coluna
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
  Observação
    Se a variável for do tipo fator, o valor retornado não tem sentido
}
Begin
  Result := TDateTime(Get(i,j));
End; { TwsDataSet.GetInt }

function TwsDataSet.GetColName(i: Integer): string;
{ Objetivo
    Recupera o niome da coluna especificada
  Parâmetros
    i: Índice da coluna
  Métodos chamados
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
  Parâmetros
    i: índice de lina
    j: índice de coluna
  Métodos chamados
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
  Parâmetros
    N: nome do conjunto de dados
    Lab: rótulo do conjunto
    ColType: tipo de coluna a inserir
  Métodos chamados
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
// Se a estrutura foi criada mas não existem dados, Fill cria os dados e inicializa-os.
// Deverá, logicamente, ser chamado sempre após a definição da estrutura dos dados.
procedure TwsDataSet.Fill(const x: Double);

  procedure FillVector(v: TObject);
  begin
    TwsVec(v).Len := FnCols; // não faz nada se igual
    TwsVec(v).Fill(x);
  end;

var i, j: Integer;
    v: TwsDFVec;
begin
  if FnRows = FList.Count then
     for i := 0 to FnRows-1 do
       begin
       TwsVec(FList[i]).Len := FnCols; // não faz nada se igual
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
    Retorna endereço do próprio conjunto
}
Begin
  Result := Self;
End;

function  TwsDataSet.IndexColsFromString(const Cols: String): TwsLIVec;
{ Objetivo
    Obtém array de índices de colunas especificadas como string
  Parâmetros
    Cols: colunas especificadas como string
  Métodos chamados
    IndexCols
  Campos alterados
    Nenhum
}
Begin
  Result := Struct.IndexCols(Cols);
End;

Function  TwsDataSet.IndexColsFromStrings(Cols: TStrings): TwsLIVec;
{ Objetivo
    Obtém array de índices de colunas especificadas como lista
  Parâmetros
    Cols: Lista de strings
  Métodos chamados
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
  Parâmetros
    R: índice de linha
    C: índice de coluna
  Métodos chamados
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
  Parâmetros
    R: índice de linha
    C: índice de coluna
    Value: valor a atribuir
  Métodos chamados
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
  Parâmetros
    i: índice de linhas
    j: índice de colunas
  Métodos chamados
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

// Permite ao chamador verificar o tipo de uma célula e ao mesmo
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
    Alisar uma seqüência de valores por meio de alisadores não lineares. Implementa
    dois alisadores não lineares: 4253H-duplo e 3RSSH-duplo
  Parâmetros
    xCol: índice da coluna dos valores da variável X, supostamente equidistantes. As linhas do
    conjunto de dados são ordenadas em função dos valores dessa variável.
    yCol: Índices das colunas da variável Y que serão alisadas. Para cada coluna que será alisada
    será criada uma coluna para armazenar as rugosidades
    S4253HD: alisador que será utilizado. Se True (default) o alisador a ser utilizado será 4253H-duplo
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
   { Alisa através de corridas de medianas de dimensão 2. Utilizada para recentrar resultados
       de corridas de medianas de dimensão 4. ESave restaura o valor original da última posição.
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
   { Obtém a mediana de Y1, Y2 e Y3 e coloca em xMed. Ch retorna True se Y2 não é a mediana
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
   { Estima valores alisados para os dois pontos extremos da seqüência utilizando a regra da
     extrapolação do ponto final. Todos os valores da seqüência, exceto os extremos, já foram
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
   { Encontra platos de 2-números na seqüência e aplica algoritmo de quebra
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
   { Alisa seqüências por meio de corridas de medianas de dimensão Len. Ao invés de utilizar
     este procedimento para Len=3, utilize S3
     W: array de trabalho no qual os dados estarão ordenados
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
     A dimensão par para amedianas aumenta a sequencia de saída em 1 posição no final, pois
     ela não pode ser simétrica. ESave guarda último valor, uma vez que não existe outra posição
     para ele. Y[1] não é alterado.
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
   { Alisamento por medianas de dimensão 5 }
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
   { Calcula corridas de medianas de dimensão 3. Ch retorna True se qualquer mudança foi
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
   { Obtém corridas repetidas de medianas de dimensão 3 }
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
      'Rugosidades para a variável '+Struct.Col[yCol[i]].Name,8,5))
    end;
  // copia valores para alisamento. X não é alterada
  for i:=1 to NRows do
    begin
    v:=TwsDFVec.Create(2*yCol.Len+1);
    v[1]:=Data[i,xCol];
    for j:=1 to yCol.Len do
      v[j+1]:=Data[i,yCol[j]];
    Result.MAdd(v)
    end;
  // Para cada variável Y
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
    Para o conjunto de valores de cada variável encontra os quantis conhecidos como
    valores letra
  Parâmetros
    xCol: índices das variáveis
  Observações
    Com exceção da mediana, os quantis são obtidos aos pares, inferior e superior.
    Para cada conjunto de dados são obtidos, no mínimo, os valores para mediana, quartos
    e extremos. Os demais dependem da quantidade de valores do conjunto, limitados a 64
    divisões no conjunto. As letras utilizadas para representação dos quantis seguem a
    denominação utilizada na literatura inglesa. Também são obtidas outras quantidades
    relacionadas: amplitudes e amplitudes médias (com exceção da mediana) e dispersão
    relativa a distribuição gaussiana (com exceção da mediana e dos extremos)
      M - Median (mediana)
      H - Hinges (quartos)
      E - Eights (oitavos)
      D -        (décimos-sextos)
      C -        (trigésimos-segundos)
      B -        (sexagésimos-quartos)
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
  Result.MLab:='Valores Letra Para Variáveis do Conjunto '+Name;
  Result.ColIdentName:='Valores_Letra';
  Col:=TwsQualitative.Create('Variavel','Variáveis Especificadas para Valores Letra');
  for J:=1 to xCol.Len do
    TwsQualitative(Col).AddLevel(Struct.Col[xCol[J]].Name);
  Result.Struct.AddColEx(Col);                        // Coluna 1. Nomes de variáveis
  Result.Struct.AddColEx(TwsNumeric.Create('Profundidade','Profundidade do Valor Letra',8,5)); //2
  Result.Struct.AddColEx(TwsNumeric.Create('Inferior','Valor Próximo ao Extremo Inferior',8,5)); //3
  Result.Struct.AddColEx(TwsNumeric.Create('Superior','Valor Próximo ao Extremo Superior',8,5)); //4
  Result.Struct.AddColEx(TwsNumeric.Create('Media','Média dos Limites Inferior e Superior',8,5)); //5
  Result.Struct.AddColEx(TwsNumeric.Create('Amplitude','Limite Superior - Limite Inferior',8,5)); //6
  Result.Struct.AddColEx(TwsNumeric.Create('Disp_Gauss','Dispersão Gaussiana',8,5)); //7

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
    Obter o dispositivo de ramo e folhas para a variável numérica especificada
  Parâmetros
    xCol: Vetor com os índices das colunas correspondentes às variáveis de interesse
    Xtrems (default=False): True para fazer a escala em relação aos extremos. Se False
      a escala é feita em relação aos valores adjacentes, excluindo os valores discrepantes,
      que são listados em ramos separados
  Observações
    Construção baseada na versão FORTRAN, disponível em
      Velleman & Hoaglin - Applications, Basics and Computing of EDA
        pág. 16 e posteriorea

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
   // Mesmo que função Int mas considera EPS para arredondamento e retorna inteiro
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
      lh:=1;                     // Posições dos valores adjacentes
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
    { Encontra comprimento de posição ótimo.
      Na entrada
        HI, LO - limite inferior e superior dos dados
        MaxP - Número máximo de posições permitida para plotagem
        MZero: True se se uma posição rotulada como -0 é permitida
      Na saída
        PTot - número desejado de posições
        U - é um inteiro potência de 10 tal que NP=Frt*U
        NP - é o comprimento ideal para a posição de plotagem
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
          // Calcula o número de posições necessárias
          PTot:=IntFN(HI/NP)-IntFN(LO/NP)+1;
          // Se -0 é possível e ( se HI e L0 têm sinais opostos ou se H1=0)
          // iremos necessitar da linha -0
          if (MZero and ((HI = 0) or (HI*LO<0))) then Inc(PTot);
          // PTot posicoes sao necessarias para plotagem. É suficiente?
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
      ifrom: a partir do índice
      ito: até o índice
      HI: True se valores são superiores ao valor adjacente superior, false se são
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
    { Obtém e imprime as profundidades
       P1, P2: Ponteiros para WI e W. Na entrada P1=P2, apontam para o primeiro valor
               ainda não impresso. Na saída, PT1 aponta para o primeiro valor da proxima
               linha. P2 não é alterado
       C: Maior valor da linha atual (positiva) ou o menor valor acima da linha atual
          (negativa)
       IAdh: aponta para o maior valor adjacente em W e WI
       H: Maior valor que está sendo mostrado
       R: Na entrada, posto  em relação ao limite inferior. Na saída é atualizado para
          incluir a contagem para a linha atual
       MYet: Flag, setado para True quando a mediana já foi processada.
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
      // Onde está a mediana?
      if MYet then
        Depth:=N-(R-LeFont)    // caso 1: mediana já passou
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
        Depth:=Rank       // caso 4: mediana ainda não chegou
        end;
      Result:=IntToStr(Depth)
    end;

    function StemP(St,LW: Integer; NNow: boolean): string;
    { Calcula e imprime o ramo
      Na entrada:
        St: limite interno (mais proximo de zero) da linha atual
        LinW: número de possíveis d´gitos folhas diferentes
        NNow: True, se a linha atual é negativa
    }
    const
      Ch5: array[1..5] of Char=('*','T','F','S','.');
    var
      NStem,LefDig: Integer;
    begin
      NStem:=St div 10;
      LefDig:=Abs(St-NStem*10);
      // Quantos dígitos por linha?
      Result:='';
      if LW=2 then
        begin
        if NStem=0 then             // caso 1: dois dígitos por linha, 5 linhas por ramo
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
          if NStem=0 then           // caso 2: 5 dígitos possíveis/ linha; 2 linhas/ramo
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
        else                        // caso 3: 10 dígitos / linha, 1 linha / ramo
          if (NStem<>0) or (not NNow) then
            Result:=Result+IntToStr(NStem)
          else
            Result:=Result+'-0';
      Result:=Result+' '
    end;

begin
  Result := TStringList.Create;
  // Constrói um dispositivo para cada coluna
  for j:= 1 to xCol.Len do
    begin
    W:=CopyCol(xCol[j]);
    N:=W.Len;
    W.QuickSort(True);

    // Obtém valores resumo
    YInfo(Med,LHing,HHing,Step,AdjL,AdjH,IAdjL,IAdjH);

    { Se os valores adjacentes sao iguais, ou se o usuario deseja,
      torna os valores adjacentes iguais aos extremos}
    if FEquals(AdjH,AdjL) or Xtrems then
      begin  // obtem escala para os extremos. Pode construir um display ruim
      IAdjH:=N; IAdjL:=1;
      AdjL:=W[1]; AdjH:=W[N];
      end;
    // Encontra número ótimo de linhas para exibição

    NLMax:=IntFN(10*Log10(IAdjH-IAdjL+1));

    // Mesmo que AdjL=AdjH, produz um display
    if FEquals(AdjH, AdjL) then
      begin
      AdjH:=AdjL+1;
      NLMax:=1
      end;

    NPosW(AdjH,AdjL,NLMax,NLins,True,Fract,Unt,NPW);
    { Re-escala tudo de acordo com a unidade. Daqui em diante tudo é número inteiro
      e os dados estão na forma SS...SL(.). Observe que IntFN ajusta para EPS para
      o arredondamento correto. Pense nos valores como Inteiro*10^Unt
      convertidos para inteiro na forma S...SL
    }
    // Para contar valores <0, =0 e >0
    WI:=TwsLIVec.Create(N);
    for I:=1 to N do
      WI[i]:=IntFN(W[I]/Unt);
    if not FEquals(Fract,10) then
      begin
      // Se todas as folhas são iguais a 0, formato será uma linha por ramo
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
    // Comprimento da linha agora é fract
    LinWid:=IntFN(fract);
    // Inicia o processo de impressão do dispositivo
    SLTitle(Unt,Struct.Col[xCol[j]].Name,Struct.Col[xCol[j]].Lab);

    // Imprime valores discrepantes (abaixo de AdjL), se existirem
    Rank:=IAdjL-1;
    if IAdjL <> 1 then
      Result.Add(OutlYP(1,Rank,False));

    { Inicializações para a parte principal do display
      Cut = corte da linha.
        Primeiro número da próxima linha dos ramos positivos
        Último número da linha atual dos ramos negativos
      L4: extremo interno (próximo de zero) da linha atual
      NegNow: flag para os ramos: true são negativos, False caso contrário
      MedYet: flag para mediana: False  até a mediana, True depois
      K1, K2, K3 são ponteiros em Y() para profundidades, impressão, zeros
      I1: conta os espaços utilizados na linha
      P5: conta as folhas naquela linha para a profundidade
      L9: valor coberto por uma linha
      Rank: conta a ordem, L2 manutenção do dígito da folha
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
    { Dois ponteiros serão utilizados. PT1conta os primeiros para as profundidades,
      PT2 segue para imprimir as folhas. Ambos são inicializados no mesmo ponto }
    PT1:=IAdjL;
    PT2:=PT1;
    { Loop principal para esgotar todas as linhas do display
      Cut - Primeiro número na próxima linha de ramos positivos ou último número
            da linha atual de ramos negativos
      Stem - limite interno (próximo de zero) da linha atual
      SpacCnt - Conta os espaços utilizados na linha
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
        if Spacnt>=PltWid then  // Terminou espaço?
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
    end; // for j - para cada variável
end; // StemAndLeaf

function TwsDataSet.IndexMat(CIdx: TwsLIVec): TwsGeneral;
{ Objetivo
    Obtem uma matriz com os possiveis índices das combinações das variáveis indicadas.
    Delineada primariamente para impressão de saídas de conjuntos de dados onde são especificadas
    as variáveis (do tipo fator) que estarao em colunas
  Parâmetros
    CIdx: índices das variáveis que estarão nas colunas. Os índices que retornarão na matriz
          dependerão destes índices de colunas
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
  Parâmetros
    aName: nome da coluna (variável)
    aLab: rótulo da coluna
    fs: largura de impressão
  Métodos chamados
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
    Libera espaço ocupado opr conjunto de dados
  Métodos chamados
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
  Parâmetros
    Value: nome do conjunto
  Métodos chamados
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
    Faz leitura do rótulo da matriz
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
}
 Begin
   rlabel := flabel^;
 End; { TwsDataSetCol.RLabel }

Procedure TwsDataSetCol.WLabel(Const alabel: String);
{ Objetivo
    Atribui rótulo ao conjunto de dados
  Métodos chamados
    Nenhum
  Campos alterados
    FLabel
}
Begin
  flabel := newstr(alabel);
End; { TwsDataSetCol.WLabel }

Procedure TwsDataSetCol.LoadFromStream(Reader:TReader);
{ Objetivo
    Recupera parte da descrição de coluna do disco
  Parâmetros
    Reader: objeto para leitura
  Métodos chamados
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
  Parâmetros
    Writer: objeto de escrita
  Métodos chamados
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
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Begin
  Case ColType Of
    dtNumeric  : Result := 'Numérica';
    dtQuant    : Result := 'Fator Quantitativo';
    dtQualit   : Result := 'Fator Qualitativo';
    dtQualitOrd: Result := 'Fator Ordenado';
    End; { Case }
End; { TwsDataSetCol.getColTypeAsString }


{ ============================== TwsNumeric ============================== }

Constructor TwsNumeric.Create(Const aName,aLab: String; fs: byte=15; fp: byte=8);
{ Objetivo
    Cria objeto descritor para coluna numérica
  Parâmetros
    aName: nome da coluna
    aLab: rótulo para coluna
    fs: largura de impressão
    fp: precisão de impressão (dígitos significativos)
  Métodos chamados
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
    Atribui precisão a coluna numérica
  Parâmetros
    APrecision: precisão desejada. Se for maior que 15, a precisão será 15.
  Métodos chamados
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
  Parâmetros
    Reader: objeto de leitura
  Métodos chamados
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
  Parâmetros
    Writer: objeto de escrita
  Métodos chamados
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
// Por enquanto só salva colunas numéricas
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
    Participa da criação de objeto do tipo fator. Não pode ser chamado diretamente.
  Parâmetros
    AName: nome da coluna
    ALab: rótulo da coluna
    fs: largura de impressão
  Métodos chamados
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
    Libera parte do espaço ocupado por objeto descritor
  Métodos chamados
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

     // Obtém todos os níveis que aparecem nos dados
     for i := 1 to FDataSet.nRows do
       sl.Add(FDataSet.AsTrimString[i, k]);

     // Remove os níveis que não aparecem
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
    Retorna o número de níveis
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Begin
  Result := FLevels.Count;
End;

Function TwsFactor.KillLevels: Boolean;
{ Objetivo
    Libera espaço ocupado pelça lista com nomes dos níveis. Retorna true se eliminação foi
    feita sem problemas.
  Métodos chamados
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
  Parâmetros
    Reader: objeto de leitura
  Métodos chamados
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
  Parâmetros
    Writer: objeto de escrita
  Métodos chamados
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
     Retorna índice do nome do nível especificado. Baseado em 0.
  Parâmetros
     N: nome do nível
  Métodos chamados
     IndexOf
  Campos alterados
}
Begin
  Result := LevelNames.IndexOf(SysUtilsEx.AllTrim(N));
End; { TwsFactor.LevelToIndex }

{Se o level já existe, não adiciona e retorna seu índice na lista}
Function TwsFactor.AddLevel(Level: String): Integer;
{ Objetivo
    Se o nome do nível ainda não existe, insere-o na lista. Retorna o índice do nome na lista.
  Parâmetros
    Level: nome do nível
  Métodos chamados
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
    Retorna tipo de nível como string
  Métodos chamados
    Nenhum
  Campos alterados
    Nenhum
}
Begin
  Case FLevelType Of
    tfRandom : Result := 'Aleatório';
    tfFixed  : Result := 'Fixo';
    End; { Case }
End; { TwsFactor.getLevelTypeAsString }

Function TwsFactor.getContrTypeAsString:String;
{ Objetivo
    Retorna tipo de contraste como string
  Métodos chamados
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
    Cria objeto descritor de uma variável do tipo fator qualitativo
  Parâmetros
    AName: nome da coluna
    ALab: rótulo da coluna
    fs: largura de impressão
  Métodos chamados
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
  Parâmetros
    Reader: objeto de leitura
  Métodos chamados
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
  Parâmetros
    Writer: objeto de escrita
  Métodos chamados
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
    Cria objeto descritor de uma variável do tipo fator qualitativo ordenado
  Parâmetros
    AName: nome da coluna
    ALab: rótulo da coluna
    fs: largura de impressão
  Métodos chamados
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
  Parâmetros
    Reader: objeto de leitura
  Métodos chamados
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
  Parâmetros
    Writer: objeto de escrita
  Métodos chamados
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
    Cria objeto descritor de uma variável do tipo fator quantitativo
  Parâmetros
    AName: nome da coluna
    ALab: rótulo da coluna
    fs: largura de impressão
  Métodos chamados
    Create herdado
  Campos alterados
    ColType
    ContrType
    FLevel
}
Begin
  Inherited Create(aname,alab,fs);
  ColType   := dtQuant;
  FLevel    := TwsDFVec.Create(0); {Valores dos níveis}
  ContrType := ctPolOrt;
End; { TwsQuantitative.Create }

Destructor TwsQuantitative.Destroy;
{ Objetivo
    Libera espaço ocupado pelo objeto
  Métodos chamados
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
    Libera espaço ocupado pelça lista com nomes dos níveis. Retorna true se eliminação foi
    feita sem problemas.
  Métodos chamados
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
  Parâmetros
    Reader: objeto de leitura
  Métodos chamados
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
  Parâmetros
    Writer: objeto de escrita
  Métodos chamados
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
    Insere nível na lista de níveis
  Parâmetros
     Level: nome do nível
  Métodos chamados
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
    Atribui valor para nível especificado
  Parâmetros
    Level: nome do nível
    Value: valor para o nível
  Métodos chamados
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
     Raise Exception.CreateFmt('Nível <%s> desconhecido', [Level]);
End;

Function TwsQuantitative.GetLevelValue(Const Level: String): Double;
{ Objetivo
    Recupera valor do nível quantitativo
  Parâmetros
    Level: nome do nível
  Métodos chamados
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
     Raise Exception.CreateFmt('Nível <%s> desconhecido', [Level]);
End;

{ ============================= TwsOrderedDataSet ========================== }

Constructor TwsOrderedDataSet.Create(aName: String; aMaxRows, aBasedCol: Integer; Ascend: Boolean);
{ Objetivo
    Cria classe para inserção ordenada de linhas
  Parâmetros
    aMaxRows: Número máximo de linhas a inserir
    aBasedCols: Coluna na qual a ordenação será baseada
  Métodos chamados
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
    Insere uma linha num conjunto de forma ordenada e respeitando número máximo de linhas.
    Sobrepõe MAdd herdado
  Parâmetros
    L: Linha a ser inserida
  Métodos chamados
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
    Retorna uma matriz a partir de um arquivo texto. Pode ser chamada na seqüência para ler
    um conjunto qualquer de matrizes gravadas no mesmo arquivo. Cada seção é definida por
    uma palavra chave. Todas as seções deverão ser encerradas pelo caracter delimitador de
    seção. As seções são:
      COM=         para definir comentários              opcional
      TIPO=        para definir o tipo da matriz         obrigatório
                   Os tipos predefinidos são (caracteres mínimos): GE, SI, DI, TR, VA, TO
      NOMELinhas=  para definir os nomes de linhas       opcional
      NOMEColunas= para definir nomes de colunas         opcional
      A matriz em si se constitui numa seção em separado e deverá se seguir a essas
      definições com o nome seguido por um sinal de igual e as linhas separadas pelo
      caracter separador de linhas.

  TFile   : Arquivo texto de onde serão lidas as matrizes
  ChLin   : Caracter que indica o final de uma linha da matriz. O caracter vírgula não
            poderá ser utilizado como separador de linha uma vez que é um separador das
            partes inteira e decimal dos elementos.
  ChEndSec: Caracter separador de seções

  Exemplos:

  COMENT=Matriz geral/
  Tipo=Geral/
  NomeCol= c1 c2 c3/
  NomeLin=l1 l2 l3/
A=1 2 3;
  3 4 5;
  5 6 7/

  COMENT=Matriz simétrica/
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

  Observações
    Os elementos que compõem cada seção deverão ser separados pelos caracteres
     [#9,#10,#13,' ','\', '"','(',')']
  Retorno
    Retorna a matriz do tipo especificado. Se não houver nenhuma matriz para ler ou se
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
{  Esta função analisa um vetor L e gera uma matriz diagonal cujos componentes da
  diagonal principal são os elementos do vetor L e os demais elementos são nulos.
     Parâmetros:
       L : um vetor do tipo TwsVec no qual seus elementos serão a diagonal principal
        de uma matriz Diagonal.  }

function VecToDiag(L: TwsVec): TwsDiagonal;
{ Objetivo
    Constrói uma matriz diagonal a opartir de um vetor
  Parâmetros
    L: vetor cujos elementos comporão a diagonal da matriz
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
    Obtém os autovalores e autovetores de Inv(B)*A. Utiliza o fator de Cholesky
  Parâmetros
    H, R: Matrizes para obtenção dos autovalores e autovetores
    EVec: Retorna a matriz dos autovetores
    EVal: Retorna uma matriz diagonal com os autovalores
    r: Para os autovalores ordenados, retorna o índice do último autovalor não
       nulo.
    eps: Precisão para obtenção de r
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
        X.Copy(mtGeneral,TwsMatrix(A));          // Transforma a matriz simétrica numa geral
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
      autovalores, suas diferenças em relação aos subsequentes e as proporções que
      representam em relação à soma.
  Parâmetros
    V: matriz diagonal dos autovalores
    Rank: número de autovalores não nulos de V
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
     apenas o dimensionamento de um vetor é necessário para composição do produto. Cada linha
     do produto que vai sendo obtida vai substituindo a linha correspondente da primeira martiz.
   Parâmetros
     A: Matriz que pré-multiplica. Na saída contém o resultado do produto
     B: Matriz que pós-multiplica. Não é alterada pelo produto
     ErrCode: Código de erro. Retorna 0 se o produto é realizado sem problemas
   Observações
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
     Produto de uma matriz por ela mesma um número especificado de vezes.
   Parâmetros
     A: Matriz para o produto
     n: Potência da matriz ou número de vezes que A será multiplicada por ela mesma
     ErrCode: Código de erro. retorna 0 se o produto transcorreu normalmente. O produto de A
     por ela mesma só é possível se A for quadrada.
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
  Parâmetros
     LN   : String com os nomes de linhas. Se não houver, deve ser passado com valor nil
     CN   : String com os nomes das colunas. Se não houver, deve ser passado com valor nil
     P    : String com os valores no formato Nome=elementos
     MT   : Tipo da matriz a ser criada
     ChLin: Caracter que indica os finais de linhas da matriz
     ChEnd: Caracter que indica o final da matriz
  Exemplo 'G=1 2 3; 4 5 6; 6 7 8/'  - matriz geral
          'S=1; 2 3; 4 5 6/'        - matriz simétrica
          'T=1; 2 3; 4 5 6/'        - matriz triangular
          'D= 1 2 3/'               - matriz mtDiagonal
          'V= 1 2 3/'               - matriz de mtVandermonde
          'O=1 2 3 4 5/'            - matriz de mtToeplitz
  Observação
    Procurar não utilizar o caracter ',' (virgula) como separador, uma vez que ele pode
    ser um caracter válido na especificação de um número.
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
  Parâmetros
    P    : string que contem a matriz geral
    ChDel: caracter que separa os elementos
    ChLin: caracter que separa as linhas
  Resultado
    Retorna uma matriz geral. Retorna nil se algum vetor linha tiver dimensão diferente
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
  Parâmetros
    Y: matriz cujas colunas definem contrastes
    Col: Retorna a última coluna testada. Se o retorno for False, indica a coluna que
      não é contraste
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
  Parâmetros
    Y  : matriz que terá as colunas testadas
    i,j: Se o retorno for False, retorna o par que não é ortogonal
  Observação
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
    Os contrastes deverão estar especificados por coluna. A coluna é considerada como
    um conjunto de coeoficientes de contrastes se a soma for nula.
  Parâmetros
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
    ortogonais. Os contrastes deverão estar especificados por coluna. Duas colunas são
    consideradas como um conjunto de coeoficientes de contrastes ortogonais se a sua soma
    de produtos for nula.
  Parâmetros
    Y: matriz com os coeficientes
    w: vetor com pesos para verificação da ortogonalidade. Tipicamente w contém o número de
    repetições de cada média envolvida no contaste
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
    Aplica uma função a uma matriz
  Parâmetros
    fOp: Função desejada. As possibilidades são:
      cABS      - valor absoluto
      cEXP      - exponencial
      cAPROXIMA - Aproxima valores dentro de um limite pré-estabelecido
      cINT      - Parte inteira do valor
      cLN       - Logaritmo neperiano
      cRAIZ     - Raiz quadrada
      cARCTAN   - Arco tangente
      cARCSEN   - Arco seno
      cARCCOS   - Arco cosseno
      cSEN      - Seno
      cCOS      - Cosseno
      cSENH     - Seno hiperbólico
      cCOSH     - Cosseno hiperbólico
      cTAN      - Tangente
      cLOG      - Logaritmo decimal
      cANG      - Transformação angular
      cLGAMA    - Logaritmo da função gama
      cTGAMA    - Derivada da função digama
      cFLOOR    - Maior inteiro
      cCEIL     - Menor inteiro
      cINV      - Inverso do valor
      cFRAC     - Parte fracionária
      cTANH     - Tangente hiperbólica
      cAcum     - Valores acumulados

    C: Matriz onde retornam os valores da função. Em C deverão estar os valores originais
  Valores perdidos
    Funções aplicadas a valores impróprios ou perdidos retornam valores perdidos
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
    cAPROXIMA: // Aproxima valores dentro de um limite pré-estabelecido
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
    cSENH: // Seno hiperbólico
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarSinH(C[i,j]);
    cCOSH: // Cosseno hiperbólico
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
    cANG: // Transformação angular
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarAng(C[i,j]);
    cLGAMA: // Logaritmo da função gama
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarLnGamma(C[i,j]);
    cTGAMA: // Logaritmo da função gama
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
    cFRAC: // Parte fracionária
      for i := 1 to C.NRows do
        for j:=1 to C.NCols do
          C[i,j] := ScalarFrac(C[i,j]);
    cTANH: // Tangente hiperbólica
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
    Efetua o produto matricial onde ambas as matrizes são gerais, simetricas, mtVandermonde
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
    mtVandermonde ou mtToeplitz e a outra é mtDiagonal.
  Parâmetros
    A: Primeira matriz do produto. Podera ser geral, simetrica, mtVandermonde ou mtToeplitz
    B: Segunda matriz do produto. Sempre devera ser uma matriz mtDiagonal
    PreMult: True se a matriz mtDiagonal premultiplica a outra matriz; false se posmultiplica
    C: Matriz resultante. Sempre sera do tipo Geral
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
  Retorno
    Retorno é sempre uma matriz geral
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
    Efetua o produto matricial onde uma matriz é geral, simetrica, de mtVandermonde ou
    mtToeplitz e a outra é mtTriangular
  Parametros
    A: Primeira matriz do produto, do tipo geral, simétrica, de mtVandermonde ou mtToeplitz
    B: Segunda matriz do produto, do tipo mtTriangular
    PreMult: True se a matriz mtTriangular (B) premultiplica; false se posmultiplica
    C: Matriz resultante. Sempre sera do tipo Geral
  Observacao
    Esta rotina NAO testa se o produto e possivel. Isto devera ser feito pela rotina que a
    utilizar
  Retorno
    O retorno é sempre uma matriz geral
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
    Efetua o produto matricial onde ambas matrizes são diagonais
  Parâmetros
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
    Efetua o produto matricial onde ambas as matrizes são triangulares
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
   Parâmetros
     A, B: Matrizes que serão operadas
     Op: Tipo de operação a ser executada. Os operadores válidos são:
         opSum,opSub,opDiv,opProd,opPower:
           Operadores aritmeticos soma, subtração, divisão, produto e potência
          opGE,opGT,opLE,opLT,opEQ,opNE:
            Operadores para comparacao maior ou igual, maior que, menor ou igual, menor que,
            igual e diferente
          opOR,opAnd:
            Operadores lógicos OU e E
          opMax,opMin
            Máximo, minimo
     C: Matriz que irá armazenar o resultado. Deverá ser criada pela rotina que chama. Na
        chamada ElemOp1(A,B,Op,C) o resultado será c[i,j]=a[i,j] Op b[i,j], para todos os
        índices i e j.
   Observação
     Esta rotina NÃO testa se as matrizes A, B e C têm a mesma ordem.
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
   Parâmetros
     A: Matriz cujos elementos da mtDiagonal serão operados
     v: Vetor cujos elementos serão operados com os da mtDiagonal de A
     Op: Tipo de operação a ser executada. Os operadores válidos são:
         opSum,opSub,opDiv,opProd,opPower:
           Operadores aritmeticos soma, subtração, divisão, produto e potência
          opGE,opGT,opLE,opLT,opEQ,opNE:
            Operadores para comparacao maior ou igual, maior que, menor ou igual, menor que,
            igual e diferente
          opOR,opAnd:
            Operadores lógicos OU e E
          opMax,opMin
            Máximo, minimo
     DFirst: Se True então a operação será a[i,i] Op v[i]; Se False, a operação será
       v[i] Op a[i,i].
     C: Matriz que irá armazenar o resultado. Deverá ser criada pela rotina que chama
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
     Opera todos os elementos das matrizes simétricas especificadas
   Parâmetros
     A, B: Matrizes simétricas que serão operadas
     Op: Tipo de operação a ser executada. Os operadores válidos são:
         opSum,opSub,opDiv,opProd,opPower:
           Operadores aritmeticos soma, subtração, divisão, produto e potência
          opGE,opGT,opLE,opLT,opEQ,opNE:
            Operadores para comparacao maior ou igual, maior que, menor ou igual, menor que,
            igual e diferente
          opOR,opAnd:
            Operadores lógicos OU e E
          opMax,opMin
            Máximo, minimo
     C: Matriz simétrica que irá armazenar o resultado. Deverá ser criada pela rotina que chama. Na
        chamada ElemOp1(A,B,Op,C) o resultado será c[i,j]=a[i,j] Op b[i,j], para os
        índices i e j, i>=j.
   Observação
     Esta rotina NÃO testa se as matrizes A, B e C têm a mesma ordem.
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
   Obtém a inversa da matriz especificada.
 Parâmetros:
   Y: Matriz a ser invertida. Como o método Inv é virtual, a chamada Y.Inv() irá executar
      os métodos específicos de inversão.
   NewMat: False provocará a inversão no local (ou na mesma matriz) e para True a provoca
           a inversão numa cópia de Y.
   ErrCode: Códigos que poderão retornar do processo de inversão. Veja método Inv de
            TwsMatrix e descendentes.
 Observação:
   Se a inversa da matriz Y existir, será tal que Y.Inv(Y) = I ,onde Inv(Y) é a matriz
  inversa de Y e I é a matriz identidade.
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
    Resolve um sistema de equações lineares através do método de Gauss-Jordan com
    pivoteamento total.
  Parâmetros:
    A: Na entrada é a matriz de coeficientes do sistema linear. Na saída, armazena a inversa
       de A.
    B: Matriz do segundo membro. Cada coluna de B especifica um novo sistema de equações
       lineares com a mesma matriz de coeficientes A. Na saída, armazena as correspondentes
       soluções dos sistemas.
    ErrCode: Código de erro. Retorna 0 se o processo se encerra normalmente; caso contrário
       retorna NSingMat (Matriz A é singular)
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
     Dado o sistema de equações lineares, retorna a solução de mínimos quadrados através
     da decomposição por valores singulares
   Parâmetros
     A: Na entrada é a matriz dos coeficientes do sistema linear. Na saída, retorna a
        matriz decorrente da decomposição A=UDV'.
     B: Na entrada possui em cada coluna o vetor do segundo membro de cada sistema linear
        Ax=b(i). Na saída, cada coluna contém a solução de cada sistema
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
    Determina a solução de mínimos quadrados de um SEL através de transformações de
    Householder. Rotina adaptada de Lawson & Hanson.
  Parâmetros:
    A: Matriz dos coeficientes do sistema. Na saída armazena valores que possibilitam
       a obtenção da transformação ortogonal
    B: Matriz com os vetores do segundo membro
    X: Matriz que retorna a solução do sistema
    H: Na saída armazena quantidades que, juntamente com aqueles de A, possibilitam a
       obtenção da transformação ortogonal
    X: Retorna a solução do sistema
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
    Determina a solução de mínimos quadrados de um SEL através de transformações de
    Householder. A diferença em relação ao procedimento HFTSolve está no pivoteamento par-
    cial realizado sobre as colunas de A de forma a tratar de forma mais adequada os casos
    de matrizes mal condicionadas. Rotina adaptada de Lawson& Hanson
  Parâmetros:
    A: Matriz dos coeficientes do sistema. Na saída armazena valores que possibilitam
       a obtenção da transformação ortogonal.
    B: Matriz com os vetores do segundo membro.
    X: Matriz que retorna a solução do sistema.
    H: Na saída armazena quantidades que, juntamente com aqueles de A, possibilitam a
       obtenção da transformação ortogonal.
    X: Retorna a solução do sistema.
    r: Posto numérico de A.
    Indx: Vetor que armazena as trocas de colunas realizadas em A.
    G, H: Retornam quantidades que, juntamente com o que retorna em A, possibilitam a
          obtenção da transformação ortogonal
    ErrCode: Código de erro.
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
     Dado o sistema de equações lineares, retorna a solução através da decomposição LU
   Parâmetros
     A: Na entrada é a matriz dos coeficientes do sistema linear. Na saída, retorna a
        matriz L (mtTriangular inferior da fatoração LU) na porção mtTriangular inferior e U
        na porção mtTriangular superior.
     B: Na entrada possui em cada coluna o vetor do segundo membro de cada sistema linear
        Ax=b(i). Na saída, cada coluna contém a solução de cada sistema
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
  Parâmetros
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
  Parâmetros
    A: matriz para atribuição dos nomes de colunas
    C: String (PChar) com os nomes
  Observação
    Os nomes de colunas deverão ter como separadores os caracteres branco, vírgula, ponto-e-
    vírgula e aspas
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
  Parâmetros
    p: string que contém a matriz
    MT: tipo da matriz a ser criada
  Observações
    Os elementos da matriz devem ser separados por espaços, as linhas por vírgulas e o final
    do string por ponto-e-vírgula
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
   Parâmetros
     nc: Ordem da matriz
}
var i: Integer;
begin
  Result := TwsGeneral(Jota(nc, nc, mtGeneral, 0));
  for i := 1 to nc do Result[i,i] := 1;
end; { Identity }

function DuncanTable(nTreat: Integer; df: TwsVec): TwsGeneral;
{ Objetivo
    Retorna uma tabela apropriada para o teste de Duncan. As taxas de erro consideradas são
    0.01, 0.05 e 0.1.
  Parâmetros
    ntreat: Número de tratamentos (ou de   médias)
    df    : Vetor com os graus de liberdade do resíduo
  Saída
    Matriz geral onde cada linha corresponde a uma combinação grau de liberdade x taxa de erro
    especificado em df e
    cada coluna a uma amplitude entre as médias, iniciando em 2 e indo até ntreat
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
  Result.MLab:='Pontos Percentuais para o Teste de Amplitude Múltipla de Duncan';
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
  Parâmetros
    Tuk   : Matriz com os valores de tabela de Tukey
    trt   : Vetor com o número de tratamentos (ou de médias)
    df    : Vetor com os graus de liberdade do resíduo
    alpha: Vetor com as taxas de erro
  Saída
    Matriz geral onde cada linha corresponde a uma uma combinação grau de liberdade x taxa
    de erro. O número de tratamentos (ou amostras) consideradas vai de 2 a nTreat. A ordem
    da matriz, portanto, será (df.len*alpha.len) x (nTreat+2)
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
  Parâmetros
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
    como um nível, a matriz de Helmert gera contrastes da seguinte forma: para a coluna j,
    estabelece j como valor para a posição (i+1, j), -1 para todas as linhas i=j-1, .., 1
    e para todas as linhas i=j+1, ..., nlin. Essa forma de codificacao gera colunas ortogonais
    entre si.
  Parâmetros
    nr: Numero de linhas da matriz
  Retorno
    Matriz nr x nr-1 em cujas colunas estão os contrastes de Helmert
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
    Obtem uma matriz de reparametrização para um modelo de classificação simples onde os
    constrastes são definidos como constrastes de Helmert. Cada linha define um novo parâmetro.
  Parâmetros
    nr: Numero de níveis do fator A ou número de tratamentos
  Retorno
    Matriz nr x nr+1 em cujas linhas (>1) estão os contrastes de Helmert
}
var
  i,j: Integer;
  a: Double;
begin
  Result := TwsGeneral.Create(nr, nr+1);
  with Result do
    begin
    Name:='LHelmert';
    MLab:='Matriz de reparametrização (MCS) com constrastes de Helemert';
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
    Obtem uma matriz indicadora para um modelo de classificação simples.
  Parâmetros
    nr: Numero de níveis do fator A ou número de tratamentos
  Retorno
    Matriz nr x nr+1 com a indicação dos efeitos
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
    matriz X do modelo atraves do contraste entre o nível i e o ultimo nível.
  Parâmetros
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
    Obtem de reparametrização para MCS utilizando o último tratamento como controle.
  Parâmetros
    nr: Numero de níveis do fator A ou número de tratamentos
  Retorno
    Matriz nr x nr+1 com a indicação dos efeitos
}
var
  i, j: Integer;
begin
  Result := TwsGeneral.Create(nr, nr+1);
  with Result do
    begin
    Name:='LControle';
    MLab:='Matriz de reparametrização (MCS) - Desvios com último tratamento';
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
  Parâmetros
    nr: Número de linhas da matriz. A ordem sera sempre nr x (nr-1)
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
    Gera uma matriz de reparametrização com contrastes indicando perfis (contrasta a posicao
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
    MLab:='Matriz de reparametrização (MCS) utilizando perfis';
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
    Gera uma matriz de reparametrização através do constraste entre um tratamento e a média
    dos demais.
  Parâmetros
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
    MLab:='Matriz de reparametrização (MCS) utilizando contrastes simples';
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
  Parâmetros
    x: valores da variavel polinomial. Não poderá ter valores repetidos
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
    Gera uma matriz de reparametrização com valores de polinomios ortogonais
  Parâmetros
    x: valores da variavel polinomial. Não poderá ter valores repetidos
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
    MLab:='Matriz de reparametrização utilizando polinômios ortogonais';
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
    Gera uma matriz com os valores de polinômios ortogonais ponderados
  Parâmetros
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
   Parâmetros
     Beta: Retorna a matriz dos coeficientes para o polinômio de grau zero, 1, 2, etc. Os
           coeficientes dos polinomios de cada grau estao localizados nas linhas. O de
           grau 0 na 1a. linha (um valor), o de grau 1 na 2a. linha (dois valores), etc.
      x:   Vetor com os valores da variável polinomial. Deverão estar ordenados em ordem
           crescente
      w:   Vetor com os valores da variável peso (usualmente o número de repetições)
      y:   Vetor com os valores da variável dependente (usualmente as médias)
      sq:  Somas de quadrados associadas a cada polinômio ortogonal
      m:   Grau do polinômio e número de colunas da matriz dos valores dos polinômios
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
    Gera a matriz de Hilbert onde o elemento na posição (i, j) é obtido como 1/(i+j-1). A matriz
    de Hilbert é utilizada como exemplo de matriz mal condicionada.
  Parâmetros
    nc: Dimensão da matriz quadrada
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
    Gera uma matriz do tipo especificado onde todos os valores são iguais
  Parâmetros
    nr: Número de linhas
    nc: Número de colunas
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
    Cria objeto para tratamento da decomposição triangular
  Parâmetros
    A: Matriz simétrica. No problema de mínimos quadrados A é a matriz de somas de
      quadrados e produtos
    RIdx: índice da variável resposta. Se RIdx = 0 não existe variável resposta
  Matrizes criadas
    Fatores L e D - posicoes 0 e 1
    Inversa de L  - posicao 2
    Matriz de covariâncias, se RIdx>0 - Posicao 3
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
    Obtém os fatores da decomposição triangular L e D e insere na lista
  Campos modificados
    Lista de matrizes
    fErr: código de erro
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
    Uma vez obtida a decomposição triangular de A, resolve o sistema Ax=B
  Parâmetro
    Cada coluna de B representa um sistema com a mesma matriz de coeficientes, ou
    seja A(x1, x2, ...) = (b1, b2, ...)
  Retorno
    Na saída B conterá nas colunas a solução de cada sistema (x1, x2, ...)
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
  C.MLab:='Matriz de covariâncias';
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
  Saída
    * Após a chamada de Decomp o conteúdo da matriz fA é o seguinte:
      a11 a12       u11 u12
      a21 a22 --->  f12 u22
      a31 a32       f13 f23
      a41 a42       f14 f24
    g=[f11, f22] e h=[(f1)'*f1, (f2)'*f2]
    sendo que com g e h é possível construir a matriz de reflexão de Householder e obter Q.
    * Se a matriz A nxp for completada à direita com uma identidade, no lugar da identidade
    retorna a matriz Q.
    * No sistema inconsistente y=Xb + e onde X é nxp entao se A=[X, y] então, no final
    Q v   é a solucao de mínimos quadrados será a solução de Qb=v e t't é a soma de
    0 t   quadrados de quadrados do resíduo
  Campos modificados
    fA: contem a matriz operada
    g, que contem os termos da diagonal de H, e h, que contem as somas de quadrados das
    colunas de H são inseridos na lista nas posições 0 e 1.
    Os valores da soma de quadrados da regressão e dav soma de quadradosm dos residuos
    para cada variável também são obtidos e os respectivos vetores inseridos na lista nas
    posições 2 e 3.
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
      Após a aplicação da transformação de Householder para uma solução de mínimos
      quadrados, o fator triangular fica armazenado na porção triangular superior
      esquerda da matriz original. Este procedimento retorna a matriz de covariância
      (sem o fator de escala) da solução de mínimos quadrados. Como a inversão é feita
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
      Após a aplicação da transformação de Householder para uma solução de mínimos
      quadrados, o fator triangular fica armazenado na porção triangular superior
      esquerda da matriz original. Este procedimento retorna (na posição 2) a
      solução de mínimos quadrados.
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
  B.MLab:='Solução de mínimos quadrados';
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
     Raise Exception.Create(Func + ': Índice de Coluna inválido');

  if (Fim > M.NRows) or (Ini < 1) then
     Raise Exception.Create(Func + ': Índice de Linha inválido');
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

{Desvio Padrão}
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

{Coeficiente de variação}
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
