unit wsAvaliadorDeExpressoes;

(******************************************************************************
 AUTOR ------------------ Adriano Rochedo Conceição.
 AUXÍLIO E COAUTORIA ---- Roger Al-Alam Krolow
 VERSÃO ----------------- 4.01
 INICIO ----------------- Junho de 1995.
 ÚLTIMA MODIFICAÇÃO ----- 03 de novembro de 1998.
 INSTITUIÇÃO ------------ Universidade Federal de Pelotas. UFPEL (PESQUISA).
 OBJETIVOS -------------- Avaliar expressões matemáticas com variáveis.
 BIBLIOGRAFIA ----------- Aho,Alfred V. COMPILERS, PRINCIPLES AND TOOLS.
 ******************************************************************************

 DOCUMENTAÇÃO EXTERNA ---------------------------------------------------------

 CLASSES

   TStack .....................................................................
     Esta classe é utilizada internamente pela classe TAvaliator, mas nada
     impede de ser utilizada livremente por qualquer um. Simplesmente ela
     encapsula a funcionalidade mínima de uma Estrutura de dados conhecida como
     Pilha.

     MÉTODOS .....................

       Push (Pointer)
         Este aqui simplesmente coloca um ponteiro na pilha, mas por ser um
         ponteiro, significa que podemos empilhar qualquer estrutura, desde
         um simples byte até uma lista de instâncias de classes diferentes.

       Pop : Pointer
         Retira um elemento da pilha.

     .............................

   TAvaliator ..................................................................
     Esta é a principal classe desta unidade, responsável por interpretar desde
     simples funções numéricas até complexas equações matriciais/vetoriais.

     MÉTODOS .....................

       Create
         Cria uma instância para a classe.

       Destroy
         Destroi a instância da classe.

       RestoreOriginalTabVar: TTabVar
         No avaliador podemos trabalhar com diferentes tabelas de variáveis ou
         tabelas temporárias, mas para isto ele precisa guardar a referênca a
         sua tabela original. Este método simplesmente faz o avaliador utilizar
         sua própria tabela.

       Evaluate: TAnswer;
         Depois que a propriedade EXPRESSION rececer um valor (uma expressão),
         poderemos chamar EVALUATE quantas vezes quisermos, é claro que isto só
         fará sentido se houverem variáveis e mudarmos os valores destas variáveis a
         cada chamada. Este método retorna informações através de um Registro
         (TAnswer) como explicado abaixo:

         TAnswer = Record
           VarType  : TipoElem;   {Tipo do Resultado (vtNumeric, vtVector, vtMatrix)}
           Espec : TMatType;   {Se Tipo = vtMatrix: Tipo específico (Geral, Triangular, ...)}
           Valor : Double;       {Se Tipo = vtNumeric: Valor conterá o resultado}
           Ender : Pointer;    {Se Tipo <> vtNumeric: Endereco para Vetores ou Matrizes}
           Erro  : Integer;    {Código de erros}
           end;

       Calculate(String)
         Esta rotina só trabalha com expressões no formato A := B onde A é um identificador
         válido pré-existente ou não na tabela de símbolos e B uma expressão matemática.
         Se a propriedade PRINT estiver setado para TRUE, este procedimento irá jogar o
         resultado para o EDITOR

     CAMPOS .....................

       Expression : String
         Expressão matemática que será calculada.
         No momento da atribuição, será gerada uma exceção se a expressão estiver incorreta.

       TabVar     : TTabVar
         Tabela de variáveis usada pelo avaliador. Pode ser trocada.

       Print      : boolean
         Se verdadeiro, irá imprimir no Editor (se ele existir) os resultados.

       Reference  : boolean
         Se verdadeiro, significa que o avaliador, no momento de atribuir um valor
         a uma variável do tipo matriz ou vetor pré-existente, o fará usando o próprio
         endereço desta variável, senão o fará atribuindo uma cópia.
         Esta propriedade é muito utilizada quando se usa o método Calculate.

     .............................

   TFunctions
     Esta classe encapsula a funcionalidade de uma lista de funções do tipo F(x) = Y.
     Exemplos:
                          2*x + 4
                          x*x + 3.65*x + 5.565
                          sen(x) + cos(sen(x+1))

     MÉTODOS .....................

       Destroy
         Destroi a instância do objeto.

       AddFunction (String, Double, Double, Double)
         Adiciona uma função a lista.
         Parâmetros:
           (1) ... Função a ser adicionada.
           (2) ... Limite Inferior (valor mínimo de x)
           (3) ... Limite Superior (valor máximo de x)
           (4) ... Variação de x

       DelFunction (Integer)
         Deleta uma função da lista.
         Parâmetros:
           (1) ... Indice da função a ser deletada.

       GetValue (double, Integer): Double
         Retorna o valor da função para um dado x.
         Parâmetros:
           (1) ... Valor da variável x.
           (2) ... Índice da função que será calculada

       GetSerie (integer): TList
         Retorna uma lista com os valores calculados para os valores de x que estão
         no intervalo [Limite inferior, Limite superior].
         Em 16 bits é possível armazenar no máximo 16.000 pontos.
         Em 32 bits é superior a 140.000.000 de pontos.
         Cada elemento da lista é um ponteiro para um Double (Elemento = ^Double).

     CAMPOS .....................

       Func [integer]: TFunctionRec
         Retorna as informações sobre a função i.

 ------------------------------------------------------------------------------

 < HISTÓRICO >

     03/07/1996 - Inclusão do objeto TStack junto com a UNIT.
                  Otimização de resolução de expressões com:
                      Prepare();
                      EvalFunctionOtimiz();
                      Dispatch();

     10/07/1996 - Criação das Excessões.

     12/07/1996 - Eval agora plota gráficos de funções - Plot2D() .

     16/07/1996 - Chamada da função GaussElimination() através de SOLVEXY.

     18/07/1996 - Criação das rotinas:
                     DeleteVar(nome);
                     VarByName(nome);
                     VarByIndex(indice);

     07/08/1996 - Eval aceita várias Tabelas de Símbolos por associação.

     05/09/1996 - Implemantação da rotina EvalCondictions()

     27/09/1996 - Inclusao de operadores e funcoes - Roger.

     27/11/1996 - Function O alterada para melhorar o desempenho.
                  Inclusão de mais 5 funções:
                     Identity(nc);
                     Helmert(nc);
                     Control(nr);
                     Experim(nr);
                     PolOrth(vec);

     20/12/1996 - Inicio da Transformação para a Classe TAvaliator.
                  Conclusão: 23/12/1996

     10/04/1997 - Inclusão da rotina Calculate.
                  ERRO - Rotina < PegaTipoFunc >

                  < CODIGO ERRADO >
                    vtVector: Case Indice Of
                                1..21,
                                28,29,
                                31,32   : Result := vtNumeric;  <------- Erro Pos <20>
                                22..25,
                                    30  : Result := vtNumeric;
                                    40  : Result := vtMatrix;
                                  Else    Result := teNul;
                              End;
                  < CODIGO ERRADO >

     11/04/1997 - ERRO - Rotina < Obtem_Token >

                  < CODIGO ERRADO >
                    1: Case Exp[p] Of
                         'A'..'Z',
                         '1'..'9' : Tok := Tok + Exp[p];    <------- Erro Pos <2>
                         else
                           State:=5;
                       End;
                  < CODIGO ERRADO >

     24/04/1997 - Reconhecimento de duas novas Funções.
                  - VecDiag
                  - DiagVec

     06/05/1997 - Última verificação.

     22/08/1997 - Revisão da rotina OperaUnário
                  Operador Not -> ^

     25/06/1998 - Adição da classe TFunctions  {Rochedo}

     02/07/1998 - Documentação complementar  {Rochedo}

     03/11/1998 - Conversão para 32 bits - Delphi 4
       - re-inversão dos operadores booleanos na gramática.
         Rotina modificada: Percorre
         
       - Identificadores aceitam notação de ponto.
         Rotina modificada: Analise_Lexica --> Lexico

     26/11/1998 - Copia da referencia de um vetor na rotina Eval

 *****************************************************************************

DOCUMENTAÇÃO INTERNA ---------------------------------------------------------

O avaliador de expressoes e' um interpretador de expressoes que contem
operadores (descritos na tabela abaixo), identificadores de variaveis
(tipo matriz, tipo vetor e tipo escalar), constantes numericas (escalares)
e funcoes.

Inicialmente implementado em Pascal Simples (não OOP), o intepretador nes-
secitou de bastante tempo e paciência para chegar na sua versão atual.
Muita pesquisa foi feita para se resolver problemas como Associatividade
Inversa de Operadores, Operadores Unários, Funções com Multiplos Parâmetros,
Tomada de Decisões, etc.

ESQUEMA GRÁFICO :

    2 * Cos( X + 100 ) /0.5 ;

    ArrayPToken                    Tab_Const       Tab_Sim
    _________________________           _____         _________________________
   | Pos | Tipo | Simb | Ind |         |VALOR|       |Simb.|TipoElen|Val.|Ender|
==============================       =========     =============================
|0 |  0  | CST  | ---  |  0  | ----> |0|  2  | |-> |0|  X  |ESCALAR |2.39| --- |
==============================       ========= |   =============================
|1 |  1  |OPER  |  *   | --- |  |--> |1| 100 | |                  .
==============================  |    ========= |                  .
|2 |  2  |FUNC  | ---  |  4  |_ ||-> |2| 0.5 | |                  .
============================== |||   ========= |
|3 |  5  |OPEN  | ---  | --- | |||       .     |
============================== |||       .     |
|4 |  6  |IDENT | ---  |  0  | -||-------------|
============================== |||
|5 |  7  |OPER  |  +   | --- | |||
============================== |||         Tab_Fun
|6 |  8  | CST  | ---  |  1  | -||         ______
============================== | |        | Nome |
|7 |  9  |CLOSE | ---  | --- | | |      ==========    OBS:
============================== | |      |1| ABS  |    (Na tabela de funções os
|8 | 10  |OPER  |  /   | --- | | |      ==========     índices começam em 1)
============================== | |      |2| SQRT |
|9 | 11  | CST  | ---  |  2  | --|      ==========
============================== |        |3| SEN  |
|10| 12  |TERM  | ---  | --- | |        ==========
============================== |------> |4| COS  |
              .                         ==========
              .                         |5| TAN  |
                                        ==========
                                             .
                                             .
OBS: O campo (Ender) na Tabela de Símbolos se refere aos endereços de variáveis
do tipo (Matriz e Vetor) representados por Objetos TWSMatrix e TWSVec.

FUNCIONAMENTO :

    A primeira coisa a ser feita é a divisão da equação em pedaços lógicos.
Esses pedaços são chamados de TOKENS e são classificados em :
    Ident, Atrib, Oper, Cst, Func, Open, Close, Term, Nulo e são identifi-
cados pelo tipo TIPOTOKEN.
    Essa separação é feita pelas rotinas de  Análise_Léxica e armazenados
em uma estrutura chamada ARRAYPTOKEN que é uma TList (Lista Encadeada).
    Nessa lista são guardados estruturas do tipo TTOKEN formada pelos cam-
pos (Pos, Tipo, Simb, Ind) que contém informações sobre cada pedaço. Cada
Cada registro, dependendo do que esta armazenado no campo (Tipo), pode es-
tar associado a uma outra estrutura ( TTab - Tabela de Símbolos,  TConst -
Tabela de Constantes,  TFun - Tabela de Funções ) ou a nenhuma. E o campo
(Ind) contém o índice para uma dessas tabelas se for o caso.

    Depois da Análise Léxica passamos para a Análise Sintática, que vai percor-
rer a tabela de Tokens (ArrayPToken) verificando erros estruturais tais como :
2 + 5 -   ,   1 * ( 5 * ( 2 ) + 1   , variáveis desconhecidas, etc.

    Depois se faz a Análise Semântica, que verifica a concordância dos tipos.
Ex : Escalar + Escalar = Escalar  ,  QuickSort( Matriz ) dará um erro.

    Depois de tudo isso é feita a avaliação propriamente dita. A pilha é usada
tanto na Análise Semântica quanto na Avaliação, na primeira empilhamos tipos ,
na segunda valores.

OBS: A tabela de tipos está explicita no código, na função COMBINAM.

     Qualquer dúvida falar com o AUTOR !!!


TABELA DOS OPERADORES :

Operacao                        Simbolo Posicao         Associatividade Tipo
Troca de sinal                  -       prefixado       direita         unario
Adicao                          +       infixado        esquerda        binario
Subtracao                       -       "               "               "
Divisao                         /       "               "               "
Multiplicacao de matriz         *       "               "               "
Multiplicacao de elemento       #       "               "               "
Produto direto                  @       "               "               "
Potencia de matriz              **      "               direita         "
Potencia de elemento            ##      "               "               "
Divisao de elemento             #/      "               esquerda        "
Produto direto horizontal       @/      "               "               "
Concatenacao horizontal         ||      "               "               "
Concatenacao vertical           //      "               "               "
Elemento maximo                 <>      "               "               "
Elemento minimo                 ><      "               "               "
E                               &       "               "               "
Ou                              |       "               "               "
Nao                             ^       prefixado       direita         unario
Menor que                       <       infixado        esquerda        binario
Maior que                       >       "               "               "
Igual `a                        =       "               "               "
Menor ou igual `a               <=      "               "               "
Maior ou igual `a               >=      "               "               "
Diferente                       ^=      "               "               "

Tabela de precedencia de operadores (do maior para o menor):

1) ^       -(prefixado)    ##      **
2) #       <>      ><      #/      @/      @
3) *       /
4) +       -
5) ||      //
6) <       <=      >       >=      =       ^=
7) &
8) |

Foi escolhido o método descendente recursivo preditivo para a implementação do
avaliador de expressões. Como haviam operadores com associatividade à esquerda
foi necessario aplicar a técnica de tradução orientada à sintaxe com herança
de atributos, devido a eliminação da recursividade à esquerda.
A heranca de atributos foi implementada usando-se uma pilha.

Gramatica (1a. versao):

Exp -> E;
  E -> E|E
  E -> E&E
  E -> E<E  | E<=E | E>E | E>=E | E=E | E^=E
  E -> E||E | E//E
  E -> E+E  | E-E
  E -> E*E  | E/E
  E -> E#E  | E<>E | E><E | E#/E | E@/E | E@E
  E -> ^E   | -E   | E##E | E**E
  E -> id   | num  | (E)  | funcao(E)

Aplicando as trnsformações para considerar precedência dos operadores e
associatividade:

Exp -> E;
  E -> E|E
  F -> E&E
  H -> E<E  | E<=E | E>E | E>=E | E=E | E^=E
  I -> E||E | E//E
  K -> E+E  | E-E
  L -> E*E  | E/E
  M -> E#E  | E<>E | E><E | E#/E | E@/E | E@E
  O -> ^E   | -E   | E##E | E**E
  T -> id   | num  | (E)  | funcao(E)

Exp -> E;
  E -> E|F  | F
  F -> F&H  | H
  H -> H<I  | H<=I | H>I | H>=I | H=I | H^=I | I
  I -> I||K | I//K | K
  K -> K+L  | K-L  | L
  L -> L*M  | L/M  | M
  M -> M#O  | M<>O | M><O | M#/O | M@/O | M@O | O
  O -> ^O   | -O   | T##O | T**O | T    (associatividade `a direita)
  T -> id   | num  | (E)  | funcao(E)

Eliminando recursividade à esquerda (nas produções, "e" significa
string vazia):

Exp  ->  E;
  E  ->  FE'
  E' ->  |FE'  | e
  F  ->  HF'
  F' ->  &HF'  | e
  H  ->  IH'
  H' ->  <IH'  | <=IH' | >IH' | >=IH' | =IH' | ^=IH' | e
  I  ->  KI'
  I' ->  ||KI' | //KI' | e
  K  ->  LK'
  K' ->  +LK'  | -LK' | e
  L  ->  ML'
  L' ->  *ML'  | /ML' | e
  M  ->  OM'
  M' ->  #OM'  | <>OM' | ><OM' | #/OM' | @/OM' | @OM' | e
  O  ->  ^O    | -O    | T##O  | T**O  | T
  T  ->  id    | num   | (E)   | funcao(E)

Adicionando os esquemas de tradução:

Exp -> E; { Exp.Val := E.Val }
  E -> E1 | F { E.Val := E1.Val | F.Val }
  E -> F { E.Val := F.Val }
  F -> F1 & H { F.Val := F1.Val & H.Val }
  F -> H { F.Val := H.Val }
  H -> H1 < I { H.Val := H1.Val < I.Val }
  .
  .
  .
  O -> ^ O1 { O.Val := ^ O1.Val }
  O -> - O1 { O.Val := -O1.Val }
  O -> T ## O1 { O.Val := T.Val ## O1.Val }
  O -> T ** O1 { O.Val := T.Val ** O1.Val }
  O -> T { O.Val := T.Val }
  T -> id { T.Val := id.simbolval }
  T -> num { T.Val := num.lexval }
  T -> (E) { T.Val := E.Val }
  T -> funcao(E) { T.Val := funcao(E.Val) }

Esquemas de tradução com herança de atributos:

Exp -> E; {Exp.val := E.val}
  E -> F { E´.i:= F.val}
       E´ { E´.val:= E´.s}
  E´-> |
       F {E´1.i:= E´.i | F.val}
       E´1{E´.s:= E´1.s}
  E´-> e { E´.s:= E´.i}
  F -> H { F´.i:= H.val}
       F´{ F.val:= F´.s}
  F´-> &
       H { F´1.i:= F´.i| H.val}
       F´1{ F´.s:= F´.s}
  F´-> e {F´.s:= F´.i}
  .
  .
  .
  0 -> ^01 {0.val:= ^01.val}
  0 -> - 01 {0.val:= - 01.val}
  0 -> T ## 01 {0.val:= T.val ## 01.val}
  0 -> T ## 01 {0.val:= T.val ** 01.val}
  0 -> T {0.val:= T.val}
  T -> id {T.val:= id.símbolos}
  T -> num { t.val:= num.lexval}
  T -> (E) {T.val:= E.val}
  T -> funçâo(E) { T.val:= funçâo(E.val)}


OBSERVAÇÕES:

26/06/97 - Para o reconhecimento dos tokens não é relevante se os caracteres são
           maiúsculos ou minúsculos. O reconhecimento de variáveis fica a cargo da classe
           TTabVar. A propriedade TTabVar.CaseSensitive deve ter valor FALSE para que seja ignorada
           a diferença.                                              - Rochedo e Roger

05/08/99 - Foi adicionado o método DelFunction na classe TFunctions. Cuidar para
           não usar o método delete da própria Tlist, para que não fique lixo na
           memória, já que com o DelFunction haverá um dispose e com o delete da
           TList (pai) será eliminado apenas o índice.                - Zeh


--------------------------------------------------------------------------------

Nomes das funcoes: Alex(03/09/97)
Legenda:
E - Função que opera com argumentos escalares.
V - Função que opera com argumentos Vetores.
M - Função que opera com argumentos Matrizes.

Função:   Argumentos:
ABS       - EVM
EXP       - EVM
APROXIMA  - EVM
INT       - EVM
LN        - EVM
RAIZ      - EVM
ARCTAN    - EVM
ARCSEN    - EVM
ARCCOS    - EVM
SEN       - EVM
COS       - EVM
SENH      - EVM
COSH      - EVM
TAN       - EVM
LOG       - EVM
ANG       - EVM
LGAMA     - EVM
FLOOR     - EVM
CEIL      - EVM
INV       - EVM
FRAC      - EVM
ABSNORM   - VM
TANH      - EVM
MEDIA     -  VM
SOMA      -  VM
SQUAD     -  VM
POSTOEMP  -  VM
RESOLVE   -
ENORM     -  VM
ACUM      -  VM
TODOS     -  VM
ALGUM     -  VM
MAX       -  VM
MIN       -  VM
QSORT     -  V
VECDIAG   -  V
QNORM     -  V
VNORM     -  V
VALUNIF   -  E
TRANSP    -   M
AtA       -   M
AAt       -   M
I         -   M
HELMERT   -   M
PERFIL    -   M
CONTROLE  -   M
EXPER     -   M
PORTOG    -   M
DIAGVEC   -   M
TRACO     -   M
POSTO     -   M
GINV      -   M
CHOL      -   M
GS        -   M
MGS       -   M
HPROJ     -   M
QPROJ     -   M
ECHELON   -   M
*)

interface
uses Classes,
     SysUtils,
     SysUtilsEx,
     wsTabelaDeSimbolos,
     wsConstTypes,
     wsMatrix,
     wsVec,
     wsExceptions;

Const
    MaxFunc      = 65; {Número de funções , ou número de tipos de TFun }

Type
    TFun    = Array [0..MaxFunc-1] Of String[10];

Const
    TamTab_Fun  = MaxFunc;
    TAMTAB      = 200;
    AMAI        = 65;
    ZMAI        = 90;
    AMIN        = 97;
    ZMIN        = 122;
    Operador     : TCharSet = ['-','+','*','#','@','/','|','>','<','&','^','=','`'];
    Letras       : TCharSet = ['A'..'Z','a'..'z'];
    Terminador   : TCharSet = [';'];      {///}
    Outros       : TCharSet = ['(',')'];

Var
    Digitos      : TCharSet = ['0'..'9','.'];

Tab_Fun :
TFun =('ABS'    ,'EXP'     ,'APROXIMA','INT'     ,'LN'       ,'RAIZ'     ,'ARCTAN',
       'ARCSEN' ,'ARCCOS'  ,'SEN'     ,'COS'     ,'SENH'     ,'COSH'     ,'TAN',
       'LOG'    ,'ANG'     ,'LGAMA'   ,'FLOOR'   ,'CEIL'     ,'INV'      ,'FRAC',
       'ABSNORM','MEDIA'   ,'SOMA'    ,'SQUAD'   ,'RESOLVE'  ,'TANH'     ,'POSTOEMP',
       'QSORT'  ,'ENORM'   ,'ACUM'    ,'T'       ,'ATA'      ,'AAT'      ,'I',
       'HELMERT','CONTROLE','EXPER'   ,'POLORT'  ,'VECDIAG'  ,'DIAGVEC'  ,'TODOS',
       'ALGUM'  ,'TRACO'   ,'POSTO'   ,'GINV'    ,'MAX'      ,'MIN'      ,'CHOL',
       'PERFIL' ,'VALUNIF' ,'QNORM'   ,'VNORM'   ,'ECHELON'  ,'GS'       ,'MGS',
       'HPROJ'  ,'QPROJ'   ,'TRIGAMA' ,'ALOC'    ,'HILBERT'  ,'G2INV'    ,'HERMITE',
       'IND'    ,'CMEDIA');
       { funções TODOS, ALGUM (11/09/97 - Alex ) }
       {adicionar CHOL - so para matriz simetrica}

Type
    TStr2     = String[2];

    TipoToken = (Ident, Atrib, Oper, Cst, Func, Open, Close, Term, Nulo);
    TRunType  = (Sintatico, Semantico, Avaliador);

    {Implementação da estrutura do token}
    Ptoken = ^Ttoken;
    Ttoken =  record
      Pos       : Integer;   {Posição do primeiro caracter do Token na String de entrada}
      TokenType : TipoToken; {Tipo do token (Ident, Oper, Cst, etc)}
      Simb      : String[2]; {Usado quando o token for um operador}
      Ind       : Integer;   {Índice das tabelas de (Símbolos, Const., Funções)}
    end;

  {Implementação da tabela de constantes}
  PConst = ^TConst;
  TConst = Record
      Value : Double;
    end;

  {Implementação da tabela de temporários, que irá armazenar os resultados
   intermediários das operações. Poderão ser valores teEscalares, ponteiros
   para matrizes ou ponteiros para vetores.}
  PTemp = ^TTemp;
  TTemp = Record
      VarType   : TwsEnumVarType;       {Tipo do identificador (vtNumeric, vtVector, vtMatrix)}
      Espec     : TwsEnumMatType;       {Tipo da matriz (Geral, Triangular, ...)}
      Value     : Double;         {Escalar - valor da const. ou variavel}
      Ender     : Pointer;        {Endereco para Vetores ou Matrizes}
      Error     : Integer;        {Verificação da existencia de erros}
    end;

  //TAnswer = TTemp;
  TEvalResult = class
  private
    FResult : TTemp;
    function GetAsFloat: Double;
    function GetAsMatrix: TwsMatrix;
    function GetAsString: String;
    function GetAsVector: TwsVec;
    function GetIsFloat: Boolean;
    function GetIsMatrix: Boolean;
    function GetIsVector: Boolean;
    function getMatrixType: TwsEnumMatType;
    function getResultType: TwsEnumVarType;
    function GetError: Integer;
  public
    property Error : Integer read GetError;

    property ResultType: TwsEnumVarType read getResultType;
    property MatrixType: TwsEnumMatType read getMatrixType;

    property IsFloat   : Boolean   read GetIsFloat;
    property IsVector  : Boolean   read GetIsVector;
    property IsMatrix  : Boolean   read GetIsMatrix;

    property AsFloat   : Double    read GetAsFloat;
    property AsString  : String    read GetAsString;
    property AsVector  : TwsVec    read GetAsVector;
    property AsMatrix  : TwsMatrix read GetAsMatrix;
  end;

Type
  EvalException = Class(Exception);

  {Objeto TStack (Pilha auxiliar nos cálculos)}
  TStack = Class(TList)
     Procedure Push(P : Pointer);
     Function  Pop : Pointer;
  end;

  TAvaliator = Class(TObject)
  Private
    Expr         : String ;       {Variável que recebe uma expressão}
    Ind, Tam     : Integer;       {Variáveis auxiliares}
    Erro         : Word;
    Funcao,                       {Verifica se um identificador é uma função}
    ErroBoolea,                   {Para verificação de erros internos}
    EstaLendo    : Boolean;       {Verdadeira enquanto Stoken <> ';' }
    PriVez       : Boolean;       {Limpa o editor a uma vez}
    T            : TipoToken;     {Variável auxiliar para o tipo do token}
    TElem        : TwsEnumVarType;      {Auxiliar para o tipo de variável}
    Temp         : Integer;       {Variável auxiliar}
    IndPToken    : Integer;       {Índice do token corrente}
    Cop          : Integer;       {Variável que conta operadores}
    PExpr        : Pointer;       {Ponteiro retornado pelo Sintático}
    PSem         : PToken ;       {PExpr convertido para PToken}
    Maior        : Integer;       {Auxiliar}
    PosError     : Integer;       {Posicao do primeiro caracter do token}
    ErrorType    : Integer;       {Tipo de erro sintático}
    Tab_Sim      : TwsTabVar;       {Tabela de simbolos (Variáveis) }
    Tab_Cst      : TList;         {Tabela de cosntantes}
    Tab_Temp     : TList;         {Tabela de temporários}
    ArrayPToken  : TList;         {Lista de Tokens}
    Stack        : TStack;        {Pilha (TList) }
    Expressao    : String;
    Token        : String[15];    {Variável que representa um Token}
    Stoken       : String[15];    {Variável auxiliar para concatenação dos tokens}
    VarDescon    : String[15];    {Variável Desconhacida}
    Pos          : Byte;          {Posição do erro na Expressão}

    TabTemp      : TwsTabVar;       {Usado tempor. para manter uma tabela re-associada}
    FResult      : TEvalResult;   {Usado para guardar o resultado da última avaliação}
    FPrint: Boolean; { Se True faz com que Calculate sempre imprima o resultado }
    FReference: Boolean; {Alex/Adriano 21/10/97} {Se true a associacao de tipos
                           vtMatrix e vtVector é por referencia, senão é por cópia.}

    Procedure LiberaMem;
    Procedure CriaTabelas;
    Procedure DestroiTabelas;
    Procedure Insere_Temp_Escalar(Value : Double);
    function  Insere_Cst(Const Simbolo: String ):integer;
    Function  PegaTipoIdent(Ind : Word) : TwsEnumVarType;

    Function  Analise_Lexica: Integer;
    Function  Percorre (RunType :TRunType; Var Erro: Word) : Boolean ;

    Function  Combinam(T1, T2 : TwsEnumVarType ; Op : String) : TwsEnumVarType;
    Function  PegaTipoFunc(Indice : TwsEnumConstFun; VarType : TwsEnumVarType) : TwsEnumVarType;

    Procedure Opera(Simb: TStr2; op1, op2: PTemp; Var Res: PTemp; Var Erro: Word);
    Function  OperaUnario(Simb: TStr2; op1: PTemp): PTemp;
    Procedure OperaFuncao(Ind: TwsEnumConstFun; op1: PTemp; Var Res: PTemp; Var Erro: Word);

    Function  Eval(Const Expr : String; var PRes: PTemp): Integer ;
    Procedure SetExpresion(Const S: String);
    Function  GetTabVar: TwsTabVar;
    Procedure SetTabVar (Value : TwsTabVar);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function RestoreOriginalTabVar: TwsTabVar;

    {Depois que a propriedade EXPRESSION rececer um valor, podemos chamar EVALUATE
     quantas vezes quisermos, é claro que isto só faz sentido se mudarmos os valores
     das variáveis }
    Function  Evaluate: TEvalResult;

    {Esta rotina só trabalha com expressões do tipo A := B onde A é um identificador
     válido pré-existente na tabela de símbolos e B uma expressão matemática
     Se a propriedade PRINT estiver setado para TRUE, este procedimento irá jogar o
     resultado para o EDITOR}
    Procedure Calculate(Const Express: String);

    Property Expression : String    Read Expr       Write SetExpresion;
    Property TabVar     : TwsTabVar Read GetTabVar  Write SetTabVar;
    property Print      : boolean   Read FPrint     Write FPrint;
    property Reference  : boolean   Read FReference Write FReference;
  End;

Type
  pTFunctionRec = ^TFunctionRec;
  TFunctionRec = Record
                   LI   : Double;
                   LS   : Double;
                   Incr : Double;
                   Eval : TAvaliator;
                 End;

  TFunctions = class (TList)
  Private
    Function GetFunc (index: integer): TFunctionRec;
  Public
    Destructor Destroy; Override;
    Procedure AddFunction(const Funcao : String; LI, LS, Incremento : Double);
    Procedure DelFunction(Index : Integer);
    Procedure SetParams(index: Integer; const Funcao: String; LI, LS, Incr: Double);
//    Procedure SetParams(index: Integer; LI, LS, Incr: Double);
    Function  GetValue (x: double; FunctionIndex: Integer): Double;
    Function  GetSerie (index: integer): TList;
    Property  Func [index: Integer]: TFunctionRec Read GetFunc;
  end;

  Function IsLogicOperator(const op: String): Boolean;

implementation

Uses wsDistribuicoesRandomicas, wsFuncoesDeEscalares, Dialogs;

Function IsLogicOperator(const op: String): Boolean;
begin
  Result :=  (op = '<' ) or (op = '>') or (op = '<=') or
             (op = '>=') or (op = '=') or (op = '^=');
end;

{ -------------  Implementação do objeto TStack  ------------- }

Procedure TStack.Push( p : Pointer );
Begin
  Add(p);
End; { Tstack.push }

Function TStack.Pop : Pointer;
var p : PTemp;
Begin
  New(p);
  p^ := PTemp(Last)^;
  With p^ do
    begin
    Pop := p;
    Dispose(PTemp(Last));
    Remove(Last);
    end;
End; { TStack.Pop }

{--------------------- R O T I N A S   A U X I L I A R E S -------------------}

Constructor TAvaliator.Create;
Begin
  Inherited Create;
  Include(Digitos, DecimalSeparator);
  FResult := TEvalResult.Create;
  FPrint := False;
  FReference := True;
  CriaTabelas;
  Tab_Sim := TwsTabVar.Create;
  TabTemp := Nil;
  Expr := '';
End;

Destructor TAvaliator.Destroy;
Begin
  FResult.Free;
  DestroiTabelas;
  If Assigned(TabTemp) Then Tab_Sim := TabTemp;
  Tab_Sim.Free;
  Inherited Destroy;
End;

{Rotina que calcula o valor de uma espressão e atribui a uma variável,
 colocando-a na tabela de símbolos atual do avaliador.
 OBS: Se a variável que receber o valor já existir na tabela, será automaticamente
      sobreescrita.}
Procedure TAvaliator.Calculate(Const Express: String);
Var NomeVar  : String[15];
    i        : Integer;
    Res      : TEvalResult;
Begin
  i := System.Pos(':=', Express);
  If i = 0 Then
     Raise EvalException.Create('Falta de Operador de Atribuição < := > na Expressão');

  NomeVar   := System.Copy(Express, 1, i-1);
  Expressao := System.Copy(Express, i+2, Length(Express));
  SysUtilsEx.AllTrim(NomeVar);
  If Not SysUtilsEx.IsValidIdent(NomeVar) Then
     Raise EvalException.CreateFmt('< %s > não é uma Variável Válida', [NomeVar]);

  Expression := Expressao;
  Res := Evaluate;

  If Res.FResult.Error = 0 Then
     Begin
     If TabVar.Exist(NomeVar, i) Then TabVar.DeleteVar(NomeVar);
     Case Res.FResult.VarType of
       vtNumeric : begin
                   TabVar.AddFloat(NomeVar, Res.FResult.Value);
                   {$ifdef prj_WinStat}
                   if FPrint then TabVar.Print([NomeVar]);
                   {$endif}
                   end;

       vtVector  : begin
                   TabVar.AddVector(NomeVar, TWSVec(Res.FResult.Ender));
                   {$ifdef prj_WinStat}
                   if FPrint then TabVar.Print([NomeVar]);
                   {$endif}
                   end;

       vtMatrix  : begin
                   TabVar.AddMatrix(NomeVar, TWSMatrix(Res.FResult.Ender));
                   {$ifdef prj_WinStat}
                   if FPrint then TabVar.Print([NomeVar]);
                   {$endif}
                   end;
       End; {Case}
     End
  Else
     ShowException(Res.FResult.Error, '');
End; {Calculate}

Function  TAvaliator.Evaluate: TEvalResult;
Var PRes: PTemp;
Begin
  New(PRes);
  try
    Eval(Expr, PRes);
    FResult.FResult := PRes^;
    Result := FResult;
  finally
    Dispose(PRes);
  end;  
End;

Procedure TAvaliator.SetExpresion(Const S: String);
Var Op : Pointer;
Begin
  If S <> Expr Then
     Begin
     LiberaMem;

     Expr := UpperCase(S) + ';';
     Tam := Length(Expr);

     if Analise_Lexica = 0 then

        If Percorre(Sintatico, Erro) Then
           Begin
           PSem := PExpr;
           Percorre(Semantico, Erro);
           Op := Stack.Pop;
           TElem := PTemp(Op)^.VarType;
           Case TElem Of
             vtNumeric, vtVector, vtMatrix : {Nada}; { Sucesso }
             Else
               Begin
               Erro := ENTypeMismatch;
               End
             End; {Case}
           Dispose(PTemp(Op));
           End {If Percorre Sintático} Else Begin {Nothing }End

        Else {Else Léxico}
           Begin
           Erro := ENUnknownVariable;
           End; {Analise_Lexica}

     If Erro <> 0 Then
        Begin
        LiberaMem;

        {Gerará uma exceção do erro específico}
        ShowException(Erro, VarDescon);
        End;

     End;
End;

(*
{Criará uma janela (Objeto do tipo TPlot2DForm) e o mostrará dependendo do
 parâmetro MOSTRA.
 Quem possui o método de desenho é a janela, a qual poderá por si só modi-
 ficar seus atributos (Função, limites, precisão).
 A equação deve ser em função de X ( Y = f(X) ). }

Function TAvaliator.Plot2D(Expr: String; Li, Ls, Precisao: Double; Mostra: Boolean): TPlot2DForm;
Var PRes   : PTemp;
    Erro   : Integer;
    NewTab : TwsTabVar;
    OldTab : TwsTabVar;
Begin
  If Li - Ls >= 0 Then Exit;

  NewTab := TwsTabVar.Create(Nil);
  NewTab.AddFloat('X', 0);

  OldTab := GetTabVar;
  SetTabVar(NewTab);

  New(PRes);
  Try
    Erro := Eval(Expr, PRes, True, True);
  Finally
    Dispose(PRes);
    SetTabVar(OldTab);
    NewTab.Free;
  End;

  If Erro <> 0 Then ShowException(Erro);
  Result := TPlot2DForm.Create(Nil, Expr, Li, Ls, Precisao);
  If Mostra Then Begin
    Result.Show;
    Result.CalculaGrafico;
  End Else
    Result.CalculaGrafico;
End;
*)

Procedure TAvaliator.SetTabVar(Value: TwsTabVar);
Begin
  If TabTemp = Value Then
     TabTemp := Nil
  Else
     TabTemp := Tab_Sim;

  Tab_Sim := Value;
End;

Function TAvaliator.GetTabVar: TwsTabVar;
Begin
  Result := Tab_Sim;
End;

Function TAvaliator.RestoreOriginalTabVar: TwsTabVar;
Begin
  Tab_Sim := TabTemp;
  TabTemp := Nil;
End;

{Libera recursos alocados da mamória}
Procedure TAvaliator.LiberaMem;
Var Pco      : PConst;
    Pte      : PTemp;
    Pt       : PToken;
    i        : Integer;
Begin
  {desaloca os elementos da tebela de temporários}
  For i := 0 to Tab_Temp.Count-1 do Begin
    Pte := Tab_Temp.Items[i];
    Dispose(Pte);
  End;
  If Tab_Temp.Count > -1 Then Tab_Temp.Clear;

  {desaloca os elementos da tebela de constantes}
  For i := 0 to Tab_Cst.Count-1 do Begin
    Pco := Tab_Cst.Items[i];
    Dispose(Pco);
  End;
  If Tab_Cst.Count > -1 Then Tab_Cst.Clear;

  {libera os tokens alocados}
  For i := 0 to ArrayPToken.Count-1 do Begin
    Pt := ArrayPToken.Items[i];
    Dispose(Pt);
  End;
  If ArrayPToken.Count > -1 Then ArrayPToken.Clear;
End; {LiberaMem}

{Insere valores intermediários na tabela de temporários}
Procedure TAvaliator.Insere_Temp_Escalar(Value : Double);
var P : PTemp;
Begin
  New(P);
  P^.Value := Value;
  Tab_Temp.Add(P);
End;

{insere uma constante na tabela}
function TAvaliator.Insere_Cst(Const Simbolo : String): Integer;
Var
  P        : PConst;
Begin
  New(P);
  P^.Value := StrToFloat(Simbolo);
  Tab_Cst.Add(p);
  Result := Tab_Cst.Count-1;
End; {Insere_Cst}

Procedure TAvaliator.CriaTabelas;
Begin
  Tab_Cst     := TList.Create;
  Tab_Temp    := Tlist.Create;
  ArrayPToken := TList.Create;
  Stack       := TStack.Create;
End;

Procedure TAvaliator.DestroiTabelas;
Begin
  LiberaMem;
  Try
    If Assigned(Tab_Cst) Then Tab_Cst.Free;
    If Assigned(Tab_Temp) Then Tab_Temp.Free;
    If Assigned(Stack) Then Stack.Free;
    If Assigned(ArrayPToken) Then ArrayPToken.Free;
  Except
    {nada}
  End;
End;

{O 'ind' é o índice do ArrayPToken}
Function TAvaliator.PegaTipoIdent(Ind : Word) : TwsEnumVarType;
var Indice : Integer;
 Begin
  {Recebe o índice da tabela de Tokens}
  Indice := PToken(ArrayPToken.Items[Ind])^.Ind;
  Try
    Result := Tab_Sim.Vars[Indice].VarType;
  Except
    on EListError do Result := vtNull;
  End;
End;

{-------------------- A N A L I S E   L É X I C A ----------------------}

Function TAvaliator.Analise_Lexica: Integer;
var Pos    : integer;
    Erro   : integer;

   Function Lexico(Var Pos: Integer; Var Erro: Integer): Integer;
   const
       ZERO = 48;
       NOVE = 57;
   var
       PosInic     : Integer;
       Tip         : TipoToken;
       Pont_Token  : Ptoken;
       Token       : String;

     function Obtem_Token(Var Pos: Integer; Var Tok: String): Integer;
     var p, State: Byte;
     Begin
       p := Pos-1;
       State := 0;
       Repeat
         inc(p);
         case State of
           0  :begin
                 case expr[p] of
                   ' '                :{Nada};
                   'A'..'Z', '_'      :state := 1;
                   '0'..'9'           :state := 2;
                   '+','&','=','(',
                   ')',';'            :state := 6;
                   '*'                :state := 7;
                   '#'                :state := 8;
                   '@'                :state := 9;
                   '/'                :state := 10;
                   '|'                :state := 11;
                   '<'                :state := 12;
                   '-'                :state := 13;
                   '>'                :state := 14;
                   '^'                :state := 15;
                   else
                      state := 16;
                   end;
                 if Expr[p] <> ' ' then Tok := Expr[p];
               end;
(*
           1  :case expr[p] of
                   'A'..'Z', '0'..'9', '_' :Tok := Tok + Expr[p]; {26/12/1997} {Rochedo}
                   '.'                     :begin
                                            Tok := Tok + Expr[p];
                                            state := 17;
                                            end;
                   else
                     state := 5;
                   end;
*)
            1: if Expr[p] in ['A'..'Z', '0'..'9', '_'] then
                  Tok := Tok + Expr[p]
               else
                  if Expr[p] = DecimalSeparator then
                     begin
                     Tok := Tok + Expr[p];
                     state := 17;
                     end
                  else
                     state := 5;
{
           2  :case Expr[p] of
                  '0'..'9'            :tok := tok + Expr[p];
                  '.'                 :begin
                                       tok := tok + Expr[p];
                                       state := 3;
                                       end;
                  else
                    state:=5;
                  end;
}
            2: if Expr[p] in ['0'..'9'] then
                  tok := tok + Expr[p]
               else
                  if Expr[p] = DecimalSeparator then
                     begin
                     tok := tok + Expr[p];
                     state := 3;
                     end
                  else
                     state := 5;

            3  :case Expr[p] of
                  '0'..'9'            :begin
                                       tok := tok + Expr[p];
                                       state := 4;
                                       end;
                  else
                    state := 16;
                  end;

            4  :case Expr[p] of
                  '0'..'9' :tok := tok + Expr[p];
                  else
                     state := 5;
                  end;

            5  :begin
                pos := p-1;
                Result := 0;
                exit;
                end;

            6  :begin
                pos := p;
                Result := 0;
                exit;
                end;

            7  :case Expr[p] of
                  '*'  :begin
                        tok := tok + Expr[p];
                        state := 6;
                        end;
                  else
                    state := 5;
                  end;

            8  :case Expr[p] of
                  '#'  :begin
                        tok := tok + Expr[p];
                        state := 6;
                        end;
                  '/'  :begin
                        tok :=tok + Expr[p];
                        state := 6;
                        end;
                  else
                    state := 5;
                  end;

            9  :case Expr[p] of
                  '/'  :begin
                        tok := tok + Expr[p];
                        state := 6;
                        end;
                  else
                    state := 5;
                  end;

            10  :case Expr[p] of
                   '/'  :begin
                         tok := tok + Expr[p];
                         state := 6;
                         end;
                   else
                     state := 5;
                   end;

            11  :case Expr[p] of
                   '|'  :begin
                         tok := tok + Expr[p];
                         state := 6;
                        end;
                   else
                      state := 5;
                   end;

            12  :case Expr[p] of
                   '>'  :begin
                         tok := tok + Expr[p];
                         state := 6;
                         end;
                   '='  :begin
                         tok := tok + Expr[p];
                         state := 6;
                         end;
                   else
                      state := 5;
                   end;

             13  :case Expr[p] of
                    '/'  :begin
                          tok :=tok + Expr[p];
                          state := 6;
                        end;
                    else
                       state := 5;
                    end;

             14  :case Expr[p] of
                    '<'  :begin
                          tok := tok + Expr[p];
                          state := 6;
                          end;
                    '='  :begin
                          tok := tok + Expr[p];
                          state := 6;
                          end;
                    else
                       state := 5;
                    end;

             15  :case Expr[p] of
                    '='  :begin
                          tok := tok + Expr[p];
                          state := 6;
                        end;
                   else
                      state := 5;
                   end;

           16  :begin {Erro}
                  pos := p;
                  lexico := 1;
                  exit;
                end;

           17  :Case Expr[p] of
                  'A'..'Z', '_': begin
                                 tok := tok + Expr[p];
                                 state := 1;
                                 end;
                  else
                     State := 16; {Erro}
                  end;

           End;
       Until FALSE;
     End; { obtem_token }

     Function EFuncao(Const Token: String): Boolean;
     var
       i : Byte;
     begin
       Result := FALSE;
       for i := 0 to TamTab_Fun-1 do    {11/04/97}
         if Token = Tab_Fun[i] then
            Begin
            Result := TRUE;
            Break;
            End;
     end; { EFuncao }

     function IndFunc(Const Token: String): Integer;
     begin
       for Result := 0 to TamTab_Fun-1 do {03/11/1998}
         if Token = Tab_Fun[Result] then Exit;
       Result := -1;
     end; { IndFunc }

     Function AlocaToken(PosI : integer; t : TipoToken;
                         Const Token: String ; Var Erro: Integer): Ptoken;
     Begin
       New(Result);
       With Result^ do
         Begin
         Case t of
             ident  : If Not Tab_Sim.Exist(Token, Ind) Then
                         Begin
                         Dispose(Result);
                         Result := Nil;
                         VarDescon := Token;
                         Erro := ENUnknownVariable;
                         Exit;
                         End;

             term   : simb := ';';
             oper   : simb := Token;
             cst    : ind  := Insere_cst(Token);
             func   : ind  := IndFunc(Token);
         End;
         Pos := PosI;
         TokenType := t;
       End; {With}
     End; { AlocaToken }

     Function GetTokenType(Const Token: String): TipoToken;
     begin
       if (ord(Token[1]) >= ZERO) and (ord(Token[1]) <= NOVE) then
          Result := Cst
       else if EFuncao(Token) then
          Result := Func
       else if SysUtilsEx.IsValidIdent(Token) Then
          Result := Ident
       else
          case Token[1] of
            '('  :Result := Open;
            ')'  :Result := Close;
            else
              if Token[1] in Operador then
                 Result := Oper
              else
                 Result := Term;
          end; {Case}
     end; { TipoToken }

  Begin { Lexico }
    PosInic := Pos;
    if Obtem_Token(Pos, Token) = 0 then
       begin
       Tip := GetTokenType(Token);
       Pont_Token := AlocaToken(PosInic, Tip, Token, Erro);
       If Pont_Token <> Nil Then ArrayPToken.Add(Pont_Token);
       Result := 0;
       end
    else
       Result := 1;
  End; { Lexico }

Begin {Analise_Lexica ----------------------------------------------------------- }
  Pos := 1;
  Erro := 0;

  While (Pos <= Length(Expr)) and (Erro = 0) Do
     Lexico(Pos, Erro);

  Result := Erro;
End; {Analise_Lexica ----------------------------------------------------------- }

 {--------------------------------------------------------------------------}
 {--------------- FUNÇÕES AUXILIARES DA ANÁLISE SEMÂNTICA ------------------}
 {--------------------------------------------------------------------------}

Function TAvaliator.Combinam(T1, T2 : TwsEnumVarType ; Op: String): TwsEnumVarType;
Begin
  Case T1 Of
    vtNumeric : Case T2 Of
      vtNumeric : If (Op = '+' ) or (Op = '-' ) or (Op = '*' ) or (Op = '#' ) or
                     (Op = '@' ) or (Op = '@/') or (Op = '/' ) or (Op = '##') or
                     (Op = '<>') or (Op = '><') or (Op = '&' ) or (Op = '|' ) or
                     (Op = '=' ) or (Op = '<' ) or (Op = '<=') or (Op = '>' ) or
                     (Op = '>=') or (Op = '^=') or (Op = '**') or (Op = '//') or
                     (Op = '||')
                  Then
                    If (Op = '//') or (Op = '||') Then
                       Result := vtVector
                    Else
                       Result := vtNumeric
                  Else
                    Result := vtNull;

      vtVector  : If (Op = '+' ) or (Op = '-' ) or (Op = '/' ) or (Op = '#' ) or
                     (Op = '*' ) or (Op = '##') or (Op = '<>') or (Op = '><') or
                     (Op = '&' ) or (Op = '|' ) or (Op = '=' ) or (Op = '<' ) or
                     (Op = '<=') or (Op = '>' ) or (Op = '>=') or (Op = '^=') or
                     (Op = '**') or (Op = '@' ) or (Op = '@/') or (Op = '//') or
                     (Op = '||')
                  Then Result := vtVector
                  Else Result := vtNull;

      vtMatrix : If (Op = '+' ) or (Op = '-' ) or (Op = '/' ) or (Op = '#' ) or
                    (Op = '*' ) or (Op = '##') or (Op = '<>') or (Op = '><') or
                    (Op = '&' ) or (Op = '|' ) or (Op = '=' ) or (Op = '<' ) or
                    (Op = '<=') or (Op = '>' ) or (Op = '>=') or (Op = '^=') or
                    (Op = '@' ) or (Op = '@/') or (Op = '//') or (Op = '||')
                 Then Result := vtMatrix
                 Else Result := vtNull;
    End;


    vtVector  : Case T2 Of
      vtNumeric : If (Op = '+' ) or (Op = '-' ) or (Op = '/' ) or (Op = '#' ) or
                     (Op = '*' ) or (Op = '##') or (Op = '<>') or (Op = '><') or
                     (Op = '&' ) or (Op = '|' ) or (Op = '=' ) or (Op = '<' ) or
                     (Op = '<=') or (Op = '>' ) or (Op = '>=') or (Op = '^=') or
                     (Op = '@')  or (Op = '@/') or (Op = '||') or (Op = '//')
                 Then Result := vtVector
                 Else Result := vtNull;

      vtVector  : If (Op = '+' ) or (Op = '-' ) or (Op = '/' ) or (Op = '*' ) or
                     (Op = '#' ) or (Op = '##') or (Op = '<>') or (Op = '><') or
                     (Op = '&' ) or (Op = '|' ) or (Op = '=' ) or (Op = '<' ) or
                     (Op = '<=') or (Op = '>' ) or (Op = '>=') or (Op = '^=') or
                     (Op = '@' ) or (Op = '@/') or (Op = '||') or (Op = '//')
                  Then
                    If (Op = '*') Then
                       Result := vtNumeric
                    Else
                       If (Op = '//') Then
                           Result := vtMatrix
                       Else
                           Result := vtVector
                  Else
                    Result := vtNull;

      vtMatrix : If (Op = '*' ) or (Op = '@' ) or (Op = '@/' ) or (Op = '||') or
                    (Op = '//')
                 Then
                   If (Op = '*' ) Then
                      Result := vtVector
                   Else
                      Result := vtMatrix
                 Else
                   Result := vtNull;
    End;

    vtMatrix : Case T2 Of
      vtNumeric : If (Op = '+' ) or (Op = '-' ) or (Op = '/' ) or (Op = '#' ) or
                     (Op = '*' ) or (Op = '@' ) or (Op = '##') or (Op = '<>') or
                     (Op = '><') or (Op = '&' ) or (Op = '|' ) or (Op = '=' ) or
                     (Op = '<' ) or (Op = '<=') or (Op = '>' ) or (Op = '>=') or
                     (Op = '^=') or (Op = '**') or (Op = '@/') or (Op = '//') or
                     (Op = '||')
                  Then Result := vtMatrix
                  Else Result := vtNull;

      vtVector  : If (Op = '*') or (Op = '@') or (Op = '@/') or (Op = '||') or (Op = '//')
                  Then
                    If (Op = '*' ) Then
                       Result := vtVector
                    Else
                       Result := vtMatrix
                  Else
                    Result := vtNull;

      vtMatrix : If (Op = '+' ) or (Op = '-' ) or (Op = '/' ) or (Op = '#' ) or
                    (Op = '##') or (Op = '<>') or (Op = '><') or (Op = '&' ) or
                    (Op = '|' ) or (Op = '=' ) or (Op = '<' ) or (Op = '<=') or
                    (Op = '>' ) or (Op = '>=') or (Op = '^=') or (Op = '*' ) or
                    (Op = '@' ) or (Op = '@/') or (Op = '//') or (Op = '||')
                 Then Result := vtMatrix
                 Else Result := vtNull;
    End;
  End; {Case}
  If (T1 = vtNull) or (T2 = vtNull) Then Result := vtNull ;
End; {Combinam}

Function TAvaliator.PegaTipoFunc(Indice : TwsEnumConstFun; VarType: TwsEnumVarType): TwsEnumVarType;
Begin
  Case VarType Of
    vtNumeric  : Case Indice Of
                    cABS..cFRAC:          Result := vtNumeric;
                    cTANH,cTGAMA:         Result := vtNumeric;
                    cIDENT..cEXPER :      Result := vtMatrix;
                    cALOC,cHILBERT,
                    cINDIC,cMEAN,
                    cPERFIL        :      Result := vtMatrix;
                    cVALUNIF,cVNORM:      Result := vtVector;
                    else                  Result := vtNull;
                 End;

    vtVector   : Case Indice Of
                    cABS..cFRAC,
                    cPOSTOEMP, cQSORT, cACUM,
                    cTGAMA                   :   Result := vtVector;
                    cABSNORM..cSQUAD, cENORM :   Result := vtNumeric;
                    cTANH,cQNORM             :   Result := vtVector;
                    cPORTOG, cVECDIAG        :   Result := vtMatrix;
                    cTODOS, cALGUM           :   Result := vtNumeric;
                    cPOSTO                   :   Result := vtVector;
                    cMax, cMIN               :   Result := vtNumeric;
                    else                         Result := vtNull;
                 End;

    vtMatrix   : Case Indice Of
                    cABS..cFRAC,
                    cQSORT,
                    cACUM..cAAt,
                    cEchelon..cQProj: Result := vtMatrix;
                    cTANH           : Result := vtMatrix;
                    cDIAGVEC        : Result := vtVector;
                    cTODOS, cALGUM  : Result := vtNumeric;
                    cTRACO          : Result := vtNumeric;
                    cAbsNorm        : Result := vtNumeric;
                    cENorm          : Result := vtNumeric;
                    cMedia          : Result := vtNumeric;
                    cSoma           : Result := vtNumeric;
                    cSquad          : Result := vtNumeric;
                    cPOSTOEMP       : Result := vtMatrix;
                    cPOSTO          : Result := vtMatrix;
                    cGINV,cTGAMA    : Result := vtMatrix;
                    cMax,cMIN       : Result := vtNumeric;
                    cChol,
                    cG2Inv,
                    cHermite        : Result := vtMatrix;
                    else
                      Result := vtNull;
                 End; // case
  End;
End; {PegaTipoFunc}

  {------------------------------------------------------------------------}
  {------------- INICIO DAS ROTINAS QUE REALIZAM AS OPERAÇÕES -------------}
  {------------------------------------------------------------------------}

Procedure TAvaliator.Opera(Simb: TStr2; op1, op2: PTemp; Var Res: PTemp; Var Erro: Word);
Var Estado : Byte;
Begin
  Erro := 0;
  Case Op1^.VarType Of
    vtNumeric : Case Op2^.VarType Of
                  vtNumeric : Estado := 1;
                  vtMatrix  : Estado := 2;
                  vtVector  : Estado := 3;
                  End;

    vtMatrix  : Case Op2^.VarType Of
                  vtNumeric : Estado := 4;
                  vtMatrix  : Estado := 5;
                  vtVector  : Estado := 6;
                  End;

    vtVector   : Case Op2^.VarType Of
                   vtNumeric : Estado := 7;
                   vtMatrix  : Estado := 8;
                   vtVector  : Estado := 9;
                   End;
  End; {Case}

  Case Estado Of
    1: {  Somente operações ESCALAR X ESCALAR  }
    Begin
       If  Simb = '+'   Then Res^.Value := ScalarSum  (op1^.Value,op2^.Value)      Else
       If  Simb = '-'   Then Res^.Value := ScalarSub  (op1^.Value,op2^.Value)      Else
       If (Simb = '*')  Or
          (Simb = '#')  Or
          (Simb = '@')  Or
          (Simb = '@/') Then Res^.Value := ScalarProd (op1^.Value,op2^.Value)      Else
       If  Simb = '/'   Then Res^.Value := ScalarDiv  (op1^.Value,op2^.Value)      Else
       If  Simb = '##'  Then Res^.Value := ScalarPower(op1^.Value,op2^.Value)      Else
       If  Simb = '<>'  Then Res^.Value := ScalarMax  (op1^.Value,op2^.Value)      Else
       If  Simb = '><'  Then Res^.Value := ScalarMin  (op1^.Value,op2^.Value)      Else
       If  Simb = '&'   Then Res^.Value := ScalarAnd  (op1^.Value,op2^.Value)      Else
       If  Simb = '|'   Then Res^.Value := ScalarOr   (op1^.Value,op2^.Value)      Else
       If  Simb = '='   Then Res^.Value := ScalarEQ   (op1^.Value,op2^.Value)      Else
       If  Simb = '<'   Then Res^.Value := ScalarLT   (op1^.Value,op2^.Value)      Else
       If  Simb = '<='  Then Res^.Value := ScalarLE   (op1^.Value,op2^.Value)      Else
       If  Simb = '>'   Then Res^.Value := ScalarGT   (op1^.Value,op2^.Value)      Else
       If  Simb = '>='  Then Res^.Value := ScalarGE   (op1^.Value,op2^.Value)      Else
       If  Simb = '^='  Then Res^.Value := ScalarNE   (op1^.Value,op2^.Value)      Else
       If  Simb = '**'  Then
         If (op2^.Value <> -1)
            Then Res^.Value := ScalarPower(op1^.Value, op2^.Value)
            Else Res^.Value := ScalarDiv(1, op1^.Value)        {<<<<<<<<< 02/05/1997}
       Else
       If  (Simb = '//') OR (Simb = '||') Then
           Begin
           Res^.Ender := ScalarAppend(op1^.Value,op2^.Value);
           Res^.VarType := vtVector;
           Exit;
           End
       Else;

       If Erro = 0 Then Res^.VarType := vtNumeric
                   Else Res^.VarType := vtNull;
    End;

    {-----------------------------------------------------------------------------------------}
    2: {  Somente operações ESCALAR X MATRIZ  }
       { Operador ** nao tem sentido }
       Begin
       If  Simb = '+' Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpSum,True,True) Else
       If  Simb = '-' Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpSub,True,True) Else
       If  Simb = '/'  Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpDiv,True,True) Else
       If (Simb = '#' ) Or
          (Simb = '@' ) Or
          (Simb = '@/') Or
          (Simb = '*' ) Then
            Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpProd,True,True) Else
       If (Simb = '##') Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpPower,True,True) Else
       If  Simb = '<>'  Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpMax,True,True) Else
       If  Simb = '><'  Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpMin,True,True) Else
       If  Simb = '&'   Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpAnd,True,True) Else
       If  Simb = '|'   Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpOR,True,True) Else
       If  Simb = '='   Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpEQ,True,True) Else
       If  Simb = '<'   Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpLT,True,True) Else
       If  Simb = '<='  Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpLE,True,True) Else
       If  Simb = '>'   Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpGT,True,True) Else
       If  Simb = '>='  Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpGE,True,True) Else
       If  Simb = '^='  Then
         Res^.Ender := TwsMatrix(op2^.Ender).ByScalar(op1^.Value,OpNE,True,True) Else
       if  Simb = '//'  Then
         Res^.Ender := TwsMatrix(Op2^.Ender).RowScalarConcat(Op1^.Value,True) Else {15/10/97}
       if  Simb = '||'  Then
         Res^.Ender := TwsMatrix(Op2^.Ender).ColScalarConcat(Op1^.Value,True) Else; {15/10/97}

       If Erro = 0 Then Res^.VarType := vtMatrix
                   Else Res^.VarType := vtNull;
    End;

    {-----------------------------------------------------------------------------------------}
    3: {  Somente operações ESCALAR X VETOR   }
    Begin
       If  Simb = '+'   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opSum,True,True) Else
       If  Simb = '-'   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opSub,True,True) Else
       If  Simb = '/'   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opDiv,True,True) Else

       If (Simb = '*' ) Or
          (Simb = '#' ) Or
          (Simb = '@' ) Or
          (Simb = '@/') Then
            Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opProd,True,True) Else

       If (Simb = '##') Then
            Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opPower,True,True) Else

       If  Simb = '<>'  Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opMax,True,True) Else
       If  Simb = '><'  Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opMin,True,True) Else
       If  Simb = '&'   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opAnd,True,True) Else
       If  Simb = '|'   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opOR,True,True) Else
       If  Simb = '='   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opEQ,True,True) Else
       If  Simb = '<'   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opLT,True,True) Else
       If  Simb = '<='  Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opLE,True,True) Else
       If  Simb = '>'   Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opGT,True,True) Else
       If  Simb = '>='  Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opGE,True,True) Else
       If  Simb = '^='  Then
         Res^.Ender := TwsVec(op2^.Ender).ByScalar(op1^.Value,opNE,True,True) Else
       If  (Simb = '//')  Or
           (Simb = '||')  Then Res^.Ender := VScalarAppend (op2^.Ender, Op1^.Value, True)    Else;

       If Erro = 0 Then Res^.VarType := vtVector
                   Else Res^.VarType := vtNull;
    End;

    {-----------------------------------------------------------------------------------------}
    4: {  Somente operações MATRIZ X ESCALAR  }
    Begin
       If  Simb = '+' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpSum,False,True) Else
       If  Simb = '-' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpSub,False,True) Else
       If  Simb = '/' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpDiv,False,True) Else
       If (Simb = '#' )  Or
          (Simb = '*' )  Or
          (Simb = '@' )  Or
          (Simb = '@/')  Then
            Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpProd,False,True) Else
       If  Simb = '##'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpPower,False,True) Else
       If  Simb = '<>'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpMax,False,True) Else
       If  Simb = '><'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpMin,False,True) Else
       If  Simb = '&'   Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpAnd,False,True) Else
       If  Simb = '|'   Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpOR,False,True) Else
       If  Simb = '='   Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpEQ,False,True) Else
       If  Simb = '<'   Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpLT,False,True) Else
       If  Simb = '<='  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpLE,False,True) Else
       If  Simb = '>'   Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpGT,False,True) Else
       If  Simb = '>='  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpGE,False,True) Else
       If  Simb = '^='  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByScalar(op2^.Value,OpNE,False,True) Else
       if  Simb = '//'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).RowScalarConcat(Op2^.Value,False) Else {15/10/97}
       if  Simb = '||'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).RowScalarConcat(Op2^.Value,False) Else {15/10/97}
       If  Simb = '**'  Then
          If op2^.Value = -1
             Then Res^.Ender := TwsMatrix(op1^.Ender).Func(cInv,True)
             Else Res^.Ender := MatPower(op1^.Ender,Trunc(op2^.Value),Erro)         Else;

       If Erro = 0 Then Res^.VarType := vtMatrix
                   Else Res^.VarType := vtNull;
    End;

    {-----------------------------------------------------------------------------------------}
    5: {  Somente operações MATRIZ X MATRIZ   }
       Begin
       If Simb = '+'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opSum,True,Erro) Else
       If Simb = '-'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opSub,True,Erro) Else
       If Simb = '/'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opDiv,True,Erro) Else
       If Simb = '*'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).Mult(op2^.Ender,Erro) Else
       If Simb = '#'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opProd,True,Erro) Else
       If Simb = '##' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opPower,True,Erro) Else
       If Simb = '<>' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opMax,True,Erro) Else
       If Simb = '><' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opMin,True,Erro) Else
       If Simb = '&'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opAnd,True,Erro) Else
       If Simb = '|'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opOR,True,Erro) Else
       If Simb = '='  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opEQ,True,Erro) Else
       If Simb = '<'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opLT,True,Erro) Else
       If Simb = '<=' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opLE,True,Erro) Else
       If Simb = '>'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opGT,True,Erro) Else
       If Simb = '>=' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opGE,True,Erro) Else
       If Simb = '^=' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ByElement(op2^.Ender,opNE,True,Erro) Else
       If Simb = '@'  Then
         Res^.Ender := TwsMatrix(op1^.Ender).Kronecker(op2^.Ender) Else
       If Simb = '@/' Then
         Res^.Ender := TwsMatrix(op1^.Ender).HorKronecker(op2^.Ender,Erro) Else
       If Simb = '//' Then
         Res^.Ender := TwsMatrix(op1^.Ender).RowConcat(op2^.Ender,Erro) Else
       If Simb = '||' Then
         Res^.Ender := TwsMatrix(op1^.Ender).ColConcat(op2^.Ender,Erro) Else {...} ;

       If Erro = 0 Then
         Res^.VarType := vtMatrix
       Else
         begin
         Res^.VarType := vtNull;
         MessageDlg('Ocorreu um erro durante a operação', mtInformation, [mbOk], 0)
         end
       End;

    {-----------------------------------------------------------------------------------------}
    6: {  Somente operações MATRIZ X VETOR    }
      Begin
      If Simb = '*'  Then
         Begin
         Res^.Ender := TwsMatrix(op1^.Ender).VecMult(op2^.Ender,False,Erro);
         If Erro = 0 Then Res^.VarType := vtVector Else Res^.VarType := vtNull;
         Exit;
         End
      Else
      If (Simb = '@' ) OR
         (Simb = '@/') Then
           Res^.Ender := TwsMatrix(op1^.Ender).VecKronecker(op2^.Ender) Else
      If Simb = '//'   Then
        Res^.Ender := TwsMatrix(Op1^.Ender).RowVecConcat(op2^.Ender,True,False,Erro) Else
      If Simb = '||'   Then
        Res^.Ender := TwsMatrix(Op1^.Ender).ColVecConcat(op2^.Ender,False,Erro) Else {...};
      If Erro = 0 Then Res^.VarType := vtMatrix
                  Else Res^.VarType := vtNull;
      End;

    {-----------------------------------------------------------------------------------------}
    7: {  Somente operações VETOR X ESCALAR   }
       Begin
       If  Simb = '+'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opSum,True,False) Else
       If  Simb = '-'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opSub,True,False) Else
       If  Simb = '/'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opDiv,True,False) Else
       If (Simb = '#' ) Or (Simb = '*' ) Or (Simb = '@' ) Or (Simb = '@/') Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opProd,True,False) Else
       If  Simb = '##' Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opPower,True,False) Else
       If  Simb = '<>' Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opMax,True,False) Else
       If  Simb = '><' Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opMin,True,False) Else
       If  Simb = '&'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opAnd,True,False) Else
       If  Simb = '|'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opOR,True,False) Else
       If  Simb = '='  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opEQ,True,False) Else
       If  Simb = '<'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opLT,True,False) Else
       If  Simb = '<=' Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opLE,True,False) Else
       If  Simb = '>'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opGT,True,False) Else
       If  Simb = '>=' Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opGE,True,False) Else
       If  Simb = '^=' Then
         Res^.Ender := TwsVec(op1^.Ender).ByScalar(op2^.Value,opNE,True,False) Else
       If  (Simb = '//') Or (Simb = '||')  Then
         Res^.Ender := VScalarAppend (op1^.Ender, Op2^.Value, False) Else;

       If Erro = 0 Then Res^.VarType := vtVector
                   Else Res^.VarType := vtNull;
       End;

    {-----------------------------------------------------------------------------------------}
    8: {  Somente operações VETOR X MATRIZ    }
      Begin
      If Simb = '*'  Then
         begin
         Res^.Ender := TwsMatrix(op2^.ender).VecMult(op1^.Ender,True,Erro);
         Res^.VarType := vtVector;
         Exit;
         end
      Else
      If (Simb = '@' ) OR (Simb = '@/') Then
        Res^.Ender := TwsMatrix(op2^.Ender).VecKronecker(op1^.ender) Else
      If Simb = '//'   Then
        Res^.Ender := TwsMatrix(op2^.Ender).RowVecConcat(op1^.Ender,True,True,Erro) Else
      If Simb = '||'   Then
        Res^.Ender := TwsMatrix(op2^.Ender).ColVecConcat(op1^.Ender,True,Erro) Else {...};

      If Erro = 0 Then Res^.VarType := vtMatrix
                  Else Res^.VarType := vtNull;
      End;

    {-----------------------------------------------------------------------------------------}
    9: {  Somente operações VETOR X VETOR     }
       Begin

       If  Simb = '+' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpSum,True,Erro) Else
       If  Simb = '-' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpSub,True,Erro) Else
       If  Simb = '/' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpDiv,True,Erro) Else
       If  Simb = '*'   Then
           Begin
           Res^.Value := VecMult(op1^.Ender, op2^.Ender, Erro);
           If Erro = 0 Then Res^.VarType := vtNumeric
                       Else Res^.VarType := vtNull;
           Exit;
           End
       Else
       If  Simb = '#' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpProd,True,Erro) Else
       If  Simb = '##' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpPower,True,Erro) Else
       If  Simb = '<>' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpMax,True,Erro) Else
       If  Simb = '><' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpMin,True,Erro) Else
       If  Simb = '&'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpAnd,True,Erro) Else
       If  Simb = '|'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpOR,True,Erro) Else
       If  Simb = '='  Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpEQ,True,Erro) Else
       If  Simb = '<'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpLT,True,Erro) Else
       If  Simb = '<=' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpLE,True,Erro) Else
       If  Simb = '>'  Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpGT,True,Erro) Else
       If  Simb = '>=' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpGE,True,Erro) Else
       If  Simb = '^=' Then
         Res^.Ender := TwsVec(op1^.Ender).ByElement(op2^.Ender,OpNE,True,Erro) Else

       If (Simb = '@' ) Or
          (Simb = '@/') Then Res^.Ender := TwsVec(op1^.Ender).Kronecker(op2^.Ender) Else

       If (Simb = '//') Then
          begin
          Res^.Ender := HVecVecConcat(op1^.Ender, op2^.Ender,Erro);
          Res^.VarType := vtMatrix;
          Exit;
          end
       Else { Alex 09/10/97 }

       If (Simb = '||') Then
          Res^.Ender := VecAppend (TwsVec(op1^.Ender), op2^.Ender, True,
                                   TWSVec(op2^.Ender).Len)
       Else {...};

       If Erro = 0 Then Res^.VarType := vtVector
                   Else Res^.VarType := vtNull;
    End;
  End; {Case Estado}
end;{Opera}

Function TAvaliator.OperaUnario(Simb : TStr2; op1 : PTemp): PTemp;
var Erro: Word;
Begin
  Erro := 0;
  New(Result);
  Case op1^.VarType Of
    vtNumeric:
      Begin
      If Simb = '-' Then Result^.Value := ScalarProd(-1,op1^.Value) Else
      If Simb = '^' Then Result^.Value := ScalarEQ (op1^.Value, 0)  Else {...};

      If Erro = 0 Then Result^.VarType := vtNumeric
                  Else Result^.VarType := vtNull;
      End;

    vtMatrix:
      Begin
      If Simb = '-' Then
        Result^.Ender := TwsMatrix(op1^.Ender).ByScalar(-1,OpProd,True,True) Else
      If Simb = '^' Then
        Result^.Ender := TwsMatrix(op1^.Ender).ByScalar(0,OpEQ,True,True) Else {...};
      If Simb = '`' Then
        Result^.Ender := TwsMatrix(op1^.Ender).Transpose Else {...};

      If Erro = 0 Then Result^.VarType := vtMatrix
                  Else Result^.VarType := vtNull;
      End;

    vtVector:
      Begin
      If Simb = '-' Then
        Result^.Ender := TwsVec(op1^.Ender).ByScalar(-1,OpProd,True,True) Else
      If Simb = '^' Then
        Result^.Ender := TwsVec(op1^.Ender).ByScalar(0,OpEQ,True,True) Else {...};

      If Erro = 0 Then Result^.VarType := vtVector
                  Else Result^.VarType := vtNull;
      End;
    End; { Case }
End;{OperaUnario}

Procedure TAvaliator.OperaFuncao(Ind: TwsEnumConstFun; op1: PTemp; Var Res: PTemp; Var Erro: Word);
{
Função:

Entradas:
Ind  : TwsEnumConstFun - Indice ou identificador da função. Ex: cAll.
Op1  : PTemp     - Operando (ou argumento).
Res  : PTemp     - Aponta para o Objeto onde será retornado o resultado.
Erro : Word      - Retorna 0 (zero) se tudo ok ou o número indicando o erro.
}
Var
//  x     : Longint;
  inutil: word;
  lixo,x: integer;
Begin
  Erro := 0;
  Case Op1^.VarType of
    vtNumeric : {Somente funções que operam com escalares}
      Begin
      Case ind of
        cABS:      Res^.Value := ScalarAbs    (Op1^.Value);
        cEXP:      Res^.Value := ScalarExp    (Op1^.Value);
        cAPROXIMA: Res^.Value := ScalarFuzz   (Op1^.Value);
        cINT:      Res^.Value := ScalarInt    (Op1^.Value);
        cLN:       Res^.Value := ScalarLn     (Op1^.Value);
        cRAIZ:     Res^.Value := ScalarSqrt   (Op1^.Value);
        cARCTAN:   Res^.Value := ScalarArcTan (Op1^.Value);
        cARCSEN:   Res^.Value := ScalarArcSin (Op1^.Value);
        cARCCOS:   Res^.Value := ScalarArcCos (Op1^.Value);
        cSEN:      Res^.Value := ScalarSin    (Op1^.Value);
        cCOS:      Res^.Value := ScalarCos    (Op1^.Value);
        cSENH:     Res^.Value := ScalarSinH   (Op1^.Value);
        cCOSH:     Res^.Value := ScalarCosH   (Op1^.Value);
        cTAN:      Res^.Value := ScalarTan    (Op1^.Value);
        cLOG:      Res^.Value := ScalarLog    (Op1^.Value);
        cANG:      Res^.Value := ScalarAng    (Op1^.Value);
        cLGAMA:    Res^.Value := ScalarLnGamma(Op1^.Value);
        cTGAMA:    Res^.Value := ScalarTriGamma(Op1^.Value);
        cFLOOR:    Res^.Value := ScalarFloor  (Op1^.Value);
        cCEIL:     Res^.Value := ScalarCeil   (Op1^.Value);
        cINV:      Res^.Value := ScalarInv    (Op1^.Value);
        cFRAC:     Res^.Value := ScalarFrac   (Op1^.Value);
        cTANH:     Res^.Value := ScalarTanH   (Op1^.Value);

        cVALUNIF:  begin
                   Res^.Ender := TwsVec(UnifValues(Trunc(Op1^.Value),1000,1));
                   Res^.VarType := vtVector;
                   Exit;
                   End;

        cALOC:     begin
                   Res^.Ender := TwsGeneral(TreatRand(Trunc(Op1^.Value)));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cHILBERT:  begin
                   Res^.Ender := TwsGeneral(Hilbert(Trunc(Op1^.Value)));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cVNORM:    begin
                   Res^.Ender := TwsVec(ValNorm(Trunc(Op1^.Value)));
                   Res^.VarType := vtVector;
                   Exit;
                   End;

        cIDENT:    Begin
                   Res^.Ender := Identity(Trunc(Op1^.Value));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cHELMERT:  Begin
                   Res^.Ender := LHelmert(Trunc(Op1^.Value));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cIndic:  Begin
                   Res^.Ender := LIndic(Trunc(Op1^.Value));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cPERFIL:  Begin
                   Res^.Ender := LProfile(Trunc(Op1^.Value));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cMean:     Begin
                   Res^.Ender := LMeanTransf(Trunc(Op1^.Value));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cCONTROLE: Begin
                   x := Trunc(Op1^.Value);
                   Res^.Ender := LControl(x);    {Cuidado com os erros}
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;

        cEXPER:    Begin
                   Res^.Ender := Experim(Trunc(Op1^.Value));
                   Res^.VarType := vtMatrix;
                   Exit;
                   End;
      End;{Case}

      If Erro = 0 Then Res^.VarType := vtNumeric
                  Else Res^.VarType := vtNull;
    End; {Escalar}

    vtMatrix : {Somente funções que trabalham com matrizes}
    Begin
      {Cuidado com o retorno de tipo}
      Case ind of
        cABS:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cAbs,True);
        cEXP:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cExp,True);
        cAPROXIMA: Res^.Ender := TwsMatrix(Op1^.Ender).Func(cAproxima,True);
        cINT:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cInt,True);
        cLN:       Res^.Ender := TwsMatrix(Op1^.Ender).Func(cLn,True);
        cRAIZ:     Res^.Ender := TwsMatrix(Op1^.Ender).Func(cRaiz,True);
        cARCTAN:   Res^.Ender := TwsMatrix(Op1^.Ender).Func(cArcTan,True);
        cARCSEN:   Res^.Ender := TwsMatrix(Op1^.Ender).Func(cArcSen,True);
        cARCCOS:   Res^.Ender := TwsMatrix(Op1^.Ender).Func(cArcCos,True);
        cSEN:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cSen,True);
        cCOS:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cCos,True);
        cSENH:     Res^.Ender := TwsMatrix(Op1^.Ender).Func(cSenH,True);
        cCOSH:     Res^.Ender := TwsMatrix(Op1^.Ender).Func(cCosH,True);
        cTAN:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cTan,True);
        cLOG:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cLog,True);
        cANG:      Res^.Ender := TwsMatrix(Op1^.Ender).Func(cAng,True);
        cLGAMA:    Res^.Ender := TwsMatrix(Op1^.Ender).Func(cLGama,True);
        cTGAMA:    Res^.Ender := TwsMatrix(Op1^.Ender).Func(cTGama,True);
        cFLOOR:    Res^.Ender := TwsMatrix(Op1^.Ender).Func(cFloor,True);
        cCEIL:     Res^.Ender := TwsMatrix(Op1^.Ender).Func(cCeil,True);
        cINV:      begin
                   TwsMatrix(Op1^.Ender).Copy(TwsMatrix(Op1^.Ender).MatType,TwsMatrix(Res^.Ender));
                   TwsMatrix(Res^.Ender).Inv(Erro);
                   end;
        cG2Inv:    Res.Ender:=TwsSymmetric(Op1^.Ender).G2Inv;
        cHermite:  Res.Ender:=TwsSymmetric(Op1^.Ender).Hermite;
        cFRAC:     Res^.Ender := TwsMatrix(Op1^.Ender).Func(cFrac,True);
        cABSNORM:  Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cAbsNorm,x);
        cMEDIA:    Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cMedia,x);
        cSOMA:     Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cSoma,x);
        cSQUAD:    Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cSQuad,x);
        cTANH:     Res^.Ender := TwsMatrix(Op1^.Ender).Func(cTanH,True);
        cPOSTOEMP: Res^.Ender := TwsMatrix(Op1^.Ender).RankTie(True);
        cENORM:    Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cENorm,x);
        cACUM:     Res^.Ender := TwsMatrix(Op1^.Ender).Func(cAcum,True);
        cT:        Res^.Ender := TwsMatrix(Op1^.Ender).Transpose;
        cAtA:      Res^.Ender := TwsMatrix(Op1^.Ender).TranspMul4;
        cAAt:      Res^.Ender := TwsMatrix(Op1^.Ender).TranspMul3;
        cDIAGVEC:  Begin
                   Res^.Ender := TwsMatrix(Op1^.Ender).DiagToVec;
                   Res^.VarType := vtVector;
                   Exit;
                   End;
        cTODOS  :  Begin
                   if TwsMatrix(Op1^.Ender).All(0,opEQ) then
                     Res^.Value := ScalarTrue
                   else
                     Res^.Value := ScalarFalse;
                   Res^.VarType := vtNumeric;
                   Exit;
                   End;
        cALGUM  :  Begin
                   if TwsMatrix(Op1^.Ender).Any(0,opEQ) then
                     Res^.Value := ScalarTrue
                   else
                     Res^.Value := ScalarFalse;
                   Res^.VarType := vtNumeric;
(*                   Res^.VarType := vtNull; *)
                   Exit;
                   End;
        cTRACO  :  Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cTraco,x);
        cPOSTO  :  Res^.Ender := TwsMatrix(Op1^.Ender).Rank(True, True);
        cGINV   :  Res^.Ender := TwsGeneral(Op1^.Ender).MoorePenrose;
        cMax    :  Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cMax,x);
        cMIN    :  Res^.Value := TwsMatrix(Op1^.Ender).ScalarFunc(cMin,x);
        cQSORT  :  Res^.Ender := TwsMatrix(Op1^.Ender).QSort;
        cEchelon:  Res^.Ender:=TwsGeneral(Op1^.Ender).Echelon(True);
        cGS     :  Res^.Ender:=TwsGeneral(Op1^.Ender).GSBase;
        cMGS    :  Res^.Ender:=TwsGeneral(Op1^.Ender).MGSBase(True);
        cHProj  :  begin
                   Res^.Ender:=TwsGeneral(Op1^.Ender).HProjX;
                   Res^.VarType:=vtMatrix;
                   Res^.Espec:=mtSymmetric;
                   Exit;
                   end;
        cQProj  :  begin
                   Res^.Ender:=TwsGeneral(Op1^.Ender).QProjX;
                   Res^.VarType := vtMatrix;
                   Res^.Espec:= mtSymmetric;
                   Exit;
                   end;
        cCHOL   :  begin {Verificar}
                   Res^.Ender := TwsSymmetric(Op1^.Ender).CholeskyFat(Lixo, True);
                   if Lixo = TwsSymmetric(Op1^.Ender).NCols then
                      begin
                      Res^.VarType := vtMatrix;
                      Res^.Espec:= mtTriangular;
                      Exit;
                      end
                   else
                       Res^.VarType := vtNull;
                   exit;

                   end;
      End;{Case}

      If Erro = 0 Then
        Res^.VarType := vtMatrix
      Else
        Res^.VarType := vtNull;
    End;

    vtVector :  {Somente funções que trabalham com vetores}
    Begin
      Case ind of
        cABS:      Res^.Ender := TwsVec(Op1^.Ender).Func(cAbs,True);
        cEXP:      Res^.Ender := TwsVec(Op1^.Ender).Func(cExp,True);
        cAPROXIMA: Res^.Ender := TwsVec(Op1^.Ender).Func(cAproxima,True);
        cINT:      Res^.Ender := TwsVec(Op1^.Ender).Func(cInt,True);
        cLN:       Res^.Ender := TwsVec(Op1^.Ender).Func(cLn,True);
        cRAIZ:     Res^.Ender := TwsVec(Op1^.Ender).Func(cRaiz,True);
        cARCTAN:   Res^.Ender := TwsVec(Op1^.Ender).Func(cArcTan,True);
        cARCSEN:   Res^.Ender := TwsVec(Op1^.Ender).Func(cArcSen,True);
        cARCCOS:   Res^.Ender := TwsVec(Op1^.Ender).Func(cArcCos,True);
        cSEN:      Res^.Ender := TwsVec(Op1^.Ender).Func(cSen,True);
        cCOS:      Res^.Ender := TwsVec(Op1^.Ender).Func(cCos,True);
        cSENH:     Res^.Ender := TwsVec(Op1^.Ender).Func(cSenH,True);
        cCOSH:     Res^.Ender := TwsVec(Op1^.Ender).Func(cCosH,True);
        cTAN:      Res^.Ender := TwsVec(Op1^.Ender).Func(cTan,True);
        cLOG:      Res^.Ender := TwsVec(Op1^.Ender).Func(cLog,True);
        cANG:      Res^.Ender := TwsVec(Op1^.Ender).Func(cAng,True);
        cLGAMA:    Res^.Ender := TwsVec(Op1^.Ender).Func(cLGama,True);
        cTGAMA:    Res^.Ender := TwsVec(Op1^.Ender).Func(cTGama,True);
        cFLOOR:    Res^.Ender := TwsVec(Op1^.Ender).Func(cFloor,True);
        cCEIL:     Res^.Ender := TwsVec(Op1^.Ender).Func(cCeil,True);
        cINV:      Res^.Ender := TwsVec(Op1^.Ender).Func(cInv,True);
        cFRAC:     Res^.Ender := TwsVec(Op1^.Ender).Func(cFrac,True);
        cABSNORM:  Res^.Value := TwsVec(Op1^.Ender).AbsoluteNorm(x);
        cMEDIA:    Res^.Value := TwsVec(Op1^.Ender).Mean(x);
        cSOMA:     Res^.Value := TwsVec(Op1^.Ender).Total(x);
        cSQUAD:    Res^.Value := TwsVec(Op1^.Ender).SumOfSq(x);
        cTANH:     Res^.Ender := TwsVec(Op1^.Ender).Func(cTanH,True);
        cPOSTOEMP: Res^.Ender := TwsVec(Op1^.Ender).RankTie;
        cQSORT:    Res^.Ender := TwsVec(Op1^.Ender).Sort(True,True);
        cENORM:    Res^.Value := TwsVec(Op1^.Ender).EuclideanNorm(x);
        cACUM:     Res^.Ender := TwsVec(Op1^.Ender).Accum(True);

        cPORTOG:   Begin
                   Res^.Ender := LPolOrth(Op1^.Ender, TWSVec(Op1^.Ender).Len - 1);
                   Res^.VarType  := vtMatrix;
                   Exit;
                   End;

        cQNORM:    Res^.Ender := QNorm(Op1^.Ender);

        cVECDIAG:  Begin
                   Res^.Ender := VecToDiag(Op1^.Ender);
                   If Erro = 0 Then
                      Begin
                      Res^.VarType  := vtMatrix;
                      Res^.Espec := mtDiagonal;
                      Exit;
                      End
                   Else
                      Res^.VarType := vtNull;
                   Exit;
                   End;

        cTODOS    :Begin
                   if TwsVec(Op1^.Ender).All(0,opEQ) then
                     Res^.Value := ScalarTrue
                   else
                     Res^.Value := ScalarFalse;
                   Res^.VarType := vtNumeric;
                   Exit;
                   End;

        cALGUM    :Begin
                   if TwsVec(Op1^.Ender).Any(0,opEQ) then
                     Res^.Value := ScalarTrue
                   else
                     Res^.Value := ScalarFalse;
                   Res^.VarType := vtNumeric;
                   Exit;
                   End;
        cPOSTO  :  Res^.Ender := TwsVec(Op1^.Ender).Rank(True, False);
        cMax    :  Res^.Value := TwsVec(Op1^.Ender).MinOrMax(False);
        cMIN    :  Res^.Value := TwsVec(Op1^.Ender).MinOrMax(True);

      End;{Case}

      Case ind Of
        cABSNORM, cMEDIA, cSOMA, cSQUAD, cENORM:
           If Erro = 0 Then Res^.VarType := vtNumeric
                       Else Res^.VarType := vtNull;

        Else
           If Erro = 0 Then Res^.VarType := vtVector
                       Else Res^.VarType := vtNull;
      End; {Case}
    End; {vtVector}
  End; {Case}
End; {OperaFuncao}


{--------------------------------------------------------------------------}
{------------------- A N Á L I S E   S I N T Á T I C A --------------------}
{--------------------------------------------------------------------------}

{Função que retornará True se não houve erro sintático e aproveitando a
 carona já monta a estrutura em forma de árvore binária.
 Foi usado nesta implementação o algoritmo Descendente Recursivo
 * A manipulação sobre a linguagem se encontra no cabeçalho da unit
}

Function TAvaliator.Percorre(RunType: TRunType; Var Erro: Word): Boolean;
Var
  oper1,oper2,p  : PTemp;    {Variáveis auxiliares da pilha}
  orelha         : Boolean;  {Variável para encher linguiça}
  VarType           : TwsEnumVarType; {Descubra idiota !!!}

  Function   E : Boolean; Forward;
  Function M_l : Boolean; Forward;
  Function L_l : Boolean; Forward;
  Function K_l : Boolean; Forward;
  Function I_l : Boolean; Forward;
  Function H_l : Boolean; Forward;
  Function F_l : Boolean; Forward;
  Function E_l : Boolean; Forward;

  Function T : Boolean;
  Var Salva, IndFunc : Integer;
  Begin
    Salva := IndPToken;
    Inc(IndPToken);
    Maior := PToken(ArrayPToken.Items[IndPToken])^.Pos;

    If PosError < Maior Then
       Begin
       PosError  := Maior;
       ErrorType := ENOperatorExpected;
       End;

    {-------  C S T   E   I D E N T I F I C A D O R E S  ------}

    {Separar Cst de Ident para aumentar velocidade 25/06/96}
    If (PToken(ArrayPToken.Items[IndPToken])^.TokenType = Cst) or
       (PToken(ArrayPToken.Items[IndPToken])^.TokenType = Ident) Then
       Begin
       Case RunType Of
         Sintatico  : {Nothing};

         Semantico  :
           If PToken(ArrayPToken.Items[indptoken])^.TokenType = Cst Then
              Begin
              New(p);
              p^.VarType := vtNumeric;
              Stack.Push(p);
              End
           Else
              Begin {Se forem variáveis}
              New(p);
              p^.VarType := PegaTipoIdent(IndPToken);
              Stack.Push(p);
              End; {If Cst}

         Avaliador  :
           Begin
           If PToken(ArrayPToken.Items[indptoken])^.TokenType = Cst Then
              Begin
              New(p);
              p^.VarType := vtNumeric;
              p^.Value := PConst(Tab_Cst.Items[PToken(ArrayPToken.Items[IndPToken])^.ind])^.Value;
              Stack.Push(p);
              End
           Else
              Begin {Se forem variáveis}
              VarType := PegaTipoIdent(IndPToken);
              If VarType <> vtNull Then
                 Begin

                 If VarType = vtNumeric Then
                    Begin
                    New(p);
                    p^.Value := TwsNumericVar(Tab_Sim.Vars[PToken(ArrayPToken.Items[IndPToken])^.ind]).AsFloat;
                    Stack.Push(p);
                    End {If VarType = vtNumeric}
                 Else
                 If VarType = vtMatrix Then
                    Begin
                    New(p);
                    {Pega o tipo especifico da Matriz. Ex Geral, Diagonal}
                    p^.Espec := TwsMatrixVar(Tab_Sim.Vars[PToken(ArrayPToken.Items[IndPToken])^.ind]).AsMatrix.MatType;;
                    p^.Ender := TwsMatrixVar(Tab_Sim.Vars[PToken(ArrayPToken.Items[IndPToken])^.ind]).AsMatrix;
                    Stack.Push(p);
                    End {If VarType = vtMatrix }
                 Else
                 If VarType = vtVector Then
                    Begin
                    New(p);
                    p^.Ender := TwsVectorVar(Tab_Sim.Vars[PToken(ArrayPToken.Items[IndPToken])^.ind]).AsVector;
                    Stack.Push(p);
                    End; {If VarType = vtVector }

                 p^.VarType := VarType;
                 End; {If VarType <> vtNull}
              End; {If Cst}
           End; {Case Avaliador}
         End; {Case}

      T := True;
      Exit;
      End; {If VarType = (Cst or Ident)}

    {---------------------  F U N Ç Õ E S  -----------------------}

    If PToken(ArrayPToken.Items[IndPToken])^.TokenType = Func Then
       Begin
       IndFunc := IndPToken;
       Inc(IndPToken);
       If PToken(ArrayPToken.Items[IndPToken])^.TokenType = Open Then
         If E Then
            Begin
            Inc(IndPToken);
            If PToken(ArrayPToken.Items[IndPToken])^.TokenType = Close Then
              Begin
              Case RunType Of
                Avaliador  :
                  Begin
                  New(p);
                  Oper1 := Stack.Pop;
                  OperaFuncao(TwsEnumConstFun(PToken(ArrayPToken.Items[IndFunc])^.Ind),
                                     Oper1, p, Erro);
                  Dispose(PTemp(Oper1));
                  Stack.Push(p);
                  End;

                Sintatico  : {Nothing};

                Semantico  :
                  Begin
                  New(p);
                  Oper1 := Stack.Pop;
                  p^.VarType := PegaTipoFunc(TwsEnumConstFun(PToken(ArrayPToken.Items[IndFunc])^.Ind),
                                PTemp(Oper1)^.VarType);
                  Dispose(PTemp(Oper1));
                  Stack.Push(p);
                  End;
                End; {Case}
              T := True ;
              Exit;
              End
           Else
              ErrorType := ENExpected_Close; { If Token = ) }
           End; {If E...}

       T := False;
       IndPToken := Salva;
       If ErrorType <> 0 Then
          ErrorType := ENExpected_Open; {05/11/1998} {<<<<<<<<<}
       Exit;
       End; {If TokenType = Func...}

    {----------------  P A R E N T E S E S  ----------------}

    If PToken(ArrayPToken.Items[IndPToken])^.TokenType = Open Then
      If E Then
         Begin
         Inc(IndPToken);
         If PToken(ArrayPToken.Items[IndPToken])^.TokenType = Close Then
            Begin
            T := True ;
            Exit;
            End
         Else
            ErrorType := ENExpected_Close;

         T := False;
         Maior := PToken(ArrayPToken.Items[IndPToken])^.Pos;
         If PosError < Maior Then PosError := Maior;
         IndPToken := Salva;
         Exit;
         End; {If E...}

    T := False;
    Maior := PToken(ArrayPToken.Items[IndPToken])^.Pos;
    If PosError < Maior Then PosError := Maior;
    ErrorType := ENIdentExpected;
    IndPToken := Salva;
  End; {T}

  Function O  : Boolean;
  Var Salva  : Integer;
  Begin
    Salva := IndPToken;
    Inc(IndPToken);
    With PToken(ArrayPToken.Items[IndPToken])^ do
      Begin
      If (Simb = '^') or (Simb = '-')  or (Simb = '`') Then   { Operações Unárias }
        If O Then
           begin
           Case RunType Of
             Avaliador  : Begin
                          New(p);
                          Oper1 := Stack.Pop;
                          p := OperaUnario(Simb,Oper1); { Roger em 27/08/96 }
                          Dispose(PTemp(Oper1));
                          Stack.Push(p);
                          End;
             Sintatico  : {Nothing} ;
             End;
           O := True;
           Exit;
           End; {If O ...}
      End; {With}

    IndPToken := Salva;
    If T Then
       Begin
       Inc(IndPToken);
       With PToken(ArrayPToken.Items[IndPToken])^ do
         Begin
         If (Simb = '##') or (Simb = '**') Then
           If O Then
             Begin
             Case RunType of
               Avaliador  :
                 Begin
                 New(p);
                 Oper2:=Stack.Pop;
                 Oper1:=Stack.Pop;
                 Opera(Simb, Oper1, OPer2, p, Word(Erro));
                 Stack.Push(p);
                 Dispose(PTemp(Oper1));
                 Dispose(PTemp(Oper2));
                 End;

               Sintatico  : {Nothing} ;

               Semantico  :
                 Begin
                 New(p);
                 Oper2 := Stack.Pop;
                 Oper1 := Stack.Pop;
                 p^.VarType := Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
                 Stack.Push(p);
                 Dispose(PTemp(Oper1));
                 Dispose(PTemp(Oper2));
                 End;
               End; {Case}
             O := True;
             Exit;
             End
           Else Begin End {If O...}
         Else Dec(IndPToken); {If Simb = '**' ...}  {Modificado 27/11/1996}
         End; {With}
       O := True;
       Exit;
       End; {If T...}
    IndPToken := Salva;
    O := False;
  End; {O}

{$B+} {Complete Boolean Eval = Ligado}
  Function M  : Boolean;
  Begin
    Result := O and M_l
  End;

  Function L  : Boolean;
  Begin
    Result := M and L_l
  End;

  Function K  : Boolean;
  Begin
    Result := L and K_l
  End;

  Function I  : Boolean;
  Begin
    Result := K and I_l
  End;

  Function H  : Boolean;
  Begin
    Result := I and H_l
  End;

  Function F  : Boolean;
  Begin
    Result := H and F_l
  End;

  Function E  : Boolean;
  Begin
    Result := F and E_l
  End;
{$B-}

  Function Exp  : Boolean;
  Begin                      {    Exp -> E;    }
    Result := False;
    If E Then
       Begin
       Inc(IndPToken);
       Result := (PToken(ArrayPToken.Items[IndPToken])^.Simb = ';');
       End;
  End;

  {----------------------------------}

  Function M_l  : Boolean;
  Var Er : Boolean;
      Salva : Integer;
  Begin
    Er := False;
    Salva := IndPToken;
    Inc(IndPToken);
    With PToken(ArrayPToken.Items[IndPToken])^ do
      If (Simb = '#' ) or (Simb = '<>') or (Simb = '><') or
         (Simb = '#/') or (Simb = '@/') or (Simb = '@' ) Then
         Begin
         Case RunType Of
            Avaliador  :
              Begin
              O;
              New(p);
              Oper2:=Stack.Pop;
              Oper1:=Stack.Pop;
              Opera(Simb, Oper1, OPer2, p, Word(Erro));
              Stack.Push(p);
              Dispose(PTemp(Oper1));
              Dispose(PTemp(Oper2));
              Orelha := M_l;
              Er := True
              End;
{$B+}
            Sintatico  :
              If O and M_l Then
                 Er := True
              Else
                 Begin
                 Er := False;
                 IndPToken := Salva;
                 End ;
{$B-}
            Semantico  :
              Begin
              O;
              New(p);
              Oper2 := Stack.Pop;
              Oper1 := Stack.Pop;
              p^.VarType := Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
              Stack.Push(p);
              Dispose(PTemp(Oper1));
              Dispose(PTemp(Oper2));
              Orelha := M_l;
              Er := True
              End;
            End; {Case}
         End
      Else
         Begin
         Er := True;
         IndPToken := Salva;
         End;

    Result := Er;
  End; {M_l}

  Function L_l  : Boolean;
  Var Er : Boolean;
      Salva : Integer;
  Begin
    Er := False;
    Salva := IndPToken;
    Inc(IndPToken);
    With PToken(ArrayPToken.Items[IndPToken])^ do
      If (Simb = '*') Or (Simb = '/') Then
        Begin
        Case RunType Of
          Avaliador  :
            Begin
            M;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            Opera(Simb, Oper1, OPer2, p, Word(Erro));
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=L_l;
            Er:=True;
            End;
{$B+}
          Sintatico  :
            If M and L_l Then
               Er := True
            Else
               Begin
               Er := False;
               IndPToken := Salva;
               End;
{$B-}
          Semantico  :
            Begin
            M;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            p^.VarType:=Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=L_l;
            Er:=True
            End;
          End; {Case}
        End
      Else
        Begin
        Er := True;
        IndPToken := Salva;
        End;
    Result := Er;
  End; {L_l}

  Function K_l  : Boolean;
  Var Er : Boolean;
      Salva : Integer;
  Begin
    Er := False;
    Salva := IndPToken;
    Inc(IndPToken);
    With Ptoken(ArrayPToken.Items[IndPToken])^ do
      If (Simb = '+') or (Simb = '-') Then
        Begin
        Er := True;
        Case RunType Of
          Avaliador  :
            Begin
            L;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            Opera(Simb, Oper1, OPer2, p, Word(Erro));
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=K_l;
            Er:=True
            End;
{$B+}
          Sintatico  :
            If L and K_l Then
               Er := True
            Else
               Begin
               Er := False;
               IndPToken := Salva ;
               End;
{$B-}
          Semantico  :
            Begin
            L;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            p^.VarType:=Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=K_l;
            Er:=True
            End;
          End; {Case}
        End
      Else
        Begin
        Er := True;
        IndPToken := Salva;
        End;
    Result := Er;
  End; {K_l}

  Function I_l  : Boolean;
  Var Er : Boolean;
      Salva : Integer;
  Begin
    Er := False;
    Salva := IndPToken;
    Inc(IndPToken);
    With PToken(ArrayPToken.Items[IndPToken])^ do
      If (Simb = '||') or (Simb = '//') Then
        Begin
        Er := True;
        Case RunType Of
          Avaliador  :
            Begin
            K;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            Opera(Simb, Oper1, OPer2, p, Word(Erro));
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=I_l;
            Er:=True
            End;
{$B+}
          Sintatico  :
            If K and I_l Then
               Er := True
            Else
               Begin
               Er := False;
               IndPToken := Salva ;
               End;
{$B-}
          Semantico  :
            Begin
            K;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            p^.VarType:=Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=I_l;
            Er:=True
            End;
          End; {Case}
        End
      Else
        Begin
        Er := True;
        IndPToken := Salva;
        End;
    Result := Er;
  End; {I_l}

 Function H_l  : Boolean;
  Var Er : Boolean;
      Salva : Integer;
  Begin
    Er := False;
    Salva := IndPToken;
    Inc(IndPToken);
    With PToken(ArrayPToken.Items[IndPToken])^ do
      Begin
      If (Simb = '<') or (Simb = '<=') or (Simb = '>') or (Simb = '>=') or
         (Simb = '=') or (Simb = '^=') Then Begin
          Case RunType Of
            Avaliador  :
              Begin
              I;
              New(p);
              Oper2:=Stack.Pop;
              Oper1:=Stack.Pop;
              Opera(Simb, Oper1, OPer2, p, Word(Erro));
              Stack.Push(p);
              Dispose(PTemp(Oper1));
              Dispose(PTemp(Oper2));
              Orelha:=H_l;
              Er:=True
              End;
{$B+}
            Sintatico  :
              If I and H_l Then
                 Er := True
              Else
                 Begin
                 Er := False;
                 IndPToken := Salva ;
                 End ;
{$B-}
            Semantico  :
              Begin
              I;
              New(p);
              Oper2:=Stack.Pop;
              Oper1:=Stack.Pop;
              p^.VarType:=Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
              Stack.Push(p);
              Dispose(PTemp(Oper1));
              Dispose(PTemp(Oper2));
              Orelha:=H_l;
              Er:=True
              End;
            End; {Case}
          End
        Else
          Begin
          Er := True;
          IndPToken := Salva;
          End;
      End; {With}
    Result := Er;
  End; {H_l}

  Function F_l  : Boolean;
  Var Er : Boolean;
      Salva : Integer;
  Begin
    Er := False;
    Salva := IndPToken;
    Inc(IndPToken);
    With PToken(ArrayPToken.Items[IndPToken])^ do
      If (Simb = '&') Then
        Begin
        Case RunType Of
          Avaliador  :
            Begin
            H;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            Opera(Simb, Oper1, OPer2, p, Word(Erro));
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=F_l;
            Er:=True
            End;
{$B+}
          Sintatico  :
            If H and F_l Then
               Er := True
            Else
               Begin
               Er := False;
               IndPToken := Salva ;
               End;
{$B-}
          Semantico  :
            Begin
            H;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            p^.VarType:=Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=F_l;
            Er:=True
            End;
          End; {With}
        End
      Else
        Begin
        Er := True;
        IndPToken := Salva;
        End;
    Result := Er;
  End; {F_l}

  Function E_l : Boolean;
  Var Er : Boolean;
      Salva : Integer;
  Begin
    Er := False;
    Salva := IndPToken;
    Inc(IndPToken);
    With PToken(ArrayPToken.Items[IndPToken])^ do
      If (Simb = '|') Then
        Begin
        Case RunType Of
          Avaliador  :
            Begin
            F;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            Opera(Simb, Oper1, OPer2, p, Word(Erro));
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=E_l;
            Er:=True
            End;
{$B+}
          Sintatico  :
            If F and E_l Then
               Er := True
            Else
               Begin
               Er := False;
               IndPToken := Salva ;
               End;
{$B-}
          Semantico  :
            Begin
            F;
            New(p);
            Oper2:=Stack.Pop;
            Oper1:=Stack.Pop;
            p^.VarType:=Combinam(PTemp(Oper1)^.VarType,PTemp(Oper2)^.VarType,Simb);
            Stack.Push(p);
            Dispose(PTemp(Oper1));
            Dispose(PTemp(Oper2));
            Orelha:=E_l;
            Er:=True
            End;
          End; {Case}
        End
      Else
        Begin
        Er := True;
        IndPToken := Salva;
        End;
    Result := Er;
  End; {E_l}

{----------------  P E R C O R R E  -----------------}

Begin {Percorre}
  Erro := 0;        {<<<<<<<<<<<<<<<<<}
  IndPToken := -1;
  Case RunType Of
    Avaliador  : Exp;

    Sintatico  :
      If Exp Then
         Percorre := True
      Else
         Begin
         Percorre := False;
         If (ErrorType = ENOperatorExpected) or
            (ErrorType = ENExpected_Open) Then Inc(PosError);

         Erro := ErrorType;
         End;

    Semantico  :
      Percorre := Exp

    End; { Case }
End; { Percorre }

Function TAvaliator.Eval(Const Expr: String; var PRes: PTemp): Integer ;
Var
  Op       : Pointer;
  ErrCode  : word;
Begin { Eval }
  If Not Assigned(Tab_Sim) then
     Raise EExpectedTabSim.Create('Nenhuma Tabela de Variáveis Associada ao Avaliador.'+
                                  'Use o método SetTabVar() do Avaliador' );

    If Erro = 0 Then
       If (TElem = vtNumeric) or (TElem = vtVector) or (TElem = vtMatrix) Then
          Percorre(Avaliador, Erro);

    If Erro <> 0 Then
       Begin
       Eval := Erro;
       Exit;
       End;

    Eval := Erro;

    {Como pegar o resultado final ?!!}
    If TElem = vtNumeric Then
       Begin
       Op := Stack.Pop;
       PRes^.Value := PTemp(Op)^.Value;
       PRes^.VarType := TElem;
       PRes^.Error := Erro;
       Dispose(PTemp(Op));
       End; {If vtNumeric}

    If (TElem = vtVector) or (TElem = vtMatrix)Then
       Begin
       Op := Stack.Pop;
       {Fazer cópia da variável caso necessário}
       if (TElem = vtMatrix) and not FReference then
          {Cria uma cópia.}
          TWSMatrix(PTemp(Op)^.Ender).Copy(TWSMatrix(PTemp(Op)^.Ender).MatType,TwsMatrix(PRes^.Ender))
       else {TElemen = vtVector}
         if not FReference then
           PRes^.Ender := TwsVec(PTemp(Op)^.Ender).Copy(1,TwsVec(PTemp(Op)^.Ender).Len)
         else
           PRes^.Ender := PTemp(Op)^.Ender;

       PRes^.VarType := TElem;
       PRes^.Error := Erro;
       Dispose(PTemp(Op));
       End;

    If Erro <> 0 Then ShowException(Erro, '');
End; { Eval }

{---------------------- Classe TFunctions ----------------------}

Destructor TFunctions.Destroy;
var I: LongInt;
Begin
  For i := 0 to (self.count-1) do
     Dispose(pTFunctionRec(Self.Items[I]));

  Inherited Destroy; {Chama o destructor da propria lista}
End;

{----------------- AddFunction ------------------}

procedure TFunctions.AddFunction (const Funcao: String; LI, LS, Incremento: Double);
var pRegFunc: pTFunctionRec;
begin
  New(pRegFunc);
  pRegFunc.LI   := LI;
  pRegFunc.LS   := LS;
  pRegFunc.Incr := Incremento;
  Try
    pRegFunc.Eval := TAvaliator.Create;
    pRegFunc.Eval.TabVar.AddFloat('x', 0); {adiciona variavel x inicializada em zero}
    pRegFunc.Eval.Expression := Funcao;
  Except
    Dispose(pRegFunc);
    Raise;
  end;
  Add(pRegFunc);
end;

{----------------- DelFunction ------------------  - zeh 5/8/99}
Procedure TFunctions.DelFunction(Index : Integer);
Begin
  Dispose(pTFunctionRec(Self.Items[Index]));
  Self.Delete(Index);
End;

{----------------- GetValue ------------------}

Function TFunctions.GetValue (x: double; FunctionIndex: Integer): Double;
var Eval: TAvaliator;
begin
  Eval := pTFunctionRec(Self.Items[FunctionIndex])^.Eval;
  Eval.TabVar.SetFloatValue('x',x);
  Result := Eval.Evaluate.FResult.Value;
end;

{----------------- GetFunc ------------------}

Function TFunctions.GetFunc (index: integer): TFunctionRec;
begin
  Try
    Result := pTFunctionRec(Self.Items[Index])^;
  Except
    Raise Exception.CreateFmt('Índice [%d] de função inválido', [Index]);
  End;
end;
(*
Procedure TFunctions.SetParams(index: Integer; LI, LS, Incr: Double);
Begin
  Try
    pTFunctionRec(Self.Items[Index])^.LI   := LI;
    pTFunctionRec(Self.Items[Index])^.LS   := LS;
    pTFunctionRec(Self.Items[Index])^.Incr := Incr;
  Except
    Raise Exception.CreateFmt('Índice [%d] de função inválido', [Index]);
  End;
End;
*)
Procedure TFunctions.SetParams(index: Integer; const Funcao: String; LI, LS, Incr: Double);
var pRegFunc: pTFunctionRec;
Begin
  Try
    pRegFunc := pTFunctionRec(Self.Items[Index]);
  Except
    Raise Exception.CreateFmt('Índice [%d] de função inválido', [Index]);
  End;
  pRegFunc.LI   := LI;
  pRegFunc.LS   := LS;
  pRegFunc.Incr := Incr;
  pRegFunc.Eval.Expression := Funcao;
End;

Function TFunctions.GetSerie (index: integer): TList;
var p  : ^Double;
    x  :  Double;
begin
  Result := TList.Create;
  x := Func[Index].LI;
  While x < Func[Index].LS do
    Try
    New(p);
    p^ := GetValue(x, Index);
    Result.Add(p);
    Except
      {Não posso mais adicionar, estorei a capacidade da lista ou não há memória, etc}
      Break;
    End;
end;

const
  cResultError = 'O resultado da operação não é do tipo %s';

{ TEvalResult }

function TEvalResult.GetAsFloat: Double;
begin
  if IsFloat then
     Result := FResult.Value
  else
     Raise Exception.CreateFmt(cResultError, ['FLOAT']);
end;

function TEvalResult.GetAsMatrix: TwsMatrix;
begin
  if IsMatrix then
     Result := TwsMatrix(FResult.Ender)
  else
     Raise Exception.CreateFmt(cResultError, ['MATRIZ']);
end;

function TEvalResult.GetAsString: String;
begin
  Result := FloatToStr(GetAsFloat);
end;

function TEvalResult.GetAsVector: TwsVec;
begin
  if IsVector then
     Result := TwsVec(FResult.Ender)
  else
     Raise Exception.CreateFmt(cResultError, ['VETOR']);
end;

function TEvalResult.GetError: Integer;
begin
  Result := FResult.Error;
end;

function TEvalResult.GetIsFloat: Boolean;
begin
  Result := (FResult.VarType = vtNumeric);
end;

function TEvalResult.GetIsMatrix: Boolean;
begin
  Result := (FResult.VarType = vtMatrix);
end;

function TEvalResult.GetIsVector: Boolean;
begin
  Result := (FResult.VarType = vtVector);
end;

function TEvalResult.getMatrixType: TwsEnumMatType;
begin
  Result := GetAsMatrix.MatType;
end;

function TEvalResult.getResultType: TwsEnumVarType;
begin
  Result := FResult.VarType;
end;
end. {UEval}
