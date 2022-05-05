unit wsConstTypes;

{
}

interface
uses Graphics, {TColor}
     SysUtils; {Exceptions}

Const

  // Tipo Texto.
  cTEXTO_FORMATADO = 0;
  cTEXTO_NAO_FORMATADO = 1;

  HTMTMPFILEFULL = 'HTMTMPFILEFULL.htm'; //Arquivo temporario qua armazena o codigo html completo (acumulado ou nao), dependendo do append
  HTMTMPFILE     = 'HTMTMPFILE.htm';     // Arquivo temporario que e salvo a cada analise, contendo somente a analise atual
  TXTTMPFILE     = 'TXTTMPFILE.TXT';     // Arquivo temporario que e salvo a cada analise, contendo somente a analise atual

  NONAME         = 'Noname';

  {Constante para WinStat}
  PixelPerInch = 96; {para o perform - Fernanda}

  { UdtFunc }
  CharEOL : Char = #10;

  MFuncError    = 500;
  NImprDim      = MFuncError + 1;
  NIterMax      = MFuncError + 2;
  NSingMat      = MFuncError + 3;
  NHouse        = MFuncError + 4;
  NSquareMat    = MFuncError + 5;
  NInvPower     = MFuncError + 6;
  NNotMatchType = MFuncError + 7;

  { UEval }
  ErrorUEval          = 1000;
  ENOperatorExpected  = ErrorUEval + 1;
  ENExpected_Open     = ErrorUEval + 2;
  ENExpected_Close    = ErrorUEval + 3;
  ENIdentExpected     = ErrorUEval + 4;
  ENTypeMismatch      = ErrorUEval + 5;
  ENUnknownVariable   = ErrorUEval + 6;
  ENMyDivByZero       = ErrorUEval + 7;
  ENIdentExpectedAfterOpen = ErrorUEval + 8;

  {Cursores}
  crTransferData     = 1050;

  {Constantes usadas nas Units UEval, Matrix1, Etc}
  EMatrixError = 1100;
  maConvError  = EMatrixError + 1;  { Nao convergiu nas iteracoes informadas }
  maHouError   = EMatrixError + 2;  { Erro na triangulacao de Householder }
  maInvError   = EMatrixError + 3;  { Matriz nao e inversivel }
  maTridError  = EMatrixError + 4;  { Erro na solucao do sistema tridiagonal }
  maToepError  = EMatrixError + 5;  { Resolucao de sistema Toeplitz sem sucesso }
  maCholError  = EMatrixError + 6;  { Matriz de Cholesky de posto incompleto }
  maInputError = EMatrixError + 7;  { Erro em dispositivo de entrada }
  maDimError   = EMatrixError + 8;  { Dimensao impropria para a operacao }
  maOpTypError = EMatrixError + 9;  { Tipos improprios para operacao }
  maMemError   = EMatrixError + 10; { Nao existe memoria suficiente para operacao }
  maMGSError   = EMatrixError + 11; { Valor menor que a precisao no MGS }
  maCNumError  = EMatrixError + 12; { Conversao numerica com problemas }
  maOutError   = EMatrixError + 13; { Objeto de impressao precisa ser inicializado }
  maTextError  = EMatrixError + 14; { Erro na abertura do arquivo texto }
  maNRCError   = EMatrixError + 15; { Nao definiu numero de linhas e/ou de colunas }
  maForError   = EMatrixError + 16; { Erro na interpretacao de formulas }

  { Operacoes com arquivos }
  FileError  = 1500;
  NReadError = FileError + 1;
  NWriteError= FileError + 2;

  { Prob }
  ProbError = 2000;
  ParamIleg   = ProbError + 1;     {Parametro ilegal em Factrl,GSer,GammP,GammQ ou BetaI}
  NumItPeq    = ProbError + 2;     {Numero de iteracoes e pequeno}
  IntProbI    = ProbError + 5;     {Integral I calculada como 1-I'}
  PrecIntProb = ProbError + 6;     {Integral I calculada sob condicao 5}
  DigSignif   = ProbError + 7;     {Perda de digitos significativos no calculo do ponto percentual}
  IntProb2I   = ProbError + 8;     {Integral foi calculada como 1-2I'}
  AproxNormal = ProbError + 11;    {Foi utilizada aproximacao normal}
  Aprox       = ProbError + 14;    {Foi utilizada uma aproximacao}
  ValNNulo    = ProbError + 15;    {Na condicao 14 retornou valor nao nulo}
  Inter3Pont  = ProbError + 20;    {Foi usada formula harmonica de 3 pontos}
  PontPercInv = ProbError + 22;    {Ponto percentual incorreto foi retornado}
  InterPont   = ProbError + 23;    {Metodo na condicao 20 foi usado, com problema na condicao 22}
  PontPercF   = ProbError + 24;    {Overflow no calculo do ponto percentual F}
  ErrGLiber   = ProbError + 30;    {Erro nos graus de liberdade nas distribuicoes Beta e Qui-quadrado}
  PontPercInf = ProbError + 31;    {p>=1 ou p<=0 levando a pont.perc.infinito}
  ParamX      = ProbError + 32;    {x<0 em X2Int,FInt,Quiq,FDen ou BInt ou X>1 em BInt ou 0>x>1 em Beta}
  ParamInval  = ProbError + 35;    {parametros inválidos em beta,weibull, exponencial, Binomial, BinNeg, poisson}

  { Randist }
  RanDistError = 2500;
  NInvalidParam = RanDistError+1;

  { LinMod }
  LinModError = 2600;
  lmUnknownVariable = LinModError + 1; { Variável não está presente no conunto de dados }


  MNOperatorExpected = 'Falta de operador';
  MENExpected_Close  = 'Falta de ")"';
  MENExpected_Open   = 'Falta de "("';
  MENIdentExpected   = 'Falta de identificador';
  MENTypeMismatch    = 'Tipos não combinam';
  MENUnknownVariable = 'Variável desconhecida';
  MMyDivByZero       = 'Divisão por zero';
  MImprDim           = 'Dimensoes improprias para a operacao';
  MIterMax           = 'Ultrapassado numero maximo de iteracoes';
  MSingMat           = 'Operacao impropria com matriz singular';
  MHouse             = 'Problema na transformacao de Householder';
  MSquareMat         = 'Matriz deve ser quadrada';
  MNotMatchType      = 'Tipos nao combinam';
  MPowerInv          = 'Potencia Invalida';
  MReadError         = 'Erro na leitura do Arquivo';
  EvalPadrao         = 'Erro na Expressão < %s > : %s';
  MIdentExpectedAfterOpen = 'Falta de Identificador após "("';

  { ProbClass }
  MInvalidParam = 'Valor %*.*g é inválido como Parâmetro';
  MInvalidProb  = 'Valor %*.*g é inválido como Probabilidade';
  MInvalidPerc  = 'Valor %*.*g é inválido como Percentil';

  { TabVar }
  MsgInvalidIndexVar = 'Índice Inválido';
  MsgInvalidNameVar  = '< %s > Não é um identificador válido';
  MsgUnknownVariable = 'Variável < %s > Desconhecida';

  { TMatrix }
  wsMsgInvalidIndexVar = 'Índice de coluna inválido <%d>';

  { Projeto WinStat }
  cNomeNaoValido     = MsgInvalidNameVar;

  // Atencao!!! Nao mudar a ordem destes nomes de estatisticas
  cLongNameEstat     : Array[0..15] of String = (
                       'Média',
                       'Variância',
                       'Desvio Padrão',
                       'Total',
                       'Mínimo',
                       'Máximo',
                       'No de Valores',
                       'No de Valores Válidos',
                       'Erro Padrão da Média',
                       'Amplitude',
                       'Coeficiente de Variação',
                       'Assimetria',
                       'Curtose',
                       'Soma dos Quad. não Corrigida',
                       'Soma dos Quad. Corrigida',
                       'Soma dos Pesos');

  // Atencao!!! Nao mudar a ordem destes nomes de estatisticas
  cShortNameEstat    : Array[0..15] of String = (
                       'Media',
                       'Variancia',
                       'DPadrao',
                       'Total',
                       'Minimo',
                       'Maximo',
                       'nObs',
                       'nObsVal',
                       'EPMedia',
                       'Amplitude',
                       'CoefVar',
                       'Assimetria',
                       'Curtose',
                       'SQNCorrig',
                       'SQCorrig',
                       'SomaPeso');

  { Reg }
  mrEstatisticas            = 1;   { mr -> Modelo de Regressão }
  mrCorrelacoes             = 2;
  mrInversa                 = 4;
  mrEstimativas             = 1;
  mrErroPadrao              = 2;
  mrLIC95                   = 4;
  mrLSC99                   = 8;
  mrTesteT                  = 16;
  mrTolerancia              = 32;
  mrInflacVariancia         = 64;
  mrEstimativasPadronizadas = 128;
  mrEstimativaResiduo       = 1;
  mrRegressaoParcial        = 2;
  mrComponenteResiduo       = 4;
  mrDFitsIndice             = 8;
  mrRExtIndice              = 16;
  mrCoeficientesI           = 1;
  mrCoeficientesS           = 2;
  mrCovarianciaI            = 4;
  mrCorrelacaoI             = 8;
  mrColinearidadeI          = 16;
  mrAnalisedaVariacaoI      = 32;
  mrAnalisedaVariacaoS      = 64;
  mrPredicaoI               = 128;
  mrPredicaoS               = 256;
  mrInfluenciaI             = 512;
  mrInfluenciaS             = 1024;

  gCores : Array [0..13]  of TColor =
    (clRed, clBlue, clOlive, clPurple, clGreen, clSilver, clAqua,
     clRed, clBlue, clOlive, clPurple, clGreen, clSilver, clAqua);

type

  TDist = (dbBeta,dbExpon,dbExpon2,dbF,dbGamma,dbGamma2,dbGamma3,dbLogNormal,dbLogNormal3,
    dbMErlang,dbNormal,dbStdNorm,dbUnif,dbUnifDisc,dbQuiq,dbT,dbWeibull,dbWeibull3,dbGumbel,
    dbPearson5,dbContUser,dbPoisson,dbBinom,dbGeom,dbNegBin,dbHyper,dbDiscUser);
    
const
  ValCode:         Integer = 0;        {Codigo de erro para procedimento Val}
  MaxNumPointer            = 16383;    { Numero maximo de ponteiros }
  MaxFArraySize            = 65528 div SizeOf(Double);
  MaxIntArray              = 32766;
  MaxNumStr                = 255;
  eps                      = 1.5e-9;   { Precisao pre-definida }
  MaxNameLen               = 80;       { Tamanho maximo para nomes de variaveis }

  { Delimitadores para quebra de tokens: Tab, LF, CR, espaco, virgula, igual,
    barra inv, aspas, parentesis}
  MinFloatValue            = -1.7E308;
  ScalarTrue               = 1;
  ScalarFalse              = 0;
  wscInvariableMissValue   = -9999999999999999; // -1E16

var
  // Pode ser alterada caso o valor perdido seja diferente do valor acima
  wscMissValue : Double = wscInvariableMissValue;
  wsvFuzzVal   : Double = 1e-10;

{ ********************************** TYPES ************************************}

Type
  TwsEnumDlgType  = (tdFix, tdFree, tdData);
  TwsEnumOperar   = (tdGerar, tdOrdenar, tdAgregar, tdSelecionar);
  TwsEnumColunas  = (tdGerCol, tdInserir, tdEditar);
  TwsEnumSigLevel = (slFive, slOne, slTen);


  pwsRecBloco = ^TwsRecBloco;
  TwsRecBloco = Record
                  Nome   : String;
                  Inicio : Integer;
                  Fim    : Integer;
                End;

  TIArray        = array[1..MaxIntArray] of Integer;
  TSIArray       = array[1..65534] of ShortInt;
  TWArray        = array[0..MaxIntArray] of Word;
//  TLIntArray     = array[1..16383] of LongInt;
  TFArray        = array[1..MaxFArraySize] of Double;
  TStrArray      = array[1..MaxNumPointer] of PString;
  TCharArray     = array[0..65534] of Char;
  TBufChar       = array[0..1023] of Char;
  TChar80        = array[0..79] of Char;
  TChar4         = array[0..4] of Char;
  TCharStr       = array[0..255] of Char;

  PathStr        = TChar80;
  Str2           = String[2];
  StrName        = String[MaxNameLen];

  PWArray        = ^TWArray;   { Ponteiros para arrays de uso geral }
  PIArray        = ^TIArray;
  PSIArray       = ^TSIArray;
//  PLIntArray     = ^TLIntArray;
  PCharArray     = ^TCharArray;
  PStrArray      = ^TStrArray;
  PVec           = ^TVecArray;
  PBufChar       = ^TBufChar;
  PPathStr       = ^PathStr;

  PFArray     = ^TFArray;
  TVecArray   = array[0..7] of PFArray;

   TwsRecBuffer = Record
                    Size : byte;
                    Data : PFArray;
                  End;

  PwsRecFixFree = ^TwsRecFixFree;
  TwsRecFixFree = Record
                       Nome       : String;
                       Descricao  : String;
                       Tipo_Var   : Char;
                       Tipo_Fator : Char;
                       Final      : Byte;
                       Campo      : String;
                       Inicio     : Byte;
                     End;

  TwsEnumStatistics = (teMedia,teVar,teDPadr,teTotal,teMin,teMax,teN,teNVal,teEPMed,teAmpl,
                       teCVar,teAssim,teCurt,teSQNCo,teSQCor,teMediana,teQuart1,teQuart2);

  {Opções para os modelos}
type
    // NÃO MUDAR A ORDEM !!!
    // As constantes enumeradas iniciam em 0
    TwsEnumModels = (            // cm ??
        cmNone,                  //  = 0;
        cmIntercept,             //  = 1;
        cmUnivar,                //  = 2;
        cmOrder,                 //  = 3;

        {Opções para anal. de regressão}
        cmEstatist,              //  = 4;  // Obtem estatisticas descritivas
        cmCorrelVar,             //  = 5;  // Obtem correlacao entre as variaveis
        cmInverse,               //  = 6;  // Imprime matriz inversa
        cmColinear,              //  = 7;  // Analise de colinearidade
        cmPrintCoef,             //  = 8;  // Imprime/salva coeficientes de regressao
        cmSaveCoef,              //  = 9;
        cmPrintCovar,            //  = 10; // Imprime matriz de covariancias
        cmPrintCorrel,           //  = 11; // Imprime matriz de correlacoes
        cmPrintVarAnalysis,      //  = 12; // Imprime/salva variaveis em analise
        cmSaveVarAnalysis,       //  = 13;
        cmPrintPredic,           //  = 14; // Imprime/salva resultados Opcao Predicao
        cmSavePredic,            //  = 15;
        cmPrintInfluence,        //  = 16; // Imprime/salva analise de influencia
        cmSaveInfluence,         //  = 17;
        cmPrintData,             //  = 18; // Imprime/salva dados do modelo
        cmSaveData,              //  = 19;

        {Opções para anal. da variação}
        cmVar_PrintQuadAnalysis, //  = 20;  // Imprime/salva quadro da analise
        cmVar_SaveQuadAnalysis,  //  = 21;
        cmVar_PrintCoef,         //  = 22;  // Imprime/salva coeficientes
        cmVar_SaveCoef,          //  = 23;
        cmVar_PrintMatMod,       //  = 24;  // Imprime/salva matriz do modelo reg e anova
        cmVar_SaveMatMod,        //  = 25;
        cmVar_PrintMatInc,       //  = 26;  // Imprime matriz de incidencia
        cmVar_PrintDadosMod,     //  = 27;  // Imprime/salva dados do modelo
        cmVar_SaveDadosMod,      //  = 28;
        cmVar_PrintMedias,       //  = 29;  // Imprime medias das combinacoes
        cmVar_Contrastes,        //  = 30;  // Imprime contrastes
        cmVar_EstOrt,            //  = 31;  // Lista matriz das estimativas ortogonais
        cmVar_SaveTesteF,        //  = 32;
        cmVar_PrintTesteCM,      //  = 33;  // Imprime/salva comparacoes de medias
        cmVar_SaveTesteCM,       //  = 34;
        cmVar_PrintTesteC,       //  = 35;  // Imprime/salva constrastes
        cmVar_SaveTesteC,        //  = 36;
        cmVar_PrintTesteRP,      //  = 37;  // Imprime/salva regressao polinomial
        cmVar_SaveTesteRP,       //  = 38;
        cmVar_SaveMedias,        //  = 39;  // Salva medias das combinacoes
        cmVar_GrafRP,            //  = 40;  // Possibilita a criação de gráficos

        {Opções para anal. multivariada}
        cmHipGeralCBWImp,        //  = 41;
        cmInterConfiancaImp,     //  = 42;
        cmSQP,                   //  = 43;
        cmHipGeralCBWSal,        //  = 44;
        cmInversaXX,             //  = 45;
        cmEstParametros,         //  = 46;
        cmDadosModeloImp,        //  = 47;
        cmAutoValAutoVet,        //  = 48;
        cmMatHip,                //  = 49;
        cmInterConfiancaSal,     //  = 50;
        cmDadosModeloSal,        //  = 51;

        cmVar_PrintMatMod2,      //  = 52;  // Imprime/salva matriz do modelo mreg
        cmVar_SaveMatMod2,       //  = 53;
        cmVar_PrintMatInc2,      //  = 54;  // Imprime/salva matriz de incidencia
        cmVar_SaveMatInc2,       //  = 55;

        cmVar_PrintMatMod3,      //  = 56;  // Imprime/salva matriz do modelo
        cmVar_SaveMatMod3,       //  = 57;
        cmVar_PrintMatInc3,      //  = 58;  // Imprime/salva matriz de incidencia
        cmVar_SaveMatInc3,       //  = 59;
        cmVar_PrintExpVal,       //  = 60;

        cmMVar_PrintMatSQeP,     //  = 61;
        cmMVar_SaveMatSQeP,      //  = 62;
        cmMVar_PrintTestes,      //  = 63;
        cmMVar_SaveTestes,       //  = 64;
        cmMVar_PrintMatMod,      //  = 65;
        cmMVar_SaveMatMod,       //  = 66;
        cmMVar_PrintModData,     //  = 67;
        cmMVar_SaveModData,      //  = 68;
        cmMVar_PrintCombMed,     //  = 69;
        cmMVar_SaveCombMed,      //  = 70;
        cmMVar_PrintMatInc,      //  = 71;
        cmMVar_PrintValExpQMs,   //  = 72;
        cmMVar_PrintCanonic,     //  = 73;
        cmMVar_SaveCanonic,      //  = 74;
        cmMVar_PrintUnivar,      //  = 75;
        cmMVar_PrintCorrel,      //  = 76;
        cmVar_MMQ                //  = 77;  // Média de Mínimos Quadrados
        );

{ Prob }
  FArray = array[1..7] of Double;
  HFunc = function(x: Double; ax: FArray; upper: Boolean): Double;

{ RotUtils }
  Str16 = String[16];


{ Opções para impressões tanto em modo texto quanto HTML }
  TwsRecPrintOptions = record
                         Center       : Boolean;
                         PrintDesc    : Boolean;   // Imprime descritores de DataSets?
                         ColWidth     : byte;      // Largura para impressão das colunas
                         ColPrecision : byte;      // Precisão (dígitos significativos) de impressão das colunas
                         MaxIDSize    : byte;      // Tamanho máximo do identificador (Nome) de linha
                         LineLen      : Integer;   // Tamanho da linha de impressão quando texto
                       end;

  TwsEnumTypeOp = (opSum,opSub,opDiv,opProd,opPower,  // Operadores aritmeticos
                   opGE,opGT,opLE,opLT,opEQ,opNE,     // Comparacao
                   opOR,opAnd,                        // Logicos
                   opMax,opMin,                       // Maximo, minimo
                   opMean,opQ1,opQ2,opQ3,opSSq);      //Media, 1o, 2o e 3o quartis, Soma de quadr

  TwsEnumConstFun =
    (cABS,    cEXP,     cAPROXIMA,cINT,   cLN,     cRAIZ,   cARCTAN,
     cARCSEN, cARCCOS,  cSEN,     cCOS,   cSENH,   cCOSH,   cTAN,
     cLOG,    cANG,     cLGAMA,   cFLOOR, cCEIL,    cINV,    cFRAC,
     cABSNORM,cMEDIA,   cSOMA,    cSQUAD, cRESOLVE,cTANH,   cPOSTOEMP,
     cQSORT,  cENORM,   cACUM,    cT,     cAtA,    cAAt,    cIDENT,
     cHELMERT,cCONTROLE,cEXPER,   cPORTOG,cVECDIAG,cDIAGVEC,cTODOS,
     cALGUM,  cTRACO,   cPOSTO,   cGINV,  cMAX,    cMIN,    cCHOL,
     cPERFIL, cVALUNIF, cQNORM,   cVNORM, cEchelon,cGS,     cMGS,
     cHProj,  cQProj,   cTGAMA,   cALOC,  cHILBERT, cG2Inv, cHermite,
     cIndic,  cMEAN);

// Eventos
  TwsCreatedObject_Event = procedure(Sender, Obj: TObject) of object;
  TwsObjectCreation_Event = procedure(Sender, Obj: TObject) of object;

implementation

end.
