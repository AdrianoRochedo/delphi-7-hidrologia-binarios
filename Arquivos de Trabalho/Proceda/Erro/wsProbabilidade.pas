unit wsProbabilidade;
{
 Unidade para definição de objetos para o uso das distribuições de Probabilidade
}
interface
Uses SysUtils,
     wsConstTypes,
     wsGLib,
     wsMatrix,
     wsIntegral,
     wsVec,
     wsDistribuicoesRandomicas,
     wsAvaliadorDeExpressoes,
     Form_Chart;

Type

  { ================== Distribuições de Probabilidade ============== }

{ ============================== TwsProb ============================== }

{ Herança
    TwsProb --> TObject
  Objetivo
    Construção de uma classe básica para implementação do cálculo de probabilidades das
    diferentes distribuições de probabilidade. Não pode ser utilizada diretamente.
}
TwsProb = class
Private
  // Probabilidade na cauda superior?
  FUpper  : Boolean;
  FRand   : TwsRanDist; // criado por cada descendente que pode gerar valores
  FnParEst: Byte;         // numero de parametros estimados da distribuicao
  FV      : TwsSymmetric; // matriz de covariancias das estimativas
  // Atribui constante de erro e emite mensagem
  // Gera valor com distribuicao
  function GetRandValue: Double; virtual; abstract;
  Procedure SetErro(num: word; const p: Double);
  Destructor Destroy; override;
Public
  // Cria e inicializa campos desta classe
  constructor Create;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); virtual; abstract;
  // Cálculo de probabilidade
  function Prob(const x: Double): Double; virtual; abstract;
  // Retorna uma matriz com medidas descritivas das distribuições
  procedure Descriptive(var Res: TwsGeneral); virtual; abstract;
  // Obtem estimativa pelo método da máxima verossimilhança
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; virtual; abstract;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); virtual; abstract;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec;  UpdatePar: Boolean=True): TwsVec; virtual; abstract;
  // Periodo de retorno pelo metodo dos momentos
  procedure MoReturnPeriods(x: TwsVec; F: TwsGeneral); virtual; abstract;
  // Obtem valores da função de log-verossimilhança para varios valores dos parametros
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec): TwsGeneral; virtual; abstract;
  // Obtem valores parciais da função de log-verossimilhança
  function PartialLogLikelihood(x: TwsVec; const xInf,xSup,xDelta: Double):
    TwsGeneral; virtual; abstract;
  // Obtem valores da função de log-verossimilhança
  function LogL(x: TwsVec): Double; virtual; abstract;

  // numero de parametros estimados
  Property nParEst: byte read FnParEst write FnParEst;
  // Probabilidade na cauda superior?
  Property Upper: Boolean read FUpper write FUpper;
  // Obtem valor com distribuicao
  property RandValue: Double read GetRandValue;
  // Obtem matriz de covariancias
  property Cov: TwsSymmetric read FV write FV;
end;

{========================= TwsProbCont =================================}
{ Herança
    TwsProbCont --> TwsProb --> TObject
  Objetivo
    Classe básica para implementação de distribuições contínuas. Não pode ser utilizada
    diretamente.
}

TwsProbCont = class(TwsProb)
Private
  // Precisão desejada para o cálculo de inversas/probabilidade
  FEps: Double;
  // Atribui precisão
  procedure SetEps(const y: Double);
  procedure GetYCoord(var yMin,yMax: Double); virtual;
  procedure GetLimits(var xInf,xSup: TwsVec; n: integer);
Public
  // Inicializa campos deste objeto
  constructor Create;
  // Inversas das distribuições
  function Quantil(x: Double): Double; virtual; abstract;
  // Função de densidade
  function Density(const x: Double): Double; virtual; abstract;
  // Vetor de valores da função de densidade
  function VecDensity(x1,x2: Double; n: Integer; var xVec: TwsVec): TwsVec; virtual;
  // Inversa da probabilidade empírica para gráfico de probabilidade
//  function QQInv(k,n: integer; const c: Double): Double; virtual;
  // Grafico de probabilidade
  function QQData(var x, p: TwsVec; const c: Double=0.375): TwsVec; virtual;
  function QQPlot(vData: TwsVec; Null,Env: Boolean; const c: Double;
    var S: TwsGeneral): TfoChart;
  function ChiSqrTest(x: TwsVec; var Res: TwsGeneral; nc: Integer=0; p: TwsVec=nil): TwsGeneral;
  // Obtem a estatistica D para o teste de Kolmogorov-Smirnov
  function KSD(x: TwsVec): TwsGeneral;
  // Executa o teste de ajustamento de Kolmogorov-Smirnov
  function KSTest(x: TwsVec; var VC: TwsGeneral):  TwsGeneral; virtual;
  // Executa o teste de ajustamento de Anderson-Darling
  function ADTest(x: TwsVec; var VC: TwsGeneral):  TwsGeneral; virtual;
end;

{ ============================== TwsProbStdNormal ========================== }
{ Herança
    TwsProbStdNormal --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição normal padrão
}

TwsProbStdNormal = class(TwsProbCont)
private
  c1: Double;
  // Gera valor com distribuicao normal padrao
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
public
  constructor Create;
  // Cálculo de probabilidade na normal padrão
  function Prob(const x: Double): Double; override;
  // Quantis da normal padrão
  function Quantil(p: Double): Double; override;
  // Função de densidade da normal padrão
  function Density(const x: Double): Double; override;
  // Medidas descritivas da normal padrão
  procedure Descriptive(var Res: TwsGeneral); override;
end;

{ ============================== TwsProbHalfNormal ========================== }
{ Herança
    TwsProbHalfNormal --> TwsProbStdNormal --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição meio normal (distribuição normal truncada na media)
}
TwsProbHalfNormal = class(TwsProbStdNormal)
private
  // Parametro de dispersao
  FLam: Double;
  // Atribui valor do parametro de dispersao
  procedure SetLam(const s: Double);
  // Gera valor com distribuicao semi-normal
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
public
  // Cria objeto para distribuição semi-normal com parametro
  constructor Create(const s: Double); overload;
  // Cria objeto para distribuição semi-normal sem parametro
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidades na distribuição normal
  function Prob(const x: Double): Double; override;
  // Quantis na distribuição normal
  function Quantil(p: Double): Double; override;
  // Medidas descritivas na distribuição normal
  procedure Descriptive(var Res: TwsGeneral); override;
  // Função de densidade da distribuição normal
  function Density(const x: Double): Double; override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec;  UpdatePar: Boolean=True): TwsVec; override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;
  // Atribui/lê valor do desvio padrão
  Property Lambda: Double read FLam write SetLam;
end;

{ ============================== TwsProbNormal ========================== }
{ Herança
    TwsProbNormal --> TwsProbStdNormal --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição normal
}
TwsProbNormal = class(TwsProbStdNormal)
Private
  // Média e desvio padrão
  FSigma,FMean: Double;
  // Atribui valor do desvio padrão
  procedure SetSigma(const s: Double);
  // Atribui valor da media
  procedure SetMean(const m: Double);
  // Gera valor com distribuicao normal
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  // Cria objeto para distribuição normal
  constructor Create(const m, s: Double); overload;
  // Cria objeto para distribuição normal sem especificar parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidades na distribuição normal
  function Prob(const x: Double): Double; override;
  // Quantis na distribuição normal
  function Quantil(p: Double): Double; override;
  // Medidas descritivas na distribuição normal
  procedure Descriptive(var Res: TwsGeneral); override;
  // Função de densidade da distribuição normal
  function Density(const x: Double): Double; override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;
  // Obtem valores da função de log-verossimilhança
  function LogL(x: TwsVec): Double; override;
  // Executa teste de Kolmogorov-Smirnov
  function KSTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral; override;
  // Executa o teste de ajustamento de Anderson-Darling
  function ADTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral; override;
  // Atribui/lê valor da média
  property Mean: Double read FMean write SetMean;
  // Atribui/lê valor do desvio padrão
  Property Sigma: Double read FSigma write SetSigma;
end;

{ ============================== TwsProbLognormal ========================== }
{ Herança
    TwsProbLognormal --> TwsProbNormal --> TwsProbStdNormal --> TwsProbCont --> TwsProb
      --> TObject
  Objetivo
    Implementa a distribuição lognormal (dois parâmetros)
}
TwsProbLogNormal = class(TwsProbNormal)
private
  // Gera valor com distribuicao lognormal
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
public
  // Cria objeto especificando parametros
  constructor Create(const LPar,APar: Double); overload;
  // Cria objeto sem especificar parametros
  constructor Create; overload;
  // Cálculo de probabilidades na distribuição lognormal
  function Prob(const x: Double): Double; override;
  // Quantis na distribuição lognormal
  function Quantil(p: Double): Double; override;
  // Medidas descritivas na distribuição lognormal
  procedure Descriptive(var Res: TwsGeneral); override;
  // Função de densidade da distribuição lognormal
  function Density(const x: Double): Double; override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;
end;

{ ============================== TwsProbNormalPower ========================== }
{ Herança
    TwsProbNormalPower --> TwsProbNormal --> TwsProbStdNormal --> TwsProbCont --> TwsProb
      --> TObject
  Objetivo
    Implementa a distribuição normal potência (Box-Cox)
}
TwsProbNormalPower = class(TwsProbNormal)
private
  FLam: Double;
  // Gera valor com distribuicao normal potencia
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
  procedure SetLam(const x: Double);
  function GetY(const x: Double): Double;
public
  // Cria objeto especificando parametros
  constructor Create(const LPar,APar: Double); overload;
  // Cria objeto sem especificar parametros
  constructor Create; overload;
  // Cálculo de probabilidades na distribuição normal potencia
  function Prob(const x: Double): Double; override;
  // Quantis na distribuição normal potencia
  function Quantil(p: Double): Double; override;
  // Medidas descritivas na distribuição normal potencia
  procedure Descriptive(var Res: TwsGeneral); override;
  // Função de densidade da distribuição normal potencia
  function Density(const x: Double): Double; override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;
  // Retorna uma matriz com valores da logverossimilhanca
  function PSearch(x: TwsVec; const cMin:Double=-2; const cMax: Double=2;
    const nVal: Integer=40): TwsGeneral;
  // Atribui valor para potencia
  property Lambda: Double read FLam write SetLam;
end;

{ ============================== TwsProbLognormal3 ========================== }
{ Herança
    TwsProbLognormal3 --> TwsProbLognormal --> TwsProbNormal --> TwsProbStdNormal
      --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição lognormal (três parâmetros)
}
TwsProbLogNormal3 = class(TwsProbLognormal)
private
  // Parâmetro de deslocamento da distribuição lognormal3
  FTheta: Double;
  // Atribui valor do parâmetro de deslocamento
  procedure SetTheta(const t: Double);
  // Gera valor com distribuicao lognormal com tres parametros
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
public
  // Cria objeto para distribuição lognormal3
  constructor Create(const t,m,s: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidades na distribuição lognormal
  function Prob(const x: Double): Double; override;
  // Quantis na distribuição lognormal
  function Quantil(p: Double): Double; override;
  // Medidas descritivas na distribuição lognormal
  procedure Descriptive(var Res: TwsGeneral); override;
  // Função de densidade da distribuição lognormal
  function Density(const x: Double): Double; override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;
  // Obtem valores parciais da função de log-verossimilhança
  function PartialLogLikelihood(x: TwsVec; const xInf,xSup,xDelta: Double): TwsGeneral; override;

  Property Theta: Double read FTheta write SetTheta;
end;

{ ============================== TwsProbGama ========================== }
{ Herança
    TwsProbGamma --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição gama de um parâmetro
}

TwsProbGamma = Class(TwsProbCont)
Private

  aux1,aux2,        // valores auxiliares
  FAlpha: Double;   // Parâmetro da distribuição gama
  // Atribui valor do parâmetro
  procedure SetAlpha(const s: Double);
  // Gera valor com distribuicao gama (1 parametro)
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  // Cria objeto para distribuição gama
  constructor Create(const a: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na gama
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na gama
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da gama
  function Density(const x: Double): Double; override;
  // Medidas descritivas da gama
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Estimativa pela maxima verossimilhança
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;

  Property Alpha: Double read FAlpha write SetAlpha;
end;

{ ============================== TwsProbGama2 ========================== }
{ Herança
    TwsProbGamma2 --> TwsProbGamma --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição gama com dois parâmetros
}

TwsProbGamma2 = Class(TwsProbGamma)
Private
  // Parâmetro da distribuição gama
  FLam: Double;
  // Atribui valor do parâmetro
  procedure SetLam(const s: Double);
  // Gera valor com distribuicao gama (2 parametros)
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  // Cria objeto para distribuição gama
  constructor Create(const a,b: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na gama
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na gama
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da gama
  function Density(const x: Double): Double; override;
  // Medidas descritivas da gama
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;

  Property Lambda: Double read FLam write SetLam;
end;

{ ============================== TwsProbGama3 ========================== }
{ Herança
    TwsProbGamma3 --> TwsProbGamma2 --> TwsProbGamma --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição gama com três parâmetros (Pearson Tipo III)
}

TwsProbGamma3 = Class(TwsProbGamma2)
Private
  // Parâmetro de deslocamento da distribuição gama
  FTheta: Double;
  // Atribui valor do parâmetro de deslocamento
  procedure SetTheta(const t: Double);
  // Gera valor com distribuicao gama (3 parametros)
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  // Cria objeto para distribuição gama
  constructor Create(const a,b,c: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na gama
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na gama
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da gama
  function Density(const x: Double): Double; override;
  // Vetor de valores da função de densidade
//  function VecDensity(x1,x2: Double; n: Integer; var xVec: TwsVec): TwsVec; override;
  // Medidas descritivas da gama
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;

  Property Theta: Double read FTheta write SetTheta;
end;

{
============================== TwsProbLogPearson3 ========================== }
{ Herança
    TwsLPearson3 --> TwsProbGamma3 --> TwsProbGamma2 --> TwsProbGamma --> TwsProbCont -->
      TwsProb --> TObject
  Objetivo
    Implementa a distribuição Log-Pearson do Tipo III
}
TwsProbLogPearson3 = Class(TwsProbGamma3)
private
  // Gera valor com distribuicao gama (3 parametros)
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
public
  // Cria objeto para distribuição gama
  constructor Create(const a,b,c: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Cálculo de probabilidade na PT3
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na PT3
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da PT3
  function Density(const x: Double): Double; override;
  // Medidas descritivas da PT3
//  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Metodo da maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
end;

{ ============================== TwsProbBeta ========================== }
{ Herança
    TwsProbBeta --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição beta
}

TwsProbBeta = Class(TwsProbCont)
Private
  aux1,aux2,aux3,               // valores auxiliares
  FAlpha, FBeta  : Double;      // Parâmetros da distribuição beta
  // Atribui valor ao parâmetro alfa
  procedure SetAlpha(const a: Double);
  // Atribui valor ao parâmetro beta
  procedure SetBeta(const b: Double);
  // Gera valor com distribuicao beta
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição beta
  constructor Create(const a, b: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na beta
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na beta
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da beta
  function Density(const x: Double): Double; override;
  // Medidas descritivas da beta
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/lê valor do parâmetro alfa
  Property Alpha: Double read FAlpha write SetAlpha;
  // Atribui/lê valor do parâmetro beta
  Property Beta: Double read FBeta write SetBeta;
end;

{ ============================== TwsProbChiSqr ========================== }
{ Herança
    TwsProbChiSqr --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição qui-quadrado
}

TwsProbChiSqr = Class(TwsProbCont)
Private
  aux1,aux2,
  aux3,aux4,           // valores auxiliares
  FDF: Double;   // Parâmetro da distribuição qui-quadrado
  // Atribui valor do parâmetro
  procedure SetDF(const DF1: Double);
  // Gera valor com distribuicao qui-quadrado
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  // Cria objeto para distribuição qui-quadrado
  constructor Create(const DF1: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na qui-quadrado
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na qui-quadrado
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da qui-quadrado
  function Density(const x: Double): Double; override;
  // Medidas descritivas da qui-quadrado
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/lê valor do parâmetro graus de liberdade
  Property DF: Double read FDF write SetDF;
end;

{ ============================== TwsProbF ========================== }
{ Herança
    TwsProbF --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição F
}

TwsProbF =  Class(TwsProbCont)
Private
  aux1,aux2,aux3,                // valores auxiliares
  aux4,aux5,aux6,aux7,           // valores auxiliares
  Fn,Fd               : Double;  // Parâmetros da distribuição F
  // Atribui valor ao parâmetro GL Numerador
  procedure SetDFNum(const num: Double);
  // Atribui valor ao parâmetro GL Denominador
  procedure SetDFden(const den: Double);
Public
  // Cria objeto para distribuição F
  constructor Create(const num, den: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na F
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na F
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da F
  function Density(const x: Double): Double; override;
  // Medidas descritivas da F
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/lê valor do parâmetro GL Num
  property DFNum: Double read Fn write SetDFNum;
  // Atribui/lê valor do parâmetro GL Den
  property DFden: Double read Fd write SetDFden;
end;

{ ============================== TwsProbtStudent ========================== }
{ Herança
    TwsProbTStudent --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição t de Student
}

TwsProbTStudent = Class(TwsProbCont)
Private
  aux1,aux2,aux3,            // Valores auxiliares
  FDF            : Double;   // Parâmetro da distribuição t
  // Distribuição unilateral?
  FOneSided: boolean;
  // Atribui valor ao parâmetro
  procedure SetDF(const DF1: Double);
Public
  // Cria objeto para distribuição t
  constructor Create(const DF1: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na t
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na t
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da t
  function Density(const x: Double): Double; override;
  // Medidas descritivas da t
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/lê unilateral
  Property OneSided: Boolean read FOneSided write FOneSided;
  // Atribui/lê graus de liberdade
  Property DF: Double read FDF write SetDF;
end;

{ ============================== TwsProbExopnencial ========================== }
{ Herança
    TwsProbExponential --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição exponencial
}

TwsProbExponential = Class(TwsProbCont)
Private
  // Parâmetro de dispersao da distribuição exponencial
  FLam: Double;
  // Atribui valor ao parâmetro
  procedure SetLam(const b: Double);
  // Gera valor com distribuicao exponencial
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  // Cria objeto para distribuição exponencial
  constructor Create(const b: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na exponencial
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na exponencial
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da exponencial
  function Density(const x: Double): Double; override;
  // Medidas descritivas da exponencial
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem estimativa pelo método da máxima verossimilhança
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;
  // Obtem inversa para grafico de probabilidade
//  function QQInv(k,n: integer; const c: Double): Double; override;
  // Executa o teste de Kolmogorov-Smirnov
  function KSTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral; override;
  // Executa o teste de ajustamento de Anderson-Darling
  function ADTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral; override;

  // Atribui/lê valor do parâmetro
  Property Lambda: Double read FLam write SetLam;
end;

{ Herança
    TwsProbExponential2 --> TwsProbExponential --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição exponencial com dois parâmetros
}

TwsProbExponential2 = Class(TwsProbExponential)
Private
  // Parâmetro de origem da distribuição exponencial
  FTheta: Double;
  // Atribui valor ao parâmetro
  procedure SetTheta(const b: Double);
  // Gera valor com distribuicao exponencial (2 parametros)
  function GetRandValue: Double; override;
  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  // Cria objeto para distribuição exponencial
  constructor Create(const Th,Lb: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na exponencial
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na exponencial
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da exponencial
  function Density(const x: Double): Double; override;
  // Medidas descritivas da exponencial
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem estimativa pelo método da máxima verossimilhança
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Obtem valores da função de log-verossimilhança
  function LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral; override;
  // Atribui/lê valor do parâmetro
  Property Theta: Double read FTheta write SetTheta;
end;

{ ============================== TwsProbUniforme ========================== }
{ Herança
    TwsProbUniform --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição uniforme
}

TwsProbUniform = Class(TwsProbCont)
Private
  FAlpha,                // limite superior
  FBeta,FAmp: Double;    // limite inferior
  // Atribui valor ao parâmetro beta
  procedure SetBeta(const b: Double);
  // Atribui valor ao parâmetro alfa
  procedure SetAlpha(const b: Double);
  // Gera valor com distribuicao uniforme continua
  function GetRandValue: Double; override;
//  procedure GetYCoord(var yMin,yMax: Double); override;
Public
  constructor Create(const a,b: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na uniforme
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na uniforme
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da uniforme
  function Density(const x: Double): Double; override;
  // Medidas descritivas da uniforme
  procedure Descriptive(var Res: TwsGeneral); override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Atribui/lê valor do parâmetro alfa
  Property Alpha: Double read FAlpha write FAlpha;
  // Atribui/lê valor do parâmetro beta
  Property Beta: Double read FBeta write SetBeta;
end;

{ ============================== TwsProbStdWeibull ========================== }
{ Herança
    TwsProbStdWeibull --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição de Weibull padronizada
}
TwsProbStdWeibull = Class(TwsProbCont)
Private
  aux,               // Valor auxiliar
  FAlpha: Double;    // parametro de formato
  // Atribui valor ao parâmetro alfa
  procedure SetAlpha(const a: Double);
  // Gera valor com distribuicao Weibull
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição Weibull
  constructor Create(const a: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na Weibull
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na Weibull
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da Weibull
  function Density(const x: Double): Double; override;
  // Medidas descritivas da Weibull
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem inversa para grafico de probabilidade
//  function QQInv(k,n: integer; const c: Double): Double; override;
  // Atribui/lê valor do parâmetro alfa
  property Alpha: Double read FAlpha write SetAlpha;
end;

{ ============================== TwsProbWeibull ========================== }
{ Herança
    TwsProbWeibull --> TwsProbStdWeibull --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição de Weibull com dois parâmetros
}
TwsProbWeibull = Class(TwsProbStdWeibull)
Private
  FLam    : Double;  // parametro de dispersao
  // Atribui valor ao parâmetro Lambda
  procedure SetLam(const b: Double);
  // Gera valor com distribuicao Weibull
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição Weibull
  constructor Create(const a, b: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na Weibull
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na Weibull
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da Weibull
  function Density(const x: Double): Double; override;
  // Medidas descritivas da Weibull
  procedure Descriptive(var Res: TwsGeneral); override;
  // Grafico de probabilidade
//  function QQData(x: TwsVec; const c: Double=0.375): TwsVec; override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Executa teste de Kolmogorov-Smirnov
  function KSTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral; override;
  // Executa o teste de ajustamento de Anderson-Darling
  function ADTest(x: TwsVec; var VC: TwsGeneral):  TwsGeneral; override;
  // Atribui/lê valor do parâmetro Lambda
  property Lambda: Double read FLam write SetLam;
end;

{ ============================== TwsProbWeibull3 ========================== }
{ Herança
    TwsProbStdWeibull --> TwsProbWeibull --> TwsProbWeibull3 --> TwsProbCont --> TwsProb
      --> TObject
  Objetivo
    Implementa a distribuição de Weibull com três parâmetros
}

TwsProbWeibull3 = Class(TwsProbWeibull)
Private
  FTheta    : Double;  // parametro de deslocamento
  // Atribui valor ao parâmetro Theta
  procedure SetTheta(const t: Double);
  // Gera valor com distribuicao Weibull
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição Weibull3
  constructor Create(const t,a,b: Double); overload;
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na Weibull3
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na Weibull3
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da Weibull3
  function Density(const x: Double): Double; override;
  // Medidas descritivas da Weibull3
  procedure Descriptive(var Res: TwsGeneral); override;
  // Grafico QQ
//  function QQInv(k,n: integer; const c: Double): Double;

  // Grafico de probabilidade
//  function QQData(x: TwsVec; const c: Double=0.375): TwsVec; override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Atribui/lê valor do parâmetro Theta
  property Theta: Double read FTheta write SetTheta;
end;

{ ============================== TwsProbStdGumbel ========================== }
{ Herança
    TwsProbStdGumbel --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição de Gumbel padronizada
}

TwsProbStdGumbel = Class(TwsProbCont)
Private
  // Gera valor com distribuicao Gumbel
  function GetRandValue: Double; override;
Public
  constructor Create; overload;
  // Cálculo de probabilidade na Gumbel
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na Gumbel
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da Gumbel
  function Density(const x: Double): Double; override;
  // Medidas descritivas da Gumbel
  procedure Descriptive(var Res: TwsGeneral); override;
  // Inversa da probabilidade empírica para gráfico de probabilidade
//  function QQInv(k,n: integer; const c: Double): Double; override;
end;

{ ============================== TwsProbGumbel ========================== }
{ Herança
    TwsProbGumbel --> TwsProbStdGumbel --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição de Gumbel dois parâmetros
}

TwsProbGumbel = Class(TwsProbStdGumbel)
Private
  // Parâmetros da distribuição
  FGamma,FLam: Double;
  // Atribui valor ao parâmetro Theta
  procedure SetGamma(const a: Double);
  // Atribui valor ao parâmetro Lambda
  procedure SetLam(const b: Double);
  // Gera valor com distribuicao Gumbel
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição Gumbel
  constructor Create(const a, b: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na Gumbel
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na Gumbel
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da Gumbel
  function Density(const x: Double): Double; override;
  // Medidas descritivas da Gumbel
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtem estimativa pelo método dos momentos
  function MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec; override;
  // Periodo de retorno pelo metodo dos momentos
  procedure MoReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Atribui/lê valor do parâmetro Theta
  property Gamma: Double read FGamma write SetGamma;
  // Atribui/lê valor do parâmetro Lambda
  property Lambda: Double read FLam write SetLam;
end;

{ ============================== TwsProbGVE ========================== }
{ Herança
    TwsProbGVE --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição de extremos generalizada (GVE) (três parâmetros)
    GVE - Generalized Extreme Distribution
}

TwsProbGVE = Class(TwsProbCont)
Private
  aux,aux1,aux2: Double;  // valores auxiliares
  // Parâmetros da distribuição
  FGamma,           // tendencia central
  FLam,             // dispersao
  FAlpha: Double;   // formato
  // Atribui valor ao parâmetro gamma
  procedure SetGamma(const a: Double);
  // Atribui valor ao parâmetro Lambda
  procedure SetLam(const b: Double);
  // Atribui valor ao parâmetro Alpha
  procedure SetAlpha(const a: Double);
  // Gera valor com distribuicao de extremos generalizada
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição de extremos generalizada
  constructor Create(const a, b, c: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na de extremos generalizada
  function Prob(const x: Double): Double; override;
  // Cálculo de Quantis (inversas) na de extremos generalizada
  function Quantil(p: Double): Double; override;
  // Valor da função de densidade da de extremos generalizada
  function Density(const x: Double): Double; override;
  // Medidas descritivas da de extremos generalizada
  procedure Descriptive(var Res: TwsGeneral); override;
  // Estimativa por maxima verossimilhanca
  function MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer; const epsl: Double=1E-7;
    UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec; override;
  // Obtem periodos de retorno para tempos informados por F
  procedure ReturnPeriods(x: TwsVec; F: TwsGeneral); override;
  // Atribui/lê valor do parâmetro Alpha
  property Alpha: Double read FAlpha write SetAlpha;
  // Atribui/lê valor do parâmetro Theta
  property Gamma: Double read FGamma write SetGamma;
  // Atribui/lê valor do parâmetro Lambda
  property Lambda: Double read FLam write SetLam;
end;

{========================== TwsProbDisc ======================= }

{ Herança
    TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Classe básica para implementação de distribuições discretas. Não pode ser utilizada
    diretamente.
}

TwsProbDisc = class(TwsProb)
private
  // Probabilidades acumuladas e em cada ponto
  FAccum,
  FProbVal: TwsDFVec;
  // Obtém valor de probabilidade acumulada
  function GetProbAccum(i: Integer): Double;
  // Obtém valor de probabilidade
  function GetProb(i: Integer): Double;
  // Matriz com as probabilidades acumuladas e no ponto
  function GetDistrib(x: Double): TwsGeneral;
  // Libera espaço ocupado pelos campos
  destructor Destroy; override;
Public
  // Inicializa campos deste objeto
  constructor Create;
  // Obtém probabilidade acumulada
  function ProbAcum(x: Double): Double;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; virtual; abstract;
  // Matriz com as probabilidades acumuladas e no ponto
  function Distrib(x: Double = 0): TwsGeneral; virtual; abstract;
  // Obtém valor da probabilidade
  property ProbVal[Index: Integer]: Double read GetProb;
  // Obtém valor da probabilidade acumulada
  property Accum[Index: Integer]: Double read GetProbAccum;
end;

{ ============================== TwsProbPoisson ========================== }
{ Herança
    TwsProbPoisson --> TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição de Poisson
}

TwsProbPoisson = class(TwsProbDisc)
Private
  // Parâmetro da distribuição
  FLambda: Double;
  // Atribui valor ao parâmetro
  procedure SetLam(const p: Double);
  // Gera valor com distribuicao Poisson
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição
  constructor Create(const ALam: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; override;
  // Cálculo de probabilidade na Poisson
  function Prob(const x: Double): Double; override;
  // Obtém matriz com valores da distribuição de Poisson
  function Distrib(x: Double = 0): TwsGeneral; override;
  // Medidas descritivas da Poisson
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/le parâmetro da Poisson
  property Lambda: Double read FLambda write SetLam;
end;

{ ============================== TwsProbBernoulli ========================== }
{ Herança
    TwsProbBernoulli --> TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição de Bernoulli
}

TwsProbBernoulli = class(TwsProbDisc)
Private
  // Parâmetro da distribuição
  FProbSuc: Double;
  // Atribui valor ao parâmetro
  procedure SetProbSuc(const p: Double);
  // Gera valor com distribuicao Bernoulli
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição de Bernoulli
  constructor Create(const p: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na Bernoulli
  function Prob(const x: Double): Double; override;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; override;
  // Obtém matriz com valores da distribuição de Bernoulli
  function Distrib(x: Double = 0): TwsGeneral; override;
  // Medidas descritivas da Bernoulli
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/le valor da probabilidade de sucesso
  property ProbSuc: Double read FProbSuc write SetProbSuc;
end;

{ ============================== TwsProbBinomial ========================== }
{ Herança
    TwsProbBinomial --> TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição Binomial
}

TwsProbBinomial = class(TwsProbDisc)
Private

  q,           // 1-Fp
  Fp: Double;  // Probabilidade de sucesso
  // Número de experimentos
  Fn: Integer;
  // Atribui valor da probailidade de sucesso
  procedure SetProbSuc(const p: Double);
  // Atribui valor do número de experimentos
  procedure SetNumExp(nn: Integer);
  // Gera valor com distribuicao binomial
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição binomial
  constructor Create(nn: Integer; const p: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na binomial
  function Prob(const x: Double): Double; override;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; override;
  // Obtém matriz com valores da distribuição Binomial
  function Distrib(x: Double = 0): TwsGeneral; override;
  // Medidas descritivas da binomial
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/le probabilidade de sucesso
  property ProbSuc: Double read Fp write SetProbSuc;
  // Atribui/le número de experimentos
  property NumExp: Integer read Fn write SetNumExp;
end;

{ ============================== TwsProbHipergeométrica ========================== }
{ Herança
    TwsProbHyperGeom --> TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição hipergeométrica
}

TwsProbHyperGeom = class(TwsProbDisc)
Private
  // Parâmetros da distribuição
  Fm,Fn,Fp: Integer;
  // Calcula probabilidades
  Function GetDistrib(x : Double): TwsGeneral;
  // Atribui valor ao tamanho do grupo de sucessos
  procedure SetSucSize(p: Integer);
  // Atribui valor ao tamanho da amostra
  procedure SetSampleSize(nn: Integer);
  // Atribui valor ao tamanho da população
  procedure SetPopSize(n: Integer);
  // Gera valor com distribuicao hipergeometrica
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição para distribuição hipergeométrica
  constructor Create(N, nn, N1: Integer); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Obtém matriz com valores da distribuição Hipergeométrica
  function Distrib(x: Double = 0): TwsGeneral; override;
  // Cálculo de probabilidade na hipergeométrica
  function Prob(const x: Double): Double; override;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; override;
  // Medidas descritivas da hipergeométrica
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtém probabilidade acumulada
  function PAccum(x : Integer): Double;
  // Atribui/lê tamanho do grupo de sucessos
  property SucSize: Integer read Fm write SetSucSize;
  // Atribui/lê tamanho da amostra
  property SampSize: Integer read Fn write SetSampleSize;
  // Atribui/lê tamanho da população
  property PopSize: Integer read Fp write SetPopSize;
end;

{ ============================== TwsProbBinNeg ========================== }
{ Herança
    TwsProbBinNeg --> TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição Binomial Negativa
}

TwsProbBinNeg = Class(TwsProbDisc)
Private
  // Parâmetros da distribuição
  Fp, Fr: Double;
  // Atribui valor probabilidade de sucesso
  procedure SetProbSuc(const p: Double);
  // Atribui valor do número de sucessos
  procedure SetNumSuc(const r: Double);
  // Gera valor com distribuicao binomial negativa
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição binomial negativa
  constructor Create(const r, p: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na binomial negativa
  function Prob(const x: Double): Double; override;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; override;
  // Obtém matriz com valores da distribuição Binomial Negativa
  function Distrib(x: Double = 0): TwsGeneral; override;
  // Medidas descritivas da binomial negativa
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/lê valor para probabilidade de sucesso
  property ProbSuc: Double read Fp write SetProbSuc;
  // Atribui/lê valor para número de sucessos
  property NumSuc: Double read Fr write SetNumSuc;
end;

{ ============================== TwsProbGeom ========================== }
{ Herança
    TwsProbGeom --> TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição Geométrica
}

TwsProbGeometric = Class(TwsProbDisc)
Private
  // Probabilidade de sucesso
  Fp: Double;
  // Atribui valor para probabilidade de sucesso
  procedure SetPb(const p: Double);
  // Gera valor com distribuicao geometrica
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição geométrica
  constructor Create(const p: Double); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na geométrica
  function Prob(const x: Double): Double; override;
  // Obtém matriz com valores da distribuição Geométrica
  function Distrib(x: Double = 0): TwsGeneral; override;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; override;
  // Medidas descritivas da geométrica
  procedure Descriptive(var Res: TwsGeneral); override;
  // Atribui/lê valor da probabilidade de sucesso
  property ProbSuc: Double read Fp write SetPb;
end;

{ ============================== TwsProbUnidisc ========================== }
{ Herança
    TwsProbUniDisc --> TwsProbDisc --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição Uniforme Discreta
}

TwsProbUniDisc = class(TwsProbDisc)
Private
  // Limite inferior
  FAlpha,
  // Limite superior
  FBeta  : Integer;
  // Obtém probabilidades
  Function GetDistrib(x : integer) : TwsGeneral;
  // Atribui valor do LI
  procedure SetAlpha(p : Integer);
  // Atribui valor do LS
  procedure SetBeta(p : Integer);
  // Gera valor com distribuicao uniforme discreta
  function GetRandValue: Double; override;
Public
  // Cria objeto para distribuição Uniforme discreta
  constructor Create(Fa,Fb: Integer); overload;
  // Cria distribuicao sem atribuir valores aos parametros
  constructor Create; overload;
  // Atribui valores aos parametros
  procedure SetPar(P: array of Double); override;
  // Cálculo de probabilidade na uniforme discreta
  function Prob(const x: Double): Double; override;
  // Retorna vetor com as probabilidades
  function VecProb(const x: Double): TwsVec; override;
  // Obtém matriz com valores da distribuição Uniforme discreta
  function Distrib(x: Double = 0): TwsGeneral; override;
  // Medidas descritivas da uniforme discreta
  procedure Descriptive(var Res: TwsGeneral); override;
  // Obtém probabilidade acumulada
  function PAccum(x : Double): Double;
  // Atribui/lê limite inferior
  property Alpha: Integer read FAlpha write SetAlpha;
  // Atribui/lê limite superior
  property Beta : Integer read FBeta write SetBeta;
end;


{ ============================== TwsProbProbUCont ========================== }
{ Herança
    TwsProbUCont --> TwsProb --> TObject
  Objetivo
    Implementa uma distribuição contínua definida pelo usuário através de uma expressão e
    os limites dos valores da variável que definem uma função de densidade
}
  TwsProbUCont = class(TwsProbCont)
  private
    // Cálculo da integral
    FIntegral: TwsIntegral;
    // Avaliador da expressão
    FAvaliador: TAvaliator;
    // Expressão para a função de densidade
    FExpr: String;
    // Limite inferior do intervalo de definição da variável
    FLL: Double;
    // Limite superior do intervalo de definição da variável
    FHL: Double;
    // Atribui constante para erro e emite mensagem
    procedure SetErro(num: word; const p: Double);
    Procedure SetSubLimit(const LowLimit, HighLimit: Double);
    // Libera espaço ocupado pelo objeto
    Destructor  Destroy; override;
  public
    // Cria objeto para distribuição contínua do usuário
    Constructor Create(Const Expr: string; const LowLimit, HighLimit: Double);
    // Funçao de distribuiçao
    Function  Prob(const H : Double) : Double; override;
    // Valor da função de densidade
    Function  Density(const XValue : Double) : Double;  override;
    // Vetor de valores da densidade
    function VecDensity(x1,x2: Double; n: Integer; Var xVec: TwsVec): TwsVec;
    // Medidas descritivas da distribuição contínua definida
    procedure Descriptive(var Res: TwsGeneral);  override;
    // Calcula probabilidade no intervalo
    function ProbInt(const L,H : Double): Double;

    // Atribui/lê limite inferior
    Property  LowLimit     : Double  read FLL   write FLL;
    // Atribui/lê limite superior
    Property  HighLimit    : Double  read FHL   write FHL;
    // Atribui/lê expressão da função de densidade
    Property  Expressao    : String read FExpr write FExpr;
  end;


{ ============================== TwsProbProbUDisc ========================== }
{ Herança
    TwsProbUDisc --> TwsProbDisc --> TObject
  Objetivo
    Implementa uma distribuição discreta definida pelo usuário através de uma matriz que
    contém na primeira linha os valores da variável discreta e na segunda as probabilidades
    correspondentes.
}

  TwsProbUDisc = Class(TwsProbDisc)
  private
    FProbMat  : TwsGeneral;
    Destructor  Destroy; override;
  public
    Constructor Create(PMat: TwsGeneral);
  // Cálculo de probabilidade na discreta definida
    Function Prob(const x: Double): Double; override;
    Function ProbAcum(XValue: Double): Double;
  // Medidas descritivas da discreta definida
    procedure Descriptive(var Res: TwsGeneral); override;
  // Obtém matriz com valores da distribuição discreta definida
    property Distrib: TwsGeneral read FProbMat;
  end;

// Rotinas gerais

 function DiscDescriptive(x,p: TwsVec): TwsGeneral;

 function KSProb(const d: Double): Double;

implementation
Uses Math,
     wsFuncoesDeProbabilidade,
     wsMath,
     wsGraficos,
     Dialogs;

{ ============================ TwsProb ========================= }

constructor TwsProb.Create;
{ Objetivo
    Classe básica a todas as demais classes que implementam distribuições de probabilidade.
    Não pode ser utilizada diretamente
  Métodos chamados
    Create herdado
  Campos modificados
    FUpper
}
begin
  inherited Create;
  FUpper:=false;
  FnParEst:=0;
end;

destructor TwsProb.Destroy;
begin
  if Assigned(FRand) then
    FRand.Free;
  if Assigned(FV) then
    FV.Free
end;

Procedure TwsProb.SetErro(num : word; const p: Double);
{ Objetivo
    Emite mensagem de erro
  Parâmetros
    num: código de erro
    p: valor associado. Supõe-se que o valor de p é a condição de erro
}
var
  msg: String;
Begin
  case num of
   0          : msg := 'Valor de probabilidade fora da amplitude (0, 1)';
   1          : msg := 'Valor da variável fora da amplitude permitida';
   2          : msg := 'Valor do parâmetro inválido';
   IntProbI   : msg:='A integral I foi calculada como 1-I''';
   PrecIntProb: msg:='A integral calculada como 1-I'' causou perda de precisão';
   DigSignif  : msg:='Houve perda de dígitos significativos';
   IntProb2I  : msg:='A integral foi calculada como 1-2I''';
   AproxNormal: msg:='Foi utilizada a aproximação normal';
   Aprox      : msg:='Foi utilizada uma aproximação';
   ValNNulo   : msg:='Foi utilizada uma aproximação';
   Inter3Pont : msg:='Foi usada fórmula de Interpolação Harmônica de 3 pontos';
   PontPercInv: msg:='Ponto percentual pode estar incorreto';
   InterPont  : msg:='Ponto percentual pode estar incorreto';
  end;

  if not wsGLib.IsMissValue(p) then
     Msg := msg + #13#13'Valor = ' + FloatToStrF(p,ffGeneral,12,7);

  raise ERangeError.Create(msg);
end;

{==========================Continuas=============================}

constructor TwsProbCont.Create;
{ Objetivo
    Cria um objeto ancestral a todos os objetos para tratamento de distribuições contínuas
  Observações
    Inicializa a propriedade Eps como 1.0E-15
}
Begin
  inherited Create;
  Feps:=1e-15;
end;

procedure TwsProbCont.SetEps(const y: Double);
{ Objetivo
    Atribui valor da precisão
}
begin
  Feps:=y;
end;

procedure TwsProbCont.GetYCoord(var yMin,yMax: Double);
begin
//
end;
(*
function TwsProbCont.QQInv(k,n: integer; const c: Double): Double;
begin
  Result:=Quantil((k-c)/(n-2*c+1))
end;
*)

function TwsProbCont.QQData(var x, p: TwsVec; const c: Double=0.375): TwsVec;
{ Objetivo
    Obtém os valores dos quantis teóricos da distribuição
  Parâmetros
    x: Vetor com os dados observados
    c: Valor da constante para obtenção das probabilidades empíricas
  Retorno
    Vetor com os quantis da exponencial. Os valores de x retornam ordenados
}
var
  k,n: Integer;
begin
  x.QuickSort(True);            // Esta ordem deve ser ascendente
  n:=x.Len;
  p := TwsDFVec.Create(n);
  Result := TwsDFVec.Create(n);
  // Obtem as inversas de acordo com cada distribuicao
  for k := 1 to n do
    begin
    p[k]:=(k-c)/(n-2*c+1);
    Result[k] := Quantil(p[k])
    end
end; // QQData

procedure TwsProbCont.GetLimits(var xInf,xSup: TwsVec; n: integer);
{ Objetivo
    Obtém, via simulação, valores para os limites dos envelopes inferior e superior
    utilizados no gráfico de probabilidades
  Parâmetros
    xInf: Vetor com os limites inferiores
    xSup: Vetor com os limites superiores
    Dist: Distribuição para a geração dos dados
    n   : Número de observações
}
var
  S    : TwsGeneral;
  i,k  : Integer;
  xi,xm: double;
begin
  S:=TwsGeneral.Create(0,n);
  // Gera a tabela 19 x n (S)
  for i:=1 to 19 do
    begin
    // Gera a amostra, ordena e insere na tabela
    xSup:=TwsDFVec.Create(n);
    for k:=1 to n do
      xSup[k]:=RandValue;
    xSup.QuickSort(True);
    S.MAdd(xSup)
    end;
  // xSup contera os maximos de cada coluna
  xSup:=TwsDFVec.Create(n);
  xSup.Name:='LS_Env';
  // xInf contera os minimos de cada coluna
  xInf:=TwsDFVec.Create(n);
  xInf.Name:='LI_Env';
  for i:=1 to S.NCols do
    begin
    S.ColExtrems(i,xi,xm);
    xInf[i]:=xi;
    xSup[i]:=xm
    end;
  S.Free;
end; // GetLimits

function TwsProbCont.QQPlot(vData: TwsVec; Null,Env: Boolean; const c: Double;
  var S: TwsGeneral): TfoChart;
{ Objetivo
    Construir gráfico dos quantis observados (variável com valores ordenados) contra os
    quantis teóricos de uma distribuição de probabilidades. Os parâmetros de origem e
    dispersão não necessitam ser informados. Parâmetros de formatos necessitarão de valores
    para a obtenção dos quantis teóricos. Assim, se não forem estimados, necessitarão ser
    especificados. A construção de um formato nulo e de um envelope por simulação também
    poderá ser feita.
  Parâmetros
    vData  : Valores das observações da variável
    Null   : True se formato nulo vai ser construido
    Env    : True para construção do envelope para o gráfico.
    c      : Valor da constante para obtenção dos quantis teóricos
    S      : Matriz geral que retorna nas linhas as quantidades utilizadas na construção do
             gráfico
}
var
  xData,p,xm,
  ym,xSup,xInf: TwsVec;
  xmin,xmax,
  ymin,ymax   : double;
  i,k,n,Err   : Integer;
begin
  // Clona vData pois ele sera modificado e inserido na matriz S
  vData := vData.Clone();
  // Cria xData. vData volta ordenado. Na realidade, QQData nao utiliza o valor do parametro
  n:=vData.Len;
  xData:=QQData({var} vData, {var} p, c);

  S:=TwsGeneral.Create(0,n);
  S.Name:='Quant_Est';
  S.MLab:='Quantis estimados e períodos de retorno';
  S.MAdd(vData);

  // Constroi envelope de simulacao?
  if Env then
     begin
     GetLimits({out} xInf, {out} xSup, n); // Cria limites para o envelope
     S.MAdd(xInf);
     S.MAdd(xSup)
     end;

  // Se o formato nulo vai ser incluido
  if Null then
    begin
    // reta vai passar por q1 e q3 de y
    ym:=TwsDFVec.Create([0.25,0.75]);
    vData.Percentis(ym);
    xm:=TwsDFVec.Create([0.25,0.75]);
    // reta vai passar por q1 e q3 de x
    xData.Percentis(xm);
    // minimo x
    xmin:=xData.MinOrMax(True);
    ymin:=xmin*(ym[2]-ym[1])-xm[1]*ym[2]+xm[2]*ym[1];
    ymin:=ymin/(xm[2]-xm[1]);

    // maximo de x
    xmax:=xData.MinOrMax(False);
    ymax:=xmax*(ym[2]-ym[1])-xm[1]*ym[2]+xm[2]*ym[1];
    ymax:=ymax/(xm[2]-xm[1]);

    Result := ws_MatrixPlotTendency(xData,S,stPoint,xmin,ymin,xmax,ymax,Err);
    xm.Free; ym.Free;
    end
  else
    Result := ws_MatrixPlot(xData,S,stPoint,Err);

  // Insere xData como 2. linha de S
  xData.Name := 'Quantis';
  S.MInsert(2, xData);

  p.Name:='Pr_Acum';
  S.MAdd(p);

  xInf:=TwsDFVec.Create(n);
  xInf.Name:='T_Retorno';
  for i:=1 to n do
    xInf[i]:=1/p[i];
  S.MAdd(xInf);

  if Err = 0 then
     Result.Chart.LeftAxis.Title.Caption:= vData.Name
  else
     raise Exception.Create('Erro na geração dos gráficos: Código ' + intToStr(Err));
end; { QQPlot }

function TwsProbCont.ChiSqrTest(x: TwsVec; var Res: TwsGeneral; nc: Integer=0;
  p: TwsVec=nil): TwsGeneral;
{ Objetivo
    Aplica o teste qui-quadrado para verificar a aderencia à distribuição proposta
  Parâmetros
    x  : Vetor com os dados. Retorna com os valores ordenados em ordem ascendente
    Res: Matriz com as quantidades intermediárias envolvidas no teste. O conteúdo das colunas
      é o seguinte:
      Coluna 1 - Limite superior do intervalo que define a classe
      Coluna 2 - Proporção esperada na classe
      Coluna 3 - Frequência observada na classe
      Coluna 4 - Frequência esperada na classe
      Coluna 5 - Parcela da estatística qui-quadrado
    nc - número de classes desejado. As seguintes situações são possíveis:
      - Se nc=0 (default) e p=nil (veja abaixo) o número de classes será calculado pela
        fórmula de Sturges
      - Se nc>0 e p=nil, este será o número de classes. Neste caso e no anterior, a proporção
        de observações esperada em cada classe será considerada constante e igual a 1/nc
    p - Vetor para indicação das proporções esperadas em cada classe. Se p=nil (default),
      nc indicará o número de intervalos e a proporção em cada classe será constante. Se
      p<>nil, então cada elemento do vetor indicará uma proporção esperada para a classe
      respectiva. O número de classes será a dimensão do vetor. Evidentemente, a soma dos
      elementos de p deve ser igual a 1. Isto não é checado pelo método.
  Retorno
    Retorna uma matriz com o valor da estatística, o número de graus de liberdade e o valor
    p associado
}
var
  i,j,k,n    : Integer;
  v          : TwsVec;
  aux,aux1,q,
  aux2       : Double;
  Err        : Word;
begin
  x.QuickSort(True);
  n:=x.Len;
  // Se nc for nulo, obtem numero de intervalos pela regra de Sturges
  if p=nil then
    begin
    if nc=0 then
      nc:=Trunc(1+3.32*log10(n))+1
    end
  else
    nc:=p.Len;
  Res:=TwsGeneral.Create(0,5);
  Res.PrintOptions.ColPrecision:=6;
  Res.Name:='QQ_Result';
  Res.MLab:='Informações sobre o Teste Qui-Quadrado';
  Res.ColName[1]:='Lim_Sup'; Res.ColName[2]:='Prop_Esp'; Res.ColName[3]:='F_Obs';
  Res.ColName[4]:='F_Esp'; Res.ColName[5]:='Q';
  j:=1;
  q:=0;
  aux1:=n/nc;
  aux2:=0;
  // Trata ate a penultima classe
  for i:=1 to nc-1 do
    begin
    v:=TwsDFVec.Create(5);
    v.Name:='Classe_'+intToStr(i);
    if p=nil then
      aux2:=i/nc                // Proporcao acumulada
    else
      aux2:=aux2+p[i];
    aux:=Quantil(aux2);
    v[1]:=aux;                 // Limite superior
    v[2]:=aux2;
    k:=0;
    while ((x[j]<aux) and (j<=n)) do
      begin
      Inc(k);
      Inc(j)
      end;
    v[3]:=k;                   // Frequencia observada
    if p<>nil then
      aux1:=n*p[i];
    v[4]:=aux1;
    v[5]:=Sqr(k-aux1)/aux1;    // Parcela qui-quadrado
    q:=q+v[5];                  // Valor da estatistica
    Res.MAdd(v);
    end;
  // Trata a ultima classe
  k:=0;
  v:=TwsDFVec.Create(5);
  v.Name:='Classe_'+intToStr(nc);
  v[1]:=wscMissValue;
  v[2]:=1;
  while ((j<=n) and (x[j]>=aux)) do
    begin
    Inc(k);
    Inc(j)
    end;
  v[3]:=k;
  if p<>nil then
    aux1:=n*p[nc];
  v[4]:=aux1;
  v[5]:=Sqr(k-aux1)/aux1;    // Parcela qui-quadrado
  q:=q+v[5];                  // Valor da estatistica
  Res.MAdd(v);

  // Resultados do teste
  Result:=TwsGeneral.Create(1,3);
  Result.Name:='QQ_Teste';
  Result.MLab:='Resultados do Teste Qui-Quadrado Para Ajustamento';
  Result.ColName[1]:='Estatistica'; Result.ColName[2]:='Graus_Lib';
  Result.ColName[3]:='p';
  Result.RowName[1]:='Valor';
  Result[1,1]:=q;
  Result[1,2]:=nc-nParEst;
  Result[1,3]:=X2Int(q,Result[1,2],True,Err);
end; // ChiSqrTest

function TwsProbCont.KSD(x: TwsVec): TwsGeneral;
{ Objetivo
    Obter a estatística D para o teste de Kolmogorv-Smirnov. Este método utiliza o método
    virtual Prob para obter os valores da função de distribuição relativos a cada valor de
    x, que é ordenado no processo.
    Ver Law & Kelton, pág. 387
  Parâmetros
    x: Valores da variável
  Resultado
    Matriz 1 x 3 com a estatística Dmax na primeira posição. O ajuste da estatística e a
    obtenção da probabilidade ou valor crítico associados são obtidos pelos descendentes
    que completam as posições desta matriz
}
var
  i,n      : Integer;
  aux,dmax,
  fn,d,fo  : Double;
begin
  n:=x.Len;
  x.QuickSort(True);
  dmax:=0; fo:=0;
  for i:=1 to n do
    begin
    aux:=Prob(x[i]);
    fn:=i/n;
    d:=wsGLib.MaxF(fn-aux,aux-fo);
    if dmax<d then
      dmax:=d;
    fo:=fn
    end;
  Result:=TwsGeneral.Create(1,3);
  Result.Name:='KSTest';
  Result.MLab:='Teste de Kolmogorov-Smirnov';
  Result.ColName[1]:='Estatistica'; Result.ColName[2]:='Estat_Ajust';
  Result.ColName[3]:='p';
  Result.RowName[1]:='Valor';
  Result[1,1]:=dmax
end; // KSD

function TwsProbCont.ADTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Obter a estatística A para o teste de Anderson-Darling. Este método utiliza o método
    virtual Prob para obter os valores da função de distribuição relativos a cada valor de
    x, que é ordenado no processo.
    Ver Law & Kelton, pág. 392
  Parâmetros
    x : Valores da variável
    VC: Retorna uma matriz com os valores críticos do teste. Esta matriz contém os valores
      críticos para o caso em que todos os parâmetros são conhecidos e, se disponível, valores
      críticos para distribuições específicas, onde os parâmetros são estimados
  Resultado
    Matriz 1 x 3 com a estatística A na primeira posição. O ajuste da estatística e a
    obtenção da probabilidade ou valor crítico associados são obtidos pelos descendentes
    que completam as posições desta matriz
}
var
  i,n,n1: Integer;
  a2    : Double;
  z     : TwsVec;
begin
  n:=x.Len;
  x.QuickSort(True);
  z:=TwsDFVec.Create(n);
  for i:=1 to n do
    z[i]:=Prob(x[i]);
  a2:=0;
  n1:=n+1;
  for i:=1 to n do
    a2:=a2+(2*i-1)*(Ln(z[i])+Ln(1-z[n1-i]));
  z.Free;
  a2:=-a2/n-n;
  Result:=TwsGeneral.Create(1,2);
  Result.Name:='ADTest';
  Result.MLab:='Resultado do Teste de Anderson-Darling';
  Result.ColName[1]:='Estatistica'; Result.ColName[2]:='Estat_Ajust';
  Result.RowName[1]:='Valor';
  Result[1,1]:=a2; Result[1,2]:=wscMissValue;
  VC:=TwsGeneral.Create(2,4);
  VC.Name:='Distrib'; VC.MLab:='Valores Críticos para Estatística Ajustada Anderson-Darling';
  VC.ColName[1]:='p1'; VC.ColName[2]:='p2'; VC.ColName[3]:='p3'; VC.ColName[4]:='p4';
  VC.RowName[1]:='1-Alfa'; VC[1,1]:=0.9; VC[1,2]:=0.95; VC[1,3]:=0.975; VC[1,4]:=0.99;
  VC.RowName[2]:='Par_Conhec'; VC[2,1]:=1.933; VC[2,2]:=2.492; VC[2,3]:=3.07; VC[2,4]:=3.857;
end; // ADTest

function TwsProbCont.KSTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Completar o teste de Kolmogorov-Smirnov, iniciado pelo método KSD. O valor da estatística
    Dmax, obtido através do método KSD, é ajustado obtido para cada situação (todos os parâmetros
    conhecidos e parâmetros estimados). O caso dos parâmetros estimados está disponível apenas
    para as distribuições normal, exponencial e Weibull. Para todas as demais distribuições, os
    parâmetros são considerados desconhecidos. O valor p associado vale apenas para o caso
    onde todos os parâmetros são conhecidos.
    Ver Law & Kelton, pág. 392
  Parâmetros
    x : Valores da variável
    VC: Retorna uma matriz com os valores críticos do teste. Esta matriz contém os valores
      críticos para o caso em que todos os parâmetros são conhecidos e, se disponível, valores
      críticos para distribuições específicas, onde os parâmetros são estimados
  Resultado
    Matriz 1 x 3 com a estatística A na primeira posição. O ajuste da estatística e a
    obtenção da probabilidade ou valor crítico associados são obtidos pelos descendentes
    que completam as posições desta matriz
}
var
  aux: Double;
begin
  Result:=KSD(x);
  aux:=sqrt(x.Len);
  Result[1,2]:=(aux+0.12+0.11/aux)*Result[1,1]; Result[1,3]:=KSProb(Result[1,2]);

  VC:=TwsGeneral.Create(2,5);
  VC.Name:='Distrib'; VC.MLab:='Valores Críticos para Estatística Ajustada Kolmogorov-Smirnov';
  VC.ColName[1]:='p1'; VC.ColName[2]:='p2'; VC.ColName[3]:='p3'; VC.ColName[4]:='p4';
  VC.ColName[5]:='p5';
  VC.RowName[1]:='1-Alfa'; VC[1,1]:=0.85; VC[1,2]:=0.9; VC[1,3]:=0.95;
  VC[1,4]:=0.975; VC[1,5]:=0.99;
  VC.RowName[2]:='Par_Conhec'; VC[2,1]:=1.138; VC[2,2]:=1.224; VC[2,3]:=1.358;
  VC[2,4]:=1.48; VC[2,5]:=1.628;
end;

function TwsProbCont.VecDensity(x1, x2: Double; n: Integer; Var xVec: TwsVec): TwsVec;
{ Objetivo
    Retorna um vetor com os valores da função de densidade da distribuição e um vetor com os
    valores das abcissas correspondentes
  Parâmetros
    x1, x2: Valor inicial e final. Se (x2<x1) ou se x1<0 os vetores resultantes são nil.
    n     : Número de valores para o cálculo. O vetor resultante tem dimensão n
    xvec  : Retorna um vetor com os valores das abscissas
}
var
  delta: Double;
  i    : Integer;
  hx   : Boolean;
begin
  if (x2>x1) then
    begin
    Result:= TwsDFVec.Create(n);
    if xVec=nil then
      begin
      xVec:= TwsDFVec.Create(n);
      delta:= (x2-x1)/n;
      xVec[1]:=x1;
      hx:=True
      end
    else
      hx:=false;
    for i := 1 to n-1 do
      begin
      Result[i] := Density(xVec[i]);
      if hx then
        xVec[i+1] := xVec[i]+delta;
      end;
    Result[n] := Density(xVec[n])
    end
  else
    Result:=nil
end;

{ =========================== TwsProbStdNormal =========================== }

constructor TwsProbStdNormal.Create;
begin
  // Cria gerador de valores aleatorios
  if (not Assigned(FRand)) then
    FRand:=TwsNormal01.Create;
  inherited Create;
  // Constante para uso repetido
  c1:=1/sqrt(2*pi);
end;

// Gera valor com distribuicao normal padrao
function TwsProbStdNormal.GetRandValue: Double;
begin
  Result:=TwsNormal01(FRand).Generate
end;

function TwsProbStdNormal.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
}
begin
  Result:=NInt(x,FUpper);
end; (* TwsProbStdNormal.Prob *)

function TwsProbStdNormal.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade p, retorna o valor correspondente da
    variavel normal padrao
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
var
   Erro : word;
begin
  if (p>0) and (p<1) then
    Result:=NInv(p,FEps,FUpper,Erro)
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end;
end; (* Quantil *)

Function TwsProbStdNormal.Density(const x: Double): Double;
{ Objetivo
    Obtém a ordenada da distribuição normal padrao
  Parâmetros
    x: Valor para obtenção da ordenada
}
Begin
  Result:= c1*Exp(-Sqr(x)/2)
End;(* Density *)

procedure TwsProbStdNormal.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição normal padrão
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(5);
  v.Name:='Valores';
  v[1]:=0; v[2]:=1; v[3]:=1; v[4]:=0; v[5]:=0;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,5);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='MD_NPadr';
    Res.MLab:='Medidas Descritivas - Normal Padrão';
    Res.ColName[1]:='Media'; Res.ColName[2]:='Variancia'; Res.ColName[3]:='Desv_Padrao';
    Res.ColName[4] := 'Assimetria'; Res.ColName[5] := 'Curtose';
    end;
  Res.MAdd(v);
end;

{ =========================== TwsProbHalfNormal =========================== }

// Cria objeto para distribuição semi-normal com parametro
constructor TwsProbHalfNormal.Create(const s: Double);
begin
  if (not Assigned(FRand)) then
    FRand:=TwsHalfNormal.Create;
  inherited Create;
  SetLam(s);
  // Redefine c1
  c1:=sqrt(2/pi);
end;

// Cria objeto para distribuição semi-normal sem parametro
constructor TwsProbHalfNormal.Create;
begin
  if (not Assigned(FRand)) then
    FRand:=TwsHalfNormal.Create;
  inherited Create;
  // Redefine c1
  c1:=sqrt(2/pi);
end;

procedure TwsProbHalfNormal.SetLam(const s: Double);
{ Objetivo
    Seta o valor do parâmetro de dispersão Lambda
  Parâmetros
    s: Valor do parâmetro
  Observações
    Erro: Se s<=0
}
begin
  if s>0 then
    begin
    // atualiza parametro
    FLam:=s;
    // atualiza parametro do gerador
    TwsHalfNormal(FRand).Lambda:=FLam
    end
  else
    SetErro(2,s)
end;

// Atribui valores aos parametros
procedure TwsProbHalfNormal.SetPar(P: array of Double);
begin
  SetLam(P[0])
end;

// Gera valor com distribuicao semi-normal
function TwsProbHalfNormal.GetRandValue: Double;
begin
  Result:=TwsHalfNormal(FRand).Generate
end;

// Cálculo de probabilidades na distribuição semi-normal
function TwsProbHalfNormal.Prob(const x: Double): Double;
begin
  Result:=2*(inherited Prob(FLam*x))-1
end;

// Medidas descritivas na distribuição semi-normal
procedure TwsProbHalfNormal.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição semi-normal
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1]:=FLam; v[2]:=c1/FLam; v[3]:=((pi-2)/pi)/Sqr(FLam); v[4]:=Sqrt(v[3]); v[5]:=2*c1;
  v[6]:=0;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Semi-Normal';
    Res.Name:='MD_SNor';
    Res.ColName[1]:='Lambda'; Res.ColName[2]:='Media'; Res.ColName[3]:='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v);
end;

// Função de densidade da distribuição semi-normal
function TwsProbHalfNormal.Density(const x: Double): Double;
begin
  Result:=FLam*c1*exp(-0.5*Sqr(FLam*x))
end;

function TwsProbHalfNormal.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da semi-normal através do método da máxima verossimilhança
  Parâmetros
    x: Vetor com os dados disponíveis. Em x estar deverá uma amostra aleatória de valores da
       variável de interesse.
    V: Retorna a matriz de covariâncias das estimativas.
    F: Na entrada, F deve possuir na primeira linha os valores dos tempos de retorno
       desejados para o calculo das magnitudes (valores das variáveis com o tempo de
       retorno especificado) desejadas. Na saída, a segunda linha contem as magnitudes e a
       terceira os erros padrões correspondentes.
    It: matriz que retorna o(s) valor(es) do(s) parâmetro(s) em cada passo do processo
      iterativo, se houver.
    Err: código de erro dom processo. Indica falta de convergencia, além de outras mensagens
    epsl: Precisão para verificação do processo iterativo. O valor default é 1e-7
    UpdatePar: Se True (valor default para entrada), as estimativas obtidas serão atribuídas
       aos parâmetros. Nesse caso, não há retorno de um vetor com as estimativas dos
       parâmetros. Se False, as estimativas retornam num vetor e os campos relativos aos
       parâmetros não são atualizados.
    MaxIter: número máximo de passos para o processo iterativo. Default é 25.
  Retorno: UpdatePar=False
    Retorna um vetor de duas posições:
      1 - Estimativa do parâmetro de dispersão
      2 - Número de observações presentes na amostra
}
var
  i,n  : Integer;
  sq   : Double;
begin
  sq:=0; n:=0;
  Err:=0;
  Result:=nil;
  for i:=1 to x.Len do
    if not wsGLib.IsMissValue(x[i]) and (x[i]>0) then
      begin
      sq:=sq+Sqr(x[i]);
      Inc(n)
      end;
  if n>0 then
    begin
    Result:=TwsDFVec.Create(2);
    Result[1]:=Sqrt(n/sq);
    Result[2]:=n;
    if UpdatePar then
      begin
      SetLam(Result[1]);
      nParEst:=1
      end
    end;
end;

procedure TwsProbHalfNormal.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;

// Obtem estimativa pelo método dos momentos
function TwsProbHalfNormal.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  ym: Double;
  n : Integer;
begin
  ym:=x.Mean(n);
  if n>0 then
    begin
    ym:=1/(ym*Sqrt(pi/2));
    if UpdatePar then
      begin
      SetLam(ym);
      nParEst:=1;
      Result:=nil
      end
    else
      begin
      Result:=TwsDFVec.Create(2);
      Result[1]:=ym;
      Result[2]:=n;
      end
    end
end;

// Obtem valores da função de log-verossimilhança
function TwsProbHalfNormal.LogLikelihood(x: TwsVec; Par: array of TwsDFVec): TwsGeneral;
{ Objetivo
    Obter os valores da função de log-verossimilhança da distribuição semi-normal
  Parâmetros
    x: Valores da amostra da variável X, supostamente com distribuição semi-normal
  Par: array com um vetor de três posições: na primeira deverá estar o menor valor desejado
    para o parâmetro, na segunda o maior e na terceira o acréscimo desejado para obtenção
    de cada valor a partir do mínimo.
  Retorno
    Matriz geral com uma linha e Trunc((lmax-lmin)/dx)+1 colunas, que contém os valores
    desejados da função de log-verossimilhança
}
var
  lmin,lmax,dx,
  sum,lx,aux   : Double;
  n,i          : Integer;
begin
  lmin:=Par[0][1];
  lmax:=Par[0][2];
  dx  :=Par[0][3];
  sum:=x.SumOfSq(n);
  aux:=0.5*n*Ln(2/pi);
  lx:=lmin;
  Result:=TwsGeneral.Create(2,Trunc((lmax-lmin)/dx)+1);
  for i:=1 to Result.NCols do
    begin
    Result[1,i]:=lx;
    Result[2,i]:=n*Ln(lx)+aux-0.5*Sqr(lx)*sum;
    lx:=lx+dx
    end;
end;

function TwsProbHalfNormal.Quantil(p: Double): Double;
{ Objetivo
    Obtém a inversa da distribuição semi-normal
  Parâmetros
    p: Probabilidade para obtenção da inversa
  Observações
    Se(p<=0 ou p>=1; Erro)
}
begin
  if (p>0) and (p<1) then
    Result:= (inherited Quantil(0.5+0.5*p))/FLam
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end
end; (* Quantil *)

{============================= TwsProbNormal ==========================}

constructor TwsProbNormal.Create(const m,s: Double);
{ Objetivo
    Cria um objeto para calculo de probabilidades da distribuição
    normal
  Parâmetros
    m: Valor da media
    s: Valor do desvio padrao. Uma mensagem de erro será gerada se s<0=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsNormal.Create;
  inherited Create;
  SetSigma(s);
  SetMean(m);
end;

constructor TwsProbNormal.Create;
{ Objetivo
    Cria um objeto para calculo de probabilidades da distribuição
    normal
  Parâmetros
    m: Valor da media
    s: Valor do desvio padrao. Uma mensagem de erro será gerada se s<0=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsNormal.Create;
  inherited Create;
end;

procedure TwsProbNormal.SetSigma(const s: Double);
{ Objetivo
    Seta o valor do parâmetro desvio padrao, acessado via
    propriedade Sigma
  Parâmetros
    s: Valor do desvio padrao
  Observações
    Erro: Se s<=0
}
begin
  if s>0 then
    begin
    FSigma:=s;
    c1:=1/(Sqrt(2*pi)*FSigma);
    TwsNormal(FRand).SDev:=FSigma
    end
  else
    SetErro(2,s)
end;

procedure TwsProbNormal.SetMean(const m: Double);
{ Objetivo
    Seta o valor do parâmetro média, acessado via propriedade Mean
  Parâmetros
    m: Valor da média
}
begin
  FMean:=m;
  TwsNormal(FRand).Mean:=m
end;

// Gera valor com distribuicao normal
function TwsProbNormal.GetRandValue: Double;
begin
  Result:=TwsNormal(FRand).Generate
end;

// Atribui valores aos parametros
procedure TwsProbNormal.SetPar(P: array of Double);
begin
  SetMean(P[0]);
  SetSigma(P[1])
end;

function TwsProbNormal.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
}
begin
  Result:= inherited Prob((x-FMean)/FSigma)
end; (* TwsProbNormal.Prob *)

function TwsProbNormal.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade p, retorna o valor correspondente da variavel normal
  Parâmetros
    p: Probabilidade para obtencao do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
var
  Erro: word;
begin
  if (p>0) and (p<1) then
    Result:= inherited Quantil(p)*FSigma+FMean
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end
end; (* Quantil *)

function TwsProbNormal.Density(const x: Double): Double;
{ Objetivo
    Ordenada da distribuição normal para o valor especificado
  Parâmetros
    x: Valor para o qual se deseja a ordenada
}
begin
  Result:= inherited Density((x-FMean)/FSigma)
end; (* Density *)

procedure TwsProbNormal.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição normal
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
    variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1]:=FMean;v[2]:=FSigma; v[3]:=FMean; v[4]:=Sqr(FSigma);v[5]:=FSigma; v[6]:=0; v[7]:=0;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Normal';
    Res.Name:='MD_Norm';
    Res.ColName[1] := 'Mi'; Res.ColName[2] := 'Sigma'; Res.ColName[3] := 'Media';
    Res.ColName[4] := 'Variancia'; Res.ColName[5] := 'Desv_Padrao';
    Res.ColName[6] := 'Assimetria'; Res.ColName[7] := 'Curtose';
    end;
  Res.MAdd(v)
end;

function TwsProbNormal.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da normal através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de três posições:
      1 - Estimativa do parãmetro de posição (média)
      2 - Estimativa do parâmetro de dispersão (desvio padrão)
      3 - Número de observações presentes na amostra
}
var
  m,s: double;
  n  : Integer;
begin
  Err:=0;
  x.VarMean(m,s,n);
  if n>1 then
    begin
    if UpdatePar then
      begin
      SetMean(m);
      SetSigma(Sqrt(s));
      nParEst:=2
      end
    else
      begin
      Result:=TwsDFVec.Create(3);
      Result[1]:=m;
      Result[2]:=Sqrt(s);
      Result[3]:=n;
      end;
    FV:=TwsSymmetric.Create(2);
    FV.Name:='Nor_Cov';
    FV.MLab:='Matriz de covariâncias para as estimativas dos parâmetros';
    FV.ColName[1]:='Media'; FV.ColName[2]:='D_Padrao';
    FV.RowName[1]:='Media'; FV.RowName[2]:='D_Padrao';
    FV[1,1]:=s/n; FV[2,2]:=2*Sqr(s)/n; FV[1,2]:=0
    end;
end; // MLE

procedure TwsProbNormal.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  v1,v2,p: TwsVec;
  i,k,n  : Integer;
  x1,a   : Double;
  Err    : Word;
begin
  if F <> nil then
    begin
    k:=F.nCols;
    n:=x.Len;
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';
    v1:=TwsDFVec.Create(k); v2:=TwsDFVec.Create(k);
    v1.Name:=x.Name; v2.Name:='Err_Padrao';
    a:=Sigma/Sqrt(n);
    for i:=1 to F.nCols do
      begin
      x1:=NInv(p[i],FEps,FUpper,Err);
      v1[i]:=Mean+x1*Sigma;
      v2[i]:=a*(1+sqr(x1)/2)
      end;
    F.MAdd(p);
    F.MAdd(v1); F.MAdd(v2)
    end
end;

// Obtem estimativa pelo método dos momentos
function TwsProbNormal.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  m,s: double;
  n  : Integer;
begin
  Result:=TwsDFVec.Create(2);
  x.VarMean(m,s,n);
  Result[1]:=m;
  Result[2]:=Sqrt(s);
  if UpdatePar then
    begin
    SetMean(Result[1]);
    SetSigma(Result[2]);
    nParEst:=2;
    end
end; // MomentEst

function TwsProbNormal.LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral;
{ Objetivo
    Obter os valores da função de log-verossimilhança da distribuição normal
  Parâmetros
    x: Valores da amostra da variável X, supostamente normalmente distribuída
  Par: array com dois vetores cada qual com três posições: na primeira deverá estar o menor
    valor desejado para o parâmetro de posição (média), na segunda o maior e na terceira o
    acréscimo desejado para obtenção de cada valor a partir do mínimo. O mesmo se repete
    para o vetor relativo ao parãmetro de dispersão (desvio padrão)
  Retorno
    Matriz geral com Trunc((tmax-tmin)/tx)+1 linhas e Trunc((lmax-lmin)/dx)+1 colunas, que
    contém os valores desejados da função de log-verossimilhança
}
var
  lmin,lmax,dx,
  tmin,tmax,tx,lpi,
  sum,LogL,lm,ls   : Double;
  n,k,i,il,it,j    : Integer;
begin
  // Parametro de posicao (media)
  tmin:=Par[0][1];
  tmax:=Par[0][2];
  tx  :=Par[0][3];
  // Parametro de dispersão (desvio padrao)
  lmin:=Par[1][1];
  lmax:=Par[1][2];
  dx  :=Par[1][3];
  // Numero de valores para a media
  it:=Trunc((tmax-tmin)/tx)+1;
  // Numero de valores para o desvio padrao
  il:=Trunc((lmax-lmin)/dx)+1;
  n:=x.Len;
  lpi:=n*0.5*ln(2*pi);
  Result:=TwsGeneral.Create(it,il);
  // Para cada valor da media
  lm:=lmin;
  for i:=1 to it do
    begin
    // menor valor para o desvio padrao
    ls:=tmin;
    // Para cada valor do desvio padrao
    for j:=1 to il do
      begin
      sum:=0;
      for k:=1 to n do
        sum:=sum+sqr((x[k]-lm)/ls);
      Result[i,j]:=-n*ln(ls)-lpi-0.5*sum;
      ls:=ls+tx;
      end;
    lm:=lm+dx
    end
end;

  // Obtem valores da função de log-verossimilhança
function TwsProbNormal.LogL(x: TwsVec): Double;
var
  i,n: Integer;
  s  : Double;
begin
  s:=0;
  n:=x.Len;
  for i:=1 to n do
    s:=s+Sqr((x[i]-FMean)/FSigma);
  Result:=-n*0.5*Ln(2*pi)-n*Ln(FSigma)-0.5*s
end;

function TwsProbNormal.KSTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Verifica, através do teste de Kolmogoro-Smirnov-Darling, se um conjunto de valores são
    valores de uma variável gaussiana.  Veja descrição em TwsProbCont.KSTest.
    Valores críticos específicos estão em Law & Kelton, pág. 390
  Parâmetros
    x: Vetor com os dados amostrais
}
var
  aux: Double;
  n  : Integer;
begin
  Result:=KSD(x);
  n:=x.Len;
  aux:=sqrt(x.Len);
  Result[1,2]:=(aux-0.01+0.85/aux)*Result[1,1]; Result[1,3]:=KSProb(Result[1,2]);
  Result.MLab:=Result.MLab+' Dist. Normal';
  VC:=TwsGeneral.Create(3,5);
  VC.Name:='Distrib'; VC.MLab:='Valores Críticos para Estatística Ajustada';
  VC.ColName[1]:='p1'; VC.ColName[2]:='p2'; VC.ColName[3]:='p3'; VC.ColName[4]:='p4';
  VC.ColName[5]:='p5';
  VC.RowName[1]:='1-Alfa'; VC[1,1]:=0.85; VC[1,2]:=0.9; VC[1,3]:=0.95;
  VC[1,4]:=0.975; VC[1,5]:=0.99;
  VC.RowName[2]:='Par_Conhec'; VC[2,1]:=1.138; VC[2,2]:=1.224; VC[2,3]:=1.358;
  VC[2,4]:=1.48; VC[2,5]:=1.628;
  VC.RowName[3]:='Par_Est'; VC[3,1]:=0.775; VC[3,2]:=0.819; VC[3,3]:=0.895;
  VC[3,4]:=0.955; VC[3,5]:=1.035;
end;

function TwsProbNormal.ADTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Verifica, através do teste de Anderson-Darling, se um conjunto de valores são valores de
    uma variável gaussiana. Valores críticos específicos estão em Law & Kelton, pág. 392
    Veja descrição em TwsProbCont.ADTest
}
begin
  Result:=inherited ADTest(x,VC);
  Result[1,2]:=(1+4/x.Len-25/Sqr(x.Len))*Result[1,1];
  VC.MAdd(TwsDFVec.Create([0.632,0.751,0.87,1.029]));
  VC.RowName[3]:='Par_Est'
end; // ADTest

{ =========================== TwsProbLogNormal =========================== }

// Cria objeto especificando parametros
constructor TwsProbLogNormal.Create(const LPar,APar: Double);
begin
  if (not Assigned(FRand)) then
    FRand:=TwsLognormal.Create;
  inherited Create(LPar,APar);
end;

// Cria objeto sem especificar parametros
constructor TwsProbLogNormal.Create;
begin
  if (not Assigned(FRand)) then
    FRand:=TwsLognormal.Create;
  inherited Create;
end;

// Gera valor com distribuicao lognormal
function TwsProbLogNormal.GetRandValue: Double;
begin
  Result:=TwsLognormal(FRand).Generate
end;

function TwsProbLogNormal.Prob(const x: Double): Double;
begin
  if x>0 then
    Result:= 0.5*(2-ErfComp((Ln(x)-FMean)/(1.41421356237*FSigma)))
  else
    Result:=0
end;

// Quantis na distribuição lognormal
function TwsProbLogNormal.Quantil(p: Double): Double;
begin
  Result:= exp(inherited Quantil(p))
end;

// Medidas descritivas na distribuição normal
procedure TwsProbLogNormal.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição lognormal. Como
    descendente, a classe utiliza as propriedades Mean e Sigma, mas elas nao são a média e
    o desvio padrão da lognormal.
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
         variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
  a1: Double;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  a1:=exp(Sqr(FSigma));
  v[1]:=FMean;v[2]:=FSigma; v[3]:=exp(FMean+0.5*Sqr(FSigma)); v[4]:=exp(2*FMean)*a1*(a1-1);
  v[5]:=Sqrt(v[4]); v[6]:=Sqrt(a1-1)*(2+a1); v[7]:=Sqr(a1)*(Sqr(a1)+2*a1+3)-6;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Lognormal';
    Res.Name:='MD_LNor';
    Res.ColName[1] := 'Lambda'; Res.ColName[2] := 'Alfa'; Res.ColName[3] := 'Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7] := 'Curtose';
    end;
  Res.MAdd(v)
end;

// Função de densidade da distribuição lognormal
function TwsProbLogNormal.Density(const x: Double): Double;
begin
  if x>0 then
    Result:=(c1/x)*exp(-0.5*Sqr((Ln(x)-FMean)/FSigma))
  else
    Result:=0
end;

function TwsProbLogNormal.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da lognormal através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de três posições:
      1 - Estimativa do parãmetro de dispersão
      2 - Estimativa do parâmetro de formato
      3 - Número de observações presentes na amostra
}
var
  wm,
  yv,x1: Double;
  v1,v2: TwsVec;
  i,n  : Integer;
begin
  Result:=nil;
  Err:=0;
  wm:=0; yv:=0; n:=0;         // media e variancia dos logaritmos
  for i:=1 to x.Len do        // pula valores perdidos, negativos ou nulos
    if (not wsGLib.IsMissValue(x[i]) and (x[i]>0)) then
      begin
      Inc(n);
      x1:=Ln(x[i])-wm;
      yv:=yv+((n-1)/n)*x1*x1;
      wm:=wm+x1/n;
      end;
  if n>0 then
    begin
    yv:=yv/(n-1);
    if UpdatePar then
      begin
      Mean:=wm;
      Sigma:=Sqrt(yv);
      nParEst:=2;
      end
    else
      begin
      Result:=TwsDFVec.Create(3);
      Result[1]:=wm;
      Result[2]:=yv;
      Result[3]:=n;
      end;
    FV:=TwsSymmetric.Create(2);
    FV[1,1]:=yv/n; FV[2,2]:=2*yv/n; FV[1,2]:=0;
    FV.Name:='LN_Cov';
    FV.MLab:='Matriz de covariâncias das estimativas';
    FV.ColName[1]:='Lambda'; FV.ColName[2]:='Alfa';
    FV.RowName[1]:='Lambda'; FV.RowName[2]:='Alfa';
    end
end; // MLE

procedure TwsProbLogNormal.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  v1,v2,p: TwsVec;
  i,k,n  : Integer;
  x1,a   : Double;
  Err    : Word;
begin
  if F <> nil then
    begin
    k:=F.nCols;
    n:=x.Len;
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';
    v1:=TwsDFVec.Create(k); v2:=TwsDFVec.Create(k);
    v1.Name:=x.Name; v2.Name:='Err_Padrao';
    a:=Sigma/Sqrt(n);
    for i:=1 to k do
      begin
      x1:=NInv(p[i],FEps,FUpper,Err);
      v1[i]:=exp(Mean+x1*Sigma);
      v2[i]:=a*Sqrt(Sqr(v1[i])*(1+sqr(x1)/2));
      end;
    F.MAdd(p);
    F.MAdd(v1); F.MAdd(v2)
    end
end;

// Obtem estimativa pelo método dos momentos
function TwsProbLogNormal.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  ym,x0,
  yv    : Double;
  n     : Integer;
begin
  x.VarMean(ym,yv,n);
  if n>1 then
    begin
    Result:=TwsDFVec.Create(3);
    x0:=Ln(1+yv/Sqr(ym));
    Result[1]:=Ln(ym)-0.5*x0;   // dispersao
    Result[2]:=x0;              // formato
    Result[3]:=n;
    if UpdatePar then
      begin
      SetMean(Result[1]);
      SetSigma(Result[2]);
      nParEst:=2
      end
    end
end; // MomentEst

// Obtem valores da função de log-verossimilhança
function TwsProbLogNormal.LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral;
{ Objetivo
    Obter os valores da função de log-verossimilhança da distribuição lognormal
  Parâmetros
    x: Valores da amostra da variável X, supostamente normalmente distribuída
  Par: array com dois vetores cada qual com três posições: na primeira deverá estar o menor
    valor desejado para o parâmetro de dispersao, na segunda o maior e na terceira o
    acréscimo desejado para obtenção de cada valor a partir do mínimo. O mesmo se repete
    para o vetor relativo ao parãmetro de formato
  Retorno
    Matriz geral com Trunc((tmax-tmin)/tx)+1 linhas e Trunc((lmax-lmin)/dx)+1 colunas, que
    contém os valores desejados da função de log-verossimilhança
}
var
  lmin,lmax,dx,lm,
  tmin,tmax,tx,lpi,
  sum,sum1,LogL,ls : Double;
  n,k,i,il,it,j    : Integer;
begin
  // Limites e incremento para o parametro de dispersao
  tmin:=Par[0][1];
  tmax:=Par[0][2];
  tx  :=Par[0][3];
  // Limites e incremento para o parametro de formato
  lmin:=Par[1][1];
  lmax:=Par[1][2];
  dx  :=Par[1][3];
  // Numero de valores para dispersao
  it:=Trunc((tmax-tmin)/tx)+1;
  // Numero de valores para formato
  il:=Trunc((lmax-lmin)/dx)+1;
  n:=x.Len;
  lpi:=0.5*ln(2*pi);
  sum1:=0;
  for i:=1 to n do
    if x[i]>0 then
      sum1:=sum1+Ln(x[i]);
  Result:=TwsGeneral.Create(it,il);
  // Para cada valor da dispersao
  lm:=lmin;
  for i:=1 to it do
    begin
    // menor valor para o formato
    ls:=tmin;
    // Para cada valor do formato
    for j:=1 to il do
      begin
      sum:=0;
      for k:=1 to n do
        sum:=sum+sqr((Ln(x[k])-lm)/ls);
      Result[i,j]:=-n*ln(ls)-lpi-sum1-0.5*sum;
      ls:=ls+tx;    // incrementa valor para o formato
      end;
    lm:=lm+dx       // incrementa valor para dispersao
    end
end;

{ =========================== TwsProbNormalPower =========================== }

// Cria objeto especificando parametros
constructor TwsProbNormalPower.Create(const LPar,APar: Double);
begin
  if (not Assigned(FRand)) then
    FRand:=TwsNormalPower.Create;
  inherited Create(LPar,APar);
end;

// Cria objeto sem especificar parametros
constructor TwsProbNormalPower.Create;
begin
  if (not Assigned(FRand)) then
    FRand:=TwsNormalPower.Create;
  inherited Create;
end;

// Gera valor com distribuicao lognormal
function TwsProbNormalPower.GetRandValue: Double;
begin
  Result:=TwsNormalPower(FRand).Generate
end;

procedure TwsProbNormalPower.SetLam(const x: Double);
begin
  if not FEquals(x,0) then
    FLam:=x
  else
    SetErro(2,x);
end;

function TwsProbNormalPower.GetY(const x: Double): Double;
begin
  Result:=(wsMath.Power(x,FLam)-1)/FLam
end;

function TwsProbNormalPower.Prob(const x: Double): Double;
begin
  Result:=inherited Prob(GetY(x))
end;

// Quantis na distribuição normal potencia
function TwsProbNormalPower.Quantil(p: Double): Double;
begin
  Result:= wsMath.Power(FLam*inherited Quantil(p)+1,1/FLam)
end;

// Medidas descritivas na distribuição normal
procedure TwsProbNormalPower.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição normal potência.
    Como descendente, a classe utiliza as propriedades Mean e Sigma, mas elas nao são a
    média e o desvio padrão da lognormal.
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
         variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
  a1: Double;
begin
(*
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  a1:=exp(Sqr(FSigma));
  v[1]:=FMean;v[2]:=FSigma; v[3]:=exp(FMean+0.5*Sqr(FSigma)); v[4]:=exp(2*FMean)*a1*(a1-1);
  v[5]:=Sqrt(v[4]); v[6]:=Sqrt(a1-1)*(2+a1); v[7]:=Sqr(a1)*(Sqr(a1)+2*a1+3)-6;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Normal Potência';
    Res.Name:='MD_PNor';
    Res.ColName[1] := 'Lambda'; Res.ColName[2] := 'Alfa'; Res.ColName[3] := 'Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7] := 'Curtose';
    end;
  Res.MAdd(v)]
*)
end;

// Função de densidade da distribuição lognormal
function TwsProbNormalPower.Density(const x: Double): Double;
begin
  Result:=wsMath.Power(x,FLam-1)*inherited Density(GetY(x))
end;

function TwsProbNormalPower.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da lognormal através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de três posições:
      1 - Estimativa do parãmetro de dispersão
      2 - Estimativa do parâmetro de formato
      3 - Número de observações presentes na amostra
}
var
  w: TwsDFVec;
  i: Integer;
begin
  Err:=0;
  w:=TwsDFVec.Create(x.Len);
  for i:=1 to x.Len do
    w[i]:=GetY(x[i]);
  Result:= inherited MLE(w,It,Err,epsl,UpdatePar,MaxIter);
  w.Free
end; // MLE

procedure TwsProbNormalPower.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;

// Obtem estimativa pelo método dos momentos
function TwsProbNormalPower.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  w: TwsDFVec;
  i: Integer;
begin
  w:=TwsDFVec.Create(x.Len);
  for i:=1 to x.Len do
    w[i]:=GetY(x[i]);
  Result:= inherited MomentEst(w,UpdatePar);
  w.Free
end; // MomentEst

// Obtem valores da função de log-verossimilhança
function TwsProbNormalPower.LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral;
{ Objetivo
    Obter os valores da função de log-verossimilhança da distribuição normal potência
  Parâmetros
    x: Valores da amostra da variável X, supostamente normalmente distribuída
  Par: array com dois vetores cada qual com três posições: na primeira deverá estar o menor
    valor desejado para o parâmetro de dispersao, na segunda o maior e na terceira o
    acréscimo desejado para obtenção de cada valor a partir do mínimo. O mesmo se repete
    para o vetor relativo ao parãmetro de formato
  Retorno
    Matriz geral com Trunc((tmax-tmin)/tx)+1 linhas e Trunc((lmax-lmin)/dx)+1 colunas, que
    contém os valores desejados da função de log-verossimilhança
}
var
  w: TwsDFVec;
  i: Integer;
begin
  w:=TwsDFVec.Create(x.Len);
  for i:=1 to x.Len do
    w[i]:=GetY(x[i]);
  Result:= inherited LogLikelihood(w,Par);
  w.Free
end; //

function TwsProbNormalPower.PSearch(x: TwsVec; const cMin:Double=-2; const cMax: Double=2;
    const nVal: Integer=40): TwsGeneral;
{ Objetivo
    Obtem valores da log-verossimilhança para cada valor de potência na transformação de Box-
    Cox
  Parâmetros
    x: Conjunto de valores da variável
    cMin: Menor valor para a potência (Default=-2)
    cMax: Maior valor para a potência (Default=2)
    nVal: Número de valores para a potência (Default=40). Equivale a um acréscimo de 0.1 a
      cada vez. Nos valores default, as potênciuas serão -2, -1.9, ...
  Retorno
    Matriz geral com duas linhas. Na primeira estão os valores das potências utilizadas. Na
    segunda, os valores da log-verossimilhança para cada caso.
  Campos modificados
    FLam
}
var
  i,j,n: Integer;
  d    : Double;
  w    : TwsVec;
begin
  d:=(cMax-cMin)/nVal;
  FLam:=cMin;
  Result:=TwsGeneral.Create(2,nVal);
  Result.RowName[1]:='Potencias';
  Result.RowName[2]:='LVerossim';
  for j:=1 to nVal do
    Result.ColName[j]:='Pot'+IntToStr(j);
  n:=x.Len;
  w:=TwsDFVec.Create(n);
  i:=1;
  repeat
    if FLam <> 0 then
      begin
      for j:=1 to n do
        w[j]:=GetY(x[i]);
      Result[1,i]:=FLam;
      Result[2,i]:=inherited LogL(w);
      FLam:=FLam+d;
      Inc(i);
      end;
  until (i>nVal) or (FLam>cMax);
  w.Free
end; // PSearch

{ ============================== TwsProbLognormal3 ========================== }
{ Herança
    TwsProbLognormal3 --> TwsProbLognormal --> TwsProbNormal --> TwsProbStdNormal
      --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição lognormal (três parâmetros)
}
  // Cria objeto para distribuição lognormal3
constructor TwsProbLogNormal3.Create(const t,m,s: Double);
begin
  if (not Assigned(FRand)) then
    FRand:=TwsLognormal3.Create;
  inherited Create(m,s);
  SetTheta(t)
end;

constructor TwsProbLogNormal3.Create;
begin
  if (not Assigned(FRand)) then
    FRand:=TwsLognormal3.Create;
  inherited Create;
end;

// Atribui valor do parâmetro de deslocamento
procedure TwsProbLogNormal3.SetTheta(const t: Double);
begin
  if t>=0 then
    begin
    FTheta:=t;
    TwsLogNormal3(FRand).Theta:=t
    end
  else
    SetErro(2,t)
end;

// Atribui valores aos parametros
procedure TwsProbLogNormal3.SetPar(P: array of Double);
begin
  SetTheta(P[0]);
  SetMean(P[1]);
  SetSigma(P[2]);
end;

// Gera valor com distribuicao normal padrao
function TwsProbLogNormal3.GetRandValue: Double;
begin
  Result:=TwsLogNormal3(FRand).Generate
end;

// Cálculo de probabilidades na distribuição lognormal
function TwsProbLogNormal3.Prob(const x: Double): Double;
begin
  Result:= inherited Prob((x-FTheta))
end;

// Quantis na distribuição lognormal
function TwsProbLogNormal3.Quantil(p: Double): Double;
begin
  Result := FTheta+(inherited Quantil(p))
end;

// Medidas descritivas na distribuição lognormal
procedure TwsProbLogNormal3.Descriptive(var Res: TwsGeneral);
var
  v : TwsDFVec;
  a1: Double;
begin
  v:=TwsDFVec.Create(8);
  v.Name:='Valores';
  a1:=exp(Sqr(FSigma));
  v[1]:=FTheta; v[2]:=FMean; v[3]:=FSigma; v[4]:=FTheta+exp(FMean+0.5*Sqr(FSigma));
  v[5]:=exp(2*FMean)*a1*(a1-1); v[6]:=Sqrt(v[4]); v[7]:=Sqrt(a1-1)*(2+a1);
  v[8]:=Sqr(a1)*(Sqr(a1)+2*a1+3)-6;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,8);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Lognormal Três Parâmetros';
    Res.Name:='MD_LNorm3';
    Res.ColName[1]:='Teta'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Alfa';
    Res.ColName[4]:='Media'; Res.ColName[5]:='Variancia'; Res.ColName[6]:='Desv_Padrao';
    Res.ColName[7]:='Assimetria'; Res.ColName[8]:='Curtose';
    end;
  Res.MAdd(v);
end;

// Função de densidade da distribuição lognormal
function TwsProbLogNormal3.Density(const x: Double): Double;
begin
  Result := inherited Density(x-FTheta)
end;

function TwsProbLogNormal3.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da lognormal3 através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de quatro posições:
      1 - Estimativa do parãmetro de deslocamento
      2 - Estimativa do parâmetro de dispersão
      3 - Estimativa do parãmetro de formato
      4 - Número de observações presentes na amostra
}
var
  i,n,nIter : Integer;
  a,b,c,d,e,
  ff,z,w,g,
  h,fcn,fpn,
  aml,ams   : Double;
  v1,v2     : TwsVec;
  Conv      : Boolean;
begin
  Err:=0;
  aml:=0.8*x.MinOrMax(True);
  nIter:=0;
  n:=x.Len;
  It:=TwsGeneral.Create(0,3);
  It.Name:='IT_LNorm3';
  It.ColName[1]:='Passo'; It.ColName[2]:='Alfa'; It.ColName[3]:='f(a)';
  It.MLab:='Passos do processo iterativo';
  It.MAdd(TwsDFVec.Create([0,aml,wscMissValue]));
  repeat
    a:=0; b:=0; c:=0; d:=0; e:=0; ff:=0;
    for i:=1 to n do
      begin
      z:=x[i]-aml;
      w:=Ln(z);
      a:=a+w;
      b:=b+Sqr(w);
      c:=c+1/z;
      d:=d+1/Sqr(z);
      e:=e+w/z;
      ff:=ff+w/Sqr(z)
      end;
    w:=a/n;
    g:=(b/n)-Sqr(w)-w;
    h:=(-2*e/n)+(2*w)*(c/n)+c/n;
    fcn:=c*g+e;
    fpn:=c*h+d*g+ff-d;
    ams:=aml-(fcn/fpn);
    Inc(nIter);
    It.MAdd(TwsDFVec.Create([nIter,ams,fcn]));
    Conv:=(Abs(ams-aml)<Abs(epsl*ams)) and (nIter<=MaxIter);
    aml:=ams;
  until Conv;
  if Conv then               // se convergiu
    begin
    ff:=0; g:=0; n:=0;        // ff eh a media
    // Obtem media e variancia novamente
    for i:=1 to x.Len do
      begin
      Inc(n);
      w:=Ln(x[i]-aml);
      w:=w-ff;
      ff:=ff+w/n;
      g := g+((n-1)/n)*w*w;
      end;
    g:=g/(n-1);                // g eh a variancia
    Result:=nil;
    // Atualiza valores dos parametros, se necessario
    if UpdatePar then
      begin
      SetTheta(aml);            // deslocamento
      SetMean(ff);              // dispersao
      SetSigma(Sqrt(g));        // formato
      nParEst:=3;
      end
    else
      begin
      Result:=TwsDFVec.Create(4);
      Result[1]:=aml;           // parametro de deslocamento
      Result[2]:=ff;            // parametro de dispersao
      Result[3]:=Sqrt(g);       // parametro de formato
      Result[4]:=n;             // num de observacoes
      end;
    // matriz de covariancias
    a:=Exp(g-ff);
    b:=Exp(g-2*ff);
    h:=exp(g/2-ff);
    d:=0.5*((g+1)*Sqr(a)-(2*g+1)*b)/g;
    FV:=TwsSymmetric.Create(3);
    FV.Name:='LN3_Cov';
    FV.MLab:='Matriz de covariâncias';
    FV.ColName[1]:='Teta'; FV.ColName[2]:='Lambda'; FV.ColName[3]:='Alfa';
    FV.RowName[1]:='Teta'; FV.RowName[2]:='Lambda'; FV.RowName[3]:='Alfa';
    FV[1,1]:=1/(2*n*d);                         // var(Teta)
    FV[2,2]:=(g/(n*d))*((g+1)/(2*g)*Sqr(a)-b);  // var(Lam)
    FV[3,3]:=(g/(n*d))*((g+1)*Sqr(a)-b);        // var(Alfa)
    FV[2,1]:=-h/(2*n*d);                        // Cov(Teta,Lam)
    FV[3,1]:=(g/(n*d))*h;                       // Cov(Teta,Alfa)
    FV[3,2]:=-(g/(n*d))*b;                      // Cov(Lam,Alfa)
    end
  else
    Err:=1
end;

procedure TwsProbLogNormal3.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  v1,v2,p: TwsVec;
  i,k    : Integer;
  z,w,g  : Double;
  Err    : Word;
begin
  if F <> nil then
    begin
    k:=F.nCols;
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';

    // Magnitudes para os tempos de retorno
    g:=Sqr(Sigma);
    v1:=TwsDFVec.Create(F.nCols); v2:=TwsDFVec.Create(F.nCols);
    v1.Name:=x.Name; v2.Name:='Err_Padr';
    for i:=1 to k do
      begin
      z:=NInv(p[i],FEps,FUpper,Err);   // Quantil N(0,1)
      w:=exp(Mean+z*Sigma);
      v1[i]:=Theta+w;
      v2[i]:=FV[1,1]+(Sqr(z*w)/(4*g))*FV[3,3]+Sqr(w)*FV[2,2]+((z*w)/Sigma)*FV[3,1]+2*w*FV[2,1]+
        (z*Sqr(w)/Sigma)*FV[3,2];
      v2[i]:=Sqrt(v2[i])
      end;
    F.MAdd(p);
    F.MAdd(v1); F.MAdd(v2);
    end
end;

// Obtem estimativa pelo método dos momentos
function TwsProbLogNormal3.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  m      : TwsVec;
  z1,z2,w: Double;
begin
{ 1: Numero de observacoes
  3: Media
  6: Desvio padrao
 10: Assimetria
}
  m:=x.Moments;
  if m[1]>3 then
    begin
    Result:=TwsDFVec.Create(4);
    z1:=m[6]/m[3];
    w:=0.5*(-m[10]+Sqrt(Sqr(m[10])+4));
    z2:=(1-wsMath.Power(w,0.6666666666667))/wsMath.Power(w,0.333333333333);
    Result[1]:=m[3]-m[6]/z2;           // deslocamento
    w:=Ln(Sqr(z2)+1);
    Result[3]:=Sqrt(w);                // formato
    Result[2]:=Ln(m[6]/z2)-0.5*w;      // dispersao
    Result[4]:=m[1]
    end;
  m.Free;
  if UpdatePar then
    begin
    SetTheta(Result[1]);
    SetMean(Result[2]);
    SetSigma(Result[3]);
    nParEst:=3;
    end
end;

  // Obtem valores parciais da função de log-verossimilhança
function TwsProbLogNormal3.PartialLogLikelihood(x: TwsVec; const xInf,xSup,xDelta: Double): TwsGeneral;
{ Objetivo
    Obtem o valor da verossimilhança parcial para um conjunto de valores de Theta, o parâmetro de
    deslocamento
  Parâmetros
    xInf: Valor inicial do parâmetro
    xSup: Valor final do parâmetro
    xDelta: Valor do acréscimo para o parâmetro
}
var
  i,k,j,n        : Integer;
  xT,x2,x3,wm,yv,
  sum,sum2,aux   : Double;   
begin
  xT:=xInf;
  // Numero de valores para Theta
  k:=Trunc((xSup-xInf)/xDelta)+1;
  Result:=TwsGeneral.Create(2,k);
  Result.RowName[1]:='Teta';
  Result.RowName[2]:='LogV';
  aux:=0.5*Ln(2*pi);
  for i:=1 to k do
    begin
    // Na primeira linha, os valores do parametro
    Result[1,i]:=xT;
    Result.ColName[i]:='Teta'+IntToStr(i);
    wm:=0; yv:=0; n:=0; x3:=0; sum:=0; sum2:=0;
    // Obtem estimativas dos parametros para valor xT de Theta
    for j:=1 to x.Len do
      if (not wsGLib.IsMissValue(x[j]) and (x[j]>0)) then
        begin
        Inc(n);
        x3:=Ln((x[j]-xT));
        sum:=sum+x3;
        x2:=x3-wm;
        yv:=yv+((n-1)/n)*x2*x2;
        wm:=wm+x2/n;
        end;
    yv:=Sqrt(yv/n);
    for j:=1 to x.Len do
      begin
      x3:=Ln((x[j]-xT));
      sum2:=sum2+Sqr((x3-wm)/yv);
      end;
    // Na segunda linha os valores da verossimilhanca
    Result[2,i]:=-n*Ln(yv)-n*aux-sum-sum2;
    // Novo valor para o parametro
    xT:=xT+xDelta
    end;
end;

// Obtem valores da função de log-verossimilhança
function TwsProbLogNormal3.LogLikelihood(x: TwsVec; Par: array of TwsDFVec):TwsGeneral;
begin
  //
end;

{ ============================ TwsProbGamma ============================ }

constructor TwsProbGamma.Create(const a: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição gama
  Parâmetros
    a: Parâmetro alfa da distribuição. Uma mensagem de erro será gerada se a<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGamma.Create;
  inherited Create;
  SetAlpha(a);
end;

constructor TwsProbGamma.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição gama
  Parâmetros
    a: Parâmetro alfa da distribuição. Uma mensagem de erro será gerada se a<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGamma.Create;
  inherited Create;
end;

procedure TwsProbGamma.SetAlpha(const s: Double);
{ Objetivo
    Atribui valor ao parâmetro alfa
  Parâmetros
    s: valor do parâmetro
  Observações
    Ocorre erro se s<=o
}
begin
  if s>0 then
    begin
    FAlpha:=s;
    aux1:=LogGamma(FAlpha);
    aux2:=FAlpha-1;
    TwsGamma(FRand).Alfa:=s
    end
  else
    SetErro(2,s);
end;

// Atribui valores aos parametros
procedure TwsProbGamma.SetPar(P: array of Double);
begin
  SetAlpha(P[0])
end;

// Gera valor com distribuicao gama (1 parametro)
function TwsProbGamma.GetRandValue: Double;
begin
  Result:=TwsGamma(FRand).Generate
end;

function TwsProbGamma.Prob(const x: Double):Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se x<=0, ocorre erro
}
var
  Erro: Word;
begin
  if x > 0 then
    Result:=X2Aux(x,FAlpha,aux1,FUpper,Erro)
  else
    begin
    SetErro(1,x);
    Result := wscMissValue
    end
end; (* TwsProbGamma.Prob *)

function TwsProbGamma.Quantil(p: Double): Double;
{ Objetivo
    Obter pontos percentuais da distribuição gama
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<0.000002 ou p>0.999998;Erro)
}

var
   Erro:Word;
begin
  if (p>0.000002) and (p<0.999998) then
    Result:=0.5*ppchi2(p,2*FAlpha,aux1,Erro)
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end
end; (* TwsProbGamma.Quantil *)

function TwsProbGamma.Density(const x: Double): Double;
{ Objetivo
    Obtém o valor da densidade da distribuição gama
  Parâmetros
    x: Valor para obtenção da ordenada
}
Begin
  if x > 0 then
    Result:=wsMath.Power(x,aux2)*exp(-x-aux1)
  else
    Result:=0
End; (* Gamma *)

procedure TwsProbGamma.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição gama
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1]:=FAlpha; v[2]:=FAlpha; v[3]:=FAlpha; v[4]:=Sqrt(FAlpha); v[5]:=2/Sqrt(FAlpha);v[6]:=6/FAlpha;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Gama Um Parâmetro';
    Res.Name:='MD_Gama';
    Res.ColName[1]:='Formato'; Res.ColName[2]:='Media'; Res.ColName[3]:='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v);
end;

  // Obtem estimativa pelo método dos momentos
function TwsProbGamma.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  n: Integer;
begin
  Result:=TwsDFVec.Create(1);
  Result[1]:=x.Mean(n);
  if UpDatePar then
    begin
    SetAlpha(Result[1]);
    nParEst:=1
    end
end; // MomentEst

function TwsProbGamma.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da gama1 através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de duas posições:
      1 - Estimativa do parâmetro de formato
      2 - Número de observações presentes na amostra
}
var
  i,n        : Integer;
  x0,x1,d,fa,
  f0,ym,wm   : Double;
  Conv       : Boolean;
  Step       : TwsVec;
begin
  Err:=0;
  ym:=0; wm:=0; n:=0;         // media e variancia dos logaritmos
  for i:=1 to x.Len do
    begin
    x0:=x[i];
    if not wsGLib.IsMissValue(x0) and (x0>0) then
      begin
      Inc(n);
      x1:=Ln(x0);
      x0:=x0-ym;
      ym:=ym+x0/n;
      x1:=x1-wm;
      wm:=wm+x1/n;
      end
    end;
  if n>1 then
    begin
    d:=Ln(ym)-wm;
    fa:=ym;       // valor inicial
    It:=TwsGeneral.Create(0,3);
    It.Name:='IT_Gama1';
    It.MLab:='Passos do processo iterativo';
    It.ColName[1]:='Passo'; It.ColName[2]:='Alfa'; It.ColName[3]:='Diferenca';
    Step:=TwsDFVec.Create(3);
    Step[1]:=0; Step[2]:= fa; Step[3]:=wscMissValue;
    It.MAdd(Step);
    i:=1;
    repeat
      f0:=fa;
      fa:=fa*(Ln(fa)-Digamma(fa,Err))/d;
      Step:=TwsDFVec.Create(3);
      Step[1]:=i; Step[2]:= fa; Step[3]:=Abs(f0-fa);
      It.MAdd(Step);
      Inc(i);
      Conv:=Abs(f0-fa)<epsl;
    until Conv or (i>MaxIter);
    Result:=nil;
    if Conv then
      begin
      if UpdatePar then
        begin
        SetAlpha(fa);
        nParEst:=1
        end
      else
        begin
        Result:=TwsDFVec.Create(2);
        Result[1]:=fa;
        Result[2]:=n
        end
      end
    end
  else
    Err:=1
end; // MLE

procedure TwsProbGamma.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;

// Obtem valores da função de log-verossimilhança
function TwsProbGamma.LogLikelihood(x: TwsVec; Par: array of TwsDFVec): TwsGeneral;
{ Objetivo
    Obter os valores da função de log-verossimilhança da distribuição gama
  Parâmetros
    x: Valores da amostra da variável X, supostamente com distribuição gama
  Par: array com um vetor de três posições: na primeira deverá estar o menor valor desejado
    para o parâmetro, na segunda o maior e na terceira o acréscimo desejado para obtenção
    de cada valor a partir do mínimo.
  Retorno
    Matriz geral com uma linha e Trunc((lmax-lmin)/dx)+1 colunas, que contém os valores
    desejados da função de log-verossimilhança
}
var
  lmin,lmax,dx,
  sum,lx,lsum,xi: Double;
  n,i           : Integer;
begin
  lmin:=Par[0][1];
  lmax:=Par[0][2];
  dx  :=Par[0][3];
  sum:=0; lsum:=0; n:=0;
  for i:=1 to x.Len do
    begin
    xi:=x[i];
    if not wsGLib.IsMissValue(xi) and (xi>0) then
      begin
      Inc(n);
      sum:=sum+xi;
      lsum:=lsum+Ln(xi);
      end
    end;
  lx:=lmin;
  Result:=TwsGeneral.Create(2,Trunc((lmax-lmin)/dx)+1);
  for i:=1 to Result.NCols do
    begin
    Result[1,i]:=lx;
    Result[2,i]:=-n*LogGamma(lx)+(lx-1)*lsum-sum;
    lx:=lx+dx
    end;
end;

{ ============================== TwsProbGama2 ========================== }
{ Herança
    TwsProbGamma2 --> TwsProbGamma --> TwsProbCont --> TwsProb --> TObject
  Objetivo
    Implementa a distribuição gama com dois parâmetros
}

constructor TwsProbGamma2.Create(const a,b: Double);
{ Objetivo
    Cria objeto para tratamento da distribuição gama com dois parâmetros
  Parâmetros
    a: Parâmetro de formato
    b: Parâmetro de dispersão
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGamma2.Create;
  inherited Create(a);
  SetLam(b)
end;

constructor TwsProbGamma2.Create;
{ Objetivo
    Cria objeto para tratamento da distribuição gama com dois parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGamma2.Create;
  inherited Create;
end;

// Atribui valor do parâmetro de dispersao
procedure TwsProbGamma2.SetLam(const s: Double);
begin
  if s>0 then
    begin
    FLam:=s;
    TwsGamma2(FRand).Lambda:=s
    end
  else
    SetErro(2,s);
end;

// Atribui valores aos parametros
procedure TwsProbGamma2.SetPar(P: array of Double);
begin
  SetLam(P[0]);
  SetAlpha(P[1]);
end;

// Gera valor com distribuicao gama (2 parametros)
function TwsProbGamma2.GetRandValue: Double;
begin
  Result:=TwsGamma2(FRand).Generate
end;

// Cálculo de probabilidade na gama
function TwsProbGamma2.Prob(const x: Double): Double;
begin
  Result:=inherited Prob(x/FLam)
end;

// Cálculo de Quantis (inversas) na gama
function TwsProbGamma2.Quantil(p: Double): Double;
begin
  Result:=FLam*inherited Quantil(p)
end;

// Valor da função de densidade da gama
function TwsProbGamma2.Density(const x: Double): Double;
begin
  Result:=inherited Density(x/FLam)/FLam
end;

// Medidas descritivas da gama
procedure TwsProbGamma2.Descriptive(var Res: TwsGeneral);
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1]:=FAlpha; v[2]:=FLam; v[3]:=FAlpha*FLam; v[4]:=FAlpha*FLam*FLam; v[5]:=Sqrt(v[4]);
  v[6]:=2/Sqrt(FAlpha); v[7]:=6/FAlpha;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Gama Dois Parâmetros';
    Res.Name:='MD_Gama';
    Res.ColName[1]:='Alfa'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v);
end;

// Obtem estimativa pelo método dos momentos
function TwsProbGamma2.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  ym, yv: Double;
  n     : Integer;
begin
  x.VarMean(ym,yv,n);
  if n>1 then
    begin
    Result:=TwsDFVec.Create(2);
    Result[1]:=yv/ym;              // dispersao
    Result[2]:=Sqr(ym)/yv;         // formato
    if UpDatePar then
      begin
      SetLam(Result[1]);
      SetAlpha(Result[2]);
      nParEst:=2
      end
    end;
end; // MomentEst

function TwsProbGamma2.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da gama2 através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de três posições:
      1 - Estimativa do parâmetro de dispersão
      2 - Estimativa do parâmetro de formato
      3 - Número de observações presentes na amostra
}
var
  i,n        : Integer;
  x0,x1,d,fa,
  f0,ym,wm,yv: Double;
  Step       : TwsVec;
begin
  Err:=0;
  ym:=0; wm:=0; yv:=0; n:=0;         // media e media dos logaritmos
  Err:=0;
  for i:=1 to x.Len do
    begin
    x0:=x[i];
    if (not wsGLib.IsMissValue(x0) and (x0>0)) then
      begin
      Inc(n);
      x1:=Ln(x0);
      x0:=x0-ym;
      ym:=ym+x0/n;
      yv := yv + ((n-1)/n)*x0*x0;
      x1:=x1-wm;
      wm:=wm+x1/n;
      end
    end;
  if n>1 then
    begin
    d:=Ln(ym)-wm;
    fa:=Sqr(ym)/(yv/(n-1));       // valor inicial pelo metodo dos momentos
    i:=1;
    It:=TwsGeneral.Create(0,3);
    It.Name:='IT_Gama2';
    It.MLab:='Passos do processo iterativo';
    It.ColName[1]:='Passo'; It.ColName[2]:='Alfa'; It.ColName[3]:='Diferenca';
    Step:=TwsDFVec.Create(3);
    Step[1]:=0; Step[2]:= fa; Step[3]:=wscMissValue;
    It.MAdd(Step);
    repeat
      f0:=fa;
      fa:=fa*(Ln(fa)-Digamma(fa,Err))/d;
      Inc(i);
      Step:=TwsDFVec.Create(3);
      Step[1]:=i; Step[2]:= fa; Step[3]:=Abs(f0-fa);
      It.MAdd(Step);
    until (Abs(f0-fa)<epsl) or (i>MaxIter);
    Result:=nil;
    if UpDatePar then
      begin
      SetLam(ym/fa);            // Dispersao
      SetAlpha(fa);              // Formato
      nParEst:=2
      end
    else
      begin
      Result:=TwsDFVec.Create(3);
      Result[1]:=ym/fa;             // Dispersao
      Result[2]:=fa;                // Formato
      Result[3]:=n;
      end
    end
end; // MLE

procedure TwsProbGamma2.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;


{ ================== TwsProbGamma3 ==================== }

constructor TwsProbGamma3.Create(const a,b,c: Double);
{ Objetivo
    Cria objeto para tratamento de uma distribuição gama com três parâmetros
  Parâmetros
    a: Parâmetro de deslocamento
    b: Parâmetro de dispersão
    c: Parâmetro de formato
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGamma3.Create;
  inherited Create(b,c);
  SetTheta(a);
end;

constructor TwsProbGamma3.Create;
{ Objetivo
    Cria objeto para tratamento de uma distribuição gama com três parâmetros sem atribuir
    valores aos parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGamma3.Create;
  inherited Create;
end;

// Atribui valor do parâmetro de deslocamento
procedure TwsProbGamma3.SetTheta(const t: Double);
begin
  if t>0 then
    begin
    FTheta:=t;
    TwsGamma3(FRand).Theta:=t
    end
  else
    SetErro(2,t)
end;

// Atribui valores aos parametros
procedure TwsProbGamma3.SetPar(P: array of Double);
begin
  SetTheta(P[0]);
  SetLam(P[1]);
  SetAlpha(P[2]);
end;

  // Gera valor com distribuicao gama (3 parametros)
function TwsProbGamma3.GetRandValue: Double;
begin
  Result:=TwsGamma3(FRand).Generate
end;

// Cálculo de probabilidade na gama
function TwsProbGamma3.Prob(const x: Double): Double;
begin
  Result:= inherited Prob((x-FTheta))
end;

// Cálculo de Quantis (inversas) na gama
function TwsProbGamma3.Quantil(p: Double): Double;
begin
  Result := FTheta+(inherited Quantil(p))
end;

// Valor da função de densidade da gama
function TwsProbGamma3.Density(const x: Double): Double;
begin
  Result := inherited Density(x-FTheta)
end;

// Medidas descritivas da gama 3
procedure TwsProbGamma3.Descriptive(var Res: TwsGeneral);
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(8);
  v.Name:='Valores';
  v[1]:= FTheta; v[2]:=FLam; v[3]:=FAlpha; v[4]:=FTheta+FAlpha*FLam; v[5]:=FAlpha*FLam*FLam;
  v[6]:=Sqrt(v[4]); v[7]:=2/Sqrt(FAlpha); v[8]:=6/FAlpha;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,8);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Gama Três Parâmetros';
    Res.Name:='MD_Gama3';
    Res.ColName[1]:='Teta'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Alfa';
    Res.ColName[4]:='Media'; Res.ColName[5]:='Variancia'; Res.ColName[6]:='Desv_Padrao';
    Res.ColName[7]:='Assimetria'; Res.ColName[8]:='Curtose';
    end;
  Res.MAdd(v);
end;

// Obtem estimativa pelo método dos momentos
function TwsProbGamma3.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  m: TwsVec;
begin
{ 1: Numero de observacoes
  3: Media
  6: Desvio padrao
 10: Assimetria
}
  m:=x.Moments;
  if m[1]>3 then
    begin
    Result:=TwsDFVec.Create(4);
//    m[10]:=m[10]*Sqrt(m[1]*(m[1]-1))/(m[1]-2)*(1+8.5/m[1]);
    Result[3]:=Sqr(2/m[10]);                // Formato
    Result[1]:=m[3]-m[6]*Sqrt(Result[3]);   // Deslocamento
    Result[2]:=m[6]/Sqrt(Result[3]);       // Dispersao
    Result[4]:=m[1];
    if UpdatePar then
      begin
      SetTheta(Result[1]);
      SetLam(Result[2]);
      SetAlpha(Result[3]);
      nParEst:=3
      end
    end;
  m.Free
end;

function TwsProbGamma3.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da gama3 através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de quatro posições:
      1 - Estimativa do parâmetro de deslocamento
      2 - Estimativa do parâmetro de dispersão
      3 - Estimativa do parâmetro de formato
      4 - Número de observações presentes na amostra
  Algoritmo adaptadao de Kite (1977) pag. 120-122
}
var
  i,n,nIter       : Integer;
  a,b,c,r,z,gml,
  alph,beta,w,u,
  vv,du,dv,ast,
  fcn,fpn,dg,tg,dw: Double;
  Conv            : Boolean;
  Step,v1,v2      : TwsVec;
begin
  Err:=0;
  // Inicio processo iterativo para obtencao de Teta
  gml:=0.99*x.MinOrMax(True);
  n:=x.Len;
  nIter:=0;
  It:=TwsGeneral.Create(0,3);
  It.Name:='IT_Gama3';
  It.Name:='Gam3_It';
  It.MLab:='Passos do processo iterativo';
  It.ColName[1]:='Passo'; It.ColName[2]:='Teta'; It.ColName[3]:='f';
  Step:=TwsDFVec.Create(3);
  Step[1]:=0; Step[2]:= gml; Step[3]:=wscMissValue;
  It.MAdd(Step);
  repeat
    a:=0; b:=0; c:=0; r:=0;
    for i:=1 to n do
      begin
      z:=x[i]-gml;
      a:=a+1/z;
      b:=b+z;
      c:=c+Ln(z);
      r:=r+(1/Sqr(z))
      end;
    beta:=a/(a-(n*n)/b);
    alph:=b/(n*beta);
    dg:=DiGamma(beta,Err);
    fcn:=-n*dg+c-n*Ln(alph);
    vv:=a-(n*n)/b;
    u:=a;
    w:=b/n-n/a;
    du:=r;
    dv:=r-n*Sqr(n/b);
    dw:=-1+(n*r)/Sqr(a);
    tg:=TriGamma(Beta,Err);
    fpn:=-n*tg*((vv*du-u*dv)/Sqr(vv))-a-n*dw/w;
    ast:=gml-fcn/fpn;
    Conv:=Abs(ast-gml)<Abs(epsl*ast);
    gml:=ast;
    Inc(nIter);
    Step:=TwsDFVec.Create(3);
    Step[1]:=nIter; Step[2]:= gml; Step[3]:=fcn;
    It.MAdd(Step);
  until (nIter>MaxIter) or Conv;
  { Se convergiu,
    1 - gml  : estimativa do parametro de deslocamento
    2 - alph: estimativa do parametro de dispersao
    3 - beta : estimativa do parametro de formato
    }
  if Conv then
    begin
    z:=beta-2;
    a:=z*wsMath.Power(alph,4);
    a:=(2*tg-((2*beta-3)/Sqr(beta-1)))/a;  // determinante da matriz de cov

    w:=n*a*Sqr(alph);

    FV:=TwsSymmetric.Create(3);
    FV.Name:='Gam3_Cov';
    FV.MLab:='Matriz de Covariâncias';
    FV.ColName[1]:='Teta'; FV.ColName[2]:='Lambda'; FV.ColName[3]:='Alfa';
    FV.RowName[1]:='Teta'; FV.RowName[2]:='Lambda'; FV.RowName[3]:='Alfa';

    FV[1,1]:=(beta*tg-1)/w;                // varg var(Teta)
    FV[2,2]:=(tg/z-1/Sqr(beta-1))/w;       // vara var(Lamb)
    FV[3,3]:=2/(Sqr(alph)*w*z);            // varb var(alpha)
    FV[2,1]:=(1/(beta-1)-tg)/w;            // ag Cov(Teta,Lam)
    FV[3,1]:=-(beta/(beta-1)-1)/(w*alph);  // bg Cov(Teta,Alfa)
    FV[3,2]:=-(1/z-1/(beta-1))/(w*alph);   // ab Cov(Lam,Alfa)


(*
    FV[2,2]:=(tg/z-1/Sqr(beta-1))/w;       // vara var(Lamb)
    FV[3,3]:=2/(Sqr(alph)*w*z);            // varb var(alpha)
    FV[1,1]:=(beta*tg-1)/w;                // varg var(Teta)
    FV[2,1]:=(1/(beta-1)-tg)/w;            // ag Cov(Teta,Lam)
    FV[3,2]:=-(1/z-1/(beta-1))/(w*alph);   // ab Cov(Lam,Alfa)
    FV[3,1]:=-(beta/(beta-1)-1)/(w*alph);  // bg Cov(Teta,Alfa)
*)
    Result:=nil;
    if UpdatePar then
      begin
      Theta:=gml;
      Lambda:=alph;
      Alpha:=Beta;
      nParEst:=3
      end
    else
      begin
      Result:=TwsDFVec.Create(4);
      Result[1]:=gml; Result[2]:=alph; Result[3]:=beta; Result[4]:=n;
      end
    end
  else
    Err:=1
end;

procedure TwsProbGamma3.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  v1,v2,p   : TwsVec;
  i,k       : Integer;
  z,a,b,u,vv: Double;
  Err: Word;
begin
  if F <> nil then
    begin
    k:=F.nCols;
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';

    v1:=TwsDFVec.Create(k); v2:=TwsDFVec.Create(k);
    v1.Name:=x.Name; v2.Name:='Err_Padrao';
    for i:=1 to k do
      begin
      z:=NInv(p[i],FEps,FUpper,Err);

      a:=wsMath.Power(Alpha,1/3)-1/(9*wsMath.Power(Alpha,2/3))+z/(3*wsMath.Power(Alpha,1/6));
      b:=1/(3*wsMath.Power(Alpha,2/3))+2/(27*wsMath.Power(Alpha,5/3))-z/(18*wsMath.Power(Alpha,7/6));
      u:=wsMath.Power(a,3);

      v1[i]:=Theta+Lambda*u;

      vv:=3*Lambda*Sqr(a)*b;

      vv:=FV[2,2]*Sqr(u)+FV[3,3]*Sqr(vv)+FV[1,1]+2*u*vv*FV[3,2]+2*u*FV[2,1]+2*vv*FV[3,1];
      v2[i]:=Sqrt(vv)
      end;
    F.MAdd(p);
    F.MAdd(v1); F.MAdd(v2)
    end
end;

{ ============= TwsProbLogPearson3 =================}

constructor TwsProbLogPearson3.Create(const a,b,c: Double);
{ Objetivo
    Cria objeto para tratamento de uma distribuição gama com três parâmetros
  Parâmetros
    a: Parâmetro de deslocamento
    b: Parâmetro de dispersão
    c: Parâmetro de formato
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsLPearson3.Create;
  inherited Create(a,b,c);
end;

constructor TwsProbLogPearson3.Create;
{ Objetivo
    Cria objeto para tratamento de uma distribuição Log-Pearson tipo III sem atribuir
    valores aos parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsLPearson3.Create;
  inherited Create;
end;

  // Gera valor com distribuicao gama (3 parametros)
function TwsProbLogPearson3.GetRandValue: Double;
begin
  Result:=TwsLPearson3(FRand).Generate
end;

  // Cálculo de probabilidade na PT3
function TwsProbLogPearson3.Prob(const x: Double): Double;
begin
  Result:=(inherited Prob(Ln(x)))
end;

  // Cálculo de Quantis (inversas) na PT3
function TwsProbLogPearson3.Quantil(p: Double): Double;
begin
  Result:= exp(inherited Quantil(p))
end;

  // Valor da função de densidade da PT3
function TwsProbLogPearson3.Density(const x: Double): Double;
begin
  Result:=(inherited Density(Ln(x)))/x;
end;

(*
// Medidas descritivas da PT3
procedure TwsProbLogPearson3.Descriptive(var Res: TwsGeneral);
var
  m1,m2,m3,m4: Double;
  v: TwsDFVec;
begin
  m1:=exp(FTheta)/wsMath.Power(1-FLam,FAlpha);
  m2:=exp(2*FTheta)/wsMath.Power(1-2*FLam,FAlpha);
  m3:=exp(3*FTheta)/wsMath.Power(1-3*FLam,FAlpha);
  m4:=exp(4*FTheta)/wsMath.Power(1-4*FLam,FAlpha);
  v:=TwsDFVec.Create(8);
  v.Name:='Valores';
  v[1]:= FTheta; v[2]:=FLam; v[3]:=FAlpha; v[4]:=m1; v[5]:=m2-Sqr(m1); v[6]:=Sqrt(v[5]);
  v[7]:=(m3-3*m2*m1+2*m1*Sqr(m1))/(v[6]*Sqrt(v[6]));
  v[8]:=(m4-4*m1*m3+6*Sqr(m1)*m2-3*Sqr(m1*m1))/v[5];
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,8);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Log-Pearson Tipo III';
    Res.Name:='MD_LP3';
    Res.ColName[1]:='Teta'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Alfa';
    Res.ColName[4]:='Media'; Res.ColName[5]:='Variancia'; Res.ColName[6]:='Desv_Padrao';
    Res.ColName[7]:='Assimetria'; Res.ColName[8]:='Curtose';
    end;
  Res.MAdd(v);
end;
*)
  // Obtem estimativa pelo método dos momentos
function TwsProbLogPearson3.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  i: Integer;
  v: TwsVec;
begin
  v:=TwsDFVec.Create(x.Len);
  for i:=1 to x.Len do
    v[i]:=Ln(x[i]);
  Result:=inherited MomentEst(v,UpdatePar);
  v.Free
end;

  // Metodo da maxima verossimilhanca
function TwsProbLogPearson3.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da Log-Pearson com três parâmetros através do método
    da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de quatro posições:
      1 - Estimativa do parâmetro de deslocamento
      2 - Estimativa do parâmetro de dispersão
      3 - Estimativa do parâmetro de formato
      4 - Número de observações presentes na amostra
  Algoritmo adaptadao de Kite (1977) pag. 129-132
}
var
  i: Integer;
  w: TwsVec;
begin
  Err:=0;
  w:=TwsDFVec.Create(x.Len);
  for i:=1 to x.Len do
    w[i]:=Ln(x[i]);
  // Utiliza Gamma3 (PIII) para os logaritmos
  Result:=inherited MLE(w,It,Err,epsl,UpdatePar,MaxIter);
  w.Free;
end;

procedure TwsProbLogPearson3.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  i,jp,ju: Integer;
begin
  // obtem periodos de retorno da Gama (Pearson) 3 parametros
  inherited ReturnPeriods(x,F);
  // marca penultima e ultima linha para modificar valores
  jp:=F.nRows-1; ju:=F.nRows;
  for i:=1 to F.nCols do
    begin
    F[jp,i]:=exp(F[jp,i]);
    F[ju,i]:=0.5*F[jp,i]*(exp(F[ju,i])-exp(-F[ju,i]));
    end;
end;

{========================= TwsProbBeta ==================================}

constructor TwsProbBeta.Create(const a,b: Double);
{ Objetivo
    Cria um objeto para calculo de probabilidades da distribuição beta
  Parâmetros
    a,b : Parâmetros alfa e beta. Uma mensagem de erro será gerada se a ou b forem nulos
      ou negativos
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsBetaAB.Create;
  inherited Create;
  SetAlpha(a);
  SetBeta(b);
end;

constructor TwsProbBeta.Create;
{ Objetivo
    Cria um objeto para calculo de probabilidades da distribuição beta sem atribuir valores
    aos parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsBetaAB.Create;
  inherited Create;
end;

procedure TwsProbBeta.SetAlpha(const a: Double);
{ Objetivo
    Atribui valor ao parâmetro Alfa
  Parâmetros
    a: valor a atribuir
  Observações
    Ocorre erro se a<=0
}
begin
  if a>0 then
    begin
    FAlpha:=a;
    aux2:=FAlpha-1;
    TwsBetaAB(FRand).Alfa:=FAlpha
    end
  else
    SetErro(2,a);
end;

procedure TwsProbBeta.SetBeta(const b: Double);
{ Objetivo
    Atribui valor ao parâmetro Beta. Deve ser chamada depois de SetAlpha
  Parâmetros
    b: valor a atribuir
  Observações
    Ocorre erro se b<=0
}
begin
  if b>0 then
    begin
     FBeta:=b;
     aux1:=wsMath.Beta(FAlpha,FBeta);
     aux3:=FBeta-1;
     TwsBetaAB(FRand).Beta:=b
     end
  else
     SetErro(2,b);
end;

// Atribui valores aos parametros
procedure TwsProbBeta.SetPar(P: array of Double);
begin
  SetAlpha(P[0]);
  SetBeta(P[1]);
end;

  // Gera valor com distribuicao beta
function TwsProbBeta.GetRandValue: Double;
begin
  Result:=TwsBetaAB(FRand).Generate
end;

function TwsProbBeta.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se (x<=0) ou (x>=1) ocorre erro e retorna valor perdido
}
var
  Erro : Word;
begin
  if FEquals(x,0) then
    Result:=0
  else
    if FEquals(x,1) then
      Result:=1
    else
      if (x>0) and (x<1) then
        Result:=BInt(x,FAlpha,FBeta,FUpper,Erro)
      else
        begin
        SetErro(0,x);
        Result := wscMissValue
        end
end; (* TwsProbBeta.Prob *)

function TwsProbBeta.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel beta
  Parâmetros
    p: Probabilidade associada
  Observações
    Se(p<=0 ou p>=1;Erro)
}
var
   Erro : word;
begin
  if (p>0) and (p<1) then
    Result:=BInv(p,FAlpha,FBeta,FEps,FUpper,Erro)
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end
end; (* TwsProbBeta.Quantil *)

function TwsProbBeta.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da função de densidade da distribuição beta
  Parâmetros
    x: Valor para obtenção
  Observações
    Se(x<=0 ou x>=1; Erro)
}
Begin
  if (x>0) and (x<1) then
    Result:=wsMath.Power(x,aux2)*wsMath.Power(1-x,aux3)/aux1
  else
    Result := 0
End; (* TwsProbBeta.Density *)

procedure TwsProbBeta.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição beta
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  aux: Double;
  v  : TwsDFVec;
begin
  aux := FAlpha+FBeta;
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1] := FAlpha; v[2] := FBeta; v[3] := FAlpha/Aux;
  v[4] := (FAlpha*FBeta)/((aux+1)*Sqr(aux));
  v[5] := Sqrt(v[4]);
  v[6] := 2*(FBeta-FAlpha)*Sqrt(aux+1)/((aux+2)*Sqrt(FAlpha*FBeta));
  v[7] := 3*aux*(aux+1)*(FAlpha+1)*(2*FBeta-FAlpha);
  v[7] := v[7]/(FAlpha*FBeta*(aux+2)*(aux+3)+FAlpha*((FAlpha-FBeta)/aux));
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Beta';
    Res.Name:='MD_Beta';
    Res.ColName[1]:='Form_Alfa'; Res.ColName[2]:='Form_Beta'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padr'; Res.ColName[6]:='Assimetria';
    Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;

{========================= TwsProbChiSqr ===============================}

constructor TwsProbChiSqr.Create(const DF1: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição qui-quadrado
  Parâmetros
    DF1; Parâmetro da distribuição (Número de graus de liberdade).Uma mensagem de erro será
    gerada se DF1<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsChiSquare.Create;
  inherited Create;
  SetDF(DF1);
end;

constructor TwsProbChiSqr.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição qui-quadrado sem atribuir valor ao
    parâmetro
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsChiSquare.Create;
  inherited Create;
end;

procedure TwsProbChiSqr.SetDF(const DF1: Double);
{ Objetivo
    Atribui valor ao parâmetro (graus de liberdade)
  Parâmetros
    DF1: valor a atribuir
  Observações
    Se DF1<=0 ocorre erro
}
begin
  if DF1>0 then
    begin
    FDF:=DF1;
    aux1:=FDF/2;
    aux2:=LogGamma(aux1);
    aux3:=wsMath.Power(2,aux1);
    aux4:=aux1-1;
    TwsChiSquare(FRand).Alfa:=FDF
    end
  else
    SetErro(2,DF1);
end;

procedure TwsProbChiSqr.SetPar(P: array of Double);
begin
  SetDF(P[0]);
end;

// Gera valor com distribuicao qui-quadrado
function TwsProbChiSqr.GetRandValue: Double;
begin
  Result:=TwsChiSquare(FRand).Generate
end;

function TwsProbChiSqr.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Ocorre erro e retorna valor perdido se x<=0
}
var
  Erro : Word;
begin
  if x>0 then
    Result:= X2Int(x,FDF,FUpper,Erro)
  else
    begin
    SetErro(1,x);
    Result := wscMissValue
    end
end; (* TwsProbChiSqr.Prob *)

function TwsProbChiSqr.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variável qui-quadrado
  Parâmetros
    p: Probabilidade associada
  Observações
    Se(p<=0 ou p>=1;Erro)
}
var
   Erro:Word;
begin
  if (p>0) and (p<1) then
    Result:=ppchi2(p,FDF,aux2,Erro)
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end;
end; (* TwsProbChiSqr.Quantil *)

function TwsProbChiSqr.Density(const x: Double): Double;
{ Objetivo
    Obtém o valor da densidade da distribuição qui-quadrado
  Parâmetros
    x: Valor para obtenção da densidade
  Observações
    Se(x<=0; Erro)
}
Begin
  if (x>0) then
    Result:=wsMath.Power(x,aux4)*exp(-0.5*x-aux2)/aux3
  else
    Result := 0
End;(* Density *)

procedure TwsProbChiSqr.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição qui-quadrado
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1]:=FDF; v[2]:=FDF; v[3]:=2*FDF; v[4]:=Sqrt(v[3]);
  v[5]:=Sqrt(8/FDF); v[6]:=3+12/FDF;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Qui-Quadrado';
    Res.Name:='MD_QQuad';
    Res.ColName[1]:='Form_GL'; Res.ColName[2]:='Media'; Res.ColName[3]:='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v)
end;

{ ======================== TwsProbF ==============================}

constructor TwsProbF.Create(const num, den: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição F
  Parâmetros
    num: graus de liberdade do numerador
    den: Graus de liberdade do denominador
  Observações
    Uma mensagem de erro será gerada se num<=0 ou den<=0
}
begin
  inherited Create;
  SetDFNum(num);
  SetDFDen(den);
end;

constructor TwsProbF.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição F sem especificar valores para os
    parâmetros
  Observações
    Uma mensagem de erro será gerada se num<=0 ou den<=0
}
begin
  inherited Create;
end;

procedure TwsProbF.SetDFNum(const num: Double);
{ Objetivo
    Atribui valor para os graus de liberdade do numerador. Atualiza valores constantes
    dependentes dos valores dos parâmetros.
  Parâmetros
    num: valor para os graus de liberdade do numerador
  Observações
    Gera um erro se num<=0
}
begin
  if num>0 then
    begin
    Fn:=num;
    aux1:= Fn/2;
    end
  else
    SetErro(2,num);
end;

procedure TwsProbF.SetDFDen(const den: Double);
{ Objetivo
    Atribui valor para os graus de liberdade do denominador. Chamada deve ser posterior à
    chamada de SetDFNum pois atualiza valores constantes dependentes dos valores dos
    parâmetros.
  Parâmetros
    den: valor para os graus de liberdade do denominador
  Observações
    Gera mensagem de erro se den<=0
}
begin
  if den>0 then
    begin
    Fd:=den;
    aux2:= Fd/2;
    aux3:=Fn/Fd;
    aux4:=wsMath.Power(aux3,aux1);
    aux5:=aux1+aux2;
    aux6:=aux1-1;
    aux7:=Beta(aux1,aux2);
    end
  else
    SetErro(2,den);
end;

procedure TwsProbF.SetPar(P: array of Double);
begin
  SetDFNum(P[0]);
  SetDFDen(P[1]);
end;

function TwsProbF.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Ocorre erro e retorna valor perdido se x<=0
}
var
   Erro: word;
begin
  if x>0 then
    Result:=FInt(x,Fn,Fd,FUpper,Erro)
  else
    begin
    SetErro(1,x);
    Result := wscMissValue
    end
end; (* Prob *)

function TwsProbF.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variável F
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
var
   Erro:word;
begin
  if (p>0) and (p<1) then
    Result:=FInv(p,Fn,Fd,FEps,FUpper,erro)
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end
end; (* TwsProbF.Quantil *)

function TwsProbF.Density(const x: Double): Double;
{ Objetivo
    Obtém o valor da densidade da distribuição F de Snedecor
  Parâmetros
    x: Valor para obtenção
  Observações
    Se(x<=0; Erro)
}
Begin
  if (x>0) then
    Result:=(aux4*wsMath.Power(x,aux6)/wsMath.Power(1+aux3*x,aux5))/aux7
  else
    Result := 0
end;(* TwsProbF.Density *)

procedure TwsProbF.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição F de Snedecor
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  aux: double;
  v  : TwsDFVec;
begin
  aux:=Fn+Fd;
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1] := Fn; v[2] := Fd;
  if Fd > 2 then
    v[3] := Fd/(Fd-2)
  else
    v[3]:=wscMissValue;
  if Fd > 4 then
    begin
    v[4] := 2*Sqr(Fd)*(aux-2)/(Fn*Sqr(Fd-2)*(Fn-4));
    v[5] := sqrt(v[4])
    end
  else
    begin
    v[4] := wscMissValue;
    v[5] := wscMissValue;
    end;
  if Fd > 6 then
    begin
    v[6] := 8*(Fd-4)/((aux-2)*Fn);
    v[6]:=Sqrt(v[6])*(2*Fn+Fd-2)/(Fd-6)
    end
  else
    v[6] := wscMissValue;
  if Fd > 8 then
    begin
    v[7] := 12*(Sqr(Fd-2)*(Fd-4)+Fn*(aux-2)*(5*Fd-22));
    v[7] := v[7]/(Fn*(Fd-6)*(Fd-8)*(aux-2))
    end
  else
    v[7] := wscMissValue;
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='MD_F';
    Res.MLab:='Medidas Descritivas - F';
    Res.ColName[1]:='Form_GLN'; Res.ColName[2]:='Form_GLD'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7] := 'Curtose';
    end;
  Res.MAdd(v)
end;

{=========================== TwsProbTStudent =============================}

constructor TwsProbTStudent.Create(const DF1: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição t de Student
  Parâmetros
    DF1: Parâmetro da distribuição t (graus de liberdade). Uma mensagem de erro será gerada
    se DF1<=0
  Observações
    Atribui True a OneSided. Assim, por default, será considerada a distribuição unilateral
}
begin
  inherited Create;
  SetDF(DF1);
  FOneSided:=true;
end;

constructor TwsProbTStudent.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição t de Student sem especificar valor para
    o parâmetro
  Observações
    Atribui True a OneSided. Assim, por default, será considerada a distribuição unilateral
}
begin
  inherited Create;
  FOneSided:=True;
end;

procedure TwsProbTStudent.SetDF(const DF1: Double);
{ Objetivo
    Atribui valor ao parâmetro da distribuição (graus de liberdade)
  Parâmetros
    DF1: valor para o parâmetro graus de liberdade
  Observações
    Ocorre erro se DF1<=0
}
begin
  if DF1 > 0 then
    begin
    FDF:=DF1;
    aux1:=0.5*(FDF+1);
    aux2:=Beta(0.5,0.5*FDF);
    aux3:=Sqrt(FDF);
    end
  else
    SetErro(2,DF1);
end;

procedure TwsProbTStudent.SetPar(P: array of Double);
begin
  SetDF(P[0]);
end;

function TwsProbTStudent.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
}
var
  Erro: word;
begin
  Result:=TInt(x,FDF,FOneSided,FUpper,Erro);
end; (* TwsProbTStudent.Prob *)


function TwsProbTStudent.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel T
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
var
  Erro:word;
begin
  if (p>0) and (p<1) then
    Result:=TInv(p,FDF,FEps,FUpper,FOneSided,Erro)
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end
end; (* TPropTStudent.Quantil *)

function TwsProbTStudent.Density(const x: Double): Double;
{ Objetivo
    Obtém o valor da densidade da distribuição t de Student
  Parâmetros
    x: Valor para obtenção
}
Begin
  Result:=(wsMath.Power(1+Sqr(x)/FDF,-aux1))/(aux2*aux3);
End;(* TStudent *)

procedure TwsProbTStudent.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição t de Student
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1] := FDF;
  if FDF> 1 then
    v[2] := 0
  else
    v[2] := wscMissValue;
  if FDF> 2 then
    begin
    v[3] := FDF/(FDF-2);
    v[4]:=Sqrt(v[3])
    end
  else
    begin
    v[3] := wscMissValue;
    v[4] := wscMissValue
    end;
  v[5] := 0;
  if FDF> 4 then
    v[6] := 6/(FDF-4)
  else
    v[6] := wscMissValue;
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='MD_T';
    Res.MLab:='Medidas Descritivas - t Student';
    Res.ColName[1]:='Form_GL'; Res.ColName[2]:='Media'; Res.ColName[3] :='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v);
end;

{======================== TwsProbExponential ==========================}

constructor TwsProbExponential.Create(const b: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição exponencial
  Parâmetros
    b: Parâmetro da distribuição exponencial.
  Observação
    Uma mensagem de erro será gerada se b<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsExponential.Create;
  inherited Create;
  SetLam(b);
end;

constructor TwsProbExponential.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição exponencial sem especificar valor para
    o parâmetro
  Observação
    Uma mensagem de erro será gerada se b<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsExponential.Create;
  inherited Create;
end;

procedure TwsProbExponential.SetLam(const b: Double);
{ Objetivo
    Atribui valor ao parâmetro da distribuição exponencial
  Parâmetros
    b: Valor a atribuir
  Observações
    Gera mensagem de erro se b<=0
}
begin
  if b>0 then
    begin
    FLam:=b;
    TwsExponential(FRand).Alfa:=FLam
    end
  else
    SetErro(2,b)
end;

procedure TwsProbExponential.SetPar(P: array of Double);
begin
  SetLam(P[0]);
end;

// Gera valor com distribuicao exponencial
function TwsProbExponential.GetRandValue: Double;
begin
  Result:=TwsExponential(FRand).Generate
end;
(*
procedure TwsProbExponential.GetYCoord(var yMin,yMax: Double);
begin
 yMin:=yMin/FLam;
 yMax:=yMax/FLam
end;
*)
function TwsProbExponential.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Ocorre erro e retorna valor perdido se x<0
}
begin
  if (x>=0) then
    begin
    Result := 1-exp(-FLam*x); // Calcula a probabilidade acumulada
    if FUpper then
      Result := 1-Result
    end
  else
    begin
    SetErro(1,x);
    Result := 0
    end;
end; (* TwsProbExponential.Prob *)

function TwsProbExponential.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel exponencial
  Parâmetros
    p: Probabilidade associada
  Observações
    Se(p<=0 ou p>=1;Erro)
}
Begin
  if (p>0) and (p<1) then
    begin
    if FUpper then p:=1-p;
    Result:=-(ln(1-p))/FLam;
    end
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end;
end; (* TwsProbExponential.Quantil *)

function TwsProbExponential.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da densidade da distribuição Exponencial
  Parâmetros
    x: Valor para obtenção
  Observações
    Se(x<0; Erro)
}
Begin
  if(x>=0) then
    Result:=exp(-(x*FLam))*FLam
  else
    Result:=0
end;(* TwsProbExponential.Density *)

procedure TwsProbExponential.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição exponencial
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
    variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1]:=FLam; v[2]:=1/FLam; v[3]:=1/Sqr(FLam); v[4]:=Sqrt(v[3]); v[5]:=2; v[6]:=6;
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='MD_Expo';
    Res.MLab:='Medidas Descritivas - Exponencial';
    Res.ColName[1] := 'Lambda'; Res.ColName[2]:='Media'; Res.ColName[3] :='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v)
end;

function TwsProbExponential.LogLikelihood(x: TwsVec; Par: array of TwsDFVec): TwsGeneral;
{ Objetivo
    Obter os valores da função de log-verossimilhança da distribuição exponencial
  Parâmetros
    x: Valores da amostra da variável X, supostamente exponencialmente distribuída
  Par: array com um vetor de três posições: na primeira deverá estar o menor valor desejado
    para o parâmetro, na segunda o maior e na terceira o acréscimo desejado para obtenção
    de cada valor a partir do mínimo.
  Retorno
    Matriz geral com uma linha e Trunc((lmax-lmin)/dx)+1 colunas, que contém os valores
    desejados da função de log-verossimilhança
}
var
  lmin,lmax,dx,
  sum,LogL,lx  : Double;
  n,i          : Integer;
begin
  lmin:=Par[0][1];
  lmax:=Par[0][2];
  dx  :=Par[0][3];
  sum:=x.Total(n);
  lx:=lmin;
  Result:=TwsGeneral.Create(2,Trunc((lmax-lmin)/dx)+1);
  for i:=1 to Result.NCols do
    begin
    Result[1,i]:=lx;
    Result[2,i]:=n*Ln(lx)-lx*sum;
    lx:=lx+dx
    end;
end;

(*
function TwsProbExponential.QQInv(k,n: integer; const c: Double): Double;
{ Objetivo
    Obtém os valores dos quantis
  Parâmetros
    x: Vetor com os dados observados
  Retorno
    Vetor com os quantis da exponencial. Os valores de x retornam ordenados
}
begin
  Result := -Ln(1-(k-c)/(n-2*c+1));
end; // QQInv
*)

function TwsProbExponential.KSTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Verifica, através do teste de Kolmogorov-Smirnov, se um conjunto de valores são valores de
    uma variável exponencial. Veja descrição em TwsProbCont.KSTest. Valores críticos específicos
    estão em Law & Kelton, pág. 390
  Parâmetros
    x: Vetor com os dados amostrais
}
var
  aux: Double;
  n  : Integer;
begin
  Result:=KSD(x);
  n:=x.Len;
  aux:=sqrt(x.Len);
  Result[1,2]:=(Result[1,1]-0.2/n)*(aux+0.26+0.5/aux); Result[1,3]:=KSProb(Result[1,2]);
  Result.MLab:=Result.MLab+' Dist. Exponencial';
  VC:=TwsGeneral.Create(3,5);
  VC.Name:='Distrib'; VC.MLab:='Valores Críticos para Estatística Ajustada';
  VC.ColName[1]:='p1'; VC.ColName[2]:='p2'; VC.ColName[3]:='p3'; VC.ColName[4]:='p4';
  VC.ColName[5]:='p5';
  VC.RowName[1]:='1-Alfa'; VC[1,1]:=0.85; VC[1,2]:=0.9; VC[1,3]:=0.95;
  VC[1,4]:=0.975; VC[1,5]:=0.99;
  VC.RowName[2]:='Par_Conhec'; VC[2,1]:=1.138; VC[2,2]:=1.224; VC[2,3]:=1.358;
  VC[2,4]:=1.48; VC[2,5]:=1.628;
  VC.RowName[3]:='Par_Est'; VC[3,1]:=0.926; VC[3,2]:=0.99; VC[3,3]:=1.094;
  VC[3,4]:=1.19; VC[3,5]:=1.308;
end;

function TwsProbExponential.ADTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Verifica, através do teste de Anderson-Darling, se um conjunto de valores são valores de
    uma variável exponencial. Valores críticos específicos estão em Law & Kelton, pág. 392
    Veja descrição em TwsProbCont.ADTest
}
begin
  Result:=inherited ADTest(x,VC);
  Result[1,2]:=(1+0.66/x.Len)*Result[1,1];
  VC.MAdd(TwsDFVec.Create([1.07,1.326,1.587,1.943]));
  VC.RowName[3]:='Par_Est'
end; // ADTest

function TwsProbExponential.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da exponencial através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de duas posições:
      1 - Estimativa do parâmetro de dispersão
      2 - Número de observações presentes na amostra
}
var
  n  : Integer;
  m,s: Double;
begin
  Err:=0;
  x.VarMean(m,s,n);
  FV:=nil;
  It:=nil;
  if m>0 then
    begin
    m:=1/m;
    if UpdatePar then
      begin
      SetLam(m);
      nParEst:=1
      end
    else
      begin
      Result:=TwsDFVec.Create(2);
      Result[1]:=m;
      Result[2]:=n;
      end
    end;
  // Matriz de covariancias
  FV:=TwsSymmetric.Create(1);
  FV.Name:='Exp1_Cov';
  FV.MLab:='Matriz de covariâncias para as estimativas dos parâmetros';
  FV.ColName[1]:='Lambda';
  FV.RowName[1]:='Lambda';
  FV[1,1]:=s/n;
end;

procedure TwsProbExponential.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;

function TwsProbExponential.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
{ Objetivo
    Obtém estimativa do parâmetro pelo método dos momentos
  Parâmetros
    x: vetor com os dados da amostra
    UpdatePar: Se True (valor default) atualiza valor do parâmetro; se false apenas retorna
    valor
}
var
  n: Integer;
begin
  Result:=TwsDFVec.Create(1);
  Result[1]:=1/x.Mean(n);
  if UpdatePar then
    begin
    SetLam(Result[1]);
    nParEst:=1
    end
end;

{======================== TwsProbExponential2 ==========================}

constructor TwsProbExponential2.Create(const Th,Lb: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição exponencial com dois parâmetros
  Parâmetros
    Th: Parâmetro de origem da distribuição exponencial.
    Lb: Parâmetro de dispersão da distribuição exponencial.
  Observação
    Uma mensagem de erro será gerada se Th<=0 ou se Lb<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsExponential2.Create;
  inherited Create(Lb);
  SetTheta(Th);
end;

constructor TwsProbExponential2.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição exponencial com dois parâmetros sem
    atribuir valores aos parãmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsExponential2.Create;
  inherited Create;
end;

procedure TwsProbExponential2.SetTheta(const b: Double);
{ Objetivo
    Atribui valor ao parâmetro da distribuição exponencial
  Parâmetros
    b: Valor a atribuir
  Observações
    Gera mensagem de erro se b<=0
}
begin
  if b>=0 then
    begin
    FTheta:=b;
    TwsExponential2(FRand).Theta:=b
    end
  else
    SetErro(2,b)
end;

procedure TwsProbExponential2.SetPar(P: array of Double);
begin
  SetTheta(P[0]);
  SetLam(P[1]);
end;

// Gera valor com distribuicao exponencial (2 parametros)
function TwsProbExponential2.GetRandValue: Double;
begin
  Result:=TwsExponential2(FRand).Generate
end;

procedure TwsProbExponential2.GetYCoord(var yMin,yMax: Double);
begin
  yMin:=FTheta+yMin/FLam;
  yMax:=FTheta+yMax/FLam
end;

function TwsProbExponential2.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Ocorre erro e retorna valor perdido se x<0
}
begin
  Result:=inherited Prob(x-FTheta)
end; (* TwsProbExponential2.Prob *)

function TwsProbExponential2.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel exponencial
  Parâmetros
    p: Probabilidade associada
  Observações
    Se(p<=0 ou p>=1;Erro)
}
Begin
  Result:=FTheta+inherited Quantil(p)
end; (* TwsProbExponential2.Quantil *)

function TwsProbExponential2.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da densidade da distribuição Exponencial
  Parâmetros
    x: Valor para obtenção
  Observações
    Se(x<0; Erro)
}
Begin
  Result:=inherited Density(x-FTheta);
end;(* TwsProbExponential2.Density *)

procedure TwsProbExponential2.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição exponencial com dois parâmetros
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1]:=FTheta; v[2]:=FLam; v[3]:=FTheta+1/FLam; v[4]:=1/Sqr(FLam); v[5]:=Sqrt(v[4]);
  v[6]:=2; v[7]:=6;
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='MD_Expo';
    Res.MLab:='Medidas Descritivas - Exponencial';
    Res.ColName[1]:='Teta'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao';
    Res.ColName[6]:='Assimetria'; Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;

function TwsProbExponential2.LogLikelihood(x: TwsVec; Par: array of TwsDFVec): TwsGeneral;
{ Objetivo
    Obter os valores da função de log-verossimilhança da distribuição exponencial com dois parãmetros
  Parâmetros
    x: Valores da amostra da variável X, supostamente exponencialmente distribuída
  Par: array com dois vetores cada qual com três posições: na primeira deverá estar o menor valor
    desejado para o parâmetro de origem, na segunda o maior e na terceira o acréscimo desejado para
    obtenção de cada valor a partir do mínimo. O mesmo se repete para o vetor relativo ao parãmetro de
    dispersão
  Retorno
    Matriz geral com Trunc((tmax-tmin)/tx)+1 linhas e Trunc((lmax-lmin)/dx)+1 colunas, que contém os
    valores desejados da função de log-verossimilhança
}
var
  lmin,lmax,dx,
  tmin,tmax,tx,
  sum,lx,lt     : Double;
  n,k,i,il,it,j : Integer;
begin
  // Parametro de origem
  tmin:=Par[0][1];
  tmax:=Par[0][2];
  tx  :=Par[0][3];
  // Parametro de dispersão
  lmin:=Par[1][1];
  lmax:=Par[1][2];
  dx  :=Par[1][3];

  // Numero de valores para o parametro Theta
  it:=Trunc((tmax-tmin)/tx)+1;
  // Numero de valores para lambda
  il:=Trunc((lmax-lmin)/dx)+1;
  n:=x.Len;
  lt:=tmin;
  Result:=TwsGeneral.Create(it,il);
  // Para cada valor de Theta
  for i:=1 to it do
    begin
    sum:=0;
    for k:=1 to n do
      sum:=sum+(x[k]-lt);
    lt:=lt+tx;
    lx:=lmin;
    // Para cada valor de Lambda
    for j:=1 to il do
      begin
      Result[i,j]:=n*Ln(lx)-lx*sum;
      lx:=lx+dx
      end
    end
end;

function TwsProbExponential2.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da esponencial2 através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de três posições:
      1 - Estimativa do parâmetro de deslocamento
      2 - Estimativa do parâmetro de dispersão
      3 - Número de observações presentes na amostra
}
var
  n,i   : Integer;
  L,T,x1: Double;
  v1,v2 : TwsVec;
begin
  Err:=0;
  x1:=x.Mean(n);
  if (n>0) and (x1>0) then
    begin
    T:=x.MinOrMax(True);
    L:=1/(x1-T);
    if UpdatePar then
      begin
      SetTheta(T);
      SetLam(L);
      nParEst:=2
      end
    else
      begin
      Result:=TwsDFVec.Create(3);
      // Theta eh estimado como o menor valor
      Result[1]:=T;
      // Estimativa do parametro de dispersao
      Result[2]:=L;
      Result[3]:=n
      end;
    // Matriz de covariancias
    FV:=TwsSymmetric.Create(2);
    FV.Name:='Exp2_Cov';
    FV.MLab:='Matriz de covariâncias para as estimativas dos parâmetros';
    FV.ColName[1]:='Lambda'; FV.ColName[2]:='Alfa';
    FV.RowName[1]:='Lambda'; FV.RowName[2]:='Alfa';
    x1:=1/Sqr(L);
    FV[1,1]:=x1/(n*n); FV[2,2]:=(x1/n)*(1+1/n-2/(n*n)); FV[1,2]:=0;
    (*
    if F <> nil then
      begin
      v1:=TwsDFVec.Create(F.nCols); v2:=TwsDFVec.Create(F.nCols);
      v1.Name:=x.Name; v2.Name:='D_Padr';
      for i:=1 to F.nCols do
        begin
        x1:=Quantil(1-1/F[1,i]);
        v1[i]:=m+x1*Sqrt(s);
        v2[i]:=Sqrt((s/n)*(1+sqr(x1)/2))
        end;
      F.MAdd(v1); F.MAdd(v2);
      end;
    *)
    end
end;

procedure TwsProbExponential2.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;

function TwsProbExponential2.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
{ Objetivo
    Obtém estimativa do parâmetro pelo método dos momentos
  Parâmetros
    x: vetor com os dados da amostra
    UpdatePar: Se True (valor default) atualiza valor do parâmetro; se false apenas retorna valor
}
var
  n: Integer;
begin
  Result:=TwsDFVec.Create(2);
  // menor valor estima parametro de origem
  Result[1]:=x.MinOrMax(True);
  // Estimativa do parametro de dispersao
  Result[2]:=1/(x.Mean(n)-Result[1]);
  // Atualiza parametros, se desejado
  if UpdatePar then
    begin
    SetTheta(Result[1]);
    SetLam(Result[2]);
    nParEst:=2
    end
end;

{ =================== Distribuição Uniforme =================== }

constructor TwsProbUniform.Create(const a,b: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição uniforme
  Parâmetros
    a: Limite superior
    b: Limite inferior
  Observações
    Uma mensagem de erro será gerada se b<=a
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsUniform.Create;
  inherited Create;
  SetAlpha(a);
  SetBeta(b);
end;

constructor TwsProbUniform.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição uniforme sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsUniform.Create;
  inherited Create;
end;

// Atribui valor ao parâmetro alfa
procedure TwsProbUniform.SetAlpha(const b: Double);
begin
  FAlpha:=b;
  TwsUniform(FRand).Min:=b
end;

procedure TwsProbUniform.SetBeta(const b: Double);
{ Objetivo
    Atribui valor do parâmetro Beta (extremo inferior do intervalo).
  Parâmetros
    b: valor do parâmetro
  Observações
    Gera mensagem de erro se b<=a (extremo superior menor ou igual ao extremo inferior).
    Deve ser chamada somente depois de SetAlpha
}
begin
  if b>FAlpha then // Beta precisa ser maior que Alpha
    begin
    FBeta := b;
    FAmp:=b-FAlpha;
    TwsUniform(FRand).Max:=b
    end
  else
    SetErro(2,b);
end;

procedure TwsProbUniform.SetPar(P: array of Double);
begin
  SetAlpha(P[0]);
  SetBeta(P[1]);
end;

// Gera valor com distribuicao uniforme continua
function TwsProbUniform.GetRandValue: Double;
begin
  Result:=TwsUniform(FRand).Generate
end;

function TwsProbUniform.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se x<alfa ou x> beta, ocorre erro e retorna valor perdido
}
begin
  if (x>=FAlpha) and (x<=FBeta) then
    begin
    Result := (x-FAlpha)/FAmp;
    if FUpper then
       Result:=1-Result
    end
  else
    begin
    SetErro(2,x);
    Result := 0
    end;
end;

function TwsProbUniform.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel uniforme
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
begin
  if (p>=0) and (p<=1) then
    Result := FAlpha+FAmp*p
  else
    begin
    SetErro(0,p);
    Result:=wscMissValue
    end
end;

function TwsProbUniform.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da função de densidade da distribuição uniforme
  Parâmetros
    x: Valor para obtenção
}
begin
  Result := 1/FAmp
end;

procedure TwsProbUniform.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição uniforme
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
      variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1]:=FAlpha; v[2]:=FBeta; v[3]:=(FAlpha+FBeta)/2; v[4]:=Sqr(FBeta-FAlpha)/12; v[5]:=Sqrt(v[4]);
  v[6]:=0; v[7]:=1.8;
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='MD_Unif';
    Res.MLab:='Medidas Descritivas - Uniforme';
    Res.ColName[1] := 'Extr_Inf'; Res.ColName[2]:='Extr_Sup'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;

// Estimativa por maxima verossimilhanca
function TwsProbUniform.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da Uniforme através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de quatro posições:
      1 - Estimativa do parâmetro alfa (limite inferior)
      2 - Estimativa do parâmetro beta (limite superior)
      3 - Número de observações presentes na amostra
}
var
  Min,Max: Double;
begin
  Err:=0;
  x.MinMax(Min,Max);
  if UpdatePar then
    begin
    SetAlpha(Min);
    SetBeta(Max);
    nParEst:=2
    end
  else
    begin
    Result:=TwsDFVec.Create(2);
    Result[1]:=Min;
    Result[2]:=Max;
    end
end;

procedure TwsProbUniform.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;


// Obtem estimativa pelo método dos momentos
function TwsProbUniform.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  ym,yv: Double;
  n    : Integer;
begin
  x.VarMean(ym,yv,n);
  if n>1 then
    begin
    yv:=sqrt(yv);
    Result:=TwsDFVec.Create(2);
    Result[1]:=ym-1.7320508*yv;
    Result[2]:=ym+1.7320508*yv;
    if UpdatePar then
      begin
      SetAlpha(Result[1]);
      SetBeta(Result[2]);
      nParEst:=2
      end
    end
end;

{============================== TwsProbStdWeibull ========================}

constructor TwsProbStdWeibull.Create(const a: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição Weibull padronizada
  Parâmetros
    a: Parâmetros alfa (formato) da distribuição Weibull.
  Observações
    Uma mensagem de erro será gerada se a<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsStdWeibull.Create;
  inherited Create;
  SetAlpha(a);
end;

constructor TwsProbStdWeibull.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição Weibull sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsStdWeibull.Create;
  inherited Create;
end;

procedure TwsProbStdWeibull.SetAlpha(const a: Double);
{ Objetivo
    Atribui valor ao parâmetro alfa (formato) da distribuição.
  Parâmetros
    a: valor a atribuir
  Observações
    Se a<=0, ocorre mensagem de erro
}
begin
  if a>0 then
    begin
    FAlpha:=a;
    aux:=a;
    TwsStdWeibull(FRand).Alpha:=a
    end
  else
    SetErro(2,a)
end;

procedure TwsProbStdWeibull.SetPar(P: array of Double);
{ Objetivo
    Atribui valores aos parâmetros
  Parâmetros
    P: array com os valores para atribuicao
      P[0]: parâmetro de formato
}
begin
  SetAlpha(P[0]);  // parametro de formato
end;

// Gera valor com distribuicao Weibull
function TwsProbStdWeibull.GetRandValue: Double;
begin
  Result:=TwsStdWeibull(FRand).Generate
end;

function TwsProbStdWeibull.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  if (x>0) then
    begin
    Result := 1-exp(-wsMath.Power(x,FAlpha));
    if upper then
      Result:= 1-Result
    end
  else
    begin
    SetErro(1,x);
    Result := 0
    end
end; (* TwsProbStdWeibull.Prob *)

function TwsProbStdWeibull.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel Weibull
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
Begin
  if (p>0) or (p<1) then
    begin
    if upper then p:=1-p;
    Result:=(wsMath.Power(-Ln(1-p),1/FAlpha));
    end
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end;
end; (* TwsProbStdWeibull.Quantil *)

function TwsProbStdWeibull.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da função de densidade da distribuição Weibull
  Parâmetros
    x: Valor para obtenção
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  if (x>0) then
    Result:= aux*wsMath.Power(x,FAlpha-1)*exp(-wsMath.Power(x,FAlpha))
  else
    Result := 0
end; (* TwsProbWeibull.Density *)

procedure TwsProbStdWeibull.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição Weibull
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
         variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v     : TwsDFVec;
  m1,m2,
  m3,m4 : Double;
begin
  m1 := exp(LogGamma(1+1/FAlpha));
  m2 := exp(LogGamma(1+2/FAlpha));
  m3 := exp(LogGamma(1+3/FAlpha));
  m4 := exp(LogGamma(1+4/FAlpha));
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1]:=FAlpha; v[2]:=m1; v[3]:=m2-Sqr(m1); v[4]:=Sqrt(v[3]);
  v[5]:=m3-3*m2*m1+2*Sqr(m1)*m1;
  v[5]:=v[5]/(v[3]*Sqrt(v[3]));
  v[6]:=m4-4*m3*m1+6*m2*Sqr(m1)-3*Sqr(Sqr(m1));
  v[6]:=v[6]/Sqr(v[3]);
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Weibull';
    Res.Name:='MD_Weib';
    Res.ColName[1]:='Alfa'; Res.ColName[2]:='Media'; Res.ColName[3]:='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v)
end; { TwsProbStdWeibull.Descriptive }
(*
  // Obtem inversa para grafico de probabilidade
function TwsProbStdWeibull.QQInv(k,n: integer; const c: Double): Double;
begin
  Result:=-Ln(1-(k-c)/(n-c+1))
end;
*)
{============================== TwsProbWeibull ========================}

constructor TwsProbWeibull.Create(const a,b: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição Weibull
  Parâmetros
    a, b: Parâmetros lambda (dispersão) e alfa (formato) da distribuição Weibull.
  Observações
    Uma mensagem de erro será gerada se a<=0 ou b<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsWeibull.Create;
  SetLam(a);
  inherited Create(b);
end;

constructor TwsProbWeibull.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição Weibull sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsWeibull.Create;
  inherited Create;
end;

procedure TwsProbWeibull.SetLam(const b: Double);
{ Objetivo
    Atribui valor ao parâmetro lambda (dispersao) da distribuição. Deve ser chamado antes
    se SetAlpha.
  Parâmetros
    b: valor a atribuir
  Observações
    Se b<=0, ocorre mensagem de erro
}
begin
  if b>0 then
    begin
    FLam:=b;
    TwsWeibull(FRand).Lambda:=b
    end
  else
     SetErro(2,b)
end;

procedure TwsProbWeibull.SetPar(P: array of Double);
{ Objetivo
    Atribui valores aos parâmetros
  Parâmetros
    P: array com os valores para atribuicao
      P[0]: parâmetro de dispersão
      P[1]: parâmetro de formato
}
begin
  SetLam(P[0]);    // parametro de dispersao
  SetAlpha(P[1]);  // parametro de formato
end;

// Gera valor com distribuicao Weibull
function TwsProbWeibull.GetRandValue: Double;
begin
  Result:=TwsWeibull(FRand).Generate
end;

function TwsProbWeibull.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  Result:=inherited Prob(x/FLam)
end; (* TwsProbWeibull.Prob *)

function TwsProbWeibull.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel Weibull
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
Begin
  Result:=FLam*inherited Quantil(p)
end; (* TwsProbWeibull.Quantil *)

function TwsProbWeibull.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da função de densidade da distribuição Weibull
  Parâmetros
    x: Valor para obtenção
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  Result:=inherited Density(x/FLam)
end; (* TwsProbWeibull.Density *)

procedure TwsProbWeibull.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição Weibull
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
         variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v     : TwsDFVec;
  m1,m2,
  m3,m4 : Double;
begin
  m1 := exp(LogGamma(1+1/FAlpha));
  m2 := exp(LogGamma(1+2/FAlpha));
  m3 := exp(LogGamma(1+3/FAlpha));
  m4 := exp(LogGamma(1+4/FAlpha));
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1]:=FLam; v[2]:=FAlpha; v[3]:=FLam*m1; v[4]:=Sqr(FLam)*(m2-Sqr(m1)); v[5]:=Sqrt(v[4]);
  v[6]:=Sqr(FLam)*FLam*(m3-3*m2*m1+2*Sqr(m1)*m1);
  v[6]:=v[6]/(v[4]*Sqrt(v[4]));
  v[7]:=Sqr(Sqr(FLam))*(m4-4*m3*m1+6*m2*Sqr(m1)-3*Sqr(Sqr(m1)));
  v[7]:=v[7]/Sqr(v[4]);
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Weibull';
    Res.Name:='MD_Weib';
    Res.ColName[1]:='Lambda'; Res.ColName[2]:='Alfa'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao';
    Res.ColName[6]:='Assimetria'; Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;

// Estimativa por maxima verossimilhanca
function TwsProbWeibull.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da Weibull através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de três posições:
      1 - Estimativa do parâmetro de dispersão
      2 - Estimativa do parâmetro de formato
      3 - Número de observações presentes na amostra
  Observações
    Algoritmo adaptado de Law & Kelton, pag. 334
}
var
  ak,ak1,a,
  bk,ck,hk,
  xp,lx    : Double;
  i,n,nIter: Integer;
  Conv     : Boolean;
  v1,v2    : TwsVec;
begin
  Err:=0;
  ak:=0; a:=0;
  n:=x.Len;
  for i:=1 to n do
    begin
    lx:=Ln(x[i]);
    a:=a+lx;       // soma dos logaritmos
    ak:=ak+Sqr(lx) // soma dos quadrados dos logaritmos
    end;
  ak:=(6/Sqr(pi)*(ak-Sqr(a)/n))/(n-1);
  ak:=1/Sqrt(ak);  // valor inicial do processo iterativo
  It:=TwsGeneral.Create(1,3);
  It.MLab:='Passos do processo iterativo';
  It.Name:='IT_Weib';
  It.ColName[1]:='Passo'; It.ColName[2]:='Alfa'; It.ColName[3]:='Aprox';
  It[1,1]:=0; It[1,2]:=ak; It[1,3]:=wscMissValue;
  nIter:=0;
  repeat
    bk:=0; ck:=0; hk:=0;
    for i:=1 to n do
      begin
      xp:=wsMath.Power(x[i],ak);
      lx:=Ln(x[i]);
      bk:=bk+xp;
      ck:=ck+xp*lx;
      hk:=hk+xp*Sqr(lx)
      end;
    xp:=(a/n+1/ak-ck/bk)/(1/Sqr(ak)+(bk*hk-Sqr(ck))/Sqr(bk));
    Conv:=Abs(xp)<epsl;
    if not Conv then
      begin
      ak1:=ak+xp;
      ak:=ak1;
      Inc(nIter);
      v1:=TwsDFVec.Create(3);
      v1[1]:=nIter; v1[2]:=ak1; v1[3]:=xp;
      It.MAdd(v1);
      end;
  until (nIter>25) or Conv;
  Result:=nil;
  if Conv then
    begin
    bk:=wsMath.Power(bk/n,1/ak1);  // bk eh o parametro de dispersao
    if UpdatePar then
      begin
      SetLam(bk);
      SetAlpha(ak1);
      nParEst:=2
      end
    else
      begin
      Result:=TwsDFVec.Create(3);
      Result[1]:=bk;  // dispersao
      Result[2]:=ak1; // formato
      Result[3]:=n    // tamanho da amostra
      end;
    FV:=TwsSymmetric.Create(2);
    FV.Name:='Weib_Cov';
    FV.MLab:='Matriz de covariâncias para as estimativas dos parâmetros - Weibull 2';
    FV.ColName[1]:='Lambda'; FV.ColName[2]:='Alfa';
    FV.RowName[1]:='Lambda'; FV.RowName[2]:='Alfa';
    lx:=0; ak:=0; ck:=0;
    for i:=1 to n do
      begin
      a:=x[i]/bk;
      xp:=wsMath.Power(a,ak1);
      ak:=ak+Ln(Sqr(a))*xp;
      ck:=ck+xp;
      lx:=lx+Ln(a)*xp;
      end;
    a:=-n*ak1/Sqr(bk)+ak1*(ak1+1)/Sqr(bk)*ck;    // Lam
    xp:=n/Sqr(ak1)+ak;                           // Alfa
    hk:=n/bk-ck/bk-ak1*lx/bk;                    // Lam,Alfa
    ck:=a*xp-Sqr(hk);                            // det
    FV[1,1]:=xp/ck;       // var(lam)
    FV[2,2]:=a/ck;        // var(alfa)
    FV[2,1]:=-hk/ck;      // covar(lam,alfa)
    end
  else
    Err:=1
end;

procedure TwsProbWeibull.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  v1,v2,p: TwsVec;
  i,k    : Integer;
  ck,ak  : Double;
begin
  if F <> nil then
    begin
    k:=F.nCols;
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';
    v1:=TwsDFVec.Create(k); v2:=TwsDFVec.Create(k);
    v1.Name:=x.Name; v2.Name:='Err_Padrao';
    for i:=1 to k do
      begin
      p[i]:=-Ln(1-1/F[1,i]);
      ck:=wsMath.Power(p[i],1/Alpha);        //dxdl
      v1[i]:=Lambda*ck;              // Magnitude
      ak:=-Lambda*ck*Ln(p[i])/sqr(Alpha); // dxda
      v2[i]:=Sqrt(Sqr(ck)*FV[1,1]+Sqr(ak)*FV[2,2]+2*ck*ak*FV[2,1]);
      end;
    F.MAdd(p);
    F.MAdd(v1); F.MAdd(v2);
    end;
end;

function TwsProbWeibull.KSTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Retorna os resultados do teste de ajustamento de Kolmogorov-Smirnov. Veja descrição
    em TwsProbCont.KSTest. Valores críticos específicos estão em Law & Kelton, pág. 391
  Parâmetros
    x: Vetor com os dados amostrais
}
var
  n: Integer;
begin
  Result:=KSD(x);
  n:=x.Len;
  Result[1,2]:=sqrt(x.Len)*Result[1,1]; Result[1,3]:=KSProb(Result[1,2]);
  Result.MLab:=Result.MLab+' Dist. Weibull';
  VC:=TwsGeneral.Create(6,5);
  VC.Name:='Distrib'; VC.MLab:='Valores Críticos para Estatística Ajustada';
  VC.ColName[1]:='nObs'; VC.ColName[2]:='p1'; VC.ColName[3]:='p2'; VC.ColName[4]:='p3';
  VC.ColName[5]:='p4';
  VC.RowName[1]:='1-Alfa'; VC[1,1]:=wscMissValue; VC[1,2]:=0.9; VC[1,3]:=0.95;
  VC[1,4]:=0.975; VC[1,5]:=0.99;
  VC.RowName[2]:='Par_Conhec'; VC[2,1]:=wscMissValue; VC[2,2]:=1.224; VC[2,3]:=1.358;
  VC[2,4]:=1.48; VC[2,5]:=1.628;
  VC.RowName[3]:='ParEst_n1'; VC[3,1]:=10; VC[3,2]:=0.76; VC[3,3]:=0.819;
  VC[3,4]:=0.88; VC[3,5]:=0.944;
  VC.RowName[4]:='ParEst_n2'; VC[4,1]:=20; VC[4,2]:=0.779; VC[4,3]:=0.843;
  VC[4,4]:=0.907; VC[4,5]:=0.973;
  VC.RowName[5]:='ParEst_n3'; VC[5,1]:=50; VC[5,2]:=0.79; VC[5,3]:=0.856;
  VC[5,4]:=0.922; VC[5,5]:=0.988;
  VC.RowName[6]:='ParEst_Inf'; VC[6,1]:=wscMissValue; VC[6,2]:=0.803; VC[6,3]:=0.874;
  VC[6,4]:=0.939; VC[6,5]:=1.007;
end;

function TwsProbWeibull.ADTest(x: TwsVec; var VC: TwsGeneral): TwsGeneral;
{ Objetivo
    Verifica, através do teste de Anderson-Darling, se um conjunto de valores são valores de
    uma variável exponencial. Valores críticos específicos estão em Law & Kelton, pág. 392
    Veja descrição em TwsProbCont.ADTest
}
begin
  Result:=inherited ADTest(x,VC);
  Result[1,2]:=(1+0.2/Sqrt(x.Len))*Result[1,1];
  VC.MAdd(TwsDFVec.Create([0.637,0.757,0.877,1.038]));
  VC.RowName[3]:='Par_Est'
end; // ADTest

(*
function TwsProbWeibull.QQData(x: TwsVec; const c: Double=0.375): TwsVec;
var
  i: Integer;
begin
  for i:=1 to x.Len do
    x[i]:=Ln(x[i]);
  Result:=inherited QQData(x,c)
end;
*)

{ TwsProbWeibull3 }

  // Cria objeto para distribuição Weibull3
constructor TwsProbWeibull3.Create(const t,a,b: Double);
begin
  if (not Assigned(FRand)) then
    FRand:=TwsWeibull3.Create;
  inherited Create(a,b);
  SetTheta(t)
end;

  // Cria objeto para distribuição Weibull3
constructor TwsProbWeibull3.Create;
begin
  if (not Assigned(FRand)) then
    FRand:=TwsWeibull3.Create;
  inherited Create;
end;

procedure TwsProbWeibull3.SetTheta(const t: Double);
{ Objetivo
    Atribui valor ao parâmetro Theta. Deve ser chamada somente depois da atribuição de valor
    ao parâmetro Lambda, que é redefinido como Lambda-Theta.
  Parâmetros
    t: valor a atribuir ao parâmetro
}
begin
  if t>0 then
    begin
    FTheta:=t;
    FLam:=FLam-t;
    TwsWeibull3(FRand).Theta:=t
    end
  else
    SetErro(2,t)
end;

  // Gera valor com distribuicao Weibull 3
function TwsProbWeibull3.GetRandValue: Double;
begin
  Result:=TwsWeibull3(FRand).Generate
end;

  // Atribui valores aos parametros
procedure TwsProbWeibull3.SetPar(P: array of Double);
{ Objetivo
    Atribui valores aos parâmetros
  Parâmetros
    P: array com os valores dos parâmetros
      P[0]: Parâmetro de deslocamento
      P[1]: Parâmetro de dispersao
      P[2]: Parâmetro de formato
}
begin
  SetLam(P[1]);
  SetAlpha(P[2]);
  SetTheta(P[0])
end;

// Cálculo de probabilidade na Weibull3
function TwsProbWeibull3.Prob(const x: Double): Double;
begin
  Result:=inherited Prob(x-FTheta)
end;

// Cálculo de Quantis (inversas) na Weibull3
function TwsProbWeibull3.Quantil(p: Double): Double;
begin
  Result:=FTheta+inherited Quantil(p)
end;

// Valor da função de densidade da Weibull3
function TwsProbWeibull3.Density(const x: Double): Double;
begin
  Result:=inherited Density(x-FTheta)/FLam
end;

// Medidas descritivas da Weibull3
procedure TwsProbWeibull3.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição Weibull 3
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
         variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v     : TwsDFVec;
  m1,m2,
  m3,m4 : Double;
begin
  m1 := exp(LogGamma(1+1/FAlpha));
  m2 := exp(LogGamma(1+2/FAlpha));
  m3 := exp(LogGamma(1+3/FAlpha));
  m4 := exp(LogGamma(1+4/FAlpha));
  v:=TwsDFVec.Create(8);
  v.Name:='Valores';
  v[1]:=FTheta; v[2]:=FLam; v[3]:=FAlpha; v[4]:=FTheta+FLam*m1; v[5]:=Sqr(FLam)*(m2-Sqr(m1));
  v[6]:=Sqrt(v[5]);
  v[7]:=Sqr(FLam)*FLam*(m3-3*m2*m1+2*Sqr(m1)*m1);
  v[7]:=v[7]/(v[5]*Sqrt(v[5]));
  v[8]:=Sqr(Sqr(FLam))*(m4-4*m3*m1+6*m2*Sqr(m1)-3*Sqr(Sqr(m1)));
  v[8]:=v[8]/Sqr(v[5]);
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,8);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Weibull Três Parâmetros';
    Res.Name:='MD_Weib3';
    Res.ColName[1]:='Theta'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Alfa';
    Res.ColName[4]:='Media'; Res.ColName[5]:='Variancia'; Res.ColName[6]:='Desv_Padrao';
    Res.ColName[7]:='Assimetria'; Res.ColName[8]:='Curtose';
    end;
  Res.MAdd(v)
end;

(*
  // Obtem inversa para grafico de probabilidade
function TwsProbWeibull3.QQInv(k,n: integer; const c: Double): Double;
begin
  Result:=wsMath.Power(inherited QQInv(k,n,c),1/FAlpha)
end;
*)

// Estimativa por maxima verossimilhanca REVISAO COMPLETA
function TwsProbWeibull3.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da Weibull (três parâmetros) através do método da
    máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de quatro posições:
      1 - Estimativa do parâmetro de deslocamento
      2 - Estimativa do parâmetro de dispersão
      3 - Estimativa do parâmetro de formato
      4 - Número de observações presentes na amostra
  Observações
    Algoritmo adaptado de Kite, pag. 148-151
}
var
  ga,a,b,c,d,ff,al,bl,
  dX,w,fcn,fpn,aux,be,
  e,g,z,t1,t2,t3,u,vv : Double;
  v1,v2                : TwsVec;
  i,j,k,n,kCount,iCount: Integer;
  Conv,Change          : Boolean;
  al1,al2,dif          : array[1..20] of Double;
begin
{ 1: Numero de observacoes
  3: Media
  6: Desvio padrao
 10: Assimetria
}
  v1:=x.Moments;
  n:=x.Len;
  // Coeficiente de variacao fora dos limites?
  Err:=0;
  if (v1[10]<-1.02) or (v1[10]>2) then
    Err:=2;
  al:=1/(0.2777757913+0.3132617714*v1[10]+0.057567091*Sqr(v1[10])-0.0013038566*Sqr(v1[10])*v1[10]
    - 0.0081523408*Sqr(Sqr(v1[10])));
  a:=x.MinOrMax(True);
  dX:=0.1*a;
  ga:=a+0.5*dX;
  It:=TwsGeneral.Create(0,4);
  It.Name:='IT_Weib3';
  It.MLab:='Passos do processo iterativo';
  It.ColName[1]:='Passo'; It.ColName[2]:='Alfa';
  It.ColName[3]:='Teta'; It.ColName[4]:='f_a';
  kCount:=0;
  It.MAdd(TwsDFVec.Create([kCount,al,ga,wscMissValue]));
  repeat
    Inc(kCount);
    for k:=1 to 20 do
      begin
      ga:=ga-dX;
      iCount:=0;
      repeat
        Inc(iCount);
        a:=0; b:=0; c:=0; d:=0; ff:=0;
        for i:=1 to n do
          begin
          w:=x[i]-ga;
          a:=a+1/w;
          b:=b+wsMath.Power(w,al);
          c:=c+wsMath.Power(w,al-1);
          d:=d+wsMath.Power(w,al-1)*Ln(w);
          ff:=ff+wsMath.Power(w,al)*Ln(w);
          end; //8
        fcn:=(al-1)/(n*al)*a-c/b;
        fpn:=1/(n*Sqr(al))*a-(b*d-c*ff)/Sqr(b);
        bl:=al-fcn/fpn;
        Conv:=(Abs(al-bl)<Abs(eps*bl)) and (iCount<=MaxIter);
        al:=bl;
      until Conv;
      al1[k]:=al;
      iCount:=0;
      repeat
        Inc(iCount);
        a:=0; b:=0; c:=0; d:=0;
        for i:=1 to n do
          begin
          w:=x[i]-ga;
          a:=a+wsMath.Power(w,al);
          b:=b+wsMath.Power(w,al)*Ln(w);
          c:=c+wsMath.Power(w,al)*Sqr(Ln(w));
          d:=d+Ln(w);
          end; //8
        fcn:=n+al*d-n*al*b/a;
        fpn:=d-n*al*((a*c-Sqr(b))/Sqr(a))-n*b/a;
        bl:=al-fcn/fpn;
        Conv:=(Abs(al-bl)<Abs(epsl*bl)) and (iCount<=MaxIter);
        al:=bl;
      until Conv;
      al2[k]:=al;
      dif[k]:=al1[k]-al2[k];
      Conv:=False;
      if k>1 then
        begin
        Conv:=(Abs(dif[k])<Abs(epsl*al1[k]));
        if Conv then
          Break
        else
          begin
          Change := ((dif[k-1]>=0) and (dif[k]<0)) or ((dif[k-1]<0) and (dif[k]>0));
          if Change then
            Break;
          end
        end
      end; // for k
    if not Conv then
      begin
      if not Change then
        begin
        Err:=1;   // processo nao convergiu
        Exit
        end
      else
        begin
        ga:=ga+dX;
        al:=0.25*(al1[k]+al1[k-1]+al2[k]+al2[k-1]);
        It.MAdd(TwsDFVec.Create([kCount,al,ga,fcn]));
        end
      end;
    dX:=dX/20;
    Conv:=Conv and (kCount<=MaxIter);
  until Conv;
  // ga eh a estimativa do parametro de deslocamento
  // al eh a estimativa do parametro de formato
  // be eh a estimativa do parametro de dispersao
//  conferir
  if Conv then
    begin
    a:=0; b:=0;
    aux:=1/al;
    for i:=1 to n do
      begin
      w:=Ln(x[i]-ga);
      RInc(a,w);
      RInc(b,wsMath.Power(w,al))
      end;
    be:=ga*wsMath.Power(b/n,aux);
    a:=0; b:=0; c:=0; d:=0; e:=0; ff:=0; g:=0; w:=0;
    for i:=1 to n do
      begin
      z:=x[i]-ga;
      aux:=wsMath.Power(z,al);
      dx:=Ln(z);
      RInc(a,aux*dx);
      RInc(b,aux*Sqr(dx));
      RInc(c,aux);
      RInc(d,wsMath.Power(z,al-1));
      RInc(e,wsMath.Power(z,al-2));
      RInc(ff,1/z);
      RInc(g,1/Sqr(z));
      RInc(w,wsMath.Power(z,al-1)*(1+al*dx));
      end;
    z:=be-ga;
    aux:=wsMath.Power(z,-al);
    fcn:=wsMath.Power(z,-al-1);
    fpn:=wsMath.Power(z,-al-2);
    dx:=Ln(z);
    bl:=wsMath.Power(z,al-1);
    u:=n/2;
    vv:=n/Sqr(z);
    al1[1]:=-n/Sqr(al)-aux*(b-Sqr(dx)*c);
    al1[2]:=u*fcn*((dx-1)*c+a);
    al1[3]:=u-ff+aux*(w+al*dx*d)+fcn*(1-al*dx)*c-al*bl*a;
    t1:=al*(al+1)*fpn*c;
    t3:=al*(al-1)*aux*e;
    t2:=Sqr(al)*fcn*d;
    al1[5]:=al*vv-t1;
    al1[6]:=-al*vv-t2+t1;
    al1[9]:=al*vv-(al-1)*g-t1+t2-t3;
    al1[4]:=al1[2];
    al1[7]:=al1[3];
    al1[8]:=al1[6];
    for k:=1 to 9 do
      al1[k]:=-al1[k];
    d:=al1[1]*(al1[5]*al1[9]-al1[6]*al1[8])-al1[2]*(al1[4]*al1[9]-al1[6]*al1[7])+
      al1[3]*(al1[4]*al1[8]-al1[5]*al1[7]);   // determinante
    FV:=TwsSymmetric.Create(3);
    FV[3,3]:=(al1[5]*al1[9]-al1[6]*al1[8])/d;       // vara
    FV[2,2]:=(al1[1]*al1[9]-al1[3]*al1[7])/d;       // varb
    FV[1,1]:=(al1[1]*al1[5]-al1[2]*al1[4])/d;       // varg
    FV[3,2]:=-(al1[4]*al1[9]-al1[6]*al1[7])/d;     // covab
    FV[3,1]:=(al1[4]*al1[8]-al1[5]*al1[7])/d;      // covag
    FV[2,1]:=-(al1[1]*al1[8]-al1[2]*al1[7])/d;     // covbg
    end // if Conv
  else
    Err:=1;
end;

procedure TwsProbWeibull3.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  v1,v2,p: TwsVec;
  i,k    : Integer;
  aux,ff,be,
  z,w,g,al,ga  : Double;
  al1    : array[1..20] of Double;
begin
  if F <> nil then
    begin
    k:=F.nCols;
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';

    aux:=1/al;
    ff:=be-ga;
    if F <> nil then
      begin
      v1:=TwsDFVec.Create(k); v2:=TwsDFVec.Create(k);
      for i:=1 to k do
        begin
        z:=-Ln(p[i]);             // y
        w:=wsMath.Power(z,aux);       // ya
        g:=wsMath.Power(-Ln(1-1/F[1,i]),aux);
        al1[1]:=1-w;                  // dxdg
        al1[2]:=w;                    // dxdb
        al1[3]:=-ff*w*Ln(z)/Sqr(al);  // dxda
        v1[i]:=ga+ff*g;
        v2[i]:=FV[3,3]*Sqr(al1[3])+FV[2,2]*Sqr(al1[2])+FV[1,1]*Sqr(al1[1])+2*al1[3]*al1[2]*FV[3,2]
          +2*al1[3]*al1[1]*FV[3,1]+2*al1[2]*al1[1]*FV[2,1];
         v2[i]:=Sqrt(v2[i])
        end;
      F.MAdd(p);
      F.MAdd(v1); F.MAdd(v2);
      end
    end
end;
{============================== TwsProbStdGumbel ========================}

constructor TwsProbStdGumbel.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição Gumbel padronizada
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsStdGumbel.Create;
  inherited Create;
end;

// Gera valor com distribuicao Gumbel
function TwsProbStdGumbel.GetRandValue: Double;
begin
  Result:=TwsStdGumbel(FRand).Generate
end;

function TwsProbStdGumbel.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  Result := exp(-exp(-x));
  if upper then
    Result:= 1-Result
end; (* TwsProbStdGumbel.Prob *)

function TwsProbStdGumbel.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel Gumbel padronizada
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
Begin
  if (p>0) or (p<1) then
    begin
    if upper then p:=1-p;
    Result:=-(Ln(-Ln(p)))
    end
  else
    begin
    SetErro(0,p);
    Result := wscMissValue
    end;
end; (* TwsProbStdGumbel.Quantil *)

function TwsProbStdGumbel.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da função de densidade da distribuição Gumbel padronizada
  Parâmetros
    x: Valor para obtenção
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  Result:=exp(-x-exp(-x))
end; (* TwsProbStdGumbel.Density *)

procedure TwsProbStdGumbel.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição Gumbel
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
    variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(5);
  v.Name:='Valores';
  v[1]:=0; v[2]:=1; v[3]:=0.5772157; v[4]:=Sqr(pi)/6;
  v[5]:=Sqrt(v[4]); v[6]:=1.1395; v[7]:=5.4;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,5);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Gumbel Padrão';
    Res.Name:='MD_Gumb';
    Res.ColName[1]:='Media'; Res.ColName[2]:='Variancia'; Res.ColName[3]:='Desv_Padrao';
    Res.ColName[4]:='Assimetria'; Res.ColName[5]:='Curtose';
    end;
  Res.MAdd(v)
end;
(*
  // Inversa da probabilidade empírica para gráfico de probabilidade
function TwsProbStdGumbel.QQInv(k,n: integer; const c: Double): Double;
begin
  Result:=-Ln(-Ln((k-c)/(n-2*c+1)));
end;
*)
{============================== TwsProbGumbel ========================}

constructor TwsProbGumbel.Create(const a,b: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição Gumbel
  Parâmetros
    a, b: Parâmetros Gamma (tendência central) e Lambda (dispersão) da distribuição Gumbel.
  Observações
    Uma mensagem de erro será gerada se b<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGumbel.Create;
  inherited Create;
  SetGamma(a);
  SetLam(b);
end;

constructor TwsProbGumbel.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição Gumbel sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGumbel.Create;
  inherited Create;
end;

procedure TwsProbGumbel.SetGamma(const a: Double);
{ Objetivo
    Atribui valor ao parâmetro gama da distribuição
  Parâmetros
    a: valor a atribuir
}
begin
  FGamma:=a;
  TwsGumbel(FRand).Gamma:=a
end;

procedure TwsProbGumbel.SetLam(const b: Double);
{ Objetivo
    Atribui valor ao parâmetro Lambda da distribuição
  Parâmetros
    b: valor a atribuir
  Observações
    Se b<=0, ocorre mensagem de erro
}
begin
  if b>0 then
    begin
    FLam:=b;
    TwsGumbel(FRand).Lambda:=b
    end
  else
     SetErro(2,b)
end;

procedure TwsProbGumbel.SetPar(P: array of Double);
begin
  SetGamma(P[0]);
  SetLam(P[1]);
end;

// Gera valor com distribuicao Gumbel
function TwsProbGumbel.GetRandValue: Double;
begin
  Result:=TwsGumbel(FRand).Generate
end;

function TwsProbGumbel.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  Result := inherited Prob(FLam*(x-FGamma))
end; (* TwsProbGumbel.Prob *)

function TwsProbGumbel.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel Gumbel
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro)
}
Begin
  Result:=FGamma+inherited Quantil(p)/FLam
end; (* TwsProbGumbel.Quantil *)

function TwsProbGumbel.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da função de densidade da distribuição Gumbel
  Parâmetros
    x: Valor para obtenção
  Observações
    Se x<=0 ocorre erro e retorna valor perdido
}
Begin
  Result:=FLam*(inherited Density((x-FGamma)*FLam))
end; (* TwsProbGumbel.Density *)

procedure TwsProbGumbel.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição Gumbel
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
    variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1]:=FGamma; v[2]:=FLam; v[3]:=FGamma+0.5772157/FLam; v[4]:=Sqr(pi/FLam)/6;
  v[5]:=Sqrt(v[4]); v[6]:=1.139547; v[7]:=5.4;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Gumbel';
    Res.Name:='MD_Gumb';
    Res.ColName[1]:='Gama'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao';
    Res.ColName[6]:='Assimetria'; Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;
(*
  // Inversa da probabilidade empírica para gráfico de probabilidade
function TwsProbGumbel.QQInv(k,n: integer; const c: Double): Double;
begin
  Result:=FGamma+inherited QQInv(k,n,c)/FLam;
end;
*)
// Obtem estimativa pelo método dos momentos
function TwsProbGumbel.MomentEst(x: TwsVec; UpdatePar: Boolean=True): TwsVec;
var
  ym,yv,y1,y2,
  a,s,s1     : Double;
  n,i,n1  : Integer;
  v1,v2,y : TwsVec;
begin
  x.VarMean(ym,yv,n);
  s:=Sqrt(yv);
  Result:=nil;
  if n>1 then
    begin
    Result:=TwsDFVec.Create(3);
    Result[1]:=ym-0.4501*s;  // Tendencia central
    Result[2]:=1.28255/s;    // Dispersao
    Result[3]:=n;
    if UpDatePar then
      begin
      SetGamma(Result[1]);
      SetLam(Result[2]);
      nParEst:=2
      end
    end;
end; // MomentEst

function TwsProbGumbel.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True; MaxIter: Integer=25): TwsVec;
{ Objetivo
    Obter estimativas dos parâmetros da Gumbel através do método da máxima verossimilhança
  Parâmetros
    Ver descrição em TwsProbHalfNormal.MLE
  Retorno
    Retorna um vetor de três posições:
      1 - Estimativa do parâmetro de tendência central
      2 - Estimativa do parâmetro de concentração
      3 - Número de observações presentes na amostra
}
var
  i,n,iTer         : Integer;
  lam,a,b,ym,aml,
  tmp,fcn,fpn,c,d,e: Double;
  Step,v1,v2       : TwsVec;
  Conv             : boolean;
begin
  x.VarMean(ym,b,n);
  if n>1 then
    begin
    Err:=0;
    aml:=1.28255/Sqrt(b);    // valor inicial de lambda pelo metodo dos momentos
    It:=TwsGeneral.Create(0,3);
    It.Name:='IT_Gumb';
    It.MLab:='Passos do processo iterativo';
    It.ColName[1]:='Passo'; It.ColName[2]:='Lambda'; It.ColName[3]:='Diferenca';
    Step:=TwsDFVec.Create(3);
    Step[1]:=0; Step[2]:= lam; Step[3]:=wscMissValue;
    It.MAdd(Step);
    Iter:=1;
    repeat
      a:=1/Sqr(aml);
      b:=ym-1/aml;
      c:=0; d:=0; e:=0;
      for i:=1 to n do
        begin
        tmp:=Exp(-aml*x[i]);
        c:=c+tmp;
        d:=d+tmp*x[i];
        e:=e+tmp*Sqr(x[i])
        end;
      fcn:=d-b*c;
      fpn:=b*d-e-a*c;
      lam:=aml-fcn/fpn;
      Step:=TwsDFVec.Create(3);
      Step[1]:=iTer; Step[2]:=lam; Step[3]:=Abs(lam-aml);
      It.MAdd(Step);
      Conv:=(Abs(lam-aml)<epsl*lam);
      Inc(iTer);
      aml:=lam;
    until Conv or (iTer>MaxIter);
    Result:=nil;
    if Conv then
      begin
      tmp:=Ln(n/c)/lam;
      if UpDatePar then
        begin
        SetLam(lam);            // Dispersao
        SetGamma(tmp);          // tendencia central
        nParEst:=2
        end
      else
        begin
        Result:=TwsDFVec.Create(3);
        Result[1]:=tmp;       // tendencia central
        Result[2]:=lam;       // concentracao
        Result[3]:=n;
        end;
      FV:=TwsSymmetric.Create(2);
      FV.Name:='Gumb_Cov';
      FV.MLab:='Matriz de Covariâncias';
      FV.ColName[1]:='Gama'; FV.ColName[2]:='Lambda';
      FV.RowName[1]:='Gama'; FV.RowName[2]:='Lambda';
      FV[1,1]:=0.257/n;
      FV[2,2]:=0.6079*Sqr(Lam)/n;
      FV[2,1]:=1.1086/(n*Sqr(Lam));
      end
    else  // if Conv
      Err:=2; // nao houve convergencia
    end
  else
    Err:=1; // acertar codigo para nao convergencia e falta de observacoes
end; // MLE

procedure TwsProbGumbel.MoReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  y,v1,v2,p: TwsVec;
  i,k,n1,n : Integer;
  y1,y2,ym,
  yv,s,s1,a: Double;

begin
  if F <> nil then
    begin
    k:=F.nCols;
    x.VarMean(ym,yv,n);
    s:=sqrt(yv);
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';

    y:=TwsDFVec.Create(n);
    n1:=n+1;
    for i:=1 to n do
      y[i]:=-Ln(-Ln((n1-i)/(n1)));
    y.VarMean(y1,y2,n);
    y.Free;
    s1:=Sqrt((n-1)*y2/n);
    v1:=TwsDFVec.Create(k); v2:=TwsDFVec.Create(k);
    v1.Name:=x.Name; v2.Name:='Err_Padrao';
    for i:=1 to k do
      begin
      a:=-Ln(-Ln(p[i]));
      a:=(a-y1)/s1;
      v1[i]:=ym+a*s;         // y para o tempo de retorno
      v2[i]:=(yv/n)*(1+1.139547093*a+1.100000027*Sqr(a));
      v2[i]:=Sqrt(v2[i])       // erro padrao
      end;
    F.MAdd(p);
    F.MAdd(v1); F.MAdd(v2);
    end
end;

procedure TwsProbGumbel.ReturnPeriods(x: TwsVec; F: TwsGeneral);
var
  v1,v2,p: TwsVec;
  i,k ,n: Integer;
  a      : Double;
begin
  if F <> nil then
    begin
    k:=F.nCols;
    n:=x.Len;
    p:=TwsDFVec.Create(k);
    for i:=1 to k do
      p[i]:=1-1/F[1,i];
    p.Name:='Prob_Acum';
    v1:=TwsDFVec.Create(k); v2:=TwsDFVec.Create(k);
    v1.Name:=x.Name; v2.Name:='Err_Padrao';
    for i:=1 to k do
      begin
      a:=-Ln(-Ln(p[i]));
      v1[i]:=Gamma+a/Lambda;         // y para o tempo de retorno
      v2[i]:=(1.1086+0.514*a+0.6079*Sqr(a))/(n*Sqr(Lambda));       // erro padrao
      end;
    F.MAdd(p);
    F.MAdd(v1); F.MAdd(v2);
    end
end;

{============================== TwsProbGVE ========================}

constructor TwsProbGVE.Create(const a,b,c: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição de extremos generalizada
  Parâmetros
    a,b,c: Parâmetros gama, lambda e alfa da distribuição, respectivamente. Alfa e lambda
      devem ser valores estritamente positivos.
  Observações
    Uma mensagem de erro será gerada se a<=0 ou b<=0
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGVE.Create;
  inherited Create;
  SetGamma(a);
  SetLam(b);
  SetAlpha(c);
end;

constructor TwsProbGVE.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição Gumbel sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGVE.Create;
  inherited Create;
end;

procedure TwsProbGVE.SetGamma(const a: Double);
{ Objetivo
    Atribui valor ao parâmetro gama da distribuição
  Parâmetros
    a: valor a atribuir
}
begin
  FGamma:=a;
  TwsGVE(FRand).Gamma:=a
end;

procedure TwsProbGVE.SetAlpha(const a: Double);
{ Objetivo
    Atribui valor ao parâmetro alfa da distribuição
  Parâmetros
    a: valor a atribuir
  Observações
    Se a<=0, ocorre mensagem de erro
}
begin
  if a>0 then
    begin
    FAlpha:=a;
    aux1:=1/FAlpha;
    aux2:=FLam/FAlpha;
    aux:=FGamma+aux1*FLam;
    TwsGVE(FRand).Alpha:=FAlpha
    end
  else
    SetErro(2,a)
end;

procedure TwsProbGVE.SetLam(const b: Double);
{ Objetivo
    Atribui valor ao parâmetro Lambda da distribuição
  Parâmetros
    b: valor a atribuir
  Observações
    Se b<=0, ocorre mensagem de erro
}
begin
  if b>0 then
    begin
    FLam:=b;
    TwsGVE(FRand).Lambda:=b
    end
  else
     SetErro(2,b)
end;

procedure TwsProbGVE.SetPar(P: array of Double);
{ Objetivo
    Atribui valores aos parâmetros da distribuições de extremos generalizada
  Parâmetros
    P: array com os valores dos parâmetros
      P[0]: parâmetro de tendência central
      P[1]: parâmetro de dispersão
      P[2]: parâmetro de de formato
}
begin
  SetGamma(P[0]);
  SetLam(P[1]);
  SetAlpha(P[2]);
end;

// Gera valor com distribuicao GVE
function TwsProbGVE.GetRandValue: Double;
begin
  Result:=TwsGVE(FRand).Generate
end;

function TwsProbGVE.Prob(const x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    Se x nao esta nos limites validos, ocorre erro e retorna valor 0
}
var
  w : Double;
  Ok: Boolean;
Begin
  Ok:= ((FAlpha<0)) and (x>=aux) or ((FAlpha>0) and (x<=aux));
  if Ok then
    begin
    w:=1-(x-FGamma)/aux2;
    Result := exp(wsMath.Power(-w,aux1))
    end
  else
    Result:=0
end; (* TwsProbGVE.Prob *)

function TwsProbGVE.Quantil(p: Double): Double;
{ Objetivo
    Dada uma probabilidade, retorna o valor correspondente da variavel GVE
  Parâmetros
    p: Probabilidade para obtenção do valor
  Observações
    Se(p<=0 ou p>=1;Erro) -> valor perdido
}
var
  w: Double;
Begin
  if (p>0) and (p<1) then
    begin
    w:=wsMath.Power(-Ln(p),FAlpha);
    Result:=FGamma+FLam*aux1*(1-w)
    end
  else
    Result:=wscMissValue
end; (* TwsProbGVE.Quantil *)

function TwsProbGVE.Density(const x: Double): Double;
{ Objetivo
    Obtém valor da função de densidade da distribuição de extremos generalizada
  Parâmetros
    x: Valor para obtenção
  Observações
    Se(x está fora dos limites permitidos; erro) -> valor perdido
}
var
  w : Double;
  Ok: Boolean;
Begin
  // x esta dentro dos limites?
  Ok:= ((FAlpha<0)) and (x>=aux) or ((FAlpha>0) and (x<=aux));
  if Ok then
    begin
    w:=1-aux2*(x-FGamma);
    Result := wsMath.Power(w,aux1-1)*exp(wsMath.Power(-w,aux1))/FLam
    end
  else
    Result:=0
end; (* TwsProbGVE.Density *)

procedure TwsProbGVE.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição Gumbel
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a
    variancia, o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
(*
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1]:=FGamma; v[2]:=FLam; v[3]:=FGamma+0.5772157*FLam; v[4]:=Sqr(pi*FLam)/6;
  v[5]:=Sqrt(v[4]); v[6]:=1.1395; v[7]:=5.4;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Gumbel';
    Res.Name:='MD_Gumb';
    Res.ColName[1]:='Gama'; Res.ColName[2]:='Lambda'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao';
    Res.ColName[6]:='Assimetria'; Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
*)
end;

// Estimativa por maxima verossimilhanca
function TwsProbGVE.MLE(x: TwsVec; var It: TwsGeneral; var Err: Integer;
  const epsl: Double=1E-7; UpdatePar: Boolean=True;MaxIter: Integer=25): TwsVec;

  { Objetivo
    Estimação de parâmetros da distribuição do valor extremo generalizada

    ALGORITHM AS215   APPL. STATIST. (1985) VOL. 34, NO. 3
    Modifications in AS R76 (1989) have been incorporated.

    Additional modifications by J. R. M. Hosking, August 1994: Modify steepest-ascent step
    so that it is invariant to rescaling the data.  Improves chance of convergence from
    poor initial values.
}

Label
  Steepest, Adjust;
const
  ACTI: array[1..9] of String[8] = ('NEWTON','ST.ASC','RESETK','SR.INF','SR.LIK','MAX.SR',
     'MAX.EV','MAX.IT','CONVGD');

{
ACCU,ACCA,ACCG ARE ACCURACY CRITERIA FOR TESTING CONVERGENCE.
STEPU,STEPA,STEPG ARE MAXIMUM STEPLENGTHS FOR ITERATIONS.
ACCU,ACCA,STEPU,STEPA ARE SCALED BY CURRENT VALUE OF A WHEN USED IN PROGRAM
}
 ACCU=1E-5;
 ACCA=1E-5;
 ACCG=1E-5;
 STEPU=0.5;
 STEPA=0.25;
 STEPG=0.2;
{
MAXEV IS MAX. NO. OF EVALUATIONS OF LIKELIHOOD FUNCTION
SRF IS STEPLENGTH REDUCTION FACTOR
MAXSR IS MAX. NO. OF STEPLENGTH REDUCTIONS PERMITTED PER ITERATION
C
}
  MAXEV=50;
  SRF=0.25;
  MAXSR=30;
{
SMALL IS A SMALL NUMBER, USED TO ADJUST THE SHAPE PARAMETER TO AVOID AN EXACT ZERO VALUE
  OR BORDERLINE INFEASIBILITY
ALNEG IS A LARGE NEGATIVE NUMBER, USED TO INITIALIZE LOG-LIKELIHOOD
}
  SMALL=1E-3;
  VLNEG=-1E37;
var
  A,AI,AIGI,D,DA,DAA,DAG,DELA,DELG,DELU,DG,DGG,DU,DUA,DUG,DUU,E,FF,FOLD,
  GAI,GG,GI,GIPQ,GNORM,H,HE,HH,P,PA,PQ,PQG,PU,Q,QA,QU,R,RA,RATIO,RG,G,
  RU,SE,SH,SHE,SHH,SHHE,SY,SYE,SYHE,SYYE,TEMP1,TEMP2,U,XMAX,XMIN,Y,YE,Z  : Double;
  i,n,nIter,NSR, nEval,kk,iType                                          : Integer;
  Bool0,Bool1,Bool2,Bool3                                                : Boolean;

  procedure UpPar(Add: Boolean=True);
  begin
    if Add then
      begin
      RInc(U,DELU); RInc(A,DELA); RInc(G,DELG)
      end
    else
      begin
      RInc(U,-DELU); RInc(A,-DELA); RInc(G,-DELG)
      end
  end;

(*
  procedure DoSteepest;
  begin
    ITYPE:=2;
    It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,2,FF,GNorm]));
    //---------------------------------------  NEXT 11 LINES ADDED, AUG.94
    D:=ABS(VLNEG);
    TEMP1:=D;
    IF (DU <> 0) then
      TEMP1:=STEPU/(ABS(DU)*A);
    TEMP2:=D;
    IF (DA <> 0) then
      TEMP2:=STEPA/(ABS(DA)*A);
    Z:=D;
    IF (DG <> 0) then
      Z:=STEPG/ABS(DG);
    RATIO:=MinArray([TEMP1, TEMP2, Z]);
    DELU:=RATIO*DU*A*A; DELA:=RATIO*DA*A*A; DELG:=RATIO*DG;
  end;

  procedure DoAdjust;
  begin
    UpPar;
    // TEST FOR FEASIBILITY
    Bool0:= not (A>G*(XMIN-U)) AND (A>G*(XMAX-U)) AND (G<>0);
    IF Bool0 then
      begin
      NSR:=1;
      Bool1:=True;
      while (NSR<=MAXSR) and Bool1 do
        begin
        It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,4,wscMissValue,wscMissValue]));
        UpPar(False);
        DELU:=SRF*DELU; DELA:=SRF*DELA; DELG:=SRF*DELG;
        UpPar;
        Bool1:= not (A>G*(XMIN-U)) AND (A>G*(XMAX-Y)) AND (G<>0);
        Inc(NSR);
        end
      end
  end;

  procedure Exit1;
  begin
    UpPar(False);
    It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,6,wscMissValue,wscMissValue]));
    IF (ITYPE = 1) then
      Steepest
    else
      begin
      Err:=4;
      Exit  // goto 160
      end;
  end;
*)
begin
// FIND MIN AND MAX DATA VALUE
  Err:=0;
  IF (x.Len <= 2) then
    begin
    Err:=1;   // tamanho insuficiente da amostra
    Exit
    end;
  x.MinMax(xMin,xMax);

{       INITIALIZATION
U IS LOCATION PARAMETER
A IS SCALE PARAMETER
G IS SHAPE PARAMETER
}
  It:=TwsGeneral.Create(0,8);
  It.Name:='IT_GVE';
  It.MLab:='Passos do processo iterativo';
  It.ColName[1]:='Iter'; It.ColName[2]:='EVal'; It.ColName[3]:='Gama';
  It.ColName[4]:='Lambda'; It.ColName[5]:='Alfa'; It.ColName[6]:='Acao';
  It.ColName[7]:='Log_L'; It.ColName[8]:='GNorm';
  Err:=0; nIter:=0; nEval:=0;
  Fold:=VLNeg;
  // Valores iniciais dos parametros - Ver Clarke - pag. 70
  x.VarMean(SE,SH,n);
  A:=sqrt(SH*6/pi); U:=SE-0.57721566*a; G:=0;
  IF (ABS(G) < SMALL) then
    G:=SMALL;
  IF (A <= 0) then
    A:=1;
{
CHECK WHETHER ALL DATA POINTS LIE WITHIN THE RANGE OF THE GEV DISTRIBUTION WITH THE INITIAL
PARAMETERS - IF NOT, ADJUST THE SHAPE PARAMETER SO AS TO BRING ALL POINTS WITHIN RANGE
}
  IF (G <= 0) then
    begin
    IF (xMin < U) then
      begin
      Z:=A/(xMin-u);
      IF (G <= Z) then
        begin
        It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,3,wscMissValue,wscMissValue]));
        G:=Z+SMALL;
        IF (G >= 0) then
          G:=0.5 * Z
        end
      end
    end
  else
   IF (XMAX > U) then
     begin
     Z:=A/(XMAX-U);
     IF (G >= Z) then
       begin
       It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,3]));
       G:=Z-SMALL;
       IF (G <= 0) then
         G:=0.5*Z
       end
     end;

  // Inicia loop principal do processo iterativo
  iType:=0;
  repeat
    Inc(nIter);
    NSR:=0;
    while (NEVAL < MAXEV) do  //50
      begin
      Inc(NEVAL);
      AI:=1/A; GI:=1/G; GAI:=G*AI; AIGI:=AI*GI; GG:=1-G;
      {
      ACCUMULATE SUMS OF QUANTITIES OCCURRING IN LIKELIHOOD DERIVATIVES
      IN PRESCOTT AND WALDEN'S NOTATION:
        Z IS 1 - K * (X(I)-U) / A
        Y IS THE REDUCED VARIATE - (1/K) * LOG(Z)
        E IS EXP(-Y)
        H IS EXP(K*Y)
      }
      SY:=0; SE:=0; SYE:=0; SYYE:=0; SH:=0; SHE:=0; SYHE:=0; SHHE:=0; SHH:=0;
      for I:=1 to N do
        begin
        Z:=1-GAI*(X[I]-U); Y:=-GI*Ln(Z); E:=EXP(-Y); H:=1/Z; YE:=Y*E; HE:=H*E; HH:=H*H;
        RInc(SY,Y); RInc(SE,E); RInc(SYE,YE); RInc(SYYE,Y*YE); RInc(SH,H); RInc(SHE,HE);
        RInc(SYHE,Y * HE); RInc(SHHE,HH * E); RInc(SHH,HH);
        end;

      // F IS CURRENT VALUE OF LIKELIHOOD FUNCTIONN
      FF:=-N*Ln(A)-GG*SY-SE;
      Bool2:=FF <= FOLD;
      IF Bool2 then
        begin
        // LIKELIHOOD HAS NOT INCREASED - REDUCE STEPLENGTH AND TRY AGAIN
        It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,5,FF,wscMissValue]));
        Bool0:=False;
        IF (NSR <> MAXSR) then
          begin
          repeat
            Inc(NSR);
            UpPar(False);
            DELU:=SRF * DELU; DELA:=SRF * DELA; DELG:=SRF * DELG;
            UpPar;
            Bool0:=(A>(G*(XMIN-U))) AND (A>(G*(XMAX-U))) AND (G<>0);
            if not Bool0 then
              It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,4,wscMissValue,wscMissValue]));
          until Bool0 or (NSR >= MAXSR);
          if Bool0 then
            Continue
          end
        end;
      {
      MAX. NO. OF STEPLENGTH REDUCTIONS REACHED
      IF CURRENT ITERATION IS NEWTON-RAPHSON, TRY STEEPEST ASCENT INSTEAD.  IF CURRENT
      ITERATION IS STEEPEST ASCENT, GIVE UP.
        }
      repeat           // 80
        if not Bool2 then
          begin
          UpPar(False);
          It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,6,wscMissValue,wscMissValue]));
          IF (ITYPE = 1) then
             GOTO Steepest
          else
            begin
            Err:=4;
            Exit
            end;
          end;
        Bool2:=True;
        // P,Q,R, ARE AS DEFINED IN FLOOD STUDIES REPORT
        FOLD:=FF; P:=n-SE; Q:=SHE-GG*SH; R:=n-SY+SYE; PQ:=P+Q; GIPQ:=GI*PQ;

        //FIRST DERIVATIVES OF LOG-LIKELIHOOD
        DU:=-AI*Q; DA:=-AIGI*PQ; DG:=-GI*(R-GIPQ); GNORM:=SQRT(DU*DU+DA*DA+DG*DG);

        //DERIVATIVES OF P,Q,R
        PU:=-AI*SHE; PA:=GI*PU+AIGI*SE; QU:=GG*AI*(SHHE+G*SHH); RU:=AI*(SH-SHE+SYHE);
        RA:=GI*RU-AIGI*(n-SE+SYE); RG:=GI*(SY-SYE+SYYE-A*RA); QA:=AI*Q+GI*(PU+QU);
        PQG:=GIPQ+A*(RA-GI*(PA+QA));

        // MINUS SECOND DERIVATIVE OF LOG-LIKELIHOOD (HESSIAN MATRIX)
        DUU:=AI*QU; DUA:=AIGI*(PU+QU); DAA:=-AIGI*(AI*PQ-PA-QA); DUG:=GI*(RU-GI*(PU+QU));
        DAG:=-AIGI*(GIPQ-PQG); DGG:=GI*(RG-GI*(PQG+R-GIPQ-GIPQ));

        // INVERT HESSIAN MATRIX
        for KK:=1 to 3 do
          begin
          Bool3:=DUU <= 0;
          IF Bool3 then
            GOTO Steepest;
          D:=1/DUU;
          TEMP1:=-DUA*D;
          IF (KK > 2) then
            TEMP1:=-TEMP1;
          TEMP2:=-DUG*D;
          IF (KK > 1) then
            TEMP2:=-TEMP2;
          DUU:=DAA+TEMP1*DUA; DUA:=DAG+TEMP1*DUG; DAA:=DGG+TEMP2*DUG; DUG:=TEMP1;
          DAG:=TEMP2; DGG:=D;
          end;

        // CALCULATE STEPLENGTHS
        ITYPE:=1;
        It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,1,FF,GNorm]));
        DELU:=DUU*DU+DUA*DA+DUG*DG; DELA:=DUA*DU+DAA*DA+DAG*DG; DELG:=DUG*DU+DAG*DA+DGG*DG;
        RATIO:=MaxArray([ABS(DELU)/(STEPU*A),ABS(DELA)/(STEPA * A),ABS(DELG)/STEPG]);
        IF RATIO<1 then
          GOTO Adjust;
        RATIO:=1/RATIO; DELU:=DELU*RATIO; DELA:=DELA*RATIO; DELG:=DELG*RATIO;
        GOTO Adjust;

        // HESSIAN IS NOT POSITIVE DEFINITE - MAKE A LARGE STEP IN THE DIRECTION OF STEEPEST ASCENT
    Steepest:
        ITYPE:=2;
        It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,2,FF,GNorm]));
        //---------------------------------------  NEXT 11 LINES ADDED, AUG.94
        D:=ABS(VLNEG);
        TEMP1:=D;
        IF (DU <> 0) then
          TEMP1:=STEPU/(ABS(DU)*A);
        TEMP2:=D;
        IF (DA <> 0) then
          TEMP2:=STEPA/(ABS(DA)*A);
        Z:=D;
        IF (DG <> 0) then
          Z:=STEPG/ABS(DG);
        RATIO:=MinArray([TEMP1, TEMP2, Z]);
        DELU:=RATIO*DU*A*A; DELA:=RATIO*DA*A*A; DELG:=RATIO*DG;
        // ADJUST PARAMETERS
    Adjust:
        UpPar;
        // TEST FOR FEASIBILITY
        Bool0:= not (A>G*(XMIN-U)) AND (A>G*(XMAX-U)) AND (G<>0);
        IF Bool0 then
          begin
          NSR:=1;
          Bool1:=True;
          while (NSR<=MAXSR) and Bool1 do
            begin
            It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,4,wscMissValue,wscMissValue]));
            UpPar(False);
            DELU:=SRF*DELU; DELA:=SRF*DELA; DELG:=SRF*DELG;
            UpPar;
            Bool1:= not (A>G*(XMIN-U)) AND (A>G*(XMAX-Y)) AND (G<>0);
            Inc(NSR);
            end
          end;
      until Bool0 or Bool1;    //GOTO 80
      // TEST FOR CONVERGENCE
      if not Bool0 then
        begin
        if (ABS(DELU)>ACCU*A) or (ABS(DELA)>ACCA*A) or (ABS(DELG)>ACCG)
          then Continue;
        It.MAdd(TwsDFVec.Create([NITER,NEVAL,U,A,G,9,wscMissValue,wscMissValue]));
        // Atualiza matriz de covariancias
{
        VCOV[1]:=DUU;
        VCOV[2]:=DUA;
        VCOV[3]:=DAA;
        VCOV[4]:=DUG;
        VCOV[5]:=DAG;
        VCOV[6]:=DGG;
}
        Break;
        end  // if not Bool0
      end;
  until nIter>MaxIter;  // 140 fim do loop principal

end; // ProbGVE.MLE

procedure TwsProbGVE.ReturnPeriods(x: TwsVec; F: TwsGeneral);
begin
  ShowMessage('Período de Retorno: ainda não implementado')
end;

{========================== Distribuições Discretas ==========================}

constructor TwsProbDisc.Create;
{ Objetivo
    Classe básica para tratamento de distribuições discretas. Não pode ser utilizada diretamente.
  Métodos chamados
    Create herdado
  Campos alterados
    FAccum
    FProbVal
}
begin
  inherited Create;
  FAccum := nil;
  FProbVal:=nil;
end;

destructor TwsProbDisc.Destroy;
{ Objetivo
    Libera espaço ocupado pelo objeto
  Campos liberados
    FAccum
    FProbVal
}
begin
  FAccum.Free;
  FProbVal.Free
end;

function TwsProbDisc.GetProbAccum(i: Integer): Double;
{ Objetivo
    Ao calcular uma probabilidade de uma distribuição discreta, o algoritmo o faz até aquele
    ponto e armazena todas as probabilidades num vetor de tamanho x+1, ou seja, se for feita
    a chamada Prob(4) serão calculadas todas as probabilidades de 0 a 4. Esta função retorna
    a probabilidade acumulada até a posição desejada e está associada à propriedade Accum. Se
    Upper=True retorna a probabilidade de que o valor esteja à direita.
  Parâmetros
     i: Posição para obtenção da probabilidade
  Observações
    Se i<=0 ou se i>FAccum.Len ocorre erro e retorna probabilidade nula
}
begin
  if (i>0) and (i<=FAccum.Len) then
    Result := FAccum[i]
  else
    Result := 0;
  if Upper then Result := 1-Result
end;

function TwsProbDisc.GetProb(i: Integer): Double;
{ Objetivo
    Ao calcular uma probabilidade de uma distribuição discreta, o algoritmo o faz até aquele ponto
    e armazena todas as probabilidades num vetor de tamanho i+1, ou seja, se for feita a chamada
    Prob(4) serao calculadas todas as probabilidades de 0 a 4. Esta função retorna a probabilidade
    até a posição desejada atraves da propriedade ProbVal.
  Parâmetros
    i: Posição para obtenção da probabilidade
  Observações
    Se (i<=0) ou (i>FProbVal.Len) ocorre erro e retorna valor perdido
}
begin
  if (i>0) and (i<=FProbVal.Len) then
    Result := FProbVal[i]
  else
    Result := 0
end;

function TwsProbDisc.ProbAcum(x: Double): Double;
{ Objetivo
    Se Upper=True calcula probabilidade (à direita) do valor x; calcula a probabilidade
    acumulada (à esquerda) caso contrário
  Parâmetros
    x: valor para o cálculo da probabilidade
  Observações
    * A probabilidade e calculada atraves de uma chamada ao metodo Prob, que obtém todos os
      valores de 0 a x. Assim esta rotina precisa ser chamada apenas uma vez para que todos
      os calculos de 0 a x estejam prontos. Valores dessas probabilidades acumuladas (desde
      que nenhum parâmetro da distribuição se altere) pode ser obtido atraves da propriedade
      Accum.
    * Se x<0 ocorre erro e retorna valor perdido
}
var
  XInt: Integer;
Begin
  XInt:= Trunc(x);
  If (Xint>=0) then
    Begin
    Prob(x);
    Result := FAccum[XInt+1]
    end
  else
    begin
    SetErro(1,XInt);
    Result := 0
    end;
  if Upper then Result:=1-Result
end; (* TwsProbDisc.ProbAcum *)

function TwsProbDisc.GetDistrib(x: Double): TwsGeneral;
{ Objetivo
    Retorna uma matriz com a distribuição discreta, incluindo o valor de X, probabilidades no ponto
    e as probabilidades acumuladas.
  Parâmetros
    x: Valor até o qual se deseja obter a distribuição
  Observações
    * O valor x inicialmente é nulo. Para algumas distribuições cujo espaço amostral é superiormente
    limitado (binomial, hipergeometrica, etc.) o valor desse parâmetro não é utilizado. Para outras
    (como Poisson, geometrica, etc) esse parâmetro ira indicar até que ponto se deseja obter a
    distribuição.

    * É criada uma matriz de três linhas, organizada da seguinte forma:

    linha 1 -> | Valores de X |
    linha 2 -> | ProbVal      |
    linha 3 -> | FAccum       |

    * Se x < 0 ocorre erro e função retorna nil
}
var
  i: Integer;
Begin
  if x >= 0 then
    begin
    if FAccum <> nil then
      begin
      FAccum.Free;
      FProbVal.Free
      end;
    Prob(x);
    Result := TwsGeneral.Create(3,FAccum.Len);
    Result.PrintOptions.ColWidth := 10;
    Result.PrintOptions.ColPrecision := 4;
    Result.RowName[1] := 'Y';
    Result.RowName[2] := 'Prob';
    Result.RowName[3] := 'P_Acum';
    for i:=1 to FAccum.Len do
      begin
      Result.ColName[i] := 'y'+IntToStr(i);
      Result[1,i]:= i-1;         // Valores de X
      Result[2,i]:= FProbVal[i]; // Função massa de probabilidade
      Result[3,i]:= FAccum[i]    // Função de distribuição
      end
    end
  else
    begin
    SetErro(1,x);
    Result := nil
    end
end; (* TwsProbDisc.ProbAcum *)

{ ======================= TwsProbPoisson ====================}

constructor TwsProbPoisson.Create(const ALam: Double);
{ Objetivo
    Cria objeto para tratamento da distribuição de Poisson
  Parâmetros
    ALam: Parâmetro da distribuiçã0 de Poisson.
  Observação
    Ocorre erro se ALam for negativo
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsPoisson.Create;
  inherited Create;
  SetLam(Alam);
end;

constructor TwsProbPoisson.Create;
{ Objetivo
    Cria objeto para tratamento da distribuição de Poisson sem atribuir valor ao parâmetro
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsPoisson.Create;
  inherited Create;
end;

procedure TwsProbPoisson.SetLam(const p: Double);
{ Objetivo
    Seta um valor para o parâmetro Lambda
  Parâmetros
    p: Valor para o parâmetro Lambda
  Observações
    Se p<=0 ocorre Erro
}
begin
  if p>0 then
    begin
    FLambda:=p;
    TwsPoisson(FRand).Lambda:=p
    end
  else
    SetErro(2,p)
end;

procedure TwsProbPoisson.SetPar(P: array of Double);
begin
  SetLam(P[0]);
end;

// Gera valor com distribuicao Poisson
function TwsProbPoisson.GetRandValue: Double;
begin
  Result:=TwsPoisson(FRand).Generate
end;

function TwsProbPoisson.Prob(const x: Double): Double;
{ Objetivo
    Calcular a probabilidade de que uma variável Poisson X assuma um determinado valor
  Parâmetros
    x: Valor da variavel Poisson para o qual sera calculada a probabilidade. O valor
    de x é truncado, abandonando as decimais.
  Observações
    * Pelo algoritmo utilizado, a probabilidade é calculada recursivamente. Assim, por
      exemplo, para calcular P(X=4), o algoritmo calcula todas as probabilidades de 0 a 4.
      Para aproveitar melhor esses valores, eles sao armazenados cumulativamente no vetor
      FAccum e FProbVal, que, posteriormente, poderao ser acessados, sem novos calculos,
      atraves das propriedades Accum e ProbVal.
    * Se x < 0 ocorre erro e o retorno é um valor perdido
}
Var
  i,XInt: integer;
Begin
  XInt:= Trunc(x);
  if (XInt>=0) then
    begin
    if FAccum <> nil then FAccum.Free;
    FAccum := TwsDFVec.Create(xInt+1);
    if FProbVal <> nil then FProbVal.Free;
    FProbVal := TwsDFVec.Create(xInt+1);
    Result:= exp(-FLambda);
    FAccum[1]:=Result;
    FProbVal[1]:=Result;
    for i:=1 to XInt do
      begin
      Result:= (FLambda/i)*Result;
      FProbVal[i+1]:=Result;
      FAccum[i+1]:=FAccum[i]+Result
      end;
    end
  else
    begin
    SetErro(1,XInt);
    Result := 0
    end;
  if Upper then Result := 1-Result
end; (* TwsProbPoisson.Prob *)

function TwsProbPoisson.VecProb(const x: Double): TwsVec;
{ Objetivo
    Obtém um vetor com as probabilidades P(X=0) até P(X=x). O valor de x é truncado
    e as decimais são abandonadas.
  Parâmetros
    x: Valor da variavel Poisson para o qual serão calculadas as probabilidades. Assim,
       x é o limite superior para o cálculo das probabilidades.
  Observações
    * Pelo algoritmo utilizado, a probabilidade é calculada recursivamente. Assim, por
      exemplo, para calcular P(X=4), o algoritmo calcula todas as probabilidades de 0 a 4.
    * Se x < 0 ocorre erro e o retorno é nil
}
Var
  i,XInt: integer;
Begin
  XInt:= Trunc(x);
  if (XInt>=0) then
    begin
    Result := TwsDFVec.Create(xInt+1);             // Probabilidades de 0 a XInt
    Result[1]:=exp(-FLambda);                      // P(X=0)
    for i:=1 to XInt do
      Result[i+1]:= (FLambda/i)*Result[i];         // Processo recursivo
    end
  else
    begin
    SetErro(1,XInt);
    Result := nil
    end;
  if Upper then                    // Determina a probabilidade do complemento
    for i:= 1 to Result.Len do
      Result[i] := 1-Result[i]
end; (* TwsProbPoisson.VecProb *)


function TwsProbPoisson.Distrib(x: Double = 0): TwsGeneral;
{ Objetivo
    Obtém a tabela (matriz) com a distribuição de Poisson, incluindo os valores de X, as
    probabilidades para cada valor e as probabilidades acumuladas
  Parâmetros
    x: Valor até o qual as probabilidades serao calculadas
  Observações
    Como a distribuição de Poisson não é limitada superiormente, o valor x indica até que
    ponto queremos calcular as probabilidades
}
begin
  Result := GetDistrib(x);
  Result.MLab:='Distribuição de Probabilidades - Poisson';
end;

procedure TwsProbPoisson.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição de Poisson
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1]:=FLambda; v[2]:=FLambda; v[3]:=FLambda; v[4]:=Sqrt(FLambda); v[5]:=1/v[4]; v[6]:=1/FLambda;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='Poisson';
    Res.MLab:='Medidas Descritivas - Poisson';
    Res.ColName[1] := 'Lambda'; Res.ColName[2] := 'Media'; Res.ColName[3] := 'Variancia';
    Res.ColName[4] := 'Desv_Padrao';Res.ColName[5] := 'Assimetria'; Res.ColName[6] := 'Curtose';
    end;
  Res.MAdd(v)
end;

{========================== TwsProbBernoulli ========================== }

constructor TwsProbBernoulli.Create(const p: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição de Bernoulli
  Parâmetros
    p: Probabilidade de sucesso.
  Observações
    Ocorre erro se p<0 ou p>1
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsBernoulli.Create;
  inherited Create;
  SetProbSuc(p)
end;

constructor TwsProbBernoulli.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição de Bernoulli sem atribuir valor ao parãmetro
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsBernoulli.Create;
  inherited Create;
end;

procedure TwsProbBernoulli.SetProbSuc(const p: Double);
{ Objetivo
    Atribui valor à probabilidade de sucesso
  Parâmetros
    p: valor a atribuir
  Observações
    Se p<=0 ou se p>=1 ocorre erro
}
begin
  if (p>=0) and (p<=1) then
    begin
    FProbSuc:=p;
    TwsBernoulli(Frand).ProbSuc:=p
    end
  else
    SetErro(0,p);
end;

procedure TwsProbBernoulli.SetPar(P: array of Double);
begin
  SetProbSuc(P[0]);
end;

// Gera valor com distribuicao Bernoulli
function TwsProbBernoulli.GetRandValue: Double;
begin
  Result:=TwsBernoulli(FRand).Generate
end;

function TwsProbBernoulli.Prob(const x: Double): Double;
{ Objetivo
    Calcular a probabilidade de que uma variavel Bernoulli X assuma um determinado valor.
    Se Upper for True, retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Valor da variavel bernoulli para o qual sera calculada a probabilidade. O valor
    de x e truncado, abandonando as decimais.
  Observações
    * Pelo algoritmo utilizado, a probabilidade e calculada recursivamente. Assim, por
      exemplo, para calcular P(X=4), o algoritmo calcula todas as probabilidades de 0 a 4.
      Para aproveitar melhor esses valores, eles sao armazenados cumulativamente no vetor
      FAccum e FProbVal, que, posteriormente, poderao ser acessados, sem novos calculos,
      atraves das propriedades Accum e ProbVal.
    * se x <> 0 ou se x <> 1 ocoore erro e retorna valor perdido
}
var
  XInt: Integer;
begin
  XInt := Trunc(x);
  if (XInt = 0) or (XInt = 1) then
    begin
    if FProbVal <> nil then FProbVal.Free;
    FProbVal:= TwsDFVec.Create(XInt+1);
    if FAccum <> nil then FAccum.Free;
    FAccum := TwsDFVec.Create(XInt+1);
    FProbVal[1] := 1-FProbSuc;
    FAccum[1] := FProbVal[1];
    if XInt = 1 then
      begin
      FProbVal[2] := FProbSuc;
      FAccum[2] := 1
      end;
    Result:= FProbVal[XInt+1];
    end
  else
    begin
    SetErro(1,XInt);
    Result := 0
    end;
  if Upper then Result:=1-Result
end;

function TwsProbBernoulli.VecProb(const x: Double): TwsVec;
{ Objetivo
    Calcular a probabilidade de que uma variavel Bernoulli X assuma valores até
    determinado valor. Se Upper for True, retorna a probabilidade de que não ocorram
    esses valores.
  Parâmetros
    x: Valor da variavel bernoulli para o qual sera calculada a probabilidade. O valor
    de x e truncado, abandonando as decimais.
  Observações
    * se x <> 0 ou se x <> 1 ocoore erro e retorna valor perdido
}
var
  XInt: Integer;
begin
  XInt := Trunc(x);
  if (XInt = 0) or (XInt = 1) then
    begin
    Result:= TwsDFVec.Create(XInt+1);
    Result[1] := 1-FProbSuc;
    if XInt = 1 then
      Result[2] := FProbSuc;
    end
  else
    begin
    SetErro(1,XInt);
    Result := nil
    end;
  if Upper then
    begin
    Result[1]:=1-Result[1];
    Result[2]:=1-Result[2]
    end
end; // VecProb


function TwsProbBernoulli.Distrib(x: Double = 0): TwsGeneral;
{ Objetivo
    Obtém a tabela (matriz) com a distribuição de Bernoulli, incluindo os valores de X, as
    probabilidades para cada valor e as probabilidades acumuladas
  Parâmetros
    Nao ha necessidade de indicação do valor
  Observações
    Como a distribuição de Bernoulli e limitada superiormente, nao existe a necessidade de
    indicar até qual valor de X se deseja chegar (neste caso ele e estabelecido como 1)
}
begin
  Result := GetDistrib(1);
  Result.MLab:='Distribuição de Probabilidades Bernoulli';
end;

procedure TwsProbBernoulli.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição de Bernoulli
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  q: Double;
  v: TwsDFVec;
begin
  q:=1-FProbSuc;
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1] := FProbSuc; v[2] := FProbSuc; v[3] := FProbSuc*q; v[4]:=Sqrt(v[3]);
  v[5] := (q-FProbSuc)/v[4]; v[6] := (1-3*FProbSuc*q)/(FProbSuc*q);
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 8;
    Res.PrintOptions.ColPrecision := 4;
    Res.Name:='Bernoulli';
    Res.MLab:='Medidas Descritivas - Bernoulli';
    Res.ColName[1]:='Prob_Suc'; Res.ColName[2]:='Media'; Res.ColName[3]:='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v)
end;
                                                                              
{========================== TwsProbBinomial ========================== }
constructor TwsProbBinomial.Create(nn: Integer; const p: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição binomial
  Parâmetros
    nn: Parâmetro da distribuição binomial. É o número de experimentos independentes.
    p: Parâmetros da distribuição binomial. É a probabilidade de sucesso.
  Onbservações
    Ocorre erro se nn<=0  ou se p<=0 ou p>1
}
begin
  if (not Assigned(FRand)) then
    Frand:=TwsBinomial.Create;
  inherited Create;
  SetProbSuc(p);
  SetNumExp(nn);
end;

constructor TwsProbBinomial.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição binomial sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    Frand:=TwsBinomial.Create;
  inherited Create;
end;

procedure TwsProbBinomial.SetProbSuc(const p: Double);
{ Objetivo
    Atribui o valor da probabilidade de sucesso da distribuição binomial
  Parâmetros
    p: Probabilidade de sucesso. Deve ser um valor entre 0 e 1 (exclusive); caso contrário
       gera uma mensagem de erro
  Observações
    Ocorre erro se p<=0 ou se p>=1
}
begin
  if (p>0) and (p<1) then
    begin
    Fp:= p;
    q:=1-p;
    TwsBinomial(Frand).ProbSuc:=p
    end
  else
    SetErro(0,p);
end;

procedure TwsProbBinomial.SetNumExp(nn: Integer);
{ Objetivo
    Seta o parâmetro número de experimentos na distribuição binomial
  Parâmetros
    nn: Número de experimentos. Deve ser um valor maior que zero; caso contrário uma
        mensagem de erro é gerada.
  Observações
    Ocorre erro se nn<=0
}
begin
  if nn>0 then
    begin
    Fn:=nn;
    TwsBinomial(FRand).NExp:=nn
    end
  else
    SetErro(2,nn);
end;

procedure TwsProbBinomial.SetPar(P: array of Double);
begin
  SetNumExp(Trunc(P[0]));
  SetProbSuc(P[1]);
end;

// Gera valor com distribuicao binomial
function TwsProbBinomial.GetRandValue: Double;
begin
  Result:=TwsBinomial(FRand).Generate
end;

function TwsProbBinomial.Prob(const x: Double): Double;
{ Objetivo
    Calcular a probabilidade de que uma variavel Binomial X assuma um determinado valor.
    Se Upper for True, retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Valor da variavel binomial para o qual sera calculada a probabilidade. O valor
    de x e truncado, abandonando as decimais.
  Observações
    * Pelo algoritmo utilizado, a probabilidade e calculada recursivamente. Assim, por
      exemplo, para calcular P(X=4), o algoritmo calcula todas as probabilidades de 0 a 4.
      Para aproveitar melhor esses valores, eles sao armazenados cumulativamente no vetor
      FAccum e FProbVal, que, posteriormente, poderao ser acessados, sem novos calculos,
      atraves das propriedades Accum e ProbVal.
    * Se x<0 ocorre erro e retorna valor perdido
}
Var
   XInt,i: Integer;
Begin
  XInt:= Trunc(x);
  if (XInt>=0) and (XInt<=Fn) then
    begin
    if FAccum <> nil then FAccum.Free;
    FAccum := TwsDFVec.Create(xInt+1);
    if FProbVal <> nil then FProbVal.Free;
    FProbVal := TwsDFVec.Create(XInt+1);

(*

    // algoritmo que utiliza beta incompleta
    q:=0;
    for i:=1 to xInt do
      begin
      FAccum[i]:=BinProb(Fn,i,Fp,Err,False); // P(X<=i-1)
      FProbVal[i]:=FAccum[i]-q;              // P(X<=i-1)
      q:=FAccum[i]
      end;
    if xInt<Fn then
      begin
      FAccum[xInt+1]:=BinProb(Fn,xInt+1,Fp,Err,False); // P(X<=xInt)
      FProbVal[xInt+1]:=FAccum[xInt+1]-q;              // P(X=xInt)
      end
    else
      begin
      FAccum[xInt+1]:=1;
      FProbVal[xInt+1]:=wsMath.Power(Fp,Fn)
      end;
*)

    // algoritmo que utiliza recursividade
    Result := wsMath.Power(q,Fn);
    FProbVal[1]:=Result;
    FAccum[1] := Result;
    for i:=1 to XInt do
      begin
      Result := (((Fn-i+1)/i)*(Fp/q))*Result;
      FProbVal[i+1]:=Result;
      FAccum[i+1] := FAccum[i] + Result;
      end;

    if FUpper then Result:=1-FProbVal[xInt+1]
    end
  else
    begin
    SetErro(1,XInt);
    Result := 0
    end;
end; (* TwsProbBinomial.Prob *)

function TwsProbBinomial.VecProb(const x: Double): TwsVec;
{ Objetivo
    Calcular a probabilidade de que uma variavel Binomial X assuma valores de 0 a x.
    Se Upper for True, retorna a probabilidade de que não ocorram esses valores.
  Parâmetros
    x: Valor da variavel binomial para o qual sera calculada a probabilidade. O valor
    de x e truncado, abandonando as decimais.
  Observações
    * Se x<0 ocorre erro e retorna nil
}
Var
  XInt: Integer;
  i   : integer;
Begin
  XInt:= Trunc(x);
  if (XInt>=0) then
    begin
    Result := TwsDFVec.Create(XInt+1);
    Result[1] := wsMath.Power(q,Fn);
    for i:=1 to XInt do
      Result[i+1] := (((Fn-i+1)/i)*(Fp/q))*Result[i];
    end
  else
    begin
    SetErro(1,XInt);
    Result := nil
    end;
  if FUpper then
    for i:=1 to Result.Len do
      Result[i]:=1-Result[i]
end; (* TwsProbBinomial.VecProb *)

function TwsProbBinomial.Distrib(x: Double = 0): TwsGeneral;
{ Objetivo
    Obtém a tabela (matriz) com a distribuição de Binomial, incluindo os valores de X, as
    probabilidades para cada valor e as probabilidades acumuladas
  Parâmetros
    Nao ha necessidade de indicação do valor
  Observações
    Como a distribuição de Binomial e limitada superiormente, nao existe a necessidade de indicar
    até qual valor de X se deseja chegar (neste caso, ele e estabelecido como o valor do parâmetro n)
}
begin
  Result := GetDistrib(Fn);
  Result.MLab:='Distribuição de Probabilidades Binomial';
end;

procedure TwsProbBinomial.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição binomial
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1] := NumExp; v[2] := Fp; v[3] := NumExp*Fp; v[4] := v[3]*q; v[5]:=Sqrt(v[4]);
  v[6] := (q-Fp)/v[5]; v[7] := (1-6*Fp*q)/v[4];
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 8;
    Res.PrintOptions.ColPrecision := 5;
    Res.Name:='Dist_Bin';
    Res.MLab:='Medidas Descritivas - Binomial';
    Res.ColName[1]:='Num_Exp'; Res.ColName[2]:='Prob_Suc'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;

{========================== TwsProbHyperGeom ========================== }
constructor TwsProbHyperGeom.Create(N, nn, N1: Integer);
{ Objetivo
    Cria um objeto para tratamento da distribuição hipergeométrica
  Parâmetros
    N: Parâmetro da distribuição hipergeométrica. Tamanho da população
    nn: Parâmetro da distribuição hipergeométrica. Tamanho da amostra
    N1: Parâmetro da distribuição hipergeométrica. Tamanho do grupo de sucesso.
  Observações
    Ocorre erro se ((N1<=0) ou (N1>N)) ou ((nn<=0) ou (nn>N)) ou (N<=0)
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsHypergeom.Create;
  inherited Create;
  SetPopSize(N);
  SetSucSize(N1);
  SetSampleSize(nn);
end;

constructor TwsProbHyperGeom.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição hipergeométrica sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsHypergeom.Create;
  inherited Create;
end;

procedure TwsProbHyperGeom.SetSucSize(p: Integer);
{ Objetivo
    Atribui o tamanho do grupo de sucesso
  Parâmetros
    p: tamanho do grupo de sucesso
  Observações
    Ocorre erro se p é negativo ou nulo ou se p for maior que o tamanho da população
}
begin
  if (p>0) and (p<=Fp) then
    begin
    Fm:= p;
    TwsHyperGeom(FRand).nSuc:=p
    end
  else
    SetErro(2,p)
end;

procedure TwsProbHyperGeom.SetSampleSize(nn: Integer);
{ Objetivo
    Atribui o tamanho da amostra na distribuição hipergeométrica
  Parâmetros
    nn: Tamanho da amostra
  Observações
    Ocorre erro se nn for negativo ou nulo ou se nn for maior que o tamanho da população
}
begin
  if (nn>0) and (nn<=Fp) then
    begin
    Fn:=nn;
    TwsHyperGeom(FRand).nSamp:=nn
    end
  else
    SetErro(2,nn)
end;

procedure TwsProbHyperGeom.SetPopSize(n: Integer);
{ Objetivo
    Atribui o tamanho da população
  Parâmetros
    n: tamanho da população
  Observações
    Ocorre erro se n for negativo ou nulo
}
begin
  if n>0 then
    begin
    Fp:=n;
    TwsHyperGeom(FRand).nPop:=n
    end
  else
    SetErro(2,n)
end;

procedure TwsProbHyperGeom.SetPar(P: array of Double);
begin
  SetPopSize(Trunc(P[0]));
  SetSucSize(Trunc(P[1]));
  SetSampleSize(Trunc(P[2]));
end;

// Gera valor com distribuicao hipergeometrica
function TwsProbHyperGeom.GetRandValue: Double;
begin
  Result:=TwsHyperGeom(FRand).Generate
end;

function TwsProbHyperGeom.Prob(const x: Double): Double;
{ Objetivo
    Calcular a probabilidade de que uma variavel hipergeométrica X assuma um determinado valor.
    Se Upper for True, retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Valor da variavel para o qual sera calculada a probabilidade.
  Observações
    * Ocorre erro e retorna valor perdido se ((x<0) ou (x>Min(Fn,SucSize)) ou
      (x<Max(0,Fn-Fp+SucSize)))
}
Var
  i,XInt: Integer;
  Total: Double;
begin
  XInt:= Trunc(x);
  if ((XInt>=0) and (XInt<=Math.Min(Fn,SucSize)) and (XInt>=Math.Max(0,Fn-Fp+SucSize))) then
    begin
    Total:= Combin(Fp,Fn);
    if FAccum <> nil then FAccum.Free;
    FAccum := TwsDFVec.Create(XInt+1);
    if FProbVal <> nil then FProbVal.Free;
    FProbVal := TwsDFVec.Create(XInt+1);
    // x=0
    Result:=Combin(Fp-Fm,Fn)/Total;
    FProbVal[1] := Result;
    FAccum[1] := Result;
    for i:=1 to XInt do
      begin
      Result:=((Combin(Fm,i))/Total)*Combin(Fp-Fm,Fn-i);
      FProbVal[i+1]:=Result;
      FAccum[i+1] := FAccum[i]+Result;
      end;
    end
  else
    Result := 0;
  if Upper then Result:=1-Result
end; (* TwsProbHyperGeom.Prob *)

function TwsProbHyperGeom.VecProb(const x: Double): TwsVec;
{ Objetivo
    Calcular a probabilidade de que uma variavel hipergeométrica X assuma valores até x.
    Se Upper for True, retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Valor da variavel para o qual sera calculada a probabilidade.
  Observações
    * Ocorre erro e retorna valor perdido se ((x<0) ou (x>Min(Fn,SucSize)) ou
      (x<Max(0,Fn-Fp+SucSize)))
}
Var
  XInt, i: Integer;
  Total  : Double;
begin
  XInt:= Trunc(x);
  if ((XInt>=0) and (XInt<=Min(Fn,SucSize)) and (XInt>=Max(0,Fn-Fp+SucSize))) then
    begin
    Total:= Combin(Fp,Fn);
    Result:=TwsDFVec.Create(XInt+1);
    for i:=0 to XInt do
      begin
      Result[i+1]:=((Combin(Fm,i))/Total)*Combin(Fp-Fm,Fn-i);
      if Upper then
        Result[i+1]:=1-Result[i+1]
      end
    end
  else
    Result := Nil
end; (* TwsProbHyperGeom.VecProb *)


function TwsProbHyperGeom.GetDistrib(x: Double): TwsGeneral;
{ Objetivo
    Retorna uma matriz com a distribuição discreta, incluindo o valor de X, probabilidades no ponto
    e as probabilidades acumuladas.
  Parâmetros
    x: Valor até o qual se deseja obter a distribuição
  Observações
    * O valor x inicialmente é nulo. Para algumas distribuições cujo espaço amostral é superiormente
    limitado (binomial, hipergeometrica, etc.) o valor desse parâmetro não é utilizado. Para outras
    (como Poisson, geometrica, etc) esse parâmetro ira indicar até que ponto se deseja obter a
    distribuição.

    * É criada uma matriz de três linhas, organizada da seguinte forma:

    linha 1 -> | Valores de X |
    linha 2 -> | ProbVal      |
    linha 3 -> | FAccum       |

    * Se x < 0 ocorre erro e função retorna nil
}
var
  i,nx,
  LInf,L : integer;
  aux    : Double;
begin
  LInf:= Max(0,Fn-Fp+SucSize);  // limite inferior para o valor de x
  nx:= (Fn-LInf+1);  // numero de valores x
  Result:= TwsGeneral.Create(3,nx);
  FProbVal:= TwsDFVec.Create(nx);
  FAccum:= TwsDFVec.Create(nx);
  nx:= Fp-SucSize; // dimensao do complemento
  aux:= 0;
  L:= 0;
  Result.PrintOptions.ColWidth:=10;
  Result.PrintOptions.ColPrecision:=4;
  Result.RowName[1] := 'y';
  Result.RowName[2] := 'Probab';
  Result.RowName[3] := 'Prob_Acum';
  for i:= LInf to Fn do
    begin
    inc(L);
    Result.ColName[L] := 'y'+IntToStr(L);
    Result[1,L]:= i;
    Result[2,L]:= Prob(i);
    Aux:= Aux+Result[2,L];
    Result[3,L]:=Aux;
    FProbVal[L]:= Result[2,L];
    FAccum[L]:= Aux;
    end;
end;

function TwsProbHyperGeom.PAccum(x: Integer): Double;
{  Objetivo
     Determinar probabilidades acumuladas na distribuição hipergeométrica
   Parâmetros
     x: Valor para obtenção da probabilidade
}
var
  i,LInf : Integer;
  aux    : Double;
begin
  Result:= 0;
  LInf:= Min(x,Fn);
  for i:= 0 to LInf do
    begin
    aux:= Prob(i);
    if not(wsGlib.IsMissValue(Aux)) then
       Result:= Result+Aux;
    end;
  if FUpper then Result:=1-Result
end;

function TwsProbHyperGeom.Distrib(x: Double): TwsGeneral;
{ Objetivo
    Obtém a tabela (matriz) com a distribuição hipergeométrica, incluindo os valores de X, as
    probabilidades para cada valor e as probabilidades acumuladas
  Parâmetros
    Nao ha necessidade de indicação do valor
  Observações
    Como a distribuição hipergeométrica é limitada superiormente, nãoo existe a necessidade de indicar
    até qual valor de X se deseja chegar (neste caso, ele é estabelecido como o valor do parâmetro Fn)
}
begin
  Result:=GetDistrib(Fn);
  Result.MLab:='Distribuição de Probabilidades Hipergeométrica';
end;

procedure TwsProbHyperGeom.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição hipergeométrica
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  v: TwsDFVec;
  p: Double;
begin
  v:=TwsDFVec.Create(8);
  v.Name:='Valores';
  p:=Fm/Fp;
  v[1]:=Fp; v[2]:=Fm; v[3]:=Fn; v[4]:=Fn*p; v[5] := v[4]*(1-p)*(1-((Fn-1)/(Fp-1)));
  v[6] := Sqrt(v[5]); v[7] := ((1-2*p)/Sqrt(Fn*p*(1-p)))*Sqrt((Fp-1)/(Fp-Fm))*((Fp-2*Fn)/(Fp-2));
  v[8] := wscMissValue;
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,8);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Hipergemétrica';
    Res.Name:='Dist_Hipergeo';
    Res.ColName[1]:='NPop'; Res.ColName[2]:='m'; Res.ColName[3]:='n';
    Res.ColName[4]:='Media'; Res.ColName[5]:='Variancia'; Res.ColName[6]:='Desv_Padrao';
    Res.ColName[7]:='Assimetria'; Res.ColName[8] := 'Curtose';
    end;
  Res.MAdd(v)
end;

{ =========================== Binomial Negativa ========================}

constructor TwsProbBinNeg.Create(const r, p: Double);
{ Objetivo
    Cria objeto para tratamento da distribuição binomial negativa
  Parâmetros
    r: Parâmetro da distribuição. Representa o número de sucessos
    p: Parâmetro da distribuição. Probabilidade de sucesso de cada experimento.
  Observações
    Ocorre erro se (r<=0) ou (p<=0 ou p>1)
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsNegBin.Create;
  inherited Create;
  SetProbSuc(p);
  SetNumSuc(r);
end;

constructor TwsProbBinNeg.Create;
{ Objetivo
    Cria objeto para tratamento da distribuição binomial negativa sem atribuir valores aos
    parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsNegBin.Create;
  inherited Create;
end;

procedure TwsProbBinNeg.SetProbSuc(const p: Double);
{ Objetivo
    Atribui valor para a probabilidade de sucesso
  Parâmetros
    p: Probabilidade de sucesso
  Observações
    Ocorre erro se p<=0 ou p>=1
}
begin
  if (p>0) and (p<1) then
    begin
    Fp:= p;
    TwsNegBin(FRand).ProbSuc:=p
    end
  else
    SetErro(0,p);
end;

procedure TwsProbBinNeg.SetNumSuc(const r: Double);
{ Objetivo
    Atribui valor ao número de sucessos
  Parâmetros
    r: número de sucessos
  Observações
    Ocorre erro se r <=0
}
begin
  if r > 0 then
    begin
    Fr := r;
    TwsNegBin(FRand).NExp:=r
    end
  else
    SetErro(2,r)
end;

procedure TwsProbBinNeg.SetPar(P: array of Double);
begin
  SetNumSuc(P[0]);
  SetProbSuc(P[1]);
end;

// Gera valor com distribuicao binomial negativa
function TwsProbBinNeg.GetRandValue: Double;
begin
  Result:=TwsNegBin(FRand).Generate
end;

function TwsProbBinNeg.Prob(const x: Double): Double;
{ Objetivo
    Calcular a probabilidade de que uma variavel binomial negativa X assuma um determinado
    número de fracassos até que um certo número de sucessos ocorra. Se Upper for True, retorna a
    probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Número de fracassos até que se alcance o número de sucessos desejados. Se o valor de x
       possui decimais, elas sao abandonadas.
  Observações
    * Pelo algoritmo utilizado, a probabilidade e calculada recursivamente. Assim, por
      exemplo, para calcular P(X=4; NumSuc, ProbSuc), o algoritmo calcula todas as
      probabilidades de 0 a 4, colocando os valores em FProbVal e os acumulados em FAccum.
      Para aproveitar melhor esses valores, eles sao armazenados cumulativamente no vetor
      FAccum e os valores individuais sao armazenados em FProbVal. As propriedades Accum e
      ProbVal possibilitam o acesso a esses valores.
    * Se (x+r-1) > 0 ocorre erro e retorna valor perdido
}
Var
   i,XInt: Integer;
   q: Double;
Begin
  XInt:= Trunc(x);
  if (XInt+Fr-1) > 0 then
    begin
    if FAccum <> nil then FAccum.Free;
    FAccum := TwsDFVec.Create(XInt+1);
    if FProbVal <> nil then FProbVal.Free;
    FProbVal := TwsDFVec.Create(XInt+1);
    q := 1-Fp;
    Result:= wsMath.Power(Fp,Fr);
    FProbVal[1]:=Result;
    FAccum[1] := Result;
    for i:=1 to XInt do
      begin
      Result:= ((Fr+i-1)/i)*q*Result;
      FProbVal[i+1]:=Result;
      FAccum[i+1] := FAccum[i]+Result
      end;
    end
  else
    begin
    SetErro(1,XInt);
    Result := 0
    end;
  if FUpper then Result:=1-Result
end; (* TwsProbBinNeg.Prob *)

function TwsProbBinNeg.VecProb(const x: Double): TwsVec;
{ Objetivo
    Calcular a probabilidade de que uma variavel binomial negativa X valores de 0 a x.
    Se Upper for True, retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Número de fracassos até que se alcance o número de sucessos desejados. Se o valor de x
       possui decimais, elas sao abandonadas.
  Observações
    * Se (x+r-1) > 0 ocorre erro e retorna valor perdido
}
Var
   i,XInt: Integer;
   q     : Double;
Begin
  XInt:= Trunc(x);
  if (XInt+Fr-1) > 0 then
    begin
    Result := TwsDFVec.Create(XInt+1);
    q := 1-Fp;
    Result[1]:= wsMath.Power(Fp,Fr);
    for i:=1 to XInt do
      Result[i+1]:= ((Fr+i-1)/i)*q*Result[i];
    end
  else
    begin
    SetErro(1,XInt);
    Result := nil
    end;
  if FUpper then
    for i:=1 to Result.Len do
      Result[i]:=1-Result[i]
end; (* TwsProbBinNeg.VecProb *)


function TwsProbBinNeg.Distrib(x: Double = 0): TwsGeneral;
{ Objetivo
    Obtém a tabela (matriz) com a distribuição Binomial Negativa, incluindo os valores de X, as
    probabilidades para cada valor e as probabilidades acumuladas
  Parâmetros
    x: Valor até o qual as probabilidades serao calculadas
  Observações
    Como a distribuição Binomial Negativa não e limitada superiormente, existe a necessidade de
    indicar até qual valor de X se deseja chegar
}
begin
  Result := GetDistrib(x);
  Result.MLab:='Distribuição de Probabilidades Binomial Negativa';
end;

procedure TwsProbBinNeg.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição binomial negativa
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  q: Double;
  v: TwsDFVec;
begin
  q:=1-Fp;
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  v[1] := Fr; v[2] := Fp; v[3] := Fr*q/Fp; v[4] := v[3]/Fp; v[5] := Sqrt(v[4]);
  v[6] := (1+q)/Sqrt(Fr*q); v[7] := (1+q*(3*Fr+4+q))/(Fr*q);
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='Dist_BinNeg';
    Res.MLab:='Medidas Descritivas - Binomial Negativa';
    Res.ColName[1]:='Num_Suc'; Res.ColName[2]:='Prob_Suc'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;

{========================== TwsProbGeometric =============================}

constructor TwsProbGeometric.Create(const p: Double);
{ Objetivo
    Cria um objeto para tratamento da distribuição geométrica
  Parâmetros
    p: Parâmetro da distribuição (probabilidade de sucesso).
  Observação
    Ocorre erro se (p<=0)
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGeometric.Create;
  inherited Create;
  SetPb(p);
end;

constructor TwsProbGeometric.Create;
{ Objetivo
    Cria um objeto para tratamento da distribuição geométrica sem atribuir valor para
    o parâmetro
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsGeometric.Create;
  inherited Create;
end;

procedure TwsProbGeometric.SetPb(const p: Double);
{ Objetivo
    Atribui valor à probabilidade de sucesso
  Parâmetros
    p: probabilidade de sucesso
  Observações
    Ocorre erro se p<=0 ou p>=1
}
begin
  if (p>0) and (p<1) then
    begin
    Fp:= p;
    TwsGeometric(FRand).ProbSuc:=p
    end
  else
    SetErro(0,p)
end;

procedure TwsProbGeometric.SetPar(P: array of Double);
begin
  SetPb(P[0]);
end;

// Gera valor com distribuicao geometrica
function TwsProbGeometric.GetRandValue: Double;
begin
  Result:=TwsGeometric(FRand).Generate
end;

function TwsProbGeometric.Prob(const x: Double): Double;
{ Objetivo
    Calcular a probabilidade de que uma variavel geométrica X assuma um determinado valor.
    Se Upper for True, retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Valor da variavel binomial para o qual sera calculada a probabilidade. O valor
    de x e truncado, abandonando as decimais.
  Observações
    * Pelo algoritmo utilizado, a probabilidade e calculada recursivamente. Assim, por
      exemplo, para calcular P(X=4), o algoritmo calcula todas as probabilidades de 1 a 4.
      Para aproveitar melhor esses valores, eles sao armazenados cumulativamente no vetor
      FAccum e FProbVal, que, posteriormente, poderao ser acessados, sem novos calculos,
      atraves das propriedades Accum e ProbVal.
    * Se x<0 ocorre erro e retorna valor perdido
}
Var
  XInt,i: integer;
  q: Double;
Begin
  XInt:= Trunc(x);
  if (XInt>0) then
    begin
    if FAccum <> nil then FAccum.Free;
    FAccum := TwsDFVec.Create(XInt);
    if FProbVal <> nil then FProbVal.Free;
    FProbVal := TwsDFVec.Create(XInt);
    q := 1-Fp;
    Result:=Fp;
    FAccum[1] := Result;
    FProbVal[1]:=Result;
    for i:=1 to XInt-1 do
      begin
      Result:=wsMath.Power(q,i)*Fp;
      FProbVal[i+1]:=Result;
      FAccum[i+1]:=FAccum[i]+Result
      end;
    end
  else
    begin
    SetErro(1,XInt);
    Result := 0
    end;
  if FUpper then Result:=1-Result
end; (* TwsProbGeometric.Prob *)

function TwsProbGeometric.VecProb(const x: Double): TwsVec;
{ Objetivo
    Calcular a probabilidade de que uma variavel geométrica X assuma um determinado valor.
    Se Upper for True, retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    x: Valor da variavel binomial para o qual sera calculada a probabilidade. O valor
    de x e truncado, abandonando as decimais.
  Observações
    * Pelo algoritmo utilizado, a probabilidade e calculada recursivamente. Assim, por
      exemplo, para calcular P(X=4), o algoritmo calcula todas as probabilidades de 0 a 4.
      Para aproveitar melhor esses valores, eles sao armazenados cumulativamente no vetor
      FAccum e FProbVal, que, posteriormente, poderao ser acessados, sem novos calculos,
      atraves das propriedades Accum e ProbVal.
    * Se x<0 ocorre erro e retorna valor perdido
}
Var
  XInt,i: integer;
  q: Double;
Begin
  XInt:= Trunc(x);
  if (XInt>0) then
    begin
    Result := TwsDFVec.Create(XInt);
    q := 1-Fp;
    Result[1]:=Fp;
    for i:=1 to XInt-1 do
      Result[i+1]:=q*Result[i];
    end
  else
    begin
    SetErro(1,XInt);
    Result := nil
    end;
  if FUpper then
    for i:=1 to Result.Len do
      Result[i]:=1-Result[i]
end; (* TwsProbGeometric.VecProb *)


function TwsProbGeometric.Distrib(x: Double = 0): TwsGeneral;
{ Objetivo
    Obtém a tabela (matriz) com a distribuição de Geometrica, incluindo os valores de X, as
    probabilidades para cada valor e as probabilidades acumuladas
  Parâmetros
    x: Valor até o qual as probabilidades serao calculadas
  Observações
    Como a distribuição de Geometrica nao limitada superiormente, existe a necessidade de indicar
    até qual valor de X se deseja chegar
}
begin
  Result := GetDistrib(x);
  Result.MLab:='Distribuição de Probabilidades Geométrica';
end;

procedure TwsProbGeometric.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição geométrica
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  q: Double;
  v: TwsDFVec;
begin
  q:=1-Fp;
  v:=TwsDFVec.Create(6);
  v.Name:='Valores';
  v[1] := Fp; v[2] := 1/Fp; v[3] := q/Sqr(Fp); v[4] := Sqrt(v[3]); v[5] := (2-Fp)/Sqrt(q);
  v[6] := 6+Sqr(Fp)/q;
  if Res=nil then
    begin
    Res := TwsGeneral.Create(0,6);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.MLab:='Medidas Descritivas - Geométrica';
    Res.Name:='Dist_Geom';
    Res.ColName[1]:='Prob_Suc'; Res.ColName[2]:='Media'; Res.ColName[3]:='Variancia';
    Res.ColName[4]:='Desv_Padrao'; Res.ColName[5]:='Assimetria'; Res.ColName[6]:='Curtose';
    end;
  Res.MAdd(v);
end;

{ ========================= TwsProbUniDisc ========================= }

constructor TwsProbUniDisc.Create(Fa, Fb: Integer);
{ Objetivo
    Cria um objeto para tartamento de distribuições uniformes discretas
  Parâmetros
    Fa: limite inferior
    Fb: limite superior
  Observações
    Ocorre erro se Fb<Fa
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsDiscreteUnif.Create;
  inherited Create;
  SetAlpha(Fa);
  SetBeta(Fb);
end;

constructor TwsProbUniDisc.Create;
{ Objetivo
    Cria um objeto para tratamento de distribuições uniformes discretas sem atribuir
    valores para os parâmetros
}
begin
  if (not Assigned(FRand)) then
    FRand:=TwsDiscreteUnif.Create;
  inherited Create;
end;

procedure TwsProbUniDisc.SetAlpha(p: Integer);
{ Objetivo
    Atribui valor ao extremo inferior
  Parâmetros
    p: valor a atribuir
}
begin
  FAlpha:= p;
  TwsDiscreteUnif(FRand).Min:=p
end;

procedure TwsProbUniDisc.SetBeta(p: Integer);
{ Objetivo
    Atribui valor ao extremo inferior. Deve ser chamada depois de SetAlpha
  Parâmetros
    p: valor a atribuir
  Observação
    Ocorre erro se o extremo superior for menor que o inferior
}
begin
  if p >= FAlpha then
    begin
    FBeta:= p;
    TwsDiscreteUnif(FRand).Max:=p
    end
  else
     SetErro(2,p);
end;

procedure TwsProbUniDisc.SetPar(P: array of Double);
begin
  SetAlpha(Trunc(P[0]));
  SetBeta(Trunc(P[1]));
end;

// Gera valor com distribuicao uniforme discreta
function TwsProbUniDisc.GetRandValue: Double;
begin
  Result:=TwsDiscreteUnif(FRand).Generate
end;

function TwsProbUniDisc.Distrib(x: Double): TwsGeneral;
{ Objetivo
    Obtém a tabela (matriz) com a distribuição uniforme discreta, incluindo os valores de X, as
    probabilidades para cada valor e as probabilidades acumuladas
  Parâmetros
    x: Valor até o qual as probabilidades serão calculadas
  Observações
    Como a distribuição uniforme discreta é limitada superiormente, o último valor considerado
    é o limite superior não havendo, portanto, necessidade da sua indicação.
}
begin
  Result:= GetDistrib(FBeta);
  Result.MLab:='Distribuição de Probabilidades Uniforme Discreta';
end;

// porque nao utiliza a getdistrib herdadada
function TwsProbUniDisc.GetDistrib(x: integer): TwsGeneral;
{ Objetivo
    Retorna uma matriz com a distribuição discreta, incluindo o valor de X, probabilidades no ponto
    e as probabilidades acumuladas.
  Parâmetros
    x: Valor até o qual se deseja obter a distribuição
  Observações
    * O valor x inicialmente é nulo. Para algumas distribuições cujo espaço amostral é superiormente
    limitado (binomial, hipergeometrica, etc.) o valor desse parâmetro não é utilizado. Para outras
    (como Poisson, geometrica, etc) esse parâmetro ira indicar até que ponto se deseja obter a
    distribuição.

    * É criada uma matriz de três linhas, organizada da seguinte forma:

    linha 1 -> | Valores de X |
    linha 2 -> | ProbVal      |
    linha 3 -> | FAccum       |

    * Se x < 0 ocorre erro e função retorna nil
}
var Aux    : Double;
    i,L,Amp: integer;
begin
  Amp:=FBeta-FAlpha+1;
  Result:= TwsGeneral.Create(3,Amp);
  FProbVal:= TwsDFVec.Create(Amp);
  FAccum:= TwsDFVec.Create(Amp);
  L:= 0;
  Aux:= 0;
  Result.PrintOptions.ColWidth:=10;
  Result.PrintOptions.ColPrecision:=4;
  Result.RowName[1] := 'Y';
  Result.RowName[2] := 'Probab';
  Result.RowName[3] := 'Prob_Acum';
  for i:= FAlpha to FBeta do
      begin
      inc(L);
      Result.ColName[L] := 'y'+IntToStr(L);
      Result[1,L]:= i;
      Result[2,L]:= Prob(i);
      Aux:= Aux+Result[2,L];
      Result[3,L]:=Aux;
      FProbVal[L]:= Result[2,L];
      FAccum[L]:= Aux;
      end;
end;

procedure TwsProbUniDisc.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição uniforme discreta
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  i,n,Amp: integer;
  m4     : double;
  v      : TwsDFVec;
begin
  v:=TwsDFVec.Create(7);
  v.Name:='Valores';
  Amp:=FBeta-FAlpha+1;
  v[1] := FAlpha; v[2] := FBeta; v[3] := (FAlpha+FBeta)/2; v[4] := (Sqr(Amp)-1)/12; v[5] := Sqrt(v[4]);
  v[6] := 0;
  m4:=0;
  for i:=FAlpha to FBeta do
    m4:=m4+wsMath.Power(i-v[3],4);
  v[7] := m4/(Amp)/Sqr(v[4]);
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,7);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='Dist_UniDisc';
    Res.MLab:='Medidas Descritivas - Uniforme Discreta';
    Res.ColName[1]:='Alfa'; Res.ColName[2]:='Beta'; Res.ColName[3]:='Media';
    Res.ColName[4]:='Variancia'; Res.ColName[5]:='Desv_Padrao'; Res.ColName[6]:='Assimetria';
    Res.ColName[7]:='Curtose';
    end;
  Res.MAdd(v)
end;

function TwsProbUniDisc.PAccum(x: Double): Double;
{  Objetivo
     Determinar probabilidades acumuladas na distribuição uniforme discreta
   Parâmetros
     x: Valor para obtenção da probabilidade
}
begin
  if (x >= FAlpha) and (x <= FBeta) then
     Result:= (x-FAlpha+1)/(FBeta-FAlpha+1)
  else
     Result:= 0;
  if FUpper then Result:=1-Result
end;

function TwsProbUniDisc.Prob(const x: Double): Double;
{  Objetivo
     Determinar probabilidades na distribuição uniforme discreta
   Parâmetros
     x: Valor para obtenção da probabilidade
   Observações
     Ocorre erro e retorna 0 se x<FAlpha ou x>FBeta
}
begin
  if (x >= FAlpha) and (x <= FBeta) then
     Result:= 1/(FBeta-FAlpha+1)
  else
     Result:= 0;
end;

function TwsProbUniDisc.VecProb(const x: Double): TwsVec;
{  Objetivo
     Determinar probabilidades na distribuição uniforme discreta
   Parâmetros
     x: Valor para obtenção da probabilidade
   Observações
     Ocorre erro e retorna valor perdido se x<FAlpha ou x>FBeta
}
var
  Amp: Integer;
begin
  if (x >= FAlpha) and (x <= FBeta) then
    begin
    Amp:=FBeta-FAlpha+1;
    Result:= VecConst(1/Amp,Amp)
    end
  else
    Result:= nil
end;

{ ============================== TwsProbUCont =========================== }

constructor TwsProbUCont.Create(const Expr: string; const LowLimit, HighLimit: Double);
{ Objeto
    Cria objeto que permite tratamento de distribuições contínuas definidas pelo usuário
  Parâmetros
    Expr: expressão que corresponde à função de densidade. Deve ser especificada na forma de
          uma expressão WinStat
    LowLimit: limite inferior do intervalo que define o espaço amostral
    HighLimit: limite superior do intervalo que define o espaço amostral
}
var V : Double;
begin
  inherited Create;
  FIntegral:= TwsMISimpson.Create(Expr, LowLimit, HighLimit);
  FIntegral.LowLimit:= LowLimit;
  FIntegral.HighLimit:= HighLimit;
  v:= FIntegral.Calculate;
  if Abs(1-v)>1e-6 then
     SetErro(2,v)
  else
    begin
    FAvaliador:= TAvaliator.Create;
    FAvaliador.TabVar.AddFloat('x', 0);
    FExpr:= Expr;
    FHL:= HighLimit;
    FLL:= LowLimit
    end
end;

destructor TwsProbUCont.Destroy;
{  Objetivo
     Libera espaço ocupado pelo objeto
   Campos liberados
     FAvaliator
     FIntegral
}
begin
  FAvaliador.Free;
  FIntegral.Free;
  inherited Destroy;
end;

Procedure TwsProbUCont.SetErro(num: word; const p: Double);
var
  msg: String;
Begin
  case num of
    0 : msg := 'Valor de probabilidade fora da amplitude (0, 1)';
    1 : msg := 'Valor da variável fora da amplitude permitida';
    2 : msg := 'Esta não é uma distribuição de probabilidade válida.'
   end;

  if not wsGLib.IsMissValue(p) then
     Msg := msg + #13#13'Valor = ' + FloatToStrF(p,ffGeneral,12,7);

  raise ERangeError.Create(msg);
end;

function TwsProbUCont.Prob(const H : Double): Double;
{ Objetivo
    Calcula o valor da funçao de distribuiçao nos limites estabelecidos
  Parâmetros
    H: Limite superior
}
begin
  SetSubLimit(FLL,H);
  Result:= FIntegral.Calculate;
end;

function TwsProbUCont.ProbInt(const L,H : Double): Double;
{ Objetivo
    Calcula a probabilidade de que a variável assuma valores entre os limites estabelecidos
  Parâmetros
    L: Limite inferior
    H: Limite superior
}
begin
  SetSubLimit(L,H);
  Result:= FIntegral.Calculate;
end;

function TwsProbUCont.VecDensity(x1,x2: Double; n: Integer; Var xVec: TwsVec): TwsVec;
{ Objetivo
    Retorna um vetor com os valores da função de densidade da distribuição e um vetor com os
    valores das abcissas correspondentes
  Parâmetros
    x1,x2: Limite inferior e superior do intervalo
    n: número de valores a calcular
    xVec: retorna valores da variável
}
var
  Incr: Double;
  j   : Integer;
  hx  : Boolean;
begin
  if xVec=nil then
    begin
    xVec:= TwsDFVec.Create(n);
    Incr:= (x2-x1)/n;
    xVec[1]:=x1;
    hx:=True
    end
  else
    hx:=false;
  Result:=TwsDFVec.Create(n);
  FAvaliador.Expression:= FExpr;
  for j:=1 to n-1 do
    begin
    FAvaliador.TabVar.SetFloatValue('x',xVec[j]);
    Result[j]:= FAvaliador.Evaluate.AsFloat;
    if hx then
      xVec[j+1]:= xVec[j] + Incr;
    end;
  FAvaliador.TabVar.SetFloatValue('x',xVec[n]);
  Result[n]:= FAvaliador.Evaluate.AsFloat;
end;

procedure TwsProbUCont.SetSubLimit(const LowLimit, HighLimit: Double);
{ Objetivo
    Atribui valor aos limites de integração para o cálculo de probabilidades
  Parâmetros
    LowLimit: limite inferior
    HighLimit: limite superior
  Observações
    Ocorre erro se LowLimit<FLL ou se HigLimit>HLL ou se LowLimit>=HighLimit
}
begin
  if (LowLimit < FLL) then
     SetErro(1,LowLimit)
  else
     if (HighLimit > FHL) or (HighLimit<=LowLimit) then
        SetErro(1,HighLimit)
     else
        begin
        FIntegral.LowLimit   := LowLimit;
        FIntegral.HighLimit  := HighLimit;
        FIntegral.Expression := FExpr;
        end
end;

function TwsProbUCont.Density(const xValue: Double): Double;
{ Objetivo
    Obtém o valor da função de densidade
  Parâmetros
    xValue: valor da variável
  Observações
    Ocorre erro se (xValue<FLL) ou (xValue>FHL)
}
begin
  if ((xValue < FLL) or (xValue > FHL)) then
     SetErro(1,xValue)
  else
     begin
     FAvaliador.TabVar.SetFloatValue('x',xValue);
     FAvaliador.Expression:= FExpr;
     Result:= FAvaliador.Evaluate.AsFloat
     end
end;

procedure TwsProbUCont.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição contínua definida pelo usuário
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  Integral: TwsIntegral;
  m1,m2,
  m3,m4   : Double;
  v       : TwsDFVec;
begin
  Integral:= TwsMISimpson.Create(FExpr,Fll,FHl);
  v:=TwsDFVec.Create(5);
  v.Name:='Valores';
  //Média
  Integral.Expression:= Integral.Expression + '*x';
  m1:=Integral.Calculate;
  v[1]:=m1;
  //Variância
  Integral.Expression:= Integral.Expression + '*x';
  m2:=Integral.Calculate;
  v[2]:=m2-Sqr(m1);
  // Desvio padrao
  v[3]:=Sqrt(v[2]);
  //Assimetria
  Integral.Expression:=Integral.Expression + '*x';
  m3:=Integral.Calculate;
  v[4]:=m3-3*m1*m2+2*(wsMath.Power(m1,3));
  v[4]:=v[4]/wsMath.Power(v[2],1.5);
  //Curtose
  Integral.Expression:=Integral.Expression + '*x';
  m4:=Integral.Calculate;
  v[5]:=m4-4*m3*m1+6*m2*Sqr(m1)-3*wsMath.Power(m1,4);
  v[5]:=v[5]/Sqr(v[2]);
  v.Name:='Valores';
  if Res=nil then
    begin
    Res:=TwsGeneral.Create(0,5);
    Res.Name:='Dist_UCont';
    Res.MLab:='Medidas Descritivas - Contínua Definida';
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.ColName[1]:='Media'; Res.ColName[2]:='Variancia'; Res.ColName[3]:='Desv_Padrao';
    Res.ColName[4]:='Assimetria'; Res.ColName[5]:='Curtose';
    end;
  Res.MAdd(v)
end;

{ ============================ TwsProbUDiscreta =========================== }

constructor TwsProbUDisc.Create(PMat: TwsGeneral);
{ Objetivo
    Cria um objeto para tratamento de uma distribuição discreta definida pelo usuário. A definição
    se dá através de uma matriz com pelo menos duas linhas, tendo na primeira os valores da
    variável discreta e na segunda as respectivas probabilidades. Para que essa matriz represente
    uma distribuição discreta, os valores da segunda linha (probabilidades) não podem ser
    negativos, devem estar entre 0 e 1 e a sua soma deve ser igual a 1.
  Parâmetros
    PMat: matriz geral com pelo menos duas linhas que representa a distribuição
  Observações
    * Ocorre erro se pelo menos uma das condições não for satisfeita.
    * As probabilidades acumuladas são obtidas e incluídas como a última linha da matriz
}
var i,j   : integer;
    Acum,x: Double;
    vAccum: TwsDFVec;
begin
  //verificação dos parâmetros
  if (PMat.NRows>=2) then
    begin
    inherited Create;
    // Para as probabilidades acumuladas
    vAccum:=TwsDFVec.Create(PMat.NCols);
    // Faz uma copia para armazenamento interno
    PMat.Copy(mtGeneral, TwsMatrix(FProbMat));
    Acum:= 0;
    for j:= 1 to FProbMat.Ncols do
      begin
      x := FProbMat[2,j];
      if (x>=0) and (x<=1) then
        begin
        Acum := Acum+x;
        vAccum[j]:=Acum
        end
      else
         Raise Exception.Create('Distribuição de probabilidade inválida'+
                                 #13 + '(Valor fora do intervalo [0,1])');
      end;
    if not FEquals(Acum,1) then
      Raise Exception.Create('Distribuição de probabilidade inválida'+
                             #13 + '(Soma das probabilidades diferente de 1)');
    vAccum.Name:='ProbAcum';
    FProbMat.MAdd(vAccum)
    end
  else
    Raise Exception.Create('Matriz não possui o número correto de linhas (2)');
end;

destructor TwsProbUDisc.Destroy;
{ Objetivo
    Libera espaço ocupado pelo objeto
  Campos liberados
    FProbMat
}
begin
  inherited Destroy;
//libera matriz que define a distribuição
  FProbMat.Free;
end;

procedure TwsProbUDisc.Descriptive(var Res: TwsGeneral);
{ Objetivo
    Retorna uma matriz com todas as medidas descritivas da distribuição discreta definida pelo usuário
  Parâmetros
    Res: Matriz geral que retorna na primeira linha os valores dos parâmetros, a media, a variancia,
         o coeficiente de assimetria e o coeficiente de curtose.
}
var
  i,j    :  integer;
  m1,m2,
  m3,m4,x:  Double;
  v      : TwsDFVec;
begin
  m1:=0; m2:=0; m3:=0; m4:=0;
  for j:= 1 to FProbMat.NCols do
    begin
    x := FProbMat[1,j]*FProbMat[2,j];
    m1 := m1+x;        // Momento ordinario de ordem 1
    x:=x*FProbMat[1,j];
    m2 := m2+x;          // Momento ordinario de ordem 2
    x:=x*FProbMat[1,j];
    m3 := m3+x;          // Momento ordinario de ordem 3
    x:=x*FProbMat[1,j];
    m4 := m4+x;          // Momento ordinario de ordem 4
    end;
  v:=TwsDFVec.Create(5);
  v.Name:='Valores';
  v[1]:= m1;                                       // Média
  v[2]:= m2-Sqr(m1);                               // Variancia
  v[3]:= Sqrt(v[2]);                               // Desvio padrao
  v[4]:= m3-3*m1*m2+2*wsMath.Power(m1,3);
  v[4]:= v[4]/wsMath.Power(v[2],1.5);       // Assimetria
  v[5]:= m4-4*m3*m1+6*m2*Sqr(m1)-3*wsMath.Power(m1,4);
  v[5]:= v[5]/Sqr(v[2]);                           // Curtose
  v.Name:='Valores';
  if Res=nil then
    begin
    Res:= TwsGeneral.Create(0,5);
    Res.PrintOptions.ColWidth := 12;
    Res.PrintOptions.ColPrecision := 7;
    Res.Name:='Dist_UDisc';
    Res.MLab:='Medidas Descritivas - Discreta Definida';
    Res.ColName[1]:= 'Media';
    Res.ColName[2]:= 'Variancia';
    Res.ColName[3]:= 'Desv_Padrao';
    Res.ColName[4]:= 'Assimetria';
    Res.ColName[5]:= 'Curtose';
    end;
  Res.MAdd(v);
end;

function TwsProbUDisc.Prob(const x: Double): Double;
{ Objetivo
    Calcular a probabilidade de que da variavel X assumir um determinado valor. Se Upper for True,
    retorna a probabilidade de não ocorrer esse valor.
  Parâmetros
    xValue: Valor da variável para o qual será calculada a probabilidade.
}
var i: integer;
begin
  // Localiza na linha 1 o valor de X
  if FProbMat.LocateAtRow(x,1,i) then
    Result:=FProbMat[2,i]      // Pega a probabilidade na linha 2
  else
    Result:=0;
end;

function TwsProbUDisc.ProbAcum(XValue: Double): Double;
{ Objetivo
    Calcular a probabilidade acumulada até um determinado valor. Para Upper = True, retorna a
    probabilidade de ocorrer um valor à sua direita.
  Parâmetros
    xValue: Valor da variável para o qual será calculada a probabilidade.
}
var
  i: integer;
begin
  // Localiza na linha 1 o valor de X
  if FProbMat.LocateAtRow(xValue,1,i) then
    Result:=FProbMat[FProbMat.NRows,i]  // acumulada esta na ultima linha
  else
    Result:=0;
end;

// Rotinas gerais

function DiscDescriptive(x,p: TwsVec): TwsGeneral;
{ Objetivo
    Retorna uma matriz com as quantidades envolvidas na determinação das medidas descritivas de
    uma distribuição discreta. As colunas dessa matriz são:
    1 - valores de X
    2 - probabilidades associadas a cada valor
    3 - produtos das probabilidades pelos valores
    4 - produto das probabilidades pelos desvios
    5 - produto das probabilidades pelos quadrados dos desvios
    6 - produto das probabilidades pelos cubos dos desvios
    7 - produto das probabilidades pelos desvios na quarta potência
    A última linha dessa matriz contém os totais de cada coluna
}
var
  i,FLin  : Integer;
  Ok      : Boolean;
  aux,aux1: Double;

begin
  // Verifica se esta tudo Ok
  Ok:=True;
  Ok:=(x.Len=p.Len);       // x e p devem ser do mesmo tamanho
  aux:=0;
  if Ok then
    for i:=1 to p.Len do
      aux:=aux+p[i];
  Ok:=FEquals(aux,1);      // soma das probabilidades deve ser igual a 1
  if Ok then
    begin
    aux:=0;
    FLin:=x.Len+1;
    Result:=TwsGeneral.Create(FLin,8);
    aux:=0;
    for i:=1 to FLin-1 do
      begin
      Result[i,1]:=x[i];
      Result[i,2]:=p[i];
      Result[FLin,2]:=Result[FLin,2]+Result[i,2];
      Result[i,3]:=Result[FLin,2];
      Result[i,4]:=x[i]*p[i];
      aux:=aux+Result[i,4]      // Media
      end;
   Result[FLin,4]:=aux;
   for i:=1 to FLin-1 do
     begin
     aux1:=x[i]-aux;
     Result[i,5]:=aux1*p[i];             // desvios ponderados
     Result[FLin,5]:=Result[FLin,5]+Result[i,5];
     Result[i,6]:=Sqr(aux1)*p[i];        // desvios ao quadrado
     Result[FLin,6]:=Result[FLin,6]+Result[i,6];
     Result[i,7]:=Sqr(aux1)*aux1*p[i];  // desvios ao cubo
     Result[FLin,7]:=Result[FLin,7]+Result[i,7];
     Result[i,8]:=Sqr(Sqr(aux1))*p[i];  // desvios na 4a potencia
     Result[FLin,8]:=Result[FLin,8]+Result[i,8];
     end
   end
 else
   begin
   Result:=nil;
   raise ERangeError.Create('Distribuição mal definida')
   end
end;

function KSProb(const d: Double): Double;
{ Objetivo
    Função que retorna probabilidade associada à estatística de Kolmogorv-Smirnov. Valores
    pequenos desta probabilidade são indicativos para rejeição da hipótese de que F(x) modela
    a distribuição de X.
  Parâmetros
    d: Valor da estatística
}
const
  eps1 = 0.001;
  eps2 = 1.0e-8;
  nIter=100;
var
  j           : Integer;
  a2,fac,sum,
  term,termbf,
  aux         : Double;
  Conv        : Boolean;
begin
  sum:=0; fac:=2; termbf:=0;
  j:=1;
  Conv:=False;
  a2:=-2*d*d;
  repeat
    term:=fac*exp(a2*j*j);
    sum:=sum+term;
    aux:=Abs(term);
    Conv:=(aux<=eps1*termbf) or (aux<=eps2*sum);
    if not Conv then
      begin
      fac:=-fac;
      termbf:=aux;
      Inc(j)
      end;
  until Conv or (j>nIter);
  if Conv then
    Result:=sum
  else
    Result:=1;
end;

end.  // unit wsProbabilidade
