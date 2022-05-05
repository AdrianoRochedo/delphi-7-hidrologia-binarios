unit wsDistribuicoesRandomicas;

interface
uses SysUtils,
     SysUtilsEx,
     wsConstTypes,
     wsGlib,
     wsVec,
     wsMatrix;

type

// =================== Amostras de valores uniformes em (0, 1) ==================

  { Descrição
      Classe genérica para obtenção de valores de variáveis uniformes em (0, 1)
    Descendência
      TwsRandom --> TObject
  }
  TwsRandom = class
  private
    idum: Integer;
  public
    constructor Create;
    function Generate: Double; virtual; abstract;
    function Range(c,n: Integer): Integer; virtual;
  end;

  { Descrição
      Classe para obtenção de valores de variáveis uniformes em (0, 1) utilizando
      o gerador interno do Object Pascal (a função Random). Para contornar problemas
      de autocorrelação utiliza um algoritmo para embaralhamento dos valores produzidos
    Descendência
      TwsShuffle --> TwsRandom --> TObject
  }
  TwsShuffle = class(TwsRandom)
  private
    RanV: array[1..97] of Double;   { Array de trabalho }
    RanY: Double;
  public
    function Generate: Double; override;
  end;

  { Descrição
      Classe para obtenção de valores de variáveis uniformes em (0, 1) utilizando
      o o método linear congruencial.
    Descendência
      TwsLinear --> TwsRandom --> TObject
  }
  TwsLinear = class(TwsRandom)
  private
    RanV: array[1..97] of Double; { Array de trabalho }
    Ix: array[1..3] of Integer;
  public
    function Generate: Double; override;
  end;

  { Descrição
      Classe para obtenção de valores de variáveis uniformes em (0, 1) utilizando
      o o método subtrativo. Este método tem como diferencial o fato de ser inteiramente
      baseado num mecanismo distinto do linear congruencial. Assim, pode ser utilizado
      em situações onde aquele não produz bons resultados.
    Descendência
      TwsLinear --> TwsRandom --> TObject
  }
  TwsSubtractive = class(TwsRandom)
  private
    RanV: array[1..55] of Double; { Array de trabalho }
    Ix: array[1..3] of Integer;
  public
    function Generate: Double; override;
  end;

{ Descrição
    Imlementa gerador de uniformes pelo método Linear Congruencial Multiplicativo de
    Módulo Primo (PMMLCG - Prime Modulus Multiplicative Linear Congruential Generator)
    baseado no gerador portável proposto por Marse & Roberts. Dá suporte a múltiplos
    streams (100) com sementes espaçadas em 100.000. Um número inteiro deve ser definido
    para dar acesso a uma determinada semente.
}

  TwsPMMLCG = class(TwsRandom)
  private
    ix: array[1..100] of Integer;             // Guarda as sementes para cada stream
    procedure SetStrSeed(Idx, ISeed: Integer);
    procedure SetIdxSeed(Idx: Integer);
    function GetStrSeed(Idx: Integer): Integer;
  public
    constructor Create;
    function Generate: double; override;
    property StrSeed[Index: Integer]: Integer read GetStrSeed write SetStrSeed;
    property IdxSeed: Integer write SetIdxSeed;
  end;

  { ======================Amostras de outras distribuições ===================== }

  { Descrição
      Classe genérica para obtenção de valores de variáveis aleatórias de muitas
      distribuições de probabilidade. Nenhum de seus métodos pode ser utilizado
      diretamente.
    Descendência
      TwsRanDist --> TObject
   }
  TwsRanDist = class
  private
    // Gerador de uniformes
    Unif: TwsRandom;
    procedure SetRandom(R:TwsRandom);
    procedure SetFParam1(const x: Double); virtual; abstract;
    procedure SetFParam2(const x: Double); virtual; abstract;
    procedure SetFParam3(const x: Double); virtual; abstract;
   public
    constructor Create;
    destructor Destroy; override;
    // gerador de valores de variaveis aleatorias
    function Generate: Double; virtual; abstract;
    // gerador de amostras de VA
    function GenerateSample(n: Integer): TObject; virtual;
    // Acessa/modifica gerador de uniformes
    property U: TwsRandom read Unif write SetRandom;
  end; { TwsRanDist }

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      padrão. O algoritmo de geração utilizado é o polar, uma modificação do algoritmo de
      Box-Miller.
    Descendência
      TwsPolar --> TwsRanDist --> TObject
  }
  TwsNormal01 = class(TwsRanDist)
  private
    Extra: Boolean;       // True se valor extra esta disponivel
    ExtraValue: Double;   // valor extra gerado no passo anterior
  public
    constructor Create;
    // Algoritmo polar
    function Generate: Double; override;
  end;

  { Descrição ===================================> conferir
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      padrão. O algoritmo de geração utilizado é o proposto por Marsaglia & Bray
    Descendência
      TwsMarsaglia --> TwsRanDist --> TObject
  }
  TwsMarsaglia = class(TwsRanDist)
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      padrão. O algoritmo de geração utilizado é o da razão de uniformes.
    Descendência
      TwsNormalUnifRatio --> TwsNormal01 --> TwsRanDist --> TObject
  }
  TwsNormalUnifRatio = class(TwsRanDist)
    // Algoritmo razão de uniformes
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      de média e desvio padrão estabelecidos. O algoritmo de geração é o polar (método
      Generate herdado), uma modificação do algoritmo de Box-Miller.
    Descendência
      TwsNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }
  TwsNormal = class(TwsNormal01)
  private
    FParam1,
    FParam2: Double;
    procedure SetFParam1(const x: Double); override;
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const m,s: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    // gerador
    function Generate: Double; override;
    // media da distribuicao normal
    property Mean: double read FParam1 write SetFParam1;
    // desvio padrao
    property SDev: double read FParam2 write SetFParam2;
  end;

TwsHalfNormal = class(TwsNormal01)
private
  FParam1: Double;
  procedure SetFParam1(const x: Double); override;
public
  // cria classe especificando parametros
  constructor Create(const s: Double); overload;
  // cria classe sem especificar parametros
  constructor Create; overload;
  // gerador
  function Generate: Double; override;
  // dispersao
  property Lambda: double read FParam1 write SetFParam1;
end;

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição lognormal
      padrão. O algoritmo básico de geração é o polar (método Generate herdado), uma
      modificação do algoritmo de Box-Muller.
    Descendência
      TwsStdLogNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }
  TwsStdLogNormal = class(TwsNormal01)
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição lognormal com
      parâmetros de dispersão e formato estabelecidos. Esses parãmetros NÂO são a média
      e o desvio padrão da distribuição lognormal e sim da normal, distribuição da variável
      transformada.
    Descendência
      TwsLogNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }
  TwsLogNormal = class(TwsNormal)
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição lognormal com
      três parâmetros: deslocamento, dispersão e formato estabelecidos.
    Descendência
      TwsLogNormal3 --> TwsLogNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }
  TwsLogNormal3 = class(TwsLogNormal)
  private
    FParam3: Double;
    procedure SetFParam3(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const AParam, BParam, TParam: double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Theta: Double read FParam3 write SetFParam3;
  end;


  { Descrição
      Classe para geração de valores de variáveis aleatórias cujo valor transformado
      (transformação de Box-Cox) tem distribuição normal.
    Descendência
      TwsNormalPower --> TwsNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }
  TwsNormalPower = class(TwsNormal)
  private
    FLam: Double;
    procedure SetFParam3(const x: Double); override;
  public
    function Generate: Double; override;
    property Lambda: Double read FLam write SetFParam3;
  end;

  { Descrição
      Gera um vetor de valores de uma variável normal multivariada
    Descendência
      TwsNormalMult --> TwsNormal01 --> TwsRanDist --> TObject
  }
  TwsNormalMult = class(TwsNormal01)
  private
    T      : TwsTriangular;
    z,FMean,
    xVec   : TwsVec;
    procedure SetMean(M: TwsVec);
    procedure SetCov(C: TwsMatrix);
   function GetXValue(i: Integer): Double;
  public
    // cria classe especificando parametros
    constructor Create(m: TwsVec; aCov: TwsSymmetric); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    destructor Destroy; override;
    function Generate: Double; override;
    function GenerateSample(n: Integer): TObject; override;
    // acesso aos valores do vetor gerado
    property x[Index: Integer]: Double read GetXValue;
    // acesso ao vetor de médias
    property Mean: TwsVec read FMean write SetMean;
    // acesso (somente para troca da matriz de covariancias)
    property Cov: TwsMatrix write SetCov;
  end;

  { Descrição
      Classe para geração de valores com distribuição Weibull padronizada
    Descendência
      TwsStdWeibull --> TwsWeibull --> TwsRanDist -- TObject
  }
  TwsStdWeibull = class(TwsRanDist)
  private
    FParam1,
    InvA: Double;
    procedure SetFParam1(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const a: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Alpha: double read FParam1 write SetFParam1;    // parametro de formato
  end;

  { Descrição
      Classe para geração de valores com distribuição Weibull
    Descendência
      TwsStdWeibull --> TwsWeibull --> TwsRanDist -- TObject
  }
  TwsWeibull = class(TwsStdWeibull)
  private
    FParam2: Double;
    procedure SetFParam2(const x: Double); override;        // parametro de dispersao
  public
    // cria classe especificando parametros
    constructor Create(const a,b: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Lambda: double read FParam2 write SetFParam2;
  end;

  { Descrição
      Classe para geração de valores com distribuição Weibull
    Descendência
      TwsWeibull3 --> TwsWeibull --> TwsRanDist -- TObject
  }
  TwsWeibull3 = class(TwsWeibull)
  private
    FParam3: Double;
    procedure SetFParam3(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const t,a,b: Double); overload;
    function Generate: Double; override;
    property Theta: double read FParam3 write SetFParam3;    // parametro de deslocamento
  end;

  { Descrição
      Classe para geração de valores com distribuição Gumbel padronizada
    Descendência
      TwsStdGumbel --> TwsRanDist -- TObject
  }
  TwsStdGumbel = class(TwsRanDist)
  public
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores com distribuição Gumbel
    Descendência
      TwsGumbel --> TwsStdGumbel --> TwsRanDist -- TObject
  }
  TwsGumbel = class(TwsStdGumbel)
  private
    FParam1,
    FParam2: Double;
    procedure SetFParam1(const x: Double); override;
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const a,b: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Gamma: double read FParam1 write SetFParam1;
    property Lambda: double read FParam2 write SetFParam2;
  end;

  { Descrição
      Classe para geração de valores com distribuição de extremos generalizada
    Descendência
      TwsGVE --> TwsRanDist -- TObject
  }
  TwsGVE = class(TwsRanDist)
  private
    aux,             // valor auxiliar
    FParam1,         // tendencia central
    FParam2,         // dispersao
    FParam3: Double; // formato
    procedure SetFParam1(const a: Double); override;
    procedure SetFParam2(const b: Double); override;
    procedure SetFParam3(const a: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const a,b,c: Double); overload;
    function Generate: Double; override;
    property Gamma: double read FParam1 write SetFParam1;
    property Lambda: double read FParam2 write SetFParam2;
    property Alpha: double read FParam3 write SetFParam3;
  end;

  { Descrição
      Classe basica para geração de valores de uma distribuição gama
    Descendência
      TwsGamma --> TwsRanDist --> TObject
  }
  TwsGamma = class(TwsRanDist)
  private
    FParam1: Double;
  public
    // cria classe especificando parametros
    constructor Create(const AParam: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    property Alfa: Double read FParam1 write SetFParam1;
  end;

  { Descrição
      Classe para geração de valores da distribuição gama incompleta
    Descendência
      TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsGamma1 = class(TwsGamma)
  private
    xx: array[1..5] of Double;
    procedure SetFParam1(const x: Double); override;
  public
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores da distribuição qui-quadrado
    Descendência
      TwsChiSquare --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

  TwsChiSquare = class(TwsGamma1)
  private
    procedure SetFParam1(const x: Double); override;
  public
    constructor Create(df: Double); overload;
    constructor Create; overload;
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores da distribuição gama de dois parâmetros
    Descendência
      TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsGamma2=class(TwsGamma1)
  private
    FParam2: double;
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const AParam, BParam: double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: double; override;
    property Lambda: Double read FParam2 write SetFParam2;
  end;

  { Descrição
      Classe para geração de valores da distribuição gama de tres parâmetros
    Descendência
      TwsGamma3 --> TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsGamma3=class(TwsGamma2)
  private
    FParam3: double;
    procedure SetFParam3(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const AParam, BParam, TParam: double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: double; override;
    property Theta: Double read FParam3 write SetFParam3;
  end;

  { Descrição
      Classe para geração de valores da distribuição Log-Pearson Tipo III
    Descendência
      TwsLPearson3 --> TwsGamma3 --> TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist
        --> TObject
  }
  TwsLPearson3=class(TwsGamma3)
  public
    function Generate: double; override;
  end;

  { Descrição
      Classe para geração de valores da distribuição Pearson Tipo V
    Descendência
      TwsPearson5 --> TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsPearson5=class(TwsGamma2)
  private
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const AParam, BParam: double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: double; override;
  end;

  { Descrição
      Classe para geração de valores da distribuição beta com valores em (0, 1)
    Descendência
      TwsBeta01 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsBeta01=class(TwsGamma1)
  private
    FParam2: double;
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const AParam, BParam: double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: double; override;
    property Beta: Double read FParam2 write SetFParam2;
  end;

  { Descrição
      Classe para geração de valores da distribuição beta com valores em (0, 1)
    Descendência
      TwsBetaAB --> TwsBeta01 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsBetaAB=class(TwsBeta01)
  private
      Min, Range: double;
  public
    // cria classe especificando parametros
    constructor Create(const xMin,xMax,AParam,BParam: double); overload;
    // cria classe sem especificar parametros
    constructor Create(const xMin,xMax: double); overload;
    function Generate: double; override;
  end;

  { Descrição
      Classe para geração de valores da distribuição Erlang (gama incompleta de
      parâmetro inteiro)
    Descendência
      TwsErlang --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsErlang = class(TwsGamma)
  private
    procedure SetFParam1(const x: Double); override;
  public
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores da distribuição M-Erlang
    Descendência
      TwsMErlang --> TwsErlang --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsMErlang = class(TwsErlang)
  private
    FParam2: Double;
    aux: Double;
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const a, b: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property m: Double read FParam2 write SetFParam2;
    property Beta: Double read FParam1 write SetFParam1;
  end;

  { Descrição
      Classe para geração de valores da distribuição exponencial
    Descendência
      TwsExponential --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsExponential = class(TwsGamma)
  private
    procedure SetFParam1(const x: Double); override;
  public
    function Generate: Double; override;
  end;

  { Descrição
      Classe para geração de valores da distribuição exponencial com dois parâmetros
    Descendência
      TwsExponential2 --> TwsExponential --> TwsGamma --> TwsRanDist --> TObject
  }
  TwsExponential2 = class(TwsExponential)
  private
    FParam2: Double;
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const xt,xl: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Theta: Double read FParam2 write SetFParam2;
  end;

  { Descrição
      Classe para geração de valores da distribuição de Poisson
    Descendência
      TwsPoisson --> TwsRanDist --> TObject
  }
  TwsPoisson = class(TwsRanDist)
  private
    xx: array[1..4] of Double;
    FParam1: Double;
    procedure SetFParam1(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const ALam: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Lambda: Double read FParam1 write SetFParam1;
  end;

  { Descrição
      Classe para geração de valores da distribuição geométrica
    Descendência
      TwsGeometric --> TwsRanDist --> TObject
  }
  TwsGeometric = class(TwsRanDist)
  private
    FParam1: Double;  // probabilidade de sucesso
    procedure SetFParam1(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const p: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property ProbSuc: Double read FParam1 write SetFParam1;
  end;

  { Descrição
      Classe para geração de valores da distribuição de Bernoulli
    Descendência
      TwsBernoulli --> TwsRanDist --> TObject
  }
  TwsBernoulli = class(TwsRanDist)
  private
    FParam1: Double;  // probabilidade de sucesso
    procedure SetFParam1(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const p: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property ProbSuc: Double read FParam1 write SetFParam1;
  end;

  { Descrição
      Classe para geração de valores da distribuição binomial negativa
    Descendência
      TwsNegBin --> TwsGeometric --> TwsRanDist --> TObject
  }
  TwsNegBin = class(TwsGeometric)
  private
    FParam2: Double;  // numero de experimentos
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const s,p: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property ProbSuc: Double read FParam1 write SetFParam1;
    property NExp: Double read FParam2 write SetFParam2;
  end;

  { Descrição
      Classe para geração de valores da distribuição uniforme
    Descendência
      TwsUniform --> TwsRanDist --> TObject
  }
  TwsUniform = class(TwsRanDist)
  private
    FParam1: Double;
    FParam2,
    Dif: Double;
    procedure SetFParam1(const a: Double); override;
    procedure SetFParam2(const b: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const a, b: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Min: Double read FParam1 write FParam1;
    property Max: Double read FParam2 write FParam2;
  end;

  { Descrição
      Classe para geração de valores da distribuição uniforme discreta
    Descendência
      TwsDiscreteUnif --> TwsRanDist --> TObject
  }
  TwsDiscreteUnif = class(TwsRanDist)
  private
    FParam1,
    FParam2,
    Num: Double;
    procedure SetFParam1(const a: Double); override;
    procedure SetFParam2(const b: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const a, b: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property Min: Double read FParam1 write FParam1;
    property Max: Double read FParam2 write FParam2;
  end;

  { Descrição
      Classe para geração de valores da distribuição binomial
    Descendência
      TwsBinomial --> TwsRanDist --> TObject
  }
  TwsBinomial = class(TwsRanDist)
  private
    xx: array[1..6] of Double;
    FParam1,
    FParam2: Double; { probabilidade p de sucesso }
    n: Integer;
    procedure SetFParam1(const x: Double); override;
    procedure SetFParam2(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const nn, pp: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property NExp: Double read FParam1 write SetFParam1;
    property ProbSuc: Double read FParam2 write SetFParam2;
  end;

  { Descrição
      Classe para geração de valores da distribuição hipergeométrica
    Descendência
      TwsHyperGeom --> TwsRanDist --> TObject
  }
  TwsHyperGeom = class(TwsRanDist)
  private
    FParam1,
    FParam2,
    FParam3: Double;
    procedure SetFParam1(const x: Double); override;
    procedure SetFParam2(const x: Double); override;
    procedure SetFParam3(const x: Double); override;
  public
    // cria classe especificando parametros
    constructor Create(const nn, nn1, pp: Double); overload;
    // cria classe sem especificar parametros
    constructor Create; overload;
    function Generate: Double; override;
    property nPop: Double read FParam1 write SetFParam1;
    property nSamp: Double read FParam2 write SetFParam2;
    property nSuc: Double read FParam3 write SetFParam3;
  end;

  { ============================ Simulacoes didaticas =========================}
  // Estatistica descritiva
  procedure Descriptive(Samp: TwsVec; out Stat,Quant: TwsVec; out DS: TwsDataSet);
  // Estabilidade da frequencia relativa
  function RFreqStability(n: Integer; const p: Double): TwsGeneral;
  // Intervalos de confianca para amostragem repetida
  function ConfidenceIntervals(n,m: Integer; const PopMean,PopSDev: Double; out f5,f1,
    t5,t1: Double; out DS: TwsDataSet; RetDS: boolean=False): TwsDataSet;
  // Testes para media de uma amostra em amostragem repetidas
  function OneSampleTest(var Samp: TwsVec; Simul: boolean; h0: Double; m,n: Integer; out f1,f5,
    t1,t5: Double; out DS: TwsDataSet; DSave: boolean=False): TwsDataSet;
  // Testes para medias de duas amostras em amostragem repetida
  function TwoSampleTest(var xSamp,ySamp: TwsVec; sPar: TwsVec; h0: Double; m,xn,yn: Integer;
    out f1,f5,t1,t5: Double; out DS: TwsDataSet; DSave: Boolean=False): TwsDataSet;
  // Amostras de distribuicao normal bivariada
  function BivariateNormal(const mPop1,dp1,mPop2,dp2,r: double; n: Integer;
    out EP: TwsGeneral): TwsDataSet;
  // Amostras de distribuicao normal bivariada com analise de regressao
  function BivariateXY(xVec,yVec: TwsVec; out EP,Anova: TwsGeneral; n: Integer): TwsDataSet;
  // Analise de classificacoes simples
  function OneWay(const m, v: TwsVec; r: TwsLIVec; out DS: TwsDataSet; DSave: boolean=False): TwsGeneral;
  // Analise de classificacoes duplas
  function TwoWay(const m, v: TwsVec; r: TwsLIVec; out DS: TwsDataSet; out A, B: TwsGeneral;
    DSave: boolean=False): TwsGeneral;
  // Obtem estatisticas de amostras simuladas
  function StatSimul(m,n: Integer; Par: array of Double; Stat: array of TwsEnumStatistics;
    DType: TDist): TwsDataSet;


// testes para geradores de numeros aleatorios

procedure RandomTest(Tipo: byte);

implementation
uses Math,
     Dialogs,
     wsMath,
     wsFuncoesDeProbabilidade,
     wsFuncoesDeEscalares,
     wsExceptions;

{ TwsRandom }

  { Descrição
      Classe genérica para obtenção de valores de variáveis uniformes em (0, 1). Tem
      como finalidades básicas servir como base para as demais classes geradoras e
      a inicialização dos geradores
    Descendência
      TwsRandom --> TObject
  }

constructor TwsRandom.Create;
begin
  Randomize;
  if RandSeed>0 then
    idum:=-RandSeed
  else
    idum:=RandSeed

end;

function TwsRandom.Range(c,n: Integer): Integer;
begin
  Result:=Trunc((n-c+1)*Generate)+c // ?????????????????????????
end;

{ TShuffle }

  { Descrição
      Classe para obtenção de valores de variáveis uniformes em (0, 1) utilizando
      o gerador interno do Object Pascal (a função Random). Para contornar problemas
      de autocorrelação utiliza um algoritmo para embaralhamento dos valores produzidos
    Descendência
      TwsShuffle --> TwsRandom --> TObject
  }

function TwsShuffle.Generate: Double;
const
  Size=97;
var
  dum: Double;
  j  : Integer;
begin
  if idum < 0 then
    begin
    idum := 1;
    for j := 1 to Size do
      dum := Random;        // exercita a rotina do sistema
    for j := 1 to Size do
      RanV[j] := Random;   // Salva 97 valores
    RanY := Random          // e o 98
    end;
  { Inicio se nao houver inicializacao. O valor obtido na chamada anterior e utilizado
  para obter um indice para entre 1 e Size. O valor obtido e utilizado para saida e para
  obtencao de outro indice }
  j := 1 + trunc(Size*RanY);
  RanY := RanV[j];
  RanV[j] := Random;            // Preenche a tabela com um novo valor
  Result := RanY
end; // Generate

{ TwsLinear }

  { Descrição
      Classe para obtenção de valores de variáveis uniformes em (0, 1) utilizando
      o o método linear congruencial.
    Descendência
      TwsLinear --> TwsRandom --> TObject
  }

function TwsLinear.Generate: Double;
const
  Size = 97;
  m1 = 259200;                     // Constantes para o primeiro gerador
  a1 = 7141;
  c1 = 54773;
  im1 = 3.8580247e-6;              // 1/m1
  m2 = 134456;                     // Constantes para o segundo gerador
  a2 = 8121;
  c2 = 28411;
  im2 = 7.4373773e-6;              // 1/m2
  m3 = 243000;                     // Constantes para o terceiro gerador
  a3 = 4561;
  c3 = 51349;
var
  j: Integer;
begin
  if idum < 0 then
    begin
    Ix[1] := (c1 - idum) Mod m1;  // Obtem semente para a primeira rotina
    Ix[1] := (a1*Ix[1] + c1) Mod m1;
    Ix[2] := Ix[1] Mod m2;        // e a utiliza como semente para a segunda rotina
    Ix[1] := (a1*Ix[1] + c1) Mod m1;
    Ix[3] := Ix[1] Mod m3;        // e para a terceira
    { Preenche a tabela com desvios uniformes sequenciais gerados com as
      primeiras duas rotinas }
    for j := 1 to Size do
      begin
      Ix[1] := (a1*Ix[1] + c1) Mod m1;
      Ix[2] := (a2*Ix[2] + c2) Mod m2;
                               // Combina as pecas de ordem mais baixa e mais alta
      RanV[j] := (Ix[1] + Ix[2]*im2)*im1;
      end;
    idum := 1
    end;
              { Exceto quando inicializa, este sera o inicio. Gera o proximo numero
                para cada sequencia }
  Ix[1] := (a1*Ix[1] + c1) Mod m1;
  Ix[2] := (a2*Ix[2] + c2) Mod m2;
  Ix[3] := (a3*Ix[3] + c3) Mod m3;
                 // Utiliza a terceira sequencia para obter um inteiro entre 1 e 97
  j := 1 + (Size*Ix[3]) div m3;
  Result := RanV[j];                   // Retorna a posicao sorteada
  RanV[j] := (Ix[1] + Ix[2]*im2)*im1   // e preenche-a novamente
end; // TwsLinear.Generate


{ TwsSubtractive }

  { Descrição
      Classe para obtenção de valores de variáveis uniformes em (0, 1) utilizando
      o o método subtrativo. Este método tem como diferencial o fato de ser inteiramente
      baseado num mecanismo distinto do linear congruencial. Assim, pode ser utilizado
      em situações onde aquele não produz bons resultados. Press et alli, pag. 221
    Descendência
      TwsLinear --> TwsRandom --> TObject
  }

function TwsSubtractive.Generate: Double;
const
  { De acordo com Knuth, qualquer valor mbig grande e qualquer valor menor (mas ainda
    grande) mseed podem ser substituidos por esses valores }
  mbig  = 1000000000;
  mseed = 161803398;
  mz    = 0;
  fac   = 1.0e-9;        // 1/mbig
  Size  = 55;
var
  i,ii,k : Integer;
  mj,mk  : Int64;
begin
  if idum < 0 then           // Inicializacao
    begin
    mj := mseed + idum;
    mj := mj Mod mbig;
    RanV[Size] :=  mj;      // Inicializa posicao 55 com semente idum e o valor mseed
    mk := 1;
    for i := 1 to 54 do      // e, a seguir, inicializa o restante da tabela
      begin
      ii := 21*i Mod Size;   // numa ordem levemente aleatoria
      RanV[ii] := mk;       // com numeros que nao sao especialmente aleatorios
      mk := mj - mk;
      if mk < mz then
        mk := mk + mbig;
      mj := Trunc(RanV[ii])
      end;
    for k := 1 to 4 do       // Vamos casualiza-los atraves do "aquecimento do gerador"
      begin
      for i := 1 to Size do
        begin
        RanV[i] := RanV[i] - RanV[1 + ((i+30) Mod Size)];
        if RanV[i] < mz then
          RanV[i] := RanV[i] + mbig
        end;
      end;
    Ix[1] := 0;              // Prepara o indice para o nosso primeiro numero gerado
    Ix[2] := 31;             // a constante 31 eh espcial
    idum := 1
    end;
  { Aqui eh onde iniciamos, exceto na inicializacao. Acrescenta 1 e volta a 1 se
    ultrapassa 56
  }
  Inc(Ix[1]);
  if Ix[1] = 56 then
    Ix[1] := 1;

  Inc(Ix[2]);                 // o mesmo vale para Ix[2]
  if Ix[2] = 56 then
    Ix[2] := 1;
                              // Cria novo numero aleatorio por subtracao

  mj := Trunc(RanV[Ix[1]] - RanV[Ix[2]]);
  if mj < mz then             // Assegura que ele pertence a amplitude
    mj := mj + mbig;
  RanV[Ix[1]] := mj;         // Armazena-o

  Result := mj*fac            // e retorna o valor uniforme derivado
end; // Generate

{ TwsPMMLCG }

{ Objetivo
    Imlementa gerador de uniformes pelo método Linear Congruencial Multiplicativo de
    Módulo Primo (PMMLCG - Prime Modulus Multiplicative Linear Congruential Generator)
    baseado no gerador portável proposto por Marse & Roberts. Dá suporte a múltiplos
    streams (100) com sementes espaçadas em 100.000. Um número inteiro deve ser definido
    para dar acesso a uma determinada semente.
}
constructor TwsPMMLCG.Create;
begin
  ix[1]:=1973272912;  ix[2]:=281629770;   ix[3]:=20006270;    ix[4]:=1280689831;
  ix[5]:=2096730329;  ix[6]:=1933576050;  ix[7]:=913566091;   ix[8]:=246780520;
  ix[9]:=1363774876;  ix[10]:=604901985;  ix[11]:=1511192140; ix[12]:=1259851944;
  ix[13]:=824064364;  ix[14]:=150493284;  ix[15]:=242708531;  ix[16]:=75253171;
  ix[17]:=1964472944; ix[18]:=1202299975; ix[19]:=233217322;  ix[20]:=1911216000;
  ix[21]:=726370533;  ix[22]:=403498145;  ix[23]:=993232223;  ix[24]:=1103205531;
  ix[25]:=762430696;  ix[26]:=1922803170; ix[27]:=1385516923; ix[28]:=76271663;
  ix[29]:=413682397;  ix[30]:=726466604;  ix[31]:=336157058;  ix[32]:=1432650381;
  ix[33]:=1120463904; ix[34]:=595778810;  ix[35]:=877722890;  ix[36]:=1046574445;
  ix[37]:=68911991;   ix[38]:=2088367019; ix[39]:=748545416;  ix[40]:=622401386;
  ix[41]:=2122378830; ix[42]:=640690903;  ix[43]:=1774806513; ix[44]:=2132545692;
  ix[45]:=2079249579; ix[46]:=78130110;   ix[47]:=852776735;  ix[48]:=1187867272;
  ix[49]:=1351423507; ix[50]:=1645973084; ix[51]:=1997049139; ix[52]:=922510944;
  ix[53]:=2045512870; ix[54]:=898585771;  ix[55]:=243649545;  ix[56]:=1004818771;
  ix[57]:=773686062;  ix[58]:=403188473;  ix[59]:=372279877;  ix[60]:=1901633463;
  ix[61]:=498067494;  ix[62]:=2087759558; ix[63]:=493157915;  ix[64]:=597104727;
  ix[65]:=1530940798; ix[66]:=1814496276; ix[67]:=536444882;  ix[68]:=1663153658;
  ix[69]:=855503735;  ix[70]:=67784357;   ix[71]:=1432404475; ix[72]:=619691088;
  ix[73]:=119025595;  ix[74]:=880802310;  ix[75]:=176192644;  ix[76]:=1116780070;
  ix[77]:=277854671;  ix[78]:=1366580350; ix[79]:=1142483975; ix[80]:=2026948561;
  ix[81]:=1053920743; ix[82]:=78662391;   ix[83]:=1792203830; ix[84]:=1494667770;
  ix[85]:=1923011392; ix[86]:=1433700034; ix[87]:=1944184613; ix[88]:=1147297105;
  ix[89]:=539712780;  ix[90]:=1545929719; ix[91]:=190641742;  ix[92]:=1645390429;
  ix[93]:=264907697;  ix[94]:=620389253;  ix[95]:=1502074852; ix[96]:=927711160;
  ix[97]:=364849192;  ix[98]:=2049576050; ix[99]:=638580085;  ix[100]:=547070247;
  idum:=1;
end;

procedure TwsPMMLCG.SetStrSeed(Idx, ISeed: Integer);
begin
  if (Idx>0) and (Idx<100) then
    ix[Idx]:=ISeed
end;

procedure TwsPMMLCG.SetIdxSeed(Idx: Integer);
begin
  if (Idx>0) and (Idx<=100) then
    idum:=Idx
end;

function TwsPMMLCG.GetStrSeed(Idx: Integer): Integer;
begin
  if (Idx>0) and (Idx<100) then
    Result:=ix[Idx]
end;

function TwsPMMLCG.Generate: Double;
const
  B2E15 = 32768;
  B2E16 = 65536;
  Modlus= 2147483647;
  Mult1 = 24112;
  Mult2 = 26143;
var
  Hi15,Hi31,Low15,
  Lowprd,Ovflow,Zi: Int64;
begin
  Zi:=ix[idum];
  Hi15:=Zi div B2E16;
  Lowprd:=(Zi-Hi15*B2E16)*Mult1;
  Low15:=Lowprd div B2E16;
  Hi31:=Hi15*Mult1+Low15;
  Ovflow:=Hi31 div B2E15;
  Zi:=(((Lowprd-Low15*B2E16)-Modlus)+(Hi31-Ovflow*B2E15)*B2E16)+Ovflow;
  if Zi<0 then
    Inc(Zi,Modlus);
  Hi15:=Zi div B2E16;
  Lowprd:=(Zi-Hi15*B2E16)*Mult2;
  Low15:=Lowprd div B2E16;
  Hi31:=Hi15*Mult2+Low15;
  Ovflow:=Hi31 div B2E15;
  Zi:=(((Lowprd-Low15*B2E16)-Modlus)+(Hi31-Ovflow*B2E15)*B2E16)+Ovflow;
  if Zi<0 then
    Inc(Zi,Modlus);
  ix[idum]:=Zi;
  Result:=(2*(Zi div 256)+1)/16777216
end;

{ TRandDist }

  { Descrição
      Classe genérica para obtenção de valores de variáveis aleatórias de muitas
      distribuições de probabilidade. Nenhum de seus métodos pode ser utilizado
      diretamente.
    Descendência
      TwsRanDist --> TObject
   }

constructor TwsRanDist.Create;
{ Objetivo
    Criaçao do obejto para geração de valores variáveis de diferentes distribuições de
    probabilidade. Define TwsSubtractive como gerador default de variáveis uniformes (0, 1)
}
begin
  inherited Create;
  Unif := TwsLinear.Create
end;

destructor TwsRanDist.Destroy;
{ Objetivo
    Libera espaço alocado pelo objeto
}
begin
  Unif.Free;
  inherited Destroy
end;

procedure TwsRanDist.SetRandom(R: TwsRandom);
{ Objetivo
    Substitui o gerador de uniformes (0, 1)
  Parâmetros
    R: novo gerador de uniformes (0, 1)
}
begin
  Unif.Free;
  Unif := R
end;

function TwsRanDist.GenerateSample(n: Integer): TObject;
{ Objetivo
    Gera uma amostra de uma distribuição qualquer utilizando o método Generate, redefinido
    para cada uma delas (ou seja, utilizando o método Generate dos descendentes).
  Parâmetros
    n: Tamanho da amostra a ser gerada
}
var
  i: Integer;
begin
  Result:=TwsDFVec.Create(n);
  for i:=1 to n do
    TwsDFVec(Result)[i]:=Generate
end;

{ TwsMarsaglia }

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      padrão. O algoritmo de geração utilizado é o proposto por Marsaglia & Bray
    Descendência
      TwsNormal01 --> TwsRanDist --> TObject
  }

function TwsMarsaglia.Generate: Double;
{ Objetivo
    Gera valores de uma distribuição normal padrão segundo o algoritmo proposto por
    Marsaglia & Bray. Algoritmo disponível em Ripley, pág. 229
}
var
  u,u1,u2,
  av,g,v,w: double;
begin
  u:=Unif.Generate;
  if u<=0.8638 then
    Result:=2*(Unif.Generate+Unif.Generate+Unif.Generate-1.5)
  else
    if u<=0.9745 then
      Result:=1.5*(Unif.Generate+Unif.Generate-1)
    else
      if u<=0.9973002039 then
        begin
        repeat
          v:=6*Unif.Generate-3;
          av:=Abs(v);
          g:=17.49731196*exp(-0.5*v*v);
          if av<1 then
            g:=g-4.73570326*(3-v*v)
          else
            g:=g-2.36785163*(3-av)*(3-av);
          if av<1.5 then
            g:=g-2.157875*(1.5-av);
        until (0.358*Unif.Generate<=g);
        Result:=v
        end
      else
        repeat
          repeat
            u1:=2*Unif.Generate-1;
            u2:=2*Unif.Generate-1;
            w:=u1*u1+u2*u2;
          until w<1;
          w:=Sqrt((9-2*Ln(w))/w);
          Result:=u1*w;
          if Abs(Result)>3 then Break;
          Result:=u2*w;
        until Abs(Result)>3
end;  // Generate

{ TwsNormal01 }

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      padrão. O algoritmo de geração utilizado é o polar, uma modificação do algoritmo de
      Box-Miller feita por Marsaglia & Bray. Cada vez que esse algoritmo é executado
      dois valores N(0, 1) independentes são gerados. Numa segunda chamada do método
      Generate, um campo boleano Extra dá acesso direto a esse valor, se for o caso.
    Descendência
      TwsPolar --> TwsRanDist --> TObject
  }

constructor TwsNormal01.Create;
begin
  inherited Create;
  Extra:=False
end;

function TwsNormal01.Generate: Double;
{ Objetivo
    Gera um par de valores independentes com distribuição normal padrão segundo um
    algoritmo denominado Polar (uma modificação do algoritmo de Box & Miller, proposta por
    Marsaglia & Bray). Como dois valores são gerados de cada vez, um fica disponível para a
    próxima chamada. Numa terceira chamada, se houver, outrhos dois são gerados, um entregue
    e outro fica disponível e assim por diante.
}
var
  fac,r,
  v1,v2 : double;
begin
  // Se nao tiver valor extra, vai gerar dois valores independentes
  if not Extra then
  // Pega dois valores uniformes no quadrado que se extende de -1 a 1, em cada direcao
    begin
    repeat
      v1 := 2*Unif.Generate-1;
      v2 := 2*Unif.Generate-1;
      r := v1*v1 + v2*v2;        // Verifica se estao num circulo de raio unitario
    until (r < 1) and (r > 0);   // se nao estiverem, tenta de novo
                           // Faz a transformacao de Box-Muller para obter dois valores
    fac := Sqrt(-2*Ln(r)/r);
    ExtraValue := v1*fac;        // Guarda um
    Result := v2*fac;            // retorna outro
    Extra := True                // informa que possui valor extra para proxima chamada
    end
  else                           // Se tem extra
    begin
    Extra := False;              // Informa que nao ha mais extra
    Result := ExtraValue         // e retorna valor extra
    end
end; // Generate


{ TwsNormalUnifRatio }

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      padrão. O algoritmo de geração utilizado é o da razão de uniformes.
    Descendência
      TwsNormalUnifRatio --> TwsNormal01 --> TwsRanDist --> TObject
  }

function TwsNormalUnifRatio.Generate: Double;
{ Objetivo
    Gera valores de uma distribuição normal padrão, utilizando o método razão de
    uniformes. Algoritmo descrito em Ripley, pág 229
}
var
  u,v,x,z: Double;
begin
  repeat
    u:=Unif.Generate;
    v:=0.8578*(2*Unif.Generate-1);
    x:=v/u;
    z:=0.25*x*x;
  until (z<(1-u)) or ((z<=(0.259/u+0.35)) and (z<=-Ln(u)));
  Result:=x
end; // Generate

{ TwsHalfNormal }

constructor TwsHalfNormal.Create(const s: Double);
begin
  inherited Create;
  SetFParam1(s)
end;

constructor TwsHalfNormal.Create;
begin
  inherited Create;
end;

procedure TwsHalfNormal.SetFParam1(const x: Double);
{ Objetivo
    Atribui valor ao parãmetro de dispersão
  Parâmetros
    x: valor que será atribuído
  Exceções
    Gera uma exceção se o valor a ser atribuído é menor ou igual a zero
}
begin
  if x>0 then
    FParam1:=x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido para dispersão',[8,10,x]))
end;

function TwsHalfNormal.Generate: Double;
{ Objetivo
    Gera um valor de uma distribuição semi-normal com parâmetro de dispersão especificado.
    O método Generate herdado é utilizado para a geração de valores normais(0, 1)
}
begin
  Result:=Abs(inherited Generate)/FParam1
end;

{ TwsNormal }

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição normal
      de média e desvio padrão estabelecidos. O algoritmo de geração padrão é o de
      Marsaglia & Bray mas pode facilmente ser substuído por outro.
    Descendência
      TwsNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }

constructor TwsNormal.Create(const m,s: Double);
{ Objetivo
    Cria e inicializa objeto para geração de valores de variáveis com distribuição
    normal de média e desvio padrão especificados.
  Parâmetros
    m: média da distribuição
    s: desvio padrão da distribuição
}
begin
  inherited Create;
  SetFParam1(m);
  SetFParam2(s);
end;

constructor TwsNormal.Create;
{ Objetivo
    Cria objeto para geração de valores de variáveis com distribuição normal
    sem especificar valores dos parâmetros.
  Parâmetros
    m: média da distribuição
    s: desvio padrão da distribuição
}
begin
  inherited Create;
end;

procedure TwsNormal.SetFParam1(const x: Double);
{ Objetivo
    Atribui valor ao parãmetro média
  Parâmetros
    x: valor que será atribuído
}
begin
  FParam1:=x
end;

procedure TwsNormal.SetFParam2(const x: Double);
{ Objetivo
    Atribui valor ao parãmetro desvio padrão
  Parâmetros
    x: valor que será atribuído
  Exceções
    Gera uma exceção se o valor a ser atribuído é menor ou igual a zero
}
begin
  if x>0 then
    FParam2:=x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido para desvio padrão',[8,10,x]))
end;

(*
procedure TwsNormal.SetN01(Nor01: TwsNormal01);
{ Objetivo
    Modifica gerador de normal padrão
  Parâmetros
    Nor01: novo gerador
}
begin
  N01.Free;
  N01:=Nor01
end;
*)

function TwsNormal.Generate: Double;
{ Objetivo
    Gera um valor de uma distribuição normal de média e desvio padrão especificados.
    O método Generate herdado é utilizado para a geração de valores normais(0, 1)
}
begin
  Result:=FParam1+FParam2*(inherited Generate)
end;

{ TwsStdLogNormal }

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição lognormal
      padrão. O algoritmo básico de geração é o polar (método Generate herdado), uma
      modificação do algoritmo de Box-Miller.
    Descendência
      TwsStdLogNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }

function TwsStdLogNormal.Generate: Double;
{ Objetivo
    Gera um valor de uma distribuição lognormal padrão através da transformação
    y=exp(normal)
}
begin
  Result := exp(inherited Generate);
end;

{ TwsLogNormal }

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição lognormal
      com média e desvio padrão estabelecido. Deve-se observar que a média e o desvio
      padrão informados NÂO são as respectivas medidas da distribuição normal e sim da
      normal, a partir da qual é feita a transformação para esta distribuição.
    Descendência
      TwsLogNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }

function TwsLogNormal.Generate: Double;
{ Objetivo
    Gera um valor de uma distribuição lognormal através da propriedade: se y~Nor(m, s)
    então x=exp(y) é lognormal
}
begin
  Result := exp(inherited Generate)
end;

  { Descrição
      Classe para geração de valores de variáveis aleatórias com distribuição lognormal com
      três parâmetros: deslocamento, dispersão e formato estabelecidos.
    Descendência
      TwsLogNormal3 --> TwsLogNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }

constructor TwsLogNormal3.Create(const AParam, BParam, TParam: double);
{ Objetivo
    Cria objeto para geração de variáveis lognormais com três parâmetros
  Parâmetros
    AParam: Parâmetro de deslocamento
    BParam: Parâmetro de dispersão
    TParam: Parâmetro de formato
}
begin
  inherited Create(AParam,BParam);
  SetFParam3(TParam)
end; (* Create *)

// cria classe sem especificar parametros
constructor TwsLogNormal3.Create;
begin
  inherited Create;
end;

procedure TwsLogNormal3.SetFParam3(const x: Double);
begin
  if x>=0 then
    FParam3:=x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido para parâmetro Teta',[8,10,x]))
end;

function TwsLogNormal3.Generate: Double;
begin
  Result:=FParam3+inherited Generate
end;

  { Descrição
      Classe para geração de valores de variáveis aleatórias cujo valor transformado
      (transformação de Box-Cox) tem distribuição normal.
    Descendência
      TwsNormalPower --> TwsNormal --> TwsNormal01 --> TwsRanDist --> TObject
  }

procedure TwsNormalPower.SetFParam3(const x: Double);
begin
  if not FEquals(x,0) then
    FLam:=x
end;

function TwsNormalPower.Generate: Double;
{ Gera uma variavel normal e destransforma }
begin
  Result:=Power(FLam*inherited Generate+1,1/FLam)
end;

{ TwsNormalMult }

  { Descrição
      Gera um vetor de valores de uma variável normal multivariada
    Descendência
      TwsNormalMult --> TwsNormal01 --> TwsRanDist --> TObject
  }

constructor TwsNormalMult.Create(m: TwsVec; aCov: TwsSymmetric);
{ Objetivo
    Classe para geração de vetores de valores de uma normal multivariada. A dimensão
    da distribuição multivariada. Para aumentar a eficiência, a matriz de covariâncias
    é fatorada somente na criação do objeto e somente este fator é armazenado
  Parâmetros
    m: vetor com a médias populacionais
    aCov: matriz (simétrica) das covariãncias
}
begin
  inherited Create;
  FMean:=nil;
  SetMean(m);
  SetCov(aCov);
  xVec:=nil;
end;

constructor TwsNormalMult.Create;
{ Objetivo
    Classe para geração de vetores de valores de uma normal multivariada. A dimensão
    da distribuição multivariada. Para aumentar a eficiência, a matriz de covariâncias
    é fatorada somente na criação do objeto e somente este fator é armazenado
}
begin
  inherited Create;
  FMean:=nil;
  xVec:=nil;
end;

destructor TwsNormalMult.Destroy;
{ Objetivo
    Libera os espaços reservados pelo objeto
}
begin
  T.Free;
  z.Free;
  xVec.Free;
  inherited Destroy
end;

procedure TwsNormalMult.SetMean(M: TwsVec);
begin
  FMean.Free;
  FMean:=M
end;

procedure TwsNormalMult.SetCov(C: TwsMatrix);
var
  j: Integer;
begin
  T.Free;
  // Obtem o fator triangular da matriz de covariancias
  T:=TwsTriangular(TwsSymmetric(C).CholeskyFat(j,True));
  if z<> nil then
    z.Free;
  z:=TwsDFVec.Create(T.NCols);
end;

function TwsNormalMult.Generate: Double;
{ Objetivo
    Gera um vetor de variáveis com distribuição normal multivariada utilizando a técnica
    da fatoração da matriz de covariâncias pelo algoritmo de Cholesky.
  Retorno
    O valor devolvido (0) não tem sentido. Os elementos do vetor de valores gerados podem
    ser obtidos através da propriedade x[index]
}
var
  i,j : Integer;
  aux : Double;
begin
  xVec:=TwsDFVec.Create(T.nCols);
  for i := 1 to T.nRows do
    begin
    aux := 0;
    z[i] := inherited Generate;     // Obtem N(0,1)
    for j := 1 to i do              // Faz o produto com fator triangular
      aux := aux+T[i,j]*z[j];
    xVec[i] := Mean[i]+aux          // e armazena no vetor
    end;
  Result:=0                         // resultado qualquer, nao tem significado
end; // Generate

function TwsNormalMult.GenerateSample(n: Integer): TObject;
{ Objetivo
    Gera um conjunto de dados cujas colunas são valores de uma distribuição normal
    multivariada
  Parâmetros
    n: dimensão da amostra
  Retorno
    O conjunto de dados resultante cujo número de colunas é a dimensão da distribuição
    e o número de linhas é o tamanho da amostra desejada
}
var
  i,j,k: Integer;
  aux  : Double;
  xx   : TwsVec;
begin
  Result:=TwsDataSet.Create('MultNormal');
  for i:=1 to T.nRows do
    TwsDataSet(Result).Struct.AddColEx(TwsNumeric.Create('X'+IntToStr(i),'Variável X'+IntToStr(i),12,7));
  // Cada linha corresponde a um valor gerado para a distribuicao multivariada
  for k:=1 to n do
    begin
    xx:=TwsDFVec.Create(T.nRows);
    for i := 1 to T.nRows do
      begin
      aux := 0;
      z[i] := inherited Generate;
      for j := 1 to i do
        aux := aux+T[i,j]*z[j];
      xx[i] := FMean[i]+aux
      end;
      TwsDataSet(Result).MAdd(xx)
    end
end; // GenerateSample

function TwsNormalMult.GetXValue(i: Integer): Double;
begin
  if (i>0) and (i<=xVec.Len) then
    Result:=xVec[i]
end;


{ TwsStdWeibull }

  { Descrição
      Classe para geração de valores com distribuição Weibull
    Descendência
      TwsStdWeibull --> TwsRanDist -- TObject
  }

constructor TwsStdWeibull.Create(const a: Double);
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição Weibull padrão
  Parâmetros
    a: parâmetros de formato da distribuição Weibull (formato)
}
begin
  inherited Create;
  SetFParam1(a); // parametro de formato
end;

constructor TwsStdWeibull.Create;
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição Weibull
}
begin
  inherited Create;
end;

procedure TwsStdWeibull.SetFParam1(const x: Double);
{ Objetivo
    Atribui valor ao parâmetro alfa (de formato)
  Parâmetros
    x: valor que será atribuído
  Exceções
    Gera uma exceção se o valor a ser atribuído é menor ou igual a zero
}
begin
  if x>0 then
    begin
    FParam1:=x;
    InvA:=1/x
    end
  else
    raise EInvalidParam.Create(Format('Valor = %*.*g inválido como parâmetro de formato',[8,10,x]))
end;

function TwsStdWeibull.Generate: Double;
{ Objetivo
    Gera valor de uma variável com distribuição Weibull. O método utilizado é o da inversão
    da função de distribuição.
  Observação
    Algoritmo descrito em Law & Kelton, pág 490
}
begin
  Result := Power(-Ln(Unif.Generate),InvA);
end;

{ TwsWeibull }

  { Descrição
      Classe para geração de valores com distribuição Weibull
    Descendência
      TwsWeibull --> TwsStdWeibull --> TwsRanDist -- TObject
  }

constructor TwsWeibull.Create(const a,b: Double);
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição Weibull
  Parâmetros
    a, b: parâmetros da distribuição Weibull (dispersão e formato)
}
begin
  inherited Create(b);
  SetFParam2(a); // parametro de dispersao
end;

constructor TwsWeibull.Create;
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição Weibull
}
begin
  inherited Create;
end;

procedure TwsWeibull.SetFParam2(const x: Double);
{ Objetivo
    Atribui valor ao parãmetro Lambda (dispersao)
  Parâmetros
    x: valor que será atribuído
  Exceções
    Gera uma exceção se o valor a ser atribuído é menor ou igual a zero
}
begin
  if x>0 then
    FParam2:=x
  else
    raise EInvalidParam.Create(Format('Valor = %*.*g inválido como parâmetro de dispersão',[8,10,x]))
end;

function TwsWeibull.Generate: Double;
{ Objetivo
    Gera valor de uma variável com distribuição Weibull. O método utilizado é o da inversão
    da função de distribuição.
  Observação
    Algoritmo descrito em Law & Kelton, pág 490
}
begin
  Result := FParam2*inherited Generate
end;

  { Descrição
      Classe para geração de valores com distribuição Weibull
    Descendência
      TwsWeibull3 --> TwsWeibull --> TwsStdWeibull --> TwsRanDist -- TObject
  }

constructor TwsWeibull3.Create(const t,a,b: Double);
{ Objetivo
    Cria classe para geração de valores aleatórios de Y com distribuição de Weibull com
    três parâmetros
  Parâmetros
    t: Parâmetro de deslocamento
    a: Parâmetro de dispersão
    b: Parâmetro de formato
}
begin
  inherited Create(a,b);
  SetFParam3(t)
end;

procedure TwsWeibull3.SetFParam3(const x: Double);
{ Objetivo
    Atribui valor ao parâmetro de deslocamento da distribuição
  Parâmetros
    x: valor do parâmetro
  Exceções
    Gera exceção se x<0
}
begin
  if x >= 0 then
    begin
    FParam3 := x;
    FParam2:=FParam2-x
    end
  else
    raise EInvalidParam.Create(Format('Valor = %*.*g inválido como parâmetro de deslocamento',[8,10,x]))
end;

function TwsWeibull3.Generate: Double;
begin
  Result:=FParam3+inherited Generate
end;

{ TwsStdGumbel }

  { Descrição
      Classe para geração de valores com distribuição Gumbel
    Descendência
      TwsStdGumbel --> TwsRanDist -- TObject
  }

function TwsStdGumbel.Generate: Double;
begin
  Result:=-Ln(-Ln(Unif.Generate))
end;

{ TwsGumbel }

  { Descrição
      Classe para geração de valores com distribuição Gumbel
    Descendência
      TwsGumbel --> TwsRanDist -- TObject
  }

constructor TwsGumbel.Create(const a,b: Double);
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição Gumbel
  Parâmetros
    a, b: parâmetros da distribuição Gumbel
}
begin
  inherited Create;
  SetFParam1(a);   // parametro de posicao
  SetFParam2(b);   // parametro de dispersao
end;

constructor TwsGumbel.Create;
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição Gumbel
}
begin
  inherited Create;
end;

procedure TwsGumbel.SetFParam1(const x: Double);
{ Objetivo
    Atribui valor ao parâmetro Gamma (tendência central)
  Parâmetros
    x: valor que será atribuído
  Exceções
    Gera uma exceção se o valor a ser atribuído é menor ou igual a zero
}
begin
  FParam1:=x
end;

procedure TwsGumbel.SetFParam2(const x: Double);
{ Objetivo
    Atribui valor ao parâmetro Lambda
  Parâmetros
    x: valor que será atribuído
  Exceções
    Gera uma exceção se o valor a ser atribuído é menor ou igual a zero
}
begin
  if x>0 then
    FParam2:=x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido como parâmetro de dispersão',[8,10,x]))
end;

function TwsGumbel.Generate: Double;
{ Objetivo
    Gera valor de uma variável com distribuição Gumbel. O método utilizado é o da
    inversão da função de distribuição.
}
begin
  Result:=FParam1+inherited Generate/FParam2
end;

{ TwsGVE }

constructor TwsGVE.Create(const a,b,c: Double);
{ Objetivo
    Cria um objeto para geração de valores da distribuição de extremos generalizada
  Parâmetros
    a,b,c: Parâmetros gama, lambda e alfa da distribuição, respectivamente. Alfa e lambda
      devem ser valores estritamente positivos.
  Observações
    Uma mensagem de erro será gerada se a<=0 ou b<=0
}
begin
  inherited Create;
  SetFParam1(a);
  SetFParam2(b);
  SetFParam3(c);
end;

procedure TwsGVE.SetFParam1(const a: Double);
{ Objetivo
    Atribui valor ao parâmetro gama da distribuição
  Parâmetros
    a: valor a atribuir
}
begin
  FParam1:=a;
end;

procedure TwsGVE.SetFParam3(const a: Double);
{ Objetivo
    Atribui valor ao parâmetro alfa da distribuição. Deve ser chamado depois de SetLam.
  Parâmetros
    a: valor a atribuir
  Observações
    Se a<=0, ocorre mensagem de erro
}
begin
  if a>0 then
    begin
    FParam3:=a;
    aux:=FParam2/FParam3
    end
  else
    raise EInvalidParam.Create(Format('Valor Alfa = %*.*g inválido como parâmetro',[8,10,a]))
end;

procedure TwsGVE.SetFParam2(const b: Double);
{ Objetivo
    Atribui valor ao parâmetro Lambda da distribuição
  Parâmetros
    b: valor a atribuir
  Observações
    Se b<=0, ocorre mensagem de erro
}
begin
  if b>0 then
    FParam2:=b
  else
    raise EInvalidParam.Create(Format('Valor Lambda = %*.*g inválido como parâmetro',[8,10,b]))
end;

function TwsGVE.Generate;
var
  w: Double;
begin
  w:=wsMath.Power(-Ln(Unif.Generate),FParam3);
  Result:=FParam1+aux*(1-w)
end;

{ TwsGamma }

  { Descrição
      Classe basica para geração de valores de uma distribuição gama
    Descendência
      TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsGamma.Create(const AParam: Double);
{ Objetivo
    Construtor da classe TwsGamma. Como TwsGamma é classe base para geradores da distribuição
    gama, não deverá ser chamado isoladamente. Como a gama incompleta possui um parâmetro
    apenas (alfa), será o construtor chamado para criar objetos da gama incompleta - TwsGamma1
  Parâmetros
    AParam: valor do parâmetro alfa
  Métodos chamados
    Create herdado
    SetFParam1 (virtual)
}
begin
  inherited Create;
  SetFParam1(AParam)
end;

constructor TwsGamma.Create;
{ Objetivo
    Construtor da classe TwsGamma. Como TwsGamma é classe base para geradores da distribuição
    gama, não deverá ser chamado isoladamente. Como a gama incompleta possui um parâmetro
    apenas (alfa), será o construtor chamado para criar objetos da gama incompleta - TwsGamma1
}
begin
  inherited Create;
end;

{ TwsGamma1 }

  { Descrição
      Classe para geração de valores da distribuição gama incompleta
    Descendência
      TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

procedure TwsGamma1.SetFParam1(const x: Double);
{ Objetivo
    Atualiza parâmetro alfa da distribuição gama. Uma vez que no processo de geração
    muitas constantes dependem deste parâmetro, o método também atualiza estas constantes
  Parâmetros
    x: valor a ser atribuído ao parâmetro alfa
  Métodos chamados
    Nenhum
  Exceções
    Gera exceção de x<=0
}
begin
  if x > 0 then
    begin
    FParam1 := x;
    // obtencao de constantes validas para toda sequencia
    if x<1 then
      begin
      xx[1]:=(2.718281828459 + x)/2.718281828459;  // (e+alfa)/e
      xx[2]:=1/x;                                  // 1/alfa
      xx[3]:=x-1                                   // alfa-1
      end
    else
      if x>1 then
        begin
        xx[2]:=2.50407739677627;                     // d=1+ln(teta)
        xx[3]:=1/Sqrt(2*x-1);                        // a
        xx[4]:=x+(1/xx[3]);                          // q=alfa+1/a
        xx[5]:=x-1.38629436111989                    // b=alfa-ln(4)
        end
    end
  else
    raise EInvalidParam.Create(Format('Valor Alfa = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsGamma1.Generate: Double;
{ Objetivo
    Geração de valores da gama incompleta (beta=1). Se 0<alfa<1 utiliza algoritmo GS
    (técnica de aceitação-rejeição) proposto por Ahrens e Dieter. Se alfa>1, utiliza o
    algoritmo proposto por Cheng, denominado GB (uma modificação do método da aceitação-
    rejeição). Valores X ~ Gama(alfa, beta) podem ser obtidos como X = beta*Y onde
    Y ~ Gama(alfa,1). Descrição em Law & Kelton, pág 487
  Métodos chamados
    Unif.Generate
}
  function GS: Double;
  var
    EndGen: boolean;
    p,y: double;
  { b      --> xx[1]
    1/alfa --> xx[2]
    alfa-1 --> xx[3]
  }
  begin
    EndGen:=False;
    repeat
      p:= xx[1]*Unif.Generate;
      if p<=1 then
        begin
        y:= Power(p,xx[2]);
        EndGen:=Unif.Generate<=exp(-y)
        end
      else
        begin
        y:= -ln((xx[1]-p)/FParam1);
        EndGen:=Unif.Generate <= Power(y,xx[3])
        end;
    until EndGen;
    Result:=y
  end;

  function GB: Double;
  var
    u1,u2,v,y,z,w: double;
  { xx[2] --> d
    xx[3] --> a
    xx[4] --> q
    xx[5] --> b
  }
  begin
    repeat
      u1:=Unif.Generate;
      u2:=Unif.Generate;
      v:=xx[3]*ln(u1/(1-u1));
      y:=FParam1*exp(v);
      z:=u1*u1*u2;
      w:=xx[5]+xx[4]*v-y;
    until ((w+xx[2]-4.5*z)>=0) or (w>=Ln(z));
    Result:=y
  end;

begin
  if FParam1=1 then             // exponencial de media 1
    Result:=-Ln(Unif.Generate)
  else
    if FParam1<1 then           // algoritmo de Ahrens & Dieter
      Result:=GS
    else
      Result:=GB                // algoritmo de Cheng
end;(* Gamma1 *)

  { Descrição
      Classe para geração de valores da distribuição qui-quadrado
    Descendência
      TwsChiSquare --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsChiSquare.Create(df: Double);
begin
  inherited Create(df/2)
end;

constructor TwsChiSquare.Create;
begin
  inherited Create
end;

procedure TwsChiSquare.SetFParam1(const x: Double);
begin
  inherited SetFParam1(x/2)
end;

function TwsChiSquare.Generate: Double;
begin
  Result:=2*(inherited Generate)
end;

{ TwsGamma2 }

  { Descrição
      Classe para geração de valores da distribuição gama de dois parâmetros
    Descendência
      TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsGamma2.Create(const AParam, BParam: double);
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição gama completa (dois
    parâmetros)
  Parâmetros
    AParam - parâmetro alfa
    BParam - parãmetro beta
  Métodos chamados
    SetFParam2
}
begin
  inherited Create(AParam);
  SetFParam2(BParam)
end;

constructor TwsGamma2.Create;
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma distribuição gama completa (dois
    parâmetros)
}
begin
  inherited Create;
end;

procedure TwsGamma2.SetFParam2(const x: Double);
{ Objetivo
    Atribui valor ao parâmetro beta da distribuição
  Parâmetros
    x: valor do parâmetro
  Exceções
    Gera exceção se x<=0
}
begin
  if x > 0 then
    FParam2 := x
  else
    raise EInvalidParam.Create(Format('Valor Lambda = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsGamma2.Generate: double;
{ Objetivo
    Gera valor da distribuição gama utilizando a propriedade: se y ~ gama(alfa,1) então
    x=Lambda*y ~ gama(alfa,Lambda)
  Métodos chamdos
    Generate herdado
}
begin
  Result:=FParam2*(inherited Generate)
end;

{ TwsGamma3 }

  { Descrição
      Classe para geração de valores da distribuição gama com tres parâmetros
    Descendência
      TwsGamma3 --> TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

// cria classe especificando parametros
constructor TwsGamma3.Create(const AParam, BParam, TParam: double);
begin
  inherited Create(AParam,BParam);
  SetFParam3(TParam)
end; (* Create *)

// cria classe sem especificar parametros
constructor TwsGamma3.Create;
begin
  inherited Create;
end;

procedure TwsGamma3.SetFParam3(const x: Double);
{ Objetivo
    Atribui valor ao parâmetro de deslocamento da distribuição
  Parâmetros
    x: valor do parâmetro
  Exceções
    Gera exceção se x<0
}
begin
  if x >= 0 then
    FParam3 := x
  else
    raise EInvalidParam.Create(Format('Valor Teta = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsGamma3.Generate: double;
begin
  Result:=FParam3+(inherited Generate)
end;

{ TwsLPearson3 }
  
  { Descrição
      Classe para geração de valores da distribuição Log-Pearson Tipo III
    Descendência
      TwsLPearson3 --> TwsGamma3 --> TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist
        --> TObject
  }
function TwsLPearson3.Generate: double;
begin
  Result:=exp(inherited Generate)
end;

{ TwsPearson5 }

  { Descrição
      Classe para geração de valores da distribuição Pearson Tipo V
    Descendência
      TwsPearson5 --> TwsGamma2 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsPearson5.Create(const AParam, BParam: double);
begin
  inherited Create(AParam,1/BParam)
end;

constructor TwsPearson5.Create;
begin
  inherited Create
end;

procedure TwsPearson5.SetFParam2(const x: Double);
begin
  if x>0 then
    inherited SetFparam2(1/x)
  else
    raise EInvalidParam.Create(Format('Valor Beta = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsPearson5.Generate: double;
begin
  Result:=1/(inherited Generate)
end;

{ TwsBeta01 }

  { Descrição
      Classe para geração de valores da distribuição beta com valores em (0, 1)
    Descendência
      TwsBeta01 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsBeta01.Create(const AParam, BParam: double);
{ Objetivo
    Cria objeto para geração de valores da distribuição beta de parâmetros especificados.
    Todos os valores gerados estão no intervalo [0, 1].
  Parâmetros
    AParam: parâmetro alfa
    BParam: parâmetro beta
  Métodos chamados
    Create herdado
    SetFParam2
}
begin
  inherited Create(AParam);
  SetFParam2(BParam)
end;

constructor TwsBeta01.Create;
{ Objetivo
    Cria objeto para geração de valores da distribuição beta de parâmetros especificados.
    Todos os valores gerados estão no intervalo [0, 1].
}
begin
  inherited Create;
end;

procedure TwsBeta01.SetFParam2(const x: Double);
{ Objetivo
    Atribui valor ao parâmetro beta da distribuição
  Parâmetros
    x: valor do parâmetro
  Exceções
    Gera exceção se x<=0
}
begin
  if x > 0 then
    FParam2 := x
  else
    raise EInvalidParam.Create(Format('Valor Beta = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsBeta01.Generate: double;
{ Objetivo
    Gera um valor entre 0 e 1 da distribuição beta de parâmetros especificados. O algoritmo
    de geração está descrito em Law & Kelton, pág. 492
  Métodos chamados
    Unif.Generate
    Generate herdado
}
var
  x,tmp: double;
begin
  if FEquals(FParam2,1) then
    if FEquals(FParam1,1) then
      Result:=Unif.Generate                      // Beta(1,1) eh uniforme
    else
      Result:=Power(inherited Generate,1/FParam1)// Inversao simples
  else
    begin
    x:=inherited Generate;              // Gera valor x=Gama(alfa,1)
    tmp:=FParam1;                       // Troca valor do parametro
    SetFParam1(FParam2);                // para gerar y=Gama(beta,1)
    Result:=x/(x+inherited Generate);   // Gera valor x/(x+y)
    SetFParam1(tmp)                     // retorna valor original
    end
end;

{ TwsBetaAB }

  { Descrição
      Classe para geração de valores da distribuição beta com valores em (0, 1)
    Descendência
      TwsBetaAB --> TwsBeta01 --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsBetaAB.Create(const xMin,xMax,AParam,BParam: double);
begin
  inherited Create(AParam,BParam);
  if xMax>xMin then
    begin
      Min:=xMin;
      Range:=xMax-xMin
    end
  else
    raise EInvalidParam.Create(Format('Valores Mínimo = %*.*g e Máximo = %*.*g inválidos',[8,10,xMin,8,10,xMax]))
end;

constructor TwsBetaAB.Create(const xMin,xMax: double);
begin
  inherited Create;
  if xMax>xMin then
    begin
      Min:=xMin;
      Range:=xMax-xMin
    end
  else
    raise EInvalidParam.Create(Format('Valores Mínimo = %*.*g e Máximo = %*.*g inválidos',[8,10,xMin,8,10,xMax]))
end;

function TwsBetaAB.Generate: double;
{ Objetivo
    Gera um valor entre a e b (b>a) da distribuição beta
  Métodos chamados
    Generate
}
begin
  Result:=Min+Range*inherited Generate
end;

{ TwsErlang }

  { Descrição
      Classe para geração de valores da distribuição Erlang (gama incompleta de
      parâmetro inteiro
    Descendência
      TwsErlang --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

procedure TwsErlang.SetFParam1(const x: Double);
{ Objetivo
    Especifica valor para o parâmetro de formato
  Parâmetro
    x: Valor a ser atribuído. O valor truncado de x é que é atribuído ao parâmetro de formato
}
begin
  if x > 0 then
    FParam1 := Trunc(x)
  else
    raise EInvalidParam.Create(Format('Valor = %*.*g inválido para parâmetro de formato',[8,10,x]))
end;

function TwsErlang.Generate: Double;
{ Objetivo
    Gera valores de uma variável com distribuição gama com parâmetro de formato inteiro
  Observação
    Algoritmo obtido de Press et alli (1989) pág. 228
}
var
  am,s,v1,
  v2,e,y : Double;
  malfa,j: Integer;
begin
  malfa:=Trunc(alfa);
  // Se alfa < 6, utiliza o metodo direto, somando os tempos de espera
  if malfa < 6 then
    begin
    Result := 1;
    for j := 1 to malfa do
      Result := Result*Unif.Generate;
    Result := -Ln(Result)
    end
  else // caso contrario, utiliza o metodo da rejeicao
    begin
    repeat
      repeat
        repeat                        // Gerando a tangente do angulo aleatorio
          v1 := 2*Unif.Generate - 1;
          v2 := 2*Unif.Generate - 1;
        until (v1*v1 + v2*v2) <= 1;
        y := v2/v1;
        am := FParam1-1;
        s := Sqrt(2*am + 1);
        Result := s*y + am;               // Decidimos se rejeitamos x
      until Result > 0;                   // Rejeitamos numa regiao de probabilidade 0
      try
        e:=(1+y*y)*exp(am*ln(Result/am)-s*y); // razao de probabilidade para comparacao
      except
        e:=1.0e-30;
      end;
    until Unif.Generate <= e        // rejeita com base num segundo desvio uniforme
  end
end; (* TErlang *)

{ TwsMErlang }

  { Descrição
      Classe para geração de valores da distribuição M-Erlang
    Descendência
      TwsMErlang --> TwsErlang --> TwsGamma1 --> TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsMErlang.Create(const a,b: Double);
begin
  inherited Create(b);   // b - parametro de formato
  SetFParam2(a);         // a - parametro de dispersao
end;

constructor TwsMErlang.Create;
begin
  inherited Create;
end;

procedure TwsMErlang.SetFParam2(const x: Double);
{ Objetivo
    Atribui valor para o parâmetro de dispersão e calcula constante para o processo de geração.
    Deve ser chamada depois de SetFParam1
}
begin
  if x>0 then
    begin
    FParam2 := x;
    aux:= FParam2/FParam1;  // dispersao/formato
    end
  else
    raise EInvalidParam.Create(Format('Valor = %*.*g inválido para parâmetro de dispersão',[8,10,x]))
end;

function TwsMErlang.Generate: Double;
{ Objetivo
    Gerar valores de uma distribuição gama com dois parâmetros, dispresão e formato, sendo o
    parâmetro de formato um número inteiro
  Observação
    Algoritmo adaptado de Law & Kelton, pág. 486
}
begin
  Result:=aux*(inherited Generate)
end;

{ TwsExponential }

  { Descrição
      Classe para geração de valores da distribuição exponencial
    Descendência
      TwsExponential --> TwsGamma --> TwsRanDist --> TObject
  }

procedure TwsExponential.SetFParam1(const x: Double);
begin
  if x > 0 then
    FParam1 := x
  else
    raise EInvalidParam.Create(Format('Valor Lambda = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsExponential.Generate: Double;
var
  x: Double;
begin
  repeat
    x := Unif.Generate;
  until x <> 0;
  Result := -Ln(x)/FParam1
end; (* Generate *)

{ TwsExponential2 }

  { Descrição
      Classe para geração de valores da distribuição exponencial
    Descendência
      TwsExponential2 --> TwsExponential --> TwsGamma --> TwsRanDist --> TObject
  }

constructor TwsExponential2.Create(const xt,xl: Double);
begin
  inherited Create(xl);
  SetFParam2(xt)
end; // Create

constructor TwsExponential2.Create;
begin
  inherited Create;
end; // Create

procedure TwsExponential2.SetFParam2(const x: Double);
begin
  if x >= 0 then
    FParam2 := x
  else
    raise EInvalidParam.Create(Format('Valor Theta = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsExponential2.Generate: Double;
begin
  Result := FParam2+inherited Generate
end; (* Generate *)


{ TwsPoisson }

  { Descrição
      Classe para geração de valores da distribuição de Poisson
    Descendência
      TwsPoisson --> TwsRanDist --> TObject
  }

constructor TwsPoisson.Create(const ALam: Double);
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição Poisson
  Parâmetros
    ALam: valor do parâmetro
  Métodos utilizados
    Create herdado
    SetFParam1
}
begin
  inherited Create;
  SetFParam1(ALam)
end;

constructor TwsPoisson.Create;
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição Poisson
}
begin
  inherited Create;
end;

procedure TwsPoisson.SetFParam1(const x: Double);
{ Objetivo
    Atualiza parâmetro da distribuição Poisson. Atualiza algumas quantidades necessárias
    ao processo de geração e que dependem somente do parâmetro, evitando que sua obtenção
    seja repetida desnecessariamente
  Parâmetros
    x: Valor do parâmetro
}
begin
  if x >= 0 then
    begin
    FParam1 := x;
    if x<12 then
      xx[4]:=exp(-x)
    else
      begin
      xx[2] := Sqrt(2*x);
      xx[3] := Ln(x);
      xx[4] := x*xx[3]-GammLn(x+1)
      end
    end
  else
    raise EInvalidParam.Create(Format('Valor Alfa = %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsPoisson.Generate: Double;
{ Objetivo
    Retorna como valor de ponto flutuante um número inteiro, valor de uma variável
    aleatória com distribuição de Poisson de média Lambda. O algoritmo utilizado está
    baseado no método da aceitação-rejeição e está descrito em Press et alli, pág. 229
}
var
  em,t,y: Double;
{ var
  PoidevSq   <-- xx[2]
  PoidevAlxm <-- xx[3]
  PoidevG    <-- xx[4]
}
begin
  // Utiliza o metodo direto
  if FParam1 < 12 then
    begin
    em := -1;
    t := 1;
    repeat
      em := em + 1;
      { Ao inves de somar valores exponenciais eh equivalente multiplicar por valores
        uniformes, de modo que nunca teremos que calcular o logaritmo mas apenas
        comparar com o valor exponencial calculado anteriormente
       }
      t := t*Unif.Generate;
    until t <= xx[4];
    end
  else           // Utiliza o metodo da rejeicao
    repeat
      repeat
        // y eh um valor da funcao de comparacao Lorentziana
        y := Pi*Unif.Generate;
        y := Sin(y)/Cos(y);
        // em eh y, com origem e escala modificados
        em := xx[2]*y + FParam1;
      until em >= 0;   // Rejeita se esta em regime de probabilidade zero
      em := Trunc(em); // Trunca para uma distribuicao de valores inteiros
      {A razao entre a distribuicao esperada e a funcao de comparacao; aceitamos ou
       rejeitamos por comparacao com outro valor uniforme. O fator 0.9 eh escolhido
       para que t nunca exceda 1}
      try
        t := 0.9*(1+y*y)*exp(em*xx[3]-GammLn(1+em)-xx[4]);
      except
        t := 1.0e-20;
      end;
    until Unif.Generate <= t;
  Result := em
end; // Generate

{ TwsBernoulli }

  { Descrição
      Classe para geração de valores da distribuição de Bernoulli
    Descendência
      TwsBernoulli --> TwsRanDist --> TObject
  }

constructor TwsBernoulli.Create(const p: Double);
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição Bernoulli
  Parâmetros
    p: probabilidade de sucesso
  Métodos utilizados
    Create herdado
    SetFParam1
}
begin
  inherited Create;
  SetFParam1(p)
end;

constructor TwsBernoulli.Create;
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição Bernoulli
}
begin
  inherited Create;
end;

procedure TwsBernoulli.SetFParam1(const x: Double);
{ Objetivo
    Atualiza parâmetro da distribuição de Bernoulli
  Parâmetros
    x: Valor do parâmetro
}
begin
  if (x>0) and (x<1) then
    FParam1 := x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido como parâmetro (não é probabilidade)',[8,10,x]))
end;

function TwsBernoulli.Generate: Double;
{ Objetivo
    Retorna como valor de ponto flutuante um número inteiro, valor de uma variável
    aleatória com distribuição de Bernoulli. O algoritmo utilizado está descrito em
    Law & Kelton, pág. 497
}
begin
  if Unif.Generate <= FParam1 then
    Result := 1
  else
    Result := 0
end;


{ TwsGeometric }

  { Descrição
      Classe para geração de valores da distribuição geométrica
    Descendência
      TwsGeometric --> TwsRanDist --> TObject
  }

constructor TwsGeometric.Create(const p: Double);
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição geométrica
  Parâmetros
    p: valor do parâmetro
  Métodos utilizados
    Create herdado
    SetFParam1
}
begin
  inherited Create;
  SetFParam1(p)
end;

constructor TwsGeometric.Create;
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição geométrica
}
begin
  inherited Create;
end;

procedure TwsGeometric.SetFParam1(const x: Double);
{ Objetivo
    Atualiza parâmetro da distribuição geométrica.
  Parâmetros
    x: Valor do parâmetro
}
begin
  if (x>=0) and (x<=1) then
    FParam1 := x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido como parâmetro (não é probabilidade)',[8,10,x]))
end;

function TwsGeometric.Generate: Double;
{ Objetivo
    Retorna como valor de ponto flutuante um número inteiro, valor de uma variável
    aleatória com distribuição geométrica de parâmetro. O algoritmo utilizado está
    descrito em Law & Kelton, pág. 502
}
begin
  Result := Floor((ln(Unif.Generate))/ln(1-FParam1))
end;

{ TwsNegBin }

  { Descrição
      Classe para geração de valores da distribuição binomial negativa
    Descendência
      TwsNegBin --> TwsGeometric --> TwsRanDist --> TObject
  }

constructor TwsNegBin.Create(const s,p: Double);
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição geométrica
  Parâmetros
    s: Número de sucessos para a modelagem do número de falhas até que esse número de
       sucessos ocorra
    p: probabilidade de sucesso em cada experimento
  Métodos utilizados
    Create herdado
    SetFParam1
    SetFParam2
}
begin
  inherited Create(p);
  SetFParam2(s)
end;

constructor TwsNegBin.Create;
{ Objetivo
    Cria e inicializa uma classe para geração de valores de uma distribuição geométrica
}
begin
  inherited Create;
end;

procedure TwsNegBin.SetFParam2(const x: Double);
{ Objetivo
    Atualiza parâmetro s da distribuição binomial negativa.
  Parâmetros
    x: Valor do parâmetro
}
begin
  if x>0 then
    FParam2 := x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido como parâmetro',[8,10,x]))
end;

function TwsNegBin.Generate: Double;
{ Objetivo
    Retorna como valor de ponto flutuante um número inteiro, valor de uma variável
    aleatória com distribuição binomial negativa, obtido como a soma de valores de
    uma variável geométrica. O algoritmo utilizado está descrito em Law & Kelton, pág. 502
}
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Trunc(FParam2) do
    Result := Result + inherited Generate
end;

{  TwsUniform }

  { Descrição
      Classe para geração de valores da distribuição uniforme
    Descendência
      TwsUniform --> TwsRanDist --> TObject
  }

constructor TwsUniform.Create(const a, b: Double);
begin
  inherited Create;
  if a<b then
    begin
    FParam1:=a;
    FParam2:=b;
    Dif:=b-a
    end
  else
    raise EInvalidParam.Create(Format(
     'Valores Alfa = %*.*g e Beta = %*.*g inválidos como parâmetros',[8,10,a,8,10,b]));
end;

constructor TwsUniform.Create;
begin
  inherited Create;
end;

procedure TwsUniform.SetFParam1(const a: Double);
begin
  FParam1:=a
end;

// deve ser chamada depois de SetFParam1
procedure TwsUniform.SetFParam2(const b: Double);
begin
  if FParam1<b then
    begin
    FParam2:=b;
    Dif:=b-FParam1
    end
  else
    raise EInvalidParam.Create(Format(
     'Valores Alfa = %*.*g e Beta = %*.*g inválidos como parâmetros',[8,10,FParam1,8,10,b]));
end;

function TwsUniform.Generate: Double;
begin
  Result := FParam1 + Dif*Unif.Generate;
end;

{ TwsDiscreteUnif }

  { Descrição
      Classe para geração de valores da distribuição uniforme discreta
    Descendência
      TwsDiscreteUnif --> TwsRanDist --> TObject
  }

constructor TwsDiscreteUnif.Create(const a, b: Double);
begin
  inherited Create;
  if a<b then
    begin
    FParam1:=a;
    FParam2:=b;
    Num:=b-a+1
    end
  else
    raise EInvalidParam.Create(Format(
      'Valores Alfa = %*.*g e Beta = %*.*g inválidos como parâmetros',[8,10,a,8,10,b]));
end;

constructor TwsDiscreteUnif.Create;
begin
  inherited Create;
end;

procedure TwsDiscreteUnif.SetFParam1(const a: Double);
begin
  FParam1:=a
end;

// deve ser chamada depois de SetFParam1
procedure TwsDiscreteUnif.SetFParam2(const b: Double);
begin
  if FParam1<b then
    begin
    FParam2:=b;
    Num:=b-FParam1+1
    end
  else
    raise EInvalidParam.Create(Format(
     'Valores Alfa = %*.*g e Beta = %*.*g inválidos como parâmetros',[8,10,FParam1,8,10,b]));
end;

function TwsDiscreteUnif.Generate: Double;
begin
  Result := FParam1 + Floor(Num*Unif.Generate);
end;

{ TwsBinomial }

  { Descrição
      Classe para geração de valores da distribuição binomial
    Descendência
      TwsBinomial --> TwsRanDist --> TObject
  }

constructor TwsBinomial.Create(const nn, pp: Double);
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma variável aleatória com
    distribuição Binomial.
  Parâmetros
    nn: número de tentativas ou experimentos
    pp: probabilidade de sucesso em cada tentativa
  Métodos chamados
    SetFParam1
    SetFParam2
}
begin
  inherited Create;
  SetFParam1(nn);
  SetFParam2(pp);
end;

constructor TwsBinomial.Create;
{ Objetivo
    Cria e inicializa objeto para geração de valores de uma variável aleatória com
    distribuição Binomial.
  Parâmetros
    nn: número de tentativas ou experimentos
    pp: probabilidade de sucesso em cada tentativa
  Métodos chamados
    SetFParam1
    SetFParam2
}
begin
  inherited Create;
end;

procedure TwsBinomial.SetFParam1(const x: Double);
{ Objetivo
    Atualiza o valor do parãmetro n (número de tentativas ou experimentos). Atualiza
    também quantidades que dele são dependentes no processo de geração de valores de
    variáveis com essa distribuição
  Parâmetros
    x: valor do parãmetro n
 }
begin
  if x > 0 then
    begin
    FParam1 := x;
    xx[3] := x;
    xx[2] := LogGamma(1+x);
    n := Trunc(x);
    end
  else
    raise EInvalidParam.Create(Format('Valor n = %*.*g inválido como parâmetro',[8,10,x]))
end;

procedure TwsBinomial.SetFParam2(const x: Double);
{ Objetivo
    Atualiza o valor do parãmetro p (probabilidade de sucesso en cada tentativas ou
    experimento). Atualiza também quantidades que dele são dependentes no processo de
    geração de valores de variáveis com essa distribuição
  Parâmetros
    x: valor do parãmetro p
 }
begin
  if (x > 0) and (x < 1) then
    begin
    FParam2 := x;
    if x <= 0.5 then
      xx[1] := x
    else
      xx[1] := 1-x;
    // Média do valor que sera produzido
    xx[4] := 1-xx[1];
    xx[5] := Ln(xx[1]);
    xx[6] := Ln(xx[4]);
    end
  else
    raise EInvalidParam.Create(Format(
      'Valor p = %*.*g inválido como parâmetro (não é probabilidade)',[8,10,x]))
end;

function TwsBinomial.Generate: Double;
{ Objetivo
    Retorna como número de ponto flutuante um inteiro, valor de uma variável aleatória
    com distribuição binomial, de parâmetros n e p. O algoritmo utilizado está baseado
    no método da aceitação-rejeição, exceto quando n<25, quando a variável é encontrada
    como a soma de variáveis Bernoulli, e está descrito em Press et alli, pág. 231
}
var
  am,em,g,
  angle,sq,
  bnl,t,y  : Double;
  j        : Integer;
{ var
  xx[1] <--  pp
  xx[2] <--  BnldevOldg
  xx[3] <--  BnldevEn
  xx[4] <--  BnldevPc
  xx[5] <--  BnldevPlog
  xx[6] <--  BnldevPclog
}
begin
  am := FParam1*xx[1];
  // Utiliza o metodo direto se n nao for muito grande. Pode gerar ate 25 uniformes
  if n < 25 then
    begin
    bnl := 0;
    for j := 1 to n do
      if Unif.Generate < xx[1] then
        bnl := bnl+1;
    end
  else
    { Se esperamos menos que um evento fora de 25 ou mais experimentos, entao a
      distribuicao Poisson eh acurada para este caso. Utiliza metodo direto para Poisson
    }
    if am < 1 then
      begin
      g := Exp(-am);
      t := 1;
      j := -1;
      repeat
        Inc(j);
        t := t*Unif.Generate;
      until (t < g) or (j = n);
      bnl := j
      end
    else // Utiliza o metodo da rejeicao
      begin
      { O codigo que segue refere-se ao metodo da rejeicao com funcao de comparacao
        Lorentziana
      }
      sq := Sqrt(2*am*xx[4]);
      repeat
        repeat
          angle := Pi*Unif.Generate;
          y := Sin(angle)/Cos(angle);
          em := sq*y + am;
        until (em >= 0) and (em < (xx[3] + 1));
        em := Trunc(em); // reduz para um valor inteiro
        try
          t := 1.2*sq*(1+y*y)*exp(xx[2]-LogGamma(em+1)-LogGamma(xx[3]-em+1)
            +em*xx[5]+(xx[3]-em)*xx[6]);
        except
          t := 1.0e-20;
        end;
      until (Unif.Generate <= t);
      { A rejeicao acontece cerca de 1.5 vezes por valor gerado, em media }
      bnl := em
      end;
    // Desfaz a transformacao de simetria
    if xx[1] <> FParam2 then
      bnl := n-bnl;
    Result := bnl
end; (* BinomialRejec *)

{ TwsHyperGeom }

  { Descrição
      Classe para geração de valores da distribuição hipergeométrica
    Descendência
      TwsHyperGeom --> TwsRanDist --> TObject
  }

constructor TwsHyperGeom.Create(const nn, nn1, pp: Double);
begin
  inherited Create;
  SetFParam1(nn); { Tamanho da populacao }
  SetFParam2(nn1);{ Tamanho da amostra }
  SetFParam3(pp)  { Tamanho do grupo de sucesso }
end;

constructor TwsHyperGeom.Create;
begin
  inherited Create;
end;

procedure TwsHyperGeom.SetFParam1(const x: Double);
{ Tamanho da populacao }
begin
  if x > 0 then
    FParam1 := x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido para Tamanho da população',[8,10,x]))
end;

procedure TwsHyperGeom.SetFParam2(const x: Double);
{ Tamanho da amostra }
begin
  if (x > 0) and (x<=FParam1) then
    FParam2 := x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido para Tamanho da amostra',[8,10,x]))
end;

procedure TwsHyperGeom.SetFParam3(const x: Double);
{ Tamanho do grupo de sucesso }
begin
  if (x > 0) and (x <= FParam1) then
    FParam3 := x
  else
    raise EInvalidParam.Create(Format('Valor %*.*g inválido para Tamanho grupo sucesso',[8,10,x]))
end;

function TwsHyperGeom.Generate: Double;
var
  s,i: Integer;
  p,N: Double;
begin
  p:=FParam3/FParam1; // probabilidade de sucesso
  N:=FParam1;
  Result := 0;
  for i:=1 to Trunc(FParam2) do
    begin
    if ((Unif.Generate-p) > 0) then
      s:=0
    else
      begin
      s:=1;
      Result:=Result+1
      end;
    p:=(N*p-s)/(N-1);
    N:=N-1
    end
end;


{ ========= Rotinas para implementacao de simulacoes didaticas ========= }

function RFreqStability(n: Integer; const p: Double): TwsGeneral;
{ Objetivo
    Simula uma série de experimentos de Bernoulli e obtém a frequência relativa de
    sucessos (valores de Bernoulli iguais a 1) para cada elemento da sequência. O
    objetivo básico é mostrar que quanto maior a sequência de experimentos mais a
    frequência relativa se aproxima do probabilidade de sucesso.
  Parâmetros
    n: Número de experimentos a realizar
    p: Probabilidade de sucesso
  Saída
    Matriz com três linhas e n colunas:
      - a primeira armazena quantos experimentos foram realizados
      - a segunda armazena a frequência absoluta de sucessos.
      - a terceira armazena a frequência relativa de sucessos.
}
var
  i,r: Integer;
  Rand    : TwsRandom;
begin
  Result:=TwsGeneral.Create(3,n);
  Result.PrintOptions.ColPrecision:=6;
  Result.RowName[1]:='Experimento';
  Result.RowName[2]:='Frequência';
  Result.RowName[3]:='Freq. Relativa';
  Rand:=TwsLinear.Create;
  r:=0;
(*
  Randomize;
  if RandSeed>0 then
    Inic:=-RandSeed
  else
    Inic:=RandSeed;
*)
  for i:=1 to n do
    begin
    Result.ColName[i]:='Exp'+IntToStr(i);
    if Rand.Generate<=p then
      Inc(r);
    Result[1,i]:=i;
    Result[2,i]:=r;
    Result[3,i]:=r/i;
    end;
  Rand.Free
end;

function ConfidenceIntervals(n,m: Integer; const PopMean,PopSDev: Double; out f5,f1,t5,t1: Double;
  out DS: TwsDataSet; RetDS: boolean=False): TwsDataSet;
{ Objetivo
    Retorna um conjunto de dados relativos a cobertura da média verdadeira pelo intervalo
    ao nível de confiança especificado. São geradas amostras de uma população normal e para
    cada uma das amostras é obtido o intervalo de confiança e verificado se ele cobre a média
    verdadeira.
  Parâmetros
    n       : Tamanho de cada amostra
    m       : Número de amostras
    PopMean,
    PopSDev : média e desvio padrão da população que será amostrada
    f,r     : frequência absoluta e relativa de intervalos que cobre a verdadeira média
    LConf   : nível de confiança para construção do intervalo
    DS      : Conjunto de dados que retorna as amostras geradas
    RetDS   : True se as amostras serão retornadas, false DS retorna nil
  Saída
    Conjunto de dados com as seguintes informações (cada linha corresponde a uma amostra)
    Coluna1: Média
    Coluna2: Desvio Padrão
    Coluna3: Limite inferior do IC 95%
    Coluna4: Limite superior do IC 95%
    Coluna5: SIM se o intervalo 95% cobre a média verdadeira, NAO, caso contrário
    Coluna6: Limite inferior do IC 99%
    Coluna7: Limite superior do IC 99%
    Coluna8: SIM se o intervalo 99% cobre a média verdadeira, NAO, caso contrário
}
var
  i,j,vObs  : Integer;
  mean,sdev : Double;

  Erro      : Word;
  Col       : TwsDataSetCol;
  Dist      : TwsRanDist;
  Samp,v    : TwsVec;
begin
  // Cria conjunto das amostras
  if RetDS then
    begin
    DS:=TwsDataSet.Create('AmostNorm');
    for i:=1 to n do
      DS.MAdd(TwsDFVec.Create(m));
    end
  else
    DS:=nil;
  // Conjunto de dados com os resultados da simulacao
  Result:=TwsDataSet.Create('CoberturaIC');
  with Result do
    begin
    MLab:='Coberturas da verdadeira média de amostras geradas de populações normais';
    ColIdentName:='Amostra';
    Struct.AddColEx(TwsNumeric.Create('Media','Média da amostra',12,7));                                     //1
    Struct.AddColEx(TwsNumeric.Create('DesvPadr','Desvio padrão da amostra',12,7));                          //2
    Struct.AddColEx(TwsNumeric.Create('ExtrInf95','Extremo inferior do intervalo de confiança a 95%',12,7)); //3
    Struct.AddColEx(TwsNumeric.Create('ExtrSup95','Extremo superior do intervalo de confiança a 95%',12,7)); //4
    Col:=TwsQualitative.Create('Cobert95','SIM, se o IC 95% cobriu a verdadeira média, NAO cc',6);           //5
    With TwsQualitative(Col) Do
      Begin
      AddLevel('SIM');
      AddLevel('NAO');
      End; { With Col }
    Struct.AddColEx(Col);
    Struct.AddColEx(TwsNumeric.Create('ExtrInf99','Extremo inferior do intervalo de confiança a 99%',12,7)); //6
    Struct.AddColEx(TwsNumeric.Create('ExtrSup99','Extremo superior do intervalo de confiança a 99%',12,7)); //7
    Col:=TwsQualitative.Create('Cobert99','SIM, se o IC 99% cobriu a verdadeira média, NAO cc',6);           //8
    With TwsQualitative(Col) Do
      Begin
      AddLevel('SIM');
      AddLevel('NAO');
      End; { With Col }
    Struct.AddColEx(Col);
    end;
  // Valor de t para o intervalo de confianca
  t1:=TInv(0.01,n-1,1.0e-10,True,False,Erro);
  t5:=TInv(0.05,n-1,1.0e-10,True,False,Erro);
  // Gerador das amostras
  Dist := TwsNormal.Create(PopMean,PopSDev);
  f5:=0;
  f1:=0;
  // Gera as amostras e constroi os IC
  for i:=1 to m do
    begin
    if RetDS then
      DS.Struct.AddColEx(TwsNumeric.Create('Amost'+IntToStr(i),'Amostra gerada '+IntToStr(i),12,5));
    // Gera amostra da normal de tamanho n
    Samp:=TwsDFVec(Dist.GenerateSample(n));
    // Obtem media e variancia da amostra
    Samp.VarMean(mean,sdev,vObs);
    // Erro padrao da media
    sdev:=Sqrt(sdev/n);

    // Linha de Result
    v:=TwsDFVec.Create(8);
    v.Name:='Amostra'+IntToStr(i);
    v[1]:=mean;
    v[2]:=sdev;
    v[3]:=mean-t5*sdev;
    v[4]:=mean+t5*sdev;
    // Media esta no intervalo 0.95?
    if (PopMean>=v[3]) and (PopMean<=v[4]) then
      begin
      v[5]:=0;
      f5:=f5+1
      end
    else
      v[5]:=1;
    v[6]:=mean-t1*sdev;
    v[7]:=mean+t1*sdev;
    // Media esta no intervalo 0.99?
    if (PopMean>=v[6]) and (PopMean<=v[7]) then
      begin
      v[8]:=0;
      f1:=f1+1
      end
    else
      v[8]:=1;
    Result.MAdd(v);

    // Guarda valores da amostra, se desejado
    if RetDS then
      for j:=1 to n do
        DS[j,i]:=Samp[j];
    Samp.Free
    end;
//  Dist.Free
end; // ConfidenceIntervals

function OneSampleTest(var Samp: TwsVec; Simul: boolean; h0: Double; m,n: Integer; out f1,f5,
  t1,t5: Double; out DS: TwsDataSet; DSave: boolean=False): TwsDataSet;
{  Objetivo
     Simula execução de testes de hipótese para uma média considerando amostras retiradas
     de distribuições normais
   Parâmetros
     Samp : Dependendo da situação contém os valores dos parâmetros para simulação ou
            os dados de uma amostra especificada. No primeiro caso:
            1 - Média da população
            2 - Desvio padrão da população
     Simul: True se os dados serão simulados, False se Samp já contiver a amostra
     h0   : valor da hipótese H0
     m, n : número e tamanho das amostras
     f5,f1: Número de amostras onde houve rejeição a 5% e 1% da hipótese Ho: mu = h0
   Resultado
     Conjunto de dados onde retornarão os resultados das simulações
       1: Média
       2: DP
       3: Erro padrão da média
       4: Estatística t para a hipótese Ho: mu = h0
       5: Rejeita Ho a 5% ?
       6: Rejeita H0 a 1% ?
}
var
  Dist      : TwsNormal01;
  v         : TwsDFVec;
  i,nObs,
  j         : Integer;
  xVar,xMean,
  pMean,pDev: Double;
  Erro      : Word;
  Col       : TwsDataSetCol;
begin
  // Valores criticos da distribuicao t
  t5:=TInv(0.05,n-1,1.0e-10,True,False,Erro);
  t1:=TInv(0.01,n-1,1.0e-10,True,False,Erro);
  // Conjunto que ira armazenar os resultados
  Result:=TwsDataSet.Create('TH_1Med');
  with Result do
    begin
    MLab:='Resultados dos testes de hipóteses para cada amostra';
    Struct.AddColEx(TwsNumeric.Create('Media','Média da amostra',12,7));              //1
    Struct.AddColEx(TwsNumeric.Create('DesvPadr','Desvio padrão da amostra',12,7));   //2
    Struct.AddColEx(TwsNumeric.Create('ErPadrao','Erro padrão da média',12,7));       //3
    Struct.AddColEx(TwsNumeric.Create('t','Estatística t',12,4));                     //4
    Col:=TwsQualitative.Create('Rejeita5','SIM, se H0 foi rejeitada a 5%, NAO cc',6);
    With TwsQualitative(Col) Do
      Begin
      AddLevel('NAO');
      AddLevel('SIM');
      End; { With Col }
    Struct.AddColEx(Col);                                                             //5
    Col:=TwsQualitative.Create('Rejeita1','SIM, se H0 foi rejeitada a 1%, NAO cc',6);
    With TwsQualitative(Col) Do
      Begin
      AddLevel('NAO');
      AddLevel('SIM');
      End; { With Col }
    Struct.AddColEx(Col);                                                             //6
    end;
  DS:=nil;
  if DSave then
    begin
    DS:=TwsDataSet.Create('Comp1Med');
    for i:=1 to m do
      DS.Struct.AddColEx(TwsNumeric.Create('Amost'+IntToStr(i),
        'Amostra de população normal '+IntToStr(i),8,5)); //m
    for i:=1 to n do
      DS.MAdd(TwsDFVec.Create(m));
    end;

  f1:=0; f5:=0;
  Dist:=nil;
  // Gerador de amostras da distribuição normal
  if Simul then
    begin
    Dist:=TwsNormal01.Create;
    pMean:=Samp[1];
    pDev:=Samp[2];
    Samp.Free;
    Samp:=TwsDFVec.Create(n);
    end;
  for i:=1 to m do                          // Para cada amostra
    begin
    for j:=1 to n do
      begin
      if Simul then
        Samp[j]:=pMean+pDev*Dist.Generate; // Gera amostra de tamanho n
      if DSave then
          DS[j,i]:=Samp[j];
      end;
    Samp.VarMean(xMean,xVar,nObs);          // Obtem media e variancia
    v:=TwsDFVec.Create(6);
    v[1]:=xMean;                            // Media
    v[2]:=Sqrt(xVar);                       // Desvio padrao
    v[3]:=Sqrt(xVar/n);                     // Erro padrao da media
    v[4]:=(xMean-h0)/v[3];                  // Estatistica t
    v[5]:=Integer(Abs(v[4])>t5);            // Significativo a 5%?
    f5:=f5+v[5];
    v[6]:=Integer(Abs(v[4])>t1);            // Significativo a 1%?
    f1:=f1+v[6];
    Result.MAdd(v)
    end;
  Dist.Free
end; // OneSampleTest

function TwoSampleTest(var xSamp,ySamp: TwsVec; sPar: TwsVec; h0: Double; m,xn,yn: Integer;
  out f1,f5,t1,t5: Double; out DS: TwsDataSet; DSave: Boolean=False): TwsDataSet;
{  Objetivo
     Simula execução de testes de hipótese para comparação de médias de duas populações
     normais das quais tem-se amostras independentes.
   Parâmetros
     xSamp : vetor que contém os valores da variável X. Se x for nil então os dados serão simulados
     ySamp : Vetor com os valores da variável Y.
     sPar  : Se x for nil contém os parâmetros para simulação
       1 - Média de X
       2 - Desvio padrão de X
       3 - Média de Y
       4 - Desvio padrão de Y
     h0   : valor da hipótese H0
     m    : número de amostras que serão retiradas
     xn,yn: tamanho das amostras
     f5,f1: número de amostras onde houve rejeição a 5% e 1% da hipótese Ho: mux-muy = h0
     t1,t5: valores críticos da distribuição t a 1 e 5%
     DS   : Conjunto de dados que armazena os dados gerados, se necessário
     DSave: True, se houver a necessidade do armazenamento dos valores gerados
   Resultado
     Conjunto de dados onde retornarão os resultados das simulações
       1: xMédia
       2: yMédia
       3: xVar
       4: yVar
       5: nx
       6: ny
       7: xyVar
       8: Estatística t para a hipótese Ho: mux-muy = h0
       9: Rejeita Ho a 5% ?
      10: Rejeita H0 a 1% ?
}
var
  Dist         : TwsNormal01;
  v            : TwsDFVec;
  GenXY        : Boolean;
  i,nObs,df,i1,
  j            : Integer;
  xVar,xMean,
  yVar,yMean   : Double;
  Erro         : Word;
  Col          : TwsDataSetCol;
begin
  df:=xN+yN-2;
  // Valores criticos da distribuicao t
  t5:=TInv(0.05,df,1.0e-10,True,False,Erro);
  t1:=TInv(0.01,df,1.0e-10,True,False,Erro);
  // Conjunto que ira armazenar os resultados
  Result:=TwsDataSet.Create('TH_2Medias');
  with Result do
    begin
    MLab:='Resultados dos testes de hipóteses para cada par de amostras';
    Struct.AddColEx(TwsNumeric.Create('xMedia','Média da amostra X',12,7));           //1
    Struct.AddColEx(TwsNumeric.Create('yMedia','Média da amostra Y',12,7));           //2
    Struct.AddColEx(TwsNumeric.Create('xVar','Variância da amostra X',12,7));         //3
    Struct.AddColEx(TwsNumeric.Create('yVar','Variância da amostra Y',12,7));         //4
    Struct.AddColEx(TwsNumeric.Create('xN','Tamanho da amostra X',12,7));             //5
    Struct.AddColEx(TwsNumeric.Create('yN','Tamanho da amostra Y',12,7));             //6
    Struct.AddColEx(TwsNumeric.Create('xyVar','Variância combinada',12,7));           //7
    Struct.AddColEx(TwsNumeric.Create('Estat_T','Valor da estatística t',12,7));      //8

    Col:=TwsQualitative.Create('Rejeita5','SIM, se H0 foi rejeitada a 5%, NAO cc',6); //9
    With TwsQualitative(Col) Do
      Begin
      AddLevel('NAO');
      AddLevel('SIM');
      End; { With Col }
    Struct.AddColEx(Col);
    Col:=TwsQualitative.Create('Rejeita1','SIM, se H0 foi rejeitada a 1%, NÃO cc',6); //10
    With TwsQualitative(Col) Do
      Begin
      AddLevel('NAO');
      AddLevel('SIM');
      End; { With Col }
    Struct.AddColEx(Col);
    end;

  if DSave then     // Se os dados gerados serao salvos
    begin
    DS:=TwsDataSet.Create('Comp2Med');
    Col:=TwsQualitative.Create('FatorA','Fator de 2 niveis cujas médias serão comparadas',3); //1
    With TwsQualitative(Col) Do
      Begin
      AddLevel('X');     // Cada nivel corresponde a uma variavel
      AddLevel('Y');
      End; { With Col }
    DS.Struct.AddColEx(Col);
    for i:=1 to m do
      DS.Struct.AddColEx(TwsNumeric.Create('Amost'+IntToStr(i),
        'Valores gerados para a amostra'+IntToStr(i),12,7)); //m+i
    for i:=1 to xN do                // primeiros xN valores serao da variavel X
      begin
      v:=TwsDFVec.Create(m+1);   // posicao do nivel + m amostras
      v[1]:=0;
      DS.MAdd(v)
      end;
    for i:=1 to yN do                // yN valores seguintes serao da variavel Y
      begin
      v:=TwsDFVec.Create(m+1);
      v[1]:=1;
      DS.MAdd(v)
      end
    end;

  GenXY:=xSamp=nil;
  Dist:=nil;
  // Gerador de amostras da distribuição normal
  if GenXY then
    begin
    Dist:=TwsNormal01.Create;
    xSamp:=TwsDFVec.Create(xN);
    ySamp:=TwsDFVec.Create(yN);
    end;

  f1:=0; f5:=0;
  for i:=1 to m do                                   // Para cada amostra
    begin
    if GenXY then
      begin
      for j:=1 to xN do
        xSamp[j]:=sPar[1]+sPar[2]*Dist.Generate; // Gera amostra X de tamanho xn
      for j:=1 to yN do
        ySamp[j]:=sPar[3]+sPar[4]*Dist.Generate; // Gera amostra X de tamanho xn
      end;
    // Se vai salvar armazena os dados gerados
    if DSave then
      begin
      i1:=i+1;
      for j:=1 to xN do
        DS[j,i1]:=xSamp[j];
      for j:=1 to yN do
        DS[xN+j,i1]:=ySamp[j];
      end;

    xSamp.VarMean(xMean,xVar,nObs);                  // Obtem media e variancia X
    ySamp.VarMean(yMean,yVar,nObs);                  // Obtem media e variancia Y

    v:=TwsDFVec.Create(10);
    v[1]:=xMean;                                     // Media X
    v[2]:=yMean;                                     // Media Y
    v[3]:=xVar;                                      // Variancia X
    v[4]:=yVar;                                      // Variancia Y
    v[5]:=xN;                                        // Variancia X
    v[6]:=yN;                                        // Variancia Y
    v[7]:=((xN-1)*xVar+(yN-1)*yVar)/df;              // Variancia combinada
    v[8]:=(xMean-yMean-h0)/Sqrt((1/xN+1/yN)*v[7]);   // Estatistica t
    v[9]:=Integer(Abs(v[8])>t5);                     // Significativo a 5%?
    f5:=f5+v[9];
    v[10]:=Integer(Abs(v[8])>t1);                    // Significativo a 1%?
    f1:=f1+v[10];
    Result.MAdd(v);
    end;
  Dist.Free
end; // TwoSampleTest

procedure Descriptive(Samp: TwsVec; out Stat,Quant: TwsVec; out DS: TwsDataSet);
{ Objetivo
    Gera uma amostra de um determinado tamanho para obtenção de medidas descritivas
  Parâmetros
    Samp: Conjunto de valores gerados
    Stat: Retorna as medidas descritivas
      1: Numero de observacoes
      3: Media
      4: Soma
      5: Variancia
      6: Desvio padrao
      9: Soma de quadrados corrigida
     10: Assimetria
     11: Curtose
   Quant: Retorna quantis da distribuição
      1 - Minimo
      2 - 1o. quartil
      3 - Mediana
      4 - 3o. quartil
      5 - Maximo
   DS: Matriz que retorna a amostra gerada e outras quantidades como desvios, etc.
     Coluna 1: Valores da amostra gerada
     Coluna 2: Desvios (diferenças em relação a média)
     Coluna 3: Diferenças em relação a mediana
     Coluna 4: Quadrados dos desvios
     Coluna 5: Quadrados das diferenças em relação a mediana
}
var
  i       : Integer;
  v       : TwsVec;
  s,sm,sqm: Double;
begin
  // Obtem os quantis e os momentos
  Samp.QuickSort(True);
  Quant:=TwsDFVec.Create([0,0.25,0.5,0.75,1]);
  Samp.Percentis(Quant);
  Stat:=Samp.Moments;

  // Conjunto de dados que ira armazenar os valores
  DS:=TwsDataSet.Create('EstatDesc');
  DS.Struct.AddColEx(TwsNumeric.Create('Y','Valores da variável',12,4));  //1
  DS.Struct.AddColEx(TwsNumeric.Create('Desvios',
    'Diferenças entre cada valor e a média',12,4));                                //2
  DS.Struct.AddColEx(TwsNumeric.Create('QDesv','Quadrados dos desvios',12,5)); //3
  DS.Struct.AddColEx(TwsNumeric.Create('DMediana',
    'Diferenças entre cada valor e a mediana',12,4));                              //4
  DS.Struct.AddColEx(TwsNumeric.Create('QDif',
    'Quadrados das diferenças entre cada valor e a mediana',12,5));                //5

  sm:=0; sqm:=0; s:=0;
  for i:=1 to Samp.Len do
    begin
    v:=TwsDFVec.Create(5);
    v.Name:='Obs.'+IntToStr(i);
    v[1]:=Samp[i];
    v[2]:=Samp[i]-Stat[3];
    v[3]:=Samp[i]-Quant[3];
    sm:=sm+v[3];
    v[4]:=Sqr(v[2]);
    s:=s+v[4];
    v[5]:=Sqr(v[3]);
    sqm:=sqm+v[5];
    DS.MAdd(v);
    end;
  v:=TwsDFVec.Create(5);
  v.Name:='Totais';
  v[1]:=Stat[4];
  v[2]:=0;
  v[3]:=sm;
  v[4]:=s;
  v[5]:=sqm;
  DS.MAdd(v);
end; // Descriptive

function BivariateNormal(const mPop1,dp1,mPop2,dp2,r: double; n: Integer;
  out EP: TwsGeneral): TwsDataSet;
{ Objetivo
    Obtém uma amostra de uma população normal bivariada. A partir dos valores dessa
    amostra obtém, através de rotação ortogonal, valores de uma amostra bivariada de
    variáveis estocasticamente independentes
  Parâmetros
    mPop1,
    dp1   : média e desvio padrão da primeira variável
    mPop2,
    dp2   : média e desvio padrão da segunda variável
    r     : coeficiente de correlação entre as variáveis a gerar
    n     : tamanho da amostra a gerar
    EP    : Matriz (9 x 2) com as estimativas dos parâmetros e outras quantidades de interesse
     (a1,b1) - coeficientes linear e angular da reta correspondente a primeira variável X/y
     (a2,b2) - coeficientes linear e angular da reta correspondente a segunda variável  Y/x
     (ma,mb) - coeficientes angular e linear (tan(alfa)) para o eixo maior da elipse
     (ay,by) - valores constantes (da forma quadrática) correspondente à elipse de contorno

     (m1,m2) - medias amostrais da primeira e da segunda variavel
     (s1,s2) - somas de quadrados de desvios da primeira e da segunda variavel
     (s12,A) - soma de produtos das variáveis 1 e 2 e ângulo (alfa) entre as duas retas
     (a,b)   - coeficientes linear e angular da reta de mínimos quadrados entre as variáveis 1 e 2
     (r,.)   - correlação amostral
   Retorno
     Conjunto de dados com 6 colunas numéricas:
       - duas para armazenar o par com distribuição normal bidimensional
       - duas para armazenar o par de variáveis obtidas pela transformação de rotação
       - duas para armazenar os valores estimados e resíduos do ajustamento de MQ
}
var
  i,j         : Integer;
  t11,t21,t22,
  p1,p2,z1,z2,
  alfa,tAlfa,
  var1,var2,
  xMean,yMean : Double;
  Nor01       : TwsNormal01;
  v           : TwsDFVec;
begin
  // Obtem fator triangular da matriz de dispersao 2x2
  t11 := dp1;
  p1 := r*dp1*dp2;                // ro*s1*s2
  p2 := 2*p1;
  t21 := p1/t11;
  var1 := Sqr(dp1);
  var2 := Sqr(dp2);
  t22 := Sqrt(var2-Sqr(t21));

  // Obtem angulo alfa e tan(alfa)
  tAlfa := (var2-var1+Sig(r)*Sqrt(Sqr(p2)+Sqr(var2-var1)))/p2;
  Alfa := ArcTan(tAlfa);

  // Matriz das estimativas dos parametros da normal bivariada
  EP:=TwsGeneral.Create(9,2);
  EP[1,2] := r*dp1/dp2;
  EP[1,1] := mPop1-EP[1,2]*mPop2;
  EP[2,2] := r*dp2/dp1;
  EP[2,1] := mPop2-EP[2,2]*mPop1;
  EP[3,2] := tAlfa;
  EP[3,1] := mPop2-tAlfa*mPop1;
  p1:=var1+var2;
  p2:=2*dp1*dp2*Sqrt(1-r*r);
  EP[4,1] := (Sqrt(p1+p2)+Sig(r)*Sqrt(p1-p2))/2;
  EP[4,2] := Sqrt(p1+p2)-EP[4,1];

  // Conjunto de dados para armazenar os valores gerados
  Result := TwsDataSet.Create('BivarNorm');
  Result.Struct.AddColEx(TwsNumeric.Create('X','Primeira variável',12,8));                //1
  Result.Struct.AddColEx(TwsNumeric.Create('Y','Segunda variável',12,8));                 //2
  Result.Struct.AddColEx(TwsNumeric.Create('X1','Primeira variável transformada',12,8));   //3
  Result.Struct.AddColEx(TwsNumeric.Create('Y1','Segunda variável transformada',12,8));    //4
  Result.Struct.AddColEx(TwsNumeric.Create('MEst','Valor estimado pela reta de MQ',12,8)); //5
  Result.Struct.AddColEx(TwsNumeric.Create('Residuo','Resíduo da estimação',12,6));        //6

  // Gera normal padrão
  Nor01 := TwsNormal01.Create;
  xMean := 0; yMean := 0; var1 := 0; var2 := 0; p2 := 0;
  for i := 1 to n do
    begin
    v := TwsDFVec.Create(6);
    z1 := Nor01.Generate;
    // valores x
    v[1] := mPop1+t11*z1;
    z2 := Nor01.Generate;
    v[2] := mPop2+t21*z1+t22*z2;

    // somas de quadrados e produtos para (x, y)
    p1 := v[1]-xMean;
    xMean := xMean+p1/i;               // media de x
    var1 := var1+p1*(v[1]-xMean);      // sqd x
    p1 := v[2]-yMean;
    yMean := yMean+p1/i;               // media de y
    p2 := p2+p1*(v[1]-xMean);          // soma de produtos
    var2 := var2+p1*(v[2]-yMean);      // sqd xy

    // valores y (transformacao ortogonal)
    v[3] := (v[1]-mPop1)*Cos(Alfa)+(v[2]-mPop2)*Sin(Alfa);
    v[4] := -(v[1]-mPop1)*Sin(Alfa)+(v[2]-mPop2)*Cos(Alfa);
    Result.MAdd(v);
    end; // for i

  EP[5,1] := xMean;                 // Media de X
  EP[5,2] := yMean;                 // Media de Y
  EP[6,1] := var1;                  // q(x)
  EP[6,2] := var2;                  // q(y)
  EP[7,1] := p2;                    // q(xy)
  EP[8,2] := p2/var1;               // coeficiente angular Y/x
  EP[8,1] := yMean-EP[8,2]*xMean;   // coeficiente linear Y/x
  EP[9,1] := p2/Sqrt(var1*var2);    // coeficiente de correlacao amostral
  EP[9,2] := Sqr(EP[9,1]);          // coeficiente de determinacao
  // Obtem valores estimados e residuos
  for i := 1 to n do
    begin
    Result[i,5]:=EP[8,1]+EP[8,2]*Result[i,1];   // yest = a + bx
    Result[i,6]:=Result[i,2] - Result[i,5]      // res = y - yest
    end;
end; // BivariateNormal


function BivariateXY(xVec,yVec: TwsVec; out EP,Anova: TwsGeneral; n: Integer): TwsDataSet;
{ Objetivo
    Ajusta o modelo de regressão linear y ~ a+bx
  Parâmetros
    x, y  : vetores com valores de x e de y, respectivamente. Se x for nil os dois vetores
            serão gerados e y, na entrada, deverá conter os parâmetros para geração. Nesse
            caso, seus componentes serão:
            1 - Media X
            2 - Media Y
            3 - DP X
            4 - DP Y
            5 - Corr XY
    EP    : Matriz (7 x 2) com as estimativas dos parâmetros e outras quantidades de interesse
      1,1 - Media de X
      1,2 - Média de Y
      2,1 - SQD x
      2,2 - SQD y
      3,1 - SPD xy
      4,2 - coeficiente angular
      4,1 - coeficiente linear
      5,1 - coeficiente de correlacao
      5,2 - coeficiente de determinacao
      3,2 - Erro padrão de b
      6,1 - estatistica t
      6,2 - Valor p
      7,1 - Limite inferior do intervalo de confianca
      7,2 - Limite superior do intervalo de confiança
     Anova: Matriz 3 x 5 com os resultados da análise da variação (RL, Res e Tot)
        1 - Graus de liberdade
        2 - Soma de quadrados
        3 - Estatística F
        4 - Quadrado médio
        5 - Valor P
    n     : Número de valores que serão gerados, se for o caso
   Retorno
     Conjunto de dados com 8 colunas numéricas (para cada observação):
       1 - x
       2 - y
       3 - Y estimado
       4 - Resíduo
       5 - Diagonal de H
       6 - Variância residual sem a observação atual
       7 - Resíduo estudentizado externamente
       8 - Desvio no ajuste padronizado
}
var
  i,j        : Integer;
  ErrCode    : Word;
  p1,p2,t11,
  t21,t22,t,f1,
  var1,var2,f2,
  xMean,yMean: Double;
  x,y,v      : TwsVec;
  Nor01      : TwsNormal01;
begin
  y:=yVec.Copy(1,yVec.Len);
  if xVec = nil then    // x e y serão simulados
    begin
    // Obtem fator triangular da matriz de dispersao 2x2
    t11 := y[3];
    t21 := y[5]*y[3]*y[4]/t11;
    t22 := Sqrt(Sqr(y[4])-Sqr(t21));
    xMean:=y[1];
    yMean:=y[2];
    y.Free; y:=nil;
    // Gera normal padrão
    Nor01 := TwsNormal01.Create;
    // x e y irao armazenar os valores gerados
    x:=TwsDFVec.Create(n);
    y:=TwsDFVec.Create(n);
    for i := 1 to n do
      begin
      p1 := Nor01.Generate;
      x[i] := xMean+t11*p1;
      p2 := Nor01.Generate;
      y[i] := yMean+t21*p1+t22*p2
      end;
    end
  else // if
    x:=xVec.Copy(1,xVec.Len);
  if n<>y.Len then
    begin
    MessageDLG('Vetores x e y devem ter a mesma dimensão', mtInformation, [mbOK], 0);
    Exit
    end;
  // Conjunto de dados para armazenar os valores gerados
  Result := TwsDataSet.Create('RegLinSimp');
  Result.Struct.AddColEx(TwsNumeric.Create('X','Variável preditora',12,5));                      //1
  Result.Struct.AddColEx(TwsNumeric.Create('Y','Variável resposta',12,5));                       //2
  Result.Struct.AddColEx(TwsNumeric.Create('MEst','Valor estimado pelo modelo',12,5));           //3
  Result.Struct.AddColEx(TwsNumeric.Create('Residuo','Resíduo da estimação',12,5));              //4
  Result.Struct.AddColEx(TwsNumeric.Create('DiagH','Diagonal de H',12,5));                       //5
  Result.Struct.AddColEx(TwsNumeric.Create('S2i','Variância residual sem a observação i',12,5)); //6
  Result.Struct.AddColEx(TwsNumeric.Create('RExt','Resíduo estudentizado externamente',12,5));   //7
  Result.Struct.AddColEx(TwsNumeric.Create('DFitS','Desvio no ajustamento padronizado',12,5));   //8
  Result.Struct.AddColEx(TwsNumeric.Create('ICMInf','Limite inferior do IC para a média',12,5)); //9
  Result.Struct.AddColEx(TwsNumeric.Create('ICMSup','Limite superior do IC para a média',12,5)); //10
  Result.Struct.AddColEx(TwsNumeric.Create('ICPInf','Limite inferior do IC para previsão',12,5));//11
  Result.Struct.AddColEx(TwsNumeric.Create('ICPSup','Limite superior do IC para previsão',12,5));//12
  Result.Struct.AddColEx(TwsNumeric.Create('BonInf','Limite inferior do IC para previsão',12,5));//13
  Result.Struct.AddColEx(TwsNumeric.Create('BonSup','Limite superior do IC para previsão',12,5));//14
  Result.Struct.AddColEx(TwsNumeric.Create('SchInf','Limite inferior do IC para previsão',12,5));//15
  Result.Struct.AddColEx(TwsNumeric.Create('SchSup','Limite superior do IC para previsão',12,5));//16

  xMean := 0; yMean := 0; var1 := 0; var2 := 0; p2 := 0;
  for i := 1 to n do
    begin
    v := TwsDFVec.Create(16);
    // valores x e y
    v[1] := x[i];
    v[2] := y[i];
    // somas de quadrados e produtos para (x, y)
    p1 := v[1]-xMean;
    xMean := xMean+p1/i;               // media de x
    var1 := var1+p1*(v[1]-xMean);      // sqd x
    p1 := v[2]-yMean;
    yMean := yMean+p1/i;               // media de y
    p2 := p2+p1*(v[1]-xMean);          // soma de produtos
    var2 := var2+p1*(v[2]-yMean);      // sqd xy
    Result.MAdd(v);
    end; // for i
  x.Free;
  y.Free;

  // Matriz das estimativas dos parametros
  EP:=TwsGeneral.Create(7,2);
  EP.Name:='Miscel';
  EP.MLab:='Miscelânea de quantidades importantes na análise';
  EP[1,1] := xMean;
  EP[1,2] := yMean;
  EP[2,1] := var1;                  // q(x)
  EP[2,2] := var2;                  // q(y)
  EP[3,1] := p2;                    // q(xy)
  EP[4,2] := ScalarDiv(p2,var1);               // coeficiente angular
  EP[4,1] := yMean-EP[4,2]*xMean;   // coeficiente linear
  EP[5,1] := ScalarDiv(p2,Sqrt(var1*var2));    // coeficiente de correlacao
  EP[5,2] := ScalarSqr(EP[5,1]);          // coeficiente de determinacao

  // Tabela para analise da variacao
  Anova:=TwsGeneral.Create(3,5);
  Anova.Name:='Anova';
  Anova.MLab:='Quadro da análise da variação';
  Anova.ColName[1]:='GL';
  Anova.ColName[2]:='S_Quadrad';
  Anova.ColName[3]:='Q_Medio';
  Anova.ColName[4]:='Estat_F';
  Anova.ColName[5]:='Valor_p';
  Anova.RowName[1]:='Reg. Linear';
  Anova.RowName[2]:='Residuo';
  Anova.RowName[3]:='Total';

  Anova[1,1]:=1;   Anova[1,2]:=ScalarDiv(Sqr(p2),var1); Anova[1,3]:=Anova[1,2];
  Anova[2,1]:=n-2; Anova[2,2]:=var2-Anova[1,2];
  Anova[2,3]:=ScalarDiv(Anova[2,2],Anova[2,1]);
  Anova[1,4]:=ScalarDiv(Anova[1,3],Anova[2,3]);  // F
  Anova[1,5]:=FInt(Anova[1,4],1,Anova[2,1],True,ErrCode); // valor p
  Anova[3,1]:=n-1; Anova[3,2]:=var2;
  Anova[2,4]:=wscMissValue; Anova[2,5]:=wscMissValue;
  Anova[3,3]:=wscMissValue; Anova[3,4]:=wscMissValue; Anova[3,5]:=wscMissValue;

  EP[3,2] := ScalarSqrt(Anova[2,3]/var1);                      // s(b1)
  EP[6,1] := ScalarDiv(EP[4,2],EP[3,2]);                       // estatistica t
  EP[6,2] := TInt(EP[6,1],n-2,False,True,ErrCode);             // Valor p
  t:=TInv(0.05,n-2,1.0e-10,True,False,ErrCode);
  f1:=FInv(0.025,1,n-2,1e-9,True,ErrCode);                     // f para Bonferr
  f2:=FInv(0.05,2,n-2,1e-9,True,ErrCode);                      // f para Scheffe

  p1 := t*EP[3,2];
  EP[7,1] := EP[4,2]-p1;                                       // LInf
  EP[7,2] := EP[4,2]+p1;                                       // LSup

  // Obtem valores estimados, residuos e estatisticas de diagnostico
  for i := 1 to n do
    begin
    Result[i,3] := EP[4,1]+EP[4,2]*Result[i,1];                   // yest
    Result[i,4] := Result[i,2]-Result[i,3];                       // r
    Result[i,5] := 1/n+Sqr(Result[i,1]-xMean)/var1;               // h
    p1 := 1-Result[i,5];                                          // 1-h
    p2 := Sqr(Result[i,4]);                                       // r**2
    Result[i,6] := (n-2)/(n-3)*Anova[2,3]-p2/((n-3)*p1);          // s2i
    try
      Result[i,7] := Sig(Result[i,4])*Sqrt(p2/(p1*Result[i,6]));  // rse
                                                                  //dfits
      Result[i,8] := Sig(Result[i,4])*Sqrt((Result[i,5]/p1)*(p2/(Result[i,6]*p1)));
    except
      Result[i,7] := wscMissValue;
      Result[i,8] := wscMissValue;
    end;
    p1:=Result[i,5]*Anova[2,3];               // h*s2
    Result[i,9]:=Result[i,3]-t*Sqrt(p1);      // LI - Media
    Result[i,10]:=Result[i,3]+t*Sqrt(p1);     // LS - Media

    Result[i,13]:=Result[i,3]-Sqrt(f1*p1);    // LI - Bonf
    Result[i,14]:=Result[i,3]+Sqrt(f1*p1);    // LS - Bonf

    Result[i,15]:=Result[i,3]-Sqrt(2*f2*p1);  // LI - Scheffe
    Result[i,16]:=Result[i,3]+Sqrt(2*f2*p1);  // LS - Scheffe

    p1:=(Result[i,5]+1)*Anova[2,3];           // (h+1)*s2
    Result[i,11]:=Result[i,3]-t*Sqrt(p1);     // LI - Previsao
    Result[i,12]:=Result[i,3]+t*Sqrt(p1);     // LS - Previsao
    end;
end; // BivariateXY

function OneWay(const m, v: TwsVec; r: TwsLIVec; out DS: TwsDataSet; DSave: boolean=False): TwsGeneral;
{ Objetivo
    Simula amostras relativas a uma classificação simples com cinco níveis, retornando uma
    matriz com quantidades de interesse na análise desse tipo de classificação
  Parâmetros
    m: vetor de médias para cada população
    v: vetor de variâncias para cada população
    r: vetor com o número de repetições em cada grupo ou amostra
    DS: Conjunto de dados que retorna os dados gerados
  Retorno
    Matriz geral com o seguinte conteúdo
     As primeiras 5 linhas armazenam os conteúdos de cada amostra e a linha 6 as quantidades
     gerais
     Colunas:
      1 - Médias amostrais (de cada amostra gerada)
      2 - Componentes da soma de quadrados entre amostras
      3 - Graus de liberdade
      4 - Componentes da soma de quadrados dentro
      5 - Variância dentro de cada amostra
}
var
  i,j,n  : Integer;
  Dist        : TwsNormal;
  x,y         : TwsDFVec;
  Col         : TwsDataSetCol;
  mean,gmean,
  ssq,gssq,aux,
  aux1,aux2   : Double;
begin
  if (m.Len=v.Len) and (m.Len=r.Len) then
    begin
    // Conjunto de dados que armazenara os dados gerados
    DS:=nil;
    if DSave then
      begin
      DS:=TwsDataSet.Create('C_Simples');
      Col:=TwsQualitative.Create('FatorA','Fator com A 5 niveis',3); //1
      With TwsQualitative(Col) Do
        Begin
        AddLevel('A1');
        AddLevel('A2');
        AddLevel('A3');
        AddLevel('A4');
        AddLevel('A5');
        End; { With Col }
      DS.Struct.AddColEx(Col);
      DS.Struct.AddColEx(TwsNumeric.Create('Y','Valores da variável resposta gerada',8,5)); //2
      end;

    // Geracao dos valores
    Dist:=TwsNormal.Create(m[1],Sqrt(v[1]));
    // Para cada nivel
    Result:=TwsGeneral.Create(6, 5);
    Result.Name:='CS_Geral';
    Result.ColName[1]:='Media'; Result.ColName[2]:='SQ_Entre'; Result.ColName[3]:='GL';
    Result.ColName[4]:='SQ_Dentro'; Result.ColName[5]:='V_Dentro';
    Result.RowName[1]:='A1'; Result.RowName[2]:='A2'; Result.RowName[3]:='A3';
    Result.RowName[4]:='A4'; Result.RowName[5]:='A5'; Result.RowName[6]:='Geral';
    Result.PrintOptions.ColPrecision:=7;
    gmean:=0;
    gssq:=0;                                // sqd total
    n:=0;
    aux2:=0;
    for i:=1 to 5 do
      begin
      Dist.Mean:=m[i];
      Dist.SDev:=Sqrt(v[i]);
      x:=TwsDFVec(Dist.GenerateSample(r[i]));
      mean:=0;
      ssq:=0;
      for j:=1 to r[i] do
        begin
        if DSave then
          begin
          y:=TwsDFVec.Create(2);
          y[1]:=i-1;
          y[2]:=x[j];
          DS.MAdd(y)
          end;
        Inc(n);                       // numero de observacoes
        aux := x[j]-mean;
        aux1 := x[j]-gmean;
        mean := mean+aux/j;
        gmean:=gmean+aux1/n;              // media geral
        ssq := ssq+aux*(x[j]-mean);       // sq dentro
        gssq:=gssq+aux1*(x[j]-gmean);      // ssq total
        end;
      aux2:=aux2+ssq;
      Result[i,1]:=mean;                 // media da amostra i
      Result[i,3]:=r[i]-1;               // graus de liberdade
      Result[i,4]:=ssq;                  // sqd da amostra i
      Result[i,5]:=ssq/(r[i]-1);         // variancia dentro
      x.Free;
      end;
    Result[6,1]:=gmean;
    Result[6,3]:=n-5;
    Result[6,4]:=gssq;
    Result[6,5]:=aux2/(n-5);
    gssq:=0;
    for i:=1 to 5 do
      begin
      Result[i,2]:=Sqr(Result[i,1]-gmean)*r[i];
      gssq:=gssq+Result[i,2]
      end;
    Result[6,2]:=gssq;
    Dist.Free;
    end
  else
    begin
    DS:=nil;
    Result:=nil;
    MessageDLG('Vetores de médias, variâncias e repetições devem ter a mesma dimensão',
      mtInformation, [mbOK], 0);
    end;
end;

function TwoWay(const m, v: TwsVec; r: TwsLIVec; out DS: TwsDataSet; out A, B: TwsGeneral;
  DSave: boolean=False): TwsGeneral;
{ Objetivo
    Simula amostras relativas a uma classificação dupla (A com 3 niveis, B com 2), retornando uma
    matriz com quantidades de interesse na análise desse tipo de classificação
  Parâmetros
    m: vetor de médias para cada população
    v: vetor de variâncias para cada população
    r: vetor com o número de repetições em cada grupo ou amostra
    DS: Conjunto de dados que retorna os dados gerados
  Retorno
    Matriz geral com o seguinte conteúdo
     As primeiras 6 linhas armazenam os conteúdos de cada amostra e a linha 7 as quantidades
     gerais
     Colunas:
      1 - Médias amostrais (de cada amostra gerada)
      2 - Componentes da soma de quadrados entre amostras
      3 - Graus de liberdade
      4 - Componentes da soma de quadrados dentro
      5 - Variância dentro de cada amostra
}
const
  na=3;
  nb=2;
var
  i,j,nLin,k,
  n,L        : Integer;
  Dist       : TwsNormal;
  x,y        : TwsDFVec;
  Col        : TwsDataSetCol;
  z,aux,aux1 : Double;
begin
  if (m.Len=v.Len) and (m.Len=r.Len) then
    begin
    // Conjunto de dados que armazenara os dados gerados
    DS:=nil;
    if DSave then
      begin
      DS:=TwsDataSet.Create('C_Dupla');
      Col:=TwsQualitative.Create('FatorA','Fator com A 3 niveis',3); //1
      With TwsQualitative(Col) Do
        for i:=1 to na do
          AddLevel('A'+IntToStr(i));
      DS.Struct.AddColEx(Col);

      Col:=TwsQualitative.Create('FatorB','Fator com B 2 niveis',3); //2
      With TwsQualitative(Col) Do
        for i:=1 to nb do
          AddLevel('B'+IntToStr(i));
      DS.Struct.AddColEx(Col);

      DS.Struct.AddColEx(TwsNumeric.Create('Y','Valores da variável resposta gerada',8,5)); //2
      end;
    // Geracao dos valores
    Dist:=TwsNormal.Create(m[1],Sqrt(v[1]));
    nLin:=na*nb+1;
    Result:=TwsGeneral.Create(nLin,5);
    Result.Name:='CD_Geral';
    Result.ColName[1]:='Media'; Result.ColName[2]:='SQEntre'; Result.ColName[3]:='GL';
    Result.ColName[4]:='SQDentro'; Result.ColName[5]:='VDentro';
    Result.PrintOptions.ColPrecision:=5;
    Result[nLin,1]:=0;
    n:=0;
    L:=0;
    a:=TwsGeneral(Jota(2,na,mtGeneral,0));
    a.Name:='A';
    a.MLab:='Matriz de médias e somas de quadrados';
    a.RowName[1]:='Médias';
    a.RowName[2]:='SQ';
    a.PrintOptions.ColPrecision:=5;

    b:=TwsGeneral(Jota(2,nb,mtGeneral,0));
    b.Name:='B';
    b.MLab:='Matriz de médias e somas de quadrados';
    b.RowName[1]:='Médias';
    b.RowName[2]:='SQ';
    b.PrintOptions.ColPrecision:=5;
    for i:=1 to na do                        // Para cada nivel de A
      for j:=1 to nb do
        begin
        inc(L);
        Result[L,1]:=0;
        Result[L,2]:=0;
        Result.RowName[L]:='A'+IntToStr(i)+'B'+IntToStr(j);
        Dist.Mean:=m[L];
        Dist.SDev:=Sqrt(v[L]);
        x:=TwsDFVec(Dist.GenerateSample(r[L]));
        for k:=1 to r[L] do
          begin
          if DSave then
            begin
            y:=TwsDFVec.Create(3);
            y[1]:=i-1;
            y[2]:=j-1;
            y[3]:=x[k];
            DS.MAdd(y)
            end;
          Inc(n);                                             // numero de observacoes
          aux := x[k]-Result[L,1];
          aux1 := x[k]-Result[nLin,1];
          Result[L,1] := Result[L,1]+aux/k;                   // media da combinacao
          Result[nLin,1] := Result[nLin,1]+aux1/n;            // media geral
          Result[L,4] := Result[L,4]+aux*(x[k]-Result[L,1]);  // sq dentro
          end;
        Result[nLin,4] := Result[nLin,4] + Result[L,4];       // acumula sq dentro
        Result[L,3] := r[L]-1;                                // graus de liberdade
        Result[L,5] := Result[L,4]/(r[L]-1);                  // variancia dentro
        a[1,i]:=a[1,i]+Result[L,1];
        b[1,j]:=b[1,j]+Result[L,1];
        x.Free;
        end;
    Result.RowName[nLin]:='Geral';

    for i:=1 to na do
      begin
      a.ColName[i]:='A'+IntToStr(i);
      a[1,i]:=a[1,i]/nb;                             // medias dos niveis de A
      a[2,i]:=Sqr(a[1,i]-Result[nLin,1])*nb*r[1];    // somas de quadrados
      end;

    for i:=1 to nb do
      begin
      b.ColName[i]:='B'+IntToStr(i);
      B[1,i]:=B[1,i]/na;                             // medias dos niveis de B
      B[2,i]:=Sqr(B[1,i]-Result[nLin,1])*na*r[1];    // somas de quadrados
      end;

    Result[nLin,3]:=n-6;                             // graus de liberdade do residuo
    Result[nLin,5]:=Result[nLin,4]/(Result[nLin,3]); // qm residuo
    Result[nLin,2]:=0;
    for i:=1 to 6 do
      begin
      Result[i,2]:=Sqr(Result[i,1]-Result[nLin,1])*r[i];
      Result[nLin,2]:=Result[nLin,2]+Result[i,2];
      end;
    Dist.Free;
    end
  else
    begin
    DS:=nil;
    Result:=nil;
    MessageDLG('Vetores de médias, variâncias e repetições devem ter a mesma dimensão',
      mtInformation, [mbOK], 0);
    end;
end; // TwoWay

function StatSimul(m,n: Integer; Par: array of Double; Stat: array of TwsEnumStatistics;
  DType: TDist): TwsDataSet;
{ Objetivo
    Obtém um conjunto de dados cujas linhas contêm estatísticas obtidas de um conjunto de
    valores simulados
  Parãmetros
    m: Quantidade de amostras a gerar
    n: Tamanho de cada amostra a gerar
    Par: Parâmetros da distribuição que gerará os valores
    Stat: Estatísticas que deverão ser calculadas para cada amostra. As estatísticas que
      poderão ser obtidas são:
       teMedia, teVar, teDPadr, teTotal, teMin, teMax, teN, teNVal, teEPMed, teAmpl, teCVar,
       teAssim, teCurt, teSQNCo, teSQCor, teMediana, teQuart1, teQuart2
    DType: Tipo de distribuição para geração dos dados (dos quais serão obtidas as estísticas)
      As que poderão ser consideradas aqui são:
        dtNormal, dtUniform, dtExponencial, dtBinomial, dtPoisson
    Retorno
      Conjunto de dados com m linhas e High(Stat)+1 colunas numéricas. Cada coluna corresponde
      aos valores de uma estatística para cada amostra
}
var
  i,j      : Integer;
  Dist     : TwsRanDist;
  x,v,QS,MS: TwsVec;
  StName   : String;
begin
  case dType of
    dbNormal:  Dist:=TwsNormal.Create(Par[0],Par[1]);
    dbUnif:    Dist:=TwsUniform.Create(Par[0],Par[1]);
    dbExpon:   Dist:=TwsExponential.Create(Par[0]);
    dbBinom:   Dist:=TwsBinomial.Create(Trunc(Par[0]),Par[1]);
    dbPoisson: Dist:=TwsPoisson.Create(Par[0]);
  end; // case
  Result:=TwsDataSet.Create;
  for j:=0 to High(Stat) do
    begin
    case Stat[j] of
     teMedia:   StName:='Media';
     teVar:     StName:='Varianc';
     teDPadr:   StName:='DesvPad';
     teTotal:   StName:='Total';
     teMin:     StName:='Minimo';
     teMax:     StName:='Maximo';
     teEPMed:   StName:='EPMedia';
     teAmpl:    StName:='Amplit';
     teCVar:    StName:='CoefVar';
     teAssim:   StName:='Assimet';
     teCurt:    StName:='Curtose';
     teSQCor:   StName:='SQCorr';
     teMediana: StName:='Mediana';
     teQuart1:  StName:='Quart1';
     teQuart2:  StName:='Quart3';
    end; // case
    Result.Struct.AddColEx(TwsNumeric.Create(StName,'Estatística '+IntToStr(j+1),12,7));
    end;
  for i:=1 to m do
    begin
    // Gera amostra
    x:=TwsDFVec(Dist.GenerateSample(n));
    // Calcula momentos amostrais
    MS:=x.Moments;
    x.QuickSort(True);
    // Calcula quantis
    QS:=TwsDFVec.Create([0,0.25,0.5,0.75,1]);
    QS.Percentis(QS);
    // Constroi a linha com as estatisticas
    v:=TwsDFVec.Create(Length(Stat));
    for j:=0 to High(Stat) do
      case Stat[j] of
       teMedia:   v[j+1]:=MS[3];
       teVar:     v[j+1]:=MS[5];
       teDPadr:   v[j+1]:=MS[6];
       teTotal:   v[j+1]:=MS[4];
       teMin:     v[j+1]:=QS[1];
       teMax:     v[j+1]:=QS[5];
       teEPMed:   v[j+1]:=MS[7];
       teAmpl:    v[j+1]:=QS[5]-QS[1];
       teCVar:    v[j+1]:=MS[8];
       teAssim:   v[j+1]:=MS[10];
       teCurt:    v[j+1]:=MS[11];
       teSQCor:   v[j+1]:=MS[9];
       teMediana: v[j+1]:=QS[3];
       teQuart1:  v[j+1]:=QS[2];
       teQuart2:  v[j+1]:=QS[4];
      end; // case
      // Insere a linha
      Result.MAdd(v);
      x.Free;
      MS.Free;
      QS.Free
    end;
  Dist.Free
end;

// Testes de rotinas para obtencao de numeros randomicos

procedure RandomTest(Tipo: byte);
const
  Iter=20;
var
  j,k,jPower        : Integer;
  yProb       : TwsDFVec;
  iy          : array[1..3] of Integer;
  x1,x2,x3,x4 : double;
  Res         : TwsGeneral;
  Rnd         : TwsRandom;

  function fnc(const x1,x2,x3,x4: double): double;
    begin
      Result:=sqrt(sqr(x1)+sqr(x2)+sqr(x3)+sqr(x4))
    end;

  function TwoToj(j: integer): integer;
    begin
      if j=0 then
        TwoToj:=1
      else
        TwoToj:=2*TwoToj(j-1)
    end;

begin
  Res:=TwsGeneral.Create(0,3);
  Res.Name:='NumRand';
  Res.MLab:='Volume de uma n-esfera - n=2, 3, 4';
  Res.ColName[1]:='Pi';Res.ColName[2]:='Pi4_3'; Res.ColName[3]:='Pi_12_2';

  case Tipo of
    0: Rnd:=TwsShuffle.Create;
    1: Rnd:=TwsLinear.Create;
    2: Rnd:=TwsSubtractive.Create;
    3: Rnd:=TwsPMMLCG.Create;
    end; // case

  iy[1]:=0;iy[2]:=0;iy[3]:=0;
  for j:=1 to Iter do
    begin
    jPower:=TwoToj(j);
    for k:=TwoToj(j-1) to jPower do
      begin
      x1:=Rnd.Generate;
      x2:=Rnd.Generate;
      x3:=Rnd.Generate;
      x4:=Rnd.Generate;
      if fnc(x1,x2,0,0)<1 then Inc(iy[1]);
      if fnc(x1,x2,x3,0)<1 then Inc(iy[2]);
      if fnc(x1,x2,x3,x4)<1 then Inc(iy[3]);
      end;
    yProb:=TwsDFVec.Create(3);
    yProb.Name:=IntToStr(jPower);
    yProb[1]:=4*iy[1]/jPower;
    yProb[2]:=8*iy[2]/jPower;
    yProb[3]:=16*iy[3]/jPower;
    Res.MAdd(yProb);
    end;

  // Valores corretos
  yProb:=TwsDFVec.Create(3);
  yProb.Name:='Correto';
  yProb[1]:=Pi;
  yProb[2]:=4*Pi/3;
  yProb[3]:=0.5*Sqr(Pi);
  Res.MAdd(yProb);        // iter+1

  // Diferencas
  yProb:=TwsDFVec.Create(3);
  yProb.Name:='Diferencas';
  yProb[1]:=Res[Iter,1]-Res[Iter+1,1];
  yProb[2]:=Res[Iter,2]-Res[Iter+1,2];
  yProb[3]:=Res[Iter,3]-Res[Iter+1,3];
  Res.MAdd(yProb);       // iter+2

  // Diferencas percentuais
  yProb:=TwsDFVec.Create(3);
  yProb.Name:='Diferencas%';
  yProb[1]:=Res[Iter+2,1]/Res[Iter+1,1]*100;
  yProb[2]:=Res[Iter+2,2]/Res[Iter+1,2]*100;
  yProb[3]:=Res[Iter+2,3]/Res[Iter+1,3]*100;
  Res.MAdd(yProb);     // iter+3
end;


end.
