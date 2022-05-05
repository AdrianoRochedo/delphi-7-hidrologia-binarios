unit Rosenbrock_Class;

interface
uses Classes,
     Rosenbrock_FormParameter,
     Rosenbrock_Form;


type
  TrbParameter = class
  private
    FForm         : TrbFormParameter;
    FLowLimit     : Real;
    FCurrentValue : Real;
    FStep         : Real;
    FValue        : Real;
    FTolerance    : Real;
    FHiLimit      : Real;
    FName         : String;
    procedure SetHiLimit(const Value: Real);
    procedure SetLowLimit(const Value: Real);
    procedure SetName(const Value: String);
    procedure SetStep(const Value: Real);
    procedure SetTolerance(const Value: Real);
    procedure SetValue(const Value: Real);
  public
    Constructor Create; overload;
    Constructor Create(const aName: String; const aValue, aLowLimit,
                       aHiLimit, aStep, aTolerance: Real); overload;

    destructor Destroy; override;
    procedure Show(Left, Top: Integer);

    property Name         : String           read FName         write SetName;
    property Value        : Real             read FValue        write SetValue;
    property LowLimit     : Real             read FLowLimit     write SetLowLimit;
    property HiLimit      : Real             read FHiLimit      write SetHiLimit;
    property Step         : Real             read FStep         write SetStep;
    property Tolerance    : Real             read FTolerance    write SetTolerance;
    property Form         : TrbFormParameter read FForm         write FForm;
  end;

  TrbParameters = class
  private
    FList: TList;
    function getCount: Integer;
    function GetPar(i: Integer): TrbParameter;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Add(Param: TrbParameter): Integer;
    function  ParamByName(const Name: String): TrbParameter;
    procedure Clear;

    property Count                 : Integer      read getCount;
    property Parameter[i: Integer] : TrbParameter read GetPar; default;
  end;

  TTermProc    = procedure of object;
  TVisualProc  = procedure(Params: TrbParameters; FO: Real; Simulacao, Estagio: Integer) of object;
  TGeralFunction   = function(Params: TrbParameters): Real of object;

  TRosenbrock = class
  private
    FParams: TrbParameters;             // Parâmetros
    FTOL   : Real;                     // Tolerância da Funcão Objetivo

    FVisualProc : TVisualProc;  // Para feedback visual
    FGeralFunction  : TGeralFunction;
    FTermProc   : TTermProc;

    FForm : TrbForm;

    FMTS : Longword;
    FMS  : Longword;

    KOUNT         : Integer;
    NumEstagio    : Integer;
    FO            : Real;
    FIncPasso     : Real;
    FDecPasso     : Real;
    FPassoInicial : Boolean;
    FTerminated   : Boolean;

    procedure ProcessMessages;
    procedure DoVisualUpdate;
    procedure SetDecPasso(const Value: Real);
    procedure SetIncPasso(const Value: Real);
    procedure SetTOL(const Value: Real);
  Public
    constructor Create;
    destructor  Destroy; override;

    procedure Execute;
    procedure Stop;
    procedure ShowFO;

    // Propriedades
    property Tolerance           : Real             read FTOL            write SetTOL;
    property IncStep             : Real             read FIncPasso       write SetIncPasso;
    property DecStep             : Real             read FDecPasso       write SetDecPasso;
    property InitialStep         : Boolean          read FPassoInicial   write FPassoInicial;
    property MaxSimulations      : Longword         read FMS             write FMS;
    property MaxTimeSimulation   : Longword         read FMTS            write FMTS;
    property Parameters          : TrbParameters    read FParams         write FParams;
    property Form                : TrbForm          read FForm           write FForm;

    // Eventos
    property OnGeralFunction     : TGeralFunction   read FGeralFunction  write FGeralFunction;
    property OnVisualProc        : TVisualProc      read FVisualProc     write FVisualProc;
    property OnTerminateProc     : TTermProc        read FTermProc       write FTermProc;
  end;

implementation
uses windows, Messages, SysUtils;

{ TRosenbrock }

constructor TRosenbrock.Create;
begin
  inherited Create;

  FMTS        := 3888000000; // 45 dias
  FMS         := MaxInt;
  FTOL        := 0.001;
  FIncPasso   := 3.0;
  FDecPasso   := 0.5;
  FParams     := TrbParameters.Create;
end;

destructor TRosenbrock.Destroy;
begin
  FForm.Free;
  FParams.Free;
  inherited;
end;

type
  TADD    = array of Real;
  TAADD   = array of TADD;
  TACD    = array of Char;
  TABD    = array of Boolean;
  TAID    = array of Byte;

procedure TRosenbrock.Execute;

var I, J                  : Longword;
    F, FO, FBest, TM      : Real;
    FTol_FO_ABS           : Real;
    B, V, VV              : TAADD;
    Eint, D, AL, H, BX    : TADD;
    FTL_ABS               : TADD;
    PriVez                : TABD;
    Vale                  : TACD;
    Marca                 : TAID;
    UmaSimulacao          : Boolean;
    Melhorou              : Boolean;
    F_Igual_FO            : Boolean;
    Par                   : TrbParameter;

    procedure Inicializacoes;
    var K, I: Integer;
    begin
      FTerminated := False;

      // Dimensiona dinamicamente os Arrays
      I := FParams.Count;

      SetLength(Eint,    I);
      SetLength(D,       I);
      SetLength(AL,      I);
      SetLength(H,       I);
      SetLength(BX,      I);
      SetLength(FTL_ABS, I);
      SetLength(PriVez,  I);
      SetLength(Vale,    I);
      SetLength(Marca,   I);

      SetLength(B,  I, I);
      SetLength(V,  I, I);
      SetLength(VV, I, I);

      KOUNT :=  0;
      NumEstagio := 0;
      UmaSimulacao := False;
      FPassoInicial := True;

      // AVALIACAO INICIAL DA FUNCAO-OBJETIVO
      F := FGeralFunction(FParams);

      FO := F;
      FBEST := FO;

      // ESTABELECE TOLERANCIA DE AJUSTE DA FUNCAO-OBJETIVO
      FTol_FO_ABS  :=  FTOL * F;

      // ESTABELECE ZONA FRONTEIRICA DA REGIAO VIAVEL E COMPUTA PRECISAO PARA PARAMETROS
      for K := 0 to FParams.Count-1 do
        begin
        Par := FParams[k];
        AL[K] := (Par.HiLimit - Par.LowLimit) * Par.Tolerance;   //??? tratar zeros
        FTL_ABS[K] := Par.Tolerance * ABS(Par.Step);
        end;

      // INICIALIZA COMO INDENTIDADE MATRIZ V DOS VETORES UNIDIRECIONAIS
      for I := 0 to FParams.Count-1 do
        for K := 0 to FParams.Count-1 do
          if I = K then V[I,K] := 1 else V[I,K] := 0;

      // GUARDA VALORES INICIAIS DO PASSO DE VARIACAO DE CADA VARIAVEL
      for K := 0 to FParams.Count-1 do EINT[K] := FParams[k].Step;

      // Obtem o tempo inicial
      TM := GetTickCount;
    end;

    procedure Finalizacoes;
    begin
      if @FTermProc <> nil then FTermProc;
    end;

    function ParametrosViaveis: Boolean;
    var K: Integer;
    begin
      Result := True;
      for K := 0 to FParams.Count-1 do
        begin
        Par := FParams[k];
        if (Par.Value < Par.LowLimit) or (Par.Value > Par.HiLimit) then
           begin
           Result := False;
           Break;
           end;
        end;
    end;

    procedure RotacionaEixo;
    var K, KK, R, C  : Integer;
        SUMVM, SUMAV : Real;
        FNV          : Integer;
    begin
      FNV := FParams.Count;

      for R := 0 to FNV-1 do
        for C := 0 to FNV-1 do
           VV[C,R] := 0.0;

      for R := 0 to FNV-1 do
        for C := 0 to FNV-1 do
          begin
          for K := R to FNV-1 do VV[R,C] := D[K] * V[K,C] + VV[R,C];
          B[R,C] := VV[R,C];
          end;

      BX[0] := 0.0;
      for C := 0 to FNV-1 do
        BX[0] := BX[0] + SQR(B[0,C]);

      BX[0] := SQRT(BX[0]);
      IF BX[0] = 0 then BX[0] := 0.000001;

      for C := 0 to FNV-1 do
        V[0,C] := B[0,C] / BX[0];

      for R := 1 to FNV-1 do  // Aqui começa em 1 mesmo
        for C := 0 to FNV-1 do
          begin
          SUMVM := 0.0;
          for K := 0 to R-2 do  // vai até o penultimo
            begin
            SUMAV := 0.0;
            for KK := 0 to FNV-1 do SUMAV := SUMAV + VV[R,KK]* V[K,KK];
            SUMVM := SUMAV*V[K,C] + SUMVM;
            end;
          B[R,C] := VV[R,C] - SUMVM;
          end;

      for R := 1 to FNV-1 do  // Aqui começa em 1
        begin
        BX[R] :=  0.0;
        for K := 0 to FNV-1 do BX[R] := BX[R] + B[R,K] * B[R,K];
        BX[R] := SQRT(BX[R]);
        IF BX[R] = 0.0 then BX[R] := 0.0000001;
        for C := 0 to FNV-1 do V[R,C] := B[R,C] / BX[R];
        end;

      // FINAL DA ROTACAO - INICIA ESTAGIO SEGUINTE DE COMPUTACOES
      inc(NumEstagio);
      FBEST := FO;
    end;

    function TodosParametrosSaoVale: Boolean;
    var K: Integer;
    begin
      Result := True;
      for K := 0 to FParams.Count-1 do
        if Vale[K] <> 'V' then
           begin
           Result := False;
           Break;
           end;
    end;

    procedure Calcula_Passo_Evitando_Inviabilidade;
    var xx: Real;
        K : Integer;
    begin
      for K := 0 to FParams.Count-1 do
        begin
        Par := FParams[K];

        xx := FParams[I].Step * V[I,K];
        if (xx < 0.0) and (xx < (Par.LowLimit - Par.Value)) then
           FParams[I].Step := (Par.LowLimit - Par.Value) / V[I,K];

        xx := FParams[I].Step * V[I,K];
        if (xx > 0.0) and (xx > (Par.HiLimit - Par.Value)) then
           FParams[I].Step := (Par.HiLimit - Par.Value) / V[I,K];
        end;
    end;

Type
  TipoFalha = (tfIneficiencia, tfInviabilidade);

    // Penaliza Função Objetivo
    function Calcula_Fp: Real;
    var BW, PW: Real;
    begin
      Par := FParams[J];

      BW := AL[J];
      if BW = 0 then BW := 10E-10; {Rochedo}
      if (Par.Value < (Par.LowLimit+BW)) or ((Par.HiLimit-BW) < Par.Value) then
         PW := (Par.LowLimit + BW - Par.Value) / BW // PARAMETROS NA REGIAO LIMITROFE INFERIOR
      else
         PW := (Par.Value - Par.HiLimit + BW) / BW; // PARAMETROS NA REGIAO LIMITROFE SUPERIOR

      BW := PW * PW; // Desta maneira reduzo a equação em uma multiplicação
      BW := 3.0 * PW - 4.0 * BW + 2.0 * BW * PW;

      // PENALIZA VALOR DA F-O QDO PARAMETROS SE ACHAM NA ZONA FRONTEIRICA
      Result := F + (H[J] - F) * BW;
    end;

    procedure IniciarNovoEstagio;
    var K: Integer;
    begin
      for K := 0 to FParams.Count-1 do
        begin
        // REINICIALIZA PASSO INICIAL
        if FPassoInicial then FParams[K].Step := EINT[K];

        // INICIALIZA D[k] : ALTERACAO TOTAL DA VARIAVEL NO ESTAGIO CORRENTE
        D[K]      :=  0.0;
        VALE[K]   :=  ' ';
        PriVez[K] :=  True;
        end;
    end;

    Procedure IniciarCicloDeParametros;
    var K: Integer;
    begin
      I     := 0; // INICIO DE CICLO DE VARIACAO DE PARAMETROS

      for K := 0 to FParams.Count-1 do
        begin
        Par := FParams[K];
        MARCA[K] := 1;

        // EVITA INSENSIBILIDADE DO PARAMETRO NAO ACEITANDO PASSO NULO
        if Par.Step = 0.0 then Par.Step := FTL_ABS[K];

        // SE VALOR INICIAL É MÁXIMO FAZ PASSO NEGATIVO
        if Abs(Par.Value - Par.HiLimit) < 10E-15 then
           Par.Step := - Par.Step;
        end;
    end;

    Function ParametrosAtingiramZonaDePrecisao: Boolean;
    var K: Integer;
    begin
      Result := True;
      for K := 0 to FParams.Count-1 do
        if ABS(FParams[K].Step) >= FTL_ABS[K] then
           Begin
           Result := False; // Continua
           Break;
           end;
    end;

    procedure IncremantaParametros;
    var K: Integer;
    begin
      // VERIFICA SE CONDICAO DE VALE FOI OBTIDA APROXIMADAMENTE
      MARCA[I] := 2;

      for K := 0 to FParams.Count-1 do
        begin
        // ALTERACAO UNIVARIACIONAL DOS PARAMETROS
        FParams[K].Value := FParams[K].Value + FParams[I].Step * V[I,K];
        //J := MARCA[K]; ????

        // GUARDA MELHOR VALOR DA F-O  NO ESTAGIO ATUAL
        H[K] := FO;
        end;
    end;

    function FimDoCiclo: Boolean;
    begin
      if TodosParametrosSaoVale then
         // VALE ENCONTRADO : FINAL DAS COMPUTACOES DESTE ESTAGIO
         // VERIFICA INICIALMENTE SE OTIMO FOI ATINGIDO
         if ABS(FBEST - FO) < FTol_FO_ABS then
            Result := True
         else
            begin
            RotacionaEixo;
            IniciarNovoEstagio;
            IniciarCicloDeParametros;
            Result := ParametrosAtingiramZonaDePrecisao;
            IncremantaParametros;
            end
      else // VERIFICA SE TODOS OS PARÂMETROS FORAM MODIFICADOS
         if I = FParams.Count-1 then
            begin
            IniciarCicloDeParametros;
            Result := ParametrosAtingiramZonaDePrecisao;
            IncremantaParametros;
            end
         else
            begin
            inc(I);
            IncremantaParametros;
            Result := False;
            end
    end; //FimDoCiclo

    function TerminaPorFalha(Tipo: TipoFalha): Boolean;
    var K : Integer;
    begin
      if UmaSimulacao then
         begin
         for K := 0 to FParams.Count-1 do
           FParams[K].Value := FParams[K].Value - FParams[I].Step * V[I,K];

         // SOLUCAO PIOROU : MUDA SENTIDO DA VARIACAO E CONTRAI O PASSO PELA METADE
         FParams[I].Step := - FDecPasso * FParams[I].Step;

         Calcula_Passo_Evitando_Inviabilidade;
         if Tipo = tfIneficiencia then
            if Vale[I] <> ' ' then Vale[I] := 'V';

         Result := FimDoCiclo;
         end
      else
         Result := True;
    end;

    function CanTerminate: Boolean;
    begin
      Result := (FTerminated or
                (KOUNT > FMS) or
                ((GetTickCount - TM) > FMTS));
    end;


begin // Inicio de Rosenbrock.Execute
  Inicializacoes;
  IniciarNovoEstagio;
  IniciarCicloDeParametros;

  if Not ParametrosAtingiramZonaDePrecisao then
     begin
     IncremantaParametros;
     Repeat
       if ParametrosViaveis then
          begin
          UmaSimulacao := True;
          inc(KOUNT);

          F := FGeralFunction(FParams);

          Self.FO := F;

          DoVisualUpdate;
          ProcessMessages;

          F_Igual_FO := (Abs(F - FO) < FTol_FO_ABS);
          if (F > FO) or (F_Igual_FO and not PriVez[I]) then
             if TerminaPorFalha(tfIneficiencia) then Break else Continue
          else
             if F_Igual_FO then PriVez[I] := False;

          // Verifica Zona Fronteirica e Melhoramento na FO
          J := 0;
          Melhorou := True;
          repeat
            Par := FParams[J];
            if (Par.Value < (Par.LowLimit + AL[J])) or
               (Par.Value > (Par.HiLimit - AL[J])) then
               begin
               F := Calcula_Fp;
               if F >= FO then
                  begin
                  Melhorou := False; {PIOROU !!}
                  Break;
                  end;
               end
            else
               H[J] :=  F; // Armazena novo valor melhor que anterior

            inc(J);
          until J = FParams.Count;

          if Melhorou then
             begin
             Par := FParams[I];
             D[I] := D[I] + Par.Step;
             Par.Step := FIncPasso * Par.Step;

             Calcula_Passo_Evitando_Inviabilidade;

             // MELHOR RESULTADO É REGISTRADO EM FO, SENDO FO GUARDADO ANTES
             // PARA TESTAR SE VALE FOI ENCONTRADO NA APROXIMACAO
             FO := F;
             if VALE[I] <> 'V' then VALE[I] := 'M';
             if FimDoCiclo then Break;
             end
          else
             if TerminaPorFalha(tfIneficiencia) then Break;
          end
       else
          if TerminaPorFalha(tfInviabilidade) then Break;

       until CanTerminate;
     end; // if Not ParametrosAtingiramZonaDePrecisao ...

  Finalizacoes;
end;

procedure TRosenbrock.ProcessMessages;
var MSG: TMSG;
begin
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
     if Msg.Message <> WM_QUIT then
        begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
        end;
end;

procedure TRosenbrock.DoVisualUpdate;
begin
  if (FForm <> nil) then FForm.FO := FO;

  if @FVisualProc <> nil then
     FVisualProc(
       FParams,          // Parâmetros
       FO,               // Valor da função objetivo recentemente calculado
       KOUNT,            // n Simulações ocorridas
       NumEstagio);      // n Estágios ocorridos
end;

{------------------------------------------------------}

(*
*****************************
     SUBROUTINE ROSEN(M,X,XMIN,XMAX,E,ETOL,F,PR,NN,PD,ETPM,QO,QSAL,
     1 XO,XOMIN,XOMAX,RSP,RSS,RSB,LS,LB,LOOPY,NSTEP,FTP,QREF1,QREF2,
     2FTOL,IQS,APAR,AREA,BI,BS,ITCP,IANO,MES)

C       METODO DE OTIMIZACAO BLOQUEADA DE ROSENBROCK

C       DICIONARIO BASICO

C       FBEST - MELHOR VALOR DA F-O ATE ULTIMA ROTACAO, OU ESTAGIO PREVIO
C       FO    - MELHOR VALOR DA F-O APOS ULTIMA ROTACAO, OU NESTE ESTAGIO
C       F     - VALOR CORRENTE DA F-O, OBTIDO NA ULTIMA SIMULACAO

	INTEGER PR,R,C,FTP
	REAL LC
	CHARACTER VALE(14),SETA(2),VAL,MEL
	CHARACTER*4 APAR(14)
	DIMENSION X(14),E(14),ETOL(14),XMIN(14),XMAX(14),V(14,14),D(14),
     1 H(14),B(14,14),BX(14),VV(14,14),EINT(14),AL(14),QSAL(5000)
	DIMENSION PD(20000),ETPM(5000),QO(5000),XO(14),XOMIN(14),
     1 XOMAX(14)
	DIMENSION MARCA(14),IND(14)
	DATA SETA/' ','<'/,VAL/'V'/,MEL/'M'/

	WRITE(15,551) M,NSTEP,LOOPY,FTOL

C     INICIALIZA PARAMETROS E VARIAVEIS

	IPRINT = PR
	LOOP = 1
	INIT = 0
	KOUNT = 0
	TERM = 0constructor TRosenbrock.Create;
begin

end;

.0

C     AVALIACAO INICIAL DA FUNCAO-OBJETIVO

	WRITE(.*,550)
550     FORMAT(' AVALIACAO PRELIMINAR DO AJUSTE'/)
	CALL FUNCAO(NN,PD,ETPM,QO,X,F,IPRINT,XO,XOMIN,XOMAX,RSP,RSS,RSB,LS
     1,LB,FTP,QREF1,QREF2,QSAL,IQS,APAR,AREA,BI,BS,ITCP,IANO,MES)
	FO = F

C     ESTABELE TOLERANCIA DE AJUSTE DA FUNCAO-OBJETIVO

	FTOL = FTOL * F

C     ESTABELECE ZONA FRONTEIRICA DA REGIAO VIAVEL E COMPUTA PRECISAO
C     PARA PARAMETROS

	DO 565 K = 1,M
		AL(K) = (XMAX(K)-XMIN(K)) * ETOL(K)
		ETOL(K) = ETOL(K) * ABS(E(K))

565     CONTINUE

C       INICIALIZA COMO INDENTIDADE MATRIZ V DOS VETORES UNIDIRECIONAIS

	DO 572 I = 1,M
	DO 572 J = 1,M
		V(I,J) = 0.0
		IF(I-J) 572,571,572
571             V(I,J) = 1.0
572     CONTINUE

C       GUARDA VALORES INICIAIS DO PASSO DE VARIACAO DE CADA VARIAVEL

	DO 576 KK=1,M
		EINT(KK) = E(KK)
576     CONTINUE

C       INICIO DA PARTE ITERATIVA :
C       ATUALIZA PASSO DE VARIACAO NO INICIO DE NOVO ESTAGIO E ZERA
C       INDICADOR DE EXISTENCIA DE VALE

578     DO 583 J = 1,M

		IF(NSTEP . EQ . 0) E(J) = EINT(J)

C       INICIALIZA D : ALTERACAO TOTAL DA VARIAVEL NO ESTADO CORRENTE

		D(J) = 0.0
		VALE(J) = SETA(1)
		IND(J) = 0

583     CONTINUE

C       ATUALIZA MELHOR VALOR DA F.O. ATE ESTAGIO ANTERIOR

	FBEST = FO
586     WRITE(.*,7)
7       FORMAT(1X,30(1H-)//' INICIO DE CICLO DE VARIACAO DE PARAMETROS')
	I = 1

C       VERIFICA SE OTIMO FOI ATINGIDO VIA PASSO DE VARIACAO

	KETOL = 0

	DO 588 K = 1,M
	MARCA(K) = 1

C       EVITA INSENSIBILIDADE DO PARAMETRO NAO ACEITANDO PASSO NULO

	IF(E(K).EQ.0.0) E(K) = ETOL(K)
	IF(X(K).EQ.XMAX(K)) E(K) = - E(K)

	IF( ABS( E(K) ) . LT . ETOL(K)) GOTO 588
	KETOL = 1
588     CONTINUE
	IF(KETOL . NE . 0) GOTO 589

	WRITE(15,755) KOUNT
	WRITE(.*,755) KOUNT
	TERM = 1
	GOTO 726

C       VERIFICA SE CONDICAO DE VALE FOI OBTIDA APROXIMADAMENTE

589     KONT = KOUNT + 1
	WRITE(.*,11) KONT,LOOP
11      FORMAT(/' VALORES PARAMETROS NA SIMULACAO',I5,' E ESTAGIO ',
     1  I2,/' PARAMETRO',3X,'MINIMO',5X,'VALOR',6X,'MAXIMO',6X,'PASSO',
     2  4X,'PRECISAO')
	MARCA(I) = 2
	DO 590 K = 1,M

C       ALTERACAO UNIVARIACIONAL DOS PARAMETROS

		X(K) = X(K) + E(I) * V(I,K)
		J = MARCA(K)
		WRITE(.*,12) K,XMIN(K),X(K),XMAX(K),E(K),ETOL(K),
     1   VALE(K),SETA(J)
12              FORMAT(4X,I2,5X,5(G10.4,1X),3X,A1,2X,A1)

C       GUARDA MELHOR VALOR DA F-O  NO ESTAGIO ATUAL

		H(K) = FO

590     CONTINUE

C       SE VALOR DO PARAMETRO FOR INVIAVEL, NAO EXECUTAR A SIMULACAO

	DO 594 K = 1,M
	IF(X(K) . GE . XMIN(K) . AND . X(K) . LE . XMAX(K)) GOTO 594
	WRITE(.*,9) K
9               FORMAT(' FALHA : PARAMETRO ', I2,' INVIAVEL')
	GOTO 714 
594     CONTINUE

	INIT = 1        
	KOUNT = KOUNT + 1
	IF(IPRINT . LT . 0) GOTO 596
	IF(MOD(KOUNT,PR) . EQ . 0 ) IPRINT = 0

C       AVALIACAO DO AJUSTE

596   CALL FUNCAO(NN,PD,ETPM,QO,X,F,IPRINT,XO,XOMIN,XOMAX,RSP,RSS,RSB,LS
     1,LB,FTP,QREF1,QREF2,QSAL,IQS,APAR,AREA,BI,BS,ITCP,IANO,MES)

	WRITE(.*,1) FBEST,FO,F
1     FORMAT(/' VALORES DA F-O : MELHOR  GLOBAL - ',1PG10.4/
     1'                  MELHOR ESTAGIO - ',1PG10.4/
     2'                           ATUAL - ',1PG10.4) 


C     TESTA LIMITE DE SIMULACOES

	IF(KOUNT . EQ . LOOPY) GOTO 740

	IPRINT = 1.

C     VERIFICA SE A SOLUCAO E' MELHOR QUE A MELHOR OBTIDA ATE
C     AGORA NESSE ESTAGIO (OU ASCENSAO DA F-O).

	IF(F . LT . FO) GO TO 610
	IF(F.GT.FO . OR . IND(I).EQ.1) GOTO 597
	IND(I) = 1
	GOTO 610
597   WRITE(.*,5)
5     FORMAT(' FALHA : PIOROU VALOR DA F-O')
	GOTO 714

C     VERIFICA SE SOLUCAO ESTA NA ZONA FRONTEIRICA, PENALIZANDO F-O
C     CASO ISSO OCORRA

610   J = 1
611   XC = X(J)
	LC = XMIN(J)
	UC = XMAX(J)
	IF(XC . LT . (LC+AL(J)) . OR . XC . GT . (UC-AL(J)) ) GOTO 627

C     GUARDA VALOR F.O. COM PARAMETRO X(J) FORA DA ZONA FRONTEIRICA

	H(J) = F
	GO TO 654

C     PARAMETROS ACHAM-SE NA ZONA FRONTEIRICA

627   BW = AL(J)
	IF(XC.LT.(LC+BW))GO TO 638
	IF((UC-BW).LT.XC) GO TO 641

C       PARAMETROS NA REGIAO LIMITROFE INFERIOR

638   PW = (LC + BW - XC) / BW
	GO TO 642

C     PARAMETROS NA REGIAO LIMITROFE SUPERIOR

641   PW = (XC - UC + BW) / BW

642   PH = 3.0 * PW - 4.0 * PW * PW + 2.0 * PW * PW * PW

C     PENALIZA VALOR DA F-O QDO PARAMETROS SE ACHAM NA ZONA FRONTEIRICA

645   F = F + (H(J) - F) * PH

C     VERIFICA SE NAO HOUVE PIORA NO VALOR CORRIGIDO DA F-O, FACE AOS
C     RESULTADOS OBTIDOS ATE AGORA NESSE ESTAGIO

	IF( F < FO ) GOTO 650 {Melhorou} else {Piorou}

C     PIOROU !

	WRITE(.*,10) J,F
10    FORMAT(' PARAMETRO X(',I1,') NA ZONA FRONTEIRICA.'/
     1       ' CORRECAO DA F-O,',1PG10.4,' PIOROU RESULTADOS') 

	GOTO 714

650   WRITE(.*,2) J,F
2     FORMAT(' PARAMETRO X(',I1,') NA ZONA FRONTEIRICA; F CORRIGIDO :
     1',1PG10.4)

C     PROSSEGUE PESQUISANDO OUTRO PARAMETRO OU ENCERRANDO ESSA AVALIACAO

654   IF(J . EQ . M) GO TO 661
	J = J + 1
	GO TO 611

C     FINAL DA AVALIACAO FRONTEIRICA
      
C     OCORRE MELHORIA NA F-O : GUARDA ALTERACAO TOTAL DA VARIAVEL (D) E
C     ACELERA O PASSO POR 3

661   D(I) = D(I) + E(I)
	E(I) = 3.0 * E(I)

C     TESTA VALOR DO PASSO EVITANDO INVIABILIDADE

	DO 663 K = 1 , M
	IF(E(I)*V(I,K) .LT. 0.0 .AND. E(I)*V(I,K) .LT. (XMIN(K) - X(K))) 
     1  E(I) = (XMIN(K) - X(K)) / V(I,K)
	IF(E(I)*V(I,K) .GT. 0.0 .AND. E(I)*V(I,K) .GT. (XMAX(K) - X(K))) 
     1  E(I) = (XMAX(K) - X(K)) / V(I,K) 
663     CONTINUE

	WRITE(.*,3)
3     FORMAT(' SUCESSO : MELHOROU VALOR DA F-O')

C     MELHOR RESULTADO E' REGISTRADO EM FO, SENDO FO GUARDADO ANTES
C     PARA TESTAR SE VALE FOI ENCONTRADO NA APROXIMACAO

	FO = F

670   IF(VALE(I).NE.VAL) VALE(I) = MEL

C     VERIFICA SE VALE FOI ENCONTRADO ATRAVES DOS VALORES DE SA

671   DO 673 JJ = 1,M
	IF(VALE(JJ) . NE . VAL) GO TO 722
673   CONTINUE

C     VALE ENCONTRADO : FINAL DAS COMPUTACOES DESTE ESTAGIO
C     VERIFICA INICIALMENTE SE OTIMO FOI ATINGIDO

	IF(ABS(FBEST - F) - FTOL) 675,675,676

C     SOLUCAO E' OTIMA - ENCERRA 

675   WRITE(15,750) KOUNT
	WRITE(.*,750) KOUNT
	TERM = 1
	GOTO 726

C     SOLUCAO SUB-OTIMA : ROTACAO DOS EIXOS

676   WRITE(.*,4)
4     FORMAT(1X,30(1H*.)/' VALE ENCONTRADO - ROTACAO E NOVO ESTAGIO')
	DO 677 R=1,M
	DO 677 C=1,M
	VV(C,R) = 0.0
677   CONTINUE

	DO 683 R=1,M
	KR = R
	DO 683 C = 1,M
	DO 682 K = KR,M
	VV(R,C) = D(K) * V(K,C) + VV(R,C)
682   CONTINUE  

	B(R,C) = VV(R,C)
683   CONTINUE

	BX(1) = 0.0

	DO 687 C=1,M
	BX(1) = BX(1) + B(1,C) * B(1,C)
687   CONTINUE
	
	BX(1) = SQRT(BX(1))
	IF(BX(1).EQ. 0.) BX(1) = 0.000001

	DO 691 C = 1,M
	V(1,C) = B(1,C) / BX(1)
691   CONTINUE

	DO 701 R=2,M
	IR = R-1
	DO 701 C=1,M
	SUMVM = 0.0
	DO 700 KK = 1,IR
	SUMAV = 0.0
	DO 699 KJ = 1,M
	SUMAV = SUMAV + VV(R,KJ)* V(KK,KJ)
699   CONTINUE
	SUMVM = SUMAV*V(KK,C) + SUMVM
700   CONTINUE
	B(R,C) = VV(R,C) - SUMVM
701   CONTINUE

	DO 708 R=2,M
	BX(R) = 0.0
	DO 705 K = 1,M
	BX(R) = BX(R) + B(R,K) * B(R,K)
705   CONTINUE
	BX(R) = SQRT(BX(R))
	IF(BX(R).EQ.0.0) BX(R) = 0.0000001

	DO 708 C = 1,M
	V(R,C) = B(R,C) / BX(R)
708   CONTINUE

C     FINAL DA ROTACAO - INICIA ESTAGIO SEGUINTE DE COMPUTACOES
	
	LOOP = LOOP + 1
	GO TO 726

C     SOLUCAO E' INVIAVEL OU PIOR : 1) RETORNA AO VALOR PREVIO DA
C     VARIAVEL ALTERADA; 2) MODIFICA O SENTIDO DA ALTERACAO;

714   IF(INIT . EQ . 0) GO TO 726
	DO 716 IX = 1,M
	X(IX) = X(IX) - E(I) * V(I,IX)
716   CONTINUE

C     SOLUCAO PIOROU : MUDA SENTIDO DE VARIACAO E CONTRAI 
C     O PASSO 'A METADE
      	
	E(I) =  - 0.5 * E(I)

C     TESTA VALOR DO PASSO EVITANDO INVIABILIDADE

	DO 720 K = 1, M

	IF(E(I)*V(I,K).LT. 0.0  . AND . E(I)*V(I,K) .LT. (XMIN(K)-X(K))) 
     1  E(I) = (XMIN(K) - X(K)) / V(I,K)
	IF(E(I)*V(I,K).GT. 0.0  . AND . E(I)*V(I,K) .GT. (XMAX(K)-X(K))) 
     2  E(I) = (XMAX(K) -X(K)) / V(I,K)

720     CONTINUE
 
C       ALTERA INDICADOR DE VALE

	IF(VALE(I) . EQ . SETA(1)) GOTO 671
	VALE(I) = VAL
	GO TO 671

C       VERIFICA SE TODOS PARAMETROS FORAM MODIFICADOS

722     IF(I . EQ . M) GO TO 586
	I = I + 1
	GO TO 589

726     WRITE(15,727)
	WRITE(15,730) LOOP,FO
	WRITE(15,732) KOUNT
	WRITE(15,734)
	WRITE(15,736) (JM,X(JM),JM=1,M)

735     IF(INIT.EQ.0.) GO TO 742
	IF(TERM.EQ.1.)  GO TO 745
	GO TO 578

740     WRITE(15,6)LOOPY
	WRITE(.*,6) LOOPY

C       RECUPERA VALORES PREVIOS DOS PARAMETROS

	DO 741 K = 1,M
	X(K) = X(K) - E(I) * V(I,K)
741     CONTINUE

	GO TO 745

742     WRITE (15,743)
745     WRITE(15,746)

	DO 748 J=1,M
		WRITE(15,749) (J,I,V(J,I), I = 1,M)
748     CONTINUE

	WRITE(15,751)
	WRITE(15,753) (J,E(J),J = 1,M)

C       AVALIACAO FINAL

	CALL FUNCAO(NN,PD,ETPM,QO,X,F,0,XO,XOMIN,XOMAX,RSP,RSS,RSB,LS,LB,
     1 FTP,QREF1,QREF2,QSAL,IQS,APAR,AREA,BI,BS,ITCP,IANO,MES)

	RETURN


C     FORMATS

551   FORMAT(///1X,90(1H=)//' MODHAC : CALIBRACAO AUTOMATICA PELO ME',
     1 'TODO DE OTIMIZACAO BLOQUEADA DE ROSENBROCK'//
     2 ' NUMERO DE PARAMETROS        = ',I4,
     25X,'OPCAO PASSO INICIAL         = ',I1/
     3 ' MAXIMO NUMERO DE INTERACOES = ',I4,
     45X,'PRECISAO FUNCAO OBJETIVO    = ',G15.8//1X,90(1H=)/)
603   FORMAT(//' ULTIMO VALOR DA FUNCAO OBJETIVO = ',1PG14.6//)
727   FORMAT(//' ESTAGIO',8X,'FUNCAO')
730   FORMAT(1X,I5,6X,1PG14.6)
732   FORMAT(//' NUMERO DE SIMULACOES = ',I5)
734   FORMAT(/' VALORES DOS PARAMETROS NESTE ESTAGIO')
736   FORMAT(5(' X(',I1,') = ',1PG14.6,2X))
6     FORMAT(1X,' NUMERO DE INTERACOES EXCEDEU O LIMITE DE',1X,I3,
     1 1X,'INTERACOES')
743   FORMAT(///' VALORES INICIAIS DOS PARAMETROS VIOLAM BLOQUEIO')
746   FORMAT(///' MATRIZ DOS VETORES DIRECIONAIS FINAIS')
749   FORMAT(/1X,5('V(',I1,',',I1,') = ' ,G14.6,2X))
750   FORMAT(//5X,'OTIMO ATINGIDO NA SIMULACAO ',I5,', PELA PRECISAO'
     1 ,' DA FUNCAO OBJETIVO'/)
751   FORMAT(//' DIMENSAO DOS PASSOS FINAIS')
753   FORMAT(/1X,5('E(',I1,') = ',1PG14.6,4X))
755   FORMAT(//5X,'OTIMO ATINGIDO NA SIMULACAO ',I5,', PELA PRECISAO'
     1,' DOS PARAMETROS'/)

	END
*****************************
*)

procedure TRosenbrock.SetDecPasso(const Value: Real);
begin
  if (Value < 0) or (Value > 1) then
     FDecPasso := 0.5
  else
     FDecPasso := Value;
end;

procedure TRosenbrock.SetIncPasso(const Value: Real);
begin
  if (Value < 0) then
     FIncPasso := 3.0
  else
     FIncPasso := Value;
end;

procedure TRosenbrock.SetTOL(const Value: Real);
begin
  if (Value < 0) or (Value > 1) then
     FTOL := 0.001
  else
     FTOL := Value;
end;

procedure TRosenbrock.Stop;
begin
  FTerminated := True;
end;

procedure TRosenbrock.ShowFO;
begin
  if FForm = nil then
     begin
     FForm := TrbForm.Create(self);
     FForm.Show;
     FForm.FO := FO;
     end
  else
     FForm.Show;
end;

{ TrbParameter }

constructor TrbParameter.Create(const aName: String; const aValue, aLowLimit,
                                      aHiLimit, aStep, aTolerance: Real);
begin
  inherited Create;

  FName         := aName;
  FValue        := aValue;
  FLowLimit     := aLowLimit;
  FHiLimit      := aHiLimit;
  FStep         := aStep;
  FTolerance    := aTolerance;
end;

constructor TrbParameter.Create;
begin
  inherited Create;
end;

destructor TrbParameter.Destroy;
begin
  FForm.Free;
  Inherited;
end;

procedure TrbParameter.SetHiLimit(const Value: Real);
begin
  FHiLimit := Value;
  if (FForm <> nil) and (FForm.Visible) then
     FForm.HiLimit := Value;
end;

procedure TrbParameter.SetLowLimit(const Value: Real);
begin
  FLowLimit := Value;
  if (FForm <> nil) and (FForm.Visible) then
     FForm.LowLimit := Value;
end;

procedure TrbParameter.SetName(const Value: String);
begin
  FName := Value;
  if (FForm <> nil) and (FForm.Visible) then
     FForm.ParName := Value;
end;

procedure TrbParameter.SetStep(const Value: Real);
begin
  FStep := Value;
  if (FForm <> nil) and (FForm.Visible) then
     FForm.Step := Value;
end;

procedure TrbParameter.SetTolerance(const Value: Real);
begin
  FTolerance := Value;
  if (FForm <> nil) and (FForm.Visible) then
     FForm.Tolerance := Value;
end;

procedure TrbParameter.SetValue(const Value: Real);
begin
  FValue := Value;
  if (FForm <> nil) and (FForm.Visible) then
     FForm.Value := Value;
end;

procedure TrbParameter.Show(Left, Top: Integer);
begin
  if FForm = nil then
     FForm := TrbFormParameter.Create(Self);

  FForm.Left := Left;
  FForm.Top  := Top;
  FForm.Show;
end;

{ TrbParameters }

constructor TrbParameters.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TrbParameters.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TrbParameters.Clear;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    TObject(FList[i]).Free;

  FList.Clear;
end;

function TrbParameters.Add(Param: TrbParameter): Integer;
begin
  FList.Add(Param);
end;

function TrbParameters.getCount: Integer;
begin
  Result := FList.Count;
end;

function TrbParameters.GetPar(i: Integer): TrbParameter;
begin
  Result := TrbParameter(FList[i]);
end;

function TrbParameters.ParamByName(const Name: String): TrbParameter;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    begin
    Result := TrbParameter(FList[i]);
    if CompareText(Result.Name, Name) = 0 then Exit;
    end;

  Result := nil;
end;

end.
