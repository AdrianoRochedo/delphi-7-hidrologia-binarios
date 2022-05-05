{$X+,S+,N+,E+}
{ Atualizacao

   Amauri 21/05/99
     Melhoria dos algoritmos para obtencao das estimativas dos parametros
     Documentacao das rotinas
     Eliminacao de rotinas para as quais existem versoes melhores
     Rotinas para garficos de distribuicao
}

unit wsFuncoesDeProbabilidade;

interface
uses SysUtils,
     wsVec,
     wsMatrix,
     wsGLib,
     wsMath,
     wsConstTypes;

(*================ Vetterling et alli - Numerical Recipes in Pascal =============*)

procedure GSer(a, x: Double; var GamSer, gln: Double; var ErrCode : Word);
procedure GCF(a, x: Double; var GammCF, gln: Double; var ErrCode : Word);
function GammP(a, x: Double; var ErrCode : word): Double;
function GammQ(a, x: Double; var ErrCode : word): Double;
// probabilidade para variável Poisson
function CumPoisson(k, x: Double; var ErrCode : word): Double;
// probabilidade qui-quadrado
function QuiSqr(ni, x: Double; var ErrCode : word): Double;
function BetaCF(a, b, x: Double; var ErrCode : word): Double;
// probabilidade beta incompleta
function BetaI(a, b, x: Double; var ErrCode : word): Double;
// probabilidade t student
function TProb(ni, x: Double; var ErrCode : word): Double;
// probabilidade F
function FProb(ni1, ni2, x: Double; var ErrCode : word): Double;
// probabilidadebinomial
function BinProb(n,x: Integer; p: Double; var ErrCode: word; Upper: boolean=True): Double;
// Complemento da funcao erro
function ErfComp(const x: Double): Double;
(*================= Mardia & Zemroch - Tables of F distribution ...==============*)

function g(x: Double): Double;
function Sig(x: Double): Integer;
function fn(x: Double; ax: FArray; upper: Boolean): Double;
function fx21(x: Double; ax: FArray; upper: Boolean): Double;
function fx22(x: Double; ax: FArray; upper: Boolean): Double;
function fb1(x: Double; ax: FArray; upper: Boolean): Double;
function fb2(x: Double; ax: FArray; upper: Boolean): Double;
procedure ZBrak(fx: HFunc; ax: FArray; upper: Boolean; x1, x2: Double;
  n: Integer; var nb: Integer;xb1,xb2: PFArray);
function ZBrent(f: HFunc; xx: FArray; upper: Boolean; x1, x2, Tol: Double;
  var Alarm: Integer): Double;
// probabilidade normal padrão
function NInt(x: Double; upper: Boolean): Double;
// probabilidade qui-quadrado
function X2Int(x, v: Double; upper: Boolean; var ErrCode : word): Double;

function X2Aux(x, v, vfac: Double; upper: Boolean; var ErrCode : word): Double;
// probabilidade F
function FInt(x, v1, v2: Double; upper: Boolean; var ErrCode : word): Double;

function BAux(x, p, q, lnpf, lnqf, c: Double; upper: Boolean): Double;

function BInt(x, a, b: Double; upper: Boolean; var ErrCode : word): Double;
// probabilidade t student
function TInt(x, n: Double; OneSided, upper: Boolean; var ErrCode : word): Double;
function Hyp(f: HFunc; var xx: Double; ax: FArray; upper: Boolean;
             x1, x2, eps: Double; var Alarm: Integer): Double;
// inversa normal
function NInv(p, eps: Double; upper: Boolean; var ErrCode : word): Double;
// inversa qui-quadrado
function X2Inv(p, v, eps: Double; upper: Boolean; var ErrCode : word): Double;
// Outra rotina para inversa qui-quadrado
function ppchi2(const p,v,g: Double; var ifault: Word): Double;
// inversa beta
function BInv(p, a, b, eps: Double; upper: Boolean; var ErrCode : word): Double;
// inversa F
function FInv(p, v1, v2, eps: Double; upper: Boolean; var ErrCode : word): Double;
// inversa t student
function TInv(p, n, eps: Double; upper, OneSided: Boolean; var ErrCode : word): Double;


(*=========================== Distribuição do Usuário ==========================*)

Function DistMeanUser(RowX,RowP: word; TabUsu: TWSGeneral; var Error: Word): Double;
Function DistVarUser(RowX, RowP: word; Mean: Double; TabUsu: TWSGeneral): Double;
Function DistSkewUser(RowX,RowP: word; Mean,Varc: Double; TabUsu: TWSGeneral): Double;
Function DistCurtUser(RowX, RowP: word; Mean,Varc:Double; TabUsu:TWSGeneral): Double;

(*============= Estimadores de Máxima Verossimilhança - Law e Kelton ==============*)
(*
procedure Estimadores_Bernoulli(var prob: Double; Sample: TWSVec);
procedure Estimadores_Beta(var alfa, beta: Double; Sample: TWSVec);
procedure Estimadores_Binomial(var nExp, prob: Double; Sample: TWSVec; tKnown: boolean);
procedure Estimadores_BinNeg(var NSuc, prob: Double; Sample: TWSVec; sKnown: boolean);
procedure Estimadores_Exponencial(var lambda: Double; Sample: TWSVec);
procedure Estimadores_Gamma(var alfa: Double; Sample: TWSVec);
procedure Estimadores_Geometrica(var prob: Double; Sample: TWSVec);
procedure Estimadores_LogNormal(var Mean, Varc: Double; Sample: TWSVec);
procedure Estimadores_Normal(var Mean, Varc: Double; Sample: TWSVec);
procedure Estimadores_Poisson(var Mean, Rate: Double; Sample: TWSVec);
procedure Estimadores_Uniforme(var limInf, LimSup: Double; Sample: TWSVec);
procedure Estimadores_UniDisc(var limInf, LimSup: Double; Sample: TWSVec);
procedure Estimadores_Weibull(var alfa, beta: Double; Sample: TWSVec);
procedure Calcula_Bk_Ck_Hk(previus: Double; var Bk, Ck, Hk: Double; Sample: TWSVec);
*)
implementation
uses Classes,
     Math,
     wsFuncoesDeEscalares;

(* ===== Funcoes retiradas de Numerical Recipes in Pascal ===== *)

procedure GSer(a, x: Double; var GamSer, gln: Double; var ErrCode: Word);
const
  eps = 3.0e-7;
  ItMax = 100;               { Numero maximo de iteracoes }
var
  n         : Integer;
  sum,del,ap: Double;
begin
  ErrCode := 0;
  gln := GammLn(a);
  if x <= 0 then
    begin
    if x < 0 then ErrCode := ParamIleg;
    GamSer := 0
    end
  else
    begin
    ap := a;
    sum := 1/a;
    del := sum;
    for n := 1 to itmax do
      begin
      ap := ap + 1;
      del := del*x/ap;
      sum := sum + del;
      if Abs(del) < Abs(sum)*eps then
        begin
        GamSer := sum*exp(-x + a*ln(x) - gln);
        Exit
        end
      end;
    ErrCode := NumItPeq;
    GamSer := sum*exp(-x + a*ln(x) - gln)
    end
end; (* GSer *)

procedure GCF(a, x: Double; var GammCF, gln: Double; var ErrCode: Word);
const
  eps = 3.0e-7;
  ItMax = 100;               { Numero maximo de iteracoes }
var
  n                : Integer;
  gold,g,fac,b1,b0,
  anf,ana,an,a1,a0 : Double;
begin
  ErrCode := 0;
  gln := GammLn(a);
  gold := 0;
  a0 := 1;
  a1 := x;
  b0 := 0;
  b1 := 1;
  fac := 1;
  for n := 1 to itmax do
    begin
    an := n;
    ana := an - a;
    a0 := (a1 + a0*ana)*fac;
    b0 := (b1 + b0*ana)*fac;
    anf := an*fac;
    a1 := x*a0 + anf*a1;
    b1 := x*b0 + anf*b1;
    if a1 <> 0 then
      begin
      fac := 1/a1;
      g := b1*fac;
      if Abs((g-gold)/g) < eps then
        begin
        GammCF := exp(-x + a*ln(x) - gln)*g;
        Exit
        end;
      gold := g;
      end;
    end;
  ErrCode := NumItPeq;
  GammCF := exp(-x + a*ln(x) - gln)*g
end;

function GammP(a, x: Double; var ErrCode: Word): Double;
var
  GamSer,GammCF,gln: Double;
begin
  ErrCode := 0;
  if (x < 0) or (a <= 0) then
    ErrCode := ParamIleg
  else
    if x < a+1 then
      begin
      GSer(a, x, GamSer, gln, Errcode);
      Result := GamSer
      end
    else
      begin
      GCF(a, x, GammCF, gln, ErrCode);
      Result := 1 - GammCF
      end
end; (* GammP *)

function GammQ(a, x: Double; var ErrCode: word): Double;
var
  GamSer, GammCF, gln: Double;
begin
  ErrCode:=0;
  if (x < 0) or (a <= 0) then
    ErrCode := ParamIleg
  else
    if x < a+1 then
      begin
      GSer(a, x, GamSer, gln, ErrCode);
      Result := 1 - GamSer
      end
    else
      begin
      GCF(a, x, GammCF, gln, Errcode);
      Result := GammCF;
      end
end; (* GammQ *)

// Complemento da funcao erro
function ErfComp(const x: Double): Double;
{ Objetivo
    Obtem o complemento da função erro com precisao maior que 1.2E-7. Algoritmo de Press et al,
    pag 185. Para obter valor da função erro basta tomar 1-ErfComp()
  Parâmetros
    x: Valor para determinação
}
var
  t,z: Double;
begin
  z:=Abs(x);
  t:=1/(1+0.5*z);
  Result:=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+t*(-0.18628806+
    t*(0.27886807+t*(-1.13520398+t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))));
  if x<0 then
    Result:=2-Result
end;

function CumPoisson(k, x: Double; var ErrCode : word): Double;
begin
  Result := GammQ(k, x, ErrCode)
end; (* CummPoisson *)

function QuiSqr(ni, x: Double; var Errcode : word): Double;
begin
  Result := GammQ(ni/2, x/2, ErrCode)
end;

function BetaCF(a, b, x: Double; var ErrCode : word): Double;
{ Objetivo
    Obtém fração continuada para função beta incompleta. Utilizada por BetaI
  Parâmetros
    a,b: parâmetrhos da distribuição beta
      x: valor para o cálculo da função
    ErrCode: retorna código de erro
      = 0: processamento normal
      = NumItPeq: não convergiu no número de iterações (100) especificado; a ou b
        muito grandes
  Referências
    Press et alli - Numerical Recipes in Pascal pág. 188
}
const
  eps  = 3.0e-7;
  ItMax= 100;               { Numero maximo de iteracoes }
var
  tem,qap,qam,qab,em,d,
  bz,bpp,bp,bm,az,app,
  am,aold,ap           : Double;
  m                    : Integer;
begin
  ErrCode:= 0;
  am := 1;
  bm := 1;
  az := 1;
  qab := a + b;
  qap := a + 1;
  qam := a - 1;
  bz := 1 - qab*x/qap;
  for m := 1 to itmax do
    begin
    em := m;
    tem := em + em;
    d := em*(b-m)*x/((qam+tem)*(a+tem));
    ap := az + d*am;
    bp := bz + d*bm;
    d := -(a+em)*(qab+em)*x/((a+tem)*(qap+tem));
    app := ap + d*az;
    bpp := bp + d*bz;
    aold := az;
    am := ap/bpp;
    bm := bp/bpp;
    az := app/bpp;
    bz := 1.0;
    if Abs(az-aold) < eps*Abs(az) then
      begin
      Result := az;
      Exit
      end
    end;
  ErrCode := NumItPeq;
end; (* BetaCF *)

function BetaI(a, b, x: Double; var ErrCode : word): Double;
{ Objetivo
    Retorna o valor da função beta incompleta
  Parâmetros
    a, b: parâmetros da distribuição beta
    ErrCode: retorna código de erro na execução
      ErrCode = 0 processamento normal
      ErrCode = ParamIleg - valores ilegais para x (x<0 ou x>1)
  Referências
    Press et alli - Numerical Recipes in Pascal pág. 188
}
var
  bt: Double;
begin
  ErrCode:=0;
  if (x < 0) or (x > 1) then
    ErrCode := ParamIleg
  else
    begin
    if (x = 0) or (x = 1) then
      bt := 0
    else
      bt := exp(GammLn(a+b)-GammLn(a)-GammLn(b)+a*ln(x)+b*ln(1-x));
    if x < (a+1)/(a+b+2) then
      Result := bt*BetaCF(a,b,x,ErrCode)/a
    else
      Result := 1-bt*BetaCF(b,a,1-x,Errcode)/b
    end
end; (* BetaI *)

function TProb(ni, x: Double; var ErrCode : Word): Double;
begin
  x := ni/(ni + x*x);
  Result := 1 - BetaI(ni/2, 1/2, x, ErrCode)
end; (* TProb *)

function FProb(ni1, ni2, x: Double; var ErrCode : word): Double;
begin
  x := ni2/(ni2 + ni1*x);
  Result := BetaI(ni2/2, ni1/2, x, ErrCode);
end; (* FProb *)

function BinProb(n,x: Integer; p: Double; var ErrCode: word; Upper: boolean=True): Double;
{ Objetivo
    Obtém probabilidades da distribuição binomial através da função beta incompleta
  Parâmetros
    n: número de experimentos
    x: Valor para o cálculo da probabilidade
    p: probabilidade de sucesso
    ErrCode: código de erro (utililizado pela beta incompleta)
    Upper: True para retorno da probabilidade P(X>=x), false para o seu complemento
}
begin
  Result:=BetaI(x,n-x+1,p,ErrCode);
  if not Upper then
    Result:=1-Result
end;

(* ===== Funcoes retiradas de Mardia e Zemroch ===== *)
function NInt(x: Double; upper: Boolean): Double;
var
  n,x2,y,s,t,vtmp,p1,
  p2,q1,q2,a1,a2,m   : Double;
begin
  if x = 0 then
    NInt := 0.5
  else
    begin
    upper := upper = (x > 0);
    x := Abs(x);
    x2 := x*x;
    y := 0.3989422804014*exp(-0.5*x2);        (* 0.398... e 1/sqrt(2pi) *)
    n := y/x;
    if not upper and (1.0 - n = 1.0) then
      NInt := 1
    else
      if upper and (n = 0.0) then
        NInt := 0
      else
        begin
        if upper then vtmp := 2.32 else vtmp := 3.5;
        if x > vtmp then
          begin
          a1 := 2.0; a2 := 0.0;
          n := x2 + 3.0; p1 := y;
          q1 := x; p2 := (n - 1.0)*y;
          q2 := n*x; m := p1/q1;
          t := p2/q2;
          if not upper then
            begin
            m := 1.0 - m;
            t := 1.0 - t
            end;
          while (m <> t) and (s <> t) do
            begin
            n := n + 4.0;
            a1 := a1 - 8.0; a2 := a1 + a2;
            s := a2*p1 + n*p2;
            p1 := p2; p2 := s;
            s := a2*q1 + n*q2;
            q1 := q2; q2 := s;
            s := m; m := t;
            if q2 > 10e30 then
              begin
              p1 := p1*10e-30; p2 := p2*10e-30;
              q1 := q1*10e-30; q2 := q2*10e-30;
              end;
            if upper then t := p2/q2 else t := 1.0 - p2/q2;
            end;
          NInt := t
          end
        else
          begin
          x := y*x;
          s := x;
          n := 3.0; t := 0.0;
          while s <> t do
            begin
            t := s; x := x*x2/n;
            s := s + x;
            n := n + 2.0;
            end;
          if upper then NInt := 0.5 - s else NInt := 0.5 + s
          end
        end
    end
end; (* NInt *)

function g(x: Double): Double;
var
  sum,term,y,n,incr: Double;
begin
  if x > 1.3 then
    g := -g(1.0/x)
  else
    if x < 10e-20 then
      g := 1.0
    else
      if x < 0.7 then
        g := (1.0 - x*x + (2.0*x)*Ln(x))/((1.0 - x)*(1.0 - x))
      else
        if x = 1.0 then
          g := 0.0
        else
          begin
          sum := 0.0; term := 1.0;
          y := 1.0 - x;
          n := 2.0;
          while Abs(incr/sum) > 10e-11 do
            begin
            term := term*y; incr := term/(n*(n+1.0));
            sum := sum + incr;
            n := n + 1.0;
            end;
          g := sum*2.0
          end;
end; (* g *)

function X2Aux(x, v, vfac: Double; upper: Boolean; var ErrCode: word): Double;
const
  big = 10e30;
  acc = 10e-11;
var
  gin,factor,a,
  b,term,rn,dif : Double;
  i             : Integer;
  pn            : array[1..6] of Double;
  UnDone        : Boolean;
begin
  ErrCode := 0;
  if x = 0.0 then
    if upper then X2Aux := 1.0 else X2Aux := 0.0
  else
    begin
    factor := v*Ln(x) - x - vfac;
    if (x > 1.0) and (x >= v) then
      begin
      a := 2.0 - v; b := a + x;
      pn[1] := 1.0;
      term := pn[1]; pn[2] := x;
      pn[3] := x + 1.0; pn[4] := b*x;
      gin := pn[3]/pn[4];
      UnDone := True;
      while UnDone do
        begin
        b := b + 2.0;
        for i := 1 to 2 do
          pn[i+4] := b*pn[i+2] - a*term*pn[i];
        if pn[6] <> 0.0 then
          begin
          rn := pn[5]/pn[6]; dif := Abs(gin - rn);
          if (Dif <= acc) and (dif <= acc*rn) then
            UnDone := False
          else
            gin := rn
          end;
        if UnDone then
          begin
          for i := 1 to 4 do pn[i] := pn[i+2];
          if (Abs(pn[5]) >= big) then
            for i := 1 to 4 do pn[i] := pn[i]/big;
          a := a + 1.0; term := term + 1.0
          end;
        end; (* while *)
      gin := exp(factor + Ln(gin));
      if not upper and (gin > 0.9) then
         ErrCode := IntProbI
      else
         ErrCode := 0;
      if upper then X2Aux := gin else X2Aux := 1.0 - gin
      end
    else
      begin
      term := 1.0;
      gin := term;
      rn := v + 1.0;
      while term > acc do
        begin
        term := term*x/rn;
        gin := gin + term;
        rn := rn + 1.0;
        end;
      gin := exp(factor)*gin/v;
      if upper and (gin > 0.9) then
         ErrCode := IntProbI
      else
         ErrCode := 0;
      if upper then X2Aux := 1.0 - gin else X2Aux := gin
      end
    end
end; (* X2Aux *)

function X2Int(x, v: Double; upper: Boolean; var ErrCode : word): Double;
var
  s, c, w: Double;
begin
  ErrCode := 0;
  if v <= 0.0 then
    begin
    ErrCode := ErrGLiber;
    Result := wscMissValue
    end
  else
    if x = 0.0 then
      if upper then Result := 1.0 else Result := 0
    else
      if x < 0.0 then
        begin
        ErrCode := ParamX;
        Result := wscMissValue
        end
      else
        begin
        { A constante 1000.0 representa o valor do parametro graus
          de liberdade (v) para o qual a aproximacao normal e
          utilizada. E uma constante dependente de maquina e seu
          valor pode ser reduzido }

        if v > 1000.0 then
          begin
          if x < 1.0 then
            if upper then Result := 1.0 else Result := 0.0
          else
            begin                            (* Aproximacao normal *)
            s := v - 1.0;
            c := -v + 0.66666666667 - 0.08/v;
            w := Sqrt((1.0 + g(s/x))/(2.0*x))*(x+c);
            Result := NInt(w, upper);
            ErrCode := AproxNormal;
            end
          end
        else
          Result := X2Aux(x*0.5, v*0.5, LogGamma(v*0.5), upper, ErrCode)
        end;
  Result:=wsGLib.Fuzz(Result)
end; (* X2Int *)

function BAux(x,p,q,lnpf,lnqf,c: Double; upper: Boolean): Double;
var
  finsum,infsum,
  temp,term,qrecur,
  index,lnterm     : Double;
  Alter            : Boolean;
begin
  if x <= 0.0 then
    if upper then Result := 1.0 else Result := 0.0
  else
    if x >= 1.0 then
      if upper then Result := 0.0 else Result := 1.0
    else
      begin
      if x <= 0.5 then
        Alter := False
      else
        begin
        Alter := True; temp := p;
        p := q; q := temp;
        x := 1.0 - x; lnpf := lnqf
        end;
      finsum := 0.0; temp := 1.0 - x;
      index := q; qrecur := index;
      if (temp < 0.75) and (p < (205.0 - 240*temp)) and (q > 400.0*temp)
      then
        begin
        term := 2*10e-77;
        lnterm := -176.605904980
        end
      else
        begin
        term := 1.0;
        lnterm := 0.0
        end;
      index := index - 1.0;
      while index > 0.0 do
        begin
        qrecur := index;
        term := term*(qrecur + 1.0)/(temp*(p + qrecur));
        finsum := finsum + term;
        index := index - 1.0
        end;
      term := 1.0; infsum := term; index := 0.0;

      { A constante 10e-11 representa a precisao na qual a integral de beta sera calculada.
        Ela pode ser alterada de acordo com as necessidades ou pode ser colocada como um
        parametro adicional formal }

      index := index + 1.0;
      while (term/infsum > 10e-11) do
        begin
        term := term*x*(index-qrecur)*(p+index-1.0)/(index*(p+index));
        infsum := infsum + term;
        index := index + 1.0;
        end;
      x := p*Ln(x);
      if finsum <= 0.0 then
        lnterm := 0.0
      else
        lnterm := exp(ln(finsum) - lnterm + q*ln(temp) + x + c);
      temp := infsum*exp(LogGamma(qrecur+p)
        -LogGamma(qrecur)-lnpf + x) + lnterm*p;
      if Alter = upper then Result := temp else Result := 1.0 - temp
      end;
  Result:=wsGLib.Fuzz(Result)
end; (* BAux *)

function BInt(x, a, b: Double; upper: Boolean; var ErrCode : word): Double;
const
  qmx = 500;               { Constante para uso nas rotinas de M & Z }

var
  n, yd, gam, h1, h3, y,
  p, lnpf, lnqf: Double;
  swap: Boolean;
begin
  ErrCode := 0;
  if (a <= 0.0) or (b <= 0.0) then
    begin
    Result := wscMissValue;
    ErrCode := ErrGLiber
    end
  else
    if (x = 0.0) or (x = 1.0) then
      if upper then Result := 1.0 - x else Result := x
    else
      if (x < 0.0) or (x > 1.0) then
        begin
        ErrCode := ParamX;
        Result := wscMissValue
        end
      else
        if ((a < 30.0) and (b > qmx)) or ((a > qmx) and (b < 30)) then
          begin
          if (a < b) then
            begin
            n := a; a := b;
            b := n; swap := True
            end
          else
            swap := False;
          n := a + 0.5*b - 0.5;
          if swap then
            yd := x/(2.0 - x)
          else
            yd := (1.0 - x)/(1.0 + x);
          yd := 2.0*n*yd;
          gam := (exp(b*Ln(yd) - yd - LogGamma(b))*(2.0*yd*yd -
            (b - 1.0)*yd - (b*b - 1.0)))/(24.0*n*n);
          swap := swap = upper;
          yd := X2Int(2.0*yd, 2.0*b, swap, Errcode);
          if swap then Result := yd - gam else Result := yd + gam;
          if Errcode = 0 then
             ErrCode := Aprox
          else
             ErrCode := ValNNulo
          end
        else
          if (a > qmx) or (b > qmx) or ((a+b-0.5)*Ln(a+b)-(b-0.5)*Ln(b)-
            (a-0.5)*Ln(a) > 175.0)
          then
            begin
            h1 := a + b - 1.0; y := 1.0 - x;
            h3 := sqrt((1.0 + y + g((a - 0.5)/(h1*x)) + x*g((b-0.5)/(h1*y)))/
              ((h1 + 0.1666666667)*x*y))*((h1 + 0.33333333333 +0.02*
              (1.0/a + 1.0/b + 1.0/(a+b)))*x - a + 0.33333333333 - 0.02/a -
              0.01/(a+b));
            Result := NInt(h3, upper);
            ErrCode := AproxNormal
            end
          else
            if Abs(x*a*(1.0-b)/(1.0+a)) < 10e-11 then
              begin
              x := exp(a*Ln(x) + LogGamma(1.0+a+b) - LogGamma(1.0+a) -
                LogGamma(1.0+b) - Ln(a/b+1.0));
              if upper then Result := 1.0 - x else Result := x;
              ErrCode := 0
              end
            else
              begin
              lnpf := LogGamma(a+1.0); lnqf := LogGamma(b+1.0);
              p := BAux(x, a, b, lnpf, lnqf, LogGamma(a+b)-lnpf-lnqf, upper);
              if ((x>0.5) = upper) or (p > 0.1) then
                ErrCode := 0
              else
                ErrCode := IntProbI;
              Result := p
              end;
  Result:=wsGLib.Fuzz(Result)
end; (* BInt *)

function FInt(x, v1, v2: Double; upper: Boolean; Var ErrCode : word): Double;
begin
  ErrCode := 0;
  if x < 0.0 then
    begin
    ErrCode := ParamX;
    Result := wscMissValue
    end
  else
    if x = 0.0 then
      if upper then Result := 1.0 else Result := 0.0
    else
      if v1 <= 0.0 then
        begin
        if v2 > 0.0 then
          Result := X2Int(v2/x, v2, not upper, ErrCode)
        else
          if ((x >= 1.0) = upper) then Result := 0.0 else Result := 1.0;
        end
      else
        if v2 <= 0.0 then
          Result := X2Int(x*v1, v1, upper, ErrCode)
        else
          begin
          x := v1*x;
          if x < v2 then
            Result := BInt(1.0/(1.0+v2/x), 0.5*v1, 0.5*v2, upper, Errcode)
          else
            Result := BInt(v2/(v2+x), 0.5*v2, 0.5*v1, not upper, ErrCode)
          end;
  Result:=wsGLib.Fuzz(Result)
end; (* FInt *)

function TInt(x, n: Double; OneSided, upper: Boolean; var Errcode : Word): Double;
var
  t: Double;
  xpos: Boolean;
begin
  xpos := x >= 0.0;
  if n > 0.0 then
    begin
    t := FInt(x*x, 1.0, n, upper = not OneSided or xpos, ErrCode);
    if not OneSided then
      Result := t
    else
      if (xpos = upper) then Result := 0.5*t else Result := 0.5 + 0.5*t
    end
  else
    begin
    t := NInt(x, (OneSided and upper) or (not OneSided and xpos));
    if OneSided then
      Result := t
    else
      if upper then Result := 2.0*t else Result := 1.0 - 2.0*t;
      if OneSided or upper or (t <= 0.45) then
        ErrCode := 0
      else
        ErrCode := IntProb2I
    end;
  Result:=wsGLib.Fuzz(Result)
end; (* TInt *)

function Sig(x: Double): Integer;
begin
  if x = 0.0 then
    Sig := 0
  else
    if x < 0.0 then Sig := -1 else Sig := 1
end; (* Sig *)

function fn(x: Double; ax: FArray; upper: Boolean): Double;
begin
  fn := ax[1] - NInt(x, upper)
end; (* fn *)

function fx21(x: Double; ax: FArray; upper: Boolean): Double;
var
   errCode: word;
begin
  fx21 := ax[1] - X2Aux(x, ax[3], ax[4], upper, Errcode)
end; (* fx21 *)

function fx22(x: Double; ax: FArray; upper: Boolean): Double;
begin
  fx22 := ax[1] - Sqrt((1.0+g(ax[3]/x))/(2.0*x))*(x+ax[4])
end; (* fx22 *)

function fb1(x: Double; ax: FArray; upper: Boolean): Double;
begin
  fb1 := ax[1] - Sqrt((1.0+(1.0-x)*g(ax[3]/x)+x*
    g(ax[4]/(1.0-x)))/(ax[5]*x*(1.0-x)))*(ax[6]*x-ax[7])
end; (* fb1 *)

function fb2(x: Double; ax: FArray; upper: Boolean): Double;
begin
  fb2 := ax[1] - BAux(x, ax[3], ax[4], ax[5], ax[6], ax[7], upper)
end; (* fb2 *)
(*
var
  xb1,xb2   : PFArray;
*)
procedure ZBrak(fx: HFunc; ax: FArray; upper: Boolean; x1, x2: Double;
  n: Integer; var nb: Integer; xb1,xb2: PFArray);
  { Dada uma funcao fx definida no intervalo de x1 a x2 subdivide o
    intervalo em n segmentos igualmente espacados e pesquisa os zeros
    da funcao. A entrada nb e o numero maximo de raizes a pesquisar
    e retorna o numero de pares xb1^[1..nb] e xb2^[1..nb] que forem
    encontrados }

label
  99;
var
  nbb,i     : Integer;
  x,fp,fc,dx: Double;
begin
  nbb := nb;
  nb := 0;
  x := x1;
  dx := (x2-x1)/n;
  fp := fx(x, ax, upper);
  for i := 1 to n do
    begin
    x := x + dx;
    fc := fx(x, ax, upper);
    if fc*fp < 0.0 then
      begin
      Inc(nb);
      xb1^[nb] := x-dx;
      xb2^[nb] := x
      end;
    fp := fc;
    if nbb = nb then GoTo 99;
    end;
99:
end; { ZBrack }

function ZBrent(f: HFunc; xx: FArray; upper: Boolean; x1, x2, Tol: Double;
  var Alarm: Integer): Double;

  { Utiliza o metodo de Brent para encontrar a raiz de uma funcao f
    definida entre x1 e x2. A raiz, que retorna como ZBrent, sera
    refinada ate que sua precisao seja Tol }

const
  ItMax = 100;               { Numero maximo de iteracoes }
  eps   = 3.0e-8;

var
  a,b,c,d,e,min1,
  min2,min,fa,fb,
  fc,p,q,r,s,tol1,xm: Double;
  iter              : Integer;
begin
  a := x1;
  b := x2;
  Alarm := 0;
  fa := f(a, xx, upper);
  fb := f(b, xx, upper);
  if fb*fa > 0.0 then
    begin
    ZBrent := 0.0;
    Alarm := Sig(fb);
    end
  else
    begin
    fc := fb;
    for iter := 1 to ItMax do
      begin
      if fb*fc > 0.0 then
        begin
        c := a;
        fc := fa;
        d := b-a;
        e := d
        end;
      if Abs(fc) < Abs(fb) then
        begin
        a := b;
        b := c;
        c := a;
        fa := fb;
        fb := fc;
        fc := fa
        end;
      tol1 := 2.0*eps*Abs(b) + 0.5*tol;
      xm := 0.5*(c - b);
      if (Abs(xm) <= tol1) or (fb = 0.0) then
        begin
        ZBrent := b;
        Exit
        end;
      if (Abs(e) >= tol1) and (Abs(fa) > Abs(fb)) then
        begin
        s := fb/fa;
        if a = c then
          begin
          p := 2.0*xm*s;
          q := 1.0 - s
          end
        else
          begin
          q := fa/fc;
          r := fb/fc;
          p := s*(2.0*xm*q*(q-r)-(b-a)*(r-1.0));
          q := (q-1.0)*(r-1.0)*(s-1.0)
          end;
        if p > 0.0 then q := -q;
        p := Abs(p);
        min1 := 3.0*xm*q-Abs(tol1*q);
        min2 := Abs(e*q);
        if min1 < min2 then min := min1 else min := min2;
        if 2.0*p < min then
          begin
          e := d;
          d := p/q
          end
        else
          begin
          d := xm;
          e := d
          end
        end
      else
        begin
        d := xm;
        e := d
        end;
      a := b;
      fa := fb;
      if Abs(d) > tol1 then
        b := b + d
      else
        begin
        if xm >= 0.0 then
          b := b + Abs(tol1)
        else
          b := b - Abs(tol1)
        end;
      fb := f(b, xx, upper);
      end; { iter }
    ZBrent := b
    end;
end; { ZBrent }

function Hyp(f: HFunc; var xx: Double; ax: FArray; upper: Boolean;
  x1, x2, eps: Double; var Alarm: Integer): Double;

var
  a, b, c, p: Integer;
  x, g, t, n: Double;
  z: array[-1..1] of Double;
  u: array[-1..1] of Double;
  done, obey: Boolean;
begin
  xx := x1; z[-1] := xx;
  u[-1] := f(xx, ax, upper);
  xx := x2; z[1] := xx;
  u[1] := f(xx, ax, upper);
  if (u[-1]*u[1] > 0.0) then
    begin
    alarm := Sig(u[1]);
    Hyp := 0.0
    end
  else
    begin
    p := 0; a := p; b := 1;
    done := false;
    x := (z[b] + z[-a-b])/2.0;
    while not done do
      begin
      xx := x;
      z[a] := x;
      u[a] := f(xx, ax, upper);
      if (Sig(u[a]) = Sig(u[b])) then
        begin
        b := -a-b;
        if p < 6 then p := 0
        end
      else
        Inc(p);
      a := -a-b;
      done := (Abs(z[a] - z[b]) <= eps) or (Abs(z[a] - z[-a-b]) <= eps);
      if not done then
        begin
        obey := True;
        if p < 6 then begin
          n := 0.0; t := n;
          c := -1;
          while (not done and (c <= 1)) do
            begin
            x := z[c];
            if u[c] = 0.0 then
              done := True
            else
              begin
              if c = 0 then
                g := (z[1] - z[-1])/u[0]
              else
                g := c*(z[-c] - z[0])/u[c];
              t := t + g*(z[c] - z[-a-b]);
              n := n + g
              end;
            Inc(c)
            end; (* while *)
          if not done and (n <> 0.0) and (Abs(u[a]) > Abs(u[-a-b])) then
            begin
            x := z[-a-b] + t/n;
            obey := Abs(x - z[-a-b]) > Abs(z[b] - z[-a-b])/2.0
            end
        end; (* p *)
        if obey and not done then
          x := (z[b] + z[-a-b])/2.0
        end (* not done *)
      end; (* while *)
    Alarm := 0;
    Hyp := x
    end
end; (* Hyp *)

function NInv(p, eps: Double; upper: Boolean; var ErrCode : word): Double;
var
  x1, x2, x: Double;
  Alarm: Integer;
  fpos: Boolean;
  xx: FArray;
begin
  if (p <= 0.0) or (p >= 1.0) then
    begin
    NInv := 0.0;
    ErrCode := PontPercInf
    end
  else
    begin
    ErrCode := 0; fpos := p > 0.5;
    if fpos then x1 := 1.0 - p else x1 := p;
    x1 := Sqrt(Ln(1.0/(x1*x1)));
    x1 := x1 - (x1*0.27061 + 2.30753)/((x1*0.04481 + 0.99229)*x1 + 1.0);
    x2 := x1 + 0.003; x1 := x1 - 0.003;
    if fpos = upper then
      begin
      x := x1; x1 := -x2;
      x2 := -x
      end;
    xx[1] := p;
    NInv := Hyp(fn , x, xx, upper, x1, x2, Abs(eps*x), Alarm);
    end
end; (* NInv *)

function X2Inv(p, v, eps: Double; upper: Boolean; var Errcode : word): Double;
var
  n, x1, x2, x, sd, s, c: Double;
  xx: FArray;
  Alarm: Integer;
begin
  if v < 0.0 then
    begin
    ErrCode := ErrGLiber;
    X2Inv := wscMissValue
    end
  else
    if (p <= 0.0) or (p >= 1.0) then
      begin
      if ((p = 0.0) and not upper) or ((p = 1.0) and upper) then
        begin
        ErrCode := 0;
        X2Inv := 0.0
        end
      else
        begin
        ErrCode := PontPercInf;
        X2Inv := wscMissValue
        end
      end
    else
      begin
      ErrCode := 0;

      { O valor 1000.0 representa o valor do parametro v para o qual
        a aproximacao normal sera utilizada. E dependente da maquina
        e pode ser reduzida para algumas maquinas }

      if v <= 1000.0 then
        begin
        n := v*0.5;

        { Aproximacao normal para p e v pequenos }

        if (v < 0.9999) or (v < 3.0001) and (upper and (p > 0.9999)
          or not upper and (p < 0.0001))
        then
          begin
          if upper then x := 1.0 - p else x := p;
          x := (Ln(x) + LogGamma(n + 1.0))/n + 0.69314718056
          end
        else
          x := 0.0;

        { O valor -27 representa o logaritmo natural do erro relativo
          maximo permitido no logaritmo do ponto percentual. Deve ser
          um valor um pouco menor que ln(epsilon) }

        if x < -27 then
          begin
          X2Inv := exp(x);
          if upper and (p < 0.1) then ErrCode := PrecIntProb
          end
        else
          begin
          x1 := n*0.5; x2 := v;
          c := LogGamma(n);
          xx[1] := p; xx[3] := n; xx[4] := c;

          x := Hyp(fx21, x, xx, upper, x1, x2, eps*x, Alarm);
          while Alarm <> 0 do
            begin
            if ((Alarm = 1) = upper) then
              begin
              x2 := x1;
              x1 := x1*0.1
              end
            else
              begin
              x1 := x2;
              x2 := x2*10
              end;
            x := Hyp(fx21, x, xx, upper, x1, x2, eps*x, Alarm);
            end; (* while *)

          X2Inv := 2.0*x;
          if ErrCode = IntProbI then ErrCode := PrecIntProb
          end
        end
      else
        begin
        n := NInv(p, eps, upper, Errcode); ErrCode := AproxNormal;
        sd := Sqrt(v*2.0); x1 := v - 3.0*sd;
        x2 := v + 3.0*sd; s := v - 1.0;
        c := -v + 0.66666666667 - 0.08/v;
        xx[1] := n; xx[3] := s; xx[4] := c;

        x := Hyp(fx22, x, xx, upper, x1, x2, eps*x, Alarm);
        while Alarm <> 0 do
          begin
          if Alarm = 1 then
            begin
            x1 := x2;
            x2 := x2 + sd
            end
          else
            begin
            x2 := x1;
            x1 := x1 - sd
            end;
          x := Hyp(fx22, x, xx, upper, x1, x2, eps*x, Alarm);
          end; (* while *)

        X2Inv := x
        end
      end
end; (* X2Inv *)

function ppchi2(const p,v,g: Double; var ifault: Word): Double;
{ Objetivo
    Obter pontos percentuais da distribuição gama.
    Algorithm AS 91   Appl. Statist. (1975) Vol.24, P.35

    Incorpora mudanças sugeridas em AS R85 (vol.40(1), pp.233-5, 1991), que elimina a necessidade de
    considerar a amplitude para o valor da probabilidade, embora isso continue a ser testado.

    Rotinas auxiliares: Pontos percentuais da distribuição normal padrão e função gama incompleta.
    No algoritmo original: PPND = AS 111 (or AS 241) and GAMMAD = AS 239.

  Parâmetros
    p: Probabilidade para obtenção do valor. Deve estar na amplitude 0.000002 a 0.999998
    v: Valor do parâmetro de formato. Deve ser positivo
    g: logaritmo da função gama ln(gama(v/2))
    ifault: código de erro.
      ifault = 0: processamento Ok
      ifault = 1: valor da probabilidade fora da amplitude
      ifault = 2: parâmetro de formato com valor negativo
      ifault = 4: o resultado é tão preciso quanto a máquina permitir.
  Observações
    Se(p<=0 ou p>=1;Erro)
}
const
  maxit = 20;
  aa    = 0.6931471806;
  e     = 0.5e-6;
  pmin  = 0.000002;
  pmax  = 0.999998;
  c: array[1..38] of Double = (0.01, 0.222222, 0.32, 0.4, 1.24, 2.2, 4.67, 6.66, 6.73, 13.32, 60, 70,
    84, 105, 120, 127, 140, 175, 210, 252, 264, 294, 346, 420, 462, 606, 672, 707, 735, 889, 932, 966,
    1141, 1182, 1278, 1740, 2520, 5040);
var
  s            : array[1..6] of Double;
  a,b,cc,ch,p1,
  q,t,x,xx,p2  : Double;
  Erro         : Word;
  i            : Integer;
begin
  // testa argumentos e inicializa
  if (p<pmin) or (p>pmax) then
    begin
    Result:=-1;
    ifault:=1;
    Exit
    end;
  if (v<=0) then
    begin
    Result:=-1;
    ifault:=2;
    Exit
    end;
  xx:=0.5*v;         // atencao !!!!
  cc:=xx-1;
  // Inicia aproximacao para valores pequenos de qq
  if v<(-c[5]*Ln(p)) then
    begin
    ch:=Power(p*xx*exp(g+xx*aa),1/xx);
    if ch<e then
      begin
      Result:=ch;
      Exit
      end
    end
  else
    // Inicia aproximação para v <=0.32
    if v<=c[3] then
      begin
      ch:=c[4];
      a:=Ln(1-p);
      repeat
        q:=ch;
        p1:=1+ch*(c[7]+ch);
        p2:=ch*(c[9]+ch*(c[8]+ch));
        t:=-0.5+(c[7]+2*ch)/p1-(c[9]+ch*(c[10]+3*ch))/p2;
        ch:=ch-(1-exp(a+g+0.5*ch+cc*aa)*p2/p1)/t;
      until Abs(q/ch-1)<=c[1];
      end
    else
      begin
      // Chama algoritmo para obter pontos percentuais da normal
      x:=NInv(p,1e-20,False,Erro);
      // Inicia aproximacao para utilizando estimativa de Wilson e Hilferty
      p1:=c[2]/v;
      ch:=v*Power(x*sqrt(p1)+1-p1,3);
      // Inicia aproximacao para p tendendo a 1
      if ch>(c[6]*v+6) then
        ch:=-2*(Ln(1-p)-cc*Ln(0.5*ch)+g);
      end;
  i:=1;
  repeat
    q:=ch;
    p1:=0.5*ch;
    p2:=p-X2Aux(p1,xx,LogGamma(xx),False,Erro);
    t := p2*exp(xx*aa+g+p1-cc*Ln(ch));
    b := t/ch;
    a := 0.5*t-b*cc;
    s[1] := (c[19]+a*(c[17]+a*(c[14]+a*(c[13]+a*(c[12]+c[11]*a)))))/c[24];
    s[2] := (c[24]+a*(c[29]+a*(c[32]+a*(c[33]+c[35]*a))))/c[37];
    s[3] := (c[19]+a*(c[25]+a*(c[28]+c[31]*a)))/c[37];
    s[4] := (c[20]+a*(c[27]+c[34]*a)+cc*(c[22]+a*(c[30]+c[36]*a)))/c[38];
    s[5] := (c[13]+c[21]*a+cc*(c[18]+c[26]*a))/c[37];
    s[6] := (c[15]+cc*(c[23]+c[16]*cc))/c[38];
    ch := ch+t*(1+0.5*t*s[1]-b*cc*(s[1]-b*(s[2]-b*(s[3]-b*(s[4]-b*(s[5]-b*s[6]))))));
    Inc(i);
  until (abs(q/ch-1) > e) or (i>maxit) or (Erro<>0);
  if i>maxit then
    ifault:=4;
  Result := ch
end; // ppchi2

function BInv(p, a, b, eps: Double; upper: Boolean; var ErrCode : word): Double;
const
  nbmax = 20;
  intv = 100;
  qmx  = 500;               { Constante para uso nas rotinas de M & Z }

var
  perpt,tmp,x1,
  x2,x,lnpf,lnqf,c,
  h1,h2,h3,h4,h5,
  h6,sd,eps1      : Double;
  swap, done      : Boolean;
  i, nb, Alarm    : Integer;
  xx              : FArray;
  xb1,xb2         : PFArray;
begin
  if (a <= 0.0) or (b <= 0.0) then
    begin
    BInv := wscMissValue;
    ErrCode := ErrGLiber
    end
  else
    if (p = 0.0) or (p = 1.0) then
      begin
      if upper then BInv := 1.0 - p else BInv := p;
      ErrCode := 0
      end
    else
      if (p < 0.0) or (p > 1.0) then
        begin
        BInv := wscMissValue;
        ErrCode := PontPercInf
        end
      else
        begin
        ErrCode := 0;
        done := (a < 0.5) or (b < 0.5) or (a = 1.0) or (b = 1.0);
        if done then
          begin
          swap := ((p < 0.5) = upper) or (a = 1.0);
          if swap then
            begin
            x := a; a := b;
            b := x
            end;
          if (swap = upper) then tmp := p else tmp := 1.0 - p;
          PerPt := (LogGamma(1.0+a)+LogGamma(1.0+b)-LogGamma(1.0+a+b)+
            Ln(a+b)-Ln(b)+Ln(tmp))/a;
          if b <> 1.0 then
            begin
            if eps < 10e-5 then tmp := eps else tmp := 10e-5;
            tmp := Ln(tmp/Abs(a*(1.0-b)/(b+1.0)));
            if PerPt > tmp then
              begin
              if swap then
                begin
                x := a; a := b;
                b := x
                end;
              done := False;
              end
            end
          end;
        if done then
          begin
          if (p < 0.1) and (swap = not upper) then ErrCode := PrecIntProb;
          x2 := exp(PerPt);
          if swap then BInv := 1.0 - x2 else BInv := x2
          end
        else
          if ((a < 30.0) and (b > qmx)) or ((a > qmx) and (b < 30.0)) then
            begin
            PerPt := FInv(p, 2.0*b, 2.0*a, eps, not upper, ErrCode);
            BInv := a/(b*PerPt + a)
            end
          else
            begin
            tmp := (a+b-0.5)*Ln(a+b)-(b-0.5)*Ln(b)-(a-0.5)*Ln(a);
            if (a > qmx) or (b > qmx) or (tmp > 175.0) then
              begin
              h3 := NInv(p, 10e-11, upper, ErrCode);
              x1 := a/(a+b);
              sd := Sqrt(a*b/((a+b)*(a+b)*(a+b+1.0)));
              x2 := x1 + 3.0*sd; x1 := x1 - 3.0*sd;
              if x1 <= 0.0 then x1 := 10e-8;
              if x2 >= 1.0 then x2 := 0.99999999;
              h1 := a+b-1.0; h4 := (b-0.5)/h1;
              h5 := (a-0.5)/h1; h6 := h1 + 0.166666666667;
              h1 := h1 + 0.333333333333 + 0.02*(1.0/a+1.0/b+1.0/(a+b));
              h2 := a - 0.333333333333+0.02/a+0.01/(a+b);
              xx[1] := h3; xx[3] := h5; xx[4] := h4;
              xx[5] := h6; xx[6] := h1; xx[7] := h2;

              PerPt := ZBrent(fb1, xx, upper, x1, x2, eps*x, Alarm);
              while Alarm <> 0 do
                begin
                if Alarm = 1 then
                  begin
                  x1 := x2;
                  x2 := x2 + 0.9*(1.0 - x2)
                  end
                else
                  begin
                  x2 := x1;
                  x1 := x1*0.1
                  end;
                PerPt := ZBrent(fb1, xx, upper, x1, x2, eps*x, Alarm);
                end; (* while *)

              BInv := PerPt;
              ErrCode := AproxNormal
              end
            else
              begin

              { Inversao da funcao beta incompleta }

              if upper then tmp := 1.0 - p else tmp := p;
              x2 := tmp*0.1+a/(a+b);
              lnpf := LogGamma(a + 1.0); lnqf := LogGamma(b + 1.0);
              c := LogGamma(a + b) - lnpf - lnqf;
              xx[1] := p; xx[3] := a; xx[4] := b;
              xx[5] := lnpf; xx[6] := lnqf; xx[7] := c;

              PerPt := ZBrent(fb2, xx, upper, 0.0, x2, eps*x2/2, Alarm);
              if Alarm <> 0 then
                begin
                GetMem(xb1,sf(nbmax));
                GetMem(xb2,sf(nbmax));
                nb := nbmax;
                ZBrak(fb2,xx,upper,0.0,1,intv,nb,xb1,xb2);
                i := 0;
                while Alarm <> 0 do
                  begin
                  Inc(i);
                  eps1 := eps*(xb1^[i] + xb2^[i])/2.0;
                  PerPt := ZBrent(fb2, xx, upper, xb1^[i], xb2^[i], eps1,
                    Alarm);
                  end;
                FreeMem(xb1, sf(nbmax));
                FreeMem(xb2, sf(nbmax));
                end;
              if (p < 0.1) and ((PerPt <= 0.5) = upper) then
                 ErrCode := PrecIntProb;

              { Testa para a igualdade de PerPt com 0.5. A tolerancia
                10e-8 abaixo deve ser maior que epsilon }

              if Abs(PerPt - 0.5) < 10e-8 then ErrCode := PontPercInv;
              BInv := PerPt
              end
            end
        end
end; (* BInv *)

function FInv(p, v1, v2, eps: Double; upper: Boolean; var ErrCode: Word): Double;
const
  qmx = 500;               { Constante para uso nas rotinas de M & Z }

var
  x,PerPt,qmx080,qmx160,
  qmx200,h,f0,f1,f2     : Double;
  ifail1                : Integer;
  swap                  : Boolean;
begin
  ErrCode := 0;
  if (p <= 0.0) or (p >= 1.0) then
    begin
    if ((p = 0.0) and not upper) or ((p = 1.0) and upper) then
      begin
      ErrCode := 0;
      FInv := 0.0
      end
    else
      begin
      ErrCode := PontPercInf;
      FInv := wscMissValue
      end
    end
  else
    if (v1 <= 0.0) then
      begin
      if v2 <= 0.0 then
        begin
        ErrCode := 0;
        FInv := 1.0
        end
      else
        FInv := v2/X2Inv(p, v2, eps, not upper, Errcode);
      end
    else
      if (v2 <= 0.0) then
        FInv := X2Inv(p, v1, eps, upper, Errcode)/v1
      else
        begin
        qmx080 := 0.8*qmx; qmx160 := 1.6*qmx; qmx200 := 2.0*qmx;
        ErrCode := 0;
        if (v1 >= 100.0) or (v2 >= 100.0) then
          swap := v1 < v2
        else
          swap := (BInt(0.5, v2*0.5, v1*0.5, upper, ErrCode) > (1.0-p)) = upper;
        if swap then
          begin
          x := v2; v2 := v1;
          v1 := x
          end;
        if (v2 < 60.0) and (v1 > qmx200) then
          begin
          h := qmx160/v1; upper := upper = not swap;
          f0 := FInv(p, -1.0, v2, eps*0.1, upper, ErrCode);
          f1 := FInv(p, qmx160, v2, eps*0.1, upper, ErrCode);
          ifail1 := ErrCode;
          f2 := FInv(p, qmx080, v2, eps*0.1, upper, ErrCode) - f1;
          if (ifail1 = PontPercInv) or (ErrCode = PontPercInv) then
             ErrCode := InterPont
          else
             ErrCode := Inter3Pont;
          f1 := f1 - f0; f2 := f2 - f1;
          PerPt := f0 + h*f1 + h*(h - 1.0)*0.5*f2;
          if swap then FInv := 1.0/PerPt else FInv := PerPt
          end
        else
          begin
          PerPt := BInv(p, v2*0.5, v1*0.5, eps, upper = swap, ErrCode);
          if PerPt > 0.9 then ErrCode := DigSignif;
          if ((PerPt = 1.0) and swap) or ((PerPt = 0.0) and not swap) then
            begin
            FInv := wscMissValue;
            ErrCode := PontPercF
            end
          else
            if swap then
              FInv := v1*PerPt/(v2*(1.0 - PerPt))
            else
              FInv := v2*(1.0 - PerPt)/(v1*PerPt)
          end
        end
end; (* FInv *)

function TInv(p,n,eps: Double; upper,OneSided: Boolean; var Errcode : word): Double;
{ Objetivo
    Retorna quantil da distribuição t de Student
  Parâmetros
    p       : Valor da probabilidade
    n       : número de graus de liberdade
    eps     : precisão de aproximação
    upper   : True se a área se refere a porção superior da distribuição
    OneSided: True se o teste for unilateral
    ErrCode : Código de erro
}
var
  x: Double;
begin
  if n > 0.0 then
    begin
    if OneSided then
      begin
      OneSided := p <= 0.5;
      if OneSided then p := 2.0*p else p := 2.0*(p - 0.5);
      OneSided := not OneSided = upper;
      if OneSided then upper := not upper
      end;
    x := FInv(p, 1.0, n, eps, upper, ErrCode);
    if x <= 0.0 then TInv := 0.0 else TInv := Sqrt(x);
    end
  else
    begin
    if OneSided then
      x := p
    else
      if upper then x := 0.5*p else x := 0.5 + 0.5*p;
    TInv := NInv(x, eps, upper, ErrCode)
    end
end; (* TInv *)


(*======================== Distribuição do Usuário ==============================*)


Function DistMeanUser(RowX,RowP: word; TabUsu: TWSGeneral; var Error: Word): Double;
{ Objetivo
   Obter a media de uma distribuicao de probabilidade definida na forma de uma tabela
  Parametros
   RowX: Linha onde estao os valores da variavel aleatoria X
   RowP: Linha onde estao os valores das probabilidades associadas aos valores
   TabUsu: Matriz onde estao os valores da variavel e respectivas probabilidades
  Observacoes
   Checa para verificar se a soma dos valores das probabilidades soma 1
}
var
   i: integer;
   aux: Double;
begin
  result:=0;
  aux:=0;
  Error := 0;
  for i:=1 to TabUsu.NCols do
  begin
    result:= result + (tabUsu[RowX,i]*TabUsu[RowP,i]);
    aux:=aux + TabUsu[RowP,i];
  end;
  if aux<>1 then
     Error := 100 { Definir uma constante para retperesenta este erro }
end;


Function DistVarUser(RowX, RowP: word; Mean: Double; TabUsu: TWSGeneral): Double;
{ Objetivo
   Obter a variancia de uma distribuicao de probabilidade definida na forma de uma tabela
  Parametros
   RowX: Linha onde estao os valores da variavel aleatoria X
   RowP: Linha onde estao os valores das probabilidades associadas aos valores
   Mean: Media da distribuicao
   TabUsu: Matriz onde estao os valores da variavel e respectivas probabilidades
}
var
   i: integer;
begin
  result:=0;
  for i:=1 to TabUsu.NCols do
      result:= result + (Sqr(TabUsu[RowX,i]-Mean) * TabUsu[RowP,i]);
end;

Function DistSkewUser(RowX,RowP: word; Mean,Varc: Double; TabUsu: TWSGeneral): Double;
var
   i: integer;
   m3: Double;
begin
  m3:=0;
  for i:=1 to TabUsu.NCols do
      m3:= m3 + (power(TabUsu[RowX,i]-Mean,3) * TabUsu[RowP,i]);
  result:= m3 / (Varc * Sqrt(Varc));
end;


Function DistCurtUser(RowX, RowP: word; Mean,Varc:Double; TabUsu:TWSGeneral): Double;
var
   i: integer;
   m4: Double;
begin
   m4:=0;
   for i:=1 to TabUsu.NCols do
       m4:= m4 + (power(TabUsu[RowX,i]-Mean,4) * TabUsu[RowP,i]);
   result:= m4/Sqr(Varc);
end;

{=========== Estimadores de Máxima Verossimilhança ============================}

procedure Calcula_Bk_Ck_Hk(previus: Double; var Bk, Ck, Hk: Double; Sample: TWSVec);
var
  i: integer;
  aux, aux2: Double;
begin
  Bk:= 0;
  Ck:= 0;
  Hk:= 0;
  for i:=1 to Sample.len do
  begin
    aux:= power(Sample[i],previus);
    aux2:= ln(Sample[i]);
    Bk:= Bk + aux;
    Ck:= Ck + (aux*aux2);
    Hk:= Hk + (aux*sqr(aux2));
  end;
end;

procedure Estimadores_Bernoulli(var prob: Double; Sample: TWSVec);
var
  i: integer;
  aux: Double;
begin
  aux:=0;
  for i:=1 to Sample.len do
      aux:= aux + Sample[i];
  prob:= aux / Sample.len;
end;

(*procedure Estimadores_Beta(var alfa, beta: Double; Sample: TWSVec);
var

begin

end;

procedure Estimadores_Binomial(var NExp, prob: Double; Sample: TWSVec; tKnown: boolean);
var
  i, k, Fk, t: integer;
  Max1, n: longint;
  Max, Min, gprev, gnext: Double;
  Vn, media, varian, p: Double;
begin
  Sample.VarMean(media, varian, n);
  if tKnown then
    Prob:= media / NExp
  else
  begin
    Vn:= (Sample.Len - 1) * varian / Sample.len;
    if media > Vn then
    begin
      Sample.MinMax(Min, Max);
      Max1:=trunc(floor((media * (max - 1)) / (1-(Vn / media))));
      t:= trunc(floor(Max))-1;
      repeat
        gprev:= gnext;
        inc(t);
        P:= media / t;
        for k:=1 to trunc(floor(Max)) do
        begin
          Fk:=0;
          For i:=1 to Sample.len do
            if Sample[i] >= k then
               inc(Fk);
          gnext:= gnext +Fk*ln(t-k+1)+Sample.len*t*ln(1-p)+Sample.len*media*ln(p/(1-p));
        end;
        if t = trunc(floor(Max)) then
          gprev:= gnext;
      until (gnext < gprev) or (t = Max1);
      NExp:= t - 1;
      Prob:= media / NExp;
    end
    else
      raise exception.Create('Não existem valores estimados para esta amostra.');
  end;
end;

procedure Estimadores_BinNeg(var NSuc, prob: Double; Sample: TWSVec; sKnown: boolean);
var
  i, Fk, K, s: integer;
  Max1, n: longint;
  Max, Min, gprev, gnext: Double;
  Vn, media, varian, p: Double;
begin
  Sample.VarMean(media, varian, n);
  if sKnown then
    Prob:= NSuc / (media + NSuc)
  else
  begin
    Vn:= (Sample.len - 1) * varian / Sample.len;
    if media <= Vn then
    begin
      Sample.MinMax(Min, Max);
      Max1:=trunc(floor((media * (max - 1)) / (1-(Vn / media))));
      s:= 0;
      repeat
        gprev:= gnext;
        inc(s);
        P:= s / (media + s);
        for k:=1 to trunc(floor(Max)) do
        begin
          {gnext:=0;}
          Fk:=0;
          For i:=1 to Sample.Len do
            if Sample[i] >= k then
               inc(Fk);
          gnext:= gnext + Fk*ln(s+k-1)+Sample.len*s*ln(p)+Sample.len*media*ln(1-p);
        end;
        if s = 1 then
          gprev:= gnext;
      until gnext < gprev;
      NSuc:= s - 1;
      Prob:= NSuc / (media + NSuc);
    end
    else
      raise exception.Create('Não existem valores estimados para esta amostra.');
  end;
end;


procedure Estimadores_Exponencial(var Lambda: Double; Sample: TWSVec);
{ Objetivo
    Determinar a estimativa de um parametro da distribuicao exponencial
  Parametros
    Lambda: Retorna valor do parametro calculado como o inverso da media amostral
    Sample: Vetor com os valores da amostra
}
var
  n: integer;
begin
  Lambda := Sample.Mean(n);
  Lambda:= ScalarInv(Lambda);
end;

procedure Estimadores_Gamma(var alfa: Double; Sample: TWSVec);
var

begin

end;

procedure Estimadores_Geometrica(var prob: Double; Sample: TWSVec);
{ Objetivo
    Obtem a estimativa do parametro da distribuicao Geometrica
  Parametros
    Prob: Retorna o valor da estimativa
    Sample: Vetor que contem os valores da amostra
}
var
  n: integer;
begin
  Prob := Sample.Mean(n);
  Prob:= ScalarInv(Prob + 1);
end;

procedure Estimadores_LogNormal(var mean, varc: Double; Sample: TWSVec);
{ Objetivo
    Obtem estimativas de parametros da distribuicao Lognormal
  Parametros
    Mean: Retorna estimativa como media aritmetica da amostra
    Varc: Retorna variancia da amostra
    Sample: Vetor que contem os valores da amostra
}
var
  n: integer;
begin
  Sample.VarMean(Mean, varc, n);
end;

procedure Estimadores_Normal(var Mean, Varc: Double; Sample: TWSVec);
{Objetivo
   Obter estimadores de maxima verossimilhanca da distribuicao normal
 Paramatros
   Mean: Retorna estimativa da media
   Varc: Retorna estimativa (MV) da variancia
   Sample: Vetor com os dados amostrais
}
var
  n: integer;
begin
  Sample.VarMean(Mean, Varc, n);
  Varc := ((n-1)/n)*Varc
end;

procedure Estimadores_Poisson(var Mean, Rate: Double; Sample: TWSVec);
{ Objetivo
    Obter estimativa do parametro da distribuicao de Poisson
  Parametros
    Mean: Retorna media aritmetica da amostra
    Rate: Retorna taxa como inverso da media aritmetica
}
var
  n: integer;
begin
  Mean := Sample.Mean(n);
  Rate := ScalarInv(Mean)
end;

procedure Estimadores_Uniforme(var limInf, LimSup: Double; Sample: TwsVec);
{Objetivo
   Obter estimativas dos parametros da distribuicao uniforme
 Parametros
   LimInf: Estimativa do limite inferior, tomado como o minimo da amostra
   LimSup: Estimativa do limite superior, tomado como o maximo da amostra
   Sample: Vetor que contem os valores que compoem a amostra
}
begin
  Sample.MinMax(LimInf, LimSup);
end;

procedure Estimadores_UniDisc(var limInf, LimSup: Double; Sample: TWSVec);
begin
  MinMax(Sample, limInf, LimSup);
end;


procedure Estimadores_Weibull(var alfa, beta: Double; Sample: TwsVec);
{Objetivo
  Obtem estimativas dos parametros da distribuicao de Weibull
 Parametros
  Alfa: Parametro de formato da distribuicao
  Beta: Parametro de escala da distribuicao
  Sample: Vetor que contem os componentes da amostra
}
var
  i: integer;
  aux1, aux2, aprev, anext, Bk, A, Ck, Hk, vpi: Double;
begin
  vpi:=3.1415926535897932385;
  aux1:= 0;
  aux2:= 0;
  for i:=1 to Sample.len do
  begin
    aux1:= aux1 + sqr(ln(Sample[i]));
    aux2:= aux2 + ln(Sample[i]);
  end;
  aprev:= power(((6/sqr(vpi))*(aux1-sqr(aux2)/Sample.len)/(Sample.len-1)),-1/2);
  A:= aux2/Sample.len;
  i:=0;
  Repeat
    if i >= 1 then
       aprev:= anext;
    Calcula_Bk_Ck_Hk(aprev, Bk, Ck, Hk, Sample);
    anext:= aprev + ((A+1/aprev-Ck/Bk)/(1/sqr(aprev)+(Bk*Hk-Sqr(Ck))/Sqr(Bk)));
    inc(i);
  until abs(anext - aprev) < 0.0001;
  alfa:= anext;
  aux1:= 0;
  for i:=1 to Sample.len do
      aux1:= aux1 + power(Sample[i],alfa);
  Beta:= power(aux1/Sample.len, 1/alfa);
end;
*)
end.
