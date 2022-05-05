{Rotinas modificadas
* Function Power( x,y: Double): Double; (21/08/97) Amauri
* function SeqBinCo(n, k: Integer; var Coef: TwsVec): Double; Incluida 10/06/99
Mudanca no parametro F das rotinas abaixo - transformado em TwsVec
* function BinCo(n, k: Integer; var F: TwsVec): Double;
* function Factrl(n: Integer; var F: TwsVec; var FactNTop: Integer): Double;
* function FactLn(n: Integer; var F: TwsVec): Double;

}


{$N+,E+}
Unit wsMath;

Interface
uses Math,
     wsGLib;

(*
 ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
 บ Standard Mathematical Functions.        Version 1.0                        บ
 บ                                                                            บ
 บ This code has been placed in the public domain.                            บ
 บ All non-trivial functions that do not reside permanently inside my head    บ
 บ (e.g. Bessel Functions) were obtain from:                                  บ
 บ                                                                            บ
 บ   _CRC_Standard_Mathematical_Tables_, CRC Press, 25th Edition, William H.  บ
 บ   Beyer, editor.                                                           บ
 บ                                                                            บ
 บ   HP-15C Owner's Manual, Feb. 1984, Hewlett-Packard Company.               บ
 บ                                                                            บ
 บ   _Probability,_Random_Variables,_and_Stochastic_Processes_, Athanasios    บ
 บ   Papoulis, Second Edition, 1984, McGraw-Hill                              บ
 บ                                                                            บ
 บ                                                                            บ
 บ All code has been tested, though not rigorusly, and is correct to the best บ
 บ of my knowledge. However, I make no guarentees. If you find any errors or  บ
 บ make any changes/enhancements, I would appreciate it if you inform me. My  บ
 บ address is:                                                                บ
 บ                                                                            บ
 บ Scott Bostater                                                             บ
 บ Georgia Tech Research Institute                                            บ
 บ 7220 Richardson Road                                                       บ
 บ Symrna, GA 30080                                                           บ
 บ (404)-528-7782    (please, only in an emergency, my boss to work for HIM)  บ
 บ                                                                            บ
 บ e-mail:                                                                    บ
 บ ---------------------------------------                                    บ
 บ Internet: bb16@prism.gatech.edu                                            บ
 บ UUCP:     ...!{allegra,amd,hplabs,ut-ngp}!gatech!edu!bb16                  บ
 บ Bitnet:   BBOSTATE@GTRI01                                                  บ
 บ                                                                            บ
 บ                                                                            บ
 บ Special Thanks to Mike Baden and Stan West for thier contributions         บ
 บ                                                                            บ
 ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
*)
// Run of the mill math functions that I always need
Function  Atan360(x,y: Double): Double;
Function  InvTanDeg (X,Y:Double):Double;
Function  InvTanRad (X,Y:Double):Double;
Function  InvSinDeg (Y,R:Double):Double;
Function  InvSinRad (Y,R:Double):Double;
Function  InvCosDeg (X,R:Double):Double;
Function  InvCosRad (X,R:Double):Double;

function  Fuzz(x: Double): Double;

// Bessel Functions
function  I0 (x:Double):Double;   { modified Bessel of Order 0 }
function  I1 (x:Double):Double;   { modified Bessel of Order 1 }
function  I2 (x:Double):Double;   { modified Bessel of Order 2 }
function  J0 (x:Double):Double;   { Bessel function of Order 0 }
function  J1 (x:Double):Double;   { Bessel function of Order 1 }

Function  U (index:Integer; T:Double):Double;

// Voltage and Power Decibel Functions

Function  db10 (x:Double):Double;
Function  db20 (x:Double):Double;
Function  undb10 (X:Double):Double;
Function  undb20 (x:Double):Double;

function Power(const Base, Exponent: Extended): Extended;

function GammLn(const xx: Double): Double;
function Beta(z, w: Double): Double;
function LogGamma(x: Double): Double;
FUNCTION DiGamma(const x: Double; var iFault: Integer): Double;
FUNCTION TriGamma(const x: Double; var iFault: Integer): Double;
function FDuncan(const Alpha,p,f: Double): Double;

function Combin(N, M: Integer): Double;
function Permut(N: Integer): Double;
function Arranjo(N, M: Integer): Double;


Implementation
uses SysUtils,
     wsConstTypes,
     wsVars {FuzzVal};

Function U( index: Integer; T: Double): Double;
var
  P: ShortInt;
Begin
  P := -1-index;
  U := 0;              { default }
  If T = 0 Then
   Begin
    If index = 0 Then U := 1
   End
  Else If T > 0 Then
    Case P of
     0: U := 1;
     1: U := T;
     2: U := Sqr(t)/2;
     3..127: U := Power(t,P)/P;
    End; { Case }
End; { Function U }

Function InvTanDeg(X,Y:Double):Double;
{ 0 <= result <= 360 }
Begin
  if X=0 then Result := Pi/2.0 else Result := ArcTan(Y/X);
  Result := Result*180.0/pi;
  if (X<=0.0) and (Y<0) then Result := Result - 180.0;
  if (X< 0.0) and (Y>0) then Result := Result + 180.0;
  If Result < 0 then Result := Result+360.0;
End;

Function InvTanRad(X,Y:Double):Double;
{ 0 <= result <= 2pi }
Begin
  if X=0 then Result := Pi/2.0 else Result := ArcTan(Y/X);
  if (X<=0.0) and (Y<0) then Result := Result - pi;
  if (X< 0.0) and (Y>0) then Result := Result + pi;
  If Result < 0 then Result := Result+2*pi;
End;

Function InvSinDeg (Y,R:Double):Double;
{ -90 <= result <= 90 }
Var
  X:Double;
Begin
  X := Sqrt(Sqr(R)-Sqr(Y));
  Result := InvTanDeg(X,Y);
  If Result > 90 then Result := Result - 360;
End;

Function InvSinRad (Y,R:Double):Double;
{ -90 <= result <= 90 }
Var
  X:Double;
Begin
  X := Sqrt(Sqr(R)-Sqr(Y));
  Result := InvTanRad(X,Y);
  If Result > 90 then Result := Result - 360;
End;

Function InvCosDeg (X,R:Double):Double;
{ -90 <= result <= 90 }
Var
  Y:Double;
Begin
  Y := Sqrt(Sqr(R)-Sqr(X));
  Result := InvTanDeg(X,Y);
  If Result > 90 then Result := Result - 360;
End;

Function InvCosRad (X,R:Double):Double;
{ -90 <= result <= 90 }
Var
  Y:Double;
Begin
  Y := Sqrt(Sqr(R)-Sqr(X));
  Result := InvTanRad(X,Y);
  If Result > 90 then Result := Result - 360;
End;

function I0(x: Double): Double;
var
  tnum,factor: word;
  term: Double;
begin
  term:=1;
  Result:=term;
  factor:=0;
  tnum:=1;
  repeat
    factor:=factor+2+2*(tnum-1);
    term:=term*sqr(x/2)/factor;
    Result:=Result+term;
    inc(tnum);
  until (abs(term)<1e-15) or (tnum=0);      {no overflow}
end; {I0}

function I1(x: Double): Double;
var
  tnum,factor: word;
  term: Double;
begin
  term:=x/4;
  Result:=term;
  factor:=0;
  tnum:=1;
  repeat
    factor:=factor+3+2*(tnum-1);
    term:=term*sqr(x/2)/factor;
    Result:=Result+term;
    inc(tnum);
  until (abs(term)<1e-15) or (tnum=0);      {no overflow}
end; {I1}

function I2(x: Double): Double;
var
  tnum,factor: word;
  term: Double;
begin
  term:=sqr(x)/24;
  Result:=term;
  factor:=0;
  tnum:=1;
  repeat
    factor:=factor+4+2*(tnum-1);
    term:=term*sqr(x/2)/factor;
    Result:=Result+term;
    inc(tnum);
  until (abs(term)<1e-15) or (tnum=0);      {no overflow}
end; {I2}

function J0(x: Double): Double;
var
  tnum: word;
  term: Double;
begin
  Result:=1;
  term:=1;
  tnum:=1;
  repeat
    term:=term*-sqr(x)/(4*sqr(tnum));
    Result:=Result+term;
    inc(tnum);
  until (abs(term)<1e-15) or (tnum=0);      {no overflow}
end; {J0}

function J1(x: Double): Double;
var
  tnum,factor: word;
  term: Double;
begin
  term:=x/2;
  Result:=term;
  factor:=0;
  tnum:=1;
  repeat
    factor:=factor+2+2*(tnum-1);
    term:=term*-sqr(x)/(4*factor);
    Result:=Result+term;
    inc(tnum);
  until (abs(term)<1e-15) or (tnum=0);      {no overflow}
end; {J1}

Function Atan360( x, y: Double): Double;
{Esta fun็ใo obt้m, em graus, o valor do arco cuja tangente vale x/y .}
Begin
  if X=0 then Result := Pi/2.0 else Result := ArcTan(X/Y);
  Result := Result*180.0/pi;
  if (X<=0.0) and (Y<0) then Result := Result - 180.0;
  if (X< 0.0) and (Y>0) then Result := Result + 180.0;
  If Result < 0 then Result := Result+360.0;
End;

function Power(const Base, Exponent: Extended): Extended;
var
  R1, R2, R3: Extended;
begin
  R1 := Abs(Exponent);
  R2 := Abs(Base);
  if Exponent = 0.0 then
    R3 := 1.0 // Always 1 for 0 exponent
  else
  begin
    if Base = 0.0 then
      R3 := 0.0 // Always 0 for 0 X
    else
    begin
      R3 := Exp(Ln(R2) * R1); // Basic calculation
      if Base < 0.0 then
        R3 := -R3; // Negate if X < 0
      if Exponent < 0.0 then
        R3 := 1.0 / R3; // Flip over if exponent negative
    end;
  end;
  Result := R3;
end;


Function db10( X: Double): Double;
Begin
  If x < 1.0e-50 Then
    Result := wscMissValue
  Else
    Result := 10 * Log10(x)
End; { Function db10 }

Function db20( X: Double): Double;
Begin
  If x < 1.0e-50 Then
    db20 := wscMissValue
  Else
    db20 := 20 * Log10(x);
End; { Function db20 }

Function Undb10( X: Double): Double;
Begin
  Undb10 := Power(10, X/10);
End; { Function Undb10 }

Function Undb20( X: Double): Double;
Begin
  Undb20 := Power(10, X/20);
End; { Function db20 }

function Fuzz(x: Double): Double;
var
  z: Extended;
begin
  z:=abs(frac(x));
  if z>= 0.5 then
     z:=int(x)+1
  else
     z:=int(x);
  if Abs(z-x) < wsvFuzzVal then
    Result:= z
  else
    Result:= x;
end;

(* ===== Funcoes retiradas de Numerical Recipes in Pascal ===== *)
function GammLn(const xx: Double): Double;
{ Objetivo
    Retorna o valor do logaritmo neperiano da funcao gama
  Parametros
    xx: Valor (>0) para o calculo
}
const
  stp = 2.50662827465;
var
  x, tmp, ser: Double;
begin
  try
    x := xx - 1;
    tmp := x + 5.5;
    tmp := (x+0.5)*ln(tmp) - tmp;
    ser := 1.0 + 76.18009173/(x+1.0) - 86.50532033/(x+2) + 24.01409822/(x+3)
      - 1.231739516/(x+4) + 0.120858003e-2/(x+5) - 0.536382e-5/(x+6);
    Result := tmp + Ln(stp*ser)
  except
    Result := wscMissValue
  end
end; (* GammLn *)

{ =========== Comparacoes de medias ============== }
function FDuncan(const Alpha,p,f: Double): Double;
{ Objetivo
    Retorna valores criticos para o teste de Duncan
  Parโmetros
    Alpha: nํvel de significancia para o teste
    p    : n๚mero de medias abrangidas pela diferenca
    f    : n๚mero de graus de liberadade associada a variancia do contraste
}
var
  r, y, z,
  w1, w2: Double;
begin
  Result := wscMissValue;
  if (FEquals(alpha,0.05) or FEquals(alpha,0.01) or FEquals(alpha,0.1)) then
    begin
    if FEquals(alpha,0.1) then
      r := Math.Min(f, p)
    else
      if FEquals(alpha,0.05) then
        r := Math.Min(f+2, p)
      else
        r := Math.Min(2*f, p);
    y := 1/f; z := 1/sqrt(r);
    if ((f<=3) or (r<=3)) then begin { Tabela 1 }
      if f = 1 then
        if FEquals(alpha,0.1) then
          Result := 8.929
        else
          if FEquals(alpha,0.05) then
            Result := 17.97
          else
            Result := 90.03
      else
        if f=2 then
          if FEquals(alpha,0.1) then
            Result := 4.13
          else
            if FEquals(alpha,0.05) then
              Result := 6.085
            else
              Result := 14.04
        else
          if ((p=2) and (f>=3)) then
            if FEquals(alpha,0.1) then
              Result := 2.3283+2.048*y+2.8317*y*y
            else
              if FEquals(alpha,0.05) then
                Result := 2.7784+3.0359*y+6.3404*y*y
              else
                Result := 3.6405+7.1927*y+8.8962*y*y+33.263*y*y*y
          else
            if ((p=3) and (f>=3)) or ((f=3) and (p>3)) then
              if FEquals(alpha,0.1) then
                Result := 2.4360+1.9996*y+1.7947*y*y
              else
                if FEquals(alpha,0.05) then
                  Result := 2.9217+3.208*y+4.6911*y*y
                else
                  Result := 3.7941+7.5559*y+9.5681*y*y+25.466*y*y*y
      end
    else
      begin { Tabela 2 }
      w1 := y*y;
      w2 := z*z;
      if FEquals(alpha,0.1) then
        if ((r >= 4) and (r <= 20)) then
          Result := 2.8913-10.3545*y+2.1921*z-2.714*w1+49.617*y*z-11.2972*w2
            +1.105*y*w1+6.995*w1*z-50.648*y*w2+11.169*w2*z
        else
          Result := 3.0549-41.493*y+3.534*z+516.79*w1+245.50*y*z-31.449*w2
            +876.07*y*w1-2643.38*w1*z-286.63*y*w2+60.41*z*w2
      else
        if FEquals(alpha,0.05) then
          if ((r >=4) and (r<=17)) then
            Result := 3.5409-4.8389*y+0.9313*z-2.123*w1+31.157*y*z-7.6639*w2
              +12.759*w1*y+0.212*w1*z-27.779*y*w2+7.353*z*w2
          else
            Result := 3.5883-45.660*y+5.099*z-2.32*w1+396.24*y*z-46.554*w2
              -1.39*y*w1+21.74*w1*z-855.99*y*w2+95.07*z*w2
          else
            if ((r>=4) and (r<=14)) then
             Result := 4.5660+3.4831*y+0.5406*z-7.246*w1+23.068*y*z-7.8283*w2
                +27.158*y*w1+26.777*w1*z-27.626*y*w2+8.134*z*w2
            else
              Result := 4.9138-12.756*y+0.341*z-31.179*w1+169.221*y*z-23.023*w2
                +18.74*w1*y+118.36*w1*z-350.15*y*w2+50.37*z*w2
      end { if f <=3 }
  end { if alpha }
end;

function Beta(z, w: Double): Double;
begin
  Result := exp(LogGamma(z) + LogGamma(w) - LogGamma(z+w))
end; (* Beta *)

function LogGamma(x: Double): Double;
var
  f, z: Double;
begin
  try
    if x < 7.0 then
      begin
      f := 1.0;
      z := x;
      while z < 7.0 do
        begin
        x := z;
        f := f*z;
        z := z+1
        end;
      x := x + 1.0;
      f := -Ln(f)
      end
    else
      f := 0;
    z := 1.0/(x*x);
    Result := f + (x-0.5)*Ln(x) - x + 0.918938533204673 +
      (((-0.000595238095238*z + 0.000793650793651)*z - 0.002777777777778)
      *z + 0.083333333333333)/x
  except
    Result := wscMissValue
  end
end; (* LogGamma *)

FUNCTION DIGAMMA(const X: Double; var IFAULT: Integer): Double;
{
  ALGORITHM AS 103  APPL. STATIST. (1976) VOL.25, NO.3

  Calculates DIGAMMA(X) = D( LOG( GAMMA(X))) / DX
}

//     Set constants, SN = Nth Stirling coefficient, D1 = DIGAMMA(1.0)
const
  S=1E-05;
  C=8.5;
  S3=8.333333333E-02;
  S4=8.3333333333E-03;
  S5=3.968253968E-03;
  D1=-0.5772156649;
var
  R,Y: Double;
BEGIN
  // Check argument is positive
  Result := 0;
  Y := X;
  if (Y <= 0) then
    begin
    IFAULT := 1;
    Exit
    end;
  IFAULT := 0;

  // Use approximation if argument <= S
  if (Y <= S) then
    begin
    Result := D1 - 1/Y;
    Exit
    end;

  // Reduce to Result(X + N) where (X + N) >= C

  while (Y < C) do
    begin
    Result := Result - 1/Y;
    Y := Y + 1;
    end;

//     Use Stirling's (actually de Moivre's) expansion if argument > C

  R := 1/Y;
  Result := Result + Ln(Y) - 0.5*R;
  R := R*R;
  Result := Result - R*(S3 - R*(S4 - R*S5))
END;

FUNCTION TriGamma(const x: Double; var iFault: Integer): Double;
{ Objetivo
    Calcula a segunda derivada da fun็ใo Ln(Gamma(.))
    trigamma(x) = d**2(log(gamma(x))) / dx**2
  Parโmetros
    x: Valor para obten็ใo num้rica da derivada
    iFault: C๓digo de falha. iFault=0, processamento normal; =1 se x<=0

algorithm as121   Appl. Statist. (1978) vol 27, no. 1
}
const
  a=0.0001;
  b=5;
  // b2, b4, b6 e b8 sao os numeros de Bernoulli
  b2=0.1666666667;
  b4=-0.03333333333;
  b6=0.02380952381;
  b8=b4; //-0.03333333333
var
  y,z: Double;
begin
  // verfifica se x eh positivo
  if x<=0 then
    begin
    iFault:=1;
    Result:=wscMissValue;
    Exit
    end;
  Result:=0;
  z:=x;
  // utiliza aproximacao para pequenos valores, se x <= a
  if (z<=a) then
    Result:=1/(z*z);
  // increase argument to (x+i) >= b
  while (z<b) do
    begin
    Result:=Result+1/(z*z);
    z:=z+1
    end;

  // aplica formula assintotica se argumento >= b
  y:=1/(z*z);
  Result:=Result+0.5*y+(1+y*(b2+y*(b4+y*(b6+y*b8))))/z
end; // TriGamma

function Combin(N, M: Integer): Double;
var
  i,mt : integer;
begin
  if n>=m then
    begin
    result:= 1;
    mt:= m;
    for i:= n downto n-m+1 do
      begin
      result:= result * i;
      if m > 0 then
         begin
         result:= result/mt;
         dec(mt);
         end;
      end;
    end
  else
    Result:=wscMissValue;
end;

function Arranjo(N, M: Integer): Double;
var
  i : integer;
begin
  if n>=m then
    begin
    result:= 1;
    for i:= n downto n-m+1 do
      result:= result * i;
    end
  else
    Result:=wscMissValue;
end;

function Permut(N: Integer): Double;
var
  i : integer;
begin
  result:= 1;
  for i:= n downto 2 do
    result:= result * i;
end;

end. // unit wsMath
