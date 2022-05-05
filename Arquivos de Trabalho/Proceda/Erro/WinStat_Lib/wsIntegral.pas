unit wsIntegral;

interface
uses SysUtils,
     wsGLib,
     wsAvaliadorDeExpressoes,
     wsMatrix,
     wsVec,
     wsMath;

type

{ Objetivo
    Implementar métodos de integração numérica. Não pode ser utilizada diretamente
}
  TwsIntegral = class
  private
    FEval:    TAvaliator;
    FLL:      Double;
    FHL:      Double;
    FExpr:    String;
    FResult:  Double;
    function  Trapzd(a,b: Double; n: integer; Var NIteracoes : Integer) : Double;
    procedure SetFHL(const Value: Double);
    procedure SetExpr(const Value: String);
    procedure Polint(xa,ya: TwsVec; n: integer; x: Double; var y,dy: Double);
    procedure SetFLL(const Value: Double);   // passar para Math
  public
    constructor Create(Const Expr: String; LL, HL: Double);
    destructor Destroy; override;
    property LowLimit: Double read FLL write SetFLL;
    property HighLimit: Double read FHL write SetFHL;
    property Expression: String read FExpr write SetExpr;
    property Result: Double read FResult;
    function Calculate: Double; virtual; abstract;
  end;

  { Objetivo
      Obtem o resultado numérico de uma integral pelo método do trapézio
      Ref.: Numerical recipes in Pascal pág. 125
  }
  TwsMIQtrapezio = class(TwsIntegral)
    function Calculate : Double; override;
  end;

  TwsMISimpson = class(TwsIntegral)
    function Calculate : Double; override;
  end;

  TwsMIRomberg = class(TwsIntegral)
    function Calculate : Double; override;
  end;

implementation

{ TwsIntegral }

constructor TwsIntegral.Create(const Expr: String; LL, HL: Double);
begin
  inherited Create;
  FLL:= LL;
  FHL:= HL;
  FEval:= TAvaliator.Create;
  FEval.TabVar.AddFloat('x', 0);
  SetExpr(Expr);
end;

destructor TwsIntegral.Destroy;
begin
  FEval.Free;
  inherited Destroy;
end;

function TwsIntegral.Trapzd(a, b: Double; n: integer; Var NIteracoes: integer): Double;
{** (a,b) indicam, respectivamente, o limite inferior e o limite superior
    para integrar a funcao.
 ** n eh o numero de iteracoes que o procedimento fara.
 ** o resultado
 }
var
  j, NumIt  : Integer;
  x,tnm,sum,
  del,R1,R2 : Double;
begin
  if (n = 1) then
    begin
    FEval.TabVar.SetFloatValue('x', a);
    R1:= FEval.Evaluate.AsFloat;

    FEval.TabVar.SetFloatValue('x', b);
    R2:= FEval.Evaluate.AsFloat;

    Result:= 0.5 * (b-a)* (R1 + R2);
    NIteracoes:= 1;
    end
  else
    begin
    NumIt:= NIteracoes;
    tnm:= NIteracoes;
    del:= (b-a)/tnm;
    x:= a+0.5*del;
    sum:= 0.0;
    for j:= 1 to NumIt do
      begin
      FEval.TabVar.SetFloatValue('x', x);
      R1:= FEval.Evaluate.AsFloat;
      sum:= sum + R1;
      x:= x + del;
      end;
    Result:= 0.5*(Result+(b-a)*sum/tnm);
    NIteracoes:= 2*NIteracoes;
    end;
end;

procedure TwsIntegral.SetExpr(const Value: String);
begin
  FEval.Expression:= Value;
  FExpr:= Value;
end;

procedure TwsIntegral.SetFHL(const Value: Double);
begin
  FHL := Value;
end;

procedure TwsIntegral.Polint(xa, ya : TwsVec; n : Integer; x : Double; Var y, dy: Double);
var
  ns, m, i    : integer;
  w, hp, ho,
  dift,dif,den: Double;
  c,d         : TwsVec;
begin
  c:= TwsDFvec.Create(n);
  d:= TwsDFvec.Create(n);
  ns:= 1;
  dif:= abs(x-xa[1]);
  for i:= 1 to n do
    begin
    dift:= abs(x-xa[i]);
    if (dift < dif) then
       begin
       ns:= i;
       dif:= dift;
       end;
    c[i]:= ya[i];
    d[i]:= ya[i];
    end;
  y:= ya[ns];
  ns:= ns-1;
  for m:= 1 to n-1 do
    begin
    for i:= 1 to n-m do
        begin
        ho:= xa[i]-x;
        hp:= xa[i+m]-x;
        w:= c[i+1]-d[i];
        den:= ho-hp;
        if (den = 0.0) then
           raise Exception.Create('Erro na rotina de interpolacao polinomial');
        den:= w/den;
        d[i]:= hp*den;
        c[i]:= ho*den;
        end;
    if ((2*ns) < (n-m)) then
       begin
       dy:= c[ns+1]
       end
    else
       begin
       dy:= d[ns];
       ns:= ns-1
       end;
    y:= y+dy
    end
end;


procedure TwsIntegral.SetFLL(const Value: Double);
begin
  FLL := Value;
end;

function TwsMIQtrapezio.Calculate: Double;
const
   eps   = 1.0e-10;
   jmax  = 20;
var
   j            : integer;
   olds         : Double;
   TrapIt       : integer;
begin
  olds:= -1.0e30;
  TrapIt:= 100;
  for j:= 1 to 10 do
    begin
    Result:= trapzd(FLL,FHL,j,TrapIt);
    if (abs(Result-olds) < eps*abs(olds)) then
       break;
    olds:= Result;
    end;
end;

{ TwsMISimpson }

function TwsMISimpson.Calculate: Double;
const
  eps   = 1.0e-10;
  jmax  = 20;
var
  j, TrapIt : integer;
  st,ost,os,
  ItValue : Double;
begin
  ost:= -1.0e30;
  os:= -1.0e30;
  TrapIt:= 100;
  for j:= 1 to jmax do
    begin
    ItValue:= Trapzd(FLL,FHL,j,TrapIt);
    Result:= (4.0*ItValue-ost)/3.0;
    if (abs(Result-os) < eps*abs(os)) then
       break;
    os:= Result;
    ost:= ItValue;
    end;
end;

function TwsMIRomberg.Calculate: Double;
const
   eps   = 1.0e-10;
   jmax  = 20;
   jmaxp = 21;   (* jmax+1 *)
   k     = 5;  // numero de pontos do polinomio interpolador
var
   i, j   : integer;
   dss    : Double;
   h,s,c,d: TwsVec;
   TrapIt : Integer;
begin
  h := TwsDFVec.Create(jmaxp);
  s := TwsDFVec.Create(jmaxp);
  c := TwsDFVec.Create(k);
  d := TwsDFVec.Create(k);
  h[1]:= 1;
  TrapIt:= 100;
  for j:= 1 to jmax do
    begin
    s[j]:= trapzd(FLL,FHL,j,TrapIt);
    if (j >= k)  then
      begin
      for i := 1 to k do
        begin
        c[i] := h[j-k+i];
        d[i] := s[j-k+i];
        end;
      Polint(c,d,k,0,Result,dss);
      if (abs(dss) < eps*abs(Result)) then
        begin
        c.free;
        d.free;
        s.free;
        h.free;
        Exit;
        end;
      end;
    s[j+1] := s[j];
    h[j+1] := 0.25*h[j]
    end;
  c.free;
  d.free;
  s.free;
  h.free;
end;

end.
