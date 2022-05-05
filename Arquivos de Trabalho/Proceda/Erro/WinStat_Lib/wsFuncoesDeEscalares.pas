{$B+}
unit wsFuncoesDeEscalares;
{ Unidade para implementacao de operacoes com escalares, considerando valores
  perdidos }

interface
uses wsConstTypes;

{ Operacoes aritmeticas }
function ScalarSum(const x, y: Double): Double;
function ScalarSub(const x, y: Double): Double;
function ScalarProd(const x, y: Double): Double;
function ScalarDiv(const x, y: Double): Double;
function ScalarPower(const x, y: Double): Double;

{ Valor maximo e minimo }
function ScalarMin(const x, y: Double): Double;
function ScalarMax(const x, y: Double): Double;

{ E OU lógico }
function ScalarAnd(const x, y: Double): Double;
function ScalarOr(const x, y: Double): Double;

{ funcoes matematicas }
function ScalarAbs(const y: Double): Double;
function ScalarLnGamma(const y: Double): Double;
function ScalarTriGamma(const y: Double): Double;
function ScalarInt(const y: Double): Double;
function ScalarFrac(const y: Double): Double;
function ScalarFuzz(const y: Double): Double;
function ScalarFloor(const y: Double): Double;
function ScalarCeil(const y: Double): Double;
function ScalarExp(const y: Double): Double;
function ScalarLog(const y: Double): Double;
function ScalarLn(const y: Double): Double;
function ScalarArcTan(const y: Double): Double;
function ScalarArcCos(const y: Double): Double;
function ScalarArcSin(const y: Double): Double;
function ScalarAng(const y: Double): Double;
function ScalarTan(const y: Double): Double;
function ScalarCos(const y: Double): Double;
function ScalarSin(const y: Double): Double;
function ScalarSinH(const y: Double): Double;
function ScalarCosH(const y: Double): Double;
function ScalarTanH(const y: Double): Double;
Function ScalarArcSinH (const y: Double): Double;
Function ScalarArcCosH (const y: Double): Double;
Function ScalarArcTanH (const y: Double): Double;

function ScalarSqrt(const y:Double): Double;
function ScalarSqr(const y:Double): Double;
function ScalarInv(const y: Double): Double;

{ operacoes boleanas }
function ScalarEQ(const x, y: Double): Double;
function ScalarLT(const x, y: Double): Double;
function ScalarLE(const x, y: Double): Double;
function ScalarGT(const x, y: Double): Double;
function ScalarGE(const x, y: Double): Double;
function ScalarNE(const x, y: Double): Double;

implementation
uses Math,
     wsGLib,
     wsMath;

{ Operacoes aritmeticas }
function ScalarSum(const x, y: Double): Double;
{ Retorna a soma de x e y. Se um deles e valor perdido, retorna valor perdido}
begin
  if not(IsMissValue(x) or IsMissValue(y)) then
    Result := x+y
  else
    Result := wscMissValue
end;

function ScalarSub(const x, y: Double): Double;
{ Retorna a diferenca de x e y. Se um deles e valor perdido, retorna valor perdido}
begin
  if not(IsMissValue(x) or IsMissValue(y)) then
    Result := x-y
  else
    Result := wscMissValue
end;

function ScalarProd(const x, y: Double): Double;
{ Retorna o produto de x e y. Se um deles e valor perdido, retorna valor perdido}
begin
  if not(IsMissValue(x) or IsMissValue(y)) then
    Result := x*y
  else
    Result := wscMissValue
end;

function ScalarDiv(const x, y: Double): Double;
{ Retorna a razao x/y. Se um deles e valor perdido ou se y=0 retorna valor perdido}
begin
  if not((IsMissValue(x) or IsMissValue(y))) and (y<>0) then
    try
      Result := x/y
    except
      Result:= wscMissValue
    end
  else
    Result := wscMissValue
end;

function ScalarPower(const x, y: Double): Double;
{ Retorna a potencia x**y. Se um deles e valor perdido ou se ambos sao nulos,
  retorna valor perdido }
begin
  if not(IsMissValue(x) or IsMissValue(y)) then
    try
      Result := Power(x,y)
    except
      Result := wscMissValue
    end
  else
    Result := wscMissValue
end;

{ Valor maximo e valor minimo }
function ScalarMin(const x, y: Double): Double;
{ Retorna o minimo de x e y. Se um deles e valor perdido, retorna valor perdido}
begin
  if not(IsMissValue(x) or IsMissValue(y)) then
    if x < y then Result := x else Result := y
  else
    Result := wscMissValue
end;

function ScalarMax(const x, y: Double): Double;
{ Retorna o maximo de x e y. Se um deles e valor perdido, retorna valor perdido}
begin
  if not(IsMissValue(x) or IsMissValue(y)) then
    if x > y then Result := x else Result := y
  else
    Result := wscMissValue
end;

{ E OU lógico }
function ScalarAnd(const x, y: Double): Double;
{ Retorna 1 se os escalares x e y forem ambos nao nulos; 0 caso contrario. }
begin
  if (x<>0) and (y<>0) then Result := ScalarTrue else Result := ScalarFalse
end;

function ScalarOr(const x, y: Double): Double;
{ Retorna 1 se os escalares x ou y forem ambos nao nulos; 0 caso contrario. }
begin
  if (x<>0) or (y<>0) then Result := ScalarTrue else Result := ScalarFalse
end;

{ funcoes matematicas ======> verificar valores validos para as funcoes }
function ScalarAbs(const y: Double): Double;
{ Retorna o valor absoluto do escalar y. Retorna wscMissValue se y for wscMissValue.}
begin
  if not IsMissValue(y) then
    Result := Abs(y)
  else
    Result := wscMissValue;
end;

function ScalarLnGamma(const y: Double): Double;
{Objetivo
   Logaritmo natuarl da função gama.
 Parâmetros
   y: Argumento da função. Deve ser positivo
 Exemplos
   x:=ScalarLnGamma(2);  // retorna 0.00000
   xScalarLnGamma(1.5);  // retorna -0.12078
}
begin
  if (y<=0) or (not IsMissValue(y)) then
    Result := LogGamma(y)
  else
    Result := wscMissValue;
end;

function ScalarTriGamma(const y: Double): Double;
{ Objetivo
    Retorna a derivada da função digama
  Parâmetro
    y: Argumento da função.
}
var
  Err: Integer;
begin
  if not IsMissValue(y) then
    Result := TriGamma(y,Err)
  else
    Result:=wscMissValue;
end;

function ScalarInt(const y: Double): Double;
{ Retorna o valor da parte inteira do escalar y. Retorna wscMissValue se y for wscMissValue.}
begin
  if not IsMissValue(y) then
    Result := Int(y)
  else
    Result := wscMissValue;
end;

function ScalarFrac(const y: Double): Double;
{ Retorna o valor da parte fracionaria do escalar y. Retorna wscMissValue se y for wscMissValue.}
begin
  if not IsMissValue(y) then
    Result := Frac(y)
  else
    Result := wscMissValue;
end;

function ScalarFuzz(const y: Double): Double;
{ Retorna o valor inteiro do escalar y se ele estiver na amplitude 1.0e-12. Retorna
  wscMissValue se y for wscMissValue. }
begin
  if not IsMissValue(y) then
    Result := Fuzz(y)
  else
    Result := wscMissValue;
end;

function ScalarFloor(const y: Double): Double;
{ Objetivo
    Maior inteiro que é menor ou igual ao argumento. Se o argumento difere do inteiro em
    menos de FuzzVal, a função retorna o inteiro.
  Exemplos
    x:=ScalarFloor(2.1);       // retorna 2
    x:=ScalarFloor(-2.4);      // retorna -3
    x:=ScalarFloor(3);         // retorna 3
    x:=ScalarFloor(-1.6);      // retorna -2
    x:=ScalarFloor(1-1.e-13);  // retorna 1
    x:=ScalarFloor(wscMissValue)  // retorna wscMissValue
}
var
  z: Double;
begin
  z:=Fuzz(y);
  if not IsMissValue(z) then
    Result := Floor(z)
  else
    Result := wscMissValue;
end;

function ScalarCeil(const y: Double): Double;
{ Objetivo
    Retorna o menor inteiro que é maior ou igual ao argumento. Se o argumento não difere do
    inteiro (em valor absoluto) de FuzzVal, retorna aquele inteiro. Se o arqgumento for um
    valor perdido, retorna valor perdido.
  Exemplos
    x:=ceil(2.1);        // retorna 3
    x:=ceil(3);          // retorna 3
    x:=ceil(-2.4);       // retorna -2
    x:=ceil(1+1.e-11);   // retorna 2
    x:=ceil(-1+1.e-11);  // retorna 0
    x:=ceil(1+1.e-13);   // retorna 1
}
var
  z: Double;
begin
  z:=ScalarFuzz(y);
  if not IsMissValue(z) then
    Result := Ceil(z)
  else
    Result := wscMissValue;
end;

function ScalarExp(const y: Double): Double;
{ Objetivo
    Retorna o valor da função exponencial. Esta função retorna a constante e (aproxidamente
    2.71828 na potência estabelecida pelo argumento.
  Parâmetro
    y: Argumento para a função
  Exemplos
    x:=ScalarExp(1);          // retorna 2.71828
    x:=ScalarExp(0);          // retorna 1
    x:=ScalarExp(wscMissValue);  // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    try
      Result := Exp(y);
    except
      Result := wscMissValue;
    end
  else
    Result := wscMissValue;
end;

function ScalarLn(const y: Double): Double;
{ Objetivo
    Retorna o logaritmo natural (neperiano ou logaritmo de base e) do argumento
  Parâmetro
    y: Argumento da função. Deve ser positivo
  Exemplos
    x:=ScalarLn(1);          // retorna 0
    x:=ScalarLn(10);         // retorna 2.30259
    x:=ScalarLn(wscMissValue);  // retorna wscMissValue
    x:=ScalarLn(-1);         // retorna wscMissValue
}
begin
  if (y>0) and not IsMissValue(y) then
    try
      Result := Ln(y)
    except
      Result := wscMissValue
    end
  else
    Result := wscMissValue;
end;

function ScalarLog(const y: Double): Double;
{ Objetivo
    Retorna o logaritmo decimal ou comum (base 10) do argumento
  Parâmetro
    y: Argumento da função, positivo.
  Exemplos
    x:=ScalarLog(1);          // retorna 0
    x:=ScalarLog(10);         // retorna 1
    x:=ScalarLog(100);        // retorna 2
    x:=ScalarLog(wscMissValue);  // retorna wscMissValue
    x:=ScalarLog(-1);         // retorna wscMissValue
}
begin
  if (y>0) and not IsMissValue(y) then
    try
      Result := Log10(y)
    except
      Result:=wscMissValue
    end
  else
    Result := wscMissValue
end;

function ScalarArcTan(const y: Double): Double;
{ Objetivo
    Retorna o arco tangente (inverso da tangente) do argumento. O resultado retorna em
    radianos
  Parâmetros
    y: Argumento da função
  Examples
    x:=ScalarArcTan(1);  // retorna 0.78540
    x:=ScalarArcTan(0);  // retorna 0.00000
    x:=ScalarArcTan(-9); // retorna -1.46014
    x:=ScalarArcTan(wscMissValue); // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    Result := ArcTan(y)
  else
    Result := wscMissValue;
end;

function ScalarArcCos(const y: Double): Double;
{ Objetivo
    Arco cosseno (inverso do cosseno) do argumento. O valor retorna em radianos
  Parâmetros
    y: Argumento da função. Deve estar entre -1 e 1.
  Exemplos
	x:=ScalarArcCos(1);    // retorna 0.00000
	x:=ScalarArcCos(0);    // retorna 1.57078
	x:=ScalarArcCos(-.5);  // retorna 2.09440
}
begin
  if (y>=-1) and (y<=1) and not IsMissValue(y) then
    Result := ArcCos(y)
  else
    Result := wscMissValue;
end;

function ScalarArcSin(const y: Double): Double;
{ Objetivo
    Retorna o arcosseno (inverso do seno) do argumento. O valor retorna em radianos
  Parâmetros
    y: Argumento da função. Deve ser um valor entre -1 e 1
  Exemplos
    x:=ArcSin(0);         // retorna 0.00000
    x:=ArcSin(1);         // retorna 1.57080
    x:=ArcSin(-0.5);      // retorna -0.52360
    x:=ArcSin(1.1);       // retorna wscMissValue
    x:=ArcSin(-1.1);      // retorna wscMissValue
    x:=ArcSin(wscMissValue); // retorna wscMissValue
}
begin
  if (y>=-1) and (y<=1) and not IsMissValue(y) then
    Result := ArcSin(y)
  else
    Result := wscMissValue;
end;

function ScalarAng(const y: Double): Double;
{ Objetivo
    Retorna (em graus) a transformacao angular do argumento, ou ArcSen(Raiz(argumento))
  Parâmetro
    y: Argumento da função. A raiz quadrada de y deve ser um valor entre 0 e 1.
}
begin
  if not IsMissValue(y) then
    begin
    Result := ScalarArcSin(ScalarSqrt(y));
    if not IsMissValue(Result) then
      Result:=Result*180/Pi
    end
  else
    Result:=wscMissValue
end;

function ScalarTan(const y: Double): Double;
{ Objetivo
    Retorna tangente do argumento
  Parâmetro
    y: Argumento para função. Deve ser especificado em radianos e não é um múltiplo impar
      de Pi/2
  Exemplos
    x:=ScalarTan(0.5);         // retorna 1.73205
    x:=ScalarTan(0);           // retorna 0.00000
    x:=ScalarTan(Pi/3);        // retorna 0.54630
    x:=ScalarTan(wscMissValue);   // retorna wscMissValue
    x:=ScalarTan(3*Pi/2);      // retorna valor muitoalto mas nao gera excecao
}
begin
  if not IsMissValue(y) then
    try
      Result := Tan(y)
    except
      Result := wscMissValue
    end
  else
    Result := wscMissValue;
end;

function ScalarCos(const y: Double): Double;
{ Objetivo
    Retorna o cosseno do argumento
  Parâmetro
    y: valor do argumento expresso em radianos
  Exemplos
    x:=ScalarCos(0.5);         // retorna 0.87758
    x:=ScalarCos(0);           // retorna 1.00000
    x:=ScalarCos(Pi/3);        // retorna 0.50000
    x:=ScalarCos(wscMissValue);   // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    Result := Cos(y)
  else
    Result := wscMissValue;
end;

function ScalarSin(const y: Double): Double;
{  Objetivo
     Retorna a função seno do argumento
   Parâmetro
     y: Argumento da função, especificado em radianos
   Exemplos
     x:=ScalarSin(0.5);       // retorna 0.47943
     x:=ScalarSin(0);         // retorna 0.00000
     x:=ScalarSin(Pi/4);      // retorna 0.70711
     x:=ScalarSin(wscMissValue); // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    try
      Result := Sin(y)
    except
      Result := wscMissValue
    end
  else
    Result := wscMissValue;
end;

function ScalarSinH(const y: Double): Double;
{ Objetivo
    Retorna o seno hiperbólico do argumento, ou seja, o valor de
     (exp(argument )-exp(-argument))/2 .
  Parâmetro
    y: Argumento da função
  Exemplos
    x:=ScalarSinH(0);         // retorna 0.00000
    x:=ScalarSinH(1);         // retorna 1.17520
    x:=ScalarSinH(-1);        // retorna -1.17520
    x:=ScalarSinH(wscMissValue); // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    Result := SinH(y)
  else
    Result := wscMissValue
end;

function ScalarCosH(const y: Double): Double;
{ Objetivo
    Retorna o cosseno hiperbólico do argumento
  Parâmetro
    y: Argumento da função. Retorna o valor de (exp(y) +exp(-y))/2
  Exemplos
    x:=ScalarCosH(0);         // retorna 1.00000
    x:=ScalarCosH(-5);        // retorna 74.20995
    x:=ScalarCosH(0.5);       // retorna  1.12763
    x:=ScalarCosH(wscMissValue); // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    Result := CosH(y)
  else
    Result := wscMissValue
end;

function ScalarTanH(const y: Double): Double;
{ Objetivo
    Retorna a tangente hiperbólica do argumento.
  Parâmetro
    y: Argumento da função. A função retorna (exp(y)-exp(-y))/(exp(y)+exp(-y))   .
  Examples
    x:=ScalarTanH(0);          // retorna -0.46212
    x:=ScalarTanH(0.5);        // retorna 0.46212
    x:=ScalarTanH(-0.5);       // retorna 0.00000
    x:=ScalarTanH(wscMissValue);  // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    Result := TanH(y)
  else
    Result := wscMissValue
end;

Function ScalarArcSinH (const y: Double): Double;
{ Objetivo
    Retorna arco seno hiperbolico do argumento
  Parâmetro
    y. Argumento da função. Se y<=1 retorna 0

}
begin
  if not IsMissValue(y) then
    Result := ArcSinH(y)
  else
    Result := wscMissValue
end;

Function ScalarArcCosH (const y: Double): Double;
{ Objetivo
    Retorna arco cosseno hiperbolico do argumento
  Parâmetro
    y: Argumento da função. Se y<=1 retorna 0; se y=wscMissValue, retorna wscMissValue

}
begin
  if not IsMissValue(y) then
    Result := ArcCosH(y)
  else
    Result := wscMissValue
end;

Function ScalarArcTanH (const y: Double): Double;
{ Objetivo
    Retorna arco tangente hiperbolico do argumento
  Parâmetro
    y: Argumento da função. Se y<=1 retorna 0

}
begin
  if (Abs(y)>=1) and not IsMissValue(y) then
    Result := ArcTanH(y)
  else
    Result := wscMissValue
end;

function ScalarSqrt(const y:Double): Double;
{ Objetivo
    Retorna a raiz quadrada do argumento.
  Parâmetro
    y: Argumento da função. Deve ser não negativo
  Exemplo
    x:=ScalarSqrt(25);         // retorna 5
    x:=ScalarSqrt(4);          // retorna 2
    x:=ScalarSqrt(wscMissValue);  // retorna wscMissValue
    x:=ScalarSqrt(-1);         // retorna wscMissValue
}
begin
  if (y>=0) and (not IsMissValue(y)) then
    try
      Result := Sqrt(y);
    except
      Result := wscMissValue
    end
  else
    Result := wscMissValue
end;

function ScalarSqr(const y:Double): Double;
{ Objetivo
    Retorna o quadrado do argumento.
  Parâmetro
    y: Argumento da função.
  Exemplo
    x:=ScalarSqr(5);         // retorna 25
    x:=ScalarSqr(2);          // retorna 4
    x:=ScalarSqrt(wscMissValue);  // retorna wscMissValue
}
begin
  if not IsMissValue(y) then
    try
      Result := Sqr(y);
    except
      Result := wscMissValue
    end
  else
    Result := wscMissValue
end;

function ScalarInv(const y: Double): Double;
{ Objetivo
    Retorna o inverso do argumento.
  Parâmetro
    y: Argumento da função. Deve ser diferente de zero
  Exemplo
    x:=ScalarInv(2);          // retorna 0.5
    x:=ScalarInv(4);          // retorna 0.25
    x:=ScalarSqrt(wscMissValue); // retorna wscMissValue
}
begin
  if (y<>0) and not IsMissValue(y) then
    try
      Result := 1/y
    except
      Result := wscMissValue
    end
  else
    Result := wscMissValue
end;

{ ======== Funcoes booleanas ============== }
function ScalarEQ(const x, y: Double): Double;
{ Retorna 1 se x=y; 0 caso contrario }
begin
  if x=y then Result := ScalarTrue else Result := ScalarFalse
end;

function ScalarLT(const x, y: Double): Double;
{Retorna 1 se x<y; 0 caso contrario. Se um dos valores for valor perdido, ele será o menor}
var
  x1, x2: Double;
begin
  if IsMissValue(x) then x1:=-MaxDouble else x1 := x;
  if IsMissValue(y) then x2:=-MaxDouble else x2 := y;
  if x1<x2 then Result := ScalarTrue else Result := ScalarFalse
end;

function ScalarLE(const x, y: Double): Double;
{ Retorna 1 se x<=y; 0 caso contrario. Se um dos valores for valor perdido, ele será o menor }
var
  x1, x2: Double;
begin
  if IsMissValue(x) then x1:=-MaxDouble else x1 := x;
  if IsMissValue(y) then x2:=-MaxDouble else x2 := y;
  if x1<=x2 then Result := ScalarTrue else Result := ScalarFalse
end;

function ScalarGT(const x, y: Double): Double;
{Retorna 1 se x>y; 0 caso contrario. Se um dos valores for valor perdido, ele será o menor}
var
  x1, x2: Double;
begin
  if IsMissValue(x) then x1:=-MaxDouble else x1 := x;
  if IsMissValue(y) then x2:=-MaxDouble else x2 := y;
  if x1>x2 then Result := ScalarTrue else Result := ScalarFalse
end;

function ScalarGE(const x, y: Double): Double;
{Retorna 1 se x>=y; 0 caso contrario. Se um dos valores for valor perdido, ele será o menor}
var
  x1, x2: Double;
begin
  if IsMissValue(x) then x1:=-MaxDouble else x1 := x;
  if IsMissValue(y) then x2:=-MaxDouble else x2 := y;
  if x1>=x2 then Result := ScalarTrue else Result := ScalarFalse
end;

function ScalarNE(const x, y: Double): Double;
{ Retorna 1 se x^=y; 0 caso contrario }
begin
  if x<>y then Result := ScalarTrue else Result := ScalarFalse
end;

end.
