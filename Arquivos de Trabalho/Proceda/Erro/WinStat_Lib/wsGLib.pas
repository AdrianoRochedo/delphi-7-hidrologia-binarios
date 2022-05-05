unit wsGlib;

{
}

Interface
uses classes,
     wsConstTypes,
     SysUtilsEx;

procedure RInc(var y: Double; const x: Double);
procedure SwitchF(var x, y: Double);
procedure SwitchW(var x, y: Word);
procedure GL(var Ind: PWArray; k, n: Word);
function  Compare(const x, y: Double; Asc: Boolean): Longint;
function  Sign(a, b: Double): Double;
function  MaxF(a, b: Double): Double;
function  MinF(a, b: Double): Double;
function  MaxArray(const x: array of Double): Double;
function  MinArray(const x: array of Double): Double;

function  Fuzz(x: Double): Double;
function  sf(n: LongInt): LongInt;
function  sl(n : LongInt) : LongInt;
function  sw(n: Word): Word;
function  WriteTextFile(var WordFile: Text; FileToWrite: PChar): Boolean;
function  OpenTextFile(var WordFile: Text; const FileToRead: String): Boolean;
function  ChPos(S: PChar; Ch: Char; First: Boolean; var Index: Integer): PChar;
function  StrGet(P: PChar; var J: Integer; Var DelChar: TCharSet): string;
function  GetToken(P, S: PChar; var J: Integer; Var DelChar:TCharSet): PChar;
function  StrGetF(var F:Text; Last: Char; var EndSec: boolean; Var DelChar:TCharSet): string;
function  CharToStrList(s: PChar; Var DelChar: TCharSet): TStrings;
function  StrListToChar(SL: TStringList; P: PChar; Start, Amt, LineLen: Word): PChar;
function  Compress(P, Temp: PChar; Var DelChar:TCharSet): PChar;
Function  SepToUnd(Const S:String; Var DelChar:TCharSet):String;
function  StrCompress(const P: String; Var DelChar:TCharSet): String;
function  Alltrim(Const S: String; DelChar: TCharSet): String;
function  ToCopy(Start, Amt, MaxLen: Word): Word;
function  FileExists(FileName: PChar): Boolean;
function  GetIndx(W: PWArray; n1, n2: Word): PWArray;
function  GetFloat(F: PFArray; n: Word; k: Double): PFArray;
function  GetWord(F: PWArray; n, k: Word): PWArray;
function  GetShort1(n: Word): PSIArray;
function  GL1(m, k, n: Word): Word;

function  Center(S, Temp: PChar; Field: Word): PChar;
function  PadLeft(S, Temp: PChar; Field: Word; Ch: Char): PChar;
function  PadRight(S, Temp: PChar; Field: Word; Ch: Char): PChar;
function  PadX(S, Temp: PChar; Amt, Start: Word; Ch: Char): PChar;

function  Constr(ch: Char; n: byte): string;
function  Pad(CONST s: string; n: byte): string;
function  LeftPad(CONST s: string; n: byte): string;
function  StrCenter(CONST s: string; w: byte): string;
function  Tab(n: byte): string;

function  FracToReal(frac : String) : Double;

Function  StringRead(Var F:File):String;
Function  ByteRead(Var F:File):Byte;
Function  WordRead(Var F:File):Word;
Function  LeDado(Var Arq:Text):String;
Function  ValMiss(Const s:String): Double;
Function  StrToStrList(Const S:String): TStrings;
Function  StrListToStr(L:TStringList):String;
Function  FloatToString(Const x:Double):String;
(*
function ToStrF(const x: Extended; Fmt: TFloatFormat=ffGeneral; Prec: Integer=12;
  Dig: Integer=8): String;
*)
Function  IsMissValue(Const x: Extended): Boolean;
Function  Fequals(const x1, x2: Double): Boolean;
function NomeFormula(Const s: String): String;

{ Funcoes usadas pelo Roger }
function  obtem_token(exp:string;var pos:Integer;var tok:string):byte;
Function  FloatToWord(n:Double):Longint;
{Function  RetiraExt(s:String):String;}
Function  CharToTStrings(PC:PChar):TStrings;

function  GetUntilChar(Str1, Str2: PChar; Ch: Char; var nc: Word; var Finish: Boolean): PChar;
function StrUntilChar(Str: String; Ch: TCharSet; var lc: Integer): string;

function NameVecKronecker(X, Y: TStrings): TStrings;
function NameTermKron(C: TList): TStrings;

Function del(s1,s2:string):string;
Function EstaContido(str1,str2 :string):boolean;
function Pertence(s1,s2 :string):boolean;
Function Posi(i,j,n: Integer): Integer;

Implementation
uses Math,
     SysUtils,
     wsFuncoesDeEscalares, //ScalarDiv
     wsVars;

function sl(n : LongInt) : LongInt;
  begin
    sl := n*SizeOf(LongInt)
  end;

function Compare(const x, y: Double; Asc: Boolean): Longint;
{ Compare compara x e y. Para Ascd = True retorna 1 se x>y, 0 se x=y
  ou -1 se x<y; inverte os sinais caso contrario }
begin
  if Asc then begin
    if x > y then
      Compare := 1
    else
      if x < y then
        Compare := -1
      else
        Compare := 0
  end
  else begin
    if x > y then
      Compare := -1
    else
      if x < y then
        Compare := 1
      else
        Compare := 0
  end
end;


{ ================  Uso Geral  ================== }

function sf(n: LongInt): LongInt;
{ Retorna o numero de bytes necessarios para armazenar um Double }
begin
  sf := n*SizeOf(Double)
end; { sf }

function sw(n: Word): Word;
begin
  sw := n*SizeOf(Word)
end; { sw }

function Sign(a, b: Double): Double;
{ Retorna o valor de a com o sinal de b }
begin
  if b >= 0.0 then Result := Abs(a) else Result := -Abs(a)
end; { Sign }

function MaxF(a, b: Double): Double;
{ Retorna o maior entre a e b }
begin
  if a > b then Result := a else Result := b
end; { MaxF }

function MinF(a, b: Double): Double;
{ Retorna o menor entre a e b }
begin
  if a < b then Result := a else Result := b
end; { MinF }

function  MaxArray(const x: array of Double): Double;
var
  i: Integer;
begin
  Result:=x[0];
  for i:=1 to High(x) do
    if x[i]>Result then
      Result:=x[i]
end;

function  MinArray(const x: array of Double): Double;
var
  i: Integer;
begin
  Result:=x[0];
  for i:=1 to High(x) do
    if x[i]<Result then
      Result:=x[i]
end;

function Fuzz(x: Double): Double; { ?????? }
{
FUZZ
  Objetivo
    Retorna o inteiro mais próximo se o argumento difere desse inteiro em menos de
    FuzzVal (1.0E-12), isto é, se a diferença absoluta entre o inteiro e o argumento é
    menor que FuzzVal; caso contrário, retorna o argumento.
  Exemplos
    x=fuzz(5.9999999999999) retorna 6.000000000000000;
    x=fuzz(5.99999999) retorna 5.999999990000000;
    x=fuzz(0.0000000000001) retorna 0
    x=fuzz(5.0000000000001) retorna 5
    x=fuzz(-5.9999999999999) retorna -6.000000000000000;
}
var
  y,z: Double;
begin
  z:=Abs(x);
  y:=Int(z);
  if Frac(z) >= 0.5 then
    y:=y+1;
  if Abs(y-z) < wsvFuzzVal then
    if x>0 then
      Result := y
    else
      Result := -y
  else
    Result := x
end;

function FracToReal(frac : String) : Double;
{ conversao de string na forma n ou n/m para numerico fazer tratamento de erro
  de transformacao nesse local utilizar excecao do delphi }
var
  s1, s2 : string;
  valor1, valor2 : Double;
begin
  {adg e Rochedo. 01/06/1998}
  frac := SysUtilsEx.AllTrim(frac);
  Try
    If SubStrings('/', S1, S2, frac) <> 0 Then
      begin
      valor1 := strToFloat(s1);
      valor2 := strToFloat(s2);
      Result := ScalarDiv(valor1, valor2)
      end
    Else
      if frac = SysUtils.DecimalSeparator then
         Result := wscMissValue
      else
         Result := strToFloat(frac);
  Except
    Result := wscMissValue;
  End;
end;

Function StringRead(Var F: File): String;
Begin
  {$IFDEF WIN32}
  SetLength(Result, ByteRead(F));
  {$ELSE}
  Result[0] := chr(ByteRead(F));
  {$ENDIF}
  BlockRead(F, Result[1], Length(Result));
End; { StringRead }

Function ByteRead(Var F: File): Byte;
Begin
  BlockRead(F, Result, 1);
End; { ByteRead }

Function WordRead(Var F: File): Word;
Begin
  BlockRead(F,Result,2);
End; { WordRead }

function PadX(S, Temp: PChar; Amt, Start: Word; Ch: Char): PChar;
{ Retorna uma copia de S com Amt caracteres Ch a partir da posicao Start.
  Observe-se que a primeira posicao e 0. }
var
  i, NewLen: Word;
  C: array[0..1] of Char;
begin
  C[0] := Ch;
  NewLen := StrLen(S) + Amt;
  StrLCopy(Temp, S, Start);
  for i := 1 to Amt do StrLCat(Temp, C, StrLen(Temp) + 1);
  StrLCat(Temp, S + Start, NewLen);
  PadX := Temp
end;

function PadRight(S, Temp: PChar; Field: Word; Ch: Char): PChar;
{ Retorna uma copia de S ajustado para a direita dentro do campo Field,
  com os caracteres a esquerda preenchidos com Ch. Retorna S com os
  primeiros Field caracteres se o espaco nao for suficientemente grande }
var
  i, L: Word;
  C: array[0..1] of Char;
begin
  C[0] := Ch;
  L := StrLen(S);
  Temp[0] := #0;
  if L < Field then
    for i := 1 to Field - L do StrLCat(Temp, C, StrLen(Temp) + 1);
  StrLCat(Temp, S, Field);
  PadRight := Temp
end;

function StrListToChar(SL: TStringList; P: PChar; Start, Amt, LineLen: Word): PChar;
{ Retorna Amt strings de SL em P, comecando por Start}
var
  i: Word;
  Q, Aux: TCharStr;
begin
  StrPCopy(Aux, SL.Strings[Start-1]);
  StrCopy(P, PadRight(Aux, Q, LineLen, #32));
  for i := 0 to Amt-2 do
    StrCat(P, PadRight(StrPCopy(Aux, SL.Strings[Start+i]), Q, LineLen, #32))
end;

function Center(S, Temp: PChar; Field: Word): PChar;
{ Retorna uma copia de S centrado dentro do campo Field. Retorna o proprio
  S se Field nao  tiver espaco suficiente }
var
  L, L1: Word;
begin
  L := StrLen(S);
  if L < Field then begin
    L1 := (Field - L) div 2;
    PadRight(S, Temp, Field - L1, ' ')
  end
  else
    StrLCopy(Temp, S, Field);
  Center := Temp
end;

function PadLeft(S, Temp: PChar; Field: Word; Ch: Char): PChar;
{ Retorna uma copia de S com Ch's inseridos no final ate completar Field.
  Retorna S com os primeiros Field caracteres se o espaco nao for suficiente }
var
  i, L: Word;
  C: array[0..1] of Char;
begin
  C[0] := Ch;
  L := StrLen(S);
  if L < Field then begin
    StrLCopy(Temp, S, L);
    for i := 1 to Field - L do StrLCat(Temp, C, StrLen(Temp) + 1);
  end
  else
    StrLCopy(Temp, S, Field);
  PadLeft := Temp
end; { PadLeft }

procedure RInc(var y: Double; const x: Double);
{ Retorna y:=y+x}
begin
  y:=y+x
end;

procedure SwitchF(var x, y: Double);
{ Troca os valores Double x e y }
var
  tmp: Double;
begin
  tmp := x;
  x := y;
  y := tmp
end; { SwitchF }

procedure SwitchW(var x, y: Word);
{ Troca os valores Word x e y }
var
  tmp: Word;
begin
  tmp := x;
  x := y;
  y := tmp
end; { SwitchW }

function FileExists(FileName: PChar): Boolean;
{ Boolean function that returns True if the file exists;otherwise,
  it returns False. Closes the file if it exists. }
var
  F: file;
begin
  {$I-}
  Assign(F, FileName);
  FileMode := 0;  { Set file access to read only }
  Reset(F);
  Close(F);
  {$I+}
  FileExists := (IOResult = 0) and (FileName <> nil);
end;  { FileExists }

function WriteTextFile(var WordFile: Text; FileToWrite: PChar): Boolean;
{ Retorna True se o arquivo FileToWrite for aberto com sucesso. A vari-
  avel arquivo retorna em WordFile }
begin
  Assign(WordFile, FileToWrite);
  ReWrite(WordFile);
  WriteTextFile := IOResult = 0
end; { WriteTextFile }

function OpenTextFile(var WordFile: Text; const FileToRead: String): Boolean;
{ Retorna True se o arquivo FileToRead for aberto com sucesso. A vari-
  avel arquivo retorna em WordFile }
begin
  Assign(WordFile, FileToRead);
  {$I-} Reset(WordFile); {$I+}
  OpenTextFile := IOResult = 0
end; { OpenTextFile }

function GetToken(P, S: PChar; var J: Integer; Var DelChar: TCharSet): PChar;
{ Retorna um ponteiro para a sequencia de caracteres a partir da posicao
  J delimitada por um caracter de Q. Em J retorna a posicao seguinte
  ao ultimo apontado por GetToken ou o final do string. Nesse ultimo caso,
  GetToken retorna nil }
var
  I, Len: Word;
begin
  Len := StrLen(P);
  while (P[J] in DelChar) and (J < Len) do Inc(J);
  I := J;
  while not (P[J] in DelChar) and (J < Len) do Inc(J);
  if I < J then
    StrLCopy(S, P + I, J - I)
  else
    S[0] := #0; { if }
  GetToken := S
end;

function StrGet(P: PChar; var J: Integer; Var DelChar: TCharSet): string;
var k: Integer;
begin
  k := StrLen(P);
  Result := '';
  While (P[J] in DelChar) and (J < k) do Inc(J);
  While Not (P[J] in DelChar) and (J < k) Do
    Begin
    Result := Result + P[J];
    Inc(J);
    End; { While }
end;

function CharToStrList(s: PChar; Var DelChar: TCharSet): TStrings;
var i: Integer;
    St: TCharStr;
begin
  i := 0;
  Result := TStringList.Create;
  repeat
    GetToken(s, st, i, DelChar);
    if st[0] <> #0 then Result.Add(String(st));
  until st[0] = #0;
end;

function StrGetF(var F: Text; Last: Char; var EndSec: boolean;Var DelChar:TCharSet):string;
{ Pega caracteres que nao estao em Del nem e o caracter que encerra. }
var
  C: Char;
  i: byte;
begin
  i := 0;
  Read(F, C);
  EndSec := False;
  while (C in DelChar) and (not EOF(F)) do Read(F, C);
  while not ((C = Last) or (C in DelChar) or (EOF(F))) do begin
    Inc(i);
    Result[i] := C;
    Read(F, C);
  end;

  {$IFDEF WIN32}
  SetLength(Result, i);
  {$ELSE}
  Result[0] := chr(i);
  {$ENDIF}

  if C = Last then EndSec := True
end;

function Compress(P, Temp: PChar; Var DelChar:TCharSet): PChar;
{ Retorna um PChar com todos os caracteres de P retirando aqueles que
  compoem Chars }
var
  Len,i,k,k1: Word;
begin
  k := 0;       { k e o numero de caracteres de P que pertencem a Chars }
  Len := StrLen(P);
  for i := 0 to Len do
    if (P[i] in DelChar) then Inc(k);
  if k > 0 then begin
    k1 := 0;
    for i := 0 to Len do
      if not (P[i] in DelChar) then begin
        Temp[k1] := P[i];
        Inc(k1)
      end;
    Compress := Temp;
  end
end; { Compress }

function StrCompress(const P: String; Var DelChar:TCharSet): String;
var
  i: byte;
begin
  Result:='';
  for i := 1 to Length(P) do
    if not (P[i] in DelChar) then
      Result := Result+P[i]
(*

  {$IFDEF WIN32}
  SetLength(Result, k)
  {$ELSE}
  Result[0] := chr(k);
  {$ENDIF}
*)
end;

Function SepToUnd(Const S:String; Var DelChar:TCharSet):String;
{ Transforma separador em underscore}
  Var
    i  :Byte;
  Begin
    Result:=S;
    For i:=1 to Length(S) Do
      If S[i] In DelChar Then Result[i]:='_';
  End;

function ChPos(S: PChar; Ch: Char; First: Boolean;
  var Index: Integer): PChar;
{ Retorna um ponteiro para a posicao que Ch ocupa em S ou nil se Ch
  nao for caracter de S. ChPos aponta da primeira posicao de Ch em S em
  diante. Index fornece o indice do caracter se Ch for caracter de S.
  Se First for True retorna a primeira posicao ocupada e se for False
  retorna a ultima }
var
  P: PChar;
begin
  if First then
    P := StrScan(S, Ch)
  else
    P := StrRScan(S, Ch);
  if P <> nil then Index := P - S + 1;
  ChPos := P
end;

function ToCopy(Start, Amt, MaxLen: Word): Word;
{Determina a quantidade de bytes necessarios para armazenar Amt caracteres
 iniciando em Start com um maximo de MaxLen }
begin
  ToCopy := Min(Amt, MaxLen - Start);
end;

function GetIndx(W: PWArray; n1, n2: Word): PWArray;
{ Obtem um array cujos componentes sao os numeros inteiros consecutivos de
  n1 ate n2. Em W^[0] retorna a quantidade de valores colocados no array }
var
  i, k: Word;
begin
  if n2 >= n1 then begin
    k := n2 - n1 + 1;
    W^[0] := k+1;
    k := 0;
    for i := n1 to n2 do begin
      Inc(k);
      W^[k] := i
    end;
    GetIndx := W
  end
end; { GetIndx }

function GetFloat(F: PFArray; n: Word; k: Double): PFArray;
var
  i: Word;
begin
  if F <> nil then
    for i := 1 to n do F^[i] := k;
  GetFloat := F
end; { GetFloat }

function GetWord(F: PWArray; n, k: Word): PWArray;
{ Retorna um array de inteiros onde cada componente vale k }
var
  i: Word;
begin
  if F <> nil then
    for i := 0 to n-1 do F^[i] := k;
  GetWord := F
end; { GetWord }

function GetShort1(n: Word): PSIArray;
var
  i: Word;
begin
  GetMem(Result, n);
  for i := 1 to n do Result^[i] := 1;
end;

function GL1(m, k, n: Word): Word;
{ Retorna um indice da analise da variacao }
begin
  GL1 := (m-1) div n mod k + 1
end; { GL }

procedure GL(var Ind: PWArray; k, n: Word);
{ Retorna um array com os indices da analise da variacao. Na entrada Ind^[0]
  deve conter o numero de componentes desejado para o vetor, que ja deve
  estar dimensionado na entrada. }
var
  ii: Word;
begin
  for ii := 1 to Ind^[0] do Ind^[ii] := GL1(ii, n, k)
end; { GL }

function Constr(ch: Char; n: byte): string;
{ Retorna um string com n caracteres ch }
begin
  {$IFDEF WIN32}
  Result := StringOfChar(Ch, n);
  {$ELSE}
  Result[0] := chr(n);
  FillChar(Result, n+1, ch);
  {$ENDIF}
end; { Constr }

function Pad(CONST s: string; n: byte): string;
{ Retorna um string ajustado a esquerda num campo de tamanho n. Se o tamanho do string for
  maior que n trunca em n }
begin
  if n <= Length(s) then
     Result := System.Copy(s, 1, n)
  else
     Result := s + Constr(' ', n - Length(s));
end; { Pad }

function LeftPad(CONST s: string; n: byte): string;
begin
  if n <= Length(s) then
     Result := System.Copy(s, 1, n)
  else
     Result := Constr(' ', n - Length(s)) + s
end; { LeftPad }

function StrCenter(CONST s: string; w: byte): string;
var
  n: byte;
begin
  n := (w div 2) - (Length(s) div 2);
  StrCenter := Constr(' ', n) + s
end;

function Tab(n: byte): string;
begin
  Tab := Constr(' ', n)
end;

Function LeDado(Var Arq:Text):String;
Const SEP=';';
Var
  Dado  :String;
  c     :Char;
Begin
  Dado:='';
  Read(Arq,c);
  If EOLn(Arq) Then Dado := Dado+c;
  While (c<>SEP) And (Not EOLn(Arq)) Do
    Begin
    Dado := Dado+c;
    Read(Arq, c);
    End;
  LeDado := Dado;
End; { LeDado }


  {   ----------------- Função Obtém_Token ------------------------------------

      Objetivo:
                Obtém o próximo token da string.

      Descrição:
                Esta função recebe uma string e devolve o próximo token a partir
                da posição passada. O token é então devolvido. Foi implementada
                com o autômato finito cuja tabela de transição de estados pode
                ser vista no projeto aval.

      Entradas:
                exp ->  String que contém a espressão.
                pos ->  Posição onde deve começar o token.

      Saídas:
                tok ->  Token montado.
                obtem_token  -> 0 se sucesso;
                                1 se erro.

  }
  function obtem_token(exp:string;var pos:integer;var tok:string):byte;
  var p,                               { Posição do caracter a ser lido }
      state  :byte;                    { Estado do Autômato Finito }
    begin
      p:=pos-1;
      state:=0;
      repeat
        inc(p);
{        exp[p]:=upcase(exp[p]);}
        case state of
          0  :begin
                case exp[p] of
                  ' '                :begin
                                      end;
                  'A'..'Z',
                  'a'..'z'           :state:=1;
                  '0'..'9'           :state:=2;
                  '+','&','=','(',
                  ')',';','.',',',
                  '"'                :state:=6;
                  '*'                :state:=7;
                  '#'                :state:=8;
                  '@'                :state:=9;
                  '/'                :state:=10;
                  '|'                :state:=11;
                  '<'                :state:=12;
                  '-'                :state:=13;
                  '>'                :state:=14;
                  '^'                :state:=15;
                  else
                    state:=16;
                  end;
                if exp[p]<>' ' then
                  tok:=exp[p];
              end;
          1  :case exp[p] of
                  'A'..'Z','a'..'z','0'..'9'  :tok:=tok+exp[p];
                  else
                    state:=5;
                  end;
          2  :case exp[p] of
                 '0'..'9'            :tok:=tok+exp[p];
                 '.'                 :begin
                                        tok:=tok+exp[p];
                                        state:=3;
                                      end;
                 else
                   state:=5;
                 end;
           3  :case exp[p] of
                 '0'..'9'            :begin
                                        tok:=tok+exp[p];
                                       state:=4;
                                      end;
                 else
                   state:=16;
                 end;
           4  :case exp[p] of
                 '0'..'9'            :tok:=tok+exp[p];
                 else
                   state:=5;
                 end;
           5  :begin
                 pos:=p-1;
                 obtem_token:=0;
                 exit;
               end;
           6  :begin
                 pos:=p;
                 obtem_token:=0;
                 exit;
              end;
           7  :case exp[p] of
                 '*'  :begin
                         tok:=tok+exp[p];
                         state:=6;
                       end;
                 else
                   state:=5;
                 end;
           8  :case exp[p] of
                 '#'  :begin
                         tok:=tok+exp[p];
                         state:=6;
                       end;
                 '/'  :begin
                         tok:=tok+exp[p];
                         state:=6;
                       end;
                 else
                   state:=5;
                 end;
           9  :case exp[p] of
                 '@'  :begin
                         tok:=tok+exp[p];
                         state:=6;
                       end;
                 else
                   state:=5;
                 end;
           10  :case exp[p] of
                  '/'  :begin
                          tok:=exp[p];
                          state:=6;
                        end;
                  else
                    state:=5;
                  end;
           11  :case exp[p] of
                  '|'  :begin
                          tok:=tok+exp[p];
                          state:=6;
                       end;
                  else
                    state:=5;
                  end;
           12  :case exp[p] of
                  '>'  :begin
                          tok:=exp[p];
                          state:=6;
                       end;
                  '='  :begin
                          tok:=exp[p];
                          state:=6;
                       end;
                  else
                    state:=5;
                  end;
            13  :case exp[p] of
                   '/',
                   '*' :begin
                          tok:=tok+exp[p];
                          state:=6;
                       end;
                   else
                    state:=5;
                   end;
            14  :case exp[p] of
                   '<'  :begin
                           tok:=tok+exp[p];
                           state:=6;
                       end;
                   '='  :begin
                           tok:=exp[p];
                           state:=6;
                       end;
                   else
                     state:=5;
                   end;
            15  :case exp[p] of
                   '='  :begin
                           tok:=tok+exp[p];
                           state:=6;
                       end;
                  else
                    state:=5;
                  end;
          16  :begin
                 pos:=p;
                 obtem_token:=1;
                 exit;
               end;
          end;
      until FALSE;
    end; { obtem_token }

Function FloatToWord(n: Double): Longint;
Begin
  FloatToWord := Trunc(n);
End; { TDataSet.FloatToWord }

Function ValMiss(Const s: String): Double; {Rochedo, 12/05/1998}
Begin
  Try
    Result := StrToFloat(s);
  Except
    Result := wscMissValue;
  End;
End; { ValMiss }

Function StrToStrList(Const S:String): TStrings;
  Const
    DelChar: TCharSet = [#9,#10,#13,' ',',','=','\', '"','(',')','[',']'];
  Var
    R  : String;
    I  : Cardinal;
  Begin
    If S<>''
      Then
        Begin
        Result := TStringList.Create;
        I:=1;
        Repeat
          R:=StrToken(S,I, DelChar);
          If R<>'' Then
            Result.Add(R);
        Until R='';
        End { If S<>'' }
      Else
        Result:=TStringList.Create;  { Colocar Nil quando a função do Rochedo (ModelFrame) for atualizada - Roger - 17/04/97}
  End; { StrToStrList }

Function StrListToStr(L:TStringList):String;
  Var
    i  :Word;
  Begin
  Result:='';
  If L.Count>0
    Then
      For i:=0 To L.Count-1 Do
        Result:=Result+' '+L.Strings[i];
  End; { StrListToStr }

Function FloatToString(Const x: Double): String;
Begin
  If IsMissValue(x) Then
     Result := '.'
  Else
     Result := FloatToStr(x);
End; { FloatToString }

(*
function ToStrF(const x: Extended; Fmt: TFloatFormat=ffGeneral; Prec: Integer=12;
  Dig: Integer=8): String;
begin
  if IsMissValue(x) then
    FloatToStrF(xMin,Fmt,Prec,Dig)
  else
    Result:='.'
end;
*)
Function IsMissValue(const x: Extended): Boolean;
begin
  Result := FEquals(x, wscMissValue)
end;

function NomeFormula(Const s: String): String;
var i: Integer;
begin
  Result := SysUtilsEx.AllTrim(s);
  i := System.Pos(':=', Result);
  if i > 0 then
    begin
    if Result[1]='[' then
      Result := System.Copy(Result, 2, i-1)
    else
      Result := System.Copy(Result, 1, i-1)
    end
end;

function GetUntilChar(Str1, Str2: PChar; Ch: Char; var nc: Word; var Finish: Boolean): PChar;
{ Pega caracteres até encontrar o fim da string ou encontrar o caractere Ch.
  Começa a procura a partir da posição nc.
  No final nc fica apontando para a posição seguinte aquela ocupada por Ch.
  Str2 contém os characteres da posição nc até a posição anterior
  onde está o caractere Ch (Ou seja, Ch não é retornado em Str2.).
  No final GetUntilChar aponta para o mesmo local de Str2, que deve estar adequadamente
  dimensionada na entrada.
}
var
  C : Char;
  i : Longint;
begin
  i := 0;
  c := Str1[nc];
  while (C <> #0) and (C <> ch) do begin
    Str2[i] := str1[nc];
    Inc(nc);
    Inc(i);
    c := Str1[nc];
  end;
  Finish := c = #0;
  Str2[i] := #0;
  Inc(nc);
  GetUntilChar := Str2
end; { GetUntilChar }

function StrUntilChar(Str: String; Ch: TCharSet; var lc: Integer): string;
{ Objetivo
    Retorna um sub-string de um string delimitado por um caracter
  Parâmetros
    Str: String do qual retornará o sub-string
    Ch : Caracteres delimitadores do sub-string
    lc : Posição a partir da qual iniciará o sub-string. Na saída conterá a posição ocupada por um
         outro Ch ou Length(str)
  Exemplo
    StrUntil('A1.A223.A',['.'],4) retornará A223
}
var
  ic,ac,stlen: Integer;
  Sai        : Boolean;
begin
  stLen:= Length(Str);
  if lc<=stLen then
    begin
    ic:=lc;
    // Pula todos os caracteres de Ch primeiro
    while (Str[lc] in Ch) and (lc<=stLen) do Inc(lc);
    Sai:=lc>stLen;
    while (not Sai) do
      begin
      if (not (Str[lc] in Ch)) and (lc<stLen) then Inc(lc);
      Sai:=(Str[lc] in Ch) or (lc=stLen);
      end;
    if (Str[lc] in Ch) then ac:=lc-1 else ac:=lc;
    Result:=System.Copy(Str,ic,ac);
    Inc(lc)
    end
  else
    Result:='';
end; // StrUntilChar


Function CharToTStrings(PC:PChar):TStrings;

{ Converte PChar em Lista de Strings. A cada 255 caracteres, uma nova string é gerada.
}

  Var
    s    :String; { Armazena temporariamente a string gerada }
    i,            { Apontador de caracter no PChar }
    j    :Word;   { Apontador de caracter na String }
  Begin
  { Inicialização }
  Result:=TStringList.Create;
  s:='';
  i:=0;
  j:=0;
  { Laço Principal }
  While PC[i]<>#0 Do
    Begin
    s:=s+PC[i];
    Inc(i);
    Inc(j);
    { A cada 255 caracteres, coloca a string na lista e reinicializa }
    If j=255 Then
      Begin
      Result.Add(s);
      s:='';
      j:=0;
      End; { If }
    End; { While }
  { Se ainda restou algo na string, adiciona }
  If s<>''
    Then
      Result.Add(s);
  End; { CharToTStrings }

Function Alltrim(Const S: String; DelChar: TCharSet): String;
Var I, F, Len: Longint;
Begin
  If S = '' Then Exit;

  If (Length(S)=1) And (S[1] in DelChar) Then Begin Result := ''; Exit; End;

  i := 1; Len := Length(S);
  While (S[I] in DelChar ) and (I < Len) do Inc(I);

  If( I <= Len) Then
    Begin
    F := Len;
    While (S[F] in DelChar) and (F > 1  ) do Dec(F);
    Result := System.Copy(S, I, F-I+1);
    End
  Else
    Result := '';
End;

function NameVecKronecker(X, Y: TStrings): TStrings;
{ Objetivo
    Retorna uma lista concatenando todos os elementos das listas especificadas
  Parâmetros
    X: Lista com elementos colocados no início
    Y:  Lista com elementos colocados no final
  }
var i, j: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to X.Count-1 do
    for j := 0 to Y.Count-1 do
      Result.Add(X[i] + '.' + Y[j]);
end; { NameVecKronecker }

{ Objetivo
    Retorna todos os strings que compõem cada elemento da lista especificada
  Parâmetros
    C: Lista com as listas de strings para concatenação }
function NameTermKron(C: TList): TStrings;
var i, j: Integer;
    Temp: TStrings;
begin
  if C.Count = 1 then
     begin
     Temp := TStrings(C.Items[0]);
     Result := TStringList.Create;
     for i := 0 to Temp.Count-1 do Result.Add(Temp[i]);    { Copia para o resultado }
     Exit
     end
  Else
     begin
     Result := NameVecKronecker(C.Items[0], C.Items[1]);     { Entre os dois primeiros }
     if C.Count > 2 then
        begin
        Temp := TStringList.Create;
        for i := 0 to Result.Count-1 do Temp.Add(Result[i]);    { Copia para o resultado }
        for i := 2 to C.Count - 1 do
          begin { Completa o produto com os demais vetores }
          Result := NameVecKronecker(Temp, C.Items[i]);
          Temp.Free;
          Temp := TStringList.Create;
          for j := 0 to Result.Count-1 do Temp.Add(Result[j]);    { Copia para o resultado }
          end; { for }
        Temp.Free;
        end; { if C.Count }
     end;
end; { NameTermKron }

Function  FEquals(const x1, x2: Double): Boolean;
begin
  // para x1 = x2 --> x1-x2 tem que tender a zero
  // Considera como iguais os valores cuja diferenca, em modulo, e menor que 1.0e-11
  Result := Abs(x1 - x2) < 1E-11;
end;

Function Posi(i,j,n: Integer): Integer;
{ Objetivo
    Obtém a posição de um arranjo retangular arrumado num array através de um empilhamento
    das linhas.
  Parâmetros
    i, j: Índice da linha e coluna do elemento
    n   : Número de colunas do arranjo retangular.
  Exemplo
    No arranjo retangular a11 a12 a13 a14
                          a21 a22 a23 a24
                                             1   2   3   4   5   6   7   8
    arrumado como colunas empilhadas fica (a11 a12 a13 a14 a21 a22 a23 a24)
    o elemento a21 ocupa no array a posicao 1+(2-1)*4=5
}
Begin
  Result := j + (i-1) * n;
End; { Posi }

function Pertence(s1,s2 :string):boolean;
{ Objetivo
    Dada uma seqüência de caracteres separada pelo separador '.', verifica se uma seqüência
    faz parte da outra. Seu maior objetivo é verificar se um termo peternce a outro.
  Parâmetros
    s1: Seqüência que poderá estar contida
    s2: Seqüência que poderá conter
  Exemplo
    Pertence(A,A.B) retorna True e Pertence(C,A.B) retorna False
}
const
  Sep: TCharSet = ['.', ' '];
var
  st: string;
  Final: Boolean;
  j: Cardinal;
begin
  Final := False;
  Result := False;
  j := 1;
  s1 := UpperCase(s1);
  s2 := UpperCase(s2);
  while not Final do
    begin
    st := StrToken(s2, j, Sep);
    Result := s1 = st;
    Final := Result or (st = '')
    end;
end; { Pertence }

Function EstaContido(str1,str2 :string):boolean;
{ Objetivo
    Pesquisa se todas as seqüências de caracteres separadas por um caracter '.' estão
    presentes em uma outra. Retorna True se todas as sequencias pesquisadas estiverem
    contidas na outra. Seu objetivo maior é verificar se um termo está contido em outro.
  Parâmetros
    str1: Desta seqüência serão retiradas as partes que estiverem estar contidas na outra
    str2: Seqüência que poderá conter as seqüências
  Exemplo
    EstaContido(A.B, A.B.C) e EstaContido(A.C, A.B.C) retornam True enquanto que
    EstaContido(A.C, A.B.D) retorna False
}
Const
  DelChar: TCharSet = ['.',' '];
var
  s1,s2    :string;
  FazParte :boolean;
  i,j      :Cardinal;
begin
  i:=1;
  s1:=' ';
  FazParte := TRUE;
  str1:=UpperCase(str1);
  str2:=UpperCase(str2);
  while (s1<>'') and FazParte do
    begin
    s1 := StrToken(str1,i,DelChar);
    if s1 <> '' then
       begin
       j := 1;
       FazParte := FALSE;
       while (j<=Length(str2)) and (FazParte=FALSE) do
         begin
         s2:=StrToken(str2,j,DelChar);
         FazParte := (s1 = s2);
         end;
       end;
    end;
  EstaContido := FazParte;
end; { EstaContido }

Function del(s1,s2:string):string;
{ Objetivo
    Elimina uma seqüência de outra
  Parametros
    s1: Seqüência da qual serão eliminados os caracteres
    s2: Seqüência de caracteres a eliminar
}
begin
  s1 := UpperCase(s1);
  s2 := UpperCase(s2);
  Delete(s1, System.pos(s2,s1), Length(s2));
  Result := s1;
end; { del }

end. { Unit GLib }
