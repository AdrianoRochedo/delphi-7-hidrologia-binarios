Unit wsCVec;

Interface
Uses Classes,
     SysUtilsEx,
     wsGLib,
     wsConstTypes,
     wsVec;

Type
  TwsCVec = class(TObject)
  private
    FLen: Integer;   { Número de caracteres de Str}
    FStr: AnsiString;

    function  GetChar(i: Integer): AnsiChar;
    procedure PutChar(i: Integer; Ch: AnsiChar);
    procedure SetStr(Const S: string);
  public
    procedure Insert(Const S: string; Start: Integer);
    procedure Delete(Start, Amt: Integer);
    procedure Append(Const S: string);
    procedure Compress(DelChar: TCharSet);
    procedure LTrim(DelChar: TCharSet);
    procedure RTrim(DelChar: TCharSet);
    procedure GetLine(var F: TextFile);
    procedure GetUntil(var F: TextFile; Last: Char; DelChar:TCharSet);

    function  Copy(Start, Amt: Integer): String;
    function  Pos(Const S: string): Integer;
    function  CharPos(Ch: Char; First: Boolean; var Index: Integer): string;

    function  ToFVec: TwsVec; // <<<<<

    function  StrUntilChar(Del: TCharSet; var n: Integer): string;
    Function  SearchLast(Var F: TextFile; Last: Char; N: Integer): Boolean;
    function  StringToStrings(DelChar: TCharSet): TStrings;

    {CUIDADO: Não confundir com a função StrGet de GLib que funciona com PCHAR !!}
    function  StrGet(var J: Integer; Var DelChar: TCharSet): string;

    property Char[Index: Integer]: AnsiChar read GetChar write PutChar; default;
    property Len: Integer read FLen write FLen;
    property Str: AnsiString read FStr write SetStr;
  end;

  function StringToCVec(Const S: string): TwsCVec;
  function CVecToString(C: TwsCVec): string;
  function GetUntilChar(P: TwsCVec; Del: TCharSet; var nc: Integer): TwsCVec;

Implementation
Uses SysUtils;

{ =================  TwsCVec  ==================== }

procedure TwsCVec.SetStr(Const S: AnsiString);
begin
  If S <> FStr Then
     begin
     FStr := S;
     FLen := Length(FStr);
     end;
end; { SetStr }

function TwsCVec.GetChar(i: Integer): AnsiChar;
begin
  Try
    Result := FStr[i];
  Except
    Result := #0;
  End;
end;

procedure TwsCVec.PutChar(i: Integer; Ch: AnsiChar);
begin
  Try
    FStr[i] := Ch;
  Except
    {nada}
  End;
end;

function TwsCVec.Copy(Start, Amt: Integer): string;
begin
  Result := System.Copy(FStr, Start, Amt);
end; { TwsCVec.Copy }

procedure TwsCVec.Insert(Const S: AnsiString; Start: Integer);
{ Insere o string S em FData iniciando em Start. Se FData nao possuir nenhum
  valor entao sera inicializado com S. Start e a posicao do caracter onde sera
  feita a insercao: 0, 1, 2, etc. }
begin
  System.Insert(S, FStr, Start);
  FLen := Length(FStr);
end; { Insert }

procedure TwsCVec.Delete(Start, Amt: Integer);
begin
  System.Delete(FStr, Start, Amt);
  FLen := Length(FStr);
end; { Delete }

procedure TwsCVec.Append(Const S: AnsiString);
begin
  FStr := FStr + S;
  FLen := Length(FStr);
end; { TwsCVec.Append }

procedure TwsCVec.Compress(DelChar: TCharSet);
var i: Integer;
begin
  i := 1;
  If FLen > 0 Then
     While Str[i] <> #0 do
        begin
        If Str[i] in DelChar Then
           begin
           Delete(i, 1);
           Continue;
           end;
        inc(i);
        end;
end; { Compress }

procedure TwsCVec.LTrim(DelChar: TCharSet);
{ Retira todos os caracteres de Chars do inicio de FData }
var k, i: Integer;
begin
  k := 0;
  For i := 1 to FLen do
    If (Str[i] in DelChar) Then Inc(k) Else Break;

  Delete(1, k);
end; { LTrim }

procedure TwsCVec.RTrim(DelChar: TCharSet);
{ Retira todos os caracteres de Chars do final de FData }
var k, i: Integer;
begin
  k := 0;
  For i := FLen DownTo 1 do
    If Str[i] in DelChar Then inc(k) Else Break;

  Delete(Flen - k, k);
end; { RTrim }

function TwsCVec.Pos(Const S: AnsiString): Integer;
begin
  Result := System.Pos(S, Str);
end;

function TwsCVec.CharPos(Ch: Char; First: Boolean; var Index: Integer): AnsiString;
{ Retorna um ponteiro para a posicao que Ch ocupa em FData ou nil se Ch
  nao for caracter de FData. CharPos aponta da primeira posicao de Ch
  em FData em diante. Index fornece o indice do caracter ou e negativo
  se Ch nao for caracter de FData. Se First for True retorna a primeira
  posicao ocupada e se for False retorna a ultima }
begin
  CharPos := String(ChPos(PChar(Str), Ch, First, Index));
end;

procedure TwsCVec.GetLine(var F: TextFile);
{ GetLine le uma linha (delimitada por CR/LF) do arquivo F e a coloca em
  FData }
const
  Buffer = 1024;
var
  S: array[0..Buffer] of AnsiChar;
begin
  while not EoLn(F) do
    begin
    Read(F, S);
    Append(String(S))
    end; { while }
  ReadLn(F)
end; { GetLine }

procedure TwsCVec.GetUntil(var F: TextFile; Last: Char; DelChar:TCharSet);
{ GetUntil le caracteres do arquivo texto F ate encontrar um caracter presente
  em D e a coloca em Data }
const
  BufSize = 253; {O valor correto eh 253 - 14/8/97}
var
  Buf: TCharStr;
  i: Integer;
  Ch: AnsiChar;
begin
  i := 0;
  repeat
    Read(F, Buf[i]);
    Ch := Buf[i];
    if (i >= BufSize) or (Ch = Last) or EoF(F) then
       If Not (Buf[i] in DelChar) Then
          Begin
          Buf[i+1] := #0;
          Append(String(Buf));
          i := 0;
          End
        Else
          Begin
          Buf[i] := #0;
          Append(String(Buf));
          i := 0;
          End
    Else
      If Not (Buf[i] in DelChar) Then Inc(i);

  until (Ch = Last) or EoF(F);
end; { GetUntil }

function TwsCVec.ToFVec: TwsVec;
{ Quando FData e uma linha de caracteres numericos, separados por delimitadores
  presentes em Chars, retorna-os num objeto TLine. }
const
  BufSize = 150;
  //DelChar: TCharSet = [#9,#10,#13,' ','=','\', '"','(',')'];
var
  sP: String;
  F: PFArray;
  J: Integer;
  Size: Integer;
begin
  J := 0;
  Size := 0;
  GetMem(F, sf(BufSize));
  Result := TwsDFVec.Create(0);
  repeat
    sP := wsGLib.StrGet(pChar(Str), J, DelChar);
    if sP <> '' then
      begin
      Inc(Size);
      F^[Size] := FracToReal(sP)
      end;

    if (Size = BufSize) or (sP = '') then
      begin
      Result.Append(F, Size);
      Size := 0
      end;
  until (sP = '') or (ValCode <> 0);
  FreeMem(F, sf(BufSize));
end;

function TwsCVec.StrUntilChar(Del: TCharSet; var n: Integer): AnsiString;
{ Objetivo:
    Retorna um string iniciando a pesquisa numa posicao determinada e parando
    quando encontra um caracter pertencente ao conjunto especificado.
  Parametros:
    Del: Conjunto de caracteres delimitadores. A sequencia de caracteres e incorporada
    ate que seja encontrado um caracter desse conjunto.
    n: Posicao inicial de pesquisa dos caracteres. Se a pesquisa for feita a partir
    do inicio, a primeira posicao deve ser estabelecida como 1.
  Exemplo: Se a sequencia de onde os caracters serao retirados for 'FEM  6  8 10' e se
  Del=[''] e n=1 então o retorno sera 'FEM' e n=4
}
begin
  Result := '';
  While (n < FLen) and (FStr[n] in Del) do Inc(n);
  While (n <= FLen) and not (FStr[n] in Del) do {Amauri 140999}
    begin
    Result := Result + FStr[n];
    Inc(n);
    end;
//  Inc(n); {Vai um caracter além do primeiro caracter delimitador encontrado}
end; { StrUntilChar }

function TwsCVec.StringToStrings(DelChar: TCharSet): TStrings;
{ Objetivo
    Retorna uma lista de substrings separados por um dos caracteres esepcificados
  Parametros
    DelChar: Conjunto de caracteres delimitadores de cada substring
}
var i:  Integer;
    St: TCharStr;
begin
  i := 0;
  Result := TStringList.Create;
  repeat
    wsGLib.GetToken(pChar(FStr), st, i, DelChar);
    if st[0] <> #0 then Result.Add(String(st));
  until st[0] = #0;
end;

Function TwsCVec.SearchLast(Var F: TextFile; Last: Char; N: Integer): Boolean;
{ Procura o caracter Last no arquivo F. Se este aparecer nos primeiros N caracteres, encerra
  retornando True. Caso contrario, retorna False. Pode ser utilizado antes de GetUntil.
  Cuidado: O arquivo é resetado ao seu incício }

var i: Integer;
    Ch: AnsiChar;
begin
  i := 0;
  While (Ch <> Last) And (i < N) And (Not EOF(F)) Do
    Begin
    Read(F, Ch);
    Inc(i);
    End; { While }

  Result := (Ch = Last);

  CloseFile(F);
  Reset(F);
End; { TwsCVec.SearchLast }

function TwsCVec.StrGet(var J: Integer; Var DelChar: TCharSet): AnsiString;
//var k: Integer;
begin
  Result := '';
  While (J <= FLen) and (FStr[J] in DelChar) do Inc(J);
  While (J <= FLen) and Not (FStr[J] in DelChar) Do
    Begin
    Result := Result + FStr[J];
    Inc(J);
    End; { While }
end;

{ ============================== Funções =============================== }

function StringToCVec(Const S: AnsiString): TwsCVec;
{ Transforma P (que e copiado) num TwsCVec }
begin
  Result := TwsCVec.Create;
  Result.Str := S;
end; { CharToCVec }

function CVecToString(C: TwsCVec): AnsiString;
begin
  Result := C.Str;
end;

function GetUntilChar(P: TwsCVec; Del: TCharSet; var nc: Integer): TwsCVec;
{ Pega caracteres até encontrar o fim de P ou encontrar o caractere Ch.
  Começa a procura a partir da posição nc.
  Na saida nc fica apontando para a posição seguinte aquela ocupada por Ch.
  Result contém os caracteres da posição nc até a posição anterior onde está o
  caractere Ch (Ou seja, Ch não é retornado em Result).).
}
const
  BufSize = 1024;
var
  Buf: pChar;
  C: Char;
  i : Integer;
begin
  i := 0;
  GetMem(Buf, BufSize+1);                { Buffer de ate BufSize caracteres }
  Result := TwsCVec.Create;
  repeat
    C := P[nc];
    if (i = BufSize) or (C in Del) then
      begin
      Buf[i] := #0;
      Result.Append(String(Buf));
      i := 0;
      end
    else
      begin
      Buf[i] := C;
      Inc(i);
      Inc(nc)
      end;
  until (C in Del) or (nc>P.Len);
  Inc(nc);
  FreeMem(Buf, BufSize+1);
end; { GetUntilChar }

End. { Unit CVec }
