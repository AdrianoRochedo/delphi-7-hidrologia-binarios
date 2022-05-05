unit wsParser;

interface
uses Classes,
     SysUtils;

  Type
  CharSet    = Set Of Char;

  Type
  TTokenType = (ttInteger, ttFloat, ttFunction, ttSymbol, ttComment,
                ttLogicOperator, ttAritOperator, ttVariable, ttKeyWord, ttNull);

  TToken = Class
  Private
    FType : TTokenType;
    FText : String;                   {Representação Interna}
    Function GetFloat: Extended;
    Function GetString: String;
    Function GetInteger: Integer;
  Public
    Constructor Create(Const Token: String; TokenType: TTokenType);

    Property TokenType: TTokenType Read FType;
    Property AsString: String Read GetString;
    Property AsInteger: Integer Read GetInteger;
    Property AsFloat: Extended Read GetFloat;
  End;

  TAbstractParser = class
  private
    FFunctions:  TStrings;  {Sen, Cos, Goto}
    FKeyWords:   TStrings;  {If, Then, Begin}
    FText:       TStrings;  {Texto a ser analisado}
    FVariables:  TStrings;  {Variáveis que foram encontradas}
    FTokens:     TList;     {Lista de Tokens depois da chamada de Scan}
    FPos:        Integer;   {Posição do Token Atual}

    // Deverao estar definidos antes do "Scan"
    FAritOpers:  TStrings;  {Conterá a lista de Operadores aritmeticos}
    FLogicOpers:  TStrings;  {Conterá a lista de Operadores Logicos}

    Procedure SetToken(Index: Integer; Token: TToken);
    Function  GetToken(Index: Integer): TToken;
    Function  GetTokensCount: Integer;
    Procedure ClearTokens;
    Procedure Initialize;
    Function  Analise_Lexica(Const Expr: String): Integer;
  protected
    function  Automaton {Parser's Brain}
       (Const Expr: String; Var Pos: Integer; Var Token: String): Boolean; Virtual; Abstract;
  public
    Constructor Create();
    Destructor Destroy(); Override;

    Procedure Scan();
    Procedure ReplaceToken(i: Integer; Const S: String; TokenType: TTokenType);
    Function  GotoToken(n: Integer): TToken;
    Function  MakeString(): String;
    Function  Next(): TToken;
    Function  Prev(): TToken;
    Function  MoveBy(Delta: Integer): TToken;

    Property Variables:  TStrings Read FVariables;
    Property TokensCount: Integer Read GetTokensCount;

    Property Token[Index: Integer]: TToken Read GetToken Write SetToken;

    Property Functions:  TStrings Read FFunctions  Write FFunctions;
    Property KeyWords:   TStrings Read FKeyWords   Write FKeyWords;
    Property Text:       TStrings Read FText       Write FText;
    Property AritOpers:  TStrings Read FAritOpers  Write FAritOpers;
    Property LogicOpers: TStrings Read FLogicOpers Write FLogicOpers;
  end {TAbstractParser};

  TExpressionParser = Class(TAbstractParser)
    Function Automaton  {Autômato de expressões Matemáticas}
      (Const Expr: String; Var Pos: Integer; Var Token: String): Boolean; Override;
  End;

implementation

{---------------------- TToken -----------------------}

Constructor TToken.Create(Const Token: String; TokenType: TTokenType);
Begin
  Inherited Create;
  FText := Token;
  FType := TokenType;
End;

Function TToken.GetFloat: Extended;
Begin
  Try
    Result := StrToFloat(FText);
  Except
    Raise Exception.Create('Erro de Conversão' + #13#10 +
    'Token não pode ser convertido para um Valor em Ponto Flutuante (Real)');
  End;
End;

Function TToken.GetString: String;
Begin
  Result := FText;
End;

Function TToken.GetInteger: Integer;
Begin
  Try
    Result := StrToInt(FText);
  Except
    Raise Exception.Create('Erro de Conversão' + #13#10 +
    'Token não pode ser convertido para um Inteiro');
  End;
End;

{---------------------- TAbstractParser -----------------------}

Constructor TAbstractParser.Create();
Begin
  Inherited Create();
  FPos := -1;
  FFunctions := TStringList.Create;
  FKeyWords  := TStringList.Create;
  FText      := TStringList.Create;
  FTokens    := TList.Create;
  FAritOpers := TStringList.Create;
  FLogicOpers := TStringList.Create;
  FVariables := TStringList.Create;
  TStringList(FVariables).Duplicates := dupIgnore;
  TStringList(FVariables).Sorted := True;
End;

Destructor TAbstractParser.Destroy();
Begin
  FFunctions.Free;
  FKeyWords.Free;
  FVariables.Free;
  FText.Free;
  FLogicOpers.Free;
  FAritOpers.Free;  
  ClearTokens;
  FTokens.Free;
  Inherited Destroy;
End;

Procedure TAbstractParser.ClearTokens;
Var i: Integer;
Begin
  For i := 0 to FTokens.Count-1 do
    TToken(FTokens[i]).Free;
  FTokens.Clear;
End;

Procedure TAbstractParser.Scan;
Var i : Integer;
Begin
  Initialize;
  For i := 0 to FText.Count-1 do
    Analise_Lexica(FText[i]+ #0);
End;

Function TAbstractParser.Analise_Lexica(Const Expr: String): Integer;
var Pos    : Integer;
    Erro   : Integer;

   Function Lexico(Var Pos: Integer; Var Erro: Integer): Boolean;
   var Token: String;

     Function IsFunction(Const Token: String): Boolean;
     var
       i: Integer;
     begin
       Result := FALSE;
       for i := 0 to FFunctions.Count-1 do
         if CompareText(Token, FFunctions[i]) = 0 then
            Begin
            Result := TRUE;
            Break;
            End;
     end; { IsFunction }

     Function IsKeyWord(Const Token: String): Boolean;
     var
       i: Integer;
     begin
       Result := FALSE;
       for i := 0 to FKeyWords.Count-1 do
         if CompareText(Token, FKeyWords[i]) = 0 then
            Begin
            Result := TRUE;
            Break;
            End;
     end; { IsKeyWord }

     Function getTokenType(Const Token: String): TTokenType;
     begin
       Result := ttNull;

       if Token[1] in ['_', 'a'..'z', 'A'..'Z'] then
          begin
          Result := ttVariable;
          FVariables.Add(Token);
          end

       else if Token[1] in ['0'..'9'] then
          if System.Pos('.', Token) = 0 then
             Result := ttInteger
          else
             Result := ttFloat

       else if Token[1] in ['(', ')', ';'] Then
          Result := ttSymbol

       else if (Token[1] = '{') Then
          Result := ttComment

       else If (Token[1] = #0) Then
          // Final da String

       else if FLogicOpers.IndexOf(Token) > -1  then
          Result := ttLogicOperator

       else if FAritOpers.IndexOf(Token) > -1  then
          Result := ttAritOperator

       else if IsFunction(Token) then
          Result := ttFunction

       else if IsKeyWord(Token) then
          Result := ttKeyWord

       else
          Raise Exception.CreateFmt('Tipo do Elemento desconhecido: "%s"', [Token]);
     end; { TipoToken }

  Begin { Lexico }
    if Automaton(Expr, Pos, Token) then
       begin
       FTokens.Add(TToken.Create(Token, getTokenType(Token)));
       Result := True;
       end
    else
       Result := False;
  End; { Lexico }

Begin {Analise_Lexica}
  Pos := 1;
  Erro := 0;
  While (Pos <= Length(Expr)) and (Erro = 0) Do Lexico(Pos, Erro);
  Result := Erro;
End; { Analise_Lexica }

Function TAbstractParser.GotoToken(n: Integer): TToken;
Begin
  Result := Nil;
  If (n > -1) and (n < FTokens.Count-1) Then
    Begin
    FPos := n;
    Result := TToken(FTokens[FPos]);
    End;
End;

Procedure TAbstractParser.ReplaceToken(i: Integer; Const S: String; TokenType: TTokenType);
Begin
  Try
    TToken(FTokens[i]).Free;
    FTokens[i] := TToken.Create(S, TokenType);
  Except
    Raise Exception.Create('Método <ReplaceToken>.'+#13#10+'Índice do Token Inválido');
  End;
End;

Function TAbstractParser.MakeString: String;
Var i: Integer;
Begin
  Result := '';
  For i:= 0 to FTokens.Count-1 do
    Result := Result + TToken(FTokens[i]).AsString;
End;

Function  TAbstractParser.Next: TToken;
Begin
  Inc(FPos);
  Try
    Result := TToken(FTokens[FPos])
  Except
    FPos := FTokens.Count-1;  {Último}
    Result := Nil;
  End;
End;

Function  TAbstractParser.Prev: TToken;
Begin
  Dec(FPos);
  Try
    Result := TToken(FTokens[FPos])
  Except
    FPos := 0; {Primeiro}
    Result := Nil;
  End;
End;

Function  TAbstractParser.MoveBy(Delta: Integer): TToken;
Var Aux: Integer;
Begin
  Aux := FPos;
  FPos := FPos + Delta;
  Try
    Result := TToken(FTokens[FPos]);
  Except
    FPos := Aux;
    Result := Nil;
  End;
End;

Procedure TAbstractParser.SetToken(Index: Integer; Token: TToken);
Begin
  TToken(FTokens[Index]).Free;
  FTokens[Index] := Token;
End;

Function  TAbstractParser.GetToken(Index: Integer): TToken;
Begin
  FPos := Index;
  Result := TToken(FTokens[Index]);
End;

Function  TAbstractParser.GetTokensCount: Integer;
Begin
  Result := FTokens.Count;
End;

Procedure TAbstractParser.Initialize;
Begin
  FVariables.Clear;
  ClearTokens;
End;

{-------------- Parser's Brain ---------------}

Function TExpressionParser.Automaton {Autômato de Expressões Matemáticas}
  (Const Expr: String; Var Pos: Integer; Var Token: String): Boolean;

var p, State: Byte;
Begin
  Token := '';
  p := Pos-1;
  State := 0;
  Repeat
    inc(p);
    case State of
      0  :begin
            case expr[p] of
              ' '                :{Nada};
              #0                 : State := 6; {Final}

              'A'..'Z',
              'a'..'z', '_'      :state := 1;

              '0'..'9'           :state := 2;

              '+','&','=','(',
              ')',';'            :state := 6;

              '*'                :state := 7;
              '#'                :state := 8;
              '@'                :state := 9;
              '/'                :state := 10;
              '|'                :state := 11;
              '<'                :state := 12;
              '-'                :state := 13;
              '>'                :state := 14;
              '^'                :state := 15;
              else
                 state := 16;
              end;
            if Expr[p] <> ' ' then Token := Expr[p];
          end;

      1  :case expr[p] of
              'A'..'Z', 'a'..'z',
              '0'..'9', '_'           :Token := Token + Expr[p]; {26/12/1997} {Rochedo}
              '.'                     :begin
                                       Token := Token + Expr[p];
                                       state := 17;
                                       end;
              else
                state := 5;
              end;

      2  :case Expr[p] of
             '0'..'9'            :Token := Token + Expr[p];
             '.'                 :begin
                                  Token := Token + Expr[p];
                                  state := 3;
                                  end;
             else
               state:=5;
             end;

       3  :case Expr[p] of
             '0'..'9'            :begin
                                  Token := Token + Expr[p];
                                  state := 4;
                                  end;
             else
               state := 16;
             end;

       4  :case Expr[p] of
             '0'..'9' :Token := Token + Expr[p];
             else
                state := 5;
             end;

       5  :begin
           pos := p-1;
           Result := True;
           exit;
           end;

       6  :begin
           pos := p;
           Result := True;
           exit;
           end;

       7  :case Expr[p] of
             '*'  :begin
                   Token := Token + Expr[p];
                   state := 6;
                   end;
             else
               state := 5;
             end;

       8  :case Expr[p] of
             '#'  :begin
                   Token := Token + Expr[p];
                   state := 6;
                   end;
             '/'  :begin
                   Token :=Token + Expr[p];
                   state := 6;
                   end;
             else
               state := 5;
             end;

       9  :case Expr[p] of
             '/'  :begin
                   Token := Token + Expr[p];
                   state := 6;
                   end;
             else
               state := 5;
             end;

       10  :case Expr[p] of
              '/'  :begin
                    Token := Token + Expr[p];
                    state := 6;
                    end;
              else
                state := 5;
              end;

       11  :case Expr[p] of
              '|'  :begin
                    Token := Token + Expr[p];
                    state := 6;
                   end;
              else
                 state := 5;
              end;

       12  :case Expr[p] of
              '>'  :begin
                    Token := Token + Expr[p];
                    state := 6;
                    end;
              '='  :begin
                    Token := Token + Expr[p];
                    state := 6;
                    end;
              else
                 state := 5;
              end;

        13  :case Expr[p] of
               '/'  :begin
                     Token :=Token + Expr[p];
                     state := 6;
                   end;
               else
                  state := 5;
               end;

        14  :case Expr[p] of
               '<'  :begin
                     Token := Token + Expr[p];
                     state := 6;
                     end;
               '='  :begin
                     Token := Token + Expr[p];
                     state := 6;
                     end;
               else
                  state := 5;
               end;

        15  :case Expr[p] of
               '='  :begin
                     Token := Token + Expr[p];
                     state := 6;
                   end;
              else
                 state := 5;
              end;

      16  :begin
             Pos := p;
             {Erro}
             Raise Exception.CreateFmt(
               'Expressão: "%s" Posição %d'#13 +
               'Erro: Elemento não reconhecido', [Expr, Pos-1]);
           end;

      17  :Case Expr[p] of
             'A'..'Z',
             'a'..'z', '_': begin
                            Token := Token + Expr[p];
                            state := 1;
                            end;
             else
                State := 16; {Erro}
             end;

      End;
  Until FALSE;
End; { TExpressionParser.Automaton }

end.
