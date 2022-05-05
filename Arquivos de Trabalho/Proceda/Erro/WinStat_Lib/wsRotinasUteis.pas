unit wsRotinasUteis;

{     Modificacoes:

        05/09/96 - Inclusao de ShowError                           - Roger
        23/09/96 - Inclusao de Rotinas do Adriano                  - Roger
        16/12/96 - Inclusão da Rotina ModelFrame()                 - Rochedo
                   Inicio:  16/12/96
                   Termino: ??/??/??
        23/12/96 - Alteração das Rotinas: MatBooleanEvaluation
                                          MatExpressionEvaluation
        27/12/96 - Conclusão da Rotina:   ModelFrame
        15/01/97 - Debug na Rotina:  ModelFrame
        29/01    - Iclusao de Bin e Bit                            - Roger e Rochedo
        03/02    - Transferencia de ModelFrame para
                   ModelFunc                                       - Roger
        25/04/97 - Conclusão da Rotina:  DSColGenerate             - Rochedo
        02/06/97 - Conclusão da Rotina:  DSCondiction
                   Rotinas <ModelFrame, DSColGenerate, DSCondiction>
                   movidas para [UDTFunc]                          - Rochedo
        05/06    - Retirada referencia a EntrMat da
                   clausula uses na parte de implementacao
                 - Retirada das funcoes de uso geral para
                   SysUtilsEx                                        - Roger e Fernanda
        30/06    - Transferencia de procedimentos da TabVar
                   (FillListWithVars,FillListWithMatrix)
                 - Inclusão de ColsDescriptionToMemo,
                   FillListWithColsDs                              - Roger e Rochedo
        20/11    - Inclusão de Espaco                              - Roger
        05/01/98 - Inclusão de ObtemNomeTemp                       - Roger
        05/11/98 - Mudança de nome: ShowError() --> ShowExcetion() - Rochedo
        14/12/98 - Exclusão de ObtemNomeTemp                       - Rochedo
                 - Exclusão de Espaço                              - Rochedo
        14/6/00  - Testa a existência de vírgula em edit box       - Zeh
        15/01/01 - Adição de uma rotina Chama help com 6 frames    - Leonardo
        03/10/02 - Adição da rotina SteamLeafToXML                 - Rochedo
}


interface
Uses Forms, WinProcs, Classes, WinTypes, StdCtrls, Graphics,
     Controls, ExtCtrls, Dialogs,
     wsMatrix,
     wsConstTypes,
     wsBXML_Output,
     wsTabelaDeSimbolos;

  Function  MatTypeToString(MatType : TwsEnumMatType): String;

  Function MatBooleanEvaluation  (Const Condiction: String; FMV : Boolean;
                                        Mat: TWSGeneral): TWSGeneral;

  Function MatExpresionEvaluation(Expressions: TStringList; Mat: TWSGeneral): TWSGeneral;

  Procedure FillListWithVars(SL: TStrings;
                             Tab: TwsTabVar;
                             Const Typ: Array of TwsEnumVarType;
                             ShowLabels: Boolean = false);

  Procedure FillListWithMatrix(SL: TStrings;
                               TabVar:TwsTabVar;
                               Const Typ: Array of TwsEnumMatType;
                               ShowLabels: Boolean = false);

  Procedure ColsDescriptionToMemo(SL: TStrings; Col:TWSDataSetCol);
  Procedure FillListWithColsDs(SL: TStrings; DS: TWSDataSet);
  Procedure FillListWithRowsDs(SL: TStrings; DS: TWSDataSet);
  Procedure FillListWithColsMatrix(SL:TStrings; M:TwsMatrix);

  procedure ChamaHelp(const aTitle, aTopLeft, aMiddLeLeft, aCentral, aBottonLeft, aBottonRight: string);
  procedure WinStatHelp(aCentral: string);

  function  testa_Virg(S: String; Key: Char) : Char;

  // Converte uma definicao de StemLeaf para uma definicao em XML.
  // Esta definicao em XML é colocada dentro de um bloco BXML que geralmente é um bloco de Texto
  procedure StemLeafToXML(StemLeaf: TStrings; Output: TwsBXML_Output);

  // Retorna o nome + Label formatados
  function ObterIdentificacao(M: TwsMatrix): String;

Var Virg :  Boolean;
implementation
Uses SysUtils,
     SysUtilsEx,
     wsVec,
     wsGLib,
     wsAvaliadorDeExpressoes,
     ShellAPI;


procedure ChamaHelp(const aTitle, aTopLeft, aMiddLeLeft, aCentral, aBottonLeft, aBottonRight: string);
{ Objetivo
    Rotina que carrega arquivos HTML para o sistema de ajuda do WinStat. Cada arquivo
    corresponde a um frame (regiao) da interface do sistema de ajuda.
  Parâmetros
    aTitle:        arquivo para a região superior esquerda. No sistema de ajuda corresponde
                   ao título
    aTopLeft:      arquivo para a regiao esquerda abaixo de aTitle. No sistema de ajuda
                   corresponde aos tópicos principais
    aMiddleLeft:   arquivo para a região esquerda abaixo de aTopLeft. No sistema de ajuda
                   corresponde às letras do glossário
    aCentral:      arquivo para a parte superior direita. No sistema de ajuda corresponde
                   à área reservada ao texto principal (tópicos)
    aBottonLeft:   arquivo para a parte inferior esquerda. No sistema de ajuda corresponde
                   às palavras do glossário
    aBottoonRight: arquivo para a parte inferior direita. No sistema de ajuda corresponde
                   ao texto do glossário.
       _______________________________
      |aTitle       |                 |
      |ATopLeft     |   aCentral      |
      |aMiddleLeft  |                 |
      |aBottonLeft  |   aBottonRight  |
       -------------------------------
}

var a : TStrings;
    App_dir, Help_dir,Title, TopLeft,MiddleLeft,Central,BottonLeft,BottonRight : string;
begin

  //---Cria uma Lista de Strings---\\
  a := TStringList.Create;

  //---Atribui diretórios para as variáveis--\\
  App_dir    := ExtractFilePath(Application.ExeName);
  Help_dir   := App_dir + 'Ajuda\';
  Title      := App_dir +'Ajuda\' + aTitle;
  MiddleLeft := App_dir +'Ajuda\' + aMiddleLeft;
  TopLeft    := App_dir +'Ajuda\' + aTopLeft;
  Central    := App_dir +'Ajuda\' + aCentral;
  BottonLeft := App_dir +'Ajuda\' + aBottonLeft;
  BottonRight:= App_dir +'Ajuda\' + aBottonRight;

  //---Adiciona na Lista de Strings o código Html, com os 6 frames---\\

  a.add('<html>');
  a.add('<head>');
  a.add('<title>Sistema de Ajuda WinStat</title>');
  a.add('<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
  a.add('</head>');
  a.add('<frameset cols="245,759" rows="*">');
  a.add('  <frameset rows="115,519" cols="*">');
  a.add('    <frame src="' + title + '" name="" frameborder="NO" scrolling="NO">');
  a.add('    <frame src="' + TopLeft + '" name="Topicos" frameborder="NO" scrolling="AUTO" bordercolor="#CCCC99">');
  a.add('  </frameset>');
  a.add('  <frameset rows="525,109" cols="*" frameborder="NO">');
  a.add('    <frame src="' + central + '" name="central" frameborder="NO" bordercolor="#CCCC99">');
  a.add('    <frame src="dir_inf.htm" name="dirinf" frameborder="YES" bordercolor="#CCCC99">');
  a.add('  </frameset>');
  a.add('</frameset>');
  a.add('<noframes>');
  a.add('<body bgcolor="#FFFFFF" text="#000000">');
  a.add('</body>');
  a.add('</noframes>');
  a.add('</html>');

 //---Salva o conteúdo da Lista de String para o arquivo Index_Dinamico.htm---\\
  a.SaveToFile(Help_dir + 'index_dinamico.htm');

  //---Chama o arquivo Index_Dinamico.htm no browser padrão---\\
  if ShellExecute(Application.Handle, 'open', PChar(App_dir + '\ajuda\index_dinamico.htm'), Nil, Nil, SW_SHOWNORMAL) <= 32 then
     MessageDlg('Erro ao tentar abrir o arquivo de ajuda', mtError, [mbOk], 0);

  //---Libera a memória alocado pela Lista de Strings---\\
  a.Free;
end;

procedure WinStatHelp(aCentral: string);
begin
  ChamaHelp('Titulo.htm','Topicos.htm','Indice_Gloss.htm',aCentral,'Cont_Glossario.htm',
    'Dir_Inf.htm');
end;

Function RealToStr(R : Real; d : Byte) : String;
var S : String;
Begin
  Str(R:0:d,S);
  Result := S;
End;

Function MatTypeToString(MatType : TwsEnumMatType): String;
Begin
  Case MatType Of
    mtGeneral     : Result := 'Geral';
    mtSymmetric   : Result := 'Simétrica';
    mtTriangular  : Result := 'Triangular';
    mtDiagonal    : Result := 'Diagonal';
    mtToeplitz    : Result := 'Toeplitz';
    mtVandermonde : Result := 'Vandermonde';
  End;
End;

{CG - Número de Colunas que estão sendo geradas}
Procedure AtualizaVariaveis(Tab: TwsTabVar; M : TWSGeneral; i : Integer; CG: Integer);
Var J : Integer;
     S : String;
Begin
   For j := 1 to (M.NCols - CG) do Begin
     S := M.CName.Strings[j-1];
     Tab.SetFloatValue(S,M[i,j]);
   End;
End;

Function FindMissValue(V: TwsVec; Index: TwsLIVec): Boolean;
Var i: Integer;
Begin
  Result := False;
  i := 1;
  While Not Result And (i <= Index.Len) do Begin
    Result := wsGLib.IsMissValue(V[Index[i]]);
    Inc(i);
  End;
End;

{Dada uma matriz e um conjunto de condições:
 Ex: Condiction = 'C1 > 0 && C2 > C1 + C2'
 Retornamos somente as linhas que estão de acordo com a condição dada,
 numa outra matriz, que é o resultado.}
Function MatBooleanEvaluation (Const Condiction: String; FMV : Boolean;
                               Mat: TWSGeneral): TWSGeneral;
Var Indices:TwsLIVec;
    Linha : TWSVec;
    Col, i, VA, Pos, Erro : Integer;
    Eval : TAvaliator;
Begin
  Eval := TAvaliator.Create;
  Try
    {Pega os nomes das colunas}
    For i := 0 to Mat.CName.Count-1 do
      Eval.TabVar.AddFloat(Mat.CName.Strings[i], 0);

    {Verifica se não ha erro na condição}
    Try
      Eval.Expression := Condiction;
    Except
      Result := Nil;
      Raise;
    End;

    Result := TWSGeneral.Create(0, Mat.NCols);
    Try
      {Indices = Vetor que contém os índices das colunas que serão copiadas}
      VA := Eval.TabVar.AccessedVars;
      Indices := TwsLIVec.Create(VA);
      Try
        Pos := 1;
        For i := 0 to Eval.TabVar.NVars-1 do
          If Eval.TabVar.Vars[i].Accessed Then Begin
             Result.CName.Add(Eval.TabVar.Vars[i].Name);
             Indices[Pos] := i+1;
             Inc(Pos);
          End;

        {Análise Booleana da Matriz passada}
        For i := 1 to Mat.NRows do Begin
          If FMV Then
            If FindMissValue(Mat.Row[i], Indices) Then Continue;

          AtualizaVariaveis(Eval.TabVar, Mat, i, 0);
          If Eval.Evaluate.AsFloat = Byte(TRUE) Then Begin
             {Insere a linha na matriz resultante}
             Linha := TwsDFVec.Create(Mat.NCols);
             For Col := 1 to Mat.NCols do
               Linha[Col] := Mat[i,Col];
             Result.MAdd(Linha);
          End; {If}
        End; {For i}

        Result.CName.Assign(Mat.CName);

      Finally
        Indices.Free;
      End;

    Except
      Result.Free;
      Result := Nil;
    End;
  Finally
    Eval.Free;
  End;
End;

{Dada uma matriz e um conjunto de Expressões:
 Ex: Expressions = 'Log(C1), Sen(C1 + C2), C1 * Cos(C3)'
 Retonaremos neste exemplo as colunas C1 = Log(C1), C2 = Sen(C1 + C2),
 C3 = C1 * Cos(C3). Preservando o número de linhas da matrix original}
Function MatExpresionEvaluation(Expressions: Classes.TStringList; Mat: TWSGeneral): TWSGeneral;
Var OldTabs, EvalList : TList;
    TempTab : TwsTabVar;
    i, j : Integer;
    Eval : TAvaliator;
Begin
  OldTabs := TList.Create;
  TempTab := TwsTabVar.Create;

  {Pega os nomes das colunas}
  For i := 0 to Mat.CName.Count-1 do
    TempTab.AddFloat(Mat.CName.Strings[i], 0);

  {Verifica se não ha erro na condição}
  Eval := TAvaliator.Create;
  OldTabs.Add(Eval.TabVar);
  Eval.TabVar := TempTab;
  For i := 0 to Expressions.Count-1 do Begin
    Try
      Eval.Expression := Expressions.Strings[i];
    Except
      On E: Exception do Begin
         ShowMessage(Format('Erro na Expressão <%s>. %s.',[Expressions.Strings[i], E.Message]));
         TempTab.Free;
         Eval.TabVar := OldTabs.Items[0];
         Eval.Free;
         OldTabs.Free;
         Result := Nil;
         Exit;
      End;
    End;
  End;

  If Expressions.Count > 1 Then Begin
     EvalList := TList.Create;
     EvalList.Add(Eval);
     Eval.Expression := Expressions.Strings[0];
     For i := 1 to Expressions.Count-1 do Begin
       Eval := TAvaliator.Create;
       OldTabs.Add(Eval.TabVar);
       Eval.TabVar := TempTab;
       Eval.Expression := Expressions.Strings[i];
       EvalList.Add(Eval);
     End;
  End;

  Try
    Result := TWSGeneral.Create(Mat.NRows, Expressions.Count);

    {Calcula e passa os novos valores para a matriz resultante}
    For j := 1 to Mat.NRows do Begin
       AtualizaVariaveis(TempTab, Mat, J, 0);
       For i := 0 to Expressions.Count-1 do
         Result[j, i+1] := TAvaliator(EvalList[i]).Evaluate.AsFloat;
    End ; {For i}

    For i := 0 to Expressions.Count-1 do
      Result.CName.Strings[i] := Expressions.Strings[i];

  Finally
    If Expressions.Count > 1 Then Begin
       TAvaliator(EvalList[i]).TabVar := OldTabs[i];
       TAvaliator(EvalList[i]).Free;
    End Else Begin
       Eval.TabVar := OldTabs[0];
       Eval.Free;
    End;
    OldTabs.Free;
    TempTab.Free;
  End;
End;

Function IsLogicalOperator(Const S: String): Boolean;
Begin
  Result := False;
  If (S = '=') or (S = '<') or (S = '<=') or (S = '<') or (S = '<=') or (S = '<>') Then
     Result := True;
End;

Procedure FillListWithVars(SL: TStrings;
                           Tab: TwsTabVar;
                           Const Typ: Array of TwsEnumVarType;
                           ShowLabels: Boolean = false);
Var i : Word;
    Tipos : Set of TwsEnumVarType;
    s: String;
Begin
  SL.Clear;
  Tipos := [];
  For i := Low(Typ) to High(Typ) do
     Include(Tipos, Typ[i]);

  If Tab.NVars > 0 Then
    With Tab do
      For i := 0 to NVars-1 do
        If (Vars[i].VarType in Tipos) or (vtAll in Tipos) Then
           begin
           s := Vars[i].Name;
           if ShowLabels and (Vars[i].Comment <> '') then
              s := s + ' (' + Vars[i].Comment + ')';
           SL.AddObject(s, Vars[i]);
           end;
End;

Procedure FillListWithMatrix(SL: TStrings;
                             TabVar: TwsTabVar;
                             Const Typ: Array of TwsEnumMatType;
                             ShowLabels: Boolean = false);
Var i : Word;
    MatSet : Set of TwsEnumMatType;
    s: String;
Begin
  SL.Clear;
  MatSet := [];
  For i := Low(Typ) to High(Typ) do
     Include(MatSet, Typ[i]);

  If TabVar.NVars > 0 Then
     For i := 0 to TabVar.NVars-1 do
       If (TabVar.Vars[i].VarType = vtMatrix) Then
          If TabVar.Vars[i].AsMatrix.MatType in MatSet Then
             begin
             s := TabVar.Vars[i].Name;
             if ShowLabels and (TabVar.Vars[i].Comment <> '') then
                s := s + ' (' + TabVar.Vars[i].Comment + ')';
             SL.AddObject(s, TabVar.Vars[i]);
             end;
End;


Procedure ColsDescriptionToMemo(SL:TStrings; Col:TWSDataSetCol);
  Var
    i: Integer;
    s: String;
  Begin
  With SL Do
    Begin
    BeginUpdate;
    Clear;
    Add('Rótulo     : ' + Col.Lab);
    Add('Tipo Var.  : ' + Col.getColTypeAsString);
    Add('Tamanho    : ' + IntToStr(Col.Size));
    Case Col.ColType Of
      dtNumeric:
        Add('Precisão   : ' + IntToStr(TwsNumeric(Col).Precision));

      dtQuant, dtQualit, dtQualitOrd :
        Begin
        Add('Tipo Nível : ' + TwsFactor(Col).getLevelTypeAsString);
        Add('Contraste  : ' + TwsFactor(Col).ContrTypeAsString);
        Add('Níveis     : ' + IntToStr(TwsFactor(Col).LevelNames.Count));
        For i := 0 To TwsFactor(Col).LevelNames.Count-1 Do
          begin
          s:= ' '+IntToStr(i+1)+'. ' +TwsFactor(Col).LevelNames[i];
          if Col.ColType = dtQuant then
            s:=s+'  '+FloatToStrF(TwsQuantitative(Col).LevelValues[i+1],ffgeneral,8,5);
          Add(s);
          end
        End;
      End; { Case }
    EndUpdate;
    End; { With SL }
  End; { ColsDescriptionToMemo }

Procedure FillListWithRowsDs(SL:TStrings; DS:TWSDataSet);
Var i : Integer;
Begin
  With SL Do
    Begin
    Clear;
    For i := 1 To DS.NRows Do
      if DS.Row[i].Name = '' then
         Add(IntToStr(i))
      else
         Add(DS.Row[i].Name);
    End; {With SL}
End; { FillListWithRowsDs }

Procedure FillListWithColsDs(SL:TStrings;DS:TWSDataSet);
Var i: Integer;
Begin
  With SL Do
    Begin
    Clear;
    For i := 1 To DS.NCols Do
      Add(DS.Struct.Col[i].Name);
    End; { With SL }
End; { FillListWithColsDs }

Procedure FillListWithColsMatrix(SL:TStrings; M:TwsMatrix);
Var i: Integer;
Begin
  With SL Do
    Begin
    Clear;
    For i := 1 To M.NCols Do
      Add(M.ColName[i]);
    End; { With SL }
End; { FillListWithColsDs }
{******************************************************************************}

function testa_Virg(S: String; Key: Char) : Char;
var i : Integer;
    tem : Boolean;
Begin
  tem:= False;
  case Key of
    #48..#57, #47 : ; //números e barra de divisão
    #44 : if Virg then Key:= #0 else Virg:= True; //vírgula
    #8 : Begin         //backspace
         for i:= 1 to Length(S)-1 do
           if S[i] = ',' then
             Begin
             tem:= True;
             break;
             End;
         if not tem then
           Virg:= False;
         End;
  else
    Key:= #0;
  End;
  Result:= Key;
End;

// Converte uma definicao de StemLeaf para uma definicao em XML.
// Esta definicao em XML é colocada dentro de um bloco BXML que geralmente é um bloco de Texto
procedure StemLeafToXML(StemLeaf: TStrings; Output: TwsBXML_Output);
var LL: Integer;
    SL: TStrings;

    procedure WriteStemLeaf(var L: Integer);
    var b: Boolean;
    begin
      // Escreve o sub-titulo de cada sub-StemLeaf (título terciário)
      Output.CenterTitle(3, StemLeaf[L]);

      // Escreve o comentário
      inc(L);
      Output.Center(1, StemLeaf[L]);
      Output.NewLine;

      // Escreve se existe os valores inferiores excluídos
      b := False;
      inc(L);
      if System.Pos('INF:', StemLeaf[L]) = 1 then
         begin
         b := True;
         Output.BeginTable(1);
           Output.BeginTableRow;
             Output.NewTableCell(StemLeaf[L]);
           Output.EndTableRow;
         Output.EndTable;
         end;

      // Escreve os ramos e as folhas
      SL := nil;
      if b then inc(L, 2) else inc(L);
      Output.BeginTable(0);
      repeat
        Split(StemLeaf[L], SL, [' ']);

        // Verifica se é o ponto central
        b := (Length(SL[0]) > 0) and (SL[0][1] = '(');
        if b then SL[0] := SubString(SL[0], '(', ')');

        Output.BeginTableRow;
          Output.NewTableCell(SL[0], b);
          Output.NewTableCell(SL[1], b);
          // pode nao existir folhas para o ramo
          if SL.Count>2 then
            Output.NewTableCell(SL[2], b);
        Output.EndTableRow;

        inc(L);
      until StemLeaf[L] = 'FIM';
      Output.EndTable;
      SL.Free;

      // Escreve se existe os valores superiores excluídos ou se posiciona no próxima bloco
      b := False;
      inc(L);
      if (L < StemLeaf.Count) and (System.Pos('SUP:', StemLeaf[L]) = 1) then
         begin
         b := True;
         Output.BeginTable(1);
           Output.BeginTableRow;
             Output.NewTableCell(StemLeaf[L]);
           Output.EndTableRow;
         Output.EndTable;
         end;

      // Posiciona no próxima bloco
      if b then inc(L);
    end;

begin
  LL := 0;
  while LL < StemLeaf.Count do
    if System.Pos('Ramo', StemLeaf[LL]) = 1 then
       WriteStemLeaf(LL);
end;

function ObterIdentificacao(M: TwsMatrix): String;
var s: String;
begin
  Result := M.Name;
  s := SysUtilsEx.AllTrim(M.MLab);
  if s <> '' then
     if s[1] = '-' then
        Result := Result + ' ' + s
     else
        Result := Result + ' - ' + s;
end;

end.
