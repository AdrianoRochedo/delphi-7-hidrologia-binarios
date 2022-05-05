unit wsTabelaDeSimbolos;

{
  ÚLTIMA ATUALIZAÇÃO: .................. Rochedo, 04/11/1998
  ESCOPO: .............................. Existência de variáveis

  HISTÓRICO  -------------------------------------------------------------------

  AUTOR E DATA: ........................ Rochedo, 10/07/1998
  ESCOPO: .............................. Existência de variáveis

  AUTOR E DATA: ........................ Rochedo, 04/11/1998
  ESCOPO: .............................. Existência de variáveis
                                         Agora eu proponho um nome diferente.
                                         Método Print melhorado.

  AUTOR E DATA: ........................ Alex e Rochedo, 12/01/99
  ESCOPO: .............................. Criamos Destructor TVariable.Destroy
                                         Também modificamos SetDataSet, SetMatrix e
                                         SetVector, incluindo a linha  FObject := FValue;

}

interface
uses Classes,
     SysUtils,
     SysUtilsEx,
     wsMatrix,
     wsVec,
     MessageManager;

type
  TwsEnumVarType  = (vtNumeric,vtMatrix,vtVector,vtNull,vtDataSet,vtAll,vtStr);

  TwsVariable = class(T_NRC_InterfacedObject, IMessageReceptor)
  Private
    FSimbolo   : String;
    FTipo      : TwsEnumVarType;
    FSize      : Longint;
    FAcessada  : Boolean;
    FObject    : TObject;
    Function  GeTDataSet: TWSDataSet;          Virtual; Abstract;
    Function  GeTMatrix: TWSMatrix;            Virtual; Abstract;
    Function  GeTVector: TWSVec;               Virtual; Abstract;
    Function  GetValue : Double;               Virtual; Abstract;
    Function  GetComment: String;              Virtual; Abstract;
    Procedure SeTDataSet(Value: TWSDataSet);   Virtual; Abstract;
    Procedure SeTMatrix (Value: TWSMatrix);    Virtual; Abstract;
    Procedure SeTVector (Value: TWSVec);       Virtual; Abstract;
    Procedure SetValue  (Const Value: Double); Virtual; Abstract;

    function ReceiveMessage(const MSG: TadvMessage): Boolean;
  Public
    Constructor Create(Const Name: String; TypeV: TwsEnumVarType);
    Destructor Destroy; Override;

    Property Name       : String           Read FSimbolo    Write FSimbolo;
    Property Comment    : String           Read GetComment;
    Property Obj        : TObject          Read FObject     Write FObject;
    Property VarType    : TwsEnumVarType   Read FTipo       Write FTipo;
    Property Accessed   : Boolean          Read FAcessada   Write FAcessada; {Mod.21/10/96}

    Property AsMatrix   : TwsMatrix   Read GeTMatrix  Write SeTMatrix;
    Property AsDataSet  : TwsDataSet  Read GeTDataSet Write SeTDataSet;
    Property AsVector   : TwsVec      Read GeTVector  Write SeTVector;
    Property AsFloat    : Double      Read GetValue   Write SetValue;
  End;

  TwsNumericVar = class(TwsVariable)
  Private
    FValue : Double;
    Function  GetValue: Double; Override;
    Procedure SetValue(Const Value: Double); Override;
    Function  GetComment: String; override;
  Public
    Constructor Create(Const Name: String; Const Value: Double);
  End;

  TwsVectorVar = class(TwsVariable)
  Private
    Function  GetVector: TwsVec;        Override;
    Procedure SetVector(Value: TwsVec); Override;
    Function  GetComment: String;       override;
  Public
    Constructor Create(Const Name: String; Value : TwsVec);
  End;

  TwsMatrixVar = class(TwsVariable)
  Private
    Function  GetMatrix: TWSMatrix;        Override;
    Procedure SetMatrix(Value: TWSMatrix); Override;
    Function  GetComment: String;          override;
  Public
    Constructor Create(Const Name: String; Value : TwsMatrix);
  End;

  TwsDataSetVar = class(TwsVariable)
  Private
    Function  GetDataSet: TWSDataSet;        Override;
    Procedure SetDataSet(Value: TWSDataSet); Override;
  Public
    Constructor Create(Const Name: String; Value : TwsDataSet);
  End;


type
  TwsTabVar = class(T_NRC_InterfacedObject, IMessageReceptor)
  Private
    FTab : TList;
    FCount : Integer;
    FCaseSens : Boolean;
    Procedure VerificaExistencia(Var Name : String);
    Function GetAccessedVars: Integer;
    Function GetVars(Index : Integer): TwsVariable;
    Function GetCount: Integer;
    function ReceiveMessage(const MSG: TadvMessage): Boolean;
  public
    Constructor Create;
    Destructor Destroy; Override;

    Function  VarByName (Const Name: String) : TwsVariable;
    Function  VarByIndex(Index : Integer) : TwsVariable;
    Function  IndexOf(Const Name: String) : Integer;
    Function  Exist(Const Name: String; Out Index : Integer) : Boolean;

    Function  AddFloat  (Name: String; Const Value : Double) : Integer;
    Function  AddVector (Name: String; Value : TWSVec) : Integer;
    Function  AddMatrix (Name: String; Value : TWSMatrix) : Integer;
    Function  AddDataSet(Name: String; Value : TWSDataSet) : Integer;

    Procedure SetFloatValue(Const Name: String; Const Val: Double); overload;
    Procedure SetFloatValue(Const IndexVar: Integer; Const Val: Double); overload;
    Procedure AssignNums(Tab : TwsTabVar);

    function DeleteVar (Const Name: String): Boolean;
    Procedure DeleteVars(Const Names: Array of String);

    {$ifdef prj_WinStat}
    Procedure Print(Const Names : Array of String);
    {$endif}

    Property  Vars[Index : Integer]: TwsVariable Read GetVars; Default;
    Property  AccessedVars: Integer Read GetAccessedVars;
    // Precisa ser conectado a uma saída externa
//    property OutPut: TOutPut read FOutPut write FOutPut;
  published
    Property  NVars: Integer Read GetCount;
    Property  CaseSensitive : Boolean Read FCaseSens Write FCaseSens Default True; { Diferencia maisucula de minuscula }
  end;

implementation
Uses wsConstTypes,
     wsExceptions,
     wsVars;

Const
  Classe = 'Classe: Tabela de Variáveis';

{----------------- TwsVariable ------------------}

Constructor TwsVariable.Create(Const Name: String; TypeV : TwsEnumVarType);
Begin
  inherited Create;
  GetMessageManager.RegisterMessage(WSM_NOME_MUDOU, self); 
  FSimbolo := Name;
  FTipo := TypeV;
  FAcessada := False;
End;

Destructor TwsVariable.Destroy;
Begin
  GetMessageManager.UnregisterMessage(WSM_NOME_MUDOU, self);
  FObject.Free;
  Inherited Destroy;
End;

function TwsVariable.ReceiveMessage(const MSG: TadvMessage): Boolean;
begin
  if (MSG.ID = WSM_NOME_MUDOU) and (FSimbolo = MSG.ParamAsString(0)) then
     FSimbolo := MSG.ParamAsString(1);
end;

{------------- TwsNumericVar --------------}

Constructor TwsNumericVar.Create(Const Name: String; Const Value : Double);
Begin
  Inherited Create(Name, vtNumeric);
  FValue := Value;
End;

function TwsNumericVar.GetComment: String;
begin
  Result := '';
end;

Function  TwsNumericVar.GetValue: Double;
Begin
  Result := FValue;
  Accessed := True;
End;

Procedure TwsNumericVar.SetValue(Const Value: Double);
Begin
  FValue := Value;
  Accessed := True;
End;

{-------------- TVectorVar ----------------}

Constructor TwsVectorVar.Create(Const Name: String; Value : TWSVec);
Begin
  Inherited Create(Name, vtVector);
  FObject := Value;
End;

function TwsVectorVar.GetComment: String;
begin
  Result := '';
end;

Function  TwsVectorVar.GetVector: TwsVec;
Begin
  Result := TwsVec(FObject);
  Accessed := True;
End;

Procedure TwsVectorVar.SetVector(Value: TwsVec);
Begin
  Accessed := True;
  FObject := Value;
End;

{---------------- TMatrixVar ----------------------}

Constructor TwsMatrixVar.Create(Const Name: String; Value : TWSMatrix);
Begin
  Inherited Create(Name, vtMatrix);
  FObject := Value;
End;

function TwsMatrixVar.GetComment: String;
begin
  Result := TwsMatrix(FObject).MLab;
end;

Function  TwsMatrixVar.GetMatrix: TwsMatrix;
Begin
  Result := TwsMatrix(FObject);
  Accessed := True;
End;

Procedure TwsMatrixVar.SetMatrix(Value: TWSMatrix);
Begin
  Accessed := True;
  FObject := Value;
End;

{--------------  TTabVar ---------------}

Constructor TwsTabVar.Create;
Begin
  Inherited Create;
  GetMessageManager.RegisterMessage(WSM_REMOVER_OBJETO, self);
  FTab := TList.Create;
  FCaseSens := False;
End;

Destructor TwsTabVar.Destroy;
Var i : Integer;
    v : TwsVariable;
Begin
  GetMessageManager.UnRegisterMessage(WSM_REMOVER_OBJETO, self);
  For i := 0 to FTab.Count-1 do
    begin
    v := FTab.Items[i];
    v.Free;
    End;
  FTab.Free;
  Inherited Destroy;
End;

Procedure TwsTabVar.VerificaExistencia(Var Name : String);
Var Index, i   : Integer;
    Msg        : String;
    ValidIdent : Boolean;
    s          : String;
Begin
  Name := SysUtilsEx.AllTrim(Name);
  If Not SysUtilsEx.IsValidIdent(Name) Then
     Name := GetValidId(Name);

  s := Name;

  i := 0;
  While Exist(Name, Index) do
    Begin
    inc(i);
    Name := s + intToStr(i);
    End;
End;

Function  TwsTabVar.VarByName(Const Name: String): TwsVariable;
Var Index : Integer;
Begin
  If Exist(SysUtilsEx.AllTrim(Name), Index) Then
     Result := FTab.Items[Index]
  Else
     Raise EUnknownVariable.CreateFmt(
           Classe + #13 +
           'Erro: ' + MsgUnknownVariable, [Name]);
End;

Function  TwsTabVar.VarByIndex(Index: Integer): TwsVariable;
Begin
  If (Index >= 0) And (Index < FTab.Count) Then
     Result := FTab.Items[Index]
  Else
     Raise EInvalidIndexVar.Create(
           Classe + #13 +
           'Erro: ' + MsgInvalidIndexVar);
End;

Function  TwsTabVar.IndexOf(Const Name: String): Integer;
Begin
  If Not Exist(SysUtilsEx.AllTrim(Name), Result) Then
     Raise EUnknownVariable.CreateFmt(
           Classe + #13 +
           'Erro: ' + MsgUnknownVariable, [Name]);
End;

Function  TwsTabVar.Exist(Const Name: String; Out Index : Integer) : Boolean;
Var i : Integer;
Begin
  Index := -1;
  Result := False;
  For i := 0 To FTab.Count-1 Do
    Begin
    If FCaseSens Then
       Begin
       If Name = TwsVariable(FTab.Items[i]).Name Then
          Begin
          Result := True;
          Index := i;
          Exit;
          End;
       End
    Else
       If CompareText(Name, TwsVariable(FTab.Items[i]).Name) = 0 Then
          Begin
          Result := True;
          Index := i;
          Exit;
          End;
    End;
End;

Function TwsTabVar.AddFloat (Name: String; Const Value: Double): Integer;
Var N : TwsNumericVar;
Begin
  VerificaExistencia(Name);
  N := TwsNumericVar.Create(Name, Value);
  Result := FTab.Add(N);
End;

Function TwsTabVar.AddVector(Name: String; Value : TwsVec) : Integer;
Var N : TwsVectorVar;
Begin
  VerificaExistencia(Name);
  Value.Name := Name;
  N := TwsVectorVar.Create(Name, Value);
  Result := FTab.Add(N);
End;

Function TwsTabVar.AddMatrix(Name: String; Value: TwsMatrix) : Integer;
Var N : TwsMatrixVar;
Begin
  VerificaExistencia(Name);
  Value.Name := Name;
  N := TwsMatrixVar.Create(Name, Value);
  Result := FTab.Add(N);
End;

Function TwsTabVar.AddDataSet(Name: String; Value: TwsDataSet) : Integer;
Var N : TwsDataSetVar;
Begin
  VerificaExistencia(Name);
  Value.Name := Name;
  N := TwsDataSetVar.Create(Name, Value);
  Result := FTab.Add(N);
End;

Procedure TwsTabVar.SetFloatValue(Const Name: String; Const Val: Double);
Begin
  TwsNumericVar(VarByName(Name)).AsFloat := Val;
End;

Procedure TwsTabVar.SetFloatValue(Const IndexVar: Integer; Const Val: Double);
Begin
  TwsNumericVar(FTab.Items[IndexVar]).AsFloat := Val;
End;

function TwsTabVar.DeleteVar (Const Name : String): boolean;
Var p : TwsVariable;
    Index : Integer;
Begin
  If Exist(SysUtilsEx.AllTrim(Name), Index) Then
     Begin
     p := VarByIndex(Index);
     p.Free;
     FTab.Remove(p);
     End;

  Result := (index > -1);   
End;

Procedure TwsTabVar.DeleteVars(Const Names : Array of String);
var i: Integer;
Begin
  For i := 0 to High(Names) do DeleteVar(Names[i]);
End;

{$ifdef prj_WinStat}
Procedure TwsTabVar.Print(Const Names : Array of String); // <<<<<
var i: Integer;
    s: String;
    p : TwsVariable;
    Index : Integer;
Begin
  //if gOutPut <> nil then gOutPut.FileName := '';
  For i := 0 to High(Names) do
    Begin
    s := Names[i];
    If Exist(s, Index) Then
       Begin
       p := VarByIndex(Index);
       Case p.VarType of
          vtNumeric:
            Begin
//               if gOutPut <> nil then
//               gOutPut.Text.Add(p.Name + ' = ' + FloatToStrF(p.Value, ffFixed, 4, 15));
            End;

          vtVector:
            Begin
//            if gOutPut <> nil then gOutPut.Text.Add(p.Name + ' = ');
//            TwsOutPut(gOutPut).WriteTable(p.Vector);
            End;

          vtMatrix:
            Begin
//            if gOutPut <> nil then gOutPut.Text.Add(p.Name + ' = ');
//            TwsOutPut(gOutPut).WriteTable(p.Matrix);
            End;

          vtDataSet:
            Begin
//            TwsOutPut(gOutPut).WriteTable(p.DataSet);
            End;
          End;
       End;
    End;
End;
{$endif}

Procedure TwsTabVar.AssignNums(Tab : TwsTabVar);
Var i,j : Integer;
    Inclui : Boolean;
Begin
  For i := 0 to Self.NVars-1 do
    Begin
    Inclui := True;
    For j := 0 to Tab.NVars-1 do
      If Self.Vars[i].Name = Tab.Vars[j].Name Then
         Begin
         Inclui := False;
         Break;
         End;
    If Inclui and (Self.Vars[i].VarType = vtNumeric) Then
       Tab.AddFloat(Self.Vars[i].Name, Self.Vars[i].AsFloat);
  End;
End;


Function TwsTabVar.GetAccessedVars: Integer;
Var i : Integer;
Begin
  Result := 0;
  For i := 0 to FTab.Count-1 do
    If Vars[i].Accessed Then Inc(Result);
End;

Function TwsTabVar.GetVars(Index : Integer): TwsVariable;
Begin
  If (Index >= 0) and (Index < FTab.Count) then
     Result := FTab.Items[Index];
End;

Function TwsTabVar.GetCount : Integer;
Begin
  FCount := FTab.Count;
  Result := FCount;
End;

function TwsTabVar.ReceiveMessage(const MSG: TadvMessage): Boolean;
var o: TObject;
    i: Integer;
begin
  if MSG.ID = WSM_REMOVER_OBJETO then
     begin
     o := MSG.ParamAsObject(0);
     for i := 0 to NVars-1 do
       if o = Vars[i].Obj then
          begin
          // Remove a referencia para que na destruicao da variável o objeto nao seja
          // destruido, pois ele sera destruida apos a mensagem ser processada.
          Vars[i].Obj := nil;
          DeleteVar(Vars[i].Name);
          Break;
          end
     end
end;

{ TDataSetVar }

Constructor TwsDataSetVar.Create(Const Name: String; Value: TWSDataSet);
Begin
  Inherited Create(Name, vtDataSet);
  FObject := Value;
End;

Function  TwsDataSetVar.GeTDataSet: TwsDataSet;
Begin
  Result := TwsDataSet(FObject);
  Accessed := True;
End;

Procedure TwsDataSetVar.SeTDataSet(Value: TwsDataSet);
Begin
  Accessed := True;
  FObject := Value;
End;

end.
