unit wsItems;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TItemsDLG = class(TForm)
    LB: TListBox;
    procedure LBClick(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    Function  GetItems: TStrings;
    Procedure SetItems(Value: TStrings);
  public
    Item : String[100];
    Property Items: TStrings Read GetItems Write SetItems; 
  end;

  Function ShowItems(Pos: TPoint; Const C: String; Items: TStrings): String;
  Function ShowItemsEx(Pos: TPoint; Const C: String; Const Items: Array of String): String;

implementation

{$R *.DFM}

Function ShowItemsEx(Pos: TPoint; Const C: String; Const Items: Array of String): String;
var i: Integer;
    SL: TStrings;
Begin
  SL := TStringList.Create;
  Try
    For i := Low(Items) to High(Items) do SL.Add(Items[i]);
    Result := ShowItems(Pos, C, SL);
  Finally
    SL.Free;
  End;
End;

Function ShowItems(Pos: TPoint; Const C: String; Items: TStrings): String;
var I: TItemsDLG;
Begin
  I := TItemsDLG.Create(Application);
  Try
    I.Left := Pos.X;
    I.Top := Pos.Y;
    I.Caption := C;
    I.Items := Items;
    I.ShowModal;
    Result := I.Item;
  Finally
    I.Release;
  End;
End;

Function  TItemsDLG.GetItems: TStrings;
Begin
  Result := LB.Items;
End;

Procedure TItemsDLG.SetItems(Value: TStrings);
Var H, W, i, AuxW: Integer;
Begin
  Try
    LB.Items.Assign(Value);
  Except
    Raise Exception.Create('A StringList passada não é válida');
  End;

  {Arruma a altura}
  If (LB.Items.Count <= 10) Then
     Begin
     H := Canvas.TextHeight('X');
     Height := (H + 3) * LB.Items.Count + GetSystemMetrics(SM_CYCAPTION);
     End
  Else
     Height := 190;

  W := 0;
  For i := 0 to LB.Items.Count - 1 Do
    Begin
    AuxW := Canvas.TextWidth(LB.Items[i]);
    If AuxW > W Then W := AuxW;
    End;

  If (W < 250) Then
     Width := W + 20;
End;

procedure TItemsDLG.LBClick(Sender: TObject);
begin
  Item := LB.Items[LB.ItemIndex];
  Close;
end;

procedure TItemsDLG.LBMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var P: TPoint; i: integer;
begin
  P.X := X; P.Y := Y;
  i := LB.ItemAtPos(P, True);
  If i <> -1 Then
     If Canvas.TextWidth(LB.Items[i]) > 250 Then
        Begin
        LB.Hint := LB.Items[i];
        LB.ShowHint := True;
        End
     Else
        LB.ShowHint := False;
end;

end.
