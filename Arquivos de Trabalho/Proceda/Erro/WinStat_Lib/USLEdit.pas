unit Usledit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type

{Atencao, Fazer rotina que remove strings duplicadas se a opcao Duplic
 estiver setada para NoDuplic}

  TSLEdit = class(TEdit)
  private
    FItems : TStrings;
    FDuplic : Boolean;
    Procedure SetItems(Value : TStrings);
    Function  GetItems: TStrings;
  protected
    { Protected declarations }
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
    Property Items : TStrings Read GetItems Write SetItems;
    property Duplicates : Boolean Read FDuplic Write FDuplic Default True;
  end;

procedure Register;

implementation
Uses wsGLib, SysUtilsEx;

procedure Register;
begin
  RegisterComponents('Dri', [TSLEdit]);
end;

Constructor TSLEdit.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FItems := TStringList.Create;
  FDuplic := True;
End;

Destructor TSLEdit.Destroy;
Begin
  TStringList(FItems).Free;
  Inherited Destroy;
End;

Procedure TSLEdit.SetItems(Value : TStrings);
Var i : Integer;
    S : String;
Begin
  FItems.Assign(TStringList(Value));
  S := '';
  For i := 0 to TStringList(Value).Count-1 do
    S := S + TStringList(Value).Strings[i] + ',';
  Delete(S, Length(S),1);
  Text := S;
End;

Function  TSLEdit.GetItems : TStrings;
Var P : Array[0..255] of Char;
Begin
  StrPCopy(P,Text);
  Result := CharToStrList(P, DelChar);
  If Not FDuplic Then
     Result := TStrings(RemoveDuplicates(TStringList(Result)));
End;

end.
