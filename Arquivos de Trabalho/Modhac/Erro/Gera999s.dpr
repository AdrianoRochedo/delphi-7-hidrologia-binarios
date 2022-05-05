program Gera999s;

{$APPTYPE CONSOLE}

  // Indere -999 para cada linha do arquivo original

uses
  SysUtils,
  Classes;

var SL, SL2: TStrings;
    Filename: string;
    i: integer;
begin
  Filename := ParamSTR(1);
  SL := TStringList.Create();
  SL.LoadFromFile(Filename);
  SL2 := TStringList.Create();
  for i := 0 to SL.Count-1 do
    begin
    SL2.Add(SL[i]);
    SL2.Add('-999');
    end;
  SL2.SaveToFile(Filename + '.txt');
end.
