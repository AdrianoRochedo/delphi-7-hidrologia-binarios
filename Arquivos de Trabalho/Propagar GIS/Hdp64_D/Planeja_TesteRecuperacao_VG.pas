program Planeja_LiberaDemandas_PC9;

var Saida    : Object; {Vari�vel j� inicializada pelo sistema}
    Projeto  : Object; {Vari�vel j� inicializada pelo sistema}
    V        : Object;
    dt       : Integer;
begin
  dt := Projeto.DeltaT;
  if dt = 1 then
     begin
     V := TwsVec(GlobalObjects.Get('Meu vetor'));
     if ObjectIsValid(V) then V.Print(Saida);
     end;
end.
