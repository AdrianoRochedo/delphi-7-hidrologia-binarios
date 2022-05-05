program ObjetosGlobais;

{ Exemplo de como criar, recuperar e destruir vari�veis globais .
  Neste exemplo usamos uma vari�vel global do tipo vetor para
  armazenar os volumes do reservat�rio Queimado. 
  Ap�s, quando chegamos ao final da simula��o, plotamos este
  vetor e destruimos a vari�vel global.}

var Saida    : Object; {Vari�vel j� inicializada pelo sistema}
    Projeto  : Object; {Vari�vel j� inicializada pelo sistema}
    V        : Object;
    PC       : Object;
    Grafico  : Object;
    Planilha : Object;
    dt       : Integer;
begin
  dt := Projeto.DeltaT;
  PC := Projeto.ObtemPCPeloNome('Queimado');

  // Cria��o da vari�vel global
  if dt = 1 then
     begin
     V := wsSFVecCreate(660);
     GlobalObjects.Add('Volumes do Queimado', V);
     end;

  // Recupera��o da vari�vel global e Type-Casting
  V := TwsSFVec(GlobalObjects.Get('Volumes do Queimado'));
  if ObjectIsValid(V) then 
     begin
     V.SetData(dt, PC.ObtemVolume(dt));
     if dt = 660 then 
        begin
        Grafico := CreateObject('TgrGrafico');
        Grafico.Series.AddLineSerie(V, 0, 'Volumes do Queimado');
        Grafico.Show;

        Planilha := CreateObject('TPlanilha');
        Planilha.Write(1, 1, 'Volume');
        Planilha.WriteVecInCol(V, 1, 3);
        Planilha.Show;

        GlobalObjects.Remove('Volumes do Queimado');
        end;
     end;
end.
