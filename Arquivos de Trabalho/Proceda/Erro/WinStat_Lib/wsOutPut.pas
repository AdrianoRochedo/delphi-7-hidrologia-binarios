unit wsOutPut;

interface
uses Windows, Classes, Forms, OutPut, wsVec, wsMatrix, Edit,
     Graphics, Chart, wsConstTypes, wsgLib;

type
  { Saída em modo TEXTO:
    O método ShowText foi modificado para que produza as saídas no Editor (Edit.Pas)
    Mas atenção: O texto só será apresentado após uma chamada do método Show de TwsOutPut
    (Saída não Sincronizada), pois o texto tem que ser passado do Buffer para o Editor.

    Para acesso direto ao Editor use a propriedade Editor (não recomendado), mas o texto
    não será Bufferizado.

    Para uso otimizado, gere toda a saída e só então chame Show.

    Saída em modo HTML:
    A saída gerada precisará ser salva em disco (método Save) antes de ser mostrada.
    Para maiores informações olhar instruções na classes TOutPut (OutPut.pas).

  }

  TwsOutPut = class(TOutPut)
  private
    FEditor: TEditor;
    FMaxCols : integer;
    FPrintDesc: Boolean;
    FCentralizar: Boolean;
    FLink: Boolean;
    FFwd: Byte;
    FDecim: Byte;
    FFuzzVal: Double; // maximo de colunas para ser impressa em uma mesma linha (modo html - tabelas)
  public
    constructor Create;
    destructor Destroy; override;

    procedure Write(Vector: TwsVec); overload;
    procedure Write(Matrix: TwsMatrix); overload;
    procedure Write(Matrix: TwsDataSet); overload;
    procedure Write(Chart: TChart); overload;
    procedure WriteTable(Matrix : TwsMatrix); overload;
    procedure WriteTable(DataSet : TwsDataSet); overload;

    {$ifdef prj_WinStat}
    procedure WriteTable(Vec: TwsVec); overload;
    procedure WriteTable(Vec: TwsVec; Inicio: integer); overload;
    {$endif}

    procedure WriteTable(Matrix : TwsMatrix;Inicio: integer);  overload;
    procedure WriteTable(DataSet : TwsDataSet;Inicio: integer);  overload;
    procedure ShowTEXT; override;

    {$ifdef prj_WinStat}
    procedure OutGraph(Graph : TForm);
    {$endif}

    property  Editor: TEditor read FEditor;
    property  MaxCols : integer read FMaxCols write FMaxCols;
  end;

implementation
uses drGraficos,
     {$ifdef prj_WinStat}
     //Graficos_Carrossel,
     WindowsManager,
     {$endif}
     dialogs;

{ TwsOutPut }

procedure TwsOutPut.Write(Vector: TwsVec);
begin
  if DocType = dtHTML then
    begin
    Text.Add('<pre>');
    //Vector.Print;
    Text.Add('</pre>');
    end
  else
    if DocType = dtTEXT then
      //Vector.Print;
end;

procedure TwsOutPut.ShowTEXT;
begin
  FEditor.Clear;
  FEditor.WriteStrings(Text);
  FEditor.Show;
end;

procedure TwsOutPut.Write(Matrix: TwsMatrix);
begin
//  Matrix.OutPut := self;

  if DocType = dtHTML then
    begin
    Text.Add('<pre>');
    //Matrix.Print;
    Text.Add('</pre>');
    end
  else
    if DocType = dtTEXT then
      //Matrix.Print;
end;

procedure TwsOutPut.Write(Matrix: TwsDataSet);
begin
//  Matrix.OutPut := self;

  if DocType = dtHTML then
    begin
    Text.Add('<pre>');
   // Matrix.Print;
    Text.Add('</pre>');
    end
  else
    if DocType = dtTEXT then
      //Matrix.Print;
end;

procedure TwsOutPut.WriteTable(Matrix : TwsMatrix);
// Escreve a tabela com no maximo 15 colunas
begin
  WriteTable(Matrix, 1);
end;

procedure TwsOutPut.WriteTable(DataSet : TwsDataSet);
// Escreve a tabela com no maximo 15 colunas
begin
  WriteTable(DataSet, 1);
end;

{$ifdef prj_WinStat}
procedure TwsOutPut.WriteTable(Vec: TwsVec);
// Escreve a tabela com no maximo 15 colunas
begin
  WriteTable(Vec, 1);
end;

// Escreve um vetor em html no modo tabela
procedure TwsOutPut.WriteTable(Vec: TwsVec; Inicio: integer);
begin
  if DocType = dtHTML then
    begin
    if Vec.PrintTable(Inicio,FMaxCols) then //Matrix tem que ser quebrada
      WriteTable(Vec,Inicio);
    end
  else
    if DocType = dtText then
      Vec.Print;
end;
{$endif}

// Escreve uma matriz em html no modo tabela
procedure TwsOutPut.WriteTable(Matrix: TwsMatrix; Inicio: integer);
begin
  if DocType = dtHTML then
    begin
    //if Matrix.PrintTable(Inicio,FMaxCols) then //Matrix tem que ser quebrada
       //WriteTable(Matrix,Inicio);
    end
  else
    if DocType = dtText then
       //Matrix.Print;
end;

// Escreve uma matriz em html no modo tabela
procedure TwsOutPut.WriteTable(DataSet: TwsDataSet; Inicio: integer);
begin
  if DocType = dtHTML then
    begin
   // if DataSet.PrintTable(Inicio,FMaxCols) then //Matrix tem que ser quebrada
      // WriteTable(DataSet,Inicio);
    end
  else
    if DocType = dtText then
      //DataSet.Print;
end;

constructor TwsOutPut.Create;
begin
  inherited Create;
  FEditor := TEditor.Create(nil);
  FMaxCols := 11;   // fazer este valor configuravel na interface.
  //Verificar tamanho de fonte
  //Incluir fonte Courier New na interface
end;

destructor TwsOutPut.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

procedure TwsOutPut.Write(Chart: TChart);
var tmpR : TRect;
    B    : TBitMap;
begin
    if DocType = dtHTML then
       begin
       B := TBitmap.Create;
       try
         tmpR     := TgrGrafico(Chart).Grafico.GetRectangle;
         B.Width  := tmpR.Right - tmpR.Left;
         B.Height := tmpR.Bottom - tmpR.Top;
         TgrGrafico(Chart).Grafico.Draw(B.Canvas, tmpR);
         BeginTable;
           BeginRow;
             BeginCell;
               Inherited Write(B);
             EndCell;
           EndRow;
         EndTable;
       finally
         B.Free;
         end;
       end
    else  // Se nao for HTML, cria janela em separado
       Chart.Show
end;

{$ifdef prj_WinStat}
procedure TwsOutPut.OutGraph(Graph: TForm);
Begin
  if DocType = dtText then
    begin
    //Graf_Carrossel.AddGraph(Graph);
    //Graf_Carrossel.Show
    //gWM.ShowManager;
    end
  else
    Write(TChart(Graph));
End;
{$endif}

end.
