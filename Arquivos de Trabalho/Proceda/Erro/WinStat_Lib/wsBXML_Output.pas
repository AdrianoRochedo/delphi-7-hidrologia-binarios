unit wsBXML_Output;

interface
uses Classes,
     Graphics,
     BXML_Output,
     XML_Utils;

type
  TwsBXML_Align    = (alLeft, alRight, alCenter);
  TwsBXML_AlignMap = array[TwsBXML_Align] of String;

const
  wsBXML_AlignMap: TwsBXML_AlignMap = ('left', 'right', 'center');

type
  TwsBXML_Output = class(TBXML_Output)
  private
    Fxw: TXML_Writer;
  public
    destructor Destroy; override;

    procedure BeginText;
    procedure EndText;

    // Texto em linha
    procedure WriteTitle(Size: Integer; const aTitle: String);
    procedure CenterTitle(Size: Integer; const aTitle: String);
    procedure Center(Size: Integer; const aText: String);
    procedure Warning(const aText: String);
    procedure WriteText(const aText: String);
    procedure WriteLink(const aText, aLink: String);
    procedure NewLine;

    // Texto Tabelado
    procedure WriteTabText(const Text: TStrings; Sep: char = #13; Align: TwsBXML_Align = alLeft);
    procedure WritePropValue(const aProperty, aValue: String; PropPercent: byte = 30; Border: byte = 0);

    // Tabela
    procedure BeginTable(Border: byte = 1; Align: TwsBXML_Align = alCenter);
    procedure EndTable;
    procedure BeginTableRow;
    procedure EndTableRow;
    procedure WriteTableHeader(const Headers: array of String);
    procedure NewTableCell(const Text: String; Bold: Boolean = false);

    // Legenda
    procedure BeginLegend(const Title: String);
    procedure EndLegend;
    procedure NewLegendItem(const Title: String; Color: TColor);

    property Writer: TXML_Writer read Fxw;
  end;

implementation
uses SysUtils, SysUtilsEx, GraphicUtils;

{ TwsBXML_Output }

destructor TwsBXML_Output.Destroy;
begin
  Fxw.Free;
  inherited;
end;

procedure TwsBXML_Output.BeginText;
begin
  BeginBlock('wsTextBlock', 'wsText', 'Estilo de texto do WinStat');
  Fxw := TXML_Writer.Create(ActiveBlock.Block);
  Fxw.IdentSize := 4;
end;

procedure TwsBXML_Output.EndText;
begin
  if Fxw <> nil then
     begin
     FreeAndNil(Fxw);
     EndBlock;
     end;
end;

procedure TwsBXML_Output.Center(Size: Integer; const aText: String);
begin
  Fxw.Write('center', ['Size'], [Size], aText);
end;

procedure TwsBXML_Output.CenterTitle(Size: Integer; const aTitle: String);
begin
  Fxw.BeginTag('center');
    WriteTitle(Size, aTitle);
  Fxw.EndTag('center');
end;

procedure TwsBXML_Output.WriteTitle(Size: Integer; const aTitle: String);
begin
  Fxw.Write('title' + IntToStr(Size), aTitle);
end;

procedure TwsBXML_Output.Warning(const aText: String);
begin
  Fxw.Write('warning', aText);
end;

procedure TwsBXML_Output.WriteText(const aText: String);
begin
  Fxw.Write('text', aText);
end;

procedure TwsBXML_Output.BeginTable(Border: byte; Align: TwsBXML_Align);
begin
  Fxw.BeginTag('table', ['border', 'align'], [Border, wsBXML_AlignMap[Align]]);
  Fxw.BeginIdent;
end;

procedure TwsBXML_Output.EndTable;
begin
  Fxw.EndIdent;
  Fxw.EndTag('table');
end;

procedure TwsBXML_Output.BeginTableRow;
begin
  Fxw.BeginTag('row');
  Fxw.BeginIdent;
end;

procedure TwsBXML_Output.EndTableRow;
begin
  Fxw.EndIdent;
  Fxw.EndTag('row');
end;

procedure TwsBXML_Output.NewTableCell(const Text: String; Bold: Boolean = false);
begin
  if Bold then
     Fxw.Write('cell', ['bold'], [byte(Bold)], Text)
  else
     Fxw.Write('cell', Text);
end;

procedure TwsBXML_Output.WriteTableHeader(const Headers: array of String);
var i: Integer;
begin
  Fxw.BeginTag('headers');
  Fxw.BeginIdent;
    for i := 0 to High(Headers) do Fxw.Write('header', Headers[i]);
  Fxw.EndIdent;
  Fxw.EndTag('headers');
end;

procedure TwsBXML_Output.BeginLegend(const Title: String);
begin
  Fxw.BeginTag('legend', ['title'], [Title]);
  Fxw.BeginIdent;
end;

procedure TwsBXML_Output.EndLegend;
begin
  Fxw.EndIdent;
  Fxw.EndTag('legend');
end;

procedure TwsBXML_Output.NewLegendItem(const Title: String; Color: TColor);
begin
  Fxw.Write('item', ['color'], ['#' + GetRGBColorAsString(Color)], Title);
end;

procedure TwsBXML_Output.WriteTabText(const Text: TStrings; Sep: char; Align: TwsBXML_Align);
var SL: TStrings;
    k, i: Integer;
begin
  SL := nil;
  BeginTable(0, Align);
  for k := 0 to Text.Count-1 do
    begin
    Split(Text[k], SL, [Sep]);
    BeginTableRow;
    for i := 0 to SL.Count-1 do NewTableCell(SL[i]);
    EndTableRow;
    end;
  EndTable;
  SL.Free;
end;

procedure TwsBXML_Output.WritePropValue(const aProperty, aValue: String; PropPercent, Border: byte);
var sPropPercent: String;
begin
  sPropPercent := IntToStr(PropPercent) + '%';
  Fxw.Write('PropertyValue',
            ['property', 'value', 'propPercent', 'border'],
            [aProperty , aValue , sPropPercent , Border],
            '');
end;

procedure TwsBXML_Output.WriteLink(const aText, aLink: String);
var b: Boolean;
begin
  b := False;
  if Fxw = nil then
     begin
     b := True;
     BeginText;
     NewLine;
     end;

  Fxw.Write('link', ['ref'], [ChangeChar(aLink, '\', '/')], aText);

  if b then EndText;
end;

procedure TwsBXML_Output.NewLine;
begin
  Fxw.WriteTag('NewLine');
end;

end.
