unit wsDB;

interface
uses DB,
     DBTables,
     wsMatrix;

  {Tranforma um DataSet em um Arquivo Paradox} {17/04/1999}
  function DSTtoDB(DS: TwsDataSet; const FileName: String): TTable;

implementation

{ Transforma conjuntos de dados em tabelas}
function DSTtoDB(DS: TwsDataSet; const FileName: String): TTable;
var i, j: Integer;
begin
  Result := TTable.Create(nil);
  for i := 1 to DS.nCols do
    if DS.Struct.Col[i].ColType = dtNumeric then
       Result.FieldDefs.Add(DS.Struct.Col[i].Name, ftFloat, 0, False)
    else
       Result.FieldDefs.Add(DS.Struct.Col[i].Name, ftString, 0, False);

  Result.TableName := FileName;
  Result.CreateTable;
  Result.Open;

  for i := 1 to DS.nRows do
    begin
    Result.Append;
    for j := 1 to DS.nCols do Result.Fields[j-1].Value := DS.AsVariant[i, j];
    Result.Post;
    end;
end;

end.
