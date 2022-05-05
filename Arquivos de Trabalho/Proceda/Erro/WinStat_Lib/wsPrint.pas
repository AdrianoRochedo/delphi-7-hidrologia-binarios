unit wsPrint;

interface
uses wsMatrix;

  procedure wsMatrix_ColPrint(M: TwsMatrix; const Cols, Condicao: String);

implementation
uses SysUtils,
     wsVec,
     wsTabelaDeSimbolos,
     wsAvaliadorDeExpressoes;

procedure wsMatrix_ColPrint(M: TwsMatrix; const Cols, Condicao: string);
{ Objetivo
    Constroi um string com valores das colunas especifcadas e o imprime se a linha
    respectiva satisfaz à condição estabelecida
  Parâmetros
    Cols: String que estabelece as colunas que serão impressas
    Condicao: Expressão que irá indicar se a linha será impressa ou não. (Ver Expressões)
  Métodos chamados
    Nenhum
  Campos modificados
    Nenhum
}
   Procedure AtualizaValoresDasColunas(Tab: TwsTabVar; Index: Integer);
   Var j : Integer;
       S : String;
   Begin
     For j := 1 to M.NCols do
       begin
       S := UpperCase(M.CName[j-1]);
       Tab.SetFloatValue(S, M.Data[Index,j]);
       end;
   End;

var
  i,ii,j,k,
  PrevI,NumCol: Integer;
  P           : String;
  Aux         : Double;
  C           : TwsLIVec;
  Eval        : TAvaliator;
begin
  i := 0;             { Imprime submatrizes com tantas colunas quanto possivel }
  if Cols <> '' then                                    { Se indica as colunas }
     C := M.IndexColsFromString(Cols)
  else                                        { Senao imprime todas as colunas }
     C := Index(1, M.NCols);

   { Se a expressao nao e vazia e' processada e colocada no formato pos-fixado }
  repeat
    PrevI := i;
    NumCol := M.SelectColsToImp(i, C);        { Quantas colunas serao impressas ?}
    M.SelectHeader(C);
   { Constroi linha de tamanho MaxLen ou esgotando colunas para saida }
    if Condicao = '' then      { Se a expressao e vazia imprime todas as linhas }
       for j := 1 To M.NRows do M.SelectToChar(j, PrevI, NumCol, C)
    else
       Begin
       {1 - Criar uma tabela para as variáveis que representam as colunas}
       {2 - Alocar as variáveis colunas}
       {3 - Atribuir valores iniciais para as variáveis colunas}
       {4 - Avaliação da condição}
       {5 - Liberação da tabela}
       {6 - Finalizações}

       {Passo 1}
       Eval := TAvaliator.Create;
       Try
         Eval.Expression := Condicao;

         {Passo 2,3} {Aloca os nomes das colunas como escalares}
         For ii := 0 to M.CName.Count-1 do
           Eval.TabVar.AddFloat(UpperCase(M.CName.Strings[ii]),0);

         {Passo 4} {Inicio da varredura da matriz}
         For ii := 1 to M.NRows do
           Begin
           AtualizaValoresDasColunas(Eval.TabVar, ii);
           If Eval.Evaluate.AsFloat = 1 Then
              M.SelectToChar(ii, PrevI, NumCol, C);
           End;  {esgota todas as linhas}
       {Passo 5,6}
       Finally
         Eval.Free;
         End;
       End;
    until i = C.Len; { Esgota todas as colunas }
  C.Free;
  //gOutPut.WriteLine; <<<<<
end; { TwsMatrix.ColPrint }

end.
