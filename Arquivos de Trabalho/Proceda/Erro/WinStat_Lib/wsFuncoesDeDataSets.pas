unit wsFuncoesDeDataSets;

{
  ATENÇÃO: Todas rotinas que retornam objetos deverao testar a ocorrencia de exceções, isto é,
           não verificar se o resultado é diferente de nil.
}

{ ------------------------------------------------------------------------

    Modificacoes:

      24/07/1997 - Retirada de Buffer.                         - Roger
                   A adição dos dados dos arquivos
                   agora e atraves da propriedade
                   TwsDataSet.DataStr[i].

      10/07/1998 - Inserção do procedimento MeanFactor.        - Amauri

      30/04/1999 - Inserção da rotina PreProcessor             - Rochedo

      08/12/1999 - (Alexdg)Inserção das rotinas:
                   DS_ImportaTextoComCabecalho
                   DS_LeCabecalhoDoArquivoTexto

                   DS_ImportaTextoAPartirDeTemplate
                   DS_AdicionaColunas_e_Linhas
                   DS_CriaEstrutura


   -------------------------------------------------------------------------- }


(****************************************************************************
            Formato de um arquivo texto a ser importado
*****************************************************************************
Autor: Alexdg  (09/12/99)

O arquivo e' dividido em "CABECALHO" e "CORPO".
 - O cabecalho possui duas secoes: <OPCOES> e <VARIAVEIS>.
 - A secao <OPCOES> comeca no inicio do arquivo. Todos os seus componentes
     sao opcionais, quando nao for definido, sera' dado o valor default.
 - A secao VARIAVEIS comeca logo apos a linha que contiver a tag <VARIAVEIS>

O Corpo comeca logo apos a linha que contiver a tag <DADOS>

Abaixo esta' a definicao do formato do arquivo:
  Obs.: O "*" (asterisco) utilizado aqui eh apenas para documentacao
        desta unidade, portanto nao deverá aparecer no arquivo texto.


TEXTO  = [FORMATADO|NAOFORMATADO]               *Default = NAOFORMATADO
NOME   = Nome_do_Conjunto                       *Default = NONAME
ROTULO = Rótulo do conjunto                     *Default =
SEPVAR = [ESPACO | TAB | ; | / | \ | " | ` ]    *Default = ESPACO
SEPOBS = [CR           | ; | / | \ | " | ` ]    *Default = CR
SEPDEC = [, | .]                                *Default = ,

<VARIAVEIS>
V1 = IDENT                  **[Ci Cf] "Rótulo"
V2 = QUALI [FIXO|ALEATORIO] **[Ci Cf] "Rótulo"   Default=Fixo
V3 = ORDEN [FIXO|ALEATORIO] **[Ci Cf] "Rótulo"   Default=Fixo
V4 = QUANT [FIXO|ALEATORIO] **[Ci Cf] "Rótulo"   Default=Fixo
V5 = NUMER                  **[Ci Cf] "Rótulo"

<DADOS>
FEM  0  8 10
FEM  0  4 10
MASC 0  0  7
MASC 1 10 13
FEM  1  8  9
MASC 1  6 11
FEM  2  8  9
MASC 2  6  6
FEM  2  4 12
MASC 0 14 12

Obs.:
*   Valor default caso nao seja definido algum.
**  Somente para texto formatado.
*** Somente para texto não-formatado.
V1..V5 são os nomes das variáveis.
Ci e Cf significam: Coluna Inicial e Coluna Final respectivamente.

------------------------------------------------------------------

Faixa de erros retornados pelas funções e estrutura das chamadas:

(1..399)
DS_ImportaTextoComCabecalho     (401..499)
  DS_LeCabecalhoDoArquivoTexto  (301..399)
  DS_CriaEstrutura              (101..299)
  LeArquivoFree ou LeArquivoFix (1..99)

(1..499)
DS_ImportaTextoAPartirDeTemplate  (401..499)
  DS_LeCabecalhoDoArquivoTexto    (301..399)
  DS_CriaEstrutura               (101.299)
  LeArquivoFree ou LeArquivoFix (1..99)

DS_LeCabecalhoDoArquivoTexto (301..399)

(101..299)
DS_CriaEstrutura     (201..299)
  DS_AdicionaColunas (101..199)

DS_AdicionaColunas (101.199)

LeArquivoFree (1..99)

LeArquivoFix (1..99)
****************************************************************************)


interface
Uses Classes,
     SysUtilsEx,
     XML_Utils,
     wsVec,
     wsMatrix,
     wsConstTypes,
     wsGLib,
     wsTabelaDeSimbolos,
     wsAvaliadorDeExpressoes,
     wsDistribuicoesRandomicas;

  Function DataSetFromText (Const NArq: String; Lista: TList; Tipo: TwsEnumDlgType;
                            Var DelChar: TCharSet; EndField, EndRec, SepDec: Char;
                            Var Erro: Integer; var NReg: Integer): TwsDataSet;

  {Alexdg(12/99) Rotinas para leitura de DataSets e importação a partir
    de arquivos texto e arquivos de template (modelo).}
  Function DS_ImportaTextoComCabecalho(nome_arq:string; Var Erro:integer): TwsDataSet;
  Function DS_ImportaTextoAPartirDeTemplate(nome_arq_template, nome_arq_dados:string;
    Var Erro:integer): TwsDataSet;
  Function DS_AdicionaColunas_e_Linhas(DataSet:TwsDataSet; Vars:TList;
    NumLinhas: Integer = 0):Integer;
  Function DS_CriaEstrutura(Nome, Rotulo: string; NumLinhas: Integer; Vars:TList;
    Var Erro:Integer):TwsDataSet;
  Function DS_LeCabecalhoDoArquivoTexto(st_arq:TStrings; var TipoTexto: Integer; var Nome,
    Rotulo: String; var SepVar, SepObs, SepDec: Char; var Vars:TList; var Posicao:Longint): Integer;
  Function ExportaDataSet(DataSet:TwsDataSet): TStrings;

  Function ModelFrame(Variaveis: TStrings;
                      Grupos: TStrings;
                      Const Condicao: String;
                      DataSet: TwsDataSet;
                      NomesVar: TStrings = nil;
                      UsaNomesNosCalculos: Boolean = False): TwsDataSets;

  Function dsSort(DataSet: TwsDataSet; Grupos: TStrings;
                  Asc: Boolean; Const New: String): TwsDataSet;

  {Marca o inicio e o final dos blocos}
  {Cada bloco representa um Sub-DataSet que poderá ser separado}
  {OBS: Esta rotina só tem sentido se usada em um conjunto ordenado}
  Function dsMark(DataSet: TwsDataSet; Grupos: TStrings): TList; {--> Lista de Blocos}

{----------------------------------------------------------------------------------}

Type
  pTIndexRec = ^TIndexRec;
  TIndexRec  = Record
                 Ini: Longint;
                 Fim: Longint;
               End;
{ Herança
    TListIndex --> TList
  Objetivo
    Constrói uma lista de registros cuja finalidade básica é armazenar dois valores que, por
    exemplo, poderão servir como limites de variação de índices.

}
  TListIndex = Class(TList)
  Private
    // Obtém o registro
    Function GetIndex(Index: integer): TIndexRec;
  Public
    // libera espaço ocupado pela lista
    Destructor Destroy; Override;
    // Insere um índice
    Procedure  AddIndex(Ini, Fim: Longint);
    // Recupera um índice
    Property   NDX[index: Integer]: TIndexRec Read GetIndex;
  end;

  {Retornará um Sub-Conjunto de um Dataset formado pelas variáveis passadas} {29/08/1997}
  {A Condicao irá retirar as linhas que não passarem no teste}
  {Se a <Condicao> for nula o dataSet resultante terá tantas linhas quanto o original}
  {Se <Vars> for igual a 'NIL' o dataSet resultante terá tantas colunas quanto o original}
  {Indexs representam os intervalos selecionados, se Indexs igual a nil, todas as linhas
  serão pegas}
  Function dsSubDataSet(DataSet: TwsDataSet; Indexs: TListIndex;
                        Const Condicao: String; Vars: TStrings): TwsDataSet;

  Function GetIndexsFromString(Const S: String): TListIndex;

  {----------------------------------------------------------------------------------}
  {Cria um vetor com os índices das variáveis do Conjunto de dados} {29/08/1997}
  Function GruposToIndexs(DataSet: TwsDataSet; Grupos: TStrings): TwsLIVec;

  {Libera os Blocos alocados} {29/08/1997}
  Procedure BlocosFree(Blocos: TList);

  {Pré-Processa uma expressão aritmétia}
  Function PreProcessor(DS: TwsDataSet; const S: String): String;

  {Verifica se uma expressâo está correta}
  procedure EvalExpression(const Expr: String; DataSet: TwsDataSet);

  Function ApproxRandomSample(RData: TwsDataSet; Const Name, Rotulo: String; Col: TwsLIVec; p: Double): TwsDataSet;

  Function RandomSample(RData: TwsDataSet; Const Name, Rotulo: String; Col: TwsLIVec; k: Integer): TwsDataSet;

  function MultivarStat(Teta:TwsVec;npred,nresp,r,rc,rw:Integer; dfres: Double): TwsDataSet;

  (*
  Retorna o indice da variavel com nome s_var no DataSet.
  Caso nao exista uma variavel com o nome s_var entao
  retorna o indice da primeira variavel que contenha o Label s_var.
  Caso nao exista nenhuma variavel como o Label s_var, entao retorna -1.  *)
  Function GetIndexVar(DataSet: TWSDataSet; const s_var: string): Integer;

  procedure PrintInGroup(DS: TwsDataSet; Cols, Rows, Vars: TwsLIVec; x: TXML_Writer);

implementation
Uses Math,
     SysUtils,
     wsCVec,
     wsFuncoesDeProbabilidade,
     wsParser;

{ -----------------------------------------------------------------------------
                        Funções da parte de interface
  -----------------------------------------------------------------------------}

Function ExportaDataSet(DataSet:TwsDataSet): TStrings;
{ Objetivo
    Retorna numa lista de strings o conjunto de dados na forma de um arquivo texto utilizando
    o tipo não formatado, variáveis separadas com espaço em branco e observações separadas pelo
    caracter final de linha (CR)
}
var i,j: integer;
    s  : string;
    col: TwsDataSetCol;
begin
  Result := TStringList.Create;
  Result.Add('Tipo   = NaoFormatado');
  Result.Add('Nome   = ' + DataSet.Name);
  Result.Add('Rotulo = ' + DataSet.MLab);
  Result.Add('SEPVAR = ESPACO');
  Result.Add('SEPOBS = CR');
  Result.Add('SEPDEC = ' + DecimalSeparator);
  Result.Add('');
  Result.Add('<VARIAVEIS>');
  for i := 1 to DataSet.NCols do
    begin
    col := DataSet.Struct.Col[i];
    s := '';
    s := col.Name + ' = ';
    Case col.ColType of
      dtNumeric  : s := s + 'NUMER';
      dtQuant    : s := s + 'QUANT';
      dtQualit   : s := s + 'QUALI';
      dtQualitOrd: s := s + 'ORDEN';
    End; //Case
    Case col.ColType of
      dtQuant, dtQualit, dtQualitOrd:
      begin
      if TwsFactor(col).LevelType = tfFixed then
          s := s + ' Fixo'
      else
          s := s + ' Aleatorio'
      end;
    End;
    if col.Lab <> '' then
       s := s + ' "' + col.Lab + '"';
    Result.Add(s);
    end;
  Result.Add('');
  Result.Add('<DADOS>');
  for i := 1 to DataSet.NRows do
    begin
    s := '';
    for j := 1 to DataSet.NCols do
      s := s + Format('%10s', [DataSet.AsString[i,j]]);
    Result.Add(s);
    end;
end; //ExportaDataSet

Procedure LeArquivoFix(Var Arq:TextFile; EndSec, SepDec: Char; DChar: TCharSet; Lista: TList; Data:TwsDataSet;
  posicao:Longint; var NReg:Integer; var Erro:Integer);
{ Objetivo
    Constrói um conjunto de dados a partir de um arquivo texto, tendo as colunas com formato fixo ou
    seja, para ada coluna existe um início e um final fixos.
  Parâmetros:
    Arq    - Arquivo texto onde se encontra o conjunto de dados. Observe que o arquivo já deve estar
      aberto. A utilização desta rotina deve ser precedida de outra que obtém as definições dos
      atributos das colunas.
    EndSec - Caractere separador de observações.
    DChar  - Conjunto de caracteres utilizados para separar as variáveis ou colunas.
    Lista  - Lista com os atributos das variáveis (colunas) que o conjunto contém.
    DataSet- Deve ser informado um conjunto de dados já criado com 0 linhas n colunas, sendo n o
      número de colunas do conjunto.
    Posicao- Posicao a partir de onde começam os dados no arquivo texto. Se o arquivo texto só
      contiver os dados do conjunto, ou seja, não contiver o cabeçalho com as informações a
      respeito das variáveis, etc. então deverá ser informado 0 (zero) como valor deste parâmetro.
    NReg   : Retorna o número de registros (observações) lidas.
    Erro   : Retorna 0 se tudo ok; 1 se ocorreu algum erro;
}
Var
    NumReg, IndMat, i : LongInt;
    Dado              : String;
    L                 : TwsVec;
    Linha             : TwsCVec;
    Vari              : PwsRecFixFree;
    ch                : Char;
Begin
  NumReg:=0;
  DChar:=DChar+[EndSec];
  Erro := 0;
  Try
    if posicao > 0 then //ADG 08/12/99
       begin
       for i := 1 to posicao do
           Read(Arq, ch);
       end;
    While Not EOF(Arq) Do
      Begin
      Inc(NumReg);
      If NumReg mod 100 =0
        Then
          Begin
          NReg := NumReg;
          End; { If }
      Linha :=TwsCVec.Create;
      Linha.GetUntil(Arq, EndSec, DChar);
      If Linha.Len>1
        Then
          Begin
          L:=TwsDFVec.Create(Lista.Count);
          Data.MAdd(L);
          IndMat:=0;
          For i:=0 To Lista.Count-1 Do
            Begin
            Vari:= pwsRecFixFree(Lista.Items[i]);
            Dado:= Linha.Copy(Vari^.Inicio, Vari^.Final-Vari^.Inicio+1);
            If Vari^.Tipo_Var = 'I'
              Then
                Begin
                Data.Row[Data.NRows].Name:=Dado;
                Data.PrintOptions.MaxIdSize := Math.Max(Data.PrintOptions.MaxIDSize, Length(Dado)+3);
                End
              Else
                Begin
                Data.AsString[Data.NRows,IndMat+1]:=Dado;
                Inc(IndMat);
                End; { Else Vari^.Tipo='I' }
            End; { For i }
          End; { If Linha.Len>1 }
      Linha.Free;
      End; { While Not EOF }
  Except
    Erro := 1;
  End; //Try
End; { LeArquivoFix }

Procedure LeArquivoFree(Var Arq:TextFile; EndRec, EndField, SepDec: Char; DelChar: TCharSet; Lista:TList;
  Data:TwsDataSet; posicao:longint; var NReg:integer; var Erro:Integer);
{ Objetivo
    Constrói um conjunto de dados a partir de um arquivo texto considerando que as colunas são
    separadas por um conjunto qualquer de caracteres. É uma rotina para ser utilizada após
    a leitura dos atributios de cada coluna.
  Parâmetros
    Arq      - Arquivo texto (já aberto)
    EndRec   - Caracter separador de observações.
    EndFiels - Caractere separador de variáveis (colunas).
    DelChar  - Conjunto de caracteres para separação das linhas
    Lista    - Lista com atributos das variáveis (colunas) que o conjunto contém.
    Data     - conjunto de dados já criado com 0 linhas n colunas, sendo n é o número de colunas
      do conjunto de dados.
    Posicao  - Posição a partir de onde começam os dados no arquivo texto. Se o arquivo texto só
      contiver os dados do conjunto, ou seja, não contiver o cabeçalho com as informações a
      respeito das variáveis, etc. deverá ser informado 0 (zero) neste parâmetro.
    NReg     - Retorna o no. de registros lidos.
    Erro     - Retorna 0 se tudo ok; 1 se ocorreu algum erro;
}
Var
    k,IndMat,i,j,Posic: Integer;
    Dado              : String;
    L                 : TwsVec;
    Linha             : TwsCVec;
    CharSetEndField   : TCharSet;
    Vari              : PwsRecFixFree;
    ch, oldDS         : char;
Begin
  Erro := 0;


  oldDS := DecimalSeparator;
  DecimalSeparator := SepDec;
  Try

    Try
      if posicao > 0 then //ADG 08/12/99
         begin
         for k := 1 to posicao do
             Read(Arq, ch);
         end;

      NReg:=0;
      DelChar:=DelChar+[EndRec];
      CharSetEndField:=[EndField];
      While Not EOF(Arq) Do
        Begin
        Inc(NReg);
        Linha := TwsCVec.Create;
        Linha.GetUntil(Arq,EndRec,DelChar);
        If Linha.Len>1
          Then
            Begin
            L:=TwsDFVec.Create(Lista.Count);
            Data.MAdd(L);
            IndMat:=0;
            Posic := 1;
            For i := 0 To Lista.Count-1 Do
              Begin
              Vari := PwsRecFixFree(Lista.Items[i]);
              Dado := Linha.StrUntilChar(CharSetEndField,Posic);
              If Vari^.Tipo_Var ='I' Then
                 Begin
                 Data.Row[Data.NRows].Name:=Dado;
                 Data.PrintOptions.MaxIdSize:=Math.Max(Data.PrintOptions.MaxIdSize, Length(Dado)+3);
                 End
              Else
                 Begin
                 Data.AsString[Data.NRows,IndMat+1]:=Dado;
                 Inc(IndMat);
                 End; { Else Vari^.Tipo='I' }
              End; { For i }
            End; { If Linha.Len>1 }
        Linha.Free;
        End; { While Not EOF }

    Except
      Erro := 1;
    End; //Try

  finally
    DecimalSeparator := oldDS;
  end;
End; { LeArquivoFree }

Function LeDataSet(DataSet: TwsDataSet;
                   NArq: String;
                   Lista: TList;
                   Tipo: TwsEnumDlgType;
                   Var DelChar: TCharSet;
                   EndField, EndRec, SepDec: Char;
                   Var Erro: Integer;
                   var NReg: Integer): TwsDataSet;
{ Objetivo : Le um conjunto de dados a partir de um arquivo texto. O arquivo texto pode estar em
   formato livre ou fixo.
  Parãmetros
    DataSet : TwsDataSet ja criado.
    NArq    : Nome e caminho do arquivo texto aonde estao os dados.
    Lista   : Lista com a estrutura do DataSet.
    Tipo    : tdFix, para colunas no formato fixo, ou tdFree, para colunas no formato livre. No
      formato fixo, as colunas são indicadas por meio de uma posição inicial e final enquanto que,
      no formato livre, as colunas são separadas por caracteres separadores.
    DelChar : Caracteres utilizados para separadores de observações
    EndField: Caracter que indica o final de campo ou de coluna. Caso se deseje tdFix, passar
      qualquer caracter (de preferencia vazio) para a função.
    EndRec  : Caracter que indica o final de registro.
    NReg    : Numero de registros lidos.
    Erro    : Codigo de erro da funcao: 0, leitura sem problemas; 1 retorno com erro
}
  Var
    Arq    : TextFile;
    i      : Word;
    Vari   : PwsRecFixFree;
    Obj    : TwsDataSetCol;
Begin { LeDataSet }
  NReg := 0;

  Erro := DS_AdicionaColunas_e_Linhas(DataSet, Lista);
  if Erro <> 0 then Exit;

  Try
    Try
      AssignFile(Arq,NArq);
      Reset(Arq);
      Case Tipo Of
        tdFix  : LeArquivoFix (Arq, EndRec, SepDec, DelChar, Lista, DataSet, 0, NReg, Erro);
        tdFree : LeArquivoFree(Arq, EndRec, EndField, SepDec, DelChar, Lista, DataSet, 0, NReg, Erro);
        End; { Case }
      if Erro = 0 then
         Result := DataSet;
    Finally
      CloseFile(Arq);
    End; { Try }
  Except
    Erro:=1;
  End; { Try }
End; { LeDataSet }


(*************************************************************************)
Function DataSetFromText(Const NArq: String; Lista: TList; Tipo: TwsEnumDlgType;
                         Var DelChar: TCharSet; EndField, EndRec, SepDec: Char;
                         Var Erro: Integer; var NReg: Integer): TwsDataSet;
Begin
  Result := TwsDataSet.Create(ChangeFileExt(ExtractFileName(NArq), ''));
  LeDataSet(Result, NArq, Lista, Tipo, DelChar, EndField, EndRec, SepDec, Erro, NReg);
End; { DataSetFromText }

(******************** DS_ImportaTextoComCabecalho ****************************
Autor : Alex Dias Gonsales (07/12/99)
Função: Importa um arquivo texto com cabeçalho. Vide formato do arquivo na
        documentação no início desta unidade.
Parâmetros:
Nome_Arq - Nome do arquivo a ser aberto.
Erro     - Retorna:

Retorna: O DataSet lido ou Nil se ocorreu algum erro. Testar a variável Erro
         para ter certeza.
*)
Function DS_ImportaTextoComCabecalho(Nome_Arq: string; Var Erro:integer): TwsDataSet;
{ Objetivo
    Cria um conjunto de dados a partir de um arquivo texto. Um conjunto de dados nesse formato
    possui três seções de definição: a primeira, que deve iniciar o arquivo, define o conjunto
    e a forma como as informações podem ser lidas, a segunda, que deve iniciar com o rótulo
    <VARIAVEIS>, fornece as variáveis e os seus atributos e a terceira, que deve iniciar com o
    rótulo <DADOS>, os dados propriamente ditos. O formato dos dados pode ser por meio de colunas
    de tamanho fixo (cada coluna possui um início e um fim) ou livre (as colunas são separadas por
    meio de caracteres, usualmente o espaço em branco, vírgulas, aspas ou caracter de tabulação).
  Parâmetros
    Nome_Arq: nome do arquivo texto onde estão as informações a serem lidas
    Erro: código de erro. Caso Erro retorne um valor não nulo, a função retorna nil.
      0        - Leitura sem problemas
      401      - Erro ao Tentar abrir arquivo.
      301..399 - Erro no cabeçalho. Codigo retornado por DS_LeCabecalhoDoArquivoTexto.
      101..299 - Erro ao criar estrutura. Codigo retornado por DS_CriaEstrutura.
      1..99    - Erro retornado por LeArquivoFree ou LeArquivoFix.

  Observações
    O arquivo e' dividido em "CABECALHO" e "CORPO".
     - O cabecalho possui duas secoes: <OPCOES> e <VARIAVEIS>.
     - A secao <OPCOES> comeca no inicio do arquivo. Todos os seus componentes
       sao opcionais, quando nao for definido, sera' dado o valor default.
     - A secao VARIAVEIS comeca logo apos a linha que contiver a tag <VARIAVEIS>
     - Cada variavel, obrigatoriamente, devera conter NOME = TIPO. As demais especificacoes
       sao definidas por default

     O Corpo comeca logo apos a linha que contiver a tag <DADOS>

     Abaixo esta' a definicao do formato do arquivo:
     Obs.: O "*" (asterisco) utilizado aqui eh apenas para documentacao desta unidade, portanto nao
       deverá aparecer no arquivo texto.

   TEXTO  = [FORMATADO|NAOFORMATADO]               *Default = NAOFORMATADO
   NOME   = Nome_do_Conjunto                       *Default = NONAME
   ROTULO = Rótulo do conjunto                     *Default =
   ***SEPVAR = [ESPACO | TAB | ; | / | \ | " | ` ] *Default = ESPACO
   SEPOBS   = [CR           | ; | / | \ | " | ` ]  *Default = CR
   SEPDEC = [, | .]                                *Default = ,

   <VARIAVEIS>
   V1 = IDENT                  **[Ci Cf] "Rótulo"
   V2 = QUALI [FIXO|ALEATORIO] **[Ci Cf] "Rótulo"   Default=Fixo
   V3 = ORDEN [FIXO|ALEATORIO] **[Ci Cf] "Rótulo"   Default=Fixo
   V4 = QUANT [FIXO|ALEATORIO] **[Ci Cf] "Rótulo"   Default=Fixo
   V5 = NUMER                  **[Ci Cf] "Rótulo"

   Obs.:
   *   Valor default caso nao seja definido algum.
   **  Somente para texto formatado.
   *** Somente para texto não-formatado.
   V1..V5 são os nomes das variáveis.
   Ci e Cf significam: Coluna Inicial e Coluna Final respectivamente.

   Exemplos
Conjunto de dados não formatado (livre)

TIPO   = NAOFORMATADO
NOME   = Clover
ROTULO = Conteúdo de nitrogênio em plantas de trevo vermelho
SEPVAR = ESPACO
SEPOBS = CR
SEPDEC = ,

<VARIAVEIS>
Nitrog = NUMER "Quantidade de nitrogênio na planta"
Strain = QUALI FIXO "Cepas de bactérias"

<DADOS>
19,4    3Dok1
32,6    3Dok1
27,0    3Dok1
32,1    3Dok1
     ...
17,3    Compos
19,4    Compos
19,1    Compos
16,9    Compos
20,8    Compos

Conjunto de dados formatado

TIPO   = FORMATADO
NOME   = Dose_Formatado
ROTULO = Rotulo do conjunto
SEPOBS = CR
SEPDEC = ,

<VARIAVEIS>
SEXO = QUALI [1 3] "Sexo do paciente"
DOSE = QUANT [6 6] "Dose aplicada ao paciente"
X    = NUMER [8 9] "Horas de sono antes do medicamento"
Y    = NUMER [11 12] "Horas de sono depois do medicamento"

<DADOS>
FEM  0  8 10
FEM  0  4 10
FEM  0  0  7
   ...
MASC 1  0 12
MASC 2 15 12
MASC 2 12  9
MASC 2  9  9

}
var st_arq                : TStrings;
    Tipo,i,n_reg,Posicao  : integer;
    Nome,Rotulo           : String;
    SepVar,SepObs, SepDec : char;
    Vars                  : TList;
    Arq_Text              : TextFile;
begin
  Erro := 0;
  Result := Nil;
  st_arq := TStringList.Create;
  try
    st_arq.LoadFromFile(nome_arq);
  except
    st_arq.Free;
    erro := 401;
    exit;
  end;
  //Se ocoreu erro entao cai fora...
  Erro := DS_LeCabecalhoDoArquivoTexto(st_arq, Tipo, Nome, Rotulo,
                                       SepVar, SepObs, SepDec, Vars, posicao);
  if Erro <> 0 then
     begin
     st_arq.Free;
     Result := Nil;
     Exit;
     end;
   st_arq.Free;
   //Cria o DataSet e as colunas.
   Result := DS_CriaEstrutura(Nome, Rotulo, 0, Vars, Erro);
   if Erro = 0 then
      begin
      AssignFile(Arq_text,nome_arq);
      Reset(Arq_text);
      if Tipo = cTEXTO_NAO_FORMATADO then
         LeArquivoFree(Arq_text,SepObs,SepVar,SepDec,[#10, #13],Vars,Result,posicao,n_reg,erro)
      else
         LeArquivoFix(arq_text,SepObs,SepDec,[#10, #13],Vars,Result,posicao,n_reg,erro);
      CloseFile(Arq_text);
      end
   else
      begin
      Result.Free;
      Result := Nil;
      end;
   if erro <> 0 then
      begin
        Result.Free;
        Result := Nil;
      end;
end; //DS_ImportaTextoComCabecalho

Function DS_ImportaTextoAPartirDeTemplate(nome_arq_template, nome_arq_dados:string;
  Var Erro:integer): TwsDataSet;
{ Objetivo
     Importa um arquivo texto, utilizando um arquivo de template para a criação das variáveis. Neste
     caso, a definição das variáveis estão em outro arquivo e sua leitura é feita separadamente. Isto
     possibilita que uma mesma definição possa ser feita para vários conjuntos de valores das variáveis.
  Parâmetros:
    arq_template - Nome do arquivo de template (modelo).
    arq_dados    - Nome do arquivo com os dados.
    Erro         - Retorna:
                   0        - leitura sem problemas
                   401      - Erro ao Tentar abrir arquivo.
                   301..399 - Erro no arquivo de template. Codigo retornado por
                              DS_LeCabecalhoDoArquivoTexto.
                   101..299 - Erro ao criar estrutura. Codigo retornado
                              por DS_CriaEstrutura.
                   1..99    - Erro retornado por LeArquivoFree ou LeArquivoFix.
  Observações
    Leia observações em DS_ImportaTextoComCabecalho
}
var st_arq                 : TStrings;
    Tipo,i,n_reg, Posicao  : Integer;
    Nome,Rotulo            : String;
    SepVar,SepObs, SepDec  : char;
    Vars                   : TList;
    Arq_Text               : TextFile;
begin
  Erro := 0;
  Result := Nil;

  st_arq := TStringList.Create;
  Try
    st_arq.LoadFromFile(nome_arq_template);
  Except
    Erro := 401;
    Exit;
  End;

  Erro := DS_LeCabecalhoDoArquivoTexto(st_arq, Tipo, Nome, Rotulo,
                                  SepVar, SepObs, SepDec, Vars, posicao);
  //Se ocoreu erro entao cai fora...
  if Erro <> 0 then
     begin
     st_arq.Free;
     Result := Nil;
     Exit;
     end;

   st_arq.Free;

   //Cria o DataSet e as colunas.
   Result := DS_CriaEstrutura(Nome, Rotulo, 0, Vars, Erro);

   if Erro = 0 then
      begin
      AssignFile(Arq_text, nome_arq_dados);
      Reset(Arq_text);
      if Tipo = cTEXTO_NAO_FORMATADO then
         LeArquivoFree(Arq_text, SepObs, SepVar, SepDec, [#10, #13], Vars, Result, 0, n_reg, erro)
      else
         LeArquivoFix(Arq_text, SepObs, SepDec, [#10, #13], Vars, Result, 0, n_reg, erro);
      CloseFile(Arq_text);
      end
   else
      begin
      Result.Free;
      Result := Nil;
      end;

   if Erro <> 0 then
      begin
      Result.Free;
      Result := Nil;
      end;

end; //DS_ImportaTextoAPartirDeTemplate

(******************* DS_LeCabecalhoDoArquivoTexto ***************************
Autor: Alex Dias Gonsales (14/12/99)
Função: Lê as informações do cabeçalho de um arquivo texto. O formato do
        arquivo está descrito na documentaçãao no início desta unidade.

Parâmetros:
arq       : Arquivo texto entrado.
TipoTexto : Retorna o tipo do texto.
            0 - Formatado.
            1 - Não-Formatado (default).
Nome      : Retorna o Nome do conjunto
Rotulo    : Retorna o Rótulo do conjunto
SepVar    : Retorna o Separador de Variáveis
SepObs    : Retorna o Separador de Observações
Vars      : Retorna a Lista das variáveis(colunas)
Posição   : Retorna a posição (em bytes) dentro do arquivo onde começam os dados.
            É o primeiro byte após a tag <DADOS>.
            Retorna zero se não encontrou a tag <DADOS>.

Retorna:
0        - ok.
301..359 - Erro retornado por Ler_Opcoes.
371      - Tipo de Variavel Inválido.
372      - Tipo de Fator Inválido.
373      - Falta coluna inicial (somente para texto formatado).
374      - Falta coluna final (somente para texto formatado).
*)
Function DS_LeCabecalhoDoArquivoTexto(st_arq:TStrings;
         var TipoTexto: Integer;
         var Nome, Rotulo:String;
         var SepVar, SepObs, SepDec: Char;
         var Vars:TList;
         Var Posicao: Longint): Integer;
Const

    //Valor da chave no arquivo texto.
    cv_SEP_VAR  : array [1..7] of String = ('ESPACO', 'TAB', ';', '/', '\', '"', '''');
    //Valor atribuido à SepVar.
    cv_SEP_VAR2 : array [1..7] of Char   = (     ' ',    #9, ';', '/', '\', '"', '''');

    //Valor da chave no arquivo texto
    cv_SEP_OBS  : array [1..4] of String = ('CR', ';', '/', '\');

    //Valor que será atribuido à SepObs.
    cv_SEP_OBS2 : array [1..4] of Char   = ( #10, ';', '/', '\');

var
  st_chave : TStrings; //Lista contendo as palavras chaves;
  Linha    : Integer;

  {********************** LerOpcoes ****************************************
  Função auxiliar que le as
    opções do cabecalho (TEXTO, NOME, ROTULO, SEPVAR e SEPOBS).
  Retorna:
  0  - se nao ocorreu nenhum erro
  301  - erro na opcao TEXTO
  311 - erro na opcao NOME
  331 - erro na opcao SEPVAR
  341 - erro na opcao SEPOBS
  342 - erro na especificacao do separador de decimais
  351 - nao encontrou Tag <VARIAVEIS>

  *}
  Function LerOpcoes: Integer;
  var
    s_linha,s_token1,
    s_token2         : string;
    j,Count          : integer;
    fim              : boolean;
  Begin
    Count  := 0;
    Fim    := False;
    Result := 351;

    //Inicializa com valores default;
    TipoTexto := cTEXTO_NAO_FORMATADO;
    Nome := 'NONAME';
    Rotulo := '';
    SepVar := ' ';
    SepObs := #10;
    SepDec := ',';

    while not fim do
    Begin
      s_linha := st_arq[Linha];
      s_linha := LeftDelimiter(s_linha, '//');
      inc(Linha);
      fim := Linha >= st_arq.Count;

      //verifica se e' a Tag <VARIAVEIS>
      if CompareText(SysUtilsEx.AllTrim(s_linha),'<VARIAVEIS>')=0 then
         begin //Encontrou a tag <VARIAVEIS>
         Result := 0;
         exit;
         end;

      If SubStrings('=', s_token1, s_token2, s_linha) > 0 then

         Begin //SubStrings

         s_token1 := UpperCase(SysUtilsEx.AllTrim(s_token1));
         Count := st_chave.IndexOf(s_token1);

         Case Count of
           0:
           begin //TIPO=
           s_token2 := UpperCase(SysUtilsEx.AllTrim(s_token2));
           if CompareText(s_token2, 'FORMATADO')=0 then
              TipoTexto := cTEXTO_FORMATADO
           else
              if CompareText(s_token2, 'NAOFORMATADO')=0 then
                 TipoTexto := cTEXTO_NAO_FORMATADO
              else
                 if s_token2 = '' then
                    TipoTexto := cTEXTO_NAO_FORMATADO
                 else
                    begin
                    Result := 301;
                    exit;
                    end;
           end; //TEXTO=

           1:
           begin //NOME=
             s_token2 := SysUtilsEx.Alltrim(s_token2);
             if s_token2 = '' then
                Nome := 'NONAME'
             else
                if not SysUtils.IsValidIdent(s_token2) then
                   begin
                   Result := 311;
                   exit
                   end
                else
                   Nome := s_token2;
           end;//NOME=

           2:
           begin //ROTULO=
             Rotulo := s_token2;
           end;//ROTULO=

           3:
           begin //SEPVAR=
           if s_token2 <> '' then
              begin
              Result := 331;
              s_token2 := UpperCase(SysUtilsEx.AllTrim(s_token2));
              //Verifica se o token é válido e qual é ele.
              for j := 1 to Length(cv_SEP_VAR) do
                if s_token2 = cv_SEP_VAR[j] then
                   begin
                   SepVar := cv_SEP_VAR2[j];
                   Result := 0;
                   break; //Já encontrou o valor, não precisa mais procurar.
                   end;
              if Result <> 0 then //Separador não é um símbolo válido.
                 exit;
              end;
           end; //SEPVAR=

           4:
           begin //SEPOBS=
           if s_token2 <> '' then
              begin
              Result := 341;
              s_token2 := UpperCase(SysUtilsEx.AllTrim(s_token2));
              //Verifica se o token é válido e qual é ele.
              for j := 1 to Length(cv_SEP_OBS) do
                if s_token2 = cv_SEP_OBS[j] then
                   begin
                   SepObs := cv_SEP_OBS2[j];
                   Result := 0;
                   break; //Já encontrou o valor, não precisa mais procurar.
                   end;
              if Result <> 0 then //Separador não é um símbolo válido.
                 exit;
              end;
           end; //SEPOBS=

           5:
           begin //SEPDEC=
           if s_token2 <> '' then
              begin
              Result := 342; // <<<<< definir um codigo
              s_token2 := SysUtilsEx.AllTrim(s_token2);

              //Verifica se o token é válido e qual é ele.
              if s_token2[1] in ['.', ','] then
                 begin
                 SepDec := s_token2[1]; // devolve
                 Result := 0;
                 end;

              if Result <> 0 then //Separador não é um símbolo válido.
                 exit;
              end;
           end; //SEPDEC=

         End; //Case
         End; //SubStrings

    End; //While

  End; //LeOpcoes

  {*********************** tipo_var ******************************************
  Retorna o tipo da variavel.}
  Function tipo_var(st:TStrings; i:integer):Char;
  var s : string;
  begin
    try
      s := st[i];
    except
      Result := #0;
      Exit;
    end;
    if CompareText(s,'QUALI')=0 then
       result := 'A'
    else if CompareText(s,'QUANT')=0 then
       result := 'Q'
    else if CompareText(s,'ORDEN')=0 then
       result := 'O'
    else if CompareText(s,'NUMER')=0 then
       result := 'N' {Numeric}
    else if CompareText(s,'IDENT')=0 then
       result := 'I'
       else
          result := #0;
  end; //tipo_var

var
  fim           : boolean;
  s1,s2,s_linha : string;
  p             : PwsRecFixFree;
  st_tokens     : TStrings;
  i             : integer;
begin //DS_LeCabecalhoDoArquivoTexto

  Result := 0;
  Linha := 0;

  st_chave := TStringList.Create;
  st_chave.add('TIPO');
  st_chave.add('NOME');
  st_chave.add('ROTULO');
  st_chave.add('SEPVAR');
  st_chave.add('SEPOBS');
  st_chave.add('SEPDEC');

  Vars := TList.Create;

  //Le opcoes do conjunto.
  Result := LerOpcoes;
  if Result <> 0 then
     Exit;

  //Le informações das variáveis.
  fim := false;
  while (Result = 0) and (not fim) do
    begin
    //le linha
    s_linha := SysUtilsEx.AllTrim(st_arq[Linha]);
    inc(Linha);
    fim := Linha >= st_arq.count;

    if s_linha <> '' then
       begin
       {Separa os tokens do rotulo
       s1 contera' os tokens.
       s2 contera' o rotulo da variavel.
       ex.:
       s  = 'DOSE = QUANT Aleatorio "Quantidade da Droga"'
       s1 = 'DOSE = QUANT Aleatorio '
       s2 = 'Quantidade da Droga'    }
       if SubStrings('"', s1, s2, s_linha) > 0 then //ok
          RemoveChar(s2, '"')
       else //Erro
          begin
          s1 := s_linha;
          s2 := '';
          end;

       st_tokens := wsgLib.StrToStrList(s1);

       i := 0;
       //Testa se ja começou a secao dos DADOS.
       if CompareText(st_tokens[i],'<DADOS>')=0 then
          begin
          Posicao := System.Pos('<DADOS>', st_arq.Text) + 8;
          Exit;
          end;

       New(p);
       // Nome da variavel
       p.Nome := SysUtilsEx.AllTrim(st_tokens[i]);
       inc(i);
       p.Tipo_Var := tipo_var(st_tokens, i);

       //Testa se o Tipo de variavel é válido.
       if p.Tipo_Var = #0 then
          begin
          Result := 371;
          Exit;
          end;

       inc(i);

       //Se for variável tipo Fator...
       if (p.Tipo_var='A') or (p.Tipo_var='Q') or (p.Tipo_var='O') then
          Try //ver se foi definido tipo fator
            // tipo de nivel pode nao ter sido definido
            if i<st_tokens.Count then
              begin
              if CompareText(st_tokens[i], 'FIXO')=0 then
                 begin
                 p.Tipo_Fator := 'F';
                 inc(i);
                 end
              else
                 if CompareText(st_tokens[i], 'ALEATORIO')=0 then
                    begin
                    p.Tipo_Fator := 'A';
                    inc(i);
                    end
                 else
                    begin
                    //Tipo fator nao foi definido, usar valor default.
                    p.Tipo_Fator := 'F';

                    //Result := 372; //Tipo de Fator inválido
                    //Exit;
                    end
              end
            else
              p.Tipo_Fator := 'F';
          Except
            //Tipo fator nao foi definido, usar valor default.
            p.Tipo_Fator := 'F';
          End; //Try

       //Próximo campo depende do tipo de texto.
       if TipoTexto = cTEXTO_FORMATADO then
          begin //pega coluna inicial e final
          try
//            p.Inicio := StrToInt(wsGLib.Alltrim(st_tokens[i],['[']));
            p.Inicio := StrToInt(st_tokens[i]);
            inc(i);
          except
            Result := 373;
            exit;
          end; //try
          try
//            p.Final := StrToInt(wsGLib.Alltrim(st_tokens[i],[']']));
            p.Final := StrToInt(st_tokens[i]);
          except
            Result := 374;
            exit;
          end; //try
          end;

       p.Descricao := s2;
       Vars.Add(p);

       st_tokens.Free;
       end; //if s_linha <> ''

    end; //while

end; //DS_LeCabecalhoDoArquivoTexto
(************************* DS_CriaEstrutura **********************************
Autor : Alex Dias Gonsales (14/12/99).
Funcao: Retorna um DataSet com as colunas criadas. Usado na criação de novo
        conjunto de dados e na importação de conjunto de dados.
        Retorna NIL se ocorrer algum erro.
Parametros:
Nome   - Nome que será dado ao conjunto.
Rotulo - Rotulo que será dado ao conjunto.
Vars   - Lista com informação sobre as colunas que serão criadas.
Erro   - Retorna: 0        - ok
                  101..199 - Erro retornado de DS_AdicionaColunas_e_Linhas.
                  201      - Nenhuma variável definida.
                  202      - O Conjunto deve possuir no mínimo uma variável que
                             não seja identificadora.
*)
Function DS_CriaEstrutura(Nome, Rotulo:string; NumLinhas: Integer; Vars:TList; Var Erro:Integer):TwsDataSet;
var P: PwsRecFixFree;
begin

  if (Vars.Count = 0) then //Erro
     begin
     Erro := 201;
     Result := Nil;
     Exit;
     end;

  Try

    P := Vars[0];
    if (P.Tipo_Var = 'I') And (Vars.Count = 1) then //Erro
       begin
       Erro := 202;
       Result := Nil;
       Exit;
       end;

    //Cria o DataSet.
    Result := TwsDataSet.Create(Nome);
    Erro := DS_AdicionaColunas_e_Linhas(Result, Vars, NumLinhas);
    Result.MLab := Rotulo;
  Except
    Result.Free;
    Result := Nil;
  End //Try
end; //DS_CriaEstrutura:

(********************** DS_AdicionaColunas ***********************************
Autor : Alex Dias Gonsales (14/12/99).
Funcao: Adiciona colunas num Conjunto de Dados. O parâmetro Vars contém
        uma lista com as informações para a criação das colunas.
        Se NRows do conjunto for maior que zero então adiciona também valores
        nulos em todas as linhas para as colunas que foram recém criadas.
        Vide método AddCol.
Parâmetros:
DataSet - Conjunto no qual serã criadas as colunas. Já deve vir criado,
          preferencialmente com 0(zero) linhas.
Vars    - Lista com informação sobre as colunas que serão criadas.

Retorna:
  0   - ok
  101 - Erro na criação das Colunas.
*)
Function DS_AdicionaColunas_e_Linhas(DataSet: TwsDataSet; Vars: TList; NumLinhas: Integer = 0): Integer;
var i   : integer;
    Vari: PwsRecFixFree;
    Obj :TwsDataSetCol;
begin
  Result := 0;
  Try
    //Cria as colunas.
    for i := 0 To Vars.Count-1 Do
      begin
      Vari := PwsRecFixFree(Vars.Items[i]);
      if Vari^.Tipo_Var ='I' Then
          DataSet.ColIdentName := Vari^.Nome
      else
          begin
          case Vari^.Tipo_Var Of
            'Q'  :Obj:=TwsQuantitative.Create (Vari^.Nome,Vari^.Descricao);
            'A'  :Obj:=TwsQualitative.Create  (Vari^.Nome,Vari^.Descricao);
            'O'  :Obj:=TwsOrdered.Create      (Vari^.Nome,Vari^.Descricao);
            'N'  :Obj:=TwsNumeric.Create      (Vari^.Nome,Vari^.Descricao);
          end; //Case Vari^.Tipo_Var
          //Se a varivel for tipo Fator entao...
          case Vari^.Tipo_Var of
          'Q', 'A', 'O':
            //verifica se e' Fixo ou Aleatorio.
            case Vari^.Tipo_Fator Of
              'F' : TwsFactor(Obj).LevelType := tfFixed;
              'A' : TwsFactor(Obj).LevelType := tfRandom;
            end; //Case
          end; //Case
          DataSet.Struct.AddColEx(Obj);
          end;
      end; //for i
    for i := 1 to NumLinhas do
      DataSet.MAdd(TwsDFVec.Create(Vars.Count));
  Except
    Result := 101;
  End //Try
end; //DS_AdicionaColunas_e_Linhas

Function GruposToIndexs(DataSet: TwsDataSet; Grupos: TStrings): TwsLIVec;
Var i,j: Integer;
Begin
  Result := TwsLIVec.Create(Grupos.Count);
  For i := 0 to Grupos.Count-1 do
    For j := 1 to DataSet.NCols do
      If CompareText(Grupos.Strings[i], DataSet.Struct.Col[j].Name) = 0 Then
         Begin
         Result[i+1] := j;
         Break;
         End;
End; {GruposToIndexs}

{--------------------------- // ---------------------------- // ---------------------------}

Procedure BlocosFree(Blocos: TList);
var i: Integer;
Begin
  For i := 0 to Blocos.Count-1 do Dispose(pwsRecBloco(Blocos[i]));
  Blocos.Free;
End; {BlocosFree}

{--------------------------- // ---------------------------- // ---------------------------}

Procedure AtualizaVariaveis(Tab: TwsTabVar; DS : TwsDataSet; i : Longint);
Var J : Longint;
Begin
  For j := 1 to DS.NCols do
    Tab.SetFloatValue(DS.Struct.Col[j].Name, DS[i,j]);
End; {AtualizaVariaveis}

{--------------------------- // ---------------------------- // ---------------------------}

Function ModelFrame(Variaveis: TStrings;
                    Grupos: TStrings;
                    Const Condicao: String;
                    DataSet: TwsDataSet;
                    NomesVar: TStrings = nil;
                    UsaNomesNosCalculos: Boolean = False): TwsDataSets;
{ Objetivo
    Dado um conjunto de dados, retorna ma lista com um ou mais conjuntos utilizando expressões, grupos
    e filtros. Esta é a rotina de tratamento de conjuntos de dados mais importante do WinStat,
    possibilitando a obtenção de subconjuntos, eliminação de linhas e criação de novas variáveis.
  Parâmetros
    Variáveis: variáveis que estarão presentes no conjunto construído. Poderão ser variáveis já
      existentes ou variáveis a serem obtidas como expressões
    Grupos: Os grupos são definidos por uma série de variáveios do tipo fator. A sua definição
      provoca a quebra do conjunto de dados em subgrupos, com um conjunto para cada combinação
      dos níveis dos fatores.
    Condição: expressão do tipo verdadeiro/falso que é aplicada a cada linha do conjunto de dados,
      perservando somente aquelas para as quais a resposta é verdadeira.
    DataSet: conjunto de dados a partir do qual serão realizadas todas as operações
    NomesVar: retorna os nomes das variáveis envolvidas
    UsaNomesNosCalculos: true se os nomes das variáveis serão gerados
}
Var
    DS                       : TwsDataSet;
    Blocos                   : TList;
    EvalList                 : TList;
    EvalCond                 : TAvaliator;
    Linha, V2, lvOld         : TwsVec;
    j,IndiceDoBloco,n        : Integer;
    NumVar                   : Integer;
    Linhas,i,k               : Longint;
    Resp                     : TEvalResult;
    S, sAux                  : String;
    Vars                     : TStrings; {Rochedo, 25/05/1998}
    NomesVarCriadoLocalmente : Boolean;  {Rochedo, 25/01/1999}
    VarsFiltro               : TStrings; {zeh, 1/8/2000 -> Lista de fatores envolvidas no filtro}
    tLevel                   : Integer;  {Zeh, 2/8/2000 -> Posição temporária na lista de níveis}
    Col                      : Integer;  {Zeh, 2/8/2000 -> Indice da coluna}
    lOld                     : tStrings; {Zeh, 2/8/2000 -> Lista dos níveis antigos}
    vAux                     : Double;

    Procedure LiberaRecursos;
    Var i : Integer;
    Begin
      For i := 0 to EvalList.Count-1 do TAvaliator(EvalList[i]).Free;
      EvalList.Free;
      If Condicao <> '' Then EvalCond.Free;
      VarsFiltro.Free;
    End;

    {Descritores de Colunas: Quando as colunas existirem no DataSet original, apenas
     copiamos seus descritores para o novo conjunto, senão as criamos (expressão).}
    Procedure CriaDescritoresDeColunas(DS: TwsDataSet);
    Var Col    : TwsDataSetCol;
        K,KK   : Word;
        Achou  : Boolean;
    Begin
      Achou := False;
      For k := 0 to Vars.Count-1 do
        Begin
        For KK := 1 to DataSet.nCols do
          If CompareText(DataSet.Struct.Col[KK].Name, Vars[k]) = 0 Then
             Begin
             Achou := True;
             Break;
             End;

        If Achou Then
           Begin
           Col := CopyDescCol(DataSet.Struct.Col[KK]);
           Achou := False;
           End
        Else
           if (NomesVar <> nil) and (NomesVar[k] <> '') Then
              Col := TwsNumeric.Create(NomesVar[k], Vars[k]) {Rochedo, 25/01/1999}
           else
              Col := TwsNumeric.Create(Vars[k], Vars[k]);

        DS.Struct.AddColEx(Col);
        End;
    End; {CriaDescritoresDeColunas}

Begin
  NomesVarCriadoLocalmente := False;

  If (Variaveis = Nil) or (Variaveis.Count = 0) Then
     raise Exception.Create('Não há expressões ou variáveis a serem calculadas')
  Else
     Vars := Variaveis;

  if NomesVar <> nil Then
     if NomesVar.Count <> Vars.Count Then
        raise Exception.CreateFmt(
          'Não há correspondência entre os nomes e as variáveis'#13 +
          'Quantidade de Nomes: %d'#13 +
          'Quantidade de Expressões: %d', [NomesVar.Count, Vars.Count])
     else
  else
     begin  {Rochedo, 25/01/1999}
     NomesVarCriadoLocalmente := True;
     NomesVar := TStringList.Create;
     For i := 0 to Vars.Count-1 do
       begin
       j := System.Pos(':=', Vars[i]);
       If j > 0 Then
          NomesVar.Add( SysUtilsEx.AllTrim(System.Copy(Vars[i], 1, j-1)) )
       else
          NomesVar.Add('');
       end;
     end;

  Result := TwsDataSets.Create;
  VarsFiltro := TStringList.Create;

 {Cria uma lista que conterá os avaliadores}
  EvalList := TList.Create;

 {Cria tantos avaliadores quanto o número de Variáveis}
  For i := 1 to Vars.Count do
    EvalList.Add(TAvaliator.Create);

  {Pega os nomes das colunas}
  For i := 1 to DataSet.nCols do
    TAvaliator(EvalList[0]).TabVar.AddFloat(DataSet.Struct.Col[i].Name, 0);

  {Nomes das variáveis que serão criadas}
  if (NomesVar <> nil) and UsaNomesNosCalculos Then  //Rochedo - 18/12/1998
     For i := 0 to NomesVar.Count-2 do // A última variável não entra
       TAvaliator(EvalList[0]).TabVar.AddFloat(NomesVar[i], 0);

  If Vars.Count > 1 Then
    For i := 1 to Vars.Count-1 do
      TAvaliator(EvalList[i]).TabVar := TAvaliator(EvalList[0]).TabVar;

  {Verifica se não ha erro na condição}
  If Condicao <> '' Then
    Try
      EvalCond := TAvaliator.Create;
      EvalCond.TabVar := TAvaliator(EvalList[0]).TabVar;
      s := PreProcessor(DataSet, Condicao);  // 03/05/1999 - Rochedo
      EvalCond.Expression := s;
      for i:= 1 to TAvaliator(EvalList[0]).TabVar.NVars do
        if DataSet.Struct.Col[i].ColType <> dtNumeric then
          VarsFiltro.Add(TAvaliator(EvalList[0]).TabVar.Vars[i-1].Name);
    Except
      On E: Exception do
        Begin
        LiberaRecursos;
        raise Exception.CreateFmt('Erro na Condição <%s>'#13'%s.', [Condicao, E.Message]);
        End;
    End;

  {Verifica se não ha erro nas Variáveis}
  Try
     For i := 0 to Vars.Count - 1 do
       begin
       j := System.Pos(':=', Vars[i]); {Rochedo, 25/01/1999}
       if j > 0 then s := System.Copy(Vars[i], j+2, Length(Vars[i])) else s := Vars[i];
       TAvaliator(EvalList[i]).Expression := s;
       end;
  Except
    On E: Exception do
      Begin
      LiberaRecursos;
      raise Exception.CreateFmt('Erro na Expressão <%s>'#13'%s.', [s, E.Message]);
      End;
  End;

  {Ordena o DataSet Pelo Grupos dados}
  DSSort(DataSet, Grupos, True, '');

  {Varre o DataSet para escolher as linhas que serão usadas nos cálculos}
  Linhas := DataSet.NRows;

  Try
    Blocos := dsMark(DataSet, Grupos);
    IndiceDoBloco := 0;

    {Pelo menos um bloco tem que existir}
    DS := TwsDataSet.Create(Format(DataSet.Name+'_%d',[IndiceDoBloco+1]));
    CriaDescritoresDeColunas(DS);
    Result.Add(DS);

    i := 0;
    NumVar := Vars.Count-1;
    Repeat
      Inc(i);
      If i > DataSet.NRows Then
         Break
      Else
         Linha := DataSet.Row[i];

      if TwsRecBloco(Blocos[IndiceDoBloco]^).Nome <> '' then
         DS.MLab := 'Grupo: ' + TwsRecBloco(Blocos[IndiceDoBloco]^).Nome
      else
        DS.MLab := '';
      If i > TwsRecBloco(Blocos[IndiceDoBloco]^).Fim Then
        Begin
        Inc(IndiceDoBloco);
        DS := TwsDataSet.Create(Format(DataSet.Name+'_%d',[IndiceDoBloco+1]));
        if TwsRecBloco(Blocos[IndiceDoBloco]^).Nome <> '' then
           DS.MLab := 'Grupo: ' + TwsRecBloco(Blocos[IndiceDoBloco]^).Nome
        else
          DS.MLab := '';
        CriaDescritoresDeColunas(DS);
        Result.Add(DS);
        End;

      AtualizaVariaveis(TAvaliator(EvalList[0]).TabVar, DataSet, i);

      If Condicao <> '' Then
         Begin
         Resp := EvalCond.Evaluate;
         If Resp.AsFloat = 1 {True} Then
            Begin
            V2 := TwsDFVec.Create(Vars.Count);
            For k := 0 to NumVar do
               begin
               V2[k+1] := TAvaliator(EvalList[k]).Evaluate.AsFloat;
               if UsaNomesNosCalculos and (k < NumVar) Then
                  TAvaliator(EvalList[0]).TabVar.SetFloatValue(NomesVar[k], V2[k+1]);
               end;
            End
         Else
            Continue;
         End
      Else
         Begin
         V2 := TwsDFVec.Create(Vars.Count);
         For k := 0 to NumVar do
            begin
            V2[k+1] := TAvaliator(EvalList[k]).Evaluate.AsFloat;
            if UsaNomesNosCalculos and (k < NumVar) Then
               TAvaliator(EvalList[0]).TabVar.SetFloatValue(NomesVar[k], V2[k+1]);
            end;
         End;

      Result[IndiceDoBloco].MAdd(V2);
    Until False;

    {Técnica para liberar e deletar coisas de uma TList sem problemas}
    i := 0;
    While i < Result.Count do
      Begin
      If Result[i].NRows = 0 Then
        Begin
        Result[i].Free;
        Result.Delete(i);
        End
      Else
        Inc(i);
      End;

    If Condicao <> '' Then
       try
       lOld:= TStringList.Create;
       for n:= 0 to Result.Count-1 do
         Begin
         DS := Result[n];
         for k:= 0 to VarsFiltro.Count-1 do
           Begin
           Col:= DS.Struct.IndexOf(VarsFiltro[k]);
           if Col <> -1 then
             Begin
             // copia nomes dos niveis
             lOld.Assign(TwsFactor(DS.Struct.Col[Col]).LevelNames);
             // se for quantitativo copia tb os valores dos niveis
             if (DS.Struct.Col[Col] is TwsQuantitative) then
               lvOld := TwsQuantitative(DS.Struct.Col[Col]).LevelValues.Copy(1,
                 TwsQuantitative(DS.Struct.Col[Col]).LevelValues.Len);
             // limpa tudo (incluindo valores dos niveis)
             TwsFactor(DS.Struct.Col[Col]).LevelsClear;
             for i:= 1 to DS.NRows do
               Begin
               tLevel:= DS.AsInteger[i,Col];
               sAux:= lOld[tLevel];
               if (DS.Struct.Col[Col] is TwsQuantitative) then
                 vAux:=lvOld[tLevel+1];
               // Esta funcao ja nao retorna tLevel?
               TwsFactor(DS.Struct.Col[Col]).AddLevel(sAux);
               tLevel:= TwsFactor(DS.Struct.Col[Col]).LevelToIndex(sAux);
               // se for quantitativo, atribui valor antigo
               if (DS.Struct.Col[Col] is TwsQuantitative) then
                 TwsQuantitative(DS.Struct.Col[Col]).SetLevelValue(sAux,vAux);
               DS[i, Col]:= tLevel;
               End;
             End;
           End;
         End;
       finally
         lvOld.Free;
         lOld.Free;
       End;

  Finally
    LiberaRecursos;
    BlocosFree(Blocos);
    if NomesVarCriadoLocalmente then NomesVar.Free;
  End;
End; { Model Frame } {--> TList (lista de DataSets)}

{--------------------------- // ---------------------------- // ---------------------------}

Function dsSort(DataSet: TwsDataSet; Grupos: TStrings; Asc: Boolean; Const New: String): TwsDataSet;
{ Objetivo
    Ordena o conjuntos de dados
  Parâmetros
    DataSet: conjunto de dados a ordenar
    Grupos: lista com os rótulos das colunas a ordenar
    Asc: True se a ordenação for ascendente; false para descendente
    New: nome do novo conjunto já ordenado. Se New for vazio, a ordenação será realizada no próprio
    conjunto
  Observações
    As colunas indicadas em Grupos são tomadas como chaves na ordem em que aparecem na lista
}
Var i        : Integer;
    Column,
    Ascend   : TwsLIVec;
Begin
  If (Grupos = Nil) or (Grupos.Count = 0) Then
     Result := DataSet {Não executa nada}
  Else
     Begin
     {Ordena o DataSet Pelos Grupos dados}
     Column := TwsLIVec.Create(Grupos.Count);
     Ascend := TwsLIVec.Create(Grupos.Count);
     For i := 0 to Grupos.Count-1 do
       Begin
       Ascend[i+1] := Longint(Asc);
       Column[i+1] := DataSet.Struct.IndexOf(Grupos[i]);
       End;

     If (New = '') Then
        Result := DataSet
     Else
        Begin
        Result := DataSet.Copy;
        Result.Name := New;
        End;

     Try
       Result.SortRows(Column, Ascend);
     Finally;
       Column.Free;
       Ascend.Free;
     End;
  End; {If Grupos.Count > 0}
End; {dsSort} {Ordena um DataSet} {--> TwsDataSet ou o Próprio}

{--------------------------- // ---------------------------- // ---------------------------}

Function dsMark(DataSet: TwsDataSet; Grupos: TStrings): TList;
{ Objetivo
    Cria uma lista de objetos TListIndex cuja finalidade é marcar a linha inicial e final de
    cada subgrupo de linhas do conjunto de dados
  Parâmetros
    DataSet: conjunto de dados para demarcação dos grupos
    Grupos: colunas do tipo fator do conjunto de dados. Cada combinação dos níveis desses
      fatores comporá um subgrupo
}
var
  ValoresDosBlocos,         {Valores dos grupos para uma dada linha do conjunto}
  IndiceDosGrupos  : TwsLIVec; {Índice das colunas de um DataSet}

    Procedure PegaValoresDosBlocos(LinhaAtual, Valores: TwsVec);
    Var i: Integer;
    Begin
      For i := 1 to IndiceDosGrupos.Len do
         Valores[i] := LinhaAtual[IndiceDosGrupos[i]];
    End; {PegaValoresDosBlocos}

    Function BlocoMudou(LinhaAtual, Valores: TwsVec): Boolean;
    Var i: Integer;
    Begin
      Result := False;
      For i := 1 to IndiceDosGrupos.Len do
        If Valores[i] <> LinhaAtual.Data[IndiceDosGrupos[i]] Then
           Begin
           Result := True;
           Break;
           End;
    End; {BlocoMudou}

    Function GruposToStringName(LinhaAtual: TwsVec): String;
    Var i,ii  : Integer;
        Index : Longint;
        C     : TwsFactor;
    Begin
      Result := '';
      For i := 1 to IndiceDosGrupos.Len do
        Begin
        ii := IndiceDosGrupos[i];
        C  := TwsFactor(DataSet.Struct.Col[ii]);
        If C.ColType <> dtNumeric Then
           Begin
           Index := LinhaAtual.AsInteger[ii];
           If Index < 0 Then
              Result := 'Bloco_Nulo'
           Else
              Result := Result + C.LevelNames[Index] + '.' ;
           End
        Else
           Begin
           //gOutPut.Write(Format('Variável <%s> é Numérica, '+'não possui Níveis.',[C.Name])); // <<<<<
           Result := C.Name;
           Exit;
           End;
        End;
        If Length(Result) > 0 Then Delete(Result, Length(Result), 1);
    End; {GruposToStringName}

var
  Bloco : pwsRecBloco;
  i     : Integer;

Begin {dsMark}
  Result := TList.Create;
  IndiceDosGrupos := nil;
  ValoresDosBlocos := nil;
  Try
    Try
      If (Grupos <> Nil) and (Grupos.Count > 0) Then
         IndiceDosGrupos := GruposToIndexs(DataSet, Grupos);

      New(Bloco);
      Bloco^.Inicio := 1;
      If (Grupos = Nil) or (Grupos.Count = 0) Then
         Begin
         Bloco^.Fim := DataSet.NRows;
         Bloco^.Nome := '';
         Result.Add(Bloco);
         End
      Else
         Begin
         ValoresDosBlocos := TwsLIVec.Create(Grupos.Count);
         PegaValoresDosBlocos(DataSet.Row[1], ValoresDosBlocos);
         For i := 2 to DataSet.NRows do
           Begin
           If BlocoMudou(DataSet.Row[i], ValoresDosBlocos) Then
              Begin
              Bloco^.Fim := i-1;
              Bloco^.Nome := GruposToStringName(DataSet.Row[i-1]);
              Result.Add(Bloco);
              New(Bloco);
              Bloco^.Inicio := i;
              PegaValoresDosBlocos(DataSet.Row[i], ValoresDosBlocos);
              End;
           If i = DataSet.NRows Then
             Begin
             Bloco^.Fim := i;
             Bloco^.Nome := GruposToStringName(DataSet.Row[i]);
             Result.Add(Bloco);
             End;
           End;

         {Remove os blocos nulos, isto é, os blocos com valores MissValue}
         i := 0;
         While i < Result.Count do
           begin
           If TwsRecBloco(Result[i]^).Nome = 'Bloco_Nulo' Then
              begin
              Result.Delete(i);
              continue;
              end;
           inc(i);
           end;

         End;
    Except
      Result.Free;
      Result := Nil;
    End
  Finally
    If Assigned(ValoresDosBlocos) Then ValoresDosBlocos.Free;
    If Assigned(IndiceDosGrupos) Then IndiceDosGrupos.Free;
  End;
End; {dsMark} {Marcação dos Blocos} {--> TList (Lista dos blocos)}

{--------------------------- // ---------------------------- // ---------------------------}

Function dsSubDataSet(DataSet: TwsDataSet;
                      Indexs: TListIndex;
                      Const Condicao: String;
                      Vars: TStrings): TwsDataSet;
{ Objetivo

  Parâmetros

}
var
  EvalList  : TList;
  EvalCond  : TAvaliator;
  i,j,K,
  Ini,Fim,
  nIndexs,
  Linha     : Integer; {Linha atual do novo conjunto}
  s         : String;
  vx,lvOld  : TwsVec;
  VarsFiltro: TStrings; {zeh, 1/8/2000 -> Lista de fatores envolvidas no filtro}
  tLevel    : Integer;  {Zeh, 2/8/2000 -> Posição temporária na lista de níveis}
  Col       : Integer;  {Zeh, 2/8/2000 -> Indice da coluna}
  lOld      : tStrings; {Zeh, 2/8/2000 -> Lista dos níveis antigos}
  vAux      : Double;
  
    Procedure LiberaRecursos;
    Var i : Integer;
    Begin
      TAvaliator(EvalList[0]).Free;
      If Vars <> Nil Then
         For i := 1 to EvalList.Count-1 do TAvaliator(EvalList[i]).Free;
      EvalList.Free;
      If Condicao <> '' Then
        begin
        EvalCond.Free;
        lOld.Free;
        VarsFiltro.Free;
        lvOld.Free
        end
    End; {LiberaRecursos}

    Procedure CopiaDados;
    Var k : Integer;
        V : TwsVec;
    Begin
      Inc(Linha);

         If Vars <> Nil Then
            V := TwsDFVec.Create(Vars.Count)
         Else
            V := TwsDFVec.Create(DataSet.NCols);

      If Vars <> Nil Then
            Begin
            V.Name := DataSet.RowName[i];
            For k := 0 to Vars.Count-1 do
              V[k+1] := TAvaliator(EvalList[k]).Evaluate.AsFloat;
            End
      Else
            Begin
            vx := TwsDFVec(DataSet.Row[i]);
            V.Name := vx.Name;
            For k := 1 to DataSet.NCols do V[k] := vx[k];
            End;
      Result.MAdd(V);
    End;

Begin {dsSubDataSet}
  lvOld:=nil;
 {Cria uma lista que conterá os avaliadores}
  EvalList := TList.Create;

 {Cria tantos avaliadores quanto o número de Variáveis}
  EvalList.Add(TAvaliator.Create); {Pelo menos 1 avaliador}
  If Vars <> Nil Then
     For i := 1 to Vars.Count-1 do
       EvalList.Add(TAvaliator.Create);

 {Pega os nomes das colunas}
  For i := 1 to DataSet.nCols do
    TAvaliator(EvalList[0]).TabVar.AddFloat(DataSet.Struct.Col[i].Name, 0);

  If (Vars <> Nil) and (Vars.Count > 1) Then
     For i := 1 to Vars.Count-1 do
        TAvaliator(EvalList[i]).TabVar := TAvaliator(EvalList[0]).TabVar;

  {Verifica se não ha erro na condição}
  If Condicao <> '' Then
    Try
      VarsFiltro:= TStringList.Create;
      EvalCond := TAvaliator.Create;
      EvalCond.TabVar := TAvaliator(EvalList[0]).TabVar;
      s := PreProcessor(DataSet, Condicao);  // 03/05/1999 - Rochedo
      EvalCond.Expression := s;
      for i:= 1 to TAvaliator(EvalList[0]).TabVar.NVars do
        if DataSet.Struct.Col[i].ColType <> dtNumeric then
          VarsFiltro.Add(TAvaliator(EvalList[0]).TabVar.Vars[i-1].Name);
    Except
      On E: Exception do
        Begin
        LiberaRecursos;
        Result := Nil;
        Raise Exception.CreateFmt('Erro na Condição <%s>'#13'%s.', [Condicao, E.Message]);
        End;
    End;

  {Verifica se não ha erro nas Variáveis}
  Try
     If Vars <> Nil Then
        For i := 0 to Vars.Count-1 do
          TAvaliator(EvalList[i]).Expression := Vars[i];
  Except
    On E: Exception do
      Begin
      LiberaRecursos;
      raise Exception.CreateFmt('Erro na Expressão <%s>'#13'%s.', [Vars[i], E.Message]);
      End;
  End;

  If Assigned(Indexs) Then
     nIndexs := Indexs.Count
  Else
     nIndexs := 1;

  Result := TwsDataSet.Create(DataSet.Name);
  {CRIA DESCRITORES DE COLUNAS E OUTRAS INFORMAÇÕES}
  If Vars <> Nil Then
     For i := 0 to Vars.Count-1 do
       Result.Struct.AddColEx(CopyDescCol(DataSet.Struct.ColByName(Vars[i])))
  Else
     For i := 1 to DataSet.NCols do
       Result.Struct.AddColEx(CopyDescCol(DataSet.Struct.Col[i]));

  Linha := 0; {É usado globalmente}
  {Para cada faixa do dataSet}
  For j := 0 To nIndexs-1 do
    Begin
    If Assigned(Indexs) Then
       Begin
       Ini := Indexs.NDX[j].Ini;
       Fim := Indexs.NDX[j].Fim;
       if Ini < 1 then
          Ini := 1;
       if Fim > DataSet.NRows Then
          Fim := DataSet.NRows;
       End
    Else
       Begin
       Ini := 1;
       Fim := DataSet.nRows;
       End;

    For i := Ini to Fim do
      Begin
      AtualizaVariaveis(TAvaliator(EvalList[0]).TabVar, DataSet, i);
      If Condicao <> '' Then
         If EvalCond.Evaluate.AsFloat = 1 {True} Then
           CopiaDados
         Else
           Continue
      Else
        CopiaDados;
      End; { For i}
    End; { For j }

  // Verifica se na exclusao de linhas foram tambem excluidos niveis completamente
  If Condicao <> '' Then
    Begin
    lOld:= TStringList.Create;
    for k:= 0 to VarsFiltro.Count-1 do
      Begin
      Col:= Result.Struct.IndexOf(VarsFiltro[k]);
      if Col <> -1 then
        Begin
        lOld.Assign(TwsFactor(Result.Struct.Col[Col]).LevelNames);
        // se for quantitativo copia tb os valores dos niveis
        if (Result.Struct.Col[Col] is TwsQuantitative) then
          lvOld := TwsQuantitative(Result.Struct.Col[Col]).LevelValues.Copy(1,
            TwsQuantitative(Result.Struct.Col[Col]).LevelValues.Len);
        // limpa tudo (incluindo valores dos niveis)
        TwsFactor(Result.Struct.Col[Col]).LevelsClear;
        for i:= 1 to Result.NRows do
          Begin
          tLevel:= Result.AsInteger[i, Col];
          s:= lOld[tLevel];
          if (Result.Struct.Col[Col] is TwsQuantitative) then
            vAux:=lvOld[tLevel+1];
          TwsFactor(Result.Struct.Col[Col]).AddLevel(s);
          tLevel:= TwsFactor(Result.Struct.Col[Col]).LevelToIndex(s);
          // se for quantitativo, atribui valor antigo
          if (Result.Struct.Col[Col] is TwsQuantitative) then
            TwsQuantitative(Result.Struct.Col[Col]).SetLevelValue(s,vAux);
          Result[i, Col]:= tLevel;
          End;
        End; // if Col
      End; // for k
    End; // if Condicao
  LiberaRecursos;
End; {dsSubDataSet} {--> TwsDataSet (Subconjunto do original)}

Function GetIndexsFromString(Const S: String): TListIndex;
{ Objetivo
    Analisa um string e constrói uma lista de índices
  Parâmetros
    S: string para interpretação
  Observação
    O string contém índices solitários ou na forma de amplitude, cujos extermos são representados
    pelo caracter - (traço). Por exemplo: 1 5-9 14 20-25 irá gerar uma lista com quatro componentes
    correspondentes aos índices
    0 --> 1, 1
    1 --> 5, 9
    2 --> 14, 14
    3 --> 20, 25
}
const
  BufLen = 150;
  RangeChar = '-';
var
  i, j,k,Hi,Lo: Cardinal;
  st       : String;
begin
  i := 1;
  k := 1;
  Result := TListIndex.Create;
  repeat
    st := StrToken(s, i, DelChar);
    if st <> '' then
      begin
      j := System.Pos(RangeChar, st);
      if j <> 0 then
        begin
        Lo := SysUtilsEx.toInt(st, 1, j-1, false, -1);
        Hi := SysUtilsEx.toInt(st, j+1, Length(st), false, -1);
        end
      else
        begin
        Lo := SysUtilsEx.toInt(st);
        Hi := Lo
        end; { if }
      Result.AddIndex(Lo,Hi);
      end;
  until st = '';
end; {GetIndexsFromString}

{----------------------------- Classes --------------------------}

Destructor TListIndex.Destroy;
{ Objetivo
    Libera espaço ocupado pela lista
  Métodos chamados
    Destroy herdado
}
var I: Integer;
Begin
  For i:= 0 to (self.count-1) do
    Dispose(pTIndexRec(Self.Items[I]));
  Inherited Destroy; {Chama o destructor da propria lista}
End;

{----------------- AddFunction ------------------}

procedure TListIndex.AddIndex (Ini, Fim: Longint);
{ Objetivo
    Cria um registro e atribui valores às posições
  Parâmetros
    Ini: valor a ser atribuído à posição inicial
    Fim: valor a ser atribuído à posição final
}
var pIndexRec: pTIndexRec;
begin
  New(pIndexRec);
  pIndexRec^.Ini   := Ini;
  pIndexRec^.Fim   := Fim;
  Add(pIndexRec);
  If (pIndexRec^.Ini > pIndexRec^.Fim) Then
     Swap(pIndexRec^.Ini, pIndexRec^.Fim);
end;

Function TListIndex.GetIndex (index: integer): TIndexRec;
{ Objetivo
    Recupera um elemnto da lista
  index: índice do elemento. Baseado em 0.
}
begin
  Try
    Result := pTIndexRec(Self.Items[Index])^;
  Except
    Raise Exception.CreateFmt('Índice [%d] da lista de índices inválido', [Index]);
  End;
end;

{- Reconhecer Var. Desconhecida
 - Reconhecer token [i-1] como operador lógico
 - Reconhecer token [i-2] como variável do tipo fator
 - Se reconhecer tudo então:
   - Procura valor do fator na lista e se achar troca-o pelo seu índice senão erro}
Function PreProcessor(DS: TwsDataSet; const S: String): String;
var P: TExpressionParser;
    i: Integer;        // i-égimo Token
    ta: TToken;        // Token atual = [i]
    op: TToken;        // Token Operador = [i-1]
    tn: TToken;        // Token Anterior = [i-2]
    v : TwsDataSetCol; // Uma das Colunas de DS
    f : TwsFactor;     // É a coluna fator
    li: Integer;       // Índice do nível
begin
  P := TExpressionParser.Create();
  P.AritOpers.Text := '+'#13'-'#13'/'#13'*'#13'#'#13'@'#13'**'#13'##'#13'#/'#13'@|'#13 +
                      '||'#13'//'#13'<>'#13'><'#13'&'#13'|'#13'^'#13'<'#13'>'#13'='#13 +
                       '<='#13'>='#13'^=';
  try
    P.Text.Add(S);
    P.Scan;
    for i := 0 to P.TokensCount-1 do
      begin
      ta := P.Token[i];
      if (i > 1) and (ta.TokenType = ttVariable) and (DS.Struct.IndexOf(ta.AsString) = -1) then
         begin
         op := P.Token[i-1];
         tn := P.Token[i-2];
         if (DS.Struct.IndexOf(tn.AsString) > -1) then
            begin
            v := DS.Struct.ColByName(tn.AsString);
            if wsAvaliadorDeExpressoes.IsLogicOperator(op.AsString) then
               if v.ColType <> dtNumeric then
                  begin
                  f  := TwsFactor(v);
                  li := f.LevelNames.IndexOf(ta.AsString);
                  if li > -1 then
                     P.ReplaceToken(i, intToStr(li), ttInteger)
                  else
                     Raise Exception.CreateFmt('Valor < %s > do fator não reconhecido', [ta.AsString])
                  end
               else
                  Raise Exception.CreateFmt('%s não é do tipo fator', [v.Name])
            else
               Raise Exception.CreateFmt('Operador %s não é um operador lógico', [op.AsString])
            end;
         end;
      end; // for
    Result := P.MakeString;
  finally
    P.Free;
  end;
end;

Function ApproxRandomSample(RData: TwsDataSet; Const Name, Rotulo: String; Col: TwsLIVec; p: Double): TwsDataSet;
{ Objetivo
    Obtem uma amostra com aproximadamente 100*p% das observações presentes num conjunto de dados
  Parâmetros
    RData : Conjunto de dados de onde serão retiradas as observações
    Name  : Nome do conjunto de dados que será criado
    Rotulo: Rótulo para o novo conjunto
    Col   : Colunas do conjunto existente que participarão das observações selecionadas
    p     : Proporção aproximada de observações a serem amostradas
}
var
  Rand  : TwsRandom;
  i,j: Integer;
  IdName: Boolean;
  v     : TwsDFVec;
begin
  Rand := TwsShuffle.Create;
  Result := TwsDataSet.Create(Name);
  Result.MLab:= Rotulo;
  IdName := RData.ColIdentName <> '';
  if IdName then
    Result.ColIdentName := RData.ColIdentName;
  for i := 1 to Col.Len do
    Result.Struct.AddColEx(CopyDescCol(RData.Struct.Col[Col[i]]));
  for i := 1 to RData.Nrows do
    if Rand.Generate < p then
      begin
      v := TwsDFVec.Create(Col.Len);
      if IdName then
        v.Name := RData.RowName[i];
      for j := 1 to Col.Len do
        v[j] := RData[i,Col[j]];
      Result.MAdd(v)
      end;
   Rand.Free
end;

Function RandomSample(RData: TwsDataSet; Const Name, Rotulo: String; Col: TwsLIVec; k: Integer): TwsDataSet;
{ Objetivo:
    Obtém uma amostra com um número exato de observações presentes num conjunto de dados
  Parâmetros
    RData: Conjunto de dados de onde serão retiradas as observações
    Name: nome do novo conjunto
    Rotulo: Rótulo do novo conjunto
    Col:   Colunas que participarão das observações selecionadas
    k:     Número de observações a serem amostradas
}
var
  Rand  : TwsRandom;
  i,j,n : Integer;
  NewCol: TwsDataSetCol;
  v     : TwsDFVec;
  IdName: Boolean;
begin
  Rand := TwsShuffle.Create;
  Result := TwsDataSet.Create(Name);
  Result.MLab:= Rotulo;
  IdName := RData.ColIdentName <> '';
  if IdName then
    Result.ColIdentName := RData.ColIdentName;
  for i := 1 to Col.Len do
    Result.Struct.AddColEx(CopyDescCol(RData.Struct.Col[Col[i]]));
  n := RData.Nrows+1;
  i:=1;
  while (i<=RData.Nrows) and (k>0) do begin
    Dec(n);
    if Rand.Generate < k/n then begin
      v := TwsDFVec.Create(Col.Len);
      if IdName then
        v.Name := RData.RowName[i];
      for j := 1 to Col.Len do
        v[j] := RData[i,Col[j]];
      Result.Madd(v);
      Dec(k);
    end; { if }
    Inc(i);
  end; { while }
  Rand.Free
end;

procedure EvalExpression(const Expr: String; DataSet: TwsDataSet);
var i: Integer;
    Eval: TAvaliator;
    s: String;
begin
  if DataSet = nil then exit;
  Eval := TAvaliator.Create;
  try
    {Pega os nomes das colunas}
    For i := 1 to DataSet.nCols do
      Eval.TabVar.AddFloat(DataSet.Struct.Col[i].Name, 0);

    s := PreProcessor(DataSet, Expr);
    Eval.Expression := s;
  finally
    Eval.Free;
  end;
end;

function MultivarStat(Teta:TwsVec;npred,nresp,r,rc,rw:Integer; dfres: Double): TwsDataSet;
{Objetivo
   Obtém as estatisticas para os quatro testes multivariados: Pillai, Roy, Wilks e
     Hotteling-Lawley. Retorna um conjunto de dados com as estatisticas, valor da
     estatistica F, probabilidades, graus de liberdade para numerador e denominador do
     teste F e tipo de aproximacao obtida.
 Parâmetros
   Teta: Vetor com os autovalores
   npred: número de variáveis preditoras
   nresp: número de variáveis respostas
   r   : Número de autovalores não nulos
   rc  : Mínimo entre o número de combinações independentes entre os parâmetros e o número
         de graus de liberdade da hipótese
   rw  : Mínimo entre o número de combinações entre as variáveis respostas e o número de
         variáveis respostas
   dfres: graus de liberdade do resíduo
 Campos modificados
   Nenhum
}
var
  i   : Integer;
  aux1: Double;
  Col :TwsDataSetCol;
  v   : TwsDFVec;

  procedure FApprox;
  var
    r1,u,t,wl,m,n: Double;
    Err          : Word;
  begin
    // Wilks
    rc:=Math.Min(NPred,rc);
    rw:=Math.Min(NResp,rw);
    r1:=DFRes-(rw-rc+1)/2;
    u := Sqr(rw)+Sqr(rc)-5;
    if u > 0 then
      t:=Sqrt((Sqr(rw)*Sqr(rc)-4)/u)
    else
      t:=1;
    u:=(rw*rc-2)/2;
    wl := Power(Result[1,1],1/t);

    Result[1,3]:=rw*rc;                                 // GL Numerador
    Result[1,4]:=r1*t-u;                                // GL Denominador
    Result[1,2]:=((1-wl)/wl)*(Result[1,4]/Result[1,3]); // Estatistica F
                                                        // Probablidade
    Result[1,5]:=FInt(Result[1,2],Result[1,3],Result[1,4],True,Err);
    if Math.Min(rw,rc)<=2 then
      Result[1,6] := 0                                  // Exato
    else
      Result[1,6]:=1;                                   // Aproximado

    m:=0.5*(Abs(rw-rc)-1); n:=0.5*(DFRes-rw-1);
    u:=(2*n+r+1); t:=(2*m+r+1);

    // Pillai
    wl:=Result[2,1];
    Result[2,2]:=(u/t)*(wl/(r-wl));                     // Estatistica F
    Result[2,3]:=r*t;                                   // GL Numerador
    Result[2,4]:=r*u;                                   // GL Denominador
                                                        // Probablidade
    Result[2,5]:=FInt(Result[2,2],Result[2,3],Result[2,4],True,Err);
    Result[2,6]:=1;                                     // Aproximado

    // Hotelling-Lawley
    Result[3,2]:=(2*(r*n+1)*Result[3,1]/(Sqr(r)*t));    // Estatistica F
    Result[3,3]:=r*t;                                   // GL Numerador
    Result[3,4]:=2*(r*n+1);                             // GL Denominador
                                                        // Probabilidade
    Result[3,5]:=FInt(Result[3,2],Result[3,3],Result[3,4],True,Err);
    Result[3,6]:=1;                                     // Aproximada

    // Roy
    t := Math.Max(rw,rc);
    Result[4,2]:=Result[4,1]*(DFRes-t+rc)/t;            // LS para estatistica F
    Result[4,3]:=t;                                     // GL Numerador
    Result[4,4]:=DFRes-t+rc;                            // GL Denominador
                                                        // LI para probabilidade
    Result[4,5]:=FInt(Result[4,2],Result[4,3],Result[4,4],True,Err);
    Result[4,6]:=2;                                     // Limite superior
  end; // FApprox

begin // MultivarStat
  Result :=TwsDataSet.Create('MultEstat');
  Result.ColIdentName := 'Estatísticas';
  with Result do
    begin
    MLab:='Testes Multivariados';
    Struct.AddColEx(TwsNumeric.Create('Valor','Valor da Estatística',12,7));  //1
    Struct.AddColEx(TwsNumeric.Create('F','Estatística F',12,7));  //2
    Struct.AddColEx(TwsNumeric.Create('GL_Num','Graus de liberdade do numerador',6,5));//3
    Struct.AddColEx(TwsNumeric.Create('GL_Den','Graus de liberdade do denominador',6,5));//4
    Struct.AddColEx(TwsNumeric.Create('Valor_p','Nível mínimo de significância',11,4)); //5
    Col:=TwsQualitative.Create('Tipo','Tipo de aproximação para estatística',8);       //6
    With TwsQualitative(Col) Do
      Begin
      AddLevel('Exata');
      AddLevel('Aprox');
      AddLevel('L_Sup');
      End; { With Col }
    Struct.AddColEx(Col);                                                              //7
    end; // with Result

  v:=TwsDFVec.Create(6);
  v.Name:='Wilks';
  aux1:=1;
  for i:=1 to r do
    aux1:=aux1*(1/(1+Teta[i]));
  v[1]:=aux1; // Wilks
  Result.MAdd(v);

  v:=TwsDFVec.Create(6);
  v.Name:='Pillai';
  aux1:=0;
  for i:=1 to r do
    aux1:=aux1+(Teta[i]/(1+Teta[i]));
  v[1]:=aux1; // Pillai
  Result.MAdd(v);

  v:=TwsDFVec.Create(6);
  v.Name:='Hot_Lawley';
  aux1:=0;
  for i:=1 to r do
    aux1:=aux1+Teta[i];
  v[1]:=aux1; // Hotelling-Lawley
  Result.MAdd(v);

  v:=TwsDFVec.Create(6);
  v.Name:='Roy';
  v[1]:=Teta[1]; // Roy
  Result.MAdd(v);

  // Obtencao das aproximacoes
  FApprox
end;

(*
Retorna o indice da variavel com nome s_var no DataSet.
Caso nao exista uma variavel com o nome s_var entao
retorna o indice da primeira variavel que contenha o Label s_var.
Caso nao exista nenhuma variavel como o Label s_var, entao retorna -1.
*)
Function GetIndexVar(DataSet: TWSDataSet; const s_var: string): Integer;
Var i : longint;
Begin
  Result := DataSet.Struct.IndexOf(s_var);
  If Result = -1 Then
     {varrer todas colunas procurando o nome s_var no label de cada coluna.}
     For i := 1 To DataSet.NCols Do
       If s_var = DataSet.Struct.Col[i].Lab Then
          Begin
          Result := i;
          Break;
          End;
End; {GetIndexVar}

  // Imprime em grupos
  procedure PrintInGroup(DS: TwsDataSet; Cols, Rows, Vars: TwsLIVec; x: TXML_Writer);

  // Encontra coluna de A que contem combinacao de niveis c
  function ColIdx(A: TwsGeneral; c: TwsLIVec): Integer;
  var
    i: Integer;
  begin
  Result:=1;
  for i:=1 to c.Len do
    while Int(A[i,Result])<>c[i] do
      Inc(Result);
  end;

  // Verifica se linha i de D tem os mesmos indices rl nas variaveis de indices r
  function SameKey(D: TwsDataSet; i: Integer; r,rl: TwsLIVec): Boolean;
  var
    k: Integer;
  begin
  Result:=True;
  k:=1;
  while (k<=r.Len) and Result do
    begin
    Result:=D[i,r[k]]=rl[k];
    Inc(k)
    end
  end;

    // Combina a Matriz M com um número de elementos
    function Combine(M: TwsGeneral; ItemsCount: integer): TwsGeneral;
    var k, i, j, e: Integer;
    begin
      // Cria uma matriz "Template" onde as linhas são os níveis do cabçalho
      // e as colunas são as combinações
      Result := TwsGeneral.Create(M.nCols + 1, M.nRows * ItemsCount);

      // Translada Grupo e preenche Cab
      k := 1;
      for i := 1 to M.nRows do
        for e := 0 to ItemsCount-1 do
          begin
          for j := 1 to M.nCols do
            Result[j, k] := M[i, j];
          Result[Result.nRows, k] := e;
          inc(k);
          end;
    end;

    // Gera o cabecalho das colunas
    procedure PrintColHeaders(x: TXML_Writer; M: TwsMatrix);

      // Retorna o valor do nível dado o Índice do Grupo e o Índice do Nível
      // O índice do grupo não é o mesmo da coluna do DataSet
      function getLevelName(Group, Level: Integer): String;
      begin
        Result := TwsFactor(DS.Struct.Col[Cols[Group]]).LevelNames[Level];
      end;

      procedure PrintGroup(row: Integer; cellPadding: Boolean);
      var i, ii, k: Integer;
          v: TwsVec;
      begin
        v := M.Row[row];

        if cellPadding then
           begin
           k := 0;
           ii := 0;
           for i := 1 to v.Len do
             begin
             if v[i] <> k then
                begin
                x.Write('cell', ['colspan', 'bgcolor'], [ii, 'silver'], getLevelName(row, k)); 
                k := v.AsInteger[i];
                ii := 0;
                end;
             inc(ii);
             end;
             x.Write('cell', ['colspan', 'bgcolor'], [ii, 'silver'], getLevelName(row, k)); 
           end
        else
           for i := 1 to v.Len do
             x.Write('cell', ['bgcolor'], ['silver'], DS.ColName[Vars[v.AsInteger[i] + 1]]); 
      end;

    var i, j: Integer;
    begin // PrintColHeaders
      // Grupos
      for i := 1 to M.nRows do
        begin
        x.BeginIdent();
        x.BeginTag('row align="center"');
          x.BeginIdent();
          for j := 1 to Rows.Len do x.WriteTag('blankCell');
          PrintGroup(i, i < M.nRows);
          x.EndIdent();
        x.EndTag('row');
        x.EndIdent();
        end;
    end;

    // Gera o código XML para uma linha de dados
    procedure PrintLine(x: TXML_Writer; IndexRow: Integer; Values: TwsDFVec;
      ChangeColor: Boolean; P: TwsLIVec);
    var i, Level: Integer;
        f: TwsFactor;
        c: String;
    begin
      if ChangeColor then c := ' bgColor="#eae7e6"' else c := '';

      x.BeginIdent();
      x.BeginTag('row align="center"' + c);
        x.BeginIdent();

        // Cabecalho das linhas
        for i := 1 to Rows.Len do
          begin
          f := TwsFactor(DS.Struct.Col[Rows[i]]);
          Level := DS.AsInteger[IndexRow, Rows[i]];
          x.Write('<cell align="left" bgcolor="silver">' + f.LevelNames[Level] + '</cell>');
          end;

        // Dados
        for i := 1 to Values.Len do
          x.Write('cell', Values.AsStringF(i,P[i]));

        x.EndIdent();
      x.EndTag('row');
      x.EndIdent();
    end;

    procedure PrintData(x: TXML_Writer; ColHeaders: TwsGeneral);
    var clIdx, rlIdx,
        Prec          : TwsLIVec;
        Values        : TwsDFVec;
        i, k, ix      : Integer;
        ChangeColor   : Boolean;
    begin
      // Vetores auxiliares
      clIdx := TwsLIVec.Create(Cols.Len);
      rlIdx := TwsLIVec.Create(Rows.Len);

      // Indices das variaveis de linha para a primeira linha
      for k := 1 to Rows.Len do
        rlIdx[k] := DS.AsInteger[1, Rows[k]];

      // vetor que contera os valores de cada linha da saida final
      Values := TwsDFVec(VecConst(wscMissValue, ColHeaders.NCols));
      Prec:=TwsLIVec.Create(Values.Len);

      for i := 1 to DS.NRows do
        begin
        // atualiza indices das variaveis de coluna na linha i (chaves)
        for k := 1 to Cols.Len do
          clIdx[k] := DS.AsInteger[i, Cols[k]];

        // Coluna de ColHeaders que possui os indices de colunas
        ix := ColIdx(ColHeaders, clIdx);

        // Chave continua a mesma ?
        if SameKey(DS, i, Rows, rlIdx) then
           for k := 0 to Vars.Len-1 do
             begin
             Prec[ix+k]:=TwsNumeric(DS.Struct.Col[Vars[k+1]]).Precision;
             Values[ix+k] := DS[i, Vars[k+1]]
             end
        else
           begin
           ChangeColor := not ChangeColor;

           // imprime vetor resultante
           PrintLine(x, i-1, Values, ChangeColor, Prec);

           // atualiza indices de variaveis de linha
           for k := 1 to Rows.Len do
             rlIdx[k] := DS.AsInteger[i, Rows[k]];

           // Limpa o vetor
           Values.Fill(wscMissValue);

           // inicia preenchimento para essa linha
           for k := 0 to Vars.Len-1 do
             Values[ix+k] := DS[i, Vars[k+1]]
           end;
        end; // for i

      // imprime vetor resultante
      PrintLine(x, DS.nRows, Values, not ChangeColor, Prec);

      Values.Free;
      Prec.Free;
      clIdx.Free;
      rlIdx.Free;
    end;

  var A, ColHeaders : TwsGeneral;

  begin // PrintInGroup
    // Obtem uma matriz de indices resultante de uma combinacao das colunas
    A := DS.IndexMat(Cols);

    // Combina a Matriz de indices com um certo numero de elementos
    // resultando em uma outra matriz de indices especial que sera
    // usada como indexadora para a construcao da tabela agrupadora.
    ColHeaders := Combine(A, Vars.Len);

    // Ordena em ordem ascendente pelos fatores de linha
    DS.SortRows(Rows, Rows);

    // Gerador de XML
    x.WriteTag('br');
    x.BeginTag('table', ['border', 'align'], [1, 'center']);
      PrintColHeaders(x, ColHeaders); // Cabeçalho das colunas e dados
      PrintData(x, ColHeaders);
    x.EndTag('table');

    // Destroi os objetos temporarios
    A.Free;
    ColHeaders.Free;
  end;

end.
