unit wsCorrelacoes;

(*
  IMPLEMENTAÇÃO:
    Alex Dias Gonsales - 12/02/1999
    - Somente passei as chamadas feitas em Fixar.Pas(Analise de Correlacoes)
      para esta Unit, implementando a classe TwsCorr.

  MODIFICACOES:

  AUTOR E DATA: ........................
  ESCOPO: ..............................
  OBS.: ................................

*)

interface
Uses Classes,
     wsConstTypes,
     wsVec,
     wsMatrix,
     wsBXML_Output;

Const
  {Constantes utilizadas na Janela Analise de Correlacoes, servem como indice para
   a variavel bitsOpcoes(TBits)}
  cIDSFrame = 1;
  cADSFrame = 2;
  cIMCorr   = 3;
  cAMCorr   = 4;
  cIMStat   = 5;
  cAMStat   = 6;

Type

  TwsCorr = Class
  Private
    bitsOpcoes :TBits; {Não liberar. É apenas um ponteiro para o argumento passado.}
    DataSet    {DataSet de onde serão escolhidos os dados.}
             : TwsDataSet;
    lstDataSets {Lista que recebera os MFrames adequados obtidos atraves
                de 'Variaveis a analisar', 'Grupos', 'Inclui se' e 'Peso'.
                Criado no Create e destruído no Destroy.}
             : TwsDataSets;
    DSFrame  {Somente uma referência para um dos DataSets gerados por ModelFrame.
              Esta variável não é diretamente liberada pois ela é apenas um
              ponteiro temporário para um dos objetos (DataSet) de lstDataSets.}
             : TwsDataSet;
    mCorr     {Matriz das correlações criada no create para cada DSFrame,
              Impressa e ou armazenada ou liberada em GeraSaidas.}
             :TwsSymmetric;
    mStat    {Matriz de estatísticas. Criada no create para cada DSFrame,
              Impressa e ou armazenada ou liberada em GeraSaidas.}
             :TwsGeneral;
    CT: TwsGeneral; // Retorna os testes realizados sobre as correlações

    // Mecanismo para geração de relatórios
    FOutput: TwsBXML_Output;

    // Evento notificador de criação de objetos
    FOnCreatedObject: TwsCreatedObject_Event;

    Procedure GeraSaidas;
  Public
    Constructor Create(aDataSet             : TwsDataSet;
                       astsVarsGrupo        : TStrings;
                       asCondicao           : String;
                       asPeso               : String;
                       sts_analisar         : TStrings;
                       sts_fixar            : TStrings;
                       var Erro             : integer;
                       abitsOpcoes          : TBits;
                       aOutput              : TwsBXML_Output;
                       aCreatedObject_Event : TwsCreatedObject_Event);

    Destructor Destroy;
  End;{TwsCorr}

implementation
Uses SysUtils,
     Dialogs,
     SysUtilsEx,
     wsFuncoesDeDataSets,
     wsGLib;

(*****************************************************************************
Saídas:
Erro
  0 - Nenhum erro.
  1 - Erro -> Deve haver no mínimo 2 variáveis a fixar.
  2 - Erro -> Erro ao chamar ModelFrame.
*)
Constructor TwsCorr.Create(aDataSet             : TwsDataSet;
                           astsVarsGrupo        : TStrings;
                           asCondicao           : String;
                           asPeso               : String;
                           sts_analisar         : TStrings;
                           sts_fixar            : TStrings;
                           var Erro             : integer;
                           abitsOpcoes          : TBits;
                           aOutput              : TwsBXML_Output;
                           aCreatedObject_Event : TwsCreatedObject_Event);

Var sts_aux    : TStrings; //Receberá os itens da ListBox Analisar e da ListBox Fixar
    v_analisar,            //Vetor das variáveis a correlacionar na lbAnalisar
    v_fixar    : TWSLIVec; //Vetor das variáveis a fixar presentes na lbFixar
    i,tipo,iw  : Integer;  //Variavel contadora
    st         : string;
begin

  {Inicializações e referências.}
  DataSet          := aDataSet;
  bitsOpcoes       := abitsOpcoes;
  FOutput          := aOutput;
  FOnCreatedObject := aCreatedObject_Event;
  Erro             := 0;
  v_analisar       := nil;
  v_fixar          := nil;
  CT               := nil;

  if sts_analisar.Count < 2 then
     begin
     Erro := 1;
     Exit;
     end;

  sts_aux := tStringList.Create;
  sts_aux.AddStrings(sts_analisar);
  sts_aux.AddStrings(sts_fixar);

  if asPeso <> '' then
     sts_aux.Add(asPeso);

  {Gera o MFrame adequado que será passado para ser avaliado}
  try
    lstDataSets := ModelFrame(sts_aux, astsVarsGrupo, asCondicao, DataSet);
  except
    sts_aux.Free;
    Erro := 2;
    exit;
  end;

  v_analisar := TwsLIVec.Create(sts_analisar.Count);
  for i := 0 to sts_analisar.Count - 1 do
      v_analisar[i+1] := TwsDataSet(lstDataSets[0]).Struct.IndexOf(NomeFormula(sts_analisar[i]));

  if asPeso = '' then               // sem variavel peso definida
     if (sts_fixar.Count = 0) then  // sem variavel a fixar
        tipo := 1
     else
        Begin
        tipo := 2;
        v_fixar:= TwsLIVec.Create(sts_fixar.Count);
        for i:= 0 to sts_fixar.Count - 1 do   //adiciona o indice das variaveis a fixar no vetor
            v_fixar[i+1]:=TwsDataSet(lstDataSets[0]).Struct.IndexOf(NomeFormula(sts_fixar[i]));
        End
  else                               // com variavel peso
     begin
     iw := lstDataSets[0].Struct.IndexOf(NomeFormula(asPeso));
     if (sts_fixar.Count = 0) then   // sem variavel a fixar
        tipo := 3
     else
        Begin
        tipo := 4;
        v_fixar:= TwsLIVec.Create(sts_fixar.Count);
        for i:= 0 to sts_fixar.Count - 1 do   //adiciona o indice das variaveis a fixar no vetor
            v_fixar[i+1] := lstDataSets[0].Struct.IndexOf(NomeFormula(sts_fixar[i]));
        End
     end;

  FOutPut.BeginText;
    st:=DataSet.Name;
    if DataSet.MLab <> '' then
      st:=st+' - '+DataSet.MLab;
    FOutput.WritePropValue('Conjunto de Dados:', st);

    if asCondicao <> '' then
      FOutput.WritePropValue('Filtro:', asCondicao);

    st:='';
    if astsVarsGrupo.Count>0 then
      begin
      for i:=0 to astsVarsGrupo.Count-1 do
        st:=st+astsVarsGrupo[i]+', ';
      System.Delete(st,Length(st)-1,2);
      FOutput.WritePropValue('Variáveis de Grupo:', st);
      end;

    st := '';
    for i := 0 to sts_analisar.Count-1 do
      st := st + sts_analisar[i] + ', ';
    System.Delete(st,Length(st)-1,2);
    FOutput.WritePropValue('Correlações entre:', st);

  if (sts_fixar.Count > 0) then
    begin
    st := 'Partial em relação a: ';
    for i := 0 to sts_fixar.Count-1 do
      st := st + sts_fixar[i] + ', ';
    System.Delete(st,Length(st)-1,2);
    FOutput.WritePropValue('Parcial em relação a:', st)
    end;

  if asPeso <> '' then
    FOutput.WritePropValue('Ponderada por:', asPeso);

  FOutPut.EndText;

  {Laço para cada DataSet gerado por ModelFrame.}
  For i := 0 to lstDataSets.Count - 1 Do
      Begin
      DSFrame := lstDataSets[i];
      Case tipo Of
        1:             // sem peso e sem variavel a fixar
        begin
        mCorr := DSFrame.Correlations(v_analisar, mStat, bitsOpcoes[cIMStat] or bitsOpcoes[cAMStat], CT);
        mCorr.Name := 'Corr_' + System.Copy(DSFrame.Name,1,5);
        if DSFrame.MLab <> '' then
          mCorr.MLab := 'Matriz de correlações de Pearson'+' - '+DSFrame.MLab
        else
          mCorr.MLab := 'Matriz de correlações de Pearson';
        end;
        2:             // sem peso e com variavel a fixar
        begin
        mCorr := DSFrame.PartialCorrel(v_analisar, v_fixar, mStat, bitsOpcoes[cIMStat] or bitsOpcoes[cAMStat], CT);
        mCorr.Name := 'Corr_' + System.Copy(DSFrame.Name,1,5);
        if DSFrame.MLab <> '' then
          mCorr.MLab := 'Matriz de correlações parciais'+' - '+DSFrame.MLab
        else
          mCorr.MLab := 'Matriz de correlações parciais';
        end;
        3:            // com peso e sem variavel a fixar
        begin
        mCorr:=DSFrame.WCorrelations(v_analisar,mStat,bitsOpcoes[cIMStat] or bitsOpcoes[cAMStat],
          iw,CT);
        if MCorr <> nil then
          begin
          mCorr.Name := 'Corr_'+System.Copy(DSFrame.Name,1,5);
          if DSFrame.MLab <> '' then
            mCorr.MLab := 'Matriz de correlações de Pearson/ Peso: ' + DSFrame.ColName[iw] + DSFrame.MLab
          else
            mCorr.MLab := 'Matriz de correlações de Pearson/ Peso: ' + DSFrame.ColName[iw];
          end
        end;
        4:
        begin
        mCorr:=DSFrame.WPartialCorrel(v_analisar,v_fixar,mStat,bitsOpcoes[cIMStat] or bitsOpcoes[cAMStat],
          iw, CT);
        if mCorr <> nil then
          begin
          mCorr.Name := 'Corr_'+System.Copy(DSFrame.Name,1,5);
          if DSFrame.MLab <> '' then
            mCorr.MLab := 'Matriz de correlações parciais/ Peso: '+DSFrame.ColName[iw] + DSFrame.MLab
          else
            mCorr.MLab := 'Matriz de correlações parciais/ Peso: '+DSFrame.ColName[iw];
          end
        end
      End;

      if bitsOpcoes[cIMStat] Or bitsOpcoes[cAMStat] then
        if DSFrame.MLab <> '' then
          mStat.MLab := mStat.MLab + DSFrame.MLab;

      GeraSaidas;
      End;

  {liberar ou nao ????????????????????}
  sts_aux.Free;
  v_fixar.Free;
  v_analisar.Free;
End; {TwsCorr.Create}

(****************************************************************************
*)
Destructor TwsCorr.Destroy;
Begin
  {Se não foi escolhida a opção para armazenar os conjuntos de dados...}
  if Not bitsOpcoes[cADSFrame] then
     if lstDataSets <> Nil Then
        lstDataSets.free;
  inherited;
End;

(****************************************************************************
ok
Entrada:
Função:
   Imprime, Armazena ou Libera: DSFrame, etc...
*)
Procedure TwsCorr.GeraSaidas;
Begin

  {--------------------- Conjunto de Dados (DSFrame) ---------------------}
  if BitsOpcoes[cIDSFrame] then
     FOutput.Add(DSFrame);

  {Obs.: lstDataSets é liberado em Destroy.}
  if BitsOpcoes[cADSFrame] and Assigned(FOnCreatedObject) then
     FOnCreatedObject(self, DSFrame);

  {Obs.: lstDataSets é liberado em Destroy.}

  {--------------------- Matriz das Correlações ---------------------------}
  if BitsOpcoes[cIMCorr] then
    begin
    if mCorr <> nil then
      FOutput.Add(mCorr)
    else
      ShowMessage('Matriz de Correlações não pode ser obtida');
    if CT <> nil then
      FOutput.Add(CT)
    else
      ShowMessage('Testes para Correlações não pode ser obtido');
    end;

  if BitsOpcoes[cAMCorr] and Assigned(FOnCreatedObject) then
    begin
    if mCorr <> nil then
      FOnCreatedObject(self, mCorr);
    if CT <> nil then
      FOnCreatedObject(self, CT)
//    else
//      FOutput.Warning('Número de observações insuficientes');
    end
  else
    begin
    FreeAndNil(mCorr);
    FreeAndNil(CT);
    end;

  {--------------------- Matriz das Estatísticas ---------------------------}
  if BitsOpcoes[cIMStat] then
    if mStat <> nil then
      FOutput.Add(mStat);

  if BitsOpcoes[cAMStat] and Assigned(FOnCreatedObject) then
    if mStat <> nil then
      FOnCreatedObject(self, mStat)
  else
     FreeAndNil(mStat);

  FOutPut.WriteLink('Veja mais detalhes sobre Análise de Correlação',
    SysUtilsEx.GetApplicationDir + 'Ajuda\Correlacao\Ideias Gerais.htm');
End;

end.
