unit DataSet_GraficoDoModeloDeRegressao;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  wsMasterModel;

type
  TfoDataSet_GraficoDoModeloDeRegressao = class(TForm)
    Label18: TLabel;
    LX: TLabel;
    LY: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    lbInfluencia: TListBox;
    lbComp: TListBox;
    lbResParcial: TListBox;
    Bevel1: TBevel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    Help: TBitBtn;
    sbIncluiX: TSpeedButton;
    sbIncluiY: TSpeedButton;
    XInfluencia: TEdit;
    XComp: TEdit;
    YInfluencia: TEdit;
    XResParcial: TEdit;
    Indice: TEdit;
    YComp: TEdit;
    YResParcial: TEdit;
    rbInfluencia: TRadioButton;
    rbCompRes: TRadioButton;
    rbResParc: TRadioButton;
    rbIndices: TRadioButton;
    Panel1: TPanel;
    procedure YInfluenciaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VariaveisDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure VariaveisDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Graficosclick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbIncluiXClick(Sender: TObject);
    procedure lbInfluenciaClick(Sender: TObject);
    procedure sbIncluiYClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure YResParcialClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
  private
    FFocus: Integer;
  public
    Model: TwsLinearModel;
    procedure Clear;
    Procedure PreencheListas;
  end;

implementation
uses WinUtils,
     SysUtilsEx,
     wsRotinasUteis,
     wsRegressoes,
     wsGraficos,
     wsGLib,
     wsMatrix,
     Form_Chart,
     Chart,
     wsConstTypes;
     //ModuloComum;

{$R *.DFM}

Procedure TfoDataSet_GraficoDoModeloDeRegressao.PreencheListas;
Begin
  With TwsUReg(Model) do
    Begin
    FillListWithColsDs(lbInfluencia.Items, OutInf); //está dando erro aqui, outinf = nil
    FillListWithColsDs(lbComp.Items, CpResid);
    FillListWithColsDs(lbResParcial.Items, ParResid);
    End;
End;

procedure TfoDataSet_GraficoDoModeloDeRegressao.YInfluenciaKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  If Key = VK_DELETE Then
     TEdit(Sender).Text := '';
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.VariaveisDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TListBox) and (TListBox(Source).ItemIndex <> -1) and
            (TListBox(Source).Tag = TEdit(Sender).Tag);
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.VariaveisDragDrop(Sender, Source: TObject; X, Y: Integer);
var LB: TListBox;
    ED: TEdit;
begin
  LB := TListBox(Source);
  ED := TEdit(Sender);

  If ED.Name = 'YInfluencia' Then
     If SysUtilsEx.AllTrim(ED.Text) = '' Then
        ED.Text := LB.Items[LB.ItemIndex]
     Else
        ED.Text := ED.Text + ', ' + LB.Items[LB.ItemIndex]
  Else
     ED.Text := LB.Items[LB.ItemIndex];
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.GraficosClick(Sender: TObject);
Var C: TCheckBox;
begin
  C := TCheckBox(Sender);
  Case C.Tag of
    0: SetEnable([XInfluencia, YInfluencia], C.Checked);
    1: SetEnable([XComp, YComp], C.Checked);
    2: SetEnable([XResParcial, YResParcial], C.Checked);
    3: SetEnable([Indice], C.Checked);
    End;
end; {GBVariaveis_Click}

procedure TfoDataSet_GraficoDoModeloDeRegressao.btnOkClick(Sender: TObject);
var Mat: TwsGeneral;
    k  : Integer;
    s  : String;
    Graf: TfoChart;
    FezGraf: Boolean;
begin
  Mat := TwsUReg(Model).XMatExtr;
  FezGraf:=False;
  if rbInfluencia.Checked then
    If (XInfluencia.Text <> '') and (YInfluencia.Text <> '') Then
      begin
      FezGraf:=True;
      Graf := ws_ScatterPlot(TwsUReg(Model).OutInf, XInfluencia.Text, StrToStrList(YInfluencia.Text));
      Graf.Caption := Format('Modelo de Regressão (%s)', [TwsUReg(Model).OutInf.Name]);
      Model.Manager.ObjectCreated(Graf);
      Model.Manager.Output.Add(Graf)
      end;

  if rbCompRes.Checked then
    If (XComp.Text <> '') and (YComp.Text <> '') Then
      begin
      FezGraf:=True;
      s := XComp.Text;
      k := System.Pos('#', s);
      If k > 0 Then s := System.Copy(s, 1, k-1);
      k := Mat.CName.IndexOf(s) + 1;
      Graf := ws_ScatterPlotTendency(TwsUReg(Model).CpResid,
                                     XComp.Text,
                                     StrToStrList(YComp.Text),
                                     Mat[1,k], Mat[5,k],
                                     Mat[2,k], Mat[6,k],
                                     Nil,
                                     Format('Gráfico do Componente + Resíduo (%s)',
                                            [TwsUReg(Model).CpResid.Name]));
      Model.Manager.ObjectCreated(Graf);
      Model.Manager.Output.Add(Graf)
      end;

  if rbResParc.Checked then
    If (XResParcial.Text <> '') and (YResParcial.Text <> '') Then
      begin
      FezGraf:=True;
      s := XResParcial.Text;
      k := System.Pos('#', s);
      s := System.Copy(s, k+1, Length(s)-k);
      k := Mat.CName.IndexOf(s) + 1;

      Graf:= ws_ScatterPlotTendency(TwsUReg(Model).ParResid,
                                    XResParcial.Text,
                                    StrToStrList(YResParcial.Text),
                                    Mat[3,k],
                                    Mat[7,k],
                                    Mat[4,k],
                                    Mat[8,k],
                                    Nil,
                                    Format('Gráfico de Regressão Parcial (%s)',
                                           [TwsUReg(Model).ParResid.Name]));
      Model.Manager.ObjectCreated(Graf);
      Model.Manager.Output.Add(Graf)
      end;

  if rbIndices.Checked then
    If Indice.Text <> '' Then
      begin
      FezGraf:=True;
      Graf := ws_ScatterPlotIndex(TwsUReg(Model).OutInf, Indice.Text);
      Graf.Caption := Format('Índice de Pontos (%s)', [TwsUReg(Model).OutInf.Name]);
      Model.Manager.ObjectCreated(Graf);
      Model.Manager.Output.Add(Graf)
      end;

  if FezGraf then
     Graf.Show(fsMDIChild);
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.Clear;
begin
  WinUtils.Clear([lbInfluencia, lbComp, lbResParcial,
                  XInfluencia, YInfluencia,
                  XComp, YComp,
                  XResParcial, YResParcial]);
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.FormShow(Sender: TObject);
begin
  If Model <> nil Then PreencheListas;
  FFocus:= 0;
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.sbIncluiXClick(Sender: TObject);
var i : Integer;
begin
  case FFocus of
    1 : Begin
        for i:= 0 to LBInfluencia.Items.Count-1 do
          if LBInfluencia.Selected[i] then
            Begin
            if rbInfluencia.Checked then
              if XInfluencia.Text = '' then
                XInfluencia.Text:= LBInfluencia.Items[i]
              else
                begin
                XInfluencia.Clear;
                XInfluencia.Text:= LBInfluencia.Items[i];
                end;
            if rbIndices.Checked then
              if Indice.Text = '' then
                Indice.Text:= LBInfluencia.Items[i]
              else
                begin
                Indice.Clear;
                Indice.Text:= LBInfluencia.Items[i];
                end;
            End;
        End; //case 1
    2 : Begin
        for i:= 0 to lbComp.Items.Count-1 do
          if lbComp.Selected[i] then
            if rbCompRes.Checked then
              if XComp.Text = '' then
                XComp.Text:= lbComp.Items[i]
              else
                begin
                XComp.Clear;
                XComp.Text:= lbComp.Items[i];
                end;
        End; //case 2
    3 : Begin
        for i:= 0 to lbResParcial.Items.Count-1 do
          if lbResParcial.Selected[i] then
            if rbResParc.Checked then
              if XResParcial.Text = '' then
                XResParcial.Text:= lbResParcial.Items[i]
              else
                begin
                XResParcial.Clear;
                XResParcial.Text:= lbResParcial.Items[i];
                end;
        End; //case 3
  End; //case ...Tag
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.lbInfluenciaClick(Sender: TObject);
begin
  sbIncluiX.Enabled:= (LBInfluencia.ItemIndex > -1) or (lbComp.ItemIndex > -1) or (lbResParcial.ItemIndex > -1);
  sbIncluiY.Enabled:= (LBInfluencia.ItemIndex > -1) or (lbComp.ItemIndex > -1) or (lbResParcial.ItemIndex > -1);
  FFocus:= TListBox(Sender).Tag;
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.sbIncluiYClick(Sender: TObject);
var i : Integer;
begin
  case FFocus of
    1 : Begin
        for i:= 0 to LBInfluencia.Items.Count-1 do
          if LBInfluencia.Selected[i] then
            if rbInfluencia.Checked then
              if YInfluencia.Text = '' then
                YInfluencia.Text:= LBInfluencia.Items[i]
              else
                YInfluencia.Text:= YInfluencia.Text + ', ' + LBInfluencia.Items[i];
        End; //case 1
    2 : Begin
        for i:= 0 to lbComp.Items.Count-1 do
          if lbComp.Selected[i] then
            if rbCompRes.Checked then
              if YComp.Text = '' then
                YComp.Text:= LBComp.Items[i]
              else
                YComp.Text:= YComp.Text + ', ' + LBComp.Items[i];
        End; //case 2
    3 : Begin
        for i:= 0 to lbResParcial.Items.Count-1 do
          if lbResParcial.Selected[i] then
            if rbResParc.Checked then
              if YResParcial.Text = '' then
                YResParcial.Text:= LBResParcial.Items[i]
              else
                YResParcial.Text:= YResParcial.Text + ', ' + LBResParcial.Items[i];
        End; //case 3
  End; //case ...Tag
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.Panel1Click(Sender: TObject);
begin
  if Screen.Cursor = crDefault then
    Begin
    Screen.Cursor:= crHandPoint;
    Panel1.BevelInner:= bvLowered;
    Panel1.BevelOuter:= bvLowered;
    End
  else
    if Screen.Cursor = crHandPoint then
      Begin
      Screen.Cursor:= crDefault;
      Panel1.BevelInner:= bvRaised;
      Panel1.BevelOuter:= bvRaised;
      End;
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.YResParcialClick(Sender: TObject);
begin
 if Screen.Cursor = crHandPoint then
    TEdit(Sender).Clear;
end;

procedure TfoDataSet_GraficoDoModeloDeRegressao.btnCancelClick(Sender: TObject);
begin
  Close;
end;
procedure TfoDataSet_GraficoDoModeloDeRegressao.HelpClick(Sender: TObject);
begin
  WinStatHelp('Modelos_Lineares\Graficos de Regressao.htm');
end;

end.

