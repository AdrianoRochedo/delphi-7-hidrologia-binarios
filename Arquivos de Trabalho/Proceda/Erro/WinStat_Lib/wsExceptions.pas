unit wsExceptions;

interface
uses SysUtils;

Type
  EvalException      = Class(Exception);
  EOperatorExpected  = Class(Exception);
  EExpected_Open     = Class(Exception);
  EExpected_Close    = Class(Exception);
  EIdentExpected     = Class(Exception);
  ETypeMismatch      = Class(Exception);
  EMyDivByZero       = Class(Exception);
  EImprDim           = Class(Exception);
  EExpectedTabSim    = Class(Exception);
  EIterMax           = Class(Exception);
  ESingMat           = Class(Exception);
  EHouse             = Class(Exception);
  ESquareMat         = Class(Exception);
  EPowerInv          = Class(Exception);
  EReadError         = Class(Exception);

  { ProbClass }
  EInvalidParam = class(Exception);

  { TabVar }
  ETabVarException = Class(Exception);
  EInvalidIndexVar = Class(ETabVarException);
  EInvalidNameVar  = Class(ETabVarException);
  EUnknownVariable = Class(ETabVarException);

  Procedure ShowException(Erro : Integer; Const S: String);

implementation
uses wsConstTypes;

Procedure ShowException(Erro: Integer; Const S: String);
Begin
  Case Erro Of
    ENOperatorExpected:
      Raise EOperatorExpected.Create(MNOperatorExpected);

    ENExpected_Close:
      Raise EExpected_Close.Create(MENExpected_Close);

    ENExpected_Open:
      Raise EExpected_Open.Create(MENExpected_Open);

    ENIdentExpected:
      Raise EIdentExpected.Create(MENIdentExpected);

    ENTypeMismatch:
      Raise ETypeMismatch.Create(MENTypeMismatch);

    ENUnknownVariable:
      Raise EUnknownVariable.CreateFmt(MsgUnknownVariable, [S]);

    ENMyDivByZero:
      Raise EMyDivByZero.Create(MMyDivByZero);

    NImprDim:
      Raise EImprDim.Create(MImprDim);

    NIterMax:
      Raise EIterMax.Create(MIterMax);

    NSingMat:
      Raise ESingMat.Create(MSingMat);

    NHouse:
      Raise EHouse.Create(MHouse);

    ENIdentExpectedAfterOpen:
      Raise Exception.Create(MIdentExpectedAfterOpen);

    Else
      Raise Exception.Create(S);  
    End;
End;

end.
