(**************************************************************************)
(* Unidade  dos percentis superiores da distribuicao das amplitudes       *)
(* estudentizadas. Algoritmo AS 190 appl. statist. (1983) vol. 32, n.2    *)
(* Avalia a probabilidade de 0 a Q para a amplitude estudentizada tendo v *)
(* graus de liberdade e R amostras. Usa o algoritmo  alnorm AS 66, para   *)
(* integral normal. Os arranjos vw e qw armazenam valores usados na soma  *)
(* das quadraturas. Os espacos nodulares sao controlados por step. pcutj  *)
(* e pcutk controla o truncamento. O numero minimo e maximo de passos sao *)
(* controlados por jmin, jmax, kmin e kmax. Precisao pode ser aumentada   *)
(* pelo uso de um gride mais refinado - aumentando a dimensao dos vetores *)
(* VW e QW, e jmin, jmax, kmin, kmax e 1/passo proporcional.              *)
(*                    31/12/96                                            *)
(**************************************************************************)
{$E+}{$N+}
Unit wsLund;
{$F+}{$O+}
Interface
// Uses wsFuncoesDeProbabilidade;

Function prtrng(Q, v, r: extended; Var ifault: Word): extended;

Function qtrng(p,v,r: extended; Var ifault: Word): extended;

Implementation

Uses
  Math, wsFuncoesDeProbabilidade;
Var
  vw,qw                                        : Array [1..60] of extended;
  g,gmid,h,r1,c,v2,gstep,gk,pk1,pk2,pk         : extended;
  wo,pz,x,jump,jj,hj,ehj,pj                    : extended;
(************************************************************************)
(* Segundo procedure                                                    *)
  t,q                                          : extended;
(************************************************************************)
(* Terceiro procedure                                                   *)
  nfault                                       : Word;
  q1,q2,p1,p2,e1,e2                            : extended;
(************************************************************************)

Function prtrng(Q, v, r: extended; Var ifault: Word): extended;
Var
  pcutj, pcutk, step,vmax, zero, fifth,
  half, one, two, cv1, cv2,cvmax               : extended;
  jmin,jmax,kmin,kmax                          : extended;
  cv                                           : Array [1..4] of extended;
  auxprtrng                                    : extended;

  (*********************************************************************)
  Procedure inicia_valores(v_iv: extended);
  Begin
    if v_iv<=7 then
      Begin
      pcutj:=3e-7 ;  (* 3e-7 *)
      pcutk:=1e-5    (* 1e-5  *)
      End
    else
      Begin
      pcutj:=0.00003; (*0.00003*)
      pcutk:=0.0001 (*0.0001*)
      End;
    step:=0.45;        (* 0.45    *)
    vmax:=10000.0;
    zero:=0.0;
    fifth:=0.20;
    half:=0.50;
    one:=1.0;
    two:=2.0;
    cv1:=0.193064705;
    cv2:=0.293525326;
    cvmax:=0.39894228;
    cv[1]:=0.318309886;
    cv[2]:=-0.268132716e-2;
    cv[3]:=0.347222222e-2;
    cv[4]:=0.833333333e-1;
    jmin:=3;    (* 3  *)
    jmax:=15;   (* 15 *)
    kmin:=7;    (* 7  *)
    kmax:=15;   (* 15 *)
  end;
  (******************************************************************)
  procedure proc19;
  Begin
    h:=step*power(v,-half);
    v2:=v*half;
    if v = one then c:=cv1
    else if v = two then c:=cv2
    else c:=sqrt(v2)*cv[1]/(one +((cv[2]/v2 + cv[3])/v2 + cv[4])/v2);
    c:= ln(c*r*g*h);
  End;
  (*****************************************************************)
  procedure proc20;
  Begin
    gstep:=g;
    qw[1]:=-one;
    qw[trunc(jmax)+1]:=-one;
    pk1:=one;
    pk2:=one;
  end;
  (*******************************************************************)
  Procedure proc23(Var j,k: extended);
     var
       auxp_23 : extended;
  Begin
//  function NInt(x: Double; upper: Boolean): Double;
    pj:=zero;
//    x:=(1-apnorm(gk-qw[trunc(jj)]))-pz;
    x:=(1-NInt(gk-qw[trunc(jj)],False))-pz;
    auxp_23:=wo+vw[trunc(jj)]+r1*ln(x);
    if ((auxp_23<-80) and (x>zero)) then
      pj:=0
    else
      if x>zero then pj:=exp(auxp_23);
    pk:=pk+pj;
    if (pj<=pcutj) and ((jj>jmin) or (k > kmin)) then
      j:=trunc(jmax)+1
  End;
  (********************************************************************)
  Procedure loop_in(Var k: extended);
    Var
      j            : extended;
  Begin
    j:=1;
    While j<=jmax do
      Begin
      jj:=j +jump;
      if qw[trunc(jj)] >zero then
        proc23(j,k)
      else
        Begin
        hj:=h*j;
        if (j < jmax) then qw[trunc(jj)+1]:=-one;
        ehj:=exp(hj);
        qw[trunc(jj)]:=q*ehj;
        vw[trunc(jj)]:=v*(hj+half-ehj*ehj*half);
        proc23(j,k);
        End;
      j:=j+1;
      End
  End;
  (*********************************************************************)
  Procedure Loop_out(q: extended);
  Var
    k                                            : extended;
  Begin
    k:=1;
    While (k<=round(kmax)) do
      Begin
      Gstep:=Gstep-g;
      Repeat
        gstep:=-gstep;
        gk:=gmid+gstep;
        pk:=zero;
        if (pk2<=pcutk) and (k > kmin) then
          Begin
          auxprtrng:=auxprtrng+pk;
          if (( k > kmin) and (pk<=pcutk) and (pk1<=pcutk)) then
            Begin
            k:=trunc(kmax)+1;
            gstep:=-1
            End
          else
            begin
            pk2:=pk1;
            pk1:=pk;
            End
          End
        Else
          Begin
          wo:=c - gk*gk*half;
          pz:=1-NInt(gk,False);
          x:=(1-NInt(gk-Q,False))-pz;
//          pz:=1-apnorm(gk);
//          x:=(1-apnorm(gk-Q))-pz;
          if ((wo+r1*ln(x))<-80)  then
            pk:=0
          else
            if (x > zero) then pk:=exp(wo+r1*ln(x));
          if V > vmax then
            Begin
            auxprtrng:=auxprtrng+pk;
            if (( k > kmin) and (pk<=pcutk) and (pk1<=pcutk)) then
              Begin
              k:=trunc(kmax)+1;
              gstep:=-1
              End
            else
              begin
              pk2:=pk1;
              pk1:=pk;
              End
            End
          else
            Begin
            jump:=-jmax;
            Repeat
              jump:=jump+jmax;
              loop_in(k);
              h:=-h;
            Until (H>=Zero);
            auxprtrng:=auxprtrng+pk;
            if (( k > kmin) and (pk<=pcutk) and (pk1<=pcutk)) then
              Begin
              k:=trunc(kmax)+1;
              gstep:=-1
              End
            else
              begin
              pk2:=pk1;
              pk1:=pk;
              End
            End;
          End;
      Until (Gstep<=zero);
      k:=k+1;
    End;
  End;
  (*******************************************************************)
Begin
  inicia_valores(v);(* iniciar valores das constantes - variaveis *)
  (* ---------------------------- checar valores iniciais ------------ *)
  auxprtrng:=zero;
  ifault:=0;
  if ((v< one) or (r <two)) then ifault:=1;
  if ((Q<=zero) or (ifault=1)) then
    begin
    prtrng:=0;
    exit;
    end;
  (* Calculo de constantes, ponto medio, ajuste de passos *)
  G:=STEP*Power(r,-FIFTH);
  GMID:=half*ln(r);
  r1:=r-one;
  c:=ln(r*g*cvmax);
  if c > vmax then
    Begin
    proc20;
    loop_out(q)
    End
  else
    Begin
    proc19;
    proc20;
    loop_out(q)
    end;
  prtrng:=auxprtrng
End;

(**************************************************************************)
(* algoritmo AS 190.2 Appl. Stat. (1983) vol. 32 no.2                     *)
(* Calcula um percentil P inicial Dist.da amplitude studentizada com V GL *)
(* e R amostras para probabilidade P [0.80..0.995]                        *)
(* usa funcoes de inversoes da normal                                     *)
(**************************************************************************)
Function qtrngo(p,v,r: extended; Var ifault: Word):extended;
  Var
    Vmax,half,one,four,c1,c2,c3,c4,c5 : extended;
    Erro: Word;
(*********************************************************************)
  Procedure inic_val;
  Begin
    vmax:=120.0;
    half:=0.5;
    one:=1.0;
    four:=4.0;
    c1:=0.8843;
    c2:=0.2368;
    c3:=1.214;
    c4:=1.208;
    c5:=1.4142
  End;
  (******************************************************************)
Begin

//NInv(p, eps: Double; upper: Boolean; var ErrCode : word): Double;

  inic_val;
  T:= NInv(half+half*p,1e-9,False,Erro);
//  T:= apnorminv(half+half*p);
  if (v < vmax) then t:=t+(t*t*t+t)/v/four;
  q:=c1-c2*t;
  if (v < vmax) then q:=q-c3/v+c4*t/v;
  qtrngo:=t*(q*ln(r-one)+c5)
End;
(**************************************************************************)
(* Algoritmo AS 190.1 Appl. Stat. (1983) vol. 32 no.2                     *)
(* aproxima o percentil P para a dist. da amplitude studentizada com V GL *)
(* e R amostras para probabilidade P [0.90..0.99]                         *)
(* usa funcoes de inversoes da normal e direta prtrng e qtrngo            *)
(**************************************************************************)
Function qtrng(p,v,r: extended; Var ifault: Word): extended;
  Var
    jmax, pcut, p75, p80,p90,p99,p995,p175,one, two, five : extended;
    j                                                     : extended;
    auxqtrng                                              : extended;
    Erro                                                  : Word;
  (*******************************************************************)
  procedure valores_inic;
  Begin
    jmax:=18;
    pcut:=0.0001;
    p75:=0.75;
    p80:=0.80;
    p90:=0.90;
    p99:=0.99;
    p995:=0.995;
    p175:=1.75;
    one:=1.0;
    two:=2.0;
    five:=5.0;
  End;
  (*******************************************************************)

//function TInv(p, n, eps: Double; upper, OneSided: Boolean; var ErrCode : word): Double;

Begin
  if r=2 then
    Begin
    auxqtrng:=TInv(0.5+p/2,v,1.0e-9,False,True,Erro)*1.41421356; //sqrt(2);
//    auxqtrng:=apinvte(trunc(v),0.5+p/2)*sqrt(2);
    qtrng:=auxqtrng
    End
  else
    Begin
    Valores_inic;
  (* checar valores iniciais                                       *)
    ifault:=0;
    nfault:=0;
    if (v<one) or (r<two) then ifault:=1;
    //if (p<p90) or (p>p99) then ifault:=2;
    if ifault<>0 then
      Begin
      if nfault<>0 then ifault:=9;
      qtrng:=0;
      exit;
      End
    else
      Begin
  (* Obtem valores iniciais                                        *)
      q1:=qtrngo(p,v,r,nfault);
      if nfault<>0 then
        Begin
        if nfault<>0 then ifault:=9;
        qtrng:=0;
        exit;
        End;
      Repeat
        p1:=prtrng(q1,v,r,nfault);
        if p1>p then q1:=q1-0.4;
        if q1<0 then q1:=0.1;
      Until p1<p;
      if nfault<>0 then
        Begin
        if nfault<>0 then ifault:=9;
        qtrng:=0;
        exit;
        End;
      auxqtrng:=q1;
      if abs(P1-P)<pcut then
        Begin
        if nfault<>0 then ifault:=9;
        auxqtrng:=q1;
        End;
      if P1 > P then P1:=P175*p-p75*p1;
      if P1 < P then P2:=P+(p-p1)*(one-p)/(one-p1)*p75;
      //if P2 < P80 then P2:=P80;
      //if P2 > P995 then P2:=P995;
      q2:=q1+0.5;
      p2:=p+0.001;
      Repeat
        p2:=prtrng(q2,v,r,nfault);
        if p2<p then q2:=q2+0.4;
        if q2<0 then q2:=1;
      Until p2>p;
      if q2<q1 then q2:=q1+0.01;
      if nfault<>0 then
        Begin
        if nfault<>0 then ifault:=9;
        qtrng:=0;
        exit;
        End;
   (* Refinamento do procedimento                                 *)
      j:=2;
      while j<=jmax do
        Begin
        p2:=prtrng(q2,v,r,nfault);
        if nfault<>0 then
          Begin
          if nfault<>0 then ifault:=9;
          j:=jmax+1;
          End
        else
          Begin
          e1:=p1-p;
          e2:=p2-p;
          if e2-e1<>0 then auxqtrng:=(e2*q1-e1*q2)/(e2-e1);
          if abs(e1)< abs(e2) then
            Begin
            if (abs(p1-p) <pcut*five) then
              Begin
              if nfault<>0 then ifault:=9;
              j:=jmax+2
              End;
            q1:=auxqtrng;
            p1:=prtrng(q1,v,r,nfault);
            End
          else
            Begin
            q1:=q2;
            p1:=p2;
            q2:=auxqtrng;
            End;
          End;
        j:=j+1;
      End;
      qtrng:=auxqtrng
    End
  End (* else r>2 *)
End;
(**************************************************************************)
End.
