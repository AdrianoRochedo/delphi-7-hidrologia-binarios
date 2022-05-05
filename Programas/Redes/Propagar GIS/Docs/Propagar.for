       PROGRAM PROPAGAR
C /===============================================VERSAO DE 26/02/97=====\
C||     Simulacao hidrologica distribuida em bacia hidrografica         ||
C||     Fase 2: Propagacao de vazoes na bacia com atendimento a demandas||
C||             e geracao de energia eletrica                           ||
C||                                                                     ||
C||     Este programa utiliza os valores de contribuicao difusa computa-||
C||     da pelo programa CONTRIB e ordenada-as de acordo com a topologia|| 
C||     da bacia que e' nele definida. Os intervalos de tempo das con-  ||
C||     tribuicoes e' grande. Por exemplo, cinco dias ou mensal. Com is-||
C||     to as questoes relacionadas ao amortecimento do hidrograma du-  ||
C||     te a propagacao nao necessitam ser consideradas.                ||
C||                                                                     ||
C||     A operacao do sistema e' inicialmenteda na Subrotina            ||
C||     Planeja: no caso de curso de agua sem reservatorio e' estabele- ||
C||     cido o suprimento `as demandas existentes, prioritaria e secun- ||
C||     daria, com eventual racionamentos; no caso de existencia de re- ||
C||     servatorios, alem do suprimento sao definidas as descargas de-  ||
C||     fluentes. A Sub-rotina Opera ira', `a posteriori, avaliar a via-||
C||     bilidade, ratificar ou retificar o planejamento; alem disto     ||
C||     estabelece uma descarga que visa ao atendimento de condicoes    ||
C||     ambientais ou legais. Tanto Planeja quanto Opera deverao ser    ||
C||     programados pelo usuario para adequar-se `a estrategia e tatica ||
C||     de gerenciamento da bacia.                                      ||
C||                                                                     ||
C||     E'possivel o calculo de um indice de performance operacional, ou||
C||     "funcao-objetivo" a ser programado pelo usuario na funcao OBJET ||
C||                                                                     ||
C||=====================================================================||
C||   Concepcao e programacao : Antonio Eduardo Lanna                   ||
C||                             Instituto de Pesquisas Hidraulicas,UFRGS||
C||                             Versao original de Julho de 1991        ||
C \======================================================================/
C
C  /---------------------------------------------------------------------\
C  |    DICIONARIO                                                       |  
C  |                                                                     |
C  |    Variaveis de entrada                                             |
C  |                                                                     |
C  |    KANO : numero de anos de simulacao                               | 
C  |    i : indice temporal de intervalos de simulacao - i = 1, ..., M.  |
C  |    N : numero de PC's que a rede de drenagem e' sub-dividida.       |
C  |        deve ser <  50. Valores > necessitarao de redefinicao das    |
C  |        das dimensoes de certas variaveis.                           | 
C  |    j : indice de pontos caracteristicos (PC),j = 1, ..., N.         |
C  |                                                                     |
C  |    IANO(k) : ano de simulacao                                       |
C  |    H(j)  : hierarquia do PC : 1 para PCs de montante.               |
C  |    NT(j) : numero de tributarios do PC considerado.                 |
C  |    T(4,j)  : tributarios a cada PC (maximo de 4).                   |
C  |    P(j)    : precipitacao sobre o reservatorio (total no intervalo  |
C  |              em (mm/km2) area molhada                               |
C  |    Q(i,j)  : contribuicoes difusas aos PCs, computadas no programa  |
C  |              CONTRIB ou diretamente aqui (media/intervalo em m3/s)  |
C  |    DDI(i)  : demandas difusas a cada PC                             |
C  |    D1(j)   : demanda prioritaria do PC j durante intervalo tempo i  |
C  |    D2(j)   : demanda secundaria do PC j durante intervalo tempo i   |
C  |    D3(j)   : demanda terciaria do PC j durante intervalo tempo i    |
C  |              (valores medios no intervalo em m3/s/escala)           |
C  |    ESC1(j) : escala de desenvolvimento da demanda prioritaria que   |
C  |                e'suprida no PC j                                    |
C  |    RET1(j) : fracao de retorno da demanda prioritaria; se 0.0       |
C  |              demanda e' totalmente consumida; se 1.0, nao consuntiva|
C  |    ESC2(j) : escala de desenvolvimento da demanda secundaria        |
C  |                no PC j                                              |
C  |    RET2(j) : fracao de retorno da demanda secundaria; se 0.0        |
C  |              demanda e' totalmente consumida; se 1.0, nao consuntiva|
C  |    ESC3(j) : escala de desenvolvimento da demanda prioritaria que   |
C  |                e'suprida no PC j                                    |
C  |    RET3(j) : fracao de retorno da demanda prioritaria; se 0.0       |
C  |              demanda e' totalmente consumida; se 1.0, nao consuntiva|
C  |    IKxy : controle - determina se o retorno do suprimento `a deman- |
C  |           da x pode ser usado para suprir a demanda y. Identificado |
C  |           pela variavel ORDEM(J) que estipula a ordem espacial com  |
C  |           que as demandas sao supridas. Se ORDEM(j) = 123, supre-se |
C  |           primeiro a demanda 1 (prioritaria) depois a 2 (secundaria)|
C  |           e por ultimo a 3 (terciaria). O contrario ocorreria se    |
C  |           ORDEM(j) fosse igual a 321.                               |
C  |                                                                     |
C  |    CENER(J) : coeficiente de transformacao energetica para hidroele-|
C  |               trica do PC j                                         |
C  |                                                                     |
C  |    IMPR : variavel auxiliar que define a forma de saida             |
C  |         1- Arquivo de saida com simulacao detalhada                 |
C  |         2- Arquivos de saida com defluencias, demandas supridas     |
C  |            e armazenamentos                                         |
C  |         3- as duas opcoes                                           |
C  |                                                                     |
C  |    Parametros referentes a regularizacao no PC                      |
C  |                                                                     |
C  |    SMX(j) : armazenamento maximo no reservatorio (Hm3).             |
C  |    SMN(j) : armazenamento minimo no reservatorio (Hm3).             |
C  |                                                                     |
C  |    Condicoes iniciais inseridas via arquivo de entrada              |
C  |                                                                     |
C  |    S(j)  : armazenamento inicial no reservatorio que controla PC    |
C  |    XP(J)   : descarga defluente planejada do curso de agua j (Hm3)  |
C  |    XCONT(J): Contribuicao afluente do curso de agua ou descarga a   |
C  |              ser defluente do reservatorio, computada em sub-rotina |
C  |              OPERA a ser definida pelo usuario  (Hm3).              |
C  |    DSAT1 ou DS1(j)  : Demanda prioritaria satisfeita (Hm3)          |
C  |    DSAT2 ou DS2(j)  : Demanda secundaria  satisfeita (Hm3)          |
C  |    DSAT3 ou DS3(j)  : Demanda terciaria   satisfeita (Hm3)          |
C  |                                                                     |
C  |    Contadores de falha de atendimento aos suprimentos:              |
C  |                                                                     |
C  |    KPMF(j) : contador de falhas mensais de suprimento prioritario   |
C  |    KPCR(j) : idem falhas criticas da dem. prioritaria               |
C  |    KPAF(j) : contador de falhas anuais  de suprimento prioritario   |
C  |    KSMF(j) : contador de falhas mensais de suprimento secundario    |
C  |    KSCR(j) : idem falhas criticas da dem. secundaria                |
C  |    KSAF(j) : contador de falhas anuais  de suprimento secundario    |
C  |    KTMF(j) : contador de falhas mensais de suprimento terciario     |
C  |    KTCR(j) : idem falhas criticas da dem. terciaria                 |
C  |    KTAF(j) : contador de falhas anuais  de suprimento terciario     |
C  |    KIPF(j) : indicador de existencia de falha suprim. prioritario   |
C  |    KISF(j) : indicador de existencia de falha suprim. secundario    |
C  |    KITF(j) : indicador de existencia de falha suprim. terciario     |
C  |                                                                     |
C  |    Variaveis com nomes dos arquivos de dados de entrada             |
C  |                                                                     |
C  |    AQVAZ : vazoes das bacias incrementais a cada PC                 |
C  |    AQDDI : demandas difusas supridas na sub-bacia dos PC's          |
C  |    AQDPR : demandas prioritarias em cada PC, por intervalo de tempo |
C  |    AQDSC : demandas secundarias em cada PC, por intervalo de tempo  |
C  |    AQDTC : demanda terciarias em cada PC, por intervalo de tempo    | 
C  |    AQETP : evapotranspiracoes potenciais sobre cada reservatorio    |
C  |    AQP   : precipitacoes sobre cada reservatorio                    |
C  |                                                                     |
C  |    Arquivos temporarios                                             |
C  |    --------------------                                             |
C  |    FORT6   : armazenamento temporarios das demandas prioritarias    |
C  |    FORT7   : armazenamento temporario de precipitacoes              |
C  |    FORT8   : armazenamento temporario de etp                        |
C  |    FORT9   : armazenamento temporario de demandas secundarias       |
C  |    FORT10  : armazenamento temporario de demandas terciarias        |
C  |                                                                     |
C  |    Arquivos de saida: unidades utilizadas                           |
C  |    -----------------                                                |
C  |      11    : saida completa tipo simulacao                          |
C  |      12    : saida grafica: defluencias dos PC                      |
C  |      13    : saida grafica: suprimento a demanda prioritaria        |
C  |      14    : saida grafica: suprimento a demanda secundaria         |
C  |      15    : saida grafica: suprimento a demanda terciaria          |
C  |      16    : saida grafica: armazenamentos nos reservatorios        |
C  |      17    : saida grafica: energia gerada                          |
C  |                                                                     |
C  |    Variaveis de saida calculadas no modelo                          |
C  |                                                                     |
C  |    S1(j)   : armazenamentos no reservatorio do PC j no inicio       |
C  |              de cada intervalo de tempo (Hm3)                       |
C  |    S (j)   : armazenamentos no reservatorio do PC j no final        |
C  |              de cada intervalo de tempo i(Hm3).                     |
C  |    DS1(j), DS2(j) e DS3(j): demandas prioritaria, secundaria e      |
C  |              terciaria supridas no PC j, no intervalo de tempo i    |
C  |              [m3/s (subroutine PLANEJA)  ou Hm3/intervalo].         |
C  |    X(j)    : contribuicoes de PCs fluviais ou descarga defluente    |
C  |              do PC para jusante, (totais no intervalo em Hm3).      |
C  |    ENERG(j) : energia gerada no PCs (em MW/intervalo)               |
C  \---------------------------------------------------------------------/
c
C     Dimensionado para ate' 50 PC's e 60 anos, com simulacao mensal
c
	CHARACTER*80 TITU1,TITU2,AQVAZ,AQDPR,AQDSC,AQDTC,AQP,AQETP,AQDDI,
     +AQEFL,AQDS1,AQDS2,AQDS3,AQS,AQE,IDENT
	CHARACTER*1 ASTERX,IFALHA(3,50),IBR,IF1,IF2                                         
	CHARACTER*8 PCNOM(50)             
	INTEGER H(50),NT(50),T(4,50),HIERAR,JRES(50),ORDEM(50)
	INTEGER IANO(60),NDM(12),NDQ(24),NDD(36),NDC(72),DIAS
	DIMENSION Q(720,50),DDI(720),P(50),EV(50),ESC1(50),ESC2(50),
     +ESC3(50),RET1(50),RET2(50),RET3(50),D1(50),D2(50),D3(50),DS1(50),
     +DS2(50),DS3(50),X(50),XCONT(50),CENER(50),ENERG(50),EFIRM(50),
     .E(50),S1(50),S(50),SMX(50),SMN(50),XP(50),KPMF(50),KPCR(50),
     +KPAF(50),KSMF(50),KSCR(50),KSAF(50),KTMF(50),KTCR(50),KTAF(50),
     +KIPF(50),KISF(50),KITF(50),KAV(50),AREA(10,50),VOLAR(10,50),
     +KQV(50),QUEDA(10,50),VOLQU(10,50)
c
	DATA NDM/31,28,31,30,31,30,2*31,30,31,30,31/ !Estabelece numero 
	DATA NDQ/15,16,15,13,15,16,3*15,16,3*15,16,  !de dias de cada mes, 
     +15,16,3*15,16,3*15,16/                       !quinzena, decendio 
	DATA NDD/2*10,11,2*10,8,2*10,11,5*10,11,5*10,!e pentadio (5 dias)
     +11,2*10,11,5*10,11,5*10,11/              
	DATA NDC/5*5, 6, 5*5, 3, 5*5, 6, 11*5, 6,       
     +11*5, 6, 5*5, 6, 11*5, 6, 11*5, 6/       
	DATA ASTERX /1h*/,IBR/1h /,IF1/1h//,IF2/1h\/
c
	OPEN(20,FILE=' ',STATUS='OLD')                  ! Abre arquivo 
	WRITE(*,*) ' Lendo arquivo de dados de entrada' ! de dados  
	READ(20,*) TITU1,TITU2
	READ(20,*) KANO,(IANO(K),K=1,KANO)
	READ(20,*) M,N
      READ(20,*) (PCNOM(J),J=1,N)
      READ(20,*) (H(J),J=1,N)
      READ(20,*) (NT(J),J=1,N)
      READ(20,*) (ORDEM(J),J=1,N)        

      HMAX = 1               ! Inicializa indicador de hierarquia maxima
	DO 1111 J = 1,N
	HMAX = MAX1(H(J),HMAX) ! Calcula maxima hierarquia
	JRES(J) = 0            ! Zera JRES para todos PC's
	IF(NT(J).EQ.0) GOTO 1111
	NTJ = NT(J)
	READ(20,*) (T(K,J),K=1,NTJ)
1111  CONTINUE
c
	READ(20,*) AQVAZ,AQDDI,MDDI,AQDPR,MDPR,AQDSC,MDSC,AQDTC,MDTC,AQP,
     +MP,AQETP,METP,IMPR
	READ(20,*) (ESC1(J),RET1(J),J=1,N)
	READ(20,*) (ESC2(J),RET2(J),J=1,N)
	READ(20,*) (ESC3(J),RET3(J),J=1,N)
	IF(IMPR .GE. 2) READ(20,*) AQEFL,AQDS1,AQDS2,AQDS3,AQS,AQE
11113 READ(20,*) (SMN(J),J=1,N),(SMX(J),J=1,N),(S1(J),J=1,N)
      
C     KF = 0
	NRES = 0
C======================================================================!
	DO 1112 J = 1,N                    ! Ciclo sobre PC's            !
	IF(RET1(J) .GT. 1.0) RET1(J) = 1.0 ! Testa                       !
	IF(RET1(J) .LT. 0.0) RET1(J) = 0.0 ! e corrige                   !
	IF(RET2(J) .GT. 1.0) RET2(J) = 1.0 ! valores de                  !
	IF(RET2(J) .LT. 0.0) RET2(J) = 0.0 ! retorno                     !
	IF(RET3(J) .GT. 1.0) RET3(J) = 1.0 !                             !
	IF(RET3(J) .LT. 0.0) RET3(J) = 0.0 !                             !
c                                                                      !
	KPMF(J) = 0 ! Zera                                               !
	KPCR(J) = 0 !                                                    !
	KSMF(J) = 0 ! contadores                                         !
	KSCR(J) = 0 !                                                    !
	KTMF(J) = 0 ! de falhas                                          !
	KTCR(J) = 0 !                                                    !
	KPAF(J) = 0 ! mensais                                            !
	KSAF(J) = 0 ! e anuais                                           !
	KTAF(J) = 0                                                      !
c                                                                      !
	IF(SMX(J) .EQ. SMN(J)) GOTO 1112                                 !
C     JRES(J) identifica posicao das informacoes sobre um reservatorio !
C     no curso de agua J nos arranjos E(J) e A(*,J)                    !
	NRES    = NRES + 1                                               !
	JRES(J) = NRES                                                   !
	IF(S1(J) .LT. SMN(J)) S1(J) = SMN(J)      ! Corrige armazenamento!
	IF(S1(J) .GT. SMX(J)) S1(J) = SMX(J)      ! inicial se necessario!
	READ(20,*) KAV(J)                                                !
	KAVJ = KAV(J)                                                    !
	READ(20,*)(AREA(K,J),K=1,KAVJ),(VOLAR(K,J),K=1,KAVJ) ! f(A-V)    !
	READ(20,*) KQV(J)                                                !
	KQVJ = KQV(J)                                                    !
	READ(20,*)(QUEDA(K,J),K=1,KQVJ),(VOLQU(K,J),K=1,KQVJ)! f(H-V)    !
	READ(20,*) CENER(j),EFIRM(J) ! coef. transf. energetica          !
1112  CONTINUE                    ! Fim de ciclo sobre PC's            !
      READ(20,*) FCRIT            ! Indicacao de nivel de falha critica!
      close (20)                                                       !
C======================================================================!      
C     Le arquivos de dados : vazoes afluentes, demandas, chuvas e etp  !
C     Grava em arquivo temporario por economia de memoria.             !
c                                                                      !
C/====Demandas prioritarias - inicio==================================\!
	OPEN(11,FILE=AQDPR,STATUS='OLD')                                 !
	WRITE(*,*) ' Lendo demandas prioritarias PC por PC'              !
	DO 1091 J = 1, N                                                 !
	READ(11,*) IDENT,(Q(I,J),I=1,MDPR)                               !
c                                                                      !
	DO 111 I = 1,MDPR             ! Transforma demandas unitarias    !
111   Q(I,J) = Q(I,J) * ESC1(J)     ! em demandas totais prioritarias  !
1091  CONTINUE                                                         !
	CLOSE(11)                                                        !
c                                                                      !
	OPEN(6,FILE='FORT6')          ! Grava demandas hidricas          !
	DO 1101 I = 1, MDPR           ! prioritarias,                    !
1101  WRITE(6,1113) (Q(I,J),J=1,N)  ! intervalo por                    !
1113  FORMAT(G12.5)                 ! intervalo                        !
	REWIND (6)                                                       !
C\====Demandas prioritarias - fim======================================/
c
C     Demandas secundarias - inicio
	OPEN(11,FILE=AQDSC,STATUS='OLD')
	WRITE(*,*) ' Lendo demandas hidricas secundarias'
	DO 109 J = 1, N
	READ(11,*) IDENT,(Q(I,J),I=1,MDSC)
c
	DO 108 I = 1,MDSC             ! Transforma demandas unitarias em 
108   Q(I,J) = Q(I,J) * ESC2(J)     ! demandas totais secundarias
109   CONTINUE
	CLOSE(11)      
c
	OPEN(9,FILE='FORT9')          ! Grava demandas 
	DO 110 I = 1, MDSC            ! secundarias, 
110   WRITE(9,1113) (Q(I,J),J=1,N)  ! intervalo por intervalo 
	REWIND (9)
c
C     Demandas terciarias - inicio
	OPEN(11,FILE=AQDTC,STATUS='OLD')
	WRITE(*,*) ' Lendo demandas hidricas terciarias'
	DO 1090 J = 1, N
	READ(11,*) IDENT,(Q(I,J),I=1,MDTC)

	DO 1080 I = 1,MDTC            ! Transforma demandas unitarias em 
1080  Q(I,J) = Q(I,J) * ESC3(J)     ! demandas totais terciarias
1090  CONTINUE
	CLOSE(11)      
c
	OPEN(10,FILE='FORT10')        ! Grava demandas 
	DO 1100 I = 1, MDTC           ! terciarias, 
1100  WRITE(10,1113) (Q(I,J),J=1,N) ! intervalo por intervalo
	REWIND (10)
C     Demandas terciarias - fim
c
	IF(NRES.EQ.0) GOTO 113        ! Se nao existe reservatorio pule 
C     Precipitacoes
	OPEN(11,FILE=AQP,STATUS='OLD')
	WRITE(*,*) ' Lendo precipitacoes em cada reservatorio'
	DO 1115 J = 1, NRES
	READ(11,*) IDENT,(Q(I,J),I=1,MP)
1115  CONTINUE
	CLOSE(11)
c
	OPEN(7,FILE='FORT7')           ! Grava precipitacoes 
	DO 112 I = 1, MP               ! intervalo por 
112   WRITE(7,1113) (Q(I,J),J=1,NRES)! intervalo
	REWIND (7)
C     Evaporacoes potenciais
	OPEN(11,FILE=AQETP,STATUS='OLD')
	WRITE(*,*) ' Lendo etp em cada reservatorio'
	DO 1121 J = 1, NRES
	READ(11,*) IDENT,(Q(I,J),I=1,METP)
1121  CONTINUE
	CLOSE(11)
c
	OPEN(8,FILE='FORT8')           ! Grava etp 
	DO 1122 I = 1, METP            ! intervalo por 
1122  WRITE(8,1113) (Q(I,J),J=1,NRES)! intervalo
	REWIND (8)
C     Vazoes afluentes
113   OPEN(11,FILE=AQVAZ,STATUS='OLD')
	WRITE(*,*) ' Lendo vazoes afluentes PC por PC'
	READ(11,*) (IDENT,(Q(I,J),I=1,M),J=1,N)
	CLOSE(11)
C     Demandas difusas
      OPEN(11,FILE=AQDDI,STATUS='OLD')
      WRITE(*,*) ' Lendo demandas difusas PC por PC'
      DO 1123 J = 1,N      ! Ciclo sobre PC's 
	READ(11,*) IDENT,(DDI(I),I=1,MDDI)
      IDDI = 1
      DO 1123 I = 1,M             ! Ciclo temporal
      IF(IDDI .GT. MDDI) IDDI = 1 ! atencao correcao em 19/2/97
      Q(I,J) = Q(I,J) - DDI(IDDI) ! Retira demandas difusas das afluencias
      IDDI = IDDI + 1
1123  CONTINUE
      CLOSE(11)
c
C     Abre arquivos de variaveis de saida e imprime informacoes iniciais
	WRITE(*,*) ' Abertura da saida '
	IF (IMPR . EQ. 2) GOTO 1131
	OPEN(11,FILE=' ',STATUS='UNKNOWN') ! Abertura do arquivo completo
	WRITE(11,2220) TITU1,TITU2
	NPCH = 0
	DO 114 J = 1,N
	IF(CENER(J) .GT. 0.) NPCH = NPCH+1 ! Conta numero de PCh's 
	IF(CENER(J) .GT. 0.) write(11,22202)PCNOM(J),SMN(J),SMX(J),S1(J),
     +ESC1(J),RET1(J),ESC2(J),RET2(J),ESC3(J),RET3(J),ORDEM(J),asterx
	IF(CENER(J) .EQ. 0.0) write(11,22201)PCNOM(J),SMN(J),SMX(J),S1(J),
     +ESC1(J),RET1(J),ESC2(J),RET2(J),ESC3(J),RET3(J),ORDEM(J)      
114   CONTINUE
2220  FORMAT('SISTEMA DE APOIO AO GERENCIAMENTO DE BACIAS HIDROGRAFICAS
     + - SAGBAH'/'Desenvolvido por A. Eduardo Lanna'/'Instituto de Pesqu
     +isas Hidraulicas - UFRGS'/'Versao 97.2'//'RESULTADOS DO PROPAGAR'/
     +/1X,A/1x,A//'PARAMETROS BASICOS DESTA SIMULACAO'/ 
     +'   PC     ARMAZ/TO  ARMAZ/TO  ARMAZ/TO DEM. PRIORITARIA    '
     +'DEM. SECUNDARIA     DEM. TERCIARIA    ORDEM  PCs COM'
     +/'           MINIMO    MAXIMO    INICIAL ESCALA  '  
     +'RETORNO     ESCALA  RETORNO     ESCALA  RETORNO   ESPACIAL  PCH')
22201 FORMAT(A,1X,9(G9.4,1X),I3)
22202 FORMAT(A,1X,9(G9.4,1X)I3,6X,A)
	DO 1130 J = 1,N
	IF(SMX(J).EQ.SMN(J)) GOTO 1130
	KAVJ = KAV(J)
	WRITE(11,22203)PCNOM(J),(AREA(K,J),VOLAR(K,J),K=1,KAVJ)
22203 FORMAT(/'DADOS SOBRE RESERVATORIO NO PC ',A,/        
     +'FUNCAO AREA-VOLUME'/'      AREA        VOLUME'/
     +'      (KM2)        (HM3)'/(1X,G12.4,1X,G12.4))
	KQVJ = KQV(J)
	WRITE(11,22204) (QUEDA(K,J),VOLQU(K,J),K=1,KQVJ)
22204 FORMAT(/'FUNCAO QUEDA HIDRAULICA-VOLUME'/
     +'      QUEDA        VOLUME'/'       (M)         (HM3)'/
     +(1X,G12.4,1X,G12.4))
	WRITE(11,22205) CENER(J)
22205 FORMAT(/'FATOR DE CONVERSAO DE ENERGIA: ',G12.4)  
1130  CONTINUE

1131  IF(IMPR .EQ. 1) GOTO 1133
	IF(IMPR.EQ.2) OPEN(11,FILE=' ',STATUS='UNKNOWN') ! Abre e imprime
	WRITE(11,11311) AQEFL,AQDS1,AQDS2,AQDS3,AQS      ! resultados
11311 FORMAT(//' ARQUIVO DE SAIDA DE RESULTADOS'/'Atencao: solicitadas '
     +'saidas graficas; encontre-as em: '//
     +' Vazoes defluentes .................. ', A/
     +' Suprimento demanda prioritaria ..... ', A/
     +' Suprimento demanda secundaria ...... ', A/
     +' Suprimento demanda terciaria ....... ', A/
     +' Armazenamentos nos reservatorios ... ', A/)
     
      OPEN(12,FILE=AQEFL,STATUS='UNKNOWN')             ! Abertura de
      OPEN(13,FILE=AQDS1,STATUS='UNKNOWN')             ! de  
      OPEN(14,FILE=AQDS2,STATUS='UNKNOWN')             ! arquivos
      OPEN(15,FILE=AQDS3,STATUS='UNKNOWN')             ! de
      IF(NRES.NE.0) OPEN(16,FILE=AQS,STATUS='UNKNOWN') ! resultados
      IF(NPCH.NE.0) OPEN(17,FILE=AQE,STATUS='UNKNOWN') ! 

C     Computa numero de estacoes anuais e determina tipo de intervalo 
C     temporal de simulacao 
1133  INTERV = M/KANO         ! INTERV: no. de intervalos/ano
	INTER = INTERV/12       ! INTERV = 12, 24, 36 ou 72
C      Se INTER : 1, intervalo e' mensal;  2, quinzenal;
C                 3, decendial;            6, pentadial (5 dias)
C=======================================================================
C/-------------------------->Inicio do ciclo de simulacao temporal: anos    
	DO 201 K = 1,KANO ! Inicia a simulacao temporal : anos
	WRITE(*,9995) IANO(K)
9995  FORMAT(' Computando ano : ',I4,'  aguarde !')

	DO 4442 J = 1,N  ! Zera indicadores 
	KIPF(J) = 0      ! de  
	KISF(J) = 0      ! falhas
	KITF(J) = 0      ! anuais
4442  CONTINUE       

	IBIS = 0                             ! Identifica ano 
	IF( MOD(IANO(K),4) .EQ. 0 ) IBIS = 1 ! bissexto (IBIS = 1)

C=======================================================================
C/--------->Inicio do ciclo de simulacao temporal : estacoes em cada ano
	DO 200 II = 1, INTERV
      KF=0
C     Recupera valores demanda terciaria D3, secundaria D2 e prioritaria 
C     D1, precipitacoes P e evapotransp. potencial E cada intervalo de 
C     simulacao
	
4447  READ(10,*,END=4448) (D3(J),J=1,N)
	GOTO 4443
4448  REWIND (10)             ! Se chegou ao fim do arquivo, 
	GOTO 4447               ! volta ao seu inicio

4443  READ(9,*,END=4444) (D2(J),J=1,N)
	GOTO 4445
4444  REWIND (9)              ! Se chegou ao fim do arquivo, 
	GOTO 4443               ! volta ao seu inicio

4445  READ(6,*,END=4446) (D1(J),J=1,N)
	GOTO 5000

4446  REWIND (6)              ! Se chegou ao fim do arquivo, 
	GOTO 4445               ! volta ao seu inicio

5000  IF(NRES .EQ. 0.)  GOTO 6000
	
5110  READ(7,*,END=5554) (P(J),J=1,NRES)! Recupera valores chuva e etp
5111  READ(8,*,END=5555) (E(J),J=1,NRES)! dos arquivos FORT7 e FORT8
	GOTO 6000
5554  REWIND (7)              ! Se chegou ao fim do arquivo, 
	GOTO 5110               ! volta ao seu inicio
5555  REWIND (8)              ! Se chegou ao fim do aquivo,
	GOTO 5111               ! volta ao seu inicio

6000  GOTO (11,12,13,14,14,16), INTER   ! Numero do dias intervalo 

11    DIAS = NDM(II)                    ! Intervalos 
	IF(II .EQ. 2 ) DIAS = DIAS + IBIS ! mensais
	GOTO 17
12    DIAS = NDQ(II)                    ! Intervalos 
	IF(II .EQ. 4 ) DIAS = DIAS + IBIS ! quinzenais
	GOTO 17
13    DIAS = NDD(II)                    ! Intervalos 
	IF(II .EQ. 6 ) DIAS = DIAS + IBIS ! decendiais
	GOTO 17
14    WRITE(*,*) 'ATENCAO : ERRO - VALORES DE KANO E M INCOMPATIVEIS'
	STOP 3
16    DIAS = NDC(II)                     ! Intervalos 
	IF(II .EQ. 12 ) DIAS = DIAS + IBIS ! cinquendiais 
	GOTO 17

C    Computa constantes de transformacao 
17    C1 = FLOAT(DIAS) * 0.0864          ! C1 : m3/s para Hm3/interv 
	C2 = .001                          ! C2 : mm * km2 para Hm3 

C    Fase estrategica : planeja a operacao dos reservatorios, ou aten-
C    dimento `as demandas
C     III = II*INTER                 ! Posicao do intervalo no ano ????

C/=====================================================================\
C     Subroutine PLANEJA: estrategia operacional: define valores       !
C     estrategicos de suprimento as demandas                           !
      IV = II+(K-1)*INTERV   ! Posicao temporal na amostra             !
	CALL PLANEJA (S,SMN,SMX,Q,IV,II,C1,D1,DS1,RET1,D2,DS2,RET2,D3,DS3!
     +,RET3,XP,N,JRES,NRES,NT,T)                                       !
C\=====================================================================/
      
C     Inicia a simulacao na ordem hierarquica : 1, 2, 3 ...
	HIERAR = 1
2     KONTA = 0

C=======================================================================
C/--------------------------->Inicio do ciclo de simulacao sobre os PC's 
	DO 100 J = 1,N                                      

C     Verifica se curso de agua J tem a hierarquia HIERAR, em computacao
	IF(H(J) .NE. HIERAR) GOTO 100
	KONTA = 1

C  |  Simulacao do PC J de hierarquia HIERAR         
C  |  Computa inicialmente a afluencia proveniente dos PCs fluviais de
C  |  montante.

	XCONT(J) = 0.0              ! Zera acumulador das afluencias
	IF(NT(J) .EQ. 0) GOTO 4     ! Se numero de PC a montante = 0, pula 
	NTRECH = NT(J)
C     NTRECH e' numero de cursos de agua contribuintes `aquele em pauta   
	DO 3 NTR = 1, NTRECH            ! acumula
	JTRECH = T(NTR,J)               ! defluencias
	XCONT(J) = XCONT(J) + X(JTRECH) ! de montante
3     CONTINUE                 
C     Nota: XCONT e' dado em Hm3/intervalo; razao: primeiro PC nao tem 
C     afluencia de PC's a montante; a partir do segundo PC, as vazoes 
C     ja' foram transfor-adas em Hm3/intervalo, conforme abaixo.

C  |  XCONT : Afluencia total = bacia incremental + PC's montante
C  |  Afluencia incremental e' corrigida para deficit de demanda
C  |  Como XCONT e' dada em Hm3 e Q em m3/s, usa C1 para transformar

4     XCONT(J) = XCONT(J) + AMAX1(C1*Q(IV,J),0.)
      X(J) = XCONT(J)      ! X(J) e' disponibilidade hidrica
      L = JRES(J)          ! Indicador posicao de dados p/reservatorio

C     Subrotina TOPOL define esquema captacoes e retornos em cada PC      
      CALL TOPOL(ORDEM(J),J,IK12,IK13,IK21,IK23,IK31,IK32)
      IF(L .GE. 1) GOTO 10 ! Se houver reservatorio va' para 10                  
	
C=======================================================================
C  /------------------------>Trecho fluvial sem controle de reservatorio
C  |  Balanco hidrico trivial sem variavel decisao - testa se afluencia
C  |  descontada e' suficiente para suprir demanda total cf. planejado
C  |  Atencao: na subroutine PLANEJA definiram-se valores de DSx(J)

      DSAT1 = C1*DS1(J)              ! Transforma suprimentos 
      DSAT2 = C1*DS2(J)              ! de m3/s para Hm3 no
      DSAT3 = C1*DS3(J)              ! intervalo de simulacao
	DS123 = DSAT1+DSAT2+DSAT3 ! ERRO: nao foi visto retorno!!!!!!!
	IF(X(J) .GE. DS123) GO TO 5 ! Se disponibilidade > demandas va' 5
	 
C  | Demandas nao podem ser totalmente supridas : racionamento
      CALL RACIONA(X(J),DSAT1,RET1(J),DSAT2,RET2(J),DSAT3,RET3(J),
     +IK12,IK13,IK21,IK23,IK31,IK32)
      DS1(J) = DSAT1
      DS2(J) = DSAT2
      DS3(J) = DSAT3
      X(J) = X(J)     ! mantem a mesma defluencia calculada em RACIONA
      GOTO 155
      
C  | Demandas podem ser supridas conforme planejado
5     DS1(J) = DSAT1                    ! Guarda suprimentos
	DS2(J) = DSAT2                    ! Desconta da disponibilidade
	DS3(J) = DSAT3                    ! os suprimentos e soma retornos
	X(J) = X(J)-(1-RET1(J))*DSAT1-(1-RET2(J))*DSAT2-(1-RET3(J))*DSAT3
	GO TO 155

C  \------------------>Fim do computo do PC sem controle de reservatorio
C=======================================================================
C  /------------------------>Trecho fluvial com controle de reservatorio
C  |  Balanco hidrico com decisao sobre descarga do reservatorio.
C  |  Inicialmente tenta implementar estrategia planejada
10    SF = S1(J)
	KCICLO = 0          ! Sub PLANEJA definiu suprimentos DSx(J)
105   DSAT1 = C1 * DS1(J) ! Transformacoes de m3/s para
	DSAT2 = C1 * DS2(J) ! Hm3/intervalo
	DSAT3 = C1 * DS3(J) ! de simulacao
	SM = (SF+S1(J))/2.  ! Estimativa armazenamento medio no intervalo
C  |  Calcula a area correspondente ao armazenamento medio
	AREAM = FINTER(J,AREA,VOLAR,KAV,SM)
C  |  [Evaporacao - chuva] no reservatorio=funcao armazenamento medio
	EV(J)=C2*(E(L)-P(L))*AREAM 
C  |  Afluencia total descontada da [evaporacao - chuva]
	XAFL = XCONT(J) - EV(J)
C  |  Fase tatica : determina a defluencia do reservatorio 
	XP1 = XP(J) ! XP1 e XP(J) dados em Hm3/intervalo
      QUEHI = FINTER(J,QUEDA,VOLQU,KQV,SM)
	CALL OPERA(II,J,S1(J),SMN(J),SMX(J),D1(J),DSAT1,RET1(J),
     +D2(J),DSAT2,RET2(J),D3(J),DSAT3,RET3(J),XAFL,XP1,C1,CENER(J),
     .EFIRM(J),QUEHI)
C     DSATx e' descarga operada para atender demanda x, x = 1,2,e 3
	X(J) = XP1 ! X e' defluencia, dada em Hm3/intervalo
C  |  Balanco hidrico do reservatorio: atende todas demandas planejadas
	S(J) = S1(J) + XAFL - DSAT1 - DSAT2 - DSAT3 - X(J)  
C  |  Verifica viabilidade fisica: limite minimo de armazenamento
	IF(S(J) .GE. SMN(J)) GOTO 130 ! S(J)>minimo:balanco OK; prossegue
C                                   ! em 130 testando armaz. maximo
C  |  Armazenamento final < que o minimo : corrige diminuindo defluencia 
C  |  do reservatorio e fazendo armazenamento igual ao minimo
	X(J) = X(J) - (SMN(J) - S(J))
	S(J) = SMN(J)
C  |  Verifica se defluencia e' viavel ( X(J)>= 0)
	IF(X(J) .GE. 0.0) GOTO 15 ! se for encerra este PC em 15
C     Defluencia menor que zero: racionamento deve ser imposto!
C     Avalia disponibilidades como soma suprimentos planejados, menos
C     deficit (outro criterio qualquer de manejo de racionamento podera'
C     ser usado se programado nas Subrotinas PLANEJA ou OPERA e trans-
C     mitido ao programa principal sem problemas de viabilidade
	X(J) = DSAT1 + DSAT2 + DSAT3 + X(J) ! Recupera afluencia inicial 
	IF(X(J) .GT. 0.) GOTO 115           ! Se existe agua, raciona
C     Nao ha' agua; zera defluencia e suprimentos
	DSAT3 = 0.0
	DSAT2 = 0.0
	DSAT1 = 0.0
	X(J)  = 0.0
	GOTO 15      ! Verifica convergencia em 15
115   CALL RACIONA(X(J),DSAT1,RET1(J),DSAT2,RET2(J),DSAT3,RET3(J),                  
     +IK12,IK13,IK21,IK23,IK31,IK32)                           
      GOTO 15 
C  |  Verifica limite maximo de armazenamento 
130   IF(S(J).LE.SMX(J)) GOTO 15
C  |  Armazenamento > maximo : faz ele maximo e verte o restante
	X(J) = X(J) + (S(J) - SMX(J))
	S(J) = SMX(J)
C  |  Final da propagacao em reservatorio : verifica convergencia
15    IF(ABS(SF-S(J)) .LE. 0.01*SMX(J)) GOTO 152 ! Verifica convergencia
	KCICLO = KCICLO + 1                        ! do armazenamento em
	IF(KCICLO.gt.10) goto 152                  ! no maximo 10 ciclos
	SF = S(J)                                  
	GOTO 105                                   ! Mais um ciclo 
C  |  Caso tenha convergido, guardar suprimentos e calcular defluencia
152   DS1(J) =  DSAT1                            ! Valores de suprimento  
	DS2(J) =  DSAT2                            ! em hm3/intervalo.  
	DS3(J) =  DSAT3
	X(J) = X(J) ! mantem defluencia calculada com ou sem racionamento
C\-------->Fim do computo do trecho fluvial com controle de reservatorio
C=======================================================================
C     Vem de PC com e sem reservatorio
C     Verifica se ha' usina hidroeletrica
155   IF(CENER(J) .EQ. 0) GOTO 100
C     Existe usina: calcula a queda hidraulica e a energia gerada
	QUEHI = FINTER(J,QUEDA,VOLQU,KQV,SM)   
	ENERG(J) = ENER(J,SM,X(J),DSAT1,DSAT2,DSAT3,C1,QUEHI,CENER(J))

100   CONTINUE                                            

C     Quando HIERAR e' igual a HMAX acabaram os PC's
	IF(HIERAR .EQ. HMAX) GOTO 190
C     Verifica se foi encontrado algum PC com a hierarquia atual
	IF(KONTA .GT. 0) GOTO 189 ! Se positivo, va'`a proxima hierarquia     
C     Negativo: ha' um erro na atribuicao de hierarquias: para
      WRITE(*,*) 'Erro: nao existe PC com hierarquia ',HIERAR
      STOP
C     Continua propagacao com PC de hierarquia maior 
189   HIERAR = HIERAR + 1
	GOTO 2
C\------------------------------->Final do ciclo de simulacao sobre PC's 
C=======================================================================

C     Saida apos haver completado computacoes de um intervalo de tempo
C     Armazena variaveis de saida em m3/s (X, EV, DS1 e DS2) ou Hm3 (S)
190   DO 196 J = 1, N  
      DO 191 ID = 1,3
191   IFALHA(ID,J) = IBR              ! "zera" indicador de falhas
	XCONT(J) = XCONT(J) / C1        ! vazao afluente
	X(J)     = X(J)  / C1           ! vazao defluente, disponibilidade
	DS1(J)   = DS1(J)/ C1           ! suprimento demanda prioritaria
	DS2(J)   = DS2(J)/ C1           ! suprimento demanda secundaria
	DS3(J)   = DS3(J)/ C1           ! suprimento demanda terciaria                  
192   EV(J)    = EV(J) / C1           ! evaporacao do reservatorio

C     Identifica falhas e altera contadores: tolerancia: 1% 
	IF(D1(J).EQ.0.0 .OR. ABS(D1(J)-DS1(J)) .LE. 0.01*D1(J)) GOTO 193
	IFALHA(1,J) = IF1
	KPMF(J) = KPMF(J) + 1       ! falha: aumenta 1 contador mensal
	KIPF(J) = 1                 ! indicadores de falha
	KF = 1                      ! ativados                        
	IF(DS1(J) .GE. FCRIT/100.*D1(J)) GOTO 1935
	IFALHA(1,J) = IF2
      KPCR(J) = KPCR(J) + 1
      GOTO 1935
193   DS1(J) = D1(J)   ! Sem erro: arredonda DS1
1935  IF(D2(J).EQ.0.0 .OR. CABS(D2(J)-DS2(J)) .LE. 0.01*D2(J)) GOTO 195
	IFALHA(2,J) = IF1
	KSMF(J) = KSMF(J) + 1       ! falha: aumenta 1 contador mensal
	KISF(J) = 1                 ! indicadores de falha
	KF = 1                      ! ativados
	IF(DS2(J) .GE. FCRIT/100.*D2(J)) GOTO 1955
	IFALHA(2,J) = IF2
      KSCR(J) = KSCR(J) + 1
      GOTO 1955
195   DS2(J) = D2(J)   ! Sem erro: arredonda DS2
1955  IF(D3(J).EQ.0.0 .OR. ABS(D3(J)-DS3(J)) .LE. 0.01*D3(J)) GOTO 1957
      IFALHA(3,J) = IF1
	KTMF(J) = KTMF(J) + 1       ! falha: aumenta 1 contador mensal
	KITF(J) = 1                 ! indicadores de falha
	KF = 1                      ! ativados
      IF(DS3(J) .GE. FCRIT/100.*D3(J)) GOTO 196
      IFALHA(3,J) = IF2
      KTCR(J) = KTCR(J) + 1
      GOTO 196
1957  DS3(J) = D3(J)   ! Sem erro: arredonda DS3      
196   CONTINUE

C     Calcula performance operacional no intervalo de tempo como funcao
C     dos atendimentos as demandas secundaria, defluencia e armazenamen-
C     to final.

      FOBJ = OBJET(II,D1,DS1,ESC1,D2,DS2,ESC2,D3,DS3,ESC3,X,ENERG,
     +S,SMX,SMN,N)

C     Saida de resultados neste intervalo
	IF(IMPR .EQ. 2) GOTO 1952   
	WRITE(11,2221) IANO(K),II,FCRIT
2221  FORMAT('RESULTADOS DO ANO ',I4,' E INTERVALO ',I4,' [falha: "/";'
     +' falha critica= ',F4.1,' %: "\"]')
	IF(NRES .EQ. 0) GOTO 22212     
	IF(KF.EQ.1) GOTO 22233
C     Impressao com reservatorios
	WRITE(11,22231)
C     Saida sem falhas        
22231 FORMAT(' HQ PC',8X,'AFLUE',6X,'EV-PLU',3X,'ARMAZ',4X,'DEM(PRI)',
     +3X,'DEM(SEC)  DEM(TER)  DEFLU',4X,'ENERGIA'/13X,'(M3/S)',6X,
     +'(M3/S)',3X,'(HM3)',4X,'(M3/S)',5X,'(M3/S)',4X,'(M3/S)',4X,
     +'(M3/S)',3X,'(MW/INT)')
     
C/=====================================================================\     
      DO 6666 HIERAR =1,HMAX                                           !
      DO 6666 J=1,N                                                    !
	IF (H(J) .EQ. HIERAR) WRITE(11,22232)H(J),PCNOM(J),XCONT(J),EV(J)!
     +,S(J),D1(J),D2(J),D3(J),X(J),ENERG(J)                            !
22232 FORMAT(1X,I2,1X,A,2X,G9.4,1X,G9.3,6(1X,G9.4))                    !     
6666  CONTINUE                                                         !
      WRITE(11,22240) FOBJ                                             !
22240 FORMAT(' Diagnostico: sem falhas'/' Funcao-objetivo = '1PG14.5)  !
C\=====================================================================/
	GOTO 1954                                                        
C     Saida com falhas
22233 WRITE(11,22234)
22234 FORMAT(' HQ PC',8X,'AFLUE',6X,'EV-PLU',3X,'ARMAZ',4X,'DEM(PRI)',
     +2X,'SUP(PRI)',2X,'DEM(SEC)',2X,'SUP(SEC)',2X,'DEM(TER)',2X,
     +'SUP(TER)',2X,'DEFLU',4X,'ENERGIA'/14X,'(M3/S)',5X,'(M3/S)',3X,
     +'(HM3)',5X,'(M3/S)',4X,'(M3/S)',4X,'(M3/S)',4X,'(M3/S)',4X,
     +'(M3/S)',4X,'(M3/S)',3X,'(M3/S)',3X,'(MW/INT)')
C/=====================================================================\
      DO 7777 HIERAR=1,HMAX                                            !
      DO 7777 J=1,N                                                    !
      IF(H(J) .EQ. HIERAR) WRITE(11,22235)H(J),PCNOM(J),XCONT(J),EV(J),!
     .S(J),D1(J),IFALHA(1,J),DS1(J),D2(J),IFALHA(2,J),DS2(J),D3(J),    !
     +IFALHA(3,J),DS3(J),X(J),ENERG(J)                                 !
22235 FORMAT                                                           !
     +(1X,I2,1X,A,2X,G9.4,1X,G9.3,1X,G9.4,3(1X,G9.4,A,G9.4),2(1X,G9.4))!                                                      !
7777  CONTINUE                                                         !
      WRITE(11,22241) FOBJ                                             !
22241 FORMAT(' Diagnostico: com falhas!'/' Funcao-objetivo = ',1PG14.5)!
C\=====================================================================/
      GOTO 1954
C     Impressao sem reservatorios 
22212 IF(KF .EQ. 1) GOTO 32233
C     Saida sem falhas        
      WRITE(11,32231)
32231 FORMAT(' HQ PC',8X,'AFLUE',4X,'DEM(PRI)',3X,'DEM(SEC)  DEM(TER)  
     +DEFLU',4X,'ENERGIA'/13X,'(M3/S)',4X,'(M3/S)',5X,'(M3/S)',4X,
     +'(M3/S)',5X,'(M3/S)',2X,'(MW/INT)')
C/=====================================================================\
      DO 8888 HIERAR=1,HMAX                                            !
      DO 8888 J=1,N                                                    !
      IF(H(J).EQ.HIERAR)WRITE(11,32232)H(J),PCNOM(J),XCONT(J),         !
     .D1(J),D2(J),D3(J),X(J),ENERG(J)                                  !
32232 FORMAT(1X,I2,1X,A,1X,6(1X,G9.4))                                 !
8888  CONTINUE                                                         !
      WRITE(11,22240) FOBJ                                             !
C\=====================================================================/
	GOTO 1954
C     Saida com falhas
32233 WRITE(11,32234)
32234 FORMAT(' HQ PC',8X,'AFLUE',4X,'DEM(PRI)',2X,'SUP(PRI)',2X,
     +'DEM(SEC)',2X,'SUP(SEC)',2X,'DEM(TER)',2X,'SUP(TER)',2X,'DEFLU',
     +4X,'ENERGIA'/14X,'(M3/S)',4X,'(M3/S)',3X,'(M3/S)',4X,'(M3/S)',5X,
     +'(M3/S)',4X,'(M3/S)',3X,'(M3/S)',4X,'(M3/S)',3X,'(MW/INT)')
C/=====================================================================\
      DO 8889 HIERAR=1,HMAX                                            !
      DO 8889 J=1,N                                                    !
	IF(H(J).EQ.HIERAR)WRITE(11,32235)H(J),PCNOM(J),XCONT(J),D1(J),   !
     +IFALHA(1,J),DS1(J),D2(J),IFALHA(2,J),DS2(J),D3(J),IFALHA(3,J),   !
     +DS3(J),X(J),ENERG(J)                                             !
32235 FORMAT(1X,I2,1X,A,2X,G9.4,3(1X,G9.4,A,G9.4),2(1X,G9.4))          !
8889  CONTINUE                                                         !
      WRITE(11,22241) FOBJ                                             !
C\=====================================================================/
1954  IF(IMPR .EQ. 1) GOTO 1961       ! saida de 
1952  WRITE(12,1113) (X(J),J=1,N)     ! resultados
	WRITE(13,1113) (DS1(J),J=1,N)   ! para
	WRITE(14,1113) (DS2(J),J=1,N)   ! graficacao
	WRITE(15,1113) (DS3(J),J=1,N) 
	IF(NRES .GT. 0) WRITE(16,1113) (S(J),J=1,N)
	IF(NPCH .GT. 0) WRITE(17,1113) (ENERG(J),J=1,N) 

1961  DO 1971 J = 1,N                  
	IF(JRES(J) .EQ. 0) GOTO 1971
	S1(J) = S(J)
1971  CONTINUE
200   CONTINUE
C\---------->Final do ciclo de simulacao temporal : estacoes em cada ano
C=======================================================================

C     Computo de falha anual (falha em qualquer intervalo do ano)
	DO 2001 J = 1,N
	IF(KIPF(J) .EQ. 1) KPAF(J) = KPAF(J) + 1
	IF(KISF(J) .EQ. 1) KSAF(J) = KSAF(J) + 1
	IF(KITF(J) .EQ. 1) KTAF(J) = KTAF(J) + 1
2001  CONTINUE
201   CONTINUE
C\-------------------------->Final  do ciclo de simulacao temporal: anos    
C=======================================================================
                                                
C     Final da computacao: imprime estatistica de falhas
	WRITE(*,99998) FCRIT,FCRIT,FCRIT
	WRITE(11,99998)FCRIT,FCRIT,FCRIT
99998 FORMAT(/' RESULTADO FINAL: FALHAS DE ATENDIMENTO `AS DEMANDAS'/
     +' HQ  PC',8X,'DEMANDA PRIMARIA   DEMANDA SECUNDARIA   DEMANDA '
     +'TERCIARIA'/17X,'INT ',F3.0,'%  ANOS',6X,'INT ',F3.0,'%  ANOS',7X,
     +'INT ',F3.0,'%',2X,'ANOS')
	DO 99999 HIERAR = 1,HMAX
	DO 99999 J = 1,N
	IF(H(J) .NE. HIERAR) GOTO 99999
      WRITE( *,9999) H(J),PCNOM(J),KPMF(J),KPCR(J),KPAF(J),KSMF(J),
     +KSCR(J),KSAF(J),KTMF(J),KTCR(J),KTAF(J)
	WRITE(11,9999) H(J),PCNOM(J),KPMF(J),KPCR(J),KPAF(J),KSMF(J),
     +KSCR(J),KSAF(J),KTMF(J),KTCR(J),KTAF(J)
99999 CONTINUE    
	WRITE( *,9998) FOBJ
	WRITE(11,9998) FOBJ
9999  FORMAT(1X,I2,2X,A,2X,3(I4,1X),5X,3(I4,1X),6X,3(I4,1X))
9998  FORMAT(' Valor final funcao-objetivo nesta simulacao: ',1PG12.5)
      
      IF(IMPR.EQ.1) STOP ! Se arquivos graficos nao serao impressos

C/====Final da ordenacao: organiza arquivos graficos por PC============\
      REWIND(12)                                                       !
      DO 99981 I=1,M                                                   !
99981 READ(12,*) (Q(I,J),J=1,N)   ! Vazoes defluentes                  !
      REWIND(12)                                                       !
      DO 99991 J=1,N                                                   !
99991 WRITE(12,1114)PCNOM(J),(Q(I,J),I=1,M)                            !
      REWIND(13)                                                       !
      DO 99982 I=1,M                                                   !
99982 READ(13,*) (Q(I,J),J=1,N)   ! Suprimento a demandas prioritarias !
      REWIND(13)                                                       !
      DO 99992 J=1,N                                                   !
99992 WRITE(13,1114)PCNOM(J),(Q(I,J),I=1,M)                            !
      REWIND(14)                                                       !
      DO 99983 I=1,M                                                   !
99983 READ(14,*) (Q(I,J),J=1,N)   ! Suprimento a demandas secundarias  !
      REWIND(14)                                                       !
      DO 99993 J=1,N                                                   !
99993 WRITE(14,1114)PCNOM(J),(Q(I,J),I=1,M)                            !
      REWIND(15)                                                       !
      DO 99984 I=1,M                                                   !
99984 READ(15,*) (Q(I,J),J=1,N)   ! Suprimento a demandas terciarias   !
      REWIND(15)                                                       !
      DO 99994 J=1,N                                                   !
99994 WRITE(15,1114)PCNOM(J),(Q(I,J),I=1,M)                            !
                                                                       !
      IF(NRES.EQ.0) GOTO 99996                                         !
      REWIND(16)                                                       !
      DO 99985 I=1,M                                                   !
99985 READ(16,*) (Q(I,J),J=1,N)   ! Armazenamentos                     !
      REWIND(16)                                                       !
      DO 99995 J=1,N                                                   !
      IF(JRES(J).EQ.0) GOTO 99995                                      !
      WRITE(16,1114)PCNOM(J),(Q(I,J),I=1,M)                            !
99995 CONTINUE                                                         !
                                                                       !
99996 IF(NPCH.EQ.0) STOP                                               !
      REWIND(17)                                                       !
      DO 99986 I = 1,M                                                 !
99986 READ(17,*) (Q(I,J),J=1,N)   ! Energia gerada                     !
      REWIND(17)                                                       !
      DO 99997 J=1,N                                                   !
      IF(CENER(J).LT. 1.E-6) GOTO 99997                                !
      WRITE(17,1114)PCNOM(J),(Q(I,J),I=1,M)                            !
99997 CONTINUE                                                         !
1114  FORMAT(1X,1H',A,1H'/(12G10.4))                                   !
      close (17)                                                       !
      close (16)                                                       !
      close (15)                                                       !
      close (14)                                                       !
      close (13)                                                       !
      close (12)                                                       !
      close (11)                                                       !
      close (10)                                                       !
C\====Encerra programa=================================================/
	STOP 
	END
C----------------------------------------------------------------------+
	FUNCTION FINTER(J,Y,X,N,S)                                       !
C +--------------------------------------------------------------------+
C |   Calcula valor de funcao representada por pontos usando interpola-|
C |   cao linear                                                       |
C +--------------------------------------------------------------------+
C | Y(K,J): pontos funcao variavel dependente (H ou A) no PC J         |
C | X(K,J): pontos da funcao da variavel independente (volume) no PC J |
C | N(J):   numero de pontos da funcao respectiva no PC J              |
C | S: valor a ser interpolado (volume medio no reservatorio)          |
C | FINTER: valor calculado na funcao                                  |
C +--------------------------------------------------------------------+

	DIMENSION Y(10,50),X(10,50),N(50)
	NJM1 = N(J) - 1
	DO 1 K = 1,NJM1
	IF(S .GE. X(K,J) .AND. S .LE. X(K+1,J)) GOTO 2
1     CONTINUE
C     Nao encontrou insercao entre dois valores: extrapolacao
	NJ = N(J)
	FINTER = Y(NJM1,J) + (S-X(NJM1,J))*(Y(NJ,J)-Y(NJM1,J))
     +/ (X(NJ,J)-X(NJM1,J))
	RETURN
C     Encontrou insercao entre dois valores: interpolacao
2     FINTER = Y(K,J)+(S-X(K,J))*(Y(K+1,J)-Y(K,J))/(X(K+1,J)-X(K,J))
	RETURN
	END
C\---------------------------------------------------------------------/
      SUBROUTINE TOPOL(ORDEM,J,IK12,IK13,IK21,IK23,IK31,IK32)
C     Estabelece fatores de contribuicao IKxy, de acordo se retorno da 
C     demanda x pode ser usado pela demanda y (funcao da variavel ORDEM)
      INTEGER ORDEM
      IF(ORDEM .EQ. 000) GOTO 10      
      IF(ORDEM .EQ. 012) GOTO 11
      IF(ORDEM .EQ. 013) GOTO 12      
      IF(ORDEM .EQ. 023) GOTO 13
      IF(ORDEM .EQ. 021) GOTO 14
      IF(ORDEM .EQ. 031) GOTO 15
      IF(ORDEM .EQ. 032) GOTO 16
      IF(ORDEM .EQ. 123) GOTO 17
      IF(ORDEM .EQ. 132) GOTO 18
      IF(ORDEM .EQ. 213) GOTO 19
      IF(ORDEM .EQ. 231) GOTO 20
      IF(ORDEM .EQ. 312) GOTO 21
      IF(ORDEM .EQ. 321) GOTO 22
      WRITE(*,5) J
5     FORMAT(' ATENCAO: codigo atendimento espacial errado no PC ',I2) 
      STOP
10    IK12 = 0 ! Esquema espacial 0 - retornos nao podem ser usados
      IK13 = 0                           ! no mesmo PC
      IK21 = 0
      IK23 = 0
      IK31 = 0
      IK32 = 0
      RETURN
11    IK12 = 1 ! Esquema espacial 12 - retorno de demanda 1 pode ser
      IK13 = 0                            ! usado pela demanda 2
      IK21 = 0
      IK23 = 0
      IK31 = 0
      IK32 = 0
      RETURN
12    IK12 = 0 ! Esquema espacial 13 - retorno da demanda 1 pode ser 
      IK13 = 1                            ! usado pela demanda 3
      IK21 = 0
      IK23 = 0
      IK31 = 0
      IK32 = 0
      RETURN
13    IK12 = 0 ! Esquema espacial 21 - retorno da demanda 2 pode ser 
      IK13 = 0                            ! usado pela demanda 1
      IK21 = 1
      IK23 = 0
      IK31 = 0
      IK32 = 0
      RETURN
14    IK12 = 0 ! Esquema espacial tipo 23 - etc...
      IK13 = 0
      IK21 = 0
      IK23 = 1
      IK31 = 0
      IK32 = 0
      RETURN
15    IK12 = 0 ! Esquema espacial tipo 31 - etc...
      IK13 = 0
      IK21 = 0
      IK23 = 0
      IK31 = 1
      IK32 = 0
      RETURN
16    IK12 = 0 ! Esquema espacial tipo 32 - etc...
      IK13 = 0
      IK21 = 0
      IK23 = 0
      IK31 = 0
      IK32 = 1
      RETURN
17    IK12 = 1 ! Esquema espacial 123 - retorno da demanda 1 pode ser 
      IK13 = 1 ! usado pelas demadas 2 e 3; da demanda 2, pela 3
      IK21 = 0
      IK23 = 1
      IK31 = 0
      IK32 = 0
      RETURN
18    IK12 = 1 ! Esquema espacial tipo 132
      IK13 = 1
      IK21 = 0
      IK23 = 0
      IK31 = 0
      IK32 = 1
      RETURN
19    IK12 = 0 ! Esquema espacial tipo 213
      IK13 = 1
      IK21 = 1
      IK23 = 1
      IK31 = 0
      IK32 = 0
      RETURN
20    IK12 = 0 ! Esquema espacial tipo 231
      IK13 = 0
      IK21 = 1
      IK23 = 1
      IK31 = 1
      IK32 = 0
      RETURN
21    IK12 = 1 ! Esquema espacial tipo 312
      IK13 = 0
      IK21 = 0
      IK23 = 0
      IK31 = 1
      IK32 = 1
      RETURN
22    IK12 = 0 ! Esquema espacial tipo 321
      IK13 = 0
      IK21 = 1
      IK23 = 0
      IK31 = 1
      IK32 = 1
      RETURN
      END
C----------------------------------------------------------------------+
      SUBROUTINE RACIONA(X,DSAT1,RET1,DSAT2,RET2,DSAT3,RET3,           !      
     +IK12,IK13,IK21,IK23,IK31,IK32)                                   !
C----------------------------------------------------------------------+
C     Estabelece sistema de racionamento em PC qdo disponibilidade de  | 
C     agua e' insuficiente, de acordo com prioridades e espacializacao |
C     tomadas de agua                                                  |
C----------------------------------------------------------------------+
C     X: disponibilidade de agua total para o PC                       |
C     DSATx: demanda com prior. x planejada atendi/o por PLANEJA       |
C     RETx: fracao de retorno da demanda x                             |
C     IKxy: f. contribuicao: se 0, retorno demanda x nao pode ser usado|
C     por demanda y; se 1 pode (retorno de x esta' a montante de y)    | 
C----------------------------------------------------------------------+
      LOOP = 0                                                         !
      DR1 = DSAT1 ! primeira estimativa supri/o `a demanda prioritaria !
      DR2 = DSAT2 ! primeira estimativa supri/o `a demanda secundaria  !
      DR3 = DSAT3 ! primeira estimativa supri/o `a demanda terciaria   !
1     LOOP = LOOP + 1                                                  !
      IF(LOOP .GT. 10) goto 15 !Se nao convergiu apos 10 iteracoes, fim!
C     Demanda terciaria                                                !
      XJ3 = X - DR1 - DR2 + IK13*RET1*DR1 + IK23*RET2*DR2  ! Disponib. !
      DS3 = AMAX1(AMIN1(XJ3,DSAT3),0.)                     ! Suprimento!
C     Demanda secundaria                                               !
      XJ2 = X - DR1 - DS3 + IK12*RET1*DR1 + IK32*RET3*DS3  ! Disponib. !
      DS2 = AMAX1(AMIN1(XJ2,DSAT2),0.)                     ! Suprimento!
C     Demanda prioritaria                                              !
      XJ1 = X - DS2 - DS3 + IK21*RET2*DS2 + IK31*RET3*DS3  ! Disponib. !
      DS1 = AMAX1(AMIN1(XJ1,DSAT1),0.)                     ! Suprimento!
      E1 = ABS(DS1-DR1)                                                !
      E2 = ABS(DS2-DR2)                                                !
      E3 = ABS(DS3-DR3)                                                !
      IF(E1.LE. .01*DS1.AND.E2.LE. .01*DS2.AND.E3.LE. .01*DS3) GOTO 20 !
      DR1=DS1                                                          !
      DR2=DS2                                                          !
      DR3=DS3                                                          !
      GOTO 1                                                           !
15    write(*,*) ' Nao convergiu'                                      !
C     Convergiu! Estima defluencia deste PC                            !
20    X = X - (1-RET1)*DS1 - (1-RET2)*DS2 - (1-RET3)*DS3               !
      DSAT1 = DS1                                                      !
      DSAT2 = DS2                                                      !
      DSAT3 = DS3                                                      !
      RETURN                                                           !
      END                                                              !
C----------------------------------------------------------------------+
C\---------------------------------------------------------------------/      
C     SUBROTINAS USADAS PELO PROPAGAR - BACIA DO RIO PARACATU,MG

C/=====================================================================\
      SUBROUTINE PLANEJA (S,SMN,SMX,Q,IR,IM,C1,D1,DS1,RET1,D2,DS2,RET2,!
     .D3,DS3,RET3,XP,N,JRES,NRES,NT,T)                                 !
C +--------------------------------------------------------------------+
C |   Estrategia operacional : planeja a vazao defluente cursos de agua!
C +--------------------------------------------------------------------+
C |   IR:     entrada - intervalo temporal real                        |
C |   IM:     entrada - intervalo de tempo no ano (mes: 1 a 12, p. ex.)|
C |   Q(ir,j): afluencia ao PC j no intervalo ir (m3/s)                |
C |   S(j):   entrada - armazenamento no reservatorio do PC j (Hm3)    |
C |   SMN(j): entrada - armazenamento minimo..................(Hm3)    |
C |   SMX(j): entrada - armazenamento maximo..................(Hm3)    |
C |   Dx(j):  entrada - demanda prioridade x no PC j .........(m3/s)   |
C |   DSx(j): entrada - demanda prioridade x suprida no PC j  (m3/s)   |
C |   RETx(j):entrada - retorno demanda prioridade x (fracao)          |   
C |                                                                    |
C |   DS(J): saida-suprimento demanda prior. x planejada no PC j (m3/s)|   
C |   XP        : saida   - descarga defluente planejada res.j  (HM3/S)|
C |                                                             -------|
C |   N : numero de PC's                                               |
C +--------------------------------------------------------------------+
C     Dimensiona variaveis externas                                    !
	INTEGER T(4,50),JRES(50),NT(50)                                  !
	DIMENSION S(50),SMN(50),SMX(50),D1(50),DS1(50),RET1(50),         !
     +D2(50),DS2(50),RET2(50),D3(50),DS3(50),RET3(50),XP(50),Q(720,50) !
                                                                       !
C......................................................................!
C     Estrategia miope : atende demanda com minima descarga            !
      DO 1 J = 1, N                                                    !
      DS1(J) = D1(J)  ! suprimento prioritario = demanda               !
      DS2(J) = D2(J)  ! suprimento secundario = demanda                !
      DS3(J) = D3(J)  ! suprimento terciario = demanda                 !   
1     CONTINUE                                                         !
                                                                       !
C/---------------------------------------------------------------------\
C     Estrategia operacao Queimado: liberar demandas PC's 9 e 10       !
C     e tambem do PC12                                                 !
      DE9  = D1(9) + D2(9) + D3(9) - Q(IR,9)        ! Demanda          !
      IF(D3(9) .EQ. 0.0) DE9 = DE9 + RET2(9)*D2(9)  ! do PC09          !
      DE10 = (1.-RET2(9))*D2(9) + (1.-RET3(9))*D3(9)! Demanda do PC10  !
     .+ D1(10) + (1.-RET2(10))*D2(10) + D3(10) - (Q(IR,10) + Q(IR,9))  !
      IF(D3(10) .EQ. 0.0) DE10 = DE10 + RET2(10)*D2(10)                !  
      DE12 = D1(12) + D2(12) + D3(12) - D1(10) - D1(7) - Q(IR,12)      !
      DE12 = AMAX1(DE12,0.)                                            !        
      DE10 = AMAX1(DE9,DE10,0.) !Libera >entre demandas do PC09  PC10  !
      DE10 = DE10 + DE12        ! Acrescenta demanda esperada do PC12  !
      XP(8) = C1*DE10                                                  !
C\---------------------------------------------------------------------/
C/---------------------------------------------------------------------\
C     Estrategia operacao Paracatu: liberar demandas dos PC's 14 e 15  !
      DE14 = D1(14) + D2(14) + D3(14) - Q(IR,14)        ! Demanda      !
      IF(D3(14) .EQ. 0.0) DE14 = DE14 + RET2(14)*D2(14) ! do PC14      !
      DE15 = (1.-RET2(14))*D2(14) + (1.-RET3(14))*D3(14)! Demanda PC15 !
     .+ D1(15) + (1.-RET2(15))*D2(15) + D3(15) - (Q(IR,15) + Q(IR,14)) !
      if(D3(15) .EQ. 0.0) DE15 = DE15 + RET2(15)*D2(15)                !
      DE15 = AMAX1(DE14,DE15,0.)                    !Libera>entre as   ! 
      XP(13) = C1*DE15                              !demandas PC14 e 15!
C\---------------------------------------------------------------------/
C     Nota: o retorno da demanda de menor prioridade sendo suprida deve!
C     ser incluido como parte da liberacao de agua pois ele nao pode   !
C     ser usado pela mesma demanda. Esta e' a razao dos "ifs" acima.   !
C......................................................................!
      RETURN
	END
C\=====================================================================/
	SUBROUTINE OPERA (I,J,S,SMN,SMX,D1,DSAT1,RET1,D2,DSAT2,RET2,
     +D3,DSAT3,RET3,AFLU,XP1,C1,CENER,EFIRM,QUEHI)
C +--------------------------------------------------------------------+
C |   Tatica operacional : computa vazao defluente reservatorio base na!
C     regra operacional nela definida e operacao planejada em PLANEJA  !                                                      |
C +--------------------------------------------------------------------+
C |   Variaveis de entrada :                                           !
C |   I = intervalo de tempo                                           !
C |   J = PC                                                           !
C |   Unidades usadas : Hm3/intervalo de tempo adotado                 !
C |   S = armazenamento do reservatorio no inicio do intervalo de tempo!
C |   SMN,SMX = armazenamentos minimo e maximo do reservatorio         !
C |   DSAT1 = demanda prioritaria a ser satisfeita no intervalo tempo  !
C |   RET1 = fracao de retorno da demanda prioritaria                  !
C |   DSAT2 = demanda secundaria a ser satisfeita no intervalo de tempo!
C |   RET2 = fracao de retorno da demanda secundaria                   !
C |   DSAT3 = demanda terciaria a ser satisfeita no intervalo de tempo !
C |   RET3 = fracao de retorno da demanda terciaria                    !
C |   AFLU = afluencia ao reservatorio durante o intervalo de tempo    !
C |                                                                    !
C |   Variavel de saida :                                              !
C |                                                                    !
C |   DSAT1 = suprimento corrigido demanda prioritaria  ! (correcao    !
C |   DSAT2 = suprimento corrigido demanda secundaria   !  em PLANEJA) !
C |   DSAT3 = suprimento corrigido demanda terciaria    !              !
C |   XP1= descarga defluente adicional para atender demandas jusante  !
C +--------------------------------------------------------------------+
C |   Regra adotada : descarga defluente basica e' mantida; adicional  !
C |   atende condicoes de contorno dadas                               !
C +--------------------------------------------------------------------+
	DSAT1 = DSAT1
	DSAT2 = DSAT2
	DSAT3 = DSAT3
	XP1   = XP1     
c	IF(J .NE. 8) RETURN
c	XFIRME = C1*EFIRM/(CENER*QUEHI) ! Vazao necessaria p/energia firme
c	IF(XP1 .LT. XFIRME) XP1 = XFIRME! em Queimados e' liberada
	RETURN                                                         
	END
	
C----------------------------------------------------------------------+
	FUNCTION ENER(J,SM,X,D1,D2,D3,C1,QUEDA,CENER)                    !
C +--------------------------------------------------------------------+
C |   Calcula energia gerada na usina localizada no PC J               !
C +--------------------------------------------------------------------+
C |   Variaveis                                                        !
C |                                                                    !
C |   SM : armazenamento medio no intervalo no reservatorio            !
C |   X  : Defluencia (hm3/intervalo)                                  !
C |   D1 : Suprimento `a demanda prioritaria (hm3/intervalo)           !
C |   D1 : Suprimento `a demanda secundaria  (hm3/intervalo)           !
C |   D1 : Suprimento `a demanda terciaria   (hm3/intervalo)           !
C |   C1 : fator de transformacao m3/s em hm3/intervalo                !
C |   B  : coeficientes do polinomio QUEDA-volume do reservatorio      !
C |   C  : coeficiente de transformacao energetica                     !
C |   ENERG : energia gerada na usina do pc j (MW/intervalo)           !
C +--------------------------------------------------------------------+
	ENER = CENER * QUEDA * X/C1 ! supoem-se que toda 
	RETURN                      ! defluencia e' turbinada
	END

C----------------------------------------------------------------------+
	FUNCTION OBJET(II,D1,DS1,ESC1,D2,DS2,ESC2,D3,DS3,ESC3,X,ENERG,   !
     .S,SMX,SMN,N)                                                     !
C +--------------------------------------------------------------------+
C |   Calcula perfomance operacao no intervalo de tempo II, baseado no !
C |   atendimento demandas prioritaria, secundaria ou terciária, na    !
C |   defluencia ou no armazenamento final intervalo. A funcao de      !
C |   calculo e' estipulada pelo usuario, que devera' programar        !
C |   esta subroutine                                                  !
C +--------------------------------------------------------------------+
C |   Variaveis de entrada relacionadas a cada PC                      !
C |                                                                    !
C |   II       : Intervalo de tempo sazonal (ex: mes)                  |
C |   D1(j)    : Demanda prioritaria (m3/s)                            !
C |   DS1(j)   : Suprimento `a demanda prioritaria (m3/s)              !
C |   ESC1(j)  : escala de desenvolvimento da demanda prioritaria (ha) !
C |   D2(j)    : Demanda secundaria (m3/s)                             !
C |   DS2(j)   : Suprimento `a demanda secundaria (m3/s)               !
C |   ESC2(j)  : escala de desenvolvimento da demanda secundaria  (ha) !
C |   D3(j)    :  Demanda terciaria  (m3/s)                            !
C |   DS3(j)   : Suprimento `a demanda terciaria  (m3/s)               !
C |   ESC3(j)  : escala de desenvolvimento da demanda terciaria   (ha) !
C |   X(j)     : Defluencia (m3/s)                                     !
C |   ENERG(j) : energia gerada na usina do pc j (MW/intervalo)        !
C |   S(j)     : Armazenamento no reservatorio do PC j (Hm3)           !
C |   SMX(j)   : Armazenamento maximo do reservatorio no PC j (Hm3)    !
C |   SMN(j)   : Armazenamento minimo do reservatorio no PC j (Hm3)    !
C |   N        : Numero de PC's                                        !
C |   FOBJ     : Valor da funcao de performance (funcao-objetivo) (?)  !
C +--------------------------------------------------------------------+

      DIMENSION D1(50),DS1(50),ESC1(50),D2(50),DS2(50),ESC2(50),D3(50),
     +DS3(50),ESC3(50),X(50),ENERG(50),S(50),SMX(50),SMN(50)
      OBJET = 0.0
      RETURN
      END
C--------------------------------------------------------FIM DO PROGRAMA
C\=====================================================================/
