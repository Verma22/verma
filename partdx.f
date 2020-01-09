C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.5.0                                   C
C                                                                      C
C  MAIN PRORGRAM PARTDX                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C Contact address: IIS, The University of Tokyo, RSS21 project         C  
C                                                                      C
C Multi physics Flow Simulation System                                 C
C                                                                      C
C======================================================================C
      PROGRAM MAIN
      IMPLICIT NONE
      INCLUDE 'gf2.h'
C
      INTEGER*4 ME,MP,MPART,MBP,MEPT,MPPT,MDOM,MBPDOM
      INTEGER*4 N
      PARAMETER ( N = 8 )
C
      INTEGER*4 IREPRS,IRPPRS,IRLIQD,IRTEMP,IRDENS,IRTURK,IRTURE,
     *          IRVFRC,IRVIS
      DATA IREPRS /0/
      DATA IRPPRS /0/
      DATA IRLIQD /0/
      DATA IRTEMP /0/
      DATA IRDENS /0/
      DATA IRTURK /0/
      DATA IRTURE /0/
      DATA IRVFRC /0/
      DATA IRVIS  /0/
C
C ARRAYS HOLDING THE GLOBAL-DOMAIN DATA
C
C
      INTEGER*4, ALLOCATABLE::
     *           NODE(:,:),LTYPE(:),LEFRM(:),LISTP(:),LISTE(:)
      REAL*4,    ALLOCATABLE::
     *           X(:),Y(:),Z(:),
     *           U(:),V(:),W(:),P(:),PN(:),F(:),T(:),S(:),
     *           TURK(:),TURE(:),FE(:),VISCN(:),WRK(:)
C
C
C ARRAYS HOLDING A SUB-DOMAIN'S DATA
C
C
      INTEGER*4, ALLOCATABLE::
     *           LISTIP(:),LISTIE(:),
     *           NODEPT(:,:),LTYPT(:),LFRMPT(:)
      REAL*4,    ALLOCATABLE::
     *           XPT(:),YPT(:),ZPT(:),
     *           UPT(:),VPT(:),WPT(:),PPT(:),
     *           PNPT(:),FPT(:),TPT(:),SPT(:),
     *           TURKPT(:),TUREPT(:),FEPT(:),VISPT(:)
C
C
C ARRAYS HOLDING BOTH SUB-DOMAINS DATA AND GLOBAL-DOMAIN DATA
C
C
      INTEGER*4, ALLOCATABLE::
     *           LPINLT(:),LPMWAL(:),LPWALL(:),LPSYMT(:),
     *           LPCCL1(:),LPCCL2(:),LPBODY(:),LPFREE(:),
     *           LPINT1(:),LPINT2(:),LPINT3(:),
     *           LPSET1(:),LPSET2(:),LPSET3(:),
     *           LPTEMP(:),LEHSRC(:),LPHTRS(:),LPHFIX(:),
     *           LPFFO1(:),LPFFO2(:),LEFFO1(:),LEFFO2(:),
     *           LPDEP1(:),LPDEP2(:)
C
      REAL*4,    ALLOCATABLE::
     *           UINLT(:),VINLT(:),WINLT(:),
     *           UWALL(:),VWALL(:),WWALL(:),
     *           TEMP(:),HSRC(:),HTRS(:),HFIX(:)
C 
C
C WORK ARRAYS ASSIGNED TO THE GLOBAL-DOMAIN NODES/ELEMENTS
C
C
      INTEGER*4, ALLOCATABLE::
     *       JFGRID(:),JFNODE(:),JFVELO(:),JFPRES(:),JFINLT(:),
     *       JFMWAL(:),JFWALL(:),JFSYMT(:),JFCCL (:),JFBODY(:),
     *       JFDEP (:),
     *       JFFREE(:),JFTEMP(:),JFHFIX(:),JFHSRC(:),JFHTRS(:),
     *       JFFFOP(:),JFFFOE(:),IFFFOP(:),IFFFOE(:),JFSET (:)
C
      REAL*4,    ALLOCATABLE::
     *       UIN(:),VIN(:),WIN(:),UWL(:),VWL(:),WWL(:),
     *       TEMPG(:),HFIXG(:),HSRCG(:),HTRSG(:) 
C
C ARRAYS HOLDING BOTH SUB-DOMAIN INTER-CONNECTING DATA
C
C
      INTEGER*4, ALLOCATABLE::
     *           LDOM(:),NBPDOM(:),IPSLF(:,:),IPSND(:,:),
     *           LPWRK(:),LEWRK(:)
C
C
C FILES TO BE ACCESSED
C
C
      CHARACTER*60 FILEDD, FILEMS, FILEBC, FILEFF, FILEPT
C
C
      INTEGER*4 IUT0,IUT5,IUT6,IUTDD,IUTMS,IUTBC,IUTFF 
      DATA IUT0   /  0 /
      DATA IUT5   /  5 /
      DATA IUT6   /  6 /
C
      DATA IUTDD  /  2 /
      DATA IUTMS  /  8 /
      DATA IUTBC  /  9 /
      DATA IUTFF  / 10 /
C
      INTEGER*4 NP,NE,NPPT,NEPT,NPART,
     *          NPINLT,NPWALL,NPSYMT,NPMWAL,NPFREE,NPCCL ,
     *          NPBODY,NPINT ,NEFFO ,NPFFO ,NPDEP ,
     *          NPTEMP,NEHSRC,NPHFIX,NPHTRS,
     *          NPSET,
     *          NPPRS,NEPRS,NPLIQD,NPT3D,NPDENS,NPTURK,NPTURE,NEVFRC,
     *          NPVIS,
     *          NDOM,NBPMAX,
     *          NEMIN,NPMIN,NNMIN,NIMIN,NEMAX,NPMAX,
     *          NNMAX,NIMAX,NBMAX,NEAVR,NPAVR,NNAVR,NIAVR,NPIMAX,NPMMAX,
     *          NPWMAX,NPSMAX,NPFMAX,NPCCMX,NPBMAX,NPINTX,
     *          NEFFMX,NPFFMX,NPTMPX,NPHFXX,NEHSCX,NPHTRX,NPSETX
C
      INTEGER*4 JMESH,JBOUN,JFLOW,IERR,IERRB(20)
      DATA JMESH /0/
      DATA JBOUN /0/
      DATA JFLOW /0/
      DATA IERR  /0/
C
      INTEGER*4 IOFF,NOSAVE,JCHECK,IP,IE,IBP,
     *          NDUM,NEMESH,NPMESH,ISTEP,NPFLOW,IPRS,
     *          I,IPPT,IEPT,IDOM,IEF
      REAL*4    OFFSET,XG,YG,ZG,TIMEP,XGPT,YGPT,ZGPT,XOFF,YOFF,ZOFF
C
C      A GENERAL PURPOSE UTILITY PROGRAM FOR SUB-DIVIDING
C     A COMPUTATIONAL DOMAIN INTO SPECIFIED NUMBER OF SUB-DOMAINS
C
C                        VERSION 2003.06.09
C      (MODIFIED BY Y.YAMADE TO SUPPORT FREE BOUNDARY NODES)
C
C                        VERSION 2004.12.02
C      (MODIFIED BY Y.YAMADE TO TETRAHEDRAL ELEMENT)
C     NOTE THAT THIS VERSION SUPPORTS ONLY TETRAHEDRAL ELEMENT
C
C                        MODIFIED BY Y.YAMADE 2007. 4. 6
C      'GFFLOW' IS REPLACED BY 'GFFLW2' TO DEAL WITH VALABLES OF NODAL 
C      PRESSURE, LIQUID FRACTION, TEMPERATURE AND EDDY VISCOCITY. 
C
C                        MODIFIED BY Y.YAMADE 2007. 4.25
C      'GFBOU2' IS REPLACED BY 'GFBOU6' TO DEAL WITH BOUNDARY CONDITIONS
C      FOR HEAT TRANSFER COMPUTATION
C
C      THIS PROGRAM READS A DOMAIN DECOMPOSITION DESCRIPTION FILE
C     (DDD FILE), A GLOBAL DOMAIN MESH, BOUNDARY CONDITIONS, AND/OR 
C     FLOW FIELD DATA FILE(S), SUB-DIVIDE THEM, AND OUTPUTS SUB-DOMAIN
C     MESH, BOUNDARY CONDITIONS, AND/OR FLOW FIELD DATA FILES.
C
C      NOTES: A DDD FILE MUST ALWAYS BE SPECIFIED TO SUB-DIVIDE ANY OF
C            MESH, BOUNDRAY CONDITIONS, AND FLOW FIELD DATA FILE, WHILE
C            MESH, BOUNDARY CONDITIONS, AND FLOW FIELD DATA FILE 
C            MUST BE SPECIFIED ONLY FOR THOSE FILES TO BE SUB-DIVIDED.
C
C      NOTES: CURRENT VERSION DOES NOT SUPPORT THE FOLLOWING DATA
C            DECOMPOSITIONS WHEN THEY NEED INTER-SUBDOMAIN DATA
C            REFERENCES:
C
C                ELEMENT TYPE DATA                    ( LTYPE )
C                CYCLIC      BOUNDARY CONDITIONS DATA ( LPCCL )
C                DEPENDING   BOUNDARY CONDITIONS DATA ( LPDEP )
C
C             IF AN INTER-SUBDOMAIN DATA REFERENCE IS DETECTED,
C            THE EXECUTION WILL BE AUTOMATICALLY TERMINATED.
C                
C      NOTES: CURRENT VERSION DOES NOT CHECK FOR LOST BOUNDARY NODES.
C
C      NOTES: THIS PROGRAM SUPPORTS EASY CHECK OF THE DOMAIN
C            DECOMPOSITION BY POSITIONING EACH OF SUB-DOMAINS WITH
C            A SLIGHT OFFSET SPECIFIED BY THE USER. SPECIFY THE
C            OFFSET PARAMETER AS APPROPRIATE TO ENABLE THIS FUNCTION.
C        
C
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ** PARTD: SUB-DIVIDING A DOMAIN DATA FILE(S) **'
C
   10 CONTINUE
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' SPECIFY DOMAIN DECOMPOSITION DESCRIPTION FILE'
      READ (IUT5,'(A60)') FILEDD
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' SPECIFY FILENAME OF MESH DATA'
      READ (IUT5,'(A60)') FILEMS
      IF(FILEMS.NE.' ') JMESH = 1
C
      WRITE(IUT6,*) ' SPECIFY FILENAME OF BOUNDARY DATA'
      READ (IUT5,'(A60)') FILEBC
      IF(FILEBC.NE.' ') JBOUN = 1
C
      WRITE(IUT6,*) ' SPECIFY FILENAME OF FLOW FIELD DATA'
      READ (IUT5,'(A60)') FILEFF
      IF(FILEFF.NE.' ') JFLOW = 1
C
      WRITE(IUT6,*) ' SPECIFY OFFSET    MODE  : IOFF'
      WRITE(IUT6,*) '     0 --- NO OFFSET'
      WRITE(IUT6,*) '     1 --- OFFSET COORDINATES IN X-DIR.'
      WRITE(IUT6,*) '     2 --- OFFSET COORDINATES IN Y-DIR.'
      WRITE(IUT6,*) '     3 --- OFFSET COORDINATES IN Z-DIR.'
      WRITE(IUT6,*) '     4 --- OFFSET COORDINATES IN Y-DIR. AND Z-DIR.'
      WRITE(IUT6,*) '     5 --- OFFSET COORDINATES IN Z-DIR. AND X-DIR.'
      WRITE(IUT6,*) '     6 --- OFFSET COORDINATES IN X-DIR. AND Y-DIR.'
      WRITE(IUT6,*) '     7 --- OFFSET COORDINATES IN ALL DIRECTIONS'
      READ (IUT5,*) IOFF
      IF(IOFF.NE.0) THEN
          WRITE(IUT6,*) ' SPECIFY OFFSET PARAMETER : OFFSET'
          READ (IUT5,*) OFFSET
      ENDIF
C
      WRITE(IUT6,*) ' ENTER 1 TO DISABLE SAVING INTER-CONNECT BC'
      READ (IUT5,*) NOSAVE
C
C
C
C CONFIRM GIVEN PARAMETERS
C
C
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'DESCRIPTION FILE:', FILEDD
      IF(JMESH.EQ.1) WRITE(IUT6,*) 'MESH DATA   FILE:', FILEMS
      IF(JBOUN.EQ.1) WRITE(IUT6,*) 'BC   DATA   FILE:', FILEBC
      IF(JFLOW.EQ.1) WRITE(IUT6,*) 'FLOW DATA   FILE:', FILEFF
C
      IF(NOSAVE.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' !!! NOTE: INTER-CONNECT BC. DISABLED'
      ENDIF
C
      IF(IOFF.NE.0) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' !!! NOTE: OFFSET MODE SET =', IOFF
          WRITE(IUT6,*) '           OFFSET PARAMETER=', OFFSET
      ENDIF
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ENTER 1 IF ABOVE PARAMETERS ARE OK'
      READ (IUT5,*) JCHECK
      IF(JCHECK.NE.1) GO TO 10
C
C
C
C
C CHECK GLOBAL DOMAIN MESH DATA
C
C
C
      IF(JMESH.EQ.0) THEN
          WRITE(IUT0,*) 'MESH DATA MUST BE READ IN THIS VERSION.'
          STOP
      ELSE 
          CALL CHKMSH(FILEMS,IUTMS,IUT0,IUT6,MP,ME,IERR)
          IF(IERR.NE.0) GOTO 9500
      ENDIF
C
      CALL CHKDDD(FILEDD,IUTDD,IUT0,IUT6,
     *            MPART,MPPT,MEPT,MBP,IERR)
      IF(IERR.NE.0) GOTO 9500
C
C NOTE THAT ARRAYS OF BOUNDARY LIST IS USED FOR
C BOTH SUB-DOMAINS DATA AND GLOBAL-ODMAIN DATA.
C
      MBP=MP
C
C
C
C
C ALLOCATE
C
C
C
      WRITE(IUT6,*)
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'PARTDX: ALLOCATING VARIABLES... '
      ALLOCATE(       X(MP  ), STAT=LERR(01))
      ALLOCATE(       Y(MP  ), STAT=LERR(02))
      ALLOCATE(       Z(MP  ), STAT=LERR(03))
      ALLOCATE(  NODE(N,ME  ), STAT=LERR(04))
      ALLOCATE(   LTYPE(ME  ), STAT=LERR(05))
      ALLOCATE(   LEFRM(ME  ), STAT=LERR(06))
      ALLOCATE(       U(MP  ), STAT=LERR(07))
      ALLOCATE(       V(MP  ), STAT=LERR(08))
      ALLOCATE(       W(MP  ), STAT=LERR(09))
      ALLOCATE(       P(ME  ), STAT=LERR(10))
      ALLOCATE(      PN(MP  ), STAT=LERR(11))
      ALLOCATE(       F(MP  ), STAT=LERR(12))
      ALLOCATE(       T(MP  ), STAT=LERR(13))
      ALLOCATE(       S(MP  ), STAT=LERR(14))
      ALLOCATE(    TURK(MP  ), STAT=LERR(15))
      ALLOCATE(    TURE(MP  ), STAT=LERR(16))
      ALLOCATE(      FE(ME  ), STAT=LERR(17))
      ALLOCATE(   LISTP(MP  ), STAT=LERR(18))
      ALLOCATE(   LISTE(ME  ), STAT=LERR(19))
      ALLOCATE(  LISTIP(MPPT), STAT=LERR(20))
      ALLOCATE(  LISTIE(MEPT), STAT=LERR(21))
      ALLOCATE(     XPT(MPPT), STAT=LERR(22))
      ALLOCATE(     YPT(MPPT), STAT=LERR(23))
      ALLOCATE(     ZPT(MPPT), STAT=LERR(24))
      ALLOCATE(NODEPT(N,MEPT), STAT=LERR(25))
      ALLOCATE(   LTYPT(MEPT), STAT=LERR(26))
      ALLOCATE(  LFRMPT(MEPT), STAT=LERR(27))
      ALLOCATE(     UPT(MPPT), STAT=LERR(28))
      ALLOCATE(     VPT(MPPT), STAT=LERR(29))
      ALLOCATE(     WPT(MPPT), STAT=LERR(30))
      ALLOCATE(     PPT(MEPT), STAT=LERR(31))
      ALLOCATE(    PNPT(MPPT), STAT=LERR(32))
      ALLOCATE(     FPT(MPPT), STAT=LERR(33))
      ALLOCATE(     TPT(MPPT), STAT=LERR(34))
      ALLOCATE(     SPT(MPPT), STAT=LERR(35))
      ALLOCATE(  TURKPT(MPPT), STAT=LERR(36))
      ALLOCATE(  TUREPT(MPPT), STAT=LERR(37))
      ALLOCATE(    FEPT(MEPT), STAT=LERR(38))
      ALLOCATE(   LPINLT(MBP), STAT=LERR(39))
      ALLOCATE(   UINLT (MBP), STAT=LERR(40))
      ALLOCATE(   VINLT (MBP), STAT=LERR(41))
      ALLOCATE(   WINLT (MBP), STAT=LERR(42))
      ALLOCATE(   LPMWAL(MBP), STAT=LERR(43))
      ALLOCATE(   UWALL (MBP), STAT=LERR(44))
      ALLOCATE(   VWALL (MBP), STAT=LERR(45))
      ALLOCATE(   WWALL (MBP), STAT=LERR(46))
      ALLOCATE(   LPWALL(MBP), STAT=LERR(47))
      ALLOCATE(   LPSYMT(MBP), STAT=LERR(48))
      ALLOCATE(   LPCCL1(MBP), STAT=LERR(49))
      ALLOCATE(   LPCCL2(MBP), STAT=LERR(50))
      ALLOCATE(   LPINT1(MBP), STAT=LERR(51))
      ALLOCATE(   LPINT2(MBP), STAT=LERR(52))
      ALLOCATE(   LPINT3(MBP), STAT=LERR(53))
      ALLOCATE(   LPBODY(MBP), STAT=LERR(54))
      ALLOCATE(   LPFREE(MBP), STAT=LERR(55))
      ALLOCATE(   LPTEMP(MBP), STAT=LERR(56))
      ALLOCATE(     TEMP(MBP), STAT=LERR(57))
      ALLOCATE(   LEHSRC(MBP), STAT=LERR(58))
      ALLOCATE(     HSRC(MBP), STAT=LERR(59))
      ALLOCATE(   LPHTRS(MBP), STAT=LERR(60))
      ALLOCATE(     HTRS(MBP), STAT=LERR(61))
      ALLOCATE(   LPHFIX(MBP), STAT=LERR(62))
      ALLOCATE(     HFIX(MBP), STAT=LERR(63))
      ALLOCATE(   LPFFO1(MBP), STAT=LERR(64))
      ALLOCATE(   LPFFO2(MBP), STAT=LERR(65))
      ALLOCATE(   LEFFO1(MBP), STAT=LERR(66))
      ALLOCATE(   LEFFO2(MBP), STAT=LERR(67))
      ALLOCATE(   LPDEP1(MBP), STAT=LERR(68))
      ALLOCATE(   LPDEP2(MBP), STAT=LERR(69))
      ALLOCATE(   LPSET1(MBP), STAT=LERR(70))
      ALLOCATE(   LPSET2(MBP), STAT=LERR(71))
      ALLOCATE(   LPSET3(MBP), STAT=LERR(72))
      ALLOCATE(    JFGRID(MP), STAT=LERR(73))
      ALLOCATE(    JFNODE(ME), STAT=LERR(74))
      ALLOCATE(    JFVELO(MP), STAT=LERR(75))
      ALLOCATE(    JFPRES(ME), STAT=LERR(76))
      ALLOCATE(    JFINLT(MP), STAT=LERR(77))
      ALLOCATE(    JFMWAL(MP), STAT=LERR(78))
      ALLOCATE(    JFWALL(MP), STAT=LERR(79))
      ALLOCATE(    JFSYMT(MP), STAT=LERR(80))
      ALLOCATE(    JFCCL (MP), STAT=LERR(81))
      ALLOCATE(    JFBODY(MP), STAT=LERR(82))
      ALLOCATE(    JFDEP (MP), STAT=LERR(83))
      ALLOCATE(    JFFREE(MP), STAT=LERR(84))
      ALLOCATE(    JFTEMP(MP), STAT=LERR(85))
      ALLOCATE(    JFHFIX(MP), STAT=LERR(86))
      ALLOCATE(    JFHSRC(ME), STAT=LERR(87))
      ALLOCATE(    JFHTRS(MP), STAT=LERR(88))
      ALLOCATE(    JFFFOP(MP), STAT=LERR(89))
      ALLOCATE(    JFFFOE(ME), STAT=LERR(90))
      ALLOCATE(    IFFFOP(MP), STAT=LERR(91))
      ALLOCATE(    IFFFOE(ME), STAT=LERR(92))
      ALLOCATE(    JFSET (MP), STAT=LERR(93))
      ALLOCATE(       UIN(MP), STAT=LERR(94))
      ALLOCATE(       VIN(MP), STAT=LERR(95))
      ALLOCATE(       WIN(MP), STAT=LERR(96))
      ALLOCATE(       UWL(MP), STAT=LERR(97))
      ALLOCATE(       VWL(MP), STAT=LERR(98))
      ALLOCATE(       WWL(MP), STAT=LERR(99))
      ALLOCATE(     TEMPG(MP), STAT=LERR(100))
      ALLOCATE(     HFIXG(MP), STAT=LERR(101))
      ALLOCATE(     HSRCG(ME), STAT=LERR(102))
      ALLOCATE(     HTRSG(MP), STAT=LERR(103)) 
      ALLOCATE(   LDOM(MPART), STAT=LERR(104)) 
      ALLOCATE( NBPDOM(MPART), STAT=LERR(105)) 
      ALLOCATE(     LPWRK(MP), STAT=LERR(106)) 
      ALLOCATE(     LEWRK(ME), STAT=LERR(107)) 
      ALLOCATE(     WRK(MP  ), STAT=LERR(108))
      ALLOCATE(     VISCN(MP), STAT=LERR(109))
      ALLOCATE(   VISPT(MPPT), STAT=LERR(110))

      CALL CHKALC(110,LERR,IUT6,IERR) 
      IF(IERR.NE.0) GOTO 9400
      WRITE(IUT6,*) 'PARTDX: ALLOCATING FINISH       '
C
      CALL CHKDD2(MPART,MPPT,MEPT,MBP,
     *            FILEDD,IUTDD,IUT0,IUT6,
     *            MDOM,MBPDOM,
     *            LPWRK,LEWRK,
     *            LPINT1,LPINT2,LPINT3,
     *            LDOM,NBPDOM,IERR)
      IF(IERR.NE.0) GOTO 9500
C
      DEALLOCATE(  LPWRK) 
      DEALLOCATE(  LEWRK) 
C
      WRITE(IUT6,*)
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'PARTDX: ALLOCATING VARIABLES.2.  '
      ALLOCATE( IPSLF(MBPDOM,MDOM), STAT=LERR(1)) 
      ALLOCATE( IPSND(MBPDOM,MDOM), STAT=LERR(2)) 
      CALL CHKALC(2,LERR,IUT6,IERR) 
      IF(IERR.NE.0) GOTO 9400
      WRITE(IUT6,*) 'PARTDX: ALLOCATING-2 FINISH      '
C
C
C
C CLEAR FLAGS
C
C
C
      DO 100 IP = 1 , MP
          JFGRID(IP) = 0
          JFVELO(IP) = 0
C 
          JFINLT(IP) = 0
          JFMWAL(IP) = 0
          JFWALL(IP) = 0
          JFSYMT(IP) = 0
          JFFREE(IP) = 0
          JFCCL (IP) = 0
          JFBODY(IP) = 0
          JFDEP (IP) = 0
          JFFFOP(IP) = 0
          IFFFOP(IP) = 0
          JFTEMP(IP) = 0
          JFHFIX(IP) = 0
          JFHTRS(IP) = 0
          JFSET (IP) = 0
  100 CONTINUE
C
      DO 110 IE = 1 , ME
          JFNODE(IE) = 0
          JFPRES(IE) = 0
          JFHSRC(IE) = 0
          JFFFOE(IE) = 0
          IFFFOE(IE) = 0
  110 CONTINUE
C
C
C
C
C READ GLOBAL DOMAIN MESH DATA
C
C
C
      IF(JMESH.EQ.1) THEN
          IACT = 1
          CALL GFALL(IUT0,IUT6,IUTMS,FILEMS,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*GRID_3D *NODE_3D *ELM_ATR !',
     *               NAME,MP,NP,X,Y,Z,
     *               NAME,ME,N,NE,NDUM,NODE,
     *               NAME,ME,NE,LEFRM,
     *               ICHECK)     
          IF(IERR.NE.0) STOP
          NEMESH = NE
          NPMESH = NP
C
          XG = 0.E0
          YG = 0.E0
          ZG = 0.E0
          DO 120 IP = 1 , NP
              XG = XG+X(IP)
              YG = YG+Y(IP)
              ZG = ZG+Z(IP)
  120     CONTINUE
          XG = XG/FLOAT(NP)
          YG = YG/FLOAT(NP)
          ZG = ZG/FLOAT(NP)
      ENDIF
C
C
C
C READ  GLOBAL DOMAIN BOUNDARY CONDITIONS DATA
C
C
C
      IF(JBOUN.EQ.1) THEN
          IACT = 1
          CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           '*BC_INLT *BC_IV3D *BC_MWAL *BC_WV3D *BC_WALL
     *            *BC_SYMT *BC_FREE *BC_CYCL *BC_BODY *BC_INTR 
     *            *BC_FORC *BC_FOIN *BC_RELY
     *            *BC_TMPN *BC_TMPV *BC_HSRN *BC_HSRV
     *            *BC_HFXN *BC_HFXV *BC_HTRN *BC_HTRV  
     *            *BC_PSET !',
     *           NAME,MBP,NPINLT,LPINLT,
     *           NAME,MBP,NPINLT,UINLT,VINLT,WINLT,
     *           NAME,MBP,NPMWAL,LPMWAL,
     *           NAME,MBP,NPMWAL,UWALL,VWALL,WWALL,
     *           NAME,MBP,NPWALL,LPWALL,
     *           NAME,MBP,NPSYMT,LPSYMT,
     *           NAME,MBP,NPFREE,LPFREE,
     *           NAME,MBP,NPCCL ,LPCCL1,LPCCL2,
     *           NAME,MBP,NPBODY,LPBODY,
     *           NAME,MBP,NPINT, LPINT1,LPINT2,LPINT3,
     *           NAME,MBP,NEFFO, LEFFO1,LEFFO2,
     *           NAME,MBP,NPFFO, LPFFO1,LPFFO2,
     *           NAME,MBP,NPDEP, LPDEP1,LPDEP2,
     *           NAME,MBP,NPTEMP,LPTEMP,NAME,MBP,NPTEMP,TEMP,
     *           NAME,MBP,NEHSRC,LEHSRC,NAME,MBP,NEHSRC,HSRC,
     *           NAME,MBP,NPHFIX,LPHFIX,NAME,MBP,NPHFIX,HFIX,
     *           NAME,MBP,NPHTRS,LPHTRS,NAME,MBP,NPHTRS,HTRS,
     *           NAME,MBP,NPSET ,LPSET1,LPSET2,LPSET3,
     *           ICHECK)     
          IF(IERR.NE.0) STOP
          NPCCL=0
      ENDIF
C
C
C
C READ GLOBAL DOMAIN FLOW DATA
C
C
C
      IF(JFLOW.EQ.1) THEN
          IACT = 1
          CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*TIME_PS *STEP_PS 
     *                *VELO_3D *PRES_3E *PRES_3D 
     *                *LIQD_3D *TEMP_3D *DENS_3D
     *                *TURK_3D *TURE_3D *VFRC_3E 
     *                *DES_VIS !',
     *               NAME,TIMEP,
     *               NAME,ISTEP,
     *               NAME,MP,NPFLOW,U,V,W,
     *               NAME,ME,NEPRS,P,
     *               NAME,MP,NPPRS,PN,
     *               NAME,MP,NPLIQD,F,
     *               NAME,MP,NPT3D,T,
     *               NAME,MP,NPDENS,S,
     *               NAME,MP,NPTURK,TURK,
     *               NAME,MP,NPTURE,TURE,
     *               NAME,ME,NEVFRC,FE,
     *               NAME,MP,NPVIS ,VISCN,
     *               ICHECK)
          IF(IERR.NE.0) STOP
C
          IF(NPFLOW.EQ.0) GOTO 9000
          IF(NEPRS .EQ.0.AND.NPPRS.EQ.0) GOTO 9000
C
          IF(JMESH.EQ.1) THEN
              IF(NPMESH.NE.NPFLOW) GOTO 9000
          ELSE
             IF(NEPRS.NE.0) THEN
                NE = NEPRS
             ELSE
                NE = NEVFRC
             ENDIF
             NP = NPFLOW
          ENDIF
C
C
          IF(NPPRS.EQ.0) THEN
              DO 2000 IP=1,NP
                  PN (IP)=0.0E0
                  WRK(IP)=0.0E0
 2000         CONTINUE
C
              DO 2100 IE=1,NE
                  DO 2200 I=1,N
                      IP=NODE(I,IE)
                      IF(IP.EQ.0) GOTO 2200
                      WRK(IP)=WRK(IP)+1.0E0
                      PN (IP)=PN (IP)+P(IE)
 2200             CONTINUE   
 2100         CONTINUE   
C
              DO 2300 IP=1,NP
                  IF(WRK(IP).EQ. 0.0E0) GOTO 9000
                  PN(IP)=PN(IP)/WRK(IP)
 2300         CONTINUE
C
              NPPRS=NP
          ENDIF
C
          IF(NEPRS .NE.0) IREPRS=1
          IF(NPPRS .NE.0) IRPPRS=1
          IF(NPLIQD.NE.0) IRLIQD=1
          IF(NPT3D .NE.0) IRTEMP=1
          IF(NPDENS.NE.0) IRDENS=1
          IF(NPTURK.NE.0) IRTURK=1
          IF(NPTURE.NE.0) IRTURE=1
          IF(NEVFRC.NE.0) IRVFRC=1
          IF(NPVIS .NE.0) IRVIS =1
C
          IF(IREPRS.EQ.1 .AND. NEPRS .NE.NE) GOTO 9000
          IF(IRPPRS.EQ.1 .AND. NPPRS .NE.NP) GOTO 9000
          IF(IRLIQD.EQ.1 .AND. NPLIQD.NE.NP) GOTO 9000
          IF(IRTEMP.EQ.1 .AND. NPT3D .NE.NP) GOTO 9000
          IF(IRDENS.EQ.1 .AND. NPDENS.NE.NP) GOTO 9000
          IF(IRTURK.EQ.1 .AND. NPTURK.NE.NP) GOTO 9000
          IF(IRTURE.EQ.1 .AND. NPTURE.NE.NP) GOTO 9000
          IF(IRVFRC.EQ.1 .AND. NEVFRC.NE.NE) GOTO 9000
          IF(IRVIS .EQ.1 .AND. NPVIS .NE.NP) GOTO 9000
C
      ENDIF
C
C
C
C SET BOUNDARY CONDITIONS FLAGS
C
C
C
      IF(JBOUN.EQ.1) THEN
C
          DO 200 I=1,13
              IERRB(I)=0
  200     CONTINUE
C
          CALL SETBC3(NPINLT,LPINLT,UINLT,VINLT,WINLT,
     *                NP,JFINLT,UIN,VIN,WIN,IUT0,IERRB(01))
          CALL SETBC3(NPMWAL,LPMWAL,UWALL,VWALL,WWALL,
     *                NP,JFMWAL,UWL,VWL,WWL,IUT0,IERRB(02))
          CALL SETBC1(NPWALL,LPWALL,NP,JFWALL,IUT0,IERRB(03))
          CALL SETBC1(NPSYMT,LPSYMT,NP,JFSYMT,IUT0,IERRB(04))
          CALL SETBC1(NPFREE,LPFREE,NP,JFFREE,IUT0,IERRB(05))
          CALL SETBC1(NPBODY,LPBODY,NP,JFBODY,IUT0,IERRB(06))
          CALL SETBC5(NPDEP ,LPDEP1,LPDEP2,NP,JFDEP ,IUT0,IERRB(07))
          CALL SETBC5(NPSET ,LPSET1,LPSET2,NP,JFSET ,IUT0,IERRB(08))
C
          CALL SETBC4(NEFFO,LEFFO1,LEFFO2,
     *                NE,JFFFOE,IFFFOE,IUT0,IERRB(09))
          CALL SETBC4(NPFFO,LPFFO1,LPFFO2,
     *                NP,JFFFOP,IFFFOP,IUT0,IERRB(10))
C
          CALL SETBC2(NPTEMP,LPTEMP,TEMP,NP,JFTEMP,TEMPG,IUT0,IERRB(11))
          CALL SETBC2(NEHSRC,LEHSRC,HSRC,NE,JFHSRC,HSRCG,IUT0,IERRB(12))
          CALL SETBC2(NPHFIX,LPHFIX,HFIX,NP,JFHFIX,HFIXG,IUT0,IERRB(13))
          CALL SETBC2(NPHTRS,LPHTRS,HTRS,NP,JFHTRS,HTRSG,IUT0,IERRB(14))
C
          DO 210 I=1,14
              IF(IERRB(I).NE.0) GOTO 9100
  210     CONTINUE
C
          IF(NPCCL.NE.0) GOTO 9200
      ENDIF
C
C
C
C START SUB-DIVIDING GLOBAL-DOMAIN DATA
C
C
C
      NEMIN = NE
      NPMIN = NP
      NNMIN = NP
      NIMIN = NP
      NEMAX  = 0
      NPMAX  = 0
      NNMAX  = 0
      NIMAX  = 0
      NBMAX  = 0
      NEAVR  = 0
      NPAVR  = 0
      NNAVR  = 0
      NIAVR  = 0
C
      NPIMAX = 0
      NPMMAX = 0
      NPWMAX = 0
      NPSMAX = 0
      NPFMAX = 0
      NPCCMX = 0
      NPBMAX = 0
      NPINTX = 0
      NEFFMX = 0
      NPFFMX = 0
      NPTMPX = 0
      NPHFXX = 0
      NEHSCX = 0
      NPHTRX = 0
      NPSETX = 0
C
      IACT=3
      CALL GFALL(IUT0,IUT6,IUTDD,FILEDD,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)     
      IF(IERR.NE.0) STOP
C
      NPART = 0
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ** NOW START SUB-DIVIDING GLOBAL-DOMAIN DATA **'
C
  305 CONTINUE
C
C
C     LOOK FOR THE NEXT DOMAIN DECOMPOSITION DESCRIPTION DATA SET
C
C
          IACT = 5
          CALL GFALL(IUT0,IUT6,IUTDD,FILEDD,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*PT_NODE *PT_ELEM *BC_INTR !',
     *               NAME,MPPT,NPPT,LISTIP,
     *               NAME,MEPT,NEPT,LISTIE,
     *               NAME,MBP ,NPINT,LPINT1,LPINT2,LPINT3,
     *               ICHECK)
          IF(IERR.NE.0) STOP
          IF(IACT.EQ.7) GO TO 1000
C
          NPART = NPART+1
C
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** GENERATING SUB-DOMAIN: IPART =', NPART
C
C
C
C     INITIALIZE THE ARRAY 'NODEPT' BEFORE WE OVERWRITE IT 
C
C
C
          DO IE=1,MEPT
             DO I=1,N
                NODEPT(I,IE)=0
             END DO
             LFRMPT(IE)=0
          END DO   
C
C
C     CHECK DOMAIN DECOMPOSITION DESCRIPTION DATA
C
C
          DO 300 IPPT = 1 , NPPT
              IP = LISTIP(IPPT)
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER :MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JFLOW.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                  STOP
              ENDIF
  300     CONTINUE
C
          DO 310 IEPT = 1 , NEPT
              IE = LISTIE(IEPT)
              IF(IE.LT.1 .OR. IE.GT.ME) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER :ME; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JFLOW.EQ.1) .AND. IE.GT.NE
     *           .AND. IPRS.EQ.1) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND ELEM REFERED;STOP'
                  STOP
              ENDIF
  310     CONTINUE
C
          DO 320 IBP = 1 , NPINT
              IPPT = LPINT1(IBP)
              IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                  STOP
              ENDIF
  320     CONTINUE
C
C
C     CALCULATE STATISTICS
C
C
          CALL DDCOM0(LPINT1,LPINT2,LPINT3,NPINT,MDOM,MBPDOM,
     *                LDOM,NBPDOM,NDOM,IPSLF,IPSND,IUT0,IERR)
          IF(IERR.NE.0) STOP
C
          NBPMAX = 0
          DO 330 IDOM = 1 , NDOM
              IF(NBPDOM(IDOM) .GE. NBPMAX) NBPMAX = NBPDOM(IDOM)
  330     CONTINUE
C
          IF(NEPT  .LE.NEMIN) NEMIN = NEPT
          IF(NPPT  .LE.NPMIN) NPMIN = NPPT
          IF(NDOM  .LE.NNMIN) NNMIN = NDOM
          IF(NPINT .LE.NIMIN) NIMIN = NPINT
C
          IF(NEPT  .GE.NEMAX) NEMAX = NEPT
          IF(NPPT  .GE.NPMAX) NPMAX = NPPT
          IF(NDOM  .GE.NNMAX) NNMAX = NDOM
          IF(NPINT .GE.NIMAX) NIMAX = NPINT
          IF(NBPMAX.GE.NBMAX) NBMAX = NBPMAX
C
                             NEAVR = NEAVR+NEPT
                             NPAVR = NPAVR+NPPT
                             NNAVR = NNAVR+NDOM
                             NIAVR = NIAVR+NPINT
          WRITE(IUT6,*) 
     &    '   NE=',NEPT,'     NP=',NPPT,'     ND=',NDOM,'     NI=',NPINT
C
C
C     MAKE INVERSE REFERENCE LIST FOR GLOBAL NODES
C
C
          DO 400 IP = 1 , MP
              LISTP(IP) = 0
  400     CONTINUE
C
          DO 410 IE = 1 , ME
              LISTE(IE) = 0
  410     CONTINUE
C
          DO 420 IPPT = 1 , NPPT
              IP        = LISTIP(IPPT)
              LISTP(IP) = IPPT
  420     CONTINUE
C
          DO 430 IEPT = 1 , NEPT
              IE        = LISTIE(IEPT)
              LISTE(IE) = IEPT
  430     CONTINUE
C
C
C     MAKE SUB-DOMAIN MESH DATA
C
C
          IF(JMESH.EQ.1) THEN
              DO 500 IPPT = 1 , NPPT
                  IP         = LISTIP(IPPT)
                  XPT(IPPT)  = X(IP)
                  YPT(IPPT)  = Y(IP)
                  ZPT(IPPT)  = Z(IP)
                  JFGRID(IP) = NPART
  500         CONTINUE
C
              LTYPT(1) = 0
              DO 520 IEPT = 1 , NEPT
                  IE = LISTIE(IEPT)
C
                  LFRMPT(IEPT)=LEFRM(IE)
C
                  DO 510 I = 1 , N
                      IP = NODE(I,IE)
                      IF(IP.GE.1)THEN
C
                         IF(LISTP(IP).EQ.0) THEN
                            WRITE(IUT0,*) 
     &                       ' ## OUT-OF-DOMAIN NODE REFERED;STOP'
                            STOP
                         ENDIF
C
                         NODEPT(I,IEPT) = LISTP(IP)
                      END IF   
  510             CONTINUE
C
C                  IF(LTYPE(1).NE.0) THEN
C                      IEF = LTYPE(IE)
C                      IF(LISTE(IEF).EQ.0) THEN
C                          WRITE(IUT0,*) 
C     &                    ' ## OUT-OF-DOMAIN ELEMENT REFERED;STOP'
C                          STOP
C                      ENDIF
C                      LTYPT (IEPT) = LISTE(IEF)
C                  ENDIF
C
                  JFNODE(IE)   = NPART
  520         CONTINUE
          ENDIF
C
C
C     MAKE SUB-DOMAIN BOUNDARY CONDITIONS DATA
C
C
          IF(JBOUN.EQ.1) THEN
              NPINLT = 0
              NPMWAL = 0
              NPWALL = 0
              NPSYMT = 0
              NPFREE = 0
              NPBODY = 0
              NPFFO  = 0
              NEFFO  = 0
              NPDEP  = 0
              NPTEMP = 0
              NEHSRC = 0
              NPHFIX = 0
              NPHTRS = 0
              NPSET  = 0
C
              DO 600 IPPT = 1 , NPPT
                  IP = LISTIP(IPPT)
                  IF(JFINLT(IP).GT.0) THEN
                      NPINLT = NPINLT+1
                      LPINLT(NPINLT) = LISTP(IP)
                       UINLT(NPINLT) = UIN  (IP)
                       VINLT(NPINLT) = VIN  (IP)
                       WINLT(NPINLT) = WIN  (IP)
                  ENDIF
C
                  IF(JFMWAL(IP).GT.0) THEN
                      NPMWAL = NPMWAL+1
                      LPMWAL(NPMWAL) = LISTP(IP)
                       UWALL(NPMWAL) = UWL  (IP)
                       VWALL(NPMWAL) = VWL  (IP)
                       WWALL(NPMWAL) = WWL  (IP)
                  ENDIF
C
                  IF(JFWALL(IP).GT.0) THEN
                      NPWALL = NPWALL+1
                      LPWALL(NPWALL) = LISTP(IP)
                  ENDIF
C
                  IF(JFSYMT(IP).GT.0) THEN
                      NPSYMT = NPSYMT+1
                      LPSYMT(NPSYMT) = LISTP(IP)
                  ENDIF
C
                  IF(JFFREE(IP).GT.0) THEN
                      NPFREE = NPFREE+1
                      LPFREE(NPFREE) = LISTP(IP)
                  ENDIF
C
                  IF(JFBODY(IP).GT.0) THEN
                      NPBODY = NPBODY+1
                      LPBODY(NPBODY) = LISTP(IP)
                  ENDIF
C
                  IF(JFFFOP(IP).GT.0) THEN
                      NPFFO = NPFFO+1
                      LPFFO1(NPFFO) = LISTP(IP)
                      LPFFO2(NPFFO) = IFFFOP(IP)
                  ENDIF
C
                  IF(JFDEP (IP).GT.0) THEN
                      NPDEP  = NPDEP+1
                      LPDEP1(NPDEP) = LISTP(IP)
                      LPDEP2(NPDEP) = JFDEP(IP)
                  ENDIF
C
                  IF(JFTEMP(IP).GT.0) THEN
                      NPTEMP = NPTEMP+1
                      LPTEMP(NPTEMP) = LISTP(IP)
                        TEMP(NPTEMP) = TEMPG(IP)
                  ENDIF
C
                  IF(JFHFIX(IP).GT.0) THEN
                      NPHFIX = NPHFIX+1
                      LPHFIX(NPHFIX) = LISTP(IP)
                        HFIX(NPHFIX) = HFIX(IP)
                  ENDIF
C
                  IF(JFHTRS(IP).GT.0) THEN
                      NPHTRS = NPHTRS+1
                      LPHTRS(NPHTRS) = LISTP(IP)
                        HTRS(NPHTRS) = HTRSG(IP)
                  ENDIF
C
                  IF(JFSET(IP).GT.0) THEN
                      NPSET = NPSET+1
                      LPSET1(NPSET) = LISTP(IP)
                      LPSET2(NPSET) = JFSET(IP)
                      LPSET3(NPSET) = JFSET(IP)
                  ENDIF

  600         CONTINUE
C
              DO 610 IEPT = 1 , NEPT
                  IE = LISTIE(IEPT)
C
                  IF(JFFFOE(IE).GT.0) THEN
                      NEFFO = NEFFO+1
                      LEFFO1(NEFFO) = LISTE (IE)
                      LEFFO2(NEFFO) = IFFFOE(IE)
                  ENDIF
C
                  IF(JFHSRC(IE).GT.0) THEN
                      NEHSRC = NEHSRC+1
                      LEHSRC(NEHSRC) = LISTE(IE)
                        HSRC(NEHSRC) = HSRCG(IE)
                  ENDIF
  610         CONTINUE
C
              IF(NPINLT .GE. NPIMAX) NPIMAX = NPINLT
              IF(NPMWAL .GE. NPMMAX) NPMMAX = NPMWAL 
              IF(NPWALL .GE. NPWMAX) NPWMAX = NPWALL 
              IF(NPSYMT .GE. NPSMAX) NPSMAX = NPSYMT 
              IF(NPFREE .GE. NPFMAX) NPFMAX = NPFREE 
              IF(NPCCL  .GE. NPCCMX) NPCCMX = NPCCL
              IF(NPBODY .GE. NPBMAX) NPBMAX = NPBODY
              IF(NPINT  .GE. NPINTX) NPINTX = NPINT
              IF(NEFFO  .GE. NEFFMX) NEFFMX = NEFFO
              IF(NPFFO  .GE. NPFFMX) NPFFMX = NPFFO
              IF(NPTEMP .GE. NPTMPX) NPTMPX = NPTEMP
              IF(NEHSRC .GE. NEHSCX) NEHSCX = NEHSRC
              IF(NPHFIX .GE. NPHFXX) NPHFXX = NPHFIX
              IF(NPHTRS .GE. NPHTRX) NPHTRX = NPHTRS
              IF(NPSET  .GE. NPSETX) NPSETX = NPSET
          ENDIF
C
C
C     MAKE SUB-DOMAIN FLOW DATA
C
C
          IF(JFLOW.EQ.1) THEN
              DO 700 IPPT = 1 , NPPT
                  IP          = LISTIP(IPPT)
                  UPT   (IPPT) = U   (IP)
                  VPT   (IPPT) = V   (IP)
                  WPT   (IPPT) = W   (IP)
                  PNPT  (IPPT) = PN  (IP)
                  FPT   (IPPT) = F   (IP)
                  TPT   (IPPT) = T   (IP)
                  SPT   (IPPT) = S   (IP)
                  TURKPT(IPPT) = TURK(IP)
                  TUREPT(IPPT) = TURE(IP)
                  VISPT (IPPT) = VISCN(IP) 
                  JFVELO(IP)   = NPART
  700         CONTINUE
C
              DO 710 IEPT = 1 , NEPT
                  IE         = LISTIE(IEPT)
                  PPT(IEPT)  = P(IE)
                  FEPT(IEPT) = FE(IE)
                  JFPRES(IE) = NPART
  710         CONTINUE
          ENDIF
C
C
C     WRITE SUB-DOMAIN MESH DATA
C
C
          IF(JMESH.EQ.1) THEN
              CALL MFNAME(FILEMS,FILEPT,NPART,IUT0,IERR)
              IF(IERR.NE.0) STOP
C
              XGPT = 0.E0
              YGPT = 0.E0
              ZGPT = 0.E0
              DO 800 IPPT = 1 , NPPT
                  XGPT = XGPT+XPT(IPPT)
                  YGPT = YGPT+YPT(IPPT)
                  ZGPT = ZGPT+ZPT(IPPT)
  800         CONTINUE
              XGPT = XGPT/FLOAT(NPPT)
              YGPT = YGPT/FLOAT(NPPT)
              ZGPT = ZGPT/FLOAT(NPPT)
C
              XOFF = 0.E0
              YOFF = 0.E0
              ZOFF = 0.E0
              IF(IOFF.EQ.1 .OR. IOFF.EQ.5 .OR. IOFF.EQ.6 .OR. IOFF.EQ.7)
     &        XOFF = OFFSET*(XGPT-XG)
              IF(IOFF.EQ.2 .OR. IOFF.EQ.4 .OR. IOFF.EQ.6 .OR. IOFF.EQ.7)
     &        YOFF = OFFSET*(YGPT-YG)
              IF(IOFF.EQ.3 .OR. IOFF.EQ.4 .OR. IOFF.EQ.5 .OR. IOFF.EQ.7)
     &        ZOFF = OFFSET*(ZGPT-ZG)
C
              DO 810 IPPT = 1 , NPPT
                  XPT(IPPT) = XPT(IPPT)+XOFF
                  YPT(IPPT) = YPT(IPPT)+YOFF
                  ZPT(IPPT) = ZPT(IPPT)+ZOFF
  810         CONTINUE
C
              IACT = 2
              NDUM=8
              CALL GFALL(IUT0,IUT6,IUTMS,FILEPT,
     *                   MCOM,NCOMFL,COMFLE,
     *                   MCOM,NCOMST,COMSET,
     *                   IACT,IWRITE,INAME,IRESV,  
     *                   ICAST,IDATA0,IALL,ISKIP,IERR,
     *                   '*GRID_3D *NODE_3D *ELM_ATR !',
     *                   NAME,MPPT,NPPT,XPT,YPT,ZPT,
     *                   NAME,MEPT,N,NEPT,NDUM,NODEPT,
     *                   NAME,MEPT,  NEPT,     LFRMPT,
     *                   ICHECK)
              IF(IERR.NE.0) STOP
          ENDIF
C
C
C     WRITE SUB-DOMAIN BOUNDARY CONDITIONS DATA
C
C
          IF(JBOUN.EQ.1) THEN
              CALL MFNAME(FILEBC,FILEPT,NPART,IUT0,IERR)
              IF(IERR.NE.0) STOP
C
              IF(NOSAVE.EQ.1) NPINT = 0
C
              IACT = 2
              CALL GFALL(IUT0,IUT6,IUTBC,FILEPT,
     *                   MCOM,NCOMFL,COMFLE,
     *                   MCOM,NCOMST,COMSET,
     *                   IACT,IWRITE,INAME,IRESV,  
     *                   ICAST,IDATA0,IALL,ISKIP,IERR,
     *                   '*BC_INLT *BC_IV3D *BC_MWAL *BC_WV3D *BC_WALL
     *                    *BC_SYMT *BC_FREE *BC_CYCL *BC_BODY *BC_INTR 
     *                    *BC_FORC *BC_FOIN *BC_RELY
     *                    *BC_TMPN *BC_TMPV *BC_HSRN *BC_HSRV
     *                    *BC_HFXN *BC_HFXV *BC_HTRN *BC_HTRV  
     *                    *BC_PSET !',
     *                   NAME,MBP,NPINLT,LPINLT,
     *                   NAME,MBP,NPINLT,UINLT,VINLT,WINLT,
     *                   NAME,MBP,NPMWAL,LPMWAL,
     *                   NAME,MBP,NPMWAL,UWALL,VWALL,WWALL,
     *                   NAME,MBP,NPWALL,LPWALL,
     *                   NAME,MBP,NPSYMT,LPSYMT,
     *                   NAME,MBP,NPFREE,LPFREE,
     *                   NAME,MBP,NPCCL ,LPCCL1,LPCCL2,
     *                   NAME,MBP,NPBODY,LPBODY,
     *                   NAME,MBP,NPINT, LPINT1,LPINT2,LPINT3,
     *                   NAME,MBP,NEFFO, LEFFO1,LEFFO2,
     *                   NAME,MBP,NPFFO, LPFFO1,LPFFO2,
     *                   NAME,MBP,NPDEP, LPDEP1,LPDEP2,
     *                   NAME,MBP,NPTEMP,LPTEMP,NAME,MBP,NPTEMP,TEMP,
     *                   NAME,MBP,NEHSRC,LEHSRC,NAME,MBP,NEHSRC,HSRC,
     *                   NAME,MBP,NPHFIX,LPHFIX,NAME,MBP,NPHFIX,HFIX,
     *                   NAME,MBP,NPHTRS,LPHTRS,NAME,MBP,NPHTRS,HTRS,
     *                   NAME,MBP,NPSET ,LPSET1,LPSET2,LPSET3,
     *                   ICHECK)     
              IF(IERR.NE.0) STOP
          ENDIF
C
C
C     WRITE SUB-DOMAIN FLOW DATA
C
C
          IF(JFLOW.EQ.1) THEN
              CALL MFNAME(FILEFF,FILEPT,NPART,IUT0,IERR)
              IF(IERR.NE.0) STOP
C
              NEPRS  =NEPT*IREPRS
              NPPRS  =NPPT*IRPPRS
              NPLIQD =NPPT*IRLIQD
              NPT3D  =NPPT*IRTEMP
              NPDENS =NPPT*IRDENS
              NPTURK =NPPT*IRTURK
              NPTURE =NPPT*IRTURE
              NEVFRC =NEPT*IRVFRC
              NPVIS  =NPPT*IRVIS 
              IACT = 2
              CALL GFALL(IUT0,IUT6,IUTFF,FILEPT,
     *                   MCOM,NCOMFL,COMFLE,
     *                   MCOM,NCOMST,COMSET,
     *                   IACT,IWRITE,INAME,IRESV,  
     *                   ICAST,IDATA0,IALL,ISKIP,IERR,
     *                   '*TIME_PS *STEP_PS 
     *                    *VELO_3D *PRES_3E *PRES_3D 
     *                    *LIQD_3D *TEMP_3D *DENS_3D
     *                    *TURK_3D *TURE_3D *VFRC_3E 
     *                    *DES_VIS !',
     *                   NAME,TIMEP,
     *                   NAME,ISTEP,
     *                   NAME,MP,NPPT,UPT,VPT,WPT,
     *                   NAME,ME,NEPRS,PPT,
     *                   NAME,MP,NPPRS,PNPT,
     *                   NAME,MP,NPLIQD,FPT,
     *                   NAME,MP,NPT3D,TPT,
     *                   NAME,MP,NPDENS,SPT,
     *                   NAME,MP,NPTURK,TURKPT,
     *                   NAME,MP,NPTURE,TUREPT,
     *                   NAME,ME,NEVFRC,FEPT,
     *                   NAME,MP,NPVIS ,VISPT,
     *                   ICHECK)     
              IF(IERR.NE.0) STOP
          ENDIF
C
          GO TO 305
 1000 CONTINUE
C
      IACT = 7
      CALL GFALL(IUT0,IUT6,IUTDD,FILEDD,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' !',ICHECK)     
      IF(IERR.NE.0) STOP
C
C
C
C END DIVIDING INTO SUB-DOMAIN DATA FILES
C
C
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' !! SUB-DOMAIN DATA DECOMPOSITION COMPLETE !!'
C
C
C
C CHECK DATA DESCOMPOSION
C
C
C (1) MESH DATA
C
C
      IF(JMESH.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** CHEKING MESH DATA DECOMPOSITION **'
C
          DO 1100 IP = 1 , NP
              IF(JFGRID(IP).EQ.0) THEN
                  WRITE(IUT0,*) ' ## SOME GRIDS DATA LOST; STOP'
                  STOP
              ENDIF
 1100     CONTINUE
C
          DO 1110 IE = 1 , NE
              IF(JFNODE(IE).EQ.0) THEN
                  WRITE(IUT0,*) ' ## SOME NODES DATA LOST; STOP'
                  STOP
              ENDIF
 1110     CONTINUE
          WRITE(IUT6,*) ' DONE!'
      ENDIF
C
C
C (2) FLOW DATA
C
C
      IF(JFLOW.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** CHEKING FLOW DATA DECOMPOSITION **'
C
          DO 1200 IP = 1 , NP
              IF(JFVELO(IP).EQ.0) THEN
                  WRITE(IUT0,*) ' ## SOME VELOCITY DATA LOST; STOP'
                  STOP
              ENDIF
 1200     CONTINUE
C
          DO 1210 IE = 1 , NE
              IF(JFPRES(IE).EQ.0) THEN
                  WRITE(IUT0,*) ' ## SOME PRESSURE DATA LOST; STOP'
                  STOP
              ENDIF
 1210     CONTINUE
          WRITE(IUT6,*) ' DONE!'
      ENDIF
C
C
C
C WRITE DOMAIN-DECOMPOSITION STATISTICS
C
C
C
      NEAVR = NEAVR/FLOAT(NPART)
      NPAVR = NPAVR/FLOAT(NPART)
      NNAVR = NNAVR/FLOAT(NPART)
      NIAVR = NIAVR/FLOAT(NPART)
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ** DOMAIN-DECOMPOSITION STATISTICS **'
      WRITE(IUT6,*) ' ELEMENT: MIN=',NEMIN,'  MAX=',NEMAX,'  AVR=',NEAVR
      WRITE(IUT6,*) ' NODE   : MIN=',NPMIN,'  MAX=',NPMAX,'  AVR=',NPAVR
      WRITE(IUT6,*) ' DOMAIN : MIN=',NNMIN,'  MAX=',NNMAX,'  AVR=',NNAVR
      WRITE(IUT6,*) ' SURFACE: MIN=',NIMIN,'  MAX=',NIMAX,'  AVR=',NIAVR
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' NBPDOM : MAX=',NBMAX
C
      IF(JBOUN.EQ.1) THEN
          WRITE(IUT6,*) ' MAX. BOUNDARY NODES'
          WRITE(IUT6,*) '  NPINLT=', NPIMAX
          WRITE(IUT6,*) '  NPMWAL=', NPMMAX
          WRITE(IUT6,*) '  NPWALL=', NPWMAX
          WRITE(IUT6,*) '  NPSYMT=', NPSMAX
          WRITE(IUT6,*) '  NPFREE=', NPFMAX
          WRITE(IUT6,*) '  NPBODY=', NPBMAX
          WRITE(IUT6,*) '  NEFFO =', NEFFMX
          WRITE(IUT6,*) '  NPFFO =', NPFFMX
          WRITE(IUT6,*) '  NPTEMP=', NPTMPX
          WRITE(IUT6,*) '  NPHFIX=', NPHFXX
          WRITE(IUT6,*) '  NEHSRC=', NEHSCX
          WRITE(IUT6,*) '  NPHTRS=', NPHTRX
      ENDIF
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' PARTD: SUCCESSFULLY TERMINATED'
      WRITE(IUT6,*) '        NUMBER OF SUB-DOMAINS GENERATED =', NPART
C     
      STOP
C
 9000 CONTINUE
      WRITE(IUT0,*) ' ## FLOW DO NOT MATCH MESH DATA; STOP'
      STOP
 9100 CONTINUE
      WRITE(IUT0,*) ' ## ERROR IN BC. ',I
      STOP
 9200 CONTINUE
      WRITE(IUT0,*) ' ## CYCLIC B.C IS NOT SUPPORTED IN PARTDX'
      STOP
 9300 CONTINUE
      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
      STOP
 9400 CONTINUE
      WRITE(IUT6,*) 'ALLOCATION ERRORS     '
 9500 CONTINUE
      WRITE(IUT0,*) 'PARTDX: AN ERROR REPORTED!; STOP'    
      STOP
      END
C
      SUBROUTINE SETBC1(NPB,LPB,NP,JF,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 NPB,NP
      INTEGER*4 LPB(NPB),JF(NP)
      INTEGER*4 IUT0,IERR
      INTEGER*4 IBP,IP
C
      DO 1000 IP = 1 , NP
          JF(IP) = 0
 1000 CONTINUE
C
      DO 1100 IBP = 1 , NPB
         IP = LPB(IBP)
C
          IF(IP.LT.1 .OR. IP.GT.NP) THEN
              WRITE(IUT0,*) ' ## LIMIT OVER :MP; STOP'
              IERR=1
              RETURN
          ENDIF
C
          JF(IP) = 1
 1100 CONTINUE
C
      RETURN
      END   
C
      SUBROUTINE SETBC2(NPB,LPB,VALB,NP,JF,VALG,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 NPB,NP
      INTEGER*4 LPB(NPB),JF(NP)
      REAL*4    VALB(NPB),VALG(NP)
      INTEGER*4 IUT0,IERR
      INTEGER*4 IBP,IP
C
      DO 1000 IP = 1 , NP
          JF(IP) = 0
 1000 CONTINUE
C
      DO 1100 IBP = 1 , NPB
         IP = LPB(IBP)
C
          IF(IP.LT.1 .OR. IP.GT.NP) THEN
              WRITE(IUT0,*) ' ## LIMIT OVER :MP; STOP'
              IERR=1
              RETURN
          ENDIF
C
          VALG(IP)=VALB(IBP)
          JF  (IP)=1
 1100 CONTINUE
C
      RETURN
      END   
C
      SUBROUTINE SETBC3(NPB,LPB,VALB1,VALB2,VALB3,
     *                  NP,JF,VALG1,VALG2,VALG3,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 NPB,NP
      INTEGER*4 LPB(NPB),JF(NP)
      REAL*4    VALB1(NPB),VALB2(NPB),VALB3(NPB)
      REAL*4    VALG1(NP ),VALG2(NP ),VALG3(NP )
      INTEGER*4 IUT0,IERR
      INTEGER*4 IBP,IP
C
      DO 1000 IP = 1 , NP
          JF(IP) = 0
 1000 CONTINUE
C
      DO 1100 IBP = 1 , NPB
         IP = LPB(IBP)
C
          IF(IP.LT.1 .OR. IP.GT.NP) THEN
              WRITE(IUT0,*) ' ## LIMIT OVER :MP; STOP'
              IERR=1
              RETURN
          ENDIF
C
          VALG1(IP)=VALB1(IBP)
          VALG2(IP)=VALB2(IBP)
          VALG3(IP)=VALB3(IBP)
          JF   (IP)=1
 1100 CONTINUE
C
      RETURN
      END   
C
      SUBROUTINE SETBC4(NPB,LPB,IVALB,NP,JF,IVALG,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 NPB,NP
      INTEGER*4 LPB(NPB),JF(NP)
      INTEGER*4 IVALB(NPB),IVALG(NP)
      INTEGER*4 IUT0,IERR
      INTEGER*4 IBP,IP
C
      DO 1000 IP = 1 , NP
          JF(IP) = 0
 1000 CONTINUE
C
      DO 1100 IBP = 1 , NPB
         IP = LPB(IBP)
C
          IF(IP.LT.1 .OR. IP.GT.NP) THEN
              WRITE(IUT0,*) ' ## LIMIT OVER :MP; STOP'
              IERR=1
              RETURN
          ENDIF
C
          IVALG(IP)=IVALB(IBP)
          JF   (IP)=1
 1100 CONTINUE
C
      RETURN
      END   
C
      SUBROUTINE SETBC5(NPB,LPB1,LPB2,NP,JF,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 NPB,NP
      INTEGER*4 LPB1(NPB),LPB2(NPB),JF(NP)
      INTEGER*4 IUT0,IERR
      INTEGER*4 IBP,IP
C
      DO 1000 IP = 1 , NP
          JF(IP) = 0
 1000 CONTINUE
C
      DO 1100 IBP = 1 , NPB
         IP = LPB1(IBP)
C
          IF(IP.LT.1 .OR. IP.GT.NP) THEN
              WRITE(IUT0,*) ' ## LIMIT OVER :MP; STOP'
              IERR=1
              RETURN
          ENDIF
C
          JF(IP) = LPB2(IBP)
 1100 CONTINUE
C
      RETURN
      END   
