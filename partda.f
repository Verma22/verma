C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.6.1                                   C
C                                                                      C
C  MAIN PRORGRAM  PARTDA                                               C
C                                                                      C
C                                       WRITTEN BY RCCM INC.,          C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, RISS project               C  
C                                                                      C
C======================================================================C
      PROGRAM MAIN
      IMPLICIT REAL*4(A-H,O-Z)
      INCLUDE 'size.h'
      PARAMETER ( N    =       8,
     1            MBP  = 20000000, MEPT   =  50000000, MPPT = 50000000,
     2            MDOM =      64, MBPDOM =  200000)
C
C
C ARRAYS HOLDING THE GLOBAL-DOMAIN DATA
C
C
      COMMON / MESHGL /
     1       X(MP),Y(MP),Z(MP),NODE(N,ME),LTYPE(ME)
C
      COMMON / ACPRGL /
     1       AP(2,MP),POWER(MP),GRADP(6,MP)
C
      COMMON / ACSRGL /
     1       ASRCT(MP),ASRCF(2,MP)
C
      COMMON / LISTGL /
     1       LISTP(MP),LISTE(ME)
C
C
C ARRAYS HOLDING A SUB-DOMAIN'S DATA
C
C
      COMMON / LISTPT /
     1       LISTIP(MPPT),LISTIE(MEPT)
C
      COMMON / MESHPT /
     1       XPT(MPPT),YPT(MPPT),ZPT(MPPT),NODEPT(N,MEPT),LTYPT(MEPT)
C
      COMMON / ACPRPT /
     1       APPT(2,MPPT),PWRPT(MPPT),GRPPT(6,MPPT)
C
      COMMON / ACSRPT /
     1       ASTPT(MPPT),ASFPT(2,MPPT)
C
C
C ARRAYS HOLDING BOTH SUB-DOMAINS DATA AND GLOBAL-DOMAIN DATA
C
C
      COMMON / BOUN /
     1       LPINLT(MBP),LPFREE(MBP),LPSYMT(MBP),
     2       LPCCL1(MBP),LPCCL2(MBP),
     3       LPAWAL(MBP),LPANOR(MBP),
     4       LPAPRS(MBP),APRES (2,MBP),
     5       LPAIMP(MBP),AIMPE (2,MBP),
     6       LPAVEL(MBP),AVELO (6,MBP),
     7       LPAACC(MBP),AACCL (6,MBP),
     8       LPINT1(MBP),LPINT2(MBP),LPINT3(MBP)
C
C
C ARRAYS HOLDING BOTH SUB-DOMAIN INTER-CONNECTING DATA
C
C
      COMMON / INTER /
     1        LDOM(MDOM),NBPDOM(MDOM),
     2        IPSLF(MBPDOM,MDOM),IPSND(MBPDOM,MDOM)
C
C
C WORK ARRAYS ASSIGNED TO THE GLOBAL-DOMAIN NODES/ELEMENTS
C
C
      COMMON / FILLED /
     1       JFGRID(MP),JFNODE(ME),
     2       JFINLT(MP),JFFREE(MP),JFSYMT(MP),JFCCL (MP),
     3       JFAWAL(MP),JFANOR(MP),
     4       JFAPRS(MP),JFAIMP(MP),JFAVEL(MP),JFAACC(MP),
     5       JFASRT(MP),JFASRF(MP),
     6       JFACPR(MP),JFACPW(MP),JFACGR(MP)
C
      COMMON / VALUE /
     1       APRS(2,MP),AIMP(2,MP),AVEL(6,MP),AACC(6,MP)
C
C
C FILES TO BE ACCESSED
C
C
      CHARACTER*60 FILEDD, FILEMS, FILEBC, FILEAS, FILEAP, FILEPT
C
      PARAMETER ( MCOMFL = 10, MCOMST = 10 )
      CHARACTER*60 CFLEMS(MCOMFL),CFLEBC(MCOMFL)
      CHARACTER*60 CFLEAS(MCOMFL),CFLEAP(MCOMFL)
      CHARACTER*60 CSETMS(MCOMST),CSETBC(MCOMST)
      CHARACTER*60 CSETAS(MCOMST),CSETAP(MCOMST)
C
      PARAMETER ( MCOM = 10 )
      CHARACTER*60 COMFLE(MCOM),COMSET(MCOM)
C
      CHARACTER*30 NAME
C
#ifdef ERR7
      DATA IUT0   /  7 /
#else
      DATA IUT0   /  0 /
#endif
      DATA IUT5   /  5 /
      DATA IUT6   /  6 /
C
      DATA IUTDD  /  2 /
      DATA IUTMS  /  8 /
      DATA IUTBC  /  9 /
      DATA IUTAS  / 10 /
      DATA IUTAP  / 11 /
C
      DATA IDIM    / 3 /
#ifdef VOS
      DATA IWRITE  / 1 /
#else
      DATA IWRITE  / 2 /
#endif
      DATA INAME   / 1 /
C
      DATA  IRSRCT / 0 /
      DATA  IRSRCF / 0 /
      DATA  IRPOWR / 0 /
      DATA  IRGRDP / 0 /
C
C
C
C      A GENERAL PURPOSE UTILITY PROGRAM FOR SUB-DIVIDING
C     A COMPUTATIONAL DOMAIN INTO SPECIFIED NUMBER OF SUB-DOMAINS
C
C                        VERSION 2010. 6. 1
C
C       THIS PROGRAM READS A DOMAIN DECOMPOSITION DESCRIPTION FILE
C      (DDD FILE), A GLOBAL DOMAIN ACOUSTIC MESH, ACOUSTIC BOUNDARY
C      CONDITIONS, ACOUSTIC SOURCES, AND/OR ACOUSTIC FIELD DATA FILE(S),
C      SUB-DIVIDE THEM, AND OUTPUTS SUB-DOMAIN ACOUSTIC MESH, ACOUSTIC
C      BOUNDARY CONDITIONS, ACOUSTIC SOURCES, AND/OR ACOUSTIC FIELD DATA
C      FILES.
C
C       NOTES: A DDD FILE MUST ALWAYS BE SPECIFIED TO SUB-DIVIDE ANY OF
C             ACOUSTIC MESH, ACOUSTIC BOUNDARY CONDITIONS, ACOUSTIC
C             SOURCES, AND ACOUSTIC FIELD DATA FILE, WHILE ACOUSTIC
C             MESH, ACOUSTIC BOUNDARY CONDITIONS, ACOUSTIC SOURCES, AND
C             ACOUSTIC FIELD DATA FILE MUST BE SPECIFIED ONLY FOR THOSE
C             FILES TO BE SUB-DIVIDED.
C
C       NOTES: CURRENT VERSION DOES NOT SUPPORT THE FOLLOWING DATA
C             DECOMPOSITIONS WHEN THEY NEED INTER-SUBDOMAIN DATA
C             REFERENCES:
C
C                 ELEMENT TYPE DATA                    ( LTYPE )
C                 CYCLIC      BOUNDARY CONDITIONS DATA ( LPCCL )
C
C              IF AN INTER-SUBDOMAIN DATA REFERENCE IS DETECTED,
C             THE EXECUTION WILL BE AUTOMATICALLY TERMINATED.
C
C       NOTES: CURRENT VERSION DOES NOT CHECK FOR LOST BOUNDARY NODES.
C
C       NOTES: THIS PROGRAM SUPPORTS EASY CHECK OF THE DOMAIN
C             DECOMPOSITION BY POSITIONING EACH OF SUB-DOMAINS WITH
C             A SLIGHT OFFSET SPECIFIED BY THE USER. SPECIFY THE
C             OFFSET PARAMETER AS APPROPRIATE TO ENABLE THIS FUNCTION.
C
C       NOTES: CURRENT VERSION DOES NOT SUPPORT THE FOLLOWING DATA
C             DECOMPOSITIONS BECAUSE ACOSIC SOLVER DOES NOT SUPPORT
C             THEM:
C
C                 INLTET BOUNDARY CONDITIONS DATA ( LPINLT )
C                 FREE   BOUNDARY CONDITIONS DATA ( LPFREE )
C
C
C
      NDUM = 0
       DUM = 0.0E0
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ** PARTDA: SUB-DIVIDING A DOMAIN DATA FILE(S) **'
C
   10 CONTINUE
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' SPECIFY DOMAIN DECOMPOSITION DESCRIPTION FILE'
      READ (IUT5,'(A60)') FILEDD
C
      JMESH = 0
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' SPECIFY FILENAME OF ACOUSTIC MESH DATA'
      READ (IUT5,'(A60)') FILEMS
      IF(FILEMS.NE.' ') JMESH = 1
C
      JBOUN = 0
      WRITE(IUT6,*) ' SPECIFY FILENAME OF ACOUSTIC BOUDNARY DATA'
      READ (IUT5,'(A60)') FILEBC
      IF(FILEBC.NE.' ') JBOUN = 1
C
      JACSR = 0
      WRITE(IUT6,*) ' SPECIFY FILENAME OF ACOUSTIC SOURCE DATA'
      READ (IUT5,'(A60)') FILEAS
      IF(FILEAS.NE.' ') JACSR = 1
C
      JACPR = 0
C     WRITE(IUT6,*) ' SPECIFY FILENAME OF ACOUSTIC FIELD DATA'
C     READ (IUT5,'(A60)') FILEAP
C     IF(FILEAP.NE.' ') JACPR = 1
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
      IF(JMESH.EQ.1) WRITE(IUT6,*) 'ACOUSTIC MESH   DATA FILE:', FILEMS
      IF(JBOUN.EQ.1) WRITE(IUT6,*) 'ACOUSTIC BC     DATA FILE:', FILEBC
      IF(JACSR.EQ.1) WRITE(IUT6,*) 'ACOUSTIC SOURCE DATA FILE:', FILEAS
      IF(JACPR.EQ.1) WRITE(IUT6,*) 'ACOUSTIC FIELD  DATA FILE:', FILEAP
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
      IF(JCHECK.NE.1) GOTO 10
C
C
C
C CLEAR FLAGS
C
C
C
      DO 100 IP = 1 , MP
          JFGRID(IP) = 0
C
          JFINLT(IP) = 0
          JFFREE(IP) = 0
          JFSYMT(IP) = 0
          JFCCL (IP) = 0
          JFAWAL(IP) = 0
          JFANOR(IP) = 0
          JFAPRS(IP) = 0
          JFAIMP(IP) = 0
          JFAVEL(IP) = 0
          JFAACC(IP) = 0
C
          JFASRT(IP) = 0
          JFASRF(IP) = 0
C
          JFACPR(IP) = 0
          JFACPW(IP) = 0
          JFACGR(IP) = 0
  100 CONTINUE
C
      DO 110 IE = 1 , ME
          JFNODE(IE) = 0
  110 CONTINUE
C
C
C
C READ GLOBAL DOMAIN MESH DATA
C
C
C
      IF(JMESH.EQ.1) THEN
          IACT = 1
          CALL GFMESH(IUT0,IUT6,IUTMS,FILEMS,
     *                IACT,IWRITE,INAME,IDIM,IRESV,
     *                MCOMFL,NFLEMS,CFLEMS,
     *                MCOMST,NSETMS,CSETMS,
     *                NAME  ,MP,NP,X,Y,Z,
     *                NAME  ,ME,NE,N,NODE,
     *                NAME  ,      LTYPE,IERR)
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
C READ GLOBAL DOMAIN BOUNDARY CONDITIONS DATA
C
C
C
      NPINLT=0
      NPFREE=0
      NPSYMT=0
      NPCCL =0
      NPAWAL=0
      NPANOR=0
      NPAPRS=0
      NPAIMP=0
      NPAVEL=0
      NPAACC=0
      NPINT =0
C
      IF(JBOUN.EQ.1) THEN
          FREQ  = 0.E0
          NFREQ = 0
          IACT  = 1
          CALL GFBOUA(IUT0,IUT6,IUTBC,FILEBC,
     *                IACT,IWRITE,INAME,
     *                MCOMFL,NFLEBC,CFLEBC,
     *                MCOMST,NSETBC,CSETBC,
     *                NAME, FREQ,
     *                NAME,NFREQ,
     *                NAME,MBP,NPINLT,LPINLT,
     *                NAME,MBP,NPFREE,LPFREE,
     *                NAME,MBP,NPSYMT,LPSYMT,
     *                NAME,MBP,NPCCL ,LPCCL1,LPCCL2,
     *                NAME,MBP,NPAWAL,LPAWAL,
     *                NAME,MBP,NPANOR,LPANOR,
     *                NAME,MBP,NPAPRS,LPAPRS,
     *                NAME,           APRES,
     *                NAME,MBP,NPAIMP,LPAIMP,
     *                NAME,           AIMPE,
     *                NAME,MBP,NPAVEL,LPAVEL,
     *                NAME,           AVELO,
     *                NAME,MBP,NPAACC,LPAACC,
     *                NAME,           AACCL,
     *                NAME,MBP,NPINT,LPINT1,LPINT2,LPINT3,IERR)
          IF(IERR.NE.0) STOP
      ENDIF
C
C
C
C READ GLOBAL DOMAIN ACOUSTIC SOURCE DATA
C
C
C
      NPSRCT = 0
      NPSRCF = 0
C
      IF(JACSR.EQ.1) THEN
          TIME   = 0.E0
          NTIME  = 0
          FREQ   = 0.E0
          NFREQ  = 0
          IACT   = 1
          ITARGT = 2
C
          CALL GFASRC(IUT0,IUT6,IUTAS,FILEAS,
     *                IACT,IWRITE,INAME,ITARGT,
     *                MCOMFL,NFLEAS,CFLEAS,
     *                MCOMST,NSETAS,CSETAS,
     *                NAME, TIME,
     *                NAME,NTIME,
     *                NAME,MP,NPSRCT,ASRCT,
     *                NAME, FREQ,
     *                NAME,NFREQ,
     *                NAME,MP,NPSRCF,ASRCF,IERR)
          IF(IERR.NE.0) STOP
C
          IF(NPSRCT.NE.0) IRSRCT=1
          IF(NPSRCF.NE.0) IRSRCF=1
C
          IF(IRSRCT.EQ.1) THEN
              WRITE(IUT0,*) 'PARTDA: ERROR! (DATA INCONSISTENT); SOTP'
              STOP
          ENDIF
C
      ENDIF
C
C
C
C READ GLOBAL DOMAIN ACOUSTIC FIELD DATA
C
C
C
      IF(JACPR.EQ.1) THEN
          IACT = 1
C
          NP    =0
          NPDUM1=0
          NPDUM2=0
C
          CALL GFAPRS(IUT0,IUT6,IUTAP,FILEAP,
     *                IACT,IWRITE,INAME,
     *                MCOMFL,NFLEAP,CFLEAP,
     *                MCOMST,NSETAP,CSETAP,
     *                NAME, FREQ,
     *                NAME,NFREQ,
     *                NAME,MP,NP,AP,
     *                NAME,   NPDUM1,POWER,
     *                NAME,   NPDUM2,GRADP,IERR)
          IF(IERR.NE.0) STOP
          NPAPRS = NP
C
          IF(NPDUM1.NE.0) IRPOWR=1
          IF(NPDUM2.NE.0) IRGRDP=1
C
          IF(IRPOWR.EQ.1 .AND. NPDUM1.NE.NPAPRS) GOTO 9000
          IF(IRGRDP.EQ.1 .AND. NPDUM2.NE.NPAPRS) GOTO 9000
C
      ENDIF
C
C
C
C SET ACOUSTIC BOUNDARY CONDITIONS FLAGS
C
C
C
      IF(JBOUN.EQ.1) THEN
          DO 200 IPINLT = 1 , NPINLT
              IP = LPINLT(IPINLT)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPINLT):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              JFINLT(IP) = 1
  200     CONTINUE
C
          DO 210 IPFREE = 1 , NPFREE
              IP = LPFREE(IPFREE)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPFREE):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              JFFREE(IP) = 1
  210     CONTINUE
C
          DO 220 IPSYMT = 1 , NPSYMT
              IP = LPSYMT(IPSYMT)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPSYMT):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              JFSYMT(IP) = 1
  220     CONTINUE
C
          DO 230 IPCCL = 1 , NPCCL
              IP1 = LPCCL1(IPCCL)
              IP2 = LPCCL2(IPCCL)
C
              IF(IP1.LT.1.OR.IP1.GT.MP .OR. IP2.LT.1.OR.IP2.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPCCL):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND.
     &           (IP1.GT.NP .OR. IP2.GT.NP)) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED; STOP'
                  STOP
              ENDIF
C
              JFCCL(IP1) = IP2
  230     CONTINUE
C
          DO 240 IPAWAL = 1 , NPAWAL
              IP = LPAWAL(IPAWAL)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPAWAL):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              JFAWAL(IP) = 1
  240     CONTINUE
C
          DO 250 IPANOR = 1 , NPANOR
              IP = LPANOR(IPANOR)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPANOR); MP;STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              JFANOR(IP) = 1
  250     CONTINUE
C
          DO 260 IPAPRS = 1 , NPAPRS
              IP = LPAPRS(IPAPRS)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPAPRS):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              APRS(1,IP) = APRES(1,IPAPRS)
              APRS(2,IP) = APRES(2,IPAPRS)
              JFAPRS(IP) = 1
  260     CONTINUE
C
          DO 270 IPAIMP = 1 , NPAIMP
              IP = LPAIMP(IPAIMP)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPAIMP):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              AIMP(1,IP) = AIMPE(1,IPAIMP)
              AIMP(2,IP) = AIMPE(2,IPAIMP)
              JFAIMP(IP) = 1
  270     CONTINUE
C
          DO 280 IPAVEL = 1 , NPAVEL
              IP = LPAVEL(IPAVEL)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPAVEL):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              DO 281 IV = 1 , 6
                  AVEL(IV,IP) = AVELO(IV,IPAVEL)
  281         CONTINUE
              JFAVEL(IP) = 1
  280     CONTINUE
C
          DO 290 IPAACC = 1 , NPAACC
              IP = LPAACC(IPAACC)
C
              IF(IP.LT.1 .OR. IP.GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER (NPAACC):MP; STOP'
                  STOP
              ENDIF
              IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND.IP.GT.NP) THEN
                  WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
                  STOP
              ENDIF
C
              DO 291 IV = 1 , 6
                  AACC(IV,IP) = AACCL(IV,IPAACC)
  291         CONTINUE
              JFAACC(IP) = 1
  290     CONTINUE
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
C
      NEMAX = 0
      NPMAX = 0
      NNMAX = 0
      NIMAX = 0
C
      NBMAX = 0
C
      NEAVR = 0
      NPAVR = 0
      NNAVR = 0
      NIAVR = 0
C
      NPIMAX = 0
      NPFMAX = 0
      NPSMAX = 0
      NPCMAX = 0
      NPWMAX = 0
      NPNMAX = 0
      NPPMAX = 0
      NPMMAX = 0
      NPVMAX = 0
      NPAMAX = 0
C
      NPPWRX = 0
      NPGRDX = 0
C
      IACT = 3
      CALL GFPART(IUT0,IUT6,IUTDD,FILEDD,
     *            IACT,IWRITE,INAME,IDIM,IRESV,
     *            MCOM,NCOMFL,COMFLE,
     *            MCOM,NCOMST,COMSET,
     *            NAME,MPPT,NPPT ,LISTIP,
     *            NAME,MEPT,NEPT ,LISTIE,
     *            NAME,MBP ,NPINT,LPINT1,LPINT2,LPINT3,
     *            IERR)
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
      CALL GFPART(IUT0,IUT6,IUTDD,FILEDD,
     *            IACT,IWRITE,INAME,IDIM,IRESV,
     *            MCOM,NCOMFL,COMFLE,
     *            MCOM,NCOMST,COMSET,
     *            NAME,MPPT,NPPT ,LISTIP,
     *            NAME,MEPT,NEPT ,LISTIE,
     *            NAME,MBP ,NPINT,LPINT1,LPINT2,LPINT3,
     *            IERR)
      IF(IERR.NE.0) STOP
      IF(IACT.EQ.7) GO TO 1000
C
      NPART = NPART+1
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ** GENERATING SUB-DOMAIN: IPART =', NPART
C
C
C     CHECK DOMAIN DECOMPOSITION DESCRIPTION DATA
C
C
      DO 300 IPPT = 1 , NPPT
          IP = LISTIP(IPPT)
          IF(IP.LT.1 .OR. IP.GT.MP) THEN
              WRITE(IUT0,*) ' ## LIMIT OVER (NPPT):MP; STOP'
              STOP
          ENDIF
          IF((JMESH.EQ.1 .OR. JACPR.EQ.1) .AND. IP.GT.NP) THEN
              WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
              STOP
          ENDIF
  300 CONTINUE
C
      DO 310 IEPT = 1 , NEPT
          IE = LISTIE(IEPT)
          IF(IE.LT.1 .OR. IE.GT.ME) THEN
              WRITE(IUT0,*) ' ## LIMIT OVER (NEPT):ME; STOP'
              STOP
          ENDIF
          IF(JMESH.EQ.1 .AND. IE.GT.NE) THEN
              WRITE(IUT0,*) ' ## OUT-OF-BOUND ELEM REFFERED:STOP'
              STOP
          ENDIF
  310 CONTINUE
C
      DO 320 IPINT = 1 , NPINT
          IPPT = LPINT1(IPINT)
          IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
              WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFFERED;STOP'
              STOP
          ENDIF
  320 CONTINUE
C
C
C     CALCULATE STATISTICS
C
C
      CALL DDCOM0(LPINT1,LPINT2,LPINT3,NPINT,MDOM,MBPDOM,
     *            LDOM,NBPDOM,NDOM,IPSLF,IPSND,IUT0,IERR)
      IF(IERR.NE.0) STOP
C
      NBPMAX = 0
      DO 330 IDOM = 1 , NDOM
          NBPMAX = MAX(NBPMAX, NBPDOM(IDOM))
  330 CONTINUE
C
      NEMIN = MIN(NEMIN, NEPT)
      NPMIN = MIN(NPMIN, NPPT)
      NNMIN = MIN(NNMIN, NDOM)
      NIMIN = MIN(NIMIN, NPINT)
C
      NEMAX = MAX(NEMAX, NEPT)
      NPMAX = MAX(NPMAX, NPPT)
      NNMAX = MAX(NNMAX, NDOM)
      NIMAX = MAX(NIMAX, NPINT)
      NBMAX = MAX(NBMAX, NBPMAX)
C
      NEAVR = NEAVR+NEPT
      NPAVR = NPAVR+NPPT
      NNAVR = NNAVR+NDOM
      NIAVR = NIAVR+NPINT
C
      WRITE(IUT6,*)
     &'   NE=',NEPT,'     NP=',NPPT,'     ND=',NDOM,'     NI=',NPINT
C
C
C     MAKE INVERSE REFFERENCE LIST FOR GLOBAL NODES
C
C
      DO 400 IP = 1 , MP
          LISTP(IP) = 0
  400 CONTINUE
C
      DO 410 IE = 1 , ME
          LISTE(IE) = 0
  410 CONTINUE
C
      DO 420 IPPT = 1 , NPPT
          IP        = LISTIP(IPPT)
          LISTP(IP) = IPPT
  420 CONTINUE
C
      DO 430 IEPT = 1 , NEPT
          IE        = LISTIE(IEPT)
          LISTE(IE) = IEPT
  430 CONTINUE
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
  500     CONTINUE
C
          LTYPT(1) = 0
          DO 520 IEPT = 1 , NEPT
              IE = LISTIE(IEPT)
              DO 510 I = 1 , N
                  IP = NODE(I,IE)
C
                  IF(LISTP(IP).EQ.0) THEN
                      WRITE(IUT0,*)
     &                ' ## OUT-OF-DOMAIN NODE REFFERED;STOP'
                      WRITE(IUT6,*)'   LISTP(IP)=',LISTP(IP)
                      STOP
                  ENDIF
C
                  NODEPT(I,IEPT) = LISTP(IP)
  510         CONTINUE
C
              IF(LTYPE(1).NE.0) THEN
                  IEF = LTYPE(IE)
                  IF(LISTE(IEF).EQ.0) THEN
                      WRITE(IUT0,*)
     &                ' ## OUT-OF-DOMAIN ELEMENT REFFERED;STOP'
                      WRITE(IUT6,*)'   LISTE(IEF)=',LISTE(IEF)
                      STOP
                  ENDIF
                  LTYPT (IEPT) = LISTE(IEF)
              ENDIF
C
              JFNODE(IE)   = NPART
  520     CONTINUE
      ENDIF
C
C
C      MAKE SUB-DOMAIN BOUNDARY CONDITIONS DATA
C
C
      IF(JBOUN.EQ.1) THEN
          NPINLT = 0
          NPFREE = 0
          NPSYMT = 0
          NPCCL  = 0
          NPAWAL = 0
          NPANOR = 0
          NPAPRS = 0
          NPAIMP = 0
          NPAVEL = 0
          NPAACC = 0
          DO 600 IPPT = 1 , NPPT
              IP = LISTIP(IPPT)
              IF(JFINLT(IP).GT.0) THEN
                  NPINLT = NPINLT+1
                  LPINLT(NPINLT) = LISTP(IP)
              ENDIF
C
              IF(JFFREE(IP).GT.0) THEN
                  NPFREE = NPFREE+1
                  LPFREE(NPFREE) = LISTP(IP)
               ENDIF
C
              IF(JFSYMT(IP).GT.0) THEN
                  NPSYMT = NPSYMT+1
                  LPSYMT(NPSYMT) = LISTP(IP)
              ENDIF
C
              IF(JFCCL(IP).GT.0) THEN
                  NPCCL = NPCCL+1
C
                  IF(LISTP(JFCCL(IP)).EQ.0) THEN
                      WRITE(IUT0,*)
     &                 ' ## OUT-OF-DOMAIN NODE REFFERED;STOP'
                        STOP
                  ENDIF
C
                  LPCCL1(NPCCL) = LISTP      (IP)
                  LPCCL2(NPCCL) = LISTP(JFCCL(IP))
              ENDIF
C
              IF(JFAWAL(IP).GT.0) THEN
                  NPAWAL = NPAWAL+1
                  LPAWAL(NPAWAL) = LISTP(IP)
              ENDIF
C
              IF(JFANOR(IP).GT.0) THEN
                  NPANOR = NPANOR+1
                  LPANOR(NPANOR) = LISTP(IP)
              ENDIF
C
              IF(JFAPRS(IP).GT.0) THEN
                  NPAPRS = NPAPRS+1
                  LPAPRS(  NPAPRS) = LISTP(  IP)
                  APRES (1,NPAPRS) = APRS (1,IP)
                  APRES (2,NPAPRS) = APRS (2,IP)
              ENDIF
C
              IF(JFAIMP(IP).GT.0) THEN
                  NPAIMP = NPAIMP+1
                  LPAIMP(  NPAIMP) = LISTP(  IP)
                  AIMPE (1,NPAIMP) = AIMP (1,IP)
                  AIMPE (2,NPAIMP) = AIMP (2,IP)
              ENDIF
C
              IF(JFAVEL(IP).GT.0) THEN
                  NPAVEL = NPAVEL+1
                  LPAVEL(  NPAVEL) = LISTP(IP)
                  DO 601 I = 1 , 6
                      AVELO(I,NPAVEL) = AVEL(I,IP)
  601             CONTINUE
              ENDIF
C
              IF(JFAACC(IP).GT.0) THEN
                  NPAACC = NPAACC+1
                  LPAACC(  NPAACC) = LISTP(IP)
                  DO 602 I = 1 , 6
                      AACCL(I,NPAACC) = AACC(I,IP)
  602             CONTINUE
              ENDIF
  600     CONTINUE
C
          NPIMAX = MAX(NPIMAX, NPINLT)
          NPFMAX = MAX(NPFMAX, NPFREE)
          NPSMAX = MAX(NPSMAX, NPSYMT)
          NPCMAX = MAX(NPCMAX, NPCCL )
          NPWMAX = MAX(NPWMAX, NPAWAL)
          NPNMAX = MAX(NPNMAX, NPANOR)
C
          NPPMAX = MAX(NPPMAX, NPAPRS)
          NPMMAX = MAX(NPMMAX, NPAIMP)
          NPVMAX = MAX(NPVMAX, NPAVEL)
          NPAMAX = MAX(NPAMAX, NPAACC)
      ENDIF
C
C
C     MAKE SUB-DOMAIN ACOUSTIC SOURCE DATA
C
C
      IF(JACSR.EQ.1) THEN
          DO 700 IPPT = 1 , NPPT
              IP            = LISTIP( IPPT)
              ASTPT(  IPPT) = ASRCT(  IP)
              ASFPT(1,IPPT) = ASRCF(1,IP)
              ASFPT(2,IPPT) = ASRCF(2,IP)
C
              JFASRT(IP)    = NPART
              JFASRF(IP)    = NPART
  700     CONTINUE
      ENDIF
C
C
C     MAKE SUB-DOMAIN ACOUSTIC FIELD DATA
C
C
      IF(JACPR.EQ.1) THEN
          DO 750 IPPT = 1 , NPPT
              IP            = LISTIP(IPPT)
              APPT (1,IPPT) = AP    (1,IP)
              APPT (2,IPPT) = AP    (2,IP)
              PWRPT(  IPPT) = POWER (  IP)
              GRPPT(1,IPPT) = GRADP (1,IP)
              GRPPT(2,IPPT) = GRADP (2,IP)
              GRPPT(3,IPPT) = GRADP (3,IP)
              GRPPT(4,IPPT) = GRADP (4,IP)
              GRPPT(5,IPPT) = GRADP (5,IP)
              GRPPT(6,IPPT) = GRADP (6,IP)
C
              JFACPR(IP)    = NPART
  750     CONTINUE
C
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
  800     CONTINUE
          XGPT = XGPT/FLOAT(NPPT)
          YGPT = YGPT/FLOAT(NPPT)
          ZGPT = ZGPT/FLOAT(NPPT)
C
          XOFF = 0.E0
          YOFF = 0.E0
          ZOFF = 0.E0
          IF(IOFF.EQ.1 .OR. IOFF.EQ.5 .OR. IOFF.EQ.6 .OR. IOFF.EQ.7)
     &    XOFF = OFFSET*(XGPT-XG)
          IF(IOFF.EQ.2 .OR. IOFF.EQ.4 .OR. IOFF.EQ.6 .OR. IOFF.EQ.7)
     &    YOFF = OFFSET*(YGPT-YG)
          IF(IOFF.EQ.3 .OR. IOFF.EQ.4 .OR. IOFF.EQ.5 .OR. IOFF.EQ.7)
     &    ZOFF = OFFSET*(ZGPT-ZG)
C
          DO 810 IPPT = 1 , NPPT
              XPT(IPPT) = XPT(IPPT)+XOFF
              YPT(IPPT) = YPT(IPPT)+YOFF
              ZPT(IPPT) = ZPT(IPPT)+ZOFF
  810     CONTINUE
C
          IACT = 2
C
          NCOMFL=0
          NCOMST=0
C
          CALL GFMESH(IUT0,IUT6,IUTMS,FILEPT,
     *                IACT,IWRITE,INAME,IDIM,IRESV,
     *                MCOMFL,NCOMFL,CFLEMS,
     *                MCOMST,NCOMST,CSETMS,
     *                NAME  ,MPPT,NPPT,XPT,YPT,ZPT,
     *                NAME  ,MEPT,NEPT,N,NODEPT,
     *                NAME  ,            LTYPT,IERR)
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
C          NPINT = 0
          IF(NOSAVE.EQ.1) NPINT = 0
C
          IACT = 2
          NCOMFL=0
          NCOMST=0
          CALL GFBOUA(IUT0,IUT6,IUTBC,FILEPT,
     *                IACT,IWRITE,INAME,
     *                MCOMFL,NCOMFL,CFLEBC,
     *                MCOMST,NCOMST,CSETBC,
     *                NAME, FREQ,
     *                NAME,NFREQ,
     *                NAME,MBP,NPINLT,LPINLT,
     *                NAME,MBP,NPFREE,LPFREE,
     *                NAME,MBP,NPSYMT,LPSYMT,
     *                NAME,MBP,NPCCL ,LPCCL1,LPCCL2,
     *                NAME,MBP,NPAWAL,LPAWAL,
     *                NAME,MBP,NPANOR,LPANOR,
     *                NAME,MBP,NPAPRS,LPAPRS,
     *                NAME,           APRES,
     *                NAME,MBP,NPAIMP,LPAIMP,
     *                NAME,           AIMPE,
     *                NAME,MBP,NPAVEL,LPAVEL,
     *                NAME,           AVELO,
     *                NAME,MBP,NPAACC,LPAACC,
     *                NAME,           AACCL,
     *                NAME,MBP,NPINT,LPINT1,LPINT2,LPINT3,IERR)
          IF(IERR.NE.0) STOP
      ENDIF
C
C
C     WRITE SUB-DOMAIN ACOUSTIC SOURCE DATA
C
C
      IF(JACSR.EQ.1) THEN
          CALL MFNAME(FILEAS,FILEPT,NPART,IUT0,IERR)
          IF(IERR.NE.0) STOP
C
          NPSRCT=NPPT*IRSRCT
          NPSRCF=NPPT*IRSRCF
          ITARGT=2
C          IF(NPSRCT.GT.0) ITARGT = 1
C
          IACT   = 2
          CALL GFASRC(IUT0,IUT6,IUTAS,FILEPT,
     *                IACT ,IWRITE,INAME,ITARGT,
     *                MCOMFL,NFLEAS,CFLEAS,
     *                MCOMST,NSETAS,CSETAS,
     *                NAME, TIME,
     *                NAME,NTIME,
     *                NAME,MP,NPSRCT,ASTPT,
     *                NAME, FREQ,
     *                NAME,NFREQ,
     *                NAME,MP,NPSRCF,ASFPT,IERR)
          IF(IERR.NE.0) STOP
      ENDIF
C
C
C     WRITE SUB-DOMAIN ACOUSTIC FIELD DATA
C
C
      IF(JACPR.EQ.1) THEN
          CALL MFNAME(FILEAP,FILEPT,NPART,IUT0,IERR)
          IF(IERR.NE.0) STOP
C
          NPPOWR=NPPT*IRPOWR
          NPGRDP=NPPT*IRGRDP
C
          IACT = 2
          CALL GFAPRS(IUT0,IUT6,IUTAP,FILEPT,
     *                IACT,IWRITE,INAME,
     *                MCOMFL,NFLEAP,CFLEAP,
     *                MCOMST,NSETAP,CSETAP,
     *                NAME, FREQ,
     *                NAME,NFREQ,
     *                NAME,MPPT,NPPT,APPT,
     *                NAME,     NPPOWR,PWRPT,
     *                NAME,     NPGRDP,GRPPT,IERR)
          IF(IERR.NE.0) STOP
      ENDIF
C
      GO TO 305
 1000 CONTINUE
C
      IACT = 7
      CALL GFPART(IUT0,IUT6,IUTDD,FILEDD,
     *            IACT,IWRITE,INAME,IDIM,IRESV,
     *            MCOM,NCOMFL,COMFLE,
     *            MCOM,NCOMST,COMSET,
     *            NAME,MPPT,NPPT ,LISTIP,
     *            NAME,MEPT,NEPT ,LISTIE,
     *            NAME,MBP ,NPINT,LPINT1,LPINT2,LPINT3,
     *            IERR)
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
C CHECK DATA DESCRIPTION
C
C
C (1) MESH DATA
C
C
      IF(JMESH.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** CHECKING MESH DATA DECOMPOSITION **'
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
C (2) ACOUSTIC SOURCE DATA
C
C
      IF(JACSR.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*)
     &    ' ** CHECKING ACOUSTIC SOURCE DATA DECOMPOSTION ** '
C
          DO 1200 IP = 1 , NP
              IF(JFASRF(IP).EQ.0) THEN
                WRITE(IUT0,*) ' ## SOME ACOUSTIC SOURCE DATA LOST; STOP'
                  STOP
              ENDIF
 1200     CONTINUE
          WRITE(IUT6,*) ' DONE!'
      ENDIF
C
C
C (3) ACOUSTIC FIELD DATA
C
C
      IF(JACPR.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*)
     &    ' ** CHECKING ACOUSTIC FIELD DATA DECOMPOSITION ** '
C
          DO 1300 IP = 1 , NP
              IF(JFAPRS(IP).EQ.0) THEN
              WRITE(IUT0,*) ' ## SOME ACOUSTIC PRESSURE DATA LOST; STOP'
                  STOP
              ENDIF
 1300     CONTINUE
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
          WRITE(IUT6,*) '  NPFREE=', NPFMAX
          WRITE(IUT6,*) '  NPSYMT=', NPSMAX
          WRITE(IUT6,*) '  NPCCL =', NPCMAX
          WRITE(IUT6,*) '  NPAWAL=', NPWMAX
          WRITE(IUT6,*) '  NPANOR=', NPNMAX
          WRITE(IUT6,*) '  NPAPRS=', NPPMAX
          WRITE(IUT6,*) '  NPAIMP=', NPMMAX
          WRITE(IUT6,*) '  NPAVEL=', NPVMAX
          WRITE(IUT6,*) '  NPAACC=', NPAMAX
      ENDIF
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' PARTDA: SUCCESSFULLY TERMINATED'
      WRITE(IUT6,*) '        NUMBER OF SUB-DOMAINS GENERATED =', NPART
C
       DUM =  DUM
      NDUM = NDUM
C
C
      STOP
C
 9000 CONTINUE
      WRITE(IUT0,*) ' ## ACOUSTIC DATA DO NOT MATCH MESH DATA; STOP'
      STOP
      END
