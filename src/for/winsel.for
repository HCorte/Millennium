C PROGRAM WINSEL
C
C V18 14-JUL-2009 FJG Read from VLF if JOKER DRAW in LTO wager
C V17 26-JUN-2009 FRP In draw files scan, skip Loto multidraw tickets having
C                     Kicker as add-on since they have been already scanned
C                     scanned in carryover file. This is because Loto winsel
C                     is performed one day before Kicker winsel, and therefore
C                     this Loto multidraw ticket with Kicker as add-on is
C                     already placed into TCF file).
C V16 18-MAY-2001 ANG ADDED WINSEL_RPT  
C V15 02-DEC-2000 UXN TOTOGOLO ADDED.
C V14 14-MAR-2000 UXN NEED_TCF ADDED TO WIN_WINLOD().
C V13 01-MAR-2000 UXN NEED_TCF ADDED.
C V12 03-FEB-2000 UXN WIN_WININT_EXTRA added here.
C V11 14-DEC-1999 OXK MULTIWIN changes.
C V10 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V09 11-JAN-1999 GPW STOPSYS OPTIMIZATION
C V08 06-OCT-1993 HXK Only allow viking tickets to be set to dead!
C V07 20-SEP-1993 GXA I guess the name of Viking's future was WIN_VIKFUTURE.
C V06 20-SEP-1993 GXA Separated Viking's Future from rest of the games. 
C                   A separate WIN_VIK_FUTURE was made due to too many problems.
C V05 15-SEP-1993 HXK Fix for Viking Winsel
C V04 28-AUG-1993 SXH Check for Block# 0 when writing to draw files.
C V03 22-AUG-1993 GXA Released for Finland Dec Conversion / Oddset.
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C LOTTO, SPORTS, AND KICKER WINNER SELECTION PROGRAM.
C
C THE FOLLOWING IS A DETAILED DESCRIPTION ON HOW WINNER SELECTION               
C IS SUPPOSED TO WORK FOR POSTPONING. PLEASE KEEP THIS INFORMATION              
C UP TO DATE IF THE CRITERIA OR DESIGN CHANGES.                                 
C                                                                               
C Note: Only Lotto and Sport type games can be postponed.                       
C       You must hold the Postponed Winner Selection prior                      
C       to the following Winner Selection being held.                           
C                                                                               
C POSTPONING WINNER SELECTION                                                   
C                                                                               
C - Perform carryover winner selection.                                         
C   * Update Joker advanced sales comming into this draw from                   
C     the carryover file (TCF).                                                 
C   * If Joker winner update the validation file with the                       
C     corresponding winning information and mark the record                     
C     with a status of VPOST(for postponed).                                    
C   * Update Joker advance sales going into the next draw.                      
C   * Do not update any sales information for game being postponed.             
C - Perform draw files winner selection.                                        
C   * Update Joker sales for each day of the draw.                              
C   * If Joker winner update the validation file with the                       
C     corresponding winning information and mark the record                     
C     status of VPOST(for postponed).                                           
C   * If new multi-week ticket update Joker advance sales                       
C     create new carryover (TCF) record and mark it has                         
C     as CPOST (for postponed).                                                 
C     NOTE: Now instead of loading all tickets into the                         
C           carryover file we are loading multi-week tickets                    
C           only and keeping all other tickets in the draw files.               
C   * Post all sales accumulated for Joker to its game file.                    
C                                                                               
C POSTPONED WINNER SELECTION                                                    
C                                                                               
C - Perform carryover winner selection.                                         
C   * Skip over records with the status of CPOST. These                         
C     records will be process during the winner selection                       
C     of the draw files.                                                        
C   * Update postponed game advance sales comming into this draw                
C     from the carryover file(TCF).                                             
C   * If postponed game winner update the validation file with the              
C     winning information and mark the record with a status                     
C     of VNOPAY (not payable until prize entry).                                
C   * Update postponed game advance sales going into the next draw.             
C   * Do not update any sales information for Joker.                            
C                                                                               
C - Perform draw files winner selection:                                        
C   * Update postponed game sales for each day of the draw.                     
C   * If postponed game winner then read the validation file                    
C     in case previously won on Joker.  Otherwise, create                       
C     new validation record. Update the validation file with                    
C     the corresponding winning information and mark the record                 
C     with a status of VNOPAY (not payable until prize entry).                  
C   * If multi-week ticket update postponed game advance sales going            
C     into the next draw. No need to create a record for the                    
C     carryover file(TCF) since this was already done when                      
C     POSTPONING the winner selection.                                          
C   * Post all sales accumulated for postponed game to its game file.           
C                                                                               
C IN ADDITION ...                                                               
C                                                                               
C WINUPD will not mark any records marked as VPOST to VUNCSH.                   
C It will update joker winnings but if the status is VPOST it                   
C will not make them VUNCSH. Only after the postponed winsel for                
C its attached game has been held and winsel has changed the                    
C status of the validation record to VNOPAY will WINUPD then                    
C change the status from VNOPAY to VUNCSH.                                      
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM WINSEL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'                             !V03
C
	INTEGER*4  TUBSIZ
	PARAMETER (TUBSIZ=I4BUCSIZ*7)
C
	INTEGER*4 GTYP			!Game Type
	INTEGER*4 LENGTH		!Transaction Length.
	INTEGER*4 TYPE			!
	INTEGER*4 K			!
	INTEGER*4 IND			!
	INTEGER*4 BLOCK			!
	INTEGER*4 S			!
	INTEGER*4 I			!
	INTEGER*4 GIND			!
	INTEGER*4 KWIN			!
	INTEGER*4 FILCNT		!
	INTEGER*4 ST			!
	INTEGER*4 WIN			!
	INTEGER*4 SAVE			!
	INTEGER*4 EOF			!
	INTEGER*4 TFDB(7)		!
	INTEGER*4 TMFBUF(8192)		!
	INTEGER*4 FILES(5,200)		!
	INTEGER*4 TEMP			!
	INTEGER*4 FILTYPE(200)		!
	INTEGER*4 LOGBUF(LREC*3)	!
	INTEGER*4 TCFBUF(TUBSIZ)	!
	INTEGER*4 VLFBUF(I4BUCSIZ)	!
	INTEGER*4 VST			!Validation Status.
	INTEGER*4 FWIN			!Winner Flag for Future.
        INTEGER*4 DAY_OF_DRAW           !
        INTEGER*4   NTSK
        INTEGER*4   INDTSK              !FUNCTION  !V03
        INTEGER*4 KGAM,KIND
	CHARACTER*20 CFILES(200)	!
	BYTE      I1TEMP(4)
	EQUIVALENCE (TEMP,I1TEMP)
	EQUIVALENCE (FILES,CFILES)
	DATA EOF/0/,SAVE/0/,WIN/0/

	INTEGER*4 NEED_TCF
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C INITIALIZE WINNER SELECTION COMMON
C
        IF (.NOT.ISSUBPROC()) THEN
            TYPE*,IAM(),
     *            'This program can be run only from *WINTSK or MULTIWIN'
            CALL GSTOP(GEXIT_FATAL)
        ENDIF

        IF(STOPMOD.EQ.WINMANUAL) THEN
             CALL WIN_WININT(CFILES,FILTYPE,FILCNT)
		ELSE
C            mulnam.def --- TSKWNAM		WINTSK -> index 1 --- NTSK=1	
             NTSK=INDTSK('WINTSK  ')
             CALL STORFIL(NTSK,CFILES,FILTYPE,FILCNT,2,1)
        ENDIF

	IF(FILCNT.EQ.0) THEN
	  TYPE*,IAM(),' Sorry, no Lotto,Vakio,Jokeri winner selections today'
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF
C	
	CARYSCAN(TLTO)=.FALSE.
C
	IF (MRGTYP(1).EQ.MRGVL .AND. STOPMOD.EQ.WINMULTI) THEN
	    NEED_TCF = 0
	ELSE
	    NEED_TCF = 1
	ENDIF
	CALL WIN_WINLOD(1,NEED_TCF)

	CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
C
	IF (NEED_TCF.EQ.0) GOTO 2001
C
C CARRYOVER WINNER SELECTION
C
1000	CONTINUE
	CARYSCAN(TLTO)=.TRUE.      !always same for TKIK and TSPT
	TYPE*,IAM(),' Carryover file winner selection started'
	CALL IOPEN(SFNAMES(1,TCF),TCF,LREC*2,LCDC,LSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,TCF),1,ST,0)
	CALL ITUBSIZE(TCF,TUBSIZ)
C
1030	CONTINUE
	CALL ISREAD(LOGBUF,TCF,TCFBUF,ST)
	IF(ST.EQ.ERREND) THEN
	  CALL ICLOSE(TCF,TCFBUF,ST)
	  GOTO 2000
	ENDIF
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,TCF),2,ST,0)
	CALL LOGTRA(TRABUF,LOGBUF)
	GIND=TRABUF(TGAMIND)

        IF(TRABUF(TSTAT).EQ.XCHD) GOTO 1030

	WIN=0
	KWIN=0
C
C CHECK IF LOTTO WINNER
C
	IF(TRABUF(TGAMTYP).EQ.TLTO) THEN
          IF(LTDELAY(GIND).EQ.1.AND.TRABUF(TFIL).EQ.CPOST) GOTO 1040
          IF(LTDELAY(GIND).EQ.2) TRABUF(TFIL)=CARRY
	  CALL WIN_POST(TRABUF)
	  CALL LIFWIN(TRABUF,WIN)
	  CALL KIFWIN(TRABUF,KWIN)
	  IF(WIN.NE.0.OR.KWIN.NE.0) THEN
	    VST = 0
	    CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
	    IF(ST.EQ.0) CALL ILBYTE(VST,V4BUF,VFSTS-1)
	    IF(ST.EQ.ERRRNF.OR.VST.EQ.VBANK)
     *         CALL FASTSET(0,V4BUF,VFLEN*4)
	    IF(WIN.NE.0)  CALL LCHKWIN(TRABUF,V4BUF,WIN)
	    IF(KWIN.NE.0) CALL KCHKWIN(TRABUF,V4BUF,KWIN)
	    IF(WIN.NE.0.OR.KWIN.NE.0) CALL WIN_WINLOD(2,V4BUF)
	  ENDIF
	ENDIF
C
C CHECK IF SPORTS WINNER
C
	IF(TRABUF(TGAMTYP).EQ.TSPT) THEN
          IF(SPDELAY(GIND).EQ.1.AND.TRABUF(TFIL).EQ.CPOST) GOTO 1040
          IF(SPDELAY(GIND).EQ.2) TRABUF(TFIL)=CARRY
	  CALL WIN_POST(TRABUF)
	  CALL SIFWIN(TRABUF,WIN)
	  CALL KIFWIN(TRABUF,KWIN)
	  IF(WIN.NE.0.OR.KWIN.NE.0) THEN
	    VST = 0
	    CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
	    IF(ST.EQ.0) CALL ILBYTE(VST,V4BUF,VFSTS-1)
	    IF(ST.EQ.ERRRNF.OR.VST.EQ.VBANK)
     *         CALL FASTSET(0,V4BUF,VFLEN*4)
	    IF(WIN.NE.0)  CALL WIN_SCHKWIN(TRABUF,V4BUF,WIN)
	    IF(KWIN.NE.0) CALL KCHKWIN(TRABUF,V4BUF,KWIN)
	    IF(WIN.NE.0.OR.KWIN.NE.0) CALL WIN_WINLOD(2,V4BUF)
	  ENDIF
	ENDIF
C
C CHECK IF TOTOGOLO WINNER
C
	IF(TRABUF(TGAMTYP).EQ.TTGL) THEN
          IF(TGDELAY(GIND).EQ.1.AND.TRABUF(TFIL).EQ.CPOST) GOTO 1040
          IF(TGDELAY(GIND).EQ.2) TRABUF(TFIL)=CARRY
	  CALL WIN_POST(TRABUF)
	  CALL TGIFWIN(TRABUF,WIN)
	  CALL KIFWIN(TRABUF,KWIN)
	  IF(WIN.NE.0.OR.KWIN.NE.0) THEN
	    VST = 0
	    CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
	    IF(ST.EQ.0) CALL ILBYTE(VST,V4BUF,VFSTS-1)
	    IF(ST.EQ.ERRRNF.OR.VST.EQ.VBANK)
     *         CALL FASTSET(0,V4BUF,VFLEN*4)
	    IF(WIN.NE.0)  CALL TGCHKWIN(TRABUF,V4BUF,WIN)
	    IF(KWIN.NE.0) CALL KCHKWIN(TRABUF,V4BUF,KWIN)
	    IF(WIN.NE.0.OR.KWIN.NE.0) CALL WIN_WINLOD(2,V4BUF)
	  ENDIF
	ENDIF
C
C CHECK IF KICKER WINNER
C
        IF(TRABUF(TGAMTYP).EQ.TKIK) THEN
          CALL WIN_POST(TRABUF)
	  CALL KIFWIN(TRABUF,KWIN)
          IF(KWIN.NE.0) THEN
	    VST = 0
            CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
	    IF(ST.EQ.0) CALL ILBYTE(VST,V4BUF,VFSTS-1)
            IF(ST.EQ.ERRRNF.OR.VST.EQ.VBANK)
     *         CALL FASTSET(0,V4BUF,VFLEN*4)
            CALL KCHKWIN(TRABUF,V4BUF,KWIN)
            CALL WIN_WINLOD(2,V4BUF)
          ENDIF
        ENDIF
C
C CHECK IF TICKET IS EXPIRED
C
1040	CONTINUE
	FWIN = 0
	IF(WIN.NE.0.OR.KWIN.NE.0) FWIN = 1
	CALL WIN_FUTURE(TRABUF,SAVE)
	IF(SAVE.NE.0) THEN
      	    CALL TRALOG(TRABUF,LOGBUF)
	    CALL WIN_WINLOD(3,LOGBUF)
	ENDIF
	GOTO 1030
C
C CARRYOVER WINNER SELECTION COMPLETE
C START DRAW FILE SCAN.
C
2000	CONTINUE
	TYPE*,IAM(),' Carryover file winner selection complete'
2001	CONTINUE
	CARYSCAN(TLTO)=.FALSE.
	CALL FASTSET(0,V4BUF,VFLEN*4)
C
C
	DO 3000 I=1,FILCNT
	CALL WIN_OPNDRW(FILES(1,I),PTMF)
	CALL IOINIT(TFDB,PTMF,128*256)
	IF(FILTYPE(I).EQ.1) THEN
	  WRITE(6,920) IAM(),(FILES(S,I),S=1,5)
	  MAILSCAN=.TRUE.
	ELSE
	  WRITE(6,910) IAM(),(FILES(S,I),S=1,5)
	  MAILSCAN=.FALSE.
	ENDIF
C
C SCAN FILE / REWRITE BLOCK WHEN NEW IS READ.
C
	BLOCK=0
	EOF=0
	IND=8192
2030	CONTINUE
	IF(IND.GE.8157) THEN
	  IF(BLOCK.GT.0) THEN
	     CALL WRITEW(TFDB,BLOCK,TMFBUF,ST)
	     IF(ST.NE.0) THEN
	        WRITE(6,930) IAM(),(FILES(K,I),K=1,5),ST,BLOCK
		CALL GPAUSE
	     ENDIF
	  ENDIF
	  BLOCK=BLOCK+1
	  IND=1
	  CALL READW(TFDB,BLOCK,TMFBUF,ST)
	  IF(ST.NE.0) THEN
	    WRITE(6,900) IAM(),(FILES(K,I),K=1,5),ST,BLOCK
	    CALL GPAUSE
	  ENDIF
	ENDIF
	IF(EOF.GT.1000) GOTO 2090
C
C
	IF(TMFBUF(IND).EQ.0) THEN
	  EOF=EOF+1
	  IND=IND+LREC
	  GOTO 2030
	ENDIF
C
C
	EOF=0
	TEMP=TMFBUF(IND+LREC-1)
	TYPE=I1TEMP(4)
	IF(TYPE.NE.LONE.AND.TYPE.NE.LREG) THEN
	  TYPE*,IAM(),' Bad record type > ',TYPE,' index > ',IND
	  IND=IND+LREC
	  GOTO 2030
	ENDIF
C
C
	LENGTH=LREC
	IF(TYPE.EQ.LONE) THEN
	  TEMP=TMFBUF(IND+LREC*2-1)
	  TYPE=I1TEMP(4)
	  IF(TYPE.EQ.LEND) LENGTH=LREC*2
	  IF(TYPE.EQ.LTWO) LENGTH=LREC*3
	ENDIF
	CALL LOGTRA(TRABUF,TMFBUF(IND))
	IND=IND+LENGTH
	GTYP=TRABUF(TGAMTYP)
	GIND=TRABUF(TGAMIND)
	IF(GTYP.NE.TLTO.AND.GTYP.NE.TSPT.AND.GTYP.NE.TKIK.AND.
     *     GTYP.NE.TTGL) GOTO 2030

        DAY_OF_DRAW = DAYCDC

C	IF(TRABUF(TFIL).EQ.CDEAD.AND.
C     *     GTYP.EQ.TLTO.AND.
C     *     GIND.EQ.2.AND.            !VIKING LOTTO ONLY
C     *     TRABUF(TCHK).LT.DAY_OF_DRAW-TRABUF(TCDC)) GOTO 2030
	WIN=0
	KWIN=0
C
C CHECK IF LOTTO WINNER
C
	IF(GTYP.EQ.TLTO) THEN
C
          KGAM=TRABUF(TWKGME)
          IF(TRABUF(TWKFLG).EQ.0.AND.TRABUF(TWKFLG2).EQ.0) KGAM=0
          IF(KGAM.NE.0) THEN  !Kicker is add-on
            KIND=GNTTAB(GAMIDX,KGAM)
            IF(LKKDRW(KIND).LT.1) GOTO 2040    !if not a kicker winsel day
            IF(TRABUF(TWKDUR).GT.1) GOTO 2050   !tkt already scanned in TCF
          ENDIF
C
2040      CONTINUE
          CALL WIN_POST(TRABUF)
C=====================================================================
C V18 INI
C=====================================================================
C         IF(LTDELAY(GIND).EQ.1) THEN
C           CALL LIFWIN(TRABUF,WIN)
C           IF(WIN.NE.0) THEN
C             CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
C             IF(ST.EQ.ERRRNF) CALL FASTSET(0,V4BUF,VFLEN*4)
C           ENDIF
C         ENDIF
C	  CALL LCHKWIN(TRABUF,V4BUF,WIN)
C	  CALL KCHKWIN(TRABUF,V4BUF,KWIN)
C=====================================================================
          CALL LIFWIN(TRABUF,WIN)
          CALL KIFWIN(TRABUF,KWIN)          	  
          IF((WIN.NE.0.AND.LTDELAY(GIND).EQ.1).OR.KWIN.NE.0) THEN
            CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
            IF(ST.EQ.ERRRNF) CALL FASTSET(0,V4BUF,VFLEN*4)
          ENDIF
          IF(WIN.NE.0) CALL LCHKWIN(TRABUF,V4BUF,WIN)
          IF(KWIN.NE.0) CALL KCHKWIN(TRABUF,V4BUF,KWIN)
C=====================================================================
C V18 FIN
C=====================================================================
	ENDIF
C
2050    CONTINUE
C
C CHECK IF SPORTS WINNER
C
	IF(GTYP.EQ.TSPT) THEN
	  CALL WIN_POST(TRABUF)
C
          IF(SPDELAY(GIND).EQ.1) THEN
            CALL SIFWIN(TRABUF,WIN)
            IF(WIN.NE.0) THEN
              CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
              IF(ST.EQ.ERRRNF) CALL FASTSET(0,V4BUF,VFLEN*4)
            ENDIF
          ENDIF
	  CALL WIN_SCHKWIN(TRABUF,V4BUF,WIN)
	  CALL KCHKWIN(TRABUF,V4BUF,KWIN)
	ENDIF
C
C CHECK IF TOTOGOLO WINNER
C
	IF(GTYP.EQ.TTGL) THEN
	  CALL WIN_POST(TRABUF)
C
          IF(TGDELAY(GIND).EQ.1) THEN
            CALL TGIFWIN(TRABUF,WIN)
            IF(WIN.NE.0) THEN
              CALL IREAD(TRABUF(TCDC),V4BUF,VLF,ST)
              IF(ST.EQ.ERRRNF) CALL FASTSET(0,V4BUF,VFLEN*4)
            ENDIF
          ENDIF
	  CALL TGCHKWIN(TRABUF,V4BUF,WIN)
	  CALL KCHKWIN(TRABUF,V4BUF,KWIN)
	ENDIF
C
C CHECK IF KICKER WINNER
C
        IF(GTYP.EQ.TKIK) THEN
          CALL WIN_POST(TRABUF)
          CALL KCHKWIN(TRABUF,V4BUF,KWIN)
        ENDIF
C
C LOAD WINNER
C
	IF(WIN.NE.0.OR.KWIN.NE.0) THEN
	  CALL WIN_WINLOD(2,V4BUF)
	  CALL FASTSET(0,V4BUF,VFLEN*4)
	ENDIF
C
C CHECK IF TICKET IS EXPIRED
C
	FWIN = 0
	IF(WIN.NE.0.OR.KWIN.NE.0) FWIN = 1
	CALL WIN_FUTURE(TRABUF,SAVE)
	IF(SAVE.NE.0) THEN
	   CALL TRALOG(TRABUF,LOGBUF)
	   CALL WIN_WINLOD(3,LOGBUF)
C           IF(GTYP.EQ.TLTO.AND.GIND.EQ.2) THEN  !VIKING LOTTO ONLY
C 	      TRABUF(TFIL) = CDEAD
C	      TRABUF(TCHK) = DAYCDC - TRABUF(TCDC)
C	      CALL TRALOG(TRABUF,TMFBUF(IND-LENGTH))
C           ENDIF
	ENDIF
	GOTO 2030
2090	CONTINUE
C
C WRITE BACK LAST BLOCK
C
	CALL WRITEW(TFDB,BLOCK,TMFBUF,ST)
	IF(ST.NE.0) THEN
	   WRITE(6,930) IAM(),(FILES(K,I),K=1,5),ST,BLOCK
	   CALL GPAUSE
	ENDIF
C
C
C WINSEL COMPLETE, FLUSH BUFFERS TO DRAW FILES
C
3000	CONTINUE
	TYPE*,IAM(),' Draw file scan complete'
	CALL WIN_WINLOD(4,NEED_TCF)
	CALL ICLOSE(VLF,VLFBUF,ST)
	CALL WIN_PSTGDF
	CALL WINSEL_RPT()
	CALL GSTOP(GEXIT_SUCCESS)
C
C
900	FORMAT(1X,A,1X,5A4,' read error> ',I4,' block> ',I8)
910	FORMAT(1X,A,1X,'Scanning file ',5A4,' for winners ',
     *	          'and carryovers ')
920	FORMAT(1X,A,1X,' Scanning file ',5A4,
     *         ' for subscription winners')
930	FORMAT(1X,A,1X,5A4,' write error> ',I8,' block> ',I8)
	END
