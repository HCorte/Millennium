C
C $Log:   GXAFIP:[GOLS]MKTMES.FOV  
C  
C     Rev 1.1   01 Feb 1997 17:39:32   RXK
C  Changes for CDU.
C  
C     Rev 1.0   17 Apr 1996 14:02:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.10   10 Aug 1995 16:20:20   RXK
C  Scandinavian uppercase letters supported
C  
C     Rev 1.9   15 Sep 1993 16:46:56   GXA
C  Cleaned up prompts and added IAM()'s where missing.
C  
C     Rev 1.8   30 Aug 1993  8:19:10   JWE
C  Check for exit request from INPNUM.
C  
C     Rev 1.7   17 Aug 1993 15:21:58   HXK
C  fix for mktmes rev bugs
C  
C     Rev 1.6   06 Aug 1993 19:13:06   HXK
C  CHANGED WAY REV NUMBER IS STORED.
C  
C     Rev 1.5   04 Aug 1993 19:48:40   HXK
C  only set lines are checksummed.
C  
C     Rev 1.4   04 Aug 1993 15:17:48   HXK
C  Added Checksum table based on line length of 30 chars.
C  
C     Rev 1.3   29 Jul 1993 18:55:22   HXK
C  FIXED LENGTH, CHECKSUM PROBLEM
C  
C     Rev 1.2   17 Jul 1993 16:01:32   GXA
C  Correct definition of BLANK.
C  
C     Rev 1.1   12 Jul 1993 20:12:04   GXA
C  Released for Finland Dec Conversion / Oddset.
C PROGRAM TO ENTER MARKETING MESSAGES (TICKET MSG) (ONLINE OR OFFLINE)
C                                                                               
C V04  1-JUN-93 HJK  VAX CONVERSION
C V03 18-NOV-91 STC  ONLY ALLOW VALID CHARACTERS IN MESSAGE                     
C V02 11-OCT-91 HJK  FIX FOR LOSING FIRST LINE OF MESSAGE                       
C V01 XX-JUL-91 MTK  INITIAL VERSION FOR FINLAND                                
C                                                                               
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 1997 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                  
C=======OPTIONS /CHECK=NOOVERFLOW                                       
      PROGRAM MKTMES                                                            
      IMPLICIT NONE                                                     

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'                                     
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'       !TKT -> MEMORY                
      INCLUDE 'INCLIB:RECTKM.DEF'       !TKM -> DISK                  
      INCLUDE 'INCLIB:RECSCF.DEF'                                     
      INCLUDE 'INCLIB:GTNAMES.DEF'                                    

      INTEGER*4 MTAB(TICKET_LENGTH,TICKET_ROWS)		!TKT msg table
      CHARACTER CMTAB(TICKET_LENGTH*4,TICKET_ROWS)	!TKT msg table char
      BYTE      BMTAB(TICKET_LENGTH*4,TICKET_ROWS)      !TKT msg table byte
      INTEGER*4 FDB(7)					!File descriptor block
      INTEGER*4 GAM					!Game #
      INTEGER*4 GTYP					!Game Type
      INTEGER*4 GIND					!Game Index
      INTEGER*4 OPN					!
      INTEGER*4 MSG					!
      INTEGER*4 ST					!Return Status
      INTEGER*4 EXT					!Return Status
      INTEGER*4 OPT					!Selection Option
      INTEGER*4 LINES					!#Lines in msg.
      INTEGER*4 BLANK					!Filler
      INTEGER*4 I, J, K					!Loop variable
      INTEGER*4 FLAG					! 
      INTEGER*4 CSUM					!Ticket Text Checksum.
      INTEGER*4 I4TEMP                                  !I*4 -> I*1 work array
      CHARACTER*6 STRING(TICKET_ROWS)			!Prompt String.
      LOGICAL   ONLINE                                  !Online/Offline flag
      BYTE      CHK_MTAB((TICKET_LENGTH*4)-2,
     *                   TICKET_ROWS)                   !TKT checksum table
      BYTE      I1TEMP(4)                               !I*4 -> I*1 work array
      EQUIVALENCE (MTAB,CMTAB,BMTAB)                           
      EQUIVALENCE (I4TEMP,I1TEMP)    
      DATA BLANK/Z20202020/
C
      INTEGER*4 LGAM                                    !return status (for P,C)
      INTEGER*4 CDU
      INTEGER*4 NUMCHAR
      INTEGER*4 CDU_ROWS                                !# of rows on CDU  
      INTEGER*4 CDU_CHAR                                !length of row on CDU
      INTEGER*4 GAM_CHAR                                !length of row for games
      INTEGER*4 CDU_CHARW                               !length of row in words
      PARAMETER(CDU_ROWS=2)
      PARAMETER(CDU_CHAR=20) 
      PARAMETER(GAM_CHAR=30) 
      PARAMETER(CDU_CHARW=5)
      BYTE      BCHK_CDU(CDU_CHAR,NUMCDU*CDU_ROWS)      !CDU texts altogether
      INTEGER*4 CHK_CDU(CDU_CHARW,NUMCDU*CDU_ROWS)
      EQUIVALENCE (BCHK_CDU,CHK_CDU)
      CHARACTER CDUTEXT(1:NUMCDU)*14
      DATA CDUTEXT/'Finnish text 1','Swedish text 1',
     *             'Finnish text 2','Swedish text 2'/
      LOGICAL   CDUMOD,CDUUPD,CDUDEL                    !CDU text modified flags
C
      INTEGER*4    SYMBV
      CHARACTER*1  SYMBOL
      EQUIVALENCE  (SYMBV,SYMBOL)

      INTEGER*4    AE/Z000000C4/,OE/Z000000D6/,AO/Z000000C5/         ! Ä,Ö,Å
      CHARACTER*1  AE_TERM/'5B'X/,OE_TERM/'5C'X/,AO_TERM/'5D'X/      ! [,|,]  
      CHARACTER*1  AE_CHAR,OE_CHAR,AO_CHAR
      EQUIVALENCE  (AE,AE_CHAR),(OE,OE_CHAR),(AO,AO_CHAR)

      INTEGER*4    AE2/Z000000E4/,OE2/Z000000F6/,AO2/Z000000E5/      ! ä,ö,å
      CHARACTER*1  AE2_TERM/'7B'X/,OE2_TERM/'7C'X/,AO2_TERM/'7D'X/   ! {,|,} 
      CHARACTER*1  AE2_CHAR,OE2_CHAR,AO2_CHAR
      EQUIVALENCE  (AE2,AE2_CHAR),(OE2,OE2_CHAR),(AO2,AO2_CHAR)
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
      CALL COPYRITE                                                             
C
      CDUMOD = .FALSE.
      CDUUPD = .FALSE.
      CDUDEL = .FALSE.
C
C BUILD LINE ID TEXT STRINGS
C
      DO I=1,TICKET_ROWS
        WRITE(STRING(I),909) I
      END DO
C                                                                               
C CHECK IF ONLINE OR OFFLINE UPDATE                                             
C         
      IF(DAYSTS.EQ.DSOPEN) THEN                                                 
        ONLINE=.TRUE.                                                           
        TYPE*,IAM(),'Online marketing message update'  
        CALL FASTMOV(TKTMES(1,1,1),TKMMES(1,1,1),
     *       TICKET_LENGTH*TICKET_ROWS*(MAXGAM+PRM_NUMOPN+NUMCDU))
        CALL FASTMOV(TKTMLN(1),TKMMLN(1),MAXGAM+PRM_NUMOPN+NUMCDU)
        CALL FASTMOV(GNTTAB(1,1),SCFGNT(1,1),2*MAXGAM)                          
        CALL FASTMOV(GLNAMES(1,1),SCFLGN(1,1),4*MAXGAM)                         
        CALL FASTMOV(SCC_OPNID(1),SCF_OPNID(1),PRM_NUMOPN)
        CALL FASTMOV(SCC_OPNDATE(1,1),SCF_OPNDATE(1,1),
     *               PRM_ENDDAT*PRM_NUMOPN)
        CALL FASTMOV(TKTMRV(1),TKMMRV(1),MAXGAM+PRM_NUMOPN+NUMCDU)
      ELSE   
        ONLINE=.FALSE.                                                          
        TYPE*,IAM(),'Offline marketing message update' 
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN
	   TYPE*,IAM(),'Unable to get System Configuration'
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
        CALL OPENW(1,SCFSFN(1,TKTM),4,0,0,ST)               
        CALL IOINIT(FDB,1,TKMSEC*256)                       
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,TKTM),1,ST,0)      
        CALL READW(FDB,1,TKMREC,ST)                                             
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,TKTM),2,ST,1)      
      ENDIF
C          
C          
10    CONTINUE
      WRITE(5,904)                                                              
      WRITE(5,900) IAM(),IAM(),IAM(),IAM(),IAM(),IAM()
      CALL PRMNUM('Enter function: ',OPT,1,3,EXT) 
      IF(EXT.LT.0) GOTO 4000
      IF(OPT.EQ.1) GOTO 1000                                                    
      IF(OPT.EQ.2) GOTO 2000                                                    
      IF(OPT.EQ.3) GOTO 3000                                                    
C                                                                               
C DISPLAY CURRENT MESSAGE                                                       
C                                                                               
1000  CONTINUE
      WRITE(5,916) IAM(),'DISPLAY.',IAM(),IAM(),IAM()            
      CALL PRMNUM('Enter number or C/P/E [E-Exit]', GAM,1,MAXGAM,LGAM)          
      IF(LGAM.LT.0.AND..NOT.(LGAM.EQ.-5.OR.LGAM.EQ.-2)) GOTO 10      !C=-5,P=-2
C
      NUMCHAR=GAM_CHAR
      IF(LGAM.EQ.-5) THEN            ! CDU text
        WRITE(5,910) IAM(),IAM(),IAM(),IAM(),IAM(),IAM()
        CALL PRMNUM('Enter number for CDU [E=exit]', CDU,1,NUMCDU,EXT) 
	IF(EXT.LT.0) GOTO 10                                           
        MSG=MAXGAM+PRM_NUMOPN+CDU
        NUMCHAR=CDU_CHAR
      ELSEIF(LGAM.EQ.-2) THEN        ! opinion poll text
        CALL PRMNUM(
     * 'Enter opinion poll number [not ID, see BLDSYS] ',
     *              OPN,1,PRM_NUMOPN,EXT)
	IF(EXT.LT.0) GOTO 10                                           
        IF(SCF_OPNID(OPN).EQ.0.OR.
     *     SCF_OPNDATE(PRM_ENDDAT,OPN).LT.DAYCDC) THEN
          TYPE*,IAM(),'Opinion poll ',OPN,' not currently active'
          GOTO 1000
        ENDIF
        MSG=MAXGAM+OPN
      ELSE
        GTYP=SCFGNT(GAMTYP,GAM)
        GIND=SCFGNT(GAMIDX,GAM)   
        IF(GTYP.LT.1.OR.GIND.LT.1) THEN   
          TYPE*,IAM(),'Game ',GAM,' not currently active'   
          GOTO 1000   
        ENDIF
        MSG=GAM
      ENDIF                                                                     
C                                                             
C             
      IF(TKMMLN(MSG).EQ.0) THEN
        IF(LGAM.EQ.-5) THEN             ! CDU text
          WRITE(5,911) IAM(),CDUTEXT(CDU)
          GOTO 1000
        ELSEIF(LGAM.EQ.-2) THEN         ! opinion poll  
          WRITE(5,9010) IAM(),OPN,SCF_OPNID(OPN)
          GOTO 1000
        ELSE                           ! game message                  
          WRITE(5,901) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GAM),K=1,4)
          GOTO 1000
        ENDIF                                                               
      ENDIF                                                                     
C                                                                               
C                                                                               
      CALL FASTSET(BLANK,MTAB,TICKET_LENGTH*TICKET_ROWS)       
      DO I=1,TKMMLN(MSG)                                                   
         CALL FASTMOV(TKMMES(1,I,MSG),MTAB(1,I),TICKET_LENGTH)    
      END DO
      LINES=TKMMLN(MSG)                                                         
      WRITE(5,904)                                                              
      IF(LGAM.EQ.-5) THEN
          WRITE(5,912) IAM(),CDUTEXT(CDU),IAM()
      ELSEIF(LGAM.EQ.-2) THEN
        WRITE(5,9020) IAM(),OPN,SCF_OPNID(OPN),IAM()
      ELSE
        WRITE(5,902) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GAM),K=1,4),
     *               IAM()  
      ENDIF
      DO I=1,LINES                                                         
         WRITE(5,903) IAM(),STRING(I),(CMTAB(K,I),K=1,NUMCHAR)  
      END DO
      WRITE(5,904)                                                              
      GOTO 1000                                                                 
C                                                                               
C ENTER GAME MESSAGE                                                            
C                                                                               
2000  CONTINUE                                                                  
      WRITE(5,916) IAM(),'UPDATE.',IAM(),IAM(),IAM()                      
      CALL PRMNUM('Enter number or C/P/E [E=exit]', GAM,1,MAXGAM,LGAM)
      IF(LGAM.LT.0.AND..NOT.(LGAM.EQ.-5.OR.LGAM.EQ.-2)) GOTO 10      !C=-5,P=-2
C
      NUMCHAR=GAM_CHAR
      IF(LGAM.EQ.-5) THEN             ! CDU TEXT
        WRITE(5,910) IAM(),IAM(),IAM(),IAM(),IAM(),IAM()
        CALL PRMNUM('Enter number for CDU [E=exit]', CDU,1,NUMCDU,EXT)         
        IF(EXT.LT.0) GOTO 10                                                   
        MSG=MAXGAM+PRM_NUMOPN+CDU
        GTYP=TIN
        GIND=CDU 
        NUMCHAR=CDU_CHAR
      ELSEIF(LGAM.EQ.-2) THEN ! OPINION POLL
        CALL PRMNUM(
     * 'Enter opinion poll number [not ID, see BLDSYS] ',
     *              OPN,1,PRM_NUMOPN,EXT)
        IF(EXT.LT.0) GOTO 10                                                   
        IF(SCF_OPNID(OPN).EQ.0.OR.
     *     SCF_OPNDATE(PRM_ENDDAT,OPN).LT.DAYCDC) THEN
          TYPE*,IAM(),'Opinion poll ',OPN,' not currently active'
          GOTO 1000
        ENDIF
        MSG=MAXGAM+OPN
        GTYP=TNBR
        GIND=OPN
      ELSE                          ! GAME TICKET MESSAGE
        GTYP=SCFGNT(GAMTYP,GAM)
        GIND=SCFGNT(GAMIDX,GAM)
        IF(GTYP.LT.1.OR.GIND.LT.1) THEN
          TYPE*,IAM(),'Game ',GAM,' not currently active'
          GOTO 1000
        ENDIF
        MSG=GAM
      ENDIF
C                   
      CALL FASTSET(BLANK,MTAB,TICKET_LENGTH*TICKET_ROWS)
      IF(LGAM.EQ.-5) THEN
        WRITE(5,914) IAM(),CDUTEXT(CDU)
        CALL WIMG(5,'Is this the correct CDU text [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 2000
      ELSEIF(LGAM.EQ.-2) THEN
        WRITE(5,9050) IAM(),OPN,SCF_OPNID(OPN)
        CALL WIMG(5,'Is this the correct opinion poll [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 2000
      ELSE
        WRITE(5,905) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GAM),K=1,4)
        CALL WIMG(5,'Is this the correct game [Y/N] ')                       
        CALL YESNO(FLAG)                                                     
        IF(FLAG.NE.1) GOTO 2000
      ENDIF                                       
2010  CONTINUE
      IF(LGAM.NE.-5) THEN     
        CALL PRMNUM('Enter number of lines ',LINES,1,TICKET_ROWS,EXT)         
        IF(EXT.LT.0) GOTO 2000                                     
        WRITE(5,906) IAM()                                         
      ELSE
        CALL PRMNUM('Enter number of lines ',LINES,1,CDU_ROWS,EXT)         
        IF(EXT.LT.0) GOTO 2000                                     
        WRITE(5,913) IAM()                                          
      ENDIF
C
      DO 2020 I=1,LINES                                                         
C
2013     CONTINUE                    
         CALL WIMG(5,STRING(I))      
         READ(5,907) (CMTAB(K,I),K=1,GAM_CHAR)
C                                                                               
C VERIFY VALID CHARACTERS                                                       
C                                                                               
         DO 2015 J=1,GAM_CHAR
            IF(LGAM.EQ.-5.AND.
     *         (I.NE.CDU_ROWS.AND.J.GT.CDU_CHAR .OR.
     *          I.EQ.CDU_ROWS.AND.J.GE.CDU_CHAR)) THEN
               CMTAB(J,I)=' '
               GOTO 2015
            ENDIF 
            IF(CMTAB(J,I).LT.' '.OR.CMTAB(J,I).GT.'z') THEN
               SYMBOL = CMTAB(J,I)
               IF(.NOT.(SYMBV.EQ.AE.OR.SYMBV.EQ.OE.OR.SYMBV.EQ.AO.OR.
     *                  SYMBV.EQ.AE2.OR.SYMBV.EQ.OE2.OR.SYMBV.EQ.AO2))
     *            THEN
                  WRITE(5,908) IAM(),J,IAM() 
                  GOTO 2013
               ENDIF 
            ENDIF    
2015     CONTINUE    
2020  CONTINUE       
C                                       
      TYPE*,IAM(),'Message entered '                        
      WRITE(5,904)                                                              
      IF(LGAM.EQ.-5) THEN
        WRITE(5,915) IAM(),CDUTEXT(CDU)
      ELSEIF(LGAM.EQ.-2) THEN
        WRITE(5,9020) IAM(),OPN,SCF_OPNID(OPN)
      ELSE
        WRITE(5,902) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GAM),K=1,4),
     *               IAM()
      ENDIF
      DO 2030 I=1,LINES                                                         
        WRITE(5,903) IAM(),STRING(I),(CMTAB(K,I),K=1,NUMCHAR)    
2030  CONTINUE                                                                  
      WRITE(5,904)                                                              
      CALL WIMG(5,'Is this correct [Y/N] ')                                     
      CALL YESNO(FLAG)                                                          
      IF(FLAG.NE.1) GOTO 2010                    
      IF(LGAM.EQ.-5) THEN
         CDUMOD=.TRUE.                               
         CDUUPD=.TRUE.                               
      ENDIF
C                                                                               
C CHANGE SCANDINAVIAN CHARACTERS
C                                                                               
      DO  I=1,LINES                                                         
         DO  J=1,GAM_CHAR
            SYMBOL = CMTAB(J,I)
            IF(SYMBV.EQ.AE) THEN 
               CMTAB(J,I) = AE_TERM
            ELSEIF(SYMBV.EQ.OE) THEN 
               CMTAB(J,I) = OE_TERM
            ELSEIF(SYMBV.EQ.AO) THEN
               CMTAB(J,I) = AO_TERM
            ELSEIF(SYMBV.EQ.AE2) THEN 
               CMTAB(J,I) = AE2_TERM
            ELSEIF(SYMBV.EQ.OE2) THEN 
               CMTAB(J,I) = OE2_TERM
            ELSEIF(SYMBV.EQ.AO2) THEN
               CMTAB(J,I) = AO2_TERM
            ENDIF
         ENDDO
      ENDDO
C
      TKMMLN(MSG)=LINES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      TYPE 1,'enne muutmist rev TKtMRV(MSG)',TKtMRV(MSG)
C      TYPE 1,'enne muutmist rev TKmMRV(MSG)',TKmMRV(MSG)
C1     format(1x,a,z8)
ccccccccccccccccccccccccccccccccccccccccccccc)
      I4TEMP=TKMMRV(MSG)
      I1TEMP(1)=I1TEMP(1)+1
      CALL FASTMOV(MTAB,TKMMES(1,1,MSG),TICKET_LENGTH*TICKET_ROWS)
      DO J=1,TICKET_ROWS
         DO I=1,GAM_CHAR
            CHK_MTAB(I,J)=BMTAB(I,J)
         ENDDO
      ENDDO
      CALL CHECKSUM(CHK_MTAB,0,GAM_CHAR*LINES,CSUM)
      I1TEMP(2)=CSUM
      TKMMRV(MSG) = I4TEMP  
cccccccccccccccccccccccccccccccccccccccccccc
C      TYPE 1,'parast muutmist rev TKmMRV(MSG)',TKmMRV(MSG)
ccccccccccccccccccccccccccccccccccccccccccccc
      IF(ONLINE) THEN                                                           
         IF(CDUUPD) THEN                           !calculate CDU texts checksum
            DO I=1,NUMCDU    
               DO J=1,CDU_ROWS
                  CALL FASTMOV(TKMMES(1,J,MAXGAM+PRM_NUMOPN+I),
     *                         CHK_CDU(1,2*(I-1)+J), CDU_CHARW)
            
               END DO
            ENDDO
            CALL CHECKSUM(BCHK_CDU,0,NUMCDU*CDU_ROWS*CDU_CHAR,CSUM)
            I4TEMP=TKTCDU
            I1TEMP(1)=I1TEMP(1)+1
            I1TEMP(2) = CSUM
            TKTCDU = I4TEMP
         ENDIF
         TKTMLN(MSG) = LINES
	 TKTMRV(MSG) = I4TEMP
cccccccccccccccccccccccccccccccccccccccccccc
C      TYPE 1,'parast muutmist rev TKtMRV(MSG)',TKtMRV(MSG)
ccccccccccccccccccccccccccccccccccccccccccccc
         CALL FASTMOV(MTAB,TKTMES(1,1,MSG),TICKET_LENGTH*TICKET_ROWS)
         CALL SETMES(MSG,LINES,MTAB)
         CALL BROTKTMES(MSG,GTYP,GIND)
         CDUUPD = .FALSE.        
      ENDIF
      GOTO 2000                                                                 
C                                                                               
C DELETE GAME MESSAGE                                                           
C                                                                               
3000  CONTINUE                                                                  
      WRITE(5,916) IAM(),'DELETE.',IAM(),IAM(),IAM()                      
      CALL PRMNUM('Enter number or C/P/E [E=exit]', GAM,1,MAXGAM,LGAM)
      IF(LGAM.LT.0.AND..NOT.(LGAM.EQ.-5.OR.LGAM.EQ.-2)) GOTO 10      !C=-5,P=-2
C
      NUMCHAR=GAM_CHAR
      IF(LGAM.EQ.-5) THEN
        WRITE(5,910) IAM(),IAM(),IAM(),IAM(),IAM(),IAM()
        CALL PRMNUM('Enter number for CDU [E=exit]', CDU,1,NUMCDU,EXT)         
        IF(EXT.LT.0) GOTO 10                                                   
        MSG=MAXGAM+PRM_NUMOPN+CDU
        GTYP=TIN
        GIND=CDU
        NUMCHAR=CDU_CHAR
      ELSEIF(LGAM.EQ.-2) THEN
        CALL PRMNUM(
     * 'Enter opinion poll number [not ID, see BLDSYS] ',
     *              OPN,1,PRM_NUMOPN,EXT)
        IF(EXT.LT.0) GOTO 10                                                   
        IF(SCF_OPNID(OPN).EQ.0.OR.
     *     SCF_OPNDATE(PRM_ENDDAT,OPN).LT.DAYCDC) THEN
          TYPE*,IAM(),'Opinion poll ',OPN,' not currently active'
          GOTO 1000
        ENDIF
        MSG=MAXGAM+OPN
        GTYP=TNBR
        GIND=OPN
      ELSE
        GTYP=SCFGNT(GAMTYP,GAM)
        GIND=SCFGNT(GAMIDX,GAM)
        IF(GTYP.LT.1.OR.GIND.LT.1) THEN
          TYPE*,IAM(),'Game ',GAM,' not currently active'
          GOTO 1000
        ENDIF
        MSG=GAM
      ENDIF
C                                                                               
      IF(TKMMLN(MSG).EQ.0) THEN
        IF(LGAM.EQ.-5) THEN             !CDU TEXT
          WRITE(5,911) IAM(),CDUTEXT(CDU)
        ELSEIF(LGAM.EQ.-2) THEN         !OPINION POLL
          WRITE(5,9010) IAM(),OPN,SCF_OPNID(OPN)
          GOTO 3000
        ELSE                           !GAME MESSAGE    
          WRITE(5,901) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GAM),K=1,4)
          GOTO 3000
        ENDIF
      ENDIF
C
      LINES=TKMMLN(MSG)                                                         
      CALL FASTSET(BLANK,MTAB,TICKET_LENGTH*TICKET_ROWS)
      DO I=1,TKMMLN(MSG)                                                   
        CALL FASTMOV(TKMMES(1,I,MSG),MTAB(1,I),TICKET_LENGTH)
      END DO
      IF(LGAM.EQ.-5) THEN
        WRITE(5,915) IAM(),CDUTEXT(CDU)
      ELSEIF(LGAM.EQ.-2) THEN  
        WRITE(5,9020) IAM(),OPN,SCF_OPNID(OPN),IAM()
      ELSE
        WRITE(5,902) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GAM),K=1,4),
     *               IAM()
      ENDIF
      DO 3020 I=1,LINES                                                         
        WRITE(5,903) IAM(),STRING(I),(CMTAB(K,I),K=1,NUMCHAR)            
3020  CONTINUE                                                                  
      WRITE(5,904)                                                              
      CALL WIMG(5,                                                              
     *  'Are you sure you want to delete this message [Y/N] ')                  
      CALL YESNO(FLAG)                                                          
      IF(FLAG.NE.1) GOTO 3000                                                   
      LINES = 0  
      TKMMLN(MSG) = 0  
      TKMMRV(MSG) = 0
      CALL FASTSET(BLANK,TKMMES(1,1,MSG),TICKET_LENGTH*TICKET_ROWS)
      TYPE*,IAM(),'Message deleted'
      IF(LGAM.EQ.-5) THEN
         CDUMOD=.TRUE.                               
         CDUDEL=.TRUE.                               
      ENDIF
      IF(ONLINE) THEN                                                           
        IF(CDUDEL) THEN                           !calculate CDU texts checksum
           DO I=1,NUMCDU    
              DO J=1,CDU_ROWS
                 CALL FASTMOV(TKMMES(1,J,MAXGAM+PRM_NUMOPN+I),
     *                        CHK_CDU(1,2*(I-1)+J), CDU_CHARW)
            
              END DO
           ENDDO
           CALL CHECKSUM(BCHK_CDU,0,NUMCDU*CDU_ROWS*CDU_CHAR,CSUM)
           I4TEMP=TKTCDU
           I1TEMP(1)=I1TEMP(1)+1
           I1TEMP(2) = CSUM
           TKTCDU = I4TEMP        
        ENDIF
        TKTMLN(MSG) = 0
	TKTMRV(MSG) = 0 
        CALL FASTSET(BLANK,TKTMES(1,1,MSG),TICKET_LENGTH*TICKET_ROWS)
        CALL SETMES(MSG,LINES,MTAB)
        CALL BROTKTMES(MSG,GTYP,GIND)                               
        CDUDEL = .FALSE.        
      ENDIF   
      GOTO 3000                                                                 
C                                                                               
C                                                                               
4000  CONTINUE                                                                  
C
C IF OFFLINE UPDATE CDU REVISION NUMBER FOR ALL TEXTS ALTOGETHER
C
      IF(CDUMOD.AND..NOT.ONLINE) THEN
         DO I=1,NUMCDU    
            DO J=1,CDU_ROWS
               CALL FASTMOV(TKMMES(1,J,MAXGAM+PRM_NUMOPN+I),
     *                      CHK_CDU(1,2*(I-1)+J), CDU_CHARW)
            
            END DO
         ENDDO
         CALL CHECKSUM(BCHK_CDU,0,NUMCDU*CDU_ROWS*CDU_CHAR,CSUM)
         I4TEMP=TKMCDU
         I1TEMP(1)=I1TEMP(1)+1
         I1TEMP(2) = CSUM
         TKMCDU = I4TEMP 
      ENDIF
ccccccccccccccccccccccccccccccccccccccccccccccccccc
C      type*,'==========================================='
C      type 2,' msg #',msg,'  checksum', TKTMRV(MSG),TKMMRV(MSG)
C2     format(1x,a,i3,a,2(z8))
C      type3,' TKTCDU=',TKtCDU,' TKMCDU=',TKMCDU
C3     format(1x,a,z8,a,z8)
C       TYPE*,' lines=',lines,'  TKTMLN(MSG)=',TKTMLN(MSG),
C      *        '  TKMMLN(MSG)=',TKmMLN(MSG)
cccccccccccccccccccccccccccccccccccccccccccccccccc
C
      IF(.NOT.ONLINE) THEN                                                      
        CALL WRITEW(FDB,1,TKMREC,ST)                                            
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,TKTM),3,ST,1)                      
        CALL CLOSEFIL(FDB)
      ENDIF
      CALL GSTOP(GEXIT_SUCCESS) 
C                                                                               
C                                                                               
900   FORMAT(1X,A,'Marketing ticket message program ',/,
     *       1X,A,'[Games, CDU texts and Opinion Polls]',//,        
     *       1X,A,'1 - Display current message',/,                  
     *       1X,A,'2 - Enter new ticket message',/,                 
     *       1X,A,'3 - Delete ticket message',/,                    
     *       1X,A,'E - Program exit ',/)                            
901   FORMAT(/,1X,A,A8,1X,I1,2X,4A4,' no message set',/)             
902   FORMAT(/,1X,A,A8,1X,I1,2X,4A4,/,1X,A,                         
     *       7X,'123456789012345678901234567890',/)                 
903   FORMAT(1X,A,A6,'>',<NUMCHAR>A1,'<')                                  
904   FORMAT(//)                                                                
905   FORMAT(1X,A,A8,1X,I1,2X,4A4)                                  
906   FORMAT(//,1X,A,8X,'123456789012345678901234567890')           
907   FORMAT(<GAM_CHAR>A1)                                           
908   FORMAT(1X,A,'Invalid character in column ',I2,/,1X,A,         
     *          'Please re-enter')                                              
909   FORMAT('Line ',I1)
9010  FORMAT(//,1X,A,'Opinion Poll ',I2,':  ID = ',I2.2,
     *       '   no message set')
9020  FORMAT(/,1X,A,'Opinion Poll ',I2,':  ID = ',I2.2,//,1X,A,
     *       7X,'123456789012345678901234567890')
9050  FORMAT(//,1X,A,'Opinion Poll ',I2,':  ID = ',I2.2)
910   FORMAT(/,1X,A,'CDU text ',/,
     *       1X,A,'1 - Text 1, Finnish language display',/,         
     *       1X,A,'2 - Text 1, Swedish language display',/,         
     *       1X,A,'3 - Text 2, Finnish language display',/,         
     *       1X,A,'4 - Text 2, Swedish language display',/,
     *       1X,A,'E - Exit ',/)                                            
911   FORMAT(//,1X,A,'CDU text for ',A14,' not set')
912   FORMAT(//,1X,A,'CDU text for ',A14,//,1X,A,                   
     *       7X,'12345678901234567890')                                
913   FORMAT(//,1X,A,8X,'12345678901234567890')                             
914   FORMAT(//,1X,A,'CDU : ',A14)
915   FORMAT(//,1X,A,'CDU : ',A14,//,1X,A,7X,'12345678901234567890')
916   FORMAT(/,1X,A,'Message ',A,'  Enter',/
     *       1X,A,'      game number for game ticket text',/,
     *       1X,A,'      C for CDU text',/,
     *       1X,A,'      P for opinion poll text',/)

      END                                                                       
