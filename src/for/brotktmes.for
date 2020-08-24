C
C $Log:   GXAFIP:[GOLS]BROTKTMES.FOV  
C  
C     Rev 1.1   01 Feb 1997 17:36:18   RXK
C  Changes for CDU.
C  
C     Rev 1.0   17 Apr 1996 12:22:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   08 Sep 1993 14:17:26   CXK
C  MADE IT WORK CORRECTLY (SMH)
C  
C     Rev 1.1   02 Sep 1993 22:50:50   JWE
C  Length was not being set in the PROCOM header.
C  
C     Rev 1.0   12 Jul 1993 20:05:58   GXA
C  Initial revision.
C                                                                               
C SUBROUTINE TO SEND TICKET MARKETING MESSAGES TO TERMINALS 
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BROTKTMES(MSG,GTYP,GIND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'  
	INCLUDE 'INCLIB:CONCOM.DEF'  
	INCLUDE 'INCLIB:PROCOM.DEF'  
	INCLUDE 'INCLIB:TASKID.DEF'  
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C           
	BYTE	  I1TEMP(4)				!TEMP VAR
C
	INTEGER*4 MSG					!Ticket MSG #
	INTEGER*4 GTYP					!Game Type
	INTEGER*4 GIND					!Game Index
	INTEGER*4 MYCHKSUM				!Calculated Checksum
	INTEGER*4 BUF					!Buffer #
	INTEGER*4 LEN					!TKT Message Length
	INTEGER*4 I,J					!Loop Variable
	INTEGER*4 IND					!Index into MESTAB
	INTEGER*4 TYPIND				!GameType/GameIndex
	INTEGER*4 I4TEMP				!TEMP VAR
	INTEGER*4 CHKLEN				!Length to checksum
        INTEGER*4 CDU_ROWS/2/                           !# of rows on CDU
        INTEGER*4 CDU_LINE_LENGTH/20/                   !length of row on CDU
C

C
	EQUIVALENCE(I4TEMP,I1TEMP)
C
C
	CALL GETBUF(BUF)
	IF(BUF.LE.0) THEN                                                 
	   TYPE*,'Buffer allocation error, message not sent ' 
	   RETURN
	ENDIF
C
C FILL IN BROADCAST HEADER INFORMATION
C                                                
        IF(MSG.LE.MAXGAM+PRM_NUMOPN) THEN
           LEN=TKTMLN(MSG)*30+8
        ELSE
           LEN=NUMCDU*CDU_LINE_LENGTH*CDU_ROWS+6
        ENDIF
	HPRO(OUTLEN,BUF) = LEN 
	HPRO(MSGNUM,BUF) = 0   
	HPRO(TRCODE,BUF) = TYPUNS
	HPRO(LINENO,BUF) = -1    
	HPRO(TERNUM,BUF) = -1
	HPRO(ENCOVR,BUF) = -1
	HPRO(PRCSRC,BUF) = 0 
C                                                                               
C TRANSFER GAME OR OPINION POLL TICKET MESSAGE TO BUFFER                                                    
C                                                                               
        IF(MSG.LE.MAXGAM+PRM_NUMOPN) THEN
C
	   TYPIND=ISHFT(GTYP,4)+GIND
	   BPRO(BINPTAB+0,BUF) = '20'X
	   BPRO(BINPTAB+1,BUF) = 'B3'X
	   BPRO(BINPTAB+4,BUF) = TYPIND
	   I4TEMP = TKTMRV(MSG)
	   BPRO(BINPTAB+5,BUF) = I1TEMP(1)
	   BPRO(BINPTAB+6,BUF) = I1TEMP(2)
	   BPRO(BINPTAB+7,BUF) = TKTMLN(MSG)*30
           IND = 9 
	   DO I=1,TKTMLN(MSG)
	      CALL MOVBYT(TKTMES(1,I,MSG),1,BPRO(BINPTAB,BUF),IND,30)
	      IND = IND + 30
	   END DO
C                                                                               
C TRANSFER CDU TEXT TO BUFFER
C                                                                               
        ELSE
	   BPRO(BINPTAB+0,BUF) = '20'X
	   BPRO(BINPTAB+1,BUF) = 'B6'X
           I4TEMP=TKTCDU
           BPRO(BINPTAB+4,BUF) = I1TEMP(1)
           BPRO(BINPTAB+5,BUF) = I1TEMP(2)
           IND = 7
           DO I=1,NUMCDU
              DO J=1,CDU_ROWS
                 CALL MOVBYT(TKTMES(1,J,MAXGAM+PRM_NUMOPN+I),1,
     *                       BPRO(BINPTAB,BUF),IND,CDU_LINE_LENGTH)
                 IND=IND+CDU_LINE_LENGTH
              ENDDO
           ENDDO
        ENDIF  
C
C CALCULATE CHECK SUM ON OUTPUT BUFFER
C
        CHKLEN = IND - 1
        I4CCITT = 0
        BPRO(BINPTAB+2,BUF) = I1CCITT(2)
        BPRO(BINPTAB+3,BUF) = I1CCITT(1)
        CALL GETCCITT(BPRO,BINPTAB+3,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        BPRO(BINPTAB+2,BUF) = I1CCITT(2)
        BPRO(BINPTAB+3,BUF) = I1CCITT(1)
C
        CALL QUETRA(LOG,BUF)
C
	RETURN
	END
C                                                                               
