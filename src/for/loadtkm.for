C
C SUBROUTINE LOADTKM
C $Log:   GXAFIP:[GOLS]LOADTKM.FOV  $
C  
C     Rev 1.3   19 Feb 1997 15:09:16   HXK
C  Don not use TKMAFL
C  
C     Rev 1.2   28 Jan 1997 16:58:14   RXK
C  IAM() added 
C  
C     Rev 1.1   13 Jan 1997 17:18:42   RXK
C  CDU, phase 2. 
C  
C     Rev 1.0   17 Apr 1996 13:52:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   27 Sep 1993 13:59:36   GXA
C  Load up Ticket Message revisions.
C  
C     Rev 1.2   16 Jun 1993 16:26:54   SXH
C  Released for Finland
C  
C     Rev 1.1   13 Jun 1993 13:19:30   HXK
C  Added AGTINF.DEF, PRMAGT.DEF, changed for Finland Ticket messages.
C  
C     Rev 1.0   21 Jan 1993 16:53:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - reset_wstn.for **
C
C
C SUBROUTINE TO LOAD TICKET MESSAGE FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE LOADTKM
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECTKM.DEF'
C
        INTEGER*4  FDB(7)                   !
        INTEGER*4  ST                       !
        INTEGER*4  K                        !
        INTEGER*4  I                        !
C
C
        TYPE*,IAM(),' Loading ticket messages '
        CALL OPENW(3,SFNAMES(1,TKTM),4,0,0,ST)
        CALL IOINIT(FDB,3,TKMSEC*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,TKTM),1,ST,0)

        CALL READW(FDB,1,TKMREC,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,TKTM),2,ST,1)

	CALL CLOSEFIL(FDB)


	CALL FASTMOV(TKMMES(1,1,1),TKTMES(1,1,1),(MAXGAM+PRM_NUMOPN+NUMCDU)*
     *               TICKET_LENGTH*TICKET_ROWS)
	CALL FASTMOV(TKMMLN(1),TKTMLN(1),MAXGAM+PRM_NUMOPN+NUMCDU)
	CALL FASTMOV(TKMMRV(1),TKTMRV(1),MAXGAM+PRM_NUMOPN+NUMCDU)
        TKTCDU = TKMCDU
C
	DO 10 I=1,MAXGAM+PRM_NUMOPN+NUMCDU
	    IF(TKTMLN(I).EQ.0) GOTO 10
	    IF(I.LE.MAXGAM) THEN
                WRITE(5,900) IAM(),(GLNAMES(K,I),K=1,4)
            ELSEIF(I.LE.MAXGAM+PRM_NUMOPN) THEN
                WRITE(5,901) IAM(),SCC_OPNID(I-MAXGAM)
            ELSE
                WRITE(5,902) IAM(),I-(MAXGAM+PRM_NUMOPN)
            ENDIF
10	CONTINUE

        TYPE*,IAM(),' Ticket message load complete'
900	FORMAT(1X,A18,' Ticket message loaded for game ',4A4)
901     FORMAT(1X,A18,
     *         ' Ticket message loaded for Opinion Poll, ID = ',
     *         I2)
902	FORMAT(1X,A18,' CDU text ',I1,' loaded')

	RETURN

	END
