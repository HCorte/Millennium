C
C SUBROUTINE BUILDX
C $Log:   GXAFXT:[GOLS]BUILDX.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:23:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.5   15 Oct 1993 14:59:50   GXA
C  Removed Type statements.
C  
C     Rev 1.4   18 Sep 1993 16:17:06   HXK
C  Type statements for possible PVCS problem
C  
C     Rev 1.3   18 Sep 1993 15:19:46   HXK
C  Removed PVCS problem
C  
C     Rev 1.2   17 Sep 1993 11:35:02   HXK
C  Handling of Viking / Jokers changed
C  
C     Rev 1.1   13 Aug 1993  1:56:02   GXA
C  Released for Finland Dec Conversion / Oddset.
C  
C     Rev 1.0   21 Jan 1993 15:46:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - valupd.for **
C
C
C
C
C
C SUBROUTINE TO BUILD EXCHANGE TICKET BUFFERS.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BUILDX(TRABUF,SER,SDRAW,SKDRAW,FILE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 FILE, SKDRAW, SDRAW, SER
C
	TRABUF(TWCSER)=TRABUF(TSER)			!For Tracebility.
	TRABUF(TWCDCOFF) = DAYCDC - TRABUF(TCDC)
C
	TRABUF(TSTAT)=EXCH
	TRABUF(TSIZE)=3
	TRABUF(TWCTER)=0
	TRABUF(TWVSTS)=0
	TRABUF(TSER)=SER
	TRABUF(TCDC)=DAYCDC

C        
C if game is 'dead', i.e. it is not to be printed on exch ticket, then 
C do not set beg draw to that passed in by valupd (applies to
C Viking with Joker 16/9/93)
C         
	IF(SDRAW.NE.0) TRABUF(TWBEG)=SDRAW
	IF(SKDRAW.NE.0) TRABUF(TWKBEG)=SKDRAW
C
	TRABUF(TFIL)=FILE
	IF(TRABUF(TGAMTYP).EQ.TNBR.AND.TRABUF(TWNAND).NE.0) THEN
	  TRABUF(TWNAND)=0
	  TRABUF(TWDUR)=1
	  TRABUF(TWBEG)=TRABUF(TWEND)
	ELSE
	  TRABUF(TWDUR)=TRABUF(TWEND)-TRABUF(TWBEG)+1
	ENDIF
	TRABUF(TWKDUR)=TRABUF(TWKEND)-TRABUF(TWKBEG)+1
	TRABUF(TWTOT)=TRABUF(TWDUR)*TRABUF(TWAMT)+
     *	              TRABUF(TWKDUR)*TRABUF(TWKAMT)+
     *	              TRABUF(TWTKC)
	RETURN
	END
