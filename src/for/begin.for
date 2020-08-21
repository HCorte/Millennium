C
C SUBROUTINE BEGIN
C $Log:   GXAFXT:[GOLS]BEGIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:15:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:41:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_screen.for **
C
C SCREEN.FOR
C
C V02 15-APR-91 TKO  Use SMG stuff to handle input lines
C V01 10-AUG-90 TKO  RELEASED FOR VAX
C
C This performs the same functions as SCREEN.MAC on concurrent.  It
C is called by VISION.FOR to handle screen output, interval waits, etc.
C
C There are 2 entry points:
C
C	BEGIN  is called if we are running dynamically
C	BEGIN1 is called if we wait for input
C
C
	SUBROUTINE BEGIN
	IMPLICIT NONE
C
	CALL VZSCREEN(0)
	RETURN
	END
