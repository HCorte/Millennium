
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM FINDSSODDS
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'

	INTEGER*4 GIND
	INTEGER*4 DRAW
	INTEGER*4 SCORE(2,3)
	INTEGER*4 EXT
	INTEGER*4 UCID
	INTEGER*4 ODDS
	INTEGER*4 AMT
        INTEGER*4 I4TEMP
        BYTE      I1TEMP(4)
        EQUIVALENCE(I4TEMP,I1TEMP)
        
50      CONTINUE

        CALL INPNUM('Enter SS game index ',GIND,1,6,EXT)
        IF(EXT.LT.0) GOTO 100
        CALL INPNUM('Enter SS draw number ',DRAW,1,999999,EXT)
        IF(EXT.LT.0) GOTO 100

        CALL INPNUM('Enter set 1 home score',SCORE(1,1),0,15,EXT)
        IF(EXT.LT.0) GOTO 100
        CALL INPNUM('Enter set 1 away score',SCORE(2,1),0,15,EXT)
        IF(EXT.LT.0) GOTO 100

        CALL INPNUM('Enter set 2 home score',SCORE(1,2),0,15,EXT)
        IF(EXT.LT.0) GOTO 100
        CALL INPNUM('Enter set 2 away score',SCORE(2,2),0,15,EXT)
        IF(EXT.LT.0) GOTO 100

        CALL INPNUM('Enter set 3 home score',SCORE(1,3),0,15,EXT)
        IF(EXT.LT.0) GOTO 100
        CALL INPNUM('Enter set 3 away score',SCORE(2,3),0,15,EXT)
        IF(EXT.LT.0) GOTO 100

        I1TEMP(3) = ISHFT(SCORE(1,1),4) + IAND(SCORE(2,1),'0F'X)
        I1TEMP(2) = ISHFT(SCORE(1,2),4) + IAND(SCORE(2,2),'0F'X)
        I1TEMP(1) = ISHFT(SCORE(1,3),4) + IAND(SCORE(2,3),'0F'X)
        UCID=I4TEMP

        CALL SSODSCAN(DRAW,GIND,UCID,ODDS,AMT)

        WRITE(6,900) IAM(),ODDS/100,MOD(ODDS,100),CMONY(AMT,8,BETUNIT)
        GOTO 50

100     CONTINUE
        CALL GSTOP(GEXIT_SUCCESS)

900     FORMAT(1X,A,'Odds are ',I6,'.',I2.2,', investment is ',A8)
        END
        
       
