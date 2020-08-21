C**************************************************
C SUBROUTINE para enviar mensagens de erro para o altura
C INPUT:
C        SBUF - mensagem que vai ser enviada
C        MESSERIAL - numero sequencial de mensagens de envio
C
C OUTPUT:
C        ST - Status do processo...
C        MESSERIAL - numero sequencial de mensagens de envio
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OUTERRORMES(TRABUF,OUTTAB,OUTLEN,ERRORNUM,RETRY)
        IMPLICIT NONE
C**************************************************        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2XSTN.DEF'
        
        INTEGER*4 MYCHKSUM, CHKLEN,ERRTYP,RETRY
        BYTE      OUTTAB(*)
        INTEGER*2 OUTLEN,I2CCITT(2)
        EQUIVALENCE (I4CCITT,I2CCITT)
        DATA ERRTYP/Z90/
        BYTE ERRORNUM
        
        IF (RETRY .EQ. 1) OUTTAB(1) = '10'X+TRABUF(TTRN)
        IF (RETRY .EQ. 2) OUTTAB(1) = '20'X+TRABUF(TTRN)
        IF (RETRY .EQ. 0) OUTTAB(1) = '10'X+TRABUF(TTRN)
        OUTTAB(2) = ERRTYP
        
        OUTTAB(5) = ERRORNUM
        OUTLEN=5
        
        I4CCITT = TRABUF(TCHK)
	OUTTAB(3) = I1CCITT(2)
	OUTTAB(4) = I1CCITT(1)
	CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM
	OUTTAB(3) = I1CCITT(2)
	OUTTAB(4) = I1CCITT(1)
C        DO RETRY=0, OUTLEN
C             TYPE 9999,RETRY,ZEXT(OUTTAB(RETRY))
C        ENDDO
C        TYPE *,' '  
        RETURN
9999    FORMAT(' ERROR SENT: ',I2.1,' - ', Z3.2)       
        END
