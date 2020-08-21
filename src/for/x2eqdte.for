C
C SUBROUTINE X2EQDTE.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2EQDTE.FOV                                  $
C  $Date::   17 Apr 1996 16:15:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C X2EQDTE - output status is 1 if equal length and address fields, else 0
C
C V01 21-DEC-91 MF  ORIGINAL
C
C Calling Sequence:
C
C     CALL X2EQDTE(LEN1,ADDR1,LEN2,ADDR2,STS)
C
C Input parameters:
C
C     LEN1,LEN2   Int*4           Length of Addr 1 and Addr 2
C                                 (Padded with 0 to even NIBBLE length)
C     ADDR1,ADDR2 Int*4(*)        Address , 8 octets (16 BCD nibbles) max
C
C Output status:
C
C     STS        Int*4            Output comparison status
C                                    1  - equal
C                                    0 - different
C**************
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE X2EQDTE(LEN1,ADDR1,LEN2,ADDR2,STS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
      INTEGER*4 LEN1, LEN2                     ! Input address length
      INTEGER*4 ADDR1(2),ADDR2(2)              ! Input addresses
      INTEGER*4 STS                            ! Output status code
C
      INTEGER*4 L1,L2                          ! Len in octets, even
      INTEGER*4 TEMP1, TEMP2,I,LEN             ! Byte extract, idx,nibbles
C
      L1 = (LEN1+1)/2
      L2 = (LEN2+1)/2
C
      IF ( L1 .EQ. L2) THEN
        IF (L1 .EQ. 0) THEN                    ! Equal and 0-len, "direct call"
          STS = 1
        ELSE                                   ! len > 0
          STS = 1                              ! initial: all octets same
          LEN=0
          DO 100 I=1,L1                        ! L1 == L2, BYTES
            CALL ILBYTE(TEMP1,ADDR1(1),I-1)
            CALL ILBYTE(TEMP2,ADDR2(1),I-1)
C
            LEN=LEN+1                          !fst nibble
            IF (LEN .LE.LEN1 .AND.
     *         IAND(TEMP1,'F0'X).NE.IAND(TEMP2,'F0'X)) STS = 0
C
            LEN=LEN+1                          !snd nibble
            IF (LEN .LE.LEN1 .AND.
     *         IAND(TEMP1,'0F'X).NE.IAND(TEMP2,'0F'X)) STS = 0
C***        IF (TEMP1 .NE. TEMP2 ) STS = 0     !found difference
100       CONTINUE
        ENDIF
      ELSE
        STS = 0                                ! different length mod(2)
      ENDIF
C
      RETURN
      END
