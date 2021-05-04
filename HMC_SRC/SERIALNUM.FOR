       PROGRAM SERIALNUM
        IMPLICIT NONE 

C        integer, parameter :: ep= selected_real_kind(21)
C        real(ep), parameter :: OVER8BYTES = 18446744073709551616.0_ep
        REAL*16 OVER8BYTES
        PARAMETER (OVER8BYTES = 18446744073709551616.0)
        REAL*16 SERIALNUM_OLM
        INTEGER*4 TWCOLMSERH_TLTO /54/, Ios
        INTEGER*8 I8TMP /3875820019684212735/
        CHARACTER*24  SERIALNUM_OLMSTR,  SERIAL_AUX
C        write(*,*) OVER8BYTES, precision(OVER8BYTES)
        write(*,*) OVER8BYTES
        
        SERIALNUM_OLM = DFLOAT(TWCOLMSERH_TLTO)*OVER8BYTES+DFLOAT(I8TMP) 
        WRITE(SERIAL_AUX,990) SERIALNUM_OLM
C        WRITE(SERIAL_AUX,990,IOSTAT=Ios) SERIALNUM_OLM 
C        IF (Ios .NE. 0) THEN
C                WRITE(*,*) 'Error: afloat too big.'
C        END IF
        SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)    

        open(UNIT=1277, FILE='serialnum.dat', ACCESS='append', STATUS='old')
        write(1277,980) SERIALNUM_OLMSTR
        close(1277)   
        
C 1 not real     332446744073709551616 -> 1205A0185B50A80000
C all 8 bytes -> 05A0185B50A80000 -> 405350746937622528
C 9º byte -> 12 Hex = 18 dec 
C 2 real Sara     210317060218032905995 -> B66BC4BF5F44A5F0B
C all 8 bytes -> 66BC4BF5F44A5F0B -> 7402875407227838219
C 9º byte -> B Hex = 11 dec 
C 3 real henrique B66BC5005629C366C -> 210317064682354783852 (210317-06-4682354783-852)
C all 8 bytes -> 66BC5005629C366C -> 7402879871549716076 
C 9º byte -> B Hex = 11 dec     
C obtido no tmir gerado 210317-06-4680700382-765. a validar o erro

990     FORMAT(F22.0)
980     FORMAT('SerialNumber OLM YES>',A)      
       END