      SUBROUTINE CONCATENATE_INT(val1, val2, stringFinal)
        IMPLICIT NONE
        integer val1, val2
        character*17 string
        character*34 stringFinal
        character*17 stringAux

        write(string,1) val1 !convert the integer value to string using internal file log-> (write)
   1    format(I4)
        string = 'm value is: '//string           

        print *, 'entao string final: '//stringFinal
        print *, 'stringAux que devia estar vazia '//stringAux
        write(stringAux,2) val2!inner file
        print *, 'stringAux que devia estar preenchido: '//stringAux
   2    format(I4)        
        stringFinal = string//' n value is: '//stringAux
        print *, 'stringFinal:'//stringFinal

        RETURN
      END

      SUBROUTINE LOG_ROW(row, filename)
        IMPLICIT NONE
        character*8 date
        character*10 time
        character*20 logdate!could be 18 caracters with 
        character*80 filename 
        character*34 row !100 with getting more then expected

        print *,'second daterow is: '//row
        print *,'filename: '//filename
        !path = 'GXPROJ:[WRK.HMC.EXAMPLES.LOGFILES]logs.dat' 
        !print *, path
        call date_and_time(date,time) !call fdate( date )

        !print *, date
        !print *, 'yyyy:'//date(1:4)
    !print *, 'mm:'//date(5:6)
        !print *, 'dd:'//date(7:8)
        !print *, time
        !print *, 'hours:'//time(1:2)
        !print *, 'minutes:'//time(3:4)
        !print *, 'seconds:'//time(5:6)
        
        logdate = date(7:8)//'/'//date(5:6)//'/'//date(1:4)
     &  //'  '//time(1:2)//':'//time(3:4)//':'//time(5:6)
        print *, 'date is: '//logdate        
        
        print *,'third daterow is: '//row
        print *, logdate//' -  '//row

        open(UNIT=1235, FILE=filename, ACCESS='append', STATUS='old')
        write(1235,*) logdate//' -  '//row
        close(1235)

        RETURN
      END