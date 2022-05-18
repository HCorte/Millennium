#!/usr/bin/expect
# should use /usr/bin/expect instead of bash run commands spawn, expect, send
# path to script C:/Users/105864/Documents/Millennium/bash_scripts (create a windows environment variable)
# install sshpass (https://stackoverflow.com/questions/37243087/how-to-install-sshpass-on-windows-through-cygwin) 
# finall step (sudo- run cygwin as administrator) make install

#####################################################################
#   cd "C:\Users\105864\Documents\Millennium\bash_scripts"          #
#   run script: expect day_rotation_bash.sh                         #
#                                                                   #
#####################################################################

#proc sshpodev1 {} {                                                                                                                                                                                               
#    SSHUSER="$1";                                                                                                                                                                               
#    [ -z "${SSHUSER}" ] && SSHUSER="console";                                                                                                                                                   
#    ssh -o "KexAlgorithms diffie-hellman-group1-sha1" -o "HostKeyAlgorithms ssh-dss" -o "Ciphers aes256-cbc" "${SSHUSER}"@10.100.82.101                                                          
#}

## Access CLI
proc new_line {} {
    expect "POSYSD::SCML> " {send "\r"} \
        timeout { puts "\n\nTest Failure: enter - new line \n\r"; exit}
}

set loginUser "console"
set loginPassword "console"

set timeout 30

#sshposysd_pass
spawn sshpass -e ssh -e "~" -o "KexAlgorithms diffie-hellman-group1-sha1" -o "HostKeyAlgorithms ssh-dss" -o "Ciphers aes256-cbc" console@10.100.82.100
#interact

#expect "password:" {send "$loginPassword\r"}

expect -ex {POSYSD::PORT> } {send "GOSCML\r"} \
    timeout { 
        puts "\n\nTest Failure: GOSCML\n\r"; 
        exit 
    }

new_line

expect -ex {POSYSD::SCML> } {send "dir *.EXE;0 /GRAND_TOTAL\r"} \
    timeout { 
        puts "\n\nTest Failure: GRAND_TOTAL\n\r"; 
        exit 
    }

new_line  

#set DATE [clock add [clock seconds] -1 day]
#set DATE [clock format $DATE -format %d-%b-%Y]
#send "dir /total/since=$DATE"

set DATE [clock add [clock seconds] -1 day]
set DATE [clock format $DATE -format %d-%m-%Y]
set TMIR_NAME "HENRIQUE-$DATE"

send "$TMIR_NAME"

new_line

send "\r\rshscml\r" 

expect {SCMLELOG2} { expect {SCMLMONGOLS} {send "sh process SCMLMONGOLS\r"} } \
    timeout { 
        puts "\n\n!!!Test Failure: find process SCMLCMDPRO followed by SCMLINCPRO!!! \n\r"; 
        interact {
            \001 {
               send_user "\n\nUser interaction completed.\n\n"
               return 
            }
        }
    }

send "shscml\r" 

expect {SCMLELOG} { expect {SCMLMONGOLS} {send "sh process SCMLMONGOLS\r"} } \
    timeout { 
       puts "\n\n!!!Test Failure: find process SCMLCMDPRO followed by SCMLINCPRO!!! \n\r"; 
       interact 
    }

new_line

interact