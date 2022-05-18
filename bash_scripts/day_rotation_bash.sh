#!/bin/bash
# should use /usr/bin/expect instead of bash run commands spawn, expect, send
# path to script C:/Users/105864/Documents/Millennium/bash_scripts (create a windows environment variable)
# install sshpass (https://stackoverflow.com/questions/37243087/how-to-install-sshpass-on-windows-through-cygwin) 
# finall step (sudo- run cygwin as administrator) make install

#####################################################################
#   cd "C:\Users\105864\Documents\Millennium\bash_scripts"          #
#   run script: ./day_rotation_bash.sh ou bash day_rotation_bash.sh #
#   to exit: enter ~.                                               #
#                                                                   #
#   man ssh                                                         #
#                                                                   #
#   Delta time format:                                              #
#   wait for OpenVMS "WAIT 00:00:05.000"   +[dddd-][hh:mm:ss.cc]    #
#   EOT or EOF ????                                                 #
#   -T      Disable pseudo-terminal allocation.   <- this flag      #
#   makes it disconnet                                              #
#####################################################################

sshpodev1 ()                                                                                                                                                                                    
{                                                                                                                                                                                               
    SSHUSER="$1";                                                                                                                                                                               
    [ -z "${SSHUSER}" ] && SSHUSER="console";                                                                                                                                                   
    ssh -o "KexAlgorithms diffie-hellman-group1-sha1" -o "HostKeyAlgorithms ssh-dss" -o "Ciphers aes256-cbc" "${SSHUSER}"@10.100.82.101                                                          
}

sshpodev1_pass ()                                                                                                                                                                                    
{                                                                                                                                                                                              
    SSHUSER="$1";                                                                                                                                                                               
    [ -z "${SSHUSER}" ] && SSHUSER="console";                                                                                                                                                   
    sshpass -e ssh -T -e "~" -o "KexAlgorithms diffie-hellman-group1-sha1" -o "HostKeyAlgorithms ssh-dss" -o "Ciphers aes256-cbc" "${SSHUSER}"@10.100.82.101 << EOF
    GOSCML
EOF
}

#sshpodev1
#sshpodev1_pass

#sshpodev1_pass  << EOF
#    GOSCML
#EOF

sshpodev1_pass
# <<EOF
#    DIR
#EOF

