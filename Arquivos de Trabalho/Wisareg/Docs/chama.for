Program chama

        character*164 diret
        character*1   cflag

        interface
          subroutine isareg(diret, cflag)
            !DEC$ ATTRIBUTES DLLIMPORT :: isareg
            !DEC$ ATTRIBUTES ALIAS: 'isareg' :: isareg
            character*164 diret
            character*1 cflag
          end subroutine isareg
        end interface

        diret = 'C:\demo_isa'
        call isareg(diret, cflag)
end
