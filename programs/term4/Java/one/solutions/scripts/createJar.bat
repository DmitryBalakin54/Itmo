SET "root=..\..\"
SET "solutions=solutions\java-solutions\"
SET "lib=shared\artifacts\info.kgeorgiy.java.advanced.implementor.jar"
SET "jar=..\scripts\Implementor.jar"
SET "dir=info\kgeorgiy\ja\balakin\implementor\"
SET "implementor=%solutions%%dir%Implementor.java"
SET "man=..\scripts\MANIFEST.MF"
SET "impl=info\kgeorgiy\java\advanced\implementor\"
SET "shr=shared\modules\info.kgeorgiy.java.advanced.implementor\"
SET "copydir=..\..\..\%solutions%%impl%"

cd %root%
javac -cp %lib% %implementor%

cd %solutions%

jar cfm %jar% %man% %dir%Implementor.class
del %dir%Implementor.class

pause