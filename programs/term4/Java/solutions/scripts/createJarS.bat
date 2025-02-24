SET "root=..\"
SET "solutions=java-solutions\"
SET "lib=..\java-advanced-2024\artifacts\info.kgeorgiy.java.advanced.implementor.jar"
SET "jar=..\scripts\Implementor.jar"
SET "dir=info\kgeorgiy\ja\balakin\implementor\"
SET "implementor=%solutions%%dir%Implementor.java"
SET "man=..\scripts\MANIFEST.MF"

cd %root%
javac -cp %lib% %implementor%

cd %solutions%

jar cfm %jar% %man% %dir%Implementor.class
del %dir%Implementor.class

pause