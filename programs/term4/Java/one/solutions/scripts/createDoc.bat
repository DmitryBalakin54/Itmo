SET "root=../../"
SET "lib=shared\lib\*"
SET "test=shared\artifacts\info.kgeorgiy.java.advanced.implementor.jar"
SET "data=shared\modules\info.kgeorgiy.java.advanced.implementor\info\kgeorgiy\java\advanced\implementor\"
SET "link= https://docs.oracle.com/en/java/javase/22/docs/api"
SET "main=solutions\java-solutions\info\kgeorgiy\ja\balakin\implementor\Implementor.java"

cd %root%

javadoc -d solutions\javadoc -link %link% -cp %lib%;%test%; -private -author -version %main% %data%Impler.java %data%JarImpler.java %data%ImplerException.java

pause