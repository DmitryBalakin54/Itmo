SET "root=..\"
SET "lib=..\java-advanced-2024\lib\*"
SET "test=..\java-advanced-2024\artifacts\info.kgeorgiy.java.advanced.implementor.jar"
SET "data=..\java-advanced-2024\modules\info.kgeorgiy.java.advanced.implementor\info\kgeorgiy\java\advanced\implementor\"
SET "link= https://docs.oracle.com/en/java/javase/22/docs/api"
SET "main=java-solutions\info\kgeorgiy\ja\balakin\implementor\Implementor.java"
SET "impler=%data%Impler.java"
SET "jarimpler=%data%JarImpler.java"
SET "exc=%data%ImplerException.java"

cd %root%

javadoc -d javadoc -link %link% -cp %lib%;%test%; -private -author -version %main% %impler% %jarimpler% %exc%

pause