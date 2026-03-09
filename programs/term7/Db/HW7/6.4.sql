MERGE INTO Marks AS M
USING NewMarks AS NM
ON M.StudentId = NM.StudentId AND M.CourseId = NM.CourseId
WHEN MATCHED AND M.Mark < NM.Mark THEN UPDATE SET M.Mark = NM.Mark
WHEN NOT MATCHED THEN INSERT (StudentId, CourseId, Mark)
    VALUES (NM.StudentId, NM.CourseId, NM.Mark)