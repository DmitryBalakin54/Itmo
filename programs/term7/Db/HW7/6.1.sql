INSERT INTO Marks (StudentId, CourseId, Mark)
SELECT NM.StudentId, NM.CourseId, NM.Mark
FROM NewMarks AS NM
WHERE NOT EXISTS (
    SELECT *
    FROM Marks AS M
    WHERE M.StudentId = NM.StudentId
    AND M.CourseId = NM.CourseId
)