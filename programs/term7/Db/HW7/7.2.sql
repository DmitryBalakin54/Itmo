CREATE VIEW AllMarks(StudentId, Marks) AS
SELECT StudentId, (
    SELECT COUNT(M.Mark)
    FROM Marks AS M
    WHERE M.StudentId = S.StudentId
) + (
    SELECT COUNT(NM.Mark)
    FROM NewMarks AS NM
    WHERE NM.StudentId = S.StudentId
)
FROM Students AS S