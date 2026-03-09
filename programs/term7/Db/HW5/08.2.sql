SELECT StudentName, SumMark
FROM Students AS S
LEFT JOIN (
    SELECT sum(Mark) AS SumMark, StudentId 
    FROM Marks
    GROUP BY StudentId 
) AS M ON S.StudentId = M.StudentId 