SELECT StudentName, AvgMark
FROM Students AS S
LEFT JOIN (
    SELECT avg(cast(Mark AS REAL)) AS AvgMark, StudentId 
    FROM Marks
    GROUP BY StudentId 
) AS M ON S.StudentId = M.StudentId 