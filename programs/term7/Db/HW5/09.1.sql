SELECT avg(cast(Mark AS REAL)) AS AvgMark 
FROM Marks
WHERE StudentId = :StudentId 
