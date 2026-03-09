SELECT GroupName, AvgAvgMark
FROM Groups AS G
LEFT JOIN (
    SELECT GroupId, avg(cast(AvgMark AS REAL)) AS AvgAvgMark
    FROM (
        SELECT avg(cast(Mark AS REAL)) AS AvgMark, StudentId
        FROM Marks
        GROUP BY StudentId 
    ) AS M
    NATURAL JOIN Students
    GROUP BY GroupId
) AS E ON G.GroupId = E.GroupId